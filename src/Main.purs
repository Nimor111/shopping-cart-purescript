module Example.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (SProxy(..), Variant, inj, on, case_)
import Database.PostgreSQL (PGError)
import Database.PostgreSQL as PostgreSQL
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Class.Console as Console
import HTTPure (Response, ResponseM, ok) as HTTPure
import HTTPure.Request (Request) as HTTPure
import HTTPure.Response (internalServerError)
import HTTPure.Response (notFound) as HTTPure
import HTTPure.Server (serve) as HTTPure
import Selda.PG.Class (insert_)
import Selda.Query.Class (hoistSeldaWith)
import Selda.Table (Table(..))


dbConfig :: PostgreSQL.PoolConfiguration
dbConfig = (PostgreSQL.defaultPoolConfiguration "shoppingcart")
  { user = Just "postgres"
  , password = Just ""
  }

execute :: String -> PostgreSQL.Connection -> Aff Unit
execute sql conn = do
    PostgreSQL.execute conn (PostgreSQL.Query sql) PostgreSQL.Row0
        >>= maybe (pure unit) (throwError <<< error <<< show)

createPeople ∷ PostgreSQL.Connection → Aff Unit
createPeople = execute """
  CREATE TABLE IF NOT EXISTS people (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER
  );"""

people ∷ Table
  ( id ∷ Int
  , name ∷ String
  , age ∷ Maybe Int
  )
people = Table { name: "people" }

type Context =
    { conn :: PostgreSQL.Connection
    , other :: String
    }

type AppError = Variant
    ( pgError :: PGError
    , error :: String
    )
_pgError = SProxy :: SProxy "pgError"
type App = ReaderT Context (ExceptT AppError Aff)

runApp :: ∀ a. Context -> App a -> Aff (Either AppError a)
runApp ctx m = runExceptT $ runReaderT m ctx

hoistSelda
  :: ∀ e r m
  .  MonadAsk { conn ∷ PostgreSQL.Connection | r } m
  => MonadThrow (Variant ( pgError ∷ PGError | e )) m
  => MonadAff m
  => ExceptT PGError (ReaderT PostgreSQL.Connection Aff) ~> m
hoistSelda = hoistSeldaWith (inj _pgError) (_.conn)

appMiddleware
   :: Context
   -> (HTTPure.Request -> App HTTPure.Response)
   -> HTTPure.Request
   -> HTTPure.ResponseM
appMiddleware ctx r request =
    runApp ctx (r request) >>= handleGenericError

handleGenericError
    :: Either (Variant ( pgError :: PGError, error :: String )) HTTPure.Response
    -> HTTPure.ResponseM
handleGenericError = either genericError pure

genericError :: Variant (pgError :: PGError, error :: String) -> HTTPure.ResponseM
genericError =
    case_
      # on _pgError
        (\s -> internalServerError ("[PGERROR]: " <> show s))
      # on (SProxy :: SProxy "error")
        (\s -> internalServerError ("[ERROR]: " <> s))

insertPeople
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
insertPeople _ =
    hoistSelda (insert_ people peopleData) >>=
    (\_ -> HTTPure.ok $ "Inserted.")
    where
        peopleData :: Array { id :: Int, name :: String, age :: Maybe Int }
        peopleData =
            [ { id: 1, name: "name1", age: Just 11 }
            , { id: 2, name: "name2", age: Just 22 }
            , { id: 3, name: "name3", age: Just 33 }
            , { id: 4, name: "name4", age: Just 44 }
            , { id: 5, name: "name5", age: Just 55 }
            ]

errorOut
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
errorOut _ = do
    other <- asks _.other

    throwError ((inj (SProxy :: SProxy "error")) "this is an error")

sayHello
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
sayHello _ = do
    other <- asks _.other

    HTTPure.ok $ "Hello, " <> other

router
    :: ∀ m
    .  MonadAff m
    => MonadAsk Context m
    => MonadThrow AppError m
    => HTTPure.Request
    -> m HTTPure.Response
router req@{ path: ["hello"] } = sayHello req
router req@{ path: ["error"] } = errorOut req
router req@{ path: ["insert"] } = insertPeople req
router _ = HTTPure.notFound

main :: Effect Unit
main = launchAff_ do
    pool <- liftEffect $ PostgreSQL.newPool dbConfig
    PostgreSQL.withConnection pool case _ of
      Left pgError -> logShow ("PostgreSQL Connection Error: " <> show pgError)
      Right conn -> do
         log "Starting server..."
         createPeople conn
         log "Created people..."

         void $ liftEffect $ (HTTPure.serve 8080 (appMiddleware { conn, other: "other" } router) do
            Console.log "Server up on port 8080")
