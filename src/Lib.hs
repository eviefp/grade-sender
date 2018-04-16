module Lib
    ( runGradeStudents
    ) where

import           Control.Applicative         (Alternative)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask, local,
                                              runReaderT)
import           Data.Aeson                  (FromJSON, parseJSON, withObject,
                                              (.!=), (.:), (.:?))
import           Data.Attoparsec.ByteString  (parseOnly)
import           Data.Bifunctor              (bimap)
import           Data.ByteString             (ByteString, readFile)
import           Data.Csv                    (NamedRecord)
import           Data.Csv.Parser             (csvWithHeader,
                                              defaultDecodeOptions)
import           Data.Foldable               (traverse_)
import           Data.HashMap.Strict         (lookupDefault, toList)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (unpack)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Text.Lazy              (Text, fromStrict)
import           Data.Vector                 (Vector)
import           Data.Yaml.Config            (loadYamlSettings, useEnv)
import           Network.HaskellNet.Auth     (Password, UserName)
import           Network.HaskellNet.SMTP     (AuthType (LOGIN), SMTPConnection,
                                              sendPlainTextMail)
import           Network.HaskellNet.SMTP.SSL (authenticate, doSMTPSTARTTLS)
import           Prelude                     (Applicative, Either (Left, Right),
                                              FilePath, Functor, IO,
                                              Maybe (Just, Nothing), Monad,
                                              Show, String, fmap, print, pure,
                                              putStrLn, snd, ($), (++), (.),
                                              (<$>))
import           Text.Glabrous               (Template, fromList, process,
                                              readTemplateFile)


(∘) ∷ (b → c) → (a → b) → a → c
(∘) = (.)

infixr 9 ∘

configPath ∷ FilePath
configPath = "config/grade-sender.yaml"

type SMTPHostName = String
type EmailTitle = String
type GradesCsvFile = FilePath
type TemplateFile = FilePath

data AppSettings = AppSettings
  { appSmtpServer    ∷ SMTPHostName
  , appSmtpUsername  ∷ UserName
  , appSmtpPassword  ∷ Password
  , appEmailFrom     ∷ String
  , appEmailTitle    ∷ EmailTitle
  , appGradesCsvFile ∷ GradesCsvFile
  , appTemplateFile  ∷ TemplateFile
  } deriving (Show)

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o → do
    appSmtpServer    ← o .: "smtpServer"
    appSmtpUsername  ← o .: "smtpUsername"
    appSmtpPassword  ← o .: "smtpPassword"
    appEmailFrom     ← o .: "emailFrom"
    appEmailTitle    ← o .:? "emailTitle"   .!= ""
    appGradesCsvFile ← o .:? "gradesFile"   .!= ""
    appTemplateFile  ← o .:? "templateFile" .!= ""
    pure AppSettings {..}

newtype AppT m a = AppT
  { unAppT :: ReaderT AppSettings m a
  } deriving (Functor, Applicative, Monad, Alternative, MonadIO)

instance Monad m ⇒ MonadReader AppSettings (AppT m) where
  ask = AppT ask
  local f (AppT app) = AppT (local f app)

runGradeStudents ∷ Maybe EmailTitle → Maybe GradesCsvFile → Maybe TemplateFile → IO ()
runGradeStudents emailTitle gradesFile templateFile = do
  settings ← readSettings emailTitle gradesFile templateFile
  liftIO $ print settings
  runReaderT (unAppT runGradeStudents') settings

readSettings ∷ Maybe EmailTitle → Maybe GradesCsvFile → Maybe TemplateFile → IO AppSettings
readSettings emailTitle gradesFile templateFile = do
  settings ← loadYamlSettings [configPath] [] useEnv
  let AppSettings {..} = settings
  pure $ AppSettings
            appSmtpServer
            appSmtpUsername
            appSmtpPassword
            appEmailFrom
            (fromMaybe appEmailTitle    emailTitle)
            (fromMaybe appGradesCsvFile gradesFile)
            (fromMaybe appTemplateFile  templateFile)

runGradeStudents' ∷ AppT IO ()
runGradeStudents' = do
  mgf ← parseGradeFiles
  case mgf of
    Nothing → liftIO ∘ putStrLn $ "CSV Parse Error"
    Just gf → do
      let prs = parse <$> gf
      doGradeMail prs

parseGradeFiles ∷ AppT IO (Maybe (Vector NamedRecord))
parseGradeFiles = do
  settings ← ask
  let AppSettings {..} = settings
  bs <- liftIO $ readFile appGradesCsvFile
  let res = parseOnly (csvWithHeader defaultDecodeOptions) bs
  case res of
    Left err -> do
      liftIO ∘ putStrLn $ "Parse error:" ++ err
      pure Nothing
    Right pr → pure . Just ∘ snd $ pr


data CsvRow = CsvRow
  { crEmail   ∷ ByteString
  , crRows    ∷ NamedRecord
  } deriving (Show)

parse ∷ NamedRecord → CsvRow
parse hm = CsvRow
             (lookupDefault "" "Email"   hm)
             hm


doGradeMail ∷ Vector CsvRow → AppT IO ()
doGradeMail vect = do
  settings ← ask
  let AppSettings {..} = settings
  et ← liftIO $ readTemplateFile appTemplateFile
  case et of
    Left err → liftIO ∘ putStrLn $ "Error parsing template file: " ++ err
    Right template → do
      liftIO ∘ doSMTPSTARTTLS appSmtpServer $ \conn -> do
        authSucceed ← authenticate LOGIN appSmtpUsername appSmtpPassword conn
        if authSucceed
          then
            let sendMails = unAppT ∘ traverse_ (\r → sendMail (constructBody r template) conn r) $ vect
            in runReaderT sendMails settings
          else putStrLn "Authentication failed."

sendMail ∷ Text → SMTPConnection → CsvRow → AppT IO ()
sendMail body conn CsvRow{..} = do
  settings ← ask
  let AppSettings {..} = settings
  liftIO $ sendPlainTextMail (unpack ∘ decodeUtf8 $ crEmail) appEmailFrom appEmailTitle body conn

constructBody ∷ CsvRow → Template → Text
constructBody CsvRow{..} template =
  fromStrict $ process template (fromList ∘ fmap (bimap decodeUtf8 decodeUtf8) ∘ toList $ crRows)

