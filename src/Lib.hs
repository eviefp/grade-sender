{-# language OverloadedStrings #-}

module Lib
    ( runGradeStudents
    ) where

import           Control.Applicative            (Alternative, (<|>))
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader           (MonadReader, ReaderT, ask,
                                                 local, runReaderT)
import Control.Monad (unless)
import           Data.Aeson                     (FromJSON, parseJSON,
                                                 withObject, (.:))
import           Data.Attoparsec.ByteString     (parseOnly)
import           Data.Bifunctor                 (bimap)
import           Data.ByteString                (ByteString, readFile)
import           Data.Csv                       (NamedRecord)
import           Data.Csv.Parser                (csvWithHeader,
                                                 defaultDecodeOptions)
import           Data.Foldable                  (traverse_)
import           Data.HashMap.Strict            (lookup, toList)
import           Data.List                      (intersperse)
import           Data.Maybe                     (fromMaybe, fromJust)
import           Data.Text.Encoding             (decodeUtf8)
import           Data.Text.Lazy                 (fromStrict)
import           Data.Vector                    (Vector)
import           Data.Yaml.Config               (loadYamlSettings, useEnv)
import           Prelude                        (Applicative, Bool, 
                                                 Either (Left, Right), FilePath,
                                                 Functor, IO,
                                                 Maybe (Just, Nothing), Monad,
                                                 Show, String, fmap, getLine, mconcat, print, pure,
                                                 putStrLn, show, snd, ($), (++),
                                                 (.), (<$>))
import           System.Console.CmdArgs         (Data, Typeable, cmdArgs,
                                                 details, help, program,
                                                 summary, typ, (&=))
import           System.Console.CmdArgs.Default (def)
import           Text.Glabrous                  (Result (..), Template,
                                                 fromList, partialProcess',
                                                 readTemplateFile)
import qualified Network.Mail.SMTP as SMTP
import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Mail.Mime as Mime

-- | Relative path for the config file.
configPath :: FilePath
configPath = "config/grade-sender.yaml"

-- | Sanity type synonyms.
type SMTPHostName = String
type EmailTitle = Text
type GradesCsvFile = FilePath
type TemplateFile = FilePath

-- | Application settings used for the ReaderT.
-- EmailTitle, GradesCsvFile and TemplateFile are ignored in the config file
-- and read as arguments.
data AppSettings = AppSettings
  { appSmtpServer    :: SMTPHostName
  , appSmtpUsername  :: String
  , appSmtpPassword  :: String
  , appEmailFrom     :: Text
  , appEmailTitle    :: EmailTitle
  , appGradesCsvFile :: GradesCsvFile
  , appTemplateFile  :: TemplateFile
  }

-- | Custom show instance because I don't like the default.
instance Show AppSettings where
    show AppSettings{..} = mconcat . intersperse "\r\n" $
      [ "SMTP Server: " ++ appSmtpServer
      , "SMTP Username: " ++ appSmtpUsername
      , "SMTP Password: " ++ appSmtpPassword
      , "Email sent from: " ++ T.unpack appEmailFrom
      , "Email title: " ++ T.unpack appEmailTitle
      , "Grades CSV file: " ++ appGradesCsvFile
      , "Template file: " ++ appTemplateFile
      ]

-- | The Yaml parser actually uses this instance.
instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    appSmtpServer    <- o .: "smtpServer"
    appSmtpUsername  <- o .: "smtpUsername"
    appSmtpPassword  <- o .: "smtpPassword"
    appEmailFrom     <- o .: "emailFrom"
    let appEmailTitle    = ""
    let appGradesCsvFile = ""
    let appTemplateFile  = ""
    pure AppSettings {..}

-- | Application monad to make passing settings easier.
newtype AppT m a = AppT
  { unAppT :: ReaderT AppSettings m a
  } deriving (Functor, Applicative, Monad, Alternative, MonadIO)

-- | MonadReader instance which just lifts to AppT.
instance Monad m ⇒ MonadReader AppSettings (AppT m) where
  ask = AppT ask
  local f (AppT app) = AppT (local f app)

-- | Command Line arguments for cmdArgs.
data CommandArguments = CommandArguments
  { title     :: Maybe EmailTitle
  , grades    :: Maybe GradesCsvFile
  , template_ :: Maybe TemplateFile
  , force     :: Bool
  } deriving (Show, Data, Typeable)

-- | Command argument definition.
argumentDefinition :: CommandArguments
argumentDefinition = CommandArguments
  { title     = def &= typ "TITLE" &= help "Title of the email to send."
  , grades    = def &= typ "FILE"  &= help "Grades csv file. Must contain Email column."
  , template_ = def &= typ "FILE"  &= help "Template file. Replaces {{Value}} with column Value from csv."
  , force     = def &= typ "FLAG"  &= help "Do not wait for confirmation."
  } &= program "grade-sender"
    &= summary "grade-sender v0.1, (c) Vladimir Ciobanu"
    &= details ["More details at https://github.com/vladciobanu/grade-sender"]

-- | Parse yaml and command line arguments and runGradeStudents'.
runGradeStudents :: IO ()
runGradeStudents = do
  args <- cmdArgs argumentDefinition
  let CommandArguments{..} = args
  case (T.unpack <$> title) <|> grades <|> template_ of
    Nothing -> putStrLn "Missing arguments. Please consult --help"
    Just _ -> do
      settings <- readSettings (fromJust title) (fromJust grades) (fromJust template_)
      unless force $ do
        print settings
        putStrLn "\r\nPress enter to continue, or CTRL+C to cancel"
        _ <- getLine
        pure ()
      runReaderT (unAppT runGradeStudents') settings

-- | Read yaml file and fill settings with data from the command line.
readSettings :: EmailTitle -> GradesCsvFile -> TemplateFile -> IO AppSettings
readSettings emailTitle gradesFile templateFile = do
  settings <- loadYamlSettings [configPath] [] useEnv
  let AppSettings {..} = settings
  pure $ AppSettings
            appSmtpServer
            appSmtpUsername
            appSmtpPassword
            appEmailFrom
            emailTitle
            gradesFile
            templateFile

-- | Parse the grade file and call.
runGradeStudents' :: AppT IO ()
runGradeStudents' = do
  mgf <- parseGradeFile
  case mgf of
    Nothing -> liftIO . putStrLn $ "CSV Parse Error"
    Just gf -> do
      let prs = parse <$> gf
      doGradeMail prs

-- | Parses the grades csv file.
parseGradeFile :: AppT IO (Maybe (Vector NamedRecord))
parseGradeFile = do
  settings <- ask
  let AppSettings {..} = settings
  bs <- liftIO $ readFile appGradesCsvFile
  let res = parseOnly (csvWithHeader defaultDecodeOptions) bs
  case res of
    Left err -> do
      liftIO . putStrLn $ "Parse error:" ++ err
      pure Nothing
    Right pr -> pure . Just . snd $ pr

-- | A row in the grades csv. Only requires the Email field.
data CsvRow = CsvRow
  { crEmail :: ByteString
  , crRows  :: NamedRecord
  } deriving (Show)

parse :: NamedRecord -> CsvRow
parse hm = CsvRow
             email
             hm
      where
        maybeEmail = lookup "Email" hm <|> lookup "EMAIL" hm <|> lookup "email" hm
        email = fromMaybe "" maybeEmail

-- | Read template file, connect to the gmail server and
-- traverse the rows in the grade csv file with sendMail.
-- The template file is also parsed once.
doGradeMail :: Vector CsvRow -> AppT IO ()
doGradeMail vect = do
  settings <- ask
  let AppSettings {..} = settings
  et <- liftIO $ readTemplateFile appTemplateFile
  case et of
    Left err -> liftIO . putStrLn $ "Error parsing template file: " ++ err
    Right template ->
      liftIO . bracket (SMTP.connectSMTPS appSmtpServer) SMTP.closeSMTP $ \conn -> do
        _ <- SMTP.login conn appSmtpUsername appSmtpPassword
        let sendMails = unAppT . traverse_ (sendMail template conn) $ vect
        runReaderT sendMails settings

-- | Construct the email body. If the row is missing any of the used fields,
-- it will not send an email.
sendMail :: Template -> SMTP.SMTPConnection -> CsvRow -> AppT IO ()
sendMail tpl conn row@CsvRow{..} = do
  settings <- ask
  let AppSettings {..} = settings
  let receiver = decodeUtf8 crEmail
  let result = constructBody row tpl
  case result of
    Partial {..} -> liftIO . print $ "Missing fields: " ++ show context
    Final strictText -> do
      let mail =
            SMTP.simpleMail
              (toAddress appEmailFrom)
              [toAddress receiver]
              []
              []
              appEmailTitle
              [Mime.plainPart $ fromStrict strictText]
      liftIO $ SMTP.renderAndSend conn mail
      liftIO . putStrLn $ "Sent email to " ++ T.unpack receiver
  where
    toAddress = SMTP.Address Nothing

-- | crRows is a `HashMap ByteString ByteString`, so we first create a [(ByteString,ByteString)].
-- Then, we fmap into the array with a bimap over the tuples, getting [(Text, Text)].
-- glabrous' `partialProcess'` takes the template and a Context (which can be creaed with fromList)
-- `partialProcess'` returns `Partial` if there are unmatched items in the template and
-- `Final text` if it completed replacing.
constructBody :: CsvRow -> Template -> Result
constructBody CsvRow{..} template =
  partialProcess' template (fromList . fmap (bimap decodeUtf8 decodeUtf8) . toList $ crRows)
