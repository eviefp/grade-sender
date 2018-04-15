module Lib
    ( parseGradeFiles
    , parse
    , doGradeMail
    , CsvRow (..)
    ) where


import           Control.Applicative         (Alternative)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask, local)
import           Data.Attoparsec.ByteString  (parseOnly)
import           Data.ByteString             (ByteString, readFile)
import           Data.Csv                    (NamedRecord)
import           Data.Csv.Parser             (csvWithHeader,
                                              defaultDecodeOptions)
import           Data.Foldable               (traverse_)
import           Data.HashMap.Strict         (lookupDefault)
import           Data.Text                   (unpack)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Text.Lazy              (Text)
import           Data.Vector                 (Vector)
import           Network.HaskellNet.Auth     (Password, UserName)
import           Network.HaskellNet.SMTP     (AuthType (LOGIN), SMTPConnection,
                                              sendPlainTextMail)
import           Network.HaskellNet.SMTP.SSL (authenticate, doSMTPSTARTTLS)
import           Prelude                     (Applicative, Either (Left, Right),
                                              FilePath, Functor, IO,
                                              Maybe (Just, Nothing), Monad,
                                              Show, String, pure, putStrLn, snd,
                                              ($), (++), (.))

(∘) ∷ (b → c) → (a → b) → a → c
(∘) = (.)

infixr 9 ∘

data AppSettings = AppSettings
  { appSmtpServer    ∷ SMTPHostName
  , appSmtpUsername  ∷ UserName
  , appSmtpPassword  ∷ Password
  , appEmailFrom     ∷ String
  , appGradesCsvFile ∷ FilePath
  } deriving (Show)

newtype AppT m a = AppT
  { unAppT :: ReaderT AppSettings m a
  } deriving (Functor, Applicative, Monad, Alternative, MonadIO)

instance Monad m ⇒ MonadReader AppSettings (AppT m) where
  ask = AppT ask
  local f (AppT app) = AppT (local f app)

parseGradeFiles ∷ FilePath → AppT IO (Maybe (Vector NamedRecord))
parseGradeFiles path = do
  settings ← ask
  bs <- liftIO $ readFile path
  let res = parseOnly (csvWithHeader defaultDecodeOptions) bs
  case res of
    Left err -> do
      liftIO ∘ putStrLn $ "Parse error:" ++ err
      pure Nothing
    Right pr → pure . Just ∘ snd $ pr


data CsvRow = CsvRow
  { crName    ∷ ByteString
  , crSurname ∷ ByteString
  , crGroup   ∷ ByteString
  , crEmail   ∷ ByteString
  , crRows    ∷ NamedRecord
  } deriving (Show)

parse ∷ NamedRecord → CsvRow
parse hm = CsvRow
             (lookupDefault "" "Prenume" hm)
             (lookupDefault "" "Nume"    hm)
             (lookupDefault "" "Grupa"   hm)
             (lookupDefault "" "Email"   hm)
             hm

type SMTPHostName = String

doGradeMail ∷ SMTPHostName → Vector CsvRow → IO ()
doGradeMail host vect =
  doSMTPSTARTTLS host $ \conn -> do
    authSucceed ← authenticate LOGIN "user" "pass" conn
    if authSucceed
      then
        traverse_ (sendMail "blah" conn) vect
      else putStrLn "Authentication failed."

sendMail ∷ Text → SMTPConnection → CsvRow → IO ()
sendMail body conn csvr = do
  let CsvRow {..} = csvr
  sendPlainTextMail (unpack ∘ decodeUtf8 $ crEmail) "from" "title" body conn
