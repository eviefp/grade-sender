module Lib
    ( parseGradeFiles
    , parse
    , doGradeMail
    , CsvRow (..)
    ) where


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
import           Network.HaskellNet.SMTP     (AuthType (LOGIN), SMTPConnection,
                                              sendPlainTextMail)
import           Network.HaskellNet.SMTP.SSL (authenticate, doSMTPSTARTTLS)
import           Prelude                     (Either (Left, Right), FilePath,
                                              IO, Maybe (Just, Nothing), Show,
                                              String, pure, putStrLn, snd, ($),
                                              (++), (.))

parseGradeFiles :: FilePath -> IO (Maybe (Vector NamedRecord))
parseGradeFiles path = do
  bs <- readFile path
  let res = parseOnly (csvWithHeader defaultDecodeOptions) bs
  case res of
    Left err -> do
      putStrLn $ "Parse error:" ++ err
      pure Nothing
    Right pr -> pure . Just . snd $ pr


data CsvRow = CsvRow
  { crName    :: ByteString
  , crSurname :: ByteString
  , crGroup   :: ByteString
  , crEmail   :: ByteString
  , crRows    :: NamedRecord
  } deriving (Show)

parse :: NamedRecord -> CsvRow
parse hm = CsvRow
             (lookupDefault "" "Prenume" hm)
             (lookupDefault "" "Nume"    hm)
             (lookupDefault "" "Grupa"   hm)
             (lookupDefault "" "Email"   hm)
             hm

type SMTPHostName = String

doGradeMail :: SMTPHostName -> Vector CsvRow -> IO ()
doGradeMail host vect = do
  doSMTPSTARTTLS host $ \conn -> do
    authSucceed <- authenticate LOGIN "user" "pass" conn
    if authSucceed
      then do
        traverse_ (sendMail "blah" conn) vect
      else putStrLn "Authentication failed."

sendMail :: Text -> SMTPConnection -> CsvRow -> IO ()
sendMail body conn csvr = do
  let CsvRow {..} = csvr
  sendPlainTextMail (unpack . decodeUtf8 $ crEmail) "from" "title" body conn
