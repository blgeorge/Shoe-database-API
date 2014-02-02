{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO) --IO in Scotty ActionM
import           Data.Aeson (object,(.=)) --JSON
import           Data.Aeson.Types --JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import           Data.Int
import qualified Data.Text.Lazy as T
import qualified Database.SQLite3 as SQL
--import           Network.Wai.Middleware.RequestLogger

import           GHC.Generics
import           Network.HTTP.Types --for server response statues
import           System.Directory --for image files
import           System.IO
import           System.FilePath.Posix
import           Web.Scotty

--JSON data type
data JSONShoe = JSONShoe {
      jssDesc :: T.Text
    , jssSize :: Int64
    , jssColor :: T.Text
    , jssFile :: B.ByteString
  } deriving (Show,Generic)

instance FromJSON JSONShoe
instance ToJSON JSONShoe

(.++) :: T.Text -> T.Text -> T.Text
a .++ b = T.concat [a,b]

packshow :: Show a => (a -> T.Text)
packshow = T.pack . show

--Scotty HTML funcs
htmlH2 :: T.Text -> T.Text
htmlH2 s = "<h2>" .++ s .++ "</h2>"

htmlDiv :: T.Text -> T.Text
htmlDiv s = "<div>" .++ s .++ "</div>"

shoeImgTag :: T.Text -> T.Text
shoeImgTag i = "<img src=\"/images/shoe-" .++ i .++ ".jpg\" />"

--Persistent database funcs
insertShoe :: SQL.Database -> JSONShoe -> IO (Either T.Text Int64)
insertShoe db (JSONShoe d s c _) = do
  stmt <- SQL.prepare db "INSERT INTO shoes (description,size,color) values (?1, ?2, ?3)"
  SQL.bindSQLData stmt 1 (SQL.SQLText $ T.toStrict d)
  SQL.bindSQLData stmt 2 (SQL.SQLInteger s)
  SQL.bindSQLData stmt 3 (SQL.SQLText $ T.toStrict c)
  res <- SQL.step stmt >> SQL.changes db --perform insert command and get number of rows changed
  case res of
    1 -> SQL.lastInsertRowId db >>= (return . Right)
    _ -> (return . Left) "Error entering information to database."

writeShoeFile :: B.ByteString -> FilePath -> IO ()
writeShoeFile b f = do
  h <- openFile f WriteMode
  B.hPut h b
  putStrLn $ "Written image file " ++ f
  hFlush h
  hClose h

--Scotty route actions
postJSON :: SQL.Database -> ActionM ()
postJSON db = do
  s <- (jsonData :: (ActionM JSONShoe))
  let b64 = B64.decodeLenient $ jssFile s
  res <- liftIO $ insertShoe db s
  case res of
    Left err -> do
      json $ object ["error" .= ("myErrorMessage" :: T.Text), "redirect" .= ("/failure" :: T.Text)]
      status internalServerError500
    Right shoeID -> do
      let shoeImgFile = "images" </> ("shoe-" ++ (show shoeID) ++ ".jpg") --TODO account for non-jpeg images
      h <- liftIO $ writeShoeFile b64 shoeImgFile
      liftIO $ putStrLn $ show s --debugging
      json $ object ["ok" .= ("ok" :: T.Text) ,"redirect" .= (("/shoe/" .++ (packshow shoeID)) :: T.Text) ]

selectShoe :: SQL.Database -> Int64 -> IO (Either T.Text (T.Text,Int64,T.Text)) --probably should replace this with more complex type
selectShoe db i = do
  stmt <- SQL.prepare db "SELECT * FROM shoes WHERE id = ?"
  SQL.bindSQLData stmt 1 (SQL.SQLInteger i)
  res <- SQL.step stmt
  case res of
    SQL.Done -> return $ Left $ "No shoe with ID " .++ (packshow i) .++ " could be found."
    SQL.Row -> do
      col <- SQL.columns stmt
      case col of
        [_, SQL.SQLText d, SQL.SQLInteger s, SQL.SQLText c] -> return $ Right (T.fromStrict d,s,T.fromStrict c)
        _ -> return $ Left $ "Improper row entry for ID " .++ (packshow i)

getShoe :: SQL.Database -> T.Text -> ActionM ()
getShoe db sid = do
  let id = (read (T.unpack sid) :: Int64)
  res <- liftIO $ selectShoe db id
  case res of
    Left err -> html err
    Right (d,s,c) -> do
      let bdy = T.intercalate "<br/>" $ [
              htmlH2 d
            , htmlDiv $ packshow s
            , htmlDiv c
            , shoeImgTag sid]
      html bdy

getImg :: T.Text -> ActionM ()
getImg s = do
  let s' = "images" </> (T.unpack s)
  f <- liftIO $ doesFileExist s'
  if f
    then do
      img <- liftIO $ BL.readFile s'
      setHeader "Content-type" "image/jpeg"
      raw img
    else liftIO $ putStrLn $ "Could not find image file " ++ s'

getIDs :: SQL.Statement -> [Int64] -> IO [Int64]
getIDs stmt accum = do
  res <- SQL.step stmt
  case res of
    SQL.Done -> (return . reverse) accum
    SQL.Row -> do
      col <- SQL.columns stmt
      case col of
        [SQL.SQLInteger i] -> getIDs stmt (i:accum)
        _ -> do
          putStrLn "Improper ID row in index call" --debugging
          getIDs stmt accum

getIndex :: SQL.Database -> ActionM ()
getIndex db = do
  stmt <- liftIO $ SQL.prepare db "SELECT id FROM shoes"
  ids <- liftIO $ getIDs stmt []
  let ids' = map packshow ids
  let mkSrc x = "/shoe/" .++ x
  let mkA x = "<a href=\"" .++ (mkSrc x) .++ "\">Shoe #" .++ x .++ "</a>"
  let idLinks = T.intercalate "<br/>" $ map mkA ids'
  let newShoe = "<a href=\"/json-form\">Add new shoe entry</a>"
  let bdy = T.intercalate "<hr width=\"80%\" />" [htmlH2 "Welcome to the shoe repository!",idLinks,newShoe]
  html bdy

--Helper functions
parseID :: (T.Text -> ActionM ()) -> T.Text -> ActionM ()
parseID f id = if T.all (`elem` "0123456789") id
  then f id
  else html $ "<h3>" .++ id .++ " is an invalid shoe ID</h3>"

main = do
  db <- SQL.open "shoe.sqlite3"
  runScotty db
  SQL.close db

runScotty db = scotty 8888 $ do
--  middleware logStdoutDev
  get (capture "/json-form") $ file "json-form.html"
  get (capture "/submitted") $ html "<h3>Thenk you foar yu0r submission</h3>"
  get (regex "^(/|/index)$") $ getIndex db
  get (capture "/shoe/:id") $ param "id" >>= parseID (getShoe db)
  get (capture "/images/:id") $ param "id" >>= getImg
  post (capture "/json") $ postJSON db
