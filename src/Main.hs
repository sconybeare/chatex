-- Main.hs
-- Copyright 2015 Sebastian Conybeare <sebmathguy@gmail.com>
-- This file is part of chatex.
--    chatex is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    chatex is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY-- without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with chatex. If not, see <http://www.gnu.org/licenses/>.

module Main where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class  (liftIO)
import           Data.Text.Lazy          (Text)
import           Web.Scotty
import qualified Data.Map.Strict as M
import           Data.Map.Strict         (empty, Map)
import           Data.Aeson              (ToJSON, toJSON, (.=), object)
import           Data.Functor            ((<$>))

newtype User = User Text deriving (Ord, Eq, Show)
data Message = Error Text | Message Text

instance ToJSON User where
    toJSON (User name) = object ["User" .= name]

instance ToJSON Message where
    toJSON (Error e) = object ["error" .= e]
    toJSON (Message m) = object ["message" .= m]

multiUser :: TVar (Map User (TChan Text)) -> ScottyM ()
multiUser bufs = do
  get "/submit/:added" $ do
    newMsg <- param "added"
    liftIO $ atomically $ do
      chans <- readTVar bufs
      sequence_ $ map (flip writeTChan newMsg . snd) (M.toList chans)
    json $ Message newMsg
    liftIO $ print newMsg
  

  get "/current/:user" $ do
    uName <- param "user"
    msg <- liftIO $ atomically $ do
      chans <- readTVar bufs
      case M.lookup (User uName) chans of
        Just usrBuf -> fmap Message (peekTChan usrBuf)
        Nothing     -> return $ Error "No such user."
    json msg


  get "/next/:user" $ do
    uName <- param "user"
    msg <- liftIO $ atomically $ do
      chans <- readTVar bufs
      case M.lookup (User uName) chans of
        Just usrBuf -> Message <$> readTChan usrBuf
        Nothing     -> return $ Error "No such user"
    json msg

  
  get "/add/:user" $ do
    user <- param "user"
    liftIO $ atomically $ do
      x <- newTChan :: STM (TChan Text) --find better name
      modifyTVar' bufs $ M.insert (User user) x
    json $ user

main :: IO ()
main = do
  userBuffers <- atomically $ (newTVar (empty :: Map User (TChan Text)))
  scotty 3000 $ multiUser userBuffers