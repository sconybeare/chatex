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
import           Data.Monoid             (mconcat)
import           Data.Text.Lazy          (pack, Text)
import           Web.Scotty
import qualified Data.Map.Strict as M
import           Data.Map.Strict         (empty, Map)

multiUser :: TVar (Map Text (TChan Text)) -> ScottyM ()
multiUser bufs = do
  get "/submit/:added" $ do
    newMsg <- param "added"
    liftIO $ atomically $ do
      chans <- readTVar bufs
      sequence_ $ map (flip writeTChan newMsg . snd) (M.toList chans)
    html $ mconcat ["<p>", pack $ show newMsg, "</p>"]
    liftIO $ print newMsg
  

  get "/current/:user" $ do
    uName <- param "user"
    msg <- liftIO $ atomically $ do
      chans <- readTVar bufs
      case M.lookup uName chans of
        Just usrBuf -> peekTChan usrBuf
        Nothing     -> return "No such user"
    html $ mconcat ["<p>", msg, "</p>"]

  get "/next/:user" $ do
    uName <- param "user"
    msg <- liftIO $ atomically $ do
      chans <- readTVar bufs
      case M.lookup uName chans of
        Just usrBuf -> readTChan usrBuf
        Nothing     -> return "No such user"
    html $ mconcat ["<p>", "Queue advanced: ", msg, "</p>"]

  
  get "/add/:user" $ do
    uName <- param "user"
    liftIO $ atomically $ do
      x <- newTChan :: STM (TChan Text) --find better name
      modifyTVar' bufs $ M.insert uName x
    html $ mconcat ["<p>", uName, "</p>"]

main :: IO ()
main = do
  userBuffers <- atomically $ (newTVar (empty :: Map Text (TChan Text)))
  scotty 3000 $ multiUser userBuffers
