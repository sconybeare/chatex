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
import           Data.Functor            ((<$>))

multiUser :: TVar [TChan Text] -> ScottyM ()
multiUser bufs = do
  get "/submit/:added" $ do
    newMsg <- param "added"
    liftIO $ atomically $ do
      chans <- readTVar bufs
      sequence_ $ map (flip writeTChan newMsg) chans
    html $ mconcat ["<p>", pack $ show newMsg, "</p>"]
    liftIO $ print newMsg
  
  get "/status/:user" $ do
    uName <- fromInteger . read <$> param "user"
    msg <- liftIO $ atomically $ do
      chans <- readTVar bufs
      readTChan (chans !! uName)
    html $ mconcat ["<p>", pack $ show msg, "</p>"]
  
  get "/add" $ do
    liftIO $ atomically $ do
      x <- newTChan :: STM (TChan Text)
      modifyTVar' bufs (x:)
    b <- liftIO $ atomically $ readTVar bufs
    html $ mconcat ["<p>", pack $ show $ length b, "</p>"]

main :: IO ()
main = do
  userBuffers <- atomically $ (newTVar ([] :: [TChan Text]))
--  scotty 3000 staticUser
  scotty 3000 $ multiUser userBuffers
