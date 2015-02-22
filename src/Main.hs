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

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (liftIO)
import           Data.Monoid             (mconcat)
import           Data.Text.Lazy
import           Web.Scotty

main :: IO ()
main = do
  msg <- newMVar []
  scotty 3000 $ do
    get "/submit/:added" $ do
      newMsg <- param "added"
      liftIO $ modifyMVar_ msg (return . (newMsg :))
      html newMsg
    get "/status" $ do
      msgs <- liftIO $ readMVar msg
      html $ mconcat ["<p>", pack $ show msgs, "</p>"]
