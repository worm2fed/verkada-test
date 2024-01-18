module Main
  ( main
  ) where

import Relude

import Main.Utf8 (withUtf8)

import Server (server)

main :: IO ()
main = withUtf8 server
