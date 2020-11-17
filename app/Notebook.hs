module Notebook where

import Lambda.Notebook.Server (notebookServer)

main :: IO ()
main = notebookServer 1337