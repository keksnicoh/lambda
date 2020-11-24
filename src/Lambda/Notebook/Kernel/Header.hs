module Lambda.Notebook.Kernel.Header where

import Data.IORef (IORef)
import qualified Data.UUID as U
import Lambda.Lib.Language (Scope, Statement, StdOut)
import Lambda.Notebook.Kernel.Model (Kernel)
import Lambda.Notebook.Storage (InsertM, LookupM)

type RunStatementM m =
  Scope ->
  Statement ->
  StdOut m (Either String Scope)

type LookupKernelHandleM h m = LookupM U.UUID (h Kernel) m

type RegisterKernelIORefM m = InsertM U.UUID (IORef Kernel) m
