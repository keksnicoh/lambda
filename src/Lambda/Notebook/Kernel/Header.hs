module Lambda.Notebook.Kernel.Header where

import qualified Data.UUID as U
import Lambda.Lib.Language (Scope, Statement, StdOut)
import Lambda.Notebook.Kernel.Model (Kernel, Register)
import Lambda.Notebook.Storage (CreateHandleM, InsertM, LookupM, ReadM)

-- api ------------------------------------------------------------------------

-- |  evaluates a statemant under a certain scope producing a stream output
type RunStatementM m =
  Scope ->
  Statement ->
  StdOut m (Either String Scope)

-- | returns a given kernel handle, if exists
type LookupKernelHandleM h m = LookupM U.UUID (h Kernel) m

-- | registers a kernel handle
type RegisterKernelHandleM h m = InsertM U.UUID (h Kernel) m

-- | create a new kernel handle
type CreateKernelHandleM h m = CreateHandleM h Kernel m

-- | read the whole register
type ReadRegisterM h m = ReadM (Register h) m

-- type classes ---------------------------------------------------------------

class HasMaxNumberOfKernels a where
  getMaxNumberOfKernels :: a -> Int