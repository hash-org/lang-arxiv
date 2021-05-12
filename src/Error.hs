-- | Hash error definitions.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Error where

import Parse.Boot

-- | Any Hash compiler error
data HashError
  = -- | Module Import errors
    ModuleImportError String
  | -- | Typechecking error has a message and an assocaited AstNode
    TypeError String (AstNode ())
  | -- | Code emit Error
    EmitError String
  | -- | AST/tokenisation error
    ParsingError String
  | -- | Something bad really happened
    InternalError String
  deriving (Show)
