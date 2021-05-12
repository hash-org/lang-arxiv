{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Hash AST definitions and parsers. Note: this parser is exceptionally
-- | terrible when running into syntactic errors. This is because it has to
-- | backtrack and re-test a large number of possibilities. In the future, we
-- | hope to get rid of this and just use a Parser generator. Additionally,
-- | because Megaparsec (the library that is used) doesn't have a good mechanism
-- | for errors bubbling up because of the 'try' construct, parsing error messages
-- | are really bad.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Parse.Ast where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Lens ((%~))
import Control.Monad (unless)
import Control.Monad.State (MonadTrans (lift), modify)
import Data.Char (isUpper)
import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Set.Ordered ((<|))
import qualified Data.Set.Ordered as OS
import Panic
import Parse.Boot
import qualified Parse.Lexer as L
import Report (reportErrorWithTokens)
import Text.Megaparsec (between, (<?>))
import qualified Text.Megaparsec as M

constructParseError :: (M.VisualStream s, M.TraversableStream s, M.ShowErrorComponent e) => M.ParseErrorBundle s e -> String
constructParseError m = "Failed to parse statement\n" ++ M.errorBundlePretty m

emptyModuleContext :: FilePath -> ModuleContext
emptyModuleContext _ = ModuleContext OS.empty

getOpeningBracket :: BrackettedType -> L.Bracket
getOpeningBracket Set = L.LeftBrace
getOpeningBracket Tuple = L.LeftParenthesis

getClosingBracket :: BrackettedType -> L.Bracket
getClosingBracket Set = L.RightBrace
getClosingBracket Tuple = L.RightParenthesis

-- | Create a name which is illegal to users, for internal use.
internalName :: String -> Name
internalName x = Name $ '$' : x

-- {-# INLINE pBraces #-}
pBraces :: Parser a -> Parser a
pBraces = between (pSpecific L.pBracket L.LeftBrace) (pSpecific L.pBracket L.RightBrace)

-- | Create an Ast node at the current position
pAstNode :: Parser a -> Parser (AstNode a)
pAstNode inner = do
  beginPos <- M.getOffset
  i <- inner
  endPos <- M.getOffset
  f <- M.getSourcePos
  return $ AstNode {body = i, offsetBegin = beginPos, offsetEnd = endPos, filename = M.sourceName f}

-- | Create an Ast node with given begin and end
astNodeWithSrc :: Int -> Int -> String -> a -> AstNode a
astNodeWithSrc begin end fname body = AstNode {body = body, offsetBegin = begin, offsetEnd = end, filename = fname}

-- | Create an Ast node at the position of another Ast node
astNodeAt :: AstNode b -> a -> AstNode a
astNodeAt otherNode b = otherNode {body = b}

-- | Parse the AST
pModule :: Parser (AstNode Module)
pModule = pAstNode $ Module <$> M.many pStatement

-- | Parse elements separated by a separator, with no ending separator.
-- {-# INLINE pSeparatedByNoEnd #-}
pSeparatedByNoEnd :: Show b => Parser a -> Parser b -> Parser [b]
pSeparatedByNoEnd sep el = do
  first <- el <|> fail (reportErrorWithTokens "Expected an " ["expression"])
  rest <- M.many $ M.try $ sep >> el
  return (first : rest)

-- | Parse elements separated by a separator, with an ending separator.
-- {-# INLINE pSeparatedByWithEnd #-}
pSeparatedByWithEnd :: Parser a -> Parser b -> Parser [b]
pSeparatedByWithEnd sep el = M.many . M.try $ do
  e <- el
  _ <- sep
  return e

-- | Parse elements separated by a separator, with an optional ending separator.
-- {-# INLINE pSeparatedByOptEnd #-}
pSeparatedByOptEnd :: Parser a -> Parser b -> Parser [b]
pSeparatedByOptEnd sep el = do
  first <- el
  rest <- M.many . M.try $ sep >> el
  _ <- M.optional sep
  return (first : rest)

-- ############################################################
--                        Identifiers
-- ############################################################

-- | Parse a single name (identifier)
pName :: Parser (AstNode Name)
pName = pAstNode $ L.pIdentifier <&> \(L.Identifier i) -> Name i

-- | Parse a specific value of the given parser
pSpecific :: Eq a => L.ShowRepr a => Parser a -> a -> Parser ()
pSpecific p a = (p >>= (\x -> unless (x == a) empty)) <?> L.showRepr a

-- | Parse an access name (qualified name)
-- e.g math::sin
pAccessName :: Parser (AstNode AccessName)
pAccessName = pAstNode $ AccessName <$> pSeparatedByNoEnd (pSpecific L.pOperator L.Namespace) pName

-- ############################################################
--                             Types
-- ############################################################

-- | Parse a type
pType :: Parser (AstNode Type)
pType =
  M.try pFunctionType <|> M.try pTupleType <|> M.try pListType <|> M.try pSetType <|> M.try pMapType
    <|> M.try (pAstNode (pSpecific L.pOperator L.QuestionMark >> return ExistentialType))
    <|> pNamedType

-- | Parse a function type (e.g. (i32) => str)
pFunctionType :: Parser (AstNode Type)
pFunctionType = pAstNode $ do
  -- get the arguments for the function (the same as a tuple type)
  fArgTypes <-
    M.between
      (pSpecific L.pBracket L.LeftParenthesis)
      (pSpecific L.pBracket L.RightParenthesis)
      (M.optional . M.try $ pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pType) -- representing the empty brackketed type
  let argTypes = fromMaybe [] fArgTypes

  _ <- pSpecific L.pOperator L.Arrow

  FunctionType argTypes <$> pType

-- | Parse a named type (e.g. Result<u32>)
pNamedType :: Parser (AstNode Type)
pNamedType = pAstNode $ do
  accName <- pAccessName
  case accName of
    AstNode (AccessName [AstNode (Name "_") _ _ _]) _ _ _ -> return InferType
    AstNode (AccessName [name]) _ _ _ | isTypeVar name -> return $ TypeVar name
    _ -> do
      typeArgs <- (M.notFollowedBy (pSpecific L.pBracket L.LeftAngleBracket) >> return []) <|> parseTypeArgs
      return $ NamedType accName typeArgs
  where
    -- type var if single upper char
    isTypeVar (AstNode (Name [ch]) _ _ _) = isUpper ch
    isTypeVar _ = False

    -- parsing type arguments
    parseTypeArgs =
      M.between
        (pSpecific L.pBracket L.LeftAngleBracket)
        (pSpecific L.pBracket L.RightAngleBracket)
        $ pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pType

-- | Parse a list type (e.g. [i32])
pListType :: Parser (AstNode Type)
pListType = do
  beginPos <- M.getOffset
  innerType <- M.try (M.between (pSpecific L.pBracket L.LeftSquareBracket) (pSpecific L.pBracket L.RightSquareBracket) pType)
  endPos <- M.getOffset
  f <- M.sourceName <$> M.getSourcePos

  return $
    astNodeWithSrc
      beginPos
      endPos
      f
      ( NamedType
          (astNodeWithSrc beginPos endPos f (AccessName [astNodeWithSrc beginPos endPos f (Name "List")]))
          [innerType]
      )

-- | Parse a set type (e.g. {str})
pSetType :: Parser (AstNode Type)
pSetType = do
  beginPos <- M.getOffset
  innerType <- M.try (pBraces pType)
  endPos <- M.getOffset

  f <- M.sourceName <$> M.getSourcePos
  return $
    astNodeWithSrc
      beginPos
      endPos
      f
      ( NamedType
          (astNodeWithSrc beginPos endPos f (AccessName [astNodeWithSrc beginPos endPos f (Name "Set")]))
          [innerType]
      )

-- | Parse a map type (e.g. {str:bool})
pMapType :: Parser (AstNode Type)
pMapType = do
  beginPos <- M.getOffset
  (k, v) <-
    M.try
      ( pBraces $
          do
            k <- pType
            pSpecific L.pOperator L.Colon
            v <- pType
            return (k, v)
      )
  endPos <- M.getOffset
  f <- M.sourceName <$> M.getSourcePos

  return $
    astNodeWithSrc
      beginPos
      endPos
      f
      ( NamedType
          (astNodeWithSrc beginPos endPos f (AccessName [astNodeWithSrc beginPos endPos f (Name "Map")]))
          [k, v]
      )

-- | Parse a tuple type (e.g. (u16, u32, ubig))
pTupleType :: Parser (AstNode Type)
pTupleType = do
  beginPos <- M.getOffset
  types <-
    M.between
      (pSpecific L.pBracket L.LeftParenthesis)
      (pSpecific L.pBracket L.RightParenthesis)
      ( ( pSpecific L.pOperator L.Comma >> return [] -- representing the empty brackketed type
        )
          <|> M.try
            ( do
                first <- pType
                _ <- pSpecific L.pOperator L.Comma -- we always require the first element with a comma
                rest <- M.optional $ pSeparatedByOptEnd (pSpecific L.pOperator L.Comma) pType

                case rest of
                  Just r -> return $ first : r
                  Nothing -> return [first]
            )
      )
  endPos <- M.getOffset
  f <- M.sourceName <$> M.getSourcePos
  return $ tupleType beginPos endPos f types
  where
    tupleType beginPos endPos f types =
      astNodeWithSrc
        beginPos
        endPos
        f
        ( NamedType
            (astNodeWithSrc beginPos endPos f (AccessName [astNodeWithSrc beginPos endPos f (Name "Tuple")]))
            types
        )

-- ############################################################
--                        Body Block
-- ############################################################

pBlock :: Parser (AstNode Block)
pBlock =
  (M.try (M.lookAhead (pSpecific L.pKeyword L.If)) >> pIf)
    <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.Match)) >> pMatch)
    <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.For)) >> pFor)
    <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.While)) >> pWhile)
    <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.Loop)) >> pLoop)
    <|> (M.try (M.lookAhead (pSpecific L.pBracket L.LeftBrace)) >> pBodyBlock)

-- | Parser used to parse interactive statements within the REPL
pREPL :: Parser (AstNode REPLBlock)
pREPL = do
  beginPos <- M.getOffset
  (s, e) <- pBodyInner
  endPos <- M.getOffset
  f <- M.getSourcePos

  let surrNode = astNodeWithSrc beginPos endPos (M.sourceName f)

  -- If an expression was given, print it.
  let printExpr expr =
        surrNode
          ( FunctionCall
              (surrNode (Variable (surrNode (AccessName [surrNode (Name "print")]))))
              ( surrNode $
                  FunctionCallArgs
                    Nothing
                    [ surrNode $
                        FunctionCall
                          (surrNode (Variable (surrNode (AccessName [surrNode (Name "conv")]))))
                          (surrNode $ FunctionCallArgs Nothing [expr] "<REPL>")
                    ]
                    "<REPL>"
              )
          )

  M.eof <|> fail "Issue found here" -- @ErrorReporting: probably update this later
  return $ astNodeWithSrc beginPos endPos (M.sourceName f) $ REPLBlock s (printExpr <$> e)

-- | Parse a generic body block
pBodyBlock :: Parser (AstNode Block)
pBodyBlock = pAstNode $ do
  (s, e) <- pBraces (M.try pBodyInner)
  return $ Body s e

-- | Parse a body inner block which can be made up of a list of statements and then  a
-- | final expresison. Since the parser tries to parse as many statements as possible, it can
-- | be the case that too many statements are parsed and thus we have to perform a 'AstNode'
-- | conversion.
pBodyInner :: Parser ([AstNode Statement], Maybe (AstNode Expression))
pBodyInner =
  do
    statements <- M.many . M.try $ pStatement
    maybeExpr <- M.optional . M.try $ pExpression

    case statements of
      [] -> return (statements, maybeExpr)
      _ -> do
        -- This is a bit of a hack but because the block parser might try to
        -- take to many statements, and it doesn't take in account for the last
        -- expression, this will not account for it.
        case maybeExpr of
          Nothing -> do
            -- take the last statement out of the 'parsed' statements and then
            -- check to see if it is either a 'BlockStatement' or a 'ExprStatement',
            -- the use that as the last maybe expr
            case last statements of
              n@(body -> BlockStatement nb@(body -> _)) -> return (init statements, Just $ astNodeAt n (BlockExpr nb))
              _ -> return (statements, maybeExpr)
          Just _ -> return (statements, maybeExpr)

pBodyExpression :: Parser (AstNode Expression)
pBodyExpression = pAstNode $ BlockExpr <$> pBodyBlock

-- ############################################################
--                        Conditionals
-- ############################################################

-- | Internal representations of bools
mkBool :: Bool -> AstNode a -> AstNode Expression
mkBool bool atNode = astNodeAt atNode $ Variable (astNodeAt atNode (AccessName [astNodeAt atNode (Name . boolStr $ bool)]))
  where
    boolStr True = "true"
    boolStr False = "false"

-- | Internal utility function to create an 'AstNode' that represents the '_' case in a match statement.
ignorePattern :: Int -> Int -> String -> AstNode Pattern
ignorePattern begin end f = astNodeWithSrc begin end f PatternIgnore

-- | Internal function to convert an 'if' condition into a match case.
convertIfCondIntoMatch :: AstNode Expression -> AstNode Pattern
convertIfCondIntoMatch cond =
  astNodeWithSrc
    (offsetBegin cond)
    (offsetEnd cond)
    (filename cond)
    (PatternIf (ignorePattern (offsetBegin cond) (offsetEnd cond) (filename cond)) cond)

pIfStatement :: Parser (AstNode MatchCase)
pIfStatement = pAstNode $ do
  -- parse the initial if statement
  _ <- pSpecific L.pKeyword L.If

  -- transpile the condition into a Pattern in the form of '_ if condition'
  condExpr <- pTerm (TermBinary maxBound)
  let cond = convertIfCondIntoMatch condExpr
  MatchCase cond <$> pBodyExpression

-- | This function will convert any if statement construct into a match statement.
-- | This will attempt an if statement expression into a `AstNode` that represents a
-- | `MatchCase` statement. Since we don't treat if-else statements as special, we just convert
-- | the to a match statement.
-- |
-- | The conversion process is quite simple. Take the if-statement block:
-- |
-- | >>> if a {1...} else if b {2...} else {3...}
-- |
-- | This function will convert the above statement into an AST representation:
-- |
-- | >>> match true {_ if a => {1...}; _ if b => {2...}; _ => {3...} }
-- |
-- | Note: Since 'else' statements are syntactically optional, this function will convert it
-- |       into a ignored case with an empty block like so:
-- |
-- | >>> if a {1...}  --->  match true {_ if a => {1...}; _ => {}}
pIf :: Parser (AstNode Block)
pIf = pAstNode $ do
  -- Attempt to match some `else-if` statements, since we can specify 0 or more here.
  ifBlocks <- pSeparatedByNoEnd (pSpecific L.pKeyword L.Else) pIfStatement

  f <- M.sourceName <$> M.getSourcePos
  -- attempt to parse the optional else statement, if we don't get one here,
  -- we fill it in since a match block should always have a return type
  maybeElse <- M.optional $
    pAstNode $ do
      start <- M.getOffset
      _ <- M.try $ pSpecific L.pKeyword L.Else
      MatchCase (ignorePattern start start f) <$> M.try pBodyExpression

  -- we'll need to get the pos here since we're insert a few artificial tokens
  end <- M.getOffset

  let closingElse =
        fromMaybe
          ( astNodeWithSrc
              end
              end
              f
              ( MatchCase
                  (ignorePattern end end f)
                  (astNodeWithSrc end end f (BlockExpr (astNodeWithSrc end end f (Body [] Nothing))))
              )
          )
          maybeElse
  return $ Match (mkBool True (astNodeWithSrc end end f ())) (ifBlocks ++ [closingElse])

-- | Parse a single match case
pMatchCase :: Parser (AstNode MatchCase)
pMatchCase = pAstNode $ do
  pat <- pPattern
  _ <- pSpecific L.pOperator L.Arrow
  expr <- pExpression
  _ <- pSpecific L.pOperator L.Semicolon
  return $ MatchCase pat expr

-- | Parse a match block
pMatch :: Parser (AstNode Block)
pMatch = pAstNode $ do
  _ <- pSpecific L.pKeyword L.Match
  cond <- pTerm (TermBinary maxBound)

  body <- pBraces (M.many pMatchCase)
  return $ Match cond body

-- ############################################################
--                        Literals
-- ############################################################

pLiteral :: Parser (AstNode Literal)
pLiteral =
  pAstNode
    ( (L.pCharLiteral <&> CharLiteral)
        <|> (L.pStrLiteral <&> StrLiteral)
        <|> M.try (L.pFloatLiteral <&> FloatLiteral)
        <|> M.try (L.pIntLiteral <&> IntLiteral)
        <|> M.try (pListLiteral <&> ListLiteral)
        <|> M.try (pBracketted Set pExpression <&> SetLiteral)
        <|> M.try (pBracketted Tuple pExpression <&> TupleLiteral)
        <|> (pMapLiteral <&> MapLiteral)
    )

-- | Compound literal parser. Compound literals are either struct or function
-- | literals. The reason why they are compound is because they are not atom
-- | and can include multiple spaces within them.
pCompoundLiteral :: Parser (AstNode Literal)
pCompoundLiteral =
  pAstNode
    ( M.try (pFunctionLiteral <&> \(x, y, z) -> FunctionLiteral x y z)
        <|> (pStructLiteral <&> uncurry StructLiteral)
    )

-- | Set or Tuple literal/pattern parsing. Set literals can be in the form of
-- | either '{,}' (representing an empty set), '{a,}' (a set with one item)
-- | or '{a,b,c,...}' ( a set with many items). It is also worth taking note
-- | that for two or more members of a set, the trailing comma is optional.
-- | The only difference between the tuple and the set syntax is what brackets
-- | are used to represent the beginning and ending of the statement.
pBracketted :: BrackettedType -> Parser a -> Parser [a]
pBracketted bType p =
  between
    (pSpecific L.pBracket (getOpeningBracket bType))
    (pSpecific L.pBracket (getClosingBracket bType))
    ( ( do
          _ <- pSpecific L.pOperator L.Comma
          return [] -- representing the empty brackketed type
      )
        <|> M.try
          ( do
              expr <- p
              _ <- pSpecific L.pOperator L.Comma -- we always require the first element with a comma
              exprs <- M.optional $ pSeparatedByOptEnd (pSpecific L.pOperator L.Comma) p

              case exprs of
                Just e -> return $ expr : e
                Nothing -> return [expr]
          )
    )

-- | This is the parser for lists. Lists are defined as a list of expressions
-- | that are separed by commas (with an optional trailling comma) at the end.
-- | List syntax slightly differs from set and tuple syntax in that it does not
-- | require a comma to denote no elements,  or a mandatory comma after the first
-- | element unlike in a set or tuple.
pListLiteral :: Parser [AstNode Expression]
pListLiteral =
  between
    (pSpecific L.pBracket L.LeftSquareBracket)
    (pSpecific L.pBracket L.RightSquareBracket)
    ( L.peek (pSpecific L.pBracket L.RightSquareBracket) >>= \case
        True -> return []
        False -> pSeparatedByOptEnd (pSpecific L.pOperator L.Comma) pExpression
    )

-- | This is the parser for maps. Mrrays are defined as a list of key-value
-- | expressions that are separed by commas (with an optional trailling comma) at
-- | the end. Map syntax slightly differs from set that it requires a colon operator
-- | to denote an empty map '{:}'. Otherwise, maps have similar syntactic properties to
-- | other brackketed types.
pMapLiteral :: Parser [AstNode MapEntry]
pMapLiteral = pBraces ((pSpecific L.pOperator L.Colon >> return []) <|> pSeparatedByOptEnd (pSpecific L.pOperator L.Comma) pMapEntry)

-- | The map entry parser, very simply two expressions separated by a colon operator.
pMapEntry :: Parser (AstNode MapEntry)
pMapEntry = pAstNode $ do
  key <- pExpression
  _ <- pSpecific L.pOperator L.Colon

  MapEntry key <$> pExpression

-- | Function literal parsing
pFunctionLiteral :: Parser ([AstNode FunctionParam], Maybe (AstNode Type), AstNode Expression)
pFunctionLiteral = do
  fArgs <-
    between
      (pSpecific L.pBracket L.LeftParenthesis)
      (pSpecific L.pBracket L.RightParenthesis)
      (M.optional . M.try $ pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pFunctionParam)

  -- the function could have zero arguments, so it's safe to assume that fArgs gets no arguments
  let args = fromMaybe [] fArgs

  -- maybe get the function return type, denoted with a colon and then some type
  fReturn <- M.optional . M.try $ do
    _ <- pSpecific L.pOperator L.Colon
    pType

  -- function body assignment which is specified by a '=>' (arrow operator)
  -- and followed by a block
  _ <- pSpecific L.pOperator L.Arrow
  fBlock <- pExpression

  return (args, fReturn, fBlock)

pFunctionParam :: Parser (AstNode FunctionParam)
pFunctionParam = pAstNode $ do
  paramName <- pName

  paramType <- M.optional . M.try $ do
    _ <- pSpecific L.pOperator L.Colon
    pType

  return (FunctionParam paramName paramType)

pStructAssignment :: Parser (AstNode Name, AstNode Expression)
pStructAssignment = do
  name <- pName
  _ <- pSpecific L.pOperator L.Equals
  (name,) <$> pExpression

-- | Struct literal parsing
pStructLiteral :: Parser (AstNode Type, HM.HashMap (AstNode Name) (AstNode Expression))
pStructLiteral = do
  -- get the idenfying name of the struct
  name <- pNamedType

  -- get any struct field asignments, note that since we allow structs
  -- to have 0 statements, it may be the case that you can have no statments
  -- within the braces to assign fields, or alternatively, all the struct
  -- fields have a default value, hence it's ok here to 'fail'.
  statements <-
    pBraces
      (M.optional . M.try $ pSeparatedByOptEnd (pSpecific L.pOperator L.Semicolon) pStructAssignment)

  case statements of
    Just s -> return (name, HM.fromList s)
    Nothing -> return (name, HM.empty)

-- ############################################################
--                        Imports
-- ############################################################

-- | Parsing module import statement which are in the form of a function
-- | call that have a single argument in the form of a string literal.
-- | The syntax is as follows:
-- |
-- | import("./relative/path/to/module")
-- |
-- | The path argument to imports automatically assumes that the path you provide
-- | is references '.hash' extension file or a directory with a 'index.hash' file
-- | contained within the directory.
pImport :: Parser (AstNode Literal)
pImport = pAstNode $ do
  _ <- pSpecific L.pKeyword L.Import

  moduleName <-
    between
      (pSpecific L.pBracket L.LeftParenthesis)
      (pSpecific L.pBracket L.RightParenthesis)
      L.pStrLiteral
      <|> fail "Import statement must use string literal."

  -- Add the import to list of dependencies
  lift . modify $ moduleDeps %~ (moduleName <|)

  return . StrLiteral $ moduleName

-- ############################################################
--                        Expressions
-- ############################################################

-- | Parse a statement
pStatement :: Parser (AstNode Statement)
pStatement =
  pAstNode (BlockStatement <$> pBlock)
    <|> ( ( M.try pReassignment
              <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.Let)) >> pLet)
              <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.Struct)) >> pAstNode (pStruct <&> StructDef))
              <|> M.try (pAstNode (pExpression <&> ExprStatement))
              <|> M.try (pAstNode (pSpecific L.pKeyword L.Break >> return Break))
              <|> M.try (pAstNode (pSpecific L.pKeyword L.Continue >> return Continue))
              <|> M.try (pAstNode (Return <$> (pSpecific L.pKeyword L.Return >> M.optional pExpression)))
              <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.Enum)) >> pAstNode (pEnum <&> EnumDef))
              <|> (M.try (M.lookAhead (pSpecific L.pKeyword L.Trait)) >> pAstNode (pTraitDef <&> TraitDef))
          )
            <* pSpecific L.pOperator L.Semicolon
        )

-- | Parse an expression
pExpression :: Parser (AstNode Expression)
pExpression = pTerm TermCompound

-- | Binary operator precedence
data BinaryPrecedence = Typed | MulDiv | AddSub | BitShift | Relational | EqNeq | BitAnd | BitXor | BitOr | LogicalAnd | LogicalOr
  deriving (Eq, Ord, Enum, Bounded)

-- | Get the previous precedence, or Nothing if the lowest is given
prevBinaryPrecedence :: BinaryPrecedence -> Maybe BinaryPrecedence
prevBinaryPrecedence p
  | p == minBound = Nothing
  | otherwise = Just (pred p)

data TermPrecedence = TermCompound | TermBinary BinaryPrecedence | TermUnary | TermFunc

-- | Parse a "term" expression
--
-- This parses binary expressions of the given precedence or lower, unary
-- expressions, or any other type of expression.
pTerm :: TermPrecedence -> Parser (AstNode Expression)
pTerm = \case
  TermCompound -> M.try (pAstNode $ LiteralExpr <$> pCompoundLiteral) <|> pTerm (TermBinary maxBound)
  TermBinary p -> M.try (pBinaryOpOrExpr p) <|> pTerm TermUnary
  TermUnary -> M.try pUnaryOp <|> pTerm TermFunc
  TermFunc -> M.try pFunctionCallOrPropAccess

-- | Parse an expression in parentheses
pParenExpr :: Parser (AstNode Expression)
pParenExpr = pAstNode $ M.between (pSpecific L.pBracket L.LeftParenthesis) (pSpecific L.pBracket L.RightParenthesis) (body <$> pExpression)

-- | Parse a literal expression
pLiteralExpr :: Parser (AstNode Expression)
pLiteralExpr = pAstNode $ LiteralExpr <$> pLiteral

-- | Parse a module import call
pModuleImport :: Parser (AstNode Expression)
pModuleImport = pAstNode $ Import <$> pImport

-- | Parse a variable reference
pVariable :: Parser (AstNode Expression)
pVariable = pAstNode $ Variable <$> pAccessName

-- | Parsing intrinsic definition
pIntrinsic :: Parser (AstNode Expression)
pIntrinsic = pAstNode $ IntrinsicExpr . IntrinsicKey <$> L.pIntrinsicLike

pAssignment :: Parser (AstNode Statement)
pAssignment = pAstNode $ do
  name <- pVariable
  _ <- pSpecific L.pOperator L.Equals
  Assign name <$> pExpression

-- | Parse a singular expression (literal, variable, parenthesized expression, or module import)
pSingleExpr :: Parser (AstNode Expression)
pSingleExpr = M.try pExprBlock <|> M.try pLiteralExpr <|> M.try pIntrinsic <|> M.try pVariable <|> M.try pParenExpr <|> M.try pModuleImport

-- | Parse an expression block
pExprBlock :: Parser (AstNode Expression)
pExprBlock = pAstNode $ BlockExpr <$> pBlock

-- ############################################################
--           Function call/property access/indexing
-- ############################################################

-- | Parse a property access (period followed by a name)
pPropertyAccess :: Parser (AstNode Name)
pPropertyAccess = pSpecific L.pOperator L.Period >> pName

-- | Parse an index access (e.g. my_arr[4])
pIndexExpr :: Parser (AstNode Expression)
pIndexExpr = M.between (pSpecific L.pBracket L.LeftSquareBracket) (pSpecific L.pBracket L.RightSquareBracket) pExpression

-- | Parse function call arguments, including generics
--
-- Eg. <u32>(arg1, arg2)
pFunctionCallArgs :: Parser (AstNode FunctionCallArgs)
pFunctionCallArgs = pAstNode $ do
  -- Optionally parse type args
  typeArgs <-
    M.optional . M.try $
      M.between
        (pSpecific L.pBracket L.LeftAngleBracket)
        (pSpecific L.pBracket L.RightAngleBracket)
        $ pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pType
  -- Parse function args
  args <-
    M.try (pSpecific L.pBracket L.LeftParenthesis >> pSpecific L.pBracket L.RightParenthesis >> return [])
      <|> M.try
        ( M.between
            (pSpecific L.pBracket L.LeftParenthesis)
            (pSpecific L.pBracket L.RightParenthesis)
            $ pSeparatedByOptEnd (pSpecific L.pOperator L.Comma) pExpression
        )

  currPos <- M.getSourcePos
  let sFile = M.sourceName currPos
  let sLine = M.unPos $ M.sourceLine currPos
  return $ FunctionCallArgs typeArgs args (sFile ++ ":" ++ show sLine)

-- | A chained operation
--
-- Either a property access, or a function call, or both.
data ChainedOperation
  = ChainedOperation
      (Maybe (AstNode Name))
      -- ^ A property access name, if any.
      (Maybe (AstNode FunctionCallArgs))
      -- ^ Function call args, if any.
      (Maybe (AstNode Expression))
      -- ^ Indexing call if any.

-- | Combine an expression and a ChainedOperation into another expression
mergeChainedOps :: AstNode Expression -> AstNode ChainedOperation -> AstNode Expression
mergeChainedOps subject (AstNode (ChainedOperation propAccessName funcCallInner indexExpr) _ funcEnd f) =
  indexed $ case (propAccessName, funcCallInner, indexExpr) of
    -- This shouldn't happen because we check for it in pFunctionCallOrPropAccess
    (Nothing, Nothing, Nothing) -> internalPanicPure "reached unreachable case"
    -- Just indexing, will be done by `indexed`
    (Nothing, Nothing, Just _) -> subject
    -- Dotted function call, e.g: a.b(c)
    --
    -- Here, we add the subject (a) to the argument list of b, effectively
    -- transpiling to b(a, c).  The function call argument AstNode begin and
    -- end are taken as <begin>a.b(c)<end>, rather than a.b<begin>(c)<end>.
    (Just name, Just (AstNode (FunctionCallArgs typeArgs args src) _ _ _), _) ->
      nodeBeginAt subject $ FunctionCall (makeDotSubject name) (nodeBeginAt subject (FunctionCallArgs typeArgs (subject : args) src))
    -- Normal function call, e.g: b(a, c)
    (Nothing, Just args, _) -> astNodeWithSrc (offsetBegin subject) funcEnd f $ FunctionCall subject args
    -- Property access, e.g: x.y
    (Just name, Nothing, _) -> nodeBeginAt subject $ PropertyAccess subject name
  where
    -- Create an AstNode at the same position as another AstNode
    nodeAt other = astNodeWithSrc (offsetBegin other) (offsetEnd other) f

    -- Create a variable expression from a name, used for dotted function call
    makeDotSubject name = nodeAt name (Variable (nodeAt name (AccessName [name])))

    -- An AstNode at (offsetBegin node, funcEnd)
    nodeBeginAt node = astNodeWithSrc (offsetBegin node) funcEnd f

    -- Enclose an expression in an indexing call if indexExpr is not Nothing
    indexed inner = case indexExpr of
      Just i ->
        nodeBeginAt inner $
          FunctionCall
            (nodeBeginAt inner $ Variable (nodeBeginAt inner (AccessName [nodeBeginAt inner $ Name "index"])))
            (nodeBeginAt inner $ FunctionCallArgs Nothing [inner, i] "<unnamed>")
      Nothing -> inner

-- | Parse a single expression or function call, property access, or indexing
--
-- These are in the same parser for efficiency purposes (they all start with
-- pSingleExpr)
pFunctionCallOrPropAccess :: Parser (AstNode Expression)
pFunctionCallOrPropAccess = do
  -- Parse some subject
  firstSubject <- pSingleExpr

  -- Get zero or more function call/property access/indexing
  ops <- M.many . M.try . pAstNode $ do
    propAccessName <- M.optional . M.try $ pPropertyAccess
    funcCall <- M.optional . M.try $ pFunctionCallArgs
    indexExpr <- M.optional . M.try $ pIndexExpr
    -- Make sure we got either property access, function call, or indexing, otherwise end chain
    case (propAccessName, funcCall, indexExpr) of
      (Nothing, Nothing, Nothing) -> empty
      _ -> return $ ChainedOperation propAccessName funcCall indexExpr
  -- Combine
  return $ foldl' mergeChainedOps firstSubject ops

-- ############################################################
--                   Unary/binary operations
-- ############################################################

-- | A mapping between binary operators and precendence
binaryOps :: [(BinaryPrecedence, L.Token)]
binaryOps =
  [ (EqNeq, L.Operator L.DoubleEquals),
    (EqNeq, L.Operator L.TripleEquals),
    (AddSub, L.Operator L.Add),
    (AddSub, L.Operator L.Subtract),
    (MulDiv, L.Operator L.Multiply),
    (MulDiv, L.Operator L.Divide),
    (MulDiv, L.Operator L.Modulus),
    (EqNeq, L.Operator L.NotEquals),
    (LogicalAnd, L.Operator L.LogicalAnd),
    (LogicalOr, L.Operator L.LogicalOr),
    (BitAnd, L.Operator L.BitwiseAnd),
    (BitOr, L.Operator L.VerticalBar),
    (BitXor, L.Operator L.BitwiseXor),
    (Relational, L.Operator L.LessEquals),
    (Relational, L.Operator L.GreaterEquals),
    (Relational, L.Bracket L.LeftAngleBracket),
    (Relational, L.Bracket L.RightAngleBracket),
    (Typed, L.Keyword L.As)
  ]

-- | All unary operators
unaryOps :: [L.Operator]
unaryOps =
  [ L.Not,
    L.BitwiseNot,
    L.Add,
    L.Subtract
  ]

-- | Parse a unary operation
pUnaryOp :: Parser (AstNode Expression)
pUnaryOp = do
  op <- pAstNode $ M.try pUnaryOperator
  expr <- pTerm TermUnary
  return $ unaryOpToExpression op expr

-- | Parse a unary operator
pUnaryOperator :: Parser L.Operator
pUnaryOperator =
  -- Try parse an operator
  M.try L.pOperator
    -- Match with unary operators
    >>= (\x -> if x `elem` unaryOps then return x else empty)

-- | Combine a unary operator and an expression into an expression
--
-- Each of the unary operators are substitited with their respective prelude
-- functions.
unaryOpToExpression :: AstNode L.Operator -> AstNode Expression -> AstNode Expression
unaryOpToExpression op expr = case body op of
  L.Not -> fnCall "not" [expr]
  L.BitwiseNot -> fnCall "bit_not" [expr]
  L.Subtract -> fnCall "neg" [expr]
  L.Add -> fnCall "add" [expr]
  _ -> internalPanicPure "reached unreachable case"
  where
    opNode = astNodeWithSrc (offsetBegin op) (offsetEnd op) (filename op)
    exprNode = astNodeWithSrc (offsetBegin expr) (offsetEnd expr) (filename op)
    fnCall name args =
      astNodeWithSrc
        (offsetBegin op)
        (offsetEnd expr)
        (filename op)
        ( FunctionCall
            (opNode $ Variable (opNode (AccessName [opNode (Name name)])))
            (exprNode $ FunctionCallArgs Nothing args "<unnamed>")
        )

-- | Parse a binary operation, or single term in the given precedence
pBinaryOpOrExpr :: BinaryPrecedence -> Parser (AstNode Expression)
pBinaryOpOrExpr p = do
  -- Parse first term
  first <- pTerm (maybe TermUnary TermBinary (prevBinaryPrecedence p))

  -- Parse rest, optionally
  rest <- M.optional . M.try $ do
    -- Parse operator
    op <- M.try . pAstNode $ pBinaryOperator p
    -- Parse second term
    second <- case (body op, p) of
      (L.Keyword L.As, Typed) -> Left <$> pType
      _ -> Right <$> pTerm (TermBinary p)
    return (op, second)

  f <- M.sourceName <$> M.getSourcePos

  return $ case rest of
    -- If no second term, just return first
    Nothing -> first
    -- Check for typed expression
    Just (_, Left second) -> astNodeWithSrc (offsetBegin first) (offsetEnd second) f $ TypedExpression first second
    -- Otherwise, create expression from two terms and operator
    Just (op, Right second) -> binaryOpToExpression first op second

-- | Parse a binary operator of the given precedence
pBinaryOperator :: BinaryPrecedence -> Parser L.Token
pBinaryOperator p =
  -- Try parse an operator or a bracket
  M.try (L.Operator <$> L.pOperator) <|> M.try (L.Bracket <$> L.pBracket) <|> M.try (L.Keyword <$> L.pKeyword)
    -- Match with relevant operators
    >>= (\x -> if x `elem` relevantOps then return x else empty)
  where
    -- Relevant operators are the ones with precedence = p
    relevantOps = map snd $ filter ((p ==) . fst) binaryOps

-- | Combine a binary operator and two expressions, into an expression
--
-- Each of the binary operators are substitited with their respective prelude
-- functions.
binaryOpToExpression :: AstNode Expression -> AstNode L.Token -> AstNode Expression -> AstNode Expression
binaryOpToExpression first op second = case body op of
  L.Operator L.DoubleEquals ->
    fnCall "eq" [first, second]
  L.Operator L.TripleEquals ->
    fnCall "ref_eq" [first, second]
  L.Operator L.Add ->
    fnCall "add" [first, second]
  L.Operator L.Subtract ->
    fnCall "sub" [first, second]
  L.Operator L.Multiply ->
    fnCall "mul" [first, second]
  L.Operator L.Divide ->
    fnCall "div" [first, second]
  L.Operator L.Modulus ->
    fnCall "mod" [first, second]
  L.Operator L.NotEquals ->
    fnCall "logical_not" [fnCall "eq" [first, second]]
  L.Operator L.LogicalAnd ->
    logicalOp LogicalAndOp first second
  L.Operator L.LogicalOr ->
    logicalOp LogicalOrOp first second
  L.Operator L.BitwiseAnd ->
    fnCall "bit_and" [first, second]
  L.Operator L.VerticalBar ->
    fnCall "bit_or" [first, second]
  L.Operator L.BitwiseXor ->
    fnCall "bit_xor" [first, second]
  L.Operator L.LessEquals ->
    letBind
      "val"
      (Just (fnCall "ord" [first, second]))
      ( logicalOp
          LogicalOrOp
          (fnCall "eq" [internalVar "val", var "Eq"])
          (fnCall "eq" [internalVar "val", var "Lt"])
      )
  L.Operator L.GreaterEquals ->
    letBind
      "val"
      (Just (fnCall "ord" [first, second]))
      ( logicalOp
          LogicalOrOp
          (fnCall "eq" [internalVar "val", var "Eq"])
          (fnCall "eq" [internalVar "val", var "Gt"])
      )
  L.Bracket L.LeftAngleBracket ->
    fnCall "eq" [fnCall "ord" [first, second], var "Lt"]
  L.Bracket L.RightAngleBracket ->
    fnCall "eq" [fnCall "ord" [first, second], var "Gt"]
  _ -> internalPanicPure "reached unreachable case"
  where
    opNode = astNodeWithSrc (offsetBegin op) (offsetEnd op) (filename op)
    firstToSecondNode = astNodeWithSrc (offsetBegin first) (offsetEnd second) (filename op)

    -- Create a function call node
    fnCall name args =
      firstToSecondNode
        ( FunctionCall
            (opNode $ Variable (opNode (AccessName [opNode (Name name)])))
            (firstToSecondNode $ FunctionCallArgs Nothing args "<unnamed>")
        )

    -- Create a logical op node
    logicalOp op arg1 arg2 = firstToSecondNode (LogicalOp op arg1 arg2)

    -- Create a variable node
    var name =
      firstToSecondNode
        ( Variable
            (opNode (AccessName [opNode (Name name)]))
        )

    -- Create an internal node
    internalVar name =
      firstToSecondNode
        ( Variable
            (opNode (AccessName [opNode (internalName name)]))
        )

    -- Create a block with a let binding
    --
    -- Will be used for operator desugaring, so its position is set to opNode.
    letBind name expr toReturn =
      firstToSecondNode
        ( BlockExpr
            (firstToSecondNode (Body [opNode (Let (opNode (PatternBinding (opNode (internalName name)))) Nothing Nothing expr)] (Just toReturn)))
        )

-- ############################################################
--                        Trait definitions
-- ############################################################

-- | Parse a trait definition
pTraitDef :: Parser (AstNode Trait)
pTraitDef = pAstNode $ do
  _ <- pSpecific L.pKeyword L.Trait
  name <- pName
  _ <- pSpecific L.pOperator L.Equals
  forAll <- M.try pForAll <* pSpecific L.pOperator L.Arrow
  Trait name forAll <$> pFunctionType

-- | Parse a trait bound
pTraitBound :: Parser (AstNode TraitBound)
pTraitBound = pAstNode $ do
  name <- pAccessName
  args <-
    M.between
      (pSpecific L.pBracket L.LeftAngleBracket)
      (pSpecific L.pBracket L.RightAngleBracket)
      $ pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pType
  return $ TraitBound name args

-- ############################################################
--                    Structs & Enum definitions
-- ############################################################
--

-- | The ForAll parser which parses type and trait bounds for either an Enumaration
-- | or Struct definition.
pForAll :: Parser (AstNode ForAll)
pForAll = pAstNode $ do
  types <-
    M.try $
      between
        (pSpecific L.pBracket L.LeftAngleBracket)
        (pSpecific L.pBracket L.RightAngleBracket)
        (pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pType)

  fTraits <-
    M.optional . M.try $ do
      _ <- pSpecific L.pKeyword L.Where
      pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pTraitBound

  let traits = fromMaybe [] fTraits
  return (ForAll types traits)

-- | The Struct parser, this parser will parse the struct definition including
-- | the struct name, type annotations and or trait bounds, and finally
-- | using the `pStructField` for struct fields.
pStruct :: Parser (AstNode Struct)
pStruct = pAstNode $ do
  _ <- pSpecific L.pKeyword L.Struct
  name <- pName
  _ <- pSpecific L.pOperator L.Equals

  -- check forAll decl
  fForall <- pAstNode . M.optional $ M.try pForAll

  forall <- case body fForall of
    Nothing -> return $ astNodeAt fForall $ ForAll [] []
    Just b -> do
      pSpecific L.pOperator L.Arrow <|> fail "Expecting =>"
      return b

  -- struct fields
  fields <- pBraces (M.many pStructField)

  return $ Struct name forall fields

-- | The Struct entry parser, entries like this can be in the form of a 'let'
-- | declaration, but without the 'let' keyword. Just like 'let' declarations,
-- | they can be simply declared (with or without) a type annotation, and (with or without)
-- | a default value. At this stage, we do not bother ourselves with the validity of
-- | the definition, this will be done later at the typechecking stage.
pStructField :: Parser (AstNode StructEntry)
pStructField = pAstNode $ do
  name <- pName
  fType <- M.optional . M.try $ do
    _ <- pSpecific L.pOperator L.Colon
    pType

  -- check if there is a default value
  fDefault <- M.optional . M.try $ do
    _ <- pSpecific L.pOperator L.Equals
    pExpression

  _ <- pSpecific L.pOperator L.Semicolon

  return $ StructEntry name fType fDefault

-- | The Enumaration parser, this parser will parse the enumaration definition
-- | including the Enumaration name, type annotations and or trait bounds, and
-- | finally using the `pEnumEntry` for enumaration members.
pEnum :: Parser (AstNode Enumeration)
pEnum = pAstNode $ do
  _ <- pSpecific L.pKeyword L.Enum
  name <- pName
  _ <- pSpecific L.pOperator L.Equals

  -- check forAll decl
  fForall <- pAstNode . M.optional $ M.try pForAll <* pSpecific L.pOperator L.Arrow

  -- enum members
  members <- pBraces (M.many pEnumEntry)

  let forall = fromMaybe (astNodeAt fForall $ ForAll [] []) (body fForall)
  return $ Enumeration name forall members

-- | The Enumeration entry parser, entries like this can be in the form of just an
-- | identifier name, but however enumerations can be annotated with traits or types
-- | parameters. Additionally, enumaration members can have types specified in the
-- | by specifiying the types within parenthesis after the name of the member.
-- | Here is an example taken from the Wiki:
-- |
-- | enum Result = <T, E> => {
-- |  Ok(T);
-- |  Err(E);
-- | };
-- |
-- | In the example above, the 'Ok' member has a type parameter 'T' and the 'Err'
-- | member has a type parameter 'E'.
pEnumEntry :: Parser (AstNode EnumEntry)
pEnumEntry = pAstNode $ do
  name <- pName

  -- check for types
  types <-
    M.optional . M.try $
      between
        (pSpecific L.pBracket L.LeftParenthesis)
        (pSpecific L.pBracket L.RightParenthesis)
        (pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pType)

  _ <- pSpecific L.pOperator L.Semicolon

  case types of
    Just t -> return $ EnumEntry name t
    Nothing -> return $ EnumEntry name []

-- ############################################################
--                            Loops
-- ############################################################

-- | The loop construct parser which will parse the loop keyword before returning
-- | a block which will be parsed as a loop.
pLoop :: Parser (AstNode Block)
pLoop = pAstNode $ do
  _ <- pSpecific L.pKeyword L.Loop
  Loop <$> pBlock

-- | Parse the content of a while-loop
pWhileContent :: Parser (AstNode (AstNode Expression, AstNode Block))
pWhileContent = pAstNode $ do
  _ <- pSpecific L.pKeyword L.While
  cond <- pTerm (TermBinary maxBound)
  b <- pBodyBlock
  return (cond, b)

-- | Transpile a parsed while loop into a loop-match construct.
-- | For more details check out the wiki page on loops:
-- | https://feds01.github.io/hash/loops.html
pWhile :: Parser (AstNode Block)
pWhile = do
  content <- pWhileContent
  let (cond, b) = body content
  return $
    astNodeAt content $
      Loop $
        astNodeAt content $
          Match
            cond
            [ astNodeAt content $
                MatchCase
                  ( astNodeAt cond $
                      PatternEnum
                        ( astNodeAt cond (AccessName [astNodeAt cond $ Name "true"])
                        )
                        []
                  )
                  (astNodeAt b $ BlockExpr b),
              astNodeAt content $
                MatchCase
                  ( astNodeAt cond $
                      PatternEnum
                        ( astNodeAt cond (AccessName [astNodeAt cond $ Name "false"])
                        )
                        []
                  )
                  (astNodeAt content $ BlockExpr (astNodeAt content $ Body [astNodeAt content Break] Nothing))
            ]

-- | Parse a for-loop into a pattern, an expression and a block.
pForContent :: Parser (AstNode (AstNode Pattern, AstNode Expression, AstNode Block))
pForContent = pAstNode $ do
  _ <- pSpecific L.pKeyword L.For
  pat <- pExhaustivePattern
  _ <- pSpecific L.pKeyword L.In <|> fail (reportErrorWithTokens "Expected " ["in"]) -- @@ErrorReporting

  -- since we only want a single expression or a property access
  iter <- pTerm (TermBinary maxBound)
  b <- pBodyBlock
  return (pat, iter, b)

-- | Transpile a parsed for-loop into a loop-match construct. Any for loop
-- | that is parsed will be transpiled into a 'loop' with a 'match' case
-- | statement. For example:
-- |
-- | for x in i.iter() {
-- |    body(x); // do something with x
-- | }
-- |
-- | Will be transpiled into:
-- |
-- | loop {
-- |   match next(i) {
-- |     Some(x) => body(x);
-- |     None => break;
-- |   }
-- | }
-- |
-- | For more details on the transpilation process, checkout the
-- | wiki page: https://feds01.github.io/hash/loops.html
pFor :: Parser (AstNode Block)
pFor = do
  content <- pForContent
  let (pat, iter, b) = body content
  return $
    astNodeAt content $
      Loop $
        astNodeAt content $
          Match
            ( astNodeAt
                iter
                ( FunctionCall
                    ( astNodeAt iter (Variable (astNodeAt iter (AccessName [astNodeAt iter $ Name "next"])))
                    )
                    (astNodeAt iter $ FunctionCallArgs Nothing [iter] "<unnamed>")
                )
            )
            [ astNodeAt content $
                MatchCase
                  ( astNodeAt pat $
                      PatternEnum
                        ( astNodeAt pat (AccessName [astNodeAt pat $ Name "Some"])
                        )
                        [pat]
                  )
                  (astNodeAt b $ BlockExpr b),
              astNodeAt content $
                MatchCase
                  ( astNodeAt pat $
                      PatternEnum
                        ( astNodeAt pat (AccessName [astNodeAt pat $ Name "None"])
                        )
                        []
                  )
                  (astNodeAt content $ BlockExpr (astNodeAt content $ Body [astNodeAt content Break] Nothing))
            ]

-- ############################################################
--                  Let, reassignment
-- ############################################################

-- | Let statement parser which parses three possible variations. The let keyword
-- | is parsed and then either a variable declaration, function declaration, or both.
-- | As such a name is returned before parsing a type, function, or both.
pLet :: Parser (AstNode Statement)
pLet = pAstNode $ do
  _ <- pSpecific L.pKeyword L.Let
  pat <- pExhaustivePattern <|> fail (reportErrorWithTokens "Expecting an " ["identifier", "destructing pattern"])

  bound <- M.optional . M.try $ pForAll

  varType <- M.optional . M.try $ do
    _ <- pSpecific L.pOperator L.Colon
    pType

  funcExpr <- M.optional . M.try $ do
    _ <- pSpecific L.pOperator L.Equals
    pExpression

  return $ Let pat bound varType funcExpr

-- | A list of reassignment operators
reassignmentOps :: [L.Operator]
reassignmentOps =
  [ L.Equals,
    L.AddEquals,
    L.SubtractEquals,
    L.MultiplyEquals,
    L.DivideEquals,
    L.ModulusEquals,
    L.LogicalAndEquals,
    L.LogicalOrEquals,
    L.BitwiseAndEquals,
    L.VerticalBarEquals,
    L.BitwiseXorEquals,
    L.LessEquals,
    L.GreaterEquals
  ]

-- | Parse a reassignment operator
pReassignmentOp :: Parser L.Operator
pReassignmentOp =
  -- Try parse an operator
  M.try L.pOperator
    -- Match with reassignment operators
    >>= (\x -> if x `elem` reassignmentOps then return x else empty)

-- | Parse a reassignment expression
--
-- - Normal reassignment (foo = bar)
-- - Operator reassignment (+=, -=, *=, ...)
-- - Indexing reassignment (something[0] = foo)
-- - Property access (dog.name = "Neil")
pReassignment :: Parser (AstNode Statement)
pReassignment = pAstNode $ do
  lhs <- pFunctionCallOrPropAccess
  op <- pAstNode pReassignmentOp
  rhs <- pExpression

  let opNode = astNodeAt op
  let wholeNode = astNodeWithSrc (offsetBegin lhs) (offsetEnd rhs) (filename rhs)

  -- Create a function call node
  let fnCall name args =
        wholeNode
          ( FunctionCall
              (opNode $ Variable (opNode (AccessName [opNode (Name name)])))
              (wholeNode $ FunctionCallArgs Nothing args "<unnamed>")
          )

  -- Create a logical op node
  let logicalOp op arg1 arg2 = wholeNode (LogicalOp op arg1 arg2)

  let convRhs = \l -> case body op of
        L.Equals -> rhs
        L.AddEquals -> fnCall "add" [l, rhs]
        L.SubtractEquals -> fnCall "sub" [l, rhs]
        L.MultiplyEquals -> fnCall "mul" [l, rhs]
        L.DivideEquals -> fnCall "div" [l, rhs]
        L.ModulusEquals -> fnCall "mod" [l, rhs]
        L.LogicalAndEquals -> logicalOp LogicalAndOp l rhs
        L.LogicalOrEquals -> logicalOp LogicalOrOp l rhs
        L.BitwiseAndEquals -> fnCall "bit_and" [l, rhs]
        L.VerticalBarEquals -> fnCall "bit_or" [l, rhs]
        L.BitwiseXorEquals -> fnCall "bit_xor" [l, rhs]
        _ -> internalPanicPure "unexpected reassignment operator"

  case lhs of
    AstNode (FunctionCall (AstNode (Variable (AstNode (AccessName [AstNode (Name "index") _ _ _]) _ _ _)) _ _ _) (AstNode (FunctionCallArgs _ args _) _ _ _)) _ _ _ ->
      -- Index access, assign `foo` and `bar` in `bar[foo]` to temporary
      -- variables first, to prevent duplicate evaluation.
      return $
        let -- The first and second args to index()
            [indexed, index] = args
            -- The last argument to index_mut(): index($indexed, $index)
            indexMutArg =
              convRhs
                ( wholeNode $
                    FunctionCall
                      (wholeNode (Variable (wholeNode (AccessName [wholeNode . Name $ "index"]))))
                      ( wholeNode
                          ( FunctionCallArgs
                              Nothing
                              ( map
                                  wholeNode
                                  [ Variable (wholeNode (AccessName [wholeNode (internalName "indexed")])),
                                    Variable (wholeNode (AccessName [wholeNode (internalName "index")]))
                                  ]
                              )
                              "<unnamed>"
                          )
                      )
                )
            -- The index_mut() call: index_mut($indexed, $index, <indexMutArg>)
            indexMutCall =
              Just
                ( wholeNode
                    ( FunctionCall
                        (wholeNode (Variable (wholeNode (AccessName [wholeNode . Name $ "index_mut"]))))
                        ( wholeNode
                            ( FunctionCallArgs
                                Nothing
                                [ wholeNode $ Variable (wholeNode (AccessName [wholeNode (internalName "indexed")])),
                                  wholeNode $ Variable (wholeNode (AccessName [wholeNode (internalName "index")])),
                                  indexMutArg
                                ]
                                "<unnamed>"
                            )
                        )
                    )
                )
         in -- We return a statement in the form
            -- {
            --      let $indexed = ...;
            --      let $index = ...;
            --      index_mut($indexed, $index, index($indexed, $index) <op> <rhs>)
            -- };
            ExprStatement
              ( wholeNode
                  ( BlockExpr
                      ( wholeNode
                          ( Body
                              [ -- First bind $indexed and $index:
                                wholeNode
                                  ( Let
                                      (wholeNode (PatternBinding $ wholeNode (internalName "indexed")))
                                      Nothing
                                      Nothing
                                      (Just indexed)
                                  ),
                                wholeNode
                                  ( Let
                                      (wholeNode (PatternBinding $ wholeNode (internalName "index")))
                                      Nothing
                                      Nothing
                                      (Just index)
                                  )
                              ]
                              -- Then call index_mut()
                              -- Returns void so it can be the expression in the BlockExpr.
                              indexMutCall
                          )
                      )
                  )
              )
    AstNode (PropertyAccess expr propName) _ _ _ ->
      -- Property access, assign `foo` in `foo.bar` to a temporary variable
      -- first, to prevent duplicate evaluation.
      return $
        let exprBind =
              wholeNode $
                PropertyAccess
                  (wholeNode $ Variable (wholeNode (AccessName [wholeNode (internalName "expr")])))
                  propName
         in ExprStatement
              ( wholeNode
                  ( BlockExpr
                      ( wholeNode
                          ( Body
                              [ wholeNode
                                  -- First bind the prefix
                                  ( Let
                                      (wholeNode (PatternBinding $ wholeNode (internalName "expr")))
                                      Nothing
                                      Nothing
                                      (Just expr)
                                  ),
                                -- Then call the assignment
                                wholeNode
                                  ( Assign
                                      exprBind
                                      (convRhs exprBind)
                                  )
                              ]
                              Nothing
                          )
                      )
                  )
              )
    AstNode (Variable _) _ _ _ ->
      -- Variable, we don't want to assign to temporary.
      return $ Assign lhs (convRhs lhs)
    AstNode expr _ _ _ ->
      -- Any other expression, assign to temporary.
      return $
        let exprBind = wholeNode $ Variable (wholeNode (AccessName [wholeNode (internalName "expr")]))
         in ExprStatement
              ( wholeNode
                  ( BlockExpr
                      ( wholeNode
                          ( Body
                              [ -- First bind expression to temporary
                                wholeNode
                                  ( Let
                                      (wholeNode (PatternBinding $ wholeNode (internalName "expr")))
                                      Nothing
                                      Nothing
                                      (Just (wholeNode expr))
                                  ),
                                -- Then call the assignment
                                wholeNode
                                  ( Assign
                                      exprBind
                                      (convRhs exprBind)
                                  )
                              ]
                              Nothing
                          )
                      )
                  )
              )

-- ############################################################
--                            Patterns
-- ############################################################

-- | Parse a single pattern or a combination of PatternIf and PatternOr
pPattern :: Parser (AstNode Pattern)
pPattern = pAstNode $ do
  first <- pSinglePattern -- <|> fail (reportErrorWithTokens "Expected a pattern here" [])
  cond <- M.optional . M.try $ pSpecific L.pKeyword L.If >> pTerm TermUnary
  second <- M.optional . M.try $ pSpecific L.pOperator L.VerticalBar >> pPattern
  -- second <- return Nothing
  return $ case second of
    Just s -> PatternOr (applyCond cond first) s
    Nothing -> body (applyCond cond first)
  where
    applyCond (Just c) pat = astNodeWithSrc (offsetBegin pat) (offsetEnd c) (filename c) $ PatternIf pat c
    applyCond _ pat = pat

-- | Parse an exhaustive pattern (used for 'let' and 'for' statements)
pExhaustivePattern :: Parser (AstNode Pattern)
pExhaustivePattern =
  M.try (pAstNode (pStructPattern <&> uncurry PatternStruct))
    <|> M.try pBindingPattern
    <|> M.try (pAstNode (pNamespacePattern <&> PatternNamespace))
    <|> pAstNode (pBracketted Tuple pPattern <&> PatternTuple)

-- | Parse a single pattern
pSinglePattern :: Parser (AstNode Pattern)
pSinglePattern =
  M.try pParenPattern
    <|> M.try (pAstNode $ pEnumPattern <&> uncurry PatternEnum)
    <|> M.try pExhaustivePattern
    <|> pAstNode
      ( -- literals must be parsed last because we're looking to match destructing patterns before literals
        -- @@Improvement: It's probably better to split brackketed literals into their own parser, since we want to treat pLiteral as a LeafNode/atom
        M.try pLiteral <&> PatternLiteral
      )

-- | `PatternStruct` parser. Struct patterns are defined as the name of the struct
-- | `AccessName` and then a list of `DestructuringEntry` enclosed in braces and
-- | separated by the ';' operator.
pStructPattern :: Parser (AstNode AccessName, [AstNode DestructuringEntry])
pStructPattern = do
  name <- pAccessName

  accessedMembers <- pBraces (M.optional . M.try $ pSeparatedByOptEnd (pSpecific L.pOperator L.Semicolon) pDestructuringEntry)

  return (name, fromMaybe [] accessedMembers)

-- | `PatternNamespace` parser. Namespace patterns are defined as a list of `DestructuringEntry`
-- | members enclosed in braces and  separated by the ',' operator.
pNamespacePattern :: Parser [AstNode DestructuringEntry]
pNamespacePattern = do
  entries <- pBraces (M.optional . M.try $ pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pDestructuringEntry)

  return $ fromMaybe [] entries

-- | `DestructuringEntry` parser. `DestructuringEntry` is used to represent patterns
-- | that access members of a compound structure like a namespace or a struct. Such
-- | entries will specify the name of what it is being destructured, and an optional
-- | name separated by the ':' operator, to what name that entry is being re-assigned.
-- | Since re-assignment is optional, if no re-assignment is specified, this parser
-- | will automatically transpile the entry as re-assigning to the same variable name.
-- | For example 'a' will converted into 'a:a', and 'a:b' will remain the same.
pDestructuringEntry :: Parser (AstNode DestructuringEntry)
pDestructuringEntry = pAstNode $ do
  start <- M.getOffset
  name <- pName
  end <- M.getOffset
  f <- M.sourceName <$> M.getSourcePos

  newName <- M.optional . M.try $ do
    _ <- pSpecific L.pOperator L.Colon
    pSinglePattern

  case newName of
    Just p -> return $ DestructuringEntry name p
    Nothing -> return $ DestructuringEntry name (astNodeWithSrc start end f (PatternBinding name))

-- | `PatternEnum` parser. Enum patterns are in the form of function calls
-- | or identfier names. An Enum pattern can reference some member of an Enum
-- | like "Some", or as a function call that binds the variants of the enum to
-- | some identifier like "Some(a)". However, this parser does not handle Enum
-- | patterns without any variant parsing because it creates an ambiguity with
-- | `PatternBinding` and `PatternStruct`. So Enums with no variant destructuring
-- | will be treated as a `PatternBinding` and then it will be later resolved at
-- | typechecking.
pEnumPattern :: Parser (AstNode AccessName, [AstNode Pattern])
pEnumPattern = do
  name <- pAccessName
  variants <-
    between
      (pSpecific L.pBracket L.LeftParenthesis)
      (pSpecific L.pBracket L.RightParenthesis)
      (pSeparatedByNoEnd (pSpecific L.pOperator L.Comma) pPattern)

  return (name, variants)

-- | Parse a pattern surrounded by parentheses
pParenPattern :: Parser (AstNode Pattern)
pParenPattern = M.between (pSpecific L.pBracket L.LeftParenthesis) (pSpecific L.pBracket L.RightParenthesis) pPattern

-- | Parse a pattern that binds to a variable
--
-- This parser also parses PatternIgnore if the given name is an underscore.
pBindingPattern :: Parser (AstNode Pattern)
pBindingPattern = do
  n <- pName
  f <- M.sourceName <$> M.getSourcePos
  return $
    astNodeWithSrc (offsetBegin n) (offsetEnd n) f $ case body n of
      Name "_" -> PatternIgnore
      _ -> PatternBinding n
