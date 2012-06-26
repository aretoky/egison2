module Language.Egison.Parser where
import Language.Egison.Types
import Control.Monad.Error
import qualified Data.Char as Char
import Data.Complex
import Data.Array
import Numeric
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Language
import Text.Parsec.Prim (ParsecT)
import qualified Text.Parsec.Token as P

egisonDef :: LanguageDef ()
egisonDef 
  = emptyDef    
  { P.commentStart   = "#|"
  , P.commentEnd     = "|#"
  , P.commentLine    = ";"
  , P.nestedComments = True
  , P.identStart     = letter <|> symbol
  , P.identLetter    = letter <|> digit <|> symbol <|> symbol2
  , P.reservedNames  = []
  , P.caseSensitive  = True
  } 

--lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser egisonDef

--dot :: ParsecT String () Identity String
dot = P.dot lexer

--parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = P.parens lexer

brackets = P.brackets lexer

braces = P.braces lexer

angles = P.angles lexer

--identifier :: ParsecT String () Identity String
identifier = P.identifier lexer

-- TODO: typedef. starting point was: whiteSpace :: CharParser ()
--whiteSpace :: ParsecT String () Identity ()
whiteSpace = P.whiteSpace lexer

--lexeme :: ParsecT String () Identity a -> ParsecT String () Identity a
lexeme = P.lexeme lexer

symbol :: Parser Char
--symbol = oneOf "!$%&|*+-/:<=>?@^_~."
symbol = oneOf "%&|*+-/:="

symbol2 :: Parser Char
--symbol = oneOf "!$%&|*+-/:<=>?@^_~."
symbol2 = oneOf "!?"

parseBool :: Parser EgisonExpr
parseBool = do _ <- string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> BoolExpr True
                          'f' -> BoolExpr False
                          _ -> BoolExpr False

parseChar :: Parser EgisonExpr
parseChar = do c <- parseChar2
               return $ CharExpr c

parseChar2 :: Parser Char
parseChar2 = do
  _ <- try (string "#\\")
  c <- anyChar
  r <- many (letter)
  let pchr = c : r
  return $ case pchr of
    "space" -> ' '
    "newline" -> '\n'
    _ -> c

parseOctalNumber :: Parser EgisonExpr
parseOctalNumber = do
  _ <- try (string "#o")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01234567")
  case (length sign) of
     0 -> return $ NumberExpr $ fst $ Numeric.readOct num !! 0
     1 -> return $ NumberExpr $ fromInteger $ (*) (-1) $ fst $ Numeric.readOct num !! 0
     _ -> pzero

parseBinaryNumber :: Parser EgisonExpr
parseBinaryNumber = do
  _ <- try (string "#b")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01")
  case (length sign) of
     0 -> return $ NumberExpr $ fst $ Numeric.readInt 2 (`elem` "01") Char.digitToInt num !! 0
     1 -> return $ NumberExpr $ fromInteger $ (*) (-1) $ fst $ Numeric.readInt 2 (`elem` "01") Char.digitToInt num !! 0
     _ -> pzero

parseHexNumber :: Parser EgisonExpr
parseHexNumber = do
  _ <- try (string "#x")
  sign <- many (oneOf "-")
  num <- many1 (digit <|> oneOf "abcdefABCDEF")
  case (length sign) of
     0 -> return $ NumberExpr $ fst $ Numeric.readHex num !! 0
     1 -> return $ NumberExpr $ fromInteger $ (*) (-1) $ fst $ Numeric.readHex num !! 0
     _ -> pzero

-- |Parser for Integer, base 10
parseDecimalNumber :: Parser EgisonExpr
parseDecimalNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 (digit)
  if (length sign) > 1
     then pzero
     else return $ (NumberExpr . read) $ sign ++ num

-- |Parser for a base 10 Integer that will also
--  check to see if the number is followed by
--  an exponent (scientific notation). If so,
--  the integer is converted to a float of the
--  given magnitude.
parseDecimalNumberMaybeExponent :: Parser EgisonExpr
parseDecimalNumberMaybeExponent = do
  num <- parseDecimalNumber
  result <- parseNumberExponent num
  return result

-- |Parse an integer in any base
parseNumber :: Parser EgisonExpr
parseNumber = parseDecimalNumberMaybeExponent <|>
              parseHexNumber <|>
              parseBinaryNumber <|>
              parseOctalNumber <?>
              "Unable to parse number"

parseNumber2 :: Parser Integer
parseNumber2 = do numExpr <- parseNumber
                  case numExpr of
                    NumberExpr n -> return n
              
-- |Parse a floating point number
parseRealNumber :: Parser EgisonExpr
parseRealNumber = do
  sign <- many (oneOf "-+")
  num <- many1 (digit)
  _ <- char '.'
  frac <- many1 (digit)
  let dec = num ++ "." ++ frac
  f <- case (length sign) of
     0 -> return $ FloatExpr $ fst $ Numeric.readFloat dec !! 0
          -- Bit of a hack, but need to support the + sign as well as the minus.
     1 -> if sign == "-" 
             then return $ FloatExpr $ (*) (-1.0) $ fst $ Numeric.readFloat dec !! 0
             else return $ FloatExpr $ fst $ Numeric.readFloat dec !! 0
     _ -> pzero
  result <- parseNumberExponent f
  return result

parseRealNumber2 :: Parser Double
parseRealNumber2 = do floatExpr <- parseRealNumber
                      case floatExpr of
                        FloatExpr d -> return d
  
-- | Parse the exponent section of a floating point number
--   in scientific notation. Eg "e10" from "1.0e10"
parseNumberExponent :: EgisonExpr -> Parser EgisonExpr
parseNumberExponent n = do 
  exp <- many $ oneOf "Ee"
  case (length exp) of
    0 -> return n
    1 -> do
      num <- try (parseDecimalNumber)
      case num of
        NumberExpr exp -> buildResult n exp
        _ -> pzero
    _ -> pzero
 where 
  buildResult (NumberExpr num) exp = return $ FloatExpr $ (fromIntegral num) * (10 ** (fromIntegral exp))
  buildResult (FloatExpr num) exp = return $ FloatExpr $ num * (10 ** (fromIntegral exp))
  buildResult num _ = pzero

parseEscapedChar :: GenParser Char st Char
parseEscapedChar = do
  _ <- char '\\'
  c <- anyChar
  return $ case c of
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    _ -> c

parseString2 :: Parser String
parseString2 = do
  _ <- char '"'
  x <- many (parseEscapedChar <|> noneOf ("\""))
  _ <- char '"'
  return $ x
    
parseString :: Parser EgisonExpr
parseString = do
  x <- parseString2
  return $ StringExpr x

parseIndexNums :: Parser [EgisonExpr]
parseIndexNums = do try (do char '_'
                            n <- parseExpr
                            ns <- parseIndexNums
                            return (n:ns))
             <|> do return []

parseInnerExp :: Parser InnerExpr
parseInnerExp =  do v <- lexeme parseExpr
                    return $ ElementExpr v
             <|> do char '@'
                    v <- lexeme parseExpr
                    return $ SubCollectionExpr v

parsePatVar2 :: Parser VarExpr
parsePatVar2 = do char '$'
                  name <- identifier
                  nums <- parseIndexNums
                  return (name, nums)

parsePatVar :: Parser EgisonExpr
parsePatVar = do (name, nums) <- parsePatVar2
                 return $ VarExpr name nums
                    
parseSymbol2 :: Parser VarExpr
parseSymbol2 = do char '#'
                  name <- identifier
                  nums <- lexeme parseIndexNums
                  return (name, nums)

parseSymbol :: Parser EgisonExpr
parseSymbol = do (name, nums) <- parseSymbol2
                 return $ SymbolExpr name nums
               
parseArgs :: Parser ArgsExpr
parseArgs = do
      try (do (name, _) <- lexeme parsePatVar2
              return $ AVar name)
  <|> try (lexeme (brackets (do args <- sepEndBy parseArgs whiteSpace
                                return $ ATuple args)))
                    
parseBindings :: Parser Bindings
parseBindings = do
  braces (do sepEndBy (brackets (do args <- lexeme parseArgs
                                    expr <- lexeme parseExpr
                                    return (args, expr)))
                      whiteSpace)

parseRecursiveBindings :: Parser RecursiveBindings
parseRecursiveBindings = do
  braces (do sepEndBy (brackets (do char '$'
                                    name <- lexeme identifier
                                    expr <- lexeme parseExpr
                                    return (name, expr)))
                      whiteSpace)

parseVar :: Parser EgisonExpr
parseVar = do name <- identifier
              nums <- lexeme parseIndexNums
              return $ VarExpr name nums

parseWildCard :: Parser EgisonExpr
parseWildCard = do string "_"
                   return WildCardExpr

parseCutPat :: Parser EgisonExpr
parseCutPat = do string "!"
                 expr <- parseExpr
                 return $ CutPatExpr expr

parseInnerExpr :: Parser InnerExpr
parseInnerExpr = do expr <- parseExpr
                    return $ ElementExpr expr
             <|> do char '@'
                    expr <- parseExpr
                    return $ SubCollectionExpr expr

parsePattern :: Parser EgisonExpr
parsePattern =
      parseWildCard
  <|> parsePatVar
  <|> parseCutPat


parseDestructInfoExpr :: Parser DestructInfoExpr
parseDestructInfoExpr = braces (sepEndBy parseDestructClause whiteSpace)

parseDestructClause :: Parser (String, EgisonExpr, [(PrimitivePattern, EgisonExpr)])
parseDestructClause = brackets (do patCons <- lexeme identifier
                                   typExpr <- lexeme parseExpr
                                   dc2s <- lexeme (braces (sepEndBy parseDestructClause2 whiteSpace))
                                   return (patCons, typExpr, dc2s))

parseDestructClause2 :: Parser (PrimitivePattern, EgisonExpr)
parseDestructClause2 = brackets (do datPat <- lexeme parsePrimitivePattern
                                    expr <- lexeme parseExpr
                                    return (datPat, expr))

parsePrimitivePattern :: Parser PrimitivePattern
parsePrimitivePattern =
      do char '_'
         return PWildCard
  <|> do c <- try parseChar2
         return (PPatChar c)
  <|> do d <- try parseRealNumber2
         return (PPatDouble d)
  <|> do n <- try parseNumber2
         return (PPatInteger n)
  <|> do char '$'
         name <- identifier
         return (PPatVar name)
  <|> angles (do c <- lexeme identifier
                 ps <- sepEndBy parsePrimitivePattern whiteSpace
                 return (PInducivePat c ps))
  <|> try (do string "{}"
              return PEmptyPat)
  <|> try (do lexeme $ char '{'
              a <- lexeme parsePrimitivePattern
              char '.'
              b <- lexeme parsePrimitivePattern
              char '}'
              return (PConsPat a b))
  <|> try (do lexeme $ char '{'
              char '.'
              a <- lexeme parsePrimitivePattern
              b <- lexeme parsePrimitivePattern
              char '}'
              return (PSnocPat a b))

parseMatchClause :: Parser MatchClause
parseMatchClause = brackets (do pat <- lexeme parseExpr
                                body <- lexeme parseExpr
                                return (pat, body))
              
-- |Parse an expression
parseExpr :: Parser EgisonExpr
parseExpr =
      try (lexeme parseRealNumber)
  <|> try (lexeme parseNumber)
  <|> lexeme parseChar
  <|> lexeme parseString
  <|> try (lexeme parseBool)
  <|> try (lexeme parseSymbol)
  <|> try (lexeme parsePattern)
--  <|> lexeme parsePatVarOmitExpr
  <|> lexeme parseVar
  <|> angles (do cons <- lexeme identifier
                 argExprs <- sepEndBy parseExpr whiteSpace
                 return $ InductiveDataExpr cons argExprs)
  <|> braces (do innerExprs <- sepEndBy parseInnerExpr whiteSpace
                 return $ CollectionExpr innerExprs)
  <|> brackets (do innerExprs <- sepEndBy parseInnerExpr whiteSpace
                   return $ TupleExpr innerExprs)
  <|> parens (do try (lexeme $ string "lambda")
                 args <- lexeme parseArgs
                 body <- lexeme parseExpr
                 return $ FuncExpr args body
          <|> do try (lexeme $ string "letrec")
                 bindings <- lexeme parseRecursiveBindings
                 body <- lexeme parseExpr
                 return (LetRecExpr bindings body)
          <|> do try (lexeme $ string "let")
                 bindings <- lexeme parseBindings
                 body <- lexeme parseExpr
                 return (LetExpr bindings body)
          <|> do try (lexeme $ string "type-ref")
                 typExpr <- lexeme parseExpr
                 name <- lexeme identifier
                 return (TypeRefExpr typExpr name)
          <|> do try (lexeme $ string "type")
                 bindings <- lexeme parseRecursiveBindings
                 return (TypeExpr bindings)
          <|> do try (lexeme $ string "destructor")
                 deconsInfo <- lexeme parseDestructInfoExpr
                 return (DestructorExpr deconsInfo)
          <|> do try (lexeme $ string "match-all")
                 tgtExpr <- lexeme parseExpr
                 typExpr <- lexeme parseExpr
                 mc <- lexeme parseMatchClause
                 return (MatchAllExpr tgtExpr typExpr mc)
          <|> do try (lexeme $ string "match")
                 tgtExpr <- lexeme parseExpr
                 typExpr <- lexeme parseExpr
                 mcs <- braces (sepEndBy parseMatchClause whiteSpace)
                 return (MatchExpr tgtExpr typExpr mcs)
          <|> do opExpr <- lexeme parseExpr
                 argExprs <- sepEndBy parseExpr whiteSpace
                 return $ ApplyExpr opExpr (TupleExpr (map ElementExpr argExprs)))
  <?> "Expression"

parseTopExpr :: Parser TopExpr
parseTopExpr =
     do whiteSpace
        parens (do try $ lexeme $ string "define"
                   char '$'
                   name <- lexeme identifier
                   expr <- lexeme parseExpr
                   return (Define name expr)
            <|> do try $ lexeme $ string "test"
                   expr <- lexeme parseExpr
                   return (Test expr)
            <|> do try $ lexeme $ string "execute"
                   args <- sepEndBy parseString2 whiteSpace
                   return (Execute args)
            <|> do try $ lexeme $ string "load-file"
                   filename <- lexeme parseString2
                   return (LoadFile filename)
            <|> do try $ lexeme $ string "load"
                   filename <- lexeme parseString2
                   return (Load filename)
                ) <?> "top expression"

mainParser :: Parser TopExpr
mainParser = do
    x <- parseTopExpr
-- FUTURE? (seemed to break test cases, but is supposed to be best practice?)    eof
    return x

-- |Use a parser to parse the given text, throwing an error
--  if there is a problem parsing the text.
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "egison" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- |Parse an expression from a string of text
readExpr :: String -> ThrowsError TopExpr
readExpr = readOrThrow mainParser

-- |Parse many expressions from a string of text
readExprList :: String -> ThrowsError [TopExpr]
readExprList = readOrThrow (endBy mainParser whiteSpace)

