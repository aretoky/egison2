module Language.Egison.Parser where
import Language.Egison.Types
import Control.Monad.Error
import qualified Data.Char as Char
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Language
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
symbol = oneOf "&*+-/:="

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

parseBool2 :: Parser Bool
parseBool2 = do
  boolExpr <- parseBool
  case boolExpr of
    BoolExpr True -> return True
    BoolExpr False -> return False
                          
parseChar :: Parser EgisonExpr
parseChar = do
  _ <- char '\''
  x <- parseEscapedChar <|> noneOf ("'")
  _ <- char '\''
  return $ CharExpr x
    
parseChar2 :: Parser Char
parseChar2 = do chrExpr <- parseChar
                case chrExpr of
                  CharExpr chr -> return chr

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

parseDecimalNumber :: Parser EgisonExpr
parseDecimalNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 (digit)
  if (length sign) > 1
     then pzero
     else return $ (NumberExpr . read) $ sign ++ num

parseDecimalNumberMaybeExponent :: Parser EgisonExpr
parseDecimalNumberMaybeExponent = do
  num <- parseDecimalNumber
  result <- parseNumberExponent num
  return result

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
                 return $ PatVarExpr name nums
                    
parseMacroVarExpr :: Parser EgisonExpr
parseMacroVarExpr = do
  char '%'
  name <- identifier
  nums <- lexeme parseIndexNums
  return $ MacroVarExpr name nums

parsePatVarOmitExpr :: Parser EgisonExpr
parsePatVarOmitExpr = do
  string "$`"
  expr <- lexeme parseExpr
  return $ PatVarOmitExpr expr

parseVarOmitExpr :: Parser EgisonExpr
parseVarOmitExpr = do
  char '`'
  expr <- lexeme parseExpr
  return $ VarOmitExpr expr

parseArgs :: Parser ArgsExpr
parseArgs = do
      try (do (name, _) <- lexeme parsePatVar2
              return $ AVar name)
  <|> try (lexeme (brackets (do args <- sepEndBy parseArgs whiteSpace
                                return $ ATuple args)))
                    
parseBindings :: Parser Bindings
parseBindings = do
  braces (do sepEndBy (brackets (do args <- lexeme parseExpr
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
parseWildCard = do char '_'
                   return WildCardExpr

parseCutPat :: Parser EgisonExpr
parseCutPat = do char '!'
                 expr <- parseExpr
                 return $ CutPatExpr expr

parseNotPat :: Parser EgisonExpr
parseNotPat = do char '^'
                 expr <- parseExpr
                 return $ NotPatExpr expr

parseValuePat :: Parser EgisonExpr
parseValuePat = do char ','
                   expr <- parseExpr
                   return $ ValuePatExpr expr

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
  <|> parseNotPat
  <|> parseValuePat
  <|> parens (do try (char '?' >> many1 space)
                 predExpr <- lexeme parseExpr
                 argExprs <- sepEndBy parseExpr whiteSpace
                 return (PredPatExpr predExpr argExprs)
          <|> do try (char '|' >> many1 space)
                 pats <- sepEndBy parseExpr whiteSpace
                 return (OrPatExpr pats)
          <|> do try (char '&' >> many1 space)
                 pats <- sepEndBy parseExpr whiteSpace
                 return (AndPatExpr pats))


parseDestructInfoExpr :: Parser DestructInfoExpr
parseDestructInfoExpr = braces (sepEndBy parseDestructClause whiteSpace)

parseDestructClause :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitivePattern, EgisonExpr)])
parseDestructClause = brackets (do pppat <- lexeme parsePrimitivePatPattern
                                   typExpr <- lexeme parseExpr
                                   dc2s <- lexeme (braces (sepEndBy parseDestructClause2 whiteSpace))
                                   return (pppat, typExpr, dc2s))

parseDestructClause2 :: Parser (PrimitivePattern, EgisonExpr)
parseDestructClause2 = brackets (do datPat <- lexeme parsePrimitivePattern
                                    expr <- lexeme parseExpr
                                    return (datPat, expr))

parsePrimitivePatPattern :: Parser PrimitivePatPattern
parsePrimitivePatPattern =
      do char '_'
         return PPWildCard
  <|> do char ','
         char '$'
         name <- lexeme identifier
         return (PPValuePat name)
  <|> angles (do c <- lexeme identifier
                 ps <- sepEndBy parsePrimitivePatPattern whiteSpace
                 return (PPInductivePat c ps))
                                    
parsePrimitivePattern :: Parser PrimitivePattern
parsePrimitivePattern =
      do char '_'
         return PWildCard
  <|> do char '$'
         name <- identifier
         return (PPatVar name)
  <|> angles (do c <- lexeme identifier
                 ps <- sepEndBy parsePrimitivePattern whiteSpace
                 return (PInductivePat c ps))
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
  <|> do b <- try parseBool2
         return (PPatBool b)
  <|> do c <- try parseChar2
         return (PPatChar c)
  <|> do d <- try parseRealNumber2
         return (PPatFloat d)
  <|> do n <- try parseNumber2
         return (PPatNumber n)

parseMatchClause :: Parser MatchClause
parseMatchClause = brackets (do pat <- lexeme parseExpr
                                body <- lexeme parseExpr
                                return (pat, body))

parseArrayElementExpr :: Parser ArrayElementExpr
parseArrayElementExpr =
      do try (lexeme (string "[~"))
         exprs <- sepEndBy parseArrayElementExpr whiteSpace
         (lexeme (string "~]"))
         return (AInnerArrayExpr exprs)
  <|> do expr <- parseExpr
         return (AElementExpr expr)

-- |Parse an expression
parseExpr :: Parser EgisonExpr
parseExpr =
      try (lexeme parseRealNumber)
  <|> try (lexeme parseNumber)
  <|> lexeme parseChar
  <|> lexeme parseString
  <|> try (lexeme parseBool)
  <|> try (lexeme parsePattern)
  <|> lexeme parseMacroVarExpr
  <|> lexeme parsePatVarOmitExpr
  <|> lexeme parseVarOmitExpr
  <|> try (lexeme (do string "undefined"
                      return UndefinedExpr))
  <|> try (lexeme (do string "Something"
                      return SomethingExpr))
  <|> lexeme parseVar
  <|> do try (lexeme (string "[|"))
         exprs <- sepEndBy parseArrayElementExpr whiteSpace
         (lexeme (string "|]"))
         return (ArrayExpr exprs)
  <|> angles (do cons <- lexeme identifier
                 argExprs <- sepEndBy parseExpr whiteSpace
                 return $ InductiveDataExpr cons argExprs)
  <|> braces (do innerExprs <- sepEndBy parseInnerExpr whiteSpace
                 return $ CollectionExpr innerExprs)
  <|> brackets (do exprs <- sepEndBy parseExpr whiteSpace
                   return $ TupleExpr exprs)
  <|> parens (do try (string "lambda" >> many1 space)
                 args <- lexeme parseArgs
                 body <- lexeme parseExpr
                 return (FuncExpr args body)
          <|> do try (string "macro" >> many1 space)
                 args <- lexeme (brackets (sepEndBy (do (name, _) <- parsePatVar2
                                                        return name)
                                                    whiteSpace))
                 body <- lexeme parseExpr
                 return (MacroExpr args body)
          <|> do try (string "if" >> many1 space)
                 condExpr <- lexeme parseExpr
                 expr1 <- lexeme parseExpr
                 expr2 <- lexeme parseExpr
                 return (IfExpr condExpr expr1 expr2)
          <|> do try (string "letrec" >> many1 space)
                 bindings <- lexeme parseRecursiveBindings
                 body <- lexeme parseExpr
                 return (LetRecExpr bindings body)
          <|> do try (string "let" >> many1 space)
                 bindings <- lexeme parseBindings
                 body <- lexeme parseExpr
                 return (LetExpr bindings body)
          <|> do try (string "do" >> many1 space)
                 bindings <- lexeme parseBindings
                 body <- lexeme parseExpr
                 return (DoExpr bindings body)
          <|> do try (string "type" >> many1 space)
                 deconsInfo <- lexeme parseDestructInfoExpr
                 return (TypeExpr deconsInfo)
          <|> do try (string "match-all" >> many1 space)
                 tgtExpr <- lexeme parseExpr
                 typExpr <- lexeme parseExpr
                 mc <- lexeme parseMatchClause
                 return (MatchAllExpr tgtExpr typExpr mc)
          <|> do try (string "match" >> many1 space)
                 tgtExpr <- lexeme parseExpr
                 typExpr <- lexeme parseExpr
                 mcs <- braces (sepEndBy parseMatchClause whiteSpace)
                 return (MatchExpr tgtExpr typExpr mcs)
          <|> do try (string "loop" >> many1 space)
                 (loopVar, _) <- lexeme parsePatVar2
                 (indexVar, _) <- lexeme parsePatVar2
                 rangeExpr <- lexeme parseExpr
                 loopExpr <- lexeme parseExpr
                 tailExpr <- lexeme parseExpr
                 return (LoopExpr loopVar indexVar rangeExpr loopExpr tailExpr)
          <|> do try (string "generate-array" >> many1 space)
                 fnExpr <- lexeme parseExpr
                 arrExpr <- lexeme parseExpr
                 return (GenerateArrayExpr fnExpr arrExpr)
          <|> do opExpr <- lexeme parseExpr
                 argExprs <- sepEndBy parseExpr whiteSpace
                 return (ApplyExpr opExpr (TupleExpr argExprs)))
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
            <|> do try $ string "load-file" >> many1 space
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

-- |Parse an top expression from a string of text
readTopExpr :: String -> ThrowsError TopExpr
readTopExpr = readOrThrow mainParser

-- |Parse an expression from a string of text
readExpr :: String -> ThrowsError EgisonExpr
readExpr = readOrThrow (whiteSpace >> parseExpr)

-- |Parse many top expressions from a string of text
readTopExprList :: String -> ThrowsError [TopExpr]
readTopExprList = readOrThrow (endBy mainParser whiteSpace)

