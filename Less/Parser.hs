module Less.Parser (lessParser, unitParser, colourParser) where
import Less.Types
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char (isSpace, ord)
import Data.Maybe (fromMaybe)
import Data.Bits ((.|.), shiftL)
import Control.Monad ((>=>))

-- some useful helpers --

lessLanguage = T.LanguageDef
    { T.commentStart = "/*"
    , T.commentEnd = "*/"
    , T.commentLine = "//"
    , T.nestedComments = False
    , T.identStart = char '@'
    , T.identLetter = alphaNum <|> oneOf "@-_"
    , T.opStart = T.opLetter lessLanguage
    , T.opLetter = oneOf "<=>+-*/"
    , T.reservedNames = ["import"]
    , T.reservedOpNames =
        [ "+"
        , "-"
        , "*"
        , "/"
        , ">"
        , ">="
        , "<"
        , "<="
        , "=="
        , "and"
        , "or"
        , "not"
        ]
    , T.caseSensitive = True
    }

lessLexer = T.makeTokenParser lessLanguage

comment = ((try (string "//") >> manyTill anyChar newline) <|> (try (string "/*") >> manyTill anyChar (try ( string "*/")))) >> return ()
simpleSpace = skipMany1 (satisfy isSpace)
whiteSpace = skipMany (simpleSpace <|> comment)
whiteSpace1 = ((skipMany1 (satisfy isSpace) >> whiteSpace) <|> (comment >> whiteSpace1))
inWhiteSpace = between whiteSpace whiteSpace
identifier = T.identifier lessLexer
colon = T.colon lessLexer
comma = T.comma lessLexer
braces = between (inWhiteSpace $ char '{') (inWhiteSpace $ char '}')
parens = between (inWhiteSpace $ char '(') (inWhiteSpace $ char ')')
semiSep = flip sepBy1 (inWhiteSpace $ char ';')
float = T.float lessLexer
integer = T.integer lessLexer

commaSep = flip sepBy1 $ T.comma lessLexer


---------------------------------------
-- all of the actual parsers go here --
---------------------------------------

lessParser = do
    statements <- many1
                  (try mixinParser
                  <|> fmap VariableS (variableParser)
                  <|> fmap IncludeS (try includeParser))
    eof
    return statements

statementParser :: Parser [Statement]
statementParser = many
                  (try mixinParser
                  <|> fmap VariableS (variableParser)
                  <|> fmap IncludeS (includeParser)
                  <|> fmap RuleS (ruleParser))

mixinParser = do
    sel <- selParser
    args <- optionMaybe $ try paramParser
    case args of
        Nothing -> bodyParser sel >>= return . ScopeS
        Just a -> do
            whiteSpace
            --guards <- optionMaybe $ try guardParser
            let guards = Nothing
            body <- bodyParser sel
            return $ MixinS $ Mixin
                { args = a
                , body = body
                , guards = guards
                }
    where
        bodyParser sel = do
            (s, r, i, m, v) <- fmap filterStatements (braces statementParser)
            return $ Scope
                { selector = sel
                , rules = r
                , includes = i
                , subscopes = s
                , mixins = m
                , variables = v
                }

selParser :: Parser Selector
selParser = sepBy1 selCombParser comma
    where
    selCombParser = do
        group <- many1 simpleSelParser
        fmap (fromMaybe (Terminus group)) $ optionMaybe $ try $ joinParser group
    joinParser sel = do
        t <-  (try $ inWhiteSpace $ oneOf "+>") <|> (whiteSpace1 >> return ' ')
        comb <- selCombParser
        return $ Combinator t sel comb
    simpleSelParser = (char '#' >> sel >>= return . IdSelector)
                      <|> (char '.' >> sel >>= return . ClassSelector)
                      <|> (char '[' >> manyTill anyChar (char ']') >>= return . AttributeSelector)
                      <|> (try (string ":not") >> parens simpleSelParser >>= return . NotSelector)
                      <|> (char ':' >> sel >>= return . PseudoClassSelector)
                      <|> (char '*' >> return UniversalSelector)
                      <|> (sel >>= return . TypeSelector)
                      <|> (char '&' >> return ParentRef)
    sel = simpleSelectorName

simpleSelectorName = many1 (alphaNum <|> oneOf "-_")

paramParser = parens $ semiSep singleParam
    where
        singleParam = do
            id <- identifier
            whiteSpace
            deflt <- optionMaybe (char ':' >> whiteSpace >> mulExpressionParser)
            return $ case deflt of
                Nothing -> Param id
                Just val -> DefaultParam id val

guardParser = parens $ boolExpressionParser

includeParser = do
    char '.'
    name <- simpleSelectorName
    params <- fmap (fromMaybe []) $ optionMaybe $ parens $ commaSep $ mulExpressionParser
    statementEnd
    return $ Include name params

ruleParser = do
    prop <- many1 $ lower <|> char '-'
    colon
    val <- mulExpressionParser
    statementEnd
    return $ Rule prop val

variableParser = do
    id <- identifier
    colon
    val <- mulExpressionParser
    statementEnd
    return $ Variable id val

statementEnd = whiteSpace >> ((lookAhead (char '}') >> return ()) <|> (char ';' >> whiteSpace))

-- ALL PLACEHOLDERS HERE

boolExpressionParser = unexpected "unimplemented"

--expression parsers

mulExpressionParser = outerExpressionParser `sepBy1` whiteSpace1

outerExpressionParser = (parens arithmeticExpressionParser) <|> valueParser
innerExpressionParser = (try arithmeticExpressionParser) <|> valueParser

valueParser = identifierParser
              <|> numberParser
              <|> try appParser
              <|> (fmap Literal quotedString)
              <|> (fmap Literal $ many1 lower)

numberParser = colourParser <|> unitNumberParser
identifierParser = fmap (Identifier 1) identifier

colourParser = do
    char '#'
    str <- (try (count 6 hexDigit) ) <|> ((count 3 hexDigit) >>= (\s -> return $ interleave s s))
    return $ Color $ (flip shiftL 8) $ foldl (\tot digit -> (shiftL tot 4) .|. (fromIntegral $ hexVal digit)) 0 str
    where
    interleave [] ys = ys
    interleave (x:xs) ys = x : (interleave ys xs)
    hexVal digit
        | zero <= d && d <= nine = d - zero
        | a <= d && d <= f = d - a + 10
        | au <= d && d <= fu = d - au + 10
        where
        d = ord digit
        a = ord 'a'
        f = ord 'f'
        au = ord 'A'
        fu = ord 'F'
        zero = ord '0'
        nine = ord '9'

unitNumberParser = do
    number <- try float <|> (integer >>= return . fromIntegral)
    unit <- unitParser
    return $ Number unit number

unitParser = (choice $ map (\(t, u) -> try (string t) >> return u) units) <?> "unit"
    where
    units =
        [ ("%", Percent)
        , ("em", Em)
        , ("pt", Pt)
        , ("px", Px)
        , ("", NA)
        ]

appParser = do
    name <- many lower
    args <- parens $ (innerExpressionParser `sepBy` comma)
    return $ FuncApp name args

quotedString = do
    quot <- oneOf "\"'"
    str <- manyTill anyChar $ char quot
    return $ quot : str ++ [quot]

-- groups of binary arithmetic operators in order of precendence
-- (least to greatest)
operators =
    [ ['+', '-']
    , ['*', '/']
    ]

arithmeticExpressionParser :: Parser Expression
arithmeticExpressionParser = arith_h operators
    where
    arith_h [] = term
    arith_h (ops:rest) = (arith_h rest) `chainl1` (try $ inWhiteSpace $ choice (map opParser ops))
    term = outerExpressionParser
    opParser = char >=> return . BinOp
