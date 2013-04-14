module Less.Parser (lessParser, parseLess, unitParser, colourParser) where
import Less.Types
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char (isSpace, ord)
import Data.Maybe (fromMaybe, maybe)
import Data.Bits ((.|.), shiftL)
import Control.Monad ((>=>), liftM)

parseLess :: String -> Processed [Statement]
parseLess = rethrowParseError . parse lessParser "less"

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
colon = T.colon lessLexer
comma = T.comma lessLexer
braces = between (inWhiteSpace $ char '{') (inWhiteSpace $ char '}')
parensOuterSpace = between (inWhiteSpace $ char '(') (inWhiteSpace $ char ')')
parensInnerSpace = between (char '(' >> whiteSpace) (whiteSpace >> char ')')
semiSep = flip sepBy (inWhiteSpace $ char ';')

commaSep = flip sepBy1 $ T.comma lessLexer


---------------------------------------
-- all of the actual parsers go here --
---------------------------------------

lessParser = do
    statements <- many1
                  (try mixinParser
                  <|> fmap ImportS (importParser)
                  <|> fmap VariableS (variableParser)
                  <|> fmap IncludeS (try includeParser))
    eof
    return statements

statementParser :: Parser [Statement]
statementParser = many
                  (try mixinParser
                  <|> fmap ImportS (importParser)
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
            (s, r, i, p, m, v) <- fmap filterStatements (braces statementParser)
            return $ Scope
                { selector = sel
                , rules = r
                , includes = i
                , imports = p
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
                      <|> (try (string ":not") >> parensOuterSpace simpleSelParser >>= return . NotSelector)
                      <|> (char ':' >> sel >>= return . PseudoClassSelector)
                      <|> (char '*' >> return UniversalSelector)
                      <|> (sel >>= return . TypeSelector)
                      <|> (char '&' >> return ParentRef)
    sel = simpleSelectorName

simpleSelectorName = many1 (alphaNum <|> oneOf "-_")

paramParser = parensOuterSpace $ semiSep singleParam
    where
        singleParam = do
            id <- varIdentifier
            whiteSpace
            deflt <- optionMaybe (char ':' >> whiteSpace >> mulExpressionParser)
            return $ case deflt of
                Nothing -> Param id
                Just val -> DefaultParam id val

--guardParser = parensOuterSpace $ boolExpressionParser

includeParser = do
    char '.'
    name <- simpleSelectorName
    params <- fmap (fromMaybe []) $ optionMaybe $ parensOuterSpace $ commaSep $ mulExpressionParser
    statementEnd
    return $ Include name params

importParser = do
    try $ string "@import"
    whiteSpace1
    quot <- oneOf "\"'"
    path <- manyTill anyChar $ char quot
    statementEnd
    return path

ruleParser = do
    prop <- identifier
    colon
    val <- mulExpressionParser
    statementEnd
    return $ Rule prop val

identifier = do
    prefix <- optionMaybe $ char '-'
    start <- lower <|> char '_'
    rest <- many (lower <|> digit <|> oneOf "-_")
    let s = start : rest
    return $ maybe s (:s) prefix

varIdentifier = char '@' >> identifier

variableParser = do
    id <- varIdentifier
    colon
    val <- mulExpressionParser
    statementEnd
    return $ Variable id val

statementEnd = whiteSpace >> ((lookAhead (char '}') >> return ()) <|> (char ';' >> whiteSpace))

-- ALL PLACEHOLDERS HERE

--boolExpressionParser = unexpected "unimplemented"

--expression parsers

mulExpressionParser = outerExpressionParser `sepBy1` whiteSpace1

outerExpressionParser = (parensInnerSpace arithmeticExpressionParser) <|> valueParser
innerExpressionParser = (try arithmeticExpressionParser) <|> valueParser

valueParser = fmap (Identifier 1) varIdentifier
              <|> numberParser
              <|> try appParser
              <|> (fmap Literal quotedString)
              <|> (fmap Literal $ manyTill anyChar $ lookAhead (whiteSpace1 <|> (oneOf ";}()" >> return ())))

numberParser = colourParser <|> unitNumberParser

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
        | otherwise = undefined
        where
        d = ord digit
        a = ord 'a'
        f = ord 'f'
        au = ord 'A'
        fu = ord 'F'
        zero = ord '0'
        nine = ord '9'

unitNumberParser = do
    sign <- liftM (fromMaybe '+') $ optionMaybe $ oneOf "+-"
    let sign' = if sign == '+' then 1 else -1

    pre <- many1 digit
    let pre' = fromIntegral $ foldl (\a d -> 10 * a + d) 0 $ map decVal pre

    post <- liftM (fromMaybe []) $ optionMaybe $ (char '.' >> many1 digit)
    let post' = foldr (\d a -> (a + d) / 10) 0 $ map (fromIntegral . decVal) post

    unit <- unitParser
    return $ Number unit $ (*) sign' $ pre' + post'
    where
    decVal = flip (-) (ord '0') . ord

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
    args <- parensInnerSpace $ (innerExpressionParser `sepBy` comma)
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
