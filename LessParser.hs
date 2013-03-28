module LessParser (lessParser) where
import LessTypes
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

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

comment = ((try (string "//") >> manyTill anyChar newline) <|> (string "/*" >> manyTill anyChar (try ( string "*/")))) >> return ()
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

commaSep = flip sepBy1 $ T.comma lessLexer


---------------------------------------
-- all of the actual parsers go here --
---------------------------------------

lessParser = many1 $ try mixinParser
                  <|> fmap VariableS variableParser
                  <|> fmap IncludeS includeParser

statementParser :: Parser [Statement]
statementParser = many1 $ try mixinParser
                  <|> fmap VariableS variableParser
                  <|> fmap IncludeS includeParser
                  <|> fmap RuleS ruleParser

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
    <?> "Expected rule or mixin"
    where
        bodyParser sel = do
            (s, r, i, m, v) <- fmap filterStatements (braces statementParser <?> "Expected statements")
            return $ Scope
                { selector = sel
                , rules = r
                , includes = i
                , subscopes = s
                , mixins = m
                , variables = v
                }
            <?> "Expected body expression for ruleset/mixin"

selParser :: Parser Selector
selParser = sepBy1 selCombParser comma <?> "Expected selector"
    where
    selCombParser = do
        group <- many1 simpleSelParser
        fmap (fromMaybe (Terminus group)) $ optionMaybe $ try $ joinParser group
        <?> "Expected selector combinator"
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
                      <?> "Expected simple selector"
    sel = simpleSelectorName

simpleSelectorName = many1 (alphaNum <|> oneOf "-_") <?> "Expected selector name"

paramParser = parens $ semiSep singleParam
    where
        singleParam = do
            id <- identifier
            whiteSpace
            deflt <- optionMaybe (char ':' >> whiteSpace >> expressionParser ";)")
            return $ case deflt of
                Nothing -> Param id
                Just val -> DefaultParam id val

guardParser = parens $ boolExpressionParser

includeParser = do
    char '.'
    name <- simpleSelectorName
    params <- fmap (fromMaybe []) $ optionMaybe $ parens $ commaSep $ expressionParser ";)"
    statementEnd
    return $ Include name params
    
quotedString = do
    quot <- oneOf "\"'"
    str <- manyTill anyChar $ char quot
    return $ quot : str ++ [quot]

ruleParser = do
    prop <- many1 $ lower <|> char '-'
    colon
    val <- expressionParser ";}"
    statementEnd
    return $ Rule prop val
    <?> "Expected rule"

variableParser = do
    id <- identifier
    colon
    val <- expressionParser ";}"
    statementEnd
    return $ Variable id val

statementEnd = (lookAhead (char '}') <|> char ';') >> whiteSpace

-- ALL PLACEHOLDERS HERE

boolExpressionParser = unexpected "unimplemented"
expressionParser end = --(parens fullExpressionParser) <|> 
                   (fmap Identifier identifier)
                   <|> (fmap Literal quotedString)
                   <|> (fmap Literal $ manyTill anyChar $ lookAhead $ oneOf end)
