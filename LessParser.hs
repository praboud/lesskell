module LessParser (lessParser) where
import LessTypes
import Text.ParserCombinators.Parsec hiding (whitespace)
import qualified Text.ParserCombinators.Parsec.Token as T

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

whiteSpace = T.whiteSpace lessLexer
inWhiteSpace = between whiteSpace whiteSpace
identifier = T.identifier lessLexer
colon = T.colon lessLexer
braces = between (inWhiteSpace $ char '{') (inWhiteSpace $ char '}')
semiSep = flip sepEndBy1 $ T.semi lessLexer


---------------------------------------
-- all of the actual parsers go here --
---------------------------------------

lessParser = statementParser

statementParser :: Parser [Statement]
statementParser = semiSep $ try mixinParser <|> fmap VariableS variableParser <|> fmap RuleS ruleParser

mixinParser = do
    sel <- selParser
    --args <- optionMaybe paramParser
    let args = Nothing
    case args of
        Nothing -> bodyParser sel >>= return . ScopeS 
        Just a -> do
            whiteSpace
            guards <- guardParser
            body <- bodyParser sel
            return $ MixinS $ Mixin
                { args = a
                , body = body
                , guards = guards
                }
    where
        bodyParser sel = do
            (s, r, m, v) <- fmap filterStatements $ braces statementParser 
            return $ Scope
                { selector = sel
                , rules = r
                , subscopes = s
                , mixins = m
                , variables = v
                }

selParser = many $ alphaNum <|> oneOf ".#:&*-_ "
--selParser = fmap concat $ many (fsing alphaNum <|> fsing (oneOf ".#:&*") <|> char )

betweenParens = between (char '(' >> whiteSpace) (whiteSpace >> char ')')

paramParser = betweenParens $ many singleParam
    where
        singleParam = do
            id <- identifier
            whiteSpace
            deflt <- optionMaybe (char ':' >> whiteSpace >> expressionParser)
            char ';'
            return $ Param id deflt

guardParser = betweenParens $ boolExpressionParser

-- ALL PLACEHOLDERS HERE

boolExpressionParser = unexpected "unimplemented"
expressionParser = unexpected "unimplemented"

ruleParser = do
    prop <- many $ lower <|> char '-'
    colon
    val <- manyTill anyChar $ lookAhead $ oneOf ";}"
    return $ Rule prop val

variableParser = do
    id <- identifier
    colon
    val <- manyTill anyChar $ lookAhead $ oneOf ";}"
    return $ Variable id val
