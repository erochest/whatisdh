
module Text.Tokenizer
    ( Token
    , tokenize
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import qualified Data.Char as C
import qualified Data.List as L
import           Data.Maybe (maybe)
import           Data.Monoid
import qualified Data.Text as T
import           Import

type Token = T.Text

tokenize :: T.Text -> Either String [(Token, TokenCategory)]
tokenize = parseOnly tokenList

tokenList :: Parser [(Token, TokenCategory)]
tokenList = many token <* skipWhile C.isSpace

token :: Parser (Token, TokenCategory)
token =   skipWhile C.isSpace >>
      (   punctuationToken
      <|> compositeToken
      <|> numberToken
      <|> alphaToken
      <|> symbolToken
      <|> markToken
      <|> unknownChar 
      )

parseToken :: (Char -> Bool) -> TokenCategory -> Parser (Token, TokenCategory)
parseToken predicate category = (flip (,) category . T.toLower) <$> takeWhile1 predicate

whitespaceToken :: Parser (Token, TokenCategory)
whitespaceToken = parseToken C.isSpace WhiteSpaceToken

punctuationToken :: Parser (Token, TokenCategory)
punctuationToken = parseToken C.isPunctuation PunctuationToken

compositeToken :: Parser (Token, TokenCategory)
compositeToken = try $ do
    first <- L.concat <$> cbody
    final <- many1 (alphaToken <|> numberToken)
    return ( T.concat . map fst $ first `mappend` final
           , CompositeToken
           )
    where
        cbody :: Parser [[(Token, TokenCategory)]]
        cbody = many1 pair

        pair :: Parser [(Token, TokenCategory)]
        pair = do
            ws <- many1 (alphaToken <|> numberToken)
            ss <- many1 (punctuationToken <|> symbolToken <|> markToken)
            return $ ws ++ ss

numberToken :: Parser (Token, TokenCategory)
numberToken = parseToken C.isNumber NumberToken

alphaToken :: Parser (Token, TokenCategory)
alphaToken = parseToken C.isAlpha AlphaToken

symbolToken :: Parser (Token, TokenCategory)
symbolToken = parseToken C.isSymbol SymbolToken

markToken :: Parser (Token, TokenCategory)
markToken = parseToken C.isMark MarkToken

unknownChar :: Parser (Token, TokenCategory)
unknownChar = flip (,) UnknownToken <$> AT.take 1

