module StringExpr.Eval
  ( eval
  , evalPos
  , matchesPrefix
  , matchesSuffix
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import StringExpr.AST

eval :: Text -> Expr -> Either String Text
eval t = \case
  Input -> pure t
  SubStr ex start end
    -> substr <$> evalPos t start <*> evalPos t end <*> eval t ex
  _ -> Left "not implemented"

evalPos :: Text -> Pos -> Either String Int
evalPos t = \case
  CPos p
    | 0 <= p && p < len        -> pure p
    | negate len <= p && p < 0 -> pure $ len + p
    | otherwise                -> Left "CPos is out of range"
  Pos rxa rxb c ->
    let matches =
          [ i
          | i <- [0..len]
          , let (a, b) = T.splitAt i t
          , rxa `matchesSuffix` a
          , rxb `matchesPrefix` b
          ]
    in case length matches of
         ml | 0 <= c && c < ml        -> pure $ matches !! c
            | negate ml <= c && c < 0 -> pure $ matches !! (ml + c)
            | otherwise               -> Left "not enough matches"
  where
    len = T.length t


matchesChar :: Token -> Char -> Bool
matchesChar = \case
  SomeOf cls -> isCls cls
  SomeNotOf cls -> not . isCls cls
  _ -> const False


matchesPrefix :: RegExp -> Text -> Bool
matchesPrefix [] _ = True
matchesPrefix (r : rx) txt
  -- as == "" if `r` is a special token like StartTok or EndTok
  = let (as, bs) = T.span (matchesChar r) txt
    in not (T.null as) && matchesPrefix rx bs

matchesSuffix :: RegExp -> Text -> Bool
matchesSuffix rx = matchesPrefix rx . T.reverse

substr :: Int -> Int -> Text -> Text
substr start end = T.take (end - start + 1) . T.drop start
