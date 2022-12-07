module StringExpr.Eval
  ( eval
  , evalPos
  , matchesPrefix
  , matchesSuffix
  ) where

import Prelude hiding ((!!))
import Data.List.Safe ((!!))
import Data.Text (Text)
import Data.Text qualified as T
import StringExpr.AST

eval :: [Text] -> AtomicExpr -> Either String Text
eval inputs = \case
  SubStr i start end -> do
    txt <- mapErr show (inputs !! i)
    substr txt
      <$> evalPos txt start
      <*> evalPos txt end

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
    in mapErr (const "not enough matches")
      $ matches !! (if c >= 0 then c else length matches + c)
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
matchesSuffix rx = matchesPrefix (reverse rx) . T.reverse


substr :: Text -> Int -> Int -> Text
substr txt start end = T.take (end - start + 1) $ T.drop start txt

mapErr :: (e -> e') -> Either e x -> Either e' x
mapErr f = either (Left . f) pure

