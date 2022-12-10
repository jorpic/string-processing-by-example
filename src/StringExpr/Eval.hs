module StringExpr.Eval
  ( EvalContext(..)
  , EvalErr
  , runEval
  , evalAtomic
  , evalPos
  , matchesPrefix
  , matchesSuffix
  ) where

import Prelude hiding ((!!))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity
import Data.List.Safe ((!!))
import Data.Text (Text)
import Data.Text qualified as T
import StringExpr.AST


data EvalContext = EvalContext
  { inputs :: [Text]
  }

type EvalErr = String
type EvalT m a = ReaderT EvalContext (ExceptT EvalErr m) a

runEval :: EvalT Identity a -> EvalContext -> Either EvalErr a
runEval f = runIdentity . runExceptT . runReaderT f

evalAtomic
  :: (MonadReader EvalContext m, MonadError EvalErr m)
  => AtomicExpr -> m Text
evalAtomic = \case
  SubStr (Input i) start end -> do
    txt <- asks inputs >>= liftError show . (!! i)
    substr txt
      <$> evalPos txt start
      <*> evalPos txt end

evalPos :: MonadError EvalErr m => Text -> Pos -> m Int
evalPos t = \case
  CPos p
    | 0 <= p && p < len        -> pure p
    | negate len <= p && p < 0 -> pure $ len + p
    | otherwise                -> throwError "CPos is out of range"
  Pos rxa rxb c ->
    let matches =
          [ i
          | i <- [0..len]
          , let (a, b) = T.splitAt i t
          , rxa `matchesSuffix` a
          , rxb `matchesPrefix` b
          ]
        ix = if c >= 0 then c else c + length matches
    in liftError (const "not enough matches")
      $ matches !! ix
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

liftError :: MonadError e' m => (e -> e') -> Either e x -> m x
liftError f = liftEither .  either (Left . f) pure
