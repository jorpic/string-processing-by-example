module StringExpr.Eval
  ( EvalContext
  , EvalErr
  , runEval
  , evalExpr
  , evalAtomic
  , evalPos
  , matchesPrefix
  , matchesSuffix
  ) where

import Prelude hiding ((!!))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Lazy (MonadWriter, tell, execWriterT)
import Data.Functor.Identity
import Data.List.Safe ((!!))
import Data.Text (Text)
import Data.Text qualified as T
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import StringExpr.AST


data EvalContext = EvalContext
  { inputs :: [Text]
  , loopVars :: IntMap Int
  }

type EvalErr = String
type EvalT m a = ReaderT EvalContext (ExceptT EvalErr m) a
type EvalM m = (MonadReader EvalContext m, MonadError EvalErr m)

runEval :: EvalT Identity a -> [Text] -> Either EvalErr a
runEval f xs
  = runIdentity $ runExceptT $ runReaderT f
  $ EvalContext
    (map (`T.snoc` '\n') xs) -- FIXME: adding '\n' to each string as a marker for EndTok
    IntMap.empty

evalExpr :: EvalM m => Expr -> m Text
evalExpr (Switch _) = throwError "NOT IMPLEMENTED"

evalAtomic :: EvalM m => AtomicExpr -> m Text
evalAtomic = \case
  ConstStr s -> pure s
  SubStr (Input i) start end -> do
    txt <- asks ((!! i) . inputs) >>= liftError show
    substr txt
      <$> evalPos txt start
      <*> evalPos txt end
  Loop (LoopVar v) f -> do
    let setLoopVarTo i c = c {loopVars = IntMap.insert v i $ loopVars c}
    let whileNotErr = (`catchError` (\_ -> pure ()))
    let iter :: (MonadWriter Text m, EvalM m) => Int -> m ()
        iter 42 = pure () -- limit recursion depth
        iter i = local (setLoopVarTo i)
          $ whileNotErr
          $ evalTrace f >>= tell >> iter (i+1)
    -- MonadWriter concatenates results of successful iterations
    execWriterT $ iter 0

evalTrace :: EvalM m => TraceExpr -> m Text
evalTrace (Concat xs) = T.concat <$> mapM evalAtomic xs


evalInt :: EvalM m => IntExpr -> m Int
evalInt = \case
  IntConst i -> pure i
  IntExpr a (LoopVar var) b -> do
    val <- asks (IntMap.lookup var . loopVars)
      >>= maybe (throwError $ "loop var is out of range " ++ show var) pure
    pure $ a * val + b

-- NB. in `Pos _ _ c` the offset `c` is zero-based in this implementation, but
-- in the paper it is one-based.
evalPos :: EvalM m => Text -> Pos -> m Int
evalPos t = \case
  CPos p
    | 0 <= p && p < len        -> pure p
    | negate len <= p && p < 0 -> pure $ len + p
    | otherwise                -> throwError "CPos is out of range"
    where
      len = T.length t
  Pos rxa rxb c -> do
    let matches =
          [ i
          | i <- [0 .. T.length t]
          , let (a, b) = T.splitAt i t
          , rxa `matchesSuffix` a
          , rxb `matchesPrefix` b
          ]
    c' <- evalInt c
    let ix = if c' >= 0 then c' else c' + length matches
    liftError (const "not enough matches") $ matches !! ix

matchesChar :: Token -> Char -> Bool
matchesChar = \case
  SomeOf cls -> isCls cls
  SomeNotOf cls -> not . isCls cls
  _ -> const False

-- FIXME: StartTok and EndTok are not implemented
matchesPrefix :: RegExp -> Text -> Bool
matchesPrefix [] _ = True
matchesPrefix (r : rx) txt
  -- as == "" if `r` is a special token like StartTok or EndTok
  = let (as, bs) = T.span (matchesChar r) txt
    in not (T.null as) && matchesPrefix rx bs

matchesSuffix :: RegExp -> Text -> Bool
matchesSuffix rx = matchesPrefix (reverse rx) . T.reverse


substr :: Text -> Int -> Int -> Text
substr txt start end = T.take (end - start) $ T.drop start txt

liftError :: MonadError e' m => (e -> e') -> Either e x -> m x
liftError f = liftEither .  either (Left . f) pure
