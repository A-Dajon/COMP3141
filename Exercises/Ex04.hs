module Ex04 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
-- tokenise  =  sequence.(fmap parseToken <$> words)
tokenise = mapM parseToken . words

newtype Calc a = C ([Int] -> Maybe ([Int], a))


pop :: Calc Int
-- C ([Int] -> Maybe ([Int], Int))
-- pop = error "'push' unimplemented"
pop = C doPop
  where
    doPop :: [a] -> Maybe ([a], a)
    doPop []      = Nothing
    doPop (x: xs) = Just (xs,x)

push :: Int -> Calc ()
push i = C (\xs -> Just(i:xs, ()))


instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s ->
      case sf s of
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s ->
      case sa s of
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

-- doPush :: [Token] -> Calc ()

evaluate :: [Token] -> Calc Int
evaluate xs = doEvaluate xs >> pop
  where
    doEvaluate :: [Token] -> Calc Int
    doEvaluate [] = pure 0
    doEvaluate (Number i:ts) = push i >> doEvaluate ts
    doEvaluate (Operator o:ts) = do
      x <- pop
      y <- pop
      push (o x y) >> doEvaluate ts

readCal :: Calc Int -> Maybe ([Int], Int)
readCal c = unwrapCalc c []
  where unwrapCalc (C a) = a

another :: Maybe (Maybe ([Int], Int)) -> Maybe Int
another (Just (Just(_, i))) = Just i
another _ = Nothing

calculate :: String -> Maybe Int
-- calculate s = pop >> evaluate >> tokenize s
calculate s = another $ readCal . evaluate <$> tokenise s





