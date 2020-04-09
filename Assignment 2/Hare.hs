{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 
-- import HareMonad (Hare, hare, failure, readCharacter)

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a, b)
  Choose :: RE a -> RE a -> RE a -- is this right?
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b
match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char cs) = do
  x <- readCharacter
  guard (x `elem` cs)
  pure x
match (Seq a b) = do 
  ra <- match a 
  rb <- match b  
  pure (ra, rb)
match (Choose a b) = match a <|> match b
match (Star a) =
      addFront <$> match a <*> match (Star a)
  <|> pure []
  where 
    addFront x xs = x:xs
match (Action f a) = fmap f (match a)



matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action (\(a, b) -> a:b) (Seq x xs)

string :: String -> RE String
string [] = Action (\x -> []) Empty -- hacky way to represent Empty as RE String
string (x:xs) = cons (Char [x]) (string xs)

rpt :: Int -> RE a -> RE [a]
rpt 0 re = Action (\x -> []) Empty
rpt n re = cons (re) (rpt (n-1) re)

rptRange :: (Int, Int) -> RE a -> RE [a]
-- rptRange (x,y) re = Choose () -- could do recursively but nicer using map
rptRange (x,y) re = choose (map (\n -> rpt n re) ranges) where
  ranges = [y, y-1..x]

option :: RE a -> RE (Maybe a)
option re = Choose
  (Action (\r -> Nothing) Empty)
  (Action (\r -> Just r) re)

plus :: RE a -> RE [a]
-- plus re = Action (\(a, b) -> a:b) (Seq re (Star re))
plus re = cons re (Star re) -- equivalent

choose :: [RE a] -> RE a
choose [] = Fail
-- choose [r] = r -- unneeded
choose (r:rs) = Choose r (choose rs)

