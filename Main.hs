module Main where

data Color = R | B deriving(Show, Eq)

data RBTree a = Null | Tree Color (RBTree a) a (RBTree a) deriving(Show, Eq)

type Set a = RBTree a

empty :: Set a
empty = Null

member :: Ord a => a -> Set a -> Bool 
member x Null = False 
member x (Tree _ l y r)
    | x < y = member x l
    | x > y = member x r
    | otherwise = True

insert :: Ord a => a -> Set a -> Set a
insert x s = makeBlack $ ins s
    where 
        ins Null = Tree R Null x Null
        ins t@(Tree c l y r) 
            | x < y = balance c (ins l) y r
            | x > y = balance c l y (ins r)
            | otherwise = t
        makeBlack (Tree _ l y r) = Tree B l y r
        
balance :: Color -> Set a -> a -> Set a -> Set a
balance B (Tree R (Tree R a x b) y c) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance B (Tree R a x (Tree R b y c)) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance B a x (Tree R (Tree R b y c) z d) = Tree R (Tree B a x b) y (Tree B c z d)
balance B a x (Tree R b y (Tree R c z d)) = Tree R (Tree B a x b) y (Tree B c z d)
balance c a x b = Tree c a x b
        
main :: IO ()
main = undefined