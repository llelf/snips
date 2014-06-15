import qualified Data.Map as M
import Data.List
import Control.Monad
import Data.Maybe
import Text.Printf
import Safe
import Data.List.Split as Split
import Debug.Trace

data Suit = A | B | C | D deriving (Enum,Show,Eq,Ord,Read)
type Rank = Int
data Card = Card { suit::Suit, rank::Rank }
            | Joker
              deriving (Eq)


instance Ord Card where
    compare Joker Joker = EQ
    compare Joker _     = GT
    compare _ Joker     = LT
    compare (Card s r) (Card s' r')
        | r==r'         = compare s s'
        | otherwise     = compare r r'

instance Show Card where
    show Joker = "Okey"
    show (Card s r) = show s ++ show r


allSuits = [A .. D]

deck = concat . replicate 2 $ oneDeck
    where oneDeck = [Joker,Joker] ++ allCards

allCards = [ Card s v | s <- allSuits, v <- [1..13] ]



-- | Hand: size and map of Card->count
data Hand = Hand Int (M.Map Card Int)
          deriving Show

data Group = Set Rank           -- ^ Set is of this rank
                 [Suit]         -- ^ remaining free suits to add to this Set
                 [Card]
           | Run Dir            -- ^ direction
                 Bool           -- ^ can grow more
                 [Card]
           | Pair [Card]
             deriving (Show,Eq)

data Dir = Up | Down deriving (Show,Eq)



win ls = concat [ win' t [] hand | t <- [SetNRuns,Pairs] ]
    where hand = handFromList ls


-- | Given already (maybe partially formed) groups and the rest of the
-- cards return all possible full groups

data TryGroups = SetNRuns | Pairs deriving Eq

win' :: TryGroups -> [Group] -> Hand -> [[Group]]

win' _ r (Hand _ rest) | M.null rest = [r]

win' w groups rest@(Hand _ hand)
    = (if newGroupsOk then newGroups else [])
      ++ growGroups
    where
      newGroupsOk | w==SetNRuns = length groups < 4
                  | w==Pairs    = length groups < 7

      -- | We try to
      -- 1) form new groups starting by min card in hard,
      -- - it's ok for sets — we don't care,
      -- - it's not ok for runs, — because of …,13,1, but we have
      -- reverse-direction Run for that case, it's faster that
      -- checking many mins here
      newGroups = concat.concat $ 
                  [ [ win' w (g : groups) rest' | g <- fun min ]
                    | fun <- newGroupConstructors ]
          where min = minInHand rest
                rest' = without min rest

      newGroupConstructors | w==SetNRuns = [newRuns,newRevRuns,newSets]
                           | w==Pairs    = [newPairs]

      -- | 2) grow each existing group:
      -- we know which card can go there, so we check if we have them in hand
      growGroups = concat [ win' w (grow next g' : gs) rest'
                            | g <- groups, let gs = groups\\[g],
                              (g',next) <- nextInGroup g,
                              isThere next rest,
                              let rest' = without next rest ]



minInHand (Hand _ r) = c
    where (c,_) = M.findMin r



isWin :: [Group] -> Bool
isWin gs = all (\g -> groupSize g `elem` [3..5]) gs
           || all (\g -> groupSize g == 2) gs

groupSize :: Group -> Int
groupSize (Run _ _ g) = length g
groupSize (Set _ _ g) = length g
groupSize (Pair g)    = length g


isThere :: Card -> Hand -> Bool
isThere c (Hand _ rest) = c `M.member` rest
                          || Joker `M.member` rest



-- all following functions handle the Jokers transparently:
-- if we want a card and it's now there but we have a Joker, it's ok

without :: Card -> Hand -> Hand
without c (Hand n rest)
    | c `M.member` rest = Hand (n-1) $ withoutElem c rest
    | otherwise         = Hand (n-1) $ withoutElem Joker rest


withoutElem x mp | mp M.! x > 1 = M.adjust pred x mp
                 | otherwise    = M.delete x mp


-- | Add card to group
grow :: Card -> Group -> Group
grow c (Set r suits cc) = Set r (suits\\[suit c]) (c:cc)
grow c (Run x g cc)     = Run x g (c:cc)
grow c (Pair cc)        = Pair (c:cc)

-- | All the possible group additions (and the modified group)
nextInGroup :: Group -> [(Group,Card)]
nextInGroup g@(Set r suits cs)
    | length cs < 4 = [ (g, Card s r) | s <- suits ]
    | otherwise     = []

nextInGroup (Run _ False _) = []
nextInGroup g@(Run dir True cs@(Card s r : rest))
    | length cs < 5 = [ (g', Card s r') ]
    | otherwise     = []
       where (g',r') = case dir of
                         Up   -> if r==13 then (dontGrowMore g, 1) else (g, r+1)
                         Down -> if r==1 then (g, 13) else (g, r-1)

nextInGroup g@(Pair [c]) = [(g,c)]
nextInGroup _            = []


dontGrowMore (Run d _ cs) = Run d False cs


-- | new possible sets and runs with the certain card
newSets, newRuns, newRevRuns, newPairs :: Card -> [Group]

newSets c@(Card s r) = [ Set r (allSuits\\[s]) [c] ]
newSets Joker = [ Set (rank c) (allSuits\\[suit c]) [c] | c <- allCards ]

newRuns c@Card{} = [ Run Up True [c] ]
newRuns Joker = [ Run Up True [c] | c <- allCards ]

newRevRuns c@(Card s r) = [ Run Down True [c] | r == 1 ]
newRevRuns Joker        = []

newPairs c@Card{} = [ Pair [c] ]
newPairs Joker    = [ Pair [c] | c <- allCards ]





tests :: [(String,Bool)]
tests = [
 ("A1 B1 C1 A2 B2 C2 A5 B5 C5 A10 A11 A12 A13 J", True),
 ("D1 B1 C1 A2 B2 C2 A5 B5 C5 A10 A11 A12 A13 A9", True),
 ("A1 B1 C1 A2 B2 C2 A5 B5 C5 A10 J A12 A13 A1", True), -- 111
 ("A1 B1 C1 A3 B3 C3 A5 B5 C5 A7 B7 C7 A9 B9", False),
 ("A1 B1 C1 A3 B3 C3 A5 B5 C5 A7 B7 C7 J B9", False),
 ("A1 B1 C1 A3 B3 C3 A5 B5 C5 A7 B7 C7 J J", True),
 ("A1 B1 C1 A3 B3 C3 A5 B5 C5 A7 B7 J J B9", False),
 ("A1 B1 C1 A3 B3 C3 A5 B5 C5 A7 J J J B9", False), --
 ("A1 B1 C1 A3 B3 C3 A5 B5 C5 J J J J B9", True),   --
 ("A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A1", True), -- 111
 ("J  J  J  J  J  J  A7 A8 A9 A10 A11 A12 A13 A1", True),
 ("J  J  J  J  J  J  J J  J J J J J", True),
 ("A3 A4 A5 A6 A7 B5 B6 B7 B8 B9 D1 D2 D3 D4", True),
 ("A3 A4 A5 A6 A7 B5 B6 B7 B8 B9 D1 A1 B1 J", True),
 ("A1 B1  A13 A12 A11 A10  B13 B12 B11  C12 C11 C10 C9 C8",True) -- x111
 ]

tests2 = [
 ("A7 A6  B1 B2 B3  C1 C2 C3 C4 C10  J J J J", True),
 ("A7 A6 J A4 A3   J B10 B11 B12 J   A1 J C1 D1", True),
 ("A7 A6 J A4 A3   B2 B10 B11 B12 B13  A1 J J D1", False),
 ("A7 A6 J A4 A3   J B10 B11 B12 B13  A1 J J D1", True),
 ("A7 A6 J A4 A3   J B10 B11 B12 B13  A2 J J D2", True),
 ("A7 A6 J A4 A3   J B1  B11 B12  C2  C3 J J C5", True),
 ("A1 A1 B3 B3 C3 C3 D7 D7 D11 D11 B12 B12 A5 A5", True),
 ("A1 A1 B3 B3 B3 B3 D7 D7 D7 D7 D7 D7 A5 A5", True),
 ("A1 A1 B3 B3 B3 B3 D7 D7 D7 D7 D7 D7 A5 J", True),
 ("A1 A1 B3 B3 B3 B3 D7 D7 D7 D7 D7 J A5 J", True)
-- ("A1 A4 A7 A13 B2 B5 B12 C3 J J J J J J", False)
-- ("A1 A4 A7 A13 B2 B5 B12 J J J J J J J", True)
 ]

test = forM_ (tests++tests2) $ \(test,ok) -> do
         let sol = find isWin . win $ parseDeck test
         printf "%s\t%s\n" (show sol)
                           (if ok == isJust sol then ""::String else "*******")


main = test

check_reveal :: [Card] -> Bool
check_reveal = isJust . find isWin . win


handFromList cc = Hand (length cc) $ M.fromListWith (+) [ (c,1) | c <- cc ]

parseCard "J" = Joker
parseCard (s:r) = Card (read [s]) (read r)

parseDeck = map parseCard . words


