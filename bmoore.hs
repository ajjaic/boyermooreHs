import Control.Monad.Trans.State
import qualified Data.Map.Lazy as Map
import Data.Array

type Text = String
type Pattern = String
type ArrText = Array Int Char
type ArrPat = Array Int Char
type Bmtable = Map.Map Char Int

--sample
text_sample = "trusthardtooxhbrtooth"
pattern_sample = "tool"
--

check plen = [(plen-1), (plen-2) .. 1]

genBadMatchTable :: Pattern -> Bmtable
genBadMatchTable p = execState (st p rindices) Map.empty where
    rindices = [(plen-1), (plen-2) .. 1]
    plen = length p
    st [x] _         = do l <- get
                          if Map.member x l
                            then put l
                            else put (Map.insert x plen l)
    st (x:xs) (y:ys) = do l <- get
                          put (Map.insert x y l)
                          st xs ys


data Substr = Substr { arrt :: ArrText, arrp :: ArrPat} deriving (Show)

makeSubstr :: Text -> Pattern -> Substr
makeSubstr t p = Substr at ap
  where
    at = listArray (1, length t) t
    ap = listArray (1, length p) p


match :: Substr -> Bmtable -> Bool
match txtpat bmt = comp $ snd bp
  where
    (bt, bp) = (bounds t, bounds p)
    (t, p) = (arrt txtpat, arrp txtpat)
    comp i = case compfromlast t p i bmt of
                Nothing -> True
                Just val -> if val > (snd $ bt)
                                then False
                                else comp val

compfromlast :: ArrText -> ArrPat -> Int -> Bmtable -> Maybe Int
compfromlast t p index bmt = if allequal then Nothing else Just newindex
  where
    pindice = snd $ bounds p
    indices = zip [index, (index-1) ..] [(pindice), (pindice-1) .. 1]
    allequal = all helper indices
    helper (ti, pi) = (t ! ti) == (p ! pi)
    lastcharintext = t ! index
    newindex = case Map.lookup lastcharintext bmt of
                    Just v -> v + index
                    Nothing -> index + pindice

demo = match (makeSubstr text_sample pattern_sample) (genBadMatchTable pattern_sample)
