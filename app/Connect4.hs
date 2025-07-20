{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Connect4
  ( redStarts
  , board
  , redChar
  , yellowChar
  , redPoints
  , yellowPoints
  , newGame
  , turn
  , play
  , winner
  , Piece (..)
  , Board
  , Column
  ) where

import Data.Char
import Data.List
import Data.Word
import Data.Maybe

type Bitstring = [Bool]

data Piece = Red | Yellow deriving (Eq, Show)
type Column = [Piece]
type Board = [Column]
data State = State
  { redStarts'  :: Bool
  , board'      :: Board
  , redChar'    :: Char
  , yellowChar' :: Char
  , redPoints'  :: Int
  , yellowPoints'  :: Int
  } deriving (Eq, Show)


redStarts :: Word64 -> Bool
redStarts = redStarts' . word64ToState

board :: Word64 -> Board
board = board' . word64ToState

redChar :: Word64 -> Char
redChar = redChar' . word64ToState

yellowChar :: Word64 -> Char
yellowChar = yellowChar' . word64ToState

redPoints :: Word64 -> Int
redPoints = redPoints' . word64ToState

yellowPoints :: Word64 -> Int
yellowPoints = yellowPoints' . word64ToState


bitstringToWord64 :: Bitstring -> Word64
bitstringToWord64 = foldl (\prev bit -> prev * 2 + if bit then 1 else 0) 0

word64ToBitstring :: Word64 -> Bitstring
word64ToBitstring x = replicate (64 - length bs) False <> bs
  where
    bs  = reverse $ h x
    h 0 = []
    h x = (odd x) : h (x `div` 2)

bitstringToChar :: Bitstring -> Char
bitstringToChar = chr . (+ 65) . fromIntegral . bitstringToWord64

charToBitstring :: Char -> Bitstring
charToBitstring = drop 59 . word64ToBitstring . fromIntegral . (subtract 65) . ord . toUpper

bitstringToPoints :: Bitstring -> (Int, Int)
bitstringToPoints = (`divMod` 5) . fromIntegral . bitstringToWord64

pointsToBitstring :: Int -> Int -> Bitstring
pointsToBitstring red yel = drop 59 . word64ToBitstring . fromIntegral $ red*5 + yel

pieceToBit :: Piece -> Bool
pieceToBit = (== Red)

columnToBitstring :: Column -> Bitstring
columnToBitstring c = replicate numEmpty False <> [True] <> pieceBits
  where
    pieceBits = pieceToBit <$> c
    numEmpty  = 6 - length c

boardToBitstring :: Board -> Bitstring
boardToBitstring = init . concat . fmap columnToBitstring

bitToPiece :: Bool -> Piece
bitToPiece b = if b then Red else Yellow

bitstringToColumn :: Bitstring -> Column
bitstringToColumn bs = bitToPiece <$> drop (1 + numEmpty) bs
  where numEmpty = fromJust $ elemIndex True bs

redDiff :: Board -> Integer
redDiff board' = foldl (\a c -> a + if c == Red then 1 else -1) 0 $ concat board'

lastBitstringToColumn :: Bitstring -> [Column] -> Bool -> Column
lastBitstringToColumn last easies redStarts' = toColumn lastBit
  where
    lastBit = any id [not $ any id last, diff == -1 && not redStarts', diff == 0, diff == 1 && redStarts']
    diff    = redDiff $ easies <> [toColumn True]
    toColumn b = bitstringToColumn $ last <> [b]

-- chunk the bitstring with the subarray lengths
chunkBitstring :: [Int] -> Bitstring -> [Bitstring]
chunkBitstring lengths bs = unfoldr h (lengths, bs)
  where
    h (_, []) = Nothing
    h (l:ls, bs) = Just $ (ls,) <$> splitAt l bs

bitstringToBoard :: Bitstring -> Bool -> Board
bitstringToBoard bs redStarts' = easies <> [lastBitstringToColumn lastCol easies redStarts']
  where
    easies  = bitstringToColumn <$> butLast
    butLast = take 6 $ chunkBitstring (repeat 7) bs
    lastCol = drop (6*7) bs

stateToBitstring :: State -> Bitstring
stateToBitstring s = concat
  [ singleton $ redStarts' s
  , boardToBitstring $ board' s
  , charToBitstring $ redChar' s
  , charToBitstring $ yellowChar' s
  , pointsToBitstring (redPoints' s) (yellowPoints' s)
  ]

bitstringToState :: Bitstring -> State
bitstringToState bs = State
  { redStarts'  = bsRedStarts'
  , board'      = bitstringToBoard bsBoard' bsRedStarts'
  , redChar'    = bitstringToChar bsRedChar'
  , yellowChar' = bitstringToChar bsYellowChar'
  , redPoints'
  , yellowPoints'
  }
  where
    (redPoints', yellowPoints') = bitstringToPoints bsPoints'
    [[bsRedStarts'], bsBoard', bsRedChar', bsYellowChar', bsPoints'] = chunkBitstring [1, 48, 5, 5, 5] bs

word64ToState :: Word64 -> State
word64ToState = bitstringToState . word64ToBitstring

stateToWord64 :: State -> Word64
stateToWord64 = bitstringToWord64 . stateToBitstring

newBoard :: Board
newBoard = replicate 7 []

newGame :: Bool -> Char -> Char -> Word64
newGame redStarts' redChar' yellowChar' = stateToWord64 $ State
  { redStarts'
  , board' = newBoard
  , redChar'
  , yellowChar'
  , redPoints' = 0
  , yellowPoints' = 0
  }

turn' :: State -> Piece
turn' s = bitToPiece $ diff == -1 || diff == 0 && redStarts' s
  where diff = redDiff $ board' s

turn :: Word64 -> Piece
turn = turn' . word64ToState

winner :: Word64 -> Maybe Piece
winner s
  | redPoints s == 4    = Just Red
  | yellowPoints s == 4 = Just Yellow
  | otherwise           = Nothing

dropPiece :: Int -> State -> State
dropPiece i s = s { board' = before <> [c <> [turn' s]] <> after }
  where (before, c:after) = splitAt i $ board' s

full :: Board -> Bool
full b = all ((== 6) . length) b

pieceAt :: Board -> (Int, Int) -> Maybe Piece
pieceAt b (c, r) = b !? c >>= (!? r)

addT :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addT (x, y) (z, w) = (x+z, y+w)

line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line dir = take 4 . iterate (addT dir)

fourInARow :: [Maybe Piece] -> Bool
fourInARow (p:ps) = all (== p) ps && p /= Nothing

lineIndices :: [[(Int, Int)]]
lineIndices = line <$> dirs <*> coords
  where
    coords = (,) <$> [0..6] <*> [0..5]
    dirs   = delete (0,0) $ (,) <$> [-1..1] <*> [-1..1]

roundWon :: Board -> Bool
roundWon b = any id wins
  where wins = fourInARow . fmap (pieceAt b) <$> lineIndices

data OverState = Ongoing | Tie | RWin | YWin
roundOver :: State -> OverState
roundOver s
  | roundWon b = if turn' s == Red then YWin else RWin
  | full b     = Tie
  | otherwise  = Ongoing
  where b = board' s

play' :: Int -> State -> State
play' validIdx s =
  case roundOver s' of
    RWin -> if redPoints == 3
      then s' { redPoints' = 4 }
      else s' { redStarts' = True, board' = newBoard, redPoints' = 1 + redPoints }
    YWin -> if yellowPoints == 3
      then s' { yellowPoints' = 4 }
      else s' { redStarts' = False, board' = newBoard, yellowPoints' = 1 + yellowPoints }
    Tie  -> s' { redStarts' = not $ redStarts' s, board' = newBoard }
    Ongoing -> s'
  where
    redPoints    = redPoints' s
    yellowPoints = yellowPoints' s
    s'           = dropPiece validIdx s

invalid :: Int -> State -> Bool
invalid i s = fromMaybe True $ (6 <=) . length <$> (board' s !? i)

play :: Int -> Word64 -> Word64
play i w = if invalid i s || done then w else stateToWord64 $ play' i s
  where
    s    = word64ToState w
    done = isJust $ winner w
