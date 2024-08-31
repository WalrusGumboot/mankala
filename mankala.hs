import Data.Char (digitToInt)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdin)
import Text.Printf (printf)

boardLength :: Int = 6

data TurnIndicator = A | B deriving (Enum, Eq, Show)

otherTurn :: TurnIndicator -> TurnIndicator
otherTurn A = B
otherTurn B = A

data BoardState = BoardState
  { aTiles :: [Int],
    bTiles :: [Int],
    aPoints :: Int,
    bPoints :: Int,
    turn :: TurnIndicator
  }
  deriving (Eq, Show)

swapTurn :: BoardState -> BoardState
swapTurn board = board {turn = otherTurn $ turn board}

pointsForPlayer :: TurnIndicator -> BoardState -> Int
pointsForPlayer turn = if turn == A then aPoints else bPoints

tilesForPlayer :: TurnIndicator -> BoardState -> [Int]
tilesForPlayer turn = if turn == A then aTiles else bTiles

activeTiles :: BoardState -> [Int]
activeTiles board = tilesForPlayer (turn board) board

otherTiles :: BoardState -> [Int]
otherTiles board = tilesForPlayer ((otherTurn . turn) board) board

addPointsForPlayer :: TurnIndicator -> Int -> BoardState -> BoardState
addPointsForPlayer turn pts board = if turn == A then board {aPoints = pointsForPlayer A board + pts} else board {bPoints = pointsForPlayer B board + pts}

addPointsForActive :: Int -> BoardState -> BoardState
addPointsForActive pts board = addPointsForPlayer (turn board) pts board

printBoard :: BoardState -> IO ()
printBoard board =
  let s =
        printf
          ( unlines
              [ "\ESC[2J\ESC[H",
                "          pts.     6   5   4   3   2   1           ",
                "        ┌─────┐  ┌───┬───┬───┬───┬───┬───┐         ",
                " %sB%s  │ %3d │  │%3d│%3d│%3d│%3d│%3d│%3d│         ",
                "        └─────┘  └───┴───┴───┴───┴───┴───┘         ",
                "                 ┌───┬───┬───┬───┬───┬───┐  ┌─────┐",
                " %sA%s           │%3d│%3d│%3d│%3d│%3d│%3d│  │ %3d │",
                "                 └───┴───┴───┴───┴───┴───┘  └─────┘",
                "                   1   2   3   4   5   6      pts. "
              ]
          )
          (if turn board == B then "[ " else "  ")
          (if turn board == B then " ]" else "  ")
          (bPoints board)
          (bTiles board !! 5)
          (bTiles board !! 4)
          (bTiles board !! 3)
          (bTiles board !! 2)
          (bTiles board !! 1)
          (head $ bTiles board)
          (if turn board == A then "[ " else "  ")
          (if turn board == A then " ]" else "  ")
          (head $ aTiles board)
          (aTiles board !! 1)
          (aTiles board !! 2)
          (aTiles board !! 3)
          (aTiles board !! 4)
          (aTiles board !! 5)
          (aPoints board)
   in putStrLn s

newGame :: BoardState =
  BoardState
    { aTiles = replicate boardLength 4,
      bTiles = replicate boardLength 4,
      aPoints = 0,
      bPoints = 0,
      turn = A
    }

-- index in board from perspective of active player
get :: Int -> BoardState -> Int
get idx board
  | idx < boardLength = activeTiles board !! idx
  | idx < 2 * boardLength = otherTiles board !! (idx - boardLength)
  | otherwise = get (idx - 2 * boardLength) board

updateList :: Int -> [Int] -> (Int -> Int) -> [Int]
updateList idx lst updateFn =
  zipWith (curry (\x -> if fst x == idx then updateFn $ snd x else snd x)) [0 ..] lst

maybeUpdateBoard :: Int -> (Int -> Int) -> TurnIndicator -> TurnIndicator -> BoardState -> [Int]
maybeUpdateBoard idx updateFn targetTurn selfTurn board =
  let selfTiles = tilesForPlayer selfTurn board
   in if turn board == targetTurn then updateList idx selfTiles updateFn else selfTiles

-- index in board from perspective of active player, function describing what to do with that tile
update :: Int -> (Int -> Int) -> BoardState -> BoardState
update idx updateFn board
  | idx < boardLength =
      board
        { aTiles = maybeUpdateBoard idx updateFn A A board,
          bTiles = maybeUpdateBoard idx updateFn B B board
        }
  | idx < 2 * boardLength =
      let newIdx = idx - boardLength
       in board
            { aTiles = maybeUpdateBoard newIdx updateFn B A board,
              bTiles = maybeUpdateBoard newIdx updateFn A B board
            }
  | otherwise = update (idx - 2 * boardLength) updateFn board

-- remaining stones in the current sequence,
-- index to be deployed to from perspective of active player,
-- already dropped stone in own points basket,
-- board
moveRemaining :: Int -> Int -> Bool -> BoardState -> BoardState
moveRemaining 0 idx alreadyDropped board
  | get (idx - 1) board == 1 && (idx - 1) < boardLength =
      let stealIdx = idx + 2 * (boardLength - idx)
          steal = get stealIdx board + 1
          zeroedOtherTileBoard = update stealIdx (const 0) board
          zeroedBothTilesBoard = update (idx - 1) (const 0) zeroedOtherTileBoard
       in swapTurn $ if get stealIdx board == 0 then board else addPointsForActive steal zeroedBothTilesBoard
  | idx == boardLength && alreadyDropped = board
  | otherwise = swapTurn board
moveRemaining remaining idx alreadyDropped board
  | idx == boardLength && not alreadyDropped = moveRemaining (remaining - 1) idx True (addPointsForActive 1 board)
  | otherwise = moveRemaining (remaining - 1) ((idx + 1) `mod` (2 * boardLength)) alreadyDropped (update idx (+ 1) board)

move :: Int -> BoardState -> BoardState
move idx board = moveRemaining (get idx board) (idx + 1) False (update idx (const 0) board)

displayAndQuery :: [Int] -> IO ()
displayAndQuery moveStack =
  let mostRecentState = foldr move newGame moveStack
   in do
        printBoard mostRecentState
        moveChar <- getChar
        let idx = digitToInt moveChar - 1
        displayAndQuery (idx : moveStack)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  displayAndQuery []
