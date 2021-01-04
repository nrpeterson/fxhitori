module Main where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Lazy
import FxHitori.Board
import FxHitori.Strategy

board :: Board
board = boardFromNums [[ 2, 8, 2, 6,10, 1, 9, 4, 2, 4],
                       [10, 9, 8, 4, 4, 7, 1, 5, 5, 6],
                       [ 2, 3, 5, 1, 7, 2, 9,10, 4, 5],
                       [ 4, 7, 9, 7, 8, 5, 3, 5, 1, 8],
                       [ 7, 5, 6,10, 4, 8, 4, 9, 6, 1],
                       [ 1, 3, 5, 9, 6, 4, 2, 8,10, 5],
                       [ 7, 2, 4, 8,10,10,10, 1, 5, 3],
                       [ 9, 6, 3, 2, 1, 3, 5, 8, 8, 2],
                       [ 4,10, 1, 7, 3, 4, 7, 6, 1, 9],
                       [ 5,10, 3, 9,10, 6,10, 8, 7, 2]]


runner :: MaybeT (WriterT [(Move, Explanation)] (State Board)) ()
runner = runForever $ stepRunner fullSolver

run :: MaybeT (WriterT w (State s)) a -> s -> (w, s)
run m st = (logs, finalSt)
    where ((_, logs), finalSt) = runState (runWriterT (runMaybeT m)) st

runForever :: Monad m => m a -> m ()
runForever = sequence_ . repeat

main :: IO ()
main = do
--    putStrLn $ concatMap printBoard [board1, board2, board3, board4, board5]
--    putStrLn $ concatMap show [move1, move2, move3, move4]
      let (res, finalBoard) = run runner board
      putStrLn $ printBoard finalBoard
      putStrLn $ concatMap ( (++ "\n") . show ) res