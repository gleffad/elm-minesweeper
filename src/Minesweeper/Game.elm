module Minesweeper.Game exposing (Game (..), updateGame, gameIsRunning)

import Minesweeper.Grid as Grid exposing (..)

{-| Status of a game -}
type Game =
    Running
    | Loose
    | Win

{-| return True if the game is running, False if not -}
gameIsRunning: Game -> Bool
gameIsRunning game =
    case game of
       Running -> True
       Loose -> False
       Win -> False

{-| Check if the user clicked on a mine -}
updateGame: Grid -> Grid -> Game
updateGame initGrid grid  =
    case grid of
    [] -> win initGrid
    h :: t -> case h.status of
                Number n -> updateGame initGrid t
                Empty -> updateGame initGrid t
                Bomb -> case h.display of
                        Hidden -> updateGame initGrid t
                        Flag -> updateGame initGrid t
                        Visible -> Loose

{-| Check if the game is over -}
win: Grid -> Game
win grid =
    case grid of
    [] -> Win
    h :: t -> case h.status of
                Number n -> case h.display of
                        Hidden -> Running
                        Flag -> Running
                        Visible -> win t
                Empty -> case h.display of
                        Hidden -> Running
                        Flag -> Running
                        Visible -> win t
                Bomb -> case h.display of
                        Hidden -> win t
                        Flag -> win t
                        Visible -> Loose