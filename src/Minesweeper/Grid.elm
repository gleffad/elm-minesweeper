module Minesweeper.Grid exposing (Type(..), Status(..), Position, Cell, initGrid, initMines, initNumber, revealCellsClick, set, Grid)
import Mine exposing (..)
import Html.Attributes exposing (width)
import Html.Attributes exposing (height)
import Html.Attributes exposing (list)

{-| type of a cell -}
type Type
    = Bomb
    | Number Int
    | Empty

{-| status of a cell -}
type Status
    = Visible
    | Hidden
    | Flag

{-| position of a cell -}
type alias Position =
    ( Int, Int )

{-| representation of a cell in a grid -}
type alias Cell =
    { display : Status
    , position : Position
    , status : Type
    }

type alias Grid = List Cell

-- {-| test grid -}
-- mytest: Grid
-- mytest = [ 
--     { display = Hidden, position = (0, 0), status = Empty }, { display = Hidden, position = (1, 0), status = Empty }, 
--     { display = Hidden, position = (2, 0), status = Empty }, { display = Hidden, position = (3, 0), status = Empty }, 
--     { display = Hidden, position = (4, 0), status = Empty }, { display = Hidden, position = (0, 1), status = Empty },
--     { display = Hidden, position = (1, 1), status = Number 1 }, { display = Hidden, position = (2, 1), status = Number 1 },
--     { display = Hidden, position = (3, 1), status = Number 1 }, { display = Hidden, position = (4, 1), status = Empty },
--     { display = Hidden, position = (0, 2), status = Empty }, { display = Hidden, position = (1, 2), status = Number 1 },
--     { display = Hidden, position = (2, 2), status = Bomb }, { display = Hidden, position = (3, 2), status = Number 1 },
--     { display = Hidden, position = (4, 2), status = Empty }, { display = Hidden, position = (0, 3), status = Empty },
--     { display = Hidden, position = (1, 3), status = Number 1 }, { display = Hidden, position = (2, 3), status = Number 1 },
--     { display = Hidden, position = (3, 3), status = Number 1 }, { display = Hidden, position = (4, 3), status = Empty },
--     { display = Hidden, position = (0, 4), status = Empty }, { display = Hidden, position = (1, 4), status = Empty },
--     { display = Hidden, position = (2, 4), status = Empty }, { display = Hidden, position = (3, 4), status = Empty },
--     { display = Hidden, position = (4, 4), status = Empty }] 

{-| Return the cell at position or Nothing if it doesn't exists -}
get : Grid -> (Int, Int) -> Maybe Cell
get grid position =
    case grid of
    [] -> Nothing
    h :: t -> if h.position == position then Just h else get t position

{-| Change the cell at position with cell and return the new grid -}
set: Grid -> Cell -> (Int, Int) -> Grid -> Grid
set grid cell position gridL =
    case grid of
    [] -> grid
    h :: t -> if h.position == position then gridL ++ (cell :: t) else set t cell position (h :: gridL)

{-| create a new grid of size width * height -}
initGrid : Int -> Int -> Grid
initGrid width height = initGridAux 0 0 width height []

{-| create a new grid column by column -}
initGridAux : Int -> Int -> Int -> Int -> Grid -> Grid
initGridAux i j l h res =
    if i < l then
        initGridAux (i+1) j l h (res ++ (initGridLine i j h []))
    else
        res

{-| create a line to create a new grid -}
initGridLine: Int -> Int -> Int -> Grid -> Grid
initGridLine i j h res =
    if j < h then
        initGridLine i (j+1) h (res ++ [{display = Hidden, position = (i, j), status = Empty}])
    else
        res

{-| generate a list of mines and add it to the grid -}
initMines : Grid -> List Mine -> Grid
initMines grid mines = initMinesAux grid mines []

{-|  -}
initMinesAux : Grid -> List Mine -> Grid -> Grid
initMinesAux grid mines res = case (grid, mines) of
    (h1 :: t1, h2 :: t2 ) -> 
        if h1.position == h2 then
            initMinesAux t1 t2 (res ++ [{display = Hidden, position = h1.position, status = Bomb}])
        else
            initMinesAux t1 mines (res ++ [h1])
    (h1 :: t1, []) -> initMinesAux t1 [] (res ++ [h1])
    ([], h2 :: t2) -> res
    ([], []) -> res

{-| Return a list of adjacents positions of mines -}
listPostionNumber : List Mine -> List (Int, Int) -> Int -> Int -> List (Int, Int)
listPostionNumber mines res width height = case mines of
    [] -> res
    h :: t -> 
        let 
            x = Tuple.first h
            y = Tuple.second h
        in
            listPostionNumber t (res ++ [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x,y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]) width height

{-| filter the grid so nothing is out limits -}
filterBorn : Grid -> List (Int, Int) -> List (Int, Int) -> List (Int, Int)
filterBorn grid list res = case list of
    [] -> res
    h :: t -> case get grid h of
            Nothing -> filterBorn grid t res
            Just cell -> case cell.status of
                            Bomb -> filterBorn grid t res
                            Number n -> filterBorn grid t (h::res)
                            Empty -> filterBorn grid t (h::res)

{-| Set cells with good numbers -}
initNumber : Grid -> List Mine -> Int -> Int -> Grid
initNumber listDesCellules mines width height = 
    let 
        posMines = listPostionNumber mines [] width height
        posMineFilterBorn = filterBorn listDesCellules posMines [] 
        listDesAdjacents = posMineFilterBorn
    in
        List.sortBy .position (incrementCells listDesAdjacents listDesCellules)

{-| Increment by one all adjacents cells of mines -}
incrementCells: List (Int, Int) -> Grid -> Grid
incrementCells list grid =
    case list of
        [] -> grid
        h :: t -> case get grid h of
                Nothing -> incrementCells t grid
                Just cell -> case cell.status of
                            Bomb -> incrementCells t grid
                            Empty -> incrementCells t (set grid {cell | status = Number 1} h [])
                            Number n -> incrementCells t (set grid {cell | status = Number (n+1)} h [])

{-| Return the list of adjacents of cell at position -}
listAdjacents: Grid -> (Int, Int) -> Int -> Int -> List (Int, Int)
listAdjacents grid position width height =
    let
        x = Tuple.first position
        y = Tuple.second position
    in
    case get grid position of
        Nothing -> []
        Just c ->  adaptList grid [ (x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1) ] width height []

{-| Adapt the list of adjacents -}
adaptList: Grid -> List(Int, Int) -> Int -> Int -> List(Int, Int) -> List(Int, Int)
adaptList grid list width height res =
    case list of
        [] -> res
        h :: t -> case get grid (Tuple.first h, Tuple.second h) of
                    Nothing -> adaptList grid t width height res
                    Just c -> case c.display of
                        Visible -> adaptList grid t width height res
                        Flag -> adaptList grid t width height res
                        Hidden -> case c.status of
                            Bomb ->  adaptList grid t width height res
                            Number n -> adaptList grid t width height (res ++ [h])
                            Empty -> adaptList grid t width height (res ++ [h])

{-| Reveal the cells in position and its adjacents if necessary -}
revealAdjacents: Grid -> List (Int, Int) -> Int -> Int -> Grid
revealAdjacents grid positions width height =
    case positions of
    [] -> grid
    h :: t -> case get grid h of
              Nothing -> grid
              Just cell -> revealAdjacents (revealCell (set grid (revealCellClick cell) cell.position []) cell width height) t width height

{-| Reval cell and its adjacents if its status is empty and do not reveal if not -}
revealCell : Grid -> Cell -> Int -> Int -> Grid
revealCell grid cell width height =
    case cell.status of
    Bomb -> grid
    Number n -> grid
    Empty -> revealAdjacents grid (listAdjacents grid cell.position width height) width height

{-| Reveal the cell that has been clicked -}
revealCellClick: Cell -> Cell
revealCellClick cell =
    { cell | display = Visible }

{-| Reveal the cell clicked and its adjacents if necessary -}
revealCellsClick : Grid -> (Int, Int) -> Int -> Int -> Grid
revealCellsClick grid position width height =
    case get grid position of
       Nothing -> grid
       Just c -> List.sortBy .position (revealCell (set grid (revealCellClick c) position []) (revealCellClick c) width height)