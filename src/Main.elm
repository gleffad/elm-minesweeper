module Main exposing (..)

import Array exposing (..)
import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Browser.Events exposing (onKeyDown)
import Mine
import Minesweeper.Grid as Grid exposing (..)
import Minesweeper.Game as Game exposing (..)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Json.Decode as Json
import Html.Attributes exposing (disabled)

{-| length of the grid
available lengths :
- 5
- 16
- 32
-}
lengthGrid : number
lengthGrid =
    16

{-| Model :
- grid : grid of the minesweeper
- gameStatus : status of the current game
- ctrl : Check if Ctrl key has been pressed -> used to put flags on cells
-}
type alias Model =
    { grid : List Cell, gameStatus: Game, ctrl : Bool }

exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = lengthGrid - 1
        , height = lengthGrid - 1
        , minMines = 8
        , maxMines = 16
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

{-| Initialize the Model -}
init : ( Model, Cmd Msg )
init =
    ( { grid = Grid.initGrid lengthGrid lengthGrid, gameStatus = Running, ctrl = False }, exampleGenerateRandomMines )

{-| Msg :
- MinesGenerated : called when mines have been initialize
- ClickOnCell : Called when the user has clicked on a cell
- HandleKeyboadEvent : Called when the user pressed a key, used to put flags (with Ctrl key) 
-}
type Msg
    = MinesGenerated (List ( Int, Int ))
    | ClickOnCell Cell
    | HandleKeyboardEvent KeyboardEvent

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickOnCell c ->
            if model.ctrl then
                if c.display == Flag then
                    let new_grid = List.sortBy .position (Grid.set model.grid { c | display = Hidden } c.position   []) in
                    ( { model | grid = new_grid, gameStatus = Game.updateGame new_grid new_grid, ctrl = False }, Cmd.none )
                else
                    let new_grid = List.sortBy .position (Grid.set model.grid { c | display = Flag } c.position   []) in
                    ( { model | grid = new_grid, gameStatus = Game.updateGame new_grid new_grid, ctrl = False }, Cmd.none )
            else
                if c.display == Flag then
                    ( model, Cmd.none )
                else
                    let new_grid = Grid.revealCellsClick model.grid c.position lengthGrid lengthGrid in
                    ({ model | grid = new_grid, gameStatus = Game.updateGame new_grid new_grid}, Cmd.none )  
                
        MinesGenerated mines ->
            ( { model | grid = Grid.initNumber (Grid.initMines model.grid mines) mines lengthGrid lengthGrid, gameStatus = Game.updateGame model.grid model.grid}, Cmd.none )

        HandleKeyboardEvent event -> 
            if event.ctrlKey then 
                ( { model | ctrl = True } , Cmd.none ) 
            else 
                ( model, Cmd.none )

{-| Decode the key pressed by the user -}
subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)

{-| display the content of a cell or a flag -}
printType : Cell -> String
printType cell =
    case cell.display of
        Hidden -> ""
        Flag -> "|>"
        Visible ->
            case cell.status of
                Bomb -> "*"
                Number n ->  String.fromInt n
                Empty -> ""

{-| Return graphic element to display a cell in function of its status -}
viewCell : Game -> Cell -> Html Msg
viewCell game c =
    case c.display of
        Hidden ->
            div [] [ button [ onClick (ClickOnCell c), disabled (not (Game.gameIsRunning game))] [ text (printType c) ] ]

        Flag ->
            div [] [ button [ onClick (ClickOnCell c), disabled (not (Game.gameIsRunning game)) ] [ text (printType c) ] ]

        Visible ->
            div [] [ text (printType c) ]

{-| Display the status of the game -}
viewStatus: Game -> Html Msg
viewStatus game =
    case game of
       Running -> div [] []
       Loose -> h1 [] [ text "Vous avez perdu" ]
       Win -> h1 [] [ text "Vous avez gagné" ]

view : Model -> Html Msg
view model =
    div [] [
            div [ class "grid", class "level16" ] ((List.map (viewCell model.gameStatus) model.grid)),
            viewStatus model.gameStatus
        ]

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = subscriptions
        }