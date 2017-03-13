module Main exposing (..)

import Bitwise
import List as L
import List.Extra as L
import Html exposing (..)
import Svg exposing (..)
import Time exposing (Time, second)

type alias Loc2d = (Int, Int)

type alias Reg = List Int

-- Unchecked invariant: the matrix is square, i.e. A.length m ==
-- A.length (get i m) for any i, modulo Maybe shenanigans
type alias Mat = List Reg

-- Functions to extend these things infinitely with zeros,
-- conceptually.  Shouldn't be necessary to tread outside established
-- locations, but just in case (and to satisfy the types).

getBit : Int -> Reg -> Int
getBit i r =
    case L.getAt i r of
        Nothing -> 0
        Just b -> b

getRow : Int -> Mat -> Reg
getRow i m =
    case L.getAt i m of
        Nothing -> zeroReg (L.length m)
        Just r -> r

getCell : Loc2d -> Mat -> Int
getCell (x, y) m = getBit y (getRow x m)


-- Reg and Mat builder functions

zeroReg : Int -> Reg
zeroReg n = L.repeat n 0

zeroMat : Int -> Mat
zeroMat n = L.repeat n (zeroReg n)

makeList : Int -> (Int -> a) -> List a
makeList len f =
    let helper i len f =
            if (i >= len) then []
            else f i :: helper (i + 1) len f
    in helper 0 len f

makeMat : (Reg -> Int -> Int -> Int) -> Reg -> Mat
makeMat f vector =
    let dim = L.length vector
    in L.map (\x -> makeList dim (f vector x))
        (L.range 0 (dim - 1))

-- make a LFSR by specifying the taps for producing the 0th bit
makeLFSR : Reg -> Mat
makeLFSR =
    makeMat (\ toprow x y ->
                 if x == 0 then
                     getBit y toprow
                 else if x - y == 1 then 1 else 0)

-- make a CASR by specifying the diagonal entries
makeCASR : Reg -> Mat
makeCASR =
    makeMat (\ diagonal x y ->
                 if x == y then
                     getBit x diagonal
                 else if x - y == 1 || y - x == 1 then 1 else 0)


-- Linear algebra functions

dotProduct : Reg -> Reg -> Int
dotProduct r1 r2 =
    let hadamardProduct = L.map2 Bitwise.and r1 r2
    in L.foldr Bitwise.xor 0 hadamardProduct

evolveReg : Mat -> Reg -> Reg
evolveReg m r =
    L.map (\i -> dotProduct r (getRow i m)) (L.range 0 (L.length r - 1))


-- ENTRY

main = Html.program { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                    }

-- MODEL

type alias Model =
    { matrix : Mat
    , reg : Reg
    , ghosts : List Reg

    , numGhosts : Int
    }

init : (Model, Cmd Msg)
init =
    ( { matrix = makeCASR [0,0,1,1,0]
      , reg = [0,0,0,0,1]
      , ghosts = []
      , numGhosts = 0
      }
    , Cmd.none)

-- UPDATE

type Msg = Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick ->
            ( { model | reg = evolveReg model.matrix model.reg}
            , Cmd.none
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Time.every second (\x -> Tick)

-- VIEW

view : Model -> Html Msg
view model =
    div [] (L.map (toString >> Html.text) model.reg)
