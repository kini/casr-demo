module Main exposing (..)

import Bitwise
import List as L
import List.Extra as L
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

type alias Loc2d = (Int, Int)

add2d : Loc2d -> Loc2d -> Loc2d
add2d (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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
getCell (x, y) m = getBit x (getRow y m)


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
                 if x - y == 0 then
                     getBit x diagonal
                 else if x - y == 1 || x - y == -1 then 1 else 0)

-- make an incorrect CASR using rules 102 and 150 instead of 90 and 150
makeBadCASR : Reg -> Mat
makeBadCASR =
    makeMat (\ diagonal x y ->
                 if x - y == 1 then
                     getBit x diagonal
                 else if x - y == 0 || x - y == -1 then 1 else 0)


-- specific LFSRs and CASRs

-- these are in my memo
smallLFSR : Mat
smallLFSR = makeLFSR [0,0,1,0,1]
smallCASR : Mat
smallCASR = makeCASR [0,0,1,1,0]

-- these are from Hortensius
myCASR53 : Mat
myCASR53 = makeCASR [1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,0,0,1,0,1,0,1]
myCASR47 : Mat
myCASR47 = makeCASR [0,0,1,1,1,0,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,1,1,1,0,0,0,0,0,1,1,0,1]
myBadCASR53 : Mat
myBadCASR53 = makeBadCASR [1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,0,0,1,0,1,0,1]
myBadCASR47 : Mat
myBadCASR47 = makeBadCASR [0,0,1,1,1,0,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,1,1,1,0,0,0,0,0,1,1,0,1]

-- Linear algebra functions

dotProduct : Reg -> Reg -> Int
dotProduct r1 r2 =
    let hadamardProduct = L.map2 Bitwise.and r1 r2
    in L.foldr Bitwise.xor 0 hadamardProduct

evolveReg : Mat -> Reg -> Reg
evolveReg m r =
    L.map (\i -> dotProduct r (getRow i m)) (L.range 0 (L.length r - 1))


-- ENTRY

main : Program Never Model Msg
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

    , maxGhosts : Int
    , interval : Maybe Time
    , height : Int
    }

modelDim : Model -> Int
modelDim model = L.length model.reg

init : (Model, Cmd Msg)
init =
    ( { matrix = myCASR47
      , reg = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
      , ghosts = []
      , maxGhosts = 5
      , interval = Just (second / 16)
      , height = 400
      }
    , Cmd.none)

-- UPDATE

type Msg = Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick ->
            ( { model
                  | reg = evolveReg model.matrix model.reg
                  , ghosts = L.take model.maxGhosts
                             (model.reg :: model.ghosts)
              }
            , Cmd.none
            )

-- SUBSCRIPTIONS

const : b -> (a -> b)
const c = \x -> c

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.interval of
        Nothing -> Sub.none
        Just interval -> Time.every interval (const Tick)

-- VIEW

-- a simple view

viewSimple : Model -> Html Msg
viewSimple model =
    div [] (L.intercalate
                [br [] []]
                (L.map (L.map (toString >> Html.text)) model.matrix ++
                 [[]] ++
                 L.map (toString >> Html.text) model.reg ::
                 L.map (L.map (toString >> Html.text)) model.ghosts))


-- displaying the matrix

viewMatCell : Model -> Loc2d -> Html Msg
viewMatCell model (i, j) =
    let size = bitHeight model
        mycolor = if getCell (i, j) model.matrix == 1 then "black" else "white"
    in rect [ x (toString (10 + i * size))
            , y (toString (10 + j * size))
            -- , rx (toString (size // 8))
            -- , ry (toString (size // 8))
            -- , class ("r" ++ toString x ++ " c" ++ toString y)
            , fill mycolor
            -- , color "black"
            , width (toString size)
            , height (toString size)
            ]
        []


-- displaying the registers

bitHeight : Model -> Int
bitHeight model = model.height // modelDim model

bitWidth : Model -> Int -- as in, graphically...
bitWidth model = clamp 10 100 (bitHeight model)

regPos : Model -> Loc2d
regPos model = (10 + model.height + 50, 10)

ghostPos : Model -> Int -> Loc2d
ghostPos model idx =
    let ghostStart = add2d (regPos model)
                     (bitWidth model + 25 + bitWidth model + 25, 0)
    in add2d ghostStart ((bitWidth model + 25) * idx, 0)

viewRegBit : Model -> Int -> Html Msg
viewRegBit model idx =
    let (x0, y0) = regPos model
        mycolor = if getBit idx model.reg == 1 then "black" else "white"
    in rect [ x (toString x0)
               , y (toString (y0 + idx * bitHeight model))
               , fill mycolor
               , rx (toString (bitWidth model // 8))
               , ry (toString (bitHeight model // 8))
               , width (toString (bitWidth model))
               , height (toString (bitHeight model))
               ] []

range2d : Int -> Int -> List Loc2d
range2d a b = L.concat (L.map (\y -> L.map (\x -> (x, y)) (L.range 0 (a - 1))) (L.range 0 (b - 1)))

view : Model -> Html Msg
view model =
    let dim = modelDim model
    in svg [ viewBox "0 0 1000 650", width "1000px" ]
        [ rect [ x "0", y "0", width "100%", height "100%", fill "gray" ] []
        , g [ stroke "black" ] (L.map (viewMatCell model) (range2d dim dim))
        , g [ stroke "black" ] (L.map (viewRegBit model) (L.range 0 (dim - 1)))
        ]
