module Main exposing (..)

import Bitwise
import List as L
import List.Extra as L
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
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

setAt : Int -> a -> List a -> List a
setAt pos val xs =
    case L.setAt pos val xs of
        Nothing -> xs
        Just xs1 -> xs1

toggleBit : Int -> Reg -> Reg
toggleBit i r = setAt i (Bitwise.xor 1 (getBit i r)) r

toggleCell : Loc2d -> Mat -> Mat
toggleCell (i, j) m =
    let row = getRow j m
        bit = getBit i row
    in setAt j (setAt i (Bitwise.xor 1 bit) row) m


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
                 if x - y == -1 then
                     getBit x diagonal
                 else if x - y == 0 || x - y == 1 then 1 else 0)


-- specific LFSRs and CASRs

-- these are in my memo
smallLFSR : Mat
smallLFSR = makeLFSR [0,0,1,0,1]
smallCASR : Mat
smallCASR = makeCASR [0,0,1,1,0]

-- these are from Hortensius et al.
myCASR53 : Mat
myCASR53 = makeCASR [1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,0,0,1,0,1,0,1]
myCASR47 : Mat
myCASR47 = makeCASR [0,0,1,1,1,0,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,1,1,1,0,0,0,0,0,1,1,0,1]
myBadCASR53 : Mat
myBadCASR53 = makeBadCASR [1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,0,0,1,0,1,0,1]
myBadCASR47 : Mat
myBadCASR47 = makeBadCASR [0,0,1,1,1,0,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,1,1,1,0,0,0,0,0,1,1,0,1]

-- These are from Zhang et al.
myCASRMin53 : Mat
myCASRMin53 = makeCASR [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
myCASRMin47 : Mat
myCASRMin47 = makeCASR [0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

-- these are from Xilinx
myLFSRMin53 : Mat
myLFSRMin53 = makeLFSR [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1]
myLFSRMin47 : Mat
myLFSRMin47 = makeLFSR [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1]

-- these are just some I made
myLFSR53 : Mat
myLFSR53 = makeLFSR [1,1,1,0,0,0,1,1,0,1,1,0,0,1,1,0,0,0,1,0,0,1,0,1,1,1,1,1,0,0,0,0,1,0,1,0,1,0,1,1,1,1,0,1,1,1,0,1,0,1,0,1,1]
myLFSR47 : Mat
myLFSR47 = makeLFSR [1,1,0,0,1,0,0,1,1,0,1,0,0,0,0,0,1,1,0,0,1,1,1,0,1,1,1,0,0,1,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1]

-- these are some registers to start or end with
topBitOnReg : Int -> Reg
topBitOnReg n = setAt 0 1 (L.repeat n 0)

bottomBitOnReg : Int -> Reg
bottomBitOnReg n = setAt (n - 1) 1 (L.repeat n 0)

middleBitOnReg : Int -> Reg
middleBitOnReg n = setAt (n // 2) 1 (L.repeat n 0)

alternatingReg : Int -> Reg
alternatingReg n =
    if n % 2 == 0 then
        L.interweave (L.repeat (n // 2) 0) (L.repeat (n // 2) 1)
    else 1 :: alternatingReg (n - 1)

invertReg : Reg -> Reg
invertReg r = L.map (\x -> Bitwise.xor 1 x) r

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

    , interval : Time
    , paused : Bool
    , height : Int
    }

modelDim : Model -> Int
modelDim model = L.length model.reg

init : (Model, Cmd Msg)
init =
    ( { matrix = smallLFSR
      , reg = middleBitOnReg 5
      , ghosts = []
      , interval = second
      , paused = True
      , height = 400
      }
    , Cmd.none
    )

-- UPDATE

type Msg = Tick
         | ToggleMatCell Loc2d
         | ToggleBit Int
         | PresetMat Int
         | PausePlay
         | SpeedUp
         | SlowDown

presets : Int -> (Mat, Int)
presets i =
    case i of
        0 -> (smallLFSR, 5)
        1 -> (myLFSR47, 47)
        2 -> (smallCASR, 5)
        3 -> (myCASR47, 47)
        4 -> (myBadCASR47, 47)
        otherwise -> (zeroMat 8, 8)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (case msg of
        Tick ->
         { model
             | reg = evolveReg model.matrix model.reg
             , ghosts = L.take (maxGhosts model)
               (model.reg :: model.ghosts)
         }
        ToggleMatCell pos ->
         { model
             | matrix = toggleCell pos model.matrix
         }
        ToggleBit pos ->
         { model
             | reg = toggleBit pos model.reg
         }
        PresetMat i ->
         let (newMat, n) = presets i in
         { model
             | matrix = newMat
             , reg = middleBitOnReg n
             , ghosts = []
         }
        PausePlay ->
         { model | paused = not model.paused }
        SpeedUp ->
         { model | interval = model.interval / 2 }
        SlowDown ->
         { model | interval = model.interval * 2 }
    , Cmd.none
    )


-- SUBSCRIPTIONS

const : b -> (a -> b)
const c = \x -> c

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none
    else Time.every model.interval (const Tick)

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
            , onClick (ToggleMatCell (i, j))
            ]
        []

viewColOverlay : Model -> Int -> Html Msg
viewColOverlay model i =
    let o = 0.2 * toFloat (case model.ghosts of
                               [] -> 0
                               g :: _ -> getBit i g)
        size = bitHeight model
        dim = modelDim model
    in rect [ x (toString (10 + i * size))
            , y "10"
            , width (toString size)
            , height (toString (size * dim))
            , fill "red"
            , opacity (toString o)
            ] []

-- displaying the registers

bitHeight : Model -> Int
bitHeight model = model.height // modelDim model

bitWidth : Model -> Int -- as in, graphically...
bitWidth model = clamp 10 100 (bitHeight model)

regPos : Model -> Loc2d
regPos model = (10 + model.height + 10, 10)

ghostSpacing : Model -> Int
ghostSpacing model = bitWidth model // 10

ghostPos : Model -> Int -> Loc2d
ghostPos model idx =
    let myWidth = bitWidth model
        spacing = ghostSpacing model
        ghostStart = add2d (regPos model)
                     (myWidth + spacing + myWidth + spacing, 0)
    in add2d ghostStart ((myWidth + spacing) * idx, 0)

maxGhosts : Model -> Int
maxGhosts model =
    let myWidth = bitWidth model
        spacing = ghostSpacing model
        (start, _) = ghostPos model 0
    in (990 - start) // (myWidth + spacing)

viewRegBit : Model -> Int -> Html Msg
viewRegBit model ib =
    let (x0, y0) = regPos model
        myWidth = bitWidth model
        myHeight = bitHeight model
        mycolor = if getBit ib model.reg == 1 then "black" else "white"
        viewArrow ib2 =
            let (xg, yg) = ghostPos model 0
            in line [ x1 (toString (x0 + myWidth))
                    , y1 (toString (y0 + myHeight * (2 * ib + 1) // 2))
                    , x2 (toString xg)
                    , y2 (toString (yg + myHeight * (2 * ib2 + 1) // 2))
                    , strokeWidth (toString (clamp 1 10 (myHeight // 10)))
                    , stroke "red"
                    ] []
    in g []
        [ rect [ x (toString x0)
               , y (toString (y0 + ib * bitHeight model))
               , fill mycolor
               -- , rx (toString (bitWidth model // 8))
               -- , ry (toString (bitHeight model // 8))
               , width (toString (bitWidth model))
               , height (toString (bitHeight model))
               , onClick (ToggleBit ib)
               ] []
        , g []
            (L.map viewArrow (L.filter
                                  (\i -> getBit i (getRow ib model.matrix) == 1)
                                  (L.range 0 (modelDim model))))
        ]

viewGhostBit : Model -> Int -> Int -> Html Msg
viewGhostBit model ig ib =
    let (x0, y0) = ghostPos model ig
        myWidth = bitWidth model
        myHeight = bitHeight model
        mycolor = if getBit ib (getRow ig model.ghosts) == 1 then "black" else "white"
    in rect [ x (toString x0)
            , y (toString (y0 + ib * bitHeight model))
            , fill mycolor
            -- , rx (toString (bitWidth model // 8))
            -- , ry (toString (bitHeight model // 8))
            , width (toString (bitWidth model))
            , height (toString (bitHeight model))
            ] []

viewGhost : Model -> Int -> Html Msg
viewGhost model ig =
    g [ stroke "black"
      -- , opacity (toString (1.0 - 0.5 * (toFloat ig) / (toFloat (maxGhosts model))))
      ]
        (L.map (viewGhostBit model ig) (L.range 0 (modelDim model - 1)))

range2d : Int -> Int -> List Loc2d
range2d a b = L.concat (L.map (\y -> L.map (\x -> (x, y)) (L.range 0 (a - 1))) (L.range 0 (b - 1)))

viewSeparator : Model -> Html Msg
viewSeparator model = line [ x1 "10"
                           , y1 (toString (10 + model.height + 10))
                           , x2 "990"
                           , y2 (toString (10 + model.height + 10))
                           , stroke "white"
                           , strokeWidth "5"
                           ] []

-- presetBtnSize : Model -> Int
-- presetBtnSize model =
--     let numBtns = 1
--     in clamp 0 30 (model.height // numBtns)

viewButton : Loc2d -> Msg -> Html Msg
viewButton pos msg =
    let (x0, y0) = pos
        size = 50
        radius = size // 2
        (xc, yc) = (x0 + radius, y0 + radius)
    in circle [ cx (toString xc)
              , cy (toString yc)
              , r (toString radius)
              , fill "#0B79CE"
              , onClick msg
              ] []

presetButtons : Model -> Html Msg
presetButtons model =
    let top = 10 + model.height + 10 + 10
    in g [ stroke "black" ]
        [ viewButton (10, top) (PresetMat 0)
        , viewButton (70, top) (PresetMat 1)
        , viewButton (130, top) (PresetMat 2)
        , viewButton (190, top) (PresetMat 3)
        , viewButton (250, top) (PresetMat 4)
        ]

view : Model -> Html Msg
view model =
    let dim = modelDim model
        btnTop = 10 + model.height + 10 + 10
    in svg [ viewBox "0 0 1000 650", width "1000px" ]
        [ rect [ x "0", y "0", width "100%", height "100%", fill "gray" ] []
        , g [ stroke "black" ] (L.map (viewMatCell model) (range2d dim dim))
        , g [ ] (L.map (viewColOverlay model) (L.range 0 (dim - 1)))
        , g [ stroke "black" ] (L.map (viewRegBit model) (L.range 0 (dim - 1)))
        , g [ stroke "black" ] (L.map (viewGhost model) (L.range 0 (L.length model.ghosts - 1)))
        , viewSeparator model
        , presetButtons model
        , rect [ x "760", y (toString btnTop), width "50", height "50", fill "green"
               , onClick Tick ] []
        , rect [ x "820", y (toString btnTop), width "50", height "50", fill "darkgray"
               , onClick PausePlay ] []
        , rect [ x "880", y (toString btnTop), width "50", height "50", fill "lightgreen"
               , onClick SpeedUp ] []
        , rect [ x "940", y (toString btnTop), width "50", height "50", fill "darkgreen"
               , onClick SlowDown ] []
        ]
