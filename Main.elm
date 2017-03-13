module Main exposing (..)

import Array as A
import List as L

type alias Loc2d = (Int, Int)

type alias Reg = A.Array Int

-- Unchecked invariant: the matrix is square, i.e. A.length m ==
-- A.length (get i m) for any i, modulo Maybe shenanigans
type alias Mat = A.Array Reg

zeroReg : Int -> Reg
zeroReg n = A.repeat n 0

zeroMat : Int -> Mat
zeroMat n = A.repeat n (zeroReg n)

makeMat : (Reg -> Int -> Int -> Int) -> Reg -> Mat
makeMat f vector =
    let dim = A.length vector
    in A.map (\x -> A.initialize dim (f vector x))
        (A.fromList (L.range 0 (dim - 1)))

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

-- Functions to extend these things infinitely with zeros,
-- conceptually.  Shouldn't be necessary to tread outside established
-- locations, but just in case (and to satisfy the types).

getBit : Int -> Reg -> Int
getBit i r =
    case A.get i r of
        Nothing -> 0
        Just b -> b

getRow : Int -> Mat -> Reg
getRow i m =
    case A.get i m of
        Nothing -> zeroReg (A.length m)
        Just r -> r

getCell : Loc2d -> Mat -> Int
getCell (x, y) m = getBit y (getRow x m)
