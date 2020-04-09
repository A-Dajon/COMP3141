module TortoiseCombinators
       ( andThen
       , loop
       , invisibly
       , retrace
       , overlay
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen (Move someDistance i1) i2 = Move someDistance $ andThen i1 i2
andThen (Turn someAngle i1) i2 = Turn someAngle $ andThen i1 i2
andThen (SetStyle someLineStyle i1) i2 = SetStyle someLineStyle $ andThen i1 i2
andThen (SetColour someColour i1) i2 =  SetColour someColour $ andThen i1 i2
andThen (PenDown i1) i2 = PenDown $ andThen i1 i2
andThen (PenUp i1) i2 = PenUp $ andThen i1 i2
andThen i1 Stop = i1
andThen Stop i1 = i1

loop :: Int -> Instructions -> Instructions
loop 0 i = Stop
loop n i
    | n < 0 = Stop
    | n > 0 = andThen i (loop (n - 1) i)

invisibilityHelp :: Bool -> Instructions -> Instructions
invisibilityHelp penIsDown (Move someDistance i) = PenUp (Move someDistance $ invisibilityHelp penIsDown i)
invisibilityHelp penIsDown (Turn someAngle i) = PenUp (Turn someAngle $ invisibilityHelp penIsDown i)
invisibilityHelp penIsDown (SetStyle someLineStyle i) = PenUp (SetStyle someLineStyle $ invisibilityHelp penIsDown i)
invisibilityHelp penIsDown (SetColour someColour i) = PenUp (SetColour someColour $ invisibilityHelp penIsDown i)
invisibilityHelp penIsDown (PenDown i) = PenUp $ invisibilityHelp True i
invisibilityHelp penIsDown (PenUp i) = PenUp $ invisibilityHelp False i
invisibilityHelp True Stop = PenDown Stop
invisibilityHelp False Stop = Stop

invisibly :: Instructions -> Instructions
invisibly (Move someDistance i) = invisibilityHelp True (Move someDistance i)
invisibly (Turn someAngle i) = invisibilityHelp True (Turn someAngle i)
invisibly (SetStyle someLineStyle i) = invisibilityHelp True (SetStyle someLineStyle i)
invisibly (SetColour someColour i) = invisibilityHelp True (SetColour someColour i)
invisibly (PenDown i) = invisibilityHelp True (PenDown i)
invisibly (PenUp i) = invisibilityHelp True (PenUp i)
invisibly i = invisibilityHelp True i

retraceHelp :: Instructions -> Instructions -> LineStyle -> Colour -> Bool -> Instructions
retraceHelp (Move someDistance i1) i2 someLineStyle someColor someBoolean = retraceHelp i1 (Move (-someDistance) i2) someLineStyle someColor someBoolean
retraceHelp (Turn someAngle i1) i2 someLineStyle someColor someBoolean = retraceHelp i1 (Turn (-someAngle) i2) someLineStyle someColor someBoolean
retraceHelp (SetStyle newLineStyle i1) i2 someLineStyle someColor someBoolean = retraceHelp i1 (SetStyle someLineStyle i2) newLineStyle someColor someBoolean
retraceHelp (SetColour newColour i1) i2 someLineStyle someColor someBoolean = retraceHelp i1 (SetColour someColor i2) someLineStyle newColour someBoolean
retraceHelp (PenUp i1) i2 someLineStyle someColor True = retraceHelp i1 (PenDown i2) someLineStyle someColor False
retraceHelp (PenUp i1) i2 someLineStyle someColor False = retraceHelp i1 (PenUp i2) someLineStyle someColor False
retraceHelp (PenDown i1) i2 someLineStyle someColor True = retraceHelp i1 (PenDown i2) someLineStyle someColor True
retraceHelp (PenDown i1) i2 someLineStyle someColor False = retraceHelp i1 (PenUp i2) someLineStyle someColor True
retraceHelp Stop i2 someLineStyle someColor someBoolean = i2

retrace :: Instructions -> Instructions
retrace (Move someDistance i) = retraceHelp (Move someDistance i) Stop (Solid 1) white True
retrace (Turn someAngle i) = retraceHelp (Turn someAngle i) Stop (Solid 1) white True
retrace (SetStyle newLineStyle i) = retraceHelp (SetStyle newLineStyle i) Stop (Solid 1) white True
retrace (SetColour newColour i) = retraceHelp (SetColour newColour i) Stop (Solid 1) white True
retrace (PenUp i) = retraceHelp (PenUp i) Stop (Solid 1) white True
retrace (PenDown i) = retraceHelp (PenDown i) Stop (Solid 1) white True
retrace Stop = Stop

overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay [i] = andThen i (retrace(invisibly i))
overlay (i : is) = andThen i (andThen (retrace(invisibly i)) (overlay is))

