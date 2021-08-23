module UnaryOperations

import Types

public export
isZeroNum : NonRec -> Either NonRec String
isZeroNum (Natural Z) = Left truth
isZeroNum (Finite 0 _) = Left truth
isZeroNum x = Left falsehood

public export
liftNum : NonRec -> Either NonRec String
liftNum (Natural n) = Left (Natural n)
liftNum (Finite f _) = Left (Natural f)
liftNum _ = Right "Unimplemented"

public export
not : NonRec -> Either NonRec String
not (Finite 1 2) = Left $ Finite 0 2
not (Finite 0 2) = Left $ Finite 1 2
not _ = Right "Not on wrong args"
