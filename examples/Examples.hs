module Examples where

import TuringInternal
import Turing

import Data.Char ( toLower )

xr :: Rule
xr = createRule 0 'x' 'y' 1 MoveRight   

xrs :: [Rule]
xrs = [Rule {currentState = 0
                        , readTape = TC 'x'
                        , writeTape = Blank
                        , newState = 1
                        , direction = MoveRight
                       },
         Rule {currentState = 0
                        , readTape = Blank
                        , writeTape = TC 'n'
                        , newState = -1
                        , direction = MoveRight
                       },
         Rule {currentState = 1
                        , readTape = TC 'x'
                        , writeTape = Blank
                        , newState = 0
                        , direction = MoveRight
                       },
         Rule {currentState = 1
                        , readTape = Blank
                        , writeTape = TC 'y'
                        , newState = -1
                        , direction = MoveRight
                       }
       ]


xm :: Machine
xm = createMachine ['x', 'x', 'x']

xrs2 :: [Rule]
xrs2 = (createReplaceBlock 0 'x' 6 7) ++ (createHaltBlock 6 'y')

xm2 :: Machine
xm2 = createMachine ['y', 'y', 'x', 'y', 'x']

xrs3 :: [Rule]
xrs3 = (createReplaceBlock 0 'x' 3 12) ++ 
       (createReplaceBlock 3 'y' 6 10) ++
       (createReplaceBlock 6 'z' 0 10) ++
       (createHaltBlock 10 'n') ++
       (createReplaceBlock 12 'y' 15 10) ++
       (createReplaceBlock 15 'z' 18 10) ++
       (createReplaceBlock 18 'y' 21 26) ++
       (createReplaceBlock 21 'z' 21 10) ++
       (createReplaceBlock 26 'z' 29 10) ++
       (createHaltBlock 29 'y')

xcm1 :: Machine
xcm1 = createMachine (map toLower "XYXXYYZZ")
xcm2 :: Machine
xcm2 = createMachine (map toLower "ZZZYZXZY")
xcm3 :: Machine
xcm3 = createMachine (map toLower "XYZZ")
xcm4 :: Machine
xcm4 = createMachine (map toLower "ZYZXYZ")
xcm5 :: Machine
xcm5 = createMachine (map toLower "YZ")
xcm6 :: Machine
xcm6 = createMachine (map toLower "YZZ")
xcm7 :: Machine
xcm7 = createMachine (map toLower "Z")

