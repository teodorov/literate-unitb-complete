module Document.Tests.GarbageCollector where

import Document.Tests.Suite

import Tests.UnitTest

test_case :: TestCase
test_case = test_cases 
    "Garbage collector" 
    [ POCase "test0: verify m0" case0 result0 
    , POCase "test1: verify m1" case1 result1 
    , POCase "test2: verify m2" case2 result2 ]

path0 :: FilePath
path0 = "Tests/garbage collector/main.tex"

case0 :: IO POResult
case0 = verify path0 0

result0 :: String
result0 = unlines
    [ "  o  m0/INIT/FIS/free"
    , "  o  m0/INIT/FIS/live"
    , "  o  m0/INIT/INV/m0:inv0"
    , "  o  m0/INIT/INV/m0:inv1"
    , "  o  m0/INIT/INV/m0:inv2"
    , "  o  m0/INIT/WD"
    , "  o  m0/INIT/WWD"
    , "  o  m0/INV/WD"
    , "  o  m0/alloc/FIS/free@prime"
    , "  o  m0/alloc/FIS/live@prime"
    , "  o  m0/alloc/INV/m0:inv0"
    , "  o  m0/alloc/INV/m0:inv1"
    , "  o  m0/alloc/INV/m0:inv2"
    , "  o  m0/alloc/WD/ACT/m0:act0"
    , "  o  m0/alloc/WD/ACT/m0:act1"
    , "  o  m0/alloc/WD/C_SCH"
    , "  o  m0/alloc/WD/F_SCH"
    , "  o  m0/alloc/WD/GRD"
    , "  o  m0/alloc/WWD"
    , "  o  m0/free/FIS/free@prime"
    , "  o  m0/free/FIS/live@prime"
    , "  o  m0/free/INV/m0:inv0"
    , "  o  m0/free/INV/m0:inv1"
    , "  o  m0/free/INV/m0:inv2"
    , "  o  m0/free/SCH/m0:grd0"
    , "  o  m0/free/WD/ACT/m0:act0"
    , "  o  m0/free/WD/ACT/m0:act1"
    , "  o  m0/free/WD/C_SCH"
    , "  o  m0/free/WD/F_SCH"
    , "  o  m0/free/WD/GRD"
    , "  o  m0/free/WWD"
    , "passed 31 / 31"
    ]

case1 :: IO POResult
case1 = verify path0 1

result1 :: String
result1 = unlines
    [ "  o  m1/INIT/FIS/free"
    , "  o  m1/INIT/FIS/live"
    , "  o  m1/INIT/WD"
    , "  o  m1/INIT/WWD"
    , "  o  m1/INV/WD"
    , "  o  m1/add/FIS/free@prime"
    , "  o  m1/add/FIS/live@prime"
    , "  o  m1/add/FIS/ptr@prime"
    , "  o  m1/add/SCH/p"
    , "  o  m1/add/SCH/q"
    , "  o  m1/add/WD/ACT/m1:act0"
    , "  o  m1/add/WD/C_SCH"
    , "  o  m1/add/WD/F_SCH"
    , "  o  m1/add/WD/GRD"
    , "  o  m1/add/WWD"
    , "  o  m1/alloc/FIS/free@prime"
    , "  o  m1/alloc/FIS/live@prime"
    , "  o  m1/alloc/FIS/ptr@prime"
    , "  o  m1/alloc/WD/C_SCH"
    , "  o  m1/alloc/WD/F_SCH"
    , "  o  m1/alloc/WD/GRD"
    , "  o  m1/alloc/WWD"
    , "  o  m1/delete/FIS/free@prime"
    , "  o  m1/delete/FIS/live@prime"
    , "  o  m1/delete/FIS/ptr@prime"
    , "  o  m1/delete/SCH/p"
    , "  o  m1/delete/WD/ACT/m1:act0"
    , "  o  m1/delete/WD/C_SCH"
    , "  o  m1/delete/WD/F_SCH"
    , "  o  m1/delete/WD/GRD"
    , "  o  m1/delete/WWD"
    , "  o  m1/free/C_SCH/weaken/m1:sch0"
    , "  o  m1/free/C_SCH/weaken/m1:sch1"
    , "  o  m1/free/FIS/free@prime"
    , "  o  m1/free/FIS/live@prime"
    , "  o  m1/free/FIS/ptr@prime"
    , "  o  m1/free/SCH/m0:grd0"
    , "  o  m1/free/WD/C_SCH"
    , "  o  m1/free/WD/F_SCH"
    , "  o  m1/free/WD/GRD"
    , "  o  m1/free/WWD"
    , "  o  m1/m1:prog0/PROG/WD/lhs"
    , "  o  m1/m1:prog0/PROG/WD/rhs"
    , "  o  m1/m1:prog0/REF/ensure/m1/SAF/WD/lhs"
    , "  o  m1/m1:prog0/REF/ensure/m1/SAF/WD/rhs"
    , "  o  m1/m1:prog0/REF/ensure/m1/TR/WD"
    , "  o  m1/m1:prog0/REF/ensure/m1/TR/WD/witness/p"
    , "  o  m1/m1:prog0/REF/ensure/m1/TR/WFIS/p/p@prime"
    , "  o  m1/m1:prog0/REF/ensure/m1/TR/free/EN"
    , "  o  m1/m1:prog0/REF/ensure/m1/TR/free/NEG"
    , " xxx m1/m1:prog0/REF/ensure/m1/add/SAF"
    , "  o  m1/m1:prog0/REF/ensure/m1/alloc/SAF"
    , "  o  m1/m1:prog0/REF/ensure/m1/delete/SAF"
    , "  o  m1/m1:prog0/REF/ensure/m1/free/SAF"
    , "passed 53 / 54"
    ]

case2 :: IO POResult
case2 = verify path0 2

result2 :: String
result2 = unlines
    [ "  o  m2/INIT/FIS/free"
    , "  o  m2/INIT/FIS/live"
    , "  o  m2/INIT/FIS/reach"
    , "  o  m2/INIT/INV/m2:inv0"
    , "  o  m2/INIT/WD"
    , "  o  m2/INIT/WWD"
    , "  o  m2/INV/WD"
    , "  o  m2/add/FIS/free@prime"
    , "  o  m2/add/FIS/live@prime"
    , "  o  m2/add/FIS/ptr@prime"
    , "  o  m2/add/FIS/reach@prime"
    , "  o  m2/add/INV/m2:inv0"
    , "  o  m2/add/WD/C_SCH"
    , "  o  m2/add/WD/F_SCH"
    , "  o  m2/add/WD/GRD"
    , "  o  m2/add/WWD"
    , "  o  m2/alloc/FIS/free@prime"
    , "  o  m2/alloc/FIS/live@prime"
    , "  o  m2/alloc/FIS/ptr@prime"
    , "  o  m2/alloc/FIS/reach@prime"
    , "  o  m2/alloc/INV/m2:inv0"
    , "  o  m2/alloc/WD/C_SCH"
    , "  o  m2/alloc/WD/F_SCH"
    , "  o  m2/alloc/WD/GRD"
    , "  o  m2/alloc/WWD"
    , "  o  m2/delete/FIS/free@prime"
    , "  o  m2/delete/FIS/live@prime"
    , "  o  m2/delete/FIS/ptr@prime"
    , "  o  m2/delete/FIS/reach@prime"
    , "  o  m2/delete/INV/m2:inv0"
    , "  o  m2/delete/WD/C_SCH"
    , "  o  m2/delete/WD/F_SCH"
    , "  o  m2/delete/WD/GRD"
    , "  o  m2/delete/WWD"
    , " xxx m2/free/C_SCH/weaken/m2:sch0"
    , " xxx m2/free/C_SCH/weaken/m2:sch1"
    , "  o  m2/free/FIS/free@prime"
    , "  o  m2/free/FIS/live@prime"
    , "  o  m2/free/FIS/ptr@prime"
    , "  o  m2/free/FIS/reach@prime"
    , "  o  m2/free/INV/m2:inv0"
    , "  o  m2/free/SCH/m0:grd0"
    , "  o  m2/free/WD/C_SCH"
    , "  o  m2/free/WD/F_SCH"
    , "  o  m2/free/WD/GRD"
    , "  o  m2/free/WWD"
    , "passed 44 / 46"
    ]
