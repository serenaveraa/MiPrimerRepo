{-# OPTIONS_GHC -fno-warn-tabs #-}
module TestLab3 where

import Lab3             -- Nombre del archivo .hs de la solución
import Test.HUnit       -- Acerca de HUnit: https://hackage.haskell.org/package/HUnit
import Data.List

-- Para ejecutar todas las pruebas usar: runTestTT allTests 

tau1_tau_should_be_true  = TestCase (assertEqual "es tau1 Tau" True  (es tau1 Tau))
tau2_tau_should_be_true  = TestCase (assertEqual "es tau2 Tau" True  (es tau2 Tau))
tau3_tau_should_be_true  = TestCase (assertEqual "es tau3 Tau" True  (es tau3 Tau))
tau4_tau_should_be_true  = TestCase (assertEqual "es tau4 Tau" True  (es tau4 Tau))
con1_tau_should_be_false = TestCase (assertEqual "es con1 Tau" False (es con1 Tau))
con2_tau_should_be_false = TestCase (assertEqual "es con2 Tau" False (es con2 Tau))
con3_tau_should_be_false = TestCase (assertEqual "es con3 Tau" False (es con3 Tau))
con4_tau_should_be_false = TestCase (assertEqual "es con4 Tau" False (es con4 Tau))
con5_tau_should_be_false = TestCase (assertEqual "es con5 Tau" False (es con5 Tau))

con1_con_should_be_true  = TestCase (assertEqual "es con1 Contra" True  (es con1 Contra))
con2_con_should_be_true  = TestCase (assertEqual "es con2 Contra" True  (es con2 Contra))
con3_con_should_be_true  = TestCase (assertEqual "es con3 Contra" True  (es con3 Contra))
con4_con_should_be_true  = TestCase (assertEqual "es con4 Contra" True  (es con4 Contra))
con5_con_should_be_true  = TestCase (assertEqual "es con5 Contra" True  (es con5 Contra))
tau1_con_should_be_false = TestCase (assertEqual "es tau1 Contra" False (es tau1 Contra))
tau2_con_should_be_false = TestCase (assertEqual "es tau2 Contra" False (es tau2 Contra))
tau3_con_should_be_false = TestCase (assertEqual "es tau3 Contra" False (es tau3 Contra))
tau4_con_should_be_false = TestCase (assertEqual "es tau4 Contra" False (es tau4 Contra))

cnt1_cnt_should_be_true  = TestCase (assertEqual "es cnt1 Cont" True  (es cnt1 Cont))
cnt2_cnt_should_be_true  = TestCase (assertEqual "es cnt2 Cont" True  (es cnt2 Cont))
cnt3_cnt_should_be_true  = TestCase (assertEqual "es cnt3 Cont" True  (es cnt3 Cont))
tau1_cnt_should_be_false = TestCase (assertEqual "es tau1 Cont" False (es tau1 Cont))
con1_cnt_should_be_false = TestCase (assertEqual "es con1 Cont" False (es con1 Cont))

tau1_sat_should_be_true  = TestCase (assertEqual "es tau1 Sat" True  (es tau1 Sat))
tau2_sat_should_be_true  = TestCase (assertEqual "es tau2 Sat" True  (es tau2 Sat))
con1_sat_should_be_false = TestCase (assertEqual "es con1 Sat" False (es con1 Sat))
con2_sat_should_be_false = TestCase (assertEqual "es con2 Sat" False (es con2 Sat))

tau1_fal_should_be_false = TestCase (assertEqual "es tau1 Fal" False (es tau1 Fal))
tau2_fal_should_be_false = TestCase (assertEqual "es tau2 Fal" False (es tau2 Fal))
con1_fal_should_be_true  = TestCase (assertEqual "es con1 Fal" True  (es con1 Fal))
con2_fal_should_be_true  = TestCase (assertEqual "es con2 Fal" True  (es con2 Fal))

tau1_satM_test = TestCase (assertEqual "satM tau1" True (b && eval (creari m) tau1))  where (b,m) = satM tau1
tau2_satM_test = TestCase (assertEqual "satM tau2" True (b && eval (creari m) tau2))  where (b,m) = satM tau2
tau3_satM_test = TestCase (assertEqual "satM tau3" True (b && eval (creari m) tau3))  where (b,m) = satM tau3
cnt1_satM_test = TestCase (assertEqual "satM cnt1" True (b && eval (creari m) cnt1))  where (b,m) = satM cnt1
cnt2_satM_test = TestCase (assertEqual "satM cnt2" True (b && eval (creari m) cnt2))  where (b,m) = satM cnt2
cnt3_satM_test = TestCase (assertEqual "satM cnt3" True (b && eval (creari m) cnt3))  where (b,m) = satM cnt3

val1_val_should_be_true  = TestCase (assertEqual "validez val1" True  (fst $ validez val1))
val2_val_should_be_true  = TestCase (assertEqual "validez val2" True  (fst $ validez val2))
val3_val_should_be_true  = TestCase (assertEqual "validez val3" True  (fst $ validez val3))
val4_val_should_be_true  = TestCase (assertEqual "validez val4" True  (fst $ validez val4))
val5_val_should_be_true  = TestCase (assertEqual "validez val5" True  (fst $ validez val5))
val6_val_should_be_true  = TestCase (assertEqual "validez val6" True  (fst $ validez val6))
val7_val_should_be_true  = TestCase (assertEqual "validez val7" True  (fst $ validez val7))
val8_val_should_be_true  = TestCase (assertEqual "validez val8" True  (fst $ validez val8))
val9_val_should_be_true  = TestCase (assertEqual "validez val9" True  (fst $ validez val9))
val10_val_should_be_true = TestCase (assertEqual "validez val10" True (fst $ validez val10))

inv1_val_should_be_false = TestCase (assertEqual "validez inv1" False (fst $ validez inv1))
inv2_val_should_be_false = TestCase (assertEqual "validez inv2" False (fst $ validez inv2))
inv3_val_should_be_false = TestCase (assertEqual "validez inv3" False (fst $ validez inv3))
inv4_val_should_be_false = TestCase (assertEqual "validez inv4" False (fst $ validez inv4))
inv5_val_should_be_false = TestCase (assertEqual "validez inv5" False (fst $ validez inv5))
inv6_val_should_be_false = TestCase (assertEqual "validez inv6" False (fst $ validez inv6))
inv7_val_should_be_false = TestCase (assertEqual "validez inv7" False (fst $ validez inv7))

tau1_satC_test = TestCase (assertEqual "satC tau1" 2 (satC tau1))
tau2_satC_test = TestCase (assertEqual "satC tau2" 4 (satC tau2))
tau3_satC_test = TestCase (assertEqual "satC tau3" 2 (satC tau3))
cnt1_satC_test = TestCase (assertEqual "satC cnt1" 3 (satC cnt1))
cnt2_satC_test = TestCase (assertEqual "satC cnt2" 6 (satC cnt2))
cnt3_satC_test = TestCase (assertEqual "satC cnt3" 1 (satC cnt3))
con1_satC_test = TestCase (assertEqual "satC con1" 0 (satC con1))
con2_satC_test = TestCase (assertEqual "satC con2" 0 (satC con2))
con3_satC_test = TestCase (assertEqual "satC con3" 0 (satC con3))

f1_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f1) f1"  True (isFNDof (fnd f1) f1))
f2_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f2) f2"  True (isFNDof (fnd f2) f2))
f3_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f3) f3"  True (isFNDof (fnd f3) f3))
f4_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f4) f4"  True (isFNDof (fnd f4) f4))
f5_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f5) f5"  True (isFNDof (fnd f5) f5))
f6_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f6) f6"  True (isFNDof (fnd f6) f6))
f7_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f7) f7"  True (isFNDof (fnd f7) f7))
f8_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f8) f8"  True (isFNDof (fnd f8) f8))
f9_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f9) f9"  True (isFNDof (fnd f9) f9))
f10_fnd_ok = TestCase (assertEqual "isFNDof (fnd f10) f10" True (isFNDof (fnd f10) f10))
f11_fnd_ok = TestCase (assertEqual "isFNDof (fnd f11) f11" True (isFNDof (fnd f11) f11))
f12_fnd_ok = TestCase (assertEqual "isFNDof (fnd f12) f12" True (isFNDof (fnd f12) f12))

tauTests = [TestLabel "Tautologia 1" tau1_tau_should_be_true, 
            TestLabel "Tautologia 2" tau2_tau_should_be_true,
            TestLabel "Tautologia 3" tau3_tau_should_be_true,
            TestLabel "Tautologia 4" tau4_tau_should_be_true,
            TestLabel "Tautologia 5" con1_tau_should_be_false,
            TestLabel "Tautologia 6" con2_tau_should_be_false,
            TestLabel "Tautologia 7" con3_tau_should_be_false,
            TestLabel "Tautologia 8" con4_tau_should_be_false,
            TestLabel "Tautologia 9" con5_tau_should_be_false]
          
conTests = [TestLabel "Contradiccion 1" con1_con_should_be_true,
            TestLabel "Contradiccion 2" con2_con_should_be_true,
            TestLabel "Contradiccion 3" con3_con_should_be_true,
            TestLabel "Contradiccion 4" con4_con_should_be_true,
            TestLabel "Contradiccion 5" con5_con_should_be_true,
            TestLabel "Contradiccion 6" tau1_con_should_be_false,
            TestLabel "Contradiccion 7" tau2_con_should_be_false,
            TestLabel "Contradiccion 8" tau3_con_should_be_false,
            TestLabel "Contradiccion 9" tau4_con_should_be_false]
            
cntTests = [TestLabel "Contingencia 1" cnt1_cnt_should_be_true,
            TestLabel "Contingencia 2" cnt2_cnt_should_be_true,
            TestLabel "Contingencia 3" cnt3_cnt_should_be_true,
            TestLabel "Contingencia 4" tau1_cnt_should_be_false,
            TestLabel "Contingencia 5" con1_cnt_should_be_false]            
            
satTests = [TestLabel "Sat 1" tau1_sat_should_be_true, 
            TestLabel "Sat 2" tau2_sat_should_be_true,
            TestLabel "Sat 3" con1_sat_should_be_false,
            TestLabel "Sat 4" con2_sat_should_be_false]    

falTests = [TestLabel "Fal 1" tau1_fal_should_be_false, 
            TestLabel "Fal 2" tau2_fal_should_be_false,
            TestLabel "Fal 3" con1_fal_should_be_true,
            TestLabel "Fal 4" con2_fal_should_be_true]             
      
satMTests = [TestLabel "SatM 1" tau1_satM_test,
             TestLabel "SatM 2" tau2_satM_test,
             TestLabel "SatM 3" tau3_satM_test,
             TestLabel "SatM 4" cnt1_satM_test,
             TestLabel "SatM 5" cnt2_satM_test,
             TestLabel "SatM 6" cnt3_satM_test]     

valTests = [TestLabel "Razonamiento valido 1" val1_val_should_be_true, 
            TestLabel "Razonamiento valido 2" val2_val_should_be_true,
            TestLabel "Razonamiento valido 3" val3_val_should_be_true,
            TestLabel "Razonamiento valido 4" val4_val_should_be_true,
            TestLabel "Razonamiento valido 5" val5_val_should_be_true,
            TestLabel "Razonamiento valido 6" val6_val_should_be_true,
            TestLabel "Razonamiento valido 7" val7_val_should_be_true,
            TestLabel "Razonamiento valido 8" val8_val_should_be_true,
            TestLabel "Razonamiento valido 9" val9_val_should_be_true,
            TestLabel "Razonamiento valido 10" val10_val_should_be_true,
            TestLabel "Razonamiento invalido 1" inv1_val_should_be_false, 
            TestLabel "Razonamiento invalido 2" inv2_val_should_be_false,
            TestLabel "Razonamiento invalido 3" inv3_val_should_be_false,
            TestLabel "Razonamiento invalido 4" inv4_val_should_be_false,
            TestLabel "Razonamiento invalido 5" inv5_val_should_be_false,
            TestLabel "Razonamiento invalido 6" inv6_val_should_be_false,
            TestLabel "Razonamiento invalido 7" inv7_val_should_be_false]                      

satCTests = [TestLabel "SatC 1" tau1_satC_test,
             TestLabel "SatC 2" tau2_satC_test,
             TestLabel "SatC 3" tau3_satC_test,
             TestLabel "SatC 4" cnt1_satC_test,
             TestLabel "SatC 5" cnt2_satC_test,
             TestLabel "SatC 6" cnt3_satC_test,
             TestLabel "SatC 7" con1_satC_test,
             TestLabel "SatC 8" con2_satC_test,
             TestLabel "SatC 9" con3_satC_test]

fndTests = [TestLabel "FND 1"  f1_fnd_ok,
            TestLabel "FND 2"  f2_fnd_ok,
            TestLabel "FND 3"  f3_fnd_ok,
            TestLabel "FND 4"  f4_fnd_ok,
            TestLabel "FND 5"  f5_fnd_ok,
            TestLabel "FND 6"  f6_fnd_ok,
            TestLabel "FND 7"  f7_fnd_ok,
            TestLabel "FND 8"  f8_fnd_ok,
            TestLabel "FND 9"  f9_fnd_ok,
            TestLabel "FND 10" f10_fnd_ok,
            TestLabel "FND 11" f11_fnd_ok,
            TestLabel "FND 12" f12_fnd_ok]

-- Todas las pruebas
allTests = TestList $ tauTests ++ conTests ++ cntTests ++ satTests ++ falTests 
                               ++ satMTests ++ valTests ++ satCTests ++ fndTests

-- Razonamiento válido    
val1 = ([], (V "q") `Imp` (V "q"))
val2 = ([((V "q") `And` (V "r")) `Imp` (V "p"), (Neg (V "q")) `Or` (V "r"), V "q"],  (V "p") `And` (V "q"))
val3 = ([(Neg (V "p")) `Or` (Neg (V "q")) `Or` (V "r"), V "p", V "q"],  V "r")
val4 = ([(V "q")], (V "q") `Iff` (V "q"))
val5 = ([(((V "p") `Or` (V "q")) `Imp` (V "r")),Neg (V "q")], (V "p") `Imp` (V "r"))
val6 = ([(V "p"), (Neg (V "p")) `Or` (V "q"), V "q"], (V "p") `Or` (V "q"))
val7 = ([((V "p") `Imp` (V "q")) `Imp` (V "r")], ((V "p") `And` (V "q")) `Imp` (V "r")) 
val8 = ([((V "p1" ) `And` (V "p2")) `Imp` ((V "p3") `And` ((V "p4")`Or`(V "p5")) ), Neg(V "p3")], (Neg(V "p1"))`Or`(Neg(V "p2")))
val9 = ([((V "p") `And` (V "q"))],(V "p") `Imp` (V "q"))
val10 = ([((V "p") `And` (V "q"))],(V "p") `And` (V "q"))

-- Razonamiento inválido  
inv1 = ([ ((V "p") `And` (V "q")) `Imp` (V "r"), V "p", V "q"], Neg (V "r") )
inv2 = ([ (V "p") `Imp` ((V "q") `And` (V "r")), Neg (V "p")], Neg (V "q") `And` Neg (V "r"))
inv3 = ([ (Neg (V "p")) `Or` ((V "q") `Or` (V "r")), (Neg (V "p")) `Or` (V "r"), (V "p") `Or` (Neg (V "q")) ], Neg (V "q") `And` Neg (V "r"))  
inv4 = ([((V "p") `Or` (V "q"))],(V "p") `Imp` (V "q"))
inv5 = ([((V "p") `Or` (V "q"))],(V "p") `And` (V "q"))
inv6 = ([((Neg (V "p")) `Imp` (Neg (V "q"))), ((V "r") `Imp` (V "q"))], ((V "p") `Imp` (V "r")))
inv7 = ([((Neg (V "p")) `Imp` (V "q")), ((V "q") `Imp` (Neg (V "r")))], ((V "r") `Or` (V "p")))

-- Conversión a FND
f1 = (V "p")
f2 = (Neg (V "p"))
f3 = (V "q") `Or` (V "q")
f4 = (V "q") `And` (V "q")
f5 = (V "q") `Imp` (V "q")
f6 = (V "q") `Iff` (V "q")
f7 = ((V "q") `And` (V "r")) `Or` (V "p")
f8 = (Neg (V "q")) `Or` (V "r")
f9 = (Neg (V "p")) `Or` (Neg (V "q")) `Or` (V "r")
f10 = (((V "p") `Imp` (V "q")) `Imp` (V "p")) `Imp` (V "p")
f11 = ((Neg (V "p")) `Imp` (Neg (V "q"))) `Imp` ((V "p") `Imp` (V "q"))
f12 = ((V "p1") `And` (V "p2")) `Or` ((V "p3") `And` (V "p4"))

-- Tautologias
tau1 = (V "p") `Imp` (V "p")
tau2 = (((V "p") `Imp` (V "q")) `Imp` (V "p")) `Imp` (V "p")
tau3 = (V "p") `Or` (Neg (V "p"))
tau4 = ((V "p") `And` (V "q")) `Or` (Neg((V "p") `And` (V "q")))

-- Contingencias
cnt1 = (V "p") `Imp` (V "q")
cnt2 = (((V "p") `Imp` (V "q")) `Imp` (V "r")) `Imp` (V "p")
cnt3 = (V "p") `And` (Neg (V "q"))

-- Contradicciones
con1 = (V "p") `And` (Neg (V "p"))
con2 = ((V "p")`Or`(Neg(V "p"))) `Imp` ((V "p")`And`(Neg(V "p"))) 
con3 = (((Neg(V "p"))`Or`(V "q"))`And`(V "r")) `And` (Neg((V "p")`Imp`(V "q")) `Or` Neg(V "r"))
con4 = ((V "p")`Imp`(V "q")) `And` ((V "p")`And`(Neg(V "q")))
con5 = ((V "p")`And`(V "q")) `And` ((Neg(V "p"))`Or`(Neg(V "q")))

-- Auxiliares sintácticos
isFND :: L -> Bool
isFND = isDisjunction 
  where 
  isDisjunction (a `Or` b) = isDisjunction a && isDisjunction b
  isDisjunction a          = isClause a

  isClause (a `And` b) = isClause a && isClause b
  isClause a           = isLiteral a  

  isLiteral (V p)       = True   
  isLiteral (Neg (V p)) = True
  isLiteral _           = False

-- Auxiliares semánticos
eval :: (Var -> Bool) -> L -> Bool
eval i (V p)     = i p
eval i (Neg a)   = not (eval i a)
eval i (a `And` b) = (eval i a) && (eval i b)
eval i (a `Or`  b) = (eval i a) || (eval i b)
eval i (a `Imp` b) = (eval i a) <= (eval i b) 
eval i (a `Iff` b) = (eval i a) == (eval i b)

creari :: [(Var, Bool)] -> (Var -> Bool)
creari []         v = error $ show v ++ " indefinida!"
creari ((a,b):ys) v = case v == a of
                        True  -> b
                        False -> creari ys v 

equiv :: L -> L -> Bool
equiv f1 f2 = all (\i -> eval (creari i) f1 == eval (creari i) f2) 
                  (ints $ nub (vars f1 ++ vars f2))
  where 
    ints :: [Var] -> [I]
    ints []     = [[]]
    ints (v:vs) = [[(v,b)] ++ xs | b <- [True,False], xs <- ints vs]

isFNDof :: L -> L -> Bool
isFNDof f1 f2 = isFND f1 && f1 `equiv` f2