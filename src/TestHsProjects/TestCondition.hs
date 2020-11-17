module TestHsProjects.TestCondition where
import Rendering.InfoTypes
import Rendering.Condition


-- Declare signals:
someTestSig1 = easyInSlv "francine_01" (Hard 8) []
someTestSig2 = easyInSlv "joe_47" (Hard 8) []
someTestSig3 = easyInSlv "harold_96" (Hard 8) []
someTestSig4 = easyInSl "jose_42" []
someTestSig5 = easyInSl "geraldo_87" []

someTestCondition1 = GreaterT someTestSig1 someTestSig2
someTestCondition2 = LesserT someTestSig2 someTestSig3
someTestCondition3 = EqualTo someTestSig1 someTestSig3 
someTestCondition4 = StdLogicHigh someTestSig4
someTestCondition5 = StdLogicLow someTestSig5

someTestCondition6 = Or [someTestCondition1, someTestCondition2, someTestCondition3, someTestCondition4, someTestCondition5]
someTestCondition7 = Nand [someTestCondition1, someTestCondition2, someTestCondition3, someTestCondition4, someTestCondition5]
