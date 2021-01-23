module Rendering.Concatenations where
import Rendering.InfoTypes
import Rendering.VectorSubRange


data Concatenation = JustInfo Information

                -- Use this when you want to concatenate all bits in everything:
            |   ConcatWhole {
                  dataType              :: DataType
                , width                 :: Width
                , infoToConcat          :: [Information]
                , comments              :: [String]
                }

                -- Use this when 1 or more 
            |   ConcatSubRange {
                  dataType              :: DataType
                , width                 :: Width
                , thing2Subrange        :: Information
                , theSubrange           :: VectorSubRange
                , concatWith            :: [Concatenation]
                , subrangesToConcat     :: [VectorSubRange]
                , comments              :: [String]
                }

            |   ZeroPadLeft {
                  dataType              :: DataType
                , width                 :: Width
                , thing2Pad             :: Information
                , comments              :: [String]
                }            

            |   OnePadLeft {
                  dataType              :: DataType
                , width                 :: Width
                , thing2Pad             :: Information
                , comments              :: [String]
                }                

            |   ZeroPadRight {
                  dataType              :: DataType
                , width                 :: Width
                , thing2Pad             :: Information
                , comments              :: [String]
                }

            |   OnePadRight {
                  dataType              :: DataType
                , width                 :: Width
                , thing2Pad             :: Information
                , comments              :: [String]
                }

            |   PadLeftPrevBits {
                  dataType              :: DataType
                , width                 :: Width
                , thing2Pad             :: Information
                , subrange2Keep         :: VectorSubRange
                , comments              :: [String]
                }

            |   PadRightPrevBits {
                  dataType              :: DataType
                , width                 :: Width
                , thing2Pad             :: Information
                , subrange2Keep         :: VectorSubRange
                , comments              :: [String]
                }

                deriving (Eq, Show)


