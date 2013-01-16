module Physics.Falling.Identification.SignedIndexGenerator
(
SignedIndexGenerator
, newGenerator
, generatePositiveId
, generateNegativeId
, recycle
)
where

data SignedIndexGenerator = SignedIndexGenerator {
                               availablePositiveIds   :: [Int]
                               , availableNegativeIds :: [Int]
                             }

newGenerator :: SignedIndexGenerator
newGenerator = SignedIndexGenerator {
                 availablePositiveIds = [ 1 .. ]
                 , availableNegativeIds = map negate [ 1 .. ]
               }

generatePositiveId :: SignedIndexGenerator -> (SignedIndexGenerator, Int)
generatePositiveId generator = let i:l = availablePositiveIds generator in
                               (generator { availablePositiveIds = l }, i)

generateNegativeId :: SignedIndexGenerator -> (SignedIndexGenerator, Int)
generateNegativeId generator = let i:l = availableNegativeIds generator in
                               (generator { availableNegativeIds = l }, i)

recycle :: Int -> SignedIndexGenerator -> SignedIndexGenerator
recycle i generator = if i < 0 then
                        generator { availableNegativeIds = i : availableNegativeIds generator }
                      else
                        generator { availablePositiveIds = i : availablePositiveIds generator }
