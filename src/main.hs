import Data.Complex (Complex((:+)), magnitude)
import System.Random (randomRIO)

-- Define the Qudit data type
data Qudit = Qudit { amplitudes :: [Complex Double] }
    deriving (Show)

-- Function to create a Qudit with validation
createQudit :: [Complex Double] -> Either String Qudit
createQudit amps
    | abs (totalProb - 1) < 1e-10 = Right (Qudit amps)
    | otherwise = Left $ "Qudit outcome probabilities do not sum to 1: " ++ show totalProb
  where
    totalProb = sum (map magnitudeSquared amps)
    magnitudeSquared x = magnitude x ** 2

-- Measure function
measureQudit :: Qudit -> Double -> Int
measureQudit (Qudit amps) obs = measureHelper amps obs 0 0
  where
    measureHelper [] _ _ _ = error "Empty amplitude list"
    measureHelper (amp:amps') obs cumulativeProb idx
        | obs < cumulativeProb + prob = idx
        | otherwise = measureHelper amps' obs (cumulativeProb + prob) (idx + 1)
      where
        prob = magnitude amp ** 2

-- Function to reset amplitudes
resetAmplitudes :: [Complex Double] -> Either String Qudit
resetAmplitudes = createQudit

-- Function to reset register (clear amplitudes)
resetRegister :: Qudit -> Qudit
resetRegister _ = Qudit []

-- Example usage
main :: IO ()
main = do
    -- Attempt to create a Qudit with normalized amplitudes
    let result = createQudit [0.6 :+ 0, 0.8 :+ 0]
    case result of
        Left errMsg -> putStrLn errMsg
        Right qudit -> do
            putStrLn $ "Created qudit: " ++ show qudit

            -- Simulate measurement with a random observation
            obs <- randomRIO (0.0, 1.0)
            let measurementResult = measureQudit qudit obs
            putStrLn $ "Measurement result: " ++ show measurementResult

            -- Reset amplitudes
            let newQudit = resetRegister qudit
            putStrLn $ "After reset: " ++ show newQudit
