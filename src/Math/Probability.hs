----------------------------------------------------------------------------------------------------
-- Copyright 2005-2006 Rob Tougher, licensed under the GNU General Public License Version 2.
----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
{-| Contains probability functions. -}
----------------------------------------------------------------------------------------------------
module Math.Probability where

--import qualified HaskellMath.UnitTest



----------------------------------------------------------------------------------------------------
{-| Approximation of cumulative normal distribution, from Black-Scholes and Beyond, by
Neil Chriss, pg. 89.
-}
----------------------------------------------------------------------------------------------------
normsdist :: (Floating a, Ord a) => a -> a
normsdist x =

		if x < 0 then
			1 - (normsdist (-x))
		else
			1 - (0.5 * (1 + (d1 * x) + (d2 * x^2) + (d3 * x^3) + (d4 * x^4) + (d5 * x^5) + (d6 * x^6))^^ (-16))

	where
		d1 = 0.0498673470
		d2 = 0.0211410061
		d3 = 0.0032776263
		d4 = 0.0000380036
		d5 = 0.0000488906
		d6 = 0.0000053830




----------------------------------------------------------------------------------------------------
{-| Calculates the normal probability density. From Black-Scholes and Beyond, pg. 181. -}
----------------------------------------------------------------------------------------------------
normalProbabilityDensity :: (Floating a) => a -> a
normalProbabilityDensity y =

		((e ** (0 - (y^^2)/2)) / (sqrt (2 * pi)))
	where
		e = exp 1




----------------------------------------------------------------------------------------------------
-- | Returns the unit test suite for the Probability module.
----------------------------------------------------------------------------------------------------
{-
unitTestSuite :: [HaskellMath.UnitTest.UnitTestResult]
unitTestSuite =

	testNormsdist ++
		testNormalProbabilityDensity




testNormsdist :: [HaskellMath.UnitTest.UnitTestResult]
testNormsdist =

		[(HaskellMath.UnitTest.assertEquals name 0.999663003240045 (normsdist 3.4)),
		(HaskellMath.UnitTest.assertEquals name 0.9087888529777782 (normsdist 1.333333)),
		(HaskellMath.UnitTest.assertEquals name 0.5 (normsdist 0)),
		(HaskellMath.UnitTest.assertEquals name 0.0005104815151352193 (normsdist (-3.2847))),
		(HaskellMath.UnitTest.assertEquals name 0 (normsdist (-10))),
		(HaskellMath.UnitTest.assertEquals name 1 (normsdist 10))]
	where
		name = "Probability.normsdist"


testNormalProbabilityDensity :: [HaskellMath.UnitTest.UnitTestResult]
testNormalProbabilityDensity =
		[(HaskellMath.UnitTest.assertEquals name 0.36703187245933 (normalProbabilityDensity 0.408333333333333)),
		(HaskellMath.UnitTest.assertEquals name 0.39608618675147506 (normalProbabilityDensity (-0.119874224852951)))]
	where
		name = "Probability.normalProbabilityDensity"
-}


