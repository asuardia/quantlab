----------------------------------------------------------------------------------------------------
-- Copyright 2005-2006 Rob Tougher, licensed under the GNU General Public License Version 2.
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
{-|
Implements the Black-Scholes formula for European options.
Supports non-dividend options, continuous dividend options, and
lumpy-dividend options. Also calculates the greeks (delta, gamma,
theta, vega, rho) for non-dividend and continuous-dividend options.
-}
----------------------------------------------------------------------------------------------------
module HaskellMath.BlackScholes where


import qualified HaskellMath.UnitTest
import qualified HaskellMath.Probability





----------------------------------------------------------------------------------------------------
{-| Calculates the 'd1' value for a European non-dividend option. For non-dividend options,
pass in zero for dividend.
-}
----------------------------------------------------------------------------------------------------
calcD1 :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ d1
calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			d1
	where
		d1 = (log (stockPrice/strikePrice) + (riskFreeRate - dividend + ((volatility^2)/2)) * yearsUntilExpiration) / (volatility * (sqrt yearsUntilExpiration))





----------------------------------------------------------------------------------------------------
{-| Calculates the 'd2' value for a European non-dividend option.
For non-dividend options, pass in zero for dividend.
-}
----------------------------------------------------------------------------------------------------
calcD2 :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ d2
calcD2 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			d2
	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		d2 = d1 - (volatility * (sqrt yearsUntilExpiration))





----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- First we'll deal with calls, both non-dividend and continuous-dividend.
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------




----------------------------------------------------------------------------------------------------
{-| Calculates the value of a European non-dividend call, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond, pg. 161.
-}
----------------------------------------------------------------------------------------------------
callValue :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ value
callValue stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			(HaskellMath.Probability.normsdist d1) * (e ** ((-dividend) * yearsUntilExpiration)) * stockPrice - ((e ** (-riskFreeRate * yearsUntilExpiration))) * strikePrice * (HaskellMath.Probability.normsdist d2)
	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		d2 = (calcD2 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1







----------------------------------------------------------------------------------------------------
{-| Calculates the delta of a European non-dividend call, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond by Neil Chriss, pg. 180.
-}
----------------------------------------------------------------------------------------------------
callDelta :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ delta
callDelta stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			(e ** ((-dividend) * yearsUntilExpiration)) * (HaskellMath.Probability.normsdist d1)
	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1



----------------------------------------------------------------------------------------------------
{-| Calculates the gamma of a European non-dividend call, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond by Neil Chriss, pg. 180.
-}
----------------------------------------------------------------------------------------------------
callGamma :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ gamma
callGamma stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			((HaskellMath.Probability.normalProbabilityDensity d1) * (e ** ((-dividend) * yearsUntilExpiration)) / (stockPrice * volatility * (sqrt yearsUntilExpiration)))
	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1





----------------------------------------------------------------------------------------------------
{-| Calculates the vega of a European non-dividend call, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond by Neil Chriss, pg. 180.
-}
----------------------------------------------------------------------------------------------------
callVega :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ vega
callVega stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			(stockPrice * (sqrt yearsUntilExpiration) * (HaskellMath.Probability.normalProbabilityDensity d1) * (e ** ((-dividend) * yearsUntilExpiration)))
	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1





----------------------------------------------------------------------------------------------------
{-| Calculates the theta of a European non-dividend call, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond by Neil Chriss, pg. 180.
-}
----------------------------------------------------------------------------------------------------
callTheta :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ theta
callTheta stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			(-expression1) - expression2 + expression3

	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		d2 = (calcD2 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1
		expression1 = (stockPrice * (HaskellMath.Probability.normalProbabilityDensity d1) * volatility * (e ** ((-dividend) * yearsUntilExpiration)) / (2 * (sqrt yearsUntilExpiration)))
		expression2 = (riskFreeRate * strikePrice * (e ** ((-riskFreeRate) * yearsUntilExpiration)) * (HaskellMath.Probability.normsdist d2))
		expression3 = (dividend * stockPrice * (HaskellMath.Probability.normsdist d1) * (e ** ((-dividend) * yearsUntilExpiration)))





----------------------------------------------------------------------------------------------------
{-| Calculates the rho of a European non-dividend call, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond by Neil Chriss, pg. 180.
-}
----------------------------------------------------------------------------------------------------
callRho :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ rho
callRho stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

		(strikePrice * yearsUntilExpiration * (e ** ((-riskFreeRate) * yearsUntilExpiration)) * (HaskellMath.Probability.normsdist d2))

	where
		d2 = (calcD2 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1







----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
-- Next we'll deal with puts, both non-dividend and continuous-dividend.
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------
{-| Calculates the value of a European non-dividend put, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond, by Neil Chriss, pg. 161.
-}
----------------------------------------------------------------------------------------------------
putValue :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ value
putValue stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			(-(HaskellMath.Probability.normsdist (-d1))) * (e ** ((-dividend) * yearsUntilExpiration)) * stockPrice + ((e ** (-riskFreeRate * yearsUntilExpiration))) * strikePrice * (HaskellMath.Probability.normsdist (-d2))
	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		d2 = (calcD2 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1







----------------------------------------------------------------------------------------------------
{-| Calculates the delta of a European non-dividend put, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond, by Neil Chriss, pg. 181.
-}
----------------------------------------------------------------------------------------------------
putDelta :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ delta
putDelta stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

			(e ** ((-dividend) * yearsUntilExpiration)) * ((HaskellMath.Probability.normsdist d1) - 1)
	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1




----------------------------------------------------------------------------------------------------
{-| Calculates the gamma of a European non-dividend put, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond, by Neil Chriss, pg. 181. Currently this is the same
as gamma for a continuous-dividend call.
-}
----------------------------------------------------------------------------------------------------
putGamma :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ delta
putGamma stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

	(callGamma stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)






----------------------------------------------------------------------------------------------------
{-| Calculates the vega of a European continuous-dividend put, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond, by Neil Chriss, pg. 181. Currently this is the same
as vega for a continuous-dividend call.
-}
----------------------------------------------------------------------------------------------------
putVega :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ delta
putVega stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

	(callVega stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)






----------------------------------------------------------------------------------------------------
{-| Calculates the theta of a European non-dividend put, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond, pg. 181.
-}
----------------------------------------------------------------------------------------------------
putTheta :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ theta
putTheta stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

		(-expression1) + expression2 - expression3

	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		d2 = (calcD2 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1
		expression1 = ((stockPrice) * (HaskellMath.Probability.normalProbabilityDensity d1) * volatility * (e ** ((-dividend) * yearsUntilExpiration)) / (2 * (sqrt yearsUntilExpiration)))
		expression2 = (riskFreeRate * strikePrice * (e ** ((-riskFreeRate) * yearsUntilExpiration)) * (HaskellMath.Probability.normsdist (-d2)))
		expression3 = dividend * stockPrice * (HaskellMath.Probability.normsdist (-d1)) * (e ** ((-dividend) * yearsUntilExpiration))






----------------------------------------------------------------------------------------------------
{-| Calculates the rho of a European non-dividend put, using the Black-Scholes formula.
Formula from Black-Scholes and Beyond, pg. 181.
-}
----------------------------------------------------------------------------------------------------
putRho :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> Double -- ^ dividend (continuous)
		-> Double -- ^ rho
putRho stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend =

		((-strikePrice) * yearsUntilExpiration * (e ** ((-riskFreeRate) * yearsUntilExpiration)) * (HaskellMath.Probability.normsdist (-d2)))

	where
		d1 = (calcD1 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		d2 = (calcD2 stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividend)
		e = exp 1



----------------------------------------------------------------------------------------------------
{-|
DividendScheduleItem is a user-defined type containing the following
fields: yearsUntilExDividend, and dividendValue.
-}
----------------------------------------------------------------------------------------------------
data DividendScheduleItem = DividendScheduleItem Double Double




----------------------------------------------------------------------------------------------------
{-|
Calculates the present value of future dividend payments. From Black Scholes and Beyond,
page 160.
-}
----------------------------------------------------------------------------------------------------
dividendPresentValue :: [DividendScheduleItem] -- ^ dividend schedule items
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ dividendPresentValue (return value)

dividendPresentValue items riskFreeRate =

		(sum values)
	where
		values = [(((exp 1) ** ((-riskFreeRate) * years)) * value) | (DividendScheduleItem years value) <- items]



----------------------------------------------------------------------------------------------------
{-|
Calculates the value of a lumpy-dividend call. From Black Scholes and Beyond, page 160.
-}
----------------------------------------------------------------------------------------------------
lumpyDividendCallValue :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> [DividendScheduleItem] -- ^ dividend schedule items
		-> Double -- ^ rho

lumpyDividendCallValue stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividendScheduleItems =

		(callValue adjustedPrice strikePrice riskFreeRate yearsUntilExpiration adjustedVolatility continuousDividend)
	where
		adjustedPrice = stockPrice - (dividendPresentValue dividendScheduleItems riskFreeRate)
		adjustedVolatility = (stockPrice / adjustedPrice) * volatility
		continuousDividend = 0


----------------------------------------------------------------------------------------------------
{-|
Calculates the value of a lumpy-dividend put. From Black Scholes and Beyond, page 160.
-}
----------------------------------------------------------------------------------------------------
lumpyDividendPutValue :: Double -- ^ stockPrice
		-> Double -- ^ strikePrice
		-> Double -- ^ riskFreeRate (continuously compounded)
		-> Double -- ^ yearsUntilExpiration
		-> Double -- ^ volatility (per year)
		-> [DividendScheduleItem] -- ^ dividend schedule items
		-> Double -- ^ rho

lumpyDividendPutValue stockPrice strikePrice riskFreeRate yearsUntilExpiration volatility dividendScheduleItems =

		(putValue adjustedPrice strikePrice riskFreeRate yearsUntilExpiration adjustedVolatility continuousDividend)
	where
		adjustedPrice = stockPrice - (dividendPresentValue dividendScheduleItems riskFreeRate)
		adjustedVolatility = (stockPrice / adjustedPrice) * volatility
		continuousDividend = 0








----------------------------------------------------------------------------------------------------
-- | Returns the unit test suite for this module.
----------------------------------------------------------------------------------------------------

unitTestSuite :: [HaskellMath.UnitTest.UnitTestResult]
unitTestSuite =

	testCallValue ++
		testCallDelta ++
		testCallGamma ++
		testCallVega ++
		testCallTheta ++
		testCallRho ++
		testPutValue ++
		testPutDelta ++
		testPutGamma ++
		testPutVega ++
		testPutTheta ++
		testPutRho ++
		testDividendPresentValue ++
		testLumpyDividendCallValue ++
		testLumpyDividendPutValue










testCallValue :: [HaskellMath.UnitTest.UnitTestResult]
testCallValue =
		[(HaskellMath.UnitTest.assertEquals functionName 8.591666390203436 (callValue 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 3.5240277018329884 (callValue 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 7.802582314269209 (callValue 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 6.756098258257438 (callValue 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 2.6078520543175934 (callValue 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.callValue"

testCallDelta :: [HaskellMath.UnitTest.UnitTestResult]
testCallDelta =
		[(HaskellMath.UnitTest.assertEquals functionName 0.6584856435982023 (callDelta 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.45229150747520186 (callDelta 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.8335430250566852 (callDelta 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.5652996952073301 (callDelta 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 0.36346264644613024 (callDelta 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.callDelta"



testCallGamma :: [HaskellMath.UnitTest.UnitTestResult]
testCallGamma =

		[(HaskellMath.UnitTest.assertEquals functionName 0.024468791497288662 (callGamma 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.03520766104457556 (callGamma 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.03006919555663521 (callGamma 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.025256035472851636 (callGamma 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 0.032697180706653284 (callGamma 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.callGamma"




testCallVega :: [HaskellMath.UnitTest.UnitTestResult]
testCallVega =
		[(HaskellMath.UnitTest.assertEquals functionName 36.703187245932995 (callVega 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 29.70646400636063 (callVega 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 12.954303733780876 (callVega 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 37.884053209277454 (callVega 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 27.588246221238705 (callVega 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.callVega"



testCallTheta :: [HaskellMath.UnitTest.UnitTestResult]
testCallTheta =
		[(HaskellMath.UnitTest.assertEquals functionName (-5.615583941925814) (callTheta 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-4.051854922005476) (callTheta 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-3.496088490706345) (callTheta 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-3.6340984681975974) (callTheta 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName (-2.7304382978376402) (callTheta 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.callTheta"

testCallRho :: [HaskellMath.UnitTest.UnitTestResult]
testCallRho =

		[(HaskellMath.UnitTest.assertEquals functionName (57.256897969616794) (callRho 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (30.397835358807153) (callRho 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (31.570766516403793) (callRho 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 49.77387126247557 (callRho 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 24.651846429142175 (callRho 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.callRho"

testPutValue :: [HaskellMath.UnitTest.UnitTestResult]
testPutValue =

		[(HaskellMath.UnitTest.assertEquals functionName 3.714608840274842 (putValue 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 5.74871945574138 (putValue 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.7837182767225226 (putValue 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 4.834487353478039 (putValue 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 7.0491287920878705 (putValue 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.putValue"



testPutDelta :: [HaskellMath.UnitTest.UnitTestResult]
testPutDelta =

		[(HaskellMath.UnitTest.assertEquals functionName (-0.3415143564017977) (putDelta 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-0.5477084925247981) (putDelta 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-0.1664569749433148) (putDelta 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-0.40514583834117796) (putDelta 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName (-0.6069828871023779) (putDelta 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.putDelta"


testPutGamma :: [HaskellMath.UnitTest.UnitTestResult]
testPutGamma =

		[(HaskellMath.UnitTest.assertEquals functionName 0.024468791497288662 (putGamma 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.03520766104457556 (putGamma 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.03006919555663521 (putGamma 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 0.025256035472851636 (putGamma 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 0.032697180706653284 (putGamma 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.putGamma"




testPutVega :: [HaskellMath.UnitTest.UnitTestResult]
testPutVega =

		[(HaskellMath.UnitTest.assertEquals functionName 36.703187245932995 (putVega 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 29.70646400636063 (putVega 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName 12.954303733780876 (putVega 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName 37.884053209277454 (putVega 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName 27.588246221238705 (putVega 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.putVega"



testPutTheta :: [HaskellMath.UnitTest.UnitTestResult]
testPutTheta =

		[(HaskellMath.UnitTest.assertEquals functionName (-0.8594368194222437) (putTheta 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (0.5816265832290273) (putTheta 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-0.8470316925836799) (putTheta 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-1.7892879463395512) (putTheta 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName (-0.2804592430872801) (putTheta 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.putTheta"


testPutRho :: [HaskellMath.UnitTest.UnitTestResult]
testPutRho =

		[(HaskellMath.UnitTest.assertEquals functionName (-37.86604448045461) (putRho 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-46.82685639510124) (putRho 75 82 0.06 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-8.056220107067354) (putRho 60 55 0.05 0.74794520547 0.16 0)),
		(HaskellMath.UnitTest.assertEquals functionName (-45.34907118759583) (putRho 100 100 0.05 1 0.15 0.03)),
		(HaskellMath.UnitTest.assertEquals functionName (-52.57284532476621) (putRho 75 82 0.06 1 0.15 0.03))]
	where
		functionName = "BlackScholes.putRho"



testDividendPresentValue :: [HaskellMath.UnitTest.UnitTestResult]
testDividendPresentValue =

		[(HaskellMath.UnitTest.assertEquals functionName (1.9408910670970163) (dividendPresentValue [(DividendScheduleItem 0.5 2.0)] 0.06)),
		(HaskellMath.UnitTest.assertEquals functionName (2.825293600752746) (dividendPresentValue [(DividendScheduleItem 1 3)] 0.06)),
		(HaskellMath.UnitTest.assertEquals functionName (4.766184667849762) (dividendPresentValue [(DividendScheduleItem 0.5 2.0), (DividendScheduleItem 1 3)] 0.06)),
		(HaskellMath.UnitTest.assertEquals functionName (0) (dividendPresentValue [] 0.06))]
	where
		functionName = "BlackScholes.dividendPresentValue"



testLumpyDividendCallValue :: [HaskellMath.UnitTest.UnitTestResult]
testLumpyDividendCallValue =

		[(HaskellMath.UnitTest.assertEquals functionName (lumpyDividendCallValue 100 100 0.05 1 0.15 []) (callValue 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName
			(lumpyDividendCallValue 100 100 0.05 1 0.15 [dsItem])
			(callValue (100 - (dividendPresentValue [dsItem] 0.05)) 100 0.05 1 ((100/(100 - (dividendPresentValue [dsItem] 0.05)))*0.15) 0))]

	where
		functionName = "BlackScholes.lumpyDividendCallValue"
		dsItem = (DividendScheduleItem 0.5 2.0)



testLumpyDividendPutValue :: [HaskellMath.UnitTest.UnitTestResult]
testLumpyDividendPutValue =

		[(HaskellMath.UnitTest.assertEquals functionName (lumpyDividendPutValue 100 100 0.05 1 0.15 []) (putValue 100 100 0.05 1 0.15 0)),
		(HaskellMath.UnitTest.assertEquals functionName
			(lumpyDividendPutValue 100 100 0.05 1 0.15 [dsItem])
			(putValue (100 - (dividendPresentValue [dsItem] 0.05)) 100 0.05 1 ((100/(100 - (dividendPresentValue [dsItem] 0.05)))*0.15) 0))]

	where
		functionName = "BlackScholes.lumpyDividendPutValue"
		dsItem = (DividendScheduleItem 0.5 2.0)



