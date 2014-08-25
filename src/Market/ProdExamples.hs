module Market.ProdExamples   
    ( 
     template1, template2, template3, template4
    ) where 
    
import Data.Time.Calendar        
import Market.FinantialConventions
import Market.Generators

 
--------------------------------------------------------------------------
template1 eD = SwapTemplate { 
                                prCapital   = 100000000,
                                prGenerator = euribor3mSwGen,
                                prStartDay  = addGregorianMonthsClip 1 eD,
                                prMat = 5,
                                legInfo1 = LegFixInfo {
                                                          lPayRec    = PAYER, 
                                                          lFixRate   = 0.02,
                                                          lModPay    = PayInArrears,
                                                          lCon       = (LIN, THIRTY360),
                                                          lDiscCurve = "EUR_CALMNY_DISC"
                                                      },
                                legInfo2 = LegFloatInfo {
                                                            lPayRec    = RECEIVER, 
                                                            lModFixing = FixUpFront,
                                                            lModPay    = PayInArrears,
                                                            lCon       = (LIN, ACT360),
                                                            lMargin    = 0.0,
                                                            lPayOff    = liborPO,
                                                            lModel     = forwardMO,
                                                            lEstCurve  = "EUR_FUTSWAP_3M",
                                                            lDiscCurve = "EUR_CALMNY_DISC"
                                                        } 
                            }    
--------------------------------------------------------------------------

template2 eD = SwapTemplate { 
                                prCapital   = 100000000,
                                prGenerator = euribor3mSwGen,
                                prStartDay  = addGregorianMonthsClip 1 eD,
                                prMat = 10,
                                legInfo1 = LegFixInfo {
                                                          lPayRec    = PAYER, 
                                                          lFixRate   = 0.05,
                                                          lModPay    = PayInArrears,
                                                          lCon       = (LIN, THIRTY360),
                                                          lDiscCurve = "EUR_CALMNY_DISC"
                                                      },
                                legInfo2 = LegFloatInfo {
                                                            lPayRec    = RECEIVER, 
                                                            lModFixing = FixUpFront,
                                                            lModPay    = PayInArrears,
                                                            lCon       = (LIN, ACT360),
                                                            lMargin    = 0.0,
                                                            lPayOff    = liborPO,
                                                            lModel     = forwardMO,
                                                            lEstCurve  = "EUR_FUTSWAP_3M",
                                                            lDiscCurve = "EUR_CALMNY_DISC"
                                                        } 
                            }       
--------------------------------------------------------------------------
template3 eD = SwapTemplate { 
                                prCapital   = 100000000,
                                prGenerator = euribor3mSwGen,
                                prStartDay  = addGregorianMonthsClip 1 eD,
                                prMat = 3,
                                legInfo1 = LegFixInfo {
                                                          lPayRec    = PAYER, 
                                                          lFixRate   = 0.01,
                                                          lModPay    = PayInArrears,
                                                          lCon       = (LIN, THIRTY360),
                                                          lDiscCurve = "EUR_CALMNY_DISC"
                                                      },
                                legInfo2 = LegFloatInfo {
                                                            lPayRec    = RECEIVER, 
                                                            lModFixing = FixUpFront,
                                                            lModPay    = PayInArrears,
                                                            lCon       = (LIN, ACT360),
                                                            lMargin    = 0.0,
                                                            lPayOff    = liborPO,
                                                            lModel     = forwardMO,
                                                            lEstCurve  = "EUR_FUTSWAP_3M",
                                                            lDiscCurve = "EUR_CALMNY_DISC"
                                                        } 
                            }                    
--------------------------------------------------------------------------
template4 eD = OptionTemplate { 
                                  prCapital   = 100000000,
                                  prGenerator = euribor3mOpGen,
                                  prStartDay  = addGregorianMonthsClip 1 eD,
                                  prMat = 3,
                                  prStrike = 0.03,
                                  legInfo1 = LegFloatInfo {
                                                              lPayRec    = RECEIVER, 
                                                              lModFixing = FixUpFront,
                                                              lModPay    = PayInArrears,
                                                              lCon       = (LIN, ACT360),
                                                              lMargin    = 0.0,
                                                              lPayOff    = capletPO 0.03,
                                                              lModel     = blackMO,
                                                              lEstCurve  = "EUR_FUTSWAP_3M",
                                                              lDiscCurve = "EUR_CALMNY_DISC"
                                                        } 
                              }                                                    
                            
                            
                            
                            
                            
                            
                            
                            
                            