$TITLE Reporting base solution
$STITLE Turkish Agricultural CGE Model - Regional.
$STITLE Based on IFPRI Standard CGE modeling system, Version 1.01

$ONTEXT
* =============================================================================
* File      : 50report.gms
* Author    : Hasan Dudu (hasan@dudu.gen.tr)
* Version   : 1.0
* Date      : 15/08/2011 17:13:57
* Changed   : 13/12/2013 18:08:24
* Changed by: Hasan Dudu (hasan@dudu.gen.tr)
* Remarks   :
This file prepares reporting tables for the base solution

This file is based on the core model file for the IFPRI/TMD Standard
CGE Model, documented in:
Lofgren, Hans, Rebecca Lee Harris, and Sherman Robinson, with the
assistance of Moataz El-Said and Marcelle Thomas. 2002. A Standard
Computable General Equilibrium (CGE) Model in GAMS. Microcomputers in
Policy Research, Vol. 5. Washington, D.C.: IFPRI.
Copyright (c) 2002, International Food Policy Research Institute (IFPRI),
Washington, DC.
* Licence   : MIT Licence
Copyright (c) <2013> <Hasan Dudu>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


$OFFTEXT
*=============================================================================



$ONTEXT

This is an include file to MOD101.GMS that produces the following reports
on the economy based on data for the base solution (with the name of the
relevant parameter in parentheses):
1. Economic structure table (STRUCBASE)
2. Natinal GDP table (GDPBASE)
3. Regional GDP table
4. Macro SAM (MACROSAM)
4. Input shares, factor wages, and elasticities (FACTAB1 - 4)
5. Household incomes (HHDTAB1 - 2)

For STRUCBASE, note that both activity and commodity data is presented
under commodity headings.

$OFFTEXT

*- 1. Economic structure table (STRUCBASE)=============================

SETS

 STRROW  Rows (only used to control order in display)
 /
 TOTAL-1
 TAGR
 TNAGR
 TOTAL-2
 /

 STRCOL  Columns
 /
 VAshr       value-added share (%)
 PRDshr      production share (%)
 EMPshr      share in total employment (%)
 EXPshr      sector share in total exports (%)
 EXP-OUTshr  exports as share in sector output (%)
 IMPshr   sector share in total imports (%)
 IMP-DEMshr  imports as share of domestic demand (%)
/

 STRCOL2(STRCOL)  Columns for summation operation
/
 VAshr    value-added share (%)
 PRDshr   production share (%)
 EMPshr   share in total employment (%)
 EXPshr      sector share in total exports (%)
 IMPshr   sector share in total imports (%)
/
;

PARAMETERS
 STRUCBASE(R,*,STRCOL)  Economic structure in the base
 TRADEBASE(R,*,ROW,STRCOL)  Trade Structure in the base
 chk1(RR1,A)

 ;

SET
MAPAC2(RR,A,RR1,C);

MAPAC2(RR,A,RR1,C)$SAM(RR,A,RR1,C)=YES;

*-Note: For VAshr, PRDshr, and EMPshr, data scaled by share of activity
*-in output of commodity to avoid double-counting when there is not a
*-one-to-one mapping between activities and commodities.

 STRUCBASE(RR,C,'VAshr')
   = 100*SUM((RR1,A)$MAPAC2(RR1,A,RR,C),
   (PXAC.L(RR1,A,RR,C)*QXAC.L(RR1,A,RR,C)/SUM((RR2,C1), PXAC.L(RR1,A,RR2,C1)*QXAC.L(RR1,A,RR2,C1)))*
     PVA.L(RR1,A)*(1-SUM(RN,tva(RR1,A,RN)))*QVA.L(RR1,A)
                          /SUM((RR4,A1), PVA.L(RR4,A1)*(1-SUM(RN,tva(RR4,A1,RN)))*QVA.L(RR4,A1)) );

 STRUCBASE(RR,C,'PRDshr')
   = 100*SUM((RR1,A)$MAPAC2(RR1,A,RR,C),
        (PXAC.L(RR1,A,RR,C)*QXAC.L(RR1,A,RR,C)/SUM((RR2,C1), PXAC.L(RR1,A,RR2,C1)*QXAC.L(RR1,A,RR2,C1)))
                       *PA.L(RR1,A)*QA.L(RR1,A)/SUM((RR3,A1), PA.L(RR3,A1)*QA.L(RR3,A1)) );

 STRUCBASE(RR,C,'EMPshr')$SUM((RR1,F), FLAB(F))
   = 100*SUM((RR2,A)$MAPAC2(RR2,A,RR,C),
        (PXAC.L(RR2,A,RR,C)*QXAC.L(RR2,A,RR,C)/SUM((RR3,C1), PXAC.L(RR2,A,RR3,C1)*QXAC.L(RR2,A,RR3,C1)))
       *SUM((RR1,F)$FLAB(F), QF.L(RR1,F,RR2,A))/
                                  SUM((RR1,F,RR4,A1)$FLAB(F), QF.L(RR1,F,RR4,A1)) );

 TRADEBASE(RR,C,ROW,'EXPshr')
  = 100*pwe.l(RR,C,ROW)*QE.L(RR,C,ROW)/SUM((RR1,C1), pwe.l(RR1,C1,ROW)*QE.L(RR1,C1,ROW));


 TRADEBASE(RR,C,ROW,'IMPshr')
   = 100*pwm.l(ROW,RR,C)*QM.L(ROW,RR,C)/SUM((RR1,C1), pwm.l(ROW,RR1,C1)*QM.L(ROW,RR1,C1));


 TRADEBASE(RR,C,ROW,'EXP-OUTshr')$(PX.L(RR,C)*QX.L(RR,C))
  = 100*PE.L(RR,C,ROW)*QE.L(RR,C,ROW)/(PX.L(RR,C)*QX.L(RR,C));

 TRADEBASE(RR,'TOTAL-1',ROW,'EXP-OUTshr')$SUM((RR1,C1), PX.L(RR1,C1)*QX.L(RR1,C1))
 = 100*SUM(C, PE.L(RR,C,ROW)*QE.L(RR,C,ROW))/SUM((RR1,C1), PX.L(RR1,C1)*QX.L(RR1,C1)) ;

 TRADEBASE(RR,'TAGR',ROW,'EXP-OUTshr')$SUM(C$CAGR(C), PX.L(RR,C)*QX.L(RR,C))
   = 100*SUM(C$CAGR(C), PE.L(RR,C,ROW)*QE.L(RR,C,ROW))/SUM(C$CAGR(C), PX.L(RR,C)*QX.L(RR,C)) ;

 TRADEBASE(RR,'TNAGR',ROW,'EXP-OUTshr')$SUM(C$CNAGR(C), PX.L(RR,C)*QX.L(RR,C))
   = 100*SUM(C$CNAGR(C), PE.L(RR,C,ROW)*QE.L(RR,C,ROW))
                                   /SUM(C$CNAGR(C), PX.L(RR,C)*QX.L(RR,C));

 TRADEBASE(RR,C,ROW,'IMP-DEMshr')$(PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C))
   = 100*PM.L(ROW,RR,C)*QM.L(ROW,RR,C)/(PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C));

 TRADEBASE(RR,'TOTAL-1',ROW,'IMP-DEMshr')
   = 100*SUM(C, PM.L(ROW,RR,C)*QM.L(ROW,RR,C))
                            /SUM((RR1,C1), PQ.L(RR1,C1)*(1 - SUM(RN,tq(RR1,C1,RN)))*QQ.L(RR1,C1));

 TRADEBASE(RR,'TAGR',ROW,'IMP-DEMshr')$SUM(C$CAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C))
   = 100*SUM(C$CAGR(C), PM.L(ROW,RR,C)*QM.L(ROW,RR,C))
                        /SUM(C$CAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C));

 TRADEBASE(RR,'TNAGR',ROW,'IMP-DEMshr')$SUM(C$CNAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C))
   = 100*SUM(C$CNAGR(C), PM.L(ROW,RR,C)*QM.L(ROW,RR,C))
                       /SUM(C$CNAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C));


*-********************

 STRUCBASE(RR,C,'EXPshr')
  = SUM(ROW,100*pwe.l(RR,C,ROW)*QE.L(RR,C,ROW))/SUM((RR1,C1,ROW), pwe.l(RR1,C1,ROW)*QE.L(RR1,C1,ROW));


 STRUCBASE(RR,C,'IMPshr')
   = SUM(ROW,100*pwm.l(ROW,RR,C)*QM.L(ROW,RR,C))/SUM((RR1,C1,ROW), pwm.l(ROW,RR1,C1)*QM.L(ROW,RR1,C1));


 STRUCBASE(RR,C,'EXP-OUTshr')$(PX.L(RR,C)*QX.L(RR,C))
  = SUM(ROW,100*PE.L(RR,C,ROW)*QE.L(RR,C,ROW))/(PX.L(RR,C)*QX.L(RR,C));

 STRUCBASE(RR,'TOTAL-1','EXP-OUTshr')$SUM((RR1,C1), PX.L(RR1,C1)*QX.L(RR1,C1))
 = 100*SUM((C,ROW), PE.L(RR,C,ROW)*QE.L(RR,C,ROW))/SUM((RR1,C1), PX.L(RR1,C1)*QX.L(RR1,C1)) ;

 STRUCBASE(RR,'TAGR','EXP-OUTshr')$SUM(C$CAGR(C), PX.L(RR,C)*QX.L(RR,C))
   = 100*SUM((ROW,C)$CAGR(C), PE.L(RR,C,ROW)*QE.L(RR,C,ROW))/SUM(C$CAGR(C), PX.L(RR,C)*QX.L(RR,C)) ;

 STRUCBASE(RR,'TNAGR','EXP-OUTshr')$SUM(C$CNAGR(C), PX.L(RR,C)*QX.L(RR,C))
   = 100*SUM((ROW,C)$CNAGR(C), PE.L(RR,C,ROW)*QE.L(RR,C,ROW))
                                   /SUM(C$CNAGR(C), PX.L(RR,C)*QX.L(RR,C));

 STRUCBASE(RR,C,'IMP-DEMshr')$(PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C))
   = SUM(ROW,100*PM.L(ROW,RR,C)*QM.L(ROW,RR,C))/(PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C));

 STRUCBASE(RR,'TOTAL-1','IMP-DEMshr')
   = 100*SUM((ROW,C), PM.L(ROW,RR,C)*QM.L(ROW,RR,C))
                            /SUM((RR1,C1), PQ.L(RR1,C1)*(1 - SUM(RN,tQ(RR1,C1,RN)))*QQ.L(RR1,C1));

 STRUCBASE(RR,'TAGR','IMP-DEMshr')$SUM((ROW,C)$CAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C))
   = 100*SUM((ROW,C)$CAGR(C), PM.L(ROW,RR,C)*QM.L(ROW,RR,C))
                        /SUM(C$CAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C));

 STRUCBASE(RR,'TNAGR','IMP-DEMshr')$SUM(C$CNAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C))
   = 100*SUM((ROW,C)$CNAGR(C), PM.L(ROW,RR,C)*QM.L(ROW,RR,C))
                       /SUM(C$CNAGR(C), PQ.L(RR,C)*(1 - SUM(RN,tq(RR,C,RN)))*QQ.L(RR,C));





*-Summation=======================

 STRUCBASE(RR,'TOTAL-1',STRCOL2)  = SUM(C, STRUCBASE(RR,C,STRCOL2)) ;

 STRUCBASE(RR,'TAGR',STRCOL2) = SUM(C$CAGR(C), STRUCBASE(RR,C,STRCOL2));
 STRUCBASE(RR,'TNAGR',STRCOL2) = SUM(C$CNAGR(C), STRUCBASE(RR,C,STRCOL2));

 STRUCBASE(RR,'TOTAL-2',STRCOL2)
   = STRUCBASE(RR,'TAGR',STRCOL2) + STRUCBASE(RR,'TNAGR',STRCOL2) ;

 STRUCBASE(RR,'TOTAL-2','IMP-DEMshr') = STRUCBASE(RR,'TOTAL-1','IMP-DEMshr') ;

 STRUCBASE(RR,'TOTAL-2','EXP-OUTshr') = STRUCBASE(RR,'TOTAL-1','EXP-OUTshr') ;

*-=======

OPTION STRUCBASE:1;

*-2. GDP table (NATGDPBASE)===============================================

SETS

 IGDPX  Items for GDP and national accounts
  /
  ABSORP      absorption
  PRVCON      private consumption
  FIXINV      fixed investment
  DSTOCK      stock change
  GOVCON      government consumption
  EXPORTS  exports
  IMPORTS  imports


  GDPMP       GDP at market prices (alt. 1: spending)
  GDPMP2      GDP at market prices (alt. 2: value-added)
  NETITAX     net indirect taxes
  GDPFC2      GDP at factor cost
  /

 IGDPXX(IGDPX)  Items for GDPMP summation
  /
  PRVCON   private consumption
  FIXINV   fixed investment
  DSTOCK   stock change
  GOVCON   government consumption
  EXPORTS  exports
  IMPORTS  imports

  /

 KGDPX  second index in GDP tables
 /
 VALUE, PERC-GDP, PERC-TOT
 /

*-================================

PARAMETERS
 NATGDPBASE(IGDPX,KGDPX)   Aggregate national accounts summary
 GDPERR                 error if alt GDP definitions are not identical
 ;
*-================================

 NATGDPBASE('PRVCON','VALUE')
   =  SUM((RR,C,RR1,H), PQ.L(RR,C)*QH.L(RR,C,RR1,H))
      + SUM((RR2,A,RR,C,RR1,H), PXAC.L(RR2,A,RR,C)*QHA.L(RR2,A,RR,C,RR1,H));

 NATGDPBASE('FIXINV','VALUE')  = SUM((RR,C), PQ.L(RR,C)*QINV.L(RR,C));
 NATGDPBASE('DSTOCK','VALUE')   = SUM((RR,C), PQ.L(RR,C)*QDST(RR,C));
 NATGDPBASE('GOVCON','VALUE')  = SUM((RR,C,RN), PQ.L(RR,C)*QG.L(RR,C,RN));
 NATGDPBASE('EXPORTS','VALUE') = SUM((RR,C,ROW)$CE(RR,C), PWE.L(RR,C,ROW)*EXR.L(ROW)*QE.L(RR,C,ROW));
 NATGDPBASE('IMPORTS','VALUE') = -SUM((RR,C,ROW)$CM(RR,C), PWM.L(ROW,RR,C)*EXR.L(ROW)*QM.L(ROW,RR,C));
 NATGDPBASE('GDPMP','VALUE') = SUM(IGDPXX, NATGDPBASE(IGDPXX,'VALUE'));
 NATGDPBASE('ABSORP','VALUE')
  = NATGDPBASE('GDPMP','VALUE')
    - NATGDPBASE('IMPORTS','VALUE')- NATGDPBASE('EXPORTS','VALUE');

 NATGDPBASE('GDPFC2','VALUE') = SUM((RR,A), PVA.L(RR,A)*(1-SUM(RN,tva(RR,A,RN)))*QVA.L(RR,A));

 NATGDPBASE('NETITAX','VALUE') =
            SUM((RR,A,RN), ta(RR,A,RN)*PA.L(RR,A)*QA.L(RR,A))
          + SUM((RR,A,RN), tva(RR,A,RN)*PVA.L(RR,A)*QVA.L(RR,A))
          + SUM((RR,C,ROW,RN)$CM(RR,C), tm(RN,ROW,RR,C)*QM.L(ROW,RR,C)*PWM.L(ROW,RR,C)*EXR.L(ROW))
          + SUM((RR,C,ROW,RN)$CE(RR,C), te(RR,C,RN,ROW)*QE.L(RR,C,ROW)*PWE.L(RR,C,ROW)*EXR.L(ROW))
          + SUM((RR,C,RN), tq(RR,C,RN)*PQ.L(RR,C)*QQ.L(RR,C))  ;
 NATGDPBASE('GDPMP2','VALUE')
  = NATGDPBASE('GDPFC2','VALUE') + NATGDPBASE('NETITAX','VALUE');

 GDPERR$(ABS(NATGDPBASE('GDPMP2','VALUE') - NATGDPBASE('GDPMP','VALUE')) GT 0.00001)
  = 1/0;

*- NATGDPBASE('GDPMP2','VALUE') = 0;


 NATGDPBASE(IGDPX,'PERC-GDP')$NATGDPBASE('GDPMP','VALUE')
  = 100*NATGDPBASE(IGDPX,'VALUE')/NATGDPBASE('GDPMP','VALUE');

*-==================

OPTION NATGDPBASE:6;

*-3.Regional GDP (REGGDPBASE)) NEEDS TO BE DEVELOPED

SETS

 RIGDPX  Items for GDP and national accounts
  /
  ABSORP      absorption
  PRVCON      private consumption
  FIXINV      fixed investment
  DSTOCK      stock change
  GOVCON      government consumption
  EXPORTS     exports
  IMPORTS     imports

  GDPMP       GDP at market prices (alt. 1: spending)
  GDPMP2      GDP at market prices (alt. 2: value-added)
  NETITAX     net indirect taxes
  GDPFC2      GDP at factor cost
  /

 RIGDPXX(RIGDPX)  Items for GDPMP summation
  /
  PRVCON   private consumption
  FIXINV   fixed investment
  DSTOCK   stock change
  GOVCON   government consumption
  EXPORTS  exports
  IMPORTS  imports

  /

 RKGDPX  second index in GDP tables
 /
 VALUE, PERC-GDP, PERC-TOT
 /

*-================================

PARAMETERS
 REGGDPBASE(RR,RIGDPX,RKGDPX)   Aggregate national accounts summary
 REGGDPERR(R)                  error if alt GDP definitions are not identical
 ;
*-================================

 REGGDPBASE(RR,'PRVCON','VALUE')
   =  SUM((C,RR1,H), PQ.L(RR,C)*QH.L(RR,C,RR1,H))
      + SUM((RR2,A,C,RR1,H), PXAC.L(RR2,A,RR,C)*QHA.L(RR2,A,RR,C,RR1,H));

 REGGDPBASE(RR,'FIXINV','VALUE')  = SUM((C), PQ.L(RR,C)*QINV.L(RR,C));
 REGGDPBASE(RR,'DSTOCK','VALUE')   = SUM((C), PQ.L(RR,C)*QDST(RR,C));
 REGGDPBASE(RR,'GOVCON','VALUE')  = SUM((RN,C), PQ.L(RR,C)*QG.L(RR,C,RN));
 REGGDPBASE(RR,'EXPORTS','VALUE') = SUM((ROW,C)$CE(RR,C), PWE.L(RR,C,ROW)*EXR.L(ROW)*QE.L(RR,C,ROW));
 REGGDPBASE(RR,'IMPORTS','VALUE') = -SUM((ROW,C)$CM(RR,C), PWM.L(ROW,RR,C)*EXR.L(ROW)*QM.L(ROW,RR,C));
 REGGDPBASE(RR,'GDPMP','VALUE') = SUM(RIGDPXX, REGGDPBASE(RR,RIGDPXX,'VALUE'));
 REGGDPBASE(RR,'ABSORP','VALUE')
  = REGGDPBASE(RR,'GDPMP','VALUE')
    - REGGDPBASE(RR,'IMPORTS','VALUE')- REGGDPBASE(RR,'EXPORTS','VALUE');

 REGGDPBASE(RR,'GDPFC2','VALUE') = SUM((A), PVA.L(RR,A)*(1-SUM(RN,tva(RR,A,RN)))*QVA.L(RR,A));

 REGGDPBASE(RR,'NETITAX','VALUE') =
            SUM((A,RN), ta(RR,A,RN)*PA.L(RR,A)*QA.L(RR,A))
          + SUM((A,RN), tva(RR,A,RN)*PVA.L(RR,A)*QVA.L(RR,A))
          + SUM((C,ROW,RN)$CM(RR,C), tm(RN,ROW,RR,C)*QM.L(ROW,RR,C)*PWM.L(ROW,RR,C)*EXR.L(ROW))
          + SUM((C,ROW,RN)$CE(RR,C), te(RR,C,RN,ROW)*QE.L(RR,C,ROW)*PWE.L(RR,C,ROW)*EXR.L(ROW))
          + SUM((C,RN), tq(RR,C,RN)*PQ.L(RR,C)*QQ.L(RR,C))  ;
 REGGDPBASE(RR,'GDPMP2','VALUE')
  = REGGDPBASE(RR,'GDPFC2','VALUE') + REGGDPBASE(RR,'NETITAX','VALUE');

*- REGGDPERR(R)$(ABS(REGGDPBASE(RR,'GDPMP2','VALUE') - REGGDPBASE(RR,'GDPMP','VALUE')) GT 0.00001)
*-  = 1/0;

 REGGDPBASE(RR,RIGDPX,'PERC-GDP')$REGGDPBASE(RR,'GDPMP','VALUE')
  = 100*REGGDPBASE(RR,RIGDPX,'VALUE')/REGGDPBASE(RR,'GDPMP','VALUE');

*-==================

OPTION REGGDPBASE:6;


*-4. Macro SAM (MACSAM)===============================================


SET
 ACMAC     macrosam accounts

 /
 ACT2        Activities
 COM2        Commodities
 FAC2        Factors
 HOU2        Domestic non-gov institution (households + enterprises)
 GOV2        Current government
 ROW2        Rest of World
 S-I2        Savings investment account
 INSTAX2     direct taxes on domestic institutions
 FACTAX2     direct factor taxes
 IMPTAX2     import taxes
 EXPTAX2     export taxes
 VATAX2      value-added taxes
 ACTTAX2     indirect taxes on activity revenue
 COMTAX2     indirect taxes on commodity sales in domestic market
*-HD: 23/03/2008
*-add water charge
 WCH2        water charge
 TOTAL2      totals

 /

 ACMACNT(ACMAC)  all macrosam accounts except TOTAL2


ACMACTAX(ACMAC) tax accounts
 /
 INSTAX2     direct taxes on domestic institutions
 FACTAX2     direct factor taxes
 IMPTAX2     import taxes
 EXPTAX2     export taxes
 VATAX2      value-added taxes
 ACTTAX2     indirect taxes on activity revenue
 COMTAX2     indirect taxes on commodity sales in domestic market
 /

 ;

ACMACNT(ACMAC)    = YES;
ACMACNT('TOTAL2') = NO;

ALIAS(ACMAC,ACMACP),(ACMACNT,ACMACNTP);

PARAMETER
   MACROSAM(ACMAC,ACMACP)   Macro SAM
   BALCHK2(ACMAC)           total balance check
   CHK(RR,A)
   ;
CHK(RR,A)=PVA.L(RR,A)*QVA.L(RR,A);
*-Defining SAM cells==============

 MACROSAM('COM2','ACT2')    = SUM((RR,C,RR1,A), PQ.L(RR,C)*QINT.L(RR,C,RR1,A));
 MACROSAM('FAC2','ACT2')    = SUM((RR,A),PVA.L(RR,A)*QVA.L(RR,A))-SUM(RR,WPAY.L(RR));
 MACROSAM('VATAX2','ACT2')  = SUM((RR1,A,RN), tva(RR1,A,RN)*PVA.L(RR1,A)*QVA.L(RR1,A));
 MACROSAM('ACTTAX2','ACT2') = SUM((RR,A,RN), ta(RR,A,RN)*PA.L(RR,A)*QA.L(RR,A));
 MACROSAM('FACTAX2','FAC2')= SUM((RR,F), SUM(RN,tf(RR,F,RN))*YF.L(RR,F)) ;
 MACROSAM('COM2','COM2')    = SUM((RR,C), PQ.L(RR,C)*QT.L(RR,C));
 MACROSAM('ACT2','COM2')    = SUM((RR,C,RR1,A), QXAC.L(RR,A,RR1,C)*PXAC.L(RR,A,RR1,C));
 MACROSAM('ROW2','COM2')    = SUM((ROW),SUM((RR,C), pwm.L(ROW,RR,C)*QM.L(ROW,RR,C))*EXR.L(ROW));
 MACROSAM('COMTAX2','COM2') = SUM((RR,C,RN), tq(RR,C,RN)*PQ.L(RR,C)*QQ.L(RR,C));
 MACROSAM('IMPTAX2','COM2') = SUM(ROW,SUM((RR,C,RN), tm(RN,ROW,RR,C)*pwm.L(ROW,RR,C)*QM.L(ROW,RR,C))*EXR.L(ROW));
 MACROSAM('EXPTAX2','COM2') = SUM(ROW,SUM((RR,C,RN), te(RR,C,RN,ROW)*pwe.L(RR,C,ROW)*QE.L(RR,C,ROW))*EXR.L(ROW));
 MACROSAM('HOU2','FAC2')    = SUM((RR,INSDNG,RR1,F), YIF.L(RR,INSDNG,RR1,F));
 MACROSAM('HOU2','WCH2')     = SUM(RR,WPAY.L(RR));
*- MACROSAM('GOV2','FAC2')    = SUM((RN,RR,F), YIF.L(RN,'GOV',RR,F));
 MACROSAM('ROW2','FAC2')    = SUM((RR,F,RN,ROW),trnsfr(RN,ROW,RR,F)*EXR.L(ROW));
 MACROSAM('COM2','HOU2')    = SUM((RR,C,RR1,H), PQ.L(RR,C)*QH.L(RR,C,RR1,H));
 MACROSAM('ACT2','HOU2')    = SUM((RR,A,RR1,C,RR2,H), PXAC.L(RR,A,RR1,C)*QHA.L(RR,A,RR1,C,RR2,H));
 MACROSAM('INSTAX2','HOU2') = SUM((RR,INSDNG,RN), TINS.L(RR,INSDNG,RN)*YI.L(RR,INSDNG));
 MACROSAM('HOU2','HOU2')    = SUM((RR,INSDNG,RR1,INSDNG1), TRII.L(RR,INSDNG,RR1,INSDNG1));
 MACROSAM('S-I2','HOU2')    = SUM((RR,INSDNG), MPS.L(RR,INSDNG)*(1 - SUM(RN,TINS.L(RR,INSDNG,RN)))*YI.L(RR,INSDNG));
 MACROSAM('COM2','GOV2')    = SUM((RR,C,RN), PQ.L(RR,C)*QG.L(RR,C,RN));
 MACROSAM('HOU2','GOV2')    = SUM((RN,INSD,RR1), trnsfr(RR1,INSD,RN,'GOV'))*CPI.L;
 MACROSAM('S-I2','GOV2')    = GSAV.L ;
 MACROSAM('COM2','ROW2')    = SUM(ROW,SUM((RR,C), pwe.L(RR,C,ROW)*QE.L(RR,C,ROW))*EXR.L(ROW));
 MACROSAM('HOU2','ROW2')    = SUM((RR,INSDNG,RN,ROW), trnsfr(RR,INSDNG,RN,ROW)*EXR.L(ROW));
 MACROSAM('GOV2','ROW2')    = SUM((RN,ROW,RN1),trnsfr(RN,'GOV',RN1,ROW)*EXR.L(ROW));
 MACROSAM('WCH2','ACT2')    = SUM(RR,WPAY.L(RR));
 MACROSAM('S-I2','ROW2')    = SUM(ROW,FSAV.L(ROW)*EXR.L (ROW));
 MACROSAM('COM2','S-I2')    = SUM((RR,C), PQ.L(RR,C)*QINV.L(RR,C)) + SUM((RR,C), PQ.L(RR,C)*qdst(RR,C));
 MACROSAM('GOV2',ACMACTAX)  = SUM(ACMAC, MACROSAM(ACMACTAX,ACMAC));

*-Computing account totals========

 MACROSAM('TOTAL2', ACMAC) = 0;
 MACROSAM(ACMAC,'TOTAL2')   = 0;

 MACROSAM('TOTAL2', ACMACNTP) = SUM(ACMACNT,  MACROSAM(ACMACNT,ACMACNTP));
 MACROSAM(ACMACNT,'TOTAL2')   = SUM(ACMACNTP, MACROSAM(ACMACNT,ACMACNTP));

*-Checking account balances=======

 BALCHK2(ACMACNT) = MACROSAM('TOTAL2',ACMACNT) - MACROSAM(ACMACNT,'TOTAL2');

PARAMETER
 MACCHK  error message if Macro SAM does not balance
 ;

 MACCHK(ACMACNT)$(ABS(BALCHK2(ACMACNT)) GT 0.00001) = 1/0;
*-==========

OPTION MACROSAM:2;


*-5. Input shares, factor wages, and elasticities (FACTAB1 - 4)=======

*-The parameters FACTAB1, FACTAB2, FACTAB3, FACTAB4 are covered in this
*-section.


SETS
 FF(AC) all factor accounts and total ;

 FF(F)       = YES;
 FF('TOTAL') = YES;

PARAMETERS
 FACTAB1(RR,AC,RR1,AC1) Factor shares within sector
 FACTAB2(RR,AC,RR1,AC1) Factor shares across sectors
 FACTAB3(RR,AC,*)   Agg factor & intd. input shares & their sub. elasticity
 FACTAB4(RR,AC,RR1,AC1) Factor wages by sector
 ;

 FACTAB1(RR,A,RR1,F)       = WFR.L(RR,F)*wfdist.L(RR1,F,RR,A)*QF.L(RR1,F,RR,A) ;
 FACTAB1(RR,'TOTAL',RR1,F) = SUM((A), FACTAB1(RR,A,RR1,F)) ;
 FACTAB1(RR,A,RR1,'TOTAL') = SUM((F), FACTAB1(RR,A,RR1,F)) ;
 FACTAB1(RR,'TOTAL',RR1,'TOTAL') = SUM((F), FACTAB1(RR,'TOTAL',RR1,F)) ;
 FACTAB2(RR,AA,RR1,FF)     = FACTAB1(RR,AA,RR1,FF) ;
 FACTAB4(RR,AA,RR1,FF)     = FACTAB1(RR,AA,RR1,FF) ;

 FACTAB2(RR,AA,RR1,FF)$FACTAB2(RR,'TOTAL',RR1,FF) = 100*FACTAB2(RR,AA,RR1,FF)/FACTAB2(RR,'TOTAL',RR1,FF) ;
 FACTAB1(RR,AA,RR1,FF)$FACTAB1(RR,AA,RR1,'TOTAL') = 100*FACTAB1(RR,AA,RR1,FF)/FACTAB1(RR,AA,RR1,'TOTAL') ;

 FACTAB3(RR,A,'QVASHR')$PA.L(RR,A)   = 100*PVA.L(RR,A)*QVA.L(RR,A)/(PA.L(RR,A)*(1-SUM(RN,ta(RR,A,RN)))*QA.L(RR,A)) ;
 FACTAB3(RR,A,'QINTSHR')$PA.L(RR,A)  = 100*PINTA.L(RR,A)*QINTA.L(RR,A)/(PA.L(RR,A)*(1-SUM(RN,ta(RR,A,RN)))*QA.L(RR,A)) ;
 FACTAB3(RR,'TOTAL','QVASHR')$SUM(A,PA.L(RR,A))  = 100*SUM((A), PVA.L(RR,A)*QVA.L(RR,A)) /
                              SUM(A, PA.L(RR,A)*(1-SUM(RN,ta(RR,A,RN)))*QA.L(RR,A)) ;
 FACTAB3(RR,'TOTAL','QINTSHR')$SUM(A,PA.L(RR,A)) = 100*SUM(A, PINTA.L(RR,A)*QINTA.L(RR,A)) /
                              SUM(A, PA.L(RR,A)*(1-SUM(RN,ta(RR,A,RN)))*QA.L(RR,A)) ;
 FACTAB3(RR,A,'ACES')     = ACES(RR,A) ;
 FACTAB3(RR,A,'SIGMAQA')$ACES(RR,A) = PRODELAS2(RR,A) ;
 FACTAB3(RR,A,'SIGMAVA')  = PRODELAS(RR,A) ;

 FACTAB4(RR,AA,RR1,'TOTAL')    = 0 ;
 FACTAB4(RR,A,RR1,F)$QF.L(RR1,F,RR,A) = FACTAB4(RR,A,RR1,F)/QF.L(RR1,F,RR,A) ;
 FACTAB4(RR,'TOTAL',RR1,F)$SUM((A), QF.L(RR1,F,RR,A)) = FACTAB4(RR,'TOTAL',RR1,F)/SUM((A), QF.L(RR1,F,RR,A)) ;

*-6. Household incomes (HHDTAB1 - 2)==================================

PARAMETERS
 HHDTAB1(RR,AC,RR1,AC1) Share of item AC1 in the total income of hhd H (%)
 HHDTAB2(RR,AC,RR1,AC1) Share of hhd AC in total hhd income from item AC1 (%)
 ;

 HHDTAB1(RR,H,RR1,ACNT)$SAM(RR,H,RR1,ACNT)       = 100*SAM(RR,H,RR1,ACNT)/SUM((RR2,ACNT1), SAM(RR,H,RR2,ACNT1));
 HHDTAB1(RR,'TOTAL',RR1,ACNT) = 100*SUM((H), SAM(RR,H,RR1,ACNT))/(SUM((RR2,H1,RR3,ACNT1), SAM(RR2,H1,RR3,ACNT1)));
 HHDTAB1(RR,AC,RR1,'TOTAL')    = SUM((ACNT), HHDTAB1(RR,AC,RR1,ACNT));
 HHDTAB2(RR,H,RR1,ACNT)$SUM((RR2,H1), SAM(RR2,H1,RR1,ACNT)) = 100*SAM(RR,H,RR1,ACNT)/SUM((RR2,H1), SAM(RR2,H1,RR1,ACNT));
 HHDTAB2(RR,H,RR1,'TOTAL')                    = 100*SUM((ACNT), SAM(RR1,H,RR1,ACNT))/SUM((RR2,H1,RR3,ACNT), SAM(RR2,H1,RR3,ACNT));
 HHDTAB2(RR,'TOTAL',RR1,AC)                   = SUM((H), HHDTAB2(RR,H,RR1,AC));



*-#*#*#*#*# THE END OF REPBASE.INC #*#*#*#*
