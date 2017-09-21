$TITLE Model
$STITLE Turkish Agricultural CGE Model - Regional.
$STITLE Based on IFPRI Standard CGE modeling system, Version 1.01
$ONTEXT
*=============================================================================
* File      : 40model.gms
* Author    : Hasan Dudu (hasan@dudu.gen.tr)
* Version   : 1.0
* Date      : 15/08/2011 17:13:57
* Changed   : 13/12/2013 18:08:24
* Changed by: Hasan Dudu (hasan@dudu.gen.tr)
* Remarks   :

This file is the main model file. It introduces nested production structure and
labor/leisure trade off to the standard IFPRI model with several improvements
for coding.

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

*--------------------------------------------------------------------------------
*-5. VARIABLE DECLARATIONS ------------------------------------------------------
*-------------------------------------------------------------------------------
*-This section only includes variables that appear in the model.
*-The variables are declared in alphabetical order.

VARIABLES
  CPI                                   consumer price index (PQ-based)
  DPI                                   index for domestic producer prices (PDS-based)
  DMPS                                  change in marginal propensity to save for selected inst
  DTINS                                 change in domestic institution tax share
  EG(RN)                                total current government expenditure
  EH(RR,H)                              household consumption expenditure
  EXR(ROW)                              exchange rate
  FSAV(ROW)                             foreign savings acc. to. trading regions
  TFSAV                                 Total foreign saving
  GADJ                                  government demand scaling factor
  GOVSHR                                govt consumption share of absorption
  GSAV                                  government savings
  IADJ                                  investment scaling factor (for fixed capital formation) in region R
  INVSHR                                investment share of absorption
  MPS(RR,INS)                           marginal propensity to save for dom non-gov inst ins
  MPSADJ                                savings rate scaling factor
  PA(RR,A)                              output price of activity a
  PDD(RR,C)                             demand price for comy. c produced & sold domestically
  PDS(RR,C)                             supply price for comy. c produced & sold domestically
  PE(RR,C,ROW)                          price of exports
  PINTA(RR,A)                           price of intermediate aggregate
  PM(ROW,RR,C)                          price of imports
  PQ(RR,C)                              price of composite good c
  PVA(RR,A)                             value added price
  PWE(RR,C,ROW)                         world price of exports
  PWM(ROW,RR,C)                         world price of imports
  PX(RR,C)                              average output price
  PXAC(RR,A,RR1,C)                      price of commodity c from activity a
  QA(RR,A)                              level of domestic activity
  QD(RR,C)                              quantity of domestic sales
  QE(RR,C,ROW)                          quantity of exports
  QF(RR,F,RR1,A)                        quantity demanded of factor f from activity a
  QFS(RR,F)                             quantity of factor supply
  QG(RR,C,RN)                           quantity of government consumption
  QH(RR,C,RR1,H)                        quantity consumed of marketed commodity c by household h
  QHA(RR,A,RR1,C,RR2,H)                 quantity consumed of home commodity c fr act a by hhd h
  QINT(RR,C,RR1,A)                      quantity of intermediate demand for c from activity a
  QINTA(RR,A)                           quantity of aggregate intermediate input
  QINV(RR,C)                            quantity of fixed investment demand
  QM(ROW,RR,C)                          quantity of imports
  QQ(RR,C)                              quantity of composite goods supply
  QT(RR,C)                              quantity of trade and transport demand for commodity c
  QVA(RR,A)                             quantity of aggregate value added
  QX(RR,C)                              quantity of aggregate marketed commodity output
  QXAC(RR,A,RR1,C)                      quantity of ouput of commodity c from activity a
  TABS                                  total absorption
  TINS(RR,INS,RN)                       rate of direct tax on domestic institutions ins paid to gov of region R1
  TINSADJ                               direct tax scaling factor
  TRII(RR,INS,RR1,INS1)                 transfers to dom. inst. insdng from INSDNG1
  WALRAS                                savings-investment imbalance (should be zero)
  WALRASSQR                             Walras squared
  WF(F)                                 economy-wide wage (rent) for factor f
  WFR(RR,F)                             region wage (rent) for factor f
  WFDIST(RR,F,RR1,A)                    factor wage distortion variable
  YF(RR,F)                              factor income
  YG(RN)                                total current government income
  YIF(RR,INS,RR1,F)                     income of institution ins from factor f
  YI(RR,INS)                            income of (domestic non-governmental) institution ins
  WPAY                                  water charge payments to households
  QFUE(RR,F)                            Quantity of unemployed people (leisure in utility)
  LABAVL(RR,H)                          Total available Labor (Population - gammal)
;

*--------------------------------------------------------------------------------
*-6. VARIABLE DEFINITIONS -------------------------------------------------------
*--------------------------------------------------------------------------------

*-The initial levels of all model variables are defined in this file.

$STITLE Input file: VARINIT.INC. Initialization of variables. TACOGEM-R

  CPI.L                           = CPI0;
  DMPS.L                          = DMPS0;
  DPI.L                           = DPI0;
  DTINS.L                         = DTINS0;
  EG.L(RN)                        = EG0(RN);
  EH.L(RR,H)                      = EH0(RR,H);
  EXR.L(ROW)                      = EXR0(ROW);
  TFSAV.L                         = TFSAV0;
  FSAV.L(ROW)                     = FSAV0(ROW);
  GADJ.L                          = GADJ0;
  GOVSHR.L                        = GOVSHR0;
  GSAV.L                          = GSAV0;
  IADJ.L                          = IADJ0;
  INVSHR.L                        = INVSHR0;
  MPS.L(RR,INSDNG)                = MPS0(RR,INSDNG);
  MPSADJ.L                        = MPSADJ0    ;
  PA.L(RR,A)                      = PA0(RR,A);
  PDD.L(RR,C)                     = PDD0(RR,C);
  PDS.L(RR,C)                     = PDS0(RR,C);
  PINTA.L(RR,A)                   = PINTA0(RR,A) ;
  PE.L(RR,C,ROW)                  = PE0(RR,C,ROW);
  PM.L(ROW,RR,C)                  = PM0(ROW,RR,C);
  PQ.L(RR,C)                      = PQ0(RR,C);
  PVA.L(RR,A)                     = PVA0(RR,A);
  PWE.L(RR,C,ROW)                 = PWE0(RR,C,ROW);
  PWM.L(ROW,RR,C)                 = PWM0(ROW,RR,C);
  PX.L(RR,C)                      = PX0(RR,C);
  PXAC.L(RR,A,RR1,C)              = PXAC0(RR,A,RR1,C);
  QA.L(RR,A)                      = QA0(RR,A);
  QD.L(RR,C)                      = QD0(RR,C);
  QE.L(RR,C,ROW)                  = QE0(RR,C,ROW);
  QF.L(RR,F,RR1,A)                = QF0(RR,F,RR1,A);
  QFS.L(RR,F)                     = QFS0(RR,F);
  QG.L(RR,C,RN)                   = QG0(RR,C,RN);
  QH.L(RR,C,RR1,H)                = QH0(RR,C,RR1,H);
  QHA.L(RR,A,RR1,C,RR2,H)         = QHA0(RR,A,RR1,C,RR2,H);
  QINT.L(RR,C,RR1,A)              = QINT0(RR,C,RR1,A);
  QINTA.L(RR,A)                   = QINTA0(RR,A) ;
  QINV.L(RR,C)                    = QINV0(RR,C);
  QM.L(ROW,RR,C)                  = QM0(ROW,RR,C);
  QQ.L(RR,C)                      = QQ0(RR,C);
  QT.L(RR,C)                      = QT0(RR,C);
  QVA.L(RR,A)                     = QVA0(RR,A);
  QX.L(RR,C)                      = QX0(RR,C);
  QXAC.L(RR,A,RR1,C)              = QXAC0(RR,A,RR1,C);
  TABS.L                          = TABS0;
  TRII.L(RR,INSDNG,RR1,INSDNG1)   = TRII0(RR,INSDNG,RR1,INSDNG1);
  TINS.L(RR,INSDNG,RN)            = TINS0(RR,INSDNG,RN);
  TINSADJ.L                       = TINSADJ0;
  WALRAS.L                        = WALRAS0;
  WALRASSQR.L                     = 0 ;
  WF.L(F)                         = WF0(F);
  WFR.L(RR,F)                     = WFR0(RR,F);
  WFDIST.L(RR,F,RR1,A)            = WFDIST0(RR,F,RR1,A);
  YF.L(RR,F)                      = YF0(RR,F);
  YG.L(RN)                        = YG0(RN);
  YI.L(RR,INS)                    = YI0(RR,INS);
  YIF.L(RR,INS,RR1,F)             = YIF0(RR,INS,RR1,F);
  WPAY.L(RR)                      = WPAY0(RR)  ;
  QFUE.L(RR,F)                    = QFUE0(RR,F);
  LABAVL.L(RR,H)                  = LABAVL0(RR,H);

*-#*#*#*#*# THE END OF VARINIT.INC #*#*#*#*

*-Optional include file that imposes lower limits for selected variables
*-The inclusion of this file may improve solver performance.
*-$INCLUDE 1VARLOW.INC

*--------------------------------------------------------------------------------
*-7. EQUATION DECLARATIONS ------------------------------------------------------
*--------------------------------------------------------------------------------

EQUATIONS
*-Price block===============================================

 PMDEF(ROW,RR,C)                 domestic import price
 PEDEF(RR,C,ROW)                 domestic export price
 PDDDEF(RR,C)                    dem price for comy. c produced and sold domestically
 PQDEF(RR,C)                     value of sales in domestic market
 PQDEF2(RR,C)                    value of sales in domestic market
 PXDEF(RR,C)                     value of marketed domestic output
 PADEF(RR,A)                     output price for activity a
 PINTADEF(RR,A)                  price of aggregate intermediate input
 PVADEF(RR,A)                    value-added price
 CPIDEF                         consumer price index
 DPIDEF                         domestic producer price index
*-Production and trade block========================
 CESAGGPRD(RR,A)                 CES aggregate prod fn (if CES top nest)
 CESAGGFOC(RR,A)                 CES aggregate first-order condition (if CES top nest)
 LEOAGGINT(RR,A)                 Leontief aggreg intermed dem (if Leontief top nest)
 LEOAGGVA(RR,A)                  Leontief aggreg value-added dem (if Leontief top nest)
 CESVAPRD(RR,A)                  CES value-added production function

 CESVAFOC1(RR,F,RR1,A)           CES value-added first-order condition
 CESVAFOC2(RR,F21N,RR1,A)       CES value-added first-order condition for the factor coming from the second nest
 CESVA2NE(RR2,F22N,RR1,A)        2nd Nest in Production

 INTDEM(RR,C,RR1,A)               intermediate demand for commodity c from activity a
 COMPRDFN(RR,A,RR1,C)             production function for commodity c and activity a
 OUTAGGFN(RR,C)                  output aggregation function
 OUTAGGFOC(RR,A,RR1,C)            first-order condition for output aggregation function
 CET(RR,C)                       CET function
 CET2(RR,C)                      domestic sales and exports for outputs without both
 ESUPPLY(RR,C,ROW)               export supply
 ARMINGTON(RR,C)                 composite commodity aggregation function
 COSTMIN(ROW,RR,C)               first-order condition for composite commodity cost min
 ARMINGTON2(RR,C)                comp supply for com's without both dom. sales and imports
 QTDEM(RR,C)                     demand for transactions (trade and transport) services

*-Institution block                ========================================
 YFDEF(RR,F)                     factor incomes
 YIFDEF(R,INS,RR,F)             factor incomes to domestic institutions
 YIDEF(RR,INSDNG)                   total incomes of domest non-gov't institutions
 EHDEF(RR,H)                     household consumption expenditures
 TRIIDEF(R,INS,R1,INS1)         transfers to inst'on ins from inst'on INS1
 HMDEM(RR,C,RR1,H)                LES cons demand by hhd h for marketed commodity c
 HADEM(RR,A,RR1,C,RR2,H)           LES cons demand by hhd h for home commodity c fr act a
 INVDEM(RR,C)                    fixed investment demand
 GOVDEM(RR,C,RN)                    government consumption demand
 EGDEF(RN)                       total government expenditures
 YGDEF(RN)                       total government income

*-System constraint block=============================
 COMEQUIL(RR,C)                  composite commodity market equilibrium
 FACEQUIL1(RR,F)                  factor market equilibrium
 FACEQUIL2(RR,F)                  factor market equilibrium
 CURACCBAL(ROW)                 current account balance (of RoW)
 GOVBAL                         government balance
 TINSDEF(R,INS,RN)              direct tax rate for inst ins
 MPSDEF(RR,INS)                  marg prop to save for inst ins
 SAVINVBAL                      savings-investment balance
 TABSEQ                         total absorption
 INVABEQ                        investment share in absorption
 GDABEQ                         government consumption share in absorption
 OBJEQ                          Objective function
 WPAYDEF(RR)                    Definition of water payments
 TFSAVE                         Total Foreign Savings
 UNEMPDEF(RR)                 labor supply equation
 LABSUPEQ(RR,H)                 labor supply equation
 LABAVLEQ(RR,H)                 Total available labor equation

;
*--------------------------------------------------------------------------------
*-8. EQUATION DEFINITIONS -------------------------------------------------------
*--------------------------------------------------------------------------------
*-Notational convention inside equations:
*-Parameters and "invariably" fixed variables are in lower case.
*-"Variable" variables are in upper case.


*-Price block===============================================

 PMDEF(ROW,RR,C)$CMR(RR,C,ROW)..
       PM(ROW,RR,C) =E= PWM(ROW,RR,C)*(1 + sum(RN,tm(RN,ROW,RR,C)))*EXR(ROW) + SUM((RR1,C1)$CT(RR1,C1), PQ(RR1,C1)*icm(RR1,C1,RR,C));

 PEDEF(RR,C,ROW)$CER(RR,C,ROW)..
        PE(RR,C,ROW) =E= PWE(RR,C,ROW)*(1 - SUM(RN,te(RR,C,RN,ROW)))*EXR(ROW) - SUM((RR1,C1)$CT(RR1,C1), PQ(RR1,C1)*ice(RR1,C1,RR,C));

 PDDDEF(RR,C)$CD(RR,C)..
        PDD(RR,C) =E= PDS(RR,C) + SUM((RR1,C1)$CT(RR1,C1), PQ(RR1,C1)*icd(RR1,C1,RR,C));

 PQDEF(RR,C)$(CM(RR,C))..
        PQ(RR,C)*(1 - sum(RN,tq(RR,C,RN))
*-If there are no subsidies then comment out the following line
*-  -sq(c)
) =E=
      (SUM(ROW$deltaq(ROW,RR,C) , deltaq(ROW,RR,C)**(1/(1+rhoq(RR,C)))*PM(ROW,RR,C)**(rhoq(RR,C)/(1+rhoq(RR,C))))
       + (1 - SUM(ROW, deltaq(ROW,RR,C)))**(1/(1+rhoq(RR,C)))*PDD(RR,C)**(rhoq(RR,C)/(1+rhoq(RR,C))) )**
       ((1+rhoq(RR,C))/rhoq(RR,C))/alphaq(RR,C) ;

 PQDEF2(RR,C)$(CD(RR,C) and not CM(RR,C))..
       PQ(RR,C)*(1 - sum(RN,tq(RR,C,RN))) =E= PDD(RR,C) ;

 PXDEF(RR,C)$CX(RR,C)..
        PX(RR,C)*QX(RR,C) =E= PDS(RR,C)*QD(RR,C) + SUM(ROW$CER(RR,C,ROW) , PE(RR,C,ROW)*QE(RR,C,ROW));

 PADEF(RR,A)..
        PA(RR,A) =E= SUM((RR1,C), PXAC(RR,A,RR1,C)*theta(RR,A,RR1,C));

 PINTADEF(RR,A)..
        PINTA(RR,A) =E= SUM((RR1,C), PQ(RR1,C)*ica(RR1,C,RR,A)) ;

 PVADEF(RR,A)..
        PA(RR,A)*(1-sum(RN,ta(RR,A,RN)))*QA(RR,A) =E= PVA(RR,A)*QVA(RR,A) + PINTA(RR,A)*QINTA(RR,A);

 CPIDEF..
        CPI =E= SUM((RR,C), cwts(RR,C)*PQ(RR,C)) ;

 DPIDEF..
        DPI =E= SUM((RR,C)$CD(RR,C), dwts(RR,C)*PDS(RR,C)) ;


*-Production and trade block================================

*-CESAGGPRD and CESAGGFOC apply to activities with CES function at
*-top of technology nest.

 CESAGGPRD(RR,A)$ACES(RR,A)..
   QA(RR,A) =E= alphaa(RR,A)*(deltaa(RR,A)*QVA(RR,A)**(-rhoa(RR,A))
               + (1-deltaa(RR,A))*QINTA(RR,A)**(-rhoa(RR,A)))**(-1/rhoa(RR,A)) ;

 CESAGGFOC(RR,A)$ACES(RR,A)..
   QVA(RR,A) =E= QINTA(RR,A)*((PINTA(RR,A)/PVA(RR,A))*(deltaa(RR,A)/
                                 (1 - deltaa(RR,A))))**(1/(1+rhoa(RR,A))) ;

*-LEOAGGINT and LEOAGGVA apply to activities with Leontief function at
*-top of technology nest.

 LEOAGGINT(RR,A)$ALEO(RR,A)..  QINTA(RR,A) =E= inta(RR,A)*QA(RR,A) ;

 LEOAGGVA(RR,A)$ALEO(RR,A)..  QVA(RR,A) =E= iva(RR,A)*QA(RR,A) ;


*-CESVAPRD, CESVAFOC, INTDEM apply to the first nest at the bottom of the technology nest (for all activities).

*- single prod nest version
$ONTEXT
 CESVAPRD(RR,A)..
   QVA(RR,A)$rhova(RR,A) =E= alphava(RR,A)*(SUM((RR1,F1N),
                      deltava(RR1,F1N,RR,A)*QF(RR1,F1N,RR,A)
                                          **(-rhova(RR,A))) )**(-1/rhova(RR,A)) ;

 CESVAFOC(RR,F1N,RR1,A)$deltava(RR,F1N,RR1,A)..
        (WFR(RR,F1N)*wfdist(RR,F1N,RR1,A)) =E=
        (1-sum(RN,tva(RR1,A,RN)))*PVA(RR1,A)*QVA(RR1,A)
                *SUM((RR2,F1N1), deltava(RR2,F1N1,RR1,A)*QF(RR2,F1N1,RR1,A)**(-rhova(RR1,A)) )**(-1)
                *deltava(RR,F1N,RR1,A)*QF(RR,F1N,RR1,A)**(-rhova(RR1,A)-1);

$OFFTEXT


*adjusted according to second prod nest:
 CESVAPRD(RR,A)..
   QVA(RR,A)$rhova(RR,A) =E= alphava(RR,A)*(SUM((RR1,F1N),
                      deltava(RR1,F1N,RR,A)*QF(RR1,F1N,RR,A)**(-rhova(RR,A)))
                                          +SUM((RR1,F21N),
                      deltava(RR1,F21N,RR,A)*QF(RR1,F21N,RR,A)**(-rhova(RR,A)))
                                                         )**(-1/rhova(RR,A)) ;

 CESVAFOC1(RR,F1N,RR1,A)$deltava(RR,F1N,RR1,A)..
        (WFR(RR,F1N)*wfdist(RR,F1N,RR1,A)) =E=
        (1-sum(RN,tva(RR1,A,RN)))*PVA(RR1,A)*QVA(RR1,A)
                *(SUM((RR2,F1N1), deltava(RR2,F1N1,RR1,A)*QF(RR2,F1N1,RR1,A)**(-rhova(RR1,A)) )
                   + SUM((RR2,F21N1), deltava(RR2,F21N1,RR1,A)*QF(RR2,F21N1,RR1,A)**(-rhova(RR1,A))))**(-1)
                *deltava(RR,F1N,RR1,A)*QF(RR,F1N,RR1,A)**(-rhova(RR1,A)-1);


*FOC for the factor coming from the second nest
 CESVAFOC2(RR,F21N,RR1,A)$deltava(RR,F21N,RR1,A)..
*        (WFR(RR,F21N)*wfdist(RR,F21N,RR1,A)+
        SUM(F2N,WFR(RR,F2N)*wfdist(RR,F2N,RR1,A)*nu(RR,F2N,RR1,A)) =E=
        (1-sum(RN,tva(RR1,A,RN)))*PVA(RR1,A)*QVA(RR1,A)
                *(SUM((RR2,F1N1), deltava(RR2,F1N1,RR1,A)*QF(RR2,F1N1,RR1,A)**(-rhova(RR1,A)) )
                   + SUM((RR2,F21N1), deltava(RR2,F21N1,RR1,A)*QF(RR2,F21N1,RR1,A)**(-rhova(RR1,A))))**(-1)
                *deltava(RR,F21N,RR1,A)*QF(RR,F21N,RR1,A)**(-rhova(RR1,A)-1);

*- The second nest at the bottom
 CESVA2NE(RR,F22N,RR1,A)..
  QF(RR,F22N,RR1,A)=E= SUM(F21N,QF(RR,F21N,RR1,A))*nu(RR,F22N,RR1,A);


 INTDEM(RR,C,RR1,A)$ica(RR,C,RR1,A)..
        QINT(RR,C,RR1,A) =E= ica(RR,C,RR1,A)*QINTA(RR1,A);


 COMPRDFN(RR,A,RR1,C)$theta(RR,A,RR1,C)..
    QXAC(RR,A,RR1,C) + SUM((RR2,H), QHA(RR,A,RR1,C,RR2,H)) =E= theta(RR,A,RR1,C)*QA(RR,A) ;

 OUTAGGFN(RR,C)$CX(RR,C)..
        QX(RR,C) =E= alphaac(RR,C)*SUM((RR1,A), deltaac(RR1,A,RR,C)*QXAC(RR1,A,RR,C)
                                 **(-rhoac(RR,C)))**(-1/rhoac(RR,C));

 OUTAGGFOC(RR,A,RR1,C)$deltaac(RR,A,RR1,C)..
   PXAC(RR,A,RR1,C) =E=
   PX(RR1,C)*QX(RR1,C)*SUM((RR2,A1), deltaac(RR2,A1,RR1,C)*QXAC(RR2,A1,RR1,C)**(-rhoac(RR1,C)) )**(-1)
   *deltaac(RR,A,RR1,C)*QXAC(RR,A,RR1,C)**(-rhoac(RR1,C)-1);

 CET(RR,C)$(CE(RR,C) AND CD(RR,C))..
        QX(RR,C) =E= alphat(RR,C)*(SUM(ROW, deltat(RR,C,ROW)*QE(RR,C,ROW)**rhot(RR,C))
                 + (1 - SUM(ROW, deltat(RR,C,ROW)))*QD(RR,C)**rhot(RR,C))**(1/rhot(RR,C)) ;

 ESUPPLY(RR,C,ROW)$(CER(RR,C,ROW) AND CD(RR,C))..
        QE(RR,C,ROW) =E=  QD(RR,C)*((PE(RR,C,ROW)/PDS(RR,C))*
             ((1 - SUM(ROW1, deltat(RR,C,ROW1)))/deltat(RR,C,ROW)))**(1/(rhot(RR,C)-1)) ;

 CET2(RR,C)$( (CD(RR,C) AND CEN(RR,C)) OR (CE(RR,C) AND CDN(RR,C)) )..
        QX(RR,C) =E= QD(RR,C) + SUM(ROW, QE(RR,C,ROW));

 ARMINGTON(RR,C)$(CM(RR,C) AND CD(RR,C))..

   QQ(RR,C) =E= alphaq(RR,C)*(SUM(ROW, deltaq(ROW,RR,C)*QM(ROW,RR,C)**(-rhoq(RR,C))) +
                  (1-SUM(ROW, deltaq(ROW,RR,C)))*QD(RR,C)**(-rhoq(RR,C)))**(-1/rhoq(RR,C));


 COSTMIN(ROW,RR,C)$(CD(RR,C) AND CMR(RR,C,ROW))..
   QM(ROW,RR,C)/QD(RR,C) =E= (PDD(RR,C)/PM(ROW,RR,C)*deltaq(ROW,RR,C)/
                 (1-SUM(ROW1, deltaq(ROW1,RR,C))))**(1/(1+rhoq(RR,C)));


 ARMINGTON2(RR,C)$( (CD(RR,C) AND CMN(RR,C)) OR (CM(RR,C) AND CDN(RR,C)) )..
        QQ(RR,C) =E= QD(RR,C) + SUM(ROW$CMR(RR,C,ROW), QM(ROW,RR,C));

 QTDEM(RR,C)$CT(RR,C)..
        QT(RR,C) =E= SUM((RR1,C1), icd(RR,C,RR1,C1)*QD(RR1,C1))
                                  + SUM((RR1,C1,ROW)$CMR(RR1,C1,ROW), icm(RR,C,RR1,C1)*QM(ROW,RR1,C1))
                                  + SUM((RR1,C1,ROW), ice(RR,C,RR1,C1)*QE(RR1,C1,ROW))
*-                +icd(C,C1)*QD(C1))
;

*-Institution block ========================================

 YFDEF(RR,F)..
*-        YF(RR,F) =E= SUM((RR1,A), (twa(RR,F,RR1,A)+WF(F)*wfdist(RR,F,RR1,A))*QF(RR,F,RR1,A));
        YF(RR,F) =E= SUM((RR1,A), WFR(RR,F)*wfdist(RR,F,RR1,A)*QF(RR,F,RR1,A));


 YIFDEF(R,INSD,RR,F)$shif(R,INSD,RR,F)..
        YIF(RR,INSD,RR,F) =E= shif(RR,INSD,RR,F)*((1-sum(RN,tf(RR,F,RN)))*YF(RR,F)
        - SUM((RN,ROW), trnsfr(RN,ROW,RR,F)*EXR(ROW)));


 WPAYDEF(RR)..
          WPAY(RR) =E= SUM((F,RR1,A), twa(RR,F,RR1,A)*QF(RR,F,RR1,A)) ;

 YIDEF(RR,INSDNG)$YI0(RR,INSDNG)..
  YI(RR,INSDNG) =E=
     SUM((RR1,F), YIF(RR,INSDNG,RR1,F))
   + shiw(RR,INSDNG)*WPAY(RR)
   + SUM((RR2,INSDNG1), TRII(RR,INSDNG,RR2,INSDNG1))
   + SUM(RN,trnsfr(RR,INSDNG,RN,'GOV'))*CPI
   + SUM((RN,ROW),trnsfr(RR,INSDNG,RN,ROW)*EXR(ROW));

 TRIIDEF(RR,INSDNG,RR1,INSDNG1)$(shii(RR,INSDNG,RR1,INSDNG1))..
  TRII(RR,INSDNG,RR1,INSDNG1) =E= shii(RR,INSDNG,RR1,INSDNG1)
          *(1 - MPS(RR1,INSDNG1))*(1 - SUM(RN,TINS(RR1,INSDNG1,RN)))* YI(RR1,INSDNG1);

 EHDEF(RR,H)..
  EH(RR,H) =E= (1 - SUM((RR1,INSDNG), shii(RR1,INSDNG,RR,H)))*(1 - MPS(RR,H))
                                          *(1 - SUM(RN,TINS(RR,H,RN)))*YI(RR,H);

 HMDEM(RR,C,RR1,H)$betam(RR,C,RR1,H)..
   PQ(RR,C)*QH(RR,C,RR1,H)=E=
    PQ(RR,C)*gammam(RR,C,RR1,H)+(betam(RR,C,RR1,H)/(1-betal(RR1,H)))*( EH(RR1,H) - SUM((RR2,C1), PQ(RR2,C1)*gammam(RR2,C1,RR1,H))
                         - SUM((RR3,A,RR2,C1), PXAC(RR3,A,RR2,C1)*gammah(RR3,A,RR2,C1,RR1,H))) ;

 HADEM(RR,A,RR1,C,RR2,H)$betah(RR,A,RR1,C,RR2,H)..
   PXAC(RR,A,RR1,C)*QHA(RR,A,RR1,C,RR2,H)=E=
     PXAC(RR,A,RR1,C)*gammah(RR,A,RR1,C,RR2,H)
                + (betah(RR,A,RR1,C,RR2,H)/(1-betal(RR2,H)))*(EH(RR2,H) - SUM((RR4,C1), PQ(RR4,C1)*gammam(RR4,C1,RR2,H))
                       - SUM((RR5,A1,RR4,C1), PXAC(RR5,A1,RR4,C1)*gammah(RR5,A1,RR4,C1,RR2,H))) ;

 LABSUPEQ(RR,H)$betal(RR,H)..
  QFS(RR,'FLAB')-QFUE(RR,'FLAB')=E= LABAVL(RR,H)-(betal(RR,H)/(1-betal(RR,H)))/WFR(RR,'FLAB')
                            *( EH(RR,H) - SUM((RR1,C1), PQ(RR1,C1)*gammam(RR1,C1,RR,H))
                                 - SUM((RR3,A,RR2,C1), PXAC(RR3,A,RR2,C1)*gammah(RR3,A,RR2,C1,RR,H)));

 UNEMPDEF(RR).. QFUE(RR,'FLAB') =E= QFS(RR,'FLAB')-SUM((RR1,A),QF(RR,'FLAB',RR1,A));

 LABAVLEQ(RR,H).. LABAVL(RR,H)/LABAVL0(RR,H)=E={[WFR(RR,'FLAB')/CPI]/[WFR0(RR,'FLAB')/CPI0]}**etals(RR,H);

 INVDEM(RR,C)$CINV(RR,C)..  QINV(RR,C) =E= IADJ*qbarinv(RR,C);

 GOVDEM(RR,C,RN)..  QG(RR,C,RN) =E= GADJ *qbarg(RR,C,RN);


 YGDEF(RN)..
     YG(RN) =E= SUM((RR1,INSDNG), TINS(RR1,INSDNG,RN)*YI(RR1,INSDNG))
             + SUM((RR1,F), tf(RR1,F,RN)*YF(RR1,F))
             + SUM((RR1,A), tva(RR1,A,RN)*PVA(RR1,A)*QVA(RR1,A))
             + SUM((RR1,A), ta(RR1,A,RN)*PA(RR1,A)*QA(RR1,A))
*-water enters in the same way as ta(A)
*-           +SUM((F,A), twa(F,A)*QF(F,A))
*-or water payments to government
*-             +SUM(RR,WPAY(RR))
             + SUM((RR1,C,ROW)$CM(RR1,C), tm(RN,ROW,RR1,C)*pwm(ROW,RR1,C)*QM(ROW,RR1,C)*EXR(ROW))
             + SUM((RR1,C,ROW)$CE(RR1,C), te(RR1,C,RN,ROW)*pwe(RR1,C,ROW)*QE(RR1,C,ROW)*EXR(ROW))
             + SUM((RR1,C), tq(RR1,C,RN)*PQ(RR1,C)*QQ(RR1,C))
*-we don't have subsidies
*-           + SUM(C, sq(C)*PQ(C)*QQ(C))

*            + SUM((RR1,F), YIF(RN,'GOV',RR1,F))
*-if already included as factor taxes exclude transfers to gov't

*-           + SUM(F$(NOT tf(F)), YIF('GOV',F))
             + sum((RN1,ROW), trnsfr(RN,'GOV',RN1,ROW)*EXR(ROW))
*-             + YF(R,'FWATU')
;

 EGDEF(RN)..
   EG(RN) =E=
        SUM((RR1,C), PQ(RR1,C)*QG(RR1,C,RN)) + SUM((RR2,INSDNG), trnsfr(RR2,INSDNG,RN,'GOV'))*CPI;

*-System constraint block===================================

 FACEQUIL1(RR,FNLAB)..
        SUM((RR1,A), QF(RR,FNLAB,RR1,A)) =E= QFS(RR,FNLAB);

 FACEQUIL2(RR,FLAB)..
        SUM((RR1,A), QF(RR,FLAB,RR1,A)) =E= QFS(RR,FLAB)-QFUE(RR,FLAB);
*        SUM((RR1,A), QF(RR,FLAB,RR1,A)) =E= QFS(RR,FLAB);

 COMEQUIL(RR,C)..
        QQ(RR,C) =E= SUM((RR1,A), QINT(RR,C,RR1,A)) + SUM((RR1,H), QH(RR,C,RR1,H)) +SUM(RN, QG(RR,C,RN))
                + QINV(RR,C) + qdst(RR,C) + QT(RR,C);

 CURACCBAL(ROW)..
        SUM((RR,C)$CM(RR,C), PWM(ROW,RR,C)*QM(ROW,RR,C)) + SUM((RN,RR1,F), trnsfr(RN,ROW,RR1,F)) =E=
        SUM((RR,C)$CE(RR,C), PWE(RR,C,ROW)*QE(RR,C,ROW)) + SUM((R,RN,INSD), trnsfr(R,INSD,RN,ROW))
        + FSAV(ROW);

 TFSAVE.. TFSAV =E=  SUM(ROW,FSAV(ROW))  ;


 GOVBAL.. SUM(RN,YG(RN)) =E= SUM(RN,EG(RN)) + GSAV;

 TINSDEF(RR,INSDNG,RN)..
  TINS(RR,INSDNG,RN) =E= tinsbar(RR,INSDNG,RN)*(1 + TINSADJ*tins01(RR,INSDNG))
                   + DTINS*tins01(RR,INSDNG);

 MPSDEF(RR,INSDNG)..
  MPS(RR,INSDNG)  =E= mpsbar(RR,INSDNG)*(1 + MPSADJ*mps01(RR,INSDNG))
                   + DMPS*mps01(RR,INSDNG);

 SAVINVBAL..
   SUM((RR,INSDNG), MPS(RR,INSDNG)*(1 - SUM(RN,TINS(RR,INSDNG,RN)))*YI(RR,INSDNG))
    + GSAV + SUM(ROW,FSAV(ROW)*EXR(ROW)) =E=
   SUM((RR,C), PQ(RR,C)*QINV(RR,C)) + SUM((RR,C), PQ(RR,C)*qdst(RR,C)) + WALRAS;

 TABSEQ..
  TABS =E=
   SUM((RR,C,RR1,H), PQ(RR,C)*QH(RR,C,RR1,H)) + SUM((RR2,A,RR,C,RR1,H), PXAC(RR2,A,RR,C)*QHA(RR2,A,RR,C,RR1,H))
  + SUM((RR,C,RN), PQ(RR,C)*QG(RR,C,RN)) + SUM((RR,C), PQ(RR,C)*QINV(RR,C)) + SUM((RR,C), PQ(RR,C)*qdst(RR,C));

 INVABEQ.. INVSHR*TABS =E= SUM((RR,C), PQ(RR,C)*QINV(RR,C)) + SUM((RR,C), PQ(RR,C)*qdst(RR,C));

 GDABEQ..  GOVSHR*TABS =E= SUM((RR,C,RN), PQ(RR,C)*QG(RR,C,RN));

 OBJEQ..   WALRASSQR   =E= WALRAS*WALRAS ;

*--------------------------------------------------------------------------------
*-9. MODEL DEFINITION -----------------------------------------------------------
*--------------------------------------------------------------------------------
*-$ONTEXT
 MODEL STANDCGE  standard CGE model

 /
*-Price block (10)
 PMDEF.PM
*- PMDEF
 PEDEF.PE
 PQDEF.PQ
 PQDEF2
 PXDEF.PX
 PDDDEF.PDD
 PADEF.PA
 PINTADEF.PINTA
 PVADEF.PVA
 CPIDEF
 DPIDEF
*-Production and trade block (17)
 CESAGGPRD.QVA
 CESAGGFOC
*- LEOAGGINT
 LEOAGGINT.QINTA
*- LEOAGGVA
 LEOAGGVA.QA
*- CESVAPRD.QVA
 CESVAPRD
*- CESVAPRD1
*- CESVAPRD2
 CESVAFOC1
 CESVAFOC2
 CESVA2NE
*- CESVAFOC.QF
 INTDEM.QINT
 COMPRDFN.PXAC
 OUTAGGFN.QX
 OUTAGGFOC.QXAC
 CET
 CET2
 ESUPPLY.QE
 ARMINGTON
 COSTMIN.QM
 ARMINGTON2
*- ESUPPLY.QE
*- ARMINGTON.QD
*- COSTMIN.QM
*- ARMINGTON2.QD
 QTDEM.QT
*-Institution block (12)
 YFDEF.YF
 YIFDEF.YIF
 YIDEF.YI
 EHDEF.EH
 TRIIDEF.TRII
 HMDEM.QH
 HADEM.QHA
 EGDEF.EG
 YGDEF.YG
 GOVDEM.QG
 GOVBAL
 INVDEM.QINV
*-System-constraint block (9)
 FACEQUIL1
 FACEQUIL2
*- COMEQUIL
 COMEQUIL.QQ
*- CURACCBAL.EXR
 CURACCBAL.FSAV
 TFSAVE
 TINSDEF.TINS
 MPSDEF.MPS
 SAVINVBAL.WALRAS
 TABSEQ.TABS
 INVABEQ
 GDABEQ
 WPAYDEF.WPAY
 UNEMPDEF
 LABSUPEQ
 LABAVLEQ.LABAVL
* FACSUPPLY
 /
;


*---------------------------------------------------------------------------------
*--10. FIXING VARIABLES NOT IN MODEL AT ZERO -------------------------------------
*---------------------------------------------------------------------------------

  PDD.FX(RR,C)$(NOT CD(RR,C)) = 0;
  PDS.FX(RR,C)$(NOT CD(RR,C)) = 0;
  PE.FX(RR,C,ROW)$(NOT CE(RR,C)) = 0;
  PM.FX(ROW,RR,C)$(NOT CM(RR,C)) = 0;
  PX.FX(RR,C)$(NOT CX(RR,C)) = 0;
  PXAC.FX(RR,A,RR1,C)$(NOT SAM(RR,A,RR1,C)) = 0;

  QD.FX(RR,C)$(NOT CD(RR,C)) = 0;
  QE.FX(RR,C,ROW)$(NOT CE(RR,C)) = 0;
  QF.FX(RR,F,RR1,A)$(NOT SAM(RR,F,RR1,A)) = 0;
  QG.FX(RR,C,RN)$(NOT SAM(RR,C,RN,'GOV')) = 0;
  QH.FX(RR,C,RR1,H)$(NOT SAM(RR,C,RR1,H)) = 0;
  QHA.FX(RR,A,RR1,C,RR2,H)$(NOT BETAH(RR,A,RR1,C,RR2,H)) = 0;
  QINT.FX(RR,C,RR1,A)$(NOT SAM(RR,C,RR1,A)) = 0;
  QINV.FX(RR,C)$(NOT CINV(RR,C)) = 0;
  QM.FX(ROW,RR,C)$(NOT CM(RR,C)) = 0;
  QQ.FX(RR,C)$(NOT (CD(RR,C) OR CM(RR,C))) = 0;
  QT.FX(RR,C)$(NOT CT(RR,C)) = 0;
  QX.FX(RR,C)$(NOT CX(RR,C)) = 0;
  QXAC.FX(RR,A,RR1,C)$(NOT SAM(RR,A,RR1,C)) = 0;
  TRII.FX(RR,INSDNG,RR1,INSDNG1)$(NOT SAM(RR,INSDNG,RR1,INSDNG1)) = 0;
  YI.FX(RR,INS)$(NOT INSD(INS)) = 0;
  YIF.FX(RR,INS,RR1,F)$((NOT INSD(INS)) OR (NOT SAM(RR,INS,RR1,F))) = 0;

*---------------------------------------------------------------------------------
*--11. MODEL CLOSURE -------------------------------------------------------------
*---------------------------------------------------------------------------------

$ONTEXT
*- In the simulation file, SIM.GMS, the user chooses between
*- alternative closures. Those choices take precedence over the choices
*- made in this file.
*-
*- In the following segment, closures is selected for the base model
*- solution in this file. The clearing variables for micro and macro
*- constraints are as follows:
*-
*- FACEQUIL - WF: for each factor, the economywide wage is the
*- market-clearing variable in a setting with perfect factor mobility across
*- activities.
*-
*- CURACCBAL - EXR: a flexible exchange rate clears the current account of
*- the RoW.
*-
*- GOVBAL - GSAV: flexible government savings clears the government
*- account.
*-
*- SAVINVBAL - SADJ: the savings rates of domestic institutions are scaled
*- to generate enough savings to finance exogenous investment quantities
*- (investment-driven savings).
*-
*- The CPI is the model numeraire.
$OFFTEXT
*--Factor markets=======

*-Original factor market closure

  QFS.FX(RR,FNLAB)  = QFS0(RR,FNLAB);

*-  QFUE.FX(RR,FNLAB)      = 0;
*--  WF.FX(F)        =WF0(F);
  WFR.LO(RR,F)        = -inf;
  WFR.UP(RR,F)        = +inf;
  WFDIST.FX(RR,F,RR1,A)  = WFDIST0(RR,F,RR1,A);
*- WFDIST.LO(RR,F,RR1,A)  = -INF;
*- WFDIST.UP(RR,F,RR1,A)  = +INF;


*--Current account of RoW===========

 EXR.FX(ROW)      = EXR0(ROW);
*-- FSAV.FX(ROW)      = FSAV0(ROW);
*-  TFSAV.FX          =TFSAV0;

*--Import and export prices (in FCU) are fixed. A change in model
*--specification is required if these prices are to be endogenous.

  PWM.FX(ROW,RR,C)    = PWM0(ROW,RR,C) ;
  PWE.FX(RR,C,ROW)    = PWE0(RR,C,ROW) ;


*--Current government balance=======

*- GSAV.FX     = GSAV0 ;
 TINSADJ.FX = TINSADJ0;
 DTINS.FX    = DTINS0;
 GADJ.FX    = GADJ0;
*- GOVSHR.FX   = GOVSHR0 ;


*--Savings-investment balance=======

 MPSADJ.FX = MPSADJ0;
 DMPS.FX    = DMPS0 ;
*- IADJ.FX  = IADJ0;
*- INVSHR.FX = INVSHR0 ;
*--Numeraire price index============

*-- CPI.FX        = CPI0;
DPI.FX        = DPI0;

*---------------------------------------------------------------------------------
*--13. SOLUTION STATEMENT --------------------------------------------------------
*---------------------------------------------------------------------------------

OPTIONS ITERLIM = 1000, LIMROW = 3000, LIMCOL = 3, SOLPRINT=ON,
        MCP=PATH, NLP=CONOPT3 ;

$ONTEXT
These options are useful for debugging. When checking whether the
initial data represent a solution, set LIMROW to a value greater than
the number of equations and search for three asterisks in the listing
file. SOLPRINT=ON provides a complete listing file. The program also
has a number of display statements, so when running experiments it is
usually not necessary to provide a solution print as well.
$OFFTEXT

 STANDCGE.HOLDFIXED   = 1 ;
 STANDCGE.TOLINFREP   = .0001 ;

$ONTEXT
The HOLDFIXED option converts all variables which are fixed (.FX) into
parameters. They are then not solved as part of the model.
The TOLINFREP parameter sets the tolerance for determinining whether
initial values of variables represent a solution of the model
equations. Whether these initial equation values are printed is
determimed by the LIMROW option. Equations which are not satsfied to
the degree TOLINFREP are printed with three asterisks next to their
listing.
$OFFTEXT

 SOLVE STANDCGE USING MCP ;

 STANDCGE.MODELSTAT = 0;
 STANDCGE.SOLVESTAT = 0;
 STANDCGE.NUMREDEF  = 0;


*---------------------------------------------------------------------------------
*--14. OPTIONAL NLP MODEL DEFINITION AND SOLUTION STATEMENT ----------------------
*---------------------------------------------------------------------------------

$ONTEXT
Define a model that can be solved using a nonlinear programming (NLP)
solver. The model includes the equation OBJEQ which defines the
variable WALRASSQR, which is the square of the Walras' Law variable,
which must be zero in equilibrium.
$OFFTEXT

 MODEL NLPCGE  standard CGE model for NLP solver
*--Price block (10)
 /
 PMDEF
 PEDEF
 PQDEF
 PQDEF2
 PXDEF
 PDDDEF
 PADEF
 PINTADEF
 PVADEF
 CPIDEF
 DPIDEF

*--Production and trade block (17)
 CESAGGPRD
 CESAGGFOC
 LEOAGGINT
 LEOAGGVA
 CESVAPRD
 CESVAFOC1
 CESVAFOC2
 CESVA2NE
 INTDEM
 COMPRDFN
 OUTAGGFN
 OUTAGGFOC
 CET
 CET2
 ESUPPLY
 ARMINGTON
 COSTMIN
 ARMINGTON2
 QTDEM
 TFSAVE

*--Institution block (12)
 YFDEF
 YIFDEF
 YIDEF
 EHDEF
 TRIIDEF
 HMDEM
 HADEM
 EGDEF
 YGDEF
 GOVDEM
 GOVBAL
 INVDEM

*--System-constraint block (9)
 FACEQUIL1
 FACEQUIL2
 COMEQUIL
 CURACCBAL
 TINSDEF
 MPSDEF
 SAVINVBAL
 TABSEQ
 INVABEQ
 GDABEQ
 OBJEQ
 WPAYDEF
 UNEMPDEF
 LABSUPEQ
 LABAVLEQ
* FACSUPPLY
 /
 ;

 NLPCGE.HOLDFIXED   = 1 ;
 NLPCGE.TOLINFREP   = .0001 ;

*- SOLVE NLPCGE MINIMIZING WALRASSQR USING NLP ;

 NLPCGE.MODELSTAT = 1;
 NLPCGE.SOLVESTAT = 1;
 NLPCGE.NUMREDEF  = 1;
*--$OFFTEXT

*tf(RR,'FLAB',RN)=tf0(RR,'FLAB',RN)*1.1;

* SOLVE STANDCGE USING MCP ;
*SOLVE NLPCGE MINIMIZING WALRASSQR USING NLP ;
