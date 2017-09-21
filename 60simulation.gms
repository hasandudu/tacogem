$TITLE Simulation files.
$STITLE Turkish Agricultural CGE Model - Regional.
$STITLE Based on IFPRI Standard CGE modeling system, Version 1.01
$ONTEXT
*=============================================================================
* File      : 60simulation.gms
* Author    : Hasan Dudu (hasan@dudu.gen.tr)
* Version   : 1.0
* Date      : 15/08/2011 17:13:57
* Changed   : 13/12/2013 18:08:24
* Changed by: Hasan Dudu (hasan@dudu.gen.tr)
* Remarks   :
This file runs the simulations in the paper.

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

$OFFSYMLIST OFFSYMXREF

*- For a well-tested model that solves without errors and needs no further
*-debugging, you may overwrite the options that were specified in the
*-file MOD*.GMS by removing the asterisks in the first columns of the
*-following two lines.
OPTIONS ITERLIM=1000, LIMROW=3000, LIMCOL=3000
        SOLPRINT=ON , MCP=PATH, NLP=CONOPT3;

$ONTEXT
The file contents in outline:
1. SETS FOR SIMULATIONS
2. DEFINING EXPERIMENT PARAMETERS
3. DEFINING NUMERAIRE AND CLOSURES FOR MACRO SYSTEM CONSTRAINTS
4. DEFINING CLOSURES FOR FACTOR MARKETS
5. REPORT SETUP
6. LOOP
7. COMPUTING PERCENTAGE CHANGE FOR REPORT PARAMETERS
8. CHECKING FOR GAP ERROR IN REPORT PARAMETERS
9. DISPLAYING REPORTS

The purpose of the simulations that are implemented is to demonstrate
the use of the simulation file.

Note on standard steps when implementing a new simulation:

a. Add element to the set SIM (cf. TARCUT1)
b. If needed, declare new experiment parameter(s) (cf. TMSIM);
c. For the relevant experiment parameters, define default value
   (if new) and value for new simulation (cf. TMSIM).
d. Create report parameters (included in LOOPREP.INC and PERCCALC.INC)
   for each new model parameter that is subject to change in new
   experiments.
e. Verify that default macro and factor market closures are OK;
   if not, select different closures for the new simulation
   (see SICLOS, GOVCLOS, ROWCLOS, FACTFE, FMOBFE, FMOBUE)
f. In the beginning of the loop, impose the value of the
   experiment parameter. If the parameter is new, additional code
   is required (cf. tm and TMSIM in LOOP)
g. Include the simulation in SIMCUR.
h. Save and run the simulation file.
$OFFTEXT



*-1. SETS FOR SIMULATIONS=============================================

*-HD: 02/09/08 Get sim sets from excel to make it more user friendly.
SET
SIM         simulations
SIMCUR(SIM) active simulations

CLS         closures
/NUMER
 SICCLS
 ROWCLS
 GOVCLS /



$call "gdxxrw i=xls\02sim.xlsx o=gdx\06sim.gdx index=index!a2"
$gdxin gdx\06sim.gdx

$load sim simcur

 SIMBASINIT(SIM) simulations with variable initialized at base level
 SIMMCP(SIM)     simulations solved as MCP problems
 ;

*-By defining SIMCUR, the user selects the experiments that are
*-carried out. BASE should always be included in SIMCUR.

 SIMCUR('BASE')    = YES;

*using conopt3 since it is far more faster
 SIMMCP(SIM)     = NO;

*-Variable initializatiion at base level may provide a better starting
*-point for selected simulations (depending on the content of the
*-preceding simulation).
 SIMBASINIT(SIM)   = NO;
 SIMBASINIT(SIM)   = YES;

*-It is typically preferable to solve the MCP version of the model.
* SIMMCP(SIM)     = YES;

*-DISPLAY SIMCUR, SIMBASINIT;


*-END: SETS FOR SIMULATIONS===========================================
*-2. DEFINING EXPERIMENT PARAMETERS===================================
*-hd loading parameters from excel

PARAMETERS
YIELD(RR,A,SIM)                % change in yield due to climate change
WATREQ(RR,F,RR1,A,SIM)         % change in water requirement per irrigated ha

chgta(RR,A,RN,SIM)             point change in activity tax
chgte(RR,C,RN,ROW,SIM)         point change in export tax
chgtf(RR,F,RN,SIM)             point change in factor tax
chgtins(RR,INS,RN,SIM)         point change in inst tax (shocking tins is a complicated story. does not work currently)
chgtm(RN,ROW,RR1,C,SIM)        point change in tariffs
chgtq(RR,C,RN,SIM)             point change in commodity tax
chgtva(RR,A,RN,SIM)            point change in value added tax
chgtwa(RR,F,RR1,A,SIM)         point change in water tax

FCLOSLOAD(F,*)                 factor closure rules
OTHCLOS(CLS)                     other closure rules
;

$onUNDF
*$load tariff
$load yield watreq fclosload othclos
$load chgta chgte chgtf chgtins chgtm chgtq chgtva chgtwa

PARAMETERS

*- TMSIM(RN,ROW,RR1,C,SIM)  tariff rate for com c by sim

*-Closure variables that may be changed exogenously in simulations
*-The values defined for these parameters matter only if the related
*-variable is fixed
 FSAVSIM(SIM)               foreign savings by sim (FCU)
 IADJSIM(SIM)               investment adjustment factor by sim
 EXRSIM(SIM)                exchange rate by sim
 GSAVSIM(SIM)               government savings by sim
 TWASIM(RR,F,RR1,A,SIM)     water charge
 DrouSIM(RR,A,SIM)          yield
 WATREQSIM(RR,F,RR1,A,SIM)  water requirement
 QFSSIM(RR,F,SIM)           amount of water available

 taSIM(RR,A,RN,SIM)         rate of tax on producer gross output value
 teSIM(RR,C,RN,ROW,SIM)     rate of tax on exports of region R
 tfSIM(RR,F,RN,SIM)         rate of direct tax on factors (soc sec tax) in region R
 tinsSIM(RR,INS,RN,SIM)     rate of direct tax on domestic institutions ins of region R
 tmSIM(RN,ROW,RR1,C,SIM)    rate of import tariff paid to gov of region R1
 tqSIM(RR,C,RN,SIM)         rate of sales tax in region R paid to gov of region R1
 tvaSIM(RR,A,RN,SIM)        rate of value-added tax paid by act A of region R
 twaSIM(RR,F,RR1,A,SIM)     rate of water charge on water quantity in region R
;

*-Unless specified otherwise below, base values are used
*- TMSIM(RN,ROW,RR1,C,SIM)    = tm0(RN,ROW,RR1,C);
 FSAVSIM(SIM)               = TFSAV0;
 IADJSIM(SIM)               = IADJ0;
 GSAVSIM(SIM)               = GSAV0;
 DrouSIM(RR,A,SIM)          = alphava0(RR,A) ;
 WATREQSIM(RR,F2N,RR1,A,SIM)  = nu(RR,F2N,RR1,A);
 QFSSIM(RR,F,SIM)           = QFS0(RR,F);

taSIM(RR,A,RN,SIM)          = ta0(RR,A,RN)     ;
teSIM(RR,C,RN,ROW,SIM)      = te0(RR,C,RN,ROW) ;
tfSIM(RR,F,RN,SIM)          = tf0(RR,F,RN)     ;
tinsSIM(RR,INS,RN,SIM)      = tins0(RR,INS,RN) ;
tmSIM(RN,ROW,RR1,C,SIM)     = tm0(RN,ROW,RR1,C);
tqSIM(RR,C,RN,SIM)          = tq0(RR,C,RN)     ;
tvaSIM(RR,A,RN,SIM)         = tva0(RR,A,RN)    ;
twaSIM(RR,F,RR1,A,SIM)      = twa0(RR,F,RR1,A) ;


*-climate change scenario
 DrouSIM(RR,A,SIM)$yield(RR,A,SIM) = yield(RR,A,SIM)*alphava0(RR,A) ;
 WATREQSIM(RR,F2N,RR1,A,SIM)$WATREQ(RR,F2N,RR1,A,SIM) = nu(RR,F2N,RR1,A)*WATREQ(RR,F2N,RR1,A,SIM);

*change in tax rates

 taSIM(RR,A,RN,SIM)      =   ta0(RR,A,RN)          + chgta  (RR,A,RN,SIM)       ;
 teSIM(RR,C,RN,ROW,SIM)  =   te0(RR,C,RN,ROW)      + chgte  (RR,C,RN,ROW,SIM)   ;
 tfSIM(RR,F,RN,SIM)      =   tf0(RR,F,RN)          + chgtf  (RR,F,RN,SIM)       ;
 tinsSIM(RR,INS,RN,SIM)  =   tins0(RR,INS,RN)      + chgtins(RR,INS,RN,SIM)     ;
 tmSIM(RN,ROW,RR1,C,SIM) =   tm0(RN,ROW,RR1,C)     + chgtm  (RN,ROW,RR1,C,SIM)  ;
 tqSIM(RR,C,RN,SIM)      =   tq0(RR,C,RN)          + chgtq  (RR,C,RN,SIM)       ;
 tvaSIM(RR,A,RN,SIM)     =   tva0(RR,A,RN)         + chgtva (RR,A,RN,SIM)       ;
 twaSIM(RR,F,RR1,A,SIM)  =   twa0(RR,F,RR1,A)      + chgtwa (RR,F,RR1,A,SIM)    ;

*-END: DEFINING EXPERIMENT PARAMETERS=================================
*-3. DEFINING NUMERAIRE AND CLOSURES FOR MACRO SYSTEM CONSTRAINTS=====

*-The selection of values for NUMERAIRE, SICLOS, ROWCLOS, and GOVCLOS
*-in this section determines the selection of variables that are fixed
*-and flexible inside the solution LOOP.

PARAMETERS

 NUMERAIRE(SIM)         numeraire
*-NUMERAIRE = 1 ---> CPI is numeraire (and fixed) -- DPI is flexible
*-NUMERAIRE = 2 ---> DPI is numeraire (and fixed) -- CPI is flexible
*-Note that a fixed exchange rate value or any nominal value or price that is
*-fixed in domestic currency is implicitly indexed to the numeraire.

 SICLOS(SIM) value for savings-investment closure
*-SICLOS = 1 ---> inv-driven sav -- uniform mps rate point change for
*-                selected ins
*-SICLOS = 2 ---> inv-driven sav -- scaled mps for for selected ins
*-SICLOS = 3 ---> inv is sav-driven
*-SICLOS = 4 ---> inv is fixed abs share
*-                - uniform mps rate point change (cf. 1)
*-SICLOS = 5 ---> inv is fixed abs share - scaled mps (cf. 2)
*-Note: SICLOS 4 and 5 are examples of balanced closures.

 ROWCLOS(SIM) value for rest-of-world closure
*-ROWCLOS = 1 ---> exch rate is flexible, for savings are fixed
*-ROWCLOS = 2 ---> exch rate is fixed   , for savings are flexible

 GOVCLOS(SIM) value for government closure
*-GOVCLOS = 1 ---> gov savings are flexible, dir tax rate is fixed
*-GOVCLOS = 2 ---> gov savings are fixed   ,
*-                 uniform dir tax rate point change for selected ins
*-GOVCLOS = 3 ---> gov savings are fixed   ,
*-                 scaled dir tax rate for selected institutions

 MPS01SIM(RR,INS,SIM)   0-1 par for potential flexing of savings rates
 TINS01SIM(RR,INS,SIM)  0-1 par for potential flexing of dir tax rates
 ;


*-NUMERAIRE, SICLOS, ROWCLOS, and GOVCLOS are set at default values

 NUMERAIRE(SIM)  = OTHCLOS('NUMER');
 SICLOS(SIM)     = OTHCLOS('SICCLS');
 ROWCLOS(SIM)    = OTHCLOS('ROWCLS');
 GOVCLOS(SIM)    = OTHCLOS('GOVCLS');

$ONTEXT
For closures with flexible savings or direct tax rates, the default is
that the rates of all domestic non-government institutions adjust. If
you deviate from the default, make sure that, for both parameters and
for simulations using the indicated closures, the value of at least one
element in INSDNG is unity. (If not, the parameters ERRMPS01 and
ERRTINS01 will generate errors.)
$OFFTEXT

 MPS01SIM(RR,INSDNG,SIM)   = 1;
 TINS01SIM(RR,INSDNG,SIM)  = 1;

*-Here: overwrite values for NUMERAIRE, SICLOS, ROWCLOS, and GOVCLOS for
*-selected simulations

*- GOVCLOS('TARCUT1') = 1;
*- GOVCLOS('TARCUT2') = 2;

*-If instutional tax is changed, then gov clos 1 or 2 should be used.
* GOVCLOS('INSSHCK') = 1;

PARAMETERS
 ERRMPS01(SIM)  UNDF if MPS01SIM not unity for at least one INSDNG
*-In order for SICLOS 1, 2, 4 and 5 to work, MPS01SIM has to have a value of
*-unity for at least one element in INSDNG for every SIM. If this is not
*-the case, an error is generated. Solution: change the definition if
*-MPS01SIM.
 ERRTINS01(SIM)  UNDF if TINS01SIM not unity for at least one INSDNG
*-In order for GOVCLOS 2 and 3 to work, TINS01SIM has to have a value of
*-unity for at least one element in INSDNG for every SIM. If this is not
*-the case, an error is generated. Solution: change the definition if
*-TINS01SIM.
 ;

 ERRMPS01(SIM)$(((SICLOS(SIM) EQ 1) OR (SICLOS(SIM) EQ 2) OR (SICLOS(SIM) EQ 4) OR (SICLOS(SIM) EQ 5))
  AND (SUM((RR,INSDNG)$(MPS01SIM(RR,INSDNG,SIM) EQ 1), 1) LT 1)) = 1/0;

 ERRTINS01(SIM)$(((GOVCLOS(SIM) EQ 2) OR (GOVCLOS(SIM) EQ 3))
  AND (SUM((RR,INSDNG)$(TINS01SIM(RR,INSDNG,SIM) EQ 1), 1) LT 1)) = 1/0;
 ;

PARAMETER
 ERRMPSEQ1(RR,INS,SIM) UNDF error if institution with MPS at unity has flexible MPS
$ONTEXT
Savings-investment closures that involve adjustments of the MPS of an
element in INSDNG for which mpsbar is at (or very close) to unity will
not work (since the total spending of such an institution will deviate
from its total income whenever the MPS changes). The user should review
the SAM and other data sources to assess whether an mpsbar of unity is
plausible and change the SAM if it is not. If it is plausible, it would
be necessary to give such an institution a value of zero for MPS01SIM
under savings-investment closures with a flexible MPS for one or more
other institutions.

This parameter is first defined in REPLOOP.INC and redefined after the
LOOP in a manner that generates an UNDF error if the error is present.

$OFFTEXT

*-END: DEFINING CLOSURES FOR MACRO SYSTEM CONSTRAINTS=================
*-4. DEFINING CLOSURES FOR FACTOR MARKETS=============================

*-Inside the solution loop, the selection of values for FMOBFE, FACTFE,
*-and FMOBUE determine the selection of variables that are fixed
*-and flexible.

SET
FCLS   Factor Closure sets
/
FNOCLS
FMOBFE
FACTFE
FMOBUE
/
;

PARAMETERS
 FCLOS(RR,F,FCLS,SIM)  Factor Closure rules
  ;

*-Default specification of factor market closures
*-Note that, for every simulation, every factor should have a non-zero
*-value for exactly one of the three parameters.

 FCLOS(RR,F,FCLS,SIM)=0;

 FCLOS(RR,F,FCLS,SIM)$FCLOSLOAD(F,FCLS) =  FCLOSLOAD(F,FCLS);

*-If no value is specified for a factor, impose FMOBFE:
 FCLOS(RR,F,'FMOBFE',SIM)$(SUM(FCLS,FCLOS(RR,F,FCLS,SIM)) EQ 0) = 1;

*-Here: overwrite values for FMOBFE, FACTFE, and FMOBUE for selected
*-factors and simulations

* FCLOS('TR7','FCAP','FMOBUE',SIM)=1;

*-Checking for errors in factor market closures

PARAMETER
 ERRFCLOS(RR,F,SIM) UNDF error if not exactly one closure by f and sim
*-Error if any factor f, in each simulation, does not have a non-zero
*-value for exactly one of the three parameters FMOBFE, FACTFE, FMOBUE.
 ;
 ERRFCLOS(RR,F,SIM)$(abs(SUM(FCLS,FCLOS(RR,F,FCLS,SIM)-1) GT 1.0E-6)) = 1/0;
* ERRFCLOS(RR,F,SIM)=SUM(FCLS,FCLOS(RR,F,FCLS,SIM));
*-5. REPORT SETUP=================================================

$INCLUDE 61REPSETUP.INC

*-====================================================================
*-6. L O O P==========================================================
*-====================================================================
LOOP(SIMCUR,

*-This include statement is optional. It may facilitate solver
*-performance by providing better starting point.
IF(SIMBASINIT(SIMCUR),

*-Initialization of variables.

  CPI.L                     = CPI0;
  DMPS.L                    = DMPS0;
  DPI.L                     = DPI0;
  DTINS.L                   = DTINS0;
  EG.L(RN)                  = EG0(RN);
  EH.L(RR,H)                = EH0(RR,H);
  EXR.L(ROW)                = EXR0(ROW);
  TFSAV.L                   = TFSAV0;
  FSAV.L(ROW)               = FSAV0(ROW);
  GADJ.L                    = GADJ0;
  GOVSHR.L                  = GOVSHR0;
  GSAV.L                    = GSAV0;
  IADJ.L                    = IADJ0;
  INVSHR.L                  = INVSHR0;
  MPS.L(RR,INSDNG)          = MPS0(RR,INSDNG);
  MPSADJ.L                  = MPSADJ0;
  PA.L(RR,A)                = PA0(RR,A);
  PDD.L(RR,C)               = PDD0(RR,C);
  PDS.L(RR,C)               = PDS0(RR,C);
  PINTA.L(RR,A)             = PINTA0(RR,A) ;
  PE.L(RR,C,ROW)            = PE0(RR,C,ROW);
  PM.L(ROW,RR,C)            = PM0(ROW,RR,C);
  PQ.L(RR,C)                = PQ0(RR,C);
  PVA.L(RR,A)               = PVA0(RR,A);
  PWE.L(RR,C,ROW)           = PWE0(RR,C,ROW);
  PWM.L(ROW,RR,C)           = PWM0(ROW,RR,C);
  PX.L(RR,C)                = PX0(RR,C);
  PXAC.L(RR,A,RR1,C)        = PXAC0(RR,A,RR1,C);
  QA.L(RR,A)                = QA0(RR,A);
  QD.L(RR,C)                = QD0(RR,C);
  QE.L(RR,C,ROW)            = QE0(RR,C,ROW);
  QF.L(RR,F,RR1,A)          = QF0(RR,F,RR1,A);
  QFS.L(RR,F)               = QFS0(RR,F);
  QG.L(RR,C,RN)             = QG0(RR,C,RN);
  QH.L(RR,C,RR1,H)          = QH0(RR,C,RR1,H);
  QHA.L(RR,A,RR1,C,RR2,H)   = QHA0(RR,A,RR1,C,RR2,H);
  QINT.L(RR,C,RR1,A)        = QINT0(RR,C,RR1,A);
  QINTA.L(RR,A)             = QINTA0(RR,A) ;
  QINV.L(RR,C)              = QINV0(RR,C);
  QM.L(ROW,RR,C)            = QM0(ROW,RR,C);
  QQ.L(RR,C)                = QQ0(RR,C);
  QT.L(RR,C)                = QT0(RR,C);
  QVA.L(RR,A)               = QVA0(RR,A);
  QX.L(RR,C)                = QX0(RR,C);
  QXAC.L(RR,A,RR1,C)        = QXAC0(RR,A,RR1,C);
  TABS.L                    = TABS0;
  TRII.L(RR,INSDNG,RR1,INSDNG1)   = TRII0(RR,INSDNG,RR1,INSDNG1);
  TINS.L(RR,INSDNG,RN)      = TINS0(RR,INSDNG,RN);
  TINSADJ.L                 = TINSADJ0;
  WALRAS.L                  = WALRAS0;
  WALRASSQR.L               = 0 ;
  WF.L(F)                   = WF0(F);
  WFR.L(RR,F)               = WFR0(RR,F);
  WFDIST.L(RR,F,RR1,A)      = WFDIST0(RR,F,RR1,A);
  YF.L(RR,F)                = YF0(RR,F);
  YG.L(RN)                  = YG0(RN);
  YI.L(RR,INS)              = YI0(RR,INS);
  YIF.L(RR,INS,RR1,F)       = YIF0(RR,INS,RR1,F);
  WPAY.L(RR)                = WPAY0(RR)  ;

*-IMPOSING PARAMETER  & FIXED VARIABLE VALUES FOR EXPERIMENTS=========
*-In this section, changes in experiment parameters and fixed
*-variables are imposed, except for changes in fixed variables related to
*-closures, which are handled in the next section.

*- tm(RN,ROW,RR1,C)   = TMSIM(RN,ROW,RR1,C,SIMCUR);
 TFSAV.FX                       = FSAVSIM(SIMCUR);
 alphava(RR,A)          = DrouSIM(RR,A,SIMCUR);
 nu(RR,F2N,RR1,A)      = WATREQSIM(RR,F2N,RR1,A,SIMCUR);


 ta(RR,A,RN)      =  taSIM(RR,A,RN,SIMCUR)      ;
 te(RR,C,RN,ROW)  =  teSIM(RR,C,RN,ROW,SIMCUR)  ;
 tf(RR,F,RN)      =      tfSIM(RR,F,RN,SIMCUR)      ;
 tins.L(RR,INS,RN)  =  tinsSIM(RR,INS,RN,SIMCUR)  ;
 tm(RN,ROW,RR1,C) =  tmSIM(RN,ROW,RR1,C,SIMCUR) ;
 tq(RR,C,RN)      =  tqSIM(RR,C,RN,SIMCUR)      ;
 tva(RR,A,RN)     =  tvaSIM(RR,A,RN,SIMCUR)     ;
 twa(RR,F,RR1,A)  =  twaSIM(RR,F,RR1,A,SIMCUR)  ;



*-IMPOSING CLOSURES FOR SYSTEM CONSTRAINTS============================

*-Selecting institutions with potentially flexible savings and direct
*-tax rates. This setting only matters for some of the alternative
*-savings-investment and government closures.
 mps01(RR,INSDNG)  = MPS01SIM(RR,INSDNG,SIMCUR);
 tins01(RR,INSDNG) = TINS01SIM(RR,INSDNG,SIMCUR);

*-------------
IF(NUMERAIRE(SIMCUR) EQ 1,
 CPI.FX = CPI0;
 DPI.LO = -INF;
 DPI.UP = +INF;
 DPI.L  = DPI0;  );

IF(NUMERAIRE(SIMCUR) EQ 2,
 DPI.FX = DPI0;
 CPI.LO = -INF;
 CPI.UP = +INF;
 CPI.L  = CPI0;  );

*-------------


IF(SICLOS(SIMCUR) EQ 1,
*-Investment-driven savings
*-Uniform MPS rate point change for selected ins
*-Fixed investment demand quantity adjustment factors
*-Flexible absorption shares for investment demand
 MPSADJ.FX = MPSADJ0;
 DMPS.LO = -INF;
 DMPS.UP = +INF;
 DMPS.L = DMPS0;
 IADJ.FX       = IADJSIM(SIMCUR);
 INVSHR.LO = -INF; INVSHR.UP = +INF; INVSHR.L = INVSHR0;
*-Fixed government demand quantity adjustment factors
*-Flexible absorption share for government demand
 GADJ.FX   = GADJ0;
 GOVSHR.LO = -INF;
 GOVSHR.UP = +INF;
 GOVSHR.L = GOVSHR0;
 );


IF(SICLOS(SIMCUR) EQ 2,
*-Investment-driven savings
*-Scaled MPS for selected institutions
*-Fixed investment demand quantity adjustment factors
*-Flexible absorption shares for investment demand
 MPSADJ.LO = -INF;
 MPSADJ.UP = +INF;
 MPSADJ.L = MPSADJ0;
 DMPS.FX = DMPS0;
 IADJ.FX = IADJSIM(SIMCUR);
 INVSHR.LO = -INF;
 INVSHR.UP = +INF;
 INVSHR.L = INVSHR0;
*-Fixed government demand quantity adjustment factors
*-Flexible absorption share for government demand
 GADJ.FX    = GADJ0;
 GOVSHR.LO = -INF;
 GOVSHR.UP = +INF;
 GOVSHR.L = GOVSHR0;
 );

IF(SICLOS(SIMCUR) EQ 3,
*-Savings-driven investment
*-Fixed marginal savings propensities
*-Flexible investment demand quantity adjustment factors
*-Flexible absorption shares for investment demand
 MPSADJ.FX = MPSADJ0;
 DMPS.FX = DMPS0;

 IADJ.LO = -INF;
 IADJ.UP = +INF;
 IADJ.L = IADJ0;
 INVSHR.LO = -INF;
 INVSHR.UP = +INF;
 INVSHR.L = INVSHR0;
*-Fixed government demand quantity adjustment factors
*-Flexible absorption share for government demand
 GADJ.FX    = GADJ0;
 GOVSHR.LO = -INF;
 GOVSHR.UP = +INF;
 GOVSHR.L = GOVSHR0;
 );

IF(SICLOS(SIMCUR) EQ 4,
*-Balanced closure.
*-Uniform MPS rate point change for selected ins
*-Flexible investment demand quantity adjustment factors
*-Fixed absorption shares for investment demand
 MPSADJ.FX = MPSADJ0;
 DMPS.LO = -INF;
 DMPS.UP = +INF;
 DMPS.L = DMPS0;

 IADJ.LO = -INF;
 IADJ.UP = +INF;
 IADJ.L = IADJ0;
 INVSHR.FX = INVSHR0;
*-Flexible government demand quantity adjustment factors
*-Fixed absorption share for government demand
 GADJ.LO = -INF;
 GADJ.UP = +INF;
 GADJ.L = GADJ0;
 GOVSHR.FX = GOVSHR0;
 );

IF(SICLOS(SIMCUR) EQ 5,
*-Balanced closure.
*-Scaled MPS for selected institutions
*-Flexible investment demand quantity adjustment factors
*-Fixed absorption shares for investment demand
 MPSADJ.LO = -INF;
 MPSADJ.UP = +INF;
 MPSADJ.L = MPSADJ0;
 DMPS.FX = DMPS0;
 IADJ.LO = -INF;
 IADJ.UP = +INF;
 IADJ.L = IADJ0;
 INVSHR.FX = INVSHR0;
*-Flexible government demand quantity adjustment factors
*-Fixed absorption share for government demand
 GADJ.LO = -INF;
 GADJ.UP = +INF;
 GADJ.L = GADJ0;
 GOVSHR.FX = GOVSHR0;
 );

*-------------

IF(GOVCLOS(SIMCUR) EQ 1,
*-Fixed direct tax rates
*-Flexible government savings
 TINSADJ.FX = TINSADJ0;
 DTINS.FX = DTINS0;
 GSAV.LO = -INF;
 GSAV.UP = +INF;
 GSAV.L = GSAV0;
 );

IF(GOVCLOS(SIMCUR) EQ 2,
*-Uniform direct tax rate point change for selected institutions
*-Fixed government savings
 TINSADJ.FX = TINSADJ0;
 DTINS.LO = -INF;
 DTINS.UP = +INF;
 DTINS.L = DTINS0;
 GSAV.FX = GSAVSIM(SIMCUR);
 );

IF(GOVCLOS(SIMCUR) EQ 3,
*-Scaled direct tax rates for selected institutions
*-Fixed government savings
 TINSADJ.LO = -INF;
 TINSADJ.UP = +INF;
 TINSADJ.L = TINSADJ0;
 DTINS.FX = DTINS0;
 GSAV.FX = GSAVSIM(SIMCUR);
 );

*-------------

IF(ROWCLOS(SIMCUR) EQ 1,
*-Fixed foreign savings -- flexible exchange rate
* TFSAV.FX = FSAVSIM(SIMCUR);

 FSAV.FX(ROW)      = FSAV0(ROW);
 EXR.LO(ROW)  = -INF; EXR.UP(ROW)  = +INF; EXR.L(ROW)   = EXR0(ROW);
 );

IF(ROWCLOS(SIMCUR) EQ 2,
*-Fixed exchange rate -- flexible foreign savings
 EXR.FX(ROW) = EXR0(ROW);
  TFSAV.LO = -INF;
  TFSAV.UP = +INF;
  TFSAV.L = TFSAV0;
  FSAV.LO(ROW)      = -INF;
  FSAV.UP(ROW)      = +INF;
  FSAV.L(ROW)      = FSAV0(ROW);
 );

*----------------------------------------------------------------------
*-Loop over all factors for alternative factor-market closures.
LOOP((RR,F),

*---
*-Factors with FMOBFE(F,SIMCUR) = 1 are fully employed and mobile between
*-activities. WF(F) is the market-clearing variable each factor.



*-IF(FNOCLS(RR,F,SIMCUR) EQ 1,
IF(FCLOS(RR,F,'FNOCLS',SIMCUR) EQ 1,

  WFR.LO(RR,F)        = -inf;
  WFR.UP(RR,F)        = +inf;
  WFR.L(RR,F)         = WFR0(RR,F);

  WFDIST.FX(RR,F,RR1,A)  = WFDIST0(RR,F,RR1,A);

  QF.LO(RR,F,RR1,A)$QF0(RR,F,RR1,A) = -INF;
  QF.UP(RR,F,RR1,A)$QF0(RR,F,RR1,A) = +INF;
  QF.L(RR,F,RR1,A)$QF0(RR,F,RR1,A)  = QF0(RR,F,RR1,A);

  QFS.LO(RR,F)           = -INF;
  QFS.UP(RR,F)           = +INF;
  QFS.L(RR,F)            = QFS0(RR,F);

);

*-IF(FMOBFE(RR,F,SIMCUR) EQ 1,
IF(FCLOS(RR,F,'FMOBFE',SIMCUR) EQ 1,

*- WFDIST.FX(F,A)      = WFDIST0(F,A);
 QFS.FX(RR,F)           = QFSSIM(RR,F,SIMCUR);
*-to shock water shadow price we do this!!! other wise use the above option.
 wfdist.FX(RR,F,RR1,A)   = WFDIST0(RR,F,RR1,A);


 WFR.LO(RR,F)            = -INF;
 WFR.UP(RR,F)            = +INF;
 WFR.L(RR,F)             = WFR0(RR,F);

 QF.LO(RR,F,RR1,A)$QF0(RR,F,RR1,A) = -INF;
 QF.UP(RR,F,RR1,A)$QF0(RR,F,RR1,A) = +INF;
 QF.L(RR,F,RR1,A)$QF0(RR,F,RR1,A)  = QF0(RR,F,RR1,A); );

*---
*-Factors with FACTFE(F,SIMCUR) = 1 are fully employed and
*-activity-specific. WFDIST(F,A) is the clearing variable, one for each
*-segment of the factor market.

*-IF(FACTFE(RR,F,SIMCUR) EQ 1,
IF(FCLOS(RR,F,'FACTFE',SIMCUR) EQ 1,

 WFR.FX(RR,F)                = WFR0(RR,F);
 QF.FX(RR,F,RR1,A)              = QF0(RR,F,RR1,A);

 WFDIST.LO(RR,F,RR1,A)$QF0(RR,F,RR1,A) = -INF;
 WFDIST.UP(RR,F,RR1,A)$QF0(RR,F,RR1,A) = +INF;
 WFDIST.L(RR,F,RR1,A)$QF0(RR,F,RR1,A)  = WFDIST0(RR,F,RR1,A);

 QFS.LO(RR,F)               = -INF;
 QFS.UP(RR,F)               = +INF;
 QFS.L(RR,F)                = QFS0(RR,F); );

*---
*-Factors with FMOBUE(F,SIMCUR) = 1 are unemployed and mobile. For each
*-activity, the wage, WFDIST(F,A)*WF(F), is fixed. QFS(F) is the
*-market-clearing variable for the unified labor market.

*-IF(FMOBUE(RR,F,SIMCUR) EQ 1,
IF(FCLOS(RR,F,'FMOBUE',SIMCUR) EQ 1,

 WFDIST.FX(RR,F,RR1,A)      = WFDIST0(RR,F,RR1,A);
 WFR.FX(RR,F)            = WFR0(RR,F);

 QF.LO(RR,F,RR1,A)$QF0(RR,F,RR1,A) = -INF;
 QF.UP(RR,F,RR1,A)$QF0(RR,F,RR1,A) = +INF;
 QF.L(RR,F,RR1,A)$QF0(RR,F,RR1,A)  = QF0(RR,F,RR1,A);

 QFS.LO(RR,F)           = -INF;
 QFS.UP(RR,F)           = +INF;
 QFS.L(RR,F)            = QFS0(RR,F);
 );
*---

);


*-End loop for factor-market closures.

*-SOLVING=============================================================

IF(SIMMCP(SIMCUR),
 SOLVE STANDCGE USING MCP;
ELSE
 SOLVE NLPCGE MINIMIZING WALRASSQR USING NLP;
 );

*-DEFINING REPORT VALUES FOR SIMCUR===================================

$BATINCLUDE 62REPLOOP.INC SIMCUR

*-====================================================================
*-END: L O O P========================================================
*-====================================================================
)
);
*-====================================================================
*-END: L O O P========================================================
*-====================================================================

*-7. COMPUTING PERCENTAGE CHANGE FOR REPORT PARAMETERS================

$INCLUDE 63REPPERC.INC

*-8. CHECKING FOR ERRORS IN SOLUTION AND REPORT PARAMETERS============


*-If error in the negative price/quantity segment, one or more solution prices
*-or quantities are negative.

NEGPWARNDOM(RR,C,SIM)$
 ((PDDX(RR,C,SIM) LT 0) OR (PDSX(RR,C,SIM) LT 0) OR
  (PQX(RR,C,SIM) LT 0)  OR (PXX(RR,C,SIM) LT 0) OR
  (SMIN((RR1,A), PXACX(RR1,A,RR,C,SIM)) LT 0) )
  = 1/0;

NEGPWARNROW(RR,C,ROW,SIM)$
  ((PEX(RR,C,ROW,SIM)  LT 0) OR (PMX(ROW,RR,C,SIM) LT 0)  OR
 (PWEX(RR,C,ROW,SIM) LT 0) OR
  (PWMX(ROW,RR,C,SIM) LT 0))
  = 1/0;


NEGQWARNDOM(RR,C,SIM)$
   ((QDX(RR,C,SIM) LT 0) OR (QQX(RR,C,SIM) LT 0)
   OR (QXX(RR,C,SIM) LT 0)      OR  (SMIN((RR1,A), QXACX(RR1,A,RR,C,SIM)) LT 0))  = 1/0;

NEGQWARNROW(RR,C,ROW,SIM)$
   ((QEX(RR,C,ROW,SIM)  LT 0) OR (QMX(ROW,RR,C,SIM) LT 0)) = 1/0;

NEGWFWARN(RR,F,SIM)$
 ((WFRX(RR,F,SIM) LT 0) OR (SMIN((RR1,A), WFDISTX(RR,F,RR1,A,SIM)) LT 0) )
  = 1/0;

NEGQFWARN(RR,F,SIM)$
 ((QFSX(RR,F,SIM) LT 0) OR (SMIN((RR1,A), QFX(RR,F,RR1,A,SIM)) LT 0) )
  = 1/0;


*-If error here, check displays of SAMBUDGAP and GDPGAP to find it and
*-fix it.
 NGAPWARN(SIM)
  $(SUM(SAC, NSAMBUDGAP(SAC,'TOTAL',SIM)) + NGDPGAP(SIM) GT 0.01) = 1/0;

*-If error here, SOLVEREP reports illegal values.
 SOLVEWARN(SOLVEIND,MODTYPE,SIM)
  $(SOLVEREP(SOLVEIND,MODTYPE,SIM) GT SOLVEMAX(SOLVEIND,MODTYPE)) = 1/0;

*-If error here, check displays of SAMBUDGAP for hints about the source
 WALRASWARN(SIM)$(ABS(WALRASX(SIM)) GT 0.001) = 1/0;

*-If error here, check explanation above where ERRMPSEQ1 is declared.
 ERRMPSEQ1(RR,INSDNG,SIM)$ERRMPSEQ1(RR,INSDNG,SIM) = 1/0;


*-9. DISPLAYING REPORTS===============================================

OPTION
 QFX:3:1:1    , QHX:3:1:1      , QHAX:3:2:1, QINTX:3:1:1  , WFAX:3:1:1
 WFAX:3:1:1   , WFDISTX:3:1:1  , YFX:3:1:1

 QFXP:3:1:1   , QHXP:3:1:1     , QHAXP:3:2:1, QINTXP:3:1:1, WFAXP:3:1:1
 WFAXP:3:1:1  , WFDISTXP:3:1:1 , YFXP:3:1:1

 NSAMBUD:3:1:1 , NSAMBUDP:3:1:1
 NGDPTAB1:3:1:1, NGDPTAB1P:3:1:1
 NGDPTAB2:3:1:1, NGDPTAB2P:3:1:1
 MACCLOS:0,     FACCLOS:0:1:1
 SOLVEREP:0:1:1
 ;

$INCLUDE 64REPSUM.INC

$ONTEXT
"The parameters MACCLOS and FACCLOS, and the sets ACES and ALEO"
"indicate the values for model features with user choice."

 MACCLOS
 "For GOV"
 "  1 -> gov savings are flexible, dir tax rate is fixed"
 "  2 -> gov savings are fixed"
 "       uniform dir tax rate point chng for selected ins"
 "  3 -> gov savings are fixed"
 "       scaled dir tax rate for selected institutions"
 ""
 "For ROW"
 "  1 -> exch rate is flexible, for savings are fixed"
 "  2 -> exch rate is fixed   , for savings are flexible"
 ""
 "For SAVINV"
  "1 -> inv-driven sav -- uniform mps rate point chng for selected ins"
  "2 -> inv-driven sav -- scaled mps for selected ins"
  "3 -> inv is sav-driven"
  "4 -> inv is fixed abs share - uniform mps rate chng (cf. 1)"
  "5 -> inv is fixed abs share - scaled mps (cf. 2)"
 ""
 FACCLOS
 "FMOBFE = 1 -> mobile and fully employed"
 "FACTFE = 1 -> activity-specific and fully employed"
 "FMOBUE = 1 -> mobile and unemployed"

 ACES
 "Activities in ACES have a CES aggregation function at the top of"
 "the technology nest."

 ALEO
 "Activities in ALEO have a Leontief aggregation function at the top of"
 "the technology nest."

 SIMBASINIT
 "For simulations in SIMBASINIT, the variables are initialized at"
 "base levels."

SAMBUDGAP , GDPGAP  ,

SOLVEREP

"MODEL STATUS:"
" 1 OPTIMAL"
" 2 LOCALLY OPTIMAL"
" 3 UNBOUNDED"
" 4 INFEASIBLE"
" 5 LOCALLY INFEASIBLE"
" 6 INTERMEDIATE INFEASIBLE"
" 7 INTERMEDIATE NON-OPTIMAL"
" 8 INTEGER SOLUTION"
" 9 INTERMEDIATE NON-INTEGER"
"10 INTEGER INFEASIBLE"
"11 (UNUSED)"
"12 ERROR UNKNOWN"
"13 ERROR NO SOLUTION"
""
"SOLVER STATUS:"
" 1 NORMAL COMPLETION"
" 2 ITERATION INTERRUPT"
" 3 RESOURCE INTERRUPT"
" 4 TERMINATED BY SOLVER"
" 5 EVALUATION ERROR LIMIT"
" 6 UNKNOWN"
" 7 (UNUSED)"
" 8 ERROR PREPROCESSOR ERROR"
" 9 ERROR SETUP FAILURE"
"10 ERROR SOLVER FAILURE"
"11 ERROR INTERNAL SOLVER ERROR"
"12 ERROR POST-PROCESSOR ERROR"
"13 ERROR SYSTEM FAILURE"
""
"NUM-REDEFEQ shows the number of redefined equations (should be zero)"
""
SOLVEWARN
"If error(s) (UNDF), one or more SOLVEREP values are illegal."
""
NEGPWARN, NEGQWARN, NEGWFWARN, NEGQFWARN
"Negative prices and quantities are economically illegal"
""
GAPWARN, WALRASWARN,
""
ERRMPSEQ1
"See the explanation where ERRMPSEQ1 is declared."
;
$OFFTEXT
