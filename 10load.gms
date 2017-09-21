$TITLE Load Necessary Data for the model
$STITLE Turkish Agricultural CGE Model - Regional.
$STITLE Based on IFPRI Standard CGE modeling system, Version 1.01

$ONTEXT
*=============================================================================
* File      : 01work.gms
* Author    : Hasan Dudu (hasan@dudu.gen.tr)
* Version   : 1.0
* Date      : 15/08/2011 14:05:20
* Changed   : 13/12/2013 18:08:24
* Changed by: Hasan Dudu (hasan@dudu.gen.tr)
* Remarks   : This file loads the required data for model to run
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

$ONSYMLIST ONSYMXREF OFFUPPER
$OFFSYMLIST OFFSYMXREF
$ONEMPTY

*-  The dollar control option makes empty data initialization statements
*- permissible (e.g. sets without elements or parameters without data)


*- ------------------------------------------------------------------------------
*- 1. SET DECLARATIONS ----------------------------------------------------------
*- ------------------------------------------------------------------------------
$ONTEXT
In this section, all sets are declared. They are divided into the
following groups:
 a. model sets (appearing in the model equations)
 b. calibration sets (used to initialize variables and define model
   parameters)
 c. report sets (used in report files)
$OFFTEXT

SETS
*-a. model sets
 AC             global set for model accounts - aggregated microsam accounts
 RALL           all regions
 R(RALL)        Regions
 RN(R)          National region
 RTOT(RALL)     dummy region for SAM totals
 RR (R)         regions only
 ROW(AC)        trade partners
 AA(AC)         global accounts
 ACNT(AC)       all elements in AC except TOTAL
 A(AC)          activities
 ACES(RR,A)     activities with CES fn at top of technology nest
 ALEO(RR,A)     activities with Leontief fn at top of technology nest
 A2N(A)         activities with the second nest
 C(AC)          commodities
 CD(RR,C)       commodities with domestic sales of output in region R
 CDN(RR,C)      commodities without domestic sales of output
 CE(RR,C)       exported commodities
 CEN(RR,C)      non-export commodities
 CM(RR,C)       imported commodities
 CMN(RR,C)      non-imported commodities
 CER(RR,C,ROW)  exported commodities to region ROW
 CENR(RR,C,ROW) non-export commodities to region ROW
 CMR(RR,C,ROW)  imported commodities from region ROW
 CMNR(RR,C,ROW) non-imported commodities from region ROW
 CX(RR,C)       commodities with output
 F(AC)          factors
 FLAB(F)        labor
 FNLAB(F)       labor
 FLND(F)        land
 FCAP(F)        capital
 F1N(F)         factors at the first nest
 F2N(F)         factors at the second nest
 F21N(F)        factor from 2nd nest to 1st nest
 F22N(F2N)      factor only in 2nd nest
 FSUP(F)        factors of which supply decision is made by household
 INS(AC)        institutions
 INSD(INS)      domestic institutions
 INSDNG(INSD)   domestic non-government institutions
 H(INSDNG)      households
 EN(INSDNG)     enterprises
 FWAT(F)        Water factor
 FNWAT(F)       Non-water factors

*-b. calibration sets
 CINV(RR,C)      fixed investment goods
 CT(RR,C)          transaction service commodities
 CTD(AC)        domestic transactions cost account
 CTE(AC)        export transactions cost account
 CTM(AC)        import transactions cost account

*-c. report sets
 AAGR(A)        agricultural activities
 AMIN(A)        mining activities
 AIND(A)        industrial activities
 ASER(A)        service activities
 ANAGR(A)       non-agricultural activities
 CAGR(C)        agricultural commodities
 CMIN(C)        mining commodities
 CIND(C)        industrial commodities
 CSER(C)        service commodities
 CNAGR(C)       non-agricultural commodities
 HURB(H)        urban households
 HRUR(H)        rural households
 TX(AC)         taxes in the model
 EXTX(TX)       export taxes
 IMTX(TX)       import taxes
 COMTX(TX)      commodity taxes
 FTX(TX)        factor taxes
 ATX(TX)        activity taxes
 INSTX(TX)      institutional taxes
 VATX(TX)       value added taxes
 WTTX(TX)       water charges

*-d. Mapping sets

 MAPAC(A,C)         mapping between regional activity to commodity
 MAPCA(C,A)         mapping between regional commodity to activity
 MAPROWEXP(ROW,TX)  mapping between trading regions and export taxes
 MAPROWIMP(ROW,TX)  mapping between trading regions and import taxes
 MAPTAXSAM(AC,TX)   mapping from tax accounts to SAM accounts

*-simulation sets


ADCRP(A)       rainfed activities
AICRP(A)       irrigated activities
CSTA(C)        staple crops with incr. world price

*-ALIAS statement to create identical sets.
*-Alias creating rule: add consequtive integers starting from 1 to the end of the original set name.
ALIAS
 (AC,AC1)  , (ACNT,ACNT1), (A,A1,A2), (A2N,A2N1),
 (R,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10), (H,H1,H2,H3)
 (RTOT,RTOT1), (C,C1,C2), (F,F1), (F1N,F1N1), (F2N,F2N1),(F21N,F21N1),(INS,INS1), (INSD,INSD1),
 (INSDNG,INSDNG1), (ROW,ROW1), (TX,TX1), (RR,RR1,RR2,RR3,RR4,RR5,RR6),
 (RN,RN1,RN2,RN3,RN4,RN5,RN6,RN7,RN8,RN9,RN10), (RALL,RALL1,RALL2,RALL3)
 ;


*-------------------------------------------------------------------------------
*-2. DATABASE ------------------------------------------------------------------
*-------------------------------------------------------------------------------

PARAMETER
 SAM(RALL,AC,RALL1,AC1)            standard SAM
 SAMBALCHK(RALL,AC)          column minus row total for SAM

 ;

PARAMETER
 SCALE           SCALING PARAMETER FOR SAM /1000/
;

*--------------------------------------------------------------------------------
*-1. SET DEFINITIONS ------------------------------------------------------------
*--------------------------------------------------------------------------------

*-Read in set definitions from Excel file (01load.xls)

$call "gdxxrw i=xls\01load.xlsx o=gdx\set.gdx index=setIndex!a6"
$gdxin gdx\set.gdx

*-Load sets
$loaddc AC AA A A2N AAGR AMIN AIND ASER  C CAGR CMIN CIND CSER F FLAB FCAP FLND F1N F2N F21N F22N FSUP
$loaddc INS INSD RALL R RN RTOT RR TX EXTX IMTX COMTX FTX ATX INSTX VATX WTTX
$loaddc INSDNG H EN HURB HRUR CTD CTE CTM ROW FWAT FNWAT  CSTA  ADCRP AICRP

*-load maps
$loaddc MAPAC MAPCA MAPROWEXP MAPTAXSAM MAPROWIMP


 ACES(RR,A)                     = YES;
 ALEO(RR,A)$(NOT ACES(RR,A))    = YES;
 ANAGR(A)                       = NOT AAGR(A);
 CNAGR(C)                       = NOT CAGR(C);
 ACNT(AC)                       = YES;
 ACNT('TOTAL')                  = NO;
 FNLAB(F)$(NOT FLAB(F))         =YES;

*- NRFA(A)$(NOT RFA(A))           = YES;
*-------------------------------------------------------------------------------
*-2. SAM -----------------------------------------------------------------------
*-------------------------------------------------------------------------------
*-Model data

$call "gdxxrw i=xls\01load.xlsx o=gdx\param.gdx index=parIndex!a6"
$gdxin gdx\param.gdx

*-Load Social Accounting Matrix (SAM)
$loaddc SAM

*-Account totals are recomputed. Check for SAM balance.

 SAM(R,'TOTAL',R1,AC) = 0;
 SAM(R,AC,R1,'TOTAL') = 0;

 SAM('TOT','TOTAL',R1,AC) = SUM((ACNT,R), SAM(R,ACNT,R1,AC));
 SAM(R,AC,'TOT','TOTAL') = SUM((ACNT,R1), SAM(R,AC,R1,ACNT));


 SAMBALCHK(R,AC)   = SAM('TOT','TOTAL',R,AC) - SAM(R,AC,'TOT','TOTAL');

*-SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');


 DISPLAY "Read in SAM", SAMBALCHK;

 SAM(R,AC,R2,AC1) =  SAM(R,AC,R2,AC1) / SCALE;

*-If account balances exceed a critical maximum value, SAMBAL.INC will
*-balance the SAM exactly.
*-$INCLUDE SAMBAL.INC

*-Defining CINV using SAM data with potential user input.

$ONTEXT
*- In the model, there are two ways of separating stock changes from fixed
*- investment:
*-
*- a. If the SAM includes the account S-I (in its column making payments to
*- commodities for fixed investments and an aggregate payment to DSTK for
*- stock changes) and DSTK (in its row paid by S-I, in its column making
*- payments, positive or negative, to commodities, representing stock
*- change values), then no user action is needed -- the set CINV will by
*- default include all elements in C that receive payments from S-I.
*-
*- b. If, the account DSTK does not appear in the SAM, then the set CINV
*- should only include the commodities that receive payments from S-I for
*- fixed investment; commodities receiving payments for stock changes
*- should be excluded. The default is that all commodities receiving
*- payments are included; the user may exclude selected commodities from
*- CINV. (An example of how to do this is provided just below.)

$OFFTEXT

*-All commodities receiving payments from S-I are included in the set CINV.
*-Note: Negative payments are for stock changes and should be treated
*-as such.
 CINV(RR,C)$SAM(RR,C,'TR0','S-I') = YES;

*-!!- User option to exclude selected commodities from the set CINV. Only
*-relevant for SAMs without the account DSTK.
*-Example:
*-If the set C includes a commodity called CWHEAT and payments in the cell
*-SAM('CWHEAT','S-I') are for stock changes, the user should include
*-the following line in the program:
*- CINV('CWHEAT')  = NO;


*--------------------------------------------------------------------------------
*-3. ELASTICITIES ---------------------------------------------------------------
*--------------------------------------------------------------------------------

$ONTEXT
*- !!- In this section, the user inputs elasticities for trade, production,
*- and household consumption. If the user does not supply all required
*- data, missing data will be generated in STDMOD.GMS using simple
*- assumptions.
$OFFTEXT

*-Trade elasticities========================================

*-SIGMAQ is the elasticity of substitution between imports
*-and domestic output in domestic demand.
*-SIGMAT is the elasticity of transformation for domestic
*-marketed output between exports and domestic supplies.

SET
 TRDELAS  trade elasticities
 /
 SIGMAQ  Armington elasticity
 SIGMAT  CET elasticity
 /

 PRDELAS  production elasticities
 /
 PRODELAS
 PRODELAS2
 /
;

PARAMETER
 TRADELAS(RR,AC,TRDELAS)           Armington and CET elasticities by commodity
 PRODELAS(RR,A)                    Elas of substit bt. factors - bottom of technology nest
 PRODELASTAB(RR,A,PRDELAS)
 PRODELAS2(RR,A)                   Elas of substit bt. agg fac & intermed - top of tech nest
 ELASAC(RR,C)                      Output aggregation elasticity for commodity C
;

*-Load trade elasticities
*-$gdxin gdx\param.gdx

$loaddc TRADELAS PRODELASTAB

 PRODELAS(RR,A)     = PRODELASTAB(RR,A,'PRODELAS');
 PRODELAS2(RR,A)    = PRODELASTAB(RR,A,'PRODELAS2');
 ELASAC(RR,C)$SUM((RR1,A), SAM(RR1,A,RR,C))    = 5.0;

*-Household consumption elasticities -------------------------------
*-Note: The Frisch parameter is included in this section.

PARAMETERS
 LESELAS1(RR,H,RR1,C)           LES demand elasticities
 FRISCH(RR,H)                   Frisch parameter for household LES demand
 LESELAS2(RR,A,RR1,C,RR2,H)     Expe elasticity of home dem by com - act - hhd
 LABOR(RR,H,*)                  Labor Market parameters
 LFPAR0(RR,H)                   Labor force participation rate
 POP(RR,H)                      Population over age 15
 QFUE0(RR,F)                    Unemployment
;

$loaddc LESELAS1 LABOR

*-If LES elasticity missing in data then assign default value
 LESELAS1(RR,H,RR1,C)$(NOT LESELAS1(RR,H,RR1,C)) = 0.9;

 LESELAS2(RR,A,RR1,C,RR2,H) =  0.9;

 POP(RR,H)=LABOR(RR,H,'POP');

 LFPAR0(RR,H)=LABOR(RR,H,'LFPAR');

 QFUE0(RR,'FLAB')=LABOR(RR,'HHD','UNEMP');


*--------------------------------------------------------------------------------
*-4. PHYSICAL FACTOR QUANTITIES AND FACTOR MARKET STRUCTURES --------------------
*--------------------------------------------------------------------------------

$ONTEXT
*- !!: If you have data on physical factor DEMANDS, add them in this
*- section. If so, there is no need to include supply quantities --
*- the model code in STDMOD.GMS will not use the supply information.
*-
*- If data are provided for SUPPLY quantities (but not for demand), the
*- model code in STDMOD.GMS will define disaggregated activity demand
*- quantities for each factor as:
*- (total factor supply) TIMES (activity share in total activity payments
*- to the factor).
*- This amounts to assuming that, for each factor, wages are uniform across
*- activities. The data on factor payments are from the SAM.
*-
*- If you don't have any data on physical factor quantities, leave
*- this section blank.
$OFFTEXT

PARAMETER
*-initial employment numbers
 QFINPUT(RR,A,RR1,F)      sectoral employment data
 QFSBASE(AC)            total employment data
 QFRSBASE(RR,F)                total employment data
 QFBASE(RR,F,RR1,A)       sectoral employment
;

$loaddc QFINPUT
 QFBASE(RR,F,RR1,A) = QFINPUT(RR1,A,RR,F)/ SCALE;
*- QFBASE(RR,FWAT,A) = QFINPUT(RR,A,FWAT);
*- QFBASE(RR,FLND,A) = QFINPUT(RR,A,FLND);
 QFRSBASE(RR,F) = SUM((RR1,A), QFBASE(RR,F,RR1,A));
 QFSBASE(F)=SUM(RR,QFRSBASE(RR,F));

*-load quantity data
PARAMETER
QATABASE(RR,A)         ;
$loaddc QATABASE

QATABASE(RR,A)=QATABASE(RR,A)/SCALE;

*--------------------------------------------------------------------------------
*-5. COMMODITY VALUE SHARES FOR HOME CONSUMPTION --------------------------------
*--------------------------------------------------------------------------------

$ONTEXT
*- !!-User input is needed only if the SAM includes household home
*- consumption, reflected in payments from households to activities, and
*- if these activities produce multiple outputs.
*-
*- If this condition is met, the user should define the parameter SHRHOME,
*- using extraneous data on commodity value shares which, for each
*- household-activity combination, should sum to unity.
*-
*- In the absence of user input, the program will compute these value
*- shares in the file STDMOD.GMS, using the OUTPUT value shares for
*- each commodity. (For single-output activities, the computed shares
*- will quite correctly be at unity.)
*-
*- Note that elasticities are needed for the parameter LESELAS2(A,C,H)
*- (above in the elasticity section) for the identified combinations
$OFFTEXT

PARAMETER
 shrhome(RR,A,RR1,C,RR2,H) value share for commy c in home cons of hhd h from act a
 ;

*-!!: If needed, manually define shrhome.
 shrhome(RR,A,RR1,C,RR2,H) = 0;


*--------------------------------------------------------------------------------
*-6. INITIALIZATION OF TAX DATA -------------------------------------------------
*--------------------------------------------------------------------------------

$ONTEXT
!!: TAXPAR is used for model calibration. A proper definition of TAXPAR is necessary for
the functioning of the model if the SAM includes taxes. The names of the taxes in the SAM
should be as follows:

FACTAX: all taxes paid by factors (including social security payments)
IMPTAX: Tariffs
EXPTAX: taxes on exports
INSTAX: taxes on firms and households
ACTAX: taxes on activities
COMTAX: taxes on commodities (VAT etc...))

These tax accounts should exist in AC even if they are zero.

$OFFTEXT

PARAMETER
 TAXPAR(RN,TX,RR1,AC)   payment by account ac of region R1 to tax account tx of region of national region RN
 ;
 TAXPAR(RN,TX,RR1,AC)  = SAM(RN,TX,RR1,AC);

*- TAXPAR(R,TX,R1,INSD)  = SAM(R,INSD,R1,TX);


*-SAM adjustments ====================================================

*-In this section, some minor adjustments are made in the SAM (when
*-needed) to fit the model structure.

*-Adjustment for sectors with only exports and no domestic sales.
*-If there is a very small value for domestic sales, add the discrepancy
*-to exports.
 SAM(RR,C,RN,ROW)$(ABS(SUM((RR1,A), SAM(RR1,A,RR,C))
 - SUM(ROW1, SAM(RR,C,RN,ROW1) - sum(TX$MAPROWIMP(ROW1,TX),TAXPAR(RN,TX,RR,C))
 - SUM(CTE, SAM(RN,CTE,RR,C))) ) LT 1.E-6)= SUM((RR1,A), SAM(RR,A,RR1,C))
 - SUM(TX$MAPROWIMP(ROW,TX),TAXPAR(RN,TX,RR,C))
 - SUM(CTE, SAM(RN,CTE,RR,C)) ;

*-Netting transfers between domestic institutions and RoW.
 SAM(R,INSD,RN,ROW)   = SAM(R,INSD,RN,ROW) - SAM(RN,ROW,R,INSD);
 SAM(RN,ROW,R,INSD)   = 0;

*-Netting transfers between factors and RoW.
 SAM(RN,ROW,RR1,F)  = SAM(RN,ROW,RR1,F) - SAM(RR1,F,RN,ROW);
 SAM(RR,F,RN,ROW)  = 0;

*-Netting transfers between government and domestic non-
*-government institutions.
 SAM(RR,INSDNG,RN,'GOV') = SAM(RR,INSDNG,RN,'GOV') - SAM(RN,'GOV',RR,INSDNG);
 SAM(RN,'GOV',RR1,INSDNG) = 0;

*-Eliminating payments of any account to itself.
 SAM(R,ACNT,R,ACNT) = 0;

*-Checking SAM balance=================================================
*-Do NOT make any changes in the parameter SAM after this line!!!!!!!!!

*-Account totals are recomputed. Check for SAM balance.
*-row sum
 SAM('TOT','TOTAL',R1,ACNT1) = SUM((ACNT,R), SAM(R,ACNT,R1,ACNT1));

*-col sum
 SAM(R,ACNT,'TOT','TOTAL') = SUM((R1,ACNT1), SAM(R,ACNT,R1,ACNT1));

 SAMBALCHK(R,AC)   = SAM('TOT','TOTAL',R,AC) - SAM(R,AC,'TOT','TOTAL');

 DISPLAY "SAM after final adjustments", SAMBALCHK;

*-Additional set definitions based on country SAM======================

*-CD is the set for commodities with domestic sales of domestic output
*-i.e., for which (value of sales at producer prices)
*-              > (value of exports at producer prices)
 CD(RR,C)  = YES$(SUM((RR1,A), SAM(RR1,A,RR,C)) GT (SUM((R1,ROW),SAM(RR,C,R1,ROW))- sum((RN,EXTX),TAXPAR(RN,EXTX,RR,C)) - SUM((RR1,CTE), SAM(RR1,CTE,RR,C))) );

 CDN(RR,C) = NOT CD(RR,C);
*-$EXIT
 CE(RR,C)  = YES$SUM((R1,ROW),SAM(RR,C,R1,ROW));
 CEN(RR,C) = NOT CE(RR,C);

 CM(RR,C)  = YES$SUM((R1,ROW),SAM(R1,ROW,RR,C));
 CMN(RR,C) = NOT CM(RR,C);

*- define sets for regional exports and imports
 CER(RR,C,ROW)  = YES$SUM(R1,SAM(RR,C,R1,ROW));
 CENR(RR,C,ROW) = NOT CER(RR,C,ROW);

 CMR(RR,C,ROW)  = YES$SUM(R1,SAM(R1,ROW,RR,C));
 CMNR(RR,C,ROW) = NOT CMR(RR,C,ROW);

 CX(RR,C) = YES$SUM((RR1,A), SAM(RR1,A,RR,C));
 CT(RR,C)$(SUM((RR1,CTD), SAM(RR,C,RR1,CTD)) + SUM((RR1,CTE), SAM(RR,C,RR1,CTE))+ SUM((RR1,CTM), SAM(RR,C,RR1,CTM)))  = YES;

 ALEO(RR,A) = YES; ACES(RR,A) = NO;
*- ALEO(RR,A) = NO; ACES(RR,A) = YES;

*-If activity has no intermediate inputs, then Leontief function has to
*-be used at the top of the technology nest
 ACES(RR,A)$(NOT SUM((RR1,C), SAM(RR1,C,RR,A))) = NO;
 ALEO(RR,A)$(NOT ACES(RR,A)) = YES;

*-Fine-tuning non-SAM data============================================

*-Generating missing data for home consumption====
*-If SAM includes home consumption but NO data were provided for SHRHOME,
*-data are generated assuming that the value shares for home consumption
*-are identical to activity output value shares.

IF(SUM((RR,A,RR1,H), SAM(RR,A,RR1,H)) AND NOT SUM((RR,A,RR1,C,RR2,H), SHRHOME(RR,A,RR1,C,RR2,H)),

 SHRHOME(RR,A,RR1,C,RR2,H)$SAM(RR,A,RR2,H) = SAM(RR,A,RR1,C)/SUM((RR3,C1), SAM(RR,A,RR3,C1));

DISPLAY
 "Default data used for SHRHOME -- data missing"
 SHRHOME
 ;
*-End IF statement
 );


*-Eliminating superfluous elasticity data=========

 TRADELAS(RR,C,'SIGMAT')$(CEN(RR,C) OR (CE(RR,C) AND CDN(RR,C)))   = 0;
 TRADELAS(RR,C,'SIGMAQ')$(CMN(RR,C) OR (CM(RR,C) AND CDN(RR,C)))   = 0;
 PRODELAS(RR,A)$(NOT SAM('TOT','TOTAL',RR,A))                    = 0;
 ELASAC(RR,C)$(NOT SUM((RR1,A), SAM(RR1,A,RR,C)))                  = 0;
 LESELAS1(RR,H,RR1,C)$(NOT SAM(RR1,C,RR,H))                        = 0;
 LESELAS2(RR,A,RR1,C,RR2,H)$(NOT SHRHOME(RR,A,RR1,C,RR2,H))          = 0;

*-Physical factor quantities======================

PARAMETER
 QF2BASE(RR,F,RR1,A)  qnty of fac f of region R employed by act a of region R1
 ;
*-If there is a SAM payment from A to F and supply (but not
*-demand) quantities have been defined in the country data file,
*-then the supply values are used to compute demand quantities.
 QF2BASE(RR,F,RR1,A)$(SAM(RR,F,RR1,A)$((NOT QFBASE(RR,F,RR1,A))$QFRSBASE(RR,F)))
   = QFRSBASE(RR,F)*SAM(RR,F,RR1,A)/SUM((RR2,A1), SAM(RR,F,RR2,A1));

*-If there is a SAM payment from A to F and neither supply nor
*-demand quantities have been defined in the country data file,
*-then SAM values are used as quantities
 QF2BASE(RR,F,RR1,A)$(SAM(RR,F,RR1,A)$((QFBASE(RR,F,RR1,A) EQ 0)$(QFSBASE(F) EQ 0)))
                                                    = SAM(RR,F,RR1,A);

*-If there is a SAM payment from A to F and demand quantities have
*-been defined in the country data file, then this information is used.
 QF2BASE(RR,F,RR1,A)$QFBASE(RR,F,RR1,A) = QFBASE(RR,F,RR1,A);

*--END OF LOADING DATA---*

