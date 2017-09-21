$TITLE Calibration
$STITLE Turkish Agricultural CGE Model - Regional.
$STITLE Based on IFPRI Standard CGE modeling system, Version 1.01
$ONTEXT
*=============================================================================
* File      : 00base.gms
* Author    : Hasan Dudu (hasan@dudu.gen.tr)
* Version   : 1.0
* Date      : 15/08/2011 17:13:57
* Changed   : 13/12/2013 18:08:24
* Changed by: Hasan Dudu (hasan@dudu.gen.tr)
* Remarks   :

This file is used to load the necessary data for SAM operations.

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
*- This section is divided into the following subsections:
*- a. Parameters appearing in model equations
*- b. Parameters used for model calibration (to initialize variables and
*-    to define model parameters)
*- In each group, the parameters are declared in alphabetical order.
$OFFTEXT

*--------------------------------------------------------------------------------
*-3. PARAMETER DECLARATIONS -----------------------------------------------------
*--------------------------------------------------------------------------------


PARAMETERS
*-a. Parameters appearing in model equations================
*-Parameters other than tax rates
 alphaa(RR,A)                    shift parameter for top level CES function in region R
 alphaac(RR,C)                   shift parameter for domestic commodity aggregation fn in region R
 alphaq(RR,C)                    shift parameter for Armington function in region R
 alphat(RR,C)                    shift parameter for CET function in region R
 alphava(RR,A)                   shift parameter for CES activity production function in region R
 alpharain(RR,A)                   rainfall parameter
 betah(RR,A,RR1,C,RR2,H2)           marg shr of hhd cons in region R2 on home commmodity c of region r1 from act a of region R
 betam(RR,C,RR1,H)                marg share of hhd cons on marketed commodity c
 cwts(RR,C)                      consumer price index weights
 deltaa(RR,A)                    share parameter for top level CES function in region R
 deltaac(RR,A,RR1,C)              share parameter for domestic commodity C of region R1 aggregation fn
 deltaq(ROW,RR,C)                share parameter for Armington function
 deltat(RR,C,ROW)                share parameter for CET function
 deltava(RR,F,RR1,A)              share parameter for CES activity production function
 dwts(RR,C)                      domestic sales price weights
 gammah(RR,A,RR1,C,RR2,H2)          per-cap subsist cons for hhd h on home com c fr act a
 gammam(RR,C,RR1,H)               per-cap subsist cons of marketed com c for hhd h
 ica(RR,C,RR1,A)                  intermediate input c from region R per unit of aggregate intermediate A of region R1
 icd(RR,C,RR1,C1)                    trade input of c per unit of comm'y C1 produced & sold dom'ly in region R
 ice(RR,C,RR1,C1)                    trade input of c per unit of comm'y C1 exported from region R
 icm(RR,C,RR1,C1)                    trade input of c per unit of comm'y C1 imported from region R
 inta(RR,A)                      aggregate intermediate input coefficient
 iva(RR,A)                       aggregate value added coefficient in region R
 mps01(RR,INS)                   0-1 par for potential flexing of savings rates in region R
 mpsbar(RR,INS)                  marg prop to save for dom non-gov inst ins of region R(exog part)
 nu(RR,F2N,RR1,A)                     water per ha. in activity A of region R
 qdst(RR,C)                      inventory investment by sector of origin
 qbarg(RR,C,RN)                  exogenous (unscaled) government demand in region R
 qbarinv(RR,C)                   exogenous (unscaled) investment demand in region R
 rhoa(RR,A)                      CES top level function exponent in region R
 rhoac(RR,C)                     domestic commodity aggregation function exponent in region R
 rhoq(RR,C)                      Armington function exponent in region R
 rhot(RR,C)                      CET function exponent in region R
 rhova(RR,A)                     CES activity production function exponent in region R
 shif(R,INS,RR1,F)               share of dom. inst'on i of region R in income of factor f of region R1
 shiw(RR,INS)                    share of dom. inst'on i  in income of  water factor f in region R
 shii(RR,INS,RR1,INS1)            share of inst'on i of region R in post-tax post-sav income of inst i1 of region R1
 theta(RR,A,RR1,C)                yield of commodity C of region R per unit of activity A of region R1
 tins01(RR,INS)               0-1 par for potential flexing of direct tax rates in region R paid
 trnsfr(RALL,AC,RALL,AC1)             transfers from inst. or factor ac of region R to institution ins of region R1

*-Tax rates
 ta(RR,A,RN)                        rate of tax on producer gross output value in region R paid to gov't of region R1
 te(RR,C,RN,ROW)                    rate of tax on exports from region R paid to gov't of region R1
 tf(RR,F,RN)                        rate of direct tax on factors (soc sec tax) paid to gov't of region R1
 tinsbar(RR,INS,RN)                 rate of (exog part of) direct tax on dom inst ins in region R paid to gov't of region R1
 tm(RN,ROW,RR1,C)                    rate of import tariff paid to gov't of region R1
 tq(RR,C,RN)                        rate of sales tax in region R paid to gov't of region R1
 tva(RR,A,RN)                       rate of value-added tax in region R paid to the gov't of region R1
 twa(RR,F,RR1,A)                  rate of water charge on water quantity used from region R by activity A of region R1

*-b. Parameters used for model calibration==================

$ONTEXT
*- For model calibration, one parameter is created for each model variable
*- with the suffix "0" added to the variable name. 0 is also added to the
*- names of parameters whose values are changed in experiments.
$OFFTEXT

PARAMETERS
*-Parameters for definition of model parameters
 alphava0(RR,A)                  shift parameter for CES activity production function
 budshr(RR,C,RR1,H)               budget share for marketed commodity c and household h
 budshr2(RR,A,RR1,C,RR2,H2)         budget share for home commodity c - act a - hhd h
 budshrchk(RR,H)                 check that budget shares some to unity
 elaschk(RR,H)                   check that expenditure elasticities satisfy Engel aggr in region R
 frisch2(RR,H)                                          alt. defn of Frisch -- ratio of cons to supernumerary cons
 leschk(RR,H)                                           check on LES parameter definitions (error mssg if error)
 leselasp(RR,H,*,RR1,C,*,RR2,C1)                   price elasticity bt c and C1 for h (with c and C1 labeled by source)
 qdst0(RR,C)                       stock change
 qbarg0(RR,C,RN)                    exogenous (unscaled) government demand in reigon R
 gammah0(RR,A,RR1,C,RR2,H2)         per-cap subsist cons for hhd h on home com c fr act a
 gammam0(RR,C,RR1,H)              per-cap subsist cons of marketed com c for hhd h
 prodshr(RR,A)                   production share of region R in total production of activity A
 predeltaa(RR,A)                 Dummy used to define deltaa
 predelta(RR,C)                  Dummy used to define deltaq
 predeltat(RR,C)                 Dummy used to define deltat
 shctd(RR,C)                     share of comm'y ct in trans services for domestic sales in region R
 shctm(RR,C)                     share of comm'y ct in trans services for imports in region R
 shcte(RR,C)                     share of comm'y ct in trans services for exports in region R
 subsist(RR,H)                                          subsistence spending
 supernum(RR,H)                  LES supernumerary income in region R
 ta0(RR,A,RN)                   rate of tax on producer gross output value paid to gov of region R1
 te0(RR,C,RN,ROW)               rate of tax on exports of region R paid to gov of region R1
 tf0(RR,F,RN)                   rate of direct tax on factors (soc sec tax) in region R paid to gov of region R1
 tins0(RR,INS,RN)               rate of direct tax on domestic institutions ins of region R to gov of region R1
 tm0(RN,ROW,RR1,C)               rate of import tariff paid to gov of region R1
 tq0(RR,C,RN)                   rate of sales tax in region R paid to gov of region R1
 tva0(RR,A,RN)                   rate of value-added tax paid by act A of region R to gov of region R1
 twa0(RR,F,RR1,A)                 rate of water charge on water quantity in region R
 nu0(RR,F2N,RR1,A)                     water per ha. in activity A of region R


*-Check parameters
  cwtschk                       check that CPI weights sum to unity
  dwtschk                       check that PDIND weights sum to unity
  shifchk                       check that factor payment shares sum to unity

*-Parameters for variable initialization
  CPI0                          consumer price index (PQ-based)
  DPI0                          index for domestic producer prices (PDS-based)
  DMPS0                         change in marginal propensity to save for selected inst
  DTINS0                        change in domestic institution tax share
  EG0(R)                        total current government expenditure in region R
  EH0(RR,H)                      household consumption expenditure in region R
*-EXR0                          exchange rate
  EXR0(ROW)                     exchange rate
  TFSAV0                        total foreign savings
  FSAV0(ROW)                    foreign savings
  GADJ0                         government demand scaling factor in region R
  GOVSHR0                       govt consumption share of absorption in region R
  GSAV0                         government savings
  IADJ0                         investment scaling factor (for fixed capital formation) for all regions
  INVSHR0                       investment share of absorption
  MPS0(RR,INS)                   marginal propensity to save for dom non-gov inst ins of region R
  MPSADJ0                   savings rate scaling factor in region R
  PA0(RR,A)                      output price of activity a
  PDD0(RR,C)                     demand price for comy. c produced & sold domestically in region R
  PDS0(RR,C)                     supply price for comy. c produced & sold domestically in region R
  PE0(RR,C,ROW)                  price of exports in region R
  PINTA0(RR,A)                   price of intermediate aggregate
  PM0(ROW,RR,C)                  price of imports
  PQ0(RR,C)                      price of composite good c
  PSUP(C)                       initial supply-side market price for commodity C
  PSUPA(A)                      initial supply-side market price for activity A
  PVA0(RR,A)                     value added price in region R
  PWE0(RR,C,ROW)                 world price of exports of region R
  PWM0(ROW,RR,C)                   world price of imports
  PX0(RR,C)                      average output price in region R
  PXAC0(RR,A,RR1,C)               price of commodity c of region R1 from activity a of region R
  QA0(RR,A)                      level of domestic activity in region R
  QD0(RR,C)                      quantity of domestic sales in region R
  QE0(RR,C,ROW)                  quantity of exports
  QF0(RR,F,RR1,A)                 quantity demanded of factor f region R from activity a of region R1
  QFS0(RR,F)                     quantity of factor supply in region R
  QG0(RR,C,RN)                   quantity of government  consumption of region R1 in region R
  QH0(RR,C,RR1,H)                quantity consumed of marketed commodity c of region R  by hhd h of region R1
  QHA0(RR,A,RR1,C,RR2,H2)           quantity consumed of home commodity c fr act a by hhd h
  QINT0(RR,C,RR1,A)               quantity of intermediate demand for c of region R from activity a of region R1
  QINTA0(RR,A)                   quantity of aggregate intermediate input in region R
  QINV0(RR,C)                      quantity of fixed investment demand in region R
  QM0(ROW,RR,C)                  quantity of imports
  QQ0(RR,C)                      quantity of composite goods supply in region R
  QT0(RR,C)                      quantity of trade and transport demand for commodity c OF region R
  QVA0(RR,A)                     quantity of aggregate value added in region R
  QX0(RR,C)                      quantity of aggregate marketed commodity output in region R
  QXAC0(RR,A,RR1,C)               quantity of output of commodity c of region R1 from activity a of region R
  TABS0                         total absorption
  TINSADJ0                   direct tax scaling factor in region R
  TRII0(RR,INS,RR1,INS1)          transfers to dom. inst. insdng of region R from insdng1 of region R1
  WALRAS0                       savings-investment imbalance (should be zero)
  WF0(F)                        economy-wide wage (rent) for factor f
  WFR0(RR,F)                     region-wide wage (rent) for factor f
  WFA(RR,F,RR1,A)                 wage for factor f of region R in activity a of region R1(used for calibration)
  WFDIST0(RR,F,RR1,A)             factor wage distortion variable
  WPAY0(R)                      payments to households from water charges in region R
  YF0(RR,F)                      factor income in region R
  YG0(R)                        total current government income in region R
  YIF0(R,INS,RR1,F)              income of institution ins of region R from factor f of region R1
  YI0(RR,INS)                    income of (domestic non-governmental) institution ins of region R
;

*-parameters for labor supply calibration
parameter
LESELASF(RR,H)          income elasticity of factor F of household H in region RR
betal(RR,H)             LES share parameter for supplied factor
LABAVL0(RR,H)           total population minus subsistence leisure
WHAVG0(RR,H)            average wage rate for HH
EXPLEI(RR,H)            expenditure on leisure
budshrlei(RR,H)         budget share of leisure in total expenditure including expenditure on leisure
budshrC(RR,C,RR1,H)     budget share of cons coms in total expenditure including expenditure on leisure
budshrC2(RR,A,RR1,C,RR2,H2)
budshrleichk(RR,H)      check budget shares after the inclusion of expenditure on lesiure
elaschkC(RR,H)          elasticity check when leisure expenditure is included.
betalchk(RR,H)          check that monotonic transformation condition holds
courn(RR3,C1,RR1,H)     cournot aggregation check
NATRUE(RR,H)            natural rate of unemployment as a percentage of total unemployment
etals(RR,H)             labor force participation elasticity parameter
QLEI0(RR,H)             quantity of leisure (# of people out of labor force)
;

*--------------------------------------------------------------------------------
*-4. PARAMETER DEFINITIONS ------------------------------------------------------
*--------------------------------------------------------------------------------

*-All parameters are defined, divided into the same blocks as the
*-equations.

*-Price block=====================================

$ONTEXT
*- The prices PDS, PX, and PE  may be initialized at any desired price.
*- The user may prefer to initialize these prices at unity or, if
*- he/she is interested in tracking commodity flows in physical units, at
*- commodity-specific, observed prices (per physical unit). For any given
*- commodity, these three prices should be identical. Initialization at
*- observed prices may be attractive for disaggregated agricultural
*- commodities. If so, the corresponding quantity values reflect physical
*- units (given the initial price).
*-
*- The remaining supply-side price, PXAC, and the non-commodity prices, EXR
*- and PA may be initizalized at any desired level. In practice, it may be
*- preferable to initialize PXAC at the relevant supply-side price and EXR
*- and PA at unity.
*-
*- If physical units are used, the user should select the unit (tons vs.
*- '000 tons) so that initial price and quantity variables are reasonably
*- scaled (for example between 1.0E-2 and 1.0E+3) -- bad scaling may cause
*- solver problems. Initialization at unity should cause no problem as long
*- as the initial SAM is reasonably scaled.

$OFFTEXT


 PSUP(C)  = 1;
 PA0(RR,A)$SUM((RR1,C),SAM(RR1,C,RR,A)) = 1;
 PA0(RR,A)$QATABASE(RR,A) = SAM('TOT','TOTAL',RR,A) / QATABASE(RR,A);

 prodshr(RR,A)$SUM(RR1, SAM('TOT','TOTAL',RR1,A)) = SAM('TOT','TOTAL',RR,A)/SUM(RR1, SAM('TOT','TOTAL',RR1,A));

 PSUPA(A) = SUM(RR, PRODSHR(RR,A)*PA0(RR,A));

 PSUP(C) =  SUM((RR,A)$MAPAC(A,C),PRODSHR(RR,A)*PA0(RR,A));

 PE0(RR,C,ROW)$CE(RR,C)    = PSUP(C);
 PX0(RR,C)$CX(RR,C)        = PSUP(C);
 PDS0(RR,C)$CD(RR,C)       = PSUP(C);
 PXAC0(RR,A,RR1,C)$SAM(RR,A,RR1,C) = PSUP(C);

$ONTEXT
*- The exchange rate may be initialized at unity, in which case all data are
*- in foreign currency units (FCU; e.g., dollars). Set the exchange rate at
*- another value to differentiate foreign exchange transactions, which will
*- be valued in FCU, and domestic transactions valued in local currency
*- units (LCU). The SAM is assumed to be valued in LCU, and the exchange rate
*- is then used to calculate FCU values for transactions with the rest of the
*- world.
$OFFTEXT

 EXR0(ROW)          = 1 ;

*-Activity quantity = payment to activity divided by activity price
*-QA covers both on-farm consumption and marketed output
*-output GROSS of tax
*-controlling for PA0(RR,A) since some activities do not exist in some regions
 QA0(RR,A)$PA0(RR,A)= SAM('TOT','TOTAL',RR,A)/PA0(RR,A) ;

*-Unit value-added price = total value-added / activity quantity
*-define pva gross of tax

 QVA0(RR,A)       =  SUM((RR1,F), SAM(RR1,F,RR,A))+ SUM((RN,VATX),(TAXPAR(RN,VATX,RR,A))) ;
 PVA0(RR,A)$QVA0(RR,A) =  (SUM((RR1,F), SAM(RR1,F,RR,A))+ SUM((RN,VATX),TAXPAR(RN,VATX,RR,A)))/QVA0(RR,A);
 iva(RR,A)$QA0(RR,A)        =  QVA0(RR,A)/QA0(RR,A) ;
 QXAC0(RR,A,RR1,C)$(PXAC0(RR,A,RR1,C)) = SAM(RR,A,RR1,C) / PXAC0(RR,A,RR1,C);
 QHA0(RR,A,RR1,C,RR2,H2)$SHRHOME(RR,A,RR1,C,RR2,H2) = SHRHOME(RR,A,RR1,C,RR2,H2)*SAM(RR,A,RR2,H2)/PXAC0(RR,A,RR1,C);


*-Output quantity = value received by producers divided by producer price
*-QX covers only marketed output

 QX0(RR,C)$SUM(   (RR1,A), SAM(RR1,A,RR,C)) = SUM((RR1,A), SAM(RR1,A,RR,C)) / PX0(RR,C);

*-Export quantity = export revenue received by producers
*-(ie. minus tax and transactions cost) divided by export price

 QE0(RR,C,ROW)$(SUM(RN,SAM(RR,C,RN,ROW)) AND PE0(RR,C,ROW)) =  SUM(RN,SAM(RR,C,RN,ROW)
 - sum(TX$MAPROWEXP(ROW,TX),TAXPAR(RN,TX,RR,C)))/PE0(RR,C,ROW);

*-RoW export price = RoW export payment (in for curr) / export qnty
 PWE0(RR,C,ROW)$QE0(RR,C,ROW) = (sum(RN,SAM(RR,C,RN,ROW))/EXR0(ROW)) / QE0(RR,C,ROW);
 te0(RR,C,RN,ROW)$SUM(R1,SAM(RR,C,R1,ROW)) = sum((TX)$MAPROWEXP(ROW,TX),TAXPAR(RN,TX,RR,C))/SUM(R1,SAM(RR,C,RN,ROW));
 te(RR,C,RN,ROW) =  te0(RR,C,RN,ROW);



*-Quantity of output sold domestically = output quantity less quantity
*-exported = value of domestic sales divided by domestic supply price
*-QD0 covers only marketed output

 QD0(RR,C)$CD(RR,C) =  QX0(RR,C) - SUM(ROW,QE0(RR,C,ROW));

*-Domestic demander price = demander payment divided by quantity bought
 PDD0(RR,C)$QD0(RR,C)= (PDS0(RR,C)*QD0(RR,C) + SUM((RR1,CTD), SAM(RR1,CTD,RR,C)))/QD0(RR,C);

*-Define import price to equal domestic price so that import and domestic
*-units are the same to the purchaser. If no domestic good, set PM to 1.

 PM0(ROW,RR,C)               = PDD0(RR,C) ;
 PM0(ROW,RR,C)$(QD0(RR,C) EQ 0) = 1 ;
*-Import quantity = demander payment for imports (including tariffs
*-and marketing cost) divided by demander price.

 QM0(ROW,RR,C)$CM(RR,C) = SUM(RN,SAM(RN,ROW,RR,C)+sum((RN1,TX)$MAPROWIMP(ROW,TX),TAXPAR(RN1,TX,RR,C)))/PM0(ROW,RR,C) ;

*-World price = import value (in foreign currency / import quantity

 PWM0(ROW,RR,C)$QM0(ROW,RR,C)= SUM(RN,SAM(RN,ROW,RR,C)/EXR0(ROW)) / QM0(ROW,RR,C);

 tm0(RN,ROW,RR,C)$SUM(R2,SAM(RN,ROW,RR,C)) = sum(TX$MAPROWIMP(ROW,TX),TAXPAR(RN,TX,RR,C))/ SAM(RN,ROW,RR,C);
 tm(RN,ROW,RR1,C) = tm0(RN,ROW,RR1,C);



*-Composite supply is the sum of domestic market sales and imports
*-(since they are initialized at the same price).

 QQ0(RR,C)$(CD(RR,C) OR CM(RR,C)) = QD0(RR,C) + SUM(ROW,QM0(ROW,RR,C)) ;
 PQ0(RR,C)$QQ0(RR,C) = (SAM(RR,C,'TOT','TOTAL') - SUM((R1,ROW),SAM(RR,C,R1,ROW)))/QQ0(RR,C);

 TQ0(RR,C,RN)$QQ0(RR,C) = SUM(COMTX,TAXPAR(RN,COMTX,RR,C))/(PQ0(RR,C)*QQ0(RR,C)) ;
 TQ(RR,C,RN)         = TQ0(RR,C,RN) ;

*-The following code works when for any number of sectors providing
*-transactions services, as well as for the case when they are not
*-in the SAM.

 shctd(RR,C)$CT(RR,C) = SUM((RR1,CTD), SAM(RR,C,RR1,CTD)/SAM('TOT','TOTAL',RR,CTD)) ;
 shctm(RR,C)$CT(RR,C) = SUM((RR1,CTM), SAM(RR,C,RR1,CTM)/SAM('TOT','TOTAL',RR,CTM)) ;
 shcte(RR,C)$CT(RR,C) = SUM((RR1,CTE), SAM(RR,C,RR1,CTE)/SAM('TOT','TOTAL',RR,CTE)) ;

 icd(RR,C,RR1,C1)$(QD0(RR1,C1) AND CT(RR,C))
   = (shctd(RR,C)*SUM((RR2,CTD), SAM(RR2,CTD,RR1,C1))/PQ0(RR,C)) / QD0(RR1,C1);

 icm(RR,C,RR1,C1)$(SUM(ROW,QM0(ROW,RR,C)) AND CT(RR,C))
  = (shctm(RR,C)*SUM((RR2,CTM), SAM(RR2,CTM,RR1,C1))/PQ0(RR,C)) / SUM(ROW,QM0(ROW,RR1,C1));

 ice(RR,C,RR1,C1)$(SUM(ROW,QE0(RR1,C,ROW)) AND CT(RR,C))
  = (shcte(RR,C)*SUM((RR2,CTE), SAM(RR2,CTE,RR,C))/PQ0(RR,C)) / SUM(ROW,QE0(RR1,C1,ROW));

*-Indirect activity tax rate = tax payment / output value
*-Tax is here applied to total output value (incl. on-farm cons.)
 tva0(RR,A,RN)$QVA0(RR,A) = SUM((VATX),TAXPAR(RN,VATX,RR,A)) / (PVA0(RR,A)*QVA0(RR,A));
 tva(RR,A,RN)          = tva0(RR,A,RN);

*-QA is GROSS of tax, so base for ta is as well
 ta0(RR,A,RN)$(SAM(RR,A,'TOT','TOTAL')) = SUM((ATX),TAXPAR(RN,ATX,RR,A)) / (SAM(RR,A,'TOT','TOTAL'));
 ta(RR,A,RN)         = ta0(RR,A,RN);

 ;

*-Yield coefficient
*- = quantity produced (including home-consumed output)
*-   /activity quantity
 theta(RR,A,RR1,C)$PXAC0(RR,A,RR1,C)
  = ( (SAM(RR,A,RR1,C) + SUM((RR2,H2), SHRHOME(RR,A,RR1,C,RR2,H2)*SAM(RR,A,RR2,H2)) ) / PXAC0(RR,A,RR1,C) )
                                                              / QA0(RR,A);

*-Intermediate input coefficient = input use / output quantity
 QINTA0(RR,A) = SUM((RR1,C)$PQ0(RR1,C), SAM(RR1,C,RR,A)  / PQ0(RR1,C)) ;

 ica(RR,C,RR1,A)$(QINTA0(RR1,A)$PQ0(RR,C))
               = SAM(RR,C,RR1,A)/PQ0(RR,C) / QINTA0(RR1,A) ;

 inta(RR,A)$QA0(RR,A)      = QINTA0(RR,A) / QA0(RR,A) ;
 pinta0(RR,A)     = SUM((RR1,C), ica(RR1,C,RR,A)*PQ0(RR1,C)) ;

*-CPI weight by comm'y = hhd cons value for commy / total hhd cons value
*-CPI does not consider on-farm consumption.
 cwts(RR,C)       = SUM((RR1,H), SAM(RR,C,RR1,H)) / SUM((RR2,C1,R3,H), SAM(RR2,C1,R3,H));

*-Domestic sales price index weight = dom sales value for commy
*- total domestic sales value
*-Domestic sales price index does not consider on-farm consumption.
 dwts(RR,C)       = (SUM((RR1,A), SAM(RR1,A,RR,C)) - (SUM((RN,ROW),SAM(RR,C,RN,ROW)) -
                  SUM((RR3,cte), SAM(RR3,cte,RR,C))))/
                  SUM((RR4,C1), SUM((RR1,A), SAM(RR1,A,RR4,C1)) - (SUM((R2,ROW),SAM(RR4,C1,R2,ROW)) -
                  SUM((RR3,CTE), SAM(RR3,CTE,RR4,C1))));

 CWTSCHK       = SUM((RR,C), cwts(RR,C));
 DWTSCHK       = SUM((RR,C), dwts(RR,C));

 CPI0          = SUM((RR,C), cwts(RR,C)*PQ0(RR,C)) ;
 DPI0          = SUM((RR,C)$CD(RR,C), dwts(RR,C)*PDS0(RR,C)) ;


*-Production and trade block==========================

*-Compute exponents from elasticites
 rhoq(RR,C)$(CM(RR,C) AND CD(RR,C)) = (1/TRADELAS(RR,C,'SIGMAQ')) - 1;
 rhot(RR,C)$(CE(RR,C) AND CD(RR,C))  = (1/TRADELAS(RR,C,'SIGMAT')) + 1;
 rhova(RR,A)$SAM('TOT','TOTAL',RR,A)         = (1/PRODELAS(RR,A)) - 1;
 rhoa(RR,A)$ACES(RR,A) = (1/PRODELAS2(RR,A)) - 1;


*-Aggregation of domestic output from different activities

 rhoac(RR,C)$elasac(RR,C) = 1/elasac(RR,C) - 1;

 deltaac(RR,A,RR1,C)$ SAM(RR,A,RR1,C)
               = (PXAC0(RR,A,RR1,C)*QXAC0(RR,A,RR1,C)**(1/ELASAC(RR1,C)))/
                 SUM((RR2,A1), PXAC0(RR2,A1,RR1,C)*QXAC0(RR2,A1,RR1,C)**(1/ELASAC(RR1,C)));

 alphaac(RR,C)$SUM((RR1,A),deltaac(RR1,A,RR,C))
               = QX0(RR,C)/
                 (SUM((RR1,A)$deltaac(RR1,A,RR,C), deltaac(RR1,A,RR,C)*QXAC0(RR1,A,RR,C)
                 **(-RHOAC(RR,C))) )**(-1/RHOAC(RR,C));



*-Demand computations=====

*-Defining factor employment and supply.

 QF0(RR,F,RR1,A)  = QF2BASE(RR,F,RR1,A);

 QFS0(RR,F)=SUM((RR2,A), QF0(RR,F,RR2,A))+QFUE0(RR,F);

*-Activity-specific wage is activity labor payment over employment
 WFA(RR,F,RR1,A)$SAM(RR,F,RR1,A) = SAM(RR,F,RR1,A)/QF0(RR,F,RR1,A);
 twa0(RR,FWAT,RR1,A)$QF0(RR,FWAT,RR1,A) =  SUM((RN,WTTX),TAXPAR(RN,WTTX,RR1,A)) / QF0(RR,FWAT,RR1,A);
 twa(RR,F,RR1,A)                =  twa0(RR,F,RR1,A);

 WFA(RR,F,RR1,A)$(SAM(RR,F,RR1,A) AND QF0(RR,F,RR1,A)) = SAM(RR,F,RR1,A)/QF0(RR,F,RR1,A);

*-Region-wide wage average is total factor income over employment
 WFR0(RR,F)$SUM((RR1,A), QF0(RR,F,RR1,A)) = SUM((RR1,A), SAM(RR,F,RR1,A))/SUM((RR1,A), QF0(RR,F,RR1,A));

*-Economy-wide wage average is total factor income over employment
 WF0(F)$SUM((RR,RR1,A), QF0(RR,F,RR1,A))   = SUM((RR,RR1,A), SAM(RR,F,RR1,A))/SUM((RR,RR1,A), QF0(RR,F,RR1,A));



DISPLAY
"If the value of WF0 for any factor is very different from one (< 0.1"
"or >10) the user may consider rescaling the initial values for QFBASE"
"or QFSBASE for this factor to get a value of WF0 such that"
"0.1 < WF0 < 10"
 WF0
 ;

*-Wage distortion factor

 wfdist0(RR,F,RR1,A)$SAM(RR,F,RR1,A) = WFA(RR,F,RR1,A)/WFR0(RR,F);

*-Land-water composite coefficient
*-the coefficient of leontieff nest is calculated according to the factor that enters the first nest
*- nu amounts of other factors is required per unit of base factor.
nu0(RR,F2N,RR1,A2N)$SUM(F21N,QF0(RR,F21N,RR1,A2N))= QF0(RR,F2N,RR1,A2N)/SUM(F21N,QF0(RR,F21N,RR1,A2N));

nu(RR,F2N,RR1,A2N)=nu0(RR,F2N,RR1,A2N);

*-parameters of the first nest in production function

*the below code is for the case with single nest at factor level
*- deltava(RR,F1N,RR1,A)$QF0(RR,F1N,RR1,A)
*-            = ((wfdist0(RR,F,RR1,A)*WF0(F)+twa0(RR,F,RR1,A))
*-            = (wfdist0(RR,F1N,RR1,A)*WFR0(RR,F1N)
*-                       *(QF0(RR,F1N,RR1,A))**(1+rhova(RR1,A)) )
*-              / SUM((RR2,F1), (wfdist0(RR2,F1,RR1,A)*WF0(F1)+twa0(RR2,F1,RR1,A))*(QF0(RR2,F1,RR1,A))**(1+rhova(RR1,A)));
*-              / SUM((RR2,F1N1), wfdist0(RR2,F1N1,RR1,A)*WFR0(RR,F1N)*(QF0(RR2,F1N,RR1,A))**(1+rhova(RR1,A)));


Parameter Deltavaden(RR,F,RR1,A) Denominator for deltava;

Deltavaden(RR,F,RR1,A)= SUM((RR3,F2N), (nu0(RR3,F2N,RR1,A)*wfdist0(RR3,F2N,RR1,A)*WFR0(RR,F2N))*(SUM(F21N,QF0(RR3,F21N,RR1,A))**(1+rhova(RR1,A))))
                 + SUM((RR2,F1N1), wfdist0(RR2,F1N1,RR1,A)*WFR0(RR2,F1N1)*(QF0(RR2,F1N1,RR1,A))**(1+rhova(RR1,A)))  ;

 deltava(RR,F1N,RR1,A)$QF0(RR,F1N,RR1,A)
            = (wfdist0(RR,F1N,RR1,A)*WFR0(RR,F1N)
                       *(QF0(RR,F1N,RR1,A))**(1+rhova(RR1,A)) )
              /Deltavaden(RR,F1N,RR1,A);

*calculate deltava seperately for the factor that comes from the second nest.
 deltava(RR,F21N,RR1,A)$QF0(RR,F21N,RR1,A)
            = SUM(F2N,nu0(RR,F2N,RR1,A)*wfdist0(RR,F2N,RR1,A)*WFR0(RR,F2N))
                       *(QF0(RR,F21N,RR1,A))**(1+rhova(RR1,A))
              / Deltavaden(RR,F21N,RR1,A);

*-case with single  nest
*- alphava0(RR,A)$sum((RR1,F1N),deltava(RR1,F1N,RR,A))= QVA0(RR,A)/( SUM((RR1,F1N)$(QF0(RR1,F1N,RR,A)), deltava(RR1,F1N,RR,A)*QF0(RR1,F1N,RR,A)
*-               **(-rhova(RR,A))) )**(-1/rhova(RR,A));

*adding the factor that is coming from the second nest

 alphava0(RR,A)$sum((RR1,F1N),deltava(RR1,F1N,RR,A))= QVA0(RR,A)/( SUM((RR1,F1N)$(QF0(RR1,F1N,RR,A)), deltava(RR1,F1N,RR,A)*QF0(RR1,F1N,RR,A)
               **(-rhova(RR,A)))+SUM((RR3,F21N),deltava(RR3,F21N,RR,A)*QF0(RR3,F21N,RR,A)**(-rhova(RR,A))) )**(-1/rhova(RR,A));

 alphava(RR,A)= alphava0(RR,A)   ;

*-CES top level production function

 predeltaa(RR,A)  = 0 ;
 predeltaa(RR,A)$(ACES(RR,A) AND QINTA0(RR,A))
                = (PVA0(RR,A)/PINTA0(RR,A))*(QVA0(RR,A)/QINTA0(RR,A))**(1+rhoa(RR,A)) ;
 deltaa(RR,A)$ACES(RR,A) = predeltaa(RR,A)/(1 + predeltaa(RR,A)) ;
 alphaa(RR,A)$deltaa(RR,A)
                = QA0(RR,A)/((deltaa(RR,A)*QVA0(RR,A)**(-rhoa(RR,A))
                  +(1-deltaa(RR,A))*QINTA0(RR,A)**(-rhoa(RR,A)))**(-1/rhoa(RR,A))) ;

*-Intermediate demand
 QINT0(RR,C,RR1,A)$PQ0(RR,C) = SAM(RR,C,RR1,A) / PQ0(RR,C);



*-Transactions demand
 QT0(RR,C)$CT(RR,C) = (SUM((RR1,CTD), SAM(RR,C,RR1,CTD)) + SUM((RR2,CTE), SAM(RR,C,RR2,CTE))
             + SUM((R3,CTM), SAM(RR,C,R3,CTM))) / PQ0(RR,C) ;


 predeltat(RR,C)$(CE(RR,C) AND CD(RR,C)) =
            (sum(ROW$CER(RR,C,ROW),PE0(RR,C,ROW)*QE0(RR,C,ROW)**(1-rhot(RR,C)))
          + PDS0(RR,C)*QD0(RR,C)**(1-rhot(RR,C)) );


 deltat(RR,C,ROW)$(CER(RR,C,ROW) AND CD(RR,C))   =
        (PE0(RR,C,ROW)*QE0(RR,C,ROW)**(1-rhot(RR,C)))/predeltat(RR,C) ;

 alphat(RR,C)$(CE(RR,C) AND CD(RR,C))
   = QX0(RR,C) / (SUM(ROW$CER(RR,C,ROW),deltat(RR,C,ROW)*QE0(RR,C,ROW)**rhot(RR,C)) + (1-SUM(ROW,deltat(RR,C,ROW)))
                 *QD0(RR,C)**rhot(RR,C))**(1/rhot(RR,C));

*-Armington aggregation

 predelta(RR,C)$(CM(RR,C) AND CD(RR,C))  = (SUM(ROW$CMR(RR,C,ROW),PM0(ROW,RR,C)*QM0(ROW,RR,C)**(1+rhoq(RR,C)) )
          + PDD0(RR,C)*QD0(RR,C)**(1+rhoq(RR,C)) );

*- USE THIS IF THERE ARE SECTORS WITHOUT DOMESTIC PRODUCTION, ONLY IMPORTS
*- predelta(C)$(QD0(c) eq 0) = sum(row,PM0(row,c)*QM0(row,c)**(1+rhoq(c)) );


 deltaq(ROW,RR,C)$(CMR(RR,C,ROW) AND CD(RR,C))
              = (PM0(ROW,RR,C)*QM0(ROW,RR,C)**(1+rhoq(RR,C)))/predelta(RR,C) ;

 alphaq(RR,C)$(CM(RR,C) AND CD(RR,C))
               = QQ0(RR,C)/(SUM(ROW$CMR(RR,C,ROW),deltaq(ROW,RR,C)*QM0(ROW,RR,C)**(-rhoq(RR,C)))
                 +(1-SUM(ROW,deltaq(ROW,RR,C)))*QD0(RR,C)**(-rhoq(RR,C)))**(-1/rhoq(RR,C)) ;

*-Institution block===============================

*-Institutional income
 YI0(RR,INSDNG) = SAM('TOT','TOTAL',RR,INSDNG);

*-Factor income by factor category
 YF0(RR,F) = SUM((RR1,A), SAM(RR,F,RR1,A));

*-Institution income from factors
 YIF0(R,INSD,RR1,F) = SAM(R,INSD,RR1,F);

 WPAY0(RN) = sum((R1,INS,WTTX),SAM(R1,INS,RN,WTTX)) ;


*-Transfers to RoW from factors
*- trnsfr('ROW',F) = SAM('ROW',F)/EXR0;
 trnsfr(RN,ROW,RR,F) = SAM(RN,ROW,RR,F)/EXR0(ROW);

*-Transfers from RoW to institutions
*- trnsfr(INSD,'ROW') = SAM(INSD,'ROW')/EXR0;
 trnsfr(R,INSD,RN,ROW) = SAM(R,INSD,RN,ROW)/EXR0(ROW);

*-Government transfers
 trnsfr(RR,INSD,RN,'GOV') = SAM(RR,INSD,RN,'GOV')/CPI0;

*-Factor taxes
 tf0(RR,F,RN)$SAM('TOT','TOTAL',RR,F) = SUM((TX),TAXPAR(RN,TX,RR,F))/SAM('TOT','TOTAL',RR,F);
 tf(RR,F,RN)      = tf0(RR,F,RN);

 shif(R,INSD,RR1,F)$SAM(RR1,F,'TOT','TOTAL')  = SAM(R,INSD,RR1,F)/(SAM(RR1,F,'TOT','TOTAL') - SUM(RN,TAXPAR(RN,'FACTAX',RR1,F))
                 - SUM((R2,ROW), SAM(R2,ROW,RR1,F)));
 shiw(RR,INSDNG)$sum((INSDNG1,WTTX,R1),SAM(RR,INSDNG1,R1,WTTX)) = SUM((R1,WTTX),SAM(RR,INSDNG,R1,WTTX))/sum((INSDNG1,R1,WTTX),SAM(RR,INSDNG1,R1,WTTX)) ;

 SHIFCHK(RR,F)    = SUM((R1,INSD), shif(R1,INSD,RR,F));


*-Inter-institution transfers
 TRII0(RR,INSDNG,RR1,INSDNG1) = SAM(RR,INSDNG,RR1,INSDNG1);

*-Share of dom non-gov institution in income of other dom non-gov
*-institutions (net of direct taxes and savings).
 shii(RR,INSDNG,RR1,INSDNG1)$SAM('TOT','TOTAL',RR1,INSDNG1)
  = SAM(RR,INSDNG,RR1,INSDNG1)
   /(SAM('TOT','TOTAL',RR1,INSDNG1) - SUM(RN,TAXPAR(RN,'INSTAX',RR1,INSDNG1) - SAM(RN,'S-I',RR1,INSDNG1)));

*-Scaling factors for savings and direct tax shares
 MPSADJ0     = 0;
 TINSADJ0    = 0;

*-Savings rates
 MPS0(RR,INSDNG)$SAM('TOT','TOTAL',RR,INSDNG)  = SUM(RN,SAM(RN,'S-I',RR,INSDNG))/(SAM('TOT','TOTAL',RR,INSDNG) - SUM(RN,TAXPAR(RN,'INSTAX',RR,INSDNG)));
 mpsbar(RR,INSDNG) = MPS0(RR,INSDNG);


*-Direct tax rates
 TINS0(RR,INSDNG,RN)$SAM('TOT','TOTAL',RR,INSDNG) = TAXPAR(RN,'INSTAX',RR,INSDNG) / SAM('TOT','TOTAL',RR,INSDNG);
 tinsbar(RR,INSDNG,RN) = TINS0(RR,INSDNG,RN);
*-ARE THOSE NECESSARY?
*-"Point" change in savings and direct tax shares
 DMPS0   = 0;
 DTINS0 = 0;


*-Selecting institutions for potential "point" change in savings and tax rates
*-If DMPS or MPSADJ is flexible, institutions with a value of 1 for mps01
*-change their savings rates.
 mps01(RR,INSDNG)  = 1;

*-If DTIMS is flexible, institutions with a value of 1 for tins01 change
*-their savings rates.
 tins01(RR,INSDNG) = 1;
*-ARE THOSE NECESSARY? UP TO HERE


*-Household consumption spending and consumption quantities.
 EH0(RR,H)        = SUM((RR1,C), SAM(RR1,C,RR,H)) + SUM((RR2,A), SAM(RR2,A,RR,H));
 QH0(RR,C,RR1,H)$PQ0(RR,C) = SAM(RR,C,RR1,H)/PQ0(RR,C);

*-Government indicators
 YG0(RN)           = SAM('TOT','TOTAL',RN,'GOV');
 EG0(RN)           = SAM('TOT','TOTAL',RN,'GOV') - SUM(R1,SAM(R1,'S-I',RN,'GOV'));
 QG0(RR,C,RN)$PQ0(RR,C) = SAM(RR,C,RN,'GOV')/PQ0(RR,C);

 qbarg0(RR,C,RN)     = QG0(RR,C,RN);
 qbarg(RR,C,RN)      = qbarg0(RR,C,RN);
 GADJ0         = 1;
 GSAV0         = SUM((RN,RN1),SAM(RN1,'S-I',RN,'GOV'));

*-LES calibration with lesiure in utility funtion===============================
*-calculate labor market parameters:

WHAVG0(RR,H)= WFR0(RR,'FLAB');
LESELASF(RR,H)= -0.35;
etals(RR,H)=0.3;


EXPLEI(RR1,H) =WHAVG0(RR1,H)*QFUE0(RR1,'FLAB');

*-budget share of cons comm in cons expenditure
 budshr(RR,C,RR1,H)$SAM(RR,C,RR1,H)    = SAM(RR,C,RR1,H)/(SUM((RR2,C1), SAM(RR2,C1,RR1,H))
 + SUM((RR3,A1), SAM(RR3,A1,RR1,H)));

 budshr2(RR,A,RR1,C,RR2,H2)$(SAM(RR1,C,RR2,H2) OR SAM(RR,A,RR2,H2)) = SAM(RR,A,RR2,H2)*SHRHOME(RR,A,RR1,C,RR2,H2)
                  /(SUM((R3,C1), SAM(R3,C1,RR2,H2)) + SUM((RR4,A1), SAM(RR4,A1,RR2,H2)));

 budshrchk(RR,H)   = SUM((RR1,C), budshr(RR1,C,RR,H)) + SUM((RR2,A,RR1,C), budshr2(RR2,A,RR1,C,RR,H));

*-budget share of cons coms in total expenditure including expenditure on leisure

 budshrlei(RR1,H)=EXPLEI(RR1,H)/(SUM((RR2,C1), SAM(RR2,C1,RR1,H))
 + SUM((RR3,A1), SAM(RR3,A1,RR1,H))+EXPLEI(RR1,H));

 budshrC(RR,C,RR1,H)$SAM(RR,C,RR1,H)    = SAM(RR,C,RR1,H)/(SUM((RR2,C1), SAM(RR2,C1,RR1,H))
 + SUM((RR3,A1), SAM(RR3,A1,RR1,H))+EXPLEI(RR,H));

 budshrC2(RR,A,RR1,C,RR2,H2)$(SAM(RR1,C,RR2,H2) OR SAM(RR,A,RR2,H2)) = SAM(RR,A,RR2,H2)*SHRHOME(RR,A,RR1,C,RR2,H2)
                  /(SUM((R3,C1), SAM(R3,C1,RR2,H2)) + SUM((RR4,A1), SAM(RR4,A1,RR2,H2))+EXPLEI(RR2,H2));

*-if error there is a problem about household expenditures!!!
 budshrleichk(RR,H)$(1-SUM((RR2,C), budshrC(RR2,C,RR,H))-(1-SUM((RR3,A,RR2,C), budshrC2(RR3,A,RR2,C,RR,H)))-budshrlei(RR,H) GT 0.0000001)=1/0;


 elaschk(RR,H)     = SUM((RR1,C), budshr(RR1,C,RR,H)*LESELAS1(RR,H,RR1,C))
                  + SUM((RR2,A,RR1,C), budshr2(RR2,A,RR1,C,RR,H)*LESELAS2(RR2,A,RR1,C,RR,H));
;


 LESELAS1(RR,H,RR1,C)$elaschk(RR,H)   = LESELAS1(RR,H,RR1,C)/elaschk(RR,H);
 LESELAS2(RR,A,RR1,C,RR2,H2)$elaschk(RR2,H2) = LESELAS2(RR,A,RR1,C,RR2,H2)/elaschk(RR2,H2);

*-error if elasticities does not satisfy Engel aggregation when leisure is included
 elaschkC(RR,H)$(abs(SUM((RR1,C), budshrC(RR1,C,RR,H)*LESELAS1(RR,H,RR1,C))
                  + SUM((RR2,A,RR1,C), budshrC2(RR2,A,RR1,C,RR,H)*LESELAS2(RR2,A,RR1,C,RR,H))+1*budshrlei(RR,H)-1) GT 0.0000001)
              =1/0;

*-calculate beta for leisure
betal(RR,H)=   QFS0(RR,'FLAB')*WHAVG0(RR,H)*LESELASF(RR,H) /
                      ( QFS0(RR,'FLAB')*WHAVG0(RR,H)*LESELASF(RR,H) - EH0(RR,H)) ;

*- frisch parameter is exogenous
frisch(RR,H)=-1.6 ;

*-Caution: will not work if there are more than one factors supplied by HH (skilled unskilled)

 betam(RR,C,RR1,H)   = (1- betal(RR1,H))*budshr(RR,C,RR1,H)*LESELAS1(RR1,H,RR,C);

 betah(RR,A,RR1,C,RR2,H2) =(1- betal(RR2,H2))* budshr2(RR,A,RR1,C,RR2,H2)*LESELAS2(RR,A,RR1,C,RR2,H2);

*-if error betal calibration is inconsistent with betam calibration!!!

 betalchk(RR1,H)$(abs(1-SUM((RR,C),betam(RR,C,RR1,H))-betal(RR1,H)) GT 1.0E-6)=1/0;

 gammam0(RR,C,RR1,H)$budshr(RR,C,RR1,H)
     = (SUM((RR3,C1), SAM(RR3,C1,RR1,H)) + SUM((RR2,A1), SAM(RR2,A1,RR1,H)))/PQ0(RR,C)
                     *( budshr(RR,C,RR1,H) + betam(RR,C,RR1,H)/((1-betal(RR1,H))*frisch(RR1,H)));

 gammah0(RR,A,RR1,C,RR2,H2)$budshr2(RR,A,RR1,C,RR2,H2)
     =  (SUM((R3,C1), SAM(R3,C1,RR2,H2)) + SUM((RR4,A1), SAM(RR4,A1,RR2,H2))) / PXAC0(RR,A,RR1,C)
                     *( budshr2(RR,A,RR1,C,RR2,H2) + betah(RR,A,RR1,C,RR2,H2)/((1-betal(RR2,H2))*frisch(RR2,H2)));

 gammam(RR,C,RR1,H)   =  gammam0(RR,C,RR1,H);

 gammah(RR,A,RR1,C,RR2,H2) =  gammah0(RR,A,RR1,C,RR2,H2);

 LABAVL0(RR,H)=QFS0(RR,'FLAB')-QFUE0(RR,'FLAB')+(betal(RR,H)/(1-betal(RR,H)))
                             *((EH0(RR,H)-SUM((RR1,C),gammam(RR1,C,RR,H)*PQ0(RR1,C)))/WHAVG0(RR,H));

*-Checking LES parameters===================================

 supernum(RR2,H2)  = SUM((RR,A,RR1,C),  gammah(RR,A,RR1,C,RR2,H2)*PXAC0(RR,A,RR1,C))
                      + SUM((RR4,C),gammam(RR4,C,RR2,H2)*PQ0(RR4,C)) ;

 frisch2(RR,H)$EH0(RR,H)   = -EH0(RR,H)/(EH0(RR,H) - SUPERNUM(RR,H));
 leschk(RR,H)$(ABS(frisch(RR,H) - frisch2(RR,H)) GT 0.00000001) = 1/0;

*$ONTEXT
*-LESELASP defines cross-price elasticities when c is different from C1 and
*-own-price elasticities when c and C1 refer to the same commodity.
*-Source: Dervis, de Melo and Robinson. 1982. General Equilibrium Models
*-for Development Policy. Cambridge University Press, p. 483
*-Cross-price elasticities

 LESELASP(RR,H,'MRK',RR2,C,'MRK',RR3,C1)
    $((ORD(C) NE ORD(C1)) AND LESELAS1(RR,H,RR2,C) AND LESELAS1(RR,H,RR3,C1))
  = -LESELAS1(RR,H,RR2,C)* PQ0(RR3,C1)*gammam(RR3,C1,RR,H) / (SUM((RR4,C2), SAM(RR4,C2,RR,H)) + SUM((RR5,A2), SAM(RR5,A2,RR,H)));

 LESELASP(RR,H,A,RR2,C,'MRK',RR3,C1)
    $((ORD(C) NE ORD(C1)) AND LESELAS2(RR,A,RR2,C,RR,H) AND LESELAS1(RR,H,RR3,C1))
  = -LESELAS2(RR,A,RR2,C,RR,H)*PQ0(RR3,C1)*gammam(RR3,C1,RR,H) / (SUM((RR4,C2), SAM(RR4,C2,RR,H)) + SUM((RR5,A2), SAM(RR5,A2,RR,H)));

 LESELASP(RR,H,'MRK',RR1,C,A,RR3,C1)
    $((ORD(C) NE ORD(C1)) AND LESELAS1(RR,H,RR1,C) AND LESELAS2(RR,A,RR3,C1,RR,H))
  = -LESELAS1(RR,H,RR1,C)* PXAC0(RR,A,RR3,C1)*gammah(RR,A,RR3,C1,RR,H) /(SUM((RR4,C2), SAM(RR4,C2,RR,H)) + SUM((RR5,A2), SAM(RR5,A2,RR,H)));

*-Own-price elasticities

 LESELASP(RR,H,'MRK',RR1,C,'MRK',RR1,C)
   = -LESELAS1(RR,H,RR1,C)
     *( PQ0(RR,C)*gammam(RR1,C,RR,H) / (SUM((RR2,C1), SAM(RR2,C1,RR,H)) + SUM((RR3,A1), SAM(RR3,A1,RR,H)))
                                                       - 1/FRISCH(RR,H));

 LESELASP(RR,H,A,RR2,C,A,RR2,C)
   = -LESELAS2(RR,A,RR2,C,RR,H)
     *( PXAC0(RR,A,RR2,C)*gammah(RR,A,RR2,C,RR,H) / (SUM((RR3,C1), SAM(RR3,C1,RR,H)) + SUM((RR4,A1), SAM(RR4,A1,RR,H)))
                                                       - 1/FRISCH(RR,H));
*check cournot aggregation

courn(RR3,C1,RR,H)=SUM((RR2,C),budshr(RR2,C,RR,H)*LESELASP(RR,H,'MRK',RR2,C,'MRK',RR3,C1));
courn(RR,C,RR1,H)$(ABS(budshr(RR,C,RR1,H)+courn(RR,C,RR1,H)) GT 1.0E-6) = 1/0;


OPTION LESELASP:3:2:2;

;

*$OFFTEXT
*-System-constraint block =========================

*-Fixed investment
 qbarinv(RR,C)$CINV(RR,C) = SUM(R1,SAM(RR,C,R1,'S-I'))/PQ0(RR,C);
 QINV0(RR,C)           = qbarinv(RR,C);
 IADJ0              = 1;
*-Stock changes
 qdst0(RR,C)$PQ0(RR,C) = (SUM(R1,SAM(RR,C,R1,'S-I'))$(NOT CINV(RR,C)) + SUM(R1,SAM(RR,C,R1,'DSTK')))/PQ0(RR,C);
 qdst(RR,C)         = qdst0(RR,C);

*- FSAV0     = SAM('S-I','ROW')/EXR0;
 FSAV0(ROW)     =sum((R,R1),SAM(R,'S-I',R1,ROW))/EXR0(ROW);
 TFSAV0         = SUM(ROW, FSAV0(ROW)/EXR0(ROW));

 TABS0         = SUM((RR,C,RR1,H), SAM(RR,C,RR1,H)) + SUM((RR,A,RR2,H2), SAM(RR,A,RR2,H2))
                 + SUM((RR,C,RN), SAM(RR,C,RN,'GOV')) + SUM((RR,C,RN), SAM(RR,C,RN,'S-I'))
                 + SUM((RR,C,RN), SAM(RR,C,RN,'DSTK'));

 INVSHR0       = SUM(RN,SAM('TOT','TOTAL',RN,'S-I'))/TABS0;
 GOVSHR0       = SUM((RR,C,RN), SAM(RR,C,RN,'GOV'))/TABS0;

 WALRAS0       = 0;

*-END OF CALIBRATION
