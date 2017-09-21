$TITLE Diagnostics
$STITLE Turkish Agricultural CGE Model - Regional.
$STITLE Based on IFPRI Standard CGE modeling system, Version 1.01
$ONTEXT
*=============================================================================
* File      : 20Diag.gms
* Author    : Hasan Dudu (hasan@dudu.gen.tr)
* Version   : 1.0
* Date      : 15/08/2011 14:05:20
* Changed   : 13/12/2013 18:08:24
* Changed by: Hasan Dudu (hasan@dudu.gen.tr)
* Remarks   :
This file runs diagnostic tests on the data. Note that susing 1/0 for error check
is not a good practice. Normally you shuld use $ABORT statements and give
error messages!
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

This is an optional include file for mod.gms. It displays and generates
information that may be useful when debugging a model. Some of this
information was generated in MOD.GMS and its include files. Some of
the information is created in this file.


*-Diagnostics=====================================

COMMENTS ON THE INFORMATION THAT APPEARS IN THE FILE DIAGNOSTICS.INC
====================================================================
This file includes the following sections (with searchable headings):

ILLEGAL SAM PAYMENTS OR VALUES

Error message if illegal payment flows appear in SAM
(which simply may reflect errors in data entry). Solutions if errors are
generated:
(1) change/restructure the SAM (typically the preferred approach);
(2) restructure the model (typically more complicated).

The model structure assumes that imports are not reexported, i.e. that
exports stem from domestic production. Accordingly, an error message is
generated if, for any commodity, exports exceeds domestic production.
Solutions if errors are generated:
(1) if there is data error, correct it;
(2) if substantial reexportation takes place, create a reexporting
activity that, as inputs, use primary factors and an imported
intermediate commmodity without domestic production. All of its outputs
should be exported.


SAMBALCHK

If the SAM that is used as the model database is unbalanced, the model
will fail to replicate the SAM and may not solve. Such imbalances are
typically due to errors made when introducing changes in the SAM after
the running of the SAM-balancing program in SAMBAL.INC.


NEGCELL

For a subset of the cells, negative entries are legal. However, entries
that are negative as a result of coding mistakes are a common source of
errors. The two-dimensional NEGCELL set has elements for all cells with
negative entries. Verify that there are no errors.


MAXCELL, MINCELL, LARGECELL AND SMALLCELL

A badly scaled SAM may make it more difficult to solve the model. The
parameters MAXCELL and MINCELL displays the maximum and minimum absolute
cell values in the SAM. The two-dimesional sets LARGECELL and SMALLCELL
indicate the payments flows in the SAM that are 'large' and 'small',
respectively.

If the value of MAXCELL exceeds 10**3 the user may consider
rescaling the SAM in the country data file (by dividing all SAM values
by 10 raised to a power such that the maximum cell value is less than
10**3.

Rescaling will reduce small cell values which may be another source of
difficulties. To eliminate small (absolute) cell values in the SAM
balancing program (in SAMBAL.INC) by increasing the value of the scalar
CUTOFF above zero.


SMALL TRADE AND VALUE-ADDED SHARES

On the basis of the information in the SAM, model parameters define
--commodity trade shares (for each commodity, the share of exports in
total output, and the shares of import in total domestic demand); and
--activity value-added shares (the share of each factor in the value-added
of each activity in which the factor is used).

If these shares are small (let's say smaller than 0.001), evaluation
errors are likely.

If trade shares are small, one remedy is to eliminate the payment from
the SAM (since it is small, it is typically unlikely to have much
influence on the results). The same approach may be applied for the case
of small value-added shares. For both trade and value-added shares, the
problem may be mitigated if the SAM is aggregated (across activities,
commodities and/or factors).

Similarly, the presence of activities that have a very small share of
total economywide value-added may be a source of errors since such
activities tend to have very small quantities for factor demands,
intermediate demands, and output. In this case, the recommended action
is to aggregate across activities.

For aggregation, the file SAMAGG.GMS may be used.

MISSING OR INCORRECT NON-SAM DATA

Error messages will be generated if data are missing or incorrect.
Examples of sources of errors: missing trade or production elasticities;
data for factor quantity use only supplied for a subset of the
activities that, according to the SAM, use a given factor; non-SAM share
data that should sum to unity do not.

$OFFTEXT


*- ILLEGAL SAM PAYMENTS OR VALUES============================

PARAMETER
 ERRSAMFLOW(RALL,AC,RALL1,AC1) if UNDF -- illegal SAM payment from 2nd index to 1st
 ERREXPOUT(C)       if UNDF -- exports exceed domestic output for comm c
 ;

*-Activities (A) are not permitted to pay to institutions (INS) or other
*-activities (AP).
 ERRSAMFLOW(R,INS,RR1,A)$SAM(R,INS,RR1,A)     = 1/0;
 ERRSAMFLOW(RR,A,RR1,A1)$SAM(RR,A,RR1,A1)       = 1/0;

*-Commodities (C) are not permitted to pay to factors (F) or domestic
*-institutions (INSD).
 ERRSAMFLOW(RR,F,RR1,C)$SAM(RR,F,RR1,C)         = 1/0;
 ERRSAMFLOW(R,INSD,RR1,C)$SAM(R,INSD,RR1,C)   = 1/0;

*-Factors (F) are not permitted to pay to activities (A), commodities (C)
*-or other factors (F1).
 ERRSAMFLOW(RR,A,RR1,F)$SAM(RR,A,RR1,F)         = 1/0;
 ERRSAMFLOW(RR,C,RR1,F)$SAM(RR,C,RR1,F)         = 1/0;
 ERRSAMFLOW(RR,F,RR1,F1)$SAM(RR,F,RR1,F1)       = 1/0;

*-Domestic institutions (INSD) are not permitted to pay to factors (F).
 ERRSAMFLOW(RR,F,R1,INSD)$SAM(RR,F,R1,INSD)   = 1/0;

*-Government (GOV) and the rest of the world (ROW) are not permitted to
*-pay to activities (A).
 ERRSAMFLOW(RR,A,R1,'GOV')$SAM(RR,A,R1,'GOV') = 1/0;
 ERRSAMFLOW(RR,A,R1,ROW)$SAM(RR,A,R1,ROW) = 1/0;

*-Enterprises (EN) are not permitted to pay to activities (A) or
*-commodities (C).
 ERRSAMFLOW(RR,A,R1,EN)$SAM(RR,A,R1,EN)       = 1/0;
 ERRSAMFLOW(RR,C,R1,EN)$SAM(RR,C,R1,EN)       = 1/0;

SET
 SIREC(AC) set for elements permitted to receive payment from S-I;
 SIREC(C) = YES;  SIREC('DSTK') = YES;

*-Savings-investment (S-I) account is only permitted to pay to accounts
*-for commodities (C) and stock change (DSTK)
 ERRSAMFLOW(R,ACNT,R1,'S-I')$(SAM(R,ACNT,R1,'S-I') AND (NOT SIREC(ACNT))) = 1/0;


*-Savings-investment (S-I) account is only permitted to receive payments
*-from institutions (INS)
 ERRSAMFLOW(R,'S-I',RR1,ACNT)$(SAM(R,'S-I',RR1,ACNT) AND (NOT INS(ACNT)))       = 1/0;

*-Stock change account (DSTK) is only permitted to receive payments from
*-savings-investment (S-I) and make payments to commodities (C).
 ERRSAMFLOW(R,'DSTK',RR1,ACNT)$(SAM(R,'DSTK',RR1,ACNT) AND (NOT ACNT('S-I')))   = 1/0;
 ERRSAMFLOW(R,ACNT,R1,'DSTK')$(SAM(R,ACNT,R1,'DSTK') AND (NOT C(ACNT)))       = 1/0;

*-It is illegal to have import tariffs for commodities without imports
*- ERRSAMFLOW('IMPTAX',C)$(TAXPAR('IMPTAX',C) AND (NOT SAM('ROW',C))) = 1/0;
 ERRSAMFLOW(RN,IMTX,RR1,C)$(TAXPAR(RN,IMTX,RR1,C) AND (NOT SUM(ROW$MAPROWIMP(ROW,IMTX),SAM(RN,ROW,RR1,C)))) = 1/0;
*- ERRSAMFLOW(R,'IMPTAXNEU',RR1,C)$(TAXPAR(R,'IMPTAXNEU',RR1,C) AND (NOT SAM(R,'NEU',RR1,C))) = 1/0;

*-It is illegal to have export taxes for commodities without exports
*- ERRSAMFLOW('EXPTAX',C)$(TAXPAR('EXPTAX',C) AND (NOT SAM(C,'ROW'))) = 1/0;
 ERRSAMFLOW(RN, EXTX,RR1,C)$(TAXPAR(RN,EXTX,RR1,C) AND (NOT SUM(ROW$MAPROWEXP(ROW,EXTX),SAM(RR1,C,RN,ROW)))) = 1/0;
*- ERRSAMFLOW(R, 'EXPTAXNEU',RR1,C)$(TAXPAR(R,'EXPTAXNEU',RR1,C) AND (NOT SAM(RR,C,R1,'NEU'))) = 1/0;


*-Value of exports is greater than the value of domestic production. Note that
*-exports can equal to value of domestic production, so test includes 1E-6.
 ERREXPOUT(C)$(SUM((ROW,RR,R1), SAM(RR,C,R1,ROW)) GT (SUM((RR,A,RR1), SAM(RR,A,RR1,C))
+ SUM((RN,EXTX,RR1),TAXPAR(RN,EXTX,RR1,C)) + SUM((CTE,R,RR1), SAM(R,CTE,RR1,C)) + 1.E-6)) = 1/0 ;

DISPLAY
"The model structure assumes that exports are produced domestically,"
"i.e., not reexported imports. The model will not work if exports"
"exceed domestic output."
;


*-SAMBALCHK=================================================

PARAMETER
 SAMGAPCUTOFF  max acceptable abs gap bt model SAM row and col totals
 ERRSAMBAL(R,AC) if UNDF -- the absolute imbalance for AC exceeds cutoff
 ;

 SAMGAPCUTOFF = 1.0e-5;

 ERRSAMBAL(R,AC)$(ABS(SAMBALCHK(R,AC)) GT SAMGAPCUTOFF) = 1/0;

DISPLAY
"Note: The last definition of SAMBALCHK is in the MOD.GMS file. To find"
"it search for 'SAM after final adjustments'."
""

"Note: If the SAM that is used as the model database is unbalanced"
"the model will fail to replicate the SAM and may not solve."
"Here absolute imbalance values in excess of the value of SAMGAPCUTOFF"
"generate an error for the parameter ERRSAMBAL."
 ;


*-NEGCELL===================================================

$ONTEXT
Check on negative SAM entries It is legal to have negative entries in a
SAM. However, entries that are negative as a result of coding mistakes
are a common source of errors. The following set has elements for all
cells with negative entries. Verify that there are no errors.
$OFFTEXT

SET
 NEGCELL(R,AC,RR1,AC1) the SAM payment from AC1 (column) to AC (row) is negative
 ;

 NEGCELL(R,AC,RR1,AC1)$(SAM(R,AC,RR1,AC1) LT 0) = YES;

DISPLAY
"Note: Unintended negative cell entries can be a source of errors."
 ;


*-MAXCELL, MINCELL, LARGECELL AND SMALLCELL=================

$ONTEXT

A badly scaled SAM may make it more difficult to solve the model. The
following segment generates information about maximum and minimum
absolute cell values as well as the location of large and small cells.
Note that no action is taken and no errors generated. This section only
provides information.

If the maximum absolute cell value exceeds 10**3 the user may consider
rescaling the SAM in the country data file (by dividing all SAM values
by 10 raised to a power such that the maximum cell value is less than
10**3). This may be done immediately after that the SAM parameter is
defined.

Rescaling will also reduce small cell values which may be another source
of difficulties. If so, the user may activate the option of eliminating
small (absolute) cell values in the SAM balancing program (in
SAMBAL.INC) by increasing the value of the scalar CUTOFF above zero.

$OFFTEXT

SCALAR
 MAXCELL     maximum absolute cell value in SAM (excluding totals)
 MINCELL     minimum absolute non-zero cell value in SAM
;

 MAXCELL = SMAX((R,ACNT,RR1,ACNT1), ABS( SAM(R,ACNT,RR1,ACNT1) ));
 MINCELL = SMIN((R,ACNT,RR1,ACNT1)$SAM(R,ACNT,RR1,ACNT1), ABS( SAM(R,ACNT,RR1,ACNT1) ));


SET
 LARGECELL(R,AC,RR1,AC1) cells with absolute values larger than cellcutoffup
 SMALLCELL(R,AC,RR1,AC1) cells with absolute values smaller than cellcutofflo
 ;

PARAMETER
 cellcutoffup absolute cell values above cutoff are displayed as large
 cellcutofflo absolute cell values below cutoff are displayed as small
 ;

*-No cells are large if cellcutoffup is set at +INF.
*-cellcutoffup = +INF;
 cellcutoffup = 1000;

*-No cells are small if cellcutofflo is set at zero.
*-cellcutofflo = 0;
 cellcutofflo = 0.01;

 LARGECELL(R,AC,RR1,AC1)$(ABS(SAM(R,AC,RR1,AC1)) GT cellcutoffup) = YES;

 SMALLCELL(R,AC,RR1,AC1)$(SAM(R,AC,RR1,AC1) AND (ABS(SAM(R,AC,RR1,AC1)) LT cellcutofflo))
   = YES;


DISPLAY
"Note: A (badly scaled) SAM with very large and/or very small (absolute)"
"cell values can be a source of errors. 'Large' and 'small' values are"
"perhaps best defined as larger and smaller than 10000 and 0.01, resp."
"Here cells with values above/below cellcutoffup/cellcutofflo are"
"defined as large/small."
 ;


*-SMALL TRADE AND VALUE-ADDED SHARES========================

PARAMETERS
 ACTFACSHR(RR,F,RR1,A) small share of factor f in total value-added of activity a
 ACTVASHR(A)    small share of activity a in total economywide value-added
 EXPSHR(C)      small share of exports in total output of commodity c
 IMPSHR(C)      small share of imports in total domestic demand for commodity c
 ;

*- ACTFACSHR(F,A) = SAM(F,A) / SUM(F1, SAM(F1,A));
*- ACTVASHR(A)    = SUM(F, SAM(F,A)) / SUM((F,AP), SAM(F,AP))
 ACTFACSHR(RR,F,RR1,A) $SUM((R2,F1), SAM(R2,F1,RR1,A)) = SAM(RR,F,RR1,A) / SUM((R3,F1), SAM(R3,F1,RR1,A));
 ACTVASHR(A)$SUM((RR,F,RR1,A1), SAM(RR,F,RR1,A1)) = SUM((RR1,R2,F), SAM(R2,F,RR1,A)) / SUM((R2,F,RR1,A1), SAM(R2,F,RR1,A1));

*- EXPSHR(C)$SUM(A, SAM(A,C))
*-  = (SAM(C,'ROW') - TAXPAR('EXPTAX',C) - SUM(CTE, SAM(CTE,C)))
*-    / SUM(A, SAM(A,C));
*-
*- IMPSHR(C)$(SAM(C,'TOTAL') - SAM(C,'ROW') - TAXPAR('COMTAX',C))
*-  = (SAM('ROW',C) + TAXPAR('IMPTAX',C) + SUM(CTM, SAM(CTM,C)))
*-    / (SAM(C,'TOTAL') - SAM(C,'ROW') - TAXPAR('COMTAX',C));

 EXPSHR(C)$SUM((RR,RR1,A), SAM(RR,A,RR1,C))
  = (SUM((RR,RN,ROW),SAM(RR,C,RN,ROW)) - SUM((RN,RR1,EXTX),TAXPAR(RN,EXTX,RR1,C)) - SUM((R,RR1,CTE), SAM(R,CTE,RR1,C)))
    / SUM((RR,RR1,A), SAM(RR,A,RR1,C));

 IMPSHR(C)$(SUM((RR,R1),SAM(RR,C,'TOT','TOTAL')) - SUM((RR,R1,ROW), SAM(RR,C,R1,ROW)) - SUM((RN,COMTX,RR1),TAXPAR(RN,COMTX,RR1,C)))
  = (SUM((RN,RR1,ROW),SAM(RN,ROW,RR1,C)) + SUM((RN,RR1,IMTX),TAXPAR(RN,IMTX,RR1,C)) + SUM((R,RR1,CTM), SAM(R,CTM,RR1,C)))
    /( SUM((RR,R1),(SAM(RR,C,'TOT','TOTAL'))) - SUM((RR,R1,ROW),SAM(RR,C,R1,ROW)) - SUM((RN,COMTX,RR1),TAXPAR(RN,COMTX,RR1,C)));

SET
 SHRITEM items for which shares and cutoffs are defined
 /FAC factor in activity, VA value-added, EXP exports , IMP imports/

PARAMETER
 SHRCUTOFF(SHRITEM)  upper limit for shares values that are displayed
 ;
 SHRCUTOFF('FAC') = 0.05;
 SHRCUTOFF('VA')  = 0.05;
 SHRCUTOFF('EXP') = 0.05;
 SHRCUTOFF('IMP') = 0.05;

*-If SHRCUTOFF is set at 1 or larger, no shares are eliminated from display
*-SHRCUTOFF(SHRITEM) = 1;

*-Eliminating shares that are above the cutoff from display.
 ACTFACSHR(RR,F,RR1,A)$(NOT (ACTFACSHR(RR,F,RR1,A) LT SHRCUTOFF('FAC'))) = 0;
 ACTVASHR(A)$(NOT (ACTVASHR(A) LT SHRCUTOFF('VA')))        = 0;
 EXPSHR(C)$(NOT (EXPSHR(C) LT SHRCUTOFF('EXP')))           = 0;
 IMPSHR(C)$(NOT (IMPSHR(C) LT SHRCUTOFF('IMP')))           = 0;

DISPLAY

"Small shares may be a source of function evaluation errors in model"
"simulations. For the following parameters (ACTFACSHR, ACTVASHR,"
"EXPSHR, IMPSHR), shares smaller than SHRCUTOFF shrcutoff are defined"
"as 'small'."
""

"'Small' shares are displayed for ACTFACSHR, ACTVASHR, EXPSHR, IMPSHR."
;

*-MISSING OR INCORRECT NON-SAM DATA=========================

*-Error messages will be generated if data are missing or incorrect.

PARAMETERS
 ERRHOME(RR,A,RR1,H)    act with home cons comm shares not summing to one
 ERRQFBASE1(RR,F,RR1,A) demand specified for factor but no SAM payment
 ERRQFBASE2(RR,F,RR1,A) demands specified but missing for f-a combination in SAM
 ERRTRADE(RR,C,TRDELAS)  commodities with missing trade elas
 ERRPROD1(RR,A)     activities with missing factor substitut elas
 ERRPROD2(RR,A)     CES activities with missing agg va - intermed elas
 ERRAC(RR,C)        commodities with missing domestic aggregation elas
 ERRLES1(RR,C,RR1,H)    marketed commodities with missing hhd cons elas
 ERRLES2(RR,A,RR1,C,RR2,H)  home comm's with shrhome but missing hhd cons elas
  ;

*-Parameter with error in data provided: SHRHOME(A,C,H)
 ERRHOME(RR,A,RR2,H)$(SAM(RR,A,RR2,H)$(SUM((RR1,C), SHRHOME(RR,A,RR1,C,RR2,H))
                      $((SUM((RR1,C), SHRHOME(RR,A,RR1,C,RR2,H)) NE 1)))) = 1/0;

*-Parameter with missing data: SAM (or error in QFBASE)
 ERRQFBASE1(RR,F,RR1,A)$(QFBASE(RR,F,RR1,A)$(NOT SAM(RR,F,RR1,A))) = 1/0;

*-Parameter with missing data: QFBASE
 ERRQFBASE2(RR,F,RR1,A)
   $(SAM(RR,F,RR1,A)$((NOT QFBASE(RR,F,RR1,A))$SUM((RR2,A1), QFBASE(RR,F,RR2,A1)))) = 1/0;



*-Parameter with missing data: TRDELAS
 ERRTRADE(RR,C,'SIGMAQ')$((CM(RR,C)AND CD(RR,C))$(TRADELAS(RR,C,'SIGMAQ')EQ 0)) = 1/0;
 ERRTRADE(RR,C,'SIGMAT')$((CE(RR,C)AND CD(RR,C))$(TRADELAS(RR,C,'SIGMAT')EQ 0)) = 1/0;

*-Parameters with missing data: PRODELAS and-or PRODELAS2
 ERRPROD1(RR,A)$(PRODELAS(RR,A) EQ 0)        = 1/0;
 ERRPROD2(RR,A)$(ACES(RR,A) AND (PRODELAS2(RR,A) EQ 0)) = 1/0;

*-Parameter with missing data: ELASAC
 ERRAC(RR,C)$(SUM((RR1,A), SAM(RR1,A,RR,C))$(ELASAC(RR,C) EQ 0)) = 1/0;

*-Parameter with missing data: LESELAS1
 ERRLES1(RR,C,RR1,H)$(SAM(RR,C,RR1,H)$(LESELAS1(RR,H,RR1,C) EQ 0)) = 1/0;

*-Parameter with missing data: LESELAS2
 ERRLES2(RR,A,RR1,C,RR2,H)$(SHRHOME(RR,A,RR1,C,RR2,H)$(LESELAS2(RR,A,RR1,C,RR2,H) EQ 0)) = 1/0;

DISPLAY

"Note: For ERRHOME, the error is due to user-defined values for"
"SHRHOME. If the user does not define any values, the program generates"
"data for SHRHOME. These are correct as long as home consumption is for"
"commodities produced by single-output activites."
;


*-#*#*#*#*# THE END OF DIAGNOSTICS.INC #*#*#*#*
