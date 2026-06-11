     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H*****************************************************************
     H/TITLE W0065R - PREMIUM HISTORY SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*    THIS PROGRAM DISPLAYS A LIST OF THE PREMIUM ENDORSEMENT    *
     H*    HISTORY RECORDS FOR THE REQUESTED POLICY.  AN INDIVIDUAL   *
     H*    RECORD CAN BE SELECTED TO VIEW WRITTEN AND INFORCE         *
     H*    PREMIUM.  THE LAST RECORD CAN BE DELETED IF IT WAS         *
     H*    WRITTEN IN THE CURRENT MONTH.                              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R - WORK WITH EMPLOYER POLICY YEARS                   *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*    PROGRAMMER: C:24                                           *
     H*    CREATED:    11/08/95     PROJECT: WC311                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/13/11 C:061 PROJECT: WC2037                                *
     H*                Display "Unearned Prem Surcharge" after
     H*                "Expense constant".                            *
     H*                                                               *
     H* 03/16/11 C:190 PROJECT: 132WPI                                *
     H*                Print the new optional heading field for       *
     H*                the contractor's credit line item if it is     *
     H*                not blank(client 132 only).                   *
     H*                                                               *
     H* 01/01/11 C:136 PROJECT: 71WLD PRISS 49                        *
     H*                CLIENT SPECIFIC - 3RD PARTY PREMIUM ENTRY      *
     H*                FIELD NOT DISPLAYED                            *
     H*                                                               *
     H* 12/31/10 C:255 Project: 71WLD (PRISS#44)                      *
     H*                Cleared TMP array after moving in HDG arrary.  *
     H*                                                               *
     H* 12/17/10 C:255 Project: 71WLD                                 *
     H*                Added code for fields Audited and Estimated non*
     H*                premium.                                       *
     H*                                                               *
     H* 12/06/10 C:255 Support: 75037                                 *
     H*                Used fields                                    *
     H*                1) EHD1W2 instead EHWDC2.                      *
     H*                2) EHD2W2 instead EHWDV2.                      *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                Add new line item for HCN credit for GIC.      *
     H*                This credit will be applied just before subject*
     H*                premium.                                       *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                FOR SPLIT DEVIATION.                           *
     H*                1) USE RRDV1H WITH PREFIX '2ND PRD'            *
     H*                2) USE FIELDS EHD1S2,EHD2S2 SAME AS            *
     H*                   EHASP2,EHDEV2                               *
     H*                                                               *
     H* 05/04/10 C:65  PROJECT: WC1838                                *
     H*                SHIFT THE LARGE DEDUCTIBLE APPLICATION AT 'S'  *
     H*                LEVEL DOWN TO BELOW ADDITIONAL LOAD AT 'S'.    *
     H*                                                               *
     H* 04/20/10 C:65  PROJECT: WC1838                                *
     H*                SHIFT THE LARGE DEDUCTIBLE APPLICATION TO BE   *
     H*                LAST ELEMENT AT EACH PREMIUM LEVEL.            *
     H*                                                               *
     H* 04/14/10 C:65  PROJECT: WC1838                                *
     H*                ENSURE SMALL DEDUCTIBLE LINE ITEMS DON'T PRINT *
     H*                FOR LARGE DEDUCTIBLE POLICIES.                 *
     H*                                                               *
     H* 01/14/10 C:191 PROJECT: 25WRC                                 *
     H*                BILLING ALGORITHM RETROFIT                     *
     H*                                                               *
     H* 12/30/09 C:232 PROJECT: WC1838                                *
     H*                MODIFIED FOR THE LARGE DEDUCTIBLE PROCESS.     *
     H*                                                               *
     H* 03/02/09 C:65  PROJECT: 81WEI                                 *
     H*                Use optional headings for terrorism charges.   *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                                                               *
     H* 08/07/07 C:65  PROJECT: WC1067/SUPP# 59393                    *
     H*                ADD IN EXP CONSTANT TO ANNUAL PREMIUM FIGURE   *
     H*                ON MINIMUM PREMIUM POLICY IF BREAKING IT OUT.  *
     H*                                                               *
     H* 01/15/07 C:136 PROJECT 133WCV                                 *
     H*                Copied project in from V6R1 version. Adjusted  *
     H*                CON heading array element to since 132WEW      *
     H*                used the array element assigned to 133WCV in   *
     H*                V6R1.                                          *
     H*                                                               *
     H* 07/06/06 C:168 PROJECT: 132WEW                                *
     H*                Added "Facultative Reinsurance Charge" line    *
     H*                                                               *
     H* 04/12/06 C:132 PROJECT: WC1417                                *
     H*                Change "Terrorism Risk Act" in table at end to *
     H*                "Foreign Terrorism", no other changes to code. *
     H*                                                               *
     H* 01/24/06 C:65  PROJECT 114WHO                                 *
     H*                ADD PROCESSING TO DISPLAY RETRO PREMIUM RCDS.  *
     H*                ALSO, CONDITION EXPENSE CONSTANT TO NOT        *
     H*                DISPLAY IF POLICY HAS NO PREMIUM.              *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC:  ADD COMMISSION MULTIPLIER    *
     H*                TO PREMIUM CALCULATION.                        *
     H*                                                               *
     H* 04/07/05        PROJECT: 126WDK                               *
     H*          C:33   PROJECT: 126WCD                               *
     H*                 Multiple Deductible discounts per Year.       *
     H*                                                               *
     H* 01/07/05 C:136  PROJECT WC1250                                *
     H*                 DISPLAY DOMESTIC TERRORISM CHARGE.            *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                ADD VOLUNTARY LABOR CHARGE.                    *
     H*                                                               *
     H* 08/20/03 C:65  PROJECT: SUPPORT                               *
     H*                DO NOT LOAD ANNUALIZED EXP CNST FOR AUDITS.    *
     H*                                                               *
     H* 01/27/03 C:136 PROJECT: WC744                                 *
     H*                PRINT A LINE ITEM FOR MASS ARAP SURCHARGE      *
     H*                                                               *
     H* 01/17/03 C:65  PROJECT: WC1120                                *
     H*                PRINT A LINE ITEM FOR TERRORISM RISK ACT.      *
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT SUPP#33855                              *
     H*               LOAD ANNUAL EXPENSE CONSTANT INTO EMPLR HISTORY *
     H*                                                               *
     H* 10/01/02 C:61  SUPPORT WA099                                  *
     H*                Show Agent#, Prod#, CX date in header.         *
     H*                                                               *
     H* 09/20/02 C:65 PROJECT: CM045/#33798                           *
     H*               BILL WAIVER SURCHARGE ON TOP OF MINIMUM PREM.   *
     H*                                                               *
     H* 06/25/02 C:61  SUPPORT WA112 33135                            *
     H*                Correct paging issue.                          *
     H*                                                               *
     H* 05/03/02 C:61  SUPPORT WA107 32374                            *
     H*                Display opt 5 in inquiry mode.                 *
     H*                                                               *
     H* 04/04/02  C:65  PROJECT SUPPORT                               *
     H*                 CALL PGM SRFEXP TO FIND THE ANNUAL EXP CNST.  *
     H*                                                               *
     H* 10/04/01  C:65  PROJECT WC1043                                *
     H*                 APPLY CONTRACTORS CREDIT IMMEDIATELY AFTER    *
     H*                 EXPERIENCE MODIFIER, INSTEAD OF JUST BEFORE   *
     H*                 STANDARD PREMIUM.                             *
     H*                                                               *
     H* 10/01/01  C:65  PROJECT WC1045                                *
     H*                 PRINT THE LOSS CONSTANT IF NEEDED.            *
     H*                                                               *
     H* 08/20/01 C:90  PROJECT: WC1041                                *
     H*                Expanded percentage from 3.2 to 5.3            *
     H*                                                               *
     H* 01/18/01 C:61  SUPPORT WA093 25245                            *
     H*                Do not allow opt "4=Delete".                   *
     H*                                                               *
     H* 10/25/99  C:65  PROJECT WC908                                 *
     H*                 REMOVE UNNECESSARY REFFERENCE TO WDF1P        *
     H*                                                               *
     H* 10/13/99  C:65  PROJECT WC942                                 *
     H*                 INCLUDE FULL EXPENSE CONSTANT IN NORMAL PREM  *
     H*                                                               *
     H* 09/29/99  C:65  PROJECT CM045                                 *
     H*                 CHANGE WAIVER PROCESS TO CHECK PREM FLAG AT   *
     H*                 THE EMPLOYER DETAIL LEVEL.                    *
     H*                                                               *
     H* 07/22/99  C:65 PROJECT:  WC577                                *
     H*                DISPLAY AMOUNT OF SCHEDULE DB/CR RESULT ON     *
     H*                THE LINE ITEM.                                 *
     H*                                                               *
     H* 06/01/99  C:65  PROJECT: CM045                                *
     H*                 ADD WAIVER OF SUBROGATION AND VARIABLE        *
     H*                 PLACEMENT OF ADDITIONAL LOAD.                 *
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H*  04/01/98  C:78  PROJECT: WC825                               *
     H*                  ALLOW ACCESS TO DARTS. F14                   *
     H*                                                               *
     H*  03/03/98  C:65  PROJECT: WC705                               *
     H*                  REMOVED AGENT AND PRODUCER TO MAKE ROOM FOR  *
     H*                  SCHEDULE DB/CR, 2ND INCR LIMITS, AND DEV #2  *
     H*                                                               *
     H*  02/09/98  C:65  PROJECT: WC705                               *
     H*                  ADD OPTIONAL HEADING FOR STOCK DISCOUNT.     *
     H*                                                               *
     H*  01/01/98  C:65  PROJECT: WC705                               *
     H*                  CORRECT EXPENSE CONSTANT FOR BY STATE.       *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*  09    INQUIRY                                                *
     H*  11    VALID COMMAND KEY                                      *
     H*                                                               *
     H* SCR01:                                                        *
     H*                                                               *
     H*  01    SFLDSP, SFLDSPCTL                                      *
     H*  02    SFLINZ                                                 *
     H*  03    SFLDLT                                                 *
     H*  04    SFLCLR                                                 *
     H*  05    SFLEND                                                 *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY SELECT FIELDS   *
     H*  19    SFLNXTCHG                                              *
     H*  20    HIGHLIGHT RECORD IN ERROR                              *
     H*  21    INVALID OPTION SELECTED                                *
     H*  23    RECORD NOT FOUND                                       *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  48    AT LEAST ONE RECORD DELETION REQUESTED                 *
     H*  49    AT LEAST ONE NON-BLANK OPTION ENTERED                  *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H*  01    SFLDSP, SFLDSPCTL                                      *
     H*  02    SFLINZ                                                 *
     H*  03    SFLDLT                                                 *
     H*  04    SFLCLR                                                 *
     H*  06    SFLEND                                                 *
     H*  19    SFLNXTCHG                                              *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*  15    PAGE UP                                                *
     H*  16    PAGE DOWN                                              *
     H*  20    CANNOT ADD OR UPDATE RECORD                            *
     H*  40    ADD MODE                                               *
     H* N40    UPDATE MODE                                            *
     H*  86    INVALID ROLL                                           *
     H*  87    FIRST/SECOND PAGE                                      *
     H*****************************************************************
     FW0065F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN1)
     F                                     SFILE(SCR02S:RRN2)
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F*** BEGIN DEL ***  10/13/99  ************************************
     F*****WDECL   IF  E           K        DISK
     F*** END DEL *****  10/13/99  ************************************
     FWDELP     IF   E             DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     F*** BEGIN DEL ***  10/25/99  ************************************
     F*****WDF1L   IF  E           K        DISK
     F*** END DEL *****  10/25/99  ************************************
     FWMEML     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWTLML1    IF   E           K DISK
      ***** BEG CHG ******* 06/25/02 **********************************
     F********WDEHL3  UF  E           K        DISK
     FWDEHL14   UF   E           K DISK
      ***** END CHG ******* 06/25/02 **********************************
     FSSYLF     IF   E           K DISK
     F*** BEGIN ADD ***  08/07/07  ************************************
     FWMEXL     IF   E           K DISK
     F*** END ADD *****  08/07/07  ************************************
     F*
     F*** BEGIN ADD ***  12/30/09  ************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
      *
     FWMLDL1    IF   E           K DISK
     F*** END ADD *****  12/30/09  ************************************
     F*
      ***** BEG DEL ******* 10/01/02 **********************************

     D*********           CX          5  7 0
      ***** END DEL ******* 10/01/02 **********************************
      ***** BEG ADD ******* 10/01/02 **********************************
      *
      * Effective and Cancel Date Arrays
      *
      *
      ***** END ADD ******* 10/01/02 **********************************
     D*** BEGIN CHG ***  01/24/06  ************************************
     D*************       DS      1   3 30               TRANSACTION TYPE
     D DS              S             30    DIM(4) CTDATA PERRCD(1)              TRANSACTION TYPE
     D*** END CHG *****  01/24/06  ************************************
     D*** BEGIN CHG ***  03/03/98  ************************************
     D*****               CON     1  16 22
     D*****               HDG        17 22
     D*****               TAM        17 11 2
     D*****               TIN        17 11 2
     D*** BEGIN CHG ***  06/01/99  ************************************
     D*******             CON     1  19 22
     D*******             HDG        18 22
     D*******             TAM        18 11 2
     D*******             TIN        18 11 2
     D*** BEGIN CHG ***  10/01/01  ************************************
     D**********          CON     1  20 22
     D**********          HDG        20 22
     D**********          TAM        20 11 2
     D**********          TIN        20 11 2
     D*** BEGIN CHG ***  01/17/03  ************************************
     D*********           CON     1  21 22
     D*********           HDG        21 22
     D*********           TAM        21 11 2
     D*********           TIN        21 11 2
     D*** BEG CHG *****  01/27/03  ************************************
     D*********           CON     1  22 22
     D*********           HDG        22 22
     D*********           TAM        22 11 2
     D*********           TIN        22 11 2
     D*** BEG CHG *****  09/01/04  ************************************
     D*********           CON     1  23 22
     D*********           HDG        23 22
     D*********           TAM        23 11 2
     D*********           TIN        23 11 2
     D***  Begin Chg  ***  01/08/04  **************************************
     D*********           CON     1  30 22
     D*********           HDG        30 22
     D*********           TAM        30 11 2
     D*********           TIN        30 11 2
     D*** begin chg ***  11/10/10  ***************************************
     D*********           CON     1  37 22
     D*** Beg chg *****  12/17/10  ***************************************
     D*********           CON     1  38 22
     D*** BEG CHG *****  01/01/11  ***************************************
     D***********         CON     1  40 22
     D*** End chg *****  12/17/10  ***************************************
     D*** end chg *****  11/10/10  ***************************************
     D*** Beg chg *****  12/17/10  ***************************************
     D***********         HDG        37 22
     D***********         TAM        37 11 2
     D*** Beg chg *****  12/31/10  ***************************************
     D***********         HDG        39 22
     D***********         TAM        39 11 2
     D***********         HDG        38 22
     D***********         TAM        38 11 2
     D*** End chg *****  12/31/10  ***************************************
     D*** End chg *****  12/17/10  ***************************************
     D***********         TIN        37 11 2
     D CON             S             22    DIM(50) CTDATA PERRCD(1)
     D*** END CHG *****  01/01/11  ***************************************
     D***  End   Chg  ***  01/08/04  **************************************
     D*** END CHG *****  09/01/04  ************************************
     D*** END CHG *****  01/27/03  ************************************
     D*** END CHG *****  01/17/03  ************************************
     D*** END CHG *****  10/01/01  ************************************
     D*** END CHG *****  06/01/99  ************************************
     D*** END CHG *****  03/03/98  ************************************
     D TMP             S              1    DIM(22)
     D NUM             S              1    DIM(5)
     D*** BEG ADD ***** 12/30/09 *************************************
     D NUM1            S              1    DIM(12)
     D*** END ADD ***** 12/30/09 *************************************
     D*** BEGIN ADD ****** 01/14/10 ************************************
     D NUM8            S              1    DIM(8)
     D*** END   ADD ****** 01/14/10 ************************************
      *
      *
      *
      ***** BEG ADD ******* 10/01/02 **********************************
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
      ***** END ADD ******* 10/01/02 **********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSMNTH                 7      8  0
     D                 DS
     D  EHTRDT                 1      7  0
     D  XXMNTH                 4      5  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D*** BEGIN CHG ***  03/03/98  ************************************
     D*****                                   1 374 HDG
     D*** BEGIN CHG ***  06/01/99  ************************************
     D*****                                   1 396 HDG
     D*** BEGIN CHG ***  10/01/01  ************************************
     D**********                              1 440 HDG
     D*** BEGIN CHG ***  01/17/03  ************************************
     D*********                               1 462 HDG
     D*** BEG CHG *****  01/27/03  ************************************
     D***************************             1 484 HDG
     D*** BEG CHG *****  09/01/04  ************************************
     D***************************             1 506 HDG
     D***  BEGIN CHG  ***  04/07/05 *************************************
     D***************************             1 660 HDG
     D*** Beg Chg *****  12/17/10  ***************************************
     D***********                             1 814 HDG
     D*** Beg Chg *****  12/31/10  ***************************************
     D***********                             1 858 HDG
     D*** BEG CHG ***** 01/01/11 *****************************************
     D***********                             1 836 HDG
     D  HDG                    1   1100
     D                                     DIM(50)
     D*** END CHG ***** 01/01/11 *****************************************
     D*** End Chg *****  12/31/10  ***************************************
     D*** End Chg *****  12/17/10  ***************************************
     D***  END   CHG  ***  04/07/05 *************************************
     D*** BEG CHG *****  09/01/04  ************************************
     D*** END CHG *****  01/27/03  ************************************
     D*** END CHG *****  01/17/03  ************************************
     D*** END CHG *****  10/01/01  ************************************
     D*** END CHG *****  06/01/99  ************************************
     D*** END CHG *****  03/03/98  ************************************
     D  SCHD01                 1     22
     D  SCHD02                23     44
     D  SCHD03                45     66
     D  SCHD04                67     88
     D  SCHD05                89    110
     D  SCHD06               111    132
     D  SCHD07               133    154
     D  SCHD08               155    176
     D  SCHD09               177    198
     D  SCHD10               199    220
     D  SCHD11               221    242
     D  SCHD12               243    264
     D  SCHD13               265    286
     D  SCHD14               287    308
     D  SCHD15               309    330
     D  SCHD16               331    352
     D  SCHD17               353    374
     D*** BEGIN ADD ***  03/03/98  ************************************
     D  SCHD18               375    396
     D*** END ADD *****  03/03/98  ************************************
     D*** BEGIN ADD ***  06/01/99  ************************************
     D  SCHD19               397    418
     D  SCHD20               419    440
     D*** END ADD *****  06/01/99  ************************************
     D*** BEGIN ADD ***  10/01/01  ************************************
     D  SCHD21               441    462
     D*** END ADD *****  10/01/01  ************************************
     D*** BEGIN ADD ***  01/17/03  ************************************
     D  SCHD22               463    484
     D*** END ADD *****  01/17/03  ************************************
     D*** BEG ADD *****  01/27/03  ************************************
     D  SCHD23               485    506
     D*** END ADD *****  01/27/03  ************************************
     D*** BEG ADD *****  09/01/04  ************************************
     D  SCHD24               507    528
     D  SCHD25               529    550
     D  SCHD26               551    572
     D  SCHD27               573    594
     D  SCHD28               595    616
     D  SCHD29               617    638
     D  SCHD30               639    660
     D*** END ADD *****  09/01/04  ************************************
     D***  BEGIN ADD  ***  04/07/05 *************************************
     D  SCHD31               661    682
     D  SCHD32               683    704
     D  SCHD33               705    726
     D  SCHD34               727    748
     D  SCHD35               749    770
     D  SCHD36               771    792
     D  SCHD37               793    814
     D***  END   ADD  ***  04/07/05 *************************************
     D*** Beg Add *****  12/17/10  ***************************************
     D  SCHD38               815    836
     D*** Beg Del *****  12/17/10  ***************************************
     D*************                         837 858 SCHD39
     D*** End Del *****  12/17/10  ***************************************
     D*** End Add *****  12/17/10  ***************************************
     D*** BEG ADD ***** 01/01/11 *****************************************
     D  SCHD39               837    858
     D  SCHD40               859    880
     D  SCHD41               881    902
     D  SCHD42               903    924
     D  SCHD43               925    946
     D  SCHD44               947    968
     D  SCHD45               969    990
     D  SCHD46               991   1012
     D  SCHD47              1013   1034
     D  SCHD48              1035   1056
     D  SCHD49              1057   1078
     D  SCHD50              1079   1100
     D*** END ADD ***** 01/01/11 *****************************************
     D*
     D                 DS
     D*** BEGIN CHG ***  03/03/98  ************************************
     D*****                                   1 187 TAM
     D*** BEGIN CHG ***  06/01/99  ************************************
     D*******                                 1 198 TAM
     D*** BEGIN CHG ***  10/01/01  ************************************
     D**********                              1 220 TAM
     D*** BEGIN CHG ***  01/17/03  ************************************
     D*********                               1 231 TAM
     D*** BEG CHG *****  01/27/03  ************************************
     D****************                        1 242 TAM
     D*** BEG CHG *****  09/01/04  ************************************
     D****************                        1 253 TAM
     D***  BEGIN CHG  ***  04/07/05 *************************************
     D****************                        1 330 TAM
     D*** Beg chg *****  12/17/10  ***************************************
     D**********                              1 407 TAM
     D*** Beg chg *****  12/31/10  ***************************************
     D**********                              1 429 TAM
     D*** BEG CHG ***** 01/01/11 *****************************************
     D***********                             1 418 TAM
     D  TAM                    1    550  2
     D                                     DIM(50)
     D*** END CHG ***** 01/01/11 *****************************************
     D*** End chg *****  12/31/10  ***************************************
     D*** End chg *****  12/17/10  ***************************************
     D***  END   CHG  ***  04/07/05 *************************************
     D*** END CHG *****  09/01/04  ************************************
     D*** END CHG *****  01/27/03  ************************************
     D*** END CHG *****  01/17/03  ************************************
     D*** END CHG *****  10/01/01  ************************************
     D*** END CHG *****  06/01/99  ************************************
     D*** END CHG *****  03/03/98  ************************************
     D  SCTA01                 1     11  2
     D  SCTA02                12     22  2
     D  SCTA03                23     33  2
     D  SCTA04                34     44  2
     D  SCTA05                45     55  2
     D  SCTA06                56     66  2
     D  SCTA07                67     77  2
     D  SCTA08                78     88  2
     D  SCTA09                89     99  2
     D  SCTA10               100    110  2
     D  SCTA11               111    121  2
     D  SCTA12               122    132  2
     D  SCTA13               133    143  2
     D  SCTA14               144    154  2
     D  SCTA15               155    165  2
     D  SCTA16               166    176  2
     D  SCTA17               177    187  2
     D*** BEGIN ADD ***  03/03/98  ************************************
     D  SCTA18               188    198  2
     D*** END ADD *****  03/03/98  ************************************
     D*** BEGIN ADD ***  06/01/99  ************************************
     D  SCTA19               199    209  2
     D  SCTA20               210    220  2
     D*** END ADD *****  06/01/99  ************************************
     D*** BEGIN ADD ***  10/01/01  ************************************
     D  SCTA21               221    231  2
     D*** END ADD *****  10/01/01  ************************************
     D*** BEGIN ADD ***  01/17/03  ************************************
     D  SCTA22               232    242  2
     D*** END ADD *****  01/17/03  ************************************
     D*** BEG ADD *****  01/27/03  ************************************
     D  SCTA23               243    253  2
     D*** END ADD *****  01/27/03  ************************************
     D*** BEG ADD *****  09/01/04  ************************************
     D  SCTA24               254    264  2
     D  SCTA25               265    275  2
     D  SCTA26               276    286  2
     D  SCTA27               287    297  2
     D  SCTA28               298    308  2
     D  SCTA29               309    319  2
     D  SCTA30               320    330  2
     D*** END ADD *****  09/01/04  ************************************
     D***  BEGIN ADD  ***  04/07/05 *************************************
     D  SCTA31               331    341  2
     D  SCTA32               342    352  2
     D  SCTA33               353    363  2
     D  SCTA34               364    374  2
     D  SCTA35               375    385  2
     D  SCTA36               386    396  2
     D  SCTA37               397    407  2
     D***  END   ADD  ***  04/07/05 *************************************
     D*** Beg Add *****  12/17/10  ***************************************
     D  SCTA38               408    418  2
     D*** Beg Del *****  12/31/10  ***************************************
     D************                          419 4292SCTA39
     D*** End Del *****  12/31/10  ***************************************
     D*** End Add *****  12/17/10  ***************************************
     D*** BEG ADD ***** 01/01/11 *****************************************
     D  SCTA39               419    429  2
     D  SCTA40               430    440  2
     D  SCTA41               441    451  2
     D  SCTA42               452    462  2
     D  SCTA43               463    473  2
     D  SCTA44               474    484  2
     D  SCTA45               485    495  2
     D  SCTA46               496    506  2
     D  SCTA47               507    517  2
     D  SCTA48               518    528  2
     D  SCTA49               529    539  2
     D  SCTA50               540    550  2
     D*** END ADD ***** 01/01/11 *****************************************
     D*
     D                 DS
     D*** BEGIN ADD ***  03/03/98  ************************************
     D*****                                   1 187 TIN
     D*** BEGIN CHG ***  06/01/99  ************************************
     D********                                1 198 TIN
     D*** BEGIN CHG ***  10/01/01  ************************************
     D**********                              1 220 TIN
     D*** BEGIN CHG ***  01/17/03  ************************************
     D*********                               1 231 TIN
     D*** BEG CHG *****  01/27/03  ************************************
     D*********                               1 242 TIN
     D*** BEG CHG *****  09/01/04  ************************************
     D****************                        1 253 TIN
     D*** END CHG *****  09/01/04  ************************************
     D***  BEGIN CHG  ***  04/07/05 *************************************
     D*****************                       1 330 TIN
     D*** BEG CHG ***** 01/01/11 ****************************************
     D***********                             1 407 TIN
     D  TIN                    1    550  2
     D                                     DIM(50)
     D*** END CHG ***** 01/01/11 ****************************************
     D***  END   CHG  ***  04/07/05 *************************************
     D*** END CHG *****  01/27/03  ************************************
     D*** END CHG *****  01/17/03  ************************************
     D*** END CHG *****  10/01/01  ************************************
     D*** END CHG *****  06/01/99  ************************************
     D*** END ADD *****  03/03/98  ************************************
     D  SCTI01                 1     11  2
     D  SCTI02                12     22  2
     D  SCTI03                23     33  2
     D  SCTI04                34     44  2
     D  SCTI05                45     55  2
     D  SCTI06                56     66  2
     D  SCTI07                67     77  2
     D  SCTI08                78     88  2
     D  SCTI09                89     99  2
     D  SCTI10               100    110  2
     D  SCTI11               111    121  2
     D  SCTI12               122    132  2
     D  SCTI13               133    143  2
     D  SCTI14               144    154  2
     D  SCTI15               155    165  2
     D  SCTI16               166    176  2
     D  SCTI17               177    187  2
     D*** BEGIN ADD ***  03/03/98  ************************************
     D  SCTI18               188    198  2
     D*** END ADD *****  03/03/98  ************************************
     D*** BEGIN ADD ***  06/01/99  ************************************
     D  SCTI19               199    209  2
     D  SCTI20               210    220  2
     D*** END ADD *****  06/01/99  ************************************
     D*** BEGIN ADD ***  10/01/01  ************************************
     D  SCTI21               221    231  2
     D*** END ADD *****  10/01/01  ************************************
     D*** BEGIN ADD ***  01/17/03  ************************************
     D  SCTI22               232    242  2
     D*** END ADD *****  01/17/03  ************************************
     D*** BEG ADD *****  01/27/03  ************************************
     D  SCTI23               243    253  2
     D*** END ADD *****  01/27/03  ************************************
     D*** BEG ADD *****  09/01/04  ************************************
     D  SCTI24               254    264  2
     D  SCTI25               265    275  2
     D  SCTI26               276    286  2
     D  SCTI27               287    297  2
     D  SCTI28               298    308  2
     D  SCTI29               309    319  2
     D  SCTI30               320    330  2
     D*** END ADD *****  09/01/04  ************************************
     D***  BEGIN ADD  ***  04/07/05 *************************************
     D  SCTI31               331    341  2
     D  SCTI32               342    352  2
     D  SCTI33               353    363  2
     D  SCTI34               364    374  2
     D  SCTI35               375    385  2
     D  SCTI36               386    396  2
     D  SCTI37               397    407  2
     D***  END   ADD  ***  04/07/05 *************************************
     D*** BEG ADD ***** 01/01/11 *****************************************
     D  SCTI38               408    418  2
     D  SCTI39               419    429  2
     D  SCTI40               430    440  2
     D  SCTI41               441    451  2
     D  SCTI42               452    462  2
     D  SCTI43               463    473  2
     D  SCTI44               474    484  2
     D  SCTI45               485    495  2
     D  SCTI46               496    506  2
     D  SCTI47               507    517  2
     D  SCTI48               518    528  2
     D  SCTI49               529    539  2
     D  SCTI50               540    550  2
     D*** END ADD ***** 01/01/11 *****************************************
     D*** BEGIN ADD ***  04/01/98  ************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*** END   ADD ***  04/01/98  ************************************
     D*
     D AGTAPI          C                   CONST('WA002R')
     D OPTNM           C                   CONST('Work with Premium Hi-
     D                                     story')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D***  BEG   Add  ***  06/21/10  **************************************
     D @2NPER          C                   CONST('2nd prd')

     I***  END   Add  ***  06/21/10  **************************************

     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  GROUP MASTER/AUXILIARY FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  GROUP DETAIL FILE
     C*
     C*** BEGIN DEL ***  10/25/99  ************************************
     C*******    F1KY      KLIST
     C*******              KFLD           XXCO#
     C*******              KFLD           XXFND
     C*******              KFLD           XXFYR
     C*** END DEL *****  10/25/99  ************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  STAT RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      ***** BEG CHG ******* 06/25/02 **********************************
     C*********            KFLD           XXEFDT
     C                   KFLD                    X1EFDT
      ***** END CHG ******* 06/25/02 **********************************
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXKEY
     C                   KFLD                    XXPFDT            7 0
     C*
     C     LMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXKEY
     C*
     C*  PREMIUM ENDORSEMENT HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   KFLD                    XXDT
     C                   KFLD                    XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      ***** BEG ADD ******* 06/25/02 **********************************
      *
     C     EHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
      ***** END ADD ******* 06/25/02 **********************************
      *
      * KEY LISTS FOR THE EXPENSE CONSTANT DEFINITION FILE
      *
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
      ***** BEG ADD ******* 06/25/02 **********************************
      *
     C*** BEG ADD ***** 12/30/09 *************************************
      *  TABLE FILE (WTBSP)
      *
     C     TBKEY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY1
      *
      * KEY LIST OF THE LARGE DEDUCTIBLE FILE (WMLDP)
     C     LDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C*** END ADD ***** 12/30/09 *************************************
      *
      *
      *================================================================
      * *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    EHDT          XXDT
     C     *LIKE         DEFINE    EHTIME        XXTIME
     C     *LIKE         DEFINE    EHDT          SVDT1
     C     *LIKE         DEFINE    EHTIME        SVTIM1
     C     *LIKE         DEFINE    EHDT          SVDT
     C     *LIKE         DEFINE    EHTIME        SVTIME
     C     *LIKE         DEFINE    EHEFDT        X1EFDT
      ***** END ADD ******* 06/25/02 **********************************
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEFDT        XXEFDT
     C     *LIKE         DEFINE    EHEFDT        SVEFDT
     C     *LIKE         DEFINE    EMNAME        XXNAME
      ****** BEGIN ADD ***** 04/01/98 *********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      ****** END   ADD ***** 04/01/98 *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     AGTPL         PLIST
      ***** BEG CHG ******* 10/01/02 **********************************
     C*********            PARM           ##CO#   30
     C                   PARM      ELCO#         ##CO#             3 0
      ***** END CHG ******* 10/01/02 **********************************
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     @@MODE        COMP      'D'                                    09
     C*
     C*** BEG ADD ***** 12/18/05 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD ***** 12/18/05 **************************************
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   MOVE      '0'           *IN25
     C     CLTID         IFEQ      '025'
     C                   MOVE      '1'           *IN25
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     OPTNM         SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C     @@RRN         CHAIN     WDELP                              60
     C*
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C                   Z-ADD     ELFYR         SCYR
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEGIN ADD ***  03/03/98  ************************************
     C                   Z-ADD     0             SCAGT#            5 0
     C                   Z-ADD     0             SCPROD            5 0
     C                   MOVE      *BLANKS       SCANAM           20
     C                   MOVE      *BLANKS       SCPNAM           20
     C*** END ADD *****  03/03/98  ************************************
     C*
     C*  GET AGENT RECORDS
     C*
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL06        CABNE     'Y'                                1313
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     FNKY          CHAIN     PF201                              60
     C*** BEGIN DEL ***  10/25/99  ************************************
     C********   F1KY      CHAINPF101                60
     C*** END DEL *****  10/25/99  ************************************
     C*** BEGIN ADD ***  08/07/07  ************************************
     C     FNKY          CHAIN     PEX01                              60
     C*** END ADD *****  08/07/07  ************************************
     C*
     C                   MOVEL(P)  FNNAME        SCNAME
     C*
     C*  GET THE EMPLOYER NAME
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
      ***** BEG DEL ******* 10/01/02 **********************************
      *********
     C*********            DO   5         I1      30
     C*********
     C*********  CX,I1     IFNE *ZERO
     C*********            Z-ADDCX,I1     SRDTI
     C*********            CALL 'SRCVTDAT'DTMDYY
     C*********            MOVELSRDTO     SCCXDT
     C*********            ELSE
     C*********            LEAVE
     C*********            ENDIF                           CX,I1<>*ZERO
     C*********
     C*********            ENDDO                           I1->5
      ***** END DEL ******* 10/01/02 **********************************
      ***** BEG ADD ******* 10/01/02 **********************************
      *
      *  Get Employer Effective/Cancellation date
      *
     C                   Z-ADD     *ZERO         SCCXDT
     C                   Z-ADD     5             X2                3 0
      *
     C     ED(X2)        DOWEQ     *ZERO
     C     X2            ANDGT     1
     C                   SUB       1             X2
     C                   ENDDO
      *
     C     CD(X2)        IFNE      *ZERO
     C                   Z-ADD     CD(X2)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ENDIF
      *
      ***** END ADD ******* 10/01/02 **********************************
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C                   MOVE      'N'           LDPFLG            1
     C     WCFL43        IFEQ      'Y'
     C                   MOVEL     ELBSCD        XXKEY1           10
     C     TBKEY         CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFEQ      'L'
      *
     C     LDKY          CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      'Y'           LDPFLG
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C*  INITIALIZE SEQUENCE #
     C*
     C                   CLEAR                   XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   CLEAR                   XXDT
     C                   CLEAR                   XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C     TOP           TAG
     C*
     C*  INITIALIZE SCREEN
     C*
     C                   Z-ADD     *ZERO         RRN1              5 0
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C                   MOVE      XXEFDT        SVEFD1            7 0
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     XXDT          SVDT1
     C                   Z-ADD     XXTIME        SVTIM1
      ***** END ADD ******* 06/25/02 **********************************
     C                   MOVE      XXEFDT        SVEFDT
      ***** BEG DEL ******* 06/25/02 **********************************
     C*********            SUB  1         SVEFDT
      ***** END DEL ******* 06/25/02 **********************************
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     XXDT          SVDT
     C                   Z-ADD     XXTIME        SVTIME
     C                   SUB       1             SVTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C*  CLEAR SUBFILE
     C*
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN01
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      *ON           *IN01
     C                   MOVE      *ON           *IN06
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVE      *OFF          *IN01
     C                   MOVE      *OFF          *IN06
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   ENDIF                                                  Q2RECF=SCR03
     C*_______________________________________________________________
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*  F3 = EXIT
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*  F5 = REFRESH
     C*
     C     *INKE         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   CLEAR                   XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   CLEAR                   XXDT
     C                   CLEAR                   XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C                   GOTO      TOP
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   ENDIF                                                  *INKE=*ON
     C*
     C*  F12 = CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   GOTO      TERM
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
      ********* BEGIN ADD ******* 04/01/98 ****************************
     C*
     C*  F14 = DARTS
     C*
     C     *INKN         IFEQ      *ON
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      EMEMP#        QCEMP#
     C                   MOVE      EMDIV         QCDIV
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C                   ENDIF                                                  *INKN=*ON
      ********* END   ADD ******* 04/01/98 ****************************
     C*
     C*  *IN17 -  PAGE UP REQUEST
     C*
     C     *IN17         IFEQ      *ON                                          PAGE UP
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR01S                                 60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     SCSEL         IFNE      *BLANK
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN1          HHREC#
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSEL<>*LANK
     C*
     C                   READC     SCR01S                                 60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   Z-ADD     SVEFD1        XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     SVDT1         XXDT
     C                   Z-ADD     SVTIM1        XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C     EHKY          SETLL     PEH01
     C     EHKY1         READPE    PEH01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN31
     C                   Z-ADD     1             HHREC#
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVE      *OFF          *IN31
     C                   Z-ADD     EHEFDT        XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     EHDT          XXDT
     C                   Z-ADD     EHTIME        XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C                   DO        10            X
     C*
     C     EHKY1         READPE    PEH01                                  60
     C*
     C   60              LEAVE
     C                   Z-ADD     EHEFDT        XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     EHDT          XXDT
     C                   Z-ADD     EHTIME        XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C                   ENDDO                                                  X->X
     C*
     C                   GOTO      TOP
     C*
     C                   ENDIF                                                  *IN17=*ON(PAGE UP)
     C*
     C*  *IN18 -  PAGE DOWN REQUEST
     C*
     C     *IN18         IFEQ      *ON                                          PAGE DOWN
     C*
     C     *IN32         IFEQ      *ON
     C                   Z-ADD     XXLAST        HHREC#
     C                   GOTO      STRT                                         NO ROLL IF EOF
     C                   ENDIF                                                  *IN32=*ON
     C*
     C                   Z-ADD     XXRRN1        RRN1
     C*
     C                   EXSR      LOAD
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN18=*ON(PAGE DOWN)
     C*
     C*** BEGIN ADD ***  06/01/99  ************************************
     C*
     C* PAGE UP ON SCR03.
     C*
     C     *IN15         IFEQ      *ON
     C*
     C     *IN87         IFEQ      *OFF
     C*
     C* ALREADY ON FIRST PAGE
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C* SCROLL TO FIRST PAGE
     C                   MOVE      *OFF          *IN87
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN15 IFEQ
     C*
     C* PAGE DOWN ON SCR03.
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     *IN87         IFEQ      *ON
     C*
     C* ALREADY ON SECOND PAGE
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C* SCROLL TO SECOND PAGE
     C                   MOVE      *ON           *IN87
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN16 IFEQ
     C*** END ADD *****  06/01/99  ************************************
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   GOTO      SCR01T
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   GOTO      SCR02T
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*_______________________________________________________________
     C*
     C*  PROCESS SCREEN 1
     C*
     C     SCR01T        TAG
     C*
     C*  CLEAR DELETION SUBFILE
     C*
     C                   Z-ADD     *ZERO         RRN2              5 0
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      *OFF          *IN04
     C                   MOVE      *OFF          *IN48
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   MOVE      *OFF          *IN49
     C*
     C     CKSEL         TAG
     C*
     C                   READC     SCR01S                                 60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *ON           *IN49
     C*
     C*  OPTION BLANK - BYPASS
     C*
     C     SCSEL         IFEQ      *BLANK
     C                   UPDATE    SCR01S
     C                   GOTO      CKSEL
     C                   ENDIF                                                  SCSEL=*BLANK
     C*
     C     *IN21         IFEQ      *OFF
     C                   Z-ADD     RRN1          HHREC#
     C                   ENDIF                                                  *IN21=*OFF
     C*
     C                   SELECT
      ***** BEG ADD ******* 01/18/01 **********************************
     C**********
     C***OPTION*4 - DELETE RECORD
     C**********
     C********** SCSEL     WHEQ '4 '
     C********** *IN09     ANDEQ*OFF
     C********** SCSEL     OREQ ' 4'
     C********** *IN09     ANDEQ*OFF
     C**********
     C**********           Z-ADDSCEFDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXEFDT
     C**********
     C********** EHKY      SETLLPEH01
     C**********
     C********** SCTIME    DOUEQEHTIME
     C********** *IN60     OREQ *ON
     C********** EHKY      READEPEH01               N    60
     C**********           ENDDO                           SCTIME=EHTIME
     C**********
     C***IF*THE*RECORD IS NOT FOUND, SEND A MESSAGE
     C**********
     C********** *IN60     IFEQ *ON
     C**********           MOVE *ON       *IN19
     C**********           MOVE *ON       *IN20
     C**********           MOVE *ON       *IN23
     C**********           UPDATSCR01S
     C**********           MOVE *OFF      *IN19
     C**********           MOVE *OFF      *IN20
     C**********           GOTO STRT
     C**********           ENDIF                           *IN60=*ON
     C**********
     C***THIS*MUST BE THE LAST RECORD
     C**********
     C********** EHKY1     READEPEH01               N    60
     C**********
     C********** *IN60     IFEQ *OFF
     C**********           MOVE *ON       *IN19
     C**********           MOVE *ON       *IN20
     C**********           MOVE *ON       *IN24
     C**********           UPDATSCR01S
     C**********           MOVE *OFF      *IN19
     C**********           MOVE *OFF      *IN20
     C**********           GOTO STRT
     C**********           ENDIF                           *IN60=*OFF
     C**********
     C***THE*RECORD MUST HAVE BEEN WRITTEN IN THE CURRENT MONTH
     C**********
     C********** XXMNTH    IFNE DSMNTH
     C**********           MOVE *ON       *IN19
     C**********           MOVE *ON       *IN20
     C**********           MOVE *ON       *IN25
     C**********           UPDATSCR01S
     C**********           MOVE *OFF      *IN19
     C**********           MOVE *OFF      *IN20
     C**********           GOTO STRT
     C**********           ENDIF                           XXMNTH<>DSMNTH
     C**********
     C**********           MOVE *ON       *IN48            REQUEST SCR02
     C**********           ADD  1         RRN2
     C**********           MOVE *ON       *IN19
     C**********
     C**********           WRITESCR02S
     C**********           UPDATSCR01S
     C**********
     C**********           MOVE *OFF      *IN19
     C**********           GOTO CKSEL
      ***** END ADD ******* 01/18/01 **********************************
     C*
     C**
     C*
     C*  OPTION 5 - DISPLAY THE PREMIUM HISTORY RECORD
     C*
     C     SCSEL         WHENEQ    '5 '
     C     SCSEL         OREQ      ' 5'
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
      *
     C                   Z-ADD     SCDT          XXDT
     C                   Z-ADD     SCTIME        XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C     EHKY          SETLL     PEH01
     C*
     C     SCTIME        DOUEQ     EHTIME
     C     *IN60         OREQ      *ON
      ***** BEG CHG ******* 06/25/02 **********************************
     C*********  EHKY      READEPEH01               N    60
     C     EHKY2         READE(N)  PEH01                                  60
      ***** END CHG ******* 06/25/02 **********************************
     C                   ENDDO                                                  SCTIME=EHTIME
     C*
     C*  IF THE RECORD IS NOT FOUND, SEND A MESSAGE
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN23
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C*
     C                   EXSR      LOAD03
     C*
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C**
     C                   OTHER
     C*
     C*  ANY OTHER OPTIONS ARE INVALID
     C*
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN21
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C*  IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO CALLER
     C*
     C                   ELSE
     C*
     C     *IN49         IFEQ      *OFF
     C*
     C                   GOTO      TERM
     C*
     C                   ELSE
     C*
     C*  IF RECORD DELETION WAS REQUESTED, DISPLAY SCREEN 2
     C*
     C     *IN48         IFEQ      *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   ENDIF                                                  *IN48=*ON
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN49=*OFF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*_______________________________________________________________
     C*
     C*  PROCESS SCREEN 2
     C*
     C     SCR02T        TAG
     C*
     C*  READ THROUGH THE SUBFILE AND DELETE THE RECORDS
     C*
     C                   READC     SCR02S                                 60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
      *
     C                   Z-ADD     SCDT          XXDT
     C                   Z-ADD     SCTIME        XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C     EHKY          SETLL     PEH01
     C*
     C     SCTIME        DOUEQ     EHTIME
     C     *IN60         OREQ      *ON
      ***** BEG CHG ******* 06/25/02 **********************************
     C*********  EHKY      READEPEH01                    60
     C     EHKY2         READE     PEH01                                  60
      ***** END CHG ******* 06/25/02 **********************************
     C                   ENDDO                                                  SCTIME=EHTIME
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PEH01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   READC     SCR02S                                 60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  FIND THE TOP RECORD OF THE LAST SCREEN USED.  SET
     C*  THE TOP BASED ON THAT RECORD.
     C*
     C     HHREC#        DIV       11            XXTOP             3 0
     C                   MVR                     XXTOP
     C*
     C     XXTOP         IFEQ      *ZERO
     C                   Z-ADD     10            XXTOP
     C                   ELSE
     C                   SUB       1             XXTOP
     C                   ENDIF                                                  XXTOP=*ZERO
     C*
     C     HHREC#        SUB       XXTOP         XXTOP
     C*
     C     XXTOP         CHAIN     SCR01S                             60
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
      *
     C                   Z-ADD     SCDT          XXDT
     C                   Z-ADD     SCTIME        XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C                   GOTO      TOP
     C*_______________________________________________________________
     C*
     C*  PROCESS SCREEN 3
     C*
     C     SCR03T        TAG
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C                   GOTO      CKSEL
     C*
     C*  END OF THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*===============================================================
     C* * *   LOAD SUBROUTINE                               L O A D  *
     C*===============================================================
     C*
     C     LOAD          BEGSR
     C*
     C                   MOVE      *OFF          *IN05
     C                   MOVE      *OFF          *IN32
     C                   MOVE      *BLANK        SCSEL
     C*
     C                   Z-ADD     SVEFDT        XXEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     SVDT          XXDT
     C                   Z-ADD     SVTIME        XXTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C     EHKY          SETGT     PEH01
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UP TO 11 LINES
     C*
     C                   DO        11            X                 6 0
     C*
     C     EHKY1         READE(N)  PEH01                                  05
     C*
     C     *IN05         IFEQ      *ON
     C     X             ANDEQ     1
     C                   MOVE      *ON           *IN32
     C                   ENDIF                                                  *IN05=*ON
     C*
     C     *IN05         IFEQ      *OFF
     C*
     C                   ADD       1             RRN1
     C*
     C     X             IFEQ      1
     C     *IN21         ANDEQ     *OFF
     C                   Z-ADD     RRN1          HHREC#
     C                   Z-ADD     RRN1          XXLAST            5 0
     C                   ENDIF                                                  X=1/*IN21=*OFF
     C*
     C                   Z-ADD     EHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT            8 0
      *
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     EHDT          SCDT
     C                   Z-ADD     EHTIME        SCTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C                   MOVE      EHEFDT        SVEFDT
      ***** BEG ADD ******* 06/25/02 **********************************
     C                   Z-ADD     EHDT          SVDT
     C                   Z-ADD     EHTIME        SVTIME
      ***** END ADD ******* 06/25/02 **********************************
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C                   SELECT
     C     EHFAF         WHENEQ    'E'
     C                   MOVE      DS(1)         SCDESC
     C     EHFAF         WHENEQ    'R'
     C                   MOVE      DS(2)         SCDESC
     C     EHFAF         WHENEQ    'A'
     C                   MOVE      DS(3)         SCDESC
     C*** BEGIN ADD ***  01/24/06  ************************************
     C     EHFAF         WHENEQ    'X'
     C                   MOVE      DS(4)         SCDESC
     C*** END ADD *****  01/24/06  ************************************
     C                   ENDSL
     C*
     C                   MOVE      EHTIME        SCTIME
     C*
     C                   MOVE      *OFF          *IN06
     C                   WRITE     SCR01S
     C*
     C                   ENDIF                                                  *IN05=*OFF
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  READ AHEAD TO SEE IF ANOTHER RECORD EXISTS
     C*
     C     EHKY1         READE(N)  PEH01                                  05
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN1          IFEQ      *ZERO
     C     *IN05         ANDEQ     *ON
     C                   CLEAR                   SCR01S
     C                   Z-ADD     1             RRN1
     C                   Z-ADD     1             XXLAST
     C                   Z-ADD     1             HHREC#
     C                   MOVE      *ON           *IN06
     C                   WRITE     SCR01S
     C                   ENDIF                                                  RRN1=*ZERO/*IN05=*ON
     C*
     C*  SAVE LAST WRITTEN RECORD NUMBER
     C*
     C                   Z-ADD     RRN1          XXRRN1            5 0
     C*
     C                   ENDSR                                                  LOAD
     C*
     C*===============================================================
     C*  LOAD03 - LOADS WRITTEN AND INFORCE PREMIUMS
     C*===============================================================
     C*
     C     LOAD03        BEGSR
     C*
     C                   MOVE      EHRTCD        XXRTCD
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
     C                   MOVE      RSJRST        XXST
      ***** BEG CHG ******* 06/25/02 **********************************
     C*********            MOVE EHPFDT    XXEFDT
     C                   MOVE      EHPFDT        X1EFDT
      ***** END CHG ******* 06/25/02 **********************************
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*** BEGIN DEL ***  10/13/99  ************************************
     C*****      ECKY      SETGTPEC01
     C*****      ECKY1     REDPEPEC01                    60
     C*
     C*****      *IN60     IFEQ *ON
     C*****                Z-ADD0         ECEXCO
     C*****                ENDIF
     C*** END DEL *****  10/13/99  ************************************
     C*
     C*  INITIALIZE SCREEN ARRAYS
     C*
     C                   CLEAR                   HDG
     C                   CLEAR                   TAM
     C                   CLEAR                   TIN
     C*
     C*  LOAD SCREEN ARRAYS
     C*
     C                   Z-ADD     1             A                 3 0          MANUAL PREM
     C*
     C*  MANUAL PREMIUM
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(1)        HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(30)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     EHWMPR        TAM(A)
     C                   Z-ADD     EHIMPR        TIN(A)
     C*
     C                   SELECT
     C     EHWMNP        WHENEQ    'Y'
     C                   SUB       EHWIN1        TAM(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C                   SUB       EHWWAV        TAM(A)
     C*** END ADD *****  09/20/02  ************************************
     C     EHIMNP        WHENEQ    'Y'
     C                   SUB       EHIIN1        TIN(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C                   SUB       EHIWAV        TIN(A)
     C*** END ADD *****  09/20/02  ************************************
     C                   ENDSL
     C*
     C                   Z-ADD     TAM(A)        XXSUBT           11 2
     C                   Z-ADD     TIN(A)        XXSUBN           11 2
     C*
     C                   ADD       1             A
     C*
     C*** BEGIN ADD ***  04/06/07  ************************************
     C*
     C*  LOAD SHORT RATE PENALTY IF APPLIED TO MANUAL PREMIUM
     C*
     C     RRSRAF        IFEQ      'M'
     C     EHWSRA        ANDNE     *ZERO
     C     RRSRAF        OREQ      ' '
     C     EHWSRA        ANDNE     *ZERO
     C*
     C                   Z-ADD     EHWSRA        TAM(A)
     C                   Z-ADD     0             TIN(A)
     C                   MOVEL     CON(36)       HDG(A)
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*** END ADD *****  04/06/07  ************************************
     C*
     C*** BEGIN ADD ***  01/13/03  ************************************
     C*
     C*  LOAD DEVIATION IF APPLIED TO MANUAL PREMIUM
     C*
     C     RRDV1L        IFEQ      'M'
     C*
     C                   Z-SUB     EHWDC2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDC2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     EHASP2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHASP2        IFLT      0
     C*
     C     EHASP2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHASP2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHASP2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C     RRDV1L        IFEQ      'M'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHWDC2    TAM,A
     C                   Z-SUB     EHD1W2        TAM(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TAM(A)        XXSUBT
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHIDC2    TIN,A
     C                   Z-SUB     EHD1I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVE      *BLANKS       XXWK25
     C*
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25           25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP1            5
     C                   ENDIF                                                  EHD1S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD1S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD1S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*  LOAD 2ND DEVIATION IF APPLIED TO MANUAL PREMIUM
     C*
     C     RRDV2L        IFEQ      'M'
     C*
     C                   Z-SUB     EHWDV2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDV2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHDEV2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHDEV2        IFLT      0
     C*
     C     EHDEV2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHDEV2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHDEV2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C*
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      EHSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     EHSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** END ADD *****  01/13/03  ************************************
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C     RRDV2L        IFEQ      'M'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHWDV2    TAM,A
     C                   Z-SUB     EHD2W2        TAM(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TAM(A)        XXSUBT
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHIDV2    TIN,A
     C                   Z-SUB     EHD2I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TIN(A)        XXSUBN
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C*
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP1            5
     C                   ENDIF                                                  EHD2S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD2S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD2S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      EHSTFC        XXASP8            8
     C                   ENDIF                                                  EHSTFC
     C*
     C                   MOVEA     XXASP8        NUM8
     C     EHSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*
     C*** BEGIN ADD ***  06/01/99  ************************************
     C*
     C* WAIVER OF SUBROGATION
     C*
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'M'
     C     ELWVFL        IFEQ      'M'
     C*** END CHG *****  09/29/99  ************************************
     C*
     C     RRWVHD        IFNE      *BLANKS
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ELSE
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(1)        HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(30)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   ENDIF
     C*
     C                   Z-ADD     EHWWAV        TAM(A)
     C                   Z-ADD     EHIWAV        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*** END ADD *****  06/01/99  ************************************
     C*
     C*  INCREASED LIMITS
     C*
     C*** BEGIN DEL ***  10/25/99  ************************************
     C*********            MOVE *BLANKS   XXEMLB 30
     C*********            MOVELF1EMLB    XXEMLB
     C*** END DEL *****  10/25/99  ************************************
     C*
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     EHILCD        XXKEY
     C                   MOVE      EHILEF        XXPFDT
     C*
     C     LMKY          SETGT     PLM01                              60
     C     LMKY2         READPE    PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C*
     C   60              Z-ADD     *ZERO         LMPCT
     C                   MOVEA     CON(2)        TMP
     C*
     C     LMPCT         IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE '.'       TMP,19
     C                   MOVE      '.'           TMP(18)
      **** END   CHG **** 08/20/01 ************************************
     C                   MOVE      LMPCT         XXLMPC            5
     C                   MOVEA     XXLMPC        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE NUM,3     TMP,18
     C                   MOVE      NUM(3)        TMP(19)
      **** END   CHG **** 08/20/01 ************************************
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C*
     C                   Z-ADD     EHWIN1        TAM(A)
     C                   ADD       TAM(A)        XXSUBT           11 2
     C                   Z-ADD     EHIIN1        TIN(A)
     C                   ADD       TIN(A)        XXSUBN           11 2
     C                   ADD       1             A
     C*** BEGIN ADD ***  03/03/98  ************************************
     C*
     C*  2ND INCREASED LIMITS
     C*
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     EHI2CD        XXKEY
     C                   MOVE      EHI2EF        XXPFDT
     C*
     C     LMKY          SETGT     PLM01                              60
     C     LMKY2         READPE    PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C*
     C   60              Z-ADD     *ZERO         LMPCT
     C                   MOVEA     CON(17)       TMP
     C*
     C     LMPCT         IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE '.'       TMP,19
     C                   MOVE      '.'           TMP(18)
      **** END   CHG **** 08/20/01 ************************************
     C                   MOVE      LMPCT         XXLMPC            5
     C                   MOVEA     XXLMPC        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
      **** BEGIN CHG **** 08/20/01 ************************************
     C******************** MOVE NUM,3     TMP,18
     C                   MOVE      NUM(3)        TMP(19)
      **** BEGIN CHG **** 08/20/01 ************************************
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C*
     C                   Z-ADD     EHWIN2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT           11 2
     C                   Z-ADD     EHIIN2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN           11 2
     C                   ADD       1             A
     C*** END ADD *****  03/03/98  ************************************
     C*** BEG ADD *****  09/01/04  ************************************
     C*
     C*  VOLUNTARY LABOR CHARGE
     C*
     C     EHWMNP        IFNE      'Y'
     C     EHIMNP        ANDNE     'Y'
     C     EHWVLC        ANDNE     *ZERO
     C*
     C     RRVLCH        IFNE      *BLANKS
     C                   MOVEL     RRVLCH        HDG(A)
     C                   ELSE
     C                   MOVEL     CON(25)       HDG(A)
     C                   ENDIF
     C*
     C                   Z-ADD     EHWVLC        TAM(A)
     C                   Z-ADD     EHIVLC        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  EHWMNP IFEQ
     C*** END ADD *****  09/01/04  ************************************
     C*
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
     C*
     C     RRRDPR        IFEQ      'M'
     C*** BEGIN ADD ***  04/14/10  ************************************
     C     LDPFLG        ANDNE     'Y'
     C*** END ADD *****  04/14/10  ************************************
     C     *IN13         ANDEQ     *OFF
     C                   Z-SUB     EHWDCT        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDCT        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(3)        TMP
     C     EHDDCT        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(31)       TMP
     C*
     C     EHDD2P        IFNE      *ZERO
     C*
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C*
     C                   ENDIF
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C***  END   ADD  ***  04/07/05 *************************************
     C*
     C                   ENDIF
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C*
     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
     C*
     C     LDPFLG        IFEQ      'Y'
     C*
     C     LDPLV1        IFEQ      'M'
     C     *IN13         ANDEQ     *OFF
     C*
     C     EHPCT1        IFNE      *ZEROS
     C                   Z-SUB     EHWDCT        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDCT        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(3)        TMP
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   END
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   END
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
     C*
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPLV1 = 'M'
     C*
     C     LDPLV2        IFEQ      'M'
     C     EHPCT2        ANDNE     *ZEROS
     C*
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(31)       TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C*
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF
     C*
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPFLG = 'Y'
     C*** END ADD ***** 12/30/09 *************************************
     C*** begin add *** 11/10/10 *************************************
     C*
     C*  HCN Credit
     C*
     C     EHHCN         IFNE      *ZERO
     C                   Z-SUB     EHWHCN        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIHCN        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     c*
     C                   MOVEA     CON(38)       TMP
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '.'           TMP(18)
     C                   CLEAR                   XXASP1
     C                   MOVE      EHHCN         XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(17)
     C                   MOVE      NUM(2)        TMP(19)
     C                   MOVE      NUM(3)        TMP(20)
     C                   MOVE      NUM(4)        TMP(21)
     C                   MOVE      NUM(5)        TMP(22)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  EHHCN IFNE
     C*** end add ***** 11/10/10 *************************************
     C*
     C*  MANAGED CARE CREDIT
     C*
     C     RR1AHD        IFNE      *BLANKS
     C                   Z-SUB     EHWDCM        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDCM        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     RR1AHD        TMP
     C     EHASPM        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(16)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHASPM        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C                   MOVE      X'22'         TMP(16)
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C*
     C*  SAFETY CREDIT
     C*
     C     RR1BHD        IFNE      *BLANKS
     C                   Z-SUB     EHWDCS        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDCS        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     RR1BHD        TMP
     C     EHASPS        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(16)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHASPS        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C                   MOVE      X'22'         TMP(16)
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C*
     C*
     C*  LOAD DEVIATION IF APPLIED TO MANUAL PREMIUM
     C*
     C     RRDV1L        IFEQ      'J'
     C*
     C                   Z-SUB     EHWDC2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDC2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     EHASP2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHASP2        IFLT      0
     C*
     C     EHASP2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHASP2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHASP2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C     RRDV1L        IFEQ      'J'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHWDC2    TAM,A
     C                   Z-SUB     EHD1W2        TAM(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TAM(A)        XXSUBT
     C*** BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHIDC2    TIN,A
     C                   Z-SUB     EHD1I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TIN(A)        XXSUBN
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP1            5
     C                   ENDIF                                                  EHD1S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD1S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD1S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  03/03/98  ************************************
     C*
     C*  LOAD 2ND DEVIATION IF APPLIED TO MANUAL PREMIUM
     C*
     C     RRDV2L        IFEQ      'J'
     C*
     C                   Z-SUB     EHWDV2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDV2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHDEV2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHDEV2        IFLT      0
     C*
     C     EHDEV2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHDEV2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHDEV2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C*
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      EHSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     EHSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** END ADD *****  03/03/98  ************************************
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C     RRDV2L        IFEQ      'J'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHWDV2    TAM,A
     C                   Z-SUB     EHD2W2        TAM(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TAM(A)        XXSUBT
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHIDV2    TIN,A
     C                   Z-SUB     EHD2I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TIN(A)        XXSUBN
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP1            5
     C                   ENDIF                                                  EHD2S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD2S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD2S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C*
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      EHSTFC        XXASP8            8
     C                   ENDIF                                                  EHSTFC
     C*
     C                   MOVEA     XXASP8        NUM8
     C     EHSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
     C*
     C* WAIVER OF SUBROGATION
     C*
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'J'
     C     ELWVFL        IFEQ      'J'
     C*** END CHG *****  09/29/99  ************************************
     C*
     C     RRWVHD        IFNE      *BLANKS
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ELSE
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(1)        HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(30)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   ENDIF
     C*
     C                   Z-ADD     EHWWAV        TAM(A)
     C                   Z-ADD     EHIWAV        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'J'
     C*
     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C*** END ADD *****  06/01/99  ************************************
     C*** BEGIN ADD ***  04/06/07  ************************************
     C*
     C*  LOAD SHORT RATE PENALTY IF APPLIED TO SUBJECT PREMIUM
     C*
     C     RRSRAF        IFEQ      'J'
     C     EHWSRA        ANDNE     *ZERO
     C*
     C                   Z-ADD     EHWSRA        TAM(A)
     C                   Z-ADD     0             TIN(A)
     C                   MOVEL     CON(36)       HDG(A)
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*** END ADD *****  04/06/07  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C*
     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO SUBJECT PREMIUM
     C*
     C     LDPFLG        IFEQ      'Y'
     C*
     C     LDPLV1        IFEQ      'J'
     C     *IN13         ANDEQ     *OFF
     C*
     C     EHPCT1        IFNE      *ZEROS
     C                   Z-SUB     EHWDCT        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDCT        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(3)        TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   END
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   END
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
     C*
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPLV1 = 'J'
     C*
     C     LDPLV2        IFEQ      'J'
     C     EHPCT2        ANDNE     *ZEROS
     C*
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(31)       TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C*
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF
     C*
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPFLG = 'Y'
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C*  SUBTOTAL
     C*
     C*** BEGIN DEL ***  03/03/98  ************************************
     C*****                MOVELCON,7     HDG,A
     C*****                Z-ADDXXSUBT    TAM,A
     C*****                Z-ADDXXSUBN    TIN,A
     C*****                ADD  1         A
     C*** END DEL *****  03/03/98  ************************************
     C*
     C*  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
     C*
     C     EHWXP1        ADD       EHWXP2        TAM(A)
     C                   ADD       EHWXP3        TAM(A)
     C                   Z-SUB     TAM(A)        XTADLD           11 2
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     EHWMNP        IFEQ      'Y'
     C     EHWVFL        ANDNE     'M'
     C     EHWVFL        ANDNE     'J'
     C                   SUB       EHWWAV        TAM(A)
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C     EHIXP1        ADD       EHIXP2        TIN(A)
     C                   ADD       EHIXP3        TIN(A)
     C                   Z-SUB     TIN(A)        XTADLN           11 2
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     EHIMNP        IFEQ      'Y'
     C     EHWVFL        ANDNE     'M'
     C     EHWVFL        ANDNE     'J'
     C                   SUB       EHIWAV        TIN(A)
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C                   MOVEA     CON(8)        TMP
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '.'           TMP(13)
     C                   CLEAR                   XXASP1
     C                   MOVE      EHEXM1        XXASP1
     C                   MOVEA     XXASP1        NUM
     C*** BEGIN CHG ***  03/03/98  ************************************
     C*****                MOVE NUM,3     TMP,12
     C*****                MOVE NUM,4     TMP,14
     C*****                MOVE NUM,5     TMP,15
     C                   MOVE      NUM(1)        TMP(12)
     C                   MOVE      NUM(2)        TMP(14)
     C                   MOVE      NUM(3)        TMP(15)
     C                   MOVE      NUM(4)        TMP(16)
     C                   MOVE      NUM(5)        TMP(17)
     C*** END CHG *****  03/03/98  ************************************
     C     EHWXP1        IFNE      TAM(A)
     C                   MOVE      '*'           TMP(19)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*** BEGIN ADD ***  03/03/98  ************************************
     C*** BEGIN ADD ***  10/04/01  ************************************
     C*
     C*  CONTRACTOR CREDIT
     C*
     C                   Z-SUB     EHWDC1        TAM(A)
     C                   Z-SUB     EHIDC1        TIN(A)
     C                   MOVE      '0'           *IN58
      ***** BEG CHG ******* 03/16/11 **********************************
     C**********           MOVEACON,10    TMP
     C     RRCCHD        IFNE      *BLANKS
     C                   MOVEL     RRCCHD        CON(10)
     C                   MOVEA     CON(10)       TMP
     C                   ELSE
     C                   MOVEA     CON(10)       TMP
     C                   ENDIF
      ***** END CHG ******* 03/16/11 **********************************
     C     EHASP1        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHASP1        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*** END ADD *****  10/04/01  ************************************
     C*
      *** BEG CHG ***** 12/17/07 **********************************
     C*  SCHEDULE DB/CR
     C*
     C********             Z-ADDEHWFRS    TAM,A
     C********             Z-ADDEHIFRS    TIN,A
     C********             Z-SUBTAM,A     XTADLD 112
     C********             Z-SUBTIN,A     XTADLN 112
     C*** BEGIN ADD ***  07/22/99  ************************************
     C********             Z-ADDEHWDC     TAM,A
     C********             Z-ADDEHIDC     TIN,A
     C*** END ADD *****  07/22/99  ************************************
     C********             MOVEACON,19    TMP
     C********   EHDBCR    IFLT 1
     C********   RRCRHD    IFNE *BLANKS
     C********             MOVEARRCRHD    TMP       P
     C********             ENDIF
     C********             ELSE
     C********   RRDBHD    IFNE *BLANKS
     C********             MOVEARRDBHD    TMP       P
     C********             ENDIF
     C********             ENDIF                           EHDBCR IFLT
     C*** BEG ADD *****  12/18/05  ************************************
     C********   CLTID     IFEQ '136'
     C********             MOVEACON,26    TMP
     C********             ENDIF
     C*** END ADD *****  12/18/05  ************************************
     C********             MOVEA*BLANKS   TMP,15
     C********             MOVE X'22'     TMP,15
     C********             MOVE '.'       TMP,18
     C********             CLEARXXASP1
     C*** BEG ADD *****  12/18/05  ************************************
     C********             SELEC
     C********   CLTID     WHEQ '136'
     C********   EHDBCR    MULT EHCMUL    XXWK54  54H
     C********             MOVE XXWK54    XXASP1  5
     C********             OTHER
     C*** END ADD *****  12/18/05  ************************************
     C********             MOVE EHDBCR    XXASP1  5
     C*** BEG ADD *****  12/18/05  ************************************
     C********             ENDSL
     C*** END ADD *****  12/18/05  ************************************
     C********             MOVEAXXASP1    NUM
     C********             MOVE NUM,1     TMP,17
     C********             MOVE NUM,2     TMP,19
     C********             MOVE NUM,3     TMP,20
     C********             MOVE NUM,4     TMP,21
     C********             MOVE NUM,5     TMP,22
     C********             MOVEATMP       HDG,A
     C********             CLEARTMP
     C********             ADD  1         A
     C*** END ADD *****  03/03/98  ************************************
     C     RRDCAP        IFEQ      ' '
     C                   EXSR      CALDBR
     C                   ENDIF                                                  ELDBCR IFNE
      *** END CHG ***** 12/17/07 **********************************
     C*
     C*
     C*  LOAD DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     C*
     C     RRDV1L        IFEQ      'E'
     C*
     C                   Z-SUB     EHWDC2        TAM(A)
     C                   ADD       EHWDC2        XTADLD
     C                   Z-SUB     EHIDC2        TIN(A)
     C                   ADD       EHIDC2        XTADLN
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     EHASP2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHASP2        IFLT      0
     C*
     C     EHASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHASP2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHASP2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV1L        IFEQ      'E'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHWDC2    TAM,A
     C***********          ADD  EHWDC2    XTADLD
     C                   Z-SUB     EHD1W2        TAM(A)
     C                   ADD       EHD1W2        XTADLD
     C***********          Z-SUBEHIDC2    TIN,A
     C                   Z-SUB     EHD1I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       EHIDC2        XTADLN
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP1            5
     C                   ENDIF                                                  EHD1S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD1S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD1S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  03/03/98  ************************************
     C*
     C*  LOAD 2ND DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     C*
     C     RRDV2L        IFEQ      'E'
     C*
     C                   Z-SUB     EHWDV2        TAM(A)
     C                   ADD       EHWDV2        XTADLD
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDV2        TIN(A)
     C                   ADD       EHIDV2        XTADLN
     C                   ADD       TIN(A)        XXSUBN
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHDEV2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHDEV2        IFLT      0
     C*
     C     EHDEV2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHDEV2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHDEV2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C*
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      EHSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     EHSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** END ADD *****  03/03/98  ************************************
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV2L        IFEQ      'E'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHWDV2    TAM,A
     C**********           ADD  EHWDV2    XTADLD
     C                   Z-SUB     EHD2W2        TAM(A)
     C                   ADD       EHD2W2        XTADLD
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TAM(A)        XXSUBT
     C*** BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHIDV2    TIN,A
     C                   Z-SUB     EHD2I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       EHIDV2        XTADLN
     C                   ADD       TIN(A)        XXSUBN
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP1            5
     C                   ENDIF                                                  EHD2S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD2S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD2S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C*
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      EHSTFC        XXASP8            8
     C                   ENDIF                                                  EHSTFC
     C*
     C                   MOVEA     XXASP8        NUM8
     C     EHSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
      ***** BEG ADD ***** 12/17/07 ***********************************
     C     RRDCAP        IFEQ      'Y'
     C                   EXSR      CALDBR
     C                   ENDIF
      ***** END ADD ***** 12/17/07 ***********************************
     C*
     C* WAIVER OF SUBROGATION
     C*
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'E'
     C     ELWVFL        IFEQ      'E'
     C*** END CHG *****  09/29/99  ************************************
     C*
     C     RRWVHD        IFNE      *BLANKS
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ELSE
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(1)        HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(30)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   ENDIF
     C*
     C                   Z-ADD     EHWWAV        TAM(A)
     C                   Z-ADD     EHIWAV        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'E'
     C*
     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C*** END ADD *****  06/01/99  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C*
     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO EXPERIENCE MODIFIER PRM
     C*
     C     LDPFLG        IFEQ      'Y'
     C*
     C     LDPLV1        IFEQ      'E'
     C     *IN13         ANDEQ     *OFF
     C*
     C     EHPCT1        IFNE      *ZEROS
     C*
     C                   Z-SUB     EHWDCT        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDCT        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(3)        TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   END
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   END
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
     C*
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPLV1 = 'E'
     C*
     C     LDPLV2        IFEQ      'E'
     C     EHPCT2        ANDNE     *ZEROS
     C*
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(31)       TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C*
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF
     C*
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
      *
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPFLG = 'Y'
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C*  ADDITIONAL LOAD
     C*
     C*** BEGIN DEL ***  06/01/99  ************************************
     C******     EHWMNP    IFNE 'Y'
     C******               ADD  EHWDC1    XTADLD
     C******               ADD  EHWSPR    XTADLD
     C******     RRRDPR    IFEQ 'S'
     C******               ADD  EHWDCT    XTADLD
     C******               ENDIF                           RRRDPR IFEQ
     C******               ELSE
     C******               Z-ADD*ZERO     XTADLD
     C******               ENDIF
     C******
     C******     EHIMNP    IFNE 'Y'
     C******               ADD  EHIDC1    XTADLN
     C******               ADD  EHISPR    XTADLN
     C******     RRRDPR    IFEQ 'S'
     C******               ADD  EHIDCT    XTADLN
     C******               ENDIF                           RRRDPR IFEQ
     C******               ELSE
     C******               Z-ADD*ZERO     XTADLN
     C******               ENDIF
     C******
     C******               MOVELCON,9     HDG,A
     C******               Z-ADDXTADLD    TAM,A
     C******               Z-ADDXTADLN    TIN,A
     C******               ADD  1         A
     C*** END DEL *****  06/01/99  ************************************
     C*** BEGIN DEL ***  10/04/01  ************************************
     C*
     C*  CONTRACTOR CREDIT
     C*
     C*******              Z-SUBEHWDC1    TAM,A
     C*******              Z-SUBEHIDC1    TIN,A
     C*******              MOVE '0'       *IN58
     C*******              MOVEACON,10    TMP
     C*******    EHASP1    IFNE *ZERO
     C*******              MOVEA*BLANKS   TMP,15
     C*******              MOVE X'22'     TMP,15
     C*******              MOVE '%'       TMP,22
     C*******              MOVE '.'       TMP,19
     C*******              MOVE EHASP1    XXASP1  5
     C*******              MOVEAXXASP1    NUM
     C*******    NUM,1     IFNE '0'
     C*******              MOVE NUM,1     TMP,16
     C*******              END
     C*******    NUM,2     IFNE '0'
     C*******    NUM,1     ORGT '0'
     C*******              MOVE NUM,2     TMP,17
     C*******              END
     C*******              MOVE NUM,3     TMP,18
     C*******              MOVE NUM,4     TMP,20
     C*******              MOVE NUM,5     TMP,21
     C*******              ENDIF
     C*******              MOVEATMP       HDG,A
     C*******              ADD  1         A
     C*** END DEL *****  10/04/01  ************************************
     C*
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
     C*
     C     RRRDPR        IFEQ      'S'
     C*** BEGIN ADD ***  04/14/10  ************************************
     C     LDPFLG        ANDNE     'Y'
     C*** END ADD *****  04/14/10  ************************************
     C     *IN13         ANDEQ     *OFF
     C                   Z-SUB     EHWDCT        TAM(A)
     C                   Z-SUB     EHIDCT        TIN(A)
     C                   MOVEA     CON(3)        TMP
     C     EHDDCT        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   MOVEA     CON(31)       TMP
     C*
     C     EHDD2P        IFNE      *ZERO
     C*
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   ENDIF
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C*
     C                   ENDIF
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C***  END   ADD  ***  04/07/05 *************************************
     C                   ENDIF
     C*
     C*
     C*** BEGIN ADD ***  10/01/01  ************************************
     C                   MOVEL     CON(21)       HDG(A)
     C                   Z-ADD     EHWLOS        TAM(A)
     C                   Z-ADD     EHILOS        TIN(A)
     C*
     C                   ADD       1             A
     C*** END ADD *****  10/01/01  ************************************
     C*** BEGIN ADD ***  04/06/07  ************************************
     C*
     C*  LOAD SHORT RATE PENALTY IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRSRAF        IFEQ      'S'
     C     EHWSRA        ANDNE     *ZERO
     C*
     C                   Z-ADD     EHWSRA        TAM(A)
     C                   Z-ADD     0             TIN(A)
     C                   MOVEL     CON(36)       HDG(A)
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRSRAF IFEQ
     C*** END ADD *****  04/06/07  ************************************
     C*
     C*  STANDARD PREMIUM
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(11)       HDG(A)                         STD PREM
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(27)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     EHWSPR        TAM(A)
     C                   Z-ADD     EHISPR        TIN(A)
     C*** BEGIN ADD ***  09/20/02  ************************************
     C     EHWMNP        IFEQ      'Y'
     C     EHWVFL        ANDNE     'M'
     C     EHWVFL        ANDNE     'J'
     C     EHWVFL        ANDNE     'E'
     C                   SUB       EHWWAV        TAM(A)
     C                   ENDIF
     C     EHIMNP        IFEQ      'Y'
     C     EHWVFL        ANDNE     'M'
     C     EHWVFL        ANDNE     'J'
     C     EHWVFL        ANDNE     'E'
     C                   SUB       EHIWAV        TIN(A)
     C                   ENDIF
     C*** END ADD *****  09/20/02  ************************************
     C                   ADD       1             A
     C*
     C*  LOAD DEVIATION IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRDV1L        IFEQ      'S'
     C*
     C                   Z-SUB     EHWDC2        TAM(A)
     C                   Z-SUB     EHIDC2        TIN(A)
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     EHASP2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHASP2        IFLT      0
     C*
     C     EHASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHASP2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHASP2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     C     RRDV1L        IFEQ      'S'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHWDC2    TAM,A
     C                   Z-SUB     EHD1W2        TAM(A)
     C**********           Z-SUBEHIDC2    TIN,A
     C                   Z-SUB     EHD1I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP1            5
     C                   ENDIF                                                  EHD1S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD1S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD1S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  03/03/98  ************************************
     C*
     C*  LOAD 2ND DEVIATION IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRDV2L        IFEQ      'S'
     C*
     C                   Z-SUB     EHWDV2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDV2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHDEV2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHDEV2        IFLT      0
     C*
     C     EHDEV2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHDEV2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHDEV2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C*
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      EHSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     EHSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** END ADD *****  03/03/98  ************************************
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     C     RRDV2L        IFEQ      'S'
     C****BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHWDV2    TAM,A
     C                   Z-SUB     EHD2W2        TAM(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TAM(A)        XXSUBT
     C*** BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHIDV2    TIN,A
     C                   Z-SUB     EHD2I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TIN(A)        XXSUBN
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP1            5
     C                   ENDIF                                                  EHD2S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD2S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD2S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C*
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      EHSTFC        XXASP8            8
     C                   ENDIF                                                  EHSTFC
     C*
     C                   MOVEA     XXASP8        NUM8
     C     EHSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
     C*
     C* WAIVER OF SUBROGATION
     C*
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'S'
     C     ELWVFL        IFEQ      'S'
     C*** END CHG *****  09/29/99  ************************************
     C*
     C     RRWVHD        IFNE      *BLANKS
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ELSE
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(1)        HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(30)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   ENDIF
     C*
     C                   Z-ADD     EHWWAV        TAM(A)
     C                   Z-ADD     EHIWAV        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'S'
     C*
     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C*** END ADD *****  06/01/99  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C*
     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
     C*
     C     LDPFLG        IFEQ      'Y'
     C*
     C     LDPLV1        IFEQ      'S'
     C     *IN13         ANDEQ     *OFF
     C*
     C     EHPCT1        IFNE      *ZEROS
     C*
     C                   Z-SUB     EHWDCT        TAM(A)
     C                   Z-SUB     EHIDCT        TIN(A)
     C                   MOVEA     CON(3)        TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   END
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   END
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
     C*
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPLV1 = 'S'
     C*
     C     LDPLV2        IFEQ      'S'
     C     EHPCT2        ANDNE     *ZEROS
     C*
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   MOVEA     CON(31)       TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C*
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF
     C*
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
     C*
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C*  DISCOUNT
     C*
     C                   Z-SUB     EHWDC3        TAM(A)
     C                   Z-SUB     EHIDC3        TIN(A)
     C*** BEGIN CHG ***  02/09/98  ************************************
     C*********            MOVEACON,12    TMP
     C     RRDSHD        IFEQ      *BLANKS
     C                   MOVEA     CON(12)       TMP
     C                   ELSE
     C                   MOVEA     RRDSHD        TMP
     C                   ENDIF                                                  RRDSHD IFEQ
     C*** END ADD *****  02/09/98  ************************************
     C     EHASP3        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C                   MOVE      EHASP3        XXASP3            5
     C                   MOVEA     XXASP3        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C*
     C*  LOAD DEVIATION IF APPLIED TO NORMAL PREMIUM
     C*
     C     RRDV1L        IFEQ      'D'
     C*
     C                   Z-SUB     EHWDC2        TAM(A)
     C                   Z-SUB     EHIDC2        TIN(A)
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     EHASP2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHASP2        IFLT      0
     C*
     C     EHASP2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHASP2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHASP2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO NORMAL PREMIUM
     C     RRDV1L        IFEQ      'D'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C**********           Z-SUBEHWDC2    TAM,A
     C                   Z-SUB     EHD1W2        TAM(A)
     C**********           Z-SUBEHIDC2    TIN,A
     C                   Z-SUB     EHD1I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD1S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP1            5
     C                   ENDIF                                                  EHD1S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD1S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD1S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  03/03/98  ************************************
     C*
     C*  LOAD 2ND DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
     C*
     C     RRDV2L        IFEQ      'D'
     C*
     C                   Z-SUB     EHWDV2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDV2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHDEV2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHDEV2        IFLT      0
     C*
     C     EHDEV2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C*
     C                   ELSE
     C*
     C                   MOVE      EHDEV2        XXASP1            5
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP1        NUM
     C*
     C     EHDEV2        IFLT      0
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHASP2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C*
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C*
     C                   ELSE
     C*
     C                   MOVE      EHSTFC        XXASP8            8
     C*
     C                   ENDIF                                                  EHASP2
     C*
     C                   MOVEA     XXASP8        NUM8
     C*
     C     EHSTFC        IFLT      0
     C*
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C                   END
     C*** END ADD *****  03/03/98  ************************************
     C*** BEG   ADD ****** 06/21/10 ************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO DISCOUNTED PREMIUM
     C     RRDV2L        IFEQ      'D'
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHWDV2    TAM,A
     C                   Z-SUB     EHD2W2        TAM(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TAM(A)        XXSUBT
     C*** BEG   CHG ****** 12/06/10 ************************************
     C***********          Z-SUBEHIDV2    TIN,A
     C                   Z-SUB     EHD2I2        TIN(A)
     C*** END   CHG ****** 12/06/10 ************************************
     C                   ADD       TIN(A)        XXSUBN
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD2S2        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(19)
     C*
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2            5 2
     C                   MOVE      XXASP2        XXASP1
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP1            5
     C                   ENDIF                                                  EHD2S2
     C*
     C                   MOVEA     XXASP1        NUM
     C     EHD2S2        IFLT      0
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '-'           TMP(16)
     C                   ELSE
     C                   MOVE      '-'           TMP(17)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   ENDIF                                                  EHD2S2 IFLT
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(16)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(17)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(20)
     C                   MOVE      NUM(5)        TMP(21)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C*
     C     EHSTFC        IFNE      *ZERO
     C                   MOVEA     *BLANKS       TMP(12)
     C                   MOVE      X'22'         TMP(12)
     C                   MOVE      '.'           TMP(16)
     C*
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   MOVE      XXASN8        XXASP8
     C                   ELSE
     C                   MOVE      EHSTFC        XXASP8            8
     C                   ENDIF                                                  EHSTFC
     C*
     C                   MOVEA     XXASP8        NUM8
     C     EHSTFC        IFLT      0
     C     NUM8(2)       IFNE      '0'
     C                   MOVE      '-'           TMP(13)
     C                   ELSE
     C                   MOVE      '-'           TMP(14)
     C                   ENDIF                                                  NUM8,2 IFNE
     C*
     C                   ENDIF                                                  EHSTFC IFLT
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(14)
     C                   END
     C                   MOVE      NUM8(2)       TMP(15)
     C                   MOVE      NUM8(3)       TMP(17)
     C                   MOVE      NUM8(4)       TMP(18)
     C                   MOVE      NUM8(5)       TMP(19)
     C                   MOVE      NUM8(6)       TMP(20)
     C                   MOVE      NUM8(7)       TMP(21)
     C                   MOVE      NUM8(8)       TMP(22)
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C                   END
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ***  06/01/99  ************************************
     C*
     C* WAIVER OF SUBROGATION
     C*
     C*** BEGIN CHG ***  09/29/99  ************************************
     C*********  RRWVFL    IFEQ 'D'
     C     ELWVFL        IFEQ      'D'
     C*** END CHG *****  09/29/99  ************************************
     C*
     C     RRWVHD        IFNE      *BLANKS
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ELSE
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(1)        HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(30)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   ENDIF
     C*
     C                   Z-ADD     EHWWAV        TAM(A)
     C                   Z-ADD     EHIWAV        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'D'
     C*
     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C*** END ADD *****  06/01/99  ************************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C*
     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO DISCOUNTED PREMIUM
     C*
     C     LDPFLG        IFEQ      'Y'
     C*
     C     LDPLV1        IFEQ      'D'
     C     *IN13         ANDEQ     *OFF
     C*
     C     EHPCT1        IFNE      *ZEROS
     C                   Z-SUB     EHWDCT        TAM(A)
     C                   Z-SUB     EHIDCT        TIN(A)
     C                   MOVEA     CON(3)        TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT1        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   END
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   END
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
     C*
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  LDPLV1 = 'D'
     C*
     C     LDPLV2        IFEQ      'D'
     C     EHPCT2        ANDNE     *ZEROS
     C*
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   MOVEA     CON(31)       TMP
     C*
     C                   MOVEA     *BLANKS       TMP(11)
     C                   MOVE      X'22'         TMP(11)
     C                   MOVE      '%'           TMP(22)
     C                   MOVE      '.'           TMP(15)
     C                   MOVEL     EHPCT2        XXASP4           12
     C                   MOVEA     XXASP4        NUM1
     C*
     C     NUM1(1)       IFNE      '0'
     C                   MOVE      NUM1(1)       TMP(12)
     C                   ENDIF
     C*
     C     NUM1(2)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C                   MOVE      NUM1(2)       TMP(13)
     C                   ENDIF
     C*
     C     NUM1(3)       IFNE      '0'
     C     NUM1(1)       ORGT      '0'
     C     NUM1(2)       ORGT      '0'
     C                   MOVE      NUM1(3)       TMP(14)
     C                   ENDIF
     C*
     C                   MOVE      NUM1(4)       TMP(16)
     C                   MOVE      NUM1(5)       TMP(17)
     C                   MOVE      NUM1(6)       TMP(18)
     C                   MOVE      NUM1(7)       TMP(19)
     C                   MOVE      NUM1(8)       TMP(20)
     C                   MOVE      NUM1(9)       TMP(21)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** END ADD ***** 12/30/09 *************************************
     C*** BEG ADD *****  01/27/03  ************************************
     C*
     C*  MASSACHUSETTS ARAP SURCHARGE
     C*
     C     EHWRP1        ADD       EHWRP2        XXWRP            13 2
     C                   ADD       EHWRP3        XXWRP
     C*
     C     XXWRP         IFNE      *ZERO
     C     EHIRP1        ADD       EHIRP2        XXIRP            13 2
     C                   ADD       EHIRP3        XXIRP
     C                   MOVEL     CON(23)       HDG(A)
     C                   Z-ADD     XXWRP         TAM(A)
     C                   Z-ADD     XXIRP         TIN(A)
     C                   ADD       1             A
     C                   ENDIF
     C*** END ADD *****  01/27/03  ************************************
     C*
     C*  NORMAL PREMIUM
     C*
     C                   MOVEL     CON(13)       HDG(A)
     C                   Z-ADD     EHWNMP        TAM(A)
     C                   Z-ADD     EHINMP        TIN(A)
     C                   ADD       1             A
      *
      ***** BEG ADD ******* 01/15/07 **********************************
      *
      *  Harbor Exposure
      *
     C                   MOVEL     CON(35)       HDG(A)
     C                   Z-ADD     EHWHXM        TAM(A)
     C                   Z-ADD     EHIHXM        TIN(A)
     C                   ADD       1             A
      ***** END ADD ******* 01/15/07 **********************************
     C*** BEG ADD *****  07/06/06  ************************************
      *
      * reinsurance charge
      *
     C     EHWARC        IFNE      *ZEROS
     C                   MOVEL     CON(34)       HDG(A)
     C                   Z-ADD     EHWARC        TAM(A)
     C                   Z-ADD     EHIARC        TIN(A)
     C                   ADD       1             A
     C                   ENDIF
      *
     C*** END ADD *****  07/06/06  ************************************
     C*
     C*  EXPENSE CONSTANT
     C*
     C                   MOVEL     CON(14)       HDG(A)                         EXP CONSTANT
     C*
     C*** BEGIN CHG ***  10/13/99  ************************************
     C*****      EHWMNP    IFEQ 'Y'
     C*****      EHPRTD    OREQ *ZERO
     C*****                Z-ADDEHPRTD    TAM,A
     C*****                ELSE
     C*****                Z-ADDECEXCO    TAM,A
     C*****                END
     C*
     C*****                Z-ADDECEXCO    TIN,A
     C*** BEGIN CHG ***  04/04/02  ************************************
     C*********  EHPROR    IFNE *ZERO
     C*********  EHPRTD    DIV  EHPROR    XXWK10 110H
     C*********            Z-ADDXXWK10    EHPRTD
     C*********            ENDIF                           EHPROR IFNE
     C*** BEGIN CHG ***  11/14/02  ************************************
     C*************        MOVE EHCO#     QQCO#
     C*************        MOVE EHFND     QQFND
     C*************        MOVE EHFYR     QQFYR
     C*************        MOVE EHEMP#    QQEMP#
     C*************        MOVE EHTRDT    QQTRDT
     C*************        Z-ADD0         QQPREM
     C*************        CALL 'SRFEXP'
     C*************        PARM           QQCO#   3
     C*************        PARM           QQFND   3
     C*************        PARM           QQFYR   3
     C*************        PARM           QQEMP#  9
     C*************        PARM           QQTRDT  70
     C*************        PARM           QQPREM 112
     C*
     C***********QQPREM    IFNE *ZERO
     C*************        Z-ADDQQPREM    EHPRTD
     C*************        ENDIF
     C*** BEGIN CHG ***  08/20/03  ************************************
     C*********            Z-ADDEHPRTF    EHPRTD
     C     EHFAF         IFNE      'A'
     C                   Z-ADD     EHPRTF        EHPRTD
     C                   ENDIF                                                  EHFAF IFNE
     C*** END CHG *****  08/20/03  ************************************
     C*** END CHG *****  11/14/02  ************************************
     C*** END CHG *****  04/04/02  ************************************
     C*
     C*** BEGIN CHG ***  01/24/06  ************************************
     C*************        Z-ADDEHPRTD    TAM,A
     C*************        Z-ADDEHPRTD    TIN,A
     C     EHWAPR        IFNE      *ZERO
     C                   Z-ADD     EHPRTD        TAM(A)
     C                   ENDIF
     C     EHIAPR        IFNE      *ZERO
     C                   Z-ADD     EHPRTD        TIN(A)
     C                   ENDIF
     C*** END CHG *****  01/24/06  ************************************
     C*** END CHG *****  10/13/99  ************************************
     C*
     C                   ADD       1             A
      ***** beg add ***  07/13/11  ************************************
      *
      * UNEARNED PREMIUM SURCHARGE AMOUNT
      *
     C     EHWUSA        IFNE      *ZERO
     C                   MOVEL     CON(43)       HDG(A)
     C                   Z-ADD     EHWUSA        TAM(A)
     C                   ADD       1             A
     C                   ENDIF                                                  EHWUSA
      *
     C***** end add ***  07/13/11  ************************************
     C*** BEGIN ADD ***  01/17/03  ************************************
     C*
     C* TERRORISM RISK ACT
     C*
     C     EHWTER        IFNE      *ZERO
     C*
     C                   MOVEL     CON(22)       HDG(A)
     C*** begin add ***  03/02/09  ************************************
     C*
     C* Set the terrorism heading fields to optional or default heading.
     C*
     C     RRTERH        IFNE      *BLANKS
     C                   MOVEL     RRTERH        HDG(A)
     C                   ENDIF
     C*** end add *****  03/02/09  ************************************
     C                   Z-ADD     EHWTER        TAM(A)
     C                   Z-ADD     EHITER        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  EHWTER IFNE
     C*** END ADD *****  01/17/03  ************************************
     C*** BEGIN ADD ***  01/07/05  ************************************
     C*
     C* TERRORISM RISK ACT
     C*
     C     EHWDTR        IFNE      *ZERO
     C*
     C                   MOVEL     CON(25)       HDG(A)
     C*** begin add ***  03/02/09  ************************************
     C*
     C* Set the terrorism heading fields to optional or default heading.
     C*
     C     RRDTRH        IFNE      *BLANKS
     C                   MOVEL     RRDTRH        HDG(A)
     C                   ENDIF
     C*** end add *****  03/02/09  ************************************
     C                   Z-ADD     EHWDTR        TAM(A)
     C                   Z-ADD     EHIDTR        TIN(A)
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  EHWTER IFNE
     C*** END ADD *****  01/07/05  ************************************
     C*
     C*  ANNUAL PREMIUM
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(15)       HDG(A)                         EST PREM
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(28)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C     EHWMNP        IFEQ      'Y'
     C*** BEGIN ADD ***  08/07/07  ************************************
     C     EXBEXP        ANDNE     'Y'
     C*** END ADD *****  08/07/07  ************************************
     C     EHPRTD        OREQ      0
     C                   Z-ADD     EHWNMP        TAM(A)
     C     RRDV1L        IFEQ      'D'
     C                   SUB       EHWDC2        TAM(A)
     C                   ENDIF
     C                   ELSE
     C*** BEGIN CHG ***  10/13/99  ************************************
     C*******    EHWNMP    ADD  ECEXCO    TAM,A
     C     EHWNMP        ADD       EHPRTD        TAM(A)
     C*** END CHG *****  10/13/99  ************************************
     C     RRDV1L        IFEQ      'D'
     C                   SUB       EHWDC2        TAM(A)
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD ***  01/17/03  ************************************
     C                   ADD       EHWTER        TAM(A)
     C*** END ADD *****  01/17/03  ************************************
     C*** BEGIN ADD ***  01/07/05  ************************************
     C                   ADD       EHWDTR        TAM(A)
     C*** END ADD *****  01/07/05  ************************************
     C*** BEGIN ADD ***  07/06/06  ************************************
     C                   ADD       EHWARC        TAM(A)
     C*** END ADD *****  07/06/06  ************************************
     C*** BEG ADD *****  01/15/07  ************************************
     C                   ADD       EHWHXM        TAM(A)
     C*** END ADD *****  01/15/07  ************************************
     C*** BEGIN ADD ****** 07/13/11 ************************************
     C                   ADD       EHWUSA        TAM(A)
     C*** END   ADD ****** 07/13/11 ************************************
     C                   Z-ADD     EHIAPR        TIN(A)
     C                   ADD       1             A
     C*
     C*  LOAD PREMIUM INTO ARRAY IF NOT EQUAL 1
     C*
     C                   Z-ADD     EHWAPR        TAM(A)
     C                   Z-ADD     *ZERO         TIN(A)
     C                   MOVEA     CON(16)       TMP
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '.'           TMP(17)
     C                   MOVE      EHPROR        XXPROR            5
     C                   MOVEA     XXPROR        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(15)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(16)
     C                   END
     C                   MOVE      NUM(3)        TMP(18)
     C                   MOVE      NUM(4)        TMP(19)
     C                   MOVE      NUM(5)        TMP(20)
     C                   MOVEA     TMP           HDG(A)
     C***  Begin Add  ***  12/31/10 *************************************
     C                   CLEAR                   TMP
     C***  End   Add  ***  12/31/10 *************************************
     C                   ADD       1             A
     C*
     C*** BEGIN ADD ***  01/24/06  ************************************
     C*
     C* RETROSPECTIVE PREMIUM.
     C*
     C     EHFAF         IFEQ      'X'
     C*
     C                   MOVEL     CON(32)       HDG(A)
     C     EHWRPR        SUB       EHWAPR        TAM(A)
     C                   Z-ADD     0             TIN(A)
     C                   ADD       1             A
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     CON(33)       HDG(A)
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     CON(29)       HDG(A)
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     EHWRPR        TAM(A)
     C                   Z-ADD     0             TIN(A)
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  EHFAF IFEQ
     C*** END ADD *****  01/24/06  ************************************
     C*** Beg chg *****  12/17/10  ***************************************
     C     CLTID         IFEQ      '071'
     C     FN3PPE        ANDEQ     'Y'
     C*** Beg chg *****  12/31/10  ***************************************
     C***********          MOVELCON,39    HDG,A
     C***********          Z-ADDEHENPM    TAM,A
     C***********          Z-ADD0         TIN,A
     C***********          ADD  1         A
     C***********          MOVELCON,40    HDG,A
     C***********          Z-ADDEHANPM    TAM,A
     C***********          Z-ADD0         TIN,A
     C***********          ADD  1         A
     C*
     C     EHFAF         IFEQ      'E'
     C     EHFAF         OREQ      'R'
     C                   MOVEL     CON(39)       HDG(A)
     C                   ENDIF
     C*
     C     EHFAF         IFEQ      'A'
     C                   MOVEL     CON(40)       HDG(A)
     C                   ENDIF
     C*
     C                   Z-ADD     EHNPRM        TAM(A)
     C                   ADD       1             A
     C*** BEG ADD ***** 01/01/11 *****************************************
      *
     C     EHFAF         IFEQ      'E'
     C     EHFAF         OREQ      'R'
     C                   MOVEL     CON(41)       HDG(A)
     C                   ENDIF
     C*
     C     EHFAF         IFEQ      'A'
     C                   MOVEL     CON(42)       HDG(A)
     C                   ENDIF
     C*
     C                   Z-ADD     EHPRM         TAM(A)
     C                   ADD       1             A
     C*** END ADD ***** 01/01/11 *****************************************
     C                   ENDIF
     C*** End chg *****  12/17/10  ***************************************
     C                   ENDSR                                                  LOAD03
     C*
      *** BEG ADD ***** 12/17/07 **********************************
     C     CALDBR        BEGSR
     C*  SCHEDULE DB/CR
     C*
     C                   Z-ADD     EHWFRS        TAM(A)
     C                   Z-ADD     EHIFRS        TIN(A)
     C                   Z-SUB     TAM(A)        XTADLD           11 2
     C                   Z-SUB     TIN(A)        XTADLN           11 2
     C*** BEGIN ADD ***  07/22/99  ************************************
     C                   Z-ADD     EHWDC         TAM(A)
     C                   Z-ADD     EHIDC         TIN(A)
     C*** END ADD *****  07/22/99  ************************************
     C                   MOVEA     CON(19)       TMP
     C     EHDBCR        IFLT      1
     C     RRCRHD        IFNE      *BLANKS
     C                   MOVEA(P)  RRCRHD        TMP
     C                   ENDIF
     C                   ELSE
     C     RRDBHD        IFNE      *BLANKS
     C                   MOVEA(P)  RRDBHD        TMP
     C                   ENDIF
     C                   ENDIF                                                  EHDBCR IFLT
     C*** BEG ADD *****  12/18/05  ************************************
     C     CLTID         IFEQ      '136'
     C                   MOVEA     CON(26)       TMP
     C                   ENDIF
     C*** END ADD *****  12/18/05  ************************************
     C                   MOVEA     *BLANKS       TMP(15)
     C                   MOVE      X'22'         TMP(15)
     C                   MOVE      '.'           TMP(18)
     C                   CLEAR                   XXASP1
     C*** BEG ADD *****  12/18/05  ************************************
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C     EHDBCR        MULT(H)   EHCMUL        XXWK54            5 4
     C                   MOVE      XXWK54        XXASP1            5
     C                   OTHER
     C*** END ADD *****  12/18/05  ************************************
     C                   MOVE      EHDBCR        XXASP1            5
     C*** BEG ADD *****  12/18/05  ************************************
     C                   ENDSL
     C*** END ADD *****  12/18/05  ************************************
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(17)
     C                   MOVE      NUM(2)        TMP(19)
     C                   MOVE      NUM(3)        TMP(20)
     C                   MOVE      NUM(4)        TMP(21)
     C                   MOVE      NUM(5)        TMP(22)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*** END ADD *****  03/03/98  ************************************
     C*
     C                   ENDSR
      *** END ADD ***** 12/17/07 **********************************
**  DS - TRANSACTION TYPE
ORIGINAL BILLING
REVISED BILLING
AUDIT BILLING
RETROSPECTIVE BILLING
**
Manual Premium
Incr Limit
Deductible
MC Credit
Safety Credit
Deviation
Subtotal
Exp Mod
Addl Load
Contr Credit
Standard Prem
Discount
Total Normal
Expense Const
Annual Prem
Prorata Factor
2nd Incr Limit
2nd Deviation
Schedule DR/CR
Waiver of Subrogation
Loss Constant
Foreign Terrorism
Mass ARAP Surcharge
Voluntary Labor Chg
Domestic Terr Chg
Company Factor
Standard contribution                                        01/14/10  27
Annual contribution                                          01/14/10  28
Retrospective contribution                                   01/14/10  29
Manual contribution                                          01/14/10  30
2nd Deductible
Retro adjustment
Retrospective premium
Facultative Reinsurance                                      07/06/06
Harbor Exposure Fee                                          01/15/07
Short Rate Penalty
Stbl Factor                                                  01/14/10  37
HCN Credit                                                   11/10/10
Estimated non-premium                                        12/17/10
Audited non-premium                                          12/17/10
Estimated premium                                            01/01/11
Audited premium                                              01/01/11
Unearn prem surcharge                                        07/13/11
NOT USED 44
NOT USED 45
NOT USED 46
NOT USED 47
NOT USED 48
NOT USED 49
NOT USED 50
