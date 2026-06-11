     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4800R - MODIFY EMPLOYER EFFECTIVE DATES
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM ALLOWS THE USER TO CHANGE ALL POLICY DATES       *
     H* SUCH AS INCEPTION DATE, POLICY START DATE, ANNIVERSARY        *
     H* EFFECTIVE DATE, AND PAYROLL CLASS CODE DATES.                 *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   EMPLOYER QUOTE MENU                                         *
     H*   W0051R - WORK WITH EMPLOYER POLICY YEARS                    *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 11/13/90               PROJECT: MDAAV                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/03/20 C:298 Project WC2231                                 *
     H*                This program is no longer used. WTPOLPFDT      *
     H*                is new redesigned program.                     *
     H*                                                               *
     H* 12/05/11 C:279 SUPPORT: 78147                                 *
     H*                ADDED CHECK FOR BILLING CODE WHEN MODIFYING    *
     H*                POLICY EFFECTIVE DATE.                         *
     H*                                                               *
     H* 11/29/11 C:136 SUPPORT: WA093 78509                           *
     H*                FIX DATE EDITS IN ERRCK2                       *
     H*                                                               *
     H* 07/15/11 C:136 SUPPORT: WA071 77382                           *
     H*                REWORK AGENT HISTORY EFF DT EDIT               *
     H*                - DO NOT UPDATE EFDT IF CURRENT A/H            *
     H*                  EFDT IS VALID FOR NEW POLICY EFDT            *
     H*                - INVOKE EDIT IF PRV A/H RECORD EXISTS         *
     H*                  AND POLICY EFDT IS MOVED TO EARLIER DATE
     H*                - RUN EDITS ACROSS ALL DIVISIONS IF            *
     H*                  SCUPDD = 'Y' FLASH MSG THAT ALL DIVISIONS    *
     H*                  MUST BE CHECKED                              *
     H*                                                               *
     H* 04/13/11 C:136 SUPPORT: WA146 76550                           *
     H*                DO NOT BLANK OUT AHEFDT IF NO CHANGE IS        *
     H*                MADE AND USER ENTERS THROUGH SCREEN            *
     H*                                                               *
     H* 01/25/11 C:136 SUPPORT: WA045 75576                           *
     H*                ADJUST LOGIC FOR POPULATING AGENT HISTORY      *
     H*                EFF DATE                                       *
     H*                                                               *
     H* 08/20/10 C:65  SUPPORT: WA139 77796                           *
     H*                ADD CODING TO ENSURE THE UPDATE OF PAYROLL     *
     H*                DOES NOT CREATE DUPLICATE PAYROLL CLASSES.     *
     H*                MUST CHECK IF CLASS ALREADY EXISTS FOR NEW     *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                CLEAR THE 2ND PERIOD DEVIATION DATES WHEN ARD  *
     H*                IS CHANGED OR MODIFICATION OF POLICY DATES MAK-*
     H*                ES THEM INVALID WHEN DEVIATION ARE SPLIT.      *
     H*                                                               *
     H* 05/09/08 C:136 SUPPORT: WA133 61804                           *
     H*                UPDATE THE YEAR AND EFDT FOR:                  *
     H*                   VENDOR HISTORY, LOCATIONS, LOCATION DETAIL, *
     H*                   DIRECT DEBIT, OWNER OFFICERS, NAMED INSUREDS*
     H*                                                               *
     H* 03/31/07 C:180 PROJECT: WA139/52857
     H*                Allow yr to be entered for non-commercial groups
     H*                Fill in expiration dates when user hits
     H*                field exit/enter.
     H*
     H* 02/07/07 C:136 SUPPORT: WA114 55160                           *
     H*               -PREVENT BILL AUX FROM BEING CALLED TWICE       *
     H*                IN A ROW UNEXPECTEDLY. CLEAR SCBSTY AFTER      *
     H*                BSCD SEACRCH.                                  *
     H*               -ALSO ADDED CORRECTIONS FOR BILL AUX AND        *
     H*                BSCD SEARCH.                                   *
     H*                                                               *
     H* 10/19/06 C:136 SUPPORT: WA071 55436                           *
     H*                CORRECT BILLING CODE AND BILLING AUX SEARCH    *
     H*                                                               *
     H* 09/06/06 C:136 SUPPORT: WA129 54190                           *
     H*                ADJUST THE BILLING AUXILIARY SEARCH TO         *
     H*                ACCOUNT FOR THE ACTUAL BILLING START DATE      *
     H*                                                               *
     H* 06/12/06 C:136 SUPPORT: WA129 53115                           *
     H*                ADJUST SRBILL EDIT AND BSCD SEARCH TO COUNT    *
     H*                THE CORRECT NUMBER OF INSTALLMENTS             *
     H*                                                               *
     H* 04/03/06 C:168 PROJECT: 129WNU                                *
     H*                Calculate the number of billing for the 15th   *
     H*                which fall whithin the policy effective dates  *
     H*                - Done for the call to W2155R and W2157R       *
     H*                                                               *
     H* 03/28/06 C:136 SUOPPORT: WA132 49638                          *
     H*                FIX BILLING SCHEDULE CODE AND BILLING          *
     H*                AUXILIARY SEARCH (REFERENCE 39357)             *
     H*                                                               *
     H* 03/19/04 C:61  SUPPORT WA081 40994                            *
     H*                Update Billing Transaction File records with   *
     H*                correct policy year when appropriate.          *
     H*                                                               *
     H* 04/29/02 C:61  SUPPORT WA122 32256                            *
     H*                Update credit expiration dates.                *
     H*                                                               *
     H* 04/25/02 C:61  SUPPORT WA122 32237                            *
     H*                Remove edit which requires new policy "TO"     *
     H*                date that is less than previous policy "TO"    *
     H*                date to be equal to the employer CX date.      *
     H*                                                               *
     H* 02/20/02 C:61  SUPPORT WA072 31248                            *
     H*                Update agent history record with change in     *
     H*                policy year.                                   *
     H*                                                               *
     H* 07/11/00 C:61  PROJECT WC930                                  *
     H*                Verify employer detail record exists before    *
     H*                update of next division.                       *
     H*                                                               *
     H*  11/10/99 C:33  PROJECT: WC959                                *
     H*                -ALLOW USER TO ENTER THE NEW POLICY YEAR.      *
     H*                -UPDATE MAINTENANCE FIELDS IN FILES.           *
     H*                                                               *
     H*  10/20/99 C:113 PROJECT WC577                                 *
     H*                 Recognize a change in policy year.            *
     H*                                                               *
     H*  07/23/99 C:61 PROJECT WC930                                  *
     H*                Add option to update all divisions.            *
     H*                                                               *
     H*  03/23/98 C:85 PROJECT WC545                                  *
     H*                DELETED CODE WHICH FORCED A SEARCH IF NEW      *
     H*                POLICY DATES WERE ENTERED.                     *
     H*                                                               *
     H*  02/02/98 C:35 PROJECT WC577                                  *
     H*                ADD BILLING START DATE TO SCREEN 2             *
     H*                                                               *
     H*  11/25/97 C:79 PROJECT WC545                                  *
     H*                -DISPLAYS INFORMATIONAL WINDOW IF EMPLOYER HAS *
     H*                 MORE THAN ONE POLICY YEAR.                    *
     H*                -DELETED CODE TO UPDATE AGENT HISTORY FILE     *
     H*                -POLICY FROM AND TO DATE SHOW IN SAME COLOR    *
     H*                -PGM UPDATES CREDIT EFFECTIVE DATES            *
     H*                -BSCODE SEARCH IS INVOKED IF EITHER POLICY FROM*
     H*                 OR TO DATES ARE CHANGED.                      *
     H*                                                               *
     H*  11/07/97 C:33 PROJECT WC577                                  *
     H*                V3 ENHANCEMENTS (NO COMMENTS)                  *
     H*                                                               *
     H*  01/22/96 C:33 PROJECT CM007                                  *
     H*                ALLOW FOR CHANGE TO POLICY TO DATE.            *
     H*                                                               *
     H*  12/08/95 C:33 PROJECT CM007                                  *
     H*                IF FUND IS DEFINED WITH UNCOMMON EXP DATES     *
     H*    (FNCGGL=Y)->(WDF2P/F2CT01 = 'Y') POLICY FROM DATE MUST BE  *
     H*                BETWEEN THE FUND YEAR (WDF1P) START AND END    *
     H*                DATES, AND THE POLICY TO DATE IS NOT REQUIRED  *
     H*                TO BE WITHIN THE FUND YEAR.                    *
     H*                                                               *
     H*  09/03/93 C10  PROJECT WC199                                  *
     H*                ADDED EDIT TO POLICY FROM DATE TO MAKE SURE    *
     H*                THAT THE POLICY FROM DATE ENTERED IS NOT       *
     H*                GREATER THAN THE POLICY TO DATE.               *
     H*                ADDED CODE TO ALLOW USER TO CHANGE BILLING     *
     H*                SCHEDULE CODE.                                 *
     H*                                                               *
     H*  05/12/92 KWK  PROJECT WC138                                  *
     H*                CHANGED UPDATE OF AHEFDT TO NOT PERFORM        *
     H*                CHAIN/READP BUT INSTEAD PERFORM CHAIN AND      *
     H*                READE UNTIL WE REACH THE FIRST ACTIVE AGENT.   *
     H*                                                               *
     H*  06/26/91 TAA  PROJECT AHZ                                    *
     H*                UPDATE AGENT QUOTE EFFECTIVE DATE IF THERE IS  *
     H*                A HIT IN WAQTP.                                *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   09  INQUIRY ONLY                                            *
     H*   10  HELP REQUEST                                            *
     H*   18  ROLL UP REQUEST                                         *
     H*   19  ROLL DOWN REQUEST                                       *
     H*                                                               *
     H*   SCR01                                                       *
     H*   20  VALIDATE EMPLOYER NUMBER                                *
     H*   21  VALIDATE EMPLOYER FUND YEAR                             *
     H*                                                               *
     H*   SCR02                                                       *
     H*   20  INVALID ROLL REQUEST                                    *
     H*   21  VALIDATE NEW EFFECTIVE DATE                             *
     H*   22  NEW EFFECTIVE DATE MUST FALL WITHIN FUND POLICY         *
     H*       START AND ENDING DATES                                  *
     H*   23  VALIDATE ANNIVERSARY RATING DATE                        *
     H*   24  ANNIVERSARY RATING DATE MUST BE LESS THAN OR EQUAL      *
     H*       TO POLICY FROM DATE                                     *
     H*   25  IF MODIFIER 1 IS ENTERED, THERE MUST BE A CORRESPONDING *
     H*       EFFECTIVE DATE                                          *
     H*   26  IF MODIFIER 2 IS ENTERED, THERE MUST BE A CORRESPONDING *
     H*       EFFECTIVE DATE                                          *
     H*   27  IF MODIFIER 3 IS ENTERED, THERE MUST BE A CORRESPONDING *
     H*       EFFECTIVE DATE                                          *
     H*   28  VALIDATE MODIFIER 1 EFFECTIVE DATE                      *
     H*   29  ALL MODIFIER EFFECTIVE DATES MUST BE IN ASCENDING ORDER *
     H*   30  VALIDATE MODIFIER 2 EFFECTIVE DATE                      *
     H*   31  VALIDATE MODIFIER 3 EFFECTIVE DATE                      *
     H*   32  MODIFIER EFFECTIVE DATE 1 MUST BE LESS THAN OR EQUAL    *
     H*       TO POLICY FROM DATE                                     *
     H*   33  POLICY FROM DATE CANNOT BE GREATER THAN POLICY TO DATE. *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*   61  GENERAL TEST INDICATOR                                  *
     H*   71  INVALID VALUE ENTERED                                   *
     H*   72  UPDATE ALL DIVISIONS                                    *
     H*   98  TO DATE INVALID                                         *
     H*   99  ALLOW CHANGE OF TO DATE                                 *
     H*                                                               *
     H*****************************************************************
     FW4800F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      ***** Beg Del ***** 03/03/20 **********************************************
     F*                                    INFDS(FIDS01)
     F*SMCOL     IF   E           K DISK
     F*WMUPL     IF   E           K DISK
     F*WMEMP     IF   E             DISK
     F*                                     RENAME(PEM01:PEM02)
     F*WMEML     UF   E           K DISK
     F*                                     INFDS(DBFDSE)
     F*WDELP     IF   E             DISK
     F*                                     RENAME(PEL01:PEL02)
     F*WDELL     UF   E           K DISK
     F*WMEPL     UF   E           K DISK
     F*** BEGIN ADD ***  08/20/10  ******************************************
     F*WMEPL1E   UF   E           K DISK
     F*                                     RENAME(PEP01:PEP02)
     F*** END ADD *****  08/20/10  ******************************************
      ***** BEG CHG ******* 02/20/02 **********************************
     F******WMAHL   IF  E           K        DISK
     F*WMAHL     UF   E           K DISK
      ***** END CHG ******* 02/20/02 **********************************
      ***** BEG ADD ******* 03/19/04 **********************************
      *
      * Billing Transaction
     F*WABLL1    UF   E           K DISK
      ***** END ADD ******* 03/19/04 **********************************
     F*WAQTL7    UF   E           K DISK
     F*WMFNL     IF   E           K DISK
     F*                                    INFDS(DBFDSF)
     F*WDF1L     IF   E           K DISK
     f*** begin add ***  12/02/12  ***************************************
     F*WMALL     UF   E           K DISK
     F*WMOOL     UF   E           K DISK
     F*WDVHL     UF   E           K DISK
     F*WMDBL     UF   E           K DISK
     F*WMANL     UF   E           K DISK
     F*WDLDL1    UF   E           K DISK
     f*** end add *****  12/02/12  ***************************************
     F*WTABL     IF   E           K DISK
     F*WTBSP     IF   E             DISK
     F*                                    RENAME(PBS01:PBS02)
     F*WMBXP     IF   E             DISK

     D*
     D* RR              S              9    DIM(3) CTDATA PERRCD(1)
     D* KEYF            S              8  0 DIM(2)
     D* KEYM            S              5  0 DIM(2)
     D* X0              S              1    DIM(5)
     D* XB              S              1    DIM(5)
     D** BEG ADD *** 11/25/97 *****************************************
     D***  BEG   CHG  ***  06/21/10  *********************************
     D************        CRED       18  7 0
     D***  END   CHG  ***  06/21/10  *********************************
     D** END ADD *** 11/25/97 *****************************************
     D*
     D* OPTNM           C                   CONST('Modify Policy Effect-
     D*                                     ive Dates')
      *** BEG ADD ***** 07/15/11 ********************************
     D* DMSG            C                   CONST('Check all divisions -
     D*                                     for error.')
      *** END ADD ***** 07/15/11 ********************************
     D*
     D*DBFDSF          DS
     D* DBRRNF               397    400B 0
     D*DBFDSE          DS
     D* DBRRNE               397    400B 0
     D*                DS
     D* DSDTTM                 1     12  0
     D* DSTIME                 1      6  0
     D* DSDATE                 7     12  0
     D*                DS
     D* XXRECF                 1     10
     D* XXSCR#                 2      5
      *** BEGIN ADD 04/03/06 ******************************************
     D*                DS
     D* XDFND                  1      3  0
     D* XX15                   4      5  0
     D* XXINS                  6      7  0
     D* XXDAY                  8      9  0
     D* XX2155                 1      9  0
      *** END   ADD 04/03/06 ******************************************
     D*                DS
     D* XXPFD                  1      8  0
     D* XXPDD                  3      4  0
     D*                DS
     D* XXWK80                 1      8  0
     D* XXWDD                  3      4  0
     D*                DS
     D* BDAYS                  1      8  0
     D* XXBDD                  3      4  0
     D*                DS
     D* DT                     1    080  0
     D*                                    DIM(10)
     D* SCDT01               001    008  0
     D* SCDT02               009    016  0
     D* SCDT03               017    024  0
     D* SCDT04               025    032  0
     D* SCDT05               033    040  0
     D* SCDT06               041    048  0
     D* SCDT07               049    056  0
     D* SCDT08               057    064  0
     D* SCDT09               065    072  0
     D* SCDT10               073    080  0
     D*                DS
     D* CL                     1     60
     D*                                    DIM(10)
     D* SCCL01                 1      6
     D* SCCL02                 7     12
     D* SCCL03                13     18
     D* SCCL04                19     24
     D* SCCL05                25     30
     D* SCCL06                31     36
     D* SCCL07                37     42
     D* SCCL08                43     48
     D* SCCL09                49     54
     D* SCCL10                55     60
     D*                DS
     D* TP                     1     10
     D*                                    DIM(10)
     D* SCTP01                 1      1
     D* SCTP02                 2      2
     D* SCTP03                 3      3
     D* SCTP04                 4      4
     D* SCTP05                 5      5
     D* SCTP06                 6      6
     D* SCTP07                 7      7
     D* SCTP08                 8      8
     D* SCTP09                 9      9
     D* SCTP10                10     10
     D*                DS
     D* SS                     1     10
     D*                                    DIM(10)
     D* SCST01                 1      1
     D* SCST02                 2      2
     D* SCST03                 3      3
     D* SCST04                 4      4
     D* SCST05                 5      5
     D* SCST06                 6      6
     D* SCST07                 7      7
     D* SCST08                 8      8
     D* SCST09                 9      9
     D* SCST10                10     10
     D*                DS
     D* AM                     1     90  0
     D*                                    DIM(10)
     D* SCAM01                 1      9  0
     D* SCAM02                10     18  0
     D* SCAM03                19     27  0
     D* SCAM04                28     36  0
     D* SCAM05                37     45  0
     D* SCAM06                46     54  0
     D* SCAM07                55     63  0
     D* SCAM08                64     72  0
     D* SCAM09                73     81  0
     D* SCAM10                82     90  0
     D*                DS
     D* XXDT                   1      7  0
     D* XXYY                   2      3  0
     D* XXMM                   4      5  0
     D* XXDD                   6      7  0
      *** BEG ADD ***** 03/28/06 ***************************
     D* XXMDD                  4      7  0
      *** END ADD ***** 03/28/06 ***************************
     D* XXMMDD                 4      7  0
     D*                DS
     D***  BEG   CHG  ***  06/21/10  *********************************
     D*******                                 1 1260CRED
     D* CRED                   1    154  0
     D*                                    DIM(22)
     D***  END   CHG  ***  06/21/10  *********************************
     D* ELCREF                 1      7  0
     D* ELCREX                 8     14  0
     D* ELDVEF                15     21  0
     D* ELDVEX                22     28  0
     D* ELD2EF                29     35  0
     D* ELD2EX                36     42  0
     D* ELILEF                43     49  0
     D* ELILEX                50     56  0
     D* ELI2EF                57     63  0
     D* ELI2EX                64     70  0
     D* ELDCEF                71     77  0
     D* ELDCEX                78     84  0
     D* ELMCEF                85     91  0
     D* ELMCEX                92     98  0
     D* ELSCEF                99    105  0
     D* ELSCEX               106    112  0
     D* ELSDEF               113    119  0
     D* ELSDEX               120    126  0
     D***  BEG   ADD  ***  06/21/10  *********************************
     D* ELD1D2               127    133  0
     D* ELD1X2               134    140  0
     D* ELD2D2               141    147  0
     D* ELD2X2               148    154  0
     D***  END   ADD  ***  06/21/10  *********************************
     D***  BEGIN ADD  ***  11/10/99  *********************************
     D*                DS
     D* X7MXDT                 1      7  0
     D* X7MXCY                 1      3  0
     D* X7MXMM                 4      5  0
     D***  END   ADD  ***  11/10/99  *********************************
     D*FIDS01        E DS                  EXTNAME(SZQ2P)
     D*PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      ***** End Del ***** 03/03/20 ***********************************

      ***** Beg Add ***** 03/03/20 ***********************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  inFound                       1

     d isFound         s              1n   inz
      ***** End Add ***** 03/03/20 ***********************************

      ***** Beg Del ***** 03/03/20 ***********************************
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C*    COKY          KLIST
     C*                  KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND
     C*
     C*    FNKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C*    F1KY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C***  BEGIN CHG  ***  11/10/99  *********************************
     C**********           KFLD           XXFYR
     C*                  KFLD                    XXNFYR
     C***  END   CHG  ***  11/10/99  *********************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C*    EMKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
      ***** BEG ADD ******* 07/23/99 **********************************
      *
     C*    EMKY1         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
      *
      ***** END ADD ******* 07/23/99 **********************************
     C*
     C*    EMKY3         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    SCFND
     C*                  KFLD                    XXEMP#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C*    ELKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXFYR
     C*
     C*    ELKY1         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*
     C*    ELKY3         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    EMDIV
     C*                  KFLD                    XXFYR
     C*
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*** BEG CHG ***** 07/15/11 *************************************
     C***********ELKYN     KLIST
     C***********          KFLD           EMCO#
     C***********          KFLD           SCFND
     C***********          KFLD           SCEMP#
     C***********          KFLD           SCDIV
     C***********          KFLD           XXNFYR
     C*    ELKYN         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXNFYR
     C*** END CHG ***** 07/15/11 *************************************
     C***  END   ADD  ***  11/10/99  *********************************
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C*    EPKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*
     C*    EPKY1         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXFYR
     C*
     C*    EPKY2         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXFYR
     C*                  KFLD                    XXEFD1
     C*                  KFLD                    XXCLAS
     C*
     C*    EPKY3         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXFYR
     C*                  KFLD                    XXEFDT
     C*** BEGIN ADD ***  08/20/10  *************************************
     C*
     C*    EPKY4         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    SVFYR
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXCLAS
     C*                  KFLD                    XXSPEC            1
     C*                  KFLD                    SVEXT
     C*                  KFLD                    SVEFDT
     C*** END ADD *****  08/20/10  *************************************
     C*
     C*  AGENT HISTORY FILE
     C*
     C*    AHKY1         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXFYR
      ***** BEG ADD ******* 02/20/02 **********************************
     C*
     C*    AHKY2         KLIST
     C*                  KFLD                    ELCO#
     C*                  KFLD                    ELFND
     C*                  KFLD                    ELEMP#
     C*                  KFLD                    ELDIV
     C*                  KFLD                    SCFYR
      ***** END ADD ******* 02/20/02 **********************************
      *** BEG ADD ***** 07/15/11 **************************************
     C*    AHKY3         KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXFYR
     C*                  KFLD                    KKEFDT            7 0
      *** END ADD ***** 07/15/11 **************************************
      ***** BEG ADD ******* 03/19/04 **********************************
      *
      *  Billing Transaction
      *
     C*    BLKY          KLIST
     C*                  KFLD                    ELCO#
     C*                  KFLD                    ELFND
     C*                  KFLD                    ELEMP#
     C*                  KFLD                    ELDIV
     C*                  KFLD                    SCFYR
      ***** END ADD ******* 03/19/04 **********************************
     C*
     C*  AGENT QUOTE FILE
     C*
     C*    QTKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    XXSTAT
     C*                  KFLD                    XXAGT#
     C*
     C*  TABLE FILE KEY
     C*
     C*    TBKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXKEY
     C*** begin add ***  12/02/12  ***************************************
     C*
     C*  Additional Location File Key List
     C*
     C*    ALKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*
     C*  Additional Named Insured File Key List
     C*
     C*    ANKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*
     C*  Owner/Officer File Key List
     C*
     C*    OOKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*
     C*  Vendor History File Key List
     C*
     C*    VHKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    SCFYR
     C*
     C*  Direct Debit File Key List
     C*
     C*    DBKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*
     C*  Location Year File Key List
     C*
     C*    LDKY          KLIST
     C*                  KFLD                    XXCO#
     C*                  KFLD                    XXFND
     C*                  KFLD                    XXEMP#
     C*                  KFLD                    XXDIV
     C*                  KFLD                    SCFYR
     C*** end add *****  12/02/12  ***************************************
     C*
     C*    *LIKE         DEFINE    EMCO#         @@CO#
     C*    *LIKE         DEFINE    EMFND         @@FND
     C*    *LIKE         DEFINE    EMEMP#        @@EMP#
     C*    *LIKE         DEFINE    EMDIV         @@DIV
     C*    *LIKE         DEFINE    ELFYR         @@FYR
     C*    *LIKE         DEFINE    EMCO#         XXCO#
     C*    *LIKE         DEFINE    EMFND         XXFND
     C*    *LIKE         DEFINE    EMEMP#        XXEMP#
     C*    *LIKE         DEFINE    EMDIV         XXDIV
     C*    *LIKE         DEFINE    EMNAME        XXNAME
     C*    *LIKE         DEFINE    ELFYR         XXFYR
     C*    *LIKE         DEFINE    ELPFDT        XXNEFD
     C*    *LIKE         DEFINE    ELPFDT        XXANDT
     C***  BEG   ADD  ***  06/21/10  *********************************
     C*    *LIKE         DEFINE    ELPFDT        @@ANDT
     C***  END   ADD  ***  06/21/10  *********************************
     C*    *LIKE         DEFINE    ELPFDT        XXEFD1
     C*    *LIKE         DEFINE    ELPFDT        XXEFDT
     *** BEG ADD *** 11/25/97 *****************************************
     C*    *LIKE         DEFINE    ELPFDT        XXOEDT
      ***** BEG ADD ******* 04/29/02 **********************************
     C*    *LIKE         DEFINE    ELPTDT        XXOXDT
      ***** END ADD ******* 04/29/02 **********************************
     C*    *LIKE         DEFINE    ELPFDT        XXFOLD
     C*    *LIKE         DEFINE    ELPFDT        XXTOLD
     C*    *LIKE         DEFINE    ELFYR         ZZFYR
     *** END ADD *** 11/25/97 *****************************************
     C*    *LIKE         DEFINE    ELEMD1        XXDAT1
     C*    *LIKE         DEFINE    ELEMD2        XXDAT2
     C*    *LIKE         DEFINE    EPCLAS        XXCLAS
     C*** BEGIN ADD ***  08/20/10  ******************************************
     C*    *LIKE         DEFINE    EPEXT         SVEXT
     C*** END ADD *****  08/20/10  ******************************************
     C*    *LIKE         DEFINE    QTSTAT        XXSTAT
     C*    *LIKE         DEFINE    QTAGT#        XXAGT#
     C*    *LIKE         DEFINE    ELPTDT        XXNTOD
     C*    *LIKE         DEFINE    ELPTDT        XXEMD1
     C*    *LIKE         DEFINE    BSKEY         XXKEY
     C*    *LIKE         DEFINE    SCPTDT        XXOFDT
     C*    *LIKE         DEFINE    SCNEFD        XXOTDT
     C*    *LIKE         DEFINE    SCPTDT        XXPTDT
     C*    *LIKE         DEFINE    SCNEFD        XXPFDT
     C*    *LIKE         DEFINE    SCEMD1        X1EMD1
     C*    *LIKE         DEFINE    SCNEFD        XXPF
     C*    *LIKE         DEFINE    SCNEFD        XXEF
     C*    *LIKE         DEFINE    Q2RECF        SVRECF
      ***** BEG ADD ******* 07/23/99 **********************************
     C*    *LIKE         DEFINE    ELDIV         SVDIV
      ***** END ADD ******* 07/23/99 **********************************
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*    *LIKE         DEFINE    ELFYR         XXNFYR
     C*    *LIKE         DEFINE    FNMXEX        XXMXEX
     C*    *LIKE         DEFINE    X7MXCY        XXMXCY
     C*    *LIKE         DEFINE    ELDT          X7DATE
     C*    *LIKE         DEFINE    *IN90         DTLESS
     C*    *LIKE         DEFINE    *IN90         LEAPYR
     C***  END   ADD  ***  11/10/99  *********************************
     C*
     C*    *ENTRY        PLIST
     C*                  PARM                    @@PRDS           40
     C*                  PARM                    @@CALL            1
     C*                  PARM                    @@KC              1
     C*                  PARM                    @@KL              1
     C*                  PARM                    @@CO#
     C*                  PARM                    @@FND
     C*                  PARM                    @@EMP#
     C*                  PARM                    @@DIV
     C*                  PARM                    @@FYR
     C*
     C*    DATCHK        PLIST
     C*                  PARM                    SRDTI             8 0
     C*                  PARM      *BLANKS       SRFFMT            8
     C*                  PARM      *BLANKS       SRTFMT            8
     C*                  PARM                    SRDTO            10
     C*                  PARM                    SRERR             1
     C*
     C*    DTCYMD        PLIST
     C*                  PARM                    SRDTI
     C*                  PARM      *BLANKS       SRFFMT
     C*                  PARM      '*CYMD'       SRTFMT
     C*                  PARM                    SRDTO
     C*                  PARM                    SRERR
     C*
     C*    DTMDYY        PLIST
     C*                  PARM                    SRDTI
     C*                  PARM      '*CYMD'       SRFFMT
     C*                  PARM      *BLANKS       SRTFMT
     C*                  PARM                    SRDTO
     C*                  PARM                    SRERR
     C*
     C*  EMPLOYER MASTER PARM LIST
     C*
     C*    EMPPL         PLIST
     C*    *INKC         PARM                    ##KC              1
     C*    *INKL         PARM                    ##KL              1
     C*                  PARM                    ##CALL            1
     C*                  PARM      *ZERO         ##RRN             9 0
     C*                  PARM      DBRRNF        ##RRN2            9 0
     C*                  PARM      'Q'           ##QTE             1
     C*
     C*  EMPLOYER DETAIL PARM LIST
     C*
     C*    DETPL         PLIST
     C*    *INKC         PARM                    ##KC
     C*    *INKL         PARM                    ##KL
     C*                  PARM                    ##CALL
     C*                  PARM      *ZERO         ##RRN
     C*                  PARM      DBRRNE        ##RRN2
     C*                  PARM      'Q'           ##QTE
     C*_______________________________________________________________
      *** BEG ADD ***** 01/25/11 **************************************
     C*                  TIME                    DSDTTM
     C*                  Z-ADD     DSDATE        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         SYSDAT            7 0
      *** END ADD ***** 01/25/11 **************************************
      *** BEG ADD ***** 07/15/11 **************************************
     C*                  MOVEL     SRDTO         X7DATE
      *** END ADD ***** 07/15/11 **************************************
      ***** BEG ADD ******* 07/23/99 **********************************
     C*                  MOVE      'Y'           SCUPDD
      ***** END ADD ******* 07/23/99 **********************************
      **** BEGIN  ADD 04/03/06 ****************************************
     C*                  CALL      'RTVCLTID'
     C*                  PARM                    XXCLT#            3
      *** END ADD ***** 04/03/06 *************************************
     C*
     C*                  MOVEL(P)  OPTNM         ##DATA
     C*
     C*                  CALL      'SRCENTER'
     C*                  PARM                    ##DATA           80
     C*                  PARM      40            ##LENG            3 0
     C*
     C*                  MOVEL     ##DATA        SCPRDS
     *** BEG ADD *** 11/25/97 *****************************************
     C*
     C*  SET INDICATOR FOR FIRST TIME THROUGH
     C*
     C*                  MOVE      *ON           *IN97
     *** END ADD *** 11/25/97 *****************************************
     C*_______________________________________________________________
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C*    TOP           TAG
     C*
     C*    Q1USER        CHAIN     PUP01                              60
     C*
     C*                  Z-ADD     UPCO#         XXCO#
     C*                  Z-ADD     UPFND         XXFND
     C*                  Z-ADD     UPEMP#        XXEMP#
     C*                  Z-ADD     UPDIV         XXDIV
     *** BEG DEL *** 11/25/97 *****************************************
     C******************** Z-ADDUPFYR     XXFYR
     *** END DEL *** 11/25/97 *****************************************
     C*                  MOVE      *OFF          SKIP01
     C*
     C*    *IN60         IFEQ      *ON
     C*                  Z-ADD     *ZERO         XXCO#
     C*                  Z-ADD     *ZERO         XXFND
     C*                  Z-ADD     *ZERO         XXEMP#
     C*                  Z-ADD     *ZERO         XXDIV
     C*                  Z-ADD     *ZERO         XXFYR
     C*                  ENDIF                                                  *IN60=*OFF
     C*
     C*    Q1PRMS        IFGE      3
     C*                  Z-ADD     @@CO#         XXCO#
     C*                  Z-ADD     @@FND         XXFND
     C*                  Z-ADD     @@EMP#        XXEMP#
     C*                  Z-ADD     @@DIV         XXDIV
     C*                  Z-ADD     @@FYR         XXFYR
     C*                  MOVE      *ON           SKIP01            1
     C*                  ENDIF                                                  Q1PRMS>=3
     C*
      *** BEG ADD ***** 07/15/11 **************************
     C*                  Z-ADD     0             DIVCNT            5 0
     C*                  MOVE      *BLANK        SCDMSG
     C*    EMKY1         SETLL     PEM01
     C*    EMKY1         READE(N)  PEM01                                  60
     C*    *IN60         DOWEQ     *OFF
     C*                  ADD       1             DIVCNT
     C*    EMKY1         READE(N)  PEM01                                  60
     C*                  ENDDO                                                  *IN65=*OFF
      *** END ADD ***** 07/15/11 **************************
     C*
     C*                  Z-ADD     XXFND         SCFND
     C*                  Z-ADD     XXEMP#        SCEMP#
     C*                  Z-ADD     XXDIV         SCDIV
     C*                  Z-ADD     XXFYR         SCFYR
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C*    COKY          CHAIN     PCO01                              60
     C*
     C*                  MOVEL(P)  CONAME        SRDFLD
     C*
     C*                  CALL      'SRCENTER'
     C*                  PARM                    SRDFLD           80
     C*                  PARM      40            SRDLEN            3 0
     C*
     C*                  MOVEL     SRDFLD        SCCONM
     C*
     C*    FNKY          CHAIN     PFN01                              60
     C*                  MOVEL(P)  FNNAME        SCNAME
     C*
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*    FNCGFL        IFEQ      'Y'
     C*
     C*    FNMXEX        IFEQ      *ZERO
     C*                  Z-ADD     12            FNMXEX
     C*                  ENDIF                                                  FNMXEX=*ZERO
     C*
     C*                  Z-ADD     FNMXEX        XXMXEX
     C*                  Z-ADD     *ZEROS        XXMXCY
     C*
     C*    XXMXEX        DOWGE     12
     C*                  ADD       1             XXMXCY
     C*                  SUB       12            XXMXEX
     C*                  ENDDO                                                  XXMXEX>=12
     C*
     C*                  ENDIF                                                  FNCGFL=Y
     C***  END   ADD  ***  11/10/99  *********************************
     C*
     C*                  TIME                    DSDTTM
     C*
     C*    @@CALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C*                  MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*    SKIP01        CABEQ     *ON           SCR01T
     C*_______________________________________________________________
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C*    STRT          TAG
     C*
     C*    Q2RECF        IFEQ      'SCR01'
     C*                  EXFMT     SCR01
     C*                  ENDIF                                                  Q2RECF=SCR01
     C*
     C*    XXDOWN        IFEQ      *ON
     C*                  MOVEL(P)  RR(3)         SCUP
     C*                  ELSE
     C*                  MOVEL(P)  RR(1)         SCUP
     C*                  ENDIF                                                  XXDOWN=*ON
     C*
     C*    XXUP          IFEQ      *ON
     C*                  MOVE(P)   *BLANKS       SCDOWN
     C*                  ELSE
     C*                  MOVEL(P)  RR(2)         SCDOWN
     C*                  ENDIF                                                  XXUP=*ON
     C*
     C*    Q2RECF        IFEQ      'SCR02'
     C*                  EXFMT     SCR02
     C*                  ENDIF                                                  Q2RECF=SCR02
     C*
     C*_______________________________________________________________
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C*    *IN11         IFEQ      *ON
     C*
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*    Q2RECF        IFEQ      'SCR02'
     C*                  EXSR      RESET
     C*                  EXSR      DATES2
     C*                  ENDIF                                                  Q2RECF=SCR02
     C***  END   ADD  ***  11/10/99  *********************************
     C*
     C*===============================================================
     C*  F3=EXIT
     C*===============================================================
     C*
     C*    *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*===============================================================
     C*  F10=SEARCH
     C*===============================================================
     C*
     C*    *INKJ         IFEQ      *ON
     C*
     C*                  MOVE      '2'           ##CALL
     C*
     C*                  SELECT
     C*
     C*    HFLD          WHENEQ    'SCEMP#'
     C*    HFLD          OREQ      'SCDIV'
     C*
     C*                  CALL      'W0050R'      EMPPL
     C*
     C*    ##RRN         IFNE      *ZERO
     C*
     C*    ##RRN         CHAIN     WMEMP                              60
     C*
     C*                  Z-ADD     EMEMP#        SCEMP#
     C*                  Z-ADD     EMDIV         SCDIV
     C*                  MOVE      *ON           *IN41
     C*
     C*                  ELSE
     C*
     C*                  MOVE      *ON           *IN40
     C*
     C*                  ENDIF                                                  ##RRN<>*ZERO
     C*
     C**
     C*    HFLD          WHENEQ    'SCFYR'
     C*
     C*    SCEMP#        IFEQ      *ZERO
     C*
     C*                  MOVE      'Y'           ##CALL
     C*
     C*                  CALL      'W0050R'      EMPPL
     *** BEG ADD *** 11/25/97 *****************************************
     C*    *INKL         IFEQ      *ON
     C*
     C*                  MOVE      *ON           *IN41
     C*                  GOTO      STRT
     C*
     C*                  ENDIF                                                  ##INKL=ON
     *** END ADD *** 11/25/97 *****************************************
     C*
     C*    ##RRN2        IFNE      *ZERO
     C*
     C*    ##RRN2        CHAIN     WDELP                              60
     C*
     C*                  Z-ADD     ELEMP#        SCEMP#
     C*                  Z-ADD     ELDIV         SCDIV
     *** BEG CHG *** 11/25/97 *****************************************
     C******************** Z-ADDELDIV     SCFYR
     C******************** GOTO SCR01T
     C*                  Z-ADD     ELFYR         SCFYR
     C*                  GOTO      STRT
     *** END CHG *** 11/25/97 *****************************************
     C*
     C*                  ELSE
     C*
     C*    ##RRN         IFNE      *ZERO
     C*
     C*    ##RRN         CHAIN     WMEMP                              60
     C*
     C*                  Z-ADD     EMEMP#        SCEMP#
     C*                  Z-ADD     EMDIV         SCDIV
     C*
     C*                  ENDIF                                                  ##RRN<>*ZERO
     C*
     C*                  MOVE      *ON           *IN41
     C*
     C*                  ENDIF                                                  ##RRN2<>*ZERO
     C*
     C*                  ELSE
     C*
     C*                  Z-ADD     SCEMP#        XXEMP#
     C*                  Z-ADD     SCDIV         XXDIV
     *** BEG ADD *** 11/25/97 *****************************************
     C*    EMKY          CHAIN(N)  WMEML                              60
     C*                  MOVE      '2'           ##CALL
     *** END ADD *** 11/25/97 *****************************************
     C*    EMKY          CHAIN(N)  PEL01                              60
     C*
     C*                  CALL      'W0051R'      DETPL
     C*
     C*    ##RRN         IFNE      *ZERO
     C*
     C*    ##RRN         CHAIN     WDELP                              60
     C*
     C*                  Z-ADD     ELFYR         SCFYR
     *** BEG CHG *** 11/25/97 *****************************************
     C******************** GOTO SCR01T
     C*                  GOTO      STRT
     *** BEG CHG *** 11/25/97 *****************************************
     C*
     C*                  ELSE
     C*
     C*                  MOVE      *ON           *IN41
     C*
     C*                  ENDIF                                                  ##RRN<>*ZERO
     C*
     C*                  ENDIF                                                  SCEMP#=*ZERO
     C*
     C*    HFLD          WHENEQ    'SCBSCD'
     C*
     C*    SCBSTY        IFEQ      *BLANK
     C*
      *** BEG ADD ***** 03/28/06 **************************************
      *
      *  GET THE NUMBER OF POSSIBLE MONTHLY INSTALLMENTS FOR THE POLICY WHICH
      *  CURRENTLY BILLS THE FIRST OF THE MONTH. SUBTRACT THIS NUMBER FROM
      *  12 TO GET THE MONTH VALUE FOR THE BILLING AUXILIARY EFFECTIVE DATE.
      *  USE THE DAY VALUE OF THE BSDT OR PFDT FOR THE DAY VALUE. CALL W2155R
      *  WITH THE ##RRN PARM EQUAL TO FFF00MMDD WHERE FFF IS THE FUND; MM THE
      *  NUMBER OF MONTHS; DD THE DAY VALUE.  THESE VALUES WILL BE USED TO
      *  FILTER THE BILLING SCHEDULE CODE RECORD SELECTION IN W2155R.
      *
     C*                  CLEAR                   ##RRN
     C*                  MOVEL     SCFND         ##RRN
     C*    SCBSDT        IFNE      *ZERO
     C*                  Z-ADD     SCBSDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXBGDT            7 0
     C*                  ELSE
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXBGDT
     C*                  ENDIF
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXNTDT            7 0
     C*                  Z-ADD     XXBGDT        XXDT
      *
      * Checks how many billings for the 1st will fall within the
      * effective dates
      *
      *** BEG ADD ***** 10/19/06 *************************************
     C*                  MOVE      XXBGDT        SVDD              2 0
      *** END ADD ***** 10/19/06 *************************************
     C*                  Z-ADD     1             XXDD
      *** BEG ADD ***** 06/12/06 *************************************
      *** BEG CHG ***** 02/07/07 *************************************
     C*****************    Z-ADDXXDT      XXBGDT
     C*                  Z-ADD     XXDT          XXSTDT            7 0
      *** END CHG ***** 02/07/07 *************************************
      *** END ADD ***** 06/12/06 *************************************
     C*                  Z-ADD     0             XXINCT            2 0
      *** BEG ADD ***** 02/07/07 *************************************
     C*                  Z-ADD     XXPTDT        SVPTDT            7 0
     C*                  MOVE      '01'          SVPTDT
     C*    XXSTDT        IFLT      XXBGDT
     C*    XXNTDT        ANDEQ     SVPTDT
     C*                  ADD       1             XXINCT
     C*                  ENDIF
      *** END ADD ***** 02/07/07 *************************************
     C*    XXDT          DOWLE     XXNTDT
     C*    XXDT          IFGE      XXBGDT
     C*    XXDT          ANDLT     XXNTDT
     C*                  ADD       1             XXINCT
     C*                  ENDIF
     C*                  ADD       1             XXMM
     C*    XXMM          IFEQ      13
     C*                  Z-ADD     1             XXMM
     C*                  ADD       1             XXYY
     C*                  ENDIF
     C*                  ENDDO
      *
      *** END ADD ***** 03/28/06 **************************************
      **** BEGIN  ADD 04/03/06 ****************************************
      *
      * Let remember the values calculated above
      *
     C*                  Z-ADD     *ZEROS        XX2155
     C*                  MOVEL     SCFND         XDFND
     C*                  Z-ADD     XXINCT        XXINS
      *** BEG CHG ***** 10/19/06 *************************************
     C***********          MOVE XXBGDT    XXDAY
     C*                  MOVE      SVDD          XXDAY
      *** END CHG ***** 10/19/06 *************************************
      *
      * Checks how many billings for the 15th will fall within the
      * effective dates
      *
     C*    XXCLT#        IFEQ      '129'
      *
     C*                  Z-ADD     XXBGDT        XXDT
     C*                  Z-ADD     15            XXDD
      *** BEG ADD ***** 06/12/06 *************************************
     C*                  Z-ADD     XXDT          XXBGDT
      *** END ADD ***** 06/12/06 *************************************
     C*                  Z-ADD     0             XXINCT            2 0
     C*    XXDT          DOWLE     XXNTDT
     C*    XXDT          IFGE      XXBGDT
     C*    XXDT          ANDLT     XXNTDT
     C*                  ADD       1             XXINCT
     C*                  ENDIF
     C*                  ADD       1             XXMM
     C*    XXMM          IFEQ      13
     C*                  Z-ADD     1             XXMM
     C*                  ADD       1             XXYY
     C*                  ENDIF
     C*                  ENDDO
      *
      * Pass the values to the parm used for call
      *
     C*                  Z-ADD     XXINCT        XX15
     C*                  ENDIF
      *
     C*                  MOVE      XX2155        ##RRN
      *** END ADD ***** 04/03/06 **************************************
      *
     C*                  CALL      'W2155R'
     C*    *INKC         PARM                    ##KC              1
     C*    *INKL         PARM                    ##KL              1
     C*                  PARM      '2'           ##CALL            1
     C*                  PARM                    ##RRN             9 0
     C*
     C*    ##RRN         CHAIN     WTBSP                              60
     C*
     C*    *IN60         IFEQ      *OFF
     C*                  MOVEL     *BLANKS       SCBSTY
     C*                  MOVEL(P)  BSKEY         SCBSCD
     C*                  MOVE      *ON           *IN51
     *** BEG ADD *** 11/25/97 *****************************************
     C*                  MOVE      *ON           *IN36
     *** END ADD *** 11/25/97 *****************************************
     C*                  ELSE
     C*                  MOVE      *ON           *IN49
     *** BEG ADD *** 11/25/97 *****************************************
     C*                  MOVE      *ON           *IN35
     *** END ADD *** 11/25/97 *****************************************
     C*                  ENDIF                                                  *IN60=*OFF
     C*
     C*                  ELSE
     C*
     C*                  EXSR      BILAUX
      *** BEG ADD ***** 02/07/07 ***************************************
     C*                  MOVE      *BLANKS       SCBSTY
      *** END ADD ***** 02/07/07 ***************************************
     C*
     C*                  ENDIF                                                  SCBSTY=*BLANKS
     C*
     C*  BILLING SCHEDULE TYPE
     C*
     C*    HFLD          WHENEQ    'SCBSTY'
     C*
     C*                  EXSR      BILAUX
     C*
     C*                  ENDSL
     C*
     C*                  GOTO      STRT
     C*
     C*                  ENDIF                                                  *INKJ=*ON
     C*
     C*===============================================================
     C*  F12=CANCEL
     C*===============================================================
     C*
     C*    *INKL         IFEQ      *ON
     C*
     C*    Q2RECF        IFEQ      'SCR02'
     C*
     C*    SKIP01        IFEQ      *OFF
     C*                  MOVE      *ON           *IN40
     C*                  MOVE      *BLANKS       SCMSG
     C*                  MOVEL(P)  'SCR01'       Q2RECF
     C*                  ELSE
     C*                  GOTO      TERM
     C*                  ENDIF                                                  SKIP01=*OFF
     C*
     C*                  ENDIF                                                  Q2RECF=SCR02
     C*
     C*                  GOTO      STRT
     C*
     C*                  ENDIF                                                  *INKL=*ON
     C*
     C*===============================================================
     C*  F13=PROCESS CHANGE OF USER PARAMETERS
     C*===============================================================
     C*
     C*    *INKM         IFEQ      *ON
     C*
     C*                  CALL      'W0006R'
     C*
     C*                  MOVE      *BLANKS       SCMSG
     C*                  MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*                  GOTO      TOP
     C*
     C*                  ENDIF                                                  *INKM=*ON
     C*
     C*===============================================================
     C*  PROCESS HELP REQUESTS
     C*===============================================================
     C*
     C*    *IN10         IFEQ      *ON
     C*
     C*                  MOVEL(P)  Q2RECF        SVRECF
     C*                  MOVEL(P)  'QHLP'        FILNAM           10
     C*                  MOVEL(P)  '*LIBL'       LIBNAM           10
     C*                  MOVE      Q1PGM         MBRNAM
     C*                  MOVE      XXSCR#        MBRNAM           10
     C*
     C*                  CALL      'M0006C'
     C*                  PARM                    FILNAM
     C*                  PARM                    LIBNAM
     C*                  PARM                    MBRNAM
     C*
     C*                  MOVEL(P)  SVRECF        Q2RECF
     C*
     C*                  GOTO      STRT
     C*
     C*                  ENDIF                                                  *IN10=*ON
     C*
     C*===============================================================
     C*  PAGE DOWN REQUEST
     C*===============================================================
     C*
     C*    *IN18         CABEQ     *ON           SCR02A
     C*
     C*===============================================================
     C*  PAGE UP REQUEST
     C*===============================================================
     C*
     C*    *IN19         CABEQ     *ON           SCR02A
     C*
     C*                  ENDIF                                                  *IN11=*ON
     C*______________________________________________________________
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C*    Q2RECF        CABEQ     'SCR01'       SCR01T
     C*    Q2RECF        CABEQ     'SCR02'       SCR02T
     C*______________________________________________________________
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C*    SCR01T        TAG
     C*
     C*  VALIDATE EMPLOYER NUMBER
     C*
     C*                  Z-ADD     SCEMP#        XXEMP#
     C*                  Z-ADD     SCDIV         XXDIV
     C*                  Z-ADD     SCFYR         XXFYR
     C*
     C*    EMKY          CHAIN(N)  PEM01                              60
     C*
     C*    *IN60         CABEQ     *ON           STRT                     20
     C*
     C*  VALIDATE THE FUND YEAR ENTERED
     C*
     C*    ELKY          CHAIN(N)  PEL01                              60
     C*
     C*    *IN60         CABEQ     *ON           STRT                     22
     *** BEG ADD *** 11/25/97 ****************************************
      *
      *  DOES THE SELECTED EMPLOYER HAVE MORE THAN ONE POLICY YEAR
      *
      *  SAVE THE SELECTED POLICY YEAR
      *
     C*                  MOVE      XXFYR         ZZFYR
      *
     C*                  Z-ADD     *ZERO         X                 2 0
      *
     C*    ELKY1         CHAIN(N)  PEL01                              60
      *
     C*    *IN60         DOWEQ     *OFF
      *
     C*    ELKY1         READE(N)  PEL01                                  60
     C*                  ADD       1             X
      *
     C*                  ENDDO
      *
      *  IF X IS GREATER THAN 1 THEN DISPLAY INFORMATIONAL WINDOW
      *
     C*    X             IFGT      1
      *
     C*                  EXFMT     SCR03
      *
     C*                  ENDIF                                                  X>1
      *
      *  LOAD THE SAVED POLICY YEAR BACK INTO XXFYR
      *
     C*                  MOVE      ZZFYR         XXFYR
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*                  Z-ADD     SCFYR         SCNFYR
     C***  END   ADD  ***  11/10/99  *********************************
      *
     *** END ADD *** 11/25/97 ****************************************
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C*                  CALL      'WA005R'
     C*                  PARM      EMEMP#        XXEMP#
     C*                  PARM      EMDIV         XXDIV
     C*                  PARM      EMNAME        XXNAME
     C*                  PARM                    XXEDN            80
     C*
     C*                  MOVEL(P)  XXEDN         SCEDN
     C*
     C*                  Z-ADD     ELPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCPTDT
      **** BEGIN ADD **** 02/02/98 ************************************
     C*                  Z-ADD     ELBSDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCBSDT
      **** END   ADD **** 02/02/98 ************************************
     C*
     C*  COPY OLD VALUES INTO A HOLD FIELDS FOR LATER USE.
     C*
     *** BEG ADD *** 11/25/97 *****************************************
     C*    *IN97         IFEQ      *ON
     C*
     C*                  MOVE      ELPFDT        XXFOLD
     C*                  MOVE      ELPTDT        XXTOLD
     C*                  MOVE      *OFF          *IN97
     C*
     C*                  ENDIF                                                  *IN97=ON
     *** BEG ADD *** 11/25/97 *****************************************
     C*                  MOVEL     SRDTO         XXOFDT
     C*
     C*                  Z-ADD     ELPFDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCNEFD
     C*
     C*  COPY OLD VALUE INTO A HOLD FIELD FOR LATER USE.
     C*
     C*                  MOVEL     SRDTO         XXOTDT
     C*
     C*                  Z-ADD     ELANDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCNADT
     C*
     C*                  Z-ADD     ELEXM1        SCEXM1
     C*
     C*                  Z-ADD     ELEMD1        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCEMD1
     C*
     C*                  Z-ADD     ELEXM2        SCEXM2
     C*
     C*                  Z-ADD     ELEMD2        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCEMD2
     C*
     C*                  Z-ADD     ELEXM3        SCEXM3
     C*
     C*                  Z-ADD     ELEMD3        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCEMD3
     C*
     C*                  MOVE      ELBSCD        SCBSCD
     C***  BEGIN DEL  ***  11/10/99  *********************************
     C**********
     C***GET*GROUP DETAIL INFORMATION
     C**********
     C********** F1KY      CHAINPF101                60
     C***  END   DEL  ***  11/10/99  *********************************
     C*
     C*                  MOVE      *OFF          XXDOWN
     C*                  MOVE      *ON           XXUP
     C*_______________________________________________________________
     C*
     C*  LOAD SUBFILE
     C*
     C*    SCR02A        TAG
     C*
     C*                  Z-ADD     *ZERO         XXEFD1
     C*                  Z-ADD     *ZERO         XXCLAS
     C*
     C*  INVALID ROLL REQUEST
     C*
     C*  18XXDOWN        CABEQ     *ON           STRT                     20
     C*
     C*  19XXUP          CABEQ     *ON           STRT                     20
     C*
     C*===============================================================
     C*  ROLL UP REQUEST
     C*===============================================================
     C*
     C*    *IN18         IFEQ      *ON
     C*
     C*                  MOVE      *OFF          XXUP
     C*
     C*                  Z-ADD     KEYF(2)       SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXEFD1
     C*
     C*                  Z-ADD     KEYM(2)       XXCLAS
     C*
     C*    EPKY2         CHAIN     WMEPL                              60
     C*
     C*                  EXSR      LOAD
     C*
     C*                  GOTO      STRT
     C*
     C*                  ENDIF                                                  *IN18=*ON
     C*
     C*===============================================================
     C*  PAGE UP REQUEST
     C*===============================================================
     C*
     C*    *IN19         IFEQ      *ON
     C*
     C*                  MOVE      *OFF          XXDOWN
     C*
     C*                  Z-ADD     KEYF(1)       SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXEFD1
     C*
     C*                  Z-ADD     KEYM(1)       XXCLAS
     C*
     C*    EPKY2         CHAIN     WMEPL                              60
     C*
     C*                  EXSR      LOADP
     C*
     C*                  GOTO      STRT
     C*
     C*                  ENDIF                                                  *IN19=*ON
     C*
     C*===============================================================
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*===============================================================
     C*
     C*    EPKY1         CHAIN(N)  WMEPL                              60
     C*
     C*                  EXSR      LOAD
     C*
     C*    FNCGFL        IFEQ      'Y'
      ***  BEGIN ADD  ***  03/31/07 ***********************************
     C*    FNCGFL        OREQ      'N'
      ***  END ADD    ***  03/31/07 ***********************************
     C*                  MOVE      *ON           *IN99
     C*                  ELSE
     C*                  MOVE      *OFF          *IN99
     C*                  ENDIF                                                  FNCGFL=Y
     C*
     C*                  MOVEL(P)  'SCR02'       Q2RECF
     C*
     C*                  GOTO      STRT
     C*_______________________________________________________________
     C*
     C*    SCR02T        TAG
     C***  BEGIN CHG  ***  11/10/99  *********************************
     C**********
     C**********           Z-ADDSCNEFD    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXNEFD
     C**********
     C**********           Z-ADDSCNADT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXANDT
     C**********
     C**********           Z-ADDSCPTDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXNTOD
     C**********
     ****BEG*DEL *** 03/23/98 *****************************************
     C**********
     ****BEG*ADD *** 11/25/97 *****************************************
     C**********
     C***IF*NEW*POLICY DATES WERE ENTERED CLEAR BILLING SCHEDULE CODE
     C***AND*TYPE TO FORCE A SEARCH
     C**********
     C********** XXFOLD    IFNE XXNEFD
     C********** XXTOLD    ORNE XXNTOD
     C**********
     C**********           MOVE *BLANKS   SCBSCD
     C**********           MOVE *BLANKS   SCBSTY
     C**********           MOVE XXNEFD    XXFOLD
     C**********           MOVE XXNTOD    XXTOLD
     C**********
     C**********           ENDIF                           ELPFDT NE XXNEF
     C**********
     ****END*ADD *** 11/25/97 *****************************************
     C**********
     ****END*DEL *** 03/23/98 *****************************************
     C**********
     C***EDIT*NEW EFFECTIVE DATE
     C**********
     C**********           Z-ADDSCNEFD    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCNEFD
     C**********
     C********** SRERR     CABEQ*ON       STRT           21
     C**********
     C***THE*NEW EFFECTIVE DATE MUST FALL WITHIN THE GROUP POLICY
     C***START*AND ENDING DATES
     C**********
     C****BEGIN*CHG *** 10/20/99 ***********************************************
     C**********           MOVE *OFF      XXMAT   1
     C********** FNKY      CHAINPF101                61
     C********** *IN61     DOWEQ*OFF
     C**********
     C********** XXNEFD    IFGE F1STPO
     C********** XXNEFD    ANDLTF1ENPO
     C**********           MOVE *ON       XXMAT
     C**********           Z-ADDF1FYR     XXNFYR  30
     C**********           LEAVE
     C**********           ENDIF
     C**********
     C********** FNKY      READEPF101                    61
     C**********           ENDDO
     C**********
     C********** XXMAT     IFNE *ON
     C**********           MOVE *ON       *IN22
     C**********           GOTO STRT
     C**********           ENDIF                           F1STPO>XXNEFD>F1ENPO
     C****END***CHG *** 10/20/99 ***********************************************
     C**********
     C***EDIT*NEW EFFECTIVE DATE
     C**********
     C**********           Z-ADDSCPTDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCPTDT
     C**********
     C********** SRERR     CABEQ*ON       STRT           98
     C**********
     C***POLICY*END DATE CANNOT BE > POLICY END DATE OF THE STATE
     C****REPORTING DIVISION
     C**********
     C********** EMKY3     CHAINPEM01               N65
     C**********
     C********** *IN65     DOWEQ*OFF
     C**********
     C********** EMSTRP    IFEQ 'Y'
     C**********
     C********** EMEMP#    IFNE SCEMP#
     C********** EMDIV     ANDNESCDIV
     C**********
     C********** ELKY3     CHAINPEL01               N66
     C**********
     C********** XXNTOD    CABGTELPTDT    STRT       83
     C**********
     C********** XXNTOD    COMP ELPTDT                 89
     C**********
     C**********           ENDIF                           EMEMP#<>SCEMP#
     C**********
     C**********           ENDIF                           EMSTRP=Y
     C**********
     C********** EMKY3     READEPEM01               N    65
     C**********
     C**********           ENDDO                           *IN65=*OFF
     C**********
     C********** ELKY      CHAINPEL01               N66
     C**********
     C***TO*DATE MUST EQUAL CANCEL DATE IN EMPLOYER MASTER FOR EACH DIV
     C**********
     C********** *IN89     IFEQ *ON
     C**********
     C********** EMKY      CHAINPEM01               N65
     C**********
     C********** XXNTOD    IFNE EMCAD1
     C********** XXNTOD    ANDNEEMCAD2
     C********** XXNTOD    ANDNEEMCAD3
     C********** XXNTOD    ANDNEEMCAD4
     C********** XXNTOD    ANDNEEMCAD5
     C**********           MOVE *ON       *IN90
     C**********           GOTO STRT
     C**********           ENDIF                           XXNTOD<>EMCAD1
     C**********
     C**********           ENDIF                           *IN89=*ON
     C**********
     C***POLICY*END DATE CANNOT BE MORE THAN 365(366) DAYS PAST POLICY
     C***START*DATE
     C**********
     C**********           CALL 'SRDDIF8'
     C**********           PARM SCNEFD    XXPFDT
     C**********           PARM SCPTDT    XXPTDT
     C**********           PARM           XXDAYS  50
     C**********           PARM           XXER    1
     C**********           PARM '1'       XXTERM  1
     C**********
     C**********           MOVE *OFF      *IN88
     C**********
     C***CHECK*FOR LEAP YEAR
     C**********
     C****TO*DATE
     C**********
     C**********           Z-ADDXXNTOD    XXDT
     C**********
     C********** XXYY      DIV  4         XXLPYR  21
     C**********           MVR            XXREM   10
     C**********
     C********** XXREM     IFEQ *ZERO
     C********** XXMMDD    ANDGT0228
     C**********           MOVE *ON       *IN88
     C**********           ENDIF                           XXREM=*ZERO
     C**********
     C****FROM*DATE
     C**********
     C**********           Z-ADDXXNEFD    XXDT
     C**********
     C********** XXYY      DIV  4         XXLPYR
     C**********           MVR            XXREM
     C**********
     C********** XXREM     IFEQ *ZERO
     C********** XXMMDD    IFLT 0301
     C**********           MOVE *ON       *IN88
     C**********           ENDIF                           XXREM=*ZERO
     C**********
     C**********           END
     C**********
     C********** *IN88     IFEQ *OFF
     C********** XXDAYS    CABGT365       STRT       84
     C**********           ELSE
     C********** XXDAYS    CABGT366       STRT       84
     C**********           ENDIF                           *IN88=*OFF
     C**********
     C**THE*NEW*EFFECTIVE DATE CAN'T BE GREATER THAN THE POLICY TO DATE
     C**********
     ****BEG*CHG *** 11/25/97 *****************************************
     C********** XXNEFD    CABGTELPTDT    STRT       33
     C********** XXNEFD    CABGTXXNTOD    STRT       33
     ****BEG*CHG *** 11/25/97 *****************************************
     C**********
     C***EDIT*ANNIVERSARY RATING DATE
     C**********
     C********** SCNADT    IFNE *ZERO
     C**********
     C**********           Z-ADDSCNADT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCNADT
     C**********
     C********** SRERR     CABEQ*ON       STRT           23
     C**********
     C**********           ENDIF                           SCNADT<>*ZERO
     C**********
     C***ANNIV*DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C**********
     C********** XXANDT    CABGTXXNEFD    STRT       24
     C*****BEGIN ADD **** 02/02/98 ***************************************
     C**********
     C***EDIT*NEW BILLING START DATE
     C**********
     C**********           Z-ADDSCBSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCBSDT
     C**********
     C********** SRERR     CABEQ*ON       STRT           38
     C*****END** ADD **** 02/02/98 ***************************************
     C**********
     C***MOD*REQUIRED IF EFFECTIVE DATE ENTERED
     C**********
     C********** SCEXM1    IFGT *ZERO
     C********** SCEMD1    ANDEQ*ZERO
     C**********           MOVE *ON       *IN25
     C**********           GOTO STRT
     C**********           ENDIF                           SCEXM1>*ZERO
     C**********
     C***EDIT*EFFECTIVE DATE
     C**********
     C********** SCEMD1    IFNE *ZERO
     C**********
     C**********           Z-ADDSCEMD1    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCEMD1
     C**********
     C********** SRERR     CABEQ*ON       STRT           28
     C**********
     C***FIRST*EFFECT DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C**********
     C**********           Z-ADDSCEMD1    X1EMD1
     C**********
     C**********           Z-ADDX1EMD1    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXEMD1
     C**********
     C********** XXEMD1    CABGTXXNEFD    STRT       32
     C**********
     C**********           ENDIF                           SCEMD1<>*ZERO
     C**********
     C***MOD*REQUIRED IF EFFECTIVE DATE ENTERED
     C**********
     C********** SCEXM2    IFGT *ZERO
     C********** SCEMD2    ANDEQ*ZERO
     C**********           MOVE *ON       *IN26
     C**********           GOTO STRT
     C**********           ENDIF                           SCEXM2>*ZERO
     C**********
     C***EDIT*EFFECTIVE DATE
     C**********
     C********** SCEMD2    IFNE *ZERO
     C**********
     C**********           Z-ADDSCEMD2    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCEMD2
     C**********
     C********** SRERR     CABEQ*ON       STRT           30
     C**********
     C**********           Z-ADDSCEMD1    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDAT1
     C**********
     C**********           Z-ADDSCEMD2    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDAT2
     C**********
     C********** XXDAT2    CABLTXXDAT1    STRT         29
     C**********
     C**********           ENDIF                           SCEMD2<>*ZERO
     C**********
     C***MOD*REQUIRED IF EFFECTIVE DATE ENTERED
     C**********
     C********** SCEXM3    IFGT *ZERO
     C********** SCEMD3    ANDEQ*ZERO
     C**********           MOVE *ON       *IN27
     C**********           GOTO STRT
     C**********           ENDIF                           SCEXM3>*ZERO
     C**********
     C***EDIT*EFFECTIVE DATE
     C**********
     C********** SCEMD3    IFNE *ZERO
     C**********
     C**********           Z-ADDSCEMD3    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCEMD3
     C**********
     C********** SRERR     CABEQ*ON       STRT           31
     C**********
     C**********           Z-ADDSCEMD2    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDAT1
     C**********
     C**********           Z-ADDSCEMD3    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDAT2
     C**********
     C********** XXDAT2    CABLTXXDAT1    STRT         29
     C**********
     C**********           ENDIF                           SCEMD2<>*ZERO
     C**********
     C********** SCBSCD    IFEQ *BLANKS
     C********** SCBSCD    OREQ *ZEROS
     C**********           EXSR BILAUX
     C**********           ENDIF                           SCBSCD=*BLANKS
     C**********
     C***VALIDATE THE BILLING SCHEDULE CODE
     C**********
     C**********           MOVELSCBSCD    XXKEY     P
     C**********
     C********** TBKY      CHAINPBS01                60
     C**********
     C********** *IN60     CABEQ*ON       STRT           34
     C********** BSSTAT    CABNE'A'       STRT       3434
     C**********           END
     C******BEG*ADD ******* 07/23/99 **********************************
     C**********
     C***'Update all divisions' must be (Y/ )
     C**********
     C********** SCUPDD    IFNE 'Y'
     C********** SCUPDD    ANDNE' '
     C**********           SETON                     71
     C**********           ENDIF                           SCUPPD
     C******END*ADD ******* 07/23/99 **********************************
     C*
     C*** BEG DEL ***** 07/15/11 **************************************
     C***********          EXSR ERRCK2
     C***********
     C************IN90     CABEQ*ON       STRT
     C*****END***CHG  ***  11/10/99  *********************************
     C***********
     C***ALL*EDITS HAVE BEEN PASSED
     C***********
     C*** END DEL ***** 07/15/11 **************************************
      ***** BEG ADD ******* 07/23/99 **********************************
      *
      *** BEG CHG ***** 07/15/11 *************************************
     C***********SCUPDD    IFNE 'Y'
      ******END*ADD ******* 07/23/99 **********************************
     C***********EMKY      CHAINPEM01                60
      ******BEG*ADD ******* 07/23/99 **********************************
     C***********          ELSE
     C***********EMKY1     CHAINPEM01                60
     C***********          ENDIF                           SCUPDD
     C****BEG*DEL ***** 05/09/08 *************************************
     C***********          Z-ADDEMDIV     XXDIV
     C***********ELKY3     CHAINPEL01                60
     C****END*DEL ***** 05/09/08 *************************************
     C***********          Z-ADDXXDIV     SVDIV
      *** BEG CHG **** 05/09/08 ***************************************
     C***********NXTDIV    TAG
     C************IN60     DOWEQ*OFF
     C****END*CHG **** 05/09/08 ***************************************
     C***********
     C*****BEGIN*ADD  ***  11/10/99  *********************************
     C***********          TIME           DSDTTM
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     X7DATE
     C*****END***ADD  ***  11/10/99  *********************************
     C***********          Z-ADDEMDIV     XXDIV
      *
     C*                  Z-ADD     SCDIV         SVDIV
     C*                  Z-ADD     SCDIV         XXDIV
     C*    SCUPDD        IFNE      'Y'
     C*    EMKY          CHAIN(N)  PEM01                              60
     C*                  ELSE
     C*    EMKY1         CHAIN(N)  PEM01                              60
     C*                  ENDIF                                                  SCUPDD
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     EMDIV         XXDIV
     C*                  EXSR      ERRCK2
     C*    SCUPDD        IFNE      'Y'
     C*                  LEAVE
     C*                  ELSE
     C*    EMKY1         READE(N)  PEM01                                  60
     C*                  ENDIF                                                  SCUPDD
     C*                  ENDDO
      *
      * ALL EDITS PASSED, PROCEED WITH UPDATE
      *
     C*                  Z-ADD     SVDIV         XXDIV
     C*    SCUPDD        IFNE      'Y'
     C*    EMKY          CHAIN     PEM01                              60
     C*                  ELSE
     C*    EMKY1         CHAIN     PEM01                              60
     C*                  ENDIF                                                  SCUPDD
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     EMDIV         XXDIV
     C*    ELKY3         CHAIN     PEL01                              60
     C*    *IN60         IFEQ      *ON
     C*                  UPDATE    PEM01
     C*    EMKY1         READE     PEM01                                  60
     C*                  ITER
     C*                  ENDIF
      *** END CHG ***** 07/15/11 *************************************
     C*** BEG DEL ***** 05/09/08 *************************************
     C***********ELKY3     CHAINPEL01                60
     C*** END DEL ***** 05/09/08 *************************************
      *
     C*    ELDIV         IFEQ      SVDIV
     C*                  SETOFF                                       72
     C*                  ELSE
     C*                  SETON                                        72
     C*                  ENDIF
      ***** END ADD ******* 07/23/99 **********************************
      ***** BEG DEL ******* 07/23/99 **********************************
     C*********  ELKY      CHAINPEL01                60
      ***** END DEL ******* 07/23/99 **********************************
     C*
     C*  IF THE ANNIVERSARY RATING DATE IS CHANGED, UPDATE ALL
     C*  PAYROLL CLASS RECORDS HAVING OLD ANNIVERSARY RATING DATE
     C*  TO NEW ANNIVERSARY RATING DATE
     C*
     *** BEG ADD *** 11/25/97 *****************************************
     C*                  MOVE      ELPFDT        XXOEDT
     *** END ADD *** 11/25/97 *****************************************
      ***** BEG ADD ******* 04/29/02 **********************************
     C*                  MOVE      ELPTDT        XXOXDT
      ***** END ADD ******* 04/29/02 **********************************
     C*
     C*    ELANDT        IFNE      XXANDT
     C*    SCNADT        ANDNE     *ZEROS
     C*
     C*    ELANDT        ADD       10000         XXEFDT
     C*    EPKY3         CHAIN     PEP01                              60
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     SCNADT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         EPEFDT
     C*                  ADD       10000         EPEFDT
      *** BEGIN ADD *** 10/20/99 ***********************************************
     C*                  Z-ADD     XXNFYR        EPFYR
      *** END   ADD *** 10/20/99 ***********************************************
     C*** BEGIN ADD ***  08/20/10  ******************************************
     C*                  MOVE      EPCLAS        XXCLAS
     C*                  MOVE      EPSPEC        XXSPEC
     C*                  Z-ADD     EPEFDT        SVEFDT            7 0
     C*                  MOVE      EPEXT         SVEXT
     C*                  Z-ADD     XXNFYR        SVFYR             3 0
     C*                  Z-ADD     EPEAPR        SVEAPR           11 2
     C*                  Z-ADD     EPEMPR        SVEMPR           11 2
     C*                  Z-ADD     EPECNT        SVECNT            7 0
     C*                  Z-ADD     EPADLD        SVADLD           11 2
     C*                  Z-ADD     EPSAPR        SVSAPR           11 2
     C*    EPKY4         CHAIN     PEP02                              50
     C*    *IN50         IFEQ      *OFF
     C*                  ADD       SVEAPR        EPEAPR
     C*                  ADD       SVEMPR        EPEMPR
     C*                  ADD       SVECNT        EPECNT
     C*                  ADD       SVADLD        EPADLD
     C*                  ADD       SVSAPR        EPSAPR
     C*                  DELETE    PEP02
     C*                  ENDIF
     C*** END ADD *****  08/20/10  ******************************************
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*                  ADD       1             DSTIME
     C*                  MOVEL(P)  Q1PGM         EPPGM
     C*                  MOVEL(P)  Q1USER        EPUSER
     C*                  Z-ADD     X7DATE        EPDT
     C*                  Z-ADD     DSTIME        EPTIME
     C***  END   ADD  ***  11/10/99  *********************************
     C*                  UPDATE    PEP01
     C*    EPKY3         READE     PEP01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     C*
     C*                  ENDIF                                                  ELANDT<>XXANDT
     C*
     C*  IF THE EFFECTIVE DATE IS CHANGED:
     C*    1. UPDATE ALL PAYROLL CLASS RECORDS
     C*    2. UPDATE AGENT HISTORY RECORD
     C*    3. UPDATE AGENT QUOTE RECORD
     C*
     C*    ELPFDT        IFNE      XXNEFD
     C*
     C*  1. EMPLOYER PAYROLL FILE
     C*
     C*                  MOVE      ELPFDT        XXEFDT
     C*
     C*    EPKY3         CHAIN     PEP01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         EPEFDT
      *** BEGIN ADD *** 10/20/99 ***********************************************
     C*                  Z-ADD     XXNFYR        EPFYR
      *** END   ADD *** 10/20/99 ***********************************************
     C*** BEGIN ADD ***  08/20/10  ******************************************
     C*                  MOVE      EPCLAS        XXCLAS
     C*                  MOVE      EPSPEC        XXSPEC
     C*                  Z-ADD     EPEFDT        SVEFDT            7 0
     C*                  MOVE      EPEXT         SVEXT
     C*                  Z-ADD     XXNFYR        SVFYR             3 0
     C*                  Z-ADD     EPEAPR        SVEAPR           11 2
     C*                  Z-ADD     EPEMPR        SVEMPR           11 2
     C*                  Z-ADD     EPECNT        SVECNT            7 0
     C*                  Z-ADD     EPADLD        SVADLD           11 2
     C*                  Z-ADD     EPSAPR        SVSAPR           11 2
     C*    EPKY4         CHAIN     PEP02                              50
     C*    *IN50         IFEQ      *OFF
     C*                  ADD       SVEAPR        EPEAPR
     C*                  ADD       SVEMPR        EPEMPR
     C*                  ADD       SVECNT        EPECNT
     C*                  ADD       SVADLD        EPADLD
     C*                  ADD       SVSAPR        EPSAPR
     C*                  DELETE    PEP02
     C*                  ENDIF
     C*** END ADD *****  08/20/10  ******************************************
     C*
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*                  ADD       1             DSTIME
     C*
     C*                  MOVEL(P)  Q1PGM         EPPGM
     C*                  MOVEL(P)  Q1USER        EPUSER
     C*                  Z-ADD     X7DATE        EPDT
     C*                  Z-ADD     DSTIME        EPTIME
     C***  END   ADD  ***  11/10/99  *********************************
     C*
     C*                  UPDATE    PEP01
     C*    EPKY3         READE     PEP01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     C*
     C*  2. AGENT HISTORY FILE
     C*
     C*    AHKY1         CHAIN     PAH01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*    AHSTAT        ANDEQ     'I'
     C*    AHKY1         READE     PAH01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     C*
     C*    *IN60         IFEQ      *OFF
     C*
     *** BEG DEL *** 11/25/97 *****************************************
     C******************** Z-ADDSCNEFD    SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     AHEFDT
     C********************
     C******************** UPDATPAH01
     *** BEG DEL *** 11/25/97 *****************************************
     C*
     C*  3. AGENT QUOTE FILE
     C*
     C*                  MOVE      'Q'           XXSTAT
     C*                  Z-ADD     AHAGT#        XXAGT#
     C*
     C*    QTKY          CHAIN     PQT01                              60
     C*
     C*    *IN60         IFEQ      *OFF
     C*
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         QTEFDT
     C*
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*                  MOVEL(P)  Q1PGM         QTPGM
     C*                  MOVEL(P)  Q1USER        QTUSER
     C*                  Z-ADD     X7DATE        QTDT
     C*                  Z-ADD     DSTIME        QTTIME
     C***  END   ADD  ***  11/10/99  *********************************
     C*
     C*                  UPDATE    PQT01
     C*
     C*                  ENDIF                                                  *IN60=*OFF
     C*
     C*                  ENDIF                                                  *IN60=*OFF
     C*
     C*                  ENDIF                                                  ELPFDT<>XXNEFD
     C*
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         EMEFD1
     C*
     C*                  Z-ADD     *ZEROS        EMEFD2
     C*                  Z-ADD     *ZEROS        EMEFD3
     C*                  Z-ADD     *ZEROS        EMEFD4
     C*                  Z-ADD     *ZEROS        EMEFD5
     C*                  Z-ADD     *ZEROS        EMCAD1
     C*                  Z-ADD     *ZEROS        EMCAD2
     C*                  Z-ADD     *ZEROS        EMCAD3
     C*                  Z-ADD     *ZEROS        EMCAD4
     C*                  Z-ADD     *ZEROS        EMCAD5
     C*
     C*                  MOVE      Q1USER        EMUSER
     C*                  MOVE      Q1PGM         EMPGM
     C***  BEGIN CHG  ***  11/10/99  *********************************
     C**********
     C**********           TIME           DSDTTM
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EMDT
     C**********
     C*                  Z-ADD     X7DATE        EMDT
     C***  END   CHG  ***  11/10/99  *********************************
     C*                  Z-ADD     DSTIME        EMTIME
     C*
     C*                  UPDATE    PEM01
     C*
      ***** BEG CHG ******* 07/23/99 **********************************
     C***************      Z-ADDSCEXM1    ELEXM1
     C* N72              Z-ADD     SCEXM1        ELEXM1
      ***** BEG CHG ******* 07/23/99 **********************************
     C*
     C*                  Z-ADD     SCEMD1        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         ELEMD1
     C*
      ***** BEG CHG ******* 07/23/99 **********************************
     C*****************    Z-ADDSCEXM2    ELEXM2
     C* N72              Z-ADD     SCEXM2        ELEXM2
      ***** BEG CHG ******* 07/23/99 **********************************
     C*
     C*                  Z-ADD     SCEMD2        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         ELEMD2
     C*
      ***** BEG CHG ******* 07/23/99 **********************************
     C*****************    Z-ADDSCEXM3    ELEXM3
     C* N72              Z-ADD     SCEXM3        ELEXM3
      ***** BEG CHG ******* 07/23/99 **********************************
     C*
     C*                  Z-ADD     SCEMD3        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         ELEMD3
     C*
     C***  BEG   ADD  ***  06/21/10  *********************************
     C*                  Z-ADD     SCNADT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         @@ANDT
     C*
     C*    @@ANDT        IFNE      ELANDT
     C*                  Z-ADD     0             ELD1D2
     C*                  Z-ADD     0             ELD1X2
     C*                  Z-ADD     0             ELD2D2
     C*                  Z-ADD     0             ELD2X2
     C*                  ENDIF
     C***  END   ADD  ***  06/21/10  *********************************
     C*                  Z-ADD     SCNADT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         ELANDT
     C*
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         ELPFDT
     C*
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         ELPTDT
      **** BEGIN ADD **** 02/02/98 ************************************
     C*                  Z-ADD     SCBSDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         ELBSDT
      **** END   ADD **** 02/02/98 ************************************
     C*
     *** BEG ADD *** 11/25/97 *****************************************
     C*
     C*  UPDATE EFFECTIVE DATES FOR ALL CREDIT FIELDS
     C*
     C*                  EXSR      SRCRED
     *** END ADD *** 11/25/97 *****************************************
     C*
     C*                  MOVE      SCBSCD        ELBSCD
     C*
     C*                  MOVE      Q1USER        ELUSER
     C*                  MOVE      Q1PGM         ELPGM
     C***  BEGIN CHG  ***  11/10/99  *********************************
     C**********
     C**********           TIME           DSDTTM
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     ELDT
     C**********
     C*                  Z-ADD     X7DATE        ELDT
     C***  END   CHG  ***  11/10/99  *********************************
     C*                  Z-ADD     DSTIME        ELTIME
     C*
      *** BEGIN ADD *** 10/20/99 ***********************************************
     C*                  Z-ADD     XXNFYR        ELFYR
      *** END   ADD *** 10/20/99 ***********************************************
     C*
     C*                  UPDATE    PEL01
      ***** BEG ADD ******* 07/23/99 **********************************
      ***** BEG ADD ******* 02/20/02 **********************************
      *** BEG DEL ***** 05/09/08 **************************************
     C***********SCNFYR    IFNE SCFYR
      *** END DEL ***** 05/09/08 **************************************
      *** BEG CHG ***** 07/15/11 **************************************
     C***********AHKY2     CHAINPAH01                60
     C************IN60     DOWEQ*OFF
      ****BEG*ADD ***** 05/09/08 **************************************
      ****BEG*CHG ***** 01/25/11 **************************************
     C***********          Z-ADDELPFDT    AHEFDT
      ****BEG*CHG ***** 04/13/11 **************************************
     C***********          Z-ADDAGTEFD    AHEFDT
     C***********AGTEFD    IFNE *ZERO
     C***********          Z-ADDAGTEFD    AHEFDT
     C***********          ENDIF
      ****END*CHG ***** 04/13/11 **************************************
      ****END*CHG ***** 01/25/11 **************************************
      ****END*ADD ***** 05/09/08 **************************************
     C***********          Z-ADDSCNFYR    AHFYR
     C***********          UPDATPAH01
     C***********AHKY2     READEPAH01                    60
     C***********          ENDDO                           *IN60=*OFF
      *
      * UPDATE AGENT HISTORY RECORD WITH AHEFDT = SAVED AHEFDT FROM
      * AHEFDT EDIT. UPDATE ALL RECORDS WITH NEW YEAR.
      *
     C*    SCNFYR        IFNE      SCFYR
     C*    ADCFLG        OREQ      *ON
     C*    AHKY1         SETLL     PAH01
     C*    AHKY1         READE     PAH01                                  60
     C*    *IN60         DOWEQ     *OFF
     C*    ADCFLG        IFEQ      *ON
     C*    AHAGT#        ANDEQ     SVAGT#
     C*    AHEFDT        ANDEQ     SVAEDT
     C*    AHSTAT        ANDEQ     'A'
     C*                  Z-ADD     AGTEFD        AHEFDT
     C*                  ENDIF
     C*    SCNFYR        IFNE      SCFYR
     C*                  Z-ADD     SCNFYR        AHFYR
     C*                  ENDIF
     C*                  UPDATE    PAH01
     C*    AHKY1         READE     PAH01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     C*                  ENDIF
      *** END CHG ***** 07/15/11 **************************************
      *
      ***** BEG ADD ******* 03/19/04 **********************************
      *
      *  Update Billing Transaction records
      *
     C*    BLKY          CHAIN     PBL01                              60
      *
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     SCNFYR        BLFYR
     C*                  Z-ADD     ELPFDT        BLPFDT
     C*                  MOVE      Q1USER        BLUSER
     C*                  MOVE      Q1PGM         BLPGM
     C*                  Z-ADD     X7DATE        BLDT
     C*                  Z-ADD     DSTIME        BLTIME
     C*                  UPDATE    PBL01
     C*    BLKY          READE     PBL01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
      *
      ***** END ADD ******* 03/19/04 **********************************
      *** BEG DEL ***** 05/09/08 **************************************
     C***********          ENDIF                           SCNFYR
      *** END DEL ***** 05/09/08 **************************************
      ***** END ADD ******* 02/20/02 **********************************
     C*** begin add ***  12/02/12  ***************************************
     c*
     c* Update Additional Location Effective Date.
     c*
     C*    ALKY          CHAIN     PAL01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     ELPFDT        ALEFDT
     C*                  MOVE      Q1USER        ALUSER
     C*                  MOVE      Q1PGM         ALPGM
     C*                  Z-ADD     X7DATE        ALDT
     C*                  Z-ADD     DSTIME        ALTIME
     C*                  UPDATE    PAL01
     C*    ALKY          READE     PAL01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     c*
     c* Update Additional Named Insured Effective Date.
     c*
     C*    ANKY          CHAIN     PAN01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     ELPFDT        ANEFDT
     C*                  MOVE      Q1USER        ANUSER
     C*                  MOVE      Q1PGM         ANPGM
     C*                  Z-ADD     X7DATE        ANDT
     C*                  Z-ADD     DSTIME        ANTIME
     C*                  UPDATE    PAN01
     C*    ANKY          READE     PAN01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     c*
     c* Update Owners/Officers Effective Date.
     c*
     C*    OOKY          CHAIN     POO01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     ELPFDT        OOEFDT
     C*                  MOVE      Q1USER        OOUSER
     C*                  MOVE      Q1PGM         OOPGM
     C*                  Z-ADD     X7DATE        OODT
     C*                  Z-ADD     DSTIME        OOTIME
     C*                  UPDATE    POO01
     C*    OOKY          READE     POO01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     c*
     c* Update Direct Debit Effective Date.
     c*
     C*    DBKY          CHAIN     PDB01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     ELPFDT        DBEFDT
     C*                  MOVE      Q1USER        DBUSER
     C*                  MOVE      Q1PGM         DBPGM
     C*                  Z-ADD     X7DATE        DBDT
     C*                  Z-ADD     DSTIME        DBTIME
     C*                  UPDATE    PDB01
     C*    DBKY          READE     PDB01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     c*
     c* Update Vendor History Efective Date and Year.
     c*
     C*    VHKY          CHAIN     PVH01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     SCNFYR        VHFYR
     C*                  Z-ADD     ELPFDT        VHEFDT
     C*                  MOVE      Q1USER        VHUSER
     C*                  MOVE      Q1PGM         VHPGM
     C*                  Z-ADD     X7DATE        VHDT
     C*                  Z-ADD     DSTIME        VHTIME
     C*                  UPDATE    PVH01
     C*    VHKY          READE     PVH01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     c*
     c* Update Location Year Detail Year.
     c*
     C*    LDKY          CHAIN     PLD01                              60
     C*
     C*    *IN60         DOWEQ     *OFF
     C*                  Z-ADD     SCNFYR        LDFYR
     C*                  MOVE      Q1USER        LDUSER
     C*                  MOVE      Q1PGM         LDPGM
     C*                  Z-ADD     X7DATE        LDDT
     C*                  Z-ADD     DSTIME        LDTIME
     C*                  UPDATE    PLD01
     C*    LDKY          READE     PLD01                                  60
     C*                  ENDDO                                                  *IN60=*OFF
     C*** end add *****  12/02/12  ***************************************
      *** BEG ADD ***** 05/09/08 **************************************
     C*** begin del ***  12/02/12  ***************************************
     C****/EXEC SQL
     C****+ UPDATE WDVHP SET VHEFDT = :ELPFDT, VHFYR = :SCNFYR
     C****+   WHERE VHCO#  = :XXCO# AND
     C****+         VHGRP  = :XXFND AND
     C****+         VHEMP# = :XXEMP# AND
     C****+         VHDIV  = :XXDIV
     C****/END-EXEC
     C****/EXEC SQL
     C****+ UPDATE WMDBP SET DBEFDT = :ELPFDT
     C****+   WHERE DBCO#  = :XXCO# AND
     C****+         DBFND  = :XXFND AND
     C****+         DBEMP# = :XXEMP# AND
     C****+         DBDIV  = :XXDIV
     C****/END-EXEC
     C****/EXEC SQL
     C****+ UPDATE WMOOP SET OOEFDT = :ELPFDT
     C****+   WHERE OOCO#  = :XXCO# AND
     C****+         OOFND  = :XXFND AND
     C****+         OOEMP# = :XXEMP# AND
     C****+         OODIV  = :XXDIV
     C****/END-EXEC
     C****/EXEC SQL
     C****+ UPDATE WMALP SET ALEFDT = :ELPFDT
     C****+   WHERE ALCO#  = :XXCO# AND
     C****+         ALFND  = :XXFND AND
     C****+         ALEMP# = :XXEMP# AND
     C****+         ALDIV  = :XXDIV
     C****/END-EXEC
     C****/EXEC SQL
     C****+ UPDATE WDLDP SET LDFYR = :SCNFYR
     C****+   WHERE LDCO#  = :XXCO# AND
     C****+         LDFND  = :XXFND AND
     C****+         LDEMP# = :XXEMP# AND
     C****+         LDDIV  = :XXDIV
     C****/END-EXEC
     C****/EXEC SQL
     C****+ UPDATE WMANP SET ANEFDT = :ELPFDT
     C****+   WHERE ANCO#  = :XXCO# AND
     C****+         ANFND  = :XXFND AND
     C****+         ANEMP# = :XXEMP# AND
     C****+         ANDIV  = :XXDIV
     C****/END-EXEC
     C*** end del *****  12/02/12  ***************************************
      *** END ADD ***** 05/09/08 **************************************
      *
      *  Update all divisions as necessary
      *
      *** BEG CHG ***** 07/15/11 *************************************
     C***********SCUPDD    IFEQ 'Y'
     C***********EMKY1     READEPEM01                    60
      ******BEG*ADD ******* 07/11/00 **********************************
     C**N60******ELKY3     CHAINPEL01                60
      ******END*ADD ******* 07/11/00 **********************************
      ****BEG*CHG ***** 05/09/08 **************************************
     C**N60******          GOTO NXTDIV
     C***********          ELSE
     C***********          MOVE *ON       *IN60
      ****END*CHG ***** 05/09/08 **************************************
     C***********          ENDIF                           *IN60
      ***********
      ******END*ADD ******* 07/23/99 **********************************
      ****BEG*ADD ***** 05/09/08 **************************************
     C***********          ENDDO
      *** END ADD ***** 05/09/08 **************************************
      *
     C*    SCUPDD        IFNE      'Y'
     C*                  LEAVE
     C*                  ELSE
     C*    EMKY1         READE     PEM01                                  60
     C*                  ENDIF                                                  SCUPDD
      *
     C*                  ENDDO
      *** END CHG ***** 07/15/11 *************************************
     C*
     C*                  MOVE      *ON           *IN40
     C*                  MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*    SKIP01        IFEQ      *OFF
     C*                  GOTO      STRT
     C*                  ENDIF                                                  SKIP01=*OFF
     C*
     C*  END THE PROGRAM
     C*
     C*    TERM          TAG
     C*
     C*    Q1PRMS        IFGE      3
     C*                  MOVE      *INKC         @@KC
     C*                  MOVE      *INKL         @@KL
     C*                  ENDIF                                                  Q1PRMS>=3
     C*
     C*                  MOVE      *ON           *INLR
     C*
     C*===============================================================
     C*  CLEAR SCREEN ARRAYS FOR PAYROLL SEARCH
     C*===============================================================
     C*
     C*    CLR           BEGSR
     C*
     C*                  Z-ADD     *ZEROS        DT
     C*                  MOVE      *BLANKS       CL
     C*                  MOVE      *BLANKS       SS
     C*                  MOVE      *BLANKS       TP
     C*                  Z-ADD     *ZEROS        AM
     C*                  Z-ADD     *ZERO         I1                2 0
     C*                  Z-ADD     *ZERO         I                 2 0
     C*
     C*                  ENDSR                                                  CLR
     C*
     C*===============================================================
     C*  FILL SCREEN ARRAYS
     C*===============================================================
     C*
     C*    FILL          BEGSR
     C*
     C*                  Z-ADD     EPEFDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         DT(I1)
     C*
     C*                  MOVE      EPCLAS        XXCLS
     C*                  MOVEA     XXCLS         X0
     C*
     C*                  SETOFF                                       60
     C*
     C*  PROCESS EACH ELEMENT IN THE ARRAY X0.
     C*
     C*    1             DO        5             I
     C*
     C*  IF THE ELEMENT IS ZERO MOVE IN A BLANK
     C*
     C* N60X0(I)         IFEQ      '0'
     C*                  MOVE      ' '           XB(I)
     C*                  ENDIF                                                  X0,I='O'
     C*
     C*  IF THE ELEMENT IS NOT A ZERO STOP THE ZERO CHECK(60)
     C*
     C*    X0(I)         IFNE      '0'
     C*    X0(I)         OREQ      '0'
     C*    *IN60         ANDEQ     '1'
     C*                  MOVE      X0(I)         XB(I)
     C*                  SETON                                        60
     C*                  ENDIF                                                  X0,I<>'0'
     C*
     C*                  ENDDO                                                  I->5
     C*
     C*                  MOVEA     XB            XXCLS             5
     C*                  MOVEL     XXCLS         XXCLSP            6
     C*                  MOVE      EPSPEC        XXCLSP
     C*                  MOVEL     XXCLSP        CL(I1)
     C*                  MOVEL     EPSTAT        SS(I1)
     C*                  MOVEL     EPSAFL        TP(I1)
     C*
     C*    EPSAFL        IFEQ      'A'
     C*                  Z-ADD     EPAAPR        AM(I1)
     C*                  ELSE
     C*                  Z-ADD     EPEAPR        AM(I1)
     C*                  ENDIF                                                  EPSAFL=A
     C*
     C*                  ENDSR                                                  FILL
     C*
     C*===============================================================
     C*  LOAD FORWARD SUBROUTINE
     C*===============================================================
     C*
     C*    LOAD          BEGSR
     C*
     C*                  EXSR      CLR
     C*
     C*    *IN60         DOWEQ     *OFF
     C*    I1            ANDLT     10
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*                  ADD       1             I1
     C*
     C*                  EXSR      FILL
     C*
     C*    EPKY1         READE(N)  WMEPL                                  60
     C*
     C*                  ENDDO                                                  *IN60=*OFF
     C*
     C*  END OF FILE
     C*
     C*  60              MOVE      *ON           XXDOWN
     C*
     C*                  Z-ADD     DT(1)         KEYF(1)
     C*                  Z-ADD     DT(10)        KEYF(2)
     C*                  MOVEL     CL(1)         KEYM(1)
     C*                  MOVEL     CL(10)        KEYM(2)
     C*
     C*                  ENDSR                                                  LOAD
     C*
     C*===============================================================
     C*  LOAD BACKWARD SUBROUTINE
     C*===============================================================
     C*
     C*    LOADP         BEGSR
     C*
     C*                  EXSR      CLR
     C*
     C*                  Z-ADD     11            I1
     C*
     C*    *IN60         DOWEQ     *OFF
     C*    I1            ANDGT     1
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*                  SUB       1             I1
     C*
     C*                  EXSR      FILL
     C*
     C*    EPKY1         READPE(N) WMEPL                                  60
     C*
     C*                  ENDDO                                                  *IN60=*OFF
     C*
     C*  BEGINNING OF FILE
     C*
     C*    I1            IFGT      1
     C*                  EXSR      LOADB
     C*                  ENDIF                                                  I1>1
     C*
     C*  60              MOVE      *ON           XXUP
     C*
     C*                  Z-ADD     DT(1)         KEYF(1)
     C*                  Z-ADD     DT(10)        KEYF(2)
     C*                  MOVEL     CL(1)         KEYM(1)
     C*                  MOVEL     CL(10)        KEYM(2)
     C*
     C*                  ENDSR                                                  LOADP
     C*
     C*===============================================================
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*===============================================================
     C*
     C*    LOADB         BEGSR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C*                  MOVE      *ON           XXUP              1
     C*                  MOVE      *OFF          XXDOWN            1
     C*
     C*    EPKY1         CHAIN     WMEPL                              62
     C*
     C*                  EXSR      LOAD
     C*
     C*                  ENDSR                                                  LOADB
     C*
     C*===============================================================
     C* BILAUX - SUBROUTINE FOR GETTING THE BILLING AUXILLARY CODE
     C*===============================================================
     C*
     C*    BILAUX        BEGSR
     C*
     C*  VALIDATE THE BILLING SCHEDULE TYPE IF ENTERED
     C*
     C*    SCBSTY        IFNE      *BLANKS
     C*    SCBSTY        ANDNE     'Q'
     C*    SCBSTY        ANDNE     'M'
     C*    SCBSTY        ANDNE     'A'
     C*    SCBSTY        ANDNE     'O'
     C*
     C*                  MOVE      *ON           *IN37
     C*                  GOTO      STRT
     C*
     C*                  ELSE
     C*
      *** BEG DEL ***** 03/28/06 ************************************
     C***********          Z-ADDSCNEFD    XXPF
     C***********
     C***********FNCGFL    IFEQ 'Y'
     C***********
     C***********          CALL 'SRDDIF8'
     C***********          PARM           SCPTDT
     C***********          PARM           SCNEFD
     C***********          PARM           QQDIFF  50
     C***********          PARM           QQERR
     C***********
     C***********QQDIFF    IFGT 364
     C***********
     C***********          Z-ADDF1STPO    SRDTI
     C***********          CALL 'SRCVTDAT'DTMDYY
     C***********          MOVELSRDTO     XXWK80
     C***********
     C***********          Z-ADDSCNEFD    XXPFD
     C***********          MOVE XXPDD     XXWDD
     C***********          MOVE XXWK80    XXPF
     C***********
     C***********          ELSE
     C***********
     C***********          Z-ADD*ZEROS    PDAYS
     C***********          Z-ADD*ZEROS    BDAYS
     C***********
     C***********365       SUB  QQDIFF    PDAYS   50
     C***********
     C***********          Z-ADDF1STPO    SRDTI
     C***********          CALL 'SRCVTDAT'DTMDYY
     C***********          MOVELSRDTO     XXEF
     C***********
     C***********          CALL 'SRFUTR8'
     C***********          PARM           XXEF
     C***********          PARM           PDAYS
     C***********          PARM           QQERR   1
     C***********          PARM '1'       QQTERM  1
     C***********
     C***********          MOVE XXEF      BDAYS
     C***********          Z-ADDSCNEFD    XXPFD
     C***********          MOVE XXPDD     XXBDD
     C***********
     C***********          MOVE BDAYS     XXPF
     C***********
     C***********          ENDIF                           QQDIFF>364
     C***********
     C***********          ENDIF                           FNCGFL=Y
     C***********
     C***********          MOVE XXPF      QQPFDT  8
      *** END DEL ***** 03/28/06 ************************************
      *** BEG ADD ***** 02/28/06 **************************************
      *
      *  GET THE NUMBER OF POSSIBLE MONTHLY INSTALLMENTS FOR THE POLICY WHICH
      *  CURRENTLY BILLS THE FIRST OF THE MONTH. SUBTRACT THIS NUMBER FROM
      *  12 TO GET THE MONTH VALUE FOR THE BILLING AUXILIARY EFFECTIVE DATE.
      *  USE THE DAY VALUE OF THE BSDT OR PFDT FOR THE DAY VALUE. CALL W2157R
      *  WITH THE QQPFDT PARM EQUAL TO MMDD0000 WHERE MM IS THE RELATIVE NUMBER
      *  OF MTHS FOR THE WMBXP KEY.  DD EQUALS THE EFFECTIVE DAY TO BE USED
      *  FILTER THE BILLING SCHEDULE CODE RECORD SELECTION IN W2157R.
      *
     C*                  CLEAR                   QQPFDT
     C*    SCBSDT        IFNE      *ZERO
     C*                  Z-ADD     SCBSDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXBGDT            7 0
     C*                  ELSE
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXBGDT
     C*                  ENDIF
      *
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXNTDT
      *
     C*                  Z-ADD     XXBGDT        XXDT
     C*                  Z-ADD     1             XXDD
      *** BEG ADD ***** 06/12/06 *************************************
      *** BEG CHG ***** 09/06/06 *************************************
     C*****************    Z-ADDXXDT      XXBGDT
     C*                  Z-ADD     XXDT          XXSTDT            7 0
      *** END CHG ***** 09/06/06 *************************************
      *** END ADD ***** 06/12/06 *************************************
     C*                  Z-ADD     0             XXINCT            2 0
      *** BEG ADD ***** 02/07/07 *************************************
     C*                  Z-ADD     XXPTDT        SVPTDT            7 0
     C*                  MOVE      '01'          SVPTDT
     C*    XXSTDT        IFLT      XXBGDT
     C*    XXNTDT        ANDEQ     SVPTDT
     C*                  ADD       1             XXINCT
     C*                  ENDIF
      *** END ADD ***** 02/07/07 *************************************
     C*    XXDT          DOWLE     XXNTDT
      *** BEG CHG ***** 09/06/06 *************************************
     C********** XXDT      IFGE XXBGDT
      *** BEG CHG ***** 02/07/07 *************************************
     C********** XXDT      IFGE XXSTDT
     C*    XXDT          IFGE      XXBGDT
      *** END CHG ***** 02/07/07 *************************************
      *** END CHG ***** 09/06/06 *************************************
     C*    XXDT          ANDLT     XXNTDT
     C*                  ADD       1             XXINCT
     C*                  ENDIF
     C*                  ADD       1             XXMM
     C*    XXMM          IFEQ      13
     C*                  Z-ADD     1             XXMM
     C*                  ADD       1             XXYY
     C*                  ENDIF
     C*                  ENDDO
     C*                  MOVE      XXBGDT        XXDD
     C*    12            SUB       XXINCT        XXMM
      *** BEG CHG ***** 09/06/06 **********************************
     C********** XXDD      IFEQ 1
     C**********           ADD  1         XXMM
     C**********           ENDIF
     C********** XXMM      IFEQ 0
     C**********           Z-ADD1         XXMM
     C**********           ENDIF
     C*                  ADD       1             XXMM
      *** END CHG ***** 09/06/06 **********************************
      *** END ADD ***** 02/28/06 **************************************
      *** BEG ADD ***** 04/03/06 **************************************
      *
      * Concatenate the efeective month/day (01/16) with the number of
      * installments which fit on the effective date range
     C*                  MOVE      *ZEROS        QQPFDT
     C*                  MOVEL     XXMDD         XXMDI1            6
     C*                  MOVE      XXINCT        XXMDI1
     C*                  MOVEL     XXMDI1        QQPFDT            8
      *
      * Calculate the number of times a billing for the 15th of the mnt
      * will fit in the effective date range
      *
     C*    XXCLT#        IFEQ      '129'
      *
     C*                  Z-ADD     XXBGDT        XXDT
     C*                  Z-ADD     15            XXDD
      *** BEG ADD ***** 06/12/06 *************************************
      *** BEG CHG ***** 09/06/06 *************************************
     C*****************    Z-ADDXXDT      XXBGDT
     C*                  Z-ADD     XXDT          XXSTDT            7 0
      *** END CHG ***** 09/06/06 *************************************
      *** END ADD ***** 06/12/06 *************************************
     C*                  Z-ADD     0             XXINCT
     C*    XXDT          DOWLE     XXNTDT
      *** BEG CHG ***** 09/06/06 *************************************
     C********** XXDT      IFGE XXBGDT
     C*    XXDT          IFGE      XXSTDT
      *** END CHG ***** 09/06/06 *************************************
     C*    XXDT          ANDLT     XXNTDT
     C*                  ADD       1             XXINCT
     C*                  ENDIF
     C*                  ADD       1             XXMM
     C*    XXMM          IFEQ      13
     C*                  Z-ADD     1             XXMM
     C*                  ADD       1             XXYY
     C*                  ENDIF
     C*                  ENDDO
      *** BEG DEL ***** 09/06/06 **************************************
     C********** 12        SUB  XXINCT    XXMM
     C********** XXMM      IFEQ 0
     C**********           Z-ADD1         XXMM
     C**********           ENDIF
      *** END DEL ***** 09/06/06 **************************************
      *
     C*                  MOVE      XXINCT        QQPFDT
     C*                  ENDIF
      *
      *** END ADD ***** 04/03/06 **************************************
      *
     C*                  Z-ADD     *ZEROS        ##RRN
     C*
     C*                  CALL      'W2157R'
     C*    *INKC         PARM                    ##KC
     C*    *INKL         PARM                    ##KL
     C*                  PARM      '2'           ##CALL
     C*                  PARM                    ##RRN
     C*                  PARM      DBRRNF        ##RRN2
     C*                  PARM                    SCBSTY
     C*                  PARM                    QQPFDT
     C*
     C*    ##RRN         IFEQ      *ZEROS
     C*
     C*                  MOVE      *ON           *IN36
     C*                  GOTO      STRT
     C*
     C*                  ELSE
     C*
     C*    ##RRN         CHAIN     WMBXP                              60
     C*
     C*                  MOVEL(P)  BXBSCD        SCBSCD
     C*                  GOTO      STRT
     C*
     C*                  ENDIF                                                  ##RRN=*ZEROS
     C*
     C*                  ENDIF                                                  XXBSTY<>*BLANKS
     C*
     C*                  ENDSR                                                  BILAUX
     *** BEG ADD *** 11/25/97 *****************************************
     C*===============================================================
     C* SRCRED - SUBROUTINE TO MODIFY CREDIT EFFECTIVE DATES
     C*===============================================================
     C*
     C*    SRCRED        BEGSR
     C*
     C*                  Z-ADD     1             X
     C*                  Z-ADD     *ZERO         Y                 2 0
     C*
     C*  PROCESS EACH CREDIT DATE FIELD
     C*
     C***  BEG   CHG  ***  06/21/10  *********************************
     C********   X         DOWLE17
     C*    X             DOWLE     21
     C***  END   CHG  ***  06/21/10  *********************************
     C*    X             ADD       1             Y
     C*
     C*    CRED(X)       IFNE      *ZERO
     C*
     C*  IF CREDIT EXP. DATE IS NOT ZERO AND LE NEW EFFECTIVE DATE
     C*  THEN CLEAR EFFECTIVE AND EXPIRATION DATES
     C*
     C*    CRED(Y)       IFNE      *ZERO
     C*    CRED(Y)       ANDLE     ELPFDT
     C*                  Z-ADD     *ZERO         CRED(X)
     C*                  Z-ADD     *ZERO         CRED(Y)
     C*
     C*                  ELSE                                                               RO
     C*
     C*  IF CREDIT EFFECTIVE DATE IS GT POLICY "TO" DATE THEN CLEAR THE
     C*  EFFECTIVE AND EXPIRATION DATES
     C*
     C*    CRED(X)       IFGT      ELPTDT
     C*                  Z-ADD     *ZERO         CRED(X)
     C*                  Z-ADD     *ZERO         CRED(Y)
     C*
     C*                  ELSE                                                               RO
     C*
     C*  IF CREDIT EFFECTIVE DATE EQUALS OLD POLICY EFFECTIVE DATE OR
     C*  IS LESS THAN NEW POLICY EFFECTIVE DATE THEN ASSIGN NEW DATE
     C*
     C*    CRED(X)       IFEQ      XXOEDT
     C*    CRED(X)       ORLT      ELPFDT
     C*                  MOVE      ELPFDT        CRED(X)
     C*                  ENDIF                                                  CRED,X EQ ELPFDT
      ***** BEG ADD ******* 04/29/02 **********************************
      *
      *  If credit expiration date equals old policy expiration date OR
      *  is less than new policy expiration date then assign new date.
      *
     C*    CRED(Y)       IFEQ      XXOXDT
     C*    CRED(Y)       ORGT      ELPTDT
     C*                  MOVE      ELPTDT        CRED(Y)
     C*                  ENDIF                                                  CRED,Y EQ ELPTDT
      *
      ***** END ADD ******* 04/29/02 **********************************
     C*
     C*                  ENDIF                                                  CRED,X GT XXPTDT
     C*
     C*                  ENDIF                                                  CRED,Y NE*ZERO
     C*
     C*                  ENDIF                                                  CRED,X NE*ZERO
     C*
     C*                  ADD       2             X
     C*                  ENDDO                                                  X DOWLE 19
     C*
     C*  ZERO OR BLANK CREDIT RATE OR CODE AS NECESSARY
     C*
     C*    ELCREF        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELASP1
     C*                  ENDIF                                                  ELCREF=0
     C*
     C*    ELDVEF        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELASP2
     C***  BEG   ADD  ***  06/21/10  *********************************
     C*                  Z-ADD     *ZERO         ELD1D2
     C*                  Z-ADD     *ZERO         ELD1X2
     C***  BEG   ADD  ***  06/21/10  *********************************
     C*                  ENDIF                                                  ELDVEF=0
     C*
     C*    ELD2EF        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELDEV2
     C***  BEG   ADD  ***  06/21/10  *********************************
     C*                  Z-ADD     *ZERO         ELD2D2
     C*                  Z-ADD     *ZERO         ELD2X2
     C***  BEG   ADD  ***  06/21/10  *********************************
     C*                  ENDIF                                                  ELD2EF=0
     C***  BEG   ADD  ***  06/21/10  *********************************
     C*    ELD1D2        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELD1S2
     C*                  ENDIF                                                  ELD2EF=0
     C*
     C*    ELD2D2        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELD2S2
     C*                  ENDIF                                                  ELD2EF=0
     C***  END   ADD  ***  06/21/10  *********************************
     C*  IF INCR. LIMITS IS ZERO THEN INCR. LIMITS 2 MUST BE ZERO
     C*
     C*    ELILEF        IFEQ      *ZERO
     C*                  MOVE      *BLANKS       ELILCD
     C*                  MOVE      *ZERO         ELI2EF
     C*                  MOVE      *ZERO         ELI2EX
     C*                  ENDIF                                                  ELILEF=0
     C*
     C*    ELI2EF        IFEQ      *ZERO
     C*                  MOVE      *BLANKS       ELINL2
     C*                  ENDIF                                                  ELI2EF=0
     C*
     C*    ELDCEF        IFEQ      *ZERO
     C*                  MOVE      *BLANKS       ELDCDE
     C*                  MOVE      *BLANK        ELCXBL
     C*                  ENDIF                                                  ELDCEF=0
     C*
     C*    ELMCEF        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELASPM
     C*                  MOVE      *BLANKS       ELMCCD
     C*                  ENDIF                                                  ELMCEF=0
     C*
     C*    ELSCEF        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELASPS
     C*                  ENDIF                                                  ELSCEF=0
     C*
     C*    ELSDEF        IFEQ      *ZERO
     C*                  MOVE      *ZERO         ELDBCR
     C*                  ENDIF                                                  ELSDEF=0
     C*
     C*                  ENDSR
     C*
     *** END ADD *** 11/25/97 *****************************************
     C*_______________________________________________________________
     C*
     C***  BEGIN ADD  ***  11/10/99  *********************************
     C*===============================================================
     C* DATES2 - FORMAT DATES FOR RE-DISPLAYING SCR02
     C*===============================================================
     C*
     C*    DATES2        BEGSR
     C*
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCNEFD
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCPTDT
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  Z-ADD     SCNADT        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCNADT
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  Z-ADD     SCBSDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCBSDT
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  Z-ADD     SCEMD1        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCEMD1
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  Z-ADD     SCEMD2        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCEMD2
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  Z-ADD     SCEMD3        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCEMD3
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*
     C*    SRERR         IFEQ      *OFF
     C*                  MOVEL     SRDTO         SCPTDT
     C*                  ENDIF                                                  SRERR=*OFF
     C*
     C*                  ENDSR                                                  DATES2
     C*
     C*===============================================================
     C* ERRCK2 - ERROR CHECKING FOR SCR02
     C*===============================================================
     C*
     C*    ERRCK2        BEGSR
     C*
     C*                  Z-ADD     SCNFYR        XXNFYR
     C*
     C*  GET GROUP DETAIL INFORMATION
     C*
     C*    F1KY          CHAIN     PF101                              60
     C*
     C* N90*IN60         IFEQ      *ON
     C*                  SETON                                        71  90
     C*                  ENDIF                                                  *IN60=*ON
     C*
     C*  CHECK TO BE SURE POLICY YEAR HAS NOT BEEN USED PREVIOUSLY
     C*
     C* N90SCFYR         IFNE      SCNFYR
     C*
B02  C*    ELKYN         CHAIN(N)  PEL01                              60
     C*
     C*    *IN60         IFEQ      *OFF
     C*                  SETON                                        72  90
     C*                  ENDIF                                                  *IN60=*OFF
     C*
     C*                  ENDIF                                                  SCFYR<>SCNFYR
     C*
      ****** BEGIN ADD  ****** 03/31/07 *******************************
      * SET DEFAULT EXPIRATION DATE
      *
     C*    FNCGFL        IFEQ      'N'
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXNTOD
      *
     C*    XXNTOD        IFNE      F1ENPO
     C*                  Z-ADD     F1ENPO        SRDTI
     C*                  CALL      'SRCVTDAT'    DTMDYY
     C*                  MOVEL     SRDTO         SCPTDT
     C*                  SETON                                            90
     C*                  ENDIF
     C*                  ENDIF
      *
      ****** END ADD    ****** 03/31/07 *******************************
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXNEFD
     C*
     C*                  Z-ADD     SCNADT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXANDT
     C*
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXNTOD
     C*
     C*  EDIT NEW POLICY FROM DATE
     C*
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*                  MOVEL     SRDTO         SCNEFD
     C*
     C* N90SRERR         IFEQ      *ON
     C*                  SETON                                        21  90
     C*                  ENDIF                                                  *IN60=*OFF
     C*
     C*  EDIT NEW POLICY TO DATE
     C*
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*                  MOVEL     SRDTO         SCPTDT
     C*
     C* N90SRERR         IFEQ      *ON
     C*                  SETON                                        98  90
     C*                  ENDIF                                                  SRERR=*ON
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C* N90XXNTOD        IFLT      XXNEFD
     C*                  SETON                                        74  90
     C*                  ENDIF                                                  XXNTOD<XXNEFD
     C*
     C*  BOTH DATES MUST BE BETWEEN GROUP POLICY START & END DATES
     C*
     C* N90FNCGFL        IFNE      'Y'
     C*
     C*    XXNEFD        IFLT      F1STPO
     C*    XXNEFD        ORGE      F1ENPO
     C*    XXNTOD        ORLT      F1STPO
     C*    XXNTOD        ORGT      F1ENPO
     C*                  SETON                                        75  90
     C*                  ENDIF                                                  XXNEFD<F1STPO
     C*
     C*                  ELSE
     C*
     C*    EMSTRP        IFEQ      'Y'
     C*
     C*    XXNEFD        IFLT      F1STPO
     C*    XXNEFD        ORGE      F1ENPO
     C*                  SETON                                        75  90
     C*                  ENDIF                                                  XXNEFD<F1STPO
     C*
     C*                  ENDIF                                                  EMSTRP=Y
     C*
     C*                  ENDIF                                                  FNCGFL<>Y
     C*
     C* DATES CANNOT OVERLAP DATE RANGES FOR THAT EMPLOYER
     C*
     C*    ELKY1         CHAIN(N)  PEL01                              60
     C*
     C* N90*IN60         DOWEQ     *OFF
     C*
     C*    ELFYR         IFNE      SCFYR
     C*
     C* N90XXNEFD        IFGE      ELPFDT
     C*    XXNEFD        ANDLT     ELPTDT
     C*                  SETON                                        76  90
     C*                  ENDIF                                                  XXNEFD>=ELPFDT
     C*
     C* N90XXNTOD        IFGT      ELPFDT
     C*    XXNTOD        ANDLT     ELPTDT
     C*                  SETON                                        77  90
     C*                  ENDIF                                                  XXNTOD>ELPFDT
     C*
     C*                  ENDIF                                                  ELFYR<>SCFYR
     C*
     C*    ELKY          READE(N)  PEL01                                  60
     C*
     C*                  ENDDO                                                  *IN60=*OFF
     C*
     C*  POLICY END DATE CANNOT BE > POLICY END DATE OF THE STATE
     C*   REPORTING DIVISION
     C*
     C*    EMKY3         CHAIN(N)  PEM01                              65
     C*
     C* N90*IN65         DOWEQ     *OFF
     C*
     C*    EMSTRP        IFEQ      'Y'
     C*
     C*    EMDIV         IFNE      SCDIV
     C*
     C*    ELKY3         CHAIN(N)  PEL01                              66
     C*
     C*    XXNTOD        IFGT      ELPTDT
     C*                  SETON                                        83  90
     C*                  LEAVE
     C*                  ENDIF                                                  XXNTOD>ELPTDT
     C*
     C*                  MOVE      *OFF          DTLESS
     C*
     C* N90XXNTOD        IFLT      ELPTDT
     C*                  MOVE      *ON           DTLESS
     C*                  ENDIF                                                  XXDAT2<ELPTDT
     C*
     C*                  ENDIF                                                  EMDIV<>SCDIV
     C*
     C*                  ENDIF                                                  EMSTRP=Y
     C*
     C*    EMKY3         READE(N)  PEM01                                  65
     C*
     C*                  ENDDO                                                  *IN65=*OFF
     C*
      *** BEG ADD ***** 07/15/11 **************************************
     C*    EMKY          CHAIN(N)  PEM01                              60
      *** END ADD ***** 07/15/11 **************************************
     C*    ELKY          CHAIN(N)  PEL01                              66
      ***** BEG DEL ******* 04/25/02 **********************************
     C*********
     C***TO*DATE MUST EQUAL CANCEL DATE IN EMPLOYER MASTER FOR EACH DIV
     C*********
     C**N90****  DTLESS    IFEQ *ON
     C*********
     C*********  EMKY      CHAINPEM01               N65
     C*********
     C**N90****  XXNTOD    IFNE EMCAD1
     C*********  XXNTOD    ANDNEEMCAD2
     C*********  XXNTOD    ANDNEEMCAD3
     C*********  XXNTOD    ANDNEEMCAD4
     C*********  XXNTOD    ANDNEEMCAD5
     C*********            SETON                     78  90
     C*********            ENDIF                           XXNTOD<>EMCAD1
     C*********
     C*********            ENDIF                           DTLESS=*ON
      ***** END DEL ******* 04/25/02 **********************************
     C*
     C*  POLICY END DATE CANNOT BE MORE THAN 365(366) DAYS PAST POLICY
     C*  START DATE
     C*
     C* N90FNCGFL        IFNE      'Y'
     C*
     C*                  CALL      'SRDDIF8'
     C*                  PARM                    SCNEFD
     C*                  PARM                    SCPTDT
     C*                  PARM                    XXDAYS            5 0
     C*                  PARM                    XXERR             1
     C*
     C*                  MOVE      *OFF          LEAPYR
     C*
     C*  CHECK FOR LEAP YEAR
     C*
     C*    TO DATE
     C*
     C*                  MOVE      XXNTOD        XXDT
     C*    XXYY          DIV       4             XXLPYR            2 1
     C*                  MVR                     XXREM             1 0
     C*
     C*    XXREM         IFEQ      *ZERO
     C*    XXMMDD        ANDGT     0228
     C*                  MOVE      *ON           LEAPYR
     C*                  ENDIF                                                  XXREM=*ZERO
     C*
     C*    FROM DATE
     C*
     C*                  MOVE      XXNEFD        XXDT
     C*    XXYY          DIV       4             XXLPYR
     C*                  MVR                     XXREM
     C*
     C*    XXREM         IFEQ      *ZERO
     C*    XXMMDD        ANDLT     0301
     C*                  MOVE      *ON           LEAPYR
     C*                  ENDIF                                                  XXREM=*ZERO
     C*
     C* N90LEAPYR        IFEQ      *OFF
     C*
     C*    XXDAYS        IFGT      365
     C*                  SETON                                        79  90
     C*                  ENDIF                                                  XXDAYS>365
     C*
     C*                  ELSE
     C*
     C*    XXDAYS        IFGT      366
     C*                  SETON                                        79  90
     C*                  ENDIF                                                  XXDAYS>366
     C*
     C*                  ENDIF                                                  LEAPYR=*OFF
     C*
     C*                  ELSE
     C*
     C*                  Z-ADD     XXNEFD        X7MXDT                         FROM DATE
     C*                  ADD       XXMXEX        X7MXMM
     C*                  ADD       XXMXCY        X7MXCY
     C*
     C*    XXNTOD        IFGT      X7MXDT
     C*                  SETON                                        80  90
     C*                  ENDIF                                                  XXNTOD>X7MXDT
     C*
     C*                  ENDIF                                                  FNCGFL<>Y
     C*
     C*  EDIT ANNIVERSARY RATING DATE
     C*
     C* N90SCNADT        IFNE      *ZERO
     C*
     C*                  Z-ADD     SCNADT        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*                  MOVEL     SRDTO         SCNADT
     C*
     C*    SRERR         IFEQ      *ON
     C*                  SETON                                        23  90
     C*                  ENDIF                                                  SRERR=*ON
     C*
     C*                  ENDIF                                                  SCNADT<>*ZERO
     C*
     C*  ANNIV DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C*
     C* N90XXANDT        IFGT      XXNEFD
     C*                  SETON                                        24  90
     C*                  ENDIF                                                  XXANDT>XXNEFD
     C*
     C*  EDIT NEW BILLING START DATE
     C*
     C*                  Z-ADD     SCBSDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*                  MOVEL     SRDTO         SCBSDT
     C*
     C* N90SRERR         IFEQ      *ON
     C*                  SETON                                        38  90
     C*                  ENDIF                                                  SRERR=*ON
     C*
     C*  MOD REQUIRED IF EFFECTIVE DATE ENTERED
     C*
     C* N90SCEXM1        IFGT      *ZERO
     C*    SCEMD1        ANDEQ     *ZERO
     C*                  SETON                                        25  90
     C*                  ENDIF                                                  SCEXM1>*ZERO
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C* N90SCEMD1        IFNE      *ZERO
     C*
     C*                  Z-ADD     SCEMD1        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*                  MOVEL     SRDTO         SCEMD1
     C*
     C*    SRERR         IFEQ      *ON
     C*                  SETON                                        28  90
     C*                  ENDIF                                                  SRERR=*ON
     C*
     C*  FIRST EFFECT DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C*
     C*                  Z-ADD     SCEMD1        X1EMD1
     C*
     C*                  Z-ADD     X1EMD1        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXEMD1
     C*
     C* N90XXEMD1        IFGT      XXNEFD
     C*                  SETON                                        32  90
     C*                  ENDIF                                                  XXEMD1>XXNEFD
     C*
     C*                  ENDIF                                                  SCEMD1<>*ZERO
     C*
     C*  MOD REQUIRED IF EFFECTIVE DATE ENTERED
     C*
     C* N90SCEXM2        IFGT      *ZERO
     C*    SCEMD2        ANDEQ     *ZERO
     C*                  SETON                                        26  90
     C*                  ENDIF                                                  SCEXM2>*ZERO
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C* N90SCEMD2        IFNE      *ZERO
     C*
     C*                  Z-ADD     SCEMD2        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*                  MOVEL     SRDTO         SCEMD2
     C*
     C*    SRERR         IFEQ      *ON
     C*                  SETON                                        30  90
     C*                  ENDIF                                                  SRERR=*ON
     C*
     C*                  Z-ADD     SCEMD1        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXDAT1
     C*
     C*                  Z-ADD     SCEMD2        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXDAT2
     C*
     C* N90XXDAT2        IFLT      XXDAT1
     C*                  SETON                                        29  90
     C*                  ENDIF                                                  XXDAT2<XXDAT1
     C*
     C*                  ENDIF                                                  SCEMD2<>*ZERO
     C*
     C*  MOD REQUIRED IF EFFECTIVE DATE ENTERED
     C*
     C* N90SCEXM3        IFGT      *ZERO
     C*    SCEMD3        ANDEQ     *ZERO
     C*                  SETON                                        27  90
     C*                  ENDIF                                                  SCEXM3>*ZERO
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C* N90SCEMD3        IFNE      *ZERO
     C*
     C*                  Z-ADD     SCEMD3        SRDTI
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*                  MOVEL     SRDTO         SCEMD3
     C*
     C*    SRERR         IFEQ      *ON
     C*                  SETON                                        31  90
     C*                  ENDIF                                                  SRERR=*ON
     C*
     C*                  Z-ADD     SCEMD2        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXDAT1
     C*
     C*                  Z-ADD     SCEMD3        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXDAT2
     C*
     C* N90XXDAT2        IFLT      XXDAT1
     C*                  SETON                                        29  90
     C*                  ENDIF                                                  XXDAT2<XXDAT1
     C*
     C*                  ENDIF                                                  SCEMD2<>*ZERO
     C*
     C* N90SCBSCD        IFEQ      *BLANKS
     C*    SCBSCD        OREQ      *ZEROS
     C*                  EXSR      BILAUX
     C*                  ENDIF                                                  SCBSCD=*BLANKS
     C*
     C*  VALIDATE THE BILLING SCHEDULE CODE
     C*
     C*                  MOVEL(P)  SCBSCD        XXKEY
     C*
     C*    TBKY          CHAIN     PBS01                              60
     C*
     C* N90*IN60         IFEQ      *ON
     C*    BSSTAT        ORNE      'A'
     C*                  SETON                                        34  90
     C*                  ENDIF                                                  *IN60=*ON
     C*** Beg Add ***** 12/05/11 ***********************************************
     C* Billings must not be posted after policy effective date
     C*
     C* N90              EXSR      SRBILL
     C*** End Add ***** 12/05/11 ***********************************************
     C*
     C*  'Update all divisions' must be (Y/ )
     C*
     C* N90SCUPDD        IFNE      'Y'
     C*    SCUPDD        ANDNE     ' '
     C*                  SETON                                        71  90
     C*                  ENDIF                                                  SCUPPD<>Y/' '
     C*
      *** BEG CHG ***** 07/15/11 **************************************
      *** BEG ADD ***** 05/09/08 **************************************
     C************IN90     IFEQ *OFF
      *** BEG ADD ***** 01/25/11 **************************************
     C***********XXNEFD    IFNE ELPFDT
     C***********XXNEFD    IFLT SYSDAT
     C***********          Z-ADDXXNEFD    AGTEFD  70
     C***********          ELSE
     C***********          Z-ADDSYSDAT    AGTEFD
     C***********          ENDIF
      *** END ADD ***** 01/25/11 **************************************
     C*******              Z-ADD0         XXCNT   50
     C*******/EXEC SQL
     C***SELECT COUNT(*) INTO :XXCNT
     C*****FROM WMAHP
     C*****WHERE  AHCO#   =  :XXCO#  AND
     C*******     AHFND   =  :XXFND  AND
     C*******     AHEMP#  =  :XXEMP# AND
     C*******     AHDIV   =  :XXDIV  AND
     C*******/END-EXEC
     C*******    XXCNT     IFGT 0
     C*******              Z-ADD0         XXCNT
      *******
     C***/EXEC*SQL
     C***SELECT COUNT(*) INTO :XXCNT
     C*****FROM WMAHP JOIN WMAAP
     C******ON AHCO#  = AACO#  AND
     C*******  AHFND  = AAFND  AND
     C*******  AHAGT# = AAAGT#
     C*****WHERE  AHCO#   =  :XXCO#  AND
     C*******     AHFND   =  :XXFND  AND
     C*******     AHEMP#  =  :XXEMP# AND
     C*******     AHDIV   =  :XXDIV  AND
      ****BEG CHG ***** 01/25/11 **************************
     C*********** :XXNEFD >= AAEFDT  AND
     C*******     :AGTEFD >= AAEFDT  AND
      ****END CHG ***** 01/25/11 **************************
     C*******     AACNDT  =  0       AND
     C*******     AASTAT  = 'A'       OR
     C*******     AHCO#   =  :XXCO#  AND
     C*******     AHFND   =  :XXFND  AND
     C*******     AHEMP#  =  :XXEMP# AND
     C*******     AHDIV   =  :XXDIV  AND
      ****BEG CHG ***** 01/25/11 **************************
     C*********** :XXNEFD >= AAEFDT  AND
     C*********** :XXNEFD <  AACNDT  AND
     C*********** AACNDT  =  0       AND
     C*******     :AGTEFD >= AAEFDT  AND
     C*******     :AGTEFD <  AACNDT  AND
     C*******     AACNDT  <> 0       AND
      ****END CHG ***** 01/25/11 **************************
     C*******     AASTAT  = 'A'
     C***/END-EXEC
     C***********XXCNT     IFEQ *ZERO
      ****BEG*CHG ***** 01/25/11 **************************
     C***********          MOVE *ON       *IN89
     C***********XXNEFD    IFLT SYSDAT
     C***********          MOVE *ON       *IN89
     C***********          ELSE
     C***********          MOVE *ON       *IN88
     C***********          ENDIF
      ****END*CHG ***** 01/25/11 **************************
     C***********          MOVE *ON       *IN90
     C***********          ENDIF
     C***********          ENDIF
      ****BEG*ADD ***** 01/25/11 **************************
     C***********          ENDIF
      ****END*ADD ***** 01/25/11 **************************
     C***********          ENDIF
     C****END*ADD ***** 05/09/08 **************************************
      * AGENT HISTORY
      *  INVOKE EDIT IF MORE THAN ONE ACTIVE AGENT HISTORY RCD
      *  EXISTS PRIOR TO CURRENT POLICY EFFECTIVE DATE
      *  IF POLICY EFF DATE IS CHANGING TO A PRIOR DATE,
      *  LOOK FOR RECORD THAT CURRENTLY SETS RATE ON POLICY AS
      *  OF THE CURRENT POLICY EFDT. IF NONE, LOOK FOR FUTURE
      *  RECORD. IF EFDT NEEDS UPDATE, ENSURE THAT AGENT ASSIGNMENT
      *  IS VALID FOR THE NEW EFDT.
      *
     C*    *LIKE         DEFINE    AHAGT#        SVAGT#
     C*    *LIKE         DEFINE    AHEFDT        AGTEFD
     C*    *LIKE         DEFINE    AHEFDT        SVAEDT
     C*    *IN90         IFEQ      *OFF
     C*                  MOVE      *OFF          ADCFLG
     C*                  Z-ADD     0             AGTEFD
     C*                  Z-ADD     0             SVAEDT
     C*                  MOVE      *OFF          NOAGT             1
     C*                  MOVE      *OFF          FUTAGT            1
     C*                  Z-ADD     0             SVAGT#
      *
     C*    XXNEFD        IFLT      XXFOLD
     C*EXEC SQL
     C*  SELECT COUNT(*) INTO :XXCNT
     C*    FROM WMAHP
     C*     WHERE  AHCO#   =  :XXCO#  AND
     C*            AHFND   =  :XXFND  AND
     C*            AHEMP#  =  :XXEMP# AND
     C*            AHDIV   =  :XXDIV  AND
     C*            AHFYR   =  :XXFYR  AND
     C*            AHSTAT  =  'A'     AND
     C*            AHEFDT <=  :XXFOLD
     C*END-EXEC
      *
     C*    XXCNT         IFGT      1
     C*    SCUPDD        IFEQ      'Y'
     C*    DIVCNT        ANDGT     1
     C*                  MOVE      DMSG          SCDMSG                         CHK ALL DIV MSG
     C*                  WRITE     SCR02
     C*                  ELSE
     C*                  MOVE      *BLANKS       SCDMSG                         CHK ALL DIV MSG
     C*                  WRITE     SCR02
     C*                  ENDIF
     C*                  MOVE      *ON           *IN84
     C*                  MOVE      *ON           *IN90
     C*                  GOTO      STRT
     C*                  ENDIF
      *
     C*                  Z-ADD     XXFOLD        KKEFDT
     C*    AHKY3         SETGT     PAH01
     C*    AHKY1         READPE(N) PAH01                                  60
     C*    *IN60         IFEQ      *ON
     C*    AHKY3         SETLL     PAH01
     C*    AHKY1         READE(N)  PAH01                                  60
     C*    *IN60         IFEQ      *OFF
     C*    AHSTAT        ANDEQ     'A'
     C*                  Z-ADD     AHEFDT        KKEFDT
     C*    AHKY3         SETGT     PAH01
     C*    AHKY1         READPE(N) PAH01                                  60
     C*                  ENDIF                                                  *IN60=*OFF
     C*                  ENDIF                                                  *IN60=*ON
      *
     C*                  SELECT
     C*    *IN60         WHENEQ    *ON
     C*                  MOVE      *ON           NOAGT
     C*    *IN60         WHENEQ    *OFF
     C*    KKEFDT        ANDNE     XXFOLD
     C*                  MOVE      *ON           FUTAGT            1
     C*                  ENDSL
      *
     C*    *IN60         DOWEQ     *OFF
     C*                  SELECT
     C*    AHSTAT        WHENNE    'A'
     C*    AHEFDT        WHENLE    XXNEFD
     C*                  Z-ADD     AHEFDT        AGTEFD
     C*                  Z-ADD     0             SVAEDT
     C*                  Z-ADD     0             SVAGT#
     C*                  LEAVE
     C*    AHEFDT        WHENGT    XXNEFD
     C*    FUTAGT        OREQ      *ON
     C*                  Z-ADD     AHEFDT        SVAEDT
     C*                  Z-ADD     AHAGT#        SVAGT#
     C*                  ENDSL
     C*    AHKY2         READPE(N) PAH01                                  60
     C*                  ENDDO
      *
      * IF NO AHEFDT EXISTS EARLIER THAN NEW EFF DT, ASSIGN ONE
      *
     C*                  SELECT
     C*    NOAGT         WHENEQ    *ON
     C*                  MOVE      *OFF          ADCFLG
     C*    AGTEFD        WHENNE    *ZERO
     C*                  MOVE      *OFF          ADCFLG
     C*    XXNEFD        WHENLE    SYSDAT
     C*                  Z-ADD     XXNEFD        AGTEFD
     C*                  MOVE      *ON           ADCFLG            1            AGT EFDT CHG FL
     C*    XXNEFD        WHENGT    SYSDAT
     C*                  Z-ADD     SYSDAT        AGTEFD
     C*                  MOVE      *ON           ADCFLG            1            AGT EFDT CHG FL
     C*                  ENDSL
     C*                  ENDIF                                                  XXNEFD<XXFOLD
      *
      * IF MOVING POLICY EFF DATE FORWARDS AND AGENT HISTORY RECORD
      * DOESN'T EXIST PRIOR, CHECK IF AGENT HISTORY RECORD EXISTS IN
      * FUTURE THAT CAN BE BACKED UP TO NEW EFDT.
      *
     C*    XXNEFD        IFGT      XXFOLD
     C*EXEC SQL
     C*  SELECT COUNT(*) INTO :XXCNT
     C*    FROM WMAHP
     C*     WHERE  AHCO#   =  :XXCO#  AND
     C*            AHFND   =  :XXFND  AND
     C*            AHEMP#  =  :XXEMP# AND
     C*            AHDIV   =  :XXDIV  AND
     C*            AHFYR   =  :XXFYR  AND
     C*            AHSTAT  =  'A'     AND
     C*            AHEFDT <=  :XXNEFD
     C*END-EXEC
     C*    XXCNT         IFEQ      0
     C*                  Z-ADD     XXNEFD        KKEFDT
     C*    AHKY3         SETLL     PAH01
     C*    AHKY1         READE(N)  PAH01                                  60
     C*    *IN60         DOWEQ     *OFF
     C*    AHSTAT        IFEQ      'A'
     C*                  Z-ADD     SYSDAT        AGTEFD
     C*                  Z-ADD     AHEFDT        SVAEDT
     C*                  Z-ADD     AHAGT#        SVAGT#
     C*                  MOVE      *ON           ADCFLG            1
     C*                  LEAVE
     C*                  ENDIF
     C*    AHKY1         READE(N)  PAH01                                  60
     C*                  ENDDO
     C*                  ENDIF                                                  XXCNT = 0
     C*                  ENDIF                                                  XXNEFD<XXFOLD
      *
      * ENSURE AGENT ASSIGNMENT RECORD EXISTS FOR NEW AHEFDT
      *
     C*    ADCFLG        IFEQ      *ON
     C*                  Z-ADD     0             XXCNT             5 0
     C*EXEC SQL
     C*  SELECT COUNT(*) INTO :XXCNT
     C*    FROM WMAAP
     C*     WHERE  AACO#   =  :XXCO#  AND
     C*            AAFND   =  :XXFND  AND
     C*            AAAGT#  =  :SVAGT# AND
     C*            AASTAT  =  'A'     AND
     C*           (:AGTEFD >= AAEFDT  AND
     C*            AACNDT  =  0       OR
     C*            :AGTEFD >= AAEFDT  AND
     C*            :AGTEFD <  AACNDT  AND
     C*            AACNDT  <> 0)
     C*END-EXEC
      *
     C*    XXCNT         IFEQ      *ZERO
     C*    SCUPDD        IFEQ      'Y'
     C*    DIVCNT        ANDGT     1
     C*                  MOVE      DMSG          SCDMSG                         CHK ALL DIV MSG
     C*                  WRITE     SCR02
     C*                  ELSE
     C*                  MOVE      *BLANKS       SCDMSG                         CHK ALL DIV MSG
     C*                  ENDIF
     C*    XXNEFD        IFLT      SYSDAT
     C*                  MOVE      *ON           *IN89
     C*                  ELSE
     C*                  MOVE      *ON           *IN88
     C*                  ENDIF                                                  XXNEFD < SYSDAT
     C*                  MOVE      *ON           *IN90
     C*                  GOTO      STRT
     C*                  ENDIF                                                  XXCNT = 0
     C*                  ENDIF                                                  ADCFLG = *ON
     C*                  ENDIF                                                  *IN90 = *OFF
      *** END CHG ***** 07/15/11 **************************************
      *
      *** BEG ADD ***** 11/29/11 **********************************
     C*    *IN90         IFEQ      *ON
     C*                  GOTO      STRT
     C*                  ENDIF
      *** END ADD ***** 11/29/11 **********************************
      *
     C*                  ENDSR                                                  ERRCK2
      ***** BEG ADD ******* 12/05/11 **********************************
      *===============================================================
      * SRBILL - CHECK NO BILLING AFTER POL EFF DT
      *===============================================================
      *
     C*    SRBILL        BEGSR
      *
      *  Only perform edit if Installment ranges in given Policy dates
      *
      *  Get number of billing periods
      *
     C*                  SELECT
     C*    BSAB12        WHENNE    *ZERO
     C*                  Z-ADD     12            XXHIBS            2 0
     C*    BSAB11        WHENNE    *ZERO
     C*                  Z-ADD     11            XXHIBS            2 0
     C*    BSAB10        WHENNE    *ZERO
     C*                  Z-ADD     10            XXHIBS            2 0
     C*    BSAB09        WHENNE    *ZERO
     C*                  Z-ADD     9             XXHIBS            2 0
     C*    BSAB08        WHENNE    *ZERO
     C*                  Z-ADD     8             XXHIBS            2 0
     C*    BSAB07        WHENNE    *ZERO
     C*                  Z-ADD     7             XXHIBS            2 0
     C*    BSAB06        WHENNE    *ZERO
     C*                  Z-ADD     6             XXHIBS            2 0
     C*    BSAB05        WHENNE    *ZERO
     C*                  Z-ADD     5             XXHIBS            2 0
     C*    BSAB04        WHENNE    *ZERO
     C*                  Z-ADD     4             XXHIBS            2 0
     C*    BSAB03        WHENNE    *ZERO
     C*                  Z-ADD     3             XXHIBS            2 0
     C*    BSAB02        WHENNE    *ZERO
     C*                  Z-ADD     2             XXHIBS            2 0
     C*    BSAB01        WHENNE    *ZERO
     C*                  Z-ADD     1             XXHIBS            2 0
     C*                  OTHER
     C*                  Z-ADD     0             XXHIBS            2 0
     C*                  ENDSL
      *
     C*    SCBSDT        IFNE      *ZERO
     C*                  Z-ADD     SCBSDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXBGDT            7 0
     C*                  ELSE
     C*                  Z-ADD     SCNEFD        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXBGDT
     C*                  ENDIF
     C*                  Z-ADD     SCPTDT        SRDTI
     C*                  CALL      'SRCVTDAT'    DTCYMD
     C*                  MOVEL     SRDTO         XXTODT            7 0
      *
      * Checks how many billings for the 1st will fall within the
      * effective dates
      *
     C*                  Z-ADD     XXBGDT        XXDT
      *
     C*    BSON15        IFEQ      'Y'
     C*                  Z-ADD     15            XXDD
     C*                  ELSE
     C*                  Z-ADD     1             XXDD
     C*                  ENDIF
     C*
     C*                  Z-ADD     XXDT          XXBGDT
     C*                  Z-ADD     0             XXINCT            2 0
     C*    XXDT          DOWLE     XXTODT
     C*    XXDT          IFGE      XXBGDT
     C*    XXDT          ANDLT     XXTODT
     C*                  ADD       1             XXINCT
     C*                  ENDIF
     C*                  ADD       1             XXMM
     C*    XXMM          IFEQ      13
     C*                  Z-ADD     1             XXMM
     C*                  ADD       1             XXYY
     C*                  ENDIF
     C*                  ENDDO
      *
     C*
     C*                  SETON                                        90  04
     C*                  ENDIF
      *
     C*                  ENDSR
      *
      ***** End Add ******* 12/05/11 **********************************
     C*===============================================================
     C* RESET  - RESET ERROR INDICATORS
     C*===============================================================
     C*
     C*    RESET         BEGSR
     C*
     C*                  MOVEA     '00000'       *IN(71)
     C*                  MOVEA     '00000'       *IN(76)
     C*                  MOVEA     '00000'       *IN(81)
     C*                  MOVEA     '00000'       *IN(86)
     C*
     C*                  ENDSR                                                  RESET
     C***  END   CHG  ***  11/10/99  *********************************
      ***** End Del ***** 03/03/20 ***********************************


      ***** Beg Add ***** 03/03/20 ***********************************
      /Free
       ChkStack('WTPOLQTE':isFound);
       If isFound;
          charDesc = 'The menu version of this program is no longer available.';
       Else;
          charDesc = 'This program is no longer used.';
       Endif;

       Exfmt WdwDisp;

       *inlr = *on;

      /End-free
      ***** End Add ***** 03/03/20 ***********************************



