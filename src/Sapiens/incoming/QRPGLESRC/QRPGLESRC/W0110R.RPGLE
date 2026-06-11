     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     h BndDir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W0110R - CASH AND ADJUSTMENT POSTING
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/25/21 C:79  Project: WC2663                                *
     H*                Set flag to no longer support                  *
     H*                                                               *
     H* 12/08/20 C:33  Project: WC2663                                *
     H*                No longer supported.                           *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP.  *
     H*                                                               *
     H* 03/03/03 C:136 SUPPORT: WA081 36232                           *
     H*                Prevent program from locking up WMEAP          *
     H*                unnecessarily.                                 *
     H*                                                               *
     H* 10/01/02 C:61  SUPPORT GL102 34221                            *
     H*                Prevent back posting for S/R employers except  *
     H*                when A/R code same as entered in GROUP DETAIL  *
     H*                INFORMATION SCR02.                             *
     H*                                                               *
     H* 04/26/02 C:132 SUPPORT# WA101 32225                           *
     H*                Add Refunds to Expense Constant Due Field      *
     H*                SCECDU on screen.                              *
     H*                                                               *
     H* 12/17/01 C:136 SUPPORT# WA065 30370                           *
     H*                Error checking routine changed to ensure uni-  *
     H*                formity.                                       *
     H*                                                               *
     H* 11/15/01 C:136 SUPPORT# 29681                                 *
     H*                Insure that each record written to A/R Detail  *
     H*                (WMEAP) has a unique timestamp.                *
     H*                                                               *
     H* 06/12/01 C:61  PROJECT WC577                                  *
     H*                Info message if refund is entered as a         *
     H*                negative.                                      *
     H*                                                               *
     H* 05/10/01 C:134 PROJECT: SUPPORT  WA109/7410                   *
     H*                Allowed for back dating of NSF.                *
     H*                                                               *
     H* 10/19/00 C:33  PROJECT: WC899                                 *
     H*                IF POLICY IS ACCOUNT CURRENT AND A PAYMENT IS  *
     H*                ENTERED, DISPLAY MESSAGE.                      *
     H*                                                               *
     H* 07/03/00  C:35   PROJECT: WC577                               *
     H*                  PGM WAS CRASHING IF AN ITEM WAS SELECTED     *
     H*                  WHEN A/R INQ INVOKED FROM SCR01.             *
     H*                                                               *
     H* 01/24/00 C:33  PROJECT: WC577                                 *
     H*                ADD VOIDS, DON'T SUBTRACT THEM.                *
     H*                                                               *
     H* 10/21/99 C:33  SEQ#: WA081/20248                              *
     H*                ACCOUNT FOR ALL 'B' AND 'A' TYPE TRANSCATIONS  *
     H*                WHEN CALCULATING THE BALANCE.                  *
     H*                                                               *
     H* 10/01/99 C:33  SEQ#: WA081/19900                              *
     H*                RETURN SCR02 A/R INFO TO CORRECT POSITION.     *
     H*                                                               *
     H*  09/29/99 C:118 PROJECT: WC939                                *
     H*                 Added 'Premium Finance Co.' in RI in display  *
     H*                 if a premium finance co. has been assigned to *
     H*                 the policy.  Added F7=Finance info which calls*
     H*                 W0045R02 WW Premium Finance Co. maintenance   *
     H*                 to display finance company details. Added     *
     H*                 WMPFP. If refund trans display prem fin co    *
     H*                 address.
     H*
     H* 08/25/99 C:33 PROJECT: WC577                                  *
     H*               WHEN ENTERING A REFUND, INFORM THE USER WHEN THE*
     H*               NET BALANCE BECOMES NEGATIVE.                   *
     H*                                                               *
     H* 08/16/99 C:61  PROJECT WC577                                  *
     H*              - Display number of divisions in entered FYR in  *
     H*                header info on SCR02.                          *
     H*              - Add activity status description and contested  *
     H*                audit flag to header info on SCR02.            *
     H*                                                               *
     H*  06/29/99  C:118 PROJECT: WC577                               *
     H*                  Fixed SCR02  so late flag would NOT clear    *
     H*                  when F10 is pressed.  Changed F10=A/R search *
     H*                  to F10=Search in display file. Policy year   *
     H*                  100 was being loaded when no selection was   *
     H*                  made.  Commented out obsolete code for       *
     H*                  F1=Restart. Added policy year validation.    *
     H*                                                               *
     H*  03/04/99  C:78  PROJECT: WC874                               *
     H*                  DELETE F16=S/R ENTRY FROM SCR02. CORRECT     *
     H*                  F7=A/R SEARCH AND CHANGE F17=TRANSS EDIT     *
     H*                  TO F17=TRANS EDIT.                           *
     H*                                                               *
     H*  11/13/98  C:78  PROJECT: WC874                               *
     H*                  ADD F7=A/R SEARCH TO SCR01. MAKE SURE USER   *
     H*                  KEYS IN FYR/EMP#/DIV BEFORE DOING SEARCH.    *
     H*                  ADD F16=S/R ENTRY TO SCR02.                  *
     H*                                                               *
     H*  06/26/98  C:78  PROJECT: 95WCJ                               *
     H*                  IF EMPLOYER HAS A CANCELLATION DATE, SHOW    *
     H*                  IT IN SCR.                                   *
     H*                                                               *
     H*  06/24/98  C:33  SEQ#: WA093/12385                            *
     H*                  DON'T WRITE RECORDS TO BILL TRANSACTION FILE.*
     H*                                                               *
     H*  05/14/98 C:90 PROJECT: WC577                                 *
     H*                CHANGED F7 SEARCH INTO F10 SEARCH              *
     H*                REMOVED F1 SEARCH-SCR02, PC ON SCYR - SCR01    *
     H*                                                               *
     H*  01/12/98 C:85 PROJECT: 91WBD                                 *
     H*                ADDED LATE PAYMENT FLAG (EALATE, SCLAT1/2/3).  *
     H*                                                               *
     H*  12/23/97 C:85 PROJECT: WC577                                 *
     H*                CHANGED FIELDS SCPODT, SCPOD2, & SCPOD3 TO     *
     H*                DEFAULT TO CURRENT DATE. INSTEAD OF            *
     H*                USER PARM DATE.                                *
     H*                                                               *
     H*  12/18/97 C:85 PROJECT: WC577                                 *
     H*                CHANGED SEARCHES FOR SCYR, SCEMP# & SCDIV.     *
     H*                                                               *
     H*  11/03/97 C:85 PROJECT: WC764                                 *
     H*                VERSION 3 REPORT STANDARDS                     *
     H*                AND ADDED DIVISION COUNT TO SCR02.             *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                                                               *
     H* 10/27/97  C:62  PROJECT WC577                                 *
     H*                 GL/AP REFERENCE IN W0110R.                    *
     H*                                                               *
     H* 12/21/95  C:42  PROJECT WC465                                 *
     H*                 ADDED CHECK TO POST DATE FIELD TO SEE IF THE  *
     H*                 A/R TYPE IS R. IF IT IS NOT THEN USER         *
     H*                 IS NOT ALLOWED TO BACK POST DATE.  THIS IS    *
     H*                 ONLY TAKEN INTO ACCOUNT IF THE EMPLOYER IS AN *
     H*                 ESTIMATING ACCOUNT. IF THE ACCOUNT IS SELF-   *
     H*                 REPORTING THE USER CAN BACK POST THE DATE.    *
     H*                 ALL OTHER TYPES SHOULD ALLOW BACKPOSTING      *
     H*                 ONLY IF POST DATE IS IN THE CURRENT MONTH.    *
     H*                                                               *
     H*  03/01/95 C:24 PROJECT WC311                                  *
     H*                REQUIRE ENTRY OF CHECK NUMBER FOR DEDUCTIBLE   *
     H*                PAYMENTS.                                      *
     H*
     H*  08/31/93 C:11 PROJECT WC209                                  *
     H*                ALLOW UP TO THREE TRANSACTIONS TO BE POSTED AT *
     H*                ONE TIME. CMD 5 OF SCR02 SHOULD PULL IN THE    *
     H*                POSTING DATE ALONG WITH AMOUNT, AND IT SHOULD  *
     H*                PLACE THE INFO ON THE REQUESTED FIELDS.        *
     H*                                                               *
     H*  05/06/92 JJJ  PROJECT WC144                                  *
     H*                CHECK FOR ACTIVE AGT AS OF SYS DT, NOT POST DT *
     H*                                                               *
     H* 04-09-92 KRR  ADDED CHECK TO POST DATE FIELD TO SEE IF THE    *
     H* PROJ#:  WC130   A/R TYPE IS P,Q OR I. IF IT IS NOT THEN USER  *
     H*                 IS NOT ALLOWED TO BACK POST DATE.  THIS IS    *
     H*                 ONLY TAKEN INTO ACCOUNT IF THE EMPLOYER IS AN *
     H*                 ESTIMATING ACCOUNT. IF THE ACCOUNT IS SELF-   *
     H*                 REPORTING THE USER CAN BACK POST THE DATE.    *
     H*                                                               *
     H* 10/09/91 DVM  ADDED CODE TO POSITION CURSOR ON EMPLOYER       *
     H* PROJ#: WC076    AFTER RETURNING FROM SCR02.                   *
     H*
     H* 09/30/91 DVM  ADDED EMPLOYER SEARCH TO SCR01.                 *
     H* PROJ#: WC076                                                  *
     H*                                                               *
     H* 09/24/90 KWK  ADDED EDIT TO ONLY ALLOW POSTING FOR EMPLOYERS  *
     H*               WITH AGENTS ACTIVE AS OF THE POSTING DATE.      *
     H*               ALSO, ADDED CODE TO ASSURE UNIQUE DATE AND      *
     H*               TIMES WILL BE ASSIGNED IF TWO POSTINGS ARE MADE *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* CALLS TO:                                                     *
     H*   AP002R - AP/GL API THAT CALLS GL INTERFACE.                 *
     H*  W0045R02 - PREMIUM FINANCE CO. DETAIL DISPLAY
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*     SCR01:                                                    *
     H*          30 - POSITION CURSOR AT EMPLOYER #                   *
     H*          31 - POSITION CURSOR AT TRANSACTION CODE #1          *
     H*          32 - POSITION CURSOR AT TRANSACTION CODE #2          *
     H*     27 - INVALID CODE FOR TRANSACTION #3                      *
     H*     33 - POSITION CURSOR AT TRANSACTION CODE #3               *
     H* SCR02:                                                        *
     H*     36 - DON'T HAVE THIRD TRANSACTION                         *
     H*     36 -                                                      *
     H*     37 - WCR0503: DESCRIPTION #3 CANNOT BE BLANK              *
     H*     38 - WCR0500: POST DATE #3 IS INVALID                     *
     H*     39 - WCR0931:                                             *
     H*     41 - WCR0556: AMOUNT #3 CANNOT BE ZERO                    *
     H*     42 - POSITION CURSOR AT CHECK #3                          *
     H*     43 - POSITION CURSOR AT POST DATE #1                      *
     H*     44 - POSITION CURSOR AT POST DATE #2                      *
     H*     45 - POSITION CURSOR AT POST DATE #3                      *
     H*     70 - INDICATES THAT AP/GL IS INSTALLED.                   *
     H*     77 - DISPLAY PREM FIN CODE AND NAME IN RI                 *
     H*     78 - DISPLAY PREM FIN ADDR IF REFUND TRANS ENTERED        *
     H*****************************************************************
     FW0110F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FWMEAL     O    E           K DISK
     FWXCNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     UF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F*** BEGIN ADD *** 12/18/97 **************************************
     F                                     INFDS(DBFDS3)
     F*** END   ADD *** 12/18/97 **************************************
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F*** BEGIN ADD *** 12/18/97 **************************************
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL03)
     F*** END   ADD *** 12/18/97 **************************************
     FWMEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F***  BEGIN ADD  ***  08/25/99  *********************************
     FWMEAL8    IF   E           K DISK
     F                                     RENAME(PEA01:PEA08)
     F***  END   ADD  ***  08/25/99  *********************************
     FWTABL     IF   E           K DISK
     F                                     RENAME(PCN01:PCN02)
     F***** BEGIN DEL **** 06/29/99 ***********************************
     F**********WDF1L   IF  E           K        DISK
     F***** END   DEL **** 06/29/99 ***********************************
      ***** BEG ADD ******* 10/01/02 **********************************
      *
      *  Group Detail
     FWDF1L     IF   E           K DISK
      ***** END ADD ******* 10/01/02 **********************************
      *
     FWMAHL     IF   E           K DISK
     F*** BEGIN ADD *** 10/27/97 **************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F*** END   ADD *** 10/27/97 **************************************
      ***** BEG ADD ******* 08/16/99 **********************************
      *
      *  Activity Status
     FWTAVL     IF   E           K DISK
      *
      ***** END ADD ******* 08/16/99 **********************************
     F*
     F***** BEGIN ADD **** 09/29/99 ***********************************
     F* Logical for Premium Finance Co.
     F*
     FWMPFL     IF   E           K DISK    INFDS(DBFDSF)
     F*
     F***** END   ADD **** 09/29/99 ***********************************

     D MG              S             40    DIM(3) CTDATA PERRCD(1)              MESSAGES
     D**** BEGIN ADD ****** 06/26/98 **********************************
     D**** END   ADD ****** 06/26/98 **********************************
     D***  BEGIN ADD  ***  08/25/99  *********************************
     D TYP             S              1    DIM(3)                               A/R TYPES
     D AMT             S              9  2 DIM(3)                               A/R AMOUNTS
     D SVA             S              9  2 DIM(3)                               SAVE A/R AMOUNTS
     D AMT2            S              9  2 DIM(3)                               SCREEN A/R AMOUNTS
     D*
     D OPTNM           C                   CONST('Post Cash & Adjustme-
     D                                     nts')
     D***  END   ADD  ***  08/25/99  *********************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*** BEGIN ADD *** 12/18/97 **************************************
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*** END   ADD *** 12/18/97 **************************************
     D***** BEGIN ADD **** 09/29/99 ***********************************
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D***** END   ADD **** 09/29/99 ***********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXDATA                 1     80
     D  XXTYPE                 1      1
     D  XXCO#                  2      4
     D  XXUSER                 5     14
     D  XXTIME                15     20
     D  XXDATE                21     28
     D  UPOUTQ                29     38
     D*
     D                 DS
      *** BEG CHG *** 12/17/01 *************************************************
     D*************************************** 1   80SCPODT
     D*************************************** 1   20XXMOTH
     D*************************************** 3   40XXDAYT
     D*************************************** 7   80XXYEAR
     D  SVPODT                 1      8  0
     D  SVMOTH                 1      2  0
     D  SVDAY                  3      4  0
     D  SVYEAR                 7      8  0
      *** END CHG *** 12/17/01 *************************************************
     D**** BEGIN ADD ****** 06/26/98 **********************************
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D**** END   ADD ****** 06/26/98 **********************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D***  Begin Add  ***  12/08/20  *********************
     D*** Beg Chg *** 03/25/21 ***************************************
     D***isDoneWC2663     s              1n   inz('0')
     DisDoneWC2663     s              1n   inz('1')
     D*** End Chg *** 03/25/21 ***************************************

     D/copy sPrGenPop
     D***  End   Add  ***  12/08/20  *********************

     C*
     C*   KEY LIST FOR THE A/R TRANSACTION TYPE TABLE FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      ***** BEG ADD ******* 08/16/99 **********************************
      *
      *  Activity Status
      *
     C     AVKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    K1KEY            10
      *
      ***** END ADD ******* 08/16/99 **********************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE CANCELLATION NOTICE.
     C*
     C     CXKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    DATE
     C*
     C*  POLICY YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*   EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C***  BEGIN ADD  ***  08/25/99  *********************************
     C*
     C     EAKY8         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C***  END   ADD  ***  08/25/99  *********************************
     C*
     C*   KEY LIST FOR EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
     C*
     C     ELKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
      ***** BEG CHG ******* 08/16/99 **********************************
     C*************        KFLD           XXFYR
     C                   KFLD                    X1FYR
      ***** END CHG ******* 08/16/99 **********************************
     C                   KFLD                    SCEMP#
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR AGENT HISTORY FILE
     C*
     C     AHKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPODT
     C*** BEGIN ADD *** 10/27/97 **************************************
     C*
     C*   KEY LIST FOR G/L SYSTEM CONTROL FILE
     C*
     C     GLKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*** END   ADD *** 10/27/97 **************************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  PREMIUM FINANCE CO. KEY
     C*
     C     PFKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCFCOD
     C*
     C     PFKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCFCOD
     C                   KFLD                    XXPFDT
     C*
     C***** END   ADD **** 09/29/99 ***********************************
     C*
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EADT          XXDT
     C     *LIKE         DEFINE    EADT          DATE
     C*** BEGIN ADD *** 10/27/97 **************************************
     C     *LIKE         DEFINE    GLKEY         KKKEY
     C*** END   ADD *** 10/27/97 **************************************
     C*
     C*** BEGIN ADD *** 12/18/97 **************************************
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*** END   ADD *** 12/18/97 **************************************
      ***** BEGIN ADD ***** 11/13/98 **********************************
     C     *LIKE         DEFINE    Q2RECF        XXSCR
      ***** END   ADD ***** 11/13/98 **********************************
      ***** BEG ADD ******* 08/16/99 **********************************
     C     *LIKE         DEFINE    ELFYR         X1FYR
      ***** END ADD ******* 08/16/99 **********************************
     C***  BEGIN ADD  ***  08/25/99  *********************************
     C     *LIKE         DEFINE    EAPODT        SYSDT7
     C     *LIKE         DEFINE    EAPODT        XXASOF
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
     C     *LIKE         DEFINE    SVBAL         XXBAL
     C     *LIKE         DEFINE    Q@CALL        @#CALL
     C***  END   ADD  ***  08/25/99  *********************************
     C***** BEGIN ADD **** 09/29/99 **********************************
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C***** END   ADD **** 09/29/99 **********************************
     C***  BEGIN ADD  ***  10/19/00  *********************************
     C     *LIKE         DEFINE    SCEMP#        SVEMP#
     C     *LIKE         DEFINE    SCEMP#        SVDIV
     C     *LIKE         DEFINE    SCYR          SVFYR
     C***  END   ADD  ***  10/19/00  *********************************
     C*
     C*  DEFINE DIVISION COUNT
     C*
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
     C*
     C*** BEGIN ADD *** 12/23/97 **************************************
     C                   Z-ADD     0             XXPRDT            8 0          POSTING DATES
     C*** END   ADD *** 12/23/97 **************************************
     C*
     C*** BEGIN ADD *** 06/26/98 **************************************
     C*  INITIALIZE VARIABLES
     C*
     C                   Z-ADD     0             SCCNDT                         CANCEL DATE
     C*
     C*** END   ADD *** 06/26/98 **************************************
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C***  BEGIN CHG  ***  08/25/99  *********************************
     C**********           PARM           QQCALL  1
     C                   PARM                    Q@CALL            1
     C***  BEGIN CHG  ***  08/25/99  *********************************
     C*** BEGIN ADD *** 10/27/97 **************************************
     C*
     C*   PARAMETER LIST FOR API AP002R
     C*
     C     AP2PL         PLIST
     C                   PARM                    ##CO#             3            COMPANY
     C                   PARM                    ##GRP             3            GROUP
     C                   PARM                    ##YEAR            3            YEAR
     C                   PARM                    ##EMP#            9            EMPLOYER
     C                   PARM                    ##DIV             5            DIVISION
     C                   PARM                    ##CODE            2            A/R CODE
     C                   PARM                    ##TYPE            1            A/R TYPE
     C                   PARM                    ##DESC           30            DESCRIPTION
     C                   PARM                    ##AMT            11            AMOUNT
     C                   PARM                    ##TRDT            7            TRANS DATE
     C                   PARM                    ##PODT            8            POST DATE
     C                   PARM                    ##TIME            6            TIME
     C*** END   ADD *** 10/27/97 **************************************
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C***** BEGIN DEL **** 06/29/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 06/29/99 ***********************************
     C***  BEGIN CHG  ***  08/25/99  *********************************
     C**********           MOVE QQPRDS    SCPRDS
     C     Q1PRMS        IFGE      1
     C     QQPRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  QQPRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C***  Begin Add  ***  12/08/20  *********************
      /Free
       If isDoneWC2663;
          PopOneBtn('This function is no longer supported.':##data);
          *inlr = *on;
          Return;
       Endif;
      /End-free
     C***  End   Add  ***  12/08/20  *********************
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   CLEAR                   @#CALL
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVEL(P)  Q@CALL        @#CALL
     C                   ENDIF                                                  Q1PRMS>=2
     C***  END   CHG  ***  08/25/99  *********************************
     C*
     C*   GET THE USER PARAMETER FILE
     C*
     C*   GET THE BASE USER DEFAULT VALUES
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C                   Z-ADD     UPFYR         SCYR
     C***  BEGIN ADD  ***  10/19/00  *********************************
     C                   EXSR      CLRSAV
     C***  END   ADD  ***  10/19/00  *********************************
     C*** BEGIN CHG *** 12/18/97 **************************************
     C**********           MOVE '1'       *IN30            ADDED 09/30/91
      **** BEGIN CHG **** 05/14/98 ************************************
     C*******************  MOVE *ON       *IN31            SCARCD POSITION
     C*** END   CHG *** 12/18/97 **************************************
     C                   MOVE      *ON           *IN40                          PC SCYR
      **** END   CHG **** 05/14/98 ************************************
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C***** BEGIN DEL **** 06/29/99 ***********************************
     C* * BEGIN ADD *** 01/12/98 **************************************
     C**********           MOVE *BLANK    SCLAT1
     C**********           MOVE *BLANK    SCLAT2
     C**********           MOVE *BLANK    SCLAT3
     C* * END   ADD *** 01/12/98 **************************************
     C***** END   DEL **** 06/29/99 ***********************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C***** BEGIN ADD **** 06/29/99 ***********************************
     C                   MOVE      *BLANK        SCLAT1
     C                   MOVE      *BLANK        SCLAT2
     C                   MOVE      *BLANK        SCLAT3
     C***** END   ADD **** 06/29/99 ***********************************
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C                   Select
     C*
     C*  F3=Exit
     C*
     C     *INKC         Wheneq    '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C*  F12=Cancel
     C*
     C     *INKL         Wheneq    '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN30
     C                   END
     C                   GOTO      STRT
     C*
     C*  F7=FINANCE INFO
     C*
     C     *INKG         Wheneq    *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C                   CALL      'W0045R02'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM      DBRRNF        @@RRN             9 0          WMPFP
     C                   PARM      DBRRN         @@RRN2            9 0          WMFNP
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  F10 = A/R SEARCH - SCREEN 2   *
     C     *INKJ         Wheneq    '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C     *INKG         OREQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR01'
     C                   MOVE      Q2RECF        XXSCR
     C     Q2RECF        IFEQ      'SCR01'
     C     SCYR          IFEQ      *ZEROS
     C                   MOVE      *ON           *IN56
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCYR=ZEROS
     C     SCEMP#        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN57
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEMP#=ZEROS
     C     SCARCD        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN58
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  SCARCD=ZEROS
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      UPFND         QQFND
     C                   MOVE      SCYR          QQYR1
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      '2'           QQCALL
     C*
     C                   CALL      'W0612R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQYR1             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQAMT             9
     C                   PARM                    QQCHK#            7
     C                   PARM                    QQRTC             1
     C*
     C*
     C     *INKL         IFEQ      *ON
     C     QQDATE        OREQ      *BLANKS
     C     Q2RECF        OREQ      'SCR01'
     C                   MOVEL     XXSCR         Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C* CHECK TO SEE WHICH AMOUNT FIELD TO MOVE INFORMATION TO
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCDESC'
     C     HFLD          OREQ      'SCPODT'
     C     HFLD          OREQ      'SCAMT'
     C     HFLD          OREQ      'SCCHK#'
     C     HFLD          OREQ      'SCLAT1'
     C                   MOVE      QQDATE        SCPODT
     C                   MOVE      QQAMT         SCAMT
     C                   MOVE      QQCHK#        SCCHK#
     C     QQAMT         COMP      *BLANK                             272743
     C     HFLD          WHENEQ    'SCDES2'
     C     HFLD          OREQ      'SCPOD2'
     C     HFLD          OREQ      'SCAMT2'
     C     HFLD          OREQ      'SCCHK2'
     C     HFLD          OREQ      'SCLAT2'
     C                   MOVE      QQDATE        SCPOD2
     C                   MOVE      QQAMT         SCAMT2
     C                   MOVE      QQCHK#        SCCHK2
     C     QQAMT         COMP      *BLANK                             282844
     C     HFLD          WHENEQ    'SCDES3'
     C     HFLD          OREQ      'SCPOD3'
     C     HFLD          OREQ      'SCAMT3'
     C     HFLD          OREQ      'SCCHK3'
     C     HFLD          OREQ      'SCLAT3'
     C                   MOVE      QQDATE        SCPOD3
     C                   MOVE      QQAMT         SCAMT3
     C                   MOVE      QQCHK#        SCCHK3
     C     QQAMT         COMP      *BLANK                             424245
     C                   ENDSL
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  F10 - SEARCH
     C     *INKJ         Wheneq    *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 - TABLE FILE SEARCH - POLICY YEAR
     C*
     C     HFLD          IFEQ      'SCYR'
     C*
     C                   MOVE      *ON           *IN40                          SCYR POSITION
     C*
     C*  EMPLOYER NUMBER EQUAL TO ZERO
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C                   ENDIF                                                  SCEMP#=0
     C*
     C*  EMPLOYER NUMBER NOT EQUAL TO ZERO
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKEY         CHAIN(N)  PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     *ZERO         @1RRN
     C                   Z-ADD     DBRRN3        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCYR
     C*
     C                   MOVE      *OFF          *IN40                          SCYR POSITION
     C                   MOVE      *ON           *IN31                          SCARCD POSITION
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  SCEMP#<>0
     C*
     C                   ENDIF                                                  HFLD=SCYR
     C*
     C*  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      *ON           *IN30                          SCEMP# POSITION
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   MOVE      *OFF          *IN30                          SCEMP# POSITION
     C                   MOVE      *ON           *IN31                          SCARCD POSITION
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C*  AR SEARCH
     C*
     C     HFLD          IFEQ      'SCARCD'
     C*
     C                   MOVE      *ON           *IN31                          SCARCD POSITION
     C                   MOVE      *BLANKS       SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C*
     C     *INKL         IFEQ      *OFF
     C     SCKEY         ANDNE     *BLANKS
     C                   MOVEL     SCKEY         SCARCD
     C                   MOVE      *OFF          *IN31                          SCARCD POSITION
     C                   MOVE      *ON           *IN32                          SCARC2 POSITION
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD SCARCD
     C*
     C     HFLD          IFEQ      'SCARC2'
     C*
     C                   MOVE      *ON           *IN32                          SCARC2 POSITION
     C                   MOVE      *BLANKS       SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     SCKEY         ANDNE     *BLANKS
     C                   MOVEL     SCKEY         SCARC2
     C                   MOVE      *OFF          *IN32                          SCARC2 POSITION
     C                   MOVE      *ON           *IN33                          SCARC3 POSITION
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD SCARC2
     C*
     C     HFLD          IFEQ      'SCARC3'
     C*
     C                   MOVE      *ON           *IN33                          SCARC3 POSITION
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     SCKEY         ANDNE     *BLANKS
     C                   MOVEL     SCKEY         SCARC3
     C                   MOVE      *OFF          *IN33                          SCARC3 POSITION
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD SCARC3
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         Wheneq    '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         Wheneq    '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
     C*   CMD 15 - PAYROLL MAINTENANCE - SCR02
     C*
     C     *INKP         Wheneq    '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      SCYR          XXFYR
     C*
     C     ELKEY         CHAIN     PEL01                              99
     C*
     C                   MOVE      '2'           XXCALL            1
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C*
     C                   CALL      'W0060R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      ' '           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN1        @@RRN2            9 0
     C                   PARM      'N'           @@BILL            1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   GOTO      STRT
      *
     C*   SUBMIT THE EDIT REQUEST
     C*
     C     *INKR         Wheneq    '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      'B'           XXTYPE
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        XXUSER
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      DSTIME        XXTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   CALL      'QSNDDTAQ'
     C                   PARM      'WEDITQ'      XXQNM            10
     C                   PARM      '*LIBL'       XXLIB            10
     C                   PARM      80            XXFLDL            5 0
     C                   PARM                    XXDATA
     C*
     C                   MOVE      MG(1)         SCMSG
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   Endsl
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C                   MOVE      '0'           *IN30
     C                   MOVE      *BLANK        SCMSG
     C*
     C*  BEGIN CALCULATION FOR DIVISION COUNT
     C*
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
     C                   Z-ADD     SCYR          X1FYR
     C*
     C     ELKY1         CHAIN     PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 = OFF
     C*
     C*
     C*  END CALCULATION FOR DIVISION COUNT
     C*
     C***** BEGIN DEL **** 06/29/99 ***********************************
     C********** F1KEY     CHAINPF101                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE '1'       *IN20
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C***** END   DEL **** 06/29/99 ***********************************
     C*
     C*  EMPLOYER AND DIVISION MUST EXIST IN EMPLOYER MASTER
     C*
     C     EMKEY         CHAIN     PEM02                              21
     C*
     C     *IN21         CABEQ     '1'           STRT
     C                   EXCEPT    PEM02E
     C*
     C***** BEGIN DEL **** 06/29/99 ***********************************
     C** STATUS CODE MUST BE 'A'-ACTIVE
     C**********
     C********** EMSTAT    CABEQ'I'       STRT           21
     C***** END   DEL **** 06/29/99 ***********************************
     C*
     C                   MOVE      EMNAME        SCENAM
     C*
     C***** BEG DEL ******* 07/07/05 **********************************
      ***** BEG ADD ******* 08/16/99 **********************************
     C********************
     C***************  Get activity status description
     C********************
     C******************** MOVELEMASTS    K1KEY     P
     C***********AVKY***** CHAINPAV01                60
     C**N60*************** MOVELAVDESC    SCAVDS    P
     C***60*************** MOVEL*BLANKS   SCAVDS    P
     C********************
      ***** END ADD ******* 08/16/99 **********************************
     C***** END DEL ******* 07/07/05 **********************************
     C*
     C***** BEGIN ADD **** 06/29/99 ***********************************
     C*  VALIDATE POLICY YEAR FOR CO#/FND/EMP#/DIV
     C*
     C                   MOVE      SCYR          XXFYR
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN51
     C                   GOTO      STRT
      ***** BEG ADD ******* 08/16/99 **********************************
      *
     C                   ELSE
      *
     C                   MOVE      ELCTST        SCCTST
     C***** BEG ADD ******* 07/07/05 **********************************
     C*
     C*  Get activity status description
     C*
     C                   MOVEL(P)  ELASCD        K1KEY
     C     AVKY          CHAIN     PAV01                              60
     C  N60              MOVEL(P)  AVDESC        SCAVDS
     C   60              MOVEL(P)  *BLANKS       SCAVDS
     C*
     C***** END DEL ******* 07/07/05 **********************************
      *
      ***** END ADD ******* 08/16/99 **********************************
     C                   ENDIF                                                  *IN60=ON
     C***** END   ADD **** 06/29/99 ***********************************
     C*
     C*  EMPLOYER MUST HAVE ASSIGNED AGENT
     C*
     C     AHKEY         CHAIN     PAH01                              55
     C     *IN55         DOWEQ     '0'
     C     AHSTAT        ANDNE     'A'
     C     AHKEY         READE     PAH01                                  55
     C                   END
     C     *IN55         CABEQ     '1'           STRT                     24
     C*
     C*  TRANSACTION CODE MUST EXIST AND BE ACTIVE
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     SCARCD        XXKEY
     C     ARKEY         CHAIN     PTT01                              22
     C     *IN22         CABEQ     '1'           STRT
     C     *IN22         IFEQ      '0'
     C     TTSTAT        ANDEQ     'I'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   MOVE      TTDESC        SCDESC
     C                   MOVE      TTTYPE        T1TYPE            1
      *** BEG ADD ***** 11/04/03 *********************************
     C                   MOVE      TTCTYP        T1CTYP            1
      *** END ADD ***** 11/04/03 *********************************
     C*
     C*
     C     SCARC2        IFNE      *BLANKS
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     SCARC2        XXKEY
     C     ARKEY         CHAIN     PTT01                              26
     C     *IN26         CABEQ     '1'           STRT
     C     *IN26         IFEQ      '0'
     C     TTSTAT        ANDEQ     'I'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      TTDESC        SCDES2
     C                   MOVE      TTTYPE        T2TYPE            1
      *** BEG ADD ***** 11/04/03 *********************************
     C                   MOVE      TTCTYP        T2CTYP            1
      *** END ADD ***** 11/04/03 *********************************
     C                   MOVE      '0'           *IN35
     C                   ELSE
     C                   MOVE      *BLANKS       T2TYPE            1
     C                   MOVE      *BLANKS       SCDES2
     C                   MOVE      '1'           *IN35
     C                   END
     C*  THIRD TRANSACTION CODE MUST EXIST AND BE ACTIVE, IF ENTERED
     C*
     C     SCARC3        IFNE      *BLANKS
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     SCARC3        XXKEY
     C     ARKEY         CHAIN     PTT01                              27
     C     *IN27         CABEQ     '1'           STRT
     C     *IN27         IFEQ      '0'
     C     TTSTAT        ANDEQ     'I'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     TTDESC        SCDES3
     C                   MOVE      TTTYPE        T3TYPE            1
      *** BEG ADD ***** 11/04/03 *********************************
     C                   MOVE      TTCTYP        T3CTYP            1
      *** END ADD ***** 11/04/03 *********************************
     C                   MOVE      '0'           *IN36
     C                   ELSE
     C                   MOVE      *BLANK        T3TYPE            1
     C                   MOVE      *BLANK        SCDES3
     C                   MOVE      '1'           *IN36
     C                   END
     C***  BEGIN ADD  ***  10/19/00  *********************************
     C*
     C*  IF THE POLICY IS ACCOUNT CURRENT, DISPLAY MESSAGE
     C*
     C                   CALL      'WA002R'
     C                   PARM      UPCO#         @@CO#             3 0
     C                   PARM      SCFND         @@FND             3 0
     C                   PARM      SCEMP#        @@EMP#            9 0
     C                   PARM      SCDIV         @@DIV             5 0
     C                   PARM      SCYR          @@FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    @@AGT#            5 0
     C***********        PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM      '*'           @@PLAN            1
     C                   PARM      *ZERO         @@DATE            8 0
     C*
     C     @@PLAN        IFEQ      'A'
     C*
     C     SCEMP#        IFNE      SVEMP#
     C     SCDIV         ORNE      SVDIV
     C     SCYR          ORNE      SVFYR
     C*
     C     T1TYPE        IFEQ      'P'
     C     T1TYPE        OREQ      'I'
     C     T1TYPE        OREQ      'Q'
     C     T2TYPE        OREQ      'P'
     C     T2TYPE        OREQ      'I'
     C     T2TYPE        OREQ      'Q'
     C     T3TYPE        OREQ      'P'
     C     T3TYPE        OREQ      'I'
     C     T3TYPE        OREQ      'Q'
     C                   SETON                                        87
     C                   Z-ADD     SCEMP#        SVEMP#
     C                   Z-ADD     SCDIV         SVDIV
     C                   Z-ADD     SCYR          SVFYR
     C                   GOTO      STRT
     C                   ENDIF                                                  T1TYPE=P/I/Q
     C*
     C                   ENDIF                                                  SCEMP#<>SVEMP#
     C*
     C                   ENDIF                                                  @@PLAN=A
     C*
     C                   MOVE      *ON           CKPLAN            1
     C***  END   ADD  ***  10/19/00  *********************************
     C*
     C*  TEST EMPLOYER FOR FLAGGED FOR CANCELLATION.
     C*
     C                   MOVE      *HIVAL        DATE
     C     CXKEY         SETGT     PCN01
     C                   READP     PCN01                                  58
     C  N58CNCO#         IFEQ      UPCO#
     C     CNFND         ANDEQ     UPFND
     C     CNEMP#        ANDEQ     SCEMP#
     C     CNDIV         ANDEQ     SCDIV
     C     CNSELC        ANDEQ     'Y'
     C     CNSTAT        ANDEQ     ' '
     C     DSPLY         ANDEQ     ' '
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           DSPLY             1
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        DSPLY
     C*
     C*  CALCULATE EXPENSE CONSTANT DUE
     C*
     C                   Z-ADD     0             SCECDU           11 2
     C     EAKEY         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'E'
     C*
     C     TTTYPE        IFEQ      'B'
      **** BEG ADD **** 04/26/02 *************************************
     C     TTTYPE        OREQ      'R'                                          REFUND CODE
      **** END ADD **** 04/26/02 *************************************
     C                   ADD       EAAMT         SCECDU
     C                   ENDIF
     C     TTTYPE        IFEQ      'P'
     C                   SUB       EAAMT         SCECDU
     C                   ENDIF
     C                   ENDIF                                                  *IN60 IFEQ
     C     EAKEY         READE     PEA02                                  60
     C                   END
     C*
     C*  CLEAR FIELDS FOR SCREEN 2
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEGIN CHG *** 12/23/97 **************************************
     C***CHANGE TO USE USER PARAMETERS DATE
     C**********           Z-ADDUPPRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     XXPRDT  80
     C*
     C*  CHANGE TO USE SYSTEM DATE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXPRDT
     C*** END   CHG *** 12/23/97 **************************************
     C*
     C***  BEGIN ADD  ***  08/25/99  *********************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT7
     C***  END   ADD  ***  08/25/99  *********************************
     C*
     C                   Z-ADD     XXPRDT        SCPODT
     C                   MOVE      *BLANKS       SCAMT
     C                   MOVE      *BLANKS       SCCHK#
     C*
     C                   Z-ADD     XXPRDT        SCPOD2
     C                   MOVE      *BLANKS       SCAMT2
     C                   MOVE      *BLANKS       SCCHK2
     C*
     C                   Z-ADD     XXPRDT        SCPOD3                         THIRD TRANSACTION
     C                   Z-ADD     *ZERO         SCAMT3
     C                   Z-ADD     *ZERO         SCCHK3
     C                   MOVE      '1'           *IN43                          PUT CURSOR @ SCPODT
     C*
     C****** BEGIN ADD ***** 06/26/98 *********************************
     C*
     C*  DISPLAY CANCEL DATE(IF APPLICABLE)
     C*
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
     C*
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    CD(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C     Y             IFEQ      0
     C     CD(Y)         ORLT      ED(X)
     C*
     C                   MOVE      *OFF          *IN71                          ACTIVE
     C                   ELSE
     C                   MOVE      *ON           *IN71                          CANCELLED
     C*
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C*
     C***  BEGIN ADD  ***  08/25/99  *********************************
     C                   Z-ADD     CD(Y)         XXCXDT
     C***  END   ADD  ***  08/25/99  *********************************
     C*
     C                   END
     C*
     C***  BEGIN ADD  ***  08/25/99  *********************************
     C*
     C*  CALCULATE BALANCE
     C*
     C     XXCXDT        IFLE      SYSDT7
     C                   Z-ADD     XXCXDT        XXASOF
     C                   ELSE
     C                   Z-ADD     SYSDT7        XXASOF
     C                   ENDIF                                                  XXCXDT<=SYSDT7
     C*
     C                   CLEAR                   SVBAL
     C*
     C     EAKY8         CHAIN     PEA08                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EAPODT        ANDLE     SYSDT7
     C*
     C                   SELECT
     C***  BEGIN DEL  ***  10/21/99  *********************************
     C********** EAARTY    WHEQ 'B'
     C********** EAPODT    ANDLEXXASOF
     C********** EAARTY    OREQ 'A'
     C********** EAPODT    ANDLEXXASOF
     C**********           ADD  EAAMT     SVBAL  112
     C***  END   DEL  ***  10/21/99  *********************************
     C     EAARTY        WHENEQ    'R'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'D'
     C***  BEGIN ADD  ***  10/21/99  *********************************
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'A'
     C***  END   ADD  ***  10/21/99  *********************************
     C***  BEGIN ADD  ***  01/24/00  *********************************
     C     EAARTY        OREQ      'V'
     C***  END   ADD  ***  01/24/00  *********************************
     C***  BEGIN CHG  ***  10/21/99  *********************************
     C**********           ADD  EAAMT     SVBAL
     C                   ADD       EAAMT         SVBAL            11 2
     C***  END   CHG  ***  10/21/99  *********************************
     C     EAARTY        WHENEQ    'P'
     C***  BEGIN DEL  ***  01/24/00  *********************************
     C********** EAARTY    OREQ 'V'
     C***  END   DEL  ***  01/24/00  *********************************
     C***  BEGIN ADD  ***  02/19/04  *********************************
     C     EAARTY        OREQ      'T'
     C***  END   ADD  ***  02/19/04  *********************************
     C                   SUB       EAAMT         SVBAL
     C                   ENDSL
     C*
     C     EAKY8         READE     PEA08                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      T1TYPE        TYP(1)
     C                   MOVE      T2TYPE        TYP(2)
     C                   MOVE      T3TYPE        TYP(3)
     C*
     C                   Z-ADD     SCAMT         AMT(1)
     C                   Z-ADD     SCAMT2        AMT(2)
     C                   Z-ADD     SCAMT3        AMT(3)
     C*
     C                   MOVEA     AMT           SVA
     C***  END   ADD  ***  08/25/99  *********************************
     C*
     C****** END   ADD ***** 06/26/98 *********************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  IF A PREMIUM FINANCE CO. IS ASSIGNED TO THE POLICY LOAD CODE
     C*
     C                   MOVE      *OFF          *IN77
     C                   MOVE      *OFF          *IN78
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     ELPFIN        SCFCOD
     C                   MOVEL     ELPFDT        XXPFDT
     C*
     C     SCFCOD        IFNE      *BLANKS
     C                   MOVE      *ON           *IN77
     C*
     C     PFKY2         SETGT     PPF01
     C*
     C     PFKY          READPE    PPF01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PFNAME        SCPFNM
     C                   MOVEL     PFNAME        SCFNAM
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  If a refund transaction type is entered, display premium
     C*    finance company name and address at bottom of screen
     C*
     C     T1TYPE        IFEQ      'R'
     C                   MOVE      *ON           *IN78
     C                   ELSE
     C*
     C     T2TYPE        IFEQ      'R'
     C                   MOVE      *ON           *IN78
     C                   ELSE
     C*
     C     T3TYPE        IFEQ      'R'
     C                   MOVE      *ON           *IN78
     C                   ENDIF                                                  T3TYPE=R
     C                   ENDIF                                                  T2TYPE=R
     C                   ENDIF                                                  T1TYPE=R
     C*
     C     *IN78         IFEQ      *ON
     C                   MOVEL(P)  PFADR1        SCADR1
     C                   MOVEL(P)  PFADR2        SCADR2
     C                   MOVEL(P)  PFZIP         XXZIP1            5
     C                   MOVE(P)   PFZIP         XXZIP2            4
     C*
     C*  Concatenate City, ST  Zip-ext
     C*
     C                   MOVE      *BLANKS       SCADR3
     C     PFCTY         CAT       ',':0         SCADR3
     C                   CAT       PFST:1        SCADR3
     C                   CAT       XXZIP1:1      SCADR3
     C     XXZIP2        IFNE      *ZEROS
     C                   CAT       '-':0         SCADR3
     C                   CAT       XXZIP2:0      SCADR3
     C                   ENDIF                                                  XXZIP2<>0
     C*
     C*  If there is no Address line 2 move Address line 3 up
     C*
     C     SCADR2        IFEQ      *BLANKS
     C                   MOVEL     SCADR3        SCADR2
     C                   MOVE      *BLANKS       SCADR3
     C                   ENDIF                                                  SCADR2=BLK
     C*
     C                   ENDIF                                                  *IN78=ON
     C*
     C                   ENDIF                                                  *SCFCOD<>BLK
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C***** END   ADD **** 09/29/99 ***********************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      *BLANK        SCMSG1
     C***  BEGIN ADD  ***  08/25/99  *********************************
     C                   Z-ADD     SVBAL         XXBAL
     C                   CLEAR                   AMT2
     C***  END   ADD  ***  08/25/99  *********************************
     C*** BEG DEL **** 12/17/01 **************************************
     C*********************
     C***DESCRIPTION*CANNOT BE BLANK
     C*********************
     C***********SCDESC****CABEQ*BLANK    STRT           20
     C*********************
     C****BEGIN*ADD*****01/12/98 **************************************
     C*********************
     C***LATE*PAYMENT*FLAG*MUST BE 'Y' OR BLANK
     C*********************
     C***********SCLAT1****IFNE 'Y'
     C***********SCLAT1****ANDNE*BLANK
     C*********************MOVE *ON       *IN46            ERR-SCLAT1<>Y/b
     C*********************GOTO STRT
     C*********************ENDIF                           SCLAT1<>'Y'/' '
     C*********************
     C****END***ADD*****01/12/98 **************************************
     C*********************
     C***IF*THE*A/R*CODE*IS FOR A DEDUCTIBLE PAYMENT, AS DEFINED IN
     C***THE*GROUP*DETAIL,*THE CHECK NUMBER IS REQUIRED
     C*********************
     C*********************MOVELSCARCD    XXKEY     P
     C***********TBKEY*****CHAINPTT01                60
     C************IN60*****IFEQ *OFF
     C*********************
     C***********TTCTYP****IFEQ 'R'
     C***********TTTYPE****ANDEQ'Q'
     C***********SCCHK#****ANDEQ*ZERO
     C*********************MOVE *ON       *IN63
     C*********************GOTO STRT
     C*********************ENDIF
     C*********************ENDIF
     C*********************
     C***POSTING*DATE*MUST*BE VALID
     C*********************
     C*********************Z-ADDSCPODT    SRDTI
     C*********************CALL 'SRCVTDAT'DATCHK
     C*********************MOVELSRDTO     SCPODT
     C*********************
     C***********SRERR*****IFEQ '1'
     C*********************MOVE '1'       *IN21
     C*********************GOTO STRT
     C*********************END
     C*********************
     C*********************Z-ADDDSDATE    SRDTI
     C*********************CALL 'SRCVTDAT'DTCYMD
     C*********************MOVELSRDTO     XXDT
     C*********************
     C*********************Z-ADDSCPODT    SRDTI
     C*********************CALL 'SRCVTDAT'DTCYMD
     C*********************MOVELSRDTO     XXPODT
     C*********************
     C***********ELKEY*****CHAINPEL01                50
     C************IN50*****IFEQ '0'
     C*********************MOVE *BLANK    XXKEY
     C*********************MOVELELBSCD    XXKEY
     C***********TBKEY*****CHAINPBS01                50
     C************IN50*****IFEQ '0'
     C***********BSSEFL****IFEQ 'E'
     C***********T1TYPE****IFNE 'P'
     C***********T1TYPE****ANDNE'I'
     C***********T1TYPE****ANDNE'Q'
     C***********T1TYPE****ANDNE'R'
     C****BEGIN*ADD*****05/10/01 **************************************
     C***********T1TYPE****ANDNE'N'
     C****END***ADD*****05/10/01 **************************************
     C***********XXPODT****IFLT XXDT
     C***********XXYEAR****IFNE UYEAR
     C***********XXMOTH****ORNE UMONTH
     C*********************MOVE '1'       *IN33
     C*********************GOTO STRT
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************
     C***EMPLOYER*MUST*HAVE ASSIGNED AGENT EFFECTIVE FOR THE POLICY
     C***YEAR*EFFECTIVE*AS*OF THE POSTING DATE
     C*********************
     C*********************
     C*********************Z-ADDDSDATE    SRDTI
     C*********************CALL 'SRCVTDAT'DTCYMD
     C*********************MOVELSRDTO     XXPODT
     C*********************
     C***********AHKEY2****SETGTPAH01
     C*********************READPPAH01                    55
     C************IN55*****DOWEQ'0'
     C***********AHSTAT****ANDEQ'I'
     C*********************READPPAH01                    55
     C*********************END
     C************IN55*****IFEQ '1'
     C***********AHCO#*****ORNE UPCO#
     C***********AHFND*****ORNE UPFND
     C***********AHEMP#****ORNE SCEMP#
     C***********AHDIV*****ORNE SCDIV
     C***********AHFYR*****ORNE XXFYR
     C*********************MOVE '1'       *IN29
     C*********************GOTO STRT
     C*********************END
     C*****BEGIN*ADD*******08/25/99  *********************************
     C*********************
     C***IF*THE*PAYMENT*IS*A REFUND, TYPE 'R', DISPLAY AN INFORMATIONAL
     C***MESSAGE*IF*THE*TRANSACTION WILL CAUSE THE BALANCE TO BE NEG
     C*********************
     C*********************Z-ADD1         X
     C*********************Z-ADDSCAMT     AMT,X
     C*********************
     C*********************EXSR CHKBAL
     C*********************
     C***********ERR*******CABEQ*ON       STRT
     C*****END***ADD*******08/25/99  *********************************
     C*** END DEL **** 12/17/01 ************************************************
     C*  DUPLICATE FIELDS IF INDICATED
     C*
     C     *IN28         IFEQ      '1'
     C                   MOVE      SCPODT        SCPOD2
     C                   END
     C     *IN29         IFEQ      '1'
     C                   MOVE      SCAMT         SCAMT2
     C                   END
     C     *IN30         IFEQ      '1'
     C                   MOVE      SCCHK#        SCCHK2
     C                   END
     C*          *IN31     IFEQ '1'
     C*                    MOVE SCSRMN    SCSRM2
     C*                    END
     C                   SETOFF                                       282930
     C*                    SETOF                     31
     C*** BEG DEL **** 12/17/01 ************************************************
     C***EDIT*LINE*2*TRANSACTION
     C*********************
     C***********SCARC2****IFNE *BLANK
     C***********SCDES2****CABEQ*BLANK    STRT           24
     C*********************
     C****BEGIN*ADD*****01/12/98 **************************************
     C*********************
     C***LATE*PAYMENT*FLAG*MUST BE 'Y' OR BLANK
     C*********************
     C***********SCLAT2****IFNE 'Y'
     C***********SCLAT2****ANDNE*BLANK
     C*********************MOVE *ON       *IN47            ERR-SCLAT2<>Y/b
     C*********************GOTO STRT
     C*********************ENDIF                           SCLAT2<>'Y'/' '
     C*********************
     C****END***ADD*****01/12/98 **************************************
     C*********************
     C***IF*THE*A/R*CODE*IS FOR A DEDUCTIBLE PAYMENT, AS DEFINED IN
     C***THE*GROUP*DETAIL,*THE CHECK NUMBER IS REQUIRED
     C*********************
     C*********************MOVELSCARC2    XXKEY     P
     C***********TBKEY*****CHAINPTT01                60
     C************IN60*****IFEQ *OFF
     C*********************
     C***********TTCTYP****IFEQ 'R'
     C***********TTTYPE****ANDEQ'Q'
     C***********SCCHK2****ANDEQ*ZERO
     C*********************MOVE *ON       *IN64
     C*********************GOTO STRT
     C*********************ENDIF
     C*********************ENDIF
     C*********************
     C***POSTING*DATE*MUST*BE VALID
     C*********************
     C*********************Z-ADDSCPOD2    SRDTI
     C*********************CALL 'SRCVTDAT'DATCHK
     C*********************MOVELSRDTO     SCPOD2
     C*********************
     C***********SRERR*****IFEQ '1'
     C*********************MOVE '1'       *IN25
     C*********************GOTO STRT
     C*********************END
     C*********************
     C*********************Z-ADDSCPOD2    SRDTI
     C*********************CALL 'SRCVTDAT'DTCYMD
     C*********************MOVELSRDTO     XXPODT
     C*********************
     C***********ELKEY*****CHAINPEL01                50
     C************IN50*****IFEQ '0'
     C*********************MOVE *BLANK    XXKEY
     C*********************MOVELELBSCD    XXKEY
     C***********TBKEY*****CHAINPBS01                50
     C************IN50*****IFEQ '0'
     C***********BSSEFL****IFEQ 'E'
     C***********T2TYPE****IFNE 'P'
     C***********T2TYPE****ANDNE'I'
     C***********T2TYPE****ANDNE'Q'
     C***********T2TYPE****ANDNE'R'
     C****BEGIN*ADD*****05/10/01**************************************
     C***********T2TYPE****ANDNE'N'
     C****END***ADD*****05/10/01**************************************
     C***********XXPODT****IFLT XXDT
     C***********XXYEAR****IFNE UYEAR
     C***********XXMOTH****ORNE UMONTH
     C*********************MOVE '1'       *IN34
     C*********************GOTO STRT
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************
     C***EMPLOYER*MUST*HAVE ASSIGNED AGENT EFFECTIVE FOR THE POLICY
     C***YEAR*EFFECTIVE*AS*OF THE POSTING DATE
     C*********************
     C*********************Z-ADDDSDATE    SRDTI
     C*********************CALL 'SRCVTDAT'DTCYMD
     C*********************MOVELSRDTO     XXPODT
     C*********************
     C***********AHKEY2****SETGTPAH01
     C*********************READPPAH01                    55
     C************IN55*****DOWEQ'0'
     C***********AHSTAT****ANDEQ'I'
     C*********************READPPAH01                    55
     C*********************END
     C************IN55*****IFEQ '1'
     C***********AHCO#*****ORNE UPCO#
     C***********AHFND*****ORNE UPFND
     C***********AHEMP#****ORNE SCEMP#
     C***********AHDIV*****ORNE SCDIV
     C***********AHFYR*****ORNE XXFYR
     C*********************MOVE '1'       *IN30
     C*********************GOTO STRT
     C*********************END
     C*********************END
     C*****BEGIN*ADD*******08/25/99  *********************************
     C*********************
     C***IF*THE*PAYMENT*IS*A REFUND, TYPE 'R', DISPLAY AN INFORMATIONAL
     C***MESSAGE*IF*THE*TRANSACTION WILL CAUSE THE BALANCE TO BE NEG
     C*********************
     C*********************Z-ADD2         X
     C*********************Z-ADDSCAMT2    AMT,X
     C*********************
     C*********************EXSR CHKBAL
     C*********************
     C***********ERR*******CABEQ*ON       STRT
     C*****END***ADD*******08/25/99  *********************************
     C*********************
     C****EDIT*THIRD*TRANSACTION
     C*********************
     C***********SCARC3****IFNE *BLANK
     C***********SCDES3****CABEQ*BLANK    STRT           37
     C*********************
     C****BEGIN*ADD*****01/12/98 **************************************
     C*********************
     C***LATE*PAYMENT*FLAG*MUST BE 'Y' OR BLANK
     C*********************
     C***********SCLAT3****IFNE 'Y'
     C***********SCLAT3****ANDNE*BLANK
     C*********************MOVE *ON       *IN48            ERR-SCLAT3<>Y/b
     C*********************GOTO STRT
     C*********************ENDIF                           SCLAT3<>'Y'/' '
     C*********************
     C****END***ADD*****01/12/98 **************************************
     C*********************
     C***IF*THE*A/R*CODE*IS FOR A DEDUCTIBLE PAYMENT, AS DEFINED IN
     C***THE*GROUP*DETAIL,*THE CHECK NUMBER IS REQUIRED
     C*********************
     C*********************MOVELSCARC3    XXKEY     P
     C***********TBKEY*****CHAINPTT01                60
     C************IN60*****IFEQ *OFF
     C*********************
     C***********TTCTYP****IFEQ 'R'
     C***********TTTYPE****ANDEQ'Q'
     C***********SCCHK3****ANDEQ*ZERO
     C*********************MOVE *ON       *IN65
     C*********************GOTO STRT
     C*********************ENDIF
     C*********************ENDIF
     C*********************
     C***THIRD*POSTING*DATE MUST BE VALID
     C*********************
     C*********************Z-ADDSCPOD3    SRDTI
     C*********************CALL 'SRCVTDAT'DATCHK
     C*********************MOVELSRDTO     SCPOD3
     C*********************
     C***********SRERR*****IFEQ '1'
     C*********************MOVE '1'       *IN38
     C*********************GOTO STRT
     C*********************END
     C*********************
     C***THIRD*TRANSACTION*TYPE MUST BE VALID
     C*********************
     C*********************Z-ADDSCPOD3    SRDTI
     C*********************CALL 'SRCVTDAT'DTCYMD
     C*********************MOVELSRDTO     XXPODT
     C*********************
     C***********ELKEY*****CHAINPEL01                50
     C************IN50*****IFEQ '0'
     C*********************MOVE *BLANK    XXKEY
     C*********************MOVELELBSCD    XXKEY
     C***********TBKEY*****CHAINPBS01                50
     C************IN50*****IFEQ '0'
     C***********BSSEFL****IFEQ 'E'
     C***********T3TYPE****IFNE 'P'
     C***********T3TYPE****ANDNE'I'
     C***********T3TYPE****ANDNE'Q'
     C***********XXPODT****IFLT XXDT
     C*********************MOVE '1'       *IN39
     C*********************GOTO STRT
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************END
     C*********************
     C***EMPLOYER*MUST*HAVE ASSIGNED AGENT EFFECTIVE FOR THE POLICY
     C***YEAR*EFFECTIVE*AS*OF THE POSTING DATE
     C*********************
     C*********************Z-ADDDSDATE    SRDTI
     C*********************CALL 'SRCVTDAT'DTCYMD
     C*********************MOVELSRDTO     XXPODT
     C*********************
     C***********AHKEY2****SETGTPAH01
     C*********************READPPAH01                    55
     C************IN55*****DOWEQ'0'
     C***********AHSTAT****ANDEQ'I'
     C*********************READPPAH01                    55
     C*********************END
     C************IN55*****IFEQ '1'
     C***********AHCO#*****ORNE UPCO#
     C***********AHFND*****ORNE UPFND
     C***********AHEMP#****ORNE SCEMP#
     C***********AHDIV*****ORNE SCDIV
     C***********AHFYR*****ORNE XXFYR
     C*********************MOVE '1'       *IN30
     C*********************GOTO STRT
     C*********************END
     C*********************END
     C*****BEGIN*ADD*******08/25/99  *********************************
     C*********************
     C***IF*THE*PAYMENT*IS*A REFUND, TYPE 'R', DISPLAY AN INFORMATIONAL
     C***MESSAGE*IF*THE*TRANSACTION WILL CAUSE THE BALANCE TO BE NEG
     C*********************
     C*********************Z-ADD3         X
     C*********************Z-ADDSCAMT3    AMT,X
     C*********************
     C*********************EXSR CHKBAL
     C*********************
     C***********ERR*******CABEQ*ON       STRT
     C*****END***ADD*******08/25/99  *********************************
     C*** END DEL **** 12/17/01 ***********************************************
     C*** BEG ADD **** 12/17/01 ***********************************************
     C*   ERROR CHECKING FOR SCR02
      *
     C     ELKEY         CHAIN     PEL01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              50
     C                   ENDIF
      *
      * Error check first transaction line
      *
     C                   SETON                                        91
     C                   MOVEL(P)  SCDESC        SVDESC           30
     C                   MOVEL(P)  SCLAT1        SVLATE            1
     C                   MOVEL(P)  SCPODT        SVPODT
     C                   Z-ADD     SCCHK#        SVCHK#            7 0
     C                   MOVE      T1TYPE        SVTYPE            1
     C                   MOVEL     SCARCD        SVARCD            2
     C                   EXSR      ERRCK2
     C     *IN90         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C                   SETOFF                                       91
     C                   ENDIF
      *
      * Error check second transaction line
      *
     C     *IN35         IFEQ      *OFF
     C                   SETON                                        92
     C                   MOVEL(P)  SCDES2        SVDESC
     C                   MOVEL(P)  SCLAT2        SVLATE
     C                   MOVEL(P)  SCPOD2        SVPODT
     C                   Z-ADD     SCCHK2        SVCHK#
     C                   MOVE      T2TYPE        SVTYPE
     C                   MOVEL     SCARC2        SVARCD
     C                   EXSR      ERRCK2
     C     *IN90         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C                   SETOFF                                       92
     C                   ENDIF
     C                   ENDIF
      *
      * Error check third transaction line
      *
     C     *IN36         IFEQ      *OFF
     C                   SETON                                        93
     C                   MOVEL(P)  SCDES3        SVDESC
     C                   MOVEL(P)  SCLAT3        SVLATE
     C                   MOVEL(P)  SCPOD3        SVPODT
     C                   Z-ADD     SCCHK3        SVCHK#
     C                   MOVE      T3TYPE        SVTYPE
     C                   MOVEL     SCARC3        SVARCD
     C                   EXSR      ERRCK2
     C     *IN90         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C                   SETOFF                                       93
     C                   ENDIF
     C                   ENDIF
      *
     C*  EMPLOYER MUST HAVE ASSIGNED AGENT EFFECTIVE FOR THE POLICY
     C*  YEAR EFFECTIVE AS OF THE POSTING DATE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C     AHKEY2        SETGT     PAH01
     C                   READP     PAH01                                  55
     C     *IN55         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  55
     C                   END
     C     *IN55         IFEQ      '1'
     C     AHCO#         ORNE      UPCO#
     C     AHFND         ORNE      UPFND
     C     AHEMP#        ORNE      SCEMP#
     C     AHDIV         ORNE      SCDIV
     C     AHFYR         ORNE      XXFYR
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*** END ADD **** 12/17/01 ***********************************************
     C******BEG*ADD******** 06/12/01 **********************************
      * Display Information Message if Negative Refund amount entered
      *
     C     TYP(1)        IFEQ      'R'
     C     AMT(1)        ANDLT     *ZERO
     C     TYP(2)        OREQ      'R'
     C     AMT(2)        ANDLT     *ZERO
     C     TYP(3)        OREQ      'R'
     C     AMT(3)        ANDLT     *ZERO
     C                   EXFMT     WDW01
      *
     C     *INKL         IFEQ      *ON
     C                   SETOFF                                       KL
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL
      *
     C                   ENDIF
      *
      ***** END ADD ******* 06/12/01 **********************************
     C*** BEGIN ADD *** 10/27/97 **************************************
     C*
     C* CHECK SGLPF TO SEE IF GLFL04 IS NOT BLANK.
     C*
     C                   MOVEL     'GL'          KKKEY
     C*
     C     GLKY          CHAIN     SGL01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     GLFL04        ANDNE     *BLANKS
     C*
     C                   SETON                                        70
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 10/27/97 **************************************
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     SCAMT         IFNE      0
     C*
     C                   Z-ADD     UPCO#         EACO#
     C                   Z-ADD     UPFND         EAFND
     C                   Z-ADD     SCYR          EAFYR
     C                   Z-ADD     SCEMP#        EAEMP#
     C                   Z-ADD     SCDIV         EADIV
     C                   MOVE      SCARCD        EAARCD
     C                   MOVE      T1TYPE        EAARTY
     C*** BEG ADD ***** 11/04/03 **************************************
     C                   MOVE      T1CTYP        EACTYP
     C*** END ADD ***** 11/04/03 **************************************
     C                   MOVE      SCDESC        EADESC
     C*
     C* * BEGIN ADD *** 01/12/98 **************************************
     C                   MOVE      SCLAT1        EALATE
     C* * END   ADD *** 01/12/98 **************************************
     C*
     C                   Z-ADD     SCAMT         EAAMT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C                   MOVE      ' '           EAEDIT
     C                   MOVE      ' '           EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     0             EABUL1
     C                   MOVE      SCCHK#        EACHK#
     C* MLS      SCSRMN    MULT 100.01    EASRMN
     C*
     C                   WRITE     PEA01
     C                   MOVE      EATIME        QQTIM1            6
     C                   ADD       1             DSTIME
     C*
     C* * * IF TTTYPE IS EQUAL TO 'I' (INTIAL DEPOSIT) THEN WE MUST UPDATE
     C* * * THE WMEML FILE AFTER ADDING THE AMOUNT TO EMINDE.
     C     T1TYPE        IFEQ      'I'
     C     EMKEY         CHAIN     PEM02                              60
     C                   ADD       SCAMT         EMINDE
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   UPDATE    PEM02
     C                   END
     C*** BEGIN ADD *** 10/27/97 **************************************
     C*
     C     T1TYPE        IFEQ      'R'
     C     *IN70         ANDEQ     *ON
     C*
     C                   EXSR      CALLAP
     C*
     C                   ENDIF
     C*** END   ADD *** 10/27/97 **************************************
     C*
     C                   END
     C*
     C     SCARC2        IFNE      *BLANK
     C     SCAMT2        ANDNE     *ZERO
     C                   Z-ADD     UPCO#         EACO#
     C                   Z-ADD     UPFND         EAFND
     C                   Z-ADD     SCYR          EAFYR
     C                   Z-ADD     SCEMP#        EAEMP#
     C                   Z-ADD     SCDIV         EADIV
     C                   MOVE      SCARC2        EAARCD
     C                   MOVE      T2TYPE        EAARTY
     C*** BEG ADD ***** 11/04/03 **************************************
     C                   MOVE      T2CTYP        EACTYP
     C*** END ADD ***** 11/04/03 **************************************
     C                   MOVE      SCDES2        EADESC
     C*
     C* * BEGIN ADD *** 01/12/98 **************************************
     C                   MOVE      SCLAT2        EALATE
     C* * END   ADD *** 01/12/98 **************************************
     C*
     C                   Z-ADD     SCAMT2        EAAMT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   Z-ADD     SCPOD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C                   MOVE      ' '           EAEDIT
     C                   MOVE      ' '           EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     0             EABUL1
     C                   MOVE      SCCHK2        EACHK#
     C* MLS      SCSRM2    MULT 100.01    EASRMN
     C*
     C                   WRITE     PEA01
     C                   MOVE      EATIME        QQTIM2            6
     C*
     C* * * IF TTTYPE IS EQUAL TO 'I' (INTIAL DEPOSIT) THEN WE MUST UPDATE
     C* * * THE WMEML FILE AFTER ADDING THE AMOUNT TO EMINDE.
     C     T2TYPE        IFEQ      'I'
     C     EMKEY         CHAIN     PEM02                              60
     C                   ADD       SCAMT2        EMINDE
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   UPDATE    PEM02
     C                   END
     C*** BEGIN ADD *** 10/27/97 **************************************
     C*
     C     T2TYPE        IFEQ      'R'
     C     *IN70         ANDEQ     *ON
     C*
     C                   EXSR      CALLAP
     C*
     C                   ENDIF
     C*** END   ADD *** 10/27/97 **************************************
     C*
     C                   END
     C*
     C*  WRITE AN A/R RECORD FOR THE THIRD TRANSACTION
     C*
     C     SCARC3        IFNE      *BLANK
     C     SCAMT3        ANDNE     *ZERO
     C                   Z-ADD     UPCO#         EACO#
     C                   Z-ADD     UPFND         EAFND
     C                   Z-ADD     SCYR          EAFYR
     C                   Z-ADD     SCEMP#        EAEMP#
     C                   Z-ADD     SCDIV         EADIV
     C                   MOVE      SCARC3        EAARCD
     C                   MOVE      T3TYPE        EAARTY
     C*** BEG ADD ***** 11/04/03 **************************************
     C                   MOVE      T3CTYP        EACTYP
     C*** END ADD ***** 11/04/03 **************************************
     C                   MOVE      SCDES3        EADESC
     C*
     C* * BEGIN ADD *** 01/12/98 **************************************
     C                   MOVE      SCLAT3        EALATE
     C* * END   ADD *** 01/12/98 **************************************
     C*
     C                   Z-ADD     SCAMT3        EAAMT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   Z-ADD     SCPOD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C                   MOVE      ' '           EAEDIT
     C                   MOVE      ' '           EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
      *** BEG ADD **** 11/15/01 *****************************************
     C                   ADD       1             DSTIME
      *** END ADD **** 11/15/01 *****************************************
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     0             EABUL1
     C                   MOVE      SCCHK3        EACHK#
     C*
     C                   WRITE     PEA01
     C*
     C*  IF TTTYPE IS EQUAL TO 'I' (INTIAL DEPOSIT) THEN WE MUST UPDATE
     C*  THE WMEML FILE AFTER ADDING THE AMOUNT TO EMINDE.
     C*
     C     T3TYPE        IFEQ      'I'
     C     EMKEY         CHAIN     PEM02                              60
     C                   ADD       SCAMT3        EMINDE
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   UPDATE    PEM02
     C                   END
     C*** BEGIN ADD *** 10/27/97 **************************************
     C*
     C     T3TYPE        IFEQ      'R'
     C     *IN70         ANDEQ     *ON
     C*
     C                   EXSR      CALLAP
     C*
     C                   ENDIF
     C*** END   ADD *** 10/27/97 **************************************
     C                   END
     C                   MOVE      *BLANK        SCARC3
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     UPFYR         SCYR
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
     C                   MOVE      *BLANK        SCMSG1
     C                   MOVE      '  '          SCARCD
     C                   MOVE      '  '          SCARC2
     C                   MOVE      *ALL'0'       QQAMT
     C                   MOVE      '1'           *IN30
     C*
     C***  BEGIN ADD  ***  10/19/00  *********************************
     C                   EXSR      CLRSAV
     C***  END   ADD  ***  10/19/00  *********************************
     C*
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 10/27/97 **************************************
     C*****************************************************************
     C*   SUBROUTINE - MVEAPV - MOVES WMEAP VALUES TO PARM VALUES.
     C*****************************************************************
     C*
     C     MVEAPV        BEGSR
     C*
     C                   MOVE      EACO#         ##CO#
     C                   MOVE      EAFND         ##GRP
     C                   MOVE      EAFYR         ##YEAR
     C                   MOVE      EAEMP#        ##EMP#
     C                   MOVE      EADIV         ##DIV
     C                   MOVEL     EAARCD        ##CODE
     C                   MOVEL     EAARTY        ##TYPE
     C                   MOVEL     EADESC        ##DESC
     C                   MOVE      EAAMT         ##AMT
     C                   MOVE      EATRDT        ##TRDT
     C*
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##PODT
     C*
     C                   MOVE      EATIME        ##TIME
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*   SUBROUTINE - CALLAP - CALLS AP002R TO UPDATE AP/GL RECORDS.
     C*****************************************************************
     C*
     C     CALLAP        BEGSR
     C*
     C                   EXSR      MVEAPV
     C*
     C                   CALL      'AP002R'      AP2PL
     C*
     C                   ENDSR
     C*
     C***  BEGIN ADD  ***  08/25/99  *********************************
     C*===============================================================
     C* CHKBAL - CHECK THE BALANCE ON A REFUND AMOUNT
     C*===============================================================
     C*
     C     CHKBAL        BEGSR
     C*
     C*
     C                   MOVE      *OFF          ERR               1
     C                   Z-SUB     XXBAL         XXNEG            11 2
     C*
     C     TYP(X)        IFEQ      'R'
     C     SVA(X)        ANDNE     AMT(X)
     C     XXNEG         ANDLT     AMT(X)
     C     X             ADD       80            Y
     C                   MOVE      *ON           *IN(Y)
     C                   MOVE      *ON           ERR
     C                   ENDIF                                                  TYP,X='R'
     C*
     C                   Z-ADD     AMT(X)        SVA(X)
     C                   Z-ADD     AMT(X)        AMT2(X)
     C*
     C     X             DO        3             Y
     C*
     C                   SELECT
     C     TYP(X)        WHENEQ    'B'
     C     TYP(X)        OREQ      'A'
     C     TYP(X)        OREQ      'R'
     C     TYP(X)        OREQ      'N'
     C     TYP(X)        OREQ      'D'
     C***  BEGIN ADD  ***  01/24/00  *********************************
     C     TYP(X)        OREQ      'V'
     C***  END   ADD  ***  01/24/00  *********************************
     C                   ADD       AMT2(Y)       XXBAL
     C     TYP(X)        WHENEQ    'P'
     C***  BEGIN DEL  ***  01/24/00  *********************************
     C********** TYP,X     OREQ 'V'
     C***  END   DEL  ***  01/24/00  *********************************
     C                   SUB       AMT2(Y)       XXBAL
     C                   ENDSL
     C*
     C                   ENDDO                                                  X->3
     C*
     C                   ENDSR                                                  CHKBAL
     C***  END   ADD  ***  08/25/99  *********************************
     C***  BEGIN ADD  ***  10/19/00  *********************************
     C*===============================================================
     C* CLRSAV - CLEAR SAVE FIELDS
     C*===============================================================
     C*
     C     CLRSAV        BEGSR
     C*
     C                   CLEAR                   SVEMP#
     C                   CLEAR                   SVDIV
     C                   CLEAR                   SVFYR
     C*
     C                   ENDSR                                                  CLRSAV
     C***  END   ADD  ***  10/19/00  *********************************
     C*
     C*****************************************************************
     C*** END   ADD *** 10/27/97 **************************************
      *** BEG ADD **** 12/17/01 ***************************************
     C*===============================================================
     C* ERRCK2 - ERROR CHECK ROUTINE FOR SCR02
     C*===============================================================
     C     ERRCK2        BEGSR
     C*
     C*  DESCRIPTION CANNOT BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SVDESC        ANDEQ     *BLANK
     C                   SETON                                        20  90
     C                   ENDIF
     C*
     C*  POSTING DATE MUST BE VALID
     C*
     C                   Z-ADD     SVPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SVPODT
     C*
     C     *IN90         IFEQ      *OFF
     C     SRERR         ANDEQ     '1'
     C                   SETON                                        21  90
     C                   END
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     SVPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C     *IN90         IFEQ      *OFF
     C     BSSEFL        ANDEQ     'E'
     C     SVTYPE        IFNE      'P'
     C     SVTYPE        ANDNE     'I'
     C     SVTYPE        ANDNE     'Q'
     C     SVTYPE        ANDNE     'R'
     C     SVTYPE        ANDNE     'N'
     C     XXPODT        IFLT      XXDT
     C     SVYEAR        IFNE      UYEAR
     C     SVMOTH        ORNE      UMONTH
     C                   SETON                                        33  90
     C                   END                                                    SVYEAR<>UYEAR
     C                   END                                                    XXPODT<>XXDT
     C                   END                                                    SVTYPE<>'P'
     C                   END                                                    *IN90 = *OFF
      ***** BEG ADD ******* 10/01/02 **********************************
      *
      *  Disallow back post for S/R employer
      *    unless using A/R code entered in "Group Detail Info" SCR02.
      *
     C     *IN90         IFEQ      *OFF
     C     BSSEFL        ANDEQ     'S'
     C     XXPODT        ANDLT     XXDT
      *
     C     SVTYPE        IFNE      'P'
     C     SVTYPE        ANDNE     'I'
     C     SVTYPE        ANDNE     'Q'
     C     SVTYPE        ANDNE     'R'
     C     SVTYPE        ANDNE     'N'
     C                   MOVE      SCYR          XXFYR
     C     F1KEY         CHAIN     PF101                              60
      *
     C     *IN60         IFEQ      *ON
     C     SVARCD        ORNE      F1SRBB
     C                   SETON                                        90  52
     C                   ENDIF                                                  F1KEY
      *
     C                   ENDIF                                                  SVTYPE
      *
     C                   ENDIF                                                  BSSEFL
      *
      ***** END ADD ******* 10/01/02 **********************************
     C*
     C*  IF THE A/R CODE IS FOR A DEDUCTIBLE PAYMENT, AS DEFINED IN
     C*  THE GROUP DETAIL, THE CHECK NUMBER IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SVARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'Q'
     C     SVCHK#        ANDEQ     *ZERO
     C                   SETON                                        63  90
     C                   ENDIF                                                  TTCTYP = 'R'
     C                   ENDIF                                                  *IN60 = *OFF
     C                   END                                                    *IN90 = *OFF
     C*
     C*  LATE PAYMENT FLAG MUST BE 'Y' OR BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SVLATE        ANDNE     'Y'
     C     SVLATE        ANDNE     *BLANK
     C                   SETON                                        46  90
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF THE PAYMENT IS A REFUND, TYPE 'R', DISPLAY AN INFORMATIONAL
     C*  MESSAGE IF THE TRANSACTION WILL CAUSE THE BALANCE TO BE NEG
     C*
     C     *IN90         IFEQ      *OFF
     C   91              Z-ADD     1             X
     C   92              Z-ADD     2             X
     C   93              Z-ADD     3             X
     C   91              Z-ADD     SCAMT         AMT(X)
     C   92              Z-ADD     SCAMT2        AMT(X)
     C   93              Z-ADD     SCAMT3        AMT(X)
     C                   EXSR      CHKBAL
     C     ERR           COMP      *ON                                    90
     C                   ENDIF
     C*
     C                   ENDSR
      *** END ADD **** 12/17/01 ***************************************
     C*
     OPEM02     E            PEM02E
**
Edit request has been submitted
Employer has been cancelled
P,I or Q only for back posting
