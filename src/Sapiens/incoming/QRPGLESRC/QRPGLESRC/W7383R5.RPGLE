     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7383R5 - EMPLOYERS A/R DETAIL LISTING -
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*        THIS PROGRAM SORTS AND TOTALS ALL A/R TRANSACTION CODES*
     H*        THAT WERE SELECTED FROM THE PROMPT SCREEN WITHIN THE   *
     H*        SELECTED DATE RANGE.                                   *
     H*        THIS PROGRAM CAN BE RUN FOR ONE (1) OR ALL AGENTS,     *
     H*            ONE (1) OR ALL EMPLOYERS.                          *
     H*        THE ORDER CAN BE IN AGENT, EMPLOYER # OR EMPLOYER      *
     H*            NAME ORDER.                                        *
     H*                                                               *
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*         20-49  USED TO LIST SELECTED A/R CODES FROM PROMPT    *
     H*                SCREEN ON ROUTING PAGE.                        *
     H*                                                               *
     H*         25-26  USED ONLY TO CONTROL HEADING FOR SUMMARY       *
     H*                TOTALS.                                        *
     H*                                                               *
     H*         50 -   USED TO READ WXEAP TO SEE IF ANY TRANSACTIONS  *
     H*                OCCURRED WITHIN DATE RANGE.                    *
     H*                                                               *
     H*         54  -  USED AS MAIN READ ON WXDTP                     *
     H*         55  -  USED TO CHAIN TO WXEAP FOR A/R CALCULATIONS    *
     H*         58  -  USED AS A DO WHILE LOOP TO CALCULATE PREVIOUS  *
     H*                AND CURRENT BALANCES.                          *
     H*         60  -  GENERAL CHAINING                               *
     H***  BEGIN ADD  ***  05/26/99  **********************************
     H*         88  -  SUPPRESS PRINT OF EMPLOYER DETAIL              *
     H***  END ADD ******  05/26/99  **********************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/24/06 C:65 PROJECT 114WHO                                  *
     H*               USE THE RETROSPECTIVE PREMIUM IF POPULATED FOR  *
     H*               PREMIUM BASIS.                                  *
     H*                                                               *
     H* 05/16/00 C:87  PROJECT: SUPP WA081 #22883                     *
     H*                Printing blank pages sometimes when run for    *
     H*                agent sequence.  Added line to correct.        *
     H*                                                               *
     H* 11/24/99 C:118 PROJECT: WC577                                 *
     H*                REMOVE ZERO SUPPRESSION EDIT CODE FROM OUTPUT  *
     H*                SPECS.                                         *
     H*                                                               *
     H* 09/01/99 C:33  PROJECT: WC577                                 *
     H*                DETERMINE PREMIUM BY USING ELFAF NOT ELAAPR.   *
     H*                                                               *
     H* 08/20/99 C:118 PROJECT: WC577                                 *
     H*                Modify report so paging is correct.            *
     H*                                                               *
     H* 08/19/99 C:33  PROJECT: WC899                                 *
     H*               -PRINT ROUTING DATA ON FIRST PAGE.              *
     H*                                                               *
     H* 07/26/99 C:118 SUPPORT: WA025/18898                           *
     H*          C:33  Modify report, so group summary prints if a    *
     H*                previous balance exists, even if no records    *
     H*                are found for the selections made.             *
     H*                                                               *
     H* 07/19/99 C:33  PROJECT: WC899                                 *
     H*                INCLUDE POLICY PLAN TYPE SELECTION ON ROUTING  *
     H*                                                               *
     H* 05/26/99 C:09  PROJECT: SUPP WA090 #18237                     *
     H*                Selection for "Print Emplrs w/No Activity"     *
     H*                should have no effect on summary totals listed *
     H*                on the last two pages.  It should only suppress*
     H*                individual employer printing on detail pages.  *
     H*                                                               *
     H* 03/24/99 C:87  PROJECT: WC890                                 *
     H*                If QQPRNT = 'Y' then print the employer even   *
     H*                if the the do not have an A/R transaction w/in *
     H*                the date range.                                *
     H*                                                               *
     H* 03/22/99 C:87  PROJECT: SUPP WA025 #17229                     *
     H*                Do not print for employer if a/r transaction   *
     H*                date is not within range, even if a previous   *
     H*                balance exists (see also seq#16744).           *
     H*                                                               *
     H* 03/24/98 C:85 Project: WC577                                  *
     H*               CHANGED RSEMP# TO LENGTH 9 AND RSDIV TO         *
     H*               LENGTH 5,0 FOR PRINTING ON ROUTING PAGE.        *
     H*               ADDED ERROR MESSAGE IF NO RECORDS WERE SELECTED.*
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     F*
     F* Output Routing File
     FSMORL     IF   E           K DISK
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* User Parameter File
     FWMUPL     IF   E           K DISK
     F*
     F* Work File for Agent Sequence - built in W7382R
     FWXDTL     IF   E           K DISK
     F*
     F* WORK FILE FOR Employer A/R
     FWXEAL     IF   E           K DISK
     F*
     F* Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F* Employer Detail File
     FWDELL1    IF   E           K DISK
     F*
     F* Table File - A/R Type
     FWTABL     IF   E           K DISK
     F*
     F* Agent Assignment Master File
     FWMAAL1    IF   E           K DISK
     F*
     F* Agent Master File
     FWMAGL     IF   E           K DISK
     F*
     FWDF1L     IF   E           K DISK
     F*
     F* Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D* Define Sort Selection Compile time Array
     D*
     D SRT             S             25    DIM(3) CTDATA PERRCD(1)              Sort Sequence
     D*
     D* Define A/R Type Summary Arrays
     D*
     D XCOD            S              2    DIM(999)                             A/R code
     D XCNT            S             11    DIM(999)                             amount
     D COMB            S             13    DIM(999)                             combination
     D*
     D* Define arrays for A/R Code selection criteria
     D COD             S              2    DIM(30) ASCEND                       selected a/r code
     D DES             S             30    DIM(30)                              description
     D CDE             S              2    DIM(30)                              a/r output code
     D***  BEGIN ADD  ***  07/19/99  *********************************
     D PLAN            S              1    DIM(3) CTDATA PERRCD(1)              PLAN TYPE
     D PDESC           S             24    DIM(3) ALT(PLAN)
     D***  END   ADD  ***  07/19/99  *********************************
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Combination Array
     D                 DS
     D  XCOMB                  1     13
     D  XCODE                  1      2
     D  XYCNT                  3     13  2
     D*
     D*
     D* External Program Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*          LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    DTAGT#        XXAGT#
     C     *LIKE         DEFINE    DTAGT#        SVAGT#
     C     *LIKE         DEFINE    AGNAME        XXAGNM
     C     *LIKE         DEFINE    EMDT          XXHTDT
     C*
     C*****************************************************************
     C*           KEY LIST
     C*****************************************************************
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Output Routing File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Table File - A/R Type Code
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRES            10
     C*
     C* Key List for the Employer A/R File
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* Key List for the Employer A/R File
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXHFDT            7 0
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* Key List for the Agent Assignment Master File
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C* Key List for the Agent Master File
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C* Key List for the Fund Detail File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Entry Parms
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQAGT             5
     C                   PARM                    QQEMP             9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQSRT             1
     C                   PARM                    QQCOD            60
     C                   PARM                    QQQUE            10
     C***  BEGIN ADD  ***  03/24/99  **********************************
     C                   PARM                    QQPRNT            1
     C***  END   ADD  ***  03/24/99  **********************************
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   PARM                    QQPLAN            1
     C***  END   ADD  ***  07/19/99  *********************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQFDT         X1FDT             8 0
     C                   MOVE      QQTDT         X1TDT             8 0
     C                   MOVEA     QQCOD         COD
     C                   MOVE      QQEMP         XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C                   TIME                    DSDTTM
     C     XXFYR         ADD       1900          XXYEAR            4 0
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXHFDT
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXHTDT
     C*
     C                   MOVEL     'WMEAP'       XXFILE           10
     C*
     C* Set-Up and Initialize
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C* Initialize A/R Summary Fields
     C*
     C                   MOVEA     *BLANKS       XCNT
     C                   MOVEA     *BLANKS       XCOD
     C                   MOVEA     *BLANKS       COMB
     C                   Z-ADD     0             XXCNT            11 2
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  Load the A/R description array so that A/R selection codes
     C*  are printed on the routing page.  Also list all selections
     C*  made on prompt screen.
     C*
     C                   MOVE      QQCO#         RSCO              3
     C                   MOVE      QQFND         RSFND             3
     C                   MOVE      QQFYR         RSFYR             3
     C     QQAGT         IFNE      *ZEROS
     C                   MOVE      QQAGT         RSAGT             5
     C                   ELSE
     C                   MOVEL     '*ALL'        RSAGT
     C                   END
     C     QQEMP         IFNE      *ZEROS
     C*
     C*** BEGIN CHG *** 03/24/98 **************************************
     C*
     C**********           MOVE QQEMP     RSEMP#  5
     C                   MOVE      QQEMP         RSEMP#            9
     C**********           MOVE QQDIV     RSDIV   30
     C                   MOVE      QQDIV         RSDIV             5 0
     C                   ELSE
     C**********           MOVEL'*ALL'    RSEMP#
     C                   MOVEL     '  *ALL  '    RSEMP#
     C*
     C*** END   CHG *** 03/24/98 **************************************
     C*
     C                   END
     C*
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFDT             6 0
     C                   MOVE      XXFDT         RSFDT             6 0
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTDT             6 0
     C                   MOVE      XXTDT         RSTDT             6 0
     C*
     C     QQSRT         IFEQ      'A'
     C                   MOVEL     SRT(1)        RSSRT            25
     C                   ELSE
     C     QQSRT         IFEQ      'E'
     C                   MOVEL     SRT(2)        RSSRT
     C                   ELSE
     C                   MOVEL     SRT(3)        RSSRT
     C                   END
     C                   END
     C*
     C***  BEGIN ADD  ***  05/26/99  **********************************
     C     QQPRNT        IFEQ      'Y'
     C                   MOVEL(P)  'YES'         RSPRNT            3
     C                   ELSE
     C                   MOVEL(P)  'NO'          RSPRNT
     C                   ENDIF                                                  QQPRNT='Y'
     C***  END   ADD  ***  05/26/99  **********************************
     c*
     C* Sort the Code Array
     C*
     C                   SORTA     COD
     C                   MOVE      *BLANKS       CDE
     C                   MOVE      *BLANKS       DES
     C*
     C     QQCOD         IFNE      *BLANKS
     C*
     C* Fill the Code Decription Array which Corresponds to the Code
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   Z-ADD     1             Z                 2 0
     C     Y             DOUEQ     31
     C*
     C* Indicators 20 to 49 are used for Output Purposes
     C*
     C     COD(Y)        IFNE      *BLANKS
     C     Z             ADD       19            Z1                2 0
     C                   MOVE      '1'           *IN(Z1)
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     COD(Y)        XXRES
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     COD(Y)        CDE(Z)
     C                   MOVEL     TTDESC        DES(Z)
     C                   ADD       1             Z
     C                   END
     C                   END
     C*
     C                   ADD       1             Y
     C                   END
     C*
     C                   ELSE                                                   COD = BLANKS
     C*
     C* PRINT *ALL CODES ON ROUTING PAGE
     C*
     C                   MOVE      '*ALL'        RSCODE            4
     C*
     C                   END                                                    COD = BLANKS
     C*
     C* Get the Distribution Information
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   Z-ADD     1             X                 1 0
     C     QQPLAN        LOOKUP    PLAN(X)                                50
     C   50              MOVEL     PDESC(X)      XXPLAN           24
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C* Print the Distribution Information
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
      *  Center Title in Field
      *
     C  N60              MOVEL(P)  ORTITL        ##DATA
     C  N60              CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C  N60              MOVEL(P)  ##DATA        ORTITL
     C*
     C* Get the Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C   60              MOVE      *BLANK        FNNAME
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
     C*
     C* Get the Fund Detail Record
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C* Reset Indicators 20-49
     C*
     C                   Z-ADD     20            Z1
     C                   DO        30
     C                   MOVE      '0'           *IN(Z1)
     C                   ADD       1             Z1
     C                   END
     C*
     C*  Begin Reading Work File
     C*
     C                   READ      PDT01                                  54
     C                   MOVE      DTAGT#        SVAGT#
     C*
     C                   EXCEPT    HEAD
     C*
     C                   MOVE      *ON           *IN35
     C     *IN54         DOWEQ     '0'
     C*
     C                   MOVE      DTAGT#        XXAGT#
     C                   MOVE      DTEMP#        XXEMP#
     C                   MOVE      DTDIV         XXDIV
     C                   EXSR      SRAGT                                        AGENT NAME
     C                   EXSR      SREMNM                                       EMPLOYER NAME
     C*
     C* CHECK TO SEE IF THERE ARE ANY TRANSACTIONS WITHIN THE DATE
     C*   RANGE.  ONLY PROCESS AN EMPLOYER IF THERE ARE TRANSACTIONS.
     C*
     C                   MOVE      *BLANK        XXFLAG            1
     C*
     C     EAKEY1        SETLL     PEA01
     C     EAKEY         READE     PEA01                                  50
     C     *IN50         DOWEQ     '0'
     C     XXFLAG        ANDEQ     *BLANK
     C     EAXXDT        IFGE      XXHFDT
     C     EAXXDT        ANDLE     XXHTDT
     C                   MOVE      'Y'           XXFLAG
     C***** BEGIN ADD **** 07/26/99 ***********************************
     C                   MOVE      *ON           *IN61
     C***** END   ADD **** 07/26/99 ***********************************
     C                   END
     C     EAKEY         READE     PEA01                                  50
     C                   END
     C***  BEGIN ADD  ***  05/26/99  **********************************
     C*
     C*  SUPPRESS PRINTING OF EMPLOYER DETAIL IF PROMPT SELECTION WAS
     C*  TO NOT PRINT EMPLOYERS W/NO ACTIVITY AND NO TRANSACTIONS
     C*  WERE FOUND IN THE DATE RANGE
     C*
     C     QQPRNT        IFEQ      ' '
     C     XXFLAG        ANDEQ     ' '
     C                   MOVE      '1'           *IN88
     C                   ELSE
     C                   MOVE      '0'           *IN88
     C                   END
     C***  END ADD ******  05/26/99  **********************************
     C*
     C     EAKEY         CHAIN     PEA01                              55
     C     *IN55         IFEQ      '0'
     C                   EXSR      SRPBAL
     C*
     C***  BEGIN CHG  ***  05/26/99  **********************************
     C***  BEGIN ADD  ***  03/24/99  **********************************
     C********** QQPRNT    IFEQ 'Y'
     C**********
     C********** PBAL      IFNE *ZEROS
     C**********           MOVE 'Y'       XXFLAG
     C**********           END
     C**********
     C**********           ENDIF                           QQPRNT='Y'
     C***  END   ADD  ***  03/24/99  **********************************
     C*
     C     PBAL          IFNE      *ZEROS
     C                   MOVE      'Y'           XXFLAG
     C***** BEGIN ADD **** 08/20/99 ***********************************
     C     QQPRNT        IFEQ      'Y'
     C***** END  ADD ****  08/20/99 ***********************************
     C***** BEGIN ADD **** 07/26/99 ***********************************
     C                   MOVE      *ON           *IN61
     C***** END   ADD **** 07/26/99 ***********************************
     C                   END
     C***  END   CHG  ***  05/26/99  **********************************
     C***** BEGIN ADD **** 08/20/99 ***********************************
     C                   ENDIF
     C***** END   ADD **** 08/20/99 ***********************************
     C*
     C***  BEGIN DEL  ***  03/22/99  **********************************
     C********** PBAL      IFNE *ZEROS
     C**********           MOVE 'Y'       XXFLAG
     C**********           END
     C***  END   DEL  ***  03/22/99  **********************************
     C*
     C     XXFLAG        IFEQ      'Y'
     C*
     C* CHECK TO SEE IF AGENT NUMBER HAS CHANGED, IF SO PAGE BREAK
     C*
     C     QQSRT         IFEQ      'A'
     C     DTAGT#        ANDNE     SVAGT#
     C***  BEGIN ADD  ***  05/16/00  **********************************
     C     *IN88         ANDEQ     '0'
     C***  END ADD ******  05/16/00  **********************************
     C                   EXCEPT    HEAD
     C                   MOVE      *ON           *IN35
     C                   MOVE      DTAGT#        SVAGT#
     C                   END
     C*
     C                   Z-ADD     0             XXANPR
     C     ELKEY         CHAIN     WDELL1                             60
     C     *IN60         IFEQ      '0'
     C*
     C***  BEGIN ADD  ***  09/01/99  *********************************
     C********** ELAAPR    IFNE 0
     C     ELFAF         IFEQ      'A'
     C***  END   ADD  ***  09/01/99  *********************************
     C                   Z-ADD     ELAAPR        XXANPR           11 2
     C*** BEGIN ADD ***  01/24/06  ************************************
     C     ELSTAM        IFNE      *ZERO
     C                   Z-ADD     ELSTAM        XXANPR           11 2
     C                   ENDIF
     C*** END ADD *****  01/24/06  ************************************
     C                   ELSE
     C                   Z-ADD     ELANPR        XXANPR
     C                   END
     C*
     C                   END                                                    READP WDELL1
     C*
     C                   ADD       PBAL          TPBAL            11 2
     C                   ADD       XXANPR        FNANPR           11 2
     C*
     C     *INOF         IFEQ      '1'
     C***  BEGIN ADD  ***  05/26/99  **********************************
     C     *IN88         ANDEQ     '0'
     C***  END ADD ******  05/26/99  **********************************
     C                   EXCEPT    HEAD
     C                   MOVE      *ON           *IN35
     C                   END
     C                   EXCEPT    EMPHD
     C                   EXCEPT    PBA
     C                   MOVE      *OFF          *IN35
     C                   Z-ADD     PBAL          CBAL
     C*
     C                   EXSR      SRCBAL
     C*
     C* Add to the Totals
     C*
     C                   ADD       CBAL          FNTOT            11 2
     C     XXDEPD        ADD       XXFEED        XXDUE
     C                   ADD       XXDNPD        XXDUE
     C                   ADD       XXPRMD        XXDUE
     C*
     C* Print Employer Total Line
     C*
     C     *INOF         IFEQ      '1'
     C***  BEGIN ADD  ***  05/26/99  **********************************
     C     *IN88         ANDEQ     '0'
     C***  END ADD ******  05/26/99  **********************************
     C                   EXCEPT    HEAD
     C                   MOVE      *ON           *IN35
     C                   EXCEPT    EMPHD
     C                   MOVE      *OFF          *IN35
     C                   END
     C***** BEGIN DEL **** 07/26/99 ***********************************
     C**********           MOVE '0'       *IN25
     C**********           MOVE '0'       *IN26
     C***** END   DEL **** 07/26/99 ***********************************
     C***** BEGIN ADD **** 08/20/99 ***********************************
     C                   MOVE      '0'           *IN25
     C                   MOVE      '0'           *IN26
     C***** END   ADD **** 08/20/99 ***********************************
     C                   EXCEPT    EMPTL
     C*
     C                   END                                                    WXEAL
     C                   END                                                    XXFLAG = "Y"
     C                   READ      PDT01                                  54
     C                   END                                                    WXDTL
     C*
     C*** BEGIN ADD *** 03/24/98 **************************************
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C**********           EXCPTHEAD1
     C*
     C     *IN61         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C*
     C*** END   ADD *** 03/24/98 **************************************
     C*
     C* Print A/R Summary
     C*
     C                   EXSR      ARDETL
     C*
     C*** BEGIN ADD *** 03/24/98 **************************************
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C     *IN61         IFEQ      *OFF
     C                   EXCEPT    NOREC
     C***** BEGIN ADD **** 08/20/99 ***********************************
     C     QQPRNT        IFEQ      ' '
     C                   EXCEPT    HEAD1
     C                   ENDIF                                                  QQPRNT=' '
     C***** END   ADD **** 08/20/99 ***********************************
     C                   ENDIF
     C*
     C*** END   ADD *** 03/24/98 **************************************
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   SETON                                        LR
     C*
     C*
     C*****************************************************************
     C* SRPBAL - SUBROUTINE TO CALCULATE PREVIOUS BALANCE             *
     C*****************************************************************
     C*
     C     SRPBAL        BEGSR
     C*
     C*  INITIALIZE TOTAL DUE FEILDS
     C*
     C                   Z-ADD     0             XXDEPD            9 2
     C                   Z-ADD     0             XXFEED            9 2
     C                   Z-ADD     0             XXPRMD            9 2
     C                   Z-ADD     0             XXDNPD            9 2           DWNP DUE
     C                   Z-ADD     0             XXDUE             9 2
     C                   MOVE      '0'           *IN58
     C                   Z-ADD     0             PBAL
     C*
     C* Get the Previous Balance for the Specified Employer
     C*
     C     EAXXDT        DOWLT     XXHFDT
     C     *IN58         ANDEQ     '0'
     C*
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C                   SUB       EAAMT         PBAL             11 2
     C                   Z-ADD     0             XXAMT            11 2
     C     EAAMT         MULT      -1            XXAMT
     C                   ELSE
     C                   ADD       EAAMT         PBAL
     C                   Z-ADD     EAAMT         XXAMT
     C                   END
     C*
     C*   1. ADD TOTAL FOR PREVIOUS ADJUSTMENTS.
     C*
     C     EAARTY        IFEQ      'A'
     C                   ADD       XXAMT         PATOT            11 2
     C                   END
     C*
     C*   2. ADD TOTAL FOR PREVIOUS BILLS.
     C*
     C     EAARTY        IFEQ      'B'
     C                   ADD       XXAMT         PBTOT            11 2
     C                   END
     C*
     C*   3. ADD TOTAL FOR PREVIOUS AUDITS.
     C*
     C     EAARTY        IFEQ      'D'
     C                   ADD       XXAMT         PDTOT            11 2
     C                   END
     C*
     C*   4. ADD TOTAL FOR PREVIOUS VOIDS.
     C*
     C     EAARTY        IFEQ      'V'
     C                   ADD       XXAMT         PVTOT            11 2
     C                   END
     C*
     C*   5. ADD TOTAL FOR PREVIOUS PAYMENTS.
     C*
     C     EAARTY        IFEQ      'P'
     C                   ADD       XXAMT         PPTOT            11 2
     C                   END
     C*
     C*   6. ADD TOTAL FOR PREVIOUS NSF'S.
     C*
     C     EAARTY        IFEQ      'N'
     C                   ADD       XXAMT         PNTOT            11 2
     C                   END
     C*
     C*   7. ADD TOTAL FOR PREVIOUS INITIAL DEPOSITS.
     C*
     C     EAARTY        IFEQ      'I'
     C                   ADD       XXAMT         PITOT            11 2
     C                   END
     C*
     C*   8. ADD TOTAL FOR PREVIOUS INITIAL DEPOSITS BILLED.
     C*
     C     EAARTY        IFEQ      'H'
     C                   ADD       XXAMT         PHTOT            11 2
     C                   END
     C*
     C*   9. ADD TOTAL FOR PREVIOUS REFUNDS.
     C*
     C     EAARTY        IFEQ      'R'
     C                   ADD       XXAMT         PRTOT            11 2
     C                   END
     C*
     C*  10. ADD TOTAL FOR PREVIOUS FEES BILLED.
     C*
     C     EAARTY        IFEQ      'F'
     C                   ADD       XXAMT         PFTOT            11 2
     C                   END
     C*
     C*  11. ADD TOTAL FOR PREVIOUS FEES PAID.
     C*
     C     EAARTY        IFEQ      'Q'
     C                   ADD       XXAMT         PQTOT            11 2
     C                   END
     C*
     C     EAARTY        IFEQ      'I'
     C     EAARTY        OREQ      'H'
     C                   ADD       XXAMT         XXDEPD
     C                   ELSE
     C     EAARTY        IFEQ      'F'
     C     EAARTY        OREQ      'Q'
     C                   ADD       XXAMT         XXFEED
     C                   ELSE
     C                   ADD       XXAMT         XXPRMD
     C                   END
     C                   END
     C*
     C     EAKEY         READE     PEA01                                  58
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRCBAL - SUBROUTINE TO CALCULATE CURRENT BALANCE             *
     C*****************************************************************
     C*
     C     SRCBAL        BEGSR
     C*
     C                   Z-ADD     0             XXTCOL            9 2
     C                   Z-ADD     0             XXTTRN            9 2
     C                   Z-ADD     0             XXTBIL            9 2
     C                   MOVE      '0'           *IN58
     C*
     C     EAXXDT        DOWLE     XXHTDT
     C     EAXXDT        ANDGE     XXHFDT
     C     *IN58         ANDEQ     '0'
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT            6 0
     C*
     C*  Initialize detail line fields
     C*
     C                   Z-ADD     0             XXCOL             9 2
     C                   Z-ADD     0             XXTRN             9 2
     C                   Z-ADD     0             XXBIL             9 2
     C*
     C*
     C* Reverse Amounts for Payments, Initial Deposits and Fee Payments
     C*
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C                   MULT      -1            EAAMT
     C                   END
     C*
     C*   1. ADD TOTAL FOR ADJUSTMENTS.
     C*
     C     EAARTY        IFEQ      'A'
     C                   ADD       EAAMT         ATOT             11 2
     C                   Z-ADD     EAAMT         XXBIL
     C                   ADD       EAAMT         XXTBIL
     C                   ADD       EAAMT         XXPRMD
     C                   END
     C*
     C*   2. ADD TOTAL FOR BILLS.
     C*
     C     EAARTY        IFEQ      'B'
     C                   ADD       EAAMT         BTOT             11 2
     C                   Z-ADD     EAAMT         XXBIL
     C                   ADD       EAAMT         XXTBIL
     C                   ADD       EAAMT         XXPRMD
     C                   END
     C*
     C*   3. ADD TOTAL FOR AUDITS.
     C*
     C     EAARTY        IFEQ      'D'
     C                   ADD       EAAMT         DTOT             11 2
     C                   Z-ADD     EAAMT         XXBIL
     C                   ADD       EAAMT         XXTBIL
     C                   ADD       EAAMT         XXPRMD
     C                   END
     C*
     C*   4. ADD TOTAL FOR VOIDS.
     C*
     C     EAARTY        IFEQ      'V'
     C                   ADD       EAAMT         VTOT             11 2
     C                   Z-ADD     EAAMT         XXTRN
     C                   ADD       EAAMT         XXTTRN
     C                   ADD       EAAMT         XXPRMD
     C                   END
     C*
     C*   5. ADD TOTAL FOR PAYMENTS.
     C*
     C     EAARTY        IFEQ      'P'
     C                   ADD       EAAMT         PTOT             11 2
     C                   Z-ADD     EAAMT         XXCOL
     C                   ADD       EAAMT         XXTCOL
     C                   ADD       EAAMT         XXPRMD
     C                   END
     C*
     C*   6. ADD TOTAL FOR NSF'S.
     C*
     C     EAARTY        IFEQ      'N'
     C                   ADD       EAAMT         NTOT             11 2
     C                   Z-ADD     EAAMT         XXTRN
     C                   ADD       EAAMT         XXTTRN
     C                   ADD       EAAMT         XXPRMD
     C                   END
     C*
     C*   7. ADD TOTAL FOR INITIAL DEPOSITS.
     C*
     C     EAARTY        IFEQ      'I'
     C                   ADD       EAAMT         ITOT             11 2
     C                   Z-ADD     EAAMT         XXCOL
     C                   ADD       EAAMT         XXTCOL
     C                   ADD       EAAMT         XXDEPD
     C                   END
     C*
     C*   8. ADD TOTAL FOR INITIAL DEPOSITS BILLED.
     C*
     C     EAARTY        IFEQ      'H'
     C                   ADD       EAAMT         HTOT             11 2
     C                   Z-ADD     EAAMT         XXBIL
     C                   ADD       EAAMT         XXTBIL
     C                   ADD       EAAMT         XXDEPD
     C                   END
     C*
     C*   9. ADD TOTAL FOR REFUNDS.
     C*
     C     EAARTY        IFEQ      'R'
     C                   ADD       EAAMT         RTOT             11 2
     C                   Z-ADD     EAAMT         XXTRN
     C                   ADD       EAAMT         XXTTRN
     C                   ADD       EAAMT         XXPRMD
     C                   END
     C*
     C*  10. ADD TOTAL FEES BILLED.
     C*
     C     EAARTY        IFEQ      'F'
     C                   ADD       EAAMT         FTOT             11 2
     C                   Z-ADD     EAAMT         XXBIL
     C                   ADD       EAAMT         XXTBIL
     C                   ADD       EAAMT         XXFEED
     C                   END
     C*
     C*  11. ADD TOTAL FOR FEES PAID.
     C*
     C     EAARTY        IFEQ      'Q'
     C                   ADD       EAAMT         QTOT             11 2
     C                   Z-ADD     EAAMT         XXCOL
     C                   ADD       EAAMT         XXTCOL
     C                   ADD       EAAMT         XXFEED
     C                   END
     C*
     C                   ADD       EAAMT         CBAL              9 2
     C*
     C                   MOVEL     EADESC        XXDESC           30
     C     *INOF         IFEQ      '1'
     C***  BEGIN ADD  ***  05/26/99  **********************************
     C     *IN88         ANDEQ     '0'
     C***  END ADD ******  05/26/99  **********************************
     C                   EXCEPT    HEAD
     C                   MOVE      *ON           *IN35
     C                   EXCEPT    EMPHD
     C                   MOVE      *OFF          *IN35
     C                   END
     C***** BEGIN DEL **** 07/26/99 ***********************************
     C*** BEGIN ADD *** 03/24/98 **************************************
     C*  SELECTION CRITERIA FOUND
     C**********           MOVE *ON       *IN61
     C*** END   ADD *** 03/24/98 **************************************
     C***** END   DEL **** 07/26/99 ***********************************
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   EXSR      ARSUM
     C*
     C     EAKEY         READE     PEA01                                  58
     C                   END
     C                   ENDSR
     C*****************************************************************
     C* SUBROUTINE    **  ARSUM  **  LOAD A/R TYPES INTO ARRAYS       *
     C*****************************************************************
     C*
     CSR   ARSUM         BEGSR
     C*
     C* Check to See if Code Exists in the Array
     C*
     C                   Z-ADD     1             V                 5 0
     C                   MOVE      '1'           *IN33
     C*
     C     EAARCD        LOOKUP    XCOD(V)                                33
     C*
     C     *IN33         IFEQ      '0'                                          NEW CODE-ADD
     C                   Z-ADD     1             V
     C*
     C* Find Next Available Position
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   ADD       1             V
     C                   END
     C*
     C* Add Fields to the Array
     C*
     C                   Z-ADD     EAAMT         XXCNT                          COUNTER
     C                   MOVE      *BLANKS       X1CNT
     C*
     C                   MOVEA     EAARCD        XCOD(V)
     C                   MOVE      XXCNT         X1CNT            11
     C                   MOVEA     X1CNT         XCNT(V)
     C*
     C*   If Code is in Array Add to Existing Amount
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXCNT                          UPDATE-EXIST
     C                   MOVE      *BLANKS       X1CNT
     C                   MOVEA     XCNT(V)       X1CNT
     C                   MOVE      X1CNT         XXCNT
     C                   ADD       EAAMT         XXCNT
     C                   MOVE      XXCNT         X1CNT
     C*
     C                   MOVEA     X1CNT         XCNT(V)
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************==*
     C*   PRINT A/R CODE SUMMARY               Subroutine  ** ARDETL  =  *
     C*****************************************************************==*
     C*
     CSR   ARDETL        BEGSR
     C*
     C* Move Code and Total Arrays to the Combination Array
     C*
     C                   Z-ADD     1             V
     C                   Z-ADD     0             SUMTTL           11 2
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   MOVEL     XCOD(V)       COMB(V)
     C                   MOVE      XCNT(V)       COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C* If There are Records in the Array then Continue
     C*
     C     V             IFNE      1
     C*
     C* Print Summary Heading                                          DES
     C*
     C***** BEGIN DEL **** 07/26/99 ***********************************
     C**********           MOVE '1'       *IN25
     C**********           MOVE '1'       *IN26
     C**********           EXCPTHEAD1
     C***** END   DEL **** 07/26/99 ***********************************
     C***** BEGIN ADD **** 08/20/99 ***********************************
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN26
     C                   EXCEPT    HEAD1
     C***** END   ADD **** 08/20/99 ***********************************
     C                   EXCEPT    ARHEAD
     C                   MOVE      '0'           *INOF
     C*
     C* Sort the Array by Code
     C*
     C                   SORTA     COMB
     C*
     C* Find the First record in the Array
     C*
     C                   Z-ADD     1             X1                5 0
     C     COMB(X1)      DOWEQ     *BLANKS
     C                   ADD       1             X1
     C                   END
     C*
     C     X1            DOWLE     999
     C                   MOVE      *BLANKS       XCOMB
     C                   MOVEA     COMB(X1)      XCOMB
     C*
     C* Retrieve A/R Code Description
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVE      *BLANKS       XXDES            30
     C                   MOVE      *BLANKS       XDES
     C                   MOVE      *BLANKS       XTYP
     C*
     C                   MOVE      XCODE         XXCD              2
     C                   MOVEL     XXCD          XXRES
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTDESC        XDES             30
     C                   MOVE      TTTYPE        XTYP              1
     C*
     C* Print the Array Detail Line
     C*
     C     *INOF         IFEQ      '1'
     C***** BEGIN DEL **** 07/26/99 ***********************************
     C**********           MOVE '1'       *IN25
     C**********           MOVE '1'       *IN26
     C**********           EXCPTHEAD1
     C***** END DEL ****** 07/26/99 ***********************************
     C***** BEGIN ADD **** 08/20/99 ***********************************
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN26
     C                   EXCEPT    HEAD1
     C***** END   ADD **** 08/20/99 ***********************************
     C                   EXCEPT    ARHEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* ADD AMOUNT TO SUMMARY TOTAL FIELD
     C*
     C                   ADD       XYCNT         SUMTTL
     C                   EXCEPT    ARDTL
     C                   ADD       1             X1
     C                   END
     C*
     C* PRINT SUMMARY TOTAL LINE
     C*
     C                   EXCEPT    ARTTL
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SUBROUTINE    **  SRAGT  **  GET AGENT NAME                   *
     C*****************************************************************
     C*
     C     SRAGT         BEGSR
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AGNAME        XXAGNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXAGNM
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       XXAGNM
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SREMNM  -  SUBROUTINE TO RETRIEVE EMPLOYER NAME               *
     C*****************************************************************
     C*
     C     SREMNM        BEGSR
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFNE      '0'
     C                   MOVE      *BLANKS       XXNAME
     C                   ELSE
     C                   MOVEL     EMNAME        XXNAME           26
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**
     O**   SELECTION CRITERIA
     O**
     O          E            ROUTE          1
     O                                           20 'COMPANY      :'
     O                       RSCO                25
     O*
     O          E            ROUTE          1
     O                                           20 'GROUP        :'
     O                       RSFND               25
     O*
     O          E            ROUTE          1
     O                                           20 'POLICY YEAR  :'
     O                       RSFYR               25
     O*
     O          E            ROUTE          1
     O                                           20 'AGENT        :'
     O                       RSAGT               27
     O*
     O          E            ROUTE          1
     O                                           20 'EMPLOYER/DIV :'
     O*
     O*** BEGIN CHG *** 03/24/98 **************************************
     O*
     O**********               RSEMP#    31
     O**********               RSDIV Z   31
     O*
     O                       RSEMP#              30
     O                       RSDIV         Z     36
     O*
     O*** END   CHG *** 03/24/98 **************************************
     O*
     O          E            ROUTE          1
     O                                           20 'DATE RANGE   :'
     O                       RSFDT               30 '  /  /  '
     O                                           32 '-'
     O                       RSTDT               41 '  /  /  '
     O***  BEGIN ADD  ***  05/26/99  **********************************
     O          E            ROUTE          1
     O                                           20 'PRINT EMPS W/ '
     O          E            ROUTE          1
     O                                           20 '  NO ACTIVITY:'
     O                       RSPRNT              25
     O***  END   ADD  ***  05/26/99  **********************************
     O*
     O          E            ROUTE          1
     O                                           20 'SORT BY      :'
     O                       RSSRT               47
     O***  BEGIN ADD  ***  07/19/99  *********************************
     O          E            ROUTE          1
     O                                           20 'POLICY PLAN  :'
     O                       XXPLAN              46
     O***  END   ADD  ***  07/19/99  *********************************
     O*
     O***  BEGIN CHG  ***  08/19/99  *********************************
     O********E* 140           ROUTE
     O          E            ROUTE       2  1
     O***  END   CHG  ***  08/19/99  *********************************
     O                                           30 'A/R CODES INCLUDED IN'
     O                                           42 'CALCULATION'
     O          E      N20   ROUTE          1
     O                       RSCODE              26
     O*
     O          E            ROUTE          1
     O                  20                       11 '1.'
     O                       CDE(1)              14
     O                       DES(1)              45
     O                  30                       50 '11.'
     O                       CDE(11)             53
     O                       DES(11)             84
     O                  40                       89 '21.'
     O                       CDE(21)             92
     O                       DES(21)            123
     O          E            ROUTE          1
     O                  21                       11 '2.'
     O                       CDE(2)              14
     O                       DES(2)              45
     O                  31                       50 '12.'
     O                       CDE(12)             53
     O                       DES(12)             84
     O                  41                       89 '22.'
     O                       CDE(22)             92
     O                       DES(22)            123
     O          E            ROUTE          1
     O                  22                       11 '3.'
     O                       CDE(3)              14
     O                       DES(3)              45
     O                  32                       50 '13.'
     O                       CDE(13)             53
     O                       DES(13)             84
     O                  42                       89 '23.'
     O                       CDE(23)             92
     O                       DES(23)            123
     O          E            ROUTE          1
     O                  23                       11 '4.'
     O                       CDE(4)              14
     O                       DES(4)              45
     O                  33                       50 '14.'
     O                       CDE(14)             53
     O                       DES(14)             84
     O                  43                       89 '24.'
     O                       CDE(24)             92
     O                       DES(24)            123
     O          E            ROUTE          1
     O                  24                       11 '5.'
     O                       CDE(5)              14
     O                       DES(5)              45
     O                  34                       50 '15.'
     O                       CDE(15)             53
     O                       DES(15)             84
     O                  44                       89 '25.'
     O                       CDE(25)             92
     O                       DES(25)            123
     O          E            ROUTE          1
     O                  25                       11 '6.'
     O                       CDE(6)              14
     O                       DES(6)              45
     O                  35                       50 '16.'
     O                       CDE(16)             53
     O                       DES(16)             84
     O                  45                       89 '26.'
     O                       CDE(26)             92
     O                       DES(26)            123
     O          E            ROUTE          1
     O                  26                       11 '7.'
     O                       CDE(7)              14
     O                       DES(7)              45
     O                  36                       50 '17.'
     O                       CDE(17)             53
     O                       DES(17)             84
     O                  46                       89 '27.'
     O                       CDE(27)             92
     O                       DES(27)            123
     O          E            ROUTE          1
     O                  27                       11 '8.'
     O                       CDE(8)              14
     O                       DES(8)              45
     O                  37                       50 '18.'
     O                       CDE(18)             53
     O                       DES(18)             84
     O                  47                       89 '28.'
     O                       CDE(28)             92
     O                       DES(28)            123
     O          E            ROUTE          1
     O                  28                       11 '9.'
     O                       CDE(9)              14
     O                       DES(9)              45
     O                  38                       50 '19.'
     O                       CDE(19)             53
     O                       DES(19)             84
     O                  48                       89 '29.'
     O                       CDE(29)             92
     O                       DES(29)            123
     O          E            ROUTE          1
     O                  29                       11 '10.'
     O                       CDE(10)             14
     O                       DES(10)             45
     O                  39                       50 '20.'
     O                       CDE(20)             53
     O                       DES(20)             84
     O                  49                       89 '30.'
     O                       CDE(30)             92
     O                       DES(30)            123
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS. INC,.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O* Report Heading
     O*
     O          E            HEAD           1 06
     O                       DSTIME              12 '  :  :  '
     O                       XCNAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               14
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                       XXFDT               64 '  /  /  '
     O                                           66 '-'
     O                       XXTDT               75 '  /  /  '
     O          E            HEAD           2
     O                                           10 'GROUP:'
     O                       XXFND         Z     14
     O                       FNNAME              75
     O***  BEGIN CHG  ***  05/26/99  **********************************
     O************************          128 'POLICY YEAR:'
     O************************ XXFYR Z  132
     O                                           91 'POLICY YEAR:'
     O                       XXFYR         Z     95
     O                                          120 'PRINT W/NO ACTIVITY:'
     O                       RSPRNT             124
     O***  END CHG ******  05/26/99  **********************************
     O*
     O* HEADING LINE TO OVERPRINT MAIN HEADING
     O*
     O          E            HEAD           0
     O                                              ' EMPLOYER   DIV'
     O                                           20 'NAME'
     O                                           65 'ANNUAL PREM'
     O                                           72 'AGENT'
     O                                           85 'AGENT NAME'
     O          E            HEAD           2
     O                                              ' EMPLOYER   DIV'
     O                                           20 'NAME'
     O                                           65 'ANNUAL PREM'
     O                                           72 'AGENT'
     O                                           85 'AGENT NAME'
     O*
     O* SINGLE EMPLOYER HEADING LINE
     O*
     O***  BEGIN CHG  ***  05/26/99  **********************************
     O*
     O*  SUPPRESS PRINTING OF EMPLOYER DETAIL IF USER HAS INDICATED
     O*  "PRINT EMPLOYERS WITH NO ACTIVITY" = "N" AND EMPLOYER HAS
     O*  NO TRANSACTIONS IN THE DATE RANGE SPECIFIED
     O*
     O          E   N35N88   EMPHD          2
     O                                           22 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '____________________'
     O                                         +  0 '_________'
     O          E   N88      EMPHD          1
     O                       XXEMP#        Z     09
     O                       XXDIV         Z     15
     O                       XXNAME              42
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XXANPRB   67
     O                       XXANPR        A     67
     O***** END   CHG **** 11/24/99 ***********************************
     O                       XXAGT#        Z     72
     O                       XXAGNM             115
     O          E   N88      EMPHD          1
     O                                           18 'TRANS DT'
     O                                           33 'POST DT CD TP'
     O                                           45 'DESCRIPTION'
     O                                           85 'CHECK#   COLLECTION'
     O                                          100 'OTHER TRANS'
     O                                          115 'BILLING'
     O                                          130 'BALANCE'
     O*
     O* Previous Balance Line
     O*
     O          E   N88      PBA            1
     O                                           50 'PREVIOUS BALANCE:'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PBAL  B  132
     O                       PBAL          A    132
     O***** END   CHG **** 11/24/99 ***********************************
     O*
     O* Detail Line
     O*
     O          E   N88      DETAIL         1
     O                       XXTRDT              18 '  /  /  '
     O                       XXPODT              27 '  /  /  '
     O                       EAARCD              30
     O                       EAARTY              32
     O                       XXDESC              64
     O                       EACHK#        Z     72
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XXCOL B   87
     O**********               XXTRN B  102
     O**********               XXBIL B  117
     O**********               CBAL  B  132
     O                       XXCOL         A     87
     O                       XXTRN         A    102
     O                       XXBIL         A    117
     O                       CBAL          A    132
     O***** END   CHG **** 11/24/99 ***********************************
     O*
     O* Total Line for Employer
     O*
     O          E   N88      EMPTL       1  1
     O                                           72 'TOTAL:'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XXTCOLB   87
     O**********               XXTTRNB  102
     O**********               XXTBILB  117
     O**********               CBAL  B  132
     O                       XXTCOL        A     87
     O                       XXTTRN        A    102
     O                       XXTBIL        A    117
     O                       CBAL          A    132
     O***** END   CHG **** 11/24/99 ***********************************
     O          E   N88      EMPTL          1
     O                                           47 'DEPOSIT DUE'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XXDEPDB   67
     O                       XXDEPD        A     67
     O***** END   CHG **** 11/24/99 ***********************************
     O          E   N88      EMPTL          1
     O                                           47 'FEES    DUE'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XXFEEDB   67
     O                       XXFEED        A     67
     O***** END   CHG **** 11/24/99 ***********************************
     O          E   N88      EMPTL          1
     O                                           47 'PREMIUM DUE'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XXPRMDB   67
     O                       XXPRMD        A     67
     O***** END   CHG **** 11/24/99 ***********************************
     O          E   N88      EMPTL          2
     O                                           47 'TOTAL DUE'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XXDUE B   67
     O                       XXDUE         A     67
     O***** END   CHG **** 11/24/99 ***********************************
     O***  END CHG ******  05/26/99  **********************************
     O*
     O* Final Total Page
     O*
     O          E            HEAD1          1 06
     O                       DSTIME              12 '  :  :  '
     O                       XCNAME              96
     O                       DSDATE        Y    132
     O          E            HEAD1          1
     O                       Q1PGM               14
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD1          1
     O                       XXFDT               63 '  /  /  '
     O                                           65 '-'
     O                       XXTDT               74 '  /  /  '
     O          E            HEAD1          2
     O                                           10 'GROUP:'
     O                       XXFND         Z     13
     O                       FNNAME              75
     O                                           98 'POLICY YEAR'
     O                       XXFYR         Z   +  1
     O***  BEGIN ADD  ***  05/26/99  **********************************
     O                                          127 'PRINT W/OUT ACTIVITY:'
     O                       RSPRNT            +  1
     O***  END   CHG  ***  05/26/99  **********************************
     O***** BEGIN CHG **** 08/20/99 ***********************************
     O***** BEGIN CHG **** 07/26/99 ***********************************
     O*
     O*  Removed conditioning on printing of group summary
     O*
     O******* E  2    N26      HEAD1
     O******* E  2             HEAD1
     O          E   N26      HEAD1          2
     O                                          117 'GROUP TOTAL:    '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               FNTOT B  132
     O                       FNTOT         A    132
     O***** END   CHG **** 11/24/99 ***********************************
     O                                           47 'TOTAL ANNUAL PREMIUM:'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               FNANPRB   67
     O                       FNANPR        A     67
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  2    N26      HEAD1
     O******* E  2             HEAD1
     O          E   N26      HEAD1          2
     O                                           24 '** GROUP SUMMARY **'
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'ADJUSTMENTS     '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PATOT B   39
     O                       PATOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'BILLS           '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PBTOT B   39
     O                       PBTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'AUDITS          '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PDTOT B   39
     O                       PDTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'NSFS            '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PNTOT B   39
     O                       PNTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'PAYMENTS        '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PPTOT B   39
     O                       PPTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'VOIDS           '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PVTOT B   39
     O                       PVTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'REFUNDS         '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PRTOT B   39
     O                       PRTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           23 'DEPOSITS REQUESTED'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PHTOT B   39
     O                       PHTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           22 'DEPOSITS RECEIVED'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PITOT B   39
     O                       PITOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'FEES            '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PFTOT B   39
     O                       PFTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  2    N25      HEAD1
     O******* E  2             HEAD1
     O          E   N25      HEAD1          2
     O                                           21 'FEE PAYMENTS    '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PQTOT B   39
     O                       PQTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  2    N25      HEAD1
     O******* E  2             HEAD1
     O          E   N25      HEAD1          2
     O                                           21 'PREVIOUS BALANCE'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               TPBAL B   39
     O                       TPBAL         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'ADJUSTMENTS     '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               ATOT  B   39
     O                       ATOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'BILLS           '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               BTOT  B   39
     O                       BTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'AUDITS          '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               DTOT  B   39
     O                       DTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'NSFS            '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               NTOT  B   39
     O                       NTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'PAYMENTS        '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               PTOT  B   39
     O                       PTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'VOIDS           '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               VTOT  B   39
     O                       VTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'REFUNDS         '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O***********              RTOT  B   39
     O                       RTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           23 'DEPOSITS REQUESTED'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               HTOT  B   39
     O                       HTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           22 'DEPOSITS RECEIVED'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               ITOT  B   39
     O                       ITOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O                                           21 'FEES            '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               FTOT  B   39
     O                       FTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  2    N25      HEAD1
     O******* E  2             HEAD1
     O          E   N25      HEAD1          2
     O                                           21 'FEE PAYMENTS    '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               QTOT  B   39
     O                       QTOT          A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O******* E  1    N25      HEAD1
     O******* E  1             HEAD1
     O          E   N25      HEAD1          1
     O***** END   CHG **** 07/26/99 ***********************************
     O***** END   CHG **** 08/20/99 ***********************************
     O                                           21 'GROUP TOTAL     '
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               FNTOT B   39
     O                       FNTOT         A     39
     O***** END   CHG **** 11/24/99 ***********************************
     O*
     O* A/R Summary Heading
     O*
     O          E            ARHEAD      3  1
     O                                           29 '* * *   A / R   T Y P E'
     O                                           53 'S U M M A R Y   * * * '
     O          E            ARHEAD         2
     O                                           17 'CODE    TYPE'
     O                                           31 'DESCRIPTION'
     O                                           67 'TOTAL'
     O*
     O* A/R Summary Detail Line
     O*
     O          E            ARDTL          1
     O                       XCODE                8
     O                       XTYP                15
     O                       XDES                50
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               XYCNT B   69
     O                       XYCNT         A     69
     O***** END   CHG **** 11/24/99 ***********************************
     O*
     O* A/R TOTAL LINE
     O*
     O          E            ARTTL       1  1
     O                                           54 'TOTAL:'
     O***** BEGIN CHG **** 11/24/99 ***********************************
     O**********               SUMTTLB   69
     O                       SUMTTL        A     69
     O***** END   CHG **** 11/24/99 ***********************************
     O*
     O*** BEGIN ADD *** 03/24/98 **************************************
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC       1
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
     O*** END   ADD *** 03/24/98 **************************************
     O*
**     SRT
AGENT SEQUENCE
EMPLOYER NUMBER SEQUENCE
EMPLOYER NAME SEQUENCE
**  PLAN (07/19/99)
AALL POLICIES
EEXCLUDE ACCT CURRENT
CACCT CURRENT ONLY
