     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H/TITLE W4086R - Large Loss Report
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  01/21/13  C:287 SEQ#: SY266_OVR                              *
     H*                  Add default activation group compiler options*
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/21/08 C:191 PROJECT: 102WEP                               *
     H*                 BACK TO BASE: CLIENT FLAGGED                  *
     H*              - Remove column for "Amount in Excess".          *
     H*              - Print Employer Name.                           *
     H*              - Add column for "Field Adjustor".               *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include SS# and claimant  *
     H*                 name                                          *
     H*                                                               *
     H* 10/09/00 C:61  PROJECT WC1005                                 *
     H*              - Change for Print, File, Both output type.      *
     H*              - Select by policy type (open, closed, both).    *
     H*              - Select one or all policy years.                *
     H*                                                               *
     H*  05/05/00  C:15  Support: WC096/22751                         *
     H*                  Chgd pgm to use the reserve history eff dt   *
     H*                  instead of the maint date to determine when  *
     H*                  the record was written.                      *
     H*                                                               *
     H*  12/16/99  C:90  PROJECT: WC577                               *
     H*                  Y2K FIX - LEFT JUSTIFY TITLE ON ROUTING PAGE.*
     H*                                                               *
     H*  08/20/99  C:90  PROJECT:WC748                                *
     H*                  CHANGED PROCESSING TO COMPUTE 'OUTSTANDING   *
     H*                  RESERVES' AND 'TOTAL INCURRED' AS AN         *
     H*                  ACCUMULATION OF ALL WMRHP RECORDS THROUGH    *
     H*                  THE ENTERED DATE FOR EACH CLAIM.             *
     H*                                                               *
     H*  12/10/98  C:15  Support: WC082/15461                         *
     H*                  Changed the overflow handling so the data    *
     H*                  for a claim is all on one page.              *
     H*                                                               *
     H*  04/17/98  C:35  PROJECT: SUPPORT WA078 11203                 *
     H*                  CURRENT DATE WAS PRINTING IN PLACE OF        *
     H*                  ACCIDENT DATE ON DETAIL LINES.               *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
     H*                                                               *
     H* JJJ REVISED 2-2-89 - ACCUMULATE BY PAYMENT CODE & PRINT
     H* SUMMARY AT END OF REPORT
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H* 71 - NO PRINTED OUTPUT - ONLY FILE OUTPUT WFLLP               *
     H* 72 - ALL POLICY YEARS                                         *
     H* 73 - PRINT HEADER FOR NO RECORDS FOUND PAGE                   *
     H* 74 - PRINT HEADER FOR ADDITIONAL LR PMT CODE SUMMARY PAGES    *
     H*                                                               *
     H*****************************************************************
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter File
     FWMUPL     IF   E           K DISK
     F*
     F* Output Routing Master File
     FSMORL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Claim Master File
     FWMCML5    IF   E           K DISK
     F*
     F* Claimant Demographic Master File
     FWMCDL     IF   E           K DISK
     F*
     F* Claim History Master File
     FWMCHL     IF   E           K DISK
     F*
     F* Claims Check Payment Detail File (co#, fnd, fyr, case)
     FWDPAL3    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     F*
     F* Claims Check Payment Detail File (co#, fnd, fyr, case, seq#)
     FWTABL     IF   E           K DISK
     F*
      **** BEGIN ADD **** 08/20/99 ************************************
     FWMRHL1    IF   E           K DISK
      **** END   ADD **** 08/20/99 ************************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      * System User Parameter
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *
      * Policy Year Detail
     FWDF1L     IF   E           K DISK
      *
      * Employer Master
     FWMEML     IF   E           K DISK
      *
      * Output File
     FWFLLP     O    E           K DISK
      ***** END ADD ******* 10/09/00 **********************************
      *
     F* Printer Output File
      ***** BEG CHG ******* 10/09/00 **********************************
     F*QSYSPRT O   F     198     OA     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
      ***** END CHG ******* 10/09/00 **********************************
     F*** BEGIN ADD *** 12/10/98 **************************************
     F                                     INFDS(PIDS01)
     F*** END ADD ***** 12/10/98 **************************************
     F*

     D*
     D* Define Arrays
     D TYP             S              2    DIM(20)
     D AMT             S             11    DIM(20)
     D COMB            S             13    DIM(20)
     D PCOD            S              2    DIM(999)
     D PAMT            S             11  2 DIM(999)
     D PDES            S             40    DIM(999)
      ***** BEG ADD ******* 10/09/00 **********************************
     D FAMT            S             11  2 DIM(999)
      ***** END ADD ******* 10/09/00 **********************************
      **** BEGIN ADD **** 08/20/99 ************************************
      **** END   ADD **** 08/20/99 ************************************
      *
      ***** BEG ADD ******* 10/09/00 **********************************
     D X1TITL          C                   CONST('Large Loss Report')
      ***** END ADD ******* 10/09/00 **********************************
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
      *** END ADD ****** 05/25/07 **********************************
     D*
     D* Reserve Type
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D*
     D* Payments
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D*
     D* Remaining Reserves
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D*
     D* Combination Array
     D                 DS
     D  XCOMB                  1     13
     D  XTYP                   1      2
     D  XAMT                   3     13  2
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXDTE                  1      8  0
     D  XXMMDD                 1      4  0
     D  XXYR                   7      8  0
     D*
     D                 DS
     D  X1DTE                  1      6  0
     D  X1MMDD                 1      4  0
     D  X1YR                   5      6  0
     D*
     D                 DS
     D  X2DTE                  1      6  0
     D  X2MMDD                 1      4  0
     D  X2YR                   5      6  0
      **** BEGIN ADD **** 08/20/99 ************************************
     D                 DS
     D  RHS                    1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
      **** END   ADD **** 08/20/99 ************************************
     D*
     D* External Program Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEGIN ADD *** 12/10/98 **************************************
     D PIDS01        E DS                  EXTNAME(SZQ4P)

     I*** END ADD ***** 12/10/98 **************************************
     I*
     I*

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Output Routing Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Claims Master File
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Key List for Claimant Demographic Master File
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C* Key List for the Claim History Master File
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE            7 0
     C*
     C* Key List for the Claims Payment Detail File
     C*
     C     P2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C*
      **** BEGIN ADD **** 08/20/99 ************************************
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      **** END   ADD **** 08/20/99 ************************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      *  Policy Year Detail
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *
      *
      ***** END ADD ******* 10/09/00 **********************************
      **** BEGIN ADD **** 12/16/99 ************************************
     C     *LIKE         DEFINE    ORTITL        XXTITL
      **** END   ADD **** 12/16/99 ************************************
      ***** BEG ADD ******* 10/09/00 **********************************
     C     *LIKE         DEFINE    LLCLNM        XXCLNM
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQAMT            11
     C                   PARM                    QQQUE            10
     C                   PARM                    QQDATE            8
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   PARM                    QQOCB             1
     C                   PARM                    QQPFB             1
      ***** END ADD ******* 10/09/00 **********************************
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
      **** BEGIN ADD **** 04/17/98 *********************************************
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      **** END   ADD **** 04/17/98 *********************************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C* Set-up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQAMT         XXAMT            11 2
     C*** BEG ADD ****** 08/21/08 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C* SET INDICATOR 75 FOR CLT 102
     C                   SETOFF                                       75
     C     XXCLT#        IFEQ      '102'
     C                   SETON                                        75
     C                   ENDIF
     C*** END ADD ****** 08/21/08 ************************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     C     QQPFB         COMP      'F'                                    71
     C     XXFYR         COMP      *ZERO                                  72
     C  N71              OPEN      QSYSPRT
      *
     C                   MOVE      *ALL'_'       LINE            198
      *
     C                   Z-ADD     0             LROPEN            6 0
     C                   Z-ADD     0             LRCLOS            6 0
     C                   Z-ADD     0             LRTLCL            6 0
     C                   Z-ADD     0             LRTLEX           11 2
     C                   Z-ADD     0             LRTLIN           11 2
     C                   Z-ADD     0             LRTLOR           11 2
     C                   Z-ADD     0             LRTLPD           11 2
     C                   Z-ADD     0             FAMT
      *
     C                   MOVE      *BLANKS       PCOD
     C                   MOVE      *BLANKS       PDES
      *
      *  Read Payment Code file and Load into table
      *
     C                   Z-ADD     1             X                 4 0
     C     TBKEY         CHAIN     PPC01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     PCDESC        PDES(X)
     C                   MOVEL     PCKEY         PCOD(X)
     C                   ADD       1             X
     C     TBKEY         READE     PPC01                                  60
     C                   ENDDO
      *
      ***** END ADD ******* 10/09/00 **********************************
      *
     C                   TIME                    DSDTTM
     C                   MOVE      QQDATE        XXDTE             8 0
     C                   MOVEL     XXMMDD        X1MMDD
     C                   MOVEL     XXYR          X1YR
      *
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
      *
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1RNDT            8 0
      *
      ***** END ADD ******* 10/09/00 **********************************
      **** BEGIN ADD **** 12/16/99 ************************************
     C                   MOVE      *BLANKS       XXTITL
      **** END   ADD **** 12/16/99 ************************************
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C* Get Company Record
     C*
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     WMUPL                              60
     C*
     C* Get the Routing Record
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      **** BEGIN ADD **** 12/16/99 ************************************
     C*  LEFT JUSTIFY TITLE
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
      **** END   ADD **** 12/16/99 ************************************
      ***** BEG DEL ******* 10/09/00 **********************************
     C**************       EXCPTROUTE
     C**************       END
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
      ****** Beg Del ***** 08/26/14 ***********************************
     C**************  N71              EXCEPT    ROUTE
      ****** End Del ***** 08/26/14 ***********************************
      *
     C                   ELSE
      *
     C                   MOVEL(P)  X1TITL        XXTITL
     C                   ENDIF
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  XXTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXTITL
      *
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* Get the Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   MOVEL     ##DATA        X1NAME           60
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
      ***** BEG DEL ******* 10/09/00 **********************************
     C**************       EXCPTHEAD
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      *  Policy Year Loop
      *
     C     XXFYR         IFNE      *ZERO
     C                   EXSR      SRFYR
      *
     C                   ELSE
      *
     C     F1KY          CHAIN     PF101                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     F1FYR         XXFYR
     C                   EXSR      SRFYR
     C     F1KY          READE     PF101                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
      * Output Final Totals
      *
     C     LRTLCL        IFNE      *ZERO
     C  N71
     CAN 72              EXCEPT    LRTOT
     C  N71
     CAN 72              EXSR      FINPCD
      *
     C                   ELSE
      *
     C                   SETON                                        7374
     C  N71              EXCEPT    LRSHD
     C  N71              EXCEPT    NOREC
     C                   ENDIF
      /Free
       // ***** Beg Add ***** 08/26/14 ********************************
       ///  If not %Open(Qsysprt) and qqPfb <> 'F';
       ///  Open Qsysprt;
       ///  If *in71 = *off;
       ///     Except Route;
       ///  Endif;
       ///Endif;
       If %Open(Qsysprt);
       xxPrt = '0';
       Chain orKey POR01;
          If %found;
             If *in71 = *off;
                Except Route;
             Endif;
          Endif;
       Endif;
       // ***** End Add ***** 08/26/14 ********************************
      /End-free
      *
     C  N71              CLOSE     QSYSPRT
     C                   SETON                                        LR
      *
      *
      *
      *================================================================
      * SRFYR - Processing for Policy year
      *================================================================
     C     SRFYR         BEGSR
      *
     C                   SETON                                        OA
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* Initialize Total Fields
     C*
     C                   Z-ADD     0             XXCASE
     C                   Z-ADD     0             XXOPEN            6 0
     C                   Z-ADD     0             XXCLOS            6 0
     C                   Z-ADD     0             XXTLCL            6 0
     C                   Z-ADD     0             XXTLEX           11 2
     C                   Z-ADD     0             XXTLIN           11 2
     C                   Z-ADD     0             XXTLOR           11 2
     C                   Z-ADD     0             XXTLPD           11 2
      ***** BEG DEL ******* 10/09/00 **********************************
     C*********            MOVE *BLANKS   PCOD             JJJ 2-2-89
     C*********            MOVE *BLANKS   PDES             |
      ***** END DEL ******* 10/09/00 **********************************
     C                   Z-ADD     0             PAMT
      ***** BEG DEL ******* 10/09/00 **********************************
     C*********                                            |
     C***READ*PAYMENT CODE FILE & LOAD INTO ARRAY          |
     C*********                                            |
     C*********            Z-ADD1         X       40       |
     C*********  TBKEY     CHAINPPC01                60    |
     C*********  *IN60     DOWEQ'0'                        |
     C*********            MOVELPCDESC    PDES,X           |
     C*********            MOVELPCKEY     PCOD,X           |
     C*********            ADD  1         X                |
     C*********  TBKEY     READEPPC01                    60|
     C*********            END                             |
      ***** END DEL ******* 10/09/00 **********************************
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      CMCASE        XXCASE
      *
     C                   Z-ADD     CMACDT        SRDTI
      **** BEGIN CHG **** 04/17/98 *********************************************
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     XXACDT  80
     C******************** MOVELXXMMDD    X2MMDD
     C******************** MOVELXXYR      X2YR
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2DTE
      **** END   CHG **** 04/17/98 *********************************************
      *
     C                   MOVE      *BLANKS       XXSTAT            1
     C                   MOVE      *BLANKS       XXACDS           60
     C*** BEG ADD ****** 08/21/08 ************************************
     C                   MOVE      *BLANKS       XXACD1           40
     C                   MOVEL     CMACDS        XXACD1
     C*** END ADD ****** 08/21/08 ************************************
     C                   MOVEL     CMACDS        XXACDS
     C*
     C* Initialize Detail Fields
     C*
     C                   MOVEA     *BLANKS       TYP
     C                   MOVEA     *BLANKS       AMT
     C                   MOVEA     *BLANKS       COMB
     C                   Z-ADD     0             XXPAID           11 2
     C                   Z-ADD     0             XXORES           11 2
     C                   Z-ADD     0             XXRRES           11 2
     C                   Z-ADD     0             XXINC            11 2
     C                   Z-ADD     0             XXEXCS           11 2
     C*
     C* Accumulate Remaining Reserves and Outstanding Reserves to Cal-
     C* culate Total Incurred.
     C*
      **** BEGIN CHG **** 08/20/99 ************************************
     C**********           Z-ADD1         R
     C**********           DO   10        R       20
     C**********           ADD  CMA,R     XXRRES           Remain Res
     C**********           ADD  CMR,R     XXORES           Out Reserves
     C**********           END
     C********** XXRRES    ADD  XXORES    XXINC            Incurred Amt
      *
     C     RHKY          CHAIN     PRH01                              61
     C*
     C     *IN61         DOWEQ     *OFF
     C*** BEGIN CHG *** 05/05/00 **************************************
     C********** RHDT      IFLE XXDATE
     C     RHEFDT        IFLE      XXDATE
     C*** END CHG ***** 05/05/00 **************************************
     C*
     C     1             DO        10            R                 2 0
     C*
     C                   ADD       RHS(R)        XXINC
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C     RHKY          READE     PRH01                                  61
     C*
     C                   ENDDO
     C*
      **** END   CHG **** 08/20/99 ************************************
     C     XXINC         SUB       XXAMT         XXEXCS
     C*
     C* If Total Incurred Losses are > or = to Entered Amount, Print
     C* Detail Line and Accumulate Totals.
     C*
     C     XXINC         IFGE      XXAMT
     C*
     C* GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     CHCO#         IFEQ      CMCO#
     C     CHFND         ANDEQ     CMFND
     C     CHFYR         ANDEQ     CMFYR
     C     CHCASE        ANDEQ     CMCASE
     C*
     C     CHOCST        IFEQ      'C'
     C                   MOVE      'C'           XXSTAT
     C                   ELSE
     C                   MOVE      'O'           XXSTAT
     C                   END
     C*
     C                   ELSE
     C*
     C*  IF NO CLAIM STATUS RECORD FOUND USE CMSTAT
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'C'           XXSTAT
     C                   ELSE
     C                   MOVE      'O'           XXSTAT
     C                   END
     C*
     C                   END
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      * Select by claim type (open, closed, both)
      *
     C     QQOCB         IFEQ      'B'
     C     QQOCB         OREQ      XXSTAT
      *
      ***** END ADD ******* 10/09/00 **********************************
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          XXAUTH            1
     C                   ELSE
     C                   MOVE      *ON           XXAUTH
     C                   ENDIF
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C* Get the Claimant Demographics Record
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C* Call Pgm to Re-arrange Claimant Name
     C*
     C                   MOVE      *BLANKS       XXNAME           24
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
      *
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME
      ***** BEG ADD ******* 10/09/00 **********************************
     C  N60              MOVEL(P)  QQLAST        XXCLNM
     C   60              MOVEL(P)  *BLANKS       XXCLNM
      ***** END ADD ******* 10/09/00 **********************************
     C*** BEG ADD ****** 05/25/07 **********************************************
     C     XXAUTH        IFEQ      *OFF
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       XXCLNM
     C                   MOVEL(P)  XXATTX        XXNAME
     C                   MOVEL(P)  XXATTX        XXCLNM
     C                   ENDIF
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C* Get Claims Payment File for Accumulating Payments
     C*
     C     P2KEY         CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATRDT        IFLE      XXDATE
     C*
     C* NEW ALGORITHM
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ADD       PAAMT         XXPAID
     C                   EXSR      SUM
     C                   ELSE
     C                   SUB       PAAMT         XXPAID
     C                   MULT      -1            PAAMT
     C                   EXSR      SUM
     C                   END
     C*
     C                   END
     C*
     C     P2KEY         READE     PPA02                                  60
     C                   END
     C*
     C*** BEGIN DLT *** 12/10/98 **************************************
     C***OA*****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           EXCPTDETAIL
     C*** END DLT ***** 12/10/98 **************************************
     C*
     C                   ADD       1             XXTLCL
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   ADD       1             LRTLCL
      ***** END ADD ******* 10/09/00 **********************************
      *
     C     XXSTAT        IFEQ      'C'
     C                   ADD       1             XXCLOS
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   ADD       1             LRCLOS
      ***** END ADD ******* 10/09/00 **********************************
     C                   ELSE
     C     XXSTAT        IFEQ      'O'
     C                   ADD       1             XXOPEN
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   ADD       1             LROPEN
      ***** END ADD ******* 10/09/00 **********************************
     C                   END
     C                   END
     C*
      **** BEGIN ADD **** 08/20/99 ************************************
     C     XXINC         SUB       XXPAID        XXORES
      **** END   ADD **** 08/20/99 ************************************
     C                   ADD       XXPAID        XXTLPD
     C                   ADD       XXORES        XXTLOR
     C                   ADD       XXINC         XXTLIN
     C                   ADD       XXEXCS        XXTLEX
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   ADD       XXPAID        LRTLPD
     C                   ADD       XXORES        LRTLOR
     C                   ADD       XXINC         LRTLIN
     C                   ADD       XXEXCS        LRTLEX
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* Print Total Paid by Type for Arrays
     C*
     C                   EXSR      SUMPRT
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     C                   ENDIF
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C                   END
     C*
     C* Get the next claim record
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C*
     C* Print Totals
     C*
      ***** BEG ADD ******* 10/09/00 **********************************
     C     XXTLCL        IFNE      *ZERO
      ***** END ADD ******* 10/09/00 **********************************
      ***** BEG DEL ******* 10/09/00 **********************************
     C***OA*************   EXCPTHEAD
     C******************   MOVE '0'       *INOA
     C******************   EXCPTTOTAL
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
     C   OA
     CANN71              EXCEPT    HEAD
     C  N71              MOVE      '0'           *INOA
     C  N71              EXCEPT    TOTAL
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C*  PRINT PAYMENT CODE SUMMARY
     C*
     C                   MOVE      '1'           *INOA
     C                   Z-ADD     1             X
     C     PCOD(X)       DOWNE     *BLANKS
     C*
     C     *INOA         IFEQ      '1'
      ***** BEG CHG ******* 10/09/00 **********************************
     C************         EXCPTSUMHD
     C************         MOVE '0'       *INOA
     C  N71              EXCEPT    SUMHD
     C  N71              MOVE      '0'           *INOA
      ***** END CHG ******* 10/09/00 **********************************
     C                   END
     C*
     C     PAMT(X)       IFNE      0
      ***** BEG CHG ******* 10/09/00 **********************************
     C*************        EXCPTSUMDTL
     C  N71              EXCEPT    SUMDTL
      ***** END CHG ******* 10/09/00 **********************************
     C                   END
     C                   ADD       1             X
     C*
     C                   END
     C*
      ***** BEG DEL ******* 10/09/00 **********************************
     C****************     MOVE '1'       *INLR
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      *
      *================================================================
      * SUBROUTINE    **  FINPCD ** OUTPUT FINAL PAYMENT CODE SUMMARY
      *================================================================
      *
     CSR   FINPCD        BEGSR
      *
      * Print Final Payment Code Summary
      *
     C     LRTLCL        IFNE      *ZERO
     C  N71              EXCEPT    LRSHD
     C  N71              MOVE      '0'           *INOA
     C                   SETON                                        74
     C                   Z-ADD     1             X
      *
     C     PCOD(X)       DOWNE     *BLANKS
      *
     C     *INOA         IFEQ      '1'
     C  N71              EXCEPT    LRSHD
     C  N71              MOVE      '0'           *INOA
     C                   ENDIF
      *
     C     FAMT(X)       IFNE      0
     C  N71              EXCEPT    LRSDTL
     C                   ENDIF
      *
     C                   ADD       1             X
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      *
      *================================================================
      * SUBROUTINE    **  SROUT ** WRITE TO OUTPUT FILE * WFLLP       *
      *================================================================
      *
     CSR   SROUT         BEGSR
      *
     C                   CLEAR                   PLL01
      *
     C                   Z-ADD     XXCO#         LLCO#
     C                   MOVEL(P)  X1NAME        LLCNAM
     C                   Z-ADD     XXFND         LLFND
     C                   MOVEL(P)  FNNAME        LLFNAM
     C                   Z-ADD     CMEMP#        LLEMP#
     C                   Z-ADD     CMDIV         LLDIV
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        LLENAM
     C                   MOVE      EMLREP        LLLREP
     C     LLLREP        CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        LLLRNM
     C   60              MOVEL(P)  *BLANKS       LLLRNM
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       LLENAM
     C                   MOVE      *BLANKS       LLLREP
     C                   MOVEL(P)  *BLANKS       LLLRNM
     C                   ENDIF
      *
     C                   Z-ADD     XXFYR         LLFYR
     C                   Z-ADD     CMCASE        LLCASE
     C                   MOVEL(P)  XXCLNM        LLCLNM
     C                   MOVEL(P)  XXSTAT        LLSTAT
      *
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         LLACDT
      *
     C                   MOVEL(P)  CMACDS        LLACDS
     C                   Z-ADD     CMCLAS        LLCLAS
     C                   Z-ADD     XXPAID        LLPAID
     C                   Z-ADD     XXORES        LLOUTS
     C                   Z-ADD     XXINC         LLTINC
     C                   Z-ADD     XXEXCS        LLEXCS
     C                   Z-ADD     X1RNDT        LLRNDT
     C                   Z-ADD     XXAMT         LLLAMT
      *
      *
     C                   MOVEL     Q1USER        LLUSER
     C                   MOVEL     Q1PGM         LLPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LLDT
      *
     C                   MOVEL     DSTIME        LLTIME
      *
     C                   WRITE     PLL01
      *
     C                   ENDSR
      *
      *
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C*================================================================
     C* SUBROUTINE    **  SUM  **  LOAD TRANS CODES INTO ARRAYS       *
     C*================================================================
     C*
     CSR   SUM           BEGSR
     C*                                                    JJJ 2-2-89
     C*   FIND PAYMENT CODE IN ARRAY AND ACCUMULATE AMOUNTS|
     C*                                                    |
     C                   Z-ADD     1             V
     C     PATRCD        LOOKUP    PCOD(V)                                33
     C     *IN33         IFEQ      '1'
     C                   ADD       PAAMT         PAMT(V)
      ***** BEG ADD ******* 10/09/00 **********************************
     C                   ADD       PAAMT         FAMT(V)
      ***** END ADD ******* 10/09/00 **********************************
     C                   END
     C*
     C* Check to see if Code exists in the Array
     C*
     C                   Z-ADD     1             V                 5 0
     C                   MOVE      '1'           *IN33
     C*
     C     PATRCD        LOOKUP    TYP(V)                                 33
     C*
     C     *IN33         IFEQ      '0'
     C                   Z-ADD     1             V
     C*
     C* Find Next Available Position
     C*
     C     TYP(V)        DOWNE     *BLANKS
     C                   ADD       1             V
     C                   END
     C*
     C* Add Fields to the Array
     C*
     C                   MOVE      *BLANKS       X2AMT            11
     C*
     C                   Z-ADD     PAAMT         X1AMT            11 2
     C                   MOVE      X1AMT         X2AMT
     C                   MOVEA     X2AMT         AMT(V)
     C                   MOVEA     PATRCD        TYP(V)
     C*
     C* If Code is in the Array Add to Existing Total
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             X1AMT
     C                   MOVE      *BLANKS       X2AMT
     C                   MOVEA     AMT(V)        X2AMT
     C                   MOVE      X2AMT         X1AMT
     C                   ADD       PAAMT         X1AMT
     C                   MOVE      X1AMT         X2AMT
     C                   MOVEA     X2AMT         AMT(V)
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C* PRINT TRANSACTION CODE SUMMARY         Subroutine  ** SUMPRT  =  *
     C*==================================================================*
     C*
     CSR   SUMPRT        BEGSR
     C*
     C* Move Code and Total Arrays to Combination Array
     C*
     C                   Z-ADD     1             V
     C*
     C     TYP(V)        DOWNE     *BLANKS
     C                   MOVEL     TYP(V)        COMB(V)
     C                   MOVE      AMT(V)        COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C*** BEGIN ADD *** 12/10/98 **************************************
     C*  If there is not enough room on the page to print all of the
     C*    detail, skip to a new page.
     C*
     C     60            SUB       V             XXLIN#            5 0
     C     Q4LIN#        IFGT      XXLIN#
      ***** BEG DEL ******* 10/09/00 **********************************
     C**************       EXCPTHEAD
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
     C     *INOA         OREQ      *ON
     C   OA              SETOFF                                       OA
     C  N71              EXCEPT    HEAD
      ***** END ADD ******* 10/09/00 **********************************
     C                   END
     C*** BEG ADD ****** 08/21/08 ************************************
      * Get Employer Name
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMNAME        XXENAM           30
     C   60              MOVEL(P)  *BLANKS       XXENAM
     C*** END ADD ****** 08/21/08 ************************************
      ***** BEG CHG ******* 10/09/00 **********************************
     C**************       EXCPTDETAIL
     C  N71              EXCEPT    DETAIL
      ***** END CHG ******* 10/09/00 **********************************
     C*** END ADD ***** 12/10/98 **************************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      * Write to output file
      *
     C     QQPFB         IFEQ      'F'
     C     QQPFB         OREQ      'B'
     C                   EXSR      SROUT
     C                   ENDIF
      *
      ***** END ADD ******* 10/09/00 **********************************
     C*
     C* If there are Records in the File Continue
     C*
     C     V             IFNE      1
     C*
     C* Sort the Array
     C*
     C                   SORTA     COMB
     C*
     C* Find the First Record
     C*
     C                   Z-ADD     1             X1                5 0
     C     COMB(X1)      DOWEQ     *BLANKS
     C                   ADD       1             X1
     C                   END
     C*
     C     X1            DOWLE     20
     C                   MOVE      *BLANKS       XCOMB
     C                   MOVEA     COMB(X1)      XCOMB
     C*
     C*  PRINT DETAIL LINE
     C*
     C*** BEGIN DLT *** 12/10/98 **************************************
     C********** *INOA     IFEQ '1'
     C**********           EXCPTHEAD
     C**********           EXCPTHEADOF
     C**********           MOVE '0'       *INOA
     C**********           END
     C*** END DLT ***** 12/10/98 **************************************
     C*
      ***** BEG CHG ******* 10/09/00 **********************************
     C**********           EXCPTDETL1
     C  N71              EXCEPT    DETL1
      ***** END CHG ******* 10/09/00 **********************************
     C                   ADD       1             X1
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
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
      **** BEGIN CHG **** 12/16/99 ************************************
     O*******************      ORTITL    80
     O                       XXTITL              80
      **** END   CHG **** 12/16/99 ************************************
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********              OROUTQ              30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*** PRINT SELECTION CRITERIA
     O*
      ***** BEG DEL ******* 10/09/00 **********************************
     O******* E  233           ROUTE
     O*******                            18 'REPORT SELECTIONS:'
     O******* E  2             ROUTE
     O*******                            26 'COMPANY        :'
     O*******                  XXCO#     41
     O******* E  2             ROUTE
     O*******                            26 'GROUP          :'
     O*******                  XXFND     41
     O******* E  2             ROUTE
     O*******                            26 'POLICY YEAR    :'
     O*******                  XXFYR Z   41
     O******* E  2             ROUTE
     O*******                            26 'LOSS AMOUNT    :'
     O*******                  XXAMT J   42
     O******* E  2             ROUTE
     O*******                            26 'AS OF DATE     :'
     O*******                  X1DTE     41 '  /  /  '
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     O          E            ROUTE          2 33
     O                                           24 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                           30 'COMPANY  . . . . . :'
     O                       XXCO#             +001
      *
     O          E            ROUTE          2
     O                                           30 'GROUP  . . . . . . :'
     O                       XXFND             +001
      *
     O          E            ROUTE          2
     O                                           30 'POLICY YEAR  . . . :'
     O              N72      XXFYR         Z   +001
     O               72                          35 '*ALL'
      *
     O          E            ROUTE          2
     O                                           30 'LOSS AMOUNT  . . . :'
     O                       XXAMT         J   +001
      *
     O          E            ROUTE          2
     O                                           30 'AS OF DATE . . . . :'
     O                       X1DTE             +001 '  /  /  '
      *
     O          E            ROUTE          2
     O                                           30 'CLAIM STATUS . . . :'
     O                       QQOCB             +001
     O                                         +010 '(O=Open,'
     O                                         +001 'C=Closed,'
     O                                         +001 'B=Both)'
      *
     O          E            ROUTE          2
     O                                           30 'OUTPUT TYPE  . . . :'
     O                       QQPFB             +001
     O                                         +010 '(P=Printed,'
     O                                         +001 'F=File,'
     O                                         +001 'B=Both)'
      *
      ***** END ADD ******* 10/09/00 **********************************
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O***                              +000 '(C)'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O* Heading Specs
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             128
     O                       DSDATE        Y    198
      *
     O          E            HEAD           2
     O                       Q1PGM               10
      ***** BEG CHG ******* 10/09/00 **********************************
     O*************            ORTITL   127
     O                       XXTITL             128
      ***** END CHG ******* 10/09/00 **********************************
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      *
      ***** BEG DEL ******* 10/09/00 **********************************
     O********E**2**           HEAD
     O**************                        'GROUP:'
     O**************           XXFND Z +  1
     O**************           FNNAME  +  1
     O**************                     82 'POLICY YEAR:'
     O**************           XXFYR Z +  1
     O**************                    122 'LOSS AMOUNT:'
     O**************           XXAMT J +  1
     O**************                    189 'AS OF DATE:'
     O**************           X1DTE    198 '  /  /  '
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       FNNAME            +  2
     O                                         +005 'LOSS AMOUNT:'
     O                       XXAMT         J   +  1
     O                                         +005 'AS OF DATE:'
     O                       X1DTE             +001 '  /  /  '
     O                                         +005 'CLAIM STATUS:'
     O                       QQOCB             +001
     O                                         +005 'OUTPUT TYPE:'
     O                       QQPFB             +001
     O               72                        +005 'POLICY YEAR: *ALL'
      *
     O          E            HEAD           2
     O                                         +000 'POLICY YEAR:'
     O                       XXFYR         Z   +001
      *
     O          E            HEAD        0  0
     O                       LINE              +000
      ***** END ADD ******* 10/09/00 **********************************
      *
     O*** BEG CHG ****** 08/21/08 ************************************
     O******* E  1             HEAD
     O          E   N75      HEAD           1
     O*** END CHG ****** 08/21/08 ************************************
     O                                           09 'EMPLOYER'
     O                                           15 'DIV'
     O                                           32 'CASE CLAIMANT'
     O                                           50 'STS'
     O                                           72 'AC DATE  DESCRIPTION'
     O                                          127 'CLASS'
     O                                          146 'TOTAL PAID  CD'
     O                                          160 'OUTS RESERVE'
     O                                          176 'TOTAL INCURRED'
     O                                          192 'AMT IN EXCESS'
     O*
     O* Detail Line Specs
     O*
     O*** BEG CHG ****** 08/21/08 ************************************
     O******  E 11             DETAIL
     O          E   N75      DETAIL      1  1
     O*** END CHG ****** 08/21/08 ************************************
     O                       CMEMP#        Z     09
     O                       CMDIV         Z     15
     O                       CMCASE        Z     23
     O                       XXNAME              48
     O                       XXSTAT              49
     O                       X2DTE               59 '  /  /  '
     O                       XXACDS             121
     O                       CMCLAS        Z    127
     O                       XXPAID        J    143
     O                       XXORES        J    161
     O                       XXINC         J    177
     O                       XXEXCS        J    193
     O*
     O* Total Paid Detail Line Specs
     O*
     O*** BEG ADD ****** 08/21/08 ************************************
     O******* E  1             DETL1
     O          E   N75      DETL1          1
     O*** END ADD ****** 08/21/08 ************************************
     O                       XAMT          J    143
     O                       XTYP               146
     O*** BEG ADD ****** 08/21/08 ************************************
      *
     O          E    75      HEAD           1
     O                                         +000 'REP'
     O                                         +002 'EMPLOYER '
     O                                         +001 'DIV  '
     O                                         +001 'NAME'
     O                                         +031 'CASE'
     O                                         +001 'CLAIMANT'
     O                                         +017 'STS'
     O                                         +002 'AC DATE'
     O                                         +002 'DESCRIPTION'
     O                                         +031 'CLASS'
     O                                         +005 'TOTAL PAID  CD'
     O                                         +002 'OUTS RESERVE'
     O                                         +003 'TOTAL INCURRED'
      *
      * Detail Line Specs
      *
     O          E    75      DETAIL      1  1
     O                       EMLREP            +000
     O                       CMEMP#        Z   +002
     O                       CMDIV         Z   +001
     O                       XXENAM            +001
     O                       CMCASE        Z   +002
     O                       XXNAME            +001
     O                       XXSTAT            +002
     O                       X2DTE             +002 '  /  /  '
     O                       XXACD1            +002
     O                       CMCLAS        Z   +002
     O                       XXPAID        J   +001
     O                       XXORES        J   +003
     O                       XXINC         J   +002
      *
      * Total Paid Detail Line Specs
      *
     O          E    75      DETL1          1
     O                       XAMT          J    163
     O                       XTYP              +001
      *
     O*** END ADD ****** 08/21/08 ************************************
     O*
     O*** BEGIN DLT *** 12/10/98 **************************************
     O* Total Paid Heading in case of Page Overflow
     O*******
     O******* E 11             HEADOF
     O*******                  CMEMP#Z   09
     O*******                  CMDIV Z   15
     O*******                  CMCASEZ   23
     O*******                  XXNAME    48
     O*** END DLT ***** 12/10/98 **************************************
     O*
     O* Total Line Specs
     O*
     O*** BEG CHG ****** 08/21/08 ************************************
     O******* E 21             TOTAL
     O          E   N75      TOTAL       2  1
     O*** END CHG ****** 08/21/08 ************************************
     O                                           17 'TOTALS:'
     O                       XXOPEN        J     77
     O                                           82 'OPEN'
     O                       XXCLOS        J     90
     O                                           97 'CLOSED'
     O                       XXTLCL        J    106
     O                                          118 'TOTAL CASES'
     O                       XXTLPD        J    143
     O                       XXTLOR        J    161
     O                       XXTLIN        J    177
     O                       XXTLEX        J    193
     O*** BEG ADD ****** 08/21/08 ************************************
      *
      * Total Line Specs
      *
     O          E    75      TOTAL       2  1
     O                                           17 'TOTALS:'
     O                       XXOPEN        J     77
     O                                           82 'OPEN'
     O                       XXCLOS        J     90
     O                                           97 'CLOSED'
     O                       XXTLCL        J    106
     O                                          118 'TOTAL CASES'
     O                       XXTLPD        J    163
     O                       XXTLOR        J    181
     O                       XXTLIN        J    198
      *
     O*** END ADD ****** 08/21/08 ************************************
     O*
     O*  PAYMENT CODE SUMMARY
     O          E            SUMHD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             128
     O                       DSDATE        Y    198
     O          E            SUMHD          2
     O                       Q1PGM               10
      ***** BEG CHG ******* 10/09/00 **********************************
     O*************            ORTITL   127
     O                       XXTITL             128
      ***** END CHG ******* 10/09/00 **********************************
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      ***** BEG DEL ******* 10/09/00 **********************************
     O********E**2****         SUMHD
     O****************                      'GROUP:'
     O****************         XXFND Z +  1
     O****************         FNNAME  +  1
     O****************                   82 'POLICY YEAR:'
     O****************         XXFYR Z +  1
     O****************                  122 'LOSS AMOUNT:'
     O****************         XXAMT J +  1
     O****************                  187 'AS OF DATE:'
     O****************         X1DTE    198 '  /  /  '
      ***** END DEL ******* 10/09/00 **********************************
      ***** BEG ADD ******* 10/09/00 **********************************
      *
     O          E            SUMHD          2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       FNNAME            +  2
     O                                         +005 'LOSS AMOUNT:'
     O                       XXAMT         J   +  1
     O                                         +005 'AS OF DATE:'
     O                       X1DTE             +001 '  /  /  '
     O                                         +005 'CLAIM STATUS:'
     O                       QQOCB             +001
     O                                         +005 'OUTPUT TYPE:'
     O                       QQPFB             +001
     O               72                        +005 'POLICY YEAR: *ALL'
      *
     O          E            SUMHD          2
     O                                         +000 'POLICY YEAR:'
     O                       XXFYR         Z   +001
      *
     O          E            SUMHD       0  0
     O                       LINE              +000
      *
      ***** END ADD ******* 10/09/00 **********************************
     O          E            SUMHD          2
     O                                           22 'PAYMENT CODE SUMMARY'
     O          E            SUMHD          2
     O                                           23 'AMOUNT   CODE'
     O                                           36 'DESCRIPTION'
      *
     O          E            SUMDTL         1
     O                       PAMT(X)       J     17
     O                       PCOD(X)             22
     O                       PDES(X)             65
      *
      ***** BEG ADD ******* 10/09/00 **********************************
      *
      * LR TOTAL
      *
     O          E            LRTOT          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             128
     O                       DSDATE        Y    198
      *
     O          E            LRTOT          2
     O                       Q1PGM               10
     O                       XXTITL             128
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E            LRTOT          2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       FNNAME            +  2
     O                                         +005 'LOSS AMOUNT:'
     O                       XXAMT         J   +  1
     O                                         +005 'AS OF DATE:'
     O                       X1DTE             +001 '  /  /  '
     O                                         +005 'CLAIM STATUS:'
     O                       QQOCB             +001
     O                                         +005 'OUTPUT TYPE:'
     O                       QQPFB             +001
     O                                         +005 'POLICY YEAR: *ALL'
      *
     O          E            LRTOT       0  0
     O                       LINE              +000
      *
     O          E            LRTOT          2
     O                                          146 'TOTAL PAID    '
     O                                          160 'OUTS RESERVE'
     O                                          176 'TOTAL INCURRED'
     O*** BEG CHG ****** 08/21/08 ************************************
     O*****************                 192 'AMT IN EXCESS'
     O              N75                         192 'AMT IN EXCESS'
     O*** END CHG ****** 08/21/08 ************************************
      *
      *
     O          E            LRTOT          3
     O                                           30 'FINAL TOTALS:'
     O                       LROPEN        J     77
     O                                           82 'OPEN'
     O                       LRCLOS        J     90
     O                                           97 'CLOSED'
     O                       LRTLCL        J    106
     O                                          118 'TOTAL CASES'
     O                       LRTLPD        J    143
     O                       LRTLOR        J    161
     O                       LRTLIN        J    177
     O*** BEG CHG ****** 08/21/08 ************************************
     O*****************        LRTLEXJ  193
     O              N75      LRTLEX        J    193
     O*** END CHG ****** 08/21/08 ************************************
      *
      * FINAL PAYMENT CODE SUMMARY
      *
     O          E    74      LRSHD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             128
     O                       DSDATE        Y    198
      *
     O          E    74      LRSHD          2
     O                       Q1PGM               10
     O                       XXTITL             128
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E    74      LRSHD          2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       FNNAME            +  2
     O                                         +005 'LOSS AMOUNT:'
     O                       XXAMT         J   +  1
     O                                         +005 'AS OF DATE:'
     O                       X1DTE             +001 '  /  /  '
     O                                         +005 'CLAIM STATUS:'
     O                       QQOCB             +001
     O                                         +005 'OUTPUT TYPE:'
     O                       QQPFB             +001
     O                                         +005 'POLICY YEAR:'
     O              N72      XXFYR         Z   +001
     O               72                         191 '*ALL'
      *
     O          E            LRSHD       0  0
     O                       LINE              +000
      *
     O          E   N73      LRSHD          2
     O                                         +000 'FINAL PAYMENT CODE'
     O                                         +001 'SUMMARY - ALL POLICY'
     O                                         +001 'YEARS'
      *
     O          E   N73      LRSHD          2
     O                                           23 'AMOUNT   CODE'
     O                                           36 'DESCRIPTION'
      *
     O          E            LRSDTL         1
     O                       FAMT(X)       J     17
     O                       PCOD(X)             22
     O                       PDES(X)             65
      *
      *
     O          E            NOREC       2
     O                                         +010 '*** NO RECORDS MEET'
     O                                         +001 'SELECTED CRITERIA ***'
      *
      ***** END ADD ******* 10/09/00 **********************************
