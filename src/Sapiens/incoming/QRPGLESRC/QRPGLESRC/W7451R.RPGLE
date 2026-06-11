     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE W7451R - DEPOSIT SLIP
     H*****************************************************************
     H*
     H* REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 07/17/06 C:53  SUPPORT: 53232
     H*                ADD USER TO PARMS TO CORRECT ROUTE TO INFO
     H*
     H* 02/19/04 C:136 PROJECT: WC1033
     H*                Add code for new A/R type of 'T' (True Void)
     H*
     H* 05/18/00 C:61  SUPPORT WA107 22067
     H*                Process account current policies.
     H*
     H* 04/10/00 C:118 PROJECT: WC987
     H*                Load alternate company name from fund master
     H*
     H* 08/30/99 C:33  PROJECT: WC577
     H*               -MAKE REPORT MORE EFFICIENT BY CHANGING THE
     H*                LOGICAL USAGE.
     H*               -V3 STANDARDS.
     H*
     H* 08/17/99 C:33  PROJECT: WC899   WA096/19296
     H*                PRINT TOTALS, EVEN IF THE LAST EMPLOYER READ
     H*                IS AN ACCOUNT CURRENT POLICY
     H*
     H* 04/14/99 C:90  PROJECT: 96WAD (IN BASE: 07/19/99 C:33 WC899)
     H*                DO NOT PROCESS ACCOUNT CURRENT POLICIES.
     H*
     H* 10/20/98 C:113 CHANGE 'FUND' TO 'GROUP' AND 'FUND YEAR' TO
     H*                'POLICY YEAR' ON REPORT.
     H*
     H* 04/17/98 C:78 PROJECT WC577
     H*               EMPLOYER NAME ADDED TO OUTPUT REPORT
     H*
     H* 01/19/98 C:77 PROJECT WC736
     H*               ADD PROCESSING TO EXCLUDE PAYMENT-TYPE RECORDS
     H*               WITH A/R TRANSACTION CODES THAT HAVE BEEN
     H*               SPECIFIED TO BE EXCLUDED FROM DEPOSIT SLIPS.
     H*
     H*****************************************************************
     F*
     F* Added code to Accumulate A/R by fund year and A/R code
     F*                                                    LJQ 9-19-88
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F***  BEGIN CHG  ***  08/30/99  *********************************
     F**********WMEAL5  IF  E           K        DISK
     FWMEAL20   IF   E           K DISK
     F***  END   CHG  ***  08/30/99  *********************************
     F*** BEGIN ADD *** 01/19/98 **************************************
     FWTABL     IF   E           K DISK
     F*** END ADD ***** 01/19/98 **************************************
     F*** BEGIN ADD *** 04/17/98 **************************************
     FWMEML     IF   E           K DISK
     F*** END   ADD *** 04/17/98 **************************************
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D XCOD            S              5    DIM(999)                              Fyr/ A/R Code
     D XAMT            S             11    DIM(999)                              Total Amount
     D COMB            S             16    DIM(999)                              Combination
     D*
     D***  BEGIN ADD  ***  08/30/99  *********************************
     D TITLE           C                   CONST('Daily Deposit Slip')
     D***  END   ADD  ***  08/30/99  *********************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D* Combination Array                                  LJQ 9-19-88
     D                 DS
     D  XCOMB                  1     16
     D  XYEAR                  1      3  0
     D  XCODE                  4      5
     D  XXAMT                  6     16  2
     D* Fund Year / A/R Code
     D                 DS
     D  XXKEY                  1      5
     D  XXYR                   1      3
     D  XXCD                   4      5
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  BEGIN ADD  ***  08/30/99  *********************************
     C                   KFLD                    XXYMD
     C***  END   ADD  ***  08/30/99  *********************************
     C*** BEGIN ADD *** 04/17/98 **************************************
     C*
     C*   KEY LIST FOR EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C*
     C*** END   ADD *** 04/17/98 **************************************
     C*
     C*** BEGIN ADD *** 01/19/98 **************************************
     C*
     C*   KEY LIST FOR TABLE FILES
     C*
     C     ABKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY2
     C*** END ADD ***** 01/19/98 **************************************
     C*
     C     *LIKE         DEFINE    UPDT          XXYMD
     C*** BEGIN ADD *** 01/19/98 **************************************
     C     *LIKE         DEFINE    TTKEY         XXKEY2
     C*** END ADD ***** 01/19/98 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQQUE            10            MLS 1/19/88
     C*** BEG ADD *** 07/17/06 ***********************************
     C                   PARM                    QQUSR            10
     C*** END ADD *** 07/17/06 ***********************************
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQDATE        XXDATE            8 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C                   TIME                    DSDTTM
     C*                                                    MLS 1/19/88
     C*   SET UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*
     C* Initialize A/R Summary Fields                      LJQ 9-19-88
     C*                                                          "
     C                   MOVEA     *BLANKS       XAMT                                 "
     C                   MOVEA     *BLANKS       XCOD                                 "
     C                   MOVEA     *BLANKS       COMB                                 "
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C*** BEG ADD *** 07/17/06 ***********************************
     C     QQUSR         IFNE      *BLANKS
     C                   MOVEL(P)  QQUSR         Q1USER
     C                   ENDIF
     C*** END ADD *** 07/17/06 ***********************************
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C***** BEGIN DEL **** 04/10/00 ***********************************
     C***  BEGIN ADD  ***  08/30/99  *********************************
     C**********
     C*  CENTER TEXT IN FIELD
     C**********
     C**********           MOVELCONAME    ##DATA
     C**********
     C**********           CALL 'SRCENTER'             99
     C**********           PARM           ##DATA 80
     C**********           PARM 40        ##LENG  30
     C**********
     C**********           MOVEL##DATA    CONAME
     C***  END   ADD  ***  08/30/99  *********************************
     C***** END   DEL **** 04/10/00 ***********************************
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C*
     C     ORKEY         CHAIN     POR01                              60
     C*
     C***  BEGIN CHG  ***  08/30/99  *********************************
     C**********           MOVE *BLANKS   XXTITL 56
     C**********           MOVELORTITL    XXTITL
     C     *IN60         IFEQ      *ON
     C                   MOVEL     TITLE         ORTITL
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
     C                   MOVEL(P)  ORTITL        XXTITL           56
     C***  END   CHG  ***  08/30/99  *********************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ***********************************
     C***********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ***********************************
     C***  BEGIN ADD  ***  08/30/99  *********************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     XXTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      56            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL
     C***  END   ADD  ***  08/30/99  *********************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C   60              MOVE      *BLANK        FNNAME
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                       22
     C*
     C*   READ THE EMPLOYER ACCOUNTS RECEIVABLE FILE.
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
      ***** BEG DEL ******* 05/18/00 **********************************
      *****BEGIN*ADD **** 04/14/99 ************************************
     C*************        Z-ADDXXCO#     @@CO#
     C*************        Z-ADDXXFND     @@FND
     C*************        Z-ADDEAEMP#    @@EMP#
     C*************        Z-ADDEADIV     @@DIV
     C*************        Z-ADDEAFYR     @@FYR
     C*************        Z-ADDXXDATE    @@DATE
     C*************        MOVEL'*'       @@PLAN
      *************
     C*************        CALL 'WA002R'
     C*************        PARM           @@CO#   30
     C*************        PARM           @@FND   30
     C*************        PARM           @@EMP#  90
     C*************        PARM           @@DIV   50
     C*************        PARM           @@FYR   30
     C*************        PARM           @@AGT#  50
     C*************        PARM           @@ANAM 60
     C*************        PARM           @@PROD  50
     C*************        PARM           @@PNAM 60
     C*************        PARM           @@AREA  30
     C*************        PARM           @@PHON  70
     C*************        PARM           @@PLAN  1
     C*************        PARM           @@DATE  80
      *************
     C***********@@PLAN    IFNE 'A'
      *****END***ADD **** 04/14/99 ************************************
      ***** END DEL ******* 05/18/00 **********************************
     C*** BEGIN ADD *** 04/17/98 **************************************
     C*
     C*   GET THE EMPLOYER NAME
     C*
     C     EMKEY         CHAIN     PEM01                              85
     C*** END   ADD *** 04/17/98 **************************************
     C*
     C***  BEGIN DEL  ***  08/30/99  *********************************
     C********** EATRDT    IFEQ XXYMD
     C***  END   DEL  ***  08/30/99  *********************************
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'                                          LJQ 9-9-88
     C***  BEGIN ADD  ***  02/19/04  *********************************
     C     EAARTY        OREQ      'T'
     C***  END   ADD  ***  02/19/04  *********************************
     C*
     C*** BEGIN ADD *** 01/19/98 **************************************
     C                   MOVEL     EAARCD        XXKEY2
     C     ABKEY         CHAIN     PTT01                              61
     C*
     C     *IN61         IFEQ      '0'
     C     TTEXDS        ANDEQ     *BLANK
     C*** END ADD ***** 01/19/98 **************************************
     C*
     C*   PRINT THE DETAIL LINE.
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C   22              EXCEPT    HEAD
     C                   EXCEPT    DETAIL
     C                   EXSR      ARSUM                                        LJQ 9-19-88
     C                   SETOFF                                       22
     C   OF              SETON                                        22
     C                   SETOFF                                       OF
     C*
     C*   ADD TO THE TOTALS.
     C*
     C                   ADD       EAAMT         XXTOTL            9 2
     C*
     C*** BEGIN ADD *** 01/19/98 **************************************
     C                   ENDIF                                                  *IN61 = '0'
     C*** END ADD ***** 01/19/98 **************************************
     C*
     C                   END
     C***  BEGIN DEL  ***  08/30/99  *********************************
     C**********           END
     C***  END   DEL  ***  08/30/99  *********************************
      ***** BEG DEL ******* 05/18/00 **********************************
      **** BEGIN ADD **** 04/14/99 ************************************
     C*************        ENDIF                           @@PLAN NE A
      **** END   ADD **** 04/14/99 ************************************
      ***** END DEL ******* 05/18/00 **********************************
     C     EAKEY         READE     PEA01                                  60
     C                   END
     C*
     C*   PRINT TOTAL LINE
     C*
     C***  BEGIN DEL  ***  08/17/99  *********************************
     C*****BEGIN ADD **** 04/14/99 ************************************
     C********** @@PLAN    IFNE 'A'
     C*****END** ADD **** 04/14/99 ************************************
     C***  END   DEL  ***  08/17/99  *********************************
     C   22              EXCEPT    HEAD2
     C                   EXCEPT    TOTAL
     C                   SETOFF                                       22
     C*
     C                   EXSR      ARDETL                                       LJQ 9-19-88
     C***  BEGIN DEL  ***  08/17/99  *********************************
     C*****BEGIN ADD **** 04/14/99 ************************************
     C**********           ENDIF                           @@PLAN NE A
     C*****END** ADD **** 04/14/99 ************************************
     C***  END   DEL  ***  08/17/99  *********************************
      /Free
       //***** Beg Add ***** 08/26/14 ********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   SETON                                        LR
     C*
     C*
     C*================================================================
     C* SUBROUTINE    **  ARSUM  **  LOAD A/R TYPES INTO ARRAYS       *
     C*================================================================
     C*                                                    LJQ 9-19-88
     CSR   ARSUM         BEGSR
     C*
     C*
     C*  CHECK TO SEE IF CODE IS ALREADY IN ARRAY
     C*
     C                   Z-ADD     1             V                 5 0
     C                   MOVE      '1'           *IN33
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      EAFYR         XXYR
     C                   MOVE      EAARCD        XXCD
     C     XXKEY         LOOKUP    XCOD(V)                                33
     C*
     C     *IN33         IFEQ      '0'                                          New code ADD
     C                   Z-ADD     1             V
     C*
     C*  FIND NEXT AVAILABLE POSITION
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   ADD       1             V
     C                   END
     C*
     C*  ADD FIELDS TO THE ARRAY
     C*
     C                   Z-ADD     EAAMT         XXAMT
     C                   MOVE      *BLANKS       X1AMT
     C*
     C                   MOVEA     XXKEY         XCOD(V)
     C                   MOVE      XXAMT         X1AMT            11
     C                   MOVEA     X1AMT         XAMT(V)
     C*
     C*   IF CODE IS IN ARRAY ADD TO EXISTING TOTALS
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXAMT
     C                   MOVE      *BLANKS       X1AMT
     C                   MOVEA     XAMT(V)       X1AMT
     C                   MOVE      X1AMT         XXAMT
     C                   ADD       EAAMT         XXAMT
     C                   MOVE      XXAMT         X1AMT
     C*
     C                   MOVEA     X1AMT         XAMT(V)
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   PRINT A/R CODE SUMMARY               Subroutine  ** ARDETL  =  *
     C*==================================================================*
     C*
     CSR   ARDETL        BEGSR
     C*
     C* MOVE CODE AND TOTAL ARRAYS TO COMBINATION ARRAY
     C*
     C                   Z-ADD     1             V
     C                   Z-ADD     0             XXTTL            11 2
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   MOVEL     XCOD(V)       COMB(V)
     C                   MOVE      XAMT(V)       COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C* IF THERE ARE RECORDS IN THE FILE CONTINUE.
     C*
     C     V             IFNE      1
     C*
     C* PRINT SUMMARY HEADING.                                         DES
     C*
     C                   EXCEPT    ARHEAD
     C*
     C*  SORT THE ARRAY
     C*
     C                   SORTA     COMB
     C*
     C*  FIND THE FIRST RECORD
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
     C*  PRINT DETAIL LINE
     C*
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    ARHEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    ARDTL
     C                   ADD       1             X1
     C                   ADD       XXAMT         XXTTL
     C                   END
     C*
     C                   EXCEPT    ARTTL
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***********************************
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
     O*  MLS 1/19/88  ADD INDICATORS
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ***********************************
      ***** Beg Add ***** 08/05/15 ***********************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/19/88  ADD INDICATORS
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ***********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ***********************************
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
     O*  MLS 1/19/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ***********************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ****** END DLT ***** 01/28/10 **********************************
     O*** BEGIN CHG *** 01/19/98 **************************************
     O*************************          46 'VE DATA SOLUTIONS. INC,'
     O*************************          51 '.1990'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                46 'VE DATA SOLUTIONS, INC.'
     O***                      *YEAR   +  1
      ****** END DLT ***** 01/28/10 **********************************
     O*** END CHG ***** 01/19/98 **************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O**** BEGIN CHG **** 04/17/98 ************************************
     O*****************        CONAME    62
     O*****************        DSDATEY   80
     O***  BEGIN CHG  ***  08/30/99  *********************************
     O**********               CONAME   108
     O                       CONAME              86
     O***  END   CHG  ***  08/30/99  *********************************
     O                       DSDATE        Y    132
     O**** END   CHG **** 04/17/98 ************************************
     O          E            HEAD           1
     O                       Q1PGM               10
     O**** BEGIN CHG **** 04/17/98 ************************************
     O*****************        XXTITL    68
     O*****************                  74 'PAGE:'
     O*****************        PAGE  Z   80
     O***  BEGIN CHG  ***  08/30/99  *********************************
     O**********               XXTITL    96
     O                       XXTITL              94
     O***  END   CHG  ***  08/30/99  *********************************
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O**** END   CHG **** 04/17/98 ************************************
     O          E            HEAD           1
     O*** BEGIN CHG ***  10/20/98  ************************************
     O************************           09 'FUND:'
     O************************ XXFND Z   13
     O************************ FNNAME    74
     O                                           10 'GROUP:'
     O                       XXFND         Z     14
     O                       FNNAME              75
     O*** END   CHG ***  10/20/98  ************************************
     O          E            HEAD           2
     O                                           13 'RUN DATE:'
     O                       XXDATE        Y     24
     O          E            HEAD           2
     O                                           19 'EMPLOYER   DIV'
     O**** BEGIN ADD **** 04/17/98 ************************************
     O                                           33 'EMPLOYER NAME'
     O**** END   ADD **** 04/17/98 ************************************
     O**** BEGIN CHG **** 04/17/98 ************************************
     O*************************          26 'FUND YR'
     O*************************          41 'TRANS DT'
     O*************************          53 'POST DT'
     O*************************          60 'CODE'
     O*************************          70 'CHECK #'
     O*************************          84 'AMOUNT'
     O*** BEGIN CHG ***  10/20/98  ************************************
     O*************************          67 'FUND YEAR'
     O                                           67 'POLICY YEAR'
     O*** END   CHG ***  10/20/98  ************************************
     O                                           82 'TRANS DT'
     O                                           94 'POST DT'
     O                                          100 'CODE'
     O                                          112 'CHECK #'
     O                                          129 'AMOUNT'
     O**** END   CHG **** 04/17/98 ************************************
     O          E            DETAIL         1
     O                       EAEMP#        Z     13
     O                       EADIV         Z     19
     O**** BEGIN ADD **** 04/17/98 ************************************
     O                       EMNAME              60
     O**** END   ADD **** 04/17/98 ************************************
     O**** BEGIN CHG **** 04/17/98 ************************************
     O*********************    EAFYR Z   26
     O*********************    XXTRDT    41 '  /  /    '
     O*********************    XXPODT    53 '  /  /    '
     O*********************    EAARCD    59
     O*********************    EACHK#Z   70
     O*********************    EAAMT B   86
     O                       EAFYR         Z     67
     O                       XXTRDT              82 '  /  /    '
     O                       XXPODT              94 '  /  /    '
     O                       EAARCD             100
     O                       EACHK#        Z    112
     O                       EAAMT         B    129
     O**** END   CHG **** 04/17/98 ************************************
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
     O                       DSDATE        Y     80
     O          E            HEAD2          1
     O                       Q1PGM               10
     O                       XXTITL              68
     O                                           74 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD2          1
     O*** BEGIN CHG ***  10/20/98  ************************************
     O************************           09 'FUND:'
     O************************ XXFND Z   13
     O************************ FNNAME    74
     O                                           10 'GROUP:'
     O                       XXFND         Z     14
     O                       FNNAME              75
     O*** END   CHG ***  10/20/98  ************************************
     O          E            HEAD2          1
     O                                           13 'RUN DATE:'
     O                       XXDATE        Y     24
     O          E            TOTAL       1  2
     O                                           61 'TOTAL:'
     O                       XXTOTL        BB    76
     O*
     O* A/R SUMMARY HEADING                                 LJQ 9-19-88
     O*
     O          E            ARHEAD         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
     O                       DSDATE        Y     80
     O          E            ARHEAD         1
     O                       Q1PGM               10
     O                       XXTITL              68
     O                                           74 'PAGE:'
     O                       PAGE          Z     80
     O          E            ARHEAD         1
     O*** BEGIN CHG ***  10/20/98  ************************************
     O************************           09 'FUND:'
     O************************ XXFND Z   13
     O************************ FNNAME    74
     O                                           10 'GROUP:'
     O                       XXFND         Z     14
     O                       FNNAME              75
     O*** END   CHG ***  10/20/98  ************************************
     O          E            ARHEAD         2
     O                                           13 'RUN DATE:'
     O                       XXDATE        Y     24
     O          E            ARHEAD         2
     O                                           51 'A/R CODE SUMMARY'
     O          E            ARHEAD         2
     O*** BEGIN CHG ***  10/20/98  ************************************
     O***********************            20 'FUND YEAR'
     O                                           21 'POLICY YEAR'
     O*** END   CHG ***  10/20/98  ************************************
     O                                           40 'A/R CODE'
     O                                           66 'AMOUNT'
     O*
     O* A/R SUMMARY DETAIL                                  LJQ 9-19-88
     O*
     O          E            ARDTL          1
     O                       XYEAR         Z     16
     O                       XCODE               37
     O                       XXAMT         B     68
     O*
     O* A/R SUMMARY TOTAL                                   LJQ 9-19-88
     O*
     O          E            ARTTL       2  2
     O                                           49 'TOTAL:'
     O                       XXTTL         B     68
