     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE N7730R - SALES JOURNAL - REPORT PROGRAM
     H***************************************************************
     H** REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H* 01/27/99 C:62  Project: Y2K
     H*                CHANGING THE DATES TO MDYY.
     H*
     H* 01/04/99 C:31  Project: MC152
     H*                1. Change size of invoice number
     H*
     H* 01/12/90 LJD  -  HAD TO REPLACE ARRAY INVA WITH FILE NARINV *
     H*                  BECAUSE WHEN USER TRIED TO RUN THIS REPORT *
     H*                  FOR 01/01/89 THRU 12/31/89 IT GOT AN ARRAY *
     H*                  INDEX ERROR BECAUSE WE HAD REACHED LIMIT   *
     H*                  OF 9999.                                   *
     H*                                                             *
     H* 08/05/93 C:20    FIX TO PRINT SELECTION CRITERIA ON ROUTING *
     H*                  CHANGED PARSM TO PASS ALL SELECTION CRITERI*
     H***************************************************************
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FNMARL9    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FNARINV    UF A E           K DISK
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
     FQSYSPRT1  O    F  127        PRINTER INFDS(PRTDS1)
     F                                     OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XC#                    1     22
     D  XXSUB#                 1      5  0
     D  XXHYP1                 6      6
     D  XXFND                  7      9  0
     D  XXHYP2                10     10
     D  XXYR                  11     12  0
     D  XXHYP3                13     13
     D  XXCASE                14     20  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PRTDS1          DS
     D  FIL21                  1    200
     D  FIL22                201    366
     D  XXLIN2               367    368B 0
     D  XXPAG2               369    372B 0
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE SUBSCRIBER MASTER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQRTYP            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C*  INITIALIZATION SECTION
     C     *LIKE         DEFINE    AREOB#        XXEOB#
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFDT         X1FDT             8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1FDT     SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFDT   60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQTDT         X1TDT             8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1TDT     SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTDT   60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQSUB#        X1SUB#            5 0
     C                   MOVE      QQFND         X1FND             3 0
     C                   MOVE      QQFYR         X1FYR             3 0
     C                   MOVE      QQCASE        X1CASE            7 0
     C                   MOVE      QQSYS         X1SYS             1
     C                   Z-ADD     0             XXIA2            10 2
     C                   Z-ADD     0             XXIA3            10 2
     C                   Z-ADD     0             XXIA4            10 2
     C                   Z-ADD     0             XXIA5            10 2
     C                   Z-ADD     0             XXINV2            5 0
     C                   Z-ADD     0             XXINV3            5 0
     C                   Z-ADD     0             XXINV4            5 0
     C                   Z-ADD     0             I                 5 0
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      '-'           XXHYP3            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C     QQRTYP        IFEQ      'B'
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN11
     C                   ELSE
     C     QQRTYP        IFEQ      'D'
     C                   MOVE      '1'           *IN10
     C                   ELSE
     C     QQRTYP        IFEQ      'S'
     C                   MOVE      '1'           *IN11
     C                   END
     C                   END
     C                   END
     C                   MOVE      '0'           *IN30
     C                   MOVE      '0'           *IN31
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN33
     C                   MOVE      '0'           *IN34
     C                   MOVE      '0'           *IN35
     C     X1SUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN30
     C                   END
     C     X1FND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN31
     C                   END
     C     X1FYR         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN32
     C                   END
     C     X1CASE        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN33
     C                   END
     C     X1SYS         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN34
     C                   END
     C*** BEGIN CHG *** 01/27/00 **************************************
     C********** XXFDT     IFEQ *ZEROS
     C     X1FDT         IFEQ      *ZEROS
     C*** END   CHG *** 01/27/00 **************************************
     C                   MOVE      '1'           *IN35
     C                   END
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   GET ROUTING INFORMATION FOR REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        XXTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*   GET ROUTING INFORMATION FOR REPORT
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        YYTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*  READ A/R OPEN QUERY FILE (ALL 'I' RECORDS IN DATE RANGE)
     C*
     C     *LOVAL        SETLL     PAR01
     C                   READ      PAR01                                  50
     C*
     C     *IN50         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C*
     C                   ADD       1             I
     C                   MOVE      ARINV#        NARIN#                          01/12/90 ADD
     C                   WRITE     NAR1                                          01/12/90 ADD
     C*
     C                   Z-ADD     ARCO#         XXCO#             3 0
     C                   Z-ADD     ARSUB#        XXSUB#            5 0
     C                   Z-ADD     ARFND         XXFND             3 0
     C                   Z-ADD     ARFYR         XXFYR             3 0
     C                   Z-ADD     ARFYR         XXYR              2 0
     C                   Z-ADD     ARCASE        XXCASE            7 0
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADDARINV#    XXINV#  50
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVE      ARSYS         XXSYS             1
     C*
     C*  GET COMPANY NAME/GROUP NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSC NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBM
     C                   END
     C*
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   END                                                    IF *IN50 = 0
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     ARSYS         IFNE      XXSYS
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   EXSR      SRSUB                                        SUB  TOTALS
     C                   EXSR      SRPRO                                        PROD TOTALS
     C                   ELSE
     C     ARSUB#        IFNE      XXSUB#
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   EXSR      SRSUB                                        SUB  TOTALS
     C                   ELSE
     C     ARFND         IFNE      XXFND
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   ELSE
     C     ARCASE        IFNE      XXCASE
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   ELSE
     C     ARINV#        IFNE      XXINV#
     C                   EXSR      SRINV                                        INV CHG
     C                   END                                                    ENDNE XXINV
     C                   END                                                    ENDNE XXCASE
     C                   END                                                    ENDNE XXFND
     C                   END                                                    ENDNE XXSUB#
     C                   END                                                    ENDNE XXSYS
     C*
     C*  MOVE FIELDS TO REPORT FIELDS
     C*
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADDARINV#    XXINV#  50
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   Z-ADD     AREOB#        XXEOB#
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXDATE  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   MOVEL     ARDESC        XXDESC           30
     C                   Z-ADD     ARAMT         XXADET            9 2           DET AMOUNT
     C                   ADD       ARAMT         XXIA2            10 2           GRP  AMOUNT
     C*
     C*  PRINT DETAIL RECORD
     C*
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C   10              EXCEPT    DETAIL
     C*
     C**********************************
     C*  READ NEXT RECORD              *
     C**********************************
     C*                                                                AME
     C                   READ      PAR01                                  50
     C*
     C     ARINV#        CHAIN     NAR1                               25
     C     *IN25         IFEQ      '1'
     C                   ADD       1             I
     C                   MOVE      ARINV#        NARIN#
     C                   WRITE     NAR1
     C                   END
     C*
     C                   END                                                    END DOWEQ 50
     C*                                                                AME
     C**********************************
     C*     END THE PROGRAM            *
     C**********************************
     C                   MOVE      '1'           *INLR
     C*
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP CHG
     C                   EXSR      SRSUB                                        SUB CHG
     C                   EXSR      SRPRO                                        PRO CHG
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    GRTOT1
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************--*
     C*  PERFORM INVOICE BREAK CALCULATIONS
     C*****************************************************************--*
     C*
     C     SRINV         BEGSR
     C*
     C                   ADD       1             XXINV2
     C                   ADD       1             XXINV3
     C                   ADD       1             XXINV4
     C*
     C                   Z-ADD     ARINV#        XXINV#
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************--*
     C*  PERFORM CASE TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRCASE        BEGSR
     C*
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARFYR         XXYR
     C                   Z-ADD     ARCASE        XXCASE
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRGRP         BEGSR
     C*
     C                   ADD       XXIA2         XXIA3
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    GRPTL1
     C*
     C                   Z-ADD     0             XXIA2
     C                   Z-ADD     0             XXINV2
     C*
     C                   Z-ADD     ARFND         XXFND
     C*
     C     *INLR         IFEQ      '0'
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM SUBSCRIBER TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRSUB         BEGSR
     C*
     C                   ADD       XXIA3         XXIA4
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    SBTOT1
     C*
     C                   Z-ADD     0             XXIA3
     C                   Z-ADD     0             XXINV3
     C*
     C                   Z-ADD     ARSUB#        XXSUB#
     C*
     C     *INLR         IFEQ      '0'
     C*
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBM
     C                   END
     C*
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM PRODUCT TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRPRO         BEGSR
     C*
     C                   ADD       XXIA4         XXIA5
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   10              EXCEPT    PRTOT
     C   11              EXCEPT    PRTOT1
     C*
     C                   Z-ADD     0             XXIA4
     C                   Z-ADD     0             XXINV4
     C*
     C                   MOVE      ARSYS         XXSYS
     C*
     C                   ENDSR
     O**
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
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
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE       2  2
     O                                           21 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           31 'PRODUCT.........:'
     O              N34      X1SYS               33
     O               34                          36 '*ALL'
     O          E            ROUTE          2
     O                                           31 'SUBSCRIBER......:'
     O              N30      X1SUB#              37
     O               30                          36 '*ALL'
     O          E            ROUTE          2
     O                                           31 'GROUP...........:'
     O              N31      X1FND               35
     O               31                          36 '*ALL'
     O          E            ROUTE          2
     O                                           31 'YEAR............:'
     O              N32      X1FYR               35
     O               32                          36 '*ALL'
     O          E            ROUTE          2
     O                                           31 'CASE............:'
     O              N33      X1CASE              39
     O               33                          36 '*ALL'
     O          E            ROUTE          2
     O                                           31 'DATE RANGE:.....:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********      N35      XXFDT Y   41
     O**********       35                41 '00/00/00'
     O**********                         44 'TO'
     O**********               XXTDT Y   53
     O              N35      X1FDT               43 '  /  /    '
     O               35                          43 '00/00/0000'
     O                                           46 'TO'
     O                       X1TDT               57 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997     '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE               8 '  /  /  '
     O                       XXCONM              84
     O                       DSTIME             127 '  :  :  '
     O          E            HEAD           1
     O                       ORPNAM              10
     O                       XXTITL              94
     O                       PAGE               127
     O                                          121 'PAGE:'
     O*
     O          E            HEAD           2
     O                                           54 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFDT Y   63
     O**********                         67 'TO:'
     O**********               XXTDT Y   76
     O                       X1FDT               65 '  /  /    '
     O                                           69 'TO:'
     O                       X1TDT               80 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD           2
     O                                            8 'PRODUCT:'
     O                       XXSYS               10
     O          E            HEAD2          2
     O                                            8 'CASE #'
     O                                           35 'INV #'
     O                                           48 'EOB #'
     O                                           62 'DATE'
     O                                           83 'DESCRIPTION'
     O                                          122 'INV. AMOUNT'
     O          E            PRTOT       1  2
     O                                            8 'PRODUCT'
     O                       XXSYS               10
     O                                           18 'TOTALS:'
     O                                           49 'NUMBER OF INVOICES: '
     O                       XXINV4        J     59
     O                                           91 'INVOICE AMOUNT: '
     O                       XXIA4         J    123
     O          E   NLRNOA   PRTOT          2
     O                                            8 'PRODUCT:'
     O                       ARSYS               10
     O          E            DETAIL         1
     O                       XC#                 22
     O                       XXINV#        Z     35
     O                       XXEOB#        Z     48
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXDATE    64 '  /  /  '
     O                       XXDATE              64 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                       XXDESC             102
     O                       XXADET        K    123
     O**
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O**
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       YYTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE1      2  2
     O                                           21 'REPORT SELECTION:'
     O          E            ROUTE1         2
     O                                           31 'PRODUCT.........:'
     O              N34      X1SYS               33
     O               34                          36 '*ALL'
     O          E            ROUTE1         2
     O                                           31 'SUBSCRIBER......:'
     O              N30      X1SUB#              37
     O               30                          36 '*ALL'
     O          E            ROUTE1         2
     O                                           31 'GROUP...........:'
     O              N31      X1FND               35
     O               31                          36 '*ALL'
     O          E            ROUTE1         2
     O                                           31 'YEAR............:'
     O              N32      X1FYR               35
     O               32                          36 '*ALL'
     O          E            ROUTE1         2
     O                                           31 'CASE............:'
     O              N33      X1CASE              39
     O               33                          36 '*ALL'
     O          E            ROUTE1         2
     O                                           31 'DATE RANGE:.....:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********      N35      XXFDT Y   41
     O**********       35                41 '00/00/00'
     O**********                         44 'TO'
     O**********               XXTDT Y   53
     O              N35      X1FDT               43 '  /  /    '
     O               35                          43 '00/00/0000'
     O                                           46 'TO'
     O                       X1TDT               57 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997     '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT1  E            HEAD1          1 06
     O                       DSDATE               8 '  /  /  '
     O                       XXCONM              84
     O                       DSTIME             127 '  :  :  '
     O          E            HEAD1          1
     O                       ORPNAM              10
     O                       YYTITL              94
     O                       PAGE1         Z    127
     O                                          121 'PAGE:'
     O*
     O          E            HEAD1          2
     O                                           54 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFDT Y   63
     O**********                         67 'TO:'
     O**********               XXTDT Y   76
     O                       X1FDT               65 '  /  /    '
     O                                           69 'TO:'
     O                       X1TDT               80 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E            HEAD12         2
     O                                            7 'PRODUCT'
     O                                           20 'SUBSCRIBER'
     O                                           28 'GROUP'
     O                                           70 '# OF INVOICES'
     O                                          115 'INVOICE AMOUNT'
     O          E            GRPTL1         2
     O                       XXSYS                4
     O                       XXSUB#              17
     O                       XXFND               27
     O                       XXINV2        J     66
     O                       XXIA2         J    112
     O          E            SBTOT1         2
     O                                           35 'SUBSCRIBER SUBTOTAL:'
     O                       XXINV3        J     66
     O                       XXIA3         J    112
     O          E            PRTOT1         2
     O                                           35 'PRODUCT TOTAL:'
     O                       XXINV4        J     66
     O                                           69 '**'
     O                       XXIA4         J    112
     O          E            GRTOT1      1  2
     O                                           35 'GRAND TOTAL:'
     O                       I             J     66
     O                                           69 '**'
     O                       XXIA5         J    112
     O          E            GRTOT1         2
     O                                           22 '** SUM OF THE PRODUCTS'
     O                                           44 ' MAY NOT EQUAL GRAND  '
     O                                           49 'TOTAL'
     O**
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
