     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W4091R - BUILD WORK FILE FOR FINANCIAL EARNED PREMIUM
     H*****************************************************************
     H** PROGRAM DESCRIPTION:                                         *
     H**                                                              *
     H**     This program loads data to WAFPP, the Financial Earned   *
     H** Premium File.  Earned Premium is stored in this file for     *
     H** each month.                                                  *
     H**                                                              *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**    W4091C                                                    *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**    (NONE)                                                    *
     H**                                                              *
     H**                                                              *
     H** PROGRAMMER:                                                  *
     H** CREATED:                     PROJECT:                        *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/14/99  C:09  PROJ: WC577  Y2K #: 47                        *
     H*                 DATE IS ALREADY IN CYYMMDD FORMAT, NO FURTHER *
     H*                 CONVERSION IS NEEDED.                         *
     H*                                                               *
     H* 02/17/97  C:35  PROJ:WC649                                    *
     H*                 IF NO EMPLOYER DETAIL IS FOUND FOR AN         *
     H*                 EXISTING WAFPP RECORD, CHECK TO SEE IF MORE   *
     H*                 THAN ONE MONTH HAS BEEN UPDATED.  IF ONLY     *
     H*                 ONE MONTH IS UPDATED, DELETE THE RECORD.  IF  *
     H*                 MORE THAN ONE MONTH IS UPDATED, OFFSET THE    *
     H*                 TOTAL AMOUNT IN THE CURRENT MONTH.            *
     H*                                                               *
     H* 03/05/97  C:35  PROJ:WC649                                    *
     H*                 SKIP EMPLOYER 0 WHEN CHECKING FOR DELETED     *
     H*                 EMPLOYER DETAIL RECORDS. EMPLOYER 0 IN THE    *
     H*                 WAFPP FILE IS THE SUMMARY RECORD.             *
     H*                                                               *
     H*****************************************************************
     FWAFPL     UF A E           K DISK
     FWDELL1    IF   E           K DISK

     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AM                     1    108  2
     D                                     DIM(12)
     D  FPAM01                 1      9  2
     D  FPAM02                10     18  2
     D  FPAM03                19     27  2
     D  FPAM04                28     36  2
     D  FPAM05                37     45  2
     D  FPAM06                46     54  2
     D  FPAM07                55     63  2
     D  FPAM08                64     72  2
     D  FPAM09                73     81  2
     D  FPAM10                82     90  2
     D  FPAM11                91     99  2
     D  FPAM12               100    108  2
     D                 DS
     D  YM                     1     60  0
     D                                     DIM(12)
     D  FPYM01                 1      5  0
     D  FPYM02                 6     10  0
     D  FPYM03                11     15  0
     D  FPYM04                16     20  0
     D  FPYM05                21     25  0
     D  FPYM06                26     30  0
     D  FPYM07                31     35  0
     D  FPYM08                36     40  0
     D  FPYM09                41     45  0
     D  FPYM10                46     50  0
     D  FPYM11                51     55  0
     D  FPYM12                56     60  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    FPEMP#        XXEMP#
     C     *LIKE         DEFINE    FPDIV         XXDIV
     C     *LIKE         DEFINE    FPDT          XXYMD
     C**** BEGIN DEL **** 12/14/99 ************************************
     C************LIKE     DEFN FPDT      X1YMD
     C**** END DEL ****** 12/14/99 ************************************
     C     *LIKE         DEFINE    FPYM01        XXYYMM
      *
     C                   Z-ADD     0             XXAMT            12 2
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDTE             6
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
     C     DTYMD         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDTE         XXDATE            6 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXYYMM
     C*
     C**** BEGIN CHG **** 12/14/99 ************************************
     C*********************MOVELXXYYMM    X1YMD
     C                   MOVEL     XXYYMM        XXYMD
     C*
     C* GET LAST DAY OF MONTH
     C*
     C                   MOVEL     QQDTE         XX                2 0
     C*********************MOVE MD,XX     X1YMD
     C                   MOVE      MD(XX)        XXYMD
     C*
     C*********************Z-ADDX1YMD     SRDTI
     C*********************CALL 'SRCVTDAT'DTYMD
     C*********************MOVELSRDTO     XXYMD
     C**** END CHG ****** 12/14/99 ************************************
     C*
     C*  CHECK FOR LEAP YEAR
     C*
     C                   MOVE      QQDTE         XXYR              2 0
     C     XXYR          DIV       4             XXRST             3 0
     C                   MVR                     XXREM             3 0
     C     XXREM         IFEQ      0
     C     XX            ANDEQ     2
     C                   ADD       1             XXYMD
     C                   END
     C                   Z-ADD     0             XXTOTL            9 2
     C*
     C*   KEY LIST FOR THE FINANCIAL EARNED PREMIUM FILE
     C*
     C     FPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     FPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C                   TIME                    DSDTTM
     C*
     C*   START READING THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   EXSR      SR1ST
     C*
     C*   MOVE THE INFORMATION TO CORRECT FIELDS
     C*
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C     FPKEY         CHAIN     PFP01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZEROS        YM
     C                   MOVE      *ZEROS        AM
     C                   MOVE      XXCO#         FPCO#
     C                   MOVE      XXFND         FPFND
     C                   MOVE      XXFYR         FPFYR
     C                   MOVE      ELEMP#        FPEMP#
     C                   MOVE      ELDIV         FPDIV
     C                   END
     C*
     C                   Z-ADD     1             X                 2 0
     C     XXYYMM        LOOKUP    YM(X)                                  50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     1             X                 2 0
     C     *ZEROS        LOOKUP    YM(X)                                  50
     C     *IN50         IFEQ      '1'
     C                   MOVE      XXYYMM        YM(X)
     C                   END
     C                   END
     C*
     C*   IF EMPLOYER IS AUDITED, TAKE THE YEAR TO DATE EARNED PREM
     C*   AND SUBTRACT PRIOR MONTH AMOUNTS TO GET CURRENT MONTH AMT
     C*
     C*   IF EMPLOYER IS NOT AUDITED, CALCULATE ANY DIFFERENCES IN
     C*   PRIOR MONTHS CALC EARNED PREM & APPLY TO CURRENT MONTH
     C*
     C                   Z-ADD     0             AM(X)                          JJJ 2-17-89
     C     ELFAF         IFEQ      'A'                                          JJJ 10-24-88
     C*                                                    |
     C                   XFOOT     AM            XXPREV           11 2          |
     C     XXPREM        SUB       XXPREV        XXMPRE                         |
     C*                                                    |
     C                   ELSE                                                   JJJ 2-16-89
     C*                                                    |
     C     XXPREM        SUB       XXMPRE        XXPREV                         |
     C                   XFOOT     AM            XXPTOT           11 2          |
     C     XXPREV        SUB       XXPTOT        XXDIFF           11 2          |
     C                   ADD       XXDIFF        XXMPRE                         |
     C*                                                    |
     C                   END                                                    |
     C                   Z-ADD     XXMPRE        AM(X)
     C                   ADD       XXMPRE        XXTOTL
     C                   MOVE      *BLANK        FPSTAT
     C                   MOVE      Q1USER        FPUSER
     C                   MOVE      Q1PGM         FPPGM
     C                   MOVE      DSTIME        FPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FPDT
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PFP01
     C                   ELSE
     C                   WRITE     PFP01
     C                   END
     C*
     C     ELKEY         READE     PEL01                                  60
     C                   END
     C*
     C*   WRITE SUMMARY RECORD TO WORK FILE
     C*
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     FPKEY         CHAIN     PFP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      XXCO#         FPCO#
     C                   MOVE      XXFND         FPFND
     C                   MOVE      XXFYR         FPFYR
     C                   Z-ADD     0             FPEMP#
     C                   Z-ADD     0             FPDIV
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       YM
     C                   END
     C*
     C                   Z-ADD     1             X                 2 0
     C     XXYYMM        LOOKUP    YM(X)                                  50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     1             X                 2 0
     C     *ZEROS        LOOKUP    YM(X)                                  50
     C     *IN50         IFEQ      '1'
     C                   MOVE      XXYYMM        YM(X)
     C                   END
     C                   END
     C                   Z-ADD     XXTOTL        AM(X)
     C                   MOVE      *BLANK        FPSTAT
     C                   MOVE      Q1USER        FPUSER
     C                   MOVE      Q1PGM         FPPGM
     C                   MOVE      DSTIME        FPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FPDT
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PFP01
     C                   ELSE
     C                   WRITE     PFP01
     C                   END
     C*
      * DELETE OR UPDATE WAFPP RECORDS IF NO EMPLOYER DETAIL IS FOUND
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
      *
     C*** BEGIN CHG *** 03/05/97 **************************************
     C********** FPKY2     CHAINPFP01                60
     C                   Z-ADD     1             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     FPKEY         SETLL     PFP01
     C     FPKY2         READE     PFP01                                  60
     C*** END   CHG *** 03/05/97 **************************************
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      FPEMP#        XXEMP#
     C                   MOVE      FPDIV         XXDIV
      *
     C     ELKY2         CHAIN     PEL01                              61
      *
     C     *IN61         IFEQ      *ON
      *
     C                   XFOOT     AM            XXAMT
     C                   SUB       FPAM01        XXAMT
      *
     C     XXAMT         IFEQ      0
      *
     C                   DELETE    PFP01
      *
     C                   ELSE
      *
     C                   ADD       FPAM01        XXAMT
     C                   MULT      -1            XXAMT
     C                   Z-ADD     1             X
     C     XXYYMM        LOOKUP    YM(X)                                  50
      *
     C     *IN50         IFEQ      '0'
      *
     C                   Z-ADD     1             X
     C     *ZEROS        LOOKUP    YM(X)                                  50
      *
     C     *IN50         IFEQ      '1'
     C                   MOVE      XXYYMM        YM(X)
     C                   ENDIF                                                  *IN50 IFEQ
      *
     C                   ENDIF                                                  *IN50 IFEQ
      *
     C                   Z-ADD     XXAMT         AM(X)
     C                   MOVE      *BLANK        FPSTAT
     C                   MOVE      Q1USER        FPUSER
     C                   MOVE      Q1PGM         FPPGM
     C                   MOVE      DSTIME        FPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FPDT
      *
     C                   UPDATE    PFP01
      *
     C                   ENDIF                                                  XXAMT IFEQ
      *
     C                   ENDIF                                                  *IN61 IFEQ
      *
     C     FPKY2         READE     PFP01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   MOVE      '1'           *INLR
      *==================================================================*
      *   Level 1 Calculations             ** Subroutine **   S U B 0 1  *
      *==================================================================*
     C*
     C     SR1ST         BEGSR
     C**
     C**    CALL PGM TO CALCULATE FINANCIAL EARNED PREMIUM
     C**
     C                   MOVEL     ELCO#         SRCO#             3
     C                   MOVEL     ELFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     ELEMP#        SREMP#            9
     C                   MOVEL     ELDIV         SRDIV             5
     C*
     C                   Z-ADD     XXYMD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTE             8 0
     C*
     C                   MOVE      XXDTE         SRDATE            8
     C                   MOVE      ' '           QQERR             1
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT2'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    SRDATE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         XXPREM           11 2
     C                   MOVE      SRAMT2        XXMPRE           11 2
     C*
     C                   ENDSR
**
312831303130313130313031
