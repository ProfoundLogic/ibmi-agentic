     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7441R - POSTER LABELS
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/07/05  C:65  PROJECT: WC1202                               *
     H*                 PRINT FULL EMPLOYER NAME ON POSTER LABEL.     *
     H*                                                               *
     H* 10/25/99  C:65  PROJECT: WC908                                *
     H*                 PULL STANDARD INCR LIMITS FROM WDRRP INSTEAD  *
     H*                 OF WDF1P.                                     *
     H*                                                               *
     H* 12/02/98  C:61  PROJECT: CM008                                *
     H*                 Print Carrier Policy Number if entered.       *
     H*                 Otherwise, FNDEMPFYR                          *
     H*                                                               *
     H* 08/06/98  C:61  PROJECT: 48WLH                                *
     H*                 Use alternate Company name in Fund Master     *
     H*                 if entered                                    *
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H*  01/08/93 C:11 PROJECT WC117                                  *
     H*                PRINT COMPANY NAME AS "LOCAL REP OR AGENT"     *
     H*                                                               *
     H*  03/15/95 C:24 PROJECT WC405                                  *
     H*                ADD FUND TO INCREASED LIMITS TABLE KEY.        *
     H*                                                               *
     H*  10/10/95 C:11 PROJECT WC453                                  *
     H*                GET THE LAST ACTIVE AGENT AS OF THE FYR, NOT   *
     H*                AS OF THE RUN DATE                             *
     H*                                                               *
     H*  10/30/95 C:35 PROJECT:CM008                                  *
     H*                1. ADDED CAPABILITY TO PRINT FOR A SINGLE      *
     H*                   EMPLOYER/DIVISION.                          *
     H*                2. ADDED CAPABILITY TO PRINT FOR A RANGE OF EFF*
     H*                   DATES.                                      *
     H*                                                               *
     H*  06/13/97 C:62 PROJECT WC681                                  *
     H*                NO MORE HARD CODED 800# USING FNPH1A           *
     H*                                                               *
     H*****************************************************************
      *
      *
     FWMEML     IF   E           K DISK
     FWMEML2    IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     F*** BEGIN CHG ***  10/25/99  ************************************
     F*****WDF1L   IF  E           K        DISK
     FWDRRL     IF   E           K DISK
     F*** END CHG *****  10/25/99  ************************************
     FWTLML1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWXERL     IF   E           K DISK
     FWXERL1    IF   E           K DISK
     F                                     RENAME(PER01:PER02)
      ***** BEG ADD ******* 12/02/98 **********************************
      *
      *  Carrier
     FWMRPL2    IF   E           K DISK
      *
      ***** END ADD ******* 12/02/98 **********************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*  REVISED 7-3-89 JJJ ADDED DS FOR EFFECT & CANCEL DT & SYS DT
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  SLNAM                  1     56
     D  SL                     1     56
     D                                     DIM(56)
     D                 DS
     D  SFNAM                  1     25
     D  SF                     1     25
     D                                     DIM(25)
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
     D                 DS
     D  EMPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
     D                 DS
     D  EMMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  FNPH1                  1      7  0
     D  XXPH11                 1      3  0
     D  XXPH12                 4      7  0
     D                 DS
     D  FNPH2                  1      7  0
     D  XXPH21                 1      3  0
     D  XXPH22                 4      7  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  EMPLOYER MASTER FILE
      *

     C     EMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C     EMKEY2        KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXRGN#            3 0
     C     EMKEY3        KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C     EMKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *   FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *                                                    LJQ 6-16-88
      *   FUND AUXILIARY FILE                                    "
      *                                                          "
     C*** BEGIN CHG ***  10/25/99  ************************************
     C********   F1KEY     KLIST                                 "
     C********             KFLD           XXCO#                  "
     C********             KFLD           XXFND                  "
     C********             KFLD           XXFYR                  "
      *
      *   STATISTICAL REPORTING/RATING FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXSTA
     C                   KFLD                    ELPFDT
      *
     C     RRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXSTA
     C*** END CHG *****  10/25/99  ************************************
      *
      *   EMPLOYER DETAIL FILE
      *
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR             3 0
      *
      *   COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *  KEY LIST FOR AGENT HISTORY FILE
      *
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
      *
      *  KEY LIST FOR AGENT ASSIGNMENT FILE
      *
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHAGT#
      *
      *   KEY LIST FOR AGENT MASTER FILE
      *
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      *  INCR LIMITS TABLE FILE
      *
     C     LMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTA             2
     C                   KFLD                    XXKEY            10
     C                   KFLD                    XXEFDT            7 0
      *
      *  RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
      ***** BEG ADD ******* 12/02/98 **********************************
      *
      *  Carrier
      *
     C     RPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
      ***** END ADD ******* 12/02/98 **********************************
      *
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          XXDATE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        XXFEMP
     C     *LIKE         DEFINE    EMEMP#        XXTEMP
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFFDT            8
     C                   PARM                    QQSORT            1
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQFRGN            3
     C                   PARM                    QQTRGN            3
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQFFDT        XXFFDT            8 0
     C                   MOVE      QQSORT        XXSORT            1
     C                   MOVE      QQFEMP        XXFEMP
     C                   MOVE      QQTEMP        XXTEMP
     C                   MOVE      QQFRGN        XXFREG            3 0
     C                   MOVE      QQTRGN        XXTREG            3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C                   MOVE      ''''          QUOTE             1            LJQ 6-16-88
      *
      *  GET THE COMPANY NAME AND ADDRESS
      *
     C     COKEY         CHAIN     PCO01                              60
      ***** BEG ADD ******* 08/06/98 **********************************
      *
      *  Use alternate Company name in fund master if entered.
      *
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C     FNANAM        ANDNE     *BLANKS
     C     FNAAD1        ANDNE     *BLANKS
     C     FNACTY        ANDNE     *BLANKS
     C     FNAST         ANDNE     *BLANKS
      *
     C                   MOVEL(P)  FNANAM        XXCONM           33
     C                   MOVEL(P)  FNACTY        XXCITY
     C                   MOVEL(P)  FNAST         XXSTAT
     C                   MOVEL     FNAZIP        XXZIP1
     C                   MOVE      FNAZIP        XXZIP2
      *
     C                   EXSR      SR2ND
     C                   MOVEL(P)  XXCSTZ        XXCCSZ           25
      *
      *  LOAD ADDRESS, & CITY, STATE, ZIP FOR ALTERNATE COMPANY
      *
     C                   MOVE      *BLANKS       XXCADR           68
     C                   MOVEL(P)  XXCCSZ        SFNAM            25
     C                   MOVEL(P)  FNAAD1        SLNAM            56
      *
     C                   ELSE
      *
      ***** END ADD ******* 08/06/98 **********************************
      *
      *  REMOVE LEADING BLANKS FROM COMPANY NAME
      *
     C                   Z-ADD     1             X                 2 0
     C     *BLANK        LOOKUP    CONM(X)                            60
     C                   MOVEA     CONM(X)       XXCONM           33
      *
      *  LOAD THE CITY, STATE AND ZIP FOR COMPANY
      *
     C                   MOVE      *BLANKS       XXCCSZ           25
     C                   MOVE      COST          XXSTAT
     C                   MOVEL     COZIP         XXZIP1
     C                   MOVE      COZIP         XXZIP2
     C                   MOVE      COCTY         XXCITY
      *
     C                   EXSR      SR2ND
      *
      ***** BEG CHG ******* 08/06/98 **********************************
     C**********           MOVELXXCSTZ    XXCCSZ
     C                   MOVEL(P)  XXCSTZ        XXCCSZ           25
      ***** END CHG ******* 08/06/98 **********************************
      *
      *  LOAD ADDRESS, & CITY, STATE, ZIP FOR COMPANY
      *
     C                   MOVE      *BLANKS       XXCADR           68
     C                   MOVE      *BLANKS       SLNAM            56
     C                   MOVE      *BLANKS       SFNAM            25
     C                   MOVEL     XXCCSZ        SFNAM
     C                   MOVEL     COADR1        SLNAM
      *
      ***** BEG ADD ******* 08/06/98 **********************************
     C                   ENDIF
      ***** END ADD ******* 08/06/98 **********************************
      *
     C                   EXSR      SR4TH
      *
     C                   MOVEL     SLNAM         XXCADR
      *
      *  GET THE FUND NAME AND PHONE NUMBER
      *
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      *BLANKS       XXNAME           50
     C                   MOVEL     FNNAME        XXNAME
      *
      *   MOVE CORRECT PHONE NUMBER TO OUTPUT
      *
     C     FNPH1         IFEQ      *ZERO
     C                   Z-ADD     XXPH21        XXPHN1            3 0
     C                   Z-ADD     XXPH22        XXPHN2            4 0
     C                   Z-ADD     FNAREA        XXAREA            3 0
      *
     C                   ELSE
      *
     C                   Z-ADD     XXPH11        XXPHN1            3 0
     C                   Z-ADD     XXPH12        XXPHN2            4 0
      *** BEGIN DEL *** 06/13/97 **************************************
      **********           Z-ADD800       XXAREA  30
      *** END   DEL *** 06/13/97 **************************************
      *** BEGIN ADD *** 06/13/97 **************************************
     C     FNPH1A        IFNE      *ZERO
     C                   Z-ADD     FNPH1A        XXAREA
     C                   ELSE
     C                   Z-ADD     800           XXAREA
     C                   ENDIF
      *** END   ADD *** 06/13/97 **************************************
     C                   END
      *                                                    LJQ 6-16-88
      *  GET FUND AUXILIARY RECORD FOR THE EMPLOYER LIABILITY AMOUNT
      *                                                         "
     C*** BEGIN DEL ***  10/25/99  ************************************
     C*********  F1KEY     CHAINPF101                60         "
     C*** END DEL *****  10/25/99  ************************************
      *
      *  CHECK WHICH SORT SELECTION WAS MADE
      *
     C     XXEMP#        IFNE      0
      *
     C     EMKY4         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   EXSR      PRTCTL
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ELSE
      *
     C     XXSORT        IFEQ      'E'
     C     XXFFDT        IFNE      0
      *
      * IF RUNNING FOR AN EFF DATE RANGE, READ WORK FILE
      *
     C     *LOVAL        SETLL     PER01
      *
     C                   READ      PER01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      EREMP#        XXEMP#
     C                   MOVE      ERDIV         XXDIV
     C                   MOVE      ERFYR         XXFYR
      *
     C     EMKY4         CHAIN     PEM01                              61
      *
     C     *IN61         IFEQ      *OFF
     C                   EXSR      PRTCTL
     C                   ENDIF                                                  *IN61 IFEQ
      *
     C                   READ      PER01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ELSE
     C                   Z-ADD     XXFEMP        XXEMP#
     C     EMKEY         SETLL     PEM01
     C     EMKEY3        READE     PEM01                                  60
     C     XXTEMP        DOWGE     EMEMP#
     C     *IN60         ANDEQ     '0'
      *
      * MOVED CODE CONTROLLING PRINT INTO SUBROUTINE PRTCTL
      *
     C                   EXSR      PRTCTL
      *
     C     EMKEY3        READE     PEM01                                  60
     C                   END
      *
     C                   ENDIF                                                  XXFFDT IFNE
      *
     C                   ELSE
      *
     C     XXFFDT        IFNE      0
      *
      * IF RUNNING FOR AN EFF DATE RANGE, READ WORK FILE
      *
     C     *LOVAL        SETLL     PER02
      *
     C                   READ      PER02                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      EREMP#        XXEMP#
     C                   MOVE      ERDIV         XXDIV
     C                   MOVE      ERFYR         XXFYR
      *
     C     EMKY4         CHAIN     PEM01                              61
      *
     C     *IN61         IFEQ      *OFF
     C                   EXSR      PRTCTL
     C                   ENDIF                                                  *IN61 IFEQ
      *
     C                   READ      PER02                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ELSE
      *
     C                   Z-ADD     XXFREG        XXRGN#
     C     EMKEY2        SETLL     PEM02
     C     EMKEY3        READE     PEM02                                  60
     C     XXTREG        DOWGE     EMRGN#
     C     *IN60         ANDEQ     '0'
      *
      * MOVED CODE CONTROLLING PRINT INTO SUBROUTINE PRTCTL
      *
     C                   EXSR      PRTCTL
      *
     C     EMKEY3        READE     PEM02                                  60
     C                   END
     C                   ENDIF                                                  XXFFDT IFNE
      *
     C                   END
     C                   ENDIF                                                  XXEMP# IFNE
      *
     C                   MOVE      '1'           *INLR
      *
      *
      * PRINT CONTROL SUBROUTINE
      *
     C     PRTCTL        BEGSR
      *
      * ONLY PRINT ACTIVE ACCOUNTS
      *
     C                   Z-ADD     1             Z                 3 0
     C     *ZERO         LOOKUP    ED(Z)                                  55
     C  N55              Z-ADD     5             Z
     C   55              SUB       1             Z
      *
     C     Z             IFNE      0
      *
     C     CD(Z)         IFEQ      0
     C     CD(Z)         ORGE      XXDATE
      *
     C                   EXSR      SR1ST
     C                   EXSR      SR3RD
      *
     C                   ENDIF                                                  CD,Z IFEQ 0
      *
     C                   ENDIF                                                  Z IFNE 0
      *
     C                   ENDSR                                                  PRTCTL
      ********************************************************
      *    SUBROUTINE TO DETERMINE WHICH ADDRESS TO USE      *
      ********************************************************
     C     SR1ST         BEGSR
      *
     C*** BEGIN ADD ***  03/07/05  ************************************
     C                   MOVEL(P)  EMNAME        XXNAM1           50
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        XXNAM1
     C                   MOVE(P)   EMNAM2        XXNAM2           50
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVE      *BLANKS       XXADR1           56
     C                   MOVE      *BLANKS       XXECSZ           56
     C     EMMPFL        IFEQ      'M'
      *
      *   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
      *
     C                   MOVE      EMMST         XXSTAT            2
     C                   MOVE      XXZPM1        XXZIP1            5
     C                   MOVE      XXZPM2        XXZIP2            4 0
     C                   MOVE      EMMCTY        XXCITY           25
      *
     C                   EXSR      SR2ND
      *
     C                   MOVEL     XXCSTZ        XXECSZ
     C                   MOVEL     EMMAD1        XXADR1
      *
      *
     C                   ELSE
      *
      *
      *   LOAD CITY, STATE, & ZIP FOR THE PHYSICAL ADDRESS
      *
     C                   MOVE      EMPST         XXSTAT            2
     C                   MOVE      XXZPP1        XXZIP1            5
     C                   MOVE      XXZPP2        XXZIP2            4 0
     C                   MOVE      EMPCTY        XXCITY           25
      *
     C                   EXSR      SR2ND
      *
     C                   MOVEL     XXCSTZ        XXECSZ
     C                   MOVEL     EMPAD1        XXADR1
      *
      *
     C                   END
     C                   ENDSR
      *
      ******************************************************
      *    -SUBROUTINE TO LOAD THE CITY, STATE, AND ZIP    *
      ******************************************************
     CSR   SR2ND         BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
      *
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSTZ           33
      *
     C                   ENDSR
      *****************************************************
      *     SUBROUTINE TO PRINT REQUESTED INFORMATION     *
      *****************************************************
     C     SR3RD         BEGSR
      *
      *  LOAD ADDRESS, & CITY, STATE, ZIP FOR EMPLOYER
      *
     C                   MOVE      *BLANKS       XXEADR           68
     C                   MOVE      *BLANKS       SLNAM            56
     C                   MOVE      *BLANKS       SFNAM            25
     C                   MOVEL     XXECSZ        SFNAM
     C                   MOVEL     XXADR1        SLNAM
      *
     C                   EXSR      SR4TH
      *
     C                   MOVEL     SLNAM         XXEADR
      *
      *  GET THE POLICY FROM AND TO DATES
      *
     C                   Z-ADD     0             XXFRDT
     C                   Z-ADD     0             XXTODT
     C                   MOVE      *BLANKS       XXEMLB           30
     C*** BEGIN DEL ***  10/25/99  ************************************
     C*************        MOVELF1EMLB    XXEMLB
     C*** END DEL *****  10/25/99  ************************************
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
      *
      *  IF GET DESCRIPTION FOR INCREASE LIMITS
      *
     C     ELILCD        IFNE      *BLANKS                                      JJJ 10-25-90
      *                                                    |
     C                   MOVE      *BLANKS       XXKEY                          |
     C                   MOVEL     ELILCD        XXKEY                          |
     C     ELILEF        IFEQ      0                                            |
     C                   Z-ADD     ELPFDT        XXEFDT                         |
     C                   ELSE                                                   |
     C                   Z-ADD     ELILEF        XXEFDT                         |
     C                   END                                                    |
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXSTA
     C     LMKEY1        SETGT     PLM01                              60        |
     C                   READP     PLM01                                  60    |
      *** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C                   READP     PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *** END ADD *****  06/16/98  ************************************
     C  N60XXCO#         COMP      LMCO#                              6060      |
     C  N60XXFND         COMP      LMFND                              6060
     C  N60XXSTA         COMP      LMST                               6060
     C  N60XXKEY         COMP      LMKEY                              6060      |
     C  N60ELPFDT        COMP      LMEFDT                               60      |
     C  N60              MOVEL     LMDESC        XXEMLB                         |
      *                                                    |
     C                   END                                                    |
     C*** BEGIN ADD ***  10/25/99  ************************************
     C     XXEMLB        IFEQ      *BLANKS
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXSTA
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RREMLB        XXEMLB
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  XXEMLB IFEQ
     C*** END ADD *****  10/25/99  ************************************
      *
      *   CHAIN TO AGENT HISTORY FILE
      *
     C                   MOVE      *BLANKS       XXANAM           40
     C                   MOVE      *BLANKS       XXAADR           68
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C     *IN60         IFEQ      '0'
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     EMEMP#
     C     AHDIV         ANDEQ     EMDIV
     C     AHFYR         ANDEQ     XXFYR
      *
      *   CHAIN TO AGENT ASSIGNMENT FILE
      *
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
      *
      *   CHAIN TO AGENT MASTER FILE
      *
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
      *
      *  LOAD THE CITY, STATE AND ZIP FOR AGENT
      *
     C                   MOVE      *BLANKS       XXCCSZ           25
     C                   MOVE      AGMST         XXSTAT
     C                   MOVEL     AGMZIP        XXZIP1
     C                   MOVE      AGMZIP        XXZIP2
     C                   MOVE      AGMCTY        XXCITY
      *
     C                   EXSR      SR2ND
      *
     C                   MOVEL     XXCSTZ        XXCCSZ
      *
      *  LOAD ADDRESS, & CITY, STATE, ZIP FOR AGENT
      *
     C                   MOVE      *BLANKS       SLNAM            56
     C                   MOVE      *BLANKS       SFNAM            25
     C                   MOVEL     XXCCSZ        SFNAM
     C                   MOVEL     AGMAD1        SLNAM
      *
     C                   EXSR      SR4TH
      *
     C                   MOVEL     SLNAM         XXAADR
      *
     C                   END
     C                   END
     C                   END
     C     XXEMLB        COMP      *BLANKS                                25
      *
      ***** BEG ADD ******* 12/02/98 **********************************
      *
      *  Print Carrier Policy Number, if entered.
      *
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
      *
     C                   MOVEL(P)  RPCAP#        XXPNUM           20
      *
     C                   ELSE
      *
     C                   MOVEL(P)  ELFND         XXPNUM
     C                   MOVEL(P)  ELEMP#        XXTMP1            9
     C                   CAT       XXTMP1:0      XXPNUM
     C                   MOVEL(P)  ELFYR         XXTMP1            9
     C                   CAT       XXTMP1:0      XXPNUM
      *
     C                   ENDIF
      *
      ***** END ADD ******* 12/02/98 **********************************
      *
     C                   EXCEPT    DET
     C                   END                                                    JJJ 7-3-89
      *
     C                   ENDSR
      **************************************************************
      *  PROGRAM SUBROUTINE SRNAME - BUILD A COMBINED NAME.        *
      *                                                            *
      *  THIS SUBROUTINE WILL ACCEPT THE FIRST AND LAST NAME       *
      *  FIELDS AND BUILD A COMBINED NAME IN THE LAST NAME FIELD   *
      *  LAST,FIRST                                                *
      **************************************************************
     C     SR4TH         BEGSR
      *
     C                   MOVE      '0'           *IN60
     C                   Z-ADD     56            SX                5 0
     C     *IN60         DOUEQ     '1'
     C     *BLANK        COMP      SL(SX)                             6060
     C     *IN60         IFEQ      '0'
     C                   SUB       1             SX
     C     SX            COMP      0                                    6060
     C                   END
     C                   END
     C                   ADD       1             SX
      *
     C     *IN60         IFEQ      '1'
     C     SX            ANDNE     1
     C     SX            ANDLE     56
     C     SFNAM         ANDNE     *BLANK
      *
     C                   MOVE      ','           SL(SX)
     C                   ADD       2             SX
     C     SX            IFLE      56
     C                   MOVEA     SFNAM         SL(SX)
     C                   END
      *
     C                   END
      *
     C                   ENDSR
      *
      *
     OQSYSPRT   E            DET            1 02
     O                                            9 'Employer:'
     O*** BEGIN CHG ***  03/07/05  ************************************
     O*********                EMNAME    50
     O                       XXNAM1              60
     O          E            DET            1
     O                       XXNAM2              60
     O*** END CHG *****  03/07/05  ************************************
     O          E            DET            1
     O                       XXEADR              70
     O          E            DET            1
     O                                           19 'is providing Worker'
     O                       QUOTE               20
     O                                           42 's Compensation through'
      ***** BEG DEL ******* 12/02/98 **********************************
     O********E* 2             DET
     O**********               XXNAME    55
     O********E* 1             DET
     O**********                         14 'Policy Number:'
     O**********               XXFND     19
     O**********                         20 '-'
     O**********               EMEMP#    29
     O**********               EMDIV Z   36
      ***** END DEL ******* 12/02/98 **********************************
      ***** BEG ADD ******* 12/02/98 **********************************
     O          E            DET            1
     O                       XXNAME              55
     O          E            DET            1
     O                                           14 'Policy Number:'
     O                       XXPNUM              35
     O                                           54 'Effective date:'
     O                       XXFRDT        Y     63
     O                                           64 '-'
     O                       XXTODT        Y     72
     O          E            DET            1
     O                                           14 'Division . . :'
     O                       ELDIV             +001
      ***** END ADD ******* 12/02/98 **********************************
      ***** BEG DEL ******* 12/02/98 **********************************
     O*********                          54 'Effective date:'
     O*********                XXFRDTY   63
     O*********                          64 '-'
     O*********                XXTODTY   72
      ***** END DEL ******* 12/02/98 **********************************
     O          E            DET            1
     O                                           20 'Local representative'
     O                                           30 'or agent:'
     O          E            DET            1
     O                       XXCONM              55
     O          E            DET            1
     O                       XXCADR              70
     O          E            DET            1
     O              N25                           8 'Employer'
     O              N25      QUOTE                9
     O              N25                          21 's Liability:'
     O              N25      XXEMLB              52
     O                                           53 '('
     O                       XXAREA              56
     O                                           57 ')'
     O                       XXPHN1              61
     O                                           62 '-'
     O                       XXPHN2              66
