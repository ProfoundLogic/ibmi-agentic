     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * ***************************************************************
      /TITLE W8591R - GENERATE LATE PAYMENTS
      * ***************************************************************
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY CLAIMANT INFORMATION.*
      *                                                               *
      *  CALLED FROM:                                                 *
      *  CALLS TO:                                                    *
      *                                                               *
      * ***************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      * 01/11/13 C:287 Project: SY266_QRY                             *
      *                                                               *
      * 07/23/97 C:47 PROJECT 60AOV                                   *
      *               USE DISABILITY DATE INSTEAD OF DATE LAST WORKED.*
      *               (WMCMP: CMDBDT, NOT CMDTLW)                     *
      * ***************************************************************
     FWMCML1    IF   E           K DISK
     FWDPAL2    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWMWHL     IF   E           K DISK
     FWMLPL     UF A E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C*   KEY LIST FOR THE PAYMENT DETAIL FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFRDT
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C*   KEY LIST FOR LATE PAYMENT FILE
     C*
     C     LPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*
     C*   KEY LIST FOR THE WORK HISTORY FILE
     C*
     C     WHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C     *LIKE         DEFINE    WHDT          XXFRDT
     C     *LIKE         DEFINE    WHDT          XXTODT
     C     *LIKE         DEFINE    WHDT          XXLTDT
     C     *LIKE         DEFINE    WHDT          SVLTDT
     C     *LIKE         DEFINE    WHDT          SVRTDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDATE            8 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTODT        XXFRDT
     C                   MOVE      '01'          XXFRDT
     C*
     C                   TIME                    DSDTTM
     C*
     C*   GET ALL INITIAL COMP PMTS MADE DURING THE MONTH UP TO THE
     C*   ENTERED DATE & DETERMINE IF ITS LATE
     C*
     C     PAKEY         SETLL     PPA01
     C     PAKY1         READE     PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C     PATRDT        ANDLE     XXTODT
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C*
     C*   IF PMT ALREADY IN LATE FILE FOR THE DATE RANGE, GET NEXT PMT
     C*
     C     LPKEY         CHAIN     PLP01                              60
     C     *IN60         IFEQ      '1'
     C*
     C*   GET FIRST COMPENSATION PAYMENT MADE ON THE CASE
     C*
     C                   Z-ADD     PASEQ#        SVSEQ#            5 0
     C     PAKY2         CHAIN     PPA02                              60
     C     *IN60         IFEQ      '0'
     C     PASEQ#        ANDEQ     SVSEQ#
     C*
     C*   THIS IS THE FIRST COMP PMT, THEREFORE CHECK IF LATE
     C*
     C                   EXSR      SRLATE
     C*
     C     PATRDT        IFGT      XXLTDT
     C                   Z-ADD     XXCO#         LPCO#
     C                   Z-ADD     PAFND         LPFND
     C                   Z-ADD     PAFYR         LPFYR
     C                   Z-ADD     PACASE        LPCASE
     C                   Z-ADD     PASEQ#        LPSEQ#
     C                   MOVE      *BLANKS       LPCODE
     C                   Z-ADD     XXLTDT        LPDUE
     C                   Z-ADD     PATRDT        LPTRDT
     C                   MOVE      *BLANK        LPSTAT
     C                   MOVEL     Q1USER        LPUSER
     C                   MOVEL     Q1PGM         LPPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LPDT
     C*
     C                   Z-ADD     DSTIME        LPTIME
     C                   WRITE     PLP01
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
     C     PAKY1         READE     PPA01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*  SRLATE - SUBROUTINE TO DETERMINE IF A COMPENSATION PMT IS LATE
     C*
     C*****************************************************************
     C*
     C     SRLATE        BEGSR
     C*
     C                   Z-ADD     9999999       XXLTDT
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
      *
      *** BEG DEL *** 07/23/97 *******************************************
     C***        CMDTLW    ANDNE0
     C***
     C***                  Z-ADDCMDTLW    XXDTLW  60
     C***                  Z-ADD14        SRDAYS  50
     C***
     C***                  Z-ADDCMDTLW    SRDTI
     C***                  CALL 'SRCVTDAT'DTMDYY
     C***                  MOVELSRDTO     SRDATE  80
      *** END DEL *** 07/23/97 *******************************************
      *
      *** BEG ADD *** 07/23/97 *******************************************
     C     CMDBDT        ANDNE     0
     C*
     C                   Z-ADD     CMDBDT        XXDTLW            6 0
     C                   Z-ADD     14            SRDAYS            5 0
     C*
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
      *** END ADD *** 07/23/97 *******************************************
     C*
     C*   CALL SUBROUTINE TO CALCULATE DAYS IN THE FUTURE
     C*
     C                   EXSR      FUTR
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLTDT
     C*
     C                   Z-ADD     0             SVRTDT                         SV RETURN DT
     C                   Z-ADD     0             SVLTDT                         SV LOST TIME
     C*
     C*   READ WORK HISTORY FILE FOR ADDITIONAL RETURN TO WORK &
     C*   LOST TIME DATES
     C*
     C     WHKEY         CHAIN     PWH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     WHWKST        IFEQ      'R'
     C*
     C*   IF RETURN TO WORK DATE FOUND & IS < OR = TO CALCULATED LATE
     C*   DATE, THEN PAYMENTS WILL NOT BE LATE BECAUSE THE CLAIMANT
     C*   HAS NOT BEEN OUT OF WORK MORE THAN 14 DAYS.
     C*
     C*   IF RETURN TO WORK DATE IS > CALCULATED LATE DATE, THEN PMTS
     C*   MADE AFTER XXLTDT WILL BE LATE. END LOOP.
     C*
     C     WHDATE        IFLE      XXLTDT
     C                   Z-ADD     XXLTDT        SVLTDT
     C                   Z-ADD     9999999       XXLTDT
     C                   Z-ADD     WHDATE        SVRTDT
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   END
     C*
     C                   ELSE
     C*
     C*   IF ANOTHER LOST TIME DATE IS FOUND, CALCULATE # DAYS BETWEEN
     C*   THE LAST RETURNED TO WORK DATE & THE NEW LOST TIME DATE. THEN
     C*   ADD THOSE DAYS TO THE CURRENT LATE DATE.
     C*
     C     SVRTDT        IFNE      0
     C*
     C                   Z-ADD     SVRTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*
     C                   Z-ADD     WHDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTO              8 0
     C*
     C                   MOVE      XXFROM        QQFRDT            8 0
     C                   MOVE      XXTO          QQTODT            8 0
     C                   Z-ADD     0             SRDAYS
     C                   MOVE      ' '           SRERR
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    QQTERM
     C*
     C*  ADD NUMBER OF DAYS WORKED TO LATE DATE
     C*
     C                   Z-ADD     SVLTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C*
     C                   EXSR      FUTR
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLTDT
     C*
     C                   Z-ADD     0             SVRTDT
     C                   Z-ADD     0             SVLTDT
     C                   END
     C*
     C                   END
     C  N60WHKEY         READE     PWH01                                  60
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  FUTR - SUBROUTINE TO CALCULATE DAYS IN FUTURE
     C*
     C*****************************************************************
     C*
     C     FUTR          BEGSR
     C*
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C                   ENDSR
