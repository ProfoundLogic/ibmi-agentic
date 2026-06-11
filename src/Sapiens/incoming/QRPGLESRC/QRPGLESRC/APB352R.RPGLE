     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE APB352R- PRINT A/P CHECK REGISTER FOR SELECTED DATE RANGE
     H*                                                               *
     H*  CALLED BY: MENU                                              *
     H*                     - PROCESS VOUCHER DATA                    *
     H*                                                               *
     H*  CALLS TO:  W0006R - CHANGE USER PARAMETERS                   *
     H*             BKD020R - BANK SEARCH                             *
     H*             APB353C - PRINT CHECK REGISTER                    *
     H*                                                               *
     H*  PROGRAMMED BY: 107  04/09/97
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  C:___   __/__/__ - ____________________________________      *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       22      GENERAL USAGE (CHAIN)                           *
      *****************************************************************
     FAPB352F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     FAPBKL1    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *  FUND MASTER KEY LIST

     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*   DEFINE THE PARM LIST.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
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
      *
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      * LOAD FUND # AND NAME
     C                   EXSR      USRPRM
     C*
     C*  GET BANK & DATE RANGE ***
     C*
     C     AGAIN         TAG
     C                   EXFMT     SCR01
     C                   MOVEA     '000'         *IN(22)
      *
      * F3 - EXIT
      *
     C     *INKC         CABEQ     *ON           OUT
      *
      * F13 - CHANGE USER PARMS
      *
     C     *INKM         IFEQ      *ON
     C                   EXSR      USRPRM
     C                   GOTO      AGAIN
     C                   END
      *
      * F5 - BANK SEARCH
      *
     C     *INKE         IFEQ      *ON
     C                   CALL      'BKD020R'
     C                   PARM                    SCBNK#
     C                   PARM                    QQRTC             1
     C     SCBNK#        CHAIN     PBK01                              22
     C*
     C     *IN22         IFEQ      *ON
     C                   CLEAR                   SCBNAM
     C                   MOVEL     'INVALID'     SCBNAM
     C                   GOTO      AGAIN
     C                   ELSE
     C                   MOVEL     BKNAME        SCBNAM
     C                   END
     C                   GOTO      AGAIN
     C                   END
     C*
     C     SCBNK#        CHAIN     PBK01                              22
     C*
     C     *IN22         IFEQ      *ON
     C                   CLEAR                   SCBNAM
     C                   MOVEL     'INVALID'     SCBNAM
     C                   GOTO      AGAIN
     C                   ELSE
     C                   MOVEL     BKNAME        SCBNAM
     C                   END
     C*
     C*  CHECK DATE VALIDITY
     C*
     C                   Z-ADD     SCBDTE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      AGAIN
     C                   END
     C*
     C                   Z-ADD     SCEDTE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      AGAIN
     C                   END
      *
      * PRINT REPORT
      *
     C                   MOVE      SCBNK#        XXBNK#            3
     C                   MOVE      SCBDTE        XXBDTE            8
     C                   MOVE      SCEDTE        XXEDTE            8
     C                   MOVE      SCCTYP        XXCTYP            1
     C                   MOVE      UPCO#         XXCO#             3
     C                   MOVE      UPFND         XXFND#            3
      *
     C                   CALL      'APB352C'
     C                   PARM                    QQKEY
     C                   PARM                    QQQUE
     C                   PARM                    QQMODE
     C                   PARM                    XXCO#
     C                   PARM                    XXFND#
     C                   PARM                    XXBNK#
     C                   PARM                    XXBDTE
     C                   PARM                    XXEDTE
     C                   PARM                    XXCTYP
      *
     C     OUT           TAG
     C                   MOVE      *ON           *INLR
      ************************************************************
      *  USRPRM - CHANGE USER PARMS/LOAD DISPLAY FIELDS.         *
      ************************************************************
     C     USRPRM        BEGSR
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C                   END
      *
     C     Q1USER        CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     UPFND         SCFND#
     C                   ELSE
     C                   CLEAR                   UPFND
     C                   CLEAR                   FNNAME
     C                   END
     C                   END
      *
     C                   ENDSR
      ************************************************************
