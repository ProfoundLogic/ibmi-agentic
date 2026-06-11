     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE APB720 - REMOVE HISTORY DATA                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF OPEN QUERY REMARKS DETAIL FILE    *
     H*       60      GENERAL USAGE (CHAIN)                           *
      *****************************************************************
     FAPB720F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPCHL1    UF   E           K DISK
     FAPGDL1    UF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   DEFINE THE PARM LIST.
     C*
     C     *ENTRY        PLIST
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
     C     GDKEY         KLIST
     C                   KFLD                    CHCO#
     C                   KFLD                    CHFAC#
     C                   KFLD                    CHVDR
     C                   KFLD                    CHSEQ1
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CHCO#         XXCO#
     C     *LIKE         DEFINE    CHIDT         XXTEST
     C     *LIKE         DEFINE    CHIDT         XX90
     C*
     C                   TIME                    DSDTTM
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
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C**
     C                   END                                                    END CMDKEYS
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VERIFY THE DATE ENTERED
     C*
     C     SCDATE        IFEQ      *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C******               MOVE UDATE     YR      20
     C                   MOVE      *YEAR         YR                4 0
     C                   MOVEL     UDATE         MO                2 0
     C                   Z-ADD     *DATE         X1TEST            8 0
     C     MO            IFGT      3
     C                   SUB       3             MO
     C                   ELSE
     C                   SUB       1             YR
     C     MO            IFEQ      1
     C                   Z-ADD     10            MO
     C                   ELSE
     C     MO            IFEQ      2
     C                   Z-ADD     11            MO
     C                   ELSE
     C                   Z-ADD     12            MO
     C                   END
     C                   END
     C                   MOVEL     MO            X1TEST
     C                   MOVE      YR            X1TEST
     C                   END
     C*
     C                   Z-ADD     X1TEST        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX90
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTEST
     C*
     C     XXTEST        IFGT      XX90
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*** DELETE HISTORY RECORDS
     C*
     C     READ          TAG
     C                   READ      PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHINVD        IFLE      XXTEST
     C                   DELETE    PCH01
     C*
     C*  DELETE ANY DISTRIBUTION FOR THIS INVOICE
     C*
     C     GDKEY         SETLL     PGD01
     C     LOOPU         TAG
     C     GDKEY         READE     PGD01                                  60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PGD01
     C                   GOTO      LOOPU
     C                   END
     C*
     C                   END
     C                   GOTO      READ
     C                   END
     C*
     C*** END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
