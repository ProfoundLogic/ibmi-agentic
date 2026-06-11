     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7200R - CONTRACT AND BILLING CHANGE REPORT
     H*
     H****************************************************************
     H*                                                              *
     H* PROGRAMMER C: 197                                            *
     H* CREATED     : 08/18/08                                       *
     H* PROJECT     : 114MPX                                         *
     H****************************************************************
     H*  INDICATORS:                                                 *
     H* SCR01                                                        *
     H*                                                              *
     H*  25 - POSITION CURSOR AT REPORT TYPE                         *
     H*  26 - POSITION CURSOR AT FROM DATE                           *
     H*  28 - ERROR MESSAGE FOR INVALID REPORT TYPE ENTERED          *
     H****************************************************************
     H* REVISIONS                                                    *
     H*                                                              *
     H****************************************************************
     FN7200F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPDT          XXFRDT
     C     *LIKE         DEFINE    UPDT          XXTODT
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   SETOFF                                       26
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*   COMMAND 03 = EXIT           *
     C*********************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C                   GOTO      STRT01
     C                   END                                                    *IN11
     C*
     C******** END OF COMMAND KEY SECTION *****************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATING REPORT TYPE FIELD
     C*
     C     SCRPTY        IFNE      'C'
     C     SCRPTY        ANDNE     'B'
     C     SCRPTY        ANDNE     ' '
     C                   SETON                                        28
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  VALIDATE THE ENTERED FROM DATE
     C*
     C*
     C     SCFRDT        IFEQ      *ZERO
     C*
     C                   Z-ADD     01011901      SCFRDT
     C*
     C                   ENDIF                                                  SCFRDT = ZERO
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
     C     SCTODT        IFEQ      *ZERO
     C*
     C                   Z-ADD     12312099      SCTODT
     C*
     C                   ENDIF                                                  SCFRDT = ZERO
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE THE ENTERED DATES FOR ASCENDING ORDER
     C*
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        CABGT     XXTODT        STRT                 24
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCRPTY        QQRPTY
     C*
     C                   CALL      'N7200C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQRPTY            1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
