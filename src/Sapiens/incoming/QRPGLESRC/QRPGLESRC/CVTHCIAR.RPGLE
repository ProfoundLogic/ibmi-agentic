     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* TITLE CVTHCIAR - SOLUCIENT ICD9 UPLOAD PROCESS - PROMPT PROGRAM
     H*
     H*****************************************************************
     H*    PROGRAM: CVTHCIAR                                          *
     H*    CREATED: 12/05/01   PROJ: 91MFB    C:62                    *
     H*    DESCRIPTION: SOLUCIENT ICD9 UPLOAD                         *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/18/10 C:198 Project: WC1879                               *
     H*                 Depending on where it is called from Standard *
     H*                 ICD-9 or CMS ICD-9 upload screen is displayed.*
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* 99 - CHAIN TO PUP01 - USER PARMS                              *
     H* 99 - CHAIN TO PCO01 - COMPANY FILE                            *
     H*                                                               *
     H*****************************************************************
     FCVTHCIAF  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D*
     D PGMNAM          C                   CONST('CVTHCIASC')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
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
     C*
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      @@PRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** Beg Add *** 05/18/10 ************************************
     C                   MOVE      *BLANKS       SCIFS
     C                   MOVE      *BLANKS       SCICD9
     C                   SELECT
     C     @@CALL        WHENEQ    'M'
     C                   SETON                                        65
     C     @@CALL        WHENEQ    'W'
     C                   SETON                                        66
     C                   ENDSL
     C*** End Add *** 05/18/10 ************************************
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C**************************************************************************
     C*  F3 = EXIT
     C**************************************************************************
     C*
     C*   CALL FOR END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C*** Beg Add *** 05/18/10 ******************************************
     C     *IN66         IFEQ      '1'
     C*
     C     SCIFS         IFEQ      ' '
     C     SCICD9        OREQ      ' '
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCIFS         IFNE      ' '
     C     SCICD9        ANDNE     ' '
     C                   CALL      'W7492R'
     C                   PARM                    SCIFS
     C                   PARM                    SCICD9
     C                   PARM                    FOUND             1
     C*
     C     FOUND         IFEQ      '0'
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCIFS <> ' '
     C*
     C                   ENDIF                                                  *IN66 = '1'
     C*** End Add *** 05/18/10 ******************************************
     C                   CALL      PGMNAM
     C*** Beg Add *** 05/18/10 ******************************************
     C                   PARM                    SCIFS
     C                   PARM                    SCICD9
     C*** End Add *** 05/18/10 ******************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
