     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* TITLE CVTCMSICDP - ICD10 UPLOAD PROCESS - PROMPT PROGRAM
     H*
     H*****************************************************************
     H*    PROGRAM: CVTCMSICDP                                        *
     H*    CREATED:  4/27/15   PROJ: WC2261   C:53                    *
     H*    DESCRIPTION: ICD 10 UPLOAD                                 *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/11/18 C:079 Support: 146/75335                            *
     H*                 Add a note to the screen to run after hours   *
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
     FCVTCMSICDPCF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D*
     D PGMNAM          C                   CONST('CVTCMSICDS')
     D XXPRDS          C                   CONST('DIAGNOSIS CODES UPLOAD')
     D XXIFS           C                   CONST('/CMS/ICD10')
     D XXICD1          C                   CONST('ICD10.csv')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D ICD10LOAD       S             50    DTAARA
     D                 ds

     D  ICDLOAD                1     50
     D  DTICD1                 1     20
     D  DTIFS                 21     50

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
     C                   IN        ICD10LOAD
     C                   MOVE      ICD10LOAD     ICDLOAD
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
     C                   MOVE      XXPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   MOVE      *BLANKS       SCIFS
     C                   MOVE      *BLANKS       SCICD1
     C                   MOVEL     DTIFS         SCIFS
     C                   MOVEL     DTICD1        SCICD1
     C****               MOVEL     XXIFS         SCIFS
     C****               MOVEL     XXICD1        SCICD1
     C                   SETON                                        66
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
     C     *IN66         IFEQ      '1'
     C*
     C     SCIFS         IFEQ      ' '
     C     SCICD1        OREQ      ' '
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C**   SCIFS         IFNE      ' '
     C**   SCICD1        ANDNE     ' '
     C**                 CALL      'W7492R'
     C**                 PARM                    SCIFS
     C**                 PARM                    SCICD1
     C**                 PARM                    FOUND             1
     C*
     C**   FOUND         IFEQ      '0'
     C**                 MOVE      '1'           *IN72
     C**                 GOTO      STRT
     C**                 ENDIF
     C*
     C**                 ENDIF                                                  SCIFS <> ' '
     C*
     C                   ENDIF                                                  *IN66 = '1'
     C                   CALL      PGMNAM
     C                   PARM                    SCIFS
     C                   PARM                    SCICD1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
