     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H/Title F8070R - Upload Process - Assigning Member Nmae
     H*
     H*****************************************************************
     H* Created on 04/05/2011 by c:198 for Project 45MHZ
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     FF8070F    CF   E             WORKSTN
     F                                     INFDS(FIDS01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     D*
     D OC              S              1    DIM(500)
     D*
     D OPTNAM          C                   CONST('Ancillary Bill Upload -
     D                                     Process')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ELSE
     C                   MOVEL     UPCO#         ##CO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
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
     C*
     C                   MOVEL(P)  OPTNAM        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
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
     C*
     C                   ENDIF                                                  *IN11 = '1'
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   CALL      'F8070C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##RTCD            1
     C*
     C                   SELECT
     C                   WHEN      ##RTCD = 'F'
     C                   WHEN      ##RTCD = '2'
     C                   WHEN      ##RTCD = '3'
     C                   WHEN      ##RTCD = '4'
     C                   ENDSL
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   SETON                                        LR
