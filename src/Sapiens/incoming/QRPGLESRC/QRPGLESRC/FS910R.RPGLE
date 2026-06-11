     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* TITLE FS910R - TAXONOMY CODE PROMPT PROGRAM
     H*
     H*****************************************************************
     H* CREATED ON 05/07/2009 BY C:62 FOR PROJECT MC332               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFS910F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D OPTNM           C                   CONST('Convert Taxonomy -
     D                                     codes')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
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
     C* GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C* GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              61
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
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    SRDFLD
     C                   PARM      40            SRDLEN
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   EXFMT     SCR01
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C**************************************************************************
     C*  F3 = EXIT
     C**************************************************************************
     C*
     C* CALL FOR END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF
     C*
     C**************************************************************************
     C*  F12 = CANCEL
     C**************************************************************************
     C*
     C* CANCEL THE JOB
     C*
     C     *INKL         IFEQ      '1'
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
     C*================================================================*********
     C*
     C                   MOVE      UPCO#         ##CO#
     C*
     C                   CALL      'FS910C'
     C                   PARM                    ##CO#             3
     C*
     C* END THE PROGRAM
     C*
     C                   SETON                                        LR
