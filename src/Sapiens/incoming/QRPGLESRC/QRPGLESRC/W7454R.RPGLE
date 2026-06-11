     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7454R - Prompt - Agent Account  Current Deposit Slip Rpt
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: Menu                                              *
     H*                                                               *
     H*  CALLS TO:  W7454C - Submit                                   *
     H*                                                               *
     H*  CREATED: 04/09/99 C:61 PROJECT 96WAD                         *
     H*  IN BASE: 07/19/99 C:33 PROJECT WC899                         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
      *
     FW7454F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *

     D OPTNM           C                   CONST('Agent Account Curren-
     D                                     t Deposit Slip')
     D COMAPI          C                   CONST('WA001R')
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *_______________________________________________________________
      *
     C/COPY SCOPYDTPL
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
      *_______________________________________________________________
      *
      * Center Screen Prompt in display field
      *
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
      *
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL(P)  ##DATA        SCPRDS
      *_______________________________________________________________
      *
     C     TOP           TAG
      *
      *  GET THE USER PARAMETER INFORMATION
      *
     C                   CALL      COMAPI        COMPL                  99
      *
     C                   MOVE      ##CONM        SCCONM
      *
     C                   TIME                    DSDTTM
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *_______________________________________________________________
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C                   ENDSL
      *
      *  PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *  F3=EXIT
      *
     C     *INKC         CABEQ     *ON           TERM                     LR
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      '1'
      *
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN10 IFEQ
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'                             99
      *
     C                   GOTO      TOP
      *
     C                   ENDIF                                                  *INKM IFEQ
      *
     C                   ENDIF                                                  *IN11 IFEQ
      *_______________________________________________________________
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  'FROM' DATE MUST BE VALID
      *
     C     SCFRDT        IFEQ      0
      *
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FRDT            7 0
      *
     C                   ENDIF                                                  SCFRDT IFEQ
      *
      *  'TO' DATE MUST BE VALID
      *
     C     SCTODT        IFEQ      0
      *
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TODT            7 0
      *
     C                   ENDIF                                                  SCTODT IFEQ
      *
      *  DATES MUST BE IN ASCENDING ORDER
      *
     C     X1FRDT        IFGT      X1TODT
     C                   SETON                                        47
     C                   GOTO      STRT
     C                   ENDIF
      *_______________________________________________________________
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      ##CO#         #@CO#             3
     C                   MOVE      SCFRDT        ##FRDT            8
     C                   MOVE      SCTODT        ##TODT            8
      *
     C                   CALL      'W7454C'
     C                   PARM                    #@CO#
     C                   PARM                    ##FRDT
     C                   PARM                    ##TODT
     C                   PARM                    ##OUTQ
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *
