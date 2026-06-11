     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W9605R - Prompt - Loss Control Tracking Report
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: Menu                                              *
     H*                                                               *
     H*  CALLS TO:  W9605C - Submit                                   *
     H*                                                               *
     H*  CREATED: 04/19/99 C:61 PROJECT 102AK                         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/17/01 C:61  PROJECT 102BZ                                  *
     H*                Add secondary sort by Visit Date or Emp#.      *
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
      *
      *
     FW9605F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
      *  System company
     FSMCOL     IF   E           K DISK
      *
      *  Fund Master
     FWMFNL     IF   E           K DISK
      *
      *  WC User Parm
     FWMUPL     IF   E           K DISK
      *
      *  System User Parm
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      *
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
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  System user key
      *
     C     SMUPKY        KLIST
     C                   KFLD                    SCSTAF
      *
      *  FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
      *
      *
      *****************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
     C   60              MOVEL(P)  *BLANKS       SCNAME
     C*
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C                   SETON                                        90
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF IFEQ
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *   COMMAND 01 = END OF JOB
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKA IFEQ
      *
      *   Process Function Keys
      *
     C   KJ              EXSR      KJDO                                         F10=Search
      *
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
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCNAME
     C   60              MOVEL(P)  *BLANKS       SCNAME
      *
     C                   GOTO      STRT01
      *
     C                   ENDIF                                                  *INKM IFEQ
      *
     C                   ENDIF                                                  *IN11 IFEQ
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
      *
      *  Staff code must be valid if entered
      *
     C     SCSTAF        IFNE      *BLANKS
     C     SMUPKY        CHAIN     PUP02                              72
     C   72              GOTO      STRT
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  SCSTAF
      ***** BEG ADD ******* 10/17/01 **********************************
      *
      *  Secondary Sort must be specified
      *
     C     SCVE          IFNE      'V'
     C     SCVE          ANDNE     'E'
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 10/17/01 **********************************
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         #1CO#             3
     C                   MOVE      UPFND         #1FND             3
     C                   MOVE      SCFRDT        #1FRDT            8
     C                   MOVE      SCTODT        #1TODT            8
     C                   MOVEL(P)  SCSTAF        #1STAF            3
      *
     C                   CALL      'W9605C'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FRDT
     C                   PARM                    #1TODT
     C                   PARM                    #1STAF
      ***** BEG ADD ******* 10/17/01 **********************************
     C                   PARM                    SCVE
      ***** END ADD ******* 10/17/01 **********************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *===============================================================
      * KJDO - SEARCH
      *===============================================================
      *
     C     KJDO          BEGSR
      *
     C                   MOVE(P)   Q2RECF        XXRECF           10
     C                   SELECT
      *
      *  FROM DATE
      *
     C     HFLD          WHENEQ    'SCFRDT'
     C                   SETON                                        55
      *
      *  TO DATE
      *
     C     HFLD          WHENEQ    'SCTODT'
     C                   SETON                                        56
      *
      *  STAFF CODE
      *
     C     HFLD          WHENEQ    'SCSTAF'
      *
     C                   CALL      'S0005R'                             99
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
      *
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PUP03                              99
     C                   MOVEL     UPSTAF        SCSTAF
      *
     C                   ELSE
      *
     C                   SETON                                        71
     C                   ENDIF                                                  ##RRN
      *
     C                   WRITE     SCR99
      *
      *
     C                   ENDSL
      *
     C                   MOVE(P)   XXRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *
      *
