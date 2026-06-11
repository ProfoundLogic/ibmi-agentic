     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7051R - ALL (PAID) CHARGES REPORT BY CASE - PROMPT PGM
     FN7051F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSML     IF   E           K DISK

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
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR THE CASE MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   ELSE
     C                   MOVE      UPFND         SCFND
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C                   MOVEL     UPSUB#        SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 06 = CASE # SEARCH
     C*
     C     *INKF         IFEQ      '1'
     C                   MOVE      SCFND         Q1FND             3 0          ADD 10/05/92
     C                   CALL      'F0570R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    XXSS#             9
     C                   PARM                    QQRTCD            1
     C                   PARM                    Q1FND                          ADD 10/05/92
     C                   MOVE      Q1FND         SCFND                          ADD 10/05/92
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFYR         OREQ      *ZERO
     C     QQRTCD        OREQ      '*'
     C                   GOTO      STRT
     C                   END                                                    END IF QQRTC
     C*
     C                   END                                                    END IF *INKE
     C*
     C*   COMMAND 07 = GROUP SEARCH
     C*
     C     *INKG         IFEQ      '1'
     C                   CALL      'F0506R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFND
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFND         OREQ      *ZERO
     C     QQRTC         OREQ      '*'
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
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
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   ELSE
     C                   MOVE      UPFND         SCFND
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C                   MOVEA     '000'         *IN(30)
     C*
     C*  DETERMINE WHAT WAS ENTERED
     C*
     C     SCFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN30
     C                   END
     C     SCFYR         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN31
     C                   END
     C     SCCASE        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C*   VALIDATE ENTRYS KEYED IN CORRECT ORDER
     C*
     C*   IF GROUP IS BLANK, YEAR AND CASE MUST BE BLANK
     C     *IN30         IFEQ      '1'
     C     *IN31         ANDEQ     '0'
     C     *IN30         OREQ      '1'
     C     *IN32         ANDEQ     '0'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   IF YEAR IS BLANK, THEN CASE MUST BE BLANK
     C     *IN31         IFEQ      '1'
     C     *IN32         ANDEQ     '0'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE GROUP
     C*
     C     *IN30         IFEQ      '0'
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20                          INVALID FUND
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C                   END                                                    END IF *IN30
     C*
     C*   VALIDATE THE CASE
     C*
     C     *IN32         IFEQ      '0'
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   CALL      'N7051C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
