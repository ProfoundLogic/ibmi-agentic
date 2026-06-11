     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F6219R -  PROMPT FOR EDI ACKNOWLEDGMENT PROCESSING
     H*****************************************************************
     H*
     H*  CREATED: 10/12/07    C:189   PROJECT: TX029
     H*                             COPIED FROM W8078R
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 00/00/00 C:00 Project: 00000
     H*                             COPIED FROM W8078R
     H*****************************************************************
     FF6219F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     FFEA2L     IF   E           K DISK
     F**          PA101                             KIGNORE

     D*________________________________________________________________
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*________________________________________________________________
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*         KEY LIST FOR THE ACKNOWLEDGMENT HEADER FILE
     C*
     C     HAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJST
     C                   KFLD                    XCOTDT
     C                   KFLD                    XCOTTM
     C*
     C*
     C*   ENTRY PARM LIST
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    A2OTDT        XCOTDT
     C     *LIKE         DEFINE    A2OTTM        XCOTTM
     C                   MOVE      *ZEROS        XXDATE            8 0
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         UPCO#
     C                   END
     C                   Z-ADD     UPCO#         XXCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      QQPRDS        SCPRDS
     C                   TIME                    DSDTTM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  INITIALIZE STATE AND ORIGINAL TRANSMISSION DATE/TIME
     C*
     C                   MOVE      *BLANK        SCJST
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SCOTDT
     C                   MOVE      DSTIME        SCOTTM
     C*
     C*** BEG ADD *** 06/27/05 ****************************
     C                   MOVE      *BLANK        SCOTDT
     C                   MOVE      *BLANK        SCOTTM
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
     C*   COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 05 = SEARCH HEADERS
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*  JURISDICTION STATE TABLE SEARCH
     C*
     C     HFLD          IFEQ      'SCJST'
     C*
     C                   CALL      'SY600R'
     C                   PARM      '  '          QQKEY             2
     C                   PARM      ' '           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      'A'
     C                   GOTO      TERM
     C                   ENDIF                                                  QQRTCD='A'
     C*
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='B'
     C*
     C     QQKEY         IFEQ      *BLANKS
     C                   GOTO      STRT
     C                   ENDIF                                                  QQKEY=BLKS
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCJST
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTCD='*'
     C*
     C                   ENDIF                                                  HFLD=SCJST
     C*
     C*
     C*     THE JURISDICTION STATE MUST BE ENTERED AND VALID, BECAUSE
     C*     THIS SEARCH ONLY DISPLAYS A LIST FOR ONE JURS STATE AT A TIME.
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCJST         QQJST             2
     C                   MOVE      SCOTDT        QQDATE            8
     C                   MOVE      SCOTTM        QQOTTM            6
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'F6219R1'
     C                   PARM                    QQCO#
     C                   PARM                    QQJST
     C                   PARM                    QQDATE
     C                   PARM                    QQOTTM
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQJST         CABEQ     ' '           STRT
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQDATE        SCOTDT
     C                   MOVE      QQOTTM        SCOTTM
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*     CF13
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*
     C*   VALIDATE JURISDICTION STATE IS IN TABLE FILE
     C*
     C     SCJST         CABEQ     *BLANKS       STRT                     24
     C                   MOVEL(P)  SCJST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   VALIDATE THE TRANSMISSION DATE
     C*
     C                   Z-ADD     SCOTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFNE      '0'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE THE HEADER RECORD.
     C*
     C                   MOVE      SCOTDT        XCDAT4            4            YEAR
     C                   MOVEL     XCDAT4        XCOTDT
     C                   MOVEL     SCOTDT        XCDAT4                          M-D
     C                   MOVE      XCDAT4        XCOTDT
     C                   MOVE      SCOTTM        XCOTTM
     C     HAKY          CHAIN     PA201                              11
     C     *IN11         IFEQ      *ON
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVEL     SCJST         QQJST
     C                   MOVE      XCOTDT        QQOTDT            8
     C                   MOVE      SCOTTM        QQOTTM
     C                   CALL      'F6219C'
     C                   PARM                    QQJST
     C                   PARM                    QQOTDT
     C                   PARM                    QQOTTM
     C                   PARM                    QQCO#
     C                   PARM                    UPOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
