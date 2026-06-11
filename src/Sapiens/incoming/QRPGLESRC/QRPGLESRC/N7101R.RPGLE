     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7101R - DELETED A/R TRANS WITHIN DATE RANGE - PROMPT PGM
     H**************************************************************************
     H* 05/12/98 C:90    PROJ:MC181                                            *
     H*                  CHANGED SCREEN TO MEET VERSION 4 STANDARDS            *
     H*
     H* 07/05/95 C:35    REQ:PROJ: NW008                                       *
     H*                  1. ALLOW SYSTEM TYPE 'M'                              *
     H**************************************************************************
     FN7101F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 05/07/98 ************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      **** END   ADD **** 05/07/98 ************************************

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
     C     *LIKE         DEFINE    SMDT          XXFRDT
     C     *LIKE         DEFINE    SMDT          XXTODT
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
     C********************************
     C*   COMMAND 01 = EXIT          *
     C********************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
      **** BEGIN ADD **** 05/07/98 ************************************
      *
     C**********************************
     C* F10 = SYSTEM SEARCH            *
     C**********************************
      *
     C     *INKJ         IFEQ      '1'
      *
     C     HFLD          IFEQ      'SCSYS'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNP            9 0
     C                   PARM      'SYS'         ##TBID           10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     25
      *
     C     ##RRNP        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCSYS
     C                   MOVE      *ON           *IN26
     C                   ELSE
     C                   MOVE      *ON           *IN25
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSYS
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  IFEQ *INKJ
      **** END   ADD **** 05/07/98 ************************************
     C********************************
     C*   PROCESS HELP REQUESTS
     C********************************
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
     C********************************
     C*   PROCESS CHANGE OF USER PARAMETERS
     C********************************
     C*
      **** BEGIN DEL **** 05/07/98 ************************************
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
     C******************** WRITESCR99
     C*
     C**********  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** MOVE *ZERO     UPSUB#
     C******************** END
     C******************** GOTO STRT01
     C******************** END
      **** END   DEL **** 05/07/98 ************************************
     C*
     C                   END                                                    END IF *IN11
     C********* END OF COMMAND KEY SECTION ***************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE SYSTEM ID - H, P, OR N ONLY
     C*
     C     SCSYS         IFNE      'H'
     C     SCSYS         ANDNE     'P'
     C     SCSYS         ANDNE     'N'
     C     SCSYS         ANDNE     'U'
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     SCSYS         ANDNE     'M'
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED FROM DATE
     C*
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCFRDT        IFNE      *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF                                                  IFNE *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
      **** BEGIN DEL **** 05/12/98 ************************************
     C********** SCTODT    IFEQ *ZERO
     C******************** MOVE UPPRDT    SCTODT
     C******************** END
      **** END   DEL **** 05/12/98 ************************************
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCTODT        IFNE      *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF                                                  IFNE *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C*
     C*  VALIDATE THE ENTERED DATES FOR ASCENDING ORDER
     C*
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ANDNE     *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C     XXFRDT        CABGT     XXTODT        STRT                 24
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF                                                  IFNE *ZEROS
      **** END   ADD **** 05/12/98 ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      SCSYS         QQSYS             1
      **** BEGIN ADD **** 05/12/98 ************************************
     C     SCFRDT        IFEQ      *ZEROS
     C                   MOVE      '01011940'    QQFRDT            8
     C                   ELSE
      **** END   ADD **** 05/12/98 ************************************
     C                   MOVE      SCFRDT        QQFRDT            8
      **** BEGIN ADD **** 05/12/98 ************************************
     C                   ENDIF
     C     SCTODT        IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQTODT            8
     C                   ELSE
      **** END   ADD **** 05/12/98 ************************************
     C                   MOVE      SCTODT        QQTODT            8
      **** BEGIN ADD **** 05/07/98 ************************************
     C                   ENDIF                                                  IFEQ SCTODT
      **** END   ADD **** 05/07/98 ************************************
     C                   CALL      'N7101C'
     C                   PARM                    QQSYS
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
