     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7901R - EOB EDIT REPORT - PROMPT SCRN PGM
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  05/11/98 C:90 PROJ:MC181                                     *
     H*                CHANGED SCREEN TO MEET VERSION 4 STANDARDS     *
     H*                COMBINED ALL SEARCHES INTO F10 SEARCH          *
     H*                                                               *
     H*****************************************************************
     FN7901F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
      **** BEGIN ADD **** 05/08/98 ************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
      **** END   ADD **** 05/08/98 ************************************

     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
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
     C                   MOVE      UPSUB#        SCSUB#
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
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             SCTDT
     C                   Z-ADD     0             SCFDT
     C                   MOVE      *BLANKS       SCBSTA
     C                   MOVE      *BLANKS       SCBTYP
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
     C*********************************
     C*   COMMAND 03 = EXIT          *
     C*********************************
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
      **** BEGIN CHG **** 05/08/98 ************************************
      *
     C*********************************
     C*      F10 = SEARCH             *
     C*********************************
      *
     C     *INKJ         IFEQ      '1'
      *
     C*********************************
     C*      SUBSCRIBER SEARCH        *
     C*********************************
      *
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     27
      *
     C     ##RRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN28
     C                   ELSE
     C                   MOVE      *ON           *IN27
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSUB#
      *
     C*******************************
     C*    SYSTEM SEARCH            *
     C*******************************
      *
     C     HFLD          IFEQ      'SCBTYP'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNP            9 0
     C                   PARM      'SYS'         ##TBID           10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     31
      *
     C     ##RRNP        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCBTYP
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCBTYP
      *
     C*********************************
     C*   REPORT TYPE SEARCH          *
     C*********************************
      *
     C     HFLD          IFEQ      'SCRTYP'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNT            9 0
     C                   PARM      'RPTY'        ##TBID           10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     32
      *
     C     ##RRNT        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCRTYP
     C                   MOVE      *ON           *IN27
     C                   ELSE
     C                   MOVE      *ON           *IN32
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCRTYP
      *
     C                   MOVE      *ON           *IN90
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ *INKJ
     C*********************************
     C**********   COMMAND 06 = SUBSCRIBER SEARCH
     C*********************************
     C*
     C********** *INKF     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#  50
     C******************** PARM           QQRTC   1
     C*
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C******************** WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTC     CABEQ'*'       STRT
     C******************** END
     C******************** GOTO STRT
     C******************** END                             *INKF
      **** END   CHG **** 05/08/98 ************************************
     C*
     C*********************************
     C*   PROCESS HELP REQUESTS
     C*********************************
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
      **** BEGIN DEL **** 05/08/98 ************************************
     C*********************************
     C***********  F13= PROCESS CHANGE OF USER PARMS
     C*********************************
     C*
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
      **** END   DEL **** 05/08/98 ************************************
     C*
     C                   END                                                    *IN11
     C************ END OF COMMAND KEY SECTION *************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   SUBSCRIBER NUMBER MUST BE VALID, IF NOT BLANK
     C*
     C     SCSUB#        IFNE      *ZEROS
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN26                           INVLD SB#
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C*   VALIDATE THE DATE ENTERED IF DATE RANGE REQUESTED
     C*
     C     SCFDT         IFNE      *ZEROS
     C     SCTDT         ORNE      *ZEROS
     C*
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     20
     C*
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     21
     C*
     C                   MOVE      SCFDT         SRFRDC            8 0
     C                   MOVE      SCTDT         SRTODC            8 0
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     22
     C*
     C                   END                                                    USE DATE
     C*                                                    RANGE?
     C*
     C*   THE BILL STATUS MUST BE 'I'. 'U', OR 'B'
     C*
     C     SCBSTA        IFNE      'I'
     C     SCBSTA        ANDNE     'U'
     C     SCBSTA        ANDNE     'B'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   THE BILL TYPE MUST BE 'H','P','N','U' OR 'A'
     C*
     C     SCBTYP        IFNE      'H'
     C     SCBTYP        ANDNE     'P'
     C     SCBTYP        ANDNE     'N'
     C     SCBTYP        ANDNE     'U'                                          ADD 12/08/92
     C     SCBTYP        ANDNE     'A'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   THE REPORT TYPE MUST BE 'D', 'S', OR 'B'
     C*
     C     SCRTYP        IFNE      'D'
     C     SCRTYP        ANDNE     'S'
     C     SCRTYP        ANDNE     'B'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCSUB#        QQSUB#            5
      **** BEGIN ADD **** 05/11/98 ************************************
     C     SCFDT         IFEQ      *ZEROS
     C                   MOVE      '01011940'    QQFDT             8
     C                   ELSE
      **** END   ADD **** 05/11/98 ************************************
     C                   MOVE      SCFDT         QQFDT             8
      **** BEGIN ADD **** 05/11/98 ************************************
     C                   ENDIF                                                  IFEQ *ZEROS
     C     SCTDT         IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQTDT             8
     C                   ELSE
      **** END   ADD **** 05/11/98 ************************************
     C                   MOVE      SCTDT         QQTDT             8
      **** BEGIN ADD **** 05/08/98 ************************************
     C                   ENDIF                                                  IFEQ SCFRDT
      **** END   ADD **** 05/08/98 ************************************
     C                   MOVE      SCBSTA        QQBSTA            1
     C                   MOVE      SCBTYP        QQBTYP            1
     C                   MOVE      SCRTYP        QQRTYP            1
     C                   CALL      'N7901C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQBSTA
     C                   PARM                    QQBTYP
     C                   PARM                    QQRTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      SCSUB#        QQSUB#
      **** BEGIN ADD **** 05/11/98 ************************************
     C     SCFDT         IFEQ      *ZEROS
     C                   MOVE      '01011940'    QQFDT             8
     C                   ELSE
      **** END   ADD **** 05/11/98 ************************************
     C                   MOVE      SCFDT         QQFDT
      **** BEGIN ADD **** 05/11/98 ************************************
     C                   ENDIF                                                  IFEQ *ZEROS
     C     SCTDT         IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQTDT             8
     C                   ELSE
      **** END   ADD **** 05/11/98 ************************************
     C                   MOVE      SCTDT         QQTDT
      **** BEGIN ADD **** 05/08/98 ************************************
     C                   ENDIF                                                  IFEQ *ZEROS
      **** END   ADD **** 05/08/98 ************************************
     C                   MOVE      SCRTYP        QQRTYP
     C                   MOVE      SCBTYP        QQBTYP
     C                   CALL      'N7151C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQBTYP
     C                   PARM                    QQRTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
