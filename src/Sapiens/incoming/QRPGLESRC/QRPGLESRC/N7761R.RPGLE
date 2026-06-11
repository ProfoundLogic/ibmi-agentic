     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7761R - INVOICED ACTIVITY REPORT - PROMPT SCRN PGM
     H**************************************************************************
     H*
     H* 08/18/00 C:62 PROJ:SUPP                                                *
     H*               CORRECT EDIT OF DATES, TO KEEP FROM BOMBING.             *
     H*
     H* 05/11/98 C:90    PROJ:MC181                                            *
     H*                  CHANGED SCREEN TO MEET VERSION 4 STANDARDS            *
     H**************************************************************************
     FN7761F    CF   E             WORKSTN
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
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             SCTDT
     C                   Z-ADD     0             SCFDT
     C                   MOVE      *BLANKS       SCRTYP
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
     C*****************************
     C*   COMMAND 03 = EXIT       *
     C*****************************
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
      **** BEGIN ADD **** 05/07/98 ************************************
      *
     C*********************************
     C*    REPORT TYPE SEARCH         *
     C*********************************
      *
     C     *INKJ         IFEQ      '1'
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
     C     *INKL         CABEQ     *ON           STRT                     26
     C     SCRTYP        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN26
     C                   ENDIF                                                  IFEQ *BLANKS
      *
     C     ##RRNT        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCRTYP
     C                   MOVE      *ON           *IN26
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCSYS
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  IFEQ *INKJ
      *
      **** END   ADD **** 05/07/98 ************************************
     C*****************************
     C*   PROCESS HELP REQUESTS
     C*****************************
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
     C                   GOTO      STRT
     C                   END
     C*
     C*****************************
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*****************************
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
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END                                                    *IN11
     C*********** END OF COMMAND KEY SECTION **********************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*
     C*   VALIDATE THE DATE ENTERED
     C*
     C*
      **** BEGIN ADD **** 05/11/98 ************************************
     C     SCFDT         IFNE      *ZEROS
     C*** BEGIN DEL *** 08/18/00 **************************************
     C********** SCTDT     ANDNE*ZEROS
     C*** END   DEL *** 08/18/00 **************************************
      **** END   ADD **** 05/11/98 ************************************
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     20
     C*** BEGIN ADD *** 08/18/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/18/00 **************************************
     C*
     C*** BEGIN DEL *** 08/18/00 **************************************
     C     SCTDT         IFNE      *ZEROS
     C*** END   DEL *** 08/18/00 **************************************
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     21
     C*** BEGIN ADD *** 08/18/00 **************************************
     C                   ENDIF
     C*
     C     SCFDT         IFNE      *ZEROS
     C     SCTDT         ANDNE     *ZEROS
     C*** END   ADD *** 08/18/00 **************************************
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
      **** BEGIN ADD **** 05/11/98 ************************************
     C                   ENDIF                                                  IFNE SCFDT
      **** END   ADD **** 05/11/98 ************************************
     C*
     C*   THE REPORT TYPE MUST BE 'D', 'S', OR 'B'
     C*
     C     SCRTYP        IFNE      'D'
     C     SCRTYP        ANDNE     'S'
     C     SCRTYP        ANDNE     'B'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPSUB#        QQSUB#            5
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
      **** BEGIN ADD **** 05/07/98 ************************************
     C                   ENDIF                                                  IFEQ SCFRDT
      **** END   ADD **** 05/07/98 ************************************
     C                   MOVE      SCRTYP        QQRTYP            1
     C                   CALL      'N7761C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQRTYP
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
