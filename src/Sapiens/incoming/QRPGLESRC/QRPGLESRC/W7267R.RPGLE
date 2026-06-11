     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7267R - PROMPT FOR CLAIMS TRANSFER AUDIT REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 10/31/00  C:35   PROJECT: WC990
     H*                  CHANMGE TO USE STANDARD PARM FORMAT.
     H*
     H*
     H*  05/09/00 C:53  PROJECT: WC990
     H*                 ADD GROUP AND DATE RANGE TO THE PROCESS.
     H*
     H*****************************************************************
     FW7267F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

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
     C*** BEG ADD *** 05/09/00 **************************
     C                   ELSE
     C     UPXFND        IFEQ      'Y'
     C                   MOVE      *ON           *IN31
     C                   MOVEL     UPFND         SCFND
     C                   ENDIF
     C*
     C*** END ADD *** 05/09/00 **************************
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
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*** BEG ADD *** 05/09/00 **************************
     C*
     C*   PROCESS F10 SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C********** HFLD      IFEQ 'SCFND'
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      '2'           ##MODE
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##CO#             3
     C                   PARM      '000'         ##FND             3
     C                   PARM      '000'         ##FYR             3
     C*
     C                   MOVE      ##FND         SCFND
     C                   GOTO      STRT01
     C*****************    ENDIF
     C                   ENDIF
     C*** END ADD *** 05/09/00 **************************
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
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C*** BEG ADD *** 05/09/00 **************************
     C*                    ELSE
     C*          UPXFND    IFEQ 'Y'
     C*                    MOVE *ON       *IN31
     C*                    MOVELUPFND     SCFND
     C*                    ENDIF
     C*
     C*** END ADD *** 05/09/00 **************************
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** BEG ADD *** 05/09/00 **************************
     C*
     C*   Both dates must be entered
     C*
      **** BEGIN CHG **** 10/31/00 *********************************************
     C**********           SELEC
     C**********
     C********** SCFRDT    WHEQ *ZERO
     C**********           MOVE *ON       *IN22
     C**********           GOTO STRT
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         CABEQ     '1'           STRT                     22
      **** END   CHG **** 10/31/00 *********************************************
     C*
      **** BEGIN CHG **** 10/31/00 *********************************************
     C********** SCTODT    WHEQ *ZERO
     C**********           MOVE *ON       *IN25
     C**********           GOTO STRT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         CABEQ     '1'           STRT                     25
     C**********
     C**********           ENDSL
      **** END   CHG **** 10/31/00 *********************************************
     C*
     C*   Validate the Date Range Entered
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0
     C     SRERR         CABEQ     '1'           STRT                     20
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT            7 0
     C     SRERR         CABEQ     '1'           STRT                     24
     C*
     C*   The dates must be in ascending order.
     C*
     C     XXTDAT        IFLT      XXFDAT
     C*
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCTODT IFLT SCFRDT
     C*** END ADD *** 05/09/00 **************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         XXFND                          ADD 05/09/00
      **** BEGIN ADD **** 10/31/00 *********************************************
     C                   MOVE      SCFRDT        QQFDAT
     C                   MOVE      SCTODT        QQTDAT
      **** END   ADD **** 10/31/00 *********************************************
     C                   CALL      'W7267C'
     C                   PARM                    QQCO#
     C                   PARM                    UPOUTQ                         MLS 1/29/88
     C                   PARM                    XXFND             3            ADD 05/09/00
      **** BEGIN CHG **** 10/31/00 *********************************************
     C******************** PARM           XXFDAT           ADD 05/09/00
     C******************** PARM           XXTDAT           ADD 05/09/00
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQTDAT            8
      **** END   CHG **** 10/31/00 *********************************************
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
