     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4600R - REPORT ORDERING SUBMIT PROMPT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                               *
     H*  08/17/99  C:118  PROJECT: WC577                              *
     H*                   Made V3 screen chgs.                        *
     H*                                                               *
     H*****************************************************************
     FW4600F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************
     FWTFQL     IF   E           K DISK
     F************************************************************

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I************************************************************

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER           10
     C*
     C*  KEY LIST FOR THE TABLE FILES
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C**************************************************************
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
     C                   MOVE      Q1USER        XXUSER
     C     *LIKE         DEFINE    FQKEY         XXKEY
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#             3 0
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
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
     C     QQCALL        COMP      '1'                                    09
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
     C                   GOTO      TERM
     C                   END
     C*
     C*   TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C***** BEGIN CHG **** 08/17/99 ***********************************
     C********** SCFREQ    IFEQ '?'
     C     Q2RECF        IFEQ      'SCR01'
     C***** END   CHG **** 08/17/99 ***********************************
     C***** BEGIN ADD **** 08/17/99 ***********************************
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFRDT'
     C                   MOVE      *ON           *IN25
     C*
     C     HFLD          WHENEQ    'SCTODT'
     C                   MOVE      *ON           *IN26
     C*
     C     HFLD          WHENEQ    'SCRNDT'
     C                   MOVE      *ON           *IN27
     C*
     C     HFLD          WHENEQ    'SCFREQ'
     C***** END   ADD **** 08/17/99 ***********************************
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2431R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCFREQ
     C***** BEGIN DEL **** 08/17/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/17/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C     QQRTC         CABEQ     'B'           STRT                     22
     C     XXKEY         CABEQ     *BLANK        STRT                     22
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C***** BEGIN ADD **** 08/17/99 ***********************************
     C                   ENDSL
     C***** END   ADD **** 08/17/99 ***********************************
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
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
     C***** BEGIN DEL **** 08/17/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/17/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE FROM DATE CANNOT BE INVALID
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE TO DATE CANNOT BE INVALID
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE RUN DATE CANNOT BE INVALID
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE FREQUENCY CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCFREQ        XXKEY
     C     TBKY          CHAIN     PFQ01                              60
     C     *IN60         IFEQ      '1'
     C     FQSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* ALL DATE EDITS PASSED
     C*
     C                   MOVE      101           QQCO#
     C                   MOVE      DSDATE        QQDT
     C                   MOVE      SCFRDT        QQFRDT
     C                   MOVE      SCTODT        QQTODT
     C                   MOVE      SCRNDT        QQRNDT
     C                   MOVE      SCFREQ        QQFREQ
     C*
     C                   CALL      'W4600C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDT              8
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQFREQ            1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
