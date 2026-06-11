     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0285R - USER / REPORT MAINTENANCE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                               *
     H*  08/16/99 C:118 PROJECT: WC577
     H*                 MADE V3 SCREEN FIXES.
     H*
     H*  09/30/94 C:15 PROJECT: 59WBZ                                 *
     H*                ADDED 'FREQUENCY CODE' TO SCR02.               *
     H*  10/17/94 C:22 CHANGED WTABL TO WTFQL                         *
     H*                                                               *
     H*  09/04/96 C:22 PROJECT: SUPPORT                               *
     H*                ENSURE UPDATE MODE IS DISPLAYED CORRECTLY      *
     H*****************************************************************
     FW0285F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************
     FWDUSL2    UF A E           K DISK
     FWMRDL     IF   E           K DISK
     FWMUDL     IF   E           K DISK
     F*** BEGIN ADD *** 09/30/94 **************************************
     FWTFQL     IF   E           K DISK
     F*WTABL   IF  E           K        DISK
     F*** END ADD ***** 09/30/94 **************************************
     F************************************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D MS              S             40    DIM(1) CTDATA PERRCD(1)
     D************************************************************
     D SRK             S              1    DIM(10)
     D************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D************************************************************
     D************************************************************
     D VALID           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789#$@ ')
     D VAL2            C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ#$@')

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
     C*  USER / REPORT DETAIL FILE
     C*
     C     USKY1         KLIST
     C                   KFLD                    SCUSID
     C                   KFLD                    SCRPKY
     C*
     C*  USER DESTINATION FILE
     C*
     C     UDKEY         KLIST
     C                   KFLD                    SCUSID
     C*
     C*  REPORT DEFINITION FILE
     C*
     C     RDKY1         KLIST
     C                   KFLD                    SCRPKY
     C*
     C*** BEGIN ADD *** 09/30/94 **************************************
     C*  KEY LIST FOR THE TABLE FILES
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*** END ADD ***** 09/30/94 **************************************
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
     C*** BEGIN ADD *** 09/30/94 **************************************
     C     *LIKE         DEFINE    FQKEY         XXKEY
     C*** END ADD ***** 09/30/94 **************************************
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
     C                   MOVE      *BLANK        SCRPKY
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
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
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        QQKY1            20
     C                   MOVEL     SCUSID        QQKY1
     C                   CALL      'W0286R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKY1
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKY1         XXUSER
     C                   MOVEL     QQKY1         SCUSID
     C                   MOVE      QQKY1         SCRPKY
     C*** BEGIN ADD 09/04/96 ***************************************
     C                   MOVE      UA(2)         SCMODE
     C*** END ADD 09/04/96 *****************************************
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKY1         OREQ      *BLANK
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVEL     UPUSNM        SCPRFD
     C     USKY1         CHAIN     PUS01                              60
     C                   MOVE      USDEST        SCDEST
     C                   MOVE      USSTAT        SCSTAT
     C*** BEGIN ADD *** 09/30/94 **************************************
     C                   MOVE      USFREQ        SCFREQ
     C*** END ADD ***** 09/30/94 **************************************
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     USKY1         CHAIN     PUS01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PUS01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      USDEST        SCDEST
     C                   MOVE      USKEY         SCRPKY
     C*** BEGIN ADD *** 09/30/94 **************************************
     C                   MOVE      USFREQ        SCFREQ
     C*** END ADD ***** 09/30/94 **************************************
     C*
     C                   END
     C*
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANK        QQKY
     C                   MOVEL     SCRPKY        QQKY              8
     C                   CALL      'W0281R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKY
     C                   PARM                    QQRTC             1
     C                   MOVE      *BLANK        SCRPKY
     C                   MOVEL     QQKY          SCRPKY
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKY          OREQ      *BLANK
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C                   END
     C*
     C                   END
     C*
     C*   COMMAND 06 = SEARCH LIST CALL
     C*
     C     *INKF         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCUSID        QQKEY            10
     C                   CALL      'W0287R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKEY         SCUSID
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   END
     C*
     C*   COMMAND 08 = ADD A DESTINATION ENTRY
     C*
     C     *INKH         IFEQ      '1'
     C                   MOVE      *BLANK        QQKEY            10
     C                   MOVEL     SCUSID        QQKEY
     C                   CALL      'W0288R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR02T
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 09/30/94 **************************************
     C*   TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C********** SCFREQ    IFEQ '?'
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        XXKEY
     C***** BEGIN ADD **** 08/16/99 ***********************************
     C                   MOVEL     SCFREQ        XXKEY
     C***** END   ADD **** 08/16/99 ***********************************
     C                   CALL      'W2431R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCFREQ
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C     QQRTC         CABEQ     'B'           STRT                     25
     C     XXKEY         CABEQ     *BLANK        STRT                     25
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           END
     C***** END   DEL **** 08/16/99 ***********************************
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*** END ADD ***** 09/30/94 **************************************
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
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
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE USER ID CANNOT BE EQUAL TO *BLANKS
     C*
     C     SCUSID        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C**********************************************************************
     C*** INVALID CHARACTERS WERE KEYED!
     C*
     C     VALID         CHECK     SCUSID                                 20
     C     *IN20         CABEQ     '1'           STRT
     C***
      * FIRST CHARACTER MUST BE VALID FOR AN OBJECT NAME *********
     C                   MOVEL     SCUSID        XX1A              1
     C     VAL2          CHECK     XX1A                                   20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C**** NO EMBEDDED BLANKS ARE ALLOWED !!!
     C                   MOVEA     SCUSID        SRK
     C                   Z-ADD     0             XX                2 0
     C*
     C*
     C     ' '           SCAN      SCUSID        XX                       22
     C     *IN22         IFEQ      '1'
     C                   MOVEA     *BLANKS       SRK(XX)
     C                   MOVEA     SRK           SCUSID
     C                   MOVE      '0'           *IN22
     C     SCUSID        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   ENDIF
     C* *******************************************************************
     C*
     C                   MOVE      SCUSID        XXUSER
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     MS(1)         SCPRFD
     C                   ELSE
     C                   MOVEL     UPUSNM        SCPRFD
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCDEST
     C                   MOVE      *BLANK        SCRPKY
     C                   MOVE      'A'           SCSTAT
     C*** BEGIN ADD *** 09/30/94 **************************************
     C                   MOVE      *BLANK        SCFREQ
     C*** END ADD ***** 09/30/94 **************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE REPORT KEY CANNOT BE BLANK
     C*
     C     SCRPKY        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE DESTINATION CANNOT BE BLANK
     C*
     C     SCDEST        IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE REPORT KEY MUST BE FOR A VALID REPORT DEFINITION
     C*
     C     RDKY1         CHAIN     PRD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  FOR DESTINATIONS 2 OR 3 THERE MUST BE ENTRY IN WMUDP
     C*
     C     SCDEST        IFEQ      '2'
     C     SCDEST        OREQ      '3'
     C     UDKEY         CHAIN     PUD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** BEGIN ADD *** 09/30/94 **************************************
     C*  VALIDATE THE FREQUENCY CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCFREQ        XXKEY
     C     TBKY          CHAIN     PFQ01                              60
     C     *IN60         IFEQ      '1'
     C     FQSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*** END ADD ***** 09/30/94 **************************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C     USKY1         CHAIN     PUS01                              40
     C                   MOVE      XXCO#         USCO#
     C                   MOVE      *BLANK        USKEY
     C                   MOVEL     SCRPKY        USKEY
     C                   MOVE      SCDEST        USDEST
     C                   MOVE      SCUSID        USURNM
     C                   MOVE      SCSTAT        USSTAT
     C                   MOVE      Q1USER        USUSER
     C                   MOVE      Q1PGM         USPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         USDT
     C                   Z-ADD     DSTIME        USTIME
     C*** BEGIN ADD *** 09/30/94 **************************************
     C                   MOVE      SCFREQ        USFREQ
     C*** END ADD ***** 09/30/94 **************************************
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PUS01
     C                   ELSE
     C                   WRITE     PUS01
     C                   END
     C*
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCRPKY
     C                   GOTO      STRT
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPUS01     E            PUS01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE MS
USER IS NOT DEFINED TO THE SYSTEM.
