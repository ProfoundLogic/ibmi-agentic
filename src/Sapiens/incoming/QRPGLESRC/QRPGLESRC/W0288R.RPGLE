     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0288R - USER DESTINATION MAINTENANCE
     H*
     H*****************************************************************
     H*  REVISIONS
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*
     H*  08/16/99  C:118  PROJECT: WC577
     H*                   MAKE V3 DISPLAY FILE FIXES
     H*
     H*****************************************************************
     FW0288F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************
     FWMUDL     UF A E           K DISK
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
     C*  USER DESTINATION  FILE
     C*
     C     UDKY1         KLIST
     C                   KFLD                    SCUSID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTCD            1
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
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
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
     C                   MOVE      QQPRDS        SCPRDS
     C***** BEGIN ADD **** 08/16/99 ***********************************
     C                   MOVE      Q1USER        SCUSER
     C***** END   ADD **** 08/16/99 ***********************************
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCUSID
     C                   END
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
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCUSER        QQKEY            10
     C                   CALL      'W0289R'
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
     C*
     C                   END
     C*
     C*   COMMAND 06 = SEARCH LIST CALL
     C*
     C     *INKF         IFEQ      '1'
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
     C*
     C                   END
     C*
     C***** BEGIN CHG **** 08/16/99 ***********************************
     C********** COMMAND 08 = EMPLOYER SEARCH
     C**********
     C********** *INKH     IFEQ '1'
     C*
     C*   COMMAND 10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C***** END   CHG **** 08/16/99 ***********************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'W0284R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   PARM                    QQNAME           40
     C                   PARM                    QQMAD1           40
     C                   PARM                    QQMAD2           40
     C                   PARM                    QQMCTY           25
     C                   PARM                    QQMST             2
     C                   PARM                    QQMZIP            9
     C                   PARM                    QQCONT           40
     C                   PARM                    QQEFND            3
     C                   PARM                    QQEMP#            5
     C                   PARM                    QQDIV             3
     C                   MOVEL     QQNAME        SCCOMP
     C                   MOVEL     QQMAD1        SCADR1
     C                   MOVEL     QQMAD2        SCADR2
     C                   MOVEL     QQMCTY        SCCTY
     C                   MOVEL     QQMST         SCST
     C                   MOVEL     QQMZIP        SCZIP1
     C                   MOVE      QQMZIP        SCZIP2
     C                   MOVEL     QQCONT        SCATTN
     C                   MOVEL     QQCONT        SCRECP
     C                   MOVE      QQEFND        SCEFND
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
     C***** BEGIN ADD **** 08/16/99 ***********************************
     C                   MOVE      *ON           *IN30
     C***** END   ADD **** 08/16/99 ***********************************
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
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
     C                   MOVE      *ZERO         UPCO#
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
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
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
     C*  THE USER ID CANNOT BE BLANK
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
     C*  GET USER PROFILE INFORMATION
     C*
     C                   MOVE      SCUSID        XXUSER
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     MS(1)         SCPRNM
     C                   ELSE
     C                   MOVEL     UPUSNM        SCPRNM
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     UDKY1         CHAIN     PUD01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PUD01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      UDCOMP        SCCOMP
     C                   MOVE      UDADR1        SCADR1
     C                   MOVE      UDADR2        SCADR2
     C                   MOVE      UDCTY         SCCTY
     C                   MOVE      UDST          SCST
     C                   MOVE      UDZIP1        SCZIP1
     C                   MOVE      UDZIP2        SCZIP2
     C                   MOVE      UDATTN        SCATTN
     C                   MOVE      UDRECP        SCRECP
     C                   MOVE      UDFCVT        SCFCVT
     C                   MOVE      UDFCT1        SCFCT1
     C                   MOVE      UDFCT2        SCFCT2
     C                   MOVE      UDFCT3        SCFCT3
     C                   MOVE      UDFCF1        SCFCF1
     C                   MOVE      UDFCF2        SCFCF2
     C                   MOVE      UDFCF3        SCFCF3
     C                   MOVE      UDFCCM        SCFCCM
     C                   MOVE      UDFPH#        SCFPH#
     C                   MOVE      UDFCLS        SCFCLS
     C                   MOVE      UDSTAT        SCSTAT
     C                   MOVE      UDEFND        SCEFND
     C                   MOVE      UDEMP#        SCEMP#
     C                   MOVE      UDDIV         SCDIV
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCCOMP
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *BLANKS       SCZIP1
     C                   MOVE      *BLANKS       SCZIP2
     C                   MOVE      *BLANKS       SCATTN
     C                   MOVE      *BLANKS       SCRECP
     C                   MOVE      *BLANKS       SCFCVT
     C                   MOVE      *BLANKS       SCFCT1
     C                   MOVE      *BLANKS       SCFCT2
     C                   MOVE      *BLANKS       SCFCT3
     C                   MOVE      *BLANKS       SCFCF1
     C                   MOVE      *BLANKS       SCFCF2
     C                   MOVE      *BLANKS       SCFCF3
     C                   MOVE      *BLANKS       SCFCCM
     C                   MOVE      *BLANKS       SCFPH#
     C                   MOVE      *BLANKS       SCFCLS
     C                   MOVE      *ZEROS        SCEFND
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C     UDKY1         CHAIN     PUD01                              40
     C                   MOVE      XXCO#         UDCO#
     C                   MOVE      SCUSID        UDUSID
     C                   MOVE      SCCOMP        UDCOMP
     C                   MOVE      SCADR1        UDADR1
     C                   MOVE      SCADR2        UDADR2
     C                   MOVE      SCCTY         UDCTY
     C                   MOVE      SCST          UDST
     C                   MOVE      SCZIP1        UDZIP1
     C                   MOVE      SCZIP2        UDZIP2
     C                   MOVE      SCATTN        UDATTN
     C                   MOVE      SCRECP        UDRECP
     C                   MOVE      SCFCVT        UDFCVT
     C                   MOVE      SCFCT1        UDFCT1
     C                   MOVE      SCFCT2        UDFCT2
     C                   MOVE      SCFCT3        UDFCT3
     C                   MOVE      SCFCF1        UDFCF1
     C                   MOVE      SCFCF2        UDFCF2
     C                   MOVE      SCFCF3        UDFCF3
     C                   MOVE      SCFCCM        UDFCCM
     C                   MOVE      SCFPH#        UDFPH#
     C                   MOVE      SCFCLS        UDFCLS
     C                   MOVE      SCEFND        UDEFND
     C                   MOVE      SCEMP#        UDEMP#
     C                   MOVE      SCDIV         UDDIV
     C                   MOVE      SCSTAT        UDSTAT
     C                   MOVE      Q1USER        UDUSER
     C                   MOVE      Q1PGM         UDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UDDT
     C                   Z-ADD     DSTIME        UDTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PUD01
     C                   ELSE
     C                   WRITE     PUD01
     C                   END
     C*
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCUSID
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      '1'           *INLR
     OPUD01     E            PUD01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE MS
USER IS NOT DEFINED TO THE SYSTEM.
