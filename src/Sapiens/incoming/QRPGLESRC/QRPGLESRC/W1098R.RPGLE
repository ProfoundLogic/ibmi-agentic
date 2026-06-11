     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W1098R - MANUAL ENTRY MAINTENANCE FOR 1099
     H*****************************************************************
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  REWRITE: 12/01/98   C:33   PROJECT: WC871                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  09/11/15 C:295 PROJECT: WC2362                               *
     H*                 1099 Process include Manual Entries           *
     H*                                                               *
     H*  10/16/03 C:132 PROJECT: WC1157                               *
     H*                 Add two new fileds to WATNPB-TNPMTD, TMPMTE   *
     H*           NOTE: THERE WERE NO CHANGES TO CODE, RECOMPILE ONLY *
     H*                                                               *
     H*  09/05/02 C:132 SUPPORT: WC1095                               *
     H*                 ADD AGENT MOD TO THE FIRST SCREEN & ADD TO    *
     H*                 THE KLSTS IN PROGRAM                          *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768                          *
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     FW1098F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWATNPBL   UF A E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAGP     IF   E             DISK
     F                                     RENAME(PAG01:PAG02)
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*
      /COPY SCOPYMT01D
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('1099 Manual Entry Ma-
     D                                     intenance')
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C*  1099 'B' RECORD WORK FILE
     C*
     C     TNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    XXYR
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    QQPRDS        XXPRDS
     C     *LIKE         DEFINE    QQCALL        XXCALL
     C     *LIKE         DEFINE    TNPTYR        XXYR
     C     *LIKE         DEFINE    TNFEIN        XXFEIN
     C     *LIKE         DEFINE    TNMOD         XXMOD
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*_______________________________________________________________
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*_______________________________________________________________
     C*
     C                   CLEAR                   XXCALL
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      QQCALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C     Q1PRMS        IFGE      1
     C     QQPRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  QQPRDS        XXPRDS
     C                   ELSE
     C                   MOVEL(P)  OPTNM         XXPRDS
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   MOVEL(P)  XXPRDS        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   TIME                    DSDTTM
     C*
     C     XXCALL        COMP      '1'                                    09
     C*_______________________________________________________________
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETERS RECORD
     C*
     C                   CALL      COMAPI        COMPL                  99
     C*
     C                   MOVEL     ##CONM        SCCONM
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL(P)  ##FNAM        SCFNAM
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*_______________________________________________________________
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C     Q2RECF        WHENEQ    'SCR02'
     C                   EXFMT     SCR02
     C                   ENDSL
     C*
     C*  PROCESS ALL F KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*  F3=EXIT
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C                   CALL      'W0040R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      1             ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM                     LR
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C*
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PAG02                              99
     C*
     C                   MOVE      AGFEIN        SCFEIN
     C                   MOVE      AGMOD         SCMOD
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ=*ON
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*  F13=CHANGE USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'                             99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   ENDIF                                                  *INKM=*ON
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *IN10=*ON
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*_______________________________________________________________
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*_______________________________________________________________
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  YEAR CANNOT BE LESS THEN OR EQUAL TO 0
     C*
     C     SCYR          CABLE     *ZERO         STRT                   2020
     C*
     C*  FEDERAL ID CANNOT BE *ZERO OR 999999999
     C*
     C     SCFEIN        IFEQ      *ZERO
     C     SCFEIN        OREQ      999999999
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                  0=SCFEIN=999999999
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCYR          XXYR
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C*
     C     TNKY          CHAIN(N)  PTN02                              40
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C     *IN09         IFEQ      *OFF
     C                   MOVE      MODEC         SCMODE
     C                   ELSE
     C                   MOVE      MODED         SCMODE
     C                   ENDIF                                                  *IN09=*OFF
     C*
     C                   MOVEL(P)  TNPNM1        SCNAME
     C                   MOVEL(P)  TNPRAD        SCADR
     C                   MOVEL(P)  TNPCTY        SCCTY
     C                   MOVE      TNPST         SCST
     C                   MOVEL     TNPZIP        SCZP1
     C                   MOVE      TNPZIP        SCZP2
     C                   MOVE      TNPMT3        SCAMT3
     C                   MOVE      TNPMT7        SCAMT7
     C****** Beg Add ***** 09/11/15*************************************
      /Free
        scCombine3 = TNcombine3;
      /End-Free
     C****** End Add ***** 09/11/15*************************************
     C*
     C                   ELSE
     C*
     C     *IN09         CABEQ     *ON           STRT                     22
     C*
     C*  IF ADD MODE, CHECK AGENT MASTER
     C*
     C                   CLEAR                   PAG01
     C*
     C     AGKY          CHAIN     PAG01                              60
     C*
     C                   MOVEL(P)  AGNAME        SCNAME
     C                   MOVEL(P)  AGMAD1        SCADR
     C                   MOVEL(P)  AGMCTY        SCCTY
     C                   MOVE      AGMST         SCST
     C                   MOVEL     AGMZIP        SCZP1
     C                   MOVE      AGMZIP        SCZP2
     C*
     C                   MOVE      MODEA         SCMODE
     C                   MOVE      *BLANKS       SCAMT3
     C                   MOVE      *BLANKS       SCAMT7
     C****** Beg Add ***** 09/11/15*************************************
      /Free
        Clear scCombine3;
      /End-Free
     C****** End Add ***** 09/11/15*************************************
     C*
     C                   ENDIF                                                  *IN40=*OFF
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*  PROCESS SCREEN 2 - ADDRESS INFORMATION
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         IFEQ      *OFF
     C*
     C*  THE NAME CANNOT BE BLANK
     C*
     C     SCNAME        CABEQ     *BLANK        STRT                     20
     C*
     C*  THE ADDRESS LINE CANNOT BE BLANK
     C*
     C     SCADR         CABEQ     *BLANK        STRT                     21
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
     C     SCCTY         CABEQ     *BLANK        STRT                     22
     C*
     C*  THE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCST          XXKEY
     C*
     C     STKY          CHAIN     PST01                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     23
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C     SCZP1         CABLE     *ZERO                                2424
     C*
     C*  AMOUNT CANNOT BE LESS THEN 0
     C*
     C     SCAMT3        CABLT     *ZERO         STRT                   27
     C*
     C****** Beg Add ***** 09/11/15*************************************
     C*****SCAMT7        CABLT     *ZERO         STRT                   29
     C                   If        scCombine3 <> 'Y'
     C     SCAMT7        CABLT     *ZERO         STRT                   29
     C                   Endif
     C****** End Add ***** 09/11/15*************************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   CLEAR                   PTN02
     C*
     C     TNKY          CHAIN     PTN02                              40
     C*
     C                   MOVE      ##CO#         TNCO#
     C                   MOVE      ##FND         TNFND
     C                   MOVE      SCYR          TNPTYR
     C                   MOVE      'B'           TNRTYP
     C                   MOVE      SCFEIN        TNFEIN
     C                   MOVE      SCMOD         TNMOD
     C                   MOVE      SCNAME        TNPNM1
     C                   MOVE      SCADR         TNPRAD
     C                   MOVEL     SCCTY         TNPCTY
     C                   MOVE      SCST          TNPST
     C                   MOVEL     SCZP1         TNPZIP
     C*
     C     SCZP2         IFNE      *ZEROS
     C                   MOVE      SCZP2         TNPZIP
     C                   ENDIF                                                  SCZP2<>*ZEROS
     C*
     C                   Z-ADD     SCAMT3        TNPMT3
     C                   Z-ADD     *ZEROS        TNPMT6
     C                   Z-ADD     SCAMT7        TNPMT7
     C****** Beg Add ***** 09/11/15*************************************
      /Free
        TNcombine3 = scCombine3;
      /End-Free
     C****** End Add ***** 09/11/15*************************************
     C*
     C     *IN40         IFEQ      *OFF
     C                   UPDATE    PTN02
     C                   ELSE
     C                   WRITE     PTN02
     C                   ENDIF                                                  *IN40=*OFF
     C*
     C                   ENDIF                                                  *IN09=*OFF
     C*
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCMOD
     C                   MOVE      *ON           *IN25
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C                   GOTO      STRT
     C*_______________________________________________________________
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
