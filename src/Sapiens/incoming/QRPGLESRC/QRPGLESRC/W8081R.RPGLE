     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W8081R - COMPENSATION PAYMENT CONTROL MAINTENANCE
     H*****************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO CHANGE THE      *
     H*               PAYMENT CONTROL FILE.                           *
     H* CALLED FROM:  MENU                                            *
     H* CALLS TO:     NONE                                            *
     H* PROGRAMMER:   C:15   09/30/93        PROJECT: WC258           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                              *
     H*  10/11/06  C:33  Seq#: WA099/41801                           *
     H*                  Use SC305R instead of SC580R.               *
     H*                                                               *
     H*  02/12/03 C: 53 PROJECT: WC1112                               *
     H*                 Add maximum # of checks field
     H*                                                               *
     H*  09/24/02 C:15  Project: WC577                                *
     H*                 Added code to process F12 correctly.          *
     H*                                                               *
     H*  02/19/99 C:112 PROJECT: 25WJW                                *
     H*           Add code to protect fields in Display only mode     *
     H*                                                               *
     H*  03/10/98 C:85 PROJECT: WC577                                 *
     H*                ADDED ERROR CHECKING TO SEARCHES FOR SCREEN 1. *
     H*                ERROR IND. IS SET ON IF NO SEARCH IS ALLOWED.  *
     H*                                                               *
     H*  01/22/98 C:85 PROJECT: WC577                                 *
     H*                DELETED FORMER SCR01, AND CHANGED FORMER       *
     H*                SCR02 TO SCR01. CHANGED PROGRAM TO BE CALLED   *
     H*                FROM THE WORK WITH GROUPS SCREEN, (W0010R)     *
     H*                OPTION 24.                                     *
     H*                                                               *
     H*  01/16/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                CHANGED SEARCHES.                              *
     H*                                                               *
     H*  03/01/94 C:15 PROJECT: WC298                                 *
     H*                ADDED A FIELD WHICH ALLOWS THE USER TO SPECIFY *
     H*                THE # OF DAYS BEFORE END OF PERIOD IN WHICH    *
     H*                CHECK WILL BE CUT.                             *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*   *IN40   -   RECORD NOT FOUND FOR FILE WMPCP                 *
     H*   *IN60   -   GENERAL USAGE                                   *
     H*                                                               *
     H***SCR01                                                        *
     H*** *IN20   -   INVALID FUND                                    *
     H***SCR02                                                        *
     H*                                                               *
     H*  SCR01                                                        *
     H*   *IN20   -   FIELD MUST BE 'Y' OR 'N'                        *
     H*   *IN21   -   INVALID REMARK                                  *
     H*   *IN22   -   FIELD CANNOT BE LESS THAN OR EQUAL TO ZERO      *
     H*   *IN23   -   INVALID REMARK                                  *
     H*****************************************************************
     FW8081F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** BEGIN CHG *** 01/22/98 **************************************
     F**********WMUPL   IF  E           K        DISK
     F**********WMFNL   IF  E           K        DISK
     FWMFNL     IF   E             DISK
     F*** END   CHG *** 01/22/98 **************************************
     FWMPCL     UF A E           K DISK
     FSTCTL1    IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*** BEGIN ADD *** 01/22/98 ***********************************************
     D OPTNM           C                   CONST('Compensation Payment-
     D                                      Control Maintenance')

     I*** END   ADD *** 01/22/98 ***********************************************
     I*

     C*
     C*   USER PARAMETER FILE KEY LIST
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   COMPANY FILE KEY LIST
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   FUND FILE KEY LIST
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   COMP PAYMENT CONTROL FILE KEY LIST
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C*
     C*** BEGIN CHG *** 01/22/98 **************************************
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C*
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*
     C*** END   CHG *** 01/22/98 **************************************
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
     C     *LIKE         DEFINE    PCCO#         XXCO#
     C     *LIKE         DEFINE    PCFND         XXFND
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     @@CALL        IFEQ      '1'
     C                   SETON                                        09        DISPLAY MODE
     C                   ENDIF
     C*** END ADD ***** 02/19/99 **************************************
     C*
     C*** BEGIN CHG *** 01/22/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*
     C*** END   CHG *** 01/22/98 **************************************
     C*
     C*** BEGIN DEL *** 01/22/98 **************************************
     C**********
     C***GET*THE USER PARAMETER FILE
     C**********
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C     TOP           TAG
     C*
     C*** BEGIN DEL *** 01/22/98 **************************************
     C********** UPKY      CHAINPUP01                60
     C**N60*****           Z-ADDUPCO#     XXCO#
     C**N60*****           Z-ADDUPFND     SCFND
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C*** BEGIN ADD *** 01/22/98 **************************************
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C                   MOVE      FNCO#         XXCO#
     C                   MOVE      FNFND         XXFND
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   Z-ADD     0             @@RRN
     C*
     C*   LOAD THE PAYMENT CONTROL RECORD
     C*   (MOVED FROM DELETED PROCESS REQUEST SCREEN 1)
     C*
     C     PCKY          CHAIN     PPC01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PPC01E
     C                   MOVE      PCARFL        SCARFL
     C                   MOVE      PCRCEP        SCRCEP
     C                   Z-ADD     PCDIDY        SCDIDY
     C                   MOVE      PCRCLP        SCRCLP
     C                   Z-ADD     PCEDTD        SCEDTD
     C                   Z-ADD     PCPRTD        SCPRTD
     C                   Z-ADD     PCMAX#        SCMAX#                         ADD 02/12/03
     C                   ELSE
     C                   MOVE      'N'           SCARFL
     C                   MOVE      *BLANKS       SCRCEP
     C                   Z-ADD     0             SCDIDY
     C                   MOVE      *BLANKS       SCRCLP
     C                   Z-ADD     0             SCEDTD
     C                   Z-ADD     0             SCPRTD
     C                   Z-ADD     0             SCMAX#                         ADD 02/12/03
     C                   END
     C*
     C*** END   ADD *** 01/22/98 **************************************
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
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
     C*** BEGIN DEL *** 01/22/98 **************************************
     C********** Q2RECF    IFEQ 'SCR02'
     C**********           EXFMTSCR02
     C**********           END
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C*** Begin add *** 09/24/02 **************************************
     C     *INKL         OREQ      '1'
     C*** End add ***** 09/24/02 **************************************
     C                   GOTO      TERM
     C                   END
     C*** BEGIN DEL *** 01/22/98 **************************************
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C********** *INKL     IFEQ '1'
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C*** BEGIN DEL *** 01/16/98 **************************************
     C*
     C****COMMAND 05 = FUND SEARCH
     C**********
     C********** *INKE     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'W0010R'
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM '2'       @@CALL  1
     C**********           PARM           @@CO#   3
     C**********           PARM           @@FND   3
     C**********           PARM           @@FYR   3
     C**********           PARM 1         @@VIEW  10
     C**********
     C**********           MOVE @@FND     SCFND
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** *INKC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C**********           GOTO STRT
     C**********           ELSE
     C**********           GOTO SCR01T
     C**********
     C**********           END
     C**********           END
     C**********           END
     C**********
     C***COMMAND 10 - TABLE FILE SEARCH - DARTS REMARK FOR EACH PMT
     C**********
     C********** *INKJ     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR02'
     C**********
     C********** SCRCEP    IFEQ '?'
     C**********           MOVE *BLANKS   SCRCEP
     C**********           CALL 'SC580R'
     C**********           PARM           SCRCEP
     C**********           PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C**********           MOVEL'SCR02'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCRCEP    OREQ *BLANKS
     C**********           MOVE '1'       *IN24
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN25
     C**********           END
     C**********
     C**********           END
     C**********
     C***COMMAND 10 - TABLE FILE SEARCH - DARTS REMARK FOR LAST PMT
     C**********
     C********** SCRCLP    IFEQ '?'
     C**********           MOVE *BLANKS   SCRCLP
     C**********           CALL 'SC580R'
     C**********           PARM           SCRCLP
     C**********           PARM           QQRTCD
     C********** QQRTCD    CABEQ'A'       TERM
     C**********           MOVEL'SCR02'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCRCLP    OREQ *BLANKS
     C**********           MOVE '1'       *IN26
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE '1'       *IN27
     C**********           END
     C**********
     C**********           END
     C**********
     C**********           GOTO STRT
     C**********           END
     C**********           END
     C*
     C*** END   DEL *** 01/16/98 **************************************
     C*
     C*** BEGIN ADD *** 01/16/98 **************************************
     C*** BEGIN DEL *** 01/22/98 **************************************
     C*
     C*  BEGIN SEARCHES
     C*
     C********** *INKJ     IFEQ *ON
     C*
     C********** Q2RECF    IFEQ 'SCR01'
     C*
     C*  FUND NUMBER SEARCH
     C*
     C********** HFLD      IFEQ 'SCFND'
     C*
     C**********           MOVE *BLANKS   @@FND   3
     C*
     C**********           CALL 'W0010R'
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM '2'       @@CALL  1
     C**********           PARM           @@CO#   3
     C**********           PARM           @@FND   3
     C**********           PARM           @@FYR   3
     C**********           PARM 1         @@VIEW  10
     C*
     C********** *INKC     CABEQ*ON       TERM
     C*
     C********** *INKL     IFEQ *OFF
     C********** @@FND     ANDNE*BLANKS
     C**********           MOVE @@FND     SCFND
     C**********           GOTO SCR01T
     C**********           ENDIF                           *INKL=OFF
     C*
     C**********           ENDIF                           HFLD=SCFND
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C*
     C**********           ENDIF                           Q2RECF=SCR01
     C*
     C*  DARTS REMARK CODE SEARCH
     C*
     C********** Q2RECF    IFEQ 'SCR02'
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C*  BEGIN SEARCHES
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*** BEGIN ADD *** 03/10/98 **************************************
     C*
     C**      ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
     C     HFLD          COMP      'SCARFL'                               31
     C     HFLD          COMP      'SCDIDY'                               32
     C     HFLD          COMP      'SCEDTD'                               33
     C     HFLD          COMP      'SCPRTD'                               34
     C     HFLD          COMP      'SCMAX#'                               36    ADD 02/12/03
     C*
     C*** END   ADD *** 03/10/98 **************************************
     C*
     C     HFLD          IFEQ      'SCRCEP'
     C     HFLD          OREQ      'SCRCLP'
     C*
     C                   MOVE      *BLANKS       QQRMCD            5
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM           QQRMCD
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C********** *INKL     IFEQ *OFF
     C********** QQRMCD    ANDNE*BLANKS
     C     QQRMCD        IFNE      *BLANKS
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C     HFLD          IFEQ      'SCRCEP'
     C                   MOVEL     QQRMCD        SCRCEP
     C                   MOVE      *ON           *IN25
     C                   ENDIF                                                  HFLD=SCRCEP
     C*
     C     HFLD          IFEQ      'SCRCLP'
     C                   MOVEL     QQRMCD        SCRCLP
     C                   MOVE      *ON           *IN27
     C                   ENDIF                                                  HFLD=SCRCLP
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     QQRMCD        OREQ      *BLANKS
     C*
     C     HFLD          COMP      'SCRCEP'                               24    SCRCEP POSITION
     C     HFLD          COMP      'SCRCLP'                               26    SCRCLP POSITION
     C*
     C                   ENDIF                                                  *INKL=ON
     C*
     C                   ENDIF                                                  HFLD=SCRCEP
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C*** END   ADD *** 01/16/98 **************************************
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN DEL *** 01/22/98 **************************************
     C*
     C****PROCESS CHANGE OF USER PARAMETERS
     C**********
     C********** *INKM     IFEQ '1'
     C**********           CALL 'W0006R'
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO TOP
     C**********           END
     C**********
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C                   END                                                    *IN11 EQ '1'
     C*
     C*** BEGIN DEL *** 01/22/98 **************************************
     C**********
     C****GOTO THE APPROPRIATE SCREEN
     C**********
     C********** Q2RECF    CABEQ'SCR01'   SCR01T
     C********** Q2RECF    CABEQ'SCR02'   SCR02T
     C**********************************
     C****PROCESS REQUEST SCREEN 1
     C**********************************
     C********** SCR01T    TAG
     C**********
     C****VALIDATE THE FUND
     C**********
     C**********           Z-ADDSCFND     XXFND
     C********** FNKY      CHAINPFN01                20
     C********** *IN20     CABEQ'1'       STRT
     C**********           MOVELFNNAME    SCNAME
     C**********
     C****LOAD THE PAYMENT CONTROL RECORD AND GOTO NEXT SCREEN
     C**********
     C********** PCKY      CHAINPPC01                40
     C********** *IN40     IFEQ '0'
     C**********           EXCPTPPC01E
     C**********           MOVE PCARFL    SCARFL
     C**********           MOVE PCRCEP    SCRCEP
     C**********           Z-ADDPCDIDY    SCDIDY
     C**********           MOVE PCRCLP    SCRCLP
     C**********           Z-ADDPCEDTD    SCEDTD
     C**********           Z-ADDPCPRTD    SCPRTD
     C**********           ELSE
     C**********           MOVE 'N'       SCARFL
     C**********           MOVE *BLANKS   SCRCEP
     C**********           Z-ADD0         SCDIDY
     C**********           MOVE *BLANKS   SCRCLP
     C**********           Z-ADD0         SCEDTD
     C**********           Z-ADD0         SCPRTD
     C**********           END
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
     C**********
     C**********************************
     C****PROCESS REQUEST SCREEN 2
     C**********************************
     C********** SCR02T    TAG
     C*
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C     SCR01T        TAG
     C*
     C*  THE AUTO RELEASE FLAG MUST BE Y OR N
     C*
     C     SCARFL        IFNE      'Y'
     C     SCARFL        ANDNE     'N'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE DARTS REMARK FOR EACH PAYMENT
     C*
     C     SCRCEP        IFNE      *BLANKS
     C     SCRCEP        CHAIN     PCT01                              21
     C  N21CTFL01        COMP      'Y'                                2828
     C     *IN21         IFEQ      '1'
     C     *IN28         OREQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  IF EITHER DARTS REMARK FIELD WAS ENTERED, THE '# OF DAYS IN
     C*  ADVANCE TO DIARY' MUST BE GREATER THAN ZERO.
     C*
     C     SCRCEP        IFNE      *BLANKS
     C     SCRCLP        ORNE      *BLANKS
     C     SCDIDY        IFLE      0
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE THE DARTS REMARK FOR THE LAST PAYMENT
     C*
     C     SCRCLP        IFNE      *BLANKS
     C     SCRCLP        CHAIN     PCT01                              23
     C  N23CTFL01        COMP      'Y'                                2929
     C     *IN23         IFEQ      '1'
     C     *IN29         OREQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  THE # OF DAYS BEFORE THE ENDING PERIOD IN WHICH THE CHECK IS
     C*  CUT CANNOT BE LESS THAN ZERO.
     C*
     C     SCPRTD        CABLT     *ZEROS        STRT                   30
     C*** BEG ADD *** 02/12/03 ***************************
     C*
     C*  MAX # OF CHECKS CANNOT BE LESS THAN ZERO.
     C*
     C     SCMAX#        CABLT     *ZEROS        STRT                   35      ADD 02/12/03
     C*** END ADD *** 02/12/03 ***************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED, LOAD FILE
     C*
     C     PCKY          CHAIN     PPC01                              40
     C                   Z-ADD     XXCO#         PCCO#
     C                   Z-ADD     XXFND         PCFND
     C                   MOVE      SCARFL        PCARFL
     C                   MOVE      SCRCEP        PCRCEP
     C                   Z-ADD     SCDIDY        PCDIDY
     C                   MOVE      SCRCLP        PCRCLP
     C                   Z-ADD     SCEDTD        PCEDTD
     C                   MOVE      Q1USER        PCUSER
     C                   MOVE      Q1PGM         PCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C*
     C                   Z-ADD     SCPRTD        PCPRTD
     C                   Z-ADD     SCMAX#        PCMAX#                         ADD 02/12/03
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPC01
     C                   ELSE
     C                   WRITE     PPC01
     C                   END
     C*
     C*** BEGIN DEL *** 01/22/98 **************************************
     C**********           Z-ADDUPFND     SCFND
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C*** END   DEL *** 01/22/98 **************************************
     C*
     C*
     C*  END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 01/22/98 **************************************
     C                   RETURN
     C*** END   ADD *** 01/22/98 **************************************
     OPPC01     E            PPC01E
