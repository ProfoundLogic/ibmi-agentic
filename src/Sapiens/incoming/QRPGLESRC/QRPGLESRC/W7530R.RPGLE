     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7530R - PROMPT FOR CUMULATIVE LOSS RATIO REPORT
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM IS A COPY FROM PCA VERSION OF CUMULATIVE RATIO   *
     H* REPORT WITH THE THE EXCEPTION OF EMPLOYER UNDERWRITER.        *
     H* THIS PROGRAM PROMPTS THE USER FOR A RUN DATE, POLICY YEAR     *
     H* RANGE, SORT SELECTION, ACTIVE MEMBER (Y/N) AND RANGES FOR     *
     H* EMPLOYER, COUNTY, CHAPTER, AND REGION.                        *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   CLAIM REPORT MENU                                           *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   W7530C                                                      *
     H*                                                               *
     H* PROGRAMMER:   C:60   09/17/96        PROJECT: 48WDV           *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*  09/22/06 C: 58 PROJECT: 71WHI
     H*              Add SIC code sort and SIC code selection range.
     H*    10.09     Correct screen captions
     H*                                                               *
     H*  05/14/01 C:136 PROJECT: WC577                                *
     H*                 Display location of output file.              *
     H*                                                               *
     H* 04/01/01 C:61  PROJECT 81WSC                                  *
     H*                Add option for Detail, Summary, Both output.   *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7530F IN SCREEN.     *
     H*                                                               *
     H*  01/07/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                CHANGED FIELD SCDTE TO DEFAULT TO CURRENT DATE,*
     H*                INSTEAD OF USER PARM DATE.                     *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   10  HELP REQUEST                                            *
     H*   20  VALIDATE RUN DATE                                       *
     H*   21  VALIDATE FROM POLICY YEAR                               *
     H*   22  VALIDATE TO POLICY YEAR                                 *
     H*   23  SORT MUST BE E,C,R,H OR A                               *
     H*   24  INVALID RANGE ENTERED                                   *
     H*   25  CURSOR MUST BE LOCATED IN CHAPTER RANGE TO SEARCH ON    *
     H*       CHAPTER                                                 *
     H*   26  CURSOR MUST BE LOCATED IN REGION RANGE TO SEARCH ON     *
     H*       REGION                                                  *
     H*   27  ACTIVE MEMBERS FLAG MUST BE 'Y' OR 'N'                  *
     H*   28  POLICY YEARS MUST BE ASCENDING ORDER                    *
     H*   29  CURSOR MUST BE LOCATED IN AGENT # RANGE TO SEARCH ON    *
     H*       AGENT                                                   *
     H*   30  POSITION CURSOR ON FROM EMPLOYER                        *
     H*   31  POSITION CURSOR ON TO EMPLOYER                          *
     H*   32  POSITION CURSOR ON FROM COUNTY                          *
     H*   33  POSITION CURSOR ON TO COUNTY                            *
     H*   34  POSITION CURSOR ON FROM CHAPTER                         *
     H*   35  POSITION CURSOR ON TO CHAPTER                           *
     H*   36  POSITION CURSOR ON FROM REGION                          *
     H*   37  POSITION CURSOR ON TO REGION                            *
     H*   40  INVALID EMPLOYER RANGE                                  *
     H*   41  INVALID COUNTY RANGE                                    *
     H*   42  INVALID CHAPTER RANGE                                   *
     H*   43  INVALID REGION RANGE                                    *
     H*   44  ONLY ONE SET OF RANGE VALUES MAY BE ENTERED             *
     H*   45  INVALID AGENT RANGE                                     *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*                                                               *
     H*****************************************************************
     FW7530F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
     FWMAAP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D***  BEGIN CHG  ***  09/22/06  *********************************
     D******************                      1 100 DATA
     D  DATA                   1    112
     D***  END   CHG  ***  09/22/06  *********************************
     D  SCFFYR                 1      3  0
     D  SCTFYR                 4      6  0
     D  SCFEMP                 7     15  0
     D  SCTEMP                16     24  0
     D  SCFDIV                25     29  0
     D  SCTDIV                30     34  0
     D  SCFCNT                35     38
     D  SCTCNT                39     42
     D  SCFCHP                43     45  0
     D  SCTCHP                46     48  0
     D  SCFRGN                49     51  0
     D  SCTRGN                52     54  0
     D  SCFAGT                55     59  0
     D  SCTAGT                60     64  0
     D  SCFLOS                65     71  2
     D  SCTLOS                72     78  2
     D  SCFPRM                79     89  2
     D  SCTPRM                90    100  2
     D***  BEGIN ADD  ***  09/22/06  *********************************
     D  SCFSIC               101    106
     D  SCTSIC               107    112

     I***  END   ADD  ***  09/22/06  *********************************

     C*
     C* KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C* Key List for the Fund Detail File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXYR
     C*
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C*
     C* Key List for the Generic Table File
     C     GTKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XZTBID            4
     C                   KFLD                    XXKEY            10
     C***  END   ADD  ***  09/22/06  *********************************
     C*
     C* ENTRY PARAMETERS
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
     C                   Z-ADD     *ZEROS        XXYR              3 0
     C                   Z-ADD     *ZEROS        XXRGN#            3 0
     C                   Z-ADD     *ZEROS        XXCHAP            3 0
     C                   Z-ADD     *ZEROS        XXCHP             3 0
     C                   Z-ADD     *ZEROS        XXEMP#            9 0
     C                   Z-ADD     *ZEROS        XXDIV             5 0
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*
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
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C*
     C*** BEGIN CHG *** 01/07/98 **************************************
     C**********           Z-ADDUPPRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCDTE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C*** END   CHG *** 01/07/98 **************************************
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C* Set-up and Initialize
     C*
     C*** BEGIN DEL *** 01/07/98 **************************************
     C**********           TIME           DSDTTM
     C*** END   DEL *** 01/07/98 **************************************
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   Z-ADD     0             SCFRYR
     C                   Z-ADD     0             SCTOYR
     C                   MOVE      *BLANKS       SCSORT
     C                   MOVE      'N'           SCACT
     C                   Z-ADD     0             SCFEMP
     C                   Z-ADD     0             SCTEMP
     C                   Z-ADD     0             SCFDIV
     C                   Z-ADD     0             SCTDIV
     C                   MOVE      *BLANKS       SCFCNT
     C                   MOVE      *BLANKS       SCTCNT
     C                   Z-ADD     0             SCFCHP
     C                   Z-ADD     0             SCTCHP
     C                   Z-ADD     0             SCFRGN
     C                   Z-ADD     0             SCTRGN
     C                   Z-ADD     0             SCFFYR
     C                   Z-ADD     0             SCTFYR
     C                   Z-ADD     0             SCFAGT
     C                   Z-ADD     0             SCTAGT
     C                   Z-ADD     0             SCFLOS
     C                   Z-ADD     0             SCTLOS
     C                   Z-ADD     0             SCFPRM
     C                   Z-ADD     0             SCTPRM
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C                   MOVE      *BLANKS       SCFSIC
     C                   MOVE      *BLANKS       SCTSIC
     C***  END   ADD  ***  09/22/06  *********************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL           10
     C                   CAT(P)    '.':0         XXRPTL
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      '0'
     C*
     C                   MOVEL     SRDTO         SCDTE
     C*
     C                   ENDIF
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
     C*** BEGIN ADD *** 01/07/98 **************************************
     C*
     C*  F10 - SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 - SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCFEMP'
     C     HFLD          OREQ      'SCFDIV'
     C     HFLD          OREQ      'SCTEMP'
     C     HFLD          OREQ      'SCTDIV'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C*
     C     HFLD          IFEQ      'SCFEMP'
     C     HFLD          OREQ      'SCFDIV'
     C                   MOVE      EMEMP#        SCFEMP
     C                   MOVE      EMDIV         SCFDIV
     C                   MOVE      *ON           *IN31                          SCTEMP POSITION
     C                   ENDIF                                                  HFLD=SCFEMP
     C*
     C     HFLD          IFEQ      'SCTEMP'
     C     HFLD          OREQ      'SCTDIV'
     C                   MOVE      EMEMP#        SCTEMP
     C                   MOVE      EMDIV         SCTDIV
     C                   MOVE      *ON           *IN32                          SCFCNT POSITION
     C                   ENDIF                                                  HFLD=SCTEMP
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C*
     C     HFLD          COMP      'SCFEMP'                               30    SCFEMP POSITION
     C     HFLD          COMP      'SCTEMP'                               31    SCTEMP POSITION
     C*
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCFEMP
     C*
     C*
     C*  F10 - SEARCH - COUNTY CODE
     C*
     C     HFLD          IFEQ      'SCFCNT'
     C     HFLD          OREQ      'SCTCNT'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C                   MOVE      *BLANKS       XXCNT            10
     C*
     C                   CALL      'W2215R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    XXCNT            10
     C                   PARM                    @@ST              2
     C                   PARM      2             @@VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     XXCNT         ANDNE     *BLANKS
     C*
     C     HFLD          IFEQ      'SCFCNT'
     C                   MOVEL(P)  XXCNT         SCFCNT
     C                   MOVE      *ON           *IN33                          SCTCNT POSITION
     C                   ENDIF                                                  HFLD=SCFCNT
     C*
     C     HFLD          IFEQ      'SCTCNT'
     C                   MOVEL(P)  XXCNT         SCTCNT
     C                   MOVE      *ON           *IN34                          SCFCHP POSITION
     C                   ENDIF                                                  HFLD=SCTCNT
     C*
     C                   ELSE
     C*
     C     HFLD          COMP      'SCFCNT'                               32    SCFCNT POSITION
     C     HFLD          COMP      'SCTCNT'                               33    SCTCNT POSITION
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCFCNT
     C*
     C*
     C*  F10 - SEARCH - CHAPTER CODE
     C*
     C     HFLD          IFEQ      'SCFCHP'
     C     HFLD          OREQ      'SCTCHP'
     C*
     C                   Z-ADD     *ZEROS        XXCHP
     C*
     C                   CALL      'W0720R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    XXCHP
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     XXCHP         ANDNE     *ZEROS
     C*
     C     HFLD          IFEQ      'SCFCHP'
     C                   Z-ADD     XXCHP         SCFCHP
     C                   MOVE      *ON           *IN35                          SCTCHP POSITION
     C                   ENDIF                                                  HFLD=SCFCHP
     C*
     C     HFLD          IFEQ      'SCTCHP'
     C                   Z-ADD     XXCHP         SCTCHP
     C                   MOVE      *ON           *IN36                          SCFRGN POSITION
     C                   ENDIF                                                  HFLD=SCFCHP
     C*
     C                   ELSE
     C*
     C     HFLD          COMP      'SCFCHP'                               34    SCFCHP POSITION
     C     HFLD          COMP      'SCTCHP'                               35    SCTCHP POSITION
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCFCHP
     C*
     C*
     C*  F10 - SEARCH - REGION CODE
     C*
     C     HFLD          IFEQ      'SCFRGN'
     C     HFLD          OREQ      'SCTRGN'
     C*
     C                   Z-ADD     *ZEROS        XXRGN#
     C*
     C                   CALL      'W0520R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    XXRGN#
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     XXRGN#        ANDNE     *ZEROS
     C*
     C     HFLD          IFEQ      'SCFRGN'
     C                   Z-ADD     XXRGN#        SCFRGN
     C                   MOVE      *ON           *IN37                          SCTRGN POSITION
     C                   ENDIF                                                  HFLD=SCFRGN
     C*
     C     HFLD          IFEQ      'SCTRGN'
     C                   Z-ADD     XXRGN#        SCTRGN
     C                   MOVE      *ON           *IN38                          SCFAGT POSITION
     C                   ENDIF                                                  HFLD=SCTRGN
     C*
     C                   ELSE
     C*
     C     HFLD          COMP      'SCFRGN'                               36    SCFRGN POSITION
     C     HFLD          COMP      'SCTRGN'                               37    SCTRGN POSITION
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCFRGN
     C*
     C*
     C*  F10 - SEARCH - AGENT CODE
     C*
     C     HFLD          IFEQ      'SCFAGT'
     C     HFLD          OREQ      'SCTAGT'
     C*
     C                   MOVE      *ZEROS        ##RRN
     C*
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     ##RRN         ANDNE     *ZEROS
     C*
     C     ##RRN         CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     HFLD          IFEQ      'SCFAGT'
     C                   Z-ADD     AAAGT#        SCFAGT
     C                   MOVE      *ON           *IN39                          SCTAGT POSITION
     C                   ENDIF                                                  HFLD=SCFAGT
     C*
     C     HFLD          IFEQ      'SCTAGT'
     C                   Z-ADD     AAAGT#        SCTAGT
     C                   MOVE      *ON           *IN47                          SCFLOS POSITION
     C                   ENDIF                                                  HFLD=SCTAGT
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C*
     C     HFLD          COMP      'SCFAGT'                               38    SCFAGT POSITION
     C     HFLD          COMP      'SCTAGT'                               39    SCTAGT POSITION
     C*
     C                   ENDIF                                                  *INKL=ON,##RRN
     C*
     C                   ENDIF                                                  HFLD=SCFAGT
     C*
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C*
     C*  F10 - SEARCH - SIC CODE
     C*
     C     HFLD          IFEQ      'SCFSIC'
     C     HFLD          OREQ      'SCTSIC'
     C                   MOVEL(P)  'SIC'         XZTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA           80
     C                   PARM                    @@MODE            1
     C                   PARM                    XZTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     XXKEY         ANDNE     *BLANKS
     C*
     C     XXKEY         IFNE      *BLANKS
     C     HFLD          IFEQ      'SCFSIC'
     C                   MOVEL(P)  XXKEY         SCFSIC
     C                   MOVE      *ON           *IN51                          SCFSIC POSITION
     C                   ENDIF                                                  HFLD=SCFSIC
     C*
     C     HFLD          IFEQ      'SCTSIC'
     C                   MOVEL(P)  XXKEY         SCTSIC
     C                   MOVE      *ON           *IN52                          SCTSIC POSITION
     C                   ENDIF                                                  HFLD=SCTSIC
     C                   ENDIF                                                  XXKEY<>BLANKS
     C*
     C                   ENDIF                                                  HFLD=SCFSIC
     C                   ENDIF                                                  *INKL EQ *OFF
     C*
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *ZEROS
     C*
     C     HFLD          COMP      'SCFSIC'                               51    SCFSIC POSITION
     C     HFLD          COMP      'SCTSIC'                               52    SCTSIC POSITION
     C*
     C                   ENDIF                                                  *INKL=ON,##RRN
     C*
     C***  END   ADD  ***  09/22/06  *********************************
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   ENDIF                                                  INKJ=ON
     C*
     C*** END   ADD *** 01/07/98 **************************************
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate the from and to policy years
     C*
     C                   Z-ADD     SCFRYR        XXYR
     C     F1KEY         CHAIN     PF101                              21
     C     *IN21         IFEQ      '1'
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCFRYR        SCFFYR
     C                   END
     C*
     C                   Z-ADD     SCTOYR        XXYR
     C     F1KEY         CHAIN     PF101                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCTOYR        SCTFYR
     C                   END
     C*
     C     SCFRYR        COMP      SCTOYR                             28
     C     *IN28         CABEQ     '1'           STRT
     C*
     C*   SORT MUST BE EQUAL TO 'E', 'C', 'R', OR 'H'
     C*      OR 'A'
     C*
     C     SCSORT        IFNE      'E'
     C     SCSORT        ANDNE     'R'
     C     SCSORT        ANDNE     'C'
     C     SCSORT        ANDNE     'H'
     C     SCSORT        ANDNE     'A'
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C     SCSORT        ANDNE     'S'
     C***  END   ADD  ***  09/22/06  *********************************
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      ***** BEG ADD ******* 04/01/01 **********************************
      *
      * Report type must be entered if sort is not 'E'
      *
     C     SCSORT        IFNE      'E'
      *
     C     SCDSB         IFNE      'D'
     C     SCDSB         ANDNE     'S'
     C     SCDSB         ANDNE     'B'
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ******* 04/01/01 **********************************
     C*
     C* ACTIVE MEMBERS ONLY (Y/N)
     C*
     C     SCACT         IFNE      'Y'
     C     SCACT         ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   EMPLOYER RANGE MUST BE SELECTED IF RUNNING BY EMPLR
     C*
     C     SCSORT        IFEQ      'E'
     C     SCFEMP        ANDEQ     0
     C     SCTEMP        ANDEQ     0
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFEMP        COMP      SCTEMP                             40
     C     *IN40         CABEQ     '1'           STRT
     C*
     C     SCFDIV        COMP      SCTDIV                             40
     C     *IN40         CABEQ     '1'           STRT
     C*
     C*   COUNTY RANGE MUST BE SELECTED IF RUNNING BY COUNTY
     C*
     C     SCSORT        IFEQ      'C'
     C     SCFCNT        ANDEQ     *BLANKS
     C     SCFCNT        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFCNT        COMP      SCTCNT                             41
     C     *IN41         CABEQ     '1'           STRT
     C*
     C*   CHAPTER RANGE MUST BE SELECTED IF RUNNING BY CHAPTER
     C*
     C     SCSORT        IFEQ      'H'
     C     SCFCHP        ANDEQ     0
     C     SCTCHP        ANDEQ     0
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFCHP        COMP      SCTCHP                             42
     C     *IN42         CABEQ     '1'           STRT
     C*
     C*   REGION RANGE MUST BE SELECTED IF RUNNING BY REGION
     C*
     C     SCSORT        IFEQ      'R'
     C     SCFRGN        ANDEQ     0
     C     SCTRGN        ANDEQ     0
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFRGN        COMP      SCTRGN                             43
     C     *IN43         CABEQ     '1'           STRT
     C*
     C*   AGENT RANGE MUST BE SELECTED IF RUNNING BY AGENT
     C*
     C     SCSORT        IFEQ      'A'
     C     SCFAGT        ANDEQ     0
     C     SCTAGT        ANDEQ     0
     C                   MOVE      '1'           *IN45
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFAGT        COMP      SCTAGT                             45
     C     *IN45         CABEQ     '1'           STRT
     C*
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C*
     C*   SIC RANGE MUST BE SELECTED IF RUNNING BY SIC
     C*
     C     SCSORT        IFEQ      'S'
     C     SCFSIC        ANDEQ     *BLANKS
     C     SCTSIC        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN50
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFSIC        COMP      SCTSIC                             50
     C     *IN50         CABEQ     '1'           STRT
     C*
     C***  END   ADD  ***  09/22/06  *********************************
     C*
     C*  ONLY ONE SET OF RANGE VALUES MAY BE ENTERED
     C*
     C     SCSORT        IFEQ      'E'
     C     SCFCNT        IFNE      *BLANKS
     C     SCTCNT        ORNE      *BLANKS
     C     SCFCHP        ORNE      *ZEROS
     C     SCTCHP        ORNE      *ZEROS
     C     SCFRGN        ORNE      *ZEROS
     C     SCTRGN        ORNE      *ZEROS
     C     SCFAGT        ORNE      *ZEROS
     C     SCTAGT        ORNE      *ZEROS
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C     SCFSIC        ORNE      *BLANKS
     C     SCTSIC        ORNE      *BLANKS
     C***  END   ADD  ***  09/22/06  *********************************
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCSORT        IFEQ      'C'
     C     SCFEMP        IFNE      *ZEROS
     C     SCTEMP        ORNE      *ZEROS
     C     SCFDIV        ORNE      *ZEROS
     C     SCTDIV        ORNE      *ZEROS
     C     SCFCHP        ORNE      *ZEROS
     C     SCTCHP        ORNE      *ZEROS
     C     SCFRGN        ORNE      *ZEROS
     C     SCTRGN        ORNE      *ZEROS
     C     SCFAGT        ORNE      *ZEROS
     C     SCTAGT        ORNE      *ZEROS
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C     SCFSIC        ORNE      *BLANKS
     C     SCTSIC        ORNE      *BLANKS
     C***  END   ADD  ***  09/22/06  *********************************
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCSORT        IFEQ      'H'
     C     SCFEMP        IFNE      *ZEROS
     C     SCTEMP        ORNE      *ZEROS
     C     SCFDIV        ORNE      *ZEROS
     C     SCTDIV        ORNE      *ZEROS
     C     SCFCNT        ORNE      *BLANKS
     C     SCTCNT        ORNE      *BLANKS
     C     SCFRGN        ORNE      *ZEROS
     C     SCTRGN        ORNE      *ZEROS
     C     SCFAGT        ORNE      *ZEROS
     C     SCTAGT        ORNE      *ZEROS
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C     SCFSIC        ORNE      *BLANKS
     C     SCTSIC        ORNE      *BLANKS
     C***  END   ADD  ***  09/22/06  *********************************
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCSORT        IFEQ      'R'
     C     SCFEMP        IFNE      *ZEROS
     C     SCTEMP        ORNE      *ZEROS
     C     SCFDIV        ORNE      *ZEROS
     C     SCTDIV        ORNE      *ZEROS
     C     SCFCNT        ORNE      *BLANKS
     C     SCTCNT        ORNE      *BLANKS
     C     SCFCHP        ORNE      *ZEROS
     C     SCTCHP        ORNE      *ZEROS
     C     SCFAGT        ORNE      *ZEROS
     C     SCTAGT        ORNE      *ZEROS
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C     SCFSIC        ORNE      *BLANKS
     C     SCTSIC        ORNE      *BLANKS
     C***  END   ADD  ***  09/22/06  *********************************
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*
     C     SCSORT        IFEQ      'A'
     C     SCFEMP        IFNE      *ZEROS
     C     SCTEMP        ORNE      *ZEROS
     C     SCFDIV        ORNE      *ZEROS
     C     SCTDIV        ORNE      *ZEROS
     C     SCFCNT        ORNE      *BLANKS
     C     SCTCNT        ORNE      *BLANKS
     C     SCFCHP        ORNE      *ZEROS
     C     SCTCHP        ORNE      *ZEROS
     C     SCFRGN        ORNE      *ZEROS
     C     SCTRGN        ORNE      *ZEROS
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C     SCFSIC        ORNE      *BLANKS
     C     SCTSIC        ORNE      *BLANKS
     C***  END   ADD  ***  09/22/06  *********************************
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCSORT        IFEQ      'U'
     C     SCFEMP        IFNE      *ZEROS
     C     SCTEMP        ORNE      *ZEROS
     C     SCFDIV        ORNE      *ZEROS
     C     SCTDIV        ORNE      *ZEROS
     C     SCFCNT        ORNE      *BLANKS
     C     SCTCNT        ORNE      *BLANKS
     C     SCFCHP        ORNE      *ZEROS
     C     SCTCHP        ORNE      *ZEROS
     C     SCFRGN        ORNE      *ZEROS
     C     SCTRGN        ORNE      *ZEROS
     C     SCFAGT        ORNE      *ZEROS
     C     SCTAGT        ORNE      *ZEROS
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C     SCFSIC        ORNE      *BLANKS
     C     SCTSIC        ORNE      *BLANKS
     C***  END   ADD  ***  09/22/06  *********************************
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C***  BEGIN ADD  ***  09/22/06  *********************************
     C*
     C     SCSORT        IFEQ      'S'
     C     SCFEMP        IFNE      *ZEROS
     C     SCTEMP        ORNE      *ZEROS
     C     SCFDIV        ORNE      *ZEROS
     C     SCTDIV        ORNE      *ZEROS
     C     SCFCNT        ORNE      *BLANKS
     C     SCTCNT        ORNE      *BLANKS
     C     SCFCHP        ORNE      *ZEROS
     C     SCTCHP        ORNE      *ZEROS
     C     SCFRGN        ORNE      *ZEROS
     C     SCTRGN        ORNE      *ZEROS
     C     SCFAGT        ORNE      *ZEROS
     C     SCTAGT        ORNE      *ZEROS
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C                   END
      *
     C*  THE SIC CODE RANGE MUST BE IN ASCENDING ORDER.
     C*
     C     SCFSIC        IFGT      SCTSIC
     C                   MOVE      '1'           *IN50
     C                   GOTO      STRT
     C                   END
     C*
     C***  END   ADD  ***  09/22/06  *********************************
     C*
     C*  THE LOSS RATIO RANGE MUST BE IN ASCENDING ORDER.
     C*
     C     SCFLOS        IFGT      SCTLOS
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE PREMIUM RANGE MUST BE IN ASCENDING ORDER.
     C*
     C     SCFPRM        IFGT      SCTPRM
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE SECOND ENTRY OF THE LOSS RATIO RANGE IS ZERO, LOAD
     C*  IT WITH THE HIGHEST VALUE.
     C*
     C     SCTLOS        IFEQ      0
     C                   MOVE      *HIVAL        SCTLOS
     C                   END
     C*
     C*  IF THE SECOND ENTRY OF THE PREMIUM RANGE IS ZERO, LOAD
     C*  IT WITH THE HIGHEST VALUE.
     C*
     C     SCTPRM        IFEQ      0
     C                   MOVE      *HIVAL        SCTPRM
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCDTE         QQDTE             8
     C                   MOVE      SCACT         QQACT             1
     C                   MOVE      SCSORT        QQSORT            1
      ***** BEG ADD ******* 04/01/01 **********************************
      *
     C     QQSORT        IFEQ      'E'
     C                   MOVE      'D'           SCDSB
     C                   ENDIF                                                  QQSORT
      ***** END ADD ******* 04/01/01 **********************************
      *
     C                   CALL      'W7530C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQDTE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C                   PARM                    DATA
     C                   PARM                    QQSORT
     C                   PARM                    QQACT
      ***** BEG ADD ******* 04/01/01 **********************************
     C                   PARM                    SCDSB             1
      ***** END ADD ******* 04/01/01 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
