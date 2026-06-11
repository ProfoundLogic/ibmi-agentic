     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7240R - PROMPT FOR RECOVERY REPORT
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  11/04/05 C:132 PROJECT: WC129 - 48736                        *
     H*                Delete Code added on 08/30/05, so builds full  *
     H*                work file, only print pgm to check for the     *
     H*                Employer Range if Sort = "N".                  *
     H*                                                               *
     H*  08/30/05 C:132 PROJECT: WC129 - 48736                        *
     H*                If Sort = 'N', and Employer Range entered,     *
     H*                Add edit for Employer Range.                   *
     H*                                                               *
     H*  10/19/98 C:78 PROJECT: WC577                                 *
     H*                IF SORT = 'E', 'A', OR 'H' THEN RANGE IS       *
     H*                REQUIRED. ALREADY DONE FOR SORT = 'C'.         *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7240F IN SCREEN.     *
     H*                                                               *
     H*  05/15/98  C:90  PROJ:WC577                                   *
     H*                  CHANGED SCREEN TO MEET VERSION 3 STANDARDS   *
     H*                  COMBINED ALL SEARCHES INTO F10 SEARCH        *
     H*                                                               *
     H*   JJJ REVISED 3-2-89 SELECT ON EMP#; COUNTY; CHAPTER; OR
     H*                      AGT#. ALLOW RANGE SELECTION FOR EACH
     H*
     H*****************************************************************
     FW7240F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMEMP     IF   E             DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      **** BEGIN ADD **** 05/15/98 ************************************
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PFRCDF)
      **** END   ADD **** 05/15/98 ************************************
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
     D*** BEGIN CHG *** 06/26/97 **************************************
     D**********  DS
     D**********                              1  66 DATA
     D**********                              1   1 SCSORT
     D**********                              2  100SCFEMP
     D**********                             11  190SCTEMP
     D**********                             20  240SCFDIV
     D**********                             25  290SCTDIV
     D**********                             30  31 SCFCNT
     D**********                             32  33 SCTCNT
     D**********                             34  360SCFCHP
     D**********                             37  390SCTCHP
     D**********                             40  440SCFAGT
     D**********                             45  490SCTAGT
     D**********                             50  51 XXSTAT
     D**********                             52  54 XXFEAD
     D**********                             55  630XXATTY
     D**********                             64  660XXMOD
     D                 DS
     D  DATA                   1     70
     D  SCSORT                 1      1
     D  SCFEMP                 2     10  0
     D  SCTEMP                11     19  0
     D  SCFDIV                20     24  0
     D  SCTDIV                25     29  0
     D  SCFCNT                30     33
     D  SCTCNT                34     37
     D  SCFCHP                38     40  0
     D  SCTCHP                41     43  0
     D  SCFAGT                44     48  0
     D  SCTAGT                49     53  0
     D  XXSTAT                54     55
     D  XXFEAD                56     58
     D  XXATTY                59     67  0
     D  XXMOD                 68     70  0

     I*** END   CHG *** 06/26/97 **************************************

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
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C     *LIKE         DEFINE    SCTEMP        XXEMP#
     C     *LIKE         DEFINE    SCTDIV        XXDIV
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
     C                   MOVE      UPFND         SCFND
     C*
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *BLANK        SCSORT                         JJJ 3-2-89
     C                   Z-ADD     0             SCFEMP                         |
     C                   Z-ADD     0             SCTEMP                         |
     C                   Z-ADD     0             SCFAGT                         |
     C                   Z-ADD     0             SCTAGT                         |
     C                   MOVE      *BLANKS       SCFCNT                         |
     C                   MOVE      *BLANKS       SCTCNT                         |
     C                   Z-ADD     0             SCFCHP                         |
     C                   Z-ADD     0             SCTCHP                         |
     C                   Z-ADD     0             SCFDIV                         |
     C                   Z-ADD     0             SCTDIV                         |
     C*                                                    *JJJ 5-29-90
     C                   MOVE      *BLANKS       XXSTAT                         *
     C                   MOVE      *BLANKS       XXFEAD                         *
     C                   Z-ADD     0             XXATTY                         *
     C                   Z-ADD     0             XXMOD                          *
     C*                                                    |
     C* Position cursor indicators                         |
     C*                                                    |
     C                   MOVEA     '0000'        *IN(30)                        |
     C                   MOVEA     '0000'        *IN(34)                        |
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
      **** BEGIN CHG **** 05/15/98 ************************************
     C*                                                    JJJ 3-2-89
     C*********** COMMAND 05 = EMPLOYER SEARCH LIST CALL             |
     C*                                                    |
     C*********************************
     C*       F10 = SEARCH            *
     C*********************************
      *
     C     *INKJ         IFEQ      '1'
      *
     C*********************************
     C*    EMPLOYER SEARCH            *
     C*********************************
      *
     C     HFLD          IFEQ      'SCFEMP'
     C     HFLD          OREQ      'SCFDIV'
     C     HFLD          OREQ      'SCTEMP'
     C     HFLD          OREQ      'SCTDIV'
      *
     C********** *INKE     IFEQ '1'                        |
     C******************** Z-ADD0         XXEMP#           |
     C******************** Z-ADD0         XXDIV            |
     C*                                                    |
     C******************** Z-ADD0         LIN     20       |
     C******************** Z-ADD0         COL     20       ¦
     C********** Q2CPOS    DIV  256       LIN              ¦
     C******************** MVR            COL     20       ¦
     C*                                                    ¦
     C********** To search employer cursor must be in an emplyr # field
     C*                                                    ¦
     C********** LIN       IFNE 16                         ¦
     C******************** MOVE '1'       *IN24            ¦
     C******************** GOTO STRT                       ¦
     C******************** END                             ¦
     C*                                                    ¦
      **** END   CHG **** 05/15/98 ************************************
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
      **** BEGIN CHG **** 05/15/98 ************************************
     C******************** PARM           @@RRN   90
     C                   PARM      *ZEROS        @@RRN             9 0
      **** END   CHG **** 05/15/98 ************************************
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM                           |
     C*                                                    |
     C     *INKL         IFEQ      *ON                                          |
     C     @@RRN         OREQ      *ZEROS                                       |
      **** BEGIN CHG **** 05/15/98 ************************************
     C     HFLD          CABEQ     'SCFEMP'      STRT                     30    PC SCFAGT
     C     HFLD          CABEQ     'SCFDIV'      STRT                     39    PC SCTAGT
     C     HFLD          CABEQ     'SCTEMP'      STRT                     31    PC SCFAGT
     C     HFLD          CABEQ     'SCFDIV'      STRT                     40    PC SCTAGT
     C                   ENDIF
     C******************** WRITESCR99                      |
     C******************** MOVE *BLANK    Q2RECF           |
     C******************** MOVEL'SCR01'   Q2RECF           |
     C******************** MOVE '1'       *IN30            |
     C******************** GOTO STRT                       |
     C******************** ELSE
      **** BEGIN CHG **** 05/15/98 ************************************
     C*                                                    |
     C     @@RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C*
     C                   WRITE     SCR99                                        |
     C                   MOVE      *BLANK        Q2RECF                         |
     C                   MOVEL     'SCR01'       Q2RECF                         |
      **** BEGIN CHG **** 05/15/98 ************************************
     C********** COL       IFGE 37                         |
     C********** COL       ANDLE45                         |
     C     HFLD          IFEQ      'SCFEMP'
     C     HFLD          OREQ      'SCFDIV'
      **** END   CHG **** 05/15/98 ************************************
     C                   Z-ADD     XXEMP#        SCFEMP                         |
     C                   Z-ADD     XXDIV         SCFDIV                         |
     C                   MOVE      '0'           *IN30                          |
     C                   MOVE      '1'           *IN31                          |
     C                   ELSE                                                   |
      **** BEGIN DEL **** 05/15/98 ************************************
     C********** COL       IFGE 55                         |
     C********** COL       ANDLE63                         |
      **** END   DEL **** 05/15/98 ************************************
     C                   Z-ADD     XXEMP#        SCTEMP                         |
     C                   Z-ADD     XXDIV         SCTDIV                         |
     C                   MOVEA     '00'          *IN(30)                        |
     C                   MOVE      '1'           *IN32                          |
      **** BEGIN DEL **** 05/15/98 ************************************
     C******************** END                             |
     C******************** END                             |
      **** END   DEL **** 05/15/98 ************************************
     C                   GOTO      STRT                                         |
     C                   END                                                    IFEQ *INKL
     C*                                                    |
      **** BEGIN CHG **** 05/15/98 ************************************
     C                   END                                                    HFLD SCFEMP
     C*                                                    |
     C*                                                    |
     C********** COMMAND 09 = CHAPTER SEARCH LIST CALL              |
     C*                                                    |
     C***********************************
     C*       CHAPTER SEARCH            *
     C***********************************
      *
     C     HFLD          IFEQ      'SCFCHP'
     C     HFLD          OREQ      'SCTCHP'
      *
     C********** *INKI     IFEQ '1'                        |
     C                   Z-ADD     0             XXCHP             3 0          |
     C******************** Z-ADD0         LIN              |
     C******************** Z-ADD0         COL              |
     C*                                                    |
     C********** Q2CPOS    DIV  256       LIN              |
     C******************** MVR            COL              |
     C*                                                    |
     C************ To search chapter cursor must be in a chapter field|
     C*                                                    |
     C********** LIN       IFNE 18                         |
     C******************** MOVE '1'       *IN25            |
     C******************** GOTO STRT                       |
     C******************** END                             |
      **** END   CHG **** 05/15/98 ************************************
     C*                                                    |
     C                   CALL      'W0720R'                                     |
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
      **** BEGIN CHG **** 05/15/98 ************************************
     C******************** PARM           @@RRN   90
     C                   PARM      *ZEROS        @@RRN             9 0
      **** END   CHG **** 05/15/98 ************************************
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    XXCHP
     C*
     C     *INKC         CABEQ     *ON           TERM                           |
     C*                                                    |
     C     *INKL         IFEQ      *ON                                          |
     C     XXCHP         OREQ      *ZEROS                                       |
      **** BEGIN CHG **** 05/15/98 ************************************
     C     HFLD          CABEQ     'SCFCHP'      STRT                     34
     C     HFLD          CABEQ     'SCTCHP'      STRT                     35
     C                   ENDIF
     C******************** WRITESCR99                      |
     C******************** MOVE *BLANK    Q2RECF           |
     C******************** MOVEL'SCR01'   Q2RECF           |
     C******************** MOVEA'0000'    *IN,30           |
     C******************** MOVE '1'       *IN34            |
     C******************** GOTO STRT                       |
     C*
     C******************** ELSE
      **** BEGIN CHG **** 05/15/98 ************************************
     C*                                                    |
     C                   WRITE     SCR99                                        |
     C                   MOVE      *BLANK        Q2RECF                         |
     C                   MOVEL     'SCR01'       Q2RECF                         |
      **** BEGIN CHG **** 05/15/98 ************************************
     C********** COL       IFGE 37                         |
     C********** COL       ANDLE39                         |
     C     HFLD          IFEQ      'SCFCHP'
     C******************** Z-ADDXXCHP     SCFCHP           |
     C                   MOVE      XXCHP         SCFCHP                         |
      **** END   CHG **** 05/15/98 ************************************
     C                   MOVEA     '00000'       *IN(30)                        |
     C                   MOVE      '1'           *IN35                          |
     C                   ELSE                                                   |
      **** BEGIN CHG **** 05/15/98 ************************************
     C********** COL       IFGE 55                         |
     C********** COL       ANDLE57                         |
     C******************** Z-ADDXXCHP     SCTCHP           |
     C                   MOVE      XXCHP         SCTCHP                         |
      **** END   CHG **** 05/15/98 ************************************
     C                   MOVEA     '000000'      *IN(30)                        |
     C                   MOVE      '1'           *IN36                          |
      **** BEGIN DEL **** 05/15/98 ************************************
     C******************** END                             |
     C******************** END                             |
      **** END   DEL **** 05/15/98 ************************************
     C*                                                    |
     C                   GOTO      STRT                                         |
     C                   END                                                    |
      **** BEGIN CHG **** 05/15/98 ************************************
     C                   END                                                    HFLD SCFCHP
     C*                                                    |
     C*                                                    |
     C********** COMMAND 10 - TABLE FILE SEARCH  - COUNTY CODE     |
     C*                                                    |
     C*********************************
     C*   COUNTY CODE SEARCH          *
     C*********************************
      *
     C     HFLD          IFEQ      'SCFCNT'
     C     HFLD          OREQ      'SCTCNT'
      *
     C********** *INKJ     IFEQ '1'                        |
      **** END   CHG **** 05/15/98 ************************************
     C*                                                    |
     C                   MOVE      *BLANKS       XX1               2            |
     C                   MOVE      *BLANKS       XX2               2            |
     C                   MOVEL     SCFCNT        XX1                            |
     C                   MOVEL     SCTCNT        XX2                            |
     C*                                                    |
     C* Check for From and To Search for County Code       |
     C*                                                    |
     C     XX1           IFEQ      '? '                                         |
     C     HFLD          OREQ      'SCFCNT'
     C                   MOVE      *BLANKS       @@CNT
     C*
     C                   MOVE      '2'           @@CALL
     C*
     C                   CALL      'W2215R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CNT            10
     C                   PARM                    @@ST              2
     C                   PARM      2             @@VIEW            1 0
     C*
     C     *INKC         IFNE      *ON
     C*
     C     *INKL         IFEQ      *ON
     C     @@CNT         OREQ      *BLANKS
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEA     '00'          *IN(30)
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCFCNT
     C                   MOVEL     @@CNT         SCFCNT
     C                   MOVEA     '000'         *IN(30)
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ELSE                                                   *INKL
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  *INKC
     C*
     C                   ENDIF                                                  HFLD
     C*
     C     XX2           IFEQ      '? '
     C     HFLD          OREQ      'SCTCNT'
     C                   MOVE      *BLANKS       @@CNT
     C*
     C                   MOVE      '2'           @@CALL
     C*
     C                   CALL      'W2215R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CNT            10
     C                   PARM                    @@ST              2
     C                   PARM      2             @@VIEW            1 0
     C*
     C     *INKC         IFNE      *ON
     C*
     C     *INKL         IFEQ      *ON
     C     @@CNT         OREQ      *BLANKS
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEA     '000'         *IN(30)
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCTCNT
     C                   MOVEL     @@CNT         SCTCNT
     C                   MOVEA     '0000'        *IN(30)
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ELSE
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  *INKC
     C*
     C                   ENDIF                                                  HFLD
     C*
     C                   GOTO      STRT
     C*
      **** BEGIN CHG **** 05/15/98 ************************************
     C                   ENDIF                                                  HFLD SCFCNT
      *
     C********************************
     C*  AGENT NUMBER SEARCH         *
     C********************************
      *
     C     HFLD          IFEQ      'SCFAGT'
     C     HFLD          OREQ      'SCTAGT'
      *
     C                   CALL      'W0040R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM                    @@VIEW            1 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      '1'
     C     @@RRN         OREQ      *ZEROS
     C     HFLD          CABEQ     'SCFAGT'      STRT                     36    PC SCFAGT
     C     HFLD          CABEQ     'SCTAGT'      STRT                     38    PC SCTAGT
     C                   ENDIF                                                  IFEQ *INKL
      *
     C     @@RRN         CHAIN     PFRCDF                             60
     C     *IN60         IFEQ      *OFF
     C     HFLD          IFEQ      'SCFAGT'
     C                   MOVE      AAAGT#        SCFAGT
     C                   MOVE      *ON           *IN38
     C                   ELSE
     C                   MOVE      AAAGT#        SCTAGT
     C                   MOVE      *ON           *IN29
     C                   ENDIF                                                  HFLD SCFAGT
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCFAGT
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *INKJ
      **** END   ADD **** 05/15/98 ************************************
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
     C*
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
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*                                                    JJJ 3-2-89
     C*   SORT MUST BE EQUAL TO 'N'; 'E'; 'C'; 'H'; OR 'A' |
     C*                                                    |
     C     SCSORT        IFNE      'N'                                          |
     C     SCSORT        ANDNE     'E'                                          |
     C     SCSORT        ANDNE     'C'                                          |
     C     SCSORT        ANDNE     'H'                                          |
     C     SCSORT        ANDNE     'A'                                          |
     C                   MOVE      '1'           *IN20                          |
     C                   GOTO      STRT                                         |
     C                   END                                                    |
     C*                                                    |
     C*   ONLY ONE RANGE CAN BE SELECTED                   |
     C*                                                    |
     C     SCTEMP        IFNE      *ZERO                                        |
     C     SCTAGT        IFNE      *ZERO                                        |
     C     SCTCNT        ORNE      *BLANKS                                      |
     C     SCTCHP        ORNE      *ZERO                                        |
     C                   MOVE      '1'           *IN21                          |
     C                   GOTO      STRT                                         |
     C                   END                                                    |
     C                   END                                                    |
     C*                                                    |
     C     SCTAGT        IFNE      *ZERO                                        |
     C     SCTCNT        IFNE      *BLANKS                                      |
     C     SCTCHP        ORNE      *ZERO                                        |
     C                   MOVE      '1'           *IN21                          |
     C                   GOTO      STRT                                         |
     C                   END                                                    |
     C                   END                                                    |
     C*                                                    |
     C     SCTCHP        IFNE      *ZERO                                        |
     C     SCTCNT        ANDNE     *BLANKS                                      |
     C                   MOVE      '1'           *IN21                          |
     C                   GOTO      STRT                                         |
     C                   END                                                    |
     C*                                                    |
     C*** BEGIN ADD *** 06/26/97 **************************************
     C*
     C* IF COUNTY SORT IS SELECTED RANGE CAN'T BE BLANK
     C*
     C     SCSORT        IFEQ      'C'
      *** BEGIN CHG *** 10/19/98 **************************************
     C********** SCTCNT    ANDEQ*BLANKS                    |
     C     SCFCNT        IFEQ      *BLANKS                                      |
     C     SCTCNT        OREQ      *BLANKS                                      |
      *** END   CHG *** 10/19/98 **************************************
     C*
     C                   MOVE      '1'           *IN37                          |
     C                   GOTO      STRT                                         |
     C*
      *** BEGIN ADD *** 10/19/98 **************************************
     C                   ELSE
     C     SCFCNT        IFGT      SCTCNT                                       |
     C                   MOVE      '1'           *IN37                          |
     C                   GOTO      STRT                                         |
     C                   ENDIF                                                  SCF > SCT
     C                   ENDIF                                                  SCFCNT
     C*** END   ADD *** 10/19/98 **************************************
     C                   ENDIF                                                  |
     C*** END   ADD *** 06/26/97 **************************************
      *** BEGIN ADD *** 10/19/98 **************************************
     C*
     C* IF EMPLOYER SORT IS SELECTED RANGE CAN'T BE BLANK
     C*
     C     SCSORT        IFEQ      'E'
     C     SCFEMP        IFEQ      *ZEROS                                       |
     C     SCTEMP        OREQ      *ZEROS                                       |
     C*
     C                   MOVE      '1'           *IN41                          |
     C                   GOTO      STRT                                         |
     C*
     C                   ELSE
     C     SCFEMP        IFGT      SCTEMP                                       |
     C                   MOVE      '1'           *IN41                          |
     C                   GOTO      STRT                                         |
     C                   ENDIF                                                  SCF > SCT
     C                   ENDIF                                                  SCFEMP
     C                   ENDIF                                                  SCSORT='E'
      *
      ***** BEG DEL ***** 11/04/05 ***************************************
      ***** BEG ADD ***** 08/30/05 ***************************************
     C*********
     C* If Claiment Sort is Selected and Employer Range entered - Edit
     C*********
     C*********  SCSORT    IFEQ 'N'
     C*********  SCFEMP    IFNE *ZEROS
     C*********  SCTEMP    ORNE *ZEROS
     C*********
     C*********  SCFEMP    IFGT SCTEMP
     C*********            MOVE '1'       *IN41
     C*********            GOTO STRT
     C*********            ENDIF                           SCF > SCT
     C*********
     C*********            ENDIF                           SCFEMP
     C*********            ENDIF                           SCSORT='N'
      ***** END ADD ***** 08/30/05 ***************************************
      ***** END DEL ***** 11/04/05 ***************************************
     C*
     C* IF CHAPTER SORT IS SELECTED RANGE CAN'T BE BLANK
     C*
     C     SCSORT        IFEQ      'H'
     C     SCFCHP        IFEQ      *ZEROS                                       |
     C     SCTCHP        OREQ      *ZEROS                                       |
     C*
     C                   MOVE      '1'           *IN42                          |
     C                   GOTO      STRT                                         |
     C*
     C                   ELSE
     C     SCFCHP        IFGT      SCTCHP                                       |
     C                   MOVE      '1'           *IN42                          |
     C                   GOTO      STRT                                         |
     C                   ENDIF                                                  SCF > SCT
     C                   ENDIF                                                  SCFCHP
     C                   ENDIF                                                  SCSORT='H'
     C*
     C* IF AGENT SORT IS SELECTED RANGE CAN'T BE BLANK
     C*
     C     SCSORT        IFEQ      'A'
     C     SCFAGT        IFEQ      *ZEROS                                       |
     C     SCTAGT        OREQ      *ZEROS                                       |
     C*
     C                   MOVE      '1'           *IN43                          |
     C                   GOTO      STRT                                         |
     C*
     C                   ELSE
     C     SCFAGT        IFGT      SCTAGT                                       |
     C                   MOVE      '1'           *IN43                          |
     C                   GOTO      STRT                                         |
     C                   ENDIF                                                  SCF > SCT
     C                   ENDIF                                                  SCFAGT
     C                   ENDIF                                                  SCSORT='A'
     C*** END   ADD *** 10/19/98 **************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   CALL      'W7240C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    DATA                           JJJ 3-2-89
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
