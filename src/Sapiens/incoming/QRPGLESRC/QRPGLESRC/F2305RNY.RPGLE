     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H* PROGRAM: F2305RNY                                             *
     H* DESCRIPTION: CPT-4 CODE MAINTENANCE FOR TX                    *
     H*****************************************************************
     H*  CREATED: 09/18/02 C:79 NY006                                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  12/17/19 C: 212 Project: NY048                               *
     H*                  Add Acupuncture fields in DSPF and related   *
     H*                  processing                                   *
     H*
     H*  04/03/19 C: 212 Project: NY044                               *
     H*                  Recompile only for changes in DSPF           *
     H*
     H*  08/03/15 C: 079 Project: SY360                               *
     H*                  Remove F10 and F23 from the screen           *
     H*****************************************************************
     FF2305FNY  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMPRPNY   UF   E             DISK
     F                                     RENAME(PPR01:DBRCDF)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('CPT-4 Code Mai-
     D                                     ntenance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PRCO#         ##CO#
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C*
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90PRDT          IFGT      SAVEDT
     C     PRDT          OREQ      SAVEDT
     C     PRTIME        ANDGT     SAVETM
     C                   SETON                                        9034
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     PRCPT4        SCCPT4
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      PRMRA         SCMRA
     C                   MOVE      PRANAM        SCANAM
     C                   MOVE      PRPROF        SCPROF
     C                   MOVE      PR92N1        SC92N1
     C                   MOVE      PR92N2        SC92N2
     C                   MOVE      PR92N3        SC92N3
     C                   MOVE      PR92N4        SC92N4
     C                   MOVE      PR92N5        SC92N5
     C                   MOVE      PR92N6        SC92N6
     C                   MOVE      PR92N7        SC92N7
     C                   MOVE      PR92N8        SC92N8
     C                   MOVE      PR92N9        SC92N9
     C                   MOVE      PR92NA        SC92NA
     C                   MOVE      PR92NB        SC92NB
     C                   MOVE      PR92NC        SC92NC
     C                   MOVE      PR92ND        SC92ND
     C                   MOVE      PR92NE        SC92NE
     C                   MOVE      PR92NF        SC92NF
     C                   MOVE      PR92NG        SC92NG
     C                   MOVE      PR92NH        SC92NH
     C                   MOVE      PR92NI        SC92NI
     C                   MOVE      PR92NJ        SC92NJ
     C                   MOVE      PR92NK        SC92NK
     C                   MOVE      PR92NL        SC92NL
     C                   MOVE      PR92NM        SC92NM
     C                   MOVE      PR92NN        SC92NN
     C                   MOVE      PR92NO        SC92NO
     C                   MOVE      PR92NP        SC92NP
     C                   MOVE      PR92NQ        SC92NQ
     C                   MOVE      PR92NR        SC92NR
     C                   MOVE      PR92NS        SC92NS
     C                   MOVE      PR92NT        SC92NT
     C                   MOVE      PR92NU        SC92NU
     C                   MOVE      PR92NV        SC92NV
     C                   MOVE      PR92NW        SC92NW
     C                   MOVE      PR92NX        SC92NX
     C                   MOVE      PR92NY        SC92NY
     C                   MOVE      PR92NZ        SC92NZ
     C                   MOVE      PR92O1        SC92O1
     C                   MOVE      PR92O2        SC92O2
     C                   MOVE      PR92O3        SC92O3
     C                   MOVE      PR92O4        SC92O4
     C                   MOVE      PR92O5        SC92O5
     C                   MOVE      PR92O6        SC92O6
     C                   MOVE      PR92O7        SC92O7
     C                   MOVE      PR92O8        SC92O8
     C                   MOVE      PR92O9        SC92O9
     C                   MOVE      PR92OA        SC92OA
     C***** Beg Add *** 12/17/19 ************************************
     C/free
        sc92OC = pr92OC;
        sc92OD = pr92OD;
        sc92OE = pr92OE;
        sc92OF = pr92OF;
      /end-free
     C***** End Add *** 12/17/19 ************************************
     C                   MOVEL     PRSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCCPT4
     C                   MOVE      *ZEROS        SCEFDT
     C*
     C* Bring region 1 fields in from the previous screen add
     C*
     C                   MOVE      PRMRA         SCMRA
     C                   MOVE      PR92N1        SC92N1
     C                   MOVE      PRPROF        SCPROF
     C                   MOVE      PRANAM        SCANAM
     C                   MOVE      *ZEROS        SC92N2
     C                   MOVE      *ZEROS        SC92N3
     C                   MOVE      *ZEROS        SC92N4
     C                   MOVE      *ZEROS        SC92N5
     C                   MOVE      *ZEROS        SC92N6
     C                   MOVE      *ZEROS        SC92N7
     C                   MOVE      *ZEROS        SC92N8
     C                   MOVE      *ZEROS        SC92N9
     C                   MOVE      *ZEROS        SC92NA
     C                   MOVE      *ZEROS        SC92NB
     C                   MOVE      *ZEROS        SC92NC
     C                   MOVE      *ZEROS        SC92ND
     C                   MOVE      *ZEROS        SC92NE
     C                   MOVE      *ZEROS        SC92NF
     C                   MOVE      *ZEROS        SC92NG
     C                   MOVE      *ZEROS        SC92NH
     C                   MOVE      *ZEROS        SC92NI
     C                   MOVE      *ZEROS        SC92NJ
     C                   MOVE      *ZEROS        SC92NK
     C                   MOVE      *ZEROS        SC92NL
     C                   MOVE      *ZEROS        SC92NM
     C                   MOVE      *ZEROS        SC92NN
     C                   MOVE      *ZEROS        SC92NO
     C                   MOVE      *ZEROS        SC92NP
     C                   MOVE      *ZEROS        SC92NQ
     C                   MOVE      *ZEROS        SC92NR
     C                   MOVE      *ZEROS        SC92NS
     C                   MOVE      *ZEROS        SC92NT
     C                   MOVE      *ZEROS        SC92NU
     C                   MOVE      *ZEROS        SC92NV
     C                   MOVE      *ZEROS        SC92NW
     C                   MOVE      *ZEROS        SC92NX
     C                   MOVE      *ZEROS        SC92NY
     C                   MOVE      *ZEROS        SC92NZ
     C                   MOVE      *ZEROS        SC92O1
     C                   MOVE      *ZEROS        SC92O2
     C                   MOVE      *ZEROS        SC92O3
     C                   MOVE      *ZEROS        SC92O4
     C                   MOVE      *ZEROS        SC92O5
     C                   MOVE      *ZEROS        SC92O6
     C                   MOVE      *ZEROS        SC92O7
     C                   MOVE      *ZEROS        SC92O8
     C                   MOVE      *ZEROS        SC92O9
     C                   MOVE      *ZEROS        SC92OA
     C***** Beg Add *** 12/17/19 ************************************
     C/free
        sc92OC = *zeros;
        sc92OD = *zeros;
        sc92OE = *zeros;
        sc92OF = *zeros;
      /end-free
     C***** End Add *** 12/17/19 ************************************
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         PRCO#
     C                   MOVEL     SCCPT4        PRCPT4
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PREFDT
     C                   MOVE      SCANAM        PRANAM
     C                   MOVE      SCMRA         PRMRA
     C                   MOVE      SC92N1        PR92N1
     C                   MOVE      SCPROF        PRPROF
     C                   MOVE      SC92N2        PR92N2
     C                   MOVE      SC92N3        PR92N3
     C                   MOVE      SC92N4        PR92N4
     C                   MOVE      SC92N5        PR92N5
     C                   MOVE      SC92N6        PR92N6
     C                   MOVE      SC92N7        PR92N7
     C                   MOVE      SC92N8        PR92N8
     C                   MOVE      SC92N9        PR92N9
     C                   MOVE      SC92NA        PR92NA
     C                   MOVE      SC92NB        PR92NB
     C                   MOVE      SC92NC        PR92NC
     C                   MOVE      SC92ND        PR92ND
     C                   MOVE      SC92NE        PR92NE
     C                   MOVE      SC92NF        PR92NF
     C                   MOVE      SC92NG        PR92NG
     C                   MOVE      SC92NH        PR92NH
     C                   MOVE      SC92NI        PR92NI
     C                   MOVE      SC92NJ        PR92NJ
     C                   MOVE      SC92NK        PR92NK
     C                   MOVE      SC92NL        PR92NL
     C                   MOVE      SC92NM        PR92NM
     C                   MOVE      SC92NN        PR92NN
     C                   MOVE      SC92NO        PR92NO
     C                   MOVE      SC92NP        PR92NP
     C                   MOVE      SC92NQ        PR92NQ
     C                   MOVE      SC92NR        PR92NR
     C                   MOVE      SC92NS        PR92NS
     C                   MOVE      SC92NT        PR92NT
     C                   MOVE      SC92NU        PR92NU
     C                   MOVE      SC92NV        PR92NV
     C                   MOVE      SC92NW        PR92NW
     C                   MOVE      SC92NX        PR92NX
     C                   MOVE      SC92NY        PR92NY
     C                   MOVE      SC92NZ        PR92NZ
     C                   MOVE      SC92O1        PR92O1
     C                   MOVE      SC92O2        PR92O2
     C                   MOVE      SC92O3        PR92O3
     C                   MOVE      SC92O4        PR92O4
     C                   MOVE      SC92O5        PR92O5
     C                   MOVE      SC92O6        PR92O6
     C                   MOVE      SC92O7        PR92O7
     C                   MOVE      SC92O8        PR92O8
     C                   MOVE      SC92O9        PR92O9
     C                   MOVE      SC92OA        PR92OA
     C***** Beg Add *** 12/17/19 ************************************
     C/free
        pr92OC = sc92OC;
        pr92OD = sc92OD;
        pr92OE = sc92OE;
        pr92OF = sc92OF;
      /end-free
     C***** End Add *** 12/17/19 ************************************
     C                   MOVEL     SCSTAT        PRSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       0940
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PRUSER
     C                   MOVEL     Q1PGM         PRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT
     C*
     C                   MOVEL     DSTIME        PRTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   ENDSR
