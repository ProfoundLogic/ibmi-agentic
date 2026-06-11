     H*****************************************************************
     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H*                                                               *
     H* PROGRAM: F2305RTN01                                           *
     H* DESCRIPTION: CPT-4 CODE MAINTENANCE FOR TN                    *
     H*****************************************************************
     H*  CREATED: 04/01/08 C:191 TN008                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     A*                                                               *
     A*  04/21/10 C:198 Project: TN014                                *
     A*                 Added 2 fields Gen Med Emergency 26 and TC    *
     A*                 fields.                                       *
     A*                                                               *
     A*  09/01/09 C:198 Support: 69594                                *
     A*                 Added 100% CMS field to the right of APC      *
     A*                 Allow.                                        *
     A*                                                               *
     H*****************************************************************
     FF2305FTN01CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMPRPTN   UF   E             DISK
     F                                     RENAME(PPR01:DBRCDF)
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
     D*
     D/COPY SCOPYMT01C
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
     C                   MOVEL     PRDSC1        SCDSC1
     C*
     C                   Z-ADD     PREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVEL     PRSTAT        SCSTAT
     C*
     C                   MOVE      PR92NS        SC92NS
     C*** Beg Add *** 09/01/09 **********************************
     C                   MOVE      PR92NR        SC92NR
     C*** End Add *** 09/01/09 **********************************
     C                   MOVE      PRMRA         SCMRA
     C                   MOVE      PR92NU        SC92NU
     C                   MOVE      PR92N5        SC92N5
     C                   MOVE      PR92NV        SC92NV
     C                   MOVE      PR92N3        SC92N3
     C                   MOVE      PR92NW        SC92NW
     C                   MOVE      PR92N4        SC92N4
     C                   MOVE      PR92NX        SC92NX
     C                   MOVE      PR92N2        SC92N2
     C                   MOVE      PR92NY        SC92NY
     C                   MOVE      PR92NT        SC92NT
     C                   MOVE      PR92NZ        SC92NZ
     C                   MOVE      PR92NF        SC92NF
     C                   MOVE      PR92NG        SC92NG
     C                   MOVE      PR92NH        SC92NH
     C                   MOVE      PR92NI        SC92NI
     C                   MOVE      PR92NJ        SC92NJ
     C                   MOVE      PR92NQ        SC92NQ
     C                   MOVE      PR92O1        SC92O1
     C*** Beg Add *** 04/21/10 *************************************
     C                   MOVE      PR92O8        SC92O8
     C                   MOVE      PR92O9        SC92O9
     C                   MOVE      PR92OA        SC92OA
     C                   MOVE      PR92OB        SC92OB
     C*** End Add *** 04/21/10 *************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCCPT4
     C                   MOVEL     *BLANKS       SCDSC1
     C                   MOVE      *ZEROS        SCEFDT
     C*
     C* Bring region 1 fields in from the previous screen add
     C*
     C                   MOVE      *ZEROS        SC92NS
     C*** Beg Add *** 09/01/09 **********************************
     C                   MOVE      *ZEROS        SC92NR
     C*** End Add *** 09/01/09 **********************************
     C                   MOVE      *ZEROS        SCMRA
     C                   MOVE      *ZEROS        SC92NU
     C                   MOVE      *ZEROS        SC92N5
     C                   MOVE      *ZEROS        SC92NV
     C                   MOVE      *ZEROS        SC92N3
     C                   MOVE      *ZEROS        SC92NW
     C                   MOVE      *ZEROS        SC92N4
     C                   MOVE      *ZEROS        SC92NX
     C                   MOVE      *ZEROS        SC92N2
     C                   MOVE      *ZEROS        SC92NY
     C                   MOVE      *ZEROS        SC92NT
     C                   MOVE      *ZEROS        SC92NZ
     C                   MOVE      *ZEROS        SC92NF
     C                   MOVE      *ZEROS        SC92NG
     C                   MOVE      *ZEROS        SC92NH
     C                   MOVE      *ZEROS        SC92NI
     C                   MOVE      *ZEROS        SC92NJ
     C                   MOVE      *ZEROS        SC92NQ
     C                   MOVE      *ZEROS        SC92O1
     C*** Beg Add *** 04/21/10 *************************************
     C                   MOVE      *ZEROS        SC92O8
     C                   MOVE      *ZEROS        SC92O9
     C                   MOVE      *ZEROS        SC92OA
     C                   MOVE      *ZEROS        SC92OB
     C*** End Add *** 04/21/10 *************************************
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
     C*
     C                   MOVE      SC92NS        PR92NS
     C*** Beg Add *** 09/01/09 **********************************
     C                   MOVE      SC92NR        PR92NR
     C*** End Add *** 09/01/09 **********************************
     C                   MOVE      SCMRA         PRMRA
     C                   MOVE      SC92NU        PR92NU
     C                   MOVE      SC92N5        PR92N5
     C                   MOVE      SC92NV        PR92NV
     C                   MOVE      SC92N3        PR92N3
     C                   MOVE      SC92NW        PR92NW
     C                   MOVE      SC92N4        PR92N4
     C                   MOVE      SC92NX        PR92NX
     C                   MOVE      SC92N2        PR92N2
     C                   MOVE      SC92NY        PR92NY
     C                   MOVE      SC92NT        PR92NT
     C                   MOVE      SC92NZ        PR92NZ
     C                   MOVE      SC92NF        PR92NF
     C                   MOVE      SC92NG        PR92NG
     C                   MOVE      SC92NH        PR92NH
     C                   MOVE      SC92NI        PR92NI
     C                   MOVE      SC92NJ        PR92NJ
     C                   MOVE      SC92NQ        PR92NQ
     C                   MOVE      SC92O1        PR92O1
     C                   MOVEL     SCSTAT        PRSTAT
     C*** Beg Add *** 04/21/10 *************************************
     C                   MOVE      SC92O8        PR92O8
     C                   MOVE      SC92O9        PR92O9
     C                   MOVE      SC92OA        PR92OA
     C                   MOVE      SC92OB        PR92OB
     C*** End Add *** 04/21/10 *************************************
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
