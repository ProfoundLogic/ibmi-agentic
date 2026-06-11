     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*  F2550R - WORK WITH BILLS WITH ERROR STATUS                   *
     H*                                                               *
     H*  CALLED FROM: F2550R                                          *
     H*                                                               *
     H*****************************************************************
     H*  Created on 02/21/2012 by c:280 for project 133MCC            *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     FF2550F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FFAU6P     UF A E             DISK
     F                                     RENAME(PU601:DBRCDF)
     F*
     F*  BILL UPLOAD ERROR STATUS BY TRID/STDT/STTM
     F*
     FFAU1L3    UF   E           K DISK
     F*
     F*  PAYMENT FILE - HEADER  BY TRID
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************
     D*
     D*
     D CLM             S              1    DIM(20)
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Bills with Errors -
     D                                     Status Mainenance')
     D OPTNMT          C                   CONST('Bills with Errors -
     D                                     Status Maintenance')
     D OPTNMD          C                   CONST('Bills with Errors -
     D                                     Status Deletion')
     D*
     D COMAPI          C                   CONST('FA007R')
      *
     C/COPY SCOPYMT01C
      *
     I*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     I*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C                   PARM                    @@TRID           15
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY           KLIST
     C                   KFLD                    KKTRID
     C                   KFLD                    KKSTDT
     C                   KFLD                    KKSTTM
     C                   KFLD                    KKESTS
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    U6TRID        KKTRID
     C     *LIKE         DEFINE    U6STDT        KKSTDT
     C     *LIKE         DEFINE    U6STTM        KKSTTM
     C     *LIKE         DEFINE    U6ESTS        KKESTS
     C*
     C     *LIKE         DEFINE    U1CO#         ##CO#
     C     *LIKE         DEFINE    U1SUB#        ##SUB#
     C     *LIKE         DEFINE    U1FND         ##FND
     C     *LIKE         DEFINE    U1FYR         ##FYR
     C     *LIKE         DEFINE    U1CASE        ##CASE
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK -
     C*================================================================
     C*
     C     ERRCK         BEGSR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   SETOFF                                       212290
     C*
     C* MUST ENTER STATUS
     C*
     C     SCESTS        IFEQ      *BLANKS
     C                   SETON                                        21
     C                   SETON                                        90
     C                   ENDIF
     C*
     C* MUST ENTER REASON
     C*
     C     SCREAS        IFEQ      *BLANKS
     C                   SETON                                        22
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      U6ESTS        SCESTS
     C                   MOVEL     U6REAS        SCREAS
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C* STANDARD USER/PGM/DATE/TIME
     C*
     C                   MOVEL     Q1USER        U6USER
     C                   MOVEL     Q1PGM         U6PGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U6DT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        U6TIME
     C*
     C                   MOVE      'A'           U6STAT
     C*
     C* ADD ?
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      SCTRID        U6TRID
     C                   MOVE      SCESTS        U6ESTS
     C                   MOVEL     SCREAS        U6REAS
     C*
     C                   MOVEL     Q1PGM         U6IPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U6STDT
     C                   MOVEL     SRDTO         U6IDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        U6STTM
     C                   Z-ADD     DSTIME        U6ITIM
     C*
     C                   ELSE
     C*
     C                   MOVE      SCESTS        U6ESTS
     C                   MOVEL     SCREAS        U6REAS
     C*
     C                   MOVEL     Q1PGM         U6PGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U6DT
     C                   MOVEL     SRDTO         U6STDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        U6TIME
     C                   Z-ADD     DSTIME        U6STTM
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       0916
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@CALL        COMP      'D'                                    16
     C*
     C     *IN16         IFEQ      *OFF
     C                   MOVEL(P)  OPTNMT        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNMD        ##DATA
     C                   ENDIF
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C                   MOVEL     'N'           XXCHG             1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDX
     C*
     C* GET PAYMENT HEADER
     C*
     C     @@TRID        CHAIN     PU101                              98
     C*
     C                   MOVEL(P)  @@TRID        SCTRID
     C*
     C                   CALL      'SRNAME'
     C                   PARM      U1CLSN        ##LAST           56
     C                   PARM      U1CFSN        ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  U1SS#         SCFEIN
      /Free
       scfein = DisplaySS#(MaskData('SSN':'U1SS#':U1SS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     U1DOI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVEL     U1CLM#        SCCLM#
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
     C     @@TRID        CHAIN     PU101                              98
     C*
     C                   MOVEL(P)  Q1USER        U1DLUS
     C                   MOVEL(P)  Q1PGM         U1DLPG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        U1DLTM
     C                   MOVE      SCESTS        U1ESTS
     C*
     C                   UPDATE    PU101
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   WRITE     DBRCDF
     C*
     C     @@TRID        CHAIN     PU101                              98
     C*
     C                   MOVEL(P)  Q1USER        U1DLUS
     C                   MOVEL(P)  Q1PGM         U1DLPG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        U1DLTM
     C                   MOVE      SCESTS        U1ESTS
     C*
     C                   UPDATE    PU101
     C*
     C                   ENDSR
     C*
