     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE F0048R01 - PROVIDER DRG RATE INFORMATION
     H*****************************************************************
     H** CREATED ON 01/08/02 BY C:62 FOR PROJECT WV004
     H*****************************************************************
     H*  REVISIONS:
     H*  10/18/02 C:79 SUPPORT: MC065 34597
     H*                ADD EFDT TO KEY WHEN ASSIGNING FEIN/MOD
     H*                                                                 *
     H*****************************************************************
     FF0048F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMP2P     UF A E             DISK
     F                                     RENAME(PP201:DBRCDF)
     FFMP2L     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFMP2L2    UF   E           K DISK
     F                                     RENAME(PP201:PP202)
     FFMSVL1    IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider DRG Rate -
     D                                     Information')
     D COMAPI          C                   CONST('FA007R')
     D*             'FSMESG'              C         SLMSGF
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
     C                   PARM                    ##CO#             3 0
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
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     P2KY1         KLIST
     C                   KFLD                    KKNAME
     C*** BEG ADD *** 10/18/02 ****************************************
     C                   KFLD                    KKEFDT
     C*** END ADD *** 10/18/02 ****************************************
     C*
     C     P2KY2         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKSV01
     C                   KFLD                    KKEFDT
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     SVKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCMSV
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    P2NAME        KKNAME
     C     *LIKE         DEFINE    P2FEIN        KKFEIN
     C     *LIKE         DEFINE    P2MOD         KKMOD
     C     *LIKE         DEFINE    P2SV01        KKSV01
     C     *LIKE         DEFINE    P2EFDT        KKEFDT
     C     *LIKE         DEFINE    SVCMSV        KKCMSV
     C     *LIKE         DEFINE    STKEY         KKST
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C     P2STAT        IFEQ      'I'
     C                   MOVE      'A'           P2STAT
     C                   ELSE
     C                   MOVE      'I'           P2STAT
     C                   ENDIF                                                  P2STAT
     C*
     C                   MOVEL     Q1USER        P2USER
     C                   MOVEL     Q1PGM         P2PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2DT
     C                   MOVEL     DSTIME        P2TIME
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* NAME IS REQUIRED
     C*
     C     *IN40         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C     SCNAME        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C* EFFECTIVE DATE IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C*
     C* DRG CODE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCSV01        KKCMSV
     C     SVKY          CHAIN     PSV01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C*
     C* ONLY VALID PROVIDERS OF THE STATE PASSED IN
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7490
     C                   ELSE
     C     P1PST         IFNE      @@ST
     C                   SETON                                        7590
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* MUST BE UNIQUE
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C                   MOVEL(P)  SCSV01        KKSV01
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C     *IN40         IFEQ      *ON
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C                   MOVEL(P)  SCSV01        KKSV01
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C     P2KY2         CHAIN     PP201                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        7690
     C                   ENDIF
     C                   ELSE
     C     P2KY2         CHAIN     PP201                              99
     C     *IN99         DOWEQ     *OFF
     C     DBRRN         IFNE      @@RRN
     C                   SETON                                        7690
     C                   LEAVE
     C                   ENDIF
     C     P2KY2         READE     PP201                                  99
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   EXSR      DATSBR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCSV01'
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      @@ST          ##ST              2
     C                   PARM      *BLANKS       ##CMSV           15
     C                   PARM      *BLANKS       ##CDSC           30
     C*
     C     ##KL          IFEQ      *ON
     C     ##CMSV        OREQ      *BLANKS
     C                   SETON                                        9031
     C                   ELSE
     C                   MOVEL(P)  ##CMSV        SCSV01
     C                   MOVEL(P)  ##CDSC        SCSVDS
     C                   SETON                                        9032
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      *BLANKS       ##TYPE            1
     C                   PARM      *BLANKS       ##1N4             1            type 1 & 4 only
     C                   PARM      *BLANKS       ##1PRV            1            Y=subset on fein/mod
     C                   PARM      *ZEROS        ##PRIV            1 0          VIEW
     C                   PARM      @@ST          ##PST             2            ADJUDICATION ST
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   SETON                                        32
     C                   ELSE
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C                   MOVE      ##FEIN        KKFEIN
     C                   MOVE      ##MOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDNAME        SCPNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPNAM
     C                   ENDIF
     C                   SETON                                        33
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   ENDSL                                                              N
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     P2NAME        SCNAME
     C*
     C                   Z-ADD     P2EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     P2SV01        SCSV01
     C                   Z-ADD     P2FEIN        SCFEIN
     C                   Z-ADD     P2MOD         SCMOD
     C                   Z-ADD     P2AM01        SCAM01
     C                   MOVEL     P2STAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   MOVEL     *BLANKS       SCSV01
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C                   Z-ADD     *ZEROS        SCAM01
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
     C                   MOVEL     SCNAME        P2NAME
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2EFDT
     C*
     C                   MOVEL     SCSV01        P2SV01
     C                   Z-ADD     SCFEIN        P2FEIN
     C                   Z-ADD     SCMOD         P2MOD
     C                   Z-ADD     SCAM01        P2AM01
     C                   MOVEL     SCSTAT        P2STAT
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
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C                   SETON                                        32
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        P2USER
     C                   MOVEL     Q1PGM         P2PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2DT
     C*
     C                   MOVEL     DSTIME        P2TIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   MOVEL(P)  P2NAME        KKNAME
     C     P2KY1         CHAIN     PP202                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     SCFEIN        P2FEIN
     C                   Z-ADD     SCMOD         P2MOD
     C                   UPDATE    PP202
     C     P2KY1         READE     PP202                                  99
     C                   ENDDO
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        P2USER
     C                   MOVEL     Q1PGM         P2PGM
     C                   MOVEL     Q1USER        P2IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2DT
     C                   MOVEL     SRDTO         P2IDT
     C                   MOVEL     DSTIME        P2TIME
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FMP2P
     C*
     C                   ENDSR
