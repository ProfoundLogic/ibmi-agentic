     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE F2315R01 - PROCEDURE CODE MODIFIER INFORMATION
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/14/10 C:251 Support:72163                                 *
     H*                 -On menu option  Work with Provider bills     *
     H*                  at PDSDEV, when select a TX physician bill,  *
     H*                  go to procedure code detail screen, take F10 *
     H*                  on procedure code modifier and then take     *
     H*                 -option 5 on a modifier, no information for   *
     H*                  that modifier comes up.                      *
     H*                  Added Overrides for FTPMP for state specific *
     H*                  files                                        *
     H*                                                               *
     H*  02/26/10 C:198 Project:MC337                                 *
     H*                 -Added a field to SCR01 for clients TML/TASB. *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                CHANGE OPTNM.                                  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF2315F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     F*** Beg Chg *** 04/14/10 ****************************************
     F*******FTPMP   UF  E                    DISK                      A
     FFTPMP     UF A E             DISK    USROPN
     F*** End Chg *** 04/14/10 ****************************************
     F                                     RENAME(PPM01:DBRCDF)
     FFTABL     IF   E           K DISK
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*** Beg Add *** 04/14/10 ****************************************
     D*
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*** End Add *** 04/14/10 ****************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*** Beg Add *** 04/14/10 ****************************************
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D*** End Add *** 04/14/10 ****************************************
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'Procedure Code Mod-  C         OPTNM
     D**********    'ifier Maintenance'
     D OPTNM           C                   CONST('Procedure Code Mod-
     D                                     ifier Information')
     D*** END   CHG *** 05/05/00 **************************************
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     PMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PMCO#         ##CO#
     C     *LIKE         DEFINE    PMKEY         KKKEY
     C     *LIKE         DEFINE    STKEY         KKST
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     PMSTAT        IFEQ      'I'
     C                   MOVE      'A'           PMSTAT
     C                   ELSE
     C                   MOVE      'I'           PMSTAT
     C                   ENDIF                                                  PMSTAT
     C*
     C                   MOVEL     Q1USER        PMUSER
     C                   MOVEL     Q1PGM         PMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMDT
     C*
     C                   MOVEL     DSTIME        PMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* PROCEDURE MOD CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE PROC/MOD CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         KKKEY
     C*
     C   40
     CANN90PMKY          CHAIN     PPM01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* PROCEDURE MOD CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C*  THE ASSISTING SURGEON CODE MUST 'Y' OR 'N'.
     C*
     C* N90      SCSURG    IFNE 'Y'
     C*          SCSURG    ANDNE'N'
     C*
     C*                    SETON                     7490
     C*
     C*                    ENDIF
     C*
     C*  THE PROF COMP FIELD MUST BE Y OR N.
     C*
     C* N90      SCPROF    IFNE 'N'
     C*          SCPROF    ANDNE'Y'
     C*
     C*                    SETON                     7590
     C*
     C*                    ENDIF
     C*
     C*   THE ASSISTING SURGEON AND PROFESSIONAL FIELDS CANNOT BOTH
     C*   BE "Y"
     C*
     C*          SCSURG    IFEQ 'Y'
     C*          SCPROF    ANDEQ'Y'
     C*
     C*                    SETON                     7690
     C*
     C*                    ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90PMDT          IFGT      SAVEDT
     C     PMDT          OREQ      SAVEDT
     C     PMTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C*** Beg Add *** 02/26/10 ****************************************
     C   28
     CANN90SCAFPY        IFNE      'Y'
     C     SCAFPY        ANDNE     ' '
     C                   SETON                                        9074
     C                   ENDIF
     C*
     C*** End Add *** 02/26/10 ****************************************
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
     C                   MOVEL     PMKEY         SCKEY
     C                   MOVEL     PMDESC        SCDESC
     C*** Beg Add *** 02/26/10 ****************************************
     C                   MOVEL     PMAFPY        SCAFPY
     C*** End Add *** 02/26/10 ****************************************
     C                   MOVE      PMPERC        SCPERC
     C                   MOVEL     PMSURG        SCSURG
     C                   MOVEL     PMPROF        SCPROF
     C                   MOVEL     PMSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C*** Beg Add *** 02/26/10 ****************************************
     C                   MOVEL     *BLANKS       SCAFPY
     C*** End Add *** 02/26/10 ****************************************
     C                   MOVE      *ZEROS        SCPERC
     C                   MOVEL     *BLANKS       SCSURG
     C                   MOVEL     *BLANKS       SCPROF
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
     C                   Z-ADD     ##CO#         PMCO#
     C                   MOVEL     SCKEY         PMKEY
     C                   MOVEL     SCDESC        PMDESC
     C*** Beg Add *** 02/26/10 ****************************************
     C                   MOVEL     SCAFPY        PMAFPY
     C*** End Add *** 02/26/10 ****************************************
     C                   MOVE      SCPERC        PMPERC
     C                   MOVEL     SCSURG        PMSURG
     C                   MOVEL     SCPROF        PMPROF
     C                   MOVEL     SCSTAT        PMSTAT
     C                   MOVEL     SCSTAT        PMSTAT
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
     C*** Beg Add *** 04/14/10 ****************************************
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FTPMP
     C*
     C*** End Add *** 04/14/10 ****************************************
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** Beg Add *** 02/26/10 ****************************************
     C                   SETOFF                                       28
     C*** End Add *** 02/26/10 ****************************************
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
     C                   MOVE      *ON           DSPTYP
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*** Beg Add *** 02/26/10 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C     XXCLT#        IFEQ      '025'
     C     XXCLT#        OREQ      '144'
     C     @@ST          IFEQ      'TX'
     C                   SETON                                        28
     C                   ENDIF
     C                   ENDIF
     C*** End Add *** 02/26/10 ****************************************
     C*
     C*** Beg Add *** 04/14/10 ****************************************
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        QQCMD
     C                   MOVEL     @@ST          QQST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      FTPMP
     C*
     C*** End Add *** 04/14/10 ****************************************
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
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
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PMUSER
     C                   MOVEL     Q1PGM         PMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMDT
     C*
     C                   MOVEL     DSTIME        PMTIME
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
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        PMUSER
     C                   MOVEL     Q1PGM         PMPGM
     C                   MOVEL     Q1USER        PMIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMDT
     C*
     C                   MOVEL     SRDTO         PMIDT
     C*
     C                   MOVEL     DSTIME        PMTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTPMP
     C*
     C                   ENDSR
**  (QCA)
DLTOVR FILE(FTPMP)
OVRDBF FILE(FTPMP) TOFILE(*LIBL/FTPMP  )
