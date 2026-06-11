     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC300R01 - SUMMARY CODE UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays the Summary Code      **
     H**     file.                                                   **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC300R - Work with Summary Codes                        **
     H**   CALLS TO:                                                 **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   12/11/97      PROJECT: DR047                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      40   Add mode
     H**      60   Used for chaining
     H**      73   SCR01 SC40002 Error - Duplicate record (SMCD)
     H**      84   SCR01 SCM0631 Error - Must be A, I, or blank (STAT)
     H**      80   SCR01 SCM0503 Error - Required field (SMCD)
     H**      81   SCR01 SCM0503 Error - Required field (ds40)
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     FSC300F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSTSMP     UF A E             DISK
     F                                     RENAME(PSM01:DBRCDF)
     FSTSML     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Summary Code Inform-
     D                                     ation')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
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
     C* CHECK SUMMARY CODE
     C*
     C  N90SCSMCD        IFEQ      *BLANKS
     C                   SETON                                        9080
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN CHANGE MODE  ****
     C     SCSMCD        IFNE      CPSMCD
     C     SCSMCD        CHAIN     PSM01                              60
     C  N60              SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C****  TO PREVENT DUPLICATE RECORD WHILE IN ADD MODE  ****
     C     *IN40         IFEQ      *ON
     C     SCSMCD        CHAIN     PSM01                              60
     C  N60              SETON                                        7390
     C                   ENDIF                                                  NE ORIGINALPATH
     C*
     C* CHECK DESCRIPTION
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        9081
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C     *IN40         IFEQ      *ON
     C                   MOVE      *BLANKS       SCSMCD
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      'A'           SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      SMSMCD        SCSMCD
     C                   MOVE      SMSMCD        CPSMCD            4
     C                   MOVE      SMDESC        SCDESC
     C                   MOVE      SMSTAT        SCSTAT
     C                   ENDIF                                                  *IN40
     C*
     C                   TIME                    DFDTTM
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      SCSMCD        SMSMCD
     C                   MOVE      SCDESC        SMDESC
     C                   MOVE      SCSTAT        SMSTAT
     C                   EXSR      MVUPDB
     C                   ENDSR
     C*================================================================
     C* MVUPDB - Move update fields to data base fields
     C*================================================================
     C     MVUPDB        BEGSR
     C                   MOVEL     Q1USER        SMUSER
     C                   MOVEL     Q1PGM         SMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SMDT
     C                   MOVEL     DSTIME        SMTIME
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*                    MOVE *ON       *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*          MODE 'S' - Change status
     C*
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     SMSTAT        IFEQ      'I'
     C                   MOVE      'A'           SMSTAT
     C                   ELSE
     C                   MOVE      'I'           SMSTAT
     C                   ENDIF
     C                   EXSR      MVUPDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        SMIUSR
     C                   MOVEL     Q1PGM         SMIPGM
     C                   MOVEL     DFTIME        SMITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SMIDT
     C                   WRITE     DBRCDF
     C                   FEOD      STSMP
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
