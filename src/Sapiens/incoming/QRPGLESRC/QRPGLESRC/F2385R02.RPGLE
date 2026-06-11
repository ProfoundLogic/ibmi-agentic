     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F2400R02 - ROLL OVER STATE MESSAGES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program copies the messages of a particular        **
     H**     state and effective date into the same or another       **
     H**     state and/or effective date.                            **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F2385R - Work with State Messages                       **
     H**   CALLS TO:                                                 **
     H**   PROGRAMMER: C:79                                          **
     H**   CREATED:   07/07/99      PROJECT: AL036                   **
     H*****************************************************************
     H*
     H** REVISIONS:
     H*
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      31-36Used for Cursor Position on Rollover (F8) Option
     H**      37   MCR0500 Error - Invalid Date in Field
     H**      38   MCR0323 Error - Invalid Category Code Entered
     H**      39   MCR2447 Error - Invalid Rule Number Entered
     H**      40   MCR0500 Error - Invalid Date in Field
     H**      41-42MCR2448 Error - Invalid State Code Entered
     H**      43   MCR2449 Error - Rules already exist for St./Eff Dt.
     H**      44   MCR2450 Error - Rules don't exist for St./Eff Dt.
     H**      45   MCR2451 Error - State Code Code Entry Required
     H**      46   MCR2452 Error - Date Entry Required mm/dd/yy format
     H**      76 - 89 are reserved for error screen error indicators
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTMIP  (DBRCDF) State Rules                            **
     H**      STABL  (PST01)  System Wide State Code Table (KEY)     **
     H*****************************************************************
     H*
     FF2385F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FSTABL     IF   E           K DISK
     FFTMIP     UF A E             DISK
     F                                     RENAME(PMI01:DBRCDF)
     FFTMIL     IF   E           K DISK
     F                                     RENAME(PMI01:PMI02)
      /COPY SCOPYMT01D
     D*
     D DESC            S             70    DIM(5)                               Text Lines
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Roll Over State Mess-
     D                                     ages')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     I*
     I* ---------------------------------------------------------------
     I* SCDATA For message number sequence
     I* ---------------------------------------------------------------

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    ##STCD            2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Key List for STABL (PST01)  Sys. Wide State Code Table (KEY)
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*  Key Lists for FTMIP: State Fee Schedule Rules
     C*
     C*  Key list for FTMIL
      *
     C     MIKY          KLIST
     C                   KFLD                    KKMSG#
     C                   KFLD                    KKMSEQ
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    MIMSG#        KKMSG#
     C     *LIKE         DEFINE    MIMSG#        XXFMSG
     C     *LIKE         DEFINE    MIMSG#        XXTMSG
     C     *LIKE         DEFINE    MIMSEQ        KKMSEQ
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
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   SETOFF                                       404142
     C                   SETOFF                                       454647
     C*****************************************************************
     C*
     C*  Verify From State is a Valid Entry
     C*
     C  N90SCFST         IFNE      *BLANKS
     C     SCFST         ANDNE     'XX'
     C*
     C                   MOVEL(P)  SCFST         KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C   99              SETON                                        9041
     C*
     C                   ENDIF
     C*
     C*    CHECK FOR BLANK ENTRY IN STATE FIELD
     C*
     C  N90SCFST         IFEQ      *BLANKS
     C*
     C                   SETON                                        4590
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*    VERIFY TO STATE IS NOT BLANK AND IS A VALID CATEGORY
     C*
     C  N90SCTST         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCTST         KKKEY
     C*
     C     STKY          CHAIN     PST01                              60
     C*
     C   60              SETON                                        9042
     C*
     C                   ENDIF
     C*
     C*
     C*    CHECK FOR BLANK ENTRY IN STATE FIELD
     C*
     C  N90SCTST         IFEQ      *BLANKS
     C*
     C                   SETON                                        4790
     C*
     C                   ENDIF
     C*****************************************************************
     C  N90SCTST         IFEQ      *BLANKS
     C*
     C                   SETON                                        4690
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
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
     C* KJDO - F10= SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C* Turns off Indicators
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       37
     C*
     C                   SELECT
     C****************T***********************************************
     C************************
     C* SCR01 - STATE SEARCH *
     C************************
     C*
     C     HFLD          WHENEQ    'SCST'
     C     HFLD          OREQ      'SCTST'
     C*
     C*
     C                   CALL      'SY600R'
     C                   PARM      *BLANKS       SCAST             2
     C                   PARM                    QQRTCD            1
     C*
     C* F3=Exit pressed
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   SETON                                        3490
     C                   ENDIF
     C*
     C* F12=Cancel or no value selected
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCAST         OREQ      *BLANKS
     C*
     C                   MOVE      *ON           *INKL
     C*
     C     HFLD          IFEQ      'SCST'
     C*
     C                   SETON                                        3190
     C*
     C                   ENDIF                                                  HFLD = SCFST
     C*
     C     HFLD          IFEQ      'SCTST'
     C*
     C*
     C                   SETON                                        3590
     C*
     C                   ENDIF                                                  QRRTC = '*'
     C                   ENDIF                                                  QQRTCD= 'B'
     C*
     C*
     C* Value was selected
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C     HFLD          IFEQ      'SCST'
     C*
     C                   MOVE      SCAST         SCFST
     C                   SETON                                        3290
     C*
     C                   ELSE
     C*
     C                   MOVE      SCAST         SCTST
     C                   SETON                                        3690
     C*
     C                   ENDIF                                                  HFLD = SCFST
     C*
     C                   SETON                                        3690
     C*
     C                   ENDIF                                                  QQRTCD= *
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  HFLD =SCPCAT
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
      * Clear Screen fields
      *
     C                   MOVEL(P)  *BLANKS       SCFST
     C                   MOVEL(P)  *BLANKS       SCTST
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      '0000001'     XXFMSG
     C                   MOVE      '9999999'     XXTMSG
     C                   MOVEL     SCFST         XXFMSG
     C                   MOVEL     SCFST         XXTMSG
      *
      * Main read of physical state message file
      *
     C     *HIVAL        SETGT     DBRCDF
     C                   READP     DBRCDF                                 99
      *
     C     *IN99         DOWEQ     '0'
      *
     C     MIMSG#        IFGE      XXFMSG
     C     MIMSG#        ANDLE     XXTMSG
      *
      * Write record only if conditions are satisfied
      *
     C                   MOVEL(P)  MIMSG#        KKMSG#
     C                   MOVEL     SCTST         KKMSG#
     C                   MOVEL     MIMSEQ        KKMSEQ
      *
     C     MIKY          CHAIN     PMI02                              98
      *
     C     *IN98         IFEQ      *ON
      *
     C                   MOVEL     SCTST         MIMSG#
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
      *
     C                   Z-ADD     DSTIME        MITIME
     C                   Z-ADD     SYDATE        MIDT
     C                   MOVE      Q1PGM         MIPGM
     C                   MOVE      Q1USER        MIUSER
      *
     C                   WRITE     DBRCDF
      *
     C                   ENDIF                                                  *IN98=ON
      *
     C                   ENDIF                                                  MSMSG# GE 'OK00001'
      *
     C                   READP     DBRCDF                                 99
      *
     C                   ENDDO                                                  ENDDOW 99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for Other functions keys
     C*================================================================
     C*     Processing for F10 Function Key
     C     OTHFKS        BEGSR
     C     *INKJ         IFEQ      *ON
     C                   EXSR      KJDO
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*          Return function key settings and exit
     C*          ------------------------------------------------------
     C                   MOVE      *INKL         @@KL
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
      *
     C                   EXSR      MVSCDB
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR
     O*
     O*----------------------------------------------------------------
     O* Record releases
     O*----------------------------------------------------------------
