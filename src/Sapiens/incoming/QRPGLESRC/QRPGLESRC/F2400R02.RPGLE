     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F2400R02 - ROLL OVER STATE RULES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program copies the rules of a particular           **
     H**     state and effective date into the same or another       **
     H**     state and/or effective date.                            **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F2400R - Work with State Rules                          **
     H**   CALLS TO:                                                 **
     H**   PROGRAMMER: C:110                                         **
     H**   CREATED:   08/12/98      PROJECT: FS138                   **
     H*****************************************************************
     H*                                                              **
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  04/02/08 C:198                                              **
     H*                PROJ: TX040                                   **
     H*                Changed program to roll over only active      **
     H*                rules to new effective date.                  **
     H*                                                              **
     H*  01/04/00 C:79 Proj: MS008                                   **
     H*                1. Do not reinitialize rule number.           **
     H*                                                              **
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
     H**      FDFRP  (DBRCDF) State Rules                            **
     H**      STABL  (PST01)  System Wide State Code Table (KEY)     **
     H**      FTGTL  (PGT01)  Generic Table File                     **
     H*****************************************************************
     H*
     FF2400F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FSTABL     IF   E           K DISK
     FFDFRP     UF A E             DISK
     F                                     RENAME(PFR01:DBRCDF)
     FFDFRL2    IF   E           K DISK
     F                                     RENAME(PFR01:PFR02)
     FFDFRL7    IF   E           K DISK
     F                                     RENAME(PFR01:PFR03)
     FFDFRL8    IF   E           K DISK
     F                                     RENAME(PFR01:PFR04)
     FFDFRL9    IF   E           K DISK
     F                                     RENAME(PFR01:PFR05)
     FFDFRL6    IF   E           K DISK
     F                                     RENAME(PFR01:PFR06)
     FFTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DESC            S             70    DIM(5)                               Text Lines
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Roll Over State Rule-
     D                                     s')
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
     C*  Key list for FTGTL  Generic Table List
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    QQID
     C                   KFLD                    QQKEY
     C*
     C*  Key List for STABL (PST01)  Sys. Wide State Code Table (KEY)
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*  Key Lists for FDFRP: State Fee Schedule Rules
     C*
     C*  Key list for FDFRL2
     C*
     C     FRKY4         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKRULE
     C*
     C*  Key List for FDFRL6
     C*
     C     FRKY          KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKPCAT
     C                   KFLD                    KKRULE
     C*  Key list for FDFRL7
     C*
     C     FRKY1         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKRULE
     C*
     C*  Key list for FDFRL8
     C*
     C     FRKY2         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C*
     C*  Key list for FDFRL9
     C*
     C     FRKY3         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKPCAT
     C*** BEG ADD *** 01/04/00 ****************************************
     C*  Key list for FDFRL9
     C*
     C     FRKY5         KLIST
     C                   KFLD                    KKST
     C*** END ADD *** 01/04/00 ****************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    FRST          KKST
     C     *LIKE         DEFINE    FRRULE        XXRULE
     C     *LIKE         DEFINE    FREFDT        KKEFDT
     C     *LIKE         DEFINE    FRPCAT        KKPCAT
     C     *LIKE         DEFINE    FRRULE        KKRULE
     C     *LIKE         DEFINE    GTTBID        QQID
     C     *LIKE         DEFINE    GTKEY         QQKEY
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
     C                   SETOFF                                       373839
     C                   SETOFF                                       404142
     C                   SETOFF                                       434445
     C                   SETOFF                                       464748
     C*****************************************************************
     C*
     C*  Verify From State is a Valid Entry
     C*
     C  N90SCST          IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCST          KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C   99              SETON                                        9041
     C*
     C                   ENDIF
     C*
     C*    CHECK FOR BLANK ENTRY IN STATE FIELD
     C*
     C  N90SCST          IFEQ      *BLANKS
     C*
     C                   SETON                                        4590
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*    Check Effective Date for Validity and Blank
     C*
     C  N90SCFRDT        IFEQ      *ZEROS
     C*
     C                   SETON                                        3790
     C*
     C                   ENDIF
     C*
     C  N90SCFRDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        9037                  R
     C*
     C                   ENDIF                                                     END SRERR
     C*
     C                   ENDIF                                                     END NE*BLANK
     C*****************************************************************
     C*
     C*    Verify Category is not Blank and is Valid
     C*
     C  N90SCPCAT        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCPCAT        QQKEY
     C                   MOVEL(P)  'PCAT'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C   60              SETON                                        9038
     C*
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C  N90SCRULE        IFNE      0
     C*
     C                   MOVE      SCTST         KKST
     C                   Z-ADD     SCRULE        KKRULE
     C*
     C     FRKY4         CHAIN     PFR02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9039
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  FRDESC        SCRULD
     C*
     C                   ENDIF
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
     C     SCTFDT        OREQ      *ZEROS
     C*
     C                   SETON                                        4690
     C                   ENDIF
     C*
     C*
     C* Check if Rule Already Exists
     C*
     C* III) Rules don't exist for this effective date.
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   Z-ADD     XXEFDT        KKEFDT
     C*
     C                   MOVE      SCST          KKST
     C*
     C  N90SCRULE        IFNE      *ZERO
     C                   Z-ADD     SCRULE        KKRULE
     C*
     C     FRKY1         CHAIN     PFR03                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        9044
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C  N90SCRULE        IFEQ      *ZEROS
     C*
     C                   MOVE      *HIVAL        KKRULE
     C*
     C     FRKY1         SETGT     PFR03
     C                   READP     PFR03                                  99
     C*
     C     XXEFDT        IFNE      FREFDT
     C     SCST          ORNE      FRST
     C*
     C                   SETON                                        9044
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Condition I) If rule is entered on screen
     C*
     C* Convert effective date to YYMMDD format
     C*
     C                   Z-ADD     SCTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
     C                   Z-ADD     XXEFDT        KKEFDT
     C*
     C                   MOVE      SCTST         KKST
     C*
     C  N90SCRULE        IFNE      *ZERO
     C                   Z-ADD     SCRULE        KKRULE
     C*
     C     FRKY1         CHAIN     PFR03                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   SETON                                        9043
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  II) If no rule was entered on screen
     C*
     C  N90SCRULE        IFEQ      *ZEROS
     C*
     C                   MOVE      *HIVAL        KKRULE
     C*
     C     FRKY1         SETGT     PFR03
     C                   READP     PFR03                                  99
     C*
     C     XXEFDT        IFEQ      FREFDT
     C     SCTST         ANDEQ     FRST
     C*
     C                   SETON                                        9043
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*    Check To Effective Date for Validity and Blank
     C*
     C  N90SCTFDT        IFEQ      *ZEROS
     C*
     C                   SETON                                        9040
     C*
     C                   ENDIF
     C*
     C  N90SCTFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        9040                  R
     C*
     C                   ENDIF                                                     END SRERR
     C*
     C                   ENDIF                                                     END NE*BLANK
     C*
     C     *IN43         IFEQ      *ON
     C     SCFRDT        ANDNE     SCTFDT
     C*
     C     *IN43         OREQ      *ON
     C     SCST          ANDNE     SCTST
     C*
     C                   EXFMT     WDWRUL
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   SETOFF                                       9043
     C*
     C                   ENDIF
     C*
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
     C*************************************
     C* SCR01 - PROCEDURE CATEGORY SEARCH *
     C*************************************
     C*
     C     HFLD          WHENEQ    'SCPCAT'
     C*
     C                   MOVE      'PCAT'        QQID
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS            4 0
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C* F3=Exit pressed
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   SETON                                          3390
     C                   ENDIF
     C*
     C* F12=Cancel or no value selected
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCPCAT        OREQ      *BLANKS
     C*
     C                   MOVE      *ON           *INKL
     C*
     C                   SETON                                        3590
     C*
     C                   ENDIF                                                  QQRTCD= 'B'
     C*
     C* Value was selected
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C                   MOVEL(P)  QQKEY         SCPCAT
     C*
     C                   SETON                                        3490
     C*
     C*
     C                   ENDIF                                                  QQRTCD= '*'
     C*                                                   END STATE
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
     C                   ENDIF                                                  HFLD = SCST
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
     C                   MOVE      SCAST         SCST
     C                   SETON                                        3290
     C*
     C                   ELSE
     C*
     C                   MOVE      SCAST         SCTST
     C                   SETON                                        3690
     C*
     C                   ENDIF                                                  HFLD = SCST
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
     C                   MOVEL(P)  *BLANKS       SCST
     C                   MOVEL(P)  *ZEROS        SCFRDT
     C                   MOVEL(P)  *BLANKS       SCPCAT
     C                   MOVEL(P)  *ZEROS        SCRULE
     C                   MOVEL(P)  *BLANKS       SCTST
     C                   MOVEL(P)  *ZEROS        SCTFDT
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*    Find starting rule number from FDFRP for sequencing
     C*
     C*    Convert date to YYMMDD Format
     C*
     C                   Z-ADD     SCTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C*          Load Key Fields
     C*          ------------------------------------------------------
     C                   MOVE      SCTST         KKST
     C                   Z-ADD     XXEFDT        KKEFDT
      *
     C*** BEG CHG *** 01/04/00 ****************************************
     C********** FRKY2     SETGTPFR03
     C********** FRKY2     REDPEPFR03                    99
     C     FRKY5         SETGT     PFR03
     C     FRKY5         READPE    PFR03                                  99
     C*** BEG CHG *** 01/04/00 ****************************************
      *
     C     *IN99         IFEQ      *OFF
      *
     C     FRRULE        ADD       1             XXRULE
      *
     C                   ELSE
      *
     C                   Z-ADD     1             XXRULE
      *
     C                   ENDIF
     C*
     C*    Convert date to YYMMDD Format
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   Z-ADD     XXEFDT        KKEFDT
     C*
     C*          Load Key Fields
     C*          ------------------------------------------------------
     C                   MOVE      SCST          KKST
     C                   MOVE      SCPCAT        KKPCAT
     C                   MOVE      SCRULE        KKRULE
      *****************************************************************
     C*
     C                   SELECT
     C*
     C* Condition I: When Rule and Category are Not Entered
     C*
     C     SCRULE        WHENEQ    *ZERO
     C     SCPCAT        ANDEQ     *BLANKS
     C*
     C     FRKY2         CHAIN(N)  PFR04                              99
     C*
     C* Condition II: When Rule and Category are Entered
     C*
     C     SCRULE        WHENNE    *ZERO
     C     SCPCAT        ANDNE     *BLANKS
     C*
     C     FRKY          CHAIN     PFR06                              99
     C*
     C* Condition III: When Category is Entered but Rule is Not
     C*
     C     SCPCAT        WHENNE    *BLANKS
     C     SCRULE        ANDEQ     *ZERO
      *
     C     FRKY3         CHAIN     PFR05                              99
      *
     C* Condition IV: When Rule is Entered but Category is Not
     C*
     C     SCRULE        WHENNE    *ZERO
     C     SCPCAT        ANDEQ     *BLANKS
      *
     C     FRKY1         CHAIN     PFR03                              99
      *
     C                   ENDSL
     C************************************************************
      *
     C     *IN99         DOWEQ     *OFF
     C*
      *  MOVE ENTRY FIELDS INTO PHYSICAL FILE FIELDS
      *
     C*
     C**** BEG ADD **** 04/02/08 **************************************
     C     FRSTAT        IFEQ      'A'
     C**** END ADD **** 04/02/08 **************************************
     C*    Convert date to YYMMDD Format
     C*
     C                   Z-ADD     SCTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   Z-ADD     XXEFDT        FREFDT
     C*
     C                   MOVE      SCTST         FRST
     C                   TIME                    DSDTTM
      *****************************************************************
      * CONVERTS THE MDY TO YMD FORMAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
      *
     C                   Z-ADD     DSTIME        FRTIME
     C                   Z-ADD     SYDATE        FRDT
      *****************************************************************
      *      ADD UPDATE OF INITIAL MAINTENANCE FIELDS:
      *
     C                   MOVE      Q1USER        FRUSER
     C                   MOVE      Q1PGM         FRPGM
     C                   MOVE      Q1USER        FRIUSR
     C                   MOVE      Q1PGM         FRIPGM
     C                   MOVE      DSTIME        FRITIM
     C                   MOVE      SYDATE        FRIDT
      *
      *      Sequence rule numbers
      *
     C                   Z-ADD     XXRULE        FRRULE
     C                   ADD       1             XXRULE
      *****************************************************************
     C                   WRITE     DBRCDF
      *****************************************************************
     C**** BEG ADD **** 04/02/08 **************************************
     C                   ENDIF
     C**** END ADD **** 04/02/08 **************************************
      *
     C                   SELECT
      *
     C* Condition I: When Rule and Category are Not Entered
      *
     C     SCRULE        WHENEQ    *ZERO
     C     SCPCAT        ANDEQ     *BLANKS
      *
     C     FRKY2         READE(N)  PFR04                                  99
      *
      * Condition II: When Rule and Category are Entered
      *
     C     SCRULE        WHENNE    *ZERO
     C     SCPCAT        ANDNE     *BLANKS
      *
     C     FRKY          READE     PFR06                                  99
      *
      * Condition III: When Category is Entered but Rule is Not
      *
     C     SCPCAT        WHENNE    *BLANKS
     C     SCRULE        ANDEQ     *ZERO
      *
     C     FRKY3         READE     PFR05                                  99
      *
      * Condition IV: When Rule is Entered but Category is Not
     C*
     C     SCRULE        WHENNE    *ZERO
     C     SCPCAT        ANDEQ     *BLANKS
      *
     C     FRKY1         READE     PFR03                                  99
      *
     C                   ENDSL
      *
     C                   ENDDO
      *
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
