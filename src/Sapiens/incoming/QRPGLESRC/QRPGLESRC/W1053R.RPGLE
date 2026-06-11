     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE W1053R - Prompt for Endorsement table records copy program
      *===============================================================*
      * PROGRAM DESCRIPTION:                                          *
      *   This program prompts the user for info to copy Endorsement  *
      *   Table records.                                              *
      *                                                               *
      * CALLED FROM: W1055R                                           *
      * CALLS TO   : W1053C                                           *
      *                                                               *
      * CREATED    : 06/21/99                                         *
      * PROGRAMMER : C:113                                            *
      * PROJECT    : CM027                                            *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 01/06/03 C:61  PROJECT: CM027                                 *
      *                Correct endorsement prompt edit text.          *
      *                (Sreen file change).                           *
      *                                                               *
      * 11/10/99 C:113 PROJECT: SUPPORT                               *
      *                Changed so that it uses the group passed in,   *
      *                not the user parm group.                       *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
      * ------------------------------------------------------------- *
      *===============================================================*
      * INDICATOR USAGE:                                              *
      *  SCR01:                                                       *
      *   ERROR:                                                      *
      *   31 - (WCR0498) Invalid Group Number entered.                *
      *   32 - (WCR0513) Invalid table value found in field.          *
      *   33 - (WCR0513) Invalid table value found in field.          *
      *                                                               *
      *===============================================================*
      ** Display file definition
     FW1053F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      ** Physical file definition (for maintaining record)
     FWTMYP     UF   E             DISK
     F                                     RENAME(PMY01:DBRCDF)
      ** Policy Page Endorsement Table File
     FWTENL     IF   E           K DISK
      ** Midterm Endorsement Table File
     FWTMYL     IF   E           K DISK
      ** User Parameter Logical File
     FWMUPL     IF   E           K DISK
      ** Group Master Physical File
     FWMFNP     IF   E             DISK
      ** Group Master Logical File
     FWMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     F                                     INFDS(FNFDB)
      *===============================================================*
      *  Data Structure definitions
      *===============================================================*
      /COPY SCOPYMT01D
     D FNFDB           DS
     D  FNRRN                397    400B 0
      *
     D***PY SCOPYMT01I
      *
      *===============================================================*
      *  /COPY required constants
      *===============================================================*
     D OPTNM           C                   CONST('ENDORSEMENT TABLE CO-
     D                                     PY')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)
      *===============================================================*
      *  PLIST definitions
      *===============================================================*
      ** Entry parameters

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@PMF             1
      *
      ** Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      ** Report program parameter list
     C     CPYPL         PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQCPGP            3
     C                   PARM                    QQNWGP            3
     C                   PARM                    QQPMF             1
     C                   PARM                    QQFORM           15
     C                   PARM                    QQMID             3
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQMODE            1
      *===============================================================*
      *  KLIST definitions
      *===============================================================*
      *
      ** User Parameter file
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      ** Policy Page Endorsement Table file
     C     ENKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCPPE
      *
      ** Group Master file
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCNWGP
      *
      ** Midterm Endorsement Table file
     C     MYKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCMIDE
      *===============================================================*
      *  *LIKE DEFN statements
      *===============================================================*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    SCPPE         XXFORM
      *
     C/COPY SCOPYMT01C
      *
      *===============================================================*
      * DLTPRE - Pre-processing before delete occurs
      *===============================================================*
     C     DLTPRE        BEGSR
     C                   ENDSR
      *===============================================================*
      * DSPLYR - Processing after displaying information
      *===============================================================*
     C     DSPLYR        BEGSR
     C                   ENDSR
      *===============================================================*
      * ERRCK1 - Error check for screen 1
      *===============================================================*
     C     ERRCK1        BEGSR
      *
      ** Copy to group must be valid if present
     C     *IN90         IFEQ      *OFF
     C     SCNWGP        ANDNE     *ZEROS
      *
     C     FNKY          CHAIN     PFN02                              60
     C   60              SETON                                        31  90
      *
     C                   ENDIF                                                  SCCPGP != 0
      *
      ** Policy Page Endorsement must be valid if present
     C     *IN90         IFEQ      *OFF
     C     *IN20         ANDEQ     *ON
     C     SCPPE         ANDNE     *BLANKS
      *
     C     ENKY          CHAIN     PEN01                              60
     C   60              SETON                                        32  90
      *
     C                   ENDIF                                                  SCPPE != BLANKS
      *
      ** Midterm Endorsement must be valid if present
     C     *IN90         IFEQ      *OFF
     C     *IN21         ANDEQ     *ON
     C     SCMIDE        ANDNE     *ZEROS
      *
     C     MYKY          CHAIN     PMY01                              60
     C   60              SETON                                        33  90
      *
     C                   ENDIF                                                  SCMIDE != BLANKS
      *
     C                   ENDSR
      *===============================================================*
      * ERRCK2 - Error checking for screen 2 (SCR02)
      *===============================================================*
     C     ERRCK2        BEGSR
     C                   ENDSR
      *===============================================================*
      * ERRCK3 - Error checking for screen 3 (SCR03)
      *===============================================================*
     C     ERRCK3        BEGSR
     C                   ENDSR
      *===============================================================*
      * KJDO - SUBROUTINE FOR F10 (SEARCHES)
      *===============================================================*
     C     KJDO          BEGSR
      *
      *
     C                   ENDSR                                                  KJDO
      *===============================================================*
      * MVDBSC - Move data base fields to screen fields
      *===============================================================*
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     0             SCNWGP
     C                   MOVE      *BLANKS       SCPPE
     C                   Z-ADD     0             SCMIDE
      *
     C                   ENDSR
      *===============================================================*
      * MVSCDB - Move screen fields to data base fields
      *===============================================================*
     C     MVSCDB        BEGSR
      *
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCCPGP        QQCPGP
      *
     C     SCNWGP        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       QQNWGP
     C                   ELSE
     C                   MOVE      SCNWGP        QQNWGP
     C                   ENDIF
      *
     C                   MOVEL     SCPPE         QQFORM
      *
     C     SCMIDE        IFEQ      0
     C                   MOVE      *BLANKS       QQMID
     C                   ELSE
     C                   MOVE      SCMIDE        QQMID
     C                   ENDIF
      *
     C                   MOVE      UPOUTQ        QQOUTQ
      *
     C                   ENDSR
      *===============================================================*
      * OTHERR - Processing for other modes
      *===============================================================*
     C     OTHERR        BEGSR
     C                   ENDSR
      *===============================================================*
      * OTHFKS - Processing for other functions keys
      *===============================================================*
     C     OTHFKS        BEGSR
      *
      *
     C                   ENDSR
      *===============================================================*
      * PGMEND - End of program processing
      *===============================================================*
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
      *===============================================================*
      * PGMSTR - Start of program processing
      *===============================================================*
     C     PGMSTR        BEGSR
      *
      ** Get the User Parameter record
     C     UPKY          CHAIN     PUP01                              60
      *
      ** Get Group Master record
     C     @@RRN         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     FNFND         SCCPGP
      *** BEGIN ADD *** 11/10/99 ***********************************************
     C                   Z-ADD     FNFND         ##FND
      *** END   ADD *** 11/10/99 ***********************************************
     C                   ELSE
     C                   Z-ADD     999           SCCPGP
     C                   ENDIF
      *
      ** Set indicators for Policy Page or Midterm Endorsement
     C     @@PMF         IFEQ      'P'
      *
     C                   SETON                                        20
     C                   SETOFF                                       21
     C                   MOVE      @@PMF         QQPMF
      *
     C                   ELSE
      *
     C                   SETOFF                                       20
     C                   SETON                                        21
     C                   MOVE      'M'           QQPMF
      *
     C                   ENDIF
      *
     C                   MOVE      'D'           QQMODE
      *
     C                   ENDSR
      *===============================================================*
      * PREOTH - Pre-processing for other modes
      *===============================================================*
     C     PREOTH        BEGSR
     C                   ENDSR
      *===============================================================*
      * UPDATR - Update data base record and after processing
      *===============================================================*
     C     UPDATR        BEGSR
      *
     C                   EXSR      MVSCDB
      *
      ** Call processing program
     C                   CALL      'W1053C'      CPYPL
      *
     C                   ENDSR
      *===============================================================*
      * WRITER - Write data base record and after processing
      *===============================================================*
     C     WRITER        BEGSR
     C                   ENDSR
