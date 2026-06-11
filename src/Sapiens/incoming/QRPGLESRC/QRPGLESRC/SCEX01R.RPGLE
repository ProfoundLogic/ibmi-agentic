     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SCEX01R - AUTO FOLDER BUILD - DOCUMENT
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program is use EXCLUSIVELY to process PDS Darts       *
     H*    remarks. Other programs handle external applications.      *
     H*    The program automatically creates a folder and document    *
     H*    structure to hold an attached letter/memo for the DARTS    *
     H*    remark begin added.                                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    SCEXITR - Auto Folder Build                                *
     H*  CALLS TO:                                                    *
     H*    SRPBFL - Provider Folder Build Program                     *
     H*    SRFBLD - Folder Build Program                              *
     H*    SRDBLD - Document Build Program                            *
     H*                                                               *
     H*  CREATED:  9/18/97  PROGRAMMER: C:09  PROJECT: WC676          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H***       9/13/00   128    Project:81WRK
     H*                    Changed the name of this program from:      S
     H*                    SCEXITR to SCEX01R to allow auto folder
     H*                    creation for external applications.        *
     H*                    SCEXITR is now a program that just directs *
     H*                    the job stream to the correct creation pgm.*
     H*                                                               *
     H***       7/25/98    02    SUPPORT:12611
     H*                    ADDED MICROSOFT SYS CTL FLAG, IF .DOC OR .XLS
     H*                    THEN SPECIAL BUILD/EDIT DOCUMENT
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60 - General CHAIN Indicator                                 *
     H*  65 - General CHAIN Indicator                                 *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*****************************************************************
     F*** BEGIN ADD *** 07/27/98 **************************************
     FSSYLF     IF   E           K DISK
     F*** END ADD *** 07/27/98 ****************************************

     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*  Data Structure to Break up key for Employer Master (W0050R)
     D*
     D                 DS
     D  W0050R                 1     40
     D  CO#050                 1      3
     D  FND050                 4      6
     D  EMP050                 7     15
     D  DIV050                16     20
     D*
     D*  Data Structure to Break up key for Claims Master (W0070R)
     D*
     D                 DS
     D  W0070R                 1     40
     D  CO#070                 1      3
     D  FND070                 4      6
     D  FYR070                 7      9
     D  CAS070                10     16
     D*
     D*  Data Structure to Break up key for Agent Master  (W0040R)
     D*
     D                 DS
     D  W0040R                 1     40
     D  FEI040                 1      9
     D  MOD040                10     12
     D*
     D*  Data Structure to Break up key for Reinsurance Policy (W0330R)
     D*
     D                 DS
     D  W0330R                 1     40
     D  CO#330                 1      3
     D  FND330                 4      6
     D  FYR330                 7      9
     D  RDT330                10     15
     D  CAS330                16     22
     D  MLT330                23     28
     D*
     D*  Data Structure to Break up key for WCMS Prov Master (W0030R)
     D*
     D                 DS
     D  W0030R                 1     40
     D  FID301                 1      6
     D  FID302                 7      9
     D  MOD30                 10     12
     D*
     D*  Data Structure to Break up key for WCMC Prov Master (F0031R)
     D*
     D                 DS
     D  F0031R                 1     40
     D  FID311                 1      6
     D  FID312                 7      9
     D  MOD31                 10     12
     D*
     D*  Data Structure to Break up key for P&C Claim Master (C0200R)
     D*
     D                 DS
     D  C0200R                 1     40
     D  CO#200                 1      3
     D  FND200                 4      6
     D  CAS200                 9     15
     D*
     D*  External Program Data Structures
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Define Work Fields
     C*
     C*** BEGIN ADD *** 07/27/98 **************************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     F*** END   ADD *** 07/27/98 **************************************
     C*  Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQCODE            2
     C                   PARM                    QQDFLD           40
     C                   PARM                    QQDCID           12
     C                   PARM                    QQERR             1
     C*** ADD 7/25/98 ****** MICROSOFT SYS CTL FLAG ***
     C                   PARM                    QQMS              1
     C*
     C                   MOVE      QQKEY         XXKEY            40
     C                   MOVE      *BLANKS       XXPNAM            6
     C                   MOVEL     QQPNAM        XXPNAM
     C                   MOVE      QQCODE        XXCODE            2
     C*** BEGIN ADD *** 07/27/98 **************************************
     C                   MOVEL     'SC'          XXSYK
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRFL06        QQMS
     C                   ENDIF
     C*** END ADD *** 07/27/98 ****************************************
     C*
     C*  Determine if this is a valid program from which auto folder
     C*  build can be executed
     C*
     C     XXPNAM        IFNE      'W0050R'
     C     XXPNAM        ANDNE     'W0070R'
     C     XXPNAM        ANDNE     'W0040R'
     C     XXPNAM        ANDNE     'W0330R'
     C     XXPNAM        ANDNE     'W0030R'
     C     XXPNAM        ANDNE     'F0031R'
     C     XXPNAM        ANDNE     'C0200R'
     C                   MOVE      '1'           QQERR
     C                   GOTO      TERM
     C                   END
     C*
     C                   MOVE      *BLANKS       SRFND             8
     C                   MOVE      *BLANKS       SRSER             8
     C                   MOVE      *BLANKS       SRSUB             8
     C                   MOVE      *BLANKS       SRFOLD            8
     C                   MOVE      *BLANKS       SRFPTH           40
     C                   MOVE      *BLANKS       SRTERM            1
     C*
     C*  Use the incoming program name to determine format of folder
     C*  to be built
     C*
     C*  W0050R = FFF/'PS'/SS/MEEEEE
     C*    F=Fund, SS=Sub-area code, E=Employer #
     C*
     C     XXPNAM        IFEQ      'W0050R'
     C                   MOVE      *BLANKS       W0050R
     C                   MOVEL     XXKEY         W0050R
     C                   MOVEL     FND050        SRFND
     C                   MOVEL     'PS'          SRSER
     C                   MOVEL     XXCODE        SRSUB
     C                   MOVEL     'M'           SRFOLD
     C                   MOVE      EMP050        XXWK70            7 0
     C                   MOVE      XXWK70        SRFOLD
     C                   END                                                    W0050R END
     C*
     C*  W0070R = FFF/'CS'/SS/CAAAAAAA
     C*    F=Fund, SS=Sub-area code, A=Case (Claim) #
     C*
     C     XXPNAM        IFEQ      'W0070R'
     C                   MOVE      *BLANKS       W0070R
     C                   MOVEL     XXKEY         W0070R
     C                   MOVEL     FND070        SRFND
     C                   MOVEL     'CS'          SRSER
     C                   MOVEL     XXCODE        SRSUB
     C                   MOVEL     'C'           SRFOLD
     C                   MOVE      CAS070        SRFOLD
     C                   END                                                    W0070R END
     C*
     C*  W0330R = FFF/'CS'/SS/CSSSSSSS
     C*    F=Fund, SS=Sub-area code, S=Specific Filing Case (Claim) #
     C*
     C     XXPNAM        IFEQ      'W0330R'
     C                   MOVE      *BLANKS       W0330R
     C                   MOVEL     XXKEY         W0330R
     C                   MOVEL     FND330        SRFND
     C                   MOVEL     'CS'          SRSER
     C                   MOVEL     XXCODE        SRSUB
     C                   MOVEL     'C'           SRFOLD
     C                   MOVE      CAS330        SRFOLD
     C                   END                                                    W0330R END
     C*
     C*  W0030R = PFFFFFFFFF/MLLL
     C*  F0031R = PFFFFFFFFF/MLLL
     C*  W0040R = PFFFFFFFFF/MLLL
     C*    F=Federal ID #, L=Modifier
     C*
     C     XXPNAM        IFEQ      'W0030R'
     C     XXPNAM        OREQ      'F0031R'
     C     XXPNAM        OREQ      'W0040R'
     C                   MOVE      *BLANKS       W0030R
     C                   MOVEL     XXKEY         W0030R
     C                   MOVEL(P)  'P'           SRFEIN           12
     C                   CAT       FID301:0      SRFEIN
     C                   CAT       '.':0         SRFEIN
     C                   CAT       FID302:0      SRFEIN
     C                   MOVE      *BLANKS       SRMOD             4
     C                   MOVEL     'M'           SRMOD
     C                   MOVE      MOD30         SRMOD
     C                   MOVEL     XXCODE        SRSUB
     C                   END                                                    W0030R END
     C*                                                    F0031R
     C*
     C*  C0200R = FFF/'CS'/SS/CSSSSSSS
     C*    F=Fund, SS=Sub-area code, A=Case (Claim) #
     C*
     C     XXPNAM        IFEQ      'C0200R'
     C                   MOVE      *BLANKS       C0200R
     C                   MOVEL     XXKEY         C0200R
     C                   MOVEL     FND200        SRFND
     C                   MOVEL     'CS'          SRSER
     C                   MOVEL     XXCODE        SRSUB
     C                   MOVEL     'C'           SRFOLD
     C                   MOVE      CAS200        SRFOLD
     C                   END                                                    C0200R END
     C*
     C     XXPNAM        IFEQ      'W0030R'
     C     XXPNAM        OREQ      'F0031R'
     C     XXPNAM        OREQ      'W0040R'
     C                   CALL      'SRPBFL'
     C                   PARM                    SRFEIN
     C                   PARM                    SRMOD
     C                   PARM                    SRFPTH
     C                   PARM                    SRTERM
     C                   ELSE
     C*
     C*  Call subprogram to build folder
     C*
     C                   CALL      'SRFBLD'
     C                   PARM                    SRFND
     C                   PARM                    SRSER
     C                   PARM                    SRSUB
     C                   PARM                    SRFOLD
     C                   PARM                    SRFPTH
     C                   PARM                    SRTERM
     C                   END                                                    W0030R/F0031R
     C*
     C*  If SRTERM is <> *blanks, then folder could not be built
     C*
     C     SRTERM        IFNE      *BLANKS
     C                   MOVE      '2'           QQERR
     C                   GOTO      TERM
     C                   END
     C*
     C*  Move built folder to screen default folder
     C*
     C                   MOVE      *BLANKS       QQDFLD
     C                   MOVEL     SRFPTH        QQDFLD
     C*
     C*  Now use the folder to build a new document
     C*
     C                   MOVE      *BLANKS       SRDOC            12
     C                   MOVE      *BLANKS       SRTERM
     C                   CALL      'SRDBLD'
     C                   PARM                    SRFPTH
     C                   PARM                    SRSUB
     C                   PARM                    SRDOC
     C                   PARM                    SRTERM
     C*** ADD 7/25/98 ****** MICROSOFT SYS CTL FLAG *** (THIS FLAG IS
     C******************** PARM           QQMS            RTVD IN DBLD)
     C*
     C     SRTERM        IFNE      *BLANKS
     C                   MOVE      '3'           QQERR
     C                   GOTO      TERM
     C                   END
     C*
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVEL     SRDOC         QQDCID
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
