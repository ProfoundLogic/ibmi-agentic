     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SCEX50R - PMSC - POLICY - AUTO FOLDER/DOCUMENT BUILD
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    This program is use EXCLUSIVELY to process PMSC Darts      *
      *    remarks. Other programs handle external applications.      *
      *    The program automatically creates a folder and document    *
      *    structure to hold an attached letter/memo for the DARTS    *
      *    remark begin added.                                        *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    SCEXITR - Auto Folder Build                                *
      *  CALLS TO:                                                    *
      *    SRFBLDX - Folder Build Program                             *
      *    SRDBLD - Document Build Program                            *
      *                                                               *
      *  CREATED: 09/13/00  PROGRAMMER: C:128  PROJECT: 81WRK         *
      *****************************************************************
      *  REVISIONS:                                                   *
      *        XX/XX/XX   C:XXX    Project:XXXXX
      *                                                               *
      *****************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  60 - General CHAIN Indicator                                 *
      *  65 - General CHAIN Indicator                                 *
      *****************************************************************
      *  FILE USAGE:                                                  *
      *                                                               *
      *****************************************************************
     FSSYLF     IF   E           K DISK
      *
      * System Date and Time Data Structure

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      *  Data Structure to Break up key for Policy Master (PMSCPOL)
      *
     D                 DS
     D  POLICY                 1     40
     D  LOC050                 1      2
     D  MCO050                 3      4
     D  SYM050                 5      7
     D  POL050                 8     14
     D  POLSIX                 9     14
     D  MOD050                15     16
      *
      *  External Program Data Structures
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  Define Work Fields
      *
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *

     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *  Entry Parameters
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQCODE            2
     C                   PARM                    QQDFLD           40
     C                   PARM                    QQDCID           12
     C                   PARM                    QQERR             1
     C                   PARM                    QQMS              1
      *
     C                   MOVE      QQKEY         XXKEY            40
     C                   MOVE      *BLANKS       XXPNAM           10
     C                   MOVEL     QQPNAM        XXPNAM
     C                   MOVE      QQCODE        XXCODE            2
      *
     C                   MOVEL     'SC'          XXSYK
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRFL06        QQMS
     C                   ENDIF
      *
      *
      *  Determine if this is a valid program from which auto folder
      *  build can be executed
      *
     C     XXPNAM        IFNE      'PMSPOL'
     C                   MOVE      '1'           QQERR
     C                   GOTO      TERM
     C                   END
      *
     C                   MOVE      *BLANKS       SRFND             8
     C                   MOVE      *BLANKS       SRSER             8
     C                   MOVE      *BLANKS       SRSUB             8
     C                   MOVE      *BLANKS       SRFOLD           12
     C                   MOVE      *BLANKS       SRFPTH           40
     C                   MOVE      *BLANKS       SRTERM            1
      *
      *  Format the folder structure to be built
      *
      *  PMSCPOL = 'PMSC'/'PS'/SS/MMXXXXXX.SY
      *  SS=Sub-area code  SY=Symbol MM=Mod
      *  XXXXXX-Policy number (right 6 digits)
      *
     C                   MOVE      *BLANKS       POLICY
     C                   MOVEL     XXKEY         POLICY
     C                   MOVEL     'PMSC'        SRFND
     C                   MOVEL     'PS'          SRSER
     C                   MOVEL     XXCODE        SRSUB
     C                   CAT       MOD050:0      SRFOLD
     C                   CAT       POLSIX:0      SRFOLD
     C                   CAT       '.':0         SRFOLD
     C                   CAT       SYM050:0      SRFOLD
      *
      *
      *  Call subprogram to build folder
      *
     C                   CALL      'SRFBLDX'
     C                   PARM                    SRFND
     C                   PARM                    SRSER
     C                   PARM                    SRSUB
     C                   PARM                    SRFOLD
     C                   PARM                    SRFPTH
     C                   PARM                    SRTERM
      *
      *  If SRTERM is <> *blanks, then folder could not be built
      *
     C     SRTERM        IFNE      *BLANKS
     C                   MOVE      '2'           QQERR
     C                   GOTO      TERM
     C                   END
      *
      *  Move built folder to screen default folder
      *
     C                   MOVE      *BLANKS       QQDFLD
     C                   MOVEL     SRFPTH        QQDFLD
      *
      *  Now use the folder to build a new document
      *
     C                   MOVE      *BLANKS       SRDOC            12
     C                   MOVE      *BLANKS       SRTERM
     C                   CALL      'SRDBLD'
     C                   PARM                    SRFPTH
     C                   PARM                    SRSUB
     C                   PARM                    SRDOC
     C                   PARM                    SRTERM
      *
     C     SRTERM        IFNE      *BLANKS
     C                   MOVE      '3'           QQERR
     C                   GOTO      TERM
     C                   END
      *
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVEL     SRDOC         QQDCID
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
