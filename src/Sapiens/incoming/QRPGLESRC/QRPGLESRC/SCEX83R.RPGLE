     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SCEX83R - SUMMIT SUBMISSIONS AUTO FOLDER BUILD
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    This program is use EXCLUSIVELY to process SUMMIT DARTS    *
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
      *  Data Structure to Break up key for SUBMISSIONS    (UND330)
      *
     D                 DS
     D  UND330                 1     40
     D  SUB                    1      5
     D  SEQ                    6      7
     D  ST                     8      9
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
     C     XXPNAM        IFNE      'UND330'
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
      *  PMSCPOL = 'SUBM'/'CS'/SS/XXXXX
      *     SS=Sub-area code  XXXXXXX=SUBMISSION NUMBER *CAT STATE
      *
      *
     C                   MOVE      *BLANKS       UND330
     C                   MOVEL     XXKEY         UND330
     C                   MOVEL     'SUBM'        SRFND
     C                   MOVEL     'CS'          SRSER
     C                   MOVEL     XXCODE        SRSUB
     C     SUB           CAT(P)    ST:0          SRFOLD
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
