     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SCEXITR - AUTO FOLDER BUILD - DIRECTOR
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    The purpose of this program has been changed from creating *
     H*    auto folders to directing the job stream to a program      *
     H*    that will actually create auto folders.                    *
     H*                                                               *
     H*    This program is called from using F20=Usr Exit on the      *
     H*    "Add a Remark" (F11 from "Work with Remarks") in program   *
     H*    SC010R.  The called program automatically creates a folder *
     H*    and document to hold an attached letter/memo for the DARTS *
     H*    remark begin added.                                        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    SC010R using F20 from "Add a Remark" screen (SCR01)        *
     H*  CALLS TO:                                                    *
     H*    SCEX01R- PDS applications -ONLY- Build Program             *
     H*    SREXnnR- External application Build Program                *
     H*    SRDBLD - Document Build Program                            *
     *                                                                        *
     * CREATED   : 09/13/2000                                                 *
     * PROGRAMMER: C:128                                                      *
     * PROJECT   : 81WRK                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * xx/xx/xxxx C:xxx PROJECT: xxxxx                                        *
     *                                                                        *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *                                                                        *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60 - General CHAIN Indicator                                 *
     H*  65 - General CHAIN Indicator                                 *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*****************************************************************
     FSMKTL1    IF   E           K DISK

     D*
     D*
     D*  External Program Data Structures
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C*   Key List for the Key Type file
      *
      *  Program call plist
      *
     C     PGMLST        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQCODE            2
     C                   PARM                    QQDFLD           40
     C                   PARM                    QQDCID           12
     C                   PARM                    QQERR             1
     C                   PARM                    QQMS              1
     C*
     C     KTKY          KLIST
     C                   KFLD                    QQPNAM
     C*  Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQPNAM           10
     C                   PARM                    QQCODE            2
     C                   PARM                    QQDFLD           40
     C                   PARM                    QQDCID           12
     C                   PARM                    QQERR             1
     C                   PARM                    QQMS              1
     C*
     C                   MOVE      QQKEY         XXKEY            40
     C                   MOVE      *BLANKS       XXPNAM           10
     C                   MOVEL     QQPNAM        XXPNAM
     C                   MOVE      QQCODE        XXCODE            2
      *
      *  If the field KTRSCA in the file SMKTL1 has a value in it,
      *  the value is a program that will build a folder structure
      *  for a external application. If the field is blank, call
      *  the PDS build program.
      *
     C     KTKY          CHAIN     PKT01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   SELECT
     C     KTSRCA        WHENEQ    *BLANKS
     C                   MOVE      *BLANKS       PGMNAM           10
     C                   MOVEL     'SCEX01R'     PGMNAM
     C     KTSRCA        WHENNE    *BLANKS
     C                   MOVE      *BLANKS       PGMNAM
     C                   MOVEL     KTSRCA        PGMNAM
     C                   ENDSL
      *
     C                   CALL      PGMNAM        PGMLST
      *
     C                   ELSE
     C                   MOVE      '1'           QQERR
     C                   ENDIF
      *
     C*
     C                   MOVE      *ON           *INLR
