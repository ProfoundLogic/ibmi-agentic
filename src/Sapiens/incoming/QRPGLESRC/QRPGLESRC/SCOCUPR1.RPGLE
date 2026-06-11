     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *TITLE SCOCUPR1 - UPDATE OPTICAL PROCESS INFO IN OC FILE        *
      *****************************************************************
      *    PROGRAM DESCRIPTION:                                       *
      *        BACKUP AND BATCH - Check and update the optical        *
      *                           process status in the Optical       *
      *                           Control File                        *
      *                                                               *
      *  CALLED FROM:                                                 *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED: 03/31/03    PROGROGRAMMER: C:145    PROJECT: 124IAA *
      *                                                               *
      *     RTN CODES:                                                *
      *                 1 -  Can't perform Copy Process               *
      *                 2 -  Can't perform Delete Process             *
      *****************************************************************
      ** REVISIONS:                                                   *
      *  CREATED:             PROGRAMMER:             PROJECT:        *
      *                                                               *
      *    03/31/03           C:145                   124IAA          *
      *    Original SCOCUPR1 included information for the select      *
      *    process, but since the batch process allows for multiple   *
      *    selections of date ranges, this functionality is not       *
      *    necessary.                                                 *
      ******************************************************************
      * INDICATOR USAGE                                                *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *  NOTES - Following is the order for Copy and Delete Options    *
      *
      *   OTCPYF | OTDELF
      *   ---------------
      *      S       N      Copy in Progress
      *      Y       N      Copy Complete
      *      Y       S      Delete in Progress                         *
      *      Y       Y      Delete Complete
      *                                                                *
      ******************************************************************
      *
     FSMOTL     UF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  FLAGS                  1      2
     D  OTCPYF                 1      1
     D  OTDELF                 2      2
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     OTKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *ENTRY        PLIST
     C                   PARM                    PROCES            1            C,D (COPY,DEL)
     C                   PARM                    BFRAFT            1            B,A (BEFORE,AFTER)
     C                   PARM                    RTNCDE            1
      *
     C                   MOVE      'G'           QQOSYS
     C                   MOVEL     'OPTMLB01'    QQLIBN
      *
      *  Current date/time
      *
     C                   TIME                    DSDTTM
      *
     C     OTKY          CHAIN     POT01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C     PROCES        CASEQ     'C'           COPY
     C     PROCES        CASEQ     'D'           DELETE
     C                   ENDCS
      *
      *  Update the OT record with Process tracking values
      *
     C                   UPDATE    POT01
      *
     C                   ELSE                                                   NO HIT ON SMOTP
      *  Error
     C                   ENDIF                                                  END *IN62 = 0
      *
     C                   MOVE      *ON           *INLR
      *
      *================================================================***
      *   COPY   SUBROUTINE                                            ***
      *================================================================***
      *
     C     COPY          BEGSR
      *
     C                   SELECT
     C     BFRAFT        WHENEQ    'B'
      *
      *  For the before Copy
      *    If Del process is done ('YY') and Copy process is either not started,
      *    started ('YY') or still processing ('SN'), mark the flag as
      *    started for Copy.                                           g as
      *
     C     FLAGS         IFEQ      'SN'
     C     FLAGS         OREQ      'YY'
     C                   MOVE      'S'           OTCPYF
     C                   ELSE
      *
      *  Error can't run the copy process
      *
     C                   MOVEL     '2'           RTNCDE
     C                   ENDIF
      *
      *  After the Copy process
      *
     C     BFRAFT        WHENEQ    'A'
      *
     C     FLAGS         IFEQ      'SN'
     C     FLAGS         OREQ      'YY'
     C     FLAGS         OREQ      'SY'
     C                   MOVE      'N'           OTDELF
     C                   ENDIF
      *
     C     FLAGS         IFEQ      'YS'
     C                   MOVE      'Y'           OTDELF
     C                   ENDIF
      *
      *  Mark the copy as complete
      *
     C                   MOVE      'Y'           OTCPYF
      *
      *  Use today's Date as Copy completion Date
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OTLCPY
      *
     C                   OTHER
      *
      *  Error if not "B" or "A"
      *
     C                   ENDSL
      *
      *
     C     ENDCPY        ENDSR
      *
      *================================================================***
      *   DELETE SUBROUTINE                                            ***
      *================================================================***
      *
     C     DELETE        BEGSR
      *
      *
     C                   SELECT
     C     BFRAFT        WHENEQ    'B'
      *
      *  For the Before delete
      *    If Copy process is done ('YN') and the delete process is not
      *    started ('YN') or still processing ('YS'), mark the flag as
      *    started for Deletion.                                       g as
      *
     C     FLAGS         IFEQ      'YN'
     C     FLAGS         OREQ      'YS'
     C                   MOVE      'S'           OTDELF
     C                   ELSE
      *
      *  Error can't run the delete process
      *
     C                   MOVEL     '3'           RTNCDE
     C                   ENDIF
      *
     C     BFRAFT        WHENEQ    'A'
      *
      *  Update the OTDELF to reset values
      *
     C     FLAGS         IFEQ      'YN'
     C     FLAGS         OREQ      'YS'
     C                   MOVE      'Y'           OTCPYF
     C                   ENDIF
      *
     C                   MOVE      'Y'           OTDELF
      *
      *  Today's Date as Copy completion Date
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OTLDEL
      *
     C                   OTHER
      *
      *  Error if not "B" or "A"
      *
     C                   ENDSL
      *
     C     ENDDEL        ENDSR
      *
