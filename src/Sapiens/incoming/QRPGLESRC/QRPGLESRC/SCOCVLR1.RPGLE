     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *   SCOCVLR1 Validates the Optical Purge Status.                *
      *****************************************************************
      *                                                               *
      *     RTN CODES:                                                *
      *                 1 -  Can't perform Copy Process               *
      *                 2 -  Can't perform Delete Process             *
      *                                                               *
      *                                                               *
      *****************************************************************
      *                                                               *
      *  REVISIONS:                                                   *
      *                                                               *
      *  03/31/03 C:145 Project 124IAA                                *
      *    Removed any references to selection phase.  Since the      *
      *    batch process allows for multiple date range selections    *
      *    by adding records to the SMPWP file and only clearing      *
      *    it after a delete is performed.                            *
      *****************************************************************
      *
     FSMOTL     IF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     OTKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
      *
     C     *ENTRY        PLIST
     C                   PARM                    PROCES            1            C,D (COPY,DEL)
     C                   PARM                    RTNCDE            1
      *
     C                   MOVE      'G'           QQOSYS
     C                   MOVEL     'OPTMLB01'    QQLIBN
      *
     C     OTKY          CHAIN     POT01                              62
      *
     C     *IN62         IFEQ      *OFF
     C*
     C     PROCES        CASEQ     'C'           COPY
     C     PROCES        CASEQ     'D'           DELETE
     C                   ENDCS
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
      * If Copy process is either not started or still processing,     ed,
      * mark the flag as started for Copy.                             g as
      *
     C     OTDELF        IFEQ      'Y'
     C     OTCPYF        ANDNE     'S'
     C*                    MOVEL*BLANK    RTNCDE
     C                   ELSE
      *
      *  Error can't run process
      *
     C                   MOVEL     '1'           RTNCDE
     C                   ENDIF
      *
     C     ENDCPY        ENDSR
      *
      *================================================================***
      *  DELETE SUBROUTINE                                             ***
      *================================================================***
      *
     C     DELETE        BEGSR
      *
      *  If Copy process is done, and Del process is either not start  ed,
      *  or still processing, mark the flag as started for Deletion.   g as
      *
     C     OTCPYF        IFEQ      'Y'
     C     OTDELF        ANDEQ     'N'
     C*          OTCPYF    OREQ 'Y'
     C*          OTDELF    ANDEQ'S'
     C                   MOVEL     *BLANK        RTNCDE
     C                   ELSE
      *
      *  Error can't run process
      *
     C                   MOVEL     '2'           RTNCDE
     C                   ENDIF
      *
     C     ENDDEL        ENDSR
      *
