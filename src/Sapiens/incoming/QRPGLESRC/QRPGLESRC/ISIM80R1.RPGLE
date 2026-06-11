     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     *=====================================================================
     *=====================================================================
     H/TITLE ISIM80R1- SUMMIT CLAIMS     IMPORT PROGRAM  - DARTS KEY VALIDATE
     *=====================================================================
     * Description:  This program PASSES a DARTS key to return and
     *
     *
     * Created   : 07/28/04
     * Programmer: C:02
     * Project   : DRXXX
     *
     *=====================================================================
     * Revisions:
     *
     *
     * (NOTE: Please add comments to the top of revisions.)
     *=====================================================================
      *
      *
     *=====================================================================
     * File Definitions
     *=====================================================================
     *=====================================================================
     * Constant definitions
     *=====================================================================
     ** Program control

     D TRUE            C                   CONST('1')
     D FALSE           C                   CONST('0')
      *
     *=====================================================================
     * Data Structure, Table & Array definitions
     *=====================================================================
     ** Program information
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D* IMPORTED DATA FROM ASCENT
     D IMPDTA        E DS                  EXTNAME(ASCREL1)
      *
     ** Date/Time
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
     *=====================================================================
     * PList definitions
     *=====================================================================
     * Entry parms

     C     *ENTRY        PLIST
     C                   PARM                    IMPDTA                         IMPORTED DATA
     C                   PARM                    @RTCD             7            RTN CODE(S)
     C                   PARM                    @KEY             40            RTN KEY
      *
     *=====================================================================
     * Main
     *=====================================================================
     ** Initalization
     C                   EXSR      SRINIT
      *
     C                   EXSR      SRPRCS
      *
     C                   EXSR      FINISH
      *
     ** End the program
     C                   MOVE      *ON           *INLR
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * SRINIT - Initialization subroutine
     *=====================================================================
     C     SRINIT        BEGSR
      *
     ** Initialize the date/time data structure
     C                   TIME                    DSDTTM
      *
      *
     C                   ENDSR                                                  SRINIT
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * FINISH - DO clean up stuff
     *=====================================================================
     C     FINISH        BEGSR
      *
      *
     C                   ENDSR                                                  FINISH
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * SRPRCS - PROCESS DART CALL
     *=====================================================================
     C     SRPRCS        BEGSR
      *
     C                   MOVE      '      1'     @RTCD
     C                   MOVE      *BLANKS       @KEY
      *
     C                   ENDSR                                                  SRPRCS
     *=====================================================================
