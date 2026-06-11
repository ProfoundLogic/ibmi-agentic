     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     *=====================================================================
     *=====================================================================
     H/TITLE ISIM53R1- OPTACOMP IMPORT PROGRAM  - DARTS KEY VALIDATE
     *=====================================================================
     * Description:  This program PASSES a DARTS key to return and
     *
     *
     * Created   : 05/06/13
     * Programmer: C:282
     * Project   : 114WJC
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

     d TRUE            C                   CONST('1')
     d FALSE           C                   CONST('0')
      *
     *=====================================================================
     * Data Structure, Table & Array definitions
     *=====================================================================
     ** Program information
     d PSDS          ESDS                  EXTNAME(SZQ1P)
      * IMPORTED DATA FROM ASCENT
     d IMPDTA        E DS                  EXTNAME(OPTIMPT)
      *
     ** Date/Time
     d                 DS
     d  DSDTTM                 1     14  0
     d  DSTIME                 1      6  0
     d  DSDATE                 7     14  0
      *
      *
     *=====================================================================
     * PList definitions
     *=====================================================================
     * Entry parms

     c     *ENTRY        PLIST
     c                   PARM                    IMPDTA                         IMPORTED DATA
     c                   PARM                    @RTCD             7            RTN CODE(S)
     c                   PARM                    @KEY             40            RTN KEY
      *
     *=====================================================================
     * Main
     *=====================================================================
     ** Initalization
     c                   EXSR      SRINIT
      *
     c                   EXSR      SRPRCS
      *
     c                   EXSR      FINISH
      *
     ** End the program
     c                   MOVE      *ON           *INLR
     *=====================================================================
      *
      *
      *
      *
     *=====================================================================
     * SRINIT - Initialization subroutine
     *=====================================================================
     c     SRINIT        BEGSR
      *
     ** Initialize the date/time data structure
     c                   TIME                    DSDTTM
      *
      *
     c                   ENDSR                                                  SRINIT
     *=====================================================================
     * FINISH - DO clean up stuff
     *=====================================================================
     c     FINISH        BEGSR
      *
     c                   ENDSR                                                  FINISH
     *=====================================================================
     * SRPRCS - PROCESS DART CALL
     *=====================================================================
     c     SRPRCS        BEGSR
      *
      *** ANY PROCESSING SHOULD BE DONE HERE AGAINST DATA FILES ETC. USING IDX
      ***    VALUES RECIEVED FROM THE SCAN.   1ST VALIDATE THE KEY.  IF NOT VALI
      ***    THEN THE TIFF SHOULD BE IMPORTED, AND QUEUED FOR MANUAL INDEXING
     c                   CALL      'ISIV03R'
     c                   PARM      OPKEY         Q#DKEY           40
     c                   PARM      ' '           @@RTN             1
      *
     c     @@RTN         IFNE      *BLANK
     c                   MOVE      '      1'     @RTCD
     c                   MOVE      *BLANKS       @KEY
     c                   ELSE
     c                   MOVEL     OPKEY         @KEY
     c                   ENDIF
      *
     c                   ENDSR                                                  SRPRCS
     *=====================================================================
