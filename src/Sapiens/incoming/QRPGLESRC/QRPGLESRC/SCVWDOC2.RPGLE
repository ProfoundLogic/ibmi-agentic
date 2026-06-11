     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE SCVWDOC2 - VIEW AFP DOCS - SPECIAL PROCESSING
     H*
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
     FSMDSP     IF   E           K DISK
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    #XDCID           12
     C                   PARM                    #XDSID           20
     C                   PARM                    #TOC              1
     C*
     C                   MOVEL     #XDCID        #DSKY            44
     C     #DSKY         CHAIN     OSLIN                              67
     C     *IN67         IFEQ      *OFF
     C                   MOVEL     LINDID        #XDSID
     C                   ELSE                                                    NO HIT
     C                   MOVE      '1'           #TOC
     C                   GOTO      ENDPGM
     C                   ENDIF
     C*
     C     ENDPGM        TAG
     C     ENDIT         TAG
     C                   MOVE      '1'           *INLR
