     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRERROR - ERORR MESSAGES                                 *
     H*****************************************************************
     H*  PROGRAMMER: C:20               REQ/PROJ:                     *
     H*  CREATED: 11/22/94                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     FSRERRORF  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*

     D ERROR           S              5    DIM(24) CTDATA PERRCD(1)             Msg ID/Msg Desc
     D MSG             S             60    DIM(24) ALT(ERROR)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQMESG            5
     C*
     C* INITIALIZE SCREEN VARIABLES
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C                   Z-ADD     1             Q                 5 0
     C                   MOVE      *BLANKS       ERRMSG
     C     QQMESG        LOOKUP    ERROR(Q)                               70
     C     *IN70         IFEQ      '1'
     C                   MOVEL     MSG(Q)        ERRMSG
     C                   END
     C*
     C* DISPLAY THE SCREEN
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C                   EXFMT     WDW01
     C                   END                                                    END Q2RECF = 'WDW01'
     C*
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C/EJECT
**  ERROR/MSG
01011                  Undefined Record type
01021          Tried to write a duplicate record
01031           Match fields out of sequence
01041           Array/Table load sequence error
01051          Excess entries in array/table file
01052         Clearing of table prior to dump of data
01071              Numeric sequence error
01201            Record mismatch detedted on input
01211         Group Auxiliary record is not set up
01215         Open issued to a file already open
01216        Error on a implicit open/close operation
01276        Error on a implicit open/close operation
01218Unable to allocate record at this time, please try later
01221        Update Oper attempted w/o prior read
01251            Permanent I/O error occurred
01255           Session or Device error occurred
01261  Attempt to exceed the maximum # of acquired devices
01281            Operation to unacquired devices
01282            Job ending with controlled option
01285         Attempt to acquire already acquired device
01286           Attempt to open shared file w/SAVDS
01287          Repsonse indicator overlap IND indicator
01299                Other I/O error detected
01331          Wait time exceeded for read from WRKSTN
