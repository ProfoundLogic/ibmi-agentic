     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC. 1995
     H**************************************************************************
     H* PROGRAM: N7725R                                                        *
     H* READ ALL OPEN 'M' SERVICES AND CALL F7301R                             *
     H*                                                                        *
     H* CREATED: 05/05/95 C:20                                                 *
     H* REQ/PROJ: NW008                                                        *
     H**************************************************************************
     H*  REVISIONS:                                                            *
     H**************************************************************************
     FFAEOL4    IF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* INITIALIZE SECTION
     C*
     C                   MOVE      *ZEROS        QQCO#             3
     C                   MOVE      *ZEROS        QQSUB#            5
     C                   MOVE      *ZEROS        QQEOB#            9
     C                   MOVE      *BLANKS       QQPRTY            1
     C                   MOVE      *BLANKS       QQMDV             1
     C                   MOVE      *BLANKS       QQMSYS            1
     C*
     C* READ EOMB FILE (OPNQRY FOR EOSTAT EQ 'P' AND EOBLFL NE 'P')
     C*
     C                   READ      PEO01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C* CALL THE PROGRAM TO CREATE THE CHECK REQUEST
     C*
     C                   MOVE      EOCO#         QQCO#
     C                   MOVE      EOSUB#        QQSUB#
     C                   MOVE      EOEOB#        QQEOB#
     C                   MOVE      'Y'           QQPRTY
     C                   MOVE      'Y'           QQMSYS
     C                   CALL      'F7301R'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQEOB#
     C                   PARM                    QQPRTY
     C                   PARM                    QQMSYS
     C*
     C* READ NEXT RECORD
     C*
     C                   READ      PEO01                                  50
     C                   END                                                    END *IN50 DOWEQ '0'
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
