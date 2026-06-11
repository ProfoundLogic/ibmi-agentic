     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXRCFSR - SET RCFSR = 'Y'
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM ASSIGNS 'Y' TO RCFSR              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    COMMAND LINE                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/01/01  PROGRAMMER: C:62  PROJECT: UC002          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  READ  - FMRCP                                           *
     H*****************************************************************
     FFMRCP     UF   E             DISK
     F*

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C*  LIKE DEFINES
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PRC01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C                   MOVEL(P)  'Y'           RCFSR
     C*
     C                   UPDATE    PRC01
     C*
     C* GET NEXT PC FILE RECORD
     C*
     C                   READ      PRC01                                  99
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C                   SETON                                        LR
