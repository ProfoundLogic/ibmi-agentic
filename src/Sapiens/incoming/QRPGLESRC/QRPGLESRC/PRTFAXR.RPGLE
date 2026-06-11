     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE PRTFAXR - PRINT FAX FROM DARTS                           *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*    None                                                       *
     H*  PROGRAMMER: 02                                               *
     H*  CREATED:    12/20/95             PROJECT:                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTION         *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  FILE USGE:                                                  *
     H*    SAIFL   (PIF01): RCID                                      *
     H*****************************************************************
     F*
     FSAIFL2    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  XXRECF                 1     10
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C                   PARM                    DCID             12
     C                   PARM                    RCODE             1
     C*
     C     DCID          CHAIN     PIF01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           RCODE
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C                   CALL      'PRTFAXC'
     C                   PARM                    IFLFM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
