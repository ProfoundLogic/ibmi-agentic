     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PRE PROCESS OPTICAL (BMICO)  BUILD WORK FILES
      *
      * * *
      ***************************************************************
      * CALLS TO/PARMS
      *
      ***************************************************************
      *  REVISIONS:
     H***************************************************************
     FSDCMLE    IF   E           K DISK
     F*
     F*
     FSXC1L1    UF   E           K DISK
     F*
     F*

     D*
     D ER              S             40    DIM(2) CTDATA PERRCD(1)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  ERDS                   1     40
     D  ERPGM                  1      8
     D  ERBLK1                 9      9
     D  ERRTCD                10     10
     D  ERBLK2                11     11
     D  ERDESC                12     40
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

     C*
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** DARTS RECORD
      *
     C     CMK           KLIST
     C                   KFLD                    DLKEY
     C                   KFLD                    DLSEQ#
      *
     C*
     C*** DOCUMENT LOCATION FILE
      *
     C*
      *
      *
     C*
      *
      * =================================== *
      * * *  PARM LISTS                     *
      * =================================== *
      *
     C*
     C*** ENTRY PARM LIST
     C     *ENTRY        PLIST
     C                   PARM                    QQVOL1           10
      *
      *
     C     QQVOL1        SETLL     PC101
     C     QQVOL1        READE     PC101                                  60
      *
      *
    **** OR LOOP THROUGH ALL DL RECORDS GET CM, AND WRITE FILE.. NO SELECT/OMIT
    *****    IF NO HIT ON CM, THEN ERROR IN CVERR <--- RCD NOT FOUND
    C*******               READ PDL01                    60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      *BLANKS       CVERR
     C     CMK           CHAIN     PCM01                              61
     C     *IN61         IFEQ      *ON
     C                   CLEAR                   PCM01
     C                   MOVEL     ER(1)         CVERR
     C                   UPDATE    PC101
     C                   ENDIF
     C     QQVOL1        READE     PC101                                  60
     C                   ENDDO
     C****
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C********
**
01 - NO CM RCD FOUND
02 - NO MD RCD FOUND
