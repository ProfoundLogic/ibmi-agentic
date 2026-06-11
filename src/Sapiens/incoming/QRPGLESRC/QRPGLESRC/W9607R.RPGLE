     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W9607R - WORK FILE BUILD - Loss Control Tracking Report
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     Write records from WMVSP that correspond to given visit   *
     H*     date range and Loss Representative selection to WXVSP.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W9606C - OVERRIDE                                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  04/19/99        PROGRAMMER C:61       PROJECT 102AK          *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  XX/XX/XX C:XX PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
      *
      * Loss Control Task File
     FWMVSL1    IF   E           K DISK
      *
      * Loss Control Task Work File
     FWXVSP     O    E             DISK
     F                                     RENAME(PVS01:PVS02)
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *   Loss Control Task
      *

     C     VSKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKVSDT
      *
     C     VSKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      *   *LIKE Define Variables
      *
     C     *LIKE         DEFINE    VSCO#         KKCO#
     C     *LIKE         DEFINE    VSFND         KKFND
     C     *LIKE         DEFINE    VSVSDT        KKVSDT
      *
     C     *LIKE         DEFINE    VSVSDT        X1FRDT
     C     *LIKE         DEFINE    VSVSDT        X1TODT
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@STAF            3
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *   Convert parms
      *
     C                   MOVE      @@CO#         KKCO#
      *
     C                   MOVE      @@FND         KKFND
      *
     C                   MOVE      @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FRDT
      *
     C                   MOVE      X1FRDT        KKVSDT
      *
     C                   MOVE      @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TODT
      *
      *  Read 1st record
      *
     C     VSKY1         SETLL     PVS01
     C     VSKY2         READE     PVS01                                  60
      *
      *  While visit date in range
      *
     C     *IN60         DOWEQ     '0'
     C     VSVSDT        ANDGE     X1FRDT
     C     VSVSDT        ANDLE     X1TODT
      *
      *  Check staff code, if called with staff code
      *
     C     @@STAF        IFNE      *BLANKS
     C     @@STAF        ANDNE     VSSTF
     C                   GOTO      NEXT
     C                   ENDIF                                                  @@STAF
      *
      *  Write record
      *
     C                   WRITE     PVS02
      *
      *  Read next record
      *
     C     NEXT          TAG
      *
     C     VSKY2         READE     PVS01                                  60
      *
     C                   ENDDO                                                  WRK RECORD
      *
      *  End of Processing
      *
     C     END           TAG
     C                   MOVE      *ON           *INLR
      *
