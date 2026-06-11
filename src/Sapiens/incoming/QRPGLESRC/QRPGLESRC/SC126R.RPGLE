     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  CHAIN INDICATOR                                           *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     H** NOTE:
     H*****************************************************************
     FSAFTL2    UF   E           K DISK

     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*
     C     FTKY          KLIST
     C                   KFLD                    XXDT
     C*
     C     *LIKE         DEFINE    FTDT          X1DT
     C     *LIKE         DEFINE    FTDT          XXDT
     C*
     C*   ENTRY PARAMETERS
     C     *ENTRY        PLIST
     C                   PARM                    QQDT              6
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
     C                   MOVE      QQDT          Q1DT              6 0
     C*
     C                   Z-ADD     Q1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DT
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C*=============================================================
     C*
     C*
     C                   MOVE      *LOVAL        XXDT
     C     FTKY          SETLL     PFT01
     C                   READ      PFT01                                  61
     C     *IN61         DOWEQ     '0'
     C*
     C     FTFCDT        IFLE      X1DT                                          LT OR EQ TO DT
     C     FTFAXC        IFEQ      'C'                                           FAX COMPLETE
     C     FTFAXC        OREQ      'E'                                             OR ERRORED
     C                   DELETE    PFT01                                         **DELETE******
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   READ      PFT01                                  61
     C                   END                                                    EOWEQ*IN61
     C*
     C*
     C*
     C*
     C* END OF THE PGM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
