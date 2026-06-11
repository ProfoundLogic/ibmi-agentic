     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA724R - GET EOFRDT AND EOTODT.                          DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA724R                                            *
     H*    CREATED: 03/10/00  C:62                                    *
     H*    DESCRIPTION: GO THROUGH T/M ACTIVITY AND GET FRDT AND TODT *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    *IN99                GENERAL CHAINING INDICATOR            *
     H*****************************************************************
     F*
     FNDTML     IF   E           K DISK
     FFAEOP     UF   E             DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER FILE
     C*
     C     TMKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
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
     C     *LIKE         DEFINE    EOFRDT        XXFRDT
     C     *LIKE         DEFINE    EOTODT        XXTODT
     C*
     C* CHAIN OUT TO FAEOP RECORD.
     C*
     C                   READ      PEO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     EOSYS         IFEQ      'M'
     C     EOSYS         OREQ      'U'
     C*
     C                   Z-ADD     *ZEROS        XXFRDT
     C                   Z-ADD     *ZEROS        XXTODT
     C*
     C* LETS LOOP THROUGH T/M ACTIVITY LINE ITEMS TO GET DATE RANGE
     C*
     C     TMKY          CHAIN     PTM01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C* LETS GET THE FROM DATE
     C*
     C     XXFRDT        IFEQ      *ZEROS
     C                   Z-ADD     TMSVDT        XXFRDT
     C                   ELSE
     C     TMSVDT        IFLT      XXFRDT
     C                   Z-ADD     TMSVDT        XXFRDT
     C                   ENDIF
     C                   ENDIF
     C*
     C* LETS GET THE TO DATE
     C*
     C     XXTODT        IFEQ      *ZEROS
     C                   Z-ADD     TMSVDT        XXTODT
     C                   ELSE
     C     TMSVDT        IFGT      XXTODT
     C                   Z-ADD     TMSVDT        XXTODT
     C                   ENDIF
     C                   ENDIF
     C*
     C     TMKY          READE     PTM01                                  99
     C                   ENDDO
     C*
     C* FINALLY, LETS UPDATE FAEOP WITH FROM AND TO DATES.
     C*
     C                   Z-ADD     XXFRDT        EOFRDT
     C                   Z-ADD     XXTODT        EOTODT
     C*
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C                   ENDIF
     C*
     C                   UPDATE    PEO01
     C*
     C                   READ      PEO01                                  99
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C                   SETON                                        LR
