     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
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
     H*  05/02/02 C:62 PROJECT:MC091 32344                            *
     H*                INITIALIZE XXFRDT AND XXTODT                   *
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
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0           1
     C                   PARM                    @@RTCD            1             2
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
     C*** BEGIN ADD *** 05/02/02 **************************************
     C                   Z-ADD     *ZEROS        XXFRDT
     C                   Z-ADD     *ZEROS        XXTODT
     C*** END   ADD *** 05/02/02 **************************************
     C*
     C* CHAIN OUT TO FAEOP RECORD.
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* LETS LOOP THROUGH T/M ACTIVITY LINE ITEMS TO GET DATE RANGE
     C*
     C     TMKY          CHAIN     PTM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C* LETS GET THE FROM DATE
     C*
     C     XXFRDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     TMSVDT        XXFRDT
     C*
     C                   ELSE
     C*
     C     TMSVDT        IFLT      XXFRDT
     C*
     C                   Z-ADD     TMSVDT        XXFRDT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* LETS GET THE TO DATE
     C*
     C     XXTODT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     TMSVDT        XXTODT
     C*
     C                   ELSE
     C*
     C     TMSVDT        IFGT      XXTODT
     C*
     C                   Z-ADD     TMSVDT        XXTODT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     TMKY          READE     PTM01                                  99
     C*
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
     C*
     C                   UPDATE    PEO01
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C                   RETURN
     C*
