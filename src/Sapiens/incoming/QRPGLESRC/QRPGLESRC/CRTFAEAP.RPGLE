     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CRTFAEAP- CREATES FAEAP RECORDS                              *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEOL3    IF   E           K DISK
     FFAEAL1    IF A E           K DISK
     FFMF1L     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@EOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*
     C     EOKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@EOB#
     C*
     C     F1KY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C     *LIKE         DEFINE    EOCO#         @@CO#
     C     *LIKE         DEFINE    EOSUB#        @@SUB#
     C     *LIKE         DEFINE    EOEOB#        @@EOB#
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         DOWEQ     *OFF
     C     EOBLST        IFEQ      'C'
     C     EORCN         ANDEQ     0
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     EOCO#         EACO#
     C                   Z-ADD     EOSUB#        EASUB#
     C                   Z-ADD     EOFND         EAFND
     C                   Z-ADD     EOFYR         EAFYR
     C                   Z-ADD     EOCASE        EACASE
     C                   Z-ADD     EOEOB#        EAEOB#
     C                   Z-ADD     EORCN         EARCN
     C*
     C                   MOVEL     EOSS#         EASS#
     C                   Z-ADD     EOACDT        EAACDT
     C                   MOVEL     EOSYS         EASYS
     C*
     C                   Z-ADD     EOCHRG        EACHRG
     C                   Z-ADD     EOMRA         EAMRA
     C                   Z-ADD     EONDIS        EANDIS
     C                   Z-ADD     EOOTHR        EAOTHR
     C                   Z-ADD     EONDIS        EANDIS
     C                   Z-ADD     EOPAID        EAPAID
     C*
     C                   Z-ADD     EOFEIN        EAFEIN
     C                   Z-ADD     EOMOD         EAMOD
     C                   MOVEL(P)  EOPNAM        EAPNAM
     C                   MOVEL(P)  EOADR1        EAADR1
     C                   MOVEL(P)  EOADR2        EAADR2
     C                   MOVEL(P)  EOCTY         EACTY
     C                   MOVEL(P)  EOST          EAST
     C                   Z-ADD     EOZIP         EAZIP
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *OFF
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '1'           EAPLNK
     C                   ELSE
     C     EOPAID        IFLE      0
     C                   MOVE      '2'           EAPLNK
     C                   ELSE
     C                   MOVE      '3'           EAPLNK
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     EODT          EADT
     C                   MOVEL     EOIDT         EAIDT
     C                   Z-ADD     EOTIME        EATIME
     C                   Z-ADD     EOITIM        EAITIM
     C*
     C                   MOVEL     EOUSER        EAUSER
     C                   MOVEL     EOIUSR        EAIUSR
     C                   MOVEL     EOPGM         EAPGM
     C                   MOVEL     EOIPGM        EAIPGM
     C*
     C                   WRITE     PEA01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOKY          READE     PEO01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
