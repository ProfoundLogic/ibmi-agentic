     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWXNEP - INITIALIZE CARRIER NUMBER AND RESET WMSHP RECORDS
     H*****************************************************************
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:35                                            *
     H*   CREATED:    10/14/98    PROJECT: SUPP WA091 #13352          *
     H*                                                               *
     H*****************************************************************
     FWXNEL     UF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMSHL     UF   E           K DISK
     FWDELL1    IF   E           K DISK
      *

     C     F2KY          KLIST
     C                   KFLD                    NECO#
     C                   KFLD                    NEFND
      *
     C     SHKY          KLIST
     C                   KFLD                    NECO#
     C                   KFLD                    NEFND
     C                   KFLD                    NEEMP#
     C                   KFLD                    NEFYR
     C                   KFLD                    NEUNID
      *
     C     ELKY          KLIST
     C                   KFLD                    NECO#
     C                   KFLD                    NEFND
     C                   KFLD                    NEFYR
     C                   KFLD                    NEEMP#
      *
     C     *LIKE         DEFINE    NECO#         XXCO#
     C     *LIKE         DEFINE    NEFND         XXFND
      *
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
      *
     C                   READ      PNE01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      *OFF          *IN61
     C                   MOVE      *OFF          *IN62
      *
     C     NECO#         IFNE      XXCO#
     C     NEFND         ORNE      XXFND
      *
     C     F2KY          CHAIN     PF201                              60
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             F2CAR#
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
      *
     C                   ENDIF                                                  NECO# IFNE XXCO#
      *
     C     NECAR1        IFEQ      0
     C                   Z-ADD     F2CAR#        NECAR1
     C                   MOVE      *ON           *IN61
     C                   ENDIF                                                  NECAR1 IFEQ 0
      *
     C     NEPFDT        IFEQ      0
      *
     C     ELKY          CHAIN     PEL01                              63
      *
     C     *IN63         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        NEPFDT
     C                   MOVE      *ON           *IN62
     C                   ENDIF                                                  *IN63 IFEQ *OFF
      *
     C                   ENDIF                                                  NEPFDT IFEQ 0
      *
     C     *IN61         IFEQ      *ON
     C     *IN62         OREQ      *ON
      *
     C                   UPDATE    PNE01
      *
     C     SHKY          CHAIN     PSH01                              63
      *
     C     *IN63         IFEQ      *OFF
      *
     C     SHSTAT        IFGT      '3'
     C                   MOVE      '3'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF                                                  SHSTAT IFGT '3'
      *
     C                   ENDIF                                                  *IN63 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C                   READ      PNE01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   MOVE      *ON           *INLR
      *
