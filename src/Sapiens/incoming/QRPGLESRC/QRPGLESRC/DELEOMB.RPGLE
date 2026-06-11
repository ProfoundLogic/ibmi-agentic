     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE DELEOMB - DELETES 'M' TYPE EOMBS
     H*****************************************************************
     H* PROGRAM: DELEOMB
     H* DESCRIPTION: DELETES 'M' TYPE EOMBS
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     FFAEAL1    UF   E           K DISK
     FFAEOL4    UF   E           K DISK

     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C                   READ      PEA01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     EABAMT        IFEQ      *ZEROS
     C     EABINV        ANDEQ     *ZEROS
     C     EASYS         ANDEQ     'M'
     C*
     C     EOKY          DELETE    PEO01                              99
     C                   DELETE    PEA01
     C*
     C                   ELSE
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C*
     C                   READ      PEA01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
