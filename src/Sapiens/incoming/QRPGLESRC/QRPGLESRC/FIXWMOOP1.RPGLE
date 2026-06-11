     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWMOOP1 - DELETE LOCATION RECORDS
     H*****************************************************************
     H*                                                               *
     H*  THERE SHOULD BE NO RECORDS FOR LOCATIONS.  LOCATIONS USE     *
     H*  SAME RECORDS AS DIVISION.  ONLY ENTITIES HAVE SEPARATE       *
     H*  OWNERS / OFFICERS.                                           *
     H*                                                               *
     H*  CREATED: 01/04/05  C:61  SUPPORT(WA129 #45291)               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
      *
      * Entity Location
     FWMALL     IF   E           K DISK
      *
      * Owner Officer
     FWMOOL     UF   E           K DISK
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     OOKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
     C                   KFLD                    ALELID
      *
      *
     C                   READ      PAL01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
     C     ALELTP        IFEQ      'L'
     C     ALELID        ANDNE     *BLANKS
     C     OOKY          CHAIN     POO01                              60
      *
     C     *IN60         DOWEQ     '0'
      *
     C     OOELID        IFEQ      ALELID
     C                   DELETE    POO01
     C                   ENDIF                                                  OOELID
      *
     C     OOKY          READE     POO01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  ALELTP
      *
     C                   READ      PAL01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
      *
