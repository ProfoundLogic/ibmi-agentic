     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N0218R - CREATE WORK FILE FOR INTERACTIVE HOLD/RELEASE
     H*               FOR PROVIDER BILLS
     H*****************************************************************
     H**                                                             **
     H** THIS PROGRAM IS USED TO CREATE A WORK FILE FROM FAEAP       **
     H** (EOB BILL HEADER AMOUNT FILE).  THE WORK FILE CREATED WILL  **
     H** CONSIST OF ALL THOSE RECORDS FROM FAEAP THAT HAVE A BILLING **
     H** AMOUNT AND HAVE NOT BEEN INVOICED.                          **
     H**                                                             **
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                              **
     H*  10/21/99 C:62 PROJ:MC201                                    **
     H*                ADD LOGIC TO SELECT RECORDS IF BPFL = 'Y'     **
     H*                                                              **
     H*****************************************************************
     FFAEAP     IF   E             DISK
     FHAEAP     O  A E             DISK
     F                                     RENAME(PEA01:PEA02)

     C*
     C* KEY LIST TO READ MATCHING EOB RECORD
     C*
     C*
     C                   MOVE      '0'           *IN60
     C     *IN60         DOWEQ     '0'
     C                   READ      PEA01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     EABBLF        IFNE      'P'
     C*** BEGIN ADD *** 10/21/99 **************************************
     C     EABPFL        ANDEQ     'Y'
     C*** END   ADD *** 10/21/99 **************************************
     C*
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'U'
     C     EASYS         OREQ      'M'
     C     EABAMT        IFNE      *ZEROS
     C                   WRITE     PEA02
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EANBLF        IFNE      'P'
     C*
     C     EASYS         IFEQ      'N'
     C     EANAMT        ANDNE     *ZEROS
     C                   WRITE     PEA02
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C*
