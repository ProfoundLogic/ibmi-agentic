     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTWMUPP
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Load new fields UPRSV1, UPRSV2, UPRSV3, UPRSV4             *
     H*                                                               *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  04/08/03 C:53 PROJECT: WC1122                                *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*      60    READ/CHAIN                                         *
     H*                                                               *
     H*****************************************************************
      *
     FWMUPP     UF   E             DISK
      *
      *  Process all Net Deductible Recovery records
      *

     C                   READ      PUP01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
     C     UPRSV1        IFEQ      *BLANKS
     C     UPRSV2        ANDEQ     *BLANKS
     C     UPRSV3        ANDEQ     *BLANKS
     C     UPRSV4        ANDEQ     *BLANKS
      *
     C                   MOVE      'M'           UPRSV1
     C                   MOVE      'C'           UPRSV2
     C                   MOVE      'L'           UPRSV3
     C                   MOVE      'E'           UPRSV4
      *
     C                   UPDATE    PUP01
     C                   ENDIF
      *
     C                   READ      PUP01                                  60
      *
     C                   ENDDO
      *
      * END PROGRAM
      *
     C                   SETON                                        LR
      *
