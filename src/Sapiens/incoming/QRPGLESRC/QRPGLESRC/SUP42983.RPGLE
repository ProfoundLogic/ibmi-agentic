     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE WDNTP
     H*****************************************************************
     H* Description:  This program removes erroneous records from     *
     H*     file WDNTP, that were written by W0070R07.                *
     H*                                                               *
     H* Created by:  015      Date: 06/23/04                          *
     H*****************************************************************
     FWDNTP     UF   E             DISK

     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C                   READ      PNT01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     NTONAM        IFEQ      NTNNAM
     C     NTOAD1        ANDEQ     NTNAD1
     C     NTOAD2        ANDEQ     NTNAD2
     C     NTOCTY        ANDEQ     NTNCTY
     C     NTOST         ANDEQ     NTNST
     C     NTOZIP        ANDEQ     NTNZIP
     C*
     C                   DELETE    PNT01
     C*
     C                   ELSE
     C*
     C     NTONAM        IFEQ      NTNNAM
     C     NTTYPE        ANDEQ     'B'
     C                   MOVE      'A'           NTTYPE
     C*
     C                   UPDATE    PNT01
     C*
     C                   ELSE
     C*
     C     NTOAD1        IFEQ      NTNAD1
     C     NTOAD2        ANDEQ     NTNAD2
     C     NTOCTY        ANDEQ     NTNCTY
     C     NTOST         ANDEQ     NTNST
     C     NTOZIP        ANDEQ     NTNZIP
     C     NTTYPE        ANDEQ     'B'
     C                   MOVE      'N'           NTTYPE
     C*
     C                   UPDATE    PNT01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PNT01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
