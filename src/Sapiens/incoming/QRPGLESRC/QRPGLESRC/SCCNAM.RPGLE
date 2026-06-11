     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE CR001R - BUILD CLAIMANT NAME PROGRAM
     H*
     H*****************************************************************
     H*  THIS SUBPROGRAM WILL TAKE THE INCOMING PARMS OF PROGRAM AND
     H*  DARTS KEY; RETRIEVE CLAIMANT NAME FROM WORK COMP FILE AND
     H*  RETURN CLAIMANT NAME TO SRKCVT PGM.
     H*****************************************************************
     H*  PROGRAMMER:  LORI J. QUINN
     H*  DATE:        JULY 25, 1991
     H*  CLIENT:      003
     H*  PROJECT:     SC030
     H*****************************************************************
     H*
     F*
     F* WORK COMP CLAIM MASTER FILE
     FWMCML1    IF   E           K DISK
     F*
     F* WORK COMP CLAIMANT DETAIL FILE
     FWMCDL     IF   E           K DISK

     D*
     D* DATA STRUCTURE FOR KEY
     D                 DS
     D  XZKEY                  1     79
     D  XZCO#                  1      3  0
     D  XZFND                  4      6  0
     D  XZFYR                  7      9  0
     D  XZCASE                10     16  0
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C*
     C* KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* KEY LIST FOR THE CLAIMANT DETAIL FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    Q1PGM            10            PROGRAM NUMBER
     C                   PARM                    Q1KEY            79            DARTS' KEY
     C                   PARM                    Q1NAME           79            NAME
     C*
     C* SET-UP AND INITIALIZE
     C*
     C                   MOVE      *BLANKS       XZKEY
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVEL     Q1KEY         XZKEY
     C*
     C* CHAIN TO CLAIM MASTER FILE TO GET SS#
     C*
     C                   Z-ADD     XZCO#         XXCO#
     C                   Z-ADD     XZFND         XXFND
     C                   Z-ADD     XZFYR         XXFYR
     C                   Z-ADD     XZCASE        XXCASE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVEL     CMSS#         XXSS#
     C*
     C* GET CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVE      *BLANK        SRLNAM           56
     C                   MOVE      *BLANKS       SRFNAM
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM           15
     C                   MOVEL     ' '           SRTERM            1
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SRTERM
     C*
     C                   MOVE      *BLANKS       Q1NAME
     C                   MOVEL     SRLNAM        Q1NAME
     C*
     C                   END                                                    *IN61 END
     C                   END                                                    *IN60 END
     C*
     C* END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
