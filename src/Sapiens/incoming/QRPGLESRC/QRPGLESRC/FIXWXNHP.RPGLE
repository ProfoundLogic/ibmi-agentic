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
     FWXNHP     UF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMRPL     IF   E           K DISK
      *

     D                 DS
     D  XPPOL                  1     18
     D  XPFND                  1      3
     D  XPEMP#                 4     12
     D  XPFYR                 13     15
      *

     C     ELKY          KLIST
     C                   KFLD                    NHCO#
     C                   KFLD                    NHFND
     C                   KFLD                    XXFYR
     C                   KFLD                    NHEMP#
      *
     C     RPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     *LIKE         DEFINE    NHFYR         XXFYR
      *
     C                   MOVE      *BLANKS       XPPOL
      *
     C                   READ      PNH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     NHPP#         IFEQ      *BLANKS
      *
     C     NHFYR         SUB       1             XXFYR
     C     ELKY          CHAIN     PEL01                              61
      *
     C     *IN61         IFEQ      *OFF
      *
     C     RPKY          CHAIN     PRP01                              62
      *
     C     *IN62         IFEQ      *OFF
     C                   MOVEL(P)  RPCAP#        NHPP#
     C                   ENDIF                                                  *IN62 IFEQ *OFF
      *
     C     NHPP#         IFEQ      *BLANKS
     C                   MOVE      ELFND         XPFND
     C                   MOVE      ELEMP#        XPEMP#
     C                   MOVE      ELFYR         XPFYR
     C                   MOVEL(P)  XPPOL         NHPP#
     C                   ENDIF                                                  NHPP# IFEQ *BLANKS
      *
     C                   UPDATE    PNH01
      *
     C                   ENDIF                                                  *IN61 IFEQ *OFF
      *
     C                   ENDIF                                                  NHPP# IFEQ *BLANKS
      *
     C                   READ      PNH01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   MOVE      *ON           *INLR
      *
