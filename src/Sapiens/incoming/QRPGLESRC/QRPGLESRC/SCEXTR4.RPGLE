     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SCEXTR3  - SIS DARTS EXTRACT
     H*****************************************************************
     H*  SCEXTR3  - SIS DARTS EXTRACT                                 *
     H*  Created: 07/27/12  C:280 Project: 114IHH                      *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     F*
     FSMARL1    IF   E           K DISK
     F*
     F* ADDITIONAL REMARKS FILE by KEY/SEQ#/ASQ#
     F*
     FSDCMLC    IF   E           K DISK
     F*
     F* Remark Detail File by PNAM/KEY
     F*
     F* Claim Interface Workfiles
     F*
     FSXCMPWC   O    E           K DISK
     F                                     RENAME(PCM01:PCM0C)
     F*
     F* Remark Detail File
     F*
     FSXARPWC   O    E           K DISK
     F                                     RENAME(PAR01:PAR0C)
     F*
     F* ADDITIONAL REMARKS FILE
     F*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     C*
     C     *LIKE         DEFINE    CMKEY         XXKEY
     C*
     C* Additional Remarks File
     C*
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    COMPANY           2
     C*
     C                   MOVE      COMPANY       CO#               2 0
     C*
     C* Do Remark Detail
     C*
     C     *loval        SETLL     PCM01
     C                   READ      PCM01                                  99
     C*
b01  C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL     CMKEY         CO                2 0
     C*
b02  C     CO#           IFEQ      CO
     C*
     C                   WRITE     PCM0C
     C*
     C* Get Additional Comment Records
     C*
     C     ARKY          CHAIN     PAR01                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C                   WRITE     PAR0C
     C*
     C     ARKY          READE     PAR01                                  98
     C*
     C                   ENDDO
     C*
e02  C                   ENDIF
     C*
     C                   Read      PCM01                                  99
     C*
e01  C                   ENDDO
     C*
     C                   EVAL      *INLR = *ON
     C*
     C***********************************************************************
