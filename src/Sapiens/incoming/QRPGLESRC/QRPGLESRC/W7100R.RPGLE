     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7100R - UPDATE EMPLOYER DETAIL WITH CLAIM COUNT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/04/10 C:190   PROJECT: 25WRE                              *
     H*                   Implement change to calculate total         *
     H*                   number of claims.                           *
     H*                                                               *
     H*  11/21/94 C:28    PROJECT: WC380                              *
     H*                   REVISED ALGORITHM EFFICIENCY. UPDATE TODAY  *
     H*                   CHANGES ONLY.                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK
     F*
     FWMCML1    IF   E           K DISK
     F*
     FWMCHL     IF   E           K DISK
     F*
     FWMCHL1    IF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     F***  BEGIN ADD  ***  01/04/10  *********************************
     FWMCMLY    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMFNL     IF   E           K DISK
     F***  END   ADD  ***  01/04/10  *********************************
     FWDPAL2    IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPCH02
     I              CHCO#                       C2CO#
     I              CHFND                       C2FND
     I              CHFYR                       C2FYR
     I              CHCASE                      C2CASE
     I              CHEFDT                      C2EFDT
     I              CHOCST                      C2OCST
     I              CHEXPL                      C2EXPL
     I              CHSTAT                      C2STAT
     I              CHUSER                      C2USER
     I              CHPGM                       C2PGM
     I              CHTIME                      C2TIME
     I              CHDT                        C2DT

     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIM HISTORY MASTER FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDT
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXDT
     C                   KFLD                    XXCO#
     C*
     C*  CLAIM CHECK PMT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C***  BEGIN ADD  ***  01/04/10  *********************************
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXDT
     C***  END   ADD  ***  01/04/10  *********************************
     C*
     C     *LIKE         DEFINE    CHEFDT        XXDT
     C     *LIKE         DEFINE    CHCO#         XXCO#
     C     *LIKE         DEFINE    CHFND         XXFND
     C     *LIKE         DEFINE    CHFYR         XXFYR
     C     *LIKE         DEFINE    CHCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CHOCST        XOCSTA
     C     *LIKE         DEFINE    CHOCST        XOCSTP
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQRNDT            8
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   MOVE      QQRNDT        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*  CHECK CLAIM STATUS CHANGES
     C*
     C     CHKY          CHAIN     PCH02                              60
     C                   MOVE      C2FND         XXFND
     C                   MOVE      C2FYR         XXFYR
     C                   MOVE      C2CASE        XXCASE
     C                   MOVE      C2OCST        XOCSTA
     C     *IN60         DOWEQ     '0'
     C     C2CASE        IFNE      XXCASE
     C*
     C                   EXSR      STSBR
     C*
     C                   MOVE      C2FND         XXFND
     C                   MOVE      C2FYR         XXFYR
     C                   MOVE      C2CASE        XXCASE
     C                   MOVE      C2OCST        XOCSTA
     C                   END
     C*
     C     CHKY          READE     PCH02                                  60
     C                   END
     C                   EXSR      STSBR
     C*
     C*
     C*
     C*  READ TODAY PAYMENTS RECORDS
     C*
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C     PASEQ#        IFEQ      1
     C                   MOVE      PAFND         XXFND
     C                   MOVE      PAFYR         XXFYR
     C                   MOVE      PACASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C                   END
     C     ELKEY         CHAIN     PEL01                              51
     C     *IN51         IFEQ      '0'
     C                   ADD       1             ELT#CL
     C                   UPDATE    PEL01
     C                   END
     C*
     C*
     C                   END
     C     PAKY          READE     PPA01                                  60
     C                   END
     C*
     C***  BEGIN ADD  ***  01/04/10  *********************************
     C*
     C     XXCO#         CHAIN     PFN01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     FNFND         XXFND
     C*
     C     CMKY2         CHAIN     PCM02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              51
     C*
     C     *IN51         IFEQ      '0'
     C                   ADD       1             ELT#CC
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C     CMKY2         READE     PCM02                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     XXCO#         READE     PFN01                                  60
     C                   ENDDO
     C***  END   CHG  ***  01/04/10  *********************************
     C*
     C                   SETON                                        LR
     C*
     C*
     C**** VERIFY STATUS CHANGES ***********************   S U B R ***
     C*
     C     STSBR         BEGSR
     C     CHKEY         CHAIN     PCH01                              61
     C                   READP     PCH01                                  61
     C     *IN61         IFEQ      '0'
     C     CHCO#         ANDEQ     XXCO#
     C     CHFND         ANDEQ     XXFND
     C     CHFYR         ANDEQ     XXFYR
     C     CHCASE        ANDEQ     XXCASE
     C                   MOVE      CHOCST        XOCSTP
     C*
     C     XOCSTP        IFNE      *BLANK
     C     XOCSTA        IFNE      XOCSTP
     C     CMKEY         CHAIN     PCM01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C                   END
     C     ELKEY         CHAIN     PEL01                              51
     C     *IN51         IFEQ      '0'
     C     XOCSTA        IFEQ      'O'
     C     XOCSTP        ANDEQ     'C'
     C                   SUB       1             ELTCLC
     C                   END
     C     XOCSTA        IFEQ      'C'
     C     XOCSTP        ANDEQ     'O'
     C                   ADD       1             ELTCLC
     C                   END
     C                   MOVE      Q1USER        ELUSER
     C                   UPDATE    PEL01
     C                   END
     C                   END
     C                   END
     C                   END
     C                   ENDSR
