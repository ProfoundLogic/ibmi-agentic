     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  SC800R - DARTS DELETE PROGRAM                                *
     H*  This program receives a specific DARTS key, and deletes all  *
     H*     associated records in the following files:
     H*           SDCMP - Remarks File
     H*           SMARP - Additional Remarks File
     H*           SMHDP - Database File - Record Format A
     H*           SMH1P - Database File - Record Format B
     H*  PROJECT: SC051                                               *
     H*  PROGRAMMER: LJD      CREATION DATE: 10/15/91                 *
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*  60  General CHAIN/READE                                     **
     H*****************************************************************
     H*  FILE USAGE:                                                 **
     H*  SDCMLB (PCM01):  KEY, No S/O                                **
     H*  SMARL1 (PAR01):  KEY, No S/O                                **
     H*  SMHDL  (PHD01):  KEY, No S/O                                **
     H*  SMH1L  (PH101):  KEY, No S/O                                **
     H*****************************************************************
     FSDCMLB    UF   E           K DISK
     FSMARL1    UF   E           K DISK
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD            1
     C*
     C     *LIKE         DEFINE    CMKEY         QQKEY
     C*
     C*** SDCMP - Remark Detail File
     C     QQKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCM01
     C     QQKEY         READE     PCM01                                  60
     C                   END
     C*
     C*** SMARP - Additional Remarks File
     C     QQKEY         CHAIN     PAR01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAR01
     C     QQKEY         READE     PAR01                                  60
     C                   END
     C*
     C*** SMHDP - Custom Group ID Data File - Format A
     C     QQKEY         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PHD01
     C     QQKEY         READE     PHD01                                  60
     C                   END
     C*
     C*** SMH1P - Custom Group ID Data File - Format B
     C     QQKEY         CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PH101
     C     QQKEY         READE     PH101                                  60
     C                   END
     C*
     C*** BEGIN DEL *** 10/15/91 *************************************
     C*** SMWCP - WC Remark Extract File
     C*********  QQKEY     CHAINPWC01                60
     C*********  *IN60     DOWEQ'0'
     C*********            DELETPWC01
     C*********  QQKEY     READEPWC01                    60
     C*********            END
     C*** END DEL ***** 10/15/91 *************************************
     C*
     C***  End the Program
     C     TERM          TAG
     C                   RETURN
