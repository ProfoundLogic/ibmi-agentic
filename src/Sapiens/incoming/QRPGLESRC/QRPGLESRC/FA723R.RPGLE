     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA723R - RETURN USER NAME BASED ON GROUP CONTROL INFO    DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA723R                                            *
     H*    CREATED: 02/25/99  C:56                                    *
     H*    DESCRIPTION: RETURN USER NAME BASED ON THE FMCXP/CXFLEX    *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/26/99 C:56 Project: MC208                                 *
     H*                If medical examiner code is '0' for the group  *
     H*                return 'NONE'.  This will signify to the EOMB  *
     H*                print program F0340R to not display the        *
     H*                medical examiner heading and field.            *
     H*
     H*  06/01/99 C:56 Project: MC152                                 *
     H*                Add '0' code for blank medical examiner        *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*****************************************************************
     F*
     FFMCXL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0           1
     C                   PARM                    @@SUB#            5 0           2
     C                   PARM                    @@FND             3 0           3
     C                   PARM                    @@FYR             3 0           4
     C                   PARM                    @@CASE            7 0           5
     C                   PARM                    @@EOB#            9 0           6
     C                   PARM                    @@USER           40             7
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  KEY LIST FOR EOMB TRANSACTION FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC CONTROL FILE LOGICAL
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKUSER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    KKSTAF
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
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    UPUSR         KKUSER
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C*
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CXFLEX        XXFLEX            1
     C*
     C                   SELECT
     C*** BEG ADD *** 06/01/99 ****************************************
     C     XXFLEX        WHENEQ    '0'
     C*** BEG CHG *** 07/26/99 ****************************************
     C**********           MOVEL*BLANKS   @@USER
     C                   MOVEL(P)  'NONE'        @@USER
     C*** END CHG *** 07/26/99 ****************************************
     C*** END ADD *** 06/01/99 ****************************************
     C     XXFLEX        WHENEQ    '1'
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EOIUSR        KKUSER
     C     UPKY          CHAIN     PUP01                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     XXFLEX        WHENEQ    '2'
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EOUSER        KKUSER
     C     UPKY          CHAIN     PUP01                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     XXFLEX        WHENEQ    '3'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMCMNU        KKSTAF
     C     UPKY1         CHAIN     PUP02                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     XXFLEX        WHENEQ    '4'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMSTF3        KKSTAF
     C     UPKY1         CHAIN     PUP02                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     XXFLEX        WHENEQ    '5'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMCMSU        KKSTAF
     C     UPKY1         CHAIN     PUP02                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     XXFLEX        WHENEQ    '6'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMCLSU        KKSTAF
     C     UPKY1         CHAIN     PUP02                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     XXFLEX        WHENEQ    '7'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMCLAD        KKSTAF
     C     UPKY1         CHAIN     PUP02                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     XXFLEX        WHENEQ    '8'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMCLFA        KKSTAF
     C     UPKY1         CHAIN     PUP02                              99
     C  N99              MOVEL(P)  UPUSNM        @@USER
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   RETURN
     C*
