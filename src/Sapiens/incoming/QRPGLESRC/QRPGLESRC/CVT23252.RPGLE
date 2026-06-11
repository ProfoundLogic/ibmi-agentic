     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO CORRECT CLAIM INITIAL ENTRY FIELDS
     H*****************************************************************
     H* Description:  This pgm corrects the claim initial entry fields*
     H*   which were changed inadvertantly.                           *
     H*                                                               *
     H*****************************************************************
     FWMCML10   UF   E           K DISK
     FWMCHL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Key list for the Claim History file
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C                   TIME                    DSDTTM
     C*-------------------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PCO01                                  99
     C                   EXCEPT    HEAD
     C     *IN99         DOWEQ     '0'
     C                   Z-ADD     COCO#         XXCO#
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C     CMIPGM        IFEQ      'W0070R07'
     C     CMIPGM        OREQ      'W0070R08'
     C     CMIPGM        OREQ      'W0070R09'
     C     CMIPGM        OREQ      'W0070R12'
     C     CMIPGM        OREQ      'W0070R6'
     C     CMIPGM        OREQ      'W8070RFL'
     C     CMIPGM        OREQ      'W4112R'
     C     CMIPGM        OREQ      'W4112R01'
     C     CMIPGM        OREQ      'W4112R09'
     C     CMIPGM        OREQ      'W4112R12'
     C     CMIPGM        OREQ      'SC010R'
     C     CMIPGM        OREQ      'W0080R'
     C     CMIPGM        OREQ      'W0190R'
     C     CMIPGM        OREQ      'W0271R'
     C     CMIPGM        OREQ      'W7011R'
     C     CMIPGM        OREQ      'W8608R'
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C*  Retrieve the first claim history record for the claim and load
     C*    the claim master initial entry fields.
     C*
     C     CHKY          CHAIN     PCH01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     CHUSER        CMIUSR
     C                   MOVEL(P)  'W0070R01'    CMIPGM
     C                   Z-ADD     CHDT          CMIDT
     C                   Z-ADD     CHTIME        CMITM
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Get the next claim record.
     C*
     C     CMKY          READE     PCM01                                  99
     C                   ENDDO
     C*
     C*  Get the next company record.
     C*
     C                   READ      PCO01                                  99
     C                   ENDDO
     C*
     C*  End of program
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           69 'Change Claim Initial'
     O                                           82 'Entry Fields'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            3 'Grp'
     O                                            8 'FYR'
     O                                           17 '   Case'
     O                                           27 'Init pgm'
     O                                           38 'Init dt'
     O                                           48 'Init tm'
     O                                           60 'Init user'
     O          E            DET         1
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       CMIPGM              29
     O                       CMIDT               38
     O                       CMITM               48
     O                       CMIUSR              61
