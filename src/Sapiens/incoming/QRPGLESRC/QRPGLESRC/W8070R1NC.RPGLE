     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8070R1NC - Build Workfile for FORM 19
     H*****************************************************************
     H*                                                               *
     H* Called from:  W8070CNC                                        *
     H* Calls to:  None                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/19/08  C:61  PROJECT 81WCN                                 *
     H*                 Created.                                      *
     H*                                                               *
     H*****************************************************************
     FWMCMLR    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWXNIL     O    E           K DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  KEY LIST FOR THE CLAIM MASTER FILE
      *

     C     CMKY          KLIST
     C                   KFLD                    KKCO#
      *
      *  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *  KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
     C     *ENTRY        PLIST
     C                   PARM                    ##CO#             3
      *
     C     *LIKE         DEFINE    CMCO#         KKCO#
      *
     C                   MOVE      ##CO#         KKCO#
     C                   TIME                    DSDTTM
      *
      *  START READING THE CLAIMS FILE
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
      *
     C                   CLEAR                   PNI01
      *
      *  GET THE CLAIMANT NAME
      *
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
      *
     C                   MOVEL     CDMID         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
      *
     C                   MOVEL     QQLAST        QQFRST
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     ' '           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
      *
     C                   MOVEL     QQLAST        NICNAM
     C                   END
      *
      *  LOAD THE EMPLOYERS NAME
      *
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL     EMNAME        NIENAM
      *
      *  WRITE THE WORKFILE RECORD
      *
     C                   Z-ADD     KKCO#         NICO#
     C                   Z-ADD     CMFND         NIFND
     C                   Z-ADD     CMFYR         NIFYR
     C                   Z-ADD     CMCASE        NICASE
     C                   Z-ADD     CMEMP#        NIEMP#
     C                   Z-ADD     CMDIV         NIDIV
     C                   Z-ADD     CMIDT         NIIDT
     C                   Z-ADD     CMENTD        NIENTD
     C     DSDATE        MULT      10000.01      NIDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        NITIME
     C                   MOVE      Q1USER        NIUSER
     C                   MOVE      Q1PGM         NIPGM
     C                   WRITE     PNI01
      *
      *  GET THE NEXT CLAIM
      *
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO
      *
      *  END OF PROGRAM
      *
     C                   MOVE      '1'           *INLR
