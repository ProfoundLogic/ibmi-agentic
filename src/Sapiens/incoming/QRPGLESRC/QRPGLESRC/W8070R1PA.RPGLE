     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W8070R1PA - Build Workfile for Pennsylvania NOI
     H*****************************************************************
     H* Program Description:  This pgm builds the workfile for the    *
     H* Pennsylvania NOI Forms                                        *
     H*                                                               *
     H* Called from:  W8070CPA                                        *
     H* Calls to:  None                                               *
     H*                                                               *
     H*****************************************************************
     H* Created: 01/10/06  C: 132   Project: 114WKO                   *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
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

     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ##CO#             3
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   TIME                    DSDTTM
     C*
     C*  START READING THE CLAIMS FILE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   CLEAR                   PNI01
     C*
     C*  GET THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     CDMID         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C*
     C                   MOVEL     QQLAST        QQFRST
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     ' '           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C*
     C                   MOVEL     QQLAST        NICNAM
     C                   END
     C*
     C*  LOAD THE EMPLOYERS NAME
     C*
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL     EMNAME        NIENAM
     C*
     C*  WRITE THE WORKFILE RECORD
     C*
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
     C*
     C*  GET THE NEXT CLAIM
     C*
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
