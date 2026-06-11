     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F8107R2 - ADD RECORDS TO TEST
     H****************************************************************
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM ALLOWS THE USER TO ADD RECORDS TO         *
     H*       TEST MANAGED CARE                                      *
     H*                                                              *
     H*  CALLED BY: F8107R1                                          *
     H*                                                              *
     H*  CALLS TO:  NONE                                             *
     H*                                                              *
     H*  PROGRAMMER: C:79                                            *
     H*  CREATED:    09/24/04    PROJECT: 48MFN                      *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H****************************************************************
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H****************************************************************
     FFMPDL     IF   E           K DISK
     F*
     FFMPDP     O    E           K DISK
     F                                     RENAME(PPD01:PPD02)
     FFMP1L     IF   E           K DISK
     FFMP1P     O    E           K DISK
     F                                     RENAME(PP101:PP102)
     FFMCML1    IF   E           K DISK
     F*
     FFMCMP     O    E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMCDL     IF   E           K DISK
     F*
     FFMCDP     O    E           K DISK
     F                                     RENAME(PCD01:PCD02)

     C*
     C*  KEY LIST FOR THE WCMS CLAIM FILE
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKSUB#            5 0
     C                   KFLD                    KKFND             3 0
     C*
     C*  COPY FMCMP, FMCDP
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      QQSUB#        KKSUB#
     C                   MOVE      QQFND         KKFND
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVE      CMSS#         KKSS#
     C*
     C                   WRITE     PCM02
     C*
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   WRITE     PCD02
     C                   ENDIF
     C*
     C     CMKY1         READE     PCM01                                  99
     C*
     C                   ENDDO
     C*
     C* COPY THE PROVIDER FILE
     C*
     C                   READ      PPD01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   WRITE     PPD02
     C*
     C                   READ      PPD01                                  99
     C*
     C                   ENDDO
     C*
     C* COPY THE PROVIDER FILE
     C*
     C                   READ      PPD01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   WRITE     PPD02
     C*
     C                   READ      PPD01                                  99
     C*
     C                   ENDDO
     C*
     C* COPY THE PROVIDER FILE AUX
     C*
     C                   READ      PP101                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   WRITE     PP102
     C*
     C                   READ      PP101                                  99
     C*
     C                   ENDDO
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
