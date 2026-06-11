     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F8107R - DELETE RECORDS FROM TEST
     H****************************************************************
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM ALLOWS THE USER TO DELETE RECORD FROM     *
     H*       TEST                                                   *
     H*                                                              *
     H*  CALLED BY: F8108C                                           *
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
     FFMCML1    UF   E           K DISK
     FFMCDL     UF   E           K DISK
     FFMPDL     UF   E           K DISK
     FFMP1L     UF   E           K DISK

     C*
     C*  KEY LIST FOR THE WCMS CLAIM FILE
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKSUB#            5 0
     C                   KFLD                    KKFND             3 0
     C*
     C*  KEY LIST FOR THE WCMS CLAIM DEMO FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C*
     C*  DELETE FMCMP, FMCDP
     C*
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      QQSUB#        KKSUB#
     C                   MOVE      QQFND         KKFND
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVE      CMSS#         KKSS#
     C*
     C                   DELETE    PCM01
     C*
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PCD01
     C                   ENDIF
     C*
     C     CMKY          READE     PCM01                                  99
     C*
     C                   ENDDO
     C*
     C                   READ      PPD01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   DELETE    PPD01
     C                   READ      PPD01                                  99
     C*
     C                   ENDDO
     C*
     C                   READ      PP101                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   DELETE    PP101
     C                   READ      PP101                                  99
     C*
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
