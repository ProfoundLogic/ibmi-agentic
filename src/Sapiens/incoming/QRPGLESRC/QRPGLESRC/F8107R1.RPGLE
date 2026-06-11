     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F8107R1 - ADD RECORDS TO TEST
     H****************************************************************
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM ALLOWS THE USER TO ADD RECORDS TO         *
     H*       TEST ON WORK COMP SIDE                                 *
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
     FWMCML     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     F*
     FWMCMP     O    E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDP     O    E           K DISK
     F                                     RENAME(PCD01:PCD02)
     FWMPDL     IF   E           K DISK
     F*
     FWMPDP     O    E           K DISK
     F                                     RENAME(PPD01:PPD02)

     C*
     C*  KEY LIST FOR THE WCMS CLAIM FILE
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#             3 0
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
     C*  COPY WMCMP, WMCDP
     C*
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      QQFND         KKFND
     C*
     C     CMKY          CHAIN     PCM01                              99
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
     C     CMKY          READE     PCM01                                  99
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
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
