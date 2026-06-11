     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7023R - GROUP CHECK - VOID/STOP/REVERSAL ASSIGN BUL#'S
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/02/96 C:15 PROJ: 68AEF                                    *
     H*                FIXED CODE WHICH ASSIGNS THE BATCH UPDATE LOG#.*
     H*                THE COMPANY FILE HOLDS THE VALUE OF THE LAST   *
     H*                BUL# USED NOT THE NEXT AVAILABLE BUL#.         *
     H*                THEREFORE, 1 MUST BE ADDED TO COBUL1 BEFORE    *
     H*                ASSIGNING IT TO A PAYMENT.                     *
     H*****************************************************************
     FWAGCL     IP   E           K DISK
     FWDPAL9    UF   E           K DISK
     FSMCOL     UF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  CLAIM CHECK PAYMENT DETAIL FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    GCCO#
     C                   KFLD                    GCTRDT
     C                   KFLD                    GCCHK#
     C*
     C     *LIKE         DEFINE    GCCO#         XXCO#
     C     *LIKE         DEFINE    COBUL1        XXBUL#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     *IN20         IFEQ      *OFF
     C                   Z-ADD     GCCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
     C                   Z-ADD     COBUL1        XXBUL#
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C*  ASSIGN A UNIQUE BUL# TO THE PAYMENTS FOR EACH GROUP CHECK
     C*
     C     PAKY1         SETLL     PPA01
     C     PAKY1         READE     PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     PAFND         IFEQ      GCFND
     C                   ADD       1             XXBUL#
     C                   Z-ADD     XXBUL#        PABUL1
     C                   UPDATE    PPA01
     C                   END
     C*
     C     PAKY1         READE     PPA01                                  60
     C                   ENDDO
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     END           TAG
     CLR   COKEY         CHAIN     PCO01                              60
     CLR   *IN60         IFEQ      *OFF
     CLR                 Z-ADD     XXBUL#        COBUL1
     CLR                 UPDATE    PCO01
     CLR                 END
