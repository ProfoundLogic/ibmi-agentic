     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRFEXP - RETURN FULL EXPENSE CONSTANT FOR WDEHP.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS CALLED TO DETERMINE THE FULL ANNUAL     *
     H**      EXPENSE CONSTANT FOR AN EMPLOYER HISTORY RECORD.  THE   *
     H**      PROGRAM WILL READ THE EXPENSE CONSTANT FILE FOR EACH    *
     H**      DIVISION WHICH HAS A WDEHP RECORD ON OR BEFORE THE      *
     H**      TRANSACTION DATE WHICH IS PASSED INTO THE PROGRAM.      *
     H**      THE PROGRAM WILL RETURN THE FULL ANNUAL EXPENSE         *
     H**      CONSTANT AMOUNT.                                        *
     H**                                                              *
     H**      RETURN: QQPREM - FULL ANNUAL EXPENSE CONSTANT AMT.      *
     H**                                                              *
     H* CALLED FROM:                                                  *
     H*   VARIOUS                                                     *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    04/04/02            PROJECT: SUPPORT              *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/23/08 C:190 PROJECT: WC1754                                *
     H*                LOGIC IS ADDED SO THAT PROGRAM CAN RETURN      *
     H*                THE NEW SECOND LEVEL OF EXPENSE CONSTANT.      *
     H*                                                               *
     H* 01/31/03 C:65  PROJECT: WC1059                                *
     H*                ADD IN CODE TO CALCULATE THE REDUCED EXP CNST. *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWDEHL6    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDECL     IF   E           K DISK

     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE EMPLOYER HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTRDT
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXFYR
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  EXPENSE CONSTANT FILE
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    EHPFDT
     C*
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQTRDT            7 0
     C                   PARM                    QQPREM           11 2
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    EHTRDT        XXTRDT
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQTRDT        XXTRDT
     C*
     C                   Z-ADD     0             QQPREM
     C                   Z-ADD     0             XXEXCO           11 2
     C*** BEGIN ADD ***  01/31/03  ************************************
     C                   Z-ADD     0             XXPREM           11 2
     C*
     C* READ THROUGH EMPLR RECORDS FIRST TIME TO GATHER STD PREMIUM.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY1         READPE    PEH01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   ADD       EHWSPR        XXPREM
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*** END ADD *****  01/31/03  ************************************
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY1         READPE    PEH01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      EHRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
     C*** BEGIN ADD ***  01/31/03  ************************************
      *
     C*** BEGIN ADD ***  12/23/08  ************************************
      *
     C     *IN60         IFEQ      *OFF
     C     EHPFDT        ANDNE     EHPTDT
     C     XXPREM        ANDLT     ECSTP3
     C     ECSTP3        ANDNE     *ZERO
     C                   Z-ADD     ECEXC3        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  12/23/08  ************************************
      * REDUCE THE EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     EHPFDT        ANDNE     EHPTDT
     C     XXPREM        ANDLT     ECSTPR
     C     ECSTPR        ANDNE     *ZERO
     C                   Z-ADD     ECEXC2        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  01/31/03  ************************************
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C     EHPFDT        ANDNE     EHPTDT
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXEXCO        IFNE      *ZERO
     C                   Z-ADD     XXEXCO        QQPREM
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
