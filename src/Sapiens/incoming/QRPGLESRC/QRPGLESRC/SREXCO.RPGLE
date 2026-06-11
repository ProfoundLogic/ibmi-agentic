     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SREXCO - CHECK FOR REDUCED EXPENSE CONSTANT.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS CALLED TO DETERMINE IF WDDSP RECORDS    *
     H**      NEED TO BE GENERATED IN ORDER TO HANDLE REDUCED EXPENSE *
     H**      CONSTANT.  THE PROGRAM RETURNS A 'Y' VALUE IF A         *
     H**      STANDARD PREMIUM LEVEL DESIGNATION IS FOUND ON THE      *
     H**      HIGHEST EXPENSE CONSTANT RECORD DEFINITION.             *
     H**                                                              *
     H**      RETURN: QQFLAG - 'Y' IF REDUCED EXP CNST IS USED.       *
     H**                                                              *
     H**                                                              *
     H* CALLED FROM:                                                  *
     H*   SRDIS2  - SPREAD DISCOUNT PROGRAM                           *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    11/27/01            PROJECT: WC1059               *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
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
     C                   KFLD                    ELPFDT
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
     C                   PARM                    QQFLAG            1
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C*
     C                   MOVE      *BLANK        QQFLAG
     C                   Z-ADD     0             XXEXCO           11 2
     C                   Z-ADD     0             XXSTPR           11 2
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   Z-ADD     ECSTPR        XXSTPR
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXSTPR        IFNE      *ZERO
     C                   MOVE      'Y'           QQFLAG
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
