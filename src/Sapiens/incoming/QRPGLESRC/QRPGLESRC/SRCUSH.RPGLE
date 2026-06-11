     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SRCUSH - WORKERS COMP RESERVE CUSHION REMAINING CALCULATOR
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*       THIS PROGRAM CALCULATES THE REMAINING RESERVE CUSHION   *
     H*       AVAILABLE FOR A CLAIM.                                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0080R - REQUEST PAYMENT                                   *
     H*    W0120R - MANUAL PAYMENTS/PAYMENT CORRECTIONS               *
     H*    W0583R2- CHANGE PAYMENT REQUEST                            *
     H*    W0581R - PAYMENT INQUIRY                                   *
     H*    W0581R1- PENDED PAYMENT SEARCH                             *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:46                                             *
     H*  CREATED:    01/09/96    PROJECT: WC411                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/08/03 C:62 PROJECT: MC129 38490                           *
     H*                DON'T INCREASE THE RESERVE IF RECORD ONLY      *
     H*                                                               *
     H*  05/30/96 C:22 PROJECT:                                       *
     H*                MODIFIED ACCUMULATION OF PAYMENTS              *
     H*                                                               *
     H* 05/13/97 C:62 PROJECT WC674                                   *
     H*               ENSURE ALL PROGRAMS USE CMLTFL = I/M/R NOT Y/N  *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*    60 - UTILITY INDICATOR                                     *
     H*                                                               *
     H*****************************************************************
     FWDRCL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWARPL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C****************************************************************
     C*   PROGRAM FILE KEY LISTINGS
     C****************************************************************
     C*
     C*   KEY LIST FOR THE RESERVE CUSHION MASTER FILE
     C*
     C     RCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRVCD
     C                   KFLD                    XXEFDT
     C*
     C     RCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRVCD
     C*
     C*   KEY LIST FOR THE CLAIM STATUS CHANGE HISTORY FILE
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXHIVL
     C*
     C     CHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE DETAIL PAYMENT FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C*
     C*  KEY LIST FOR THE REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C****************************************************************
     C*   PROGRAM *LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    RCCO#         XXCO#
     C     *LIKE         DEFINE    RCFND         XXFND
     C     *LIKE         DEFINE    RCRVCD        XXRVCD
     C     *LIKE         DEFINE    RCEFDT        XXEFDT
     C     *LIKE         DEFINE    CHFYR         XXFYR
     C     *LIKE         DEFINE    CHCASE        XXCASE
     C     *LIKE         DEFINE    CHEFDT        XXHIVL
     C     *LIKE         DEFINE    RCCO#         QQCO#
     C     *LIKE         DEFINE    RCFND         QQFND
     C     *LIKE         DEFINE    CHFYR         QQFYR
     C     *LIKE         DEFINE    CHCASE        QQCASE
     C     *LIKE         DEFINE    RCRVCD        QQRECD
     C     *LIKE         DEFINE    RCMAMT        QQCUSH
     C     *LIKE         DEFINE    RCMAMT        XXCUSH
     C     *LIKE         DEFINE    RCMAMT        XXSUBT
     C*
     C****************************************************************
     C*   PROGRAM ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRECD
     C                   PARM                    QQLTFL            1
     C                   PARM                    QQCUSH
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQRECD        XXRVCD
     C                   MOVE      *BLANK        XXRCFL            1
     C                   Z-ADD     *ZERO         XXCUSH
     C*
     C*  GET EFFECTIVE DATE OF CLOSING
     C*
     C                   Z-ADD     *HIVAL        XXHIVL
     C     CHKY          SETGT     WMCHL
     C     CHKY1         READPE    WMCHL                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     'C'           ANDEQ     CHOCST
     C*
     C*   CHECK TO SEE IF PAYMENT TYPE WILL ALLOW PAYMENT DUE TO A
     C*   RESERVE CUSHION. THE RESERVE CODE ASSOCIATED WITH THE
     C*   PAYMENT TYPE MUST EXIST IN THE RESERVE CUSHION FILE.
     C*
     C                   Z-ADD     CHEFDT        XXEFDT
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** QQLTFL    IFEQ 'Y'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     QQLTFL        IFEQ      'I'
     C*** END   ADD *** 05/13/97 **************************************
     C                   Z-ADD     RCLAMT        XXCUSH                         LOST TIME AMT
     C                   ELSE
     C*** BEGIN DEL *** 05/13/97 **************************************
     C*********  QQLTFL    IFEQ 'N'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     QQLTFL        IFEQ      'M'
     C*** BEGIN DEL *** 08/08/03 **************************************
     C********** QQLTFL    OREQ 'R'
     C*** END   DEL *** 08/08/03 **************************************
     C*** END   ADD *** 05/13/97 **************************************
     C                   Z-ADD     RCMAMT        XXCUSH                         MED ONLY AMT
     C                   END                                                    CMLTFL = 'M'
     C                   END                                                    CMLTFL = 'L'
     C*
     C     XXCUSH        IFNE      0
     C                   MOVE      'Y'           XXRCFL
     C                   END
     C*
     C                   END                                                    RCKY = '0'
     C                   END                                                    CHKY = '0'
     C*
     C* IF THE RESERVE CUSHION FLAG IS 'Y' THEN REDUCE CUSHION AMOUNT
     C* BY THE AMOUNT OF PAYMENTS MADE SINCE CLOSING THE CLAIM.
     C*
     C     XXRCFL        IFEQ      'Y'
     C*
     C                   Z-ADD     0             XXSUBT
     C*
     C*  ACCUMULATE PAYMENTS RELEASED
     C*
     C     RPKY          SETLL     PRP01
     C     PAKY          READE     PRP01                                  60
     C     *IN60         DOWEQ     '0'
     C     RPRECD        IFEQ      XXRVCD
     C     RPSTAT        ANDEQ     ' '
     C                   ADD       RPAMT         XXSUBT
     C                   END
     C     RPKY          READE     PRP01                                  60
     C                   END                                                    *IN60 DOWEQ '0'
     C*
     C*  ACCUMULATE PAYMENTS MADE
     C*
     C     PAKY2         SETLL     PPA01
     C     PAKY          READE     PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C     PARECD        IFEQ      XXRVCD
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         XXSUBT
     C                   ELSE
     C                   SUB       PAAMT         XXSUBT
     C                   END
     C                   END
     C     PAKY          READE     PPA01                                  60
     C                   END                                                    *IN60 DOWEQ '0'
     C*
     C*  SUBTRACT TOTAL PAYMENT FROM THE CUSHION AMOUNT.  IF
     C*  THE CUSHION IS LESS THAN ZERO, SET IT TO ZERO.
     C*
     C                   SUB       XXSUBT        XXCUSH
     C     XXCUSH        IFLT      *ZERO
     C                   Z-ADD     *ZERO         XXCUSH
     C                   END
     C                   END
     C*
     C                   Z-ADD     XXCUSH        QQCUSH
     C*
     C                   MOVE      *ON           *INLR
