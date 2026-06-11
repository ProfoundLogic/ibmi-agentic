     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE EMPLOYER DETAIL UPDATE
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*           THIS PROGRAM WILL UPDATE THE OUTSTANDING RESERVE,   *
     H*    TOTAL CLAIMS PAID, TOTAL # OF CLAIMS AND TOTAL # OF CLOSED *
     H*    CLAIMS FIELD IN THE EMPLOYER DETAIL FILE.  THIS PGM WILL   *
     H*    BE RUN IN THE MONTHLY PART OF THE DAILY BATCH PGM.         *
     H*                                                               *
     H* CALLED FROM: W6000C                                           *
     H* CALL TO:     NONE                                             *
     H*                                                               *
     H* PROGRAMMER: DAVID MONCRIEF                                    *
     H* CREATED:    09/05/91    PROJECT:  WC062                       *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/04/10 C:190 PROJECT 25WRE                                 *
     H*                 Update new field WDELP/ELT#CC                 *
     H*                                                               *
     H*  10/25/00 C:15 Support: WC102/24753                           *
     H*                Chgd the value loaded to XXEFDT because when   *
     H*                the pgm was trying to retrieve the most recent *
     H*                Claim History record, it was not getting any   *
     H*                records with a date >= 1/1/2000.               *
     H*                                                               *
     H*  12/17/91 JJJ  PROJECT WC105                                  *
     H*                DONT UPDATE MAINT FIELDS IN WDELP BECAUSE IT   *
     H*                DOES NOT ALLOW US AN ACCURATE ACCOUNT OF WHEN  *
     H*                THE LAST USER CHANGE OCCURRED.                 *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*      50  -   MAINLINE LOOP (WDELP)                            *
     H*      51  -   SECONDARY LOOP (WMCMP)                           *
     H*      60  -   CHAIN TO WMCHP                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK
     FWMCML     IF   E           K DISK
     FWMCHL     IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              DVM 09/05/91
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              DVM 09/05/91
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2

     C****************************************************************
     C*    LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    CHCASE        XXCASE
     C     *LIKE         DEFINE    CHEFDT        XXEFDT
     C*
     C                   Z-ADD     0             XXTORS            9 2          OUT. RES
     C                   Z-ADD     0             XXTPMT           11 2          PAYMENTS
     C                   Z-ADD     0             XXSORS            9 2          SUB TOTL OUT. RES
     C                   Z-ADD     0             XXSPMT           11 2          SUB TOTL PAYMENTS
     C                   Z-ADD     *ZERO         XXTOTA            7 0          # OF CLAIMS
     C                   Z-ADD     *ZERO         XXTOTB            7 0          # OF CL CLAIMS
     C*** BEGIN ADD *** 01/04/10 **************************************
     C                   Z-ADD     *ZERO         XXTOTC            7 0          TOTAL CLAIMS COUNT
     C*** END   ADD *** 01/04/10 **************************************
     C*** BEGIN ADD *** 10/25/00 **************************************
     C                   MOVE      *HIVAL        XXEFDT
     C*** END ADD ***** 10/25/00 **************************************
     C*
     C****************************************************************
     C*    KEY LISTING
     C****************************************************************
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  CLAIM HISTORY MASTER FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C*
     C****************************************************************
     C*    ENTRY PARMS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*
     C****************************************************************
     C*    SET UP AND INITIALIZE
     C****************************************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C*
     C*****************************************************************
     C*  UPDATE EMPLOYER DETAIL FILE WITH CASE TOTALS FOR MONTH END
     C*****************************************************************
     C*  READ THE EMPLOYER RECORDS
     C*
     C     ELKEY         SETLL     PEL01
     C     *IN50         DOUEQ     '1'
     C     XXCO#         READE     PEL01                                  50
     C     *IN50         IFEQ      '0'
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     0             XXTORS                         OUT. RES
     C                   Z-ADD     0             XXTPMT                         PAYMENTS
     C                   Z-ADD     0             XXSORS                         SUB TOTL OUT. RES
     C                   Z-ADD     0             XXSPMT                         SUB TOTL PMTS
     C                   Z-ADD     *ZERO         XXTOTA
     C                   Z-ADD     *ZERO         XXTOTB
     C*** BEGIN ADD *** 01/04/10 **************************************
     C                   Z-ADD     *ZERO         XXTOTC
     C*** END   ADD *** 01/04/10 **************************************
     C*
     C*  READ THE CLAIM RECORDS
     C*
     C     CMKEY         SETLL     PCM01
     C     *IN51         DOUEQ     '1'
     C     CMKEY         READE     PCM01                                  51
     C     *IN51         IFEQ      '0'
     C*
     C     CMPCTR        IFGE      1
     C                   ADD       1             XXTOTA                         # OF CLAIMS
     C                   END
     C*** BEGIN ADD *** 01/04/10 **************************************
     C                   ADD       1             XXTOTC                         TOT.CLAIMS COUNT
     C*** END   ADD *** 01/04/10 **************************************
     C*
     C                   XFOOT     AR            XXSPMT                         TOTAL PMT
     C                   XFOOT     RR            XXSORS                         TOTAL OUT. RES
     C*
     C*  ADD CLAIM AMOUNT TO TOTAL AMOUNT FIELD
     C*
     C                   ADD       XXSPMT        XXTPMT                         TOTAL PMT
     C                   ADD       XXSORS        XXTORS                         TOTAL OUT. RES
     C*
     C*  CHECK CLAIM STATUS
     C*
     C                   MOVE      CMCASE        XXCASE
     C*** BEGIN DLT *** 10/25/00 **************************************
     C******************** Z-ADD999999    XXEFDT
     C*** END DLT ***** 10/25/00 **************************************
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHCO#         ANDEQ     XXCO#
     C     CHFND         ANDEQ     XXFND
     C     CHFYR         ANDEQ     XXFYR
     C     CHCASE        ANDEQ     XXCASE
     C     CHOCST        ANDEQ     'C'
     C                   ADD       1             XXTOTB
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   Z-ADD     XXTORS        ELOUTS
     C                   Z-ADD     XXTPMT        ELTCLP
     C                   Z-ADD     XXTOTA        ELT#CL
     C                   Z-ADD     XXTOTB        ELTCLC
     C*** BEGIN ADD *** 01/04/10 **************************************
     C                   Z-ADD     XXTOTC        ELT#CC
     C*** END   ADD *** 01/04/10 **************************************
     C                   UPDATE    PEL01
     C*
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
