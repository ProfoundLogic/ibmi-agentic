     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FA015R  - Access MC EOMB using WCMS key fields
     H*****************************************************************
     H*  DESCRIPTION:
     H*    This program will access the WCMC EOMB file using WCMS info*
     H*    The program will return:                                   *
     H*              RRN# of EOMB record                              *
     H*              Return Code                                      *
     H*              WCMC Subscriber #                                *
     H*              WCMC Group #                                     *
     H*                                                               *
     H*    If RRN# is zero, EOMB was NOT found.                       *
     H*    Return code *BLANK means EOMB Found in active EOMBs.       *
     H*      "     "    'D'   means EOMB is Deleted.                  *
     H*      "     "    'N'   means EOMB is NOT found.                *
     H*      "     "    'F'   means WCMC Fund WCMS Fund # rec NOT fnd.*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 08/06/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/29/00  C:62  PROJECT: MC048 22221                         *
     H*                  PASS IN RCN.                                 *
     H*                                                               *
     H*  05/10/99  C:94  PROJECT: SUP17993                            *
     H*                  RETURN FEIN AND MOD                          *
     H*                                                               *
     H*  01/23/99  C:94  PROJECT: 25MKL                               *
     H*                  RETURN ADJUDICATION STATE                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  WCMC Group Master file
     FFMFNL1    IF   E           K DISK
      *__________________________________________________________
      *  WCMC EOB Master file
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
      *__________________________________________________________
      *  WCMC Deleted EOB  file
     FFDEOL1    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
      *__________________________________________________________
      *

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** BEG ADD *** 01/23/99 ***************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END ADD *** 01/23/99 ***************************************
      *
      *  WCMC Fund file
      *

     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@WCMS
      *
      *  WCMC EOMB file
      *
     C     EOKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@GRP#
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
      *
     C     *LIKE         DEFINE    EOSUB#        @@SUB#
     C     *LIKE         DEFINE    EOFND         @@GRP#
     C     *LIKE         DEFINE    EOCO#         @@CO#
     C     *LIKE         DEFINE    EOFND         @@WCMS
     C     *LIKE         DEFINE    EOFYR         @@FYR
     C     *LIKE         DEFINE    EOCASE        @@CASE
     C     *LIKE         DEFINE    EOEOB#        @@EOB#
     C*** BEG ADD *** 05/10/99 ***************************************
     C     *LIKE         DEFINE    EOFEIN        @@FEIN
     C     *LIKE         DEFINE    EOMOD         @@MOD
     C*** END ADD *** 05/10/99 ***************************************
      *
      *================================================================
      *  Program Start
      *================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                          Company #
     C                   PARM                    @@WCMS                         WCMS Fund
     C                   PARM                    @@FYR                          Policy Year
     C                   PARM                    @@CASE                         Claim #
     C                   PARM                    @@EOB#                         EOMB #
     C                   PARM                    @@RRN             9 0          EOMB RRN#
     C                   PARM                    @@SUB#                         Subscriber #
     C                   PARM                    @@GRP#                         WCMC Group #
     C                   PARM                    @@RC              1            Return Code
     C*** BEG ADD *** 01/23/99 ***************************************
     C                   PARM                    @@AST             2
     C*** END ADD *** 01/23/99 ***************************************
     C*** BEG ADD *** 05/10/99 ***************************************
     C                   PARM                    @@FEIN
     C                   PARM                    @@MOD
     C*** END ADD *** 05/10/99 ***************************************
     C*** BEGIN ADD *** 03/29/00 **************************************
     C                   PARM                    @@RCN             3 0
     C*** END   ADD *** 03/29/00 **************************************
      *
      *  Access the WCMC Fund X-REF file to get the Subscriber# and Group#
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      'F'           @@RC
     C                   Z-ADD     *ZERO         @@RRN
     C                   Z-ADD     *ZERO         @@SUB#
     C                   Z-ADD     *ZERO         @@GRP#
     C                   RETURN
     C                   ENDIF
      *
     C                   Z-ADD     FNSUB#        @@SUB#
     C                   Z-ADD     FNFND         @@GRP#
      *
      *  Attempt EOMB file access.
      *
     C     EOKY          CHAIN     PEO01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      'N'           @@RC
     C                   Z-ADD     *ZERO         @@RRN
      *
      *  Not found in EOMB file, Attempt Deleted EOMB file access.
      *
     C     EOKY          CHAIN     PEO02                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'D'           @@RC
     C                   RETURN
     C                   ENDIF
     C*** BEGIN ADD *** 03/29/00 **************************************
     C*
     C                   ELSE
     C*
     C     Q1PRMS        IFGT      12
     C*
     C     @@RCN         IFNE      EORCN
     C*
     C                   MOVE      'F'           @@RC
     C                   Z-ADD     *ZERO         @@RRN
     C                   Z-ADD     *ZERO         @@SUB#
     C                   Z-ADD     *ZERO         @@GRP#
     C                   RETURN
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 03/29/00 **************************************
     C                   ENDIF
      *
     C                   Z-ADD     DBRRN         @@RRN
     C*** BEG ADD *** 01/23/99 **************************************
     C     Q1PRMS        IFGT      9
     C                   MOVE      EOAST         @@AST
     C                   ENDIF
     C*** END ADD *** 01/23/99 **************************************
      *
     C*** BEG ADD *** 05/10/99 **************************************
     C     Q1PRMS        IFGT      10
     C                   MOVE      EOFEIN        @@FEIN
     C                   MOVE      EOMOD         @@MOD
     C                   ENDIF
     C*** END ADD *** 05/10/99 **************************************
     C                   RETURN
