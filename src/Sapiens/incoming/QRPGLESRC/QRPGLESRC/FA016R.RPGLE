     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      * ***************************************************************
      /TITLE FA016R  -  RETURN FAEOP:EOSYS FLAG
      * ***************************************************************
      *  DESCRIPTION:                                                 *
      *                                                               *
      *  THIS PROGRAM WILL RETURN THE SYSTEM TYPE FLAG (FAEOP:EOSYS). *
      *                                                               *
      *   CALLED BY:  W7011R                                          *
      *                                                               *
      *    If RRN# is zero, EOMB was NOT found.                       *
      *    RETURN CODES:'1'   means WCMC Fund WCMS Fund # rec NOT fnd.*
      *                 '2'         EOMB is NOT found.                *
      *                 'P'         Physician                         *
      *                 'H'         Hospital                          *
      *                 'N'         Network                           *
      *                 'D'         Dental                            *
      *                 'U'         Pre/Re-Cert                       *
      *                 'M'         Medical Mgmt T/M                  *
      *                                                               *
      *  CREATED: 09/23/97  C:47  Project:  WC659                     *
      * ***************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      * ***************************************************************
      *  WCMC Group Master file
     FFMFNL1    IF   E           K DISK
      *__________________________________________________________
      *  WCMC EOB Master file
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
      *__________________________________________________________
      *

     D DBFDS           DS
     D  DBRRN                397    400B 0
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
      *
      *  Access the WCMC Fund X-REF file to get the Subscriber# and Group#
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           @@RC
     C                   Z-ADD     *ZERO         @@RRN
     C                   Z-ADD     *ZERO         @@SUB#
     C                   Z-ADD     *ZERO         @@GRP#
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     FNSUB#        @@SUB#
     C                   Z-ADD     FNFND         @@GRP#
      *
      *  Attempt EOMB file access.
      *
     C     EOKY          CHAIN     PEO01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      '2'           @@RC
     C                   Z-ADD     *ZERO         @@RRN
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EOSYS         @@RC
     C                   Z-ADD     DBRRN         @@RRN
     C                   ENDIF
      *
     C                   ENDIF
     C                   RETURN
