     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  WA052R1 - API to Check for Duplicate Payments                *
     H*                                                               *
     H*  Called from: F8042R - Bill Upload Part 2                     *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 01/22/14 by c:282 for Project 133MOI              *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     FWARPL     IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FFAEOL31   IF   E           K DISK
      *
     dcharBTYP         s              1
     C*
     C*   REQUEST PAYMENT FILE KEY LIST (WARPL)
     C*
     C     RPKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C*  PAYMENT DETAIL FILE - PARTIAL
     C*
     C     PAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C*  EOB Work File
     C*
     C     keyFAEOL31    KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    intEOB#           9 0
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0          Company #
     C                   PARM                    @@FND             3 0          Fund
     C                   PARM                    @@FYR             3 0          Fund Year
     C                   PARM                    @@CASE            7 0          Case
     C                   PARM                    @@FROM            7 0          From Date
     C                   PARM                    @@TO              7 0          To Date
     C                   PARM                    @@AMT             9 2          Amount
     C                   PARM                    @@RECD            2            Reserve Code
     C                   PARM                    @@FEIN            9 0          FEIN #
     C                   PARM                    @@RETP            1            Reserve Type
     C                   PARM                    @@RTCD            2            Modifier
     C                   PARM                    @@BTYP            1            Bill Type
     C*
     C*  Check for Duplicate or Possible Duplicate Payment
     C*
     C                   MOVEL     *BLANKS       @@RTCD
     C*
     C     RPKY          CHAIN     PRP01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C*  Skip record if Payment Type is Not 'M' or 'P'.
     C*
     C     RPTYPE        IFEQ      'M'
     C     RPTYPE        OREQ      'P'
     C*
     C*  Payment Request is an Exact Duplicate if a Previous Request has
     C*  the Same Payment Reserve Code, Provider, Dates of Service,
     C*  Payment Amount, and the Sequence Number of Both WARPP records
     C*  are different.
     C*
      /Free
       intEOB# = RPeob#;
       Chain keyFAEOL31 peo01;
       If %Found(faeol31);
          charBTYP = EObtyp;
       EndIf;
      /End-Free
     C     @@RECD        IFEQ      RPRECD
     C     @@FEIN        ANDEQ     RPFEIN
     C     @@FROM        ANDEQ     RPFRDT
     C     @@TO          ANDEQ     RPTODT
     C     @@BTYP        ANDEQ     charBTYP
     C     @@AMT         ANDGT     0
     C     RPSTAT        ANDNE     'D'
     C                   MOVEL     'DR'          @@RTCD
     C                   LEAVE
     C                   ENDIF                                                  Exact Dup
     C                   ENDIF                                                  Skip
     C     RPKY          READE     PRP01                                  99
     C                   ENDDO                                                  DOW *IN99 RPKEY
     C*
     C* If no match in request file let's check the payment
     C*
     C     @@RTCD        IFEQ      *BLANKS
     C*
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C*  Skip record if Payment Type is Not 'M' or 'P'.
     C*
     C     PATYPE        IFEQ      'M'
     C     PATYPE        OREQ      'P'
      *
      /Free
       intEOB# = PAeob#;
       Chain keyFAEOL31 peo01;
       If %Found(faeol31);
          charBTYP = EObtyp;
       EndIf;
      /End-Free
      *  Payment Request is an Exact Duplicate if a Previous Payment has
      *  the Same Payment Reserve Code, Provider, Dates of Service and
      *  Payment Amount.
      *
     C     @@RECD        IFEQ      PARECD
     C     @@FEIN        ANDEQ     PAFEIN
     C     @@FROM        ANDEQ     PAFRDT
     C     @@TO          ANDEQ     PATODT
     C     @@BTYP        ANDEQ     charBTYP
     C     @@AMT         ANDGT     0
     C*
     C                   EXSR      SRVOID
     C     XXVOID        IFEQ      *OFF
     C                   MOVEL     'DP'          @@RTCD
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF                                                  Exact Dup
     C                   ENDIF                                                  Skip
     C*
     C     PAKY          READE     PPA01                                  99
     C                   ENDDO                                                  DOW *IN99 PAKY
     C*
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*================================================================
     C*  SRVOID - Determine if a Dup Payment has been Voided.
     C*================================================================
     C     SRVOID        BEGSR
     C*
     C     PABASQ        IFNE      *ZERO
     C     PAADST        ORNE      *BLANKS
     C                   MOVE      *ON           XXVOID            1
     C                   ELSE
     C                   MOVE      *OFF          XXVOID
     C                   ENDIF
     C*
     C                   ENDSR
