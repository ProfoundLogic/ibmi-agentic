     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  WA052R - Checks for duplicate payments                       *
     H*                                                               *
     H*  Called from: F8262R - Concentra Upload Process               *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 12/11/2003 by c:62 for Project 129MEQ             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDPAL     IF   E           K DISK
     FWARPL     IF   E           K DISK

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
     C     @@RECD        IFEQ      RPRECD
     C     @@FEIN        ANDEQ     RPFEIN
     C     @@FROM        ANDEQ     RPFRDT
     C     @@TO          ANDEQ     RPTODT
     C     @@AMT         ANDEQ     RPAMT
     C     RPSTAT        ANDNE     'D'
     C                   MOVEL     'DR'          @@RTCD
     C                   LEAVE
     C                   ELSE
     C*
     C*  Payment Request is a Possible Duplicate if a Previous Request
     C*  has the Same Reserve Code, Provider, and the Dates of Service
     C*  for the Selected Request overlap Dates of Service for the
     C*  Previous Request, and the Sequence Number of Both WARPP Records
     C*  are Different.
     C*
     C                   MOVE      *OFF          MATCH             1
     C*
     C     @@RETP        IFEQ      RPRETP
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRETP=RPRETP
     C*
     C     @@RECD        IFEQ      RPRECD
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRECD=RPRECD
     C*
     C     MATCH         IFEQ      *ON
     C     @@FEIN        ANDEQ     RPFEIN
     C     RPSTAT        ANDNE     'D'
     C     @@FROM        IFGE      RPFRDT
     C     @@FROM        ANDLE     RPTODT
     C     @@TO          ORGE      RPFRDT
     C     @@TO          ANDLE     RPTODT
     C     @@FROM        ORLE      RPFRDT
     C     @@TO          ANDGE     RPTODT
     C                   MOVEL     'PR'          @@RTCD
     C                   LEAVE
     C                   ENDIF                                                  Dates Overlap
     C                   ENDIF                                                  Possible Dup
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
      *  Payment Request is an Exact Duplicate if a Previous Payment has
      *  the Same Payment Reserve Code, Provider, Dates of Service and
      *  Payment Amount.
      *
     C     @@RECD        IFEQ      PARECD
     C     @@FEIN        ANDEQ     PAFEIN
     C     @@FROM        ANDEQ     PAFRDT
     C     @@TO          ANDEQ     PATODT
     C     @@AMT         ANDEQ     PAAMT
     C*
     C                   EXSR      SRVOID
     C     XXVOID        IFEQ      *OFF
     C                   MOVEL     'DP'          @@RTCD
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C*
     C*  Payment Request is a Possible Duplicate if a Previous Payment
     C*  has the Same Payment Reserve Code, Provider, and the Dates
     C*  of Service for the NEW Request Overlap Dates of Service for
     C*  the Previous Payment.
     C*
     C                   MOVE      *OFF          MATCH
     C*
     C     @@RETP        IFEQ      PARETP
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRETP=PARETP
     C*
     C     @@RECD        IFEQ      PARECD
     C                   MOVE      *ON           MATCH
     C                   ENDIF                                                  SVRECD=PARECD
     C*
     C     MATCH         IFEQ      *ON
     C     @@FEIN        ANDEQ     PAFEIN
     C     @@FROM        IFGE      PAFRDT
     C     @@FROM        ANDLE     PATODT
     C     @@TO          ORGE      PAFRDT
     C     @@TO          ANDLE     PATODT
     C     @@FROM        ORLE      PAFRDT
     C     @@TO          ANDGE     PATODT
     C*
     C                   EXSR      SRVOID
     C     XXVOID        IFEQ      *OFF
     C                   MOVEL     'PP'          @@RTCD
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF                                                  Dates Overlap
     C                   ENDIF                                                  Possible Dup
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
