     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTPADR - LOAD PHYSICAL ADDRESS INTO FMP1P
     H*****************************************************************
     H*  PROGRAMMER: C:32               PROJECT: MCMNT                *
     H*  CREATED: 06/21/95                                            *
     H*  THIS PROGRAM LOADS THE PHYSICAL ADDRESS FROM NMHSP & NMPHP   *
     H*  INTO NEW ADDRESS FLDS IN PMP1P.                              *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  60    GENERAL CHAIN INDICATOR                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H**************************************************************************
     H*
     FFMPDL     IF   E           K DISK
     FFMP1L     UF   E           K DISK

     C* KEY LIST DEFINITIONS
     C*
     C*  PHYSICIAN MASTER FILE
     C*
     C*
     C*  PAYEE DEMOGRAPHICS FILE(FMPDP/PPD01)
     C*
     C     PDKEY         KLIST
     C                   KFLD                    P1FEIN
     C                   KFLD                    P1MOD
     C*
     C                   Z-ADD     991231        XXDT              6 0
     C*
     C* READ THE PROVIDER AUXILIARY FILE
     C*
     C                   READ      PP101                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     PDKEY         CHAIN     PPD01                              62
     C*
     C     *IN62         IFEQ      '0'
     C*
     C                   MOVEL(P)  PDADR1        P1ADR1
     C                   MOVEL(P)  PDCTY         P1CTY
     C                   MOVEL(P)  PDST          P1PST
     C                   MOVEL     PDZIP         P1ZIP
     C                   Z-ADD     PDAREA        P1AREA
     C                   Z-ADD     PDPHN#        P1PHN#
     C                   MOVEL(P)  PDNAME        P1NAME
     C                   MOVEL(P)  PDFSN         P1FSN
     C                   UPDATE    PP101
     C*
     C                   ENDIF
     C*
     C                   READ      PP101                                  60
     C                   ENDDO                                                  ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
