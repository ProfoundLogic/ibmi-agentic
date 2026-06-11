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
     FNMHSL1    IF   E           K DISK
     FNMPHL1    IF   E           K DISK

     C* KEY LIST DEFINITIONS
     C*
     C*  HOSPITAL MASTER FILE
     C*
     C     *LIKE         DEFINE    PDDT          XXDT
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     HSKEY         KLIST
     C                   KFLD                    P1FEIN
     C                   KFLD                    P1MOD
     C*
     C     HSKY1         KLIST
     C                   KFLD                    P1FEIN
     C                   KFLD                    P1MOD
     C                   KFLD                    XXDT
     C*
     C*  PHYSICIAN MASTER FILE
     C*
     C     PHKEY         KLIST
     C                   KFLD                    P1FEIN
     C                   KFLD                    P1MOD
     C*
     C     PHKY1         KLIST
     C                   KFLD                    P1FEIN
     C                   KFLD                    P1MOD
     C                   KFLD                    XXDT
     C*
     C*  PAYEE DEMOGRAPHICS FILE(FMPDP/PPD01)
     C*
     C     PDKEY         KLIST
     C                   KFLD                    P1FEIN
     C                   KFLD                    P1MOD
     C*
     C                   Z-ADD     0991231       XXDT
     C*
     C* READ THE PROVIDER AUXILIARY FILE
     C*
     C                   READ      PP101                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      'N'           XXUPD             1
     C     PDKEY         CHAIN     PPD01                              62
     C*
     C     *IN62         IFEQ      '0'
     C*
     C     PDTYPE        IFEQ      '1'
     C     PHKY1         SETGT     PPH01
     C     PHKEY         READPE    PPH01                                  62
     C     *IN62         IFEQ      *OFF
     C     PHB1A1        IFNE      *BLANKS
     C                   MOVE      'Y'           XXUPD             1
     C                   MOVEL(P)  PHB1A1        P1ADR1
     C                   MOVEL(P)  PHB1A2        P1ADR2
     C                   MOVEL(P)  PHB1CY        P1CTY
     C                   MOVEL(P)  PHB1ST        P1PST
     C                   MOVEL     PHB1ZP        P1ZIP
     C                   MOVE      PHB1ZS        P1ZIP
     C                   Z-ADD     PHB1AR        P1AREA
     C                   Z-ADD     PHB1PH        P1PHN#
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     PDTYPE        IFEQ      '4'
     C     HSKY1         SETGT     PHS01
     C     HSKEY         READPE    PHS01                                  62
     C     *IN62         IFEQ      *OFF
     C     HSB1A1        IFNE      *BLANKS
     C                   MOVE      'Y'           XXUPD             1
     C                   MOVEL(P)  HSB1A1        P1ADR1
     C                   MOVEL(P)  HSB1A2        P1ADR2
     C                   MOVEL(P)  HSB1CY        P1CTY
     C                   MOVEL(P)  HSB1ST        P1PST
     C                   MOVEL     HSB1ZP        P1ZIP
     C                   MOVE      HSB1ZS        P1ZIP
     C                   Z-ADD     HSB1AR        P1AREA
     C                   Z-ADD     HSB1PH        P1PHN#
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXUPD         IFEQ      'N'
     C                   MOVEL(P)  PDADR1        P1ADR1
     C                   MOVE(P)   *BLANKS       P1ADR2
     C                   MOVEL(P)  PDCTY         P1CTY
     C                   MOVEL(P)  PDST          P1PST
     C                   Z-ADD     PDZIP         P1ZIP
     C                   Z-ADD     PDAREA        P1AREA
     C                   Z-ADD     PDPHN#        P1PHN#
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDNAME        P1NAME
     C                   MOVEL(P)  PDFSN         P1FSN
     C                   UPDATE    PP101
     C*
     C                   ENDIF
     C*
     C                   READ      PP101                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
