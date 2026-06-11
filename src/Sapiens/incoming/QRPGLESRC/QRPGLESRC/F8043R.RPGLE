     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE F8043R - Creates Provider Records
     H*****************************************************************
     H* Description:  This program loads FMPDP, FMP1P AND WMPDP       *
     H*               from FAU1P.                                     *
     H*                                                               *
     H* Called from: F8041R - Upload Bills - Transfer Pgm 1           *
     H* Calls to   : None                                             *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/20/2004 by C:62 for Project 51MFN               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  04/05/06  C:163  PROJECT: WC132 51861                        *
     H*                   ADD CONDITION FOR WRITING PROVIDER RECORDS  *
     H*                                                               *
     H*****************************************************************
     FFAU1L1    IF   E             DISK
     FFMPDP     O    E             DISK
     FFMP1P     O    E             DISK
     FWMPDL     IF A E           K DISK
     F                                     RENAME(PPD01:PPD02)
     FSTSTL1    IF   E           K DISK

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  XXZIP                  1      9  0
     D  XXTEST                 1      4  0
     D  XXZIPA                 5      9  0
     D*
     D* Constants
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*
     C*   KEY LIST FOR THE WCMC PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C*
     C*   KEY LIST FOR THE STATE MASTER FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
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
     C*
     C*  LIKE DEFINES
     C*
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*
     C                   TIME                    DSDTTM
     C*
     C* READ THE INCOMING PC FILE
     C*
     C                   READ      PU101                                  99
     C     *IN99         DOWEQ     *OFF
     C     U1FEIN        IFNE      *ZEROS
     C                   Z-ADD     U1FEIN        KKFEIN
     C     PDKY          CHAIN     PPD02                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 04/05/06 ****************************************
     C     U1PNAM        ANDNE     *BLANKS
     C     U1ADR1        ANDNE     *BLANKS
     C     U1CTY         ANDNE     *BLANKS
     C     U1ST          ANDNE     *BLANKS
     C     U1ZIP         ANDNE     *BLANKS
     C*** END ADD *** 04/05/06 ****************************************
     C                   EXSR      WRITER
     C                   ENDIF                                                  END *IN99
     C                   ENDIF                                                  END U1FEIN
     C                   READ      PU101                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C***************************************************************
     C* SUBROUTINE TO WRITE WCMC PROVIDER MASTER RECORD
     C***************************************************************
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   PPD01
     C                   Z-ADD     U1FEIN        PDFEIN
     C                   Z-ADD     U1MOD         PDMOD
     C                   MOVEL(P)  U1PNAM        PDNAME
     C                   MOVEL(P)  U1PFSN        PDFSN
     C                   MOVEL(P)  U1ADR1        PDADR1
     C                   MOVEL(P)  U1ADR2        PDADR2
     C                   MOVEL(P)  U1CTY         PDCTY
     C                   MOVEL(P)  U1ST          PDST
     C                   MOVE      U1ZIP         PDZIP
     C                   MOVEL(P)  'Y'           PD1099
     C*
     C     U1BTYP        IFEQ      'H'
     C                   MOVEL(P)  '4'           PDTYPE
     C                   ELSE
     C                   MOVEL(P)  '1'           PDTYPE
     C                   ENDIF
     C*
     C                   MOVEL(P)  'N'           PDPARN
     C                   MOVEL(P)  'N'           PDDBA1
     C                   MOVEL(P)  'N'           PDWCCE
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL(P)  'A'           PDSTAT
     C                   MOVEL(P)  Q1USER        PDUSER
     C                   MOVEL(P)  Q1PGM         PDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PDDT
     C*
     C                   WRITE     PPD01
     C                   WRITE     PPD02
     C*
     C                   EXSR      WRTP1P
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO WRITE WCMC PROVIDER AUXILIARY RECORDS
     C***************************************************************
     C     WRTP1P        BEGSR
     C*
     C                   CLEAR                   PP101
     C*
     C                   Z-ADD     PDFEIN        P1FEIN
     C                   Z-ADD     PDMOD         P1MOD
     C*
     C                   MOVEL(P)  U1ST          KKKEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A3         ANDEQ     'Y'
     C                   MOVEL(P)  U1ST          P1ST
     C                   ELSE
     C                   MOVEL(P)  'UC'          P1ST
     C                   ENDIF
     C*
     C                   MOVEL(P)  U1LIC#        P1LIC#
     C                   MOVEL(P)  PDNAME        P1NAME
     C                   MOVEL(P)  PDFSN         P1FSN
     C                   MOVEL(P)  PDADR1        P1ADR1
     C                   MOVEL(P)  PDADR2        P1ADR2
     C                   MOVEL(P)  PDCTY         P1CTY
     C                   MOVEL(P)  PDST          P1PST
     C                   Z-ADD     PDZIP         P1ZIP
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL(P)  'A'           P1STAT
     C                   MOVEL(P)  Q1USER        P1USER
     C                   MOVEL(P)  Q1USER        P1IUSR
     C                   MOVEL(P)  Q1PGM         P1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        P1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P1DT
     C                   MOVEL     SRDTO         P1IDT
     C*
     C                   WRITE     PP101
     C*
     C                   ENDSR
