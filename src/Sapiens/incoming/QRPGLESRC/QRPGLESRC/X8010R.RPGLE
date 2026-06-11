     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE X8010R - CONVERT MODIFIED PREMIUM FIELDS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM WILL MULTIPLY THE PREMIUMS IN ELEXP1,2,3 AND   *
     H*   ELAXP1,2,3 BY THE ASSOCIATED MODIFIER.  THIS WILL NOT BE    *
     H*   DONE FOR EMPLOYERS WITH ANNIVERSARY RATED MODIFIERS, AS     *
     H*   THESE WERE CALCULATED THAT WAY IN THE BILLS.  ONLY RUN THIS *
     H*   PROGRAM ONCE.  THIS CONVERSION MUST COMPLETE PRIOR TO ANY   *
     H*   BILLING RUNS OR ANY EMPLOYER MAINTENANCE.                   *
     H* CALLED FROM:                                                  *
     H*   NONE - RUN FROM COMMAND ENTRY                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 03/01/95               PROJECT: WC311                *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - UTILITY INDICATOR                                   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDELP     UPE  E             DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C                   TIME                    DSDTTM
     C*
     C*  BYPASS IF NOT THIS FUND YEAR, OR IF ALREADY PROCESSED
     C*
     C     ELFYR         IFEQ      95
     C     ELPGM         ANDNE     Q1PGM
     C*
     C*  BYPASS ANNIVERSARY RATED EXPERIENCE MODS
     C*
     C     ELANDT        ADD       10000         XXANDT            6 0
     C*
     C     ELANDT        IFEQ      *ZERO
     C     ELEMD1        ORNE      ELANDT
     C     ELEMD2        ORNE      XXANDT
     C     ELEXM1        OREQ      ELEXM2
     C*
     C     ELEXP1        MULT(H)   ELEXM1        XXRND             9 0
     C                   Z-ADD     XXRND         ELEXP1
     C     ELEXP2        MULT(H)   ELEXM2        XXRND
     C                   Z-ADD     XXRND         ELEXP2
     C     ELEXP3        MULT(H)   ELEXM3        XXRND
     C                   Z-ADD     XXRND         ELEXP3
     C     ELAXP1        MULT(H)   ELEXM1        XXRND
     C                   Z-ADD     XXRND         ELAXP1
     C     ELAXP2        MULT(H)   ELEXM2        XXRND
     C                   Z-ADD     XXRND         ELAXP2
     C     ELAXP3        MULT(H)   ELEXM3        XXRND
     C                   Z-ADD     XXRND         ELAXP3
     C*
     C                   ELSE
     C*
     C*  IF ANNIVERSARY RATED, ROUND TO WHOLE DOLLARS
     C*
     C                   Z-ADD(H)  ELEXP1        XXRND             9 0
     C                   Z-ADD     XXRND         ELEXP1
     C                   Z-ADD(H)  ELEXP2        XXRND
     C                   Z-ADD     XXRND         ELEXP2
     C                   Z-ADD(H)  ELEXP3        XXRND
     C                   Z-ADD     XXRND         ELEXP3
     C                   Z-ADD(H)  ELAXP1        XXRND
     C                   Z-ADD     XXRND         ELAXP1
     C                   Z-ADD(H)  ELAXP2        XXRND
     C                   Z-ADD     XXRND         ELAXP2
     C                   Z-ADD(H)  ELAXP3        XXRND
     C                   Z-ADD     XXRND         ELAXP3
     C*
     C                   ENDIF                                                  NOT ANNIV RATED
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  FYR = 94
