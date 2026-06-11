     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWTMYP - CHANGE MIDTERM END RECORDS FOR OWN/OFFC, ENT/LOC
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 05/07/99  C:09  PROJECT CM044                       *
     H*                                                               *
     H*  THIS PROGRAM WILL EXPAND THE CURRENT OPTIONS UNDER OPT 55 ON *
     H*  WORK WITH EMPLOYERS FROM "ENT/LOC CHGS", "OWN/OFFC CHGS" TO  *
     H*  "ENT/LOC ADD, DELETE, CHG" AND "OWN/OFFC ADD, DELETE, CHG"   *
     H*                                                               *
     H*****************************************************************
     H*
     FWMFNP     IF   E           K DISK
     FWTMYL     UF A E           K DISK

     D OPTS            S             30    DIM(6) CTDATA PERRCD(1)
     D PGMS            S             10    DIM(6) CTDATA PERRCD(1)
     D SORT            S              3  0 DIM(6) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     MYKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXSORT
     C*
     C     *LIKE         DEFINE    DSTIME        XXTIME
     C     *LIKE         DEFINE    MYSORT        XXSORT
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C                   READ      PFN01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     SORT(1)       XXSORT
     C     MYKY          CHAIN     PMY01                              65
     C     *IN65         IFEQ      '0'
     C*
     C                   DO        6             X                 2 0
     C*
     C                   Z-ADD     SORT(X)       XXSORT
     C     MYKY          CHAIN     PMY01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVE      *BLANKS       MYDESC
     C                   MOVEL     OPTS(X)       MYDESC
     C                   MOVE      *BLANKS       MYPGM1
     C                   MOVEL     PGMS(X)       MYPGM1
     C                   MOVEL     OPTS(X)       MYDSC1
     C                   MOVEL     Q1USER        MYUSER
     C                   MOVEL     Q1PGM         MYPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MYDT
     C                   ADD       1             XXTIME
     C                   Z-ADD     XXTIME        MYTIME
     C                   UPDATE    PMY01
     C                   ELSE
     C                   Z-ADD     FNCO#         MYCO#
     C                   Z-ADD     FNFND         MYFND
     C                   MOVE      *BLANKS       MYDESC
     C                   MOVEL     OPTS(X)       MYDESC
     C                   Z-ADD     SORT(X)       MYSORT
     C                   MOVE      'N'           MYSRFL
     C                   MOVE      *BLANKS       MYPGM1
     C                   MOVEL     PGMS(X)       MYPGM1
     C                   MOVE      *BLANKS       MYPGM2
     C                   MOVE      *BLANKS       MYPGM3
     C                   MOVE      *BLANKS       MYPGM4
     C                   MOVE      *BLANKS       MYPGM5
     C                   MOVEL     OPTS(X)       MYDSC1
     C                   MOVE      *BLANKS       MYDSC2
     C                   MOVE      *BLANKS       MYDSC3
     C                   MOVEL     Q1USER        MYUSER
     C                   MOVEL     Q1PGM         MYPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MYDT
     C                   ADD       1             XXTIME
     C                   Z-ADD     XXTIME        MYTIME
     C                   WRITE     PMY01
     C                   ENDIF                                                  MYKY *IN70
     C*
     C                   ENDDO                                                  DO 6
     C*
     C                   ENDIF                                                  IF *IN65
     C*
     C                   READ      PFN01                                  60
     C                   ENDDO                                                  DOW *IN60
     C*
     C                   MOVE      *ON           *INLR
**  OPTS
ADDITION OF ENTITY/LOCATION(S)
ENTITY/LOCATION CHANGES
DELETION OF ENTITY/LOCATION(S)
ADDITION OF OWNER/OFFICER(S)
OWNER/OFFICER CHANGES
DELETION OF OWNER/OFFICER(S)
**  PGMS
W0051R15
W0051R55
W0051R65
W0051R16
W0051R56
W0051R66
**  SORT
150
151
152
160
161
162
