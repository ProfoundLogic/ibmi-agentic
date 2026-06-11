     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMSHPST - INITIALIZE STATE INFORMATION ON WMSHP.       ND   R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     INITIALIZE STATE INFORMATION ON WMSHP RECORDS.            *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:65                                            *
     H*   CREATED:    08/10/01    PROJECT: 81WSE                      *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/27/04 C:65  PROJECT: SUPPORT                               *
     H*                UPDATE STATUS 5 RECORDS BECAUSE ENDORSEMENT    *
     H*                RECORDS WILL NOT UPDATE STATE IF POLICY RECORD *
     H*                DOES NOT HAVE THEM.                            *
     H*                                                               *
     H*****************************************************************
     FWMSHP     UF   E             DISK
     FWDELL1    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMRSL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  SST                    1     30
     D                                     DIM(15)
     D  SHST01                 1      2
     D  SHST02                 3      4
     D  SHST03                 5      6
     D  SHST04                 7      8
     D  SHST05                 9     10
     D  SHST06                11     12
     D  SHST07                13     14
     D  SHST08                15     16
     D  SHST09                17     18
     D  SHST10                19     20
     D  SHST11                21     22
     D  SHST12                23     24
     D  SHST13                25     26
     D  SHST14                27     28
     D  SHST15                29     30
     D                 DS
     D  SSS                    1     15
     D                                     DIM(15)
     D  SHSS01                 1      1
     D  SHSS02                 2      2
     D  SHSS03                 3      3
     D  SHSS04                 4      4
     D  SHSS05                 5      5
     D  SHSS06                 6      6
     D  SHSS07                 7      7
     D  SHSS08                 8      8
     D  SHSS09                 9      9
     D  SHSS10                10     10
     D  SHSS11                11     11
     D  SHSS12                12     12
     D  SHSS13                13     13
     D  SHSS14                14     14
     D  SHSS15                15     15

     C*
     C     GTKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C     ELKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
     C*
     C     RSKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    XXRTCD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C*
     C                   READ      PSH01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     SHSTAT        IFEQ      '3'
     C     SHSTAT        OREQ      '4'
     C*** BEGIN ADD ***  10/27/04  ************************************
     C     SHSTAT        OREQ      '5'
     C*** END ADD *****  10/27/04  ************************************
     C     SHSTAT        OREQ      'E'
     C*
     C                   EXSR      UPDPSH
     C*
     C                   ENDIF                                                  SHSTAT IFEQ
     C*
     C                   READ      PSH01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*****************************************************************
     C     UPDPSH        BEGSR
      *
     C                   MOVE      *BLANK        XXUPSH            1
     C                   Z-ADD     1             X                 2 0
     C     *BLANKS       LOOKUP    SST(X)                                 60
      *
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL(P)  'NCCI'        XXTBID
     C                   MOVEL(P)  RSJRST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'XX'          XZST              2
     C*
     C                   Z-ADD     1             I                 2 0
     C     RSJRST        LOOKUP    SST(I)                                 60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       SST(I)
     C                   MOVE      *BLANKS       SSS(I)
     C                   MOVE      'Y'           XXUPSH            1
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C                   MOVE      RSJRST        XZST              2
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     1             I                 2 0
     C     XZST          LOOKUP    SST(I)                                 60
     C*
     C     *IN60         IFEQ      *OFF
     C     X             ANDLE     15
     C                   MOVE      XZST          SST(X)
     C                   MOVE      SHSTAT        SSS(X)
     C                   ADD       1             X
     C                   MOVE      'Y'           XXUPSH            1
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     1             C                 2 0
     C     C             DOWLT     15
     C*
     C                   Z-ADD     1             A                 2 0
     C                   Z-ADD     2             B                 2 0
     C     A             DOWLT     15
     C     SST(A)        IFEQ      *BLANKS
     C                   MOVE      SST(B)        SST(A)
     C                   MOVE      SSS(B)        SSS(A)
     C                   MOVE      *BLANKS       SST(B)
     C                   MOVE      *BLANKS       SSS(B)
     C                   ENDIF
     C                   ADD       1             A
     C                   ADD       1             B
     C                   ENDDO
     C*
     C                   ADD       1             C
     C                   ENDDO
     C*
     C     XXUPSH        IFEQ      'Y'
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   UPDATE    PSH01
     C                   ENDIF
      *
     C                   ENDSR                                                  UPDPSH
     C*****************************************************************
