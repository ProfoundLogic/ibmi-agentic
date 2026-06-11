     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE FIXRSVBAL - Fix Claim Reserve History
     H*****************************************************************
     H* Description:  This pgm reads all claims and makes adjustments *
     H*   to the Reserve History file if the total incurred does not  *
     H*   match the total of reserve history records.                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*      Process claim only if the History total incurred differs.
     H*
     H*      If next available array accumulator is used, also move
     H*    the reserve type code to corresponding array element.
     H*
     H*****************************************************************
     H*
     fsmupl     if   e           k disk    Rename(pup01:pup00)                  claim file
     FWMRHL     IF A E           K DISK
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D TC              S              9  2 DIM(10)
     D TR              S              9  2 DIM(10)
     D DF              S              9  2 DIM(10)
     D DS              S             40    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  RC                     1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2

      /copy wPrClmRsv

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     d company#        s                         Like(CMco#)
     d group#          s                         Like(CMfnd)
     d inGroup#        s                         Like(CMfnd)

     C**************************************************************************
      * Key list for the Claim Master file
      *
     c     cmky          klist
     c                   kfld                    company#
     c                   kfld                    group#
     C*
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Z                 3 0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD
     C**************************************************************************
     C*  Start reading the claim records.
     C*
       // Are we runnng for one group or all?
       group# = 0;
       If q1Prms > 0;
          group# = inGroup#;
       EndIf;

       Chain Q1user pup00;
       If %Found(smupl);
          company# = UPco#;
       Else;
          company# = 777;
       Endif;
     c                   if        group# = 0
     C                   READ      PCM01                                  50
     c                   Else
     c     cmky          Chain     pcm01                              60
     c                   EndIf
     C     *IN50         DOWEQ     '0'
     C*
     C*  Clear the workfields.
     C*
     C                   CLEAR                   TC
     C                   CLEAR                   TR
     C                   Z-ADD     XXDATE        SVDATE            7 0
     C*
     C*  Load the claim total incurred.
     C*
     C                   DO        10            X
     C     RR(X)         ADD       AR(X)         TC(X)
     C                   ENDDO
     C*
     C*  Read the reserve history records.
     C*
     C     RHKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     RHEFDT        IFLE      SVDATE
     C                   Z-ADD     RHEFDT        SVDATE
     C                   ENDIF
     C*
     C                   DO        10            X
     C*
     C                   Z-ADD     1             Z
     C     RC(X)         LOOKUP    RT(Z)                                  60
     C   60              ADD       SU(X)         TR(Z)
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     *BLANKS       LOOKUP    RT(Z)                                  60
     C                   ADD       SU(X)         TR(Z)
     C*
     C                   MOVEL     RC(X)         RT(Z)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     RHKY          READE     PRH01                                  60
     C                   ENDDO
     C*
     C*  Determine if the reserves are out of balance.  If so, create
     C*    a resrve history entry.
     C*
     C*  Check total reserves to total incurred.
     C                   XFOOT     TC            CLMTI            11 2
     C                   XFOOT     TR            HISTI            11 2
     C*
     C                   MOVE      'N'           XXUPD             1
     C                   DO        10            X
     C*
     C     TC(X)         IFNE      TR(X)
     C     TC(X)         SUB       TR(X)         DF(X)
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      'Y'           XXUPD
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     XXUPD         IFEQ      'Y'
     C     QQUPD         ANDEQ     'Y'
     C                   EXSR      SRUPD
     C                   ENDIF
     C*
     C*  Get the next claim record.
     C*
     c                   If        group# = 0
     C                   READ      PCM01                                  50
     c                   Else
     c     cmky          ReadE     pcm01                                  60
     c                   EndIf
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     C*  SRUPD - This subroutine creates a Reserve History record for
     C*    claims which are out of balance.
     C**************************************************************************
     C     SRUPD         BEGSR
     C*
     C                   CLEAR                   PRH01
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C*
     C                   DO        10            X
     C*
     C                   MOVE      'N'           XXUPD
     C*
     C     TC(X)         SUB       TR(X)         DF(X)
     C     DF(X)         IFNE      0
     C     RT(X)         ANDNE     *BLANKS
     C                   MOVE(P)   RT(X)         RC(1)
     C                   Z-ADD     DF(X)         SU(1)
     C                   MOVE      'Y'           XXUPD
     C                   ENDIF
     C*
     C     DF(X)         IFGE      0
     C                   Z-ADD     SVDATE        RHEFDT
     C                   ELSE
     C                   Z-ADD     XXDATE        RHEFDT
     C                   ENDIF
     C*
     C                   MOVEL(P)  DS(1)         RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   Z-ADD     XXDATE        RHDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RHTIME
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
     C     XXUPD         IFEQ      'Y'
     C                   WRITE     PRH01
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           65 'Reserve History vs.'
     O                                           86 'Claim Total Incurred'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                            9 'FIXRSVBAL'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           33 'RSV CODE'
     O                                           49 'RSV HISTORY'
     O                                           65 'CLM TOT INC'
     O                                           81 'DIFFERENCE'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       RT(X)               30
     O                       TR(X)               50 '       .  -'
     O                       TC(X)               66 '       .  -'
     O                       DF(X)               82 '       .  -'
** RSV CHG DESC
ADJUSTED RESERVES
