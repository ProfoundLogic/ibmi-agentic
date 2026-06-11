     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE PROGRAM TO COMPARE PAID AMOUNTS IN CLAIM MASTER FILE
     H*****************************************************************
     H* Description:  This pgm prints a list of claims which have a   *
     H*   discrepancy between the total paid in the Claim file and    *
     H*   the total paid in the Pmt Detail file.                      *
     H*                                                               *
     H*****************************************************************
     fsmupl     if   e           k disk    Rename(pup01:pup00)                  claim file
     FWMCML1    iF   E           K DISK
     FWDPAL     IF A E           K DISK
     FWDRPP     o    E             DISK    Rename(prp01:prp0p)
     FSMCOL     IF   E           K DISK
     FWtabl     IF   E           K DISK    Ignore(ppa01)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D AM              S              9  2 DIM(10)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
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

     d company#        s                         Like(CMco#)
     d group#          s                         Like(CMfnd)
     d inGroup#        s                         Like(CMfnd)
     d maxSeq#         s                         Like(PAseq#)

     d/copy sprdate

     C**************************************************************************
     C*
     C*  Key list for the Pmt Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE

      * Key list for the Claim Master file
      *
     c     cmky          klist
     c                   kfld                    company#
     c                   kfld                    group#
     C*
     C*  Key list for the Company file
     C*
     C     COKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C                   PARM                    QQGROUP#          3 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*  Start of main loop
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
     C                   READ      PCM01                                  60
     c                   Else
     c     cmky          Chain     pcm01                              60
     c                   EndIf
     C*
     C  N60COKY          CHAIN     PCO01                              61
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   CLEAR                   AM
     C*
     C*  GET ALL THE CLAIM DETAIL RECORDS
     C*
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATYPE        IFNE      'P'
     C     PATYPE        ANDNE     'M'
     C     PATYPE        ANDNE     'Z'
     C     PATYPE        ANDNE     'A'
     C                   MULT      -1            PAAMT
     C                   ENDIF
     C*
     C                   Z-ADD     1             X                 3 0
     C     PARECD        LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    RT(X)                                  60
     C                   MOVE      PARECD        RT(X)
     C                   ENDIF
     C                   ADD       PAAMT         AM(X)
     C                   Z-ADD     PAseq#        maxSeq#
     C*
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *OFF          *IN21
     C                   DO        10            X
     C*
     C     AM(X)         COMP      AR(X)                              6161
     C*
     C     *IN61         IFEQ      *ON
     C     AR(X)         SUB       AM(X)         XXDIFF            9 2
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      *ON           *IN21
     C     QQUPD         IFEQ      'Y'
     C                   EXSR      WRITEPMT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C   21              ADD       1             XTCASE            5 0
     C*
     c                   If        group# = 0
     C                   READ      PCM01                                  60
     c                   Else
     c     cmky          ReadE     pcm01                                  60
     c                   EndIf
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C                   SETON                                        LR
     C**************************************************************************
     C* This subroutine updates the Claim Master file to reflect the correct
     C*   total paid amount.
     C**************************************************************************
     C     WritePmt      BEGSR
     C*
     C     AR(X)         SUB       AM(X)         XXDIFF            9 2
     C*
       Clear ppa01;
       PAco#  = CMco#;
       PAfnd  = CMfnd;
       PAfyr  = CMfyr;
       PAcase = CMcase;
       PAchk# = 0;
       PAseq# = maxSeq# + 1;
       PAbasq = 0;
       PAtrDt = DateTo7(systemDate());
       PApoDt = DateTo7(systemDate());
       PAfein = 999999999;
       PAmod  = 0;
       PAamt  = xxDiff;

       PAfrDt = DateTo7(systemDate());
       PAtoDt = DateTo7(systemDate());
       PAwks  = 0;
       PAdays = 0;
       PAtrCd = 'XX';
       PAtype = 'P';

       PAreCd = RT(x);

       Exec Sql
          Select RTtype Into :PAreTp
          From WTRTP
          Where RTco# = :CMco# And RTkey = :PAreCd;

       Exec Sql
          Select PCkey,PCdesc Into :PAtrCd, :PAdesc
          From WTPCP
          Where PCreCd = :PAreCd And PCtrTp = 'P';


       PAstat = ' ';
       PAadSt = ' ';
       PAuser = Q1user;
       PApgm  = Q1pgm;
       PAdt   = DateTo7(systemDate());
       PAtime = TimeTo6(systemTime());
       PAeob# = 0;
       PApgtp = ' ';
       PAeobs = 0;
       PAacct = ' ';
       PAckTp = ' ';
       PAchk# = 0;
       PAoff# = CMoff#;
       PArcvd = 0;
       PAdue  = 0;
       PAbr2  = 'XX';

       If CMltFl = 'I';
          PAbr7 = 'LT';
       Else;
          PAbr7 = 'NL';
       EndIf;

       PAdue  = 0;
       PAltCd = ' ';
       PAdpCd = 0;
       PAdpCu = ' ';
       PArqst = Q1user;
       PAfnal = ' ';
       PArcDt = 0;
       PArAdj = ' ';
       PAdSq# = 0;
       PAlmps = ' ';
       PAiUsr = Q1user;
       PAiPgm = Q1pgm;
       PAiDt  = DateTo7(systemDate());
       PAiTim = TimeTo6(systemTime());
       Write ppa01;

       Clear prp0p;
       RPco#  = PAco#;
       RPfnd  = PAfnd;
       RPfyr  = PAfyr;
       RPcase = PAcase;
       RPchk# = PAchk#;
       RPtrDt = PAtrDt;
       RPrmds = 'Payment Offset';
       RPrmk1 = ' ';
       RPrmk2 = ' ';
       RPrmk3 = ' ';
       RPpNam = 'Offset Payee';
       RPadr1 = 'Offset Address';
       RPadr2 = ' ';
       RPcty  = 'Offset City';
       RPst   = 'XX';
       RPzip  = 999999999;
       RPctry = 'USA';
       RPadr3 = ' ';
       RPstat = ' ';
       RPuser = Q1user;
       RPpgm  = Q1pgm;
       RPdt   = DateTo7(systemDate());
       RPtime = Timeto6(systemTime());
       RPbul1 = 0;
       RPseq# = PAseq#;
       Write prp0p;

     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'CMPPMTBAL'
     O                                           76 'COMPARE WMCMP & WDPAP'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           33 'PMT DET AMT'
     O                                           50 'CLM MST AMT'
     O                                           59 'RSV CD'
     O                                           73 'DIFFERENCE'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       AM(X)         J     34
     O                       AR(X)         J     51
     O                       RT(X)               56
     O                       XXDIFF        J     74
     O          E            TOT         2
     O                                            7 'TOTALS:'
     O                       XTCASE        3     17
