     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE WSCLMACTP - Program to write claim action panel data for Scriptura
     H*====================================================================
     H*  Description:
     H*
     H*  This program is called by STLETTER via the RunDataPgm subroutine.
     H*  It is used to write the claim action panel record once the
     H*  interactive document has been created on the Scriptura server.
     H*
     H*  Created:  04/04/16  C:65  Project: WC2374/SY371
     H*====================================================================
     H*  Revisions:
     H*
     H*
     H*====================================================================
     FXMLPREL1  IF   E           K DISK
     fwacap     o  a e             disk    rename(pca01:pca0p)
     f                                     prefix(c1:2)
     fsdcmlu    if   e           k disk    rename(pcm01:pcm0u)
     d*
     d psds          esds                  extname(szq1p)
     d*
     D                 DS
     D  QCKEYc                 1     40
     D  QCCO#c                 1      3  0
     D  QCFNDc                 4      6  0
     D  QCFYRc                 7      9  0
     D  QCCASEc               10     16  0
     d*
     d/copy sPrDate
     C*
     C* Entry list to program.
     C*
     C     *entry        plist
     C                   parm                    QQXML            60
      /free

       Chain(n) (QQXML) xp100;

       QCCO#c =  XPCO#;
       QCFndc  = XPFnd;
       QCFyrc  = XPFyr;
       QCCasec = XPCase;

       Clear pca0p;

       C1co#  = XPco#;
       C1fnd  = XPfnd;
       C1fyr  = XPfyr;
       C1case = XPcase;
       C1acdt = DateTo7(SystemDate());
       C1cusr = q1user;
       C1apln = %trim(XPText1);
       C1sts  = 'Status Text';
       C1rsvr = 'Reserve Rationale Text';
       C1cc   = 'Email CC: Text';

       Setgt (QCKeyC:XPRmcd) pcm0u;
       Readpe (QCKeyC:XPRmcd) pcm0u;

       If not %eof;
          C1key  = CMKey;
          C1seq# = CMSeq#;
       Endif;

       C1pgm  = q1pgm;
       C1time = TimeTo6(SystemTime());

       Write pca0p;

       *inlr = *on;
       return;

      /end-free
