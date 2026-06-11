     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     fWDDDP     o    e             disk    rename(pdd01:pdd0p)
     fWDPAP     o    e             disk    rename(ppa01:ppa0p)
     fWDPAL     if   e           k disk    rename(ppa01:ppa0l) prefix(l)
     fWMEAP     o    e             disk    rename(pea01:pea0p)
     fWMCML1    uf   e           k disk    rename(pcm01:pcm0l1)
     d dta             s             70    dim(10) ctdata perrcd(1)
     d dtaDS           ds            70
     d  dsCo#                         3  0
     d  dsFnd                         3  0
     d  dsEmp#                        9  0
     d  dsDiv                         5  0
     d  dsFyr                         3  0
     d  dsCase                        5  0
     d  dsPV1                         2
     d  dsPV2                         2
     d  dsTrCd                        2
     d  dsRsvCd                       2
     d  dsRsvTyp                      1
     d  dsArCd                        2
     d  dsSort                        1
     d  dsAmt                         9  2
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
     d x               s              2  0
     d sysDate         s              7  0
     d sysTime         s              6  0
     d WrtDedAdj       pr
     d WrtPayAdj       pr
     d AdjWMCMP        pr
     d WrtARAdj        pr
     d getNxtSeq#      pr             5  0
     d/copy sprdate
     d psds          esds                  extname(szq1p)
      /free
       sysDate = DateTo7(SystemDate);
       sysTime = Timeto6(SystemTime());
       x =  1;
       Dow x < 11;
          dtaDS = Dta(x);
          Callp WrtDedAdj();
          Callp WrtPayAdj();
          Callp AdjWMCMP();
          Callp WrtARAdj();
          x += 1;
       Enddo;
       *inlr = *on;
       Return;
      /end-free
      *====================================================
     p WrtDedAdj       b
      *====================================================
      /free
       Clear pdd0p;
       DDco#  = dsCo#;
       DDfnd  = dsFnd;
       DDbfyr = dsFyr;
       DDemp# = dsEmp#;
       DDdiv  = dsdiv;
       DDcase = dsCase;
       DDpodt = sysDate;
       DDamtb = dsAmt * -1;
       DDrecd = dsRsvCd;
       DDsort = %int(dsSort);
       DDstat = 'A';
       DDuser = Q1user;
       DDpgm  = Q1pgm;
       DDdt   = sysDate;
       DDtime = sysTime;
       DDiusr = Q1user;
       DDidt  = sysDate;
       Write pdd0p;
      /end-free
     p WrtDedAdj       e
      *====================================================
     p WrtPayAdj       b
      *====================================================
      /free
       Clear ppa0p;
       PAco#  = dsCo#;
       PAfnd  = dsFnd;
       PAfyr  = dsFyr;
       PAcase = dsCase;
       PAseq# = getNxtSeq#();
       PAacct = lPAacct;
       PAcase = dsCase;
       PAtrdt = sysDate;
       PApodt = sysDate;
       PAamt  = dsAmt * -1;
       PAbr2  = dsPV1;
       PAbr7  = dsPV2;
       PAtrcd = dsTrCd;
       PAtype = 'R';
       PArecd = dsRsvCd;
       PAretp = dsRsvTyp;
       PAdesc = 'Reverse Ded Recovery';
       PAstat = 'A';
       PAuser = Q1user;
       PApgm  = Q1pgm;
       PAtime = sysTime;
       PAdt   = sysDate;
       PAiusr = Q1user;
       PAipgm = Q1pgm;
       PAitim = sysTime;
       PAidt  = sysDate;
       Write ppa0p;
      /end-free
     p WrtPayAdj       e
      *====================================================
     p GetNxtSeq#      b
      *====================================================
     d GetNxtSeq#      pi             5  0
      /free
       Setgt  (dsCo#:dsFnd:dsFyr:dsCase) ppa0l;
       Readpe (dsCo#:dsFnd:dsFyr:dsCase) ppa0l;
       Return lPAseq# + 1;
      /end-free
     p GetNxtSeq#      e
      *====================================================
     p AdjWMCMP        b
      *====================================================
     d y               s              2  0
      /free
       Chain (dsCo#:dsFnd:dsFyr:dsCase) pcm0l1;
       If not %found or CMstat = 'C';
          Return;
       Endif;
       y = %lookup(dsRsvCd:RT);
       ar(y) = ar(y) + dsAmt;
       rr(y) = rr(y) - dsAmt;
       If rr(y) < 0;
          rr(y) = 0;
       Endif;
       Update pcm0l1;
      /end-free
     p AdjWMCMP        e
      *====================================================
     p WrtARAdj        b
      *====================================================
      /free
       clear pea0p;
       EAco#  = dsCo#;
       EAfnd  = dsFnd;
       Eafyr  = dsFyr;
       EAemp# = dsEmp#;
       EAdiv  = dsDiv;
       EAarcd = dsArCd;
       EAarty = 'F';
       EActyp = 'R';
       EAdesc = 'Reverse Ded Recovery';
       EAamt  = dsAmt * -1;
       EAtrdt = sysDate;
       EApodt = sysDate;
       EAuser = Q1user;
       EApgm  = Q1pgm;
       EAtime = sysTime;
       EAdt   = sysDate;
       Write pea0p;
      /end-free
     p WrtARAdj        e
      *====================================================
** dta
7773500000000850000011301395MQNLMRMEMDF2000014349
7773500000000810000011301491MQNLMRMEMDF2000044179
7773500000000850000011301586MQNLMRMEMDF2000019384
7773500000000850000011301589MQNLMRMEMDF2000014349
7773500000000810000011301630MQNLMRMEMDF2000027021
7773500000000850000011302108MQNLMRMEMDF2000025751
7775220000002010000011234984MQNLMRMEMDF2000023100
7775220000002300000011335873MQNLMRMEMDF2000020515
7775220000001140000011335885MQNLMRMEMDF2000019643
7775220000002300000011335966MQNLMRMEMDF2000048935
