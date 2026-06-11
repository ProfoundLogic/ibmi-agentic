     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')

     fwdell     uf   e           k Disk    Rename(pel01:pel00)
     fwdrpl3    if   e           k Disk    Rename(prp01:prp03)
     fwdrpp     o    e             Disk    Rename(prp01:prp0p)
     fwdpal     uf   e           k Disk    Rename(ppa01:ppa00) Prefix(P2:2)
     fwdpap     o    e             Disk    Rename(ppa01:ppa0p)
     fwmcml1    uf   e           k Disk
     fwmrhp     o    e             Disk    Rename(prh01:prh0p)
     fwtabl     if   e           k Disk
     f                                     Ignore(PAT01)
     f                                     Ignore(PSP01)
     f                                     Ignore(PNC01)
     f                                     Ignore(PNA01)
     f                                     Ignore(PPA01)
     f                                     Ignore(PCA01)
     f                                     Ignore(PRC01)
     f                                     Ignore(PB701)
     f                                     Ignore(PBS01)
     f                                     Ignore(PTT01)
     f                                     Ignore(PCC01)
     f                                     Ignore(PBC01)
     f                                     Ignore(PCS01)
     f                                     Ignore(PCN01)
     f                                     Ignore(PRS01)
     f                                     Ignore(PLR01)
     f                                     Ignore(PVS01)
     f                                     Ignore(PTR01)
     f                                     Ignore(PHY01)
     f                                     Ignore(PSY01)
     f                                     Ignore(PBA01)
     f                                     Ignore(PID01)
     f                                     Ignore(PIS01)

     d desc1           c                   Const('Adjustment due to ba-
     d                                     cing out payments.')

     d xxSeq#          s                   Like(P2seq#)
     d x               s              2  0
     d y               s              3  0
     d sqlStmtStr      s               a   len(32000) varying
     d moreRows        s               n   Inz(*On)

     d psds          esds                  ExtName(szq1p)

     d                 ds
     d  ar                     1     90  2
     d                                     Dim(10)
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2

     d                 ds
     d  rr                     1     90  2
     d                                     Dim(10)
     d  CMrr01                 1      9  2
     d  CMrr02                10     18  2
     d  CMrr03                19     27  2
     d  CMrr04                28     36  2
     d  CMrr05                37     45  2
     d  CMrr06                46     54  2
     d  CMrr07                55     63  2
     d  CMrr08                64     72  2
     d  CMrr09                73     81  2
     d  CMrr10                82     90  2

     d                 ds
     d  rt                     1     20
     d                                     Dim(10)
     d  CMrt01                 1      2
     d  CMrt02                 3      4
     d  CMrt03                 5      6
     d  CMrt04                 7      8
     d  CMrt05                 9     10
     d  CMrt06                11     12
     d  CMrt07                13     14
     d  CMrt08                15     16
     d  CMrt09                17     18
     d  CMrt10                19     20

     d                 ds                  Inz
     d  rrt                    1     20
     d                                     Dim(10)
     d  RHrt01                 1      2
     d  RHrt02                 3      4
     d  RHrt03                 5      6
     d  RHrt04                 7      8
     d  RHrt05                 9     10
     d  RHrt06                11     12
     d  RHrt07                13     14
     d  RHrt08                15     16
     d  RHrt09                17     18
     d  RHrt10                19     20

     d                 ds                  Inz
     d  rsu                    1     90  2
     d                                     Dim(10)
     d  RHsu01                 1      9  2
     d  RHsu02                10     18  2
     d  RHsu03                19     27  2
     d  RHsu04                28     36  2
     d  RHsu05                37     45  2
     d  RHsu06                46     54  2
     d  RHsu07                55     63  2
     d  RHsu08                64     72  2
     d  RHsu09                73     81  2
     d  RHsu10                82     90  2

     ddsWDPAP        e ds                  extname(WDPAP) qualified inz

     d/copy sprdate
     d/Copy sPrSql
     d/copy wPrClmRsv

       sqlStmtStr = 'Select * From WDPAP Where PAfnd in (630, 631, 635) ' +
          'And PAfein = 251686685 And PAmod = 9 And PAbr2=''NA'' And ' +
          'PAbr7=''C1'' And PAtype = ''P'' And PAadst = '' ''';

       Exec SQL close pmtCursor;
       Exec SQL prepare pmtStmt from :sqlStmtStr;
       Exec SQL declare pmtCursor cursor for pmtStmt;
       Exec SQL open pmtCursor;

       DoU moreRows = *Off;
          Exec SQL fetch next from pmtCursor into :dsWDPAP;

          Select;
          When not IsValidFetch(sqlstt);
             moreRows = *off;
             Iter;
          Endsl;
          ExSr CreateAdjustment;
       EndDo;
       Exec SQL close pmtCursor;

       *inLR = *On;

       //===================================================================
       // CreateAdjustment - Create the adjustment record
       //===================================================================
       BegSr CreateAdjustment;

          // Mark Payment as having been adjusted
          Chain (dsWDPAP.PAco#:dsWDPAP.PAfnd:dsWDPAP.PAfyr:
                 dsWDPAP.PAcase:dsWDPAP.PAseq#) ppa00;
          If %Found(wdpal);
             P2adSt = 'A';
             P2user = Q1user;
             P2pgm  = Q1pgm;
             P2dt   = DateTo7(systemDate());
             P2time = TimeTo6(systemTime());
             Update ppa00;
          EndIf;

          // Get next sequence #
          SetGt (dsWDPAP.PAco#:dsWDPAP.PAfnd:
                 dsWDPAP.PAfyr:dsWDPAP.PAcase) ppa00;
          ReadPE(n) (dsWDPAP.PAco#:dsWDPAP.PAfnd:
                     dsWDPAP.PAfyr:dsWDPAP.PAcase) ppa00;
          If %Eof(wdpal);
             xxSeq# = 0;
          Else;
             xxSeq# = P2seq#;
          EndIf;

          // Write adjustment payment
          Clear ppa0p;

          PAseq# = xxSeq# + 1;
          PAamt  = dsWDPAP.PAamt * -1;
          PAco#  = dsWDPAP.PAco#;
          PAfnd  = dsWDPAP.PAfnd;
          PAfyr  = dsWDPAP.PAfyr;
          PAcase = dsWDPAP.PAcase;
          PAchk# = 0;
          PAbaSq = 0;
          PAfein = dsWDPAP.PAfein;
          PAmod  = dsWDPAP.PAmod;
          PAtrDt = DateTo7(systemDate());
          PApoDt = DateTo7(systemDate());
          PAfrDt = dsWDPAP.PAfrDt;
          PAtoDt = dsWDPAP.PAtoDt;
          PAbr2  = dsWDPAP.PAbr2;
          PAbr7  = dsWDPAP.PAbr7;

          PAtrCd = dsWDPAP.PAtrCd;
          SetLL *loval ppc01;
          Read ppc01;
          DoW Not %Eof(wtabl);
             If PCreCd = dsWDPAP.PAreCd And PCtrTp = 'A';
                PAtrCd = PCkey;
                Leave;
             EndIf;
             Read ppc01;
          EndDo;

          PAdesc = desc1;
          PAtype = 'A';
          PAreCd = dsWDPAP.PAreCd;
          PAreTp = dsWDPAP.PAreTp;
          PAeob# = dsWDPAP.PAeob#;
          PAeobs = dsWDPAP.PAeobs;
          PAoff# = CMoff#;
          PAstat = dsWDPAP.PAstat;
          PArqst = Q1user;
          PAuser = Q1user;
          PApgm  = Q1pgm;
          PAdt   = DateTo7(systemDate());
          PAtime = TimeTo6(systemTime());
          PAiUsr = Q1user;
          PAiPgm = Q1pgm;
          PAiDt  = DateTo7(systemDate());
          PAitim = TimeTo6(systemTime());

          Write ppa0p;

          // Create Request Payment Detail Record
          Clear prp0p;
          Chain (PAco#:PAfnd:PAfyr:PAcase:dsWDPAP.PAseq#) prp03;
          RPtrDt = PAtrDt;
          RPseq# = PAseq#;
          RPstat = ' ';
          RPuser = Q1user;
          Write prp0p;

          // Update claim master
          Chain (PAco#:PAfnd:PAfyr:PAcase) pcm01;
          If %Found(wmcml1);

             // Find the reserve bucket being updated
             x = %LookUp(PAreCd:rt);

             // Adjust the reserves
             If CMstat = 'C' Or PAamt > rr(x);
                ExSr WriteReserveHistory;
             Else;
                rr(x) -= PAamt;
             EndIf;

             ar(x) += PAamt;
             CMuser = Q1user;
             CMpgm  = Q1pgm;
             CMdt   = DateTo7(systemDate());
             CMtime = TimeTo6(systemTime());

             Update pcm01;

             // Update employer detail file with the outstanding
             // reserve and total claims paid.
             Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
             If %Found(wdell);

                If CMstat = 'O';
                   ELouts -= PAamt;
                Endif;

                ELtclp += PAamt;
                ELuser = Q1user;
                ELpgm  = Q1pgm;
                ELdt   = DateTo7(systemDate());
                ELtime = TimeTo6(systemTime());

                Update pel00;
             EndIf;
          EndIf;

       EndSr;

       //===================================================================
       // CreateAdjustment - Create the adjustment record
       //===================================================================
       BegSr WriteReserveHistory;

          Clear prh0p;

          RHco#  = CMco#;
          RHfnd  = CMfnd;
          RHfyr  = CMfyr;
          RHcase = CMcase;

          // Find the reserve bucket being updated
          y = %LookUp(PAreCd:rt);
          rrt(y) = PArecd;
          rsu(y) = PAamt;
          RHexpl = desc1;
          RHuser = Q1user;
          RHpgm  = Q1pgm;
          RHdt   = DateTo7(systemDate());
          RHefDt = DateTo7(systemDate());
          RHtime = TimeTo6(systemTime());
          RHiDt  = DateTo7(systemDate());
          RHiUsr = Q1user;
          RHiPgm = Q1pgm;
          RHiTim = TimeTo6(systemTime());
          RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
          Write PRH0P;

       EndSr;
