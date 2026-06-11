     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H*================================================================
     H/TITLE LODRRDVCHG - Stat Rpt/Rating - Load allow changes
     H*
     H* Created: 07/12/18 Project: C148WAV  C: 291
     H*
     H*================================================================
     D sysDt7          s              7  0
     D sysTime6        s              6  0
     D psds          esds                  extname(szq1p)

     D/copy sprdate

      /Free

        sysDt7 = DateTo7(SystemDate);
        sysTime6 = TimeTo6(SystemTime);

        Exec SQL
         Update wdrrp
          set RRdv1Chg = 'Y', RRuser = :q1user,
          RRpgm = :q1pgm, RRDt = :sysDt7, RRtime = :sysTime6
          where RRdv1Chg = ' ';

        Exec SQL
         Update wdrrp
          set RRdv2Chg = 'Y', RRuser = :q1user,
          RRpgm = :q1pgm, RRDt = :sysDt7, RRtime = :sysTime6
          where RRdv2Chg = ' ';

        *InLr = *On;
      /End-free

