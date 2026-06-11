     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H*=====================================================================
     H/TITLE FIXWTENPC - Update Attach/Print programs for Contractor Class
     H*
     H* Created: 07/18/18 Project: WC2627  C: 291
     H*=====================================================================
     H* REVISIONS:
     H*
     H* 05/25/22 C:053 Project: WC2871
     H*                Add WC040314B to SQL's for SRPEOCLT
     H*
     H* 02/22/21 C:291 Support: 94470
     H*                Add SQL for manually attached owner/officer endorsements
     H*
     H* 05/14/20 C:291 Project: WC2608
     H*                Add SROFFICER conversion
     H*
     H* 04/07/20 C:136 Support: 88299
     H*                Remove WC310319 from endorsements using SRCONTCLS.
     H*
     H* 02/27/20 C:136 Project: WC2549
     H*                Add SRPEOCLT conversion
     H*
     H* 02/12/20 C:291 Project: WC2549
     H*                Activate WC2549 in V20R1.
     H*
     H* 02/19/19 C:291 Project: WC2549
     H*                Update WTENP with Print/Additional Info program
     H*                SRALTEMP for endorsement forms WC000301*.
     H*=====================================================================

     D sysDt7          s              7  0
     D sysTime6        s              6  0
     D CltId           s              3

     D psds          esds                  extname(szq1p)

     D GetClientID     pr                  ExtPgm('RTVCLTID')
     D  outCltID                      3



     D/copy sprdate

      /Free

        sysDt7 = DateTo7(SystemDate);
        sysTime6 = TimeTo6(SystemTime);
        GetClientID(CltID);

        Exec SQL
         Update wtenp
          set ENpgm1 = 'SRCONTCLS', ENpgm2 = 'SRCONTCLS', ENuser = :q1user,
          ENpgm = :q1pgm, ENDt = :sysDt7, ENtime = :sysTime6
          where substring(ENform,1,8) in (
          'WC060401',
          'WC070402',
          'WC090401',
          'WC120402',
          'WC190401',
          'WC290401',
          'WC310401',
          'WC350404',
          'WC370402',
          'WC450604',
          'WC540402',
          'WC480401'
          );

          Exec SQL
           Update WTENP
           Set ENpgm1 = ' ', ENpgm2 = 'SRALTEMP', ENpgm3 = 'SRALTEMP',
            ENuser = :q1user, ENpgm = :q1pgm, ENDt = :sysDt7,
            ENtime = :sysTime6
           Where ENmnau = 'M' And Substring(ENform,1,8)
            In ('WC000301', 'WC160302', 'WC220306', 'WC320304');
          Exec SQL
           Update WTENP
           Set ENpgm1 = 'SRALTEMP', ENpgm2 = 'SRALTEMP', ENpgm3 = ' ',
            ENuser = :q1user, ENpgm = :q1pgm, ENDt = :sysDt7,
            ENtime = :sysTime6
           Where ENmnau = 'A' And Substring(ENform,1,8)
            In ('WC000301', 'WC160302', 'WC220306', 'WC320304');

          Exec SQL
           Update WTENP
           Set ENpgm1 = 'SRPEOCLT',
            ENpgm2 = 'SRPEOCLT',
            ENpgm3 = ' ',
            ENuser = :q1user,
            ENpgm = :q1pgm,
            ENDt = :sysDt7,
            ENtime = :sysTime6
            Where ENmnau = 'A'
             And (Substring(ENform,1,8)
              In ('WC100302',
              'WC120309',
              'WC160302',
              'WC320306',
              'WC420311',
              'WC450301',
              'WC480314')
             Or Substring(ENform,1,9)
              In ('WC040314B'));

          Exec SQL
           Update WTENP
           Set ENpgm1 = ' ',
            ENpgm2 = 'SRPEOCLT',
            ENpgm3 = 'SRPEOCLT',
            ENuser = :q1user,
            ENpgm = :q1pgm,
            ENDt = :sysDt7,
            ENtime = :sysTime6
            Where ENmnau = 'M'
             And (Substring(ENform,1,8)
              In ('WC100302',
              'WC160302',
              'WC320306',
              'WC160302',
              'WC420311',
              'WC450301',
              'WC480314')
             Or Substring(ENform,1,9)
              In ('WC040314B'));

          Exec SQL
           Update WTENP
           Set ENpgm1 = 'SROFFICER',
            ENpgm2 = 'SROFFICER',
            ENpgm3 = ' ',
            ENuser = :q1user,
            ENpgm = :q1pgm,
            ENDt = :sysDt7,
            ENtime = :sysTime6,
            ENotobf =
            Case
            When  Substring(ENform,1,8) = 'WC310304' Then 'E'
            When  Substring(ENform,1,8) = 'WC420308' Then 'E'
            Else 'I' End,
            ENotobv =
            Case
            When Substring(ENform,1,8) = 'WC040302' Then '2'
            When Substring(ENform,1,9) = 'WC040303 ' and :CltId <> '136'
             Then '34'
            When Substring(ENform,1,9) = 'WC040303 ' and :CltId = '136'
             Then 'A'
            When Substring(ENform,1,9) = 'WC040303A' Then '3'
            When Substring(ENform,1,9) = 'WC040303B' Then '3'
            When Substring(ENform,1,9) = 'WC040303C' Then '3'
            When Substring(ENform,1,9) = 'WC040304 ' Then '1'
            When Substring(ENform,1,8) = 'WC040318'  Then 'A'
            When Substring(ENform,1,9) = 'WC040366 ' Then '3'
            When Substring(ENform,1,8) = 'WC070301'  Then '12'
            When Substring(ENform,1,8) = 'WC070302'  Then '3'
            When Substring(ENform,1,8) = 'WC290307'  Then '12AD'
            When Substring(ENform,1,8) = 'WC310304'  Then '12'
            When Substring(ENform,1,9) = 'WC310306A' Then '3'
            When Substring(ENform,1,9) = 'WC310313A' Then '12'
            When Substring(ENform,1,9) = 'WC310313B' Then '12AD'
            When Substring(ENform,1,9) = 'WC310313C' Then '12AD'
            When Substring(ENform,1,9) = 'WC310316 ' Then '12'
            When Substring(ENform,1,9) = 'WC310316A' Then '12AD'
            When Substring(ENform,1,9) = 'WC310316B' Then '12AD'
            When Substring(ENform,1,9) = 'WC310316C' Then '12AD'
            When Substring(ENform,1,9) = 'WC370310C' Then '3'
            When Substring(ENform,1,8) = 'WC420308'  Then '1'
            When Substring(ENform,1,8) = 'WC420313'  Then '123'
            When Substring(ENform,1,8) = 'WC420314'  Then '123'
            Else ' '
            End,
            ENoef =
            Case
            When Substring(ENform,1,8) = 'WC000308' Then 'E'
            When Substring(ENform,1,8) = 'WC000310' Then 'I'
            When Substring(ENform,1,8) = 'WC040302' Then 'E'
            When Substring(ENform,1,8) = 'WC040303' Then 'E'
            When Substring(ENform,1,8) = 'WC040304' Then 'I'
            When Substring(ENform,1,8) = 'WC040318' Then 'E'
            When Substring(ENform,1,8) = 'WC040366' Then 'E'
            When Substring(ENform,1,8) = 'WC070301' Then 'I'
            When Substring(ENform,1,8) = 'WC070302' Then 'E'
            When Substring(ENform,1,8) = 'WC290307' Then 'I'
            When Substring(ENform,1,8) = 'WC310304' Then 'E'
            When Substring(ENform,1,8) = 'WC310306' Then 'I'
            When Substring(ENform,1,8) = 'WC310313' Then 'I'
            When Substring(ENform,1,8) = 'WC310316' Then 'E'
            When Substring(ENform,1,8) = 'WC370310' Then 'E'
            When Substring(ENform,1,8) = 'WC420308' Then 'E'
            When Substring(ENform,1,8) = 'WC420310' Then 'I'
            When Substring(ENform,1,8) = 'WC420313' Then 'I'
            When Substring(ENform,1,8) = 'WC420314' Then 'E'
            Else ' '
            End
            Where ENmnau = 'A'
             And Substring(ENform,1,8)
              In (
                 'WC000308',
                 'WC000310',
                 'WC040302',
                 'WC040303',
                 'WC040304',
                 'WC040318',
                 'WC040366',
                 'WC070301',
                 'WC070302',
                 'WC290307',
                 'WC310304',
                 'WC310306',
                 'WC310313',
                 'WC310316',
                 'WC370310',
                 'WC420308',
                 'WC420310',
                 'WC420313',
                 'WC420314'
                 );

          Exec SQL
           Update WTENP
           Set ENpgm1 = '         ',
            ENpgm2 = 'SROFFICER',
            ENpgm3 = ' ',
            ENuser = :q1user,
            ENpgm = :q1pgm,
            ENDt = :sysDt7,
            ENtime = :sysTime6,
            ENotobf =
            Case
            When  Substring(ENform,1,8) = 'WC310304' Then 'E'
            When  Substring(ENform,1,8) = 'WC420308' Then 'E'
            Else 'I' End,
            ENotobv =
            Case
            When Substring(ENform,1,8) = 'WC040302' Then '2'
            When Substring(ENform,1,9) = 'WC040303 ' and :CltId <> '136'
             Then '34'
            When Substring(ENform,1,9) = 'WC040303 ' and :CltId = '136'
             Then 'A'
            When Substring(ENform,1,9) = 'WC040303A' Then '3'
            When Substring(ENform,1,9) = 'WC040303B' Then '3'
            When Substring(ENform,1,9) = 'WC040303C' Then '3'
            When Substring(ENform,1,9) = 'WC040304 ' Then '1'
            When Substring(ENform,1,8) = 'WC040318'  Then 'A'
            When Substring(ENform,1,9) = 'WC040366 ' Then '3'
            When Substring(ENform,1,8) = 'WC070301'  Then '12'
            When Substring(ENform,1,8) = 'WC070302'  Then '3'
            When Substring(ENform,1,8) = 'WC290307'  Then '12AD'
            When Substring(ENform,1,8) = 'WC310304'  Then '12'
            When Substring(ENform,1,9) = 'WC310306A' Then '3'
            When Substring(ENform,1,9) = 'WC310313A' Then '12'
            When Substring(ENform,1,9) = 'WC310313B' Then '12AD'
            When Substring(ENform,1,9) = 'WC310313C' Then '12AD'
            When Substring(ENform,1,9) = 'WC310316 ' Then '12'
            When Substring(ENform,1,9) = 'WC310316A' Then '12AD'
            When Substring(ENform,1,9) = 'WC310316B' Then '12AD'
            When Substring(ENform,1,9) = 'WC310316C' Then '12AD'
            When Substring(ENform,1,9) = 'WC370310C' Then '3'
            When Substring(ENform,1,8) = 'WC420308'  Then '1'
            When Substring(ENform,1,8) = 'WC420313'  Then '123'
            When Substring(ENform,1,8) = 'WC420314'  Then '123'
            Else ' '
            End,
            ENoef =
            Case
            When Substring(ENform,1,8) = 'WC000308' Then 'E'
            When Substring(ENform,1,8) = 'WC000310' Then 'I'
            When Substring(ENform,1,8) = 'WC040302' Then 'E'
            When Substring(ENform,1,8) = 'WC040303' Then 'E'
            When Substring(ENform,1,8) = 'WC040304' Then 'I'
            When Substring(ENform,1,8) = 'WC040318' Then 'E'
            When Substring(ENform,1,8) = 'WC040366' Then 'E'
            When Substring(ENform,1,8) = 'WC070301' Then 'I'
            When Substring(ENform,1,8) = 'WC070302' Then 'E'
            When Substring(ENform,1,8) = 'WC290307' Then 'I'
            When Substring(ENform,1,8) = 'WC310304' Then 'E'
            When Substring(ENform,1,8) = 'WC310306' Then 'I'
            When Substring(ENform,1,8) = 'WC310313' Then 'I'
            When Substring(ENform,1,8) = 'WC310316' Then 'E'
            When Substring(ENform,1,8) = 'WC370310' Then 'E'
            When Substring(ENform,1,8) = 'WC420308' Then 'E'
            When Substring(ENform,1,8) = 'WC420310' Then 'I'
            When Substring(ENform,1,8) = 'WC420313' Then 'I'
            When Substring(ENform,1,8) = 'WC420314' Then 'E'
            Else ' '
            End
            Where ENmnau = 'M'
             And Substring(ENform,1,8)
              In (
                 'WC000308',
                 'WC000310',
                 'WC040302',
                 'WC040303',
                 'WC040304',
                 'WC040318',
                 'WC040366',
                 'WC070301',
                 'WC070302',
                 'WC290307',
                 'WC310304',
                 'WC310306',
                 'WC310313',
                 'WC310316',
                 'WC370310',
                 'WC420308',
                 'WC420310',
                 'WC420313',
                 'WC420314'
                 );





        *InLr = *On;
      /End-free

