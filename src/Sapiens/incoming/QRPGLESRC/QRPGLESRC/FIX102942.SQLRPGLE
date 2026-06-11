     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     disEofData        s              1n   inz
     ddsData           ds                  inz
     d rrn                            9  0
     d time                           6  0
     d timeInit                       6  0
     d seconds                        2  0
     d minutes                        2  0
     d newTime                        6  0
     d newTimeInit                    6  0
      /copy sPrSql
       Exec sql set option commit = *none;
       Exec sql close a1;
       Exec sql declare a1 cursor for
        with
         calcs as
         (select rrn(ah) as calcsRRN, AHtime, AHitm,
           AHtime-floor(cast(AHtime/100 as decimal(6,2)))*100 as seconds,
           floor((AHtime-(floor(cast(AHtime/10000 as decimal(6,4)))*10000))/100)
            as minutes
           from WMAHP ah
         ),
         newCalcs as
         (select calcs.*,
           case
            when minutes > 59 and seconds > 59 then AHtime-5959
            when minutes > 59 then AHtime-5900
            when seconds > 59 then AHtime-59
            else AHtime end as newTime
           from calcs
         )
        select newCalcs.*,
         case
          when a2.ahitm=a2.ahtime then newTime
          else a2.ahitm end as newTimeInit
         from newCalcs
          join WMAHP a2 on rrn(a2)=calcsRRN
        where (minutes > 59  or seconds > 59);
       Exec sql open a1;
       Reset isEofData;
       Dou isEofData;
          Exec sql fetch a1 into :dsData;
          Select;
          When not IsValidFetch(sqlstt);
             isEofData = *on;
             Iter;
          Endsl;
          Exec sql
           update WMAHP
            set AHtime = :newTime, AHiTm = :newTimeInit
            where rrn(WMAHP) = :rrn;
       Enddo;
       *inlr = *on;
