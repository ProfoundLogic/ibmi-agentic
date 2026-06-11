     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     ddsInput          ds                  inz
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inCase                         7  0
     d inDt                           7  0
     d inTime                         6  0

     disUpdate         s              1n   inz('1')
      /Free
       Exec SQL
        declare cursor1 cursor for
         with date as (
          select d.RHCO#, d.RHFND, d.RHFYR, d.RHCASE, MIN(d.RHDT) min_date
           from WMRHP d
           group by d.RHCO#, d.RHFND, d.RHFYR, d.RHCASE
           order by d.RHCO#, d.RHFND, d.RHFYR, d.RHCASE
         )
         select t.RHCO#, t.RHFND, t.RHFYR, t.RHCASE, t.RHDT,
           MIN(t.RHTIME) min_time
          from WMRHP t
          join date d on d.RHCO#=t.RHCO# and d.RHFND=t.RHFND
           and d.RHFYR=t.RHFYR
           and d.RHCASE=t.RHCASE and min_date=t.RHDT
          group by t.RHCO#, t.RHFND, t.RHFYR, t.RHCASE, t.RHDT
          order by t.RHCO#, t.RHFND, t.RHFYR, t.RHCASE, t.RHDT;
       Exec SQL
        open cursor1;
       Dou SQLCOD <> 0;
          Exec SQL
           fetch cursor1 into :dsInput;
          Select;
          When SQLCOD <> 0;
             Iter;
          When not isUpdate;
             Iter;
          Endsl;
          Exec SQL
           update WMRHP
            set RHinitRsv = 'Y'
            where RHCO# = :inCo# and RHFND = :inFnd and RHFYR = :inFyr
             and RHCASE = :inCase and RHDT = :inDt and RHTIME = :inTime
            with NC;
       Enddo;
       *inlr = *on;
      /End-free
