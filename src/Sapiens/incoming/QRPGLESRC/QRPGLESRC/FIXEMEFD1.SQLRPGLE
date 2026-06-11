     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H* This program is to ensure that the initial inception date of the
     H*  division is not greater than the earliest policy effective date.
     H*
     H* Created: 05/08/18   C:33   Support: 70670
     H*====================================================================
     Ddate7            s              7  0
     Dtime6            s              6  0
     DdsEmp            ds                  qualified inz
     D co#                            3  0
     D fnd                            3  0
     D emp#                           9  0
     D div                            5  0
     D minDt                          7  0
     Dpsds           esds                  extname(szq1p)
     D/copy sPrDate
     D/copy sPrMessage
      /Free
       date7 = DateTo7(SystemDate);
       time6 = TimeTo6(SystemTime);
       Exec sql
        set option commit=*none;
       Exec sql
        declare c1 cursor for
         with subWDELP as
           (select ELCO#, ELFND, ELEMP#, ELDIV, min(ELPFDT) minPfDt
             from WDELP
             group by ELCO#, ELFND, ELEMP#, ELDIV
             order by ELCO#, ELFND, ELEMP#, ELDIV)
         select EMCO#, EMFND, EMEMP#, EMDIV, minPfDt
          from WMEMP
          join subWDELP
           on (ELCO# = EMCO# and ELFND = EMFND and
               ELEMP# = EMEMP# and ELDIV = EMDIV)
          where EMEFD1 > minPfDt
          order by EMCO#, EMFND, EMEMP#, EMDIV;
       Exec sql open c1;
       Dou sqlstt <> '00000';
          Exec sql
           fetch c1 into :dsEmp;
          Select;
          When sqlstt <> '00000';
             Iter;
          Endsl;
          Exec sql
           update WMEMP
            set EMEFD1 = :dsEmp.minDt,
             EMPGM = :q1pgm, EMUSER = :q1user, EMDT = :date7, EMTIME = :time6
            where EMCO# = :dsEmp.co# and EMFND = :dsEmp.fnd
             and EMEMP# = :dsEmp.emp# and EMDIV = :dsEmp.div;
          Select;
          When sqlstt <> '00000';
             SndPgmMsg('CPF9898':'*STATUS':'QCPFMSG''*LIBL':
              'FIXEMEFD1 ended abnormally.');
          Endsl;
       Enddo;
       Exec sql close c1;
       *inlr = *on;
      /End-free
