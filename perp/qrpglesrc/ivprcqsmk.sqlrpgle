**free

// ---------------------------------------------------------------------
// Program: ivprcqsmk  (itmvprcq smoke test)
// Purpose: One-shot caller that exercises itmvprcq_history and prints
//          the results via SNDPGMMSG so a joblog + interactive session
//          confirms the service program is bound correctly.
//          Meant to be CALLed once from an interactive session:
//              CALL PGM(PERPDEMO/IVPRCQSMK) PARM('ACM' 'WIDGET1')
//          Program object name is itself only 9 chars ('itmvprcqsmk'
//          would be 11 -- CPD0074, IBM i object names cap at 10).
// Epic:    PERP-5 (PERP-32)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP');

/copy itmvprcq_pr.rpgle

dcl-pr QMHSNDPM extpgm;
  msgId       char(7)   const;
  msgF        char(20)  const;
  msgData     char(256) const;
  msgDataLen  int(10)   const;
  msgType     char(10)  const;
  stackEntry  char(10)  const;
  stackCntr   int(10)   const;
  msgKey      char(4);
  errorCode   char(8)   const;
end-pr;

dcl-pi *n;
  p_company  char(3);
  p_item     char(25);
end-pi;

dcl-ds rows likeds(itmvprcq_row) dim(200);
dcl-s item     varchar(25);
dcl-s errmsg   varchar(80);
dcl-s cnt      int(10);
dcl-s i        int(10);
dcl-s line     char(256);
dcl-s msgkey   char(4);
dcl-s efftoTxt varchar(10);

item = %trim(p_item);

cnt = itmvprcq_history(p_company : item : rows : errmsg);
line = 'itmvprcq_history rows=' + %char(cnt) + ' err=' + errmsg;
callMsg(line);

for i = 1 to cnt;
  if %trim(rows(i).effto) = '';
    efftoTxt = 'current';
  else;
    efftoTxt = %trim(rows(i).effto);
  endif;
  line = %trim(rows(i).vendor) + ' (' + %trim(rows(i).vendorNm) + ') '
       + %trim(rows(i).efffrm) + ' to ' + efftoTxt
       + ' : ' + %char(rows(i).price) + ' ' + %trim(rows(i).currency);
  callMsg(line);
endfor;

*inlr = *on;
return;

dcl-proc callMsg;
  dcl-pi *n;
    text varchar(256) const;
  end-pi;
  dcl-s data char(256);
  data = text;
  QMHSNDPM(
    'CPF9897' :
    'QCPFMSG   QSYS      ' :
    data :
    %len(text) :
    '*INFO     ' :
    '*         ' :
    1 :
    msgkey :
    x'0000000000000000');
end-proc;
