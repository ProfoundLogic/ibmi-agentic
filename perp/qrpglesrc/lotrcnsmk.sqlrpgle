**free

// ---------------------------------------------------------------------
// Program: lotrcnsmk  (lotrecon smoke test)
// Purpose: One-shot caller that exercises lotrecon_run and prints the
//          results via SNDPGMMSG so a joblog + interactive session
//          confirms the service program is bound correctly. Also
//          proves out the CoderFlow demo path: reconciled_by =
//          'CODERFLOW' when called with that parameter.
//          Meant to be CALLed once from an interactive session:
//              CALL PGM(PERPDEMO/LOTRCNSMK) PARM('ACM' 'CODERFLOW         ')
// Epic:    PERP-8 (PERP-44)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP');

/copy lotrecon_pr.rpgle

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
  p_company      char(3);
  p_reconciledby char(18);
end-pi;

dcl-ds rows likeds(lotrecon_row) dim(200);
dcl-s errmsg   varchar(80);
dcl-s cnt      int(10);
dcl-s i        int(10);
dcl-s line     char(256);
dcl-s msgkey   char(4);

cnt = lotrecon_run(p_company : p_reconciledby : rows : errmsg);
line = 'lotrecon_run discrepancies=' + %char(cnt) + ' err=' + errmsg;
callMsg(line);

for i = 1 to cnt;
  line = %trim(rows(i).item) + ' before=' + %char(rows(i).qtybefore)
       + ' lotsum=' + %char(rows(i).lotsum)
       + ' after=' + %char(rows(i).qtyafter);
  callMsg(line);
endfor;

// The service program never commits (leaf module rule, DDL_STYLE_GUIDE
// Sec.13) -- the caller commits here.
exec sql commit;

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
