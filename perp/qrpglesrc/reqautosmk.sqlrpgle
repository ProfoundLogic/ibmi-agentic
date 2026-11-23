**free

// ---------------------------------------------------------------------
// Program: reqautosmk  (reqauto smoke test)
// Purpose: One-shot caller that exercises reqauto_evaluate against a
//          real requisition so a joblog + interactive session confirms
//          the service program is bound correctly.
//          Meant to be CALLed once from an interactive session:
//              CALL PGM(PERPDEMO/REQAUTOSMK) PARM('ACM' '3       ')
//          (requisition 3 must exist and be status SUBMITTED). The
//          requisition number parameter is CHAR(8), not numeric -- raw
//          CALL/PARM (no *CMD definition, no RPG prototype on the
//          caller's side) sends exactly the literal's own length with
//          no padding to the receiver's declared size, so the literal
//          MUST be padded to exactly 8 characters (trailing blanks) or
//          the receiver reads past the passed argument (RNX0105 at
//          runtime, confirmed PERP-36 -- an unquoted numeric literal or
//          an unpadded quoted one both corrupt the value silently/
//          fatally rather than raising a friendly error).
// Epic:    PERP-6 (PERP-36)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP');

/copy reqauto_pr.rpgle

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
  p_company char(3);
  p_reqnbr  char(8);
end-pi;

dcl-s errmsg varchar(80);
dcl-s result int(10);
dcl-s reqnbr  int(20);
dcl-s line   char(256);
dcl-s msgkey char(4);

reqnbr = %dec(%trim(p_reqnbr) : 20 : 0);
result = reqauto_evaluate(p_company : reqnbr : errmsg);
line = 'reqauto_evaluate(' + %trim(p_company) + ',' + %char(reqnbr)
     + ') = ' + %char(result) + ' err=' + errmsg;
callMsg(line);

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
