**free

// ---------------------------------------------------------------------
// Program: docseqsmk  (docseq smoke test)
// Purpose: One-shot caller that exercises docseq_next / docseq_peek and
//          prints the results via SNDPGMMSG so a joblog + interactive
//          session confirms the service program is bound correctly.
//          Meant to be CALLed once from an interactive session:
//              CALL PGM(PERPDEMO/DOCSEQSMK) PARM('ACM' 'PO ')
//          Also runs a peek to prove no-op reads.
// Epic:    PERP-2 (PERP-19)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new) bnddir('PERP');

/copy docseq_pr.rpgle

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
  p_doctype  char(3);
end-pi;

dcl-s doctype  varchar(20);
dcl-s errmsg   varchar(80);
dcl-s n1       int(20);
dcl-s n2       int(20);
dcl-s peek     int(20);
dcl-s line     char(256);
dcl-s msgkey   char(4);

doctype = %trim(p_doctype);

// Peek before bumping.
peek = docseq_peek(p_company : doctype : errmsg);
line = 'docseq peek before: ' + %char(peek) + ' err=' + errmsg;
callMsg(line);

// First bump.
n1 = docseq_next(p_company : doctype : errmsg);
line = 'docseq_next #1: ' + %char(n1) + ' err=' + errmsg;
callMsg(line);

// Second bump.
n2 = docseq_next(p_company : doctype : errmsg);
line = 'docseq_next #2: ' + %char(n2) + ' err=' + errmsg;
callMsg(line);

// Commit so the bumps stick.
exec sql commit;

// Peek after.
peek = docseq_peek(p_company : doctype : errmsg);
line = 'docseq peek after : ' + %char(peek) + ' err=' + errmsg;
callMsg(line);

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
