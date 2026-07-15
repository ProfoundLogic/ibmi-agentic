**free

// ---------------------------------------------------------------------
// Program: perpselr  (PERP company selection utility)
// Purpose: Interactive picker over the company table. On selection,
//          persists the chosen company_code to the job's *LDA (positions
//          1-3), which every IBM i job has automatically. Downstream
//          PERP programs read *LDA[1:3] to scope their queries.
//          Session-local: the LDA vanishes when the job ends, exactly
//          the behavior the epic asks for.
// Epic:    PERP-2 (PERP-16)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f perpseld workstn sfile(cosfl:rrn) sfile(comsgsfl:msgrrn);

// Local Data Area — every job has one, 1024 chars. Positions 1-3 hold
// the selected PERP company_code for downstream PERP programs.
dcl-ds ldaDS dtaara(*lda) len(1024) qualified;
  compcd char(3) pos(1);
end-ds;

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

dcl-ds statusDS psds qualified;
  programName char(10) pos(334);
end-ds;

dcl-ds coRow qualified;
  code    char(3);
  name    varchar(60);
  curr    varchar(20);
end-ds;

dcl-ds companies likeds(coRow) dim(200);
dcl-s numCo    int(10);
dcl-s i        int(10);
dcl-s rrn      int(10);
dcl-s msgrrn   int(10);
dcl-s selRrn   int(10);
dcl-s msgkey   char(4);

in ldaDS;
scursel = ldaDS.compcd;

dow not *in03 and not *in12;
  exsr clearMsgs;
  exsr loadCompanies;

  if numCo = 0;
    *in30 = *off;
    write conone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write cofoot;
  if msgrrn > 0;
    *in40 = *on;
    write comsgctl;
  else;
    *in40 = *off;
  endif;

  exfmt coctl;

  if *in03 or *in12;
    leave;
  endif;

  if *in05;
    iter;  // refresh
  endif;

  // Guard on numCo: READC against a subfile that was never written to
  // this cycle (0 rows loaded) raises a "Session or device error"
  // (CPF5006-class) runtime error instead of just returning *EOF.
  if numCo > 0;
    selRrn = 0;
    readc cosfl;
    dow not %eof(perpseld);
      if sopt = '1';
        if selRrn = 0;
          selRrn = rrn;
        else;
          writeMsg('Only one company may be selected per Enter.');
        endif;
      elseif sopt <> '';
        writeMsg('Option ' + %trim(sopt) + ' is not valid - use 1.');
      endif;
      readc cosfl;
    enddo;
  endif;

  if selRrn > 0 and msgrrn = 0;
    chain selRrn cosfl;
    ldaDS.compcd = scompc;
    out ldaDS;
    writeMsg('Company ' + ldaDS.compcd + ' selected for this session.');
    scursel = ldaDS.compcd;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadCompanies;
  numCo = 0;
  exec sql declare cocsr cursor for
    select company_code, company_name, base_currency
      from perpdemo.company
     where is_active = 'Y'
     order by company_code;
  exec sql open cocsr;
  if sqlcode < 0;
    writeMsg('SQL error opening cursor: ' + %char(sqlcode));
    return;
  endif;

  dow numCo < %elem(companies);
    exec sql fetch cocsr into :coRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numCo += 1;
    companies(numCo) = coRow;
  enddo;
  exec sql close cocsr;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write coctl;
  *in31 = *off;
  for i = 1 to numCo;
    *in50 = *off;
    *in51 = *off;
    sopt   = '';
    scompc = companies(i).code;
    scompn = companies(i).name;
    scurr  = companies(i).curr;
    rrn += 1;
    write cosfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write comsgctl;
  *in41 = *off;
endsr;

// ---------------------------------------------------------------------
dcl-proc writeMsg;
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
    smsgkey :
    x'0000000000000000');
  msgrrn += 1;
  spgmq = statusDS.programName;
  write comsgsfl;
end-proc;
