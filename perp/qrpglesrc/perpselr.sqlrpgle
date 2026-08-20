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
dcl-s numCo     int(10);
dcl-s i         int(10);
dcl-s rrn       int(10);
dcl-s msgrrn    int(10);
dcl-s selRrn    int(10);
dcl-s changeRrn int(10);
dcl-s selOpt    char(1);
dcl-s msgkey    char(4);
dcl-s holdMsg   ind;

in ldaDS;
scursel = ldaDS.compcd;

dow not *in03 and not *in12;
  // A message queued by an action handler below (F6=Add, 2=Change,
  // 1=Select) must survive one full loop pass before being cleared,
  // or it never reaches the screen -- clearMsgs wipes msgrrn back to 0
  // on the very next pass, before this pass's own exfmt ever shows it.
  // holdMsg skips exactly one clearMsgs call right after such a
  // message was queued. Same pattern as wrkivpr (PERP-74).
  if holdMsg;
    holdMsg = *off;
  else;
    exsr clearMsgs;
  endif;
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

  if *in06;
    exsr addCompany;
    iter;
  endif;

  // Guard on numCo: READC against a subfile that was never written to
  // this cycle (0 rows loaded) raises a "Session or device error"
  // (CPF5006-class) runtime error instead of just returning *EOF.
  if numCo > 0;
    selRrn    = 0;
    changeRrn = 0;
    readc cosfl;
    dow not %eof(perpseld);
      if sopt <> '';
        selOpt = sopt;
        if selRrn > 0 or changeRrn > 0;
          writeMsg('Only one option may be used per Enter.');
        elseif selOpt = '1';
          selRrn = rrn;
        elseif selOpt = '2';
          changeRrn = rrn;
        else;
          writeMsg('Option ' + %trim(selOpt) + ' is not valid - use 1 or 2.');
        endif;
      endif;
      readc cosfl;
    enddo;
  endif;

  // 1=Select commits the LDA update and then exits the program
  // straight back to the caller (the menu) -- same as F3 -- instead
  // of redisplaying this list. No point queuing a confirmation message
  // first: with no further EXFMT on this device, it would never be
  // seen.
  if selRrn > 0 and msgrrn = 0;
    chain selRrn cosfl;
    ldaDS.compcd = scompc;
    out ldaDS;
    scursel = ldaDS.compcd;
    leave;
  endif;

  if changeRrn > 0 and msgrrn = 0;
    chain changeRrn cosfl;
    exsr changeCompany;
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
    leavesr;
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
// PERP-52: F6=Create new company. Minimal add panel over the company
// table -- required fields only, defaults matching the DDL's DEFAULTs
// (country_code='US', base_currency='USD'). *in60 off -- ECOMPC (the
// primary key) is editable while adding, protected while changing
// (see changeCompany below).
begsr addCompany;
  emode    = 'A';
  *in60    = *off;
  ecompc   = '';
  ecompnm  = '';
  eaddr1   = '';
  ecity    = '';
  estate   = '';
  epostcd  = '';
  ecntry   = 'US';
  ebasecur = 'USD';
  exfmt coedit;
  if not *in12 and ecompc <> '' and ecompnm <> '';
    exec sql
      insert into perpdemo.company
        (company_code, company_name, address_line1, city_name,
         state_code, postal_code, country_code, base_currency)
        values (:ecompc, :ecompnm, :eaddr1, :ecity,
                :estate, :epostcd, :ecntry, :ebasecur);
    if sqlcode < 0;
      writeMsg('Add company failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Company ' + %trim(ecompc) + ' created.');
    endif;
    holdMsg = *on;
  endif;
  // *in12 (F12) here only cancels the Add Company panel, not the
  // whole Select Company screen -- reset before returning to the
  // outer dow, which also tests *in12 to decide whether to exit.
  *in12 = *off;
endsr;

// ---------------------------------------------------------------------
// PERP-52: 2=Change. Same COEDIT panel as Add, but the company code
// (primary key, referenced by FK from nearly every other PERP table)
// is protected -- *in60 on makes ECOMPC display-only via DSPATR(PR)
// in the DDS, so the WHERE clause below always matches the row the
// user actually selected, never a typo'd or retyped code.
begsr changeCompany;
  emode    = 'C';
  *in60    = *on;
  ecompc   = scompc;
  exec sql
    select company_name, address_line1, city_name, state_code,
           postal_code, country_code, base_currency
      into :ecompnm, :eaddr1, :ecity, :estate,
           :epostcd, :ecntry, :ebasecur
      from perpdemo.company
     where company_code = :ecompc;
  if sqlcode <> 0;
    writeMsg('Company ' + %trim(ecompc) + ' disappeared before change.');
    holdMsg = *on;
    leavesr;
  endif;
  exfmt coedit;
  if not *in12;
    exec sql
      update perpdemo.company
         set company_name  = :ecompnm,
             address_line1 = :eaddr1,
             city_name     = :ecity,
             state_code    = :estate,
             postal_code   = :epostcd,
             country_code  = :ecntry,
             base_currency = :ebasecur,
             updated_at    = current_timestamp,
             updated_by    = user
       where company_code = :ecompc;
    if sqlcode < 0;
      writeMsg('Change failed: SQLCODE=' + %char(sqlcode)
             + ' SQLSTATE=' + sqlstate);
    else;
      writeMsg('Company ' + %trim(ecompc) + ' updated.');
    endif;
    holdMsg = *on;
  endif;
  *in12 = *off;
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
