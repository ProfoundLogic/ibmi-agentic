**free

// ---------------------------------------------------------------------
// Program: itmprmt  (standard, reusable Item Number prompt/lookup)
// Purpose: System-wide "?" + Enter lookup for any keyable Item Number
//          field. Caller CALLs this program passing the company code
//          and the field's current value; typing '?' into the field
//          before Enter is the trigger convention every caller uses,
//          so this program treats a '?' seed the same as a blank
//          search (full list). Any other seed value pre-fills the
//          Search field so partial text the user already typed keeps
//          working as a filter. The subfile lists item_number +
//          item_description, filtered case-insensitively on either
//          column; 1=Select on a row returns that item_number in the
//          same parameter. F3/F12 cancel and return the field blank.
//          Model: perpselr's subfile-picker pattern (PERP-16),
//          adapted for field-level invocation instead of a full-screen
//          menu step. Called via a plain dynamic CALL (EXTPGM
//          prototype declared in each caller) -- the same idiom
//          wrkitmr already uses to call wrkcnvr/wrklotr -- not a bound
//          service program, so no bnddir/exports wiring is needed.
// Callers: wrkcnvr (PERP-54), wrkitmr (PERP-57), wrkivnr (PERP-58),
//          wrkivpr (PERP-59), wrklotr (PERP-60), reqentr (PERP-61),
//          poentr (PERP-62).
// Epic:    PERP-51 (PERP-56)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-pi *n;
  pCompcd char(3)     const;
  pItem   varchar(25);
end-pi;

dcl-f itmprm2d workstn sfile(itpsfl:rrn) sfile(itpmsgsfl:msgrrn);

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

dcl-ds itemRow qualified;
  item varchar(25);
  desc varchar(60);
end-ds;

dcl-ds rows likeds(itemRow) dim(500);
dcl-s numRows int(10);
dcl-s i       int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s msgkey  char(4);
dcl-s selRrn  int(10);
dcl-s search  varchar(30);

search = %trim(pItem);
if search = '?';
  search = '';
endif;
ssearch = search;

dow not *in03 and not *in12;
  exsr clearMsgs;
  exsr loadRows;

  if numRows = 0;
    *in30 = *off;
    write itpnone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write itpfoot;
  if msgrrn > 0;
    *in40 = *on;
    write itpmsgctl;
  else;
    *in40 = *off;
  endif;

  exfmt itpctl;

  if *in03 or *in12;
    pItem = '';
    leave;
  endif;

  if *in05;
    iter;
  endif;

  if ssearch <> search;
    search = %trim(ssearch);
    iter;
  endif;

  // Guard on numRows: READC against a subfile that was never written
  // to this cycle (0 rows loaded) raises a "Session or device error"
  // (CPF5006-class) runtime error instead of just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    readc itpsfl;
    dow not %eof(itmprm2d);
      if sopt = '1';
        if selRrn = 0;
          selRrn = rrn;
        else;
          writeMsg('Only one item may be selected per Enter.');
        endif;
      elseif sopt <> '';
        writeMsg('Option ' + sopt + ' is not valid - use 1.');
      endif;
      readc itpsfl;
    enddo;
  endif;

  if selRrn > 0 and msgrrn = 0;
    chain selRrn itpsfl;
    pItem = siitem;
    leave;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare ip1 cursor for
    select item_number, item_description
      from perpdemo.item
     where company_code = :pCompcd
       and (:search = ''
            or upper(item_number) like '%' || upper(:search) || '%'
            or upper(item_description) like '%' || upper(:search) || '%')
     order by item_number;
  exec sql open ip1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch ip1 into :itemRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = itemRow;
  enddo;
  exec sql close ip1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write itpctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt   = '';
    siitem = rows(i).item;
    sidesc = %subst(rows(i).desc : 1 : %min(%len(rows(i).desc) : 48));
    rrn += 1;
    write itpsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write itpmsgctl;
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
  write itpmsgsfl;
end-proc;
