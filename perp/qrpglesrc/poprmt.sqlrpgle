**free

// ---------------------------------------------------------------------
// Program: poprmt  (standard, reusable PO Number prompt/lookup)
// Purpose: PERP-98. Lists purchase orders still awaiting receipt
//          (status_code OPEN or PARTIAL) for the calling company,
//          with a Search box filtering on PO number or vendor
//          code/name, case-insensitive. 1=Select on a row returns
//          that po_number. F3/F12 cancel and return 0.
//          po_number is a NUMERIC field (HPONBR on rcventd.dspf is
//          15Y 0), so this program does NOT use the "?" + Enter
//          convention the item/vendor prompts use (a 5250 numeric
//          input field rejects a literal "?" character at the device
//          level, before RPG ever sees it) -- callers instead invoke
//          this via a dedicated F4=Prompt key, the conventional IBM i
//          UI idiom for a numeric-field lookup.
// Callers: rcventr (PERP-98).
// Epic:    PERP-51 (PERP-98)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-pi *n;
  pCompcd char(3)       const;
  pPonbr  packed(15:0);
end-pi;

dcl-f poprmtd workstn sfile(ppsfl:rrn) sfile(ppmsgsfl:msgrrn);

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

dcl-ds poRow qualified;
  ponbr int(20);
  vndr  varchar(10);
  vndnm varchar(60);
  stat  varchar(20);
end-ds;

dcl-ds rows likeds(poRow) dim(500);
dcl-s numRows int(10);
dcl-s i       int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s msgkey  char(4);
dcl-s selRrn  int(10);
dcl-s search  varchar(30);

search = '';
ssearch = search;

dow not *in03 and not *in12;
  exsr clearMsgs;
  exsr loadRows;

  if numRows = 0;
    *in30 = *off;
    write ppnone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write ppfoot;
  if msgrrn > 0;
    *in40 = *on;
    write ppmsgctl;
  else;
    *in40 = *off;
  endif;

  exfmt ppctl;

  if *in03 or *in12;
    pPonbr = 0;
    leave;
  endif;

  if *in05;
    iter;
  endif;

  if %trim(ssearch) <> search;
    search = %trim(ssearch);
    iter;
  endif;

  // Guard on numRows: READC against a subfile that was never written
  // to this cycle (0 rows loaded) raises a "Session or device error"
  // (CPF5006-class) runtime error instead of just returning *EOF.
  if numRows > 0;
    selRrn = 0;
    readc ppsfl;
    dow not %eof(poprmtd);
      if sopt = '1';
        if selRrn = 0;
          selRrn = rrn;
        else;
          writeMsg('Only one PO may be selected per Enter.');
        endif;
      elseif sopt <> '';
        writeMsg('Option ' + sopt + ' is not valid - use 1.');
      endif;
      readc ppsfl;
    enddo;
  endif;

  if selRrn > 0 and msgrrn = 0;
    chain selRrn ppsfl;
    pPonbr = siponbr;
    leave;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare pp1 cursor for
    select h.po_number, h.vendor_code, v.vendor_name, h.status_code
      from perpdemo.po_header h
      join perpdemo.vendor v
        on v.company_code = h.company_code and v.vendor_code = h.vendor_code
     where h.company_code = :pCompcd
       and h.status_code in ('OPEN', 'PARTIAL')
       and (:search = ''
            or char(h.po_number) like '%' || :search || '%'
            or upper(v.vendor_code) like '%' || upper(:search) || '%'
            or upper(v.vendor_name) like '%' || upper(:search) || '%')
     order by h.po_number desc;
  exec sql open pp1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch pp1 into :poRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = poRow;
  enddo;
  exec sql close pp1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write ppctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt    = '';
    siponbr = rows(i).ponbr;
    sivndr  = rows(i).vndr;
    sivndnm = rows(i).vndnm;
    sistat  = rows(i).stat;
    rrn += 1;
    write ppsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write ppmsgctl;
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
  write ppmsgsfl;
end-proc;
