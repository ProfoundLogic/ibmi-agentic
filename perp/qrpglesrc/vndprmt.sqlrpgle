**free

// ---------------------------------------------------------------------
// Program: vndprmt  (standard, reusable Vendor Number prompt/lookup)
// Purpose: Companion to itmprmt (PERP-56). System-wide "?" + Enter
//          lookup for any keyable Vendor field. Same calling
//          convention: caller CALLs this program passing the company
//          code and the field's current value; '?' (or blank) lists
//          all vendors, any other seed pre-fills the Search field.
//          Subfile lists vendor_code + vendor_name, filtered
//          case-insensitively on either column; 1=Select on a row
//          returns that vendor_code. F3/F12 cancel and return the
//          field blank. Called via a plain dynamic CALL (EXTPGM
//          prototype declared in each caller), same idiom as itmprmt.
// Callers: wrkivnr (PERP-71).
// Epic:    PERP-51 (PERP-70)
// ---------------------------------------------------------------------

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-pi *n;
  pCompcd char(3)     const;
  pVendor varchar(10);
end-pi;

dcl-f vndprmtd workstn sfile(vpsfl:rrn) sfile(vpmsgsfl:msgrrn);

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

dcl-ds vendorRow qualified;
  vendor varchar(10);
  name   varchar(60);
end-ds;

dcl-ds rows likeds(vendorRow) dim(500);
dcl-s numRows int(10);
dcl-s i       int(10);
dcl-s rrn     int(10);
dcl-s msgrrn  int(10);
dcl-s msgkey  char(4);
dcl-s selRrn  int(10);
dcl-s search  varchar(30);

search = %trim(pVendor);
if search = '?';
  search = '';
endif;
ssearch = search;

dow not *in03 and not *in12;
  exsr clearMsgs;
  exsr loadRows;

  if numRows = 0;
    *in30 = *off;
    write vpnone;
  else;
    exsr fillSubfile;
    *in30 = *on;
  endif;

  write vpfoot;
  if msgrrn > 0;
    *in40 = *on;
    write vpmsgctl;
  else;
    *in40 = *off;
  endif;

  exfmt vpctl;

  if *in03 or *in12;
    pVendor = '';
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
    readc vpsfl;
    dow not %eof(vndprmtd);
      if sopt = '1';
        if selRrn = 0;
          selRrn = rrn;
        else;
          writeMsg('Only one vendor may be selected per Enter.');
        endif;
      elseif sopt <> '';
        writeMsg('Option ' + sopt + ' is not valid - use 1.');
      endif;
      readc vpsfl;
    enddo;
  endif;

  if selRrn > 0 and msgrrn = 0;
    chain selRrn vpsfl;
    pVendor = sivendor;
    leave;
  endif;

enddo;

*inlr = *on;
return;

// ---------------------------------------------------------------------
begsr loadRows;
  numRows = 0;
  exec sql declare vp1 cursor for
    select vendor_code, vendor_name
      from perpdemo.vendor
     where company_code = :pCompcd
       and (:search = ''
            or upper(vendor_code) like '%' || upper(:search) || '%'
            or upper(vendor_name) like '%' || upper(:search) || '%')
     order by vendor_code;
  exec sql open vp1;
  if sqlcode < 0;
    writeMsg('SQL open failed: SQLCODE=' + %char(sqlcode));
    leavesr;
  endif;

  dow numRows < %elem(rows);
    exec sql fetch vp1 into :vendorRow;
    if sqlcode = 100 or sqlcode < 0;
      leave;
    endif;
    numRows += 1;
    rows(numRows) = vendorRow;
  enddo;
  exec sql close vp1;
endsr;

// ---------------------------------------------------------------------
begsr fillSubfile;
  rrn = 0;
  *in31 = *on;
  write vpctl;
  *in31 = *off;
  for i = 1 to numRows;
    *in50 = *off;
    *in51 = *off;
    sopt     = '';
    sivendor = rows(i).vendor;
    sivname  = rows(i).name;
    rrn += 1;
    write vpsfl;
  endfor;
endsr;

// ---------------------------------------------------------------------
begsr clearMsgs;
  msgrrn = 0;
  *in41 = *on;
  write vpmsgctl;
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
  write vpmsgsfl;
end-proc;
