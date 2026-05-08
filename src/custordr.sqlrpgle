**free

/////////////////////////////////////////////////////////////////////
// Customer Order Inquiry
// Drill-down browser:
//   Screen 1 = Customers + summary order counts and totals
//   Screen 2 = Orders for selected customer with grand totals
//   Screen 3 = Detail lines for selected order
/////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);

dcl-f custordd workstn sfile(custsfl : ccrrn)
                       sfile(ordsfl  : orrn)
                       sfile(dtlsfl  : drrn)
                       sfile(msgsfl  : msgrrn)
                       indds(ind);

exec sql set option commit = *none;
exec sql set option closqlcsr = *endmod;

dcl-ds ind qualified;
  exit       ind pos(3);
  cancel     ind pos(12);
  cstSflDsp  ind pos(30);
  cstSflClr  ind pos(31);
  ordSflDsp  ind pos(32);
  ordSflClr  ind pos(33);
  dtlSflDsp  ind pos(34);
  dtlSflClr  ind pos(35);
  msgSflDsp  ind pos(40);
  msgSflClr  ind pos(41);
  optDsp     ind pos(50);
  optChg     ind pos(51);
  ooptDsp    ind pos(52);
  ooptChg    ind pos(53);
end-ds;

dcl-ds statusDS psds qualified;
  programName char(10) pos(334);
end-ds;

dcl-c MAX_ROWS const(999);

dcl-ds custSum_t qualified template;
  custno    packed(6 : 0);
  cname     char(40);
  cstate    char(2);
  cstatus   char(1);
  ordcnt    int(10);
  ordtot    packed(13 : 2);
end-ds;

dcl-ds ordRow_t qualified template;
  ordno     packed(8 : 0);
  orddate   packed(8 : 0);
  ordstat   char(1);
  lincnt    int(10);
  ordtotal  packed(11 : 2);
  ponumber  char(20);
  slsperson char(6);
end-ds;

dcl-ds ordHdr_t qualified template;
  custno    packed(6 : 0);
  cname     char(40);
  ordno     packed(8 : 0);
  orddate   packed(8 : 0);
  ordstat   char(1);
  ordtotal  packed(11 : 2);
  ponumber  char(20);
  slsperson char(6);
  shipvia   char(2);
end-ds;

dcl-ds dtlRow_t qualified template;
  lineno    packed(3 : 0);
  itemno    char(15);
  itemdesc  char(60);
  qtyord    packed(7 : 2);
  unitprice packed(9 : 2);
  extprice  packed(11 : 2);
end-ds;

dcl-ds custList likeds(custSum_t) dim(MAX_ROWS) inz;
dcl-ds ordList  likeds(ordRow_t)  dim(MAX_ROWS) inz;
dcl-ds dtlList  likeds(dtlRow_t)  dim(MAX_ROWS) inz;
dcl-ds curOrdHdr likeds(ordHdr_t) inz;

dcl-ds custBuf likeds(custSum_t) inz;
dcl-ds ordBuf  likeds(ordRow_t)  inz;
dcl-ds dtlBuf  likeds(dtlRow_t)  inz;

dcl-s numCust int(10);
dcl-s numOrd  int(10);
dcl-s numDtl  int(10);

dcl-s ccrrn   int(5);
dcl-s orrn    int(5);
dcl-s drrn    int(5);
dcl-s msgrrn  int(5);

dcl-s findCust   like(sfndcust);
dcl-s filter     like(sfilter);
dcl-s selCust    packed(6 : 0);
dcl-s selOrd     packed(8 : 0);
dcl-s i          int(10);

// ============================================================
// Mainline drill-down
// ============================================================
dow not ind.exit;
  exsr showCustomers;
  if ind.exit or selCust = 0;
    if not ind.exit;
      iter;
    endif;
    leave;
  endif;

  dow not ind.exit;
    exsr showOrders;
    if ind.exit or selOrd = 0;
      leave;
    endif;
    exsr showLines;
    if ind.exit;
      leave;
    endif;
  enddo;
enddo;

*inlr = *on;
return;

// ============================================================
// Screen 1 - Customer list with order summary
// ============================================================
begsr showCustomers;
  selCust = 0;
  exsr clearMSGSFL;
  exsr loadCustomers;

  dou *on;
    exsr buildCustSFL;
    ind.cstSflDsp = (numCust > 0);
    if ind.cstSflDsp;
      scoptdesc = '5=Display orders';
    else;
      scoptdesc = '';
    endif;
    sfkeys = 'F3=Exit   F5=Refresh';

    sfndcust = findCust;
    sfilter  = filter;

    write footer;
    if numCust = 0;
      write custnone;
    endif;
    if msgrrn > 0;
      ind.msgSflDsp = *on;
      write msgctl;
    else;
      ind.msgSflDsp = *off;
    endif;
    exfmt custctl;

    if ind.exit;
      leavesr;
    endif;

    if sfndcust <> findCust or sfilter <> filter;
      if sfndcust <> findCust;
        findCust = sfndcust;
        filter   = '';
      else;
        filter   = sfilter;
        findCust = 0;
      endif;
      exsr clearMSGSFL;
      exsr loadCustomers;
      iter;
    endif;

    exsr clearMSGSFL;
    sopt = '';
    readc custsfl;
    dow not %eof(custordd);
      if %trim(sopt) <> '';
        if %trim(sopt) = '5' and selCust = 0;
          selCust = scustno;
        elseif %trim(sopt) <> '5';
          writeMSGSFL('Invalid option: ' + %trim(sopt));
          ind.optDsp = *on;
          ind.optChg = *on;
          update custsfl;
          ind.optDsp = *off;
          ind.optChg = *off;
        endif;
      endif;
      readc custsfl;
    enddo;

    if selCust <> 0;
      leavesr;
    endif;
  enddo;
endsr;

begsr buildCustSFL;
  ccrrn = 0;
  ind.cstSflClr = *on;
  write custctl;
  ind.cstSflClr = *off;
  for i = 1 to numCust;
    sopt      = '';
    scustno   = custList(i).custno;
    scname    = %subst(custList(i).cname : 1 : 28);
    scstate   = custList(i).cstate;
    scstatus  = custList(i).cstatus;
    scordcnt  = custList(i).ordcnt;
    scordtot  = custList(i).ordtot;
    ind.optDsp = *off;
    ind.optChg = *off;
    ccrrn += 1;
    write custsfl;
  endfor;
endsr;

// ============================================================
// Screen 2 - Orders for selected customer
// ============================================================
begsr showOrders;
  selOrd = 0;
  ind.cancel = *off;
  exsr clearMSGSFL;
  exsr loadOrders;

  dou *on;
    exsr buildOrdSFL;
    ind.ordSflDsp = (numOrd > 0);
    if ind.ordSflDsp;
      sooptdesc = '5=Display lines';
    else;
      sooptdesc = '';
    endif;
    sfkeys = 'F3=Exit   F12=Cancel';

    socustno   = curOrdHdr.custno;
    socname    = curOrdHdr.cname;
    socnt      = numOrd;
    sogrndtot  = 0;
    for i = 1 to numOrd;
      sogrndtot += ordList(i).ordtotal;
    endfor;

    write footer;
    if numOrd = 0;
      write ordnone;
    endif;
    if msgrrn > 0;
      ind.msgSflDsp = *on;
      write msgctl;
    else;
      ind.msgSflDsp = *off;
    endif;
    exfmt ordctl;

    if ind.exit;
      leavesr;
    endif;
    if ind.cancel;
      leavesr;
    endif;

    exsr clearMSGSFL;
    soopt = '';
    readc ordsfl;
    dow not %eof(custordd);
      if %trim(soopt) <> '';
        if %trim(soopt) = '5' and selOrd = 0;
          selOrd = soordno;
        elseif %trim(soopt) <> '5';
          writeMSGSFL('Invalid option: ' + %trim(soopt));
          ind.ooptDsp = *on;
          ind.ooptChg = *on;
          update ordsfl;
          ind.ooptDsp = *off;
          ind.ooptChg = *off;
        endif;
      endif;
      readc ordsfl;
    enddo;

    if selOrd <> 0;
      leavesr;
    endif;
  enddo;
endsr;

begsr buildOrdSFL;
  orrn = 0;
  ind.ordSflClr = *on;
  write ordctl;
  ind.ordSflClr = *off;
  for i = 1 to numOrd;
    soopt    = '';
    soordno  = ordList(i).ordno;
    if ordList(i).orddate <> 0;
      soorddt = %char(%date(ordList(i).orddate) : *usa);
    else;
      soorddt = '';
    endif;
    sostat   = ordList(i).ordstat;
    solins   = ordList(i).lincnt;
    sototal  = ordList(i).ordtotal;
    sopono   = %subst(ordList(i).ponumber : 1 : 14);
    soslsp   = ordList(i).slsperson;
    ind.ooptDsp = *off;
    ind.ooptChg = *off;
    orrn += 1;
    write ordsfl;
  endfor;
endsr;

// ============================================================
// Screen 3 - Order line detail (display only)
// ============================================================
begsr showLines;
  ind.cancel = *off;
  exsr clearMSGSFL;
  exsr loadOrderLines;
  exsr buildDtlSFL;

  ind.dtlSflDsp = (numDtl > 0);
  sfkeys = 'F3=Exit   F12=Cancel';

  sdordno  = curOrdHdr.ordno;
  if curOrdHdr.orddate <> 0;
    sdorddt = %char(%date(curOrdHdr.orddate) : *usa);
  else;
    sdorddt = '';
  endif;
  sdordst  = curOrdHdr.ordstat;
  sdordtot = curOrdHdr.ordtotal;
  sdcustno = curOrdHdr.custno;
  sdcname  = curOrdHdr.cname;
  sdpono   = curOrdHdr.ponumber;
  sdslsp   = curOrdHdr.slsperson;
  sdshipv  = curOrdHdr.shipvia;

  write footer;
  if numDtl = 0;
    write dtlnone;
  endif;
  if msgrrn > 0;
    write msgctl;
  endif;
  exfmt dtlctl;
endsr;

begsr buildDtlSFL;
  drrn = 0;
  ind.dtlSflClr = *on;
  write dtlctl;
  ind.dtlSflClr = *off;
  for i = 1 to numDtl;
    sdlineno  = dtlList(i).lineno;
    sditemno  = %subst(dtlList(i).itemno : 1 : 12);
    sditemds  = %subst(dtlList(i).itemdesc : 1 : 22);
    sdqtyord  = dtlList(i).qtyord;
    sdunitpr  = dtlList(i).unitprice;
    sdextpr   = dtlList(i).extprice;
    drrn += 1;
    write dtlsfl;
  endfor;
endsr;

// ============================================================
// SQL data loaders
// ============================================================
begsr loadCustomers;
  numCust = 0;
  clear custList;

  if findCust > 0;
    exec sql declare c_cust1 cursor for
      select c.custno, c.cname, c.cstate, c.cstatus,
             coalesce(o.ordcnt, 0),
             coalesce(o.ordtot, 0)
      from aidemobase/custp c
      left join (
        select custno, count(*) as ordcnt, sum(ordtotal) as ordtot
        from aidemobase/ordhdrp group by custno
      ) o on o.custno = c.custno
      where c.custno = :findCust
      order by c.custno;

    exec sql open c_cust1;
    exsr fetchCust1;
    exec sql close c_cust1;
  else;
    exec sql declare c_cust2 cursor for
      select c.custno, c.cname, c.cstate, c.cstatus,
             coalesce(o.ordcnt, 0),
             coalesce(o.ordtot, 0)
      from aidemobase/custp c
      left join (
        select custno, count(*) as ordcnt, sum(ordtotal) as ordtot
        from aidemobase/ordhdrp group by custno
      ) o on o.custno = c.custno
      where :filter = '' or
            ucase(c.cname)  like '%' concat ucase(:filter) concat '%' or
            ucase(c.cemail) like '%' concat ucase(:filter) concat '%' or
            c.cphone like ucase(:filter) concat '%'
      order by c.custno;

    exec sql open c_cust2;
    exsr fetchCust2;
    exec sql close c_cust2;
  endif;

  if sqlcode < 0;
    writeMSGSFL('Error loading customers. SQLCODE=' + %char(sqlcode));
  endif;
endsr;

begsr fetchCust1;
  dow numCust < MAX_ROWS;
    exec sql fetch c_cust1 into :custBuf;
    if sqlcode <> 0;
      leave;
    endif;
    numCust += 1;
    custList(numCust) = custBuf;
  enddo;
endsr;

begsr fetchCust2;
  dow numCust < MAX_ROWS;
    exec sql fetch c_cust2 into :custBuf;
    if sqlcode <> 0;
      leave;
    endif;
    numCust += 1;
    custList(numCust) = custBuf;
  enddo;
endsr;

begsr loadOrders;
  numOrd = 0;
  clear ordList;
  clear curOrdHdr;

  exec sql
    select custno, cname into :curOrdHdr.custno, :curOrdHdr.cname
    from aidemobase/custp where custno = :selCust;

  exec sql declare c_ord cursor for
    select o.ordno, o.orddate, o.ordstat,
           coalesce(d.lcnt, 0), o.ordtotal,
           o.ponumber, o.slsperson
    from aidemobase/ordhdrp o
    left join (
      select ordno, count(*) as lcnt
      from aidemobase/orddtlp group by ordno
    ) d on d.ordno = o.ordno
    where o.custno = :selCust
    order by o.ordno desc;

  exec sql open c_ord;
  dow numOrd < MAX_ROWS;
    exec sql fetch c_ord into :ordBuf;
    if sqlcode <> 0;
      leave;
    endif;
    numOrd += 1;
    ordList(numOrd) = ordBuf;
  enddo;
  exec sql close c_ord;

  if sqlcode < 0;
    writeMSGSFL('Error loading orders. SQLCODE=' + %char(sqlcode));
  endif;
endsr;

begsr loadOrderLines;
  numDtl = 0;
  clear dtlList;

  exec sql
    select o.custno, c.cname, o.ordno, o.orddate, o.ordstat,
           o.ordtotal, o.ponumber, o.slsperson, o.shipvia
    into  :curOrdHdr.custno, :curOrdHdr.cname,
          :curOrdHdr.ordno,  :curOrdHdr.orddate,
          :curOrdHdr.ordstat,:curOrdHdr.ordtotal,
          :curOrdHdr.ponumber,:curOrdHdr.slsperson,
          :curOrdHdr.shipvia
    from  aidemobase/ordhdrp o
    left join aidemobase/custp c on c.custno = o.custno
    where o.ordno = :selOrd;

  exec sql declare c_dtl cursor for
    select d.lineno, d.itemno,
           coalesce(p.itemdesc, ''),
           d.qtyord, d.unitprice, d.extprice
    from aidemobase/orddtlp d
    left join aidemobase/prodp p on p.itemno = d.itemno
    where d.ordno = :selOrd
    order by d.lineno;

  exec sql open c_dtl;
  dow numDtl < MAX_ROWS;
    exec sql fetch c_dtl into :dtlBuf;
    if sqlcode <> 0;
      leave;
    endif;
    numDtl += 1;
    dtlList(numDtl) = dtlBuf;
  enddo;
  exec sql close c_dtl;

  if sqlcode < 0;
    writeMSGSFL('Error loading order lines. SQLCODE=' + %char(sqlcode));
  endif;
endsr;

// ============================================================
// Message subfile helpers
// ============================================================
begsr clearMSGSFL;
  msgrrn = 0;
  ind.msgSflClr = *on;
  write msgctl;
  ind.msgSflClr = *off;
endsr;

dcl-proc writeMSGSFL;
  dcl-pi *n;
    msgData varchar(80) const options(*varsize);
  end-pi;

  dcl-pr QMHSNDPM extpgm;
    msgId char(7) const;
    msgF char(20) const;
    msgData char(80) const;
    msgDataLen int(10) const;
    msgType char(10) const;
    stackEntry char(10) const;
    stackCounter int(10) const;
    msgKey char(4);
    errorCode char(8) const;
  end-pr;

  dcl-s data char(80);
  data = msgData;
  QMHSNDPM(
    'CPF9897' :
    'QCPFMSG   QSYS      ' :
    data :
    %len(msgData) :
    '*INFO     ' :
    '*         ' :
    1 :
    smsgkey :
    x'0000000000000000'
  );
  msgrrn += 1;
  spgmq = 'CUSTORDR';
  write msgsfl;
end-proc;
