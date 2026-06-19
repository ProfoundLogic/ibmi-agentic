**free

/////////////////////
// Pallet Contents Maintenance (EJS Screen Mode)
//
// Hornady-branded EJS clone of HYR0138 option 2 -- a CRUD-style view
// over HYPPLLT (shipping pallet master), joined with HDCUST for the
// ship-to customer name.
//
// The original HYR0138 is blocked by a missing HYD0138.DSPF; this
// program reads / mutates the same table directly.
//
// Actions:
//   SEARCH / CLEAR / EXIT  -- filter controls
//   CREATE                 -- insert a new pallet (newshto+dimensions)
//   CLOSE                  -- set PMSHT='C' (close pallet)
//   REOPEN                 -- set PMSHT='O'
//   DELETE                 -- SQL DELETE
//   TOGGLE_HAZ             -- flip PMHZLB between Y / N
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt option(*srcstmt : *nodebugio);

dcl-f hyr0138eo workstn sfile(palsfl : rrn) handler('PROFOUNDUI(HANDLER)');

dcl-ds pal_t qualified template;
  psrl    zoned(8 : 0);
  pshto   zoned(7 : 0);
  pshtonm char(30);
  pstat   char(1);
  pbol    zoned(8 : 0);
  pinvc   char(1);
  phaz    char(1);
  plen    zoned(7 : 2);
  pwid    zoned(7 : 2);
  phgt    zoned(7 : 2);
  pempl   zoned(5 : 0);
  ptstp   timestamp;
end-ds;

dcl-ds pals likeds(pal_t) dim(9999);
dcl-ds pRow likeds(pal_t);
dcl-s numPals int(10);

dcl-s rrn  int(10);
dcl-s i    int(10);
dcl-s curStatus char(1);
dcl-s curFilter char(30);
dcl-s curHaz    char(1);
dcl-s nextSrl   zoned(8 : 0);

dow action <> 'EXIT';

  exsr processAction;
  exsr loadPallets;

  if numPals = 0 and msg = '';
    msg = 'No pallets match the current filter.';
    msgType = 'info';
  endif;

  exsr loadSubfile;

  ffilter = curFilter;
  fstatus = curStatus;
  action  = '';
  selShTo = 0;
  selPsrl = 0;
  newShTo = 0;
  newLen  = 0;
  newWid  = 0;
  newHgt  = 0;
  newHzlb = '';
  exfmt palctl;

  if action = 'CLEAR';
    curFilter = '';
    curStatus = '';
  else;
    curFilter = ffilter;
    curStatus = fstatus;
  endif;

enddo;

*inlr = *on;

//////////////////////////////////////////////////////////////////////
// processAction -- mutate HYPPLLT then refresh.
//////////////////////////////////////////////////////////////////////
begsr processAction;
  msg = '';
  msgType = '';

  select;
    when action = '' or action = 'SEARCH' or action = 'CLEAR' or
         action = 'EXIT';
      // filter-only / no-op
    when action = 'CREATE';
      if newShTo = 0;
        msg = 'Ship-to customer is required.';
        msgType = 'error';
      else;
        // Get the next serial number in the range [9001..9999]
        nextSrl = 0;
        exec sql
          SELECT COALESCE(MAX(PMPSRL#) + 1, 9001)
            INTO :nextSrl
            FROM HYPPLLT
           WHERE PMPSRL# BETWEEN 9001 AND 9999;
        if newLen = 0;
          newLen = 48;
        endif;
        if newWid = 0;
          newWid = 40;
        endif;
        if newHgt = 0;
          newHgt = 48;
        endif;
        if newHzlb <> 'Y';
          newHzlb = 'N';
        endif;
        exec sql
          INSERT INTO HYPPLLT
            (PMSHTO, PMPSRL#, PMDSHP, PMSHT, PMBOL#, PMINVC,
             PMEMPL, PMEMTY, PMLEMPL, PMLEMTY, PMLACT,
             PMHZLB, PMSRC, PMLEN, PMWID, PMHGT,
             PMTSTP, PMLSTP, PMOSTP)
          VALUES
            (:newShTo, :nextSrl, 0, 'O', 0, 'N',
             12345, 'P', 12345, 'P', 'CREATE',
             :newHzlb, 'M', :newLen, :newWid, :newHgt,
             CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
        if sqlcode = 0;
          msg = 'Pallet ' + %char(nextSrl) + ' created for ship-to ' +
                %char(newShTo) + '.';
          msgType = 'success';
        else;
          msg = 'Insert failed -- SQLCODE ' + %char(sqlcode) + '.';
          msgType = 'error';
        endif;
      endif;

    when (action = 'CLOSE' or action = 'REOPEN' or
          action = 'DELETE' or action = 'TOGGLE_HAZ') and
         selShTo > 0 and selPsrl > 0;
      select;
        when action = 'CLOSE';
          exec sql
            UPDATE HYPPLLT
               SET PMSHT = 'C', PMLSTP = CURRENT_TIMESTAMP
             WHERE PMSHTO = :selShTo AND PMPSRL# = :selPsrl;
          msg = 'Pallet ' + %char(selPsrl) + ' closed.';
          msgType = 'success';
        when action = 'REOPEN';
          exec sql
            UPDATE HYPPLLT
               SET PMSHT = 'O', PMLSTP = CURRENT_TIMESTAMP
             WHERE PMSHTO = :selShTo AND PMPSRL# = :selPsrl;
          msg = 'Pallet ' + %char(selPsrl) + ' reopened.';
          msgType = 'success';
        when action = 'TOGGLE_HAZ';
          exec sql
            UPDATE HYPPLLT
               SET PMHZLB = CASE WHEN PMHZLB = 'Y' THEN 'N' ELSE 'Y' END,
                   PMLSTP = CURRENT_TIMESTAMP
             WHERE PMSHTO = :selShTo AND PMPSRL# = :selPsrl;
          msg = 'Hazmat flag toggled on pallet ' + %char(selPsrl) + '.';
          msgType = 'success';
        when action = 'DELETE';
          exec sql
            DELETE FROM HYPPLLT
             WHERE PMSHTO = :selShTo AND PMPSRL# = :selPsrl;
          msg = 'Pallet ' + %char(selPsrl) + ' deleted.';
          msgType = 'success';
      endsl;

    other;
      msg = 'Unknown / incomplete action: ' + %trim(action);
      msgType = 'error';
  endsl;
endsr;

//////////////////////////////////////////////////////////////////////
// loadPallets -- cursor over HYPPLLT with current filters.
//////////////////////////////////////////////////////////////////////
begsr loadPallets;
  numPals  = 0;
  totPal   = 0;
  openCt   = 0;
  closedCt = 0;
  hazCt    = 0;
  totVol   = 0;

  exec sql
    DECLARE C_PAL CURSOR FOR
      SELECT P.PMPSRL#, P.PMSHTO,
             COALESCE(C.CMCNA1, ''),
             P.PMSHT, P.PMBOL#, P.PMINVC, P.PMHZLB,
             P.PMLEN, P.PMWID, P.PMHGT, P.PMEMPL, P.PMTSTP
        FROM HYPPLLT P
        LEFT JOIN HDCUST C ON C.CMCUST = P.PMSHTO
       WHERE (:curStatus = '' OR P.PMSHT = :curStatus)
         AND (:curFilter = '' OR
              CHAR(P.PMSHTO) LIKE '%' CONCAT RTRIM(:curFilter) CONCAT '%' OR
              CHAR(P.PMPSRL#) LIKE '%' CONCAT RTRIM(:curFilter) CONCAT '%' OR
              UPPER(C.CMCNA1) LIKE '%' CONCAT UPPER(RTRIM(:curFilter)) CONCAT '%')
       ORDER BY P.PMSHTO, P.PMPSRL#;

  exec sql OPEN C_PAL;
  dow numPals < %elem(pals);
    exec sql FETCH FROM C_PAL INTO :pRow;
    if sqlcode <> 0;
      leave;
    endif;
    numPals += 1;
    pals(numPals) = pRow;
  enddo;
  exec sql CLOSE C_PAL;

  totPal = numPals;
  for i = 1 to numPals;
    select;
      when pals(i).pstat = 'O';
        openCt += 1;
      when pals(i).pstat = 'C';
        closedCt += 1;
    endsl;
    if pals(i).phaz = 'Y';
      hazCt += 1;
    endif;
    totVol += pals(i).plen * pals(i).pwid * pals(i).phgt / 1728;  // cu ft
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// loadSubfile
//////////////////////////////////////////////////////////////////////
begsr loadSubfile;
  rrn = 0;
  sflclear = '1';
  write palctl;
  sflclear = '0';
  for i = 1 to numPals;
    psrl     = pals(i).psrl;
    pshto    = pals(i).pshto;
    pshtonm  = pals(i).pshtonm;
    pstat    = pals(i).pstat;
    pstatdsc = palStatusDesc(pals(i).pstat);
    pbol     = pals(i).pbol;
    pinvc    = pals(i).pinvc;
    phaz     = pals(i).phaz;
    plen     = pals(i).plen;
    pwid     = pals(i).pwid;
    phgt     = pals(i).phgt;
    pvol     = pals(i).plen * pals(i).pwid * pals(i).phgt / 1728;
    pempl    = pals(i).pempl;
    ptstp    = fmtTs(pals(i).ptstp);
    rrn += 1;
    write palsfl;
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// palStatusDesc -- O = Open, C = Closed
//////////////////////////////////////////////////////////////////////
dcl-proc palStatusDesc;
  dcl-pi *n char(12);
    stat char(1) const;
  end-pi;
  select;
    when stat = 'O';
      return 'Open';
    when stat = 'C';
      return 'Closed';
    other;
      return stat;
  endsl;
end-proc;

//////////////////////////////////////////////////////////////////////
// fmtTs -- YYYY-MM-DD HH:MM:SS or '' for *LOVAL
//////////////////////////////////////////////////////////////////////
dcl-proc fmtTs;
  dcl-pi *n char(19);
    ts timestamp const;
  end-pi;
  dcl-s s char(26);
  if ts = z'0001-01-01-00.00.00.000000';
    return '';
  endif;
  s = %char(ts : *iso);
  return %subst(s:1:10) + ' ' +
         %subst(s:12:2) + ':' + %subst(s:15:2) + ':' + %subst(s:18:2);
end-proc;
