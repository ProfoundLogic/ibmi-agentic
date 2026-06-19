**free

/////////////////////
// Shipment Processing (EJS Screen Mode)
//
// Hornady-branded EJS clone of HYR0600 option 1. Skips the original
// program's "Employee Number Prompt" loop and renders the open
// shipment list as a single-page dashboard over HYPSGCU joined with
// HYPSGHD for status + DC location.
//
// Actions:
//   SEARCH / CLEAR / EXIT  -- filter controls
//   VIEW / CLOSE_VIEW      -- drill into shipment detail (HYPSGDT)
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt option(*srcstmt : *nodebugio);

dcl-f hyr0600eo workstn sfile(shipsfl : rrn) handler('PROFOUNDUI(HANDLER)');

dcl-ds ship_t qualified template;
  grp    zoned(9 : 0);
  shto   zoned(7 : 0);
  name   char(30);
  shpv   char(2);
  shvds  char(30);
  stat   char(1);
  loc    char(4);
  items  zoned(7 : 0);
  weight zoned(11 : 2);
  tstp2  timestamp;
end-ds;

dcl-ds ships likeds(ship_t) dim(9999);
dcl-ds sRow likeds(ship_t);
dcl-s numShips int(10);

dcl-ds det_t qualified template;
  item   char(15);
  desc   char(25);
  ord    zoned(8 : 0);
  ordl   zoned(4 : 0);
  lot    char(10);
  qopen  zoned(9 : 0);
  qpack  zoned(9 : 0);
  qstoc  zoned(9 : 0);
  bln    zoned(5 : 0);
  ploc   char(14);
end-ds;
dcl-ds detRow likeds(det_t);
dcl-s numDet int(10);

dcl-s rrn       int(10);
dcl-s i         int(10);
dcl-s curStatus char(1);
dcl-s curLoc    char(4);
dcl-s curFilter char(30);

dow action <> 'EXIT';

  exsr processAction;
  exsr loadShipments;

  if numShips = 0 and msg = '';
    msg = 'No shipments match the current filter.';
    msgType = 'info';
  endif;

  exsr loadSubfile;

  // Detail-modal handling
  if action = 'VIEW' and selGrp > 0;
    viewGrp = selGrp;
    exsr loadDetail;
  elseif action = 'CLOSE_VIEW';
    viewGrp = 0;
    exsr clearDetail;
  elseif viewGrp > 0;
    exsr loadDetail;
  else;
    exsr clearDetail;
  endif;

  ffilter = curFilter;
  fstatus = curStatus;
  floc    = curLoc;
  action  = '';
  selGrp  = 0;
  exfmt shipctl;

  if action = 'CLEAR';
    curFilter = '';
    curStatus = '';
    curLoc    = '';
  else;
    curFilter = ffilter;
    curStatus = fstatus;
    curLoc    = floc;
  endif;

enddo;

*inlr = *on;

//////////////////////////////////////////////////////////////////////
// processAction -- filter actions are passive; VIEW/CLOSE_VIEW handled
// in the mainline.
//////////////////////////////////////////////////////////////////////
begsr processAction;
  msg = '';
  msgType = '';
  // no per-row mutate actions yet -- screen is read-only
endsr;

//////////////////////////////////////////////////////////////////////
// loadShipments -- cursor over HYPSGCU + HYPSGHD with filters.
//
// "Open" status is GCTSTP2 = *LOVAL (HYR0600's own convention).
//////////////////////////////////////////////////////////////////////
begsr loadShipments;
  numShips = 0;
  totShips  = 0;
  totItems  = 0;
  totWeight = 0;
  openCt    = 0;
  completed = 0;
  westCt    = 0;
  aldaCt    = 0;
  giCt      = 0;

  exec sql
    DECLARE C_SHIP CURSOR FOR
      SELECT C.GCGRP#, C.GCSHTO, C.GCSTNM, C.GCSHPV, C.GCSVDS,
             COALESCE(H.GHSTS, ''),
             COALESCE(H.GHDCLOC, ''),
             C.GCITCT, C.GCSHWT, C.GCTSTP2
        FROM HYPSGCU C
        LEFT JOIN HYPSGHD H ON H.GHGRP# = C.GCGRP#
       WHERE (:curLoc = '' OR H.GHDCLOC = :curLoc)
         AND (:curStatus = '' OR
              (:curStatus = 'O' AND C.GCTSTP2 = '0001-01-01-00.00.00.000000') OR
              (:curStatus = 'C' AND C.GCTSTP2 <> '0001-01-01-00.00.00.000000'))
         AND (:curFilter = '' OR
              CHAR(C.GCGRP#) LIKE '%' CONCAT RTRIM(:curFilter) CONCAT '%' OR
              UPPER(C.GCSTNM) LIKE '%' CONCAT UPPER(RTRIM(:curFilter)) CONCAT '%' OR
              UPPER(C.GCSVDS) LIKE '%' CONCAT UPPER(RTRIM(:curFilter)) CONCAT '%')
       ORDER BY C.GCGRP#;

  exec sql OPEN C_SHIP;

  dow numShips < %elem(ships);
    exec sql FETCH FROM C_SHIP INTO :sRow;
    if sqlcode <> 0;
      leave;
    endif;
    numShips += 1;
    ships(numShips) = sRow;
  enddo;

  exec sql CLOSE C_SHIP;

  totShips = numShips;
  for i = 1 to numShips;
    totItems  += ships(i).items;
    totWeight += ships(i).weight;
    if ships(i).tstp2 = z'0001-01-01-00.00.00.000000';
      openCt += 1;
    else;
      completed += 1;
    endif;
    select;
      when ships(i).loc = 'WEST';
        westCt += 1;
      when ships(i).loc = 'ALDA';
        aldaCt += 1;
      when ships(i).loc = 'GI';
        giCt += 1;
    endsl;
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// loadSubfile -- push ships[] to SHIPSFL.
//////////////////////////////////////////////////////////////////////
begsr loadSubfile;
  rrn = 0;
  sflclear = '1';
  write shipctl;
  sflclear = '0';
  for i = 1 to numShips;
    sgrp      = ships(i).grp;
    sshto     = ships(i).shto;
    sname     = ships(i).name;
    sshpv     = ships(i).shpv;
    sshvds    = ships(i).shvds;
    if ships(i).tstp2 = z'0001-01-01-00.00.00.000000';
      sstat     = 'O';
      sstatdesc = 'Open';
    else;
      sstat     = 'C';
      sstatdesc = 'Completed';
    endif;
    sloc      = ships(i).loc;
    sitems    = ships(i).items;
    sweight   = ships(i).weight;
    rrn += 1;
    write shipsfl;
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// loadDetail -- header data for the modal + a JSON array of detail
// lines retrieved from HYPSGDT.
//////////////////////////////////////////////////////////////////////
begsr loadDetail;
  viewShip = '';
  viewBill = '';
  viewVia  = '';
  viewStat = '';
  viewLoc  = '';
  viewZip  = '';
  viewWeight = 0;
  viewItems  = 0;

  exec sql
    SELECT C.GCSTNM, C.GCBTNM, C.GCSVDS, C.GCSTZP,
           C.GCSHWT, C.GCITCT,
           COALESCE(H.GHDCLOC, ''), C.GCTSTP2
      INTO :viewShip, :viewBill, :viewVia, :viewZip,
           :viewWeight, :viewItems, :viewLoc, :sRow.tstp2
      FROM HYPSGCU C
      LEFT JOIN HYPSGHD H ON H.GHGRP# = C.GCGRP#
     WHERE C.GCGRP# = :viewGrp;
  if sqlcode = 0;
    if sRow.tstp2 = z'0001-01-01-00.00.00.000000';
      viewStat = 'Open';
    else;
      viewStat = 'Completed';
    endif;
  endif;

  numDet = 0;
  exec sql
    DECLARE C_DET CURSOR FOR
      SELECT GDITEM, GDIMDS, GDORD#, GDORL#, GDLOT,
             GDQOPN, GDQOPK, GDQSTC, GDBLN#, GDPLOC
        FROM HYPSGDT
       WHERE GDGRP# = :viewGrp
       ORDER BY GDORD#, GDORL#, GDRSEQ;
  exec sql OPEN C_DET;

  detLines = '[';
  dow numDet < 100;
    exec sql FETCH FROM C_DET INTO :detRow;
    if sqlcode <> 0;
      leave;
    endif;
    if numDet > 0;
      detLines = %trimr(detLines) + ',';
    endif;
    numDet += 1;
    detLines = %trimr(detLines) +
               '{"item":"'   + %trimr(detRow.item)  + '"' +
               ',"desc":"'  + %trimr(detRow.desc)  + '"' +
               ',"ord":'    + %char(detRow.ord)    +
               ',"ordl":'   + %char(detRow.ordl)   +
               ',"lot":"'   + %trimr(detRow.lot)   + '"' +
               ',"qopen":'  + %char(detRow.qopen)  +
               ',"qpack":'  + %char(detRow.qpack)  +
               ',"qstoc":'  + %char(detRow.qstoc)  +
               ',"bin":'    + %char(detRow.bln)    +
               ',"ploc":"'  + %trimr(detRow.ploc)  + '"}';
  enddo;
  detLines = %trimr(detLines) + ']';
  exec sql CLOSE C_DET;
endsr;

//////////////////////////////////////////////////////////////////////
// clearDetail
//////////////////////////////////////////////////////////////////////
begsr clearDetail;
  detLines = '[]';
  viewShip = '';
  viewBill = '';
  viewVia  = '';
  viewStat = '';
  viewLoc  = '';
  viewZip  = '';
  viewWeight = 0;
  viewItems  = 0;
endsr;
