**free

/////////////////////
// Shipment Lot Inquiry (EJS Screen Mode)
//
// Hornady-branded EJS clone of HYR0606 option 3.  Single-page lot
// inquiry over HYPSGLD with a group-number selector + optional item
// filter.
//
// Actions: SEARCH / CLEAR / EXIT.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt option(*srcstmt : *nodebugio);

dcl-f hyr0606eo workstn sfile(lotsfl : rrn) handler('PROFOUNDUI(HANDLER)');

dcl-ds lot_t qualified template;
  lgrp  zoned(9 : 0);
  lturn zoned(9 : 0);
  ltseq zoned(3 : 0);
  litem char(15);
  llot  char(10);
  lqty  zoned(9 : 0);
  lsscc zoned(20 : 0);
end-ds;

dcl-ds lots likeds(lot_t) dim(9999);
dcl-ds lRow likeds(lot_t);
dcl-s numLots int(10);

dcl-s shipName char(30);
dcl-s curName  char(30);
dcl-s rrn      int(10);
dcl-s i        int(10);
dcl-s curGroup zoned(9 : 0);
dcl-s curItem  char(15);
dcl-s distinctItems int(10);
dcl-s prevItem char(15);
dcl-s lookupGrp zoned(9 : 0);

dow action <> 'EXIT';

  msg = '';
  msgType = '';

  if action = 'CLEAR';
    curGroup = 0;
    curItem  = '';
  elseif action = 'SEARCH';
    curGroup = fGroup;
    curItem  = fItem;
  elseif action = '';
    // first entry -- pre-seed with group 1001 from the original demo path
    curGroup = 1001;
    curItem  = '';
  endif;

  exsr loadLots;

  if numLots = 0 and msg = '';
    if curGroup = 0;
      msg = 'Enter a shipment group # and tap Apply to see its lots.';
      msgType = 'info';
    else;
      msg = 'No lots found for group ' + %char(curGroup) + '.';
      msgType = 'warn';
    endif;
  endif;

  // ship-to name for the selected group (best-effort)
  shipName = '';
  if curGroup > 0;
    exec sql
      SELECT GCSTNM INTO :shipName
        FROM HYPSGCU WHERE GCGRP# = :curGroup
       FETCH FIRST ROW ONLY;
    if sqlcode <> 0;
      shipName = '';
    endif;
  endif;

  exsr loadSubfile;

  fGroup = curGroup;
  fItem  = curItem;
  action = '';
  exfmt lotctl;

enddo;

*inlr = *on;

//////////////////////////////////////////////////////////////////////
// loadLots
//////////////////////////////////////////////////////////////////////
begsr loadLots;
  numLots       = 0;
  totLots       = 0;
  totQty        = 0;
  distinctItems = 0;
  prevItem      = '';

  if curGroup = 0 and %trim(curItem) = '';
    return;
  endif;

  exec sql
    DECLARE C_LOT CURSOR FOR
      SELECT LDGRP#, LDTURN, LDTSEQ, LDITEM, LDLOT#, LDQSTC, LDSSCC
        FROM HYPSGLD
       WHERE (:curGroup = 0 OR LDGRP# = :curGroup)
         AND (:curItem = '' OR LDITEM = :curItem)
       ORDER BY LDGRP#, LDTURN, LDTSEQ, LDITEM, LDLOT#;
  exec sql OPEN C_LOT;

  dow numLots < %elem(lots);
    exec sql FETCH FROM C_LOT INTO :lRow;
    if sqlcode <> 0;
      leave;
    endif;
    numLots += 1;
    lots(numLots) = lRow;
    if lRow.litem <> prevItem;
      distinctItems += 1;
      prevItem = lRow.litem;
    endif;
  enddo;
  exec sql CLOSE C_LOT;

  totLots = numLots;
  distct  = distinctItems;
  for i = 1 to numLots;
    totQty += lots(i).lqty;
  endfor;
endsr;

//////////////////////////////////////////////////////////////////////
// loadSubfile
//////////////////////////////////////////////////////////////////////
begsr loadSubfile;
  rrn = 0;
  sflclear = '1';
  write lotctl;
  sflclear = '0';
  for i = 1 to numLots;
    lgrp  = lots(i).lgrp;
    lturn = lots(i).lturn;
    ltseq = lots(i).ltseq;
    litem = lots(i).litem;
    llot  = lots(i).llot;
    lqty  = lots(i).lqty;
    lsscc = lots(i).lsscc;
    // Resolve ship-to name once per fetch loop iteration -- could be
    // expensive on huge groups but the dataset is small.
    curName   = '';
    lookupGrp = lots(i).lgrp;
    exec sql
      SELECT GCSTNM INTO :curName
        FROM HYPSGCU WHERE GCGRP# = :lookupGrp
       FETCH FIRST ROW ONLY;
    lname = curName;
    rrn += 1;
    write lotsfl;
  endfor;
endsr;
