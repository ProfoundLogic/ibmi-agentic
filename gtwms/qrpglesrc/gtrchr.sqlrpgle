**free

//////////////////////////////////////////////////////////////////////////
// GTRCHR -- Receiving, home screen.
//
// "Scan something on the pallet coming in, show all the items in that
// receipt." This is the something-on-the-pallet half.
//
// The pallet label is a GS1-128 carrying an SSCC-18 plus, usually, GTIN, lot
// and expiry. GTBAR parses all of it from the one scan, which is the strongest
// technical beat in the receiving demo and is pure RPG string handling -- no
// library, no service, no middleware.
//
// A receipt can be reached three ways, in this order of preference:
//   1. scan the pallet label      -- SSCC matched against GTRCPTHDR
//   2. scan or key a receipt/PO   -- for a label that has peeled off
//   3. tap a receipt in the list  -- what a supervisor actually does
//
// ONE FORMAT PER DISPLAY FILE. The line list is GTRCLR with its own display
// file, called rather than chained so exiting it lands back here.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

dcl-f gtrchd workstn sfile(rcpsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pr gtrclr extpgm;
  receipt char(10) const;
  lang    char(2);
end-pr;

dcl-ds r likeds(gtbar_result_t);

dcl-c MAX_ROWS 40;

dcl-s rrn      int(10) inz(0);
dcl-s i        int(10);
dcl-s done     ind inz(*off);
dcl-s payload  varchar(120);
dcl-s curRcpt  char(10);

// The open-receipt list is held between round trips so a tap can be resolved
// to a receipt id without re-querying, and so the subfile can be rebuilt
// after returning from the line screen.
dcl-ds rcpts qualified dim(MAX_ROWS);
  rcpt   char(10);
  po     char(12);
  vend   varchar(40);
  door   char(4);
  stat   char(4);
  sscc   char(18);
  lines  int(10);
  donen  int(10);
  varn   int(10);
  qtye   packed(11 : 2);
  qtyr   packed(11 : 2);
end-ds;
dcl-s rcptCount int(10) inz(0);

// Work fields. RPG rejects dcl-s inside a begsr in the main procedure
// (RNF0724), so everything the subroutines need lives here.
dcl-s wRcpt  char(10);
dcl-s wPo    char(12);
dcl-s wVend  varchar(40);
dcl-s wDoor  char(4);
dcl-s wStat  char(4);
dcl-s wSscc  char(18);
dcl-s wLines int(10);
dcl-s wDone  int(10);
dcl-s wVar   int(10);
dcl-s wQtye  packed(11 : 2);
dcl-s wQtyr  packed(11 : 2);
dcl-s wKey   char(20);

//------------------------------------------------------------------------
// Main
//------------------------------------------------------------------------
exsr loadOperator;

dow not done;
  exsr loadRcptSubfile;
  action  = *blanks;
  exfmt rcvhome;
  msg = *blanks;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'LANGEN';
      langpref = 'EN';
      exsr saveLanguage;

    when action = 'LANGFR';
      langpref = 'FR';
      exsr saveLanguage;

    when action = 'SCAN';
      exsr doScan;

    when %subst(action : 1 : 4) = 'PICK';
      // PICKnn -- the operator tapped a receipt card.
      i = %int(%subst(action : 5));
      if i >= 1 and i <= rcptCount;
        curRcpt = rcpts(i).rcpt;
        exsr openReceipt;
      endif;

    when action = *blanks;
      // Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// The scan. GTBAR classifies whatever arrived, then we decide what it means
// HERE -- the service program identifies barcodes, it does not know about
// receipts.
//------------------------------------------------------------------------
begsr doScan;
  payload = %trim(scanval);

  if payload = *blanks;
    exsr msgNothingScanned;
    leavesr;
  endif;

  clear r;
  gtbar_parse(payload : r);

  // An SSCC is the pallet label: the receipt that pallet belongs to.
  // GTBAR returns the SSCC either as the whole payload (plain SSCC-18) or
  // parsed out of AI 00 in a GS1-128, and either way it lands in r.sscc.
  curRcpt = *blanks;

  // GTBAR resolves a receipt label itself, so take that when it did.
  if r.receiptId <> *blanks;
    curRcpt = r.receiptId;
    scanval = *blanks;
    exsr openReceipt;
    leavesr;
  endif;

  if r.sscc <> *blanks;
    exec sql
      select receipt_id into :curRcpt
        from gtrcpthdr
       where sscc = :r.sscc
       fetch first row only;
    if sqlcode <> 0;
      curRcpt = *blanks;
      exsr msgUnknownPallet;
      leavesr;
    endif;
  else;
    // Not a pallet label. Try it as a receipt id, then as a PO -- a receiver
    // whose label has peeled off keys the paperwork instead.
    wKey = %upper(%trim(payload));

    exec sql
      select receipt_id into :curRcpt
        from gtrcpthdr
       where receipt_id = :wKey
       fetch first row only;

    if sqlcode <> 0;
      exec sql
        select receipt_id into :curRcpt
          from gtrcpthdr
         where po_number = :wKey
         order by expected_ts desc
         fetch first row only;
    endif;

    if sqlcode <> 0;
      curRcpt = *blanks;
      exsr msgUnknownLabel;
      leavesr;
    endif;
  endif;

  scanval = *blanks;
  exsr openReceipt;
endsr;

//------------------------------------------------------------------------
// Hand off to the line list. A posted receipt still opens -- read-only is
// GTRCLR's decision, not ours; refusing here would leave a receiver unable
// to look at what they just posted.
//------------------------------------------------------------------------
begsr openReceipt;
  if curRcpt = *blanks;
    leavesr;
  endif;
  gtrclr(curRcpt : langpref);
endsr;

//------------------------------------------------------------------------
// Receipts open today. GTVRCVOPEN already excludes posted ones, so the list
// and the count cannot disagree.
//------------------------------------------------------------------------
begsr loadRcptSubfile;
  // The CLEAR writes the RECORD FORMAT, not the subfile record. Writing the
  // subfile with rrn still 0 gives CPF5021 "incorrect relative record
  // number" -- an inquiry message on the operator's screen, not a
  // compile error.
  rrn = 0;
  sflclear = '1';
  write rcvhome;
  sflclear = '0';
  rcptCount = 0;

  exec sql
    declare rcpCsr cursor for
      select receipt_id, po_number, vendor_name, door_id, receipt_status,
             sscc, line_count, lines_done, lines_variance,
             qty_expected, qty_received
        from gtvrcvopen
       order by expected_ts asc
       fetch first 40 rows only;

  exec sql open rcpCsr;

  dow rcptCount < MAX_ROWS;
    exec sql
      fetch rcpCsr into :wRcpt, :wPo, :wVend, :wDoor, :wStat,
                        :wSscc, :wLines, :wDone, :wVar, :wQtye, :wQtyr;
    if sqlcode <> 0;
      leave;
    endif;

    rcptCount += 1;
    rcpts(rcptCount).rcpt  = wRcpt;
    rcpts(rcptCount).po    = wPo;
    rcpts(rcptCount).vend  = wVend;
    rcpts(rcptCount).door  = wDoor;
    rcpts(rcptCount).stat  = wStat;
    rcpts(rcptCount).sscc  = wSscc;
    rcpts(rcptCount).lines = wLines;
    rcpts(rcptCount).donen = wDone;
    rcpts(rcptCount).varn  = wVar;
    rcpts(rcptCount).qtye  = wQtye;
    rcpts(rcptCount).qtyr  = wQtyr;

    rrn    += 1;
    rseq    = rcptCount;
    rrcpt   = wRcpt;
    rpo     = wPo;
    rvend   = wVend;
    rdoor   = wDoor;
    rstat   = wStat;
    rsscc   = wSscc;
    rlines  = wLines;
    rdone   = wDone;
    rvar    = wVar;
    rqtye   = wQtye;
    rqtyr   = wQtyr;
    write rcpsfl;
  enddo;

  exec sql close rcpCsr;

  nopen = rcptCount;
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, lang_pref
      into :opername, :langpref
      from gtoperator
     where operator_id = 'GT001' and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    opername = 'Demo Operator';
    langpref = 'EN';
  endif;
endsr;

//------------------------------------------------------------------------
begsr saveLanguage;
  exec sql
    update gtoperator set lang_pref = :langpref
     where operator_id = 'GT001';
  if sqlcode < 0;
    exec sql rollback;
  else;
    exec sql commit;
  endif;
endsr;

//------------------------------------------------------------------------
// Messages. Bilingual, and specific about what actually happened -- "not
// found" is useless to somebody holding a pallet.
//------------------------------------------------------------------------
begsr msgNothingScanned;
  if langpref = 'FR';
    msg = 'Scannez l''étiquette de la palette ou saisissez un numéro.';
  else;
    msg = 'Scan the pallet label, or key a receipt or PO number.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUnknownPallet;
  if langpref = 'FR';
    msg = 'Palette ' + %trim(r.sscc) + ' : aucune réception connue.';
  else;
    msg = 'Pallet ' + %trim(r.sscc) + ' is not on any known receipt.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgUnknownLabel;
  if langpref = 'FR';
    msg = 'Aucune réception pour ' + %trim(payload) + '.';
  else;
    msg = 'No receipt found for ' + %trim(payload) + '.';
  endif;
endsr;
