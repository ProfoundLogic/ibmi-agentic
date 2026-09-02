**free

/////////////////////
// Schedule data confidence (EJS)                                GJA-917
//
// Every finding behind the confidence percentage on the board: what is
// wrong, which work order it is on, and what it blocks. Option 5 opens the
// work order, so a finding is one keystroke from the record that has it.
//
// The findings are computed on every entry rather than stored, so fixing a
// record makes the finding disappear on the next look. A stored list of
// known problems is a list that goes stale and then lies.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flschdeo workstn sfile(dqsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pr flsch1eo extpgm;
  wo packed(8:0) const;
end-pr;

dcl-ds shop likeds(fl_shop_t);
dcl-ds findings likeds(fl_dq_t) dim(400);
dcl-s numFind int(10) inz(0);
dcl-s shown int(10) inz(0);
dcl-s wantSev char(1) inz('*');
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s j int(10);
dcl-s selrrn int(10);
// Category rollup, one JSON payload. Sized over the char(900) field so the
// append cannot overflow the VARCHAR; the fit is checked explicitly.
dcl-s catBuf varchar(2000);
dcl-s one varchar(300);
dcl-s catShown int(10) inz(0);
dcl-s catName char(26) dim(60);
dcl-s catSev char(1) dim(60);
dcl-s catCount int(10) dim(60);
dcl-s numCats int(10) inz(0);
dcl-s hit ind;

dow action <> 'EXIT';

  exsr loadFindings;

  if shown > 0;
    soptdesc = 'Double-click a finding to open the work order it is on, or type 5';
    exsr loadSFL;
  else;
    soptdesc = '';
    exsr clearSFL;
  endif;

  ssev = wantSev;
  action = '';
  exfmt dqctl;

  if action = 'EXIT';
    leave;
  endif;

  if ssev <> wantSev;
    wantSev = ssev;
    iter;
  endif;

  if shown > 0;
    selrrn = 0;
    sopt = '';
    readc dqsfl;
    dow not %eof(flschdeo);
      if sopt <> '' and selrrn = 0;
        selrrn = rrn;
      endif;
      update dqsfl;
      readc dqsfl;
    enddo;
    if selrrn <> 0;
      chain selrrn dqsfl;
      if %trim(sopt) = '5';
        flsch1eo(sdwo);
      else;
        msg = 'Option ' + %trim(sopt) + ' is not valid. Use 5 to open the '
            + 'work order.';
      endif;
    endif;
  endif;

enddo;

*inlr = *on;

begsr loadFindings;
  err = fl_shopSummary(shop);
  if err <> '';
    msg = err;
    leavesr;
  endif;
  sfind  = shop.dqfind;
  sord   = shop.dqorders;
  shigh  = shop.dqhigh;
  smed   = shop.dqfind - shop.dqhigh;
  strust = shop.dqtrust;
  stotal = shop.orders;
  spct   = shop.dqpct;

  err = fl_scanSchedule(findings : %elem(findings) : numFind);
  if err <> '';
    msg = err;
    leavesr;
  endif;

  shown = 0;
  for i = 1 to numFind;
    if wantSev = '*' or findings(i).sev = wantSev;
      shown += 1;
    endif;
  endfor;

  exsr buildCats;
endsr;

// Findings per category, in the order the scan produced them - which is
// severity first, so the categories that matter lead the strip.
begsr buildCats;
  numCats = 0;
  for i = 1 to numFind;
    hit = *off;
    for j = 1 to numCats;
      if catName(j) = findings(i).cat;
        catCount(j) += 1;
        hit = *on;
        leave;
      endif;
    endfor;
    if not hit and numCats < %elem(catName);
      numCats += 1;
      catName(numCats) = findings(i).cat;
      catSev(numCats) = findings(i).sev;
      catCount(numCats) = 1;
    endif;
  endfor;

  catBuf = '[';
  catShown = 0;
  for i = 1 to numCats;
    one = '{"c":' + fl_jsonStr(%trim(catName(i)))
        + ',"s":' + fl_jsonStr(%trim(catSev(i)))
        + ',"n":' + fl_jsonNum(%char(catCount(i))) + '}';
    if %len(catBuf) + %len(one) + 2 > 880;
      leave;
    endif;
    if catShown > 0;
      catBuf += ',';
    endif;
    catBuf += one;
    catShown += 1;
  endfor;
  catBuf += ']';
  scats = catBuf;
  if catShown < numCats;
    msg = 'Showing ' + %char(catShown) + ' of ' + %char(numCats)
        + ' categories in the summary strip.';
  endif;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write dqctl;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numFind;
    if wantSev <> '*' and findings(i).sev <> wantSev;
      iter;
    endif;
    sopt  = '';
    ssevc = findings(i).sev;
    ssevd = findings(i).sevd;
    scat  = findings(i).cat;
    sdwo  = findings(i).wo;
    ssubj = findings(i).subject;
    sfnd  = findings(i).finding;
    simp  = findings(i).impact;
    rrn += 1;
    write dqsfl;
  endfor;
endsr;
