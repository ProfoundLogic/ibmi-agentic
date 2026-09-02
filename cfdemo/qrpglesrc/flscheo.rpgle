**free

/////////////////////
// Production schedule board (EJS)                               GJA-917
//
// Demo A4 from the onsite plan: the visual replacement for the physical
// sticky-note schedule. Machine builds by work centre, shop load against
// capacity, material shortages, and promised against projected ship.
//
// The data-confidence panel is not decoration. Fletcher told us they are
// "getting into actually using the data in the system to plug into
// scheduling applications and finding a lot of database issues", so a board
// that presented these dates as reliable would be the wrong answer
// confidently delivered. It says what it cannot stand behind, and why.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flscheo workstn sfile(wosfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pr flsch1eo extpgm;
  wo packed(8:0) const;
end-pr;

dcl-pr flschdeo extpgm;
end-pr;

dcl-ds shop likeds(fl_shop_t);
dcl-ds lanes likeds(fl_wctr_t) dim(50);
dcl-ds orders likeds(fl_wo_t) dim(300);
dcl-s numLanes int(10) inz(0);
dcl-s numOrders int(10) inz(0);
dcl-s wantView char(1) inz('A');
dcl-s wantWctr char(6) inz(*blanks);
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s selrrn int(10);
// Lane payload. Sized well over the char(900) screen field: appending past a
// VARCHAR's declared length is RNX0100, and the truncation on the way out is
// explicit and counted rather than silent.
dcl-s lanesBuf varchar(2000);
dcl-s one varchar(300);
dcl-s lanesShown int(10) inz(0);

dow action <> 'EXIT';

  exsr loadBoard;

  if numOrders > 0;
    soptdesc = 'Double-click a work order to open it, or type 5';
  else;
    soptdesc = '';
  endif;

  if numOrders > 0;
    exsr loadSFL;
  else;
    exsr clearSFL;
  endif;

  sview = wantView;
  swctr = wantWctr;
  action = '';
  exfmt schctl;

  select;
    when action = 'EXIT';
      leave;
    when action = 'DQ';
      flschdeo();
      iter;
  endsl;

  // The view dropdown and the work centre lanes both submit by changing a
  // field, so one comparison covers both.
  if sview <> wantView or swctr <> wantWctr;
    wantView = sview;
    wantWctr = swctr;
    iter;
  endif;

  if numOrders > 0;
    selrrn = 0;
    sopt = '';
    readc wosfl;
    dow not %eof(flscheo);
      if sopt <> '' and selrrn = 0;
        selrrn = rrn;
      endif;
      update wosfl;
      readc wosfl;
    enddo;
    if selrrn <> 0;
      chain selrrn wosfl;
      if %trim(sopt) = '5';
        flsch1eo(swo);
      else;
        msg = 'Option ' + %trim(sopt) + ' is not valid. Use 5 to open the '
            + 'work order.';
      endif;
    endif;
  endif;

enddo;

*inlr = *on;

begsr loadBoard;
  err = fl_shopSummary(shop);
  if err <> '';
    msg = err;
    leavesr;
  endif;
  sorders  = shop.orders;
  swip     = shop.wip;
  slate    = shop.late;
  srisk    = shop.risk;
  sontrk   = shop.ontrack;
  shold    = shop.hold;
  sshort   = shop.shorts;
  sshortv  = shop.shortval;
  svalue   = shop.value;
  sremhrs  = shop.remhrs;
  scapday  = shop.capday;
  sweeks   = shop.weeks;
  sdqfind  = shop.dqfind;
  sdqord   = shop.dqorders;
  sdqhigh  = shop.dqhigh;
  sdqtrust = shop.dqtrust;
  sdqpct   = shop.dqpct;

  err = fl_listWorkCentres(lanes : %elem(lanes) : numLanes);
  if err <> '';
    msg = err;
    leavesr;
  endif;
  exsr buildLanes;

  err = fl_listWorkOrders(wantView : wantWctr : orders : %elem(orders)
                        : numOrders);
  if err <> '';
    msg = err;
  endif;
endsr;

// The lane strip travels as one JSON string, because two subfiles in one RDF
// format compile to malformed DDS. If the payload will not fit the field, the
// lanes that were dropped are counted and said out loud - a silently short
// strip reads as "that is all the work centres there are".
begsr buildLanes;
  lanesBuf = '[';
  lanesShown = 0;
  for i = 1 to numLanes;
    one = '{"c":' + fl_jsonStr(%trim(lanes(i).code))
        + ',"d":' + fl_jsonStr(%trim(lanes(i).descr))
        + ',"q":' + fl_jsonNum(%char(lanes(i).queued))
        + ',"w":' + fl_jsonNum(%char(lanes(i).wip))
        + ',"h":' + fl_jsonNum(%char(lanes(i).loadhrs))
        + ',"p":' + fl_jsonNum(%char(lanes(i).loadpct))
        + ',"k":' + fl_jsonNum(%char(lanes(i).weeks))
        + ',"l":' + fl_jsonNum(%char(lanes(i).late))
        + ',"s":' + fl_jsonStr(%trim(lanes(i).status)) + '}';
    if %len(lanesBuf) + %len(one) + 2 > 1380;
      leave;
    endif;
    if lanesShown > 0;
      lanesBuf += ',';
    endif;
    lanesBuf += one;
    lanesShown += 1;
  endfor;
  lanesBuf += ']';
  slanes = lanesBuf;
  if lanesShown < numLanes;
    msg = 'Showing ' + %char(lanesShown) + ' of ' + %char(numLanes)
        + ' work centres - the rest did not fit the lane strip.';
  endif;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write schctl;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numOrders;
    sopt    = '';
    swo     = orders(i).wo;
    sser    = orders(i).serial;
    smodel  = orders(i).model;
    stype   = orders(i).otyped;
    scust   = orders(i).custname;
    sdesc   = orders(i).descr;
    sstat   = orders(i).statusd;
    sprio   = orders(i).priod;
    swcc    = orders(i).wctr;
    spctr   = orders(i).pctroute;
    sprom   = orders(i).promised;
    sproj   = orders(i).projectd;
    sslip   = orders(i).slipdays;
    spromd  = orders(i).promdays;
    sprojd  = orders(i).projdays;
    sshortc = orders(i).shorts;
    sdqh    = orders(i).dqhigh;
    sflag   = orders(i).flag;
    snote   = orders(i).note;
    rrn += 1;
    write wosfl;
  endfor;
endsr;
