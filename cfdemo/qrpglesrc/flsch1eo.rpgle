**free

/////////////////////
// Work order build detail (EJS)                                 GJA-917
//
// One machine build: what it is, who it is for, where it is on the floor,
// and the routing operation by operation with the material that is holding
// each one up. Read-only - the board is an inquiry, not a maintenance
// screen.
//
// The confidence findings for THIS order are listed here rather than only
// counted. "1 blocking finding" tells a scheduler nothing they can act on.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flsch1eo workstn sfile(opsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pi *n;
  p_wo packed(8:0) const;
end-pi;

dcl-ds order likeds(fl_wo_t);
dcl-ds ops likeds(fl_op_t) dim(100);
dcl-ds findings likeds(fl_dq_t) dim(400);
dcl-s numOps int(10) inz(0);
dcl-s numFind int(10) inz(0);
dcl-s found ind;
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s dqBuf varchar(2000);
dcl-s one varchar(400);
dcl-s dqShown int(10) inz(0);

dow action <> 'EXIT';

  exsr loadOrder;
  if not found;
    msg = 'Work order ' + %char(p_wo) + ' not found.';
    leave;
  endif;

  if numOps > 0;
    soptdesc = 'Rows shaded red are waiting on material';
    exsr loadSFL;
  else;
    soptdesc = '';
    exsr clearSFL;
  endif;

  action = '';
  exfmt wodtl;

  if action = 'EXIT';
    leave;
  endif;

enddo;

*inlr = *on;

begsr loadOrder;
  err = fl_getWorkOrder(p_wo : order : found);
  if err <> '';
    msg = err;
    leavesr;
  endif;
  if not found;
    leavesr;
  endif;

  swo      = order.wo;
  sser     = order.serial;
  smodel   = order.model;
  smodeld  = order.modeldsc;
  scustno  = order.custno;
  scust    = order.custname;
  ssite    = order.sitename;
  stype    = order.otyped;
  sstat    = order.statusd;
  sprio    = order.priod;
  sopened  = order.opened;
  sprom    = order.promised;
  sstarted = order.started;
  scompl   = order.compl;
  sproj    = order.projectd;
  sslip    = order.slipdays;
  spromd   = order.promdays;
  sprojd   = order.projdays;
  swcc     = order.wctr;
  swcd     = order.wctrd;
  spctrptd = order.pctrptd;
  spctr    = order.pctroute;
  sstdhrs  = order.stdhrs;
  sacthrs  = order.acthrs;
  sremhrs  = order.remhrs;
  sops     = order.ops;
  sopsdone = order.opsdone;
  sshortc  = order.shorts;
  sshortl  = order.shortlat;
  svalue   = order.value;
  sdesc    = order.descr;
  snote    = order.note;
  sflag    = order.flag;
  sdqc     = order.dqcount;
  sdqh     = order.dqhigh;

  err = fl_listOperations(p_wo : ops : %elem(ops) : numOps);
  if err <> '';
    msg = err;
    leavesr;
  endif;

  exsr buildDq;
endsr;

// The findings for this order, as a JSON array of strings. Same reasoning as
// the board's lane strip: a second list cannot be a second subfile.
begsr buildDq;
  err = fl_scanSchedule(findings : %elem(findings) : numFind);
  if err <> '';
    msg = err;
    leavesr;
  endif;
  dqBuf = '[';
  dqShown = 0;
  for i = 1 to numFind;
    if findings(i).wo <> p_wo;
      iter;
    endif;
    one = fl_jsonStr(%trim(findings(i).finding) + ' '
                   + %trim(findings(i).impact));
    if %len(dqBuf) + %len(one) + 2 > 690;
      leave;
    endif;
    if dqShown > 0;
      dqBuf += ',';
    endif;
    dqBuf += one;
    dqShown += 1;
  endfor;
  dqBuf += ']';
  sdqtext = dqBuf;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write wodtl;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numOps;
    sseq     = ops(i).seq;
    sowc     = ops(i).wctr;
    sowcd    = ops(i).wctrd;
    sodesc   = ops(i).descr;
    sostd    = ops(i).stdhrs;
    soact    = ops(i).acthrs;
    sovar    = ops(i).varpct;
    sostat   = ops(i).statusd;
    sostrt   = ops(i).started;
    socomp   = ops(i).compl;
    soshort  = ops(i).shorts;
    soshdsc  = ops(i).shortdsc;
    soshpart = ops(i).shortpar;
    soshdue  = ops(i).shortdue;
    soshpo   = ops(i).shortpo;
    soshven  = ops(i).shortven;
    rrn += 1;
    write opsfl;
  endfor;
endsr;
