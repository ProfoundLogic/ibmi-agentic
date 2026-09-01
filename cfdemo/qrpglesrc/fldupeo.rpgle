**free

/////////////////////
// Duplicate part review queue (EJS)                             GJA-916
//
// Scorecard over the whole item master, then the candidate clusters the
// detector found. One cluster is one decision.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f fldupeo workstn sfile(dupsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pr fldup1eo extpgm;
  clid packed(9:0) const;
end-pr;

dcl-ds sc likeds(fl_dupsc_t);
dcl-ds clusters likeds(fl_dupc_t) dim(500);
dcl-s numClu int(10) inz(0);
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s selrrn int(10);
dcl-s wantFilter char(1) inz('N');
dcl-s created int(10);
dcl-s skipped int(10);
dcl-s run packed(9:0);

dow action <> 'EXIT';

  exsr loadQueue;

  if numClu > 0;
    soptdesc = 'Double-click a cluster to review it, or type 5';
  else;
    soptdesc = '';
  endif;

  if numClu > 0;
    exsr loadSFL;
  else;
    exsr clearSFL;
  endif;

  sfilter = wantFilter;
  action = '';
  exfmt dupctl;

  select;
    when action = 'EXIT';
      leave;
    when action = 'RESCAN';
      run = %dec(%char(%date() : *iso0) : 8 : 0);
      err = fl_scanDuplicates(run : created : skipped);
      if err <> '';
        msg = err;
      else;
        msg = 'Detector run complete. ' + %char(created) + ' new cluster(s); '
            + %char(skipped) + ' already decided and left alone.';
      endif;
      iter;
  endsl;

  if sfilter <> wantFilter;
    wantFilter = sfilter;
    iter;
  endif;

  if numClu > 0;
    selrrn = 0;
    sopt = '';
    readc dupsfl;
    dow not %eof(fldupeo);
      if sopt <> '' and selrrn = 0;
        selrrn = rrn;
      endif;
      update dupsfl;
      readc dupsfl;
    enddo;
    if selrrn <> 0;
      chain selrrn dupsfl;
      if %trim(sopt) = '5';
        fldup1eo(sclid);
      else;
        msg = 'Option ' + %trim(sopt) + ' is not valid. Use 5.';
      endif;
    endif;
  endif;

enddo;

*inlr = *on;

begsr loadQueue;
  numClu = 0;
  err = fl_dupScorecard(sc);
  if err <> '';
    msg = err;
    return;
  endif;
  sparts  = sc.parts;
  sclust  = sc.clusters;
  sinvol  = sc.involved;
  sval    = sc.val;
  sbom    = sc.bomlines;
  sconf80 = sc.c80;
  scnew   = sc.cnew;
  scmerg  = sc.cmerged;
  screj   = sc.crej;
  scdef   = sc.cdef;
  err = fl_listClusters(wantFilter : clusters : %elem(clusters) : numClu);
  if err <> '';
    msg = err;
  endif;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write dupctl;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numClu;
    sopt     = '';
    sclid    = clusters(i).id;
    sconf    = clusters(i).conf;
    scnt     = clusters(i).cnt;
    stopdesc = clusters(i).topdesc;
    sreason  = clusters(i).reason;
    sstatd   = clusters(i).statusd;
    svalc    = clusters(i).val;
    sbomc    = clusters(i).bom;
    rrn += 1;
    write dupsfl;
  endfor;
endsr;
