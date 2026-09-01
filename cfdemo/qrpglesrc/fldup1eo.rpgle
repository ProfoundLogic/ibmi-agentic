**free

/////////////////////
// Duplicate cluster review (EJS)                                GJA-916
//
// Shows the members side by side with their blast radius, takes a survivor
// and a decision, and records who decided and why. Superseding is
// reversible; nothing is deleted.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f fldup1eo workstn sfile(memsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pi *n;
  p_clid packed(9:0) const;
end-pi;

dcl-ds psds psds;
  jobUser char(10) pos(254);
end-ds;

dcl-ds hdr likeds(fl_dupc_t);
dcl-ds members likeds(fl_dupm_t) dim(50);
dcl-s numMem int(10) inz(0);
dcl-s found ind;
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s selrrn int(10);
dcl-s picked char(15) inz(*blanks);
dcl-s keepNote char(120) inz(*blanks);
dcl-s decision char(1);
// parts the reviewer has marked to merge. Held across the exfmt loop so the
// marks survive a redisplay, since the subfile is reloaded from the database.
dcl-s marked char(15) dim(50);
dcl-s numMarked int(10) inz(0);
dcl-s j int(10);
dcl-s isMarked ind;

dow action <> 'EXIT';

  exsr loadCluster;
  if not found;
    msg = 'Cluster not found.';
    leave;
  endif;

  soptdesc = 'Double-click the part to keep, then choose an action';
  if numMem > 0;
    exsr loadSFL;
  else;
    exsr clearSFL;
  endif;

  ssurv = picked;
  snote = keepNote;
  action = '';
  exfmt dupdet;

  keepNote = snote;
  picked = ssurv;

  // Read EVERY changed row: 1 nominates the part to keep, 2 marks a part to
  // merge into it, blank leaves it alone.
  if numMem > 0;
    sopt = '';
    readc memsfl;
    dow not %eof(fldup1eo);
      select;
        when %trim(sopt) = '1';
          picked = spart;
        when %trim(sopt) = '2';
          if %lookup(spart : marked : 1 : numMarked) = 0
             and numMarked < %elem(marked);
            numMarked += 1;
            marked(numMarked) = spart;
          endif;
        when %trim(sopt) = '';
          // explicitly cleared - drop any previous mark
          j = %lookup(spart : marked : 1 : numMarked);
          if j > 0;
            for selrrn = j to numMarked - 1;
              marked(selrrn) = marked(selrrn + 1);
            endfor;
            numMarked -= 1;
          endif;
        other;
          msg = 'Option ' + %trim(sopt) + ' is not valid. Use 1 to keep, '
              + '2 to merge.';
      endsl;
      readc memsfl;
    enddo;
  endif;

  decision = ' ';
  select;
    when action = 'EXIT';
      leave;
    when action = 'MERGE';
      decision = 'M';
    when action = 'REJECT';
      decision = 'R';
    when action = 'DEFER';
      decision = 'D';
  endsl;

  if decision <> ' ';
    err = fl_resolveCluster(p_clid : picked : marked : numMarked : decision
                          : keepNote : jobUser);
    if err <> '';
      msg = err;
      iter;
    endif;
    leave;
  endif;

enddo;

*inlr = *on;

begsr loadCluster;
  msg = '';
  numMem = 0;
  err = fl_getCluster(p_clid : hdr : found);
  if err <> '';
    msg = err;
    return;
  endif;
  if not found;
    return;
  endif;
  sclid   = hdr.id;
  sconf   = hdr.conf;
  sreason = hdr.reason;
  sstatd  = hdr.statusd;
  stotbom = hdr.bom;
  stotval = hdr.val;
  if picked = *blanks and hdr.survivor <> *blanks;
    picked = hdr.survivor;
  endif;
  err = fl_listClusterMembers(p_clid : members : %elem(members) : numMem);
  if err <> '';
    msg = err;
  endif;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write dupdet;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numMem;
    sopt     = '';
    spart    = members(i).part;
    sdescr   = members(i).descr;
    screated = members(i).created;
    sstatusd = members(i).statusd;
    ssuper   = members(i).super;
    sstock   = members(i).stock;
    sprice   = members(i).price;
    svalm    = members(i).val;
    sbomm    = members(i).bom;
    sordm    = members(i).ord;
    swhy     = members(i).why;
    srole    = members(i).role;
    sroled   = members(i).roled;
    // show the pending decision, not just what is already stored
    if %trimr(members(i).part) = %trimr(picked);
      sopt = '1';
      sroled = 'Keep';
    elseif %lookup(members(i).part : marked : 1 : numMarked) > 0;
      sopt = '2';
      sroled = 'Merge in';
    endif;
    rrn += 1;
    write memsfl;
  endfor;
endsr;
