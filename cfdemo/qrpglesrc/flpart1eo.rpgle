**free

/////////////////////
// Near-duplicate part cluster (EJS)                        GJA-912
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flpart1eo workstn sfile(dupsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pi *n;
  p_part char(15) const;
end-pi;

dcl-ds dupes likeds(fl_part_t) dim(50);
dcl-s numDupes int(10) inz(0);
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);

msg = '';
spart = p_part;
sdescr = '';

err = fl_findDuplicates(p_part : dupes : %elem(dupes) : numDupes);
if err <> '';
  msg = err;
endif;

// the source part is part of its own cluster - use its description as the title
for i = 1 to numDupes;
  if dupes(i).part = p_part;
    sdescr = dupes(i).descr;
    leave;
  endif;
endfor;
scount = numDupes;

exsr loadSFL;

action = '';
exfmt dupwin;

*inlr = *on;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write dupwin;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numDupes;
    sdpart    = dupes(i).part;
    sddescr   = dupes(i).descr;
    sdyear    = dupes(i).assy;
    sdstock   = dupes(i).stock;
    sdprice   = dupes(i).price;
    sdstatusd = dupes(i).statusd;
    sdsuper   = dupes(i).superby;
    rrn += 1;
    write dupsfl;
  endfor;
endsr;
