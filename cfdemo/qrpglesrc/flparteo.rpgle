**free

/////////////////////
// Parts Finder - parts that fit a machine as built (EJS)   GJA-912
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flparteo workstn sfile(partsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pi *n;
  p_serial char(8) const options(*nopass);
end-pi;

dcl-pr flpart1eo extpgm;
  part char(15) const;
end-pr;

dcl-pr flmacheo extpgm;
  search char(40) const;
  serial char(8);
end-pr;

dcl-ds machine likeds(fl_mach_t);
dcl-ds parts likeds(fl_part_t) dim(500);
dcl-ds opts likeds(fl_optn_t) dim(50);
dcl-s numParts int(10) inz(0);
dcl-s numOpts int(10) inz(0);
dcl-s found ind;
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s selrrn int(10);
dcl-s wantSer char(8) inz(*blanks);
dcl-s wantSearch char(40) inz(*blanks);
dcl-s optList varchar(200);
dcl-s picked char(8);

// a serial passed in wins (Fleet 360 jumps straight here for one machine)
if %parms() >= 1 and p_serial <> *blanks;
  wantSer = p_serial;
endif;

// otherwise open on a machine with a rich fitment list rather than an empty form
if wantSer = *blanks;
  err = fl_defaultMachine(wantSer);
  if err <> '';
    msg = err;
  endif;
endif;

dow action <> 'EXIT';

  if wantSer <> *blanks;
    exsr loadMachine;
  endif;

  if numParts = 0;
    soptdesc = '';
  else;
    soptdesc = 'Double-click a row, or type 5';
  endif;

  if numParts > 0;
    exsr loadSFL;
  else;
    exsr clearSFL;
  endif;

  sserial = wantSer;
  ssearch = wantSearch;
  action = '';
  exfmt partctl;

  select;
    when action = 'EXIT';
      leave;
    when action = 'LOOKUP';
      // Open the lookup unfiltered. Seeding it with the serial already on
      // screen would just return the machine the user is trying to move away
      // from - they want to see the whole installed base and narrow it down.
      picked = *blanks;
      flmacheo(*blanks : picked);
      if picked <> *blanks;
        wantSer = picked;
        wantSearch = *blanks;
      endif;
      iter;
  endsl;

  // serial or search changed on screen
  if sserial <> wantSer or ssearch <> wantSearch;
    wantSer = sserial;
    wantSearch = ssearch;
    iter;
  endif;

  // subfile selection
  if numParts > 0;
    selrrn = 0;
    sopt = '';
    readc partsfl;
    dow not %eof(flparteo);
      if sopt <> '' and selrrn = 0;
        selrrn = rrn;
      endif;
      update partsfl;
      readc partsfl;
    enddo;
    if selrrn <> 0;
      chain selrrn partsfl;
      select;
        when %trim(sopt) = '5';
          flpart1eo(spart);
        other;
          msg = 'Option ' + %trim(sopt) + ' is not valid. Use 5.';
      endsl;
    endif;
  endif;

enddo;

*inlr = *on;

begsr loadMachine;
  msg = '';
  numParts = 0;
  sdupwarn = 0;
  clear machine;
  smodel = '';
  smodeldsc = '';
  scustname = '';
  ssitename = '';
  sbuilt = '';
  soptions = '';

  err = fl_getMachine(wantSer : machine : found);
  if err <> '';
    msg = err;
    return;
  endif;
  if not found;
    // Dead ends are unhelpful. Here the typed text IS worth carrying over -
    // the user typed something meaningful that simply did not match a serial,
    // so seed the lookup with it. (The Look up button, by contrast, opens
    // unfiltered - see the LOOKUP action above.)
    picked = *blanks;
    flmacheo(wantSer : picked);
    if picked <> *blanks;
      wantSer = picked;
      err = fl_getMachine(wantSer : machine : found);
    endif;
    if not found;
      msg = 'No machine matched "' + %trim(wantSer) + '". Use Look up machine.';
      wantSer = *blanks;
      return;
    endif;
  endif;

  smodel    = machine.model;
  smodeldsc = machine.modeldsc;
  scustname = machine.custname;
  ssitename = machine.sitename;
  sbuilt    = machine.built;

  // options fitted - what makes this machine different from its model
  err = fl_listOptions(wantSer : opts : %elem(opts) : numOpts);
  if err = '';
    optList = '';
    for i = 1 to numOpts;
      if optList <> '';
        optList += ', ';
      endif;
      optList += %trimr(opts(i).descr);
    endfor;
    soptions = optList;
  endif;

  err = fl_listPartsForMachine(wantSer : %trim(wantSearch) : parts :
                               %elem(parts) : numParts);
  if err <> '';
    msg = err;
    return;
  endif;
  for i = 1 to numParts;
    if parts(i).dupes > 0;
      sdupwarn += 1;
    endif;
  endfor;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write partctl;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numParts;
    sopt     = '';
    spart    = parts(i).part;
    sdescr   = parts(i).descr;
    sassy    = parts(i).assy;
    sqty     = parts(i).qty;
    sstock   = parts(i).stock;
    sprice   = parts(i).price;
    slead    = parts(i).lead;
    sstatusd = parts(i).statusd;
    sdupes   = parts(i).dupes;
    rrn += 1;
    write partsfl;
  endfor;
endsr;
