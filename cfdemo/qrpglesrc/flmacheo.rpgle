**free

/////////////////////
// Machine lookup - find a machine without knowing its serial   GJA-912
//
// Called from the Parts Finder. Returns the selected serial, or blanks if
// the user cancels.
/////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('FLETCHER');

dcl-f flmacheo workstn sfile(lkupsfl : rrn) handler('PROFOUNDUI(HANDLER)');

/copy fldata_pr.rpgle

dcl-pi *n;
  p_search char(40) const;
  p_serial char(8);
end-pi;

dcl-ds machines likeds(fl_mach_t) dim(500);
dcl-s numMach int(10) inz(0);
dcl-s err varchar(80);
dcl-s rrn int(10);
dcl-s i int(10);
dcl-s selrrn int(10);
dcl-s wantSearch like(ssearch);

p_serial = *blanks;
wantSearch = p_search;

dow action <> 'EXIT';

  exsr findMachines;

  if numMach > 0;
    soptdesc = 'Double-click a row, or type 5';
    exsr loadSFL;
  else;
    soptdesc = '';
    exsr clearSFL;
  endif;

  ssearch = wantSearch;
  scount = numMach;
  action = '';
  exfmt machlkup;

  if action = 'EXIT';
    leave;
  endif;

  if ssearch <> wantSearch;
    wantSearch = ssearch;
    iter;
  endif;

  if numMach > 0;
    selrrn = 0;
    sopt = '';
    readc lkupsfl;
    dow not %eof(flmacheo);
      if sopt <> '' and selrrn = 0;
        selrrn = rrn;
      endif;
      update lkupsfl;
      readc lkupsfl;
    enddo;
    if selrrn <> 0;
      chain selrrn lkupsfl;
      if %trim(sopt) = '5';
        p_serial = sserial;
        leave;
      else;
        msg = 'Option ' + %trim(sopt) + ' is not valid. Use 5.';
      endif;
    endif;
  endif;

enddo;

*inlr = *on;

begsr findMachines;
  msg = '';
  numMach = 0;
  err = fl_findMachines(%trim(wantSearch) : machines : %elem(machines) : numMach);
  if err <> '';
    msg = err;
  endif;
endsr;

begsr clearSFL;
  rrn = 0;
  sflclear = '1';
  write machlkup;
  sflclear = '0';
endsr;

begsr loadSFL;
  exsr clearSFL;
  for i = 1 to numMach;
    sopt      = '';
    sserial   = machines(i).ser;
    smodel    = machines(i).model;
    smodeldsc = machines(i).modeldsc;
    scustname = machines(i).custname;
    ssitename = machines(i).sitename;
    sbuilt    = machines(i).built;
    sstatusd  = machines(i).statusd;
    rrn += 1;
    write lkupsfl;
  endfor;
endsr;
