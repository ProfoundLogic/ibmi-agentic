**free
ctl-opt nomain;

// Stub service module for the procedures HYR0600 calls via EXTPROC.
// Real implementations live in HYR0610 / HYR0614 / HYR9930 in the
// Hornady source slice -- they aren't shipped here, so these no-ops
// satisfy the bind.

dcl-proc Center export;
  dcl-pi *n char(256);
    Input char(256) const;
    Width int(10) const options(*nopass);
  end-pi;
  return Input;
end-proc;

dcl-proc RtvForeign export;
  dcl-pi *n ind;
    CMCCLS char(2) const;
    GCSTCT char(2) const;
  end-pi;
  return *off;
end-proc;

dcl-proc CompCustShip export;
  dcl-pi *n int(10);
    Grp# zoned(9) const;
    BlTo# zoned(7) const;
    ShTo# zoned(7) const;
    DShp# zoned(7) const;
    GenBOL ind const;
    PltCnt zoned(5) const;
    ShutDown ind const;
  end-pi;
  return 0;
end-proc;

dcl-proc RtvCSPRO# export;
  dcl-pi *n char(20);
    DCLoc char(4) const;
    ShipVia char(2) const;
  end-pi;
  return '';
end-proc;

dcl-proc BOLReq export;
  dcl-pi *n ind;
    ShipVia char(2) const;
    Foreign ind const;
  end-pi;
  return *off;
end-proc;
