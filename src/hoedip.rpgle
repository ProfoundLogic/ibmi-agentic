**free
ctl-opt nomain;

dcl-proc HOEDIP export;
  dcl-pi *n;
    Turn zoned(4);
    Cust# zoned(7);
  end-pi;
  // Stub for HOEDIP.  Called only in commented-out code in HYR0600.
end-proc;
