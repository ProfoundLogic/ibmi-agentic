**free
// HYR9960 -- Item / barcode utility procedures (STUB).
// Real source: Hornady/src/HornadyDemo/Source/HYR9960.RPGLE.  Real bodies
// require the BARDATA / BARCUST tables which we haven't promoted.  Stubs
// return empty / zero so PICKERR can bind cleanly; scans won't match real
// SKUs until the real procedures are wired up.

ctl-opt nomain option(*srcstmt: *nodebugio);

  /copy pickerr_pr.rpgle

dcl-proc BarcodeItem export;
  dcl-pi *n char(15);
    Barcode char(14) const;
  end-pi;
  return *blanks;
end-proc;

dcl-proc BarcodeQty export;
  dcl-pi *n packed(5:0);
    Barcode char(14) const;
  end-pi;
  return 0;
end-proc;

dcl-proc sd_HYR9960 export;
  dcl-pi *n end-pi;
  // no-op cleanup
  return;
end-proc;
