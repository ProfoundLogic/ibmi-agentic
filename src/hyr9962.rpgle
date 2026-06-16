**free
// HYR9962 -- Customer-owned barcode utility procedures (STUB).
// Real source: Hornady/src/HornadyDemo/Source/HYR9962.RPGLE.  Real bodies
// require the BARCUST table which we haven't promoted.  Stubs return
// empty / zero so PICKERR can bind cleanly.

ctl-opt nomain option(*srcstmt: *nodebugio);

  /copy pickerr_pr.rpgle

dcl-proc BarcodeItemC export;
  dcl-pi *n char(30);
    Barcode   char(14) const;
    ItemOwner char(1)  const;
  end-pi;
  return *blanks;
end-proc;

dcl-proc BarcodeQtyC export;
  dcl-pi *n packed(7:0);
    Barcode char(14) const;
  end-pi;
  return 0;
end-proc;
