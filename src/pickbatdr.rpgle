**free
// PICKBATDR -- stub for the Pick Batch detail / edit program.
// The real source (Hornady/src/HornadyDemo/Source/PICKBATDR.SQLRPGLE) is
// large and references the still-incomplete pick-batch data layer.  This
// stub matches the prototype that PICKBATR uses so the dashboard binds
// cleanly; selecting a row to edit just no-ops today.

ctl-opt option(*srcstmt: *nodebugio) dftactgrp(*no) actgrp(*caller);

dcl-pi pickbatdr;
  KeyPICKBAT zoned(8:0) const;
end-pi;

return;
