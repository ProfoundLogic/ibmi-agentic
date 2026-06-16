**free
// PICKBATLR2 -- stub for the "Publish Shipments" worker.
// Real source (Hornady/src/HornadyDemo/Source/PICKBATLR2.SQLRPGLE) is the
// MegaBatch shipment publisher.  Stub returns out_ShpGrp=0 so PICKBATR can
// bind and run the dashboard.

ctl-opt option(*srcstmt: *nodebugio) dftactgrp(*no) actgrp(*caller);

dcl-pi pickbatlr2;
  in_Batch         packed(8:0) const;
  out_ShpGrp       packed(9:0);
  in_OverrideGrp#  packed(8:0) options(*nopass: *omit);
  in_MegaBatch#    packed(8:0) const options(*nopass);
end-pi;

out_ShpGrp = 0;
return;
