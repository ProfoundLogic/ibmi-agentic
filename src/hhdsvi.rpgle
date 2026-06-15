**free
ctl-opt nomain;

dcl-proc HHDSVI export;
  dcl-pi *n;
    ShpVia char(2);
  end-pi;
  // Stub for HHDSVI - Ship-via prompt.
  // Real program is not in the HornadyDemo source slice.
end-proc;
