**free
ctl-opt nomain;

dcl-proc HHDCSS export;
  dcl-pi *n;
    Cust# packed(7);
  end-pi;
  // Stub for HHDCSS - Customer number prompt.
  // Real program is not in the HornadyDemo source slice.
  // No-op: leave Cust# as caller set it.
end-proc;
