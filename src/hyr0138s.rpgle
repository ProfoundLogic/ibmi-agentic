**free
ctl-opt nomain;

// Stub service module for procedures HYR0138 imports from /COPY
// HYR0139 / HYR0552 / HYR0622 / HYR9930.  HHDCSS/HHDSVI/Center already
// live in HYR0600S/HHDCSS/HHDSVI service programs; everything else
// HYR0138 specifically needs is stubbed here.

// ------- /COPY HYR0139 procedures -------

dcl-proc ResetDropShips export;
  dcl-pi *n;
    Pallet# packed(9) const;
    Order#  packed(8) const;
  end-pi;
end-proc;

dcl-proc PalletLbl export;
  dcl-pi *n;
    Pallet# packed(9) const;
    Copies  packed(3) const;
  end-pi;
end-proc;

dcl-proc sd_HYR0139 export;
end-proc;

// ------- /COPY HYR0139A procedures (we have the implementation source,
//          but the stub keeps the bind simple; replace with the real
//          HYR0139A.module once needed) -------

dcl-proc WrtPRLog export;
  dcl-pi *n;
    Pallet# packed(9) const;
    Action  char(8)  const;
  end-pi;
end-proc;

dcl-proc RtvPRSts1 export char(10);
  dcl-pi *n char(10);
    Pallet# packed(9) const;
  end-pi;
  return '';
end-proc;

dcl-proc RtvPRSts2 export char(10);
  dcl-pi *n char(10);
    Pallet# packed(9) const;
  end-pi;
  return '';
end-proc;

dcl-proc sd_HYR0139A export;
end-proc;

// ------- /COPY HYR0552 / HYR0622 shutdown procs -------

dcl-proc sd_HYR0552 export;
end-proc;

dcl-proc sd_HYR0622 export;
end-proc;

// ------- HHDISI / HHDPCS - search popup stubs -------

dcl-proc HHDISI export;
  dcl-pi *n;
    Item# char(15);
  end-pi;
end-proc;

dcl-proc HHDPCS export;
  dcl-pi *n;
    ProdClass char(4);
  end-pi;
end-proc;
