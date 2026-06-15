**free
// Stub for HYR0602 - Shipment Detail Processing.
// Accepts the parm signature HYR0600 passes; immediately returns.

dcl-pi *n;
  i_PgmMode char(5) const;
  i_Grp#    zoned(9) const;
  i_BlTo#   zoned(7) const;
  i_ShTo#   zoned(7) const;
  i_DShp#   zoned(7) const;
end-pi;

return;
