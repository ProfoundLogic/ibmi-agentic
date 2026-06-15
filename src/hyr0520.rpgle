**free
// Stub for HYR0520 - Display ship-via and comments.

dcl-pi *n;
  i_PgmMode      char(2);
  i_ShTo#        zoned(7);
  i_AryOrd       zoned(8) dim(99);
  i_AryCnt       zoned(5);
  i_ShipVia      char(2);
  i_ExtraComment char(256);
end-pi;

return;
