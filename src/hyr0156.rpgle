**free
// Stub for HYR0156 - "Missing pallet check".

dcl-pi *n;
  Pallet#    packed(9)  const;
  OrderList  varchar(2400) const options(*varsize);
  TurnList   varchar(2700) const options(*varsize);
  OrderCnt   int(10)    const;
  AlwNone    ind        const;
  ForceF21   ind        const;
  ShutDown   ind        const;
  RtnCode    char(2);
end-pi;

RtnCode = '  ';
return;
