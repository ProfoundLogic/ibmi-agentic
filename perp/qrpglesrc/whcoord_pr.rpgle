**free

// ---------------------------------------------------------------------
// Prototypes:  whcoord  (warehouse coordinate query service)
// Module:      perp
// Purpose:     Iterator over located items in a company, with x/y/z
//              coordinates derived from warehouse_layout. Data source
//              for the (future RDF/EJS) 3D map renderer -- this story
//              only delivers the queryable data.
// Epic:        PERP-4 (3D Warehouse Map) / PERP-27
//
// Usage:       ok = whcoord_open(company : errmsg);
//              dow whcoord_fetch(itemnbr : itemdsc : aisle : bay : shelf
//                               : xm : ym : zm : onhand : avail : errmsg);
//                ... one row ...
//              enddo;
//              whcoord_close();
//
// Design note: item.aisle_code/bay_code/shelf_code (PERP-3/PERP-20) are
// free-text VARCHAR(10) location codes, and real seed data mixes a
// letter prefix with the ordinal (e.g. 'A1', 'B2', 'S3' -- confirmed
// against the WIDGET1 seed row). CAST(... AS INTEGER) rejects that
// outright (SQL0420), so whcoord.sqlrpgle's private ordinalFromCode()
// pulls the digit characters out of each code instead of casting the
// whole string. Only items where all three codes are non-blank are
// returned -- unlocated items are simply not on the map yet.
// ---------------------------------------------------------------------

// whcoord_open -- scope the iterator to one company. Also loads that
// company's warehouse_layout row (if any); if none exists yet,
// whcoord_fetch still enumerates items but returns x/y/z as 0.
// Returns *off on SQL error opening the cursor (errmsg carries detail).
dcl-pr whcoord_open ind;
  company char(3)     const;
  errmsg  varchar(80);
end-pr;

// whcoord_fetch -- advance to the next located item. Returns *off at
// end of data or on error (errmsg carries 'SQLCODE=... SQLSTATE=...'
// in the error case; blank at normal end of data).
dcl-pr whcoord_fetch ind;
  itemNumber      varchar(25);
  itemDescription varchar(60);
  aisleOrdinal    int(10);
  bayOrdinal      int(10);
  shelfOrdinal    int(10);
  xMeters         packed(9:4);
  yMeters         packed(9:4);
  zMeters         packed(9:4);
  qtyOnHand       packed(15:4);
  qtyAvailable    packed(15:4);
  errmsg          varchar(80);
end-pr;

// whcoord_close -- close the cursor. Always returns *on.
dcl-pr whcoord_close ind;
end-pr;
