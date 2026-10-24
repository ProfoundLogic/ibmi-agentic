**free

// ---------------------------------------------------------------------
// Module:  whcoord  (warehouse coordinate query service)
// Purpose: Return every located item in a company joined with x/y/z
//          coordinates computed from that company's warehouse_layout
//          grid config. Data source for the future 3D map renderer.
// Epic:    PERP-4 (3D Warehouse Map) / PERP-27
// ---------------------------------------------------------------------

ctl-opt nomain;

// Real commitment control against PERPJRN -- per DDL_STYLE_GUIDE §7.
exec sql set option closqlcsr = *endmod;

/copy whcoord_pr.rpgle

// Module-scope: the company the cursor is open against, and that
// company's layout dims (cached at open time rather than re-queried
// per fetched row).
dcl-s g_company    char(3);
dcl-s g_haslayout  ind;
dcl-s g_binwid     packed(9:4);
dcl-s g_binhgt     packed(9:4);
dcl-s g_aslspc     packed(9:4);

// Only items with a fully-populated location are placed on the map --
// see the design note in whcoord_pr.rpgle. Codes come back as-is (not
// CAST to INTEGER here) -- real seed data mixes a letter prefix with
// the ordinal (e.g. 'A1', 'B2', 'S3'), which CAST(... AS INTEGER)
// rejects outright (SQL0420). ordinalFromCode() below parses it.
exec sql declare whcoordcsr cursor for
  select item_number, item_description,
         aisle_code, bay_code, shelf_code,
         qty_on_hand, qty_available
    from perpdemo.item
   where company_code = :g_company
     and aisle_code <> ''
     and bay_code   <> ''
     and shelf_code <> ''
   order by item_number;

// ---------------------------------------------------------------------
dcl-proc whcoord_open export;
  dcl-pi *n ind;
    oc_company char(3)     const;
    oc_errmsg  varchar(80);
  end-pi;

  oc_errmsg   = '';
  g_company   = oc_company;
  g_haslayout = *off;

  exec sql
    select bin_width_m, bin_height_m, aisle_spacing_m
      into :g_binwid, :g_binhgt, :g_aslspc
      from perpdemo.warehouse_layout
     where company_code = :g_company;
  if sqlcode = 0;
    g_haslayout = *on;
  endif;

  exec sql open whcoordcsr;
  if sqlcode < 0;
    oc_errmsg = 'whcoord_open: SQLCODE=' + %char(sqlcode)
              + ' SQLSTATE=' + sqlstate;
    return *off;
  endif;

  return *on;
end-proc;

// ---------------------------------------------------------------------
dcl-proc whcoord_fetch export;
  dcl-pi *n ind;
    fc_itemnbr varchar(25);
    fc_itemdsc varchar(60);
    fc_aisle   int(10);
    fc_bay     int(10);
    fc_shelf   int(10);
    fc_xm      packed(9:4);
    fc_ym      packed(9:4);
    fc_zm      packed(9:4);
    fc_onhand  packed(15:4);
    fc_avail   packed(15:4);
    fc_errmsg  varchar(80);
  end-pi;

  dcl-s aisleCode varchar(10);
  dcl-s bayCode   varchar(10);
  dcl-s shelfCode varchar(10);

  fc_errmsg = '';

  exec sql
    fetch whcoordcsr into :fc_itemnbr, :fc_itemdsc,
                          :aisleCode, :bayCode, :shelfCode,
                          :fc_onhand, :fc_avail;
  if sqlcode = 100;
    return *off;
  endif;
  if sqlcode < 0;
    fc_errmsg = 'whcoord_fetch: SQLCODE=' + %char(sqlcode)
              + ' SQLSTATE=' + sqlstate;
    return *off;
  endif;

  // Ordinal 1 is the floor -- a code with no digits at all (shouldn't
  // happen given the non-blank WHERE filter, but be defensive) still
  // places the item at the grid origin rather than off in the
  // negative direction.
  fc_aisle = %max(1 : ordinalFromCode(aisleCode));
  fc_bay   = %max(1 : ordinalFromCode(bayCode));
  fc_shelf = %max(1 : ordinalFromCode(shelfCode));

  if g_haslayout;
    fc_xm = (fc_bay   - 1) * g_binwid;
    fc_ym = (fc_aisle - 1) * g_aslspc;
    fc_zm = (fc_shelf - 1) * g_binhgt;
  else;
    fc_xm = 0;
    fc_ym = 0;
    fc_zm = 0;
  endif;

  return *on;
end-proc;

// ---------------------------------------------------------------------
dcl-proc whcoord_close export;
  dcl-pi *n ind;
  end-pi;

  exec sql close whcoordcsr;
  return *on;
end-proc;

// ---------------------------------------------------------------------
// ordinalFromCode -- module-private. Pulls every digit character out
// of a location code (e.g. 'A1' -> 1, 'B12' -> 12) and returns it as
// an integer. Returns 0 if the code has no digits at all.
dcl-proc ordinalFromCode;
  dcl-pi *n int(10);
    code varchar(10) const;
  end-pi;

  dcl-s i      int(10);
  dcl-s trimmed varchar(10);
  dcl-s digits varchar(10);
  dcl-s ch     char(1);

  trimmed = %trim(code);
  digits = '';
  for i = 1 to %len(trimmed);
    ch = %subst(trimmed : i : 1);
    if ch >= '0' and ch <= '9';
      digits += ch;
    endif;
  endfor;

  if digits = '';
    return 0;
  endif;
  return %int(digits);
end-proc;
