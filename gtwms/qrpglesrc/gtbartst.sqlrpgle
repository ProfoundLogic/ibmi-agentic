**free

//////////////////////////////////////////////////////////////////////////
// GTBARTST -- unit test for the GTBAR barcode engine.
//
// Not part of the application. Barcode parsing is the one piece of logic
// that fails in front of the audience if it is wrong, so it is verified
// against real seeded data rather than by eye.
//
// Test payloads are pulled from the database rather than hardcoded, so the
// test stays valid when the demo data is regenerated. Results land in
// GTBARTST for inspection from outside the program.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTBAR');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtbar_pr.rpgle

dcl-pr runCmd extpgm('QCMDEXC');
  cmd char(3000) const options(*varsize);
  len packed(15 : 5) const;
end-pr;

dcl-c GS x'1D';

dcl-ds r likeds(gtbar_result_t);

dcl-s cmd      varchar(300);
dcl-s testNo   int(10) inz(0);

// Sample keys read back out of the seeded data.
dcl-s upcA     varchar(20);
dcl-s itf14    varchar(20);
dcl-s locId    varchar(10);
dcl-s badge    varchar(20);
dcl-s sscc     varchar(18);
dcl-s gs1      varchar(120);
dcl-s badUpc   varchar(20);

//------------------------------------------------------------------------
cmd = 'ADDLIBLE LIB(TIGERPOC) POSITION(*FIRST)';
monitor;
  runCmd(cmd : %len(cmd));
on-error;
endmon;

exec sql delete from gtbartst;

// ---- Gather real payloads from the seed ------------------------------
exec sql
  select barcode into :upcA from gtbarcode
   where symbology = 'UPC-A' order by barcode fetch first row only;

exec sql
  select barcode into :itf14 from gtbarcode
   where symbology = 'ITF-14' order by barcode fetch first row only;

exec sql
  select location_id into :locId from gtlocation
   where location_type = 'BULK' order by location_id fetch first row only;

exec sql
  select badge_barcode into :badge from gtoperator
   order by operator_id fetch first row only;

exec sql
  select h.sscc into :sscc from gtrcpthdr h
   where h.receipt_status = 'OPEN' order by h.receipt_id fetch first row only;

// A realistic GS1-128 pallet label: SSCC, GTIN, lot, expiry. The variable
// length lot has to be terminated by a separator before the next AI.
gs1 = '00' + %trim(sscc) +
      '01' + %trim(itf14) +
      '10' + 'L2026A17' + GS +
      '17' + '270131';

// Same UPC with the last digit bumped, so the check digit must fail.
badUpc = %subst(%trim(upcA) : 1 : %len(%trim(upcA)) - 1) +
         %char(%rem(%int(%subst(%trim(upcA) : %len(%trim(upcA)) : 1)) + 1 : 10));

// ---- Run the cases ---------------------------------------------------
runCase('UPC-A from seed'          : upcA        : 'ITEM');
runCase('ITF-14 case code'         : itf14       : 'ITEM');
runCase('GS1-128 pallet label'     : gs1         : 'RCPT');
runCase('Location label'           : locId       : 'LOC');
runCase('Operator badge'           : badge       : 'OPER');
runCase('Bad check digit'          : badUpc      : 'UNKNWN');
runCase('Not one of ours'          : '5901234123457' : 'UNKNWN');
// A phone scanner returns UPC-A as EAN-13 with a leading zero. This must
// still resolve to the item, or every real product scans as unknown.
runCase('UPC-A as 13-digit EAN'    : '0' + %trim(upcA) : 'ITEM');
runCase('Empty scan'               : ''          : 'UNKNWN');
// AIM identifier and literal {GS}, as different scanners emit them.
runCase('AIM prefix stripped'      : ']C1' + %trim(gs1) : 'RCPT');
runCase('Literal {GS} separator'   : '00' + %trim(sscc) + '01' + %trim(itf14) +
                                     '10' + 'L2026A17' + '{GS}' + '17' + '270131' : 'RCPT');

exec sql commit;

*inlr = *on;
return;

//========================================================================
// runCase -- parse one payload and record what came back.
//========================================================================
dcl-proc runCase;
  dcl-pi *n;
    name     varchar(40)  const;
    payload  varchar(120) const;
    expected char(6)      const;
  end-pi;

  dcl-s gs1Flag char(1);
  dcl-s chkFlag char(1);
  dcl-s descr   varchar(60);

  testNo += 1;
  gtbar_parse(payload : r);

  gs1Flag = 'N';
  if r.isGs1;
    gs1Flag = 'Y';
  endif;

  chkFlag = 'N';
  if r.checkOk;
    chkFlag = 'Y';
  endif;

  descr = r.descEn;

  exec sql
    insert into gtbartst
           (test_no, test_name, payload, kind, symbology, is_gs1, check_ok,
            sku, location_id, sscc, gtin, lot, expiry, qty_count, descr, expected)
    values (:testNo, :name, :payload, :r.kind, :r.symbology, :gs1Flag, :chkFlag,
            :r.sku, :r.locationId, :r.sscc, :r.gtin, :r.lot, :r.expiry,
            :r.qtyCount, :descr, :expected);
end-proc;
