**free

//////////////////////////////////////////////////////////////////////////
// GTIMGTST -- proves the capture path before any UI depends on it.
//
// Takes the most recently staged row from GTIMGSTG, hands its base64 to the
// same GTIMG_ADD that the Item Lookup detail screen calls, and reports the
// resulting image_id. Everything downstream of that call is then exercised
// for real: BASE64_DECODE, the journaled BLOB, the IFS publish, and -- once
// the row exists -- GTVITEMIMG picking it up as an 'P' source image.
//
// Verification is external: query GTIMAGE, query GTVITEMIMG, and fetch the
// published URL and compare the bytes.
//
// WHY THERE ARE NO PARAMETERS
// ---------------------------
// The first version took the base64 as a CL parameter and stored nothing at
// all, silently. CL does not pad a character literal to the receiver's
// declared size, so a char(30000) parameter given a 4,560-character literal
// reads 30,000 bytes out of a 4,560-byte buffer -- the length guard then sees
// garbage and returns 0. Nothing is passed in now; the payload is staged in a
// table with SQL, which is also the only practical way to get 17,000
// characters of base64 onto the machine.
//
// Re-running is idempotent: prior photographs for the target SKU are removed
// first, so the external checks stay unambiguous.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTIMG');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtimg_pr.rpgle

dcl-pr runCmd extpgm('QCMDEXC');
  cmd char(3000) const options(*varsize);
  len packed(15 : 5) const;
end-pr;

dcl-s cmd    varchar(300);
dcl-s newId  int(10);
dcl-s b64    varchar(32000);
dcl-s sku    char(15);
dcl-s stgId  int(10);

cmd = 'ADDLIBLE LIB(TIGERPOC) POSITION(*FIRST)';
monitor;
  runCmd(cmd : %len(cmd));
on-error;
endmon;

exec sql
  select stg_id, target_sku, payload
    into :stgId, :sku, :b64
    from gtimgstg
   order by stg_id desc
   fetch first row only;

if sqlcode <> 0;
  cmd = 'SNDPGMMSG MSG(''GTIMGTST: nothing staged in GTIMGSTG'')';
  monitor;
    runCmd(cmd : %len(cmd));
  on-error;
  endmon;
  *inlr = *on;
  return;
endif;

//  Only the operator photographs are cleared. Catalogue imagery keys on
//  'FAMnnnn', never on a SKU, so it cannot be caught by this.
exec sql delete from gtimage where ref_type = 'ITEM' and ref_key = :sku;
exec sql commit;

newId = gtimg_add('ITEM' : sku : b64 :
                  'Warehouse photo - capture test' :
                  'Photo d''entrepôt - essai de capture');

cmd = 'SNDPGMMSG MSG(''GTIMGTST stg=' + %char(stgId) + ' sku=' + %trim(sku) +
      ' b64=' + %char(%len(b64)) + ' newId=' + %char(newId) + ''')';
monitor;
  runCmd(cmd : %len(cmd));
on-error;
endmon;

*inlr = *on;
return;
