**free

//////////////////////////////////////////////////////////////////////////
// GTIMG -- image capture service.
//
// A photo taken on the device arrives here as base64 in a display-file
// field, and leaves as two things:
//
//   1. A journaled BLOB row in GTIMAGE  -- the system of record.
//   2. A JPEG file in the static serving directory -- so the carousel can
//      show it IMMEDIATELY, on this round trip, rather than after an
//      export run.
//
// WHY THE PROGRAM WRITES THE FILE ITSELF
// --------------------------------------
// Section 29 established that no /cgi-bin alias on this server can be
// pointed at TIGERPOC, so images are served as static files exported from
// the BLOBs. That was fine for seeded catalogue imagery, but a photo the
// operator just took has to appear at once -- "upload, then wait for a
// batch job" is not a demo. RPG can write to the IFS directly through the
// C runtime, so it does, and the export tool remains for bulk rebuilds.
//
// WHY BASE64 RATHER THAN BINARY
// -----------------------------
// The same reason as the upload endpoint that never happened: the base64
// alphabet is entirely invariant characters, so it survives the ASCII to
// EBCDIC translation of a display-file field untouched. Raw bytes would not.
//
// SIZE
// ----
// The payload rides in a single display-file field, so it is bounded by the
// record length. The client resizes until the base64 fits; see gt-photo.js.
//////////////////////////////////////////////////////////////////////////

ctl-opt nomain;

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtimg_pr.rpgle

//------------------------------------------------------------------------
// IFS access through the C runtime. No CGI, no shell, no external command.
//------------------------------------------------------------------------
dcl-pr ifsOpen int(10) extproc('open');
  path  pointer value options(*string);
  flags int(10) value;
  mode  uns(10) value options(*nopass);
  ccsid uns(10) value options(*nopass);
end-pr;

dcl-pr ifsWrite int(10) extproc('write');
  fd  int(10) value;
  buf pointer value;
  len uns(10) value;
end-pr;

dcl-pr ifsClose int(10) extproc('close');
  fd int(10) value;
end-pr;

//  O_WRONLY(2) + O_CREAT(8) + O_TRUNC(64) + O_CCSID(32)
dcl-c O_FLAGS 106;
dcl-c MODE_RW 438;      // 0666 -- the HTTP server has to read these
dcl-c CCSID_BINARY 65535;

//  ------------------------------------------------------------------------
//  The one place a filesystem path is named. Same reasoning as the library
//  constant in gtjrn.table.sql: change it here and nowhere else.
//  ------------------------------------------------------------------------
dcl-c IMG_DIR '/home/drusso/puidist/htdocs/profoundui/userdata/ui/gtimg/';

//========================================================================
// gtimg_add -- decode, store, publish.
//========================================================================
dcl-proc gtimg_add export;
  dcl-pi *n int(10);
    refType  char(4)        const;
    refKey   char(20)       const;
    b64      varchar(32000) const;
    capEn    varchar(60)    const;
    capFr    varchar(60)    const;
  end-pi;

  dcl-s newId   int(10) inz(0);
  dcl-s nextSeq int(10) inz(0);
  dcl-s payload varchar(32000);

  payload = b64;
  if %len(%trim(payload)) < 64;
    // Nothing worth storing. A truncated payload is far more likely than a
    // genuinely tiny photograph.
    return 0;
  endif;

  // Captured photos sit after the catalogue images for the same item.
  exec sql
    select coalesce(max(seq_no), 0) + 1 into :nextSeq
      from gtimage
     where ref_type = :refType and ref_key = :refKey;

  // thumb_data is deliberately the same bytes. A captured photo is already
  // small enough to serve directly, and only the seeded catalogue images are
  // ever requested at thumbnail size (from the search results list).
  exec sql
    insert into gtimage
           (ref_type, ref_key, seq_no, mime_type, width_px, height_px,
            byte_size, image_data, thumb_data, caption_en, caption_fr,
            committed_flag)
    values (:refType, :refKey, :nextSeq, 'image/jpeg', 0, 0,
            length(QSYS2.BASE64_DECODE(:payload)),
            QSYS2.BASE64_DECODE(:payload),
            QSYS2.BASE64_DECODE(:payload),
            :capEn, :capFr, 'Y');

  if sqlcode < 0;
    exec sql rollback;
    return 0;
  endif;

  exec sql
    select identity_val_local() into :newId from sysibm.sysdummy1;

  exec sql commit;

  if newId <= 0;
    return 0;
  endif;

  // Publish before returning, so the caller can render the new image on the
  // very next screen write.
  if not gtimg_publish(newId);
    // The row is committed and correct; only the servable copy failed. The
    // export tool can rebuild it, so this is not worth losing the photo over.
    return newId;
  endif;

  return newId;
end-proc;

//========================================================================
// gtimg_publish -- write one image's BLOB to the static serving path.
//========================================================================
dcl-proc gtimg_publish export;
  dcl-pi *n ind;
    imageId int(10) const;
  end-pi;

  dcl-s fd     int(10);
  dcl-s written int(10);
  dcl-s path   varchar(256);
  dcl-s blobData sqltype(blob : 4194304);

  exec sql
    select image_data into :blobData
      from gtimage
     where image_id = :imageId;

  if sqlcode <> 0 or blobData_len = 0;
    return *off;
  endif;

  path = IMG_DIR + %char(imageId) + '.jpg';

  fd = ifsOpen(path : O_FLAGS : MODE_RW : CCSID_BINARY);
  if fd < 0;
    return *off;
  endif;

  written = ifsWrite(fd : %addr(blobData_data) : blobData_len);
  ifsClose(fd);

  if written <> blobData_len;
    return *off;
  endif;

  // The carousel also asks for <id>t.jpg for some callers; point it at the
  // same bytes rather than leaving a broken thumbnail behind.
  path = IMG_DIR + %char(imageId) + 't.jpg';
  fd = ifsOpen(path : O_FLAGS : MODE_RW : CCSID_BINARY);
  if fd >= 0;
    ifsWrite(fd : %addr(blobData_data) : blobData_len);
    ifsClose(fd);
  endif;

  return *on;
end-proc;
