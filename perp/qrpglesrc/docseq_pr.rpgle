**free

// ---------------------------------------------------------------------
// Prototypes:  docseq  (document sequence service)
// Module:      perp
// Purpose:     Atomic per-company document-number allocator.
//              Callers: requisition entry (REQ), PO entry (PO),
//              receipt entry (RCP), any future doc-number consumer.
// Epic:        PERP-2 (PERP-19)
// ---------------------------------------------------------------------

// docseq_next — bump the (company, doc_type) high-water mark and
// return the newly-issued number. Auto-inserts the sequence row
// starting at 1 on first use. Returns 0 on error; errmsg carries
// 'SQLCODE=... SQLSTATE=...'. Callers ROLLBACK on error.
dcl-pr docseq_next int(20);
  company      char(3)      const;
  doctype      varchar(20)  const;
  errmsg       varchar(80);
end-pr;

// docseq_peek — read the current high-water mark WITHOUT bumping.
// Returns 0 if the row doesn't exist yet.
dcl-pr docseq_peek int(20);
  company      char(3)      const;
  doctype      varchar(20)  const;
  errmsg       varchar(80);
end-pr;
