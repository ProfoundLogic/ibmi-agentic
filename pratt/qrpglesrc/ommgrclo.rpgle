**FREE
// ---------------------------------------------------------------------------
// OMMGRCLO - classic Material Requisition Manager (OMMGR) green screen.
//
// RECONSTRUCTION STUB.  Pratt supplied OMMGRCL.rpgle (3,915 lines of RPG III)
// and OMMGRCLO.clp, but NOT the OMMGR display file DDS, and that display file
// is the program's primary WORKSTN file.  Without it OMMGR cannot be compiled
// at all, so the green screen half of the hybrid is out of reach from this
// bundle.  The original sources are kept in qrpglesrc/ommgrcl.rpgle and
// qclsrc/ommgrclo.clp for reference.
//
// This stub exists so that the "open the manager" buttons on PMSELUI and
// PODTLUI return cleanly instead of failing with a program-not-found error.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('OMMGRCLO');
    @UPDAT CHAR(1) CONST;
  END-PI;
  // no-op: see comment above
END-PROC;
