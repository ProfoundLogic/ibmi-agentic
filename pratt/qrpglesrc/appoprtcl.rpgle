**FREE
// ---------------------------------------------------------------------------
// APPOPRTCL - approve / transmit a purchase order to the vendor.
//
// RECONSTRUCTION STUB with the one behaviour the screen depends on.  Pratt's
// version prints the PO and e-mails or EDIs it to the vendor.  Here, pressing
// Approve marks the order as transmitted (OHMETH = 'Y') so the status shown on
// PODTLUI changes, which is what the Approve button is meant to demonstrate.
// Parameters are taken verbatim from Pratt's prototype.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('APPOPRTCL');
    @OHYY      CHAR(2);
    @OHPP      CHAR(2);
    @OH#       PACKED(6);
    @OHDC      CHAR(2);
    @OHMETH    CHAR(1);
    @SEND      CHAR(1);
    @OHSTAT    CHAR(1);
  END-PI;

  IF @SEND = 'Y' AND @OHSTAT <> 'X';
    @OHMETH = 'Y';
  ENDIF;
END-PROC;
