      // Inline prototype copybook for PICKERR.
      // Replaces QCPYLESRC/HYR9960 and QCPYLESRC/HYR9962 which were not
      // shipped with the HornadyDemo package.  Only the procedures actually
      // referenced from PICKERR are declared here -- BarcodeItem /
      // BarcodeQty / BarcodeItemC / BarcodeQtyC for scan parsing, and
      // sd_HYR9960 for the program-end cleanup hook.

       dcl-pr BarcodeItem char(15);
         Barcode char(14) const;
       end-pr;

       dcl-pr BarcodeQty packed(5:0);
         Barcode char(14) const;
       end-pr;

       dcl-pr BarcodeItemC char(30);
         Barcode    char(14) const;
         ItemOwner  char(1)  const;
       end-pr;

       dcl-pr BarcodeQtyC packed(7:0);
         Barcode char(14) const;
       end-pr;

       dcl-pr sd_HYR9960;
       end-pr;
