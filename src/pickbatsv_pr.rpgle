      // Prototype copybook for PICKBATSV service program.
      // Replaces QCPYLESRC/PICKBATSVD which was not shipped with the
      // HornadyDemo package.  Only the procedure(s) actually referenced by
      // PICKBATR (and PICKERR if extended) need declarations here.

       dcl-pr pickbatsv_setDefaultPickOrder ind;
         batch zoned(8:0) value;
       end-pr;
