**FREE
 //*********************************
 //  PICKBATSV service program
 //   Note: Create program by submitting
 //      CALL PGM(CTSRVPGM)
 //       PARM((PICKBATSV (*CHAR 10)) (XXHORNADY(*CHAR 10)))
 //        ** You can change XXHORNADY for HMCLIB
 //
 //   You can also create the service by
 //     create the module from this source and then the service program.
 //
 //   Note: Binding Source PICKBATSV.BND should be in QSRVSRC and
 //         COPYBOOK PICKBATSVD shoud be in QCPYLESRC
 //         Both should be in the library passed to CTSRVPGM
 //
 //********************************
  Ctl-Opt OPTION(*SRCSTMT : *NODEBUGIO)
          datfmt(*iso)
           langid('ENU')
           srtseq(*langidshr)
           cvtopt(*NOVARCHAR:*NODATETIME:*NOGRAPHIC:*NOVARGRAPHIC)
           extbinint(*yes)
           NOMAIN;


      /copy qcpylesrc,pickbatsvd
 exec sql
   set option COMMIT = *NONE,CLOSQLCSR = *ENDMOD, DATFMT = *ISO,
          TIMFMT = *ISO, DATSEP = *DASH,TIMSEP = *PERIOD,
          SRTSEQ = *LANGIDSHR, SQLPATH = *LIBL, NAMING = *SYS;

 //--------------------------------------------------//
 //  Procedure pickbatsv_setDefaultPickOrder
 //    Reset PICKOVRORD of the items in a batch.
 //--------------------------------------------------//
  dcl-proc pickbatsv_setDefaultPickOrder export;
   dcl-pi *n ind;
     batch zoned(8:0) value;
   end-pi;

   dcl-s pickorder zoned(8:0);
   dcl-s wrkOrder zoned(8:0) inz(10);

   EXEC SQL DECLARE pickorderc CURSOR FOR
      SELECT PICKOVRORD
       FROM PICKBATDP
       WHERE PICKBAT = :batch
        order by PICKWHS,PICKSTKRM,PICKAISLE,PICKLOC
       FOR update of PICKOVRORD;

  monitor;
   exec sql
      open  pickorderc;

   if sqlstate <> '00000' and %subst(sqlstate:1:2) <> '01';
     return *off;
   endif;

   exec sql
     fetch pickorderc into :pickorder;

   dow sqlstate = '00000' or %subst(sqlstate:1:2) = '01';

    exec sql
     update PICKBATDP
     set PICKOVRORD = :wrkOrder
     where current of pickorderc;

    wrkOrder += 10;

    exec sql
    fetch pickorderc into :pickorder;
   enddo;

   return *on;
  on-error;
   return *off;
  endmon;

  on-exit;
    exec sql
      close pickorderc;

 end-proc;


