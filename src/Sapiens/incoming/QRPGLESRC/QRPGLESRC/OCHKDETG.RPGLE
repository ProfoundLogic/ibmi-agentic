      *
      *  File CHECKS except name CHECK:  Group Check Stub
      *
      * ******************************************************************
      * COPYBOOK:  OCHKCHECK
      * ******************************************************************
      * REVISIONS:
      *                                                               *
      *  03/10/16 C:309 Support: 39961                                *
      *                 Print Group number/name also in case when     *
      *                 *In71 = *Off                                  *
      *                                                               *
      *  06/24/14 C:053 Support: 86118                                *
      *                 If client 129 print heading further down      *
      *
      *  06/11/12 C:277 PROJECT 114MER
      *                 Convert from RPG to RPGLE
      *
      * ******************************************************************
     O*** Beg Del *** 06/24/14 ****************************************
     O****CHECKS    E            DETG           1 25
     O*** End Del *** 06/24/14 ****************************************
     O*** Beg Add *** 06/24/14 ****************************************
     OCHECKS    E      N71   DETG           1 25
    O*** Beg Add *** 03/10/16 ****************************************
     O                                            7 'GROUP:'
     O                       XXFND         4     11
     O                       XXFN                73
    O*** End Add *** 03/10/16 ****************************************
     OCHECKS    E       71   DETG           1 32
     O*** End Add *** 06/24/14 ****************************************
     O                                            7 'GROUP:'
     O                       XXFND         4     11
     O                       XXFN                73
     O          E            DETG           1
     O                                           14 'GROUP CHECK#:'
     O                       RPCHK#        4     42
     O          E            DETG           1
     O                                           24 'GROUP CHECK DATE RANGE:'
     O                       XXFRDT        Y     33
     O                                           34 '-'
     O                       XXTODT        Y     42
     O          E            DETG           1
     O                                           20 'GROUP CHECK AMOUNT:'
     O                       RPAMT         J     43
     O          E            DETG           1
     O                                           18 'GROUP CHECK DATE:'
     O                       XXDATE        Y     42
      *
