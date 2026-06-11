      *
      *  File FILECOPY except name DETG:  Group Check Stub
      *
      * ******************************************************************
      * REVISIONS:
      *
      *  06/11/12 C:277 PROJECT 114MER
      *                 Convert from RPG to RPGLE
      *
      * ******************************************************************
     OFILECOPY  E            DETG           1 25
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
