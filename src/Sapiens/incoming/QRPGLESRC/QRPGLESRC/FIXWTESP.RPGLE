     H* (C) Copyright Fiserv Insurance Solution Inc., 2006          *
     H*
     H/TITLE FIXWTESP - Initialize WTESP Table
     H*****************************************************************
     H* Revised:
     H*
     H* 07/03/08 C:61 Project WC1530 PRISS #9
     H*               Rewrite.  Changes not commented.
     H*
     H*****************************************************************
      *
     Fsmcol     if   e           k disk
      *
      *
     Fwtesp     uf a e             disk
     Fwtesl     if   e           k disk    rename(pes01:pes02)
      *
      *
     d table_length    c                   const(93)

     d save_form       s             15
     d x1              s              2  0
     d process_form    s              3
      *
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)

     D                 ds
     D record                        22
     D  xxform                       15    overlay(record:1)
     D  xxst                          2    overlay(record:18)
     D  xxapex                        1    overlay(record:22)

     D psds          esds                  extname(szq1p)

     d tb1             s             22    dim(table_length)
     d                                     perrcd(1)
     d                                     ctdata
      *
      *  Endorsement States Approved table
      *
     C     esky          klist
     C                   kfld                    coco#
     C                   kfld                    xxform

     c     dtcymd        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      '*CYMD'       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1
      *
      *===============================================
      * Main
      *===============================================

      * for each company

     c                   eval      save_form = *blanks

     c     *loval        setll     pco01
     c                   read      pco01

     c                   dow       not %eof

      * for each record in table

     c                   eval      x1 = 1

     c                   dow       x1 <= table_length
     c                   eval      record = tb1(x1)

     c                   if        xxform <> save_form
     c                   eval      save_form = xxform
     c     esky          chain     pes02

     c                   if        %found
     c                   eval      process_form = 'NO'
     c                   else
     c                   eval      process_form = 'YES'
     c                   endif

     c                   endif

     c                   if        process_form = 'YES'
     c                   exsr      srwrt
     c                   endif

     c                   eval      x1 = x1 + 1
     c                   enddo

     c                   read      pco01
     c                   enddo

     c                   eval      *inlr = *on
      *
      *   END OF PROGRAM
      *
      *=========================================================
      * srwrt - write new record
      *=========================================================
     c     srwrt         begsr
     c                   clear                   pes01
      *
     c                   eval      esco#  = coco#
     c                   eval      esform = xxform
     c                   eval      esst   = xxst
     c                   eval      esapex = xxapex
     C                   eval      esusr  = q1user
     C                   eval      esiusr = q1user
     C                   eval      espgm = q1pgm
     C                   eval      esipgm = q1pgm
      *
     C                   time                    dsdttm
     C                   eval      estim  = dstime
     C                   eval      esitim = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         esdt
     C                   movel(p)  srdto         esidt
     C                   write     pes01
      *
     c                   endsr
      *
**CTDATA tb1
WC000421B        AL  A
WC000421B        AZ  A
WC000421B        AR  A
WC000421B        CO  A
WC000421B        CT  A
WC000421B        DC  A
WC000421B        GA  A
WC000421B        ID  A
WC000421B        IL  A
WC000421B        IN  A
WC000421B        IA  A
WC000421B        KS  A
WC000421B        KY  A
WC000421B        LA  A
WC000421B        ME  A
WC000421B        MD  A
WC000421B        MS  A
WC000421B        MT  A
WC000421B        NE  A
WC000421B        NV  A
WC000421B        NH  A
WC000421B        NC  A
WC000421B        OK  A
WC000421B        OR  A
WC000421B        RI  A
WC000421B        SC  A
WC000421B        SD  A
WC000421B        TN  A
WC000421B        UT  A
WC000421B        VT  A
WC000421B        WV  A
WC000422         AL  A
WC000422         AZ  A
WC000422         AR  A
WC000422         CO  A
WC000422         CT  A
WC000422         DC  A
WC000422         GA  A
WC000422         ID  A
WC000422         IL  A
WC000422         IN  A
WC000422         IA  A
WC000422         KS  A
WC000422         KY  A
WC000422         LA  A
WC000422         ME  A
WC000422         MD  A
WC000422         MS  A
WC000422         MT  A
WC000422         NE  A
WC000422         NV  A
WC000422         NH  A
WC000422         NC  A
WC000422         OK  A
WC000422         OR  A
WC000422         RI  A
WC000422         SC  A
WC000422         SD  A
WC000422         TN  A
WC000422         UT  A
WC000422         VT  A
WC000422         WV  A
WC000113A        AL  A
WC000113A        AZ  A
WC000113A        AR  A
WC000113A        CO  A
WC000113A        CT  A
WC000113A        DC  A
WC000113A        GA  A
WC000113A        ID  A
WC000113A        IL  A
WC000113A        IN  A
WC000113A        IA  A
WC000113A        KS  A
WC000113A        KY  A
WC000113A        LA  A
WC000113A        ME  A
WC000113A        MD  A
WC000113A        MS  A
WC000113A        MT  A
WC000113A        NE  A
WC000113A        NV  A
WC000113A        NH  A
WC000113A        NC  A
WC000113A        OK  A
WC000113A        OR  A
WC000113A        RI  A
WC000113A        SC  A
WC000113A        SD  A
WC000113A        TN  A
WC000113A        UT  A
WC000113A        VT  A
WC000113A        WV  A
