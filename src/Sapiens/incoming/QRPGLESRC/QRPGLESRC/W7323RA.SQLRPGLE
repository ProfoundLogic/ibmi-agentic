     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H optimize(*none)
     H*****************************************************************
     H/TITLE W7323RA - Deductible Billing Deletion
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* CREATED:                                                      *
     H*                                                               *
     H* 06/25/10 C:189 Project: WC1878                                *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/09/24 C:300 Project: 146WKU (Prescient)                    *
     H*                Collateral Phase 2: Spread over Multi-Employers*
     H*                - Delete the link batch records and Collateral *
     H*                Transfer batch records associated with other   *
     H*                Employers which provided Collateral to current Employer
     H*                                                               *
     H* 06/24/24 C:300 Project: 146WKR                                *
     H*                Collateral Processing                          *
     H*                Delete Collateral and Linked batch A/R records *
     H*                                                               *
     H* 06/24/24 C:079 Project: 146WKR                                *
     H*                Collateral Processing - Recomile               *
     H*                                                               *
     H* 09/14/21 C:079  WC2663                                        *
     H*                 Recompile to recognize new invoice# field     *
     H*                                                               *
     H* 07/01/13 C:180  WA146/82311                                   *
     H*                 Add missing code from v11 (Remove WDDCP recds)*
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  71 - 80  Used for error messaging                            *
     H*  85 - 88  Used for positioning cursor                         *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
     H*
     F* Employer A/R Batch File
     Fwdeal4    uf   e           K disk
     F                                     rename(Pea01:pea04) prefix(A_)
     FWdeal29   uf a e           k disk    usropn
     Fwmupl     if   e           K disk
     fwmemp     if   e             disk    rename(pem01 : pem0p)
     fwmfnl     if   e           k disk
     fwtabl     if   e           k disk
     fwmeal9    if a e           k disk    rename(pea01 : pea05)
     c*
     d getDDPCnt       pr
     d   xxpodt                       7  0
     d   xxdiv                        5  0
     c*
     d XXSYTP          s                   like(SYTYPE)
     d XXTYPE          s                   like(EAARTY)
     d XXBAT#          s              9  0
     d @batch#         s              9  0
     d X@PODT          s              7  0
     d xxbatch#        s              9  0 dim(9999)
     d xxtrans#        s               n   dim(9999)
     d   xxcount       s               n   inz(*off)
     d   xxtransfer    s               n   inz(*off)
     d found           s               n   inz(*off)
     d ##fnam          s             40
     d ##fnd           s              3  0
     d inkc            s              1
     d error           s              1
     d inkl            s              1
     d rrnGrp          s              9  0
     d rrnEmp          s              9  0
     d xxdiv1          s              5  0
     d grpselect       s               n   inz(*off)
     d EmpSelect       s               n   inz(*off)
     d DivSelect       s               n   inz(*off)
     d Xcounter        s              5  0
     d Z               s              5  0
     d xxbata          s             11  0
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D savedt          s              7  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     C*** Begin Add *** 06/24/24 *******************************************
     D xxlnkbat#       s              9  0 inz
     C*** End   Add *** 06/24/24 *******************************************
     C*** Begin Add *** 09/09/24 *******************************************
     D xxlike          s             11    inz
     C*** End   Add *** 09/09/24 *******************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    qqco#             3
     C                   PARM                    qqfnd             3
     C                   PARM                    qqemp#            9
     C                   PARM                    qqdiv             5
     C                   PARM                    qqpodt            7
     C*
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         savedt
     C*
     C                   Move      qqco#         xxco#             3 0
     C                   Move      qqfnd         xxfnd             3 0
     C                   Move      qqemp#        xxemp#            9 0
     C                   Move      qqdiv         xxdiv             5 0
     C                   Move      qqpodt        xxpodt            7 0
     C*
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    @batch#
     C*
     c     eaky#         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    x@podt
     c                   kfld                    @batch#
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     C*
     c     eaky@         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    x@podt
     c                   kfld                    @batch#
     c                   kfld                    xxemp#
     C*
     c     eaky$         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    x@podt
     c                   kfld                    @batch#
     C*
     c     tbkey         klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey            10
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C* Mainline code starts here
       //*** Beg Add *** 09/14/21 **************************************
        Exec sql SET OPTION commit=*none;
       //*** End Add *** 09/14/21 **************************************
     C                   exsr      selmode
      *
     C                   CAllP     getDDPCnt(xxpodt:xxdiv)
      *
     C                   exsr      Procdelete
     C
     C                   exsr      pgmend
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C                   ENDSR
    c*====================================================================
    c* Search employers
    c*====================================================================
     c     wrtar         begsr
     c
     c*Retrieve next available batch no
     c
     C                   eval      xxBata = 0
     C                   eval      xxBat# = *hival
     c
     c
     C     eaky          setgt     pea04
     C                   readp     pea04                                  60
     C                   eval      xxBat# = 1 + A_EABAT#
     c
     c*Read all the transferred records of the batch
     c                   open      wdeal29
     C                   eval      x@podt = xxpodt
     c
     c                   select
     c                   When      DIVSELECT = *ON
     C     eaky#         setll     pea01
     C     eaky#         reade(n)  pea01                                  60
     c                   When      EMPSELECT = *ON
     C     eaky@         setll     pea01
     C     eaky@         reade(n)  pea01                                  60
     c                   When      GRPSELECT = *ON
     C     eaky$         setll     pea01
     C     eaky$         reade(n)  pea01                                  60
     c                   endsl
     c
     C                   dow       *in60 = *off
     c
     c* Process only Dedeuctible catagory records
     c
     c                   if        eactyp = 'R'
     C*** Begin Add *** 06/24/24 *******************************************
     c* Also include to process Collateral transafer records (category 'G')
     c                             or eactyp = 'G'
     C*** End   Add *** 06/24/24 *******************************************
     c
     c                   eval      eabat# = xxbat#
     c                   eval      eaamt  = eaamt * -1
     c                   eval      xxBata= xxBata + eaamt
      *** beg del ***** 09/14/21 ***************************
     c***                eval      eapodt = savedt
      *** end del ***** 09/14/21 ***************************
     c                   eval      eatrdt = savedt
     c                   eval      eastat = 'T'
     c                   eval      eatfap = 'Y'
     c                   eval      eatffl = 'Y'
     C                   move      q1user        eauser
     C                   move      q1pgm         eapgm
     C                   move      q1user        eaiusr
     C                   move      q1pgm         eaipgm
     C                   move      dstime        eatime
     c                   write     pea01
     c
     c*Write WMEAP recoords and update batch log
     c
      *Get the A/R type
     C                   MOVEL     EASYCD        XXKEY
     c
     C*** Beg Chg *** 09/14/21 ********************************************
     C*****tbkey         Chain     PSY01
     C     tbkey         Chain     PSY01                              50
     C*** End Chg *** 09/14/21 ********************************************
     C                   if        *IN50 = '0' and SYSTAT  <> 'I'
     C                   MOVEL     SYTYPE        XXSYTP
     C                   endif
     c
     C                   Movel     EAARCD        XXKEY
      *
     C*** Beg Chg *** 09/14/21 ********************************************
     C**** tbkey         Chain     PTT01
     C     tbkey         Chain     PTT01                              50
     C*** End Chg *** 09/14/21 ********************************************
      *
     C                   if        *IN50 = '0' and TTSTAT  <> 'I'
     C                   Movel     TTTYPE        XXTYPE
      *
     C                   ELSE
     C                   MOVE      *BLANKS       XXTYPE
     C                   END
     C*
     C                   IF        XXSYTP = 'Y'
     C                   MOVE      XXTYPE        EAARTY
     C                   else
     C                   MOVE      ' '           EAARTY
     C                   endif
     C*
     c                   eval      eastat = 'A'
     c                   eval      eabul1= eaBat#
       //*** Beg Add *** 09/14/21 ******************************************
        EAninv = EAinv#;
       //*** End Add *** 09/14/21 ******************************************
     C*
     c                   write     pea05
     C*
     c                   endif                                                  eactyp = 'R'
     c*
     c                   select
     c                   When      DIVSELECT = *ON
     C     eaky#         reade(n)  pea01                                  60
     c                   When      EMPSELECT = *ON
     C     eaky@         reade(n)  pea01                                  60
     c                   When      GRPSELECT = *ON
     C     eaky$         reade(n)  pea01                                  60
     C                   endsl
     c                   enddo
     c
     c*Retrieve the user OutQueue
     c     q1user        Chain     PUP01                              60
     c
     c*Get the A/R batch edit report
     c
     c                   eval      @batch# = xxBat#
     c                   eval      x@podt  = savedt
     c
     c                   select
     c                   When      DIVSELECT = *ON
     C     eaky#         chain     pea01                              60
     c                   When      EMPSELECT = *ON
     C     eaky@         chain     pea01                              60
     c                   When      GRPSELECT = *ON
     C     eaky$         chain     pea01                              60
     C                   endsl
     c
     c     *IN60         Doweq     '0'
     c                   Z-ADD     xxbata        EABATA
     c
     c                   UPDATE    pea01
     c                   select
     c                   When      DIVSELECT = *ON
     C     eaky#         reade     pea01                                  60
     c                   When      EMPSELECT = *ON
     C     eaky@         reade     pea01                                  60
     c                   When      GRPSELECT = *ON
     C     eaky$         reade     pea01                                  60
     C                   endsl
     c                   enddo
     c
     C                   MOVE      xxCO#         @@CO#             3
     C                   MOVE      xxBat#        @@BAT#            9
     C                   MOVE      upoutq        @@OUTQ           10
     C
     c
     c                   Call      'W9201C'
     C                   Parm                    @@CO#
     C                   Parm                    @@BAT#
     C                   Parm                    @@outq
     c
     c                   IF        %Open(Wdeal29)
     c                   Close     wdeal29
     c                   endif
     c
     c                   endsr
     c
     C*** Begin Add *** 06/24/24 *******************************************
     c*=====================================================================---*
     c* Wrtar2 - Write offset record for transferred A/R batch of Collateral Offset
     c*          link batch record
     c*=====================================================================---*
     c     Wrtar2        begsr
     c
     c*Retrieve next available batch no
     c
     C                   eval      xxBata = 0
     C                   eval      xxBat# = *hival
     c
     c
     C     eaky          setgt     pea04
     C                   readp     pea04                                  60
     C                   eval      xxBat# = 1 + A_EABAT#
     c
     c*Read the transferred records of the link batch
     c                   open      wdeal29
     C                   eval      x@podt = xxpodt
     C                   eval      @batch# = xxlnkbat#
     c
     c                   select
     c                   When      DIVSELECT = *ON
     C     eaky#         setll     pea01
     C     eaky#         reade(n)  pea01                                  60
     c                   When      EMPSELECT = *ON
     C     eaky@         setll     pea01
     C     eaky@         reade(n)  pea01                                  60
     c                   When      GRPSELECT = *ON
     C     eaky$         setll     pea01
     C     eaky$         reade(n)  pea01                                  60
     c                   endsl
     c
     C*** Begin Chg *** 09/09/24 *******************************************
     C*****              If        *in60 = *off
     C                   Dow       *in60 = *off
     C*** End   Chg *** 09/09/24 *******************************************
     c
     c* Process only Collateral offset record (category 'G')
     c
     c                   if        eactyp = 'G'
     c
     c                   eval      eabat# = xxbat#
     c                   eval      eaamt  = eaamt * -1
     c                   eval      xxBata= xxBata + eaamt
     c                   eval      eatrdt = savedt
     c                   eval      eastat = 'T'
     c                   eval      eatfap = 'Y'
     c                   eval      eatffl = 'Y'
     C                   move      q1user        eauser
     C                   move      q1pgm         eapgm
     C                   move      q1user        eaiusr
     C                   move      q1pgm         eaipgm
     C                   move      dstime        eatime
     c                   write     pea01
     c
     c*Write WMEAP recoords and update batch log
     c
      *Get the A/R type
     C                   MOVEL     EASYCD        XXKEY
     c
     C     tbkey         Chain     PSY01                              50
     C                   if        *IN50 = '0' and SYSTAT  <> 'I'
     C                   MOVEL     SYTYPE        XXSYTP
     C                   endif
     c
     C                   Movel     EAARCD        XXKEY
      *
     C     tbkey         Chain     PTT01                              50
      *
     C                   if        *IN50 = '0' and TTSTAT  <> 'I'
     C                   Movel     TTTYPE        XXTYPE
      *
     C                   ELSE
     C                   MOVE      *BLANKS       XXTYPE
     C                   END
     C*
     C                   IF        XXSYTP = 'Y'
     C                   MOVE      XXTYPE        EAARTY
     C                   else
     C                   MOVE      ' '           EAARTY
     C                   endif
     C*
     c                   eval      eastat = 'A'
     c                   eval      eabul1= eaBat#
        EAninv = EAinv#;
     C*
     c                   write     pea05
     C*
     c                   endif                                                  eactyp = 'G'
     C*** Begin Chg *** 09/09/24 *******************************************
     c*****              endif
     c                   select
     c                   When      DIVSELECT = *ON
     C     eaky#         reade(n)  pea01                                  60
     c                   When      EMPSELECT = *ON
     C     eaky@         reade(n)  pea01                                  60
     c                   When      GRPSELECT = *ON
     C     eaky$         reade(n)  pea01                                  60
     C                   endsl
     c                   enddo
     C*** End   Chg *** 09/09/24 *******************************************
     c
     c                   endsr
     C*** End   Add *** 06/24/24 *******************************************
     c
     c*=====================================================================---*
     c* Procdelete - Delete the records based on selection
     c*=====================================================================---*
     c     procdelete    begsr
     C*
     C                   select
     C*
     c                   When      DIVSELECT = *ON
     C*
     c/exec SQL
     C+    delete from WDDAP  where DAFND = :xxfnd and DAEMP# = :xxemp#  and
     C+               DADIV  = :xxdiv  and DAPODT = :xxpodt
     c/end-exec
     C*** Begin Add *** 07/01/13 *******************************************
     c/exec SQL
     C+    delete from WDDCP  where DCFND = :xxfnd and DCEMP# = :xxemp#  and
     C+               DCDIV  = :xxdiv  and DCPODT = :xxpodt
     c/end-exec
     C***End    Add *** 07/01/13 *******************************************
     c                   eval      z = 0
     c                   dou       z = Xcounter
     C                   eval      z = ( z + 1 )
     C                   Eval      @batch# = xxbatch#(Z)
     c/exec SQL
     C+    delete from WDDDP  where DDFND = :xxfnd and DDEMP# = :xxemp#  and
     C+   DDDIV  = :xxdiv  and DDPODT = :xxpodt  and DDBAT# = :@batch#
     c/end-exec
     C
     C*** Begin Add *** 06/24/24 *******************************************
     c* Check if any link batch record exists for current batch#
      /Free
       xxlnkbat# = 0;
       Setll eaky1 pea04;
       ReadE(N) eaky1 pea04;
       Dow not %eof(wdeal4);
          If A_EAbat2 <> 0;
             xxlnkbat# = A_EAbat2;
             leave;
          Endif;
          ReadE(N) eaky1 pea04;
       Enddo;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     C
     C                   If        xxtrans#(Z) = *off
     C*** Begin Add *** 06/24/24 *******************************************
     c* Delete the link batch record and Collateral Transfer batch record
     c* if exists for current batch#
      /Free
       If xxlnkbat# <> 0;
          exec SQL
           delete from WDEAP  where EAFND = :xxfnd and EAEMP# = :xxemp#  and
           EADIV  = :xxdiv  and EAPODT = :xxpodt  and
           (EABAT# = :xxlnkbat# or EABAT# = :@batch#) and
           EACTYP = 'G';

          //*** Begin Add *** 09/09/24 *******************************************
          // Delete the link batch record and Collateral Transfer batch record
          // associated with other employers which provided Collateral to current Employer

          // Remove checking of fund because Collateral can come from Employer of different fund

          xxlike = *blanks;                                 // example- '%Employer%'
          xxlike = '%' + %char(xxemp#) + '%';               // example- '%10246%'
          exec SQL
           delete from WDEAP  where
           EADIV  = :xxdiv  and EAPODT = :xxpodt  and
           (EABAT# = :xxlnkbat# or EABAT# = :@batch#) and
           EAdesc like :xxlike  and EACTYP = 'G';
          //*** End   Add *** 09/09/24 *******************************************
       Endif;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     c/exec SQL
     C+    delete from WDEAP  where EAFND = :xxfnd and EAEMP# = :xxemp#  and
     C+   EADIV  = :xxdiv  and EAPODT = :xxpodt  and EABAT# = :@batch# and
     C+   EACTYP = 'R'
     c/end-exec
     c* Update the remaining batch amount for non -deleted records
     C*
     c                   exsr      updbata
     c                   else
     c
     C* Write an offset record
     c                   exsr      wrtar
     C*** Begin Add *** 06/24/24 *******************************************
     C* Write an offset record for the link batch record if exists for current batch#
      /Free
       If xxlnkbat# <> 0;
          Exsr wrtar2;
       Endif;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     c                   endif
     c
     C                   enddo
     c
     c                   When      EMPSELECT = *ON
     c/exec SQL
     C+    delete from WDDAP  where DAFND = :xxfnd and DAEMP# = :xxemp#  and
     C+                                    DAPODT = :xxpodt
     c/end-exec
     C*** Begin Add *** 07/01/13 *******************************************
     c/exec SQL
     C+    delete from WDDCP  where DCFND = :xxfnd and DCEMP# = :xxemp#  and
     C+                                    DCPODT = :xxpodt
     c/end-exec
     C*** End   Add *** 07/01/13 *******************************************
     c                   eval      z = 0
     c                   dou       z = Xcounter
     C                   eval      z = ( z + 1 )
     C                   Eval      @batch# = xxbatch#(Z)
     c/exec SQL
     C+    delete from WDDDP  where DDFND = :xxfnd and DDEMP# = :xxemp#  and
     C+     DDPODT = :xxpodt  and DDBAT# = :@batch#
     c/end-exec
     C
     C*** Begin Add *** 06/24/24 *******************************************
     c* Check if any link batch record exists for current batch#
      /Free
       xxlnkbat# = 0;
       Setll eaky1 pea04;
       ReadE(N) eaky1 pea04;
       Dow not %eof(wdeal4);
          If A_EAbat2 <> 0;
             xxlnkbat# = A_EAbat2;
             leave;
          Endif;
          ReadE(N) eaky1 pea04;
       Enddo;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     C
     C                   If        xxtrans#(Z) = *off
     C*** Begin Add *** 06/24/24 *******************************************
     c* Delete the link batch record and Collateral Transfer batch record
     c* if exists for current batch#
      /Free
       If xxlnkbat# <> 0;
          exec SQL
           delete from WDEAP  where EAFND = :xxfnd and EAEMP# = :xxemp#  and
           EAPODT = :xxpodt  and (EABAT# = :xxlnkbat# or EABAT# = :@batch#) and
           EACTYP = 'G';

          //*** Begin Add *** 09/09/24 *******************************************
          // Delete the link batch record and Collateral Transfer batch record
          // associated with other employers which provided Collateral to current Employer

          // Remove checking of fund because Collateral can come from Employer of different fund

          xxlike = *blanks;                                 // example- '%Employer%'
          xxlike = '%' + %char(xxemp#) + '%';               // example- '%10246%'
          exec SQL
           delete from WDEAP  where
           EAPODT = :xxpodt  and (EABAT# = :xxlnkbat# or EABAT# = :@batch#) and
           EAdesc like :xxlike  and  EACTYP = 'G';
          //*** End   Add *** 09/09/24 *******************************************
       Endif;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     c/exec SQL
     C+    delete from WDEAP  where EAFND = :xxfnd and EAEMP# = :xxemp#  and
     C+    EAPODT = :xxpodt  and EABAT# = :@batch#  and
     C+    EACTYP = 'R'
     c/end-exec
     c* Update the remaining batch amount for non -deleted records
     C*
     c                   exsr      updbata
     c                   else
     c
     C* Write an offset record
     c                   exsr      wrtar
     C*** Begin Add *** 06/24/24 *******************************************
     C* Write an offset record for the link batch record if exists for current batch#
      /Free
       If xxlnkbat# <> 0;
          Exsr wrtar2;
       Endif;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     c                   endif
     c
     C                   enddo
     c
     c                   When      GRPSELECT = *ON
     c/exec SQL
     C+    delete from WDDAP  where DAFND = :xxfnd  and
     C+                                    DAPODT = :xxpodt
     c/end-exec
     C*** Begin Add *** 07/01/13 *******************************************
     c/exec SQL
     C+    delete from WDDCP  where DCFND = :xxfnd  and
     C+                                    DCPODT = :xxpodt
     c/end-exec
     C*** End   Add *** 07/01/13 *******************************************
     c                   eval      z = 0
     c                   dou       z = Xcounter
     C                   eval      z = ( z + 1 )
     C                   Eval      @batch# = xxbatch#(Z)
     c/exec SQL
     C+    delete from WDDDP  where DDFND = :xxfnd   and
     C+     DDPODT = :xxpodt  and DDBAT# = :@batch#
     c/end-exec
     C
     C*** Begin Add *** 06/24/24 *******************************************
     c* Check if any link batch record exists for current batch#
      /Free
       xxlnkbat# = 0;
       Setll eaky1 pea04;
       ReadE(N) eaky1 pea04;
       Dow not %eof(wdeal4);
          If A_EAbat2 <> 0;
             xxlnkbat# = A_EAbat2;
             leave;
          Endif;
          ReadE(N) eaky1 pea04;
       Enddo;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     C
     C                   If        xxtrans#(Z) = *off
     C*** Begin Add *** 06/24/24 *******************************************
     c* Delete the link batch record and Collateral Transfer batch record
     c* if exists for current batch#
      /Free
       If xxlnkbat# <> 0;
          exec SQL
           delete from WDEAP  where EAFND = :xxfnd and
           EAPODT = :xxpodt  and (EABAT# = :xxlnkbat# or EABAT# = :@batch#) and
           EACTYP = 'G';

          //*** Begin Add *** 09/09/24 *******************************************
          // Delete the link batch record and Collateral Transfer batch record
          // associated with other employers which provided Collateral to current Employer

          // Remove checking of fund because Collateral can come from Employer of different fund

          exec SQL
           delete from WDEAP  where
           EAPODT = :xxpodt  and (EABAT# = :xxlnkbat# or EABAT# = :@batch#) and
           EACTYP = 'G';
          //*** End   Add *** 09/09/24 *******************************************
       Endif;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     c/exec SQL
     C+    delete from WDEAP  where EAFND = :xxfnd and
     C+    EAPODT = :xxpodt  and EABAT# = :@batch# and
     C+    EACTYP = 'R'
     c/end-exec
     c* Update the remaining batch amount for non -deleted records
     C*
     c                   exsr      updbata
     c                   else
     c
     C* Write an offset record
     c                   exsr      wrtar
     C*** Begin Add *** 06/24/24 *******************************************
     C* Write an offset record for the link batch record if exists for current batch#
      /Free
       If xxlnkbat# <> 0;
          Exsr wrtar2;
       Endif;
      /End-Free
     C*** End   Add *** 06/24/24 *******************************************
     c                   endif
     c
     C                   enddo
     C                   endsl
     c                   endsr
     C*================================================================
     C* selmode - Get the mode from user selection
     C*================================================================
     C     selmode       BEGSR

     C                   time                    dsdttm
     C                   SELECT
     C                   WHEN      QQDIV <> *BLANK
     C                   EVAL      DIVSELECT = *ON
     C
     C                   WHEN      XXEMP# <> 0
     C                   EVAL      EMPSELECT = *ON
     C
     C                   OTHER
     C                   EVAL      GRPSELECT = *ON
     C                   ENDSL

     C                   ENDSR
     C*================================================================
     C* UPDBATA - Update the Batch amount after the deductible deletion from WDEAP
     C*================================================================
     C     UPDBATA       BEGSR

     C                   Eval      XXBATA = 0

     C     eaky1         Chain     pea04                              60
     C                   Dow       *in60 = *off
     C                   Add       A_EAAMT       XXBATA
     C     eaky1         Reade     pea04                                  60
     C                   Enddo
     C
     C
     C     eaky1         Chain     pea04                              60
     C                   Dow       *in60 = *off
     C                   Z-add     XXBATA        A_EABATA
     c                   update    pea04
     C     eaky1         Reade     pea04                                  60
     C                   Enddo
     C
     C                   ENDSR
     c*=====================================================================---*
     c* getDDPCnt  - gets the count of WDDDP  records
     c*=====================================================================---*
     p getDDPCnt       B
     d
     d getDDPcnt       PI
     d   x1podt                       7  0
     d   x1div                        5  0
     d inputSrchCnd    s            256a
     d SqlStmtStr      s            256a
     d openArrChar     s              3a
     d openArray       s               n   dim(3)

     c*
     c                   if        xxFND  <> 0
     c                   eval      inputSrchCnd = 'DDFND = ? AND DDPODT = ?'
     c                   eval      openArray(1) = *on
     c                   Endif
     c*
     c                   if        xxEMP# <> 0
     c                   eval      inputSrchCnd = %trim(inputSrchCnd) +
     c                             ' and ' + 'DDEMP# = ?'
     c                   eval      openArray(2) = *on
     c                   Endif
     c
     c                   if        QQDIV <> ' '
     c                   eval      inputSrchCnd = %trim(inputSrchCnd) +
     c                             ' and ' + 'DDDIV  = ?'
     c                   eval      openArray(3) = *on
     c                   Endif
     c
     c/exec SQL
     c+                  declare DARTScursor cursor
     c+                    with Hold
     c+                    for dynSqlStmt
     c/end-exec
     c
     c                   eval      sqlStmtStr = 'Select DISTINCT(DDBAT#) +
     c                             from  WDDDP where ' + inputSrchCnd
     c
     c/exec SQL
     c+                  prepare   dynSqlStmt
     c+                    from :sqlStmtStr
     c/end-exec
     c*
     c* OPEN THE CURSOR DEPENDING ON SCREEN PARAMETERS
     c*
     c                   movea     openArray     openArrChar
     c*
     c                   select
     c*
     c*only fnd and Posting date
     c                   when      openArrChar = '100'
     c/exec SQL
     c+                  open      DARTScursor
     c+                  using     :xxfnd, :x1podt
     c/end-exec
     c*
     c*only fnd ,Emp and Posting date
     c                   when      openArrChar = '110'
     c/exec SQL
     c+                  open      DARTScursor
     c+                  using     :xxfnd, :x1podt, :xxemp#
     c/end-exec
     c*
     c*Fnd ,Emp, Division and Posting date
     c                   when      openArrChar = '111'
     c/exec SQL
     c+                  open      DARTScursor
     c+                  using     :xxfnd, :x1podt, :xxemp#, :x1div
     c/end-exec
     C                   ENDSL
     C
     C*First Time fetch the cursor
     C
     c/exec SQL
     c+                  fetch next
     c+                    from  DARTScursor
     c+                    into :XXBAT#
     c/end-exec
     c
     c                   Eval      Xcounter = 0
     C*
     C* Read cursor in loop
     C*
     C                   Dow       sqlstt = '00000'                             (Read was success)
     C*
     c                   Eval      Xcounter = Xcounter + 1
     C                   Eval      xxbatch#(Xcounter) = XXBAT#
     C*
     C     eaky          Chain     pea04                              60
     C                   IF        *in60 = *off
     C                   If        A_Eastat = 'T'
     C                   Eval      xxtrans#(Xcounter) = *on
     C                   else
     C                   Eval      xxtrans#(Xcounter) = *off
     C                   endif
     C                   endif
     c/exec SQL
     c+                  fetch next
     c+                    from  DARTScursor
     c+                    into :XXBAT#
     c/end-exec

     C*
     C                   enddo
     c/exec SQL
     C+                  Close DARTScursor
     c/end-exec

     p getDDPcnt       E
