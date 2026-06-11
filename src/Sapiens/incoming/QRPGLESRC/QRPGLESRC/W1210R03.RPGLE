     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h*============================================================================================*
      /title W1210R03 - W/W Auto Cash Dissolve
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     h*============================================================================================*
     h*  DESCRIPTION: Work with WZEAP (Auto Cash Dissolve Entries). After editing these records    *
     h*               are added to WDEAP.                                                          *
     h*                                                                                            *
     h*  CALLED FROM: W1210R02                                                                     *
     h*                                                                                            *
     h*  CALLS TO:                                                                                 *
     h*                                                                                            *
     h*  CREATED: 10/07/05  PROGRAMMER: C:136 PROJECT: WC1179                                      *
     h*============================================================================================*
     h*  REVISIONS:                                                                                *
     H*                                                                                            *
     H* 06/24/24 C:079 Project: 146WKR
     H*                Collateral Processing - Recomile
     h*
     h* 12/02/22 C:062 Project: WC2663
     h*                Assign invoice#
     h*                                                                                            *
     h*  01/28/13 C:282 Project: 133WOC                                                            *
     h*                          A/R History Function button to the screens.                       *
     h*                                                                                            *
     h*  12/03/10 C:136 Support: WA133 74239                                                       *
     h*                          Fix for Agent History edit should apply to KRMCA as well per GW   *
     h*                                                                                            *
     h*  09/20/10 C:136 Support: WA133 74239                                                       *
     h*                          xRef to 133WKA PRISS 22                                           *
     h*                          Temp fix for 70587                                                *
     h*                                                                                            *
     h*  02/25/10 C:220 Support:  70587                                                            *
     h*                           Adding edit in error check1 for the Posting Date                 *
     h*                                                                                            *
     h*  03/27/09 C:189 Support:  67784                                                            *
     h*                           Date validation not happening correctly resulting                *
     h*                           invalid posting date entering the system                         *
     h*                                                                                            *
     h*  01/18/07 C:136 Support:  WA091 56725                                                      *
     h*                 Edit invoking improperly if adding transaction to cash dissolve screen.    *
     h*                 Removed SCEMP# and SCDIV from SCR01S subfile. Also, allow user to delete   *
     h*                 entry from cash dissolve screen.                                           *
     h*                                                                                            *
     h*  02/21/06 C:136 SUPPORT: WA109 51569                                                       *
     h*                 Do not assign a transaction date unless one is assigned via Lockbox or     *
     h*                 ACH.                                                                       *
     h*============================================================================================*
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     fw1210f03  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     sfile(scr01s:sfrrn1)
     f                                     sfile(scr02s:sfrrn2)
     f                                     infds(fids01)
     f*
     f*  Physical file definition (for refreshing subfile record)
     fwzeap     uf a e             disk
     f                                     rename(pea01:pfrcdf)
     f*
     f*  Primary logical file definition (for displaying items)
     fwzeal     if   e           k disk
     f                                     rename(pea01:dbrcdf)
     f                                     infds(dbfds)
     f*
     f*
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwdeap     uf a e             disk    rename(pea01:dea0p) prefix(d)
     fwmahl     if   e           k disk
     f*** beg add ***** 01/28/13 **************************************
     fwmeml     if   e           k disk
     f                                     infds(emfds)
     f*** end add ***** 01/28/13 **************************************
     f*** Begin Add *** 12/02/22 **************************************
     Fwmeal8    if   e           k Disk    Rename(pea01:pea08) Prefix(L8)
     Fwdeal2    if   e           k Disk    Rename(pea01:pea02) Prefix(L2)
     f*** End   Add *** 12/02/22 **************************************
     d*================================================================
     d*  /COPY required constants
     d*================================================================
     d optnm           C                   const('A/R Batch Detail')
     d slmsgi          C                   const('WC39998')
     d slmsgf          C                   const('WMESGG')
     d acdpgm          C                   const('NOPGM')
     d comapi          C                   const('WA001R')
     d uspapi          C                   const('W0006R')
     d sf1pag          C                   const(12)
     d deletd          C                   const('*Deleted')
     d*================================================================
     d*  stand alone fields - top
     d*================================================================
     d*  Data structures for SCDATA
     d                 DS
     d  XXDATA                 1     34
     d  XXHIER                 1      3  0
     d  XXCTYP                 8      8
     d  XXDESC                15     34
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
     d*** beg add ***** 01/28/13 **************************************
     d EMfds           ds
     d  EMrrn#               397    400B 0
     d*** beg add ***** 01/28/13 **************************************

     d*** Beg Add *** 12/02/22 ****************************************
     d intInv#         s                   Like(DEAinv#)
     d isInvoiceCloud  s              1n   Inz
     d isICgroup       s              1n   Inz
     d*** End Add *** 12/02/22 ****************************************
     d kkco#           s                   like(EACO#)
     d kkfnd           s                   like(EAFND)
     d kkemp#          s                   like(EAEMP#)
     d kkdiv           s                   like(EADIV)
     d kkfyr           s                   like(EAFYR)
     d kkpodt          s                   like(EAPODT)
     d kkarcd          s                   like(EAARCD)
     d emrrn           s              9  0
     d elrrn           s              9  0
     d totAmt          s                   like(DEAAMT) inz(0)
     d tfrAmt          s                   like(DEAAMT) inz(0)
     d arType          s              1
     d key             s             10
     d errFlg          s              1
     d sysTime         s                   like(DEATIME)
     d sysDate         s                   like(DEADT)
     d svTrdt          s                   like(DEADT)
     d achFlg          s              1
     d*** beg add ***** 01/28/13 **************************************
     d intEMrrn        s              9  0
     d charCall        s              1
     d*** beg add ***** 01/28/13 **************************************

     d hd              s             78    dim(1) ctdata perrcd(1)              Column headings
     d*** beg add ***** 01/28/13 **************************************
     d**** of              s             1    dim(10) ctdata perrcd(1)             Options, Function
     d**** ofd             s             35    dim(10) alt(of)
     d of              s              1    dim(11) ctdata perrcd(1)             Options, Function
     d ofd             s             35    dim(11) alt(of)
     d*** end add ***** 01/28/13 **************************************
     d*================================================================
     d*  stand alone fields - bot
     d*================================================================
     d/copy wpremplr
     d/copy wprgroup
     d/copy wprar
     d/copy sprdate
     c*** Beg Add *** 12/02/22 ***************************************
     d/Copy wPrPmtSvc
     d/Copy sPrModule
     c*** End Add *** 12/02/22 ***************************************
     c/copy scopyww02c
     c*================================================================
     c*  PLIST definitions
     c*================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc              1
     c                   parm      *inkl         @@kl              1
     c                   parm                    @@call            1
     c                   parm                    @@mode            1
     c                   parm                    @@rrn             9 0
     c*
     c*  Add/change/display program parameter list
     c*
     c     acdpl         plist
     c     *inkc         parm                    ##kc              1
     c     *inkl         parm                    ##kl              1
     c                   parm                    ##mode            1
     c                   parm      scdbrr        ##rrn             9 0
     c*
     c*  Company file/fund (group) retrieve program parameter list
     c*
     c     compl         plist
     c                   parm                    ##co#             3 0
     c                   parm                    ##fnd             3 0
     c                   parm                    ##conm           40
     c                   parm                    ##fnam           40
     c*
     c*
     c     dtcymd2       plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*================================================================
     c*  KLIST definitions
     c*================================================================
     c     pageKy        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c                   kfld                    kkpodt
     c                   kfld                    kkarcd

     c     readKy        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c*
     c*
     c     AHKY          KLIST
     c                   KFLD                    ##CO#
     c                   KFLD                    SCFND
     c                   KFLD                    SCEMP#
     c                   KFLD                    SCDIV
     c                   KFLD                    SCFYR
     c*
     c     AHKY1         KLIST
     c                   KFLD                    ##CO#
     c                   KFLD                    SCFND
     c                   KFLD                    SCEMP#
     c                   KFLD                    SCDIV
     c                   KFLD                    SCFYR
     c                   KFLD                    KKTRDT            7 0
     c*
     c*** beg add ***** 01/28/13 **************************************
     c     keyWMEML      Klist
     c                   Kfld                    kkco#
     c                   Kfld                    kkfnd
     c                   Kfld                    kkemp#
     c                   Kfld                    kkdiv
     c*** end add ***** 01/28/13 **************************************
     c*================================================================
     c* ERRCKA - Error check all subfile records (entire screen)
     c*================================================================
     c     errcka        begsr
      *
      *  Total amount should equal zero.  The total of the payment buckets should equal the transfer
      *  amount.
      *
     c                   if        totAmt <> DEACKAM
     c                   seton                                        75  90
     c                   endif

     c                   endsr
     c*================================================================
     c*
     c*================================================================
     c     errckc        begsr
     c                   eval      totAmt = 0
     c                   eval      tfrAmt = 0
     c                   endsr
     c*================================================================
     c* ERRCK1 - Additional errors on subfile selection
     c*================================================================
     c     errck1        begsr
      *
      *   Validate A/R Code
      *
     c                   if        scarcd <> *blank and
     c                             (scdesc = *blank or scfyr = *zero or
     c                              scpodt = *zero or scamt = 0)
     c                   seton                                        76  90
     c                   endif

     c                   if        scarcd <> *blank

     c                   eval      arType = GetARType(scarcd)
     c                   if        ValidCode(scarcd) = '0' or
     c                             (arType <> 'P' and arType <> 'I' and
     c                              arType <> 'Q')
     c                   seton                                        71  90
     c                   endif
      *
      *   Validate Year
      *
     c  n90              if        ValidEmpYr(scfyr:scemp#:scdiv:elrrn:scfnd)
     c                             = '0'
     c                   seton                                        72  90
     c                   endif
      *
      *   Validate posting date
      *
     c     datchk        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      *blanks       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1
     c
     c                   eval      srdti = scpodt
     c                   clear                   srerr
     c
     c                   call      'SRCVTDAT'    datchk
     C  n90              If        srerr = '1'
     c                   seton                                        73  90
     c                   endif
      *
      *   Validate Amount
      *
     c  n90              if        scamt = 0
     c                   seton                                        74  90
     c                   endif
      *
      *   Total screen amounts to display possible error in ERRCKA.
      *
     c                   if        scamt > 0
     c                   eval      totAmt = totAmt + scamt
     c                   endif

     c                   else

     c                   if        scdesc <> *blank or
     c                             scfyr  <> *zero  or
     c                             scpodt <> *zero  or
     c                             scamt  <> *zero
     c                   seton                                        76  90
     c                   endif

     c                   endif
      *
      *
      *  Employer must have assigned Agent effective for the fund
      *  year effective as of the Transaction Date
      *
     c                   if        not(*in90)
     c                   if        svtrdt = *zero
     c                   eval      kktrdt = xxdt
     c                   else
     c                   eval      kktrdt = svtrdt
     c                   endif

     c     ahky1         setgt     pah01
     c     ahky          readpe    pah01
     c                   dow       not(%eof) and
     c                             AHSTAT = 'I'
     c     ahky          readpe    pah01
     c                   enddo

     c                   if        %eof

     c                   if        scarcd <> *blank or
     c                             scpodt <> *zero  or
     c                             scamt  <> *zero

     c                   if        svtrdt = *zero
     c                   seton                                        8090
     c                   else
     c                   seton                                        6990
     c                   endif                                                  svtrdt
     c*
     c                   endif                                                  scarcd
     c                   endif                                                  %eof
     c                   endif                                                  *in90

     c                   endsr
       //*** Beg Add *** 12/02/22 *************************************
       //==============================================================
       // SRfindInv# - Look for invoice number in WMEAP and WDEAP
       //==============================================================
       Begsr SRfindInv#;

          intInv# = 0;
          // Check WMEAP
          Setll (DEAco#:DEAfnd:DEAfYr:DEAemp#:DEAdiv:DEApoDt) pea08;
          Reade (DEAco#:DEAfnd:DEAfYr:DEAemp#:DEAdiv:DEApoDt) pea08;
          Dow not %Eof(WMEAL8);
             If L8EAnInv > 0;
                intInv# = L8EAnInv;
                Leave;
             Endif;
             Reade (DEAco#:DEAfnd:DEAfYr:DEAemp#:DEAdiv:DEApoDt) pea08;
          Enddo;

          // Check WDEAP
          If intInv# = 0;
             Setll (DEAco#:DEAfnd:DEAfYr:DEAemp#:DEAdiv:DEApoDt) pea02;
             Reade (DEAco#:DEAfnd:DEAfYr:DEAemp#:DEAdiv:DEApoDt)
             pea02;
             Dow not %Eof(WDEAL2);
                If L2EAinv# > 0;
                   intInv# = L2EAinv#;
                   Leave;
                Endif;
                Reade (DEAco#:DEAfnd:DEAfYr:DEAemp#:DEAdiv:DEApoDt) pea02;
             Enddo;
          Endif;

       Endsr;
       //*** End Add *** 12/02/22 *************************************
     c*================================================================
     c* KEYINZ - Initialize key values for program start
     c*================================================================
     c     keyinz        begsr
     c     @@rrn         chain     dea0p
     c                   eval      kkco#  =  ##co#
     c                   eval      kkfnd  =  DEAFND
     c                   eval      kkemp# =  DEAEMP#
     c                   eval      kkdiv  =  DEADIV
     c                   eval      kkfyr  =  *loval
     c                   eval      kkpodt =  *loval
     c                   eval      kkarcd =  *loval
     c                   eval      achFlg = DEAACHF
     c                   eval      svTrdt = DEATRDT
     c                   endsr
     c*================================================================
     c* KEYSET - Set key values when doing "position to"
     c*================================================================
     c     keyset        begsr
     c                   endsr
     c*================================================================
     c* KEYSFL - Set key values from subfile records for paging
     c*================================================================
     c     keysfl        begsr
     c                   eval      kkfyr  =  scfyr
     c                   eval      kkpodt =  dtCYMD(scpodt)
     c                   eval      kkarcd =  scarcd
     c                   endsr
     c*** beg add ***** 01/28/13 **************************************
     c*===============================================================
     c* KIDO - SUBROUTINE FOR F09
     c*===============================================================
      /Free
       BegSr KIdo;

       // Retrieve the rrn from Employer Master File and pass in A/R Historical Balance.
          Chain keyWMEML  pem01;
          If %Found(WMEML);
             intEMrrn = EMrrn#;
             charCall = '2';
       // Call the A/R Historical Balance Program
             ARHistBal(charCall:*Inkc:*Inkl:intEMrrn);
             Write  scr02;
          EndIf;                                                          // %Found(WMEML)

       EndSr;

      /End-Free
     c*** end add ***** 01/28/13 **************************************
     c*===============================================================
     c* KJDO - SUBROUTINE FOR F10
     c*===============================================================
     c     kjdo          begsr

     c     hrrn          chain     scr01s
     c                   if        hfld = 'SCARCD'
     c                   eval      key = ARCodeSrch(*inkc:*inkl)
     c
     c                   if        key <> *blanks
     c                   eval      scarcd = key
     c                   eval      *in82 = *on
     c                   eval      *in08 = *on
     c                   update    scr01s
     c                   else
     c                   eval      *in81 = *on
     c                   eval      *in08 = *on
     c                   update    scr01s
     c                   endif

     c                   eval      scrrn1 = hrrn
     c                   write     scr02

     c                   endif

     c                   if        hfld  = 'SCFYR'
     c                   eval      elrrn = 0
     c                   eval      elrrn = EmpYrSrch(*inkc: *inkl: errFlg:
     c                                               DEAEMP#: DEADIV: DEAFND)
     c
     c                   if        elrrn <> *zero
     c     elrrn         chain     pel0p
     c                   eval      scfyr = elfyr
     c                   eval      *in84 = *on
     c                   eval      *in08 = *on
     c                   update    scr01s
     c                   else
     c                   eval      *in83 = *on
     c                   eval      *in08 = *on
     c                   update    scr01s
     c                   endif

     c                   eval      *in08 = *off
     c                   eval      scrrn1 = hrrn
     c                   write     scr02

     c                   endif

     c                   endsr
     c*================================================================
     c* MVDBSC - Move data base fields to screen fields
     c*================================================================
     c     mvdbsc        begsr
     c                   eval      scarcd = EAARCD
     c                   eval      scdesc = GetARDesc(EAARCD)
     c                   eval      scpodt = DtMDYY(EAPODT)
     c                   eval      scfyr  = EAFYR
     c                   eval      scamt  = EAAMT
     c                   eval      *in08  = *on
     c                   endsr
     c*================================================================
     c* MVSCDB - Move screen fields to data base fields
     c*================================================================
     c     mvscdb        begsr
     c                   eval      EAARCD =  scarcd
     c                   if        scdesc = *blank
     c                   eval      EADESC = GetARDesc(scarcd)
     c                   else
     c                   eval      EADESC =  scdesc
     c                   endif
     c                   eval      EAPODT =  DtCYMD(scpodt)
     c                   eval      EAFYR  =  scfyr
     c                   eval      EAAMT  =  scamt
     c                   endsr
     c*================================================================
     c* OTHFKS - Process other function keys
     c*================================================================
     c     othfks        begsr
      *** beg add ***** 01/28/13 ***********************************************
     c     *inki         caseq     *on           kido                     90
      *** end add ***** 01/28/13 ***********************************************
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     c                   endsr
     c*================================================================
     c* OTHOPT - Process other options
     c*================================================================
     c     othopt        begsr
     c                   endsr
     c*================================================================
     c* PGMEND - End of program processing
     c*================================================================
     c     pgmend        begsr

     c                   if        not *inkc and
     c                             not *inkl

     c                   eval      sysDate = DateTo7(SystemDate)
     c                   eval      sysTime = TimeTo6(SystemTime)
     c     1             setll     pfrcdf
     c                   read      pfrcdf
     c                   dow       not %eof
     c                   eval      DEAARCD = EAARCD
     c                   eval      DEADESC = EADESC
     c                   if        achFlg  = 'L'
     c                   eval      DEATRDT = svTrdt
     c                   endif
     c                   eval      DEAFYR  = EAFYR
     c                   eval      DEAPODT = EAPODT
     c                   eval      DEAAMT  = EAAMT
     c                   eval      DEACASH = 'D'
     c*** Begin Add *** 12/02/22 *******************************************
       //*** Beg Add *** 03/25/21 ************************************
       // If invoice number = 0 then look in WDEAP and WMEAP
       isICgroup = IcGroupOpt(EAco#:EAfnd);
       If DEAinv# = 0 and isInvoiceCloud and isICgroup;
          Exsr SRfindInv#;
          DEAinv# = intInv#;
          intInv# = 0;
       Endif;
       //*** End Add *** 03/25/21 ************************************
     c*** End   Add *** 12/02/22 *******************************************
     c                   eval      DEACTYP = GetARCat(EAARCD)
     c                   eval      DEAIDT  = sysDate
     c                   eval      DEAIUSR = Q1USER
     c                   eval      DEAIPGM = Q1PGM
     c                   eval      DEADT   = sysDate
     c                   eval      DEATIME = sysTime
     c                   eval      sysTime = sysTime + 1
     c                   eval      DEAUSER  = Q1USER
     c                   eval      DEAPGM  = Q1PGM
     c                   if        EAFYR <> *zero and
     c                             EAPODT <> *zero and
     c                             EADESC <> *blanks and
     c                             EAARCD <> *blanks
     c                   write     dea0p
       //*** Beg Add *** 12/02/22 ************************************
          DEAinv# = 0;
       //*** End Add *** 12/02/22 ************************************
     c                   endif
     c                   read      pfrcdf
     c                   enddo

     c     @@rrn         chain     dea0p
     c                   delete    dea0p
     c                   endif

     c                   seton                                        lr

     c                   endsr
     c*================================================================
     c* PGMSTR - Start of program processing
     c*================================================================
     c     pgmstr        begsr
     c                   call      'RTVCLTID'
     c                   parm                    cltID             3
     c                   eval      scfnd  = DEAFND
     c                   eval      scemp# = DEAEMP#
     c                   eval      scdiv  = DEADIV
     c                   eval      scfnam = GetGrpName( DEAFND )
     c                   eval      scedn  = EmpDivName( DEAEMP#:
     c                                                  DEADIV :
     c                                                  emrrn  :
     c                                                  DEAFND  )
     c                   eval      scchk# = DEACHK#
     c                   eval      scckam = DEACKAM
     c                   eval      scbat# = DEABAT#
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd2
     c                   movel     SRDTO         xxdt              7 0
       //*** Beg Add *** 12/02/22 *************************************
       isInvoiceCloud = isModEnabled('Invoice Cloud');
       //*** End Add *** 12/02/22 *************************************
     c                   endsr
     c*================================================================
     c* READ   - Read next record from file
     c*================================================================
     c     read          begsr
     c     readKy        reade     dbrcdf                                 95
     c                   endsr
     c*================================================================
     c* READP  - Read previous record from file
     c*================================================================
     c     readp         begsr
     c     readKy        readpe    dbrcdf                                 95
     c                   endsr
     c*================================================================
     c* SETGT  - Set greater than on file
     c*================================================================
     c     setgt         begsr
     c     pageKy        setgt     dbrcdf                             96
     c                   endsr
     c*================================================================
     c* SETLL  - Set lower limits on file
     c*================================================================
     c     setll         begsr
     c     pageKy        setll     dbrcdf                             96
     c                   if        *in96 = *on
     c                   eval      dbrrn = 0
     c                   endif
     c                   endsr
     c*================================================================
     c* SF1UPD - Update subfile record when item is deleted
     c*================================================================
     c     sf1upd        begsr
     c                   endsr
     c*================================================================
     c* UPDATR - Update data base record and after processing
     c*================================================================
     c     updatr        begsr

     c     scdbrr        chain     pfrcdf
     c                   if        %found
     c                   exsr      mvscdb
     c                   update    pfrcdf
     c                   endif

     c                   endsr
     c*================================================================
     c* WRITER - Write data base record and after processing
     c*================================================================
     c     writer        begsr
     c                   clear                   pfrcdf
     c                   exsr      mvscdb
     c                   write     pfrcdf
     c                   endsr
     c*================================================================
** Column headings
A/R Code  Description                     Year   Posting Date        Amount
** Description--------------MISN
1011=Select                 NNN
1022=change                 NNN
1055=display                NNN
12020=Change Status         NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
209F9=A/R History           YNN                                                 // 01/28/13
210F10=Search               YNN
212F12=Cancel               YYN
217F17=Subset               NNN
3
