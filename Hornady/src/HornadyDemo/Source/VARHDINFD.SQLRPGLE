     H COPYRIGHT ('Varsity Logistics, Inc.  San Francisco, CA  2003')
     H DFTACTGRP (*NO) ACTGRP('HDSOPT') BNDDIR('VARHDINFD')
     H OPTION(*SrcStmt: *NoDebugIO)
     **********************************************************************************************
      * Program Name: VARHDINFD
      * Program Author: BRIAN LARSON
      * Program Date: Apr. 28, 2008
     *
     * Program Description:  Interface between Harris Data and Varsity Shipsoft.
     *
     **********************************************************************************************
     * Modification Log:
     *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     *  Programmer:        Date:       Purpose:
     *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      *  MCA                09/08/2008  Update ship-via description when ship-via code changed.
      *                                 Update ship-via fields in detail when header changed.
      *  MCA                05/12/2009  Change to sequentially number shipments (instead of
      *                                 always using xxxxxx-0000001).
      *                                 If re-shipping, remove prior Varsity detail.
      *  MCA                10/04/2010  Flag ammo shipment for "adult signature required".
      *  MCA                10/04/2010  Fix Brian's bad logic for retrieving order detail.
      *  MCA                10/07/2010  Fix Ammo dangerous goods logic to allow HAZ rate selection.
      *  MCA                10/07/2010  Fix logic to remove VPPACK records from prior shipment on
      *                                 reship.
      *  MCA                10/07/2010  Because Brian's logic was fixed on 10/4/2010, 'HAZ' started
      *                                 working.  It should not be selected.
      *  MCA                10/19/2010  Remove P.O. boxes from address.
      *  MCA                06/03/2013  Maintain turnaround number separation.
      *  MCA                01/29/2015  Fix bug - need to ensure correct OEHDWK record is read and
      *                                 updated.  Changed to allow order control number to be passed
      *                                 and used for OEHDWK retrieval.
      *  MCA                09/14/2017  Varsity 2.70 upgrade to 3.50.
      *  MCA                09/27/2017  Print Home Depot PO# and store number on UPS label.
      *  MCA                10/04/2017  Varsity FedEx changes (initial FedEx install).
      *  MCA                10/09/2017  Fix bug - when order numbers grew to 7 digits, it messed
      *                                 up the shipment number determination.
      *  MCA                10/13/2017  Fix bug - 2nd try.
      *  MCA                02/04/2019  Handle Hornady West user names.
      *  MCA                09/16/2021  Firearms shipments.
keb01 *  KEB                06/09/2022  Add Number of Boxes.
keb02 *  KEB                06/14/2022  Print PO Box for USPS Only
keb03 *  KEB                06/20/2022  If Ship Via code changes pass
      *                                 back to Calling Program
      *  MCA                01/25/2024  Fix bug. File GUPTDAT was added as part of change
      *                                 made 2/4/2019.  It was defined as an update/add file,
      *                                 but should have been defined as an input file.
      *  MCA                03/06/2024  When printing only PO boxes for USPS, if any address
      *                                 line is cleared, move address lines up.  Note: This
      *                                 code will also work to squish addresses with missing
      *                                 address lines in the master file.
     *
kb04  *  KEB                04/22/2026  Add Costco account number to ship Costco Dropship
     *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     *--ORDER ENTRY HEADER----------------------------------------------
     Foeorhd    uf   e           k disk
     *--ORDER ENTRY COMMENt---------------------------------------------
     Foehdwk99  uf   e           k disk
     *--ORDER Detail----------------------------------------------------
     Foeordt    if   e           k disk
     *--Order Detail Logical--------------------------------------------
     F***oeordt01  if   e           k disk    rename(oeordtr:orlogr)
     F***                                     prefix(AK:2)
     *--ORDER Detail Work File------------------------------------------
     Foedtwk    uf   e           k disk
     *--ORDER WORK FILE-------------------------------------------------
     Foecmwk    if   e           k disk
     *--FREIGHT CLASS FILE----------------------------------------------
     FHDFTCL    if   e           k disk
     *--ITEM WAREHOUSE FILE---------------------------------------------
     FHDIWHS    if   e           k disk
     *--ITEM Master file..................------------------------------
     FHDIMST    if   e           k disk
     *--SHIP VIA Master file..............------------------------------
     FHDSHPV    if   e           k disk    usropn
     *--Varsity Shipping Instruction File-------------------------------
     Fctrlsi    if   e           k disk
     *--Varsity Shipping Instruction File-------------------------------
     Fctrlsil1  if   e           k disk    rename(ctrlsir:logr)
     F                                     prefix(L:1)
     *--Varsity Carrier/Service File------------------------------------
     Fctrlcc    if   e           k disk
     *--Varsity Shipper Control File (User/Group ID) -------------------
     Fctrlgilu  if   e           k disk
     *--Varsity Manifest Header File------------------------------------
     Fmfh1mh    uf   e           k disk
     *--Varsity Manifest Header History File----------------------------
     Fmfhhmh    if   e           k disk    prefix(M2:2)
     *--Varsity Manifest Detail File------------------------------------
     Fmfd1md    if   e           k disk
     *--DROP SHIP FILE--------------------------------------------------
     Fhddshp03  if   e           k disk
     *--CUSTOMER MASTER FILE IN HD--------------------------------------
     Fhdcust    if   e           k disk
     *--Master Location File--------------------------------------------
     Fhdlctn    if   e           k disk
     *--Varsity Packer Detail File--------------------------------------
     Fvpdetll2  uf   e           k disk    usropn
     *--Varsity Container File------------------------------------------
     Fvpcont    uf a e           k disk
     *--Varsity Pack File-----------------------------------------------
     Fvppack    uf a e           k disk
     *--Hornady generic table file -------------------------------------
     Fguptdat   if   e           k disk
     *--Display Screen--------------------------------------------------
    Fvarhddf   cf   e             workstn
     *
      /COPY QPRPSRC,HYR9912

     D spcladdress     PR             1A
     D   address                     26A     VALUE

     D QMHSNDPM        PR                  EXTPGM('QMHSNDPM')                   Send program message
     D  out_MsgID                     7A   CONST
     D  out_MsgFile                  20A   CONST
     D  out_MsgData                 512A   CONST
     D  out_MsgDtaLen                10I 0 CONST
     D  out_MsgType                  10A   CONST
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  in_MsgKey                     4A
     D  io_MsgErrCode                10I 0

     *
     *----------------------------------------------------------------
     *  Data structures.
     *------------------------------------------------------------------
     d plist1        e ds                  extname(plist1ds) inz
     d oeparm        e ds                  extname(oedata)
     *
     d psds           sds
     d  @jobnam              244    253
     d  @usrnam              254    263
     d  @pgmnam              324    333
     *
     d                 ds
     d phone#                  1     15
     d  char1                  1      1
     d  filler                 2     15
     d
     d                 ds
     d tzip                    1      7
     d  tzip1                  1      3
     d  tzip2                  5      6
     d  tzip3                  7      7
     *
     d                 ds
     d ds_shipment#            1     20
     d ds_hyphen               7      7
     d ds_hyphen2              8      8
     d ds_shipseq#x            8     14
     d ds_shipseq#             8     14  0
     d ds_shipseq#x2           9     15
     d ds_shipseq#2            9     15  0
     *--------------------------------------------------------------------
     *  Alphanumeric Define Fields
     *--------------------------------------------------------------------
     d visible         s              1
     d shipvia         s             15
     d tmp18           s             18
     d hldphone        s             11
     d**** hldord          s              8
     d endloop         s              1
     d endprog         s              1    inz('N')
     d ordershp        s              1
     d dltflag         s              1
     d ordkey          s             20
     d ordtype         s              1    inz
     d comp#           s              4    inz
     d doctyp          s              3    inz('INV')
     d varsity         s              2
     d hldtrknum       s             25
     d pkptyp          s              5
     d pord#           s              8  0
     d pvarord#        s             10
     d poctl           S              8  0
     d pordin          s              8
     d pvarordin       s             10
     d poctlin         S              8
     d wk_shipseq#     s              7  0
     D wk_HypPos       S              3  0
     D wk_HypPosSv     S              3  0
     D wk_HypPosStr    S              3  0
     D wk_shipseq#x    S              7
     D wk_shipseq#chk  S              7  0
     D wk_DspMsg       S             52
     d wk_CommCode     s             12
     d wk_Item         s             15
     d wk_ProdClass    s              4
     d wk_DftPhone#    s             15    inz('3083821390')
     d wk_GunCnt       s              7P 0
     d wk_VarUser      s             10
     d wk_DatabaseID2  s              2
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     d sv_svsv         s                   like(h1svsv)
     d kf_TABL         s                   like(TDTABL)
     d kf_KEY1         s                   like(TDKEY1)
     d kf_KEY2         s                   like(TDKEY2)
keb01d wk_pkgcnt       s                   like(ptpkg)
keb01d Wk_Cont         s                   like(cxcnid)
keb01d Wk_Ship         s                   like(cxship)
keb01d Turn#           s              9
keb01d OrderType       s              2
keb01d OptOrd          s                   like(pordr)
keb01d Suffix          s              4
keb01d OrderNum        s                   like(pordr)
keb01d OrdPos          s              2  0
keb02d PrtPOBox2       s              1
keb02d PrtPOBox3       s              1
keb02d PrtPOBox4       s              1
keb02d @yes            s              1    inz('Y')
keb02d @no             s              1    inz('N')
keb03d parm_svsv       s                   like(h1svsv)
keb03d parm_svds       s                   like(h1svds)
kb04 d Costco          s              1
     *--------------------------------------------------------------------
     *  Numeric Define Fields
     *--------------------------------------------------------------------
     d pos             s              1  0
     d x               s              3  0
     d y               s              3  0
     d boxes           s              7  0
     d custchg         s              7  2
     d delchg          s              7  2
     d ordlin#         s              3  0 inz(999)
     d blkrel          s              3  0 inz(0)
     d UTIME           s              6  0
     *--------------------------------------------------------------------
     *  Constants
     *--------------------------------------------------------------------
     d yes             c                   const('Y')
     d no              c                   const('N')
     d digits          c                   const('0123456789')
     D at_Attention    C                   CONST('A')                           Addr type: Attention
     D at_POBox        C                   CONST('P')                           Addr type: P.O. Box
     D at_Spacer       C                   CONST('S')                           Addr type: Spacer
     D at_AKA          C                   CONST('K')                           Addr type: AKA
     D at_CareOf       C                   CONST('C')                           Addr type: Care of
     *--------------------------------------------------------------------
     *  Arrays
     *--------------------------------------------------------------------
     d**** hldord#         s              1    dim(8)
     d msg             s             50    dim(3) ctdata perrcd(1)
     d track#          s             25    dim(100)
     *--------------------------------------------------------------------
     *  Data Areas.
     *--------------------------------------------------------------------
     d wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     *--------------------------------------------------------------------
     *  Main processing
     *--------------------------------------------------------------------
     c                   in        wk_DatabaseID
     c                   select
     c                   when      wk_DatabaseID = 'HY'  or
     c                               wk_DatabaseID = 'XX'
     c                   eval      wk_DatabaseID2 = 'HY'
     c                   when      wk_DatabaseID = 'SS'  or
     c                               wk_DatabaseID = 'SX'
     c                   eval      wk_DatabaseID2 = 'SS'
     c                   when      wk_DatabaseID = 'PV'  or
     c                               wk_DatabaseID = 'PX'
     c                   eval      wk_DatabaseID2 = 'PV'
     c                   other
     c                   eval      wk_DatabaseID2 = wk_DatabaseID
     c                   endsl

     c                   dow       endprog = no
     c                   exsr      $clrscreen
     c                   enddo
     *
     c                   eval      *inlr = *on
     *--------------------------------------------------------------------
     *  Clear Interface Screen
     *--------------------------------------------------------------------
     c     $clrscreen    begsr
     *
     c                   clear                   @order
     c                   clear                   @carcd
     c                   clear                   @srvtp
     c                   clear                   @svdsc
     c                   clear                   @pkcnt
     c                   clear                   @pkprc
     c                   clear                   @pkgwgt
     c                   clear                   @totwgt
     c****               clear                   hldord
     c****               clear                   hldord#
     c                   clear                   plist1
     c                   movel     pordin        pord#
     c                   eval      pvarord# = pvarordin
     C                   movel     poctlin       poctl
     c                   eval      visible = 'N'
     c                   eval      endloop = 'N'
     c                   eval      *in75 = *off
     *
     c                   exsr      $getorder
     *
     c                   endsr
     *--------------------------------------------------------------------
     *  Get the order information
     *--------------------------------------------------------------------
     c     $getorder     begsr
     *
     c     pord#         chain(n)  oeorhd
     c                   if        %found = *on
     c                   eval      @order = oeord#
kb04                     if oeblto = 863033;
kb04                        Costco = @yes;
kb04                     else;
kb04                        Costco = @no;
kb04                     endif;
     *
     c     oeloc#        chain     hdlctn
     c                   if        %found = *on
     c                   movel     loco#         comp#
     c****               movel     oeord#        hldord
     c****               movea     hldord        hldord#
     c****               clear                   hldord
     c****               do        8             pos
     c****               if        hldord#(pos) > '0'
     c****               leave
     c****               else
     c****               if        hldord#(pos) = '0'
     c****               eval      hldord#(pos) = *blank
     c****               endif
     c****               endif
     c****               enddo
     c****               movea     hldord#       hldord
     c****               eval      ordkey = %triml(hldord)
     c                   eval      ordkey = pvarord#
     c                   eval      ordershp = no
     c     comp_typ_ord  chain(n)  mfh1mh
     c                   if        %found = *on
     c                   eval      visible = yes
     c                   eval      ordershp = yes
     c                   eval      ds_shipment# = MHSHMT
     c                   if        ds_hyphen = '-'
     c                   if        %check(digits:ds_shipseq#x) = *zero
     c                   eval      wk_shipseq# = ds_shipseq#
     c                   else
     c                   eval      wk_shipseq# = 1
     c                   endif                                                  %check(digits:ds_shi
     c                   else
     c                   if        %check(digits:ds_shipseq#x2) = *zero
     c                   eval      wk_shipseq# = ds_shipseq#2
     c                   else
     c                   eval      wk_shipseq# = 1
     c                   endif                                                  %check(digits:ds_shi
     c                   endif                                                  ds_hyphen='-'
     c                   eval      wk_HypPosSv = *Zero
     c                   eval      wk_HypPosStr = 1
     c                   dou       wk_HypPos = *Zero
     c                   eval      wk_HypPos = %scan('-':MHSHMT:wk_HypPosStr)
     c                   if        wk_HypPos > *zero
     c                   eval      wk_HypPosSv = wk_HypPos
     c                   eval      wk_HypPosStr = wk_HypPos + 1
     c                   endif                                                  wk_HypPosSv>*Zero
     c                   enddo                                                  wk_HypPos=*Zero
     c                   eval      wk_HypPos = wk_HypPosSv
     c                   eval      wk_shipseq#x = %subst(MHSHMT:wk_HypPos+1:7)
     c                   if        wk_HypPos > *Zero  and
     c                               %check(digits:wk_shipseq#x) = *zero
     c                   eval      wk_shipseq#chk = %dec(wk_shipseq#x:7:0) + 1
     c                   else
     c                   eval      wk_shipseq#chk = 1
     c                   endif                                                  wk_HypPos>*Zero and
     c                   if        wk_shipseq#chk <> wk_shipseq#
     c                   eval      wk_DspMsg = 'VAR1- Order#: ' + ordkey
     c     wk_DspMsg     dsply(E)  'BEARL'
     c                   eval      wk_DspMsg = 'VAR1- wk_shipseq#: ' +
     c                               %editc(wk_shipseq#:'X') +
     c                               '  wk_shipseq#chk: ' +
     c                               %editc(wk_shipseq#chk:'X')
     c     wk_DspMsg     dsply(E)  'BEARL'
     c                   eval      wk_shipseq# = wk_shipseq#chk
     c                   endif                                                  wk_shipseq#chk<>
     c****               exsr      $rmvvardtl
     c                   else
     c     comp_typ_ord  setgt     mfhhmh
     c     comp_typ_ord  readpe    mfhhmh
     c                   if        %eof(mfhhmh)
     c                   eval      wk_shipseq# = 1
     c                   else
     c                   eval      ds_shipment# = M2SHMT
     c                   if        ds_hyphen = '-'
     c                   if        %check(digits:ds_shipseq#x) = *zero
     c                   eval      wk_shipseq# = ds_shipseq# + 1
     c                   else
     c                   eval      wk_shipseq# = 1
     c                   endif                                                  %check(digits:ds_shi
     c                   else
     c                   if        %check(digits:ds_shipseq#x2) = *zero
     c                   eval      wk_shipseq# = ds_shipseq#2 + 1
     c                   else
     c                   eval      wk_shipseq# = 1
     c                   endif                                                  %check(digits:ds_shi
     c                   endif                                                  ds_hyphen='-'
     c                   eval      wk_HypPosSv = *Zero
     c                   eval      wk_HypPosStr = 1
     c                   dou       wk_HypPos = *Zero
     c                   eval      wk_HypPos = %scan('-':M2SHMT:wk_HypPosStr)
     c                   if        wk_HypPos > *zero
     c                   eval      wk_HypPosSv = wk_HypPos
     c                   eval      wk_HypPosStr = wk_HypPos + 1
     c                   endif                                                  wk_HypPosSv>*Zero
     c                   enddo                                                  wk_HypPos=*Zero
     c                   eval      wk_HypPos = wk_HypPosSv
     c                   eval      wk_shipseq#x = %subst(M2SHMT:wk_HypPos+1:7)
     c                   if        wk_HypPos > *Zero  and
     c                               %check(digits:wk_shipseq#x) = *zero
     c                   eval      wk_shipseq#chk = %dec(wk_shipseq#x:7:0) + 1
     c                   else
     c                   eval      wk_shipseq#chk = 1
     c                   endif                                                  wk_HypPos>*Zero and
     c                   if        wk_shipseq#chk <> wk_shipseq#
     c                   eval      wk_DspMsg = 'VAR2- Order#: ' + ordkey
     c     wk_DspMsg     dsply(E)  'BEARL'
     c                   eval      wk_DspMsg = 'VAR2- wk_shipseq#: ' +
     c                               %editc(wk_shipseq#:'X') +
     c                               '  wk_shipseq#chk: ' +
     c                               %editc(wk_shipseq#chk:'X')
     c     wk_DspMsg     dsply(E)  'BEARL'
     c                   eval      wk_shipseq# = wk_shipseq#chk
     c                   endif                                                  wk_shipseq#chk<>
     c                   endif                                                  %eof(mfhhmh)
     c                   endif
     *
     c                   exsr      $rmvvardtl
     *
     c                   exsr      $getcarrier
     c
     c                   endif
     c                   endif
     *
     c                   endsr
     *--------------------------------------------------------------------
     *  Get the Carrier and Service Type
     *--------------------------------------------------------------------
     c     $getcarrier   begsr
     *
      /Free
           wk_GunCnt = *Zero;

           chain(n) (pord#:poctl) oehdwk99;
           if %found(oehdwk99);
             EXEC SQL select count(*) into :wk_GunCnt
                      from oedtwk a
                      where a.o1octl=:poctl and a.o1item='FIREARM' and
                       a.o1qstc>0;
              if h1svsv <> *blanks;
                 pshin = h1svsv;
                 ShipVia = h1svsv;
              endif;
           else;
              pshin = oesvsv;
              ShipVia = oesvsv;
           endif;

           if ShipVia <> *blanks;
              chain (ShipVia) ctrlsi;
              if %found(ctrlsi);
                 chain (sicrcd:sisvtp) ctrlcc;
                 if %found(ctrlcc);
                    @carcd = cccrcd;
                    @srvtp = ccsvtp;
                    @svdsc = ccsdsc;
                 endif;
              else;
                 pshin = 'FE';
              endif;
           else;
              pshin = 'FE';
           endif;
      /End-Free
     *
     c                   eval      visible = yes
     c                   exsr      $popplist1
     *
     c                   endsr
     *--------------------------------------------------------------------
     *  Populate PLIST1 with Shipping Information
     *--------------------------------------------------------------------
     c     $popplist1    begsr
     *
     c****               movel     oeord#        hldord
     c****               movea     hldord        hldord#
     c****               clear                   hldord
     c                   eval      pordr = ordkey
     *
     c                   movel     loco#         pcmno
     c** see $getcarrier eval      pshin = h1svsv
     c                   if        wk_GunCnt > *Zero
     c                   eval      kf_TABL = 'ALDAWKSTN'
     c                   eval      kf_KEY1 = @jobnam
     c     key_TD2       chain     GUPTDAT
     c                   if        %found(GUPTDAT)
     c                   eval      puser = 'FIREARMAL'
     c                   else
     c                   eval      kf_TABL = 'GIWKSTN'
     c                   eval      kf_KEY1 = @jobnam
     c     key_TD2       chain     GUPTDAT
     c                   if        %found(GUPTDAT)
     c                   eval      puser = 'FIREARMGI'
     c                   else
     c                   eval      puser = 'FIREARMHW'
     c                   endif                                                  %found(GUPTDAT)
     c                   endif                                                  %found(GUPTDAT)
     c                   eval      wk_MsgID = 'MSG9999'
     c                   eval      wk_MsgData = 'Firearm shipped using +
     c                               Varsity user ' + %trimr(puser) +
     c                             '.  Varsity order number ' +
     c                               %trimr(pvarord#) + '.'
     c                   exsr      $WrtJobLog
     c                   else
     c                   eval      pterm = n5fbcd
     c                   eval      kf_TABL = 'VARSITYDFT'
     c                   eval      kf_KEY1 = @usrnam
     c                   eval      kf_KEY2 = wk_DatabaseID2
     c     key_TD        chain     guptdat
     c                   if        %found(guptdat)
     c                   eval      puser = TDCF01
     c                   else
     c                   if        sicrcd = 'FEDEX'
     c                   eval      wk_VarUser = %trimr(@usrnam) + wk_DatabaseID2
     c     wk_VarUser    setll     ctrlgilu
     c                   select
     c                   when      %equal(ctrlgilu)
     c                   eval      puser = wk_VarUser
     c                   when      %subst(@usrnam:1:3) = 'DCA'
     c                   if        wk_DatabaseID2 = 'SS'
     c                   eval      puser = 'DCAUPS1SS'
     c                   else
     c                   eval      puser = 'DCAUPS1HY'
     c                   endif                                                  wk_DatabaseID2='SS'
     c                   other
     c                   eval      puser = 'DCUPS1HY'
     c                   endsl
     c                   else
     c                   eval      puser = @usrnam
     c                   endif                                                  sicrcd='FEDEX'
     c                   endif                                                  %found(guptdat)
     c                   endif                                                  wk_GunCnt>*Zero
     c                   eval      piamt = oetiva
     c                   eval      pordv = oetiva
     *
     c                   select
     c                   when      wk_DatabaseID = 'SS'  or
     c                               wk_DatabaseID = 'SX'
     c                   if        oeblto = 3408                                Home Depot
     c                   if        oeorrf <> *Blanks
     c                   eval      pusea = 'PO# ' + OEORRF
     c                   endif                                                  oeorrf<>*Blanks
     c     oeshto        chain     hdcust
     c                   if        %found(hdcust)
     c                   if        cmstor <> *Blanks  and
     c                               %len(%trimr(cmstor)) < 12
     c                   eval      puseb = 'STORE# ' + CMSTOR
     c                   endif                                                  cmstor<>*Blanks and
     c                   endif                                                  %found(hdcust)
     c                   endif                                                  oeblto=3408
     c                   endsl
     * drop shipment
     c**                 if        n2dshp <> 0
     c**   n2dshp        chain     hddshp03
     c                   if        oedshp <> 0
     c     oedshp        chain     hddshp03
     c                   if        %found = *on
     c                   eval      psnam = dsname
     *
     c                   select
     c                   when      dsadr1 <> *blanks
     c                   eval      psad1 = dsadr1
     c                   eval      psad2 = dsadr2
     c                   eval      psad3 = dsadr3
     c                   when      dsadr1 = *blanks
     c                   eval      psad1 = dsadr2
     c                   eval      psad2 = dsadr3
     c                   endsl
     *
     c                   if        dsctry = *blanks
     c                   eval      pctry = 'US'
     c                   else
     c                   eval      pctry = dsctry
     c                   endif
     c                   eval      pcity = dscity
     c                   eval      pstat = dsst
      *
     c                   if        pctry = 'US'
     c****               movel     dszip         pzip1
     c****               move      *blank        pzip2
     c                   eval      pzipc = %subst(dszip:1:5)
     c                   else
     c                   if        pctry = 'CA'
     c                   eval      pshin = 'UPSC0'
     c                   eval      @carcd = 'UPS'
     c                   eval      @srvtp = 'C0'
     c                   eval      @svdsc = 'CANADA STANDARD'
     c****               movel     dszip         tzip
     c****               movel     tzip1         pzip1
     c****               move      tzip2         pzip1
     c****               movel     tzip3         pzip2
     c                   if        %subst(dszip:4:1) = *Blank
     c                   eval      pzipc = %subst(dszip:1:3) + %subst(dszip:5:3)
     c                   else
     c                   eval      pzipc = dszip
     c                   endif
     c                   else
     c****               movel     dszip         pzip1
     c****               move      dszip         pzip2
     c                   eval      pzipc = dszip
     c                   endif
     c                   endif
     *
     c                   if        dsphon <> *Zero
     c                   movel     dsphon        phone#
     c                   else
     c     oeshto        chain     hdcust
     c                   if        %found(hdcust)  and  cmphon <> *Zero
     c                   movel     cmphon        phone#
     c                   else
     c                   eval      phone# = wk_DftPhone#
     c                   endif                                                  %found(hdcust) and
     c                   endif                                                  dsphon<>*Zero
     c                   if        char1 = '0'
     c                   eval      char1 = *blank
     c                   endif
     c                   eval      pphon = %triml(phone#)
     *
     c                   endif
     *
     c                   else
     *
     c**   n2shpt        chain     hdcust
     c     oeshto        chain     hdcust
     c                   if        %found = *on
     *
keb02c                   clear                   PrtPOBox2
keb02c                   clear                   PrtPOBox3
keb02c                   clear                   PrtPOBox4
     c                   eval      psnam = cmcna1
     *
     c                   if        cmcna2 <> *blanks  and
     c                               spcladdress(cmcna2) = at_POBox
keb02c                   if        sicrcd = 'ENDIC'
keb02c                   eval      PrtPOBox2 = @yes
keb02c                   else
     c                   eval      cmcna2 = *Blanks
keb02c                   endif                                                  cmcna2<>*blanks and
     c                   endif                                                  cmcna2<>*blanks and
     c                   if        cmcna3 <> *blanks  and
     c                               spcladdress(cmcna3) = at_POBox
keb02c                   if        sicrcd = 'ENDIC'
keb02c                   eval      PrtPOBox3 = @yes
keb02c                   else
     c                   eval      cmcna3 = *Blanks
keb02c                   endif                                                  cmcna2<>*blanks and
     c                   endif                                                  cmcna3<>*blanks and
     c                   if        cmcna4 <> *blanks  and
     c                               spcladdress(cmcna4) = at_POBox
keb02c                   if        sicrcd = 'ENDIC'
keb02c                   eval      PrtPOBox4 = @yes
keb02c                   else
     c                   eval      cmcna4 = *Blanks
keb02c                   endif                                                  cmcna2<>*blanks and
     c                   endif                                                  cmcna4<>*blanks and
     *  Modifications - KEB02
      /Free
          if PrtPOBox2 = @yes;
            if cmcna3 <> *Blanks  and  spcladdress(cmcna3) = *Blanks;
             clear cmcna3;
            endif;                                                           // cmcna3<>*Blanks and
            if cmcna4 <> *Blanks  and  spcladdress(cmcna4) = *Blanks;
             clear cmcna4;
            endif;                                                           // cmcna4<>*Blanks and
          endif;
          if PrtPOBox3 = @yes;
            if cmcna2 <> *Blanks  and  spcladdress(cmcna2) = *Blanks;
             clear cmcna2;
            endif;                                                           // cmcna2<>*Blanks and
            if cmcna4 <> *Blanks  and  spcladdress(cmcna4) = *Blanks;
             clear cmcna4;
            endif;                                                           // cmcna4<>*Blanks and
          endif;
          if PrtPOBox4 = @yes;
            if cmcna2 <> *Blanks  and  spcladdress(cmcna2) = *Blanks;
             clear cmcna2;
            endif;                                                           // cmcna2<>*Blanks and
            if cmcna3 <> *Blanks  and  spcladdress(cmcna3) = *Blanks;
             clear cmcna3;
            endif;                                                           // cmcna3<>*Blanks and
          endif;
          if cmcna2 = *Blanks;
            if cmcna3 <> *Blanks;
              cmcna2 = cmcna3;
              cmcna3 = *Blanks;
            else;
              if cmcna4 <> *Blanks;
                cmcna2 = cmcna4;
                cmcna4 = *Blanks;
              endif;                                                         // cmcna4<>*Blanks
            endif;                                                           // cmcna3<>*Blanks
          endif;                                                             // cmcna2=*Blanks
          if cmcna3 = *Blanks;
              if cmcna4 <> *Blanks;
                cmcna3 = cmcna4;
                cmcna4 = *Blanks;
              endif;                                                         // cmcna4<>*Blanks
          endif;                                                             // cmcna3=*Blanks
      /End-Free
     *  End Modifications - KEB02
     c                   select
     c                   when      cmcna2 <> *blanks
     c                   eval      psad1 = cmcna2
     c                   eval      psad2 = cmcna3
     c                   eval      psad3 = cmcna4
     c                   when      cmcna2 = *blanks
     c                   eval      psad1 = cmcna3
     c                   eval      psad2 = cmcna4
     c                   endsl
     *
     c                   eval      pcity = cmccty
     c                   eval      pstat = cmst
     *
     c                   if        cmctry = *blanks
     c                   eval      pctry = 'US'
     c                   else
     c                   eval      pctry = cmctry
     c                   endif
      *
     c                   if        pctry = 'US'
     c****               movel     cmzip         pzip1
     c****               move      *blank        pzip2
     c                   eval      pzipc = %subst(cmzip:1:5)
     c                   else
     c                   if        pctry = 'CA'
     c                   eval      pshin = 'UPSC0'
     c                   eval      @carcd = 'UPS'
     c                   eval      @srvtp = 'C0'
     c                   eval      @svdsc = 'CANADA STANDARD'
     c****               movel     cmzip         tzip
     c****               movel     tzip1         pzip1
     c****               move      tzip2         pzip1
     c****               movel     tzip3         pzip2
     c                   if        %subst(cmzip:4:1) = *Blank
     c                   eval      pzipc = %subst(cmzip:1:3) + %subst(cmzip:5:3)
     c                   else
     c                   eval      pzipc = cmzip
     c                   endif
     c                   else
     c****               movel     cmzip         pzip1
     c****               move      cmzip         pzip2
     c                   eval      pzipc = cmzip
     c                   endif
     c                   endif
     *
     c                   if        cmphon <> *Zero
     c                   movel     cmphon        phone#
     c                   else
     c                   eval      phone# = wk_DftPhone#
     c                   endif                                                  cmphon<>*Zero
     c                   if        char1 = '0'
     c                   eval      char1 = *blank
     c                   endif
     c                   eval      pphon = %triml(phone#)
     *
     c                   endif
     c                   endif
     *
keb01c                   exsr      $NumofBoxes
kb04                     if Costco = @yes;
kb04                        if psc01 = *blanks;
kb04                           psc01 = 'RES';
kb04                        else;
kb04                           psc02 = 'RES';
kb04                        endif;
kb04                        pterm = 'PP';
                            puser = 'COSTCO';
                            pusea = 'PO# ' + OEORRF;
kb04                     endif;
     *
     c                   if        pctry <> 'US'
     c                   exsr      $loadintl
     c                   endif
     *
     c                   exsr      $loadhaz
     *
     c                   exsr      $dspscreen
     *
     c                   endsr
keb01*--------------------------------------------------------------------
keb01*  Determine number of shippable containers                      ible
keb01*--------------------------------------------------------------------
     * Begin Mods = KEB01
      /Free
           begsr $NumofBoxes;

                 OrdPos = %scan('-':pordr:1);
                 if OrdPos > *zeros;
                    OrderNum = %subst(pordr:1:OrdPos - 1);
                 else;
                    OrderNum = pordr;
                 endif;

       // Get the Varsity ShipOptimizer Order Number
                 Suffix = *blanks;
                 if (%subst(Turn#:2:8) = *blanks);
                    clear Turn#;
                    Turn# = %trim('0000');
                 endif;
                 pos = %scan(' ':Turn#);
                    if pos = *zeros;
                       Suffix = %subst(Turn#:6:4);
                    else;
                       Suffix = %subst(Turn#:Pos - 4:4);
                    endif;

       // Master Order Number
                 OrderType = 'MO';
                 OptOrd = %trim(OrderNum) + '-' + Suffix + '-' +
                            %trim(OrderType);

                 exec sql select cxship into :wk_ship
                          from vpcont
                          where cxship = :OptOrd;

       // Single Order Number
                 if wk_ship = *blanks;
                    OrderType = 'SO';
                    OptOrd = %trim(OrderNum) + '-' + Suffix + '-' +
                             %trim(OrderType);
                    exec sql select cxship into :wk_ship
                             from vpcont
                             where cxship = :OptOrd;
                 endif;

       // Not Master or Single Order Number
                 if wk_ship = *blanks;
                    OrderType = '  ';
                    OptOrd = %trim(OrderNum) + '-' + Suffix;
                    exec sql select cxship into :wk_ship
                             from vpcont
                             where cxship = :OptOrd;
                 endif;

                 Wk_PkgCnt = *zeros;
                 Wk_Cont = *blanks;

                 exec sql select count(*) into :Wk_PkgCnt
                          from vpcont
                          where cxincn = :Wk_Cont and cxship = :Wk_Ship;

                 if Wk_PkgCnt > *zeros;
                    ptpkg = Wk_pkgCnt;
                 else;
                    ptpkg = 1;
                 endif;

           endsr;
      /End-Free
     * End Mods = KEB01
     *--------------------------------------------------------------------
     *  Display Shipping Screen Either Interface Screen or Varsity Visible
     *--------------------------------------------------------------------
     c     $dspscreen    begsr
     *
     c                   if        visible = yes
     c                   clear                   pptrk
     c****               call      'S4CS10'
     c****               parm                    plist1
     c                   eval      PVSBL = 'Y'
     c                   call      'VSSHP001'
     c                   parm                    plist1
     c                   if        perror > '1'
     c                   eval      *inlr = *on
     c                   return
     c                   else
     c                   eval      endprog = 'Y'
     c****               if        pstus = 'U'
     c                   if        perrid = 'INFO'  and
     c                              (abrcd<> *Blanks  or  pptrk <> *Blanks)
     c                   exsr      $wrtshipinfo
     c                   endif
     *
     c                   endif
     *
     c                   if        ordershp = yes
     c                   exsr      $chkfordlt
     c                   endif
     c                   else
     c                   exsr      $procscrn
     c                   endif
     *
     c                   endsr
     *--------------------------------------------------------------------
     *  Check for Delete
     *--------------------------------------------------------------------
     c     $chkfordlt    begsr
     *
     c     comp_typ_ord  chain     mfh1mh
     c                   if        %found = *on
     c                   if        mhboxc <> mhboxi
     c                   clear                   custchg
     c                   clear                   delchg
     c                   clear                   boxes
     c                   clear                   x
     c                   clear                   y
     c                   clear                   track#
     c     comp_typ_ord  setll     mfd1md
     c     comp_typ_ord  reade     mfd1md
     c                   dow       %eof = *off
     c                   eval      custchg = custchg + mdcdch
     c                   eval      delchg = delchg + mddchg
     c                   eval      boxes = boxes + 1
     c                   eval      x = x + 1
     c                   if        mdfbrc = *blanks
     c                   eval      track#(x) = mdftrk
     c                   else
     c                   eval      track#(x) = mdfbrc
     c                   endif
     c     comp_typ_ord  reade     mfd1md
     c                   enddo
     *
     c                   eval      oetfrt = custchg
     c                   eval      mhdchg = delchg
     c                   eval      mhcusc = custchg
     c                   eval      mhboxi = boxes
     c                   update    mfh1mhr
     c                   endif
     *
     c                   else
     c                   clear                   oetfrt
     c                   endif
     *
     c***                update    oeorhdr
     *
     c                   if        pstus = 'D'
     *
     c                   eval      endprog = no
     *
     c     *loval        setll     oecmwk
     c                   read      oecmwk
     c                   dow       %eof = *off
     c                   eval      varsity = %subst(o4cmnt:46:2)
     c                   if        varsity = 'VS'
     c**                 delete    oecmwkr
     c                   endif
     c                   read      oecmwk
     c                   enddo
     *
     c                   else
     *
     c                   eval      endprog = yes
     *
     c     *loval        setll     oecmwk
     c                   read      oecmwk
     c                   dow       %eof = *off
     c                   eval      varsity = %subst(o4cmnt:46:2)
     c                   if        varsity = 'VS'
     c                   eval      hldtrknum = %subst(o4cmnt:1:25)
     c                   eval      y = 1
     c     hldtrknum     lookup    track#(y)                              50
     c                   if        *in50 = *off
     c**                 delete    oecmwkr
     c                   endif
     c                   endif
     c                   read      oecmwkr
     c                   enddo
     *
     c                   endif
     *
     c                   endsr
      *-----------------------------------------------------------------
      *  Load International Document Information to PLIST1
      *-----------------------------------------------------------------
     c     $loadintl     begsr
      *
      * Company Defaults
     c                   eval      aseit = 'E'
     c                   eval      asein = '47-0351752'
     c                   eval      ascon = 'Sue Lemburg'
     c                   eval      aauth = 'Greg Hanson'
     c                   eval      atitl = 'Director of Sales'
     c                   eval      arelp = 'N'
     c                   eval      asede = 'OS'
     c                   eval      adscg = 'Ammo/Tools/Supplies'
     c                   eval      astyp = 'PRE'
     c                   eval      atosa = 'DDP'
     c                   eval      aptrm = 'On mccount'
     c                   eval      arexp = 'OS'
     c                   eval      afcur = 'USD'
     c                   eval      pintl = 'Y'
     c                   eval      acdst = pctry
      *
     c                   endsr
      *-----------------------------------------------------------------
      *  Load Hazmat into VPCONT and VPPACK
      *-----------------------------------------------------------------
     c     $loadhaz      begsr
      *
      * CHECK FOR HAZMAT
      * Write Container Record
     c                   eval      cxcmno = pcmno
     c                   movel     udate         tmp12            12
     c                   move      utime         tmp12
     c                   eval      cxcnid = %trim(ordkey) + '-' + tmp12
      *
     c****               eval      pshmt = %trim(ordkey) + '-' + '0000001'
     c                   eval      pshmt = %trim(ordkey) + '-' +
     c                               %editc(wk_shipseq#:'X')
     c                   eval      cxship = pshmt
     c                   eval      cxstat = 'OPEN'
     c                   eval      cxlevl = 00001
     c                   write     vpcontr
      *
      * Write Pack File Record
     c                   eval      pkcmno = pcmno
     c                   eval      pkship = cxship
     c                   eval      pkpckc = cxship
     c                   eval      pkcnid = cxcnid
     c                   eval      pkshcn = cxcnid
     c                   eval      pcpkg  = cxcnid
     c                   eval      pkstat = 'OPEN'
      *
     c***  oeord#        setll     oedtwk
     c     h1octl        setll     oedtwk
     c                   dou       *in99 = *on
     c***  oeord#        reade(n)  oedtwk                                 99
     c     h1octl        reade(n)  oedtwk                                 99
     c                   if        *in99 = *off
      *
     c***  ordtlky       chain     oeordt01
     c***                if        %found(oeordt01)
     c***                          and akqstc <> o1qstc
     c     ordtlky       chain     oeordt
     c                   if        %found(oeordt)
     c                             and odqstc <> o1qstc
      *
     c                   eval      pkpkit = oditem
     c                   movel     odord#        pkordr
     c                   eval      pkitds = odimds
     c                   eval      pkmacd = 'EA'
     c                   eval      pkhdes = odimds
     c                   eval      pkpkqt = o1qstc
     c                   eval      pkorig = 'D'
     c                   eval      pkccor = 'US'
     c                   eval      pkcmfg = 'US'
     c                   eval      pknprd = 'Y'
     c                   eval      pkprct = 'A'
     c                   eval      pkdgcd = '  '
     c                   eval      ahazf = ' '
     c                   eval      pklin# = 1
     c                   eval      pkitum = odprum
     c                   eval      pkuntp = odslpr
      *
     c                   eval      wk_ProdClass = odpcls
     c                   eval      wk_Item = oditem
     c                   eval      wk_CommCode =
     c                              RtvCommCode(wk_ProdClass:Wk_item)
     c                   movel     wk_CommCode   pkharm
     c                   movel     wk_CommCode   pkscdb
     c                   movel     wk_CommCode   pkclas
      *
     c     oditem        chain     hdimst
     c                   if        %found(hdimst)
     c                   eval      pkiwgt = IMIMWG
     c                   else
     c                   eval      pkiwgt = 1
     c                   eval      IMPCLS = ODPCLS
     c                   endif
      *
     c     iwhskey       chain     hdiwhs
     c                   if        %found(hdiwhs)
     c     iwftcl        chain     hdftcl
     c                   if        %found(hdftcl)
     c****               if        ftld03 <> *blank
     c****               select
     c****               when      %scan(' UN':ftld03) > *Zero
     c****               eval      pkdgcd = %subst(ftld03:1:%scan(' UN':ftld03))
     c****               other
     c****               movel     ftld03        pkdgcd
     c****               endsl
     c****               movel     'Y'           ahazf
     c****               movel     'Y'           pkhaz
     c****               eval      pkhqty = odqstc
     c****               eval      psc01 = 'HAZ'
     c****               endif
     c                   endif
     c                   endif
      *
     C                   IF        %subst(IMPCLS:1:1) = 'A'  and  oeloc# <> 91
     C                   IF        sicrcd = 'FEDEX'
     C                   EVAL      psc01 = 'SIGNA'
     C                   ELSE
     C                   EVAL      psc01 = 'ASDCR'
     C                   ENDIF                                                  SICRCD='FEDEX'
     C                   ENDIF                                                  $subst(IMPCLS:1:1)=
      *
     c     packkey       chain     vppack
     c                   if        %found(vppack)
     c                   update    vppackr
     c                   else
     c                   write     vppackr
     c                   endif
     c                   endif
     c                   endif
     c                   enddo
      *
     c                   endsr
     *--------------------------------------------------------------------
     *  Process Interface Screen
     *--------------------------------------------------------------------
     c     $procscrn     begsr
     *
     c                   eval      *in75 = *off
     *
     c                   dow       endloop = no
     *
     c                   exfmt     screen1
     *
     c                   clear                   pptrk
     *
     c                   if        *in03 = *on
     c                   eval      endloop = yes
     c                   eval      endprog = yes
     c                   endif
     *
     c                   if        *in10 = *on
     c****               call      'S4CS10'
     c****               parm                    plist1
     c                   eval      PVSBL = 'Y'
     c                   call      'VSSHP001'
     c                   parm                    plist1
     c                   if        perror > '1'
     c                   return
     c                   endif
     *
     c                   eval      @totwgt = ptwgt
     c                   eval      @pkprc = ptpkg
     c                   eval      endloop = yes
     c                   else
     c                   eval      *in75 = *on
     c                   if        @pkgwgt = *zeros
     c                   eval      *in76 = *on
     c                   iter
     c                   else
     c                   eval      *in76 = *off
     c                   endif
     *
     c****               eval      ppkgw = @pkgwgt
     c                   eval      pawgt = @pkgwgt
     c                   eval      ptpkg = @pkcnt
     *
     c                   eval      @pkprc = @pkprc + 1
     *
     c                   if        @pkcnt = @pkprc
     c                   eval      pendf = yes
     c                   eval      endloop = yes
     c                   endif
     *
     c****               call      'S4CS09'
     c****               parm                    plist1
     c                   eval      PVSBL = 'N'
     c                   call      'VSSHP001'
     c                   parm                    plist1
     *
     c                   if        perror > '1'
     c****               call      'S4CS10'
     c****               parm                    plist1
     c                   eval      PVSBL = 'Y'
     c                   call      'VSSHP001'
     c                   parm                    plist1
     c                   if        perror > '1'
     c                   return
     c                   endif
     *
     c                   eval      @totwgt = ptwgt
     c                   eval      @pkprc = ptpkg
     c                   eval      endloop = yes
     c                   else
     c                   eval      @totwgt = @totwgt + prwgt
     c                   clear                   @pkgwgt
     c****               clear                   ppkgw
     c                   clear                   pawgt
     c                   endif
     *
     c                   endif
     *
     c                   enddo
     *
     c****               if        pstus = 'U'
     c                   if        perrid = 'INFO'  and
     c                              (abrcd<> *Blanks  or  pptrk <> *Blanks)
     c                   exsr      $wrtshipinfo
     c                   eval      rtnmsg = msg(1)
     c                   eval      *in77 = *on
     c                   exfmt     screen1
     c                   if        *in10 = *on
     c                   eval      endprog = no
     c                   else
     c                   eval      endprog = yes
     c                   endif
     c                   endif
     *
     c                   endsr
     *--------------------------------------------------------------------
     *  Write Shipping Information
     *--------------------------------------------------------------------
     c     $wrtshipinfo  begsr
     *
     c**** pord#         chain     oehdwk99
     c     orhdrky       chain     oehdwk99
     c                   if        %found(oehdwk99)
     c                   if        sicrcd = 'UPS'
     c                   if        abrcd <> *blank
     c                   movel     abrcd         tmp18
     c                   move      tmp18         h1udf3
     c                   else
     c                   movel     pptrk         tmp18
     c                   move      tmp18         h1udf3
     c                   endif
     c                   else
kebxxc                   if        sicrcd = 'ENDIC'
     c                   if        abrcd <> *blank
kebxxc                   evalr     h1udf3 = abrcd
kebxxc                   else
kebxxc                   evalr     h1udf3 = pptrk
     c                   endif
kebxxc                   else
     c                   if        abrcd <> *blank
     c                   eval      h1udf3 = abrcd
     c                   else
     c                   eval      h1udf3 = pptrk
     c                   endif                                                  abrcd<>*blank
     c                   endif                                                  SICRCD='UPS'
kebxxc                   endif                                                  SICRCD='UPS'
      *
     c                   eval      sv_svsv = h1svsv
     c     log_key       chain     ctrlsil1
     c                   if        %found(ctrlsil1)
     c                   eval      h1svsv = lishin
     c                   else
     c                   eval      h1svsv = pshin
     c                   endif
      *
     c                   if        h1svsv <> sv_svsv
     c                   if        not %open(hdshpv)
     c                   open      hdshpv
     c                   endif
     c     h1svsv        chain     hdshpv
     c                   if        %found(hdshpv)
     c                   eval      h1svds = svsvds
     c                   else
     c                   eval      h1svds = *blanks
     c                   endif
     c                   endif
      *
keb03c                   eval      parm_svsv = *blanks
keb03c                   eval      parm_svds = *blanks
      *
     c                   if        h1svsv <> sv_svsv
keb03c                   eval      parm_svsv = h1svsv
keb03c                   eval      parm_svds = h1svds
     c     h1octl        setll     oedtwk
     c                   dou       *in99 = *on
     c     h1octl        reade     oedtwk                                 99
     c                   if        *in99 = *off
     c                   eval      o1svsv = h1svsv
     c                   eval      o1svds = h1svds
     C                   update    oedtwkr
     c                   endif
     c                   enddo
     c                   endif
      *
     c                   update    oehdwkr
     c                   endif
     *
     c                   endsr
     *--------------------------------------------------------------------
     *  Remove Varsity file detail.
     *--------------------------------------------------------------------
     c     $rmvvardtl    begsr
     *
     * Delete Packer Detail File records.
     *
     c                   if        not %open(VPDETLL2)
     c                   open      VPDETLL2
     c                   endif                                                  not %open(VPDETLL2)
     c     key_DT        setll     VPDETLL2
     c                   dou       %eof(VPDETLL2)
     c     key_DT        reade     VPDETLL2
     c                   if        %eof(VPDETLL2)
     c                   leave
     c                   endif                                                  %eof(VPDETLL2)
     c                   delete    VPDETLR
     c                   enddo                                                  %eof(VPDETLL2)
     *
     * Delete Packed Container File records.
     *
     c     key_CX        setll     VPCONT
     c                   dou       %eof(VPCONT)
     c     key_CX        reade     VPCONT
     c                   if        %eof(VPCONT)
     c                   leave
     c                   endif                                                  %eof(VPCONT)
     c                   delete    VPCONTR
     c                   enddo                                                  %eof(VPCONT)
     *
     * Delete Pack File records.
     *
     c     key_PK        setll     VPPACK
     c                   dou       %eof(VPPACK)
     c     key_PK        reade     VPPACK
     c                   if        %eof(VPPACK)
     c                   leave
     c                   endif                                                  %eof(VPPACK)
     c                   delete    VPPACKR
     c                   enddo                                                  %eof(VPPACK)
     *
     c                   endsr
     *--------------------------------------------------------------------
     *  Write joblog message.
     *--------------------------------------------------------------------
     CSR   $WrtJobLog    BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR
     *--------------------------------------------------------------------
     *  Initialization Subroutine
     *--------------------------------------------------------------------
     c     *inzsr        begsr
     *
     c                   TIME                    UTIME
     *'
     c                   eval      pcmno = '1   '
     *
     c     *entry        plist
     c                   parm                    pordin
     c                   parm                    pvarordin
     c                   parm                    poctlin
keb01c                   parm                    Turn#
keb01c                   parm                    OrderType
keb02c                   parm                    parm_svsv
keb02c                   parm                    parm_svds
     *
     c     car_serv      klist
     c                   kfld                    sicrcd
     c                   kfld                    sisvtp
     *
     c     log_key       klist
     c                   kfld                    pcrcd
     c                   kfld                    psvtp
     *
     c     comp_typ_ord  klist
     c                   kfld                    comp#
     c                   kfld                    ordtype
     c                   kfld                    ordkey
     *
     c     cmtkey        klist
     c                   kfld                    n2ord#
     c                   kfld                    ordlin#
     c                   kfld                    blkrel
     c                   kfld                    doctyp
     *
     c     iwhskey       klist
     c                   kfld                    oditem
     c                   kfld                    odwh
     *
     c     orhdrky       klist
     c                   kfld                    pord#
     c                   kfld                    poctl
     *
     c***  ordtlky       klist
     c***                kfld                    o1octl
     c***                kfld                    o1item
     c***                kfld                    o1wh
     c     ordtlky       klist
     c                   kfld                    pord#
     c                   kfld                    o1orl#
     c                   kfld                    o1bln#
     *
     c     packkey       klist
     c                   kfld                    pkcmno
     c                   kfld                    pkship
     c                   kfld                    pkpckc
     c                   kfld                    pkptyp
     c                   kfld                    pkcnid
     c                   kfld                    pkpkit
     *
     c     key_DT        klist                                                  VPDETLL2
     c                   kfld                    comp#
     c                   kfld                    MHSHMT
     *
     c     key_CX        klist                                                  VPCONT
     c                   kfld                    comp#
     c                   kfld                    MHSHMT
     *
     c     key_PK        klist                                                  VPPACK
     c                   kfld                    comp#
     c                   kfld                    MHSHMT
     *
     c     key_TD        klist                                                  GUPTDAT
     c                   kfld                    kf_TABL
     c                   kfld                    kf_KEY1
     c                   kfld                    kf_KEY2

     c     key_TD2       klist                                                  GUPTDAT
     c                   kfld                    kf_TABL
     c                   kfld                    kf_KEY1
     *
     c                   endsr
     P/EJECT
     P**************************************************************************
     P* Check for special address field.  Return a character field.            *
     P**************************************************************************
     P spcladdress     B
     D spcladdress     PI             1A
     D   address                     26A   VALUE
     D addresstype     S              1A
     D ValidBoxChar    C                   CONST(' -0123456789')                Valid P.O. Box Chars
     C                   EVAL      addresstype=*Blank
     C                   SELECT
     C                   WHEN      %subst(address:1:4) = 'ATT:' or
     C                             %subst(address:1:5) = 'ATTN:' or
     C                             %subst(address:1:4) = 'ATT;' or
     C                             %subst(address:1:5) = 'ATTN;' or
     C                             %subst(address:1:4) = 'ATT ' or
     C                             %subst(address:1:5) = 'ATTN '
     C                   EVAL      addresstype = at_Attention
     C                   WHEN      %subst(address:1:4) = 'BOX ' or
     C                             %subst(address:1:4) = 'Box '
     C     ValidBoxChar  CHECK     address:5                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:5) = 'PO BX'
     C     ValidBoxChar  CHECK     address:6                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:6) = 'PO BOX'
     C     ValidBoxChar  CHECK     address:7                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:7) = 'PO  BOX'
     C     ValidBoxChar  CHECK     address:8                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:4) = 'P.O.' or
     C                             %subst(address:1:4) = 'P O '
     C     ValidBoxChar  CHECK     address:9                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:6) = 'P. O. '
     C     ValidBoxChar  CHECK     address:11                             90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      address = '.'
     C                   EVAL      addresstype = at_Spacer
     C                   WHEN      %subst(address:1:4) = 'AKA '
     C                   EVAL      addresstype = at_AKA
     C                   WHEN      %subst(address:1:1) = '%'
     C                               or  %subst(address:1:4) = 'C/O '
     C                   EVAL      addresstype = at_CareOf
     C                   ENDSL
     C                   RETURN    addresstype
     P spcladdress     E
     P/EJECT
** Messages
Order Shipped
Order Has Already Been Shipped
Invalid Function Key
