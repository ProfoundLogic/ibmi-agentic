     H DATFMT(*ISO) TIMFMT(*ISO) OPTION(*SrcStmt: *NoDebugIO)
     H COPYRIGHT('Varsity Logistics, INC.  Chicago, IL.  2024')
     H DFTACTGRP (*NO) ACTGRP('HDSOPT') BNDDIR('VARHDINFD')
     **********************************************************************
     * Process Records from BoxLogix and DigiPick to ShipSelect
     *
     * Program Name: VPRBLDP
     *
     * Author: Kevin Blackmon - Varsity Logistics, Inc.
     **********************************************************************
     *------------------------MODIFICATION LOG-----------------------------
     * DATE      PGMR         REMARKS
     *
     *
     *------------------------MODIFICATIONS END----------------------------
     * Shipment Group Customer
     fhypsgcu   if   e           k disk
     * Shipment Group Detail
     fhylsgdt3  if   e           k disk
     * Order Header File
     foeorhd    if   e           k disk
     * Order Detail
     foeordt    if   e           k disk
     * Master-pack Content Detail
     fhylmpcd3  if   e           k disk
     * Master-pack Content Header
     fhypmphd   if   e           k disk
     * Shipping Pallet Master File
     fhyppllt   if   e           k disk
     * Master Location File
     fhdlctn    if   e           k disk
     * Varsity Container File
     fvpcntrl3  if   e           k disk
     * Varsity Package Unit File
     fpkguntl1  if   e           k disk
     * Varsity Container Pack File
     fvpcont    uf a e           k disk
     **********************************************************************
     * Parameters
     **********************************************************************
     d VPRBLDP         PI
     d  ShipGrp#                      9S 0 CONST
     d  BillTo#                       7S 0 CONST
     d  ShipTo#                       7S 0 CONST
     d  DropShip#                     7S 0 CONST
     d  MaxDaysIntr                   3P 0 CONST
     d  Caller                       10A   CONST
     d  ShipVia                       2A
     d  RtnCode                       2A
     **********************************************************************
     * Program Calls
     **********************************************************************
     d ITEMDIMWGT      PR                  extpgm('VPRDMWTCT')
     d  Item                         15
     d  OrdQty                        9  0
     d  PCqty                         7  0
     d  PCcnt                         7  0
     d  PCLength                     11  4
     d  PCWidth                      11  4
     d  PCHeight                     11  4
     d  PCWeight                     11  4
     d  PCCubes                      16  4
     d  PCUom                         2
     d  PCMastPck                     1
     d  MPQty                         7  0
     d  MPCQty                        7  0
     d  MPcnt                         7  0
     d  MPLength                     11  4
     d  MPWidth                      11  4
     d  MPHeight                     11  4
     d  MPWeight                     11  4
     d  MPCubes                      16  4
     d  MPUom                         2
     d  MPMastPck                     1
     d  CPQty                         7  0
     d  CPCQty                        7  0
     d  CPcnt                         7  0
     d  CPLength                     11  4
     d  CPWidth                      11  4
     d  CPHeight                     11  4
     d  CPWeight                     11  4
     d  CPCubes                      16  4
     d  CPUom                         2
     d  CPMastPck                     1
     d  Company                       4
     d  WareHouse                     3  0
     d  Bulk                          1
     d  ExtWeight                     5  0
     d  FreightClass                  5
     d  FAKClass                      2
     *
     d CRTCONTAINERS   PR                  extpgm('VPCCRTCNT')
     d  Company                       4
     d  VarOrder                     20
     d  State                         2
     d  PostalCode                   13
     d  Country                       2
     d  ShipVia                       2
     d  RtnCode                       2
     d  PalletCnt                     4  0
     **********************************************************************
     * Program Defined Alphanumeric Fields
     **********************************************************************
     d Order#          s              8  0
     d Turn#           s              9  0
     d Length          s             11
     d Width           s             11
     d Height          s             11
     d Weight          s             11
     d Wk_weight       s             11  4
     d MstPckSav       s                   like(mdmp#)
     d                                     dim(9999)
     d SavMstPck       s                   like(mdmp#)
     d Item            s             15
     d OrderQty        s             14
     d PkgId           s                   like(cnunit)
     d x               s              3  0
     d Serial          s                   like(mdsrl#) inz
     d Company         s              4    inz
     d Warehouse       s              3  0 inz
     d Warehouse10     s             10    inz
     d State           s                   like(gcstst)
     d PostalCode      s                   like(gcstzp)
     d Country         s                   like(gcstct)
     d OrdQty          s              9  0
     d PCCnt           s              7  0
     d PCQty           s              7  0
     d PCLength        s             11  4
     d PCWidth         s             11  4
     d PCHeight        s             11  4
     d PCWeight        s             11  4
     d PCCubes         s             16  4
     d PCUom           s              2
     d PCMastPck       s              1
     d MPCnt           s              7  0
     d MPQty           s              7  0
     d MPCQty          s              7  0
     d MPLength        s             11  4
     d MPWidth         s             11  4
     d MPHeight        s             11  4
     d MPWeight        s             11  4
     d MPCubes         s             16  4
     d MPUom           s              2
     d MPMastPck       s              1
     d CPCnt           s              7  0
     d CPQty           s              7  0
     d CPCQty          s              7  0
     d CPLength        s             11  4
     d CPWidth         s             11  4
     d CPHeight        s             11  4
     d CPWeight        s             11  4
     d CPCubes         s             16  4
     d CPUom           s              2
     d CPMastPck       s              1
     d Bulk            s              1
     d ExtWeight       s              5  0
     d FreightClass    s              5
     d FrghtClass      s             16
     d FAKClass        s              2
     d MstOrder        s             20
     d ErrCde          s              7
     d VarOrder        s                   like(cxship)
     d VarCont         s                   like(cxcnid)
     d ContRec         s              1
     d Wk_PltCnt       s              4  0
     d PalletCnt       s              4  0
     d Wk_ContCnt      s              9  0
     d Len             s             11  4
     d Wdt             s             11  4
     d Hgt             s             11  4
     **********************************************************************
     * Program Defined Constants
     **********************************************************************
     d @yes            s              1    inz('Y')
     d @no             s              1    inz('N')
     **********************************************************************
     * Data Structures
     **********************************************************************
     *  Program status information.
     d                sds
     d wpgm                          10
     d  wpgm1                         6    overlay(wpgm)
     d  Pgpgm                  1     10
     d @parms                 37     39  0
     d wsjob                 244    253
     d wsuser                        10
     d @job#                          6  0
     **********************************************************************
     * Program Defined Arrays
     **********************************************************************
     d dims            s             11  4 dim(3) DESCEND
     **********************************************************************
      /Free
       // ============================================================
       // Main Line Process
       // ============================================================
           clear ShipVia;
           clear RtnCode;
           clear Warehouse;
           clear ContRec;

           chain (ShipGrp#:BillTo#:ShipTo#:DropShip#) hypsgcu;
           if %found(hypsgcu);
              exsr $GetItems;
           endif;

           if ContRec = @yes;
              exsr $PalletCnt;
              CRTCONTAINERS (Company:VarOrder:State:PostalCode:
                             Country:ShipVia:RtnCode:PalletCnt);
           endif;

           *inlr = *on;
       // ============================================================
       // Get Items on an Order
       // ============================================================
           begsr $GetItems;

                 VarOrder = *blanks;

       // Get Items from Shipment Group Detail File
                 setll (ShipGrp#:BillTo#:ShipTo#:DropShip#) hylsgdt3;
                 reade (ShipGrp#:BillTo#:ShipTo#:DropShip#) hylsgdt3;
                 dow %eof(hylsgdt3) = *off;
                     if VarOrder = *blanks;
                        VarOrder = %trim(%char(gdord#)) + %trim(%char(gdturn));
                        exsr $ChkContFile;
                     endif;
                     Order# = gdord#;
                     Turn# = gdturn;
                     Item = gditem;
                     OrderQty = %char(gdqstc);
                     State = gcstst;
                     PostalCode = gcstzp;
                     Country = gcstct;
                     if Warehouse = *zeros;
                        exsr $GetCompWh;
                        exsr $ChkMstBoxes;
                     else;
                        exsr $ChkMstBoxes;
                     endif;
                 reade (ShipGrp#:BillTo#:ShipTo#:DropShip#) hylsgdt3;
                 enddo;

           endsr;
       // ============================================================
       // Check to see if Order is in VPCONT, if it is delete it
       // ============================================================
           begsr $ChkContFile;

                 clear Wk_ContCnt;

                 exec sql
                 select count(*) into :Wk_ContCnt
                  from vpcont where cxship = :VarOrder;

                 if Wk_ContCnt > *zeros;
                    exec sql
                    delete vpcont where cxship = :VarOrder;
                 endif;

           endsr;
       // ============================================================
       // Get Warehouse Number
       // ============================================================
           begsr $GetCompWh;

                 chain (gdord#) oeorhd;
                 if %found(oeorhd);
                    chain (oeloc#) hdlctn;
                    if %found(hdlctn);
                       Company = %EditC(loco#:'X');
                    endif;
                 endif;

            // Get Warehouse
                 if Warehouse = *zeros;
                    chain (gdord#:gdorl#) oeordt;
                    if %found(oeordt);
                       Warehouse = odwh;
                       Warehouse10 = %char(odwh);
                    endif;
                 endif;

           endsr;
       // =============================================================
       // Check for Master Boxes
       // =============================================================
           begsr $ChkMstBoxes;

                 Length = *blanks;
                 Width = *blanks;
                 Height = *blanks;
                 Weight = *blanks;
                 Serial = gdsscc;

                 chain (gdord#:gdturn:Serial) hylmpcd3;
                 if %found(hylmpcd3);
                    SavMstPck = mdmp#;
                    x = %lookup(SavMstPck:MstPckSav);
                    if x = *zeros;
                       x = %lookup(*blanks:MstPckSav);
                       MstPckSav(x) = SavMstPck;
                       chain (mdmp#) hypmphd;
                       if %found(hypmphd);
                          if mhmphgt = *zeros;
                             chain (mhbxid) vpcntrl3;
                             if %found(vpcntrl3);
                                PkgId = cnunit;
                                if cnlen = *zeros or
                                   cnwdt = *zeros or
                                   cnhgt = *zeros;
                                   Length = %char(mhmphgt);
                                   Width = %char(mhmpwid);
                                   Height = %char(mhmpdep);
                                   Dims(1) = %dec(Length:11:4);
                                   Dims(2) = %dec(Width:11:4);
                                   Dims(3) = %dec(Height:11:4);
                                   sorta Dims;
                                   Length = %char(Dims(1));
                                   Width = %char(Dims(2));
                                   Height = %char(Dims(3));
                                else;
                                   Dims(1) = cnlen;
                                   Dims(2) = cnwdt;
                                   Dims(3) = cnhgt;
                                   sorta Dims;
                                   Length = %char(Dims(1));
                                   Width = %char(Dims(2));
                                   Height = %char(Dims(3));
                                endif;
                             endif;
                          else;
                             PkgId = mhbxid;
                             if PkgId = *blanks;
                                PkgId = 'VP';
                             endif;
                             Length = %char(mhmphgt);
                             Width = %char(mhmpwid);
                             Height = %char(mhmpdep);
                             Dims(1) = %dec(Length:11:4);
                             Dims(2) = %dec(Width:11:4);
                             Dims(3) = %dec(Height:11:4);
                             sorta Dims;
                             Length = %char(Dims(1));
                             Width = %char(Dims(2));
                             Height = %char(Dims(3));
                          endif;
                          exsr $GetWeight;
                          VarCont = mdmp#;
                          exsr $CrtCont;
                       endif;
               //   else;
               //       exsr $GetWeight;
               //       VarCont = mdmp#;
               //       exsr $CrtCont;
                    endif;
               //   VarCont = mdmp#;
               //   exsr $GetWeight;
               //   exsr $CrtCont;
                 else;
                    VarCont = gdssccbc;
                    exsr $NotMstPacked;
                 endif;

           endsr;
       // ============================================================
       // Get Weight of Items in Master Pack
       // ============================================================
           begsr $GetWeight;

                 clear Wk_Weight;

                 exec sql
                 select sum(round(a.mduomq * b.imimwg,2))
                  into :Wk_Weight
                 from hypmpcd a join hdimst b on a.mditem = b.imitem
                 where a.mdord# = :Order# and a.mdturn = :Turn# and
                 a.mdmp# = :SavMstPck;

                 Weight = %char(Wk_Weight);
                 exsr $GetItemInfo;

           endsr;
       // =============================================================
       // Check for Master Boxes
       // =============================================================
           begsr $NotMstPacked;

                 exsr $GetItemInfo;

                 if Bulk = @yes and PCcnt = *zeros and
                    CPcnt = *zeros;
                    PCcnt = OrdQty;
                 endif;

           // Item Level Detail
                 if PCcnt > *zeros;
                    Length = %char(PCLength);
                    Width = %char(PCWidth);
                    Height = %char(PCHeight);
                    if PCWeight <> gdpuwt;
                    Weight = %char(PCWeight * OrdQty);
                    else;
                       Weight = %char(PCWeight);
                    endif;
                 endif;

           // Master Pack Detail
                 if MPcnt > *zeros;
                    Length = %char(MPLength);
                    Width = %char(MPWidth);
                    Height = %char(MPHeight);
                    Weight = %char(MPWeight);
                 endif;

           // Case Pack Detail
                 if CPcnt > *zeros;
                    Length = %char(CPLength);
                    Width = %char(CPWidth);
                    Height = %char(CPHeight);
                    Weight = %char(CPWeight);
                 endif;

                 if Length = *blanks or
                    Width = *blanks or
                    Height = *blanks or
                    Weight = *blanks;
                    PkgID = 'BX';
                    Length = '10.00';
                    Width = '10.00';
                    Height = '10.00';
                    Weight = %char(gdpuwt);
                 endif;

                 clear Dims;
                 Dims(1) = %dec(Length:11:4);
                 Dims(2) = %dec(Width:11:4);
                 Dims(3) = %dec(Height:11:4);
                 sorta Dims;
                 Length = %char(Dims(1));
                 Width = %char(Dims(2));
                 Height = %char(Dims(3));

                 exsr $CrtCont;

           endsr;
       // ============================================================
       // Get the Item Information for ShipOptimizer
       // ============================================================
           begsr $GetItemInfo;

                 OrdQty = %dec(OrderQty:9:0);

                 ITEMDIMWGT (Item:
                             OrdQty:
                             PCqty:
                             PCcnt:
                             PCLength:
                             PCWidth:
                             PCHeight:
                             PCWeight:
                             PCCubes:
                             PCUom:
                             PCMastPck:
                             MPQty:
                             MPCQty:
                             MPcnt:
                             MPLength:
                             MPWidth:
                             MPHeight:
                             MPWeight:
                             MPCubes:
                             MPUom:
                             MPMastPck:
                             CPQty:
                             CPCQty:
                             CPcnt:
                             CPLength:
                             CPWidth:
                             CPHeight:
                             CPWeight:
                             CPCubes:
                             CPUom:
                             CPMastPck:
                             Company:
                             WareHouse:
                             Bulk:
                             ExtWeight:
                             FreightClass:
                             FAKClass);

           endsr;
       // ============================================================
       // Create Container Record
       // ============================================================
           begsr $CrtCont;

                 chain (Company:VarOrder:VarCont) vpcont;
                 if %found(vpcont);
                    cxwgt = cxwgt + %dec(Weight:11:4);
                    update vpcontr;
                 else;
       // Create New Carton
                    clear vpcontr;

                    cxcntp = 'BOX';
               //   cxwgt = %dec(Weight:7:4);
                    cxwgt = %dec(Weight:11:4);
                    cxship = VarOrder;
                    cxcmno = Company;
                    cxlen = %dec(Length:7:4);
                    cxwdt = %dec(Width:7:4);
                    cxhgt = %dec(Height:7:4);
                    cxcubc = cxlen * cxwdt * cxhgt;
                    cxfrtc = FAKClass;
                    cxcnid = VarCont;
                    if PkgID = *blanks;
                       chain (cxlen:cxwdt:cxhgt) pkguntl1;
                       if %found(pkguntl1);
                          cxcode = pkunit;
                       else;
                          cxcode = 'VP';
                       endif;
                    else;
                       cxcode = PkgID;
                    endif;
                    write vpcontr;
                    ContRec = @yes;
                 endif;

           endsr;
       // ============================================================
       // Get PalletCnt
       // ============================================================
           begsr $PalletCnt;

                 clear Wk_PltCnt;
                 clear PalletCnt;

                 exec sql
                 select count(*) into :Wk_PltCnt
                  from hypsgdt a where gdgrp#=:ShipGrp# and gdblto=:billto# and
                  gdshto=:shipto# and gddshp=:dropship# and
                  exists (select * from hypscwk b where
                  b.s1srl#=a.gdsscc and s1psrl#=0);

                 if Wk_PltCnt= *zeros;
                    exec sql
                    select count(distinct s1psrl#) into :PalletCnt
                     from hypsgdt a join hypscwk b on a.gdsscc=b.s1srl# where
                     gdgrp#=:ShipGrp# and gdblto=:billto# and gdshto=:shipto#
                     and gddshp=:DropShip# and b.s1psrl#<>0;
                 endif;

           endsr;
      /End-free
