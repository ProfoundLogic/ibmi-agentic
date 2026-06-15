     H DATFMT(*ISO) TIMFMT(*ISO) OPTION(*SrcStmt: *NoDebugIO)
     H COPYRIGHT('Varsity Logistics, INC.  Chicago, IL.  2021')
     H DFTACTGRP (*NO) ACTGRP('HDSOPT') BNDDIR('VARHDINFD')
     **********************************************************************
     * Get Dims., Weights and Allowed to Master Pack  cubarsity
     *
     * Program Name: VPRDIMSWGT
     *
     * Author: Kevin Blackmon - Varsity Logistics, Inc.
     **********************************************************************
     *------------------------MODIFICATION LOG-----------------------------
     * DATE      PGMR         REMARKS
     *
     *
     *------------------------MODIFICATIONS END----------------------------
     * Item File
     fhdimst    if   e           k disk
     * Item File - Hornady Fields
     fhdimst2   if   e           k disk
     * Item Warehouse
     fhdiwhs    if   e           k disk
     * BARDATA-BAR CODE DDS **new FORMAT 10
     fbardata   if   e           k disk
     * Items that fit into Envelopes
     fvpritmenv if   e           k disk
     * Varsity Container File
     fvpcntr    if   e           k disk
     * Varsity's GENERAL -- Codes Details
     fgncodp    if   e           k disk
     **********************************************************************
     * Calling Parameters
     **********************************************************************
     d VPRDMWTCT       PI
     d  Item                         15
     d  OrdQty                        9  0
     d  PCQty                         7  0
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
     **********************************************************************
     * Program Defined Alphanumeric Fields
     **********************************************************************
     d @yes            s              1    inz('Y')
     d @no             s              1    inz('N')
     d ErrLen          s             11  4 inz(99.99)
     d ErrWdt          s             11  4 inz(99.99)
     d ErrHgt          s             11  4 inz(99.99)
     d ErrWgt          s             11  4 inz(999.9)
     d ErrCub          s             11  4 inz(9999999.0)
     d LenErr          s              1
     d WdtErr          s              1
     d HgtErr          s              1
     d WgtErr          s              1
     d Length          s             11  4
     d Width           s             11  4
     d Height          s             11  4
     d Rem             s              7  0
     d wk_pscpn        s             15
     d wk_psppn        s             15
     d Cubes           s             16  4
     d CPFlag          s              1
     d MPFlag          s              1
     d DimError        s              1
     d ItemDims        s              1
     d rnd             s              3  2 inz(.49)
     d pos             s              2  0
     d PalletCode      s              5    inz('PL')
     d Pallet          s              1
     d Blk_imitem      s                   like(imitem)
     d Wk_imitem       s                   like(imitem)
     d Wk_impcls       s                   like(impcls)
     d PartPlt         s              1
     d Grouping        s                   like(cdgrid)
     d GrpCarr         s                   like(cdcrcd)
     d GrpCode         s                   like(cdcode)
     d overridepcpc    s              1
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
     *-------------------------------------------------------------------*
     * Program Arrays
     *-------------------------------------------------------------------*
     d dims            s             11  4 dim(3) DESCEND
     **********************************************************************
     * Procedures
     **********************************************************************
      /Free
       // ============================================================
       // Main Line Process
       // ============================================================
             ItemDims = @no;
             Pallet = @no;
             Bulk = @no;
             clear PCcnt;
             clear PCLength;
             clear PCWidth;
             clear PCHeight;
             clear PCWeight;
             clear PCCubes;
             clear PCUom;
             clear PCMastPck;
             clear MPQty;
             clear MPCQty;
             clear MPcnt;
             clear MPLength;
             clear MPWidth;
             clear MPHeight;
             clear MPWeight;
             clear MPCubes;
             clear MPUom;
             clear MPMastPck;
             clear CPQty;
             clear CPCQty;
             clear CPcnt;
             clear CPLength;
             clear CPWidth;
             clear CPHeight;
             clear CPWeight;
             clear CPCubes;
             clear CPUom;
             clear CPMastPck;

         // If Warehouse is equal to 001 and no dimensions then make
         //  Dimensions small and weight small
            if WareHouse = 0 or Warehouse = 1 or Warehouse = 3 or
               WareHouse = 4;
               exsr $ItemDims;
            endif;

         // If Item belongs in an envelope check dimensions and weight
         //  If no Dimensions or weight make them small
            if ItemDims = @no;
               chain (Item) vpritmenv;
               if %found(vpritmenv);
                  exsr $ItemDims;
               endif;
            endif;

        // If Item is a Warranty Item make small dimensions
            if ItemDims = @no;
               if Item = 'NONSTKWGT';
                  exsr $ItemDims;
                  if ExtWeight > *zeros;
                     PCWeight = ExtWeight;
                  endif;
               endif;
           endif;

        // If Item is a Non-Entry Item
            if ItemDims = @no and OrdQty = *zeros;
                  exsr $ItemDims;
           endif;

        // Determine If a Bulk Item or not
            if ItemDims = @no;
        // Break out Packing Units
               chain (Item) hdimst;
               if %found(hdimst);
                  clear blk_imitem;
                  wk_imitem = Item;
                  wk_impcls = impcls;

                  exec sql
                   select a.imitem into :blk_imitem from hdimst
                    a where a.imitem = :wk_imitem and
                    a.impcls = :wk_impcls and exists
                    (select * from GUPTDAT b where
                     b.TDTABL='BULKPRDCLS' and b.TDKEY1=a.impcls);

                  if blk_imitem = *blanks;
                     exsr $PackUnits;
                  else;
                     exsr $BulkPackUnits;
                  endif;
               endif;
            endif;
        // Determine if Master Packed is Allowed
               exsr $VarMasterPack;

        // Determine if Master Packed is Allowed
               exsr $GetFrgtClass;

           *inlr = *on;
       // ============================================================
       // On Certain Conditions - Dims or Weight - make small to pack
       //    in one box.
       // ============================================================
           begsr $ItemDims;

                 ItemDims = @yes;

                 chain (Item) hdimst;

                 if %found(hdimst);

                    if imuda2 <> *blanks;
                       Height = %dec(imuda2:11:4);
                    else;
                       Height = .1;
                    endif;

                    if imuda3 <> *blanks;
                       Width = %dec(imuda3:11:4);
                    else;
                       Width = .1;
                    endif;

                    if imuda4 <> *blanks;
                       Length = %dec(imuda4:11:4);
                    else;
                       Length = .1;
                    endif;

                    PCLength = Length;
                    PCWidth = Width;
                    PCHeight = Height;

                    PCCubes = PCLength * PCWidth * PCHeight;

         // Determine Piece Weight
                    if imudn4 > *zeros;
                       PCWeight = imudn4;
                    endif;

                    if imimwg > *zeros and PCWeight = *zeros;
                       PCWeight = imimwg;
                    endif;

                    if PCWeight = *zeros;
                       PCWeight = .1;
                    endif;

                    PCCnt = Ordqty;

                    if PCCnt > *zeros;
                       PCUom = 'EA';
                    endif;
                 endif;

                 if Height = *zeros or Width = *zeros or
                    Length = *zeros or PCWeight = *zeros;
                    Height = .1;
                    Width = .1;
                    Length = .1;
                    PCLength = Length;
                    PCWidth = Width;
                    PCHeight = Height;
                    PCCubes = PCLength * PCWidth * PCHeight;
                    PCWeight = .1;
                    PCUom = 'EA';
                 endif;

           endsr;
       // ============================================================
       // Break out Packing Units by Quantity - Non Bulk Items
       // ============================================================
           begsr $PackUnits;

         // Master Pack Qty
                 chain (Item) bardata;
                 if %found(bardata);
                    MPQty = bdqtybar;
                    MPCQty = bdqtybar;
                 endif;

         // Carton Qty
                 chain (Item:Warehouse) hdiwhs;
                 if %found(hdiwhs);
                    CPQty = iwpcpc;
                    CPCQty = iwpcpc;
                 endif;

         // Determine How Many Master Packs
                 if MPQty > *zeros and OrdQty >= MPQty;
                    MPCnt = %div(OrdQty:MPQty);
                    Rem = %rem(OrdQty:MPQty);
                    MPFlag = @yes;
                 endif;

         // Determine How Many Case Cartons
                 if MPFlag = @yes;
                    if (CPQty > *zeros and Rem > CPQty);
                       CPCnt = %div(Rem:CPQty);
                    endif;
                    CPFlag = @yes;
                 else;
                    if (CPQty > *zeros and OrdQty >= CPQty);
                       CPCnt = %div(OrdQty:CPQty);
                       Rem = %rem(OrdQty:CPQty);
                    CPFlag = @yes;
                    endif;
                 endif;

         // Determine How Many Pieces
                 if MPFlag = @yes or CPFlag = @yes;
                    if (Rem > *zeros) or (OrdQty < MPQty) or
                       (OrdQty < CPQty);
                       PCCnt = Rem;
                    endif;
                 else;
                    PCCnt = Ordqty;
                 endif;

         // Determine if it is a full pallet
                 chain (Item) hdimst2;
                 if %found(hdimst2);
                    PCQty = impltq * iwpcpc;
                    if PCQty = OrdQty;
                       PCQty = 1;
                       PCCnt = 1;
                       clear CPqty;
                       clear CPCnt;
                       clear CPCqty;
                       clear MPQty;
                       clear MPCQty;
                       Pallet = @yes;
                    endif;
                 endif;

                 if Pallet = @no;
                    exsr $ItemInfo;
                 else;
                    exsr $PackInfo;
                 endif;

           endsr;
       // ============================================================
       // Get Item Length, Width, Height and Weight
       // ============================================================
           begsr $ItemInfo;

                 LenErr = @no;
                 WdtErr = @no;
                 HgtErr = @no;
                 WgtErr = @no;

                 chain (Item) hdimst;
                 if %found(hdimst);
                    if imuda2 <> *blanks;
                       Height = %dec(imuda2:11:4);
                    else;
                       HgtErr = @yes;
                    endif;
                    if imuda3 <> *blanks;
                       Width = %dec(imuda3:11:4);
                    else;
                       WdtErr = @yes;
                    endif;
                    if imuda4 <> *blanks;
                       Length = %dec(imuda4:11:4);
                    else;
                       LenErr = @yes;
                    endif;
                    if LenErr = @yes or WdtErr = @yes or
                       HgtErr = @yes;
                       Length = ErrLen;
                       Width = ErrWdt;
                       Height = ErrHgt;
                    endif;
                    exsr $FormatDims;
                    PCLength = Length;
                    PCWidth = Width;
                    PCHeight = Height;
         // Determine Piece Weight
                    if imudn4 > *zeros;
                       PCWeight = imudn4;
                    endif;

                    if imimwg > *zeros and PCWeight = *zeros;
                       PCWeight = imimwg;
                    endif;

                    if PCWeight = *zeros;
                       PCWeight = .1;
                    endif;

                    PCCubes = Cubes;
                    if PCCnt > *zeros;
                       PCUom = 'EA';
                    endif;

                    exsr $PackInfo;

                 endif;

           endsr;
       // ============================================================
       // Item Length, Width, Height and Weight
       // ============================================================
           begsr $PackInfo;

           // Get Hornady Master Pack Dimensions and Weight
                 if MPCnt > *zeros;
                    chain (item) hdimst2;
                    if %found(hdimst2);
                       if immdep > *zeros;
                          Length = immdep;
                       else;
                          LenErr = @yes;
                       endif;
                       if immwid > *zeros;
                          Width = immwid;
                       else;
                          WdtErr = @yes;
                       endif;
                       if immhgt > *zeros;
                          Height = immhgt;
                       else;
                          HgtErr = @yes;
                       endif;
                       if LenErr = @yes or WdtErr = @yes or
                          HgtErr = @yes;
                          Length = ErrLen;
                          Width = ErrWdt;
                          Height = ErrHgt;
                       endif;
                       exsr $FormatDims;
                       MPLength = Length;
                       MPWidth = Width;
                       MPHeight = Height;
                       if PCWeight <> ErrWgt;
                          MPWeight = PCWeight * MPQty;
                       else;
                          MPWeight = ErrWgt;
                       endif;
                       MPCubes = Cubes;
                       MPUom = 'MP';
                    endif;
                 endif;

           // Retrieving carton size from Bill of Material
                 if CPCnt > *zeros;

                    wk_psppn = item;
                    clear wk_pscpn;

                    exec sql
                     select a.pscpn into :wk_pscpn from HDMPSM a
                      where a.psppn = :wk_psppn and
                       exists (select * from GUPTDAT b where
                       b.TDTABL = 'SHIPCTN' and b.TDKEY1 = a.PSCPN);

                    if wk_pscpn > *blanks;
                       chain (wk_pscpn) hdimst;
                       if %found(hdimst);
                          if imuda2 <> *blanks;
                             Height = %dec(imuda2:11:4);
                          else;
                             HgtErr = @yes;
                          endif;
                          if imuda3 <> *blanks;
                             Width = %dec(imuda3:11:4);
                          else;
                             WdtErr = @yes;
                          endif;
                          if imuda4 <> *blanks;
                             Length = %dec(imuda4:11:4);
                          else;
                             LenErr = @yes;
                          endif;
                          if LenErr = @yes or WdtErr = @yes or
                             HgtErr = @yes;
                             Length = ErrLen;
                             Width = ErrWdt;
                             Height = ErrHgt;
                          endif;
                          exsr $FormatDims;
                          CPLength = Length;
                          CPWidth = Width;
                          CPHeight = Height;
                          if PCWeight <> ErrWgt;
                             CPWeight = PCWeight * CPQty;
                          else;
                             CPWeight = ErrWgt;
                          endif;
                          CPCubes = Cubes;
                          CPUom = 'CP';
                       endif;
                    else;
           // Retrieving carton size Item Master Two - No BOM
                       chain (item) hdimst2;
                       if %found(hdimst2);
                          if imcdep > *zeros;
                             Length = imcdep;
                          else;
                             LenErr = @yes;
                          endif;
                          if imcwid > *zeros;
                             Width = imcwid;
                          else;
                             WdtErr = @yes;
                          endif;
                          if imchgt > *zeros;
                             Height = imchgt;
                          else;
                             HgtErr = @yes;
                          endif;
                          if LenErr = @yes or WdtErr = @yes or
                             HgtErr = @yes;
                             Length = ErrLen;
                             Width = ErrWdt;
                             Height = ErrHgt;
                          endif;
                          exsr $FormatDims;
                          CPLength = Length;
                          CPWidth = Width;
                          CPHeight = Height;
                          if PCWeight <> ErrWgt;
                             CPWeight = PCWeight * CPQty;
                          else;
                             CPWeight = ErrWgt;
                          endif;
                          CPCubes = Cubes;
                          CPUom = 'CP';
                       endif;
                    endif;
                 endif;

           // Retrieving Pallet size
                 if Pallet = @yes;
                    chain (Company:PalletCode) vpcntr;
                    if %found(vpcntr);
                       if cnlen > *zeros;
                          Length = cnlen;
                       else;
                          LenErr = @yes;
                       endif;
                       if cnwdt > *zeros;
                          Width = cnwdt;
                       else;
                          WdtErr = @yes;
                       endif;
                       if cnhgt > *zeros;
                          Height = cnhgt;
                       else;
                          HgtErr = @yes;
                       endif;
                       if LenErr = @yes or WdtErr = @yes or
                          HgtErr = @yes;
                          Length = ErrLen;
                          Width = ErrWdt;
                          Height = ErrHgt;
                       endif;
                       exsr $FormatDims;
                       PCLength = Length;
                       PCWidth = Width;
                       PCHeight = Height;
                       PCCubes = Cubes;
                       PCUom = 'PL';
                       if ExtWeight > *zeros;
                          PCWeight = ExtWeight;
                       else;
                          PCWeight = imimwg;
                       endif;
                    endif;
                 endif;

           endsr;
       // ============================================================
       // Break out Packing Units by Quantity - Bulk Items
       // ============================================================
           begsr $BulkPackUnits;

                 Bulk = @yes;
                 PartPlt = @no;

         // Case Qty
                 overridepcpc = @no;
                 chain (Item:Warehouse) hdiwhs;
                 if %found(hdiwhs);
                    if iwpcpc = *zeros;
                       iwpcpc = 1;
                       overridepcpc = @yes;
                    endif;
             //     if OrdQty = iwpcpc;
             //        CPQty = 1;
             //        CPCnt = 1;
             //        CPCQty = 1;
             //     endif;

         // Determine if it is a full pallet
                    chain (Item) hdimst2;
                    if %found(hdimst2);
                       PCQty = impltq * iwpcpc;
                       if PCQty = OrdQty;
                          PCQty = 1;
                          PCCnt = 1;
                          clear CPqty;
                          clear CPCnt;
                          clear CPCqty;
                          Pallet = @yes;
                       else;
                          CPCnt = OrdQty/iwpcpc;
                          chain (Item) hdimst;
                          if %found(hdimst);
                      //  if CPCnt = 1;
                             CPQty = iwpcpc;
                             CPCQty = iwpcpc;
                             if imimwg > *zeros;
                                PCWeight = imudn4;
                             endif;
                          endif;
                          if PCWeight = *zeros;
                             PCWeight = .1;
                          endif;
                      //  else;
                      //     PCQty = 1;
                      //     PCCnt = 1;
                      //     clear CPqty;
                      //     clear CPCnt;
                      //     clear CPCqty;
                      //     Pallet = @yes;
                      //     PartPlt = @yes;
                      //  endif;
                       endif;
                    endif;
                    if overridepcpc = @yes;
                       overridepcpc = @no;
                       iwpcpc = *zeros;
                    endif;
                 endif;

                 exsr $BulkItemInfo;

           endsr;
       // ============================================================
       // Get Item Length, Width, Height and Weight for Bulk Items
       // ============================================================
           begsr $BulkItemInfo;

                 LenErr = @no;
                 WdtErr = @no;
                 HgtErr = @no;
                 WgtErr = @no;

           // Retrieving carton size Item Master Two - Bulk
                 if CPCnt > *zeros;
                    chain (item) hdimst2;
                    if %found(hdimst2);
                       if imcdep > *zeros;
                          Length = imcdep;
                       else;
                          LenErr = @yes;
                       endif;
                       if imcwid > *zeros;
                          Width = imcwid;
                       else;
                          WdtErr = @yes;
                       endif;
                       if imchgt > *zeros;
                          Height = imchgt;
                       else;
                          HgtErr = @yes;
                       endif;
                       if LenErr = @yes or WdtErr = @yes or
                          HgtErr = @yes;
                          Length = ErrLen;
                          Width = ErrWdt;
                          Height = ErrHgt;
                       endif;
                       exsr $FormatDims;
                       CPLength = Length;
                       CPWidth = Width;
                       CPHeight = Height;
                       chain (item) hdimst2;
                       if %found(hdimst2);
                          CPWeight = imimwg;
                       endif;
                       if CPWeight = *zeros;
                          CPWeight = ExtWeight;
                       endif;
                       CPCubes = Cubes;
                       CPUom = 'CP';
                    endif;
                 endif;

           // Retrieving Pallet size
                 if PCCnt > *zeros;
                    chain (Company:PalletCode) vpcntr;
                    if %found(vpcntr);
                       if cnlen > *zeros;
                          Length = cnlen;
                       else;
                          LenErr = @yes;
                       endif;
                       if cnwdt > *zeros;
                          Width = cnwdt;
                       else;
                          WdtErr = @yes;
                       endif;
                       if cnhgt > *zeros;
                          Height = cnhgt;
                       else;
                          HgtErr = @yes;
                       endif;
                       if LenErr = @yes or WdtErr = @yes or
                          HgtErr = @yes;
                          Length = ErrLen;
                          Width = ErrWdt;
                          Height = ErrHgt;
                       endif;
                       exsr $FormatDims;
                       PCLength = Length;
                       PCWidth = Width;
                       PCHeight = Height;
                       PCCubes = Cubes;
                       PCUom = 'PL';
                       chain (Item:Warehouse) hdiwhs;
                       if %found(hdiwhs);
                          chain (Item) hdimst;
                          if %found(hdimst);
                             PCWeight =  imimwg * impltq;
                             if PCWeight = *zeros;
                                PCWeight = ErrWgt;
                             endif;
                          endif;
                       endif;
                       if PCWeight = *zeros;
                          PCWeight = ExtWeight;
                       endif;
                    endif;
                 endif;
                 if PartPlt = @yes;
                    PCWeight = imudn4;
                 endif;

           endsr;
       // ============================================================
       // Format Dimension for Varsity
       // ============================================================
           begsr $FormatDims;

                 clear Dims;
                 clear Cubes;

                 Dims(1) = Length;
                 Dims(2) = Width;
                 Dims(3) = Height;

                 sorta Dims;

                 Length = Dims(1);
                 Width = Dims(2);
                 Height = Dims(3);

                 if Length = ErrLen or Width = ErrWdt or
                    Height = ErrHgt;
                      Cubes = ErrCub;
                 else;
                      Cubes = Length * Width * Height;
                 endif;

                 LenErr = @no;
                 WdtErr = @no;
                 HgtErr = @no;

           endsr;
       // ============================================================
       // Can be Master Packed
       // ============================================================
           begsr $VarMasterPack;

                 PCMastPck = @no;
                 MPMastPck = @no;
                 CPMastPck = @no;
                 DimError = @no;

                 if Length = ErrLen or Width = ErrWdt or Height = ErrHgt or
                    PCWeight = ErrWgt;
                    DimError = @yes;
                 endif;

                 if DimError = @no;
                    chain (item) hdimst2;
                    if %found(hdimst2);

                       If imcbmp = 'Y' or imcbmp = 'M';
                          PCMastPck = @yes;
                          MPMastPck = @yes;
                          CPMastPck = @yes;
                       endif;
                    endif;
                 endif;

                 if Pallet = @yes;
                    PCMastPck = @no;
                 endif;

           endsr;
       // ============================================================
       // Get Freight Class
       // ============================================================
           begsr $GetFrgtClass;

                 chain (item) hdiwhs;
                 if %found(hdiwhs);

                    Grouping = '$FRGTCLASS';
                    GrpCarr = *blanks;
                    GrpCode = iwftcl;
                    FreightClass = iwftcl;

                    chain (Grouping:GrpCarr:GrpCode) gncodp;
                    if %found(gncodp);
                       FAKClass = cdalcd;
                    endif;
                 endif;

           endsr;
      /End-Free
