     H DATFMT(*ISO) TIMFMT(*ISO) OPTION(*SrcStmt: *NoDebugIO)
     H COPYRIGHT('Varsity Logistics, INC.  Chicago, IL.  2021')
     H DFTACTGRP (*NO) ACTGRP('HDSOPT') BNDDIR('VARHDINFD')
     **********************************************************************
     * ShipSoft SWOG Interface for Mixed Items
     *
     * Program Name: VPRSWOGINF
     *
     * Author: Kevin Blackmon - Varsity Logistics, Inc.
     **********************************************************************
     *------------------------MODIFICATION LOG-----------------------------
     * DATE      PGMR         REMARKS
     * 09/18/24  MCA          Pass master order number seperate from array.
     *                        Note: Array element 1 is still expected to be
     *                        the same order as the passed master order no.
     *
     *------------------------MODIFICATIONS END----------------------------
     * Order Header File
     foeorhd    if   e           k disk
     * Master Location File
     fhdlctn    if   e           k disk
     * Customer Master File
     fhdcust    if   e           k disk
     * Drop Ship File
     fhddshp03  if   e           k disk
     * Varsity's SWOG Master Order File
     fvsswogmsp uf a e           k disk
     * Varsity's SWOG Child Order File
     fvsswogchp uf a e           k disk
     **********************************************************************
     * Parameters
     **********************************************************************
     d VPRSWOGINF      PI
     d  MstOrd#                      20A
     d  Orders                        8  0 dim(600)
     **********************************************************************
     * Program Calls
     **********************************************************************
     **********************************************************************
     * Program Defined Alphanumeric Fields
     **********************************************************************
     d @yes            s              1    inz('Y')
     d @no             s              1    inz('N')
     d indx            s              3  0
     d idx             s              3  0
     d i               s              3  0
     d OrderNum        s              8  0
     d Company         s                   like(swmcom)
     d OrdType         s                   like(swmotp)
     d                                     inz(*blank)
     d Shipment        s                   like(swmord)
     d ChildOrd        s                   like(swccor)
     d today           s               d   inz(*sys)
     d                                     datfmt(*ISO)
     d thetime         s              6  0
     **********************************************************************
     * Program Defined Constants
     **********************************************************************
     D at_Attention    C                   CONST('A')                           Addr type: Attention
     D at_POBox        C                   CONST('P')                           Addr type: P.O. Box
     D at_Spacer       C                   CONST('S')                           Addr type: Spacer
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
     *
     d                 ds
     d TheDate                        8  0
     d  ccyy                          4  0 overlay(thedate)
     d  mm                            2  0 overlay(thedate:5)
     d  dd                            2  0 overlay(thedate:7)
     **********************************************************************
     * Procedures
     **********************************************************************
     D spcladdress     PR             1A
     D   address                     26A     VALUE
      /Free
       // ============================================================
       // Main Line Process
       // ============================================================
           // Determine How Many Orders are being SWOGed
               indx = %lookup(*zeros:Orders);
               if indx > 2  and  MstOrd# <> *Blanks;
                  exsr $GetOrdInfo;
               endif;

               *inlr = *on;
       // ============================================================
       // Get the Order Master Header Information
       // ============================================================
           begsr $GetOrdInfo;

           // First Order will be the Master Order Number
                 idx = 1;
                 Ordernum = Orders(idx);

                 chain (OrderNum) oeorhd;
                 if %found(oeorhd);
                    chain (oeloc#) hdlctn;
                    if %found(hdlctn);
                       Company = %EditC(loco#:'X');
               //      Shipment = %char(OrderNum);
                       Shipment = MstOrd#;
                       exsr $LoadMstOrder;
                    endif;
                 endif;

           endsr;
       // ============================================================
       // Load Master Order Number
       // ============================================================
           begsr $LoadMstOrder;

                 chain(n) (Company:OrdType:Shipment) vsswogmsp;
                 if not %found(vsswogmsp);

                    clear vsswogmsr;

                    swmcom = Company;
              // Master Order Number
                    swmord = Shipment;
              // Number of Orders to SWOG
                    swmoct = (indx - 1);
              // Type of Interface - E = ERP System
                    swmifl = 'E';
              // Pro-ration Flag - O = By Order
                    swmprf = 'O';
              // Name and Address
                    exsr $ShipToInfo;

              // Shipping Instruction
                    swmspi = oesvsv;

              // User/Time/Date/Display
                    TheTime = %int(%char(%time():*hms0));
                    swmcrd = TheDate;

                    write vsswogmsr;
                 endif;

                 exsr $LoadChildOrds;

           endsr;
       // ============================================================
       // Get Ship-To Information
       // ============================================================
           begsr $ShipToInfo;

                 chain (oeshto) hdcust;
                 if %found(hdcust);

                    swmsnm = cmcna1;

                    if cmcna2 <> *blanks and
                       spcladdress(cmcna2) = at_POBOX;
                         if cmcna3 = *blanks and cmcna4 = *blanks;
                         else;
                            cmcna2 = *blanks;
                         endif;
                    endif;

                    if cmcna3 <> *blanks and
                       spcladdress(cmcna3) = at_POBOX;
                         cmcna3 = *blanks;
                    endif;

                    if cmcna4 <> *blanks and
                       spcladdress(cmcna4) = at_POBOX;
                         cmcna4 = *blanks;
                    endif;

                    select;
                    when cmcna2 <> *blanks;
                         swmsa1 = cmcna2;
                         swmsa2 = cmcna3;
                         swmsa3 = cmcna4;
                    when cmcna2 = *blanks;
                         swmsa1 = cmcna3;
                         swmsa2 = cmcna4;
                    endsl;

                    swmsct = cmccty;
                    swmsst = cmst;

                    if cmctry = *blanks;
                       swmsco = 'US';
                    else;
                       swmsco = cmctry;
                    endif;

                    if swmsco = 'US';
                       swmszp = %subst(cmzip:1:5);
                    endif;

                    if swmsco = 'CA';
                       if %subst(cmzip:4:1) = *blanks;
                          swmszp = %subst(cmzip:1:3) +
                                   %subst(cmzip:5:3);
                       else;
                          swmszp = cmzip;
                       endif;
                    endif;
                 else;
                    chain (oedshp) hddshp03;
                    if %found(hddshp03);

                       swmsnm = dsname;

                       if dsadr1 <> *blanks and
                          spcladdress(dsadr1) = at_POBOX;
                         if dsadr2 = *blanks and dsadr3 = *blanks;
                         else;
                            dsadr1 = *blanks;
                         endif;
                       endif;

                       if dsadr2 <> *blanks and
                          spcladdress(dsadr2) = at_POBOX;
                            dsadr2 = *blanks;
                       endif;

                       if dsadr3 <> *blanks and
                          spcladdress(dsadr3) = at_POBOX;
                            dsadr3 = *blanks;
                       endif;

                       select;
                       when dsadr1 <> *blanks;
                            swmsa1 = dsadr1;
                            swmsa2 = dsadr2;
                            swmsa3 = dsadr3;
                       when dsadr2 = *blanks;
                            swmsa1 = dsadr2;
                            swmsa2 = dsadr3;
                       endsl;

                       swmsct = dscity;
                       swmsst = dsst;

                       if dsctry = *blanks;
                          swmsco = 'US';
                       else;
                          swmsco = dsctry;
                       endif;

                       if swmsco = 'US';
                          swmszp = %subst(dszip:1:5);
                       endif;

                       if swmsco = 'CA';
                          if %subst(dszip:4:1) = *blanks;
                             eval swmszp = %subst(dszip:1:3) +
                                           %subst(dszip:5:3);
                          else;
                             swmszp = dszip;
                          endif;
                       endif;
                    endif;
                 endif;

           endsr;
       // ============================================================
       // Load Child Orders
       // ============================================================
           begsr $LoadChildOrds;

                 indx = indx - 1;

                 for i = 2 to indx;
                     ChildOrd = %char(Orders(i));
                     chain(n) (Company:OrdType:Shipment:ChildOrd) vsswogchp;
                     if not %found(vsswogchp);
                        clear vsswogchr;
                        swccom = Company;
                        swcotp = *blanks;
                        swcmor = Shipment;
                        swccor = ChildOrd;
                        swcifl = 'E';
                        write vsswogchr;
                     endif;
                 endfor;

           endsr;
       // ============================================================
       // Initialization Subroutine
       // ============================================================
           begsr *inzsr;

                 ccyy = %subdt(today:*Y);
                 mm = %subdt(today:*M);
                 dd = %subdt(today:*D);

           endsr;
      /End-free
       // ============================================================
       // Check for Special Address Field.  Return a Character Field
       // ============================================================
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
     C                   ENDSL
     C                   RETURN    addresstype
     P spcladdress     E
