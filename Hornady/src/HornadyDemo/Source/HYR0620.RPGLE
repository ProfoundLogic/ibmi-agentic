     H COPYRIGHT('(C) Hornady Manufacturing Company, 2018')
     H BNDDIR('HYR0620')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Create ZPL label string for UCC-128 labels                *
      *          BY: Mark Hajny                                                *
      *        DATE: July 2018                                                 *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  10/26/2018  MH   Added the orientation string back in.                *
      *  04/04/2019  MCA  Flip ds_CurItem and ds_CustItem use.                 *
      *  09/18/2024  MH   Change some formatting around the UPC code based     *
      *                   on a request from Sports South.                      *
      *  09/25/2024  MH   Removed upper right (BOL,PRO,etc) section.           *
      *  09/26/2024  MH   Reduced font size for VPN UPC section.               *
      *  01/26/2026  MCA  Fix bug - one ZPL line had FSS instead of FS.        *
      *                                                                        *
      **************************************************************************

     Hnomain

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D ZPLCode         PR         32000A
     D  in_DS                              LIKEDS(PARMDS)

      **************************************************************************
      * Parameter Data Structure.                                              *
      **************************************************************************
     D PARMDS          DS
     D  ds_SFName              1     50
     D  ds_SFAddr1            51    100
     D  ds_SFAddr2           101    150
     D  ds_SFAddr3           151    200
     D  ds_CustVend          201    215
     D  ds_SCAC              216    225
     D  ds_ShipVia           226    250
     D  ds_PRONum            251    275
     D  ds_BOLNum            276    300
     D  ds_OrderNum          301    310
     D  ds_STName            311    360
     D  ds_STAddr1           361    410
     D  ds_STAddr2           411    460
     D  ds_STAddr3           461    510
     D  ds_STCity            511    540
     D  ds_STState           541    545
     D  ds_STZip             546    560
     D  ds_STCountry         561    590
     D  ds_StoreNum          591    615
     D  ds_PONum             616    640
     D  ds_CurItem           641    670
     D  ds_CustItem          671    700
     D  ds_CurItemUPC        701    725
     D  ds_CurItemDes        726    775
     D  ds_PriceUOM          776    780
     D  ds_Qty               781    795
     D  ds_CtnCount          796    825
     D  ds_DeptNum           826    850
     D  ds_DeptName          851    880
     D  ds_SSCCSerRef        881    930
     D  ds_Ext1              931    980
     D  ds_Ext2              981   1030
     D  ds_Ext3             1031   1080
     D  ds_Ext4             1081   1130
     D  ds_Ext5             1131   1180

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_OutString    S          32000A
     D wk_ShutDown     S              1N
     D eol             S              1A
     D wk_UCCtxt       S             50A
     D wk_UCCtemp      S             50A

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D CrLf            C                   x'0D25'
     D*HeaderCodes     C                   '^XA^MMT^POI^PW831^LL1244^LS0'
     D HeaderCodes     C                   '^XA^POI'
     D TrailerCodes    C                   '^PQ1,0,1,Y^XZ'

      **************************************************************************
      * Return ZPL code for the label data passed in the data structure.       *
      **************************************************************************

     P ZPLCode         B                   EXPORT
     D                 PI         32000A
     D  in_DS                              LIKEDS(PARMDS)

     C                   EVAL      PARMDS = in_DS
     C                   EVAL      wk_OutString = *Blanks
     C*                  EVAL      eol = *Blanks
     C                   EVAL      eol = CrLf

     C                   EVAL      wk_OutString = HeaderCodes + eol

      *
      *  From section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT13,36^A0N,23,24^FH\^FDFROM:^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT92,41^A0N,28,28^FH\^FD' +
     C                             %trim(ds_SFName) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT92,71^A0N,28,28^FH\^FD' +
     C                             %trim(ds_SFAddr1) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT92,102^A0N,28,28^FH\^FD' +
     C                             %trim(ds_SFAddr2) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT92,135^A0N,28,28^FH\^FD' +
     C                             %trim(ds_SFAddr3) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT90,159^A0N,23,24^FH\^FDHMC ORDER #:^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT244,162^A0N,28,28^FH\^FD' +
     C                             %trim(ds_OrderNum) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT10,75^A0N,23,24^FH\^FD' +
     C                             %trim(ds_CustVend) +
     C                             '^FS' + eol

      *
      *  Carrier section.
      *
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT440,41^A0N,28,28^FH\^FDCARRIER:^FS'
     C*                            + eol
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT566,40^A0N,28,28^FH\^FD' +
     C*                            %trim(ds_ShipVia) +
     C*                            '^FS' + eol
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT440,79^A0N,23,24^FH\^FDSCAC:^FS'
     C*                            + eol
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT509,79^A0N,25,24^FH\^FD' +
     C*                            %trim(ds_SCAC) +
     C*                            '^FS' + eol
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT440,110^A0N,23,24^FH\^FDPRO#:^FS'
     C*                            + eol
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT509,113^A0N,25,24^FH\^FD' +
     C*                            %trim(ds_PRONum) +
     C*                            '^FS' + eol
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT440,140^A0N,23,24^FH\^FDBOL#:^FS'
     C*                            + eol
     C*                  EVAL      wk_OutString = %trim(wk_OutString) +
     C*                            '^FT509,144^A0N,25,24^FH\^FD' +
     C*                            %trim(ds_BOLNum) +
     C*                            '^FS' + eol

      *
      *  Ship To section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT12,208^A0N,23,24^FH\^FDTO:^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT56,211^A0N,39,38^FH\^FD' +
     C                             %trim(ds_STName) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT56,245^A0N,39,38^FH\^FD' +
     C                             %trim(ds_STAddr1) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT56,281^A0N,39,38^FH\^FD' +
     C                             %trim(ds_STAddr2) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT56,315^A0N,39,38^FH\^FD' +
     C                             %trim(ds_STAddr3) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT56,342^A0N,28,28^FH\^FD' +
     C                             %trim(ds_STCity) + '               '  +
     C                             %trim(ds_STState) + '      '  +
     C                             %trim(ds_STZip) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT56,367^A0N,28,28^FH\^FD' +
     C                             %trim(ds_STCountry) +
     C                             '^FS' + eol

      *
      *  Ship To Post section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT35,519^A0B,23,24^FH\^FDSHIP TO POST^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT59,411^A0N,34,33^FH\^FD' +
     C                             %trim(ds_STZip) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^BY1,3,97^FT60,516^B3N,N,,N,N' +
     C                             '^FD' + %trim(ds_STZip) + '^FS'
     C                             + eol

      *
      *  Store Number section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT532,504^A0B,23,24^FH\^FDTO STORE #^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT549,411^A0N,34,33^FH\^FD' +
     C                             %trim(ds_StoreNum) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^BY1,3,97^FT550,514^B3N,N,,N,N' +
     C                             '^FD' + %trim(ds_StoreNum) + '^FS'
     C                             + eol

      *
      *  PO Number section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT36,660^A0B,23,24^FH\^FDP.O. NUMBER^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT59,555^A0N,34,33^FH\^FD' +
     C                             %trim(ds_PONum) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^BY1,3,97^FT57,655^B3N,N,,N,N' +
     C                             '^FD' + %trim(ds_PONum) + '^FS'
     C                             + eol

      *
      *  Item section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT34,746^A0B,23,24^FH\^FDITEM^FS'
     C                             + eol
     C                   IF        ds_CurItem = 'MIXED CARTON'
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT64,723^A0N,56,55^FH\^FDMIXED^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT64,767^A0N,56,55^FH\^FDCARTON^FS'
     C                             + eol
     C                   ELSE
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT48,691^A0N,34,33^FH\^FD' +
     C                             %trim(ds_CustItem) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT48,713^A0N,25,24^FH\^FD' +
     C                             %trim(ds_CurItemDes) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^BY1,3,59^FT48,778^B3N,N,,N,N' +
     C                             '^FD' + %trim(ds_CustItem) + '^FS'
     C                             + eol
     C                   ENDIF                                                  ds_CurItem='MIXED C

      *
      *  Vendor Part Number, UPC Section.
      *
     C                   IF        ds_CurItem = 'MIXED CARTON'
     C                   ELSE
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT471,678^A0N,22,21^FH\^FDVPN #:^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT543,678^A0N,22,21^FH\^FD' +
     C                             %trim(ds_CurItem) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT471,696^A0N,22,21^FH\^FDUPC #:^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT543,696^A0N,22,21^FH\^FD' +
     C                             %trim(ds_CurItemUPC) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^BY2,2,62^FT539,765^BUN,,Y,N' +
     C                             '^FD' + %trim(ds_CurItemUPC) + '^FS'
     C                             + eol
     C                   ENDIF                                                  ds_CuItem='MIXED CAR

      *
      *  Quantity section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT37,902^A0B,23,24^FH\^FDQUANTITY^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT58,826^A0N,34,33^FH\^FD' +
     C                             %trim(ds_Qty) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^BY1,3,75^FT58,907^B3N,N,,N,N' +
     C                             '^FD' + %trim(ds_Qty)  +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT226,825^A0N,34,33^FH\^FDUOM:^FS'
     C                             + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT302,826^A0N,34,33^FH\^FD' +
     C                             %trim(ds_PriceUOM) +
     C                             '^FS' + eol

      *
      *  Carton Quantity and Department section.
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT436,826^A0N,34,33^FH\^FD' +
     C                             %trim(ds_CtnCount) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT439,873^A0N,34,33^FH\^FD' +
     C                             %trim(ds_DeptNum) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT439,900^A0N,34,33^FH\^FD' +
     C                             %trim(ds_DeptName) +
     C                             '^FS' + eol

      *
      *  UCC-128 barcode section.
      *
     C                   EVAL      wk_UCCtemp = %trim(ds_SSCCSerRef)
     C                   EVAL      wk_UCCtxt = '(' + %subst(wk_UCCtemp:1:2)
     C                             + ') ' + %subst(wk_UCCtemp:3:1)
     C                             + %subst(wk_UCCtemp:4:7)
     C                             + ' '
     C                             + %subst(wk_UCCtemp:11:9)
     C                             + ' '
     C                             + %subst(wk_UCCtemp:20:1)
     C                             + ' '
     C                             + %subst(wk_UCCtemp:21:1)
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FT187,962^A0N,39,38^FH\^FD' +
     C                             %trim(wk_UCCtxt) +
     C                             '^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^BY4,3,253^FT83,1231^BCN,,N,N' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FD>;' +
     C                             %trim(ds_SSCCSerRef) +
     C                             '^FS' + eol

      *
      *  Vertical and Horizontal Lines
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO5,167^GB821,0,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO3,783^GB821,0,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO3,908^GB821,0,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO3,658^GB821,0,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO3,522^GB821,0,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO5,370^GB821,0,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO425,4^GB0,166,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO456,660^GB0,126,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO414,785^GB0,125,2^FS' + eol
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             '^FO500,374^GB0,150,2^FS' + eol

      *
      *  Trailer Codes
      *
     C                   EVAL      wk_OutString = %trim(wk_OutString) +
     C                             TrailerCodes + eol

     C                   RETURN    wk_OutString
     P ZPLCode         E
