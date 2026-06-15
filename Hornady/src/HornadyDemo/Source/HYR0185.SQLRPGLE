     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2008')
      **************************************************************************
      * APPLICATION: Shipping Management                                       *
      *       TITLE: Pick Ticket Print Selection Functions                     *
      *          BY: William Earl (MCA)                                        *
      *        DATE: August 2008                                               *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  09/10/2009  MCA  Add SetOrdHold function.                             *
      *  10/26/2009  MCA  Add customer group selection.  Add fields for full   *
      *                   case quantities.                                     *
      *  01/21/2010  MCA  Remove held quantity from onhand quantity.           *
      *  05/24/2010  MCA  Print wholesale pick tickets for SMS quantity only.  *
      *  11/29/2011  MCA  Print OEM wholesale pick tickets for open quantity.  *
      *                   Don't write SMPTQ record for OEM item.               *
      *  03/27/2012  MCA  Changes to facilitate order subset.                  *
      *  07/01/2013  MCA  For ResetOrdPrt, add parameter to control whether    *
      *                   the SMPTQ records are deleted.                       *
      *  05/18/2018  MCA  Don't skip customers on credit hold if customer has  *
      *                   some available credit.                               *
      *  05/15/2019  MCA  Handle manual allocations.                           *
      *  05/28/2019  MCA  Print West DC pick tickets for SMS quantity only.    *
      *  01/26/2022  MCA  For SetLinePrt and SetOrdPrt functions, allow        *
      *                   inventory available retrieval by inventory location. *
      *  04/12/2022  MCA  For non-inventory items, if the order has other ammo *
      *                   blem, ensure this non-inventory item is included in  *
      *                   SMS.                                                 *
      *  08/05/2022  MCA  Skip location 050-WILLOW.                            *
      *  02/14/2023  MCA  Skip locations xxx-xxx- -STAGE.                      *
      *  02/19/2023  MCA  Whole-order ship-complete.  Written for Get Loaded   *
      *                   orders.                                              *
      *  01/06/2025  MCA  Pallet pick requests.                                *
      *  01/28/2025  MCA  Skip location TRUCK.                                 *
      *  11/18/2025  MCA  In RtvAvailQty, needed to skip more locations.       *
      *                   Changed from hard-coded to table (GUPTDAT.LOCUNAVAL).*
      **************************************************************************

     Hnomain

     FSMPKDT    UF   E           K DISK    USROPN
     FSMPKDT01  UF   E           K DISK    USROPN RENAME(SMPKDTR:SMPKDT01R)
     FSMPKDT02  UF   E           K DISK    USROPN RENAME(SMPKDTR:SMPKDT02R)
     FSMPKGD    IF   E           K DISK    USROPN
     FSMPTQ     UF A E           K DISK    USROPN
     FSMITPK    UF A E           K DISK    USROPN
     FSMCOSU04  UF   E           K DISK    USROPN
     FSMCUSU31  UF   E           K DISK    USROPN
     FSMCTRL    IF   E             DISK    USROPN
     FHDIWHS    IF   E           K DISK    USROPN
     FHDITSL05  IF   E           K DISK    USROPN
     FHDIMST    IF   E           K DISK    USROPN
     FGUPTDAT   IF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY QPRPSRC,HYR0185

     D UpdPickQty      PR            10I 0
     D  Order#                        8P 0 CONST
     D  Line#                         3P 0 CONST
     D  Blanket#                      3P 0 CONST
     D  PickQty                      13P 4 CONST

     D UpdPndQty       PR            10I 0
     D  Item#                        15A   CONST
     D  Whs#                          3P 0 CONST
     D  PendQty                      13P 4 CONST

     D RtvSecFlds      PR            10I 0
     D  CustGrp                      20A   CONST
     D  FullCases                     1A   CONST

     D RtvAvailQty     PR            13P 4
     D  Item#                        15A   CONST
     D  Whs#                          3P 0 CONST
     D  InvtryLoc                     4A   CONST

     D RtvSMSQty       PR            13P 4
     D  AvlQtyFld                     1P 0 CONST

     D ItemOEM         PR             1N
     D  Item#                        15A   CONST
     D  Whs#                          3P 0 CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_QAV1         S                   LIKE(PDQAV1)
     D wk_QAV2         S                   LIKE(PDQAV2)
     D wk_QAV3         S                   LIKE(PDQAV3)
     D wk_QAV4         S                   LIKE(PDQAV4)
     D wk_QAV5         S                   LIKE(PDQAV5)
     D wk_QAV6         S                   LIKE(PDQAV6)
     D wk_QAV7         S                   LIKE(PDQAV7)
     D wk_QAV8         S                   LIKE(PDQAV8)
     D wk_HOLD         S                   LIKE(PDHOLD)
     D kf_CustGrp      S                   LIKE(GDCGRP)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D PrintPending    C                   'P'
     D PckTktPrtd      C                   'X'

     P/EJECT
      **************************************************************************
      * Set an order line to print a pick ticket.                              *
      *   Return values: 0-No error, 1-SMPKDT record not found                 *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  4-Insufficient quantity available,                    *
      *                  5-SMITPK update error,                                *
      *                  6-Pick ticket already printed.                        *
      *                  7-Order line is held.                                 *
      *                  8-SMPTQ update error.                                 *
      *                  9-Ship-complete whole order is not available.         *
      **************************************************************************
     P SetLinePrt      B                   EXPORT
     D                 PI            10I 0
     D Order#                         8P 0 CONST
     D Line#                          3P 0 CONST
     D Blanket#                       3P 0 CONST
     D CustGrp                       20A   CONST
     D FullCases                      1A   CONST
     D AvlQtyFld                      1P 0 CONST

     D wk_Order#       S                   LIKE(PDORD#)
     D wk_Line#        S                   LIKE(PDORL#)
     D wk_Blanket#     S                   LIKE(PDBLN#)
     D wk_CustGrp      S                   LIKE(GDCGRP)
     D wk_FullCases    S              1A
     D wk_AvlQtyFld    S              1P 0
     D wk_Error        S             10I 0
     D wk_SMSQty       S             13P 4
     D wk_ItemOEM      S              1N
     D wk_InvtryLoc    S              4A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_CustGrp = CustGrp
     C                   EVAL      wk_FullCases = FullCases
     C                   EVAL      wk_AvlQtyFld = AvlQtyFld
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(SMPKDT)
     C                   OPEN      SMPKDT
     C                   ENDIF                                                  not %open(SMPKDT)

     C                   IF        not %open(SMCTRL)
     C                   OPEN      SMCTRL
     C                   ENDIF                                                  not %open(SMCTRL)

     C     1             CHAIN     SMCTRL

     C     key_PD1       KLIST                                                  SMPKDT
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Line#
     C                   KFLD                    wk_Blanket#

     C     key_PD1       CHAIN(E)  SMPKDT
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        not %found(SMPKDT)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %found(SMPKDT)

     C                   IF        PDPTPP = PrintPending
     C                   UNLOCK    SMPKDT
     C                   LEAVE
     C                   ENDIF                                                  PDPTPP=PrintPending

     C                   IF        PDPTPP = PckTktPrtd
     C                   EVAL      wk_Error = 6
     C                   UNLOCK    SMPKDT
     C                   LEAVE
     C                   ENDIF                                                  PDPTPP=PckTktPrtd

     C                   CALLP(E)  RtvSecFlds(wk_CustGrp:wk_FullCases)

     C                   IF        wk_HOLD <> *Blank  and  wk_HOLD <> 'X'
     C                   IF        wk_HOLD = '/'
     C                   EVAL      wk_Error = 9
     C                   ELSE
     C                   EVAL      wk_Error = 7
     C                   ENDIF                                                  wk_HOLD='/'
     C                   UNLOCK    SMPKDT
     C                   LEAVE
     C                   ENDIF                                                  wk_HOLD<>*Blank and

     C                   IF        PDPCLS <> '3NI'
     C                   IF        not QtyAvail(PDITEM:PDWHS)
     C                   UNLOCK    SMPKDT
     C                   EVAL      wk_Error = 4
     C                   LEAVE
     C                   ENDIF                                                  not QtyAvail(PDITEM:
     C                   ENDIF                                                  PDPCLS<>'3NI'

     C                   EVAL      PDPTPP = PrintPending
     C                   UPDATE(E) SMPKDTR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error

     C                   EVAL      wk_ItemOEM = ItemOEM(PDITEM:PDWHS)

     C****               IF        PDLOC# = 91
     C                   IF        (PDLOC# = 91  or  SMILOC='WEST')
     C                               and  not wk_ItemOEM
     C                               and  PDPCLS <> '3NI'
     C****               IF        wk_AvlQtyFld = -1
     C                   IF        wk_AvlQtyFld < *Zero
     C                   SELECT
     C                   WHEN      wk_AvlQtyFld = -2
     C                   EVAL      wk_InvtryLoc = 'WEST'
     C                   WHEN      wk_AvlQtyFld = -3
     C                   EVAL      wk_InvtryLoc = 'ALDA'
     C                   WHEN      wk_AvlQtyFld = -4
     C                   EVAL      wk_InvtryLoc = 'GI'
     C                   OTHER
     C                   EVAL      wk_InvtryLoc = *Blanks
     C                   ENDSL
     C                   EVAL      wk_SMSQty = RtvAvailQty(PDITEM:PDWHS:
     C                                                     wk_InvtryLoc)
     C                   IF        wk_SMSQty <= *Zero
     C                   UNLOCK    SMPKDT
     C                   EVAL      wk_Error = 4
     C                   LEAVE
     C                   ENDIF                                                  wk_SMSQty<=*Zero
     C                   IF        wk_SMSQty > PDQOPN
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  wk_SMSQty>PDQOPN
     C                   ELSE
     C                   EVAL      wk_SMSQty = RtvSMSQty(wk_AvlQtyFld)
     C****               ENDIF                                                  wk_AvlQtyFld=-1
     C                   ENDIF                                                  wk_AvlQtyFld<*Zero
     C                   ELSE
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  PDLOC#=91

     C                   IF        not wk_ItemOEM
     C                   IF        UpdPickQty(wk_Order#:wk_Line#:wk_Blanket#:
     C                               wk_SMSQty) > *Zero
     C                   EVAL      wk_Error = 8
     C                   LEAVE
     C                   ENDIF                                                  UpdPickQty(wk_Order#
     C                   ENDIF                                                  not wk_ItemOEM

     C                   IF        UpdPndQty(PDITEM:PDWHS:wk_SMSQty) > *Zero
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  UpdPndQty(PDITEM:PDW

     C                   ENDDO

     C                   RETURN    wk_Error

     P SetLinePrt      E

     P/EJECT
      **************************************************************************
      * Set an order to print a pick ticket.                                   *
      *   Return values: 0-No error, 1-SMPKDT record not found,                *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  5-SMITPK update error                                 *
      *                  8-SMPTQ update error.                                 *
      *                  9-Ship-complete whole order is not available.         *
      **************************************************************************
     P SetOrdPrt       B                   EXPORT
     D                 PI            10I 0
     D Order#                         8P 0 CONST
     D ReqDate                        7P 0 CONST                                cyymmdd
     D CustGrp                       20A   CONST
     D Item#                         15A   CONST
     D FullCases                      1A   CONST
     D AvlQtyFld                      1P 0 CONST

     D wk_Order#       S                   LIKE(PDORD#)
     D wk_ReqDate      S                   LIKE(PDRQDT)
     D wk_CustGrp      S                   LIKE(GDCGRP)
     D wk_Item#        S                   LIKE(PDITEM)
     D wk_FullCases    S              1A
     D wk_AvlQtyFld    S              1P 0
     D wk_Error        S             10I 0
     D wk_RcdUpd       S              1N
     D wk_SMSQty       S             13P 4
     D wk_ItemOEM      S              1N
     D wk_InvtryLoc    S              4A
     D wk_ShipCmplt1   S              1N
     D wk_ShipCmplt2   S              1N

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_ReqDate = ReqDate
     C                   EVAL      wk_CustGrp = CustGrp
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_FullCases = FullCases
     C                   EVAL      wk_AvlQtyFld = AvlQtyFld
     C                   EVAL      wk_Error = *Zero

     C                   EVAL      wk_ShipCmplt1 = *OFF
     C                   EVAL      wk_ShipCmplt2 = *OFF
     C                   EVAL      wk_RcdUpd = *OFF

     C                   IF        not %open(SMPKDT)
     C                   OPEN      SMPKDT
     C                   ENDIF                                                  not %open(SMPKDT)

     C                   IF        not %open(SMCTRL)
     C                   OPEN      SMCTRL
     C                   ENDIF                                                  not %open(SMCTRL)

     C     1             CHAIN     SMCTRL

     C     wk_Order#     SETLL     SMPKDT
     C                   IF        not %equal(SMPKDT)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %equal(SMPKDT)

     C                   DOU       %eof(SMPKDT)
     C     wk_Order#     READE(E)  SMPKDT
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        %eof(SMPKDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(SMPKDT)
     C                   CALLP(E)  RtvSecFlds(wk_CustGrp:wk_FullCases)
     C                   SELECT
     C                   WHEN      wk_HOLD = *Blank
     C                   EVAL      wk_ShipCmplt1 = *ON
     C                   WHEN      wk_HOLD = '/'
     C                   EVAL      wk_ShipCmplt2 = *ON
     C                   ENDSL
     C                   IF        (PDRQDT > wk_ReqDate  and
     C                               PDMALC <> 'Y')  or
     C                               (wk_Item# <> *Blanks  and
     C                                PDITEM <> wk_Item#)  or
     C                               PDPTPP = PrintPending  or
     C                               PDPTPP = PckTktPrtd  or
     C                               (wk_HOLD <> *Blank  and  wk_HOLD <> 'X')
     C                   UNLOCK    SMPKDT
     C                   ITER
     C                   ENDIF                                                  PDRQDT>wk_ReqDate or
     C                   IF        PDPCLS <> '3NI'
     C                   IF        (wk_AvlQtyFld = 1  and  wk_QAV1 <= *Zero)  or
     C                              (wk_AvlQtyFld = 2  and  wk_QAV2 <= *Zero) or
     C                              (wk_AvlQtyFld = 3  and  wk_QAV3 <= *Zero) or
     C                              (wk_AvlQtyFld = 4  and  wk_QAV4 <= *Zero) or
     C                              (wk_AvlQtyFld = 5  and  wk_QAV5 <= *Zero) or
     C                              (wk_AvlQtyFld = 6  and  wk_QAV6 <= *Zero) or
     C                              (wk_AvlQtyFld = 7  and  wk_QAV7 <= *Zero) or
     C                              (wk_AvlQtyFld = 8  and  wk_QAV8 <= *Zero)
     C                   UNLOCK    SMPKDT
     C                   ITER
     C                   ENDIF                                                  not QtyAvail(PDITEM:
     C                   IF        not QtyAvail(PDITEM:PDWHS)
     C                   UNLOCK    SMPKDT
     C                   ITER
     C                   ENDIF                                                  not QtyAvail(PDITEM:
     C                   ENDIF                                                  PDPCLS<>'3NI'
     C                   EVAL      PDPTPP = PrintPending
     C                   UPDATE(E) SMPKDTR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   EVAL      wk_ItemOEM = ItemOEM(PDITEM:PDWHS)
     C****               IF        PDLOC# = 91
     C                   IF        (PDLOC# = 91  or  SMILOC='WEST')
     C                               and  not wk_ItemOEM
     C                               and  PDPCLS <> '3NI'
     C****               IF        wk_AvlQtyFld = -1
     C                   IF        wk_AvlQtyFld < *Zero
     C                   SELECT
     C                   WHEN      wk_AvlQtyFld = -2
     C                   EVAL      wk_InvtryLoc = 'WEST'
     C                   WHEN      wk_AvlQtyFld = -3
     C                   EVAL      wk_InvtryLoc = 'ALDA'
     C                   WHEN      wk_AvlQtyFld = -4
     C                   EVAL      wk_InvtryLoc = 'GI'
     C                   OTHER
     C                   EVAL      wk_InvtryLoc = *Blanks
     C                   ENDSL
     C                   EVAL      wk_SMSQty = RtvAvailQty(PDITEM:PDWHS:
     C                                                     wk_InvtryLoc)
     C                   IF        wk_SMSQty <= *Zero
     C                   UNLOCK    SMPKDT
     C                   ITER
     C                   ENDIF                                                  wk_SMSQty<=*Zero
     C                   IF        wk_SMSQty > PDQOPN
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  wk_SMSQty>PDQOPN
     C                   ELSE
     C                   EVAL      wk_SMSQty = RtvSMSQty(wk_AvlQtyFld)
     C****               ENDIF                                                  wk_AvlQtyFld=-1
     C                   ENDIF                                                  wk_AvlQtyFld<*Zero
     C                   ELSE
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  PDLOC#=91
     C                   IF        not wk_ItemOEM
     C                   IF        UpdPickQty(wk_Order#:PDORL#:PDBLN#:
     C                               wk_SMSQty) > *Zero
     C                   EVAL      wk_Error = 8
     C                   LEAVE
     C                   ENDIF                                                  UpdPickQty(wk_Order#
     C                   ENDIF                                                  wk_ItemOEM
     C                   IF        UpdPndQty(PDITEM:PDWHS:wk_SMSQty) > *Zero
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  UpdPndQty(PDITEM:PDW
     C                   EVAL      wk_RcdUpd = *ON
     C                   ENDDO                                                  %eof(SMPKDT)

     C                   IF        not wk_ShipCmplt1  and  wk_ShipCmplt2
     C                   EVAL      wk_Error = 9
     C                   ENDIF                                                  not wk_ShipCmplt1

     C                   IF        wk_Error > *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Error>*Zero

     C                   IF        not wk_RcdUpd
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not wk_RcdUpd

     C                   ENDDO

     C                   RETURN    wk_Error

     P SetOrdPrt       E

     P/EJECT
      **************************************************************************
      * Set a customer's orders to print a pick ticket.                        *
      *   Return values: 0-No error, 1-SMPKDT record not found,                *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  5-SMITPK update error                                 *
      *                  8-SMPTQ update error.                                 *
      *                  9-Ship-complete whole order is not available.         *
      **************************************************************************
     P SetCustPrt      B                   EXPORT
     D                 PI            10I 0
     D ShipTo#                        7P 0 CONST
     D ReqDate                        7P 0 CONST                                cyymmdd
     D CustGrp                       20A   CONST
     D Item#                         15A   CONST
     D FullCases                      1A   CONST
     D AvlQtyFld                      1P 0 CONST

     D wk_ShipTo#      S                   LIKE(PDSHTO)
     D wk_ReqDate      S                   LIKE(PDRQDT)
     D wk_CustGrp      S                   LIKE(GDCGRP)
     D wk_Item#        S                   LIKE(PDITEM)
     D wk_FullCases    S              1A
     D wk_AvlQtyFld    S              1P 0
     D wk_Error        S             10I 0
     D wk_RcdUpd       S              1N
     D wk_SMSQty       S             13P 4
     D wk_ItemOEM      S              1N
     D wk_ShipCmplt1   S              1N
     D wk_ShipCmplt2   S              1N

     C                   DO

     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_ReqDate = ReqDate
     C                   EVAL      wk_CustGrp = CustGrp
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_FullCases = FullCases
     C                   EVAL      wk_AvlQtyFld = AvlQtyFld
     C                   EVAL      wk_Error = *Zero

     C                   EVAL      wk_ShipCmplt1 = *OFF
     C                   EVAL      wk_ShipCmplt2 = *OFF
     C                   EVAL      wk_RcdUpd = *OFF

     C                   IF        not %open(SMPKDT02)
     C                   OPEN      SMPKDT02
     C                   ENDIF                                                  not %open(SMPKDT02)

     C                   IF        not %open(SMCTRL)
     C                   OPEN      SMCTRL
     C                   ENDIF                                                  not %open(SMCTRL)

     C     1             CHAIN     SMCTRL

     C     key_PD2       KLIST                                                  SMPKDT02
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_Item#

     C                   IF        wk_Item# = *Blanks
     C     wk_ShipTo#    SETLL     SMPKDT02
     C                   ELSE
     C     key_PD2       SETLL     SMPKDT02
     C                   ENDIF                                                  wk_Item#=*Blanks
     C                   IF        not %equal(SMPKDT02)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %equal(SMPKDT02)

     C                   DOU       %eof(SMPKDT02)
     C                   IF        wk_Item# = *Blanks
     C     wk_ShipTo#    READE(E)  SMPKDT02
     C                   ELSE
     C     key_PD2       READE(E)  SMPKDT02
     C                   ENDIF                                                  wk_Item#=*Blanks
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        %eof(SMPKDT02)
     C                   LEAVE
     C                   ENDIF                                                  %eof(SMPKDT02)
     C                   CALLP(E)  RtvSecFlds(wk_CustGrp:wk_FullCases)
     C                   SELECT
     C                   WHEN      wk_HOLD = *Blank
     C                   EVAL      wk_ShipCmplt1 = *ON
     C                   WHEN      wk_HOLD = '/'
     C                   EVAL      wk_ShipCmplt2 = *ON
     C                   ENDSL
     C                   IF        (PDRQDT > wk_ReqDate  and
     C                               PDMALC <> 'Y')  or
     C                               (wk_Item# <> *Blanks  and
     C                                PDITEM <> wk_Item#)  or
     C                               PDPTPP = PrintPending  or
     C                               PDPTPP = PckTktPrtd  or
     C                               (wk_HOLD <> *Blank  and  wk_HOLD <> 'X')
     C                   UNLOCK    SMPKDT02
     C                   ITER
     C                   ENDIF                                                  PDRQDT>wk_ReqDate or
     C                   IF        PDPCLS <> '3NI'
     C                   IF        (wk_AvlQtyFld = 1  and  wk_QAV1 <= *Zero)  or
     C                              (wk_AvlQtyFld = 2  and  wk_QAV2 <= *Zero) or
     C                              (wk_AvlQtyFld = 3  and  wk_QAV3 <= *Zero) or
     C                              (wk_AvlQtyFld = 4  and  wk_QAV4 <= *Zero) or
     C                              (wk_AvlQtyFld = 5  and  wk_QAV5 <= *Zero) or
     C                              (wk_AvlQtyFld = 6  and  wk_QAV6 <= *Zero) or
     C                              (wk_AvlQtyFld = 7  and  wk_QAV7 <= *Zero) or
     C                              (wk_AvlQtyFld = 8  and  wk_QAV8 <= *Zero)
     C                   UNLOCK    SMPKDT02
     C                   ITER
     C                   ENDIF                                                  not QtyAvail(PDITEM:
     C                   IF        not QtyAvail(PDITEM:PDWHS)
     C                   UNLOCK    SMPKDT02
     C                   ITER
     C                   ENDIF                                                  not QtyAvail(PDITEM:
     C                   ENDIF                                                  PDPCLS<>'3NI'
     C                   EVAL      PDPTPP = PrintPending
     C                   UPDATE(E) SMPKDT02R
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   EVAL      wk_ItemOEM = ItemOEM(PDITEM:PDWHS)
     C****               IF        PDLOC# = 91
     C                   IF        (PDLOC# = 91  or  SMILOC='WEST')
     C                               and  not wk_ItemOEM
     C                               and  PDPCLS <> '3NI'
     C                   EVAL      wk_SMSQty = RtvSMSQty(wk_AvlQtyFld)
     C                   ELSE
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  PDLOC#=91
     C                   IF        not wk_ItemOEM
     C                   IF        UpdPickQty(PDORD#:PDORL#:PDBLN#:
     C                               wk_SMSQty) > *Zero
     C                   EVAL      wk_Error = 8
     C                   LEAVE
     C                   ENDIF                                                  UpdPickQty(wk_Order#
     C                   ENDIF                                                  wk_ItemOEM
     C                   IF        UpdPndQty(PDITEM:PDWHS:wk_SMSQty) > *Zero
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  UpdPndQty(PDITEM:PDW
     C                   EVAL      wk_RcdUpd = *ON
     C                   ENDDO                                                  %eof(SMPKDT02)

     C                   IF        not wk_ShipCmplt1  and  wk_ShipCmplt2
     C                   EVAL      wk_Error = 9
     C                   ENDIF                                                  not wk_ShipCmplt1

     C                   IF        wk_Error > *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Error>*Zero

     C                   IF        not wk_RcdUpd
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not wk_RcdUpd

     C                   ENDDO

     C                   RETURN    wk_Error

     P SetCustPrt      E

     P/EJECT
      **************************************************************************
      * Reset an order line to not print a pick ticket.                        *
      *   Return values: 0-No error, 1-SMPKDT record not found,                *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  5-SMITPK update error,                                *
      *                  6-Pick ticket already printed.                        *
      **************************************************************************
     P ResetLinePrt    B                   EXPORT
     D                 PI            10I 0
     D Order#                         8P 0 CONST
     D Line#                          3P 0 CONST
     D Blanket#                       3P 0 CONST

     D wk_Order#       S                   LIKE(PDORD#)
     D wk_Line#        S                   LIKE(PDORL#)
     D wk_Blanket#     S                   LIKE(PDBLN#)
     D wk_Error        S             10I 0
     D wk_SMSQty       S             13P 4

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(SMPKDT)
     C                   OPEN      SMPKDT
     C                   ENDIF                                                  not %open(SMPKDT)

     C                   IF        not %open(SMPTQ)
     C                   OPEN      SMPTQ
     C                   ENDIF                                                  not %open(SMPTQ)

     C     key_PD1       KLIST                                                  SMPKDT
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Line#
     C                   KFLD                    wk_Blanket#

     C     key_PD1       CHAIN(E)  SMPKDT
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        not %found(SMPKDT)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %found(SMPKDT)

     C                   IF        PDPTPP = PckTktPrtd
     C                   EVAL      wk_Error = 6
     C                   UNLOCK    SMPKDT
     C                   LEAVE
     C                   ENDIF                                                  PDPTPP=PckTktPrtd

     C                   EVAL      PDPTPP = *Blank
     C                   UPDATE(E) SMPKDTR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error

     C     key_PQ        KLIST                                                  SMPTQ
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Line#
     C                   KFLD                    wk_Blanket#

     C     key_PQ        CHAIN     SMPTQ
     C                   IF        %found(SMPTQ)
     C                   EVAL      wk_SMSQty = PQQPIK
     C                   DELETE    SMPTQR
     C                   ELSE
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  %found(SMPTQ)

     C                   IF        UpdPndQty(PDITEM:PDWHS:wk_SMSQty * -1) >
     C                               *Zero
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  UpdPndQty(PDITEM:PDW

     C                   ENDDO

     C                   RETURN    wk_Error

     P ResetLinePrt    E

     P/EJECT
      **************************************************************************
      * Reset an order to not print a pick ticket.                             *
      *   Return values: 0-No error, 1-SMPKDT record not found,                *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  5-SMITPK update error                                 *
      **************************************************************************
     P ResetOrdPrt     B                   EXPORT
     D                 PI            10I 0
     D Order#                         8P 0 CONST
     D DltPTQ                         1N   CONST

     D wk_Order#       S                   LIKE(PDORD#)
     D wk_DltPTQ       S              1N
     D wk_Error        S             10I 0
     D wk_RcdUpd       S              1N
     D wk_SMSQty       S             13P 4

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_DltPTQ = DltPTQ
     C                   EVAL      wk_Error = *Zero

     C                   EVAL      wk_RcdUpd = *OFF

     C                   IF        not %open(SMPKDT)
     C                   OPEN      SMPKDT
     C                   ENDIF                                                  not %open(SMPKDT)

     C                   IF        not %open(SMPTQ)
     C                   OPEN      SMPTQ
     C                   ENDIF                                                  not %open(SMPTQ)

     C     wk_Order#     SETLL     SMPKDT
     C                   IF        not %equal(SMPKDT)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %equal(SMPKDT)

     C                   DOU       %eof(SMPKDT)
     C     wk_Order#     READE(E)  SMPKDT
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        %eof(SMPKDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(SMPKDT)
     C                   IF        PDPTPP = PckTktPrtd
     C                   UNLOCK    SMPKDT
     C                   ITER
     C                   ENDIF                                                  PDPTPP=PckTktPrtd
     C                   EVAL      PDPTPP = *Blanks
     C                   UPDATE(E) SMPKDTR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C     key_PQ        KLIST                                                  SMPTQ
     C                   KFLD                    wk_Order#
     C                   KFLD                    PDORL#
     C                   KFLD                    PDBLN#
     C     key_PQ        CHAIN     SMPTQ
     C                   IF        %found(SMPTQ)
     C                   EVAL      wk_SMSQty = PQQPIK
     C                   IF        wk_DltPTQ
     C                   DELETE    SMPTQR
     C                   ELSE
     C                   UNLOCK    SMPTQ
     C                   ENDIF                                                  wk_DltPTQ
     C                   ELSE
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  %found(SMPTQ)
     C                   IF        UpdPndQty(PDITEM:PDWHS:wk_SMSQty * -1) >
     C                               *Zero
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  UpdPndQty(PDITEM:PDW
     C                   EVAL      wk_RcdUpd = *ON
     C                   ENDDO                                                  %eof(SMPKDT)

     C                   IF        wk_Error > *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Error>*Zero

     C                   IF        not wk_RcdUpd
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not wk_RcdUpd

     C                   ENDDO

     C                   RETURN    wk_Error

     P ResetOrdPrt     E

     P/EJECT
      **************************************************************************
      * Reset all orders to not print a pick ticket.                           *
      *   Return values: 0-No error, 1-SMPKDT record not found,                *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  5-SMITPK update error                                 *
      **************************************************************************
     P ResetPrt        B                   EXPORT
     D                 PI            10I 0

     D wk_Error        S             10I 0
     D wk_RcdUpd       S              1N
     D wk_SMSQty       S             13P 4

     C                   DO

     C                   EVAL      wk_Error = *Zero

     C                   EVAL      wk_RcdUpd = *OFF

     C                   IF        not %open(SMPKDT01)
     C                   OPEN      SMPKDT01
     C                   ENDIF                                                  not %open(SMPKDT01)

     C                   IF        not %open(SMPTQ)
     C                   OPEN      SMPTQ
     C                   ENDIF                                                  not %open(SMPTQ)

     C     *Loval        SETLL     SMPKDT01
     C                   IF        not %found(SMPKDT01)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %found(SMPKDT01)

     C                   DOU       %eof(SMPKDT01)
     C                   READ(E)   SMPKDT01
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        %eof(SMPKDT01)
     C                   LEAVE
     C                   ENDIF                                                  %eof(SMPKDT01)
     C                   IF        PDPTPP = PckTktPrtd
     C                   UNLOCK    SMPKDT01
     C                   ITER
     C                   ENDIF                                                  PDPTPP=PckTktPrtd
     C                   EVAL      PDPTPP = *Blanks
     C                   UPDATE(E) SMPKDT01R
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C     key_PQ        KLIST                                                  SMPTQ
     C                   KFLD                    PDORD#
     C                   KFLD                    PDORL#
     C                   KFLD                    PDBLN#
     C     key_PQ        CHAIN     SMPTQ
     C                   IF        %found(SMPTQ)
     C                   EVAL      wk_SMSQty = PQQPIK
     C                   DELETE    SMPTQR
     C                   ELSE
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  %found(SMPTQ)
     C                   IF        UpdPndQty(PDITEM:PDWHS:wk_SMSQty * -1) >
     C                               *Zero
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  UpdPndQty(PDITEM:PDW
     C                   EVAL      wk_RcdUpd = *ON
     C                   ENDDO                                                  %eof(SMPKDT01)

     C                   IF        wk_Error > *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Error>*Zero

     C                   IF        not wk_RcdUpd
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not wk_RcdUpd

     C                   ENDDO

     C                   RETURN    wk_Error

     P ResetPrt        E

     P/EJECT
      **************************************************************************
      * Delete a picking detail line.                                          *
      *   Return values: 0-No error, 1-SMPKDT record not found,                *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  5-SMITPK update error                                 *
      **************************************************************************
     P DltPickDtl      B                   EXPORT
     D                 PI            10I 0
     D Order#                         8P 0 CONST
     D Line#                          3P 0 CONST
     D Blanket#                       3P 0 CONST

     D wk_Order#       S                   LIKE(PDORD#)
     D wk_Line#        S                   LIKE(PDORL#)
     D wk_Blanket#     S                   LIKE(PDBLN#)
     D wk_Error        S             10I 0
     D wk_SMSQty       S             13P 4

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(SMPKDT)
     C                   OPEN      SMPKDT
     C                   ENDIF                                                  not %open(SMPKDT)

     C                   IF        not %open(SMPTQ)
     C                   OPEN      SMPTQ
     C                   ENDIF                                                  not %open(SMPTQ)

     C     key_PD1       KLIST                                                  SMPKDT
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Line#
     C                   KFLD                    wk_Blanket#

     C     key_PD1       CHAIN(E)  SMPKDT
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        not %found(SMPKDT)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %found(SMPKDT)

     C                   EVAL      PDPTPP = PckTktPrtd
     C                   UPDATE(E) SMPKDTR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error

     C     key_PQ        KLIST                                                  SMPTQ
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Line#
     C                   KFLD                    wk_Blanket#

     C     key_PQ        CHAIN     SMPTQ
     C                   IF        %found(SMPTQ)
     C                   EVAL      wk_SMSQty = PQQPIK
     C                   DELETE    SMPTQR
     C                   ELSE
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDIF                                                  %found(SMPTQ)

     C                   IF        UpdPndQty(PDITEM:PDWHS:wk_SMSQty * -1) >
     C                               *Zero
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  UpdPndQty(PDITEM:PDW

     C                   ENDDO

     C                   RETURN    wk_Error

     P DltPickDtl      E

     P/EJECT
      **************************************************************************
      * Update SMS pick ticket print quantity file.                            *
      *   Return values: 0-No error,                                           *
      *                  2-SMPTQ read (possibly lock) error,                   *
      *                  3-SMPTQ update error                                  *
      **************************************************************************
     P UpdPickQty      B
     D                 PI            10I 0
     D Order#                         8P 0 CONST
     D Line#                          3P 0 CONST
     D Blanket#                       3P 0 CONST
     D PickQty                       13P 4 CONST


     D wk_Order#       S                   LIKE(PQORD#)
     D wk_Line#        S                   LIKE(PQORL#)
     D wk_Blanket#     S                   LIKE(PQBLN#)
     D wk_PickQty      S                   LIKE(PQQPIK)
     D wk_Error        S             10I 0

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_PickQty = PickQty
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(SMPTQ)
     C                   OPEN      SMPTQ
     C                   ENDIF                                                  not %open(SMPTQ)

     C     key_PQ        KLIST                                                  SMPTQ
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Line#
     C                   KFLD                    wk_Blanket#

     C     key_PQ        CHAIN(E)  SMPTQ
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error

     C                   IF        not %found(SMPTQ)
     C                   EVAL      PQQPIK = *Zero
     C                   ENDIF                                                  not %found(SMPTQ)

     C                   EVAL      PQQPIK = wk_PickQty

     C                   SELECT
     C                   WHEN      %found(SMPTQ)  and  PQQPIK > *Zero
     C                   UPDATE(E) SMPTQR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   WHEN      %found(SMPTQ)  and  PQQPIK <= *Zero
     C                   DELETE(E) SMPTQR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   WHEN      not %found(SMPTQ)  and  PQQPIK > *Zero
     C                   EVAL      PQORD# = wk_Order#
     C                   EVAL      PQORL# = wk_Line#
     C                   EVAL      PQBLN# = wk_Blanket#
     C                   EVAL      PQPLTP = *Blank
     C                   WRITE(E)  SMPTQR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   ENDSL

     C                   ENDDO

     C                   RETURN    wk_Error

     P UpdPickQty      E

     P/EJECT
      **************************************************************************
      * Update item pick pending quantity file.                                *
      *   Return values: 0-No error,                                           *
      *                  2-SMITPK read (possibly lock) error,                  *
      *                  3-SMITPK update error                                 *
      **************************************************************************
     P UpdPndQty       B
     D                 PI            10I 0
     D Item#                         15A   CONST
     D Whs#                           3P 0 CONST
     D PendQty                       13P 4 CONST

     D wk_Item#        S                   LIKE(IPITEM)
     D wk_Whs#         S                   LIKE(IPWHS)
     D wk_PendQty      S                   LIKE(IPPPQT)
     D wk_Error        S             10I 0

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Whs# = Whs#
     C                   EVAL      wk_PendQty = PendQty
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(SMITPK)
     C                   OPEN      SMITPK
     C                   ENDIF                                                  not %open(SMITPK)

     C     key_IP        KLIST                                                  SMITPK
     C                   KFLD                    wk_Item#
     C                   KFLD                    wk_Whs#

     C     key_IP        CHAIN(E)  SMITPK
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error

     C                   IF        not %found(SMITPK)
     C                   EVAL      IPPPQT = *Zero
     C                   ENDIF                                                  not %found(SMITPK)

     C                   EVAL      IPPPQT = IPPPQT + wk_PendQty

     C                   SELECT
     C                   WHEN      %found(SMITPK)  and  IPPPQT > *Zero
     C                   UPDATE(E) SMITPKR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   WHEN      %found(SMITPK)  and  IPPPQT <= *Zero
     C                   DELETE(E) SMITPKR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   WHEN      not %found(SMITPK)  and  IPPPQT > *Zero
     C                   EVAL      IPITEM = wk_Item#
     C                   EVAL      IPWHS = wk_Whs#
     C                   WRITE(E)  SMITPKR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   ENDSL

     C                   ENDDO

     C                   RETURN    wk_Error

     P UpdPndQty       E

     P/EJECT
      **************************************************************************
      * Check whether item has quantity available.                             *
      *   Return values: 0-No quantity available, 1-Quantity is available      *
      **************************************************************************
     P QtyAvail        B                   EXPORT
     D                 PI             1N
     D Item#                         15A   CONST
     D Whs#                           3P 0 CONST

     D wk_Item#        S                   LIKE(PDITEM)
     D wk_Whs#         S                   LIKE(PDWHS)
     D wk_QtyAvail     S              1N

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Whs# = Whs#
     C                   EVAL      wk_QtyAvail = *OFF

     C                   IF        not %open(HDIWHS)
     C                   OPEN      HDIWHS
     C                   ENDIF                                                  not %open(HDIWHS)
     C                   IF        not %open(SMITPK)
     C                   OPEN      SMITPK
     C                   ENDIF                                                  not %open(SMITPK)

     C     key_IW        KLIST                                                  HDIWHS
     C                   KFLD                    wk_Item#
     C                   KFLD                    wk_Whs#

     C     key_IP        KLIST                                                  SMITPK
     C                   KFLD                    wk_Item#
     C                   KFLD                    wk_Whs#

     C     key_IW        CHAIN     HDIWHS
     C                   IF        not %found(HDIWHS)
     C                   LEAVE
     C                   ENDIF                                                  not %found(HDIWHS)

     C     key_IP        CHAIN(N)  SMITPK
     C                   IF        not %found(SMITPK)
     C                   EVAL      IPPPQT = *Zero
     C                   ENDIF                                                  not %found(SMITPK)

     C                   IF        IWOHQT - IWQHSR - IWQOPK - IPPPQT > *Zero
     C                   EVAL      wk_QtyAvail = *ON
     C                   ENDIF                                                  IWOHQT-IWQHSR-IWQOPK

     C                   ENDDO

     C                   RETURN    wk_QtyAvail

     P QtyAvail        E

     P/EJECT
      **************************************************************************
      * Set an order (in SMS files only) to hold status.                       *
      *   Return values: 0-No error, 1-SMPKDT record not found,                *
      *                  2-SMPKDT read (possibly lock) error,                  *
      *                  3-SMPKDT update error,                                *
      *                  4-SMCOSU record not found,                            *
      *                  5-SMCOSU read (possibly lock) error,                  *
      *                  6-SMCOSU update error,                                *
      *                  7-SMCUSU record not found,                            *
      *                  8-SMCUSU read (possibly lock) error,                  *
      *                  9-SMCUSU update error                                 *
      **************************************************************************
     P SetOrdHold      B                   EXPORT
     D                 PI            10I 0
     D Order#                         8P 0 CONST
     D HoldCode                       1A   CONST
     D OvrCurHold                     1N   CONST

     D wk_Order#       S                   LIKE(PDORD#)
     D wk_HoldCode     S                   LIKE(PDHOLD)
     D wk_OvrCurHold   S              1N
     D wk_Error        S             10I 0
     D wk_ShpTo#       S                   LIKE(PDSHTO)
     D wk_RcdUpd1      S              1N
     D wk_RcdUpd2      S              1N
     D wk_RcdUpd3      S              1N

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_HoldCode = HoldCode
     C                   EVAL      wk_OvrCurHold = OvrCurHold
     C                   EVAL      wk_Error = *Zero

     C                   EVAL      wk_ShpTo# = *Zero
     C                   EVAL      wk_RcdUpd1 = *OFF
     C                   EVAL      wk_RcdUpd2 = *OFF
     C                   EVAL      wk_RcdUpd3 = *OFF

     C                   IF        not %open(SMPKDT)
     C                   OPEN      SMPKDT
     C                   ENDIF                                                  not %open(SMPKDT)

     C     wk_Order#     SETLL     SMPKDT
     C                   IF        not %equal(SMPKDT)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %equal(SMPKDT)

     C                   DOU       %eof(SMPKDT)
     C     wk_Order#     READE(E)  SMPKDT
     C                   IF        %error
     C                   EVAL      wk_Error = 2
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        %eof(SMPKDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(SMPKDT)
     C                   IF        PDPTPP <> *Blank  or
     C                               (PDHOLD <> *Blank  and  not wk_OvrCurHold)
     C                   UNLOCK    SMPKDT
     C                   ITER
     C                   ENDIF                                                  PDPTPP=*Blank or
     C                   EVAL      PDHOLD = wk_HoldCode
     C                   UPDATE(E) SMPKDTR
     C                   IF        %error
     C                   EVAL      wk_Error = 3
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   EVAL      wk_RcdUpd1 = *ON
     C                   EVAL      wk_ShpTo# = PDSHTO
     C                   ENDDO                                                  %eof(SMPKDT)

     C                   IF        wk_Error > *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Error>*Zero

     C                   IF        wk_Shpto# <> *Zero

     C                   IF        not %open(SMCOSU04)
     C                   OPEN      SMCOSU04
     C                   ENDIF                                                  not %open(SMCOSU04)

     C     key_CO        KLIST                                                  SMCOSU04
     C                   KFLD                    wk_ShpTo#
     C                   KFLD                    wk_Order#

     C     key_CO        SETLL     SMCOSU04
     C                   IF        not %equal(SMCOSU04)
     C                   EVAL      wk_Error = 4
     C                   LEAVE
     C                   ENDIF                                                  not %equal(SMCOSU04)

     C                   DOU       %eof(SMCOSU04)
     C     key_CO        READE(E)  SMCOSU04
     C                   IF        %error
     C                   EVAL      wk_Error = 5
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        %eof(SMCOSU04)
     C                   LEAVE
     C                   ENDIF                                                  %eof(SMCOSU04)
     C                   IF        COHOLD1 <> *Blank  and
     C                             COHOLD2 <> *Blank  and
     C                             COHOLD3 <> *Blank  and
     C                             COHOLD4 <> *Blank  and
     C                             COHOLD5 <> *Blank  and
     C                             COHOLD6 <> *Blank  and
     C                             COHOLD7 <> *Blank  and
     C                             COHOLD8 <> *Blank  and
     C                             not wk_OvrCurHold
     C                   UNLOCK    SMCOSU04
     C                   ITER
     C                   ENDIF                                                  COHOLD1<>*Blank and
     C                   IF        COHOLD1 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD1 = wk_HoldCode
     C                   ENDIF                                                  COHOLD1=*Blank or
     C                   IF        COHOLD2 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD2 = wk_HoldCode
     C                   ENDIF                                                  COHOLD2=*Blank or
     C                   IF        COHOLD3 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD3 = wk_HoldCode
     C                   ENDIF                                                  COHOLD3=*Blank or
     C                   IF        COHOLD4 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD4 = wk_HoldCode
     C                   ENDIF                                                  COHOLD4=*Blank or
     C                   IF        COHOLD5 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD5 = wk_HoldCode
     C                   ENDIF                                                  COHOLD5=*Blank or
     C                   IF        COHOLD6 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD6 = wk_HoldCode
     C                   ENDIF                                                  COHOLD6=*Blank or
     C                   IF        COHOLD7 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD7 = wk_HoldCode
     C                   ENDIF                                                  COHOLD7=*Blank or
     C                   IF        COHOLD8 = *Blank  or  wk_OvrCurHold
     C                   EVAL      COHOLD8 = wk_HoldCode
     C                   ENDIF                                                  COHOLD8=*Blank or
     C                   UPDATE(E) SMCOSUR
     C                   IF        %error
     C                   EVAL      wk_Error = 6
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   EVAL      wk_RcdUpd2 = *ON
     C                   ENDDO                                                  %eof(SMCOSU04)

     C                   IF        wk_Error > *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Error>*Zero

     C                   IF        not %open(SMCUSU31)
     C                   OPEN      SMCUSU31
     C                   ENDIF                                                  not %open(SMCUSU31)

     C     wk_ShpTo#     SETLL     SMCUSU31
     C                   IF        not %equal(SMCUSU31)
     C                   EVAL      wk_Error = 7
     C                   LEAVE
     C                   ENDIF                                                  not %equal(SMCUSU31)

     C                   DOU       %eof(SMCUSU31)
     C     wk_ShpTo#     READE(E)  SMCUSU31
     C                   IF        %error
     C                   EVAL      wk_Error = 8
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   IF        %eof(SMCUSU31)
     C                   LEAVE
     C                   ENDIF                                                  %eof(SMCUSU31)
     C                   IF        CSHOLD1 <> *Blank  and
     C                             CSHOLD2 <> *Blank  and
     C                             CSHOLD3 <> *Blank  and
     C                             CSHOLD4 <> *Blank  and
     C                             CSHOLD5 <> *Blank  and
     C                             CSHOLD6 <> *Blank  and
     C                             CSHOLD7 <> *Blank  and
     C                             CSHOLD8 <> *Blank  and
     C                             not wk_OvrCurHold
     C                   UNLOCK    SMCUSU31
     C                   ITER
     C                   ENDIF                                                  CSHOLD1<>*Blank and
     C                   IF        CSHOLD1 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD1 = 'Y'
     C                   ENDIF                                                  CSHOLD1=*Blank or
     C                   IF        CSHOLD2 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD2 = 'Y'
     C                   ENDIF                                                  CSHOLD2=*Blank or
     C                   IF        CSHOLD3 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD3 = 'Y'
     C                   ENDIF                                                  CSHOLD3=*Blank or
     C                   IF        CSHOLD4 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD4 = 'Y'
     C                   ENDIF                                                  CSHOLD4=*Blank or
     C                   IF        CSHOLD5 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD5 = 'Y'
     C                   ENDIF                                                  CSHOLD5=*Blank or
     C                   IF        CSHOLD6 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD6 = 'Y'
     C                   ENDIF                                                  CSHOLD6=*Blank or
     C                   IF        CSHOLD7 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD7 = 'Y'
     C                   ENDIF                                                  CSHOLD7=*Blank or
     C                   IF        CSHOLD8 = *Blank  or  wk_OvrCurHold
     C                   EVAL      CSHOLD8 = 'Y'
     C                   ENDIF                                                  CSHOLD8=*Blank or
     C                   UPDATE(E) SMCUSUR
     C                   IF        %error
     C                   EVAL      wk_Error = 9
     C                   LEAVE
     C                   ENDIF                                                  %error
     C                   EVAL      wk_RcdUpd3 = *ON
     C                   ENDDO                                                  %eof(SMCUSU31)

     C                   IF        wk_Error > *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Error>*Zero

     C                   ENDIF                                                  wk_ShpTo#<>*Zero

     C                   IF        not wk_RcdUpd1
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not wk_RcdUpd1
     C                   IF        not wk_RcdUpd2
     C                   EVAL      wk_Error = 4
     C                   LEAVE
     C                   ENDIF                                                  not wk_RcdUpd2
     C                   IF        not wk_RcdUpd3
     C                   EVAL      wk_Error = 7
     C                   LEAVE
     C                   ENDIF                                                  not wk_RcdUpd3

     C                   ENDDO

     C                   RETURN    wk_Error

     P SetOrdHold      E

     P/EJECT
      **************************************************************************
      * Retrieve pick ticket weight from SMS Pick Ticket Quantity record.      *
      *   Return values: Pick ticket weight.                                   *
      *                  -1 - Pick ticket quantity record not found.           *
      **************************************************************************
     P RtvPTWgt        B                   EXPORT
     D                 PI             7P 0
     D Order#                         8P 0 CONST
     D Line#                          3P 0 CONST
     D Blanket#                       3P 0 CONST
     D Item#                         15A   CONST

     D wk_Order#       S                   LIKE(PQORD#)
     D wk_Line#        S                   LIKE(PQORL#)
     D wk_Blanket#     S                   LIKE(PQBLN#)
     D wk_Item#        S                   LIKE(IMITEM)
     D wk_PTWgt        S              7P 0
     D wk_UnitWgt      S              9P 5
     D kf_TABL         S                   LIKE(TDTABL)
     D kf_KEY1         S                   LIKE(TDKEY1)

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_PTWgt = *Zero

     C                   IF        not %open(SMPTQ)
     C                   OPEN      SMPTQ
     C                   ENDIF                                                  not %open(SMPTQ)

     C                   IF        not %open(HDIMST)
     C                   OPEN      HDIMST
     C                   ENDIF                                                  not %open(HDIMST)

     C                   IF        not %open(GUPTDAT)
     C                   OPEN      GUPTDAT
     C                   ENDIF                                                  not %open(GUPTDAT)

     C     key_PQ        KLIST                                                  SMPTQ
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_Line#
     C                   KFLD                    wk_Blanket#

     C     key_TD        KLIST                                                  GUPTDAT
     C                   KFLD                    kf_TABL
     C                   KFLD                    kf_KEY1

     C     key_PQ        CHAIN(N)  SMPTQ
     C                   IF        %found(SMPTQ)
     C     wk_Item#      CHAIN     HDIMST
     C                   IF        %found(HDIMST)
     C                   EVAL      kf_TABL = 'BULKPRDCLS'
     C                   EVAL      kf_KEY1 = IMPCLS
     C     key_TD        CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_UnitWgt = IMUDN4
     C                   ELSE
     C                   EVAL      wk_UnitWgt = IMIMWG
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ELSE
     C                   EVAL      wk_UnitWgt = *Zero
     C                   ENDIF                                                  %found(HDIMST)
     C                   EVAL(H)   wk_PTWgt = wk_UnitWgt * PQQPIK
     C                   ELSE
     C                   EVAL      wk_PTWgt = -1
     C                   ENDIF                                                  %found(SMPTQ)

     C                   ENDDO

     C                   RETURN    wk_PTWgt

     P RtvPTWgt        E

     P/EJECT
      **************************************************************************
      * Retrieve secondary quantity available fields.                          *
      *   Return values: 0-No error.                                           *
      **************************************************************************
     P RtvSecFlds      B
     D                 PI            10I 0
     D CustGrp                       20A   CONST
     D FullCases                      1A   CONST

     D wk_CustGrp      S                   LIKE(GDCGRP)
     D wk_FullCases    S              1A
     D wk_Error        S             10I 0

     C                   DO

     C                   EVAL      wk_CustGrp = CustGrp
     C                   EVAL      wk_FullCases = FullCases
     C                   EVAL      wk_Error = *Zero

     C                   IF        wk_CustGrp <> *Blanks
     C                   IF        not %open(SMPKGD)
     C                   OPEN      SMPKGD
     C                   ENDIF                                                  not %open(SMPKGD)
     C     key_GD        KLIST                                                  SMPKGD
     C                   KFLD                    PDORD#
     C                   KFLD                    PDORL#
     C                   KFLD                    PDBLN#
     C                   KFLD                    kf_CustGrp
     C                   EVAL      kf_CustGrp = wk_CustGrp
     C     key_GD        CHAIN     SMPKGD
     C                   IF        not %found(SMPKGD)
     C                   CLEAR     *ALL          SMPKGDR
     C                   ENDIF                                                  not %found(SMPKGD)
     C                   ENDIF                                                  wk_CustGrp<>*Blanks

     C                   IF        wk_FullCases = *Blank
     C                   IF        wk_CustGrp = *Blanks
     C                   EVAL      wk_QAV1 = PDQAV1
     C                   EVAL      wk_QAV2 = PDQAV2
     C                   EVAL      wk_QAV3 = PDQAV3
     C                   EVAL      wk_QAV4 = PDQAV4
     C                   EVAL      wk_QAV5 = PDQAV5
     C                   EVAL      wk_QAV6 = PDQAV6
     C                   EVAL      wk_QAV7 = PDQAV7
     C                   EVAL      wk_QAV8 = PDQAV8
     C                   EVAL      wk_HOLD = PDHOLD
     C                   ELSE
     C                   EVAL      wk_QAV1 = GDQAV1
     C                   EVAL      wk_QAV2 = GDQAV2
     C                   EVAL      wk_QAV3 = GDQAV3
     C                   EVAL      wk_QAV4 = GDQAV4
     C                   EVAL      wk_QAV5 = GDQAV5
     C                   EVAL      wk_QAV6 = GDQAV6
     C                   EVAL      wk_QAV7 = GDQAV7
     C                   EVAL      wk_QAV8 = GDQAV8
     C                   EVAL      wk_HOLD = GDHOLD
     C                   ENDIF                                                  wk_CustGrp=*Blanks
     C                   ELSE
     C                   IF        wk_CustGrp = *Blanks
     C                   EVAL      wk_QAV1 = PDQAV1C
     C                   EVAL      wk_QAV2 = PDQAV2C
     C                   EVAL      wk_QAV3 = PDQAV3C
     C                   EVAL      wk_QAV4 = PDQAV4C
     C                   EVAL      wk_QAV5 = PDQAV5C
     C                   EVAL      wk_QAV6 = PDQAV6C
     C                   EVAL      wk_QAV7 = PDQAV7C
     C                   EVAL      wk_QAV8 = PDQAV8C
     C                   EVAL      wk_HOLD = PDHOLDC
     C                   ELSE
     C                   EVAL      wk_QAV1 = GDQAV1C
     C                   EVAL      wk_QAV2 = GDQAV2C
     C                   EVAL      wk_QAV3 = GDQAV3C
     C                   EVAL      wk_QAV4 = GDQAV4C
     C                   EVAL      wk_QAV5 = GDQAV5C
     C                   EVAL      wk_QAV6 = GDQAV6C
     C                   EVAL      wk_QAV7 = GDQAV7C
     C                   EVAL      wk_QAV8 = GDQAV8C
     C                   EVAL      wk_HOLD = GDHOLDC
     C                   ENDIF                                                  wk_CustGrp=*Blanks
     C                   ENDIF                                                  wk_FullCases=*Blank

      *
      * Rebuild program (HYR0184) was changed to include orders on credit hold
      * if the customer has available credit.  The following code will allow
      * those orders to be set for printing.
      *
     C                   IF        wk_HOLD = 'C'                                Past Credit Limit
     C                   EVAL      wk_HOLD = 'X'
     C                   ENDIF                                                  wk_HOLD='C'

     C                   ENDDO

     C                   RETURN    wk_Error

     P RtvSecFlds      E

     P/EJECT
      **************************************************************************
      * Retrieve item available quantity                                       *
      *   Return values: Item available quantity.                              *
      **************************************************************************
     P RtvAvailQty     B
     D                 PI            13P 4
     D Item#                         15A   CONST
     D Whs#                           3P 0 CONST
     D InvtryLoc                      4A   CONST

     D wk_Item#        S                   LIKE(PDITEM)
     D wk_Whs#         S                   LIKE(PDWHS)
     D wk_AvailQty     S             13P 4
     D wk_InvtryLoc    S              4A
     D wk_QtyOH        S                   LIKE(ISQOH)
     D wk_ToolsWhs     S                   LIKE(ISWHS) INZ(1)
     D wk_SecWhs       S                   LIKE(ISWHS) INZ(12)
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Whs# = Whs#
     C                   EVAL      wk_InvtryLoc = InvtryLoc

     C                   IF        not %open(HDIWHS)
     C                   OPEN      HDIWHS
     C                   ENDIF                                                  not %open(HDIWHS)
     C                   IF        not %open(HDITSL05)
     C                   OPEN      HDITSL05
     C                   ENDIF                                                  not %open(HDITSL05)
     C                   IF        not %open(SMITPK)
     C                   OPEN      SMITPK
     C                   ENDIF                                                  not %open(SMITPK)

     C     key_IW        KLIST                                                  HDIWHS
     C                   KFLD                    wk_Item#
     C                   KFLD                    wk_Whs#

     C     key_IS        KLIST                                                  HDITSL05
     C                   KFLD                    wk_Item#
     C                   KFLD                    wk_Whs#

     C     key_IP        KLIST                                                  SMITPK
     C                   KFLD                    wk_Item#
     C                   KFLD                    wk_Whs#

     C     key_IW        CHAIN     HDIWHS
     C                   IF        not %found(HDIWHS)
     C                   EVAL      wk_AvailQty = *Zero
     C                   LEAVE
     C                   ENDIF                                                  not %found(HDIWHS)

     C                   IF        wk_InvtryLoc <> *Blanks
     C                   EVAL      wk_QtyOH = *Zero
     C     key_IS        SETLL     HDITSL05
     C                   DOU       %eof(HDITSL05)
     C     key_IS        READE     HDITSL05
     C                   IF        %eof(HDITSL05)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDITSL05)
     C                   IF        (wk_InvtryLoc = 'WEST'  and
     C                              (ISWHS = wk_ToolsWhs  or
     C                               ISWHS = wk_SecWhs  or
     C                               ISSTKR = 'CNT'  or
     C                               %subst(ISSTKR:1:1) = 'A'  or
     C                               %subst(ISSTKR:1:1) = 'G'))  or
     C                             (wk_InvtryLoc = 'GI'  and
     C                              (ISWHS = wk_ToolsWhs  or
     C                               ISWHS = wk_SecWhs  or
     C                               (ISSTKR <> 'CNT'  and
     C                               %subst(ISSTKR:1:1) <> 'G')))  or
     C                             (wk_InvtryLoc = 'ALDA'  and
     C                               ISWHS <> wk_ToolsWhs and
     C                               ISWHS <> wk_SecWhs and
     C                               %subst(ISSTKR:1:1) <> 'A')
     C                   ITER
     C                   ENDIF                                                  wk_InvtryLoc='WEST'
     C****               IF        ISAILE = 'Z000'
     C****                           or  ISSTKR = 'I06'                         Interim 006
     C****                           or  ISAILE = 'I06'                         Interim 006
     C****                           or  ISSTKR = 'B20'
     C****                           or  ISSLOC = 'STAGE'
     C****                           or (ISSTKR = '050'  and  ISSLOC = 'WILLOW')
     C****                           or  ISAILE = 'TRUC'
     C****                           or  ISSLOC = 'TRUCK'
     C****               ITER
     C****               ENDIF                                                  ISAILE='Z000'
      *
      * Skip specified locations.
      *
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='LOCUNAVAL' and
                      (TDKEY1=digits(:ISWHS) or TDKEY1='*ALL') and
                      (TDKEY2=:ISSTKR or TDKEY2='*ALL') and
                      (TDKEY3=:ISAILE or TDKEY3='*ALL') and
                      (TDKEY4=:ISSLOC or TDKEY4='*ALL') and
                      TDFF01='Y';
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   ITER
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     C                   IF        ISQOH <= *Zero  or  ISQHSR > ISQOH
     C                   ITER
     C                   ENDIF                                                  ISQOH<=*Zero or
     C                   IF        ISQHSR < *Zero
     C                   EVAL      ISQHSR = *Zero
     C                   ENDIF                                                  ISQHSR<*Zero
     C                   EVAL      wk_QtyOH = wk_QtyOH + ISQOH - ISQHSR
     C                   ENDDO                                                  %eof(HDITSL05)
     C                   ENDIF                                                  wk_InvtryLoc<>*Blank

     C     key_IP        CHAIN(N)  SMITPK
     C                   IF        not %found(SMITPK)
     C                   EVAL      IPPPQT = *Zero
     C                   ENDIF                                                  not %found(SMITPK)

     C                   IF        wk_InvtryLoc = *Blanks
     C                   EVAL      wk_AvailQty = IWOHQT - IWQHSR - IWQOPK -
     C                               IPPPQT
     C                   ELSE
     C                   EVAL      wk_AvailQty = wk_QtyOH - IWQOPK -
     C                               IPPPQT
     C                   ENDIF                                                  wk_InvtryLoc=*Blanks
     C                   IF        wk_AvailQty < *Zero
     C                   EVAL      wk_AvailQty = *Zero
     C                   ENDIF                                                  wk_AvailQty<*Zero

     C                   ENDDO

     C                   RETURN    wk_AvailQty

     P RtvAvailQty     E

     P/EJECT
      **************************************************************************
      * Retrieve SMS pick ticket quantity to print.                            *
      *   Return values: Pick ticket quantity.                                 *
      **************************************************************************
     P RtvSMSQty       B
     D                 PI            13P 4
     D AvlQtyFld                      1P 0 CONST

     D wk_AvlQtyFld    S              1P 0
     D wk_SMSQty       S             13P 4

     C                   EVAL      wk_AvlQtyFld = AvlQtyFld

     C                   SELECT
     C                   WHEN      wk_AvlQtyFld = 1
     C                   EVAL      wk_SMSQty = wk_QAV1
     C                   WHEN      wk_AvlQtyFld = 2
     C                   EVAL      wk_SMSQty = wk_QAV2
     C                   WHEN      wk_AvlQtyFld = 3
     C                   EVAL      wk_SMSQty = wk_QAV3
     C                   WHEN      wk_AvlQtyFld = 4
     C                   EVAL      wk_SMSQty = wk_QAV4
     C                   WHEN      wk_AvlQtyFld = 5
     C                   EVAL      wk_SMSQty = wk_QAV5
     C                   WHEN      wk_AvlQtyFld = 6
     C                   EVAL      wk_SMSQty = wk_QAV6
     C                   WHEN      wk_AvlQtyFld = 7
     C                   EVAL      wk_SMSQty = wk_QAV7
     C                   WHEN      wk_AvlQtyFld = 8
     C                   EVAL      wk_SMSQty = wk_QAV8
     C                   OTHER
     C                   EVAL      wk_SMSQty = PDQOPN
     C                   ENDSL

     C                   RETURN    wk_SMSQty

     P RtvSMSQty       E

     P/EJECT
      **************************************************************************
      * Retrieve item OEM status.                                              *
      *   Return values: OEM status.                                           *
      **************************************************************************
     P ItemOEM         B
     D                 PI             1N
     D Item#                         15A   CONST
     D Whs#                           3P 0 CONST

     D wk_Item#        S                   LIKE(PDITEM)
     D wk_Whs#         S                   LIKE(PDWHS)
     D wk_OEM          S              1N

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Whs# = Whs#
     C                   EVAL      wk_OEM = *OFF

     C                   IF        not %open(HDIWHS)
     C                   OPEN      HDIWHS
     C                   ENDIF                                                  not %open(HDIWHS)

     C     key_IW        KLIST                                                  HDIWHS
     C                   KFLD                    wk_Item#
     C                   KFLD                    wk_Whs#

     C     key_IW        CHAIN     HDIWHS
     C                   IF        not %found(HDIWHS)
     C                   LEAVE
     C                   ENDIF                                                  not %found(HDIWHS)

     C                   IF        IWUDA1 = 'OEM'
     C                   EVAL      wk_OEM = *ON
     C                   ENDIF                                                  IWUDA1='OEM'

     C                   ENDDO

     C                   RETURN    wk_OEM

     P ItemOEM         E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR0185      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(SMPKDT)
     C                   CLOSE     SMPKDT
     C                   ENDIF                                                  %open(SMPKDT)

     C                   IF        %open(SMPKDT01)
     C                   CLOSE     SMPKDT01
     C                   ENDIF                                                  %open(SMPKDT01)

     C                   IF        %open(SMPKDT02)
     C                   CLOSE     SMPKDT02
     C                   ENDIF                                                  %open(SMPKDT02)

     C                   IF        %open(SMPKGD)
     C                   CLOSE     SMPKGD
     C                   ENDIF                                                  %open(SMPKGD)

     C                   IF        %open(SMCOSU04)
     C                   CLOSE     SMCOSU04
     C                   ENDIF                                                  %open(SMCOSU04)

     C                   IF        %open(SMCUSU31)
     C                   CLOSE     SMCUSU31
     C                   ENDIF                                                  %open(SMCUSU31)

     C                   IF        %open(SMPTQ)
     C                   CLOSE     SMPTQ
     C                   ENDIF                                                  %open(SMPTQ)

     C                   IF        %open(SMITPK)
     C                   CLOSE     SMITPK
     C                   ENDIF                                                  %open(SMITPK)

     C                   IF        %open(SMCTRL)
     C                   CLOSE     SMCTRL
     C                   ENDIF                                                  %open(SMCTRL)

     C                   IF        %open(HDIWHS)
     C                   CLOSE     HDIWHS
     C                   ENDIF                                                  %open(HDIWHS)

     C                   IF        %open(HDIMST)
     C                   CLOSE     HDIMST
     C                   ENDIF                                                  %open(HDIMST)

     C                   IF        %open(GUPTDAT)
     C                   CLOSE     GUPTDAT
     C                   ENDIF                                                  %open(GUPTDAT)

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR0185      E

     P/EJECT
