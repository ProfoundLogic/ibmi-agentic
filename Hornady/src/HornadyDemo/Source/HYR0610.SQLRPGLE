     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2018')
     H BNDDIR('HYR0610':'QC2LE')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Consolidated Shipping subprocedures                       *
      *          BY: William Earl (MCA)                                        *
      *        DATE: July 2018                                                 *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  12/13/2019  MCA  Skip stockroom I06 locations.                        *
      *  01/28/2020  MCA  Handle multiple-quantity conveyable items without    *
      *                   inner-packs (IWPCPC=0 or IWPCPC=1).  Prior to this   *
      *                   change, one HYPSGDT record was written for the       *
      *                   entire pick, instead of one HYPSGDT record for each  *
      *                   carton pick required.                                *
      *  02/25/2020  MCA  In CrtShipRcds, update HYPSGCU.GCITCT from count of  *
      *                   distinct HYPSGDT.GDITEM.  Sometimes HYR0180 doesn't  *
      *                   correctly populate GCITCT.                           *
      *  03/23/2020  MCA  In CrtGrpTxtFile, include GCMLLI at end of record.   *
      *  04/07/2020  MCA  Cabela's BoxLogix fields - revert to using Bass Pro  *
      *                   fields.                                              *
      *  06/08/2020  MCA  Allow prior-partial-shipped pallet-level orders to   *
      *                   ship as a pallet pick if 50%+ pallet quantity is     *
      *                   still open.  tkt #5162                               *
      *  06/28/2020  MCA  Retrieve shipping-carton item number, and populate   *
      *                   HYPSGDT.GDCTN#.                                      *
      *  06/30/2020  MCA  In CrtGrpTxtFile, include GCMLLC, carton item number,*
      *                   and "create seperate delivery" PO number at end of   *
      *                   record.                                              *
      *  07/31/2020  MCA  For "home" location defaulting (when no inventory    *
      *                   found in BoxLogix-defined locations), ensure only    *
      *                   stockrooms 006 and 060 are used.  This is needed     *
      *                   because the location names in Alda were recently     *
      *                   restructured such that they utilized single-         *
      *                   character aisles, which had been how this program    *
      *                   was ensuring that Alda "home" locations weren't used.*
      *  09/02/2020  MCA  Change ammo seconds to not collect a lot number.     *
      *  10/19/2020  MCA  Skip stockroom A06 aisle I06 locations.              *
      *  01/05/2021  MCA  Fix bug - in CrtShipRcds, when processing "each" or  *
      *                   "box" quantities, if the item is set to              *
      *                   non-conveyable, set pick mode to "tote".             *
      *  03/01/2021  MCA  Skip stockroom B20 locations.                        *
      *  04/27/2021  MCA  Quit always sending picker to A-29-A-10 for blem     *
      *                   ammo.  Instead, blem picker will now go where the    *
      *                   inventory is.                                        *
      *  06/13/2021  MCA  Use *DTAARA SHPLBLSHPV to determine whether to print *
      *                   carrier information on carton labels.  txt #6145     *
      *  07/15/2021  MCA  Purchased ammo inventory FET credits (FETV).         *
      *  07/16/2021  MCA  Fix bug.  When the only inventory for a case-pack    *
      *                   non-conveyable item pick is available in the gondola *
      *                   ("each" location), the label SSCC was being assigned,*
      *                   and should not be.  This caused the P&A shipping     *
      *                   program (HYR0630) to update HYPSGDT.GDQSTC twice.    *
      *  08/03/2021  MCA  Fix bug.  In the pallet quantity adjustment allowing *
      *                   pallet pick for open quantities less than standard   *
      *                   pallet quantities, if the order line's open quantity *
      *                   was large enough that it equated to more than 99     *
      *                   pallets, the wk_PctPlltQty field was not large enough*
      *                   to hold the result.  Changed program to not calculate*
      *                   wk_PctPlltQty if line open quantity was more than    *
      *                   pallet quantity.                                     *
      *  08/06/2021  MCA  Skip "TRUC" locations.                               *
      *  07/07/2022  MCA  For purchased items, use BoxLogix lot code from      *
      *                   AMMOPURCH table if specified.                        *
      *  08/05/2022  MCA  Skip location 050-WILLOW.                            *
      *  08/13/2022  MCA  BoxLogix BL20177 enhancements database change.       *
      *  08/26/2022  MCA  BoxLogix BL20177 enhancements - add new fields to    *
      *                   .txt download file.                                  *
      *  09/06/2022  MCA  BoxLogix BL20177 enhancements.                       *
      *  01/19/2023  MCA  When create group label .txt file, create it with    *
      *                   .tmp suffix, and then when the file is complete,     *
      *                   rename it to .txt.                                   *
      *  03/02/2023  MCA  When renaming group label .tmp file to .txt, check   *
      *                   for and handle errors.                               *
      *  03/22/2023  MCA  Remove all references to BARDATA1 (used for the      *
      *                   BARDATA2.0 conversion).                              *
      *  11/18/2023  MCA  In RtvCSPRO#, check for automatic PRO assignment     *
      *                   override ship-via (e.g., FF to use FT assignment).   *
      *  01/22/2024  MCA  In RtvPickLoc, allow "P" aisle to be valid.          *
      *  05/28/2024  MCA  Change ammo seconds to start collecting a lot number *
      *                   again.                                               *
      *  05/29/2024  MCA  Handle stock master-packs.                           *
      *  08/15/2024  MCA  Conveyable flag "M" for conveyable master-packs.     *
      *  08/22/2024  MCA  If no case-pack barcode found, use UPC.              *
      *  09/27/2024  MCA  Handle Stock master-packs for "pallet" locations.    *
      *  10/06/2024  MCA  Fix bug. When stock master-packs are inventoried in  *
      *                   locations that are defined to be "each" locations    *
      *                   (currently sections 29 & 30 of every "pick-location" *
      *                   aisle), the math wasn't handled to remove the master *
      *                   pack quantity from the case-pack quantity, causing   *
      *                   extra picking records to be created for the master   *
      *                   pack quantities,                                     *
      *  10/07/2024  MCA  Remove "each" location definition (which used to be  *
      *                   sections 29 and 30 of every aisle). There are no     *
      *                   physical locations that are currently defined as     *
      *                    "each" locations.  Keeping this programming was not *
      *                   an issue for case-pack picks. But it was an issue    *
      *                   for the master-pack pick logic, which was ignored    *
      *                   for defined "each" locations.                        *
      *  10/08/2024  MCA  Handle double-letter aisle locations.                *
      *  12/24/2024  MCA  In CrtShipRcds, skip xWRNTY lines when writing       *
      *                   shipment detail.                                     *
      *  01/03/2025  MCA  Pallet pick requests.                                *
      *  01/28/2025  MCA  Skip location TRUCK.                                 *
      *  05/20/2025  MCA  Include ship-to phone number on foreign UCC-128      *
      *                   labels.                                              *
      *  06/18/2025  MCA  When opening the group text file and the group       *
      *                   archive text file, monitor for and handle errors.    *
      *  07/30/2025  MCA  Pallet pick prevention requests.                     *
      *  10/16/2025  MCA  Force oversized items to Tote pick mode.             *
      *  10/16/2025  MCA  Create one pick line for each oversized item.  For   *
      *                   example, if the pick quantity is 4, the program will *
      *                   create 4 pick lines with quantity 1.  That is so the *
      *                   shipper can get 4 UCC-128 labels.                    *
      *  11/18/2025  MCA  In CrtShipRcds, needed to skip more locations.       *
      *                   Changed from hard-coded to table (GUPTDAT.LOCUNAVAL).*
      *                   In RtvPickLoc, skip RDW locations (still hard-coded).*
      *  12/02/2025  MCA  Exclude 006-ALDA-TRUCK location.                     *
      **************************************************************************

     Hnomain

     FOEORHP    IF   E           K DISK    USROPN
     FOEORDP    IF   E           K DISK    USROPN
     FHYPBXPP   IF   E           K DISK    USROPN
     FHYPSGCU   IF   E           K DISK    USROPN
     FHYPSGDT   IF   E           K DISK    USROPN
     FHYPSGDTU  UF   E           K DISK    EXTDESC('HYPSGDT') EXTFILE(*EXTDESC)
     F                                     RENAME(HYRSGDT:HYRSGDTU) USROPN
     FHYPBXPA   UF   E           K DISK    USROPN
     FHYPWICT   IF   E           K DISK    USROPN BLOCK(*NO)
     FHYPWILG   O    E             DISK    USROPN BLOCK(*NO)
     FOECTRL    UF   E             DISK    USROPN
     FHYW0610A  IF   E           K DISK    USROPN
     FHYW0610B  UF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY QPRPSRC,HYR0610

      /COPY QPRPSRC,HYR0185

      /COPY QPRPSRC,HYR9933

      /COPY QPRPSRC,HYR9934

      /COPY QPRPSRC,HYR9937

      /COPY QPRPSRC,EDR9900

      /COPY QPRPSRC,CHKDIGIT

      /COPY EXPAT/QRPGLESRC,ERRNO_H

      /COPY EXPAT/QRPGLESRC,IFSIO_H

     D sleep           PR            10I 0 ExtProc('sleep')
     D  Seconds                      10U 0 Value

     D RtvPickLoc      PR            17A
     D  ShipGrp#                      9S 0 CONST
     D  Item#                        15A   CONST
     D  Whs                           3S 0 CONST
     D  PickType                      1A   CONST
      *** P = Pallet
      *** C = Case
      *** E = Each/box
     D  PickQty                       9P 0 CONST                                Stocking UOM qty
     D  CaseQty                       9P 0 CONST                                Each/boxes per case
     D  MixLotCtns                    1A   CONST
     D  PrtlCtns                      1A   CONST
     D  ProdCls                       4A   CONST

     D RtvPltLotCnt    PR             5P 0
     D  ShipGrp#                      9S 0 CONST
     D  Item#                        15A   CONST
     D  Whs                           3S 0 CONST
     D  PltUnitQty                    9P 0 CONST                                Full pallet unit qty

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D  Command                      80A   OPTIONS(*VARSIZE) CONST
     D  CommandLen                   15P 5 CONST

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

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263
     D  ps_Job#              264    269

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_ShipGrpNbr   S              9P 0 DTAARA(SHPGRPNBR)                    Shipment group no.
     D wk_SSCCSrlRef   S              9P 0 DTAARA(SSCCSRLREF)                   SSCC serial no.
     D wk_ShpLblShpV   S              2A   DTAARA(SHPLBLSHPV)                   Print shp-via on lbl
     D wk_UCC128Phn    S              1A   DTAARA(HYAUCCPHN)                    Print phone# UCC-128
     D wk_BxlMXPO      S              1A   DTAARA(HYABXLMXPO)                   Seperate POs
     D wk_BXLPath1     S            128A   DTAARA(BXLPATH1)                     BoxLogix group path
     D wk_BXLPath2     S            128A   DTAARA(BXLPATH2)                     BoxLogix label path
     D wk_BXLPath3     S            128A   DTAARA(BXLPATH3)                     BoxLogix unsusp path
     D wk_BXLPath4     S            128A   DTAARA(BXLPATH4)                     BoxLogix group arch
     D wk_BXLPath5     S            128A   DTAARA(BXLPATH5)                     BoxLogix label arch

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_CtnLblShpV   S              1N
     D wk_EachTaken    S              5P 0
     D kf_GRP#         S              9S 0
     D kf_ITEM         S             15A
     D kf_WHS          S              3S 0
     D kf_LTYP         S              1A
     D kf_PPTURN       S                   LIKE(PPTURN)
     D kf_PPSEQ#       S                   LIKE(PPSEQ#)
     D kf_PGM          S                   LIKE(WCPGM)
     D kf_CTYP         S                   LIKE(WCCTYP)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

      **************************************************************************
      * Define key lists.                                                      *
      **************************************************************************
     C     key_W1        KLIST                                                  HYW0610A
     C                   KFLD                    kf_GRP#
     C                   KFLD                    kf_ITEM
     C                   KFLD                    kf_WHS
     C                   KFLD                    kf_LTYP

     C     key_W2        KLIST                                                  HYW0610B
     C                   KFLD                    kf_GRP#
     C                   KFLD                    kf_ITEM
     C                   KFLD                    kf_WHS

     C     key_PP        KLIST                                                  HYPBXPP
     C                   KFLD                    kf_PPTURN
     C                   KFLD                    kf_PPSEQ#

     C     key_WC        KLIST                                                  HYPWICT
     C                   KFLD                    kf_PGM
     C                   KFLD                    kf_CTYP

     P/EJECT
      **************************************************************************
      * Check order's busy flag.                                               *
      **************************************************************************
     P ChkOrdBusy      B                   EXPORT
     D                 PI             1N
     D Order#                         8S 0 CONST

     D wk_Order#       S              8S 0
     D wk_Busy         S              1N
     D wk_OEBUSY       S              1A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Busy = *OFF
     C                   EVAL      wk_OEBUSY = *Blank

      /Free
       EXEC SQL select OEBUSY into :wk_OEBUSY
                from OEORHD
                where OEORD#=:wk_Order#;
      /End-Free

     C                   IF        wk_OEBUSY = 'B'
     C                   EVAL      wk_Busy = *ON
     C                   ENDIF                                                  wk_OEBUSY='B'

     C                   ENDDO

     C                   RETURN    wk_Busy

     P ChkOrdBusy      E

     P/EJECT
      **************************************************************************
      * Retrieve order's busy info.                                            *
      *  Pos  1-10  User                                                       *
      *  Pos 12-30  Date/Time (yyyy-mm-dd-hh.mm.ss)                            *
      **************************************************************************
     P RtvOrdBusy      B                   EXPORT
     D                 PI            30A
     D Order#                         8S 0 CONST

     D wk_Order#       S              8S 0
     D wk_OEBUSY       S              1A
     D wk_OEUDF9       S             30A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_OEBUSY = *Blank
     C                   EVAL      wk_OEUDF9 = *Blanks

      /Free
       EXEC SQL select OEBUSY, OEUDF9 into :wk_OEBUSY, :wk_OEUDF9
                from OEORHD
                where OEORD#=:wk_Order#;
      /End-Free

     C                   IF        wk_OEBUSY <> 'B'  and
     C                               wk_OEUDF9 <> *Blanks
     C                   EVAL      wk_OEUDF9 = *Blanks
     C                   ENDIF                                                  wk_OEBUSY<>'B' and

     C                   IF        wk_OEBUSY = 'B'  and
     C                               wk_OEUDF9 = *Blanks
     C                   EVAL      wk_OEUDF9 = '*UNKNOWN'
     C                   ENDIF                                                  wk_OEBUSY='B' and

     C                   ENDDO

     C                   RETURN    wk_OEUDF9

     P RtvOrdBusy      E

     P/EJECT
      **************************************************************************
      * Set order's busy flag.                                                 *
      *   Return values: 0-No error                                            *
      *                  1-Order busy for another user                         *
      **************************************************************************
     P SetOrdBusy      B                   EXPORT
     D                 PI            10I 0
     D Order#                         8S 0 CONST
     D BusyUser                      10A   CONST
     D BusyTstp                      19A   CONST
      ***  yyyy-mm-dd-hh.mm.ss or *CURRENT

     D wk_Order#       S              8S 0
     D wk_BusyUser     S             10A
     D wk_BusyTstp     S             19A
     D wk_Error        S             10I 0
     D wk_OEUDF9       S             30A

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_BusyUser = BusyUser
     C                   EVAL      wk_BusyTstp = BusyTstp
     C                   EVAL      wk_Error = *Zero

     C                   IF        wk_BusyTstp = '*CURRENT'
     C                   EVAL      wk_BusyTstp = %char(%timeStamp)
     C                   ENDIF                                                  wk_BusyTstp='*CURREN

     C                   EVAL      wk_OEUDF9 = wk_BusyUser + ' ' + wk_BusyTstp

      /Free
       EXEC SQL update OEORHD set OEBUSY='B', OEUDF9=:wk_OEUDF9
                where OEORD#=:wk_Order# and OEBUSY=' ';
       IF SQLER3 = *Zero  and                                                // NOTE: SQLER3 has the
          %subst(RtvOrdBusy(wk_Order#):1:10) <> wk_BusyUser;                 //   rcds updated count
         wk_Error = 1;
       ENDIF;
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_Error

     P SetOrdBusy      E

     P/EJECT
      **************************************************************************
      * Set turnaround's busy user.                                            *
      *   Return values: 0-No error                                            *
      *                  1-Turnaround busy for another user                    *
      **************************************************************************
     P SetTurnBusy     B                   EXPORT
     D                 PI            10I 0
     D Turn#                          9S 0 CONST
     D BusyUser                      10A   CONST
     D BusyTstp                        Z   CONST
      ***  yyyy-mm-dd-hh.mm.ss.mmmmmm or 0001-01-01-00.00.00.000000 (*CURRENT)

     D wk_Turn#        S              9S 0
     D wk_BusyUser     S             10A
     D wk_BusyTstp     S               Z
     D wk_Error        S             10I 0

     C                   DO

     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_BusyUser = BusyUser
     C                   EVAL      wk_BusyTstp = BusyTstp
     C                   EVAL      wk_Error = *Zero

     C                   IF        wk_BusyTstp = *Loval
     C                   EVAL      wk_BusyTstp = %timestamp
     C                   ENDIF                                                  wk_BusyTstp=*Loval

      /Free
       EXEC SQL update HYPTDTA set TDBUSY=:wk_BusyUser, TDTSTP2=:wk_BusyTstp
                where TDTURN=:wk_Turn# and TDBUSY=' ';
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_Error

     P SetTurnBusy     E

     P/EJECT
      **************************************************************************
      * Check shipment's orders' busy flags (prior to shipment creation).      *
      **************************************************************************
     P ChkShipOrdBusy  B                   EXPORT
     D                 PI             1N

     D wk_Busy         S              1N
     D wk_BusyCnt      S              5P 0

     C                   DO

     C                   EVAL      wk_Busy = *OFF
     C                   EVAL      wk_BusyCnt = *Zero

      /Free
       EXEC SQL select count(distinct PDORD#) into :wk_BusyCnt
                from SMPKDT a
                where a.PDPTPP='P' and
                 exists (select * from OEORHD b where b.OEORD#=a.PDORD# and
                  b.OEBUSY='B' and b.OEHOLD=' ' and b.OEFL05<>'1') and
                 exists (select * from OEORDT c where c.ODORD#=a.PDORD# and
                  c.ODORL#=a.PDORL# and c.ODBLN#=a.PDBLN# and c.ODBLKT<>'Y'
                  and c.ODORST='O');
      /End-Free

     C                   IF        wk_BusyCnt > *Zero
     C                   EVAL      wk_Busy = *ON
     C                   ENDIF                                                  wk_BusyCnt>*Zero

     C                   ENDDO

     C                   RETURN    wk_Busy

     P ChkShipOrdBusy  E

     P/EJECT
      **************************************************************************
      * Create new shipment group.                                             *
      *   Return values: New shipment group number.                            *
      **************************************************************************
     P CrtShipGrp      B                   EXPORT
     D                 PI             9S 0
     D DCLoc                          4A   CONST
     D Timestamp                       Z   CONST

     D wk_DCLoc        S              4A
     D wk_Timestamp    S               Z
     D wk_LovalTS      S               Z
     D wk_ShipGrp#     S              9S 0
     D wk_User         S             10A

     C                   DO

     C                   EVAL      wk_DCLoc = DCLoc
     C                   EVAL      wk_Timestamp = Timestamp

     C                   EVAL      wk_LovalTS = *Loval
     C                   EVAL      wk_User = ps_User

      *
      * Retrieve next shipment group number.
      *
     C     *LOCK         IN        wk_ShipGrpNbr
     C                   EVAL      wk_ShipGrpNbr = wk_ShipGrpNbr + 1
     C                   OUT       wk_ShipGrpNbr

     C                   EVAL      wk_ShipGrp# = wk_ShipGrpNbr

      *
      * Create a shipment group header record.
      *
      /Free
       EXEC SQL insert into HYPSGHD
                       (GHGRP#, GHSTS, GHDCLOC, GHUSER,
                        GHTSTP1, GHSCUS, GHSCWS, GHTSTP2)
                values(:wk_ShipGrp#, 'O', :wk_DCLoc, :wk_User,
                       :wk_Timestamp,' ',' ',:wk_LovalTS);
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_ShipGrp#

     P CrtShipGrp      E

     P/EJECT
      **************************************************************************
      * Create shipment records.                                               *
      *   Return values: 0-No error                                            *
      *                  1-Invalid turnaround number                           *
      *                  2-Ship-to name not found                              *
      *                  3-Shipment group customer record not found            *
      **************************************************************************
     P CrtShipRcds     B                   EXPORT
     D                 PI            10I 0
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D OrdCtl#                        8S 0 CONST
     D Lane1                          1S 0 CONST
     D Lane2                          1S 0 CONST
     D Lane3                          1S 0 CONST
     D Lanes                          1S 0 CONST
     D ShipType                       1A   CONST                                P=Parcel, T=Truck
     D ShipVia                        2A   CONST
     D Timestamp                       Z   CONST

     D wk_ShipGrp#     S              9S 0
     D wk_Turn#        S              9S 0
     D wk_OrdCtl#      S              8S 0
     D wk_Lane1        S              1S 0
     D wk_Lane2        S              1S 0
     D wk_Lane3        S              1S 0
     D wk_Lanes        S              1S 0
     D wk_ShipType     S              1A
     D wk_ShipVia      S              2A
     D wk_Timestamp    S               Z
     D wk_Error        S             10I 0
     D wk_LovalTS      S               Z
     D wk_DatabaseID@  S              2A
     D wk_STName       S             30A
     D wk_STZip        S             13A
     D wk_BillTo#X     S              7A
     D wk_BuyGrpCust   S              1N
     D wk_CMCNA1       S             26A
     D wk_CMMXPO       S              1A
     D wk_CMMXLC       S              1A
     D wk_CMPRCN       S              1A
     D wk_CMPLLB       S              1A
     D wk_CMSLLC       S              1A
     D wk_IMPLLB       S              1A
     D wk_AlwPlltLbls  S              1A
     D wk_OETFRT       S              9P 2
     D wk_OETMSG       S              9P 2
     D wk_OEUDF1       S             15A
     D wk_ODITEM       S             15A
     D wk_ODIMDS       S             30A
     D wk_ODPCLS       S              4A
     D wk_ODWH         S              3S 0
     D wk_ODQORD       S             13P 4
     D wk_ODQSTD       S             13P 4
     D wk_ODQSTC       S             13P 4
     D wk_ODOREC       S              1A
     D wk_IMPCLS       S              4A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN4       S             13P 5
     D wk_IMLOT        S              1A
     D wk_IMUOMS       S              2A
     D wk_IMCNVY       S              1A
     D wk_IMOVSZ       S              1A
     D wk_IMPLTQ       S              7P 0
     D wk_IMPLTQ@U     S              9P 0
     D wk_PlltQty      S              7P 0
     D wk_PlltQty@U    S              9P 0
     D wk_BDITEM       S             15A
     D wk_BDUPC12      S             12A
     D wk_BDUCC14      S             14A
     D wk_BDUCCB14     S             14A
     D wk_BDQTYBAR     S              7P 0
     D wk_BDUCC14I     S             14A
     D wk_BDQTYIPC     S              5P 0
     D wk_IWPCPC       S              9P 4
     D wk_GCGRP        S              9S 0
     D wk_GCCPTY       S              3S 0
     D wk_GCBLTO       S              7S 0
     D wk_GCSHTO       S              7S 0
     D wk_GCDSHP       S              7S 0
     D wk_GCSHPV       S              2A
     D wk_GCSVDS       S             15A
     D wk_GCFTRM       S              2A
     D wk_GCFRTA       S              7P 2
     D wk_GCCHGA       S              7P 2
     D wk_GCITCT       S              5P 0
     D wk_GCSHWT       S              5P 0
     D wk_GCPANS       S              1A
     D wk_GDPANS       S              1A
     D wk_IHBLTOx      S              7A
     D wk_TDTABL       S             10A
     D wk_TDKEY1       S             15A
     D wk_TDKEY2       S             15A
     D wk_TDCF03       S             30A
     D wk_LotCtrl      S              1A
     D wk_TblLotCode   S              2A
     D wk_LotEntry     S              2A
     D wk_MixLotCtns   S              1A
     D wk_PrtlCtns     S              1A
     D wk_CtnItem#     S             15A
     D wk_QtyOpen      S             13P 4
     D wk_PctPlltQty   S              5P 3
     D wk_LblLoc       S              1A
     D wk_Bulk         S              1N
     D wk_CaseQty      S              5P 0
     D wk_W1Cnt        S              5P 0
     D wk_PickUnitWgt  S              9P 5
     D wk_PickUnitQty  S              9P 0
     D wk_PickUOM      S              2A
     D wk_PickLOC      S             17A
     D wk_CompPlt      S              1A
     D wk_PltLotCnt    S              5P 0
     D wk_ItemBC       S             14A
     D wk_SSCCBC       S             20A
     D wk_SSCC         S              9P 0
     D wk_StkUOMQty    S              9P 0
     D wk_StkUOMQty2   S              9P 0
     D wk_FullPlts     S              5P 0
     D wk_MstrPacks    S              5P 0
     D wk_FullCases    S              5P 0
     D wk_PrtlUnits    S              5P 0
     D wk_PltUnits     S              9P 0
     D wk_PltCnt       S              5P 0
     D wk_MPUnits      S              7P 0
     D wk_MPCnt        S              5P 0
     D wk_EachQty1     S              5P 0
     D wk_EachQty2     S              5P 0
     D wk_Done1        S              1N
     D wk_PickMode     S              1A
     D wk_ToteCases    S              5P 0
     D wk_FrtTerms     S              2A
     D wk_FrtAmt       S              7P 2
     D wk_SpclChg      S              7P 2
     D wk_PickQty      S              9P 0
     D wk_CaseCnt      S              5P 0
     D wk_Serial#      S              9P 0
     D wk_ChkDgt       S              1A
     D wk_Seq#         S                   LIKE(GDRSEQ)
     D wk_RC0185       S             10I 0
     D sv_TURN         S                   LIKE(GDTURN)
     D sv_TSEQ         S                   LIKE(GDTSEQ)
     D sv_PickMode     S                   LIKE(wk_PickMode)

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_OrdCtl# = OrdCtl#
     C                   EVAL      wk_Lane1 = Lane1
     C                   EVAL      wk_Lane2 = Lane2
     C                   EVAL      wk_Lane3 = Lane3
     C                   EVAL      wk_Lanes = Lanes
     C                   EVAL      wk_ShipType = ShipType
     C                   EVAL      wk_ShipVia = ShipVia
     C                   EVAL      wk_Timestamp = Timestamp
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(OEORHP)
     C                   OPEN      OEORHP
     C                   ENDIF                                                  not %open(OEORHP)

     C                   IF        not %open(OEORDP)
     C                   OPEN      OEORDP
     C                   ENDIF                                                  not %open(OEORDP)

     C                   IF        not %open(HYPBXPP)
     C                   OPEN      HYPBXPP
     C                   ENDIF                                                  not %open(HYPBXPP)

     C                   IF        wk_DatabaseID = *Blanks
     C                   IN        wk_DatabaseID
     C                   ENDIF                                                  wk_DatabaseID=*Blank
     C                   EVAL      wk_DatabaseID@ = wk_DatabaseID               Copy to local var

     C                   IF        wk_BXLPath1 = *Blanks
     C                   IN        wk_BxlPath1
     C                   ENDIF                                                  wk_BXLPath1=*Blanks
     C                   IF        wk_BXLPath2 = *Blanks
     C                   IN        wk_BxlPath2
     C                   ENDIF                                                  wk_BXLPath2=*Blanks
     C                   IF        wk_BXLPath4 = *Blanks
     C                   IN        wk_BxlPath4
     C                   ENDIF                                                  wk_BXLPath4=*Blanks
     C                   IF        wk_BXLPath5 = *Blanks
     C                   IN        wk_BxlPath5
     C                   ENDIF                                                  wk_BXLPath5=*Blanks

     C                   EVAL      wk_LovalTS = *Loval

     C     wk_Turn#      CHAIN     OEORHP
     C                   IF        not %found(OEORHP)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %found(OEORHP)

      /Free
       wk_STName = *Blanks;
       wk_STZip = *Blanks;
       IF IHDSHP = *Zero;
         EXEC SQL select CMCNA1, CMZIP into :wk_STName, :wk_STZip
                  from HDCUST
                  where CMCUST=:IHSHTO;
       ELSE;
         EXEC SQL select DSNAME, DSZIP into :wk_STName, :wk_STZip
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:IHSHTO and DSNMBR=:IHDSHP;
       ENDIF;
       IF wk_STName= *Blanks;
         EVAL wk_Error = 2;
         LEAVE;
       ENDIF;                                                                // wk_STName=*Blanks

       wk_BillTo#X = %editc(IHBLTO:'X');
       wk_TDKEY2 = *Blanks;
       EXEC SQL select TDKEY2 into :wk_TDKEY2
                from GUPTDAT
                where TDTABL='BUYINGGRPS' and TDKEY1=:wk_DatabaseID@ and
                 TDKEY2=:wk_BillTo#X;
       IF wk_TDKEY2 = wk_BillTo#X;
         wk_BuyGrpCust = *ON;
       ELSE;
         wk_BuyGrpCust = *OFF;
       ENDIF;                                                                // wk_TDKEY2=wk_BillTo#

       wk_CMMXPO = *Blank;
       wk_CMMXLC = *Blank;
       wk_CMPRCN = *Blank;
       wk_CMPLLB = *Blank;
       wk_CMSLLC = *Blank;
       IF wk_BuyGrpCust;
         EXEC SQL select CMMXPO, CMMXLC, CMPRCN, CMPLLB,
                         CMSLLC
                  into :wk_CMMXPO, :wk_CMMXLC, :wk_CMPRCN, :wk_CMPLLB,
                       :wk_CMSLLC
                  from HDCUST2
                  where CMCUST=:IHSHTO;
       ELSE;
      //   IF IHBLTO = 59;
      //     wk_CMCNA1 = *Blanks;
      //     EXEC SQL select CMCNA1 into :wk_CMCNA1
      //              from HDCUST
      //              where CMCUST=:IHSHTO;
      //   ENDIF;                                                              // IHBLTO=59
      //   IF IHBLTO = 59  and  %scan('CABELA':wk_CMCNA1) > *Zero;
      //     EXEC SQL select CMMXPO, CMMXLC, CMPRCN, CMPLLB,
      //                     CMSLLC
      //              into :wk_CMMXPO, :wk_CMMXLC, :wk_CMPRCN, :wk_CMPLLB,
      //                   :wk_CMSLLC
      //              from HDCUST2
      //              where CMCUST=37;
      //   ELSE;
           EXEC SQL select CMMXPO, CMMXLC, CMPRCN, CMPLLB,
                           CMSLLC
                    into :wk_CMMXPO, :wk_CMMXLC, :wk_CMPRCN, :wk_CMPLLB,
                         :wk_CMSLLC
                    from HDCUST2
                    where CMCUST=:IHBLTO;
      //   ENDIF;                                                              // IHBLTO=59 and
       ENDIF;                                                                // wk_BuyGrpCust

      // Expected ship-label location values - T=Top, F=Front, M=Manual.
      // If M=Manual (or invalid value), use T=Top.
       IF wk_CMSLLC = 'F';
         wk_LblLoc = 'F';
       ELSE;
         wk_LblLoc = 'T';
       ENDIF;                                                                // wk_CMSLLC='F'

       wk_OETFRT = *Zero;
       wk_OETMSG = *Zero;
       wk_OEUDF1 = *Blanks;
       EXEC SQL select OETFRT, OETMSG, OEUDF1
                 into :wk_OETFRT, :wk_OETMSG, :wk_OEUDF1
                from OEORHD
                where OEORD#=:IHORD#;
       wk_FrtTerms = %subst(wk_OEUDF1:1:2);

       wk_GCGRP = *Zero;
       wk_GCCPTY = *Zero;
       wk_GCBLTO = *Zero;
       wk_GCSHTO = *Zero;
       wk_GCDSHP = *Zero;
       wk_GCSHPV = *Blanks;
       wk_GCSVDS = *Blanks;
       wk_GCFTRM = *Blanks;
       wk_GCFRTA = *Zero;
       wk_GCCHGA = *Zero;
       wk_GCITCT = *Zero;
       wk_GCSHWT = *Zero;
       wk_GCPANS = *Blank;
       EXEC SQL select GCGRP#, GCCPTY, GCBLTO, GCSHTO,
                       GCDSHP, GCSHPV, GCSVDS, GCFTRM,
                       GCFRTA, GCCHGA, GCITCT, GCSHWT,
                       GCPANS
                 into :wk_GCGRP, :wk_GCCPTY, :wk_GCBLTO, :wk_GCSHTO,
                      :wk_GCDSHP, :wk_GCSHPV, :wk_GCSVDS, :wk_GCFTRM,
                      :wk_GCFRTA, :wk_GCCHGA, :wk_GCITCT, :wk_GCSHWT,
                      :wk_GCPANS
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:IHBLTO and
                 GCSHTO=:IHSHTO and GCDSHP=:IHDSHP;

       IF wk_GCGRP = *Zero  or  wk_GCBLTO=*Zero  or
         wk_GCSHTO = *Zero;
         EVAL wk_Error = 3;
         LEAVE;
       ENDIF;                                                                // wk_GCGRP=*Zero or
      /End-Free

     C     wk_Turn#      SETLL     OEORDP
     C                   DOU       %eof(OEORDP)
     C     wk_Turn#      READE     OEORDP
     C                   IF        %eof(OEORDP)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP)

     C                   IF        IDQOPK <= *Zero
     C                   ITER
     C                   ENDIF                                                  IDQOPK<=Zero

     C                   EVAL      wk_StkUOMQty = IDQOPK                        Stk UOM pick qty

      /Free
       wk_ODITEM = *Blanks;
       wk_ODIMDS = *Blanks;
       wk_ODPCLS = *Blanks;
       wk_ODWH = *Zero;
       wk_ODQORD = *Zero;
       wk_ODQSTD = *Zero;
       wk_ODQSTC = *Zero;
       wk_ODOREC = *Blank;
       EXEC SQL select ODITEM, ODIMDS, ODPCLS, ODWH,
                       ODQORD, ODQSTD, ODQSTC, ODOREC
                 into :wk_ODITEM, :wk_ODIMDS, :wk_ODPCLS, :wk_ODWH,
                      :wk_ODQORD, :wk_ODQSTD, :wk_ODQSTC, :wk_ODOREC
                from OEORDT
                where ODORD#=:IDORD# and ODORL#=:IDORL# and
                 ODBLN#=:IDBLN#;

       IF %subst(wk_ODITEM:2:6) = 'WRNTY ';                                  // Skip xWRNTY lines
         ITER;
       ENDIF;                                                                // %subst(wk_ODITEM:2:6

       IF wk_ODOREC = 'S'  or  wk_ODOREC='X';
         wk_IMPCLS = *Blanks;
         wk_IMIMWG = *Zero;
         wk_IMUDN4 = *Zero;
         wk_IMLOT = *Blank;
         wk_IMUOMS = *Blanks;
         EXEC SQL select IMPCLS, IMIMWG, IMUDN4, IMLOT, IMUOMS
                   into :wk_IMPCLS, :wk_IMIMWG, :wk_IMUDN4, :wk_IMLOT,
                        :wk_IMUOMS
                  from HDIMST
                  where IMITEM=:wk_ODITEM;
         wk_IMCNVY = *Blank;
         wk_IMOVSZ = *Blank;
         wk_IMPLTQ = *Zero;
         wk_IMPLLB = *Blank;
         EXEC SQL select IMCNVY, IMOVSZ, IMPLTQ, IMPLLB
                   into :wk_IMCNVY, :wk_IMOVSZ, :wk_IMPLTQ, :wk_IMPLLB
                  from HDIMST2
                  where IMITEM=:wk_ODITEM;
       ELSE;
         wk_IMPCLS = wk_ODPCLS;
         wk_IMIMWG = *Zero;
         wk_IMUDN4 = *Zero;
         wk_IMLOT = 'N';
         wk_IMCNVY = *Blank;
         wk_IMOVSZ = *Blank;
         wk_IMPLTQ = *Zero;
         wk_IMPLLB = *Blank;
       ENDIF;                                                                // wk_ODOREC='S' or

       IF wk_IMPLLB = 'Y';
         wk_AlwPlltLbls = 'Y';
       ELSE;
         wk_AlwPlltLbls = wk_CMPLLB;
       ENDIF;                                                                // wk_IMPLLB='Y'

       wk_BDITEM = *Blanks;
       wk_BDUPC12 = *Blanks;
       wk_BDUCC14 = *Blanks;
       wk_BDUCCB14 = *Blanks;
       wk_BDQTYBAR = *Zero;
       wk_BDUCC14I = *Blanks;
       wk_BDQTYIPC = *Zero;
       EXEC SQL select BDITEM, BDUPC12, BDUCC14, BDUCC14I,
                       BDUCCB14, BDQTYBAR,
                       BDQTYIPC
                 into :wk_BDITEM, :wk_BDUPC12, :wk_BDUCC14, :wk_BDUCC14I,
                      :wk_BDUCCB14, :wk_BDQTYBAR,
                      :wk_BDQTYIPC
                from BARDATA
                where BDITEM=:wk_ODITEM;
       // IF wk_BDITEM <> wk_ODITEM;
       //   EXEC SQL select BDITEM, BDUPC12, BDUCC14, BDUCC14I,              // If not found in 2.0,
       //                   BDQTYIPC                                         // check legacy file
       //             into :wk_BDITEM, :wk_BDUPC12, :wk_BDUCC14, :wk_BDUCC14I,
       //                  :wk_BDQTYIPC
       //            from BARDATA1
       //            where BDITEM=:wk_ODITEM;
       // ENDIF;                                                             // wk_BDITEM<>wk_ODITEM
       IF wk_BDUPC12 = *Blanks;
         wk_BDUPC12 = wk_ODITEM;
       ENDIF;                                                                // wk_BDUPC12=*Blanks
       IF wk_BDUCC14 = *Blanks;
      // wk_BDUCC14 = wk_ODITEM;
         wk_BDUCC14 = wk_BDUPC12;
       ENDIF;                                                                // wk_BDUCC14=*Blanks

       wk_TDTABL = *Blanks;
       wk_TDKEY1 = *Blanks;
       EXEC SQL select TDTABL, TDKEY1 into :wk_TDTABL, :wk_TDKEY1
                from GUPTDAT
                where TDTABL='BULKPRDCLS' and TDKEY1=:wk_IMPCLS;
       IF wk_TDTABL = 'BULKPRDCLS'  and  wk_TDKEY1 = wk_IMPCLS;
         wk_Bulk = *ON;
       ELSE;
         wk_Bulk = *OFF;
       ENDIF;                                                                // wk_TDTABL='BULKPRDCL

       wk_IWPCPC = 1;
       IF wk_ODOREC = 'S'  or  wk_ODOREC = 'X';
         EXEC SQL select IWPCPC into :wk_IWPCPC
                  from HDIWHS
                  where IWWHS=:wk_ODWH and IWITEM=:wk_ODITEM;
       IF wk_IWPCPC = *Zero;
         wk_IWPCPC = 1;
       ENDIF;                                                                // wk_IWPCPC=*Zero;
       ENDIF;                                                                // wk_ODOREC='S' or
       EVAL wk_CaseQty = wk_IWPCPC;

      //
      // If conveyable item is oversized, change to tote pick.
      //
       IF wk_IMCNVY <> *Blank  and  wk_IMOVSZ <> *Blank;
         SELECT;
           WHEN wk_IMOVSZ = 'E';                                             // Each
             wk_IMCNVY = *Blank;
           WHEN wk_IMOVSZ = 'C'  and  wk_CaseQty > 1;                        // Carton
             IF wk_StkUOMQty >= wk_CaseQty;
               wk_IMCNVY = *Blank;
             ELSE;
               wk_IMOVSZ = *Blank;
             ENDIF;                                                          // wk_StkUOMQty>=wk_Cas
           WHEN wk_IMOVSZ = 'M'  and  wk_BDQTYBAR > 1;                       // Master-Carton
             IF wk_StkUOMQty >= wk_BDQTYBAR;
               wk_IMCNVY = *Blank;
             ELSE;
               wk_IMOVSZ = *Blank;
             ENDIF;                                                          // wk_StkUOMQty>=wk_BDQ
         ENDSL;
       ENDIF;                                                                // wk_IMCNVY<>*Blank

       wk_IMPLTQ@U = wk_IMPLTQ * wk_CaseQty;

       IF wk_IMLOT = 'M'  or  wk_IMLOT = 'A';
         wk_LotCtrl = 'Y';
       ELSE;
         wk_LotCtrl = 'N';
       ENDIF;                                                                // wk_IMLOT='M' or

       IF wk_LotCtrl = 'Y'  or
         wk_IMPCLS = 'AAAA'  or  wk_IMPCLS = 'AAGC'  or                      // Rimfire
         (%subst(wk_IMPCLS:1:1) = 'F'  and                                   // Frontier
           %subst(wk_IMPCLS:1:3) <> 'FET')  or
         %subst(wk_IMPCLS:1:2) = 'AS';                                       // Ammo seconds
         wk_TblLotCode = *Blanks;
         wk_TDCF03 = *Blanks;
         EXEC SQL select TDCF03 into :wk_TDCF03
                  from GUPTDAT
                  where TDTABL='AMMOPURCH' and TDKEY1=:wk_ODITEM;
         IF wk_TDCF03 <> *Blanks  and  %len(%trimr(wk_TDCF03)) = 2;
           wk_TblLotCode = wk_TDCF03;
         ENDIF;                                                              // wk_TDCF03<>*Blanks
         SELECT;
         WHEN wk_TblLotCode <> *Blanks;                                      // Table-specified
           wk_LotEntry = wk_TblLotCode;
         WHEN wk_IMPCLS = 'AAAA';                                            // Rimfire ammo
           wk_LotEntry = '02';
         WHEN wk_IMPCLS = 'AAGA';                                            // Cntrfire ref & proof
           wk_LotEntry = '03';
         WHEN wk_IMPCLS = 'AAGC';                                            // Rimfire ref & proof
           wk_LotEntry = '04';
         WHEN %subst(wk_IMPCLS:1:2) = 'AS';                                  // Ammo seconds
       //  wk_LotEntry = '05';
       //  wk_LotEntry = '00';
           wk_LotEntry = '05';
         WHEN %subst(wk_IMPCLS:1:1) = 'A';                                   // Centerfile ammo
           wk_LotEntry = '01';
         WHEN %subst(wk_IMPCLS:1:1) = 'F';                                   // Frontier ammo
           wk_LotEntry = '06';
         OTHER;                                                              // Unknown
           wk_LotEntry = '99';
         ENDSL;
       ELSE;
         wk_LotEntry = '00';
       ENDIF;                                                                // wk_LotCtrl='Y'

       IF wk_LotEntry = '00'  and
         (%subst(wk_IMPCLS:1:2) = 'BB'  or                                   // Bulk bullets
          wk_IMPCLS = 'CAAA'  or                                             // Bulk cases
          wk_IMPCLS = 'CAAC'  or                                             //   "   "
          wk_IMPCLS = 'CBAA'  or                                             //   "   "
          wk_IMPCLS = 'CBAC'  or                                             //   "   "
          wk_IMPCLS = 'CCAA'  or                                             //   "   "
          wk_IMPCLS = 'CCAC'  or                                             //   "   "
          %subst(wk_IMPCLS:1:2) = 'CH');                                     //   "   "
         wk_TDTABL = *Blanks;
         wk_TDKEY1 = *Blanks;
         wk_TDKEY2 = *Blanks;
         EXEC SQL select TDTABL, TDKEY1, TDKEY2
                   into :wk_TDTABL, :wk_TDKEY1, :wk_TDKEY2
                  from GUPTDAT
                  where TDTABL='ITEMGRP' and TDKEY1='BBSKIPGRP' and
                        TDKEY2=:wk_ODITEM;
         IF wk_TDTABL <> 'ITEMGRP'  or  wk_TDKEY1 <> 'BBSKIPGRP'  or
              wk_TDKEY2 <> wk_ODITEM;
           wk_IHBLTOx = %editc(IHBLTO:'X');
           IF IHORTY <> 'V';
             wk_TDTABL = *Blanks;
             wk_TDKEY1 = *Blanks;
             EXEC SQL select TDTABL, TDKEY1
                       into :wk_TDTABL, :wk_TDKEY1
                      from GUPTDAT
                      where TDTABL='BULKLOTCUS' and TDKEY1=:wk_IHBLTOx;
           ENDIF;                                                            // IHORTY<>'V'
           IF IHORTY = 'V'  or
             (wk_TDTABL = 'BULKLOTCUS'  and  wk_TDKEY1 = wk_IHBLTOx);
             SELECT;
             WHEN %subst(wk_IMPCLS:1:2) = 'BB';                              // Bulk bullets
               wk_LotEntry = '07';
             WHEN %subst(wk_IMPCLS:1:1) = 'C';                               // Bulk cases
               wk_LotEntry = '08';
             OTHER;
               wk_LotEntry = '99';
             ENDSL;
           ENDIF;                                                            // IHORTY='V' or
         ENDIF;                                                              // wk_TDTABL='ITEMGRP'
       ENDIF;                                                                // wk_LotEntry='00' and

       IF wk_IMLOT = 'N';
         wk_MixLotCtns = *Blank;
       ELSE;
         IF wk_CMMXLC = *Blank;
           wk_MixLotCtns = 'Y';
         ELSE;
           wk_MixLotCtns = wk_CMMXLC;
         ENDIF;                                                              // wk_CMMXLC=*Blank
       ENDIF;                                                                // wk_IMLOT='N'

       IF wk_CMPRCN = 'Y'  or
           (wk_CMPRCN = 'A'  and  %subst(wk_IMPCLS:1:1) = 'A')  or
           (wk_CMPRCN = 'B'  and  %subst(wk_IMPCLS:1:1) = 'B')  or
           (wk_CMPRCN = 'C'  and  %subst(wk_IMPCLS:1:1) = 'C')  or
           (wk_CMPRCN = 'I'  and  wk_BDUCC14I <> *Blanks);
         wk_PrtlCtns = 'Y';
       ELSE;
         wk_PrtlCtns = 'N';
       ENDIF;                                                                // wk_CMPRCN='Y' or

       wk_QtyOpen = wk_ODQORD - wk_ODQSTD - wk_ODQSTC;
       IF wk_QtyOpen < *Zero;
         wk_QtyOpen = *Zero;
       ENDIF;                                                                // wk_QtyOpen<*Zero

       IF wk_IMPLTQ@U <> *Zero  and  wk_QtyOpen < wk_IMPLTQ@U;
         wk_PctPlltQty = wk_QtyOpen / wk_IMPLTQ@U;
       ELSE;
         wk_PctPlltQty = 1.00;
       ENDIF;                                                                // wk_IMPLTQ@U<>*Zero

       // Allow pallet-pick (by adjusting the pallet quantity) if -
       //  - the customer will take a pallet pick
       //  - the item was ordered in at least a pallet quantity
       //  - the order line was partially-shipped prior
       //  - 50% or more of the order quantity is still open
       //  - the open quantity is less than a normal pallet quantity
       kf_PPTURN = IDTURN;
       kf_PPSEQ# = IDSEQ#;
       CHAIN key_PP HYPBXPP;
       IF %found(HYPBXPP);
        IF PPQOPK = -1;
         wk_AlwPlltLbls = 'N';
         wk_PlltQty = wk_IMPLTQ;
         wk_PlltQty@U = wk_IMPLTQ@U;
        ELSE;
         wk_PlltQty = PPQOPK / wk_CaseQty;
         wk_PlltQty@U = wk_PlltQty * wk_CaseQty;
        ENDIF;                                                               // PPQOPK=-1
       ELSE;
       IF wk_AlwPlltLbls = 'Y'  and  wk_IMPLTQ@U > *Zero  and
           wk_ODQORD >= wk_IMPLTQ@U  and
           (wk_ODQSTD > 0  or  wk_ODQSTC >0)  and
           wk_PctPlltQty >= .50  and
           wk_QtyOpen < wk_IMPLTQ@U;
         wk_PlltQty = wk_QtyOpen / wk_CaseQty;
         wk_PlltQty@U = wk_QtyOpen;
       ELSE;
         wk_PlltQty = wk_IMPLTQ;
         wk_PlltQty@U = wk_IMPLTQ@U;
       ENDIF;                                                                // wk_AlwPlltLbls='Y'
       ENDIF;                                                                // %found(HYPBXPP)

       //
       // Retrieve shipping-carton item number.
       //
       wk_CtnItem# = *Blanks;
       // EXEC SQL select a.PSCPN into :wk_CtnItem#
       //          from HDMPSM a join HDIMST b on a.PSCPN=b.IMITEM
       //          where a.PSPPN=:wk_ODITEM and b.IMIMDS like '%SHIP CTN%';
       EXEC SQL select a.PSCPN into :wk_CtnItem#
                from HDMPSM a
                where a.PSPPN=:wk_ODITEM and exists (select * from
                 GUPTDAT b where b.TDTABL='SHIPCTN' and b.TDKEY1=a.PSCPN);
       IF wk_CtnItem# = *Blanks;
         wk_CtnItem# = 'CTN-' + wk_ODITEM;
       ENDIF;                                                                // wk_CtnItem#=*Blanks

      **************************************************************************
      * If location work records haven't been created yet, create them.        *
      **************************************************************************
      /Free
       EXEC SQL select count(*) into :wk_W1Cnt
                from HYW0610A
                where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_ODITEM and
                      W1WHS=:wk_ODWH;
       IF wk_W1Cnt = *Zero;
         EXEC SQL insert into HYW0610A
                   select :wk_ShipGrp#, ISITEM, ISWHS, ' ', ISRATE,
                          ISSTKR, ISAILE, ISSLOC, ISLOT, ISQOH-ISQHSR, 0,
                          0, 0
                   from HDITSL a
                   where ISITEM=:wk_ODITEM and ISWHS=:wk_ODWH and
                        ISQOH>0 and ISQOH>ISQHSR and
                         not exists (select * from GUPTDAT b where
                          b.TDTABL='LOCUNAVAL' and
                          (b.TDKEY1=digits(a.ISWHS) or b.TDKEY1='*ALL') and
                          (b.TDKEY2=a.ISSTKR or b.TDKEY2='*ALL') and
                          (b.TDKEY3=a.ISAILE or b.TDKEY3='*ALL') and
                          (b.TDKEY4=a.ISAILE or b.TDKEY4='*ALL') and
                          b.TDFF01='Y')
                   order by ISITEM, ISWHS, ISRATE, ISSTKR, ISAILE,
                            ISSLOC, ISLOT;
         EXEC SQL update HYW0610A
                  set W1LTYP =
                  case
                   when W1STKR = '060'
                          and  W1AILE <> 'WEST'
                          and  W1AILE <> 'ALDA'
                     then 'P'                                                // Pallet location
                   when W1STKR='006' and W1AILE=' ' and W1SLOC=' '
                     then 'X'                                                // 006 no location
                   when W1STKR='006' and W1AILE='ALDA'
                     then 'X'                                                // 006 ALDA
                   when W1STKR='006'
                     then 'C'                                                // Case location
                   when W1STKR='006' and substr(W1SLOC,1,2) not in ('29','30')
                     then 'C'                                                // Case location
                   when W1STKR='006' and substr(W1SLOC,1,2) in ('29','30')
                     then 'E'                                                // Each/box location
                   else 'X'
                   end
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_ODITEM and
                        W1WHS=:wk_ODWH;
         EXEC SQL update HYW0610A a
                  set W1QPIK1 =
                   (select coalesce(sum(W1QPIK2-W1QSHP),0)
                    from HYW0610A b where b.W1ITEM=a.W1ITEM and
                    b.W1WHS=a.W1WHS and b.W1STKR=a.W1STKR and
                    b.W1AILE=a.W1AILE and b.W1SLOC=a.W1SLOC and
                    b.W1LOT=a.W1LOT and b.W1GRP#<>a.W1GRP# and
                    b.W1QPIK2>a.W1QSHP and
                    exists (select * from HYPSGHD c where
                    c.GHGRP#=b.W1GRP# and c.GHSTS='O'))
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_ODITEM and
                        W1WHS=:wk_ODWH;
       ENDIF;                                                                // wk_W1Cnt=*Zero
      /End-Free

      **************************************************************************
      * Calculate full-case, full-pallet, and partial-unit pick quanities.     *
      **************************************************************************
      **** Following line same as: wk_FullCases = %div(IDQOPK:IWPCPC)
     C                   EVAL      wk_FullCases = %div(wk_StkUOMQty:wk_CaseQty)
     C****               IF        wk_IMPLTQ = *Zero  or  wk_CMPLLB <> 'Y'
     C****               IF        wk_IMPLTQ = *Zero  or  wk_AlwPlltLbls <> 'Y'
     C                   IF        wk_PlltQty = *Zero  or  wk_AlwPlltLbls <> 'Y'
     C                   EVAL      wk_FullPlts = *Zero
     C                   ELSE
     C****               EVAL      wk_FullPlts = %div(wk_FullCases:wk_IMPLTQ)
     C                   EVAL      wk_FullPlts = %div(wk_FullCases:wk_PlltQty)
     C****               ENDIF                                                  wk_IMPLTQ=*Zero  or
     C                   ENDIF                                                  wk_PlltQty=*Zero  or
     C                   IF        wk_BDQTYBAR > *Zero  and
     C                               wk_BDUCCB14 <> *Blanks  and
     C                               wk_StkUOMQty >= wk_BDQTYBAR  and
     C                               %subst(wk_IMPCLS:1:2) <> 'BB'
     C                   IF        wk_FullPlts = *Zero
     C                   EVAL      wk_StkUOMQty2 = wk_StkUOMQty
     C                   ELSE
     C                   EVAL      wk_StkUOMQty2 = wk_StkUOMQty -               Qty w/o full pallets
     C                               (wk_FullPlts * wk_PlltQty * wk_CaseQty)
     C                   ENDIF                                                  wk_FullPlts=*Zero
     C                   IF        wk_StkUOMQty2 >= wk_BDQTYBAR
     C                   EVAL      wk_MstrPacks =
     C                               %div(wk_StkUOMQty2:wk_BDQTYBAR)
     C                   ELSE
     C                   EVAL      wk_MstrPacks = *Zero
     C                   ENDIF                                                  wk_StkUOMQty2>=wk_BD
     C                   ELSE
     C                   EVAL      wk_MstrPacks = *Zero
     C                   ENDIF                                                  wk_BDQTYBAR>*Zero
     C                   EVAL      wk_PrtlUnits = %rem(wk_StkUOMQty:wk_CaseQty)

      **************************************************************************
      * Process any pallet quantities.                                         *
      **************************************************************************
     C                   IF        wk_FullPlts > *Zero
     C****               EVAL      wk_PltUnits = wk_IMPLTQ * wk_CaseQty
     C                   EVAL      wk_PltUnits = wk_PlltQty * wk_CaseQty
     C                   EVAL      wk_PickMode = 'P'
     C                   FOR       wk_PltCnt = 1 to wk_FullPlts
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:wk_PickMode:wk_PltUnits:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   EVAL      wk_PickQty = wk_PltUnits
     C****               EVAL      wk_PickUnitQty = wk_IMPLTQ
     C                   EVAL      wk_PickUnitQty = wk_PlltQty
     C                   EVAL      wk_PickUOM = 'CT'
     C                   EVAL      wk_ItemBC = wk_BDUCC14
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   IF        not wk_Bulk
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_PickQty
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_PickQty
     C                   ENDIF                                                  not wk_Bulk
     C****               EVAL      wk_FullCases = wk_FullCases - wk_IMPLTQ
     C                   EVAL      wk_FullCases = wk_FullCases - wk_PlltQty
     C                   EVAL      wk_CompPlt = 'Y'
     C                   IF        wk_LotCtrl = 'Y'
     C                   EVAL      wk_PltLotCnt = RtvPltLotCnt (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:wk_PltUnits)
     C                   ELSE
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   ENDIF                                                  wk_LotCtrl='Y'

     C                   EXSR      $WrtSGDT
     C                   ENDFOR                                                 wk_PltCnt=1 to
     C                   ENDIF                                                  wk_FullPlts>*Zero

      **************************************************************************
      * Process any master-pack quantities.                                    *
      **************************************************************************
     C                   IF        wk_MstrPacks > *Zero
     C                   EVAL      wk_MPUnits = wk_BDQTYBAR
     C                   IF        wk_IMCNVY = 'Y'
     C                               or  wk_IMCNVY = 'M'
     C                   EVAL      wk_PickMode = 'C'
     C                   ELSE
     C                   EVAL      wk_PickMode = 'T'
     C                   ENDIF                                                  wk_IMCNVY='Y'
     C                   FOR       wk_MPCnt = 1 to wk_MstrPacks
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:wk_PickMode:wk_MPUnits:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   IF        wk_PickLoc = '*TRY060'
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:'P':wk_MPUnits:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   ENDIF                                                  wk_PickLoc='*TRY060'
     C                   IF        wk_PickLoc = '*EACHLOC'
     C                   EVAL      wk_PrtlUnits = wk_PrtlUnits + wk_MPUnits
     C                   EVAL      wk_EachTaken = wk_EachTaken + wk_MPUnits
     C                   EVAL      wk_FullCases = wk_FullCases -
     C                                (wk_MPUnits / wk_CaseQty)
     C                   ITER
     C                   ENDIF                                                  wk_PickLoc='*EACHLOC
     C                   EVAL      wk_PickQty = wk_MPUnits
     C                   EVAL      wk_PickUnitQty = 1
     C                   EVAL      wk_PickUOM = 'MP'
     C                   EVAL      wk_ItemBC = wk_BDUCCB14
     C                   EXSR      $RtvSSCCSrl#
     C                   EVAL      wk_SSCC = wk_Serial#
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(wk_Serial#:'X')
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt
     C                   IF        not wk_Bulk
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_PickQty
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_PickQty
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_FullCases = wk_FullCases -
     C                                (wk_MPUnits / wk_CaseQty)
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero

     C                   EXSR      $WrtSGDT
     C                   ENDFOR                                                 wk_MPCnt=1 to
     C                   ENDIF                                                  wk_MstrPacks>*Zero

      **************************************************************************
      * Process any full-case quantities.                                      *
      **************************************************************************
     C****               IF        wk_FullCases > *Zero  and  wk_CaseQty > 1
     C                   IF        wk_FullCases > *Zero  and
     C                               (wk_CaseQty > 1  or  wk_IMCNVY = 'Y')
     C                   IF        wk_IMCNVY = 'Y'
     C                   EVAL      wk_PickMode = 'C'
     C                   ELSE
     C                   EVAL      wk_PickMode = 'T'
     C                   ENDIF                                                  wk_IMCNVY='Y'
     C                   EVAL      wk_EachTaken = *Zero
     C                   FOR       wk_CaseCnt = 1 to wk_FullCases
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:'C':wk_CaseQty:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   IF        wk_PickLoc = '*TRY060'
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:'P':wk_CaseQty:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   ENDIF                                                  wk_PickLoc='*TRY060'
     C                   IF        wk_PickLoc = '*EACHLOC'
     C                   EVAL      wk_PrtlUnits = wk_PrtlUnits + wk_CaseQty
     C                   EVAL      wk_EachTaken = wk_EachTaken + wk_CaseQty
     C                   ITER
     C                   ENDIF                                                  wk_PickLoc='*EACHLOC
     C                   IF        wk_PickLoc <> '*MULT'
     C                   EVAL      wk_PickQty = wk_CaseQty
     C                   EVAL      wk_PickUnitQty = 1
     C                   EVAL      wk_PickUOM = 'CT'
     C                   EVAL      wk_ItemBC = wk_BDUCC14
     C                   IF        wk_PickMode = 'C'
     C                   EXSR      $RtvSSCCSrl#
     C                   EVAL      wk_SSCC = wk_Serial#
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(wk_Serial#:'X')
     C****               EVAL      wk_ChkDgt = RtvM10ChkDgt(wk_SSCCBC)
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt
     C                   ELSE
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   ENDIF                                                  wk_PickMode='C'
     C                   IF        not wk_Bulk
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_CaseQty
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_CaseQty
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   ELSE
     C                   EVAL      sv_PickMode = wk_PickMode
     C                   IF        not %open(HYW0610B)
     C                   OPEN      HYW0610B
     C                   ENDIF                                                  not %open(HYW0610B)
     C                   EVAL      kf_GRP# = wk_ShipGrp#
     C                   EVAL      kf_ITEM = wk_ODITEM
     C                   EVAL      kf_WHS = wk_ODWH
     C     key_W2        SETLL     HYW0610B
     C                   DOU       %eof(HYW0610B)
     C     key_W2        READE     HYW0610B
     C                   IF        %eof(HYW0610B)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0610B)
     C                   EVAL      wk_PickLoc = W2PLOC
     C                   EVAL      wk_PickMode = 'T'
     C                   EVAL      wk_PickQty = W2QPIK
     C                   EVAL      wk_PickUnitQty = W2QPIK
     C                   EVAL      wk_PickUOM = wk_IMUOMS
     C                   EVAL      wk_ItemBC = wk_BDUPC12
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   IF        not wk_Bulk
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * W2QPIK
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * W2QPIK
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   DELETE    HYW0610BR
     C                   ENDDO                                                  %eof(HYW0610B)
     C                   EVAL      wk_PickMode = sv_PickMode
     C                   ENDIF                                                  wk_PickLoc<>'*MULT'
     C                   ENDFOR                                                 wk_CaseCnt=1 to
     C                   ENDIF                                                  wk_FullCases>*Zero

      **************************************************************************
      * Process any each or box quantities.                                    *
      **************************************************************************
     C                   IF        wk_CaseQty > 1  and
     C****                           wk_PrtlUnits > wk_CaseQty
     C                               wk_PrtlUnits >= wk_CaseQty
     C                   EVAL      wk_ToteCases = %div(wk_PrtlUnits:wk_CaseQty)
     C                   EVAL      wk_PrtlUnits = %rem(wk_PrtlUnits:wk_CaseQty)
     C                   FOR       wk_CaseCnt = 1 to wk_ToteCases
     C****               EVAL      wk_PickMode = 'T'
     C                   IF        wk_IMCNVY = 'Y'
     C                   EVAL      wk_PickMode = 'C'
     C                   ELSE
     C                   EVAL      wk_PickMode = 'T'
     C                   ENDIF                                                  wk_IMCNVY='Y'
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:'E':wk_CaseQty:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   IF        wk_PickLoc = '*TRY060'
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:'P':wk_CaseQty:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   ENDIF                                                  wk_PickLoc='*TRY060'
     C                   EVAL      wk_PickQty = wk_CaseQty
     C****               EVAL      wk_PickUnitQty = wk_CaseQty
     C****               EVAL      wk_PickUOM = wk_IMUOMS
     C****               EVAL      wk_ItemBC = wk_BDUPC12
     C****               EVAL      wk_SSCCBC = *Blanks
     C****               EVAL      wk_SSCC = *Zero
     C                   EVAL      wk_PickUnitQty = 1
     C                   EVAL      wk_PickUOM = 'CT'
     C                   EVAL      wk_ItemBC = wk_BDUCC14
     C                   IF        wk_PickMode = 'C'
     C                   EXSR      $RtvSSCCSrl#
     C                   EVAL      wk_SSCC = wk_Serial#
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(wk_Serial#:'X')
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt
     C                   ELSE
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   ENDIF                                                  wk_PickMode='C'
     C                   IF        not wk_Bulk
     C                   IF        wk_IMIMWG * wk_PrtlUnits < 10000
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_CaseQty
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = 9999
     C                   ENDIF                                                  wk_IMIMWG * wk_PrtlU
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_CaseQty
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   ENDFOR                                                 wk_CaseCnt=1 to
     C                   ENDIF                                                  wk_CaseQty>1 and

     C                   IF        wk_PrtlUnits > *Zero  or
     C****                           (wk_CaseQty = 1  and wk_FullCases > *Zero)
     C                               (wk_CaseQty = 1  and wk_FullCases > *Zero
     C                                and wk_IMCNVY <> 'Y')
     C                   IF        wk_PrtlUnits = *Zero
     C****               EVAL      wk_PrtlUnits = 1
     C                   EVAL      wk_PrtlUnits = wk_FullCases
     C                   ENDIF                                                  wk_PrtlUnits=*Zero
     C                   IF        wk_IMCNVY = 'Y'  and  wk_CaseQty = 1
     C                   EVAL      wk_PickMode = 'C'
     C                   ELSE
     C                   EVAL      wk_PickMode = 'T'
     C                   ENDIF                                                  wk_IMCNVY='Y' and
     C                   IF        wk_IMOVSZ <> *Blank                          Create 1 pick line
     C                   EVAL      wk_EachQty1 = wk_PrtlUnits                    per each item
     C                   EVAL      wk_PrtlUnits = 1
     C                   EVAL      wk_EachQty2 = *Zero
     C                   ENDIF                                                  wk_IMOVSZ<>*Blank
     C                   EVAL      wk_Done1 = *OFF
     C                   DOU       wk_Done1 = *ON
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:'E':wk_PrtlUnits:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   IF        wk_PickLoc = '*TRY060'
     C                   EVAL      wk_PickLoc = RtvPickLoc (wk_ShipGrp#:
     C                               wk_ODITEM:wk_ODWH:'P':wk_PrtlUnits:
     C                               wk_CaseQty:wk_MixLotCtns:wk_PrtlCtns:
     C                               wk_IMPCLS)
     C                   ENDIF                                                  wk_PickLoc='*TRY060'
     C                   EVAL      wk_PickQty = wk_PrtlUnits
     C****               EVAL      wk_PickUnitQty = 1
     C                   EVAL      wk_PickUnitQty = wk_PrtlUnits
     C                   EVAL      wk_PickUOM = wk_IMUOMS
     C                   EVAL      wk_ItemBC = wk_BDUPC12
     C                   IF        wk_PickMode = 'C'
     C                   IF        wk_BDUCC14 <> *Blanks  and                   Fix Bulk Frontier
     C                               wk_BDUCC14 <> wk_ODITEM                     to use carton
     C                   EVAL      wk_ItemBC = wk_BDUCC14                        barcode
     C                   ENDIF                                                  wk_BDUCC14=*Blanks
     C                   EXSR      $RtvSSCCSrl#
     C                   EVAL      wk_SSCC = wk_Serial#
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(wk_Serial#:'X')
     C****               EVAL      wk_ChkDgt = RtvM10ChkDgt(wk_SSCCBC)
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt
     C                   ELSE
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   ENDIF                                                  wk_PickMode='C'
     C                   IF        not wk_Bulk
     C                   IF        wk_IMIMWG * wk_PrtlUnits < 10000
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_PrtlUnits
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = 9999
     C                   ENDIF                                                  wk_IMIMWG * wk_PrtlU
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_PrtlUnits
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   IF        wk_IMOVSZ = *Blank
     C                   EVAL      wk_Done1 = *ON
     C                   LEAVE
     C                   ELSE
     C                   EVAL      wk_EachQty2 = wk_EachQty2 + 1
     C                   IF        wk_EachQty2 >= wk_EachQty1
     C                   EVAL      wk_Done1 = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_EachQty2>=wk_Each
     C                   ENDIF                                                  wk_IMOVSZ=*Blank
     C                   ENDDO                                                  wk_Done1=*ON
     C                   ENDIF                                                  wk_PrtlUnits>*Zero
      **************************************************************************

     C                   EVAL       wk_RC0185 = DltPickDtl(IHORD#:IDORL#:IDBLN#)

     C                   ENDDO                                                  %eof(OEORDP)

      *
      * Update HYPSGDT record sequence to provide uniqueness for multiple
      * pallet-picks for the same item for the same customer.
      *

     C                   IF        not %open(HYPSGDTU)
     C                   OPEN      HYPSGDTU
     C                   ENDIF                                                   not %open(HYPSGDTU)

     C                   EVAL      sv_TURN = -1
     C                   EVAL      sv_TSEQ = -1
     C     wk_ShipGrp#   SETLL     HYPSGDTU
     C                   DOU       %eof(HYPSGDTU)
     C     wk_ShipGrp#   READE     HYPSGDTU
     C                   IF        %eof(HYPSGDTU)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGDTU)
     C                   IF        GDTURN <> sv_TURN  or
     C                                GDTSEQ <> sv_TSEQ
     C                   EVAL      wk_Seq# = *Zero
     C                   EVAL      sv_TURN = GDTURN
     C                   EVAL      sv_TSEQ = GDTSEQ
     C                   ENDIF                                                  GDTURN<>sv_TURN
     C                   EVAL      wk_Seq# = wk_Seq# + 1
     C                   EVAL      GDRSEQ = wk_Seq#
     C                   UPDATE    HYRSGDTU
     C                   ENDDO                                                  %eof(HYPSGDTU)

      *
      * Update Shipment Group Customer Summary (HYPSGCU) record.
      *
      /Free
       IF wk_FrtTerms <> *Blanks  and
           wk_FrtTerms <> wk_GCFTRM  and
           wk_GCFTRM <> '**';
          wk_FrtTerms = *Blanks;
       ENDIF;                                                                // wk_FrtTerms<>*Blanks
       wk_FrtAmt = wk_GCFRTA + wk_OETFRT;
       wk_SpclChg = wk_GCCHGA + wk_OETMSG;
       EXEC SQL update HYPSGCU a
                set GCFTRM=:wk_FrtTerms, GCFRTA=:wk_FrtAmt,
                    GCCHGA=:wk_SpclChg,
                    GCITCT=(select count(distinct GDITEM) from HYPSGDT b
                     where b.GDGRP#=a.GCGRP# and b.GDBLTO=a.GCBLTO and
                     b.GDSHTO=a.GCSHTO and b.GDDSHP=a.GCDSHP)
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:IHBLTO and
                 GCSHTO=:IHSHTO and GCDSHP=:IHDSHP;
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_Error

     C/EJECT
      **************************************************************************
      * Write HYPSGDT record.                                                  *
      **************************************************************************
     CSR   $WrtSGDT      BegSR

     C                   IF        wk_GCPANS = 'Y'  and  wk_PickMode = 'C'
     C                   EVAL      wk_GDPANS = 'Y'
     C                   ELSE
     C                   EVAL      wk_GDPANS = *Blank
     C                   ENDIF                                                  wk_GCPANS='Y' and

      /Free
       EXEC SQL insert into HYPSGDT
                       (GDGRP#, GDBLTO, GDSHTO, GDDSHP,
                        GDNAME, GDTURN, GDTSEQ, GDRSEQ,
                        GDOCTL, GDORD#, GDORL#, GDBLN#,
                        GDITEM, GDIMDS, GDQOPN, GDQOPKO,
                        GDQOPK, GDQSTC, GDLBL1,
                        GDLBL2, GDLBL3, GDBRCD, GDSSCCBC,
                        GDSSCC, GDLOT, GDPUWT, GDPUQT,
                        GDPUOM, GDPLOC, GDPLTQ, GDPLBL,
                        GDCNVY, GDPSHP, GDPLCT, GDSHPV,
                        GDSVDS, GDLLOC, GDMXPO, GDMXLT,
                        GDCPTY, GDPO#, GDCTN#, GDTOTE,
                        GDDVRT, GDSPPK, GDPIKO, GDPOPP,
                        GDPANS)
                values(:wk_ShipGrp#, :IHBLTO, :IHSHTO, :IHDSHP,
                       :wk_STName, :wk_Turn#, :IDSEQ#, 0,
                       :wk_OrdCtl#, :IHORD#, :IDORL#, :IDBLN#,
                       :wk_ODITEM,  :wk_ODIMDS, :wk_QtyOpen, :wk_PickQty,
                       :wk_PickQty, 0, 'N',
                       'N', 'N', :wk_ItemBC, :wk_SSCCBC,
                       :wk_SSCC, :wk_LotEntry, :wk_PickUnitWgt,
                         :wk_PickUnitQty,
                       :wk_PickUOM, :wk_PickLoc, :wk_PlltQty, :wk_AlwPlltLbls,
                       :wk_PickMode, :wk_CompPlt, :wk_PltLotCnt, :wk_GCSHPV,
                       :wk_GCSVDS, :wk_LblLoc, :wk_CMMXPO, :wk_MixLotCtns,
                       :wk_GCCPTY, :IHORRF, :wk_CtnItem#, ' ',
                       0, ' ', 0, 0,
                       :wk_GDPANS);
      /End-Free

     CSR                 EndSR

     C/EJECT
      **************************************************************************
      * Retrieve next GS1-128 SSCC serial number.                              *
      **************************************************************************
     CSR   $RtvSSCCSrl#  BEGSR

     C     *LOCK         IN        wk_SSCCSrlRef
     C                   EVAL      wk_SSCCSrlRef = wk_SSCCSrlRef + 1
     C                   OUT       wk_SSCCSrlRef
     C                   EVAL      wk_Serial# = wk_SSCCSrlRef

     CSR                 ENDSR

     P CrtShipRcds     E

     P/EJECT
      **************************************************************************
      * Retrieve picking location (with available quantity).                   *
      **************************************************************************
     P RtvPickLoc      B
     D                 PI            17A
     D ShipGrp#                       9S 0 CONST
     D Item#                         15A   CONST
     D Whs                            3S 0 CONST
     D PickType                       1A   CONST
      *** P = Pallet
      *** C = Case
      *** E = Each/box
     D PickQty                        9P 0 CONST                                Stocking UOM qty
     D CaseQty                        9P 0 CONST                                Each/boxes per case
     D MixLotCtns                     1A   CONST
     D PrtlCtns                       1A   CONST
     D ProdCls                        4A   CONST

     D wk_ShipGrp#     S              9S 0
     D wk_Item#        S             15A
     D wk_Whs          S              3S 0
     D wk_PickType     S              1A
     D wk_PickQty      S              9P 0
     D wk_CaseQty      S              9P 0
     D wk_MixLotCtns   S              1A
     D wk_PrtlCtns     S              1A
     D wk_ProdCls      S              4A
     D wk_PickLoc      S             17A
     D wk_PlltQtyAval  S             13P 4
     D wk_MinAval      S             13P 4
     D wk_QtyAval      S             13P 4
     D wk_SkipQty      S             13P 4
     D wk_PickLeft     S              9P 0
     D wk_LocPickQty   S              9P 0
     D wk_Done         S              1N
     D wk_TotQty       S             13P 4
     D wk_TotQtyC      S             13P 4
     D wk_CurrLot      S             15A
     D wk_LogData      S             64A
     D wk_LogMSG       S             64A
     D wk_DSITEM       S             15A
     D wk_DSSTKR       S              3A
     D wk_DSAILE       S              4A
     D wk_DSSLOC       S              8A
     D wk_Pgnm         S             10A
     D wk_User         S             10A
     D wk_Job          S             10A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Whs = Whs
     C                   EVAL      wk_PickType = PickType
     C                   EVAL      wk_PickQty = PickQty
     C                   EVAL      wk_CaseQty = CaseQty
     C                   EVAL      wk_MixLotCtns = MixLotCtns
     C                   EVAL      wk_PrtlCtns = PrtlCtns
     C                   EVAL      wk_ProdCls = ProdCls
     C                   EVAL      wk_PickLoc = *Blank
     C                   EVAL      wk_Done = *OFF

      *
      * Always send picker to A-29-A-10 for ammo seconds.
      *
     C****               IF        %subst(wk_ProdCls:1:2) = 'AS'
     C****               EVAL      wk_PickLoc = 'A-29-A-10'
     C****               LEAVE
     C****               ENDIF                                                  %subst(wk_ProdCls:1:

      *
      * For pallet picks, in case less than defined pallet quantity, find
      * minimum inventory pallet quantity available that is at least pick
      * quantity.  This will force a pallet pick to "allocate" a pallet
      * with the least quantity that still satisfies the pick quantity.
      *
      /Free
       IF wk_PickType = 'P';
         wk_MinAval = *Zero;
         EXEC SQL select coalesce(min(W1QOH-W1QPIK1),0)
                   into :wk_MinAval
                  from HYW0610A
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                        W1WHS=:wk_Whs and W1LTYP='P' and
                        W1QOH-W1QPIK1>=:wk_PickQty;
         IF wk_MinAval = *Zero;
           wk_MinAval = *Hival;
         ENDIF;                                                              // wk_MinAval=*Zero
       ENDIF;                                                                // wk_PickType='P'
      /End-Free

     C                   IF        not %open(HYW0610A)
     C                   OPEN      HYW0610A
     C                   ENDIF                                                  not %open(HYW0610A)

     C                   EVAL      kf_GRP# = wk_ShipGrp#
     C                   EVAL      kf_ITEM = wk_Item#
     C                   EVAL      kf_WHS = wk_Whs
     C                   EVAL      kf_LTYP = wk_PickType

      *
      * Find single pick location with enough inventory.
      *
     C     key_W1        SETLL     HYW0610A
     C                   DOU       %eof(HYW0610A)
     C     key_W1        READE     HYW0610A
     C                   IF        %eof(HYW0610A)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0610A)
     C                   EVAL      wk_QtyAval = W1QOH - W1QPIK1
     C                   IF        wk_QtyAval <= *Zero  or
     C                               wk_QtyAval < wk_PickQty
     C                   ITER
     C                   ENDIF                                                  wk_QtyAval<=*Zero
     C                   IF        wk_PickType = 'P'  and
     C                               wk_QtyAval > wk_MinAval
     C                   ITER
     C                   ENDIF                                                  wk_PickType='P' and
     C                   EVAL      wk_PickLoc = %trimr(W1AILE) + '-' + W1SLOC
      /Free
       EXEC SQL update HYW0610A
                set W1QPIK1=W1QPIK1+:wk_PickQty,
                    W1QPIK2=W1QPIK2+:wk_PickQty
                where W1GRP#=:W1GRP# and W1ITEM=:W1ITEM and W1WHS=:W1WHS and
                      W1LTYP=:W1LTYP and W1RATE=:W1RATE and W1STKR=:W1STKR and
                      W1AILE=:W1AILE and W1SLOC=:W1SLOC and W1LOT=:W1LOT;
       IF wk_PickType = 'P';
         EXEC SQL update HDITSL set ISQRES=case
                   when ISQRES+:wk_PickQty>ISQOH then ISQOH
                   else ISQRES+:wk_PickQty end
                  where ISITEM=:W1ITEM and ISWHS=:W1WHS and ISSTKR=:W1STKR
                   and ISAILE=:W1AILE and ISSLOC=:W1SLOC and ISLOT=:W1LOT;
       ENDIF;                                                                // wk_PickType='P'
      /End-Free
     C                   EVAL      wk_Done = *ON
     C                   LEAVE
     C                   ENDDO                                                  %eof(HYW0610A)

     C                   IF        wk_Done
     C                   LEAVE
     C                   ENDIF                                                  wk_Done

     C                   EVAL      wk_PickLeft = wk_PickQty

      *
      * Find multiple pick locations with enough inventory in total.  For
      * case-picks, only pick full-case-quantities.
      *

     C                   IF        wk_PickType = 'E'
     C                   EVAL      wk_TotQty = *Zero
     C                   EVAL      wk_TotQtyC = *Zero
     C                   IF        wk_CaseQty > 1
      /Free
       EXEC SQL select coalesce(sum(W1QOH-W1QPIK1),0) into :wk_TotQty
                from HYW0610A
                where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                 W1WHS=:wk_Whs and W1LTYP=:wk_PickType and W1QOH>W1QPIK1;
       IF wk_TotQty < wk_PickQty;
         EXEC SQL select coalesce(sum(W1QOH-W1QPIK1),0) into :wk_TotQtyC
                  from HYW0610A
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                   W1WHS=:wk_Whs and W1LTYP='C' and W1QOH>W1QPIK1;
       ENDIF;                                                                // wk_TotQty<wk_PickQty
      /End-Free
     C                   ENDIF                                                  wk_CaseQty>1
     C                   ENDIF                                                  wk_PickType='E'

     C                   IF        wk_PickType <> 'E'  or                       Case or pallet picks
     C                              (wk_PickType = 'E'  and                     Each picks - if not
     C                               wk_TotQty >= wk_PickQty)  or                enough qty at
     C                              (wk_PickType = 'E'  and                      "each" location,
     C                               wk_TotQty < wk_PickQty  and                 and a case is
     C                               wk_TotQtyC > *Zero)  or                     available, skip
     C                              (wk_PickType = 'E'  and                      this section.
     C                               wk_CaseQty <= 1)
     C     key_W1        SETLL     HYW0610A
     C                   DOU       %eof(HYW0610A)
     C     key_W1        READE     HYW0610A
     C                   IF        %eof(HYW0610A)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0610A)
     C                   EVAL      wk_QtyAval = W1QOH - W1QPIK1
     C                   IF        wk_QtyAval <= *Zero  or
     C                              (wk_PickType = 'C'  and
     C                               wk_QtyAval < wk_CaseQty)
     C                   ITER
     C                   ENDIF                                                  wk_QtyAval<=*Zero
     C                   IF        wk_PickType = 'C'
     C                   EVAL      wk_SkipQty =
     C                               %rem(%dec(wk_QtyAval:9:0):wk_CaseQty)
     C                   IF        wk_SkipQty = wk_QtyAval
     C                   ITER
     C                   ENDIF                                                  wk_SkipQty=wk_QtyAva
     C                   EVAL      wk_QtyAval = wk_QtyAval - wk_SkipQty
     C                   ENDIF                                                  wk_PickType='C'
     C                   EVAL      wk_PickLoc = %trimr(W1AILE) + '-' + W1SLOC
     C                   IF        wk_PickLeft <= wk_QtyAval
     C                   EVAL      wk_LocPickQty = wk_PickLeft
     C                   EVAL      wk_PickLeft = *Zero
     C                   ELSE
     C                   EVAL      wk_LocPickQty = wk_QtyAval
     C                   EVAL      wk_PickLeft = wk_PickLeft - wk_QtyAval
     C                   ENDIF                                                  wk_PickLeft<=wk_QtyA
      /Free
       EXEC SQL update HYW0610A
                set W1QPIK1=W1QPIK1+:wk_LocPickQty,
                    W1QPIK2=W1QPIK2+:wk_LocPickQty
                where W1GRP#=:W1GRP# and W1ITEM=:W1ITEM and W1WHS=:W1WHS and
                      W1LTYP=:W1LTYP and W1RATE=:W1RATE and W1STKR=:W1STKR and
                      W1AILE=:W1AILE and W1SLOC=:W1SLOC and W1LOT=:W1LOT;
       IF wk_PickType = 'P';
         EXEC SQL update HDITSL set ISQRES=case
                   when ISQRES+:wk_LocPickQty>ISQOH then ISQOH
                   else ISQRES+:wk_LocPickQty end
                  where ISITEM=:W1ITEM and ISWHS=:W1WHS and ISSTKR=:W1STKR
                   and ISAILE=:W1AILE and ISSLOC=:W1SLOC and ISLOT=:W1LOT;
       ENDIF;                                                                // wk_PickType='P'
      /End-Free
     C                   IF        wk_PickLeft = *Zero
     C                   EVAL      wk_Done = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_PickLeft=*Zero
     C                   ENDDO                                                  %eof(HYW0610A)

     C                   IF        wk_Done
     C                   LEAVE
     C                   ENDIF                                                  wk_Done

     C                   ENDIF                                                  wk_PickType<>'E' or

      *
      * Find multiple case-pick locations with enough inventory in total,
      * allowing partial (and mixed-lot, if applicable) cases to fulfill
      * the pick request.
      *
     C                   IF        wk_PickType = 'C'

      /Free
       wk_QtyAval = *Zero;
       EXEC SQL select coalesce(sum(W1QOH-W1QPIK1),0) into :wk_QtyAval
                from HYW0610A
                where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                      W1WHS=:wk_Whs and W1LTYP='C';
      /End-Free

     C                   IF        wk_QtyAval >= wk_PickQty

     C                   EVAL      wk_CurrLot = *Blanks

     C     key_W1        SETLL     HYW0610A
     C                   DOU       %eof(HYW0610A)
     C     key_W1        READE     HYW0610A
     C                   IF        %eof(HYW0610A)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0610A)

     C                   EVAL      wk_QtyAval = W1QOH - W1QPIK1
     C                   IF        wk_QtyAval <= *Zero
     C                   ITER
     C                   ENDIF                                                  wk_QtyAval<=*Zero

     C                   IF        wk_CurrLot <> *Blanks  and
     C                               W1LOT <> wk_CurrLot
     C                   ITER
     C                   ENDIF                                                  wk_CurrLot<>*Blanks

      /Free
       IF W1LOT <> *Blanks  and  wk_MixLotCtns <> 'Y'
           and  wk_CurrLot = *Blanks;
         wk_QtyAval = *Zero;
         EXEC SQL select sum(W1QOH-W1QPIK1) into :wk_QtyAval
                  from HYW0610A
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                        W1WHS=:wk_Whs and W1LTYP='C' and W1LOT=:W1LOT;
         IF wk_QtyAval < wk_PickQty;
           ITER;
         ENDIF;                                                              // wk_QtyAval<wk_PickQt
         EVAL wk_CurrLot = W1LOT;
       ENDIF;                                                                // W1LOT<>*Blanks and
      /End-Free

     C                   EVAL      wk_PickLoc = %trimr(W1AILE) + '-' + W1SLOC
     C                   IF        wk_PickLeft <= wk_QtyAval
     C                   EVAL      wk_LocPickQty = wk_PickLeft
     C                   EVAL      wk_PickLeft = *Zero
     C                   ELSE
     C                   EVAL      wk_LocPickQty = wk_QtyAval
     C                   EVAL      wk_PickLeft = wk_PickLeft - wk_QtyAval
     C                   ENDIF                                                  wk_PickLeft<=wk_QtyA
      /Free
       EXEC SQL insert into HYW0610B
                       (W2GRP#, W2ITEM, W2WHS, W2PLOC,
                        W2QPIK)
                values(:wk_ShipGrp#, :W1ITEM, :W1WHS, :wk_PickLoc,
                       :wk_LocPickQty);
       EXEC SQL update HYW0610A
                set W1QPIK1=W1QPIK1+:wk_LocPickQty,
                    W1QPIK2=W1QPIK2+:wk_LocPickQty
                where W1GRP#=:W1GRP# and W1ITEM=:W1ITEM and W1WHS=:W1WHS and
                      W1LTYP=:W1LTYP and W1RATE=:W1RATE and W1STKR=:W1STKR and
                      W1AILE=:W1AILE and W1SLOC=:W1SLOC and W1LOT=:W1LOT;
      /End-Free
     C                   EVAL      wk_PickLoc = '*MULT'

     C                   IF        wk_PickLeft = *Zero
     C                   EVAL      wk_Done = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_PickLeft=*Zero

     C                   ENDDO                                                  %eof(HYW0610A)

     C                   IF        wk_Done
     C                   LEAVE
     C                   ENDIF                                                  wk_Done

     C                   ENDIF                                                  wk_QtyAval>=wk_PickQ

      *
      * If pick locations not found for all quantities (wk_PickLeft>*Zero),
      * check pallet quantities available for replenishment and each-pick
      * locations.
      *
      /Free
       wk_PlltQtyAval = *Zero;
       EXEC SQL select coalesce(sum(W1QOH-W1QPIK1),0) into :wk_PlltQtyAval
                from HYW0610A
                where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                      W1WHS=:wk_Whs and W1LTYP='P';
       wk_QtyAval = *Zero;
       IF wk_MixLotCtns = *Blank or  wk_MixLotCtns = 'Y';
         EXEC SQL select coalesce(sum(W1QOH-W1QPIK1),0) into :wk_QtyAval
                  from HYW0610A
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                        W1WHS=:wk_Whs and W1LTYP='E';
       ELSE;
         EXEC SQL select  coalesce(sum(dec((W1QOH-W1QPIK1)/:wk_CaseQty,9,0)
                   *:wk_CaseQty),0) into :wk_QtyAval
                  from HYW0610A
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                        W1WHS=:wk_Whs and W1LTYP='E';
       ENDIF;                                                                // wk_MixedLotCtns=*Bla
      /End-Free

     C                   IF        wk_PlltQtyAval = *Zero
     C                   IF        wk_QtyAval - wk_EachTaken >= wk_PickLeft
     C                   EVAL      wk_PickLoc = '*EACHLOC'
     C                   EVAL      wk_Done = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_QtyAval>=wk_PickL
     C                   ENDIF                                                  wk_PlltQtyAval=*Zero

     C                   ENDIF                                                  wk_PickType='C'

      *
      * Find each-pick case-locations with enough inventory.  This is needed
      * if there is insufficient quantity for an each-pick in an each-location,
      * to send the picker to break open a full case in the case-location.
      *
     C*****              IF        wk_PickType = 'E'  and
     C*****                          wk_CaseQty > 1
     C                   IF        wk_PickType = 'E'

     C                   EVAL      kf_LTYP = 'C'
     C     key_W1        SETLL     HYW0610A
     C                   DOU       %eof(HYW0610A)
     C     key_W1        READE     HYW0610A
     C                   IF        %eof(HYW0610A)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0610A)
     C                   EVAL      wk_QtyAval = W1QOH - W1QPIK1
     C                   IF        wk_QtyAval <= *Zero
     C                   ITER
     C                   ENDIF                                                  wk_QtyAval<=*Zero
     C                   EVAL      wk_PickLoc = %trimr(W1AILE) + '-' + W1SLOC
     C                   IF        wk_PickLeft <= wk_QtyAval
     C                   EVAL      wk_LocPickQty = wk_PickLeft
     C                   EVAL      wk_PickLeft = *Zero
     C                   ELSE
     C                   EVAL      wk_LocPickQty = wk_QtyAval
     C                   EVAL      wk_PickLeft = wk_PickLeft - wk_QtyAval
     C                   ENDIF                                                  wk_PickLeft<=wk_QtyA
      /Free
       EXEC SQL update HYW0610A
                set W1QPIK1=W1QPIK1+:wk_LocPickQty,
                    W1QPIK2=W1QPIK2+:wk_LocPickQty
                where W1GRP#=:W1GRP# and W1ITEM=:W1ITEM and W1WHS=:W1WHS and
                      W1LTYP=:W1LTYP and W1RATE=:W1RATE and W1STKR=:W1STKR and
                      W1AILE=:W1AILE and W1SLOC=:W1SLOC and W1LOT=:W1LOT;
      /End-Free
     C                   IF        wk_PickLeft = *Zero
     C                   EVAL      wk_Done = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_PickLeft=*Zero
     C                   ENDDO                                                  %eof(HYW0610A)

     C                   IF        wk_Done
     C                   LEAVE
     C                   ENDIF                                                  wk_Done

     C                   ENDIF                                                  wk_PickType='E'

      *
      * If SMS determined quantities were available for this pick, but this
      * subprocedure can't determine the location, use default stock location
      * for case-level and box/each picks, or default '060' for pallet picks.
      * Note, this might happen if inventory changes after an SMS rebuild, or
      * if inventory is in the DC (whs 6), but not within the confines of the
      * new DC.  For example, if bullets are OR'd into 6-333, they could be
      * consumed by SMS, but this program would not know of the inventory
      * (and in HYW0610A, the location would be marked as location type "X").
      *
     C                   IF        wk_PickLoc = *Blanks
     C                   IF        wk_PickType = 'P'
     C                   EVAL      wk_PickLoc = '060'
     C                   ELSE
      /Free
       wk_DSITEM = *Blanks;
       wk_DSSTKR = *Blanks;
       wk_DSAILE = *Blanks;
       wk_DSSLOC = *Blanks;
       EXEC SQL select DSITEM, DSSTKR, DSAILE, DSSLOC
                 into :wk_DSITEM, :wk_DSSTKR, :wk_DSAILE, :wk_DSSLOC
                from HDDSTL
                where DSAPID='OE' and DSWHS=:wk_Whs and
                      DSITEM=:wk_Item# and DSOTYP=' ';
      /End-Free
     C                   IF        wk_DSITEM = wk_Item#  and
     C                              (wk_DSAILE <> *Blanks  or
     C                               wk_DSSLOC <> *Blanks)
     C                                                    and
     C                               %subst(wk_DSAILE:2:3) = ' '
     C                                                    and
     C                              (wk_DSSTKR = '006'  or
     C                               wk_DSSTKR = '060')
     C                   EVAL      wk_PickLoc =
     C                               %trimr(wk_DSAILE) + '-' + %trimr(wk_DSSLOC)
     C                   ELSE
      *
      * If item does not have a pick location, but there is stock in
      * stockroom 060, set pick location to *TRY060, and leave this
      * subprocedure.  *TRY060 will cause the calling routine to
      * call this subprodedure a 2nd time to check for 060 locations.
      *
      /Free
       wk_QtyAval = *Zero;
       EXEC SQL select coalesce(sum(W1QOH-W1QPIK1),0)
                 into :wk_QtyAval
                from HYW0610A
                where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                 W1WHS=:wk_Whs and W1STKR='060';
      /End-Free
     C                   IF        wk_QtyAval >= wk_PickQty
     C                   EVAL      wk_PickLoc = '*TRY060'
     C                   LEAVE
     C                   ENDIF                                                  wk_QtyAval>=wk_PickQ
     C                   EVAL      wk_PickLoc = '006'
     C                   ENDIF                                                  wk_DSITEM=wk_Item#
     C                   ENDIF                                                  wk_PickType='P'

      *
      * Location '006' will sabotage the group.  Use a real location that will
      * probably never have any inventory (because it's a column location).
      *
     C                   IF        wk_PickLoc = '006'
     C                               or  wk_PickLoc = '060'
     C                               or  wk_PickLoc = 'I06'
     C                               or  wk_PickLoc = 'B20'
     C                               or  wk_PickLoc = 'TRUC'
     C                               or  wk_PickLoc = 'RDW'
     C                               or  %subst(wk_PickLoc:1:4) = 'I06-'
     C                               or  %subst(wk_PickLoc:1:4) = 'B20-'
     C                               or  %subst(wk_PickLoc:1:4) = 'TRUC'
     C                               or  %subst(wk_PickLoc:1:4) = 'RDW-'
     C                               or  %subst(wk_PickLoc:1:4) = 'Z000'
     C                   IF        wk_PickType = 'P'
     C****               EVAL      wk_PickLoc = 'C-35-B-02'
     C                   EVAL      wk_PickLoc = 'C-35-Z-02'
     C                   ELSE
     C                   EVAL      wk_PickLoc = 'J-35-A-02'
     C                   ENDIF                                                  wk_PickType='P'
     C                   ENDIF                                                  wk_PickLoc='006'

      /Free
       wk_LogData = 'Grp: ' + %triml(%editc(wk_ShipGrp#:'Z')) +
                    '; Item: ' + %trimr(wk_Item#) +
                    '; Whs: ' + %triml(%editc(wk_Whs:'Z')) +
                    '; Type: ' + wk_PickType +
                    '; Loc: ' + %trimr(wk_PickLoc);
       wk_LogMsg = 'Dft loc ' + %trimr(wk_PickLoc) +
                   ' used for item ' + %trimr(wk_Item#) +
                   ' qty ' + %triml(%editc(wk_PickQty:'J'));
       wk_Pgnm = ps_Pgnm;
       wk_User = ps_User;
       wk_Job = ps_Job;
       EXEC SQL insert into hypmilg
                       (mllgty, mldata, mlmsg, mlpgm,
                        mluser, mlws, mltstp)
                values('BX02', :wk_LogData, :wk_LogMsg, :wk_Pgnm,
                       :wk_User, :wk_Job, current timestamp);
      /End-Free
     C                   ENDIF                                                  wk_PickLoc=*Blanks

     C                   ENDDO

      *
      * Locations without anything more than the stockroom will sabotage
      * the group.  Use a real location that will probably never have any
      * inventory (because it's a column location).
      *
     C                   IF        wk_PickLoc = '-'
     C                   IF        wk_PickType = 'P'
     C****               EVAL      wk_PickLoc = 'C-35-B-02'
     C                   EVAL      wk_PickLoc = 'C-35-Z-02'
     C                   ELSE
     C                   EVAL      wk_PickLoc = 'J-35-A-02'
     C                   ENDIF                                                  wk_PickType='P'
     C                   ENDIF                                                  wk_PickLoc='-'

      *
      * Attempt to ensure pick location is a valid BoxLogix location.
      * Currently, all valid BoxLogix locations are in format a-bb-l-xx
      * (where a=aisle, b=bay, l-level, and x=location), e.g., A-34-B-01.
      * Or, alternatively, they are in format aa-bb-l-xx
      * (where a=aisle, b=bay, l-level, and x=location), e.g., AA-31-A-01.
      * If the pick location is not in that format, use "pole" location.
      *
     C                   IF        wk_PickLoc <> '*TRY060'  and
     C                               wk_PickLoc <> '*EACHLOC'  and
     C                               wk_PickLoc <> '*MULT'
     C                   IF        (%len(%trimr(wk_Pickloc)) = 9  and           Single-letter aisle
     C                             ((%subst(wk_PickLoc:1:1) < 'A'  or
     C                               %subst(wk_PickLoc:1:1) > 'V')  or
     C                               %subst(wk_PickLoc:2:1) <> '-'  or
     C                               %subst(wk_PickLoc:3:2) = *Blanks  or
     C                               %subst(wk_PickLoc:5:1) <> '-'  or
     C                              (%subst(wk_PickLoc:6:1) < 'A'  or
     C                               %subst(wk_PickLoc:6:1) > 'Z')  or
     C                               %subst(wk_PickLoc:7:1) <> '-'  or
     C                               %subst(wk_PickLoc:8:2) = *Blanks))  or
     C                             (%len(%trimr(wk_Pickloc)) = 10  and          Double-letter aisle
     C                             ((%subst(wk_PickLoc:1:1) < 'A'  or
     C                               %subst(wk_PickLoc:1:1) > 'V')  or
     C                               %subst(wk_Pickloc:2:1) <>
     C                                 %subst(wk_Pickloc:1:1)  or
     C                               %subst(wk_PickLoc:3:1) <> '-'  or
     C                               %subst(wk_PickLoc:4:2) = *Blanks  or
     C                               %subst(wk_PickLoc:6:1) <> '-'  or
     C                              (%subst(wk_PickLoc:7:1) < 'A'  or
     C                               %subst(wk_PickLoc:7:1) > 'Z')  or
     C                               %subst(wk_PickLoc:8:1) <> '-'  or
     C                               %subst(wk_PickLoc:9:2) = *Blanks))  or
     C                             (%len(%trimr(wk_Pickloc)) < 9  or
     C                              %len(%trimr(wk_Pickloc)) > 10)
     C                   IF        wk_PickType = 'P'
     C                   EVAL      wk_PickLoc = 'C-35-Z-02'
     C                   ELSE
     C                   EVAL      wk_PickLoc = 'J-35-A-02'
     C                   ENDIF                                                  wk_PickType='P'
     C                   ENDIF                                                  %subst(wk_PickLoc:1:
     C                   ENDIF                                                  wk_PickLoc<>'*TRY060

     C                   RETURN    wk_PickLoc

     P RtvPickLoc      E

     P/EJECT
      **************************************************************************
      * Retrieve pallet lot count.                                             *
      **************************************************************************
     P RtvPltLotCnt    B
     D                 PI             5P 0
     D ShipGrp#                       9S 0 CONST
     D Item#                         15A   CONST
     D Whs                            3S 0 CONST
     D PltUnitQty                     9P 0 CONST                                Full pallet unit qty

     D wk_ShipGrp#     S              9S 0
     D wk_Item#        S             15A
     D wk_Whs          S              3S 0
     D wk_PltUnitQty   S              9P 0
     D wk_AvgPltLotCt  S              5P 0
     D wk_AvgPltLotX   S              5P 0
     D wk_PltLotCnt    S              5P 0
     D wk_PltUnitCnt   S              7P 0
     D wk_PltCnt       S              7P 0
     D wk_PltCntX      S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Whs = Whs
     C                   EVAL      wk_PltUnitQty = PltUnitQty
     C                   EVAL      wk_AvgPltLotCt = *Zero

     C                   IF        wk_PltUnitQty = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_PltUnitQty

      /Free
       wk_PltLotCnt = *Zero;
       wk_PltUnitCnt = *Zero;
       EXEC SQL select min(lotcount) into :wk_PltLotCnt                      // Rtv count of lots on
                from (select count(distinct W1LOT) lotcount                  // all full pallets for
                  from HYW0610A                                              // current item
                  where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and
                        W1WHS=:wk_Whs and W1LTYP='P' and
                        W1QPIK1<W1QOH
                  group by W1ITEM, W1WHS) as tempfile;
       IF wk_PltLotCnt = *Zero;
         LEAVE;
       ENDIF;                                                                // wk_PltLotCnt=*Zero
       EXEC SQL select sum(W1QOH-W1QPIK1) into :wk_PltUnitCnt                // Rtv count of units
                from HYW0610A                                                // (e.g., boxes) on
                where W1GRP#=:wk_ShipGrp# and W1ITEM=:wk_Item# and           // all full pallets for
                      W1WHS=:Whs and W1LTYP='P' and                          // current item
                      W1QPIK1<W1QOH;
       IF wk_PltUnitCnt = *Zero;
         LEAVE;
       ENDIF;                                                                // wk_PltUnitCnt=*Zero
      /End-Free
     C                   EVAL      wk_PltCnt = %div(wk_PltUnitCnt:              Calc count of full
     C                               wk_PltUnitQty)                             pallets for current
     C                   EVAL      wk_PltCntX =                                 item
     C                               %rem(wk_PltUnitCnt:wk_PltUnitQty)
     C                   IF        wk_PltCntX > *Zero
     C                   EVAL      wk_PltCnt = wk_PltCnt + 1
     C                   ENDIF                                                  wk_PltCntX>*Zero
     C                   EVAL      wk_AvgPltLotCt =
     C                               %div(wk_PltLotCnt:wk_PltCnt)
     C                   EVAL      wk_AvgPltLotX =
     C                               %rem(wk_PltLotCnt:wk_PltCnt)
     C                   IF        wk_AvgPltLotX > *Zero
     C                   EVAL      wk_AvgPltLotCt = wk_AvgPltLotCt + 1
     C                   ENDIF                                                  wk_AvgPltLotX>*Zero

     C                   ENDDO

     C                   RETURN    wk_AvgPltLotCt

     P RtvPltLotCnt    E

     P/EJECT
      **************************************************************************
      * Assign consolidated shipment PRO numbers.                              *
      **************************************************************************
     P AsnCSPRO#       B                   EXPORT
     D                 PI            10I 0
     D ShipGrp#                       9S 0 CONST

     D wk_ShipGrp#     S              9S 0
     D wk_PROCnt       S             10I 0
     D wk_GHDCLOC      S              4A
     D wk_PRO#         S             30A
     D wk_SVSVSV       S              2A
     D wk_SVBOLR       S              1A
     D wk_SkipPRO      S              1N
     D wk_RcdCnt       S              5P 0
     D wk_GCBLTOx      S              7A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_PROCnt = *Zero

     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)

      /Free
       wk_GHDCLOC = *Blanks;
       EXEC SQL select GHDCLOC into :wk_GHDCLOC
                from HYPSGHD
                where GHGRP#=:wk_ShipGrp#;
      /End-Free

     C     wk_ShipGrp#   SETLL     HYPSGCU
     C                   DOU       %eof(HYPSGCU)
     C     wk_ShipGrp#   READE     HYPSGCU
     C                   IF        %eof(HYPSGCU)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGCU)
     C                   IF        GCPRO# <> *Blanks
     C                   ITER
     C                   ENDIF                                                  GCPRO#<>*Blanks
      /Free
       IF GCSHPV<>wk_SVSVSV;
         EVAL wk_SVSVSV = *Blanks;
         EVAL wk_SVBOLR = *Blanks;
         EXEC SQL select SVSVSV, SVBOLR into :wk_SVSVSV, :wk_SVBOLR
                  from HDSHPV
                  where SVSVSV=:GCSHPV;
       ENDIF;                                                                // GCSHPV<>wk_SVSVSV
      /End-Free
     C                   IF        wk_SVBOLR = 'Y'
      /Free
       wk_SkipPRO = *OFF;
       wk_GCBLTOx = %editc(GCBLTO:'X');
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='CUSTGRP' and TDKEY1='BXLSKIPASNPRO' and
                      TDKEY2='*CUST' and TDKEY3=:wk_GCBLTOx;
       IF wk_RcdCnt > *Zero;
         wk_SkipPRO = *ON;
       ELSE;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from GUPTDAT
                  where TDTABL='CUSTGRP' and TDKEY1='BXLSKIPASNPRO' and
                        TDKEY2='*STATE' and TDKEY3=:GCSTST;
         IF wk_RcdCnt > *Zero;
           wk_SkipPRO = *ON;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free
     C                   IF        not wk_SkipPRO
     C                   EVAL      wk_PRO# = RtvCSPRO#(wk_GHDCLOC:GCSHPV)
     C                   IF        wk_PRO# <> *Blanks
      /Free
       EXEC SQL update HYPSGCU set GCPRO#=:wk_PRO#
                where GCGRP#=:GCGRP# and GCBLTO=:GCBLTO and
                      GCSHTO=:GCSHTO and GCDSHP=:GCDSHP;
      /End-Free
     C                   EVAL      wk_PROCnt = wk_PROCnt + 1
     C                   ENDIF                                                  wk_PRO#<>*Blanks
     C                   ENDIF                                                  not wk_SkipPRO
     C                   ENDIF                                                  wk_SVBOLR='Y'
     C                   ENDDO                                                  %eof(HYPSGCU)

     C                   ENDDO

     C                   RETURN    wk_PROCnt

     P AsnCSPRO#       E

     P/EJECT
      **************************************************************************
      * Create .txt shipment group detail file.                                *
      **************************************************************************
     P CrtGrpTxtFile   B                   EXPORT
     D                 PI            10I 0
     D ShipGrp#                       9S 0 CONST
     D Timestamp                       Z   CONST

      /COPY QPRPSRC,HYR0620

     D wk_ShipGrp#     S              9S 0
     D wk_Timestamp    S               Z
     D wk_Error        S             10I 0
     D wk_DatabaseID@  S              2A
     D wk_BxlMXPO@     S              1A                                        Seperate POs
     D wk_GCLANE1      S              1S 0
     D wk_GCLANE2      S              1S 0
     D wk_GCLANE3      S              1S 0
     D wk_GCLANES      S              1S 0
     D wk_GCSTYP       S              1A
     D wk_GCMLLI       S              1A
     D wk_GCMLLC       S              1A
     D wk_BillTo#X     S              7A
     D wk_ShipTo#X     S              7A
     D wk_BuyGrpCust   S              1N
     D wk_PlltShpOnly  S              1N
     D wk_SkipLblShpV  S              1N
     D wk_GDBLTOx      S              7A
     D wk_RcdCnt       S              5P 0
     D wk_OneTime      S              1N
     D wk_JobLogged    S              1N
     D wk_SFName       S             30A
     D wk_SFAddr1      S             30A
     D wk_SFAddr2      S             30A
     D wk_SFAddr3      S             30A
     D wk_StoreNum     S             25A
     D wk_StorePfxPtr  S              3P 0
     D wk_CustVend     S             15A
     D**** wk_STName       S             30A
     D wk_STName       S             50A
     D wk_STAddr1      S             30A
     D wk_STAddr2      S             30A
     D wk_STAddr3      S             30A
     D wk_STCity       S             26A
     D wk_STState      S              2A
     D wk_STZip        S             13A
     D wk_STCountry    S             30A
     D wk_STPhone      S             11S 0
     D wk_PhoneCtry    S              3A
     D wk_ExportLbl    S              1N
     D wk_FmtPhone     S             15A
     D wk_FPLen        S              3P 0
     D wk_DeptNum      S             25A
     D wk_DeptName     S             30A
     D wk_CaseQty      S              9P 0
     D wk_Carton#      S              5P 0
     D wk_CtnCount     S             30A
     D wk_CustItem     S             30A
     D wk_GHDCLOC      S              4A
     D wk_SLNAME       S             30A
     D wk_SLADDR1      S             30A
     D wk_SLADDR2      S             30A
     D wk_SLCITY       S             19A
     D wk_SLST         S              2A
     D wk_SLZIP        S             10A
     D wk_SLPHON       S             11S 0
     D wk_CMCUST2      S              7S 0
     D wk_CMMXPO       S              1A
     D wk_CMSLLC       S              1A
     D wk_CMCUST       S              7S 0
     D wk_CMCNA1       S             26A
     D wk_CMCNA2       S             26A
     D wk_CMCNA3       S             26A
     D wk_CMCNA4       S             19A
     D wk_CMCCTY       S             26A
     D wk_CMST         S              2A
     D wk_CMZIP        S             13A
     D wk_CMCTRY       S              3A
     D wk_CMPHON       S             11S 0
     D wk_CMSTOR       S             20A
     D wk_DSNMBR       S              7S 0
     D wk_DSNAME       S             30A
     D wk_DSADR1       S             30A
     D wk_DSADR2       S             30A
     D wk_DSADR3       S             30A
     D wk_DSCITY       S             26A
     D wk_DSST         S              2A
     D wk_DSZIP        S             13A
     D wk_DSCTRY       S              3A
     D wk_DSPHON       S             11S 0
     D wk_CNCDES       S             30A
     D wk_GCPRO#       S             30A
     D wk_GCBOL#       S              8S 0
     D wk_TDKEY1       S             15A
     D wk_TDKEY2       S             15A
     D wk_TDFF01       S              1A
     D wk_SVSVSV       S              2A
     D wk_SVSCAC       S              4A
     D wk_IMITEM       S             15A
     D wk_IMUOMS       S              2A
     D wk_BDITEM       S             15A
     D wk_BDUPC12      S             12A
     D wk_VCVCIT       S             30A
     D wk_ETTPID       S             15A
     D wk_EDI850Cnt    S              5P 0
     D wk_REF02        S             30A   BASED(wk_REF02Ptr)
     D wk_REF03        S             80A   BASED(wk_REF03Ptr)
     D wk_ShipTo       S             15A
     D wk_Lot          S              2A
     D wk_BxlPO#       S             22A
     D wk_OpenErrors   S              3P 0
     D wk_DSPLYMsg     S             52A
     D wk_TxtFull1a    S            192A
     D wk_TxtName1a    S             64A
     D wk_TxtFull1b    S            192A
     D wk_TxtName1b    S             64A
     D wk_TxtFull1c    S            192A
     D wk_TxtName2a    S             64A
     D wk_TxtFull2a    S            192A
     D wk_TxtName2b    S             64A
     D wk_TxtFull2b    S            192A
     D wk_TxtFull2c    S            192A
     D wk_LblArchOpn   S              1N
     D wk_DumpID       S             10A
     D wk_RnmErrCnt    S              5P 0
     D wk_Tries        S              3P 0
     D wk_FirstError   S             10I 0
     D wk_Rcd          S          32766    VARYING
     D wk_Error2       S             10I 0
     D wk_ZPLStr       S          32000A
     D wk_PgmEDR9900   S              1N
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D fd1             S             10I 0                                      Shipment detail file
     D fd2             S             10I 0                                      Shipment label file
     D fd3             S             10I 0                                      Shipment detail arch
     D fd4             S             10I 0                                      Shipment label arch
     D sv_BLTO         S                   LIKE(GDBLTO) INZ(*Hival)
     D sv_SHTO         S                   LIKE(GDSHTO) INZ(*Hival)
     D sv_DSHP         S                   LIKE(GDDSHP) INZ(*Hival)

     D CustNameLen     C                   26
     D BPStoreNbrLen   C                   4
     D DSGStoreNbrLen  C                   4
     D GMStoreNbrLen   C                   4
     D CtnTextPfx      C                   'CARTON #:'
     D Digits          C                   '0123456789'

      /COPY QCPYILE,HYR0620                                                     Define PARMDS0620

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Timestamp = Timestamp
     C                   EVAL      wk_Error = *Zero

     C                   EVAL      wk_OneTime = *OFF
     C                   EVAL      wk_Carton# = *Zero
     C                   EVAL      wk_PgmEDR9900 = *OFF
     C                   EVAL      wk_JobLogged = *OFF

     C                   IF        not %open(HYPSGDT)
     C                   OPEN      HYPSGDT
     C                   ENDIF                                                  not %open(HYPSGDT)

     C                   IF        not %open(HYPWICT)
     C                   OPEN      HYPWICT
     C                   ENDIF                                                  not %open(HYPWICT)

     C                   IF        wk_DatabaseID = *Blanks
     C                   IN        wk_DatabaseID
     C                   ENDIF                                                  wk_DatabaseID=*Blank
     C                   EVAL      wk_DatabaseID@ = wk_DatabaseID               Copy to local var

     C                   IF        wk_ShpLblShpV = *Blanks
     C                   IN        wk_ShpLblShpV
     C                   IF        %subst(wk_ShpLblShpV:1:1) = 'Y'
     C                   EVAL      wk_CtnLblShpV = *ON
     C                   ELSE
     C                   EVAL      wk_CtnLblShpV = *OFF
     C                   ENDIF                                                  %subst(wk_ShpLblShpV
     C                   ENDIF                                                  wk_ShpLblShpV=*Blank

     C                   IN        wk_UCC128Phn

     C                   IN        wk_BxlMXPO
     C                   EVAL      wk_BxlMXPO@ = wk_BxlMXPO                     Copy to local var

     C                   IF        wk_BXLPath1 = *Blanks
     C                   IN        wk_BxlPath1
     C                   ENDIF                                                  wk_BXLPath1=*Blanks
     C                   IF        wk_BXLPath2 = *Blanks
     C                   IN        wk_BxlPath2
     C                   ENDIF                                                  wk_BXLPath2=*Blanks
     C                   IF        wk_BXLPath4 = *Blanks
     C                   IN        wk_BxlPath4
     C                   ENDIF                                                  wk_BXLPath4=*Blanks
     C                   IF        wk_BXLPath5 = *Blanks
     C                   IN        wk_BxlPath5
     C                   ENDIF                                                  wk_BXLPath5=*Blanks

     C                   EVAL      kf_PGM = ps_Pgnm
     C                   EVAL      kf_CTYP = '*DFT'
     C     key_WC        CHAIN     HYPWICT
     C                   IF        not %found(HYPWICT)
     C                   EVAL      WCWAIT = 10
     C                   EVAL      WCTMAX = *Zero                               Default no retries
     C                   ENDIF                                                  not %found(HYPWICT)

     C                   EXSR      $OpenTxtFile
     C                   IF        fd1 < *Zero
     C                   LEAVE
     C                   ENDIF                                                  fd1<*Zero

     C     wk_ShipGrp#   SETLL     HYPSGDT
     C                   DOU       %eof(HYPSGDT)
     C     wk_ShipGrp#   READE     HYPSGDT
     C                   IF        %eof(HYPSGDT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGDT)
     C                   IF        GDBLTO <> sv_BLTO  or
     C                               GDSHTO <> sv_SHTO
      /Free
       wk_ShipTo#X = %editc(GDSHTO:'X');
       wk_BillTo#X = %editc(GDBLTO:'X');
       wk_TDKEY1 = *Blanks;
       wk_TDFF01 = *Blank;
       EXEC SQL select TDKEY1, TDFF01 into :wk_TDKEY1, :wk_TDFF01
                from GUPTDAT
                where TDTABL='BXLCUST' and  TDKEY1=:wk_ShipTo#X and
                 TDKEY2='S';
       IF wk_TDKEY1 <> wk_ShipTo#X;
         wk_TDKEY1 = *Blanks;
         wk_TDFF01 = *Blank;
         EXEC SQL select TDKEY1, TDFF01 into :wk_TDKEY1, :wk_TDFF01
                  from GUPTDAT
                  where TDTABL='BXLCUST' and  TDKEY1=:wk_BillTo#X and
                   TDKEY2='B';
       ENDIF;                                                                // wk_TDKEY1<>wk_ShipTo
       IF (wk_TDKEY1 = wk_BillTo#X  or  wk_TDKEY1 = wk_ShipTo#X)  and
            wk_TDFF01 = 'Y';
         wk_PlltShpOnly = *ON;
       ELSE;
         wk_PlltShpOnly = *OFF;
       ENDIF;                                                                // wk_TDKEY1=wk_BillTo#
      /End-Free
     C                   EVAL      sv_BLTO = GDBLTO
     C                   ENDIF                                                  GDBLTO<>sv_BLTO
     C                   IF        GDSHTO <> sv_SHTO  or
     C                               GDDSHP <> sv_DSHP
      /Free
       EVAL wk_GCLANE1 = *Zero;
       EVAL wk_GCLANE2 = *Zero;
       EVAL wk_GCLANE3 = *Zero;
       EVAL wk_GCLANES = *Zero;
       EVAL wk_GCSTYP = *Blank;
       EVAL wk_GCMLLI = *Blank;
       EVAL wk_GCMLLC = *Blank;
       EXEC SQL select GCLANE1, GCLANE2, GCLANE3, GCLANES,
                       GCSTYP, GCMLLI, GCMLLC
                into :wk_GCLANE1, :wk_GCLANE2, :wk_GCLANE3, :wk_GCLANES,
                     :wk_GCSTYP, :wk_GCMLLI, :wk_GCMLLC
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:GDBLTO and
                 GCSHTO=:GDSHTO and GCDSHP=:GDDSHP;

       wk_BillTo#X = %editc(GDBLTO:'X');
       wk_TDKEY2 = *Blanks;
       EXEC SQL select TDKEY2 into :wk_TDKEY2
                from GUPTDAT
                where TDTABL='BUYINGGRPS' and TDKEY1=:wk_DatabaseID@ and
                 TDKEY2=:wk_BillTo#X;
       IF wk_TDKEY2 = wk_BillTo#X;
         wk_BuyGrpCust = *ON;
       ELSE;
         wk_BuyGrpCust = *OFF;
       ENDIF;                                                                // wk_TDKEY2=wk_BillTo#
      /End-Free
     C                   EVAL      sv_SHTO = GDSHTO
     C                   EVAL      sv_DSHP = GDDSHP
     C                   ENDIF                                                  GDSHTO<>sv_SHTO or
     C                   IF        GDDSHP = *Zero
     C                   EVAL      wk_ShipTo = %triml(%editc(GDSHTO:'Z'))
     C                   ELSE
     C                   EVAL      wk_ShipTo = %triml(%editc(GDSHTO:'Z')) +
     C                               '-' + %triml(%editc(GDDSHP:'Z'))
     C                   ENDIF                                                  GDSHTO=*Zero
     C                   IF        %subst(GDLOT:1:1) = '0'
     C                   EVAL      wk_Lot = %subst(GDLOT:2:1)
     C                   ELSE
     C                   EVAL      wk_Lot = GDLOT
     C                   ENDIF                                                  %subst(GDLOT:1:1)=
      /Free
       wk_CMMXPO = *Blank;
       IF wk_BuyGrpCust;
         EXEC SQL select CMMXPO
                   into :wk_CMMXPO
                  from HDCUST2
                  where CMCUST=:GDSHTO;
       ELSE;
         EXEC SQL select CMMXPO
                   into :wk_CMMXPO
                  from HDCUST2
                  where CMCUST=:GDBLTO;
       ENDIF;                                                                // wk_BuyGrpcust
      /End-Free
     C                   IF        wk_BxlMXPO@ = 'Y'  and  wk_CMMXPO = 'N'
     C                   EVAL      wk_BxlPO# = GDPO#
     C                   ELSE
     C                   EVAL      wk_BxlPO# = *Blanks
     C                   ENDIF                                                  wk_BxlMXPO@='Y' and
     C                   EXSR      $WrtTxtDtl
     C                   IF        GDCNVY = 'C'
     C                               or  (GDCNVY = 'S'  and  GDSPPK = 'C')
     C                   EXSR      $CrtLblTxt
     C                   ENDIF                                                  GDCNVY='C'
     C                   ENDDO                                                  %eof(HYPSGDT)

      *
      * Close .txt shipment group detail file.
      *
     C                   CALLP(E)  close(fd1)

     C                   IF        wk_BXLPath4 <> '*NONE'
     C                   CALLP(E)  close(fd3)
     C                   ENDIF                                                  wk_BxlPath4<>'*NONE'

      *
      * Rename .txt shipment group detail file from .tmp suffix to .txt.
      *
     C                   CALLP(E)  rename(%trimr(wk_TxtFull1a):
     C                                    %trimr(wk_TxtFull1b))

      *
      * Shutdown EDI subprocedures service program if used.
      *
     C                   IF        wk_PgmEDR9900                                EDR9900 called
     C                   CALLP(E)  sd_EDR9900
     C                   EVAL      wk_PgmEDR9900 = *OFF
     C                   ENDIF                                                  wk_PgmEDR9900

     C                   ENDDO

     C                   RETURN    wk_Error

     C/EJECT
      **************************************************************************
      * Open .txt shipment group detail file.                                  *
      **************************************************************************
     CSR   $OpenTxtFile  BEGSR

     C                   EVAL      wk_TxtName1a = 'GRP' +
     C                               %editc(wk_ShipGrp#:'X') + ' ' +
     C                               %char(wk_Timestamp) + '.tmp'

     C                   EVAL      wk_TxtFull1a = %trimr(wk_BXLPath1) +
     C                               wk_TxtName1a

     C                   EVAL      wk_TxtName1b = 'GRP' +
     C                               %editc(wk_ShipGrp#:'X') + ' ' +
     C                               %char(wk_Timestamp) + '.txt'

     C                   EVAL      wk_TxtFull1b = %trimr(wk_BXLPath1) +
     C                               wk_TxtName1b

      *
      * Open stream file.
      *
     C                   EVAL      wk_Tries = *Zero
     C                   EVAL      wk_FirstError = *Zero
     C                   DOU       fd1 >= *Zero  or  wk_Tries > WCTMAX
     C                   EVAL      wk_Tries = wk_Tries + 1
     C                   EVAL      fd1 = open(%trimr(wk_TxtFull1a):
     C                               O_CREAT+O_EXCL+O_CCSID+O_WRONLY+
     C                               O_TEXTDATA+O_TEXT_CREAT:
     C                               M_RWX : 819 : 0)
     C                   IF        fd1 < *Zero  and  wk_Tries = 1
     C                   EVAL      wk_FirstError = fd1
     C                   ENDIF                                                  fd1<*Zero and
     C                   IF        fd1 < *Zero  and  wk_Tries <= WCTMAX  and
     C                               WCWAIT > *Zero
     C                   CALLP(E)  sleep(WCWAIT)
     C                   ENDIF                                                  fd1<*Zero and
     C                   ENDDO                                                  fd1>=*Zero or
     C                   IF        fd1 < *Zero  or  wk_Tries > 1
     C                   EXSR      $WrtWILG1
     C                   EVAL      wk_DSPLYMsg = 'Error1 open GRP.txt file +
     C                               for ' + %char(wk_ShipGrp#) +'.'
     C     wk_DSPLYMsg   DSPLY     '*EXT'
     C     wk_DSPLYMsg   DSPLY     'BEARL'
     C     wk_DSPLYMsg   DSPLY     'JELLIS'
     C     wk_DSPLYMsg   DSPLY     'JSCOTT'
     C     wk_DSPLYMsg   DSPLY     'DSIMONS'
     C                   ENDIF                                                  fd1<*Zero or
     C                   IF        fd1 < *Zero
     C                   EVAL      wk_Error = 101
     C                   EVAL      wk_DumpID = 'OPNGRP-1'
     C                   DUMP(A)
     C                   CALLP     EscErrno(errno)
     C                   ENDIF                                                  fd1<*Zero

      *
      * Open stream file - archive.
      *
     C                   IF        wk_BXLPath4 <> '*NONE'
     C                   EVAL      wk_TxtFull1c = %trimr(wk_BXLPath4) +
     C                               wk_TxtName1b
     C                   EVAL      wk_Tries = *Zero
     C                   EVAL      wk_FirstError = *Zero
     C                   DOU       fd3 >= *Zero  or  wk_Tries > WCTMAX
     C                   EVAL      wk_Tries = wk_Tries + 1
     C                   EVAL      fd3 = open(%trimr(wk_TxtFull1c):
     C                               O_CREAT+O_EXCL+O_CCSID+O_WRONLY+
     C                               O_TEXTDATA+O_TEXT_CREAT:
     C                               M_RWX : 819 : 0)
     C                   IF        fd3 < *Zero  and  wk_Tries = 1
     C                   EVAL      wk_FirstError = fd3
     C                   ENDIF                                                  fd3<*Zero and
     C                   IF        fd3 < *Zero  and  wk_Tries <= WCTMAX  and
     C                               WCWAIT > *Zero
     C                   CALLP(E)  sleep(WCWAIT)
     C                   ENDIF                                                  fd3<*Zero and
     C                   ENDDO                                                  fd3>=*Zero or
     C                   IF        fd3 < *Zero  or  wk_Tries > 1
     C                   EXSR      $WrtWILG3
     C                   EVAL      wk_DSPLYMsg = 'Error3 open GRP.txt file +
     C                               for ' + %char(wk_ShipGrp#) +'.'
     C     wk_DSPLYMsg   DSPLY     '*EXT'
     C     wk_DSPLYMsg   DSPLY     'BEARL'
     C     wk_DSPLYMsg   DSPLY     'JELLIS'
     C     wk_DSPLYMsg   DSPLY     'JSCOTT'
     C     wk_DSPLYMsg   DSPLY     'DSIMONS'
     C                   ENDIF                                                  fd3<*Zero or
     C                   IF        fd3 < *Zero
     C                   EVAL      wk_Error = 101
     C                   EVAL      wk_DumpID = 'OPNGRPA-1'
     C                   DUMP(A)
     C                   CALLP     EscErrno(errno)
     C                   ENDIF                                                  fd3<*Zero
     C                   ENDIF                                                  wk_BxlPath4<>'*NONE'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write .txt shipment group detail record.                               *
      **************************************************************************
     CSR   $WrtTxtDtl    BegSR

     C                   EVAL      wk_Rcd =
     C                               %editc(wk_ShipGrp#:'X') + '|' +
     C                               %trimr(wk_ShipTo) +'|' +
     C                               %trimr(GDNAME) + '|' +
     C                               %triml(%editc(wk_GCLANE1:'Z')) + '|' +
     C                               %triml(%editc(wk_GCLANE2:'Z')) + '|' +
     C                               %triml(%editc(wk_GCLANE3:'Z')) + '|' +
     C                               %triml(%editc(wk_GCLANES:'Z')) + '|' +
     C                               GDMXPO + '|' +
     C                               GDMXLT + '|' +
     C                               %triml(%editc(GDCPTY:'Z')) +'|' +
     C                               %triml(%editc(GDORD#:'Z')) +'|' +
     C                               %trimr(GDPO#) + '|' +
     C                               %triml(%editc(GDTURN:'Z')) +'|' +
     C                               %trimr(GDITEM) + '|' +
     C                               %trimr(PreAsciiCvt('T':GDIMDS)) + '|' +
     C                               %trimr(GDBRCD) + '|' +                     UPC or GTIN-14
     C                               %trimr(GDSSCCBC) + '|' +
     C                               %trimr(wk_Lot) + '|' +
     C                               %trim(%editc(GDPUWT:'L')) +'|' +
     C                               %trim(%editc(GDPUQT:'L')) +'|' +
     C                               %trimr(GDPUOM) + '|' +
     C                               %trimr(GDPLOC) + '|' +
     C                               %trim(%editc(GDPLTQ:'L')) +'|' +
     C                               GDPLBL  + '|' +
     C                               GDCNVY + '|' +
     C                               GDPSHP + '|' +
     C                               %triml(%editc(GDPLCT:'4')) + '|' +
     C                               wk_GCSTYP + '|' +                          P=Parcel, T=Truck
     C                               %trimr(GDSHPV) + '|' +
     C                               %trimr(GDSVDS) + '|' +
     C                               GDLLOC + '|' +
     C                               wk_GCMLLI + '|' +
     C                               wk_GCMLLC + '|' +
     C                               %trimr(GDCTN#) + '|' +
     C                               %trimr(wk_BxlPO#) + '|' +
     C                               %triml(%editc(GDPIKO:'Z')) +'|' +
     C                               %triml(%editc(GDPOPP:'Z')) +'|' +
     C                               %trimr(GDPANS) + '|' +
     C                               %trimr(GDSPPK)
     C                   EVAL      wk_Rcd = %trimr(wk_Rcd) + x'400D25'

     C                   EVAL      wk_Error2 = write(fd1:%addr(wk_Rcd)+2:
     C                               %len(%trimr(wk_Rcd)))

     C                   IF        wk_BXLPath4 <> '*NONE'
     C                   EVAL      wk_Error2 = write(fd3:%addr(wk_Rcd)+2:
     C                               %len(%trimr(wk_Rcd)))
     C                   ENDIF                                                  wk_BxlPath4<>'*NONE'

     CSR                 EndSR

     C/EJECT
      **************************************************************************
      * Create .txt shipment group detail label file.                          *
      **************************************************************************
     CSR   $CrtLblTxt    BEGSR

     C                   EVAL      wk_TxtName2a = 'LBL' +
     C                               %editc(wk_ShipGrp#:'X') + ' ' +
     C                               %trimr(GDSSCCBC) + ' ' +
     C                               %char(wk_Timestamp) + '.tmp'

     C                   EVAL      wk_TxtFull2a = %trimr(wk_BXLPath2) +
     C                               wk_TxtName2a

     C                   EVAL      wk_TxtName2b = 'LBL' +
     C                               %editc(wk_ShipGrp#:'X') + ' ' +
     C                               %trimr(GDSSCCBC) + ' ' +
     C                               %char(wk_Timestamp) + '.txt'

     C                   EVAL      wk_TxtFull2b = %trimr(wk_BXLPath2) +
     C                               wk_TxtName2b

      *
      * Open stream file.
      *
     C                   EVAL      wk_OpenErrors = *Zero
     C                   DOU       fd2 >= *Zero
     C                   EVAL      fd2 = open(%trimr(wk_TxtFull2a):
     C                               O_CREAT+O_EXCL+O_CCSID+O_WRONLY+
     C                               O_TEXTDATA+O_TEXT_CREAT:
     C                               M_RWX : 819 : 0)
     C                   IF        fd2 < *Zero
     C                   EVAL      wk_OpenErrors = wk_OpenErrors + 1
     C                   IF        wk_OpenErrors < 5
     C                   EVAL      wk_DSPLYMsg = 'Error1 open LBL.txt file +
     C                               for ' + GDSSCCBC +'.'
     C     wk_DSPLYMsg   DSPLY     '*EXT'
     C     wk_DSPLYMsg   DSPLY     'BEARL'
     C     wk_DSPLYMsg   DSPLY     'JELLIS'
     C                   ELSE
     C                   EVAL      wk_Error = 102
     C                   EVAL      wk_DumpID = 'OPNLBL-1'
     C                   DUMP(A)
     C****               CALLP     EscErrno(errno)
     C                   EVAL      wk_DSPLYMsg = 'Error2 open LBL.txt file +
     C                               for ' + GDSSCCBC +'.'
     C     wk_DSPLYMsg   DSPLY     '*EXT'
     C     wk_DSPLYMsg   DSPLY     'BEARL'
     C     wk_DSPLYMsg   DSPLY     'JELLIS'
     C                   LEAVESR
     C                   ENDIF                                                  wk_OpenErrors<5
     C                   ENDIF                                                  fd2<*Zero
     C                   ENDDO                                                  f2>=*Zero

      *
      * Open stream file - archive.
      *
     C                   EVAL      wk_LblArchOpn = *OFF
     C                   IF        wk_BXLPath5 <> '*NONE'
     C                   EVAL      wk_TxtFull2c = %trimr(wk_BXLPath5) +
     C                               wk_TxtName2b
     C                   EVAL      wk_OpenErrors = *Zero
     C                   DOU       fd4 >= *Zero
     C                   EVAL      fd4 = open(%trimr(wk_TxtFull2c):
     C                               O_CREAT+O_EXCL+O_CCSID+O_WRONLY+
     C                               O_TEXTDATA+O_TEXT_CREAT:
     C                               M_RWX : 819 : 0)
     C                   IF        fd4 < *Zero
     C                   EVAL      wk_OpenErrors = wk_OpenErrors + 1
     C                   IF        wk_OpenErrors < 5
     C                   EVAL      wk_DSPLYMsg = 'Error1a open LBL.txt archive +
     C                               file for ' + GDSSCCBC +'.'
     C     wk_DSPLYMsg   DSPLY     '*EXT'
     C     wk_DSPLYMsg   DSPLY     'BEARL'
     C     wk_DSPLYMsg   DSPLY     'JELLIS'
     C                   ELSE
     C                   EVAL      wk_Error = 102
     C                   EVAL      wk_DumpID = 'OPNLBLA-1'
     C                   DUMP(A)
     C                   EVAL      wk_DSPLYMsg = 'Error2a open LBL.txt archive +
     C                               file for ' + GDSSCCBC +'.'
     C     wk_DSPLYMsg   DSPLY     '*EXT'
     C     wk_DSPLYMsg   DSPLY     'BEARL'
     C     wk_DSPLYMsg   DSPLY     'JELLIS'
     C                   LEAVE
     C                   ENDIF                                                  wk_OpenErrors<5
     C                   ELSE
     C                   EVAL      wk_LblArchOpn = *ON
     C                   ENDIF                                                  fd4<*Zero
     C                   ENDDO                                                  fd4>=*Zero
     C                   ENDIF                                                  wk_BXLPath5<>'*NONE'

     C                   EVAL      wk_Carton# = wk_Carton# + 1

     C                   EXSR      $RtvLblData

     C                   EVAL      wk_ZPLStr = ZPLCode (PARMDS0620)
     C****               EVAL      wk_ZPLStr = GDSSCCBC

      *
      * Write .txt shipment group detail label record.
      *
     C                   EVAL      wk_Rcd =
     C                               %editc(wk_ShipGrp#:'X') + '|' +
     C                               %trimr(GDSSCCBC) + '|' +
     C                               %trimr(wk_ZPLStr)
     C                   EVAL      wk_Rcd = %trimr(wk_Rcd) + x'400D25'

     C                   EVAL      wk_Error2 = write(fd2:%addr(wk_Rcd)+2:
     C                               %len(%trimr(wk_Rcd)))

     C                   IF        wk_BXLPath5 <> '*NONE'  and  wk_LblArchOpn
     C                   EVAL      wk_Error2 = write(fd4:%addr(wk_Rcd)+2:
     C                               %len(%trimr(wk_Rcd)))
     C                   ENDIF                                                  wk_BXLPath5<>'*NONE'

      *
      * Close .txt shipment group detail label file.
      *
     C                   CALLP(E)  close(fd2)

     C                   IF        wk_BXLPath5 <> '*NONE'  and  wk_LblArchOpn
     C                   CALLP(E)  close(fd4)
     C                   ENDIF                                                  wk_BXLPath5<>'*NONE'

      *
      * Rename .txt shipment group detail label file from .tmp suffix to .txt.
      *
     C****               CALLP(E)  rename(%trimr(wk_TxtFull2a):
     C****                                %trimr(wk_TxtFull2b))
     C                   EVAL      wk_RnmErrCnt = *Zero
     C                   DOU       wk_Error2 = *Zero
     C                   EVAL      wk_Error2 = rename(%trimr(wk_TxtFull2a):
     C                                                 %trimr(wk_TxtFull2b))
     C                   IF        wk_Error2 < *Zero
     C                   EVAL      wk_RnmErrCnt = wk_RnmErrCnt + 1
     C                   IF        wk_RnmErrCnt > 5
     C                   EVAL      wk_DumpID = 'RNMLBL-1'
     C                   DUMP(A)
     C                   LEAVE
     C                   ENDIF                                                  wk_RnmErrCnt>5
     C                   CALLP(E)  sleep(2)
     C                   ENDIF                                                  wk_Error2<*Zero
     C                   ENDDO                                                  wk_Error2=*Zero

     CSR                 EndSR

     C/EJECT
      **************************************************************************
      * Retrieve label data.                                                   *
      **************************************************************************
     CSR   $RtvLblData   BEGSR

     C                   IF        not wk_OneTime

      *
      * Retrieve ship-from data.
      *
      /Free
       wk_GHDCLOC = *Blanks;
       wk_SLNAME = *Blanks;
       wk_SLADDR1 = *Blanks;
       wk_SLADDR2 = *Blanks;
       wk_SLCITY = *Blanks;
       wk_SLST = *Blanks;
       wk_SLZIP = *Blanks;
       wk_SLPHON = *Zero;
       EXEC SQL select GHDCLOC into :wk_GHDCLOC
                from HYPSGHD
                where GHGRP#=:wk_ShipGrp#;
       EXEC SQL select SLNAME, SLADDR1, SLADDR2, SLCITY,
                       SLST, SLZIP, SLPHON
                 into :wk_SLNAME, :wk_SLADDR1, :wk_SLADDR2, :wk_SLCITY,
                      :wk_SLST, :wk_SLZIP, :wk_SLPHON
                from HYPSLOC
                where SLSLOC=:wk_GHDCLOC;
      /End-Free

      *
      * Populate ship-from fields.
      *
     C                   SELECT
     C                   WHEN      wk_SLNAME <> *Blanks
     C                   EVAL      wk_SFName = wk_SLNAME
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   EVAL      wk_SFName = 'HORNADY MFG COMPANY'
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   EVAL      wk_SFName = 'SNAPSAFE'
     C                   WHEN      wk_DatabaseID = 'PV'  or
     C                               wk_DatabaseID = 'PX'
     C                   EVAL      wk_SFName = 'PLATTE VALLEY ENERGETICS'
     C                   OTHER
     C                   EVAL      wk_SFName = 'HORNADY MFG COMPANY'
     C                   ENDSL

     C                   EVAL      wk_SFAddr1 = wk_SLADDR1
     C                   EVAL      wk_SFAddr2 = %trimr(wk_SLCITY) + ', ' +
     C                               wk_SLST +  ' ' + wk_SLZIP
     C                   EVAL      wk_SFAddr3 = %triml(
     C                               %editw(wk_SLPHON:'0&(   )&   -    '))
     C                   EVAL      %subst(wk_SFAddr3:20:3) = 'USA'

     C                   EVAL      wk_OneTime = *ON

     C                   ENDIF                                                  not wk_OneTime

      *
      * Retrieve bill-to data.
      *
     C****               IF        GDBLTO <> wk_CMCUST2
     C                   IF        (wk_BuyGrpCust  and
     C                               GDSHTO <> wk_CMCUST2)  or
     C                             (not wk_BuyGrpCust  and
     C                               GDBLTO <> wk_CMCUST2)
      /Free
       wk_CMCUST2 = *Zero;
       wk_CMSLLC = *Blank;
       IF wk_BuyGrpCust;
         EXEC SQL select CMCUST, CMSLLC
                   into :wk_CMCUST2, :wk_CMSLLC
                  from HDCUST2
                  where CMCUST=:GDSHTO;
       ELSE;
       //  IF GDBLTO = 59  and  %scan('CABELA':GDNAME) > *Zero;
       //    EXEC SQL select CMCUST, CMSLLC
       //              into :wk_CMCUST2, :wk_CMSLLC
       //             from HDCUST2
       //             where CMCUST=37;
       //  ELSE;
           EXEC SQL select CMCUST, CMSLLC
                     into :wk_CMCUST2, :wk_CMSLLC
                    from HDCUST2
                    where CMCUST=:GDBLTO;
       //  ENDIF;                                                              // GDBLTO=59 and
       ENDIF;                                                                // wk_BuyGrpcust
      /End-Free
     C                   ENDIF                                                  wk_BuyGrpCust and

      *
      * Retrieve ship-to data.
      *
     C                   IF        GDSHTO <> wk_CMCUST  or
     C                               GDDSHP <> wk_DSNMBR
      /Free
       wk_CMCUST = *Zero;
       wk_CMCNA1 = *Blanks;
       wk_CMCNA2 = *Blanks;
       wk_CMCNA3 = *Blanks;
       wk_CMCNA4 = *Blanks;
       wk_CMCCTY = *Blanks;
       wk_CMST = *Blanks;
       wk_CMZIP = *Blanks;
       wk_CMCTRY = *Blanks;
       wk_CMPHON = *Zero;
       wk_CMSTOR = *Blanks;
       EXEC SQL select CMCUST, CMCNA1, CMCNA2, CMCNA3,
                       CMCNA4, CMCCTY, CMST, CMZIP,
                       CMCTRY, CMPHON, CMSTOR
                 into :wk_CMCUST, :wk_CMCNA1, :wk_CMCNA2, :wk_CMCNA3,
                      :wk_CMCNA4, :wk_CMCCTY, :wk_CMST, :wk_CMZIP,
                      :wk_CMCTRY, :wk_CMPHON, :wk_CMSTOR
                from HDCUST
                where CMCUST=:GDSHTO;
       wk_DSNMBR = *Zero;
       wk_DSNAME = *Blanks;
       wk_DSADR1 = *Blanks;
       wk_DSADR2 = *Blanks;
       wk_DSADR3 = *Blanks;
       wk_DSCITY = *Blanks;
       wk_DSST = *Blanks;
       wk_DSZIP = *Blanks;
       wk_DSCTRY = *Blanks;
       wk_DSPHON = *Zero;
       IF GDDSHP <> *Zero;
         EXEC SQL select DSNMBR, DSNAME, DSADR1, DSADR2,
                         DSADR3, DSCITY, DSST, DSZIP,
                         DSCTRY, DSPHON
                   into :wk_DSNMBR, :wk_DSNAME, :wk_DSADR1, :wk_DSADR2,
                        :wk_DSADR3, :wk_DSCITY, :wk_DSST, :wk_DSZIP,
                        :wk_DSCTRY, :wk_DSPHON
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:GDSHTO and DSNMBR=:GDDSHP;
       ENDIF;
      /End-Free

      *
      * Populate store number.
      *
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_databaseID = 'XX'
     C                   SELECT
     C                   WHEN      GDBLTO = 17                                  Sports, Inc.
     C                   IF        %check(Digits:wk_CMCNA1:23) = *Zero
     C                   EVAL      wk_StoreNum = %subst(wk_CMCNA1:23:4)
     C                   ELSE
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ENDIF                                                  %check(Digits:%subst
     C                   WHEN      GDBLTO = 52                                  Academy Stores
     C                   EVAL      wk_StoreNum = '00004'
     C                   WHEN      GDBLTO = 59                                  Bass Pro Shops
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - BPStoreNbrLen - 1
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ELSE
     C                   EVAL      wk_StoreNum = %subst(wk_CMCNA1 :
     C                               wk_StorePfxPtr + 2 : BPStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   WHEN      GDBLTO = 337                                 Dick's Sporting Good
     C                               or  GDBLTO = 536527                        DSG Field & Stream
     C                   IF        wk_CMSTOR <> *Blanks
     C                   EVAL      wk_StoreNum = wk_CMSTOR
     C                   ELSE
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - DSGStoreNbrLen - 1
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ELSE
     C                   EVAL      wk_StoreNum = %subst(wk_CMCNA1 :
     C                               wk_StorePfxPtr + 2 : DSGStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   ENDIF                                                  wk_CMSTOR<>*Blanks
     C                   WHEN      GDBLTO = 21419                               Gander Mountain
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - GMStoreNbrLen - 1
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ELSE
     C                   EVAL      wk_StoreNum = %subst(wk_CMCNA1 :
     C                               wk_StorePfxPtr + 2 : GMStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   OTHER
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_databaseID = 'SX'
     C                   SELECT
     C                   WHEN      GDBLTO = 4                                   Sports, Inc.
     C                   IF        %check(Digits:wk_CMCNA1:23) = *Zero
     C                   EVAL      wk_StoreNum = %subst(wk_CMCNA1:23:4)
     C                   ELSE
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ENDIF                                                  %check(Digits:%subst
     C                   WHEN      GDBLTO = 3035                                Academy Stores
     C                   EVAL      wk_StoreNum = wk_CMSTOR
     C                   WHEN      GDBLTO = 3069                                Bass Pro Shops
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - BPStoreNbrLen - 1
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ELSE
     C                   EVAL      wk_StoreNum = %subst(wk_CMCNA1 :
     C                               wk_StorePfxPtr + 2 : BPStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   WHEN      GDBLTO = 93                                  Dick's Sporting Good
     C                   IF        wk_CMSTOR <> *Blanks
     C                   EVAL      wk_StoreNum = wk_CMSTOR
     C                   ELSE
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_CMCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - DSGStoreNbrLen - 1
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ELSE
     C                   EVAL      wk_StoreNum = %subst(wk_CMCNA1 :
     C                               wk_StorePfxPtr + 2 : DSGStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   ENDIF                                                  wk_CMSTOR<>*Blanks
     C                   OTHER
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ENDSL
     C                   OTHER
     C                   EVAL      wk_StoreNum = *Blanks
     C                   ENDSL

      *
      * Populate customer's vendor number.
      *
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      GDBLTO = 52                                  Academy Stores
     C                   EVAL      wk_CustVend = '01211'
     C                   OTHER
     C                   EVAL      wk_CustVend = *Blanks
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   SELECT
     C                   WHEN      GDBLTO = 3035                                Academy Stores
     C                   EVAL      wk_CustVend = '47562'
     C                   OTHER
     C                   EVAL      wk_CustVend = *Blanks
     C                   ENDSL
     C                   OTHER
     C                   EVAL      wk_CustVend = *Blanks
     C                   ENDSL

      *
      * Populate ship-to fields.
      *
     C                   EVAL      wk_ExportLbl = *OFF
     C                   IF        GDDSHP = *Zero  or
     C                               (wk_CMCTRY <> 'US'  and
     C                                wk_CMCTRY <> 'CA'  and
     C                                wk_CMCTRY <> *Blanks)
     C                   EVAL      wk_STName = wk_CMCNA1
     C                   EVAL      wk_STAddr1 = wk_CMCNA2
     C                   EVAL      wk_STAddr2 = wk_CMCNA3
     C                   EVAL      wk_STAddr3 = wk_CMCNA4
     C                   EVAL      wk_STCity = wk_CMCCTY
     C                   EVAL      wk_STState = wk_CMST
     C                   EVAL      wk_STZip = wk_CMZIP
     C                   EVAL      wk_STPhone = wk_CMPHON
     C                   EVAL      wk_PhoneCtry = wk_CMCTRY
     C                   IF        wk_CMCTRY = 'US'  or  wk_CMCTRY = *Blanks
     C                   EVAL      wk_STCountry = *Blanks
     C                   ELSE
     C                   EVAL      wk_ExportLbl = *ON
      /Free
       wk_CNCDES = *Blanks;
       EXEC SQL select CNCDES into :wk_CNCDES
                from HDCTRY
                where CNCTCD=:wk_CMCTRY;
       IF wk_CNCDES = *Blanks;
         wk_CNCDES = wk_CMCTRY;
       ENDIF;                                                                // wk_CNCDES=*Blanks
      /End-Free
     C                   IF        wk_CNCDES <> wk_STAddr1  and
     C                               wk_CNCDES <> wk_STAddr2  and
     C                               wk_CNCDES <> wk_STAddr3  and
     C                               wk_CNCDES <> wk_STCity
     C                   EVAL      wk_STCountry = wk_CNCDES
     C                   ELSE
     C                   EVAL      wk_STCountry = *Blanks
     C                   ENDIF                                                  wk_CNCDES<>wk_STAddr
     C                   ENDIF                                                  wk_CMCTRY='US' or
     C                   ELSE
     C                   EVAL      wk_STName = wk_DSNAME
     C                   EVAL      wk_STAddr1 = wk_DSADR1
     C                   EVAL      wk_STAddr2 = wk_DSADR2
     C                   EVAL      wk_STAddr3 = wk_DSADR3
     C                   EVAL      wk_STCity = wk_DSCITY
     C                   EVAL      wk_STState = wk_DSST
     C                   EVAL      wk_STZip = wk_DSZIP
     C                   IF        wk_DSPHON <> *Zero
     C                   EVAL      wk_STPhone = wk_DSPHON
     C                   EVAL      wk_PhoneCtry = wk_DSCTRY
     C                   ELSE
     C                   EVAL      wk_STPhone = wk_CMPHON
     C                   EVAL      wk_PhoneCtry = wk_CMCTRY
     C                   ENDIF                                                  wk_DSPHON<>*Zero
     C                   IF        wk_DSCTRY = 'US'  or  wk_DSCTRY = *Blanks
     C                   EVAL      wk_STCountry = *Blanks
     C                   ELSE
     C                   EVAL      wk_ExportLbl = *ON
      /Free
       wk_CNCDES = *Blanks;
       EXEC SQL select CNCDES into :wk_CNCDES
                from HDCTRY
                where CNCTCD=:wk_DSCTRY;
       IF wk_CNCDES = *Blanks;
         wk_CNCDES = wk_DSCTRY;
       ENDIF;                                                                // wk_CNCDES=*Blanks
      /End-Free
     C                   IF        wk_CNCDES <> wk_STAddr1  and
     C                               wk_CNCDES <> wk_STAddr2  and
     C                               wk_CNCDES <> wk_STAddr3  and
     C                               wk_CNCDES <> wk_STCity
     C                   EVAL      wk_STCountry = wk_CNCDES
     C                   ELSE
     C                   EVAL      wk_STCountry = *Blanks
     C                   ENDIF                                                  wk_CNCDES<>wk_STAddr
     C                   ENDIF                                                  wk_DSCTRY='US' or
     C                   ENDIF                                                  GDDSHP=*Zero

      *
      * Print phone number on label when applicable (and if it fits).
      * Note: The ship-to name field allows for more than 30 characters,
      *       but this was kept to 30 characters so that the generated
      *       labels would look the same as those generated from HYR0110,
      *       HYR0622, HYR0134, and HYR0117.
      *
     C                   IF        wk_UCC128Phn = 'Y'  or
     C                              (wk_UCC128Phn = 'F'  and  wk_ExportLbl)
     C                   IF        wk_STPhone <> *Zero
     C                   EVAL      wk_FmtPhone =
     C                               FmtPhone(wk_STPhone:wk_PhoneCtry:'2')      2=E.164 format
     C                   EVAL      wk_FPLen = %len(%trimr(wk_FmtPhone))
     C                   SELECT
     C                   WHEN      %len(%trimr(wk_STName)) + wk_FPLen + 1 <= 30
     C                   EVAL      %subst(wk_STName:30-wk_FPLen + 1) =
     C                                     wk_FmtPhone
     C                   WHEN      %len(%trimr(wk_STCountry)) + wk_FPLen + 1
     C                                     <= 30
     C                   EVAL      %subst(wk_STCountry:30-wk_FPLen + 1) =
     C                                     wk_FmtPhone
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR1496'
     C                   EVAL      wk_MsgData = wk_FmtPhone +
     C                                            %subst(wk_STName:1:30) +
     C                                            wk_STCountry
     C                   EXSR      $WrtJobLog
     C                   ENDSL
     C                   ENDIF                                                  wk_STPhone<>*Zero
     C                   ENDIF                                                  wk_UCC128Phn='Y' or

      *
      * Retrieve shipment-group-customer level data.
      *
      /Free
       wk_GCPRO# = *Blanks;
       wk_GCBOL# = *Zero;
       EXEC SQL select GCPRO#, GCBOL# into :wk_GCPRO#, :wk_GCBOL#
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:GDBLTO and
                      GCSHTO=:GDSHTO and GCDSHP=:GDDSHP;
      /End-Free

     C                   ENDIF                                                  GDSHTO<>wk_CMCUST

     C                   IF        GDSHPV <> wk_SVSVSV

      *
      * Retrieve ship via SCAC.
      *
      /Free
       wk_SVSVSV = *Blanks;
       wk_SVSCAC = *Blanks;
       EXEC SQL select SVSVSV, SVSCAC into :wk_SVSVSV, :wk_SVSCAC
                from HDSHPV
                where SVSVSV=:GDSHPV;
      /End-Free

     C                   ENDIF                                                  GDSHPV<>wk_SVSVSV

      *
      * Determnine if Ship Via data should print on UCC-128 label.
      *
      /Free
       IF wk_CtnLblShpV;
       wk_SkipLblShpV = *OFF;
       wk_GDBLTOx = %editc(GDBLTO:'X');
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='CUSTGRP' and TDKEY1='BXLSKIPLBLSV' and
                      TDKEY2='*CUST' and TDKEY3=:wk_GDBLTOx;
       IF wk_RcdCnt > *Zero;
         wk_SkipLblShpV = *ON;
       ELSE;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from GUPTDAT
                  where TDTABL='CUSTGRP' and TDKEY1='BXLSKIPLBLSV' and
                        TDKEY2='*STATE' and TDKEY3=:wk_STState;
         IF wk_RcdCnt > *Zero;
           wk_SkipLblShpV = *ON;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDIF;                                                                // wk_RcdCnt>*Zero
       ELSE;
         wk_SkipLblShpV = *ON;
       ENDIF;                                                                // wk_CtnLblShpV
      /End-Free

     C                   IF        GDITEM <> wk_IMITEM

      *
      * Retrieve item's UOM.
      *
      /Free
       wk_IMITEM = *Blanks;
       wk_IMUOMS = *Blanks;
       EXEC SQL select IMITEM, IMUOMS into :wk_IMITEM, :wk_IMUOMS
                from HDIMST
                where IMITEM=:GDITEM;
      /End-Free

      *
      * Retrieve item's UPC.
      *
      /Free
       wk_BDITEM = *Blanks;
       wk_BDUPC12 = *Blanks;
       EXEC SQL select BDITEM, BDUPC12
                 into :wk_BDITEM, :wk_BDUPC12
                from BARDATA
                where BDITEM=:GDITEM;
       // IF wk_BDITEM <> GDITEM;
       //   EXEC SQL select BDITEM, BDUPC12                                  // If not found in 2.0,
       //             into :wk_BDITEM, :wk_BDUPC12                           // check legacy file
       //            from BARDATA1
       //            where BDITEM=:GDITEM;
       // ENDIF;                                                             // wk_BDITEM<>GDITEM
      /End-Free

      *
      * Retrieve customer's item number.
      *
      /Free
       wk_VCVCIT = *Blanks;
       EXEC SQL select VCVCIT into :wk_VCVCIT
                from HDVCIT
                where VCVCF='C' and VCVNCS=:GDSHTO and VCITEM=:GDITEM;
       IF wk_VCVCIT = *Blanks;
         EXEC SQL select VCVCIT into :wk_VCVCIT
                  from HDVCIT
                  where VCVCF='C' and VCVNCS=:GDBLTO and VCITEM=:GDITEM;
       ENDIF;                                                                // wk_VCVCIT=*Blanks
       IF wk_VCVCIT = *Blanks;
         wk_CustItem = GDITEM;
       ELSE;
         wk_CustItem = wk_VCVCIT;
       ENDIF;                                                                // wk_VCVCIT=*Blanks
      /End-Free

     C                   ENDIF                                                  GDITEM<>wk_IMITEM

      *
      * Populate department fields.
      *
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      GDBLTO = 337                                 Dick's Sporting Good
     C                               or  GDBLTO = 536527                        DSG Field & Stream
      /Free
       wk_EDI850Cnt = *Zero;
       EXEC SQL select count(*) into :wk_EDI850Cnt
            from EDITPM
            where ET0143='850' and ETCUST=:GDSHTO;
       wk_ETTPID = *Blanks;
       SELECT;
       WHEN wk_EDI850Cnt = 1;
         EXEC SQL select ETTPID into :wk_ETTPID
              from EDITPM
              where ET0143='850' and ETCUST=:GDSHTO;
       WHEN wk_EDI850Cnt > 1;
         EXEC SQL select min(ETTPID) into :wk_ETTPID
              from EDITPM
              where ET0143='850' and ETCUST=:GDSHTO and
                ETTPID<>' ';
       ENDSL;
      /End-Free
     C                   IF        wk_EDI850Cnt > *Zero
     C                   EVAL      wk_Ref02Ptr =
     C                               EDI850Char(wk_ETTPID:GDPO#:'REF02':'DP')
     C                   EVAL      wk_DeptNum = wk_Ref02
     C                   EVAL      wk_Ref03Ptr =
     C                               EDI850Char(wk_ETTPID:GDPO#:'REF03':'DP')
     C                   EVAL      wk_DeptName = wk_Ref03
     C                   EVAL      wk_PgmEDR9900 = *ON
     C                   ELSE
     C                   EVAL      wk_DeptNum = *Blanks
     C                   EVAL      wk_DeptName = *Blanks
     C                   ENDIF                                                  wk_EDI850Cnt>*Zero
     C                   OTHER
     C                   EVAL      wk_DeptNum = *Blanks
     C                   EVAL      wk_DeptName = *Blanks
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   SELECT
     C                   WHEN      GDBLTO = 93                                  Dick's Sporting Good
      /Free
       wk_EDI850Cnt = *Zero;
       EXEC SQL select count(*) into :wk_EDI850Cnt
            from EDITPM
            where ET0143='850' and ETCUST=:GDSHTO;
       wk_ETTPID = *Blanks;
       SELECT;
       WHEN wk_EDI850Cnt = 1;
         EXEC SQL select ETTPID into :wk_ETTPID
              from EDITPM
              where ET0143='850' and ETCUST=:GDSHTO;
       WHEN wk_EDI850Cnt > 1;
         EXEC SQL select min(ETTPID) into :wk_ETTPID
              from EDITPM
              where ET0143='850' and ETCUST=:GDSHTO and
                ETTPID<>' ';
       ENDSL;
      /End-Free
     C                   IF        wk_EDI850Cnt > *Zero
     C                   EVAL      wk_Ref02Ptr =
     C                               EDI850Char(wk_ETTPID:GDPO#:'REF02':'DP')
     C                   EVAL      wk_DeptNum = wk_Ref02
     C                   EVAL      wk_Ref03Ptr =
     C                               EDI850Char(wk_ETTPID:GDPO#:'REF03':'DP')
     C                   EVAL      wk_DeptName = wk_Ref03
     C                   EVAL      wk_PgmEDR9900 = *ON
     C                   ELSE
     C                   EVAL      wk_DeptNum = *Blanks
     C                   EVAL      wk_DeptName = *Blanks
     C                   ENDIF                                                  wk_EDI850Cnt>*Zero
     C                   OTHER
     C                   EVAL      wk_DeptNum = *Blanks
     C                   EVAL      wk_DeptName = *Blanks
     C                   ENDSL
     C                   OTHER
     C                   EVAL      wk_DeptNum = *Blanks
     C                   EVAL      wk_DeptName = *Blanks
     C                   ENDSL

      *
      * Populate carton quantity.
      *
     C                   EVAL      wk_CaseQty = GDQOPK

      *
      * Populate carton count text.
      *
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      GDBLTO = 59                                  Bass Pro Shops
     C                               or  GDBLTO = 52                            Academy Stores
     C                               or  GDBLTO = 337                           Dick's Sporting Good
     C                               or  GDBLTO = 21419                         Gander Mountain
     C                               or  GDBLTO = 536527                        DSG Field & Stream
     C                   EVAL      wk_CtnCount = CtnTextPfx + ' ' +
     C                               %triml(%editc(wk_Carton#:'4'))
     C                   OTHER
     C                   EVAL      wk_CtnCount = *Blanks
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   SELECT
     C                   WHEN      GDBLTO = 3069                                Bass Pro Shops
     C                               or  GDBLTO = 3035                          Academy Stores
     C                               or  GDBLTO = 93                            Dick's Sporting Good
     C                   EVAL      wk_CtnCount = CtnTextPfx + ' ' +
     C                               %triml(%editc(wk_Carton#:'4'))
     C                   OTHER
     C                   EVAL      wk_CtnCount = *Blanks
     C                   ENDSL
     C                   ENDSL

      *
      * Populate ZPLCode interface fields.
      *
     C                   IF        wk_CMSLLC <> 'M'
     C                   EVAL      ds_SFName = wk_SFName
     C                   EVAL      ds_SFAddr1 = wk_SFAddr1
     C                   EVAL      ds_SFAddr2 = wk_SFAddr2
     C                   EVAL      ds_SFAddr3 = wk_SFAddr3
     C                   EVAL      ds_CustVend = wk_CustVend
     C                   IF        not wk_PlltShpOnly
     C                               and  not wk_SkipLblShpV
     C                   EVAL      ds_SCAC = wk_SVSCAC
     C****               EVAL      ds_ShipVia = GDSHPV
     C                   EVAL      ds_ShipVia = GDSVDS
     C                   EVAL      ds_PRONum = wk_GCPRO#
     C                   EVAL      ds_BOLNum = %triml(%editc(wk_GCBOL#:'Z'))
     C                   ELSE
     C                   EVAL      ds_SCAC = *Blanks
     C                   EVAL      ds_ShipVia = *Blanks
     C                   EVAL      ds_PRONum = *Blanks
     C                   EVAL      ds_BOLNum = *Blanks
     C                   ENDIF                                                  not wk_PlltShpOnly
     C                   EVAL      ds_OrderNum = %triml(%editc(GDORD#:'Z'))
     C                   EVAL      ds_STName = wk_STName
     C                   EVAL      ds_STAddr1 = wk_STAddr1
     C                   EVAL      ds_STAddr2 = wk_STAddr2
     C                   EVAL      ds_STAddr3 = wk_STAddr3
     C                   EVAL      ds_STCity = wk_STCity
     C                   EVAL      ds_STState = wk_STState
     C                   EVAL      ds_STZip = wk_STZip
     C                   EVAL      ds_STCountry = wk_STCountry
     C                   EVAL      ds_StoreNum = wk_StoreNum
     C                   EVAL      ds_PONum = GDPO#
     C                   EVAL      ds_CurItem = GDITEM
     C                   EVAL      ds_CustItem = wk_CustItem
     C                   EVAL      ds_CurItemUPC = wk_BDUPC12
     C                   EVAL      ds_CurItemDes = GDIMDS
     C                   EVAL      ds_PriceUOM = wk_IMUOMS
     C                   EVAL      ds_Qty = %triml(%editc(wk_CaseQty:'Z'))
     C                   EVAL      ds_CtnCount = wk_CtnCount
     C                   EVAL      ds_DeptNum = wk_DeptNum
     C                   EVAL      ds_DeptName = wk_DeptName
     C                   EVAL      ds_SSCCSerRef = GDSSCCBC
     C                   EVAL      ds_Ext1 = *Blanks
     C                   EVAL      ds_Ext2 = *Blanks
     C                   EVAL      ds_Ext3 = *Blanks
     C                   EVAL      ds_Ext4 = *Blanks
     C                   EVAL      ds_Ext5 = *Blanks
     C                   ELSE
     C                   CLEAR                   PARMDS0620
     C                   EVAL      ds_SFName = wk_SFName
     C                   EVAL      ds_STName = '#' + %triml(%editc(GDSHTO:'Z'))
     C                   IF        GDDSHP <> *Zero
     C                   EVAL      %subst(ds_STName:10:2) = 'DS'
     C                   ENDIF                                                  GDDSHP<>*Zero
     C                   EVAL      ds_STAddr1 = wk_STName
     C                   EVAL      ds_OrderNum = %triml(%editc(GDORD#:'Z'))
     C                   EVAL      ds_CurItem = GDITEM
     C                   EVAL      ds_CustItem = wk_CustItem
     C                   EVAL      ds_CurItemDes = GDIMDS
     C                   EVAL      ds_PriceUOM = wk_IMUOMS
     C                   EVAL      ds_Qty = %triml(%editc(wk_CaseQty:'Z'))
     C                   EVAL      ds_CtnCount = wk_CtnCount
     C                   EVAL      ds_SSCCSerRef = GDSSCCBC
     C                   ENDIF                                                  wk_CMSLLC<>'M'

     CSR                 EndSR

     C/EJECT
      **************************************************************************
      * Write HYPWILG record for OPNGRP-1 error.                               *
      **************************************************************************
     CSR   $WrtWILG1     BegSR

     C                   IF        not %open(HYPWILG)
     C                   OPEN      HYPWILG
     C                   ENDIF                                                  not %open(HYPWILG)

     C                   EVAL      WIFUNC = 'OPEN'
     C                   EVAL      WIERR = wk_FirstError
     C                   EVAL      WIDUMP = 'OPNGRP-1'
     C                   EVAL      WITCNT = wk_Tries
     C                   IF        fd1 >= *Zero
     C                   EVAL      WIUSUC = 'Y'
     C                   ELSE
     C                   EVAL      WIUSUC = 'N'
     C                   ENDIF                                                  fd1>=*Zero
     C                   EVAL      WIPGM = ps_Pgnm
     C                   EVAL      WIJOB = ps_Job
     C                   EVAL      WIUSER = ps_User
     C                   EVAL      WIJOB# = ps_Job#
     C                   EVAL      WITSTP = %timestamp
     C                   EVAL      WIFILE = wk_TxtFull1a
     C                   EVAL      WIDATA = *Blanks

     C                   WRITE     HYRWILG

     CSR                 EndSR

     C/EJECT
      **************************************************************************
      * Write HYPWILG record for OPNGRPA-1 error.                              *
      **************************************************************************
     CSR   $WrtWILG3     BegSR

     C                   IF        not %open(HYPWILG)
     C                   OPEN      HYPWILG
     C                   ENDIF                                                  not %open(HYPWILG)

     C                   EVAL      WIFUNC = 'OPEN'
     C                   EVAL      WIERR = wk_FirstError
     C                   EVAL      WIDUMP = 'OPNGRPA-1'
     C                   EVAL      WITCNT = wk_Tries
     C                   IF        fd3 >= *Zero
     C                   EVAL      WIUSUC = 'Y'
     C                   ELSE
     C                   EVAL      WIUSUC = 'N'
     C                   ENDIF                                                  fd3>=*Zero
     C                   EVAL      WIPGM = ps_Pgnm
     C                   EVAL      WIJOB = ps_Job
     C                   EVAL      WIUSER = ps_User
     C                   EVAL      WIJOB# = ps_Job#
     C                   EVAL      WITSTP = %timestamp
     C                   EVAL      WIFILE = wk_TxtFull1c
     C                   EVAL      WIDATA = *Blanks

     C                   WRITE     HYRWILG

     CSR                 EndSR

     C/EJECT
      **************************************************************************
      * Write joblog message - *DIAG                                           *
      **************************************************************************
     CSR   $WrtJobLog    BegSR

     C                   IF        not wk_JobLogged
     C                   CALLP(E)  QCMDEXC ('CHGJOB LOG(4 00 *SECLVL)':24)
     C                   EVAL      wk_JobLogged = *ON
     C                   ENDIF                                                  not wk_JobLogged

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*DIAG' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 EndSR

     P CrtGrpTxtFile   E

     P/EJECT
      **************************************************************************
      * Retrieve next consolidated shipment PRO number.                        *
      **************************************************************************
     P RtvCSPRO#       B                   EXPORT
     D                 PI            30A
     D ShpLoc                         4A   CONST
     D ShpVia                         2A   CONST

     D wk_ShpLoc       S              4A
     D wk_ShpVia       S              2A
     D wk_PRO#         S             30A
     D wk_NbrLen       S              3P 0
     D wk_ChkDgt       S              1A
     D wk_TDCF03       S             30A

     C                   DO

     C                   EVAL      wk_ShpLoc = ShpLoc
     C                   EVAL      wk_ShpVia = ShpVia
     C                   EVAL      wk_PRO# = *Blanks

     C                   IF        not %open(HYPBXPA)
     C                   OPEN      HYPBXPA
     C                   ENDIF                                                  not %open(HYPBXPA)

       wk_TDCF03 = *Blanks;
       EXEC SQL select TDCF03 into :wk_TDCF03
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_ShpVia;
       IF wk_TDCF03 <> *Blanks;
         wk_ShpVia = wk_TDCF03;
       ENDIF;                                                                // wk_TDCF03<>*Blanks

       SETLL (wk_ShpLoc:wk_ShpVia) HYPBXPA;
       DOU %eof(HYPBXPA) or (PASTS = '2' and PALASTNO < PAENDNO);
         READE (wk_ShpLoc:wk_ShpVia) HYPBXPA;
       ENDDO;                                                                // %eof(HYPBXPA) or
       IF %eof(HYPBXPA);
         SETLL (wk_ShpLoc:wk_ShpVia) HYPBXPA;
         DOU %eof(HYPBXPA) or (PASTS = '1' and PALASTNO < PAENDNO);
           READE (wk_ShpLoc:wk_ShpVia) HYPBXPA;
         ENDDO;                                                              // %eof(HYPBXPA) or
         IF %eof(HYPBXPA);
           LEAVE;                                                            // ERROR - no PRO#
         ENDIF;                                                              // %eof(HYPBXPA)
       ENDIF;

       IF PALASTNO = *Zero;
         PALASTNO = PABEGNO;
       ELSE;
         PALASTNO = PALASTNO + 1;
       ENDIF;
       IF PALASTNO < PAENDNO;
         PASTS = '2';
       ELSE;
         PASTS = '3';
       ENDIF;

       IF PACDTYP = *Blank  or PANBRLEN = *Zero;
         wk_NbrLen = PANBRLEN;
       ELSE;
         wk_NbrLen = PANBRLEN - 1;
       ENDIF;                                                                // PACDTYP=*Blank or

       IF wk_NbrLen = *Zero  or  wk_NbrLen = %len(PALASTNO);
         wk_PRO# = %trimr(PAPFX) + %editc(PALASTNO:'X') + PASFX;
       ELSE;
         wk_PRO# = %trimr(PAPFX) +
           %subst(%editc(PALASTNO:'X'):%len(PALASTNO)-wk_NbrLen+1:wk_NbrLen) +
           PASFX;
       ENDIF;                                                                // wk_NbrLen=*Zero or

       SELECT;
       WHEN PACDTYP = '1';
         wk_ChkDgt = RtvM10ChkDgt(wk_PRO#);
         wk_PRO# = %trimr(wk_PRO#) + wk_ChkDgt;
       WHEN PACDTYP = '2';
         wk_ChkDgt = RtvM7ChkDgt(wk_PRO#);
         wk_PRO# = %trimr(wk_PRO#) + wk_ChkDgt;
       ENDSL;

       UPDATE HYRBXPA %fields(PASTS:PALASTNO);

     C                   ENDDO

     C                   RETURN    wk_PRO#

     P RtvCSPRO#       E

     P/EJECT
      **************************************************************************
      * Retrieve next consolidated shipment BOL number.                        *
      **************************************************************************
     P RtvCSBOL#       B                   EXPORT
     D                 PI             8P 0

     D wk_BOL#         S              8P 0
     D wk_BOL#Chk      S              8P 0

     C                   DO

     C                   EVAL      wk_BOL# = *Zero

     C                   IF        not %open(OECTRL)
     C                   OPEN      OECTRL
     C                   ENDIF                                                  not %open(OECTRL)

     C                   DOW       wk_BOL# = *Zero
     C                   DOU       not %error
     C     1             CHAIN(E)  OECTRL
     C                   ENDDO
     C                   IF        %found(OECTRL)
     C                   EVAL      CELBL# = CELBL# + 1
     C                   UPDATE    OECTRLR
      /Free
      //
      // Determine whether BOL already exists.
      //
       wk_BOL#Chk = *Zero;
       EXEC SQL Select SDBOL# into :wk_BOL#Chk
                From OEBOLH Where SDBOL#=:CELBL#;
       IF wk_BOL#Chk <> CELBL#;
         wk_BOL# = CELBL#;
       ENDIF;                                                                // wk_BOL#Chk<>CELBL#
      /End-Free
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  %found(OECTRL)
     C                   ENDDO                                                  wk_BOL#=*Zero

     C                   ENDDO

     C                   RETURN    wk_BOL#

     P RtvCSBOL#       E

     P/EJECT
      ********************************************************************
      * Error handling procedure.                                        *
      ********************************************************************
      /define ERRNO_LOAD_PROCEDURE
      /COPY EXPAT/QRPGLESRC,ERRNO_H

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR0610      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(OEORHP)
     C                   CLOSE     OEORHP
     C                   ENDIF                                                  %open(OEORHP)

     C                   IF        %open(OEORDP)
     C                   CLOSE     OEORDP
     C                   ENDIF                                                  %open(OEORDP)

     C                   IF        %open(HYPBXPP)
     C                   CLOSE     HYPBXPP
     C                   ENDIF                                                  %open(HYPBXPP)

     C                   IF        %open(HYPSGCU)
     C                   CLOSE     HYPSGCU
     C                   ENDIF                                                  %open(HYPSGCU)

     C                   IF        %open(HYPSGDT)
     C                   CLOSE     HYPSGDT
     C                   ENDIF                                                  %open(HYPSGDT)

     C                   IF        %open(HYPSGDTU)
     C                   CLOSE     HYPSGDTU
     C                   ENDIF                                                  %open(HYPSGDTU)

     C                   IF        %open(HYPBXPA)
     C                   CLOSE     HYPBXPA
     C                   ENDIF                                                  %open(HYPBXPA)

     C                   IF        %open(HYPWICT)
     C                   CLOSE     HYPWICT
     C                   ENDIF                                                  %open(HYPWICT)

     C                   IF        %open(HYPWILG)
     C                   CLOSE     HYPWILG
     C                   ENDIF                                                  %open(HYPWILG)

     C                   IF        %open(OECTRL)
     C                   CLOSE     OECTRL
     C                   ENDIF                                                  %open(OECTRL)

     C                   IF        %open(HYW0610A)
     C                   CLOSE     HYW0610A
     C                   ENDIF                                                  %open(HYW0610A)

     C                   IF        %open(HYW0610B)
     C                   CLOSE     HYW0610B
     C                   ENDIF                                                  %open(HYW0610B)

     C                   CALLP(E)  sd_HYR0185 ()

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR0610      E

     P/EJECT
