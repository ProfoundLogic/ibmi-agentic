     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2010')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0138')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Pallet Contents Entry/Maintenance                         *
      *          BY: William Earl (MCA)                                        *
      *        DATE: December 2010                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  02/16/2011  MCA  Add pallet attributes display/change.                *
      *  02/18/2011  MCA  Fix generic output queue retrieval.                  *
      *  07/20/2011  MCA  Allow F3=Exit on pallet selection screen.            *
      *  08/05/2011  MCA  Allow pallet attribute screen access directly from   *
      *                   "add carton to pallet" screen.                       *
      *  09/14/2011  MCA  Allow temporary employee agency number entry.        *
      *  10/03/2011  MCA  Fix bug - if pallet contents were put on a BOL while *
      *                   still palletizing the rest of the shipment, an error *
      *                   was shown that the BOL# was not the same as the rest *
      *                   of the pallet.                                       *
      *  11/10/2011  MCA  Check if shipment has been totally scanned.          *
      *  11/17/2011  MCA  Change check for partial shipment not palletized to  *
      *                   display the cartons not palletized.                  *
      *  12/01/2011  MCA  Add pallet banner print.                             *
      *  03/16/2012  MCA  Add subset by carton UCC-128 serial number.          *
      *  04/02/2012  MCA  Quit looking to TEST for HYLTEMP2.                   *
      *  06/26/2012  MCA  Force keyboard reset for any errors when adding      *
      *                   cartons to pallet.                                   *
      *  08/22/2012  MCA  Temporary - in order to trap the bug where the       *
      *                   pallet totals are shown wrong on the screen, each    *
      *                   time FMT3B is displayed, the UCC-128 records will be *
      *                   resummarized and the totals compared.                *
      *  05/31/2013  MCA  Maintain turnaround number separation.               *
      *  06/21/2013  MCA  Fix bug - handle 8-digit employee number entry -     *
      *                   always invalid.  Prior would get (C G D F) target    *
      *                   too small to hold the result.                        *
      *  07/22/2013  MH   Changed to accomodate 200 orders and turns.          *
      *  11/03/2014  MCA  Change call interface to pallet content report.      *
      *  07/14/2015  MCA  Add option to generate FBI pallet banner.            *
      *  04/14/2016  MCA  SnapSafe changes.                                    *
      *  12/02/2016  MCA  SnapSafe changes - add 2nd GS1 prefix.  Tkt #2053    *
      *  01/11/2017  MCA  SnapSafe changes - allow HY employee number.         *
      *  06/13/2017  MCA  SnapSafe changes - allow HY license plate prefix.    *
      *  08/17/2017  MCA  Force keyboard reset for any errors when adding      *
      *                   cartons to pallet - ERR0694.                         *
      *  08/30/2018  MH   Add pallet length and width fields.                  *
      *  10/17/2018  MH   Default pallet width and length on write to hyppllt. *
      *  10/18/2018  MH   Pallet Length was not staying on the screen.  I      *
      *                   dicovered the field name in the dspf was FHLEN       *
      *                   when it should have been F8LEN.  Because of urgency  *
      *                   I changed the program to use FHLEN instead of        *
      *                   changing the DSPF.                                   *
      *  10/23/2018  MCA  Print hazard labels for BoxLogix shipments if        *
      *                   required and not already printed.                    *
      *  02/01/2019  MCA  Fix bug - when palletizer keyed the 9-digit SSCC,    *
      *                   instead of scanning the SSCC barcode, hazard label   *
      *                   data was not generated.  Also, if required to print, *
      *                   the label did not print.                             *
      *  02/21/2019  MCA  Allow BoxLogix shipment hazard label print when this *
      *                   program not entered through BoxLogix screens.        *
      *  02/24/2019  MCA  Prompt for pallet report print when changing pallets *
      *                   or exiting the program.                              *
      *  02/24/2019  MCA  Due to other DSPF changes, FHLEN changed back to     *
      *                   F8LEN.  See 10/18/2018 mod.                          *
      *  03/12/2019  MCA  Fix bug on pallet report print prompt.  Bill-to# was *
      *                   not always being populated.                          *
      *  03/20/2019  MCA  Cabela's BoxLogix fields.                            *
      *  03/28/2019  MCA  Check for zero weights.                              *
      *  06/11/2019  MCA  Changed to HYR0156 to accomodate 300 orders & turns. *
      *  06/13/2019  MCA  Duplicate label approval for cartons with BoxLogix   *
      *                   errors.                                              *
      *  07/15/2019  MCA  Generate explosive label data for Alaska and Hawaii, *
      *                   and any U.S. territories (in addition to generating  *
      *                   for foreign customers based on customer class and    *
      *                   any shipment to be shipped outside of the U.S.).     *
      *  01/30/2020  MCA  If this program called from BoxLogix shipment        *
      *                   program, validate that scanned label was generated   *
      *                   within the shipment group associated with the call   *
      *                   to this program.                                     *
      *  04/17/2020  MCA  Changes to pallet report.  Change to print with      *
      *                   PO# and to use customer's item number if available.  *
      *  07/09/2020  MCA  Warn user for customer not allowing mixed POs on     *
      *                   pallet if new PO scanned to pallet.                  *
      *  10/14/2021  MCA  Handle shipping master-pack scan.                    *
      *  05/16/2022  MCA  Add CRTNEQSUMM parm to HYC3512 call.                 *
      *  11/28/2022  MCA  If any hazard label data was generated, move it from *
      *                   HYPEDWK to HYPELD.                                   *
      *  04/27/2023  MCA  Fix bug.  Ship-to customer class was being used when *
      *                   should have been bill-to class.  This affected when  *
      *                   a hazard label might print (or its data might be     *
      *                   written).  And it affected when subsetting the pallet*
      *                   list by Domestic/Foreign.  Only affected if ship-to  *
      *                   customer was different from bill-to customer.        *
      *  08/03/2023  MCA  HYR0156 interface changes.  AlwNone added for        *
      *                   HYR0140.                                             *
      *  09/08/2023  MCA  Fix Bug.  The first palletized carton for a new      *
      *                   pallet did not create a HYPELD record, when the      *
      *                   shipment required it (for example, for a foreign     *
      *                   shipment shipped through BoxLogix).                  *
      *  04/10/2024  MCA  If QC data is needed for the carton being scanned,   *
      *                   don't allow it to be scanned.                        *
      *  07/22/2024  MCA  When printing pallet banner, pass print type parm.   *
      *  11/27/2024  MCA  Handle wk_LastPO SQL null retrieval (no SQL0305).    *
      *  04/26/2025  MCA  Print hazard labels based on drop-ship address state *
      *                   or country (if otherwise applicable).                *
      *  09/01/2025  MCA  Allow palletizer to specify if hazard labels print   *
      *                   and to print hazard labels on demand.                *
      *  09/12/2025  MCA  Handle situation where source powder charge is not   *
      *                   found, but the hazard label record's powder weight   *
      *                   (NEQ) was manually added (e.g., S9MMLUGER+P).        *
      *  03/10/2026  MCA  Remove "temporary" change made 8/22/12.  13 yrs ago! *
      **************************************************************************

     FHYD0138   CF   E             WORKSTN
     F                                     SFILE(SFL01:wk_Sfl01RRN)
     F                                     SFILE(SFL02:wk_Sfl02RRN)
     F                                     SFILE(SFL03:wk_Sfl03RRN)
     F                                     SFILE(SFL04:wk_Sfl04RRN)
     FHYPPLLT   UF A E           K DISK
     FHYLPLLT1  IF   E           K DISK    RENAME(HYRPLLT:HYRPLLT1)
     FHYLSSCC8  UF   E           K DISK    RENAME(HYRSSCC:HYRSSCC8)             k-SCSRL#
     F                                     RENAME(HYRSCWK:HYRSCWK8)
     F                                     PREFIX(SC:2)
     FHYLSSCC9  UF   E           K DISK    RENAME(HYRSSCC:HYRSSCC9)             k-SCPSRL#
     F                                     RENAME(HYRSCWK:HYRSCWK9)
     F                                     PREFIX(SC:2)
     FHYPMPCD   IF   E           K DISK
     FHYLMPCD2  IF   E           K DISK    RENAME(HYRMPCD:HYRMPCD2)
     FOEORHD    IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FHDDSHP    IF   E           K DISK
     FHDCTRY    IF   E           K DISK
     FOEBOLH    IF   E           K DISK
     FHDIMST    IF   E           K DISK
     FHDPCLS    IF   E           K DISK
     FHREMPL20  IF   E           K DISK
     FHREMPL20HYIF   E           K DISK    EXTDESC('HYHDSDATA/HREMPL20')
     F                                     EXTFILE(*EXTDESC)
     F                                     RENAME(HREMPLR:HREMPLRHY)
     FHYLTEMP2  IF   E           K DISK
     F****                                 EXTFILE('XXHORNADY/HYLTEMP2')
     FHDSHPV    IF   E           K DISK
     FGUPTDAT   IF   E           K DISK
     FHYPWSOQ   IF   E           K DISK
     FHYPPMLG   O    E             DISK    BLOCK(*NO) USROPN
     FHYPMILG   O    E             DISK    BLOCK(*NO) USROPN

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0138         PR                  EXTPGM('HYR0138')
     D  PgmMode                       1A   CONST

      /COPY *LIBL/QPRPSRC,HYR0139

      /COPY *LIBL/QPRPSRC,HYR0139A

      /COPY *LIBL/QPRPSRC,HYR0552

      /COPY *LIBL/QPRPSRC,HYR0622

      /COPY *LIBL/QPRPSRC,HYR9930

     D HYC3512         PR                  EXTPGM('HYC3512')                    Pallet report print
     D   out_Pallet#                  9P 0 CONST
     D   out_BOL#                     8P 0 CONST
     D   out_ShipTo#                  7P 0 CONST
     D   out_InclInvc                 4A   CONST
     D   out_ShowPO                   4A   CONST
     D   out_Item#Typ                 5A   CONST
     D   out_CrtNEQSu                 4A   CONST
     D   out_CrtSplf                  4A   CONST
     D   out_CSVDocID                40A   CONST
     D   out_OutQ                    10A   CONST
     D   out_Drawer                   3P 0 CONST
     D   out_Copies                   3P 0 CONST
     D   out_HoldSplf                 4A   CONST
     D   out_Submit                   4A   CONST

     D HYC3550         PR                  EXTPGM('HYC3550')                    Pallet banner print
     D   out_Pallet#                  9P 0 CONST
     D   out_BannerTy                 1A   CONST
     D   out_PrintTyp                 1A   CONST
     D   out_OutQ                    10A   CONST
     D   out_Drawer                   3P 0 CONST
     D   out_Copies                   3P 0 CONST
     D   out_HoldSplf                 4A   CONST
     D   out_Caller                  10A   CONST
     D   in_RtnCode                   2A

     D HYR0152         PR                  EXTPROC('HYR0152')                   HYPEDWK --> HYPELD
     D  Function                      1A   CONST
     D  Order#                        8P 0 CONST
     D  Line#                         3P 0 CONST
     D  Blanket#                      3P 0 CONST
     D  BoxWgt                        6P 2 CONST
     D  HazMatNQ                      6P 2 CONST
     D  PowderWgt                     6P 2 CONST
     D  WgtUOM                        2A   CONST
     D  Serial#                       9P 0 CONST

     D HYR0156         PR                  EXTPGM('HYR0156')                    Missing pallet check
     D  Pallet#                       9P 0 CONST
     D  OrderList                  2400A   CONST VARYING OPTIONS(*VARSIZE)      Up to 300 orders
     D  TurnList                   2700A   CONST VARYING OPTIONS(*VARSIZE)      Up to 300 turns
     D  OrderCnt                     10I 0 CONST
     D  AlwNone                       1N   CONST
     D  ForceF21                      1N   CONST
     D  ShutDown                      1N   CONST
     D  RtnCode                       2A

     D HHDCSS          PR                  EXTPROC('HHDCSS')                    Customer no. search
     D  io_Cust#                      7P 0

     D HHDSVI          PR                  EXTPROC('HHDSVI')                    Ship via search
     D  io_ShipVia                    2A

     D HHDISI          PR                  EXTPROC('HHDISI')                    Item number search
     D  Item#                        15A

     D HHDPCS          PR                  EXTPROC('HHDPCS')                    Prod class search
     D  ProdClass                     4A

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')                    Exec system command
     D  out_Command                 512A   CONST
     D  out_CmdLen                   15P 5 CONST

     D QSYGETPH        PR                  EXTPGM('QSYGETPH')                   Get user prof handle
     D  UserID                       10A   CONST
     D  Password                     10A   CONST
     D  Handle                       12A
     D  ErrorCode                   528A
     D  PasswordLen                  10I 0 CONST
     D  PasswordCCSID                10I 0 CONST

     D QSYRLSPH        PR                  EXTPGM('QSYRLSPH')                   Rls user prof handle
     D  Handle                       12A

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

     D QMHRMVPM        PR                  EXTPGM('QMHRMVPM')                   Remove program msgs
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  out_MsgKey                    4A   CONST
     D  out_MsgToRmv                 10A   CONST
     D  io_MsgErrCode                10I 0

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0138         PI
     D  in_PgmMode                    1A   CONST

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263
     D  ps_Job#              264    269

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D                 DS
     D TimeDate                1     14S 0
     D  td_Time                1      6S 0
     D  td_Date                7     14S 0

     D                 DS
     D ds_CYMD                 2      8  0
     D ds_MDY                  5     10  0
     D ds_YY1                  3      4  0
     D ds_YY2                  9     10  0

     D ds_APIError     DS
     D ds_APIBytesAvl          5      8B 0
     D ds_APIErrID             9     15
     D ds_APIErrMsgDt         17    528

      **************************************************************************
      * Define arrays.                                                         *
      **************************************************************************
     D CustAddr        S             45    DIM(4)                               Customer address
     D idx             S              3P 0
     D idx2            S              3P 0

     D ary_MP          S                   LIKE(MDMP#) DIM(200)
     D idx3            S              3P 0

     D ary_HLOrd       S                   LIKE(SCORD#) DIM(200)                Hazard label orders
     D idx4            S              3P 0
     D idx4a           S              3P 0

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_GS1Comp      S              6A   DTAARA(GS1COMP)                      GS1 Company Prefix
     D wk_GS1Comp2     S              6A   DTAARA(GS1COMP2)                     GS1 Company Prefix 2
     D PLTSRLREF       S              9P 0 DTAARA                               Pallet UCC-128 ref#
     D wk_BXLDupChk    S             10A   DTAARA(HYABXLDUP)                    Dup check BXL error
     D wk_LDA          S           1024A   DTAARA(*LDA)                         *LDA

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PgmMode      S              1A
     D wk_Subr1Mode    S              1A
     D wk_Subr3Mode    S              1A
     D wk_EmpType      S              1A                                        Employee type
     D wk_Emply#       S              7P 0                                      Employee number
     D wk_GS1Comp3     S              6A                                        GS1 Company Prefix 3
     D wk_IN25         S                   LIKE(*IN25)
     D wk_Sfl01RRN     S              5P 0
     D wk_Sfl01PC      S              5P 0
     D wk_Sfl01PS      S              3P 0 INZ(11)
     D wk_Sfl02RRN     S              5P 0
     D wk_Sfl03RRN     S              5P 0
     D wk_Sfl03PC      S              5P 0
     D wk_Sfl03PS      S              3P 0 INZ(9)
     D wk_Sfl04RRN     S              5P 0
     D wk_Include      S              1N   INZ(*OFF)
     D wk_SelPr        S              1N   INZ(*OFF)
     D wk_DltPr        S              1N   INZ(*OFF)
     D wk_DspPr        S              1N   INZ(*OFF)
     D wk_PrtPr        S              1N   INZ(*OFF)
     D wk_MovPr        S              1N   INZ(*OFF)
     D wk_DtlPr        S              1N   INZ(*OFF)
     D wk_LblPr        S              1N   INZ(*OFF)
     D wk_AttrPr       S              1N   INZ(*OFF)
     D wk_ChkMissPr    S              1N   INZ(*OFF)
     D wk_BannerPr     S              1N   INZ(*OFF)
     D wk_Banner2Pr    S              1N   INZ(*OFF)
     D wk_PrtHzLbPr    S              1N   INZ(*OFF)
     D wk_OptPr        S              1N   INZ(*OFF)
     D wk_Refresh      S              1N   INZ(*OFF)
     D wk_Error        S              1N   INZ(*OFF)
     D wk_ClrDone      S              1N   INZ(*OFF)
     D wk_FirstDlt     S                   LIKE(wk_Sfl01RRN)
     D wk_BillToName   S                   LIKE(CMCNA1)
     D wk_ShipToName   S                   LIKE(CMCNA1)
     D wk_Pro#         S                   LIKE(SDPRO#)
     D wk_BOL#         S                   LIKE(SDBOL#)
     D wk_BOL#2        S                   LIKE(SCBOL#)
     D wk_ShipVia      S                   LIKE(OESVSV)
     D wk_ShipViaDesc  S                   LIKE(OESVDS)
     D wk_ExtWgt       S              7P 2
     D wk_TotCtn       S              5P 0
     D wk_TotWgt       S              7P 2
     D wk_TotCtnX      S              5P 0
     D wk_TotWgtX      S              7P 2
     D wk_TotWgtXX     S              5P 0
     D wk_LastPO       S                   LIKE(OEORRF)
     D wk_Pallet#      S                   LIKE(PMPSRL#)                        Active pallet#
     D wk_Pallet#2     S                   LIKE(PMPSRL#)                        RtvPltTots pallet#
     D wk_Pallet#3     S                   LIKE(PMPSRL#)                        $Carton pallet#
     D wk_Pallet#4     S                   LIKE(PMPSRL#)                        $ChkAllScan pallet#
     D wk_PSrl#        S                   LIKE(PMPSRL#)                        $PalletSumm pallet#
     D wk_ChkType      S              1A
     D wk_ShipTo#      S                   LIKE(PMSHTO)
     D wk_ShipTo#2     S                   LIKE(PMSHTO)
     D wk_DropShip#    S                   LIKE(PMDSHP)
     D wk_DropShip#2   S                   LIKE(PMDSHP)
     D wk_Order#       S                   LIKE(SCORD#)
     D wk_Order#2      S                   LIKE(SCORD#)
     D wk_Order#X      S              8A
     D wk_Turn#X       S              9A
     D wk_Serial#      S                   LIKE(SCSRL#)
     D wk_SSPallet#    S                   LIKE(PMPSRL#)
     D wk_AlwF3        S              1N
     D wk_ReqF11       S              1N
     D wk_ChkPrtl      S              1N
     D wk_SelMade      S              1N
     D wk_NewPallet    S              1N
     D wk_AlwF22       S              1N   INZ(*OFF)
     D wk_F22Srl#      S                   LIKE(wk_Serial#)
     D wk_Foreign      S              1N
     D wk_FrgnChkCust  S                   LIKE(PMSHTO) INZ(-1)
     D wk_BillTo#      S                   LIKE(CMBLTO)
     D wk_BillTo#X     S              7A
     D wk_CMCNA1       S             26A
     D wk_BTCCLS       S              2A
     D wk_BTCCLS#      S                   LIKE(CMBLTO)
     D wk_ShowPO       S              4A
     D wk_Item#Type    S              5A
     D wk_ODWH         S              3S 0
     D wk_IWFTCL       S              5A
     D wk_GDGRP#       S              9S 0
     D wk_GDORD#       S              8S 0
     D wk_GDORL#       S              3S 0
     D wk_GDBLN#       S              3S 0
     D wk_GDLBL2       S              1A
     D wk_GDSSCC       S              9S 0
     D wk_GDSSCCBC     S             20A
     D wk_FromBxlgx    S              1N                                        Frgn hazard lbls
     D wk_ShipGrp#     S              9S 0
     D wk_HazLblCtl    S              1A                                        Hazard label control
     D wk_HazLbl       S              1N                                        Frgn hazard lbls
     D wk_PrtHazLbl    S              1N
     D wk_LastPallet#  S                   LIKE(wk_Pallet#)
     D wk_LastPallet2  S                   LIKE(wk_Pallet#)
     D wk_PlltPrtType  S              5A
     D wk_BuyGrpCust   S              1N
     D wk_CMMXPO       S              1A
     D wk_CMPLDR       S              1A
     D wk_TDKEY2       S             15A
     D wk_MXPOShipTo#  S                   LIKE(wk_ShipTo#) INZ(-1)
     D wk_MPScanned    S              1N   INZ(*OFF)
     D wk_MP#          S                   LIKE(MDMP#)
     D wk_AllUpd       S              1N   INZ(*OFF)
     D wk_PAVErr       S              1N   INZ(*OFF)
     D wk_QCNeeded     S              1N   INZ(*OFF)
     D wk_RcdCnt       S              7P 0
     D wk_RcdCnt2      S              7P 0
     D wk_ProfHndl     S             12A
     D wk_OutQType     S                   LIKE(WOOQTY)
     D wk_OutQ         S                   LIKE(WOOUTQ)
     D wk_OutQ1        S                   LIKE(WOOUTQ)
     D wk_OutQ2        S                   LIKE(WOOUTQ)
     D wk_OutQ3        S                   LIKE(WOOUTQ)
     D wk_Drawer       S              3P 0
     D wk_Copies       S              3P 0
     D wk_Wkstn        S                   LIKE(ps_Job)
     D wk_Len          S              3P 0
     D wk_Cmd          S            256A
     D wk_IN12         S                   LIKE(*IN12)
     D wk_ForceF21     S              1N
     D wk_WdwRfrsh     S              1N
     D wk_RC0139A      S             10I 0
     D wk_RC0156       S              2A
     D wk_RC0552       S             10I 0
     D wk_RC0622       S             10I 0
     D wk_RC3550       S              2A
     D wk_Pgm0152      S              1N   INZ(*OFF)
     D wk_Pgm0156      S              1N   INZ(*OFF)
     D wk_Pgm0552      S              1N   INZ(*OFF)
     D wk_Pgm0622      S              1N   INZ(*OFF)
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D io_Cust#        S                   LIKE(CMCUST)
     D io_ShipVia      S                   LIKE(SVSVSV)
     D io_Item#        S                   LIKE(IMITEM)
     D io_ProdCls      S                   LIKE(PCPCLS)
     D sv_Order#       S                   LIKE(SCORD#)
     D sv_Serial#      S                   LIKE(SCSRL#)
     D sv_HGT          S                   LIKE(PMHGT)
     D sv_LEN          S                   LIKE(PMLEN)
     D sv_WID          S                   LIKE(PMWID)
     D sv_SSDF         S                   LIKE(F6DF)
     D sv_SSShto       S                   LIKE(F6SHTO)
     D sv_SSSTNm       S                   LIKE(F6STNM)
     D sv_SSDS         S                   LIKE(F6DS)
     D sv_SSBOL#       S                   LIKE(F6BOL#)
     D sv_SSPRO#       S                   LIKE(F6PRO#)
     D sv_SSShpV       S                   LIKE(F6SHPV)
     D sv_SSSVDs       S                   LIKE(F6SVDS)
     D sv_SSCtnSrl#    S                   LIKE(F6SRL#)
     D sv_SSPallet#    S                   LIKE(PMPSRL#)
     D sv_SSItem       S                   LIKE(F7ITEM)
     D sv_SSItemDescX  S                   LIKE(F7IMDSX)
     D sv_SSItemDesc   S                   LIKE(F7IMDS)
     D sv_SSProdCls    S                   LIKE(F7PCLS)
     D sv_SSProdClsDs  S                   LIKE(F7PCDS)
     D sv_F3SRL#       S                   LIKE(F3SRL#)
     D kf_Emply#       S                   LIKE(EMEMPL)
     D kf_Table        S                   LIKE(TDTABL)
     D kf_Key1         S                   LIKE(TDKEY1)
     D kf_Key2         S                   LIKE(TDKEY2)
     D kf_Key3         S                   LIKE(TDKEY3)
     D kf_WSID         S                   LIKE(WOWSID)
     D kf_OQTY         S                   LIKE(WOOQTY)
     D @UPSRL#         S                   LIKE(PMPSRL#)                        SFL01 keys
     D @USRL#          S                   LIKE(SCSRL#)                         SFL03 keys
     D @UORD#          S                   LIKE(SCORD#)                            "   "
     D @UORL#          S                   LIKE(SCORL#)                            "   "
     D @UBLN#          S                   LIKE(SCBLN#)                            "   "
     D @ULOT#          S                   LIKE(SCLOT#)                            "   "
     D @DPSRL#         S                   LIKE(PMPSRL#)                        SFL01 keys
     D @DSRL#          S                   LIKE(SCSRL#)                         SFL03 keys
     D @DORD#          S                   LIKE(SCORD#)                            "   "
     D @DORL#          S                   LIKE(SCORL#)                            "   "
     D @DBLN#          S                   LIKE(SCBLN#)                            "   "
     D @DLOT#          S                   LIKE(SCLOT#)                            "   "
     D C1HDG2A         S                   LIKE(C1HDG2)
     D C3HDG2A         S                   LIKE(C3HDG2)
     D C3HDG2B         S                   LIKE(C3HDG2)
     D C3HDG2C         S                   LIKE(C3HDG2)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D EntryMode       C                   '1'
     D MaintMode       C                   '2'
     D MaintMode1      C                   'A'
     D MaintMode2      C                   'B'
     D InqMode         C                   '3'
     D SelMode         C                   '4'
     D BldELD          C                   '2'
     D ShutDown        C                   '9'
     D @SEL            C                   ' 1'
     D @SEL2           C                   '1 '
     D @DLT            C                   ' 4'
     D @DLT2           C                   '4 '
     D @DSP            C                   ' 5'
     D @DSP2           C                   '5 '
     D @PRT            C                   ' 6'
     D @PRT2           C                   '6 '
     D @MOV            C                   ' 7'
     D @MOV2           C                   '7 '
     D @DTL            C                   ' 8'
     D @DTL2           C                   '8 '
     D @LBL            C                   ' 9'
     D @LBL2           C                   '9 '
     D @ATTR           C                   '12'
     D @CHKMISS        C                   '13'
     D @BANNER         C                   '18'
     D @BANNER2        C                   '19'
     D @PRTHZLB        C                   '23'
     D Digits          C                   '0123456789'
     D ErrorMsg1       C                   'Press Enter to try again.'

     D EntryHdg1       C                   'Shipping Pallet Contents Entry'
     D MaintHdg1       C                   'Shipping Pallet Contents Maintenanc-
     D                                     e'
     D InqHdg1         C                   'Shipping Pallet Contents Inquiry'
     D PalletSelect    C                   'Pallet Selection'
     D OptDsc1A        C                   '4=Delete   5=Display   6=Report   8-
     D                                     =Detail   9=Labels   12=Pallet attr'
     D OptDsc1B        C                   '5=Display   6=Report   8=Detail   9-
     D                                     =Labels   12=Pallet attr'
     D OptDsc1C        C                   '1=Select   5=Display   6=Report   8-
     D                                     =Detail   9=Labels   12=Pallet attr'
     D OptDsc2         C                   '13=Check missing scans   18=Banner -
     D                                       19=FBI Banner'
     D OptDsc2A        C                   '23=Print hazard labels'

     D MaintHdg3       C                   'Pallet Detail Maintenance'
     D InqHdg3         C                   'Pallet Detail Inquiry'
     D OptDsc3A        C                   '4=Remove   5=Display   -
     D                                     23=Print hazard label'
     D OptDsc3B        C                   '5=Display   7=Move'
     D OptDsc3C        C                   '5=Display'

     D SummHdg2A       C                   'Pallet Review'
     D SummHdg2B       C                   'Pallet Summary'
     D PressEnter      C                   'Press Enter to continue.'
     D PressF11        C                   'Press F11 to continue.'

     D NewPallet       C                   'New Pallet Carton Detail Entry'
     D AddCarton       C                   'Add Carton To Pallet'

     D InqHdg5         C                   'Carton UCC-128 Data'

     D F3Txt           C                   'F3=Exit'
     D F11Txt          C                   'F11=Continue'
     D F12Txt          C                   'F12=Cancel'
     D F14Txta         C                   'F14=Print hazard labels'
     D F14Txtb         C                   'F14=No hazard labels'

     D DftPWidth       C                   40
     D DftPLength      C                   48

     D LC              C                   'abcdefghijklmnopqrstuvwxyz'
     D UC              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      **************************************************************************
      * Define input records.                                                  *
      **************************************************************************
     IHYRSSCC8      01

     IHYRSCWK8      02

     IHYRSSCC9      01

     IHYRSCWK9      02

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_PgmMode = in_PgmMode

     C                   EXSR      $InzSR

     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C                   EXSR      $Prompt1
     C                   WHEN      wk_PgmMode = MaintMode
     C                   EXSR      $Prompt1
     C                   WHEN      wk_PgmMode = InqMode
     C                   EVAL      F1EMP# = *Blanks
     C                   EVAL      wk_Subr1Mode = InqMode                       Inquiry mode
     C                   EXSR      $Pallet
     C                   ENDSL

      *
      * If any HYPEDWK records were written (by PrtUCC128Lbl), call HYR0152
      * to move those records to HYPELD.
      *
     C                   IF        idx4 > *Zero
     C                   EXSR      $MoveToED
     C                   ENDIF                                                  idx4>*Zero

     C                   EXSR      $ShutDownPgms

     C                   EVAL      *INLR = *ON
     C                   RETURN

     C/EJECT
      **************************************************************************
      *****                S  U  B  R  O  U  T  I  N  E  S                 *****
      **************************************************************************
      **************************************************************************
      * Prompt for employee number (one time at start of program only).        *
      **************************************************************************
     CSR   $Prompt1      BEGSR

     C                   EVAL      F1EMP# = *Blanks

     C                   DOW       not *IN03

     C                   WRITE     MSFCTL01
     C                   EXFMT     DSPFMT1A
     C                   MOVE      *ON           *IN93
     C                   EXSR      $RMVPM
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN89 = *OFF

     C                   IF        *IN25

     C                   SELECT
     C                   WHEN      *IN03  or  *IN12
     C                   LEAVE
     C                   ENDSL

     C                   ELSE

     C                   EXSR      $Edit1
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C                   EXSR      $AddCarton
     C                   WHEN      wk_PgmMode = MaintMode
     C                   EVAL      wk_Subr1Mode = MaintMode                     Maintenance mode
     C                   EXSR      $Pallet
     C                   ENDSL

     C                   LEAVE

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate prompt screen.                                                *
      **************************************************************************
     CSR   $Edit1        BEGSR

     C                   EVAL      wk_Error = *OFF
     C                   EVAL      wk_EmpType = *Blank
     C                   EVAL      wk_Emply# = *Zero

      *
      * Employee number is required and must be valid.
      *
     C                   IF        F1EMP# = *Blanks
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0440'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        %subst(F1EMP#:1:1) = 'T'
     C                   IF        %subst(F1EMP#:2) <> *Blanks  and
     C                               %check(Digits:%trimr(F1EMP#):2) = *Zero
     C                   EVAL      wk_EmpType = 'T'
     C                   EVAL      wk_Emply# =
     C                               %dec(%subst(%trimr(F1EMP#):2):7:0)
     C                   IF        wk_Emply# <> *Zero
     C     wk_Emply#     CHAIN     HYLTEMP2
     C                   IF        not %found(HYLTEMP2)
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0441'
     C                   EVAL      wk_MsgData = F1EMP#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  not %found(HYLTEMP2)
     C                   ELSE
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0441'
     C                   EVAL      wk_MsgData = F1EMP#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_Emply#<>*Zero
     C                   ELSE
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0441'
     C                   EVAL      wk_MsgData = F1EMP#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  %check(Digits:%trimr
     C                   ELSE
     C                   IF        %check(Digits:%trimr(F1EMP#)) = *Zero
     C                               and  %len(%trimr(F1EMP#)) < 8
     C                               and  (F1EMP# <> '88888'  or
     C                                      ps_User = 'BEARL')
     C                   EVAL      wk_EmpType = 'P'
     C                   EVAL      wk_Emply# = %dec(%trimr(F1EMP#):7:0)
     C                   IF        wk_Emply# < 100000
     C                   EVAL      kf_Emply# = wk_Emply#
     C     kf_Emply#     CHAIN     HREMPL20
     C                   IF        not %found(HREMPL20)  and
     C                              (wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX')
     C     kf_Emply#     CHAIN     HREMPL20HY
     C                   ENDIF                                                  not %found(HREMPL20)
     C****               IF        not %found(HREMPL20)
     C                   IF        not %found
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0441'
     C                   EVAL      wk_MsgData = F1EMP#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  not %found(HREMPL20)
     C                   ELSE
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0441'
     C                   EVAL      wk_MsgData = F1EMP#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_Emply#<100000
     C                   ELSE
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0441'
     C                   EVAL      wk_MsgData = F1EMP#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  %check(Digits:%trimr
     C                   ENDIF                                                  %subst(F1EMP#:1:1)
     C                   ENDIF                                                  F1EMP#=*Blanks

      *
      * If errors detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Add carton to pallet.                                                  *
      **************************************************************************
     CSR   $AddCarton    BEGSR

     C                   EVAL      wk_NewPallet = *ON
     C                   EVAL      wk_HazLblCtl = 'D'                           *DEFAULT
     C                   EVAL      F3HZLB = '*DEFAULT'
     C                   EVAL      F3F14TXT = F14Txta

     C                   SELECT
     C                   WHEN      wk_EmpType = 'P'
     C                   EVAL      F3EMP = EMRNAM
     C                   WHEN      wk_EmpType = 'T'
     C                   IF        TEMIDI = *Blank
     C                   EVAL      F3EMP = %trimr(TEFNAM) + ' ' + TELNAM
     C                   ELSE
     C                   EVAL      F3EMP = %trimr(TEFNAM) + ' ' + TEMIDI + ' ' +
     C                               TELNAM
     C                   ENDIF                                                  TEMIDI=*Blank
     C                   ENDSL
     C                   EVAL      F3SRL# = *Blanks
     C                   EVAL      *IN99 = *OFF

     C                   DOW       not *IN03

     C                   IF        not wk_NewPallet
     C                   EVAL      wk_Pallet#2 = wk_Pallet#
     C****               EXSR      $ChkPltTots
     C                   ENDIF                                                  not wk_NewPallet
     C                   WRITE     MSFCTL01
     C                   IF        wk_NewPallet
     C                   EVAL      *IN22 = *OFF
     C                   EXFMT     DSPFMT3A
     C                   ELSE
     C                   EVAL      *IN22 = wk_AlwF22
     C                   IF        wk_AlwF22
     C                   EVAL      F3MSG =
     C                               'Press F22 to reset drop ship numbers.'
     C                   ENDIF                                                  wk_AlwF22
     C                   EXFMT     DSPFMT3B
     C                   ENDIF                                                  wk_NewPallet
     C                   MOVE      *ON           *IN93
     C                   EXSR      $RMVPM
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN89 = *OFF
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      wk_AlwF22 = *OFF
     C                   EVAL      F3MSG = *Blanks

     C                   IF        *IN25

     C                   SELECT

      *
      * Exit or cancel.
      *
     C                   WHEN      *IN03  or  *IN12
     C                   IF        wk_LastPallet# <> *Zero
     C                   EXSR      $ChkPlltRpt
     C                   IF        *IN12
     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_LastPallet#<>*Zer
     C                   IF        wk_Pallet# <> *Zero
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '1'
     C                   EXSR      $ChkAllScan
     C                   IF        wk_Error
     C                   EVAL      *IN03 = *OFF
     C                   ITER
     C                   ENDIF                                                  wk_Error
     C                   ENDIF                                                  wk_Pallet#<>*Zero
     C                   LEAVE

      *
      * New pallet.
      *
     C                   WHEN      *IN06
     C                   IF        wk_LastPallet# <> *Zero
     C                   EXSR      $ChkPlltRpt
     C                   IF        *IN12
     C                   EVAL      *IN06 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_LastPallet#<>*Zer
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '1'
     C                   EXSR      $ChkAllScan
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error
     C                   EVAL      wk_NewPallet = *ON
     C                   EVAL      wk_Pallet# = *Zero
     C                   EVAL      F3SRL# = *Blanks
     C                   EVAL      wk_HazLblCtl = 'D'                           *DEFAULT
     C                   EVAL      F3HZLB = '*DEFAULT'
     C                   EVAL      F3F14TXT = F14Txta

     C                   EVAL      *IN06 = *OFF

      *
      * Change pallet - choose existing pallet.
      *
     C                   WHEN      *IN07
     C                   IF        wk_LastPallet# <> *Zero
     C                   EXSR      $ChkPlltRpt
     C                   IF        *IN12
     C                   EVAL      *IN07 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_LastPallet#<>*Zer
     C                   EVAL      wk_Subr1Mode = SelMode                       Selection mode
     C                   EXSR      $Pallet
     C                   IF        not *IN12
     C     wk_Pallet#    CHAIN(N)  HYPPLLT
     C                   IF        not %found(HYPPLLT)
     C                   CLEAR     *ALL          HYRPLLT
     C                   ENDIF                                                  not %found(HYPPLLT)
     C                   IF        PMSHTO <> F3CUST
     C                   IF        idx4 > *Zero
     C                   EXSR      $MoveToED
     C                   ENDIF                                                  idx4>*Zero
     C                   ENDIF                                                  PMSHTO<>F3CUST
     C                   EVAL      wk_ShipTo# = PMSHTO
     C                   EVAL      wk_DropShip# = PMDSHP
     C                   EVAL      wk_BOL# = PMBOL#
     C                   EXSR      $MoveToF3
     C                   EVAL      wk_Pallet#2 = wk_Pallet#
     C                   EXSR      $RtvPltTots
     C                   EVAL      F3TCTN = wk_TotCtn
     C                   EVAL(H)   F3TWGT = wk_TotWgt
     C                   SELECT
     C                   WHEN      PMHZLB = 'Y'
     C                   EVAL      wk_HazLblCtl = 'Y'
     C                   EVAL      F3HZLB = 'YES'
     C                   EVAL      F3F14TXT = F14Txtb
     C                   WHEN      PMHZLB = 'N'
     C                   EVAL      wk_HazLblCtl = 'N'
     C                   EVAL      F3HZLB = 'NO'
     C                   EVAL      F3F14TXT = F14Txta
     C                   OTHER
     C                   EVAL      wk_HazLblCtl = 'N'
     C                   EVAL      F3HZLB = 'NO'
     C                   EVAL      F3F14TXT = F14Txta
     C                   ENDSL
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count (distinct b.OEORRF) into :wk_RcdCnt
                from HYPSSCC a join OEORHD b on a.SCORD#=b.OEORD#
                where a.SCPSRL#=:wk_Pallet#;
       IF wk_RcdCnt <= 1;
         wk_RcdCnt2 = *Zero;
         EXEC SQL select count (distinct b.OEORRF) into :wk_RcdCnt2
                  from HYPSCWK a join OEORHD b on a.S1ORD#=b.OEORD#
                  where a.S1PSRL#=:wk_Pallet# and
                  not exists (select * from HYPSSCC c join OEORHD d
                  on c.SCORD#=d.OEORD# where c.SCPSRL#=a.S1PSRL# and
                  d.OEORRF=b.OEORRF);
         wk_RcdCnt = wk_RcdCnt + wk_RcdCnt2;
       ENDIF;                                                                // wk_RcdCnt<=1
       SELECT;
       WHEN wk_RcdCnt = *Zero;
         EVAL wk_LastPO = '*NEW';
       WHEN wk_RcdCnt = 1;
         EVAL wk_LastPO = *Blanks;
         EXEC SQL select coalesce(min(b.OEORRF),' ') into :wk_LastPO
                  from HYPSSCC a join OEORHD b on a.SCORD#=b.OEORD#
                  where a.SCPSRL#=:wk_Pallet#;
       WHEN wk_RcdCnt > 1;
         EVAL wk_LastPO = '*MULT';
       ENDSL;
      /End-Free
     C                   EVAL      wk_NewPallet = *OFF
     C                   ENDIF                                                  not *IN12
     C                   EVAL      F3SRL# = *Blanks

      *
      * Pallet detail.
      *
     C                   WHEN      *IN08
     C                   EVAL      wk_Pallet#3 = wk_Pallet#
     C                   EVAL      wk_Subr3Mode = MaintMode1                    Maintenance mode 1
     C                   EXSR      $Carton
     C                   EVAL      F3SRL# = *Blanks
     C                   EVAL      *IN08 = *OFF

      *
      * Pallet summary.
      *
     C                   WHEN      *IN09
     C                   EVAL      wk_PSrl# = wk_Pallet#
     C                   EVAL      wk_ReqF11 = *OFF
     C                   EVAL      wk_ChkPrtl = *ON
     C                   EVAL      F2HDG2 = SummHdg2B
     C                   EXSR      $PalletSumm
     C                   EVAL      F3SRL# = *Blanks
     C                   EVAL      *IN09 = *OFF

      *
      * Display/change pallet attributes.
      *
     C                   WHEN      *IN10
     C                   EVAL      wk_PSrl# = wk_Pallet#
     C                   EXSR      $PalletAttr
     C                   EVAL      *IN10 = *OFF

      *
      * Carton entry complete.
      *
     C                   WHEN      *IN11
     C                   IF        wk_LastPallet# <> *Zero
     C                   EXSR      $ChkPlltRpt
     C                   IF        *IN12
     C                   EVAL      *IN11 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_LastPallet#<>*Zer
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '1'
     C                   EXSR      $ChkAllScan
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error
     C                   LEAVE

      *
      * Check for missing scans.
      *
     C                   WHEN      *IN13
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '2'
     C                   EVAL      wk_WdwRfrsh = *OFF
     C                   EXSR      $ChkAllScan
     C                   EVAL      *IN13 = *OFF

      *
      * Change whether to print hazard labels.
      *
     C                   WHEN      *IN14
     C                   IF         wk_HazLblCtl = 'N'  or
     C                                wk_HazLblCtl = 'D'
     C                   EVAL      wk_HazLblCtl = 'Y'
     C                   EVAL      F3HZLB = 'YES'
     C                   EVAL      F3F14TXT = F14Txtb
     C                   ELSE
     C                   EVAL      wk_HazLblCtl = 'N'
     C                   EVAL      F3HZLB = 'NO'
     C                   EVAL      F3F14TXT = F14Txta
     C                   ENDIF                                                  wk_HazLblCtl='N' or
     C                   EVAL      *IN14 = *OFF
      *
      * Pallet report print.
      *
     C                   WHEN      *IN16
     C                   EVAL      wk_PSrl# = wk_Pallet#
     C                   EVAL      wk_PlltPrtType = '*MNL'
     C                   EXSR      $PrtPallet
     C                   EVAL      *IN16 = *OFF

      *
      * Pallet labels print.
      *
     C                   WHEN      *IN17
     C                   EVAL      wk_PSrl# = wk_Pallet#
     C                   EXSR      $PrtLabels
     C                   EVAL      *IN17 = *OFF

      *
      * Pallet banner print.
      *
     C                   WHEN      *IN18
     C                   EVAL      wk_PSrl# = wk_Pallet#
     C                   EXSR      $PrtBanner
     C                   EVAL      *IN18 = *OFF

      *
      * Reset order drop ships.
      *
     C                   WHEN      *IN22
     C                   EVAL      wk_PSrl# = wk_Pallet#
     C                   EXSR      $ResetDS
     C                   EVAL      *IN22 = *OFF

     C                   ENDSL

     C                   ELSE

     C                   EXSR      $Edit3
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   IF        wk_NewPallet

     C                   EVAL      wk_PSrl# = -1
     C                   EVAL      wk_ReqF11 = *ON
     C                   EVAL      wk_ChkPrtl = *OFF
     C                   EVAL      F2HDG2 = SummHdg2A
     C                   EXSR      $PalletSumm
     C                   IF        *IN03  or  *IN12
     C                   ITER
     C                   ENDIF                                                  *IN03 or *IN12

     C                   EVAL      wk_ShipTo# = OESHTO
     C                   EVAL      wk_DropShip# = OEDSHP
     C                   EVAL      wk_TotCtn = *Zero
     C                   EVAL      wk_TotWgt = *Zero
     C                   EVAL      F3TCTN = *Zero
     C                   EVAL      F3TWGT = *Zero
     C                   EVAL      wk_LastPO = OEORRF

     C                   ENDIF                                                  wk_NewPallet

     C                   IF        OEORRF <> wk_LastPO
     C                   IF        wk_LastPO <> '*NEW'
     C                   IF        wk_ShipTo# <> wk_MXPOShipTo#
      /Free
       wk_RcdCnt = *Zero;
       wk_CMMXPO = *Blank;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='BUYINGGRPS' and TDKEY1=:wk_DatabaseID
                 and TDKEY2=digits(:OEBLTO);
       IF wk_RcdCnt = *Zero;
         EXEC SQL select CMMXPO into :wk_CMMXPO
                  from HDCUST2
                  where CMCUST=:OEBLTO;
       ELSE;
         EXEC SQL select CMMXPO into :wk_CMMXPO
                  from HDCUST2
                  where CMCUST=:OESHTO;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free
     C                   EVAL      wk_MXPOShipTo# = wk_ShipTo#
     C                   ENDIF                                                  wk_ShipTo#<>wk_MXPOS
     C                   IF        wk_CMMXPO = 'N'
     C                   EVAL      W1TEXT1 = 'The current carton scan is for +
     C                               a new customer PO.  POs'
     C                   EVAL      W1TEXT2 = 'must be separated on pallets +
     C                               for this customer.'
     C                   EVAL      W1TEXT3 = *Blanks
     C                   EXSR      $GenWarn
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_CMMXPO='N'
     C                   ENDIF                                                  wk_LastPO<>'*NEW'
     C                   EVAL      wk_LastPO = OEORRF
     C                   ENDIF                                                  OEORRF<>wk_LastPO

     C                   EXSR      $Update3

     C                   IF        not wk_MPScanned
     C                   EVAL      wk_MsgID = 'MSG0200'
     C                   EVAL      wk_MsgData =
     C                               %triml(%editc(wk_Pallet#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:9) =
     C                               %triml(%editc(wk_Serial#:'Z'))
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      wk_MsgID = 'MSG0436'
     C                   EVAL      wk_MsgData =
     C                               %triml(%editc(wk_Pallet#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:12) = wk_MP#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  not wk_MPScanned

     C                   EVAL      wk_LastPallet# = wk_Pallet#
     C                   IF        RtvPRSts1 (wk_LastPallet#) = '*PRINT'
     C                   EVAL      wk_RC0139A =
     C                               WrtPRLog (wk_LastPallet#:'*RESET')
     C                   ENDIF                                                  RtvPRSts1 (wk_LastPa

     C                   EVAL      F3SRL# = *Blanks

     C                   IF        wk_NewPallet
     C                   IF        wk_ShipTo# <> F3CUST
     C                   IF        idx4 > *Zero
     C                   EXSR      $MoveToED
     C                   ENDIF                                                  idx4>*Zero
     C                   ENDIF                                                  wk_ShipTo#<>F3CUST
     C                   EXSR      $MoveToF3
     C                   EVAL      wk_NewPallet = *OFF
     C                   ENDIF                                                  wk_NewPallet

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate add carton to pallet screen.                                  *
      **************************************************************************
     CSR   $Edit3        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * UCC-128 license plate number, a.k.a. serial number, is required
      * and must be valid.
      *
     C                   EVAL      wk_MPScanned = *OFF
     C                   EVAL      wk_MP# = *Blanks
     C                   EVAL      sv_F3SRL# = *Blanks
     C                   IF        %len(%trim(F3SRL#)) = 12  and                Shipping master
     C                                %subst(F3SRL#:1:3) = 'HMP'                  pack??
     C                   EVAL      wk_MP# = %trim(F3SRL#)
     C     wk_MP#        CHAIN     HYPMPCD
     C                   IF        %found(HYPMPCD)
     C                   EVAL      sv_F3SRL# = F3SRL#
     C                   EVAL      F3SRL# = %editc(MDSRL#:'X')
     C                   EVAL      wk_MPScanned = *ON
     C                   ENDIF                                                  %found(HYPMPCD)
     C                   ENDIF                                                  %len(%trim(F3SRL#))=
     C                   IF        F3SRL# = *Blanks
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0666'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ELSE
     C****               IF        %check(Digits:%trim(F3SRL#)) > *Zero  or
     C****                           (%len(%trim(F3SRL#)) = 20  and
     C****                            %subst(F3SRL#:1:10) <> '0000090255')  or
     C****                           (%len(%trim(F3SRL#)) > 9  and
     C****                            %len(%trim(F3SRL#)) < 20)  or
     C****                            %subst(F3SRL#:21:1) <> ' '
     C                   IF        %check(Digits:%trim(F3SRL#)) > *Zero  or
     C                               (%len(%trim(F3SRL#)) = 20  and
     C                                (%subst(F3SRL#:1:4) <> '0000'  or
     C                                 (%subst(F3SRL#:5:6) <> wk_GS1Comp  and
     C                                  %subst(F3SRL#:5:6) <> wk_GS1Comp2  and
     C                                  %subst(F3SRL#:5:6) <> wk_GS1Comp3)))  or
     C                               (%len(%trim(F3SRL#)) > 9  and
     C                                %len(%trim(F3SRL#)) < 20)  or
     C                                %subst(F3SRL#:21:1) <> ' '
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        %len(%trim(F3SRL#)) < 10
     C                   EVAL      wk_Serial# = %dec(%trim(F3SRL#):9:0)
     C                   ELSE
     C                   EVAL      wk_Serial# = %dec(%subst(F3SRL#:11:9):9:0)
     C                   ENDIF                                                  %len(%trim(F3SRL#))<
     C                   IF        *IN22
     C                   LEAVESR
     C                   ENDIF                                                  *IN22
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      *IN02 = *OFF
     C     wk_Serial#    CHAIN(N)  HYLSSCC8
     C                   IF        not %found(HYLSSCC8)
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        *IN02
     C                   EVAL      SCBOL# = *Zero
     C                   ENDIF                                                  *IN02
     C                   IF        SCPSRL# <> *Zero
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   IF        SCPSRL# = wk_Pallet#
     C                   EVAL      *IN99 = *ON
     C                   EVAL      wk_MsgID = 'ERR0694'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   ELSE
     C                   EVAL      *IN99 = *ON
     C                   EVAL      wk_MsgID = 'ERR0695'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EVAL      %subst(wk_MsgData:21:9) =
     C                               %triml(%editc(wk_Serial#:'Z'))
     C                   EVAL      %subst(wk_MsgData:30:9) =
     C                               %triml(%editc(SCPSRL#:'Z'))
     C                   ENDIF
     C                   EXSR      $SndPM
     C                   EXSR      $ChkPAVErr
     C                   IF        wk_PAVErr
     C                   EVAL      *IN99 = *OFF
     C                   EXSR      $ShowWINDOW03
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_PAVErr
     C                   ELSE
     C                   EVAL      wk_Order# = SCORD#
     C     wk_Order#     CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   EVAL      OESHTO = *Zero
     C                   EVAL      OEDSHP = *Zero
     C                   ENDIF                                                  not %found(OEORHD)
     C                   IF        not wk_NewPallet
     C                   IF        OESHTO <> wk_ShipTo#
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0687'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EVAL      %subst(wk_MsgData:21:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        OEDSHP <> wk_DropShip#
     C                   EVAL      wk_F22Srl# = wk_Serial#
     C                   EVAL      wk_AlwF22 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0716'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EVAL      %subst(wk_MsgData:21:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:28:7) = '*none'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:28:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   IF        OEDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:35:7) = '*none'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:35:7) =
     C                               %triml(%editc(OEDSHP:'Z'))
     C                   ENDIF                                                  OEDSHP=*Zero
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        SCBOL# <> wk_BOL#
     C                   IF        wk_BOL# = *Zero
     C                   EXSR      $RtvBOL#
     C                   ENDIF                                                  wk_BOL#=*Zero
     C                   IF        SCBOL# <> wk_BOL#
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0693'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EVAL      %subst(wk_MsgData:21:8) =
     C                               %triml(%editc(wk_BOL#:'Z'))
     C                   EXSR      $SndPM
     C                   ENDIF                                                  SCBOL#<>wk_BOL#
     C                   ENDIF                                                  SCBOL#<>wk_BOL#
     C                   ENDIF                                                  OEDSHP<>wk_DropShip#
     C                   ENDIF                                                  OESHTO<>wk_ShipTo#
     C                   ENDIF                                                  not wk_NewPallet
     C                   ENDIF                                                  SCPSRL#<>*Zero
     C                   ENDIF                                                  not %found(HYLSSCC8)
     C                   ENDIF                                                  %check(Digits:%trim(
     C                   ENDIF                                                  F3SRL#=*Blanks

     C                   IF        wk_Error  and  wk_MPScanned
     C                   EVAL      F3SRL# = sv_F3SRL#
     C                   ENDIF                                                  wk_Error  and

     C                   IF        not wk_Error  and  wk_FromBxlgx
      /Free
       wk_GDGRP# = *Zero;
       wk_GDSSCC = *Zero;
       EXEC SQL select GDGRP#, GDSSCC
                 into :wk_GDGRP#, :wk_GDSSCC
                from HYPSGDT
                where GDSSCC=:wk_Serial#
                fetch first row only;
      /End-Free
     C                   IF        wk_GDSSCC = wk_Serial#
     C                   IF        wk_GDGRP# <> wk_ShipGrp#
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1343'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EVAL      %subst(wk_MsgData:21:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:30:9) =
     C                               %triml(%editc(wk_GDGRP#:'Z'))
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_GDGRP#<>wk_ShipGr
     C                   ELSE
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1344'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_GDSSCC=wk_Serial#
     C                   ENDIF                                                  wk_Error and

      *
      * If no other errors, check for zero weight.
      *
     C                   IF        not wk_Error  and  SCUWGT = *Zero
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      wk_MsgID = 'ERR1295'
     C                   EVAL      wk_MsgData = %trim(F3SRL#)
     C                   EVAL      %subst(wk_MsgData:21:15) = SCITEM
     C                   EXSR      $SndPM
     C                   ENDIF                                                  not wk_Error

      *
      * If no other errors, check for missing QC data.
      *
     C                   IF        not wk_Error  and  SCLOT# <> *Blanks
     C                   EVAL      wk_QCNeeded = NeedQCData (SCITEM:
     C                               SCLOT#:OEBLTO:OESHTO:OEDSHP)
     C                   IF        wk_QCNeeded
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPELD
                where EDSRL#=:wk_Serial# and EDPWGT>0;
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEDWK
                  where EWSRL#=:wk_Serial# and EWPWGT>0;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      wk_MsgID = 'ERR1458'
     C                   EVAL      wk_MsgData = SCITEM + SCLOT#
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  wk_QCNeeded
     C                   ENDIF                                                  not wk_Error and

      *
      * If errors detected, sound alarm and force keyboard reset.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check for BoxLogix P&A Verify error (indicating a manual shipment).    *
      **************************************************************************
     CSR   $ChkPAVErr    BEGSR

     C                   EVAL      wk_PAVErr = *OFF

     C                   IF        wk_BxlDupChk = '*NOCHK'
     C                   LEAVESR
     C                   ENDIF                                                  wk_BxlDupChk='*NOCHK

      /Free
       EXEC SQL select count(*) into :wk_Rcdcnt
                from HYPSGLG
                where LGSSCC=:wk_Serial#;
      /End-Free

     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_PAVErr = *ON
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Show WINDOW03.                                                         *
      **************************************************************************
     CSR   $ShowWINDOW03 BEGSR

     C                   EVAL      W3USER = *Blanks
     C                   EVAL      W3PWD = *Blanks

     C                   DOW       not *IN03
     C                   WRITE     MSFCTL01
     C                   IF        wk_NewPallet
     C                   WRITE     DSPFMT3A
     C                   ELSE
     C                   WRITE     DSPFMT3B
     C                   ENDIF                                                  wk_NewPallet
     C                   EXFMT     WINDOW03
     C                   EXSR      $RmvPM
     C                   IF        W3USER = wk_BxlDupChk
     C                   EVAL      W3USER = '*BYPASS'
     C                   LEAVE
     C                   ENDIF                                                  W3USER=wk_BxlDupChk
     C                   IF        W3USER = *Blanks
     C                   EVAL      wk_MsgID = 'ERR1305'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ENDIF                                                  W3USER=*Blanks
     C                   IF        W3PWD = *Blanks
     C                   EVAL      wk_MsgID = 'ERR1306'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ENDIF                                                  W3PWD=*Blanks
     C                   IF        W3USER = *Blanks  or  W3PWD = *Blanks
     C                   ITER
     C                   ENDIF                                                  W3USER=*Blanks or
     C                   EVAL      W3PWD = %xlate(LC:UC:W3PWD)
     C                   EVAL      kf_Table = 'AUTHUSER'
     C                   EVAL      kf_Key1 = ps_Pgnm
     C                   EVAL      kf_Key2 = 'SUPERAPRVL'
     C                   EVAL      kf_Key3 = W3USER
     C     key_TD3       CHAIN     GUPTDAT
     C                   IF        not %found(GUPTDAT)
     C                   EVAL      wk_MsgID = 'ERR1304'
     C                   EVAL      wk_MsgData = W3USER
     C                   EXSR      $SndPM
     C                   ITER
     C                   ELSE
     C                   IF        TDCF04 = *Blanks
     C                   CALLP(E)  QSYGETPH  (W3USER:W3PWD:wk_ProfHndl:
     C                               ds_APIError:10:-1)
     C                   IF        ds_APIBytesAvl > 8  and
     C                               ds_APIErrID <> *Blanks
     C                   EVAL      wk_MsgID = ds_APIErrID
     C                   IF        ds_APIBytesAvl > 16
     C                   EVAL      wk_MsgData =
     C                               %subst(ds_APIErrMsgDt:1:ds_APIBytesAvl-16)
     C                   ELSE
     C                   EVAL      wk_MsgData = *Blanks
     C                   ENDIF                                                  ds_APIBytesAval>16
     C                   EXSR      $SndPM2
     C                   EVAL      W3PWD = *Blanks
     C                   ITER
     C                   ELSE
     C                   CALLP(E)  QSYRLSPH  (wk_ProfHndl)
     C                   LEAVE
     C                   ENDIF                                                  ds_APIBytesAval>8
     C                   ELSE
     C                   IF        W3PWD <> TDCF04
     C                   EVAL      wk_MsgID = 'ERR1307'
     C                   EVAL      wk_MsgData = W3USER
     C                   EXSR      $SndPM
     C                   EVAL      W3PWD = *Blanks
     C                   ITER
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  W3PWD<>TDCF04
     C                   ENDIF                                                  TDCF04=*Blanks
     C                   ENDIF                                                  not %found(GUPTDAT)
     C                   ENDDO                                                  not *IN03

      /Free
       EXEC SQL update HYPSGLG set LGPDUS=:W3USER, LGPDTS=current timestamp
                where LGSSCC=:wk_Serial#;
      /End-Free
     C                   EVAL      F3SRL# = *Blanks
     C                   EVAL      wk_MsgID = 'MSG0399'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_Serial#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:10) = W3USER
     C                   EXSR      $SndPM

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Add carton to pallet - update fields.                                  *
      **************************************************************************
     CSR   $Update3      BEGSR

     C                   IF        wk_NewPallet
     C     *LOCK         IN        PLTSRLREF
     C                   EVAL      PLTSRLREF = PLTSRLREF + 1
     C                   OUT       PLTSRLREF
     C                   EVAL      wk_Pallet# = PLTSRLREF
     C                   IF        wk_HazLblCtl <> 'Y'  and
     C                               wk_HazLblCtl <> 'N'
     C                   IF        OEMAIL = 'AIR'                               OEORHD rtvd $Edit3
     C                   EVAL      wk_HazLblCtl = 'Y'
     C                   EVAL      F3HZLB = 'YES'
     C                   EVAL      F3F14TXT = F14Txtb
     C                   ELSE
     C                   EVAL      wk_HazLblCtl = 'N'
     C                   EVAL      F3HZLB = 'NO'
     C                   EVAL      F3F14TXT = F14Txta
     C                   ENDIF                                                  OEMAIL='AIR'
     C                   ENDIF                                                  wk_HazLblCtl<>'Y'
     C                   ENDIF                                                  wk_NewPallet

     C                   EVAL      wk_AllUpd = *OFF
     C                   IF        wk_MPScanned
     C                   EVAL      wk_Serial# = *Zero
     C     wk_MP#        SETLL     HYPMPCD
     C                   ENDIF                                                  wk_MPScanned

     C                   DOU       wk_AllUpd

     C                   IF        wk_MPScanned
     C     wk_MP#        READE     HYPMPCD
     C                   IF        %eof(HYPMPCD)
     C                   EVAL      wk_AllUpd = *ON
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPMPCD)
     C                   IF        MDSRL# = wk_Serial#
     C                   ITER
     C                   ENDIF                                                  MDSRL#=wk_Serial#
     C                   EVAL      wk_Serial# = MDSRL#
     C                   ENDIF                                                  wk_MPScanned

     C                   EVAL      wk_BOL#2 = *Zero
     C     wk_Serial#    SETLL     HYLSSCC8
     C                   DOU       %eof(HYLSSCC8)
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      *IN02 = *OFF
     C     wk_Serial#    READE     HYLSSCC8
     C                   IF        %eof(HYLSSCC8)
     C                   ITER
     C                   ENDIF                                                  %eof(HYLSSCC8)
     C                   EVAL      SCPSRL# = wk_Pallet#
     C                   EVAL      SCPEMTY = wk_EmpType
     C                   EVAL      SCPEMPL = wk_Emply#
     C                   SELECT
     C                   WHEN      *IN01
     C                   UPDATE    HYRSSCC8
     C                   WHEN      *IN02
     C                   UPDATE    HYRSCWK8
     C                   ENDSL
     C                   EVAL(H)   wk_ExtWgt = SCUWGT * SCUOMQ
     C                   EVAL      wk_TotWgt = wk_TotWgt + wk_ExtWgt
     C                   EVAL(H)   F3TWGT = wk_TotWgt
      /Free
       wk_ODWH = *Zero;
       EXEC SQL select ODWH into :wk_ODWH                                    // Fixes HDIWHS SQL
                from OEORDT                                                  //  below (SQL0811)
                where ODORD#=:SCORD# and ODORL#=:SCORL# and ODBLN#=:SCBLN#;
       wk_IWFTCL = *Blanks;
       EXEC SQL select IWFTCL into :wk_IWFTCL
                from HDIWHS
                where IWITEM=:SCITEM and IWWHS=:wk_ODWH;
       IF not wk_FromBxlgx;
         wk_HazLbl = *OFF;
         wk_ShipGrp# = *Zero;
         wk_GDSSCC = *Zero;
         EXEC SQL select coalesce(max(GDGRP#),0), coalesce(max(GDSSCC),0)
                   into :wk_ShipGrp#, :wk_GDSSCC
                  from HYPSGDT
                  where GDSSCC=:wk_Serial#;
         IF wk_GDSSCC = wk_Serial#;
           IF wk_ShipTo# <> CMCUST  or
               wk_NewPallet;
             CHAIN wk_ShipTo# HDCUST;
             IF not %found(HDCUST);
               CMST = *Blanks;
               CMCTRY = *Blanks;
               CMCCLS = *Blanks;
             ENDIF;                                                          // not %found(HDCUST)
           ENDIF;                                                            // wk_ShipTo#<>CMCUST
           IF wk_DropShip# <> *Zero  and
               (wk_DropShip# <> DSNMBR)  or
                wk_NewPallet;
             CHAIN ('C':wk_ShipTo#:wk_DropShip#) HDDSHP;
             IF not %found(HDDSHP);
               DSST = *Blanks;
               DSCTRY = *Blanks;
             ENDIF;                                                          // not %found(HDDSHP)
           ENDIF;                                                            // wk_DropShip#<>*Zero
           IF wk_BTCCLS# <> wk_BillTo#;
             wk_BTCCLS = *Blanks;
             EXEC SQL select CMCCLS into :wk_BTCCLS
                      from HDCUST
                      where CMCUST=:wk_BillTo#;
             wk_BTCCLS# = wk_BillTo#;
           ENDIF;                                                            // wk_BTCCLS#<>wk_BillT
           IF (CMCTRY <> 'US'  and  CMCTRY <> *Blanks)  or
               (CMST = 'AK'  or  CMST = 'HI'  or                             // Alaska & Hawaii
                CMST = 'AS'  or  CMST = 'GU'  or                             // US Territories
                CMST = 'MP'  or  CMST = 'PR'  or
                CMST = 'VI')  or
              (wk_DropShip# <> *Zero and
               ((DSCTRY <> 'US'  and  DSCTRY <> *Blanks)  or
                (DSST = 'AK'  or  DSST = 'HI'  or                             // Alaska & Hawaii
                 DSST = 'AS'  or  DSST = 'GU'  or                             // US Territories
                 DSST = 'MP'  or  DSST = 'PR'  or
                 DSST = 'VI')))  or
                wk_BTCCLS = 'RF';
             wk_HazLbl = *ON;
           ELSE;
             kf_Table = 'CUSTCLASS';
             kf_Key1 = wk_BTCCLS;
             CHAIN key_TD GUPTDAT;
             IF %found(GUPTDAT)  and  TDFF02 = 'Y';
               wk_HazLbl = *ON;
             ENDIF;                                                          // %found(GUPTDAT) and
           ENDIF;                                                            // CMCTRY<>'US' and
         ENDIF;                                                              // wk_GDSSCC=wk_Serial#
       ENDIF;                                                                // not wk_FromBxlgx
       IF (wk_HazLbl  and  wk_IWFTCL='AMMO')  or
          (wk_HazLblCtl='Y'  and  wk_IWFTCL='AMMO')  or
           wk_IWFTCL = 'AMMOB';
         wk_GDORD# = *Zero;
         wk_GDORL# = *Zero;
         wk_GDBLN# = *Zero;
         wk_GDLBL2 = *Blank;
         wk_GDSSCCBC = *Blanks;
         EXEC SQL select GDORD#, GDORL#, GDBLN#, GDLBL2,
                         GDSSCCBC
                   into :wk_GDORD#, :wk_GDORL#, :wk_GDBLN#, :wk_GDLBL2,
                        :wk_GDSSCCBC
                  from HYPSGDT a
                  where a.GDGRP#=:wk_ShipGrp# and a.GDSSCC=:wk_Serial# and
                   exists (select * from HDIMST b where b.IMITEM=a.GDITEM and
                   ((substr(b.IMPCLS,1,1)='A' and b.IMLOT<>'N') or b.IMPCLS in
                   ('AAAA','AAGC') or (substr(b.IMPCLS,1,1)='F' and
                   substr(b.IMPCLS,1,2)<>'FT') or substr(b.IMPCLS,1,2)='AS'));
      // IF wk_GDLBL2 = 'N';
      //   IF wk_HazLbl;
         IF wk_GDLBL2 = 'N'  or  wk_HazLblCtl = 'Y';
           IF wk_HazLblCtl = 'Y';
             wk_PrtHazLbl = *ON;
           ELSE;
             wk_PrtHazLbl = *OFF;
           ENDIF;                                                            // wk_HazLbl
           wk_Pgm0622 = *ON;
        // wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:wk_GDORD#:wk_GDORL#:wk_GDBLN#:
        //                           wk_GDSSCCBC:'E':wk_PrtHazLbl:*OFF:*Zero:
        //                           *Blanks);
           wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:wk_GDORD#:wk_GDORL#:wk_GDBLN#:
                                     wk_GDSSCCBC:'X':wk_PrtHazLbl:*OFF:*Zero:
                                     *Blanks);
           IF wk_RC0622 = 2  or  wk_RC0622 = 4;
             EXEC SQL update HYPSGDT set GDLBL2='Y'
                      where GDGRP#=:wk_ShipGrp# and GDSSCC=:wk_Serial#;
           ENDIF;                                                            // wk_RC0622=2 or
           idx4a = %lookup(wk_GDORD#:ary_HLOrd:1:idx4);
           IF idx4a = *Zero;
             idx4 = idx4 + 1;
             ary_HLOrd(idx4) = wk_GDORD#;
           ENDIF;                                                            // idx4a=*Zero
         ENDIF;                                                              // wk_GDLBL2='N'
       ENDIF;                                                                // wk_HazLbl and
      /End-Free
     C                   IF        wk_NewPallet
     C                   IF        wk_BOL#2 = *Zero
     C                   EVAL      wk_BOL#2 = SCBOL#
     C                   ELSE
     C                   IF        SCBOL# <> wk_BOL#2
     C                   EVAL      wk_BOL#2 = -1
     C                   ENDIF                                                  SCBOL#<>wk_BOL#2
     C                   ENDIF                                                  wk_BOL#2=*Zero
     C                   ENDIF                                                  wk_NewPallet
     C                   ENDDO                                                  %eof(HYLSSCC8)

     C                   IF        not wk_MPScanned
     C                   EVAL      wk_AllUpd = *ON
     C                   ENDIF                                                  not wk_MPScanned

     C                   ENDDO                                                  wk_AllUpd

     C                   EVAL      wk_TotCtn = wk_TotCtn + 1
     C                   EVAL      F3TCTN = wk_TotCtn

     C                   IF        wk_NewPallet

     C                   IF        wk_BOL#2 = -1
     C                   EVAL      wk_BOL# = *Zero
     C                   ELSE
     C                   EVAL      wk_BOL# = wk_BOL#2
     C                   ENDIF                                                  wk_BOL#2=-1

     C                   EVAL      PMPSRL# = wk_Pallet#
     C                   EVAL      PMSHTO = wk_ShipTo#
     C                   EVAL      PMDSHP = wk_DropShip#
     C                   EVAL      PMBOL# = wk_BOL#
     C                   EVAL      PMINVC = *Blank
     C                   EVAL      PMSRC = 'P'
     C                   EVAL      PMHZLB = wk_HazLblCtl
     C                   EVAL      PMEMTY = wk_EmpType
     C                   EVAL      PMEMPL = wk_Emply#
     C                   EVAL      PMOSTP = %timestamp
     C                   EVAL      PMTSTP = PMOSTP
     C                   EVAL      PMLEN = DftPLength
     C                   EVAL      PMWID = DftPWidth
     C                   WRITE     HYRPLLT

     C                   ELSE

     C     wk_Pallet#    CHAIN     HYPPLLT
     C                   IF        %found(HYPPLLT)
     C                   EVAL      PMHZLB = wk_HazLblCtl
     C                   EVAL      PMTSTP = %timestamp
     C                   UPDATE    HYRPLLT
     C                   ENDIF                                                  %found(HYPPLLT)

     C                   ENDIF                                                  wk_NewPallet

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Add informational data to DSPFMT3B.                                    *
      **************************************************************************
     CSR   $MoveToF3     BEGSR

     C                   EVAL      F3PSRL# = %triml(%editc(wk_Pallet#:'Z'))
     C                   EVAL      F3CUST = wk_ShipTo#
     C                   IF        F3CUST = CMCUST
     C                   EVAL      F3NAME = CMCNA1
     C                   EVAL      wk_BillTo# = CMBLTO
     C                   ELSE
     C     F3CUST        CHAIN     HDCUST
     C                   IF        %found(HDCUST)
     C                   EVAL      F3NAME = CMCNA1
     C                   EVAL      wk_BillTo# = CMBLTO
     C                   ELSE
     C                   EVAL      F3NAME = *Blanks
     C                   EVAL      wk_BillTo# = F3CUST
     C                   ENDIF                                                  %found(HDCUST)
     C                   ENDIF                                                  F3CUST=CMCUST
     C     PMBOL#        CHAIN     OEBOLH
     C                   IF        %found(OEBOLH)
     C                   EVAL      F3PRO# = SDPRO#
     C                   ELSE
     C                   EVAL      F3PRO# = *Blanks
     C                   ENDIF                                                  %found(OEBOLH)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check if shipment has been totally scanned.                            *
      **************************************************************************
     CSR   $ChkAllScan   BEGSR

     C                   EVAL      wk_Error = *OFF

     C****               EVAL      wk_Order# = ChkNotPltz1(wk_Pallet#4)
     C                   IF        wk_ChkType = '1'
     C                   EVAL      wk_ForceF21 = *ON
     C                   ELSE
     C                   EVAL      wk_ForceF21 = *OFF
     C                   ENDIF                                                  wk_ChkType='1'
     C                   CALLP(E)  HYR0156 (wk_Pallet#4:wk_Order#X:wk_Turn#X:
     C                               *Zero:*ON:wk_ForceF21:*OFF:wk_RC0156)
     C                   EVAL      wk_Pgm0156 = *ON
     C                   IF        wk_RC0156 <> *Zeros
     C                   EVAL      *IN93 = *OFF
     C                   SELECT
     C                   WHEN      wk_RC0156 = '01'
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYPPLLT'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %editc(wk_Pallet#4:'X')
     C                   EXSR      $SNDPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   WHEN      wk_RC0156 = '05'
     C                   EVAL      wk_MsgID = 'ERR0828'
     C                   EVAL      wk_MsgData = 'HYR0156'
     C                   EVAL      %subst(wk_MsgData:16:4) = wk_RC0156
     C                   EXSR      $SNDPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   WHEN      wk_RC0156 = '12'
     C                   EVAL      wk_MsgID = 'ERR0826'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_Pallet#4:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:8) = '*N'
     C                   EXSR      $SNDPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN12 = *ON
     C                   EVAL      wk_WdwRfrsh = *ON
     C                   WHEN      wk_RC0156 = '21'
     C                   IF        not %open(HYPMILG)
     C                   OPEN      HYPMILG
     C                   ENDIF                                                  not %open(HYPMILG)
     C                   EVAL      MLLGTY = 'WMB1'                              Wrn msg bypassed
     C                   EVAL      MLDATA = 'Pallet# ' +
     C                               %triml(%editc(wk_Pallet#4:'Z'))
     C                   EVAL      MLMSG = 'User ignored warning that all +
     C                               ctns might not have been scanned.'
     C                   EVAL      MLPGM = ps_Pgnm
     C                   EVAL      MLUSER = ps_User
     C                   EVAL      MLWS = ps_Job
     C                   EVAL      MLTSTP = %timestamp()
     C                   WRITE     HYRMILG
     C                   EVAL      wk_WdwRfrsh = *ON
     C                   WHEN      wk_RC0156 = '99'
     C                   EVAL      wk_WdwRfrsh = *ON
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9013'
     C                   EVAL      wk_MsgData = 'HYR0156'
     C                   EVAL      %subst(wk_MsgData:16:5) = wk_RC0156
     C                   EXSR      $SNDPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_WdwRfrsh = *ON
     C                   ENDSL
     C                   ELSE
     C                   IF        wk_ChkType = '2'
     C                   EVAL      W1PSRL# = %triml(%editc(wk_Pallet#4:'Z'))
     C                   IF        wk_WdwRfrsh
     C                   WRITE     DSPBLANK
     C                   EVAL      wk_WdwRfrsh = *OFF
     C                   ENDIF                                                  wk_WdwRfrsh
     C                   EXFMT     WINDOW01C
     C                   ENDIF                                                  wk_ChkType='2'
     C                   ENDIF                                                  wk_RC0156<>*Zeros

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display pallet summary.                                                *
      **************************************************************************
     CSR   $PalletSumm   BEGSR

     C                   IF        wk_PSrl# = -1

     C     SCORD#        CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   EVAL      OESHTO = *Zero
     C                   ENDIF                                                  not %found(OEORHD)

     C                   EVAL      F2PSRL# = '*NEW'
     C                   EVAL      F2CUST = OESHTO
     C                   IF        OESHTO <> CMCUST
     C     OESHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   CLEAR     *ALL          HDCUSTR
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  OESHTO<>CMCUST
     C                   EVAL      wk_BillTo# = CMBLTO
     C                   EVAL      wk_ShipToName = CMCNA1
     C                   EXSR      $SquishAddr
     C                   EVAL      F2ADR1 = CustAddr(1)
     C                   EVAL      F2ADR2 = CustAddr(2)
     C                   EVAL      F2ADR3 = CustAddr(3)
     C                   EVAL      F2ADR4 = CustAddr(4)
     C                   EVAL      F2NAME = CMCNA1
     C                   IF        CMCTRY = 'US'  or  CMCTRY = *Blanks
     C                   EVAL      F2CTRY = *Blanks
     C                   ELSE
     C     CMCTRY        CHAIN     HDCTRY
     C                   IF        %found(HDCTRY)
     C                   EVAL      F2CTRY = CNCDES
     C                   ELSE
     C                   EVAL      F2CTRY = CMCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   ENDIF                                                  CMCTRY='US'
     C                   EVAL      F2PHON = CMPHON
     C                   EVAL      F2CTN = *Zero
     C                   EVAL      F2WGT = *Zero
     C                   EVAL      F2BOL# = SCBOL#
     C                   IF        SCBOL# = *Zero
     C                   EVAL      F2PRO# = *Blanks
     C                   EVAL      F2SHPV = wk_ShipVia
     C                   EVAL      F2SVDS = wk_ShipViaDesc
     C                   ELSE
     C     SCBOL#        CHAIN     OEBOLH
     C                   IF        %found(OEBOLH)
     C                   EVAL      F2PRO# = SDPRO#
     C                   EVAL      F2SHPV = SDSHPV
     C                   EVAL      F2SVDS = SDSVDS
     C                   ELSE
     C                   EVAL      F2PRO# = *Blanks
     C                   EVAL      F2SHPV = *Blanks
     C                   EVAL      F2SVDS = *Blanks
     C                   ENDIF                                                  %found(OEBOLH)
     C                   ENDIF                                                  SCBOL#=*Zero

     C                   ELSE

     C     wk_PSrl#      CHAIN(N)  HYPPLLT
     C                   IF        not %found(HYPPLLT)
     C                   CLEAR     *ALL          HYRPLLT
     C                   ENDIF                                                  not %found(HYPPLLT)

     C                   EVAL      F2PSRL# = %triml(%editc(wk_PSrl#:'Z'))
     C                   EVAL      F2CUST = PMSHTO
     C                   IF        PMSHTO <> CMCUST
     C     PMSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   CLEAR     *ALL          HDCUSTR
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  PMSHTO<>CMCUST
     C                   EVAL      wk_ShipToName = CMCNA1
     C                   EXSR      $SquishAddr
     C                   EVAL      F2ADR1 = CustAddr(1)
     C                   EVAL      F2ADR2 = CustAddr(2)
     C                   EVAL      F2ADR3 = CustAddr(3)
     C                   EVAL      F2ADR4 = CustAddr(4)
     C                   EVAL      F2NAME = CMCNA1
     C                   IF        CMCTRY = 'US'  or  CMCTRY = *Blanks
     C                   EVAL      F2CTRY = *Blanks
     C                   ELSE
     C     CMCTRY        CHAIN     HDCTRY
     C                   IF        %found(HDCTRY)
     C                   EVAL      F2CTRY = CNCDES
     C                   ELSE
     C                   EVAL      F2CTRY = CMCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   ENDIF                                                  CMCTRY='US'
     C                   EVAL      F2PHON = CMPHON
     C                   EVAL      wk_Pallet#2 = wk_PSrl#
     C                   EXSR      $RtvPltTots
     C                   EVAL      F2CTN = wk_TotCtn
     C                   EVAL(H)   F2WGT = wk_TotWgt
     C                   EVAL      F2BOL# = PMBOL#
     C                   IF        PMBOL# = *Zero
     C                   EVAL      F2PRO# = *Blanks
     C                   EVAL      F2SHPV = wk_ShipVia
     C                   EVAL      F2SVDS = wk_ShipViaDesc
     C                   ELSE
     C     PMBOL#        CHAIN     OEBOLH
     C                   IF        %found(OEBOLH)
     C                   EVAL      F2PRO# = SDPRO#
     C                   EVAL      F2SHPV = SDSHPV
     C                   EVAL      F2SVDS = SDSVDS
     C                   ELSE
     C                   EVAL      F2PRO# = *Blanks
     C                   EVAL      F2SHPV = *Blanks
     C                   EVAL      F2SVDS = *Blanks
     C                   ENDIF                                                  %found(OEBOLH)
     C                   ENDIF                                                  PMBOL#=*Zero

     C                   ENDIF                                                  wk_PSrl#=-1

     C                   EVAL      F2HDG2 = center(F2HDG2)
     C                   SELECT
     C                   WHEN      wk_EmpType = 'P'
     C                   EVAL      F2EMP = EMRNAM
     C                   WHEN      wk_EmpType = 'T'
     C                   IF        TEMIDI = *Blank
     C                   EVAL      F2EMP = %trimr(TEFNAM) + ' ' + TELNAM
     C                   ELSE
     C                   EVAL      F2EMP = %trimr(TEFNAM) + ' ' + TEMIDI + ' ' +
     C                               TELNAM
     C                   ENDIF                                                  TEMIDI=*Blank
     C                   ENDSL
     C                   IF        wk_ReqF11
     C                   EVAL      F2INST = PressF11
     C                   EVAL      F2FKEYS =  F3Txt + '   ' + F11Txt + '   ' +
     C                               F12Txt
     C                   ELSE
     C                   EVAL      F2INST = PressEnter
     C                   EVAL      F2FKEYS =  F3Txt + '   ' + F12Txt
     C                   ENDIF                                                  wk_ReqF11

     C                   IF        wk_PgmMode = InqMode
     C                   EVAL      *IN81 = *OFF
     C                   ELSE
     C                   EVAL      *IN81 = *ON
     C                   ENDIF                                                  wk_PgmMode=InqMode

     C                   DOW       not *IN03

     C                   IF        wk_ReqF11
     C                   EVAL      *IN11 = *ON
     C                   ELSE
     C                   EVAL      *IN11 = *OFF
     C                   ENDIF                                                  wk_ReqF11

     C                   WRITE     MSFCTL01
     C                   EXFMT     DSPFMT2A
     C                   MOVE      *ON           *IN93
     C                   EXSR      $RMVPM
     C                   EVAL      *IN40 = *OFF

     C                   IF        *IN25

     C                   SELECT
     C                   WHEN      *IN03
     C                   IF        wk_ChkPrtl  and  wk_Pallet# <> *Zero
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '1'
     C                   EXSR      $ChkAllScan
     C                   IF        wk_Error
     C                   EVAL      *IN03 = *OFF
     C                   ITER
     C                   ENDIF                                                  wk_Error
     C                   ENDIF                                                  wk_ChkPrtl and
     C                   LEAVE
     C                   WHEN      *IN12
     C                   LEAVE
     C                   WHEN      *IN11
     C                   LEAVE
     C                   ENDSL

     C                   ELSE

     C                   IF        wk_ReqF11
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'GUM0031'
     C                   EVAL      wk_MsgData = 'F11'
     C                   EXSR      $SndPM
     C                   ITER
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  wk_ReqF11

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process "Pallet" subfile.                                              *
      **************************************************************************
     CSR   $Pallet       BEGSR

     C                   SELECT
     C                   WHEN      wk_Subr1Mode = MaintMode                     Maintenance mode
     C                   EVAL      C1HDG2 = *Blanks
     C                   EVAL      C1OPTS = OptDsc1A
     C                   EVAL      C1OPTS2 = OptDsc2 + '   ' + OptDsc2A
     C                   EVAL      wk_AlwF3 = *ON
     C                   WHEN      wk_Subr1Mode = InqMode                       Inquiry mode
     C                   EVAL      C1HDG2 = *Blanks
     C                   EVAL      C1OPTS = OptDsc1B
     C                   EVAL      C1OPTS2 = OptDsc2
     C                   EVAL      wk_AlwF3 = *ON
     C                   WHEN      wk_Subr1Mode = SelMode                       Selection mode
     C                   EVAL      C1HDG2 = C1HDG2A
     C                   EVAL      C1OPTS = OptDsc1C
     C                   EVAL      C1OPTS2 = OptDsc2
     C                   EVAL      wk_SelMade = *OFF
     C                   EVAL      wk_AlwF3 = *ON
     C                   ENDSL

     C                   EVAL      C1PSRL# = *Blanks

     C                   EXSR      $PageDownSf1

     C                   DOW       not *IN03

      *
      * Display Subfile
      *
     C                   WRITE     MSFCTL01
      *
      * Check for No Records to Display
      *
     C                   IF        wk_Sfl01RRN > *Zero
     C                   EVAL      *IN27 = *ON
     C                   ENDIF                                                  wk_Sfl01RRN>*Zero

     C                   EVAL      *IN26 = *OFF
     C                   EVAL      *IN03 = wk_AlwF3

     C                   WRITE     SFLCMD01
     C                   EXFMT     SFLCTL01

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN89 = *OFF
     C                   EXSR      $RMVPM
      *
      * Valid Command Key?
      *
     C                   IF        *IN25

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   IF        wk_Subr1Mode = SelMode  and                  Selection mode
     C                               wk_Pallet# <> *Zero
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '1'
     C                   EXSR      $ChkAllScan
     C                   IF        wk_Error
     C                   EVAL      *IN03 = *OFF
     C                   ITER
     C                   ENDIF                                                  wk_Error
     C                   ENDIF                                                  wk_Subr1Mode=SelMode
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Check for function key use with options present.
      *
     C                   EVAL      wk_Error = *OFF

     C                   IF        *IN17  or  *IN18  or  *IN20  or  *IN91  or
     C                                *IN92

     C                   IF        wk_Sfl01RRN <> *Zero
      *
      * Edit options if subfile has records.
      *
     C                   EXSR      $EditSf1
      *
      * If function key pressed, and options are present, cannot do
      * function, send error message.
      *
     C                   IF        wk_Error
     C                   IF        *IN17  or  *IN18  or  *IN20
     C                   EVAL      wK_MsgID = 'GUM0005'
     C                   ELSE
     C                   EVAL      wK_MsgID = 'GUM0004'
     C                   ENDIF                                                  *IN17 or *IN18
     C                   EVAL      wk_MsgData = *Blanks
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_Error
      *
     C                   ENDIF                                                  wk_Sfl01RRN<>*Zero

     C                   ENDIF                                                  *IN17 or *IN18 or

     C                   IF        not wk_Error

     C                   SELECT

      *
      * Page Down Key?
      *
     C                   WHEN      *IN91
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN91 = *OFF

      *
      * Page Up Key?
      *
     C                   WHEN      *IN92
     C                   EXSR      $PageUpSf1
     C                   EVAL      *IN92 = *OFF

      *
      * Refresh?
      *
     C                   WHEN      *IN05
     C                   EXSR      $PageDownSF1
     C                   EVAL      *IN05 = *OFF

      *
      * Top?
      *
     C                   WHEN      *IN17
     C                   EXSR      $Top1
     C                   EVAL      *IN17 = *OFF

      *
      * Bottom?
      *
     C                   WHEN      *IN18
     C                   EXSR      $Bottom1
     C                   EVAL      *IN18 = *OFF

      *
      * Subset?
      *
     C                   WHEN      *IN20
     C                   EXSR      $Subset1
     C                   EVAL      *IN20 = *OFF

     C                   ENDSL

      *
      * Home Key pressed?, set cursor on position field.
      *
     C                   IF        *IN94
     C                   EVAL      *IN42 = *ON
     C                   ENDIF                                                  *IN94

     C                   ENDIF                                                  not wk_Error

     C                   ELSE

      *
      * Enter Key
      *
      * If no records in subfile and not in selection mode, return to calling subroutine.
      *
     C                   IF        wk_Sfl01RRN = *Zero  and
     C                               wk_Subr1Mode <> SelMode
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

      *
      * Edit subfile options.
      *
     C                   EXSR      $EditSf1

      *
      * If errors are detected, sound alarm and redisplay subfile.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * If options are present, read and process subfile.
      *
     C                   IF        wk_OptPr
     C                   EXSR      $ProcessSf1
     C                   ENDIF                                                  wk_OptPr

      *
      * If subroutine is in selection mode and a selection has been made,
      * return to calling subroutine.
      *
     C                   IF        wk_Subr1Mode = SelMode  and  wk_SelMade
     C                   LEAVE
     C                   ENDIF                                                  wk_Subr1Mode=SelectM

      *
      * If position-to requested, refill subfile.
      *
     C                   IF        C1PSRL# <> *Blanks
     C****               IF        %check(Digits:%trim(C1PSRL#)) > *Zero  or
     C****                           (%len(%trim(C1PSRL#)) = 20  and
     C****                            %subst(C1PSRL#:1:10) <> '0000090255')  or
     C****                           (%len(%trim(C1PSRL#)) > 9  and
     C****                            %len(%trim(C1PSRL#)) < 20)  or
     C****                            %subst(C1PSRL#:21:1) <> ' '
     C                   IF        %check(Digits:%trim(C1PSRL#)) > *Zero  or
     C                               (%len(%trim(C1PSRL#)) = 20  and
     C                                (%subst(C1PSRL#:1:4) <> '0000'  or
     C                                 (%subst(C1PSRL#:5:6) <> wk_GS1Comp  and
     C                                  %subst(C1PSRL#:5:6) <> wk_GS1Comp2  and
     C                                  %subst(C1PSRL#:5:6) <> wk_GS1Comp3))) or
     C                               (%len(%trim(C1PSRL#)) > 9  and
     C                                %len(%trim(C1PSRL#)) < 20)  or
     C                                %subst(C1PSRL#:21:1) <> ' '
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(C1PSRL#)
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  %check(Digits:%trim(
     C                   IF        %len(%trim(C1PSRL#)) < 10
     C                   EVAL      wk_Serial# = %dec(%trim(C1PSRL#):9:0)
     C                   ELSE
     C                   EVAL      wk_Serial# = %dec(%subst(C1PSRL#:11:9):9:0)
     C                   ENDIF                                                  %len(%trim(C1PSRL#))
     C                   EVAL      @UPSRL# = wk_Serial#
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF
     C                   EVAL      wk_Refresh = *OFF
     C                   EVAL      C1PSRL# = *Blanks
     C                   ENDIF                                                  C1PSRL#<>*Blanks

     C                   ENDIF                                                  *IN25=*ON

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page down subfile SFL01 - pallet subfile.                             *
      **************************************************************************
     CSR   $PageDownSf1  BEGSR

      *
      * If no records, clear subfile and initialize relative record and page count.
      *
     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C     *Loval        SETLL     HYLPLLT1
     C                   OTHER
     C     *Loval        SETLL     HYPPLLT
     C                   ENDSL
     C                   IF        not %found

     C                   EXSR      $ClearSfl01

     C                   IF        *IN91
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ENDIF                                                  *IN91
     C                   EVAL      S@MORE = 'Bottom'
     C                   LEAVESR

     C                   ELSE

      *
      * Set starting point in queue
      *   If Page Down, start at next page
      *   If Refresh, start at top
      *   If Search, start at position to...
      *   Otherwise, initial build or page up, start at BOF
      *
     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY1       SETGT     HYLPLLT1
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY1       SETLL     HYLPLLT1
     C                   OTHER                                                  Enter,...
     C     *Loval        SETLL     HYLPLLT1
     C                   ENDSL
     C                   DOU       %eof(HYLPLLT1)  or  wk_Include
     C                   READ      HYLPLLT1
     C                   IF        not %eof(HYLPLLT1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLPLLT1)
     C                   ENDDO                                                  %eof(HYLPLLT1) or
     C                   OTHER
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY1       SETGT     HYPPLLT
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY1       SETLL     HYPPLLT
     C                   OTHER                                                  Enter,...
     C     *Loval        SETLL     HYPPLLT
     C                   ENDSL
     C                   DOU       %eof(HYPPLLT)  or  wk_Include
     C                   READ(N)   HYPPLLT
     C                   IF        not %eof(HYPPLLT)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYPPLLT)
     C                   ENDDO                                                  %eof(HYPPLLT) or
     C                   ENDSL
     C                   ENDIF                                                  not %found

      *
      * If EOF, send Bottom of the List message
      *
     C                   IF        %eof
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM

      *
      * If refresh, get last record.
      *
     C                   IF        *IN05
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C     *Hival        SETGT     HYLPLLT1
     C                   DOU       %eof(HYLPLLT1)  or  wk_Include
     C                   READP     HYLPLLT1
     C                   IF        not %eof(HYLPLLT1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLPLLT1)
     C                   ENDDO                                                  %eof(HYLPLLT1) or
     C                   OTHER
     C     *Hival        SETGT     HYPPLLT
     C                   DOU       %eof(HYPPLLT)  or  wk_Include
     C                   READP(N)  HYPPLLT
     C                   IF        not %eof(HYPPLLT)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYPPLLT)
     C                   ENDDO                                                  %eof(HYPPLLT) or
     C                   ENDSL
     C                   ENDIF                                                  *IN05

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   ENDIF                                                  %eof

     C                   DOW       (wk_PgmMode = EntryMode  and
     C                               not %eof(HYLPLLT1)  or
     C                              wk_PgmMode <> EntryMode  and
     C                               not %eof(HYPPLLT))

      *
      * If subfile is full, quit.
      *
     C                   IF        wk_Sfl01PC = wk_Sfl01PS
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01PC=wk_Sfl01P

      *
      * Blank Option Field.  Set off SFLNXTCHG.
      *
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF

      *
      * Move Database Fields to Screen
      *
     C                   EXSR      $MoveToSfl01

      *
      * Increment Page Counter, Rel Rec Nbr
      *
     C                   EVAL      wk_Sfl01PC = wk_Sfl01PC + 1
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN + 1

     C                   WRITE     SFL01

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01PC = 1
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UPSRL# = PMPSRL#
     C                   ENDIF                                                  wk_Sfl01PC=1
     C                   EVAL      @DPSRL# = PMPSRL#

      *
      * Get next pallet master record.
      *
     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C                   DOU       %eof(HYLPLLT1)  or  wk_Include
     C                   READ      HYLPLLT1
     C                   IF        not %eof(HYLPLLT1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLPLLT1)
     C                   ENDDO                                                  %eof(HYLPLLT1) or
     C                   OTHER
     C                   DOU       %eof(HYPPLLT)  or  wk_Include
     C                   READ(N)   HYPPLLT
     C                   IF        not %eof(HYPPLLT)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYPPLLT)
     C                   ENDDO                                                  %eof(HYPPLLT) or
     C                   ENDSL

     C                   ENDDO                                                  wk_PgmMode=EntryMode

      *
      * Set More... or Bottom   subfile message.
      *
     C                   IF        (wk_PgmMode = EntryMode  and
     C                               %eof(HYLPLLT1)  or
     C                              wk_PgmMode <> EntryMode  and
     C                               %eof(HYPPLLT))
     C                   EVAL      S@MORE = 'Bottom'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  wk_PgmMode=EntryMode

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear subfile 01, initialize relative record and page count.           *
      **************************************************************************
     CSR   $ClearSfl01   BEGSR

     C                   EVAL      *IN26 = *ON
     C                   EVAL      *IN27 = *OFF
     C                   WRITE     SFLCTL01
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero
     C                   EVAL      wk_Sfl01PC = *Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page up subfile SFL01 - pallet subfile.                               *
      **************************************************************************
     CSR   $PageUpSf1    BEGSR

     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C     PGUKEY1       SETLL     HYLPLLT1
     C                   DOU       %eof(HYLPLLT1)  or  wk_Include
     C                   READP     HYLPLLT1
     C                   IF        not %eof(HYLPLLT1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLPLLT1)
     C                   ENDDO                                                  %eof(HYLPLLT1) or
     C                   OTHER
     C     PGUKEY1       SETLL     HYPPLLT
     C                   DOU       %eof(HYPPLLT)  or  wk_Include
     C                   READP(N)  HYPPLLT
     C                   IF        not %eof(HYPPLLT)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYPPLLT)
     C                   ENDDO                                                  %eof(HYPPLLT) or
     C                   ENDSL

      *
      * If BOF, send Top of the List message
      *
     C                   IF        %eof

     C                   EVAL      wk_MsgID = 'GUM0003'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01PS + 1

     C                   ENDIF                                                  %eof

     C                   DOW       (wk_PgmMode = EntryMode  and
     C                               not %eof(HYLPLLT1)  or
     C                              wk_PgmMode <> EntryMode  and
     C                               not %eof(HYPPLLT))

      *
      * If subfile is full, quit.
      *
     C                   IF        wk_Sfl01PC = wk_Sfl01PS
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01PC=wk_Sfl01P

      *
      * Blank Option Field.  Set off SFLNXTCHG.
      *
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF

      *
      * Move Database Fields to Screen
      *
     C                   EXSR      $MoveToSfl01

      *
      * Increment Page Counter, Decrement Rel Rec Nbr
      *
     C                   EVAL      wk_Sfl01PC = wk_Sfl01PC + 1
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN - 1

     C                   WRITE     SFL01

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01RRN = wk_Sfl01PS
     C                   EVAL      @DPSRL# = PMPSRL#
     C                   ENDIF                                                  wk_Sfl01RRN=wk_Sfl01
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UPSRL# = PMPSRL#

      *
      * Get previous pallet master record.
      *
     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C                   DOU       %eof(HYLPLLT1)  or  wk_Include
     C                   READP     HYLPLLT1
     C                   IF        not %eof(HYLPLLT1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLPLLT1)
     C                   ENDDO                                                  %eof(HYLPLLT1) or
     C                   OTHER
     C                   DOU       %eof(HYPPLLT)  or  wk_Include
     C                   READP(N)  HYPPLLT
     C                   IF        not %eof(HYPPLLT)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYPPLLT)
     C                   ENDDO                                                  %eof(HYPPLLT) or
     C                   ENDSL

     C                   ENDDO                                                  wk_PgmMode=EntryMode

      *
      * Set More... or Top if there are more records or BOF.
      * If Beg of File reached and not a full page, fill from top down.
      *
     C                   IF        (wk_PgmMode = EntryMode  and
     C                               %eof(HYLPLLT1)  or
     C                              wk_PgmMode <> EntryMode  and
     C                               %eof(HYPPLLT))
     C                   IF        wk_Sfl01PC <> wk_Sfl01PS
     C                   EVAL      @UPSRL# = *Loval
     C                   EXSR      $PageDownSf1
     C                   ENDIF                                                  wk_Sfl01PC<>wk_Sfl01
     C                   EVAL      S@MORE = 'Top'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  wk_PgmMode=EntryMode

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move database fields to subfile fields.                                *
      **************************************************************************
     CSR   $MoveToSfl01  BEGSR

     C                   IF        PMSHTO <> CMCUST
     C     PMSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCUST = *Zero
     C                   EVAL      CMCNA1 = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  PMSHTO<>CMCUST

     C                   EVAL      S1SRL# = PMPSRL#
     C                   EVAL      S1SHTO = PMSHTO
     C                   EVAL      S1STNM = CMCNA1
     C                   IF        PMDSHP = *Zero
     C                   EVAL      S1DS = *Blanks
     C                   ELSE
     C                   EVAL      S1DS = 'DS'
     C                   ENDIF                                                  PMDSHP=*Zero
     C                   EVAL      S1BOL# = PMBOL#
     C                   EVAL      wk_Pallet#2 = PMPSRL#
     C                   EXSR      $RtvPltTots
     C                   EVAL      S1CTN = wk_TotCtn
     C                   EVAL(H)   S1WGT = wk_TotWGT

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL01 "pallet" subfile.                                         *
      **************************************************************************
     CSR   $Filter1      BEGSR

     C                   EVAL      wk_Include = *ON

      *
      * Domestic/Foreign customer subset.
      *
     C                   IF        F6DF <> *Blank
     C                   IF        PMSHTO <> CMCUST
     C     PMSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCUST = *Zero
     C                   EVAL      CMBLTO = *Zero
     C                   EVAL      CMCCLS = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  PMSHTO<>CMCUST
      /Free
       IF wk_BTCCLS# <> CMBLTO;
         wk_BTCCLS = *Blanks;
         EXEC SQL select CMCCLS into :wk_BTCCLS
                  from HDCUST
                  where CMCUST=:CMBLTO;
         wk_BTCCLS# = CMBLTO;
       ENDIF;                                                                // wk_BTCCLS#<>CMBLTO
      /End-Free
     C                   IF        wk_FrgnChkCust <> PMSHTO
     C****               IF        CMCCLS = 'RF'
     C                   IF        wk_BTCCLS = 'RF'
     C                   EVAL      wk_Foreign = *ON
     C                   ELSE
     C                   EVAL      kf_Table = 'CUSTCLASS'
     C****               EVAL      kf_Key1 = CMCCLS
     C                   EVAL      kf_Key1 = wk_BTCCLS
     C     key_TD        CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)  and  TDFF02 = 'Y'
     C                   EVAL      wk_Foreign = *ON
     C                   ELSE
     C                   EVAL      wk_Foreign = *OFF
     C                   ENDIF                                                  %found(GUPTDAT) and
     C                   ENDIF                                                  CMCCLS='RF'
     C                   EVAL      wk_FrgnChkCust = PMSHTO
     C                   ENDIF                                                  wk_FrgnChkCust=PMSHT
     C                   IF        (F6DF = 'D'  and  wk_Foreign)  or
     C                               (F6DF = 'F'  and  not wk_Foreign)
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  F6DF='D' and
     C                   ENDIF                                                  F6DF<>*Blank

      *
      * Customer number subset.
      *
     C                   IF        F6SHTO <> *Zero
     C                   IF        PMSHTO <> F6SHTO
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  PMSHTO<>F6SHTO
     C                   ENDIF                                                  F6SHTO<>*Zero

      *
      * Drop ship yes/no subset.
      *
     C                   IF        F6DS <> *Blank
     C                   IF        (F6DS = 'Y'  and  PMDSHP = *Zero)  or
     C                               (F6DS = 'N'  and  PMDSHP <> *Zero)
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  F6DS='Y' and PMDSHP
     C                   ENDIF                                                  F6DS<>*Blank

      *
      * BOL number subset.
      *
     C                   IF        F6BOL# <> *Zero
     C                   IF        PMBOL# <> F6BOL#
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  PMBOL#<>F6BOL#
     C                   ENDIF                                                  F6BOL#<>*Zero

      *
      * Pro number subset.
      *
     C                   IF        F6PRO# <> *Blanks
     C                   IF        PMBOL# = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  PMBOL#=*Zero
     C                   IF        PMBOL# <> SDBOL#
     C     PMBOL#        CHAIN     OEBOLH
     C                   IF        not %found(OEBOLH)
     C                   EVAL      SDBOL# = *Zero
     C                   EVAL      SDPRO# = *Blanks
     C                   ENDIF                                                  not %found(OEBOLH)
     C                   ENDIF                                                  PMBOL#<>SDBOL#
     C                   IF        SDPRO# <> F6PRO#
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  SDPRO#<>F6PRO#
     C                   ENDIF                                                  F6PRO#<>*Blanks

      *
      * Ship via subset.
      *
     C                   IF        F6SHPV <> *Blanks
     C                   IF        PMBOL# = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  PMBOL#=*Zero
     C                   IF        PMBOL# <> SDBOL#
     C     PMBOL#        CHAIN     OEBOLH
     C                   IF        not %found(OEBOLH)
     C                   EVAL      SDBOL# = *Zero
     C                   EVAL      SDSHPV = *Blanks
     C                   ENDIF                                                  not %found(OEBOLH)
     C                   ENDIF                                                  PMBOL#<>SDBOL#
     C                   IF        SDSHPV <> F6SHPV
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  SDSHPV<>F6SHPV
     C                   ENDIF                                                  F6SHPV<>*Blanks

      *
      * Carton UCC-128 subset.
      *
     C                   IF        F6SRL# <> *Blanks
     C                   IF        PMPSRL# <> wk_SSPallet#
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  PMPSRL#<>wk_SSPallet
     C                   ENDIF                                                  F6SRL#<>*Blanks

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Edit options subfile SFL01 - pallet subfile.                          *
      **************************************************************************
     CSR   $EditSf1      BEGSR

      *
      *  Reset indicators.
      *
     C                   EVAL      wk_Error = *OFF
     C                   EVAL      wk_SelPr = *OFF
     C                   EVAL      wk_DltPr = *OFF
     C                   EVAL      wk_DspPr = *OFF
     C                   EVAL      wk_PrtPr = *OFF
     C                   EVAL      wk_DtlPr = *OFF
     C                   EVAL      wk_LblPr = *OFF
     C                   EVAL      wk_AttrPr = *OFF
     C                   EVAL      wk_ChkMissPr = *OFF
     C                   EVAL      wk_BannerPr = *OFF
     C                   EVAL      wk_Banner2Pr = *OFF
     C                   EVAL      wk_PrtHzLbPr = *OFF
     C                   EVAL      wk_OptPr = *OFF
     C                   EVAL      wk_ClrDone = *OFF

      *
      * Save function key indicators.
      *
     C                   EVAL      wk_IN25 = *IN25

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01

     C                   DOW       not %eof(HYD0138)

      *
      * Reset error indicator and turn on SFLNXTCHG indicator.
      *
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN28 = *ON

      *
      * If a valid function key, turn on error indicator.
      *
     C                   IF        wk_IN25
     C                   EVAL      *IN41 = *ON
     C                   ENDIF                                                  wk_IN25

      *
      * Selects present?
      *
     C                   IF        (S1OPT = @SEL  or  S1OPT = @SEL2)
     C                               and  wk_Subr1Mode = SelMode

     C                   EVAL      S1OPT = @SEL

     C                   IF        wk_SelPr

      *
      *  Set Cursor on First Error
      *
     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   UPDATE    SFL01
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0008'
     C                   EXSR      $SndPM

     C                   ELSE
     C                   EVAL      wk_SelPr = *ON
     C                   ENDIF                                                  wk_SelPr
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Deletes present?
      *
     C                   IF        (S1OPT = @DLT  or  S1OPT = @DLT2)
     C                               and  wk_Subr1Mode = MaintMode

     C                   EVAL      S1OPT = @DLT

     C                   EVAL      wk_DltPr = *ON
     C                   UPDATE    SFL01

      *
      * If first delete, clear delete subfile and save first delete
      * subfile relative record number.
      *
     C                   IF        wk_ClrDone = *OFF
     C                   EVAL      wk_ClrDone = *ON
     C                   EVAL      wk_FirstDlt = wk_Sfl01RRN

      *
      *  Clear Subfile, Reset Rel Rec Nbr
      *
     C                   EVAL      *IN29 = *ON
     C                   WRITE     SFLCTL02
     C                   EVAL      *IN29 = *OFF
     C                   EVAL      wk_Sfl02RRN = *Zero
     C                   ENDIF                                                  wk_ClrDone=*OFF

      *
      *  Write to "delete" subfile.
      *
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   WRITE     SFL02

     C                   ELSE

      *
      * Displays present?
      *
     C                   IF        (S1OPT = @DSP  or  S1OPT = @DSP2)

     C                   EVAL      S1OPT = @DSP

     C                   EVAL      wk_DspPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Print reports present?
      *
     C                   IF        (S1OPT = @PRT  or  S1OPT = @PRT2)

     C                   EVAL      S1OPT = @PRT

     C                   EVAL      wk_PrtPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE


      *
      * Details present?
      *
     C                   IF        (S1OPT = @DTL  or  S1OPT = @DTL2)

     C                   EVAL      S1OPT = @DTL

     C                   EVAL      wk_DtlPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Print labels present?
      *
     C                   IF        (S1OPT = @LBL  or  S1OPT = @LBL2)

     C                   EVAL      S1OPT = @LBL

     C                   EVAL      wk_LblPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Change/display attributes present?
      *
     C                   IF        S1OPT = @ATTR

     C                   EVAL      wk_AttrPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Check missing scans present?
      *
     C                   IF        S1OPT = @CHKMISS

     C                   EVAL      wk_ChkMissPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Print banners present?
      *
     C                   IF        S1OPT = @BANNER

     C                   EVAL      wk_BannerPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Print FBI banners present?
      *
     C                   IF        S1OPT = @BANNER2

     C                   EVAL      wk_Banner2Pr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

      *
      * Print hazard labels present?
      *
     C                   IF        S1OPT = @PRTHZLB

     C                   EVAL      wk_PrtHzLbPr = *ON
     C                   UPDATE    SFL01

     C                   ELSE

     C                   IF        S1OPT <> *Blanks

      *
      *  Set Cursor on First Error
      *
     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   UPDATE    SFL01
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EXSR      $SndPM

     C                   ELSE

      *
      * Ignore Blank Options, Turn off Error & SFLNXTCHG
      *
     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   UPDATE    SFL01

     C                   ENDIF                                                   S1OPT<>*Blanks

     C                   ENDIF                                                   S1OPT=@PRTHZLB

     C                   ENDIF                                                   S1OPT=@BANNER2

     C                   ENDIF                                                   S1OPT=@BANNER

     C                   ENDIF                                                   S1OPT=@CHKMISS

     C                   ENDIF                                                   S1OPT=@ATTR or

     C                   ENDIF                                                   S1OPT=@LBL or

     C                   ENDIF                                                   S1OPT=@DTL or

     C                   ENDIF                                                   S1OPT=@PRT or

     C                   ENDIF                                                   S1OPT=@DSP or

     C                   ENDIF                                                   S1OPT=@DLT or

     C                   ENDIF                                                   S1OPT=@SEL or

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01

     C                   ENDDO                                                  not %eof(HYD0138)

      *
      * Turn off error indicator.
      *
     C                   EVAL      *IN41 = *OFF

      *
      * Restore function key indicators.
      *
     C                   EVAL      *IN25 = wk_IN25

      *
      * If options present, and Page Down or Page Up requested, set
      * error flag.
      *
     C                   IF        *IN25
     C                   IF        wk_SelPr  or  wk_DltPr  or  wk_DspPr  or
     C                               wk_PrtPr  or  wk_DtlPr  or  wk_LblPr  or
     C                               wk_AttrPr  or  wk_ChkMissPr  or
     C                               wk_BannerPr  or  wk_Banner2Pr
     C                               or  wk_PrtHzLBPr
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_SelPr or
     C                   ELSE
     C                   IF        wk_SelPr  or  wk_DltPr  or  wk_DspPr  or
     C                               wk_PrtPr  or  wk_DtlPr  or  wk_LblPr  or
     C                               wk_AttrPr  or  wk_ChkMissPr  or
     C                               wk_BannerPr  or  wk_Banner2Pr
     C                               or  wk_PrtHzLBPr
     C                   EVAL      wk_OptPr = *ON
     C                   ENDIF                                                  wk_SelPr or
     C                   ENDIF                                                  *IN25

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process options for subfile SFL01.                                     *
      **************************************************************************
     CSR   $ProcessSf1   BEGSR

     C                   EVAL      *IN12 = *OFF
     C                   EVAL      wk_WdwRfrsh = *OFF

     C                   READC     SFL01

     C                   DOW       not %eof(HYD0138)  and  not *IN03

      *
      *  F12=Cancel Key
      *
     C                   IF        not *IN12

     C                   SELECT

     C                   WHEN      S1OPT = @SEL
     C                   IF        wk_Pallet# <> *Zero  and
     C                               S1SRL# <> wk_Pallet#
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '1'
     C                   EXSR      $ChkAllScan
     C                   ENDIF                                                  wk_Pallet#<>*Zero
     C                   IF        wk_Pallet# = *Zero  or
     C                               S1SRL# = wk_Pallet#  or  not wk_Error
     C                   EVAL      wk_Pallet# = S1SRL#
     C                   EVAL      wk_SelMade = *ON
     C                   ENDIF                                                  wk_Pallet#=*Zero or
     C                   EVAL      *IN28 = *ON
     C                   UPDATE    SFL01

     C                   WHEN      S1OPT = @DSP
     C                   EVAL      wk_PSrl# = S1SRL#
     C                   EVAL      wk_ReqF11 = *OFF
     C                   IF        wk_PgmMode = EntryMode
     C                   EVAL      wk_ChkPrtl = *ON
     C                   ELSE
     C                   EVAL      wk_ChkPrtl = *OFF
     C                   ENDIF                                                  wk_PgmMode=EntryMode
     C                   EVAL      F2HDG2 = SummHdg2B
     C                   EXSR      $PalletSumm
     C                   IF        *IN12
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ENDIF                                                  *IN12
     C                   UPDATE    SFL01
     C                   EVAL      wk_WdwRfrsh = *ON

     C                   WHEN      S1OPT = @PRT
     C                   EVAL      wk_PSrl# = S1SRL#
     C     S1SHTO        CHAIN     HDCUST
     C                   IF        %found(HDCUST)
     C                   EVAL      wk_BillTo# = CMBLTO
     C                   ELSE
     C                   EVAL      wk_BillTo# = S1SHTO
     C                   ENDIF                                                  %found(HDCUST)
     C                   EVAL      wk_PlltPrtType = '*MNL'
     C                   EXSR      $PrtPallet
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   UPDATE    SFL01

     C                   WHEN      S1OPT = @DTL
     C                   EVAL      wk_Pallet#3 = S1SRL#
     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C                   IF        wk_Subr1Mode = SelMode                       Selection mode
     C                   EVAL      wk_Subr3Mode = InqMode                       Inquiry mode
     C                   ELSE
     C                   EVAL      wk_Subr3Mode = MaintMode1                    Maintenance mode 1
     C                   ENDIF                                                  wk_Subr1Mode=SelMode
     C                   WHEN      wk_PgmMode = MaintMode
     C                   EVAL      wk_Subr3Mode = MaintMode2                    Maintenance mode 2
     C                   OTHER
     C                   EVAL      wk_Subr3Mode = InqMode                       Inquiry mode
     C                   ENDSL
     C                   EXSR      $Carton
     C                   IF        *IN12
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ENDIF                                                  *IN12
     C                   UPDATE    SFL01
     C                   EVAL      wk_WdwRfrsh = *ON

     C                   WHEN      S1OPT = @LBL
     C                   EVAL      wk_PSrl# = S1SRL#
     C                   EXSR      $PrtLabels
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   UPDATE    SFL01

     C                   WHEN      S1OPT = @ATTR
     C                   EVAL      wk_PSrl# = S1SRL#
     C                   EXSR      $PalletAttr
     C                   IF        *IN12
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ENDIF                                                  *IN12
     C                   UPDATE    SFL01
     C                   EVAL      wk_WdwRfrsh = *ON

     C                   WHEN      S1OPT = @CHKMISS
     C                   EVAL      wk_Pallet#4 = S1SRL#
     C                   EVAL      wk_ChkType = '2'
     C                   EXSR      $ChkAllScan
     C                   IF        *IN12
     C                   EVAL      *IN28 = *ON
     C                   EVAL      *IN41 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ENDIF                                                  *IN12
     C                   UPDATE    SFL01
     C                   EVAL      *IN41 = *OFF

     C                   WHEN      S1OPT = @BANNER
     C                   EVAL      wk_PSrl# = S1SRL#
     C                   EXSR      $PrtBanner
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   UPDATE    SFL01

     C                   WHEN      S1OPT = @BANNER2
     C                   EVAL      wk_PSrl# = S1SRL#
     C                   EXSR      $PrtBanner2
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   UPDATE    SFL01

     C                   WHEN      S1OPT = @PRTHZLB
     C                   EVAL      wk_PSrl# = S1SRL#
     C                   EXSR      $PrtHazLbl1

     C                   WHEN      S1OPT = @DLT

      *
      * Keep "deletes" active - turn SFLNXTCHG on.
      *
     C                   EVAL      *IN28 = *ON
     C                   UPDATE    SFL01

     C                   ENDSL

     C                   EVAL      S@RCNO = wk_Sfl01RRN

     C                   ELSE

      *
      * Cancel requested - save remaining options.
      *
     C                   EVAL      *IN28 = *ON
     C                   UPDATE    SFL01

     C                   ENDIF                                                  not *IN12

      *
      * Get next changed subfile record (save status of F12=Cancel).
      *
     C                   IF        not %eof(HYD0138)  and  not *IN03
     C                   EVAL      wk_IN12 = *IN12
     C                   READC     SFL01
     C                   EVAL      *IN12 = wk_IN12
     C                   ENDIF                                                  not %eof(HYD0138)

     C                   ENDDO                                                  not %eof(HYD0138)

      *
      * If "deletes" are present and not F12=Cancel, process "deletes".
      *
     C                   IF        wk_DltPr  and  not *IN03  and  not *IN12
     C                   EVAL      S@RCNO = wk_FirstDlt
     C                   EXSR      $ProcDeletes
     C                   ENDIF                                                  wk_DltPr and

     C                   IF        not *IN03  and  not *IN12  and  wk_Refresh
     C                               and  C1PSRL# = *Blanks
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF
     C                   EVAL      wk_Refresh = *OFF
     C                   ENDIF                                                  not *IN03 and

     C                   EVAL      *IN12 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process pallet deletes.                                                *
      **************************************************************************
     CSR   $ProcDeletes  BEGSR

     C                   DOW       not *IN03  and  not *IN12
     C

      *
      * Format delete confirmation screen.
      *
     C                   WRITE     MSFCTL01
     C                   WRITE     SFLCMD02
     C                   EVAL      *IN29 = *OFF
     C                   EXFMT     SFLCTL02
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $RmvPM

      *
      * Valid function key?
      *
     C                   IF        *IN25

     C                   ELSE

      *
      * Read & process deletes, then exit.
      *
     C                   READC     SFL02

     C                   DOW       not %eof(HYD0138)

     C     S1SRL#        CHAIN     HYPPLLT
     C                   IF        %found(HYPPLLT)

     C     S1SRL#        SETLL     HYLSSCC9
     C                   DOU       %eof(HYLSSCC9)
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      *IN02 = *OFF
     C     S1SRL#        READE     HYLSSCC9
     C                   IF        %eof(HYLSSCC9)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC9)
     C                   EVAL      SCPSRL# = *Zero
     C                   EVAL      SCPEMPL = *Zero
     C                   SELECT
     C                   WHEN      *IN01
     C                   UPDATE    HYRSSCC9
     C                   WHEN      *IN02
     C                   UPDATE    HYRSCWK9
     C                   ENDSL
     C                   ENDDO                                                  %eof(HYLSSCC9)

     C                   DELETE    HYRPLLT
     C                   IF        not %open(HYPPMLG)
     C                   OPEN      HYPPMLG
     C                   ENDIF                                                  not %open(HYPPMLG)
     C                   EVAL      PMLACT = 'D'
     C                   EVAL      PMLEMTY = wk_EmpType
     C                   EVAL      PMLEMPL = wk_Emply#
     C                   EVAL      PMLSTP = %timestamp
     C                   WRITE     HYRPMLG
      *
      * Send "pallet deleted" message.
      *
     C                   EVAL      wk_MsgID = 'MSG0201'
     C                   EVAL      wk_MsgData = %triml(%editc(S1SRL#:'Z'))
     C                   EXSR      $SndPM

     C                   EVAL      wk_LastPallet# = S1SRL#
     C                   IF        RtvPRSts1 (wk_LastPallet#) = '*PRINT'
     C                   EVAL      wk_RC0139A =
     C                               WrtPRLog (wk_LastPallet#:'*RESET')
     C                   ENDIF                                                  RtvPRSts1 (wk_LastPa

     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYPPLLT'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %triml(%editc(S1SRL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Press F5 to refresh.'
     C                   EXSR      $SndPM
     C                   ENDIF                                                  %eof(HYPPLLT)

     C                   EVAL      wk_Refresh = *ON

     C                   READC     SFL02

     C                   ENDDO                                                  not %eof(HYD0138)

     C                   LEAVE

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03 and

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display/change pallet attributes.                                      *
      **************************************************************************
     CSR   $PalletAttr   BEGSR

     C     wk_PSrl#      CHAIN(N)  HYPPLLT
     C                   IF        not %found(HYPPLLT)
     C                   CLEAR     *ALL          HYRPLLT
     C                   ENDIF                                                  not %found(HYPPLLT)

     C                   EVAL      F8PSRL# = %triml(%editc(wk_PSrl#:'Z'))
     C                   EVAL      F8CUST = PMSHTO
     C                   IF        PMSHTO <> CMCUST
     C     PMSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   CLEAR     *ALL          HDCUSTR
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  PMSHTO<>CMCUST
     C                   EVAL      wk_ShipToName = CMCNA1
     C                   EXSR      $SquishAddr
     C                   EVAL      F8ADR1 = CustAddr(1)
     C                   EVAL      F8ADR2 = CustAddr(2)
     C                   EVAL      F8ADR3 = CustAddr(3)
     C                   EVAL      F8ADR4 = CustAddr(4)
     C                   EVAL      F8NAME = CMCNA1
     C                   IF        CMCTRY = 'US'  or  CMCTRY = *Blanks
     C                   EVAL      F8CTRY = *Blanks
     C                   ELSE
     C     CMCTRY        CHAIN     HDCTRY
     C                   IF        %found(HDCTRY)
     C                   EVAL      F8CTRY = CNCDES
     C                   ELSE
     C                   EVAL      F8CTRY = CMCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   ENDIF                                                  CMCTRY='US'

     C                   SELECT
     C                   WHEN      wk_EmpType = 'P'
     C                   EVAL      F8EMP = EMRNAM
     C                   WHEN      wk_EmpType = 'T'
     C                   IF        TEMIDI = *Blank
     C                   EVAL      F8EMP = %trimr(TEFNAM) + ' ' + TELNAM
     C                   ELSE
     C                   EVAL      F8EMP = %trimr(TEFNAM) + ' ' + TEMIDI + ' ' +
     C                               TELNAM
     C                   ENDIF                                                  TEMIDI=*Blank
     C                   ENDSL
     C                   EVAL      F8HGT = PMHGT
     C                   EVAL      sv_HGT = PMHGT
     C                   EVAL      F8LEN = PMLEN
     C                   EVAL      sv_LEN = PMLEN
     C                   EVAL      F8WID = PMWID
     C                   EVAL      sv_WID = PMWID

     C                   IF        wk_PgmMode = InqMode
     C                   EVAL      *IN81 = *OFF
     C                   ELSE
     C                   EVAL      *IN81 = *ON
     C                   ENDIF                                                  wk_PgmMode=InqMode

     C                   DOW       not *IN03

     C                   WRITE     MSFCTL01
     C                   IF        wk_PgmMode = InqMode
     C                   EXFMT     DSPFMT8B
     C                   ELSE
     C                   EXFMT     DSPFMT8A
     C                   ENDIF                                                  wk_PgmMode=InqMode
     C                   MOVE      *ON           *IN93
     C                   EXSR      $RMVPM
     C                   EVAL      *IN40 = *OFF

     C                   IF        *IN25

     C                   SELECT
     C                   WHEN      *IN03
     C                   IF        wk_PgmMode = EntryMode  and
     C                               wk_Pallet# <> *Zero
     C                   EVAL      wk_Pallet#4 = wk_Pallet#
     C                   EVAL      wk_ChkType = '1'
     C                   EXSR      $ChkAllScan
     C                   IF        wk_Error
     C                   EVAL      *IN03 = *OFF
     C                   ITER
     C                   ENDIF                                                  wk_Error
     C                   ENDIF                                                  wk_PgmMode=EntryMode
     C                   LEAVE
     C                   WHEN      *IN12
     C                   LEAVE
     C                   ENDSL

     C                   ELSE

     C                   IF        wk_PgmMode = InqMode
     C                   LEAVE
     C                   ENDIF                                                  wk_PgmMode=InqMode

     C                   IF        F8HGT = sv_HGT
     C                              and  F8LEN = sv_LEN
     C                              and  F8WID = sv_WID
     C                   LEAVE
     C                   ENDIF                                                  F8HGT=sv_HGT

     C                   EXSR      $EDIT8
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C     wk_PSrl#      CHAIN(E)  HYPPLLT
     C                   IF        %error
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'ERR9009'
     C                   EVAL      wk_MsgData = 'HYPPLLT'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %editc(wk_PSrl#:'X')
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Press F12 to cancel.'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        %found(HYPPLLT)
     C                   IF        PMHGT = sv_HGT
     C                              or PMLEN = sv_LEN
     C                              or PMWID = sv_WID
     C                   EVAL      PMHGT = F8HGT
     C                   EVAL      PMLEN = F8LEN
     C                   EVAL      PMWID = F8WID
     C                   UPDATE    HYRPLLT
     C                   EVAL      wk_MsgID = 'MSG0211'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_PSrl#:'Z'))
     C                   EXSR      $SndPM
     C                   LEAVE
     C                   ELSE
     C                   UNLOCK    HYPPLLT
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'ERR9004'
     C                   EVAL      wk_MsgData = 'HYPPLLT'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %editc(wk_PSrl#:'X')
     C                   EXSR      $SndPM
     C                   ENDIF                                                  PMHGT=sv_HGT
     C                   ELSE
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYPPLLT'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %editc(wk_PSrl#:'X')
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Press F12 to cancel.'
     C                   EXSR      $SndPM
     C                   ENDIF                                                  %found(HYPPLLT)
     C                   ENDIF                                                  %error

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate add carton to pallet screen.                                  *
      **************************************************************************
     CSR   $Edit8        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * If errors detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display subfile SFL01 from the top.                                    *
      **************************************************************************
     CSR   $Top1         BEGSR

     C                   EVAL      @UPSRL# = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      S@MORE = 'Top'

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Display subfile SFL01 from the bottom.                                 *
      **************************************************************************
     CSR   $Bottom1      BEGSR

     C                   EVAL      @UPSRL# = *Hival
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageUpSf1
     C                   EVAL      S@MORE = 'Bottom'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Subset "pallet" subfile prompt.                                        *
      **************************************************************************
     CSR   $Subset1      BEGSR

     C                   EXSR      $InitSSInd1
     C                   EVAL      *IN93 = *OFF

     C                   DOW       not *IN12

      *
      * Format "subset" screen.
      *
     C                   WRITE     MSFCTL01
     C                   EXFMT     DSPFMT6A
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $InitSSInd1
     C                   EXSR      $RmvPM

      *
      * If user pressed a valid command key . . .
      *
     C                   IF        *IN25

     C                   SELECT

      *
      * F12 = Cancel.
      *
     C                   WHEN      *IN12
     C                   EXSR      $RefreshSS1
     C                   LEAVE

      *
      * F4 = Prompt for valid values.
      *
     C                   WHEN      *IN04
     C                   EXSR      $ListSS1

      *
      * F5 = Refresh subset fields.
      *
     C                   WHEN      *IN05
     C                   EXSR      $RefreshSS1

      *
      * F14 = Reset subset fields to initial program values.
      *
     C                   WHEN      *IN14
     C                   EXSR      $InitSS1

     C                   ENDSL

     C                   ELSE

      *
      * Edit subset fields.
      *
     C                   EXSR      $EditSS1
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   LEAVE

     C                   ENDIF                                                  *IN25
     C                   ENDDO                                                  not *IN12

     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   ELSE
     C                   IF        F6DF <> sv_SSDF  or
     C                               F6SHTO <> sv_SSShto  or
     C                               F6DS <> sv_SSDS  or
     C                               F6BOL# <> sv_SSBOL#  or
     C                               F6PRO# <> sv_SSPRO#  or
     C                               F6SHPV <> sv_SSShpV  or
     C                               F6SRL# <> sv_SSCtnSrl# or
     C                               wk_Sfl01RRN = *Zero
     C                   EXSR      $Top1
     C                   EXSR      $SaveSS1
     C                   ENDIF                                                  F6DF<>sv_SSDF or
     C                   ENDIF                                                  *IN12

     C                   IF        F6DF <> *Blank  or
     C                               F6SHTO <> *Zero  or
     C                               F6DS <> *Blank  or
     C                               F6BOL# <> *Zero  or
     C                               F6PRO# <> *Blanks  or
     C                               F6SHPV <> *Blanks  or
     C                               F6SRL# <> *Blanks
     C                   EVAL      C1SSTX = '*SUBSET*'
     C                   ELSE
     C                   EVAL      C1SSTX = *Blanks
     C                   ENDIF                                                  F6DF<>*Blank or

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate subset prompt screen fields.                                  *
      **************************************************************************
     CSR   $EditSS1      BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Domestic/foreign selection be valid, if non-blank.
      *
     C                   IF        F6DF <> *Blank  and  F6DF <> 'D'  and
     C                               F6DF <> 'F'
     C                   EVAL      wk_MsgID = 'ERR0454'
     C                   EVAL      wk_MsgData = F6DF
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F6DF<>*Blanks and

      *
      * Ship-to number must be valid, if non-blank.
      *
     C                   IF        F6SHTO <> *Zero
     C     F6SHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      wk_MsgID = 'ERR0426'
     C                   EVAL      wk_MsgData = %triml(%editc(F6SHTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      F6STNM = *Blanks
     C                   ELSE
     C                   EVAL      F6STNM = CMCNA1
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ELSE
     C                   EVAL      F6STNM = *Blanks
     C                   ENDIF                                                  F6SHTO<>*Zero

      *
      * Drop ship yes/no selection be valid, if non-blank.
      *
     C                   IF        F6DS <> *Blank  and  F6DS <> 'Y'  and
     C                               F6DS <> 'N'
     C                   EVAL      wk_MsgID = 'ERR0496'
     C                   EVAL      wk_MsgData = F6DS + 'drop ships'
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F6DF<>*Blanks and

      *
      * BOL number must be valid, if non-blank.
      *
     C                   IF        F6BOL# <> *Zero
     C     F6BOL#        CHAIN     OEBOLH
     C                   IF        not %found(OEBOLH)
     C                   EVAL      wk_MsgID = 'ERR0655'
     C                   EVAL      wk_MsgData = %triml(%editc(F6BOL#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN44 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  F6BOL#<>*Zero

      *
      * Ship via code must be valid, if non-blank.
      *
     C                   IF        F6SHPV <> *Blanks
     C     F6SHPV        CHAIN     HDSHPV
     C                   IF        not %found(HDSHPV)
     C                   EVAL      wk_MsgID = 'ERR0387'
     C                   EVAL      wk_MsgData = F6SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN46 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not %found(HDSHPV)
     C                   ENDIF                                                  F6SHPV<>*Blanks

      *
      * Carton UCC-128 serial number must be valid, if non-blank.
      *
     C                   IF        F6SRL# = *Blanks
     C                   EVAL      wk_SSPallet# = *Zero
     C                   ELSE
     C****               IF        %check(Digits:%trim(F6SRL#)) > *Zero  or
     C****                           (%len(%trim(F6SRL#)) = 20  and
     C****                            %subst(F6SRL#:1:10) <> '0000090255')  or
     C****                           (%len(%trim(F6SRL#)) > 9  and
     C****                            %len(%trim(F6SRL#)) < 20)  or
     C****                            %subst(F6SRL#:21:1) <> ' '
     C                   IF        %check(Digits:%trim(F6SRL#)) > *Zero  or
     C                               (%len(%trim(F6SRL#)) = 20  and
     C                                (%subst(F6SRL#:1:4) <> '0000'  or
     C                                 (%subst(F6SRL#:5:6) <> wk_GS1Comp  and
     C                                  %subst(F6SRL#:5:6) <> wk_GS1Comp2  and
     C                                  %subst(F6SRL#:5:6) <> wk_GS1Comp3)))  or
     C                               (%len(%trim(F6SRL#)) > 9  and
     C                                %len(%trim(F6SRL#)) < 20)  or
     C                                %subst(F6SRL#:21:1) <> ' '
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN47 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(F6SRL#)
     C                   EXSR      $SndPM
     C                   EVAL      wk_SSPallet# = *Zero
     C                   ELSE
     C                   IF        %len(%trim(F6SRL#)) < 10
     C                   EVAL      wk_Serial# = %dec(%trim(F6SRL#):9:0)
     C                   ELSE
     C                   EVAL      wk_Serial# = %dec(%subst(F6SRL#:11:9):9:0)
     C                   ENDIF                                                  %len(%trim(F6SRL#))<
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      *IN02 = *OFF
     C     wk_Serial#    CHAIN(N)  HYLSSCC8
     C                   IF        not %found(HYLSSCC8)
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN47 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(F6SRL#)
     C                   EXSR      $SndPM
     C                   EVAL      wk_SSPallet# = *Zero
     C                   ELSE
     C                   IF        SCPSRL# = *Zero
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN47 = *ON
     C                   EVAL      wk_MsgID = 'ERR0929'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_Serial#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      wk_SSPallet# = *Zero
     C                   ELSE
     C                   EVAL      wk_SSPallet# = SCPSRL#
     C                   ENDIF                                                  SCPSRL#=*Zero
     C                   ENDIF                                                  not %found(HYLSSCC8)
     C                   ENDIF                                                  %check(Digits:%trim(
     C                   ENDIF                                                  F6SRL#=*Blanks

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize subset prompt screen indicators.                            *
      **************************************************************************
     CSR   $InitSSInd1   BEGSR

     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN44 = *OFF
     C                   EVAL      *IN45 = *OFF
     C                   EVAL      *IN46 = *OFF
     C                   EVAL      *IN47 = *OFF
     C                   EVAL      *IN89 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Reset subset prompt screen fields to initial program values.           *
      **************************************************************************
     CSR   $InitSS1      BEGSR

     C                   EVAL      F6DF = *Blank
     C                   EVAL      F6SHTO = *Zero
     C                   EVAL      F6STNM = *Blank
     C                   EVAL      F6DS = *Blank
     C                   EVAL      F6BOL# = *Zero
     C                   EVAL      F6PRO# = *Blanks
     C                   EVAL      F6SHPV = *Blanks
     C                   EVAL      F6SVDS = *Blanks
     C                   EVAL      F6SRL# = *Blanks
     C                   EVAL      wk_SSPallet# = *Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Refresh subset prompt screen fields.                                   *
      **************************************************************************
     CSR   $RefreshSS1   BEGSR

     C                   EVAL      F6DF = sv_SSDF
     C                   EVAL      F6SHTO = sv_SSShto
     C                   EVAL      F6STNM = sv_SSSTNm
     C                   EVAL      F6DS = sv_SSDS
     C                   EVAL      F6BOL# = sv_SSBOL#
     C                   EVAL      F6PRO# = sv_SSPRO#
     C                   EVAL      F6SHPV = sv_SSShpV
     C                   EVAL      F6SVDS = sv_SSSVDs
     C                   EVAL      F6SRL# = sv_SSCtnSrl#
     C                   EVAL      wk_SSPallet# = sv_SSPallet#

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Save subset prompt screen fields.                                      *
      **************************************************************************
     CSR   $SaveSS1      BEGSR

     C                   EVAL      sv_SSDF = F6DF
     C                   EVAL      sv_SSShto = F6SHTO
     C                   EVAL      sv_SSSTNm = F6STNM
     C                   EVAL      sv_SSDS = F6DS
     C                   EVAL      sv_SSBOL# = F6BOL#
     C                   EVAL      sv_SSPRO# = F6PRO#
     C                   EVAL      sv_SSShpV = F6SHPV
     C                   EVAL      sv_SSSVDs = F6SVDS
     C                   EVAL      sv_SSCtnSrl# = F6SRL#
     C                   EVAL      sv_SSPallet# = wk_SSPallet#

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid values for subset prompt screen fields.                     *
      **************************************************************************
     CSR   $ListSS1      BEGSR

     C                   SELECT

     C                   WHEN      CSRFLD = 'F6SHTO'
     C                   EXSR      $ListCusts
     C                   EVAL      *IN42 = *ON

     C                   WHEN      CSRFLD = 'F6SHPV'
     C                   EXSR      $ListShipVias
     C                   EVAL      *IN46 = *ON

     C                   OTHER
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0013'
     C                   EXSR      $SndPM

     C                   SELECT
     C                   WHEN      CSRFLD = 'F6DF'
     C                   EVAL      *IN41 = *ON
     C                   WHEN      CSRFLD = 'F6DS'
     C                   EVAL      *IN43 = *ON
     C                   WHEN      CSRFLD = 'F6BOL#'
     C                   EVAL      *IN44 = *ON
     C                   WHEN      CSRFLD = 'F6PRO#'
     C                   EVAL      *IN45 = *ON
     C                   WHEN      CSRFLD = 'F6SRL#'
     C                   EVAL      *IN47 = *ON
     C                   OTHER
     C                   EVAL      *IN41 = *ON
     C                   ENDSL

     C                   ENDSL

     C                   EVAL      *IN89 = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid customers.                                                  *
      **************************************************************************
     CSR   $ListCusts    BEGSR

     C                   EVAL      io_Cust# = -1

     C                   CALLP(E)  HHDCSS (io_Cust#)

     C                   IF        io_Cust# = *Zero
     C                   EVAL      wk_MsgID = 'GUM0014'
     C                   EVAL      wk_MsgData = 'F12'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      F6SHTO = io_Cust#
     C     F6SHTO        CHAIN     HDCUST
     C                   IF        %found(HDCUST)
     C                   EVAL      F6STNM = CMCNA1
     C                   ELSE
     C                   EVAL      F6STNM = *Blanks
     C                   ENDIF                                                  %found(HDCUST)
     C                   EVAL      wk_MsgID = 'MSG0116'
     C                   EVAL      wk_MsgData = %triml(%editc(F6SHTO:'Z'))
     C                   EXSR      $SndPM
     C                   ENDIF                                                  io_Cust#=*Zero

     C                   EVAL      *IN93 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid ship via codes.                                             *
      **************************************************************************
     CSR   $ListShipVias BEGSR

     C                   EVAL      io_ShipVia = F6SHPV

     C                   CALLP(E)  HHDSVI (io_ShipVia)

     C                   IF        io_ShipVia = *Blanks
     C                   EVAL      wk_MsgID = 'GUM0014'
     C                   EVAL      wk_MsgData = 'F12'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      F6SHPV = io_ShipVia
     C     F6SHPV        CHAIN     HDSHPV
     C                   IF        %found(HDSHPV)
     C                   EVAL      F6SVDS = SVSVDS
     C                   ELSE
     C                   EVAL      F6SVDS = *Blanks
     C                   ENDIF                                                  %found(HDSHPV)
     C                   EVAL      wk_MsgID = 'MSG0122'
     C                   EVAL      wk_MsgData = io_ShipVia
     C                   EXSR      $SndPM
     C                   ENDIF                                                  io_ShipVia=*Blanks

     C                   EVAL      *IN93 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process "Pallet Detail", a.k.a. "Carton", subfile.                     *
      **************************************************************************
     CSR   $Carton       BEGSR

     C                   SELECT
     C                   WHEN      wk_Subr3Mode = MaintMode1                    Maintenance mode 1
     C                   EVAL      C3HDG2 = C3HDG2A
     C                   EVAL      C3OPTS = OptDsc3A
     C                   WHEN      wk_Subr3Mode = MaintMode2                    Maintenance mode 2
     C                   EVAL      C3HDG2 = C3HDG2B
     C                   EVAL      C3OPTS = OptDsc3B
     C                   WHEN      wk_Subr3Mode = InqMode                       Inquiry mode
     C                   EVAL      C3HDG2 = C3HDG2C
     C                   EVAL      C3OPTS = OptDsc3C
     C                   ENDSL

     C                   EVAL      C3PSRL# = %triml(%editc(wk_Pallet#3:'Z'))
     C     wk_Pallet#3   CHAIN(N)  HYPPLLT
     C                   IF        %found(HYPPLLT)
     C                   IF        PMSHTO <> CMCUST
     C     PMSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCUST = *Zero
     C                   EVAL      CMBLTO = *Zero
     C                   EVAL      CMCNA1 = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  PMSHTO<>CMCUST
     C                   EVAL      C3SHTO = %triml(%editc(PMSHTO:'Z')) + '-' +
     C                               CMCNA1
     C                   EVAL      wk_BillTo# = CMBLTO
     C                   ELSE
     C                   EVAL      C3SHTO = *Blanks
     C                   ENDIF                                                  %found(HYPPLLT)

     C                   EVAL      C3SRL# = *Blanks

     C                   EXSR      $PageDownSf3

     C                   DOW       not *IN12

      *
      * Display Subfile
      *
     C                   WRITE     MSFCTL01
      *
      * Check for No Records to Display
      *
     C                   IF        wk_Sfl03RRN > *Zero
     C                   EVAL      *IN27 = *ON
     C                   ENDIF                                                  wk_Sfl03RRN>*Zero

     C                   EVAL      *IN26 = *OFF

     C                   WRITE     SFLCMD03
     C                   EXFMT     SFLCTL03

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN89 = *OFF
     C                   EXSR      $RMVPM
      *
      * Valid Command Key?
      *
     C                   IF        *IN25

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   IF        wk_LastPallet# <> *Zero  and
     C                               wk_PgmMode = MaintMode
     C                   EXSR      $ChkPlltRpt
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_LastPallet#<>*Zer
     C                   IF        wk_LastPallet2 <> *Zero  and
     C                               wk_PgmMode = MaintMode
     C                   EVAL      wk_LastPallet# = wk_LastPallet2
     C                   EVAL      wk_LastPallet2 = *Zero
     C                   EXSR      $ChkPlltRpt
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_LastPallet2<>*Zer
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Check for function key use with options present.
      *
     C                   EVAL      wk_Error = *OFF

     C                   IF        *IN17  or  *IN18  or  *IN20  or  *IN91  or
     C                                *IN92

     C                   IF        wk_SFL03RRN <> *Zero
      *
      * Edit options if subfile has records.
      *
     C                   EXSR      $EditSf3
      *
      * If function key pressed, and options are present, cannot do
      * function, send error message.
      *
     C                   IF        wk_Error
     C                   IF        *IN17  or  *IN18  or  *IN20
     C                   EVAL      wK_MsgID = 'GUM0005'
     C                   ELSE
     C                   EVAL      wK_MsgID = 'GUM0004'
     C                   ENDIF                                                  *IN17 or *IN18
     C                   EVAL      wk_MsgData = *Blanks
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_Error
      *
     C                   ENDIF                                                  wk_SFL03RRN<>*Zero

     C                   ENDIF                                                  *IN17 or *IN18 or

     C                   IF        not wk_Error

     C                   SELECT

      *
      * Page Down Key?
      *
     C                   WHEN      *IN91
     C                   EXSR      $PageDownSf3
     C                   EVAL      *IN91 = *OFF

      *
      * Page Up Key?
      *
     C                   WHEN      *IN92
     C                   EXSR      $PageUpSf3
     C                   EVAL      *IN92 = *OFF

      *
      * Refresh?
      *
     C                   WHEN      *IN05
     C                   EXSR      $PageDownSf3
     C                   EVAL      *IN05 = *OFF

      *
      * Top?
      *
     C                   WHEN      *IN17
     C                   EXSR      $Top3
     C                   EVAL      *IN17 = *OFF

      *
      * Bottom?
      *
     C                   WHEN      *IN18
     C                   EXSR      $Bottom3
     C                   EVAL      *IN18 = *OFF

      *
      * Subset?
      *
     C                   WHEN      *IN20
     C                   EXSR      $Subset3
     C                   EVAL      *IN20 = *OFF

     C                   ENDSL

      *
      * Home Key pressed?, set cursor on position field.
      *
     C                   IF        *IN94
     C                   EVAL      *IN42 = *ON
     C                   ENDIF                                                  *IN94

     C                   ENDIF                                                  not wk_Error

     C                   ELSE

      *
      * Enter Key
      *
      * If no records in subfile, return to calling subroutine.
      *
     C                   IF        wk_SFL03RRN = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_SFL03RRN=*Zero

      *
      * Edit subfile options.
      *
     C                   EXSR      $EditSf3

      *
      * If errors are detected, sound alarm and redisplay subfile.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * If no options are present and position-to not requested, return
      * to calling subroutine.
      *
     C                   IF        not wk_OptPr  and  C3SRL# = *Blanks
     C                   LEAVE
     C                   ENDIF                                                  not wk_OptPr and

      *
      * If options are present, read and process subfile.
      *
     C                   IF        wk_OptPr
     C                   EXSR      $ProcessSf3
     C                   ENDIF                                                  wk_OptPr

      *
      * If position-to requested, refill subfile.
      *
     C                   IF        C3SRL# <> *Blanks
     C****               IF        %check(Digits:%trim(C3SRL#)) > *Zero  or
     C****                           (%len(%trim(C3SRL#)) = 20  and
     C****                            %subst(C3SRL#:1:10) <> '0000090255')  or
     C****                           (%len(%trim(C3SRL#)) > 9  and
     C****                            %len(%trim(C3SRL#)) < 20)  or
     C****                            %subst(C3SRL#:21:1) <> ' '
     C                   IF        %check(Digits:%trim(C3SRL#)) > *Zero  or
     C                               (%len(%trim(C3SRL#)) = 20  and
     C                                (%subst(C3SRL#:1:4) <> '0000'  or
     C                                 (%subst(C3SRL#:5:6) <> wk_GS1Comp  and
     C                                  %subst(C3SRL#:5:6) <> wk_GS1Comp2  and
     C                                  %subst(C3SRL#:5:6) <> wk_GS1Comp3)))  or
     C                               (%len(%trim(C3SRL#)) > 9  and
     C                                %len(%trim(C3SRL#)) < 20)  or
     C                                %subst(C3SRL#:21:1) <> ' '
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(C3SRL#)
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  %check(Digits:%trim(
     C                   IF        %len(%trim(C3SRL#)) < 10
     C                   EVAL      wk_Serial# = %dec(%trim(C3SRL#):9:0)
     C                   ELSE
     C                   EVAL      wk_Serial# = %dec(%subst(C3SRL#:11:9):9:0)
     C                   ENDIF                                                  %len(%trim(C3SRL#))<
     C                   EVAL      @USRL# = wk_Serial#
     C                   EVAL      @UORD# = *Zero
     C                   EVAL      @UORL# = *Zero
     C                   EVAL      @UBLN# = *Zero
     C                   EVAL      @ULOT# = *Blanks
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf3
     C                   EVAL      *IN05 = *OFF
     C                   EVAL      wk_Refresh = *OFF
     C                   EVAL      C3SRL# = *Blanks
     C                   ENDIF                                                  C3SRL#<>*Blanks

     C                   ENDIF                                                  *IN25=*ON

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page down subfile SFL03 - pallet detail subfile.                      *
      **************************************************************************
     CSR   $PageDownSf3  BEGSR

      *
      * If no records, clear subfile and initialize relative record and page count.
      *
     C     wk_Pallet#3   SETLL     HYLSSCC9
     C                   IF        not %equal(HYLSSCC9)

     C                   EXSR      $ClearSFL03

     C                   IF        *IN91
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ENDIF                                                  *IN91
     C                   EVAL      S@MORE = 'Bottom'
     C                   LEAVESR

     C                   ELSE

      *
      * Set starting point in queue
      *   If Page Down, start at next page
      *   If Refresh, start at top
      *   If Search, start at position to...
      *   Otherwise, initial build or page up, start at BOF
      *
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY3       SETGT     HYLSSCC9
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY3       SETLL     HYLSSCC9
     C                   OTHER                                                  Enter,...
     C     wk_Pallet#3   SETLL     HYLSSCC9
     C                   ENDSL
     C                   DOU       %eof(HYLSSCC9)  or  wk_Include
     C     wk_Pallet#3   READE(N)  HYLSSCC9
     C                   IF        not %eof(HYLSSCC9)
     C                   EXSR      $Filter3
     C                   ENDIF                                                  not %eof(HYLSSCC9)
     C                   ENDDO                                                  %eof(HYLSSCC9) or

     C                   ENDIF                                                  not %found(HYLSSCC9)

      *
      * If EOF, send Bottom of the List message
      *
     C                   IF        %eof(HYLSSCC9)
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM

      *
      * If refresh, get last record.
      *
     C                   IF        *IN05
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSFL03

     C     wk_Pallet#3   SETGT     HYLSSCC9
     C                   DOU       %eof(HYLSSCC9)  or  wk_Include
     C     wk_Pallet#3   READPE(N) HYLSSCC9
     C                   IF        not %eof(HYLSSCC9)
     C                   EXSR      $Filter3
     C                   ENDIF                                                  not %eof(HYLSSCC9)
     C                   ENDDO                                                  %eof(HYLSSCC9) or
     C                   ENDIF                                                  *IN05

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSFL03

     C                   ENDIF                                                  %eof(HYLSSCC9)

     C                   DOW         not %eof(HYLSSCC9)

      *
      * If subfile is full, quit.
      *
     C                   IF        wk_SFL03PC = wk_SFL03PS
     C                   LEAVE
     C                   ENDIF                                                  wk_SFL03PC=wk_SFL03P

      *
      * Blank Option Field.  Set off SFLNXTCHG.
      *
     C                   EVAL      S3OPT = *Blanks
     C                   EVAL      *IN28 = *OFF

      *
      * Move Database Fields to Screen
      *
     C                   EXSR      $MoveToSFL03

      *
      * Increment Page Counter, Rel Rec Nbr
      *
     C                   EVAL      wk_SFL03PC = wk_SFL03PC + 1
     C                   EVAL      wk_SFL03RRN = wk_SFL03RRN + 1

     C                   WRITE     SFL03

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_SFL03PC = 1
     C                   Z-ADD     wk_SFL03RRN   S@RCNO
     C                   EVAL      @USRL# = SCSRL#
     C                   EVAL      @UORD# = SCORD#
     C                   EVAL      @UORL# = SCORL#
     C                   EVAL      @UBLN# = SCBLN#
     C                   EVAL      @ULOT# = SCLOT#
     C                   ENDIF                                                  wk_SFL03PC=1
     C                   EVAL      @DSRL# = SCSRL#
     C                   EVAL      @DORD# = SCORD#
     C                   EVAL      @DORL# = SCORL#
     C                   EVAL      @DBLN# = SCBLN#
     C                   EVAL      @DLOT# = SCLOT#

      *
      * Get next carton UCC-128 record.
      *
     C                   DOU       %eof(HYLSSCC9)  or  wk_Include
     C     wk_Pallet#3   READE(N)  HYLSSCC9
     C                   IF        not %eof(HYLSSCC9)
     C                   EXSR      $Filter3
     C                   ENDIF                                                  not %eof(HYLSSCC9)
     C                   ENDDO                                                  %eof(HYLSSCC9) or

     C                   ENDDO                                                  not %eof(HYLSSCC9)

      *
      * Set More... or Bottom   subfile message.
      *
     C                   IF        %eof(HYLSSCC9)
     C                   EVAL      S@MORE = 'Bottom'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSSCC9)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear subfile 01, initialize relative record and page count.           *
      **************************************************************************
     CSR   $ClearSFL03   BEGSR

     C                   EVAL      *IN26 = *ON
     C                   EVAL      *IN27 = *OFF
     C                   WRITE     SFLCTL03
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_SFL03RRN = *Zero
     C                   EVAL      wk_SFL03PC = *Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page up subfile SFL03 - pallet detail subfile.                        *
      **************************************************************************
     CSR   $PageUpSf3    BEGSR

     C     PGUKEY3       SETLL     HYLSSCC9
     C                   DOU       %eof(HYLSSCC9)  or  wk_Include
     C     wk_Pallet#3   READPE(N) HYLSSCC9
     C                   IF        not %eof(HYLSSCC9)
     C                   EXSR      $Filter3
     C                   ENDIF                                                  not %eof(HYLSSCC9)
     C                   ENDDO                                                  %eof(HYLSSCC9) or

      *
      * If BOF, send Top of the List message
      *
     C                   IF        %eof(HYLSSCC9)

     C                   EVAL      wk_MsgID = 'GUM0003'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSFL03
     C                   EVAL      wk_SFL03RRN = wk_SFL03PS + 1

     C                   ENDIF                                                  %eof(HYLSSCC9)

     C                   DOW       not %eof(HYLSSCC9)

      *
      * If subfile is full, quit.
      *
     C                   IF        wk_SFL03PC = wk_SFL03PS
     C                   LEAVE
     C                   ENDIF                                                  wk_SFL03PC=wk_SFL03P

      *
      * Blank Option Field.  Set off SFLNXTCHG.
      *
     C                   EVAL      S3OPT = *Blanks
     C                   EVAL      *IN28 = *OFF

      *
      * Move Database Fields to Screen
      *
     C                   EXSR      $MoveToSFL03

      *
      * Increment Page Counter, Decrement Rel Rec Nbr
      *
     C                   EVAL      wk_SFL03PC = wk_SFL03PC + 1
     C                   EVAL      wk_SFL03RRN = wk_SFL03RRN - 1

     C                   WRITE     SFL03

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_SFL03RRN = wk_SFL03PS
     C                   EVAL      @DSRL# = SCSRL#
     C                   EVAL      @DORD# = SCORD#
     C                   EVAL      @DORL# = SCORL#
     C                   EVAL      @DBLN# = SCBLN#
     C                   EVAL      @DLOT# = SCLOT#
     C                   ENDIF                                                  wk_SFL03RRN=wk_SFL03
     C                   Z-ADD     wk_SFL03RRN   S@RCNO
     C                   EVAL      @USRL# = SCSRL#
     C                   EVAL      @UORD# = SCORD#
     C                   EVAL      @UORL# = SCORL#
     C                   EVAL      @UBLN# = SCBLN#
     C                   EVAL      @ULOT# = SCLOT#

      *
      * Get previous carton UCC-128 record.
      *
     C                   DOU       %eof(HYLSSCC9)  or  wk_Include
     C     wk_Pallet#3   READPE(N) HYLSSCC9
     C                   IF        not %eof(HYLSSCC9)
     C                   EXSR      $Filter3
     C                   ENDIF                                                  not %eof(HYLSSCC9)
     C                   ENDDO                                                  %eof(HYLSSCC9) or

     C                   ENDDO                                                  not %eof(HYLSSCC9)

      *
      * Set More... or Top if there are more records or BOF.
      * If Beg of File reached and not a full page, fill from top down.
      *
     C                   IF        %eof(HYLSSCC9)
     C                   IF        wk_SFL03PC <> wk_SFL03PS
     C                   EVAL      @USRL# = *Loval
     C                   EVAL      @UORD# = *Zero
     C                   EVAL      @UORL# = *Zero
     C                   EVAL      @UBLN# = *Zero
     C                   EVAL      @ULOT# = *Blanks
     C                   EXSR      $PageDownSf3
     C                   ENDIF                                                  wk_SFL03PC<>wk_SFL03
     C                   EVAL      S@MORE = 'Top'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSSCC9)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move database fields to subfile fields.                                *
      **************************************************************************
     CSR   $MoveToSFL03  BEGSR

     C     SCITEM        CHAIN     HDIMST
     C                   IF        not %found(HDIMST)
     C                   EVAL      IMITEM = *Blanks
     C****               EVAL      IMIMDS = *Blanks
     C                   EVAL      IMIMDS = SCIMDS
     C                   ENDIF                                                  not %found(HDIMST)

     C                   EVAL      S3SRL# = SCSRL#
     C                   EVAL      S3ITEM = SCITEM
     C                   EVAL      S3IMDS = IMIMDS
     C                   EVAL      S3UOMQ = SCUOMQ
     C                   EVAL      S3EWGT = SCEWGT
     C                   EVAL      S3ORD# = SCORD#
     C                   EVAL      S3ORL# = SCORL#
     C                   EVAL      S3BLN# = SCBLN#
     C                   EVAL      S3LOT# = SCLOT#

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL03 "pallet detail" subfile.                                  *
      **************************************************************************
     CSR   $Filter3      BEGSR

     C                   EVAL      wk_Include = *ON

      *
      * Item number subset.
      *
     C                   IF        F7ITEM <> *Blanks
     C                   IF        SCITEM <> F7ITEM
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  SCITEM<>F7ITEM
     C                   ENDIF                                                  F7ITEM<>*Blanks

      *
      * Item description subset.
      *
     C                   IF        F7IMDS <> *Blanks
     C                   IF        %scan(%trimr(F7IMDS):SCIMDS) = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  %scan(%triml(F7IMDS)
     C                   ENDIF                                                  F7IMDS<>*Blanks

      *
      * Product class subset.
      *
     C                   IF        F7PCLS <> *Blanks
     C                   IF        SCITEM <> IMITEM
     C     SCITEM        CHAIN     HDIMST
     C                   IF        not %found(HDIMST)
     C                   EVAL      IMITEM = *Blanks
     C                   EVAL      IMPCLS = *Blanks
     C                   ENDIF                                                  not %found(HDIMST)
     C                   ENDIF                                                  SCITEM<>IMITEM
     C                   IF        %subst(IMPCLS:1:%len(%trimr(F7PCLS))) <>
     C                               %trimr(F7PCLS)
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  %subst(IMPCLS:1:%len
     C                   ENDIF                                                  F7IMDS<>*Blanks

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Edit options subfile SFL03 - pallet detail subfile.                   *
      **************************************************************************
     CSR   $EditSf3      BEGSR

      *
      *  Reset indicators.
      *
     C                   EVAL      wk_Error = *OFF
     C                   EVAL      wk_DltPr = *OFF
     C                   EVAL      wk_DspPr = *OFF
     C                   EVAL      wk_MovPr = *OFF
     C                   EVAL      wk_PrtHzLBPr = *OFF
     C                   EVAL      wk_OptPr = *OFF
     C                   EVAL      wk_ClrDone = *OFF

      *
      * Save function key indicators.
      *
     C                   EVAL      wk_IN25 = *IN25

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL03

     C                   DOW       not %eof(HYD0138)

      *
      * Reset error indicator and turn on SFLNXTCHG indicator.
      *
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN28 = *ON

      *
      * If a valid function key, turn on error indicator.
      *
     C                   IF        wk_IN25
     C                   EVAL      *IN41 = *ON
     C                   ENDIF                                                  wk_IN25

      *
      * Deletes present?
      *
     C                   IF        (S3OPT = @DLT  or  S3OPT = @DLT2)
     C                                and  wk_Subr3Mode = MaintMode1

     C                   EVAL      S3OPT = @DLT

     C                   EVAL      wk_DltPr = *ON
     C                   UPDATE    SFL03

      *
      * If first delete, clear delete subfile and save first delete
      * subfile relative record number.
      *
     C                   IF        wk_ClrDone = *OFF
     C                   EVAL      wk_ClrDone = *ON
     C                   EVAL      wk_FirstDlt = wk_SFL03RRN

      *
      *  Clear Subfile, Reset Rel Rec Nbr
      *
     C                   EVAL      *IN29 = *ON
     C                   WRITE     SFLCTL04
     C                   EVAL      *IN29 = *OFF
     C                   EVAL      wk_Sfl04RRN = *Zero
     C                   ENDIF                                                  wk_ClrDone=*OFF

      *
      *  Write to "delete" subfile.
      *
     C                   EVAL      wk_Sfl04RRN = wk_Sfl04RRN + 1
     C                   WRITE     SFL04

     C                   ELSE

      *
      * Displays present?
      *
     C                   IF        (S3OPT = @DSP  or  S3OPT = @DSP2)

     C                   EVAL      S3OPT = @DSP

     C                   EVAL      wk_DspPr = *ON
     C                   UPDATE    SFL03

     C                   ELSE

      *
      * Details present?
      *
     C                   IF        (S3OPT = @MOV  or  S3OPT = @MOV2)
     C                                and  wk_Subr3Mode = MaintMode2

     C                   EVAL      S3OPT = @MOV

     C                   EVAL      wk_MovPr = *ON
     C                   UPDATE    SFL03

     C                   ELSE

      *
      * Print hazard labels present?
      *
     C                   IF        (S3OPT = @PRTHZLB)
     C                                and  wk_Subr3Mode = MaintMode1

     C                   EVAL      wk_PrtHzLBPr = *ON
     C                   UPDATE    SFL03

     C                   ELSE

     C                   IF        S3OPT <> *Blanks

      *
      *  Set Cursor on First Error
      *
     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_SFL03RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   UPDATE    SFL03
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EXSR      $SndPM

     C                   ELSE

      *
      * Ignore Blank Options, Turn off Error & SFLNXTCHG
      *
     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   UPDATE    SFL03

     C                   ENDIF                                                   S3OPT<>*Blanks

     C                   ENDIF                                                   S3OPT=@PRTHZLB

     C                   ENDIF                                                   S3OPT=@MOV or

     C                   ENDIF                                                   S3OPT=@DSP or

     C                   ENDIF                                                   S3OPT=@DLT or

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL03

     C                   ENDDO                                                  not %eof(HYD0138)

      *
      * Turn off error indicator.
      *
     C                   EVAL      *IN41 = *OFF

      *
      * Restore function key indicators.
      *
     C                   EVAL      *IN25 = wk_IN25

      *
      * If options present, and Page Down or Page Up requested, set
      * error flag.
      *
     C                   IF        *IN25
     C                   IF        wk_DltPr  or  wk_DspPr  or  wk_MovPr
     C                               or  wk_PrtHzLBPr
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_DltPr or
     C                   ELSE
     C                   IF        wk_DltPr  or  wk_DspPr  or  wk_MovPr
     C                               or  wk_PrtHzLBPr
     C                   EVAL      wk_OptPr = *ON
     C                   ENDIF                                                  wk_DltPr or
     C                   ENDIF                                                  *IN25

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process options for subfile SFL03.                                     *
      **************************************************************************
     CSR   $ProcessSf3   BEGSR

     C                   EVAL      *IN12 = *OFF

     C                   READC     SFL03

     C                   DOW       not %eof(HYD0138)  and  not *IN03

      *
      *  F12=Cancel Key
      *
     C                   IF        not *IN12

     C                   SELECT

     C                   WHEN      S3OPT = @DSP
     C                   EXSR      $DisplayCtn
     C                   IF        *IN12
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S3OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ENDIF                                                  *IN12
     C                   UPDATE    SFL03

     C                   WHEN      S3OPT = @MOV
     C                   EVAL      wk_Subr3Mode = MaintMode2                    Maintenance mode 2
     C                   EXSR      $MoveCtn
     C                   IF        *IN12
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S3OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ENDIF                                                  *IN12
     C                   UPDATE    SFL03

     C                   WHEN      S3OPT = @DLT

      *
      * Keep "removes" active - turn SFLNXTCHG on.
      *
     C                   EVAL      *IN28 = *ON
     C                   UPDATE    SFL03

     C                   WHEN      S3OPT = @PRTHZLB
     C                   EXSR      $PrtHazLbl2

     C                   ENDSL

     C                   EVAL      S@RCNO = wk_SFL03RRN

     C                   ELSE

      *
      * Cancel requested - save remaining options.
      *
     C                   EVAL      *IN28 = *ON
     C                   UPDATE    SFL03

     C                   ENDIF                                                  not *IN12

      *
      * Get next changed subfile record (save status of F12=Cancel).
      *
     C                   IF        not %eof(HYD0138)  and  not *IN03
     C                   EVAL      wk_IN12 = *IN12
     C                   READC     SFL03
     C                   EVAL      *IN12 = wk_IN12
     C                   ENDIF                                                  not %eof(HYD0138)

     C                   ENDDO                                                  not %eof(HYD0138)

      *
      * If "removes" are present and not F12=Cancel, process "removes".
      *
     C                   IF        wk_DltPr  and  not *IN03  and  not *IN12
     C                   EVAL      S@RCNO = wk_FirstDlt
     C                   EXSR      $ProcRemoves
     C                   ENDIF                                                  wk_DltPr and

     C                   IF        not *IN03  and  not *IN12  and  wk_Refresh
     C                               and  C3SRL# = *Blanks
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf3
     C                   EVAL      *IN05 = *OFF
     C                   EVAL      wk_Refresh = *OFF
     C                   ENDIF                                                  not *IN03 and

     C                   EVAL      *IN12 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process carton removes from pallet.                                    *
      **************************************************************************
     CSR   $ProcRemoves  BEGSR

     C                   DOW       not *IN03  and  not *IN12
     C

      *
      * Format remove confirmation screen.
      *
     C                   WRITE     MSFCTL01
     C                   WRITE     SFLCMD04
     C                   EVAL      *IN29 = *OFF
     C                   EXFMT     SFLCTL04
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $RmvPM

      *
      * Valid function key?
      *
     C                   IF        *IN25

     C                   ELSE

     C                   EVAL      ary_MP = *Blanks
     C                   EVAL      idx3 = *Zero

      *
      * Read & process removes, then exit.
      *
     C                   READC     SFL04

     C                   DOW       not %eof(HYD0138)

     C     S3SRL#        SETLL     HYLSSCC8
     C                   DOU       %eof(HYLSSCC8)
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      *IN02 = *OFF
     C     S3SRL#        READE     HYLSSCC8
     C                   IF        %eof(HYLSSCC8)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC8)
     C                   EVAL      wk_LastPallet# = SCPSRL#
     C                   IF        RtvPRSts1 (wk_LastPallet#) = '*PRINT'
     C                   EVAL      wk_RC0139A =
     C                               WrtPRLog (wk_LastPallet#:'*RESET')
     C                   ENDIF                                                  RtvPRSts1 (wk_LastPa
     C                   EVAL      SCPSRL# = *Zero
     C                   EVAL      SCPEMPL = *Zero
     C                   SELECT
     C                   WHEN      *IN01
     C                   UPDATE    HYRSSCC8
     C                   WHEN      *IN02
     C                   UPDATE    HYRSCWK8
     C                   ENDSL
     C                   ENDDO                                                  %eof(HYLSSCC8)

      *
      * If this removed UCC-128s is part of a master-pack, remove any other
      * UCC-128s that were part of the same master-pack.
      *
     C     S3SRL#        CHAIN     HYLMPCD2
     C                   IF        %found(HYLMPCD2)
     C                   IF        %lookup(MDMP#:ary_MP:1:idx3) = *Zero
     C                   EVAL      idx3 = idx3 + 1
     C                   EVAL      ary_MP(idx3) = MDMP#

     C                   EVAL      wk_MP# = MDMP#
     C     wk_MP#        SETLL     HYPMPCD
     C                   DOU       %eof(HYPMPCD)
     C     wk_MP#        READE     HYPMPCD
     C                   IF        %eof(HYPMPCD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPMPCD)
     C     MDSRL#        SETLL     HYLSSCC8
     C                   DOU       %eof(HYLSSCC8)
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      *IN02 = *OFF
     C     MDSRL#        READE     HYLSSCC8
     C                   IF        %eof(HYLSSCC8)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC8)
     C                   IF        SCPSRL# = *Zero
     C                   UNLOCK    HYLSSCC8
     C                   ITER
     C                   ENDIF                                                  SCPSRL#=*Zero
     C                   EVAL      SCPSRL# = *Zero
     C                   EVAL      SCPEMPL = *Zero
     C                   SELECT
     C                   WHEN      *IN01
     C                   UPDATE    HYRSSCC8
     C                   WHEN      *IN02
     C                   UPDATE    HYRSCWK8
     C                   ENDSL
     C                   ENDDO                                                  %eof(HYLSSCC8)
     C                   ENDDO                                                  %eof(HYPMPCD)

     C                   ENDIF                                                  %lookyp(MDMP#:ary_MP
     C                   ENDIF                                                  %found(HYLMPCD2)

     C                   EVAL      wk_Refresh = *ON

      *
      * Send "carton removed from pallet" message.
      *
     C                   IF        not %found(HYLMPCD2)
     C                   EVAL      wk_MsgID = 'MSG0196'
     C                   EVAL      wk_MsgData =
     C                               %triml(%editc(wk_Pallet#3:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:9) =
     C                               %triml(%editc(S3SRL#:'Z'))
     C                   ELSE
     C                   EVAL      wk_MsgID = 'MSG0438'
     C                   EVAL      wk_MsgData =
     C                               %triml(%editc(wk_Pallet#3:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:12) = wk_MP#
     C                   ENDIF                                                  not %found(HYLMPCD2)
     C                   EXSR      $SndPM

     C                   READC     SFL04

     C                   ENDDO                                                  not %eof(HYD0138)

     C                   IF        wk_Refresh
     C                   EXSR      $RtvPltTots
     C                   EVAL      F3TCTN = wk_TotCtn
     C                   EVAL(H)   F3TWGT = wk_TotWgt
     C                   ENDIF                                                  wk_Refresh

     C                   LEAVE

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03 and

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display subfile SFL03 from the top.                                    *
      **************************************************************************
     CSR   $Top3         BEGSR

     C                   EVAL      @USRL# = *Loval
     C                   EVAL      @UORD# = *Zero
     C                   EVAL      @UORL# = *Zero
     C                   EVAL      @UBLN# = *Zero
     C                   EVAL      @ULOT# = *Blanks
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf3
     C                   EVAL      S@MORE = 'Top'

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Display subfile SFL03 from the bottom.                                 *
      **************************************************************************
     CSR   $Bottom3      BEGSR

     C                   EVAL      @USRL# = *Hival
     C                   EVAL      @UORD# = *Hival
     C                   EVAL      @UORL# = *Hival
     C                   EVAL      @UBLN# = *Hival
     C                   EVAL      @ULOT# = *Hival
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageUpSf3
     C                   EVAL      S@MORE = 'Bottom'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Subset "pallet detail" subfile prompt.                                 *
      **************************************************************************
     CSR   $Subset3      BEGSR

     C                   EXSR      $InitSSInd3
     C                   EVAL      *IN93 = *OFF

     C                   DOW       not *IN12

      *
      * Format "subset" screen.
      *
     C                   WRITE     MSFCTL01
     C                   EXFMT     DSPFMT7A
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $InitSSInd3
     C                   EXSR      $RmvPM

      *
      * If user pressed a valid command key . . .
      *
     C                   IF        *IN25

     C                   SELECT

      *
      * F12 = Cancel.
      *
     C                   WHEN      *IN12
     C                   EXSR      $RefreshSS3
     C                   LEAVE

      *
      * F4 = Prompt for valid values.
      *
     C                   WHEN      *IN04
     C                   EXSR      $ListSS3

      *
      * F5 = Refresh subset fields.
      *
     C                   WHEN      *IN05
     C                   EXSR      $RefreshSS3

      *
      * F14 = Reset subset fields to initial program values.
      *
     C                   WHEN      *IN14
     C                   EXSR      $InitSS3

     C                   ENDSL

     C                   ELSE

      *
      * Edit subset fields.
      *
     C                   EXSR      $EditSS3
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   LEAVE

     C                   ENDIF                                                  *IN25
     C                   ENDDO                                                  not *IN12

     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   ELSE
     C                   IF        F7ITEM <> sv_SSItem  or
     C                               F7IMDS <> sv_SSItemDesc  or
     C                               F7PCLS <> sv_SSProdCls  or
     C                               wk_Sfl03RRN = *Zero
     C                   EXSR      $Top3
     C                   EXSR      $SaveSS3
     C                   ENDIF                                                  F7ITEM<>sv_SSItem or
     C                   ENDIF                                                  *IN12

     C                   IF        F7ITEM <> *Blanks  or
     C                               F7IMDS <> *Blanks  or
     C                               F7PCLS <> *Blanks
     C                   EVAL      C3SSTX = '*SUBSET*'
     C                   ELSE
     C                   EVAL      C3SSTX = *Blanks
     C                   ENDIF                                                  F7ITEM<>*Blanks or

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate subset prompt screen fields.                                  *
      **************************************************************************
     CSR   $EditSS3      BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Item number must be valid, if non-blank.
      *
     C                   IF        F7ITEM <> *Blanks
     C     F7ITEM        CHAIN     HDIMST
     C                   IF        not %found(HDIMST)
     C                   EVAL      wk_MsgID = 'ERR0319'
     C                   EVAL      wk_MsgData = F7ITEM
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      F7IMDSX = *Blanks
     C                   EVAL      IMITEM = *Blanks
     C                   ELSE
     C                   EVAL      F7IMDSX = IMIMDS
     C                   ENDIF                                                  not %found(HDIMST)
     C                   ELSE
     C                   EVAL      F7IMDSX = *Blanks
     C                   ENDIF                                                  F7ITEM<>*Blanks

      *
      * Product class must be valid, if non-blank.
      *
     C                   IF        F7PCLS <> *Blanks
     C     F7PCLS        CHAIN     HDPCLS
     C                   IF        not %found(HDPCLS)
     C                   EVAL      wk_MsgID = 'ERR0317'
     C                   EVAL      wk_MsgData = F7ITEM
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      F7PCDS = *Blanks
     C                   ELSE
     C                   EVAL      F7PCDS = PCPCDS
     C                   ENDIF                                                  not %found(HDPCLS)
     C                   ELSE
     C                   EVAL      F7PCDS = *Blanks
     C                   ENDIF                                                  F7PCLS<>*Blanks

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize subset prompt screen indicators.                            *
      **************************************************************************
     CSR   $InitSSInd3   BEGSR

     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN89 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Reset subset prompt screen fields to initial program values.           *
      **************************************************************************
     CSR   $InitSS3      BEGSR

     C                   EVAL      F7ITEM = *Blanks
     C                   EVAL      F7IMDSX = *Blanks
     C                   EVAL      F7IMDS = *Blanks
     C                   EVAL      F7PCLS = *Blanks
     C                   EVAL      F7PCDS = *Blanks

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Refresh subset prompt screen fields.                                   *
      **************************************************************************
     CSR   $RefreshSS3   BEGSR

     C                   EVAL      F7ITEM = sv_SSItem
     C                   EVAL      F7IMDSX = sv_SSItemDescX
     C                   EVAL      F7IMDS = sv_SSItemDesc
     C                   EVAL      F7PCLS = sv_SSProdCls
     C                   EVAL      F7PCDS = sv_SSProdClsDs

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Save subset prompt screen fields.                                      *
      **************************************************************************
     CSR   $SaveSS3      BEGSR

     C                   EVAL      sv_SSItem = F7ITEM
     C                   EVAL      sv_SSItemDescX = F7IMDSX
     C                   EVAL      sv_SSItemDesc = F7IMDS
     C                   EVAL      sv_SSProdCls = F7PCLS
     C                   EVAL      sv_SSProdClsDs = F7PCDS

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid values for subset prompt screen fields.                     *
      **************************************************************************
     CSR   $ListSS3      BEGSR

     C                   SELECT

     C                   WHEN      CSRFLD = 'F7ITEM'
     C                   EXSR      $ListItems
     C                   EVAL      *IN41 = *ON

     C                   WHEN      CSRFLD = 'F7PCLS'
     C                   EXSR      $ListProdClss
     C                   EVAL      *IN43 = *ON

     C                   OTHER
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0013'
     C                   EXSR      $SndPM

     C                   SELECT
     C                   WHEN      CSRFLD = 'F7IMDS'
     C                   EVAL      *IN42 = *ON
     C                   OTHER
     C                   EVAL      *IN41 = *ON
     C                   ENDSL

     C                   ENDSL

     C                   EVAL      *IN89 = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid items.                                                      *
      **************************************************************************
     CSR   $ListItems    BEGSR

     C                   EVAL      io_Item# = F7ITEM

     C                   CALLP(E)  HHDISI (io_Item#)

     C                   IF        io_Item# = *Blanks
     C                   EVAL      wk_MsgID = 'GUM0014'
     C                   EVAL      wk_MsgData = 'F12'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      F7ITEM = io_Item#
     C     F7ITEM        CHAIN     HDIMST
     C                   IF        %found(HDIMST)
     C                   EVAL      F7IMDSX = IMIMDS
     C                   ELSE
     C                   EVAL      F7IMDSX = *Blanks
     C                   EVAL      IMITEM = *Blanks
     C                   ENDIF                                                  %found(HDCUST)
     C                   EVAL      wk_MsgID = 'MSG0144'
     C                   EVAL      wk_MsgData = F7ITEM
     C                   EXSR      $SndPM
     C                   ENDIF                                                  io_Item#=*Blanks

     C                   EVAL      *IN93 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid product classes.                                            *
      **************************************************************************
     CSR   $ListProdClss BEGSR

     C                   EVAL      io_ProdCls = F7PCLS

     C                   CALLP(E)  HHDPCS (io_ProdCls)

     C                   IF        io_ProdCls = *Blanks
     C                   EVAL      wk_MsgID = 'GUM0014'
     C                   EVAL      wk_MsgData = 'F12'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      F7PCLS = io_ProdCls
     C     F7PCLS        CHAIN     HDPCLS
     C                   IF        %found(HDPCLS)
     C                   EVAL      F7PCDS = PCPCDS
     C                   ELSE
     C                   EVAL      F7PCDS = *Blanks
     C                   ENDIF                                                  %found(HDPCLS)
     C                   EVAL      wk_MsgID = 'MSG0157'
     C                   EVAL      wk_MsgData = F7PCLS
     C                   EXSR      $SndPM
     C                   ENDIF                                                  io_ProdCls=*Blanks

     C                   EVAL      *IN93 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move carton from one pallet to another.                                *
      **************************************************************************
     CSR   $MoveCtn      BEGSR

     C     key_SC        CHAIN(N)  HYLSSCC8
     C                   IF        not %found(HYLSSCC8)
     C                   CLEAR                   HYRSSCC8
     C                   CLEAR                   HYRSCWK8
     C                   ENDIF                                                  not %found(HYLSSCC8)

     C                   IF        wk_Pallet#3 <> PMPSRL#
     C     wk_Pallet#3   CHAIN(N)  HYPPLLT
     C                   IF        not %found(HYPPLLT)
     C                   EVAL      PMPSRL# = *Zero
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYPPLLT'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %editc(wk_Pallet#3:'X')
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Press F5 to refresh.'
     C                   EVAL      *IN12 = *ON
     C                   LEAVESR
     C                   ENDIF                                                  not %found(HYPPLLT)
     C                   ENDIF                                                  wk_Pallet#3<>PMPSRL#

     C                   IF        PMSHTO <> CMCUST
     C     PMSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCUST = *Zero
     C                   EVAL      CMCNA1 = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  PMSHTO<>CMCUST

     C                   EVAL      F4SRL# = %triml(%editc(S3SRL#:'Z'))
     C                   EVAL      F4CUST = %triml(%editc(PMSHTO:'Z')) + '  ' +
     C                               CMCNA1
     C                   SELECT
     C                   WHEN      wk_EmpType = 'P'
     C                   EVAL      F4EMP = EMRNAM
     C                   WHEN      wk_EmpType = 'T'
     C                   IF        TEMIDI = *Blank
     C                   EVAL      F4EMP = %trimr(TEFNAM) + ' ' + TELNAM
     C                   ELSE
     C                   EVAL      F4EMP = %trimr(TEFNAM) + ' ' + TEMIDI + ' ' +
     C                               TELNAM
     C                   ENDIF                                                  TEMIDI=*Blank
     C                   ENDSL
     C                   EVAL      F4PSRL#1 = %triml(%editc(wk_Pallet#3:'Z'))
     C                   EVAL      F4PSRL#2 = *Blanks

     C                   EVAL      wk_ShipTo#2 = PMSHTO
     C                   EVAL      wk_DropShip#2 = PMDSHP
     C                   EVAL      wk_BOL#2 = PMBOL#

     C                   DOW       not *IN03

     C                   WRITE     MSFCTL01
     C                   EXFMT     DSPFMT4A
     C                   MOVE      *ON           *IN93
     C                   EXSR      $RMVPM
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN89 = *OFF

     C                   IF        *IN25

     C                   SELECT

      *
      * Exit or cancel.
      *
     C                   WHEN      *IN03  or  *IN12
     C                   LEAVE
     C                   ENDSL

     C                   ELSE

     C                   EXSR      $Edit4
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   EXSR      $Update4

     C                   EVAL      wk_MsgID = 'MSG0202'
     C                   EVAL      wk_MsgData =
     C                               %triml(%editc(S3SRL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:9) =
     C                               %triml(%editc(wk_Pallet#3:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:9) =
     C                               %triml(%editc(wk_Serial#:'Z'))
     C                   EXSR      $SndPM

     C                   EVAL      F4PSRL#2 = *Blanks
     C                   EVAL      wk_Refresh = *ON

     C                   LEAVE

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate move carton from one pallet to another screen.                *
      **************************************************************************
     CSR   $Edit4        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Pallet UCC-128 license plate number, a.k.a. serial number, is
      * required and must be valid.
      *
     C                   IF        F4PSRL#2 = *Blanks
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0666'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        F4PSRL#2 = '*NEW'
     C                   EVAL      wk_Serial# = -1
     C                   ELSE
     C****               IF        %check(Digits:%trim(F4PSRL#2)) > *Zero  or
     C****                           (%len(%trim(F4PSRL#2)) = 20  and
     C****                            %subst(F4PSRL#2:1:10) <> '0000090255')  or
     C****                           (%len(%trim(F4PSRL#2)) > 9  and
     C****                            %len(%trim(F4PSRL#2)) < 20)  or
     C****                            %subst(F4PSRL#2:21:1) <> ' '
     C                   IF        %check(Digits:%trim(F4PSRL#2)) > *Zero  or
     C                               (%len(%trim(F4PSRL#2)) = 20  and
     C                                (%subst(F4PSRL#2:1:4) <> '0000'  or
     C                                 (%subst(F4PSRL#2:5:6) <> wk_GS1Comp  and
     C                                  %subst(F4PSRL#2:5:6) <> wk_GS1Comp2  and
     C                                 %subst(F4PSRL#2:5:6) <> wk_GS1Comp3))) or
     C                               (%len(%trim(F4PSRL#2)) > 9  and
     C                                %len(%trim(F4PSRL#2)) < 20)  or
     C                                %subst(F4PSRL#2:21:1) <> ' '
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(F4PSRL#2)
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        %len(%trim(F4PSRL#2)) < 10
     C                   EVAL      wk_Serial# = %dec(%trim(F4PSRL#2):9:0)
     C                   ELSE
     C                   EVAL      wk_Serial# = %dec(%subst(F4PSRL#2:11:9):9:0)
     C                   ENDIF                                                  %len(%trim(F4PSRL#2)
     C     wk_Serial#    CHAIN(N)  HYPPLLT
     C                   IF        not %found(HYPPLLT)
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0667'
     C                   EVAL      wk_MsgData = %trim(F4PSRL#2)
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        PMSHTO <> wk_ShipTo#2  or
     C                               PMDSHP <> wk_DropShip#2
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0696'
     C                   EVAL      wk_MsgData = %trim(F4PSRL#2)
     C                   EVAL      %subst(wk_MsgData:21:7) =
     C                               %triml(%editc(wk_ShipTo#2:'Z'))
     C                   EXSR      $SndPM
     C                   ELSE
     C                   IF        PMBOL# <> wk_BOL#2
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0697'
     C                   EVAL      wk_MsgData = %trim(F4PSRL#2)
     C                   EVAL      %subst(wk_MsgData:21:8) =
     C                               %triml(%editc(wk_BOL#2:'Z'))
     C                   EXSR      $SndPM
     C                   ENDIF                                                  PMBOL#<>wk_BOL#2
     C                   ENDIF                                                  PMSHTO<>wk_ShipTo#2
     C                   ENDIF                                                  not %found(HYPPLLT)
     C                   ENDIF                                                  %check(Digits:%trim(
     C                   ENDIF                                                  F4PSRL#2='*NEW'
     C                   ENDIF                                                  F4PSRL#2=*Blanks

      *
      * If errors detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move carton from one pallet to another - update fields.                *
      **************************************************************************
     CSR   $Update4      BEGSR

     C                   IF        wk_Serial# = -1
     C     *LOCK         IN        PLTSRLREF
     C                   EVAL      PLTSRLREF = PLTSRLREF + 1
     C                   OUT       PLTSRLREF
     C                   EVAL      wk_Serial# = PLTSRLREF
     C                   ENDIF                                                  wk_Serial#=-1

     C     S3SRL#        SETLL     HYLSSCC8
     C                   DOU       %eof(HYLSSCC8)
     C                   EVAL      *IN01 = *OFF
     C                   EVAL      *IN02 = *OFF
     C     S3SRL#        READE     HYLSSCC8
     C                   IF        %eof(HYLSSCC8)
     C                   ITER
     C                   ENDIF                                                  %eof(HYLSSCC8)
     C                   EVAL      SCPSRL# = wk_Serial#
     C                   EVAL      SCPEMTY = wk_EmpType
     C                   EVAL      SCPEMPL = wk_Emply#
     C                   SELECT
     C                   WHEN      *IN01
     C                   UPDATE    HYRSSCC8
     C                   WHEN      *IN02
     C                   UPDATE    HYRSCWK8
     C                   ENDSL
     C                   ENDDO                                                  %eof(HYLSSCC8)

     C                   EVAL      wk_LastPallet# = wk_Pallet#3
     C                   IF        RtvPRSts1 (wk_LastPallet#) = '*PRINT'
     C                   EVAL      wk_RC0139A =
     C                               WrtPRLog (wk_LastPallet#:'*RESET')
     C                   ENDIF                                                  RtvPRSts1 (wk_LastPa

     C                   EVAL      wk_LastPallet2 = wk_Serial#
     C                   IF        RtvPRSts1 (wk_Serial#) = '*PRINT'
     C                   EVAL      wk_RC0139A =
     C                               WrtPRLog (wk_Serial#:'*RESET')
     C                   ENDIF                                                  RtvPRSts1 (wk_LastPa

     C                   IF        F4PSRL#2 = '*NEW'
     C                   EVAL      PMPSRL# = wk_Serial#
     C                   EVAL      PMSHTO = wk_ShipTo#2
     C                   EVAL      PMDSHP = wk_DropShip#2
     C                   EVAL      PMBOL# = wk_BOL#2
     C                   EVAL      PMINVC = *Blank
     C                   EVAL      PMSRC = 'P'
     C                   EVAL      PMHZLB = *Blank
     C                   EVAL      PMEMTY = wk_EmpType
     C                   EVAL      PMEMPL = wk_Emply#
     C                   EVAL      PMOSTP = %timestamp
     C                   EVAL      PMTSTP = PMOSTP
     C                   EVAL      PMLEN = DftPLength
     C                   EVAL      PMWID = DftPWidth
     C                   WRITE     HYRPLLT
     c* temp code start
     C                   MOVE      '4'           POPT              1
     C                   CALL      'TESTMH'
     C                   PARM                    PMPSRL#
     C                   PARM                    POPT
     c* temp code end
     C                   ELSE
     C     wk_Serial#    CHAIN     HYPPLLT
     C                   IF        %found(HYPPLLT)
     C                   EVAL      PMTSTP = %timestamp
     C                   UPDATE    HYRPLLT
     C                   ENDIF                                                  %found(HYPPLLT)
     C                   ENDIF                                                  F4PSRL#2='*NEW'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display carton detail.                                                 *
      **************************************************************************
     CSR   $DisplayCtn   BEGSR

     C     key_SC        CHAIN(N)  HYLSSCC8
     C                   IF        not %found(HYLSSCC8)
     C                   CLEAR                   HYRSSCC8
     C                   CLEAR                   HYRSCWK8
     C                   ENDIF                                                  not %found(HYLSSCC8)

     C                   IF        PMSHTO <> CMCUST
     C     PMSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCUST = *Zero
     C                   EVAL      CMCNA1 = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)
     C                   ENDIF                                                  PMSHTO<>CMCUST

     C                   EVAL      F5PSRL# = %triml(%editc(wk_Pallet#3:'Z'))
     C                   EVAL      F5SRL# = %triml(%editc(S3SRL#:'Z'))
     C                   EVAL      F5CUST = %triml(%editc(PMSHTO:'Z')) + '  ' +
     C                               CMCNA1
     C                   EVAL      F5ORD# = %triml(%editc(S3ORD#:'Z'))
     C                   EVAL      F5ORL# = %triml(%editc(S3ORL#:'Z'))
     C                   EVAL      F5ITEM = SCITEM
     C                   EVAL      F5IMDS = SCIMDS
     C                   EVAL      F5LOT# = SCLOT#
     C                   EVAL      F5UOMQ = %triml(%editc(SCUOMQ:'J'))
     C                   IF        %subst(F5UOMQ:%len(%trimr(F5UOMQ))-4:5) =
     C                               '.0000'
     C                   EVAL      F5UOMQ =
     C                               %subst(F5UOMQ:1:%len(%trimr(F5UOMQ))-5)
     C                   ENDIF                                                  %subst(F5UOMQ:%len(%
     C                   EVAL      F5EWGT = %triml(%editc(SCEWGT:'J'))
     C                   EVAL      F5TSTP = %char(%date(SCTSTP):*USA) + '  ' +
     C                               %char(%time(SCTSTP):*USA)
     C     S3SRL#        CHAIN     HYLMPCD2
     C                   IF        %found(HYLMPCD2)
     C                   EVAL      F5MP# = MDMP#
     C                   ELSE
     C                   EVAL      F5MP# = '*NONE'
     C                   ENDIF                                                  %found(HYLMPCD2)

     C                   DOW       not *IN03

     C                   WRITE     MSFCTL01
     C                   EXFMT     DSPFMT5A
     C                   MOVE      *ON           *IN93
     C                   EXSR      $RMVPM
     C                   EVAL      *IN40 = *OFF

     C                   IF        *IN25

     C                   SELECT
     C                   WHEN      *IN12
     C                   LEAVE
     C                   ENDSL

     C                   ELSE

     C                   LEAVE

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move HYPEDWK records to HYPELD.                                        *
      **************************************************************************
     CSR   $MoveToED     BEGSR

     C                   FOR       idx4a = 1 to idx4
     C                   EVAL      wk_Order#2 = ary_HLOrd(idx4a)
     C                   CALLP(E)  HYR0152 (BldELD:wk_Order#2:*Zero:*Zero:
     C                              *Zero:*Zero:*Zero:*Blanks:*Zero)
     C                   ENDFOR                                                 idx4a=1 to idx4
     C                   EVAL      wk_Pgm0152 = *ON
     C                   EVAL      ary_HLOrd = *Zero
     C                   EVAL      idx4 = *Zero
     C                   EVAL      idx4a = *Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display and process generic warning window.                            *
      **************************************************************************
     CSR   $GenWarn      BEGSR

     C                   EXFMT     WINDOW01D

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Reset order drop ships.                                                *
      **************************************************************************
     CSR   $ResetDS      BEGSR

     C                   EXSR      $Edit3
     C                   IF        wk_Error
     C                   EVAL      wk_F22Srl# = *Hival
     C                   LEAVESR
     C                   ENDIF                                                  wk_Error

     C                   IF        wk_Serial# <> wk_F22Srl#
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR0717'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      wk_F22Srl# = *Hival
     C                   LEAVESR
     C                   ENDIF                                                  wk_Serial#<>wk_F22Sr

     C                   CALLP(E)  ResetDropShips(wk_PSrl#:OEORD#)
     C                   SELECT
     C                   WHEN      %error
     C                   OTHER
     C                   EVAL      wk_DropShip# = *Zero
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check whether a pallet report needs to be printed, and, if so,         *
      * print it.                                                              *
      **************************************************************************
     CSR   $ChkPlltRpt   BEGSR

     C                   IF        RtvPRSts1 (wk_LastPallet#) = '*PRINT'
     C                   LEAVESR
     C                   ENDIF                                                  RtvPRSts1 (wk_LastPa

      /Free
       wk_BillTo#X = %editc(wk_BillTo#:'X');
       wk_TDKEY2 = *Blanks;
       EXEC SQL select TDKEY2 into :wk_TDKEY2
                from GUPTDAT
                where TDTABL='BUYINGGRPS' and TDKEY1=:wk_DatabaseID and
                 TDKEY2=:wk_BillTo#X;
       IF wk_TDKEY2 = wk_BillTo#X;
         wk_BuyGrpCust = *ON;
       ELSE;
         wk_BuyGrpCust = *OFF;
       ENDIF;                                                                // wk_TDKEY2=wk_BillTo#
       wk_CMPLDR = *Blanks;
       IF wk_BuyGrpCust;
         EXEC SQL select CMPLDR into :wk_CMPLDR
                  from HDCUST2
                  where CMCUST = :wk_ShipTo#;
       ELSE;
         IF wk_BillTo# = 59;
           wk_CMCNA1 = *Blanks;
           EXEC SQL select CMCNA1 into :wk_CMCNA1
                    from HDCUST
                    where CMCUST=:wk_ShipTo#;
         ENDIF;                                                              // wk_BillTo#=59
         IF wk_BillTo# = 59  and  %scan('CABELA':wk_CMCNA1) > *Zero;
           EXEC SQL select CMPLDR into :wk_CMPLDR
                    from HDCUST2
                    where CMCUST=37;
         ELSE;
           EXEC SQL select CMPLDR into :wk_CMPLDR
                    from HDCUST2
                    where CMCUST=:wk_BillTo#;
         ENDIF;                                                              // wk_BillTo#=59 and
       ENDIF;                                                                // wk_BuyGrpCust
       IF wk_CMPLDR <> 'Y';
         LEAVESR;
       ENDIF;                                                                // wk_CMPLDR<>'Y'
      /End-Free

     C                   EVAL      W2PSRL# =
     C                                %triml(%editc(wk_LastPallet#:'Z')) + '?'
     C                   EVAL      W2PRT = *Blank
     C                   DOU       *IN12
     C                   EXFMT     WINDOW02
     C                   SELECT
     C                   WHEN      *IN12
     C                   LEAVESR
     C                   WHEN      *IN13
     C                   EVAL      wk_RC0139A =
     C                               WrtPRLog (wk_LastPallet#:'*SKIP')
     C                   EVAL      *IN13 = *OFF
     C                   LEAVESR
     C                   OTHER
     C                   IF        W2PRT = 'Y'
     C                   LEAVE
     C                   ENDIF                                                   W1PRT='Y'
     C                   ENDSL
     C                   ENDDO                                                  *IN12

     C                   EVAL      wk_PSrl# = wk_LastPallet#
     C                   EVAL      wk_PlltPrtType = '*AUTO'
     C                   EXSR      $PrtPallet

     C                   EVAL      wk_LastPallet# = *Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print pallet report.                                                   *
      **************************************************************************
     CSR   $PrtPallet    BEGSR

     C                   IF        wk_OutQ1 = *Blanks
     C                   EVAL      wk_OutQType = 'PALLETRPT'
     C                   EXSR      $RtvOutQ
     C                   EVAL      wk_OutQ1 = wk_OutQ
     C                   ENDIF                                                  wk_OutQ1=*Blanks
     C                   SELECT
     C                   WHEN      wk_OutQ1 = 'PRT16'
     C                   EVAL      wk_Drawer = 5
     C                   OTHER
     C                   EVAL      wk_Drawer = 3
     C                   ENDSL
     C                   EVAL      wk_Copies = 1

     C****               IF        wk_BillTo# <> 52                             Academy
     C****               EVAL      wk_ShowPO = '*NO'
     C****               ELSE
     C                   EVAL      wk_ShowPO = '*YES'
     C****               ENDIF                                                  wk_BillTo#<>52
     C                   EVAL      wk_Item#Type = '*CUST'

     C                   IF        wk_PlltPrtType = '*AUTO'
     C                   CALLP(E)  HYC3512 (wk_PSrl#:-2:-1:'*YES':wk_ShowPO:
     C                               wk_Item#Type:
     C                               '*NO ':
     C                               '*YES':'*NONE':wk_OutQ1:
     C                               wk_Drawer:wk_Copies:'*NO':'*NO')
     C                   ELSE
     C                   CALLP(E)  HYC3512 (wk_PSrl#:-1:-1:'*YES':wk_ShowPO:
     C                               wk_Item#Type:
     C                               '*NO ':
     C                               '*YES':'*NONE':wk_OutQ1:
     C                               wk_Drawer:wk_Copies:'*NO':'*NO')
     C                   ENDIF                                                  wk_PlltPrtType='*AUT

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print pallet labels.                                                   *
      **************************************************************************
     CSR   $PrtLabels    BEGSR

     C                   IF        wk_OutQ2 = *Blanks
     C                   EVAL      wk_OutQType = 'PALLETLBL'
     C                   EXSR      $RtvOutQ
     C                   EVAL      wk_OutQ2 = wk_OutQ
     C                   EVAL      wk_Copies = 3
     C                   EVAL      wk_Cmd = 'OVRPRTF FILE(BCLABELS) +
     C                               OUTQ(' + %trimr(wk_OutQ2) +
     C                               ') OVRSCOPE(*CALLLVL)'
     C                   CALLP(E)  QCMDEXC (wk_Cmd:%len(%triml(wk_Cmd)))
     C                   ENDIF                                                  wk_OutQ2=*Blanks

     C                   CALLP(E)  PalletLbl (wk_PSrl#:wk_Copies)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print pallet banner.                                                   *
      **************************************************************************
     CSR   $PrtBanner    BEGSR

     C                   IF        wk_OutQ3 = *Blanks
     C                   EVAL      wk_OutQType = 'PALLETBANR'
     C                   EXSR      $RtvOutQ
     C                   EVAL      wk_OutQ3 = wk_OutQ
     C                   ENDIF                                                  wk_OutQ1=*Blanks
     C                   SELECT
     C                   WHEN      wk_OutQ3 = 'PRT16'
     C                   EVAL      wk_Drawer = 5
     C                   OTHER
     C                   EVAL      wk_Drawer = 3
     C                   ENDSL
     C                   EVAL      wk_Copies = 1

     C                   CALLP(E)  HYC3550 (wk_PSrl#:'1':'1':wk_OutQ3:wk_Drawer:
     C                               wk_Copies:'*NO':ps_Pgnm:wk_RC3550)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print FBI pallet banner.                                               *
      **************************************************************************
     CSR   $PrtBanner2   BEGSR

     C                   IF        wk_OutQ3 = *Blanks
     C                   EVAL      wk_OutQType = 'PALLETBANR'
     C                   EXSR      $RtvOutQ
     C                   EVAL      wk_OutQ3 = wk_OutQ
     C                   ENDIF                                                  wk_OutQ1=*Blanks
     C                   SELECT
     C                   WHEN      wk_OutQ3 = 'PRT16'
     C                   EVAL      wk_Drawer = 5
     C                   OTHER
     C                   EVAL      wk_Drawer = 3
     C                   ENDSL
     C                   EVAL      wk_Copies = 1

     C                   CALLP(E)  HYC3550 (wk_PSrl#:'2':'1':wk_OutQ3:wk_Drawer:
     C                               wk_Copies:'*NO':ps_Pgnm:wk_RC3550)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print pallet hazard labels.                                            *
      **************************************************************************
     CSR   $PrtHazLbl1   BEGSR

     C                   EVAL      wk_RC0552 = PrtHazLbl (*Zero:*Zero:
     C                               wk_PSrl#:*Zero:'P')

     C                   EVAL      wk_Pgm0552 = *ON

     C                   IF        wk_RC0552 = *Zero
     C                   EVAL      wk_MsgID = 'MSG0506'
     C                   EVAL      wk_MsgData = %char(wk_PSrl#)
     C                   EXSR      $SndPM
     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR9013'
     C                   EVAL      wk_MsgData = 'PrtHazLbl'
     C                   EVAL      %subst(wk_MsgData:16:5) = %char(wk_RC0552)
     C                   EXSR      $SndPM
     C                   EVAL      *IN28 = *ON
     C                   ENDIF                                                  wk_RC0552=*Zero

     C                   UPDATE    SFL01

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print carton hazard label.                                             *
      **************************************************************************
     CSR   $PrtHazLbl2   BEGSR

     C                   EVAL      wk_RC0552 = PrtHazLbl (*Zero:*Zero:
     C                               *Zero:S3SRL#:'P')

     C                   EVAL      wk_Pgm0552 = *ON

     C                   IF        wk_RC0552 = *Zero
     C                   EVAL      wk_MsgID = 'MSG0507'
     C                   EVAL      wk_MsgData = %char(S3SRL#)
     C                   EXSR      $SndPM
     C                   EVAL      S3OPT = *Blanks
     C                   EVAL      *IN28 = *OFF
     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR9013'
     C                   EVAL      wk_MsgData = 'PrtHazLbl'
     C                   EVAL      %subst(wk_MsgData:16:5) = %char(wk_RC0552)
     C                   EXSR      $SndPM
     C                   EVAL      *IN28 = *ON
     C                   ENDIF                                                  wk_RC0552=*Zero

     C                   UPDATE    SFL03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve output queue.                                                 *
      **************************************************************************
     CSR   $RtvOutQ      BEGSR

      *
      * Check for absolute workstation output queue assignment.
      *
     C                   EVAL      kf_WSID = ps_Job
     C                   EVAL      kf_OQTY = wk_OutQType
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)

      *
      * Check for generic workstation output queue assignment.
      *
     C                   EVAL      wk_Wkstn = ps_Job
     C****               EVAL      wk_Len = %checkr(' ':wk_Wkstn) - 1
     C                   EVAL      wk_Len = %checkr(' ':wk_Wkstn)
     C                   DOU       wk_Len = *Zero
     C                   EVAL      kf_WSID =
     C                               %triml(%subst(wk_Wkstn:1:wk_Len)) + '*'
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        %found(HYPWSOQ)
     C                   LEAVE
     C                   ENDIF                                                  %found(HYPWSOQ)
     C                   EVAL      wk_Len = wk_Len - 1
     C                   ENDDO                                                  wk_Len=*Zero

      *
      * Retrieve default output queue.
      *
     C                   IF        wk_Len = *Zero
     C                   EVAL      kf_WSID = '*DFT'
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      WOOUTQ = '*JOB'
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   ENDIF                                                  wk_Len=*Zero

     C                   ENDIF                                                  not %found(HYPWSOQ)

     C                   EVAL      wk_OutQ = WOOUTQ

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Remove blank lines from address.                                       *
      **************************************************************************
     CSR   $SquishAddr   BEGSR

     C                   EVAL      idx = *Zero
     C                   EVAL      CustAddr = *Blanks
     C                   IF        CMCNA2 <> *Blanks
     C                   EVAL      idx = idx + 1
     C                   EVAL      CustAddr(idx) = CMCNA2
     C                   ENDIF                                                  CMCNA2<>*Blanks
     C                   IF        CMCNA3 <> *Blanks
     C                   EVAL      idx = idx + 1
     C                   EVAL      CustAddr(idx) = CMCNA3
     C                   ENDIF                                                  CMCNA3<>*Blanks
     C                   IF        CMCNA4 <> *Blanks
     C                   EVAL      idx = idx + 1
     C                   EVAL      CustAddr(idx) = CMCNA4
     C                   ENDIF                                                  CMCNA4<>*Blanks
     C                   EVAL      idx = idx + 1
     C                   IF        CMCCTY <> *Blanks
     C                   EVAL      CustAddr(idx) = CMCCTY
     C                   IF        CMST <> *Blanks
     C                   EVAL      CustAddr(idx) = %trimr(CustAddr(idx)) + ','
     C                   ENDIF                                                  CMST<>*Blanks
     C                   ENDIF                                                  CMCITY<>*Blanks
     C                   EVAL      CustAddr(idx) = %trimr(CustAddr(idx)) + ' ' +
     C                               CMST
     C                   EVAL      CustAddr(idx) = %trimr(CustAddr(idx)) +
     C                               '  ' + CMZIP

     C                   DO        3             idx
     C                   IF        CustAddr(idx) = *Blanks
     C                   EVAL      idx2 = idx
     C                   DOU       CustAddr(idx) <> *Blanks  or
     C                               idx2 = 4
     C                   EVAL      idx2 = idx2 + 1
     C                   EVAL      CustAddr(idx) = CustAddr(idx2)
     C                   EVAL      CustAddr(idx2) = *Blanks
     C                   ENDDO                                                  CustAddr(idx)<>*Blan
     C                   ENDIF                                                  CustAddr(idx)=*Blank
     C                   ENDDO                                                  DO 3

     C                   IF        wk_BOL# = *Zero
     C                   IF        CMCTRY <> 'US'
     C                   DO        4             idx
     C                   IF        CustAddr(idx) = *Blanks
     C     CMCTRY        CHAIN     HDCTRY
     C                   IF        %found(HDCTRY)
     C                   EVAL      CustAddr(idx) = CNCDES
     C                   ELSE
     C                   EVAL      CustAddr(idx) = CMCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   EVAL      CMCTRY = *Blanks
     C                   LEAVE
     C                   ENDIF                                                  CustAddr(idx)=*Blank
     C                   ENDDO                                                  DO 4
     C                   ENDIF                                                  SDCTRY<>'US'
     C                   ELSE
     C                   IF        SDCTRY <> 'US'
     C                   DO        4             idx
     C                   IF        CustAddr(idx) = *Blanks
     C     SDCTRY        CHAIN     HDCTRY
     C                   IF        %found(HDCTRY)
     C                   EVAL      CustAddr(idx) = CNCDES
     C                   ELSE
     C                   EVAL      CustAddr(idx) = SDCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   EVAL      SDCTRY = *Blanks
     C                   LEAVE
     C                   ENDIF                                                  CustAddr(idx)=*Blank
     C                   ENDDO                                                  DO 4
     C                   ENDIF                                                  SDCTRY<>'US'
     C                   ENDIF                                                  wk_BOL#=*Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve pallet total cartons and weight.                              *
      **************************************************************************
     CSR   $RtvPltTots   BEGSR

     C                   EVAL      wk_TotCtn = *Zero
     C                   EVAL      wk_TotWgt = *Zero
     C                   EVAL      sv_Serial# = *Hival
     C                   EVAL      sv_Order# = *Hival
     C                   EVAL      wk_ShipVia = *Blanks
     C                   EVAL      wk_ShipViaDesc = *Blanks
     C                   EVAL      ary_MP = *Blanks
     C                   EVAL      idx3 = *Zero

     C     wk_Pallet#2   SETLL     HYLSSCC9
     C                   DOU       %eof(HYLSSCC9)
     C     wk_Pallet#2   READE(N)  HYLSSCC9
     C                   IF        %eof(HYLSSCC9)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC9)
     C                   EVAL(H)   wk_ExtWgt = SCUWGT * SCUOMQ
     C                   EVAL      wk_TotWgt = wk_TotWgt + wk_ExtWgt
     C                   IF        SCSRL# = sv_Serial#
     C                   ITER
     C                   ENDIF                                                  SCSRL#=sv_Serial#
     C     SCSRL#        CHAIN     HYLMPCD2
     C                   IF        %found(HYLMPCD2)
     C                   IF        %lookup(MDMP#:ary_MP:1:idx3) > *Zero
     C                   ITER
     C                   ELSE
     C                   EVAL      idx3 = idx3 + 1
     C                   EVAL      ary_MP(idx3) = MDMP#
     C                   ENDIF                                                  %lookyp(MDMP#:ary_MP
     C                   ENDIF                                                  %found(HYLMPCD2)
     C                   EVAL      wk_TotCtn = wk_TotCtn + 1
     C                   IF        sv_Order# <> SCORD#
     C     SCORD#        CHAIN     OEORHD
     C                   IF        %found(OEORHD)
     C                   SELECT
     C                   WHEN      wk_ShipVia = *Blanks
     C                   EVAL      wk_ShipVia = OESVSV
     C                   EVAL      wk_ShipViaDesc = OESVDS
     C                   WHEN      wk_ShipVia = '@@'
     C                   WHEN      wk_ShipVia <> OESVSV
     C                   EVAL      wk_ShipVia = '@@'
     C                   EVAL      wk_ShipViaDesc = *Blanks
     C                   ENDSL
     C                   ENDIF                                                  %found(OEORHD)
     C                   EVAL      sv_Order# = SCORD#
     C                   ENDIF                                                  sv_Order#<>SCORD#
     C                   IF        wk_ShipVia = '@@'
     C                   EVAL      wk_ShipVia = *Blanks
     C                   ENDIF                                                  wk_ShipVia='@@'
     C                   EVAL      sv_Serial# = SCSRL#
     C                   ENDDO                                                  %eof(HYLSSCC9)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check pallet total cartons and weight.                                 *
      **************************************************************************
     CSR   $ChkPltTots   BEGSR

     C                   EVAL      wk_TotCtnX = *Zero
     C                   EVAL      wk_TotWgtX = *Zero
     C                   EVAL      sv_Serial# = *Hival
     C                   EVAL      ary_MP = *Blanks
     C                   EVAL      idx3 = *Zero

     C     wk_Pallet#2   SETLL     HYLSSCC9
     C                   DOU       %eof(HYLSSCC9)
     C     wk_Pallet#2   READE(N)  HYLSSCC9
     C                   IF        %eof(HYLSSCC9)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC9)
     C                   EVAL(H)   wk_ExtWgt = SCUWGT * SCUOMQ
     C                   EVAL      wk_TotWgtX = wk_TotWgtX + wk_ExtWgt
     C                   IF        SCSRL# = sv_Serial#
     C                   ITER
     C                   ENDIF                                                  SCSRL#=sv_Serial#
     C     SCSRL#        CHAIN     HYLMPCD2
     C                   IF        %found(HYLMPCD2)
     C                   IF        %lookup(MDMP#:ary_MP:1:idx3) > *Zero
     C                   ITER
     C                   ELSE
     C                   EVAL      idx3 = idx3 + 1
     C                   EVAL      ary_MP(idx3) = MDMP#
     C                   ENDIF                                                  %lookyp(MDMP#:ary_MP
     C                   ENDIF                                                  %found(HYLMPCD2)
     C                   EVAL      wk_TotCtnX = wk_TotCtnX + 1
     C                   EVAL      sv_Serial# = SCSRL#
     C                   ENDDO                                                  %eof(HYLSSCC9)

     C                   EVAL(H)   wk_TotWgtXX = wk_TotWgtX
     C                   IF        wk_TotCtnX <> F3TCTN  or
     C                               wk_TotWgtXX <> F3TWGT
     C                   EVAL      *IN99 = *ON
     C                   EVAL      F3MSG = x'2B' + '    ** ERROR ** - Total +
     C                               carton count and/or total weight are +
     C                               incorrect!    ' + x'20'
     C                   EVAL      wk_MsgID = 'ERR9999'
     C                   EVAL      wk_MsgData = 'Resummarized   Count: ' +
     C                               %triml(%editc(wk_TotCtnX:'J')) +
     C                               '  Weight: ' +
     C                               %triml(%editc(wk_TotWgtXX:'J'))
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_TotCtnX<>F3TCTN

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve pallet BOL#.                                                  *
      **************************************************************************
     CSR   $RtvBOL#      BEGSR

     C                   EVAL      wk_BOL#2 = *Zero
     C     wk_Pallet#    SETLL     HYLSSCC9
     C                   DOU       %eof(HYLSSCC9)
     C     wk_Pallet#    READE(N)  HYLSSCC9
     C                   IF        %eof(HYLSSCC9)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC9)
     C                   IF        wk_BOL#2 = *Zero
     C                   EVAL      wk_BOL#2 = SCBOL#
     C                   ELSE
     C                   IF        SCBOL# <> wk_BOL#2
     C                   EVAL      wk_BOL#2 = -1
     C                   ENDIF                                                  SCBOL#<>wk_BOL#2
     C                   ENDIF                                                  wk_BOL#2=*Zero
     C                   ENDDO                                                  %eof(HYLSSCC9)

     C                   IF        wk_BOL#2 = -1
     C                   EVAL      wk_BOL# = *Zero
     C                   ELSE
     C                   EVAL      wk_BOL# = wk_BOL#2
     C                   ENDIF                                                  wk_BOL#2=-1

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Send message to program message queue.                                 *
      **************************************************************************
     CSR   $SndPM        BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Send message to program message queue using QCPFMSG.                   *
      **************************************************************************
     CSR   $SndPM2       BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'QCPFMSG   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Remove messages from program message queue.                            *
      **************************************************************************
     CSR   $RmvPM        BEGSR

     C                   CALLP(E)  QMHRMVPM ('*' : *Zero : *Blanks : '*ALL' :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR

     C                   CALLP(E)  sd_HYR0139

     C                   IF        wk_Pgm0152
     C                   CALLP(E)  HYR0152 (ShutDown:*Zero:*Zero:*Zero:
     C                              *Zero:*Zero:*Zero:*Blanks:*Zero)
     C                   ENDIF                                                  wk_Pgm0152

     C                   IF        wk_Pgm0156
     C                   CALLP(E)  HYR0156 (*Zero:wk_Order#X:wk_Turn#X:*Zero:
     C                               *OFF:*OFF:*ON:wk_RC0156)
     C                   ENDIF                                                  wk_Pgm0156

     C                   IF        wk_Pgm0552
     C                   CALLP(E)  sd_HYR0552
     C                   ENDIF                                                  wk_Pgm0552

     C                   IF        wk_Pgm0622
     C                   CALLP(E)  sd_HYR0622
     C                   ENDIF                                                  wk_Pgm0622

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - each time program is called.                  *
      **************************************************************************
     CSR   $InzSR        BEGSR

     C                   SELECT
     C                   WHEN      wk_PgmMode = EntryMode
     C                   EVAL      F1HDG1 = EntryHdg1
     C                   WHEN      wk_PgmMode = MaintMode
     C                   EVAL      F1HDG1 = MaintHdg1
     C                   WHEN      wk_PgmMode = InqMode
     C                   EVAL      F1HDG1 = InqHdg1
     C                   ENDSL

     C                   EVAL      F1HDG1 = center(F1HDG1)
     C                   EVAL      F2HDG1 = F1HDG1
     C                   EVAL      F3HDG1 = F1HDG1
     C                   EVAL      F4HDG1 = F1HDG1
     C                   EVAL      F5HDG1 = F1HDG1
     C                   EVAL      F6HDG1 = F1HDG1
     C                   EVAL      F7HDG1 = F1HDG1
     C                   EVAL      F8HDG1 = F1HDG1
     C                   EVAL      C1HDG1 = F1HDG1
     C                   EVAL      C3HDG1 = F1HDG1

     C                   IF        wk_PgmMode = EntryMode
     C                   EVAL      F3HDG2A = NewPallet
     C                   EVAL      F3HDG2A = center(F3HDG2A)
     C                   EVAL      F3HDG2B = AddCarton
     C                   EVAL      F3HDG2B = center(F3HDG2B)
     C                   EVAL      C1HDG2A = PalletSelect
     C                   EVAL      C1HDG2A = center(C1HDG2A)
     C                   ENDIF                                                  wk_PgmMode=EntryMode

     C                   EVAL      F5HDG2 = InqHdg5
     C                   EVAL      F5HDG2 = center(F5HDG2)

     C                   EVAL      C3HDG2A = MaintHdg3
     C                   EVAL      C3HDG2A = center(C3HDG2A)
     C                   EVAL      C3HDG2B = C3HDG2A
     C                   EVAL      C3HDG2C = InqHdg3
     C                   EVAL      C3HDG2C = center(C3HDG2C)

     C                   IN        wk_LDA
     C                   IF        %subst(wk_LDA:1:5) = 'SHTO:'
     C                   EVAL      F6SHTO = %dec(%subst(wk_LDA:6:7):7:0)
     C                   ENDIF                                                  %subst(wk_LDA:1:5)=
     C                   IF        %subst(wk_LDA:14:7) = '*HAZLBL'
     C                   IF        %subst(wk_LDA:21:3) = '(Y)'
     C                   EVAL      wk_HazLbl = *ON
     C                   ELSE
     C                   EVAL      wk_HazLbl = *OFF
     C                   ENDIF                                                  %subst(wk_LDA:21:3)=
     C                   EVAL      wk_ShipGrp# = %dec(%subst(wk_LDA:26:9):9:0)
     C                   EVAL      wk_FromBxlgx = *ON
     C                   ELSE
     C                   EVAL      wk_HazLbl = *OFF
     C                   EVAL      wk_ShipGrp# = *Zero
     C                   EVAL      wk_FromBxlgx = *OFF
     C                   ENDIF                                                  %subst(wk_LDA:14:7)=
     C                   EVAL      %subst(wk_LDA:1:34) = *Blanks
     C                   OUT       wk_LDA

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - first time only.                              *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define  Key Lists
      *
     C     key_SC        KLIST                                                  HYLSSCC8
     C                   KFLD                    S3SRL#
     C                   KFLD                    S3ORD#
     C                   KFLD                    S3ORL#
     C                   KFLD                    S3BLN#
     C                   KFLD                    S3LOT#

     C     key_TD        KLIST                                                  GUPTDAT
     C                   KFLD                    kf_Table
     C                   KFLD                    kf_Key1

     C     key_TD3       KLIST                                                  GUPTDAT
     C                   KFLD                    kf_Table
     C                   KFLD                    kf_Key1
     C                   KFLD                    kf_Key2
     C                   KFLD                    kf_Key3

     C     key_WO        KLIST                                                  HYPWSOQ
     C                   KFLD                    kf_WSID
     C                   KFLD                    kf_OQTY

     C     RFSKEY1       KLIST                                                  SFL01 - HYPPLLT
     C                   KFLD                    @UPSRL#

     C     PGUKEY1       KLIST                                                  SFL01 - HYPPLLT
     C                   KFLD                    @UPSRL#

     C     PGDKEY1       KLIST                                                  SFL01 - HYPPLLT
     C                   KFLD                    @DPSRL#

     C     RFSKEY3       KLIST                                                  SFL03 - HYLSSCC9
     C                   KFLD                    wk_Pallet#3
     C                   KFLD                    @USRL#
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @ULOT#

     C     PGUKEY3       KLIST                                                  SFL03 - HYLSSCC9
     C                   KFLD                    wk_Pallet#3
     C                   KFLD                    @USRL#
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @ULOT#

     C     PGDKEY3       KLIST                                                  SFL03 - HYLSSCC9
     C                   KFLD                    wk_Pallet#3
     C                   KFLD                    @DSRL#
     C                   KFLD                    @DORD#
     C                   KFLD                    @DORL#
     C                   KFLD                    @DBLN#
     C                   KFLD                    @DLOT#

      *
      * Initialize Work Fields
      *
     C                   EVAL      S@PGMQ = '*'
     C                   EVAL      S@RCNO = *Zero
     C                   EVAL      P@PGNM = ps_Pgnm
     C                   EVAL      P@JOBN = ps_Job
     C                   EVAL      F3RESET = 'Press the Reset key to continue.'

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID
     C                   IF        wk_DatabaseID = 'HY'
     C                               or  wk_DataBaseID = 'PV'
     C                               or  wk_DataBaseID = 'SS'
     C                   EVAL      F1ENV = *Blanks
     C                   EVAL      C1ENV = *Blanks
     C                   ELSE
     C                   EVAL      F1ENV = '*TEST*'
     C                   EVAL      C1ENV = '*TEST*'
     C                   ENDIF                                                  wk_DatabaseID='HY'

      *
      * Retrieve current GS1 company prefix (e.g., 090255 or 851529).
      *
     C                   IN        wk_GS1Comp
     C                   IN        wk_GS1Comp2
     C                   IF        wk_GS1Comp2 = *Blanks
     C                   EVAL      wk_GS1Comp2 = *Hival
     C                   ENDIF                                                  wk_GS1Comp2=*Blanks

     C                   IF        wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   EVAL      wk_GS1Comp3 = '090255'
     C                   ELSE
     C                   EVAL      wk_GS1Comp3 = *Hival
     C                   ENDIF                                                  wk_DatabaseID='SS'

      *
      * Retrieve how to handle duplicate carton checking for BoxLogix manually
      * shipped cartons.  Valid values are:
      *           *NOCHK - Don't check for BoxLogix manually-shipped cartons,
      *                    nor process differently than other duplicate cartons.
      *           Bypass word/name - a word or name that can be used to bypass
      *                    the entry of a lead person's credentials.
      *
     C                   IN        wk_BXLDupChk

     CSR                 ENDSR

     C/EJECT
