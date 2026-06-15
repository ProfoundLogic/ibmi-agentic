     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2018')
     H BNDDIR('HYR0614')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Consolidated Shipping subprocedures                       *
      *          BY: William Earl (MCA)                                        *
      *        DATE: September 2018                                            *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  10/25/2019  MCA  Use RY (archive) HYPCCRH MFUKEY, if found, to create *
      *                   any new credit card referential authorization.       *
      *                   Tkt #4714                                            *
      *  12/13/2019  LWB  WSOSHAG - Agilx post to authorize ecommerce credit   *
      *                             term 'EC' payment for shipment.            *
      *                   Don't send order status emails to customer if Agilx  *
      *                   ecommerce order.  OEOSUS = 'WSECS'.                  *
      *                   WSOACCPAY - Authorize ecommerce credit card payment  *
      *                               type 'EC' for shipment.                  *
      *  04/17/2020  MCA  Changes to pallet report.  Change to print with      *
      *                   PO# and to use customer's item number if available.  *
      *  05/22/2020  MCA  Remove any HDITSL reserved quantities from completed *
      *                   shipments with pallet picks that were not shipped.   *
      *  04/05/2021  MCA  For orders using Varsity ShipSoft, update files for  *
      *                   inquiry.                                             *
      *  06/17/2021  MCA  For CP orders, check if OEPAYO.OPPAYA is less than   *
      *                   current shipment amount, and only if true, then      *
      *                   check (Curbstone server) C3FP9020.MFSETR vs          *
      *                   OEORHH.HHTIVA to determine whether to call           *
      *                   SettleFinal.                                         *
      *  09/06/2021  MCA  In CompCustShip, auto-release fully-shipped retail   *
      *                   and employee orders, that were only on intransit     *
      *                   hold because they were set to not allow backorders.  *
      *  11/16/2021  MCA  In CompCustShip, print ocean-compliant BOL when      *
      *                   necessary.                                           *
      *  02/17/2022  MCA  In CompCustShip, update HYPTDTA TDVORD and TDTRK#    *
      *                   before calling COEPIN.                               *
      *  04/24/2022  MCA  In CompCustShip, auto-release intransit hold for     *
      *                   fully-shipped non-foreign drop-shipment orders,      *
      *                   regardless of customer type (i.e., location no.).    *
      *                   Tkt #29856                                           *
      *  08/22/2022  MCA  New Varsity modules go-live with normal              *
      *                   (non-BoxLogix) shipping program.                     *
      *  11/01/2022  MCA  Check shipment type before calling Varsity.  This is *
      *                   needed now because we need to change truck ship-vias *
      *                   in SHIPVIA table to be set to go to Varisty, and     *
      *                   this program is not yet ready for those settings.    *
      *  04/27/2023  MCA  Pass order# and turn# arrays to HYR0500 because      *
      *                   program interface changed for SWOG mode (not         *
      *                   currently used in this program).                     *
      *  04/28/2023  MCA  In CompCustShip, for truck shipments, ensure all     *
      *                   cartons for the current customer shipment have been  *
      *                   palletized.                                          *
      *  07/07/2023  LWB  Call WSOSHAG if eComm Store or Support Portal order. *
      *  08/19/2023  MCA  In CompCustShip, if a shipment will create a BOL,    *
      *                   and if a PRO number has not yet been assigned,       *
      *                   retrieve and assign the next available PRO number.   *
      *  10/04/2023  LWB  Send order status emails to customer if Agilx        *
      *  12/12/2023  LWB  Pass FrtAmtO to procedure InvcReq to set *ON invoice *
      *                   required flag wk_InvcReq if FrtAmtO <> 0.            *
      *  01/01/2024  LWB  WSOSHAG parameters update.                           *
      *  01/10/2024  MCA  In CompCustShip - Carton UCC-128 label print for     *
      *                   multiple-carton UCC-128 labels.                      *
      *  02/02/2024  LWB  Update PI RtvShipAmt to EXSR $WrtTaxRcds and         *
      *                   $CalcRtvTaxes if wk_FrtAmt <> *Zero.                 *
      *  09/03/2024  MCA  Automatic pallet banner printing.                    *
      *  09/10/2024  MCA  Switch from VARHDINFDK to VARHDINFD.  VARHDINFDK was *
      *                   the "reverted" original source from August 2022 when *
      *                   our original Varisty phase 1 (SWOG, ShipOptimizer,   *
      *                   ShipSelect) go live had major issues.  Did a compare *
      *                   of the two sources, and new changes had been made    *
      *                   since the revert to VARHDINFD, but not to VARHDINFDK.*
      *                   VARHDINFD has been used since Varsity phase 1 2nd    *
      *                   go live in calls from HYR0110 and HYR0140 (for SWOG).*
      *                   So VARHDINFD is tested and solid.                    *
      *  09/13/2024  MCA  In CompCustShip, for multiple-turnaround parcel      *
      *                   customer shipments, call Varsity ShipSoft in SWOG    *
      *                   mode.  Available only if *DTAARA HYABXLSWOG='Y'.     *
      *  09/24/2024  MCA  In $VarsityCS (aka, SWOG Mode), also check MFHHMH    *
      *                   for order's last used shipment number.               *
      *  09/24/2024  MCA  When calling HYR0500, pass calling-program parm.     *
      *  10/23/2024  MCA  Fix bug.  Populate ship-via for calling ShipSoft.    *
      *  10/29/2024  MCA  Fix bug.  Update OEHDWK.H1SVSV before ShipSoft for   *
      *                   SWOG.                                                *
      *  10/30/2024  MCA  Fix bug.  OEUDF4 is populated with the ship time in  *
      *                   positions 12 thru 15.  Before this fix, this program *
      *                   was populating the time WITH a colon, so 9:35am      *
      *                   would be 09:3, and should have been 0935.            *
      *  12/05/2024  MCA  Invoice History files archive.                       *
      *  12/24/2024  MCA  In CompCustShip, if xWRNTY line is only line         *
      *                   not-yet-shipped, automatically ship it.              *
      *  03/31/2025  LWB  Change IF ws_webSrvcsOrd to                          *
      *                   IF ws_webSrvcsOrd OR wk_OECTRM = 'EC'                *
      *  04/09/2025  MCA  Convert ounce weights to pounds for shipment totals. *
      *  08/07/2025  MCA  After calling Varisty ShipSoft, and after retrieving *
      *                   the new tracking number from H1UDF3 (which is only   *
      *                   15 characters), try to retrieve the full tracking    *
      *                   number from the Varsity files.                       *
      *  02/27/2026  MCA  Don't print invoices for location 91 CC orders.      *
      *  03/18/2026  MCA  When building freight class array, if line was not   *
      *                   shipped, skip it.                                    *
      **************************************************************************

     Hnomain

     FHYLSGDT1  IF   E           K DISK    RENAME(HYRSGDT:HYRSGDT1) PREFIX(G1:2)
     F                                     USROPN
     FHYLSGDT3  IF   E           K DISK    RENAME(HYRSGDT:HYRSGDT3) USROPN
     FHYLPLLT4  IF   E           K DISK    USROPN
     FHYPOREL   IF   E           K DISK    USROPN
     FHYLOREL1  IF   E           K DISK    RENAME(HYROREL:HYROREL1) USROPN
     FHYPWSOQ   IF   E           K DISK    USROPN
     FHYPINTR   UF   E           K DISK    USROPN
     FOECTRL    UF   E             DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY QPRPSRC,HYR0614
      /COPY QPRPSRC,HYR9928
      /COPY QPRPSRC,WSOSHAG

     D InvcReq         PR             1N
     D  Order#                        8S 0 CONST
     D  CustTerms                     2A   CONST
     D  Foreign                       1N   CONST
     D  ShipAmt                      11P 2 CONST
     D  FrtAmtO                       7P 2 CONST
     D  Loc#                          3S 0 CONST

     D RtvShipAmt      PR            11P 2
     D  OrdCtl#                       8S 0 CONST
     D  Turn#                         9S 0 CONST
     D  Order#                        8S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  VolDiscPct                    1P 0 CONST
     D  FrtAmt                        7P 2 CONST
     D  SpclChgAmt                    7P 2 CONST
     D  TaxCode                       1A   CONST
     D  TaxOvr                        1A   CONST
     D  TaxState                      2A   CONST
     D  TaxCntyCode                   3S 0 CONST
     D  TaxCityCode                   5S 0 CONST
     D  TaxLoc1Code                   4S 0 CONST
     D  TaxLoc2Code                   4S 0 CONST
     D  TaxLoc3Code                   4S 0 CONST
     D  HDR1RL                        5P 1 CONST
     D  InvcReq                       1N   CONST

     D RtvIntrHoldCS   PR             1N
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST

     D RtvIntrHold     PR             1N
     D  Order#                        8S 0 CONST
     D  Turn#                         9S 0 CONST

     D CallVarsity     PR             1N
     D  ShipVia                       2A   CONST
     D  Intrhold                      1N   CONST

     D ShipType        PR             1A
     D  ShipVia                       2A   CONST

     D RtvNxtBOL#      PR             8S 0

     D RtvShipDay      PR             1N

     D RtvNxtShipDate  PR             7S 0

     D RtvCDVar        PR            34A
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  InDesc                       34A   CONST
     D  FrtCls                        5A   CONST

     D VarHDInfD       PR                  ExtPgm('VARHDINFD')                  Varsity interface
     D  Order#X                       8A   Const
     D  VarsityOrder#                10A   CONST
     D  OrderControl#                 8A   CONST
     D  Turn#X                        9A   CONST
     D  SSOrderType                   2A   CONST
     D  SSShipVia                     2A
     D  SSShipViaDesc                15A

     D VPRSWOGINF      PR                  EXTPGM('VPRSWOGINF')                 Varsity SWOG setup
     D  MstOrd#                      20A
     D  Ord#Ary                       8  0 DIM(600) CONST

      /COPY *LIBL/QPRPSRC,HYR0804C3

      /COPY *LIBL/QPRPSRC,HYR0810C3

      /COPY *LIBL/QPRPSRC,HYR0812

      /COPY *LIBL/QPRPSRC,HYR9906

     D HOETXI          PR                  EXTPROC('HOETXI')                    AvaTax interface
     D  OrdCtl#                       8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  Order#                        8P 0 CONST
     D  TaxCode                       1A   CONST
     D  FrtAmt                        9P 2 CONST
     D  SpclChgAmt                    9P 2 CONST
     D  ProcessType                   1A   CONST

     D HOETAX          PR                  EXTPROC('HOETAX')                    Calculate taxes
     D  OrdCtl#                       8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  Order#                        8P 0 CONST
     D  ShipAmt                      11P 2 CONST
     D  FrtAmt                        9P 2 CONST

     D HYR0114         PR                  EXTPROC('HYR0114')                   Packing List
     D  APID                          2A   CONST
     D  Order#                        8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  PrtBO                         1A   CONST
     D  PDFName                      60A   CONST
     D  PkgList#                      8P 0 CONST

     D COEPIN          PR                  EXTPGM('COEPIN')                     Invoice print
     D  WrkstnID                     10A   CONST
     D  UserID                       10A   CONST
     D  APID                          2A   CONST
     D  InvcBatch#                    4A   CONST
     D  Order#                        8A   CONST
     D  Turn#                         9A   CONST

      /COPY QPRPSRC,HYR0139A

     D HYR0500         PR                  EXTPGM('HYR0500')                    UCC-128 / Trk# assoc
     D  PgmMode                       1A   CONST                                1=By order, 2=SWOG
     D  Caller                       10A   CONST                                Calling program
     D  Order#                        8S 0 CONST
     D  Turn#                         9S 0 CONST
     D  Cust#                         7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  ShipVia                       2A   CONST
     D  Ord#Ary                       8P 0 DIM(600) CONST                       SWOG order array
     D  TurnAry                       9P 0 DIM(600) CONST                       SWOG turn array
     D  RtnCode                       2A

     D HYR0540         PR                  EXTPGM('HYR0540')
     D  Order#                        8S 0 CONST
     D  Turn#                         9S 0 CONST
     D  MultCtnSrl#                   9S 0 CONST
     D  SSCCProc                      5P 0

     D HYR0150         PR                  EXTPROC('HYR0150')                   HYPSCWK --> HYPSSCC
     D  Function                      1A   CONST
     D  Serial#                       9P 0 CONST
     D  Carton#                       5P 0 CONST
     D  Order#                        8P 0 CONST
     D  Line#                         3P 0 CONST
     D  Blanket#                      3P 0 CONST
     D  ShipQty                      13P 4 CONST
     D  Lot#                         15A   CONST
     D  EmpType                       1A   CONST
     D  Employee#                     7P 0 CONST
     D  Turn#                         9P 0 CONST

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

     D HYR0148         PR                  EXTPROC('HYR0148')                   Add 856 tickler rcd
     D  BOL#                          8P 0 CONST
     D  ShipTo#                       7P 0 CONST
     D  PriorBOL#                     8P 0 CONST
     D  ShutDown                      1N   CONST

     D HYR0142         PR                  EXTPROC('HYR0142')                   Print BOL
     D  BOL#                          8P 0 CONST
     D  PDFName                      60A   CONST

     D HYR0240         PR                  EXTPROC('HYR0240')                   Update Varsity files
     D  Order#                        8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  VarsityOrder#                10A   CONST
     D  ShutDown                      1N   CONST

     D HYR2022         PR                  EXTPROC('HYR2022')                   Non-truck EDI BOL
     D  Order#                        8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  ShipVia                       2A   CONST
     D  Pro#                         30A   CONST
     D  ShutDown                      1N   CONST
     D  RtnCode                       2A

     D HYR3504         PR                  EXTPGM('HYR3504')                    Powder Wgt Summ Rpt
     D  BOL#                          8P 0 CONST
     D  OUTQ                         10A   CONST

     D HYC3512         PR                  EXTPGM('HYC3512')                    Pallet report print
     D  Pallet#                       9P 0 CONST
     D  BOL#                          8P 0 CONST
     D  ShipTo#                       7P 0 CONST
     D  InclInvc                      4A   CONST
     D  ShowPO                        4A   CONST
     D  Item#Type                     5A   CONST
     D  CrtSplf                       4A   CONST
     D  CSVDocID                     40A   CONST
     D  OutQ                         10A   CONST
     D  Drawer                        3P 0 CONST
     D  Copies                        3P 0 CONST
     D  HoldSplf                      4A   CONST
     D  Submit                        4A   CONST

     D HYR3552         PR                  EXTPGM('HYR3552')                    Pallet banner print
     D  BOL#                          8S 0 CONST
     D  Caller                       10A   CONST
     D  RtnCode                       2A

     D HYR6080         PR                  ExtPgm('HYR6080')
     D  Order#                        8P 0 CONST
     D  Turn#                         9P 0 CONST
     D  EmailType                    10A   CONST
     D  SendTo                        1A   CONST
     D  Source                       10A   CONST
     D  ShutDown                      1N   CONST
     D  RtnCode                       2A

     D BSTSDLY         PR                  EXTPROC('BSTSDLY')                   Status msg w/ delay
     D  MessageID                     7A   CONST
     D  DelaySeconds                  3P 0 CONST

     D CSYRLO          PR                  EXTPROC('CSYRLO')                    Process record lock
     D  FileName                     10A   CONST
     D  LockCount                     1P 0

     D QSNDDTAQ        PR                  EXTPGM('QSNDDTAQ')                   Send data que entry
     D  DataQName                    10A   CONST
     D  DataQLib                     10A   CONST
     D  EntryLen                      5P 0 CONST
     D  Data                        256A   CONST

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

     D**** WSOACCPAY       PR                  EXTPROC('WSOACCPAY')
     D****  ws_ShpMthd                   10a   CONST
     D****  ws_Ord#                       8s 0 CONST
     D****  ws_OCtl                       8s 0 CONST
     D****  ws_Turn#                      9s 0 CONST
     D****  ws_ipPayA                    11p 2 CONST

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_DayCutoff4   S              4P 0 DTAARA(SHIPCUTOFF)                   Shipping day cutoff
     D wk_OrdStsEml    S              1A   DTAARA(HYABXLOSE)                    Send order sts email
     D wk_BxlSWOG      S              1A   DTAARA(HYABXLSWOG)                   Bxl/DP ShipSoft SWOG

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Pgm0139A     S              1N   INZ(*OFF)
     D wk_Pgm0148      S              1N   INZ(*OFF)
     D wk_Pgm0150      S              1N   INZ(*OFF)
     D wk_Pgm0152      S              1N   INZ(*OFF)
     D wk_Pgm0240      S              1N   INZ(*OFF)
     D wk_Pgm0540      S              1N   INZ(*OFF)
     D wk_Pgm0804      S              1N   INZ(*OFF)
     D wk_Pgm0810      S              1N   INZ(*OFF)
     D wk_Pgm0812      S              1N   INZ(*OFF)
     D wk_Pgm2022      S              1N   INZ(*OFF)
     D wk_Pgm6080      S              1N   INZ(*OFF)
     D wk_Pgm9906      S              1N   INZ(*OFF)
     D wk_Pgm9928      S              1N   INZ(*OFF)
     D kf_ShipGrp#     S              9S 0
     D kf_BillTo#      S              7S 0
     D kf_ShipTo#      S              7S 0
     D kf_DropShip#    S              7S 0
     D kf_Ord#         S              8S 0
     D kf_Turn#        S              9S 0
     D kf_WSID         S                   LIKE(WOWSID)
     D kf_OQTY         S                   LIKE(WOOQTY)
     D ws_TIvA         s             11P 2
     D ws_RtnMsg       s             75A
     D ws_RtnStsCd     s             10I 0
     D Zero_Turn       S              9S 0
     D Zero_Invoice    S              7P 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

      **************************************************************************
      * Define key lists.                                                      *
      **************************************************************************
     C     key_GD1       KLIST                                                  HYLSGDT1
     C                   KFLD                    kf_ShipGrp#
     C                   KFLD                    kf_Turn#

     C     key_GD3       KLIST                                                  HYLSGDT3
     C                   KFLD                    kf_ShipGrp#
     C                   KFLD                    kf_BillTo#
     C                   KFLD                    kf_ShipTo#
     C                   KFLD                    kf_DropShip#

     C     key_IO        KLIST                                                  HYPINTR
     C                   KFLD                    kf_Ord#
     C                   KFLD                    Zero_Turn
     C                   KFLD                    Zero_Invoice

     C     key_WO        KLIST                                                  HYPWSOQ
     C                   KFLD                    kf_WSID
     C                   KFLD                    kf_OQTY

     P/EJECT
      **************************************************************************
      * Complete the shipping process for a customer shipment.                 *
      **************************************************************************
     P CompCustShip    B                   EXPORT
     D                 PI            10I 0
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D GenBOL                         1N   CONST
     D PlltCnt                        5P 0 CONST
     D SaveOnly                       1N   CONST

     D wk_ShipGrp#     S              9S 0
     D wk_BillTo#      S              7S 0
     D wk_ShipTo#      S              7S 0
     D wk_DropShip#    S              7S 0
     D wk_GenBOL       S              1N
     D wk_PlltCnt      S              5P 0
     D wk_SaveOnly     S              1N
     D wk_Error        S             10I 0
     D wk_ShipDay      S              1N
     D wk_NxtShipDate  S              7S 0
     D wk_TurnShipped  S              1N
     D wk_SSCCProc     S              5P 0
     D wk_BIPLLT       S              5P 0
     D wk_HDR1RL       S              5P 1
     D wk_CMCCLS       S              2A
     D wk_CMALPH       S              4A
     D wk_CMCNA1       S             26A
     D wk_CMCNA2       S             26A
     D wk_CMCNA3       S             26A
     D wk_CMCNA4       S             19A
     D wk_CMCCTY       S             26A
     D wk_CMST         S              2A
     D wk_CMZIP        S             13A
     D wk_CMCTRY       S              3A
     D wk_CMPLDR       S              1A
     D wk_DSNAME       S             30A
     D wk_DSADR1       S             30A
     D wk_DSADR2       S             30A
     D wk_DSADR3       S             30A
     D wk_DSCITY       S             26A
     D wk_DSST         S              2A
     D wk_DSZIP        S             13A
     D wk_DSCTRY       S              3A
     D wk_STName       S             30A
     D wk_STAdr1       S             30A
     D wk_STAdr2       S             30A
     D wk_STAdr3       S             30A
     D wk_STCity       S             26A
     D wk_STST         S              2A
     D wk_STZip        S             13A
     D wk_STCtry       S              3A
     D wk_GHDCLOC      S              4A
     D wk_GCSHPV       S              2A
     D wk_GCSVDS       S             15A
     D wk_GCPRO#       S             30A
     D wk_GCFRTA       S              7P 2
     D wk_GCCHGA       S              7P 2
     D wk_Track#       S             30A
     D wk_Foreign      S              1N
     D wk_BOLReq       S              1N
     D wk_EDICust      S              1N
     D wk_CrtBOL       S              1N
     D wk_OECTRM       S              2A
     D wk_OECTXC       S              1A
     D wk_OETAXO       S              1A
     D wk_OESTTX       S              2A
     D wk_OECNTX       S              3P 0
     D wk_OECTTX       S              5P 0
     D wk_OELOC1       S              4S 0
     D wk_OELOC2       S              4S 0
     D wk_OELOC3       S              4S 0
     D wk_OEOSUS       S             10a
     D wk_OEBOAL       S              1A
     D wk_OEUDF5       S             15A
     D wk_OEUDF6       S             30A
     D wk_OELOC#       S              3S 0
     D wk_RHUKEY       S             15A
     D wk_RefMFUKEY    S             15A
     D wk_VolDiscPct   S              1P 0
     D wk_TurnCnt      S              3P 0
     D wk_SWOG         S              1N
     D wk_IntrHoldCS   S              1N
     D wk_IntrHold     S              1N
     D wk_FrtAmtO      S              7P 2
     D wk_SpclChgAmtO  S              7P 2
     D wk_ShipAmt      S             11P 2
     D wk_InvcReq      S              1N
     D wk_TaxAmt       S              9P 2
     D wk_TaxAmt2      S              9P 2
     D wk_OETIVA       S             11P 2
     D wk_PrtBO        S              1A
     D wk_PkgList#     S              8P 0
     D wk_BOL#         S              8S 0
     D wk_PLPKL#       S              8S 0
     D wk_CCAmt        S             11P 2
     D wk_CCFatal      S              1A
     D wk_CCResponse   S              2A
     D wk_CCNewKey     S             15A
     D wk_CCMsgText    S             35A
     D wk_TotCCAmt     S             11P 2
     D wk_Settle$      S             11P 2
     D wk_StlAmt       S             11P 2
     D wk_PayTypeCode  S              4A
     D wk_PayAmt       S             15P 5
     D wk_LocalVoid    S              1A
     D wk_PaySeq#      S              2S 0
     D wk_PrimOrd#     S              8S 0
     D wk_BillTo#X     S              7A
     D wk_Order#X      S              8A
     D wk_PriorTrk#    S             15A
     D wk_Varsity      S              1N   INZ(*OFF)
     D wk_VarsityOrd#  S             10A
     D wk_VarsityOrd2  S             20A
     D wk_LastVarSeq#  S              3P 0
     D wk_LastVarSq#1  S              3P 0
     D wk_LastVarSq#2  S              3P 0
     D wk_VarSeq#1     S              3P 0
     D wk_VarSeq#2     S              2P 0
     D wk_VarShipCnt   S              3P 0
     D wk_VarShipCnt1  S              3P 0
     D wk_VarShipCnt2  S              3P 0
     D wk_SSShipVia    S              2A
     D wk_SSShpViaDes  S             15A
     D wk_ShipType     S              1A
     D wk_ShipVia      S              2A
     D wk_MHSHIN       S             15A
     D wk_BuyGrpCust   S              1N
     D wk_H1OCTL       S              8S 0
     D wk_H1SVSV       S              2A
     D wk_H1UDF3       S             15A
     D wk_H1UDF4       S             15A
     D wk_TWSTTA       S              9P 2
     D wk_TWCNTA       S              9P 2
     D wk_TWCTTA       S              9P 2
     D wk_TWL1TA       S              9P 2
     D wk_TWL2TA       S              9P 2
     D wk_TWL3TA       S              9P 2
     D wk_SVSCAC       S              4A
     D wk_ASNGen       S              1A
     D wk_TotPkgs      S              5P 0
     D wk_TotWgt       S              5P 0
     D wk_EstDlvyDt    S              7S 0
     D wk_FTLD01       S             30A
     D wk_FTLD02       S             30A
     D wk_FTLD03       S             30A
     D wk_FTHAZM       S              1A
     D wk_FTRPQT       S             13P 4
     D wk_FTNMFC       S              5A
     D wk_BLUDA1       S             30A
     D wk_BLUDA2       S             30A
     D wk_FrtCls       S              5A
     D wk_PkgCnt       S              5P 0
     D wk_ShipWgt      S              6P 0
     D wk_ClsPlltCnt   S              5P 0
     D wk_HazMtl       S              2A
     D wk_BCM8         S             32A
     D wk_AmmoCtnCnt   S              7P 0
     D wk_IMITEM       S             15A
     D wk_IMPCLS       S              4A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN4       S             13P 5
     D wk_IMUOMW       S              2A
     D wk_Bulk         S              1N
     D wk_O1QSTC       S             13P 4
     D wk_O1WH         S              3S 0
     D wk_IWITEM       S             15A
     D wk_IWWHS        S              3S 0
     D wk_IWFTCL       S              5A
     D wk_LineWgt      S              9P 2
     D wk_FCCQWgtx     S              9P 2 DIM(10)
     D wk_FTCL         S              5A
     D wk_MDFTRK       S             25A
     D wk_MDFBRC       S             50A
     D wk_VarsityTrk#  S             30A
     D wk_CtnCnt       S              5P 0
     D wk_CtnCntP      S              9P 2
     D wk_TDKEY1       S             15A
     D wk_TDKEY2       S             15A
     D wk_TDCF01       S             30A
     D wk_TDCF02       S             30A
     D wk_TDCF03       S             30A
     D wk_TDFF01       S              1A
     D wk_TDFF02       S              1A
     D wk_CurCYMD      S              7S 0
     D wk_CurHMS       S              6S 0
     D wk_SysTime      S              6P 0
     D wk_DayCutoff    S              6P 0
     D wk_DelayTime    S              3P 0
     D wk_ShowPO       S              4A
     D wk_Item#Type    S              5A
     D wk_OceanBOL     S              1N
     D wk_EmergInfo    S              1A
     D wk_EmergPhone   S             11S 0 INZ(8004249300)
     D wk_DropShip#X   S              7A
     D wk_GDORD#       S              8S 0
     D wk_GDTURN       S              9S 0
     D wk_GDOCTL       S              8S 0
     D wk_SSMstOrd#    S              8S 0
     D wk_SSMstTurn    S              9S 0
     D wk_SSMstOCtl    S              8S 0
     D wk_StartTurn    S              9S 0
     D ary_Ord#        S              8  0 DIM(600)
     D ary_Turn        S              9  0 DIM(600)
     D wk_Abort        S              1N
     D wk_LogData      S             64A
     D wk_LogMSG       S             64A
     D wk_OutQType     S                   LIKE(WOOQTY)
     D wk_OutQ         S                   LIKE(WOOUTQ)
     D wk_OutQ1        S                   LIKE(WOOUTQ)
     D wk_Drawer       S              3P 0
     D wk_Wkstn        S                   LIKE(ps_Job)
     D wk_Len          S              3P 0
     D wk_RcdCnt       S              7P 0
     D wk_RC0500       S              2A
     D wk_RC2022       S              2A
     D wk_RC3552       S              2A
     D wk_RC6080       S              2A
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D ws_webSrvcsOrd  s              1n
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D sv_TURN         S              9S 0
     D idx1            S              3P 0
     D idx2            S              3P 0
     D idx3            S              3P 0
     D idx4            S              3P 0

     D AryOrd          S              8  0 DIM(600)
     D AryTurn         S              9  0 DIM(600)

     D OEDTAQ        E DS                  EXTNAME(OEDTAQPL)

     D                 DS
     D wk_FCCQ                 1    140
     D FCCQx                               DIM(10) OVERLAY(wk_FCCQ)             Frt cls case qty
     D  FCCQ_FTCL                     5A   OVERLAY(FCCQx:1)
     D  FCCQ_CSQT                     5P 0 OVERLAY(FCCQx:6)
     D  FCCQ_WGT                      7P 0 OVERLAY(FCCQx:9)
     D  FCCQ_PLLT                     3P 0 OVERLAY(FCCQx:13)

     D                 DS
     D wk_FCCQ2                1    140
     D FCCQx2                              DIM(10) OVERLAY(wk_FCCQ2)            Frt cls case qty
     D  FCCQ_FTCL2                    5A   OVERLAY(FCCQx2:1)
     D  FCCQ_CSQT2                    5P 0 OVERLAY(FCCQx2:6)
     D  FCCQ_WGT2                     7P 0 OVERLAY(FCCQx2:9)
     D  FCCQ_PLLT2                    3P 0 OVERLAY(FCCQx2:13)

     D BldSSCC         C                   '2'
     D BldELD          C                   '2'
     D HazYes          C                   ' X'
     D HazRptQty       C                   'RQ'

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_GenBOL = GenBOL
     C                   EVAL      wk_PlltCnt = PlltCnt
     C                   EVAL      wk_SaveOnly = SaveOnly
     C                   EVAL      wk_Error = *Zero

     C                   IF        not %open(HYLSGDT1)
     C                   OPEN      HYLSGDT1
     C                   ENDIF                                                  not %open(HYLSGDT1)

     C                   IF        not %open(HYLSGDT3)
     C                   OPEN      HYLSGDT3
     C                   ENDIF                                                  not %open(HYLSGDT3)

     C                   IF        wk_DatabaseID = *Blanks
     C                   IN        wk_DatabaseID
     C                   ENDIF                                                  wk_DatabaseID=*Blank

     C                   IF        wk_OrdStsEml = *Blank
     C                   IN        wk_OrdStsEml
     C                   ENDIF                                                  wk_OrdStsEml=*Blank

     C                   IF        wk_BxlSWOG = *Blank
     C                   IN        wk_BxlSWOG
     C                   ENDIF                                                  wk_BxlSWOG=*Blank

     C                   IF        wk_DayCutoff4 = *Zero
     C                   IN        wk_DayCutoff4
     C                   ENDIF                                                  wk_DayCutoff4=*Zero
     C                   EVAL      wk_DayCutoff = (wk_DayCutoff4 * 100) - 1

     C                   EVAL      wk_ShipDay = RtvShipDay ()

     C                   EVAL      wk_NxtShipDate = RtvNxtShipDate ()

      /Free
       wk_HDR1RL = *Zero;
       EXEC SQL select HDR1RL into :wk_HDR1RL
                from HDCTRL;

       wk_CMCCLS = *Blanks;
       EXEC SQL select CMCCLS into :wk_CMCCLS
                from HDCUST
                where CMCUST=:wk_BillTo#;

       wk_CMALPH = *Blanks;
       wk_CMCNA1 = *Blanks;
       wk_CMCNA2 = *Blanks;
       wk_CMCNA3 = *Blanks;
       wk_CMCNA4 = *Blanks;
       wk_CMCCTY = *Blanks;
       wk_CMST = *Blanks;
       wk_CMZIP = *Blanks;
       wk_CMCTRY = *Blanks;
       EXEC SQL select CMALPH, CMCNA1, CMCNA2, CMCNA3,
                       CMCNA4, CMCCTY, CMST, CMZIP,
                       CMCTRY
                 into :wk_CMALPH, :wk_CMCNA1, :wk_CMCNA2, :wk_CMCNA3,
                      :wk_CMCNA4, :wk_CMCCTY, :wk_CMST, :wk_CMZIP,
                      :wk_CMCTRY
                from HDCUST
                where CMCUST=:wk_ShipTo#;

       IF wk_DropShip# = *Zero;
         wk_STName = wk_CMCNA1;
         wk_STAdr1 = wk_CMCNA2;
         wk_STAdr2 = wk_CMCNA3;
         wk_STAdr3 = wk_CMCNA4;
         wk_STCity = wk_CMCCTY;
         wk_STST = wk_CMST;
         wk_STZip = wk_CMZIP;
         wk_STCtry = wk_CMCTRY;
       ELSE;
         wk_DSNAME = *Blanks;
         wk_DSADR1 = *Blanks;
         wk_DSADR2 = *Blanks;
         wk_DSADR3 = *Blanks;
         wk_DSCITY = *Blanks;
         wk_DSST = *Blanks;
         wk_DSZIP = *Blanks;
         wk_DSCTRY = *Blanks;
         EXEC SQL select DSNAME, DSADR1, DSADR2, DSADR3,
                         DSCITY, DSST, DSZIP, DSCTRY
                   into :wk_DSNAME, :wk_DSADR1, :wk_DSADR2, :wk_DSADR3,
                         :wk_DSCITY, :wk_DSST, :wk_DSZIP, :wk_DSCTRY
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:wk_ShipTo# and
                   DSNMBR=:wk_DropShip#;
         wk_STName = wk_DSNAME;
         wk_STAdr1 = wk_DSADR1;
         wk_STAdr2 = wk_DSADR2;
         wk_STAdr3 = wk_DSADR3;
         wk_STCity = wk_DSCITY;
         wk_STST = wk_DSST;
         wk_STZip = wk_DSZIP;
         wk_STCtry = wk_DSCTRY;
       ENDIF;

       wk_GCSHPV = *Blanks;
       wk_GCSVDS = *Blanks;
       wk_GCPRO# = *Blanks;
       wk_GCFRTA = *Zero;
       wk_GCCHGA = *Zero;
       EXEC SQL select GCSHPV, GCSVDS, GCPRO#, GCFRTA,
                       GCCHGA
                 into :wk_GCSHPV, :wk_GCSVDS, :wk_GCPRO#, :wk_GCFRTA,
                      :wk_GCCHGA
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;
      /End-Free

     C                   EVAL      wk_Track# = wk_GCPRO#

     C                   EVAL      wk_Foreign = RtvForeign (wk_CMCCLS:wk_STCtry)

     C                   EVAL      wk_BOLReq = BOLReq (wk_GCSHPV:wk_Foreign)

     C                   EVAL      wk_EDICUST = EDICust (wk_ShipTo#)

     C                   EVAL      wk_CrtBOL = wk_GenBOL

     C                   EVAL      wk_CurCYMD = %dec(%date:*CYMD)
     C                   EVAL      wk_CurHMS = %dec(%time:*HMS)

      /Free
       IF wk_BxlSWOG = 'Y';
         wk_TurnCnt = *Zero;
         EXEC SQL select count(distinct gdturn) into :wk_TurnCnt
                  from HYPSGDT
                  where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                        GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                        GDQSTC>0;
         IF wk_TurnCnt > 1;
           wk_SWOG = *ON;
         ELSE;
           wk_SWOG = *OFF;
         ENDIF;                                                              // wk_TurnCnt>1
       ELSE;
         wk_SWOG = *OFF;
       ENDIF;                                                                // wk_BxlSWOG='Y'
      /End-Free

      *
      * If there are multiple shipped turns in the customer shipment, call
      * Varsity ShipSoft up front to handle SWOG.
      *
     C                   IF        wk_SWOG
     C                   IF        not wk_SaveOnly
     C                   EVAL      wk_IntrHoldCS = RtvIntrHoldCS (wk_ShipGrp#:
     C                               wk_BillTo#:wk_ShipTo#: wk_DropShip#)
     C                   IF        CallVarsity (wk_GCSHPV:wk_IntrHoldCS)
     C                   EVAL      wk_ShipType = ShipType (wk_GCSHPV)
     C                   EXSR      $VarsityCS
     C                   IF        wk_Error < *Zero
     C                   EVAL      wk_Abort = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_Error<*Zero
     C                   ENDIF                                                  CallVarsity
     C                   ENDIF                                                  not wk_SaveOnly
     C                   ENDIF                                                  wk_SWOG

     C                   EVAL      wk_BIPLLT = wk_PlltCnt
     C                   EVAL      FCCQ_FTCL2 = *Blanks
     C                   EVAL      FCCQ_CSQT2 = *Zero
     C                   EVAL      FCCQ_WGT2 = *Zero
     C                   EVAL      FCCQ_PLLT2 = *Zero
     C                   EVAL      idx3 = *Zero
     C                   EVAL      idx4 = *Zero

     C                   EVAL      wk_Abort = *OFF

     C                   EVAL      kf_ShipGrp# = wk_ShipGrp#
     C                   EVAL      kf_BillTo# = wk_BillTo#
     C                   EVAL      kf_ShipTo# = wk_ShipTo#
     C                   EVAL      kf_DropShip# = wk_DropShip#

     C                   EVAL      sv_TURN = -1
     C     key_GD3       SETLL     HYLSGDT3
     C                   DOU       %eof(HYLSGDT3)
     C     key_GD3       READE     HYLSGDT3
     C                   IF        %eof(HYLSGDT3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT3)
     C                   IF        GDTURN = sv_TURN
     C                   ITER
     C                   ENDIF                                                  GDTURN=sv_TURN
     C                   EVAL      sv_TURN = GDTURN

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:GDTURN and
                  GDQSTC<>0;
       IF wk_RcdCnt > *Zero;
         wk_TurnShipped = *ON;
       ELSE;
         wk_TurnShipped = *OFF;
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

      *
      * Build FCCQ data sructure.
      *
     C                   EXSR      $BldFCCQ

      /Free
       wk_OECTRM = *Blanks;
       wk_OECTXC = *Blank;
       wk_OETAXO = *Blank;
       wk_OESTTX = *Blanks;
       wk_OECNTX = *Zero;
       wk_OECTTX = *Zero;
       wk_OELOC1 = *Zero;
       wk_OELOC2 = *Zero;
       wk_OELOC3 = *Zero;
       wk_OEUDF5 = *Blanks;
       wk_OEUDF6 = *Blanks;
       wk_OELOC# = *Zero;
       wk_OEOSUS = *Blanks;
       wk_OEBOAL = *Blank;
       EXEC SQL select OECTRM, OECTXC, OETAXO, OESTTX,
                       OECNTX, OECTTX, OELOC1, OELOC2,
                       OELOC3, OEUDF5, OEUDF6, OELOC#,
                       OEOSUS, OEBOAL
                 into :wk_OECTRM, :wk_OECTXC, :wk_OETAXO, :wk_OESTTX,
                      :wk_OECNTX, :wk_OECTTX, :wk_OELOC1, :wk_OELOC2,
                      :wk_OELOC3, :wk_OEUDF5, :wk_OEUDF6, :wk_OELOC#,
                      :wk_OEOSUS, :wk_OEBOAL
                from OEORHD
                where OEORD#=:GDORD#;
      /End-Free

     C                   IF        %subst(wk_OEUDF5:1:1) >= '0'  and
     C                               %subst(wk_OEUDF5:1:1) <= '9'  and
     C                               %subst(wk_OEUDF5:2:1) = '%'
     C                   EVAL      wk_VolDiscPct =
     C                               %dec(%subst(wk_OEUDF5:1:1):1:0)
     C                   ELSE
     C                   EVAL      wk_VolDiscPct = *Zero
     C                   ENDIF                                                  %subst(wk_OEUDF5:1:1

      *
      * Release intransit hold for fully-shipped retail and employee orders
      * on intransit hold only because order was marked "no backorders".
      * Release intransit hold for fully-shipped non-foreign drop-shipment
      * orders, regardless of customer type (i.e., location no.).
      *
     C                   EXSR      $ShipCompRls


     C                   EVAL      wk_IntrHold = RtvIntrHold (GDORD#:GDTURN)

      *
      * If any turnarounds associated with the shipment are on intransit hold,
      * don't create a BOL.
      *
     C                   IF        wk_IntrHold  or
     C                                wk_SaveOnly
     C                   EVAL      wk_CrtBOL = *OFF
     C                   ENDIF                                                  wk_IntrHold

      *
      * If creating a BOL, ensure all cartons for the current turnaround
      * have been palletized.
      *
      /Free
       IF wk_BOLReq and wk_CrtBOL;
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSCWK
                  where S1ORD#=:GDORD# and S1TURN=:GDTURN and
                        S1PSRL#=0;
         IF wk_RcdCnt > *Zero;
           wk_CrtBOL = *OFF;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDIF;                                                                // wk_BOLReq and
      /End-Free

      *
      * Use any freight and/or special charges on the first order for the
      * customer.
      *
     C                   EVAL      wk_FrtAmtO = wk_GCFRTA
     C                   EVAL      wk_SpclChgAmtO = wk_GCCHGA
     C                   EVAL      wk_GCFRTA = *Zero
     C                   EVAL      wk_GCCHGA = *Zero

     C                   EVAL      wk_ShipAmt = RtvShipAmt (GDOCTL:GDTURN:
     C                               GDORD#:GDSHTO:GDDSHP:wk_VolDiscPct:
     C                               wk_FrtAmtO:wk_SpclChgAmtO:wk_OECTXC:
     C                               wk_OETAXO:wk_OESTTX:wk_OECNTX:wk_OECTTX:
     C                               wk_OELOC1:wk_OELOC2:wk_OELOC3:wk_HDR1RL:
     C                               *ON)

     C                   EVAL      wk_InvcReq = InvcReq (GDORD#:wk_OECTRM:
     C                               wk_Foreign:wk_ShipAmt:wk_FrtAmtO:wk_OELOC#)

     C                   IF        wk_InvcReq  and
     C                               not wk_SaveOnly

      *
      * Obtain any necessary credit card authorizations.
      *
     C                   IF        wk_OECTRM = 'CC'  or  wk_OECTRM = 'CP'
     C                   EXSR      $CCProc
     C                   IF        wk_Error < *Zero
     C                   EVAL      wk_Abort = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_Error<*Zero
     C                   ELSE
     C                   EVAL      wk_CCNewKey = *Blanks                        Clear unique key
     C                   ENDIF                                                  wk_OECTRM='CC' or

      /Free
       wk_TWSTTA = *Zero;
       wk_TWCNTA = *Zero;
       wk_TWCTTA = *Zero;
       wk_TWL1TA = *Zero;
       wk_TWL2TA = *Zero;
       wk_TWL3TA = *Zero;
       EXEC SQL select coalesce(sum(TWSTTA),0), coalesce(sum(TWCNTA),0),
                       coalesce(sum(TWCTTA),0), coalesce(sum(TWL1TA),0),
                       coalesce(sum(TWL2TA),0), coalesce(sum(TWL3TA),0)
                 into :wk_TWSTTA, :wk_TWCNTA,
                      :wk_TWCTTA, :wk_TWL1TA,
                      :wk_TWL2TA, :wk_TWL3TA
                from OETXWK
                where TWOCTL=:GDOCTL;

       EXEC SQL update OEHDWK
                set H1OTOT=:wk_ShipAmt, H1STTA=:wk_TWSTTA,
                    H1CNTA=:wk_TWCNTA, H1CTTA=:wk_TWCTTA,
                    H1LC1A=:wk_TWL1TA, H1LC2A=:wk_TWL2TA,
                    H1LC3A=:wk_TWL3TA, H1UDF6=:wk_CCNewKey,
                    H1FRT=:wk_FrtAmtO, H1CHRG=:wk_SpclChgAmtO,
                    H1SVSV=:wk_GCSHPV, H1SVDS=:wk_GCSVDS
                where H1OCTL=:GDOCTL;
      /End-Free

     C                   ELSE

      /Free
       EXEC SQL update OEHDWK
                set H1OTOT=:wk_ShipAmt, H1FRT=:wk_FrtAmtO,
                    H1CHRG=:wk_SpclChgAmtO, H1SVSV=:wk_GCSHPV,
                    H1SVDS=:wk_GCSVDS, H1UDF3=:wk_GCPRO#
                where H1OCTL=:GDOCTL;
      /End-Free

     C                   ENDIF                                                  wk_InvcReq

      *
      * Process parcel shipments through Varsity.
      *
     C                   IF        not wk_SaveOnly  and  wk_TurnShipped
     C                               and  not wk_SWOG
     C                   IF        CallVarsity (wk_GCSHPV:wk_IntrHold)
     C                   EVAL      wk_ShipType = ShipType (wk_GCSHPV)
     C                   EXSR      $Varsity
     C                   IF        wk_Error < *Zero
     C****               IF        wk_Error = -1005
     C                   EVAL      wk_Abort = *ON
     C****               ENDIF                                                  wk_Error=-1005
     C                   LEAVE
     C                   ENDIF                                                  wk_Error<*Zero
     C                   ENDIF                                                  CallVarsity
     C                   ENDIF                                                  not wk_SaveOnly and

      *
      * If a BOL will be created, and if a PRO number has not yet been
      * assigned, retrieve and assign the next available PRO number.
      *
     C                   IF        wk_BOLReq  and  wk_CrtBOL  and
     C                               not wk_SaveOnly  and  wk_Track# = *Blanks
      /Free
       wk_GHDCLOC = *Blanks;
       EXEC SQL select GHDCLOC into :wk_GHDCLOC
                from HYPSGHD
                where GHGRP#=:wk_ShipGrp#;
       wk_Track# = RtvNxtPRO (wk_GHDCLOC:wk_GCSHPV);
       wk_Pgm9928 = *ON;
       IF wk_Track# <> *Blanks;
         EXEC SQL update HYPSGCU set GCPRO#=:wk_Track#
                  where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                        GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;
         wk_H1UDF3 = wk_Track#;
         EXEC SQL update OEHDWK a set H1UDF3=:wk_H1UDF3
                  where exists (select * from HYPSGDT b where
                        b.GDGRP#=:wk_ShipGrp# and b.GDBLTO=:wk_BillTo# and
                        b.GDSHTO=:wk_ShipTo# and b.GDDSHP=:wk_DropShip# and
                        b.GDOCTL=a.H1OCTL);
         IF wk_DropShip# = *Zero;
           wk_DropShip#X = '*N';
         ELSE;
           wk_DropShip#X = %triml(%editc(wk_DropShip#:'Z'));
         ENDIF;                                                              // wk_DropShip#=*Zero
         wk_MsgID = 'MSG0468';
         wk_MsgData = %triml(%editc(wk_ShipGrp#:'Z'));
         %subst(wk_MsgData:10:7) = %triml(%editc(wk_BillTo#:'Z'));
         %subst(wk_MsgData:17:7) = %triml(%editc(wk_ShipTo#:'Z'));
         %subst(wk_MsgData:24:7) = wk_DropShip#X;
         %subst(wk_MsgData:31:4) = wk_GHDCLOC;
         %subst(wk_MsgData:35:30) = wk_Track#;
         EXSR $WrtJobLog2;
         wk_LogData = 'SHIPGRP#(' + %triml(%editc(wk_ShipGrp#:'Z')) +
                      ') BILLTO#(' + %triml(%editc(wk_BillTo#:'Z')) +
                      ') SHIPTO#(' + %triml(%editc(wk_ShipTo#:'Z')) +
                      ') DROPSHIP#(' + %trimr(wk_DropShip#X) + ')';
         wk_LogMsg = 'PRO no. ' + %trimr(wk_Track#) +
                     ' assigned automatically.  DCLOC(' +
                     %trimr(wk_GHDCLOC) +')';
         EXEC SQL Insert into hypmilg
                         (mllgty, mldata, mlmsg, mlpgm,
                          mluser, mlws, mltstp)
                  values('SG01', :wk_LogData, :wk_LogMsg, :ps_Pgnm,
                         :ps_User, :ps_Job, current timestamp);
       ENDIF;                                                                // wk_Track#<>*Blanks
      /End-Free
     C                   ENDIF                                                  wk_BOLReq and

      /Free
       // Agilx credit card funds capture for shipment.
       IF NOT wk_SaveOnly and wk_InvcReq and wk_TurnShipped;
         ws_webSrvcsOrd = *OFF; // Web services order.
         EXEC SQL
           SELECT ws.webSrvcsOrd
           INTO :ws_webSrvcsOrd
           FROM((SELECT '1' AS webSrvcsOrd
                 FROM WSORDHDR
                 WHERE ohOrd# = :gdOrd#
                 AND ohOrdSts = 'GOOD')
                UNION
                (SELECT '1' AS webSrvcsOrd
                 FROM WSWAROHDR
                 WHERE whOrdNo = :gdOrd#)
           FETCH FIRST 1 ROW ONLY) AS ws;
         IF ws_webSrvcsOrd OR wk_OECTRM = 'EC';
           ws_rtnStsCd = 0; // 202
           ws_RtnMsg = *BLANKS;
           MONITOR;
             ws_TIvA = wk_ShipAmt + wk_FrtAmtO + wk_SpclChgAmtO;
             ws_rtnStsCd = WsOShAg('HYR0614'
                                 : gdTurn
                                 : gdOCtl
                                 : gdOrd#
                                 : ws_TivA
                                 : ws_rtnMsg);
             IF ws_rtnStsCd <> H_ACCEPTED; // 202
               wk_Abort = *ON;
               wk_Error = -406;
               wk_MsgID = 'ERR9999'; // Credit card authorization failed.
               wk_MsgData = %TRIM(ws_rtnMsg) + ' The amount of the denied +
                            authorization was ' + %TRIM(%EDITC(wk_ShipAmt:'J'));
               EXSR $WrtJobLog1;
               LEAVE;
             ENDIF; // ws_rtnStsCd <> H_ACCEPTED
           ON-ERROR;
             wk_Abort = *ON;
             wk_Error = -500;
             wk_MsgID = 'ERR9999'; // Error detected in call to program
             wk_MsgData = 'Error on call to Agilx credit card approval. +
                           The shipment was NOT completed.';
             EXSR $WrtJobLog1;
             LEAVE;
           ENDMON;
         ENDIF; // ws_webSrvcsOrd OR wk_OECTRM = 'EC'
       ENDIF; // NOT wk_SaveOnly and wk_InvcReq and wk_TurnShipped

       IF wk_TurnShipped;
         //
         // If xWRNTY line is only line not-yet-shipped, automatically ship it.
         //
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from OEDTWK
                  where O1OCTL=:GDOCTL and substr(O1ITEM,2,6)='WRNTY ' and
                        O1ORST<>'C' and O1QORD>0 and O1QORD>O1QSTD+O1QSTC;
         IF wk_RcdCnt > *Zero;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from OEDTWK
                    where O1OCTL=:GDOCTL and substr(O1ITEM,2,6)<>'WRNTY ' and
                          O1ORST<>'C' and
                          ((O1QORD>0 and O1QORD>O1QSTD+O1QSTC) or
                           (O1QORD<0 and O1QORD<O1QSTD+O1QSTC));
           IF wk_RcdCnt = *Zero;
             EXEC SQL update OEDTWK
                       set O1MNCD='C', O1QSTC=O1QORD-O1QSTD, O1QOBO=0
                      where O1OCTL=:GDOCTL and substr(O1ITEM,2,6)='WRNTY ' and
                            O1ORST<>'C' and O1QORD>0 and O1QORD>O1QSTD+O1QSTC;
           ENDIF;                                                            // wk_RcdCnt=*Zero
         ENDIF;                                                              // wk_RcdCnt>*Zero

         // For any lines not shipped, where backorder is not allowed,
         // set O1MNCD='C' so that HOEUOE will create a record in
         // OEORDP (if it doesn't already exist), so that HOEUPI will
         // close the line.
         EXEC SQL update OEDTWK set O1MNCD='C', O1QOBO=0
                  where O1OCTL=:GDOCTL and O1MNCD=' ' and O1BOAL='N' and
                   O1QSTC=0 and O1ORST<>'C';

      // wk_H1UDF4 = ps_Job + ' ' + %subst(%char(%time:*HMS):1:4);
         wk_H1UDF4 = ps_Job + ' ' + %subst(%char(%time:*HMS0):1:4);
         EXEC SQL update OEHDWK
                  set H1UDF4=:wk_H1UDF4
                  where H1OCTL=:GDOCTL;
       ENDIF;                                                                // wk_TurnShipped
      /End-Free

      *
      * Update order entry files.
      *
     C                   CLEAR                   OEDTAQ
     C                   EVAL      OQTURN = GDTURN
     C                   EVAL      OQORD# = GDORD#
     C                   EVAL      OQOCTL = GDOCTL
     C                   EVAL      wk_SysTime = %dec(%time:*HMS)
     C                   IF        wk_SysTime > wk_DayCutoff
     C                               or  not wk_ShipDay
     C                   EVAL      OQTSTP =
     C                               %timestamp((wk_NxtShipDate + 19000000)
     C                               * 1000000000000)
     C                   ELSE
     C                   EVAL      OQTSTP = %timestamp
     C                   ENDIF                                                  wk_SysTime>wk_DayCut
     C                   EVAL      OQTSUS = ps_User
     C                   EVAL      OQTSWS = ps_Job
     C                   EVAL      OQPGM = 'HY0614'

     C                   CALLP(E)  QSNDDTAQ ('OEDTAQ':'*LIBL':%len(OEDTAQ):
     C                               OEDTAQ)

      *
      * Wait for the order processor to update the files before continuing.
      *
     C                   EVAL      wk_DelayTime = 1
     C                   DOU       wk_H1OCTL = *Zero
      /Free
       wk_H1OCTL = *Zero;
       EXEC SQL select H1OCTL into :wk_H1OCTL
                from OEHDWK
                where H1OCTL=:GDOCTL;
      /End-Free
     C                   IF        wk_H1OCTL = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_H1OCTL=*Zero
     C                   IF        wk_DelayTime > 5
     C                   EVAL      wk_MsgID = 'MSG5008'
     C                   ELSE
     C                   EVAL      wk_MsgID = 'MSG5005'
     C                   ENDIF                                                  wk_DelayTime>5
     C                   CALLP(E)  BSTSDLY (wk_MsgID:wk_DelayTime)
     C                   IF        wk_DelayTime < 999
     C                   EVAL      wk_DelayTime = wk_DelayTime + 2
     C                   ENDIF                                                  wk_DelayTime<999
     C                   ENDDO                                                  wk_H1OCTL=*Zero

      *
      * If Varsity was processed, update Varsity order number and tracking
      * number in HYPTDTA.
      *
     C                   IF        wk_Varsity
      /Free
       wk_MDFTRK = *Blanks;
       wk_MDFBRC = *Blanks;
       EXEC SQL select MDFTRK, MDFBRC
                 into :wk_MDFTRK, :wk_MDFBRC
                from MFD1MD
                where MDCMNO='01' and MDOTYP=' ' and
                      MDORDR=:wk_VarsityOrd# and MDBXNO=1;
       IF wk_MDFTRK = *Blanks;
         wk_VarsityTrk# = wk_MDFBRC;
       ELSE;
         wk_VarsityTrk# = wk_MDFTRK;
       ENDIF;
       IF wk_VarsityTrk# <> *Blanks;
         EXEC SQL Update HYPTDTA
                   set TDTRK#=:wk_VarsityTrk#, TDVORD=:wk_VarsityOrd#
                  Where TDTURN=:GDTURN;
       ELSE;
         EXEC SQL Update HYPTDTA
                   set TDVORD=:wk_VarsityOrd#
                  Where TDTURN=:GDTURN;
       ENDIF;                                                                // wk_VarsityTrk#<>*Bla
      /End-Free
     C                   ENDIF                                                  wk_Varsity

     C                   IF        not wk_SaveOnly  and  wk_TurnShipped

     C                   IF        wk_InvcReq

     C                   EVAL      wk_MsgID = 'MSG5004'
     C                   EVAL      wk_MsgData = ps_Pgnm
     C                   EXSR      $SndSM

      /Free
       //-----------------------------------------------------------------------
       // Authorize ecommerce credit card payment for shipment.
       //-----------------------------------------------------------------------
       IF wk_OECTRM = 'EC'
       AND ws_rtnStsCd = H_ACCEPTED  // 202
       AND (NOT wk_SaveOnly AND wk_InvcReq AND wk_TurnShipped);
         WSOACCPAY('Shipment'
                 : gdORD#
                 : gdOCtl
                 : gdTurn
                 : ws_TIVA);
       ENDIF; // OECTRM = 'EC'
       //
       // If the order is currently on intransit hold, release it.
       //
       // wk_CurCYMD = %dec(%date:*CYMD);
       // wk_CurHMS = %dec(%time:*HMS);
       EXEC SQL update HYPINTR
                set IORLSU=:ps_User, IORLSD=:wk_CurCYMD, IORLST=:wk_CurHMS
                where IOORD#=:GDORD# and IOTURN=:GDTURN and
                      IOAIV#=0 and IORLSD=0 and
                      (IOTURN=0 or IOTURN=:GDTURN);
      /End-Free

      /Free
        EXEC SQL Update HYPTDTA set TDCCKY=:wk_CCNewKey
                 Where TDTURN=:GDTURN;
      /End-Free

      *
      * Generate an invoice.
      *
     C                   CALLP(E)  COEPIN (ps_Job:ps_User:'OE':*Zeros:
     C                               %editc(GDORD#:'X'):%editc(GDTURN:'X'))

     C                   ELSE

     C                   IF        not wk_IntrHold

     C****               EVAL      wk_ShipAmt = RtvShipAmt (GDOCTL:GDTURN:
     C****                           GDORD#:GDSHTO:GDDSHP:wk_VolDiscPct:
     C****                           wk_FrtAmtO:wk_SpclChgAmtO:wk_OECTXC:
     C****                           wk_OETAXO:wk_OESTTX:wk_OECNTX:wk_OECTTX:
     C****                           wk_OELOC1:wk_OELOC2:wk_OELOC3:wk_HDR1RL:
     C****                           *OFF)

      /Free
       wk_OETIVA = *Zero;
       EXEC SQL select OETIVA into :wk_OETIVA
                from OEORHD
                where OEORD#=:GDORD#;
      /End-Free

     C                   EVAL      wk_MsgID = 'MSG5007'
     C                   EVAL      wk_MsgData = ps_Pgnm
     C                   EXSR      $SndSM

     C                   IF        wk_OETIVA = *Zero  or
     C                               wk_CMCCLS = 'WR'  or                       Writer
     C                               wk_CMCCLS = 'TV'  or                       TV sponsorship
     C                               wk_CMCCLS = 'SS'  or                       Sponsored shooter
     C                               wk_CMCCLS = 'IP'  or                       Industry prof
     C                               wk_CMCCLS = 'IV'  or                       Industry VIP
     C                               wk_CMCCLS = 'EM'  or                       Employee
     C                               wk_CMCCLS = 'RT'  or                       Retail
     C                               wk_CMCCLS = 'RF'  or                       Retail - foreign
     C                               wk_CMCCLS = 'RP'  or                       Rep
     C                               wk_CMCCLS = 'SF'                           Rep - foreign
     C                   EVAL      wk_PrtBO = 'Y'
     C                   ELSE
     C                   EVAL      wk_PrtBO = 'N'
     C                   ENDIF                                                  wk_OETIVA=*Zero

      *
      * Generate a packing list.
      *
     C                   CALLP(E)  HYR0114 ('OE':GDORD#:GDTURN:wk_PrtBO:
     C                                      '*NONE':wk_PkgList#)

     C                   ENDIF                                                  not wk_IntrHold

     C                   ENDIF                                                  wk_InvcReq

     C                   ENDIF                                                  not wk_SaveOnly and

     C                   EVAL      wk_MsgID = 'MSG5005'
     C                   EVAL      wk_MsgData = ps_Pgnm
     C                   EXSR      $SndSM

     C                   IF        wk_TurnShipped

      *
      * Build HYPSSCC records from HYPSCWK records.
      *
     C                   CALLP(E)  HYR0150 (BldSSCC:*Zero:*Zero:GDORD#:*Zero:
     C                              *Zero:*Zero:*Blanks:'B':1:GDTURN)
     C                   EVAL      wk_Pgm0150 = *ON

      *
      * Build HYPELD records from HYPEDWK records.
      *
     C                   CALLP(E)  HYR0152 (BldELD:GDORD#:*Zero:*Zero:
     C                              *Zero:*Zero:*Zero:*Blanks:*Zero)
     C                   EVAL      wk_Pgm0152 = *ON

      *
      * If customer is set up to print pallet shipment carton labels,
      * print them for any multiple-carton labels (as defined in HYPPCLC).
      *
      /Free
       wk_BillTo#X = %editc(GDBLTO:'X');
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='PLLTLBLCTN' and TDKEY1=:wk_BillTo#X;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   CALLP(E)  HYR0540 (GDORD#:GDTURN:*Zero:wk_SSCCProc)
     C                   EVAL      wk_Pgm0540 = *ON
     C                   ELSE
     C                   EVAL      wk_SSCCProc = *Zero
     C                   ENDIF                                                  wk_RcdCnt>*Zero

      *
      * Write HYPBOL record.
      *
      /Free
       wk_ShipWgt = %xfoot(FCCQ_WGT);
       EXEC SQL insert into HYPBOL
                  (BIORD#, BITURN, BIAIV#, BIALPH,
                   BISHTO, BIDSHP, BISVSV, BISVDS,
                   BIPRO#, BIFCCQ, BIPLLT, BIWGT,
                   BIBOL#, BIPRTD)
                 values (:GDORD#, :GDTURN, 0, :wk_CMALPH,
                         :wk_ShipTo#, :wk_DropShip#, :wk_GCSHPV, :wk_GCSVDS,
                         :wk_Track#, :wk_FCCQ, :wk_BIPLLT, :wk_ShipWgt,
                         0, 0);
       wk_BIPLLT = *Zero;
      /End-Free

      *
      * If Varsity ShipSoft was used, update files for inquiry.
      *
     C                   IF        wk_Varsity
      /Free
       // EXEC SQL Update HYPTDTA set TDVORD=:wk_VarsityOrd#
       //          Where TDTURN=:GDTURN;
       IF wk_InvcReq;
         EXEC SQL Update HYPIDTA set ISVORD=:wk_VarsityOrd#
                  Where ISTURN=:GDTURN;
       ENDIF;                                                                // wk_InvcReq
      /End-Free
     C                   CALLP(E)  HYR0240 (GDORD#:GDTURN:wk_VarsityOrd#:*OFF)
     C                   EVAL      wk_Pgm0240 = *ON
     C                   ENDIF                                                  wk_Varsity

     C                   IF        not wk_IntrHold  and
     C                               not wk_SaveOnly

      *
      * If a BOL is not required, but the customer is an EDI customer, and
      * the shipment is not on intransit hold, create BOL records for ASN's.
      *
     C                   IF        not wk_BOLReq  and  wk_EDICust  and
     C                               not wk_IntrHold
     C                   EVAL      wk_MsgID = 'MSG5005'
     C                   EVAL      wk_MsgData = ps_Pgnm
     C                   EXSR      $SndSM
     C                   CALLP(E)  HYR2022 (GDORD#:GDTURN:wk_GCSHPV:wk_Track#:
     C                               *OFF:wk_RC2022)
     C                   EVAL      wk_Pgm2022 = *ON
     C                   ENDIF                                                  not wk_BOLReq and

     C                   IF        wk_OrdStsEml = 'Y'
     C                   IF        (wk_OELOC# = 91  or  wk_OELOC# = 93  or      Wholesale/Dealer/
     C                               wk_OELOC# = 95)                                Retail
     C*                            and wk_OEOSUS <> 'WSECS'                     Agilx ECommerce
     C                   CALLP(E)  HYR6080 (GDORD#:GDTURN:'SHIPPED':'*':
     C                               'HYR0614':*OFF:wk_RC6080)
     C                   EVAL      wk_Pgm6080 = *ON
     C                   ENDIF                                                  wk_OELOC#=91 or
     C                   ENDIF                                                  wk_OrdStsEml='Y'

     C                   ENDIF                                                  not wk_IntrHold

     C                   ENDIF                                                  wk_TurnShipped

     C                   ENDDO                                                  %eof(HYLSGDT3)

     C                   IF        wk_Abort
     C                   LEAVE
     C                   ENDIF                                                  wk_Abort

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                      GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                      GDCNVY='P' and GDQSTC=0;
       IF wk_RcdCnt > *Zero;
         EXEC SQL update HDITSL a set ISQRES=0
                  where a.ISWHS=6 and a.ISSTKR='060' and
                   a.ISQRES>0 and
                   exists (select * from HYPSGDT b where
                    b.GDGRP#=:wk_ShipGrp# and b.GDBLTO=:wk_BillTo# and
                    b.GDSHTO=:wk_ShipTo# and b.GDDSHP=:wk_DropShip# and
                    b.GDCNVY='P' and b.GDQSTC=0 and
                    substr(b.GDPLOC,1,1)=a.ISAILE and
                    substr(b.GDPLOC,3,7)=a.ISSLOC);
         IF SQLER3 > *Zero;                                                  // NOTE: SQLER3 has the
           wk_MsgID = 'MSG0418';                                             //   rcds updated count
           wk_MsgData = %triml(%editc(wk_ShipTo#:'Z'));
           %subst(wk_MsgData:8:7) = %triml(%editc(%dec(SQLER3:5:0):'J'));
           EXSR $WrtJobLog1;
         ELSE;
           wk_MsgID = 'MSG0419';
           wk_MsgData = %triml(%editc(wk_ShipTo#:'Z'));
           EXSR $WrtJobLog1;
         ENDIF;                                                              // SQLER3>*Zero
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C****               IF        not wk_SaveOnly
     C                   IF        wk_BOLReq  and  wk_CrtBOL  and
     C                               not wk_SaveOnly

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
       wk_CMPLDR = *Blank;
       IF wk_BuyGrpCust;
         EXEC SQL select CMPLDR into :wk_CMPLDR
                  from HDCUST2
                  where CMCUST = :wk_ShipTo#;
       ELSE;
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
      /End-Free

     C                   IF        wk_CMPLDR = 'Y'
     C                   EVAL      wk_Pgm0139A = *ON
     C                   IF        wk_OutQ1 = *Blanks
     C                   EVAL      wk_OutQType = 'PALLETRPT'
     C                   EXSR      $RtvOutQ
     C                   EVAL      wk_OutQ1 = wk_OutQ
     C                   SELECT
     C                   WHEN      wk_OutQ1 = 'PRT16'
     C                   EVAL      wk_Drawer = 5
     C                   OTHER
     C                   EVAL      wk_Drawer = 3
     C                   ENDSL
     C                   ENDIF                                                  wk_OutQ1=*Blanks
     C****               IF        wk_BillTo# <> 52                             Academy
     C****               EVAL      wk_ShowPO = '*NO'
     C****               ELSE
     C                   EVAL      wk_ShowPO = '*YES'
     C****               ENDIF                                                  wk_BillTo#<>52
     C                   EVAL      wk_Item#Type = '*CUST'
     C                   IF        not %open(HYLPLLT4)
     C                   OPEN      HYLPLLT4
     C                   ENDIF                                                  not %open(HYLPLLT4)
     C     wk_ShipTo#    SETLL     HYLPLLT4
     C                   DOU       %eof(HYLPLLT4)
     C     wk_ShipTo#    READE     HYLPLLT4
     C                   IF        %eof(HYLPLLT4)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLPLLT4)
     C                   IF        PMDSHP <> wk_DropShip#
     C                   ITER
     C                   ENDIF                                                  PMDSHP<>wk_DropShip#
     C                   IF        RtvPRSts1 (PMPSRL#) = '*PRINT'
     C                   ITER
     C                   ENDIF                                                  RtvPRSts1 (PMPSRL#)
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSSCC a
                where SCPSRL#=:PMPSRL# and exists (select *
                 from HYPSGDT b where b.GDSSCC=a.SCSRL# and
                 b.GDGRP#=:wk_ShipGrp# and b.GDBLTO=:wk_BillTo# and
                 b.GDSHTO=:wk_ShipTo# and b.GDDSHP=:wk_DropShip# and
                 b.GDORD#=a.SCORD# and b.GDORL#=a.SCORL# and
                 b.GDBLN#=a.SCBLN#);
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   CALLP(E)  HYC3512 (PMPSRL#:-1:-1:'*NO':wk_ShowPO:
     C                               wk_Item#Type:
     C                               '*YES':'*NONE':wk_OutQ1:
     C                               wk_Drawer:1:'*NO':'*NO')
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDDO                                                  %eof(HYLPLLT4)
     C                   ENDIF                                                  wk_CMPLDR='Y'

     C                   ENDIF                                                  not wk_SaveOnly

     C                   IF        wk_BOLReq  and  wk_CrtBOL  and
     C                               not wk_SaveOnly

     C                   EVAL      wk_MsgID = 'MSG5012'
     C                   EVAL      wk_MsgData = ps_Pgnm
     C                   EXSR      $SndSM

     C                   EXSR      $CrtBOL

     C                   ELSE

     C                   EVAL      wk_BOL# = *Zero

     C                   ENDIF                                                  wk_BOLReq and

     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm
     C                   EXSR      $SndSM

      /Free
       EXEC SQL update HYPSGCU set GCSTS='C', GCBOL#=:wk_BOL#,
                        GCTSTP2=current timestamp
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_Error

     C/EJECT
     C**************************************************************************
     C* Release intransit hold for fully-shipped retail and employee orders on *
     C* intransit hold only because order was marked "no backorders".          *
     C* Release intransit hold for fully-shipped non-foreign drop-shipment     *
     C* orders, regardless of customer type (i.e., location no.).              *
     C**************************************************************************
     CSR   $ShipCompRls  BEGSR

      *
      * Release intransit hold for fully-shipped retail and employee orders on
      * intransit hold only because order was marked "no backorders".
      *
     C                   IF        (wk_OELOC# = 95  or  wk_OELOC# = 97)  and    Retail or employee
     C                               wk_OEBOAL = 'N'

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEDTWK
                where O1OCTL=:GDOCTL and O1ORST<>'C' and
                      O1QORD > O1QSTC + O1QSTD;
      /End-Free

     C                   IF        wk_RcdCnt = *Zero
     C                   IF        not %open(HYPINTR)
     C                   OPEN      HYPINTR
     C                   EVAL      Zero_Turn = *Zero
     C                   EVAL      Zero_Invoice = *Zero
     C                   ENDIF                                                  not %open(HYPINTR)
     C                   EVAL      kf_Ord# = GDORD#
     C     key_IO        CHAIN     HYPINTR
     C                   IF        %found(HYPINTR)
     C                   IF        IORLSD = *Zero
     C                   EVAL      IORLSU = 'HYR0614'
     C                   EVAL      IORLSD = %dec(%date:*CYMD)
     C                   EVAL      IORLST = %dec(%time:*HMS)
     C                   UPDATE    HYRINTR
     C                   ELSE
     C                   UNLOCK    HYPINTR
     C                   ENDIF                                                  IORLSD=*Zero
     C                   ENDIF                                                  %found(HYPINTR)
     C                   ENDIF                                                  wk_RcdCnt=*Zero

     C                   ENDIF                                                  wk_OELOC#=95 or

      *
      * Release intransit hold for fully-shipped non-foreign drop-shipment
      * orders, regardless of customer type (i.e., location no.).
      *
     C                   IF        wk_DropShip# <> *Zero

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEDTWK
                where O1OCTL=:GDOCTL and O1ORST<>'C' and
                      O1QORD > O1QSTC + O1QSTD;
      /End-Free

     C                   IF        wk_RcdCnt = *Zero
     C                   IF        not %open(HYPINTR)
     C                   OPEN      HYPINTR
     C                   EVAL      Zero_Turn = *Zero
     C                   EVAL      Zero_Invoice = *Zero
     C                   ENDIF                                                  not %open(HYPINTR)
     C                   EVAL      kf_Ord# = GDORD#
     C     key_IO        CHAIN     HYPINTR
     C                   IF        %found(HYPINTR) and
     C                              (IOKYF1 <> 'CUSTCLASS'  or  IOKYV1 <> 'EX')
     C                   IF        IORLSD = *Zero
     C                   EVAL      IORLSU = 'HYR0110'
     C                   EVAL      IORLSD = %dec(%date:*CYMD)
     C                   EVAL      IORLST = %dec(%time:*HMS)
     C                   UPDATE    HYRINTR
     C                   ELSE
     C                   UNLOCK    HYPINTR
     C                   ENDIF                                                  IORLSD=*Zero
     C                   ENDIF                                                  %found(HYPINTR)
     C                   ENDIF                                                  wk_RcdCnt=*Zero

     C                   ENDIF                                                  wk_DropShip#<>*Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Credit card processing.                                                *
      **************************************************************************
     CSR   $CCProc       BEGSR

     C                   IF        wk_ShipAmt > *Zero

     C                   EVAL      wk_Pgm0810 = *ON
     C                   IF        wk_OECTRM = 'CP'
     C                   EVAL      wk_Pgm9906 = *ON
     C                   ENDIF                                                  wk_OECTRM='CP'
      /Free
         EVAL wk_CCAmt = *Zero;
         EXEC SQL Select coalesce(sum(OPPAYA),0) into :wk_CCamt
                  From OEPAYO Where OPORD=:GDORD# and OPCCID<>' ';
      /End-Free
     C                   SELECT
     C                   WHEN      wk_OECTRM = 'CC'  and  wk_CCAmt < wk_ShipAmt
     C                   EVAL      wk_MsgID = 'MSG5003'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-CC'
     C                   EXSR      $SndSM
      /Free
       wk_TaxAmt = *Zero;
       IF wk_OETAXO <> 'Y';
         EXEC SQL select coalesce(sum(TWSTTA+TWCNTA+TWCTTA+TWL1TA+TWL2TA+
                          TWL3TA),0)
                   into :wk_TaxAmt
                  from OETXWK
                  where TWOCTL=:GDOCTL;
       ELSE;
         EXEC SQL select coalesce(H1STTA+H1CNTA+H1CTTA+H1LC1A+H1LC2A+
                          +H1LC3A,0)
                   into :wk_TaxAmt
                  from OEHDWK
                  where H1OCTL=:GDOCTL;
       ENDIF;                                                                // wk_OETAXO<>'Y'

      //
      // Determine MFUKEY for new referential authorization request.
      //   1) Retrieve most recent RY archive MFUKEY (from HYPCCRH)
      //      associated with the order.  If found, use that MFUKEY.
      //   2) Otherwise, if OEORHD.OEUDF6 is non-blank, use it.  It should
      //      be the MFUKEY associated with the order's most recent prior
      //      authorization.
      //   3) Otherwise, retrieve most recent prior authorization MFUKEY
      //      (from HYPCCRH) associated with the order, and use it.
      *
       wk_RHUKEY = *Blanks;
       EXEC SQL select coalesce(max(RHUKEY),' ') into :wk_RHUKEY
                from HYPCCRH
                where RHORD#=:GDORD# and RHTYPE='RY';
       IF wk_RHUKEY <> *Blanks;
         wk_RefMFUKEY = wk_RHUKEY;
       ELSE;
         wk_RefMFUKEY = wk_OEUDF6;
       ENDIF;                                                                // wk_RHUKEY<>*Blanks
      /End-Free
     C****               IF        wk_OEUDF6 = *Blanks
     C                   IF        wk_RefMFUKEY = *Blanks
      /Free
       EXEC SQL select coalesce(max(RHUKEY),' ') into :wk_OEUDF6
                from HYPCCRH a
                where a.RHORD#=:GDORD# and a.RHTYPE='RA' and
                 exists (select * from C3/C3FP9020 b where
                 b.MFUKEY=a.RHUKEY and b.MF$LVD='0');
       wk_RefMFUKEY = wk_OEUDF6;
      /End-Free
     C                   ENDIF                                                  wk_RefMFUKEY=*Blanks

     C****               CALLP(E)  HYR0804C3 ('REAUTH':wk_OEUDF6:wk_ShipAmt:
     C                   CALLP(E)  HYR0804C3 ('REAUTH':wk_RefMFUKEY:wk_ShipAmt:
     C                               wk_TaxAmt:GDORD#:wk_CCFatal:wk_CCResponse:
     C                               wk_CCNewKey:wk_CCMsgText)
     C                   EVAL      wk_Pgm0804 = *ON
     C                   IF        %error
     C                   EVAL      wk_Error = -1001
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0804C3 REAUTH'
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ENDIF                                                  %error
     C                   IF        wk_CCFatal = '8'  or
     C                               wk_CCResponse <> 'UG'
     C                   EVAL      wk_Error = -1002
     C                   EVAL      wk_MsgID = 'ERR0508'
     C                   EVAL      wk_MsgData = wk_CCResponse + wk_CCMsgText +
     C                               %trim(%editc(wk_ShipAmt:'J'))
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ENDIF                                                  wk_CCFatal='8' or
     C                   IF        wk_CCNewKey <> wk_OEUDF6
     C                               and  wk_CCNewKey <> wk_RefMFUKEY
      /Free
       EXEC SQL insert into HYPCCRH
                  (RHORD#,RHUKEY,RHTYPE,RHTYP2,
                   RHAMT1,RHUSER,RHTSTP)
                  Values (:GDOCTL,:wk_CCNewKey,'RA','PA',
                          :wk_ShipAmt,:ps_User,current timestamp);
      /End-Free
     C                   ENDIF                                                  wk_CCNewKey<>wk_OEUD

      /Free
       wk_PaySeq# = *Zero;
       EXEC SQL Select coalesce(max(WPPSEQ),0) into :wk_PaySeq#
                From OEPAYW Where WPOCTL=:GDOCTL;
       wk_PaySeq# = wk_PaySeq# + 1;
       wk_PayAmt = *Zero;
       EXEC SQL Select MFRVNA, MFAMT1, MF$LVD
                  into :wk_PayTypeCode, :wk_PayAmt, :wk_LocalVoid
                From C3/C3FP9020 Where MFUKEY=:wk_CCNewKey;
       IF wk_LocalVoid = '1';
         wk_PayAmt = *Zero;
       ENDIF;                                                                // wk_LocalVoid='1'
       EXEC SQL Insert Into OEPAYW
                  (WPOCTL,WPPSEQ,WPPAYT,WPDESC,
                   WPPAYA,WPCHK,WPMEMO,WPCCTP,
                   WPCCID)
                  Values (:GDOCTL, :wk_PaySeq#,' ', 'HYR0614 auth',
                          :wk_PayAmt, ' ', ' ', :wk_PayTypeCode,
                          :wk_CCNewKey);
      /End-Free

     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-CC exit'
     C                   EXSR      $SndSM

     C                   WHEN      wk_OECTRM = 'CC'  and  wk_CCAmt > wk_ShipAmt
     C                   EVAL      wk_CCNewKey = wk_OEUDF6                      Retain unique key

     C                   WHEN      wk_OECTRM = 'CP'
     C                               and  wk_CCAmt < wk_ShipAmt
     C                   EVAL      wk_MsgID = 'MSG5003'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-CP'
     C                   EXSR      $SndSM
     C                   IF        not %open(HYPOREL)
     C                   OPEN      HYPOREL
     C                   ENDIF                                                  not %open(HYPOREL)
     C                   IF        not %open(HYLOREL1)
     C                   OPEN      HYLOREL1
     C                   ENDIF                                                  not %open(HYLOREL1)
     C     GDORD#        CHAIN     HYLOREL1
     C                   IF        not %found(HYLOREL1)
     C                   EVAL      wk_Settle$ = RtvOrdTranAmt(GDORD#:'SETTLED')
     C                               + RtvOrdTranAmt(GDORD#:'SETTLERQS')
     C                   EVAL      wk_Pgm0810 = *ON
     C                   EVAL      wk_StlAmt =
     C                               RtvOrdInvc$(GDORD#:*ON) + wk_ShipAmt
     C                   EVAL      wk_Pgm9906 = *ON
     C                   EVAL      wk_PrimOrd# = *Zero
     C                   ELSE
     C                   EVAL      wk_Settle$ = RtvOrdTranAmt(ORPORD#:'SETTLED')
     C                               + RtvOrdTranAmt(ORPORD#:'SETTLERQS')
     C                   EVAL      wk_Pgm0810 = *ON
     C                   EVAL      wk_StlAmt = wk_ShipAmt
     C                   EVAL      wk_PrimOrd# = ORPORD#
     C     wk_PrimOrd#   SETLL     HYPOREL
     C                   DOU       %eof(HYPOREL)
     C     wk_PrimOrd#   READE     HYPOREL
     C                   IF        %eof(HYPOREL)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPOREL)
     C                   EVAL      wk_StlAmt = wk_StlAmt +
     C                               RtvOrdInvc$(ORSORD#:*ON)
     C                   EVAL      wk_Pgm9906 = *ON
     C                   ENDDO                                                  %eof(HYPOREL)
     C                   ENDIF                                                  %found(HYLOREL1)

     C                   IF        wk_Settle$ < wk_StlAmt
      /Free
       wk_TaxAmt = *Zero;
       IF wk_OETAXO <> 'Y';
         EXEC SQL select coalesce(sum(TWSTTA+TWCNTA+TWCTTA+TWL1TA+TWL2TA+
                          TWL3TA),0)
                   into :wk_TaxAmt
                  from OETXWK
                  where TWOCTL=:GDOCTL;
       ELSE;
         EXEC SQL select coalesce(H1STTA+H1CNTA+H1CTTA+H1LC1A+H1LC2A+
                          +H1LC3A,0)
                   into :wk_TaxAmt
                  from OEHDWK
                  where H1OCTL=:GDOCTL;
       ENDIF;                                                                // wk_OETAXO<>'Y'
       IF wk_PrimOrd# <> *Zero;
         wk_TaxAmt2 = *Zero;
         EXEC SQL Select
                   coalesce(sum(HISTTA+HICNTA+HICTTA+HIL1TA+HIL2TA+HIL3TA),0)
                    into :wk_TaxAmt2
                  From HYPOREL a join OEIVHH b on a.ORSORD#=b.HIORD#
                  Where a.ORPORD#=:wk_PrimOrd#;
         IF wk_TaxAmt2 = *Zero;
           EXEC SQL Select
                     coalesce(sum(HISTTA+HICNTA+HICTTA+HIL1TA+HIL2TA+HIL3TA),0)
                      into :wk_TaxAmt2
                    From HYPOREL a join OEIVHHA b on a.ORSORD#=b.HIORD#
                    Where a.ORPORD#=:wk_PrimOrd#;
         ENDIF;                                                              // wk_TaxAmt2=*Zero
         wk_TaxAmt = wk_TaxAmt + wk_TaxAmt2;
       ENDIF;                                                                // wk_PrimOrd#<>*Zero
      /End-Free
     C                   CALLP(E)  SettleFinal(GDORD#:wk_StlAmt:wk_TaxAmt:
     C                               'S':
     C                               wk_TotCCAmt:wk_CCFatal:wk_CCResponse:
     C                               wk_CCMsgText)
     C                   EVAL      wk_Pgm0812 = *ON
     C                   IF        %error
     C                   EVAL      wk_Error = -1003
     C                   EVAL      wk_MsgID = 'ERR9013'
     C                   EVAL      wk_MsgData = 'SettleFinal    '
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ENDIF                                                  %error
     C                   IF        wk_CCFatal = '8'  or
     C                               wk_CCResponse <> 'UG'
     C                   EVAL      wk_Error = -1004
     C                   EVAL      wk_MsgID = 'ERR0508'
     C                   EVAL      wk_MsgData = wk_CCResponse + wk_CCMsgText +
     C                               %trim(%editc(wk_ShipAmt:'J'))
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ENDIF                                                  wk_CCFatal='8' or
     C                   ENDIF                                                  wk_Settle$<wk_StlAmt

     C                   EVAL      wk_MsgID = 'MSG0373'
     C                   EVAL      wk_MsgData = ps_Pgnm +
     C                               %triml(%editc(GDORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:15) =
     C                               %triml(%editc(wk_StlAmt:'Z'))
     C                   EVAL      %subst(wk_MsgData:34:15) =
     C                               %triml(%editc(wk_TaxAmt:'Z'))
     C                   EVAL      %subst(wk_MsgData:49:15) =
     C                               %triml(%editc(wk_TotCCAmt:'Z'))
     C                   EVAL      %subst(wk_MsgData:64:1) = wk_CCFatal
     C                   EVAL      %subst(wk_MsgData:65:2) = wk_CCResponse
     C                   EVAL      %subst(wk_MsgData:67:35) = wk_CCMsgText
     C                   EXSR      $WrtJobLog2

     C                   EVAL      wk_CCNewKey = wk_OEUDF6                      Retain unique key
     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-CP exit'
     C                   EXSR      $SndSM

     C                   OTHER
     C                   EVAL      wk_CCNewKey = wk_OEUDF6                      Retain unique key

     C                   ENDSL

     C                   ELSE
     C                   EVAL      wk_CCNewKey = wk_OEUDF6                      Retain unique key
     C                   ENDIF                                                  wK_ShipAmt>*Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process parcel shipments through Varsity.                              *
      **************************************************************************
     CSR   $Varsity      BEGSR

      *
      * The following 3 lines are temporary until ShipOptimzer and ShipSelect
      * programming is added to this program.
      *
     C                   IF        wk_ShipType <> 'P'
     C                   LEAVESR
     C                   ENDIF                                                  wk_ShipType=<>'P'

     C                   EVAL      wk_Varsity = *OFF

      /Free
       wk_VarsityOrd# = *Blanks;
       EXEC SQL select TDVORD into :wk_VarsityOrd#
                from HYPTDTA
                where TDTURN=:GDTURN;
       IF wk_VarsityOrd# = *Blanks;
         wk_Order#X = %triml(%editc(GDORD#:'Z'));
         IF GDORD# < 1000000;
           EXEC SQL select coalesce(max(translate(                           // Order numbers
                        substr(MHORDR,8,3),'0',' ')),0), count(*)            //  up to 999999
                     into :wk_LastVarSeq#, :wk_VarShipCnt
                    from MFH1MH
                    where substr(MHORDR,1,6)=:wk_Order#X;
         ELSE;
           EXEC SQL select coalesce(max(translate(                           // Order numbers
                        substr(MHORDR,9,2),'0',' ')),0), count(*)            //  after 999999
                     into :wk_LastVarSeq#, :wk_VarShipCnt
                    from MFH1MH
                    where substr(MHORDR,1,7)=:wk_Order#X;
         ENDIF;
         IF wk_VarShipCnt = *Zero;
           EVAL wk_VarsityOrd# = %triml(%editc(GDORD#:'Z'));
         ELSE;
           IF GDORD# < 1000000;
             EVAL wk_VarSeq#1 = wk_LastVarSeq# + 1;
             EVAL wk_VarsityOrd# = %triml(%editc(GDORD#:'Z')) + '-' +
                    %editc(wk_VarSeq#1:'X');
           ELSE;
             EVAL wk_VarSeq#2 = wk_LastVarSeq# + 1;
             EVAL wk_VarsityOrd# = %triml(%editc(GDORD#:'Z')) + '-' +
                    %editc(wk_VarSeq#2:'X');
           ENDIF;
         ENDIF;                                                              // wk_VarShipCnt=
       ENDIF;                                                                // wk_VarsityOrd#=
      /End-Free

     C                   EVAL      wk_SSShipVia = wk_GCSHPV
     C                   EVAL      wk_SSShpViaDes = wk_GCSVDS

     C                   EVAL      wk_PriorTrk# = wk_H1UDF3
     C                   CALLP(E)  VarHDInfD (%editc(GDORD#:'X'):
     C                               wk_VarsityOrd#:
     C                               %editc(GDOCTL:'X'):
     C                               %editc(GDTURN:'X'):
     C                               'SO':
     C                               wk_SSShipVia:
     C                               wk_SSShpViaDes)

      /Free
       wk_H1OCTL = *Zero;
       wk_H1UDF3 = *Blanks;
       EXEC SQL select H1OCTL, H1UDF3 into :wk_H1OCTL, :wk_H1UDF3
                from OEHDWK
                where H1OCTL=:GDOCTL;
      /End-Free
     C                   IF        wk_H1OCTL = GDOCTL
     C                   IF        wk_H1UDF3 = wk_PriorTrk#
     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-VS'
     C                   EXSR      $SndSM
     C                   EVAL      wk_Error = -1005
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'VarHDInfD Trk# not new'
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ELSE
      ********* NEED TO DEAL WITH "1Z6" FOR UPS!!
     C                   EVAL      wk_Track# = wk_H1UDF3
      /Free
       wk_MDFTRK = *Blanks;
       wk_MDFBRC = *Blanks;
       EXEC SQL select MDFTRK, MDFBRC
                 into :wk_MDFTRK, :wk_MDFBRC
                from MFD1MD
                where MDCMNO='01' and MDOTYP=' ' and
                      MDORDR=:wk_VarsityOrd# and
                      (MDFTRK like '%' concat trim(:wk_Track#) concat '%' or
                       MDFBRC like '%' concat trim(:wk_Track#) concat '%')
                order by MDBXNO
                fetch first row only;
       SELECT;
       WHEN wk_MDFTRK <> *Blanks;
         wk_Track# = wk_MDFTRK;
       WHEN wk_MDFBRC <> *Blanks;
         wk_Track# = wk_MDFBRC;
       ENDSL;

       EXEC SQL update HYPSGCU set GCPRO#=:wk_Track#
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;
      /End-Free
     C                   EVAL      wk_Varsity = *ON
     C                   ENDIF                                                  wk_H1UDF3=wk_PriorTr
     C                   ENDIF                                                  *IN95=*OFF

      *
      * If Varsity was processed (for UPS, FedEx, etc.), and the customer
      * requires UCC-128 license plate number association with tracking number
      * (for EDI), call the program that allows (and forces) that association.
      *
     C                   IF        wk_Varsity
      /Free
       wk_BillTo#X = %editc(GDBLTO:'X');
       wk_TDKEY1 = *Blanks;
       EXEC SQL select TDKEY1 into :wk_TDKEY1
                from GUPTDAT
                where TDTABL='SSCCTRK' and TDKEY1=:wk_BillTo#X and
                TDKEY2=:wk_DatabaseID;
       IF wk_TDKEY1 = *Blanks;
         EXEC SQL select TDKEY1 into :wk_TDKEY1
                  from GUPTDAT
                  where TDTABL='SSCCTRK' and TDKEY1='*ALLPARCEL' and
                  TDKEY2=:wk_DatabaseID;
       ENDIF;                                                                // wk_TDKEY1=*Blanks
      /End-Free
     C                   IF        wk_TDKEY1 = wk_BillTo#X
     C                               or  wk_TDKEY1='*ALLPARCEL'
     C****               CALLP(E)  HYR0500 (GDORD#:GDTURN:wk_RC0500)
      /Free
       wk_ShipVia = *Blanks;
       wk_MHSHIN = *Blanks;
       EXEC SQL select MHSHIN into :wk_MHSHIN
                from MFH1MH
                where MHCMNO='01' and MHOTYP=' ' and
                      MHORDR=:wk_VarsityOrd#;
       IF %len(%trimr(wk_MHSHIN)) = 2;
         wk_ShipVia = wk_MHSHIN;
       ELSE;
         wk_ShipVia = wk_GCSHPV;
       ENDIF;                                                                // %len(%trimr(wk_MHSHI
      /End-Free
     C                   CLEAR                   AryOrd
     C                   CLEAR                   AryTurn
     C                   EVAL      AryOrd(1) = GDORD#
     C                   EVAL      AryTurn(1) = GDTURN

     C                   CALLP(E)  HYR0500 ('1':ps_Pgnm:
     C                                      GDORD#:GDTURN:*Zero:*Zero:
     C                                      wk_ShipVia:
     C                                      AryOrd:AryTurn:
     C                                      wk_RC0500)
     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-VS'
     C                   EXSR      $SndSM
     C                   EVAL      wk_Error = -1006
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0500   %error=''1'''
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   WHEN      wk_RC0500 <> *Zeros
     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-VS'
     C                   EXSR      $SndSM
     C                   EVAL      wk_Error = -1007
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0500   RtnCode=' + wk_RC0500
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ENDSL
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ENDIF                                                  wk_Varsity

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process parcel shipments through Varsity for multiple orders/turns.    *
      **************************************************************************
     CSR   $VarsityCS    BEGSR

     C                   EVAL      wk_Varsity = *OFF

     C                   IF        wk_ShipType <> 'P'
     C                   LEAVESR
     C                   ENDIF                                                  wk_ShipType=<>'P'

      /Free
       ary_Ord# = *Zero;
       ary_Turn = *Zero;
       wk_SSMstOrd# = *Zero;
       wk_SSMstTurn = *Zero;
       wk_SSMstOCtl = *Zero;
       wk_StartTurn = *Zero;
       FOR idx1 = 1 to wk_TurnCnt;                                           // Create array
         wk_GDTURN = *Zero;                                                  //  of orders
         wk_GDOCTL = *Zero;                                                  //  of orders
         wk_GDORD# = *Zero;
         EXEC SQL select GDTURN, GDOCTL, GDORD#
                   into :wk_GDTURN, :wk_GDOCTL, :wk_GDORD#
                  from HYPSGDT
                  where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                        GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                        GDTURN>:wk_StartTurn
                  order by GDTURN
                  fetch first row only;
         ary_Ord#(idx1) = wk_GDORD#;
         ary_Turn(idx1) = wk_GDTURN;
         IF idx1 = 1;
           wk_SSMstTurn = wk_GDTURN;
           wk_SSMstOCtl = wk_GDOCTL;
           wk_SSMstOrd# = wk_GDORD#;
         ENDIF;                                                              // idx1=1
         wk_StartTurn = wk_GDTURN;
       ENDFOR;

       wk_VarsityOrd# = *Blanks;
       EXEC SQL select TDVORD into :wk_VarsityOrd#
                from HYPTDTA
                where TDTURN=:wk_SSMstTurn;
       IF wk_VarsityOrd# = *Blanks;
         wk_Order#X = %triml(%editc(wk_SSMstOrd#:'Z'));
         wk_LastVarSq#1 = *Zero;
         wk_VarShipCnt1 = *Zero;
         wk_LastVarSq#2 = *Zero;
         wk_VarShipCnt2 = *Zero;
           EXEC SQL select coalesce(max(translate(                           // Order numbers
                        substr(MHORDR,9,2),'0',' ')),0), count(*)            //  after 999999
                     into :wk_LastVarSq#1, :wk_VarShipCnt1
                    from MFH1MH
                    where substr(MHORDR,1,7)=:wk_Order#X;
           EXEC SQL select coalesce(max(translate(                           // Order numbers
                        substr(MHORDR,9,2),'0',' ')),0), count(*)            //  after 999999
                     into :wk_LastVarSq#2, :wk_VarShipCnt2
                    from MFHHMH
                    where substr(MHORDR,1,7)=:wk_Order#X;
         IF wk_VarShipCnt1 = *Zero  and  wk_VarShipCnt2 = *Zero;
           EVAL wk_VarsityOrd# = %triml(%editc(wk_SSMstOrd#:'Z'));
         ELSE;
           IF wk_LastVarSq#1 > wk_LastVarSq#2;
             EVAL wk_VarSeq#1 = wk_LastVarSq#1 + 1;
           ELSE;
             EVAL wk_VarSeq#1 = wk_LastVarSq#2 + 1;
           ENDIF;                                                            // wk_LastVarSq#1>
           SELECT;
           WHEN wk_VarSeq#1 < 100;
             EVAL wk_VarSeq#2 = wk_VarSeq#1;
           OTHER;
             EVAL wk_VarSeq#2 = 99;                                          // Seq "-99"
           ENDSL;
             EVAL wk_VarsityOrd# = %triml(%editc(wk_SSMstOrd#:'Z')) + '-' +
                    %editc(wk_VarSeq#2:'X');
         ENDIF;                                                              // wk_VarShipCnt=
       ENDIF;                                                                // wk_VarsityOrd#=
       wk_VarsityOrd2 = wk_VarsityOrd#;
      /End-Free

     C                   CALLP(E)  VPRSWOGINF (wk_VarsityOrd2:ary_Ord#)
     C                   IF        %error
     C                   EVAL      wk_Error = -1008
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'VPRSWOGINF Error on call'
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ENDIF                                                  %error

      /Free
       wk_H1OCTL = *Zero;
       wk_H1UDF3 = *Blanks;
       wk_H1SVSV = *Blanks;
       EXEC SQL select H1OCTL, H1UDF3, H1SVSV
                 into :wk_H1OCTL, :wk_H1UDF3, :wk_H1SVSV
                from OEHDWK
                where H1OCTL=:wk_SSMstOCtl;
       wk_PriorTrk# = wk_H1UDF3;

       EXEC SQL update OEHDWK
                 set H1SVSV=:wk_GCSHPV
                where H1OCTL=:wk_SSMstOCtl;
      /End-Free

     C                   EVAL      wk_SSShipVia = *Blanks
     C                   EVAL      wk_SSShpViaDes = *Blanks

     C                   CALLP(E)  VarHDInfD (%editc(wk_SSMstOrd#:'X'):
     C                               wk_VarsityOrd#:
     C                               %editc(wk_SSMstOCtl:'X'):
     C                               %editc(wk_SSMstTURN:'X'):
     C                               'MO':
     C                               wk_SSShipVia:
     C                               wk_SSShpViaDes)

      /Free
       wk_H1OCTL = *Zero;
       wk_H1UDF3 = *Blanks;
       EXEC SQL select H1OCTL, H1UDF3 into :wk_H1OCTL, :wk_H1UDF3
                from OEHDWK
                where H1OCTL=:wk_SSMstOCtl;
      /End-Free
     C                   IF        wk_H1OCTL = wk_SSMstOCtl
     C                   IF        wk_H1UDF3 = wk_PriorTrk#
      /Free
       EXEC SQL update OEHDWK
                 set H1SVSV=:wk_H1SVSV
                where H1OCTL=:wk_SSMstOCtl;
      /End-Free
     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-VS'
     C                   EXSR      $SndSM
     C                   EVAL      wk_Error = -1005
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'VarHDInfD Trk# not new'
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ELSE
      ********* NEED TO DEAL WITH "1Z6" FOR UPS!!
     C                   EVAL      wk_Track# = wk_H1UDF3
      /Free
       wk_MDFTRK = *Blanks;
       wk_MDFBRC = *Blanks;
       EXEC SQL select MDFTRK, MDFBRC
                 into :wk_MDFTRK, :wk_MDFBRC
                from MFD1MD
                where MDCMNO='01' and MDOTYP=' ' and
                      MDORDR=:wk_VarsityOrd# and
                      (MDFTRK like '%' concat trim(:wk_Track#) concat '%' or
                       MDFBRC like '%' concat trim(:wk_Track#) concat '%')
                order by MDBXNO
                fetch first row only;
       SELECT;
       WHEN wk_MDFTRK <> *Blanks;
         wk_Track# = wk_MDFTRK;
       WHEN wk_MDFBRC <> *Blanks;
         wk_Track# = wk_MDFBRC;
       ENDSL;

       EXEC SQL update HYPSGCU set GCPRO#=:wk_Track#
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;
      /End-Free
     C                   EVAL      wk_Varsity = *ON
     C                   ENDIF                                                  wk_H1UDF3=wk_PriorTr
     C                   ENDIF                                                  *IN95=*OFF

      *
      * If Varsity was processed (for UPS, FedEx, etc.), and the customer
      * requires UCC-128 license plate number association with tracking number
      * (for EDI), call the program that allows (and forces) that association.
      *
     C                   IF        wk_Varsity
      /Free
       wk_BillTo#X = %editc(wk_BillTo#:'X');
       wk_TDKEY1 = *Blanks;
       EXEC SQL select TDKEY1 into :wk_TDKEY1
                from GUPTDAT
                where TDTABL='SSCCTRK' and TDKEY1=:wk_BillTo#X and
                TDKEY2=:wk_DatabaseID;
       IF wk_TDKEY1 = *Blanks;
         EXEC SQL select TDKEY1 into :wk_TDKEY1
                  from GUPTDAT
                  where TDTABL='SSCCTRK' and TDKEY1='*ALLPARCEL' and
                  TDKEY2=:wk_DatabaseID;
       ENDIF;                                                                // wk_TDKEY1=*Blanks
      /End-Free
     C                   IF        wk_TDKEY1 = wk_BillTo#X
     C                               or  wk_TDKEY1='*ALLPARCEL'
      /Free
       wk_ShipVia = *Blanks;
       wk_MHSHIN = *Blanks;
       EXEC SQL select MHSHIN into :wk_MHSHIN
                from MFH1MH
                where MHCMNO='01' and MHOTYP=' ' and
                      MHORDR=:wk_VarsityOrd#;
       IF %len(%trimr(wk_MHSHIN)) = 2;
         wk_ShipVia = wk_MHSHIN;
       ELSE;
         wk_ShipVia = wk_GCSHPV;
       ENDIF;                                                                // %len(%trimr(wk_MHSHI
      /End-Free

     C                   CALLP(E)  HYR0500 ('2':ps_Pgnm:                        SWOG mode
     C                                      *Zero:*Zero:wk_ShipTo#:
     C                                      wk_DropShip#:wk_ShipVia:
     C                                      Ary_Ord#:Ary_Turn:
     C                                      wk_RC0500)
     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-VS'
     C                   EXSR      $SndSM
     C                   EVAL      wk_Error = -1006
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0500   %error=''1'''
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   WHEN      wk_RC0500 <> *Zeros
     C                   EVAL      wk_MsgID = 'MSG5000'
     C                   EVAL      wk_MsgData = ps_Pgnm + '-VS'
     C                   EXSR      $SndSM
     C                   EVAL      wk_Error = -1007
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0500   RtnCode=' + wk_RC0500
     C                   EXSR      $WrtJobLog1
     C                   LEAVESR
     C                   ENDSL
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ENDIF                                                  wk_Varsity

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Create BOL.                                                            *
      **************************************************************************
     CSR   $CrtBOL       BEGSR

      *
      * Retrieve next BOL number.
      *
     C                   EVAL      wk_BOL# = RtvNxtBOL# ()

      *
      * Write OEBOLH record.
      *
     C                   EXSR      $WrtOEBOLH

      *
      * Write OEBOLD records.
      *
     C                   EXSR      $WrtOEBOLD

      *
      * The next 3 subroutines need to executed for every turnaround in
      * the customer shipment.
      *
     C                   EVAL      sv_TURN = -1
     C     key_GD3       SETLL     HYLSGDT3
     C                   DOU       %eof(HYLSGDT3)
     C     key_GD3       READE     HYLSGDT3
     C                   IF        %eof(HYLSGDT3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT3)
     C                   IF        GDTURN = sv_TURN
     C                   ITER
     C                   ENDIF                                                  GDTURN=sv_TURN
     C                   EVAL      sv_TURN = GDTURN

      *
      * Update HYPBOL records with BOL number.
      *
     C                   EXSR      $UpdHYPBOL

      *
      * Update OEPKLH records with BOL number.
      *
     C                   EXSR      $UpdOEPKLH

      *
      * Update HYPSSCC records with BOL number.
      *
     C                   EXSR      $UpdHYPSSCC

     C                   ENDDO                                                  %eof(HYLSGDT3)

      *
      * Update HYPPLLT records with BOL number.
      *
     C                   EXSR      $UpdHYPPLLT

      *
      * Print pallet banners for all shipment pallets where the banner has not
      * yet been printed with the shipment's current PRO number.  Banners are
      * printed at printers based on HYPWSOQ.PLTBNRAUTO definition.
      *
     C                   CALLP(E)  HYR3552 (wk_BOL#:ps_Pgnm:wk_RC3552)

      *
      * Print the bill of lading.
      *
     C                   CALLP(E)  HYR0142 (wk_BOL#:'*NONE')

      *
      * If foreign (but not Canada), and explosive labels have been generated,
      * print powder weight summary report.
      *
     C                   IF        wk_Foreign and  wk_STCtry <> 'CA'
      /Free
        wk_AmmoCtnCnt = *Zero;
        EXEC SQL select count(*)
                  into :wk_AmmoCtnCnt
                 from HYPSSCC a
                 where a.SCBOL#=:wk_BOL# and
                   exists (select * from HYPELD b where b.EDSRL#=a.SCSRL#);
      /End-Free
     C                   IF        wk_AmmoCtnCnt > *Zero
     C                   CALLP(E)  HYR3504 (wk_BOL#:'*ENV')
     C                   ENDIF                                                  wk_AmmoCtnCnt>*Zero
     C                   ENDIF                                                  wk_Foreign and

      *
      * For EDI customer, write EO56TK record.
      *
     C                   IF        wk_EDICust
     C                   EXSR      $WrtEO56TK
     C                   ENDIF                                                  wk_EDICust

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write OEBOLH record.                                                   *
      **************************************************************************
     CSR   $WrtOEBOLH    BEGSR

     C                   EVAL      wk_BCM8 = *Blanks
     C                   EVAL      %subst(wk_BCM8:29:4) = 'WEST'

      /Free
       wk_SVSCAC = *Blanks;
       EXEC SQL select SVSCAC into :wk_SVSCAC
                from HDSHPV
                where SVSVSV=:wk_GCSHPV;
      /End-Free
     C****               EVAL      wk_CurCYMD = %dec(%date:*CYMD)
     C****               EVAL      wk_CurHMS = %dec(%time:*HMS)
     C                   IF        wk_EDICUST
     C                   EVAL      wk_ASNGen = 'Y'
     C                   ELSE
     C                   EVAL      wk_ASNGen = 'N'
     C                   ENDIF                                                  wk_EDICust
      **********  need to calc est delivery date
     C                   EVAL      wk_EstDlvyDt = *Zero

      *********** Need to populate wk_TotPkgs and wk_TotWgt

      /Free
       EXEC SQL insert into OEBOLH
                  (SDSTS, SDBOL#, SDBOLP, SDSHFM,
                   SDSHTO, SDDSHP, SDNAME, SDADR1,
                   SDADR2, SDADR3, SDCITY, SDST,
                   SDZIP, SDCTRY, SDBLTO, SDSHPV,
                   SDSVDS, SDCRCD, SDSCAC, SDPRO#,
                   SDSHDT, SDSTIM, SDBOLR, SDASNG,
                   SDTPKG, SDTWGT, SDCFPB, SDCODF,
                   SDCODA, SDRMTO, SDEQCD, SDEQIN,
                   SDEQNO, SDERPH, SDBOLC, SDBHF1,
                   SDBHF2, SDBHF3, SDBHF4, SDBHF5,
                   SDBHF6, SDBHF7, SDBHF8, SDBCM1,
                   SDBCM2, SDBCM3, SDBCM4, SDBCM5,
                   SDBCM6, SDBCM7, SDBCM8, SDCBTC,
                   SDSTZN, SDESTD, SDESTT, SDTRMC,
                   SDPPCD, SDPPLC, SDERSQ, SDFOB,
                   SDFRB#, SDTRL#, SDAWB#, SDSEAL)
                 values ('S', :wk_BOL#, 'Y', 6,
                         :wk_ShipTo#, :wk_DropShip#, :wk_STName, :wk_STAdr1,
                         :wk_STAdr2, :wk_STAdr3, :wk_STCity, :wk_STST,
                         :wk_StZip, :wk_STCTRY, :wk_BillTo#, :wk_GCSHPV,
                         :wk_GCSVDS, ' ', :wk_SVSCAC, :wk_Track#,
                         :wk_CurCYMD, :wk_CurHMS, 'Y', :wk_ASNGen,
                         :wk_TotPkgs, :wk_TotWgt, ' ', 0,
                         0, 0, ' ', ' ',
                         ' ', 0, ' ', ' ',
                         ' ', ' ', ' ', ' ',
                         ' ', ' ', ' ', ' ',
                         ' ', ' ', ' ', ' ',
                         ' ', ' ', :wk_BCM8, ' ',
                         'CT', :wk_EstDlvyDt, 0, ' ',
                         ' ', ' ', ' ', 'DK',
                         ' ', ' ', ' ', ' ');
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write OEBOLD records.                                                  *
      **************************************************************************
     CSR   $WrtOEBOLD    BEGSR

     C                   EVAL      wk_EmergInfo = *Blank

      *
      * Retrieve freight class quantities and write detail records.
      *
     C                   FOR       idx4 = 1 to idx3

     C                   IF        FCCQ_CSQT2(idx4) <> *Zero  or
     C                               FCCQ_WGT2(idx4) <> *Zero  or
     C                               FCCQ_PLLT2(idx4) <> *Zero
     C                   EVAL      wk_FrtCls = FCCQ_FTCL2(idx4)
     C                   EVAL      wk_PkgCnt = FCCQ_CSQT2(idx4)
     C                   EVAL      wk_ShipWgt = FCCQ_WGT2(idx4)
     C                   EVAL      wk_ClsPlltCnt = FCCQ_PLLT2(idx4)
      /Free
       wk_FTLD01 = *Blanks;
       wk_FTLD02 = *Blanks;
       wk_FTLD03 = *Blanks;
       wk_FTHAZM = *Blank;
       wk_FTRPQT = *Zero;
       wk_FTNMFC = *Blanks;
       EXEC SQL select FTLD01, FTLD02, FTLD03, FTHAZM,
                       FTRPQT, FTNMFC
                 into :wk_FTLD01, :wk_FTLD02, :wk_FTLD03, :wk_FTHAZM,
                      :wk_FTRPQT, :wk_FTNMFC
                from HDFTCL
                where FTCLAS=:wk_FrtCls;
       IF wk_FTHAZM = *Blank;
         wk_FTHAZM = 'N';
       ENDIF;                                                                // wk_FTHAZM=*Blank

       wk_OceanBOL = *OFF;
       IF wk_STCTRY = 'US';
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from GUPTDAT
                  where TDTABL='BOLOCEAN' and TDKEY1=:wk_STST;
         IF wk_RcdCnt > *Zero;
           wk_OceanBOL = *ON;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDIF;                                                                // wk_STCTRY='US'

       IF wk_OceanBOL;
         wk_TDKEY2 = *Blanks;
         wk_TDCF01 = *Blanks;
         wk_TDCF02 = *Blanks;
         wk_TDCF03 = *Blanks;
         wk_TDFF01 = *Blank;
         wk_TDFF02 = *Blank;
         wk_TDKEY1 = %trimr(wk_STCTRY) + '-OCEAN';
         EXEC SQL select TDKEY2, TDCF01, TDCF02, TDCF03,
                         TDFF01, TDFF02
                   into :wk_TDKEY2, :wk_TDCF01, :wk_TDCF02, :wk_TDCF03,
                        :wk_TDFF01, :wk_TDFF02
                  from GUPTDAT
                  where TDTABL='FRTCLSCTRY' and TDKEY1=:wk_TDKEY1 and
                  TDKEY2=:wk_FrtCls;
         IF wk_TDKEY2 = wk_FrtCls;
           IF wk_TDCF01 <> '*DFT';
             wk_FTLD01 = wk_TDCF01;
           ENDIF;
           EVAL wk_FTHAZM = wk_TDFF01;
           wk_BLUDA1 = RtvCDVar(wk_ShipGrp#:wk_BillTo#:wk_ShipTo#:
                                wk_DropShip#:wk_TDCF02:wk_FrtCls);
           wk_BLUDA2 = wk_TDCF03;
           IF wk_TDFF02 = 'Y';
             wk_EmergInfo = 'Y';
           ENDIF;                                                            // wk_TDFF02='Y'
         ELSE;
           wk_OceanBOL = *OFF;
         ENDIF;                                                              // wk_TDKEY2=wk_FrtCls
       ENDIF;                                                                // wk_OceanBOL

       IF not wk_OceanBOL;
       wk_TDKEY2 = *Blanks;
       wk_TDCF01 = *Blanks;
       wk_TDCF02 = *Blanks;
       wk_TDCF03 = *Blanks;
       wk_TDFF01 = *Blank;
       wk_TDFF02 = *Blank;
       wk_TDKEY1 = wk_STCTRY;
       EXEC SQL select TDKEY2, TDCF01, TDCF02, TDCF03,
                       TDFF01, TDFF02
                 into :wk_TDKEY2, :wk_TDCF01, :wk_TDCF02, :wk_TDCF03,
                      :wk_TDFF01, :wk_TDFF02
                from GUPTDAT
                where TDTABL='FRTCLSCTRY' and TDKEY1=:wk_TDKEY1 and
                TDKEY2=:wk_FrtCls;
       IF wk_TDKEY2 = wk_FrtCls;
         IF wk_TDCF01 <> '*DFT';
           wk_FTLD01 = wk_TDCF01;
         ENDIF;
         EVAL wk_FTHAZM = wk_TDFF01;
         wk_BLUDA1 = RtvCDVar(wk_ShipGrp#:wk_BillTo#:wk_ShipTo#:
                              wk_DropShip#:wk_TDCF02:wk_FrtCls);
         wk_BLUDA2 = wk_TDCF03;
         IF wk_TDFF02 = 'Y';
           wk_EmergInfo = 'Y';
         ENDIF;                                                              // wk_TDFF02='Y'
       ELSE;
         IF wk_Foreign  and  wk_STCTRY <> 'CA';
           wk_TDKEY1 = 'ALLFRGN';
           EXEC SQL select TDKEY2, TDCF01, TDCF02, TDCF03,
                           TDFF01, TDFF02
                     into :wk_TDKEY2, :wk_TDCF01, :wk_TDCF02, :wk_TDCF03,
                          :wk_TDFF01, :wk_TDFF02
                    from GUPTDAT
                    where TDTABL='FRTCLSCTRY' and TDKEY1=:wk_TDKEY1 and
                    TDKEY2=:wk_FrtCls;
           IF wk_TDKEY2 = wk_FrtCls;
             IF wk_TDCF01 <> '*DFT';
               wk_FTLD01 = wk_TDCF01;
             ENDIF;
             EVAL wk_FTHAZM = wk_TDFF01;
             wk_BLUDA1 = RtvCDVar(wk_ShipGrp#:wk_BillTo#:wk_ShipTo#:
                                  wk_DropShip#:wk_TDCF02:wk_FrtCls);
             wk_BLUDA2 = wk_TDCF03;
             IF wk_TDFF02 = 'Y';
               wk_EmergInfo = 'Y';
             ENDIF;                                                          // wk_TDFF02='Y'
           ELSE;
             wk_BLUDA1 = *Blanks;
             wk_BLUDA2 = *Blanks;
           ENDIF;                                                            // wk_TDKEY2=wk_FrtCls
         ELSE;
           wk_BLUDA1 = *Blanks;
           wk_BLUDA2 = *Blanks;
         ENDIF;                                                              // wk_Foreign and
       ENDIF;                                                                // wk_TDKEY2=wk_FrtCls
       ENDIF;                                                                // not wk_OceanBOL

       IF wk_FTHAZM = 'Y';
         IF wk_FTRPQT = *Zero;
           wk_HazMtl = HazYes;
         ELSE;
           wk_HazMtl = HazRptQty;
         ENDIF;                                                              // wk_FTRPQT=*Zero
       ELSE;
         wk_HazMtl = *Blanks;
       ENDIF;                                                                // wk_FTHAZM='Y'

       EXEC SQL insert into OEBOLD
                  (BLBOL#, BLFTCL, BLFDS1, BLFDS2,
                   BLFDS3, BLPKGS, BLPKUM, BLWGT,
                   BLGRSW, BLWTUM, BLHAZM, BLNMFC,
                   BLUDA1, BLUDA2, BLUDA3, BLUDN1,
                   BLUDN2, BLUDN3)
                 values (:wk_BOL#, :wk_FrtCls, :wk_FTLD01, :wk_FTLD02,
                         :wk_FTLD03, :wk_PkgCnt, 'CS', :wk_ShipWgt,
                         0, 'LB', :wk_HazMtl, :wk_FTNMFC,
                         :wk_BLUDA1, :wk_BLUDA2, ' ', :wk_ClsPlltCnt,
                         0, 0);
      /End-Free
     C                   ENDIF                                                  FCCQ_CSQT(idx2)<>*Ze

     C                   ENDFOR                                                 idx4=1 to idx3

     C                   IF        wk_EmergInfo = 'Y'
      /Free
        EXEC SQL Update OEBOLH set SDERPH=:wk_EmergPhone
                 Where SDBOL#=:wk_BOL#;
      /End-Free
     C                   ENDIF                                                  wk_EmergInfo='Y'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Update HYPBOL records.                                                 *
      **************************************************************************
     CSR   $UpdHYPBOL    BEGSR

      /Free
       EXEC SQL update HYPBOL set BIBOL#=:wk_BOL#, BIPRTD=:wk_CurCYMD
                where BIORD#=:GDORD# and BITURN=:GDTURN and BIAIV#=0;
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Update OEPKLH records.                                                 *
      **************************************************************************
     CSR   $UpdOEPKLH    BEGSR

      /Free
       wk_PLPKL# = *Zero;
       EXEC SQL select PLPKL# into :wk_PLPKL#
                from OEPKLD
                where PLORD#=:GDORD# and PLTURN=:GDTURN
                order by PLORD#, PLPKL# desc
                fetch first row only;

       IF wk_PLPKL# = *Zero;
         LEAVESR;
       ENDIF;                                                                // wk_PLPKL#=*Zero

       EXEC SQL update OEPKLH set SDBOL#=:wk_BOL#
                where SDPKL#=:wk_PLPKL#;
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Update HYPSSCC records.                                                *
      **************************************************************************
     CSR   $UpdHYPSSCC   BEGSR

      /Free
       EXEC SQL update HYPSSCC set SCBOL#=:wk_BOL#
                where SCORD#=:GDORD# and SCTURN=:GDTURN;
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Update HYPPLLT records.                                                *
      **************************************************************************
     CSR   $UpdHYPPLLT   BEGSR

      /Free
       EXEC SQL update HYPPLLT a set PMBOL#=:wk_BOL#
                where a.PMBOL#=0 and a.PMSHTO=:wk_ShipTo# and
                 exists (select * from HYPSSCC b where b.SCPSRL#=a.PMPSRL# and
                       b.SCBOL#=:wk_BOL#);
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write EO56TK record.                                                   *
      **************************************************************************
     CSR   $WrtEO56TK    BEGSR

     C                   CALLP(E)  HYR0148 (wk_BOL#:wk_ShipTo#:*Zero:*OFF)
     C                   EVAL      wk_Pgm0148 = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Build FCCQ data sructure.                                              *
      **************************************************************************
     CSR   $BldFCCQ      BEGSR

     C                   EVAL      FCCQ_FTCL = *Blanks
     C                   EVAL      FCCQ_CSQT = *Zero
     C                   EVAL      FCCQ_WGT = *Zero
     C                   EVAL      FCCQ_PLLT = *Zero
     C                   EVAL      wk_FCCQWgtx = *Zero
     C                   EVAL      idx1 = *Zero

     C                   EVAL      kf_Turn# = GDTURN
     C     key_GD1       SETLL     HYLSGDT1
     C                   DOU       %eof(HYLSGDT1)
     C     key_GD1       READE     HYLSGDT1
     C                   IF        %eof(HYLSGDT1)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT1)

      *
      * If line was not shipped, skip it.
      *
     C                   IF        G1QSTC = *Zero
     C                   ITER
     C                   ENDIF                                                  G1QSTC=*Zero

      /Free
       IF G1ITEM <> wk_IMITEM;
         wk_IMITEM = *Blanks;
         wk_IMPCLS = *Blanks;
         wk_IMIMWG = *Zero;
         wk_IMUDN4 = *Zero;
         wk_IMUOMW = *Blanks;
         EXEC SQL select IMITEM, IMPCLS, IMIMWG, IMUDN4,
                         IMUOMW
                   into :wk_IMITEM, :wk_IMPCLS, :wk_IMIMWG, :wk_IMUDN4,
                        :wk_IMUOMW
                  from HDIMST
                  where IMITEM=:G1ITEM;

         wk_TDKEY1 = *Blanks;
         EXEC SQL select TDKEY1 into :wk_TDKEY1
                  from GUPTDAT
                  where TDTABL='BULKPRDCLS' and TDKEY1=:wk_IMPCLS;
         IF wk_TDKEY1 = wk_IMPCLS;
           wk_Bulk = *ON;
         ELSE;
           wk_Bulk = *OFF;
         ENDIF;                                                              // wk_TDKEY1=wk_IMPCLS
       ENDIF;                                                                // G1ITEM<>wk_IMITEM

       wk_O1WH = *Zero;
       EXEC SQL select O1WH into :wk_O1WH
                from OEDTWK
                where O1OCTL=:G1OCTL and O1ORL#=:G1ORL# and
                      O1BLN#=:G1BLN#;

       IF G1ITEM <> wk_IWITEM  or  wk_O1WH <> wk_IWWHS;
         wk_IWITEM = *Blanks;
         wk_IWWHS = *Zero;
         wk_IWFTCL = *Blanks;
         EXEC SQL select IWITEM, IWWHS, IWFTCL
                   into :wk_IWITEM, :wk_IWWHS, :wk_IWFTCL
                  from HDIWHS
                  where IWITEM=:G1ITEM and IWWHS=:wk_O1WH;
       ENDIF;                                                                // G1ITEM<>wk_IWITEM or
      /End-Free

     C                   IF        not wk_Bulk
     C                   EVAL(H)   wk_LineWgt = G1QSTC * wk_IMIMWG
     C                   ELSE
     C                   EVAL(H)   wk_LineWgt = G1QSTC * wk_IMUDN4
     C                   ENDIF                                                  not wk_Bulk
     C                   IF        wk_IMUOMW = 'OZ'
     C                   EVAL(H)   wk_LineWgt = wk_LineWgt / 16
     C                   ENDIF                                                  wk_IMUOMW='OZ'

     C                   EVAL      idx2 = %lookup(wk_IWFTCL:FCCQ_FTCL:1:idx1)
     C                   IF        idx2 = *Zero
     C                   EVAL      idx1 = idx1 + 1
     C                   EVAL      FCCQ_FTCL(idx1) = wk_IWFTCL
     C                   EVAL      FCCQ_WGT(idx1) = wk_LineWgt
     C                   EVAL      wk_FCCQWgtx(idx1) = wk_LineWgt
     C                   IF        idx1 = 1
     C                   EVAL      FCCQ_PLLT(idx1) = wk_PlltCnt
     C                   EVAL      wk_PlltCnt = *Zero
     C                   ELSE
     C                   EVAL      FCCQ_PLLT(idx1) = *Zero
     C                   ENDIF                                                  idx1=1
     C                   ELSE
     C                   EVAL      FCCQ_WGT(idx2) = FCCQ_WGT(idx2) + wk_LineWgt
     C                   EVAL      wk_FCCQWgtx(idx2) =
     C                               wk_FCCQWgtx(idx2) + wk_LineWgt
     C                   ENDIF                                                  idx2=*Zero

     C                   ENDDO                                                  %eof(HYLSGDT1)

     C                   EVAL(H)   FCCQ_WGT = wk_FCCQWgtx

     C                   IF        idx1 = 1
      /Free
       wk_CtnCnt = *Zero;
       wk_CtnCntP = *Zero;
       EXEC SQL select count(distinct b.S1SRL#) into :wk_CtnCnt
                from HYPSGDT a join HYPSCWK b
                 on a.GDORD#=b.S1ORD# and a.GDTURN=b.S1TURN
                  and a.GDSSCC=b.S1SRL#
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDTURN=:GDTURN and a.GDCNVY<>'P';
       EXEC SQL select dec(coalesce(sum(a.GDQSTC/case when b.IWPCPC<>0 then
                 dec(b.IWPCPC,5,0) else 1 end),0),9,2) into :wk_CtnCntP
                from HYPSGDT a join HDIWHS b on a.GDITEM=b.IWITEM and b.IWWHS=6
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDTURN=:GDTURN and a.GDCNVY='P';
      /End-Free
     C                   EVAL      FCCQ_CSQT(1) = wk_CtnCnt + wk_CtnCntP
     C                   ELSE
     C                   FOR       idx2 = 1 to idx1
     C                   EVAL      wk_FTCL = FCCQ_FTCL(idx2)
      /Free
       wk_CtnCnt = *Zero;
       wk_CtnCntP = *Zero;
       EXEC SQL select count(distinct b.S1SRL#) into :wk_CtnCnt
                from HYPSGDT a join HYPSCWK b
                 on a.GDORD#=b.S1ORD# and a.GDORL#=b.S1ORL# and
                    a.GDBLN#=b.S1BLN# and a.GDTURN=b.S1TURN
                    and a.GDSSCC=b.S1SRL#
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDTURN=:GDTURN and a.GDCNVY<>'P' and exists
                 (select * from HDIWHS c where c.IWITEM=a.GDITEM and
                 c.IWFTCL=:wk_FTCL);
       EXEC SQL select dec(coalesce(sum(a.GDQSTC/case when b.IWPCPC<>0 then
                 dec(b.IWPCPC,5,0) else 1 end),0),9,2) into :wk_CtnCntP
                from HYPSGDT a join HDIWHS b on a.GDITEM=b.IWITEM and b.IWWHS=6
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDTURN=:GDTURN and a.GDCNVY='P' and b.IWFTCL=:wk_FTCL;
      /End-Free
     C                   EVAL      FCCQ_CSQT(idx2) = wk_CtnCnt + wk_CtnCntP
     C                   ENDFOR                                                 idx2=1 to idx1
     C                   ENDIF                                                  idx1=1

      *
      * Buld shipment-level FCCQ summary array.
      *
     C                   FOR       idx2 = 1 to idx1
     C                   EVAL      idx4 =
     C                               %lookup(FCCQ_FTCL(idx2):FCCQ_FTCL2:1:idx3)
     C                   IF        idx4 = *Zero
     C                   EVAL      idx3 = idx3 + 1
     C                   EVAL      FCCQ_FTCL2(idx3) = FCCQ_FTCL(idx2)
     C                   EVAL      FCCQ_CSQT2(idx3) = FCCQ_CSQT(idx2)
     C                   EVAL      FCCQ_WGT2(idx3) = FCCQ_WGT(idx2)
     C                   EVAL      FCCQ_PLLT2(idx3) = FCCQ_PLLT(idx2)
     C                   ELSE
     C                   EVAL      FCCQ_CSQT2(idx4) =
     C                               FCCQ_CSQT2(idx4) + FCCQ_CSQT(idx2)
     C                   EVAL      FCCQ_WGT2(idx4) =
     C                               FCCQ_WGT2(idx4) + FCCQ_WGT(idx2)
     C                   EVAL      FCCQ_PLLT2(idx4) =
     C                               FCCQ_PLLT2(idx4) + FCCQ_PLLT(idx2)
     C                   ENDIF                                                  idx4=*Zero
     C                   ENDFOR                                                 idx2=1 to idx1

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve output queue.                                                 *
      **************************************************************************
     CSR   $RtvOutQ      BEGSR

     C                   IF        not %open(HYPWSOQ)
     C                   OPEN      HYPWSOQ
     C                   ENDIF                                                  not %open(HYPWSOQ)

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
      * Send status message.                                                   *
      **************************************************************************
     CSR   $SndSM        BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*STATUS' :
     C                               '*EXT' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Write joblog message - *DIAG                                           *
      **************************************************************************
     CSR   $WrtJobLog1   BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*DIAG' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Write joblog message - *INFO                                           *
      **************************************************************************
     CSR   $WrtJobLog2   BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     P CompCustShip    E

     P/EJECT
      **************************************************************************
      * Retrieve whether customer is foreign.                                  *
      **************************************************************************
     P RtvForeign      B                   EXPORT
     D                 PI             1N
     D CustClass                      2A   CONST
     D STCtry                         3A   CONST

     D wk_CustClass    S              2A
     D wk_STCtry       S              3A
     D wk_Foreign      S              1N
     D wk_TDKEY1       S             15A
     D wk_TDFF02       S              1A

     C                   DO

     C                   EVAL      wk_CustClass = CustClass
     C                   EVAL      wk_STCtry = STCtry
     C                   EVAL      wk_Foreign = *OFF

     C                   IF        wk_STCtry <> 'US'  and
     C                               wk_STCtry <> *Blanks
     C                   EVAL      wk_Foreign = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_STCtry<>'US' and

      /Free
       IF wk_CustClass = 'RF';
         wk_Foreign = *ON;
       ELSE;
         wk_TDKEY1 = *Blanks;
         wk_TDFF02 = *Blank;
         EXEC SQL select TDKEY1, TDFF02 into :wk_TDKEY1, :wk_TDFF02
                  from GUPTDAT
                  where TDTABL='CUSTCLASS' and TDKEY1=:wk_CustClass;
         IF wk_TDKEY1 = wk_CustClass  and  wk_TDFF02 = 'Y';
           wk_Foreign = *ON;
         ENDIF;                                                              // wk_TDKEY1=wk_CustCla
       ENDIF;                                                                // wk_CMCCLS='RF'
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_Foreign

     P RtvForeign      E

     P/EJECT
      **************************************************************************
      * Determine whether an invoice print is required.                        *
      **************************************************************************
     P InvcReq         B
     D                 PI             1N
     D Order#                         8S 0 CONST
     D CustTerms                      2A   CONST
     D Foreign                        1N   CONST
     D ShipAmt                       11P 2 CONST
     D FrtAmtO                        7P 2 CONST
     D Loc#                           3S 0 CONST

     D wk_Order#       S                   LIKE(GDORD#)
     D wk_CustTerms    S              2A
     D wk_Foreign      S              1N
     D wk_ShipAmt      S             11P 2
     D wk_InvcReq      S              1N
     D wk_FrtAmtO      S              7P 2
     D wk_Loc#         S              3S 0

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_CustTerms = CustTerms
     C                   EVAL      wk_Foreign = Foreign
     C                   EVAL      wk_ShipAmt = ShipAmt
     C                   EVAL      wk_FrtAmtO = FrtAmtO
     C                   EVAL      wk_Loc# = Loc#
     C                   EVAL      wk_InvcReq = *OFF

     C                   IF        ((wk_CustTerms = 'CC'  or
     C                               wk_CustTerms = 'EC'  or
     C                               wk_CustTerms = 'CP')  and
     C                               wk_Loc# <> 91  and
     C                               (wk_ShipAmt <> *Zero or
     C                                wk_FrtAmtO <> *Zero))  or
     C                             ((wk_CustTerms = 'CA'  or
     C                               wk_CustTerms = 'CH')  and
     C                               not wk_Foreign  and
     C                               (wk_ShipAmt <> *Zero or
     C                                wk_FrtAmtO <> *Zero))  or
     C                             (wk_CustTerms = 'CO')
     C                   EVAL      wk_InvcReq = *ON
     C                   ENDIF                                                  wk_CustTerms='CC' or

     C                   ENDDO

     C                   RETURN    wk_InvcReq

     P InvcReq         E

     P/EJECT
      **************************************************************************
      * Retrieve shipment amount.                                              *
      **************************************************************************
     P RtvShipAmt      B
     D                 PI            11P 2
     D OrdCtl#                        8S 0 CONST
     D Turn#                          9S 0 CONST
     D Order#                         8S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D VolDiscPct                     1P 0 CONST
     D FrtAmt                         7P 2 CONST
     D SpclChgAmt                     7P 2 CONST
     D TaxCode                        1A   CONST
     D TaxOvr                         1A   CONST
     D TaxState                       2A   CONST
     D TaxCntyCode                    3S 0 CONST
     D TaxCityCode                    5S 0 CONST
     D TaxLoc1Code                    4S 0 CONST
     D TaxLoc2Code                    4S 0 CONST
     D TaxLoc3Code                    4S 0 CONST
     D HDR1RL                         5P 1 CONST
     D InvcReq                        1N   CONST

     D wk_OrdCtl#      S              8S 0
     D wk_Turn#        S              9S 0
     D wk_Order#       S              8S 0
     D wk_ShipTo#      S              7S 0
     D wk_DropShip#    S              7S 0
     D wk_VolDiscPct   S              1P 0
     D wk_FrtAmt       S              7P 2
     D wk_SpclChgAmt   S              7P 2
     D wk_TaxCode      S              1A
     D wk_TaxOvr       S              1A
     D wk_TaxState     S              2A
     D wk_TaxCntyCode  S              3S 0
     D wk_TaxCityCode  S              5S 0
     D wk_TaxLoc1Code  S              4S 0
     D wk_TaxLoc2Code  S              4S 0
     D wk_TaxLoc3Code  S              4S 0
     D wk_HDR1RL       S              5P 1
     D wk_InvcReq      S              1N
     D wk_ShipAmt      S             11P 2
     D wk_VolDiscPct2  S              3P 2
     D wk_VolDiscAmt   S              9P 2
     D wk_TaxSHTO      S              7S 0
     D wk_TaxDS        S              1A
     D wk_TWSTTA       S              9P 2
     D wk_TWCNTA       S              9P 2
     D wk_TWCTTA       S              9P 2
     D wk_TWL1TA       S              9P 2
     D wk_TWL2TA       S              9P 2
     D wk_TWL3TA       S              9P 2
     D wk_SlsTaxAmt    S              9P 2
     D wk_DumpID       S              8A

     C                   DO

     C                   EVAL      wk_OrdCtl# = OrdCtl#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_VolDiscPct = VolDiscPct
     C                   EVAL      wk_FrtAmt = FrtAmt
     C                   EVAL      wk_SpclChgAmt = SpclChgAmt
     C                   EVAL      wk_TaxCode = TaxCode
     C                   EVAL      wk_TaxOvr = TaxOvr
     C                   EVAL      wk_TaxState = TaxState
     C                   EVAL      wk_TaxCntyCode = TaxCntyCode
     C                   EVAL      wk_TaxCityCode = TaxCityCode
     C                   EVAL      wk_TaxLoc1Code = TaxLoc1Code
     C                   EVAL      wk_TaxLoc2Code = TaxLoc2Code
     C                   EVAL      wk_TaxLoc3Code = TaxLoc3Code
     C                   EVAL      wk_HDR1RL = HDR1RL
     C                   EVAL      wk_InvcReq = InvcReq
     C                   EVAL      wk_ShipAmt = *Zero

      /Free
       EXEC SQL select sum(round(O1QSTC * O1SLPR,2)) into :wk_ShipAmt
                from OEDTWK
                where O1OCTL=:wk_OrdCtl#;
      /End-Free

     C                   IF        wk_ShipAmt <> *Zero  and
     C                               wk_VolDiscPct > *Zero
     C                   EVAL      wk_VolDiscPct2 = wk_VolDiscPct / 100
     C                   EVAL(H)     wk_VolDiscAmt = wk_ShipAmt *
     C                                 wk_VolDiscPct2
     C                   ELSE
     C                   EVAL      wk_VolDiscAmt = *Zero
     C                   ENDIF                                                  wk_ShipAmt<>*Zero

     C                   IF        wk_InvcReq

     C                   EXSR      $DltTaxRcds

     C                   IF        (wk_ShipAmt <> *Zero or wk_FrtAmt <> *Zero)
     C                             and wk_TaxOvr <> 'Y'
     C                   EXSR      $WrtTaxRcds
     C                   EXSR      $CalcRtvTaxes
     C                   ELSE
     C                   EVAL      wk_SlsTaxAmt = *Zero
     C                   ENDIF                                                  wk_ShipAmt<>*Zero

     C                   ELSE

     C                   EXSR      $RtvH1Taxes

     C                   ENDIF                                                  wk_InvcReq

     C                   EVAL      wk_ShipAmt = wk_ShipAmt - wk_VolDiscAmt +
     C                               wk_SlsTaxAmt

     C                   ENDDO

     C                   RETURN    wk_ShipAmt

     C/EJECT
     C**************************************************************************
     C* Delete any prior sales tax work file records.                          *
     C**************************************************************************
     CSR   $DltTaxRcds   BEGSR                                                  aka SRDTAX

      /Free
       EXEC SQL delete from OETXWK where TWOCTL=:wk_OrdCtl#;
      /End-Free

     CSR                 ENDSR

     C/SPACE 3
     C**************************************************************************
     C* Write sales tax work file records.                                     *
     C**************************************************************************
     CSR   $WrtTaxRcds   BEGSR                                                  aka SRCTAX

     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      wk_TaxSHTO = wk_ShipTo#
     C                   EVAL      wk_TaxDS = 'N'
     C                   ELSE
     C                   EVAL      wk_TaxSHTO = wk_DropShip#
     C                   EVAL      wk_TaxDS = 'Y'
     C                   ENDIF                                                  OEDSHP=*Zero

      /Free
       IF wk_HDR1RL<>*Zero;
         EXEC SQL insert into OETXWK
                   select :wk_OrdCtl#, :wk_Turn#, :wk_Order#, O1ORL#,
                          :wk_TaxSHTO, :wk_TaxDS, :wk_TaxState,
                          :wk_TaxCntyCode, :wk_TaxCityCode,
                          :wk_TaxLoc1Code, :wk_TaxLoc2Code,
                          :wk_TaxLoc3Code, :wk_TaxCode,
                          case when :wk_TaxCode<>'T' then
                            round(O1QSTC * O1SLPR,2) else 0 end,
                          case when :wk_TaxCode='T' then
                            round(O1QSTC * O1SLPR,2) else 0 end,
                          case when :wk_TaxCode='T' then
                            round(O1QSTC * O1SLPR,2) else 0 end,
                          0, ' ', 0, 0, 0, 0, 0, 0, 0, ' ', ' ', ' '
                   from OEDTWK
                   where O1OCTL=:wk_OrdCtl# and O1BLKT<>' ' and
                         O1ORST<>'C'
                   order by O1OCTL, O1ORL#, O1BLN#;
       ELSE;
         EXEC SQL insert into OETXWK
                   select :wk_OrdCtl#, :wk_Turn#, :wk_Order#, 0,
                          :wk_TaxSHTO, :wk_TaxDS, :wk_TaxState,
                          :wk_TaxCntyCode, :wk_TaxCityCode,
                          :wk_TaxLoc1Code, :wk_TaxLoc2Code,
                          :wk_TaxLoc3Code, :wk_TaxCode,
                          sum(case when :wk_TaxCode<>'T' then
                            round(O1QSTC * O1SLPR,2) else 0 end),
                          sum(case when :wk_TaxCode='T' then
                            round(O1QSTC * O1SLPR,2) else 0 end),
                          sum(case when :wk_TaxCode='T' then
                            round(O1QSTC * O1SLPR,2) else 0 end),
                          0, ' ', 0, 0, 0, 0, 0, 0, 0, ' ', ' ', ' '
                   from OEDTWK
                   where O1OCTL=:wk_OrdCtl# and O1BLKT<>' ' and
                         O1ORST<>'C';
       ENDIF;                                                                // wk_HDR1RL<>*Zero
      /End-Free

     C                   ENDSR

     C/EJECT
     C**************************************************************************
     C* Calculate and retrieve sales tax amounts.                              *
     C**************************************************************************
     C     $CalcRtvTaxes BEGSR                                                  aka SRRTAX

     C                   IF        wk_HDR1RL <> *Zero
     C                   CALLP(E)  HOETXI (wk_OrdCtl#:wk_Turn#:wk_Order#:
     C                               wk_TaxCode:wk_FrtAmt:wk_SpclChgAmt:'S')
     C                   IF        %error
     C                   EVAL      wk_DumpID = 'DUMP-001'
     C                   DUMP(A)
     C                   ENDIF
     C                   ELSE
     C                   CALLP(E)  HOETAX (wk_OrdCtl#:wk_Turn#:wk_Order#:
     C                               wk_ShipAmt:wk_FrtAmt)
     C                   IF        %error
     C                   EVAL      wk_DumpID = 'DUMP-002'
     C                   DUMP(A)
     C                   ENDIF
     C                   ENDIF                                                  wk_HDR1RL<>*Zero

      /Free
       wk_TWSTTA = *Zero;
       wk_TWCNTA = *Zero;
       wk_TWCTTA = *Zero;
       wk_TWL1TA = *Zero;
       wk_TWL2TA = *Zero;
       wk_TWL3TA = *Zero;
       EXEC SQL select coalesce(sum(TWSTTA),0), coalesce(sum(TWCNTA),0),
                       coalesce(sum(TWCTTA),0), coalesce(sum(TWL1TA),0),
                       coalesce(sum(TWL2TA),0), coalesce(sum(TWL3TA),0)
                 into :wk_TWSTTA, :wk_TWCNTA,
                      :wk_TWCTTA, :wk_TWL1TA,
                      :wk_TWL2TA, :wk_TWL3TA
                from OETXWK
                where TWOCTL=:wk_OrdCtl#;
      /End-Free

     C                   EVAL      wk_SlsTaxAmt = wk_TWSTTA + wk_TWCNTA +
     C                               wk_TWCTTA + wk_TWL1TA + wk_TWL2TA +
     C                               wk_TWL3TA

     C                   ENDSR

     C/EJECT
     C**************************************************************************
     C* Retrieve existing header record sales tax amounts.                     *
     C**************************************************************************
     C     $RtvH1Taxes   BEGSR

      /Free
       wk_SlsTaxAmt = *Zero;
       EXEC SQL select coalesce(H1STTA+H1CNTA+H1CTTA+H1LC1A+H1LC2A+
                        +H1LC3A,0)
                 into :wk_SlsTaxAmt
                from OEHDWK
                where H1OCTL=:wk_OrdCtl#;
      /End-Free

     C                   ENDSR

     P RtvShipAmt      E

     P/EJECT
      **************************************************************************
      * Retrieve customer shipment intransit hold status.  If any of the       *
      * shipment's orders or turns are on intransit hold, the shipment is on   *
      * intransit hold for these purposes.                                     *
      **************************************************************************
     P RtvIntrHoldCS   B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDBLTO)
     D wk_DropShip#    S                   LIKE(GDBLTO)
     D wk_IntrHoldCS   S              1N
     D wk_IntrHold     S              1N
     D sv_TURN         S              9S 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_IntrHoldCS = *OFF

     C                   EVAL      kf_ShipGrp# = wk_ShipGrp#
     C                   EVAL      kf_BillTo# = wk_BillTo#
     C                   EVAL      kf_ShipTo# = wk_ShipTo#
     C                   EVAL      kf_DropShip# = wk_DropShip#

     C                   EVAL      sv_TURN = -1
     C     key_GD3       SETLL     HYLSGDT3
     C                   DOU       %eof(HYLSGDT3)
     C     key_GD3       READE     HYLSGDT3
     C                   IF        %eof(HYLSGDT3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT3)
     C                   IF        GDTURN = sv_TURN
     C                   ITER
     C                   ENDIF                                                  GDTURN=sv_TURN
     C                   EVAL      sv_TURN = GDTURN

     C                   EVAL      wk_IntrHold = RtvIntrHold (GDORD#:GDTURN)
     C                   IF        wk_IntrHold
     C                   EVAL      wk_IntrHoldCS = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_IntrHold

     C                   ENDDO                                                  %eof(HYLSGDT3)

     C                   ENDDO

     C                   RETURN    wk_IntrHoldCS

     P RtvIntrHoldCS   E

     P/EJECT
      **************************************************************************
      * Retrieve intransit hold status.                                        *
      **************************************************************************
     P RtvIntrHold     B
     D                 PI             1N
     D Order#                         8S 0 CONST
     D Turn#                          9S 0 CONST

     D wk_Order#       S                   LIKE(GDORD#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_IntrHold     S              1N
     D wk_RcdCnt1      S              7P 0
     D wk_RcdCnt2      S              7P 0

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_IntrHold = *OFF

      /Free
       wk_RcdCnt1 = *Zero;
       wk_RcdCnt2 = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt1
                from HYPINTR
                where IOORD#=:wk_Order# and IOTURN=:wk_Turn# and
                      IOAIV#=0;
       IF wk_RcdCnt1 = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt2
                  from HYPINTR
                  where IOORD#=:wk_Order# and IOTURN=0 and
                        IOAIV#=0 and IORLSU=' ';
       ELSE;
         EXEC SQL select count(*) into :wk_RcdCnt2
                  from HYPINTR
                  where IOORD#=:wk_Order# and IOTURN=:wk_Turn# and
                        IOAIV#=0 and IORLSU=' ';
       ENDIF;
      /End-Free

     C                   IF        wk_RcdCnt2 > *Zero
     C                   EVAL      wk_IntrHold = *ON
     C                   ENDIF                                                  wk_RcdCnt2>*Zero

     C                   ENDDO

     C                   RETURN    wk_IntrHold

     P RtvIntrHold     E

     P/EJECT
      **************************************************************************
      * Retrieve whether to call Varsity interface.                            *
      **************************************************************************
     P CallVarsity     B
     D                 PI             1N
     D ShipVia                        2A   CONST
     D Intrhold                       1N   CONST

     D wk_ShipVia      S              2A
     D wk_IntrHold     S              1N
     D wk_CallVarsity  S              1N
     D wk_TDKEY1       S             15A
     D wk_TDFF02       S              1A

     C                   DO

     C                   EVAL      wk_ShipVia = ShipVia
     C                   EVAL      wk_IntrHold = IntrHold
     C                   EVAL      wk_CallVarsity = *OFF

     C                   IF        wk_IntrHold
     C                   LEAVE
     C                   ENDIF                                                  wk_IntrHold

      /Free
       wk_TDKEY1 = *Blanks;
       wk_TDFF02 = *Blank;
       EXEC SQL select TDKEY1, TDFF02 into :wk_TDKEY1, :wk_TDFF02
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_ShipVia;
      /End-Free
     C                   IF        wk_TDKEY1 = wk_ShipVia  and  wk_TDFF02 = 'Y'
     C                   EVAL      wk_CallVarsity = *ON
     C                   ENDIF                                                  wk_TDKEY1=wk_ShipVia

     C                   ENDDO

     C                   RETURN    wk_CallVarsity

     P CallVarsity     E

     P/EJECT
      **************************************************************************
      * Retrieve shipment type.                                                *
      **************************************************************************
     P ShipType        B
     D                 PI             1A
     D ShipVia                        2A   CONST

     D wk_ShipVia      S              2A
     D wk_ShipType     S              1A
     D wk_TDFF01       S              1A

     C                   DO

     C                   EVAL      wk_ShipVia = ShipVia
     C                   EVAL      wk_ShipType = *Blank

      /Free
       wk_TDFF01 = *Blank;
       EXEC SQL select TDFF01 into :wk_TDFF01
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_ShipVia;
      /End-Free

     C                   EVAL      wk_ShipType = wk_TDFF01

     C                   ENDDO

     C                   RETURN    wk_ShipType

     P ShipType        E

     P/EJECT
      **************************************************************************
      * Determine whether a BOL print is required.                             *
      **************************************************************************
     P BOLReq          B                   EXPORT
     D                 PI             1N
     D ShipVia                        2A   CONST
     D Foreign                        1N   CONST

     D wk_ShipVia      S              2A
     D wk_Foreign      S              1N
     D wk_BOLReq       S              1N
     D wk_SVBOLR       S              1A

     C                   DO

     C                   EVAL      wk_ShipVia = ShipVia
     C                   EVAL      wk_Foreign = Foreign
     C                   EVAL      wk_BOLReq = *OFF

     C***                IF        wk_Foreign
     C***                EVAL      wk_BOLReq = *ON
     C***                LEAVE
     C***                ENDIF                                                  wk_Foreign

      /Free
       wk_SVBOLR = *Blank;
       EXEC SQL select SVBOLR into :wk_SVBOLR
                from HDSHPV
                where SVSVSV=:wk_ShipVia;
      /End-Free

     C                   IF        wk_SVBOLR = 'Y'
     C                   EVAL      wk_BOLReq = *ON
     C                   ENDIF                                                  wk_SVBOLR='Y'

     C                   ENDDO

     C                   RETURN    wk_BOLReq

     P BOLReq          E

     P/EJECT
      **************************************************************************
      * Determine whether customer is an EDI 856 customer.                     *
      **************************************************************************
     P EDICust         B
     D                 PI             1N
     D ShipTo#                        7S 0 CONST

     D wk_ShipTo#      S              7S 0
     D wk_EDICust      S              1N
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_EDICust = *OFF

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from EDOTPM
                where EO0143='856' and EOSHTO=:wk_ShipTo#;
      /End-Free

     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_EDICust = *ON
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     C                   ENDDO

     C                   RETURN    wk_EDICust

     P EDICust         E

     P/EJECT
      **************************************************************************
      * Retrieve next bill of lading number.                                   *
      **************************************************************************
     P RtvNxtBOL#      B
     D                 PI             8S 0

     D wk_BOL#         S              8S 0
     D wk_RcdCnt       S              7P 0
     D RLCNT           S              1P 0

     C                   DO

     C                   EVAL      wk_BOL# = *Zero

     C                   IF        not %open(OECTRL)
     C                   OPEN      OECTRL
     C                   ENDIF                                                  not %open(OECTRL)

     C                   DOW       wk_BOL# = *Zero
     C                   DOU       not %error
     C     1             CHAIN(E)  OECTRL
     C                   IF        %error
     C                   CALLP(E)  CSYRLO ('OECTRL':RLCNT)
     C                   ENDIF                                                  %error
     C                   ENDDO                                                  not %error
     C                   EVAL      RLCNT = *Zero
     C                   IF        %found(OECTRL)
     C                   EVAL      CELBL# = CELBL# + 1
     C                   UPDATE    OECTRLR
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEBOLH
                where SDBOL#=:CELBL#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_BOL# = CELBL#
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  %found(OECTRL)
     C                   ENDDO                                                  wk_BOL#=*Zero

     C                   ENDDO

     C                   RETURN    wk_BOL#

     P RtvNxtBOL#      E

     P/EJECT
      **************************************************************************
      * Retrieve whether the current day is a normal shipping day.             *
      **************************************************************************
     P RtvShipDay      B
     D                 PI             1N

     D wk_ShipDay      S              1N
     D wk_CurCYMD      S              7S 0
     D wk_CDDYCD       S              1A
     D wk_CDWRKD       S              1A

     C                   DO

     C                   EVAL      wk_ShipDay = *ON

     C                   EVAL      wk_CurCYMD = %dec(%date:*CYMD)

      /Free
       wk_CDDYCD = *Blank;
       wk_CDWRKD = *Blank;
       EXEC SQL select CDDYCD, CDWRKD into :wk_CDDYCD, :wk_CDWRKD
                from HDCALD
                where CDCALN=1 and CDCYMD=:wk_CurCYMD;
       IF wk_CDWRKD = *Blank;
         wk_CDWRKD = 'Y';
       ENDIF;                                                                // wk_CDWRKD=*Blank;
      /End-Free

      *
      * Determine whether today is a ship day, i.e., not a Sunday or holiday.
      *
     C                   IF        wk_CDWRKD = 'Y'  or  wk_CDDYCD = 'A'         Work day or Saturday
     C                   EVAL      wk_ShipDay = *ON
     C                   ELSE
     C                   EVAL      wk_ShipDay = *OFF
     C                   ENDIF                                                  wk_CDWRKD='Y' or

     C                   ENDDO

     C                   RETURN    wk_ShipDay

     P RtvShipDay      E

     P/EJECT
      **************************************************************************
      * Retrieve next normal shipping day.                                     *
      **************************************************************************
     P RtvNxtShipDate  B
     D                 PI             7S 0

     D wk_NxtShipDate  S              7S 0
     D wk_CurCYMD      S              7S 0
     D wk_CDCYMD       S              7S 0

     C                   DO

     C                   EVAL      wk_NxtShipDate = *Zero

     C                   EVAL      wk_CurCYMD = %dec(%date:*CYMD)

      /Free
       wk_CDCYMD = *Zero;
       EXEC SQL select CDCYMD into :wk_CDCYMD
                from HDCALD
                where CDCALN=1 and CDCYMD>:wk_CurCYMD and CDWRKD='Y'
                order by CDCALN, CDCYMD
                fetch first row only;
       IF wk_CDCYMD = *Zero;
         wk_CDCYMD = wk_CurCYMD;
       ENDIF;                                                                // wk_CDCYMD=*Zero;
      /End-Free

     C                   EVAL      wk_NxtShipDate = wk_CDCYMD

     C                   ENDDO

     C                   RETURN    wk_NxtShipDate

     P RtvNxtShipDate  E

     C/EJECT
      **************************************************************************
      * Retrieve commodity description variable data.                          *
      **************************************************************************
     P RtvCDVar        B
     D                 PI            34A
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D InDesc                        34A   CONST
     D FrtCls                         5A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_ShipGrp#     S              9S 0
     D wk_BillTo#      S              7S 0
     D wk_ShipTo#      S              7S 0
     D wk_DropShip#    S              7S 0
     D wk_Desc         S             34A
     D wk_FrtCls       S              5A
     D wk_UOM          S              3A
     D wk_NEQ          S              7P 2
     D wk_Pos          S              3P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Desc = InDesc
     C                   EVAL      wk_FrtCls = FrtCls

     C                   EVAL      wk_Pos = %scan('@NEW':wk_Desc)
     C                   IF        wk_Pos > *Zero
      /Free
       wk_NEQ = *Zero;
       SELECT;
       WHEN (wk_DatabaseID = 'PV'  or  wk_DatabaseID = 'PX')  and
            (wk_FrtCls = 'PRIMB'  or  wk_FrtCls = 'PRIMR');
        EXEC SQL Select sum(b.IMUDN5*a.GDQSTC)
                   into :wk_NEQ
                 From HYPSGDT a Join HDIMST b on a.GDITEM=b.IMITEM
                 Where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                   a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                   exists (select * from HDIWHS c where c.IWITEM=a.GDITEM and
                   c.IWFTCL=:wk_FrtCls);
        wk_UOM = ' LB';
       OTHER;
        EXEC SQL Select sum(b.EDPWGT)
                   into :wk_NEQ
                 From HYPSGDT a Join HYPELD b on a.GDORD#=b.EDORD# and
                  a.GDORL#=b.EDORL# and a.GDBLN#=b.EDBLN# and a.GDSSCC=b.EDSRL#
                 Where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                   a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                   exists (select * from HDIWHS c where c.IWITEM=a.GDITEM and
                   c.IWFTCL=:wk_FrtCls);
        wk_UOM = 'kg';
       ENDSL;
      /End-Free
     C                   EVAL      wk_Desc =
     C                             %replace(%triml(%editc(wk_NEQ:'3'))+
     C                             %trimr(wk_UOM):wk_Desc:wk_Pos:4)
     C                   ENDIF                                                  wk_Pos>*Zero

     C                   ENDDO

     C                   RETURN    wk_Desc

     P RtvCDVar        E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR0614      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0
     D wk_RC2022       S              2A
     D wk_RC6080       S              2A
     D wk_SSCCProc     S              5P 0

     D ShutDown        C                   '9'

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(HYLSGDT1)
     C                   CLOSE     HYLSGDT1
     C                   ENDIF                                                  %open(HYLSGDT1)

     C                   IF        %open(HYLSGDT3)
     C                   CLOSE     HYLSGDT3
     C                   ENDIF                                                  %open(HYLSGDT3)

     C                   IF        %open(HYPOREL)
     C                   CLOSE     HYPOREL
     C                   ENDIF                                                  %open(HYPOREL)

     C                   IF        %open(HYLOREL1)
     C                   CLOSE     HYLOREL1
     C                   ENDIF                                                  %open(HYLOREL1)

     C                   IF        %open(HYPINTR)
     C                   CLOSE     HYPINTR
     C                   ENDIF                                                  %open(HYPINTR)

     C                   IF        %open(HYLPLLT4)
     C                   CLOSE     HYLPLLT4
     C                   ENDIF                                                  %open(HYLPLLT4)

     C                   IF        %open(HYPWSOQ)
     C                   CLOSE     HYPWSOQ
     C                   ENDIF                                                  %open(HYPWSOQ)

     C                   IF        %open(OECTRL)
     C                   CLOSE     OECTRL
     C                   ENDIF                                                  %open(OECTRL)

     C                   IF        wk_Pgm0139A
     C                   CALLP(E)  sd_HYR0139A ()
     C                   ENDIF                                                  wk_Pgm0139A

     C                   IF        wk_Pgm0148
     C                   CALLP(E)  HYR0148 (*Zero:*Zero:*Zero:*ON)
     C                   ENDIF                                                  wk_Pgm0148

     C                   IF        wk_Pgm0150
     C                   CALLP(E)  HYR0150 (ShutDown:*Zero:*Zero:*Zero:*Zero:
     C                              *Zero:*Zero:*Blanks:*Blank:*Zero:*Zero)
     C                   EVAL      wk_Pgm0150 = *OFF
     C                   ENDIF                                                  wk_Pgm0150

     C                   IF        wk_Pgm0152
     C                   CALLP(E)  HYR0152 (ShutDown:*Zero:*Zero:*Zero:
     C                              *Zero:*Zero:*Zero:*Blanks:*Zero)
     C                   EVAL      wk_Pgm0152 = *OFF
     C                   ENDIF                                                  wk_Pgm0152

     C                   IF        wk_Pgm0240
     C                   CALLP(E)  HYR0240 (*Zero:*Zero:*Blanks:*ON)
     C                   ENDIF                                                  wk_Pgm0240

     C                   IF        wk_Pgm0540
     C                   CALLP(E)  HYR0540 (-1:*Zero:*Zero:wk_SSCCProc)
     C                   ENDIF                                                  wk_Pgm0540

     C                   IF        wk_Pgm0810
     C                   CALLP(E)  sd_HYR0810 ()
     C                   ENDIF                                                  wk_Pgm0810

     C                   IF        wk_Pgm0812
     C                   CALLP(E)  sd_HYR0812 ()
     C                   ENDIF                                                  wk_Pgm0812

     C                   IF        wk_Pgm2022
     C                   CALLP(E)  HYR2022 (*Zero:*Zero:*Blanks:*Blanks:*ON:
     C                               wk_RC2022)
     C                   ENDIF                                                  wk_Pgm2022

     C                   IF        wk_Pgm6080                                   HYR6080 called
     C                   CALLP(E)  HYR6080 (*Zero:*Zero:*Blanks:*Blank:
     C                               *Blanks:*ON:wk_RC6080)
     C                   ENDIF                                                  wk_Pgm6080

     C                   IF        wk_Pgm9906
     C                   CALLP(E)  sd_HYR9906 ()
     C                   ENDIF                                                  wk_Pgm9906

     C                   IF        wk_Pgm9928
     C                   CALLP(E)  sd_HYR9928 ()
     C                   ENDIF                                                  wk_Pgm9928

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR0614      E

     P/EJECT
