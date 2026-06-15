     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2018')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0602')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Shipment Detail Processing                                *
      *          BY: William Earl (MCA)                                        *
      *        DATE: September 2018                                            *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  02/05/2020  MCA  Display AMMOB items in yellow.                       *
      *  03/21/2020  MCA  Expand record sequence from 3,0 to 5,0.              *
      *  04/08/2020  MCA  Mixed label must have mixed items.                   *
      *  04/08/2020  MCA  Partial labels quantity and UOM was not populated.   *
      *  08/24/2020  MCA  Don't allow ship of orders linked to vendor-direct   *
      *                   POs.  This could only happen if the link had been    *
      *                   broken long enough to create a BoxLogix shipment     *
      *                   group (because SMS excludes linked orders), and the  *
      *                   link was fixed prior to shipment.                    *
      *  09/02/2020  MCA  Blem carton conveyable processing - require lot      *
      *                   entry.                                               *
      *  03/16/2022  MCA  Tote verify.                                         *
      *  08/21/2022  MCA  Handle small-parcel picks (which could span multiple *
      *                   shipment groups and/or customer shipments for a      *
      *                   single tote).                                        *
      *  09/03/2022  MCA  Add 2nd "display" view, so that picking-related      *
      *                   fields can be shown.                                 *
      *  09/03/2022  MCA  For subsetting purposes, treat toteable small-parcel *
      *                   picks as tote picks.                                 *
      *  10/17/2022  MCA  Don't allow option 7=Lot Entry if Tote Verify is     *
      *                   enabled.  This is to prevent double shipment         *
      *                   quantities updated.                                  *
      *  10/19/2022  MCA  Change F7=Tote proc to F7=Tote vfy.  Add tote ID to  *
      *                   subset screen.  NOTE: Purposefully left unreferenced *
      *                   subroutine $ToteProc (which was supplanted by        *
      *                   $ToteVfy2) and wk_ToteMode, in case we want to       *
      *                   reinstate by other means (e.g., if the tote ID is    *
      *                   the only subsetted field),                           *
      *  10/20/2022  MCA  If Tote Verify is not enabled, use F7 will go to     *
      *                   $ToteProc instead of $ToteVfy2.                      *
      *  03/22/2023  MCA  Remove all references to BARDATA1 (used for the      *
      *                   BARDATA2.0 conversion).                              *
      *  11/27/2023  PRF  Handle Digi-Pick entries                             *
      *  12/02/2023  MCA  Change subset status filtering.  Prior logic         *
      *                   included the selected status plus any "lesser"       *
      *                   statuses.  Changed to include ONLY the selected      *
      *                   status.  Also added "Not yet picked" status.  This   *
      *                   had prior been added for NOT tote-verify mode, but   *
      *                   was missed for tote-verify mode.                     *
      *  02/27/2024  MCA  Use HYR0626 to generate UCC-128 label when no        *
      *                   selections are made from this program's subfile.     *
      *                   This was needed when there were more pick/ship lines *
      *                   than could be shown on one subfile page              *
      *                   (specifically for some DigiPick Tools shipments).    *
      *  03/27/2024  MCA  Fix bug in original DigiPick programming. Subfile    *
      *                   updates reversed image (or not) pick ticket quantity *
      *                   based on the last subfile record written.            *
      *  07/03/2024  MCA  Don't allow "ship" option if label has already been  *
      *                   printed.                                             *
      *  09/04/2024  MCA  Check for recalled lots for displayed/filtered       *
      *                   pick-line status.                                    *
      *  11/15/2024  MCA  Fix bug - don't allow Tote Verify option for pallet  *
      *                   picks.                                               *
      *  11/18/2024  MCA  When verifying totes, totes that have already been   *
      *                   closed for the shipment will need to be re-opened    *
      *                   via F16 from the prompt window.  Note: F16 does not  *
      *                   actually re-open the tote.  It just allows the tote  *
      *                   to be re-verified.                                   *
      *  11/21/2024  MCA  Handle wk_PrevLbld SQL null retrieval (no SQL0305).  *
      *  11/25/2024  MCA  Handle wk_LDQSTC SQL null retrieval (no SQL0305).    *
      *  11/25/2024  MCA  Fix bug - in $ProcSelects, when retrieving GDDVRT,   *
      *                   include RSEQ on select criteria.  This should also   *
      *                   stop SQL0811 from being shown in the job log.        *
      *  05/06/2025  MCA  Fix paging bug in $Bottom1 and $PageUpSf1.           *
      *  06/19/2025  MCA  Fix bug - in $WrtHYPSCWK, when retrieving HYPSGDT    *
      *                   fields for full-carton labels, include RSEQ on       *
      *                   select criteria.  This should also prevent SQL0811   *
      *                   from being shown in the job log.                     *
      *  08/29/2025  MCA  Display "DigiPick" for GDCNVY='D'.                   *
      **************************************************************************

     FHYD0602A  CF   E             WORKSTN
     F                                     SFILE(SFL01A:wk_Sfl01RRN)
     F                                     SFILE(SFL02A:wk_Sfl02RRN)
     FHYD0602B  CF   E             WORKSTN
     FHYLSGDT3  IF   E           K DISK    RENAME(HYRSGDT:HYRSGDT3)             by Turn#
     FHYLSGDT4  IF   E           K DISK    RENAME(HYRSGDT:HYRSGDT4) USROPN      by Order#
     FHYLSGDT5  IF   E           K DISK    RENAME(HYRSGDT:HYRSGDT5) USROPN      by Item#
     FHYPSGDT   UF   E           K DISK    USROPN
     FHYPSGLD   UF   E           K DISK    USROPN
     FHYPSCWK   O    E           K DISK    BLOCK(*NO) PREFIX(SC:2) USROPN
     FHYW0602   IF   E           K DISK    USROPN

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0602         PR                  EXTPGM('HYR0602')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  RtnCode                       2A

     D HYR0604         PR                  EXTPGM('HYR0604')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  SSCCBC                       20A   CONST
     D  RcdSeq#                       5S 0 CONST
     D  Data15                       15A
     D  RtnCode                       2A

     D HYR0606         PR                  EXTPGM('HYR0606')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  RtnCode                       2A

     D HYR0608         PR                  EXTPGM('HYR0608')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Tote                         10A   CONST
     D  RtnCode                       2A

     D HYR0626         PR                  EXTPGM('HYR0626')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  LblType                       1A   CONST                                P=Partial, M=Mixed
     D  RtnCode                       2A

      /COPY QPRPSRC,HYR0622

      /COPY QPRPSRC,HYR9930

      /COPY QPRPSRC,CHKDIGIT

     D SelOK           PR             2A                                        Check if select ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  SSCCBC                       20A   CONST
     D  Order#                        8S 0 CONST
     D  Item#                        15A   CONST
     D  LblType                       1A   CONST

     D ChgOK           PR             2A                                        Check if change ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  SSCCBC                       20A   CONST

     D LotEntryOK      PR             2A                                        Check if lotentry ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST
     D  SSCC                          9S 0 CONST

     D ShipOK          PR             2A                                        Check if ship ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST

     D LblOK           PR             2A                                        Check if lbl prt ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST
     D  SSCC                          9S 0 CONST

     D ToteVfyOK       PR             2A                                        Check if tote vfy ok
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Tote                         10A   CONST

     D DltLblOK        PR             2A                                        Check if dlt lbl ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  SSCCBC                       20A   CONST

     D LotsNeeded      PR             1N                                        Check if lots needed
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST
     D  SSCC                          9S 0 CONST

     D LotsRecalled    PR             1N                                        Check lots recalled
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST

     D LblsNeeded      PR             1N                                        Check if lbls needed
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  OrdCtl#                       8S 0 CONST
     D  Line#                         3S 0 CONST
     D  Blanket#                      3S 0 CONST
     D  Lbl1                          1A   CONST
     D  Lbl2                          1A   CONST
     D  Lbl3                          1A   CONST

     D TVNeeded        PR             1N                                        Check Tote Vfy neede
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST

     D PltzNeeded      PR             1N
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Order#                        8S 0 CONST
     D  Line#                         3S 0 CONST
     D  Blanket#                      3S 0 CONST
     D  SSCC                          9S 0 CONST

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
     D HYR0602         PI
     D  in_PgmMode                    1A   CONST
     D  in_ShipGrp#                   9S 0 CONST
     D  in_BillTo#                    7S 0 CONST
     D  in_ShipTo#                    7S 0 CONST
     D  in_DropShip#                  7S 0 CONST
     D  out_RtnCode                   2A

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
     D wk_SSCCSrlRef   S              9P 0 DTAARA(SSCCSRLREF)                   SSCC serial no.
     D wk_LDA          S           1024A   DTAARA(*LDA)                         *LDA

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PgmMode      S              1A
     D wk_ShipGrp#     S              9S 0
     D wk_BillTo#      S              7S 0
     D wk_ShipTo#      S              7S 0
     D wk_DropShip#    S              7S 0
     D wk_RtnCode      S              2A
     D wk_SuperUser    S              1N   INZ(*OFF)
     D wk_EMPL         S              5S 0
     D wk_ToteVfy      S              1N   INZ(*OFF)
     D wk_StsTVNeed    S              1N
     D wk_StsLotsNeed  S              1N
     D wk_StsLblsNeed  S              1N
     D wk_StsPltzNeed  S              1N
     D wk_GCSTNM       S             30A
     D wk_GCSTYP       S              1A
     D wk_ToteMode     S              1N   INZ(*OFF)
     D wk_Include      S              1N   INZ(*OFF)
     D wk_PgUpFrmBot1  S              1N   INZ(*OFF)
     D wk_IN12         S                   LIKE(*IN12)
     D wk_IN25         S                   LIKE(*IN25)
     D wk_Sfl01RRN     S              5P 0
     D wk_Sfl01PC      S              5P 0
     D wk_Sfl01PS      S              3P 0 INZ(12)
     D wk_Sfl02RRN     S              5P 0
     D wk_SelPr        S              1N   INZ(*OFF)
     D wk_ChgPr        S              1N   INZ(*OFF)
     D wk_DspPr        S              1N   INZ(*OFF)
     D wk_LotEntryPr   S              1N   INZ(*OFF)
     D wk_ShipPr       S              1N   INZ(*OFF)
     D wk_LblPr        S              1N   INZ(*OFF)
     D wk_TVPr         S              1N   INZ(*OFF)
     D wk_LotInqPr     S              1N   INZ(*OFF)
     D wk_DltLblPr     S              1N   INZ(*OFF)
     D wk_OptPr        S              1N   INZ(*OFF)
     D wk_ClrDone      S              1N   INZ(*OFF)
     D wk_FirstSel     S                   LIKE(wk_Sfl01RRN)
     D wk_Refresh      S              1N   INZ(*OFF)
     D wk_Sort         S              1A
     D wk_SelChk       S              2A
     D wk_ChgChk       S              2A
     D wk_LotEntryChk  S              2A
     D wk_ShipChk      S              2A
     D wk_LblChk       S              2A
     D wk_TVChk        S              2A
     D wk_DltLblChk    S              2A
     D wk_LblType      S              1A
     D wk_LblQty       S              7P 0
     D wk_LblUOM       S              2A
     D wk_S1LTYP       S              1A
     D wk_S1UOMQ       S             13P 4
     D wk_S1UOM        S              2A
     D wk_SSCC         S              9P 0
     D wk_SSCCBC       S             20A
     D wk_ChkDgt       S              1A
     D wk_ReprtLbl     S              1N
     D wk_GDTURN       S              9S 0
     D wk_GDTSEQ       S              3S 0
     D wk_GDRSEQ       S              5S 0
     D wk_GDITEM       S             15A
     D wk_GDIMDS       S             30A
     D wk_GDPLOC       S             17A
     D wk_GDQSTC       S             13P 4
     D wk_GDSSCCBC     S             20A
     D wk_GDSSCC       S              9S 0
     D wk_GDCNVY       S              1A
     D wk_GDQOPK       S             13P 4
     D wk_GDLBL1       S              1A
     D wk_GDLBL2       S              1A
     D wk_GDLBL3       S              1A
     D wk_GDORD#       S              8S 0
     D wk_GDORL#       S              3S 0
     D wk_GDBLN#       S              3S 0
     D wk_ODWH         S              3S 0
     D wk_IWFTCL       S              5A
     D wk_IMPCLS       S              4A
     D wk_IMLOT        S              1A
     D wk_IMUOMS       S              2A
     D wk_IMUOMW       S              2A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN1       S             13P 5
     D wk_IMUDN2       S             13P 5
     D wk_IMUDN4       S             13P 5
     D wk_IWPCPC       S              9P 4
     D wk_TDKEY1       S             15A
     D wk_BDITEM       S             15A
     D wk_BDUPC12      S             12A
     D wk_LotCnt       S              5P 0
     D wk_Carton#      S              5P 0
     D wk_Bulk         S              1N
     D wk_UnitWgt      S              9P 5
     D wk_DspView      S              1A
     D wk_ChkCNVY      S              1A
     D wk_SelItem#     S             15A
     D wk_SelOrder#    S              8S 0
     D wk_Data15       S             15A
     D wk_CurShipQty   S              9P 0
     D wk_OEORD#       S              8S 0
     D wk_OEORRF       S             22A
     D wk_IMITEM       S             15A
     D wk_MinItem      S             15A
     D wk_MaxItem      S             15A
     D wk_TotLDQSTC    S             13P 4
     D wk_W1Type       S              1A
     D wk_ToteClosed   S              1N
     D wk_ToteWarn1    S                   LIKE(W1TOTE)
     D wk_EMFNAM       S             18A
     D wk_EMLNAM       S             18A
     D wk_CustCnt      S              7P 0
     D wk_RcdCnt       S              7P 0
     D wk_AlwF16       S              1N   INZ(*OFF)
     D wk_AlwF22       S              1N   INZ(*ON)
     D wk_RC0604       S              2A
     D wk_RC0606       S              2A
     D wk_RC0608       S              2A
     D wk_RC0622       S             10I 0
     D wk_RC0626       S              2A
     D wk_Error        S              1N   INZ(*OFF)
     D wk_Pgm01        S              1N   INZ(*OFF)                            HYR0604 called
     D wk_Pgm02        S              1N   INZ(*OFF)                            HYR0622 called
     D wk_Pgm03        S              1N   INZ(*OFF)                            HYR0606 called
     D wk_Pgm04        S              1N   INZ(*OFF)                            HYR0608 called
     D wk_Pgm05        S              1N   INZ(*OFF)                            HYR0626 called
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D wk_PackLane     S             10A
     D wk_ScanMode     S               N   INZ(*OFF)
     D wk_PrevLbld     S                   like(wk_GDQSTC)
     D wk_GDDVRT       S                   like(GDDVRT)
     D wk_ShipPick     S              4A
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D sv_QOPK         S                   LIKE(GDQOPK)
     D sv_SSORD#       S              8S 0
     D sv_SSPO#        S             22A
     D sv_SSITEM       S             15A
     D sv_SSCNVY       S              1A
     D sv_SSTOTE       S             10A
     D sv_SSSTS        S              2S 0
     D @UTURN          S                   LIKE(GDTURN)                         SFL01 keys
     D @UITEM          S                   LIKE(GDITEM)                            "   "
     D @UORD#          S                   LIKE(GDORD#)                            "   "
     D @UORL#          S                   LIKE(GDORL#)                            "   "
     D @UBLN#          S                   LIKE(GDBLN#)                            "   "
     D @URSEQ          S                   LIKE(GDRSEQ)                            "   "
     D @USSCCBC        S                   LIKE(GDSSCCBC)                          "   "
     D @DTURN          S                   LIKE(GDTURN)                            "   "
     D @DITEM          S                   LIKE(GDITEM)                            "   "
     D @DORD#          S                   LIKE(GDORD#)                            "   "
     D @DORL#          S                   LIKE(GDORL#)                            "   "
     D @DBLN#          S                   LIKE(GDBLN#)                            "   "
     D @DRSEQ          S                   LIKE(GDRSEQ)                            "   "
     D @DSSCCBC        S                   LIKE(GDSSCCBC)                          "   "

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D MaintMode       C                   '1'
     D MaintMode2      C                   '2'
     D InqMode         C                   '2'
     D ShutDownMode    C                   '9'
     D @SEL            C                   ' 1'
     D @SEL2           C                   '1 '
     D @CHG            C                   ' 2'
     D @CHG2           C                   '2 '
     D @DSP            C                   ' 5'
     D @DSP2           C                   '5 '
     D @LOTENTRY       C                   ' 7'
     D @LOTENTRY2      C                   '7 '
     D @SHIP           C                   '10'
     D @LBL            C                   '12'
     D @TOTEVFY        C                   '14'
     D @LOTINQ         C                   '16'
     D @DLTLBL         C                   '29'
     D MaintHdg        C                   'Shipment Detail Processing'
     D InqHdg          C                   'Shipment Detail Inquiry'
     D MixedLblHdg     C                   'Mixed Label Selection Confirmation'
     D PrtlLblHdg      C                   'Partial Label Selection +
     D                                     Confirmation'
     D MaintOpts       C                   '1=Select   2=Change   5=Display -
     D                                       7=Lot entry   10=Ship   12=Label -
     D                                       16=Lot inquiry   29=Delete label'
     D MaintOptsDP     C                   '1=Select   5=Display -
     D                                       7=Lot entry   10=Ship   12=Label -
     D                                       16=Lot inquiry   29=Delete label'
     D MaintOptsTV     C                   '1=Select   2=Change   5=Display -
     D                                       7=Lot entry   10=Ship   12=Label -
     D                                       14=Tote verify -
     D                                       16=Lot inquiry   29=Delete label'
     D ToteOpts        C                   '1=Select   2=Change   5=Display -
     D                                       7=Lot entry   12=Label -
     D                                       16=Lot inquiry   29=Delete label'
     D InqOpts         C                   '5=Display   16=Lot inquiry'
     D F3Txt           C                   'F3=Exit'
     D F5Txt           C                   'F5=Refresh'
     D F6Txt           C                   'F6=Scn Shpmt'
     D F7TxtB          C                   'F7=Tote proc'
     D F7Txt           C                   'F7=Tote vfy'
     D F8Txt           C                   'F8=Mixed Lbl'
     D F9Txt           C                   'F9=Partial Lbl'
     D F12Txt          C                   'F12=Cancel'
     D F17Txt          C                   'F17=Top'
     D F18Txt          C                   'F18=Bottom'
     D F20Txt          C                   'F20=Subset'
     D F22Txt          C                   'F22=Emply#'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_PgmMode = in_PgmMode
     C                   EVAL      wk_ShipGrp# = in_ShipGrp#
     C                   EVAL      wk_BillTo# = in_BillTo#
     C                   EVAL      wk_ShipTo# = in_ShipTo#
     C                   EVAL      wk_DropShip# = in_DropShip#
     C                   EVAL      wk_RtnCode = *Zeros
     C                   EVAL      out_RtnCode = *Zeros

     C                   IF        wk_PgmMode = ShutDownMode
     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF

      /free
       wk_PackLane = *Blanks;
       EXEC SQL select listagg(distinct GDDVRT,',')
                 within group(order by GDDVRT)
                 into :wk_PackLane
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                      GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                      GDDVRT <> 0 and GDQOPK > 0;
      /end-free

     C                   DO

     C                   EXSR      $InzSR

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip#;
      /End-Free

     C                   IF        wk_RcdCnt > *Zero
     C                   EXSR      $PageDownSf1
     C                   ELSE
     C                   EXSR      $ClearSfl01
     C                   EVAL      S@MORE = 'Bottom'
     C                   EVAL      *IN93 = *OFF
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     C                   DOW       not *IN03

     C                   IF        wk_ToteMode
     C                   EVAL      *IN07 = *OFF
     C                   EVAL      *IN20 = *OFF
     C                   ELSE
     C                   IF        wk_PgmMode = MaintMode
     C                   EVAL      *IN07 = *ON
     C                   ELSE
     C                   EVAL      *IN07 = *OFF
     C                   ENDIF                                                  wk_PgmMode=MaintMode
     C                   EVAL      *IN20 = *ON
     C                   ENDIF                                                  wk_ToteMode

     C                   EVAL      *IN22 = wk_AlwF22
     C                   IF        wk_PackLane = '999'
     C                   EVAL      *IN06 = *ON
     C                   ENDIF

      *
      * Display Subfile
      *
     C                   WRITE     MSFCTL01A
      *
      * Check for No Records to Display
      *
     C                   IF        wk_Sfl01RRN = *Zero
     C                   WRITE     SFLNOR01
     C                   ELSE
     C                   EVAL      *IN27 = *ON
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   EVAL      *IN26 = *OFF

     C                   WRITE     SFLCMD01
     C                   EXFMT     SFLCTL01A

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN49 = *OFF
     C                   EXSR      $RMVPM

      *
      * Valid Command Key?
      *
     C                   IF        *IN25

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   EVAL      wk_RtnCode = '03'
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   IF        wk_ToteMode
     C                   EXSR      $LeaveToteProc
     C                   ITER
     C                   ENDIF                                                  wk_ToteMode
     C                   EVAL      wk_RtnCode = '12'
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Check for function key use with options present.
      *
     C                   EVAL      wk_Error = *OFF

     C                   IF        *IN07  or  *IN08  or  *IN09  or *IN17 or
     C                                *IN18  or  *IN20  or  *IN22  or  *IN91  or
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
     C                   IF        *IN07  or  *IN08  or  *IN09  or *IN17 or
     C                                *IN18  or  *IN20  or  *IN22
     C                   EVAL      wK_MsgID = 'GUM0005'
     C                   ELSE
     C                   EVAL      wK_MsgID = 'GUM0004'
     C                   ENDIF                                                  *IN07 or *IN08 or
     C                   EVAL      wk_MsgData = *Blanks
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * F8=Mixed Label and F9=Partial Label require selections.
      *
     C****               IF        (*IN08  or  *IN09)  and
     C****                           not wk_SelPr
     C****               EVAL      wk_MsgID = 'ERR1256'
     C****               SELECT
     C****               WHEN      *IN08
     C****               EVAL      wk_MsgData = 'F8=Mixed Label'
     C****               WHEN      *IN09
     C****               EVAL      wk_MsgData = 'F9=Partial Label'
     C****               ENDSL
     C****               EVAL      *IN40 = *ON
     C****               EXSR      $SndPM
     C****               ITER
     C****               ENDIF                                                  *IN08 or

      *
      * F8=Mixed Label must have mixed items.  This validation is to help
      * ensure that same-item mixed-lot cartons instead generate a "partial"
      * label.
      *
     C                   IF        *IN08  and  wk_SelItem# <> *Blanks  and
     C                               wk_SelItem# <> '*MIXED'
     C                   EVAL      wk_MsgID = 'ERR1345'
     C                   EVAL      wk_MsgData = wk_SelItem#
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  *IN08 and

     C                   ENDIF                                                  wk_Sfl01RRN<>*Zero

     C                   ENDIF                                                  *IN07 or *IN08 or

     C                   EVAL      wk_ScanMode = *OFF
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
     C                   EVAL      C1TURN = *Zero
     C                   EVAL      C1ORD# = *Zero
     C                   EVAL      C1ITEM = *Blanks
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

      *
      * Scan Shipments?
      *
     C                   WHEN      *IN06
     C                   EVAL      wk_ScanMode = *ON
     C                   EXSR      $ScanMode
     C                   EXSR      $Top1

      *
      * Tote processing?
      *
     C                   WHEN      *IN07
     C                   IF        not wk_ToteVfy
     C                   EXSR      $ToteProc
     C                   ELSE
     C                   EXSR      $ToteVfy2
     C                   ENDIF                                                  not wk_ToteVfy

      *
      * Mixed Label or Partial Label?
      *
     C                   WHEN      *IN08  or  *IN09
     C                   IF        wk_SelPr
     C                   IF        wk_Sfl01RRN <> *Zero
     C                   EXSR      $ProcessSf1
     C                   ENDIF                                                  wk_Sfl01RRN<>*Zero
     C                   ELSE
     C                   IF        *IN08
     C                   EVAL      wk_LblType = 'M'
     C                   ELSE
     C                   EVAL      wk_LblType = 'P'
     C                   ENDIF                                                  *IN08
     C                   CALLP(E)  HYR0626 ('1':wk_ShipGrp#:wk_BillTo#:
     C                                      wk_ShipTo#:wk_DropShip#:wk_LblType:
     C                                      wk_RC0626)
     C                   EVAL      wk_Pgm05 = *ON
     C                   IF        wk_RC0626 = *Zeros
     C                   EVAL      C1TURN = *Zero
     C                   EVAL      C1ORD# = *Zero
     C                   EVAL      C1ITEM = *Blanks
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF
     C                   ENDIF                                                  wk_RC0626=*Zeros
     C                   EVAL      *IN93 = *OFF
     C                   ITER
     C                   ENDIF                                                  wk_SelPr

      *
      * Complete customer shipment?
      *
     C****               WHEN      *IN09
     C****               EXSR      $CompShip

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

      *
      * Change employee number?
      *
     C                   WHEN      *IN22
     C                   EXSR      $PromptEmply
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      *IN22 = *OFF

     C                   ENDSL

      *
      * Home Key pressed?, set cursor on position field.
      *
     C                   IF        *IN94
     C                   EVAL      *IN49 = *ON
     C                   ENDIF                                                  *IN94

     C                   ELSE

      *
      * Enter Key or F8=Mixed Label or F9=Partial Label
      *
      * No Records in Subfile?, Try to refresh subfile.
      *
     C                   IF        wk_Sfl01RRN = *Zero

     C                   EXSR      $Top1

     C                   ELSE

      *
      * Edit options if subfile has records.
      *
     C                   EXSR      $EditSf1

      *
      * If all options are valid, process the selections.
      *
     C                   IF        not wk_Error

      *
      * If user presse Enter, but no options or position-to present,
      * exit program.
      *
     C                   IF        not wk_OptPr  and  C1TURN = *Zero and
     C                               C1ORD# = *Zero  and  C1ITEM = *Blanks
     C                   LEAVE
     C                   ENDIF                                                  not wk_OprPr and

      *
      * Read and Process Subfile
      *
     C                   EXSR      $ProcessSf1

     C                   IF        C1TURN <> *Zero  or  C1ORD# <> *Zero  or
     C                               C1ITEM <> *Blanks
     C                   EXSR      $PageDownSf1
     C                   ENDIF                                                  C1TURN<>*Zero or

     C                   ELSE
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  not wk_Error

     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   ENDIF                                                  *IN25=*ON

     C                   ENDDO                                                  not *IN03

     C                   ENDDO

     C                   EVAL      out_RtnCode = wk_RtnCode

     C                   RETURN

     C/EJECT
      **************************************************************************
      *****                S  U  B  R  O  U  T  I  N  E  S                 *****
      **************************************************************************
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
      * Remove messages from program message queue.                            *
      **************************************************************************
     CSR   $RmvPM        BEGSR

     C                   CALLP(E)  QMHRMVPM ('*' : *Zero : *Blanks : '*ALL' :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page down subfile SFL01 - main subfile.                               *
      **************************************************************************
     CSR   $PageDownSf1  BEGSR

      *
      * If position-to entered, set sort control field.
      *
     C                   IF        not *IN91  and  not *IN05
     C                   SELECT
     C                   WHEN      C1TURN <> *Zero
     C                   EVAL      wk_Sort = '1'                                by Turn#
     C                   IF        not %open(HYLSGDT3)
     C                   OPEN      HYLSGDT3
     C                   ENDIF                                                  not %open(HYLSGDT3)
     C                   WHEN      C1ORD# <> *Zero
     C                   EVAL      wk_Sort = '2'                                by Order#
     C                   IF        not %open(HYLSGDT4)
     C                   OPEN      HYLSGDT4
     C                   ENDIF                                                  not %open(HYLSGDT4)
     C                   WHEN      C1ITEM <> *Blanks
     C                   EVAL      wk_Sort = '3'                                by Item#
     C                   IF        not %open(HYLSGDT5)
     C                   OPEN      HYLSGDT5
     C                   ENDIF                                                  not %open(HYLSGDT5)
     C                   ENDSL
     C                   ENDIF                                                  not *IN91 and

      *
      * If no records, clear subfile and initialize relative record and page count.
      *
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C     *LOVAL        SETLL     HYLSGDT3
     C                   WHEN      wk_Sort = '2'                                by Order#
     C     *LOVAL        SETLL     HYLSGDT4
     C                   WHEN      wk_Sort = '3'                                by Item#
     C     *LOVAL        SETLL     HYLSGDT5
     C                   ENDSL

     C                   IF        not %found

     C                   EXSR      $ClearSfl01

     C                   IF        *IN91
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   ENDIF                                                  *IN91
     C                   ELSE

      *
      * Set starting point in queue
      *   If Page Down, start at next page
      *   If Refresh, start at top
      *   If Search, start at position to...
      *   Otherwise, initial build or page up, start at BOF
      *
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY1       SETGT     HYLSGDT3
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY1       SETLL     HYLSGDT3
     C                   OTHER                                                  Enter,...
     C     key_GD1       SETLL     HYLSGDT3
     C                   ENDSL
     C                   WHEN      wk_Sort = '2'                                by Order#
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY2       SETGT     HYLSGDT4
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY2       SETLL     HYLSGDT4
     C                   OTHER                                                  Enter,...
     C     key_GD2       SETLL     HYLSGDT4
     C                   ENDSL
     C                   WHEN      wk_Sort = '3'                                by Item#
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY3       SETGT     HYLSGDT5
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY3       SETLL     HYLSGDT5
     C                   OTHER                                                  Enter,...
     C     key_GD3       SETLL     HYLSGDT5
     C                   ENDSL
     C                   ENDSL

     C                   ENDIF                                                  not %found

     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C                   DOU       %eof(HYLSGDT3)  or  wk_Include
     C     key_GDx       READE     HYLSGDT3
     C                   IF        not %eof(HYLSGDT3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT3)
     C                   ENDDO                                                  %eof(HYLSGDT3) or
     C                   WHEN      wk_Sort = '2'                                by Order#
     C                   DOU       %eof(HYLSGDT4)  or  wk_Include
     C     key_GDx       READE     HYLSGDT4
     C                   IF        not %eof(HYLSGDT4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT4)
     C                   ENDDO                                                  %eof(HYLSGDT4) or
     C                   WHEN      wk_Sort = '3'                                by Item#
     C                   DOU       %eof(HYLSGDT5)  or  wk_Include
     C     key_GDx       READE     HYLSGDT5
     C                   IF        not %eof(HYLSGDT5)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT5)
     C                   ENDDO                                                  %eof(HYLSGDT5) or
     C                   ENDSL

      *
      * If EOF, send Bottom of the List message
      *
     C                   IF        %eof
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   IF        not *IN91  and  not *IN05
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C     key_GDx       SETLL     HYLSGDT3
     C                   WHEN      wk_Sort = '2'                                by Order#
     C     key_GDx       SETLL     HYLSGDT4
     C                   WHEN      wk_Sort = '3'                                by Item#
     C     key_GDx       SETLL     HYLSGDT5
     C                   ENDSL
     C****               IF        %found
     C                   IF        %equal
     C                   EXSR      $Bottom1
     C                   ELSE
     C                   EVAL      S@MORE = 'Bottom'
     C                   ENDIF                                                  %found
     C                   LEAVESR
     C                   ENDIF                                                  not *IN91 and

      *
      * If refresh, get last record.
      *
     C                   IF        *IN05
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C                   EVAL      C1TURN = *Hival
     C     key_GD1       SETGT     HYLSGDT3
     C                   EVAL      C1TURN = *Zero
     C                   DOU       %eof(HYLSGDT3)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT3
     C                   IF        not %eof(HYLSGDT3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT3)
     C                   ENDDO                                                  %eof(HYLSGDT3) or
     C                   WHEN      wk_Sort = '2'                                by Order#
     C                   EVAL      C1ORD# = *Hival
     C     key_GD2       SETGT     HYLSGDT4
     C                   EVAL      C1ORD# = *Zero
     C                   DOU       %eof(HYLSGDT4)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT4
     C                   IF        not %eof(HYLSGDT4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT4)
     C                   ENDDO                                                  %eof(HYLSGDT4) or
     C                   WHEN      wk_Sort = '3'                                by Item#    '
     C                   EVAL      C1ITEM = *Hival
     C     key_GD3       SETGT     HYLSGDT5
     C                   EVAL      C1ITEM = *Blanks
     C                   DOU       %eof(HYLSGDT5)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT5
     C                   IF        not %eof(HYLSGDT5)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT5)
     C                   ENDDO                                                  %eof(HYLSGDT5) or
     C                   ENDSL
     C                   IF        %eof
     C                   EVAL      S@MORE = 'Bottom'
     C                   EVAL      C1TURN = *Zero
     C                   EVAL      C1ORD# = *Zero
     C                   EVAL      C1ITEM = *Blanks
     C                   LEAVESR
     C                   ENDIF                                                  %eof
     C                   ENDIF                                                  *IN05

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   ENDIF                                                  %eof

     C                   DOW        (wk_Sort = '1' and not %eof(HYLSGDT3)) or
     C                               (wk_Sort = '2' and not %eof(HYLSGDT4)) or
     C                               (wk_Sort = '3' and not %eof(HYLSGDT5))

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

     C                   WRITE     SFL01A

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01PC = 1
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UTURN = GDTURN
     C                   EVAL      @UITEM = GDITEM
     C                   EVAL      @UORD# = GDORD#
     C                   EVAL      @UORL# = GDORL#
     C                   EVAL      @UBLN# = GDBLN#
     C                   EVAL      @URSEQ = GDRSEQ
     C                   EVAL      @USSCCBC = GDSSCCBC
     C                   ENDIF                                                  wk_Sfl01PC=1
     C                   EVAL      @DTURN = GDTURN
     C                   EVAL      @DITEM = GDITEM
     C                   EVAL      @DORD# = GDORD#
     C                   EVAL      @DORL# = GDORL#
     C                   EVAL      @DBLN# = GDBLN#
     C                   EVAL      @DRSEQ = GDRSEQ
     C                   EVAL      @DSSCCBC = GDSSCCBC

      *
      * Get next shipment group detail record.
      *
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C                   DOU       %eof(HYLSGDT3)  or  wk_Include
     C     key_GDx       READE     HYLSGDT3
     C                   IF        not %eof(HYLSGDT3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT3)
     C                   ENDDO                                                  %eof(HYLSGDT3) or
     C                   WHEN      wk_Sort = '2'                                by Order#
     C                   DOU       %eof(HYLSGDT4)  or  wk_Include
     C     key_GDx       READE     HYLSGDT4
     C                   IF        not %eof(HYLSGDT4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT4)
     C                   ENDDO                                                  %eof(HYLSGDT4) or
     C                   WHEN      wk_Sort = '3'                                by Item#
     C                   DOU       %eof(HYLSGDT5)  or  wk_Include
     C     key_GDx       READE     HYLSGDT5
     C                   IF        not %eof(HYLSGDT5)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT5)
     C                   ENDDO                                                  %eof(HYLSGDT5) or
     C                   ENDSL

     C                   ENDDO                                                  wk_Sort='1' and

      *
      * Set More... or Bottom   subfile messge.
      *
     C                   IF        (wk_Sort = '1' and %eof(HYLSGDT3)) or
     C                               (wk_Sort = '2' and %eof(HYLSGDT4)) or
     C                               (wk_Sort = '3' and %eof(HYLSGDT5))
     C                   EVAL      S@MORE = 'Bottom'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  wk_Sort='1' and

     C                   EVAL      C1TURN = *Zero
     C                   EVAL      C1ORD# = *Zero
     C                   EVAL      C1ITEM = *Blanks

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear subfile 01, initialize relative record and page count.           *
      **************************************************************************
     CSR   $ClearSfl01   BEGSR

     C                   EVAL      *IN26 = *ON
     C                   EVAL      *IN27 = *OFF
     C                   WRITE     SFLCTL01A
     C                   EVAL      *IN26 = *OFF
     C                   EVAL      wk_Sfl01RRN = *Zero
     C                   EVAL      wk_Sfl01PC = *Zero

     C                   EVAL      *IN41 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page up subfile SFL01 - main subfile.                                 *
      **************************************************************************
     CSR   $PageUpSf1    BEGSR

     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C     PGUKEY1       SETLL     HYLSGDT3
     C                   DOU       %eof(HYLSGDT3)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT3
     C                   IF        not %eof(HYLSGDT3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT3)
     C                   ENDDO                                                  %eof(HYLSGDT3) or
     C                   WHEN      wk_Sort = '2'                                by Order#
     C     PGUKEY2       SETLL     HYLSGDT4
     C                   DOU       %eof(HYLSGDT4)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT4
     C                   IF        not %eof(HYLSGDT4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT4)
     C                   ENDDO                                                  %eof(HYLSGDT4) or
     C                   WHEN      wk_Sort = '3'                                by Item#
     C     PGUKEY3       SETLL     HYLSGDT5
     C                   DOU       %eof(HYLSGDT5)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT5
     C                   IF        not %eof(HYLSGDT5)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT5)
     C                   ENDDO                                                  %eof(HYLSGDT5) or
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

     C                   DOW       (wk_Sort = '1' and not %eof(HYLSGDT3)) or
     C                               (wk_Sort = '2' and not %eof(HYLSGDT4)) or
     C                               (wk_Sort = '3' and not %eof(HYLSGDT5))
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
      * If subfile is full, quit.
      *
     C                   IF        wk_Sfl01PC = wk_Sfl01PS
     C                   LEAVE
     C                   ENDIF                                                  wk_Sfl01PC=wk_Sfl01P

      *
      * Increment Page Counter, Decrement Rel Rec Nbr
      *
     C                   EVAL      wk_Sfl01PC = wk_Sfl01PC + 1
     C                   EVAL      wk_Sfl01RRN = wk_Sfl01RRN - 1
     C                   WRITE     SFL01A

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01RRN = wk_Sfl01PS
     C                   EVAL      @DTURN = GDTURN
     C                   EVAL      @DITEM = GDITEM
     C                   EVAL      @DORD# = GDORD#
     C                   EVAL      @DORL# = GDORL#
     C                   EVAL      @DBLN# = GDBLN#
     C                   EVAL      @DRSEQ = GDRSEQ
     C                   EVAL      @DSSCCBC = GDSSCCBC
     C                   ENDIF                                                  wk_Sfl01RRN=wk_Sfl01
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UTURN = GDTURN
     C                   EVAL      @UITEM = GDITEM
     C                   EVAL      @UORD# = GDORD#
     C                   EVAL      @UORL# = GDORL#
     C                   EVAL      @UBLN# = GDBLN#
     C                   EVAL      @URSEQ = GDRSEQ
     C                   EVAL      @USSCCBC = GDSSCCBC

      *
      * Get previous shipment group detail record.
      *
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Turn#
     C                   DOU       %eof(HYLSGDT3)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT3
     C                   IF        not %eof(HYLSGDT3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT3)
     C                   ENDDO                                                  %eof(HYLSGDT3) or
     C                   WHEN      wk_Sort = '2'                                by Order#
     C                   DOU       %eof(HYLSGDT4)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT4
     C                   IF        not %eof(HYLSGDT4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT4)
     C                   ENDDO                                                  %eof(HYLSGDT4) or
     C                   WHEN      wk_Sort = '3'                                by Item#
     C                   DOU       %eof(HYLSGDT5)  or  wk_Include
     C     key_GDx       READPE    HYLSGDT5
     C                   IF        not %eof(HYLSGDT5)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGDT5)
     C                   ENDDO                                                  %eof(HYLSGDT5) or
     C                   ENDSL

     C                   ENDDO                                                  wk_Sort='1' and

      *
      * Set More... or Top if there are more records or BOF.
      * If Beg of File reached and not a full page, fill from top down.
      *
     C                   IF        (wk_Sort = '1' and %eof(HYLSGDT3)) or
     C                               (wk_Sort = '2' and %eof(HYLSGDT4)) or
     C                               (wk_Sort = '3' and %eof(HYLSGDT5))
     C                   EVAL      S@MORE = 'Top'
     C                   IF        wk_Sfl01PC <> wk_Sfl01PS
     C                               and not wk_PgUpFrmBot1
     C                   EVAL      @UTURN = *Zero
     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UORD# = *Zero
     C                   EVAL      @UORL# = *Zero
     C                   EVAL      @UBLN# = *Zero
     C                   EVAL      @URSEQ = *Zero
     C                   EVAL      @USSCCBC = *Loval
     C                   EXSR      $PageDownSf1
     C                   ENDIF                                                  wk_Sfl01PC<>wk_Sfl01
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  wk_Sort='1' and

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move database fields to subfile fields.                                *
      **************************************************************************
     CSR   $MoveToSfl01  BEGSR

      /Free
       wk_ODWH = *Zero;
       wk_IWFTCL = *Blanks;
       EXEC SQL select ODWH into :wk_ODWH
                from OEORDT
                where ODORD#=:GDORD# and ODORL#=:GDORL# and
                      ODBLN#=:GDBLN#;
       EXEC SQL select IWFTCL into :wk_IWFTCL
                from HDIWHS
                where IWITEM=:GDITEM and IWWHS=:wk_ODWH;
       IF wk_IWFTCL = 'AMMOB';
         *IN61 = *ON;
       ELSE;
         *IN61 = *OFF;
       ENDIF;                                                                // wk_IWFTCL='AMMOB'
       S1IN61 = *IN61;
      /End-Free

     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      S1TURN = GDTURN
     C                   EVAL      S1TSEQ = GDTSEQ
     C                   EVAL      S1RSEQ = GDRSEQ
     C                   EVAL      S1OCTL = GDOCTL
     C                   EVAL      S1ORD# = GDORD#
     C                   EVAL      S1ORL# = GDORL#
     C                   EVAL      S1BLN# = GDBLN#
     C                   EVAL      S1ITEM = GDITEM
     C                   EVAL      S1ITEMX = GDITEM
     C                   EVAL      S1IMDS = GDIMDS
     C                   EVAL      S1QOPN = GDQOPN
     C                   EVAL      S1QOPK = GDQOPK
     C                   EVAL      S1QOPKX = GDQOPK
      *
     C                   IF        GDQOPK <> GDQOPKO
     C                   EVAL      *IN62 = *ON
     C                   ELSE
     C                   EVAL      *IN62 = *OFF
     C                   ENDIF
     C                   EVAL      S1IN62 = *IN62
      *
     C                   EVAL      S1QSTC = GDQSTC
     C                   EVAL      S1QSTCX = GDQSTC
     C                   EVAL      S1CNVY = GDCNVY
     C                   EVAL      S1TOTE = GDTOTE
     C                   EVAL      S1MXPO = GDMXPO
     C                   EVAL      S1MXLT = GDMXLT
     C                   EVAL      S1PO# = GDPO#
     C****               EVAL      S1PO#X = GDPO#
     C****               EVAL      S1PLOCX = GDPLOC
     C                   EVAL      S1SSCCBC = GDSSCCBC
     C                   EVAL      S1SSCC = GDSSCC
     C                   SELECT
     C                   WHEN      GDQOPK = *Zero
     C                   EVAL      S1STS = *Blanks
     C                   WHEN      GDQSTC = *Zero
     C*                   EVAL      S1STS = 'Ship'
     C                   EVAL      S1STS = ShipOrPick (GDGRP#:GDTURN:
     C                               GDORD#:GDORL#)
     C                   WHEN      GDQSTC < GDQOPK
     C                   EVAL      S1STS = 'Prtl'
     C                   WHEN      TVNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ)
     C                   EVAL      S1STS = 'Tote'
     C                   WHEN      LotsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ:GDSSCCBC:GDSSCC)
     C                   EVAL      S1STS = 'Lot'
     C                   WHEN      LotsRecalled (GDGRP#:GDTURN:GDTSEQ:GDRSEQ)
     C                   EVAL      S1STS = 'Recal'
     C                   WHEN      LblsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDOCTL:GDORL#:GDBLN#:GDLBL1:GDLBL2:GDLBL3)
     C                   EVAL      S1STS = 'Label'
     C                   WHEN      PltzNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDORD#:GDORL#:GDBLN#:GDSSCC)
     C                   EVAL      S1STS = 'Pallt'
     C                   OTHER
     C                   EVAL      S1STS = *Blanks
     C                   ENDSL

     C                   EVAL      S1PLOCSTS = GDPLOC
     C                   SELECT
     C                   WHEN      S1STS <> *Blanks
     C                   EVAL      %subst(S1PLOCSTS:15:6) = x'22' + S1STS
     C                   WHEN      GDTOTE <> *Blanks  and
     C                               %subst(GDTOTE:1:3) <> 'PLT'
     C                   EVAL      %subst(S1PLOCSTS:13:8) = ' ' + GDTOTE
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL01 main subfile.                                             *
      **************************************************************************
     CSR   $Filter1      BEGSR

     C                   EVAL      wk_Include = *ON

     C                   IF        wk_ToteMode

      *
      * Tote number subset.
      *
     C                   IF        W1TOTE <> *Blanks
     C                   IF        GDTOTE = '*MULT'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGTD
                where TDGRP#=:GDGRP# and TDBLTO=:GDBLTO and
                      TDSHTO=:GDSHTO and TDDSHP=:GDDSHP and
                      TDTOTE=:W1TOTE;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ELSE
     C                   IF        GDTOTE <> W1TOTE
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GDTOTE<>W1TOTE
     C                   ENDIF                                                  GDTOTE='*MULT'
     C                   ENDIF                                                  W1TOTE<>*Blanks


     C                   ELSE

      *
      * Order number subset.
      *
     C                   IF        F4ORD# <> *Zero
     C                   IF        GDORD# <> F4ORD#
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GDORD#<>F4ORD#
     C                   ENDIF                                                  F4ORD#<>*Zero

      *
      * Purchase order number subset.
      *
     C                   IF        F4PO# <> *Blanks
     C                   IF        GDPO# <> F4PO#
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GDPO#<>F4PO#
     C                   ENDIF                                                  F4PO#<>*Blanks

      *
      * Item number subset.
      *
     C                   IF        F4ITEM <> *Blanks
     C                   IF        GDITEM <> F4ITEM
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GDITEM<>F4ITEM
     C                   ENDIF                                                  F4ITEM<>*Blanks

      *
      * Pick mode subset.
      *
     C                   IF        F4CNVY <> *Blank
     C                   IF        GDCNVY = 'S'  and  GDSPPK = 'T'
     C                   EVAL      wk_ChkCNVY = 'T'
     C                   ELSE
     C                   EVAL      wk_ChkCNVY = GDCNVY
     C                   ENDIF                                                  GDCNVY='S' and
     C****               IF        GDCNVY <> F4CNVY
     C                   IF        wk_ChkCNVY <> F4CNVY
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_ChkCNVY<>F4CNVY
     C                   ENDIF                                                  F4CNVY<>*Blanks

      *
      * Tote ID subset.
      *
     C                   IF        F4TOTE <> *Blanks
     C                   IF        GDTOTE <> F4TOTE
     C                   IF        GDTOTE <> '*MULT'
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGTD
                where TDGRP#=:GDGRP# and TDTURN=:GDTURN and
                      TDTSEQ=:GDTSEQ and TDRSEQ=:GDRSEQ and
                      TDTOTE=:F4TOTE;
       IF wk_RcdCnt = *Zero;
         wk_Include = *OFF;
         LEAVESR;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free
     C                   ENDIF                                                  GDTOTE<>'*MULT'
     C                   ENDIF                                                  GDTOTE<>F4TOTE
     C                   ENDIF                                                  F4TOTE<>*Blanks

      *
      * Status subset.
      *
     C                   IF        F4STS <> *Zero
     C                   IF        (F4STS = 1  or  F4STS = 2  or
     C                               F4STS = 7)  and  GDDVRT=999
     C                   EVAL      wk_ShipPick = ShipOrPick (GDGRP#:GDTURN:
     C                               GDORD#:GDORL#)
     C                   ELSE
     C                   EVAL      wk_ShipPick = *Blanks
     C                   ENDIF                                                  F4STS=1 or F4STS=7
     C****               IF        (F4STS = 1  and  GDQSTC >= GDQOPK)  or
     C****                           (F4STS = 2  and (GDQSTC=0  or
     C****                            not TVNeeded(GDGRP#:
     C****                            GDBLTO:GDSHTO:GDDSHP:GDTURN:GDTSEQ:
     C****                            GDRSEQ))) or
     C****                           (F4STS = 3  and (GDQSTC=0  or
     C****                            not LotsNeeded(GDGRP#:
     C****                            GDBLTO:GDSHTO:GDDSHP:GDTURN:GDTSEQ:
     C****                            GDRSEQ:GDSSCCBC:GDSSCC))) or
     C****                           (F4STS = 4  and  not LblsNeeded(GDGRP#:
     C****                            GDBLTO:GDSHTO:GDDSHP:GDOCTL:GDORL#:
     C****                            GDBLN#:GDLBL1:GDLBL2:GDLBL3)) or
     C****                           (F4STS = 5  and  not PltzNeeded(GDGRP#:
     C****                            GDBLTO:GDSHTO:GDDSHP:GDORD#:GDORL#:
     C****                            GDBLN#:GDSSCC)) or
     C****                           (F4STS = 6  and  GDQSTC >= GDQOPK  and
     C****                             not LotsNeeded(GDGRP#:GDBLTO:GDSHTO:
     C****                             GDDSHP:GDTURN:GDTSEQ:GDRSEQ:GDSSCCBC:
     C****                             GDSSCC) and
     C****                             not LblsNeeded(GDGRP#:GDBLTO:GDSHTO:
     C****                             GDDSHP:GDOCTL:GDORL#:GDBLN#:GDLBL1:
     C****                             GDLBL2:GDLBL3) and
     C****                             not PltzNeeded(GDGRP#:GDBLTO:GDSHTO:
     C****                             GDDSHP:GDORD#:GDORL#:GDBLN#:GDSSCC)) or
     C****                           (F4STS = 7 and wk_ShipPick = 'Pick') or
     C****                           (GDQOPK = *Zero)
     C                   IF        (F4STS = 3  or  F4STS = 7)  and
     C                                GDDVRT <> 999
     C                   EVAL      wk_StsTvNeed =
     C                                TVNeeded(GDGRP#:
     C                                GDBLTO:GDSHTO:GDDSHP:GDTURN:GDTSEQ:
     C                                GDRSEQ)
     C                   ELSE
     C                   EVAL      wk_StsTvNeed = *OFF
     C                   ENDIF                                                 F4STS=3 or F4STS=7
     C                   IF        (F4STS = 4  or  F4STS = 7)
     C                   EVAL      wk_StsLotsNeed =
     C                                LotsNeeded(GDGRP#:
     C                                GDBLTO:GDSHTO:GDDSHP:GDTURN:GDTSEQ:
     C                                GDRSEQ:GDSSCCBC:GDSSCC)
     C                   IF        not wk_StsLotsNeed                           Include recalled
     C                   EVAL      wk_StsLotsNeed =                              lots with "Lot"
     C                                LotsRecalled(GDGRP#:                       status filter.
     C                                GDTURN:GDTSEQ:GDRSEQ)
     C                   ENDIF                                                  not wk_StsLotsNeed
     C                   ELSE
     C                   EVAL      wk_StsLotsNeed = *OFF
     C                   ENDIF                                                 F4STS=4 or F4STS=7
     C                   IF        (F4STS = 5  or  F4STS = 7)
     C                   EVAL      wk_StsLblsNeed =
     C                                LblsNeeded(GDGRP#:
     C                                GDBLTO:GDSHTO:GDDSHP:GDOCTL:GDORL#:
     C                                GDBLN#:GDLBL1:GDLBL2:GDLBL3)
     C                   ELSE
     C                   EVAL      wk_StsLblsNeed = *OFF
     C                   ENDIF                                                 F4STS=5 or F4STS=7
     C                   IF        (F4STS = 6  or  F4STS = 7)
     C                   EVAL      wk_StsPltzNeed =
     C                                PltzNeeded(GDGRP#:
     C                                GDBLTO:GDSHTO:GDDSHP:GDORD#:GDORL#:
     C                                GDBLN#:GDSSCC)
     C                   ELSE
     C                   EVAL      wk_StsPltzNeed = *OFF
     C                   ENDIF                                                 F4STS=6 or F4STS=7
     C                   IF        (F4STS = 1  and  (GDDVRT <> 999  or          Needs picked (DP)
     C                                wk_ShipPick <> 'Pick'))  or
     C                               (F4STS = 2  and  (GDQSTC >= GDQOPK  or     Needs shipped
     C                                 (GDDVRT = 999  and
     C                                  wk_ShipPick = 'Pick')))  or
     C                               (F4STS = 3  and (GDQOPK = *Zero  or        Needs tote-verified
     C                                not wk_StsTVNeed))  or
     C                               (F4STS = 4  and (GDQSTC = *Zero  or        Needs lot numbers
     C                                not wk_StsLotsNeed))  or
     C                               (F4STS = 5  and (GDQSTC = *Zero  or        Needs labels
     C                                not wk_StsLblsNeed))  or
     C                               (F4STS = 6  and (GDQSTC = *Zero  or        Needs palletized
     C                                not wk_StsPltzNeed))  or
     C                               (F4STS = 7  and                            Any of the above
     C                                 (GDQSTC >= GDQOPK  and
     C                                  not wk_StsTVNeed  and
     C                                  not wk_StsLotsNeed  and
     C                                  not wk_StsLblsNeed  and
     C                                  not wk_StsPltzNeed))
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  F4STS=1 and
     C                   ENDIF                                                  F4STS<>*Zero

     C                   ENDIF                                                  wk_ToteMode

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Edit options subfile SFL01 - main subfile.                            *
      **************************************************************************
     CSR   $EditSf1      BEGSR

      *
      *  Reset indicators.
      *
     C                   EVAL      wk_Error = *OFF
     C                   EVAL      wk_SelPr = *OFF
     C                   EVAL      wk_ChgPr = *OFF
     C                   EVAL      wk_DspPr = *OFF
     C                   EVAL      wk_LotEntryPr = *OFF
     C                   EVAL      wk_ShipPr = *OFF
     C                   EVAL      wk_LblPr = *OFF
     C                   EVAL      wk_TVPr = *OFF
     C                   EVAL      wk_LotInqPr = *OFF
     C                   EVAL      wk_DltLblPr = *OFF
     C                   EVAL      wk_OptPr = *OFF
     C                   EVAL      wk_ClrDone = *OFF
     C                   EVAL      wk_SelItem# = *Blanks
     C                   EVAL      wk_SelOrder# = *Zero

      *
      * Save function key indicators.
      *
     C                   EVAL      wk_IN25 = *IN25

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01A

     C                   DOW       not %eof(HYD0602A)

      *
      * Reset error indicators and turn on SFLNXTCHG indicator.
      *
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
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
     C                   IF        (S1OPT = @SEL  or  S1OPT = @SEL2)  and
     C                               wk_PgmMode = MaintMode

     C                   EVAL      S1OPT = @SEL

     C                   IF        *IN08  or  *IN09

     C                   SELECT
     C                   WHEN      *IN08
     C                   EVAL      wk_LblType = 'M'
     C                   WHEN      *IN09
     C                   EVAL      wk_LblType = 'P'
     C                   ENDSL

      ** Check to see if you can print labels...
     C                   EVAL      wk_LblChk = *Zeros
     C                   EVAL      wk_LblChk = LblOK(wk_ShipGrp#:S1TURN:
     C                               S1TSEQ:S1RSEQ:S1SSCCBC:S1SSCC)

     C                   EVAL      wk_SelChk =
     C                               SelOK(wk_ShipGrp#:S1TURN:S1TSEQ:S1SSCCBC:
     C                                     S1ORD#:S1ITEM:wk_LblType)

     C                   IF        wk_SelChk = *Zeros and wk_LblChk = *Zeros

     C                   EVAL      wk_SelPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

      *
      * If first select, clear select subfile and save first select
      * subfile relative record number.
      *
     C                   IF        wk_ClrDone = *OFF
     C                   EVAL      wk_ClrDone = *ON
     C                   EVAL      wk_FirstSel = wk_Sfl01RRN

      *
      *  Clear Subfile, Reset Rel Rec Nbr
      *
     C                   EVAL      *IN29 = *ON
     C                   WRITE     SFLCTL02A
     C                   EVAL      *IN29 = *OFF
     C                   EVAL      wk_Sfl02RRN = *Zero
     C                   ENDIF                                                  wk_ClrDone=*OFF

      *
      *  Write to "select" subfile.
      *
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   WRITE     SFL02A

     C                   IF        wk_SelItem# = *Blanks
     C                   EVAL      wk_SelItem# = S1ITEM
     C                   ELSE
     C                   IF        wk_SelItem# <> S1ITEM  and
     C                               wk_SelItem# <> '*MIXED'
     C                   EVAL      wk_SelItem# = '*MIXED'
     C                   ENDIF                                                  wk_SelItem#<>S1ITEM
     C                   ENDIF                                                  wk_SelItem#=*Blanks

     C                   IF        wk_SelOrder# = *Zero
     C                   EVAL      wk_SelOrder# = S1ORD#
     C                   ELSE
     C                   IF        wk_SelOrder# <> S1ORD#  and
     C                               wk_SelOrder# <> -1
     C                   EVAL      wk_SelOrder# = -1
     C                   ENDIF                                                  wk_SelOrder#<>S1ORD#
     C                   ENDIF                                                  wk_SelOrder#=*Zero

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   IF        wk_SelChk <> *Zeros
     C                   EVAL      wk_MsgID = 'ERR1244'
     C                   EVAL      wk_MsgData = wk_SelChk
     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR1272'
     C                   EVAL      wk_MsgData = wk_LblChk
     C                   ENDIF
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ENDIF                                                  wk_SelChk=*Zeros

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1255'
     C                   EVAL      wk_MsgData = ' '
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ENDIF                                                  *IN08 or *IN09

     C                   ELSE

      *
      * Changes present?
      *
     C                   IF        (S1OPT = @CHG  or  S1OPT = @CHG2)
     C                               and  wk_PgmMode = MaintMode

     C                   EVAL      S1OPT = @CHG
     C                   EVAL      wk_ChgChk = ChgOK(wk_ShipGrp#:
     C                               S1TURN:S1TSEQ:S1SSCCBC)

     C                   IF        wk_ChgChk = *Zeros

     C                   EVAL      wk_ChgPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1258'
     C                   EVAL      wk_MsgData = wk_ChgChk
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_ChgChk=*Zero

     C                   ELSE

      *
      * Displays present?
      *
     C                   IF        S1OPT = @DSP  or  S1OPT = @DSP2

     C                   EVAL      S1OPT = @DSP
     C                   EVAL      wk_DspPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

      *
      * Lot entrys present?
      *
     C                   IF        (S1OPT = @LOTENTRY  or  S1OPT = @LOTENTRY2)
     C                               and  wk_PgmMode = MaintMode

     C                   EVAL      S1OPT = @LOTENTRY
     C                   EVAL      wk_LotEntryChk = LotEntryOK(wk_ShipGrp#:
     C                               S1TURN:S1TSEQ:S1RSEQ:S1SSCCBC:S1SSCC)

     C                   IF        wk_LotEntryChk = *Zeros

     C                   EVAL      wk_LotEntryPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1245'
     C                   EVAL      wk_MsgData = wk_LotEntryChk
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_LotEntryChk=*Zero

     C                   ELSE

      *
      * Shipments present?
      *
     C                   IF        S1OPT = @SHIP  and
     C                               wk_PgmMode = MaintMode  and
     C                               not wk_ToteMode

     C                   EVAL      wk_ShipChk = ShipOK(wk_ShipGrp#:S1TURN:
     C                               S1TSEQ:S1RSEQ:S1SSCCBC)

     C                   IF        wk_ShipChk = *Zeros

     C                   EVAL      wk_ShipPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1246'
     C                   EVAL      wk_MsgData = wk_ShipChk
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_ShipChk=*Zeros

     C                   ELSE

      *
      * Labels present?
      *
     C                   IF        S1OPT = @LBL  and
     C                               wk_PgmMode = MaintMode

     C                   EVAL      wk_LblChk = LblOK(wk_ShipGrp#:S1TURN:
     C                               S1TSEQ:S1RSEQ:S1SSCCBC:S1SSCC)

     C                   IF        wk_LblChk = *Zeros

     C                   EVAL      wk_LblPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1272'
     C                   EVAL      wk_MsgData = wk_LblChk
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_LblChk=*Zeros

     C                   ELSE

      *
      * Tote verifys present?
      *
     C                   IF        S1OPT = @TOTEVFY  and
     C                               wk_PgmMode = MaintMode  and
     C                               not wk_ToteMode
     C                               and  wk_ToteVfy                            <--- TEMPORARY

     C                   EVAL      wk_TVChk = ToteVfyOK(wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:S1TOTE)

     C                   IF        wk_TVChk = *Zeros

     C                   EVAL      wk_TVPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1397'
     C                   EVAL      wk_MsgData = wk_TVChk
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_TVChk=*Zeros

     C                   ELSE

      *
      * Lot inquirys present?
      *
     C                   IF        S1OPT = @LOTINQ

     C                   EVAL      wk_LotInqPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

      *
      * Delete labels present?
      *
     C                   IF        S1OPT = @DLTLBL
     C                               and  wk_PgmMode = MaintMode

     C                   EVAL      wk_DltLblChk = DltLblOK(wk_ShipGrp#:
     C                               S1TURN:S1TSEQ:S1SSCCBC)

     C                   IF        wk_DltLblChk = *Zeros

     C                   EVAL      wk_ChgPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1259'
     C                   EVAL      wk_MsgData = wk_DltLblChk
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(wk_BillTo#:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(wk_ShipTo#:'Z'))
     C                   IF        wk_DropShip# = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(wk_DropShip#:'Z'))
     C                   ENDIF                                                  wk_DropShip#=*Zero
     C                   EVAL      %subst(wk_MsgData:33:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:42:15) = S1ITEM
     C                   EVAL      %subst(wk_MsgData:57:8) =
     C                               %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:65:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:68:3) =
     C                               %triml(%editc(S1BLN#:'Z'))
     C                   EVAL      %subst(wk_MsgData:71:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_DltLblChk=*Zero

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
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EXSR      $SndPM

     C                   ELSE

      *
      * Ignore Blank Options, Turn off Error & SFLNXTCHG
      *
     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ENDIF                                                   S1OPT<>*Blanks

     C                   ENDIF                                                   S1OPT=@DLTLBL

     C                   ENDIF                                                   S1OPT=@TOTEVFY

     C                   ENDIF                                                   S1OPT=@LOTINQ

     C                   ENDIF                                                   S1OPT=@LBL

     C                   ENDIF                                                   S1OPT=@SHIP

     C                   ENDIF                                                   S1OPT=@LOTENTRY or

     C                   ENDIF                                                   S1OPT=@DSP or

     C                   ENDIF                                                   S1OPT=@CHG or

     C                   ENDIF                                                   S1OPT=@SEL or

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01A

     C                   ENDDO                                                  not %eof(HYD0602A)

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
     C                   IF        wk_ChgPr  or  wk_DspPr  or  wk_LotEntryPr  or
     C                               wk_ShipPr  or  wk_LblPr  or
     C                               wk_TVPr  or
     C                               wk_LotInqPr  or  wk_DltLblPr
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_ChgPr or
     C                   ELSE
     C                   IF        wk_SelPr  or  wk_ChgPr  or  wk_DspPr  or
     C                               wk_LotEntryPr  or  wk_ShipPr  or
     C                               wk_TVPr  or
     C                               wk_LblPr  or  wk_LotInqPr  or  wk_DltLblPr
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

     C                   READC     SFL01A

     C                   DOW       not %eof(HYD0602A)  and  not *IN03

      *
      *  F12=Cancel Key
      *
     C                   IF        not *IN12

     C                   SELECT

     C                   WHEN      S1OPT = @CHG
     C                   EXSR      $Change

     C                   WHEN      S1OPT = @DSP
     C                   EXSR      $Display

     C                   WHEN      S1OPT = @LOTENTRY
     C                   EXSR      $LotEntry

     C                   WHEN      S1OPT = @SHIP
     C                   EXSR      $Ship

     C                   WHEN      S1OPT = @LBL
     C                   EXSR      $PrtLabel

     C                   WHEN      S1OPT = @TOTEVFY
     C                   EXSR      $ToteVfy

     C                   WHEN      S1OPT = @LOTINQ
     C                   EXSR      $LotInq

     C                   WHEN      S1OPT = @DLTLBL
     C                   EXSR      $DltLabel

     C                   WHEN      S1OPT = @SEL

      *
      * Keep "selects" active - turn SFLNXTCHG on.
      *
     C                   EVAL      *IN28 = *ON

     C                   ENDSL

     C                   IF        not *IN03
     C                   IF        *IN12  or  S1OPT = @SEL
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   ENDIF                                                   *IN12
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A
     C                   ENDIF                                                   not *IN03

     C                   EVAL      S@RCNO = wk_Sfl01RRN

     C                   ELSE

      *
      * Cancel requested - save remaining options.
      *
     C                   EVAL      *IN28 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   EVAL      *IN62 = S1IN62
     C                   UPDATE    SFL01A

     C                   ENDIF                                                  not *IN12

      *
      * Get next changed subfile record (save status of F12=Cancel).
      *
     C                   IF        not %eof(HYD0602A)  and  not *IN03
     C                   EVAL      wk_IN12 = *IN12
     C                   READC     SFL01A
     C                   EVAL      *IN12 = wk_IN12
     C                   ENDIF                                                  not %eof(HYD0602A)

     C                   ENDDO                                                  not %eof(HYD0602A)

      *
      * If "selects" are present and Not F12=Cancel, process "selects".
      *
     C                   IF        wk_SelPr  and  not *IN03  and  not *IN12
     C                   IF        *IN08  or  *IN09
     C                   EVAL      S@RCNO = wk_FirstSel
     C                   EXSR      $ProcSelects
     C                   ENDIF                                                  *IN08 or *IN09
     C                   ENDIF                                                  wk_SelPr and

     C                   IF        not *IN03  and  not *IN12  and  wk_Refresh
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF
     C                   EVAL      wk_Refresh = *OFF
     C                   ENDIF                                                  not *IN03 and

     C                   EVAL      *IN12 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process selects.                                                       *
      **************************************************************************
     CSR   $ProcSelects  BEGSR

     C                   SELECT
     C                   WHEN      *IN08                                        Mixed label prt
     C                   EVAL      C2HDG2 = MixedLblHdg
     C                   EVAL      C2HDG2 = center(C2HDG2)
     C                   WHEN      *IN09                                        Partial label prt
     C                   EVAL      C2HDG2 = PrtlLblHdg
     C                   EVAL      C2HDG2 = center(C2HDG2)
     C                   OTHER
     C                   EVAL      C1HDG1 = *Blanks
     C                   ENDSL

     C                   DOW       not *IN03  and  not *IN12
     C

      *
      * Format select confirmation screen.
      *
     C                   WRITE     MSFCTL01A
     C                   WRITE     SFLCMD02
     C                   EVAL      *IN29 = *OFF
     C                   EVAL      *IN27 = *ON
     C                   EXFMT     SFLCTL02A
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $RmvPM

      *
      * Valid function key?
      *
     C*****              IF        *IN25  and  not *IN08  and not *IN09
     C                   IF        *IN25

     C                   ELSE

     C                   EVAL      wk_LblQty = *Zero
     C                   EVAL      wk_LblUOM = *Blanks

      *
      * Read & process selects, then exit.
      *
     C                   READC     SFL02A

     C                   DOW       not %eof(HYD0602A)

      /Free
       wk_GDDVRT = 0;
       wk_PrevLbld = 0;

       EXEC SQL insert into HYW0602
                       (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                        W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                        W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                values(:ps_Job#, :wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                       :wk_DropShip#, :S1TURN, :S1TSEQ, :S1RSEQ,
                       :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);

       EXEC SQL select GDDVRT
                  into :wk_GDDVRT
                  from HYPSGDT
                 where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                       GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                       GDOCTL=:S1OCTL and GDORD#=:S1ORD# and
                       GDORL#=:S1ORL# and GDBLN#=:S1BLN# and
                       GDRSEQ=:S1RSEQ;

       EXEC SQL select coalesce(sum(S1UOMQ),0)
                  into :wk_PrevLbld
                  from hypscwk
                 where S1ORD# = :S1ORD# and S1ORL# = :S1ORL# and
                       S1TURN = :S1TURN;

      /End-Free

     C                   IF        wk_GDDVRT = 999 and wk_PrevLbld > 0
     C                   EVAL      wk_LblQty = wk_LblQty +
     C                             (S1QSTC - wk_PrevLbld)
     C                   ELSE
     C                   EVAL      wk_LblQty = wk_LblQty + S1QSTC
     C                   ENDIF
     C                   IF        wk_LblUOM = *Blanks  and *IN09
      /Free
       wk_IMUOMS = *Blanks;
       EXEC SQL select IMUOMS into :wk_IMUOMS
                from HDIMST a
                where IMITEM=:S1ITEM;
       wk_LblUOM = wk_IMUOMS;
      /End-Free
     C                   ENDIF                                                  wk_LblUOM=*Blanks

     C                   READC     SFL02A

     C                   ENDDO                                                  not %eof(HYD0602A)

     C                   IF        *IN08  or  *IN09
     C                   SELECT
     C                   WHEN      *IN08
     C                   EVAL      wk_LblType = 'M'
     C                   WHEN      *IN09
     C                   EVAL      wk_LblType = 'P'
     C                   ENDSL

      /Free
       wk_SSCC = *Zero;
       EXSR $WrtHYPSCWK;

     C                   SELECT
     C                   WHEN      *IN08
     C                   EVAL      wk_LblQty = *Zero
     C                   EVAL      wk_LblUOM = *Blanks
     C****               WHEN      *IN09
     C****               EVAL      wk_LblQty = wk_GDQSTC
     C****               EVAL      wk_LblUOM = wk_IMUOMS
     C                   ENDSL

       wk_Pgm02 = *ON;
       // wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:
       //                           wk_SSCCBC:wk_LblType:*ON:*ON:0:*Blanks);
       wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:wk_SSCCBC:
                                 wk_LblType:*ON:*ON:wk_LblQty:wk_LblUOM);

       EXEC SQL update HYPSGDT a
                 set GDLBL1='Y', GDSSCC=:wk_SSCC, GDSSCCBC=:wk_SSCCBC
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDSSCCBC=' ' and
                 exists (select * from HYW0602 b where
                 b.W1JOB#=:ps_Job# and b.W1GRP#=a.GDGRP# and
                 b.W1BLTO=a.GDBLTO and b.W1SHTO=a.GDSHTO and
                 b.W1DSHP=a.GDDSHP and b.W1OCTL=a.GDOCTL and
                 b.W1ORD#=a.GDORD# and b.W1ORL#=a.GDORL# and
                 b.W1BLN#=a.GDBLN# and b.W1RSEQ=a.GDRSEQ);
       EXEC SQL delete from HYW0602
                where W1JOB#=:ps_Job#;
      /End-Free

     C                   EVAL      wk_Refresh = *ON

      *
      * Send "Mixed label printed" or "Partial label printed" message.
      *
     C                   EVAL      wk_MsgID = 'MSG0377'
     C                   IF        *IN08
     C                   EVAL      wk_MsgData = 'Mixed carton'
     C                   EVAL      %subst(wk_MsgData:15:30) = '.'
     C                   ELSE
     C                   EVAL      wk_MsgData = 'Partial carton'
     C                   EVAL      %subst(wk_MsgData:15:30) = ' for item ' +
     C                               %trimr(wk_SelItem#) + '.'
     C                   ENDIF                                                  *IN08
     C                   EXSR      $SndPM
     C                   ENDIF                                                  *IN08 or *IN09

     C                   LEAVE

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03 and

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write HYPSCWK records.                                                 *
      **************************************************************************
     CSR   $WrtHYPSCWK   BEGSR


     C                   IF        not %open(HYW0602)
     C                   OPEN      HYW0602
     C                   ENDIF                                                  not %open(HYW0602)

     C                   IF        not %open(HYPSGLD)
     C                   OPEN      HYPSGLD
     C                   ENDIF                                                  not %open(HYPSGLD)

     C                   IF        not %open(HYPSCWK)
     C                   OPEN      HYPSCWK
     C                   ENDIF                                                  not %open(HYPSCWK)

     C                   IF        wk_SSCC = *Zero
     C     *LOCK         IN        wk_SSCCSrlRef
     C                   EVAL      wk_SSCCSrlRef = wk_SSCCSrlRef + 1
     C                   OUT       wk_SSCCSrlRef
     C                   EVAL      wk_SSCC = wk_SSCCSrlRef
     C                   ENDIF                                                  wk_SSCC=*Zero
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(wk_SSCC:'X')
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt

     C     key_W1        SETLL     HYW0602
     C                   DOU       %eof(HYW0602)
     C     key_W1        READE     HYW0602
     C                   IF        %eof(HYW0602)
     C                   ITER
     C                   ENDIF                                                  %eof(HYW0602)

      /Free

       wk_PrevLbld = 0;
       wk_GDDVRT = 0;
       wk_GDTURN = *Zero;
       wk_GDTSEQ = *Zero;
       wk_GDITEM = *Blanks;
       wk_GDIMDS = *Blanks;
       wk_GDPLOC = *Blanks;
       wk_GDQSTC = *Zero;
       IF wk_LblType <> 'F';
         EXEC SQL select GDTURN, GDTSEQ, GDITEM, GDIMDS,
                         GDPLOC, GDQSTC, GDDVRT
                   into :wk_GDTURN, :wk_GDTSEQ, :wk_GDITEM, :wk_GDIMDS,
                        :wk_GDPLOC, :wk_GDQSTC, :wk_GDDVRT
                  from HYPSGDT
                  where GDGRP#=:W1GRP# and GDBLTO=:W1BLTO and
                   GDSHTO=:W1SHTO and GDDSHP=:W1DSHP and
                   GDOCTL=:W1OCTL and GDORD#=:W1ORD# and
                   GDORL#=:W1ORL# and GDBLN#=:W1BLN# and
                   GDRSEQ=:W1RSEQ;
       ELSE;
         EXEC SQL select GDTURN, GDTSEQ, GDITEM, GDIMDS,
                         GDPLOC, GDQSTC, GDDVRT
                   into :wk_GDTURN, :wk_GDTSEQ, :wk_GDITEM, :wk_GDIMDS,
                        :GDPLOC, :wk_GDQSTC, :wk_GDDVRT
                  from HYPSGDT
                  where GDGRP#=:W1GRP# and GDBLTO=:W1BLTO and
                   GDSHTO=:W1SHTO and GDDSHP=:W1DSHP and
                   GDOCTL=:W1OCTL and GDORD#=:W1ORD# and
                   GDORL#=:W1ORL# and GDBLN#=:W1BLN# and
                   GDRSEQ=:W1RSEQ and
                   GDSSCCBC=:wk_SSCCBC;
       ENDIF;                                                                // wk_LblType<>'F'
       IF wk_GDTURN = *Zero;
         ITER;
       ENDIF;                                                                // wk_GDTURN=*Zero

       wk_IMPCLS = *Blanks;
       wk_IMLOT = *Blanks;
       wk_IMUOMS = *Blanks;
       wk_IMUOMW = *Blanks;
       wk_IMIMWG = *Zero;
       wk_IMUDN1 = *Zero;
       wk_IMUDN4 = *Zero;
       EXEC SQL select IMPCLS, IMLOT, IMUOMS, IMUOMW,
                       IMIMWG, IMUDN1, IMUDN4
                 into :wk_IMPCLS, :wk_IMLOT, :wk_IMUOMS, :wk_IMUOMW,
                      :wk_IMIMWG, :wk_IMUDN1, :wk_IMUDN4
                from HDIMST
                where IMITEM=:wk_GDITEM;

       wk_TDKEY1 = *Blanks;
       EXEC SQL select TDKEY1 into :wk_TDKEY1
                from GUPTDAT
                where TDTABL='BULKPRDCLS' and TDKEY1=:wk_IMPCLS;
       IF wk_TDKEY1 = wk_IMPCLS;
         wk_Bulk = *ON;
       ELSE;
         wk_Bulk = *OFF;
       ENDIF;                                                                // wk_TDKEY1=wk_IMPCLS

       IF wk_Bulk;
         wk_UnitWgt = wk_IMUDN4;
       ELSE;
         wk_UnitWgt = wk_IMIMWG;
       ENDIF;                                                                // wk_Bulk

       wk_BDITEM = *Blanks;
       wk_BDUPC12 = *Blanks;
       EXEC SQL select BDITEM, BDUPC12
                 into :wk_BDITEM, :wk_BDUPC12
                from BARDATA
                where BDITEM=:wk_GDITEM;
       // IF wk_BDITEM = *Blanks;
       //   EXEC SQL select BDITEM, BDUPC12
       //             into :wk_BDITEM, :wk_BDUPC12
       //            from BARDATA1
       //            where BDITEM=:wk_GDITEM;
       // ENDIF;                                                             // wk_BDITEM=*Blanks

       wk_LotCnt = *Zero;
       EXEC SQL select count(*) into :wk_LotCnt
                from HYPSGLD
                where LDGRP#=:W1GRP# and LDTURN=:wk_GDTURN and
                 LDTSEQ=:wk_GDTSEQ and LDRSEQ=:W1RSEQ;

       wk_Carton# = *Zero;
       EXEC SQL select coalesce(max(S1CTN#),0) into :wk_Carton#
                from HYPSCWK
                where S1ORD#=:W1ORD# and S1TURN=:wk_GDTURN;
       wk_Carton# = wk_Carton# + 1;
      /End-Free

     C                   EVAL      SCORD# = W1ORD#
     C                   EVAL      SCSLOC = %scanrpl('-':'':wk_GDPLOC)
     C                   EVAL      SCORL# = W1ORL#
     C                   EVAL      SCBLN# = W1BLN#
     C                   EVAL      SCSRL# = wk_SSCC
     C                   EVAL      SCITEM = wk_GDITEM
     C                   EVAL      SCIMDS = wk_GDIMDS
     C                   EVAL      SCUOM = wk_IMUOMS
     C                   EVAL      SCUOMQ = wk_GDQSTC
     C                   EVAL      SCQPER = wk_IMUDN1
     C                   EVAL      SCEAQ = wk_GDQSTC * wk_IMUDN1
     C                   EVAL      SCUOMW = wk_IMUOMW
     C                   EVAL      SCUWGT = wk_UnitWgt
     C                   EVAL(H)   SCEWGT = wk_GDQSTC * wk_UnitWgt
     C                   EVAL      SCUPC = wk_BDUPC12
     C                   EVAL      SCLTYP = wk_LblType
     C                   EVAL      SCCTN# = wk_Carton#
     C                   EVAL      SCTURN = wk_GDTURN
     C                   EVAL      SCPSRL# = *Zero
     C                   EVAL      SCPEMTY = *Blank
     C                   EVAL      SCPEMPL = *Zero
     C                   IF        wk_GDDVRT = 999
     C                   EVAL      SCEMTY = 'D'                                 DigiPick
     C                   ELSE
     C                   EVAL      SCEMTY = 'B'                                 BoxLogix
     C                   ENDIF
     C                   IF        wk_EMPL <> *Zero
     C                   EVAL      SCEMPL = wk_EMPL
     C                   ELSE
     C                   EVAL      SCEMPL = 2
     C                   ENDIF                                                  wk_EMPL<>*Zero
     C
     C                   EVAL      SCJOBN = ps_Job#
     C                   EVAL      SCTSTP = %timestamp
     C                   IF        wk_LotCnt = *Zero
     C                   EVAL      SCLOT# = *Blanks
     C                   WRITE     HYRSCWK
     C                   ELSE
     C                   EVAL      wk_TotLDQSTC = *Zero
     C     key_LD        SETGT     HYPSGLD
     C                   DOU       %eof(HYPSGLD)
     C     key_LD        READPE    HYPSGLD
     C                   IF        %eof(HYPSGLD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGLD)
      **** The following code was kicking our ass when LDSSCC was populated
      **** as the HYPSGLD record was written.  Commented-out it out, and
      **** changed the HYPSGLD read loop to use SETGT/READPE, which should
      **** fix the problem that the following code was added to fix.
      **** Note - the following code was added on 11/06/18, removed on 7/17/19.
     C****               IF        LDSSCC = wk_SSCC                             Skip re-read of a
     C****               UNLOCK    HYPSGLD                                      just updated record.
     C****               ITER
     C****               ENDIF                                                  LDSSCC=wk_SSCC
     C                   IF        LDSSCC <> *Zero  and  LDSSCC <> wk_SSCC
     C                   UNLOCK    HYPSGLD
     C                   ITER
     C                   ENDIF                                                  LDSSCC<>*Zero and
     C                   IF        LDRSEQ <> W1RSEQ
     C                   UNLOCK    HYPSGLD
     C                   ITER
     C                   ENDIF                                                  LDRSEQ<>W1RSEQ
     C                   EVAL      SCLOT# = LDLOT#
     C                   EVAL      SCUOMQ = LDQSTC
     C                   EVAL      SCEAQ = LDQSTC * wk_IMUDN1
     C                   EVAL(H)   SCEWGT = LDQSTC * wk_UnitWgt
     C                   WRITE     HYRSCWK
     C                   EVAL      LDSSCC = wk_SSCC
     C                   UPDATE    HYRSGLD
     C                   EVAL      wk_TotLDQSTC = wk_TotLDQSTC + LDQSTC
     C                   IF        wk_TotLDQSTC >= wk_GDQSTC
     C                   LEAVE
     C                   ENDIF                                                  wk_TotLDQSTC>=wk_GDQ
     C                   ENDDO                                                  %eof(HYPSGLD)
     C                   ENDIF                                                  wk_LotCnt=*Zero

     C                   ENDDO                                                  %eof(HYW0602)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Subset prompt.                                                         *
      **************************************************************************
     CSR   $Subset1      BEGSR

     C                   EXSR      $InitSSInd1
     C                   EVAL      *IN93 = *OFF

     C                   DOW       not *IN12

      *
      * Format "subset" screen.
      *
     C                   WRITE     MSFCTL01B
     C                   IF        not wk_ToteVfy
     C                   EXFMT     DSPFMT4A
     C                   ELSE
     C                   EXFMT     DSPFMT4B
     C                   ENDIF                                                  not wk_ToteVfy
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
     C                   IF        F4ORD# <> sv_SSORD#  or
     C                               F4PO# <> sv_SSPO#  or
     C                               F4ITEM <> sv_SSITEM  or
     C                               F4CNVY <> sv_SSCNVY  or
     C                               F4TOTE <> sv_SSTOTE  or
     C                               F4STS <> sv_SSSTS  or
     C                               wk_Sfl01RRN = *Zero
     C                   EXSR      $Top1
     C                   EXSR      $SaveSS1
     C                   ENDIF                                                  F4ORD#<>sv_SSORD# or
     C                   ENDIF                                                  *IN12

     C                   IF        F4ORD# <> *Zero  or
     C                               F4PO# <> *Blanks  or
     C                               F4ITEM <> *Blanks  or
     C                               F4CNVY <> *Blank  or
     C                               F4TOTE <> *Blanks  or
     C                               F4STS <> *Zero
     C                   EVAL      C1SSTX = '*SUBSET*'
     C                   ELSE
     C                   EVAL      C1SSTX = *Blanks
     C                   ENDIF                                                  F4ORD#<>*Zero or

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate subset prompt screen fields.                                  *
      **************************************************************************
     CSR   $EditSS1      BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Order number must be valid.
      *
     C                   IF        F4ORD# <> *Zero
      /Free
       wk_OEORD# = *Zero;
       EXEC SQL select OEORD# into :wk_OEORD#
                from OEORHD
                where OEORD#=:F4ORD#;
      /End-Free
     C                   IF        wk_OEORD# <> F4ORD#
     C                   EVAL      wk_MsgID = 'ERR0464'
     C                   EVAL      wk_MsgData = %triml(%editc(F4ORD#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                      GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                      GDORD#=:F4ORD#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1247'
     C                   EVAL      wk_MsgData = %triml(%editc(F4ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  wk_OEORD#<>F4ORD#
     C                   ENDIF                                                  F4ORD#<>*Zero

      *
      * Purchase order number must be valid.
      *
     C                   IF        F4PO# <> *Blanks
      /Free
       wk_OEORRF = *Blanks;
       EXEC SQL select OEORRF into :wk_OEORRF
                from OEORHD
                where OEORRF=:F4PO#;
      /End-Free
     C                   IF        wk_OEORRF <> F4PO#
     C                   EVAL      wk_MsgID = 'ERR1248'
     C                   EVAL      wk_MsgData = F4PO#
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                      a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                      exists (select * from OEORHD b where b.OEORD#=a.GDORD#
                      and b.OEORRF=:F4PO#);
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1249'
     C                   EVAL      wk_MsgData = F4PO#
     C                   EVAL      %subst(wk_MsgData:23:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  wk_OEORRF<>F4PO#
     C                   ENDIF                                                  F4PO#<>*Blanks

      *
      * Item number must be valid.
      *
     C                   IF        F4ITEM <> *Blanks
      /Free
       wk_IMITEM = *Blanks;
       EXEC SQL select IMITEM into :wk_IMITEM
                from HDIMST
                where IMITEM=:F4ITEM;
      /End-Free
     C                   IF        wk_IMITEM <> F4ITEM
     C                   EVAL      wk_MsgID = 'ERR0138'
     C                   EVAL      wk_MsgData = F4ITEM
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                      GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                      GDITEM=:F4ITEM;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1250'
     C                   EVAL      wk_MsgData = F4ITEM
     C                   EVAL      %subst(wk_MsgData:16:9) =
     C                               %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  wk_IMITEM<>F4ITEM
     C                   ENDIF                                                  F4ITEM<>*Blanks

      *
      * Pick mode must be valid.
      *
     C                   IF        F4CNVY <> *Blank
     C                   IF        F4CNVY <> 'C'  and  F4CNVY <> 'T'  and
     C                               F4CNVY <> 'P'
     C                   EVAL      wk_MsgID = 'ERR1267'
     C                   EVAL      wk_MsgData = F4CNVY
     C                   EXSR      $SndPM
     C                   EVAL      *IN44 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4CNVY<>'C' and
     C                   ENDIF                                                  F4CNVY<>*Blank

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
     C                   EVAL      *IN89 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Reset subset prompt screen fields to initial program values.           *
      **************************************************************************
     CSR   $InitSS1      BEGSR

     C                   EVAL      F4ORD# = *Zero
     C                   EVAL      F4PO# = *Blanks
     C                   EVAL      F4ITEM = *Blanks
     C                   EVAL      F4CNVY = *Blank
     C                   EVAL      F4TOTE = *Blanks
     C                   EVAL      F4STS = *Zero

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Refresh subset prompt screen fields.                                   *
      **************************************************************************
     CSR   $RefreshSS1   BEGSR

     C                   EVAL      F4ORD# = sv_SSORD#
     C                   EVAL      F4PO# = sv_SSPO#
     C                   EVAL      F4ITEM = sv_SSITEM
     C                   EVAL      F4CNVY = sv_SSCNVY
     C                   EVAL      F4TOTE = sv_SSTOTE
     C                   EVAL      F4STS = sv_SSSTS

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Save subset prompt screen fields.                                      *
      **************************************************************************
     CSR   $SaveSS1      BEGSR

     C                   EVAL      sv_SSORD# = F4ORD#
     C                   EVAL      sv_SSPO# = F4PO#
     C                   EVAL      sv_SSITEM = F4ITEM
     C                   EVAL      sv_SSCNVY = F4CNVY
     C                   EVAL      sv_SSTOTE = F4TOTE
     C                   EVAL      sv_SSSTS = F4STS

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Tote processing.                                                       *
      **************************************************************************
     CSR   $ToteProc     BEGSR

     C                   EVAL      wk_W1Type = 'F'                              Find Tote

     C                   EXSR      $InitIndW1

     C                   EVAL      W1TOTE = *Blanks
     C                   EVAL      W1MSG = *Blanks

     C                   DOU       *IN12

     C                   EXFMT     WINDOW01B

     C                   EVAL      W1MSG = *Blanks
     C                   EXSR      $InitIndW1

     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

     C                   EXSR      $EditW1
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   LEAVE

     C                   ENDDO                                                  *IN12

     C                   EVAL      wk_ToteMode = *ON
     C                   EVAL      C1HDG2 = 'Tote No. ' + W1TOTE
     C                   EVAL      C1HDG2 = center(C1HDG2)
     C                   EVAL      C1OPTS = ToteOpts
     C                   EXSR      $FuncKey

     C                   EVAL      @UTURN = *Loval
     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UORD# = *Loval
     C                   EVAL      @UORL# = *Loval
     C                   EVAL      @UBLN# = *Loval
     C                   EVAL      @URSEQ = *Loval
     C                   EVAL      @USSCCBC = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Tote verify via F7.                                                    *
      **************************************************************************
     CSR   $ToteVfy2     BEGSR

     C                   EVAL      wk_W1Type = 'V'                              Tote Verify
     C                   EVAL      wk_AlwF16 = *OFF                             Don't allow F16
     C                   EVAL      wk_ToteWarn1 = *Blanks                       Clear last warning

     C                   EXSR      $InitIndW1

     C                   EVAL      W1TOTE = *Blanks
     C                   EVAL      W1MSG = *Blanks

     C                   DOU       *IN12

     C                   IF        not wk_AlwF16
     C                   EXFMT     WINDOW01
     C                   ELSE
     C                   EXFMT     WINDOW01A
     C                   ENDIF                                                  not wk_AlwF16

     C                   EVAL      W1MSG = *Blanks
     C                   EXSR      $InitIndW1

     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

     C                   EXSR      $EditW1
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   LEAVE

     C                   ENDDO                                                  *IN12

      /Free
       wk_CustCnt = *Zero;
       EXEC SQL select count(distinct digits(a.GDGRP#) || digits(a.GDBLTO) ||
                                      digits(a.GDSHTO) || digits(a.GDDSHP))
                 into :wk_CustCnt
                from HYPSGDT a
                where a.GDTOTE=:W1TOTE and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP# and
                 b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP
                 and b.GCSTS='O') and
                 ((a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip#) or
                 not exists (select * from HYPSGTC c where c.TCGRP#=a.GDGRP#
                 and c.TCBLTO=a.GDBLTO and c.TCSHTO=a.GDSHTO and
                 c.TCDSHP=a.GDDSHP and c.TCTOTE=a.GDTOTE));
      /End-Free

     C                   EVAL      wk_Pgm04 = *ON

     C                   IF        wk_CustCnt = 1
     C                   CALLP(E)  HYR0608 (MaintMode:wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:W1TOTE:wk_RC0608)
     C                   ELSE
     C                   CALLP(E)  HYR0608 (MaintMode2:*Zero:*Zero:
     C                               *Zero:*Zero:W1TOTE:wk_RC0608)
     C                   ENDIF                                                  wk_CustCnt=1

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0608'
     C                   EXSR      $SNDPM
     C                   LEAVESR
     C                   WHEN      wk_RC0608 = '12'
     C                   LEAVESR
     C                   WHEN      wk_RC0608 = *Zeros
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0602'
     C                   EXSR      $SNDPM
     C                   LEAVESR
     C                   ENDSL

      /Free
       wk_GDTURN = *Zero;
       wk_GDTSEQ = *Zero;
       wk_GDITEM = *Blanks;
       wk_GDORD# = *Zero;
       wk_GDORL# = *Zero;
       wk_GDBLN# = *Zero;
       wk_GDRSEQ = *Zero;
       EXEC SQL select GDTURN, GDTSEQ, GDITEM, GDORD#,
                       GDORL#, GDBLN#, GDRSEQ
                 into :wk_GDTURN, :wk_GDTSEQ, :wk_GDITEM, :wk_GDORD#,
                      :wk_GDORL#, :wk_GDBLN#, :wk_GDRSEQ
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                      GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                      GDTOTE=:W1TOTE
                order by GDGRP#, GDTURN, GDTSEQ, GDRSEQ
                fetch first row only;
      /End-Free

     C                   IF        wk_GDTURN <> *Zero
     C                   EVAL      @UTURN = wk_GDTURN
     C                   EVAL      @UITEM = wk_GDITEM
     C                   EVAL      @UORD# = wk_GDORD#
     C                   EVAL      @UORL# = wk_GDORL#
     C                   EVAL      @UBLN# = wk_GDBLN#
     C                   EVAL      @URSEQ = wk_GDRSEQ
     C                   ENDIF                                                  wk_GDTURN<>*Zero
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit tote verify prompt window fields.                                 *
      **************************************************************************
     CSR   $EditW1       BEGSR

     C                   EVAL      wk_Error = *OFF

     C                   DO

      *
      * Tote ID scan (or entry) is required.
      *
     C                   IF        W1TOTE = *Blanks
     C                   IF        not wk_Error
     C                   EVAL      W1MSG = 'Tote ID is required.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   LEAVE
     C                   ENDIF                                                  W1TOTE=*Blanks

     C                   EVAL      W1TOTE = %triml(W1TOTE)

     C                   IF        W1TOTE = '*MULT'
     C                   EVAL      W1MSG = '*MULT is invalid for Tote Verify.'
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   LEAVE
     C                   ENDIF                                                  W1TOTE='*MULT'

     C                   IF        %subst(W1TOTE:1:3) = 'PLT'
     C                   EVAL      W1MSG = 'Pallet pick is invalid for Tote +
     C                               Verify.'
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   LEAVE
     C                   ENDIF                                                  W1TOTE='*MULT'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGTD
                where TDGRP#=:wk_ShipGrp# and TDBLTO=:wk_BillTo# and
                      TDSHTO=:wk_ShipTo# and TDDSHP=:wk_DropShip# and
                      TDTOTE=:W1TOTE;
       IF wk_RcdCnt = *Zero;
         W1MSG = 'Tote no. not found for current shipment.';
         wk_Error = *ON;
         *IN41 = *ON;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       IF wk_W1Type = 'V';
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGTC
                  where TCGRP#=:wk_ShipGrp# and TCBLTO=:wk_BillTo# and
                        TCSHTO=:wk_ShipTo# and TCDSHP=:wk_DropShip# and
                        TCTOTE=:W1TOTE;
         IF wk_RcdCnt = *Zero;
           wk_ToteClosed = *OFF;
         ELSE;
           wk_ToteClosed = *ON;
         ENDIF;                                                              // wk_RcdCnt=*Zero
         IF wk_ToteClosed  and  (W1TOTE <> wk_ToteWarn1  or  not *IN16);
           wk_ToteWarn1 = W1TOTE;
           wk_AlwF16 = *ON;
           W1MSG = 'Tote closed.  Press F16 to re-open.';
           wk_Error = *ON;
           *IN41 = *ON;
           LEAVE;
         ENDIF;                                                              // wk_ToteClosed and
       ENDIF;                                                                // wk_W1Type='V'
      /End-Free

     C                   ENDDO

     C                   IF        wk_Error
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize tote verify prompt window fields.                           *
      **************************************************************************
     CSR   $InitIndW1    BEGSR

     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN90 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Leave tote processing mode.                                            *
      **************************************************************************
     CSR   $LeaveToteProcBEGSR

     C                   EVAL      W1TOTE = *Blanks

     C                   EVAL      wk_ToteMode = *OFF

     C                   EVAL      C1HDG2 = *Blanks
     C                   IF        not wk_ToteVfy
     C                   EVAL      C1OPTS = MaintOpts
     C                   ELSE
     C                   EVAL      C1OPTS = MaintOptsTV
     C                   ENDIF                                                  not wk_ToteVfy

     C                   EVAL      *IN12 = *OFF

     C                   EXSR      $FuncKey

     C                   EVAL      @UTURN = *Loval
     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UORD# = *Loval
     C                   EVAL      @UORL# = *Loval
     C                   EVAL      @UBLN# = *Loval
     C                   EVAL      @URSEQ = *Loval
     C                   EVAL      @USSCCBC = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print a partial label (or reprint any prior-generated label).          *
      **************************************************************************
     CSR   $PrtLabel     BEGSR

      /Free
       IF S1SSCCBC = *Blanks;
         wk_GDSSCCBC = *Blanks;
         wk_GDSSCC = *Zero;
         wk_GDLBL1 = *Blank;
         wk_GDCNVY = *Blank;
         wk_GDQSTC = *Zero;
         wk_GDITEM = *Blanks;
         EXEC SQL select GDSSCCBC, GDSSCC, GDLBL1, GDCNVY,
                         GDQSTC, GDITEM
                   into :wk_GDSSCCBC, :wk_GDSSCC, :wk_GDLBL1, :wk_GDCNVY,
                        :wk_GDQSTC, :wk_GDITEM
                  from HYPSGDT
                  where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                   GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                   GDTURN=:S1TURN and GDTSEQ=:S1TSEQ and GDCNVY=:S1CNVY and
                   GDRSEQ=:S1RSEQ;
       ELSE;
         wk_GDSSCCBC = S1SSCCBC;
         wk_GDSSCC = S1SSCC;
         wk_GDLBL1 = *Blank;
         wk_GDCNVY = *Blank;
         wk_GDQSTC = *Zero;
         wk_GDITEM = *Blanks;
         EXEC SQL select GDLBL1, GDCNVY, GDQSTC, GDITEM
                   into :wk_GDLBL1, :wk_GDCNVY, :wk_GDQSTC, :wk_GDITEM
                  from HYPSGDT
                  where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                   GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                   GDTURN=:S1TURN and GDTSEQ=:S1TSEQ and GDSSCCBC=:S1SSCCBC
                   and GDRSEQ=:S1RSEQ;
       ENDIF;                                                                // S1SSCCBC=*Blanks

       IF wk_GDSSCCBC = *Blanks  or  wk_GDLBL1 = 'N';
         wk_ReprtLbl = *OFF;
       ELSE;
         wk_ReprtLbl = *ON;
       ENDIF;                                                                // wk_GDSSCCBC=*Blanks

       IF not wk_ReprtLbl;
         EXEC SQL delete from HYW0602
                  where W1JOB#=:ps_Job#;
         EXEC SQL insert into HYW0602
                         (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                          W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                          W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                  values(:ps_Job#, :wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                         :wk_DropShip#, :S1TURN, :S1TSEQ, :S1RSEQ,
                         :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);
         IF wk_GDCNVY = 'C';
          wk_IMUDN2 = *Zero;
          EXEC SQL select IMUDN2 into :wk_IMUDN2
                   from HDIMST
                   where IMITEM=:wk_GDITEM;
          wk_IWPCPC = *Zero;
          EXEC SQL select IWPCPC into :wk_IWPCPC
                   from HDIWHS
                   where IWITEM=:wk_GDITEM and IWWHS=:wk_IMUDN2;
          IF wk_GDQSTC = wk_IWPCPC;
           wk_LblType = 'F';
          ELSE;
           wk_LblType = 'P';
          ENDIF;                                                             // wk_GDQSTC=wk_IWPCPC
         ELSE;
           wk_LblType = 'P';
         ENDIF;                                                              // wk_GDCNVY='C'
         IF wk_GDLBL1 = 'N';
           wk_SSCC = wk_GDSSCC;
         ELSE;
           wk_SSCC = *Zero;
         ENDIF;                                                              // wk_GDLBL1='N'
         EXSR $WrtHYPSCWK;
         wk_LblQty = wk_GDQSTC;
         wk_LblUOM = wk_IMUOMS;
       ELSE;
         wk_S1LTYP = *Blank;
         wk_S1UOMQ = *Zero;
         wk_S1UOM = *Blanks;
         wk_RcdCnt = *Zero;
         EXEC SQL select max(S1LTYP), sum(S1UOMQ), max(S1UOM), count(*)
                   into :wk_S1LTYP, :wk_S1UOMQ, :wk_S1UOM, :wk_RcdCnt
                  from HYPSCWK
                  where S1SRL#=:wk_GDSSCC and S1ORD#=:S1ORD# and
                   S1ORL#=:S1ORL# and S1BLN#=:S1BLN#;
         IF wk_RcdCnt = *Zero;
           wk_MinItem = *Blanks;
           wk_MaxItem = *Blanks;
           EXEC SQL select min(SCITEM), max(SCITEM), sum(SCUOMQ), max(SCUOM)
                     into :wk_MinItem, :wk_MaxItem, :wk_S1UOMQ, :wk_S1UOM
                    from HYPSSCC
                    where SCSRL#=:wk_GDSSCC and SCORD#=:S1ORD# and
                     SCORL#=:S1ORL# and SCBLN#=:S1BLN#;
           IF wk_MaxItem = wk_MinItem;
             wk_S1LTYP = 'P';
           ELSE;
             wk_S1LTYP = 'M';
           ENDIF;                                                            // wk_MaxItem<>wk_MinIt
         ENDIF;                                                              // wk_RcdCnt=*Zero
         IF wk_S1LTYP = 'F' or wk_S1LTYP = 'P' or wk_S1LTYP = 'M';
           wk_LblType = wk_S1LTYP;
         ELSE;
           wk_LblType = 'M';
         ENDIF;                                                              // wk_S1LTYP='F' or
         IF wk_LblType = 'P';
           wk_LblQty = wk_S1UOMQ;
           wk_LblUOM = wk_S1UOM;
         ELSE;
           wk_LblQty = *Zero;
           wk_LblUOM = *Blanks;
         ENDIF;                                                              // wk_LblType='P'
       ENDIF;                                                                // not wk_ReprtLbl

       wk_Pgm02 = *ON;
       IF not wk_ReprtLbl;
         wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:
                                   wk_SSCCBC:wk_LblType:*ON:*ON:
                                   wk_LblQty:wk_LblUOM);
       ELSE;
         wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:
                                   wk_GDSSCCBC:wk_LblType:*ON:*ON:
                                   wk_LblQty:wk_LblUOM);
       ENDIF;                                                                // not wk_ReprtLbl

       IF not wk_ReprtLbl  and  wk_GDCNVY <> 'C';
         EXEC SQL update HYPSGDT a
                   set GDLBL1='Y', GDSSCC=:wk_SSCC, GDSSCCBC=:wk_SSCCBC
                  where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                   a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                   a.GDSSCCBC=' ' and a.GDRSEQ=:S1RSEQ and
                   exists (select * from HYW0602 b where
                   b.W1JOB#=:ps_Job# and b.W1GRP#=a.GDGRP# and
                   b.W1BLTO=a.GDBLTO and b.W1SHTO=a.GDSHTO and
                   b.W1DSHP=a.GDDSHP and b.W1OCTL=a.GDOCTL and
                   b.W1ORD#=a.GDORD# and b.W1ORL#=a.GDORL# and
                   b.W1BLN#=a.GDBLN# and b.W1RSEQ=a.GDRSEQ);
         EXEC SQL delete from HYW0602
                  where W1JOB#=:ps_Job#;
      //
      // Send "Mixed label printed" or "Partial label printed" message.
      //
         wk_MsgID = 'MSG0377';
         IF wk_LblType = 'M';
           wk_MsgData = 'Mixed carton';
           %subst(wk_MsgData:15:30) = '.';
         ELSE;
           wk_MsgData = 'Partial carton';
           %subst(wk_MsgData:15:30) = ' for item ' +
             %trimr(S1ITEM) + '.';
         ENDIF;                                                              // wk_LblType='M'
         EXSR $SndPM;
       ELSE;
         IF not wk_ReprtLbl  and  wk_GDCNVY = 'C';
           EXEC SQL update HYPSGDT a
                     set GDLBL1='Y'
                    where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                     a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                     a.GDSSCCBC=:wk_GDSSCCBC and a.GDRSEQ=:S1RSEQ;
           EXEC SQL delete from HYW0602
                    where W1JOB#=:ps_Job#;
      //
      // Send "Carton label printed" message.
      //
           wk_MsgID = 'MSG0377';
           wk_MsgData = 'Carton';
           %subst(wk_MsgData:15:30) = ' for item ' +
             %trimr(S1ITEM) + '.';
           EXSR $SndPM;
         ELSE;
      //
      // Send "Mixed label reprinted" or "Full carton label reprinted" or
      // "Partial label reprinted" message.
      //
           wk_MsgID = 'MSG0381';
           SELECT;
           WHEN wk_LblType = 'M';
             wk_MsgData = 'Mixed carton';
             %subst(wk_MsgData:15:30) = '.';
           WHEN wk_LblType = 'F';
             wk_MsgData = 'Full carton';
             %subst(wk_MsgData:15:30) = ' for item ' +
               %trimr(S1ITEM) + '.';
           OTHER;
             wk_MsgData = 'Partial carton';
             %subst(wk_MsgData:15:30) = ' for item ' +
               %trimr(S1ITEM) + '.';
           ENDSL;
           EXSR $SndPM;
         ENDIF;                                                              // not wk_ReprtLbl and
       ENDIF;                                                                // not wk_ReprtLbl and

       IF not wk_ReprtLbl;
         wk_Refresh = *ON;
       ENDIF;                                                                // not wk_ReprtLbl
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Delete a label.                                                        *
      **************************************************************************
     CSR   $DltLabel     BEGSR

      /Free
       EXEC SQL delete from HYPEDWK
                where EWSRL#=:S1SSCC;

       EXEC SQL delete from HYPSCWK
                where S1SRL#=:S1SSCC;

       EXEC SQL update HYPSGLD
                set LDSSCC=0
                where LDGRP#=:wk_ShipGrp# and LDTURN=:S1TURN and
                 LDTSEQ=:S1TSEQ and LDSSCC=:S1SSCC;

       EXEC SQL update HYPSGDT
                set GDSSCCBC=' ', GDSSCC=0, GDLBL1='N', GDLBL2='N'
                where GDGRP#=:wk_ShipGrp# and GDSSCCBC=:S1SSCCBC;

       wk_Refresh = *ON;

      //
      // Send "Label has been deleted" message.
      //
       wk_MsgID = 'MSG0383';
       wk_MsgData = S1SSCCBC;
       %subst(wk_MsgData:21:7) = %triml(%editc(wk_ShipTo#:'Z'));
       EXSR $SndPM;
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display subfile SFL01 from the top.                                    *
      **************************************************************************
     CSR   $Top1         BEGSR

     C                   EVAL      @UTURN = *Loval
     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UORD# = *Loval
     C                   EVAL      @UORL# = *Loval
     C                   EVAL      @UBLN# = *Loval
     C                   EVAL      @URSEQ = *Loval
     C                   EVAL      @USSCCBC = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   IF        *IN17
     C                   EVAL      S@MORE = 'Top'
     C                   ENDIF                                                  *IN17

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Display subfile SFL01 from the bottom.                                 *
      **************************************************************************
     CSR   $Bottom1      BEGSR

     C                   EVAL      @UTURN = *Hival
     C                   EVAL      @UITEM = *Hival
     C                   EVAL      @UORD# = *Hival
     C                   EVAL      @UORL# = *Hival
     C                   EVAL      @UBLN# = *Hival
     C                   EVAL      @URSEQ = *Hival
     C                   EVAL      @USSCCBC = *Hival
     C                   EVAL      *IN05 = *ON
     C                   EVAL      wk_PgUPFrmBot1 = *ON
     C                   EXSR      $PageUpSf1
     C                   EVAL      wk_PgUPFrmBot1 = *OFF
     C                   EVAL      S@MORE = 'Bottom'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Change shipment detail record.                                         *
      **************************************************************************
     CSR   $Change       BEGSR

      *
      * Retrieve shipment detail record.
      *
     C                   IF        not %open(HYPSGDT)
     C                   OPEN      HYPSGDT
     C                   ENDIF                                                  not %open(HYPSGDT)
     C**** key_GD        CHAIN(N)  HYPSGDT
     C****               IF        not %found(HYPSGDT)
     C****               CLEAR     *ALL          HYRSGDT
     C****               ENDIF                                                  not %found(HYPSGDT)
     C     key_GD        SETLL     HYPSGDT
     C                   DOU       %eof(HYPSGDT)  or  GDRSEQ=S1RSEQ
     C     key_GD        READE(N)  HYPSGDT
     C                   ENDDO                                                  %eof(HYPSGDT) or
     C                   IF        %eof(HYPSGDT)
     C                   CLEAR     *ALL          HYRSGDT
     C                   ENDIF                                                  %eof(HYPSGDT)
     C

      *
      * Save changable fiekds.
      *
     C                   EVAL      sv_QOPK = GDQOPK

      *
      * Populate screen fields.
      *
     C                   EXSR      $MoveToFmt2

      *
      * Initialize screen indicators.
      *
     C                   EXSR      $InitInd2
     C                   EVAL      *IN93 = *OFF

     C                   DOU       *IN03  or  *IN12

      *
      * Format Display Screen
      *
     C                   WRITE     MSFCTL01B
     C                   EXFMT     DSPFMT2A
     C                   MOVE      *ON           *IN93
     C                   EXSR      $InitInd2
     C                   EXSR      $RMVPM

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Refresh?
      *
     C                   IF        *IN05
     C**** key_GD        CHAIN(N)  HYPSGDT
     C****               IF        not %found(HYPSGDT)
     C****               CLEAR     *ALL          HYRSGDT
     C****               ENDIF                                                  not %found(HYPSGDT)
     C     key_GD        SETLL     HYPSGDT
     C                   DOU       %eof(HYPSGDT)  or  GDRSEQ=S1RSEQ
     C     key_GD        READE(N)  HYPSGDT
     C                   ENDDO                                                  %eof(HYPSGDT) or
     C                   IF        %eof(HYPSGDT)
     C                   CLEAR     *ALL          HYRSGDT
     C                   ENDIF                                                  %eof(HYPSGDT)
     C                   EXSR      $MoveToFmt2
     C                   ITER
     C                   ENDIF                                                  *IN05

      *
      * Enter pressed.  Edit screen data.
      *
     C                   EXSR      $Edit2
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * Change shipment detail record.
      *

      *
      * No errors - continue.
      *
      * If no fields were changed, exit subroutine.
      *
     C                   IF        F2QOPK = sv_QOPK
     C                   LEAVE
     C                   ENDIF                                                  F2QOPK=sv_QOPK and

      *
      * Change shipment detail record.
      *
      * Retrieve shipment detail record for update.
      *
     C**** key_GD        CHAIN     HYPSGDT
     C****               IF        not %found(HYPSGDT)
     C     key_GD        SETLL     HYPSGDT
     C                   DOU       %eof(HYPSGDT)  or  GDRSEQ=S1RSEQ
     C     key_GD        READE     HYPSGDT
     C                   ENDDO                                                  %eof(HYPSGDT) or
     C                   IF        %eof(HYPSGDT)
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYPSGDT'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %trimr(%editc(wk_ShipGrp#:'3')) + '/' +
     C                               %trimr(%editc(S1TURN:'3')) + '/' +
     C                               %trimr(%editc(S1TSEQ:'3')) + '/' +
     C                               S1SSCCBC
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Press F12 to cancel.'
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   ITER
     C****               ENDIF                                                  not %found(HYPSGDT)
     C                   ENDIF                                                  %eof(HYPSGDT)

      *
      * If shipment detail record has been changed by another user, abort this change.
      *
     C                   IF        GDQOPK <> sv_QOPK
     C                   EVAL      wk_MsgID = 'ERR1260'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GDBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GDSHTO:'Z'))
     C                   IF        GDDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GDDSHP:'Z'))
     C                   ENDIF                                                  GDDSHP=*Zero
     C                   EVAL      %subst(wk_MsgData:31:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:40:3) =
     C                               %triml(%editc(S1TSEQ:'Z'))
     C                   EVAL      %subst(wk_MsgData:43:20) = S1SSCCBC
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   UNLOCK    HYPSGDT
     C                   ITER
     C                   ENDIF                                                  GDQOPK<>sv_QOPK

     C                   EVAL      GDQOPK = F2QOPK
     C                   IF        F2QOPK > sv_QOPK
     C                   EVAL      GDLBL1 = 'N'
     C                   ENDIF

     C                   UPDATE    HYRSGDT

     C                   EVAL      wk_MsgID = 'MSG0382'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_ShipGrp#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GDBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GDSHTO:'Z'))
     C                   IF        GDDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GDDSHP:'Z'))
     C                   ENDIF                                                  GDDSHP=*Zero
     C                   EVAL      %subst(wk_MsgData:31:9) =
     C                               %triml(%editc(S1TURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:40:3) =
     C                               %triml(%editc(S1TSEQ:'Z'))
     C                   EVAL      %subst(wk_MsgData:43:20) = S1SSCCBC
     C                   EXSR      $SndPM

     C                   EVAL      wk_Refresh = *ON

     C                   LEAVE

     C                   ENDDO                                                  *IN03 or *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit change shipment detail screen fields.                             *
      **************************************************************************
     CSR   $Edit2        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Quantity on pick ticket must be valid.
      *
     C                   IF        F2QOPK > GDQOPKO
     C                   EVAL      wk_MsgID = 'ERR1261'
     C                   IF        %subst(%editc(GDQOPKO:'J'):13:4) = *Zeros
     C                   EVAL      wk_MsgData =
     C                               %triml(%subst(%editc(GDQOPKO:'J'):1:11))
     C                   ELSE
     C                   EVAL      wk_MsgData = %triml(%editc(GDQOPKO:'J'))
     C                   ENDIF                                                  %subst(%editc(GDQOPK
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        F2QOPK < F2QSTC
     C                   EVAL      wk_MsgID = 'ERR1262'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        GDCNVY = 'C'  and
     C                               F2QOPK <> *Zero  and
     C                               F2QOPK <> GDQOPKO
     C                   EVAL      wk_MsgID = 'ERR1263'
     C                   IF        %subst(%editc(GDQOPKO:'J'):13:4) = *Zeros
     C                   EVAL      wk_MsgData =
     C                               %triml(%subst(%editc(GDQOPKO:'J'):1:11))
     C                   ELSE
     C                   EVAL      wk_MsgData = %triml(%editc(GDQOPKO:'J'))
     C                   ENDIF                                                  %subst(%editc(GDQOPK
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  GDCNVY='C'
     C                   ENDIF                                                  F2QOPK<F2QSTC
     C                   ENDIF                                                  F2QOPK>GDQOPKO

      *
      * If error detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move data to change shipment detail screen.                            *
      **************************************************************************
     CSR   $MoveToFmt2   BEGSR

      /Free
       wk_OEORRF = *Blanks;
       EXEC SQL select OEORRF into :wk_OEORRF
                from OEORHD
                where OEORD#=:GDORD#;
      /End-Free
     C                   EVAL      F2GRP#X = %triml(%editc(wk_ShipGrp#:'3'))
     C                   EVAL      F2SHTOX = C1SHTO
     C                   EVAL      F2TURNX = %triml(%editc(S1TURN:'3'))
     C                   EVAL      F2ORD#X = %triml(%editc(S1ORD#:'3'))
     C                   EVAL      F2PO# = wk_OEORRF
     C                   EVAL      F2ITEM = GDITEM
     C                   EVAL      F2IMDS = GDIMDS
     C                   EVAL      F2QOPN = GDQOPN
     C                   EVAL      F2QOPKO = GDQOPKO
     C                   EVAL      F2QOPK = GDQOPK
     C                   EVAL      F2QSTC = GDQSTC
     C                   EVAL      F2PLBL = GDPLBL
     C                   EVAL      F2MXPO = GDMXPO
     C                   EVAL      F2MXLT = GDMXLT
     C                   EVAL      F2SSCCX = %triml(%editc(GDSSCC:'Z'))
     C                   EVAL      F2TOTE = GDTOTE

     C                   SELECT
     C                   WHEN      GDQOPK = *Zero
     C                   EVAL      F2STS = *Blanks
     C                   WHEN      GDQSTC = *Zero
     C                   EVAL      F2STS = 'Not yet shipped'
     C                   WHEN      GDQSTC < GDQOPK
     C                   EVAL      F2STS = 'Partially shipped'
     C                   WHEN      TVNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ)
     C                   EVAL      F2STS = 'Tote not yet verified'
     C                   WHEN      LotsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ:GDSSCCBC:GDSSCC)
     C                   EVAL      F2STS = 'Lot number(s) not yet collected'
     C                   WHEN      LotsRecalled (GDGRP#:GDTURN:GDTSEQ:GDRSEQ)
     C                   EVAL      F2STS = 'Lot(s) recalled'
     C                   WHEN      LblsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDOCTL:GDORL#:GDBLN#:GDLBL1:GDLBL2:GDLBL3)
     C                   EVAL      F2STS = 'Label(s) not yet printed'
     C                   WHEN      PltzNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDORD#:GDORL#:GDBLN#:GDSSCC)
     C                   EVAL      F2STS = 'Not yet palletized'
     C                   OTHER
     C                   EVAL      F2STS = 'Complete'
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize change shipment detail screen indicators.                   *
      **************************************************************************
     CSR   $InitInd2     BEGSR

     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN89 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display shipment detail record.                                        *
      **************************************************************************
     CSR   $Display      BEGSR

      *
      * Retrieve customer shipment record.
      *
     C                   IF        not %open(HYPSGDT)
     C                   OPEN      HYPSGDT
     C                   ENDIF                                                  not %open(HYPSGDT)
     C**** key_GD        CHAIN(N)  HYPSGDT
     C****               IF        not %found(HYPSGDT)
     C****               CLEAR     *ALL          HYRSGDT
     C****               ENDIF                                                  not %found(HYPSGDT)
     C     key_GD        SETLL     HYPSGDT
     C                   DOU       %eof(HYPSGDT)  or  GDRSEQ=S1RSEQ
     C     key_GD        READE(N)  HYPSGDT
     C                   ENDDO                                                  %eof(HYPSGDT) or
     C                   IF        %eof(HYPSGDT)
     C                   CLEAR     *ALL          HYRSGDT
     C                   ENDIF                                                  %eof(HYPSGDT)

      *
      * Populate screen fields.
      *
     C                   EXSR      $MoveToFmt3

      *
      * Initialize screen indicators.
      *
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN93 = *OFF

     C                   DOU       *IN03  or  *IN12

      *
      * Format Display Screen
      *
     C                   WRITE     MSFCTL01B
     C                   IF        wk_DspView = '1'
     C                   EXFMT     DSPFMT3A
     C                   ELSE
     C                   EXFMT     DSPFMT3B
     C                   ENDIF                                                  wk_DspView='1'
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $RMVPM

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   EVAL      wk_RtnCode = '03'
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * F7 = View 1 / View 2??
      *
     C                   IF        *IN07
     C                   IF        wk_DspView = '1'
     C                   EVAL      wk_DspView = '2'
     C                   ELSE
     C                   EVAL      wk_DspView = '1'
     C                   ENDIF                                                  wk_DspView='1'
     C                   ITER
     C                   ENDIF                                                  *IN07

      *
      * Enter key pressed?
      *
     C                   LEAVE

     C                   ENDDO                                                  *IN03 or *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move data to display shipment detail screen.                           *
      **************************************************************************
     CSR   $MoveToFmt3   BEGSR

      /Free
       wk_OEORRF = *Blanks;
       EXEC SQL select OEORRF into :wk_OEORRF
                from OEORHD
                where OEORD#=:GDORD#;
      /End-Free
     C                   EVAL      F3GRP#X = %triml(%editc(wk_ShipGrp#:'3'))
     C                   EVAL      F3SHTOX = C1SHTO
     C                   EVAL      F3TURNX = %triml(%editc(S1TURN:'3'))
     C                   EVAL      F3ORD#X = %triml(%editc(S1ORD#:'3'))
     C                   EVAL      F3PO# = wk_OEORRF
     C                   EVAL      F3ITEM = GDITEM
     C                   EVAL      F3IMDS = GDIMDS
     C                   EVAL      F3QOPN = GDQOPN
     C                   EVAL      F3QOPK = GDQOPK
     C                   EVAL      F3QOPKO = GDQOPKO
     C                   EVAL      F3QSTC = GDQSTC
     C                   EVAL      F3PLBL = GDPLBL
     C                   EVAL      F3MXPO = GDMXPO
     C                   EVAL      F3MXLT = GDMXLT
     C                   EVAL      F3SSCCX = %triml(%editc(GDSSCC:'Z'))
     C                   EVAL      F3TOTE = GDTOTE

     C                   SELECT
     C                   WHEN      GDCNVY = 'C'
     C                   EVAL      F3CNVY = 'Conveyable'
     C                   WHEN      GDCNVY = 'T'
     C                   EVAL      F3CNVY = 'Tote'
     C                   WHEN      GDCNVY = 'S'
     C                   EVAL      F3CNVY = 'Small parcel pick'
     C                   WHEN      GDCNVY = 'P'
     C                   EVAL      F3CNVY = 'Pallet'
     C                   WHEN      GDCNVY = 'D'
     C                   EVAL      F3CNVY = 'DigiPick'
     C                   OTHER
     C                   EVAL      F3CNVY = GDCNVY
     C                   ENDSL

     C                   SELECT
     C                   WHEN      GDSPPK = 'C'
     C                   EVAL      F3SPPK = 'Conveyable'
     C                   WHEN      GDSPPK = 'T'
     C                   EVAL      F3SPPK = 'Tote'
     C                   OTHER
     C                   EVAL      F3SPPK = GDSPPK
     C                   ENDSL

     C                   IF        GDPANS = 'Y'
     C                   EVAL      F3PANS = 'Yes'
     C                   ELSE
     C                   EVAL      F3PANS = GDPANS
     C                   ENDIF                                                  GDPANS='Y'

     C                   EVAL      F3PIKO = %triml(%editc(GDPIKO:'4'))
     C                   EVAL      F3POPP = %triml(%editc(GDPOPP:'4'))

     C                   SELECT
     C                   WHEN      GDQOPK = *Zero
     C                   EVAL      F3STS = *Blanks
     C                   WHEN      GDQSTC = *Zero
     C                   EVAL      F3STS = 'Not yet shipped'
     C                   WHEN      GDQSTC < GDQOPK
     C                   EVAL      F3STS = 'Partially shipped'
     C                   WHEN      TVNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ)
     C                   EVAL      F3STS = 'Tote not yet verified'
     C                   WHEN      LotsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDTURN:GDTSEQ:GDRSEQ:GDSSCCBC:GDSSCC)
     C                   EVAL      F3STS = 'Lot number(s) not yet collected'
     C                   WHEN      LotsRecalled (GDGRP#:GDTURN:GDTSEQ:GDRSEQ)
     C                   EVAL      F3STS = 'Lot(s) recalled'
     C                   WHEN      LblsNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDOCTL:GDORL#:GDBLN#:GDLBL1:GDLBL2:GDLBL3)
     C                   EVAL      F3STS = 'Label(s) not yet printed'
     C                   WHEN      PltzNeeded (GDGRP#:GDBLTO:GDSHTO:GDDSHP:
     C                               GDORD#:GDORL#:GDBLN#:GDSSCC)
     C                   EVAL      F3STS = 'Not yet palletized'
     C                   OTHER
     C                   EVAL      F3STS = 'Complete'
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Enter lot number for non-conveyable item.                              *
      **************************************************************************
     CSR   $LotEntry     BEGSR

     C                   EVAL      wk_Pgm01 = *ON

     C                   CALLP(E)  HYR0604 ('2':wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:S1TURN:S1TSEQ:
     C                               S1SSCCBC:S1RSEQ:wk_Data15:wk_RC0604)

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0604'
     C                   EXSR      $SndPM
     C                   WHEN      wk_RC0604 = '03'
     C                   EVAL      *IN03 = *ON
     C                   EVAL      wk_RtnCode = '03'
     C                   WHEN      wk_RC0604 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0604 = *Zeros
     C                   IF        %subst(wk_Data15:1:4) = 'CNT:'
     C                   EVAL      wk_MsgID = 'MSG0375'
     C                   EVAL      wk_MsgData = %subst(wk_Data15:5:7)
     C                   EVAL      %subst(wk_MsgData:8:15) = S1ITEM
     C                   ELSE
     C                   EVAL      wk_MsgID = 'MSG0374'
     C                   EVAL      wk_MsgData = wk_Data15 + S1ITEM
     C                   ENDIF                                                  %subst(wk_Lot#RJ:1:4
     C                   EXSR      $SndPM
     C                   EVAL      wk_Refresh = *ON
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR6004'
     C                   EXSR      $SndPM
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Ship item via scanning (HYR0604)                                       *
      **************************************************************************
     CSR   $ScanMode     BEGSR

     C                   EVAL      wk_Pgm01 = *ON

      *                   Call HYR0604 in 'ScanMode'
     C                   CALLP(E)  HYR0604 ('3':wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:0:0:
     C                               ' ':0:wk_Data15:wk_RC0604)

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0604'
     C                   EXSR      $SndPM
     C                   WHEN      wk_RC0604 = '03'
     C                   EVAL      *IN03 = *ON
     C                   EVAL      wk_RtnCode = '03'
     C                   WHEN      wk_RC0604 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0604 = *Zeros
     C                   EVAL      wk_Refresh = *ON
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0604'
     C                   EXSR      $SndPM
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Ship item, either non-conveyable item, or item not shipped in BoxLogix.*
      **************************************************************************
     CSR   $Ship         BEGSR

     C                   EVAL      wk_Pgm01 = *ON

     C                   CALLP(E)  HYR0604 ('1':wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:S1TURN:S1TSEQ:
     C                               S1SSCCBC:S1RSEQ:wk_Data15:wk_RC0604)

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0604'
     C                   EXSR      $SndPM
     C                   WHEN      wk_RC0604 = '03'
     C                   EVAL      *IN03 = *ON
     C                   EVAL      wk_RtnCode = '03'
     C                   WHEN      wk_RC0604 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0604 = *Zeros
      /Free
       wk_GDQSTC = *Zero;
       EXEC SQL select GDQSTC, GDLBL1, GDLBL2, GDLBL3
                   into :wk_GDQSTC, :wk_GDLBL1, :wk_GDLBL2, :wk_GDLBL3
                  from HYPSGDT
                  where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                   GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                   GDTURN=:S1TURN and GDTSEQ=:S1TSEQ and GDRSEQ=:S1RSEQ;
       wk_CurShipQty = wk_GDQSTC - S1QSTC;
       S1QSTC = wk_GDQSTC;
       S1QSTCX = wk_GDQSTC;
      /End-Free
     C                   SELECT
     C                   WHEN      S1QOPK = *Zero
     C                   EVAL      S1STS = *Blanks
     C                   WHEN      wk_GDQSTC = *Zero
     C                   EVAL      S1STS = 'Ship'
     C                   WHEN      wk_GDQSTC < S1QOPK
     C                   EVAL      S1STS = 'Prtl'
     C                   WHEN      TVNeeded (wk_ShipGrp#:wk_BillTo#:
     C                              wk_ShipTo#:wk_DropShip#:S1TURN:S1TSEQ:
     C                              S1RSEQ)
     C                   EVAL      S1STS = 'Tote'
     C                   WHEN      LotsNeeded (wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:S1TURN:S1TSEQ:
     C                               S1RSEQ:S1SSCCBC:S1SSCC)
     C                   EVAL      S1STS = 'Lot'
     C                   WHEN      LotsRecalled (wk_ShipGrp#:S1TURN:S1TSEQ:
     C                                           S1RSEQ)
     C                   EVAL      S1STS = 'Recal'
     C                   WHEN      LblsNeeded (wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:S1OCTL:S1ORL#:
     C                               S1BLN#:wk_GDLBL1:wk_GDLBL2:wk_GDLBL3)
     C                   EVAL      S1STS = 'Label'
     C                   WHEN      PltzNeeded (wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:S1ORD#:S1ORL#:
     C                               S1BLN#:S1SSCC)
     C                   EVAL      S1STS = 'Pallt'
     C                   OTHER
     C                   EVAL      S1STS = *Blanks
     C                   ENDSL
     C                   EVAL      wk_MsgID = 'MSG0376'
     C                   EVAL      wk_MsgData = %triml(%editc(S1ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:3) =
     C                               %triml(%editc(S1ORL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:15) = S1ITEM
     C****               EVAL      %subst(wk_MsgData:27:12) =
     C****                           %subst(wk_Data15:5:11)
     C                   EVAL      %subst(wk_MsgData:27:12) =
     C                               %triml(%editc(wk_CurShipQty:'J'))
     C                   EXSR      $SndPM
     C****               EVAL      wk_Refresh = *ON
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0604'
     C                   EXSR      $SndPM
     C                   ENDSL

      *
      * For conveyable shipments (something happened, probably manually -
      * this is unusual), if the HYPSCWK record has not yet been written,
      * write it.
      *
      /Free
       IF S1SSCC <> *Zero;
         wk_GDCNVY = *Blank;
         wk_GDQOPK = *Zero;
         wk_GDQSTC = *Zero;
         wk_GDLBL1 = *Blank;
         wk_GDITEM = *Blanks;
         EXEC SQL select GDCNVY, GDQOPK, GDQSTC, GDLBL1,
                         GDITEM
                   into :wk_GDCNVY, :wk_GDQOPK, :wk_GDQSTC, :wk_GDLBL1,
                        :wk_GDITEM
                  from HYPSGDT
                  where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                        GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                        GDTURN=:S1TURN and GDTSEQ=:S1TSEQ and
                        GDSSCCBC=:S1SSCCBC and GDRSEQ=:S1RSEQ;
         wk_RcdCnt = *Zero;
         IF wk_GDCNVY='C'  and  wk_GDQSTC = wk_GDQOPK and wk_GDLBL1 = 'Y';
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPSCWK
                    where S1ORD#=:S1ORD# and S1ORL#=:S1ORL# and
                     S1BLN#=:S1BLN# and S1SRL#=:S1SSCC;
      // ENDIF;   Moved down 40 lines - was creating zero HYPELD rcds ****   // wk_GDCNVY='C' and
         IF wk_RcdCnt = *Zero;
           EXEC SQL delete from HYW0602
                    where W1JOB#=:ps_Job#;
           EXEC SQL insert into HYW0602
                           (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                            W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                            W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                    values(:ps_Job#, :wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                           :wk_DropShip#, :S1TURN, :S1TSEQ, :S1RSEQ,
                           :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);
           wk_IMUDN2 = *Zero;
           EXEC SQL select IMUDN2 into :wk_IMUDN2
                    from HDIMST
                    where IMITEM=:wk_GDITEM;
           wk_IWPCPC = *Zero;
           EXEC SQL select IWPCPC into :wk_IWPCPC
                    from HDIWHS
                    where IWITEM=:wk_GDITEM and IWWHS=:wk_IMUDN2;
           IF wk_GDQSTC = wk_IWPCPC;
           wk_LblType = 'F';
           ELSE;
           wk_LblType = 'P';
           ENDIF;                                                            // wk_GDQSTC=wk_IWPCPC
           wk_SSCC = S1SSCC;
           IF wk_GDQSTC = wk_IWPCPC;
           EXSR $WrtHYPSCWK;
           ELSE;
             wk_SSCCBC = S1SSCCBC;
           ENDIF;                                                            // wk_GDQSTC=wk_IWPCPC
           wk_Pgm02 = *ON;
       // **** 9/28/19 - Should the following be an "E" type label request????
           wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:
                                     wk_SSCCBC:'E':*ON:*OFF:0:*Blanks);
           IF wk_RC0622 = 2  or  wk_RC0622 = 4;
             EXEC SQL update HYPSGDT a
                       set GDLBL2='Y'
                      where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                       a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                       a.GDSSCCBC=:wk_SSCCBC and a.GDRSEQ=:S1RSEQ;
           ENDIF;                                                            // wk_RC0622=2 or
         ENDIF;
         ENDIF;                                                              // wk_GDCNVY='C' and
       ENDIF;                                                                // S1SSCC<>*Zero
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Tote verify.                                                           *
      **************************************************************************
     CSR   $ToteVfy      BEGSR

      /Free
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGTC
                where TCGRP#=:wk_ShipGrp# and TCBLTO=:wk_BillTo# and
                      TCSHTO=:wk_ShipTo# and TCDSHP=:wk_DropShip# and
                      TCTOTE=:S1TOTE;
       IF wk_RcdCnt = *Zero;
         wk_ToteClosed = *OFF;
       ELSE;
         wk_ToteClosed = *ON;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free

     C                   IF        wk_ToteClosed
     C                   EVAL      W1TXT1 = 'Tote ' + %trimr(S1TOTE) +
     C                               ' has been fully verified'
     C                   EVAL      W1TXT2 = 'and is closed to this shipment.'
     C                   EVAL      W1TXT3 = 'Press F16 to re-open the tote for +
     C                               this'
     C                   EVAL      W1TXT4 = 'shipment.'
     C                   EVAL      W1MSG = *Blanks
     C                   EVAL      *IN90 = *OFF

     C                   DOU       *IN12

     C                   EXFMT     WINDOW01C

     C                   EVAL      W1MSG = *Blanks
     C                   EVAL      *IN90 = *OFF

     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

     C                   IF        not *IN16
     C                   EVAL      W1MSG = 'Press F16 to re-open the tote.'
     C                   EVAL      *IN90 = *ON
     C                   ITER
     C                   ENDIF                                                  not *IN16

     C                   LEAVE

     C                   ENDDO                                                  *IN12

     C                   ENDIF                                                  wk_ToteClosed

      /Free
       wk_CustCnt = *Zero;
       EXEC SQL select count(distinct digits(a.GDGRP#) || digits(a.GDBLTO) ||
                                      digits(a.GDSHTO) || digits(a.GDDSHP))
                 into :wk_CustCnt
                from HYPSGDT a
                where a.GDTOTE=:S1TOTE and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP# and
                 b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP
                 and b.GCSTS='O') and
                 ((a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip#) or
                 not exists (select * from HYPSGTC c where c.TCGRP#=a.GDGRP#
                 and c.TCBLTO=a.GDBLTO and c.TCSHTO=a.GDSHTO and
                 c.TCDSHP=a.GDDSHP and c.TCTOTE=a.GDTOTE));
      /End-Free

     C                   EVAL      wk_Pgm04 = *ON

     C                   IF        wk_CustCnt = 1
     C                   CALLP(E)  HYR0608 ('1':wk_ShipGrp#:wk_BillTo#:
     C                              wk_ShipTo#:wk_DropShip#:S1TOTE:wk_RC0608)
     C                   ELSE
     C                   CALLP(E)  HYR0608 ('2':*Zero:*Zero:
     C                               *Zero:*Zero:S1TOTE:wk_RC0608)
     C                   ENDIF                                                  wk_CustCnt=1

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0608'
     C                   EXSR      $SndPM
     C                   WHEN      wk_RC0608 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0608 = *Zeros
     C                   EVAL      wk_Refresh = *ON
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0608'
     C                   EXSR      $SndPM
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shipment lot inquiry.                                                  *
      **************************************************************************
     CSR   $LotInq       BEGSR

     C                   EVAL      wk_Pgm03 = *ON

     C                   CALLP(E)  HYR0606 ('2':wk_ShipGrp#:S1TURN:
     C                               S1TSEQ:S1RSEQ:wk_RC0606)

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0606'
     C                   EXSR      $SndPM
     C                   WHEN      wk_RC0606 = '03'
     C                   EVAL      *IN03 = *ON
     C                   EVAL      wk_RtnCode = '03'
     C                   WHEN      wk_RC0606 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0606 = *Zeros
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0606'
     C                   EXSR      $SndPM
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Prompt for employee number.                                            *
      **************************************************************************
     CSR   $PromptEmply  BEGSR

     C                   EXSR      $InitIndW3

     C                   EVAL      W3EMPL = *Zero
     C                   EVAL      W3MSG = *Blanks

     C                   DOU       *IN12

     C                   EXFMT     WINDOW03

     C                   EVAL      W3MSG = *Blanks
     C                   EXSR      $InitIndW3

     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

     C                   EXSR      $EditW3
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   EVAL      %subst(wk_LDA:36:5) = 'EMPL:'
     C                   EVAL      %subst(wk_LDA:41:5) = %editc(W3EMPL:'X')
     C                   OUT       wk_LDA

     C                   EVAL      wk_EMPL = W3EMPL

      /Free
       wk_EMFNAM = *Blanks;
       wk_EMLNAM = *Blanks;
       EXEC SQL select EMFNAM, EMLNAM into :wk_EMFNAM, :wk_EMLNAM
                from HREMPL
                where EMEMPL=:wk_EMPL;
      /End-Free
     C                   EVAL      C1ENAM = 'Shipper: ' +
     C                               %subst(wk_EMFNAM:1:1) + ' ' + wk_EMLNAM

     C                   LEAVE

     C                   ENDDO                                                  *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit employee number prompt window fields.                             *
      **************************************************************************
     CSR   $EditW3       BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Employee number is required and must be valid.
      *
     C                   IF        W3EMPL = *Zero
     C                   IF        not wk_Error
     C                   EVAL      W3MSG = 'Employee number is required.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HREMPL
                where EMEMPL=:W3EMPL;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   IF        not wk_Error
     C                   EVAL      W3MSG = 'Employee number ' +
     C                               %triml(%editc(W3EMPL:'Z')) + ' is invalid.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  W3EMPL=*Zero

     C                   IF        wk_Error
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize employee number prompt window fields.                       *
      **************************************************************************
     CSR   $InitIndW3    BEGSR

     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN90 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Populate function key descriptions for available functions.            *
      **************************************************************************
     CSR   $FuncKey      BEGSR

     C                   EVAL      C1FKY1 = F3Txt + '   ' + F5Txt
     C                   IF        wk_PgmMode = MaintMode  and
     C                              wk_PackLane = '999'
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F6Txt
     C                   ELSE
     C                   IF        wk_PgmMode = MaintMode  and  not wk_ToteMode
     C                   IF        wk_ToteVfy
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F7Txt
     C                   ELSE
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F7TxtB
     C                   ENDIF                                                  wk_ToteVfy
     C                   ENDIF                                                  wk_PgmMode=MaintMode
     C                   ENDIF                                                  wk_PgmMode=MaintMode
     C                   IF        wk_PgmMode = MaintMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F8Txt +
     C                               '   ' + F9Txt
     C                   ENDIF                                                  wk_PgmMode=MaintMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F12Txt +
     C                               '   ' + F17Txt + '   ' + F18Txt
     C                   IF        not wk_ToteMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F20Txt
     C                   ENDIF                                                  not wk_ToteMode

     C                   IF        wk_PgmMode = MaintMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F22Txt
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C****               IF        wk_PgmMode = MaintMode
     C****               EVAL      C1FKY1 = F3Txt + '   ' + F5Txt + '   ' +
     C****                           F8Txt + '   ' + F12Txt + '   ' + F17Txt +
     C****                           '   ' + F18Txt + '   ' + F20Txt
     C****               ELSE
     C****               EVAL      C1FKY1 = F3Txt + '   ' + F5Txt + '   ' +
     C****                           F12Txt + '   ' + F17Txt + '   ' + F18Txt +
     C****                           '   ' + F20Txt
     C****               ENDIF                                                  wk_PgmMode=MaintMode

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR

     C                   IF        wk_Pgm01
     C                   CALLP(E)  HYR0604 ('9':wk_ShipGrp#:wk_BillTo#:
     C                               wk_ShipTo#:wk_DropShip#:*Zero:*Zero:
     C                               *Blanks:*Zero:wk_Data15:wk_RC0604)
     C                   ENDIF                                                  wk_Pgm01

     C                   IF        wk_Pgm03
     C                   CALLP(E)  HYR0606 ('9':wk_ShipGrp#:*Zero:
     C                               *Zero:*Zero:wk_RC0606)
     C                   ENDIF                                                  wk_Pgm03

     C                   IF        wk_Pgm04
     C                   CALLP(E)  HYR0608 ('9':wk_ShipGrp#:*Zero:
     C                              *Zero:*Zero:*Blanks:wk_RC0608)
     C                   ENDIF                                                  wk_Pgm04

     C                   IF        wk_Pgm05
     C                   CALLP(E)  HYR0626 ('9':wk_ShipGrp#:*Zero:
     C                                      *Zero:*Zero:*Blank:
     C                                      wk_RC0626)
     C                   ENDIF                                                  wk_Pgm05

     C                   IF        wk_Pgm02
     C                   CALLP(E)  sd_HYR0622
     C                   ENDIF                                                  wk_Pgm02

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - each call.                                    *
      **************************************************************************
     CSR   $InzSR        BEGSR

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode
     C                   EVAL      C1HDG1 = MaintHdg
     C                   EVAL      F1HDG1 = MaintHdg
     C                   EVAL      C1OPTS = MaintOpts
     C                   EVAL      wk_AlwF22 = *ON
     C                   WHEN      wk_PgmMode = InqMode
     C                   EVAL      C1HDG1 = InqHdg
     C                   EVAL      F1HDG1 = InqHdg
     C                   EVAL      C1OPTS = InqOpts
     C                   EVAL      wk_AlwF22 = *OFF
     C                   ENDSL
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   EVAL      F1HDG1 = center(F1HDG1)
     C                   EVAL      C1HDG2 = *Blanks

     C                   EXSR      $InitSS1
     C                   EXSR      $SaveSS1

     C                   EVAL      wk_Sort = '1'

     C****               EXSR      $FuncKey

     C                   EVAL      C1GRP# = %triml(%editc(wk_ShipGrp#:'Z'))

      /Free
       wk_GCSTNM = *Blanks;
       wk_GCSTYP = *Blank;
       EXEC SQL select GCSTNM, GCSTYP into :wk_GCSTNM, :wk_GCSTYP
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;
      /End-Free

     C                   EVAL      C1SHTO = '#' +
     C                               %triml(%editc(wk_ShipTo#:'Z')) + '-' +
     C                               wk_GCSTNM
     C                   IF        wk_DropShip# <> *Zero
     C                   EVAL      %subst(C1SHTO:39:5) = ' (DS)'
     C                   ENDIF                                                  wk_DropShip#<>*Zero

     C                   EVAL      C1NOTE = *Blanks

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                      a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                      exists (select * from OEORDT b join HDIWHS c on
                      b.ODITEM=c.IWITEM and b.ODWH=c.IWWHS where
                      b.ODORD#=a.GDORD# and b.ODORL#=a.GDORL# and
                      b.ODBLN#=a.GDBLN# and c.IWFTCL='AMMOB');
       IF wk_RcdCnt > *Zero;
         %subst(C1NOTE:6:7) = x'32' + 'AMMOB' + x'20';
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C                   IF        wk_PgmMode = MaintMode
     C                   IN        wk_LDA
     C                   IF        %subst(wk_LDA:36:5) = 'EMPL:'
     C                   EVAL      wk_EMPL = %dec(%subst(wk_LDA:41:5):5:0)
      /Free
       wk_EMFNAM = *Blanks;
       wk_EMLNAM = *Blanks;
       EXEC SQL select EMFNAM, EMLNAM into :wk_EMFNAM, :wk_EMLNAM
                from HREMPL
                where EMEMPL=:wk_EMPL;
      /End-Free
     C                   EVAL      C1ENAM = 'Shipper: ' +
     C                               %subst(wk_EMFNAM:1:1) + ' ' + wk_EMLNAM
     C                   ELSE
     C                   EVAL      wk_EMPL = *Zero
     C                   EVAL      C1ENAM = *Blanks
     C                   ENDIF                                                  %subst(wk_LDA:36:5)=
     C                   ELSE
     C                   EVAL      wk_EMPL = *Zero
     C                   EVAL      C1ENAM = *Blanks
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   IN        wk_LDA
     C                   IF        %subst(wk_LDA:93:8) = '*TOTEVFY'
     C                   EVAL      wk_ToteVfy = *ON
     C                   EVAL      C1OPTS = MaintOptsTV
     C                   ELSE
     C                   EVAL      wk_ToteVfy = *OFF
     C                   ENDIF                                                  %subst(wk_LDA:93:8)

     C                   EXSR      $FuncKey

     C                   EVAL      wk_DspView = '1'
     C                   EVAL      *IN03 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - first time only.                              *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define  Key Lists
      *
     C     key_GD        KLIST                                                  HYPSGDT
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    S1TURN
     C                   KFLD                    S1TSEQ
     C                   KFLD                    S1SSCCBC

     C     key_GDx       KLIST                                                  HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#

     C     key_GD1       KLIST                                                  HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    C1TURN

     C     key_GD2       KLIST                                                  HYLSGDT4
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    C1ORD#

     C     key_GD3       KLIST                                                  HYLSGDT5
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    C1ITEM

     C     key_W1        KLIST                                                  HYW0602
     C                   KFLD                    ps_Job#
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#

     C     key_LD        KLIST                                                  HYPSGLD
     C                   KFLD                    W1GRP#
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ

     C     RFSKEY1       KLIST                                                  SFL01 - HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UTURN
     C                   KFLD                    @UITEM
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

     C     PGUKEY1       KLIST                                                  SFL01 - HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UTURN
     C                   KFLD                    @UITEM
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

     C     PGDKEY1       KLIST                                                  SFL01 - HYLSGDT3
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @DTURN
     C                   KFLD                    @DITEM
     C                   KFLD                    @DORD#
     C                   KFLD                    @DORL#
     C                   KFLD                    @DBLN#
     C                   KFLD                    @DRSEQ
     C                   KFLD                    @DSSCCBC

     C     RFSKEY2       KLIST                                                  SFL01 - HYLSGDT4
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

     C     PGUKEY2       KLIST                                                  SFL01 - HYLSGDT4
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

     C     PGDKEY2       KLIST                                                  SFL01 - HYLSGDT4
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @DORD#
     C                   KFLD                    @DORL#
     C                   KFLD                    @DBLN#
     C                   KFLD                    @DRSEQ
     C                   KFLD                    @DSSCCBC

     C     PGDKEY3       KLIST                                                  SFL01 - HYLSGDT5
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @DITEM
     C                   KFLD                    @DTURN
     C                   KFLD                    @DORD#
     C                   KFLD                    @DORL#
     C                   KFLD                    @DBLN#
     C                   KFLD                    @DRSEQ
     C                   KFLD                    @DSSCCBC

     C     PGUKEY3       KLIST                                                  SFL01 - HYLSGDT5
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UITEM
     C                   KFLD                    @UTURN
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

     C     RFSKEY3       KLIST                                                  SFL01 - HYLSGDT5
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    @UITEM
     C                   KFLD                    @UTURN
     C                   KFLD                    @UORD#
     C                   KFLD                    @UORL#
     C                   KFLD                    @UBLN#
     C                   KFLD                    @URSEQ
     C                   KFLD                    @USSCCBC

      *
      * Initialize Work Fields
      *
     C                   EVAL      S@PGMQ = '*'
     C                   EVAL      S@RCNO = *Zero
     C                   EVAL      P@PGNM = ps_Pgnm
     C                   EVAL      P@JOBN = ps_Job
     C                   EVAL      P@USER = ps_User

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'
     C                   EVAL      C1ENV = *Blanks
     C                   EVAL      F1ENV = *Blanks
     C                   WHEN      wk_DatabaseID = 'SS'
     C                   EVAL      C1ENV = 'SS'
     C                   EVAL      F1ENV = 'SS'
     C                   OTHER
     C                   EVAL      C1ENV = '*TEST*'
     C                   EVAL      F1ENV = '*TEST*'
     C                   ENDSL

     C                   IF        ps_User = 'BEARL'
     C                   EVAL      wk_SuperUser = *ON
     C                   ELSE
     C                   EVAL      wk_SuperUser = *OFF
     C                   ENDIF                                                  ps_User='BEARL'

     CSR                 ENDSR

     P/EJECT
      **************************************************************************
      * Determine whether detail record can be selected for current function.  *
      **************************************************************************
     P SelOK           B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D SSCCBC                        20A   CONST
     D Order#                         8S 0 CONST
     D Item#                         15A   CONST
     D LblType                        1A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_Order#       S                   LIKE(GDORD#)
     D wk_Item#        S                   LIKE(GDITEM)
     D wk_LblType      S              1A
     D wk_RtnCode      S              2A
     D wk_GDLOT        S                   LIKE(GDLOT)
     D wk_GDQSTC       S                   LIKE(GDQSTC)
     D wk_RcdCnt       S              7P 0
     D wk_LDQSTC       S                   LIKE(LDQSTC)

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_LblType = LblType
     C                   EVAL      wk_RtnCode = *Zeros

      *
      * 01-UCC128 label has already been printed.
      *
     C                   IF        wk_SSCCBC <> *Blanks
     C                   EVAL      wk_RtnCode = '01'
     C                   LEAVE
     C                   ENDIF                                                  wk_SSCCBC<>*Blanks

      /Free
       //
       // 02-Lot number has not yet been entered.
       //
       wk_GDLOT = *Blanks;
       wk_GDQSTC = *Zero;
       EXEC SQL select GDLOT, GDQSTC into :wk_GDLOT, :wk_GDQSTC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDSSCCBC=' ';
       IF wk_GDLOT <> *Blanks  and  wk_GDLOT <> *Zeros;
         wk_RcdCnt = *Zero;
         wk_LDQSTC = *Zero;
         EXEC SQL select count(*), coalesce(sum(LDQSTC),0)
                   into :wk_RcdCnt, :wk_LDQSTC
                  from HYPSGLD
                  where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                        LDTSEQ=:wk_TurnSeq# and LDSSCC=0;
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC < wk_GDQSTC;
           wk_RtnCode = '02';
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt=*Zero or
       ENDIF;                                                                // wk_GDLOT<>*Blanks
      /End-Free

      *
      *   03-All items selected must on the same order.
      *
     C                   IF        wk_Order# <> wk_SelOrder#
     C                               and  wk_SelOrder# <> *Zero
     C                   EVAL      wk_RtnCode = '03'
     C                   LEAVE
     C                   ENDIF                                                  wk_Order#<>wk_SelOrd

      *
      *   04-All items selected for a partial carton label must be the same.
      *
     C                   IF        wk_LblType = 'P'                             Partial Label
     C                   IF        wk_Item# <> wk_SelItem#
     C                               and  wk_SelItem# <> *Blanks
     C                   EVAL      wk_RtnCode = '04'
     C                   LEAVE
     C                   ENDIF                                                  wk_Item#<>wk_SelItem
     C                   ENDIF                                                  wk_LblType='P'

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P SelOK           E

     P/EJECT
      **************************************************************************
      * Determine whether detail record can be changed.                        *
      **************************************************************************
     P ChgOK           B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D SSCCBC                        20A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_RtnCode      S              2A
     D wk_GDBLTO       S              7S 0
     D wk_GDSHTO       S              7S 0
     D wk_GDDSHP       S              7S 0
     D wk_GCSTS        S              1A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       wk_GDBLTO = *Zero;
       wk_GDSHTO = *Zero;
       wk_GDDSHP = *Zero;
       EXEC SQL select GDBLTO, GDSHTO, GDDSHP
                 into :wk_GDBLTO, :wk_GDSHTO, :wk_GDDSHP
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDSSCCBC=:wk_SSCCBC;

       //
       // 01-Customer shipment has been completed.
       //
       wk_GCSTS = *Blank;
       EXEC SQL select GCSTS into :wk_GCSTS
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_GDBLTO and
                      GCSHTO=:wk_GDSHTO and GCDSHP=:wk_GDDSHP;
       IF wk_GCSTS <> 'O';
         wk_RtnCode = '01';
         LEAVE;
       ENDIF;                                                                // wk_GCSTS='C'
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P ChgOK           E

     P/EJECT
      **************************************************************************
      * Determine whether a lot number can be entered for detail record.       *
      **************************************************************************
     P LotEntryOK      B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST
     D SSCCBC                        20A   CONST
     D SSCC                           9S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_SSCC         S                   LIKE(GDSSCC)
     D wk_RtnCode      S              2A
     D wk_GDIMDS       S                   LIKE(GDIMDS)
     D wk_GDLOT        S                   LIKE(GDLOT)
     D wk_GDQSTC       S                   LIKE(GDQSTC)
     D wk_RcdCnt       S              7P 0
     D wk_LDQSTC       S             13P 4
     D wk_GDCNVY       S              1A
     D wk_GDSPPK       S              1A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_SSCC = SSCC
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       //
       // 03-Lot number is not required.
       //
       wk_GDIMDS = *Blanks;
       wk_GDLOT = *Blanks;
       EXEC SQL select GDIMDS, GDLOT into :wk_GDIMDS, :wk_GDLOT
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDRSEQ=:wk_RcdSeq#
                      fetch first row only;
       IF wk_GDLOT = *Zeros  and
            %subst(wk_GDIMDS:1:5) <> 'SAMMO'  and
            %subst(wk_GDIMDS:1:5) <> 'SFRON';
         wk_RtnCode = '03';
         LEAVE;
       ENDIF;                                                                // wk_GDLOT=*Zeros

       //
       // 01-Item record has not yet been shipped.
       // 02-Lot number has already been entered.
       //
       wk_GDQSTC = *Zero;
       EXEC SQL select sum(GDQSTC) into :wk_GDQSTC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDRSEQ=:wk_RcdSeq#;
       IF wk_GDQSTC = *Zero;
         wk_RtnCode = '01';
         LEAVE;
       ENDIF;                                                                // wk_GDQSTC=*Zero
       wk_RcdCnt = *Zero;
       wk_LDQSTC = *Zero;
       EXEC SQL select count(*), coalesce(sum(LDQSTC),0)
                 into :wk_RcdCnt, :wk_LDQSTC
                from HYPSGLD
                where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                      LDTSEQ=:wk_TurnSeq# and LDRSEQ=:wk_RcdSeq#;
       IF wk_RcdCnt > *Zero  and  wk_LDQSTC >= wk_GDQSTC;
         wk_RtnCode = '02';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       //
       // 04-Tote lot numbers must be entered via Tote Verify (if Tote
       //    Verify is enabled).
       //
       IF wk_ToteVfy;
         wk_GDCNVY = *Blank;
         wk_GDSPPK = *Blank;
         EXEC SQL select GDCNVY, GDSPPK
                   into :wk_GDCNVY, :wk_GDSPPK
                  from HYPSGDT
                  where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                        GDTSEQ=:wk_TurnSeq# and GDRSEQ=:wk_RcdSeq#;
         IF wk_GDCNVY = 'T'  or  (wk_GDCNVY = 'S' and wk_GDSPPK = 'T');
           wk_RtnCode = '04';
           LEAVE;
         ENDIF;                                                              // wk_GDCNVY='T' or
       ENDIF;                                                                // wk_ToteVfy
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P LotEntryOK      E

     P/EJECT
      **************************************************************************
      * Determine whether detail record can be selected for shipping.          *
      **************************************************************************
     P ShipOK          B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST
     D SSCCBC                        20A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_RtnCode      S              2A
     D wk_GDBLTO       S              7S 0
     D wk_GDSHTO       S              7S 0
     D wk_GDDSHP       S              7S 0
     D wk_GDORD#       S              8S 0
     D wk_GDORL#       S              3S 0
     D wk_GDBLN#       S              3S 0
     D wk_GDCNVY       S              1A
     D wk_GDSPPK       S              1A
     D wk_GDSSCC       S              9S 0
     D wk_GCSTS        S              1A
     D wk_ODPO#        S              8S 0
     D wk_ODDSHP       S              1A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       wk_GDBLTO = *Zero;
       wk_GDSHTO = *Zero;
       wk_GDDSHP = *Zero;
       wk_GDORD# = *Zero;
       wk_GDORL# = *Zero;
       wk_GDBLN# = *Zero;
       wk_GDCNVY = *Blank;
       wk_GDSPPK = *Blank;
       wk_GDSSCC = *Zero;
       EXEC SQL select GDBLTO, GDSHTO, GDDSHP, GDORD#,
                       GDORL#, GDBLN#, GDCNVY, GDSPPK,
                       GDSSCC
                 into :wk_GDBLTO, :wk_GDSHTO, :wk_GDDSHP, :wk_GDORD#,
                      :wk_GDORL#, :wk_GDBLN#, :wk_GDCNVY, :wk_GDSPPK,
                      :wk_GDSSCC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDRSEQ=:wk_RcdSeq#;

       //
       // 01-Customer shipment has been completed.
       //
       wk_GCSTS = *Blank;
       EXEC SQL select GCSTS into :wk_GCSTS
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_GDBLTO and
                      GCSHTO=:wk_GDSHTO and GCDSHP=:wk_GDDSHP;
       IF wk_GCSTS <> 'O';
         wk_RtnCode = '01';
         LEAVE;
       ENDIF;                                                                // wk_GCSTS='C'

       //
       // 02-Order line is linked to a P.O.
       //
       wk_ODPO# = *Zero;
       wk_ODDSHP = *Blank;
       EXEC SQL select ODPO#, ODDSHP into :wk_ODPO#, :wk_ODDSHP
                from OEORDT
                where ODORD#=:wk_GDORD# and ODORL#=:wk_GDORL# and
                      ODBLN#=:wk_GDBLN#;
       IF wk_ODPO# <> *Zero  and  wk_ODDSHP = 'Y';
         wk_RtnCode = '02';
         LEAVE;
       ENDIF;                                                                // wk_ODPO#<>*Zero and

       //
       // 03-Label has already been printed.
       //
          // Following commented is code that should be included, but conveyable
          // labels currently can't be deleted.
       // IF ((wk_GDCNVY = 'C'  or  (wk_GDCNVY = 'S' and wk_GDSPPK='C'))  and
       //     wk_GDLBL1 = 'Y')  or
       IF ((wk_GDCNVY <> 'C'  and
                              (wk_GDCNVY <> 'S'  or  wk_GDSPPK <> 'C'))  and
            wk_GDSSCC <> *Zero);
         wk_RtnCode = '03';
         LEAVE;
       ENDIF;
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P ShipOK          E

     P/EJECT
      **************************************************************************
      * Determine whether detail record can be selected to create a label.     *
      **************************************************************************
     P LblOK           B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST
     D SSCCBC                        20A   CONST
     D SSCC                           9S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_SSCC         S                   LIKE(GDSSCC)
     D wk_RtnCode      S              2A
     D wk_GDQOPK       S             13P 4
     D wk_GDQSTC       S             13P 4
     D wk_GDLOT        S              2A
     D wk_LDQSTC       S             13P 4
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_SSCC = SSCC
     C                   EVAL      wk_RtnCode = *Zeros

      *
      * If this is a reprint, nothing to check - leave the subprocedure.
      *
     C****               IF        wk_SSCCBC <> *Blanks
     C****               LEAVE
     C****               ENDIF                                                  wk_SSCCBC<>*Blanks

      /Free
       wk_GDQOPK = *Zero;
       wk_GDQSTC = *Zero;
       wk_GDLOT = *Blanks;
       EXEC SQL select GDQOPK, GDQSTC, GDLOT
                 into :wk_GDQOPK, :wk_GDQSTC, :wk_GDLOT
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDRSEQ=:wk_RcdSeq#;

      *
      * If this is a reprint and the quantity shipped > *zero, nothing to
      * check - leave the subprocedure.
      *
       IF wk_SSCCBC <> *Blanks  and  wk_GDQSTC>0;
         LEAVE;
       ENDIF;                                                                // wk_SSCCBC<>*Blanks

       //
       // 01-Item must be shipped before labels can be generated.
       //
       IF wk_GDQSTC = *Zero  or  wk_GDQSTC < wk_GDQOPK;
         wk_RtnCode = '01';
         LEAVE;
       ENDIF;                                                                // wk_GDQSTC=*Zero or

       //
       // 02-Lot numbers, if required, must be entered before labels
       // can be generated.
       //
       IF wk_GDLOT <> '00';
         wk_LDQSTC = *Zero;
         EXEC SQL select coalesce(sum(LDQSTC),0), count(*)
                   into :wk_LDQSTC, :wk_RcdCnt
                  from HYPSGLD
                  where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                        LDTSEQ=:wk_TurnSeq# and LDRSEQ=:wk_RcdSeq#;
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC <> wk_GDQSTC;
           wk_RtnCode = '02';
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt=*Zero
       ENDIF;                                                                // wk_Lot<>'00'
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P LblOK           E

     P/EJECT
      **************************************************************************
      * Determine whether tote verify is available.                            *
      **************************************************************************
     P ToteVfyOK       B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D Tote                          10A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_Tote         S                   LIKE(GDTOTE)
     D wk_RtnCode      S              2A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Tote = Tote
     C                   EVAL      wk_RtnCode = *Zeros

     C                   IF        wk_Tote = *Blanks
     C                   EVAL      wk_RtnCode = '01'
     C                   LEAVE
     C                   ENDIF                                                  wk_Tote=*Blanks

     C                   IF        wk_Tote = '*MULT'
     C                   EVAL      wk_RtnCode = '02'
     C                   LEAVE
     C                   ENDIF                                                  wk_Tote='*MULT'

     C                   IF        %subst(wk_Tote:1:3) = 'PLT'
     C                   EVAL      wk_RtnCode = '04'
     C                   LEAVE
     C                   ENDIF                                                  %subst(wk_Tote:1:3)=

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDTOTE=:wk_Tote and
                 a.GDQSTC>(select coalesce(sum(b.TSQSCN),0)
                 from HYPSGTS b where b.TSGRP#=a.GDGRP# and
                 b.TSTURN=a.GDTURN and b.TSTSEQ=a.GDTSEQ and
                 b.TSRSEQ=a.GDRSEQ);
       IF wk_RcdCnt = *Zero;
         wk_RtnCode = '03';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /END-FREE

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P ToteVfyOK       E

     P/EJECT
      **************************************************************************
      * Determine whether label can be deleted.                                *
      **************************************************************************
     P DltLblOK        B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D SSCCBC                        20A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_RtnCode      S              2A
     D wk_GDBLTO       S              7S 0
     D wk_GDSHTO       S              7S 0
     D wk_GDDSHP       S              7S 0
     D wk_GDCNVY       S              1A
     D wk_GCSTS        S              1A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       //
       // 01-Label has not yet been created.
       //
       IF wk_SSCCBC = *Blanks;
         wk_RtnCode = '01';
         LEAVE;
       ENDIF;                                                                // wk_SSCCBC=*Blanks

       //
       // 02-Conveyable carton label cannot be deleted.
       //
       wk_GDBLTO = *Zero;
       wk_GDSHTO = *Zero;
       wk_GDDSHP = *Zero;
       wk_GDCNVY = *Blank;
       EXEC SQL select GDBLTO, GDSHTO, GDDSHP, GDCNVY
                 into :wk_GDBLTO, :wk_GDSHTO, :wk_GDDSHP, :wk_GDCNVY
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                      GDTSEQ=:wk_TurnSeq# and GDSSCCBC=:wk_SSCCBC;
       IF wk_GDCNVY = 'C';
         wk_RtnCode = '02';
         LEAVE;
       ENDIF;                                                                // wk_GDCNVY='C'

       //
       // 03-Customer shipment has been completed.
       //
       wk_GCSTS = *Blank;
       EXEC SQL select GCSTS into :wk_GCSTS
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_GDBLTO and
                      GCSHTO=:wk_GDSHTO and GCDSHP=:wk_GDDSHP;
       IF wk_GCSTS <> 'O';
         wk_RtnCode = '03';
         LEAVE;
       ENDIF;                                                                // wk_GCSTS='C'
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P DltLblOK        E

     P/EJECT
      **************************************************************************
      * Determine whether lot number entry is required.                        *
      **************************************************************************
     P LotsNeeded      B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST
     D SSCCBC                        20A   CONST
     D SSCC                           9S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_SSCC         S                   LIKE(GDSSCC)
     D wk_LotsNeeded   S              1N
     D wk_GDIMDS       S             30A
     D wk_GDLOT        S              2A
     D wk_GDQSTC       S             13P 4
     D wk_LDQSTC       S             13P 4
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_SSCC = SSCC
     C                   EVAL      wk_LotsNeeded = *OFF

      /Free
       wk_GDIMDS = *Blanks;
       wk_GDLOT = *Blanks;
       wk_GDQSTC = *Zero;
       EXEC SQL select GDIMDS, GDLOT, GDQSTC
                 into :wk_GDIMDS, :wk_GDLOT, :wk_GDQSTC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDTURN=:wk_Turn# and GDTSEQ=:wk_TurnSeq# and
                 GDRSEQ=:wk_RcdSeq# and GDSSCCBC=:wk_SSCCBC;
       IF wk_GDLOT <> *Blanks  and
           (wk_GDLOT <> *Zeros or
            %subst(wk_GDIMDS:1:5)='SAMMO' or
            %subst(wk_GDIMDS:1:5)='SFRON');
         wk_RcdCnt = *Zero;
         wk_LDQSTC = *Zero;
         EXEC SQL select count(*), coalesce(sum(LDQSTC),0)
                   into :wk_RcdCnt, :wk_LDQSTC
                  from HYPSGLD
                  where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                        LDTSEQ=:wk_TurnSeq# and LDRSEQ=:wk_RcdSeq#;
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC < wk_GDQSTC;
           wk_LotsNeeded = *ON;
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt=*Zero or
       ENDIF;                                                                // wk_GDLOT<>*Blanks
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_LotsNeeded

     P LotsNeeded      E

     P/EJECT
      **************************************************************************
      * Determine whether any lots have been recalled.                         *
      **************************************************************************
     P LotsRecalled    B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_LotsRecalld  S              1N
     D wk_GDQSTC       S             13P 4
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_LotsRecalld = *OFF

      /Free
       wk_GDQSTC = *Zero;
       EXEC SQL select GDQSTC
                 into :wk_GDQSTC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                 GDTSEQ=:wk_TurnSeq# and GDRSEQ=:wk_RcdSeq#;
       IF wk_GDQSTC = *Zero;
         LEAVE;
       ENDIF;                                                                // wk_GDQSTC=*Zero

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGLD a
                where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                 LDTSEQ=:wk_TurnSeq# and LDRSEQ=:wk_RcdSeq# and
                 exists (select * from HYPRCLT b where
                 b.RLLOT=a.LDLOT#);
       IF wk_RcdCnt > *Zero;
         wk_LotsRecalld = *ON;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero or
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_LotsRecalld

     P LotsRecalled    E

     P/EJECT
      **************************************************************************
      * Determine whether labels have not yet been generated.                 *
      **************************************************************************
     P LblsNeeded      B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D OrdCtl#                        8S 0 CONST
     D Line#                          3S 0 CONST
     D Blanket#                       3S 0 CONST
     D Lbl1                           1A   CONST
     D Lbl2                           1A   CONST
     D Lbl3                           1A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_OrdCtl#      S                   LIKE(GDOCTL)
     D wk_Line#        S                   LIKE(GDORL#)
     D wk_Blanket#     S                   LIKE(GDBLN#)
     D wk_Lbl1         S                   LIKE(GDLBL1)
     D wk_Lbl2         S                   LIKE(GDLBL2)
     D wk_Lbl3         S                   LIKE(GDLBL3)
     D wk_LblsNeeded   S              1N

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_OrdCtl# = OrdCtl#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_Lbl1 = Lbl1
     C                   EVAL      wk_Lbl2 = Lbl2
     C                   EVAL      wk_Lbl3 = Lbl3
     C                   EVAL      wk_LblsNeeded = *OFF

     C                   IF        wk_Lbl1 <> 'Y'
     C                   EVAL      wk_LblsNeeded = *ON
     C                   ENDIF                                                  wk_Lbl1='Y'

     C                   ENDDO

     C                   RETURN    wk_LblsNeeded

     P LblsNeeded      E

     P/EJECT
      **************************************************************************
      * Determine whether totes need verified.                                 *
      **************************************************************************
     P TVNeeded        B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D Turn#                          9S 0 CONST
     D TurnSeq#                       3S 0 CONST
     D RcdSeq#                        5S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_TVNeeded     S              1N
     D wk_GDTOTE       S             10A
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_TVNeeded = *OFF

     C                   IF        not wk_ToteVfy
     C                   LEAVE
     C                   ENDIF                                                  not wk_ToteVfy

      /Free
       wk_GDTOTE = *Blanks;
       EXEC SQL select GDTOTE
                 into :wk_GDTOTE
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDTURN=:wk_Turn# and GDTSEQ=:wk_TurnSeq# and
                 GDRSEQ=:wk_RcdSeq#;
       IF wk_GDTOTE = *Blanks  or  %subst(wk_GDTOTE:1:3) = 'PLT';
         LEAVE;
       ENDIF;                                                                // wk_GDTOTE=*Blanks

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 GDTURN=:wk_Turn# and GDTSEQ=:wk_TurnSeq# and
                 GDRSEQ=:wk_RcdSeq# and
                 a.GDQSTC>(select coalesce(sum(b.TSQSCN),0)
                 from HYPSGTS b where b.TSGRP#=a.GDGRP# and
                 b.TSTURN=a.GDTURN and b.TSTSEQ=a.GDTSEQ and
                 b.TSRSEQ=a.GDRSEQ);
       IF wk_RcdCnt > *Zero;
         wk_TVNeeded = *ON;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_TVNeeded

     P TVNeeded        E

     P/EJECT
      **************************************************************************
      * Determine whether item has been palletized.                           *
      **************************************************************************
     P PltzNeeded      B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D Order#                         8S 0 CONST
     D Line#                          3S 0 CONST
     D Blanket#                       3S 0 CONST
     D SSCC                           9S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_BillTo#      S                   LIKE(GDBLTO)
     D wk_ShipTo#      S                   LIKE(GDSHTO)
     D wk_DropShip#    S                   LIKE(GDDSHP)
     D wk_Order#       S                   LIKE(GDORD#)
     D wk_Line#        S                   LIKE(GDORL#)
     D wk_Blanket#     S                   LIKE(GDBLN#)
     D wk_SSCC         S                   LIKE(GDSSCC)
     D wk_PltzNeeded   S              1N
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_SSCC = SSCC
     C                   EVAL      wk_PltzNeeded = *OFF

     C                   IF        wk_GCSTYP <> 'T'
     C                   LEAVE
     C                   ENDIF                                                  wk_GCSTYP<>'T'

     C                   IF        wk_SSCC = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_SSCCBC=*Zero

      /Free
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSCWK
                where S1ORD#=:wk_Order# and S1ORL#=:wk_Line# and
                 S1BLN#=:wk_Blanket# and S1SRL#=:wk_SSCC and
                 S1PSRL#=0;
       IF wk_RcdCnt > *Zero;
         wk_PltzNeeded = *ON;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_PltzNeeded

     P PltzNeeded      E

     P/EJECT

      **************************************************************************
      * Determine whether the status is 'Ship' or 'Pick'                       *
      **************************************************************************
     P ShipOrPick      B
     D                 PI             4A
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D Order#                         8S 0 CONST
     D Line#                          3S 0 CONST

     D wk_Status       S              4A
     D wk_ShipGrp#     S                   like(GDGRP#)
     D wk_Turn#        S                   like(GDTURN)
     D wk_Order#       S                   like(GDORD#)
     D wk_Line#        S                   like(GDORL#)
     D wk_PackLane     S             10A
     D wk_PickStatus   S              1A

      /Free

       wk_Status = 'Ship';
       wk_ShipGrp# = ShipGrp#;
       wk_Turn# = Turn#;
       wk_Order# = Order#;
       wk_Line# = Line#;

       wk_PackLane = *Blanks;
       EXEC SQL select GDDVRT
                  into :wk_PackLane
                  from HYPSGDT
                 where GDGRP#=:wk_ShipGrp# and GDTURN=:wk_Turn# and
                  GDORD#=:wk_Order# and GDORL#=:wk_Line#
                 fetch first row only;

       if wk_PackLane = '999';
         EXEC SQL select PICKITMST
                    into :wk_PickStatus
                    from PICKBATDP
                   where PICKTURN = :wk_Turn# and PICKORD = :wk_Order# and
                         PICKORDL = :wk_Line#;
         if wk_PickStatus = *blanks;
           wk_Status = 'Pick';
         endif;
       endif;

       return wk_Status;

      /End-Free

     P ShipOrPick      E

     P/EJECT

