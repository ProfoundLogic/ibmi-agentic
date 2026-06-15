     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2022')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR0608')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Shipment Tote Processing                                  *
      *          BY: William Earl (MCA)                                        *
      *        DATE: March 2022                                                *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  08/19/2022  MCA  Handle small-parcel picks (which could span multiple *
      *                   shipment groups and/or customer shipments for a      *
      *                   single tote).                                        *
      *  09/14/2022  MCA  Fix bug.  Any lot-tracked items were doubling        *
      *                   packing list quantities, invoice quantities, and     *
      *                   double-relieving inventory quantities.  These are    *
      *                   items that aren't lot-controlled, but are lot-tracked*
      *                   at shipping.                                         *
      *  09/22/2022  MCA  Handle *MULT totes.                                  *
      *  09/30/2022  MCA  Add TDRSEQ.                                          *
      *  10/17/2022  MCA  Fix label printing when checking for lot entry.      *
      *  11/11/2022  MCA  When creating labels, in addition to checking for    *
      *                   data in HYPSGLD, also need to check in HYW0608B.     *
      *  11/26/2022  MCA  Fix bug.  Sometimes HYPSCWK.S1UOMQ was populated     *
      *                   incorrectly.                                         *
      *  03/22/2023  MCA  Remove all references to BARDATA1 (used for the      *
      *                   BARDATA2.0 conversion).                              *
      *  01/24/2024  MCA  For location parsing, use new file HYPSTKR to        *
      *                   determine stockroom if a record exists for that      *
      *                   location.  Otherwise, logic in place since BoxLogix  *
      *                   shipping began will still be used.                   *
      *  08/21/2024  MCA  Add F12=Cancel confirmation prompt.                  *
      *  08/21/2024  MCA  Fix bugs.  Probably contributed to invalid labels.   *
      *                   1) When printing a label via option 12,              *
      *                      HYW0608A.W1SSCC was not being populated.          *
      *                   2) When F12=Cancel was pressed, applicable HYPSGDT   *
      *                      record fields GDLBL1, GDSSCCBC, and GDSSCC were   *
      *                      not cleared.                                      *
      *  11/01/2024  MCA  Handle double-letter aisle locations.                *
      *  11/07/2024  MCA  Fix bug.  In maint mode 2, after label print, wrong  *
      *                   refresh subfile subroutine was executed.             *
      *  11/15/2024  MCA  Write Tote Closed record when all items in tote are  *
      *                   verified.                                            *
      *  11/20/2024  MCA  Fix bug.  $UpdShipRcds handling of lot-controlled    *
      *                   and non-lot-controlled items was sometimes (maybe    *
      *                   always) wrong if a given tote both contained lot-    *
      *                   controlled items (i.e., most ammo) and contained     *
      *                   non-lot-controlled but still lot-tracked items       *
      *                   (e.g., rimfire ammo, blem ammo, etc.).  This was     *
      *                   because HDIMST.IMLOT was not being repopulated for   *
      *                   the current item being updated.                      *
      *  11/24/2024  MCA  Fix bug.  SQL variable GDPLOC should have been       *
      *                   wk_GDPLOC.                                           *
      *  11/25/2024  MCA  Handle wk_LDQSTC SQL null retrieval (no SQL0305).    *
      *  03/28/2025  MCA  Add option 2=change to change pick quantity.         *
      *  04/08/2025  MCA  Enhance blem ammo lot number validations.  For blem, *
      *                   check for ammo item numbers that aren't the blem's   *
      *                   normal parent item number (using file HYPBAOS).      *
      *  06/19/2025  MCA  Fix bug - in $WrtHYPSCWK, when retrieving HYPSGDT    *
      *                   fields for full-carton labels, include RSEQ on       *
      *                   select criteria.  This should also prevent SQL0811   *
      *                   from being shown in the job log.                     *
      *  08/20/2025  MCA  Fix bug - in SelOK and LblOK, ensure line has been   *
      *                   verified and a label has not already been printed.   *
      *                   This not being checked before this change is         *
      *                   probably the source of the duplicate labels (two     *
      *                   labels with separate serial numbers for the same     *
      *                   carton) that we have been seeing every once in a     *
      *                   while.                                               *
      *  08/20/2025  MCA  Shore up option 29 to delete labels.  In DltLblOK,   *
      *                   when verifying that a label has prior been generated,*
      *                   also check for labels generated during the current   *
      *                   Tote Verify session.  When deleting the label, also  *
      *                   remove the label's UCC serial number from the Tote   *
      *                   Verify work file.                                    *
      *  10/15/2025  MCA  Automatically print UCC-128 label when carton or     *
      *                   master-carton quantity scanned.                      *
      *  10/15/2025  MCA  Fix bug.  In $PrtLabel, check for full-carton label  *
      *                   regardless of whether GDCNVY='C'.                    *
      *  10/15/2025  MCA  Fix bug.  In $WrtHYPSCWK, SQL to retrieve HYPSGDT    *
      *                   fields should not have used GDSSCCBC for selection   *
      *                   for full carton labels.  This caused the SQL to not  *
      *                   return any data, because GDSSCCBC had not yet been   *
      *                   updated.  On 6/19/25, selection was added by GDRSEQ, *
      *                   and that is all that is needed to get to the correct *
      *                   HYPSGDT record.                                      *
      *  10/15/2025  MCA  Fix bug causing SQL0811 joblog error.  In $DltLabel, *
      *                   2 SQL statements needed "fetch first row only".      *
      *                   SQL's added to program on 8/20/2025.                 *
      *  10/21/2025  MCA  Add "not-done" or "all" filter.                      *
      *  11/05/2025  MCA  Fix bug from 10/21/25 change. If the initial subfile *
      *                   is empty, because we are only showing pick lines not *
      *                   yet done, this progam loops endless. This is caused  *
      *                   only when F7 is used in HYR0602 to call this program *
      *                   (as opposed to using option 14), and the tote is     *
      *                   already fully verified, and the user pressed F16     *
      *                   from the F7 prompt window to "re-open" the tote.     *
      *  04/15/2026  MCA  Check for (and prevent) order over-shipment.         *
      **************************************************************************

     FHYD0608A  CF   E             WORKSTN
     F                                     SFILE(SFL01A:wk_Sfl01RRN)
     F                                     SFILE(SFL01B:wk_Sfl01RRN)
     F                                     SFILE(SFL02A:wk_Sfl02RRN)
     FHYD0608B  CF   E             WORKSTN
     FHYLSGTD1  IF   E           K DISK    RENAME(HYRSGTD:HYRSGTD1)
     FHYLSGTD2  IF   E           K DISK    RENAME(HYRSGTD:HYRSGTD2)
     FHYLSGDT9  IF   E           K DISK    RENAME(HYRSGDT:HYRSGDT9)
     FHYLSGDT11 IF   E           K DISK    RENAME(HYRSGDT:HYRSGDT11)
     FHYPSGLD   UF A E           K DISK
     FHYLSGLD1  IF   E           K DISK    RENAME(HYRSGLD:HYRSGLD1)
     FHYPSGTS   IF A E           K DISK
     FHYPSGTD   IF   E           K DISK
     FHYPSGCU   IF   E           K DISK    USROPN
     FHYPSGTC   O    E           K DISK    BLOCK(*NO)
     FHDIMST    IF   E           K DISK
     FOESLWK    UF A E           K DISK
     FHDKTIT    IF   E           K DISK
     FHYPSTKR   IF   E           K DISK
     FHYPSCWK   O    E           K DISK    BLOCK(*NO) PREFIX(SC:2) USROPN
     FHYW0608A  IF A E           K DISK                                         W1
     FHYW0608AL2IF   E           K DISK    RENAME(HYW0608AR:HYW0608AR2) USROPN  W1
     FHYW0608B  UF A E           K DISK                                         W2
     FHYW0608BL1IF   E           K DISK    RENAME(HYW0608BR:HYW0608BR1) USROPN  W2
     FHYW0602   IF   E           K DISK    PREFIX(W3:2) USROPN                  W3

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0608         PR                  EXTPGM('HYR0608')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Tote                         10A   CONST
     D  RtnCode                       2A

     D HYR0606TV       PR                  EXTPGM('HYR0606TV')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  Tote                         10A   CONST
     D  RtnCode                       2A

      /COPY QPRPSRC,HYR0622

      /COPY QPRPSRC,HYR9930

      /COPY QPRPSRC,HYR9960

      /COPY QPRPSRC,HYR9962

      /COPY QPRPSRC,CHKDIGIT

     D SelOK           PR             2A                                        Check if select ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST
     D  Order#                        8S 0 CONST
     D  Item#                        15A   CONST
     D  LblType                       1A   CONST

     D LblOK           PR             2A                                        Check if lbl prt ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST
     D  SSCC                          9S 0 CONST

     D DltLblOK        PR             2A                                        Check if dlt lbl ok
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  TurnSeq#                      3S 0 CONST
     D  RcdSeq#                       5S 0 CONST
     D  SSCCBC                       20A   CONST

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

     D QMHSNDM         PR                  EXTPGM('QMHSNDM')                    Send non-pgm message
     D  out_MsgID                     7A   CONST
     D  out_MsgFile                  20A   CONST
     D  out_MsgData                 512A   CONST
     D  out_MsgDtaLen                10I 0 CONST
     D  out_MsgType                  10A   CONST
     D  out_MsgQ                    120A   CONST
     D  out_MsgQCnt                  10I 0 CONST
     D  out_RpyMsgQ                  20A   CONST
     D  in_MsgKey                     4A
     D  io_MsgErrCode                10I 0

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0608         PI
     D  in_PgmMode                    1A   CONST
     D  in_ShipGrp#                   9S 0 CONST
     D  in_BillTo#                    7S 0 CONST
     D  in_ShipTo#                    7S 0 CONST
     D  in_DropShip#                  7S 0 CONST
     D  in_Tote                      10A   CONST
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
     D wk_AlwFilter1   S              1A   DTAARA(HYABXLTVF)                    Alw not-done filter
     D wk_UpdAud       S              1A   DTAARA(HYABXLAUD1)                   Audit updates
     D wk_SSCCSrlRef   S              9P 0 DTAARA(SSCCSRLREF)                   SSCC serial no.
     D wk_GS1Comp      S              6A   DTAARA(GS1COMP)                      GS1 Company Prefix
     D wk_GS1Comp2     S              6A   DTAARA(GS1COMP2)                     GS1 Company Prefix 2
     D wk_LDA          S           1024A   DTAARA(*LDA)                         *LDA

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PgmMode      S              1A
     D wk_ShipGrp#     S              9S 0
     D wk_BillTo#      S              7S 0
     D wk_ShipTo#      S              7S 0
     D wk_DropShip#    S              7S 0
     D wk_Tote         S             10A
     D sv_Tote         S             10A
     D wk_RtnCode      S              2A
     D wk_CurrMode     S              1A
     D wk_EMPL         S              5S 0
     D wk_ShowAll      S              1N
     D wk_AutoF19      S              1N
     D wk_AllVerified  S              1N
     D wk_ShipToName   S             30A
     D wk_GCSTNM       S             30A
     D wk_Alda         S              1N
     D wk_GI281        S              1N
     D wk_WestDC       S              1N
     D wk_CustCnt      S              3P 0
     D wk_OrdCnt       S              3P 0
     D wk_CnfrmCncl    S              1N   INZ(*OFF)
     D wk_ItemScanned  S              1N   INZ(*OFF)
     D wk_SCWK         S              1N   INZ(*OFF)
     D wk_O1WH         S              3S 0
     D wk_O1OREC       S              1A
     D wk_O1KTRL       S              1A
     D wk_LotRJ        S             15A
     D wk_Lot#         S             15A
     D wk_PrimItem#    S             15A
     D wk_QSTCLeft     S              9P 0
     D wk_InProc       S             13P 4
     D wk_WHS          S              3S 0
     D wk_STKR         S              3A
     D wk_AILE         S              4A
     D wk_SLOC         S              8A
     D wk_KitCmpnt     S              1A
     D wk_KTQSTC       S             13P 4
     D wk_ISSID#       S              9P 0
     D wk_ISRATE       S              1S 0
     D wk_SLSTID       S              9P 0
     D wk_SLRATE       S              1S 0
     D wk_CITM         S                   LIKE(O3CITM) INZ(*Blanks)
     D wk_OITM         S                   LIKE(O3OITM) INZ(*Blanks)
     D wk_ToteMode     S              1N   INZ(*OFF)
     D wk_DftQty       S                   LIKE(C1QSTC)
     D wk_C1SCAN       S             15A
     D wk_Item#        S             15A
     D wk_CBCUST       S              7P 0
     D wk_CBITEM       S             15A
     D wk_CBCUPC       S             12A
     D wk_CBIPL        S              1A
     D wk_UseCustUPC   S              1N
     D wk_Timestamp    S               Z
     D wk_Include      S              1N   INZ(*OFF)
     D wk_IN12         S                   LIKE(*IN12)
     D wk_IN25         S                   LIKE(*IN25)
     D wk_Sfl01RRN     S              5P 0
     D wk_Sfl01PC      S              5P 0
     D wk_Sfl01PS      S              3P 0 INZ(12)
     D wk_Sfl02RRN     S              5P 0
     D wk_SelPr        S              1N   INZ(*OFF)
     D wk_ChgPr        S              1N   INZ(*OFF)
     D wk_DspPr        S              1N   INZ(*OFF)
     D wk_LblPr        S              1N   INZ(*OFF)
     D wk_LotInqPr     S              1N   INZ(*OFF)
     D wk_DltLblPr     S              1N   INZ(*OFF)
     D wk_OptPr        S              1N   INZ(*OFF)
     D wk_ClrDone      S              1N   INZ(*OFF)
     D wk_FirstSel     S                   LIKE(wk_Sfl01RRN)
     D wk_SelGrp#      S                   LIKE(S1GRP#)
     D wk_SelShto      S                   LIKE(S1SHTOX)
     D wk_Refresh      S              1N   INZ(*OFF)
     D wk_SelChk       S              2A
     D wk_ChgChk       S              2A
     D wk_LblChk       S              2A
     D wk_DltLblChk    S              2A
     D wk_LblType      S              1A
     D wk_LblQty       S              7P 0
     D wk_LblUOM       S              2A
     D wk_S1LTYP       S              1A
     D wk_S1UOMQ       S             13P 4
     D wk_S1UOM        S              2A
     D wk_SSCC         S              9P 0
     D wk_SSCCBC       S             20A
     D wk_W1QSCN       S             13P 4
     D wk_ChkDgt       S              1A
     D wk_ReprtLbl     S              1N
     D wk_GDGRP#       S              9S 0
     D wk_GDBLTO       S              7S 0
     D wk_GDSHTO       S              7S 0
     D wk_GDDSHP       S              7S 0
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
     D wk_TDQSTC       S             13P 4
     D wk_GDRcdsFnd    S              1N
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
     D wk_BDQTYBAR     S              7S 0
     D wk_LotCnt       S              5P 0
     D wk_Carton#      S              5P 0
     D wk_Bulk         S              1N
     D wk_UnitWgt      S              9P 5
     D wk_SelItem#     S             15A
     D wk_SelOrder#    S              8S 0
     D wk_CurShipQty   S              9P 0
     D wk_ToteQty      S             13P 4
     D wk_PkgQty       S              5P 0
     D wk_OEORD#       S              8S 0
     D wk_OEORRF       S             22A
     D wk_IMITEM       S             15A
     D wk_MinItem      S             15A
     D wk_MaxItem      S             15A
     D wk_TotLDQSTC    S             13P 4
     D wk_EMFNAM       S             18A
     D wk_EMLNAM       S             18A
     D wk_W1SSCC       S                   LIKE(W1SSCC)
     D wk_SysCYMD      S              7S 0
     D wk_RcdCnt       S              7P 0
     D wk_UpdCnt       S              7P 0
     D wk_ItemCnt      S              5P 0
     D wk_LblCnt       S              5P 0
     D wk_BAOSCnt      S              7P 0
     D wk_ODQORD       S             13P 4
     D wk_ODQSTD       S             13P 4
     D wk_ODQSTC       S             13P 4
     D wk_OpenQty      S              9P 0
     D wk_AlwF11       S              1N   INZ(*OFF)
     D wk_AlwF19       S              1N   INZ(*OFF)
     D wk_RC0606       S              2A
     D wk_RC0622       S             10I 0
     D wk_Error        S              1N   INZ(*OFF)
     D wk_Pgm02        S              1N   INZ(*OFF)                            HYR0622 called
     D wk_Pgm03        S              1N   INZ(*OFF)                            HYR0606TV called
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D sv_QOPK         S                   LIKE(GDQOPK)
     D kf_LSRTYP       S                   LIKE(LSRTYP) INZ('SHIP1')
     D kf_LSLOC        S                   LIKE(LSLOC)
     D @UGRP#          S                   LIKE(TDGRP#)                         SFL01 keys SPP extra
     D @UBLTO          S                   LIKE(TDBLTO)                            "   "
     D @USHTO          S                   LIKE(TDSHTO)                            "   "
     D @UDSHP          S                   LIKE(TDDSHP)                            "   "
     D @UITEM          S                   LIKE(TDITEM)                         SFL01 keys
     D @UTURN          S                   LIKE(TDTURN)                            "   "
     D @UTSEQ          S                   LIKE(TDTSEQ)                            "   "
     D @URSEQ          S                   LIKE(TDRSEQ)                            "   "
     D @DGRP#          S                   LIKE(TDGRP#)                         SFL01 keys SPP extra
     D @DBLTO          S                   LIKE(TDBLTO)                            "   "
     D @DSHTO          S                   LIKE(TDSHTO)                            "   "
     D @DDSHP          S                   LIKE(TDDSHP)                            "   "
     D @DITEM          S                   LIKE(TDITEM)                         SFL01 keys
     D @DTURN          S                   LIKE(TDTURN)                            "   "
     D @DTSEQ          S                   LIKE(TDTSEQ)                            "   "
     D @DRSEQ          S                   LIKE(TDRSEQ)                            "   "

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D MaintMode1      C                   '1'                                  Single customer tote
     D MaintMode2      C                   '2'                                  Multi customer tote
     D ShutDownMode    C                   '9'
     D ItemScan        C                   '1'
     D LotScan         C                   '2'
     D @SEL            C                   ' 1'
     D @SEL2           C                   '1 '
     D @CHG            C                   ' 2'
     D @CHG2           C                   '2 '
     D @DSP            C                   ' 5'
     D @DSP2           C                   '5 '
     D @LBL            C                   '12'
     D @LOTINQ         C                   '16'
     D @DLTLBL         C                   '29'
     D ToteHdg1        C                   'Shipment Detail Processing'
     D ToteHdg2        C                   'Tote Verify'
     D MixedLblHdg     C                   'Mixed Label Selection Confirmation'
     D PrtlLblHdg      C                   'Partial Label Selection +
     D                                     Confirmation'
     D F5Txt           C                   'F5=Refresh'
     D F8Txt           C                   'F8=Mixed Lbl'
     D F9Txt           C                   'F9=Partial Lbl'
     D F11Txt          C                   'F11=Complete'
     D F12Txt          C                   'F12=Cancel'
     D F17Txt          C                   'F17=Top'
     D F18Txt          C                   'F18=Bottom'
     D F19Txta         C                   'F19=Show All'
     D F19Txtb         C                   'F19=Show Not Done'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_PgmMode = in_PgmMode
     C                   EVAL      wk_ShipGrp# = in_ShipGrp#
     C                   EVAL      wk_BillTo# = in_BillTo#
     C                   EVAL      wk_ShipTo# = in_ShipTo#
     C                   EVAL      wk_DropShip# = in_DropShip#
     C                   EVAL      wk_Tote = in_Tote
     C                   EVAL      wk_RtnCode = *Zeros
     C                   EVAL      out_RtnCode = *Zeros

     C                   IF        wk_PgmMode = ShutDownMode
     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF

     C                   DO

     C                   EXSR      $InzSR

     C                   EXSR      $BldWork

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageDownSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageDownSf1b
     C                   ENDSL

     C                   DOW       not *IN12

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

     C                   IF        wk_CurrMode = ItemScan
     C                   EVAL      *IN11 = wk_AlwF11
     C                   EVAL      *IN19 = wk_AlwF19
     C                   ELSE
     C                   EVAL      *IN11 = *OFF
     C                   EVAL      *IN19 = *OFF
     C                   ENDIF                                                  wk_CurrMode=ItemScan

     C                   WRITE     SFLCMD01
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXFMT     SFLCTL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXFMT     SFLCTL01B
     C                   ENDSL

     C                   EVAL      *IN27 = *OFF
     C                   EVAL      *IN93 = *ON
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      *IN73 = *OFF
     C                   EXSR      $RMVPM

      *
      * Valid Command Key?
      *
     C                   IF        *IN25

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   IF        wk_CurrMode = LotScan
     C                   EVAL      C1LOT = *Blanks
     C                   EVAL      wk_CurrMode = ItemScan
     C                   EVAL      *IN81 = *OFF
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ELSE
     C                   EXSR      $CnfrmCncl
     C                   IF        wk_CnfrmCncl
     C                   IF        wk_SCWK
     C                   EXSR      $DltSCWK
     C                   EVAL      wk_SCWK = *OFF
     C                   ENDIF                                                  wk_SCWK
     C                   EXSR      $ClrWork
     C                   EVAL      wk_ItemScanned = *OFF
     C                   EVAL      wk_RtnCode = '12'
     C                   LEAVE
     C                   ELSE
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  wk_CnfrmCncl
     C                   ENDIF                                                  wk_CurrMode=LotScan
     C                   ENDIF                                                  *IN12

      *
      * Check for function key use with item scan or options present.
      *
     C                   EVAL      wk_Error = *OFF

     C                   IF        *IN08  or  *IN09  or  *IN15  or *IN17 or
     C                                *IN19  or
     C                                *IN18  or  *IN91  or  *IN92

     C                   IF        C1SCAN <> *Blanks
     C                   ENDIF                                                  C1SCAN=*Blanks

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
     C                   IF        *IN08  or  *IN09  or *IN15 or
     C                                *IN19  or
     C                                *IN17  or  *IN18
     C                   EVAL      wK_MsgID = 'GUM0005'
     C                   ELSE
     C                   EVAL      wK_MsgID = 'GUM0004'
     C                   ENDIF                                                  *IN08 or *IN09
     C                   EVAL      wk_MsgData = *Blanks
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * F8=Mixed Label and F9=Partial Label require selections.
      *
     C                   IF        (*IN08  or  *IN09)  and
     C                               not wk_SelPr
     C                   EVAL      wk_MsgID = 'ERR1256'
     C                   SELECT
     C                   WHEN      *IN08
     C                   EVAL      wk_MsgData = 'F8=Mixed Label'
     C                   WHEN      *IN09
     C                   EVAL      wk_MsgData = 'F9=Partial Label'
     C                   ENDSL
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  *IN08 or

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

     C                   ENDIF                                                  *IN08 or *IN09 or

     C                   SELECT

      *
      * Page Down Key?
      *
     C                   WHEN      *IN91
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageDownSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageDownSf1b
     C                   ENDSL
     C                   EVAL      *IN91 = *OFF

      *
      * Page Up Key?
      *
     C                   WHEN      *IN92
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageUpSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageUpSf1b
     C                   ENDSL
     C                   EVAL      *IN92 = *OFF

      *
      * Refresh?
      *
     C                   WHEN      *IN05
     C                   IF        wk_CurrMode = ItemScan
     C                   EVAL      C1QSTC = wk_DftQty
     C                   EVAL      C1SCAN = *Blanks
     C                   EVAL      C1ITEM = *Blanks
     C                   ENDIF                                                  wk_CurrMode=ItemScan
     C                   EVAL      C1LOT = *Blanks
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      *IN73 = *OFF
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageDownSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageDownSf1b
     C                   ENDSL
     C                   EVAL      *IN05 = *OFF

      *
      * Mixed Label or Partial Label?
      *
     C                   WHEN      *IN08  or  *IN09
     C                   IF        wk_Sfl01RRN <> *Zero
     C                   EXSR      $ProcessSf1
     C                   ENDIF                                                  wk_Sfl01RRN<>*Zero

      *
      * Show not-done or all tote lines.
      *
     C                   WHEN      *IN19
     C                   IF        wk_ShowAll
     C                   EVAL      wk_ShowAll = *OFF
     C                   ELSE
     C                   EVAL      wk_ShowAll = *ON
     C                   ENDIF                                                  wk_ShowAll
     C                   EXSR      $SflAfterF19
     C                   EVAL      *IN05 = *OFF
     C                   EXSR      $FuncKey
     C                   ITER

      *
      * Complete tote verify process?
      *
     C                   WHEN      *IN11
     C                   EXSR      $ChkVfySts
     C                   IF        not wk_AllVerified
     C                   DOU       *IN11  or  *IN12
     C                   EXFMT     WINDOW01
     C                   ENDDO                                                  *IN11 or *IN12
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  not wk_AllVerified
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $MoveToMain
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $MoveToMain2
     C                   ENDSL
     C                   EVAL      wk_SCWK = *OFF
     C                   EVAL      wk_ItemScanned = *OFF
     C                   EVAL      *IN12 = *ON
     C                   LEAVE

      *
      * Change default quantity?
      *
     C                   WHEN      *IN15
     C                   IF        wk_DftQty = 5
     C                   EVAL      wk_DftQty = 1
     C                   ELSE
     C                   EVAL      wk_DftQty = 5
     C                   ENDIF                                                  wk_DftQty=5
     C                   EVAL      C1QSTC = wk_DftQty
     C                   EVAL      *IN15 = *OFF

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

     C                   ENDSL

     C                   ELSE

      *
      * Enter Key or F8=Mixed Label or F9=Partial Label
      *
      * No Records in Subfile?, Try to refresh subfile.
      *
     C                   IF        wk_Sfl01RRN = *Zero  and
     C                              (wk_CurrMode = ItemScan  and
     C                               C1SCAN = *Blanks)

     C                   EXSR      $Top1

     C                   ELSE

     C                   SELECT

     C                   WHEN      wk_CurrMode = ItemScan
     C                   IF        C1SCAN <> *Blanks
     C                   EXSR      $ItemScan
     C                   ENDIF                                                  C1SCAN<>*Blanks

     C                   WHEN      wk_CurrMode = LotScan
     C                   EXSR      $LotScan

     C                   ENDSL

     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * Edit options if subfile has records.
      *
     C                   EXSR      $EditSf1

      *
      * If all options are valid, process the selections.
      *
     C                   IF        not wk_Error

      *
      * Read and Process Subfile
      *
     C                   EXSR      $ProcessSf1

     C                   IF        wk_Refresh

      *
      * If this is the first verify transaction in this TV session (wk_AutoF19),
      * and we are currently showing all pick lines, change to show only the
      * pick lines not yet done.
      *
     C                   IF        wk_AutoF19  and  wk_ShowAll
     C                   EVAL      wk_ShowAll = *OFF
     C                   EXSR      $FuncKey
     C                   EVAL      wk_AutoF19 = *OFF
     C                   ENDIF                                                  wk_AutoF19

     C                   EVAL      *IN05 = *ON
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageDownSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageDownSf1b
     C                   ENDSL

      * If all tote pick lines have been verified, and all UCC-128 labels
      * have been printed, but current filter is to show only not-done,
      * change filter to show all an rebuild subfile.

     C                   IF        wk_Sfl01RRN = *Zero  and  not wk_ShowAll
     C                   EVAL      wk_ShowAll = *ON
     C                   EXSR      $SflAfterF19
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   EVAL      *IN05 = *OFF
     C                   EVAL      wk_Refresh = *OFF
     C                   ENDIF                                                  wk_Refresh

     C                   ELSE
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  not wk_Error

     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   ENDIF                                                  *IN25=*ON

     C                   EVAL      *IN12 = *OFF

     C                   ENDDO                                                  not *IN12

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
      * Process item scan.                                                     *
      **************************************************************************
     CSR   $ItemScan     BEGSR

     C                   EVAL      wk_UseCustUPC = *OFF

     C                   IF        (%subst(C1SCAN:1:1) = '0'  and
     C                              (%subst(C1SCAN:2:6) = wk_GS1Comp  or
     C                               %subst(C1SCAN:2:6) = wk_GS1Comp2)  and
     C                               %subst(C1SCAN:14:2) = *Blanks)
     C                   EVAL      C1SCAN = %subst(C1SCAN:2:12)
     C                   ENDIF                                                  %subst(C1SCAN:1:7)=

     C                   IF        %len(%trimr(C1SCAN)) > 11  and
     C                                %subst(C1SCAN:15:1) = *Blank
     C                   EVAL      wk_Item# = BarcodeItem(C1SCAN)
     C                   EVAL      wk_PkgQty = BarcodeQty(C1SCAN)
     C                   IF        wk_Item# = *Blanks
     C                   EVAL      wk_Item# = BarcodeItemC(C1SCAN:'H')
     C                   EVAL      wk_PkgQty = BarcodeQtyC(C1SCAN)
     C                   IF        wk_Item# <> *Blanks
      /Free
       wk_CBCUST = *Zero;
       wk_CBITEM = *Blanks;
       wk_CBCUPC = *Blanks;
       wk_CBIPL = *Blank;
       EXEC SQL select CBCUST, CBITEM, CBCUPC, CBIPL
                into :wk_CBCUST, :wk_CBITEM, :wk_CBCUPC, :wk_CBIPL
                from BARCUST
                where CBCUST=:wk_BillTo# and CBITEM=:wk_Item#;
       IF wk_CBCUST = wk_BillTo#  and  wk_CBITEM = wk_Item#  and
            wk_CBCUPC <> *Blanks  and  wk_CBIPL = 'C';
         wk_UseCustUPC = *ON;
       ENDIF;                                                                // wk_CBCUST=wk_BillTo#
      /End-Free
     C                   ENDIF                                                  wk_Item#<>*Blanks
     C                   ENDIF                                                  wk_Item#=*Blanks
     C                   ELSE
     C                   EVAL      wk_Item# = *Blanks
     C                   EVAL      wk_PkgQty = *Zero
     C                   ENDIF                                                  %len(%trimr(C1SCAN))

     C                   IF        wk_Item# = *Blanks
     C                   EVAL      C1ITEM = C1SCAN
     C                   IF        C1QSTC = wk_DftQty
     C                   EXSR      $RtvCaseQty
     C                   EVAL      C1QSTC = wk_IWPCPC
     C                   ENDIF                                                  C1QSTC=wk_DftQty
     C                   ELSE
     C                   EVAL      C1ITEM = wk_Item#
     C                   EXSR      $RtvCaseQty
     C                   IF        wk_PkgQty = wk_IWPCPC
     C                   IF        C1QSTC = wk_DftQty
     C                   EVAL      C1QSTC = wk_IWPCPC
     C                   ENDIF                                                  C1QSTC=wk_DftQty
     C                   ELSE
     C                   IF        (not wk_UseCustUPC  and
     C                               ItemBarcode(wk_Item#:'B') = C1SCAN)  or
     C                             (wk_UseCustUPC  and
     C                               ItemBarcodeC(wk_Item#:wk_BillTo#:'H':'B') =
     C                               C1SCAN)
     C                   IF        C1QSTC = wk_DftQty
     C                   IF        not wk_UseCustUPC
     C                   EVAL      C1QSTC = ItemQty(wk_Item#:'B')
     C                   ELSE
     C                   EVAL      C1QSTC =
     C                               ItemQtyC(wk_Item#:wk_BillTo#:'H':'B')
     C                   ENDIF                                                  not wk_UseCustUPC
     C                   ENDIF                                                  C1QSTC=wk_DftQty
     C                   ELSE
     C                   IF        (not wk_UseCustUPC  and
     C                               ItemBarcode(wk_Item#:'I') = C1SCAN)  or
     C                             (wk_UseCustUPC  and
     C                               ItemBarcodeC(wk_Item#:wk_BillTo#:'H':'I') =
     C                               C1SCAN)
     C                   IF        C1QSTC = wk_DftQty
     C                   IF        not wk_UseCustUPC
     C                   EVAL      C1QSTC = ItemQty(wk_Item#:'I')
     C                   ELSE
     C                   EVAL      C1QSTC =
     C                               ItemQtyC(wk_Item#:wk_BillTo#:'H':'I')
     C                   ENDIF                                                  not wk_UseCustUPC
     C                   ENDIF                                                  C1QSTC=wk_DftQty
     C                   ELSE
     C                   IF        wk_PkgQty > *Zero
     C                   IF        C1QSTC * wk_PkgQty < 999999999
     C                   EVAL      C1QSTC = C1QSTC * wk_PkgQty
     C                   ELSE
     C                   EVAL      C1QSTC = 999999999
     C                   ENDIF                                                  C1QSTC*wk_PkgQty<
     C                   ENDIF                                                  wk_PkgQty>*Zero
     C                   ENDIF                                                  ItemBarcode(wk_Item#
     C                   ENDIF                                                  ItemBarcode(wk_Item#
     C                   ENDIF                                                  wk_PkgQty=IWPCPC
     C                   ENDIF                                                  wk_Item#=*Blanks

     C                   EVAL      wk_C1SCAN = C1SCAN
     C                   EVAL      C1SCAN = *Blanks

     C                   EXSR      $Edit1

     C                   IF        wk_Error
     C                   EVAL      C1SCAN = wk_C1SCAN
     C                   LEAVESR
     C                   ENDIF                                                  wk_Error

      *
      * Retrieve applicable HYPSGDT record.
      *
     C                   EXSR      $RtvGDRcd
     C                   IF        wk_Error
     C                   EVAL      C1SCAN = wk_C1SCAN
     C                   LEAVESR
     C                   ENDIF                                                  wk_Error

      *
      * After HYPSGDT record has been retrieved, check that order will not be
      * over-shipped with the current scan.  Should not need this check, but
      * sometimes we do because of a pick-ticket ship prior to this BoxLogix.
      *
      /Free
       wk_ODQORD = *Zero;
       wk_ODQSTD = *Zero;
       wk_ODQSTC = *Zero;
       EXEC SQL select ODQORD, ODQSTD, ODQSTC
                 into :wk_ODQORD, :wk_ODQSTD, :wk_ODQSTC
                from OEORDT
                where ODORD#=:GDORD# and ODORL#=:GDORL# and
                      ODBLN#=:GDBLN#;
      /End-Free
     C                   IF        wk_ODQORD > *Zero  and                       If scan will cause
     C                               wk_ODQSTD + wk_ODQSTC + C1QSTC > wk_ODQORD  an order over-ship
     C                   EVAL      wk_OpenQty =                                   send error and get
     C                               wk_ODQORD - wk_ODQSTD - wk_ODQSTC            out.
     C                   EVAL      *IN71 = *ON
     C                   EVAL      wk_MsgID = 'ERR1540'
     C                   EVAL      wk_MsgData = %triml(%editc(GDORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:15) = C1ITEM
     C                   EVAL      %subst(wk_MsgData:24:12) =
     C                               %triml(%editc(wk_OpenQty:'J'))
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      C1SCAN = wk_C1SCAN
     C                   LEAVESR
     C                   ENDIF                                                  wk_ODQORD>*Zero and

     C                   IF        GDLOT = *Zeros  and
     C                               %subst(GDIMDS:1:5) <> 'SAMMO'  and
     C                               %subst(GDIMDS:1:5) <> 'SFRON'
     C                   EVAL      wk_Timestamp = %timestamp
     C                   EXSR      $WrtWork1
     C                   EVAL      wk_MsgID = 'MSG0442'
     C                   EVAL      wk_MsgData = %triml(%editc(GDORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:15) = C1ITEM
     C                   EVAL      %subst(wk_MsgData:24:12) =
     C                               %triml(%editc(C1QSTC:'J'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_Refresh = *ON

      *
      * Print UCC-128 label when carton or master-carton quantity scanned.
      *
     C                   EXSR      $PrtCtnLbl

     C                   EVAL      C1ITEM = *Blanks
     C                   EVAL      C1QSTC = wk_DftQty
     C                   IF        not wk_AlwF11
     C                   EVAL      wk_AlwF11 = *ON
     C                   EXSR      $FuncKey
     C                   ENDIF                                                  not wk_AlwF11
     C                   ELSE
     C                   EVAL      wk_CurrMode = LotScan
     C                   EVAL      *IN81 = *ON
     C                   EXSR      $FuncKey
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  GDLOT=*Zeros and

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve case-pack quantity.                                           *
      **************************************************************************
     CSR   $RtvCaseQty   BEGSR

      /Free
       wk_IWPCPC = *Zero;
       EXEC SQL select IWPCPC into :wk_IWPCPC
                from HDIWHS
                where IWITEM=:C1ITEM and IWWHS=6;
       IF wk_IWPCPC = *Zero;
         EXEC SQL select IWPCPC into :wk_IWPCPC
                  from HDIWHS
                  where IWITEM=:C1ITEM;
       ENDIF;                                                                // wk_IWPCPC=*Zero
       IF wk_IWPCPC = *Zero;
         wk_IWPCPC = 1;
       ENDIF;                                                                // wk_IWPCPC=*Zero
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate item scan.                                                    *
      **************************************************************************
     CSR   $Edit1        BEGSR

     C                   EVAL      wk_Error = *OFF

     C                   DO

      *
      * Item number must be valid.
      *
     C     C1ITEM        CHAIN     HDIMST
     C                   IF        not %found(HDIMST)
     C                   EVAL      *IN72 = *ON
     C                   EVAL      wk_MsgID = 'ERR0319'
     C                   EVAL      wk_MsgData = C1ITEM
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   LEAVE
     C                   ENDIF                                                  not %found(HDIMST)

      *
      * Retrieve item quantities included in the tote.
      *
      /Free
       wk_RcdCnt = *Zero;
       wk_GDQSTC = *Zero;
       wk_TDQSTC = *Zero;

       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL select count(*), sum(GDQSTC)
                 into :wk_RcdCnt, :wk_GDQSTC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDTOTE=:wk_Tote and GDITEM=:C1ITEM;
       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL select count(*), sum(GDQSTC)
                 into :wk_RcdCnt, :wk_GDQSTC
                from HYPSGDT a
                where GDTOTE=:wk_Tote and GDITEM=:C1ITEM and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP#
                 and b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and
                 b.GCDSHP=a.GDDSHP and b.GCSTS='O');
       ENDSL;

       IF wk_Rcdcnt = *Zero;
         wk_GDRcdsFnd = *OFF;
       ELSE;
         wk_GDRcdsFnd = *ON;
         wk_RcdCnt = *Zero;
       ENDIF;                                                                // wk_RcdCnt=*Zero

       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL select count(*), sum(TDQSTCB)
                 into :wk_RcdCnt, :wk_TDQSTC
                from HYPSGTD
                where TDGRP#=:wk_ShipGrp# and TDBLTO=:wk_BillTo# and
                 TDSHTO=:wk_ShipTo# and TDDSHP=:wk_DropShip# and
                 TDTOTE=:wk_Tote and TDITEM=:C1ITEM;
       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL select count(*), sum(TDQSTCB)
                 into :wk_RcdCnt, :wk_TDQSTC
                from HYPSGTD a
                where TDTOTE=:wk_Tote and TDITEM=:C1ITEM and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.TDGRP#
                 and b.GCBLTO=a.TDBLTO and b.GCSHTO=a.TDSHTO and
                 b.GCDSHP=a.TDDSHP and b.GCSTS='O');
       ENDSL;

      // NOTE: If C1ITEM is valid, wk_GDRcdsFnd=*OFF when tote#='*MULT'.
      //       In that case, need to use TDQSTC for tote quantity.
       IF wk_GDRcdsFnd;
         wk_ToteQty = wk_GDQSTC;
       ELSE;
         wk_ToteQty = wk_TDQSTC;
       ENDIF;                                                                // wk_GDRcdsFnd
      /End-Free

      *
      * Item must be associated with the tote.
      *
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      *IN72 = *ON
     C                   EVAL      wk_MsgID = 'ERR1395'
     C                   EVAL      wk_MsgData = C1ITEM + wk_Tote
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_RcdCnt=*Zero

      *
      * Quantity must be greater than zero.
      *
     C                   IF        C1QSTC <= *Zero
     C                   EVAL      *IN71 = *ON
     C                   EVAL      wk_MsgID = 'ERR0636'
     C                   EVAL      wk_MsgData = %triml(%editc(C1QSTC:'J'))
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  C1QSTC<=*Zero

      *
      * Quantity cannot be greater than item's tote quantity.
      *
     C                   IF        C1QSTC > wk_ToteQty
     C                   EVAL      *IN71 = *ON
     C                   EVAL      wk_MsgID = 'ERR1396'
     C                   EVAL      wk_MsgData = C1ITEM + wk_Tote +
     C                               %triml(%editc(wk_ToteQty:'J'))
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  C1QSTC>wk_ToteQty

     C                   ENDDO

      *
      * If error detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process lot scan.                                                      *
      **************************************************************************
     CSR   $LotScan      BEGSR

     C                   EXSR      $Edit2

     C                   IF        wk_Error
     C                   LEAVESR
     C                   ENDIF                                                  wk_Error

     C                   EVAL      wk_Timestamp = %timestamp
     C                   EXSR      $WrtWork1
     C                   EXSR      $WrtWork2
     C                   EVAL      wk_MsgID = 'MSG0443'
     C                   EVAL      wk_MsgData = %triml(%editc(GDORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:15) = C1ITEM
     C                   EVAL      %subst(wk_MsgData:24:15) = %triml(C1LOT)
     C                   EVAL      %subst(wk_MsgData:39:12) =
     C                               %triml(%editc(C1QSTC:'J'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_Refresh = *ON
     C                   IF        not wk_AlwF11
     C                   EVAL      wk_AlwF11 = *ON
     C                   EXSR      $FuncKey
     C                   ENDIF                                                  not wk_AlwF11

      *
      * Print UCC-128 label when carton or master-carton quantity scanned.
      *
     C                   EXSR      $PrtCtnLbl

     C                   EVAL      C1ITEM = *Blanks
     C                   EVAL      C1LOT = *Blanks
     C                   EVAL      C1QSTC = wk_DftQty
     C                   EVAL      wk_CurrMode = ItemScan
     C                   EVAL      *IN81 = *OFF
     C                   EXSR      $FuncKey

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate lot scan.                                                     *
      **************************************************************************
     CSR   $Edit2        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Lot number is required and must be valid.
      *
     C                   IF        C1LOT = *Blanks
     C                   EVAL      *IN73 = *ON
     C                   EVAL      wk_MsgID = 'ERR0899'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       IF %subst(GDIMDS:1:5) <> 'SAMMO'  and
          %subst(GDIMDS:1:5) <> 'SFRON';
         EVAL wk_O1WH = *Zero;
         EXEC SQL select O1WH into :wk_O1WH
                  from OEDTWK
                  where O1OCTL=:GDOCTL and O1ORL#=:GDORL# and
                   O1BLN#=:GDBLN#;

         IF IMLOT = 'A'  or  IMLOT = 'M';
           EVALR wk_LotRJ = %trimr(C1LOT);
         ELSE;
           wk_LotRJ = *Blanks;
         ENDIF;                                                              // IMLOT='A' or
         wk_RcdCnt = *Zero;
         SELECT;
         WHEN wk_Alda;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HDITSL
                    WHERE ISITEM=:C1ITEM and ISWHS=:wk_O1WH and
                     substr(ISSTKR,1,1)='A' and ISLOT=:wk_LotRJ;
         WHEN wk_GI281;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HDITSL
                    WHERE ISITEM=:C1ITEM and ISWHS=:wk_O1WH and
                     substr(ISSTKR,1,1)='G' and ISLOT=:wk_LotRJ;
         WHEN wk_WestDC;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HDITSL
                    WHERE ISITEM=:C1ITEM and ISWHS=:wk_O1WH and
                     ISSTKR in ('006','060') and ISLOT=:wk_LotRJ;
         ENDSL;
         IF wk_RcdCnt = *Zero;
           wk_Error = *ON;
           *IN73 = *ON;
           wk_MsgID = 'ERR0900';
           wk_MsgData = %triml(C1LOT);
           EXSR $SndPM;
         ENDIF;                                                              // wk_RcdCnt=*Zero
       ELSE;
         wk_Lot# = %triml(C1LOT);
         wk_PrimItem# = %subst(C1ITEM:2:14);
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from LDPMORD
                  where OMLOT=:wk_Lot# and
                   OMPN in (:C1ITEM, :wk_PrimItem#);
         IF wk_RcdCnt = *Zero;
           wk_BAOSCnt = *Zero;
           EXEC SQL select count(*) into :wk_BAOSCnt
                    from HYPBAOS a
                    where a.BSBLEM=:C1ITEM and
                     exists (select * from LDPMORD b where
                     b.OMPN=a.BSITEM and b.OMLOT=:wk_Lot#);
          IF wk_BAOSCnt = *Zero;
           wk_Error = *ON;
           *IN73 = *ON;
           wk_MsgID = 'ERR0900';
           wk_MsgData = %triml(C1LOT);
           EXSR $SndPM;
          ENDIF;                                                             // wk_BAOSCnt=*Zero
         ENDIF;                                                              // wk_RcdCnt=*Zero
       ENDIF;                                                                // %subst(GDIMDS:1:5)<>
     C                   ENDIF                                                  C1LOT=*Blanks

      *
      * If error detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print UCC-128 label when carton or master-carton quantity scanned.     *
      * Note: Data from the HYPSGDT record, retrieved by $RtvGDRcd in          *
      *       $ItemScan, is used in this subroutine.                           *
      **************************************************************************
     CSR   $PrtCtnLbl    BEGSR

      /Free
       wk_W1QSCN = *Zero;
       EXEC SQL select coalesce(sum(W1QSCN),0)
                 into :wk_W1QSCN
                from HYW0608A
                where W1GRP#=:GDGRP# and W1TURN=:GDTURN and
                 W1TSEQ=:GDTSEQ and W1RSEQ=:GDRSEQ and
                 W1TOTE=:wk_Tote;
      /End-Free

     C                   IF        wk_W1QSCN = 1
     C                   LEAVESR
     C                   ENDIF                                                  wk_W1QSCN=1

     C                   IF        wk_W1QSCN = GDQOPK                           If fully scanned
     C                   EXSR      $RtvCaseQty
      /Free
       wk_BDQTYBAR = *Zero;
       EXEC SQL select BDQTYBAR into :wk_BDQTYBAR
                from BARDATA
                where BDITEM=:C1ITEM;
      /End-Free
     C                   IF        wk_W1QSCN = wk_IWPCPC  or                    Carton qty
     C                               wk_W1QSCN = wk_BDQTYBAR                    Master-Carton qty
     C                   EVAL      S1SSCCBC = *Blanks
     C                   EVAL      S1GRP# = GDGRP#
     C                   EVAL      S1BLTO = GDBLTO
     C                   EVAL      S1SHTO = GDSHTO
     C                   EVAL      S1DSHP = GDDSHP
     C                   EVAL      S1TURN = GDTURN
     C                   EVAL      S1TSEQ = GDTSEQ
     C                   EVAL      S1RSEQ = GDRSEQ
     C                   EVAL      S1CNVY = GDCNVY
     C                   EVAL      S1OCTL = GDOCTL
     C                   EVAL      S1ORD# = GDORD#
     C                   EVAL      S1ORL# = GDORL#
     C                   EVAL      S1BLN# = GDBLN#
     C                   EVAL      S1ITEM = GDITEM
     C                   EVAL      wk_GDITEM = GDITEM
     C                   EVAL      wk_GDQSTC = wk_W1QSCN
     C                   EVAL      wk_GDLBL1 = GDLBL1
     C                   EVAL      wk_GDCNVY = GDCNVY
     C                   EXSR      $PrtLabel
     C                   ENDIF                                                  wk_W1QSCN=wk_IWPCPC
     C                   ENDIF                                                  wk_W1QSCN=GDQOPK

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page down subfile SFL01 - main subfile.                               *
      **************************************************************************
     CSR   $PageDownSf1  BEGSR

      *
      * If no records, clear subfile and initialize relative record and page count.
      *
     C     *LOVAL        SETLL     HYLSGTD2

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
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY1       SETGT     HYLSGTD2
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY1       SETLL     HYLSGTD2
     C                   OTHER                                                  Enter,...
     C     key_TD2       SETLL     HYLSGTD2
     C                   ENDSL

     C                   ENDIF                                                  not %found

     C                   DOU       %eof(HYLSGTD2)  or  wk_Include
     C     key_TD2       READE     HYLSGTD2
     C                   IF        not %eof(HYLSGTD2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGTD2)
     C                   ENDDO                                                  %eof(HYLSGTD2) or

      *
      * If EOF, send Bottom of the List message
      *
     C                   IF        %eof
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   IF        not *IN91  and  not *IN05
     C     key_TD2       SETLL     HYLSGTD2
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

     C                   EVAL      C1QSTC = wk_DftQty
     C                   EVAL      C1SCAN = *Blanks
     C                   EVAL      C1ITEM = *Blanks
     C                   EVAL      C1LOT = *Blanks

     C     key_TD2       SETGT     HYLSGTD2
     C                   DOU       %eof(HYLSGTD2)  or  wk_Include
     C     key_TD2       READPE    HYLSGTD2
     C                   IF        not %eof(HYLSGTD2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGTD2)
     C                   ENDDO                                                  %eof(HYLSGTD2) or
     C                   IF        %eof
     C                   EVAL      S@MORE = 'Bottom'
     C                   LEAVESR
     C                   ENDIF                                                  %eof
     C                   ENDIF                                                  *IN05

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   ENDIF                                                  %eof

     C                   DOW        not %eof(HYLSGTD2)

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
     C                   EVAL      @UITEM = TDITEM
     C                   EVAL      @UTURN = TDTURN
     C                   EVAL      @UTSEQ = TDTSEQ
     C                   EVAL      @URSEQ = TDRSEQ
     C                   ENDIF                                                  wk_Sfl01PC=1
     C                   EVAL      @DITEM = TDITEM
     C                   EVAL      @DTURN = TDTURN
     C                   EVAL      @DTSEQ = TDTSEQ
     C                   EVAL      @DRSEQ = TDRSEQ

      *
      * Get next tote detail record.
      *
     C                   DOU       %eof(HYLSGTD2)  or  wk_Include
     C     key_TD2       READE     HYLSGTD2
     C                   IF        not %eof(HYLSGTD2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGTD2)
     C                   ENDDO                                                  %eof(HYLSGTD2) or

     C                   ENDDO                                                  not %eof(HYLSGTD2)

      *
      * Set More... or Bottom   subfile messge.
      *
     C                   IF        %eof(HYLSGTD2)
     C                   EVAL      S@MORE = 'Bottom'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSGTD2)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page down subfile SFL01B - main subfile.                              *
      **************************************************************************
     CSR   $PageDownSf1b BEGSR

      *
      * If no records, clear subfile and initialize relative record and page count.
      *
     C     *Loval        SETLL     HYLSGTD1

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
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY1b      SETGT     HYLSGTD1
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY1b      SETLL     HYLSGTD1
     C                   OTHER                                                  Enter,...
     C     wk_Tote       SETLL     HYLSGTD1
     C                   ENDSL

     C                   ENDIF                                                  not %found

     C                   DOU       %eof(HYLSGTD1)  or  wk_Include
     C     wk_Tote       READE     HYLSGTD1
     C                   IF        not %eof(HYLSGTD1)
     C                   EXSR      $Filter1b
     C                   ENDIF                                                  not %eof(HYLSGTD1)
     C                   ENDDO                                                  %eof(HYLSGTD1) or

      *
      * If EOF, send Bottom of the List message
      *
     C                   IF        %eof
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   IF        not *IN91  and  not *IN05
     C     wk_Tote       SETLL     HYLSGTD1
     C                   IF        %equal
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDTOTE=:wk_Tote and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP#
                 and b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and
                 b.GCDSHP=a.GDDSHP and b.GCSTS='O');
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EXSR      $Bottom1
     C                   ELSE
     C                   EVAL      S@MORE = 'Bottom'
     C                   ENDIF                                                  wk_RcdCnt>*Zero
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

     C                   EVAL      C1QSTC = wk_DftQty
     C                   EVAL      C1SCAN = *Blanks
     C                   EVAL      C1ITEM = *Blanks
     C                   EVAL      C1LOT = *Blanks

     C     wk_Tote       SETGT     HYLSGTD1
     C                   DOU       %eof(HYLSGTD1)  or  wk_Include
     C     wk_Tote       READPE    HYLSGTD1
     C                   IF        not %eof(HYLSGTD1)
     C                   EXSR      $Filter1b
     C                   ENDIF                                                  not %eof(HYLSGTD1)
     C                   ENDDO                                                  %eof(HYLSGTD1) or
     C                   IF        %eof
     C                   EVAL      S@MORE = 'Bottom'
     C                   LEAVESR
     C                   ENDIF                                                  %eof
     C                   ENDIF                                                  *IN05

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   ENDIF                                                  %eof

     C                   DOW        not %eof(HYLSGTD1)

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

     C                   WRITE     SFL01B

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01PC = 1
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UGRP# = TDGRP#
     C                   EVAL      @UBLTO = TDBLTO
     C                   EVAL      @USHTO = TDSHTO
     C                   EVAL      @UDSHP = TDDSHP
     C                   EVAL      @UITEM = TDITEM
     C                   EVAL      @UTURN = TDTURN
     C                   EVAL      @UTSEQ = TDTSEQ
     C                   EVAL      @URSEQ = TDRSEQ
     C                   ENDIF                                                  wk_Sfl01PC=1
     C                   EVAL      @DGRP# = TDGRP#
     C                   EVAL      @DBLTO = TDBLTO
     C                   EVAL      @DSHTO = TDSHTO
     C                   EVAL      @DDSHP = TDDSHP
     C                   EVAL      @DITEM = TDITEM
     C                   EVAL      @DTURN = TDTURN
     C                   EVAL      @DTSEQ = TDTSEQ
     C                   EVAL      @DRSEQ = TDRSEQ

      *
      * Get next tote detail record.
      *
     C                   DOU       %eof(HYLSGTD1)  or  wk_Include
     C     wk_Tote       READE     HYLSGTD1
     C                   IF        not %eof(HYLSGTD1)
     C                   EXSR      $Filter1b
     C                   ENDIF                                                  not %eof(HYLSGTD1)
     C                   ENDDO                                                  %eof(HYLSGTD1) or

     C                   ENDDO                                                  not %eof(HYLSGTD1)

      *
      * Set More... or Bottom   subfile messge.
      *
     C                   IF        %eof(HYLSGTD1)
     C                   EVAL      S@MORE = 'Bottom'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSGTD1)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear subfile 01, initialize relative record and page count.           *
      **************************************************************************
     CSR   $ClearSfl01   BEGSR

     C                   EVAL      *IN26 = *ON
     C                   EVAL      *IN27 = *OFF
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   WRITE     SFLCTL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   WRITE     SFLCTL01B
     C                   ENDSL
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

     C     PGUKEY1       SETLL     HYLSGTD2
     C                   DOU       %eof(HYLSGTD2)  or  wk_Include
     C     key_TD2       READPE    HYLSGTD2
     C                   IF        not %eof(HYLSGTD2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGTD2)
     C                   ENDDO                                                  %eof(HYLSGTD2) or

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

     C                   DOW       not %eof(HYLSGTD2)
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
     C                   EVAL      @DITEM = TDITEM
     C                   EVAL      @DTURN = TDTURN
     C                   EVAL      @DTSEQ = TDTSEQ
     C                   EVAL      @DRSEQ = TDRSEQ
     C                   ENDIF                                                  wk_Sfl01RRN=wk_Sfl01
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UITEM = TDITEM
     C                   EVAL      @UTURN = TDTURN
     C                   EVAL      @UTSEQ = TDTSEQ
     C                   EVAL      @URSEQ = TDRSEQ

      *
      * Get previous tote detail work record.
      *
     C                   DOU       %eof(HYLSGTD2)  or  wk_Include
     C     key_TD2       READPE    HYLSGTD2
     C                   IF        not %eof(HYLSGTD2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGTD2)
     C                   ENDDO                                                  %eof(HYLSGTD2) or

     C                   ENDDO                                                  not %eof(HYLSGTD2)

      *
      * Set More... or Top if there are more records or BOF.
      * If Beg of File reached and not a full page, fill from top down.
      *
     C                   IF        %eof(HYLSGTD2)
     C                   EVAL      S@MORE = 'Top'
     C                   IF        wk_Sfl01PC <> wk_Sfl01PS
     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UTURN = *Zero
     C                   EVAL      @UTSEQ = *Zero
     C                   EVAL      @URSEQ = *Zero
     C                   EXSR      $PageDownSf1
     C                   ENDIF                                                  wk_Sfl01PC<>wk_Sfl01
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSGTD2)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      *  Page up subfile SFL01B - main subfile.                                *
      **************************************************************************
     CSR   $PageUpSf1b   BEGSR

     C     PGUKEY1b      SETLL     HYLSGTD1
     C                   DOU       %eof(HYLSGTD1)  or  wk_Include
     C     wk_Tote       READPE    HYLSGTD1
     C                   IF        not %eof(HYLSGTD1)
     C                   EXSR      $Filter1b
     C                   ENDIF                                                  not %eof(HYLSGTD1)
     C                   ENDDO                                                  %eof(HYLSGTD1) or

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

     C                   DOW       not %eof(HYLSGTD1)
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
     C                   WRITE     SFL01B

      *
      * Set Page Up and Page Down Keys
      *
     C                   IF        wk_Sfl01RRN = wk_Sfl01PS
     C                   EVAL      @DGRP# = TDGRP#
     C                   EVAL      @DBLTO = TDBLTO
     C                   EVAL      @DSHTO = TDSHTO
     C                   EVAL      @DDSHP = TDDSHP
     C                   EVAL      @DITEM = TDITEM
     C                   EVAL      @DTURN = TDTURN
     C                   EVAL      @DTSEQ = TDTSEQ
     C                   EVAL      @DRSEQ = TDRSEQ
     C                   ENDIF                                                  wk_Sfl01RRN=wk_Sfl01
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UGRP# = TDGRP#
     C                   EVAL      @UBLTO = TDBLTO
     C                   EVAL      @USHTO = TDSHTO
     C                   EVAL      @DDSHP = TDDSHP
     C                   EVAL      @UITEM = TDITEM
     C                   EVAL      @UTURN = TDTURN
     C                   EVAL      @UTSEQ = TDTSEQ
     C                   EVAL      @URSEQ = TDRSEQ

      *
      * Get previous tote detail work record.
      *
     C                   DOU       %eof(HYLSGTD1)  or  wk_Include
     C     wk_Tote       READPE    HYLSGTD1
     C                   IF        not %eof(HYLSGTD1)
     C                   EXSR      $Filter1b
     C                   ENDIF                                                  not %eof(HYLSGTD1)
     C                   ENDDO                                                  %eof(HYLSGTD1) or

     C                   ENDDO                                                  not %eof(HYLSGTD1)

      *
      * Set More... or Top if there are more records or BOF.
      * If Beg of File reached and not a full page, fill from top down.
      *
     C                   IF        %eof(HYLSGTD1)
     C                   EVAL      S@MORE = 'Top'
     C                   IF        wk_Sfl01PC <> wk_Sfl01PS
     C                   EVAL      @UGRP# = *Zero
     C                   EVAL      @UBLTO = *Zero
     C                   EVAL      @USHTO = *Zero
     C                   EVAL      @UDSHP = *Zero
     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UTURN = *Zero
     C                   EVAL      @UTSEQ = *Zero
     C                   EVAL      @URSEQ = *Zero
     C                   EXSR      $PageDownSf1b
     C                   ENDIF                                                  wk_Sfl01PC<>wk_Sfl01
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  %eof(HYLSGTD1)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move database fields to subfile fields.                                *
      **************************************************************************
     CSR   $MoveToSfl01  BEGSR

      /Free
       wk_W1QSCN = *Zero;
       EXEC SQL select coalesce(sum(W1QSCN),0)
                 into :wk_W1QSCN
                from HYW0608A
                where W1GRP#=:GDGRP# and W1TURN=:GDTURN and
                 W1TSEQ=:GDTSEQ and W1RSEQ=:GDRSEQ and
                 W1TOTE=:wk_Tote;

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
     C                   EVAL      S1GRP# = GDGRP#
     C                   EVAL      S1BLTO = GDBLTO
     C                   EVAL      S1SHTO = GDSHTO
     C                   EVAL      S1DSHP = GDDSHP
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EVAL      S1SHTOX = C1SHTO
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      S1SHTOX = '#' +
     C                               %triml(%editc(GDSHTO:'Z')) + '-' +
     C                               GDNAME
     C                   IF        GDDSHP <> *Zero
     C                   EVAL      %subst(S1SHTOX:39:5) = ' (DS)'
     C                   ENDIF                                                  GDDSHP<>*Zero
     C                   ENDSL
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
     C                   EVAL      S1QSTC = GDQSTC
     C                   EVAL      S1QSTCT = TDQSTCB
     C                   EVAL      S1QSTCX = TDQSTCB
     C                   EVAL      S1QSCN = wk_W1QSCN
     C                   EVAL      S1QSCNX = wk_W1QSCN
     C                   EVAL      S1CNVY = GDCNVY
     C                   EVAL      S1MXLT = GDMXLT
     C                   EVAL      S1PO# = GDPO#
     C                   EVAL      S1SSCCBC = GDSSCCBC
     C                   EVAL      S1SSCC = GDSSCC

     C                   SELECT
     C                   WHEN      wk_W1QSCN = *Zero
     C                   EVAL      S1TVSTS = *Blanks
     C****               WHEN      wk_W1QSCN < GDQOPK
     C                   WHEN      wk_W1QSCN < TDQSTCB
     C                   EVAL      S1TVSTS = 'Prtl verify'
     C                   OTHER
     C                   EVAL      S1TVSTS = 'Verified'
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL01 main subfile.                                             *
      **************************************************************************
     CSR   $Filter1      BEGSR

     C                   EVAL      wk_Include = *ON

     C     key_GD1       CHAIN     HYLSGDT11
     C                   IF        not %found(HYLSGDT11)
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  not %found(HYLSGDT11

     C                   IF        GDQSTC = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GDQSTC=*Zero

      *
      * If only showing not-done (i.e., not verified, or verified but
      * no UCC-128 label printed yet), exclude "done" pick lines.
      *
     C                   IF        not wk_ShowAll
      /Free
       wk_W1QSCN = *Zero;
       EXEC SQL select coalesce(sum(W1QSCN),0)
                 into :wk_W1QSCN
                from HYW0608A
                where W1GRP#=:GDGRP# and W1TURN=:GDTURN and
                 W1TSEQ=:GDTSEQ and W1RSEQ=:GDRSEQ and
                 W1TOTE=:wk_Tote;
      /End-Free
     C                   IF        wk_W1QSCN > *Zero  and
     C                               wk_W1QSCN = TDQSTCB  and
     C                               GDLBL1 = 'Y'
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_W1QSCN>*Zero and
     C                   ENDIF                                                  not wk_ShowAll

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL01B main subfile.                                            *
      **************************************************************************
     CSR   $Filter1b     BEGSR

     C                   EVAL      wk_Include = *ON

     C     key_GD1       CHAIN     HYLSGDT11
     C                   IF        not %found(HYLSGDT11)
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  not %found(HYLSGDT11

     C                   IF        GDQSTC = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GDQSTC=*Zero

     C                   IF        GDGRP# <> GCGRP#  or  GDBLTO <> GCBLTO  or
     C                               GDSHTO <> GCSHTO  or  GCDSHP <> GDDSHP
     C     key_GC        CHAIN     HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   EVAL      GCSTS = '-'
     C                   ENDIF                                                  not %found(HYPSGCU)
     C                   ENDIF                                                  GDGRP#<>GCGRP# or
     C                   IF        GCSTS <> 'O'
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GCSTS<>'O'

      *
      * If only showing not-done (i.e., not verified, or verified but
      * no UCC-128 label printed yet), exclude "done" pick lines.
      *
     C                   IF        not wk_ShowAll
      /Free
       wk_W1QSCN = *Zero;
       EXEC SQL select coalesce(sum(W1QSCN),0)
                 into :wk_W1QSCN
                from HYW0608A
                where W1GRP#=:GDGRP# and W1TURN=:GDTURN and
                 W1TSEQ=:GDTSEQ and W1RSEQ=:GDRSEQ and
                 W1TOTE=:wk_Tote;
      /End-Free
     C                   IF        wk_W1QSCN > *Zero  and
     C                               wk_W1QSCN = TDQSTCB  and
     C                               GDLBL1 = 'Y'
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_W1QSCN>*Zero and
     C                   ENDIF                                                  not wk_ShowAll

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
     C                   EVAL      wk_LblPr = *OFF
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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   READC     SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   READC     SFL01B
     C                   ENDSL

     C                   DOW       not %eof(HYD0608A)

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
     C                   IF        (S1OPT = @SEL  or  S1OPT = @SEL2)

     C                   EVAL      S1OPT = @SEL

     C                   IF        *IN08  or  *IN09

     C                   SELECT
     C                   WHEN      *IN08
     C                   EVAL      wk_LblType = 'M'
     C                   WHEN      *IN09
     C                   EVAL      wk_LblType = 'P'
     C                   ENDSL

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EVAL      wk_SelChk =
     C                               SelOK(wk_ShipGrp#:S1TURN:S1TSEQ:S1RSEQ:
     C                                     S1SSCCBC:
     C                                     S1ORD#:S1ITEM:wk_LblType)
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      wk_SelChk =
     C                               SelOK(S1GRP#:S1TURN:S1TSEQ:S1RSEQ:
     C                                     S1SSCCBC:
     C                                     S1ORD#:S1ITEM:wk_LblType)
     C                   ENDSL

     C                   IF        wk_SelChk = *Zeros

     C                   EVAL      wk_SelPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

      *
      * If first select, clear select subfile and save first select
      * subfile relative record number.
      *
     C                   IF        wk_ClrDone = *OFF
     C                   EVAL      wk_ClrDone = *ON
     C                   EVAL      wk_FirstSel = wk_Sfl01RRN

     C                   IF        wk_PgmMode = MaintMode2
     C                   EVAL      wk_SelGrp# = S1GRP#
     C                   EVAL      wk_SelShto = S1SHTOX
     C                   ENDIF                                                  wk_PgmMode=MaintMode

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
     C                   EVAL      wk_MsgID = 'ERR1244'
     C                   EVAL      wk_MsgData = wk_SelChk
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
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
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(S1GRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(S1BLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(S1SHTO:'Z'))
     C                   IF        S1DSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(S1DSHP:'Z'))
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   ENDSL
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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ENDIF                                                  *IN08 or *IN09

     C                   ELSE

      *
      * Changes present?
      *
     C                   IF        S1OPT = @CHG  or  S1OPT = @CHG2

     C                   EVAL      S1OPT = @CHG
     C                   EVAL      wk_ChgPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ELSE

      *
      * Displays present?
      *
     C                   IF        S1OPT = @DSP  or  S1OPT = @DSP2

     C                   EVAL      S1OPT = @DSP
     C                   EVAL      wk_DspPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ELSE

      *
      * Labels present?
      *
     C                   IF        S1OPT = @LBL

     C                   SELECT
     C                   WHEN      wk_PgmMode = Maintmode1
     C                   EVAL      wk_LblChk = LblOK(wk_ShipGrp#:S1TURN:
     C                               S1TSEQ:S1RSEQ:S1SSCCBC:S1SSCC)
     C                   WHEN      wk_PgmMode = Maintmode2
     C                   EVAL      wk_LblChk = LblOK(S1GRP#:S1TURN:
     C                               S1TSEQ:S1RSEQ:S1SSCCBC:S1SSCC)
     C                   ENDSL

     C                   IF        wk_LblChk = *Zeros

     C                   EVAL      wk_LblPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1272'
     C                   EVAL      wk_MsgData = wk_LblChk
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
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
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(S1GRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(S1BLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(S1SHTO:'Z'))
     C                   IF        S1DSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(S1DSHP:'Z'))
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   ENDSL
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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL
     C                   ENDIF                                                  wk_LblChk=*Zeros

     C                   ELSE

      *
      * Lot inquirys present?
      *
     C                   IF        S1OPT = @LOTINQ

     C                   EVAL      wk_LotInqPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ELSE

      *
      * Delete labels present?
      *
     C                   IF        S1OPT = @DLTLBL

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EVAL      wk_DltLblChk = DltLblOK(wk_ShipGrp#:
     C                               S1TURN:S1TSEQ:S1RSEQ:S1SSCCBC)
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      wk_DltLblChk = DltLblOK(S1GRP#:
     C                               S1TURN:S1TSEQ:S1RSEQ:S1SSCCBC)
     C                   ENDSL

     C                   IF        wk_DltLblChk = *Zeros

     C                   EVAL      wk_DltLblPr = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1259'
     C                   EVAL      wk_MsgData = wk_DltLblChk
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
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
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      %subst(wk_MsgData:3:9) =
     C                               %triml(%editc(S1GRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:12:7) =
     C                               %triml(%editc(S1BLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:19:7) =
     C                               %triml(%editc(S1SHTO:'Z'))
     C                   IF        S1DSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:26:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:26:7) =
     C                               %triml(%editc(S1DSHP:'Z'))
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   ENDSL
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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL
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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL
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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ENDIF                                                   S1OPT<>*Blanks

     C                   ENDIF                                                   S1OPT=@DLTLBL

     C                   ENDIF                                                   S1OPT=@LOTINQ

     C                   ENDIF                                                   S1OPT=@LBL

     C                   ENDIF                                                   S1OPT=@DSP or

     C                   ENDIF                                                   S1OPT=@CHG or

     C                   ENDIF                                                   S1OPT=@SEL or

      *
      * Get next changed subfile record.
      *
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   READC     SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   READC     SFL01B
     C                   ENDSL

     C                   ENDDO                                                  not %eof(HYD0608A)

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
     C                   IF        wk_DspPr  or  wk_LblPr  or
     C                               wk_ChgPr  or
     C                               wk_LotInqPr  or  wk_DltLblPr
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_DspPr or
     C                   ELSE
     C                   IF        wk_SelPr  or  wk_DspPr  or
     C                               wk_ChgPr  or
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

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   READC     SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   READC     SFL01B
     C                   ENDSL

     C                   DOW       not %eof(HYD0608A)

      *
      *  F12=Cancel Key
      *
     C                   IF        not *IN12

     C                   SELECT

     C                   WHEN      S1OPT = @CHG
     C                   EXSR      $Change

     C                   WHEN      S1OPT = @DSP
     C                   EXSR      $Display

     C                   WHEN      S1OPT = @LBL
     C                   EXSR      $PrtLabel

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

     C                   IF        *IN12  or  S1OPT = @SEL
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   ENDIF                                                   *IN12
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   EVAL      S@RCNO = wk_Sfl01RRN

     C                   ELSE

      *
      * Cancel requested - save remaining options.
      *
     C                   EVAL      *IN28 = *ON
     C                   EVAL      *IN61 = S1IN61
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   UPDATE    SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   UPDATE    SFL01B
     C                   ENDSL

     C                   ENDIF                                                  not *IN12

      *
      * Get next changed subfile record (save status of F12=Cancel).
      *
     C                   IF        not %eof(HYD0608A)
     C                   EVAL      wk_IN12 = *IN12
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   READC     SFL01A
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   READC     SFL01B
     C                   ENDSL
     C                   EVAL      *IN12 = wk_IN12
     C                   ENDIF                                                  not %eof(HYD0608A)

     C                   ENDDO                                                  not %eof(HYD0608A)

      *
      * If "selects" are present and Not F12=Cancel, process "selects".
      *
     C                   IF        wk_SelPr  and  not *IN12
     C                   IF        *IN08  or  *IN09
     C                   EVAL      S@RCNO = wk_FirstSel
     C                   EXSR      $ProcSelects
     C                   IF        not *IN12  and  wk_Refresh
     C                   EVAL      *IN05 = *ON
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageDownSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageDownSf1b
     C                   ENDSL
     C                   EVAL      *IN05 = *OFF
     C                   EVAL      wk_Refresh = *OFF
     C                   ENDIF                                                  not *IN12
     C                   ENDIF                                                  *IN08 or *IN09
     C                   ENDIF                                                  wk_SelPr and

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
     C                   EVAL      C2HDG2 = *Blanks
     C                   ENDSL

     C                   IF        wk_PgmMode = MaintMode2
     C                   EVAL      C1GRP# = %triml(%editc(wk_SelGrp#:'Z'))
     C                   EVAL      C1SHTO = wk_SelShto
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   DOW       not *IN12
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
     C                   IF        *IN25

     C                   ELSE

     C                   EVAL      wk_LblQty = *Zero
     C                   EVAL      wk_LblUOM = *Blanks

      *
      * Read & process selects, then exit.
      *
     C                   READC     SFL02A

     C                   DOW       not %eof(HYD0608A)

      /Free
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL insert into HYW0602
                       (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                        W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                        W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                values(:ps_Job#, :wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                       :wk_DropShip#, :S1TURN, :S1TSEQ, :S1RSEQ,
                       :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);
       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL insert into HYW0602
                       (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                        W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                        W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                values(:ps_Job#, :S1GRP#, :S1BLTO, :S1SHTO,
                       :S1DSHP, :S1TURN, :S1TSEQ, :S1RSEQ,
                       :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);
       ENDSL;
      /End-Free

     C                   EVAL      wk_LblQty = wk_LblQty + S1QSCN
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

     C                   ENDDO                                                  not %eof(HYD0608A)

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
     C                   ENDSL

       SELECT;
       WHEN wk_PgmMode = MaintMode1;

       wk_Pgm02 = *ON;
       wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:wk_SSCCBC:
                                 wk_LblType:*ON:*ON:wk_LblQty:wk_LblUOM);

       EXEC SQL update HYW0608A a
                 set W1SSCC=:wk_SSCC
                where a.W1GRP#=:wk_ShipGrp# and a.W1BLTO=:wk_BillTo# and
                 a.W1SHTO=:wk_ShipTo# and a.W1DSHP=:wk_DropShip# and
                 a.W1SSCC=0 and
                 exists (select * from HYW0602 b where
                 b.W1JOB#=:ps_Job# and b.W1GRP#=a.W1GRP# and
                 b.W1BLTO=a.W1BLTO and b.W1SHTO=a.W1SHTO and
                 b.W1DSHP=a.W1DSHP and b.W1OCTL=a.W1OCTL and
                 b.W1ORD#=a.W1ORD# and b.W1ORL#=a.W1ORL# and
                 b.W1BLN#=a.W1BLN# and b.W1TURN=a.W1TURN and
                 b.W1TSEQ=a.W1TSEQ and b.W1RSEQ=a.W1RSEQ);
       EXEC SQL delete from HYW0602
                where W1JOB#=:ps_Job#;

       WHEN wk_PgmMode = MaintMode2;

       wk_Pgm02 = *ON;
       wk_RC0622 = PrtUCC128Lbl (S1GRP#:S1ORD#:S1ORL#:S1BLN#:wk_SSCCBC:
                                 wk_LblType:*ON:*ON:wk_LblQty:wk_LblUOM);

       EXEC SQL update HYW0608A a
                 set W1SSCC=:wk_SSCC
                where a.W1GRP#=:S1GRP# and a.W1BLTO=:S1BLTO and
                 a.W1SHTO=:S1SHTO and a.W1DSHP=:S1DSHP and
                 a.W1SSCC=0 and
                 exists (select * from HYW0602 b where
                 b.W1JOB#=:ps_Job# and b.W1GRP#=a.W1GRP# and
                 b.W1BLTO=a.W1BLTO and b.W1SHTO=a.W1SHTO and
                 b.W1DSHP=a.W1DSHP and b.W1OCTL=a.W1OCTL and
                 b.W1ORD#=a.W1ORD# and b.W1ORL#=a.W1ORL# and
                 b.W1BLN#=a.W1BLN# and b.W1TURN=a.W1TURN and
                 b.W1TSEQ=a.W1TSEQ and b.W1RSEQ=a.W1RSEQ);
       EXEC SQL delete from HYW0602
                where W1JOB#=:ps_Job#;

       ENDSL;
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

     C                   ENDDO                                                  not *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write HYPSCWK records.                                                 *
      **************************************************************************
     CSR   $WrtHYPSCWK   BEGSR


     C                   IF        not %open(HYW0602)
     C                   OPEN      HYW0602
     C                   ENDIF                                                  not %open(HYW0602)

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

     C     key_W3        SETLL     HYW0602
     C                   DOU       %eof(HYW0602)
     C     key_W3        READE     HYW0602
     C                   IF        %eof(HYW0602)
     C                   ITER
     C                   ENDIF                                                  %eof(HYW0602)

      /Free
       wk_GDTURN = *Zero;
       wk_GDTSEQ = *Zero;
       wk_GDITEM = *Blanks;
       wk_GDIMDS = *Blanks;
       wk_GDPLOC = *Blanks;
       wk_GDQSTC = *Zero;
       IF wk_LblType <> 'F';
         EXEC SQL select GDTURN, GDTSEQ, GDITEM, GDIMDS,
                         GDPLOC, GDQSTC
                   into :wk_GDTURN, :wk_GDTSEQ, :wk_GDITEM, :wk_GDIMDS,
                        :wk_GDPLOC, :wk_GDQSTC
                  from HYPSGDT
                  where GDGRP#=:W3GRP# and GDBLTO=:W3BLTO and
                   GDSHTO=:W3SHTO and GDDSHP=:W3DSHP and
                   GDOCTL=:W3OCTL and GDORD#=:W3ORD# and
                   GDORL#=:W3ORL# and GDBLN#=:W3BLN# and
                   GDRSEQ=:W3RSEQ;
       ELSE;
         EXEC SQL select GDTURN, GDTSEQ, GDITEM, GDIMDS,
                         GDPLOC, GDQSTC
                   into :wk_GDTURN, :wk_GDTSEQ, :wk_GDITEM, :wk_GDIMDS,
                        :wk_GDPLOC, :wk_GDQSTC
                  from HYPSGDT
                  where GDGRP#=:W3GRP# and GDBLTO=:W3BLTO and
                   GDSHTO=:W3SHTO and GDDSHP=:W3DSHP and
                   GDOCTL=:W3OCTL and GDORD#=:W3ORD# and
                   GDORL#=:W3ORL# and GDBLN#=:W3BLN# and
                   GDRSEQ=:W3RSEQ;
       ENDIF;                                                                // wk_LblType<>'F'
       IF wk_GDTURN = *Zero;
         ITER;
       ENDIF;                                                                // wk_GDTURN=*Zero

       wk_W1QSCN = *Zero;
       EXEC SQL select coalesce(sum(W1QSCN),0)
                 into :wk_W1QSCN
                from HYW0608A
                where W1GRP#=:W3GRP# and W1TURN=:W3TURN and
                 W1TSEQ=:W3TSEQ and W1RSEQ=:W3RSEQ;

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
                where LDGRP#=:W3GRP# and LDTURN=:wk_GDTURN and
                 LDTSEQ=:wk_GDTSEQ and LDRSEQ=:W3RSEQ;
       IF wk_LotCnt = *Zero;
         EXEC SQL select count(*) into :wk_LotCnt
                  from HYW0608B
                  where W2GRP#=:W3GRP# and W2TURN=:wk_GDTURN and
                   W2TSEQ=:wk_GDTSEQ and W2RSEQ=:W3RSEQ;
       ENDIF;                                                                // wk_LotCnt=*Zero

       wk_Carton# = *Zero;
       EXEC SQL select coalesce(max(S1CTN#),0) into :wk_Carton#
                from HYPSCWK
                where S1ORD#=:W3ORD# and S1TURN=:wk_GDTURN;
       wk_Carton# = wk_Carton# + 1;
      /End-Free

     C                   EVAL      SCORD# = W3ORD#
     C                   EVAL      SCSLOC = %scanrpl('-':'':wk_GDPLOC)
     C                   EVAL      SCORL# = W3ORL#
     C                   EVAL      SCBLN# = W3BLN#
     C                   EVAL      SCSRL# = wk_SSCC
     C                   EVAL      SCITEM = wk_GDITEM
     C                   EVAL      SCIMDS = wk_GDIMDS
     C                   EVAL      SCUOM = wk_IMUOMS
     C****               EVAL      SCUOMQ = wk_W1QSCN
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
     C                   EVAL      SCEMTY = 'B'                                 BoxLogix
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
     C                   EVAL      wk_SCWK = *ON
     C                   ELSE
     C                   EVAL      wk_TotLDQSTC = *Zero
     C     key_LD2       SETGT     HYPSGLD
     C                   DOU       %eof(HYPSGLD)
     C     key_LD2       READPE    HYPSGLD
     C                   IF        %eof(HYPSGLD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGLD)
     C                   IF        LDSSCC <> *Zero  and  LDSSCC <> wk_SSCC
     C                   UNLOCK    HYPSGLD
     C                   ITER
     C                   ENDIF                                                  LDSSCC<>*Zero and
     C                   IF        LDRSEQ <> W3RSEQ
     C                   UNLOCK    HYPSGLD
     C                   ITER
     C                   ENDIF                                                  LDRSEQ<>W3RSEQ
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSCWK
                where S1SRL#=:SCSRL# and S1ORD#=:SCORD# and S1ORL#=:SCORL# and
                      S1BLN#=:SCBLN# and S1LOT#=:LDLOT#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      SCLOT# = LDLOT#
     C                   EVAL      SCUOMQ = LDQSTC
     C                   EVAL      SCEAQ = LDQSTC * wk_IMUDN1
     C                   EVAL(H)   SCEWGT = LDQSTC * wk_UnitWgt
     C                   WRITE     HYRSCWK
     C                   ELSE
      /Free
       EXEC SQL update HYPSCWK
                 set S1UOMQ=S1UOMQ+:LDQSTC
                where S1SRL#=:SCSRL# and S1ORD#=:SCORD# and S1ORL#=:SCORL# and
                      S1BLN#=:SCBLN# and S1LOT#=:LDLOT#;
       EXEC SQL update HYPSCWK
                 set S1EAQ=S1UOMQ*S1QPER, S1EWGT=round(S1UOMQ*S1UWGT,0)
                where S1SRL#=:SCSRL# and S1ORD#=:SCORD# and S1ORL#=:SCORL# and
                      S1BLN#=:SCBLN# and S1LOT#=:LDLOT#;
      /End-Free
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   EVAL      wk_SCWK = *ON
     C                   EVAL      LDSSCC = wk_SSCC
     C                   UPDATE    HYRSGLD
     C                   EVAL      wk_TotLDQSTC = wk_TotLDQSTC + LDQSTC
     C                   IF        wk_TotLDQSTC >= wk_GDQSTC
     C                   LEAVE
     C                   ENDIF                                                  wk_TotLDQSTC>=wk_GDQ
     C                   ENDDO                                                  %eof(HYPSGLD)
     C                   IF        wk_TotLDQSTC < wk_GDQSTC
     C     key_W2C       SETGT     HYW0608B
     C                   DOU       %eof(HYW0608B)
     C     key_W2C       READPE    HYW0608B
     C                   IF        %eof(HYW0608B)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0808B)
     C                   IF        W2SSCC <> *Zero  and  W2SSCC <> wk_SSCC
     C                   UNLOCK    HYW0608B
     C                   ITER
     C                   ENDIF                                                  W2SSCC<>*Zero and
     C                   IF        W2RSEQ <> W3RSEQ
     C                   UNLOCK    HYW0608B
     C                   ITER
     C                   ENDIF                                                  W2RSEQ<>W3RSEQ
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSCWK
                where S1SRL#=:SCSRL# and S1ORD#=:SCORD# and S1ORL#=:SCORL# and
                      S1BLN#=:SCBLN# and S1LOT#=:W2LOT#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      SCLOT# = W2LOT#
     C                   EVAL      SCUOMQ = W2QSTC
     C                   EVAL      SCEAQ = W2QSTC * wk_IMUDN1
     C                   EVAL(H)   SCEWGT = W2QSTC * wk_UnitWgt
     C                   WRITE     HYRSCWK
     C                   ELSE
      /Free
       EXEC SQL update HYPSCWK
                 set S1UOMQ=S1UOMQ+:W2QSTC
                where S1SRL#=:SCSRL# and S1ORD#=:SCORD# and S1ORL#=:SCORL# and
                      S1BLN#=:SCBLN# and S1LOT#=:W2LOT#;
       EXEC SQL update HYPSCWK
                 set S1EAQ=S1UOMQ*S1QPER, S1EWGT=round(S1UOMQ*S1UWGT,0)
                where S1SRL#=:SCSRL# and S1ORD#=:SCORD# and S1ORL#=:SCORL# and
                      S1BLN#=:SCBLN# and S1LOT#=:W2LOT#;
      /End-Free
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   EVAL      wk_SCWK = *ON
     C                   EVAL      W2SSCC = wk_SSCC
     C                   UPDATE    HYW0608BR
     C                   EVAL      wk_TotLDQSTC = wk_TotLDQSTC + W2QSTC
     C                   IF        wk_TotLDQSTC >= wk_GDQSTC
     C                   LEAVE
     C                   ENDIF                                                  wk_TotLDQSTC>=wk_GDQ
     C                   ENDDO                                                  %eof(HYW0608B)
     C                   ENDIF                                                  wk_TotLDQSTC < wk_GD
     C                   ENDIF                                                  wk_LotCnt=*Zero

     C                   ENDDO                                                  %eof(HYW0602)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Print a partial label (or reprint any prior-generated label).          *
      **************************************************************************
     CSR   $PrtLabel     BEGSR

      /Free
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
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

       WHEN wk_PgmMode = MaintMode2;
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
                  where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                   GDSHTO=:S1SHTO and GDDSHP=:S1DSHP and
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
                  where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                   GDSHTO=:S1SHTO and GDDSHP=:S1DSHP and
                   GDTURN=:S1TURN and GDTSEQ=:S1TSEQ and GDSSCCBC=:S1SSCCBC
                   and GDRSEQ=:S1RSEQ;
       ENDIF;                                                                // S1SSCCBC=*Blanks

       ENDSL;

       IF wk_GDSSCCBC = *Blanks  or  wk_GDLBL1 = 'N';
         wk_ReprtLbl = *OFF;
       ELSE;
         wk_ReprtLbl = *ON;
       ENDIF;                                                                // wk_GDSSCCBC=*Blanks

       IF not wk_ReprtLbl;
         EXEC SQL delete from HYW0602
                  where W1JOB#=:ps_Job#;
        SELECT;
        WHEN wk_PgmMode = MaintMode1;
         EXEC SQL insert into HYW0602
                         (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                          W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                          W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                  values(:ps_Job#, :wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                         :wk_DropShip#, :S1TURN, :S1TSEQ, :S1RSEQ,
                         :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);
        WHEN wk_PgmMode = MaintMode2;
         EXEC SQL insert into HYW0602
                         (W1JOB#, W1GRP#, W1BLTO, W1SHTO,
                          W1DSHP, W1TURN, W1TSEQ, W1RSEQ,
                          W1OCTL, W1ORD#, W1ORL#, W1BLN#)
                  values(:ps_Job#, :S1GRP#, :S1BLTO, :S1SHTO,
                         :S1DSHP, :S1TURN, :S1TSEQ, :S1RSEQ,
                         :S1OCTL, :S1ORD#, :S1ORL#, :S1BLN#);
        ENDSL;
      // IF wk_GDCNVY = 'C';
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
      // ELSE;
      //   wk_LblType = 'P';
      // ENDIF;                                                              // wk_GDCNVY='C'
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
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       IF not wk_ReprtLbl;
         wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:
                                   wk_SSCCBC:wk_LblType:*ON:*ON:
                                   wk_LblQty:wk_LblUOM);
       ELSE;
         wk_RC0622 = PrtUCC128Lbl (wk_ShipGrp#:S1ORD#:S1ORL#:S1BLN#:
                                   wk_GDSSCCBC:wk_LblType:*ON:*ON:
                                   wk_LblQty:wk_LblUOM);
       ENDIF;                                                                // not wk_ReprtLbl
       WHEN wk_PgmMode = MaintMode2;
       IF not wk_ReprtLbl;
         wk_RC0622 = PrtUCC128Lbl (S1GRP#:S1ORD#:S1ORL#:S1BLN#:
                                   wk_SSCCBC:wk_LblType:*ON:*ON:
                                   wk_LblQty:wk_LblUOM);
       ELSE;
         wk_RC0622 = PrtUCC128Lbl (S1GRP#:S1ORD#:S1ORL#:S1BLN#:
                                   wk_GDSSCCBC:wk_LblType:*ON:*ON:
                                   wk_LblQty:wk_LblUOM);
       ENDIF;                                                                // not wk_ReprtLbl
       ENDSL;

       IF not wk_ReprtLbl  and  wk_GDCNVY <> 'C';
        SELECT;
        WHEN wk_PgmMode = MaintMode1;
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
         EXEC SQL update HYW0608A
                   set W1SSCC=:wk_SSCC
                  where W1GRP#=:wk_ShipGrp# and W1TURN=:S1TURN and
                        W1TSEQ=:S1TSEQ and W1RSEQ=:S1RSEQ and
                        W1TOTE=:wk_Tote and W1SSCC=0;
        WHEN wk_PgmMode = MaintMode2;
         EXEC SQL update HYPSGDT a
                   set GDLBL1='Y', GDSSCC=:wk_SSCC, GDSSCCBC=:wk_SSCCBC
                  where a.GDGRP#=:S1GRP# and a.GDBLTO=:S1BLTO and
                   a.GDSHTO=:S1SHTO and a.GDDSHP=:S1DSHP and
                   a.GDSSCCBC=' ' and a.GDRSEQ=:S1RSEQ and
                   exists (select * from HYW0602 b where
                   b.W1JOB#=:ps_Job# and b.W1GRP#=a.GDGRP# and
                   b.W1BLTO=a.GDBLTO and b.W1SHTO=a.GDSHTO and
                   b.W1DSHP=a.GDDSHP and b.W1OCTL=a.GDOCTL and
                   b.W1ORD#=a.GDORD# and b.W1ORL#=a.GDORL# and
                   b.W1BLN#=a.GDBLN# and b.W1RSEQ=a.GDRSEQ);
         EXEC SQL update HYW0608A
                   set W1SSCC=:wk_SSCC
                  where W1GRP#=:S1GRP# and W1TURN=:S1TURN and
                        W1TSEQ=:S1TSEQ and W1RSEQ=:S1RSEQ and
                        W1TOTE=:wk_Tote and W1SSCC=0;
        ENDSL;
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
          IF wk_LblType = 'F';
           wk_MsgData = 'Full carton';
           %subst(wk_MsgData:15:30) = ' for item ' +
             %trimr(S1ITEM) + '.';
          ELSE;
           wk_MsgData = 'Partial carton';
           %subst(wk_MsgData:15:30) = ' for item ' +
             %trimr(S1ITEM) + '.';
          ENDIF;                                                             // wk_LblType='F'
         ENDIF;                                                              // wk_LblType='M'
         EXSR $SndPM;
       ELSE;
         IF not wk_ReprtLbl  and  wk_GDCNVY = 'C';
          SELECT;
          WHEN wk_PgmMode = MaintMode1;
           EXEC SQL update HYPSGDT a
                     set GDLBL1='Y'
                    where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                     a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                     a.GDSSCCBC=:wk_GDSSCCBC and a.GDRSEQ=:S1RSEQ;
          WHEN wk_PgmMode = MaintMode2;
           EXEC SQL update HYPSGDT a
                     set GDLBL1='Y'
                    where a.GDGRP#=:S1GRP# and a.GDBLTO=:S1BLTO and
                     a.GDSHTO=:S1SHTO and a.GDDSHP=:S1DSHP and
                     a.GDSSCCBC=:wk_GDSSCCBC and a.GDRSEQ=:S1RSEQ;
          ENDSL;
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
       wk_W1SSCC = *Zero;

       EXEC SQL delete from HYPEDWK
                where EWSRL#=:S1SSCC;

       EXEC SQL delete from HYPSCWK
                where S1SRL#=:S1SSCC;

       SELECT;

       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL update HYPSGLD
                set LDSSCC=0
                where LDGRP#=:wk_ShipGrp# and LDTURN=:S1TURN and
                 LDTSEQ=:S1TSEQ and LDSSCC=:S1SSCC;
       EXEC SQL update HYPSGDT
                set GDSSCCBC=' ', GDSSCC=0, GDLBL1='N', GDLBL2='N'
                where GDGRP#=:wk_ShipGrp# and GDSSCCBC=:S1SSCCBC;
       IF S1SSCC <> *Zero;
         EXEC SQL update HYW0608A
                  set W1SSCC=0
                  where W1GRP#=:wk_ShipGrp# and W1SSCC=:S1SSCC;
       ELSE;
         wk_W1SSCC = *Zero;
         EXEC SQL select W1SSCC into :wk_W1SSCC
                  from HYW0608A
                  where W1GRP#=:wk_ShipGrp# and W1TURN=:S1TURN and
                        W1TSEQ=:S1TSEQ and W1RSEQ=:S1RSEQ
                  fetch first row only;
         IF wk_W1SSCC <> *Zero;
           EXEC SQL update HYW0608A
                    set W1SSCC=0
                    where W1GRP#=:wk_ShipGrp# and W1SSCC=:wk_SSCC;
         ENDIF;                                                              // wk_W1SSCC<>*Zero
       ENDIF;                                                                // S1SSCC<>*Zero

       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL update HYPSGLD
                set LDSSCC=0
                where LDGRP#=:S1GRP# and LDTURN=:S1TURN and
                 LDTSEQ=:S1TSEQ and LDSSCC=:S1SSCC;
       EXEC SQL update HYPSGDT
                set GDSSCCBC=' ', GDSSCC=0, GDLBL1='N', GDLBL2='N'
                where GDGRP#=:S1GRP# and GDSSCCBC=:S1SSCCBC;
       IF S1SSCC <> *Zero;
         EXEC SQL update HYW0608A
                  set W1SSCC=0
                  where W1GRP#=:S1GRP# and W1SSCC=:S1SSCC;
       ELSE;
         wk_W1SSCC = *Zero;
         EXEC SQL select W1SSCC into :wk_W1SSCC
                  from HYW0608A
                  where W1GRP#=:S1GRP# and W1TURN=:S1TURN and
                        W1TSEQ=:S1TSEQ and W1RSEQ=:S1RSEQ
                  fetch first row only;
         IF wk_W1SSCC <> *Zero;
           EXEC SQL update HYW0608A
                    set W1SSCC=0
                    where W1GRP#=:S1GRP# and W1SSCC=:wk_SSCC;
         ENDIF;                                                              // wk_W1SSCC<>*Zero
       ENDIF;                                                                // S1SSCC<>*Zero

       ENDSL;

       wk_Refresh = *ON;

      //
      // Send "Label has been deleted" message.
      //
       wk_MsgID = 'MSG0383';
       IF S1SSCCBC <> *Blanks;
       wk_MsgData = S1SSCCBC;
       ELSE;
         wk_MsgData = %triml(%editc(wk_W1SSCC:'Z'));
       ENDIF;                                                                // S1SSCCBC<>*Blanks
       %subst(wk_MsgData:21:7) = %triml(%editc(wk_ShipTo#:'Z'));
       EXSR $SndPM;
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display subfile SFL01 from the top after F19.                          *
      **************************************************************************
     CSR   $SflAfterF19  BEGSR

     C                   EVAL      C1QSTC = wk_DftQty
     C                   EVAL      C1SCAN = *Blanks
     C                   EVAL      C1ITEM = *Blanks
     C                   EVAL      C1LOT = *Blanks
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      *IN73 = *OFF
     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UTURN = *Zero
     C                   EVAL      @UTSEQ = *Zero
     C                   EVAL      @URSEQ = *Zero
     C                   EVAL      *IN05 = *ON

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageDownSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageDownSf1b
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display subfile SFL01 from the top.                                    *
      **************************************************************************
     CSR   $Top1         BEGSR

     C                   IF        wk_PgmMode = MaintMode2
     C                   EVAL      @UGRP# = *Zero
     C                   EVAL      @UBLTO = *Zero
     C                   EVAL      @USHTO = *Zero
     C                   EVAL      @UDSHP = *Zero
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   EVAL      @UITEM = *Loval
     C                   EVAL      @UTURN = *Zero
     C                   EVAL      @UTSEQ = *Zero
     C                   EVAL      @URSEQ = *Zero
     C                   EVAL      *IN05 = *ON
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageDownSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageDownSf1b
     C                   ENDSL
     C                   IF        *IN17
     C                   EVAL      S@MORE = 'Top'
     C                   ENDIF                                                  *IN17

     CSR                 ENDSR

     C/SPACE 3
      **************************************************************************
      * Display subfile SFL01 from the bottom.                                 *
      **************************************************************************
     CSR   $Bottom1      BEGSR

     C                   IF        wk_PgmMode = MaintMode2
     C                   EVAL      @UGRP# = *Hival
     C                   EVAL      @UBLTO = *Hival
     C                   EVAL      @USHTO = *Hival
     C                   EVAL      @UDSHP = *Hival
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   EVAL      @UITEM = *Hival
     C                   EVAL      @UTURN = *Hival
     C                   EVAL      @UTSEQ = *Hival
     C                   EVAL      @URSEQ = *Hival
     C                   EVAL      *IN05 = *ON
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EXSR      $PageUpSf1
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EXSR      $PageUpSf1b
     C                   ENDSL
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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C     key_GD2       CHAIN     HYLSGDT11
     C     key_TD4a      CHAIN     HYPSGTD
     C                   WHEN      wk_PgmMode = MaintMode2
     C     key_GD2b      CHAIN     HYLSGDT11
     C     key_TD4b      CHAIN     HYPSGTD
     C                   ENDSL
     C                   IF        not %found(HYLSGDT11)
     C                   CLEAR     *ALL          HYRSGDT11
     C                   ENDIF                                                  not %found(HYLSGDT11
     C                   IF        not %found(HYPSGTD)
     C                   CLEAR     *ALL          HYRSGTD
     C                   ENDIF                                                  not %found(HYPSGTD)

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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C     key_GD2       CHAIN     HYLSGDT11
     C     key_TD4a      CHAIN     HYPSGTD
     C                   WHEN      wk_PgmMode = MaintMode2
     C     key_GD2b      CHAIN     HYLSGDT11
     C     key_TD4b      CHAIN     HYPSGTD
     C                   ENDSL
     C                   IF        not %found(HYLSGDT11)
     C                   CLEAR     *ALL          HYRSGDT11
     C                   ENDIF                                                  not %found(HYLSGDT11
     C                   IF        not %found(HYPSGTD)
     C                   CLEAR     *ALL          HYRSGTD
     C                   ENDIF                                                  not %found(HYPSGTD)
     C                   EXSR      $MoveToFmt2
     C                   ITER
     C                   ENDIF                                                  *IN05

      *
      * Enter pressed.  Edit screen data.
      *
     C                   EXSR      $Edit2a
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
       /Free
        IF F2QOPK > sv_QOPK;
          EXEC SQL update HYPSGDT
                    set GDQOPK=:F2QOPK, GDLBL1='N'
                   where GDGRP#=:GDGRP# and GDTURN=:GDTURN and
                         GDTSEQ=:GDTSEQ and GDRSEQ=:GDRSEQ;
        ELSE;
          EXEC SQL update HYPSGDT
                    set GDQOPK=:F2QOPK
                   where GDGRP#=:GDGRP# and GDTURN=:GDTURN and
                         GDTSEQ=:GDTSEQ and GDRSEQ=:GDRSEQ;
        ENDIF;                                                               // F2QOPK>sv_QOPK
       /End-Free

     C                   IF        SQLER3 = 1                                   If 1 record updated
     C                   EVAL      wk_MsgID = 'MSG0382'
     C                   EVAL      wk_MsgData = %triml(%editc(GDGRP#:'Z'))
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
     C                               %triml(%editc(GDTURN:'Z'))
     C                   EVAL      %subst(wk_MsgData:40:3) =
     C                               %triml(%editc(GDTSEQ:'Z'))
     C                   EVAL      %subst(wk_MsgData:43:20) = GDSSCCBC
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR9024'
     C                   EVAL      wk_MsgData = 'HYPSGDT'
     C                   IF        SQLER3 = *Zero
     C                   EVAL      %subst(wk_MsgData:11:5) = 'Zero'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:11:5) = %char(SQLER3)
     C                   ENDIF                                                  SQLER3=*zero
     C                   EVAL      %subst(wk_MsgData:16:10) = ps_Pgnm
     C                   EVAL      %subst(wk_MsgData:26:10) = 'UPDFAIL1'
     C                   EVAL      %subst(wk_MsgData:36:30) =
     C                               'Press F5 to refresh.'
     C                   EVAL      %subst(wk_MsgData:66:100) =
     C                               %char(GDGRP#) + '/' + %char(GDTURN) + '/' +
     C                               %char(GDTSEQ) + '/'+ %char(GDRSEQ)
     C                   EXSR      $SndPM
     C                   ENDIF                                                  SQLER3=1

     C                   EVAL      wk_Refresh = *ON

     C                   LEAVE

     C                   ENDDO                                                  *IN03 or *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit change shipment detail screen fields.                             *
      **************************************************************************
     CSR   $Edit2a       BEGSR

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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C     key_GD2       CHAIN     HYLSGDT11
     C     key_TD4a      CHAIN     HYPSGTD
     C                   WHEN      wk_PgmMode = MaintMode2
     C     key_GD2b      CHAIN     HYLSGDT11
     C     key_TD4b      CHAIN     HYPSGTD
     C                   ENDSL
     C                   IF        not %found(HYLSGDT11)
     C                   CLEAR     *ALL          HYRSGDT11
     C                   ENDIF                                                  not %found(HYLSGDT11
     C                   IF        not %found(HYPSGTD)
     C                   CLEAR     *ALL          HYRSGTD
     C                   ENDIF                                                  not %found(HYPSGTD)

      *
      * Populate screen fields.
      *
     C                   EXSR      $MoveToFmt3

      *
      * Initialize screen indicators.
      *
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN93 = *OFF

     C                   DOU       *IN12

      *
      * Format Display Screen
      *
     C                   WRITE     MSFCTL01B
     C                   EXFMT     DSPFMT3A
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $RMVPM

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Enter key pressed?
      *
     C                   LEAVE

     C                   ENDDO                                                  *IN12

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
     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EVAL      F3GRP#X = %triml(%editc(wk_ShipGrp#:'3'))
     C                   EVAL      F3SHTOX = C1SHTO
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      F3GRP#X = %triml(%editc(S1GRP#:'3'))
     C                   EVAL      F3SHTOX = S1SHTOX
     C                   ENDSL
     C                   EVAL      F3TURNX = %triml(%editc(S1TURN:'3'))
     C                   EVAL      F3ORD#X = %triml(%editc(S1ORD#:'3'))
     C                   EVAL      F3PO# = wk_OEORRF
     C                   EVAL      F3ITEM = GDITEM
     C                   EVAL      F3IMDS = GDIMDS
     C                   EVAL      F3QOPN = GDQOPN
     C                   EVAL      F3QOPK = GDQOPK
     C                   EVAL      F3QSTC = GDQSTC
     C                   EVAL      F3QSTCT = TDQSTCB
     C                   EVAL      F3QSCN = S1QSCNX
     C                   IF        GDMXLT = 'Y'
     C                   EVAL      F3MXLT = 'YES'
     C                   ELSE
     C                   EVAL      F3MXLT = GDMXLT
     C                   ENDIF                                                  GDMXLT='Y'
     C                   EVAL      F3SSCCX = %triml(%editc(GDSSCC:'Z'))
     C                   IF        GDTOTE <> '*MULT'
     C                   EVAL      F3TOTE = GDTOTE
     C                   ELSE
     C                   EVAL      F3TOTE = %trimr(TDTOTE) + ' (*MULT)'
     C                   ENDIF                                                  GDTOTE<>'*MULT'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shipment lot inquiry.                                                  *
      **************************************************************************
     CSR   $LotInq       BEGSR

     C                   EVAL      wk_Pgm03 = *ON

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   CALLP(E)  HYR0606TV ('2':wk_ShipGrp#:S1TURN:
     C                               S1TSEQ:S1RSEQ:wk_Tote:wk_RC0606)
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   CALLP(E)  HYR0606TV ('2':S1GRP#:S1TURN:
     C                               S1TSEQ:S1RSEQ:wk_Tote:wk_RC0606)
     C                   ENDSL

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0606TV'
     C                   EXSR      $SndPM
     C                   WHEN      wk_RC0606 = '03'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0606 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0606 = *Zeros
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0606TV'
     C                   EXSR      $SndPM
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Populate function key descriptions for available functions.            *
      **************************************************************************
     CSR   $FuncKey      BEGSR

     C                   SELECT

     C                   WHEN      wk_CurrMode = ItemScan
     C                   EVAL      C1FKY1 = F5Txt + '   ' + F8Txt + '   ' +
     C                               F9Txt
     C                   IF        wk_AlwF11
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F11Txt
     C                   ENDIF                                                  wk_AlwF11
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F12Txt +
     C                               '   ' + F17Txt + '   ' + F18Txt
     C                   IF        wk_AlwF19
     C                   IF        wk_ShowAll
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F19Txtb
     C                   ELSE
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F19Txta
     C                   ENDIF                                                  wk_ShowAll
     C                   ENDIF                                                  wk_AlwF19

     C                   WHEN      wk_CurrMode = LotScan
     C                   EVAL      C1FKY1 = F5Txt + '   ' + F12Txt

     C                   ENDSL

     C                   EVAL      *IN93 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retrieve applicable HYPSGDT record.                                    *
      **************************************************************************
     CSR   $RtvGDRcd     BEGSR

      /Free
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       wk_GDTURN = *Zero;
       wk_GDTSEQ = *Zero;
       wk_GDRSEQ = *Zero;
       EXEC SQL select GDTURN, GDTSEQ, GDRSEQ
                 into :wk_GDTURN, :wk_GDTSEQ, :wk_GDRSEQ
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 a.GDITEM=:C1ITEM and
                 ((a.GDTOTE=:wk_Tote and
                   a.GDQSTC>=(select coalesce(sum(b.W1QSCN),0)
                   from HYW0608A b where b.W1GRP#=a.GDGRP# and
                   b.W1TURN=a.GDTURN and b.W1TSEQ=a.GDTSEQ and
                   b.W1RSEQ=a.GDRSEQ and b.W1TOTE=a.GDTOTE)+:C1QSTC)  or
                  (a.GDTOTE='*MULT' and exists
                   (select * from HYPSGTD c where c.TDGRP#=a.GDGRP# and
                   c.TDTURN=a.GDTURN and c.TDTSEQ=a.GDTSEQ and
                   c.TDRSEQ=a.GDRSEQ and c.TDTOTE=:wk_Tote and
                   c.TDQSTCB>=(select coalesce(sum(b.W1QSCN),0)
                   from HYW0608A b where b.W1GRP#=c.TDGRP# and
                   b.W1TURN=c.TDTURN and b.W1TSEQ=c.TDTSEQ and
                   b.W1RSEQ=c.TDRSEQ and b.W1TOTE=c.TDTOTE)+:C1QSTC)))
                order by GDTURN, GDTSEQ, GDRSEQ
                fetch first row only;
       WHEN wk_PgmMode = MaintMode2;
       wk_GDGRP# = *Zero;
       wk_GDBLTO = *Zero;
       wk_GDSHTO = *Zero;
       wk_GDDSHP = *Zero;
       wk_GDTURN = *Zero;
       wk_GDTSEQ = *Zero;
       wk_GDRSEQ = *Zero;
       EXEC SQL select GDGRP#, GDBLTO, GDSHTO, GDDSHP,
                       GDTURN, GDTSEQ, GDRSEQ
                 into :wk_GDGRP#, :wk_GDBLTO, :wk_GDSHTO, :wk_GDDSHP,
                      :wk_GDTURN, :wk_GDTSEQ, :wk_GDRSEQ
                from HYPSGDT a
                where a.GDITEM=:C1ITEM and
                 ((a.GDTOTE=:wk_Tote and
                   a.GDQSTC>=(select coalesce(sum(b.W1QSCN),0)
                   from HYW0608A b where b.W1GRP#=a.GDGRP# and
                   b.W1TURN=a.GDTURN and b.W1TSEQ=a.GDTSEQ and
                   b.W1RSEQ=a.GDRSEQ and b.W1TOTE=a.GDTOTE)+:C1QSTC)  or
                  (a.GDTOTE='*MULT' and exists
                   (select * from HYPSGTD c where c.TDGRP#=a.GDGRP# and
                   c.TDTURN=a.GDTURN and c.TDTSEQ=a.GDTSEQ and
                   c.TDRSEQ=a.GDRSEQ and c.TDTOTE=:wk_Tote and
                   c.TDQSTCB>=(select coalesce(sum(b.W1QSCN),0)
                   from HYW0608A b where b.W1GRP#=c.TDGRP# and
                   b.W1TURN=c.TDTURN and b.W1TSEQ=c.TDTSEQ and
                   b.W1RSEQ=c.TDRSEQ and b.W1TOTE=c.TDTOTE)+:C1QSTC))) and
                 exists (select * from HYPSGCU d where d.GCGRP#=a.GDGRP#
                 and d.GCBLTO=a.GDBLTO and d.GCSHTO=a.GDSHTO and
                 d.GCDSHP=a.GDDSHP and d.GCSTS='O')
                order by GDGRP#, GDBLTO, GDSHTO, GDDSHP,
                         GDTURN, GDTSEQ, GDRSEQ
                fetch first row only;
       wk_ShipGrp# = wk_GDGRP#;
       wk_BillTo# = wk_GDBLTO;
       wk_ShipTo# = wk_GDSHTO;
       wk_DropShip# = wk_GDDSHP;
       ENDSL;
       IF wk_GDTURN = *Zero;
         wk_RcdCnt = *Zero;
         SELECT;
         WHEN wk_PgmMode = MaintMode1;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGDT a
                  where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                   a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                   (a.GDTOTE=:wk_Tote or (a.GDTOTE='*MULT' and exists
                   (select * from HYPSGTD b where b.TDGRP#=a.GDGRP# and
                   b.TDTURN=a.GDTURN and b.TDTSEQ=a.GDTSEQ and
                   b.TDRSEQ=a.GDRSEQ and
                   b.TDTOTE=:wk_Tote))) and a.GDITEM=:C1ITEM;
         WHEN wk_PgmMode = MaintMode2;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGDT a
                  where (a.GDTOTE=:wk_Tote or (a.GDTOTE='*MULT' and exists
                   (select * from HYPSGTD b where b.TDGRP#=a.GDGRP# and
                   b.TDTURN=a.GDTURN and b.TDTSEQ=a.GDTSEQ and
                   b.TDRSEQ=a.GDRSEQ and
                   b.TDTOTE=:wk_Tote))) and a.GDITEM=:C1ITEM and
                   exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP#
                   and b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and
                   b.GCDSHP=a.GDDSHP and b.GCSTS='O');
         ENDSL;
       ENDIF;                                                                // wk_GDTURN=*Zero
      /End-Free

     C                   IF        wk_GDTURN = *Zero
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      *IN72 = *ON
     C                   EVAL      wk_MsgID = 'ERR1395'
     C                   EVAL      wk_MsgData = C1ITEM + wk_Tote
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   ELSE
     C                   EVAL      *IN72 = *ON
     C                   EVAL      wk_MsgID = 'ERR1396'
     C                   EVAL      wk_MsgData = C1ITEM + wk_Tote +
     C                               '*N'
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   LEAVESR
     C                   ENDIF                                                  wk_GDTURN=*Zero

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C     key_GD3       CHAIN     HYLSGDT11
     C                   WHEN      wk_PgmMode = MaintMode2
     C     key_GD3b      CHAIN     HYLSGDT11
     C                   ENDSL
     C                   IF        not %found(HYLSGDT11)  or
     C                               GDITEM <> C1ITEM  or
     C                               (GDTOTE <> wk_Tote  and
     C                                GDTOTE <> '*MULT')
     C                   EVAL      *IN72 = *ON
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYLSGDT11 '  +
     C                               %triml(%editc(wk_ShipGrp#:'Z')) + '/' +
     C                               %triml(%editc(wk_BillTo#:'Z')) + '/' +
     C                               %triml(%editc(wk_ShipTo#:'Z')) + '/' +
     C                               %triml(%editc(wk_DropShip#:'Z')) + '/' +
     C                               %trimr(wk_Tote) + '/' + %trimr(C1ITEM) +
     C                               '/' +
     C                               %triml(%editc(wk_GDTURN:'Z')) + '/' +
     C                               %triml(%editc(wk_GDTSEQ:'Z')) + '/' +
     C                               %triml(%editc(wk_GDRSEQ:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  not %found(HYLSGDT11

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check verify status.                                                   *
      **************************************************************************
     CSR   $ChkVfySts    BEGSR

     C                   EVAL      wk_AllVerified = *OFF

      /Free
       wk_RcdCnt = *Zero;
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 ((a.GDTOTE=:wk_Tote and
                   a.GDQSTC>(select coalesce(sum(b.W1QSCN),0)
                   from HYW0608A b where b.W1GRP#=a.GDGRP# and
                   b.W1TURN=a.GDTURN and b.W1TSEQ=a.GDTSEQ and
                   b.W1RSEQ=a.GDRSEQ and b.W1TOTE=a.GDTOTE))  or
                  (a.GDTOTE='*MULT' and exists
                   (select * from HYPSGTD c where c.TDGRP#=a.GDGRP# and
                   c.TDTURN=a.GDTURN and c.TDTSEQ=a.GDTSEQ and
                   c.TDRSEQ=a.GDRSEQ and c.TDTOTE=:wk_Tote and
                   c.TDQSTCB>(select coalesce(sum(b.W1QSCN),0)
                   from HYW0608A b where b.W1GRP#=c.TDGRP# and
                   b.W1TURN=c.TDTURN and b.W1TSEQ=c.TDTSEQ and
                   b.W1RSEQ=c.TDRSEQ and b.W1TOTE=c.TDTOTE))));
       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where ((a.GDTOTE=:wk_Tote and
                 exists (select * from HYPSGCU c where c.GCGRP#=a.GDGRP#
                 and c.GCBLTO=a.GDBLTO and c.GCSHTO=a.GDSHTO and
                 c.GCDSHP=a.GDDSHP and c.GCSTS='O') and
                 a.GDQSTC>(select coalesce(sum(b.W1QSCN),0)
                 from HYW0608A b where b.W1GRP#=a.GDGRP# and
                 b.W1TURN=a.GDTURN and b.W1TSEQ=a.GDTSEQ and
                 b.W1RSEQ=a.GDRSEQ and b.W1TOTE=a.GDTOTE))  or
                (a.GDTOTE='*MULT' and
                 exists (select * from HYPSGCU c where c.GCGRP#=a.GDGRP#
                 and c.GCBLTO=a.GDBLTO and c.GCSHTO=a.GDSHTO and
                 c.GCDSHP=a.GDDSHP and c.GCSTS='O') and
                 exists (select * from HYPSGTD c where c.TDGRP#=a.GDGRP# and
                 c.TDTURN=a.GDTURN and c.TDTSEQ=a.GDTSEQ and
                 c.TDRSEQ=a.GDRSEQ and c.TDTOTE=:wk_Tote and
                 c.TDQSTCB>(select coalesce(sum(b.W1QSCN),0)
                 from HYW0608A b where b.W1GRP#=c.TDGRP# and
                 b.W1TURN=c.TDTURN and b.W1TSEQ=c.TDTSEQ and
                 b.W1RSEQ=c.TDRSEQ and b.W1TOTE=c.TDTOTE))));
       ENDSL;
      /End-Free

     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_AllVerified = *ON
     C                   ENDIF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Build work files.                                                      *
      **************************************************************************
     CSR   $BldWork      BEGSR

     C                   EXSR      $ClrWork

      ***
      *** HYW0608A - Tote scan detail work file.
      ***     1) For each tote, file is initially empty.
      ***     2) There will be one record per valid tote scan.
      ***     3) Upon F11=Complete, records are copied to HYPSGTS and then cleared.
      ***
      *** HYW0608B - Tote scan lot detail work file.
      ***     1) For each tote, file is initially empty.
      ***     2) There will be one record per valid lot-controlled item tote scan.
      ***     3) Upon F11=Complete, records are copied to HYPSGLD and then cleared.
      ***

     C     key_TD2       SETLL     HYLSGTD2
     C                   DOU       %eof(HYLSGTD2)
     C     key_TD2       READE     HYLSGTD2
     C                   IF        %eof(HYLSGTD2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGTD2)
     C     key_GD1       CHAIN     HYLSGDT11
     C                   IF        not %found(HYLSGDT11)
     C                   ITER
     C                   ENDIF                                                  not %found(HYLSGDT11
     C     key_TS        SETLL     HYPSGTS
     C                   DOU       %eof(HYPSGTS)
     C     key_TS        READE     HYPSGTS
     C                   IF        %eof(HYPSGTS)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGTS)
     C                   EVAL      W1GRP# = TSGRP#
     C                   EVAL      W1BLTO = TSBLTO
     C                   EVAL      W1SHTO = TSSHTO
     C                   EVAL      W1DSHP = TSDSHP
     C                   EVAL      W1TURN = TSTURN
     C                   EVAL      W1TSEQ = TSTSEQ
     C                   EVAL      W1RSEQ = TSRSEQ
     C                   EVAL      W1OCTL = TSOCTL
     C                   EVAL      W1ORD# = TSORD#
     C                   EVAL      W1ORL# = TSORL#
     C                   EVAL      W1BLN# = TSBLN#
     C                   EVAL      W1ITEM = TSITEM
     C                   EVAL      W1IMDS = TSIMDS
     C                   EVAL      W1QOPK = TSQOPK
     C                   EVAL      W1QSTC = TSQSTC
     C                   EVAL      W1SCAN = TSSCAN
     C                   EVAL      W1QSCN = TSQSCN
     C                   EVAL      W1LOT = TSLOT
     C                   EVAL      W1TOTE = TSTOTE
     C                   EVAL      W1SSCC = *Zero
     C                   EVAL      W1USER = TSUSER
     C                   EVAL      W1EMPL = TSEMPL
     C                   EVAL      W1POSTED = 'P'
     C                   EVAL      W1TSTP1 = TSTSTP1
     C                   WRITE     HYW0608AR

     C     key_LD1       SETLL     HYLSGLD1
     C                   DOU       %eof(HYLSGLD1)
     C     key_LD1       READE     HYLSGLD1
     C                   IF        %eof(HYLSGLD1)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGLD1)
     C                   EVAL      W2GRP# = LDGRP#
     C                   EVAL      W2BLTO = LDBLTO
     C                   EVAL      W2SHTO = LDSHTO
     C                   EVAL      W2DSHP = LDDSHP
     C                   EVAL      W2TURN = LDTURN
     C                   EVAL      W2TSEQ = LDTSEQ
     C                   EVAL      W2RSEQ = LDRSEQ
     C                   EVAL      W2OCTL = LDOCTL
     C                   EVAL      W2ORD# = LDORD#
     C                   EVAL      W2ORL# = LDORL#
     C                   EVAL      W2BLN# = LDBLN#
     C                   EVAL      W2ITEM = LDITEM
     C                   EVAL      W2IMDS = TSIMDS
     C                   EVAL      W2LOT# = LDLOT#
     C                   EVAL      W2QSTC = LDQSTC
     C                   EVAL      W2TOTE = TSTOTE
     C                   EVAL      W2SSCC = LDSSCC
     C                   EVAL      W2POSTED = 'P'
     C                   EVAL      W2TSTP1 = *Loval
     C                   EVAL      W2TSTP2 = *Loval
     C                   WRITE     HYW0608BR
     C                   ENDDO                                                  %eof(HYLSGLD1)

     C                   ENDDO                                                  %eof(HYPSGTS)

     C                   ENDDO                                                  %eof(HYLSGTD2)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write work file - HYW0608A.                                            *
      **************************************************************************
     CSR   $WrtWork1     BEGSR

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EVAL      W1GRP# = wk_ShipGrp#
     C                   EVAL      W1BLTO = wk_BillTo#
     C                   EVAL      W1SHTO = wk_ShipTo#
     C                   EVAL      W1DSHP = wk_DropShip#
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      W1GRP# = GDGRP#
     C                   EVAL      W1BLTO = GDBLTO
     C                   EVAL      W1SHTO = GDSHTO
     C                   EVAL      W1DSHP = GDDSHP
     C                   ENDSL
     C                   EVAL      W1TURN = GDTURN
     C                   EVAL      W1TSEQ = GDTSEQ
     C                   EVAL      W1RSEQ = GDRSEQ
     C                   EVAL      W1OCTL = GDOCTL
     C                   EVAL      W1ORD# = GDORD#
     C                   EVAL      W1ORL# = GDORL#
     C                   EVAL      W1BLN# = GDBLN#
     C                   EVAL      W1ITEM = GDITEM
     C                   EVAL      W1IMDS = GDIMDS
     C                   EVAL      W1QOPK = GDQOPK
     C                   EVAL      W1QSTC = GDQSTC
     C                   EVAL      W1SCAN = wk_C1SCAN
     C                   EVAL      W1QSCN = C1QSTC
     C                   EVAL      W1LOT = GDLOT
     C                   EVAL      W1TOTE = wk_Tote
     C                   EVAL      W1SSCC = *Zero
     C                   EVAL      W1USER = ps_User
     C                   EVAL      W1EMPL = wk_EMPL
     C                   EVAL      W1POSTED = *Blank
     C                   EVAL      W1TSTP1 = wk_Timestamp

     C                   WRITE     HYW0608AR

     C                   EVAL      wk_ItemScanned = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write work file - HYW0608B.                                            *
      **************************************************************************
     CSR   $WrtWork2     BEGSR

     C                   EVAL      wk_Lot# = %triml(C1LOT)
     C                   EVAL      wk_SSCC = *Zero

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C     key_W2        CHAIN     HYW0608B
     C                   WHEN      wk_PgmMode = MaintMode2
     C     key_W2b       CHAIN     HYW0608B
     C                   ENDSL

     C                   IF        not %found(HYW0608B)

     C                   SELECT
     C                   WHEN      wk_PgmMode = MaintMode1
     C                   EVAL      W2GRP# = wk_ShipGrp#
     C                   EVAL      W2BLTO = wk_BillTo#
     C                   EVAL      W2SHTO = wk_ShipTo#
     C                   EVAL      W2DSHP = wk_DropShip#
     C                   WHEN      wk_PgmMode = MaintMode2
     C                   EVAL      W2GRP# = GDGRP#
     C                   EVAL      W2BLTO = GDBLTO
     C                   EVAL      W2SHTO = GDSHTO
     C                   EVAL      W2DSHP = GDDSHP
     C                   ENDSL
     C                   EVAL      W2TURN = GDTURN
     C                   EVAL      W2TSEQ = GDTSEQ
     C                   EVAL      W2RSEQ = GDRSEQ
     C                   EVAL      W2OCTL = GDOCTL
     C                   EVAL      W2ORD# = GDORD#
     C                   EVAL      W2ORL# = GDORL#
     C                   EVAL      W2BLN# = GDBLN#
     C                   EVAL      W2ITEM = GDITEM
     C                   EVAL      W2IMDS = GDIMDS
     C                   EVAL      W2LOT# = %triml(C1LOT)
     C                   EVAL      W2QSTC = C1QSTC
     C                   EVAL      W2TOTE = wk_Tote
     C                   EVAL      W2SSCC = *Zero
     C                   EVAL      W2POSTED = *Blank
     C                   EVAL      W2TSTP1 = wk_Timestamp
     C                   EVAL      W2TSTP2 = wk_Timestamp

     C                   WRITE     HYW0608BR

     C                   ELSE

     C                   EVAL      W2QSTC = W2QSTC + C1QSTC
     C                   EVAL      W2TSTP2 = wk_Timestamp

     C                   UPDATE    HYW0608BR

     C                   ENDIF                                                  not %found(HYW0608B)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move work records to main files.                                       *
      **************************************************************************
     CSR   $MoveToMain   BEGSR

      /Free
       wk_ShipToName = *Blanks;
       IF wk_DropShip# = *Zero;
         EXEC SQL select CMCNA1 into :wk_ShipToName
                  from HDCUST
                  where CMCUST=:wk_ShipTo#;
       ELSE;
         EXEC SQL select DSNAME into :wk_ShipToName
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:wk_ShipTo# and
                   DSNMBR=:wk_DropShip#;
       ENDIF;                                                                // wk_DropShip#=*Zero
      /End-Free

     C                   EVAL      wk_Timestamp = %timestamp

     C     wk_ShipGrp#   SETLL     HYW0608A                                     Use HYW0608AL1????
     C                   DOU       %eof(HYW0608A)
     C     wk_ShipGrp#   READE     HYW0608A
     C                   IF        %eof(HYW0608A)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0608A)
     C                   IF        W1TOTE <> wk_Tote
     C                   ITER
     C                   ENDIF                                                  W1TOTE<>wk_Tote
     C                   IF        W1POSTED = 'P'
     C                   ITER
     C                   ENDIF                                                  W1POSTED='P'

     C                   EVAL      TSGRP# = W1GRP#
     C                   EVAL      TSBLTO = W1BLTO
     C                   EVAL      TSSHTO = W1SHTO
     C                   EVAL      TSDSHP = W1DSHP
     C                   EVAL      TSNAME = wk_ShipToName
     C                   EVAL      TSTURN = W1TURN
     C                   EVAL      TSTSEQ = W1TSEQ
     C                   EVAL      TSRSEQ = W1RSEQ
     C                   EVAL      TSOCTL = W1OCTL
     C                   EVAL      TSORD# = W1ORD#
     C                   EVAL      TSORL# = W1ORL#
     C                   EVAL      TSBLN# = W1BLN#
     C                   EVAL      TSITEM = W1ITEM
     C                   EVAL      TSIMDS = W1IMDS
     C                   EVAL      TSQOPK = W1QOPK
     C                   EVAL      TSQSTC = W1QSTC
     C                   EVAL      TSSCAN = W1SCAN
     C                   EVAL      TSQSCN = W1QSCN
     C                   EVAL      TSLOT = W1LOT
     C                   EVAL      TSTOTE = W1TOTE
     C                   EVAL      TSUSER = W1USER
     C                   EVAL      TSEMPL = W1EMPL
     C                   EVAL      TSTSTP1 = W1TSTP1
     C                   EVAL      TSTSTP2 = wk_Timestamp
     C                   WRITE     HYRSGTS

     C                   IF        W1SSCC <> *Zero
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(W1SSCC:'X')
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt
      /Free
       EXEC SQL update HYPSGDT
                 set GDSSCC=:W1SSCC, GDSSCCBC=:wk_SSCCBC, GDLBL1='Y'
                where GDGRP#=:W1GRP# and GDTURN=:W1TURN and
                 GDTSEQ=:W1TSEQ and GDRSEQ=:W1RSEQ;
      /End-Free
     C                   ENDIF                                                  W1SSCC<>*Zero

     C                   ENDDO                                                  %eof(HYW0608A)

     C     wk_ShipGrp#   SETLL     HYW0608B
     C                   DOU       %eof(HYW0608B)
     C     wk_ShipGrp#   READE     HYW0608B
     C                   IF        %eof(HYW0608B)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0608B)
     C                   IF        W2TOTE <> wk_Tote
     C                   ITER
     C                   ENDIF                                                  W2TOTE<>wk_Tote
     C                   IF        W2POSTED = 'P'
     C                   ITER
     C                   ENDIF                                                  W2POSTED='P'

     C                   EVAL      LDGRP# = W2GRP#
     C                   EVAL      LDBLTO = W2BLTO
     C                   EVAL      LDSHTO = W2SHTO
     C                   EVAL      LDDSHP = W2DSHP
     C                   EVAL      LDTURN = W2TURN
     C                   EVAL      LDTSEQ = W2TSEQ
     C                   EVAL      LDRSEQ = W2RSEQ
     C                   EVAL      LDOCTL = W2OCTL
     C                   EVAL      LDORD# = W2ORD#
     C                   EVAL      LDORL# = W2ORL#
     C                   EVAL      LDBLN# = W2BLN#
     C                   EVAL      LDITEM = W2ITEM
     C                   EVAL      LDLOT# = W2LOT#
     C                   EVAL      LDQSTC = W2QSTC
     C                   EVAL      LDSSCC = W2SSCC
     C                   WRITE     HYRSGLD

      /Free
       wk_IMLOT = *Blanks;
       EXEC SQL select IMLOT
                 into :wk_IMLOT
                from HDIMST
                where IMITEM=:W2ITEM;
      /End-Free

     C****               IF        IMLOT = 'A'  or  IMLOT = 'M'
     C                   IF        wk_IMLOT = 'A'  or  wk_IMLOT = 'M'
     C                   EXSR      $UpdShipRcds
     C                   ENDIF                                                  IMLOT='A' or

     C                   ENDDO                                                  %eof(HYW0608B)

     C                   IF        wk_AllVerified
     C                   EXSR      $WrtSGTC
     C                   ENDIF                                                  wk_AllVerified

     C                   EXSR      $ClrWork

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move work records to main files.                                       *
      **************************************************************************
     CSR   $MoveToMain2  BEGSR

     C                   EVAL      wk_Timestamp = %timestamp

     C     wk_Tote       SETLL     HYW0608AL2
     C                   DOU       %eof(HYW0608Al2)
     C     wk_Tote       READE     HYW0608AL2
     C                   IF        %eof(HYW0608AL2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0608AL2)
     C                   IF        W1POSTED = 'P'
     C                   ITER
     C                   ENDIF                                                  W1POSTED='P'

      /Free
       wk_ShipToName = *Blanks;
       IF W1DSHP = *Zero;
         EXEC SQL select CMCNA1 into :wk_ShipToName
                  from HDCUST
                  where CMCUST=:W1SHTO;
       ELSE;
         EXEC SQL select DSNAME into :wk_ShipToName
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:W1SHTO and
                   DSNMBR=:W1DSHP;
       ENDIF;                                                                // W1DSHP=*Zero
      /End-Free

     C                   EVAL      TSGRP# = W1GRP#
     C                   EVAL      TSBLTO = W1BLTO
     C                   EVAL      TSSHTO = W1SHTO
     C                   EVAL      TSDSHP = W1DSHP
     C                   EVAL      TSNAME = wk_ShipToName
     C                   EVAL      TSTURN = W1TURN
     C                   EVAL      TSTSEQ = W1TSEQ
     C                   EVAL      TSRSEQ = W1RSEQ
     C                   EVAL      TSOCTL = W1OCTL
     C                   EVAL      TSORD# = W1ORD#
     C                   EVAL      TSORL# = W1ORL#
     C                   EVAL      TSBLN# = W1BLN#
     C                   EVAL      TSITEM = W1ITEM
     C                   EVAL      TSIMDS = W1IMDS
     C                   EVAL      TSQOPK = W1QOPK
     C                   EVAL      TSQSTC = W1QSTC
     C                   EVAL      TSSCAN = W1SCAN
     C                   EVAL      TSQSCN = W1QSCN
     C                   EVAL      TSLOT = W1LOT
     C                   EVAL      TSTOTE = W1TOTE
     C                   EVAL      TSUSER = W1USER
     C                   EVAL      TSEMPL = W1EMPL
     C                   EVAL      TSTSTP1 = W1TSTP1
     C                   EVAL      TSTSTP2 = wk_Timestamp
     C                   WRITE     HYRSGTS

     C                   IF        W1SSCC <> *Zero
     C                   EVAL      wk_SSCCBC = '0000' + '090255' +
     C                               %editc(W1SSCC:'X')
     C                   EVAL      wk_ChkDgt = RtvM10ChkDigit(
     C                               %dec(%subst(wk_SSCCBC:1:19):19:0):19)
     C                   EVAL      wk_SSCCBC = %trimr(wk_SSCCBC) + wk_ChkDgt
      /Free
       EXEC SQL update HYPSGDT
                 set GDSSCC=:W1SSCC, GDSSCCBC=:wk_SSCCBC
                where GDGRP#=:W1GRP# and GDTURN=:W1TURN and
                 GDTSEQ=:W1TSEQ and GDRSEQ=:W1RSEQ;
      /End-Free
     C                   ENDIF                                                  W1SSCC<>*Zero

     C                   ENDDO                                                  %eof(HYW0608AL2)

     C     wk_Tote       SETLL     HYW0608BL1
     C                   DOU       %eof(HYW0608BL1)
     C     wk_Tote       READE     HYW0608BL1
     C                   IF        %eof(HYW0608BL1)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0608BL1)
     C                   IF        W2POSTED = 'P'
     C                   ITER
     C                   ENDIF                                                  W2POSTED='P'

     C                   EVAL      LDGRP# = W2GRP#
     C                   EVAL      LDBLTO = W2BLTO
     C                   EVAL      LDSHTO = W2SHTO
     C                   EVAL      LDDSHP = W2DSHP
     C                   EVAL      LDTURN = W2TURN
     C                   EVAL      LDTSEQ = W2TSEQ
     C                   EVAL      LDRSEQ = W2RSEQ
     C                   EVAL      LDOCTL = W2OCTL
     C                   EVAL      LDORD# = W2ORD#
     C                   EVAL      LDORL# = W2ORL#
     C                   EVAL      LDBLN# = W2BLN#
     C                   EVAL      LDITEM = W2ITEM
     C                   EVAL      LDLOT# = W2LOT#
     C                   EVAL      LDQSTC = W2QSTC
     C                   EVAL      LDSSCC = W2SSCC
     C                   WRITE     HYRSGLD

      /Free
       wk_IMLOT = *Blanks;
       EXEC SQL select IMLOT
                 into :wk_IMLOT
                from HDIMST
                where IMITEM=:W2ITEM;
      /End-Free

     C****               IF        IMLOT = 'A'  or  IMLOT = 'M'
     C                   IF        wk_IMLOT = 'A'  or  wk_IMLOT = 'M'
     C                   EXSR      $UpdShipRcds
     C                   ENDIF                                                  IMLOT='A' or

     C                   ENDDO                                                  %eof(HYW0608BL1)

     C                   IF        wk_AllVerified
     C                   EXSR      $WrtSGTC
     C                   ENDIF                                                  wk_AllVerified

     C                   EXSR      $ClrWork

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Update shipment records.                                               *
      **************************************************************************
     CSR   $UpdShipRcds  BEGSR

      /Free
      // IF IMLOT = 'A'  or  IMLOT = 'M';
       IF wk_IMLOT = 'A'  or  wk_IMLOT = 'M';
         EVALR wk_LotRJ = %trimr(W2LOT#);
       ELSE;
         wk_LotRJ = *Blanks;
       ENDIF;                                                                // IMLOT='A' or

         EXEC SQL update OEDTWK
                  set O1QSTC=O1QSTC+:W2QSTC, O1MNCD='C', O1SHPF='1'
                  where O1OCTL=:W2OCTL and O1ORL#=:W2ORL# and
                   O1BLN#=:W2BLN#;
      /End-Free

     C                   EVAL      wk_QSTCLeft = W2QSTC
     C     key_TD        SETLL     HYPSGTD
     C                   DOU       %eof(HYPSGTD)
     C     key_TD        READE     HYPSGTD
     C                   IF        %eof(HYPSGTD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGTD)
     C                   IF        TDQSTCH >= TDQSTCB
     C                   ITER
     C                   ENDIF                                                  TDQSTCH>=TDQSTCB
     C                   EVAL      wk_InProc = TDQSTCB - TDQSTCH
     C                   IF        wk_InProc < wk_QSTCLeft
     C                   EVAL      wk_ToteQty = wk_Inproc
     C                   ELSE
     C                   EVAL      wk_ToteQty = wk_QSTCLeft
     C                   ENDIF                                                  wk_InProc<wk_QSTCLef
      /Free
       EXEC SQL update HYPSGTD
                 set TDQSTCH=TDQSTCH+:wk_ToteQty
                where TDGRP#=:TDGRP# and TDTURN=:TDTURN and
                      TDTSEQ=:TDTSEQ and TDRSEQ=:TDRSEQ and
                      TDTOTE=:TDTOTE;
      /End-Free
     C                   EVAL      wk_QSTCLeft = wk_QSTCLeft - wk_ToteQty
     C                   IF        wk_QSTCLeft <= *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_QSTCLeft<=*Zero
     C                   ENDDO                                                  %eof(HYPSGTD)

     C                   IF        wk_QSTCLeft > *Zero
      /Free
         SELECT;
         WHEN wk_PgmMode = MaintMode1;
           EXEC SQL insert into HYPSGTD
                           (TDGRP#, TDBLTO, TDSHTO, TDDSHP,
                            TDTURN, TDTSEQ, TDRSEQ, TDOCTL,
                            TDORD#,
                            TDORL#, TDBLN#, TDITEM, TDTOTE,
                            TDQSTCB, TDQSTCH)
                    values(:wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                              :wk_DropShip#,
                           :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                           :W2ORD#,
                           :W2ORL#, :W2BLN#, :W2ITEM, 'HYR0608',
                           0,:wk_QSTCLeft);
         WHEN wk_PgmMode = MaintMode2;
           EXEC SQL insert into HYPSGTD
                           (TDGRP#, TDBLTO, TDSHTO, TDDSHP,
                            TDTURN, TDTSEQ, TDRSEQ, TDOCTL,
                            TDORD#,
                            TDORL#, TDBLN#, TDITEM, TDTOTE,
                            TDQSTCB, TDQSTCH)
                    values(:W2GRP#, :W2BLTO, :W2SHTO,
                              :W2DSHP,
                           :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                           :W2ORD#,
                           :W2ORL#, :W2BLN#, :W2ITEM, 'HYR0608',
                           0,:wk_QSTCLeft);
         ENDSL;
      /End-Free
     C                   EVAL      wk_QSTCLeft = *Zero
     C                   ENDIF                                                  wk_QSTCLeft>*Zero

      /Free
       wk_GDPLOC = *Blanks;
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL select GDPLOC
                 into :wk_GDPLOC
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDTURN=:W2TURN and
                 GDTSEQ=:W2TSEQ and GDRSEQ=:W2RSEQ;
       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL select GDPLOC
                 into :wk_GDPLOC
                from HYPSGDT
                where GDGRP#=:W2GRP# and GDTURN=:W2TURN and
                 GDTSEQ=:W2TSEQ and GDRSEQ=:W2RSEQ;
       ENDSL;

       wk_O1WH = *Zero;
       wk_O1OREC = *Blank;
       EXEC SQL select O1WH, O1OREC, O1KTRL
                 into :wk_O1WH, :wk_O1OREC, :wk_O1KTRL
                from OEDTWK
                where O1OCTL=:W2OCTL and O1ORL#=:W2ORL# and
                 O1BLN#=:W2BLN#;
      /End-Free

     C                   IF        wk_O1OREC = 'S'
     C                   EXSR      $ParseLoc
      /Free
       EXEC SQL update HDIWHS set
                 IWOHQT=IWOHQT-:W2QSTC,
                 IWRESQ=IWRESQ-:W2QSTC,
                 IWQOPK=IWQOPK-:W2QSTC,
                 IWQTRS=IWQTRS+:W2QSTC
                where IWITEM=:W2ITEM and IWWHS=:wk_O1WH;
       IF SQLER3 = *Zero;                                                    // NOTE: SQLER3 has the
         wk_MsgID = 'ERR1221';                                               //   rcds updated count
        SELECT;
        WHEN wk_PgmMode = MaintMode1;
         wk_MsgData = %triml(%editc(wk_ShipGrp#:'3'));
        WHEN wk_PgmMode = MaintMode2;
         wk_MsgData = %triml(%editc(W2GRP#:'3'));
        ENDSL;
         %subst(wk_MsgData:10:9) = %triml(%editc(W2TURN:'3'));
         %subst(wk_MsgData:19:15) = W2ITEM;
         %subst(wk_MsgData:34:3) = %triml(%editc(wk_O1WH:'Z'));
         %subst(wk_MsgData:37:10) = ps_Pgnm;
         EXSR $SendITMsg;
       ENDIF;
       IF wk_UpdAud = 'Y';
         wk_UpdCnt = SQLER3;
         SELECT;
         WHEN wk_PgmMode = MaintMode1;
         EXEC SQL insert into HYPSGSH
                         (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                          SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                          SHORD#, SHORL#, SHBLN#, SHITEM,
                          SHTTYP, SHQSTC, SHSTKR, SHAILE,
                          SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                          SHUPDCT, SHWRT, SHPGM, SHPMOD,
                          SHUSER, SHTSTP)
                  values(:wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                            :wk_DropShip#,
                         :W2Turn, :W2TSEQ, :W2RSEQ, :W2OCTL,
                         :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                         'HDIWHS', :W2QSTC, ' ', ' ',
                         ' ',  ' ', :W2LOT#, 'T',
                         :wk_UpdCnt, ' ', 'HYR0608', '2',
                         :ps_User, current timestamp);
         WHEN wk_PgmMode = MaintMode2;
         EXEC SQL insert into HYPSGSH
                         (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                          SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                          SHORD#, SHORL#, SHBLN#, SHITEM,
                          SHTTYP, SHQSTC, SHSTKR, SHAILE,
                          SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                          SHUPDCT, SHWRT, SHPGM, SHPMOD,
                          SHUSER, SHTSTP)
                  values(:W2GRP#, :W2BLTO, :W2SHTO,
                            :W2DSHP,
                         :W2Turn, :W2TSEQ, :W2RSEQ, :W2OCTL,
                         :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                         'HDIWHS', :W2QSTC, ' ', ' ',
                         ' ',  ' ', :W2LOT#, 'T',
                         :wk_UpdCnt, ' ', 'HYR0608', '2',
                         :ps_User, current timestamp);
         ENDSL;
       ENDIF;                                                                // wk_UpdAud='Y'
       EXEC SQL update HDITSL set
                 ISQOH=ISQOH-:W2QSTC
                where ISITEM=:W2ITEM and ISWHS=:wk_O1WH and
                      ISSTKR=:wk_STKR and ISAILE=:wk_AILE and
                      ISSLOC=:wk_SLOC and ISLOT=:wk_LotRJ;
       IF SQLER3 = *Zero;                                                    // NOTE: SQLER3 has the
         wk_MsgID = 'ERR1222';                                               //   rcds updated count
         SELECT;
         WHEN wk_PgmMode = MaintMode1;
         wk_MsgData = %triml(%editc(wk_ShipGrp#:'3'));
         WHEN wk_PgmMode = MaintMode2;
         wk_MsgData = %triml(%editc(W2GRP#:'3'));
         ENDSL;
         %subst(wk_MsgData:10:9) = %triml(%editc(W2TURN:'3'));
         %subst(wk_MsgData:19:15) = W2ITEM;
         %subst(wk_MsgData:34:3) = %triml(%editc(wk_O1WH:'Z'));
         %subst(wk_MsgData:37:15) = %triml(wk_LotRJ);
         %subst(wk_MsgData:52:17) = wk_GDPLOC;
         %subst(wk_MsgData:69:10) = ps_Pgnm;
         EXSR $SendITMsg;
       ENDIF;                                                                // SQLER3=*Zero
       IF wk_UpdAud = 'Y';
         wk_UpdCnt = SQLER3;
         SELECT;
         WHEN wk_PgmMode = MaintMode1;
         EXEC SQL insert into HYPSGSH
                         (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                          SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                          SHORD#, SHORL#, SHBLN#, SHITEM,
                          SHTTYP, SHQSTC, SHSTKR, SHAILE,
                          SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                          SHUPDCT, SHWRT, SHPGM, SHPMOD,
                          SHUSER, SHTSTP)
                  values(:wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                            :wk_DropShip#,
                         :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                         :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                         'HDITSL', :W2QSTC, :wk_STKR, :wk_AILE,
                         :wk_SLOC,  :wk_LotRJ, :W2LOT#, 'T',
                         :wk_UpdCnt, ' ', 'HYR0608', '2',
                         :ps_User, current timestamp);
         WHEN wk_PgmMode = MaintMode2;
         EXEC SQL insert into HYPSGSH
                         (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                          SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                          SHORD#, SHORL#, SHBLN#, SHITEM,
                          SHTTYP, SHQSTC, SHSTKR, SHAILE,
                          SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                          SHUPDCT, SHWRT, SHPGM, SHPMOD,
                          SHUSER, SHTSTP)
                  values(:W2GRP#, :W2BLTO, :W2SHTO,
                            :W2DSHP,
                         :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                         :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                         'HDITSL', :W2QSTC, :wk_STKR, :wk_AILE,
                         :wk_SLOC,  :wk_LotRJ, :W2LOT#, 'T',
                         :wk_UpdCnt, ' ', 'HYR0608', '2',
                         :ps_User, current timestamp);
         ENDSL;
       ENDIF;                                                                // wk_UpdAud='Y'
       IF wk_LotRJ <> *Blanks;
         EXEC SQL update HDLOT set
                   LTQOH=LTQOH-:W2QSTC
                  where LTITEM=:W2ITEM and LTWH=:wk_O1WH and
                        LTLT#=:wk_LotRJ;
         IF SQLER3 = *Zero;                                                  // NOTE: SQLER3 has the
           wk_MsgID = 'ERR1239';                                             //   rcds updated count
           SELECT;
           WHEN wk_PgmMode = MaintMode1;
           wk_MsgData = %triml(%editc(wk_ShipGrp#:'3'));
           WHEN wk_PgmMode = MaintMode2;
           wk_MsgData = %triml(%editc(W2GRP#:'3'));
           ENDSL;
           %subst(wk_MsgData:10:9) = %triml(%editc(W2TURN:'3'));
           %subst(wk_MsgData:19:15) = W2ITEM;
           %subst(wk_MsgData:34:3) = %triml(%editc(wk_O1WH:'Z'));
           %subst(wk_MsgData:37:15) = %triml(wk_LotRJ);
           %subst(wk_MsgData:52:10) = ps_Pgnm;
           EXSR $SendITMsg;
         ENDIF;                                                              // SQLER3=*Zero
         wk_UpdCnt = SQLER3;
         IF wk_UpdAud = 'Y';
          SELECT;
          WHEN wk_PgmMode = MaintMode1;
           EXEC SQL insert into HYPSGSH
                           (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                            SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                            SHORD#, SHORL#, SHBLN#, SHITEM,
                            SHTTYP, SHQSTC, SHSTKR, SHAILE,
                            SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                            SHUPDCT, SHWRT, SHPGM, SHPMOD,
                            SHUSER, SHTSTP)
                  values(:wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                            :wk_DropShip#,
                         :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                         :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                         'HDLOT', :W2QSTC, ' ', ' ',
                         ' ',  :wk_LotRJ, :W2LOT#, 'T',
                         :wk_UpdCnt, ' ', 'HYR0608', '2',
                         :ps_User, current timestamp);
          WHEN wk_PgmMode = MaintMode2;
           EXEC SQL insert into HYPSGSH
                           (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                            SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                            SHORD#, SHORL#, SHBLN#, SHITEM,
                            SHTTYP, SHQSTC, SHSTKR, SHAILE,
                            SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                            SHUPDCT, SHWRT, SHPGM, SHPMOD,
                            SHUSER, SHTSTP)
                  values(:W2GRP#, :W2BLTO, :W2SHTO,
                            :W2DSHP,
                         :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                         :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                         'HDLOT', :W2QSTC, ' ', ' ',
                         ' ',  :wk_LotRJ, :W2LOT#, 'T',
                         :wk_UpdCnt, ' ', 'HYR0608', '2',
                         :ps_User, current timestamp);
          ENDSL;
         ENDIF;                                                              // wk_UpdAud='Y'
         IF wk_UpdCnt = *Zero;                                               // NOTE: SQLER3 has the
           wk_SysCYMD = %dec(%date:*CYMD);                                   //   rcds updated count
           EXEC SQL insert into HDLOT
                           (LTITEM, LTPLT, LTWH, LTLT#,
                            LTTSDT, LTDTRC, LTDTLP, LTDTEX,
                            LTVEND, LTVLOT, LTPO#, LTMORD,
                            LTLINE, LTPORL, LTQOH, LTQAL,
                            LTQRC, LTQSL, LTQIS, LTQAD,
                            LTQTR, LTQKI, LTQRVT, LTLRCP,
                            LTLQHR, LTQHSR, LTLSCR, LTLSCS,
                            LTLSCW, LTIMCS, LTLTCM, LTLLDT,
                            LTLFDS, LTLLDS, LTOTYP, LTECN,
                            LTUDF1, LTUDF2, LTUDF3)
                    values(:W2ITEM, 1, :wk_O1WH, :wk_LotRJ,
                           0, 0, 0, 0,
                           0, ' ', 0, ' ',
                           0, 0, :W2QSTC * -1, 0,
                           0, 0, 0, 0,
                           0, 0, 0, 0,
                           0, 0, 0, 0,
                           0, 0, ' ', :wk_SysCYMD,
                           0, 0, ' ', ' ',
                           ' ', ' ', ' ');
           IF wk_UpdAud = 'Y';
             wk_UpdCnt = SQLER3;
             SELECT;
             WHEN wk_PgmMode = MaintMode1;
             EXEC SQL insert into HYPSGSH
                             (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                              SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                              SHORD#, SHORL#, SHBLN#, SHITEM,
                              SHTTYP, SHQSTC, SHSTKR, SHAILE,
                              SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                              SHUPDCT, SHWRT, SHPGM, SHPMOD,
                              SHUSER, SHTSTP)
                      values(:wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                                :wk_DropShip#,
                             :W2TURN, :W2TSEQ, :W2RSEQ,
                                :W2OCTL,
                             :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                             'HDLOT', :W2QSTC, ' ', ' ',
                             ' ',  :wk_LotRJ, :W2LOT#, 'T',
                             0, 'Y', 'HYR0608', '2',
                             :ps_User, current timestamp);
             WHEN wk_PgmMode = MaintMode2;
             EXEC SQL insert into HYPSGSH
                             (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                              SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                              SHORD#, SHORL#, SHBLN#, SHITEM,
                              SHTTYP, SHQSTC, SHSTKR, SHAILE,
                              SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                              SHUPDCT, SHWRT, SHPGM, SHPMOD,
                              SHUSER, SHTSTP)
                      values(:W2GRP#, :W2BLTO, :W2SHTO,
                                :W2DSHP,
                             :W2TURN, :W2TSEQ, :W2RSEQ,
                                :W2OCTL,
                             :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                             'HDLOT', :W2QSTC, ' ', ' ',
                             ' ',  :wk_LotRJ, :W2LOT#, 'T',
                             0, 'Y', 'HYR0608', '2',
                             :ps_User, current timestamp);
             ENDSL;
           ENDIF;                                                            // wk_UpdAud='Y'
         ENDIF;                                                              // wk_UpdCnt=*Zero
       ENDIF;                                                                // wk_LotRJ<>*Blanks
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL update HYW0610A set
                 W1QSHP=W1QSHP+:W2QSTC
                where W1GRP#=:wk_ShipGrp# and W1ITEM=:W2ITEM and
                      W1WHS=:wk_O1WH and W1STKR=:wk_STKR and
                      W1AILE=:wk_AILE and W1SLOC=:wk_SLOC and
                      W1LOT=:wk_LotRJ;
       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL update HYW0610A set
                 W1QSHP=W1QSHP+:W2QSTC
                where W1GRP#=:W2GRP# and W1ITEM=:W2ITEM and
                      W1WHS=:wk_O1WH and W1STKR=:wk_STKR and
                      W1AILE=:wk_AILE and W1SLOC=:wk_SLOC and
                      W1LOT=:wk_LotRJ;
       ENDSL;
      /End-Free
     C                   ENDIF                                                  wk_O1OREC='S'

     C                   EXSR      $WrtOESLWK

     CSR                 ENDSR

     C/EJECT
     C**************************************************************************
     C* Write/Update OESLWK record.                                            *
     C**************************************************************************
     C     $WrtOESLWK    BEGSR

     C                   SELECT

     C                   WHEN      wk_O1KTRL = *Blank  or  wk_O1KTRL = 'K'

      /Free
       wk_ISSID# = *Zero;
       wk_ISRATE = *Zero;
       EXEC SQL select ISSID#, ISRATE into :wk_ISSID#, :wk_ISRATE
                from HDITSL
                where ISITEM=:W2ITEM and ISWHS=:wk_O1WH and
                      ISSTKR=:wk_STKR and ISAILE=:wk_AILE and
                      ISSLOC=:wk_SLOC and ISLOT=:wk_LotRJ;
      /End-Free

     C                   IF        wk_ISSID# = *Zero
     C                   EVAL      wk_KitCmpnt = 'N'
     C                   EXSR      $WrtHDITSL
     C                   ENDIF                                                  wk_ISSID#=*Zero

     C     key_O3a       CHAIN     OESLWK
     C                   IF        %found(OESLWK)
     C                   EVAL      O3LQTY = O3LQTY + W2QSTC
     C                   EVAL      O3WQTY = O3WQTY + W2QSTC
     C                   UPDATE    OESLWKR
     C                   ELSE
     C                   EVAL      O3OCTL = W2OCTL
     C                   EVAL      O3ORL# = W2ORL#
     C                   EVAL      O3BLN# = W2BLN#
     C                   EVAL      O3ITEM = W2ITEM
     C                   EVAL      O3CITM = wk_CITM
     C                   EVAL      O3OITM = wk_OITM
     C                   EVAL      O3WHS = wk_O1WH
     C                   EVAL      O3RATE = wk_ISRATE
     C                   EVAL      O3STKR = wk_STKR
     C                   EVAL      O3AILE = wk_AILE
     C                   EVAL      O3SLOC = wk_SLOC
     C                   EVAL      O3SID# = wk_ISSID#
     C                   EVAL      O3LOTN = wk_LotRJ
     C                   EVAL      O3LQTY = W2QSTC
     C                   EVAL      O3WQTY = W2QSTC
     C                   EVAL      O3QTYR = *Blank
     C                   EVAL      O3ADSL = *Blank
     C                   WRITE     OESLWKR
     C                   ENDIF                                                  %found(OESLWK)

     C                   WHEN      wk_O1KTRL = 'C'

     C                   IF        not %open(HDKTIT)
     C                   OPEN      HDKTIT
     C                   ENDIF                                                  not %open(HDKTIT)
     C     W2ITEM        SETLL     HDKTIT
     C                   DOU       %eof(HDKTIT)
     C     W2ITEM        READE     HDKTIT
     C                   IF        %eof(HDKTIT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HDKTIT)
     C                   EVAL      wk_KTQSTC = W2QSTC * KIIMQT
      /Free
       wk_ISSID# = *Zero;
       wk_ISRATE = *Zero;
       EXEC SQL select ISSID#, ISRATE into :wk_ISSID#, :wk_ISRATE
                from HDITSL
                where ISITEM=:KIITEM and ISWHS=:wk_O1WH and
                      ISSTKR=:wk_STKR and ISAILE=:wk_AILE and
                      ISSLOC=:wk_SLOC and ISLOT=:wk_LotRJ;
      /End-Free

     C                   IF        wk_ISSID# = *Zero
     C                   EVAL      wk_KitCmpnt = 'Y'
     C                   EXSR      $WrtHDITSL
     C                   ENDIF                                                  wk_ISSID#=*Zero

     C     key_O3b       CHAIN     OESLWK
     C                   IF        %found(OESLWK)
     C                   EVAL      O3LQTY = O3LQTY + wk_KTQSTC
     C                   EVAL      O3WQTY = O3WQTY + wk_KTQSTC
     C                   UPDATE    OESLWKR
     C                   ELSE
     C                   EVAL      O3OCTL = W2OCTL
     C                   EVAL      O3ORL# = W2ORL#
     C                   EVAL      O3BLN# = W2BLN#
     C                   EVAL      O3ITEM = W2ITEM
     C                   EVAL      O3CITM = KIITEM
     C                   EVAL      O3OITM = wk_OITM
     C                   EVAL      O3WHS = wk_O1WH
     C                   EVAL      O3RATE = wk_ISRATE
     C                   EVAL      O3STKR = wk_STKR
     C                   EVAL      O3AILE = wk_AILE
     C                   EVAL      O3SLOC = wk_SLOC
     C                   EVAL      O3SID# = wk_ISSID#
     C                   EVAL      O3LOTN = wk_LotRJ
     C                   EVAL      O3LQTY = wk_KTQSTC
     C                   EVAL      O3WQTY = wk_KTQSTC
     C                   EVAL      O3QTYR = *Blank
     C                   EVAL      O3ADSL = *Blank
     C                   WRITE     OESLWKR
     C                   ENDIF                                                  %found(OESLWK)
     C                   ENDDO                                                  %eof(HDKTIT)

     C                   ENDSL

     C                   ENDSR

     C/EJECT
     C**************************************************************************
     C* Write HDITSL record.                                                   *
     C**************************************************************************
     C     $WrtHDITSL    BEGSR

      /Free
       EVAL wk_SLSTID = *Zero;
       EVAL wk_SLRATE = *Zero;
       EXEC SQL select SLSTID, SLRATE
                 into :wk_SLSTID, :wk_SLRATE
                from HDSTLC
                where SLWHS=:wk_O1WH and SLSTKR=:wk_STKR and
                 SLAILE=:wk_AILE and SLSLOC=:wk_SLOC;
       IF wk_SLSTID <> *Zero;
         IF wk_KitCmpnt = 'N';
           EXEC SQL insert into HDITSL
                           (ISSID#, ISSTKR, ISAILE, ISSLOC,
                            ISITEM, ISWHS, ISLOT, ISDTRC,
                            ISRATE, ISSGRP, ISECQT, ISMXOH,
                            ISRPQT, ISQOH, ISQRES, ISQHSR)
                    values(:wk_SLSTID, :wk_STKR, :wk_AILE, :wk_SLOC,
                            :W2ITEM, :wk_O1WH, :wk_LotRJ, 0,
                            :wk_SLRATE, 0, 0, 0,
                            0, :W2QSTC*-1, 0, 0);
           IF wk_UpdAud = 'Y';
            SELECT;
            WHEN wk_PgmMode = MaintMode1;
             EXEC SQL insert into HYPSGSH
                             (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                              SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                              SHORD#, SHORL#, SHBLN#, SHITEM,
                              SHTTYP, SHQSTC, SHSTKR, SHAILE,
                              SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                              SHUPDCT, SHWRT, SHPGM, SHPMOD,
                              SHUSER, SHTSTP)
                      values(:wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                                :wk_DropShip#,
                             :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                             :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                             'HDITSL', :W2QSTC, :wk_STKR, :wk_AILE,
                             :wk_SLOC,  :wk_LotRJ, :W2LOT#, 'T',
                             0, 'Y', 'HYR0608', '2',
                             :ps_User, current timestamp);
            WHEN wk_PgmMode = MaintMode2;
             EXEC SQL insert into HYPSGSH
                             (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                              SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                              SHORD#, SHORL#, SHBLN#, SHITEM,
                              SHTTYP, SHQSTC, SHSTKR, SHAILE,
                              SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                              SHUPDCT, SHWRT, SHPGM, SHPMOD,
                              SHUSER, SHTSTP)
                      values(:W2GRP#, :W2BLTO, :W2SHTO,
                                :W2DSHP,
                             :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                             :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                             'HDITSL', :W2QSTC, :wk_STKR, :wk_AILE,
                             :wk_SLOC,  :wk_LotRJ, :W2LOT#, 'T',
                             0, 'Y', 'HYR0608', '2',
                             :ps_User, current timestamp);
            ENDSL;
           ENDIF;                                                            // wk_UpdAud='Y'
         ELSE;
           EXEC SQL insert into HDITSL
                           (ISSID#, ISSTKR, ISAILE, ISSLOC,
                            ISITEM, ISWHS, ISLOT, ISDTRC,
                            ISRATE, ISSGRP, ISECQT, ISMXOH,
                            ISRPQT, ISQOH, ISQRES, ISQHSR)
                    values(:wk_SLSTID, :wk_STKR, :wk_AILE, :wk_SLOC,
                            :KIITEM, :wk_O1WH, :wk_LotRJ, 0,
                            :wk_SLRATE, 0, 0, 0,
                            0, 0, 0, 0);
           IF wk_UpdAud = 'Y';
            SELECT;
            WHEN wk_PgmMode = MaintMode1;
             EXEC SQL insert into HYPSGSH
                             (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                              SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                              SHORD#, SHORL#, SHBLN#, SHITEM,
                              SHTTYP, SHQSTC, SHSTKR, SHAILE,
                              SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                              SHUPDCT, SHWRT, SHPGM, SHPMOD,
                              SHUSER, SHTSTP)
                      values(:wk_ShipGrp#, :wk_BillTo#, :wk_ShipTo#,
                                :wk_DropShip#,
                             :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                             :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                             'HDITSL', :W2QSTC, :wk_STKR, :wk_AILE,
                             :wk_SLOC,  :wk_LotRJ, :W2LOT#, 'T',
                             0, 'Y', 'HYR0608', '2',
                             :ps_User, current timestamp);
            WHEN wk_PgmMode = MaintMode2;
             EXEC SQL insert into HYPSGSH
                             (SHGRP#, SHBLTO, SHSHTO, SHDSHP,
                              SHTURN, SHTSEQ, SHRSEQ, SHOCTL,
                              SHORD#, SHORL#, SHBLN#, SHITEM,
                              SHTTYP, SHQSTC, SHSTKR, SHAILE,
                              SHSLOC, SHLOT1, SHLOT2, SHCNVY,
                              SHUPDCT, SHWRT, SHPGM, SHPMOD,
                              SHUSER, SHTSTP)
                      values(:W2GRP#, :W2BLTO, :W2SHTO,
                                :W2DSHP,
                             :W2TURN, :W2TSEQ, :W2RSEQ, :W2OCTL,
                             :W2ORD#, :W2ORL#, :W2BLN#, :W2ITEM,
                             'HDITSL', :W2QSTC, :wk_STKR, :wk_AILE,
                             :wk_SLOC,  :wk_LotRJ, :W2LOT#, 'T',
                             0, 'Y', 'HYR0608', '2',
                             :ps_User, current timestamp);
            ENDSL;
           ENDIF;                                                            // wk_UpdAud='Y'
         ENDIF;                                                              // wk_KitCmpnt='N'
         wk_ISSID# = wk_SLSTID;
         wk_ISRATE = wk_SLRATE;
       ELSE;
         wk_MsgID = 'ERR9001';
         wk_MsgData = 'HDSTLC';
         %subst(wk_MsgData:11:128) = %editc(wk_O1WH:'X') + '/' + wk_STKR + '/' +
           %trimr(wk_AILE) + '/' + %trimr(wk_SLOC);
         %subst(wk_MsgData:139:128) = 'HDITSL not written for BoxLogix +
           shipment.  See program ' + %trimr(ps_Pgnm) + '.';
         EXSR $SendITMsg;
       ENDIF;                                                                // wk_SLSTID<>*Zero
      /End-Free

     C                   ENDSR

     C/EJECT
      **************************************************************************
      * Write HYPSGTC record for closed totes.                                 *
      **************************************************************************
     CSR   $WrtSGTC      BEGSR

     C                   SELECT

     C                   WHEN      wk_PgmMode = MaintMode1
      /Free
       wk_ItemCnt = *Zero;
       wk_LblCnt = *Zero;
       EXEC SQL select count(distinct W1ITEM) into :wk_ItemCnt
                from HYW0608A
                where W1GRP#=:wk_ShipGrp# and W1BLTO=:wk_BillTo# and
                 W1SHTO=:wk_ShipTo# and W1DSHP=:wk_DropShip# and
                 W1TOTE=:wk_Tote and W1QSCN>0;
       EXEC SQL select count(distinct W1SSCC) into :wk_LblCnt
                from HYW0608A
                where W1GRP#=:wk_ShipGrp# and W1BLTO=:wk_BillTo# and
                 W1SHTO=:wk_ShipTo# and W1DSHP=:wk_DropShip# and
                 W1TOTE=:wk_Tote and W1QSCN>0 and W1SSCC<>0;
      /End-Free
     C                   EVAL      TCGRP# = wk_ShipGrp#
     C                   EVAL      TCBLTO = wk_BillTo#
     C                   EVAL      TCSHTO = wk_ShipTo#
     C                   EVAL      TCDSHP = wk_DropShip#
     C                   EVAL      TCTOTE = wk_Tote
     C                   EVAL      TCITCT = wk_ItemCnt
     C                   EVAL      TCLBCT = wk_Lblcnt
     C                   EVAL      TCPGM = ps_Pgnm
     C                   EVAL      TCUSER = ps_User
     C                   EVAL      TCEMPL = wk_EMPL
     C                   EVAL      TCTSTP = wk_Timestamp
     C                   WRITE     HYRSGTC

     C                   WHEN      wk_PgmMode = MaintMode2
     C                   IF        not %open(HYW0608AL2)
     C                   OPEN      HYW0608AL2
     C                   ENDIF                                                  not %open(HYW0608AL2
     C                   EVAL      sv_TOTE = *Hival
     C     wk_Tote       SETLL     HYW0608AL2
     C                   DOU       %eof(HYW0608AL2)
     C     wk_Tote       READE     HYW0608AL2
     C                   IF        %eof(HYW0608AL2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYW0608AL2)
     C                   IF        W1TOTE = sv_TOTE
     C                   ITER
     C                   ENDIF                                                  W1TOTE=sv_TOTE
     C                   EVAL      sv_TOTE = W1TOTE
      /Free
       wk_ItemCnt = *Zero;
       wk_LblCnt = *Zero;
       EXEC SQL select count(distinct W1ITEM) into :wk_ItemCnt
                from HYW0608A
                where W1GRP#=:W1GRP# and W1BLTO=:W1BLTO and
                 W1SHTO=:W1SHTO and W1DSHP=:W1DSHP and
                 W1TOTE=:wk_Tote and W1QSCN>0;
       EXEC SQL select count(distinct W1SSCC) into :wk_LblCnt
                from HYW0608A
                where W1GRP#=:W1GRP# and W1BLTO=:W1BLTO and
                 W1SHTO=:W1SHTO and W1DSHP=:W1DSHP and
                 W1TOTE=:wk_Tote and W1QSCN>0 and W1SSCC<>0;
      /End-Free
     C                   EVAL      TCGRP# = W1GRP#
     C                   EVAL      TCBLTO = W1BLTO
     C                   EVAL      TCSHTO = W1SHTO
     C                   EVAL      TCDSHP = W1DSHP
     C                   EVAL      TCTOTE = wk_Tote
     C                   EVAL      TCITCT = wk_ItemCnt
     C                   EVAL      TCLBCT = wk_Lblcnt
     C                   EVAL      TCPGM = ps_Pgnm
     C                   EVAL      TCUSER = ps_User
     C                   EVAL      TCEMPL = wk_EMPL
     C                   EVAL      TCTSTP = wk_Timestamp
     C                   WRITE     HYRSGTC
     C                   ENDDO                                                  %eof(HYW0608AL2)

     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
     C**************************************************************************
     C* Parse location.                                                        *
     C**************************************************************************
     C     $ParseLoc     BEGSR

     C                   IF        wk_O1WH = 6

     C                   EVAL      kf_LSLOC = wk_GDPLOC
     C     key_LS        CHAIN     HYPSTKR

     C                   EVAL      wk_WHS = 6

     C                   IF        %found(HYPSTKR)  and  LSSTKR <> *Blanks
     C                   EVAL      wk_STKR = LSSTKR
     C                   ELSE
     C                   IF        %subst(wk_GDPLOC:3:1) = '-'                  Double-letter aisle
     C                   EVAL      wk_STKR = '060'
     C                   ELSE
     C                   IF        %subst(wk_GDPLOC:6:1) <= 'M'
     C                   EVAL      wk_STKR = '006'
     C                   ELSE
     C                   EVAL      wk_STKR = '060'
     C                   ENDIF                                                  %subst(wk_GDPLOC:6:1
     C                   ENDIF                                                  %subst(wk_GDPLOC:3:1
     C                   ENDIF                                                  %found(HYPSTKR) and
     C                   IF        %subst(wk_GDPLOC:3:1) = '-'                  Double-letter aisle
     C                   EVAL      wk_AILE = %subst(wk_GDPLOC:1:2)
     C                   EVAL      wk_SLOC = %subst(wk_GDPLOC:4:7)
     C                   ELSE
     C                   EVAL      wk_AILE = %subst(wk_GDPLOC:1:1)
     C                   EVAL      wk_SLOC = %subst(wk_GDPLOC:3:7)
     C                   ENDIF                                                  %subst(wk_GDPLOC:3:1

     C                   ELSE

      *
      * The following was added as an attempt to handle shipping
      * ammo seconds via BoxLogix.
      *
     C                   IF        %len(%trimr(wk_GDPLOC)) = 9                  e.g., A-29-A-10
     C                   EVAL      wk_WHS = wk_O1WH
     C                   EVAL      wk_STKR = '006'
     C                   EVAL      wk_AILE = %subst(wk_GDPLOC:1:1)
     C                   EVAL      wk_SLOC = %subst(wk_GDPLOC:3:7)
     C                   ELSE
     C                   EVAL      wk_WHS = wk_O1WH
     C                   EVAL      wk_STKR = %editc(wk_O1WH:'X')
     C                   EVAL      wk_AILE = *Blanks
     C                   EVAL      wk_SLOC = *Blanks
     C                   ENDIF                                                  %len(%trimr(wk_GDPLO

     C                   ENDIF                                                  wk_O1WH=6

     C                   ENDSR

     C/EJECT
     C**************************************************************************
     C* Confirm cancel request.                                                *
     C**************************************************************************
     C     $CnfrmCncl    BEGSR

     C                   IF        not wk_ItemScanned
     C                   EVAL      wk_CnfrmCncl = *ON
     C                   LEAVESR
     C                   ENDIF                                                  not wk_ItemScanned

     C                   EVAL      wk_CnfrmCncl = *OFF

     C                   EVAL      W2LINE1 = 'Items have been verified for the +
     C                               current tote.  These'
     C                   EVAL      W2LINE2 = 'verifications will be lost if +
     C                               the requested cancel is'
     C                   IF        not wk_SCWK
     C                   EVAL      W2LINE3 = 'performed.'
     C                   EVAL      W2LINE4 = *Blanks
     C                   ELSE
     C                   EVAL      W2LINE3 = 'performed.  Also, any labels +
     C                               generated in the current'
     C                   EVAL      W2LINE4 = 'Tote Verify session will be lost +
     C                               (and not be valid).'
     C                   ENDIF                                                  not wk_SCWK
     C                   EVAL      W2DOIT = *Blank

     C                   DOU       *IN12
     C                   EXFMT     WINDOW02
     C                   IF        *IN12
     C                   EVAL      W2DOIT = 'N'
     C                   LEAVE
     C                   ENDIF                                                  *IN12
     C                   IF        W2DOIT = 'Y'  or  W2DOIT = 'N'
     C                   LEAVE
     C                   ENDIF                                                  W2DOIT='Y' or
     C                   ENDDO                                                  *IN12

     C                   IF        W2DOIT = 'Y'
     C                   EVAL      wk_CnfrmCncl = *ON
     C                   ENDIF                                                  W2DOIT='Y'

     C                   ENDSR

     C/EJECT
     C**************************************************************************
     C* Send user message.                                                     *
     C**************************************************************************
     C     $SendITMsg    BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               'BEARL     *LIBL     JELLIS    *LIBL     +
     C                                *SYSOPR' :
     C                               3 : *Blanks : in_MsgKey : io_MsgErrCode)

     C                   ENDSR

     C/EJECT
      **************************************************************************
      * Clear work files.                                                      *
      **************************************************************************
     CSR   $ClrWork      BEGSR

      /Free
       SELECT;

       WHEN wk_PgmMode = MaintMode1;
       EXEC SQL delete from HYW0608A
                where W1GRP#=:wk_ShipGrp# and W1BLTO=:wk_BillTo# and
                 W1SHTO=:wk_ShipTo# and W1DSHP=:wk_DropShip# and
                 W1TOTE=:wk_Tote;
       EXEC SQL delete from HYW0608B
                where W2GRP#=:wk_ShipGrp# and W2BLTO=:wk_BillTo# and
                 W2SHTO=:wk_ShipTo# and W2DSHP=:wk_DropShip# and
                 W2TOTE=:wk_Tote;

       WHEN wk_PgmMode = MaintMode2;
       EXEC SQL delete from HYW0608A
                where W1TOTE=:wk_Tote;
       EXEC SQL delete from HYW0608B
                where W2TOTE=:wk_Tote;

       ENDSL;
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Delete temporarily created HYPSCWK records.                            *
      **************************************************************************
     CSR   $DltSCWK      BEGSR

      /Free
       SELECT;
       WHEN wk_PgmMode = MaintMode1;
         EXEC SQL update HYPSGDT a
                  set GDSSCCBC=' ', GDSSCC=0, GDLBL1='N', GDLBL2='N'
                  where GDGRP#=:wk_ShipGrp# and GDTOTE=:wk_Tote and
                   exists (select * from HYW0608A b where
                   b.W1TOTE=a.GDTOTE and b.W1SSCC=a.GDSSCC and
                   b.W1TURN=a.GDTURN);
       WHEN wk_PgmMode = MaintMode2;
         EXEC SQL update HYPSGDT a
                  set GDSSCCBC=' ', GDSSCC=0, GDLBL1='N', GDLBL2='N'
                  where GDTOTE=:wk_Tote and
                   exists (select * from HYW0608A b where
                   b.W1TOTE=a.GDTOTE and b.W1SSCC=a.GDSSCC and
                   b.W1TURN=a.GDTURN);
       ENDSL;

       EXEC SQL delete from HYPSCWK a
                where exists (select * from HYW0608A b where
                 b.W1SSCC=a.S1SRL# and b.W1TURN=a.S1TURN);
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR

     C                   IF        wk_Pgm03
     C                   CALLP(E)  HYR0606TV ('9':wk_ShipGrp#:*Zero:
     C                               *Zero:*Zero:*Blanks:wk_RC0606)
     C                   ENDIF                                                  wk_Pgm03

     C                   IF        wk_Pgm02
     C                   CALLP(E)  sd_HYR0622
     C                   ENDIF                                                  wk_Pgm02

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - each call.                                    *
      **************************************************************************
     CSR   $InzSR        BEGSR

     C                   IN        wk_AlwFilter1
     C                   IF        wk_AlwFilter1 = 'Y'
     C                   EVAL      wk_ShowAll = *ON
     C                   EVAL      wk_AutoF19 = *ON                             Auto chg to not done
     C                   EVAL      wk_AlwF19 = *ON
     C                   ELSE
     C                   EVAL      wk_ShowAll = *ON
     C                   EVAL      wk_AutoF19 = *OFF
     C                   EVAL      wk_AlwF19 = *OFF
     C                   ENDIF                                                  wk_AlwFilter1='Y'

     C                   IN        wk_UpdAud

     C                   IF        wk_PgmMode = MaintMode2
     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)
     C                   IF        not %open(HYW0608AL2)
     C                   OPEN      HYW0608AL2
     C                   ENDIF                                                  not %open(HYW0608AL2
     C                   IF        not %open(HYW0608BL1)
     C                   OPEN      HYW0608BL1
     C                   ENDIF                                                  not %open(HYW0608BL1
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   EVAL      C1HDG1 = ToteHdg1
     C                   EVAL      C1HDG2 = ToteHdg2
     C                   EVAL      F1HDG1 = ToteHdg1
     C                   EVAL      wk_AlwF11 = *OFF
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   EVAL      C1HDG2 = center(C1HDG2)
     C                   EVAL      F1HDG1 = center(F1HDG1)

     C                   EVAL      C1GRP# = %triml(%editc(wk_ShipGrp#:'Z'))

      /Free
       wk_GCSTNM = *Blanks;
       EXEC SQL select GCSTNM
                 into :wk_GCSTNM
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

     C                   EVAL      C1TOTE = wk_Tote

     C                   IF        wk_PgmMode = MaintMode2
      /Free
       wk_CustCnt = *Zero;
       wk_OrdCnt = *Zero;
       EXEC SQL select count(distinct digits(a.GDGRP#) || digits(a.GDBLTO) ||
                                      digits(a.GDSHTO) || digits(a.GDDSHP)),
                       count(distinct a.GDORD#)
                 into :wk_CustCnt, :wk_OrdCnt
                from HYPSGDT a
                where a.GDTOTE=:wk_Tote and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP# and
                 b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP
                 and b.GCSTS='O');
      /End-Free
     C                   EVAL      C1CUCT = %triml(%editc(wk_CustCnt:'3'))
     C                   EVAL      C1ORCT = %triml(%editc(wk_OrdCnt:'3'))
     C                   ELSE
     C                   EVAL      C1CUCT = *Blanks
     C                   EVAL      C1ORCT = *Blanks
     C                   ENDIF                                                  wk_PgmMode=MaintMode

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

     C                   EVAL      wk_CurrMode = ItemScan
     C                   EVAL      *IN81 = *OFF
     C                   EVAL      wk_DftQty = 5
     C                   EVAL      C1QSTC = wk_DftQty
     C                   EVAL      C1SCAN = *Blanks
     C                   EVAL      C1ITEM = *Blanks
     C                   EVAL      C1LOT = *Blanks

     C                   EXSR      $FuncKey

     C                   EVAL      *IN12 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      *IN73 = *OFF
     C                   EVAL      wk_CnfrmCncl = *OFF
     C                   EVAL      wk_ItemScanned = *OFF
     C                   EVAL      wk_SCWK = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization - first time only.                              *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Define  Key Lists
      *

     C     key_GD1       KLIST                                                  HYLSGDT11
     C                   KFLD                    TDGRP#
     C                   KFLD                    TDTURN
     C                   KFLD                    TDTSEQ
     C                   KFLD                    TDRSEQ

     C     key_GD2       KLIST                                                  HYLSGDT11
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    S1TURN
     C                   KFLD                    S1TSEQ
     C                   KFLD                    S1RSEQ

     C     key_GD2b      KLIST                                                  HYLSGDT11
     C                   KFLD                    S1GRP#
     C                   KFLD                    S1TURN
     C                   KFLD                    S1TSEQ
     C                   KFLD                    S1RSEQ

     C     key_GD3       KLIST                                                  HYLSGDT11
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ
     C                   KFLD                    wk_GDRSEQ

     C     key_GD3b      KLIST                                                  HYLSGDT11
     C                   KFLD                    wk_GDGRP#
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ
     C                   KFLD                    wk_GDRSEQ

     C     key_TD        KLIST                                                  HYPSGTD
     C                   KFLD                    W2GRP#
     C                   KFLD                    W2TURN
     C                   KFLD                    W2TSEQ
     C                   KFLD                    W2RSEQ

     C     key_TD2       KLIST                                                  HYLSGTD2
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    wk_Tote

     C     key_TD3c      KLIST                                                  HYLSGTD2
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    wk_Tote
     C                   KFLD                    C1ITEM
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ
     C                   KFLD                    wk_GDRSEQ

     C     key_TD3d      KLIST                                                  HYLSGTD2
     C                   KFLD                    wk_GDGRP#
     C                   KFLD                    wk_GDBLTO
     C                   KFLD                    wk_GDSHTO
     C                   KFLD                    wk_GDDSHP
     C                   KFLD                    wk_Tote
     C                   KFLD                    C1ITEM
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ
     C                   KFLD                    wk_GDRSEQ

     C     key_TD4a      KLIST                                                  HYPSGTD
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    S1TURN
     C                   KFLD                    S1TSEQ
     C                   KFLD                    S1RSEQ
     C                   KFLD                    wk_Tote

     C     key_TD4b      KLIST                                                  HYPSGTD
     C                   KFLD                    S1GRP#
     C                   KFLD                    S1TURN
     C                   KFLD                    S1TSEQ
     C                   KFLD                    S1RSEQ
     C                   KFLD                    wk_Tote

     C     key_GC        KLIST                                                  HYPSGCU
     C                   KFLD                    GDGRP#
     C                   KFLD                    GDBLTO
     C                   KFLD                    GDSHTO
     C                   KFLD                    GDDSHP

     C     key_TS        KLIST                                                  HYPSGTS
     C                   KFLD                    TDGRP#
     C                   KFLD                    TDTURN
     C                   KFLD                    TDTSEQ
     C                   KFLD                    TDRSEQ
     C                   KFLD                    TDTOTE

     C     key_LD1       KLIST                                                  HYLSGLD1
     C                   KFLD                    TSGRP#
     C                   KFLD                    TSTURN
     C                   KFLD                    TSTSEQ
     C                   KFLD                    TSRSEQ

     C     key_LD2       KLIST                                                  HYPSGLD
     C                   KFLD                    W1GRP#
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ

     C     key_O3a       KLIST                                                  OESLWK
     C                   KFLD                    W2OCTL
     C                   KFLD                    W2ORL#
     C                   KFLD                    W2BLN#
     C                   KFLD                    W2ITEM
     C                   KFLD                    wk_CITM
     C                   KFLD                    wk_OITM
     C                   KFLD                    wk_O1WH
     C                   KFLD                    wk_ISRATE
     C                   KFLD                    wk_STKR
     C                   KFLD                    wk_AILE
     C                   KFLD                    wk_SLOC
     C                   KFLD                    wk_LotRJ

     C     key_O3b       KLIST                                                  OESLWK
     C                   KFLD                    W2OCTL
     C                   KFLD                    W2ORL#
     C                   KFLD                    W2BLN#
     C                   KFLD                    W2ITEM
     C                   KFLD                    KIITEM
     C                   KFLD                    wk_OITM
     C                   KFLD                    wk_O1WH
     C                   KFLD                    wk_ISRATE
     C                   KFLD                    wk_STKR
     C                   KFLD                    wk_AILE
     C                   KFLD                    wk_SLOC
     C                   KFLD                    wk_LotRJ

     C     key_LS        KLIST                                                  HYPSTKR
     C                   KFLD                    kf_LSRTYP
     C                   KFLD                    kf_LSLOC

     C     key_W2        KLIST                                                  HYW0608B
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    GDTURN
     C                   KFLD                    GDTSEQ
     C                   KFLD                    GDRSEQ
     C                   KFLD                    wk_Tote
     C                   KFLD                    GDITEM
     C                   KFLD                    wk_Lot#
     C                   KFLD                    wk_SSCC

     C     key_W2b       KLIST                                                  HYW0608B
     C                   KFLD                    GDGRP#
     C                   KFLD                    GDTURN
     C                   KFLD                    GDTSEQ
     C                   KFLD                    GDRSEQ
     C                   KFLD                    wk_Tote
     C                   KFLD                    GDITEM
     C                   KFLD                    wk_Lot#
     C                   KFLD                    wk_SSCC

     C     key_W2c       KLIST                                                  HYW0608B
     C                   KFLD                    W1GRP#
     C                   KFLD                    wk_GDTURN
     C                   KFLD                    wk_GDTSEQ

     C     key_W3        KLIST                                                  HYW0602
     C                   KFLD                    ps_Job#
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#

     C     RFSKEY1       KLIST                                                  SFL01 - HYLSGTD2
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    wk_Tote
     C                   KFLD                    @UITEM
     C                   KFLD                    @UTURN
     C                   KFLD                    @UTSEQ
     C                   KFLD                    @URSEQ

     C     PGUKEY1       KLIST                                                  SFL01 - HYLSGTD2
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    wk_Tote
     C                   KFLD                    @UITEM
     C                   KFLD                    @UTURN
     C                   KFLD                    @UTSEQ
     C                   KFLD                    @URSEQ

     C     PGDKEY1       KLIST                                                  SFL01 - HYLSGTD2
     C                   KFLD                    wk_ShipGrp#
     C                   KFLD                    wk_BillTo#
     C                   KFLD                    wk_ShipTo#
     C                   KFLD                    wk_DropShip#
     C                   KFLD                    wk_Tote
     C                   KFLD                    @DITEM
     C                   KFLD                    @DTURN
     C                   KFLD                    @DTSEQ
     C                   KFLD                    @DRSEQ

     C     RFSKEY1b      KLIST                                                  SFL01B - HYLSGTD1
     C                   KFLD                    wk_Tote
     C                   KFLD                    @UGRP#
     C                   KFLD                    @UBLTO
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UITEM
     C                   KFLD                    @UTURN
     C                   KFLD                    @UTSEQ
     C                   KFLD                    @URSEQ

     C     PGUKEY1b      KLIST                                                  SFL01B - HYLSGTD1
     C                   KFLD                    wk_Tote
     C                   KFLD                    @UGRP#
     C                   KFLD                    @UBLTO
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UITEM
     C                   KFLD                    @UTURN
     C                   KFLD                    @UTSEQ
     C                   KFLD                    @URSEQ

     C     PGDKEY1b      KLIST                                                  SFL01B - HYLSGTD1
     C                   KFLD                    wk_Tote
     C                   KFLD                    @DGRP#
     C                   KFLD                    @DBLTO
     C                   KFLD                    @DSHTO
     C                   KFLD                    @DDSHP
     C                   KFLD                    @DITEM
     C                   KFLD                    @DTURN
     C                   KFLD                    @DTSEQ
     C                   KFLD                    @DRSEQ

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

      *
      * Retrieve current GS1 company prefix (e.g., 090255 or 851529).
      *
     C                   IN        wk_GS1Comp
     C                   IN        wk_GS1Comp2
     C                   IF        wk_GS1Comp2 = *Blanks
     C                   EVAL      wk_GS1Comp2 = *Hival
     C                   ENDIF                                                  wk_GS1Comp2=*Blanks

      *
      * Retrieve current shipping location.
      *
      /Free
       wk_Alda = *OFF;
       wk_GI281 = *OFF;
       wk_WestDC = *OFF;
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from GUPTDAT
                where TDTABL='ALDAWKSTN' and TDKEY1=:ps_Job;
       IF wk_RcdCnt > *Zero;
         wk_Alda = *ON;
       ELSE;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from GUPTDAT
                  where TDTABL='GIWKSTN' and TDKEY1=:ps_Job;
         IF wk_RcdCnt > *Zero;
           wk_GI281 = *ON;
         ELSE;
           wk_WestDC = *ON;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

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
     D RcdSeq#                        5S 0 CONST
     D SSCCBC                        20A   CONST
     D Order#                         8S 0 CONST
     D Item#                         15A   CONST
     D LblType                        1A   CONST

     D wk_ShipGrp#     S                   LIKE(GDGRP#)
     D wk_Turn#        S                   LIKE(GDTURN)
     D wk_TurnSeq#     S                   LIKE(GDTSEQ)
     D wk_RcdSeq#      S                   LIKE(GDRSEQ)
     D wk_SSCCBC       S                   LIKE(GDSSCCBC)
     D wk_Order#       S                   LIKE(GDORD#)
     D wk_Item#        S                   LIKE(GDITEM)
     D wk_LblType      S              1A
     D wk_RtnCode      S              2A
     D wk_GDLOT        S                   LIKE(GDLOT)
     D wk_GDQSTC       S                   LIKE(GDQSTC)
     D wk_RcdCnt       S              7P 0
     D wk_LDQSTC       S                   LIKE(LDQSTC)
     D wk_RcdCnt2      S              7P 0
     D wk_W2QSTC       S                   LIKE(W2QSTC)
     D wk_ShipQty      S                   LIKE(W1QSTC)
     D wk_ScanQty      S                   LIKE(W1QSCN)

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
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
         wk_RcdCnt2 = *Zero;
         wk_W2QSTC = *Zero;
         EXEC SQL select count(*), coalesce(sum(LDQSTC),0)
                   into :wk_RcdCnt, :wk_LDQSTC
                  from HYPSGLD
                  where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                        LDTSEQ=:wk_TurnSeq# and LDSSCC=0;
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC < wk_GDQSTC;
           EXEC SQL select count(*), coalesce(sum(W2QSTC),0)
                     into :wk_RcdCnt2, :wk_W2QSTC
                    from HYW0608B
                    where W2GRP#=:wk_ShipGrp# and W2TURN=:wk_Turn# and
                          W2TSEQ=:wk_TurnSeq# and W2SSCC=0;
           wk_RcdCnt = wk_RcdCnt + wk_RcdCnt2;
           wk_LDQSTC = wk_LDQSTC + wk_W2QSTC;
         ENDIF;                                                              // wk_RcdCnt=*Zero or
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC < wk_GDQSTC;
           wk_RtnCode = '02';
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt=*Zero or
       ENDIF;                                                                // wk_GDLOT<>*Blanks
      /End-Free

      *
      *   05-Not verified yet.
      *
      /Free
       wk_ShipQty = *Zero;
       wk_ScanQty = *Zero;
       EXEC SQL select coalesce(min(w1qstc),0), coalesce(sum(W1QSCN),0)
                 into :wk_ShipQty, :wk_ScanQty
                from HYW0608A
                where W1GRP#=:wk_ShipGrp# and W1TURN=:wk_Turn# and
                      W1TSEQ=:wk_TurnSeq# and W1RSEQ=:wk_RcdSeq#;
       IF wk_ScanQty = *Zero  or  wk_ScanQty < wk_ShipQty;
         wk_RtnCode = '05';
         LEAVE;
       ENDIF;                                                                // wk_ScanQty=*Zero
      /End-Free

      *
      *   06-Label has already been printed.
      *
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYW0608A
                where W1GRP#=:wk_ShipGrp# and W1TURN=:wk_Turn# and
                      W1TSEQ=:wk_TurnSeq# and W1RSEQ=:wk_RcdSeq# and
                      W1SSCC<>0;
       IF wk_RcdCnt > *Zero;
         wk_RtnCode = '06';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero
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
     D wk_W2QSTC       S             13P 4
     D wk_RcdCnt2      S              7P 0
     D wk_ShipQty      S                   LIKE(W1QSTC)
     D wk_ScanQty      S                   LIKE(W1QSCN)

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_SSCC = SSCC
     C                   EVAL      wk_RtnCode = *Zeros

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
         wk_RcdCnt = *Zero;
         wk_W2QSTC = *Zero;
         wk_RcdCnt2 = *Zero;
         EXEC SQL select coalesce(sum(LDQSTC),0), count(*)
                   into :wk_LDQSTC, :wk_RcdCnt
                  from HYPSGLD
                  where LDGRP#=:wk_ShipGrp# and LDTURN=:wk_Turn# and
                        LDTSEQ=:wk_TurnSeq# and LDRSEQ=:wk_RcdSeq#;
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC < wk_GDQSTC;
           EXEC SQL select sum(W2QSTC), count(*) into :wk_W2QSTC, :wk_RcdCnt2
                    from HYW0608B
                    where W2GRP#=:wk_ShipGrp# and W2TURN=:wk_Turn# and
                          W2TSEQ=:wk_TurnSeq# and W2RSEQ=:wk_RcdSeq#;
           wk_RcdCnt = wk_RcdCnt + wk_RcdCnt2;
           wk_LDQSTC = wk_LDQSTC + wk_W2QSTC;
         ENDIF;                                                              // wk_RcdCnt=*Zero
         IF wk_RcdCnt = *Zero  or  wk_LDQSTC <> wk_GDQSTC;
           wk_RtnCode = '02';
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt=*Zero
       ENDIF;                                                                // wk_Lot<>'00'

      //
      //  03-Not verified yet.
      //
       wk_ShipQty = *Zero;
       wk_ScanQty = *Zero;
       EXEC SQL select coalesce(min(w1qstc),0), coalesce(sum(W1QSCN),0)
                 into :wk_ShipQty, :wk_ScanQty
                from HYW0608A
                where W1GRP#=:wk_ShipGrp# and W1TURN=:wk_Turn# and
                      W1TSEQ=:wk_TurnSeq# and W1RSEQ=:wk_RcdSeq#;
       IF wk_ScanQty = *Zero  or  wk_ScanQty < wk_ShipQty;
         wk_RtnCode = '03';
         LEAVE;
       ENDIF;                                                                // wk_ScanQty=*Zero
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P LblOK           E

     P/EJECT
      **************************************************************************
      * Determine whether label can be deleted.                                *
      **************************************************************************
     P DltLblOK        B
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
     D wk_GDCNVY       S              1A
     D wk_GCSTS        S              1A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_TurnSeq# = TurnSeq#
     C                   EVAL      wk_RcdSeq# = RcdSeq#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       //
       // 01-Label has not yet been created.
       //
       IF wk_SSCCBC = *Blanks;
        wk_RcdCnt = *Zero;
        EXEC SQL select count(*) into :wk_RcdCnt
                 from HYW0608A
                 where W1GRP#=:wk_ShipGrp# and W1TURN=:wk_Turn# and
                       W1TSEQ=:wk_TurnSeq# and W1RSEQ=:wk_RcdSeq# and
                       W1SSCC<>0;
        IF wk_RcdCnt = *Zero;
         wk_RtnCode = '01';
         LEAVE;
        ENDIF;                                                               // wk_RcdCnt=*Zero
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
