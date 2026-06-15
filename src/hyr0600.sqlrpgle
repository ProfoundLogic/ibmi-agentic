     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2018')
     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('HYR0600')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Shipment Processing                                       *
      *          BY: William Earl (MCA)                                        *
      *        DATE: August 2018                                               *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  12/04/2019  MCA  Allow PRO number entry/change.                       *
      *  12/13/2019  LWB  Check CompCustShip return codes from Agilx credit    *
      *                   card fund approval. OECTRM = 'EC'                    *
      *  01/14/2020  MCA  Tweak "Find Shipment" tote scan logic.               *
      *  01/29/2020  MCA  Disallow ship via codes not valid at shipping (e.g., *
      *                   BW).  For bulk ammo shipments, disallow ship via     *
      *                   codes that cannot be used for bulk ammo.             *
      *  05/06/2020  MCA  Don't allow F11 to complete shipment if the shipment *
      *                   is associated with pallets that contain product that *
      *                   is not associated with the current shipment.         *
      *  09/02/2020  MCA  Blem carton conveyable processing - require lot      *
      *                   entry.                                               *
      *  10/21/2021  MCA  Display Sales-specified ship-via or any ship-via     *
      *                   comments when completing a shipment.                 *
      *  02/16/2022  MCA  in $CompShip, if F5TRK# is entered by the user and   *
      *                   it its length is greater than 15 chars, as it is     *
      *                   written to HYPSTRK also update HYPTDTA.TDTRK#.  See  *
      *                   See HOEUOE for explanation.                          *
      *  03/16/2022  MCA  Tote verify.                                         *
      *  03/31/2022  MCA  Ensure user is setup for Varsity if ship-via will go *
      *                   there.                                               *
      *  04/11/2022  MCA  Notify user that Case Prep Duo shipment air waybill  *
      *                   needs specific text.  Created a "generic" window     *
      *                   (WINDOW05B) for this purpose, and for future use for *
      *                   other purposes.                                      *
      *  05/16/2022  MCA  Handle each DC seperately (as a subsetable value).   *
      *  08/20/2022  MCA  Handle small-parcel picks (which could span multiple *
      *                   shipment groups and/or customer shipments for a      *
      *                   single tote).                                        *
      *  09/02/2022  MCA  Add 2nd "display" view, so that picking-related      *
      *                   fields can be shown.                                 *
      *  09/02/2022  MCA  Add subsetting for tote-picks, small-parcel picks,   *
      *                   and pick priority overrides.                         *
      *  09/12/2022  MCA  Change HYR0520 program interface to allow passing of *
      *                   new "extra comment" paramter.                        *
      *  09/15/2022  MCA  Check for shipment requiring liftgate service, and   *
      *                   pass requirement to HYR0520 in the new "extra        *
      *                   comment" parameter.                                  *
      *  09/30/2022  MCA  Add TDRSEQ (needed for Tote Verify).                 *
      *  10/14/2022  MCA  Handle *MULT totes.                                  *
      *  03/08/2023  MCA  If any pick line is over-shipped, don't allow the    *
      *                   shipment to be completed or placed into intransit    *
      *                   hold until it is fixed.  The most likely cause is    *
      *                   that the Packer manually "shipped" a pick line that  *
      *                   was later conveyed.                                  *
      *  08/23/2023  PRF  Handle Digi-Pick entries
      *  11/21/2023  MCA  Allow shipping lane 8 on subset prompt.              *
      *  01/13/2024  MCA  Notify packer if carton UCC-128 labels printed for   *
      *                   multiple-carton UCC-128 labels.                      *
      *  05/13/2024  MCA  Tweak "Find Shipment" tote scan logic.               *
      *  06/03/2024  MCA  Varsity phase 3 - ShipSelect.                        *
      *  06/03/2024  MCA  Varsity phase 3 - ShipSelect.  Retrieve new PRO no.  *
      *                   when applicable.                                     *
      *  08/07/2024  MCA  Don't call ShipSelect if any of the shipment orders  *
      *                   are on intransit hold.                               *
      *  09/04/2024  MCA  For CompOK and IntrHoldOK, check for recalled lots.  *
      *  11/15/2024  MCA  When finding/verifying totes, skip totes that have   *
      *                   already been closed for the shipment (unless, in     *
      *                   $FindShip, we are last-ditch just looking for last   *
      *                   shipment that used the tote, regardless of shipment  *
      *                   status).                                             *
      *  11/18/2024  MCA  Fix bug - don't allow Tote Verify function for       *
      *                   pallet picks.                                        *
      *  12/27/2024  MCA  Protect freight and special charges from changes for *
      *                   credit card orders (including eComm EC).             *
      *  04/30/2025  MCA  Fix bug determining when to NOT call ShipSelect.     *
      *                   When excluding foreign shipments, the programming    *
      *                   had been using wk_GCSTST to also exclude shipments   *
      *                   to Alaska and Hawaii and the U.S. territories, but   *
      *                   the field was only populated (via SQL) in the        *
      *                   $Palletize subroutine.  It was not being populated   *
      *                   in the $CompShip subroutine. The HYPSGCU record was  *
      *                   already being retrieved in the $CompShip subroutine, *
      *                   so changed the "foreign exclude" logic to instead    *
      *                   use GCSTST. NOTE: This would not have been a huge    *
      *                   or noticeable problem.                               *
      *  03/19/2026  MCA  Don't allow shipment completion or placing intransit *
      *                   if the shipped freight class count exceeds maximum.  *
      *  06/11/2026  MCA  Check truck shipment fully palletized before allowed *
      *                   to place on intransit hold (option 18).              *
      **************************************************************************

     FHYD0600A  CF   E             WORKSTN
     F                                     SFILE(SFL01A:wk_Sfl01RRN)
     F                                     SFILE(SFL02A:wk_Sfl02RRN)
     F                                     SFILE(SFL03A:wk_Sfl03RRN)
     FHYD0600B  CF   E             WORKSTN
     FHYLSGCU1  IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU1)
     FHYLSGCU2  IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU2) USROPN
     FHYLSGCU3  IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU3) USROPN
     FHYLSGCU4  IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU4) USROPN
     FHYLSGCU11 IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU11) USROPN
     FHYLSGCU12 IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU12) USROPN
     FHYLSGCU13 IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU13) USROPN
     FHYLSGCU14 IF   E           K DISK    RENAME(HYRSGCU:HYRSGCU14) USROPN
     FHYPSGCU   UF   E           K DISK    USROPN
     FHYLSGDT3  IF   E           K DISK    USROPN
     FHYPSGHD   IF   E           K DISK
     FHYPINTR   IF A E           K DISK    USROPN

     D/EJECT
      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR0600         PR                  EXTPGM('HYR0600')
     D  PgmMode                       1A   CONST

     D HYR0602         PR                  EXTPGM('HYR0602')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  RtnCode                       2A

     D HYR0608         PR                  EXTPGM('HYR0608')
     D  PgmMode                       1A   CONST
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  Tote                         10A   CONST
     D  RtnCode                       2A

      /copy hyr0600_pr.rpgle

      * /COPY QPRPSRC,HYR0614 -- handled by hyr0600_pr.rpgleinc

     D HYC0138         PR                  EXTPGM('HYC0138')                    Palletize shipment
     D  PgmMode                       1A   CONST

     D HYR0189         PR                  EXTPGM('HYR0189')                    Display comments
     D  ShipTo#                       7S 0 CONST
     D  ForceF11                      1N   CONST
     D  RtnCode                       2A

     D HYR0520         PR                  EXTPGM('HYR0520')
     D  Caller                        2A   CONST
      ****  01 - Prior to Shipping ShipSelect (HYR0510)
      ****  02 - Normal shipping backend (HYR0110)
      ****  03 - BoxLogix shipping backend (HYR0600)
      ****  04 - Combine (BOL and Parcel) screen (HYR0140)
     D  ShipTo#                       7S 0 CONST
     D  Ord#                          8S 0 DIM(100)
     D  OrdCnt                        3P 0
     D  XtraCmt                       2A   CONST
      ****  01 - Liftgate service required
     D  RtnCode                       2A

     D VPRBLDP         PR                  EXTPGM('VPRBLDP')                    Varsity ShipSelect
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  MaxDaysIntr                   3P 0 CONST
     D  Caller                       10A   CONST
     D  ShipVia                       2A
     D  RtnCode                       2A

      * /COPY QPRPSRC,HYR9930 -- handled by hyr0600_pr.rpgleinc

     D HHDCSS          PR                  EXTPROC('HHDCSS')                    Customer nbr prompt
     D  Cust#                         7P 0

     D HHDSVI          PR                  EXTPROC('HHDSVI')                    Ship via prompt
     D  ShpVia                        2A

     D RtvShipWgt      PR             5P 0                                      Rtv shipment weight
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST
     D  PickShip                      1A   CONST

     D DltOK           PR             2A                                        Check if delete ok
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST

     D CompOK          PR             2A                                        Check if complete ok
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST

     D IntrHoldOK      PR             2A                                        Check if intr hld ok
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST

     D OnIntrHold      PR             1N
     D  Order#                        8S 0 CONST
     D  Turn#                         9S 0 CONST

     D ForceCmts       PR             1N
     D  ShipGrp#                      9S 0 CONST
     D  BillTo#                       7S 0 CONST
     D  ShipTo#                       7S 0 CONST
     D  DropShip#                     7S 0 CONST

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

     D QMHRMVPM        PR                  EXTPGM('QMHRMVPM')                   Remove program msgs
     D  out_MsgStack                 10A   CONST
     D  out_MsgStkCtr                10I 0 CONST
     D  out_MsgKey                    4A   CONST
     D  out_MsgToRmv                 10A   CONST
     D  io_MsgErrCode                10I 0

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR0600         PI
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
     D ds_CYMD                 2      8  0
     D ds_MDY                  5     10  0
     D ds_YYYY1                1      4  0
     D ds_YY2                  9     10  0

     D ds_CS           DS            30
     D  ds_CSGRP#              1      9  0
     D  ds_CSBLTO             10     16  0
     D  ds_CSSHTO             17     23  0
     D  ds_CSDSHP             24     30  0

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_LDA          S           1024A   DTAARA(*LDA)                         *LDA
     D wk_BxlSSOnOff   S              1A   DTAARA(HYABXLSS)                     Bxl/DP ShipSelect
     D wk_CCFrtAlw     S             10A   DTAARA(HYACCFRT)                     CC/EComm frt chg alw

      **************************************************************************
      * Define arrays.                                                         *
      **************************************************************************
     D ary_Ord#        S              8S 0 DIM(100)
     D idx1            S              3P 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PgmMode      S              1A
     D wk_SuperUser    S              1N   INZ(*OFF)
     D wk_W3SIZE       S              1A
     D wk_EMPL         S              5S 0
     D wk_Alda         S              1N                                        Alda wkstn indicator
     D wk_GI281        S              1N                                        GI Hwy 281 indicator
     D wk_ToteVfy      S              1N   INZ(*OFF)
     D wk_Include      S              1N   INZ(*OFF)
     D wk_IN12         S                   LIKE(*IN12)
     D wk_IN25         S                   LIKE(*IN25)
     D wk_Sfl01RRN     S              5P 0
     D wk_Sfl01PC      S              5P 0
     D wk_Sfl01PS      S              3P 0 INZ(14)
     D wk_Sfl02RRN     S              5P 0
     D wk_Sfl03RRN     S              5P 0
     D wk_SelPr        S              1N   INZ(*OFF)
     D wk_ChgPr        S              1N   INZ(*OFF)
     D wk_DltPr        S              1N   INZ(*OFF)
     D wk_DspPr        S              1N   INZ(*OFF)
     D wk_DtlPr        S              1N   INZ(*OFF)
     D wk_PlltzPr      S              1N   INZ(*OFF)
     D wk_CompPr       S              1N   INZ(*OFF)
     D wk_HldPr        S              1N   INZ(*OFF)
     D wk_CmtsPr       S              1N   INZ(*OFF)
     D wk_OptPr        S              1N   INZ(*OFF)
     D wk_ClrDone      S              1N   INZ(*OFF)
     D wk_FirstDlt     S                   LIKE(wk_Sfl01RRN)
     D wk_Refresh      S              1N   INZ(*OFF)
     D wk_Sort         S              1A
     D wk_SortX        S              1A
     D wk_History      S              1N   INZ(*OFF)
     D wk_Completed    S              1N   INZ(*OFF)
     D wk_DltChk       S              2A
     D wk_PlltzChk     S              2A
     D wk_CompChk      S              2A
     D wk_IntrHoldChk  S              2A
     D wk_DspView      S              1A
     D wk_STName       S             26A
     D wk_DSName       S             30A
     D wk_SVSVSV       S              2A
     D wk_SVSVDS       S             15A
     D wk_SVBOLR       S              1A
     D wk_CMCUST       S              7S 0
     D wk_CMCCLS       S              2A
     D wk_CMALPH       S              4A
     D wk_GCSTST       S              2A
     D wk_GCSTCT       S              2A
     D wk_GCSTYP       S              1A
     D wk_GCSHPV       S              2A
     D wk_GCTSTP2      S               Z
     D wk_BOLReq       S              1N
     D wk_GenBOL       S              1N
     D wk_HistCutoff   S               D
     D W1FRDT1D        S               D
     D W1TODT2D        S               D
     D wk_DftLane      S              3P 0
     D wk_DftInclPPk   S              1A
     D wk_SSZero       S              1N
     D wk_TDKEY1       S             15A
     D wk_TDNF01       S             15P 4
     D wk_TDFF01       S              1A
     D wk_TDFF02       S              1A
     D wk_TDFF04       S              1A
     D wk_TDFF05       S              1A
     D wk_TDFF06       S              1A
     D wk_HoldCnt1     S              5P 0
     D wk_HoldCnt2     S              5P 0
     D wk_FindBCLen    S              3P 0
     D wk_PackLane     S             10A
     D wk_CtnList      S            100A
     D wk_GDGRP#       S              9S 0
     D wk_GDTURN       S              9S 0
     D wk_GDBLTO       S              7S 0
     D wk_GDSHTO       S              7S 0
     D wk_GDDSHP       S              7S 0
     D wk_GDSSCCBC     S             20A
     D wk_GCCPTY       S              3S 0
     D wk_GCSTNM       S             30A
     D wk_GCSTS        S              1A
     D wk_CallSS       S              1N
     D wk_MaxIntr      S              3P 0
     D wk_RCBLDP       S              2A
     D wk_MaxFtCl      S              3P 0 INZ(10)
     D wk_EMFNAM       S             18A
     D wk_EMLNAM       S             18A
     D wk_CmtsForced   S              1N
     D wk_NtfyWdw      S              3P 0
     D wk_ShipAMMOB    S              1N
     D wk_XtraCmt      S              2A
     D wk_LGNeeded     S              1N
     D wk_LGCnt        S              5P 0
     D wk_IntrHold     S              1N
     D wk_MultCSFnd    S              1N
     D wk_CSList       S            900A                                        Holds up to 30 CS's
     D wk_Ptr          S              5P 0
     D wk_SelGrp#      S              9S 0
     D wk_SelBLTO      S              7S 0
     D wk_SelSHTO      S              7S 0
     D wk_SelDSHP      S              7S 0
     D wk_RC0520       S              2A
     D wk_XtraOrds     S              3P 0
     D wk_CustCnt      S              7P 0
     D wk_LblCnt       S              7P 0
     D wk_RcdCnt       S              7P 0
     D wk_MinSVSV      S              2A
     D wk_MaxSVSV      S              2A
     D wk_LogData      S             64A
     D wk_LogMSG       S             64A
     D wk_Cust#        S                   LIKE(GCBLTO)
     D wk_ShipVia      S                   LIKE(GCSHPV)
     D wk_F5SHPVold    S                   LIKE(F5SHPV)
     D wk_SSShipVia    S                   LIKE(GCSHPV)
     D wk_SSPRO#       S                   LIKE(GCPRO#)
     D wk_TDCF03       S             30A
     D wk_TDFF08       S              1A
     D wk_GHDCLOC      S              4A
     D wk_PRO#         S             30A
     D wk_UpdPRO#      S             30A
     D wk_ValueRtnd    S              1N
     D wk_AlwF7        S              1N   INZ(*ON)
     D wk_AlwF9        S              1N   INZ(*ON)
     D wk_AlwF11       S              1N
     D wk_AlwF12       S              1N
     D wk_AlwF21       S              1N   INZ(*OFF)
     D wk_AlwF22       S              1N   INZ(*ON)
     D wk_FKeySet      S              1A
     D wk_IN11         S              1N
     D io_Cust#        S                   LIKE(GCBLTO)
     D io_ShipVia      S                   LIKE(GCSHPV)
     D wk_RC0189       S              2A
     D wk_RC0602       S              2A
     D wk_RC0608       S              2A
     D wk_RC0614       S             10I 0
     D wk_Error        S              1N   INZ(*OFF)
     D wk_Pgm01        S              1N   INZ(*OFF)                            HYR0602 called
     D wk_Pgm02        S              1N   INZ(*OFF)                            HHDCSS called
     D wk_Pgm03        S              1N   INZ(*OFF)                            HHDSVI called
     D wk_Pgm04        S              1N   INZ(*OFF)                            HYR0189 called
     D wk_Pgm05        S              1N   INZ(*OFF)                            HYR0520 called
     D wk_Pgm06        S              1N   INZ(*OFF)                            HYR0608 called
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D wk_MsgIDxx      S              7A
     D wk_MsgDataxx    S            512A
     D sv_SSBLTO       S                   LIKE(F4BLTO)
     D sv_SSSHTO       S                   LIKE(F4SHTO)
     D sv_SSSHPV       S                   LIKE(F4SHPV)
     D sv_SSDVRT       S                   LIKE(F4DVRT)
     D sv_SSIPPK       S                   LIKE(F4IPPK)
     D sv_SSITPK       S                   LIKE(F4ITPK)
     D sv_SSISPP       S                   LIKE(F4ISPP)
     D sv_SSIPPO       S                   LIKE(F4IPPO)
     D sv_SSITEM       S                   LIKE(F4ITEM)
     D sv_SSORD#       S                   LIKE(F4ORD#)
     D sv_SSPO#        S                   LIKE(F4PO#)
     D sv_SSDCLOC      S                   LIKE(F4DCLOC)
     D sv_SHPV         S                   LIKE(GCSHPV)
     D sv_PRO#         S                   LIKE(GCPRO#)
     D sv_FRTA         S                   LIKE(GCFRTA)
     D sv_CHGA         S                   LIKE(GCCHGA)
     D sv_TURN         S                   LIKE(GDTURN)
     D sv_ORD#         S                   LIKE(GDORD#)
     D kf_ORD#         S              8S 0
     D kf_TURN         S              9S 0
     D kf_AIV#         S              7S 0
     D kf_GRP#         S              9S 0
     D kf_BLTO         S              7S 0
     D kf_SHTO         S              7S 0
     D kf_DSHP         S              7S 0
     D @UGRP#          S                   LIKE(GCGRP#)                         SFL01 keys
     D @UCPTY          S                   LIKE(GCCPTY)                            "   "
     D @UBLTO          S                   LIKE(GCBLTO)                            "   "
     D @USHTO          S                   LIKE(GCSHTO)                            "   "
     D @UDSHP          S                   LIKE(GCDSHP)                            "   "
     D @USTNM          S                   LIKE(GCSTNM)                            "   "
     D @DGRP#          S                   LIKE(GCGRP#)                            "   "
     D @DCPTY          S                   LIKE(GCCPTY)                            "   "
     D @DBLTO          S                   LIKE(GCBLTO)                            "   "
     D @DSHTO          S                   LIKE(GCSHTO)                            "   "
     D @DDSHP          S                   LIKE(GCDSHP)                            "   "
     D @DSTNM          S                   LIKE(GCSTNM)                            "   "
     D F4DVRTX         S              3A

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
     D @DLT            C                   ' 4'
     D @DLT2           C                   '4 '
     D @DSP            C                   ' 5'
     D @DSP2           C                   '5 '
     D @DTL            C                   ' 8'
     D @DTL2           C                   '8 '
     D @PLLTZ          C                   '15'
     D @COMP           C                   '17'
     D @HOLD           C                   '18'
     D @CMTS           C                   '21'
     D ErrorMsg1       C                   'Record not updated.'
     D ErrorMsg2       C                   'Record not deleted.'
     D MaintHdg        C                   'Shipment Processing'
     D InqHdg          C                   'Shipment Inquiry'
     D OpenShips       C                   'Open Shipments Only'
     D TodaysShips     C                   'Today''s Shipments Only'
     D HistShips       C                   'History Shipments Only'
     D MaintOpts       C                   '2=Change   4=Delete   5=Display   -
     D                                     8=Detail   15=Palletize   -
     D                                     17=Complete   18=Intransit hold   -
     D                                     21=Comments'
     D InqOpts         C                   '5=Display   8=Detail   21=Comments'
     D F3Txt           C                   'F3=Exit'
     D F5Txt           C                   'F5=Refresh'
     D F7Txta          C                   'F7=Include completed'
     D F7Txtb          C                   'F7=Exclude completed'
     D F8Txt           C                   'F8=Find'
     D F9Txt           C                   'F9=Tote verify'
     D F11Txt          C                   'F11=History'
     D F12Txt          C                   'F12=Cancel'
     D F17Txt          C                   'F17=Top'
     D F18Txt          C                   'F18=Bottom'
     D F20Txt          C                   'F20=Subset'
     D F22Txt          C                   'F22=Emply#'
     D F24Txt          C                   'F24=More keys'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************

     C                   EVAL      wk_PgmMode = in_PgmMode

     C                   IF        wk_PgmMode = ShutDownMode
     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C                   ENDIF

     C                   DO

     C                   IF        wk_PgmMode = MaintMode
     C                   EVAL      wk_W3SIZE = '2'
     C                   EXSR      $PromptEmply
     C                   IF        *IN12
     C                   LEAVE
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   EXSR      $InzSR

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGCU a
                where GCSTS='O' and
                 exists (select * from hypsghd b where b.GHGRP#=a.GCGRP#
                 and b.GHDCLOC=:F4DCLOC);
      /End-Free

     C                   IF        wk_RcdCnt > *Zero
     C                   EXSR      $PageDownSf1
     C                   ELSE
     C                   EXSR      $ClearSfl01
     C                   ENDIF                                                  wk_RcdCnt>*Zero

     C                   DOW       not *IN03

     C                   EVAL      *IN07 = wk_AlwF7
     C                   EVAL      *IN09 = wk_AlwF9
     C                   EVAL      *IN22 = wk_AlwF22
     C                   IF        not wk_ToteVfy
     C                   EVAL      *IN24 = *OFF
     C                   ELSE
     C                   EVAL      *IN24 = *ON
     C                   ENDIF                                                  not wk_ToteVfy

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
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Cancel?
      *
     C                   IF        *IN12
     C                   IF        wk_History
     C                   EXSR      $LeaveHist
     C                   ITER
     C                   ENDIF                                                  wk_History
     C                   LEAVE
     C                   ENDIF                                                  *IN12

      *
      * Check for function key use with options present.
      *
     C                   EVAL      wk_Error = *OFF

     C                   IF        *IN07  or  *IN08  or  *IN11  or
     C                                *IN09  or
     C                                *IN17  or  *IN18  or  *IN20  or
     C                                *IN22  or
     C                                *IN24  or
     C                                *IN91  or  *IN92

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
     C                   IF        *IN07  or  *IN08  or  *IN11  or
     C                                *IN09  or
     C                                *IN17  or  *IN18  or  *IN20  or
     C                                *IN22
     C                                or *IN24
     C                   EVAL      wK_MsgID = 'GUM0005'
     C                   ELSE
     C                   EVAL      wK_MsgID = 'GUM0004'
     C                   ENDIF                                                  *IN07 or *IN08 or
     C                   EVAL      wk_MsgData = *Blanks
     C                   EVAL      *IN40 = *ON
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   ENDIF                                                  wk_Sfl01RRN<>*Zero

     C                   ENDIF                                                  *IN07 or *IN08 or

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
     C                   EVAL      C1GRP# = *Zero
     C                   EVAL      C1BLTO = *Zero
     C                   EVAL      C1SHTO = *Zero
     C                   EVAL      C1STNM = *Blanks
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

      *
      * Include/Exclude completed shipments toggle?
      *
     C                   WHEN      *IN07
     C                   EXSR      $InclComp

      *
      * Find a shipment?
      *
     C                   WHEN      *IN08
     C                   EXSR      $FindShip

      *
      * Tote verify?
      *
     C                   WHEN      *IN09
     C                   EXSR      $ToteVfy

      *
      * Show current or history shipments toggle?
      *
     C                   WHEN      *IN11
     C                   EXSR      $CurrHist

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
     C                   EVAL      wk_W3SIZE = '1'
     C                   EXSR      $PromptEmply
     C                   EVAL      *IN12 = *OFF
     C                   EVAL      *IN22 = *OFF

      *
      * More function keys?
      *
     C                   WHEN      *IN24
     C                   IF        wk_FKeySet = '1'
     C                   EVAL      wk_FKeySet = '2'
     C                   ELSE
     C                   EVAL      wk_FKeySet = '1'
     C                   ENDIF                                                  wk_FKeySet='1'
     C                   EXSR      $FuncKey
     C                   EVAL      *IN24 = *OFF
     C                   ITER

     C                   ENDSL

      *
      * Home Key pressed?, set cursor on position field.
      *
     C                   IF        *IN94
     C                   EVAL      *IN49 = *ON
     C                   ENDIF                                                  *IN94

     C                   ELSE

      *
      * Enter Key
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
      * Read and Process Subfile
      *
     C                   EXSR      $ProcessSf1

     C                   IF        C1GRP# <> *Zero  or  C1BLTO <> *Zero  or
     C                              C1SHTO <> *Zero  or  C1STNM <> *Blanks
     C                   EXSR      $PageDownSf1
     C                   ENDIF                                                  C1GRP#<>*Zero or

     C                   ELSE
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  not wk_Error

     C                   ENDIF                                                  wk_Sfl01RRN=*Zero

     C                   ENDIF                                                  *IN25=*ON

     C                   ENDDO                                                  not *IN03

     C                   ENDDO

     C                   EXSR      $ShutDownPgms
     C                   EVAL      *INLR = *ON

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

     C/SPACE 3
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
     C                   WHEN      C1GRP# <> *Zero
     C                   EVAL      wk_Sort = '1'                                by Shipment group
     C                   IF        not wk_History
     C                   IF        not %open(HYLSGCU1)
     C                   OPEN      HYLSGCU1
     C                   ENDIF                                                  not %open(HYLSGCU1)
     C                   ELSE
     C                   IF        not %open(HYLSGCU11)
     C                   OPEN      HYLSGCU11
     C                   ENDIF                                                  not %open(HYLSGCU11)
     C                   ENDIF                                                  not wk_History
     C                   WHEN      C1BLTO <> *Zero
     C                   EVAL      wk_Sort = '2'                                by Bill-to#
     C                   IF        not wk_History
     C                   IF        not %open(HYLSGCU2)
     C                   OPEN      HYLSGCU2
     C                   ENDIF                                                  not %open(HYLSGCU2)
     C                   ELSE
     C                   IF        not %open(HYLSGCU12)
     C                   OPEN      HYLSGCU12
     C                   ENDIF                                                  not %open(HYLSGCU12)
     C                   ENDIF                                                  not wk_History
     C                   WHEN      C1SHTO <> *Zero
     C                   EVAL      wk_Sort = '3'                                by Ship-to#
     C                   IF        not wk_History
     C                   IF        not %open(HYLSGCU3)
     C                   OPEN      HYLSGCU3
     C                   ENDIF                                                  not %open(HYLSGCU3)
     C                   ELSE
     C                   IF        not %open(HYLSGCU13)
     C                   OPEN      HYLSGCU13
     C                   ENDIF                                                  not %open(HYLSGCU13)
     C                   ENDIF                                                  not wk_History
     C                   WHEN      C1STNM <> *Blanks
     C                   EVAL      wk_Sort = '4'                                by Ship-to name
     C                   IF        not wk_History
     C                   IF        not %open(HYLSGCU4)
     C                   OPEN      HYLSGCU4
     C                   ENDIF                                                  not %open(HYLSGCU4)
     C                   ELSE
     C                   IF        not %open(HYLSGCU14)
     C                   OPEN      HYLSGCU14
     C                   ENDIF                                                  not %open(HYLSGCU14)
     C                   ENDIF                                                  not wk_History
     C                   ENDSL
     C                   ENDIF                                                  not *IN91 and

      *
      * If no records, clear subfile and initialize relative record and page count.
      *
     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     *LOVAL        SETLL     HYLSGCU1
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     *LOVAL        SETLL     HYLSGCU2
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     *LOVAL        SETLL     HYLSGCU3
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     *LOVAL        SETLL     HYLSGCU4
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     *LOVAL        SETLL     HYLSGCU11
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     *LOVAL        SETLL     HYLSGCU12
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     *LOVAL        SETLL     HYLSGCU13
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     *LOVAL        SETLL     HYLSGCU14
     C                   ENDSL
     C                   ENDIF                                                  not wk_History
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
     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY1       SETGT     HYLSGCU1
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY1       SETLL     HYLSGCU1
     C                   OTHER                                                  Enter,...
     C     C1GRP#        SETLL     HYLSGCU1
     C                   ENDSL
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY2       SETGT     HYLSGCU2
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY2       SETLL     HYLSGCU2
     C                   OTHER                                                  Enter,...
     C     C1BLTO        SETLL     HYLSGCU2
     C                   ENDSL
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY3       SETGT     HYLSGCU3
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY3       SETLL     HYLSGCU3
     C                   OTHER                                                  Enter,...
     C     C1SHTO        SETLL     HYLSGCU3
     C                   ENDSL
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY4       SETGT     HYLSGCU4
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY4       SETLL     HYLSGCU4
     C                   OTHER                                                  Enter,...
     C     C1STNM        SETLL     HYLSGCU4
     C                   ENDSL
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY1       SETGT     HYLSGCU11
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY1       SETLL     HYLSGCU11
     C                   OTHER                                                  Enter,...
     C     C1GRP#        SETLL     HYLSGCU11
     C                   ENDSL
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY2       SETGT     HYLSGCU12
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY2       SETLL     HYLSGCU12
     C                   OTHER                                                  Enter,...
     C     C1BLTO        SETLL     HYLSGCU12
     C                   ENDSL
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY3       SETGT     HYLSGCU13
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY3       SETLL     HYLSGCU13
     C                   OTHER                                                  Enter,...
     C     C1SHTO        SETLL     HYLSGCU13
     C                   ENDSL
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C                   SELECT
     C                   WHEN      *IN91                                        Page Down
     C     PGDKEY4       SETGT     HYLSGCU14
     C                   WHEN      *IN05                                        Refresh
     C     RFSKEY4       SETLL     HYLSGCU14
     C                   OTHER                                                  Enter,...
     C     C1STNM        SETLL     HYLSGCU14
     C                   ENDSL
     C                   ENDSL
     C                   ENDIF                                                  not wk_History

     C                   ENDIF                                                  not %found

     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   DOU       %eof(HYLSGCU1)  or  wk_Include
     C                   READ      HYLSGCU1
     C                   IF        not %eof(HYLSGCU1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU1)
     C                   ENDDO                                                  %eof(HYLSGCU1) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   DOU       %eof(HYLSGCU2)  or  wk_Include
     C                   READ      HYLSGCU2
     C                   IF        not %eof(HYLSGCU2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU2)
     C                   ENDDO                                                  %eof(HYLSGCU2) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU3)  or  wk_Include
     C                   READ      HYLSGCU3
     C                   IF        not %eof(HYLSGCU3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU3)
     C                   ENDDO                                                  %eof(HYLSGCU3) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU4)  or  wk_Include
     C                   READ      HYLSGCU4
     C                   IF        not %eof(HYLSGCU4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU4)
     C                   ENDDO                                                  %eof(HYLSGCU4) or
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   DOU       %eof(HYLSGCU11)  or  wk_Include
     C                   READ      HYLSGCU11
     C                   IF        not %eof(HYLSGCU11)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU11)
     C                   ENDDO                                                  %eof(HYLSGCU11) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   DOU       %eof(HYLSGCU12)  or  wk_Include
     C                   READ      HYLSGCU12
     C                   IF        not %eof(HYLSGCU12)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU12)
     C                   ENDDO                                                  %eof(HYLSGCU12) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU13)  or  wk_Include
     C                   READ      HYLSGCU13
     C                   IF        not %eof(HYLSGCU13)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU13)
     C                   ENDDO                                                  %eof(HYLSGCU13) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C                   DOU       %eof(HYLSGCU14)  or  wk_Include
     C                   READ      HYLSGCU14
     C                   IF        not %eof(HYLSGCU14)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU14)
     C                   ENDDO                                                  %eof(HYLSGCU14) or
     C                   ENDSL
     C                   ENDIF                                                  not wk_History

      *
      * If EOF, send Bottom of the List message
      *
     C                   IF        %eof
     C                   EVAL      wk_MsgID = 'GUM0002'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   IF        not *IN91  and  not *IN05
     C                   IF        not wk_SSZero
     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     *Loval        SETLL     HYLSGCU1
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     *Loval        SETLL     HYLSGCU2
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     *Loval        SETLL     HYLSGCU3
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     *Loval        SETLL     HYLSGCU4
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     *Loval        SETLL     HYLSGCU11
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     *Loval        SETLL     HYLSGCU12
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     *Loval        SETLL     HYLSGCU13
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     *Loval        SETLL     HYLSGCU14
     C                   ENDSL
     C                   ENDIF                                                  not wk_History
     C                   IF        %found
     C                   EXSR      $Bottom1
     C                   ELSE
     C                   EVAL      S@MORE = 'Bottom'
     C                   ENDIF                                                  %found
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      wk_SSZero = *OFF
     C                   ENDIF                                                  not wk_SSZero
     C                   ENDIF                                                  not *IN91 and

      *
      * If refresh, get last record.
      *
     C                   IF        *IN05
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     *Hival        SETGT     HYLSGCU1
     C                   DOU       %eof(HYLSGCU1)  or  wk_Include
     C                   READP     HYLSGCU1
     C                   IF        not %eof(HYLSGCU1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU1)
     C                   ENDDO                                                  %eof(HYLSGCU1) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     *Hival        SETGT     HYLSGCU2
     C                   DOU       %eof(HYLSGCU2)  or  wk_Include
     C                   READP     HYLSGCU2
     C                   IF        not %eof(HYLSGCU2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU2)
     C                   ENDDO                                                  %eof(HYLSGCU2) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     *Hival        SETGT     HYLSGCU3
     C                   DOU       %eof(HYLSGCU3)  or  wk_Include
     C                   READP     HYLSGCU3
     C                   IF        not %eof(HYLSGCU3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU3)
     C                   ENDDO                                                  %eof(HYLSGCU3) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     *Hival        SETGT     HYLSGCU4
     C                   DOU       %eof(HYLSGCU4)  or  wk_Include
     C                   READP     HYLSGCU4
     C                   IF        not %eof(HYLSGCU4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU4)
     C                   ENDDO                                                  %eof(HYLSGCU4) or
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     *Hival        SETGT     HYLSGCU11
     C                   DOU       %eof(HYLSGCU11)  or  wk_Include
     C                   READP     HYLSGCU11
     C                   IF        not %eof(HYLSGCU1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU11)
     C                   ENDDO                                                  %eof(HYLSGCU11) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     *Hival        SETGT     HYLSGCU12
     C                   DOU       %eof(HYLSGCU12)  or  wk_Include
     C                   READP     HYLSGCU12
     C                   IF        not %eof(HYLSGCU12)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU12)
     C                   ENDDO                                                  %eof(HYLSGCU12) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     *Hival        SETGT     HYLSGCU13
     C                   DOU       %eof(HYLSGCU13)  or  wk_Include
     C                   READP     HYLSGCU13
     C                   IF        not %eof(HYLSGCU13)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU13)
     C                   ENDDO                                                  %eof(HYLSGCU13) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     *Hival        SETGT     HYLSGCU14
     C                   DOU       %eof(HYLSGCU14)  or  wk_Include
     C                   READP     HYLSGCU14
     C                   IF        not %eof(HYLSGCU14)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU14)
     C                   ENDDO                                                  %eof(HYLSGCU14) or
     C                   ENDSL
     C                   ENDIF                                                  not wk_History
     C                   IF        %eof
     C                   EVAL      S@MORE = 'Bottom'
     C                   EVAL      C1GRP# = *Zero
     C                   EVAL      C1BLTO = *Zero
     C                   EVAL      C1SHTO = *Zero
     C                   EVAL      C1STNM = *Blanks
     C                   LEAVESR
     C                   ENDIF                                                  %eof
     C                   ENDIF                                                  *IN05

     C                   ELSE
      *
      * Clear Subfile, Initialize Rel Rec, Page Count
      *
     C                   EXSR      $ClearSfl01

     C                   ENDIF                                                  %eof

     C                   DOW       (not wk_History  and
     C                              ((wk_Sort = '1' and not %eof(HYLSGCU1)) or
     C                               (wk_Sort = '2' and not %eof(HYLSGCU2)) or
     C                               (wk_Sort = '3' and not %eof(HYLSGCU3)) or
     C                               (wk_Sort = '4' and not %eof(HYLSGCU4))))
     C                                   or
     C                             (wk_History  and
     C                              ((wk_Sort = '1' and not %eof(HYLSGCU11)) or
     C                               (wk_Sort = '2' and not %eof(HYLSGCU12)) or
     C                               (wk_Sort = '3' and not %eof(HYLSGCU13)) or
     C                               (wk_Sort = '4' and not %eof(HYLSGCU14))))

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
     C                   EVAL      @UGRP# = GCGRP#
     C                   EVAL      @UCPTY = GCCPTY
     C                   EVAL      @UBLTO = GCBLTO
     C                   EVAL      @USHTO = GCSHTO
     C                   EVAL      @UDSHP = GCDSHP
     C                   EVAL      @USTNM = GCSTNM
     C                   ENDIF                                                  wk_Sfl01PC=1
     C                   EVAL      @DGRP# = GCGRP#
     C                   EVAL      @DCPTY = GCCPTY
     C                   EVAL      @DBLTO = GCBLTO
     C                   EVAL      @DSHTO = GCSHTO
     C                   EVAL      @DDSHP = GCDSHP
     C                   EVAL      @DSTNM = GCSTNM

      *
      * Get next shipment group customer record.
      *
     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   DOU       %eof(HYLSGCU1)  or  wk_Include
     C                   READ      HYLSGCU1
     C                   IF        not %eof(HYLSGCU1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU1)
     C                   ENDDO                                                  %eof(HYLSGCU1) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   DOU       %eof(HYLSGCU2)  or  wk_Include
     C                   READ      HYLSGCU2
     C                   IF        not %eof(HYLSGCU2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU2)
     C                   ENDDO                                                  %eof(HYLSGCU2) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU3)  or  wk_Include
     C                   READ      HYLSGCU3
     C                   IF        not %eof(HYLSGCU3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU3)
     C                   ENDDO                                                  %eof(HYLSGCU3) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C                   DOU       %eof(HYLSGCU4)  or  wk_Include
     C                   READ      HYLSGCU4
     C                   IF        not %eof(HYLSGCU4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU4)
     C                   ENDDO                                                  %eof(HYLSGCU4) or
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   DOU       %eof(HYLSGCU11)  or  wk_Include
     C                   READ      HYLSGCU11
     C                   IF        not %eof(HYLSGCU11)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU11)
     C                   ENDDO                                                  %eof(HYLSGCU11) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   DOU       %eof(HYLSGCU12)  or  wk_Include
     C                   READ      HYLSGCU12
     C                   IF        not %eof(HYLSGCU12)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU12)
     C                   ENDDO                                                  %eof(HYLSGCU12) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU13)  or  wk_Include
     C                   READ      HYLSGCU13
     C                   IF        not %eof(HYLSGCU13)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU13)
     C                   ENDDO                                                  %eof(HYLSGCU13) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C                   DOU       %eof(HYLSGCU14)  or  wk_Include
     C                   READ      HYLSGCU14
     C                   IF        not %eof(HYLSGCU14)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU14)
     C                   ENDDO                                                  %eof(HYLSGCU14) or
     C                   ENDSL
     C                   ENDIF                                                  not wk_History

     C                   ENDDO                                                  not wk_History and

      *
      * Set More... or Bottom   subfile messge.
      *
     C                   IF        (not wk_History  and
     C                              ((wk_Sort = '1' and %eof(HYLSGCU1)) or
     C                               (wk_Sort = '2' and %eof(HYLSGCU2)) or
     C                               (wk_Sort = '3' and %eof(HYLSGCU3)) or
     C                               (wk_Sort = '4' and %eof(HYLSGCU4))))
     C                                   or
     C                             (wk_History  and
     C                              ((wk_Sort = '1' and %eof(HYLSGCU11)) or
     C                               (wk_Sort = '2' and %eof(HYLSGCU12)) or
     C                               (wk_Sort = '3' and %eof(HYLSGCU13)) or
     C                               (wk_Sort = '4' and %eof(HYLSGCU14))))
     C                   EVAL      S@MORE = 'Bottom'
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  not wk_History and

     C                   EVAL      C1GRP# = *Zero
     C                   EVAL      C1BLTO = *Zero
     C                   EVAL      C1SHTO = *Zero
     C                   EVAL      C1STNM = *Blanks

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

     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     PGUKEY1       SETLL     HYLSGCU1
     C                   DOU       %eof(HYLSGCU1)  or  wk_Include
     C                   READP     HYLSGCU1
     C                   IF        not %eof(HYLSGCU1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU1)
     C                   ENDDO                                                  %eof(HYLSGCU1) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     PGUKEY2       SETLL     HYLSGCU2
     C                   DOU       %eof(HYLSGCU2)  or  wk_Include
     C                   READP     HYLSGCU2
     C                   IF        not %eof(HYLSGCU2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU2)
     C                   ENDDO                                                  %eof(HYLSGCU2) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     PGUKEY3       SETLL     HYLSGCU3
     C                   DOU       %eof(HYLSGCU3)  or  wk_Include
     C                   READP     HYLSGCU3
     C                   IF        not %eof(HYLSGCU3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU3)
     C                   ENDDO                                                  %eof(HYLSGCU3) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     PGUKEY4       SETLL     HYLSGCU4
     C                   DOU       %eof(HYLSGCU4)  or  wk_Include
     C                   READP     HYLSGCU4
     C                   IF        not %eof(HYLSGCU4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU4)
     C                   ENDDO                                                  %eof(HYLSGCU4) or
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C     PGUKEY1       SETLL     HYLSGCU11
     C                   DOU       %eof(HYLSGCU11)  or  wk_Include
     C                   READP     HYLSGCU11
     C                   IF        not %eof(HYLSGCU11)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU11)
     C                   ENDDO                                                  %eof(HYLSGCU11) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C     PGUKEY2       SETLL     HYLSGCU12
     C                   DOU       %eof(HYLSGCU12)  or  wk_Include
     C                   READP     HYLSGCU12
     C                   IF        not %eof(HYLSGCU12)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU12)
     C                   ENDDO                                                  %eof(HYLSGCU12) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C     PGUKEY3       SETLL     HYLSGCU13
     C                   DOU       %eof(HYLSGCU13)  or  wk_Include
     C                   READP     HYLSGCU13
     C                   IF        not %eof(HYLSGCU13)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU13)
     C                   ENDDO                                                  %eof(HYLSGCU13) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to name
     C     PGUKEY4       SETLL     HYLSGCU14
     C                   DOU       %eof(HYLSGCU14)  or  wk_Include
     C                   READP     HYLSGCU14
     C                   IF        not %eof(HYLSGCU14)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU14)
     C                   ENDDO                                                  %eof(HYLSGCU14) or
     C                   ENDSL
     C                   ENDIF                                                  not wk_History

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

     C                   DOW       (not wk_History  and
     C                              ((wk_Sort = '1' and not %eof(HYLSGCU1)) or
     C                               (wk_Sort = '2' and not %eof(HYLSGCU2)) or
     C                               (wk_Sort = '3' and not %eof(HYLSGCU3)) or
     C                               (wk_Sort = '4' and not %eof(HYLSGCU4))))
     C                                   or
     C                             (wk_History  and
     C                              ((wk_Sort = '1' and not %eof(HYLSGCU11)) or
     C                               (wk_Sort = '2' and not %eof(HYLSGCU12)) or
     C                               (wk_Sort = '3' and not %eof(HYLSGCU13)) or
     C                               (wk_Sort = '4' and not %eof(HYLSGCU14))))
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
     C                   EVAL      @DGRP# = GCGRP#
     C                   EVAL      @DCPTY = GCCPTY
     C                   EVAL      @DBLTO = GCBLTO
     C                   EVAL      @DSHTO = GCSHTO
     C                   EVAL      @DDSHP = GCDSHP
     C                   EVAL      @DSTNM = GCSTNM
     C                   ENDIF                                                  wk_Sfl01RRN=wk_Sfl01
     C                   Z-ADD     wk_Sfl01RRN   S@RCNO
     C                   EVAL      @UGRP# = GCGRP#
     C                   EVAL      @UCPTY = GCCPTY
     C                   EVAL      @UBLTO = GCBLTO
     C                   EVAL      @USHTO = GCSHTO
     C                   EVAL      @UDSHP = GCDSHP
     C                   EVAL      @USTNM = GCSTNM

      *
      * Get previous shipment group customer record.
      *
     C                   IF        not wk_History
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   DOU       %eof(HYLSGCU1)  or  wk_Include
     C                   READP     HYLSGCU1
     C                   IF        not %eof(HYLSGCU1)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU1)
     C                   ENDDO                                                  %eof(HYLSGCU1) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   DOU       %eof(HYLSGCU2)  or  wk_Include
     C                   READP     HYLSGCU2
     C                   IF        not %eof(HYLSGCU2)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU2)
     C                   ENDDO                                                  %eof(HYLSGCU2) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU3)  or  wk_Include
     C                   READP     HYLSGCU3
     C                   IF        not %eof(HYLSGCU3)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU3)
     C                   ENDDO                                                  %eof(HYLSGCU3) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU4)  or  wk_Include
     C                   READP     HYLSGCU4
     C                   IF        not %eof(HYLSGCU4)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU4)
     C                   ENDDO                                                  %eof(HYLSGCU4) or
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C                   WHEN      wk_Sort = '1'                                by Shipment group
     C                   DOU       %eof(HYLSGCU11)  or  wk_Include
     C                   READP     HYLSGCU11
     C                   IF        not %eof(HYLSGCU11)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU11)
     C                   ENDDO                                                  %eof(HYLSGCU11) or
     C                   WHEN      wk_Sort = '2'                                by Bill-to#
     C                   DOU       %eof(HYLSGCU12)  or  wk_Include
     C                   READP     HYLSGCU12
     C                   IF        not %eof(HYLSGCU12)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU12)
     C                   ENDDO                                                  %eof(HYLSGCU12) or
     C                   WHEN      wk_Sort = '3'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU13)  or  wk_Include
     C                   READP     HYLSGCU13
     C                   IF        not %eof(HYLSGCU13)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU13)
     C                   ENDDO                                                  %eof(HYLSGCU13) or
     C                   WHEN      wk_Sort = '4'                                by Ship-to#
     C                   DOU       %eof(HYLSGCU14)  or  wk_Include
     C                   READP     HYLSGCU14
     C                   IF        not %eof(HYLSGCU14)
     C                   EXSR      $Filter1
     C                   ENDIF                                                  not %eof(HYLSGCU14)
     C                   ENDDO                                                  %eof(HYLSGCU14) or
     C                   ENDSL
     C                   ENDIF                                                  not wk_History

     C                   ENDDO                                                  wk_Sort='1' and

      *
      * Set More... or Top if there are more records or BOF.
      * If Beg of File reached and not a full page, fill from top down.
      *
     C                   IF        (not wk_History  and
     C                              ((wk_Sort = '1' and %eof(HYLSGCU1)) or
     C                               (wk_Sort = '2' and %eof(HYLSGCU2)) or
     C                               (wk_Sort = '3' and %eof(HYLSGCU3)) or
     C                               (wk_Sort = '4' and %eof(HYLSGCU4))))
     C                                   or
     C                             (wk_History  and
     C                              ((wk_Sort = '1' and %eof(HYLSGCU11)) or
     C                               (wk_Sort = '2' and %eof(HYLSGCU12)) or
     C                               (wk_Sort = '3' and %eof(HYLSGCU13)) or
     C                               (wk_Sort = '4' and %eof(HYLSGCU14))))
     C                   EVAL      S@MORE = 'Top'
     C                   IF        wk_Sfl01PC <> wk_Sfl01PS
     C                   EVAL      @UGRP# = *Zero
     C                   EVAL      @UCPTY = *Zero
     C                   EVAL      @UBLTO = *Zero
     C                   EVAL      @USHTO = *Zero
     C                   EVAL      @UDSHP = *Zero
     C                   EVAL      @USTNM = *Loval
     C                   EXSR      $PageDownSf1
     C                   ENDIF                                                  wk_Sfl01PC<>wk_Sfl01
     C                   ELSE
     C                   EVAL      S@MORE = 'More..'
     C                   ENDIF                                                  not wk_History and

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move database fields to subfile fields.                                *
      **************************************************************************
     CSR   $MoveToSfl01  BEGSR

     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      S1GRP# = GCGRP#
     C                   EVAL      S1CPTY = GCCPTY
     C                   EVAL      S1BLTO = GCBLTO
     C                   EVAL      S1BTNM = GCBTNM
     C                   EVAL      S1SHTO = GCSHTO
     C                   EVAL      S1DSHP = GCDSHP
     C                   EVAL      S1STNM = GCSTNM
     C                   EVAL      S1STNMX = GCSTNM
     C                   EVAL      S1STZP = GCSTZP
     C                   EVAL      S1SHPV = GCSHPV
     C                   EVAL      S1SVDS = GCSVDS
     C                   EVAL      S1FRZN = GCFRZN
     C                   EVAL      S1ITCT = GCITCT
     C                   EVAL      S1SHWT = GCSHWT
     C                   EVAL      S1TSTP1 = GCTSTP1
     C                   IF        GCTSTP2 = *Loval
     C                   EVAL      S1COMP = *Blank
     C                   ELSE
     C                   EVAL      S1COMP = 'Y'
     C                   ENDIF                                                  GCTSTP1=*Loval
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HDCCMT
                where CXVCF='C' and CXNMBR in (:GCBLTO, :GCSHTO) and
                 CXDOCT='PIC';
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGDT a join OEOCMT b on a.GDORD#=b.OCORD#
                  where a.GDGRP#=:GCGRP# and a.GDBLTO=:GCBLTO and
                   a.GDSHTO=:GCSHTO and a.GDDSHP=:GCDSHP and
                   b.OCDOCT='PIC';
       ENDIF;                                                                // wk_RcdCnt=*Zero
       IF wk_RcdCnt = *Zero;
         S1CMTS = *Blank;
       ELSE;
         S1CMTS = '*';
       ENDIF;                                                                // wk_RcdCnt=*Zero

       wk_PackLane = *Blanks;
       EXEC SQL select listagg(distinct GDDVRT,',')
                 within group(order by GDDVRT)
                 into :wk_PackLane
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and GDDVRT<>0 and
                      GDQOPK>0;

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_Rcdcnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and GDCNVY='P' and
                      GDQOPK>0;

       IF wk_RcdCnt > *Zero;
         IF wk_PackLane = *Blanks;
           wk_PackLane = 'P';
         ELSE;
           wk_PackLane = %trimr(wk_PackLane) + ',P';
         ENDIF;                                                              // wk_PackLane=*Blanks
       ENDIF;                                                                // wk_RcdCnt>*Zero

       // GHOPWOOD - Show '999's as Digi-Pick
       if wk_PackLane = '999';
         S1LANE = 'Digi Pick';
       else;
         S1LANE = wk_PackLane;
       endif;

      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Filter SFL01 main subfile.                                             *
      **************************************************************************
     CSR   $Filter1      BEGSR

     C                   EVAL      wk_Include = *ON

      *
      * DC location subset.
      *
     C                   IF        GCGRP# <> GHGRP#
     C     GCGRP#        CHAIN     HYPSGHD
     C                   IF        not %found(HYPSGHD)
     C                   EVAL      GHDCLOC = F4DCLOC
     C                   ENDIF                                                  not %found(HYPSGHD)
     C                   ENDIF                                                  GCGRP#<>GHGRP#
     C                   IF        GHDCLOC <> F4DCLOC
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GHDCLOC<>F4DCLOC

      *
      * Exclude completed shipments if processing/viewing open shipments only.
      *
     C                   IF        not wk_Completed  and  not wk_History
     C                               and  GCTSTP2 <> *Loval
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  not wk_Completed and

      *
      * History subsets.
      *
     C                   IF        wk_History
     C                   SELECT
     C                   WHEN      W1HIST = 1                                   Yesterday only
     C                   IF        %date(GCTSTP2) <> wk_HistCutoff
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  %date(GCTSTP2)<>
     C                   WHEN      W1HIST = 2                                   Last 7 days
     C                   IF        %date(GCTSTP2) < wk_HistCutoff
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  %date(GCTSTP2)<
     C                   WHEN      W1HIST = 3                                   Date range
     C                   IF        %date(GCTSTP2) < W1FRDT1D  or
     C                               %date(GCTSTP2) > W1TODT2D
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  %date(GCTSTP2)<
     C                   ENDSL
     C                   ENDIF                                                  wk_History

      *
      * Bill-to number subset.
      *
     C                   IF        F4BLTO <> *Zero
     C                   IF        GCBLTO <> F4BLTO
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GCBLTO<>F4BLTO
     C                   ENDIF                                                  F4BLTO<>*Zero

      *
      * Ship-to number subset.
      *
     C                   IF        F4SHTO <> *Zero
     C                   IF        GCSHTO <> F4SHTO
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GCSHTO<>F4SHTO
     C                   ENDIF                                                  F4SHTO<>*Zero

      *
      * Ship-via subset.
      *
     C                   IF        F4SHPV <> *Blanks
     C                   IF        GCSHPV <> F4SHPV
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  GCSHPV<>F4SHPV
     C                   ENDIF                                                  F4SHPV<>*Blanks

      *
      * Shipping lane subset.
      *
     C                   IF        F4DVRT <> *Zero
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      substr(digits(GDDVRT),7,3)=:F4DVRTX;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4DVRT<>*Zero

      *
      * Include shipments with one or more pallet picks subset.
      *
     C                   IF        F4IPPK = 'Y'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      GDCNVY='P';
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4IPPK='Y'

      *
      * Include shipments with one or more tote picks subset.
      *
     C                   IF        F4ITPK = 'Y'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      (GDCNVY='T' or (GDCNVY='S' and GDSPPK='T'));
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4ITPK='Y'

      *
      * Include shipments designated as small parcel picks subset.
      *
     C                   IF        F4ISPP = 'Y'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      GDCNVY='S';
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4ISPP='Y'

      *
      * Include shipments with pick priority overrides subset.
      *
     C                   IF        F4IPPO = 'Y'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      GDPIKO>0;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4IPPO='Y'

      *
      * Item number subset.
      *
     C                   IF        F4ITEM <> *Blanks
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      GDITEM=:F4ITEM;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4ITEM<>*Blanks

      *
      * Order number subset.
      *
     C                   IF        F4ORD# <> *Zero
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      GDORD#=:F4ORD#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4ORD#<>*Zero

      *
      * P.O. number subset.
      *
     C                   IF        F4PO# <> *Blanks
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and
                      GDPO#=:F4PO#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_Include = *OFF
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  F4PO#<>*Blanks

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
     C                   EVAL      wk_ChgPr = *OFF
     C                   EVAL      wk_DltPr = *OFF
     C                   EVAL      wk_DspPr = *OFF
     C                   EVAL      wk_DtlPr = *OFF
     C                   EVAL      wk_PlltzPr = *OFF
     C                   EVAL      wk_CompPr = *OFF
     C                   EVAL      wk_HldPr = *OFF
     C                   EVAL      wk_CmtsPr = *OFF
     C                   EVAL      wk_OptPr = *OFF
     C                   EVAL      wk_ClrDone = *OFF

      *
      * Save function key indicators.
      *
     C                   EVAL      wk_IN25 = *IN25

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01A

     C                   DOW       not %eof(HYD0600A)

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
      * Changes present?
      *
     C                   IF        (S1OPT = @CHG  or  S1OPT = @CHG2)  and
     C                               wk_PgmMode = MaintMode

     C                   EVAL      S1OPT = @CHG
     C                   EVAL      wk_ChgPr = *ON
     C                   UPDATE    SFL01A

     C                   ELSE

      *
      * Deletes present?
      *
     C                   IF        (S1OPT = @DLT  or  S1OPT = @DLT2)  and
     C                               wk_PgmMode = MaintMode  and  wk_SuperUser

     C                   EVAL      S1OPT = @DLT

     C                   EVAL      wk_DltChk =
     C                               DltOK(S1GRP#:S1BLTO:S1SHTO:S1DSHP)

     C                   IF        wk_DltChk = *Zeros

     C                   EVAL      wk_DltPr = *ON
     C                   UPDATE    SFL01A

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
     C                   WRITE     SFLCTL02A
     C                   EVAL      *IN29 = *OFF
     C                   EVAL      wk_Sfl02RRN = *Zero
     C                   ENDIF                                                  wk_ClrDone=*OFF

      *
      *  Write to "delete" subfile.
      *
     C                   EVAL      wk_Sfl02RRN = wk_Sfl02RRN + 1
     C                   WRITE     SFL02A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1237'
     C                   EVAL      wk_MsgData = '01'
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
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   UPDATE    SFL01A

     C                   ENDIF                                                  wk_DltChk=*Zeros

     C                   ELSE

      *
      * Displays present?
      *
     C                   IF        S1OPT = @DSP  or  S1OPT = @DSP2

     C                   EVAL      S1OPT = @DSP
     C                   EVAL      wk_DspPr = *ON
     C                   UPDATE    SFL01A

     C                   ELSE

      *
      * Details present?
      *
     C                   IF        S1OPT = @DTL  or  S1OPT = @DTL2

     C                   EVAL      S1OPT = @DTL
     C                   EVAL      wk_DtlPr = *ON
     C                   UPDATE    SFL01A

     C                   ELSE

      *
      * Palletizes present?
      *
     C                   IF        S1OPT = @PLLTZ  and
     C                               wk_PgmMode = MaintMode

     C                   EVAL      wk_PlltzChk =
     C                               PalletizeOK(S1GRP#:S1BLTO:S1SHTO:S1DSHP)

     C                   IF        wk_PlltzChk = *Zeros

     C                   EVAL      wk_PlltzPr = *ON
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1251'
     C                   EVAL      wk_MsgData = wk_PlltzChk
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
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_PlltzChk=*Zeros

     C                   ELSE

      *
      * Shipment completes present?
      *
     C                   IF        S1OPT = @COMP  and
     C                               wk_PgmMode = MaintMode

     C                   EVAL      wk_CompChk =
     C                               CompOK(S1GRP#:S1BLTO:S1SHTO:S1DSHP)

     C                   IF        wk_CompChk = *Zeros

     C                   EVAL      wk_CompPr = *ON
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   IF        wk_CompChk < '10'
     C                   EVAL      wk_MsgID = 'ERR1238'
     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR1538'
     C                   ENDIF                                                  wk_CompChk<'10'
     C                   EVAL      wk_MsgData = wk_CompChk
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
     C                   EVAL      %subst(wk_MsgData:33:3) =
     C                               %triml(%editc(wk_MaxFtCl:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_CompChk=*Zeros

     C                   ELSE

      *
      * Intransit holds present?
      *
     C                   IF        S1OPT = @HOLD  and
     C                               wk_PgmMode = MaintMode

     C                   EVAL      wk_IntrHoldChk =
     C                               IntrHoldOK(S1GRP#:S1BLTO:S1SHTO:S1DSHP)

     C                   IF        wk_IntrHoldChk = *Zeros

     C                   EVAL      wk_HldPr = *ON
     C                   UPDATE    SFL01A

     C                   ELSE

     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1265'
     C                   EVAL      wk_MsgData = wk_IntrHoldChk
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
     C                   EVAL      %subst(wk_MsgData:33:3) =
     C                               %triml(%editc(wk_MaxFtCl:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   UPDATE    SFL01A
     C                   ENDIF                                                  wk_IntrHoldChk=*Zero

     C                   ELSE

      *
      * Comments present?
      *
     C                   IF        S1OPT = @CMTS

     C                   EVAL      wk_CmtsPr = *ON
     C                   UPDATE    SFL01A

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
     C                   UPDATE    SFL01A

     C                   ENDIF                                                   S1OPT<>*Blanks

     C                   ENDIF                                                   S1OPT=@CMTS

     C                   ENDIF                                                   S1OPT=@HOLD

     C                   ENDIF                                                   S1OPT=@COMP

     C                   ENDIF                                                   S1OPT=@PLLTZ

     C                   ENDIF                                                   S1OPT=@DTL or

     C                   ENDIF                                                   S1OPT=@DSP or

     C                   ENDIF                                                   S1OPT=@DLT or

     C                   ENDIF                                                   S1OPT=@CHG or

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL01A

     C                   ENDDO                                                  not %eof(HYD0600A)

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
     C                   IF        wk_ChgPr  or  wk_DltPr  or  wk_DspPr  or
     C                               wk_DtlPr  or  wk_PlltzPr  or  wk_CompPr  or
     C                               wk_HldPr  or  wk_CmtsPr
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_ChgPr or
     C                   ELSE
     C                   IF        wk_ChgPr  or  wk_DltPr  or  wk_DspPr  or
     C                               wk_DtlPr  or  wk_PlltzPr  or  wk_CompPr  or
     C                               wk_HldPr  or  wk_CmtsPr
     C                   EVAL      wk_OptPr = *ON
     C                   ENDIF                                                  wk_ChgPr or
     C                   ENDIF                                                  *IN25

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process options for subfile SFL01.                                     *
      **************************************************************************
     CSR   $ProcessSf1   BEGSR

     C                   EVAL      *IN12 = *OFF

     C                   READC     SFL01A

     C                   DOW       not %eof(HYD0600A)  and  not *IN03

     C                   EVAL      *IN41 = *OFF

      *
      *  F12=Cancel Key
      *
     C                   IF        not *IN12

     C                   SELECT

     C                   WHEN      S1OPT = @CHG
     C                   EXSR      $Change

     C                   WHEN      S1OPT = @DSP
     C                   EXSR      $Display

     C                   WHEN      S1OPT = @DTL
     C                   EXSR      $Detail

     C                   WHEN      S1OPT = @PLLTZ
     C                   EXSR      $Palletize

     C                   WHEN      S1OPT = @COMP
     C                   EXSR      $CompShip

     C                   WHEN      S1OPT = @HOLD
     C                   EXSR      $IntrHold

     C                   WHEN      S1OPT = @CMTS
     C                   EVAL      wk_CmtsForced = *OFF
     C                   EXSR      $DspCmts

     C                   WHEN      S1OPT = @DLT

      *
      * Keep "deletes" active - turn SFLNXTCHG on.
      *
     C                   EVAL      *IN28 = *ON

     C                   ENDSL

     C                   IF        not *IN03
     C                   IF        *IN12  or  S1OPT = @DLT
     C                   EVAL      *IN28 = *ON
     C                   ELSE
     C                   EVAL      S1OPT = *Blanks
     C                   ENDIF                                                   *IN12
     C                   UPDATE    SFL01A
     C                   ENDIF                                                   not *IN03

     C                   EVAL      S@RCNO = wk_Sfl01RRN

     C                   ELSE

      *
      * Cancel requested - save remaining options.
      *
     C                   EVAL      *IN28 = *ON
     C                   UPDATE    SFL01A

     C                   ENDIF                                                  not *IN12

      *
      * Get next changed subfile record (save status of F12=Cancel).
      *
     C                   IF        not %eof(HYD0600A)  and  not *IN03
     C                   EVAL      wk_IN12 = *IN12
     C                   READC     SFL01A
     C                   EVAL      *IN12 = wk_IN12
     C                   ENDIF                                                  not %eof(HYD0600A)

     C                   ENDDO                                                  not %eof(HYD0600A)

      *
      * If "deletes" are present and Not F12=Cancel, process "deletes".
      *
     C                   IF        wk_DltPr  and  not *IN03  and  not *IN12
     C                   EVAL      S@RCNO = wk_FirstDlt
     C                   EXSR      $ProcDeletes
     C                   ENDIF                                                  wk_DltPr and

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
      * Process deletes.                                                       *
      **************************************************************************
     CSR   $ProcDeletes  BEGSR

     C                   DOW       not *IN03  and  not *IN12
     C

      *
      * Format delete confirmation screen.
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

      *
      * Read & process deletes, then exit.
      *
     C                   READC     SFL02A

     C                   DOW       not %eof(HYD0600A)

      /Free
       EXEC SQL delete from OECMWK a where exists
                (select * from HYPSGDT b where b.GDOCTL=a.O4OCTL and
                 GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                 GDSHTO=:S1SHTO and GDDSHP=:S1DSHP);

       EXEC SQL delete from OEDTWK a where exists
                (select * from HYPSGDT b where b.GDOCTL=a.O1OCTL and
                 GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                 GDSHTO=:S1SHTO and GDDSHP=:S1DSHP);

       EXEC SQL delete from OEHDWK a where exists
                (select * from HYPSGDT b where b.GDOCTL=a.H1OCTL and
                 GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                 GDSHTO=:S1SHTO and GDDSHP=:S1DSHP);

      //  EXEC SQL delete from HYW0610A                              Need to decrease W1QPIK and
      //          where W1GRP#=:S1GRP# and GDBLTO=:S1BLTO and         possibly W1QSHP.  Delete all
      //           GDSHTO=:S1SHTO and GDDSHP=:S1DSHP;                 records when all custs gone.

      //  CALLP(E) HOEDIP (GDTURN:*Zero)                             Delete all turns for cust/grp

       EXEC SQL update OEORHD a set OEBUSY=' ', OEUDF9=' '
                where OEBUSY<>' ' and substr(OEUDF9,1,8)='BOXLOGIX' and
                 exists (select * from HYPSGDT b where b.GDORD#=a.OEORD# and
                 GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                 GDSHTO=:S1SHTO and GDDSHP=:S1DSHP);

       EXEC SQL delete from HYPSGDT
                where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                 GDSHTO=:S1SHTO and GDDSHP=:S1DSHP;

       EXEC SQL delete from HYPSGCU
                where GCGRP#=:S1GRP# and GCBLTO=:S1BLTO and
                 GCSHTO=:S1SHTO and GCDSHP=:S1DSHP;

       wk_LogData = *Blanks;
       wk_LogMsg = 'Customer ' +  %triml(%editc(S1SHTO:'Z')) +
                   ' shipment group ' +  %triml(%editc(S1GRP#:'Z')) +
                   ' deleted by ' + %trimr(ps_User) + '.';
       EXEC SQL insert into hypmilg
                       (mllgty, mldata, mlmsg, mlpgm,
                        mluser, mlws, mltstp)
                values('BX01', :wk_LogData, :wk_LogMsg, :ps_Pgnm,
                       :ps_User, :ps_Job, current timestamp);
      /End-Free
     C                   EVAL      wk_Refresh = *ON

      *
      * Send "shipment group deleted" message.
      *
     C                   EVAL      wk_MsgID = 'MSG0370'
     C                   EVAL      wk_MsgData = %triml(%editc(S1SHTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:8:9) =
     C                               %triml(%editc(S1GRP#:'Z'))
     C                   EXSR      $SndPM

     C                   READC     SFL02A

     C                   ENDDO                                                  not %eof(HYD0600A)

     C                   LEAVE

     C                   ENDIF                                                  *IN25

     C                   ENDDO                                                  not *IN03 and

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
     C                   EXFMT     DSPFMT4A
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
     C                   EVAL      F4DVRTX = %editc(F4DVRT:'X')
     C                   IF        F4BLTO <> sv_SSBLTO  or
     C                               F4SHTO <> sv_SSSHTO  or
     C                               F4SHPV <> sv_SSSHPV  or
     C                               F4DVRT <> sv_SSDVRT  or
     C                               F4IPPK <> sv_SSIPPK  or
     C                               F4ITPK <> sv_SSITPK  or
     C                               F4ISPP <> sv_SSISPP  or
     C                               F4IPPO <> sv_SSIPPO  or
     C                               F4ITEM <> sv_SSITEM  or
     C                               F4ORD# <> sv_SSORD#  or
     C                               F4PO# <> sv_SSPO#  or
     C                               F4DCLOC <> sv_SSDCLOC  or
     C                               wk_Sfl01RRN = *Zero
     C                   EXSR      $Top1
     C                   EXSR      $SaveSS1
     C                   ENDIF                                                  F4BLTO<>sv_SSBLTO or
     C                   ENDIF                                                  *IN12

     C                   IF        F4BLTO <> *Zero  or
     C                               F4SHTO <> *Zero  or
     C                               F4SHPV <> *Blanks  or
     C                               F4ITPK <> *Blank  or
     C                               F4ISPP <> *Blank  or
     C                               F4IPPO <> *Blank  or
     C                               F4ITEM <> *Blanks  or
     C                               F4ORD# <> *Zero  or
     C                               F4PO# <> *Blanks  or
     C                              (F4DVRT <> *Zero  and  F4DVRT <> wk_DftLane)
     C                                or
     C                              (F4IPPK <> *Blank and
     C                                 F4IPPK <> wk_DftInclPPk)
     C                   EVAL      C1SSTX = '*SUBSET*'
     C                   ELSE
     C                   EVAL      C1SSTX = *Blanks
     C                   ENDIF                                                  F4BLTO<>*Zero or

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Validate subset prompt screen fields.                                  *
      **************************************************************************
     CSR   $EditSS1      BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Bill-to customer number must be valid.
      *
     C                   IF        F4BLTO <> *Zero
      /Free
       wk_CMCUST = *Zero;
       EXEC SQL select CMCUST into :wk_CMCUST
                from HDCUST
                where CMCUST=:F4BLTO;
      /End-Free
     C                   IF        wk_CMCUST <> F4BLTO
     C                   EVAL      wk_MsgID = 'ERR0396'
     C                   EVAL      wk_MsgData = %triml(%editc(F4BLTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGCU
                where GCBLTO=:F4BLTO;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1231'
     C                   EVAL      wk_MsgData = %triml(%editc(F4BLTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  wk_CMCUST<>F4BLTO
     C                   ENDIF                                                  F4BLTO<>*Zero

      *
      * Ship-to customer number must be valid.
      *
     C                   IF        F4SHTO <> *Zero
      /Free
       wk_CMCUST = *Zero;
       EXEC SQL select CMCUST into :wk_CMCUST
                from HDCUST
                where CMCUST=:F4SHTO;
      /End-Free
     C                   IF        wk_CMCUST <> F4SHTO
     C                   EVAL      wk_MsgID = 'ERR0909'
     C                   EVAL      wk_MsgData = %triml(%editc(F4SHTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGCU
                where GCSHTO=:F4SHTO;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1232'
     C                   EVAL      wk_MsgData = %triml(%editc(F4SHTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  wk_CMCUST<>F4SHTO
     C                   ENDIF                                                  F4SHTO<>*Zero

      *
      * Ship-via code must be valid.
      *
     C                   IF        F4SHPV <> *Blanks
      /Free
       wk_SVSVSV = *Blanks;
       EXEC SQL select SVSVSV into :wk_SVSVSV
                from HDSHPV
                where SVSVSV=:F4SHPV;
      /End-Free
     C                   IF        wk_SVSVSV <> F4SHPV
     C                   EVAL      wk_MsgID = 'ERR0137'
     C                   EVAL      wk_MsgData = F4SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGCU
                where GCSHPV=:F4SHPV;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1233'
     C                   EVAL      wk_MsgData = F4SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  wk_SVSVSV<>F4SHPV
     C                   ENDIF                                                  F4SHPV<>*Blanks

      *
      * Shipping lane must be valid.
      *
     C                   IF        F4DVRT <> *Zero
     C                   IF        (F4DVRT < *Zero  or  F4DVRT > 8)
     C                             and F4DVRT <> 999
     C                   EVAL      wk_MsgID = 'ERR1253'
     C                   EVAL      wk_MsgData = '1 thru 8 or 999'
     C                   EXSR      $SndPM
     C                   EVAL      *IN44 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4DVRT<*Zero or
     C                   ENDIF                                                  F4DVRT<>*Zero

      *
      * Includes pallet pick must be valid.
      *
     C                   IF        F4IPPK <> *Blank  and  F4IPPK <> 'Y'
     C                   EVAL      wk_MsgID = 'ERR1266'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN45 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4IPPK<>*Blank and

      *
      * Includes tote pick must be valid.
      *
     C                   IF        F4ITPK <> *Blank  and  F4ITPK <> 'Y'
     C                   EVAL      wk_MsgID = 'ERR1418'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN50 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4ITPK<>*Blank and

      *
      * Includes small-parcel pick must be valid.
      *
     C                   IF        F4ISPP <> *Blank  and  F4ISPP <> 'Y'
     C                   EVAL      wk_MsgID = 'ERR1419'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN51 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4ISPP<>*Blank and

      *
      * Includes pick priority override must be valid.
      *
     C                   IF        F4IPPO <> *Blank  and  F4IPPO <> 'Y'
     C                   EVAL      wk_MsgID = 'ERR1420'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN52 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4ISPP<>*Blank and

      *
      * Includes item number must be valid.
      *
     C                   IF        F4ITEM <> *Blanks
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HDIMST
                where IMITEM=:F4ITEM;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR0319'
     C                   EVAL      wk_MsgData = F4ITEM
     C                   EXSR      $SndPM
     C                   EVAL      *IN46 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  F4ITEM<>*Blanks

      *
      * Includes order number be valid.
      *
     C                   IF        F4ORD# <> *Zero
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEORHD
                where OEORD#=:F4ORD#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR0464'
     C                   EVAL      wk_MsgData = %triml(%editc(F4ORD#:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN47 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  F4ORD#<>*Zero

      *
      * Includes P.O. number be valid.
      *
     C                   IF        F4PO# <> *Blanks
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEORHD
                where OEORRF=:F4PO#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   EVAL      wk_MsgID = 'ERR1276'
     C                   EVAL      wk_MsgData = F4PO#
     C                   EXSR      $SndPM
     C                   EVAL      *IN48 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  F4PO#<>*Blanks

      *
      * DC location must be valid.
      *
     C                   IF        F4DCLOC = *Blanks
     C                   SELECT
     C                   WHEN      wk_Alda
     C                   EVAL      F4DCLOC = 'ALDA'
     C                   WHEN      wk_GI281
     C                   EVAL      F4DCLOC = 'GI'
     C                   OTHER
     C                   EVAL      F4DCLOC = 'WEST'
     C                   ENDSL
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4DCLOC=*Blanks

     C                   IF        F4DCLOC <> 'WEST'  and
     C                               F4DCLOC <> 'ALDA'  and
     C                               F4DCLOC <> 'GI'
     C                   EVAL      wk_MsgID = 'ERR1415'
     C                   EVAL      wk_MsgData = F4DCLOC
     C                   EXSR      $SndPM
     C                   EVAL      *IN49 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F4DCLOC<>*Blanks

      *
      * If error detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

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
     C                   EVAL      *IN48 = *OFF
     C                   EVAL      *IN49 = *OFF
     C                   EVAL      *IN50 = *OFF
     C                   EVAL      *IN51 = *OFF
     C                   EVAL      *IN52 = *OFF
     C                   EVAL      *IN89 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Reset subset prompt screen fields to initial program values.           *
      **************************************************************************
     CSR   $InitSS1      BEGSR

     C                   EVAL      F4BLTO = *Zero
     C                   EVAL      F4SHTO = *Zero
     C                   EVAL      F4SHPV = *Blanks
     C                   EVAL      F4DVRT = *Zero
     C                   EVAL      F4ITEM = *Blanks
     C                   EVAL      F4ORD# = *Zero
     C                   EVAL      F4PO# = *Blanks
     C                   EVAL      F4IPPK = *Blank
     C                   EVAL      F4ITPK = *Blank
     C                   EVAL      F4ISPP = *Blank
     C                   EVAL      F4IPPO = *Blank
     C                   SELECT
     C                   WHEN      wk_Alda
     C                   EVAL      F4DCLOC = 'ALDA'
     C                   WHEN      wk_GI281
     C                   EVAL      F4DCLOC = 'GI'
     C                   OTHER
     C                   EVAL      F4DCLOC = 'WEST'
     C                   ENDSL
     C                   EVAL      wk_DftLane = *Zero
     C                   EVAL      wk_DftInclPPk = *Blank

      /Free
       wk_TDKEY1 = *Blanks;
       wk_TDNF01 = *Zero;
       wk_TDFF01 = *Blank;
       EXEC SQL select TDKEY1, TDNF01, TDFF01
                 into :wk_TDKEY1, :wk_TDNF01, :wk_TDFF01
            from GUPTDAT
            where TDTABL='BXLLINDFTS' and TDKEY1=:ps_User;
       IF wk_TDKEY1 = ps_User  and  wk_TDNF01<>*Zero;
         F4DVRT = wk_TDNF01;
         wk_DftLane = wk_TDNF01;
       ENDIF;                                                                // wk_TDKEY1=ps_User
       IF wk_TDKEY1 = ps_User  and  wk_TDFF01='Y';
         F4IPPK = wk_TDFF01;
         wk_DftInclPPk = wk_TDFF01;
       ENDIF;                                                                // wk_TDKEY1=ps_User
      /End-Free

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Refresh subset prompt screen fields.                                   *
      **************************************************************************
     CSR   $RefreshSS1   BEGSR

     C                   EVAL      F4BLTO = sv_SSBLTO
     C                   EVAL      F4SHTO = sv_SSSHTO
     C                   EVAL      F4SHPV = sv_SSSHPV
     C                   EVAL      F4DVRT = sv_SSDVRT
     C                   EVAL      F4IPPK = sv_SSIPPK
     C                   EVAL      F4ITPK = sv_SSITPK
     C                   EVAL      F4ISPP = sv_SSISPP
     C                   EVAL      F4IPPO = sv_SSIPPO
     C                   EVAL      F4ITEM = sv_SSITEM
     C                   EVAL      F4ORD# = sv_SSORD#
     C                   EVAL      F4PO# = sv_SSPO#
     C                   EVAL      F4DCLOC = sv_SSDCLOC

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Save subset prompt screen fields.                                      *
      **************************************************************************
     CSR   $SaveSS1      BEGSR

     C                   EVAL      sv_SSBLTO = F4BLTO
     C                   EVAL      sv_SSSHTO = F4SHTO
     C                   EVAL      sv_SSSHPV = F4SHPV
     C                   EVAL      sv_SSDVRT = F4DVRT
     C                   EVAL      sv_SSIPPK = F4IPPK
     C                   EVAL      sv_SSITPK = F4ITPK
     C                   EVAL      sv_SSISPP = F4ISPP
     C                   EVAL      sv_SSIPPO = F4IPPO
     C                   EVAL      sv_SSITEM = F4ITEM
     C                   EVAL      sv_SSORD# = F4ORD#
     C                   EVAL      sv_SSPO# = F4PO#
     C                   EVAL      sv_SSDCLOC = F4DCLOC

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid values for subset prompt screen fields.                     *
      **************************************************************************
     CSR   $ListSS1      BEGSR

     C                   SELECT

     C                   WHEN      S@IFLD = 'F4BLTO'
     C                   EVAL      wk_Cust# = F4BLTO
     C                   EXSR      $ListCust
     C                   IF        wk_ValueRtnd
     C                   EVAL      F4BLTO = wk_Cust#
     C                   ENDIF                                                  wk_ValueRtnd
     C                   EVAL      *IN41 = *ON

     C                   WHEN      S@IFLD = 'F4SHTO'
     C                   EVAL      wk_Cust# = F4SHTO
     C                   EXSR      $ListCust
     C                   IF        wk_ValueRtnd
     C                   EVAL      F4SHTO = wk_Cust#
     C                   ENDIF                                                  wk_ValueRtnd
     C                   EVAL      *IN42 = *ON

     C                   WHEN      S@IFLD = 'F4SHPV'
     C                   EVAL      wk_ShipVia = F4SHPV
     C                   EXSR      $ListShipVia
     C                   IF        wk_ValueRtnd
     C                   EVAL      F4SHPV = wk_ShipVia
     C                   ENDIF                                                  wk_ValueRtnd
     C                   EVAL      *IN43 = *ON

     C                   OTHER
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0013'
     C                   EXSR      $SndPM

     C                   SELECT
     C                   WHEN      S@IFLD = 'F4DVRT'
     C                   EVAL      *IN44 = *ON
     C                   WHEN      S@IFLD = 'F4IPPK'
     C                   EVAL      *IN45 = *ON
     C                   WHEN      S@IFLD = 'F4ITPK'
     C                   EVAL      *IN50 = *ON
     C                   WHEN      S@IFLD = 'F4ISPP'
     C                   EVAL      *IN51 = *ON
     C                   WHEN      S@IFLD = 'F4IPPO'
     C                   EVAL      *IN52 = *ON
     C                   WHEN      S@IFLD = 'F4ITEM'
     C                   EVAL      *IN46 = *ON
     C                   WHEN      S@IFLD = 'F4ORD#'
     C                   EVAL      *IN47 = *ON
     C                   WHEN      S@IFLD = 'F4PO#'
     C                   EVAL      *IN48 = *ON
     C                   WHEN      S@IFLD = 'F4DCLOC'
     C                   EVAL      *IN49 = *ON
     C                   OTHER
     C                   EVAL      *IN41 = *ON
     C                   ENDSL

     C                   EVAL      *IN89 = *ON

     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Prompt for valid customer numbers.                                     *
      **************************************************************************
     CSR   $ListCust     BEGSR

     C                   EVAL      wk_ValueRtnd = *OFF

     C                   EVAL      io_Cust# = -1

     C                   CALLP(E)  HHDCSS (io_Cust#)

     C                   IF        io_Cust# = *Zero
     C                   EVAL      wk_MsgID = 'GUM0014'
     C                   EVAL      wk_MsgData = 'F12'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      wk_ValueRtnd = *ON
     C                   EVAL      wk_Cust# = io_Cust#
     C                   EVAL      wk_MsgID = 'MSG0116'
     C                   EVAL      wk_MsgData = %triml(%editc(wk_Cust#:'Z'))
     C                   EXSR      $SndPM
     C                   ENDIF                                                  io_Cust#=*Zero

     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN89 = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Prompt for valid ship via codes.                                       *
      **************************************************************************
     CSR   $ListShipVia  BEGSR

     C                   EVAL      wk_ValueRtnd = *OFF

     C                   EVAL      io_ShipVia = wk_ShipVia

     C                   CALLP(E)  HHDSVI (io_ShipVia)

     C                   IF        io_ShipVia = *Blanks
     C                   EVAL      wk_MsgID = 'GUM0014'
     C                   EVAL      wk_MsgData = 'F12'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      wk_ValueRtnd = *ON
     C                   EVAL      wk_ShipVia = io_ShipVia
     C                   EVAL      wk_MsgID = 'MSG0122'
     C                   EVAL      wk_MsgData = wk_ShipVia
     C                   EXSR      $SndPM
     C                   ENDIF                                                  io_ShipVia=*Blanks

     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN89 = *ON

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Include/Exclude completed shipments toggle.                            *
      **************************************************************************
     CSR   $InclComp     BEGSR

     C                   IF        wk_Completed
     C                   EVAL      wk_Completed = *OFF
     C                   EVAL      C1HDG2 = OpenShips
     C                   ELSE
     C                   EVAL      wk_Completed = *ON
     C                   EVAL      C1HDG2 = TodaysShips
     C                   ENDIF                                                  wk_Completed
     C                   EVAL      C1HDG2 = center(C1HDG2)

     C                   EVAL      *IN07 = *OFF

     C                   EXSR      $FuncKey

     C                   EVAL      @UGRP# = *Loval
     C                   EVAL      @UCPTY = *Loval
     C                   EVAL      @UBLTO = *Loval
     C                   EVAL      @USHTO = *Loval
     C                   EVAL      @UDSHP = *Loval
     C                   EVAL      @USTNM = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Find a shipment.                                                       *
      **************************************************************************
     CSR   $FindShip     BEGSR

     C                   EXSR      $InitIndW2

     C                   EVAL      W2SCAN = *Blanks
     C                   EVAL      W2MSG = *Blanks

     C                   DOU       *IN12

     C                   EXFMT     WINDOW02

     C                   EVAL      W2MSG = *Blanks
     C                   EXSR      $InitIndW2

     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

      *
      * Check for multiple customer shipment totes.  If found, prompt user
      * to select which customer shipment to process.
      *
     C                   EXSR      $ChkMultCS
     C                   IF        *IN12
     C                   EVAL      *IN12 = *OFF
     C                   WRITE     MSFCTL01A
     C                   IF        wk_Sfl01RRN = *Zero
     C                   WRITE     SFLNOR01
     C                   ELSE
     C                   EVAL      *IN27 = *ON
     C                   ENDIF                                                  wk_Sfl01RRN=*Zero
     C                   EVAL      *IN26 = *OFF
     C                   WRITE     SFLCMD01
     C                   WRITE     SFLCTL01A
     C                   EVAL      *IN27 = *OFF
     C                   EVAL      W2SCAN = *Blanks
     C                   ITER
     C                   ENDIF                                                  *IN12
     C                   IF        wk_MultCSFnd
     C                   LEAVE
     C                   ENDIF                                                  wk_MultCSFnd

     C                   EXSR      $EditW2
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   LEAVE

     C                   ENDDO                                                  *IN12

     C                   EVAL      wk_Pgm01 = *ON

     C                   CALLP(E)  HYR0602 (wk_PgmMode:wk_GDGRP#:wk_GDBLTO:
     C                               wk_GDSHTO:wk_GDDSHP:wk_RC0602)

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0602'
     C                   EXSR      $SNDPM
     C                   WHEN      wk_RC0602 = '03'
     C                   EVAL      *IN03 = *ON
     C                   WHEN      wk_RC0602 = '12'
     C                   WHEN      wk_RC0602 = *Zeros
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0602'
     C                   EXSR      $SNDPM
     C                   ENDSL

     C                   IF        not *IN03
      /Free
       wk_GCCPTY = *Zero;
       wk_GCSTNM = *Blanks;
       EXEC SQL select GCCPTY, GCSTNM into :wk_GCCPTY, :wk_GCSTNM
                from HYPSGCU
                where GCGRP#=:wk_GDGRP# and GCBLTO=:wk_GDBLTO and
                 GCSHTO=:wk_GDSHTO and GCDSHP=:wk_GDDSHP;
      /End-Free
     C                   EVAL      @UGRP# = wk_GDGRP#
     C                   EVAL      @UCPTY = wk_GCCPTY
     C                   EVAL      @UBLTO = wk_GDBLTO
     C                   EVAL      @USHTO = wk_GDSHTO
     C                   EVAL      @UDSHP = wk_GDDSHP
     C                   EVAL      @USTNM = wk_GCSTNM
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

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

     C                   ENDIF                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit find shipment prompt window fields.                                *
      **************************************************************************
     CSR   $EditW2       BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Barcode scan (or entry) is required.
      *
     C                   IF        W2SCAN = *Blanks
     C                   IF        not wk_Error
     C                   EVAL      W2MSG = 'Carton barcode or tote number is +
     C                               required.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   ENDIF                                                  W1SCAN=*Blanks

     C                   EVAL      W2SCAN = %triml(W2SCAN)
     C                   EVAL      wk_FindBCLen = %len(%trimr(W2SCAN))

      /Free
       wk_GDGRP# = *Zero;
       wk_GDTURN = *Zero;
       wk_GDBLTO = *Zero;
       wk_GDSHTO = *Zero;
       wk_GDDSHP = *Zero;
       wk_GDSSCCBC = *Blanks;
       wk_RcdCnt = *Zero;
       SELECT;
       WHEN wk_FindBCLen <= 10;
         // For totes, look for the associated shipment, using the first
         // shipment found.
         //   1) If the screen is currently subsetted by shipping lane,
         //      find the most recent open shipment group for the tote
         //      where it has been associated with that shipping lane.
         //   2) If the screen is not currently subsetted by shipping lane
         //      (or it is subsetted by shipping lane but the tote was not
         //      found associated with the shipping lane), find the most
         //      recent open shipment group for the tote (regardless of
         //      shipping lane).
         //   3) If, after steps 1 and 2, the shipment group has not yet
         //      been identified, find the most recent shipment group
         //      for the tote (regardless of status and shipping lane).
         IF F4DVRT > *Zero;
           EXEC SQL select coalesce(max(GDGRP#),0), count(*)
                     into :wk_GDGRP#, :wk_RcdCnt
                    from HYPSGDT a
                    where a.GDTOTE=:W2SCAN and a.GDDVRT=:F4DVRT and
                     exists (select * from HYPSGCU b where
                     b.GCGRP#=a.GDGRP# and b.GCBLTO=a.GDBLTO and
                     b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP and
                     b.GCSTS='O') and
                     not exists (select * from HYPSGTC c where
                     c.TCGRP#=a.GDGRP# and c.TCBLTO=a.GDBLTO and
                     c.TCSHTO=a.GDSHTO and c.TCDSHP=a.GDDSHP and
                     c.TCTOTE=a.GDTOTE);
         ENDIF;                                                              // F4DVRT=*Zero
         IF wk_RcdCnt = *Zero;
           EXEC SQL select coalesce(max(GDGRP#),0), count(*)
                     into :wk_GDGRP#, :wk_RcdCnt
                    from HYPSGDT a
                    where a.GDTOTE=:W2SCAN and
                     exists (select * from HYPSGCU b where
                     b.GCGRP#=a.GDGRP# and b.GCBLTO=a.GDBLTO and
                     b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP and
                     b.GCSTS='O') and
                     not exists (select * from HYPSGTC c where
                     c.TCGRP#=a.GDGRP# and c.TCBLTO=a.GDBLTO and
                     c.TCSHTO=a.GDSHTO and c.TCDSHP=a.GDDSHP and
                     c.TCTOTE=a.GDTOTE);
           IF wk_RcdCnt = *Zero;
             EXEC SQL select coalesce(max(GDGRP#),0), count(*)
                       into :wk_GDGRP#, :wk_RcdCnt
                      from HYPSGDT a
                      where a.GDTOTE=:W2SCAN;
           ENDIF;                                                            // wk_RcdCnt=*Zero
         ENDIF;                                                              // wk_RcdCnt=*Zero
         IF wk_RcdCnt > *Zero;
       //  EXEC SQL select max(GDBLTO)
       //            into :wk_GDBLTO
       //           from HYPSGDT
       //           where GDTOTE=:W2SCAN and GDGRP#=:wk_GDGRP#;
       //  EXEC SQL select max(GDSHTO)
       //            into :wk_GDSHTO
       //           from HYPSGDT
       //           where GDTOTE=:W2SCAN and GDGRP#=:wk_GDGRP# and
       //            GDBLTO=:wk_GDBLTO;
       //  EXEC SQL select max(GDDSHP)
       //            into :wk_GDDSHP
       //           from HYPSGDT
       //           where GDTOTE=:W2SCAN and GDGRP#=:wk_GDGRP# and
       //            GDBLTO=:wk_GDBLTO and GDSHTO=:wk_GDSHTO;
           EXEC SQL select max(GDTURN)
                     into :wk_GDTURN
                    from HYPSGDT
                    where GDTOTE=:W2SCAN and GDGRP#=:wk_GDGRP#;
           EXEC SQL select GDBLTO, GDSHTO, GDDSHP
                     into :wk_GDBLTO, :wk_GDSHTO, :wk_GDDSHP
                    from HYPSGDT
                    where GDTOTE=:W2SCAN and GDGRP#=:wk_GDGRP# and
                     GDTURN=:wk_GDTURN
                    fetch first row only;
         ELSE;
           IF not wk_Error;
             W2MSG = 'Tote number not found in active shipments.';
             wk_Error = *ON;
           ENDIF;                                                            // not wk_Error
           *IN41 = *ON;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       WHEN wk_FindBCLen = 20;
         EXEC SQL select GDGRP#, GDBLTO, GDSHTO, GDDSHP,
                         GDSSCCBC
                   into :wk_GDGRP#, :wk_GDBLTO, :wk_GDSHTO, :wk_GDDSHP,
                        :wk_GDSSCCBC
                  from HYPSGDT
                  where GDSSCCBC=:W2SCAN;
         IF wk_GDSSCCBC <> W2SCAN;
           IF not wk_Error;
             W2MSG = 'Carton barcode not found in active shipments.';
             wk_Error = *ON;
           ENDIF;                                                            // not wk_Error
           *IN41 = *ON;
         ENDIF;                                                              // wk_GDSSCCBC=W2SCAN
       OTHER;
         IF not wk_Error;
           W2MSG = 'Carton barcode or tote number is invalid.';
           wk_Error = *ON;
         ENDIF;                                                              // not wk_Error
         *IN41 = *ON;
       ENDSL;
      /End-Free

     C                   IF        wk_Error
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize tote number prompt window fields.                           *
      **************************************************************************
     CSR   $InitIndW2    BEGSR

     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN90 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check for multiple customer shipment totes.  If found, prompt user to  *
      * select which customer shipment to process.                             *
      **************************************************************************
     CSR   $ChkMultCS    BEGSR

     C                   EVAL      wk_MultCSFnd = *OFF

      *
      * If barcode scan (or entry) is empty, leave subroutine.
      *
     C                   IF        W2SCAN = *Blanks
     C                   LEAVESR
     C                   ENDIF                                                  W2SCAN=*Blanks

     C                   EVAL      W2SCAN = %triml(W2SCAN)
     C                   EVAL      wk_FindBCLen = %len(%trimr(W2SCAN))

      *
      * If barcode scan is more than 10 characters, it can't be a tote.
      * Leave subroutine.
      *
     C                   IF        wk_FindBCLen > 10
     C                   LEAVESR
     C                   ENDIF                                                  wk_FIndBCLen>10

      *
      * Determine count of open customer shipments associated with the tote.
      *
      /Free
       wk_CustCnt = *Zero;
       EXEC SQL select count(distinct digits(a.GDGRP#) || digits(a.GDBLTO) ||
                                      digits(a.GDSHTO) || digits(a.GDDSHP))
                 into :wk_CustCnt
                from HYPSGDT a
                where a.GDTOTE=:W2SCAN and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP# and
                 b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP
                 and b.GCSTS='O');
      /End-Free

      *
      * If multiple open customer shipments are associated with the tote,
      * prompt user to select which customer shipment to process.
      *
     C                   IF        wk_CustCnt > 1
     C                   EXSR      $BldSFL03A
     C                   EVAL      C1HDG3 = 'Shipment Group Selection - Tote ' +
     C                               W2SCAN
     C                   EVAL      C1HDG3 = center(C1HDG3)
     C                   EXSR      $SelectCS
     C                   IF        not *IN12
     C                   EVAL      wk_GDGRP# = wk_SelGrp#
     C                   EVAL      wk_GDBLTO = wk_SelBLTO
     C                   EVAL      wk_GDSHTO = wk_SelSHTO
     C                   EVAL      wk_GDDSHP = wk_SelDSHP
     C                   EVAL      wk_MultCSFnd = *ON
     C                   ENDIF                                                  not *IN12
     C                   ENDIF                                                  wk_CustCnt>1

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Select customer shipment.                                              *
      **************************************************************************
     CSR   $SelectCS     BEGSR

     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'MSG0447'
     C                   EVAL      wk_MsgData = W2SCAN
     C                   EXSR      $SndPM

     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN29 = *OFF

     C                   DOU       *IN12
     C                   WRITE     MSFCTL01A
     C                   EXFMT     SFLCTL03A
     C                   EVAL      *IN93 = *ON
     C                   EXSR      $RMVPM
     C                   EVAL      *IN40 = *OFF
     C                   SELECT
     C                   WHEN      *IN12
     C                   LEAVE
     C                   OTHER
     C                   EXSR      $EditSFL03A
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error
     C                   LEAVE
     C                   ENDSL
     C                   ENDDO                                                  *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit SFL03A - Shipment selection subfile.                              *
      **************************************************************************
     CSR   $EditSFL03A   BEGSR

     C                   EVAL      wk_IN25 = *IN25
     C                   EVAL      wk_Error = *OFF
     C                   EVAL      wk_SelPr = *OFF

     C                   READC     SFL03A

     C                   DOW       not %eof(HYD0600A)

      *
      * Reset error indicators and turn on SFLNXTCHG indicator.
      *
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF

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
     C                   IF        not wk_SelPr
     C                   EVAL      wk_SelPr = *ON
     C                   EVAL      wk_SelGrp# = S1GRP#
     C                   EVAL      wk_SelBLTO = S1BLTO
     C                   EVAL      wk_SelSHTO = S1SHTO
     C                   EVAL      wk_SelDSHP = S1DSHP
     C                   ELSE
     C                   EVAL      *IN40 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'GUM0008'
     C                   EXSR      $SndPM
     C                   IF        not wk_Error
     C                   EVAL      S@RCNO = wk_Sfl01RRN
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   ENDIF                                                  not wk_SelPr
     C                   UPDATE    SFL03A

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
     C                   UPDATE    SFL03A
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0001'
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON

     C                   ELSE

      *
      * Ignore Blank Options, Turn off Error & SFLNXTCHG
      *
     C                   EVAL      *IN28 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   UPDATE    SFL03A

     C                   ENDIF                                                   S1OPT<>*Blanks

     C                   ENDIF                                                   S1OPT=@SEL or

      *
      * Get next changed subfile record.
      *
     C                   READC     SFL03A

     C                   ENDDO                                                  not %eof(HYD0600A)

      *
      * If a selection was not made, send message for user to make one.
      *
     C                   IF        not wk_SelPr
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0040'
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_SelPr

      *
      * Turn off error indicator.
      *
     C                   EVAL      *IN41 = *OFF

      *
      * Restore function key indicators.
      *
     C                   EVAL      *IN25 = wk_IN25

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Build SFL03A - Shipment selection subfile.                             *
      **************************************************************************
     CSR   $BldSFL03A    BEGSR

     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)

     C                   EVAL      *IN29 = *ON
     C                   WRITE     SFLCTL03A
     C                   EVAL      *IN29 = *OFF
     C                   EVAL      wk_Sfl03RRN = *Zero

      /Free
       wk_CSList = *Blanks;
       EXEC SQL select listagg(distinct digits(a.GDGRP#) || digits(a.GDBLTO) ||
                                        digits(a.GDSHTO) || digits(a.GDDSHP))
                 into :wk_CSList
                from HYPSGDT a
                where a.GDTOTE=:W2SCAN and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP# and
                 b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP
                 and b.GCSTS='O');
      /End-Free

     C                   FOR       wk_Ptr = 1 to 871 by 30

     C                   EVAL      ds_CS = %subst(wk_CSList:wk_Ptr:30)

     C                   IF        ds_CS = *Blanks
     C                   IF        wk_Ptr > 1
     C                   LEAVE
     C                   ELSE                                                   Something is wrong!
     C                   EVAL      ds_CS = *Zeros
     C                   ENDIF                                                  wk_Ptr>1
     C                   ENDIF                                                  ds_CS=*Blanks

     C     key_GC2       CHAIN(N)  HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   CLEAR                   HYRSGCU
     C                   ENDIF                                                  not %found(HYPSGCU)

     C                   EVAL      S1OPT = *Blanks
     C                   EVAL      S1GRP# = GCGRP#
     C                   EVAL      S1CPTY = GCCPTY
     C                   EVAL      S1BLTO = GCBLTO
     C                   EVAL      S1BTNM = GCBTNM
     C                   EVAL      S1SHTO = GCSHTO
     C                   EVAL      S1DSHP = GCDSHP
     C                   EVAL      S1STNM = GCSTNM
     C                   EVAL      S1STNMX = GCSTNM
     C                   EVAL      S1STZP = GCSTZP
     C                   EVAL      S1SHPV = GCSHPV
     C                   EVAL      S1SVDS = GCSVDS
     C                   EVAL      S1FRZN = GCFRZN
     C                   EVAL      S1ITCT = GCITCT
     C                   EVAL      S1SHWT = GCSHWT
     C                   EVAL      S1TSTP1 = GCTSTP1
     C                   IF        GCTSTP2 = *Loval
     C                   EVAL      S1COMP = *Blank
     C                   ELSE
     C                   EVAL      S1COMP = 'Y'
     C                   ENDIF                                                  GCTSTP1=*Loval

      /Free
       wk_PackLane = *Blanks;
       EXEC SQL select listagg(distinct GDDVRT,',')
                 within group(order by GDDVRT)
                 into :wk_PackLane
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and GDDVRT<>0 and
                      GDQOPK>0;

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_Rcdcnt
                from HYPSGDT
                where GDGRP#=:GCGRP# and GDBLTO=:GCBLTO and
                      GDSHTO=:GCSHTO and GDDSHP=:GCDSHP and GDCNVY='P' and
                      GDQOPK>0;

       IF wk_RcdCnt > *Zero;
         IF wk_PackLane = *Blanks;
           wk_PackLane = 'P';
         ELSE;
           wk_PackLane = %trimr(wk_PackLane) + ',P';
         ENDIF;                                                              // wk_PackLane=*Blanks
       ENDIF;                                                                // wk_RcdCnt>*Zero

       S1LANE = wk_PackLane;
      /End-Free

     C                   EVAL      wk_Sfl03RRN = wk_Sfl03RRN + 1
     C                   WRITE     SFL03A

     C                   ENDFOR                                                 wk_Ptr=1 to 871 by

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Tote verify.                                                           *
      **************************************************************************
     CSR   $ToteVfy      BEGSR

     C                   EXSR      $InitIndW4

     C                   EVAL      W4SCAN = *Blanks
     C                   EVAL      W4MSG = *Blanks

     C                   DOU       *IN12

     C                   EXFMT     WINDOW04

     C                   EVAL      W4MSG = *Blanks
     C                   EXSR      $InitIndW4

     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

     C                   EXSR      $EditW4
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
                where a.GDTOTE=:W4SCAN and
                 exists (select * from HYPSGCU b where b.GCGRP#=a.GDGRP# and
                 b.GCBLTO=a.GDBLTO and b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP
                 and b.GCSTS='O') and
                 not exists (select * from HYPSGTC c where c.TCGRP#=a.GDGRP#
                 and c.TCBLTO=a.GDBLTO and c.TCSHTO=a.GDSHTO and
                 c.TCDSHP=a.GDDSHP and c.TCTOTE=a.GDTOTE);
      /End-Free

     C                   EVAL      wk_Pgm06 = *ON

     C                   IF        wk_CustCnt = 1
     C                   CALLP(E)  HYR0608 (MaintMode:wk_GDGRP#:wk_GDBLTO:
     C                               wk_GDSHTO:wk_GDDSHP:W4SCAN:wk_RC0608)
     C                   ELSE
     C                   CALLP(E)  HYR0608 (MaintMode2:*Zero:*Zero:
     C                               *Zero:*Zero:W4SCAN:wk_RC0608)
     C                   ENDIF                                                  wk_CustCnt=1

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0608'
     C                   EXSR      $SNDPM
     C                   WHEN      wk_RC0608 = '12'
     C                   WHEN      wk_RC0608 = *Zeros
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0602'
     C                   EXSR      $SNDPM
     C                   ENDSL

      /Free
       wk_GCCPTY = *Zero;
       wk_GCSTNM = *Blanks;
       EXEC SQL select GCCPTY, GCSTNM into :wk_GCCPTY, :wk_GCSTNM
                from HYPSGCU
                where GCGRP#=:wk_GDGRP# and GCBLTO=:wk_GDBLTO and
                 GCSHTO=:wk_GDSHTO and GCDSHP=:wk_GDDSHP;
      /End-Free
     C                   EVAL      @UGRP# = wk_GDGRP#
     C                   EVAL      @UCPTY = wk_GCCPTY
     C                   EVAL      @UBLTO = wk_GDBLTO
     C                   EVAL      @USHTO = wk_GDSHTO
     C                   EVAL      @UDSHP = wk_GDDSHP
     C                   EVAL      @USTNM = wk_GCSTNM
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit tote verify prompt window fields.                                 *
      **************************************************************************
     CSR   $EditW4       BEGSR

     C                   EVAL      wk_Error = *OFF

     C                   DO

      *
      * Tote ID scan (or entry) is required.
      *
     C                   IF        W4SCAN = *Blanks
     C                   IF        not wk_Error
     C                   EVAL      W4MSG = 'Tote number is required.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   LEAVE
     C                   ENDIF                                                  W4SCAN=*Blanks

     C                   IF        W4SCAN = '*MULT'
     C                               or  %subst(W4SCAN:1:3) = 'PLT'
     C                   EVAL      W4MSG = 'Tote no. not found in active +
     C                               shipments.'
     C                   EVAL      wk_Error = *ON
     C                   EVAL      *IN41 = *ON
     C                   LEAVE
     C                   ENDIF                                                  W4SCAN='*TOTE'

     C                   EVAL      W4SCAN = %triml(W4SCAN)

      /Free
       wk_GDGRP# = *Zero;
       wk_GDBLTO = *Zero;
       wk_GDSHTO = *Zero;
       wk_GDDSHP = *Zero;
       wk_GDSSCCBC = *Blanks;
       wk_RcdCnt = *Zero;
         // Look for the associated shipment, using the first shipment found.
         //   1) If the screen is currently subsetted by shipping lane,
         //      find the most recent open shipment group for the tote
         //      where it has been associated with that shipping lane.
         //   2) If the screen is not currently subsetted by shipping lane
         //      (or it is subsetted by shipping lane but the tote was not
         //      found associated with the shipping lane), find the most
         //      recent open shipment group for the tote (regardless of
         //      shipping lane).
         IF F4DVRT > *Zero;
           EXEC SQL select coalesce(max(GDGRP#),0), count(*)
                     into :wk_GDGRP#, :wk_RcdCnt
                    from HYPSGDT a
                    where a.GDTOTE=:W4SCAN and a.GDDVRT=:F4DVRT and
                     exists (select * from HYPSGCU b where
                     b.GCGRP#=a.GDGRP# and b.GCBLTO=a.GDBLTO and
                     b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP and
                     b.GCSTS='O') and
                     not exists (select * from HYPSGTC c where
                     c.TCGRP#=a.GDGRP# and c.TCBLTO=a.GDBLTO and
                     c.TCSHTO=a.GDSHTO and c.TCDSHP=a.GDDSHP and
                     c.TCTOTE=a.GDTOTE);
         ENDIF;                                                              // F4DVRT=*Zero
         IF wk_RcdCnt = *Zero;
           EXEC SQL select coalesce(max(GDGRP#),0), count(*)
                     into :wk_GDGRP#, :wk_RcdCnt
                    from HYPSGDT a
                    where a.GDTOTE=:W4SCAN and
                     exists (select * from HYPSGCU b where
                     b.GCGRP#=a.GDGRP# and b.GCBLTO=a.GDBLTO and
                     b.GCSHTO=a.GDSHTO and b.GCDSHP=a.GDDSHP and
                     b.GCSTS='O') and
                     not exists (select * from HYPSGTC c where
                     c.TCGRP#=a.GDGRP# and c.TCBLTO=a.GDBLTO and
                     c.TCSHTO=a.GDSHTO and c.TCDSHP=a.GDDSHP and
                     c.TCTOTE=a.GDTOTE);
         ENDIF;                                                              // wk_RcdCnt=*Zero
         IF wk_RcdCnt > *Zero;
           EXEC SQL select max(GDBLTO)
                     into :wk_GDBLTO
                    from HYPSGDT
                    where GDTOTE=:W4SCAN and GDGRP#=:wk_GDGRP#;
           EXEC SQL select max(GDSHTO)
                     into :wk_GDSHTO
                    from HYPSGDT
                    where GDTOTE=:W4SCAN and GDGRP#=:wk_GDGRP# and
                     GDBLTO=:wk_GDBLTO;
           EXEC SQL select max(GDDSHP)
                     into :wk_GDDSHP
                    from HYPSGDT
                    where GDTOTE=:W4SCAN and GDGRP#=:wk_GDGRP# and
                     GDBLTO=:wk_GDBLTO and GDSHTO=:wk_GDSHTO;
         ELSE;
           IF not wk_Error;
             W4MSG = 'Tote no. not found in active shipments.';
             wk_Error = *ON;
           ENDIF;                                                            // not wk_Error
           *IN41 = *ON;
         ENDIF;                                                              // wk_RcdCnt>*Zero
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
     CSR   $InitIndW4    BEGSR

     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN90 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Show current or history shipments toggle.                              *
      **************************************************************************
     CSR   $CurrHist     BEGSR

     C                   EXSR      $InitIndW1

     C                   IF        not wk_History
     C                   EVAL      W1HIST = *Zero
     C                   EVAL      W1FRDT1 = *Zero
     C                   EVAL      W1TODT2 = *Zero
     C                   ENDIF                                                  not wk_History
     C                   EVAL      W1MSG = *Blanks

     C                   DOU       *IN12

     C                   EXFMT     WINDOW01

     C                   EVAL      W1MSG = *Blanks
     C                   EXSR      $InitIndW1

     C                   IF        *IN12
     C                   EVAL      *IN11 = *OFF
     C                   EVAL      wk_MsgID = 'GUM0019'
     C                   EVAL      wk_MsgData = 'F12'
     C                   EXSR      $SndPM
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

     C                   EXSR      $EditW1
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   LEAVE

     C                   ENDDO                                                  *IN12

     C                   SELECT
     C                   WHEN      W1HIST = 1                                   Yesterday only
     C****               IF        not %open(HYLSGCU11)
     C****               OPEN      HYLSGCU11
     C****               ENDIF                                                  not %open(HYLSGCU11)
     C**** *Hival        SETGT     HYLSGCU11
     C****               READP     HYLSGCU11
     C****               IF        not %eof(HYLSGCU11)
     C****               EVAL      wk_HistCutoff = %date(GCTSTP1)
     C****               ELSE
     C****               EVAL      wk_HistCutoff = %date - %days(1)
     C****               ENDIF                                                  not %eof(HYLSGCU11)
      /Free
       wk_GCTSTP2 = *Loval;
       EXEC SQL select max(GCTSTP2) into :wk_GCTSTP2
                from HYPSGCU
                where GCSTS='H';
       IF wk_GCTSTP2 <> *Loval;
         wk_HistCutoff = %date(wk_GCTSTP2);
       ELSE;
         wk_HistCutoff = %date - %days(1);
       ENDIF;                                                                // wk_GCTSTP2<>*Loval
      /End-Free
     C                   EVAL      W1FRDT1D = *Loval
     C                   EVAL      W1TODT2D = *Loval
     C                   WHEN      W1HIST = 2                                   Last 7 days
     C                   EVAL      wk_HistCutoff = %date - %days(7)
     C                   EVAL      W1FRDT1D = *Loval
     C                   EVAL      W1TODT2D = *Loval
     C                   WHEN      W1HIST = 3                                   Date range
     C                   EVAL      wk_HistCutoff = *Loval
     C                   EVAL      W1FRDT1D = %date(W1FRDT1:*MDY)
     C                   EVAL      W1TODT2D = %date(W1TODT2:*MDY)
     C                   ENDSL

     C                   EVAL      wk_History = *ON
     C                   EVAL      C1HDG2 = HistShips
     C                   EVAL      C1HDG2 = center(C1HDG2)
     C                   EVAL      wk_AlwF7 = *OFF
     C                   EVAL      wk_AlwF9 = *OFF

     C                   EVAL      *IN11 = *OFF

     C                   EXSR      $FuncKey

     C                   SELECT
     C                   WHEN      wk_Sort = '1'
     C                   IF        not %open(HYLSGCU11)
     C                   OPEN      HYLSGCU11
     C                   ENDIF                                                  not %open(HYLSGCU11)
     C                   WHEN      wk_Sort = '2'
     C                   IF        not %open(HYLSGCU12)
     C                   OPEN      HYLSGCU12
     C                   ENDIF                                                  not %open(HYLSGCU12)
     C                   WHEN      wk_Sort = '3'
     C                   IF        not %open(HYLSGCU13)
     C                   OPEN      HYLSGCU13
     C                   ENDIF                                                  not %open(HYLSGCU13)
     C                   WHEN      wk_Sort = '4'
     C                   IF        not %open(HYLSGCU14)
     C                   OPEN      HYLSGCU14
     C                   ENDIF                                                  not %open(HYLSGCU14)
     C                   ENDSL

     C                   EVAL      @UGRP# = *Loval
     C                   EVAL      @UCPTY = *Loval
     C                   EVAL      @UBLTO = *Loval
     C                   EVAL      @USHTO = *Loval
     C                   EVAL      @UDSHP = *Loval
     C                   EVAL      @USTNM = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit history prompt window fields.                                     *
      **************************************************************************
     CSR   $EditW1       BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * History view is required.
      *
     C                   IF        W1HIST = *Zero
     C                   IF        not wk_Error
     C                   EVAL      W1MSG = 'History view is required.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   ENDIF                                                  W1HIST=*Zero

      *
      * History date range is required if "Date range" selected for History
      * view, is not allowed if "Date range" not selected for History view,
      * and must be valid if non-zero.
      *
     C                   IF        W1HIST = 3
     C                   IF        W1FRDT1 = *Zero  or  W1TODT2 = *Zero
     C                   IF        not wk_Error
     C                   EVAL      W1MSG = 'History date range is required.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN42 = *ON
     C                   EVAL      *IN43 = *ON
     C                   ENDIF                                                  W1FRDT1=*Zero or
     C                   IF        W1FRDT1 <> *Zero
     C     *MDY          TEST(DE)                W1FRDT1
     C                   IF        %error
     C                   IF        not wk_Error
     C                   EVAL      W1MSG = 'From history date is invalid.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN42 = *ON
     C                   ENDIF                                                  %error
     C                   ENDIF                                                  W1FRDT1<>*Zero
     C                   IF        W1TODT2 <> *Zero
     C     *MDY          TEST(DE)                W1TODT2
     C                   IF        %error
     C                   IF        not wk_Error
     C                   EVAL      W1MSG = 'Thru history date is invalid.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN43 = *ON
     C                   ENDIF                                                  %error
     C                   ENDIF                                                  W1TODT2<>*Zero
     C                   IF        W1TODT2 <> *Zero  and  W1FRDT1 <> *Zero  and
     C                               not *IN42  and  not *IN43
     C                   IF        %date(W1TODT2:*MDY) < %date(W1FRDT1:*MDY)
     C                   IF        not wk_Error
     C                   EVAL      W1MSG = 'History date range is invalid.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN42 = *ON
     C                   EVAL      *IN43 = *ON
     C                   ENDIF                                                  %date(W1TODT2:*MDY)<
     C                   ENDIF                                                  W1FRDT1<>*Zero and
     C                   ELSE
     C                   IF        W1FRDT1 <> *Zero  or  W1TODT2 <> *Zero
     C                   IF        not wk_Error
     C                   EVAL      W1MSG = 'History date range is not allowed.'
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   EVAL      *IN42 = *ON
     C                   EVAL      *IN43 = *ON
     C                   ENDIF                                                  W1FRDT1=*Zero or
     C                   ENDIF                                                  W1HIST=3

     C                   IF        wk_Error
     C                   EVAL      *IN90 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize history prompt window fields.                               *
      **************************************************************************
     CSR   $InitIndW1    BEGSR

     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN90 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Leave history-shipment mode (back to currrent-data mode).              *
      **************************************************************************
     CSR   $LeaveHist    BEGSR

     C                   EVAL      wk_History = *OFF
     C                   IF        wk_Completed
     C                   EVAL      C1HDG2 = TodaysShips
     C                   ELSE
     C                   EVAL      C1HDG2 = OpenShips
     C                   ENDIF                                                  wk_Commpleted
     C                   EVAL      C1HDG2 = center(C1HDG2)
     C                   EVAL      wk_AlwF7 = *ON
     C                   IF        wk_PgmMode = MaintMode
     C                   IF        wk_ToteVfy
     C                   EVAL      wk_ALwF9 = *ON
     C                   ENDIF                                                  wk_ToteVfy
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   EVAL      *IN12 = *OFF

     C                   EXSR      $FuncKey

     C                   SELECT
     C                   WHEN      wk_Sort = '1'
     C                   IF        not %open(HYLSGCU1)
     C                   OPEN      HYLSGCU1
     C                   ENDIF                                                  not %open(HYLSGCU1)
     C                   WHEN      wk_Sort = '2'
     C                   IF        not %open(HYLSGCU2)
     C                   OPEN      HYLSGCU2
     C                   ENDIF                                                  not %open(HYLSGCU2)
     C                   WHEN      wk_Sort = '3'
     C                   IF        not %open(HYLSGCU3)
     C                   OPEN      HYLSGCU3
     C                   ENDIF                                                  not %open(HYLSGCU3)
     C                   WHEN      wk_Sort = '4'
     C                   IF        not %open(HYLSGCU4)
     C                   OPEN      HYLSGCU4
     C                   ENDIF                                                  not %open(HYLSGCU4)
     C                   ENDSL

     C                   EVAL      @UGRP# = *Loval
     C                   EVAL      @UCPTY = *Loval
     C                   EVAL      @UBLTO = *Loval
     C                   EVAL      @USHTO = *Loval
     C                   EVAL      @UDSHP = *Loval
     C                   EVAL      @USTNM = *Loval
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageDownSf1
     C                   EVAL      *IN05 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display subfile SFL01 from the top.                                    *
      **************************************************************************
     CSR   $Top1         BEGSR

     C                   EVAL      @UGRP# = *Loval
     C                   EVAL      @UCPTY = *Loval
     C                   EVAL      @UBLTO = *Loval
     C                   EVAL      @USHTO = *Loval
     C                   EVAL      @UDSHP = *Loval
     C                   EVAL      @USTNM = *Loval
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

     C                   EVAL      @UGRP# = *Hival
     C                   EVAL      @UCPTY = *Hival
     C                   EVAL      @UBLTO = *Hival
     C                   EVAL      @USHTO = *Hival
     C                   EVAL      @UDSHP = *Hival
     C                   EVAL      @USTNM = *Hival
     C                   EVAL      *IN05 = *ON
     C                   EXSR      $PageUpSf1
     C                   EVAL      S@MORE = 'Bottom'

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Change customer shipment.                                              *
      **************************************************************************
     CSR   $Change       BEGSR

      *
      * Retrieve whether freight and special charges amounts can be changed
      * at shipping.
      *
      /Free
       IN wk_CCFrtAlw;

       SELECT;
       WHEN wk_CCFrtAlw = '*ALL'  or  wk_CCFrtAlw = ps_Job;
         *IN82 = *OFF;                                                       // Alw frt chrg chng
         *IN83 = *OFF;                                                       // Alw spcl chrg chng
       WHEN wk_CCFrtAlw = '*NONE'  or  wk_CCFrtAlw = *Blanks  or
            (wk_CCFrtAlw <> *Blanks  and  wk_CCFRTAlw <> ps_Job);
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGDT a
                  where a.GDGRP#=:S1GRP# and a.GDBLTO=:S1BLTO and
                   a.GDSHTO=:S1SHTO and a.GDDSHP=:S1DSHP and
                   exists (select * from OEORHD b where b.OEORD#=a.GDORD# and
                   b.OECTRM in ('CC','CP','EC'));
         IF wk_RcdCnt = *Zero;
           *IN82 = *OFF;                                                     // Alw frt chrg chng
           *IN83 = *OFF;                                                     // Alw spcl chrg chng
         ELSE;
           *IN82 = *ON;                                                      // No frt chrg chng
           *IN83 = *ON;                                                      // No spcl chrg chng
         ENDIF;                                                              // wk_RcdCnt=*Zero
       OTHER;
         *IN82 = *OFF;                                                       // Alw frt chrg chng
         *IN83 = *OFF;                                                       // Alw spcl chrg chng
       ENDSL;
      /End-Free

      *
      * Retrieve customer shipment record.
      *
     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)
     C     key_GC        CHAIN(N)  HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   CLEAR                   HYRSGCU
     C                   ENDIF                                                  not %found(HYPSGCU)

      *
      * Save changable fiekds.
      *
     C                   EVAL      sv_SHPV = GCSHPV
     C                   EVAL      sv_PRO# = GCPRO#
     C                   EVAL      sv_FRTA = GCFRTA
     C                   EVAL      sv_CHGA = GCCHGA

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
      * Prompt?
      *
     C                   IF        *IN04
     C                   EXSR      $List2
     C                   ITER
     C                   ENDIF                                                  *IN04

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
     C     key_GC        CHAIN(N)  HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   CLEAR                   HYRSGCU
     C                   ENDIF                                                  not %found(HYPSGCU)
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
      * Change customer shipment record.
      *

      *
      * No errors - continue.
      *
      * If no fields were changed, exit subroutine.
      *
     C                   IF        F2SHPV = sv_SHPV  and
     C                               F2PRO# = sv_PRO#  and
     C                               F2FRTA = sv_FRTA  and
     C                               F2CHGA = sv_CHGA
     C                   LEAVE
     C                   ENDIF                                                  F2SHPV=sv_SHPV and

      *
      * Change customer shipment record.
      *
      * Retrieve customer shipment record for update.
      *
     C     key_GC        CHAIN     HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYPSGCU'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %trimr(%editc(S1GRP#:'3')) + '/' +
     C                               %trimr(%editc(S1BLTO:'3')) + '/' +
     C                               %trimr(%editc(S1SHTO:'3')) + '/' +
     C                               %trimr(%editc(S1DSHP:'3'))
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Press F12 to cancel.'
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   ITER
     C                   ENDIF                                                  not %found(HYPSGCU)

      *
      * If customer shipment record has been changed by another user, abort this change.
      *
     C                   IF        GCSHPV <> sv_SHPV  or
     C                               GCPRO# <> sv_PRO#  or
     C                               GCFRTA <> sv_FRTA  or
     C                               GCCHGA <> sv_CHGA
     C                   EVAL      wk_MsgID = 'ERR1234'
     C                   EVAL      wk_MsgData = %triml(%editc(GCGRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GCBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GCSHTO:'Z'))
     C                   IF        GCDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GCDSHP:'Z'))
     C                   ENDIF                                                  GCDSHP=*Zero
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   UNLOCK    HYPSGCU
     C                   ITER
     C                   ENDIF                                                  GCSHPV<>sv_SHPV

     C                   IF        F2SHPV <> GCSHPV
      /Free
       EXEC SQL update HYPSGDT
                 set GDSHPV=:F2SHPV, GDSVDS=:F2SVDS
                 where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                       GDSHTO=:S1SHTO and GDDSHP=:S1DSHP;
      /End-Free
     C                   ENDIF                                                  F2SHPV<>GCSHPV

     C                   IF        F2PRO#<> GCPRO#
      /Free
       EXEC SQL update OEHDWK a
                 set H1UDF3=:F2PRO#
                 where exists (select * from HYPSGDT b where
                       b.GDGRP#=:S1GRP# and b.GDBLTO=:S1BLTO and
                       b.GDSHTO=:S1SHTO and b.GDDSHP=:S1DSHP and
                       b.GDOCTL=a.H1OCTL);
      /End-Free
     C                   ENDIF                                                  F2PRO#<>GCPRO#

     C                   EVAL      GCSHPV = F2SHPV
     C                   EVAL      GCSVDS = F2SVDS
     C                   EVAL      GCPRO# = F2PRO#
     C                   EVAL      GCFRTA = F2FRTA
     C                   EVAL      GCCHGA = F2CHGA

     C                   UPDATE    HYRSGCU

     C                   EVAL      wk_MsgID = 'MSG0371'
     C                   EVAL      wk_MsgData = %triml(%editc(GCGRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GCBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GCSHTO:'Z'))
     C                   IF        GCDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GCDSHP:'Z'))
     C                   ENDIF                                                  GCDSHP=*Zero
     C                   EXSR      $SndPM

     C                   EVAL      wk_Refresh = *ON

     C                   LEAVE

     C                   ENDDO                                                  *IN03 or *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit change customer group screen fields.                              *
      **************************************************************************
     CSR   $Edit2        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Ship via is required and must be valid.
      *
     C                   IF        F2SHPV = *Blanks
     C                   EVAL      wk_MsgID = 'ERR0136'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      F2SVDS = *Blanks
     C                   ELSE
      /Free
       wk_SVSVSV = *Blanks;
       wk_SVSVDS = *Blanks;
       wk_SVBOLR = *Blank;
       EXEC SQL select SVSVSV, SVSVDS, SVBOLR
                 into :wk_SVSVSV, :wk_SVSVDS , :wk_SVBOLR
                from HDSHPV
                where SVSVSV=:F2SHPV;
      /End-Free
     C                   IF        wk_SVSVSV <> F2SHPV
     C                   EVAL      wk_MsgID = 'ERR0137'
     C                   EVAL      wk_MsgData = F2SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      F2SVDS = *Blanks
     C                   ELSE
     C                   EVAL      F2SVDS = wk_SVSVDS
     C                   ENDIF                                                  wk_SVSVSV<>F2SHPV
     C                   ENDIF                                                  F2SHPV=*Blanks

      *
      * PRO number is allowed only for truck shipments.
      *
     C                   IF        F2PRO# <> *Blanks  and  F2PRO# <> sv_PRO#
     C                               and wk_SVBOLR <> 'Y'
     C                   EVAL      wk_MsgID = 'ERR1330'
     C                   EVAL      wk_MsgData = %triml(%editc(S1GRP#:'3'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(S1BLTO:'3'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(S1SHTO:'3'))
     C                   IF        S1DSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(S1DSHP:'3'))
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   EVAL      %subst(wk_MsgData:31:2) = F2SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN45 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F2PRO#<>*Blanks and

      *
      * Freight charge amount must be valid.
      *
     C                   IF        F2FRTA < *Zero
     C                   EVAL      wk_MsgID = 'ERR1235'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F2FRTA<*Zero

      *
      * Special charge amount must be valid.
      *
     C                   IF        F2CHGA < *Zero
     C                   EVAL      wk_MsgID = 'ERR1236'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F2CHGA<*Zero

      *
      * If error detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid values for change customer group screen fields.             *
      **************************************************************************
     CSR   $List2        BEGSR

     C                   SELECT

     C                   WHEN      S@IFLD = 'F2SHPV'
     C                   EVAL      wk_ShipVia = F2SHPV
     C                   EXSR      $ListShipVia
     C                   IF        wk_ValueRtnd
     C                   EVAL      F2SHPV = wk_ShipVia
     C                   ENDIF                                                  wk_ValueRtnd
     C                   EVAL      *IN41 = *ON

     C                   OTHER
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0013'
     C                   EXSR      $SndPM

     C                   SELECT
     C                   WHEN      S@IFLD = 'F2FRTA'
     C                   EVAL      *IN42 = *ON
     C                   WHEN      S@IFLD = 'F2CHGA'
     C                   EVAL      *IN43 = *ON
     C                   OTHER
     C                   EVAL      *IN41 = *ON
     C                   ENDSL

     C                   EVAL      *IN89 = *ON

     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move data to change customer group screen.                             *
      **************************************************************************
     CSR   $MoveToFmt2   BEGSR

      /Free
       wk_STName = *Blanks;
       EXEC SQL select CMCNA1 into :wk_STName
                from HDCUST
                where CMCUST=:S1SHTO;
       wk_DSName = *Blanks;
       IF S1DSHP <> *Zero;
         EXEC SQL select DSNAME into :wk_DSName
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:S1SHTO and DSNMBR=:S1DSHP;
       ENDIF;                                                                // S1DSHP<>*Zero
      /End-Free

     C                   EVAL      F2GRP#X = %triml(%editc(S1GRP#:'3'))
     C                   EVAL      F2CPTYX = %triml(%editc(S1CPTY:'3'))
     C                   EVAL      F2BLTOX = %triml(%editc(S1BLTO:'3')) + '-' +
     C                               S1BTNM
     C                   EVAL      F2SHTOX = %triml(%editc(S1SHTO:'3')) + '-' +
     C                               wk_STName
     C                   IF        S1DSHP = *Zero
     C                   EVAL      F2DSHPX = *Blanks
     C                   ELSE
     C                   EVAL      F2DSHPX = %triml(%editc(S1DSHP:'3')) + '-' +
     C                               wk_DSName
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   EVAL      F2STZP = S1STZP
     C                   SELECT
     C                   WHEN      GCSTYP = 'T'
     C                   EVAL      F2STYP = 'TRUCK'
     C                   WHEN      GCSTYP = 'P'
     C                   EVAL      F2STYP = 'PARCEL'
     C                   OTHER
     C                   EVAL      F2STYP = GCSTYP
     C                   ENDSL
     C                   EVAL      F2SHPV = GCSHPV
     C                   EVAL      F2SVDS = GCSVDS
     C                   EVAL      F2PRO# = GCPRO#
     C                   EVAL      F2FTRM = GCFTRM
     C                   EVAL      F2FRTA = GCFRTA
     C                   EVAL      F2CHGA = GCCHGA
     C                   EVAL      F2FRZN = GCFRZN
     C                   EVAL      F2ITCT = GCITCT
     C                   EVAL      F2SHWT = GCSHWT
     C                   EVAL      F2TSTP1 = GCTSTP1
     C                   IF        GCTSTP2 = *Loval
     C                   EVAL      F2TSTP2 = *Blanks
     C                   ELSE
     C                   EVAL      F2TSTP2 = %char(GCTSTP2)
     C                   ENDIF                                                  GCTSTP2=*Loval

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize change customer group screen indicators.                    *
      **************************************************************************
     CSR   $InitInd2     BEGSR

     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN45 = *OFF
     C                   EVAL      *IN89 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display customer shipment.                                             *
      **************************************************************************
     CSR   $Display      BEGSR

      *
      * Retrieve customer shipment record.
      *
     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)
     C     key_GC        CHAIN(N)  HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   CLEAR                   HYRSGCU
     C                   ENDIF                                                  not %found(HYPSGCU)

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
      * Move data to display customer shipment screen.                         *
      **************************************************************************
     CSR   $MoveToFmt3   BEGSR

      /Free
       wk_STName = *Blanks;
       EXEC SQL select CMCNA1 into :wk_STName
                from HDCUST
                where CMCUST=:S1SHTO;
       wk_DSName = *Blanks;
       IF S1DSHP <> *Zero;
         EXEC SQL select DSNAME into :wk_DSName
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:S1SHTO and DSNMBR=:S1DSHP;
       ENDIF;                                                                // S1DSHP<>*Zero
      /End-Free

     C                   EVAL      F3GRP#X = %triml(%editc(S1GRP#:'3'))
     C                   EVAL      F3CPTYX = %triml(%editc(S1CPTY:'3'))
     C                   EVAL      F3BLTOX = %triml(%editc(S1BLTO:'3')) + '-' +
     C                               S1BTNM
     C                   EVAL      F3SHTOX = %triml(%editc(S1SHTO:'3')) + '-' +
     C                               wk_STName
     C                   IF        S1DSHP = *Zero
     C                   EVAL      F3DSHPX = *Blanks
     C                   ELSE
     C                   EVAL      F3DSHPX = %triml(%editc(S1DSHP:'3')) + '-' +
     C                               wk_DSName
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   EVAL      F3STZP = S1STZP
     C                   SELECT
     C                   WHEN      GCSTYP = 'T'
     C                   EVAL      F3STYP = 'TRUCK'
     C                   WHEN      GCSTYP = 'P'
     C                   EVAL      F3STYP = 'PARCEL'
     C                   OTHER
     C                   EVAL      F3STYP = GCSTYP
     C                   ENDSL
     C                   EVAL      F3SHPV = GCSHPV
     C                   EVAL      F3SVDS = GCSVDS
     C                   EVAL      F3PRO# = GCPRO#
     C                   EVAL      F3FTRM = GCFTRM
     C                   EVAL      F3FRTA = GCFRTA
     C                   EVAL      F3CHGA = GCCHGA
     C                   EVAL      F3FRZN = GCFRZN
     C                   EVAL      F3ITCT = GCITCT
     C****               EVAL      F3SHWT = GCSHWT
     C                   EVAL      F3SHWT = RtvShipWgt (S1GRP#:S1BLTO:S1SHTO:
     C                                        S1DSHP:'P')
     C                   IF        GCSPPK = 'Y'
     C                   EVAL      F3SPPK = 'Yes'
     C                   ELSE
     C                   EVAL      F3SPPK = *Blanks
     C                   ENDIF                                                  GCSPPK='Y'
     C                   IF        GCPANS = 'Y'
     C                   EVAL      F3PANS = 'Yes'
     C                   ELSE
     C                   EVAL      F3PANS = *Blanks
     C                   ENDIF                                                  GCPANS='Y'
     C                   IF        GCPIKO = 'Y'
     C                   EVAL      F3PIKO = 'Yes'
     C                   ELSE
     C                   EVAL      F3PIKO = *Blanks
     C                   ENDIF                                                  GCPIKO='Y'
     C                   EVAL      F3TSTP1 = GCTSTP1
     C                   IF        GCTSTP2 = *Loval
     C                   EVAL      F3TSTP2 = *Blanks
     C                   ELSE
     C                   EVAL      F3TSTP2 = %char(GCTSTP2)
     C                   ENDIF                                                  GCTSTP2=*Loval

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shipment detail processing/inquiry.                                    *
      **************************************************************************
     CSR   $Detail       BEGSR

     C                   IF        wk_PgmMode = MaintMode
     C                               and  wk_EMPL = *Zero
     C                   EVAL      wk_W3SIZE = '1'
     C                   EXSR      $PromptEmply
     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   IF        wk_PgmMode = MaintMode
     C                   IF        ForceCmts (S1GRP#:S1BLTO:S1SHTO:S1DSHP)
     C                   EVAL      wk_CmtsForced = *ON
     C                   EXSR      $DspCmts
     C                   ENDIF                                                  ForceCmts(S1GRP#:
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   CALLP(E)  HYR0602 (wk_PgmMode:S1GRP#:S1BLTO:S1SHTO:
     C                               S1DSHP:wk_RC0602)

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0602'
     C                   EXSR      $SNDPM
     C                   WHEN      wk_RC0602 = '03'
     C                   EVAL      *IN03 = *ON
     C                   WHEN      wk_RC0602 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0602 = *Zeros
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0602'
     C                   EXSR      $SNDPM
     C                   ENDSL


     C                   IF        not *IN03
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
     C                   ENDIF                                                  not *IN03

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Palletize shipment.                                                    *
      **************************************************************************
     CSR   $Palletize    BEGSR

     C                   EVAL      %subst(wk_LDA:1:5) = 'SHTO:'
     C                   EVAL      %subst(wk_LDA:6:7) = %editc(S1SHTO:'X')

      /Free
       wk_GCSTST = *Blanks;
       wk_GCSTCT = *Blanks;
       EXEC SQL select GCSTST, GCSTCT into :wk_GCSTST, :wk_GCSTCT
                from HYPSGCU
                where GCGRP#=:S1GRP# and GCBLTO=:S1BLTO and
                      GCSHTO=:S1SHTO and GCDSHP=:S1DSHP;

       wk_CMCCLS = *Blanks;
       EXEC SQL select CMCCLS into :wk_CMCCLS
                from HDCUST
                where CMCUST=:S1BLTO;
      /End-Free

     C                   IF        RtvForeign(wk_CMCCLS:wk_GCSTCT)
     C                               or
     C                              (wk_GCSTST = 'AK'  or  wk_GCSTST = 'HI'  or Alaska & Hawaii
     C                               wk_GCSTST = 'AS'  or  wk_GCSTST = 'GU'  or US Territories
     C                               wk_GCSTST = 'MP'  or  wk_GCSTST = 'PR'  or
     C                               wk_GCSTST = 'VI')
     C                   EVAL      %subst(wk_LDA:14:10) = '*HAZLBL(Y)'
     C                   ELSE
     C                   EVAL      %subst(wk_LDA:14:10) = '*HAZLBL(N)'
     C                   ENDIF                                                  RtvForeign(wk_CMCCLS
     C                   EVAL      %subst(wk_LDA:26:9) = %editc(S1GRP#:'X')

     C                   OUT       wk_LDA

     C                   CALLP(E)  HYC0138 (wk_PgmMode)

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYC0138'
     C                   EXSR      $SNDPM
     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Complete customer shipment.                                            *
      **************************************************************************
     CSR   $CompShip     BEGSR

     C                   IF        ForceCmts (S1GRP#:S1BLTO:S1SHTO:S1DSHP)
     C                   EVAL      wk_CmtsForced = *ON
     C                   EXSR      $DspCmts
     C                   ENDIF                                                  ForceCmts(S1GRP#:

     C                   EVAL      ary_Ord# = *Zero
     C                   EVAL      idx1 = *Zero
     C                   EVAL      wk_XtraOrds = *Zero
     C                   EVAL      wk_LGNeeded = *OFF
     C                   EVAL      sv_ORD# = -1

     C                   IF        not %open(HYLSGDT3)
     C                   OPEN      HYLSGDT3
     C                   ENDIF                                                  not %open(HYLSGDT3)

     C                   EVAL      wk_IntrHold = *OFF

     C     key_GD        SETLL     HYLSGDT3
     C                   DOU       %eof(HYLSGDT3)
     C     key_GD        READE     HYLSGDT3
     C                   IF        %eof(HYLSGDT3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT3)
     C                   IF        GDORD# <> sv_ORD#
     C                   IF        not wk_IntrHold
     C                   IF        OnIntrHold (GDORD#:GDTURN)
     C                   EVAL      wk_IntrHold = *ON
     C                   ENDIF                                                  OnIntrHold (GDORD#:
     C                   ENDIF                                                  not wk_IntrHold
     C                   IF        idx1 < 100
     C                   EVAL      idx1 = idx1 +1
     C                   EVAL      ary_Ord#(idx1) = GDORD#
     C                   ELSE
     C                   EVAL      wk_XtraOrds = wk_XtraOrds + 1
     C                   ENDIF                                                  idx1<100
      /Free
       IF not wk_LGNeeded;
         wk_LGCnt = *Zero;
         EXEC SQL select count(*) into :wk_LGCnt
                  from HYPSCWK a
                   join HDIMST b on a.S1ITEM=b.IMITEM
                   join OEORHD c on a.S1ORD#=c.OEORD#
                  where a.S1ORD#=:GDORD# and a.S1TURN=:GDTURN and
                   b.IMIMWG>=150 and c.OEUCA1='LG';
         IF wk_LGCnt > *Zero;
           wk_LGNeeded = *ON;
         ENDIF;                                                              // wk_LGCnt>*Zero
       ENDIF;                                                                // not wk_LGNeeded
      /End-Free
     C                   EVAL      sv_ORD# = GDORD#
     C                   ENDIF                                                  GDORD#<>sv_ORD#
     C                   ENDDO                                                  %eof(HYLSGDT3)

     C                   IF        wk_XtraOrds > *Zero
     C                   EVAL      ary_Ord#(100) = wk_XtraOrds * -1
     C                   ENDIF                                                  wk_XtraOrds>*Zero

     C                   IF        wk_LGNeeded
     C                   EVAL      wk_XtraCmt = '01'
     C                   ELSE
     C                   EVAL      wk_XtraCmt = *Blanks
     C                   ENDIF                                                  wk_LGNeeded

     C                   CALLP(E)  HYR0520 ('03':S1SHTO:ary_Ord#:
     C                                      idx1:wk_XtraCmt:wk_RC0520)
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      wk_Pgm05 = *ON

     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0520'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   WHEN      wk_RC0520 = '03'
     C                   EVAL      *IN12 = *ON
     C                   LEAVESR
     C                   WHEN      wk_RC0520 = '12'
     C                   EVAL      *IN12 = *ON
     C                   LEAVESR
     C                   WHEN      wk_RC0520 <> *Zeros  and  wk_RC0520 <> '99'
     C                   EVAL      wk_MsgID = 'ERR9019'
     C                   EVAL      wk_MsgData = 'HYR0520   ' + wk_RC0520
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ENDSL

      *
      * Retrieve whether freight and special charges amounts can be changed
      * at shipping.
      *
      /Free
       IN wk_CCFrtAlw;

       SELECT;
       WHEN wk_CCFrtAlw = '*ALL'  or  wk_CCFrtAlw = ps_Job;
         *IN82 = *OFF;                                                       // Alw frt chrg chng
         *IN83 = *OFF;                                                       // Alw spcl chrg chng
       WHEN wk_CCFrtAlw = '*NONE'  or  wk_CCFrtAlw = *Blanks  or
            (wk_CCFrtAlw <> *Blanks  and  wk_CCFRTAlw <> ps_Job);
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGDT a
                  where a.GDGRP#=:S1GRP# and a.GDBLTO=:S1BLTO and
                   a.GDSHTO=:S1SHTO and a.GDDSHP=:S1DSHP and
                   exists (select * from OEORHD b where b.OEORD#=a.GDORD# and
                   b.OECTRM in ('CC','CP','EC'));
         IF wk_RcdCnt = *Zero;
           *IN82 = *OFF;                                                     // Alw frt chrg chng
           *IN83 = *OFF;                                                     // Alw spcl chrg chng
         ELSE;
           *IN82 = *ON;                                                      // No frt chrg chng
           *IN83 = *ON;                                                      // No spcl chrg chng
         ENDIF;                                                              // wk_RcdCnt=*Zero
       OTHER;
         *IN82 = *OFF;                                                       // Alw frt chrg chng
         *IN83 = *OFF;                                                       // Alw spcl chrg chng
       ENDSL;
      /End-Free

      *
      * Retrieve customer shipment record.
      *
     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)
     C     key_GC        CHAIN(N)  HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   CLEAR                   HYRSGCU
     C                   ENDIF                                                  not %found(HYPSGCU)

      *
      * Save changable fields.
      *
     C                   EVAL      sv_SHPV = GCSHPV
     C                   EVAL      sv_PRO# = GCPRO#
     C                   EVAL      sv_FRTA = GCFRTA
     C                   EVAL      sv_CHGA = GCCHGA
     C                   EVAL      wk_SSShipVia = *Blanks
     C                   EVAL      wk_SSPRO# = *Blanks
     C                   EVAL      wk_UpdPRO# = *Blanks

      *
      * Determine whether bulk ammo (aka, loose ammo) is included in the
      * shipment.
      *
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                      GDSHTO=:S1SHTO and GDDSHP=:S1DSHP and
                      exists (select * from HDIWHS b where
                      b.IWITEM=a.GDITEM and b.IWWHS=6 and
                      b.IWFTCL='AMMOB');
       IF wk_Rcdcnt > *Zero;
         wk_ShipAMMOB = *ON;
       ELSE;
         wk_ShipAMMOB = *OFF;
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

      *
      * Retrieve whether ShipSelect is available.
      *
     C                   IN        wk_BxlSSOnOff
     C                   IF        wk_BxlSSOnOff = 'Y'
     C                   EVAL      *IN81 = *ON
     C                   ELSE
     C                   EVAL      *IN81 = *OFF
     C                   ENDIF                                                  wk_BxlSSOnOff='Y'

      *
      * Populate screen fields.
      *
     C                   EXSR      $MoveToFmt5

      *
      * Initialize screen indicators.
      *
     C                   EXSR      $InitInd5
     C                   EVAL      *IN93 = *OFF

     C                   DOU       *IN03  or  *IN12

      *
      * Format Display Screen
      *
     C                   WRITE     MSFCTL01B
     C                   EXFMT     DSPFMT5A
     C                   MOVE      *ON           *IN93
     C                   EXSR      $InitInd5
     C                   EXSR      $RMVPM

      *
      * Exit?
      *
     C                   IF        *IN03
     C                   LEAVE
     C                   ENDIF                                                  *IN03

      *
      * Prompt?
      *
     C                   IF        *IN04
     C                   EXSR      $List5
     C                   ITER
     C                   ENDIF                                                  *IN04

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
     C     key_GC        CHAIN(N)  HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   CLEAR                   HYRSGCU
     C                   ENDIF                                                  not %found(HYPSGCU)
     C                   EXSR      $MoveToFmt5
     C                   ITER
     C                   ENDIF                                                  *IN05

      *
      * Enter pressed.  Edit screen data.
      *
     C                   EXSR      $Edit5
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * If F11 or F20 to complete shipment not pressed, send error.
      *
     C                   IF        not *IN11  and  not *IN20
     C                               and  not *IN21
     C                   EVAL      wk_MsgID = 'GUM0036'
     C                   EVAL      wk_MsgData = 'F11  complete the shipment'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  not *IN11 and

      *
      * Change customer shipment record and/or complete customer shipment.
      *

     C                   IF        F5SHPV <> wk_SSShipVia
     C                   EVAL      wk_UpdPRO# = *Blanks
     C                   ENDIF                                                  F5SHPV<>wk_SSShipVia

      *
      * No errors and F11 or F20 or F21 pressed - continue.
      *

      *
      * Call ShipSelect for eligible shipments.
      *
     C                   IF        *IN11  and  wk_BxlSSOnOff = 'Y'  and
     C                               not wk_IntrHold  and
     C                               F5SHPV <> 'PU'  and F5SHWT < 10000
     C                   IF        RtvForeign(wk_CMCCLS:wk_GCSTCT)
     C                               or
     C                              (GCSTST = 'AK'  or  GCSTST = 'HI'  or       Alaska & Hawaii
     C                               GCSTST = 'AS'  or  GCSTST = 'GU'  or       US Territories
     C                               GCSTST = 'MP'  or  GCSTST = 'PR'  or
     C                               GCSTST = 'VI')
     C                   EVAL      wk_CallSS = *OFF                             Foreign shipment
     C                   ELSE
      /Free
       wk_RcdCnt = *Zero;
       wk_MinSVSV = *Blanks;
       wk_MaxSVSV = *Blanks;
       EXEC SQL select count(*), coalesce(min(SVSVSV),' '),
                       coalesce(max(SVSVSV),' ')
                 into :wk_RcdCnt, :wk_MinSVSV,
                      :wk_MaxSVSV
                from HYPSGDT a join HYPSVCT b
                 on a.GDORD#=b.SVORD#
                where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                      GDSHTO=:S1SHTO and GDDSHP=:S1DSHP and
                      SVSTS='A';
      /End-Free
     C                   IF        wk_RcdCnt > *Zero  and
     C                               wk_MaxSVSV = wk_MinSVSV
     C                   EVAL      wk_CallSS = *OFF                             Sales specd ship-via
     C                   ELSE
     C                   EVAL      wk_CallSS = *ON
     C                   ENDIF                                                  wk_RcdCnt>*Zero and
     C                   ENDIF                                                  RtvForeign(wk_CMCCLS
     C                   ELSE
     C                   EVAL      wk_CallSS = *OFF
     C                   ENDIF                                                  *IN11 and
     C                   IF        wk_CallSS  and  F5SHPV <> *Blanks  and
     C                               F5SHPV = wk_SSShipVia  and
     C                               wk_UpdPRO# = wk_SSPRO#
     C                   EVAL      wk_CallSS = *OFF
     C                   ENDIF                                                  wk_CallSS and

     C                   IF        wk_CallSS

     C                   EVAL      *IN93 = *OFF

     C                   CALLP(E)  QCMDEXC ('CHGJOB STSMSG(*NORMAL)':22)

     C                   EVAL      wk_MsgID = 'MSG5014'
     C                   EVAL      wk_MsgData = %triml(%editc(S1SHTO:'Z'))
     C                   EXSR      $SndSM

     C                   CALLP(E)  QCMDEXC ('CHGJOB STSMSG(*NONE)':20)

      /Free
       wk_MaxIntr = *Zero;
       wk_ShipVia = *Blanks;
       wk_RCBLDP = *Zeros;

       CALLP(E) VPRBLDP (S1GRP#:S1BLTO:S1SHTO:S1DSHP:
                         wk_MaxIntr:ps_Pgnm:wk_ShipVia:wk_RCBLDP);

       SELECT;
       WHEN %error;
         wk_MsgID = 'ERR9002';
         wk_MsgData = 'VPRBLDP';
         EXSR $SndPM;
         wk_Error = *ON;
         ITER;
       WHEN wk_RCBLDP <> *Zeros;
         IF wk_RCBLDP = '03'  or  wk_RCBLDP = '12';
           wk_MsgID = 'MSG0485';
           IF wk_RCBLDP = '03';
             wk_MsgData = 'F3';
           ELSE;
             wk_MsgData = 'F12';
           ENDIF;                                                            // wk_RCBLDP='03'
           EXSR $SndPM;
         ELSE;
           wk_MsgID = 'ERR9019';
           wk_MsgData = 'VPRBLDP';
           %subst(wk_MsgData:11:5) = wk_RCBLDP;
           EXSR $SndPM;
           wk_Error = *ON;
         ENDIF;
         ITER;
       WHEN wk_ShipVia <> F5SHPV;
         wk_F5SHPVold = F5SHPV;
         F5SHPV = wk_ShipVia;
         F5SVDS = *Blanks;
         EXEC SQL select SVSVDS into :F5SVDS
                  from HDSHPV
                  where SVSVSV=:wk_ShipVia;
         EXSR $NewPRO;
         wk_SSShipVia = wk_ShipVia;
         wk_SSPRO# = wk_UpdPRO#;
         wk_MsgID = 'MSG0122';
         wk_MsgData = wk_ShipVia;
         EXSR $SndPM;
         IF wk_MsgIDxx <> *Blanks;
           wk_MsgID = wk_MsgIDxx;
           wk_MsgData = wk_MsgDataxx;
           EXSR $SndPM;
           wk_MsgIDxx = *Blanks;
           wk_MsgDataxx = *Blanks;
         ENDIF;                                                              // wk_MsgIDxx<>*Blanks
         ITER;
       ENDSL;
      /End-Free

     C                   CALLP(E)  QCMDEXC ('CHGJOB STSMSG(*NORMAL)':22)

     C                   EVAL      wk_MsgID = 'MSG5000'                         Clear status message
     C                   EVAL      wk_MsgData = '*'
     C                   EXSR      $SndSM

     C                   ENDIF                                                  wk_CallSS

      *
      * If F21 was pressed to skip ShipSelect, set *IN11 as if
      * F11 had been pressed.
      *
     C                   IF        *IN21
     C                   EVAL      *IN11 = *ON
     C                   ENDIF                                                  *IN21

      *
      * Change customer shipment record.
      *
      * Retrieve customer shipment record for update.
      *
     C     key_GC        CHAIN     HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   EVAL      wk_MsgID = 'ERR9001'
     C                   EVAL      wk_MsgData = 'HYPSGCU'
     C                   EVAL      %subst(wk_MsgData:11:128) =
     C                               %trimr(%editc(S1GRP#:'3')) + '/' +
     C                               %trimr(%editc(S1BLTO:'3')) + '/' +
     C                               %trimr(%editc(S1SHTO:'3')) + '/' +
     C                               %trimr(%editc(S1DSHP:'3'))
     C                   EVAL      %subst(wk_MsgData:139:128) =
     C                               'Press F12 to cancel.'
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   ITER
     C                   ENDIF                                                  not %found(HYPSGCU)

      *
      * If customer shipment record has been changed by another user, abort this completion.
      *
     C                   IF        GCSHPV <> sv_SHPV  or
     C                               GCPRO# <> sv_PRO#  or
     C                               GCFRTA <> sv_FRTA  or
     C                               GCCHGA <> sv_CHGA  or
     C                               GCTSTP2 <> *Loval
     C                   EVAL      wk_MsgID = 'ERR1234'
     C                   EVAL      wk_MsgData = %triml(%editc(GCGRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GCBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GCSHTO:'Z'))
     C                   IF        GCDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GCDSHP:'Z'))
     C                   ENDIF                                                  GCDSHP=*Zero
     C                   EXSR      $SndPM
     C                   EVAL      wk_Error = *ON
     C                   UNLOCK    HYPSGCU
     C                   ITER
     C                   ENDIF                                                  GCSHPV<>sv_SHPV

     C                   EVAL      wk_MsgID = 'MSG9999'
     C                   EVAL      wk_MsgData =
     C                               'Completing shipment.  Please wait . . .'
     C                   EXSR      $SndSM

     C                   IF        F5SHPV <> GCSHPV  or
     C                               F5FRTA <> GCFRTA  or
     C                               F5CHGA <> GCCHGA
      /Free
       EXEC SQL update HYPSGDT
                 set GDSHPV=:F5SHPV, GDSVDS=:F5SVDS
                 where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                       GDSHTO=:S1SHTO and GDDSHP=:S1DSHP;
      /End-Free
     C                   ENDIF                                                  F5SHPV<>GCSHPV  or

     C                   EVAL      GCSHPV = F5SHPV
     C                   EVAL      GCSVDS = F5SVDS
     C                   EVAL      GCFRTA = F5FRTA
     C                   EVAL      GCCHGA = F5CHGA
     C                   IF        wk_UpdPRO# <> *Blanks
     C                   EVAL      GCPRO# = wk_UpdPRO#
     C                   ELSE
     C                   IF        GCPRO# = *Blanks
     C                   EVAL      GCPRO# = F5TRK#
     C                   ENDIF                                                  GCPRO#=*Blanks
     C                   ENDIF                                                  wk_UpdPRO#<>*Blanks

     C                   UPDATE    HYRSGCU

     C                   IF        *IN11
     C                   EVAL      wk_GenBOL = *ON
     C                   ELSE
     C                   EVAL      wk_GenBOL = *OFF
     C                   ENDIF                                                  *IN11

     C                   EVAL      *IN93 = *OFF

     C                   EVAL      wk_RC0614 = CompCustShip (S1GRP#:S1BLTO:
     C                               S1SHTO:S1DSHP:wk_GenBOL:F5PLTS:*OFF)
     C                   SELECT
     C                   WHEN      wk_RC0614 = -406                             Aglix credit card
     C                   EVAL      wk_MsgID = 'ERR9999'                         was not accepted.
     C                   EVAL      wk_MsgData = 'Agilx credit card author+
     C                             ization failed. Shipment not completed.'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   WHEN      wk_RC0614 = -500                             Error on call to
     C                   EVAL      wk_MsgID = 'ERR9002'                         Agilx CC auth.
     C                   EVAL      wk_MsgData = 'WSOSHAG'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   WHEN      wk_RC0614 = -1001
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0804C3'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   WHEN      wk_RC0614 = -1003
     C                   EVAL      wk_MsgID = 'ERR9013'
     C                   EVAL      wk_MsgData = 'SettleFinal'
     C                   EVAL      %subst(wk_MsgData:16:5) = '*N'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   WHEN      wk_RC0614 = -1002  or  wk_RC0614 =-1004
     C                   EVAL      wk_MsgID = 'ERR0508'
     C                   EVAL      wk_MsgData = '*N'
     C                   EVAL      %subst(wk_MsgData:3:35) = '*N'
     C                   EVAL      %subst(wk_MsgData:39:15) = '*N'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   WHEN      wk_RC0614 = -1005
     C                   EVAL      wk_MsgID = 'ERR1286'
     C                   EVAL      wk_MsgData = %triml(%editc(GCSHTO:'Z'))
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   WHEN      wk_RC0614 = -1006  or  wk_RC0614 = -1007
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0500'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   WHEN      wk_RC0614 < *Zero
     C                   EVAL      wk_MsgID = 'ERR1240'
     C                   EVAL      wk_MsgData = %char(wk_RC0614*-1)
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDSL

     C                   IF        F5TRK# <> *Blanks
      /Free
       EXEC SQL insert into HYPSTRK
                 select GDORD#, GDTURN, 1, :F5TRK#,
                        :ps_User, current timestamp
                 from HYPSGDT
                 where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                       GDSHTO=:S1SHTO and GDDSHP=:S1DSHP
                 group by GDORD#, GDTURN;
       IF %len(%trimr(F5TRK#)) > 15;
         EXEC SQL Update HYPTDTA a set TDTRK#=:F5TRK#
                  Where exists (select * from HYPSGDT b
                   where b.GDGRP#=:S1GRP# and b.GDBLTO=:S1BLTO and
                         b.GDSHTO=:S1SHTO and b.GDDSHP=:S1DSHP and
                         b.GDTURN=a.TDTURN);
       ENDIF;                                                                // %len(%trimr(F5TRK#))
      /End-Free
     C                   ENDIF                                                  F5TRK#<>*Blanks

     C                   EVAL      wk_MsgID = 'MSG9999'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndSM

      *
      * Check for and process any required notification windows.
      *
     C                   EVAL      wk_NtfyWdw = 1
     C                   EXSR      $ChkNtfyWdw

     C                   EVAL      wk_NtfyWdw = 2
     C                   EXSR      $ChkNtfyWdw

     C                   EVAL      wk_MsgID = 'MSG0372'
     C                   EVAL      wk_MsgData = %triml(%editc(GCGRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GCBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GCSHTO:'Z'))
     C                   IF        GCDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GCDSHP:'Z'))
     C                   ENDIF                                                  GCDSHP=*Zero
     C                   EXSR      $SndPM

     C                   EVAL      wk_Refresh = *ON

     C                   LEAVE

     C                   ENDDO                                                  *IN03 or *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Retreive new PRO number if applicable.                                 *
      **************************************************************************
     CSR   $NewPRO       BEGSR

      /Free
       wk_TDCF03 = *Blanks;
       wk_TDFF08 = *Blank;
       EXEC SQL select TDCF03, TDFF08
                 into :wk_TDCF03, :wk_TDFF08
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_ShipVia;
       IF wk_TDFF08 <> 'Y'  or  wk_TDCF03 = wk_F5SHPVold;
         LEAVESR;
       ENDIF;                                                                // wk_TDFF08<>'Y' or

       wk_TDCF03 = *Blanks;
       EXEC SQL select TDCF03 into :wk_TDCF03
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_F5SHPVold;
       IF wk_TDCF03 = wk_ShipVia;
         LEAVESR;
       ENDIF;                                                                // wk_TDCF03=wk_ShipVia

       wk_GHDCLOC = *Blanks;
       EXEC SQL select GHDCLOC into :wk_GHDCLOC
                from HYPSGHD
                where GHGRP#=:S1GRP#;

       wk_PRO# = RtvCSPRO#(wk_GHDCLOC:wk_ShipVia);

       IF wk_PRO# <> *Blanks;
         wk_MsgIDxx = 'MSG0458';
         wk_MsgDataxx = wk_PRO#;
       ELSE;
         wk_MsgID = 'ERR1463';
         wk_MsgData = wk_ShipVia + wk_F5SHPVold + GCPRO#;
         EXSR $SndPM;
       ENDIF;                                                                // wk_PRO#<>*Blanks

       wk_UpdPRO# = wk_PRO#;

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit customer group complete shipment screen fields.                   *
      **************************************************************************
     CSR   $Edit5        BEGSR

     C                   EVAL      wk_Error = *OFF

      *
      * Ship via is required and must be valid.
      *
     C                   IF        F5SHPV = *Blanks
     C                   EVAL      wk_MsgID = 'ERR0136'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      F5SVDS = *Blanks
     C                   ELSE
      /Free
       wk_SVSVSV = *Blanks;
       wk_SVSVDS = *Blanks;
       EXEC SQL select SVSVSV, SVSVDS into :wk_SVSVSV, :wk_SVSVDS
                from HDSHPV
                where SVSVSV=:F5SHPV;
      /End-Free
     C                   IF        wk_SVSVSV <> F5SHPV
     C                   EVAL      wk_MsgID = 'ERR0137'
     C                   EVAL      wk_MsgData = F5SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   EVAL      F5SVDS = *Blanks
     C                   ELSE
     C                   EVAL      F5SVDS = wk_SVSVDS
      /Free
       wk_TDFF02 = *Blank;
       wk_TDFF04 = *Blank;
       wk_TDFF06 = *Blank;
       EXEC SQL select TDFF02, TDFF04, TDFF06
                 into :wk_TDFF02, :wk_TDFF04, :wk_TDFF06
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:F5SHPV;
      /End-Free
     C                   IF        wk_TDFF04 <> 'Y'                             Ship via not allowed
     C                   EVAL      wk_MsgID = 'ERR1341'                          at shipping.
     C                   EVAL      wk_MsgData = F5SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        wk_ShipAMMOB  and  wk_TDFF06 <> 'Y'          Ship via not allowed
     C                   EVAL      wk_MsgID = 'ERR1342'                          for bulk ammo
     C                   EVAL      wk_MsgData = F5SHPV                           shipment
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ELSE
     C                   IF        wk_TDFF02 = 'Y'
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from CTRLGI
                where GIUSER=:ps_User;
       IF wk_RcdCnt = *Zero;
         wk_MsgID = 'ERR1398';
         wk_MsgData = ps_User + F5SHPV;
         EXSR $SndPM;
         *IN41 = *ON;
         wk_Error = *ON;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free
     C                   ENDIF                                                  wk_TDFF02='Y'
     C                   ENDIF                                                  wk_ShipAMMOB and
     C                   ENDIF                                                  wk_TDFF04<>'Y'
     C                   ENDIF                                                  wk_SVSVSV<>F5SHPV
     C                   ENDIF                                                  F5SHPV=*Blanks

      *
      * Freight charge amount must be valid.
      *
     C                   IF        F5FRTA < *Zero
     C                   EVAL      wk_MsgID = 'ERR1235'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN42 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F5FRTA<*Zero

      *
      * Special charge amount must be valid.
      *
     C                   IF        F5CHGA < *Zero
     C                   EVAL      wk_MsgID = 'ERR1236'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN43 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F5CHGA<*Zero

      *
      * Changed to always retrieve whether a BOL is required.
      *--- If pallet count is zero or F20 was pressed, retrieve whether a
      *--- BOL is required for this shipment.
      *
     C*****              IF        F5PLTS = *Zero  or  *IN20
      /Free
       wk_CMCCLS = *Blanks;
       EXEC SQL select CMCCLS into :wk_CMCCLS
                from HDCUST
                where CMCUST=:S1BLTO;
      /End-Free
     C                   EVAL      wk_BOLReq = BOLReq(F5SHPV:
     C                                        RtvForeign(wk_CMCCLS:GCSTCT))
     C*****              ENDIF                                                  F5PLTS=*Zero or

      *
      * Pallet count is conditionally required and must be valid.
      *
     C                   IF        F5PLTS = *Zero
     C                   IF        wk_BOLReq
     C                   EVAL      wk_MsgID = 'ERR1241'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN44 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_BOLReq
     C                   ELSE
     C                   IF        F5PLTS < *Zero
     C                   EVAL      wk_MsgID = 'ERR1242'
     C                   EVAL      wk_MsgData = *Blanks
     C                   EXSR      $SndPM
     C                   EVAL      *IN44 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  F5PLTS<*Zero
     C                   ENDIF                                                  F5PLTS=*Zero

      *
      * Validate tracking number based on ship via code.
      *
      /Free
       wk_TDFF05 = *Blanks;
       EXEC SQL select TDFF05 into :wk_TDFF05
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:F5SHPV;
      /End-Free

      *** SHIPVIA.TDFF05 values -
      ***  'R' - Tracking number entry is required
      ***  'O' - Tracking number entry is optional
      ***  'N' - Tracking number entry is not allowed.

     C                   IF        wk_TDFF05 = 'R'  and  F5TRK# = *Blanks
     C                   EVAL      wk_MsgID = 'ERR1290'
     C                   EVAL      wk_MsgData = F5SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN45 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_TDFF05='R' and

     C                   IF        wk_TDFF05 = 'N'  and  F5TRK# <> *Blanks
     C                   EVAL      wk_MsgID = 'ERR1291'
     C                   EVAL      wk_MsgData = F5SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN45 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  wk_TDFF05='N' and

      *
      * F20 is allowed only for shipments generating a BOL.
      *
     C                   IF        *IN20
     C                   IF        not wk_BOLReq
     C                   EVAL      wk_MsgID = 'ERR1243'
     C                   EVAL      wk_MsgData = F5SHPV
     C                   EXSR      $SndPM
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_BOLReq
     C                   ENDIF                                                  *IN20

      *
      * F11 is not allowed for shipments creating a BOL but having
      * shared pallets with other shipments (for the same customer).
      * User should instead use F20 for this shipment, and then use
      * the Combine Orders on BOL screen to create the BOL.
      *
     C                   IF        *IN11  and  wk_BOLReq
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a join HYPSSCC b on a.GDSSCC=b.SCSRL#
                where a.GDGRP#=:S1GRP# and a.GDBLTO=:S1BLTO and
                 a.GDSHTO=:S1SHTO and a.GDDSHP=:S1DSHP and
                 a.GDQSTC>0 and
                 exists (select * from HYPSSCC c where c.SCPSRL#=b.SCPSRL#
                 and not exists (select * from HYPSGDT d where
                 d.GDGRP#=a.GDGRP# and d.GDBLTO=a.GDBLTO and
                 d.GDSHTO=a.GDSHTO and d.GDDSHP=a.GDDSHP and
                 d.GDSSCC=c.SCSRL#));
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_MsgID = 'ERR1346'
     C                   EVAL      wk_MsgData = %triml(%editc(S1GRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(S1BLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(S1SHTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(S1DSHP:'Z'))
     C                   EXSR      $SndPM
     C                   IF        not wk_Error
     C                   EVAL      *IN41 = *ON
     C                   EVAL      *IN89 = *ON
     C                   EVAL      wk_Error = *ON
     C                   ENDIF                                                  not wk_Error
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   ENDIF                                                  *IN11 and

      *
      * If error detected, sound alarm.
      *
     C                   IF        wk_Error
     C                   EVAL      *IN40 = *ON
     C                   ENDIF                                                  wk_Error

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * List valid values for customer group complete shipment screen fields.  *
      **************************************************************************
     CSR   $List5        BEGSR

     C                   SELECT

     C                   WHEN      S@IFLD = 'F5SHPV'
     C                   EVAL      wk_ShipVia = F5SHPV
     C                   EXSR      $ListShipVia
     C                   IF        wk_ValueRtnd
     C                   EVAL      F5SHPV = wk_ShipVia
     C                   ENDIF                                                  wk_ValueRtnd
     C                   EVAL      *IN41 = *ON

     C                   OTHER
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'GUM0013'
     C                   EXSR      $SndPM

     C                   SELECT
     C                   WHEN      S@IFLD = 'F5FRTA'
     C                   EVAL      *IN42 = *ON
     C                   WHEN      S@IFLD = 'F5CHGA'
     C                   EVAL      *IN43 = *ON
     C                   WHEN      S@IFLD = 'F5PLTS'
     C                   EVAL      *IN44 = *ON
     C                   WHEN      S@IFLD = 'F5TRK#'
     C                   EVAL      *IN45 = *ON
     C                   OTHER
     C                   EVAL      *IN41 = *ON
     C                   ENDSL

     C                   EVAL      *IN89 = *ON

     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move data to customer group complete shipment screen.                  *
      **************************************************************************
     CSR   $MoveToFmt5   BEGSR

      /Free
       wk_STName = *Blanks;
       EXEC SQL select CMCNA1 into :wk_STName
                from HDCUST
                where CMCUST=:S1SHTO;
       wk_DSName = *Blanks;
       IF S1DSHP <> *Zero;
         EXEC SQL select DSNAME into :wk_DSName
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:S1SHTO and DSNMBR=:S1DSHP;
       ENDIF;                                                                // S1DSHP<>*Zero
      /End-Free

     C                   EVAL      F5GRP#X = %triml(%editc(S1GRP#:'3'))
     C                   EVAL      F5CPTYX = %triml(%editc(S1CPTY:'3'))
     C                   EVAL      F5BLTOX = %triml(%editc(S1BLTO:'3')) + '-' +
     C                               S1BTNM
     C                   EVAL      F5SHTOX = %triml(%editc(S1SHTO:'3')) + '-' +
     C                               wk_STName
     C                   IF        S1DSHP = *Zero
     C                   EVAL      F5DSHPX = *Blanks
     C                   ELSE
     C                   EVAL      F5DSHPX = %triml(%editc(S1DSHP:'3')) + '-' +
     C                               wk_DSName
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   EVAL      F5STZP = S1STZP
     C                   EVAL      F5SHPV = GCSHPV
     C                   EVAL      F5SVDS = GCSVDS
     C                   EVAL      F5FTRM = GCFTRM
     C                   EVAL      F5FRTA = GCFRTA
     C                   EVAL      F5CHGA = GCCHGA
     C                   EVAL      F5FRZN = GCFRZN
     C                   EVAL      F5ITCT = GCITCT
     C****               EVAL      F5SHWT = GCSHWT
     C                   EVAL      F5SHWT = RtvShipWgt (S1GRP#:S1BLTO:S1SHTO:
     C                                        S1DSHP:'S')
     C                   EVAL      F5PLTS = *Zero
     C                   EVAL      F5TRK# = *Blanks

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize customer group complete shipment screen indicators.         *
      **************************************************************************
     CSR   $InitInd5     BEGSR

     C                   EVAL      *IN41 = *OFF
     C                   EVAL      *IN42 = *OFF
     C                   EVAL      *IN43 = *OFF
     C                   EVAL      *IN44 = *OFF
     C                   EVAL      *IN45 = *OFF
     C                   EVAL      *IN89 = *OFF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Place shipment on intransit hold.                                      *
      **************************************************************************
     CSR   $IntrHold     BEGSR

     C                   IF        ForceCmts (S1GRP#:S1BLTO:S1SHTO:S1DSHP)
     C                   EVAL      wk_CmtsForced = *ON
     C                   EXSR      $DspCmts
     C                   ENDIF                                                  ForceCmts(S1GRP#:

      *
      * Retrieve customer shipment record.
      *
     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)
     C     key_GC        CHAIN(N)  HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   CLEAR                   HYRSGCU
     C                   ENDIF                                                  not %found(HYPSGCU)

     C                   EVAL      wk_AlwF21 = *OFF

      *
      * Populate screen fields.
      *
     C                   EXSR      $MoveToFmt6

      *
      * Initialize screen indicators.
      *
     C                   EVAL      *IN40 = *OFF
     C                   EVAL      *IN93 = *OFF
     C                   EVAL      *IN21 = wk_AlwF21

     C                   DOU       *IN03  or  *IN12

      *
      * Format Display Screen
      *
     C                   WRITE     MSFCTL01B
     C                   EXFMT     DSPFMT6A
     C                   EVAL      *IN93 = *ON
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
      * IF F21 pressed to ignore truck shipment not fully palletized, also
      * set on F11.
      *
     C                   IF        *IN21
     C                   EVAL      *IN11 = *ON
     C                   ENDIF                                                  *IN21

      *
      * Enter pressed.  Edit screen data.
      *
     C                   EXSR      $Edit6
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

      *
      * Enter key pressed?
      *
     C                   IF        not *IN11
     C                   EVAL      wk_MsgID = 'GUM0036'
     C                   EVAL      wk_MsgData = 'F11  place shipment on +
     C                               intransit hold'
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  not *IN11

      *
      * F11 pressed - recheck that shipment can be placed into intransit hold.
      *
     C                   EVAL      wk_IntrHoldChk =
     C                               IntrHoldOK(S1GRP#:S1BLTO:S1SHTO:S1DSHP)
     C                   IF        wk_IntrHoldChk <> *Zeros
     C                   EVAL      *IN40 = *ON
     C                   EVAL      wk_MsgID = 'ERR1265'
     C                   EVAL      wk_MsgData = wk_IntrHoldChk
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
     C                   EXSR      $SndPM
     C                   ITER
     C                   ENDIF                                                  wk_IntrHoldChk=

      *
      * F11 pressed - put all orders/turns for the group into intransit hold.
      *
     C                   IF        not %open(HYLSGDT3)
     C                   OPEN      HYLSGDT3
     C                   ENDIF                                                  not %open(HYLSGDT3)
     C                   IF        not %open(HYPINTR)
     C                   OPEN      HYPINTR
     C                   ENDIF                                                  not %open(HYPINTR)

      /Free
       wk_CMALPH = *Blanks;
       EXEC SQL select CMALPH into :wk_CMALPH
                from HDCUST
                where CMCUST=:S1SHTO;
      /End-Free

     C                   EVAL      sv_TURN = -1
     C                   EVAL      wk_HoldCnt1 = *Zero
     C                   EVAL      wk_HoldCnt2 = *Zero
     C     key_GD        SETLL     HYLSGDT3
     C                   DOU       %eof(HYLSGDT3)
     C     key_GD        READE     HYLSGDT3
     C                   IF        %eof(HYLSGDT3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT3)

     C                   IF        GDTURN = sv_TURN
     C                   ITER
     C                   ENDIF                                                  GDTURN=sv_TURN
     C                   EVAL      sv_TURN = GDTURN

     C                   IF        not OnIntrHold (GDORD#:GDTURN)
     C                   EVAL      IOORD# = GDORD#
     C                   EVAL      IOTURN = GDTURN
     C                   EVAL      IOAIV# = *Zero
     C                   EVAL      IOALPH = wk_CMALPH
     C                   EVAL      IOSHTO = GDSHTO
     C                   EVAL      IOKYF1 = 'BO_COLLECT'
     C                   EVAL      IOKYV1 = 'Y'
     C                   EVAL      IOKYF2 = *Blanks
     C                   EVAL      IOKYV2 = *Blanks
     C                   EVAL      IORLSU = *Blanks
     C                   EVAL      IORLSD = *Zero
     C                   EVAL      IORLST = *Zero
     C                   WRITE     HYRINTR
     C                   EVAL      wk_HoldCnt1 = wk_HoldCnt1 + 1
     C                   ELSE
     C                   EVAL      wk_HoldCnt2 = wk_HoldCnt2 + 1
     C                   ENDIF                                                  not OnIntrHold

     C                   ENDDO                                                  %eof(HYLSGDT3)

     C                   IF        wk_HoldCnt1 > *Zero  and  wk_HoldCnt2 = *Zero
     C                   EVAL      wk_MsgID = 'MSG0378'
     C                   EVAL      wk_MsgData = %triml(%editc(GCGRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GCBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GCSHTO:'Z'))
     C                   IF        GCDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GCDSHP:'Z'))
     C                   ENDIF                                                  GCDSHP=*Zero
     C                   EVAL      %subst(wk_MsgData:31:7) =
     C                               %triml(%editc(wk_HoldCnt1:'J'))
     C                   EVAL      %subst(wk_MsgData:38:7) = '*NONE'
     C                   EXSR      $SndPM
     C                   ELSE
     C                   EVAL      wk_MsgID = 'MSG0379'
     C                   EVAL      wk_MsgData = %triml(%editc(GCGRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GCBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GCSHTO:'Z'))
     C                   IF        GCDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GCDSHP:'Z'))
     C                   ENDIF                                                  GCDSHP=*Zero
     C                   IF        wk_HoldCnt1 = *Zero
     C                   EVAL      %subst(wk_MsgData:31:7) = 'No'
     C                   EVAL      %subst(wk_MsgData:38:7) = '*NONE'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:31:7) =
     C                               %triml(%editc(wk_HoldCnt1:'J'))
     C                   EVAL      %subst(wk_MsgData:38:7) =
     C                               %triml(%editc(wk_HoldCnt1:'J'))
     C                   ENDIF                                                  wk_HoldCnt1=*Zero
     C                   IF        wk_HoldCnt2 = *Zero
     C                   EVAL      %subst(wk_MsgData:45:7) = '*NONE'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:45:7) =
     C                               %triml(%editc(wk_HoldCnt2:'J'))
     C                   ENDIF                                                  wk_HoldCnt2=*Zero
     C                   EXSR      $SndPM
     C                   ENDIF                                                  wk_HoldCnt1>*Zero

      *
      * If the customer shipment is open, save the shipment data.  In
      * addition to other file updates, this will change the shipment's
      * status to "C" (completed).
      *

      /Free
       wk_GCSTS = *Blank;
       EXEC SQL select GCSTS into :wk_GCSTS
                from HYPSGCU
                where GCGRP#=:S1GRP# and GCBLTO=:S1BLTO and
                 GCSHTO=:S1SHTO and GCDSHP=:S1DSHP;
      /End-Free

     C                   IF        wk_GCSTS <> 'O'
     C                   EVAL      wk_Refresh = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_GCSTS<>'O'

     C                   EVAL      wk_MsgID = 'MSG9999'
     C                   EVAL      wk_MsgData =
     C                               'Updating shipment.  Please wait . . .'
     C                   EXSR      $SndSM

     C                   EVAL      *IN93 = *OFF
     C                   EVAL      wk_GenBOL = *OFF

     C                   EVAL      wk_RC0614 = CompCustShip (S1GRP#:S1BLTO:
     C                               S1SHTO:S1DSHP:wk_GenBOL:*Zero:*ON)
     C                   IF        wk_RC0614 < *Zero
     C                   EVAL      wk_MsgID = 'ERR1264'
     C                   EVAL      wk_MsgData = %char(wk_RC0614*-1)
     C                   EXSR      $SndPM
     C                   EVAL      *IN40 = *ON
     C                   ITER
     C                   ENDIF                                                  wk_RC0614<*Zero

     C                   EVAL      wk_MsgID = 'MSG0386'
     C                   EVAL      wk_MsgData = %triml(%editc(GCGRP#:'Z'))
     C                   EVAL      %subst(wk_MsgData:10:7) =
     C                               %triml(%editc(GCBLTO:'Z'))
     C                   EVAL      %subst(wk_MsgData:17:7) =
     C                               %triml(%editc(GCSHTO:'Z'))
     C                   IF        GCDSHP = *Zero
     C                   EVAL      %subst(wk_MsgData:24:7) = '*N'
     C                   ELSE
     C                   EVAL      %subst(wk_MsgData:24:7) =
     C                               %triml(%editc(GCDSHP:'Z'))
     C                   ENDIF                                                  GCDSHP=*Zero
     C                   EXSR      $SndPM

     C                   EVAL      wk_Refresh = *ON

     C                   LEAVE

     C                   ENDDO                                                  *IN03 or *IN12

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Edit place customer group shipment into intransit hold screen fields.  *
      **************************************************************************
     CSR   $Edit6        BEGSR

     C                   EVAL      wk_Error = *OFF

     C                   IF        not *IN21
      /Free
       wk_GCSTYP = *Blank;
       wk_GCSHPV = *Blanks;
       EXEC SQL select GCSTYP, GCSHPV
                 into :wk_GCSTYP, :wk_GCSHPV
                from HYPSGCU
                where GCGRP#=:S1GRP# and GCBLTO=:S1BLTO and
                 GCSHTO=:S1SHTO and GCDSHP=:S1DSHP;

       wk_TDFF01 = *Blanks;
       EXEC SQL select TDFF01 into :wk_TDFF01
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_GCSHPV;

       IF wk_TDFF01 = 'T'  or  wk_GCSTYP = 'T';                              // Truck shipments must
         wk_RcdCnt = *Zero;                                                  // have UCC-128 labels
         EXEC SQL select count(*) into :wk_RcdCnt                            // generated and must
                  from HYPSGDT a                                             // be palletized
                  where a.GDGRP#=:S1GRP# and a.GDBLTO=:S1BLTO and
                        a.GDSHTO=:S1SHTO and a.GDDSHP=:S1DSHP and
                        (a.GDQSTC>0 and (a.GDLBL1<>'Y' or
                        exists (select * from HYPSCWK b
                        where b.S1SRL#=a.GDSSCC and b.S1PSRL#=0)));
         IF wk_RcdCnt > *Zero;
           wk_CtnList = *Blanks;
           EXEC SQL select listagg(distinct GDSSCC,',')
                     within group(order by GDSSCC)
                     into :wk_CtnList
                    from HYPSGDT a
                    where a.GDGRP#=:S1GRP# and a.GDBLTO=:S1BLTO and
                          a.GDSHTO=:S1SHTO and a.GDDSHP=:S1DSHP and
                          (a.GDQSTC>0 and (a.GDLBL1<>'Y' or
                          exists (select * from HYPSCWK b
                          where b.S1SRL#=a.GDSSCC and b.S1PSRL#=0)));
           wk_MsgID = 'ERR1546';
           wk_MsgData = %triml(%editc(S1GRP#:'Z'));
           %subst(wk_MsgData:10:7) = %triml(%editc(S1BLTO:'Z'));
           %subst(wk_MsgData:17:7) = %triml(%editc(S1SHTO:'Z'));
           %subst(wk_MsgData:24:7) = %triml(%editc(S1DSHP:'Z'));
           %subst(wk_MsgData:31:100) = wk_CtnList;
           EXSR $SndPM;
           wk_Error = *ON;
           wk_AlwF21 = *ON;
         ENDIF;                                                              // wk_RcdCnt>*Zero
       ENDIF;                                                                // wk_TDFF01='T'
      /End-Free
     C                   ENDIF                                                  not *IN21

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move data to intransit hold prompt screen.                             *
      **************************************************************************
     CSR   $MoveToFmt6   BEGSR

      /Free
       wk_STName = *Blanks;
       EXEC SQL select CMCNA1 into :wk_STName
                from HDCUST
                where CMCUST=:S1SHTO;
       wk_DSName = *Blanks;
       IF S1DSHP <> *Zero;
         EXEC SQL select DSNAME into :wk_DSName
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:S1SHTO and DSNMBR=:S1DSHP;
       ENDIF;                                                                // S1DSHP<>*Zero
      /End-Free

     C                   EVAL      F6GRP#X = %triml(%editc(S1GRP#:'3'))
     C                   EVAL      F6CPTYX = %triml(%editc(S1CPTY:'3'))
     C                   EVAL      F6BLTOX = %triml(%editc(S1BLTO:'3')) + '-' +
     C                               S1BTNM
     C                   EVAL      F6SHTOX = %triml(%editc(S1SHTO:'3')) + '-' +
     C                               wk_STName
     C                   IF        S1DSHP = *Zero
     C                   EVAL      F6DSHPX = *Blanks
     C                   ELSE
     C                   EVAL      F6DSHPX = %triml(%editc(S1DSHP:'3')) + '-' +
     C                               wk_DSName
     C                   ENDIF                                                  S1DSHP=*Zero
     C                   EVAL      F6STZP = S1STZP
     C                   EVAL      F6SHPV = GCSHPV
     C                   EVAL      F6SVDS = GCSVDS
     C                   EVAL      F6PRO# = GCPRO#
     C                   EVAL      F6FRZN = GCFRZN
     C                   EVAL      F6ITCT = GCITCT
     C****               EVAL      F6SHWT = GCSHWT
     C                   EVAL      F6SHWT = RtvShipWgt (S1GRP#:S1BLTO:S1SHTO:
     C                                        S1DSHP:'S')
     C                   EVAL      F6TSTP1 = GCTSTP1
     C                   IF        GCTSTP2 = *Loval
     C                   EVAL      F6TSTP2 = 'N/A'
     C                   ELSE
     C                   EVAL      F6TSTP2 = %char(GCTSTP2)
     C                   ENDIF                                                  GCTSTP2=*Loval

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Display comments.                                                      *
      **************************************************************************
     CSR   $DspCmts      BEGSR

      /Free
       EXEC SQL insert into HYW0189
                 select :ps_Job#, GDSHTO, GDORD#
                 from HYPSGDT
                 where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                       GDSHTO=:S1SHTO and GDDSHP=:S1DSHP
                 group by GDSHTO, GDORD#
                 order by GDSHTO, GDORD#;
      /End-Free

     C                   EVAL      wk_Pgm04 = *ON
     C                   CALLP(E)  HYR0189 (S1SHTO:*OFF:wk_RC0189)
     C                   SELECT
     C                   WHEN      %error
     C                   EVAL      *IN12 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR9002'
     C                   EVAL      wk_MsgData = 'HYR0189'
     C                   EXSR      $SndPM
     C                   WHEN      wk_RC0189 = '12'
     C                   EVAL      *IN12 = *ON
     C                   WHEN      wk_RC0189 = '99'
     C                   EVAL      *IN12 = *ON
     C                   EVAL      *IN41 = *ON
     C                   EVAL      wk_MsgID = 'ERR1254'
     C                   EVAL      wk_MsgData = %triml(%editc(S1SHTO:'Z'))
     C                   EXSR      $SndPM
     C                   ENDSL

      /Free
       EXEC SQL delete from HYW0189 where W1JOB#=:ps_Job#;
      /End-Free

     C                   IF        not wk_CmtsForced  and  not %error and
     C                               wk_RC0189 <> '99'
      /Free
       EXEC SQL insert into HYPSGCV                                          // CVVWTY='O'
                       (CVGRP#, CVBLTO, CVSHTO, CVDSHP,                      //  means viewed
                        CVVWTY, CVEMPL, CVUSER, CVWS,                        //  optionally
                        CVTSTP)
                values(:S1GRP#, :S1BLTO, :S1SHTO, :S1DSHP,
                       'O', :wk_Empl, :ps_User, :ps_Job,
                       current timestamp);
      /End-Free
     C                   ENDIF                                                  not wk_CmtsForced

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Check whether a notification window should be displayed.               *
      **************************************************************************
     CSR   $ChkNtfyWdw   BEGSR

      /Free
       SELECT;

       WHEN wk_NtfyWdw = 1;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT
                where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                      GDSHTO=:S1SHTO and GDDSHP=:S1DSHP and
                      GDITEM='050180' and GDQSTC>0;
       IF wk_RcdCnt > *Zero;
         W5HDG1 = 'Case Prep Duo Shipment Prompt   ** IMPORTANT **';
         W5TEXT1 = 'If this Case Prep Duo shipment will require an';
         W5TEXT2 = 'air waybill, the following text is required to';
         W5TEXT3 = 'be written in the "nature and quantity of goods"';
         W5TEXT4 = 'box on the waybill.  "Y" and F11 to acknowledge.';
         W5TEXT5 = *Blanks;
         W5TEXT6 = '   ' + x'22' + 'Lithium ion batteries in compliance with' +
                   x'20';
         W5TEXT7 = '   ' + x'22' + 'section II of PI967' +x'20';
         W5FKEY = 'F11=Continue';
         wk_AlwF11 = *ON;
         wk_AlwF12 = *OFF;
         EXSR $ProcNtfyWdw;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       WHEN wk_NtfyWdw = 2;
       wk_RcdCnt = *Zero;
       wk_LblCnt = *Zero;
       EXEC SQL select count(*), count(distinct gdsscc)
                 into :wk_RcdCnt, :wk_LblCnt
                from HYPSGDT a
                where GDGRP#=:S1GRP# and GDBLTO=:S1BLTO and
                      GDSHTO=:S1SHTO and GDDSHP=:S1DSHP and
                      GDQSTC>0 and exists (select * from HYPPCLS b
                      where b.CSSRL#=a.GDSSCC);
       IF wk_RcdCnt > *Zero;
         W5HDG1 = '    Carton UCC-128 Labels Printed Prompt';
         W5TEXT1 = 'Individual UCC-128 carton labels have been';
         W5TEXT2 = 'printed for ' + %triml(%editc(wk_LblCnt:'Z')) +
                   ' multiple-carton UCC-128 labels';
         W5TEXT3 = 'for the current shipment.  These labels (with';
         W5TEXT4 = 'backing still affixed) need to be included with';
         W5TEXT5 = 'the shipment.';
         W5TEXT6 = *Blanks;
         W5TEXT7 = *Blanks;
         W5FKEY = 'F11=Continue';
         wk_AlwF11 = *ON;
         wk_AlwF12 = *OFF;
         EXSR $ProcNtfyWdw;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       ENDSL;
      /End-Free

      *
      * Note - when window allows F12, need to deal with *IN12=*ON.  The first
      * use of this generic window does not allow F12.
      *

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Process generic notification windows.                                  *
      **************************************************************************
     CSR   $ProcNtfyWdw  BEGSR

     C                   EVAL      wk_IN11 = *IN11
     C                   EVAL      W5CONT = *Blank

     C                   DOU       (W5CONT = 'Y'  and  *IN11)  or  *IN12
     C                   EVAL      *IN11 = wk_AlwF11
     C                   EXFMT     WINDOW05B
     C                   IF        *IN12  and  not  wk_AlwF12
     C                   EVAL      *IN12 = *OFF
     C                   ITER
     C                   ENDIF                                                  *IN12 and
     C                   ENDDO                                                  W5CONT='Y' and
     C                   IF        not *IN11
     C                   EVAL      *IN03 = *OFF
     C                   ENDIF                                                  not *IN11
     C                   EVAL      *IN11 = wk_IN11

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

     C                   IF        wk_W3SIZE = '1'
     C                   EXFMT     WINDOW03
     C                   ELSE
     C                   EXFMT     WINDOW03B
     C                   ENDIF                                                  wk_W3SIZE='1'

     C                   EVAL      W3MSG = *Blanks
     C                   EXSR      $InitIndW3

     C                   IF        *IN12
     C                   LEAVESR
     C                   ENDIF                                                  *IN12

     C                   EXSR      $EditW3
     C                   IF        wk_Error
     C                   ITER
     C                   ENDIF                                                  wk_Error

     C                   IN        wk_LDA
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

     C                   SELECT

     C                   WHEN      not wk_ToteVfy
     C                   EVAL      C1FKY1 = F3Txt + '   ' + F5Txt
     C                   IF        not wk_History
     C                   IF        wk_Completed
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F7Txtb
     C                   ELSE
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F7Txta
     C                   ENDIF                                                  wk_Completed
     C                   ENDIF                                                  not wk_History
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F8Txt +
     C                               '   ' + F11Txt + '   ' + F12Txt + '   ' +
     C                               F17Txt + '   ' + F18Txt + '   ' + F20Txt
     C                   IF        wk_PgmMode = MaintMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F22Txt
     C                   ENDIF                                                  wk_PgmMode=MaintMode

     C                   WHEN      wk_FKeySet = '1'
     C                   EVAL      C1FKY1 = F3Txt + '   ' + F5Txt
     C                   IF        not wk_History
     C                   IF        wk_Completed
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F7Txtb
     C                   ELSE
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F7Txta
     C                   ENDIF                                                  wk_Completed
     C                   ENDIF                                                  not wk_History
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F8Txt
     C                   IF        wk_PgmMode = MaintMode  and  not wk_History
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F9Txt
     C                   ENDIF                                                  wk_PgmMode=MantMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F11Txt +
     C                               '   ' + F20Txt
     C                   IF        wk_PgmMode = MaintMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F22Txt
     C                   ENDIF                                                  wk_PgmMode=MaintMode
     C                   EVAL      C1FKY1 = %trimr(C1FKY1) + '   ' + F24Txt

     C                   WHEN      wk_FKeySet = '2'
     C                   EVAL      C1FKY1 = F3Txt + '   ' + F12Txt + '   ' +
     C                               F17Txt + '   ' + F18Txt + '   ' + F24Txt

     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Shut down called programs.                                             *
      **************************************************************************
     CSR   $ShutDownPgms BEGSR

     C                   IF        wk_Pgm01
     C                   CALLP(E)  HYR0602 (ShutDownMode:*Zero:*Zero:
     C                               *Zero:*Zero:wk_RC0602)
     C                   ENDIF                                                  wk_Pgm01

     C                   IF        wk_Pgm02
     C                   ENDIF                                                  wk_Pgm02

     C                   IF        wk_Pgm03
     C                   ENDIF                                                  wk_Pgm03

     C                   IF        wk_Pgm04
     C                   CALLP(E)  HYR0189 (-1:*OFF:wk_RC0189)
     C                   ENDIF                                                  wk_Pgm04

     C                   IF        wk_Pgm05
     C                   EVAL      ary_Ord# = *Zero
     C                   EVAL      idx1 = *Zero
     C                   CALLP(E)  HYR0520 ('03':-1:ary_Ord#:
     C                                      idx1:*Blanks:wk_RC0520)
     C                   ENDIF                                                  wk_Pgm05

     C                   IF        wk_Pgm06
     C                   CALLP(E)  HYR0608 (ShutDownMode:*Zero:*Zero:
     C                               *Zero:*Zero:*Blanks:wk_RC0608)
     C                   ENDIF                                                  wk_Pgm06

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
     C                   EVAL      wk_AlwF9 = *ON
     C                   EVAL      wk_AlwF22 = *ON
     C                   WHEN      wk_PgmMode = InqMode
     C                   EVAL      C1HDG1 = InqHdg
     C                   EVAL      F1HDG1 = InqHdg
     C                   EVAL      C1OPTS = InqOpts
     C                   EVAL      wk_AlwF9 = *OFF
     C                   EVAL      wk_AlwF22 = *OFF
     C                   ENDSL
     C                   EVAL      C1HDG1 = center(C1HDG1)
     C                   EVAL      F1HDG1 = center(F1HDG1)

     C                   EXSR      $InitSS1
     C                   EVAL      F4DVRTX = %editc(F4DVRT:'X')
     C                   EXSR      $SaveSS1
     C                   IF        wk_DftLane <> *Zero  or  wk_DftInclPPk = 'Y'
     C                   EVAL      wk_SSZero = *ON
     C                   ELSE
     C                   EVAL      wk_SSZero = *OFF
     C                   ENDIF                                                  wk_DftLane<>*Zero or

     C                   EVAL      wk_Sort = '1'
     C                   EVAL      wk_History = *OFF
     C                   EVAL      wk_Completed = *OFF
     C                   EVAL      C1HDG2 = OpenShips
     C                   EVAL      C1HDG2 = center(C1HDG2)

     C                   IN        wk_LDA
     C                   IF        %subst(wk_LDA:93:8) = '*TOTEVFY'
     C                   EVAL      wk_ToteVfy = *ON
     C                   ELSE
     C                   EVAL      wk_ToteVfy = *OFF
     C                   EVAL      wk_AlwF9 = *OFF
     C                   ENDIF                                                  %subst(wk_LDA:93:8)

     C                   EVAL      wk_FKeySet = '1'
     C                   EXSR      $FuncKey

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
     C     key_GC        KLIST                                                  HYPSGCU
     C                   KFLD                    S1GRP#
     C                   KFLD                    S1BLTO
     C                   KFLD                    S1SHTO
     C                   KFLD                    S1DSHP

     C     key_GC2       KLIST                                                  HYPSGCU
     C                   KFLD                    ds_CSGRP#
     C                   KFLD                    ds_CSBLTO
     C                   KFLD                    ds_CSSHTO
     C                   KFLD                    ds_CSDSHP

     C     key_GD        KLIST                                                  HYLSGDT3
     C                   KFLD                    S1GRP#
     C                   KFLD                    S1BLTO
     C                   KFLD                    S1SHTO
     C                   KFLD                    S1DSHP

     C     key_GD2       KLIST                                                  HYLSGDT3
     C                   KFLD                    kf_GRP#
     C                   KFLD                    kf_BLTO
     C                   KFLD                    kf_SHTO
     C                   KFLD                    kf_DSHP

     C     key_IO        KLIST                                                  HYPINTR
     C                   KFLD                    kf_ORD#
     C                   KFLD                    kf_TURN
     C                   KFLD                    kf_AIV#

     C     RFSKEY1       KLIST                                                  SFL01 - HYLSGCU1
     C                   KFLD                    @UGRP#
     C                   KFLD                    @UCPTY
     C                   KFLD                    @UBLTO
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP

     C     PGUKEY1       KLIST                                                  SFL01 - HYLSGCU1
     C                   KFLD                    @UGRP#
     C                   KFLD                    @UCPTY
     C                   KFLD                    @UBLTO
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP

     C     PGDKEY1       KLIST                                                  SFL01 - HYLSGCU1
     C                   KFLD                    @DGRP#
     C                   KFLD                    @DCPTY
     C                   KFLD                    @DBLTO
     C                   KFLD                    @DSHTO
     C                   KFLD                    @DDSHP

     C     RFSKEY2       KLIST                                                  SFL01 - HYLSGCU2
     C                   KFLD                    @UBLTO
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UGRP#

     C     PGUKEY2       KLIST                                                  SFL01 - HYLSGCU2
     C                   KFLD                    @UBLTO
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UGRP#

     C     PGDKEY2       KLIST                                                  SFL01 - HYLSGCU2
     C                   KFLD                    @DBLTO
     C                   KFLD                    @DSHTO
     C                   KFLD                    @DDSHP
     C                   KFLD                    @DGRP#

     C     PGDKEY3       KLIST                                                  SFL01 - HYLSGCU3
     C                   KFLD                    @DSHTO
     C                   KFLD                    @DDSHP
     C                   KFLD                    @DGRP#

     C     PGUKEY3       KLIST                                                  SFL01 - HYLSGCU3
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UGRP#

     C     RFSKEY3       KLIST                                                  SFL01 - HYLSGCU3
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UGRP#

     C     PGDKEY4       KLIST                                                  SFL01 - HYLSGCU4
     C                   KFLD                    @DSTNM
     C                   KFLD                    @DSHTO
     C                   KFLD                    @DDSHP
     C                   KFLD                    @DGRP#

     C     PGUKEY4       KLIST                                                  SFL01 - HYLSGCU4
     C                   KFLD                    @USTNM
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UGRP#

     C     RFSKEY4       KLIST                                                  SFL01 - HYLSGCU4
     C                   KFLD                    @USTNM
     C                   KFLD                    @USHTO
     C                   KFLD                    @UDSHP
     C                   KFLD                    @UGRP#

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

      /Free
       wk_Alda = *OFF;
       wk_GI281 = *OFF;
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
       ENDIF;                                                                // wk_RcdCnt>*Zero
       ENDIF;                                                                // wk_RcdCnt>*Zero
      /End-Free

     C                   IF        ps_User = 'BEARL'
     C                   EVAL      wk_SuperUser = *ON
     C                   ELSE
     C                   EVAL      wk_SuperUser = *OFF
     C                   ENDIF                                                  ps_User='BEARL'

     C                   EVAL      wk_DspView = '1'

     CSR                 ENDSR

     P/EJECT
      **************************************************************************
      * Retrieve shipped weight for a customer shipment.                       *
      **************************************************************************
     P RtvShipWgt      B
     D                 PI             5P 0
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST
     D PickShip                       1A   CONST

     D wk_ShipGrp#     S                   LIKE(GCGRP#)
     D wk_BillTo#      S                   LIKE(GCBLTO)
     D wk_ShipTo#      S                   LIKE(GCSHTO)
     D wk_DropShip#    S                   LIKE(GCDSHP)
     D wk_PickShip     S              1A
     D wk_ShipWgt      S              5P 0
     D wk_IMPCLS       S              4A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN4       S             13P 5
     D wk_TDTABL       S             10A
     D wk_TDKEY1       S             15A
     D wk_Bulk         S              1N
     D wk_ExtWgt       S              7P 2
     D wk_TotWgt       S              7P 2

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_PickShip = PickShip
     C                   EVAL      wk_ShipWgt = *Zero

     C                   IF        not %open(HYLSGDT3)
     C                   OPEN      HYLSGDT3
     C                   ENDIF                                                  not %open(HYLSGDT3)

     C                   EVAL      kf_GRP# = wk_ShipGrp#
     C                   EVAL      kf_BLTO = wk_BillTo#
     C                   EVAL      kf_SHTO = wk_ShipTo#
     C                   EVAL      kf_DSHP = wk_DropShip#

     C                   EVAL      wk_TotWgt = *Zero

     C     key_GD2       SETLL     HYLSGDT3
     C                   DOU       %eof(HYLSGDT3)
     C     key_GD2       READE     HYLSGDT3
     C                   IF        %eof(HYLSGDT3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSGDT3)
     C                   IF        (wk_PickShip = 'P'  and  GDQOPK = *Zero)  or
     C                             (wk_PickShip = 'S'  and  GDQSTC = *Zero)
     C                   ITER
     C                   ENDIF                                                  GDQSTC=*Zero
      /Free
       wk_IMPCLS = *Blanks;
       wk_IMIMWG = *Zero;
       wk_IMUDN4 = *Zero;
       EXEC SQL select IMPCLS, IMIMWG, IMUDN4
                 into :wk_IMPCLS, :wk_IMIMWG, :wk_IMUDN4
                from HDIMST
                where IMITEM=:GDITEM;

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
      /End-Free

     C                   SELECT
     C                   WHEN      wk_PickShip = 'P'
     C                   IF        not wk_Bulk
     C                   EVAL(H)   wk_ExtWgt = GDQOPK * wk_IMIMWG
     C                   ELSE
     C                   EVAL(H)   wk_ExtWgt = GDQOPK * wk_IMUDN4
     C                   ENDIF                                                  not wk_Bulk
     C                   WHEN      wk_PickShip = 'S'
     C                   IF        not wk_Bulk
     C                   EVAL(H)   wk_ExtWgt = GDQSTC * wk_IMIMWG
     C                   ELSE
     C                   EVAL(H)   wk_ExtWgt = GDQSTC * wk_IMUDN4
     C                   ENDIF                                                  not wk_Bulk
     C                   ENDSL

     C                   EVAL      wk_TotWgt = wk_TotWgt + wk_ExtWgt

     C                   ENDDO                                                  %eof(HYLSGDT3)

     C                   EVAL(H)   wk_ShipWgt = wk_TotWgt

     C                   ENDDO

     C                   RETURN    wk_ShipWgt

     P RtvShipWgt      E

     P/EJECT
      **************************************************************************
      * Determine whether customer shipment is okay to be deleted.             *
      **************************************************************************
     P DltOK           B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GCGRP#)
     D wk_BillTo#      S                   LIKE(GCBLTO)
     D wk_ShipTo#      S                   LIKE(GCSHTO)
     D wk_DropShip#    S                   LIKE(GCDSHP)
     D wk_RtnCode      S              2A

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_RtnCode = *Zeros

     C                   IF        ps_User <> 'BEARLX'
     C                   EVAL      wk_RtnCode = '01'
     C                   ENDIF                                                  ps_User<>'BEARLX'

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P DltOK           E

     P/EJECT
      **************************************************************************
      * Determine whether customer shipment is eligible to be palletized.      *
      **************************************************************************
     P PalletizeOK     B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GCGRP#)
     D wk_BillTo#      S                   LIKE(GCBLTO)
     D wk_ShipTo#      S                   LIKE(GCSHTO)
     D wk_DropShip#    S                   LIKE(GCDSHP)
     D wk_RtnCode      S              2A
     D wk_GCTSTP2      S               Z

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       wk_GCTSTP2=*Loval;
       EXEC SQL select GCTSTP2 into :wk_GCTSTP2
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;

       IF wk_GCTSTP2 <> *Loval;                                              // Shipment already
         wk_RtnCode = '01';                                                  // completed
         LEAVE;
       ENDIF;                                                                // wk_GCTSTP2<>*Loval
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P PalletizeOK     E

     P/EJECT
      **************************************************************************
      * Determine whether customer shipment is eligible to be completed.       *
      **************************************************************************
     P CompOK          B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GCGRP#)
     D wk_BillTo#      S                   LIKE(GCBLTO)
     D wk_ShipTo#      S                   LIKE(GCSHTO)
     D wk_DropShip#    S                   LIKE(GCDSHP)
     D wk_RtnCode      S              2A
     D wk_GCSHPV       S              2A
     D wk_GCTSTP2      S               Z
     D wk_CMLOC#       S              3S 0
     D wk_TDFF01       S              1A
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       wk_GCSHPV=*Blanks;
       wk_GCTSTP2=*Loval;
       EXEC SQL select GCSHPV, GCTSTP2 into :wk_GCSHPV, :wk_GCTSTP2
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:wk_BillTo# and
                      GCSHTO=:wk_ShipTo# and GCDSHP=:wk_DropShip#;

       IF wk_GCTSTP2 <> *Loval;                                              // Shipment already
         wk_RtnCode = '04';                                                  // completed
         LEAVE;
       ENDIF;                                                                // wk_GCTSTP2<>*Loval

       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDQSTC<GDQOPK;
       IF wk_RcdCnt > *Zero;                                                 // Ships are needed
         wk_RtnCode = '05';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 (GDLOT<>'00' or substr(GDIMDS,1,5)='SAMMO' or
                 substr(GDIMDS,1,5)='SFRON') and
                 GDQSTC>0 and
                 not exists (select * from HYPSGLD b where
                 b.LDGRP#=a.GDGRP# and b.LDTURN=a.GDTURN and
                 b.LDTSEQ=a.GDTSEQ and (b.LDSSCC=a.GDSSCC or b.LDSSCC=0));
       IF wk_RcdCnt > *Zero;                                                 // Lots are needed
         wk_RtnCode = '06';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_CMLOC#=*Zero;
       EXEC SQL select CMLOC# into :wk_CMLOC#
                from HDCUST
                where CMCUST=:wk_BillTo#;

       wk_TDFF01 = *Blanks;
       EXEC SQL select TDFF01 into :wk_TDFF01
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:wk_GCSHPV;

       wk_RcdCnt = *Zero;

       SELECT;

       WHEN wk_TDFF01 = 'T';                                                 // Truck shipments must
         EXEC SQL select count(*) into :wk_RcdCnt                            // have UCC-128 labels
                  from HYPSGDT a                                             // generated and must
                  where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and   // be palletized
                        a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                        (a.GDQSTC<a.GDQOPK or (a.GDQOPK>0 and
                        (a.GDLBL1<>'Y' or
                        exists (select * from HYPSCWK b
                        where b.S1SRL#=a.GDSSCC and b.S1PSRL#=0))));
       IF wk_RcdCnt > *Zero;
         wk_RtnCode = '01';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       WHEN wk_CMLOC# = 91  or  wk_CMLOC# = 93;                              // Large customer
         EXEC SQL select count(*) into :wk_RcdCnt                            // non-truck shipments
                  from HYPSGDT a                                             // must have UCC-128
                  where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and   // labels generated
                        a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                        (a.GDQSTC<a.GDQOPK or (a.GDQOPK>0 and
                        a.GDLBL1<>'Y'));
       IF wk_RcdCnt > *Zero;
         wk_RtnCode = '02';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       OTHER;                                                                // Small customer
         EXEC SQL select count(*) into :wk_RcdCnt                            // non-truck shipments
                  from HYPSGDT                                               // must have been
                  where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and       // shipped via BoxLogix
                        GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and      // (pgm HYR0630)
                        GDQSTC<GDQOPK;
       IF wk_RcdCnt > *Zero;
         wk_RtnCode = '03';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       ENDSL;

       IF wk_RtnCode = *Zeros;
        IF wk_ToteVfy;
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGDT a
                  where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                   a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                   a.GDTOTE<>' ' and substr(a.GDTOTE,1,3)<>'PLT' and
                   ((a.GDTOTE<>'*MULT' and
                   a.GDQSTC>0 and
                   a.GDQSTC>(select coalesce(sum(b.TSQSCN),0)
                   from HYPSGTS b where b.TSGRP#=a.GDGRP# and
                   b.TSTURN=a.GDTURN and b.TSTSEQ=a.GDTSEQ and
                   b.TSRSEQ=a.GDRSEQ and b.TSTOTE=a.GDTOTE))  or
                                      (a.GDTOTE='*MULT' and
                   a.GDQSTC>0 and
                   a.GDQSTC>(select coalesce(sum(c.TSQSCN),0)
                   from HYPSGTS c where c.TSGRP#=a.GDGRP# and
                   c.TSTURN=a.GDTURN and c.TSTSEQ=a.GDTSEQ and
                   c.TSRSEQ=a.GDRSEQ)));
         IF wk_RcdCnt > *Zero;                                               // Totes not verified
           wk_RtnCode = '07';
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt>*Zero
        ENDIF;                                                               // wk_ToteVfy
       ENDIF;                                                                // wk_RtnCode=*Zeros

       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDQSTC>GDQOPK;
       IF wk_RcdCnt > *Zero;                                                 // Shipment cannot be
         wk_RtnCode = '08';                                                  //  over-shipped
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 (GDLOT<>'00' or substr(GDIMDS,1,5)='SAMMO' or
                 substr(GDIMDS,1,5)='SFRON') and
                 GDQSTC>0 and
                 exists (select * from HYPSGLD b where
                 b.LDGRP#=a.GDGRP# and b.LDTURN=a.GDTURN and
                 b.LDTSEQ=a.GDTSEQ and b.LDRSEQ=a.GDRSEQ and
                 exists (select * from HYPRCLT c where
                 c.RLLOT=b.LDLOT#));
       IF wk_RcdCnt > *Zero;                                                 // Recalled lot
         wk_RtnCode = '09';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_RcdCnt=*Zero;
       EXEC SQL select count(distinct b.iwftcl) into :wk_RcdCnt
                from HYPSGDT a join HDIWHS b
                 on a.GDITEM=b.IWITEM and b.IWWHS=6
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDQSTC<>0;
       IF wk_RcdCnt > wk_MaxFtCl;
         wk_RtnCode = '10';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>wk_MaxFtCl
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P CompOK          E

     P/EJECT
      **************************************************************************
      * Determine whether customer shipment is eligible to be placed into      *
      * intransit hold.                                                        *
      **************************************************************************
     P IntrHoldOK      B
     D                 PI             2A
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GCGRP#)
     D wk_BillTo#      S                   LIKE(GCBLTO)
     D wk_ShipTo#      S                   LIKE(GCSHTO)
     D wk_DropShip#    S                   LIKE(GCDSHP)
     D wk_RtnCode      S              2A
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_RtnCode = *Zeros

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                 a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                 exists (select * from HYPIDTA b where b.ISTURN=a.GDTURN);
       IF wk_RcdCnt > *Zero;
         wk_RtnCode = '01';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 (GDLOT<>'00' or substr(GDIMDS,1,5)='SAMMO' or
                 substr(GDIMDS,1,5)='SFRON') and
                 GDQSTC>0 and
                 not exists (select * from HYPSGLD b where
                 b.LDGRP#=a.GDGRP# and b.LDTURN=a.GDTURN and
                 b.LDTSEQ=a.GDTSEQ and b.LDRSEQ=a.GDRSEQ);
       IF wk_RcdCnt > *Zero;                                                 // Lots are needed
         wk_RtnCode = '02';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       EXEC SQL select count(*) into :wk_RcdCnt                              // Any shipped item
                from HYPSGDT a                                               // must have UCC-128
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and     // labels generated
                      a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                      a.GDQSTC>0 and a.GDLBL1<>'Y';
       IF wk_RcdCnt > *Zero;
         wk_RtnCode = '03';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       IF wk_RtnCode = *Zeros;
        IF wk_ToteVfy;
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPSGDT a
                  where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                   a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                   a.GDTOTE<>' ' and substr(a.GDTOTE,1,3)<>'PLT' and
                   ((a.GDTOTE<>'*MULT' and
                   a.GDQSTC>0 and
                   a.GDQSTC>(select coalesce(sum(b.TSQSCN),0)
                   from HYPSGTS b where b.TSGRP#=a.GDGRP# and
                   b.TSTURN=a.GDTURN and b.TSTSEQ=a.GDTSEQ and
                   b.TSRSEQ=a.GDRSEQ and b.TSTOTE=a.GDTOTE))  or
                                      (a.GDTOTE='*MULT' and
                   a.GDQSTC>0 and
                   a.GDQSTC>(select coalesce(sum(c.TSQSCN),0)
                   from HYPSGTS c where c.TSGRP#=a.GDGRP# and
                   c.TSTURN=a.GDTURN and c.TSTSEQ=a.GDTSEQ and
                   c.TSRSEQ=a.GDRSEQ)));
         IF wk_RcdCnt > *Zero;                                               // Totes not verified
           wk_RtnCode = '04';
           LEAVE;
         ENDIF;                                                              // wk_RcdCnt>*Zero
        ENDIF;                                                               // wk_ToteVfy
       ENDIF;                                                                // wk_RtnCode=*Zeros

       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDQSTC>GDQOPK;
       IF wk_RcdCnt > *Zero;                                                 // Shipment cannot be
         wk_RtnCode = '05';                                                  //  over-shipped
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_RcdCnt=*Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 (GDLOT<>'00' or substr(GDIMDS,1,5)='SAMMO' or
                 substr(GDIMDS,1,5)='SFRON') and
                 GDQSTC>0 and
                 exists (select * from HYPSGLD b where
                 b.LDGRP#=a.GDGRP# and b.LDTURN=a.GDTURN and
                 b.LDTSEQ=a.GDTSEQ and b.LDRSEQ=a.GDRSEQ and
                 exists (select * from HYPRCLT c where
                 c.RLLOT=b.LDLOT#));
       IF wk_RcdCnt > *Zero;                                                 // Recalled lot
         wk_RtnCode = '06';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>*Zero

       wk_RcdCnt=*Zero;
       EXEC SQL select count(distinct b.iwftcl) into :wk_RcdCnt
                from HYPSGDT a join HDIWHS b
                 on a.GDITEM=b.IWITEM and b.IWWHS=6
                where GDGRP#=:wk_ShipGrp# and GDBLTO=:wk_BillTo# and
                 GDSHTO=:wk_ShipTo# and GDDSHP=:wk_DropShip# and
                 GDQSTC<>0;
       IF wk_RcdCnt > wk_MaxFtCl;
         wk_RtnCode = '07';
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt>wk_MaxFtCl
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_RtnCode

     P IntrHoldOK      E

     P/EJECT
      **************************************************************************
      * Determine if order is on intransit hold.                               *
      **************************************************************************
     P OnIntrHold      B
     D                 PI             1N
     D Order#                         8S 0 CONST
     D Turn#                          9S 0 CONST

     D wk_Order#       S              8S 0
     D wk_Turn#        S              9S 0
     D wk_OnIntrHold   S              1N
     D wk_OrdLvlRls    S              1N

     C                   DO

     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_OnIntrHold = *OFF
     C                   EVAL      wk_OrdLvlRls = *OFF

     C                   EVAL      kf_ORD# = wk_Order#
     C                   EVAL      kf_TURN = wk_Turn#
     C                   EVAL      kf_AIV# = *Zero

     C                   IF        not %open(HYPINTR)
     C                   OPEN      HYPINTR
     C                   ENDIF                                                  not %open(HYPINTR)

      *
      * If the current turnaround number already has its own HYPINTR record,
      * check its release status to determine whether it is on intransit
      * hold.
      *
     C                   IF        wk_Turn# <> *Zero
     C     key_IO        CHAIN     HYPINTR
     C                   IF        %found(HYPINTR)
     C                   IF        IORLSD = *Zero
     C                   EVAL      wk_OnIntrHold = *ON
     C                   ENDIF                                                  IORLSD=*Zero
     C                   LEAVE
     C                   ENDIF                                                  %found(HYPINTR)
     C                   ENDIF                                                  wk_Turn#<>*Zero

      *
      * If the current turnaround number does not yet have its own HYPINTR
      * record, check for any other record for the order that has an order-
      * level hold type and that has not yet been invoiced.  If any found,
      * the order, and thus this turnaround, is on intransit hold.
      * Exception to this is if a zero-turn record is found with a release
      * date.  In that case, no shipment for the order is on intransit hold.
      *
     C     wk_Order#     SETLL     HYPINTR
     C                   DOU       %eof(HYPINTR)
     C     wk_Order#     READE     HYPINTR
     C                   IF        %eof(HYPINTR)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPINTR)
     C                   IF        IOAIV# <> *Zero
     C                   ITER
     C                   ENDIF                                                  IOAIV#<>*Zero
     C                   IF        IOTURN = *Zero  and  IORLSD <> *Zero
     C                   EVAL      wk_OrdLvlRls = *ON
     C                   LEAVE
     C                   ENDIF                                                  IOTURN=*Zero and
     C****               IF        IOTURN <> *Zero
     C****               EVAL      wk_OnIntrHold = *ON
     C****               LEAVE
     C****               ENDIF                                                  IOTURN<>*Zero
     C                   ENDDO                                                  %eof(HYPINTR)
     C                   IF        wk_OnIntrHold  or  wk_OrdLvlRls
     C                   LEAVE
     C                   ENDIF                                                  wk_OnIntrHold

      *
      * After all checks for intransit hold at the shipment level have
      * been exhausted, if there is an order-level (zero-turn) HYPINTR
      * record for the order, and it has not been released, the order
      * (every shipment) is on intransit hold.
      *
     C     wk_Order#     SETLL     HYPINTR
     C                   DOU       %eof(HYPINTR)
     C     wk_Order#     READE     HYPINTR
     C                   IF        %eof(HYPINTR)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPINTR)
     C                   IF        IOTURN > *Zero
     C                   LEAVE
     C                   ENDIF                                                  IOTURN>*Zero
     C                   IF        IOAIV# <> *Zero
     C                   ITER
     C                   ENDIF                                                  IOAIV#<>*Zero
     C                   IF        IORLSD = *Zero
     C                   EVAL      wk_OnIntrHold = *ON
     C                   LEAVE
     C                   ENDIF                                                  IORLSD=*Zero
     C                   ENDDO                                                  %eof(HYPINTR)

     C                   ENDDO

     C                   RETURN    wk_OnIntrHold

     P OnIntrHold      E

     P/EJECT
      **************************************************************************
      * Determine whether to force shipment comments inquiry.                  *
      **************************************************************************
     P ForceCmts       B
     D                 PI             1N
     D ShipGrp#                       9S 0 CONST
     D BillTo#                        7S 0 CONST
     D ShipTo#                        7S 0 CONST
     D DropShip#                      7S 0 CONST

     D wk_ShipGrp#     S                   LIKE(GCGRP#)
     D wk_BillTo#      S                   LIKE(GCBLTO)
     D wk_ShipTo#      S                   LIKE(GCSHTO)
     D wk_DropShip#    S                   LIKE(GCDSHP)
     D wk_ForceCmts    S              1N
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_BillTo# = BillTo#
     C                   EVAL      wk_ShipTo# = ShipTo#
     C                   EVAL      wk_DropShip# = DropShip#
     C                   EVAL      wk_ForceCmts = *OFF

      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGDT a
                where a.GDGRP#=:wk_ShipGrp# and a.GDBLTO=:wk_BillTo# and
                      a.GDSHTO=:wk_ShipTo# and a.GDDSHP=:wk_DropShip# and
                      exists (select * from OEOCMT b where
                      b.OCORD#=a.GDORD# and b.OCORL# in (0, 999, a.GDORL#)
                      and b.OCDOCT='PIC');
       IF wk_RcdCnt = *Zero;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt=*Zero

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPSGCV a
                where a.CVGRP#=:wk_ShipGrp# and a.CVBLTO=:wk_BillTo# and
                      a.CVSHTO=:wk_ShipTo# and a.CVDSHP=:wk_DropShip# and
                      (a.CVUSER=:ps_User or a.CVWS=:ps_Job);
       IF wk_RcdCnt > *Zero;
         LEAVE;
       ENDIF;                                                                // wk_RcdCnt=*Zero

       EXEC SQL insert into HYPSGCV
                       (CVGRP#, CVBLTO, CVSHTO, CVDSHP,
                        CVVWTY, CVEMPL, CVUSER, CVWS,
                        CVTSTP)
                values(:S1GRP#, :S1BLTO, :S1SHTO, :S1DSHP,
                       'F', :wk_Empl, :ps_User, :ps_Job,
                       current timestamp);

       wk_ForceCmts = *ON;
      /End-Free

     C                   ENDDO

     C                   RETURN    wk_ForceCmts

     P ForceCmts       E

     P/EJECT
