     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2018')
     H BNDDIR('HYR0622')
      **************************************************************************
      * APPLICATION: Shipping/BoxLogix                                         *
      *       TITLE: Print UCC-128 labels                                      *
      *          BY: William Earl (MCA)                                        *
      *        DATE: September 2018                                            *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  04/25/2021  MCA  Seconds ammo can NEQ calculation.                    *
      *  06/12/2021  MCA  Use *DTAARA SHPLBLSHPV to determine whether to print *
      *                   carrier information on carton labels.  txt #6145     *
      *  06/29/2021  MCA  Add product classes ASAD and ASAE to allow explosive *
      *                   label to be printed.                                 *
      *  07/15/2021  MCA  Purchased ammo inventory FET credits (FETV).         *
      *  02/21/2024  MCA  Handle mixed-lot and mixed-item hazard labels.       *
      *  03/19/2024  MCA  Handle mixed-lot and mixed-item hazard labels (2).   *
      *  03/20/2024  MCA  Handle mixed-lot and mixed-item hazard labels (3).   *
      *  04/30/2025  MCA  Print hazard labels based on drop-ship address       *
      *                   country (if otherwise applicable).                   *
      *  05/14/2025  MCA  Include ship-to phone number on foreign UCC-128      *
      *                   labels.                                              *
      *  06/02/2025  MCA  When generating exlposive label data, if shipping    *
      *                   ammo cans (which originate from other packaged       *
      *                   ammo), retrieve the powder charge from the original  *
      *                   ammo.                                                *
      *  09/07/2025  MCA  Add new label type "X", which will work like label   *
      *                   type "E", to print a hazard label regardless of      *
      *                   shipment destination, if item is otherwise           *
      *                   applicable for a hazard label. Essentially overrides *
      *                   to print hazard labels for domestic shipments.       *
      *  01/12/2026  MCA  Change Ammo NEQ calulation to use the item's product *
      *                   structure to get the powder weight (instead of floor *
      *                   QC), and to add primer NEQ if it rounds to at least  *
      *                   .01 kg.                                              *
      *                                                                        *
      **************************************************************************

     Hnomain

     FOEORHD    IF   E           K DISK    USROPN
     FOEORDT    IF   E           K DISK    USROPN
     FHYPSGCU   IF   E           K DISK    USROPN
     FHYLSLBL1  UF A E           K DISK    USROPN
     FHYLELBL1  UF A E           K DISK    USROPN
     FLDLMORD1  IF   E           K DISK    RENAME(LDRMORD:LDRMORD1) USROPN
     FLDLMORD2  IF   E           K DISK    RENAME(LDRMORD:LDRMORD2) USROPN
     FLDPFLRA   IF   E           K DISK    USROPN
     FLDPMODT   IF   E           K DISK    USROPN
     FLDPMOD3   IF   E           K DISK    USROPN
     FHYPACSL   IF   E           K DISK    PREFIX(SZ:2) USROPN
     FHYLSSCC8  IF   E           K DISK    RENAME(HYRSSCC:HYRSSCC8)             k-SCSRL#
     F                                     RENAME(HYRSCWK:HYRSCWK8)
     F                                     PREFIX(SC:2) USROPN
     FGUPTDAT   IF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************

      /COPY *LIBL/QPRPSRC,HYR0622

     D HYR0116         PR                  EXTPROC('HYR0116')
     D  APID                          2A   CONST
     D  Order#                        8P 0 CONST
     D  Line#                         3P 0 CONST
     D  Blanket#                      3P 0 CONST
     D  CrtnType                      1A   CONST
     D  ExplsLbl                      1A   CONST
     D  ErrorCode                     4A

     D HYR0152         PR                  EXTPROC('HYR0152')
     D  Function                      1A   CONST
     D  Order#                        8P 0 CONST
     D  OrdLin#                       3P 0 CONST
     D  Blanket#                      3P 0 CONST
     D  BoxWgt                        6P 2 CONST
     D  HazMatNQ                      6P 2 CONST
     D  PowderWgt                     6P 2 CONST
     D  WgtUOM                        2A   CONST
     D  Serial#                       9P 0 CONST

      /COPY *LIBL/QPRPSRC,HYR9916

      /COPY *LIBL/QPRPSRC,HYR9916A

      /COPY *LIBL/QPRPSRC,HYR9933

      /COPY *LIBL/QPRPSRC,HYR9960

      /COPY *LIBL/QPRPSRC,HYR9962

      /COPY *LIBL/QPRPSRC,EDR9900

     D RtvAmzHvy       PR             8A
     D  Item#                        15A   CONST
     D  UnitWgt                       9P 5 CONST
     D  CtnQty                        9P 0 CONST

     D RtvWSLoc        PR             4A

     D NeedPhone       PR             1N
     D  Foreign                       1N   CONST
     D  ShipToCtry                    3A   CONST

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
      * Define work fields.                                                    *
      **************************************************************************
     D wk_DatabaseID2  S              2A                                        Database ID 2
     D wk_CtnLblShpV   S              1N
     D wk_WSLoc        S              4A   INZ(*Blank)
     D wk_Pgm0152      S              1N   INZ(*OFF)
     D wk_PgmEDR9900   S              1N   INZ(*OFF)
     D kf_GRP#         S                   LIKE(GCGRP#)
     D kf_BLTO         S                   LIKE(GCBLTO)
     D kf_SHTO         S                   LIKE(GCSHTO)
     D kf_DSHP         S                   LIKE(GCDSHP)
     D kf_ORD#         S                   LIKE(ODORD#)
     D kf_ORL#         S                   LIKE(ODORL#)
     D kf_BLN#         S                   LIKE(ODBLN#)
     D kf_CAN          S                   LIKE(SZCAN)
     D kf_CLOT         S                   LIKE(SZCLOT)
     D kf_JOBN         S                   LIKE(SLJOBN)
     D kf_TABL         S                   LIKE(TDTABL)
     D kf_KEY1         S                   LIKE(TDKEY1)
     D kf_KEY2         S                   LIKE(TDKEY2)

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_ShpLblShpV   S              2A   DTAARA(SHPLBLSHPV)                   Print shp-via on lbl
     D wk_UCC128Phn    S              1A   DTAARA(HYAUCCPHN)                    Print phone# on lbl

      **************************************************************************
      * Define key lists.                                                      *
      **************************************************************************
     C     key_OD        KLIST                                                  OEORDT
     C                   KFLD                    kf_ORD#
     C                   KFLD                    kf_ORL#
     C                   KFLD                    kf_BLN#

     C     key_GC        KLIST                                                  HYPSGCU
     C                   KFLD                    kf_GRP#
     C                   KFLD                    kf_BLTO
     C                   KFLD                    kf_SHTO
     C                   KFLD                    kf_DSHP

     C     key_OM        KLIST                                                  LDLMORD2
     C                   KFLD                    ODCPN
     C                   KFLD                    ODLOT

     C     key_SZ        KLIST                                                  HYPACSL
     C                   KFLD                    kf_CAN
     C                   KFLD                    kf_CLOT

     C     key_TD1       KLIST                                                  GUPTDAT
     C                   KFLD                    kf_TABL
     C                   KFLD                    kf_KEY1

     C     key_TD2       KLIST                                                  GUPTDAT
     C                   KFLD                    kf_TABL
     C                   KFLD                    kf_KEY1
     C                   KFLD                    kf_KEY2

     P/EJECT
      **************************************************************************
      * Print a UCC-128 shipping label.                                        *
      **************************************************************************
     P PrtUCC128Lbl    B                   EXPORT
     D                 PI            10I 0
     D ShipGrp#                       9S 0 CONST
     D Order#                         8S 0 CONST
     D Line#                          3S 0 CONST
     D Blanket#                       3S 0 CONST
     D SSCCBC                        20A   CONST
     D LblType                        1A   CONST
      *** F=Full, P=Partial, M=Mixed, E=Explosive label only, H=Heavy label only
      *** X-Explosive label any desination (foreign or domestic)
     D PrtExplsLbl                    1N   CONST
     D PrtHvyLbl                      1N   CONST
     D PrtlQty                        7P 0 CONST
     D PrtlUOM                        2A   CONST

     D wk_ShipGrp#     S              9S 0
     D wk_Order#       S              8S 0
     D wk_Line#        S              3S 0
     D wk_Blanket#     S              3S 0
     D wk_SSCCBC       S             20A
     D wk_LblType      S              1A
     D wk_PrtExplsLbl  S              1N
     D wk_PrtHvyLbl    S              1N
     D wk_PrtlQty      S              7P 0
     D wk_PrtlUOM      S              2A
     D wk_RtnCode      S             10I 0
     D wk_Lbl1         S              1N
     D wk_Lbl2         S              1N
     D wk_Lbl3         S              1N
     D wk_HzLbAnyDest  S              1N
     D wk_SLNAME       S             30A
     D wk_SLADDR1      S             30A
     D wk_SLADDR2      S             30A
     D wk_SLCITY       S             19A
     D wk_SLST         S              2A
     D wk_SLZIP        S             10A
     D wk_SLPHON       S             11S 0
     D wk_GDORD#       S              8S 0
     D wk_GDDSHP       S              7S 0
     D wk_BTCCLS       S              2A
     D wk_STCCLS       S              2A
     D wk_STCNA1       S             26A
     D wk_STCNA2       S             26A
     D wk_STCNA3       S             26A
     D wk_STCNA4       S             19A
     D wk_STCCTY       S             26A
     D wk_STST         S              2A
     D wk_STZIP        S             13A
     D wk_STCTRY       S              3A
     D wk_STSTOR       S             20A
     D wk_STPHON       S             11S 0
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
     D wk_SVSCAC       S              4A
     D wk_IMIMDS       S             30A
     D wk_IMPCLS       S              4A
     D wk_IMLOT        S              1A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN1       S             13P 5
     D wk_IMUDN4       S             13P 5
     D wk_IWPCPC       S              9P 4
     D wk_IWFTCL       S              5A
     D wk_SRL#         S              9P 0
     D wk_Lot#         S             15A
     D wk_Lot#2        S             15A
     D wk_CtnQty       S              5P 0
     D wk_CtnUOMQty    S              9P 0
     D wk_CtnUOMQty2   S              9P 0
     D wk_Carton#      S              5P 0
     D wk_Bulk         S              1N
     D wk_S1SRL#       S              9P 0
     D wk_S1LOT#       S             15A
     D wk_S1UOMQ       S             13P 4
     D wk_S1CTN#       S              5P 0
     D wk_SCSRL#       S              9P 0
     D wk_SCLOT#       S             15A
     D wk_SCUOMQ       S             13P 4
     D wk_SCCTN#       S              5P 0
     D wk_Foreign      S              1N
     D wk_ExplLbl      S              1A
     D wk_OMORD        S                   LIKE(OMORD)
     D wk_UseCustUPC   S              1N
     D wk_CBCUST       S              7P 0
     D wk_CBITEM       S             15A
     D wk_CBCUPC       S             12A
     D wk_CBIPL        S              1A
     D wk_ETTPID       S             15A
     D wk_PrimItm      S             15A
     D wk_StorePfxPtr  S              3P 0
     D wk_ShipLocAddr  S              4A
     D wk_ExplsLbl     S              1A
     D wk_EDWKWritten  S              1N   INZ(*OFF)
     D wk_CrtHzLblRcd  S              1N   INZ(*OFF)
     D wk_Grains       S                   LIKE(FAPCHG)
     D wk_TempNum      S             15P 5
     D wk_LbsToKgCvt   S              7P 7 INZ(.4535924)
     D wk_MixedCtn     S              1N   INZ(*OFF)
     D wk_MixCtnAmmo   S              1N   INZ(*OFF)
     D wk_MixCtnSAmmo  S              1N   INZ(*OFF)
     D wk_MixCtn1Shot  S              1N   INZ(*OFF)
     D wk_ELBWGT       S                   LIKE(ELBWGT)
     D wk_ELBWGT2      S                   LIKE(ELBWGT)
     D wk_ELHMNQ       S                   LIKE(ELHMNQ)
     D wk_ELHMNQ2      S                   LIKE(ELHMNQ)
     D wk_ELPWGT       S                   LIKE(ELPWGT)
     D wk_ELPWGT2      S                   LIKE(ELPWGT)
     D wk_REF02        S             30A   BASED(wk_REF02Ptr)
     D wk_REF03        S             80A   BASED(wk_REF03Ptr)
     D wk_HvyLbl       S              8A
     D wk_GenHeavyLbl  S              1N   INZ(*OFF)
     D wk_SRL#x        S              9A
     D wk_Msg52        S             52A
     D wk_CtnItemCnt   S              7P 0
     D wk_STPhonLbl    S             11S 0
     D wk_STPhonCtry   S              3A
     D wk_FmtPhone     S             15A
     D wk_FPLen        S              3P 0
     D wk_RcdCnt       S              7P 0
     D wk_RC0116       S              4A
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0
     D sv_Order#       S                   LIKE(wk_Order#)

     D Yes             C                   'Y'
     D No              C                   'N'
     D Only            C                   'O'
     D Digits          C                   '0123456789'
     D CustNameLen     C                   %len(wk_STCNA1)
     D BPStoreNbrLen   C                   4
     D DSGStoreNbrLen  C                   4
     D GMStoreNbrLen   C                   4
     D WrtEDWK         C                   '1'
     D CtnTextPfx      C                   'CARTON #:'
     D Heavy           C                   CONST('H')
     D HeavyAmz        C                   CONST('Z')

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Order# = Order#
     C                   EVAL      wk_Line# = Line#
     C                   EVAL      wk_Blanket# = Blanket#
     C                   EVAL      wk_SSCCBC = SSCCBC
     C                   EVAL      wk_LblType = LblType
     C                   EVAL      wk_PrtExplsLbl = PrtExplsLbl
     C                   EVAL      wk_PrtHvyLbl = PrtHvyLbl
     C                   EVAL      wk_PrtlQty = PrtlQty
     C                   EVAL      wk_PrtlUOM = PrtlUOM
     C                   EVAL      wk_RtnCode = *Zero

     C                   IF        wk_DatabaseID = *Blanks
     C                   IN        wk_DatabaseID
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'XX'
     C                   EVAL      wk_DatabaseID2 = 'HY'
     C                   WHEN      wk_DatabaseID = 'SX'
     C                   EVAL      wk_DatabaseID2 = 'SS'
     C                   WHEN      wk_DatabaseID = 'PX'
     C                   EVAL      wk_DatabaseID2 = 'PV'
     C                   OTHER
     C                   EVAL      wk_DatabaseID2 = wk_DatabaseID
     C                   ENDSL
     C                   ENDIF                                                  wk_DatabaseID=*Blank

     C                   IF        wk_ShpLblShpV = *Blanks
     C                   IN        wk_ShpLblShpV
     C                   IF        %subst(wk_ShpLblShpV:1:1) = 'Y'
     C                   EVAL      wk_CtnLblShpV = *ON
     C                   ELSE
     C                   EVAL      wk_CtnLblShpV = *OFF
     C                   ENDIF                                                  %subst(wk_ShpLblShpV
     C                   ENDIF                                                  wk_ShpLblShpV=*Blank

     C                   IF        not %open(OEORHD)
     C                   OPEN      OEORHD
     C                   ENDIF                                                  not %open(ORORHD)
     C                   IF        not %open(OEORDT)
     C                   OPEN      OEORDT
     C                   ENDIF                                                  not %open(ORORDT)
     C                   IF        not %open(HYPSGCU)
     C                   OPEN      HYPSGCU
     C                   ENDIF                                                  not %open(HYPSGCU)
     C                   IF        not %open(HYLSLBL1)
     C                   OPEN      HYLSLBL1
     C                   ENDIF                                                  not %open(HYLSLBL1)
     C                   IF        not %open(GUPTDAT)
     C                   OPEN      GUPTDAT
     C                   ENDIF                                                  not %open(GUPTDAT)

     C                   IF        wk_WSLoc = *Blanks
     C                   EVAL      wk_WSLoc = RtvWSLoc ()
     C                   ENDIF                                                  wk_WSLoc=*Blanks

     C                   EVAL      wk_Lbl1 = *OFF
     C                   EVAL      wk_Lbl2 = *OFF
     C                   EVAL      wk_Lbl3 = *OFF

     C                   IF        wk_LblType = 'X'
     C                   EVAL      wk_HzLbAnyDest = *ON
     C                   EVAL      wk_LblType = 'E'
     C                   ELSE
     C                   EVAL      wk_HzLbAnyDest = *OFF
     C                   ENDIF                                                  wk_LblType='X'

     C                   EXSR      $RtvLblDta

     C                   IF        wk_RtnCode < *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_RtnCode<*Zero

     C                   IF        wk_LblType = 'E'
     C                   IF        wk_ExplsLbl = No
     C                   LEAVE
     C                   ENDIF                                                  wk_ExplsLbl=No
     C                   EVAL      wk_LblType = 'F'
     C                   EVAL      wk_ExplsLbl = Only
     C                   ENDIF                                                  wk_LblType='E'

     C                   IF        wk_LblType <> 'H'
     C                               and
     C                             (wk_ExplsLbl <> Only  or  wk_PrtExplsLbl)
     C                   CALLP(E)  HYR0116 ('OE':wk_Order#:wk_Line#:wk_Blanket#:
     C                               wk_LblType:wk_ExplsLbl:wk_RC0116)
     C                   ENDIF                                                  wk_LblType<>'H'

     C                   EVAL      wk_Lbl1 = *ON
     C                   IF        wk_ExplsLbl = Yes
     C                               or  wk_ExplsLbl = Only
     C                   EVAL      wk_Lbl2 = *ON
     C                   ENDIF                                                  wk_ExplsLbl='Y'

     C                   IF        wk_PrtHvyLbl
     C                   EXSR      $PrtHvyLbl
     C                   ENDIF                                                  wk_PrtHvyLbl

     C                   SELECT
      *** UCC-128 shipping label only.
     C                   WHEN      wk_Lbl1  and  not wk_Lbl2  and  not wk_Lbl3
     C                   EVAL      wk_RtnCode = 1
      *** UCC-128 shipping label and explosive label.
     C                   WHEN      wk_Lbl1  and  wk_Lbl2  and  not wk_Lbl3
     C                   EVAL      wk_RtnCode = 2
      *** UCC-128 shipping label and "HEAVY" label.
     C                   WHEN      wk_Lbl1  and  not wk_Lbl2  and  wk_Lbl3
     C                   EVAL      wk_RtnCode = 3
      *** UCC-128 shipping label, explosive label, and "HEAVY" label.
     C                   WHEN      wk_Lbl1  and  wk_Lbl2  and  wk_Lbl3
     C                   EVAL      wk_RtnCode = 4
     C                   ENDSL

     C                   ENDDO

     C                   RETURN    wk_RtnCode

      **************************************************************************
      * Retrieve label data.                                                   *
      **************************************************************************
     C     $RtvLblDta    BEGSR

     C     wk_Order#     CHAIN     OEORHD
     C                   IF        not %found(OEORHD)
     C                   EVAL      wk_RtnCode = -1
     C                   LEAVESR
     C                   ENDIF                                                  not %found(OEORHD)

     C                   EVAL      kf_ORD# = wk_Order#
     C                   EVAL      kf_ORL# = wk_Line#
     C                   EVAL      kf_BLN# = wk_Blanket#
     C     key_OD        CHAIN     OEORDT
     C                   IF        not %found(OEORDT)
     C                   EVAL      wk_RtnCode = -2
     C                   LEAVESR
     C                   ENDIF                                                  not %found(OEORDT)

      *
      * Retrieve drop ship number from Shipment Group Customer Shipment record,
      * instead of using the order header's drop ship number.
      * Note - This was needed because, sometimes after a shipment group was
      *  created, the order header record's drop ship number was changed.
      *  If this function was called after the order header was changed, the
      *  HYPSGCU record was not found, a label was not generated, and the
      *  function returned error code -3.
      *
      /Free
       wk_GDORD# = *Zero;
       wk_GDDSHP = *Zero;
       EXEC SQL select GDORD#, GDDSHP
                 into :wk_GDORD#, :wk_GDDSHP
                from HYPSGDT
                where GDGRP#=:wk_ShipGrp# and GDORD#=:wk_Order# and
                      GDORL#=:wk_Line# and GDBLN#=:wk_Blanket#
                fetch first row only;
       IF wk_GDORD# <> wk_Order#;
         wk_GDDSHP = OEDSHP;
         IF wk_DatabaseID = 'HY';
           IF wk_SSCCBC <> *Blanks  and
               %check(Digits:%subst(wk_SSCCBC:11:9)) = *Zero;
             wk_SRL#x = %triml(%editc(%dec(%subst(wk_SSCCBC:11:9):9:0):'Z'));
           ELSE;
             wk_SRL#x = ' *N';
           ENDIF;                                                              // wk_SSCCBC<>*Blanks
           wk_Msg52 = 'PrtUCC128Lbl HYPSGDT no rcd. Grp#' +
                      %triml(%editc(wk_ShipGrp#:'Z')) + ' Lbl#' + wk_SRL#x;
           DSPLY(E) wk_Msg52 'QSYSOPR';
           DSPLY(E) wk_Msg52 'BEARL';
           wk_Msg52 = '  Ord#' + %triml(%editc(wk_Order#:'Z')) +
                      ' Lin#' + %triml(%editc(wk_Line#:'Z')) +
                      ' Blkt#' + %triml(%editc(wk_Blanket#:'Z'));
           DSPLY(E) wk_Msg52 'QSYSOPR';
           DSPLY(E) wk_Msg52 'BEARL';
         ENDIF;                                                              // wk_DatabaseID='HY'
       ENDIF;                                                                // wk_GDORD#<>wk_Order#
      /End-Free

     C                   EVAL      kf_GRP# = wk_ShipGrp#
     C                   EVAL      kf_BLTO = OEBLTO
     C                   EVAL      kf_SHTO = OESHTO
     C****               EVAL      kf_DSHP = OEDSHP
     C                   EVAL      kf_DSHP = wk_GDDSHP
     C     key_GC        CHAIN     HYPSGCU
     C                   IF        not %found(HYPSGCU)
     C                   EVAL      wk_RtnCode = -3
     C                   LEAVESR
     C                   ENDIF                                                  not %found(HYPSGCU)

      /Free
       wk_BTCCLS = *Blanks;
       wk_STCNA1 = *Blanks;
       wk_STCNA2 = *Blanks;
       wk_STCNA3 = *Blanks;
       wk_STCNA4 = *Blanks;
       wk_STCCTY = *Blanks;
       wk_STST   = *Blanks;
       wk_STZIP  = *Blanks;
       wk_STCTRY = *Blanks;
       wk_STCCLS = *Blanks;
       wk_STSTOR = *Blanks;
       wk_STPHON = *Zero;
       EXEC SQL select CMCCLS into :wk_BTCCLS
                from HDCUST
                where CMCUST=:OEBLTO;
       EXEC SQL select CMCNA1, CMCNA2, CMCNA3, CMCNA4,
                       CMCCTY, CMST, CMZIP, CMCTRY,
                       CMCCLS, CMSTOR, CMPHON
                 into  :wk_STCNA1, :wk_STCNA2, :wk_STCNA3, :wk_STCNA4,
                       :wk_STCCTY, :wk_STST, :wk_STZIP, :wk_STCTRY,
                       :wk_STCCLS, :wk_STSTOR, :wk_STPHON
                from HDCUST
                where CMCUST=:OESHTO;

       wk_DSNAME = *Blanks;
       wk_DSADR1 = *Blanks;
       wk_DSADR2 = *Blanks;
       wk_DSADR3 = *Blanks;
       wk_DSCITY = *Blanks;
       wk_DSST   = *Blanks;
       wk_DSZIP  = *Blanks;
       wk_DSCTRY = *Blanks;
       wk_DSPHON = *Zero;
       // IF OEDSHP <> *Zero;
       IF wk_GDDSHP <> *Zero;
         EXEC SQL select DSNAME, DSADR1, DSADR2, DSADR3,
                         DSCITY, DSST, DSZIP, DSCTRY,
                         DSPHON
                   into  :wk_DSNAME, :wk_DSADR1, :wk_DSADR2, :wk_DSADR3,
                         :wk_DSCITY, :wk_DSST, :wk_DSZIP, :wk_DSCTRY,
                         :wk_DSPHON
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:OESHTO and DSNMBR=:wk_GDDSHP;
       ENDIF;                                                                // wk_GDDSHP<>*Zero

       wk_SVSCAC = *Blanks;
       EXEC SQL select SVSCAC into :wk_SVSCAC
                from HDSHPV
                where SVSVSV=:GCSHPV;

       wk_IMIMDS = *Blanks;
       wk_IMPCLS = *Blanks;
       wk_IMLOT = *Blank;
       wk_IMIMWG = *Zero;
       wk_IMUDN1 = *Zero;
       wk_IMUDN4 = *Zero;
       EXEC SQL select IMIMDS, IMPCLS, IMLOT, IMIMWG,
                       IMUDN1, IMUDN4
                 into :wk_IMIMDS, :wk_IMPCLS, :wk_IMLOT, :wk_IMIMWG,
                      :wk_IMUDN1, :wk_IMUDN4
                from HDIMST
                where IMITEM=:ODITEM;
       IF wk_IMIMDS = *Blanks;
         wk_IMIMDS = ODIMDS;
       ENDIF;                                                                // wk_IMIMDS=*Blanks
       IF wk_IMPCLS = *Blanks;
         wk_IMPCLS = ODPCLS;
       ENDIF;                                                                // wk_IMPCLS=*Blanks
       kf_TABL = 'BULKPRDCLS';
       kf_KEY1 = wk_IMPCLS;
       CHAIN key_TD1 GUPTDAT;
       IF %found(GUPTDAT);
        wk_Bulk = *ON;
       ELSE;
        wk_Bulk = *OFF;
       ENDIF;                                                                // %found(GUPTDAT)

       wk_IWPCPC = *Zero;
       wk_IWFTCL = *Blanks;
       EXEC SQL select IWPCPC, IWFTCL into :wk_IWPCPC, :wk_IWFTCL
                From HDIWHS Where IWITEM=:ODITEM and IWWHS=:ODWH;
       IF wk_IWPCPC > *Zero;
         wk_CtnQty = wk_IWPCPC;
       ELSE;
         wk_CtnQty = 1;
       ENDIF;                                                                // wk_IWPCPC>*Zero

       IF wk_SSCCBC <> *Blanks  and
           %check(Digits:%subst(wk_SSCCBC:11:9)) = *Zero;
         wk_SRL# = %dec(%subst(wk_SSCCBC:11:9):9:0);
       ELSE;
         wk_SRL# = *Zero;
       ENDIF;                                                                // wk_SSCCBC<>*Blanks
       wk_Lot# = *Blanks;
       wk_CtnUOMQty = *Zero;
       wk_Carton# = *Zero;
       wk_S1SRL# = *Zero;
       wk_S1LOT# = *Blanks;
       wk_S1UOMQ = *Zero;
       wk_S1CTN# = *Zero;
       wk_SCSRL# = *Zero;
       wk_SCLOT# = *Blanks;
       wk_SCUOMQ = *Zero;
       wk_SCCTN# = *Zero;
       IF wk_SRL# <> *Zero;
         wk_RcdCnt = *Zero;
         EXEC SQL select coalesce(sum(S1UOMQ),0), count(*)
                   into :wk_S1UOMQ, :wk_RcdCnt
                  from HYPSCWK
                  where S1ORD#=:wk_Order# and S1ORL#=:wk_Line# and
                   S1BLN#=:wk_Blanket# and S1SRL#=:wk_SRL#;
         IF wk_RcdCnt > *Zero;
           EXEC SQL select S1SRL#, S1LOT#, S1CTN#
                     into :wk_S1SRL#, :wk_S1LOT#, :wk_S1CTN#
                    from HYPSCWK
                    where S1ORD#=:wk_Order# and S1ORL#=:wk_Line# and
                     S1BLN#=:wk_Blanket# and S1SRL#=:wk_SRL#
                    order by S1UOMQ desc, S1LOT# desc
                    fetch first row only;
         ENDIF;                                                              // wk_RcdCnt>*Zero
         IF  wk_S1SRL# = wk_SRL#;
           wk_Lot# = wk_S1LOT#;
           wk_CtnUOMQty = wk_S1UOMQ;
           wk_Carton# = wk_S1CTN#;
         ELSE;
           wk_RcdCnt = *Zero;
           EXEC SQL select coalesce(sum(SCUOMQ),0), count(*)
                     into :wk_SCUOMQ, :wk_RcdCnt
                    from HYPSSCC
                    where SCORD#=:wk_Order# and SCORL#=:wk_Line# and
                     SCBLN#=:wk_Blanket# and SCSRL#=:wk_SRL#;
           IF SQLCODE < *Zero;
             wk_Msg52 = 'PrtUCC128Lbl SQL#1 error ' + %editc(SQLCODE:'L') +
                        'Grp#' + %triml(%editc(wk_ShipGrp#:'Z')) +
                        ' Lbl#' + wk_SRL#x;
             DSPLY(E) wk_Msg52 'QSYSOPR';
             DSPLY(E) wk_Msg52 'BEARL';
             wk_Msg52 = '  Ord#' + %triml(%editc(wk_Order#:'Z')) +
                        ' Lin#' + %triml(%editc(wk_Line#:'Z')) +
                        ' Blkt#' + %triml(%editc(wk_Blanket#:'Z'));
             DSPLY(E) wk_Msg52 'QSYSOPR';
             DSPLY(E) wk_Msg52 'BEARL';
           ENDIF;                                                            // SQLCODE<*Zero
           IF wk_RcdCnt > *Zero;
             EXEC SQL select SCSRL#, SCLOT#, SCCTN#
                       into :wk_SCSRL#, :wk_SCLOT#, :wk_SCCTN#
                      from HYPSSCC
                      where SCORD#=:wk_Order# and SCORL#=:wk_Line# and
                       SCBLN#=:wk_Blanket# and SCSRL#=:wk_SRL#
                      order by SCUOMQ desc, SCLOT# desc
                      fetch first row only;
           ENDIF;                                                            // wk_RcdCnt>*Zero
           IF  wk_SCSRL# = wk_SRL#;
             wk_Lot# = wk_SCLOT#;
             wk_CtnUOMQty = wk_SCUOMQ;
             wk_Carton# = wk_SCCTN#;
           ENDIF;                                                            // wk_SCSRL#=wk_SRL#
         ENDIF;                                                              // wk_S1SRL#=wk_SRL#
       ENDIF;                                                                // wk_SRL#<>*Zero
      /End-Free

     C                   IF        wk_BTCCLS = 'RF'
     C                   EVAL      wk_Foreign = *ON
     C                   ELSE
     C                   EVAL      kf_TABL = 'CUSTCLASS'
     C                   EVAL      kf_KEY1 = wk_BTCCLS
     C     key_TD1       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)  and  TDFF02 = 'Y'
     C                   EVAL      wk_Foreign = *ON
     C                   ELSE
     C                   EVAL      wk_Foreign = *OFF
     C                   ENDIF                                                  %found(GUPTDAT) and
     C                   ENDIF                                                  wk_BTCCLS='RF'
     C                   IF        not wk_Foreign  and  wk_STCCLS <> wk_BTCCLS
     C                   IF        wk_STCCLS = 'RF'
     C                   EVAL      wk_Foreign = *ON
     C                   ELSE
     C                   EVAL      kf_TABL = 'CUSTCLASS'
     C                   EVAL      kf_KEY1 = wk_STCCLS
     C     key_TD1       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)  and  TDFF02 = 'Y'
     C                   EVAL      wk_Foreign = *ON
     C                   ENDIF                                                  %found(GUPTDAT) and
     C                   ENDIF                                                  wk_STCCLS='RF'
     C                   ENDIF                                                  not wk_Foreign and
     C                   EVAL      kf_TABL = 'CUSTUSABT'
     C                   EVAL      kf_KEY1 = %editc(OEBLTO:'X')
     C     key_TD1       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_ExplLbl = TDFF01
     C                   ELSE
     C                   EVAL      wk_ExplLbl = *Blank
     C                   ENDIF                                                  %found(GUPTDAT)

      /Free
       wk_UseCustUPC = *OFF;
       wk_CBCUST = *Zero;
       wk_CBITEM = *Blanks;
       wk_CBCUPC = *Blanks;
       wk_CBIPL = *Blank;
       EXEC SQL select CBCUST, CBITEM, CBCUPC, CBIPL
                into :wk_CBCUST, :wk_CBITEM, :wk_CBCUPC, :wk_CBIPL
                from BARCUST
                where CBCUST=:OEBLTO and CBITEM=:ODITEM;
       IF wk_CBCUST = OEBLTO  and  wk_CBITEM = ODITEM  and
            wk_CBCUPC <> *Blanks  and  wk_CBIPL = 'C';
         wk_UseCustUPC = *ON;
       ENDIF;                                                                // wk_CBCUST=OEBLTO and
      /End-Free

      *
      * Retrieve this job's HYPSLBL work record.  If this job just now
      * started up, the work record won't be found and will be written
      * (instead of updated) below.
      *
     C                   EVAL      kf_JOBN = ps_Job#
     C     kf_JOBN       CHAIN     HYLSLBL1

     C                   EVAL      SLORD# = wk_Order#
     C                   IF        wk_CtnLblShpV
     C                   EVAL      SLSVSV = GCSHPV
     C                   EVAL      SLSVDS = GCSVDS
     C                   EVAL      SLSCAC = wk_SVSCAC
     C                   EVAL      SLPRO# = GCPRO#
     C                   ELSE
     C                   EVAL      SLSVSV = *Blanks
     C                   EVAL      SLSVDS = *Blanks
     C                   EVAL      SLSCAC = *Blanks
     C                   EVAL      SLPRO# = *Blanks
     C                   ENDIF                                                  wk_CtnLblShpV
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      OEBLTO = 17                                  Sports, Inc.
     C                   IF        %check(Digits:wk_STCNA1:23) = *Zero
     C                   EVAL      SLSTOR = %subst(wk_STCNA1:23:4)
     C                   ELSE
     C                   EVAL      SLSTOR = *Blanks
     C                   ENDIF                                                  %check(Digits:%subst
     C                   WHEN      OEBLTO = 52                                  Academy Stores
     C                   EVAL      SLSTOR = '00004'
     C                   WHEN      OEBLTO = 59                                  Bass Pro Shops
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_STCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - BPStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(wk_STCNA1 :
     C                               wk_StorePfxPtr + 2 : BPStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   WHEN      OEBLTO = 337                                 Dick's Sporting Good
     C                               or  OEBLTO = 536527                        DSG Field & Stream
     C                   IF        wk_STSTOR <> *Blanks
     C                   EVAL      SLSTOR = wk_STSTOR
     C                   ELSE
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_STCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - DSGStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(wk_STCNA1 :
     C                               wk_StorePfxPtr + 2 : DSGStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   ENDIF                                                  wk_STSTOR<>*Blanks
     C                   WHEN      OEBLTO = 21419
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_STCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - GMStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(wk_STCNA1 :
     C                               wk_StorePfxPtr + 2 : GMStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   OTHER
     C                   EVAL      SLSTOR = *Blanks
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_databaseID = 'SX'
     C                   SELECT
     C                   WHEN      OEBLTO = 4                                   Sports, Inc.
     C                   IF        %check(Digits:wk_STCNA1:23) = *Zero
     C                   EVAL      SLSTOR = %subst(wk_STCNA1:23:4)
     C                   ELSE
     C                   EVAL      SLSTOR = *Blanks
     C                   ENDIF                                                  %check(Digits:%subst
     C                   WHEN      OEBLTO = 3035                                Academy Stores
     C                   EVAL      SLSTOR = wk_STSTOR
     C                   WHEN      OEBLTO = 3069                                Bass Pro Shops
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_STCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - BPStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(wk_STCNA1 :
     C                               wk_StorePfxPtr + 2 : BPStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   WHEN      OEBLTO = 93                                  Dick's Sporting Good
     C****                           or  OEBLTO = xxxxx                         DSG Field & Stream
     C                   IF        wk_STSTOR <> *Blanks
     C                   EVAL      SLSTOR = wk_STSTOR
     C                   ELSE
     C                   EVAL      wk_StorePfxPtr = %scan(' #' : wk_STCNA1)
     C                   IF        wk_StorePfxPtr = *Zero  or
     C                               wk_StorePfxPtr >
     C                                 CustNameLen - DSGStoreNbrLen - 1
     C                   EVAL      SLSTOR = *Blanks
     C                   ELSE
     C                   EVAL      SLSTOR = %subst(wk_STCNA1 :
     C                               wk_StorePfxPtr + 2 : DSGStoreNbrLen)
     C                   ENDIF                                                  wk_StorePfxPtr=*Zero
     C                   ENDIF                                                  wk_STSTOR<>*Blanks
     C                   OTHER
     C                   EVAL      SLSTOR = *Blanks
     C                   ENDSL
     C                   ENDSL
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      OEBLTO = 52                                  Academy Stores
     C                   EVAL      SLVEND = '01211'
     C                   OTHER
     C                   EVAL      SLVEND = *Blanks
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   SELECT
     C                   WHEN      OEBLTO = 3035                                Academy Stores
     C                   EVAL      SLVEND = '47562'
     C                   OTHER
     C                   EVAL      SLVEND = *Blanks
     C                   ENDSL
     C                   ENDSL
     C****               IF        OEDSHP = *Zero  or
     C                   IF        wk_GDDSHP = *Zero  or
     C                               (wk_STCTRY <> 'US'  and
     C                                wk_STCTRY <> 'CA'  and
     C                                wk_STCTRY <> *Blanks)
     C                   EVAL      SLSTNM = wk_STCNA1
     C                   EVAL      SLSTA1 = wk_STCNA2
     C                   EVAL      SLSTA2 = wk_STCNA3
     C                   EVAL      SLSTA3 = wk_STCNA4
     C                   EVAL      SLSTCY = wk_STCCTY
     C                   EVAL      SLSTST = wk_STST
     C                   EVAL      SLSTZP = wk_STZIP
     C                   IF        wk_STCTRY = 'US'  or  wk_STCTRY = *Blanks
     C                   EVAL      SLSTCT = *Blanks
     C                   ELSE
      /Free
       wk_CNCDES = *Blanks;
       EXEC SQL select CNCDES into :wk_CNCDES
                from HDCTRY
                where CNCTCD=:wk_STCTRY;
       IF wk_CNCDES = *Blanks;
         wk_CNCDES = wk_STCTRY;
       ENDIF;                                                                // wk_CNCDES=*Blanks
      /End-Free
     C                   IF        wk_CNCDES <> SLSTA1  and
     C                               wk_CNCDES <> SLSTA2  and
     C                               wk_CNCDES <> SLSTA3  and
     C                               wk_CNCDES <> SLSTCY
     C                   EVAL      SLSTCT = wk_CNCDES
     C                   ELSE
     C                   EVAL      SLSTCT = *Blanks
     C                   ENDIF                                                  wk_CNCDES<>SLSTA1
     C                   ENDIF                                                  CMCTRY='US' or
     C                   EVAL      wk_STPhonLbl = wk_STPHON
     C                   EVAL      wk_STPhonCtry = wk_STCTRY
     C                   ELSE
     C                   EVAL      SLSTNM = wk_DSNAME
     C                   EVAL      SLSTA1 = wk_DSADR1
     C                   EVAL      SLSTA2 = wk_DSADR2
     C                   EVAL      SLSTA3 = wk_DSADR3
     C                   EVAL      SLSTCY = wk_DSCITY
     C                   EVAL      SLSTST = wk_DSST
     C                   EVAL      SLSTZP = wk_DSZIP
     C                   IF        wk_DSCTRY = 'US'  or  wk_DSCTRY = *Blanks
     C                   EVAL      SLSTCT = *Blanks
     C                   ELSE
      /Free
       wk_CNCDES = *Blanks;
       EXEC SQL select CNCDES into :wk_CNCDES
                from HDCTRY
                where CNCTCD=:wk_DSCTRY;
       IF wk_CNCDES = *Blanks;
         wk_CNCDES = wk_DSCTRY;
       ENDIF;                                                                // wk_CNCDES=*Blanks
      /End-Free
     C                   IF        wk_CNCDES <> SLSTA1  and
     C                               wk_CNCDES <> SLSTA2  and
     C                               wk_CNCDES <> SLSTA3  and
     C                               wk_CNCDES <> SLSTCY
     C                   EVAL      SLSTCT = wk_CNCDES
     C                   ELSE
     C                   EVAL      SLSTCT = *Blanks
     C                   ENDIF                                                  wk_CNCDES<>SLSTA1
     C                   ENDIF                                                  DSCTRY='US' or
     C                   EVAL      wk_STPhonLbl = wk_DSPHON
     C                   EVAL      wk_STPhonCtry = wk_DSCTRY
     C                   ENDIF                                                  wk_GDDSHP=*Zero

      *
      * Retrieve whether to include ship-to phone number on UCC-128 labels.
      *
     C                   IF        wk_Order# <> sv_Order#  or
     C                               wk_UCC128Phn = *Blank
     C                   IN        wk_UCC128Phn
     C                   EVAL      sv_Order# = wk_Order#
     C                   ENDIF                                                  wk_Order#<>sv_Order#

      *
      * Include phone number on label, if needed.
      *
     C                   IF        NeedPhone (wk_Foreign:wk_STPhonCtry)
     C                   EVAL      wk_FmtPhone =
     C                               FmtPhone (wk_STPhonLbl:wk_STPhonCtry:'2')  2=E.164
     C                   IF        wk_FmtPhone <> *Blanks
     C                   EVAL      wk_FPLen = %len(%trimr(wk_FmtPhone))
     C                   SELECT
     C                   WHEN      %len(%trimr(SLSTNM)) + wk_FPLen + 1 <= 30
     C                   EVAL      %subst(SLSTNM:30-wk_FPLen + 1) = wk_FmtPhone
     C                   WHEN      %len(%trimr(SLSTCT)) + wk_FPLen + 1 <= 30
     C                   EVAL      %subst(SLSTCT:30-wk_FPLen + 1) = wk_FmtPhone
     C                   OTHER
     C                   EVAL      wk_MsgID = 'ERR1496'
     C                   EVAL      wk_MsgData = wk_FmtPhone + SLSTNM + SLSTCT
     C                   EXSR      $WrtJobLog
     C                   ENDSL
     C                   ENDIF                                                  wk_FmtPhone<>*Blanks
     C                   ENDIF                                                  NeedPhone(wk_Foreign

     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      OEBLTO = 337                                 Dick's Sporting Good
     C                               or  OEBLTO = 536527                        DSG Field & Stream
      /Free
       wk_RcdCnt = *Zero;
       wk_ETTPID = *Blanks;
       EXEC SQL select count(*) into :wk_RcdCnt
                from EDITPM
                where ET0143='850' and ETCUST=:OESHTO;
       SELECT;
       WHEN wk_RcdCnt = 1;
         EXEC SQL select ETTPID into :wk_ETTPID
                  from EDITPM
                  where ET0143='850' and ETCUST=:OESHTO;
       WHEN wk_RcdCnt > 1;
         EXEC SQL select min(ETTPID) into :wk_ETTPID
                  from EDITPM
                  where ET0143='850' and ETCUST=:OESHTO and
                   ETTPID<>' ';
       ENDSL;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_Ref02Ptr =
     C                               EDI850Char(wk_ETTPID:OEORRF:'REF02':'DP')
     C                   EVAL      SLDEPT = wk_Ref02
     C                   EVAL      wk_Ref03Ptr =
     C                               EDI850Char(wk_ETTPID:OEORRF:'REF03':'DP')
     C                   EVAL      SLDPNM = wk_Ref03
     C                   EVAL      wk_PgmEDR9900 = *ON
     C                   ELSE
     C                   EVAL      SLDEPT = *Blanks
     C                   EVAL      SLDPNM = *Blanks
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   OTHER
     C                   EVAL      SLDEPT = *Blanks
     C                   EVAL      SLDPNM = *Blanks
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   SELECT
     C                   WHEN      OEBLTO = 93                                  Dick's Sporting Good
     C****                           or  OEBLTO = xxxxx                         DSG Field & Stream
      /Free
       wk_RcdCnt = *Zero;
       wk_ETTPID = *Blanks;
       EXEC SQL select count(*) into :wk_RcdCnt
                from EDITPM
                where ET0143='850' and ETCUST=:OESHTO;
       SELECT;
       WHEN wk_RcdCnt = 1;
         EXEC SQL select ETTPID into :wk_ETTPID
                  from EDITPM
                  where ET0143='850' and ETCUST=:OESHTO;
       WHEN wk_RcdCnt > 1;
         EXEC SQL select min(ETTPID) into :wk_ETTPID
                  from EDITPM
                  where ET0143='850' and ETCUST=:OESHTO and
                   ETTPID<>' ';
       ENDSL;
      /End-Free
     C                   IF        wk_RcdCnt > *Zero
     C                   EVAL      wk_Ref02Ptr =
     C                               EDI850Char(wk_ETTPID:OEORRF:'REF02':'DP')
     C                   EVAL      SLDEPT = wk_Ref02
     C                   EVAL      wk_Ref03Ptr =
     C                               EDI850Char(wk_ETTPID:OEORRF:'REF03':'DP')
     C                   EVAL      SLDPNM = wk_Ref03
     C                   EVAL      wk_PgmEDR9900 = *ON
     C                   ELSE
     C                   EVAL      SLDEPT = *Blanks
     C                   EVAL      SLDPNM = *Blanks
     C                   ENDIF                                                  wk_RcdCnt>*Zero
     C                   OTHER
     C                   EVAL      SLDEPT = *Blanks
     C                   EVAL      SLDPNM = *Blanks
     C                   ENDSL
     C                   ENDSL
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      wk_WSLoc = 'ALDA'  and                       Alda wkstn
     C                               not wk_Foreign
     C                   EVAL      wk_ShipLocAddr = 'ALDA'
     C                   WHEN      wk_WSLoc = 'GI'  and                         GI Hwy 281 wkstn
     C                               not wk_Foreign
     C                   EVAL      wk_ShipLocAddr = 'GI'
     C                   OTHER
     C                   EVAL      wk_ShipLocAddr = 'WEST'                      West DC wkstn
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   SELECT
     C                   WHEN      wk_WSLoc = 'ALDA'                            Alda wkstn
     C                   EVAL      wk_ShipLocAddr = '095'
     C                   WHEN      wk_WSLoc = 'GI'                              GI Hwy 281 wkstn
     C                   EVAL      wk_ShipLocAddr = 'GI'
     C                   OTHER
     C                   EVAL      wk_ShipLocAddr = 'WEST'                      West DC wkstn
     C                   ENDSL
     C                   WHEN      wk_DatabaseID = 'PV'  or
     C                               wk_DatabaseID = 'PX'
     C                   EVAL      wk_ShipLocAddr = 'PVE'
     C                   OTHER
     C                   EVAL      wk_ShipLocAddr = '****'
     C                   ENDSL
      /Free
       wk_SLNAME = *Blanks;
       wk_SLADDR1 = *Blanks;
       wk_SLADDR2 = *Blanks;
       wk_SLCITY = *Blanks;
       wk_SLST = *Blanks;
       wk_SLZIP = *Blanks;
       wk_SLPHON = *Zero;
       EXEC SQL select SLNAME, SLADDR1, SLADDR2, SLCITY,
                       SLST, SLZIP, SLPHON
                 into  :wk_SLNAME, :wk_SLADDR1, :wk_SLADDR2, :wk_SLCITY,
                       :wk_SLST, :wk_SLZIP, :wk_SLPHON
                from HYPSLOC
                where SLSLOC=:wk_ShipLocAddr;
       IF wk_SLNAME = *Blanks;
         SELECT;
         WHEN wk_DatabaseID = 'HY'  or
                wk_DatabaseID = 'XX';
           wk_SLNAME = 'HORNADY MFG COMPANY';
         WHEN wk_DatabaseID = 'SS'  or
                wk_DatabaseID = 'SX';
           wk_SLNAME = 'SNAPSAFE';
         WHEN wk_DatabaseID = 'PV'  or
                wk_DatabaseID = 'PX';
           wk_SLNAME = 'PLATTE VALLEY ENERGETICS';
         OTHER;
           wk_SLNAME = 'HORNADY MFG COMPANY';
         ENDSL;
       ENDIF;                                                                // wk_SLNAME=*Blanks
      /End-Free
     C                   EVAL      SLSFNM = wk_SLNAME
     C                   EVAL      SLSFA1 = wk_SLADDR1
     C                   IF        wk_SLADDR2 = *Blanks
     C                   EVAL      SLSFA2 = %trimr(wk_SLCITY) + ', ' + wk_SLST +
     C                               ' ' + wk_SLZIP
     C                   EVAL      SLSFA3 = %triml(
     C                               %editw(wk_SLPHON:'0&(   )&   -    '))
     C                   EVAL      %subst(SLSFA3:20:3) = 'USA'
     C                   EVAL      SLSFA4 = *Blanks
     C                   ELSE
     C                   EVAL      SLSFA2 = wk_SLADDR2
     C                   EVAL      SLSFA3 = %trimr(wk_SLCITY) + ', ' + wk_SLST +
     C                               ' ' + wk_SLZIP
     C                   EVAL      SLSFA4 = %triml(
     C                               %editw(wk_SLPHON:'0&(   )&   -    '))
     C                   EVAL      %subst(SLSFA4:20:3) = 'USA'
     C                   ENDIF                                                  wk_SLADDR2=*Blanks
     C                   EVAL      SLJOBN = ps_Job#
     C                   EVAL      SLITEM = ODITEM
     C                   EVAL      SLIMDS = ODIMDS
     C                   IF        OEBLTO = 126820                              Mills Fleet Farm
     C                   EVAL      SLUPC = *Blanks
     C                   ELSE
     C                   IF        not wk_UseCustUPC
     C                   EVAL      SLUPC =
     C                               %subst(ItemBarcode(ODITEM:'U'):3:12)
     C                   ELSE
     C                   EVAL      SLUPC = %subst(ItemBarcodeC(ODITEM:OEBLTO:
     C                                       'H':'U'):3:12)
     C                   ENDIF                                                  not wk_UseCustUPC
     C                   ENDIF                                                  OEBLTO=126820

     C                   EVAL      SLFCQT = wk_CtnQty
     C                   EVAL      SLQTY = wk_PrtlQty
     C                   EVAL      SLUOM = wk_PrtlUOM
     C                   EVAL      SLSRL# = wk_SRL#
     C                   EVAL      SLCTN# = wk_Carton#
     C                   EXSR      $BldCtnText

     C                   IF        %found(HYLSLBL1)
     C                   UPDATE    HYRSLBL
     C                   ELSE
     C                   WRITE     HYRSLBL
     C                   ENDIF                                                  %found(HYLSLBL1)

     C                   IF        wk_PrtExplsLbl  or
     C                               wk_LblType = 'E'
      /Free
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPEDWK
                where EWSRL#=:wk_SRL# and EWBWGT<>0;
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPELD
                  where EDSRL#=:wk_SRL# and EDBWGT<>0;
       ENDIF;                                                                // wk_RcdCnt=*Zero
       IF wk_RcdCnt = *Zero;
         wk_CrtHzLblRcd = *ON;
       ELSE;
         wk_CrtHzLblRcd = *OFF;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free
     C                   IF        ((wk_Foreign  and  wk_STCTRY <> 'CA')  or
     C                              (wk_GDDSHP <> *Zero  and
     C                               wk_DSCTRY <> 'US'  and
     C                               wk_DSCTRY <> 'CA'  and
     C                               wk_DSCTRY <> *Blanks)  or
     C                              wk_HzLbAnyDest  or
     C                              wk_ExplLbl = 'Y') and
     C                              ((%subst(wk_IMPCLS:1:1) = 'A'  and          Lot-controlled ammo
     C                                wk_IMLOT <> 'N')  or
     C                               wk_IMPCLS = 'AAAA'  or                     Rimfire
     C                               wk_IMPCLS = 'AAGC'  or                     Proof & reference
     C****                           %subst(wk_IMPCLS:1:3) = 'FAA'  or          Frontier shoot rifle
     C****                           %subst(wk_IMPCLS:1:3) = 'FAB'  or          Frontier match rifle
     C                               (%subst(wk_IMPCLS:1:1) = 'F'  and          Frontier
     C                                 %subst(wk_IMPCLS:1:2) <> 'FT'  and
     C                                 %subst(wk_IMPCLS:1:3) <> 'FET')  or
     C                               wk_IMPCLS = 'ASAA'  or                     Ammo seconds
     C                               wk_IMPCLS = 'ASAB'  or                     Ammo seconds
     C                               wk_IMPCLS = 'ASAD'  or                     Ammo seconds
     C                               wk_IMPCLS = 'ASAE'  or                     Ammo seconds
     C                               wk_IMPCLS = 'ASAC')                        Ammo seconds
     C                   IF        wk_ExplLbl <> 'N'
     C                   EXSR      $ExplosiveWork
     C                   IF        wk_CrtHzLblRcd
     C                   CALLP(E)  HYR0152 (WrtEDWK:wk_Order#:Wk_Line#:
     C                               wk_Blanket#:ELBWGT:ELHMNQ:ELPWGT:ELUOMW:
     C                               wk_SRL#)
     C                   EVAL      wk_Pgm0152 = *ON
     C                   EVAL      wk_EDWKWritten = *ON
     C                   ENDIF                                                  wk_CrtHzLblRcd
     C                   EVAL      wk_ExplsLbl = Yes
     C                   ENDIF                                                  wk_ExplLbl<>'N'
     C                   ELSE
      *
      * If a bulk ammo item, update explosive label work record, but don't
      * print explosive label (i.e., set "wk_ExplsLbl = No").
      *
     C                   IF        wk_IWFTCL = 'AMMOB'
     C                               or
     C                              (wk_IWFTCL = 'AMMO'  and
     C                               ((wk_STCTRY <> 'US'  and                   Foreign
     C                                 wk_STCTRY <> *Blanks)  or
     C                                (wk_STST = 'AK'  or  wk_STST = 'HI'  or   Alaska & Hawaii
     C                                 wk_STST = 'AS'  or  wk_STST = 'GU'  or   US Territories
     C                                 wk_STST = 'MP'  or  wk_STST = 'PR'  or
     C                                 wk_STST = 'VI')  or
     C****                            (OEDSHP <> *Zero and
     C                                (wk_GDDSHP <> *Zero and
     C                                  (wk_DSST = 'AK'  or  wk_DSST = 'HI'  or Alaska & Hawaii
     C                                   wk_DSST = 'AS'  or  wk_DSST = 'GU'  or US Territories
     C                                   wk_DSST = 'MP'  or  wk_DSST = 'PR'  or
     C                                   wk_DSST = 'VI'))))
     C                   EXSR      $ExplosiveWork
     C                   IF        wk_CrtHzLblRcd
     C                   CALLP(E)  HYR0152 (WrtEDWK:wk_Order#:wk_Line#:
     C                               wk_Blanket#:ELBWGT:ELHMNQ:ELPWGT:ELUOMW:
     C                               wk_SRL#)
     C                   EVAL      wk_Pgm0152 = *ON
     C                   EVAL      wk_EDWKWritten = *ON
     C                   ENDIF                                                  wk_CrtHzLblRcd
     C                   EVAL      wk_ExplsLbl = No
     C                   ELSE
     C                   EVAL      wk_ExplsLbl = No
     C                   ENDIF                                                  wk_IWFTCL='AMMOB'
     C                   ENDIF                                                  wk_Foreign and
     C                   ENDIF                                                  wk_PrtExplsLbl

     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Build shipment carton text.                                      *
     C********************************************************************
     C     $BldCtnText   BEGSR

     C                   SELECT

     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   SELECT
     C                   WHEN      OEBLTO = 59                                  Bass Pro Shops
     C                               or  OEBLTO = 52                            Academy Stores
     C                               or  OEBLTO = 337                           Dick's Sporting Good
     C                               or  OEBLTO = 21419                         Gander Mountain
     C                               or  OEBLTO = 536527                        DSG Field & Stream
     C                   EVAL      SLCCTX = CtnTextPfx + ' ' +
     C                               %triml(%editc(SLCTN#:'4'))
     C***                WHEN      OEBLTO = 21419                               Gander Mountain
     C***                            or  OEBLTO = 337                           Dick's Sporting Good
     C***                EVAL      SLCCTX = CtnTextPfx + ' ' +
     C***                            %triml(%editc(SLCTN#:'4')) +
     C***                            ' OF ____'
     C                   OTHER
     C                   EVAL      SLCCTX = *Blanks
     C                   ENDSL

     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   SELECT
     C                   WHEN      OEBLTO = 3069                                Bass Pro Shops
     C                               or  OEBLTO = 3035                          Academy Stores
     C                               or  OEBLTO = 93                            Dick's Sporting Good
     C                   EVAL      SLCCTX = CtnTextPfx + ' ' +
     C                               %triml(%editc(SLCTN#:'4'))
     C                   OTHER
     C                   EVAL      SLCCTX = *Blanks
     C                   ENDSL

     C                   ENDSL

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Write shipment carton text.                                      *
     C********************************************************************
     CSR   $ExplosiveWorkBEGSR
      * Note: wk_IMUDN1 is the quantity per box.
     C                   IF        not %open(HYLELBL1)
     C                   OPEN      HYLELBL1
     C                   ENDIF                                                  not %open(HYLELBL1)
     C                   IF        not %open(LDLMORD1)
     C                   OPEN      LDLMORD1
     C                   ENDIF                                                  not %open(LDLMORD1)
     C                   IF        not %open(LDLMORD2)
     C                   OPEN      LDLMORD2
     C                   ENDIF                                                  not %open(LDLMORD2)
     C                   IF        not %open(LDPFLRA)
     C                   OPEN      LDPFLRA
     C                   ENDIF                                                  not %open(LDPFLRA)
     C                   IF        not %open(HYPACSL)
     C                   OPEN      HYPACSL
     C                   ENDIF                                                  not %open(HYPACSL)

      *
      * Retrieve this job's HYPELBL work record.  If this job just now
      * started up, the work record won't be found and will be written
      * (instead of updated) below.
      *
     C                   EVAL      kf_JOBN = ps_Job#
     C     kf_JOBN       CHAIN     HYLELBL1

      /Free
       wk_RcdCnt = *Zero;
       wk_CtnItemCnt = *Zero;
       EXEC SQL select count(*), count (distinct S1ITEM)
                 into :wk_RcdCnt, :wk_CtnItemCnt
                from HYPSCWK
                where S1SRL#=:wk_SRL#;
       IF wk_RcdCnt = *Zero;
         EXEC SQL select count(*), count (distinct SCITEM)
                   into :wk_RcdCnt, :wk_CtnItemCnt
                  from HYPSSCC
                  where SCSRL#=:wk_SRL#;
       ENDIF;                                                                // wk_RcdCnt=*Zero
       IF wk_RcdCnt > 1;
         wk_MixedCtn = *ON;
       ELSE;
         wk_MixedCtn = *OFF;
       ENDIF;                                                                // wk_RcdCnt>1
      /End-Free

     C                   IF        wk_MixedCtn
     C                   EVAL      wk_MixCtnAmmo = *OFF
     C                   EVAL      wk_MixCtnSAmmo = *OFF
     C                   EVAL      wk_MixCtn1Shot = *OFF
     C                   EVAL      wk_ELBWGT2 = *Zero
     C                   EVAL      wk_ELHMNQ2 = *Zero
     C                   EVAL      wk_ELPWGT2 = *Zero
     C                   EVAL      wk_Lot#2 = wk_Lot#
     C                   EVAL      wk_CtnUOMQty2 = wk_CtnUOMQty
     C                   IF        not %open(HYLSSCC8)
     C                   OPEN      HYLSSCC8
     C                   ENDIF                                                  not %open(HYLSSCC8)
     C     wk_SRL#       SETLL     HYLSSCC8
     C                   ENDIF                                                  wk_MixedCtn

     C                   DOU       not wk_MixedCtn  or
     C                               wk_MixedCtn  and  %eof(HYLSSCC8)

     C                   IF        wk_MixedCtn
     C     wk_SRL#       READE     HYLSSCC8
     C                   IF        %eof(HYLSSCC8)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC8)
     C                   EVAL      wk_Lot# = SCLOT#
     C                   EVAL      wk_CtnUOMQty = SCUOMQ
     C                   IF        wk_CtnItemCnt > 1
     C                   IF        SCITEM <> ODITEM
     C                   EVAL      ODITEM = SCITEM
      /Free
       EXEC SQL select IMIMDS, IMIMWG
                 into :wk_IMIMDS, :wk_IMIMWG
                from HDIMST
                where IMITEM=:ODITEM;
      /End-Free
     C                   ENDIF                                                  SCITEM<>ODITEM
     C                   SELECT
     C                   WHEN      %subst(wk_IMIMDS:1:4) = 'AMMO'  or
     C                               %subst(wk_IMIMDS:1:8) = 'FRONTIER'
     C                   EVAL      wk_MixCtnAmmo = *ON
     C                   WHEN      %subst(wk_IMIMDS:1:5) = 'SAMMO'  or
     C                               %subst(wk_IMIMDS:1:9) = 'SFRONTIER'
     C                   EVAL      wk_MixCtnSAmmo = *ON
     C                   WHEN      %subst(wk_IMIMDS:1:8) = 'ONE SHOT'  or
     C                               %subst(wk_IMIMDS:1:7) = 'ONESHOT'
     C                   EVAL      wk_MixCtn1Shot = *ON
     C                   ENDSL
     C                   ENDIF                                                  wk_CtnItemCnt>1
     C                   ENDIF                                                  wk_MixedCtn

     C                   EVAL      ELORD# = wk_Order#
     C                   EVAL      ELJOBN = ps_Job#
     C                   EVAL      kf_CAN = ODITEM
     C                   EVAL      kf_CLOT = wk_Lot#
     C     key_SZ        CHAIN     HYPACSL
     C                   IF        %found(HYPACSL)
     C     SZSLOT        CHAIN     LDLMORD1
     C                   ELSE
     C                   IF        %subst(ODITEM:1:1) = 'S'                     Ammo seconds
     C                   EVAL      wk_PrimItm = %subst(ODITEM:2)
     C     wk_PrimItm    SETGT     LDLMORD2
     C                   DOU       %eof(LDLMORD2)
     C     wk_PrimItm    READPE    LDLMORD2
     C                   IF        %eof(LDLMORD2)
     C                   EVAL      OMLOT = *Zero
     C                   LEAVE
     C                   ENDIF                                                  %eof(LDLMORD2)
     C                   IF        %len(%trimr(OMLOT)) <> 7
     C                   ITER
     C                   ENDIF                                                  %len(%trimr(OMLOT))
     C     OMORD         SETGT     LDPFLRA
     C     OMORD         READPE    LDPFLRA
     C                   IF        not %eof(LDPFLRA)  and  FAPCHG > *Zero
     C                   LEAVE
     C                   ENDIF                                                  not %eof(LDPFLRA)
     C                   ENDDO                                                  %eof(LDLMORD2) or
     C     OMLOT         CHAIN     LDLMORD1
     C                   ELSE
     C     wk_Lot#       CHAIN     LDLMORD1
     C                   ENDIF                                                  %subst(ODITEM:2)
     C                   ENDIF                                                  %found(HYPACSL)
     C                   IF        %found(LDLMORD1)
     C     OMORD         SETLL     LDPFLRA
     C                   IF        not %equal(LDPFLRA)
     C                   EXSR      $RtvMfgAmmo
     C                   ENDIF                                                  not %equal(LDPFLRA)
     C     OMORD         SETGT     LDPFLRA
     C                   DOU       %eof(LDPFLRA)  or  FAPCHG <> *Zero
     C     OMORD         READPE    LDPFLRA
     C                   ENDDO                                                  %eof(LDPFLRA) or
     C                   ENDIF                                                  %found(LDLMORD1)
     C                   IF        %found(LDLMORD1)  and  not %eof(LDPFLRA)
     C                   EVAL      wk_Grains = FAPCHG
     C                   ELSE
     C                   EVAL      kf_TABL = 'AMMOPURCH'
     C                   EVAL      kf_KEY1 = ODITEM
     C     key_TD1       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_Grains = TDNF01
     C                   ELSE
     C                   EVAL      wk_Grains = *Zero
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ENDIF                                                  %found(LDLMORD1) and
     C                   EVAL      ELITEM = ODITEM
     C                   EVAL      ELIMDS = wk_IMIMDS
     C                   IF        not wk_Bulk
     C                   EVAL      wk_TempNum = wk_CtnUOMQty * wk_IMIMWG        Box weight - lbs
     C                   ELSE
     C                   EVAL      wk_TempNum = wk_CtnUOMQty * wk_IMUDN4        Box weight - lbs
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL(H)   wk_TempNum = wk_TempNum * wk_LbsToKgCvt      Box weight - kg
     C                   IF        not wk_MixedCtn
     C                   EVAL(H)   ELBWGT = wk_TempNum * 1
     C                   ELSE
     C                   EVAL(H)   wk_ELBWGT = wk_TempNum * 1
     C                   EVAL      wk_ELBWGT2 = wk_ELBWGT2 + wk_ELBWGT
     C                   ENDIF                                                  not wk_MixedCtn
     C                   IF        not wk_Bulk
     C                   EVAL      wk_TempNum = wk_CtnUOMQty * wk_IMIMWG * .96  HAZMAT Net Qty
     C                   ELSE
     C                   EVAL      wk_TempNum = wk_CtnUOMQty * wk_IMUDN4 * .96  HAZMAT Net Qty
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL(H)   wk_TempNum = wk_TempNum * wk_LbsToKgCvt
     C                   IF        not wk_MixedCtn
     C                   EVAL(H)   ELHMNQ = wk_TempNum * 1                       (convert to kg)
     C****               EVAL(H)   ELPWGT = wk_CtnUOMQty * wk_IMUDN1 *          Powder weight
     C****                                    wk_Grains / 7000
     C                   EVAL      ELPWGT = RtvAmmoNEQ (ODITEM:                 Rtv NEQ in kg
     C                                        wk_CtnUOMQty:'TOT':'KG')
     C                   IF        ELPWGT = *Zero  and wk_Grains <> *Zero       s/b purchased only
     C                   EVAL(H)   ELPWGT = wk_CtnUOMQty * wk_IMUDN1 *          Powder weight
     C                                        wk_Grains / 7000
     C                   EVAL(H)   ELPWGT = ELPWGT * wk_LbsToKgCvt               (convert to kg)
     C                   ENDIF                                                  ELPWGT=*Zero and
     C                   IF        ELPWGT = *Zero
     C                   EVAL      ELPWGT = RtvHazWgt(ODITEM:'AMMO':wk_Lot#)
     C                   EVAL      ELPWGT = ELPWGT +
     C                                      RtvHazWgt(ODITEM:'AMMOB':wk_Lot#)
     C                   EVAL(H)   ELPWGT = ELPWGT * wk_LbsToKgCvt               (convert to kg)
     C                   ENDIF                                                  ELPWGT=*Zero
     C****               EVAL(H)   ELPWGT = ELPWGT * wk_LbsToKgCvt               (convert to kg)
     C                   ELSE
     C                   EVAL(H)   wk_ELHMNQ = wk_TempNum * 1                    (convert to kg)
     C                   EVAL      wk_ELHMNQ2 = wk_ELHMNQ2 + wk_ELHMNQ
     C****               EVAL(H)   wk_ELPWGT = wk_CtnUOMQty * wk_IMUDN1 *       Powder weight
     C****                                    wk_Grains / 7000
     C                   EVAL      wk_ELPWGT = RtvAmmoNEQ (ODITEM:              Rtv NEQ in kg
     C                                        wk_CtnUOMQty:'TOT':'KG')
     C                   IF        wk_ELPWGT = *Zero  and wk_Grains <> *Zero    s/b purchased only
     C                   EVAL(H)   wk_ELPWGT = wk_CtnUOMQty * wk_IMUDN1 *       Powder weight
     C                                        wk_Grains / 7000
     C                   EVAL(H)   wk_ELPWGT = wk_ELPWGT * wk_LbsToKgCvt         (convert to kg)
     C                   ENDIF                                                  wk_ELPWGT=*Zero and
     C                   IF        wk_ELPWGT = *Zero
     C                   EVAL      wk_ELPWGT = RtvHazWgt(ODITEM:'AMMO':wk_Lot#)
     C                   EVAL      wk_ELPWGT = wk_ELPWGT +
     C                                      RtvHazWgt(ODITEM:'AMMOB':wk_Lot#)
     C                   EVAL(H)   wk_ELPWGT = wk_ELPWGT * wk_LbsToKgCvt         (convert to kg)
     C                   ENDIF                                                  wk_ELPWGT=*Zero
     C****               EVAL(H)   wk_ELPWGT = wk_ELPWGT * wk_LbsToKgCvt         (convert to kg)
     C                   EVAL      wk_ELPWGT2 = wk_ELPWGT2 + wk_ELPWGT
     C                   ENDIF                                                  not wk_MixedCtn
     C                   EVAL      ELUOMW = 'kg'

     C                   ENDDO                                                  not wk_MixedCtn or

     C                   IF        wk_MixedCtn
     C                   IF        wk_CtnItemCnt > 1
     C                   EVAL      ELITEM = 'MIXED'
     C                   EVAL      ELIMDS = *Blanks
     C                   IF        wk_MixCtnAmmo
     C                   EVAL      ELIMDS = 'AMMO'
     C                   ENDIF                                                  wk_MixCtnAmmo
     C                   IF        wk_MixCtnSAmmo
     C                   EVAL      ELIMDS = %trimr(ELIMDS) + ' ' + 'SAMMO'
     C                   ENDIF                                                  wk_MixCtnSAmmo
     C                   IF        wk_MixCtn1Shot
     C                   EVAL      ELIMDS = %trimr(ELIMDS) + ' ' + 'ONE SHOT'
     C                   ENDIF                                                  wk_MixCtn1Shot
     C                   ENDIF                                                  wk_CtnItemCnt > 1
     C                   EVAL      ELBWGT = wk_ELBWGT2
     C                   EVAL      ELHMNQ = wk_ELHMNQ2
     C                   EVAL      ELPWGT = wk_ELPWGT2
     C                   EVAL      wk_Lot# = wk_Lot#2
     C                   EVAL      wk_CtnUOMQty = wk_CtnUOMQty2
     C                   ENDIF                                                  wk_MixedCtn

     C                   IF        %found(HYLELBL1)
     C                   UPDATE    HYRELBL
     C                   ELSE
     C                   WRITE     HYRELBL
     C                   ENDIF                                                  %found(HYLELBL1)

     CSR                 ENDSR

     C/EJECT
      *****************************************************************
      * Retrieve LDPMORD record for manufactured lot number when      *
      * shipping packaged ammo that originated from bulk.             *
      *****************************************************************
     CSR   $RtvMfgAmmo   BEGSR

     C                   IF        not %open(LDPMODT)
     C                   OPEN      LDPMODT
     C                   ENDIF                                                  not %open(LDPMODT)

     C                   EVAL      wk_OMORD = OMORD

     C     wk_OMORD      SETLL     LDPMODT
     C                   DOU       %eof(LDPMODT)
     C     wk_OMORD      READE     LDPMODT
     C                   IF        %eof(LDPMODT)
     C                   EVAL      OMORD = *Hival
     C                   LEAVE
     C                   ENDIF                                                  %eof(LDPMODT)
     C                   IF        %subst(ODLOT:1:1) = '3'                      Ammo (hopefully mfr)
     C     key_OM        CHAIN     LDLMORD2
     C                   IF        %found(LDLMORD2)
     C     OMORD         SETLL     LDPFLRA
     C                   IF        %equal(LDPFLRA)
     C****               LEAVE
     C                   LEAVESR
     C                   ENDIF                                                  %equal(LDPFLRA)
     C                   ENDIF                                                  %found(LDLMORD2)
     C                   ENDIF                                                  %subst(ODLOT:1:1)='3
     C                   ENDDO                                                  %eof(LDPMODT)

     C                   IF        not %open(LDPMOD3)
     C                   OPEN      LDPMOD3
     C                   ENDIF                                                  not %open(LDPMOD3)

     C     wk_OMORD      SETLL     LDPMOD3
     C                   DOU       %eof(LDPMOD3)
     C     wk_OMORD      READE     LDPMOD3
     C                   IF        %eof(LDPMOD3)
     C                   EVAL      OMORD = *Hival
     C                   LEAVE
     C                   ENDIF                                                  %eof(LDPMOD3)
     C                   IF        %subst(O3LOT:1:1) = '3'                      Ammo (hopefully mfr)
     C     O3LOT         CHAIN     LDLMORD1
     C                   IF        %found(LDLMORD1)
     C     OMORD         SETLL     LDPFLRA
     C                   IF        %equal(LDPFLRA)
     C                   LEAVESR
     C                   ENDIF                                                  %equal(LDPFLRA)
     C                   ENDIF                                                  %found(LDLMORD1)
     C                   ENDIF                                                  %subst(O3LOT:1:1)='3
     C                   ENDDO                                                  %eof(LDPMOD3)

     CSR                 ENDSR

     C/EJECT
      *****************************************************************
      * Print a "HEAVY" label, if required.                           *
      *****************************************************************
     CSR   $PrtHvyLbl    BEGSR

     C                   SELECT

      *
      * Generate a "HEAVY" label for Amazon, if required based on their
      * guidelines.
      *
     C                   WHEN      OEBLTO = 35475                               amazon.com
     C                   IF        not wk_Bulk
     C                   EVAL      wk_HvyLbl = RtvAmzHvy (ODITEM:wk_IMIMWG:
     C                               wk_CtnUOMQty)
     C                   ELSE
     C                   EVAL      wk_HvyLbl = RtvAmzHvy (ODITEM:wk_IMUDN4:
     C                               wk_CtnUOMQty)
     C                   ENDIF                                                  not wk_Bulk
     C                   IF        wk_HvyLbl <> '*NONE'  and
     C                               wk_HvyLbl <> *Blanks
     C                   CALLP(E)  HYR0116 ('OE':*Zero:*Zero:*Zero:
     C                               HeavyAmz:'N':wk_RC0116)
     C                   EVAL      wk_Lbl3 = *ON
     C                   ENDIF                                                  wk_HvyLbl<>'*NONE'

      *
      * For all other customers, retrieve whether to generate "HEAVY" labels.
      *
     C                   OTHER

     C                   EVAL      KF_TABL = 'CUSTOMER'
     C                   EVAL      kf_KEY1 = %editc(OEBLTO:'X')
     C                   EVAL      kf_KEY2 = wk_DatabaseID2
     C     key_TD2       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   IF        TDFF06 = 'Y'
     C                   EVAL      wk_GenHeavyLbl = *ON
     C                   ELSE
     C                   EVAL      wk_GenHeavyLbl = *OFF
     C                   ENDIF                                                  TDFF06='Y'
     C                   ELSE
     C                   EVAL      wk_GenHeavyLbl = *OFF
     C                   ENDIF                                                  %found(GUPTDAT)
     C*
     C* Check the ship to customer to see if they receive a heavy label
     C* This added 9/4/18 because cabelas DCs are supposed to get heavy
     C* labels and the bill to numbers are changed to bass pro.  So we need
     C* to check the ship tos to see if they are cabelas.
     C*
     C                   IF        wk_GenHeavyLbl = *OFF
     C                   EVAL      kf_TABL = 'CUSTHVY'
     C                   EVAL      kf_KEY1 = %editc(OESHTO:'X')
     C                   EVAL      kf_KEY2 = wk_DatabaseID2
     C     key_TD2       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   IF        TDFF06 = 'Y'
     C                   EVAL      wk_GenHeavyLbl = *ON
     C                   ENDIF                                                  TDFF06='Y'
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ENDIF                                                  wk_GenHeavyLbl=*OFF

      *
      * If the carton weighs more than 49 pounds, generate a "HEAVY" label
      * for specified customers.
      *
     C                   IF        wk_GenHeavyLbl
     C****               IF        wk_IMIMWG * wk_CtnUOMQty > 49
     C                   IF        (not wk_Bulk  and
     C                                wk_IMIMWG * wk_CtnUOMQty > 49)  or
     C                             (wk_Bulk  and
     C                                wk_IMUDN4 * wk_CtnUOMQty > 49)
     C                   CALLP(E)  HYR0116 ('OE':*Zero:*Zero:*Zero:
     C                               Heavy:'N':wk_RC0116)
     C                   EVAL      wk_Lbl3 = *ON
     C                   ENDIF                                                  wk_IMIMWG*wk_CtnUOMQ
     C                   ENDIF                                                  wk_GenHeavyLbl

     C                   ENDSL

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Write joblog message - *DIAG                                           *
      **************************************************************************
     CSR   $WrtJobLog    BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDPM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*DIAG' :
     C                               '*' : *Zero : in_MsgKey :
     C                               io_MsgErrCode)

     CSR                 ENDSR

     P PrtUCC128Lbl    E

     P/EJECT
      **************************************************************************
      * Determine if any UCC-128 labels need to include the ship-to phone nbr. *
      **************************************************************************
     P NeedPhone       B
     D                 PI             1N
     D Foreign                        1N   CONST
     D ShipToCtry                     3A   CONST

     D wk_Foreign      S              1N
     D wk_ShipToCtry   S              3A
     D wk_NeedPhone    S              1N

     C                   DO

     C                   EVAL      wk_Foreign = Foreign
     C                   EVAL      wk_ShipToCtry = ShipToCtry
     C                   EVAL      wk_NeedPhone = *OFF

     C                   IF        wk_UCC128Phn = 'N'
     C                   LEAVE
     C                   ENDIF                                                  wk_UCC128Phn='N'

     C                   IF        wk_UCC128Phn = 'Y'
     C                   EVAL      wk_NeedPhone = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_UCC128Phn='N'

     C                   IF        wk_UCC128Phn <> 'F'
     C                   LEAVE
     C                   ENDIF                                                  wk_UCC128Phn<>'F'

     C                   IF        wk_Foreign
     C                   EVAL      wk_NeedPhone = *ON
     C                   LEAVE
     C                   ENDIF                                                  %found(GUPTDAT) and

     C                   IF        (wk_ShipToCtry <> 'US'  and
     C                               wk_ShipToCtry <> *Blanks)
     C                   EVAL      wk_NeedPhone = *ON
     C                   LEAVE
     C                   ENDIF                                                  wk_ShipToCtry<>'US'

     C                   ENDDO

     C                   RETURN    wk_NeedPhone

     P NeedPhone       E

     P/EJECT
      **************************************************************************
      * Determine if an Amazon item needs a "heavy" label.                     *
      **************************************************************************
     P RtvAmzHvy       B
     D                 PI             8A
     D Item#                         15A   CONST
     D UnitWgt                        9P 5 CONST
     D CtnQty                         9P 0 CONST

     D wk_Item#        S             15A
     D wk_UnitWgt      S              9P 5
     D wk_CtnQty       S              9P 0
     D wk_HvyLbl       S              8A
     D wk_CtnWgt       S              9P 2
     D wk_Side1        S             11P 5
     D wk_Side2        S             11P 5
     D wk_Side3        S             11P 5

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_UnitWgt = UnitWgt
     C                   EVAL      wk_CtnQty = CtnQty
     C                   EVAL      wk_HvyLbl = '*NONE'

     C                   EVAL(H)   wk_CtnWgt = wk_CtnQty * wk_UnitWgt

      /Free
       wk_Side1 = *Zero;
       wk_Side2 = *Zero;
       wk_Side3 = *Zero;
       EXEC SQL select dec(imuda2,11,5), dec(imuda3,11,5), dec(imuda4,11,5)
                  into :wk_Side1, :wk_Side2, :wk_Side3
                from HDIMST
                where IMITEM=:wk_Item#;
      /End-Free

     C                   IF        wk_Side1 > 25  or  wk_Side2 > 25  or
     C                               wk_Side3 > 25
     C                   IF        wk_CtnWgt > 35
     C                   EVAL      wk_HvyLbl = 'DC-HEAMZ'
     C                   ENDIF                                                  wk_CtnWgt>35
     C                   ELSE
     C                   IF        wk_CtnQty > 1
     C                   IF        wk_CtnWgt > 50
     C                   EVAL      wk_HvyLbl = 'DC-HEAMZ'
     C                   ENDIF                                                  wk_CtnWgt>50
     C                   ELSE
     C                   IF        wk_CtnWgt > 50
     C                   EVAL      wk_HvyLbl = 'DC-HEAMZ'
     C                   ENDIF                                                  wk_CtnWgt>50
     C                   ENDIF                                                  wk_CtnQty>1
     C                   ENDIF                                                  wk_Side1>25 or

     C                   ENDDO

     C                   RETURN    wk_HvyLbl

     P RtvAmzHvy       E

     P/EJECT
      **************************************************************************
      * Retrieve workstation location.                                         *
      **************************************************************************
     P RtvWSLoc        B
     D                 PI             4A

     D wk_WSLoc        S              4A

     C                   DO

     C                   EVAL      kf_TABL = 'ALDAWKSTN'
     C                   EVAL      kf_KEY1 = ps_Job
     C     Key_TD1       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_WSLoc = 'ALDA'
     C                   LEAVE
     C                   ENDIF                                                  %found(GUPTDAT)

     C                   EVAL      kf_TABL = 'GIWKSTN'
     C                   EVAL      kf_KEY1 = ps_Job
     C     Key_TD1       CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_WSLoc = 'GI'
     C                   LEAVE
     C                   ENDIF                                                  %found(GUPTDAT)

     C                   EVAL      wk_WSLoc = 'WEST'

     C                   ENDDO

     C                   RETURN    wk_WSLoc

     P RtvWSLoc        E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR0622      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     D ShutDown        C                   '9'

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        wk_Pgm0152                                   HYR0152 called
     C                   CALLP(E)  HYR0152 (ShutDown:*Zero:*Zero:
     C                               *Zero:*Zero:*Zero:*Zero:*Blanks:
     C                               *Zero)
     C                   ENDIF                                                  wk_Pgm0152

     C                   IF        wk_PgmEDR9900                                EDR9900 called
     C                   CALLP(E)  sd_EDR9900
     C                   ENDIF                                                  wk_PgmEDR9900

     C                   IF        %open(OEORHD)
     C                   CLOSE     OEORHD
     C                   ENDIF                                                  %open(ORORHD)

     C                   IF        %open(OEORDT)
     C                   CLOSE     OEORDT
     C                   ENDIF                                                  %open(ORORDT)

     C                   IF        %open(HYPSGCU)
     C                   CLOSE     HYPSGCU
     C                   ENDIF                                                  %open(HYPSGCU)

     C                   IF        %open(HYLSLBL1)
     C                   CLOSE     HYLSLBL1
     C                   ENDIF                                                  %open(HYLSLBL1)

     C                   IF        %open(HYLELBL1)
     C                   CLOSE     HYLELBL1
     C                   ENDIF                                                  %open(HYLELBL1)

     C                   IF        %open(LDLMORD1)
     C                   CLOSE     LDLMORD1
     C                   ENDIF                                                  %open(LDLMORD1)

     C                   IF        %open(LDLMORD2)
     C                   CLOSE     LDLMORD2
     C                   ENDIF                                                  %open(LDLMORD2)

     C                   IF        %open(LDPMODT)
     C                   CLOSE     LDPMODT
     C                   ENDIF                                                  %open(LDPMODT)

     C                   IF        %open(LDPMOD3)
     C                   CLOSE     LDPMOD3
     C                   ENDIF                                                  %open(LDPMOD3)

     C                   IF        %open(LDPFLRA)
     C                   CLOSE     LDPFLRA
     C                   ENDIF                                                  %open(LDPFLRA)

     C                   IF        %open(HYPACSL)
     C                   CLOSE     HYPACSL
     C                   ENDIF                                                  %open(HYPACSL)

     C                   IF        %open(HYLSSCC8)
     C                   CLOSE     HYLSSCC8
     C                   ENDIF                                                  %open(HYLSSCC8)

     C                   IF        %open(GUPTDAT)
     C                   CLOSE     GUPTDAT
     C                   ENDIF                                                  %open(GUPTDAT)

     C                   IF        SLORD# <> *Zero
      /Free
       EXEC SQL delete from HYPSLBL
                where SLORD#=:SLORD# and SLJOBN=:ps_Job#;
      /End-Free
     C                   ENDIF                                                  SLORD#<>*Zero

     C                   IF        ELORD# <> *Zero
      /Free
       EXEC SQL delete from HYPELBL
                where ELORD#=:ELORD# and ELJOBN=:ps_Job#;
      /End-Free
     C                   ENDIF                                                  ELORD#<>*Zero

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR0622      E

     P/EJECT
