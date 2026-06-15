     H COPYRIGHT('(C) Copyright Hornady Manufacturing Company 2004')
      **************************************************************************
      * TITLE: Write HYPSCWK Records and Copy to HYPSSCC
      *    BY: William Earl (MCA)
      *  DATE: May 2004
      *
      *   REV DATE   BY   CHANGE
      *  ----------  ---  -----------------------------------------------------
      *  11/01/2004  MCA  Add shipment carton number.
      *  01/12/2005  MCA  HD4.2 upgrade.  IMUPC field length was changed from
      *                   13,0 to 14,0.
      *  02/24/2009  MCA  Changes to utilize AS/400 job number field in work
      *                   file records.  (This was changed to enable mulitple
      *                   user shipping.)
      *  08/27/2009  MCA  Add employee number to serial number files.
      *  09/14/2009  MCA  Add function to update HYPSCWK order shipments that
      *                   did not print labels.
      *  12/16/2010  MCA  Update pallet-level data.
      *  09/14/2011  MCA  Handle temporary employee agency numbers.
      *  05/20/2013  MCA  Add turnaround number to UCC-128 files.
      *  03/20/2021  MCA  Add actual shipping location, invoice date, and
      *                   shipment type to HYPSSCC.
      *  08/19/2025  MCA  Handle LBS UOM shipments. This was done for a powder
      *                   shipment (which is extremely unusual).
      *
      **************************************************************************
      *
     FHYPSCWK   UF A E           K DISK
     FOEORDT    IF   E           K DISK
     FHDIMST    IF   E           K DISK
     FGUPTDAT   IF   E           K DISK
     FHYPSSCC   UF A E           K DISK    USROPN

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D                SDS
     D ps_Wkstn              244    253
     D ps_JobNumber          264    269

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D in_Function     S              1A
     D in_Serial#      S                   LIKE(S1SRL#)
     D in_Carton#      S                   LIKE(S1CTN#)
     D in_Order#       S                   LIKE(S1ORD#)
     D in_OrdLin#      S                   LIKE(S1ORL#)
     D in_Blanket#     S                   LIKE(S1BLN#)
     D in_ShipQty      S                   LIKE(S1UOMQ)
     D in_Lot#         S                   LIKE(S1LOT#)
     D in_EmpType      S                   LIKE(S1EMTY)
     D in_Emply#       S                   LIKE(S1EMPL)
     D in_Turn#        S                   LIKE(S1TURN)
     D wk_Function     S                   LIKE(in_Function)
     D wk_Serial#      S                   LIKE(S1SRL#)
     D wk_Carton#      S                   LIKE(S1CTN#)
     D wk_Order#       S                   LIKE(S1ORD#)
     D wk_OrdLin#      S                   LIKE(S1ORL#)
     D wk_Blanket#     S                   LIKE(S1BLN#)
     D wk_ShipQty      S                   LIKE(S1UOMQ)
     D wk_Lot#         S                   LIKE(S1LOT#)
     D wk_EmpType      S                   LIKE(S1EMTY)
     D wk_Emply#       S                   LIKE(S1EMPL)
     D wk_Turn#        S                   LIKE(S1TURN)
     D wk_Table        S                   LIKE(TDTABL)
     D wk_TblKey1      S                   LIKE(TDKEY1)
     D wk_BulkItem     S              1A   INZ(Yes)
     D wk_DCLoc        S              4A
     D sv_ORL#         S                   LIKE(S1ORL#)
     D sv_BLN#         S                   LIKE(S1BLN#)
     D sv_LOT#         S                   LIKE(S1LOT#)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Yes             C                   'Y'
     D No              C                   'N'
     D WrtSCWK         C                   '1'
     D BldSSCC         C                   '2'
     D ClrSCWK         C                   '3'
     D UpdNoLbl        C                   '4'
     D ShutDown        C                   '9'
     D tbn_BulkPrdCls  C                   'BULKPRDCLS'
     D Full            C                   'F'

      *****************************************************************
      * Define data areas.
      *****************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D SSCCSRLREF      S              9P 0 DTAARA

     C/EJECT

      **************************************************************************
      * Mainline.                                                              *
      **************************************************************************

     C                   SELECT
     C                   WHEN      wk_Function = WrtSCWK
     C                   EXSR      $WrtSCWK
     C                   WHEN      wk_Function = BldSSCC
     C                   EXSR      $BldSSCC
     C                   WHEN      wk_Function = ClrSCWK
     C                   EXSR      $ClrSCWK
     C                   WHEN      wk_Function = UpdNoLbl
     C                   EXSR      $UpdNoLbl
     C                   WHEN      wk_Function = Shutdown
     C                   EVAL      *INLR = *ON
     C                   ENDSL

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Write HYPSCWK record.                                                  *
      **************************************************************************
     CSR   $WrtSCWK      BEGSR

     C     key_OD        CHAIN     OEORDT
     C                   IF        not %found(OEORDT)
     C                   EVAL      ODITEM = *Blanks
     C                   EVAL      ODIMDS = *Blanks
     C                   EVAL      ODPRUM = *Blanks
     C                   EVAL      ODUDA3 = *Blanks
     C                   ENDIF                                                  not %found(OEORDT)
     C     ODITEM        CHAIN     HDIMST
     C                   IF        not %found(HDIMST)
     C                   EVAL      IMUDN1 = *Zero
     C                   EVAL      IMUDN4 = *Zero
     C                   EVAL      IMUOMW = *Blanks
     C                   EVAL      IMIMWG = *Zero
     C                   EVAL      IMUPC = *Zero
     C                   EVAL      IMPCLS = *Blanks
     C                   ENDIF                                                  not %found(HDIMST)
     C                   EVAL      wk_Table = tbn_BulkPrdCls
     C                   EVAL      wk_TblKey1 = IMPCLS
     C     key_TD        CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_BulkItem = Yes
     C                   ELSE
     C                   EVAL      wk_BulkItem = No
     C                   ENDIF                                                  %found(GUPTDAT)
     C     key_S1        CHAIN     HYPSCWK
     C                   IF        %found(HYPSCWK)
     C                   EVAL      S1UOMQ = S1UOMQ + wk_ShipQty
     C                   ELSE
     C                   EVAL      S1ORD# = wk_Order#
     C                   EVAL      S1SLOC = ODUDA3
     C                   EVAL      S1ORL# = wk_OrdLin#
     C                   EVAL      S1BLN# = wk_Blanket#
     C                   EVAL      S1SRL# = wk_Serial#
     C                   EVAL      S1CTN# = wk_Carton#
     C                   EVAL      S1ITEM = ODITEM
     C                   EVAL      S1IMDS = ODIMDS
     C                   EVAL      S1LOT# = wk_Lot#
     C                   EVAL      S1EMTY = wk_EmpType
     C                   EVAL      S1EMPL = wk_Emply#
     C                   EVAL      S1UOM = ODPRUM
     C                   EVAL      S1UOMQ = wk_ShipQty
     C                   EVAL      S1QPER = IMUDN1
     C                   EVAL      S1UOMW = IMUOMW
     C                   IF        wk_BulkItem = Yes
     C                   EVAL      S1UWGT = IMUDN4
     C                   ELSE
     C                   EVAL      S1UWGT = IMIMWG
     C                   ENDIF                                                  wk_BulkItem=Yes
     C                   IF        IMUPC = *Zero
     C                   EVAL      S1UPC = *Blanks
     C                   ELSE
     C***                EVAL      S1UPC = %triml(%editw(IMUPC:'0            '))
     C                   EVAL      S1UPC =
     C                               %triml(%editw(IMUPC:' 0            '))
     C                   ENDIF                                                  IMUPC=*Zero
     C                   EVAL      S1PSRL# = *Zero
     C                   EVAL      S1PEMTY = *Blank
     C                   EVAL      S1PEMPL = *Zero
     C                   ENDIF                                                  %found(HYPSCWK)
     C                   EVAL      S1EAQ = S1UOMQ * S1QPER
     C                   SELECT
     C                   WHEN      S1UOM = S1UOMW
     C                   IF        S1UOMQ < 99999
     C                   EVAL      S1EWGT = S1UOMQ
     C                   ELSE
     C                   EVAL      S1EWGT = 99999
     C                   ENDIF                                                  S1UOMQ<99999
     C                   OTHER
     C                   EVAL(H)   S1EWGT = S1UWGT * S1UOMQ
     C                   ENDSL
     C                   IF        wk_Serial# = *Zero
     C                   EVAL      S1LTYP = *Blank
     C                   ELSE
     C                   EVAL      S1LTYP = Full
     C                   ENDIF                                                  S1LTYP=*Blank
     C                   EVAL      S1TURN = wk_Turn#
     C                   EVAL      S1TSTP = %timestamp

     C                   IF        %found(HYPSCWK)
     C                   UPDATE    HYRSCWK
     C                   ELSE
     C                   EVAL      S1JOBN = ps_JobNumber
     C                   WRITE     HYRSCWK
     C                   ENDIF                                                  %found(HYPSCWK)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Build HYPSSCC records from HYPSCWK records.                            *
      **************************************************************************
     CSR   $BldSSCC      BEGSR

     C                   IF        not %open(HYPSSCC)
     C                   OPEN      HYPSSCC
     C                   ENDIF                                                  not %open(HYPSSCC)

     C     wk_Order#     SETLL     HYPSCWK
     C                   DOU       %eof(HYPSCWK)
     C     wk_Order#     READE     HYPSCWK
     C                   IF        %eof(HYPSCWK)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSCWK)
     C     key_SC        CHAIN     HYPSSCC
     C                   IF        %found(HYPSSCC)
     C                   EVAL      SCUOMQ = SCUOMQ + S1UOMQ
     C                   EVAL      SCEAQ = SCEAQ + S1EAQ
     C                   EVAL      SCEWGT = SCEWGT + S1EWGT
     C                   EVAL      SCPSRL# = S1PSRL#
     C                   EVAL      SCPEMTY = S1PEMTY
     C                   EVAL      SCPEMPL = S1PEMPL
     C                   UPDATE    HYRSSCC
     C                   ELSE
     C                   EXSR      $MoveS1toSC
     C                   WRITE     HYRSSCC
     C                   ENDIF                                                  %found(HYPSSCC)
     C                   DELETE    HYRSCWK
     C                   ENDDO                                                  %eof(HYPSCWK)

     C                   FEOD      HYPSSCC

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Clear HYPSCWK records for order.                                       *
      **************************************************************************
     CSR   $ClrSCWK      BEGSR

     C     wk_Order#     SETLL     HYPSCWK
     C                   DOU       %eof(HYPSCWK)
     C     wk_Order#     READE     HYPSCWK
     C                   IF        %eof(HYPSCWK)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSCWK)
     C                   DELETE    HYRSCWK
     C                   ENDDO                                                  %eof(HYPSCWK)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Update HYPSCWK records for order shipments without labels.             *
      * NOTE: All records will be assigned the same serial number, unless      *
      *       doing so would create duplicate records when copied to HYPSSCC.  *
      **************************************************************************
     CSR   $UpdNoLbl     BEGSR

     C                   EVAL      sv_ORL# = *Hival
     C                   EVAL      sv_BLN# = *Hival
     C                   EVAL      sv_LOT# = *Hival
     C                   EVAL      wk_Serial# = *Zero
     C     wk_Order#     SETLL     HYPSCWK
     C                   DOU       %eof(HYPSCWK)
     C     wk_Order#     READE     HYPSCWK
     C                   IF        %eof(HYPSCWK)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSCWK)
     C                   IF        S1SRL# <> *Zero
     C                   UNLOCK    HYPSCWK
     C                   ITER
     C                   ENDIF                                                  S1SRL#<>*Zero
     C                   IF        (S1ORL# = sv_ORL#  and  S1BLN# = sv_BLN#  and
     C                               S1LOT# = sv_LOT#)  or  wk_Serial# = *Zero
     C                   EXSR      $RtvSerial#
     C                   ENDIF                                                  S1ORL#=sv_ORL# and
     C                   EVAL      sv_ORL# = S1ORL#
     C                   EVAL      sv_BLN# = S1BLN#
     C                   EVAL      sv_LOT# = S1LOT#
     C                   EVAL      S1SRL# = wk_Serial#
     C                   UPDATE    HYRSCWK
     C                   ENDDO                                                  %eof(HYPSCWK)

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Move HYPSCWK fields (S1) to HYPSSCC fields (SC).                       *
      **************************************************************************
     CSR   $MoveS1toSC   BEGSR

     C     key_OD2       CHAIN     OEORDT
     C                   IF        not %found(OEORDT)
     C                   EVAL      ODPO# = *Zero
     C                   EVAL      ODDSHP = 'N'
     C                   ENDIF                                                  not %found(OEORDT)

     C                   EVAL      SCSRL# = S1SRL#
     C                   EVAL      SCORD# = S1ORD#
     C                   EVAL      SCORL# = S1ORL#
     C                   EVAL      SCBLN# = S1BLN#
     C                   EVAL      SCITEM = S1ITEM
     C                   EVAL      SCIMDS = S1IMDS
     C                   EVAL      SCLOT# = S1LOT#
     C                   EVAL      SCUOM = S1UOM
     C                   EVAL      SCUOMQ = S1UOMQ
     C                   EVAL      SCQPER = S1QPER
     C                   EVAL      SCEAQ = S1EAQ
     C                   EVAL      SCUOMW = S1UOMW
     C                   EVAL      SCUWGT = S1UWGT
     C                   EVAL      SCEWGT = S1EWGT
     C                   EVAL      SCUPC = S1UPC
     C                   EVAL      SCCTN# = S1CTN#
     C                   EVAL      SCTURN = S1TURN
     C                   EVAL      SCPSRL# = S1PSRL#
     C                   EVAL      SCPEMTY = S1PEMTY
     C                   EVAL      SCPEMPL = S1PEMPL
     C                   EVAL      SCBOL# = *Zero
     C                   EVAL      SCEMTY = S1EMTY
     C                   EVAL      SCEMPL = S1EMPL
     C                   IF        ODPO# <> *Zero  and  ODDSHP = 'Y'
     C                   EVAL      SCALOC = 'DS'
     C                   ELSE
     C                   EVAL      SCALOC = wk_DCLoc
     C                   ENDIF                                                  ODPO#<>*Zero and
     C                   EVAL      SCIVDT = *Loval
     C                   EVAL      SCSTYP = *Blank
     C                   EVAL      SCTSTP = S1TSTP

     CSR                 ENDSR

     C/EJECT
      *****************************************************************
      * Retrieve next serial number.
      *****************************************************************
     CSR   $RtvSerial#   BEGSR

     C     *LOCK         IN        SSCCSRLREF
     C                   EVAL      SSCCSRLREF = SSCCSRLREF + 1
     C                   OUT       SSCCSRLREF
     C                   EVAL      wk_Serial# = SSCCSRLREF

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *INZSR        BEGSR

     C     *ENTRY        PLIST
     C     wk_Function   PARM                    in_Function
     C     wk_Serial#    PARM                    in_Serial#
     C     wk_Carton#    PARM                    in_Carton#
     C     wk_Order#     PARM                    in_Order#
     C     wk_OrdLin#    PARM                    in_OrdLin#
     C     wk_Blanket#   PARM                    in_Blanket#
     C     wk_ShipQty    PARM                    in_ShipQty
     C     wk_Lot#       PARM                    in_Lot#
     C     wk_EmpType    PARM                    in_EmpType
     C     wk_Emply#     PARM                    in_Emply#
     C     wk_Turn#      PARM                    in_Turn#

     C     key_OD        KLIST                                                  OEORDT
     C                   KFLD                    wk_Order#
     C                   KFLD                    wk_OrdLin#
     C                   KFLD                    wk_Blanket#

     C     key_OD2       KLIST                                                  OEORDT
     C                   KFLD                    S1ORD#
     C                   KFLD                    S1ORL#
     C                   KFLD                    S1BLN#

     C     key_TD        KLIST                                                  GUPTDAT
     C                   KFLD                    wk_Table
     C                   KFLD                    wk_TblKey1

     C     key_S1        KLIST                                                  HYPSCWK
     C                   KFLD                    wk_Order#
     C                   KFLD                    ODUDA3
     C                   KFLD                    wk_OrdLin#
     C                   KFLD                    wk_Blanket#
     C                   KFLD                    wk_Lot#
     C                   KFLD                    wk_Serial#

     C     key_SC        KLIST                                                  HYPSSCC
     C                   KFLD                    S1SRL#
     C                   KFLD                    S1ORD#
     C                   KFLD                    S1ORL#
     C                   KFLD                    S1BLN#
     C                   KFLD                    S1LOT#

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID

      *
      * Determine workstation's DC location.
      *
     C                   IF        wk_DatabaseID = 'PV'  or
     C                               wk_DatabaseID = 'PX'
     C                   EVAL      wk_DCLoc = 'PVE'
     C                   ELSE
     C                   EVAL      wk_Table = 'ALDAWKSTN'
     C                   EVAL      wk_TblKey1 = ps_Wkstn
     C     key_TD        CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_DCLoc = 'ALDA'
     C                   ELSE
     C                   EVAL      wk_Table = 'GIWKSTN'
     C                   EVAL      wk_TblKey1 = ps_Wkstn
     C     key_TD        CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_DCLoc = 'GI'
     C                   ELSE
     C                   EVAL      wk_DCLoc = 'WEST'
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ENDIF                                                  wk_DatabaseID='PV'

     CSR                 ENDSR
     C/EJECT
