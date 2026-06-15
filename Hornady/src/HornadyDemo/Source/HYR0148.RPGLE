     H COPYRIGHT('(C) Copyright Hornady Manufacturing Company 2004')
     H BNDDIR('HYR0148')
      **************************************************************************
      * TITLE: Add record to EDI 856 tickler file
      *    BY: William Earl (MCA)
      *  DATE: June 2004
      *
      *   REV DATE   BY   CHANGE
      *  ----------  ---  -----------------------------------------------------
      *  03/26/2007  MCA  Write tickler records only if at least 1 "E" order is
      *                   on the bill-of-lading.
      *  02/14/2020  MCA  Skip ASN for drop shipment (if specified for customer).
      *                   Note - this was done for Bass Pro.
      *
      **************************************************************************
      *
     FEDOTPM    IF   E           K DISK
     FHYLBOL1   IF   E           K DISK    USROPN
     FOEBOLH    IF   E           K DISK    USROPN
     FOEORHD    IF   E           K DISK    USROPN
     FGUPTDAT   IF   E           K DISK    USROPN
     FEO56TK    UF   E           K DISK    USROPN

     D in_BOL#         S              8P 0
     D in_ShipToCust   S                   LIKE(EOSHTO)
     D in_PriorBOL#    S              8P 0
     D in_ShutDown     S              1N
     D wk_BOL#         S                   LIKE(in_BOL#)
     D wk_ShipToCust   S                   LIKE(EOSHTO)
     D wk_PriorBOL#    S                   LIKE(in_PriorBOL#)
     D wk_ShutDown     S                   LIKE(in_ShutDown)
     D wk_DatabaseID2  S              2A                                        Database ID 2
     D wk_TranSetID    S                   LIKE(EO0143) INZ('856')
     D wk_Error        S              1A
     D kf_TABL         S                   LIKE(TDTABL)
     D kf_KEY1         S                   LIKE(TDKEY1)
     D kf_KEY2         S                   LIKE(TDKEY2)

      **************************************************************************
      * Define constants.
      **************************************************************************
     D Yes             C                   'Y'
     D No              C                   'N'

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID

     C/EJECT

     C                   SELECT

     C                   WHEN      wk_ShutDown = *ON
     C                   EVAL      *INLR = *ON

     C                   OTHER
     C                   EXSR      $EDI856

     C                   ENDSL

     C                   RETURN

     C/EJECT
      **************************************************************************
      * Add record to EDI 856 tickler file.
      **************************************************************************
     CSR   $EDI856       BEGSR

      *
      * If customer is not set up for ASN in the EDI outbound trading partner
      * master file, leave subroutine.
      *
     C     key_EO        CHAIN     EDOTPM
     C                   IF        not %found(EDOTPM)
     C                   LEAVESR
     C                   ENDIF                                                  not %found(EDOTPM)

      *
      * Skip ASN for drop shipment (if specified for customer).
      *
     C                   IF        not %open(OEBOLH)
     C                   OPEN      OEBOLH
     C                   ENDIF                                                  not %open(OEBOLH)
     C                   IF        not %open(GUPTDAT)
     C                   OPEN      GUPTDAT
     C                   ENDIF                                                  not %open(GUPTDAT)
     C     wk_BOL#       CHAIN     OEBOLH
     C                   IF        not %found(OEBOLH)
     C                   EVAL      SDBLTO = wk_ShipToCust
     C                   EVAL      SDDSHP = *Zero
     C                   ENDIF                                                  not %found(OEBOLH)
     C                   IF        SDDSHP <> *Zero
     C                   EVAL      kf_TABL = 'EDI856'
     C                   EVAL      kf_KEY1 = %editc(SDBLTO:'X')
     C                   EVAL      kf_KEY2 = wk_DatabaseID2
     C     key_TD        CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)  and  TDFF02 = 'N'
     C                   LEAVESR                                                <--- Skip ASN
     C                   ENDIF                                                  %found(GUPTDAT) and
     C                   ENDIF                                                  SDDSHP<>*Zero

      *
      * If all orders on the bill-of-lading are not 'E' type, leave
      * subroutine.
      *
     C                   IF        not %open(HYLBOL1)
     C                   OPEN      HYLBOL1
     C                   ENDIF                                                  not %open(HYLBOL1)
     C                   IF        not %open(OEORHD)
     C                   OPEN      OEORHD
     C                   ENDIF                                                  not %open(OEORHD)
     C     wk_BOL#       SETLL     HYLBOL1
     C                   DOU       %eof(HYLBOL1)
     C     wk_BOL#       READE     HYLBOL1
     C                   IF        %eof(HYLBOL1)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLBOL1)
     C     BIORD#        CHAIN     OEORHD
     C                   IF        %found(OEORHD)  and  OEORTY = 'E'
     C                   LEAVE
     C                   ENDIF                                                  %found(OEORHD) and
     C                   ENDDO                                                  %eof(HYLBOL1)
     C                   IF        %eof(HYLBOL1)
     C                   LEAVESR
     C                   ENDIF                                                  %eof(HYLBOL1)

      *
      * If the bill-of-lading is a replacement, remove the prior EDI856 tickler
      * record.
      *
     C                   IF        wk_PriorBOL# <> *Zero
     C                   IF        not %open(EO56TK)
     C                   OPEN      EO56TK
     C                   ENDIF                                                  not %open(EO56TK)
     C     wk_PriorBOL#  CHAIN     EO56TK
     C                   IF        %found(EO56TK)
     C                   DELETE    EO56TKR
     C                   ENDIF                                                  %found(EO56TK)
     C                   ENDIF                                                  wk_PriorBOL#<>*Zero

      *
      * Add record to EDI 856 (advance ship notice) tickler file.
      *
     C                   CALLB     'HEO56G'      HEO56G

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Initialize program.
      **************************************************************************
     CSR   *INZSR        BEGSR

     C     *ENTRY        PLIST
     C     wk_BOL#       PARM                    in_BOL#
     C     wk_ShipToCust PARM                    in_ShipToCust
     C     wk_PriorBOL#  PARM                    in_PriorBOL#
     C     wk_ShutDown   PARM                    in_ShutDown

     C     HEO56G        PLIST
     C                   PARM                    wk_BOL#
     C                   PARM                    wk_Error

     C     key_EO        KLIST                                                  EDOTPM
     C                   KFLD                    wk_TranSetID
     C                   KFLD                    wk_ShipToCust

     C     key_TD        KLIST                                                  GUPTDAT
     C                   KFLD                    kf_TABL
     C                   KFLD                    kf_KEY1
     C                   KFLD                    kf_KEY2

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID

      *
      * Determine database ID for key to master data, e.g., generic tables.
      *
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

     CSR                 ENDSR
     C/EJECT
