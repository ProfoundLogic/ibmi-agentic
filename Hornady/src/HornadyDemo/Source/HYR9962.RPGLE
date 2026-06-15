     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2017')
     H nomain
      **************************************************************************
      * APPLICATION: Miscellaneous                                             *
      *       TITLE: Subprocedures - Customer Item Barcode                     *
      *          BY: William Earl (MCA)                                        *
      *        DATE: December 2017                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  03/18/2020  MCA  In HMCItemFrmCust, also check for customer's UCC-14. *
      *                                                                        *
      **************************************************************************

     FBARCUST   IF   E           K DISK    USROPN
     FBARCUSTL2 IF   E           K DISK    RENAME(BARCUSTR:BARCUSTR2) USROPN
     FBARCUSTL3 IF   E           K DISK    RENAME(BARCUSTR:BARCUSTR3) USROPN
     FBARCUSTL4 IF   E           K DISK    RENAME(BARCUSTR:BARCUSTR4) USROPN
     FBARCUSTL5 IF   E           K DISK    RENAME(BARCUSTR:BARCUSTR5) USROPN
     FBARCUSTL6 IF   E           K DISK    RENAME(BARCUSTR:BARCUSTR6) USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9962

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D kf_ITEM         S             15A
     D kf_CITEM        S             30A
     D kf_CUST         S              7P 0

      **************************************************************************
      * Define key lists.                                                      *
      **************************************************************************
     C     key_CB1       KLIST                                                  BARCUST
     C                   KFLD                    kf_ITEM
     C                   KFLD                    kf_CUST

     C     key_CB2       KLIST                                                  BARCUSTL6
     C                   KFLD                    kf_CITEM
     C                   KFLD                    kf_CUST

     P/EJECT
      **************************************************************************
      * Retrieve whether a customer-level record exists for the item and cust. *
      *  Return values:  *OFF or *ON if a customer-level record exists.        *
      **************************************************************************
     P CustLvlBC       B                   EXPORT

     D                 PI             1N
     D Item#                         15A   CONST
     D Cust#                          7P 0 CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             15A
     D wk_Cust#        S              7P 0
     D wk_Exists       S              1N

      *
      * Determine whether a customer-level record exists.
      *
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Cust# = Cust#
     C                   EVAL      wk_Exists = *OFF

     C                   DO

     C                   IF        not %open(BARCUST)
     C                   OPEN      BARCUST
     C                   ENDIF                                                  not %open(BARCUST)
     C                   EVAL      kf_ITEM = wk_Item#
     C                   EVAL      kf_CUST = wk_Cust#
     C     key_CB1       SETLL     BARCUST
     C                   IF        %equal(BARCUST)
     C                   EVAL      wk_Exists = *ON
     C                   ENDIF                                                  %equal(BARCUST)

     C                   ENDDO

      *
      * Return whether a customer-level record exists.
      *
     C                   RETURN    wk_Exists

     P CustLvlBC       E

     P/EJECT
      **************************************************************************
      * Retrieve a customer item number from a Hornady item number.            *
      **************************************************************************
     P CustItemFrmHMC  B                   EXPORT

     D                 PI            30A
     D HMCItem#                      15A   CONST
     D Cust#                          7P 0 CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_HMCItem#     S             15A
     D wk_Cust#        S              7P 0
     D wk_CustItem#    S             30A

      *
      * Retrieve the customer item number.
      *
     C                   EVAL      wk_HMCItem# = HMCItem#
     C                   EVAL      wk_Cust# = Cust#
     C                   EVAL      wk_CustItem# = *Blanks

     C                   DO

     C                   IF        not %open(BARCUST)
     C                   OPEN      BARCUST
     C                   ENDIF                                                  not %open(BARCUST)
     C                   EVAL      kf_ITEM = wk_HMCItem#
     C                   EVAL      kf_CUST = wk_Cust#
     C     key_CB1       CHAIN     BARCUST
     C                   IF        %found(BARCUST)  and  CBCITEM <> wk_HMCItem#
     C                   EVAL      wk_CustItem# = CBCITEM
     C                   ENDIF                                                  %found(BARCUST)

     C                   ENDDO

      *
      * Return the customer item number.
      *
     C                   RETURN    wk_CustItem#

     P CustItemFrmHMC  E

     P/EJECT
      **************************************************************************
      * Retrieve a Hornady item number from a customer item number.            *
      **************************************************************************
     P HMCItemFrmCust  B                   EXPORT

     D                 PI            15A
     D CustItem#                     30A   CONST
     D Cust#                          7P 0 CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_CustItem#    S             30A
     D wk_Cust#        S              7P 0
     D wk_HMCItem#     S             15A
     D wk_UCCMC        S             14A

      *
      * Retrieve the Hornady item number.
      *
     C                   EVAL      wk_CustItem# = CustItem#
     C                   EVAL      wk_Cust# = Cust#
     C                   EVAL      wk_HMCItem# = *Blanks

     C                   DO

     C                   IF        not %open(BARCUSTL3)
     C                   OPEN      BARCUSTL3
     C                   ENDIF                                                  not %open(BARCUSTL3)
     C                   IF        not %open(BARCUSTL6)
     C                   OPEN      BARCUSTL6
     C                   ENDIF                                                  not %open(BARCUSTL6)
     C                   EVAL      kf_CITEM = wk_CustItem#
     C                   EVAL      kf_CUST = wk_Cust#
     C                   IF        wk_Cust# = *Zero
     C     wk_CustItem#  CHAIN     BARCUSTL6
     C                   IF        %found(BARCUSTL6)
     C                   EVAL      wk_HMCItem# = CBITEM
     C                   LEAVE
     C                   ENDIF                                                  %found(BARCUST)
     C                   ELSE
     C     key_CB2       CHAIN     BARCUSTL6
     C                   IF        %found(BARCUSTL6)
     C                   EVAL      wk_HMCItem# = CBITEM
     C                   LEAVE
     C                   ENDIF                                                  %found(BARCUST)
     C                   ENDIF                                                  wk_Cust#=*Zero

      *
      * Check if passed customer item number is actually a customer UCC-14.
      *
     C                   EVAL      wk_UCCMC = wk_CustItem#
     C     wk_UCCMC      CHAIN     BARCUSTL3
     C                   IF        %found(BARCUSTL3)  and
     C                               (wk_Cust# = *Zero  or  CBCUST = wk_Cust#)
     C                   EVAL      wk_HMCItem# = CBITEM
     C                   LEAVE
     C                   ENDIF                                                  %found(BARCUSTL3)

     C                   ENDDO

      *
      * Return the customer item number.
      *
     C                   RETURN    wk_HMCItem#

     P HMCItemFrmCust  E

     P/EJECT
      **************************************************************************
      * Retrieve an item number from a barcode.                                *
      **************************************************************************
     P BarcodeItemC    B                   EXPORT

     D                 PI            30A
     D Barcode                       14A   CONST
     D ItemOwner                      1A   CONST                                H=Hornady, C=Cust

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Barcode      S             14A
     D wk_ItemOwner    S              1A
     D wk_Item#        S             30A
     D wk_UPC12        S             12A

      *
      * Retrieve the item number.
      *
     C                   EVAL      wk_Barcode = Barcode
     C                   EVAL      wk_ItemOwner = ItemOwner
     C                   EVAL      wk_Item# = *Blanks

     C                   DO

     C                   IF        %subst(wk_Barcode:13:2) = *Blanks
     C                   EVAL      wk_UPC12 = %subst(wk_Barcode:1:12)
     C                   IF        not %open(BARCUSTL2)
     C                   OPEN      BARCUSTL2
     C                   ENDIF                                                  not %open(BARCUSTL2)
     C     wk_UPC12      CHAIN     BARCUSTL2
     C                   IF        %found(BARCUSTL2)
     C                   IF        wk_ItemOwner = 'C'  and  CBCITEM <> *Blanks
     C                   EVAL      wk_Item# = CBCITEM
     C                   ELSE
     C                   EVAL      wk_Item# = CBITEM
     C                   ENDIF                                                  wk_ItemOwner='C'
     C                   ENDIF                                                  %found(BARCUSTL2)
     C                   ELSE
     C                   IF        not %open(BARCUSTL3)
     C                   OPEN      BARCUSTL3
     C                   ENDIF                                                  not %open(BARCUSTL3)
     C     wk_Barcode    CHAIN     BARCUSTL3
     C                   IF        %found(BARCUSTL3)
     C                   IF        wk_ItemOwner = 'C'  and  CBCITEM <> *Blanks
     C                   EVAL      wk_Item# = CBCITEM
     C                   ELSE
     C                   EVAL      wk_Item# = CBITEM
     C                   ENDIF                                                  wk_ItemOwner='C'
     C                   ELSE
     C                   IF        not %open(BARCUSTL4)
     C                   OPEN      BARCUSTL4
     C                   ENDIF                                                  not %open(BARCUSTL4)
     C     wk_Barcode    CHAIN     BARCUSTL4
     C                   IF        %found(BARCUSTL4)
     C                   IF        wk_ItemOwner = 'C'  and  CBCITEM <> *Blanks
     C                   EVAL      wk_Item# = CBCITEM
     C                   ELSE
     C                   EVAL      wk_Item# = CBITEM
     C                   ENDIF                                                  wk_ItemOwner='C'
     C                   ELSE
     C                   IF        not %open(BARCUSTL5)
     C                   OPEN      BARCUSTL5
     C                   ENDIF                                                  not %open(BARCUSTL5)
     C     wk_Barcode    CHAIN     BARCUSTL5
     C                   IF        %found(BARCUSTL5)
     C                   IF        wk_ItemOwner = 'C'  and  CBCITEM <> *Blanks
     C                   EVAL      wk_Item# = CBCITEM
     C                   ELSE
     C                   EVAL      wk_Item# = CBITEM
     C                   ENDIF                                                  wk_ItemOwner='C'
     C                   ENDIF                                                  %found(BARCUSTL5)
     C                   ENDIF                                                  %found(BARCUSTL4)
     C                   ENDIF                                                  %found(BARCUSTL3)
     C                   ENDIF                                                  %subst(wk_Barcode:13

     C                   ENDDO

      *
      * Return the item number.
      *
     C                   RETURN    wk_Item#

     P BarcodeItemC    E

     P/EJECT
      **************************************************************************
      * Retrieve the packaging quantity of a barcode.                          *
      **************************************************************************
     P BarcodeQtyC     B                   EXPORT

     D                 PI             7P 0
     D Barcode                       14A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Barcode      S             14A
     D wk_UPC12        S             12A
     D wk_Qty          S              7P 0

      *
      * Retrieve the barcode packaging quantity.
      *
     C                   EVAL      wk_Barcode = Barcode
     C                   EVAL      wk_Qty = *Zero

     C                   DO

     C                   IF        %subst(wk_Barcode:13:2) = *Blanks
     C                   EVAL      wk_UPC12 = %subst(wk_Barcode:1:12)
     C                   IF        not %open(BARCUSTL2)
     C                   OPEN      BARCUSTL2
     C                   ENDIF                                                  not %open(BARCUSTL2)
     C     wk_UPC12      CHAIN     BARCUSTL2
     C                   IF        %found(BARCUSTL2)
     C                   EVAL      wk_Qty = 1
     C                   ENDIF                                                   %found(BARCUSTL2)
     C                   ELSE
     C                   IF        not %open(BARCUSTL3)
     C                   OPEN      BARCUSTL3
     C                   ENDIF                                                  not %open(BARCUSTL3)
     C     wk_Barcode    CHAIN     BARCUSTL3
     C                   IF        %found(BARCUSTL3)
     C                   EVAL      wk_Qty = CBBOXCTN
     C                   ELSE
     C                   IF        not %open(BARCUSTL4)
     C                   OPEN      BARCUSTL4
     C                   ENDIF                                                  not %open(BARCUSTL4)
     C     wk_Barcode    CHAIN     BARCUSTL4
     C                   IF        %found(BARCUSTL4)
     C                   EVAL      wk_Qty = CBQTYBAR
     C                   ELSE
     C                   IF        not %open(BARCUSTL5)
     C                   OPEN      BARCUSTL5
     C                   ENDIF                                                  not %open(BARCUSTL5)
     C     wk_Barcode    CHAIN     BARCUSTL5
     C                   IF        %found(BARCUSTL5)
     C                   EVAL      wk_Qty = CBQTYIPC
     C                   ENDIF                                                  %found(BARCUSTL5)
     C                   ENDIF                                                  %found(BARCUSTL4)
     C                   ENDIF                                                  %found(BARCUSTL3)
     C                   ENDIF                                                  %subst(wk_Barcode:13

     C                   ENDDO

      *
      * Return the packaging quantity.
      *
     C                   RETURN    wk_Qty

     P BarcodeQtyC     E

     P/EJECT
      **************************************************************************
      * Retrieve a barcode from an item number.                                *
      *  Valid packaging levels: U=Unit, M=MasterCarton, I=InnerPackCarton.    *
      *                          B=BarrelCarton                                *
      **************************************************************************
     P ItemBarcodeC    B                   EXPORT

     D                 PI            14A
     D Item#                         30A   CONST
     D Cust#                          7P 0 CONST
     D ItemOwner                      1A   CONST                                H=Hornady, C=Cust
     D PkgLvl                         1A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             30A
     D wk_Cust#        S              7P 0
     D wk_ItemOwner    S              1A
     D wk_PkgLvl       S              1A
     D wk_Barcode      S             14A

      *
      * Retrieve the barcode.
      *
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Cust# = Cust#
     C                   EVAL      wk_ItemOwner = ItemOwner
     C                   EVAL      wk_PkgLvl = PkgLvl
     C                   EVAL      wk_Barcode = *Blanks

     C                   DO

     C                   IF        wk_ItemOwner = 'C'
     C                   IF        not %open(BARCUSTL6)
     C                   OPEN      BARCUSTL6
     C                   ENDIF                                                  not %open(BARCUSTL6)
     C                   EVAL      kf_CITEM = wk_Item#
     C                   EVAL      kf_CUST = wk_Cust#
     C     key_CB2       CHAIN     BARCUSTL6
     C                   ELSE
     C                   IF        %subst(wk_Item#:16:15) <> *Blanks
     C                   LEAVE
     C                   ENDIF                                                  %subst(wk_Item#:16:
     C                   IF        not %open(BARCUST)
     C                   OPEN      BARCUST
     C                   ENDIF                                                  not %open(BARCUST)
     C                   EVAL      kf_ITEM = wk_Item#
     C                   EVAL      kf_CUST = wk_Cust#
     C     key_CB1       CHAIN     BARCUST
     C                   ENDIF                                                  wk_ItemOwner='C'
     C                   IF        %found
     C                   SELECT
     C                   WHEN      wk_PkgLvl = 'U'                              Unit
     C                   IF        CBCUPC <> *Blanks
     C                   EVAL      wk_Barcode = '00' + CBCUPC
     C                   ENDIF                                                  CBCUPC<>*Blanks
     C                   WHEN      wk_PkgLvl = 'M'                              Master carton
     C                   EVAL      wk_Barcode = CBCUCCMC
     C                   WHEN      wk_PkgLvl = 'I'                              Inner-pack carton
     C                   EVAL      wk_Barcode = CBCUCCIP
     C                   WHEN      wk_PkgLvl = 'B'                              Master barrel carton
     C                   EVAL      wk_Barcode = CBCUCCBC
     C                   ENDSL
     C                   ENDIF                                                  %found

     C                   ENDDO

      *
      * Return the barcode.
      *
     C                   RETURN    wk_Barcode

     P ItemBarcodeC    E

     P/EJECT
      **************************************************************************
      * Retrieve the packaging quantity of an item.                            *
      *  Valid packaging levels: U=Unit, M=MasterCarton, I=InnerPackCarton.    *
      *                          B=BarrelCarton                                *
      **************************************************************************
     P ItemQtyC        B                   EXPORT

     D                 PI             7P 0
     D Item#                         30A   CONST
     D Cust#                          7P 0 CONST
     D ItemOwner                      1A   CONST                                H=Hornady, C=Cust
     D PkgLvl                         1A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             30A
     D wk_Cust#        S              7P 0
     D wk_ItemOwner    S              1A
     D wk_PkgLvl       S              1A
     D wk_Qty          S              7P 0

      *
      * Retrieve the barcode.
      *
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_Cust# = Cust#
     C                   EVAL      wk_ItemOwner = ItemOwner
     C                   EVAL      wk_PkgLvl = PkgLvl
     C                   EVAL      wk_Qty = *Zero

     C                   DO

     C                   IF        wk_ItemOwner = 'C'
     C                   IF        not %open(BARCUSTL6)
     C                   OPEN      BARCUSTL6
     C                   ENDIF                                                  not %open(BARCUSTL6)
     C                   EVAL      kf_CITEM = wk_Item#
     C                   EVAL      kf_CUST = wk_Cust#
     C     key_CB2       CHAIN     BARCUSTL6
     C                   ELSE
     C                   IF        %subst(wk_Item#:16:15) <> *Blanks
     C                   LEAVE
     C                   ENDIF                                                  %subst(wk_Item#:16:
     C                   IF        not %open(BARCUST)
     C                   OPEN      BARCUST
     C                   ENDIF                                                  not %open(BARCUST)
     C                   EVAL      kf_ITEM = wk_Item#
     C                   EVAL      kf_CUST = wk_Cust#
     C     key_CB1       CHAIN     BARCUST
     C                   ENDIF                                                  wk_ItemOwner='C'
     C                   IF        %found
     C                   SELECT
     C                   WHEN      wk_PkgLvl = 'U'                              Unit
     C                   EVAL      wk_Qty = 1
     C                   WHEN      wk_PkgLvl = 'M'                              Master carton
     C                   EVAL      wk_Qty = CBBOXCTN
     C                   WHEN      wk_PkgLvl = 'I'                              Inner-pack carton
     C                   EVAL      wk_Qty = CBQTYIPC
     C                   WHEN      wk_PkgLvl = 'B'                              Master barrel carton
     C                   EVAL      wk_Qty = CBQTYBAR
     C                   ENDSL
     C                   ENDIF                                                  %found

     C                   ENDDO

      *
      * Return the packaging quantity.
      *
     C                   RETURN    wk_Qty

     P ItemQtyC        E

     P/EJECT
      **************************************************************************
      * Shut down service program.                                             *
      **************************************************************************
     P sd_HYR9962      B                   EXPORT
     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(BARCUST)
     C                   CLOSE     BARCUST
     C                   ENDIF                                                  %open(BARCUST)

     C                   IF        %open(BARCUSTL2)
     C                   CLOSE     BARCUSTL2
     C                   ENDIF                                                  %open(BARCUSTL2)

     C                   IF        %open(BARCUSTL3)
     C                   CLOSE     BARCUSTL3
     C                   ENDIF                                                  %open(BARCUSTL3)

     C                   IF        %open(BARCUSTL4)
     C                   CLOSE     BARCUSTL4
     C                   ENDIF                                                  %open(BARCUSTL4)

     C                   IF        %open(BARCUSTL5)
     C                   CLOSE     BARCUSTL5
     C                   ENDIF                                                  %open(BARCUSTL5)

     C                   IF        %open(BARCUSTL6)
     C                   CLOSE     BARCUSTL6
     C                   ENDIF                                                  %open(BARCUSTL6)

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR9962      E

     P/EJECT
