     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2007')
     H nomain
      **************************************************************************
      * APPLICATION: Miscellaneous                                             *
      *       TITLE: Subprocedures - Item Barcode                              *
      *          BY: William Earl (MCA)                                        *
      *        DATE: August 2007                                               *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  11/21/2011  MCA  Add master barrel carton UPC-14 code.                *
      *  04/16/2012  MCA  Add BARDATA file processing.                         *
      *  04/16/2012  MCA  Add ItemQty subprocedure.                            *
      *  03/04/2013  MCA  Remove BARCODE file.                                 *
      *  11/18/2016  MCA  Add BARDATA1 files.  This is temporary for tkt# 2041.*
      *  03/22/2023  MCA  Remove all references to BARDATA1 (used for the      *
      *                   BARDATA2.0 conversion).                              *
      *                                                                        *
      **************************************************************************

     FBARDATA   IF   E           K DISK    USROPN
     FBARDATAL2 IF   E           K DISK    RENAME(BARDATAR:BARDATAR2) USROPN
     FBARDATAL3 IF   E           K DISK    RENAME(BARDATAR:BARDATAR3) USROPN
     FBARDATAL4 IF   E           K DISK    RENAME(BARDATAR:BARDATAR4) USROPN
     FBARDATAL5 IF   E           K DISK    RENAME(BARDATAR:BARDATAR5) USROPN
     F****BARDATA1  IF   E           K DISK    RENAME(BARDATAR:BARDATA1R1) USROPN
     F****BARDATA1L2IF   E           K DISK    RENAME(BARDATAR:BARDATA1R2) USROPN
     F****BARDATA1L3IF   E           K DISK    RENAME(BARDATAR:BARDATA1R3) USROPN
     F****BARDATA1L4IF   E           K DISK    RENAME(BARDATAR:BARDATA1R4) USROPN
     F****BARDATA1L5IF   E           K DISK    RENAME(BARDATAR:BARDATA1R5) USROPN
     FHDIWHS    IF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9960

     P/EJECT
      **************************************************************************
      * Retrieve an item number from a barcode.                                *
      **************************************************************************
     P BarcodeItem     B                   EXPORT

     D                 PI            15A
     D Barcode                       14A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Barcode      S             14A
     D wk_UPC12        S             12A
     D wk_Item#        S             15A

      *
      * Retrieve the item number.
      *
     C                   EVAL      wk_Barcode = Barcode
     C                   EVAL      wk_Item# = *Blanks

     C                   DO

     C                   EVAL      BDITEM = *Blanks
     C                   EVAL      BDSTS = *Blanks

     C                   IF        %subst(wk_Barcode:13:2) = *Blanks
     C                   EVAL      wk_UPC12 = %subst(wk_Barcode:1:12)
     C                   IF        not %open(BARDATAL2)
     C                   OPEN      BARDATAL2
     C                   ENDIF                                                  not %open(BARDATAL2)
     C     wk_UPC12      CHAIN     BARDATAL2
     C                   IF        %found(BARDATAL2)
     C                   EVAL      wk_Item# = BDITEM
     C                   ENDIF                                                   %found(BARDATAL2)
     C                   ELSE
     C                   IF        not %open(BARDATAL3)
     C                   OPEN      BARDATAL3
     C                   ENDIF                                                  not %open(BARDATAL3)
     C     wk_Barcode    CHAIN     BARDATAL3
     C                   IF        %found(BARDATAL3)
     C                   EVAL      wk_Item# = BDITEM
     C                   ELSE
     C                   IF        not %open(BARDATAL4)
     C                   OPEN      BARDATAL4
     C                   ENDIF                                                  not %open(BARDATAL4)
     C     wk_Barcode    CHAIN     BARDATAL4
     C                   IF        %found(BARDATAL4)
     C                   EVAL      wk_Item# = BDITEM
     C                   ELSE
     C                   IF        not %open(BARDATAL5)
     C                   OPEN      BARDATAL5
     C                   ENDIF                                                  not %open(BARDATAL5)
     C     wk_Barcode    CHAIN     BARDATAL5
     C                   IF        %found(BARDATAL5)
     C                   EVAL      wk_Item# = BDITEM
     C                   ENDIF                                                  %found(BARDATAL5)
     C                   ENDIF                                                  %found(BARDATAL4)
     C                   ENDIF                                                  %found(BARDATAL3)
     C                   ENDIF                                                  %subst(wk_Barcode:13

     C****               IF        BDITEM <> *Blanks  and  BDSTS = 'A'
     C****               LEAVE
     C****               ENDIF                                                  BDITEM=*Blanks

     C****               EVAL      wk_Item# = *Blanks

     C****               IF        %subst(wk_Barcode:13:2) = *Blanks
     C****               EVAL      wk_UPC12 = %subst(wk_Barcode:1:12)
     C****               IF        not %open(BARDATA1L2)
     C****               OPEN      BARDATA1L2
     C****               ENDIF                                                  not %open(BARDATA1L2
     C**** wk_UPC12      CHAIN     BARDATA1L2
     C****               IF        %found(BARDATA1L2)
     C****               EVAL      wk_Item# = BDITEM
     C****               ENDIF                                                  %found(BARDATA1L2)
     C****               ELSE
     C****               IF        not %open(BARDATA1L3)
     C****               OPEN      BARDATA1L3
     C****               ENDIF                                                  not %open(BARDATA1L3
     C**** wk_Barcode    CHAIN     BARDATA1L3
     C****               IF        %found(BARDATA1L3)
     C****               EVAL      wk_Item# = BDITEM
     C****               ELSE
     C****               IF        not %open(BARDATA1L4)
     C****               OPEN      BARDATA1L4
     C****               ENDIF                                                  not %open(BARDATA1L4
     C**** wk_Barcode    CHAIN     BARDATA1L4
     C****               IF        %found(BARDATA1L4)
     C****               EVAL      wk_Item# = BDITEM
     C****               ELSE
     C****               IF        not %open(BARDATA1L5)
     C****               OPEN      BARDATA1L5
     C****               ENDIF                                                  not %open(BARDATA1L5
     C**** wk_Barcode    CHAIN     BARDATA1L5
     C****               IF        %found(BARDATA1L5)
     C****               EVAL      wk_Item# = BDITEM
     C****               ENDIF                                                  %found(BARDATA1L5)
     C****               ENDIF                                                  %found(BARDATA1L4)
     C****               ENDIF                                                  %found(BARDATA1L3)
     C****               ENDIF                                                  %subst(wk_Barcode:13

     C                   ENDDO

      *
      * Return the item number.
      *
     C                   RETURN    wk_Item#

     P BarcodeItem     E

     P/EJECT
      **************************************************************************
      * Retrieve the packaging quantity of a barcode.                          *
      **************************************************************************
     P BarcodeQty      B                   EXPORT

     D                 PI             5P 0
     D Barcode                       14A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Barcode      S             14A
     D wk_Item#        S             15A
     D wk_UPC12        S             12A
     D wk_Qty          S              5P 0
     D wk_BCFnd        S              1N

      *
      * Retrieve the barcode packaging quantity.
      *
     C                   EVAL      wk_Barcode = Barcode
     C                   EVAL      wk_Item# = *Blanks
     C                   EVAL      wk_Qty = *Zero
     C                   EVAL      wk_BCFnd = *OFF

     C                   DO

     C                   EVAL      BDITEM = *Blanks
     C                   EVAL      BDSTS = *Blanks

     C                   IF        %subst(wk_Barcode:13:2) = *Blanks
     C                   EVAL      wk_UPC12 = %subst(wk_Barcode:1:12)
     C                   IF        not %open(BARDATAL2)
     C                   OPEN      BARDATAL2
     C                   ENDIF                                                  not %open(BARDATAL2)
     C     wk_UPC12      CHAIN     BARDATAL2
     C                   IF        %found(BARDATAL2)
     C                   EVAL      wk_Qty = 1
     C                   ENDIF                                                   %found(BARDATAL2)
     C                   ELSE
     C                   IF        not %open(BARDATAL3)
     C                   OPEN      BARDATAL3
     C                   ENDIF                                                  not %open(BARDATAL3)
     C     wk_Barcode    CHAIN     BARDATAL3
     C                   IF        %found(BARDATAL3)
     C                   EVAL      wk_Item# = BDITEM
     C                   IF        not %open(HDIWHS)
     C                   OPEN      HDIWHS
     C                   ENDIF                                                  not %open(HDIWHS)
     C     wk_Item#      CHAIN     HDIWHS
     C                   IF        %found(HDIWHS)
     C                   EVAL      wk_Qty = IWPCPC
     C                   ENDIF                                                  %found(HDIWHS)
     C                   ELSE
     C                   IF        not %open(BARDATAL4)
     C                   OPEN      BARDATAL4
     C                   ENDIF                                                  not %open(BARDATAL4)
     C     wk_Barcode    CHAIN     BARDATAL4
     C                   IF        %found(BARDATAL4)
     C                   EVAL      wk_Qty = BDQTYIPC
     C                   ELSE
     C                   IF        not %open(BARDATAL5)
     C                   OPEN      BARDATAL5
     C                   ENDIF                                                  not %open(BARDATAL5)
     C     wk_Barcode    CHAIN     BARDATAL5
     C                   IF        %found(BARDATAL5)
     C                   EVAL      wk_Qty = BDQTYBAR
     C                   ENDIF                                                  %found(BARDATAL5)
     C                   ENDIF                                                  %found(BARDATAL4)
     C                   ENDIF                                                  %found(BARDATAL3)
     C                   ENDIF                                                  %subst(wk_Barcode:13

     C****               IF        BDITEM <> *Blanks  and  BDSTS = 'A'
     C****               LEAVE
     C****               ENDIF                                                  BDITEM<>*Blanks

     C****               EVAL      wk_Qty = *Zero

     C****               IFAL      %subst(wk_Barcode:13:2) = *Blanks
     C****               EVAL      wk_UPC12 =Z%subst(wk_Barcode:1:12)
     C****               IFAL      notQ%open(BARDATA1L2)
     C****               OPEN      BARDATA1L2Zero
     C****               ENDIF     wk_Qty = *Zero                               not %open(BARDATA1L2
     C**** wk_UPC12      CHAIN     BARDATA1L2Zero
     C****               IFAL      %found(BARDATA1L2)
     C****               EVAL      wk_Qty = 1Zero
     C****               ENDIF     wk_Qty = *Zero                                %found(BARDATA1L2)
     C****               ELSE      wk_Qty = *Zero
     C****               IFAL      notQ%open(BARDATA1L3)
     C****               OPEN      BARDATA1L3Zero
     C****               ENDIF     wk_Qty = *Zero                               not %open(BARDATA1L3
     C**** wk_Barcode    CHAIN     BARDATA1L3Zero
     C****               IFAL      %found(BARDATA1L3)
     C****               EVAL      wk_Item# =ZBDITEM
     C****               IFAL      notQ%open(HDIWHS)
     C****               OPEN      HDIWHS = *Zero
     C****               ENDIF     wk_Qty = *Zero                               not %open(HDIWHS)
     C**** wk_Item#      CHAIN     HDIWHS = *Zero
     C****               IFAL      %found(HDIWHS)
     C****               EVAL      wk_Qty = IWPCPC
     C****               ENDIF     wk_Qty = *Zero                               %found(HDIWHS)
     C****               ELSE      wk_Qty = *Zero
     C****               IFAL      notQ%open(BARDATA1L4)
     C****               OPEN      BARDATA1L4Zero
     C****               ENDIF     wk_Qty = *Zero                               not %open(BARDATA1L4
     C**** wk_Barcode    CHAIN     BARDATA1L4Zero
     C****               IFAL      %found(BARDATA1L4)
     C****               EVAL      wk_Qty = BDQTYIPC
     C****               ELSE      wk_Qty = *Zero
     C****               IFAL      notQ%open(BARDATA1L5)
     C****               OPEN      BARDATA1L5Zero
     C****               ENDIF     wk_Qty = *Zero                               not %open(BARDATA1L5
     C**** wk_Barcode    CHAIN     BARDATA1L5Zero
     C****               IFAL      %found(BARDATA1L5)
     C****               EVAL      wk_Qty = BDQTYBAR
     C****               ENDIF     wk_Qty = *Zero                               %found(BARDATA1L5)
     C****               ENDIF     wk_Qty = *Zero                               %found(BARDATA1L4)
     C****               ENDIF     wk_Qty = *Zero                               %found(BARDATA1L3)
     C****               ENDIF     wk_Qty = *Zero                               %subst(wk_Barcode:13

     C                   ENDDO

      *
      * Return the packaging quantity.
      *
     C                   RETURN    wk_Qty

     P BarcodeQty      E

     P/EJECT
      **************************************************************************
      * Retrieve a barcode from an item number.                                *
      *  Valid packing levels: U=Unit, M=MasterCarton, I=InnerPackCarton.      *
      *                        B=BarrelCarton                                  *
      **************************************************************************
     P ItemBarcode     B                   EXPORT

     D                 PI            14A
     D Item#                         15A   CONST
     D PkgLvl                         1A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             15A
     D wk_PkgLvl       S              1A
     D wk_Barcode      S             14A

      *
      * Retrieve the barcode.
      *
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_PkgLvl = PkgLvl
     C                   EVAL      wk_Barcode = *Blanks

     C                   DO

     C                   EVAL      BDITEM = *Blanks
     C                   EVAL      BDSTS = *Blanks

     C                   IF        not %open(BARDATA)
     C                   OPEN      BARDATA
     C                   ENDIF                                                  not %open(BARDATA)
     C     wk_Item#      CHAIN     BARDATA
     C                   IF        %found(BARDATA)
     C                   SELECT
     C                   WHEN      wk_PkgLvl = 'U'                              Unit
     C                   EVAL      wk_Barcode = '00' + BDUPC12
     C                   WHEN      wk_PkgLvl = 'M'                              Master carton
     C                   EVAL      wk_Barcode = BDUCC14
     C                   WHEN      wk_PkgLvl = 'I'                              Inner-pack carton
     C                   EVAL      wk_Barcode = BDUCC14I
     C                   WHEN      wk_PkgLvl = 'B'                              Master barrel carton
     C                   EVAL      wk_Barcode = BDUCCB14
     C                   ENDSL
     C                   ENDIF                                                  %found(BARDATA)

     C****               IF        BDITEM <> *Blanks  and  BDSTS = 'A'
     C****               LEAVE
     C****               ENDIF                                                  BDITEM<>*Blanks

     C****               EVAL      wk_Barcode = *Blanks

     C****               IF        not %open(BARDATA1)
     C****               OPEN      BARDATA1
     C****               ENDIF                                                  not %open(BARDATA1)
     C**** wk_Item#      CHAIN     BARDATA1
     C****               IF        %found(BARDATA1)
     C****               SELECT
     C****               WHEN      wk_PkgLvl = 'U'                              Unit
     C****               EVAL      wk_Barcode = '00' + BDUPC12
     C****               WHEN      wk_PkgLvl = 'M'                              Master carton
     C****               EVAL      wk_Barcode = BDUCC14
     C****               WHEN      wk_PkgLvl = 'I'                              Inner-pack carton
     C****               EVAL      wk_Barcode = BDUCC14I
     C****               WHEN      wk_PkgLvl = 'B'                              Master barrel carton
     C****               EVAL      wk_Barcode = BDUCCB14
     C****               ENDSL
     C****               ENDIF                                                  %found(BARDATA1)

     C                   ENDDO

      *
      * Return the barcode.
      *
     C                   RETURN    wk_Barcode

     P ItemBarcode     E

     P/EJECT
      **************************************************************************
      * Retrieve the packaging quantity of an item.                            *
      *  Valid packing levels: U=Unit, M=MasterCarton, I=InnerPackCarton.      *
      *                        B=BarrelCarton                                  *
      **************************************************************************
     P ItemQty         B                   EXPORT

     D                 PI             7P 0
     D Item#                         15A   CONST
     D PkgLvl                         1A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             15A
     D wk_PkgLvl       S              1A
     D wk_Qty          S              5P 0
     D wk_BCFnd        S              1N

      *
      * Retrieve the barcode.
      *
     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_PkgLvl = PkgLvl
     C                   EVAL      wk_Qty = *Zero
     C                   EVAL      wk_BCFnd = *OFF

     C                   DO

     C                   EVAL      BDITEM = *Blanks
     C                   EVAL      BDSTS = *Blanks

     C                   SELECT
     C                   WHEN      wk_PkgLvl = 'U'                              Unit
     C                   EVAL      wk_Qty = 1
     C                   WHEN      wk_PkgLvl = 'M'                              Master carton
     C                   IF        not %open(HDIWHS)
     C                   OPEN      HDIWHS
     C                   ENDIF                                                  not %open(HDIWHS)
     C     wk_Item#      CHAIN     HDIWHS
     C                   IF        %found(HDIWHS)
     C                   EVAL      wk_Qty = IWPCPC
     C                   ENDIF                                                  %found(HDIWHS)
     C                   WHEN      wk_PkgLvl = 'I'                              Inner-pack carton
     C                   IF        not %open(BARDATA)
     C                   OPEN      BARDATA
     C                   ENDIF                                                  not %open(BARDATA)
     C     wk_Item#      CHAIN     BARDATA
     C                   IF        %found(BARDATA)
     C                   EVAL      wk_Qty = BDQTYIPC
     C                   ENDIF                                                  %found(BARDATA)
     C                   WHEN      wk_PkgLvl = 'B'                              Master barrel carton
     C                   IF        not %open(BARDATA)
     C                   OPEN      BARDATA
     C                   ENDIF                                                  not %open(BARDATA)
     C     wk_Item#      CHAIN     BARDATA
     C                   IF        %found(BARDATA)
     C                   EVAL      wk_Qty = BDQTYBAR
     C                   ENDIF                                                  %found(BARDATA)
     C                   ENDSL

     C****               IF        (wk_PkgLvl = 'U'  or  wk_PkgLvl = 'M')  or
     C****                         ((wk_PkgLvl = 'I'  or  wk_PkgLvl = 'B')  and
     C****                            BDITEM <> *Blanks  and  BDSTS = 'A')
     C****               LEAVE
     C****               ENDIF                                                  wk_PkgLvl='I' or

     C****               EVAL      wk_Qty = *Zero

     C****               SELECT
     C****               WHEN      wk_PkgLvl = 'I'                              Inner-pack carton
     C****               IF        not %open(BARDATA1)
     C****               OPEN      BARDATA1
     C****               ENDIF                                                  not %open(BARDATA1)
     C**** wk_Item#      CHAIN     BARDATA1
     C****               IFAL      %found(BARDATA1)
     C****               EVAL      wk_Qty = BDQTYIPC
     C****               ENDIF                                                  %found(BARDATA1)
     C****               WHEN      wk_PkgLvl = 'B'                              Master barrel carton
     C****               IF        not %open(BARDATA1)
     C****               OPEN      BARDATA1
     C****               ENDIF                                                  not %open(BARDATA1)
     C**** wk_Item#      CHAIN     BARDATA1
     C****               IFAL      %found(BARDATA1)
     C****               EVAL      wk_Qty = BDQTYBAR
     C****               ENDIF                                                  %found(BARDATA1)
     C****               ENDSL

     C                   ENDDO

      *
      * Return the packaging quantity.
      *
     C                   RETURN    wk_Qty

     P ItemQty         E

     P/EJECT
      **************************************************************************
      * Shut down service program.                                             *
      **************************************************************************
     P sd_HYR9960      B                   EXPORT
     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(BARDATA)
     C                   CLOSE     BARDATA
     C                   ENDIF                                                  %open(BARDATA)

     C                   IF        %open(BARDATAL2)
     C                   CLOSE     BARDATAL2
     C                   ENDIF                                                  %open(BARDATAL2)

     C                   IF        %open(BARDATAL3)
     C                   CLOSE     BARDATAL3
     C                   ENDIF                                                  %open(BARDATAL3)

     C                   IF        %open(BARDATAL4)
     C                   CLOSE     BARDATAL4
     C                   ENDIF                                                  %open(BARDATAL4)

     C                   IF        %open(BARDATAL5)
     C                   CLOSE     BARDATAL5
     C                   ENDIF                                                  %open(BARDATAL5)

     C                   IF        %open(HDIWHS)
     C                   CLOSE     HDIWHS
     C                   ENDIF                                                  %open(HDIWHS)

     C****               IF        %open(BARDATA1)
     C****               CLOSE     BARDATA1
     C****               ENDIF                                                  %open(BARDATA)

     C****               IF        %open(BARDATA1L2)
     C****               CLOSE     BARDATA1L2
     C****               ENDIF                                                  %open(BARDATA1L2)

     C****               IF        %open(BARDATA1L3)
     C****               CLOSE     BARDATA1L3
     C****               ENDIF                                                  %open(BARDATA1L3)

     C****               IF        %open(BARDATA1L4)
     C****               CLOSE     BARDATA1L4
     C****               ENDIF                                                  %open(BARDATA1L4)

     C****               IF        %open(BARDATA1L5)
     C****               CLOSE     BARDATA1L5
     C****               ENDIF                                                  %open(BARDATA1L5)

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR9960      E

     P/EJECT
