      // @MBRTEXT: Get warehouse current ship pack
      *================================================================
      *
      *  Program........: GETSPK
      *  Written by.....: M. Pryor
      *  Written for....: Variety Wholesalers, Inc.
      *  Date written...: January 2017
      *
      *  Purpose........: Get the current ship pack from INVVENJ2 for
      *                   a sku and warehouse.
      *
      *  04/04/17: IVINPK can be zero ... apparently.  If so, use
      *            IMINPK from INVMST.
      *
      *================================================================

     FInvVenJ2  IF   E           K DISK
     FInvMst    IF   E           K DISK

      * ...............................................................
      * : Work variables.                                             :
      * :.............................................................:

     D PriVnd          S              1S 0 INZ(1)

      * ...............................................................
      * : Receive incoming paramters.                                 :
      * :.............................................................:

     D                 PI
     D SkuNo                          9P 0
     D WhsNo                          5P 0
     D ShpPk                          7P 0
     D CasPk                          7P 0

      * ...............................................................
      * : Define access key for INVVENJ2.                             :
      * :.............................................................:

     C     VenKey        KLIST
     C                   KFLD                    SkuNo
     C                   KFLD                    WhsNo
     C                   KFLD                    PriVnd

      *================================================================
      * MAIN - If there is a record in INVVENJ2 for the requested
      *        sku/warehouse, return that ship pack.
      *================================================================
      /FREE

       CHAIN VenKey InvVenJ2;
       IF %FOUND AND IvinPk > 0;
           ShpPk = Ivinpk;
           CasPk = IvsDpk;
       ELSE;
           CHAIN SkuNo rInvMst;
           IF %FOUND;
               ShpPk = IminPk;
               CasPk = IstdPk;
           ELSE;
               ShpPk = 0;
               CasPk = 0;
           ENDIF;
       ENDIF;

       *INLR = *ON;

      /END-FREE