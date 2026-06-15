     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2026')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Subprocedures - Hazardous Items (2)                       *
      *          BY: William Earl (MCA)                                        *
      *        DATE: January 2026                                              *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                                                                        *
      **************************************************************************

     Hnomain

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9916A

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

     P/EJECT
      **************************************************************************
      * Retrieve ammo NEQ.                                                     *
      **************************************************************************
     P RtvAmmoNEQ      B                   EXPORT

     D                 PI             6P 2
     D Item#                         15A   CONST
     D BoxQty                         5P 0 CONST
     D NEQType                        3A   CONST
      ** PWD - Powder, PRI - Primer, TOT - Total
     D NEQUOM                         2A   CONST
      ** KG - Kilograms, LB - Pounds

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             15A
     D wk_BoxQty       S              5P 0
     D wk_NEQType      S              3A
     D wk_NEQUOM       S              2A
     D wk_NEQ          S              6P 2
     D wk_PwdrLbs      S              9P 5
     D wk_TotPwdrLbs   S              9P 5
     D wk_TotPwdrNEQ   S              9P 2
     D wk_PrimerGrns   S              5P 3
     D wk_TotPrimGrns  S              9P 3
     D wk_RndsPerBox   S              5P 0
     D wk_TotPrimLbs   S              5P 3
     D wk_TotPrimNEQ   S              5P 2
     D wk_PSQPER       S              9P 5
     D wk_IMUDN1       S             13P 5
     D wk_TDNF01       S             15P 4
     D wk_LbsToKgCvt   S              7P 7 INZ(.4535924)
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_BoxQty = BoxQty
     C                   EVAL      wk_NEQType = NEQType
     C                   EVAL      wk_NEQUOM = NEQUOM
     C                   EVAL      wk_NEQ = *Zero
     C                   EVAL      wk_PwdrLbs = *Zero
     C                   EVAL      wk_TotPwdrLbs = *Zero
     C                   EVAL      wk_TotPwdrNEQ = *Zero
     C                   EVAL      wk_PrimerGrns = *Zero
     C                   EVAL      wk_TotPrimGrns = *Zero
     C                   EVAL      wk_RndsPerBox = *Zero
     C                   EVAL      wk_TotPrimLbs = *Zero
     C                   EVAL      wk_TotPrimNEQ = *Zero

      **************************************************************************

      *
      * If the item is manufactured under a different item, and the item
      * itself does not have a product structure BOM, use the item's
      * associated manufactured item's BOM.
      *
     C                   IF        %subst(wk_Item#:1:1) = 'S'  or               Seconds
     C                               %subst(wk_Item#:1:2) = 'LA'                Lot Acceptance
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HDMPSM
                where a.PSPPLT=1 and a.PSPPN=:wk_Item#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   SELECT
     C                   WHEN      %subst(wk_Item#:1:1) = 'S'                   Seconds
     C                   EVAL      wk_Item# = %subst(wk_Item#:2:14)
     C                   WHEN      %subst(wk_Item#:1:2) = 'LA'                  Lot Acceptance
     C                   EVAL      wk_Item# = %subst(wk_Item#:3:13)
     C                   ENDSL
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  %subst(wk_Item#:1:1)

      **************************************************************************

     C                   IF        wk_NEQType = 'PWD'  or
     C                               wk_NEQType = 'TOT'
      /Free
       wk_PSQPER = *Zero;
       EXEC SQL select coalesce(sum(a.PSQPER),0) into :wk_PSQPER
                from HDMPSM a
                where a.PSPPLT=1 and a.PSPPN=:wk_Item# and
                exists (select * from HDIPLT b where b.IPPLT=1 and
                 b.IPITEM=a.PSCPN and b.IPCLAS in ('3C','3U'));
       IF wk_PSQPER = *Zero;
         EXEC SQL select coalesce(sum(a.PSQPER*b.PSQPER),0)
                   into :wk_PSQPER
                  from HDMPSM a
                   join HDMPSM b on a.PSPPLT=b.PSPPLT and a.PSCPN=b.PSPPN
                  where a.PSPPLT=1 and a.PSPPN=:wk_Item# and
                  exists (select * from HDIPLT c where c.IPPLT=1 and
                   c.IPITEM=b.PSCPN and c.IPCLAS in ('3C','3U'));
       ENDIF;                                                                // wk_PSQPER=*Zero
       wk_PwdrLbs = wk_PSQPER;
      /End-Free
     C                   EVAL      wk_TotPwdrLbs = wk_PwdrLbs * wk_BoxQty
     C                   SELECT
     C                   WHEN      %upper(wk_NEQUOM) = 'KG'
     C                   EVAL(H)   wk_TotPwdrNEQ = wk_TotPwdrLbs * wk_LbsToKgCvt
     C                   WHEN      %upper(wk_NEQUOM) = 'LB'
     C                   EVAL(H)   wk_TotPwdrNEQ = wk_TotPwdrLbs
     C                   OTHER
     C                   EVAL      wk_TotPwdrNEQ = *Zero
     C                   ENDSL

     C                   ENDIF                                                  wk_NEQType='PWD' or

      **************************************************************************

     C                   IF        wk_NEQType = 'PRI'  or
     C                               wk_NEQType = 'TOT'
      /Free
       wk_TDNF01 = *Zero;
       EXEC SQL select d.TDNF01 into :wk_TDNF01
                from HDMPSM a
                 join HDMPSM b on a.PSPPLT=b.PSPPLT and a.PSCPN=b.PSPPN
                 join HDIMST c on b.PSCPN=c.IMITEM
                 join GUPTDAT d on d.TDTABL='PRIMER' and c.IMPCLS=d.TDKEY1
                where a.PSPPLT=1 and a.PSPPN=:wk_Item#;
       IF wk_TDNF01 = *Zero;
         EXEC SQL select e.TDNF01 into :wk_TDNF01
                  from HDMPSM a
                   join HDMPSM b on a.PSPPLT=b.PSPPLT and a.PSCPN=b.PSPPN
                   join HDMPSM c on b.PSPPLT=c.PSPPLT and b.PSCPN=c.PSPPN
                   join HDIMST d on c.PSCPN=d.IMITEM
                   join GUPTDAT e on e.TDTABL='PRIMER' and d.IMPCLS=e.TDKEY1
                  where a.PSPPLT=1 and a.PSPPN=:wk_Item#;
       ENDIF;                                                                // wk_TDNF01=*Zero
       wk_PrimerGrns = wk_TDNF01;

       wk_IMUDN1 = *Zero;
       EXEC SQL select IMUDN1 into :wk_IMUDN1
                from HDIMST where IMITEM=:wk_Item#;
       wk_RndsPerBox = wk_IMUDN1;
      /End-Free

     C                   EVAL      wk_TotPrimGrns = wk_PrimerGrns *
     C                               wk_RndsPerBox * wk_BoxQty
     C                   EVAL(H)   wk_TotPrimLbs = wk_TotPrimGrns / 7000
     C                   SELECT
     C                   WHEN      %upper(wk_NEQUOM) = 'KG'
     C                   EVAL(H)   wk_TotPrimNEQ = wk_TotPrimLbs * wk_LbsToKgCvt
     C                   WHEN      %upper(wk_NEQUOM) = 'LB'
     C                   EVAL(H)   wk_TotPrimNEQ = wk_TotPrimLbs
     C                   OTHER
     C                   EVAL      wk_TotPrimNEQ = *Zero
     C                   ENDSL

     C                   ENDIF                                                  wk_NEQType='PRI' or

      **************************************************************************

     C                   EVAL      wk_NEQ = wk_TotPwdrNEQ + wk_TotPrimNeq

     C                   ENDDO

      *
      * Return the NEQ.
      *
     C                   RETURN    wk_NEQ

     P RtvAmmoNEQ      E

     P/EJECT
      **************************************************************************
      * Retrieve ammo partial box NEQ.                                         *
      **************************************************************************
     P RtvAmmoPtBxNEQ  B                   EXPORT

     D                 PI             6P 2
     D Item#                         15A   CONST
     D RoundsQty                      5P 0 CONST
     D NEQType                        3A   CONST
      ** PWD - Powder, PRI - Primer, TOT - Total
     D NEQUOM                         2A   CONST
      ** KG - Kilograms, LB - Pounds

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             15A
     D wk_RoundsQty    S              5P 0
     D wk_NEQType      S              3A
     D wk_NEQUOM       S              2A
     D wk_NEQ          S              6P 2
     D wk_PwdrLbs      S              9P 5
     D wk_TotPwdrLbs   S              9P 5
     D wk_TotPwdrNEQ   S              9P 2
     D wk_PrimerGrns   S              5P 3
     D wk_TotPrimGrns  S              9P 3
     D wk_RndsPerBox   S              5P 0
     D wk_TotPrimLbs   S              5P 3
     D wk_TotPrimNEQ   S              5P 2
     D wk_PSQPER       S              9P 5
     D wk_IMUDN1       S             13P 5
     D wk_TDNF01       S             15P 4
     D wk_LbsToKgCvt   S              7P 7 INZ(.4535924)
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_RoundsQty = RoundsQty
     C                   EVAL      wk_NEQType = NEQType
     C                   EVAL      wk_NEQUOM = NEQUOM
     C                   EVAL      wk_NEQ = *Zero
     C                   EVAL      wk_PwdrLbs = *Zero
     C                   EVAL      wk_TotPwdrLbs = *Zero
     C                   EVAL      wk_TotPwdrNEQ = *Zero
     C                   EVAL      wk_PrimerGrns = *Zero
     C                   EVAL      wk_TotPrimGrns = *Zero
     C                   EVAL      wk_RndsPerBox = *Zero
     C                   EVAL      wk_TotPrimLbs = *Zero
     C                   EVAL      wk_TotPrimNEQ = *Zero

      **************************************************************************

      *
      * If the item is manufactured under a different item, and the item
      * itself does not have a product structure BOM, use the item's
      * associated manufactured item's BOM.
      *
     C                   IF        %subst(wk_Item#:1:1) = 'S'  or               Seconds
     C                               %subst(wk_Item#:1:2) = 'LA'                Lot Acceptance
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HDMPSM
                where a.PSPPLT=1 and a.PSPPN=:wk_Item#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   SELECT
     C                   WHEN      %subst(wk_Item#:1:1) = 'S'                   Seconds
     C                   EVAL      wk_Item# = %subst(wk_Item#:2:14)
     C                   WHEN      %subst(wk_Item#:1:2) = 'LA'                  Lot Acceptance
     C                   EVAL      wk_Item# = %subst(wk_Item#:3:13)
     C                   ENDSL
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  %subst(wk_Item#:1:1)

      **************************************************************************

     C                   IF        wk_NEQType = 'PWD'  or
     C                               wk_NEQType = 'TOT'
      /Free
       wk_PSQPER = *Zero;
       EXEC SQL select coalesce(sum(a.PSQPER),0) into :wk_PSQPER
                from HDMPSM a
                where a.PSPPLT=1 and a.PSPPN=:wk_Item# and
                exists (select * from HDIPLT b where b.IPPLT=1 and
                 b.IPITEM=a.PSCPN and b.IPCLAS in ('3C','3U'));
       IF wk_PSQPER = *Zero;
         EXEC SQL select coalesce(sum(a.PSQPER*b.PSQPER),0)
                   into :wk_PSQPER
                  from HDMPSM a
                   join HDMPSM b on a.PSPPLT=b.PSPPLT and a.PSCPN=b.PSPPN
                  where a.PSPPLT=1 and a.PSPPN=:wk_Item# and
                  exists (select * from HDIPLT c where c.IPPLT=1 and
                   c.IPITEM=b.PSCPN and c.IPCLAS in ('3C','3U'));
       ENDIF;                                                                // wk_PSQPER=*Zero
       wk_PwdrLbs = wk_PSQPER;

       wk_IMUDN1 = *Zero;
       EXEC SQL select IMUDN1 into :wk_IMUDN1
                from HDIMST where IMITEM=:wk_Item#;
       wk_RndsPerBox = wk_IMUDN1;
      /End-Free
     C                   EVAL(H)   wk_TotPwdrLbs =
     C                               wk_PwdrLbs * wk_RoundsQty / wk_RndsPerBox
     C                   SELECT
     C                   WHEN      %upper(wk_NEQUOM) = 'KG'
     C                   EVAL(H)   wk_TotPwdrNEQ = wk_TotPwdrLbs * wk_LbsToKgCvt
     C                   WHEN      %upper(wk_NEQUOM) = 'LB'
     C                   EVAL(H)   wk_TotPwdrNEQ = wk_TotPwdrLbs
     C                   OTHER
     C                   EVAL      wk_TotPwdrNEQ = *Zero
     C                   ENDSL

     C                   ENDIF                                                  wk_NEQType='PWD' or

      **************************************************************************

     C                   IF        wk_NEQType = 'PRI'  or
     C                               wk_NEQType = 'TOT'
      /Free
       wk_TDNF01 = *Zero;
       EXEC SQL select d.TDNF01 into :wk_TDNF01
                from HDMPSM a
                 join HDMPSM b on a.PSPPLT=b.PSPPLT and a.PSCPN=b.PSPPN
                 join HDIMST c on b.PSCPN=C.IMITEM
                 join GUPTDAT d on d.TDTABL='PRIMER' and c.IMPCLS=d.TDKEY1
                where a.PSPPLT=1 and a.PSPPN=:wk_Item#;
       IF wk_TDNF01 = *Zero;
         EXEC SQL select e.TDNF01 into :wk_TDNF01
                  from HDMPSM a
                   join HDMPSM b on a.PSPPLT=b.PSPPLT and a.PSCPN=b.PSPPN
                   join HDMPSM c on b.PSPPLT=c.PSPPLT and b.PSCPN=c.PSPPN
                   join HDIMST d on c.PSCPN=d.IMITEM
                   join GUPTDAT e on e.TDTABL='PRIMER' and d.IMPCLS=e.TDKEY1
                  where a.PSPPLT=1 and a.PSPPN=:wk_Item#;
       ENDIF;                                                                // wk_TDNF01=*Zero
       wk_PrimerGrns = wk_TDNF01;

       wk_IMUDN1 = *Zero;
       EXEC SQL select IMUDN1 into :wk_IMUDN1
                from HDIMST where IMITEM=:wk_Item#;
       wk_RndsPerBox = wk_IMUDN1;
      /End-Free

     C                   EVAL      wk_TotPrimGrns =
     C                               wk_PrimerGrns * wk_RoundsQty
     C                   EVAL(H)   wk_TotPrimLbs = wk_TotPrimGrns / 7000
     C                   SELECT
     C                   WHEN      %upper(wk_NEQUOM) = 'KG'
     C                   EVAL(H)   wk_TotPrimNEQ = wk_TotPrimLbs * wk_LbsToKgCvt
     C                   WHEN      %upper(wk_NEQUOM) = 'LB'
     C                   EVAL(H)   wk_TotPrimNEQ = wk_TotPrimLbs
     C                   OTHER
     C                   EVAL      wk_TotPrimNEQ = *Zero
     C                   ENDSL

     C                   ENDIF                                                  wk_NEQType='PRI' or

      **************************************************************************

     C                   EVAL      wk_NEQ = wk_TotPwdrNEQ + wk_TotPrimNeq

     C                   ENDDO

      *
      * Return the NEQ.
      *
     C                   RETURN    wk_NEQ

     P RtvAmmoPtBxNEQ  E

     P/EJECT
      **************************************************************************
      * Retrieve primer NEQ.                                                   *
      **************************************************************************
     P RtvPrimerNEQ    B                   EXPORT

     D                 PI             6P 2
     D Item#                         15A   CONST
     D EachQty                        7P 0 CONST
     D NEQUOM                         2A   CONST
      ** KG - Kilograms, LB - Pounds

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Item#        S             15A
     D wk_EachQty      S              7P 0
     D wk_NEQUOM       S              2A
     D wk_NEQ          S              6P 2
     D wk_PrimerGrns   S              5P 3
     D wk_TotPrimGrns  S              9P 3
     D wk_TotPrimLbs   S              5P 3
     D wk_TotPrimNEQ   S              5P 2
     D wk_TDNF01       S             15P 4
     D wk_LbsToKgCvt   S              7P 7 INZ(.4535924)
     D wk_RcdCnt       S              7P 0

     C                   DO

     C                   EVAL      wk_Item# = Item#
     C                   EVAL      wk_EachQty = EachQty
     C                   EVAL      wk_NEQUOM = NEQUOM
     C                   EVAL      wk_NEQ = *Zero
     C                   EVAL      wk_PrimerGrns = *Zero
     C                   EVAL      wk_TotPrimGrns = *Zero
     C                   EVAL      wk_TotPrimLbs = *Zero
     C                   EVAL      wk_TotPrimNEQ = *Zero

      **************************************************************************

      *
      * If the item is manufactured under a different item, and the item
      * itself does not have a product structure BOM, use the item's
      * associated manufactured item's BOM.
      *
     C                   IF        %subst(wk_Item#:1:1) = 'S'  or               Seconds
     C                               %subst(wk_Item#:1:2) = 'LA'                Lot Acceptance
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HDMPSM
                where a.PSPPLT=1 and a.PSPPN=:wk_Item#;
      /End-Free
     C                   IF        wk_RcdCnt = *Zero
     C                   SELECT
     C                   WHEN      %subst(wk_Item#:1:1) = 'S'                   Seconds
     C                   EVAL      wk_Item# = %subst(wk_Item#:2:14)
     C                   WHEN      %subst(wk_Item#:1:2) = 'LA'                  Lot Acceptance
     C                   EVAL      wk_Item# = %subst(wk_Item#:3:13)
     C                   ENDSL
     C                   ENDIF                                                  wk_RcdCnt=*Zero
     C                   ENDIF                                                  %subst(wk_Item#:1:1)

      **************************************************************************

      /Free
       wk_TDNF01 = *Zero;
       EXEC SQL select b.TDNF01 into :wk_TDNF01
                from HDIMST a
                 join GUPTDAT b on b.TDTABL='PRIMER' and a.IMPCLS=b.TDKEY1
                where a.IMITEM=:wk_Item#;
       wk_PrimerGrns = wk_TDNF01;
      /End-Free

     C                   EVAL      wk_TotPrimGrns =
     C                               wk_PrimerGrns * wk_EachQty
     C                   EVAL(H)   wk_TotPrimLbs = wk_TotPrimGrns / 7000
     C                   SELECT
     C                   WHEN      %upper(wk_NEQUOM) = 'KG'
     C                   EVAL(H)   wk_TotPrimNEQ = wk_TotPrimLbs * wk_LbsToKgCvt
     C                   WHEN      %upper(wk_NEQUOM) = 'LB'
     C                   EVAL(H)   wk_TotPrimNEQ = wk_TotPrimLbs
     C                   OTHER
     C                   EVAL      wk_TotPrimNEQ = *Zero
     C                   ENDSL

     C                   EVAL      wk_NEQ = wk_TotPrimNeq

     C                   ENDDO

      *
      * Return the NEQ.
      *
     C                   RETURN    wk_NEQ

     P RtvPrimerNEQ    E

