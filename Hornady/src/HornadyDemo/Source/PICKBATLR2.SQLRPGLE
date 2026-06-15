     H option(*nodebugio:*srcstmt) dftactgrp(*no) bnddir('PICKBATLR2')
      **************************************************************************
      * APPLICATION: Pick list                                                 *
      *       TITLE: Load Pick List BATCH                                      *
      *          BY: Glenn Hopwood                                             *
      *        DATE: Nov 2023                                                  *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  06/13/2024  MCA  When retrieving group number, ensure all customers   *
      *                   in the current pick batch are not already in the     *
      *                   group for a prior (current day's) shipment.          *
      *  06/13/2024  MCA  Fix HYPSGCU.GCSTNM population for foreign shipments  *
      *                   being drop-shipped (but not being drop-shipped to a  *
      *                   U.S. forwarder - those were already good).           *
      *  06/13/2024  MCA  Remove all references to BARDATA1 (used for the      *
      *                   BARDATA2.0 conversion).                              *
      *  08/08/2024  MCA  Fix bug.  When creating master-pack HYPSGDT records, *
      *                   and there is a full-case quantity remainder, don't   *
      *                   combine that remainder quantity into one HYPSGDT     *
      *                   record.  The remainder should be split into one      *
      *                   HYPSGDT record PER case.                             *
      *  04/29/2025  MCA  For isForeign subprocedure, also check drop-ship     *
      *                   state and country (when applicable).                 *
      *  12/03/2025  RBM  Modified to handle MegaBatch publishing.             *
      *        - Added support for override shipment group (in_OvrGrp#) passed *
      *          from PICKBATR when publishing component batches inside a      *
      *          Mega Batch.                                                   *
      *                                                                        *
      *        - When in_OvrGrp# > 0, all HYPSGHD / HYPSGCU / HYPSGDT records  *
      *          use the override group number instead of generating a new     *
      *          group via getShipGrpID#.                                      *
      *                                                                        *
      *        - Updated logic so OE order header lookup uses Turn# (IHTURN)   *
      *          instead of Order# (IHORD#), matching the actual key of file   *
      *          OEORHP and ensuring correct order retrieval during publish.   *
      *                                                                        *
      *        - Ensures all pick batches within a Mega Batch publish into the *
      *          same shipment group, allowing HYR0600 to show a single        *
      *          consolidated shipment as designed.                            *
      *                                                                        *
      *        - Verified behavior: component batches now correctly generate   *
      *          CU and DT rows under one shipment group, and HYPSGDT records  *
      *          are no longer fragmented across multiple groups.              *
      *  03/03/2026  RBM  Mega batch quantity rollup.                          *
      *          in_OvrGrp# Added *omit to options(*nopass:*omit)              *
      *          to allow callers to omit                                      *
      *          this parameter while passing in_MegaBatch#.                   *
      *  03/17/2026  RBM  Add %Omitted test for in_Ovrgrp                      *
      *  04/23/2026  RBM  Idempotent group reuse. If shipment records          *
      *          already exist for a batch reuse the existing open group       *
      *          rather than creating a new one.                               *
      **************************************************************************

     FPICKBATHP IF A E           K DISK
     FPICKBATDL1IF A E           K DISK    prefix(dt_)
     FHYPSGHD   UF A E           K DISK
     FHYPSGCU   UF A E           K DISK
     FHYPSGDT   UF A E           K DISK
     FHYPSGDTU  UF   E           K DISK    EXTDESC('HYPSGDT') EXTFILE(*EXTDESC)
     F                                     RENAME(HYRSGDT:HYRSGDTU) USROPN
     FHYLSGDT3  UF   E           K DISK    RENAME(HYRSGDT:HYRSGDT3) prefix(d_)
     FOEORHP    IF   E           K DISK
     FOEORHD    IF   E           K DISK
     FOEORDP    IF   E           K DISK    USROPN
     F*OEORDP01  IF   E           K DISK
     FOEORDT    IF   E           K DISK
     FHDIWHS    IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FGUPTDAT   IF   E           K DISK
     FHDDSHP    IF   E           K DISK
     FHYLSGSD1  IF   E           K DISK

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D* QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D*  Command                     500A   CONST
     D*  CommandLen                   15P 5 CONST

      /COPY QPRPSRC,HYR0610
      /COPY QPRPSRC,CHKDIGIT

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D PICKBATLR2      PI
     D  in_Batch                           like(PICKBAT)
     D  out_ShpGrp                         like(GHGRP#)
     D  in_OvrGrp#                         like(PICKBAT) options(*nopass:*omit)
     D  in_MegaBatch#                      like(PICKBAT) options(*nopass)

      **************************************************************************
      * Build O/E work rcds                                                    *
      **************************************************************************
     D HYR0120         PR                  EXTPROC('HYR0120')
     D  Order#                        8P 0 CONST
     D  Turnaround#                   9P 0 CONST
     D  Control#                      8P 0

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_             DS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263
     D  ps_Job#              264    269
     D  ps_CurUser           358    367

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D*                DS
     D* TimeDate                1     14S 0
     D*  td_Time                1      6S 0
     D*  td_Date                7     14S 0

      **************************************************************************
      * Define arrays.                                                         *
      **************************************************************************
     D*Cmd             S             80    DIM(4) CTDATA                        CL commands

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_SSCCSrlRef   S              9P 0 DTAARA(SSCCSRLREF)                   SSCC serial no.

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D sqlError        S               n
     D wk_InvLoc       S              4A
     D wk_ShpGrp       S                   like(GHGRP#)
     D wk_ShpGrpSts    S              1A   inz(' ')
     D wk_ShpGrpChk    S              1N   inz(*off)
     D wk_Seq          S                   like(dt_PICKSEQ)
     D wk_TimeStmp     S               z
     D hld_Seq         S                   like(dt_PICKSEQ)
     D HYPSGDTSeq      S                   like(GDTSEQ)
     D wk_PROCnt       S             10I 0
     D wk_Turn         S                   like(IHTURN)
     D wk_Ord          S              9p 0
     D wk_OrdCtl#      S              8S 0
     D wk_GrpLowLim    S              9S 0
     D wk_Lane1        S              1S 0
     D wk_Lane2        S              1S 0
     D wk_Lane3        S              1S 0
     D wk_Lanes        S              1S 0
     D wk_ShipType     S              1A
     D wk_ShipVia      S              2A
     D wk_Timestamp    S               Z
     D wk_Error        S             10I 0
     D wk_LovalTS      S               Z
     D wk_DatabaseID@  S              2A
     D wk_STName       S             30A
     D wk_STZip        S             13A
     D wk_BillTo#X     S              7A
     D wk_BuyGrpCust   S              1N
     D wk_CMCNA1       S             26A
     D wk_CMMXPO       S              1A
     D wk_CMMXLC       S              1A
     D wk_ExistingGrp  S                   like(GHGRP#)
     D wk_CMPRCN       S              1A
     D wk_CMPLLB       S              1A
     D wk_CMSLLC       S              1A
     D wk_IMPLLB       S              1A
     D wk_AlwPlltLbls  S              1A
     D wk_OETFRT       S              9P 2
     D wk_OETMSG       S              9P 2
     D wk_OEUDF1       S             15A
     D wk_ODITEM       S             15A
     D wk_ODIMDS       S             30A
     D wk_ODPCLS       S              4A
     D wk_ODWH         S              3S 0
     D wk_ODQORD       S             13P 4
     D wk_ODQSTD       S             13P 4
     D wk_ODQSTC       S             13P 4
     D wk_ODOREC       S              1A
     D wk_IMPCLS       S              4A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN4       S             13P 5
     D wk_IMLOT        S              1A
     D wk_IMUOMS       S              2A
     D wk_IMCNVY       S              1A
     D wk_IMPLTQ       S              7P 0
     D wk_IMPLTQ@U     S              9P 0
     D wk_PlltQty      S              7P 0
     D wk_MSTPkQty     S              7P 0
     D wk_MSTPkCnt     S              5P 0
     D wk_PlltQty@U    S              9P 0
     D wk_BDITEM       S             15A
     D wk_BDUPC12      S             12A
     D wk_BDUCC14      S             14A
     D wk_BDUCC14I     S             14A
     D wk_BDQTYIPC     S              5P 0
     D wk_BDQTYBAR     S              7S 0
     D wk_IWPCPC       S              9P 4
     D hld_IWPCPC      S              9P 0
     D wk_GCGRP        S              9S 0
     D wk_GCCPTY       S              3S 0
     D wk_GCBLTO       S              7S 0
     D wk_GCSHTO       S              7S 0
     D wk_GCDSHP       S              7S 0
     D wk_GCSHPV       S              2A
     D wk_GCSVDS       S             15A
     D wk_GCFTRM       S              2A
     D wk_GCFRTA       S              7P 2
     D wk_GCCHGA       S              7P 2
     D wk_GCITCT       S              5P 0
     D wk_GCSHWT       S              5P 0
     D wk_GCPANS       S              1A
     D wk_IHBLTOx      S              7A
     D wk_TDTABL       S             10A
     D wk_TDKEY1       S             15A
     D wk_TDKEY2       S             15A
     D wk_TDCF03       S             30A
     D wk_LotCtrl      S              1A
     D wk_TblLotCode   S              2A
     D wk_LotEntry     S              2A
     D wk_MixLotCtns   S              1A
     D wk_PrtlCtns     S              1A
     D wk_CtnItem#     S             15A
     D wk_QtyOpen      S             13P 4
     D wk_PctPlltQty   S              5P 3
     D wk_LblLoc       S              1A
     D wk_Bulk         S              1N
     D wk_CaseQty      S              5P 0
     D wk_W1Cnt        S              5P 0
     D wk_PickUnitWgt  S              9P 5
     D wk_PickUnitQty  S              9P 0
     D wk_PickUOM      S              2A
     D wk_PickLOC      S             17A
     D wk_CompPlt      S              1A
     D wk_PltLotCnt    S              5P 0
     D wk_ItemBC       S             14A
     D wk_SSCCBC       S             20A
     D wk_SSCC         S              9P 0
     D wk_StkUOMQty    S              9P 0
     D wk_FullPlts     S              5P 0
     D wk_FullCases    S              5P 0
     D wk_PrtlUnits    S              5P 0
     D wk_PltUnits     S              9P 0
     D wk_PltCnt       S              5P 0
     D wk_PickMode     S              1A
     D wk_ToteCases    S              5P 0
     D wk_FrtTerms     S              2A
     D wk_FrtAmt       S              7P 2
     D wk_SpclChg      S              7P 2
     D wk_PickQty      S              9P 0
     D hld_PickQty     S              9P 0
     D rem_PickQty     S              9P 0
     D hld_LoopCnt     S              5P 0
     D hld_CurrLoop    S              5P 0
     D hld_Extra       S              5P 0
     D wk_CaseCnt      S              5P 0
     D wk_Serial#      S              9P 0
     D wk_ChkDgt       S              1A
     D wk_Seq#         S                   LIKE(GDRSEQ)
     D wk_RC0185       S             10I 0
     D wk_Control#     S              8P 0
     D wk_CustClass    S                   LIKE(CMCCLS)
     D wk_Foreign      S               N
     D wk_RcdCnt       S              7P 0
     D wk_QtyBatch     S                   Like(PICKBAT)
     D hld_Result      S             10I 0
     D kf_TABL         S                   LIKE(TDTABL)
     D kf_KEY1         S                   LIKE(TDKEY1)
     D sv_TURN         S                   LIKE(GDTURN)
     D sv_TSEQ         S                   LIKE(GDTSEQ)
     D sv_PickMode     S                   LIKE(wk_PickMode)
     D gotHYPSGSD      S               N
     D hld_CDDS#       S                   LIKE(CDDS#)
     D hld_CDBLTO      S                   LIKE(CDBLTO)
     D hld_CDSHTO      S                   LIKE(CDSHTO)
     D hld_CDDSHP      S                   LIKE(CDDSHP)
     D hld_CDSTYP      S                   LIKE(CDSTYP)
     D hld_CDSHPV      S                   LIKE(CDSHPV)
      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D*NONE            C                   ' 5'

      **************************************************************************
      * Create shipment records.                                               *
      *   Return values: 0-No error                                            *
      *                  1-Invalid turnaround number                           *
      *                  2-Ship-to name not found                              *
      **************************************************************************
     D crtShipRcds2    PR            10I 0
     D  ShipGrp#                      9S 0 CONST
     D  Turn#                         9S 0 CONST
     D  OrdCtl#                       8S 0 CONST
     D  toteid                       50a   const
     D  QtyBatch                      8P 0 Const

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************
      /free

       exec sql
         set option
           naming = *sys,
           commit = *none,
           usrprf = *user,
           dynusrprf = *user,
           datfmt = *iso,
           closqlcsr = *endmod;

       chain (in_Batch) PICKBATHP;
         wk_InvLoc = PICKINVLOC;
         wk_TimeStmp = %timestamp();

        // Check if shipment records already exist for this batch.
        // If so reuse the existing group - makes re-runs idempotent.
         wk_ExistingGrp = 0;
         exec sql
           select d.GDGRP# into :wk_ExistingGrp
             from HYPSGDT d
             join PICKBATDP p
               on p.PICKTURN = d.GDTURN
              and p.PICKORD  = d.GDORD#
             join HYPSGHD h
               on h.GHGRP#  = d.GDGRP#
            where p.PICKBAT  = :in_Batch
              and h.GHSTS    = 'O'
              and d.GDDVRT   = 999
            fetch first row only;

         if SQLCODE = 0 and wk_ExistingGrp > 0;
           wk_ShpGrp = wk_ExistingGrp;
           exec sql CALL SYSTOOLS.LPRINTF(
             'PICKBATLR2 reusing existing group='
             concat trim(char(:wk_ExistingGrp))
             concat ' for batch=' concat trim(char(:in_Batch)));
           exec sql
             insert into PICKBATLOG
              (LOGTS, LOGBAT, LOGGRP, LOGMSG, LOGUSR, LOGJOB, LOGJOB#)
             values(
               current_timestamp,
               :in_Batch,
               :wk_ExistingGrp,
               'Duplicate LoadShip blocked - reused existing group',
               :ps_CurUser,
               :ps_Job,
               :ps_Job#);
         else;
          if %parms >= %parmnum(in_OvrGrp#) and not %omitted(in_OvrGrp#)
            and in_OvrGrp# > 0;
              wk_ShpGrp = in_OvrGrp#;                      // use provided ship group
          else;
              wk_ShpGrp = getShipGrpID#(wk_Invloc: wk_TimeStmp); // existing behavior
          endif;
        endif;

         wk_ShpGrpSts = *Blanks;
         // If the requested ship group already exists and is not OPEN,
         // do NOT reuse it (prevents combining new work into a completed/closed shipment).
         exec sql
           select GHSTS into :wk_ShpGrpSts
             from HYPSGHD
            where GHGRP# = :wk_ShpGrp
            fetch first 1 row only;
         if SQLCODE = 0 and wk_ShpGrpSts <> 'O';
           wk_ShpGrp = getShipGrpID#(wk_Invloc: wk_TimeStmp);
         endif;

         exec sql
           merge into HYPSGHD h
           using (values(:wk_ShpGrp, :wk_InvLoc, :ps_User))
                as s(GHGRP#, GHDCLOC, GHUSER)
              on h.GHGRP# = s.GHGRP#
           when matched then
                update set GHDCLOC = s.GHDCLOC
           when not matched then
                insert (GHGRP#, GHSTS, GHDCLOC, GHUSER, GHTSTP1)
                values (s.GHGRP#, 'O', s.GHDCLOC, s.GHUSER, current timestamp);
         out_ShpGrp = wk_ShpGrp;
         //gotHYPSGSD = *off;
         // Resolve batch set anchor for qty rollup.
         // - Mega batch: wk_QtyBatch = in_OvrGrp# so PICKMBATDP children are used.
         // - Normal batch: wk_QtyBatch = in_Batch so only this batch is used.
         if %parms >= %parmnum(in_MegaBatch#) and in_MegaBatch# > 0;
           wk_QtyBatch = in_MegaBatch#;
         else;
           wk_QtyBatch = in_Batch;
         endif;

         exec sql
         declare global temporary table src_orders
         ( ord# dec(9,0) not null,
           turn# dec(9,0) not null);

         exec sql delete from src_orders;


         exec sql
          insert into src_orders (ord#, turn#)
          select distinct pickord, pickturn
           from pickbatdp
          where pickbat = :in_Batch;

          exsr dclCursorMain;
          exec sql
            open cursorMain;

          if sqlError = *off;
            exec sql
              fetch cursorMain into :wk_Ord,:wk_Turn;

         dow SQLCODE = 0;

       //    if gotHYPSGSD = *off;
       //      chain (wk_Turn) HYLSGSD1;
       //      if %found(HYLSGSD1);
       //        hld_CDDS# = CDDS#;
       //        hld_CDSTYP = CDSTYP;
       //        hld_CDSHPV = CDSHPV;
       //        gotHYPSGSD = *on;
       //
       //        // Update all of the HYPSGSD rows with the same CDDS#
       //        exec sql update HYPSGSD
       //                 set CDTSTP2 = CURRENT TIMESTAMP
       //                 where CDDS# = :hld_CDDS#;
       //      endif;
       //    endif;

           chain (wk_Turn) OEORHP;
           if %found(OEORHP);

             // Build O/E-update work records.
             HYR0120 (IHORD#:wk_Turn:wk_Control#);

             // create the HYPSGCU record
             exsr crtHYPSGCU;

           // Delete only the HYPSGDT lines that belong to in_Batch for this
           // turn/order.  Scoping by order line prevents a second component
           // batch from wiping rows already written by an earlier component
           // batch that shares the same shipment group, order, and turn.
             exec sql
               delete from HYPSGDT
                  where GDGRP# = :wk_ShpGrp
                     and GDORD#  = :wk_Ord
                     and GDTURN  = :wk_Turn
                     and GDORL#  in (
                       select distinct PICKORDL
                         from PICKBATDP
                        where PICKBAT  = :in_Batch
                         and PICKORD  = :wk_Ord
                         and PICKTURN = :wk_Turn
                   );

             // Create the HYPSGDT records based on OEORDP
             hld_Result = crtShipRcds2(wk_ShpGrp:wk_Turn:wk_Control#:PICKTOT:
                wk_QtyBatch) ;

           endif;

           exec sql
             fetch cursorMain into :wk_Ord, :wk_Turn;
         enddo;

       endif;

       exec sql close cursorMain;

       exsr updShpFlds;

       *inlr = *on;
       return;

       //-----------------------------------------------------------------------
       // updShpFlds - Update the shipment fields
       //   GCSTYP, GCSHPV and GCSVDS
       //-----------------------------------------------------------------------
       begsr updShpFlds;

         hld_CDSHPV = *blanks;

         setll (wk_ShpGrp) HYPSGCU;
         reade (wk_ShpGrp) HYPSGCU;
         dow not %eof(HYPSGCU);

           gotHYPSGSD = *off;

           setll (GCGRP#:GCBLTO:
                  GCSHTO:GCDSHP) HYLSGDT3;
           reade (GCGRP#:GCBLTO:
                  GCSHTO:GCDSHP) HYLSGDT3;
           dow not %eof(HYLSGDT3);
             chain (d_GDTURN) HYLSGSD1;
             if %found(HYLSGSD1);
               hld_CDDS# = CDDS#;
               hld_CDBLTO = CDBLTO;
               hld_CDSHTO = CDSHTO;
               hld_CDDSHP = CDDSHP;
               hld_CDSTYP = CDSTYP;
               hld_CDSHPV = CDSHPV;
               gotHYPSGSD = *on;

               // Update all of the HYPSGSD rows with the same CDDS#
               exec sql update HYPSGSD
                        set CDTSTP2 = CURRENT TIMESTAMP,
                            CDSTYP = :hld_CDSTYP,
                            CDSHPV = :hld_CDSHPV
                        where CDDS# = :hld_CDDS# and
                              CDBLTO = :hld_CDBLTO and
                              CDSHTO = :hld_CDSHTO and
                              CDDSHP = :hld_CDDSHP;

               leave;
             endif;
             reade (GCGRP#:GCBLTO:
                    GCSHTO:GCDSHP) HYLSGDT3;
           enddo;

           if gotHYPSGSD = *on;

             GCSTYP = hld_CDSTYP;
             GCSHPV = hld_CDSHPV;
             gotHYPSGSD = *off;

           else;

             // decide Ship Via
             //   if it's a foreign shipment
         //  if isForeign(wk_ShpGrp:GCBLTO:GCSHTO);
             if isForeign(wk_ShpGrp:GCBLTO:GCSHTO:GCDSHP);
               GCSTYP = 'T';
               GCSHPV = 'BW';
             //   if it's an in-house order NOT being drop shipped
             elseif (GCSHTO = 110731 or GCSHTO = 410) and
                     GCDSHP = 0;
               GCSTYP = 'P';
               GCSHPV = 'PU';
             elseif GCSHWT < 200;
               GCSTYP = 'P';
               GCSHPV = 'FE';
             elseif GCSHWT >= 200 and GCSHWT < 1200;
               GCSTYP = 'T';
               GCSHPV = 'OD';
             else;
               GCSTYP = 'T';
               GCSHPV = 'AB';
             endif;

             // look for an override...
             wk_TDKEY1 = %editc(GCBLTO:'X');
             chain ('SHIPCARR':wk_TDKEY1:wk_DatabaseID:GCSHPV) GUPTDAT;
             if %found(GUPTDAT);
               GCSHPV = TDCF01;
             endif;

           endif;

           select;
             when GCSHPV = 'PU';
               GCSVDS = 'PICKED UP';
             when GCSHPV = 'FE';
               GCSVDS = 'FEDEX GROUND';
             when GCSHPV = 'FT';
               GCSVDS = 'FDX FREIGHT';
             when GCSHPV = 'OD';
               GCSVDS = 'OLD DOMINION';
             when GCSHPV = 'AB';
               GCSVDS = 'ABF FREIGHT';
             when GCSHPV = 'UP';
               GCSVDS = 'UPS GROUND';
             other;
               GCSVDS = 'BEST WAY';
           endsl;

           update HYRSGCU;
           reade (wk_ShpGrp) HYPSGCU;
         enddo;

       endsr;

       //-----------------------------------------------------------------------
       // dclCursorMain -
       //-----------------------------------------------------------------------
       begsr dclCursorMain;

         exec sql declare cursorMain cursor for
           select
             ord#, turn#
           from src_orders
           for read only;

         if SQLCODE < 0;
           sqlError = *on;
         endif;

       endsr;

       //-----------------------------------------------------------------------
       // crtHYPSGCU - create the HYPSGCU row (unless it exists)
       //-----------------------------------------------------------------------
       begsr crtHYPSGCU;

         chain (wk_ShpGrp:IHBLTO:IHSHTO:IHDSHP) HYPSGCU;
         if not %found(HYPSGCU);

           GCGRP# = wk_ShpGrp;
           GCCPTY = getShipPty(wk_Turn);
           GCBLTO = IHBLTO;
           chain (IHBLTO) HDCUST;
           if %found(HDCUST);
             GCBTNM = CMCNA1;
             wk_CustClass = CMCCLS;
           endif;
           GCSHTO = IHSHTO;
           exsr setAddress;
           GCDSHP = IHDSHP;
           GCSTS = 'O';
           GCLANES = 1;
           GCSTYP = 'P';
           chain (IHORD#) OEORHD;
           if %found(OEORHD);
             // According to Bill
             //GCFTRM = %subst(OEUDF1:1:2);
           endif;
           GCSHPV = 'BW';
           GCSVDS = 'BEST WAY';
           GCMLLI = '0';
           GCMLLC = '0';
           GCTSTP1 = wk_TimeStmp;
           write HYRSGCU;

         endif;

       endsr;

       //-----------------------------------------------------------------------
       // setAddress - Figure out the address to use...
       //-----------------------------------------------------------------------
       begsr setAddress;

         // Is it a foreign shipment?
         chain ('CUSTCLASS':wk_CustClass) GUPTDAT;
         if %found(GUPTDAT) and TDFF02 = 'Y';
           wk_Foreign = *on;
         else;
           wk_Foreign = *off;
         endif;

         if IHDSHP = 0 or wk_Foreign;
           chain (IHSHTO) HDCUST;
           if %found(HDCUST);
             GCSTNM = CMCNA1;
             GCSTZP = CMZIP;
             GCSTST = CMST;
             GCSTCT = CMCTRY;
           endif;
           if IHDSHP <> 0;
             chain ('C':IHSHTO:IHDSHP) HDDSHP;
             if %found(HDDSHP);
               if DSCTRY <> 'US'  and  DSCTRY <> *Blanks;
                 GCSTNM = DSNAME;
               endif;                                                        // DSCTRY<>'US' and
               GCSTZP = DSZIP;
               GCSTST = DSST;
               GCSTCT = DSCTRY;
             endif;                                                          // %found(HDDSHP)
           endif;                                                            // IHDSHP<>0
         else;
           chain ('C':IHSHTO:IHDSHP) HDDSHP;
           if %found(HDDSHP);
             GCSTNM = DSNAME;
             GCSTZP = DSZIP;
             GCSTST = DSST;
             GCSTCT = DSCTRY;
           endif;
         endif;

         wk_STName = GCSTNM;

       endsr;

      /end-free

      **************************************************************************
      * Get Ship Group Record ID#
      *   Return values: group ID#
      **************************************************************************
     P getShipGrpID#   B
     D                 PI             9S 0
     D DCLoc                          4A   CONST
     D Timestamp                       Z   CONST

     D wk_Date         S               d   inz
     D outShpGrp       S                   like(GHGRP#)

      /free

        outShpGrp = 0;
        wk_Date = %date();
        wk_GrpLowLim = *Zero;

       DOW outShpGrp = 0;

        EXEC SQL
          select a.GHGRP# into :outShpGrp from HYPSGHD a
           where date(a.GHTSTP1)= :wk_Date and
                 a.GHSTS= 'O' and
                 a.GHDCLOC= :DCLoc
                 and a.GHGRP#>:wk_GrpLowLim
           and exists
            (select * from HYPSGDT b where b.GDGRP#=a.GHGRP# and b.GDDVRT=999)
           order by GHGRP#
           fetch first row only;

        // Ensure customer is not already in the group for a prior
        // (current day's) shipment.

        IF outShpGrp = 0;
          LEAVE;
        ELSE;
          wk_RcdCnt = *Zero;
          EXEC SQL select count(*) into :wk_RcdCnt
                   from PICKBATDP a join OEORHP b
                    on a.PICKTURN=b.IHTURN
                   where a.PICKBAT=:in_Batch and
                    exists (select * from HYPSGCU c where
                    c.GCGRP#=:outShpGrp and c.GCBLTO=b.IHBLTO and
                    c.GCSHTO=b.IHSHTO and c.GCDSHP=b.IHDSHP);
        IF wk_RcdCnt = *Zero;
          LEAVE;
        ELSE;
          wk_GrpLowLim = outShpGrp;
          outShpGrp = 0;
          ITER;
        ENDIF;                                                               // wk_RcdCnt=*Zero
        ENDIF;                                                               // outShpGrp=0

       ENDDO;                                                                // outShpGrp=0

        if outShpGrp = 0;
          outShpGrp = CrtShipGrp(DCLoc:Timestamp);
        endif;

        return outShpGrp;

      /end-free

     P getShipGrpID#   E

      **************************************************************************
      * Get Shipment priority
      *   Return values: shipment priority
      **************************************************************************
     P getShipPty      B
     D                 PI             3  0
     D inTurn                              LIKE(GDTURN) CONST

     D wk_Pty          S              3  0 inz

      /free

       wk_Pty = 88;

       EXEC SQL
         select coalesce(min(b.ODUDN5),99) into :wk_Pty
           from OEORDP a
           join OEORDT b on (a.IDORD#,a.IDORL#)=(b.ODORD#,b.ODORL#)
           where a.IDTURN = :inTurn;

       return wk_Pty;

      /end-free

     P getShipPty      E

      **************************************************************************
      * Decide if it's a foreign shipment
      *   Return values: on/off
      **************************************************************************
     P isForeign       B
     D                 PI              n
     D inShpGrp                            LIKE(wk_ShpGrp) CONST
     D inBillTo                            LIKE(GCBLTO) CONST
     D inShipTo                            LIKE(GCSHTO) CONST
     D inDropShip                          LIKE(GCDSHP) CONST

     D wk_Foreign      S               n   inz

      /free

       // default to *OFF
       wk_Foreign = *off;

       // Check the customer class of the bill-to. If it's foreign then bail
       //   with an *ON
       chain (inBillTo) HDCUST;
       if %found(HDCUST);
         // Is it a foreign shipment?
         chain ('CUSTCLASS':CMCCLS) GUPTDAT;
         if %found(GUPTDAT) and TDFF02 = 'Y';
           return *on;
         endif;
       endif;

       // Check the state/country of the ship-to.
       chain (inShipTo) HDCUST;
       if %found(HDCUST);
         if (CMCTRY <> 'US'  and  CMCTRY <> *Blanks)  or
            (CMST = 'AK'  or  CMST = 'HI'  or
             CMST = 'AS'  or  CMST = 'GU'  or
             CMST = 'MP'  or  CMST = 'PR'  or
             CMST = 'VI');
           return *on;
         endif;
       endif;

       // Check the state/country of the drop-ship (if any).
       if inDropShip <> *Zero;
         chain ('C':inShipTo:inDropShip) HDDSHP;
         if %found(HDCUST);
           if (DSCTRY <> 'US'  and  DSCTRY <> *Blanks)  or
              (DSST = 'AK'  or  DSST = 'HI'  or
               DSST = 'AS'  or  DSST = 'GU'  or
               DSST = 'MP'  or  DSST = 'PR'  or
               DSST = 'VI');
             return *on;
           endif;
         endif;
       endif;

       return wk_Foreign;

      /end-free

     P isForeign       E

     P/EJECT
      **************************************************************************
      * Create shipment records.                                               *
      *   Return values: 0-No error                                            *
      *                  1-Invalid turnaround number                           *
      *                  2-Ship-to name not found                              *
      *                  3-Shipment group customer record not found            *
      **************************************************************************
     P crtShipRcds2    B                   EXPORT
     D                 PI            10I 0
     D ShipGrp#                       9S 0 CONST
     D Turn#                          9S 0 CONST
     D OrdCtl#                        8S 0 CONST
     D toteid                        50a   const
     D QtyBatch                       8p 0 const

     D wk_ShipGrp#     S              9S 0
     D wk_Turn#        S              9S 0
     D wk_OrdCtl#      S              8S 0
     D wk_Error        S             10I 0
     D wk_LovalTS      S               Z
     D wk_DatabaseID@  S              2A
     D wk_STName       S             30A
     D wk_STZip        S             13A
     D wk_BillTo#X     S              7A
     D wk_BuyGrpCust   S              1N
     D wk_CMMXPO       S              1A
     D wk_CMMXLC       S              1A
     D wk_CMPRCN       S              1A
     D wk_CMPLLB       S              1A
     D wk_CMSLLC       S              1A
     D wk_IMPLLB       S              1A
     D wk_AlwPlltLbls  S              1A
     D wk_OETFRT       S              9P 2
     D wk_OETMSG       S              9P 2
     D wk_OEUDF1       S             15A
     D wk_ODITEM       S             15A
     D wk_ODIMDS       S             30A
     D wk_ODPCLS       S              4A
     D wk_ODWH         S              3S 0
     D wk_ODQORD       S             13P 4
     D wk_ODQSTD       S             13P 4
     D wk_ODQSTC       S             13P 4
     D wk_ODOREC       S              1A
     D wk_IMPCLS       S              4A
     D wk_IMIMWG       S              7P 3
     D wk_IMUDN4       S             13P 5
     D wk_IMLOT        S              1A
     D wk_IMUOMS       S              2A
     D wk_IMCNVY       S              1A
     D wk_IMPLTQ       S              7P 0
     D wk_IMPLTQ@U     S              9P 0
     D wk_PlltQty      S              7P 0
     D wk_PlltQty@U    S              9P 0
     D wk_BDITEM       S             15A
     D wk_BDUPC12      S             12A
     D wk_BDUCC14      S             14A
     D wk_BDUCC14I     S             14A
     D wk_BDQTYIPC     S              5P 0
     D wk_IWPCPC       S              9P 4
     D wk_GCGRP        S              9S 0
     D wk_GCCPTY       S              3S 0
     D wk_GCBLTO       S              7S 0
     D wk_GCSHTO       S              7S 0
     D wk_GCDSHP       S              7S 0
     D wk_GCSHPV       S              2A
     D wk_GCSVDS       S             15A
     D wk_GCFTRM       S              2A
     D wk_GCFRTA       S              7P 2
     D wk_GCCHGA       S              7P 2
     D wk_GCITCT       S              5P 0
     D wk_GCSHWT       S              5P 0
     D wk_GCPANS       S              1A
     D wk_GDPANS       S              1A
     D wk_IHBLTOx      S              7A
     D wk_TDTABL       S             10A
     D wk_TDKEY1       S             15A
     D wk_TDKEY2       S             15A
     D wk_TDCF03       S             30A
     D wk_LotCtrl      S              1A
     D wk_TblLotCode   S              2A
     D wk_LotEntry     S              2A
     D wk_MixLotCtns   S              1A
     D wk_PrtlCtns     S              1A
     D wk_CtnItem#     S             15A
     D wk_QtyOpen      S             13P 4
     D wk_PctPlltQty   S              5P 3
     D wk_LblLoc       S              1A
     D wk_Bulk         S              1N
     D wk_CaseQty      S              5P 0
     D wk_W1Cnt        S              5P 0
     D wk_PickUnitWgt  S              9P 5
     D wk_PickUnitQty  S              9P 0
     D wk_PickUOM      S              2A
     D wk_PickLOC      S             17A
     D wk_CompPlt      S              1A
     D wk_PltLotCnt    S              5P 0
     D wk_ItemBC       S             14A
     D wk_SSCCBC       S             20A
     D wk_SSCC         S              9P 0
     D wk_StkUOMQty    S              9P 0
     D wk_FullPlts     S              5P 0
     D wk_FullCases    S              5P 0
     D wk_PrtlUnits    S              5P 0
     D wk_PltUnits     S              9P 0
     D wk_PltCnt       S              5P 0
     D wk_PickMode     S              1A
     D wk_ToteCases    S              5P 0
     D wk_NoCases      S               N
     D wk_FrtTerms     S              2A
     D wk_FrtAmt       S              7P 2
     D wk_SpclChg      S              7P 2
     D wk_PickQty      S              9P 0
     D wk_CaseCnt      S              5P 0
     D wk_Serial#      S              9P 0
     D wk_ChkDgt       S              1A
     D wk_Seq#         S                   LIKE(GDRSEQ)
     D wk_RC0185       S             10I 0
     D sv_TURN         S                   LIKE(GDTURN)
     D sv_TSEQ         S                   LIKE(GDTSEQ)
     D sv_PickMode     S                   LIKE(wk_PickMode)
     D wk_NeedQty      S             13P 4
     D wk_PickedQty    S             13P 4
     D wk_sqlcode      S             10i 0
     D**** wk_MSTPKRem     s              7p 0
     D wk_MSTPKLines   s             10i 0
     D

     C                   DO

     C                   EVAL      wk_ShipGrp# = ShipGrp#
     C                   EVAL      wk_Turn# = Turn#
     C                   EVAL      wk_OrdCtl# = OrdCtl#
     C                   EVAL      wk_Error = *Zero
     C                   IF        not %open(OEORHP)
     C                   OPEN      OEORHP
     C                   ENDIF                                                  not %open(OEORHP)

     C                   IF        not %open(OEORDP)
     C                   OPEN      OEORDP
     C                   ENDIF                                                  not %open(OEORDP)

     C                   IF        wk_DatabaseID = *Blanks
     C                   IN        wk_DatabaseID
     C                   ENDIF                                                  wk_DatabaseID=*Blank
     C                   EVAL      wk_DatabaseID@ = wk_DatabaseID               Copy to local var

     C                   EVAL      wk_LovalTS = *Loval

     C     wk_Turn#      CHAIN     OEORHP
     C                   IF        not %found(OEORHP)
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  not %found(OEORHP)

      /Free
       wk_STName = *Blanks;
       wk_STZip = *Blanks;
       IF IHDSHP = *Zero;
         EXEC SQL select CMCNA1, CMZIP into :wk_STName, :wk_STZip
                  from HDCUST
                  where CMCUST=:IHSHTO;
       ELSE;
         EXEC SQL select DSNAME, DSZIP into :wk_STName, :wk_STZip
                  from HDDSHP
                  where DSVCF='C' and DSVNCS=:IHSHTO and DSNMBR=:IHDSHP;
       ENDIF;
       IF wk_STName= *Blanks;
         EVAL wk_Error = 2;
         LEAVE;
       ENDIF;                                                                // wk_STName=*Blanks

       wk_BillTo#X = %editc(IHBLTO:'X');
       wk_TDKEY2 = *Blanks;
       EXEC SQL select TDKEY2 into :wk_TDKEY2
                from GUPTDAT
                where TDTABL='BUYINGGRPS' and TDKEY1=:wk_DatabaseID@ and
                 TDKEY2=:wk_BillTo#X;
       IF wk_TDKEY2 = wk_BillTo#X;
         wk_BuyGrpCust = *ON;
       ELSE;
         wk_BuyGrpCust = *OFF;
       ENDIF;                                                                // wk_TDKEY2=wk_BillTo#

       wk_CMMXPO = *Blank;
       wk_CMMXLC = *Blank;
       wk_CMPRCN = *Blank;
       wk_CMPLLB = *Blank;
       wk_CMSLLC = *Blank;
       IF wk_BuyGrpCust;
         EXEC SQL select CMMXPO, CMMXLC, CMPRCN, CMPLLB,
                         CMSLLC
                  into :wk_CMMXPO, :wk_CMMXLC, :wk_CMPRCN, :wk_CMPLLB,
                       :wk_CMSLLC
                  from HDCUST2
                  where CMCUST=:IHSHTO;
       ELSE;
           EXEC SQL select CMMXPO, CMMXLC, CMPRCN, CMPLLB,
                           CMSLLC
                    into :wk_CMMXPO, :wk_CMMXLC, :wk_CMPRCN, :wk_CMPLLB,
                         :wk_CMSLLC
                    from HDCUST2
                    where CMCUST=:IHBLTO;
       ENDIF;                                                                // wk_BuyGrpCust

      // Expected ship-label location values - T=Top, F=Front, M=Manual.
      // If M=Manual (or invalid value), use T=Top.
       IF wk_CMSLLC = 'F';
         wk_LblLoc = 'F';
       ELSE;
         wk_LblLoc = 'T';
       ENDIF;                                                                // wk_CMSLLC='F'

       wk_OETFRT = *Zero;
       wk_OETMSG = *Zero;
       wk_OEUDF1 = *Blanks;
       EXEC SQL select OETFRT, OETMSG, OEUDF1
                 into :wk_OETFRT, :wk_OETMSG, :wk_OEUDF1
                from OEORHD
                where OEORD#=:IHORD#;
       wk_FrtTerms = %subst(wk_OEUDF1:1:2);

       wk_GCGRP = *Zero;
       wk_GCCPTY = *Zero;
       wk_GCBLTO = *Zero;
       wk_GCSHTO = *Zero;
       wk_GCDSHP = *Zero;
       wk_GCSHPV = *Blanks;
       wk_GCSVDS = *Blanks;
       wk_GCFTRM = *Blanks;
       wk_GCFRTA = *Zero;
       wk_GCCHGA = *Zero;
       wk_GCITCT = *Zero;
       wk_GCSHWT = *Zero;
       wk_GCPANS = *Blank;
       EXEC SQL select GCGRP#, GCCPTY, GCBLTO, GCSHTO,
                       GCDSHP, GCSHPV, GCSVDS, GCFTRM,
                       GCFRTA, GCCHGA, GCITCT, GCSHWT,
                       GCPANS
                 into :wk_GCGRP, :wk_GCCPTY, :wk_GCBLTO, :wk_GCSHTO,
                      :wk_GCDSHP, :wk_GCSHPV, :wk_GCSVDS, :wk_GCFTRM,
                      :wk_GCFRTA, :wk_GCCHGA, :wk_GCITCT, :wk_GCSHWT,
                      :wk_GCPANS
                from HYPSGCU
                where GCGRP#=:wk_ShipGrp# and GCBLTO=:IHBLTO and
                 GCSHTO=:IHSHTO and GCDSHP=:IHDSHP;

       IF wk_GCGRP = *Zero  or  wk_GCBLTO=*Zero  or
         wk_GCSHTO = *Zero;
         EVAL wk_Error = 3;
         LEAVE;
       ENDIF;                                                                // wk_GCGRP=*Zero or
      /End-Free

     C     wk_Turn#      SETLL     OEORDP
     C                   DOU       %eof(OEORDP)
     C     wk_Turn#      READE     OEORDP
     C                   IF        %eof(OEORDP)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP)

     C                   IF        IDQOPK <= *Zero
     C                   ITER
     C                   ENDIF                                                  IDQOPK<=Zero

     C                   EVAL      wk_StkUOMQty = IDQOPK                        Stk UOM pick qty

      /Free

       chain (in_Batch:wk_Turn:IDORD#:IDORL#) PICKBATDL1;
       if %found(PICKBATDL1);
         wk_PickLoc = %trimr(dt_PICKAISLE) + '-' + dt_PICKLOC;
       endif;

       wk_ODITEM = *Blanks;
       wk_ODIMDS = *Blanks;
       wk_ODPCLS = *Blanks;
       wk_ODWH = *Zero;
       wk_ODQORD = *Zero;
       wk_ODQSTD = *Zero;
       wk_ODQSTC = *Zero;
       wk_ODOREC = *Blank;
       EXEC SQL select ODITEM, ODIMDS, ODPCLS, ODWH,
                       ODQORD, ODQSTD, ODQSTC, ODOREC
                 into :wk_ODITEM, :wk_ODIMDS, :wk_ODPCLS, :wk_ODWH,
                      :wk_ODQORD, :wk_ODQSTD, :wk_ODQSTC, :wk_ODOREC
                from OEORDT
                where ODORD#=:IDORD# and ODORL#=:IDORL# and
                 ODBLN#=:IDBLN#;

       IF wk_ODOREC = 'S'  or  wk_ODOREC='X';
         wk_IMPCLS = *Blanks;
         wk_IMIMWG = *Zero;
         wk_IMUDN4 = *Zero;
         wk_IMLOT = *Blank;
         wk_IMUOMS = *Blanks;
         EXEC SQL select IMPCLS, IMIMWG, IMUDN4, IMLOT, IMUOMS
                   into :wk_IMPCLS, :wk_IMIMWG, :wk_IMUDN4, :wk_IMLOT,
                        :wk_IMUOMS
                  from HDIMST
                  where IMITEM=:wk_ODITEM;
         wk_IMCNVY = *Blank;
         wk_IMPLTQ = *Zero;
         wk_IMPLLB = *Blank;
         EXEC SQL select IMCNVY, IMPLTQ, IMPLLB
                   into :wk_IMCNVY, :wk_IMPLTQ, :wk_IMPLLB
                  from HDIMST2
                  where IMITEM=:wk_ODITEM;
       ELSE;
         wk_IMPCLS = wk_ODPCLS;
         wk_IMIMWG = *Zero;
         wk_IMUDN4 = *Zero;
         wk_IMLOT = 'N';
         wk_IMCNVY = *Blank;
         wk_IMPLTQ = *Zero;
         wk_IMPLLB = *Blank;
       ENDIF;                                                                // wk_ODOREC='S' or

       IF wk_IMPLLB = 'Y';
         wk_AlwPlltLbls = 'Y';
       ELSE;
         wk_AlwPlltLbls = wk_CMPLLB;
       ENDIF;                                                                // wk_IMPLLB='Y'

       wk_BDITEM = *Blanks;
       wk_BDUPC12 = *Blanks;
       wk_BDUCC14 = *Blanks;
       wk_BDUCC14I = *Blanks;
       wk_BDQTYIPC = *Zero;
       wk_BDQTYBAR = *Zero;
       EXEC SQL select BDITEM, BDUPC12, BDUCC14, BDUCC14I,
                       BDQTYIPC, BDQTYBAR
                 into :wk_BDITEM, :wk_BDUPC12, :wk_BDUCC14, :wk_BDUCC14I,
                      :wk_BDQTYIPC, :wk_BDQTYBAR
                from BARDATA
                where BDITEM=:wk_ODITEM;
       // IF wk_BDITEM <> wk_ODITEM;
       //   EXEC SQL select BDITEM, BDUPC12, BDUCC14, BDUCC14I,              // If not found in 2.0,
       //                   BDQTYIPC, BDQTYBAR                                         // check lega
       //             into :wk_BDITEM, :wk_BDUPC12, :wk_BDUCC14, :wk_BDUCC14I,
       //                  :wk_BDQTYIPC, :wk_BDQTYBAR
       //            from BARDATA1
       //            where BDITEM=:wk_ODITEM;
       // ENDIF;                                                             // wk_BDITEM<>wk_ODITEM
       IF wk_BDUPC12 = *Blanks;
         wk_BDUPC12 = wk_ODITEM;
       ENDIF;                                                                // wk_BDUPC12=*Blanks
       IF wk_BDUCC14 = *Blanks;
         wk_BDUCC14 = wk_ODITEM;
       ENDIF;                                                                // wk_BDUCC14=*Blanks

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

       wk_NoCases = *off;
       wk_IWPCPC = 1;
       IF wk_ODOREC = 'S'  or  wk_ODOREC = 'X';
         EXEC SQL select IWPCPC into :wk_IWPCPC
                  from HDIWHS
                  where IWWHS=:wk_ODWH and IWITEM=:wk_ODITEM;
         IF wk_IWPCPC = *Zero;
           wk_NoCases = *on;
           wk_IWPCPC = 1;
         ELSE;
           wk_NoCases = *off;
         ENDIF;                                                                // wk_IWPCPC=*Zero;
       ENDIF;                                                                // wk_ODOREC='S' or
       EVAL wk_CaseQty = wk_IWPCPC;

       wk_IMPLTQ@U = wk_IMPLTQ * wk_CaseQty;

       IF wk_IMLOT = 'M'  or  wk_IMLOT = 'A';
         wk_LotCtrl = 'Y';
       ELSE;
         wk_LotCtrl = 'N';
       ENDIF;                                                                // wk_IMLOT='M' or

       IF wk_LotCtrl = 'Y'  or
         wk_IMPCLS = 'AAAA'  or  wk_IMPCLS = 'AAGC'  or                      // Rimfire
         (%subst(wk_IMPCLS:1:1) = 'F'  and                                   // Frontier
           %subst(wk_IMPCLS:1:3) <> 'FET')  or
         %subst(wk_IMPCLS:1:2) = 'AS';                                       // Ammo seconds
         wk_TblLotCode = *Blanks;
         wk_TDCF03 = *Blanks;
         EXEC SQL select TDCF03 into :wk_TDCF03
                  from GUPTDAT
                  where TDTABL='AMMOPURCH' and TDKEY1=:wk_ODITEM;
         IF wk_TDCF03 <> *Blanks  and  %len(%trimr(wk_TDCF03)) = 2;
           wk_TblLotCode = wk_TDCF03;
         ENDIF;                                                              // wk_TDCF03<>*Blanks
         SELECT;
         WHEN wk_TblLotCode <> *Blanks;                                      // Table-specified
           wk_LotEntry = wk_TblLotCode;
         WHEN wk_IMPCLS = 'AAAA';                                            // Rimfire ammo
           wk_LotEntry = '02';
         WHEN wk_IMPCLS = 'AAGA';                                            // Cntrfire ref & proof
           wk_LotEntry = '03';
         WHEN wk_IMPCLS = 'AAGC';                                            // Rimfire ref & proof
           wk_LotEntry = '04';
         WHEN %subst(wk_IMPCLS:1:2) = 'AS';                                  // Ammo seconds
       //  wk_LotEntry = '00';
           wk_LotEntry = '05';
         WHEN %subst(wk_IMPCLS:1:1) = 'A';                                   // Centerfile ammo
           wk_LotEntry = '01';
         WHEN %subst(wk_IMPCLS:1:1) = 'F';                                   // Frontier ammo
           wk_LotEntry = '06';
         OTHER;                                                              // Unknown
           wk_LotEntry = '99';
         ENDSL;
       ELSE;
         wk_LotEntry = '00';
       ENDIF;                                                                // wk_LotCtrl='Y'

       IF wk_LotEntry = '00'  and
         (%subst(wk_IMPCLS:1:2) = 'BB'  or                                   // Bulk bullets
          wk_IMPCLS = 'CAAA'  or                                             // Bulk cases
          wk_IMPCLS = 'CAAC'  or                                             //   "   "
          wk_IMPCLS = 'CBAA'  or                                             //   "   "
          wk_IMPCLS = 'CBAC'  or                                             //   "   "
          wk_IMPCLS = 'CCAA'  or                                             //   "   "
          wk_IMPCLS = 'CCAC'  or                                             //   "   "
          %subst(wk_IMPCLS:1:2) = 'CH');                                     //   "   "
         wk_TDTABL = *Blanks;
         wk_TDKEY1 = *Blanks;
         wk_TDKEY2 = *Blanks;
         EXEC SQL select TDTABL, TDKEY1, TDKEY2
                   into :wk_TDTABL, :wk_TDKEY1, :wk_TDKEY2
                  from GUPTDAT
                  where TDTABL='ITEMGRP' and TDKEY1='BBSKIPGRP' and
                        TDKEY2=:wk_ODITEM;
         IF wk_TDTABL <> 'ITEMGRP'  or  wk_TDKEY1 <> 'BBSKIPGRP'  or
              wk_TDKEY2 <> wk_ODITEM;
           wk_IHBLTOx = %editc(IHBLTO:'X');
           IF IHORTY <> 'V';
             wk_TDTABL = *Blanks;
             wk_TDKEY1 = *Blanks;
             EXEC SQL select TDTABL, TDKEY1
                       into :wk_TDTABL, :wk_TDKEY1
                      from GUPTDAT
                      where TDTABL='BULKLOTCUS' and TDKEY1=:wk_IHBLTOx;
           ENDIF;                                                            // IHORTY<>'V'
           IF IHORTY = 'V'  or
             (wk_TDTABL = 'BULKLOTCUS'  and  wk_TDKEY1 = wk_IHBLTOx);
             SELECT;
             WHEN %subst(wk_IMPCLS:1:2) = 'BB';                              // Bulk bullets
               wk_LotEntry = '07';
             WHEN %subst(wk_IMPCLS:1:1) = 'C';                               // Bulk cases
               wk_LotEntry = '08';
             OTHER;
               wk_LotEntry = '99';
             ENDSL;
           ENDIF;                                                            // IHORTY='V' or
         ENDIF;                                                              // wk_TDTABL='ITEMGRP'
       ENDIF;                                                                // wk_LotEntry='00' and

       IF wk_IMLOT = 'N';
         wk_MixLotCtns = *Blank;
       ELSE;
         IF wk_CMMXLC = *Blank;
           wk_MixLotCtns = 'Y';
         ELSE;
           wk_MixLotCtns = wk_CMMXLC;
         ENDIF;                                                              // wk_CMMXLC=*Blank
       ENDIF;                                                                // wk_IMLOT='N'

       IF wk_CMPRCN = 'Y'  or
           (wk_CMPRCN = 'A'  and  %subst(wk_IMPCLS:1:1) = 'A')  or
           (wk_CMPRCN = 'B'  and  %subst(wk_IMPCLS:1:1) = 'B')  or
           (wk_CMPRCN = 'C'  and  %subst(wk_IMPCLS:1:1) = 'C')  or
           (wk_CMPRCN = 'I'  and  wk_BDUCC14I <> *Blanks);
         wk_PrtlCtns = 'Y';
       ELSE;
         wk_PrtlCtns = 'N';
       ENDIF;                                                                // wk_CMPRCN='Y' or

       wk_QtyOpen = wk_ODQORD - wk_ODQSTD - wk_ODQSTC;
       IF wk_QtyOpen < *Zero;
         wk_QtyOpen = *Zero;
       ENDIF;                                                                // wk_QtyOpen<*Zero

       IF wk_IMPLTQ@U <> *Zero  and  wk_QtyOpen < wk_IMPLTQ@U;
         wk_PctPlltQty = wk_QtyOpen / wk_IMPLTQ@U;
       ELSE;
         wk_PctPlltQty = 1.00;
       ENDIF;                                                                // wk_IMPLTQ@U<>*Zero

       // Allow pallet-pick (by adjusting the pallet quantity) if -
       //  - the customer will take a pallet pick
       //  - the item was ordered in at least a pallet quantity
       //  - the order line was partially-shipped prior
       //  - 50% or more of the order quantity is still open
       //  - the open quantity is less than a normal pallet quantity
       IF wk_AlwPlltLbls = 'Y'  and  wk_IMPLTQ@U > *Zero  and
           wk_ODQORD >= wk_IMPLTQ@U  and
           (wk_ODQSTD > 0  or  wk_ODQSTC >0)  and
           wk_PctPlltQty >= .50  and
           wk_QtyOpen < wk_IMPLTQ@U;
         wk_PlltQty = wk_QtyOpen / wk_CaseQty;
         wk_PlltQty@U = wk_QtyOpen;
       ELSE;
         wk_PlltQty = wk_IMPLTQ;
         wk_PlltQty@U = wk_IMPLTQ@U;
       ENDIF;                                                                // wk_AlwPlltLbls='Y'

       //
       // Retrieve shipping-carton item number.
       //
       wk_CtnItem# = *Blanks;
       EXEC SQL select a.PSCPN into :wk_CtnItem#
                from HDMPSM a
                where a.PSPPN=:wk_ODITEM and exists (select * from
                 GUPTDAT b where b.TDTABL='SHIPCTN' and b.TDKEY1=a.PSCPN);
       IF wk_CtnItem# = *Blanks;
         wk_CtnItem# = 'CTN-' + wk_ODITEM;
       ENDIF;                                                                // wk_CtnItem#=*Blanks

      /End-Free

      **************************************************************************
      * Calculate full-case, full-pallet, master-pack and                      *
      *   partial-unit pick quanities.                                         *
      **************************************************************************

     C                   IF        wk_NoCases
     C                   EVAL      wk_FullCases = 0
     C                   EVAL      wk_PrtlUnits = wk_StkUOMQty
     C                   ELSE
     C                   EVAL      wk_FullCases = %div(wk_StkUOMQty:wk_CaseQty)
     C                   EVAL      wk_PrtlUnits = %rem(wk_StkUOMQty:wk_CaseQty)
     C                   ENDIF

     C                   IF        wk_PlltQty = *Zero  or  wk_AlwPlltLbls <> 'Y'
     C                   EVAL      wk_FullPlts = *Zero
     C                   ELSE
     C                   EVAL      wk_FullPlts = %div(wk_FullCases:wk_PlltQty)
     C                   ENDIF                                                  wk_PlltQty=*Zero  or

      **************************************************************************
      * Process any pallet quantities.                                         *
      **************************************************************************
     C                   IF        wk_FullPlts > *Zero
     C                   EVAL      wk_PltUnits = wk_PlltQty * wk_CaseQty
     C                   EVAL      wk_PickMode = 'P'
     C                   FOR       wk_PltCnt = 1 to wk_FullPlts
     C                   EVAL      wk_PickQty = wk_PltUnits
     C                   EVAL      wk_PickUnitQty = wk_PlltQty
     C                   EVAL      wk_PickUOM = 'CT'
     C                   EVAL      wk_ItemBC = wk_BDUCC14
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   IF        not wk_Bulk
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_PickQty
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_PickQty
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_FullCases = wk_FullCases - wk_PlltQty
     C                   EVAL      wk_CompPlt = 'Y'
     C                   EVAL      wk_PltLotCnt = *Zero

     C                   EXSR      $WrtSGDT
     C                   ENDFOR                                                 wk_PltCnt=1 to
     C                   ENDIF                                                  wk_FullPlts>*Zero

      **************************************************************************
      * Process any master-pack quantities.                                    *
      **************************************************************************
     C                   IF        wk_BDQTYBAR = *Zero
     C                   EVAL      wk_MSTPkQty = *Zero
     C                   ELSEIF    wk_FullCases > *Zero
     C                   EVAL      wk_MSTPkQty = %div(
     C                             wk_FullCases * wk_CaseQty :
     C                             wk_BDQTYBAR)
     C****               eval      wk_MSTPKRem = %rem(
     C****                         wk_FullCases * wk_CaseQty :
     C****                         wk_BDQTYBAR)
     C****               if        wk_MSTPKRem > 0
     C****               eval      wk_MSTPKLines = wk_MSTPkQty + 1
     C****               else
     C                   eval      wk_MSTPKLines = wk_MSTPkQty
     C****               endif
     C                   ENDIF

     C                   IF        wk_MSTPkQty > *Zero

     C                   FOR       wk_MSTPkCnt = 1 to wk_MSTPKLines
     C****               if        wk_MSTPkCnt < wk_MSTPKLines
     C                   EVAL      wk_PickQty = wk_BDQTYBAR
     C****               else
     C****               if        wk_MSTPKRem > 0
     C****               EVAL      wk_PickQty = wk_MSTPKRem
     C****               else
     C****               EVAL      wk_PickQty = wk_BDQTYBAR
     C****               ENDIF
     C****               endif
     C                   EVAL      wk_PickUnitQty = 1
     C****               EVAL      wk_PickUOM = 'CT'
     C                   EVAL      wk_PickUOM = 'MP'
     C                   EVAL      wk_ItemBC = wk_BDUCC14
     C                   EVAL      wk_FullCases = wk_FullCases -
     C                               %div(wk_BDQTYBAR:wk_CaseQty)
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   IF        not wk_Bulk
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_BDQTYBAR
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_BDQTYBAR
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   ENDFOR

     C                   ENDIF

      **************************************************************************
      * Process any full-case quantities.                                      *
      **************************************************************************
     C                   IF        wk_FullCases > *Zero  and
     C                               (wk_CaseQty > 1  or  wk_IMCNVY = 'Y') or
     C                               (wk_CaseQty = 1  and wk_NoCases = *off)

     C                   FOR       wk_CaseCnt = 1 to wk_FullCases
     C                   EVAL      wk_PickQty = wk_CaseQty
     C                   EVAL      wk_PickUnitQty = 1
     C                   EVAL      wk_PickUOM = 'CT'
     C                   EVAL      wk_ItemBC = wk_BDUCC14
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   IF        not wk_Bulk
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_CaseQty
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_CaseQty
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   ENDFOR                                                 wk_CaseCnt=1 to

     C                   EVAL      wk_FullCases = 0
     C                   ENDIF                                                  wk_FullCases>*Zero

      **************************************************************************
      * Process any each or box quantities.                                    *
      **************************************************************************
     C                   IF        wk_CaseQty > 1  and
     C                               wk_PrtlUnits >= wk_CaseQty
     C                   EVAL      wk_ToteCases = %div(wk_PrtlUnits:wk_CaseQty)
     C                   EVAL      wk_PrtlUnits = %rem(wk_PrtlUnits:wk_CaseQty)
     C                   FOR       wk_CaseCnt = 1 to wk_ToteCases
     C                   EVAL      wk_PickQty = wk_CaseQty
     C                   EVAL      wk_PickUnitQty = 1
     C                   EVAL      wk_PickUOM = 'CT'
     C                   EVAL      wk_ItemBC = wk_BDUCC14
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   IF        not wk_Bulk
     C                   IF        wk_IMIMWG * wk_PrtlUnits < 10000
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_CaseQty
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = 9999
     C                   ENDIF                                                  wk_IMIMWG * wk_PrtlU
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_CaseQty
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   ENDFOR                                                 wk_CaseCnt=1 to
     C                   ENDIF                                                  wk_CaseQty>1 and

     C                   IF        wk_PrtlUnits > *Zero  or
     C                               (wk_CaseQty = 1  and wk_FullCases > *Zero
     C                                and wk_IMCNVY <> 'Y')
     C                   IF        wk_PrtlUnits = *Zero
     C                   EVAL      wk_PrtlUnits = wk_FullCases
     C                   ENDIF                                                  wk_PrtlUnits=*Zero
     C                   EVAL      wk_PickQty = wk_PrtlUnits
     C                   EVAL      wk_PickUnitQty = wk_PrtlUnits
     C                   EVAL      wk_PickUOM = wk_IMUOMS
     C                   EVAL      wk_ItemBC = wk_BDUPC12
     C                   EVAL      wk_SSCCBC = *Blanks
     C                   EVAL      wk_SSCC = *Zero
     C                   IF        not wk_Bulk
     C                   IF        wk_IMIMWG * wk_PrtlUnits < 10000
     C                   EVAL      wk_PickUnitWgt = wk_IMIMWG * wk_PrtlUnits
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = 9999
     C                   ENDIF                                                  wk_IMIMWG * wk_PrtlU
     C                   ELSE
     C                   EVAL      wk_PickUnitWgt = wk_IMUDN4 * wk_PrtlUnits
     C                   ENDIF                                                  not wk_Bulk
     C                   EVAL      wk_CompPlt = 'N'
     C                   EVAL      wk_PltLotCnt = *Zero
     C                   EXSR      $WrtSGDT
     C                   ENDIF                                                  wk_PrtlUnits>*Zero

     C                   ENDDO                                                  %eof(OEORDP)

      *
      * Update HYPSGDT record sequence to provide uniqueness for multiple
      * pallet-picks for the same item for the same customer.
      *

     C                   IF        not %open(HYPSGDTU)
     C                   OPEN      HYPSGDTU
     C                   ENDIF                                                   not %open(HYPSGDTU)

     C                   EVAL      sv_TURN = -1
     C                   EVAL      sv_TSEQ = -1
     C     wk_ShipGrp#   SETLL     HYPSGDTU
     C                   DOU       %eof(HYPSGDTU)
     C     wk_ShipGrp#   READE     HYPSGDTU
     C                   IF        %eof(HYPSGDTU)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPSGDTU)
     C                   if        GDRSEQ > 0
     C                   unlock(e) HYPSGDTU
     C                   iter
     C                   endif
     C                   IF        GDTURN <> sv_TURN  or
     C                                GDTSEQ <> sv_TSEQ
     C                   EVAL      wk_Seq# = getMaxGDRSEQ(GDGRP#
     C                                                   :GDTURN
     C                                                   :GDTSEQ)
     C                   EVAL      sv_TURN = GDTURN
     C                   EVAL      sv_TSEQ = GDTSEQ
     C                   ENDIF                                                  GDTURN<>sv_TURN
     C                   EVAL      wk_Seq# = wk_Seq# + 1
     C                   EVAL      GDRSEQ = wk_Seq#
     C                   UPDATE    HYRSGDTU
     C                   ENDDO                                                  %eof(HYPSGDTU)

      *
      * Update Shipment Group Customer Summary (HYPSGCU) record.
      *
      /Free
       IF wk_FrtTerms <> *Blanks  and
           wk_FrtTerms <> wk_GCFTRM  and
           wk_GCFTRM <> '**';
          wk_FrtTerms = *Blanks;
       ENDIF;                                                                // wk_FrtTerms<>*Blanks
       wk_FrtAmt = wk_GCFRTA + wk_OETFRT;
       wk_SpclChg = wk_GCCHGA + wk_OETMSG;

       EXEC SQL update HYPSGCU a
                set a.GCFTRM=:wk_FrtTerms, a.GCFRTA=:wk_FrtAmt,
                  a.GCCHGA=:wk_SpclChg,
                  a.GCITCT=(select count(distinct GDITEM) from HYPSGDT b
                   where b.GDGRP#=a.GCGRP# and b.GDBLTO=a.GCBLTO and
                   b.GDSHTO=a.GCSHTO and b.GDDSHP=a.GCDSHP),
                  a.GCSHWT=(select sum(GDPUWT) from HYPSGDT b
                   where b.GDGRP#=a.GCGRP# and b.GDBLTO=a.GCBLTO and
                   b.GDSHTO=a.GCSHTO and b.GDDSHP=a.GCDSHP),
                  a.GCSTYP = (CASE
                   WHEN (select sum(GDPUWT) from HYPSGDT b
                    where b.GDGRP#=a.GCGRP# and b.GDBLTO=a.GCBLTO and
                    b.GDSHTO=a.GCSHTO and b.GDDSHP=a.GCDSHP)
                    < 200 THEN 'P'
                   ELSE 'T'
                   END)
                where GCGRP#=:wk_ShpGrp and GCBLTO=:IHBLTO and
                      GCSHTO=:IHSHTO and GCDSHP=:IHDSHP;

      /End-Free

     C                   ENDDO

     C                   RETURN    wk_Error

     C/EJECT
      **************************************************************************
      * Write HYPSGDT record.                                                  *
      **************************************************************************
     CSR   $WrtSGDT      BegSR

     C                   IF        wk_GCPANS = 'Y'  and  wk_PickMode = 'C'
     C                   EVAL      wk_GDPANS = 'Y'
     C                   ELSE
     C                   EVAL      wk_GDPANS = *Blank
     C                   ENDIF                                                  wk_GCPANS='Y' and

      /Free

         // move data...
         GDGRP# = wk_ShpGrp;
         GDBLTO = IHBLTO;
         GDSHTO = IHSHTO;
         GDDSHP = IHDSHP;
         GDNAME = wk_STName;
         GDTURN = wk_Turn#;
         GDTSEQ = IDSEQ#;
         GDRSEQ = 0;
         GDOCTL = wk_OrdCtl#;
         GDORD# = IHORD#;
         GDORL# = IDORL#;
         GDBLN# = IDBLN#;
         GDITEM = wk_ODITEM;
         GDIMDS = wk_ODIMDS;
         wk_NeedQty = 0;
         wk_PickedQty = 0;
         exec sql
           select coalesce(sum(p.PICKNEED), 0),
                  coalesce(sum(p.PICKQTYP), 0)
             into :wk_NeedQty, :wk_PickedQty
             from PICKBATDP p
            where p.PICKTURN = :wk_Turn#
             and p.PICKORD  = :IHORD#
             and p.PICKORDL = :IDORL#
             and p.PICKITEM = :wk_ODITEM
             and p.PICKBAT in (
                   select pickbat
                     from pickmbatdp
                    where pickmbat = :QtyBatch
                   union all
                   select :in_Batch
                     from sysibm.sysdummy1
                    where :QtyBatch = :in_Batch
                   );
         // Temporary debug - dump key values to joblog
           wk_sqlcode = SQLCOD;
         GDQOPN  = wk_QtyOpen;
         GDQOPKO = wk_PickQty;
         GDQOPK  = wk_PickQty;
         GDQSTC = 0;
         GDLBL1 = 'N';
         GDLBL2 = 'N';
         GDLBL3 = 'N';
         GDBRCD = wk_ItemBC;
         //GDSSCCBC = wk_SSCCBC;
         //GDSSCC = wk_SSCC;
         GDSSCCBC = *blanks;
         GDSSCC = 0;
         GDLOT = wk_LotEntry;
         GDPUWT = wk_PickUnitWgt;
         GDPUQT = wk_PickUnitQty;
         GDPUOM = wk_PickUOM;
         GDPLOC = wk_PickLoc;
         GDPLTQ = wk_PlltQty;
         GDPLBL = wk_AlwPlltLbls;
         GDCNVY = 'D';
         GDPSHP = wk_CompPlt;
         GDPLCT = wk_PltLotCnt;
         GDSHPV = wk_GCSHPV;
         GDSVDS = wk_GCSVDS;
         GDLLOC = wk_LblLoc;
         GDMXPO = wk_CMMXPO;
         GDMXLT = wk_MixLotCtns;
         GDCPTY = wk_GCCPTY;
         GDPO# = IHORRF;
         GDCTN# = wk_CtnItem#;
         GDTOTE = toteid;
         GDDVRT = 999;
         GDSPPK = ' ';
         GDPIKO = 0;
         GDPOPP = 0;
         GDPANS = wk_GDPANS;
         write HYRSGDT;

      /End-Free

     CSR                 EndSR

     P crtShipRcds2    E
      * -----------------------------------------------------------------------------*
     p getMaxGDRSEQ    B
     d                 pi             5s 0
     d  groupShipTo                   9s 0 value
     d  turn                          9s 0 value
     d  turnseq                       3s 0 value
     d
     d  maxSeq         s              5s 0 inz(0)
      /free

         exec sql
          select max(GDRSEQ)
            into :maxSeq
          from HYPSGDT
          where GDGRP# = :groupShipTo
            and GDTURN = :turn
            and GDTSEQ = :turnseq;

         return maxSeq;

      /end-free
     p getMaxGDRSEQ    E
