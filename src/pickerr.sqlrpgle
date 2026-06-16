     H option(*nodebugio:*srcstmt) dftactgrp(*no) bnddir('PICKERR')
      **************************************************************************
      * APPLICATION: Pick list                                                 *
      *       TITLE: Load Pick List BATCH                                      *
      *          BY: Glenn Hopwood                                             *
      *        DATE: Sept 2021                                                 *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  12/03/2025  RBM  Add Record Lock Handling on PICKBATHP                *
      *  12/08/2025  RBM  Update GDTOTE when line already exists in HYPSGDT    *
      *  01/12/2025  RBM  Enforce a single tote per pick batch                 *
      *  03/03/2026  RBM  Tote Complete Issue resolution                       *
      *  04/23/2026  RBM  Block re-entry of completed (Z) batches in startPick *
      *                   Prevents duplicate shipment groups when a completed  *
      *                   batch is reopened and re-picked                      *
      *  04/23/2026  RBM  Fix wrkbatch drift in loadShipSR.  Always use        +
      *                   PICKBATDP_dt.PICKBAT instead of wk_Batch to ensure   *
      *                   correct batch number when picker works multiple      +
      *                   batches in the same sesson.                          *
      **************************************************************************

     FOEORHP    IF   E           K DISK
     FOEORDP01  IF   E           K DISK
     FHDCCMT    IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FPICKBATHP UF A E           K DISK
     FPICKBATHL1IF   E           K DISK    rename(PICKBATHR:PACKBATHL)
     FPICKBATHL2IF   E           K DISK    rename(PICKBATHR:PACKBATHL1)
     FPICKBATDP UF A E           K DISK    prefix(dt_)
     FPICKBATSP UF A E           K DISK    prefix(sc_)
     FPICKBATMP UF A E           K DISK    prefix(ms_)
     FHDIMST    IF   E           K DISK
     FHDIWHS    IF   E           K DISK
     FGUPTDAT   IF   E           K DISK
     F*HYLSGDT1  IF   E           K DISK    rename(HYRSGDT:HYRSGDT1)
     FHYPSGDT   UF   E           K DISK
     FPICKERD   cf   e             workstn
     F                                     sfile(homesflr:rrn1)
     F                                     sfile(pickdtlr:rrn2)
     F                                     sfile(scannedr:rrn3)
     F                                     HANDLER('GENIE(HANDLER)')

     D/EJECT

      /copy pickerr_pr.rpgle
      // /copy pickerr_pr.rpgle (already copied)

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D QCMDEXC         PR                  EXTPGM('QCMDEXC')
     D  Command                     500A   CONST
     D  CommandLen                   15P 5 CONST

     D LoadShip        PR                  EXTPGM('PICKBATLR2')
     D  xBatch                             like(PICKBAT) const
     D  xShpGrp                       9p 0


     d underlingBatchList...
     d                 s             10i 0 dim(*auto:100)

     d batchShipDoneArr...
     d                 s             10i 0 dim(*auto:100)


      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D*NONE            PI
     D* out_RtnCode                   2A

      **************************************************************************
      * Program status data structure.                                         *
      **************************************************************************
     D ps_            SDS
     D  ps_Pgnm                1     10
     D  ps_Job               244    253
     D  ps_User              254    263
     D  ps_Job#              264    269  0
     D  ps_curUser           358    367

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D PICKBATDP_dt  E DS                  extname('PICKBATDP') qualified

      **************************************************************************
      * Define arrays.                                                         *
      **************************************************************************
     D*Cmd             S             80    DIM(4) CTDATA                        CL commands

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D*lastUsedBatch   S             11P 0 DTAARA(PICKBATD)

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D RRN1            S              5  0
     D RRN2            S              5  0
     D RRN3            S              5  0
     D wk_Batch        S                   like(PICKBAT)
     D doneBatch       S                   like(PICKBAT)
     D wk_ShpGrp       S              9p 0
     D lastToteBatch   S                   like(wk_Batch) inz(*zeros)
     D lastToteId      S             10A   inz(*blanks)
     D wk_sqlcode      S              5s 0
     D wk_ItmCnt       S              5  0
     D wk_NetItmCnt    S              5  0
     D wk_LastName     S             10
     D scn_QtyScan     S              5  0
     D hld_QtyScan     S              5  0
     D hld_CurrQty     S              5  0
     D hld_IHBLTO      S                   like(IHBLTO)
     D paused          S               n
     D openGetPicks    S               n
     D pickLoop        S               n   inz(*on)
     D pickDone        S               n   inz(*off)
     D shipDone        S               n
     D firstOne        S               n
     D sqlFail         S               n
     D PICKERNUM       S             10
     D hld_CXCVF       C                   'C'
     D hld_PICCMT      C                   'PIC'
     D hld_CaseQty     S              9  0
     D hld_PickDiff    S              9  0
     D hld_newQDQOPK   S              9  0
     D hld_TURNSeq     S              3  0
     D hld_PrtlUnits   S              5  0
     D hld_IMUOMS      S                   like(IMUOMS)
     D hld_BDITEM      S             15A
     D hld_BDUPC12     S             12A
     D hld_BDUCC14     S             14A
     D wk_PickUnitWgt  S              9P 5
     D hld_GDBRCD      S             14A
     D hld_GDPUQT      S              9  0
     D hld_GDPUOM      S              2A
     D hld_GDPUWT      S              9  5
     D INVLOC          S              4a
     D hldQtyPick      S              9P 0
     D wk_Item#        S             15A
     D wk_PkgQty       S              5P 0
     D hld_numRows     S              5P 0
     D hld_ToteOpen    S             10I 0
     D wk_GS1Comp      S              6A   DTAARA(GS1COMP)                      GS1 Company Prefix
     D wk_GS1Comp2     S              6A   DTAARA(GS1COMP2)                     GS1 Company Prefix 2
     d wk_toteid       s             10a
     d hld_Unpicked    S             10i 0
     d uToteMsg        S             50a

     d loopJumps       ds                  qualified
     d  continue                     10i 0 inz(0)
     d  iter                         10i 0 inz(1)
     d  leave                        10i 0 inz(2)
     d
     d processScanDat...
     d  aRtn           s             10i 0
     d
     d updateDuration  s              1a
     d lockErrOccured  s               n
     d lockJobLogInfo  s           3000a
     d lockMsgText     s           3000a
      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D*NONE            C                   ' 5'

     C/EJECT
      **************************************************************************
      * Main procedure.                                                        *
      **************************************************************************
      /free

       EXEC SQL
         Set Option
           Naming = *Sys,
           Commit = *None,
           UsrPrf = *User,
           DynUsrPrf = *User,
           Datfmt = *iso,
           CloSqlCsr = *EndMod ;

       //
       // Retrieve current GS1 company prefix (e.g., 090255 or 851529).
       //
       in wk_GS1Comp;
       in wk_GS1Comp2;
       if wk_GS1Comp2 = *Blanks;
         wk_GS1Comp2 = *Hival;
       endif;

       LOGIN = *off;
       EXIT = *off;
       LOGINERR = *off;
       CONFIRM = *off;
       BTNALDA = *off;
       BTNWEST = *off;
       BTNGI = *off;

       dow EXIT = *off;

         REDO = *off;

         exfmt LOGINR;

         chain ('SHIPUSER':EMPCODE) GUPTDAT;
         if %found(GUPTDAT);
           EMPNAME = TDDESC;
           PICKERNUM = TDKEY1;
           wk_LastName = %subst(TDCF01:1:10);
           LOGINERR = *off;
           CONFIRM = *on;
         else;
           EMPNAME = *blanks;
           PICKERNUM = *blanks;
           LOGINERR = *on;
           CONFIRM = *off;
           iter;
         endif;

         if BTNALDA = *on;
           INVLOC = 'ALDA';
           exsr DoIt;
         elseif BTNWEST = *on;
           INVLOC = 'WEST';
           exsr DoIt;
         elseif BTNGI = *on;
           INVLOC = 'GI';
           exsr DoIt;
         elseif REDO = *on;
           EMPCODE = *blanks;
           INVLOC = *blanks;
           CONFIRM = *off;
           LOGINERR = *off;
         endif;

       enddo;

       sd_HYR9960();
       *inlr = *on;
       return;

       //-----------------------------------------------------------------------
       // DoIt -
       //-----------------------------------------------------------------------
       begsr DoIt;

        EXIT = *off;

        dow EXIT = *off;
          exsr clearBtchSFL;
          exsr loadBtchSFL;

          exfmt HOMER;

          if BEGIN = *on;
            %elem(batchShipDoneArr) = 0;

            if myBatch <> 0;
              wk_Batch = myBatch;
            else;
              wk_Batch = getPickBatch();
            endif;
            exsr startPick;
          endif;

        enddo;

       endsr;

       //-----------------------------------------------------------------------
       // loadBtchSFL - Load items
       //-----------------------------------------------------------------------
       begsr loadBtchSFL;

        setll (*loval) PICKBATHP;
        read(n) PICKBATHP;
        dow not %eof(PICKBATHP);

          if (PICKER = PICKERNUM and (PICKSTAT = 'P' or PICKSTAT = 'B'));
            chain(n) (PICKBAT) PICKBATDP;
            if %found(PICKBATDP);
              SCPICKORD = getOrd(PICKBAT);
              SHIPTONAME = getShipToSum(PICKBAT);
       //       chain (PICKORD) OEORHP;
       //       if %found(OEORHP);
       //         chain (IHSHTO) HDCUST;
       //         if %found(HDCUST);
       //           SHIPTONAME = CMCNA1;
       //         endif;
       //       endif;
              RRN1 += 1;
              write homesflr;
            endif;
          endif;

          if PICKSTAT = 'B';
           pickstat = 'P';
           exec sql
            update PICKBATHP
             set PICKSTAT = 'P'
            where pickbat = :PICKBAT;

          endif;

          read(n) PICKBATHP;
        enddo;

       endsr;

       //-----------------------------------------------------------------------
       // startPick - start picking
       //-----------------------------------------------------------------------
       begsr startPick;

        exsr check4MSG;

        noBatch = *off;
        updateDuration  = 'N';
        chain (wk_Batch) PICKBATHP;
        if %found(PICKBATHP);
          If PICKSTAT = 'Z';
            ERRNOTES = 'Batch ' + %char(wk_Batch) +
                       ' is already complete.';
            noBatch = *On;
            exec sql
              insert into PICKBATLOG
                (LOGTS, LOGBAT, LOGGRP, LOGMSG, LOGUSR, LOGJOB, LOGJOB#)
              values(
                current_timestamp,
                :wk_Batch,
                0,
                'Re-entry of completed batch blocked in PICKERR',
                :ps_User,
                :ps_Job,
                :ps_Job#);
            leavesr;
          Endif;
          PICKTOT = wk_toteid;
          PICKER = PICKERNUM;
          PICKSTAT = 'B';
          PICKSTART = %timestamp();
          //PICKCHGUSR = ps_curUser;
          PICKCHGUSR = wk_LastName;
          PICKCHGTZ = %timestamp();
          PICKCHGJBU = wk_LastName;
          PICKCHGJBN = ps_Job;
          PICKCHGJB# = ps_Job#;
          update PICKBATHR;
          updateUnderliningBatches(wk_Batch:updateDuration);
          chain(n) (wk_Batch) PICKBATHP;
          exsr loadPickSFL;
          chain (wk_Batch) PICKBATHP;
          if paused = *on;
            PICKSTAT = 'P';
          elseif back = *on;
            clear PICKER;
            PICKSTAT = 'A';
            PICKSTART = *loval;
            PICKEND = *loval;
            PICKCHGUSR = *blanks;
            PICKCHGTZ = *loval;
            PICKCHGJBU = *blanks;
            PICKCHGJBN = *blanks;
            PICKCHGJB# = *zeros;
            clear PICKDUR;
          else;
            PICKSTAT = getBatchStatus(wk_Batch);
            PICKEND = %timestamp();
            PICKDUR = %diff(PICKEND:PICKSTART:*seconds);
            updateDuration = 'Y';
            PICKCHGJBU = *blanks;
            PICKCHGJBN = *blanks;
            PICKCHGJB# = *zeros;
          endif;
          update PICKBATHR;
          updateUnderliningBatches(wk_Batch:updateDuration);
        else;
          noBatch = *on;
        endif;

        exsr check4MSG;

       endsr;

       //-----------------------------------------------------------------------
       // loadPickSFL -
       //-----------------------------------------------------------------------
       begsr loadPickSFL;

        BACK = *off;
        START = *off;
        openGetPicks = *off;

        dow BACK = *off;
          exsr clearPickSFL;

          exsr pickDtlCrsr;

          EXEC SQL
            FETCH GETPICKS INTO :PICKBATDP_dt;
          dow SQLCODE = 0;

            PICKITEM = PICKBATDP_dt.PICKITEM;
            PICKAMT = PICKBATDP_dt.PICKNEED;
            PICKTURN = PICKBATDP_dt.PICKTURN;
            // Get Item Desc
            chain (PICKBATDP_dt.PICKITEM) HDIMST;
            if %found(HDIMST);
              ITMDESC = %trimr(IMIMDS);
            endif;
            // Set item pick status
            if PICKBATDP_dt.PICKITMST = *blanks;
              PICKITMST = '-';
              STATCOLOR = 'black';
            else;
              PICKITMST = PICKBATDP_dt.PICKITMST;
              if PICKBATDP_dt.PICKITMST = 'S';
                STATCOLOR = 'green';
              else;
                STATCOLOR = '#ed1b2f';
              endif;
            endif;
            LOCTXT = PICKBATDP_dt.PICKSTKRM + ' / ' + PICKBATDP_dt.PICKAISLE +
                     PICKBATDP_dt.PICKLOC;
            // Get Shipto info...
            chain (PICKBATDP_dt.PICKTURN) OEORHP;
            if %found(OEORHP);
              chain (IHSHTO) HDCUST;
              if %found(HDCUST);
                SHIPTO# = IHSHTO;
                SHIPTOTEXT = %char(IHSHTO) +
                 ' - ' + %trim(CMCNA1);
              endif;
            endif;

            RRN2 += 1;
            write PICKDTLR;

            EXEC SQL
              FETCH GETPICKS INTO :PICKBATDP_dt;
          enddo;

          EXEC SQL CLOSE GETPICKS;

          clear ORDCMT;
          firstOne = *on;
          shipDone = *off;
          exsr pickDtlOrdCmt;

          EXEC SQL
            FETCH GETORDCMT INTO :hld_IHBLTO;
          dow SQLCODE = 0;

            // Get billto level comments...
            // loop through HDCCMT...
            setll (hld_CXCVF:hld_IHBLTO:hld_PICCMT) HDCCMT;
            reade (hld_CXCVF:hld_IHBLTO:hld_PICCMT) HDCCMT;
            dow not %eof(HDCCMT);

              if firstOne;
                ORDCMT = %trim(CXCMNT);
                firstOne = *off;
              else;
                ORDCMT = %trim(ORDCMT) + X'25' + %trim(CXCMNT);
              endif;

              reade (hld_CXCVF:hld_IHBLTO:hld_PICCMT) HDCCMT;
            enddo;

            EXEC SQL
              FETCH GETORDCMT INTO :hld_IHBLTO;
          enddo;

          EXEC SQL CLOSE GETORDCMT;

          exsr check4MSG;
          // exfmt PICKSUMR;
          displayPicksumr();
          wk_toteid = toteid;
          exec sql
            update PICKBATHP
             set picktot = :wk_toteid
            where pickbat = :wk_batch;

          exsr check4MSG;

          if START = *on;
            exsr clearScanDB;
            pickLoop = *on;

            dow pickLoop;
              exsr doPicking;
            enddo;

            exsr clearScanDB;
            leave;
          endif;

        enddo;

       endsr;

       //-----------------------------------------------------------------------
       // pickDtlOrdCmt -
       //-----------------------------------------------------------------------
       begsr pickDtlOrdCmt;

        EXEC SQL DECLARE GETORDCMT CURSOR FOR
          select distinct b.IHBLTO
            from pickbatdp as a
            join oeorhp as b on (a.pickturn=b.ihturn)
            where a.pickbat = :wk_Batch
            order by b.IHBLTO
          FOR FETCH ONLY ;

        EXEC SQL
          OPEN GETORDCMT using :wk_Batch;

       endsr;

       //-----------------------------------------------------------------------
       // pickDtlCrsr -
       //-----------------------------------------------------------------------
       begsr pickDtlCrsr;
         exec sql
          drop table session.batchlist1;

         exec sql
          declare global temporary table batchlist1
           (
             batch numeric(8,0)
           );
        if isMegaBatch(wk_Batch);
         exec sql
           insert into batchlist1
            (select pickbat from pickmbatdp
               where pickmbat = :wk_Batch);
        else;
         exec sql
          insert into batchlist1
           values (:wk_Batch);

        endif;

        EXEC SQL DECLARE GETPICKS CURSOR FOR
          select *
            from pickbatdp
            where pickbat in
               (select batch from batchlist1)
            order by PICKOVRORD,PICKWHS,PICKSTKRM,PICKAISLE,PICKLOC
          FOR FETCH ONLY ;

        EXEC SQL
          OPEN GETPICKS using :wk_Batch;

        openGetPicks = *on;

       endsr;

       //-----------------------------------------------------------------------
       // doPicking -
       //-----------------------------------------------------------------------
       begsr doPicking;

        paused = *off;
        //shipDone = *off;
        wk_ItmCnt = 0;
        wk_NetItmCnt = 0;
        %elem(underlingBatchList) = 0;
        if openGetPicks;
          EXEC SQL
            OPEN GETPICKS using :wk_Batch;
        else;
          exsr pickDtlCrsr;
        endif;

        EXEC SQL
          FETCH GETPICKS INTO :PICKBATDP_dt;
        dow SQLCODE = 0;

          wk_ItmCnt += 1;

          if PICKBATDP_dt.PICKITMST = ' ' ;
            wk_NetItmCnt += 1;
            ITEMCNTDWN = 'Turn ' + %char(PICKBATDP_dt.PICKTURN) + ' - ' +
                         'Item ' + %char(wk_ItmCnt) + ' of ' +
                         %char(PICKNUMITM);
            chain (PICKBATDP_dt.PICKITEM) HDIMST;
            if %found(HDIMST);
              ITMDESC = IMIMDS;
            endif;
            PICKITEM = PICKBATDP_dt.PICKITEM;
            PICKAMT = PICKBATDP_dt.PICKNEED;
            LOCTXT = PICKBATDP_dt.PICKSTKRM + ' / ' + PICKBATDP_dt.PICKAISLE +
                     ' / ' + PICKBATDP_dt.PICKLOC;
            if PICKBATDP_dt.PICKITEMCT <> *blanks;
              ITMNOTES = PICKBATDP_dt.PICKITEMCT;
            else;
              ITMNOTES = *blanks;
            endif;

            // Handle the scanning....
            exsr doScanning;

            if paused = *on;
              leave;
            endif;

            chain (PICKBATDP_dt.PICKBAT:PICKBATDP_dt.PICKSEQ) PICKBATDP;
            if %found(PICKBATDP);
              dt_PICKITMST = PICKBATDP_dt.PICKITMST;
              dt_PICKQTYP = PICKBATDP_dt.PICKQTYP;
              update PICKBATDR;
            endif;
          // Tote complete prompt for Mega Batches.
          // When the batch is fully picked, show TOTEDONE so the picker
          // knows they can remove the current tote/magnet.
          if pickDone= *On and paused  <> *On;
          if isMegaBatch(wk_Batch);
            doneBatch = PICKBATDP_dt.PICKBAT;
          else;
            doneBatch = wk_Batch;
          Endif;

          if %len(%trim(lastToteid)) > 0;
            exec sql
              select count(*) into :hld_Unpicked
                from PICKBATDP
               where PICKBAT  = :doneBatch
                 and PICKNEED > 0
                 and PICKITMST = ' ';
             wk_sqlcode = SQLCODE;
            if wk_sqlcode = 0 and hld_Unpicked = 0;
              exfmt totedone;
            endif;
          endif;
          endif;

          endif;

          EXEC SQL
            FETCH GETPICKS INTO :PICKBATDP_dt;
        enddo;

        EXEC SQL CLOSE GETPICKS;

        if wk_NetItmCnt = 0;
          pickLoop = *off;
        endif;

       endsr;

       //-----------------------------------------------------------------------
       // doScanning -
       //-----------------------------------------------------------------------
       begsr doScanning;

        exsr loadScanSFL;
        hld_QtyScan = scn_QtyScan;
        pickDone = *off;
        clear PICKNOTES;
        SCAN = '0';
        DODELETE = '0';
        DFT_QTY = 1;

        // stay here until picked = qty needed
        dou pickDone;

          clear SCANDATA;
          clear DELETECODE;
          clear SCANQTY;
          exsr check4MSG;
          ensureToteIdForBatch(wk_Batch:PICKBATDP_dt.PICKBAT);
          exfmt itemdtlr;
          exsr check4MSG;
          clear ERRNOTES;

          if DODELETE = *on;

            if DELETECODE <> *blanks;
              chain (PICKBATDP_dt.PICKBAT:
                     PICKBATDP_dt.PICKSEQ:
                     DELETECODE) PICKBATSP;
              if %found(PICKBATSP);
                hld_QtyScan -= sc_SCANQTY;
                delete PICKBATSR;
                PICKNOTES = 'Scanned ' + %char(hld_QtyScan) +
                            ' of ' + %char(%int(PICKBATDP_dt.PICKNEED));
                if hld_QtyScan = PICKBATDP_dt.PICKNEED;
                  PICKBATDP_dt.PICKITMST = 'S';
                  PICKBATDP_dt.PICKQTYP = hld_QtyScan;
                  exsr updHYPSGData;
                  pickDone = *on;
                  leave;
                endif;
              endif;
              exsr loadScanSFL;
            endif;

          elseif SKIP = *on;

            SKIPYES = '0';
            SKIPNO = '0';
            exfmt skipconf;
            if SKIPYES = '1';
            //PICKBATDP_dt.PICKITMST = '';
            //PICKBATDP_dt.PICKQTYP = hld_QtyScan;
              pickDone = *on;
              loadShipSR();
           // exsr updHYPSGData;
              leave;
            endif;

          elseif UNDERPICK = *on;

            UPICKYES = '0';
            UPICKNO = '0';
            exfmt upickconf;
            if UPICKYES = '1';
              if scandata = *blanks;
                scandata = pickitem;
                exsr processScanData;
                if processScanDataRtn = loopJumps.iter;
                 iter;
                endif;
              endif;
              PICKBATDP_dt.PICKITMST = 'X';
              PICKBATDP_dt.PICKQTYP = hld_QtyScan;
              pickDone = *on;
              loadShipSR();
              exsr updHYPSGData;
              leave;
            endif;

          elseif PAUSE = *on;

            paused = *on;
            pickDone = *on;
            pickLoop = *off;
            leave;

          elseif SCANDATA = *blanks;

            iter;

          elseif SCAN = *on or SCANDATA <> *blanks;
            exsr processScanData;
            select;
              when processScanDataRtn = loopJumps.iter;
                iter;
              when processScanDataRtn = loopJumps.leave;
                leave;
            endsl;
          endif;

        enddo;

        if pickDone = *on and paused <> *on;
          exsr check4MSG;
          PICKORD = PICKBATDP_dt.PICKORD;
          PICKTURN = PICKBATDP_dt.PICKTURN;
          SHIPTONAME = getShipToName(PICKBATDP_dt.PICKTURN);
          PICKITEM = PICKBATDP_dt.PICKITEM;
          PICKNEEDX = PICKBATDP_dt.PICKNEED;
          PICKQTYPX = PICKBATDP_dt.PICKQTYP;
          exfmt pickdoner;

          exsr check4MSG;
        endif;

       endsr;

       //-----------------------------------------------------------------------
       //  processScanData
       //   Run scanning logic.
       //-----------------------------------------------------------------------
       begsr processScanData;
            if (%subst(SCANDATA:1:1) = '0'  and
               (%subst(SCANDATA:2:6) = wk_GS1Comp  or
                %subst(SCANDATA:2:6) = wk_GS1Comp2)  and
                %subst(SCANDATA:14:2) = *Blanks);
              SCANDATA = %subst(SCANDATA:2:12);
            endif;

            wk_Item# = BarcodeItem(SCANDATA);
            wk_PkgQty = BarcodeQty(SCANDATA);
            if wk_Item# = *Blanks;
              wk_Item# = BarcodeItemC(SCANDATA:'H');
              wk_PkgQty = BarcodeQtyC(SCANDATA);
            endif;

            if SCANDATA = PICKBATDP_dt.PICKITEM;
              // allow the user to key the Item# instead of a barcode
            elseif wk_Item# <> PICKBATDP_dt.PICKITEM;
              ERRNOTES = 'Wrong item';
              processScanDataRtn = loopJumps.iter;
              leavesr;
            endif;
            hld_CurrQty = wk_PkgQty;

            if hld_CurrQty = 0;
              hld_CurrQty = 1;
            endif;

            // If the user did a QTY/SCAN...
            if SCANQTY > 0;
              hld_CurrQty = hld_CurrQty * SCANQTY;
            else;
              if  UNDERPICK = *on;
               hld_CurrQty = 0;
              else;
               hld_CurrQty = hld_CurrQty * DFT_QTY;
              endif;
            endif;

            hld_QtyScan += hld_CurrQty;

            if hld_QtyScan > PICKBATDP_dt.PICKNEED;
              ERRNOTES = 'Scanned too many';
              hld_QtyScan -= hld_CurrQty;
            else;

              // Load data into the DB
              sc_PICKBAT = PICKBATDP_dt.PICKBAT;
              sc_PICKSEQ = PICKBATDP_dt.PICKSEQ;
              sc_SCANCODE = SCANDATA;
              sc_SCANQTY = hld_CurrQty;
              write PICKBATSR;

              // Load the scanned data into the SFL...
              exsr loadScanSFL;

              if hld_QtyScan = PICKBATDP_dt.PICKNEED;
                PICKBATDP_dt.PICKITMST = 'S';
                PICKBATDP_dt.PICKQTYP = hld_QtyScan;
                pickDone = *on;
                loadShipSR();
                exsr updHYPSGData;
                processScanDataRtn = loopJumps.leave;
                leavesr;
              endif;

              PICKNOTES = 'Scanned ' + %char(hld_QtyScan) +
                          ' of ' + %char(%int(PICKBATDP_dt.PICKNEED));

            endif;

            processScanDataRtn = loopJumps.continue;

       endsr;

      //-----------------------------------------------------------------------
      // loadShipSR
      //-----------------------------------------------------------------------
      //  begsr loadShipSR;

      //    hld_numRows = 0;
      //    EXEC SQL
      //      select count(*) into :hld_numRows
      //        from HYPSGDT
      //       where GDTURN = :PICKBATDP_dt.PICKTURN and
      //             GDORD# = :PICKBATDP_dt.PICKORD and
      //             GDORL# = :PICKBATDP_dt.PICKORDL;

      //    if hld_numRows = 0 and
      //     shipDone = *off;
      //    // shipDone = *off and hld_QtyScan > 0;
      //      LoadShip(wk_Batch:wk_ShpGrp);
      //      shipDone = *on;
      //    endif;

      //  endsr;

       //-----------------------------------------------------------------------
       // updHYPSGData
       //-----------------------------------------------------------------------
       begsr updHYPSGData;

        if PICKBATDP_dt.PICKNEED <> PICKBATDP_dt.PICKQTYP;

       //   hld_PickDiff = PICKBATDP_dt.PICKNEED - PICKBATDP_dt.PICKQTYP;
       //
       //   // get the Turn seq#
       //   EXEC SQL
       //      select IDSEQ# into :hld_TURNSeq
       //        from OEORDP
       //        where IDTURN = :PICKBATDP_dt.PICKTURN and
       //              IDORD# = :PICKBATDP_dt.PICKORD and
       //              IDORL# = :PICKBATDP_dt.PICKORDL;
       //
       //   setgt (wk_ShpGrp:
       //          PICKBATDP_dt.PICKTURN:
       //          hld_TURNSeq) HYLSGDT11;
       //   readpe (wk_ShpGrp:
       //           PICKBATDP_dt.PICKTURN:
       //           hld_TURNSeq) HYLSGDT11;
       //   dow not %eof(HYLSGDT11);
       //
       //     if hld_PickDiff >= GDQOPK;
       //       hld_newQDQOPK = 0;
       //       hld_PickDiff -= GDQOPK;
       //     else;
       //       hld_newQDQOPK = GDQOPK - hld_PickDiff;
       //       hld_PickDiff = 0;
       //     endif;
       //
       //     chain (PICKBATDP_dt.PICKITEM:PICKBATDP_dt.PICKWHS) HDIWHS;
       //     if %found(HDIWHS) and IWPCPC > 0;
       //       hld_CaseQty = IWPCPC;
       //     else;
       //       hld_CaseQty = 1;
       //     endif;
       //     hld_PrtlUnits = %rem(hld_newQDQOPK:hld_CaseQty);
       //
       //     chain (PICKBATDP_dt.PICKITEM) HDIMST;
       //     if %found(HDIMST);
       //       hld_IMUOMS = IMUOMS;
       //       hld_GDPUWT = IMIMWG * hld_newQDQOPK;
       //     endif;
       //
       //     hld_BDITEM = *Blanks;
       //     hld_BDUPC12 = *Blanks;
       //     hld_BDUCC14 = *Blanks;
       //     EXEC SQL select BDITEM, BDUPC12, BDUCC14
       //               into :hld_BDITEM, :hld_BDUPC12, :hld_BDUCC14
       //              from BARDATA
       //              where BDITEM=:PICKBATDP_dt.PICKITEM;
       //     IF hld_BDITEM <> PICKBATDP_dt.PICKITEM;
       //       EXEC SQL select BDITEM, BDUPC12, BDUCC14
       //                 into :hld_BDITEM, :hld_BDUPC12, :hld_BDUCC14
       //                from BARDATA1
       //                where BDITEM=:PICKBATDP_dt.PICKITEM;
       //     ENDIF;
       //
       //     // figure out the proper weight, barcode and pick unit qty
       //     if (hld_newQDQOPK < hld_CaseQty) or (hld_PrtlUnits <> 0);
       //       hld_GDBRCD = hld_BDUPC12;
       //       hld_GDPUQT = hld_newQDQOPK;
       //       hld_GDPUOM = hld_IMUOMS;
       //     else;
       //       hld_GDBRCD = hld_BDUCC14;
       //       hld_GDPUQT = hld_newQDQOPK / hld_CaseQty;
       //       hld_GDPUOM = 'CT';
       //     endif;
       //
       //     GDQOPK = hld_newQDQOPK;
       //     GDBRCD = hld_GDBRCD;
       //     GDPUQT = hld_GDPUQT;
       //     GDPUOM = hld_GDPUOM;
       //     GDPUWT = hld_GDPUWT;
       //     update HYRSGDT;
       //
       //     if hld_PickDiff <= 0;
       //       leave;
       //     endif;
       //
       //     readpe (wk_ShpGrp:
       //             PICKBATDP_dt.PICKTURN:
       //             hld_TURNSeq) HYLSGDT11;
       //   enddo;

          hldQtyPick = PICKBATDP_dt.PICKQTYP;

          chain (PICKBATDP_dt.PICKTURN:
                 PICKBATDP_dt.PICKORD:
                 PICKBATDP_dt.PICKORDL)
                OEORDP01;
          if %found(OEORDP01);
            setll (wk_ShpGrp:PICKBATDP_dt.PICKTURN:IDSEQ#) HYPSGDT;
            reade (wk_ShpGrp:PICKBATDP_dt.PICKTURN:IDSEQ#) HYPSGDT;
            dow not %eof(HYPSGDT);

              // update GDQOPK
              if hldQtyPick <= GDQOPK;
                GDQOPK = hldQtyPick;
                hldQtyPick = 0;
                update HYRSGDT;
              else;
                hldQtyPick = hldQtyPick - GDQOPK;
              endif;

              reade (wk_ShpGrp:PICKBATDP_dt.PICKTURN:IDSEQ#) HYPSGDT;
            enddo;
          endif;

          // Update the CU record...
          EXEC SQL update HYPSGCU a
                    set a.GCITCT=(select count(distinct GDITEM) from HYPSGDT b
                          where b.GDGRP#=a.GCGRP# and b.GDBLTO=a.GCBLTO and
                            b.GDSHTO=a.GCSHTO and b.GDDSHP=a.GCDSHP),
                        a.GCSHWT=(select sum(GDPUWT) from HYPSGDT b
                          where b.GDGRP#=a.GCGRP# and b.GDBLTO=a.GCBLTO and
                            b.GDSHTO=a.GCSHTO and b.GDDSHP=a.GCDSHP)
                    where GCGRP#=:wk_ShpGrp;

        endif;

       endsr;

       //-----------------------------------------------------------------------
       // clearBtchSFL -
       //-----------------------------------------------------------------------
       begsr clearBtchSFL;

        RRN1 = 0;
        SFLCLR = *on;
        write homer;
        SFLCLR = *off;

       endsr;

       //-----------------------------------------------------------------------
       // clearPickSFL -
       //-----------------------------------------------------------------------
       begsr clearPickSFL;

        RRN2 = 0;
        SFLCLR = *on;
        write picksumr;
        SFLCLR = *off;

       endsr;

       //-----------------------------------------------------------------------
       // clearScanSFL -
       //-----------------------------------------------------------------------
       begsr clearScanSFL;

        // clear the SFL AND the DB
        RRN3 = 0;
        SFLCLR = *on;
        write itemdtlr;
        SFLCLR = *off;

       endsr;

       //-----------------------------------------------------------------------
       // clearScanDB -
       //-----------------------------------------------------------------------
       begsr clearScanDB;

        setll (PICKBATDP_dt.PICKBAT) PICKBATSP;
        reade (PICKBATDP_dt.PICKBAT) PICKBATSP;
        dow not %eof(PICKBATSP);

          delete PICKBATSR;

          reade (PICKBATDP_dt.PICKBAT) PICKBATSP;
        enddo;

       endsr;

       //-----------------------------------------------------------------------
       // loadScanSFL -
       //-----------------------------------------------------------------------
       begsr loadScanSFL;

        scn_QtyScan = 0;

        // clear and reload the SFL
        exsr clearScanSFL;

        setll (PICKBATDP_dt.PICKBAT:PICKBATDP_dt.PICKSEQ) PICKBATSP;
        reade (PICKBATDP_dt.PICKBAT:PICKBATDP_dt.PICKSEQ) PICKBATSP;
        dow not %eof(PICKBATSP);

          SCANCODE = sc_SCANCODE;
          SCANQTY = sc_SCANQTY;
          RRN3 += 1;
          write scannedr;

          scn_QtyScan += sc_SCANQTY;

          reade (PICKBATDP_dt.PICKBAT:PICKBATDP_dt.PICKSEQ) PICKBATSP;
        enddo;

       endsr;

       //-----------------------------------------------------------------------
       // check4MSG -
       //-----------------------------------------------------------------------
       begsr check4MSG;

        lockErrOccured = *off;
        clear PICKBATMR;
        monitor;
          chain (wk_Batch) PICKBATMP;
        on-error 01218;
          lockErrOccured = *on;
          exsr showPickbatmpLock;
        endmon;

        if lockErrOccured;
          leavesr;
        endif;

        if %found(PICKBATMP);
          if ms_PICKBATMSG <> *blanks;
            MESSAGE = ms_PICKBATMSG;
            exfmt messenger;
          endif;
          delete PICKBATMR;
        endif;

       endsr;

       begsr showPickbatmpLock;

        lockMsgText = 'Record lock encountered on PICKBATMP. Please try again.';
        lockJobLogInfo = *blanks;

        exec sql
          select cast(message_text concat ' ' concat
            coalesce(message_second_level_text,'') as varchar(3000))
            into :lockJobLogInfo
            from table(QSYS2.JOBLOG_INFO('*')) jl
            where jl.message_id in ('CPF5027','CPF5026')
            order by jl.message_timestamp desc
            fetch first 1 row only;

          if SQLCODE = 0 and lockJobLogInfo <> *blanks;
            lockMsgText = 'Record lock encountered on PICKBATMP.  ' +
             %trim(lockJobLogInfo);
          elseif SQLCODE < 0;
            lockMsgText = 'Record lock on PICKBATMP. See joblog.';
          endif;

          MESSAGE = lockMsgText;
          exfmt messenger;

       endsr;

      /end-free

      **************************************************************************
      * Get the users pick batch number
      *   Return values: batch number
      **************************************************************************
     P getBatchStatus  B
     D                 PI             1
     D inBatch                        8  0

     D numIncomplete   S              5  0
     D batchStatus     S              1
      /free

       numIncomplete = 0;
       // Default to 'incomplete' (Y)
       batchStatus = 'Y';

       // get the count of items with an 'X' status
       EXEC SQL
         select count(*)
           into :numIncomplete
           from pickbatdp
           where pickitmst = 'X' and pickbat = :inBatch;

       if numIncomplete = 0;
         batchStatus = 'Z';
       endif;

       return batchStatus;

      /end-free
     P getBatchStatus  E

      **************************************************************************
      * Get the users pick batch number
      *   Return values: batch number
      **************************************************************************
     P getPickBatch    B
     D                 PI             8s 0

     D errBatch        S              8s 0 inz(99999999)
      /free

       // Look for explicitly assigned batches
       setll (PICKERNUM) PICKBATHL1;
       reade (PICKERNUM) PICKBATHL1;
       dow not %eof(PICKBATHL1);

         if PICKSTAT = 'A' and
            PICKINVLOC = INVLOC;
           return PICKBAT;
         endif;

         reade (PICKERNUM) PICKBATHL1;
       enddo;

       // If you're still here, return the first unassigned batch
       setll (*loval) PICKBATHL2;
       read PICKBATHL2;
       dow not %eof(PICKBATHL2);

         if PICKSTAT = 'A' and PICKER = *blanks and
            PICKINVLOC = INVLOC;
           return PICKBAT;
         endif;

         read PICKBATHL2;
       enddo;

       // if you're still here return an error batch...
       return errBatch;

      /end-free
     P getPickBatch    E

      **************************************************************************
      * Get the order for the pick batch
      *   Return values: text with order# or order# + *
      **************************************************************************
     P getOrd          B
     D                 PI            10
     D inBatch                        8  0

     D wrkOrdTxt       s             10
     D ordCount        s              8  0

      /free

       wrkOrdTxt = *blanks;

       // get the count of orders in the batch
       EXEC SQL
        select count(distinct pickord)
          into :ordCount
          from pickbatdp
          where pickbat = :inBatch;

       setll (inBatch) PICKBATDP;
       reade(n) (inBatch) PICKBATDP;
       if not %eof(PICKBATDP);
         wrkOrdTxt = %char(dt_PICKORD);
         if ordCount > 0;
           wrkOrdTxt = %trim(wrkOrdTxt) + ' *';
         endif;
       endif;

       return wrkOrdTxt;

      /end-free
     P getOrd          E

      **************************************************************************
      * Get the shipto name (summary -- basically the first one)
      *   Return values: text with shipto name or shipto name + *
      **************************************************************************
     P getShipToSum    B
     D                 PI            30
     D inBatch                        8  0

     D wrkShipToTxt    s             30
     D ordCount        s              8  0

      /free

       wrkShipToTxt = *blanks;

       // get the count of orders in the batch
       EXEC SQL
        select count(distinct pickord)
          into :ordCount
          from pickbatdp
          where pickbat = :inBatch;

       setll (inBatch) PICKBATDP;
       reade(n) (inBatch) PICKBATDP;
       if not %eof(PICKBATDP);
         chain (dt_PICKTURN) OEORHP;
         if %found(OEORHP);
           chain (IHSHTO) HDCUST;
           if %found(HDCUST);
             wrkShipToTxt = CMCNA1;
             if ordCount > 0;
               wrkShipToTxt = %trim(wrkShipToTxt) + ' *';
             endif;
           endif;
         endif;
       endif;

       return wrkShipToTxt;

      /end-free
     P getShipToSum    E

      **************************************************************************
      * Get the shipto name
      *   Return values: text with shipto name
      **************************************************************************
     P getShipToName   B
     D                 PI            30
     D inTurn                              like(PICKBATDP_dt.PICKTURN)

     D wrkShipToTxt    s             30

      /free

       chain (inTurn) OEORHP;
       if %found(OEORHP);
         chain (IHSHTO) HDCUST;
         if %found(HDCUST);
           wrkShipToTxt = CMCNA1;
         endif;
       endif;

       return wrkShipToTxt;

      /end-free
     P getShipToName   E
      **************************************************************************
      * Display PICKSUMR
      *  display Picksumr. If toteid is in use redisplay
      **************************************************************************
     p displayPicksumr...
     p                 b
     d                 pi
     d
     d continue        s               n   inz(*on)
      /free

        toteid = *blanks;
        toteInUse = *off;
        dow continue;
          if isMegaBatch(wk_batch);
            maintoteId = *off;
          else;
            maintoteId = *on;
          endif;
          exfmt PICKSUMR;
          if not toteIdInUse(toteid);
           continue = *off;
          else;
           toteInUse = *on;
          endif;
        enddo;



      /end-free
     p displayPicksumr...
     p                 e
      **************************************************************************
      * toteIdInUse
      *  check if toteid is in use. Return *on if in use.
      **************************************************************************
     p toteIdInUse     b
     d                 pi              n
     d  tote                         10a   value
     d
     d  rowexists      s             10i 0 inz(0)
      /free

         if tote = *blanks;
           return *off;
         endif;

         exec sql
           select count(*)
              into :rowexists
             from hypsgdt a
             join hypsgcu b
                   on (b.GCGRP#,b.GCBLTO,b.GCSHTO,b.GCDSHP)=
                   (a.GDGRP#,a.GDBLTO,a.GDSHTO,a.GDDSHP)
             where a.gdtote = :tote
            and b.gcsts = 'O'
            fetch first 1 rows only;

         return (rowexists > 0);

      /end-free
     p toteIdInUse     e

       //-------------------------------------------------//
       // Procedure:isMegaBatch                           //
       //  is this batch a mega batch                     //
       //-------------------------------------------------//

       dcl-proc isMegaBatch;
        dcl-pi *n ind;
         inBatch zoned(8:0) value;
        end-pi;


        dcl-ds rowexists qualified;
          ind1 ind inz(*off);
          char1 char(1) pos(1);
        end-ds;

        exec sql
          select
            case count(1)
             when 0
              then '0'
              else '1'
             end
             into :rowexists.char1
            from sysibm.sysdummy1
           where exists ( select 1
                           from pickmbatdp
                           where pickmbat = :inBatch);



         return rowexists.ind1;

        end-proc;

       //-------------------------------------------------//
       // Procedure:isMegaBatch                           //
       //  is this batch a mega batch                     //
       //-------------------------------------------------//

       dcl-proc updateUnderliningBatches;
        dcl-pi *n;
         inBatch zoned(8:0) value;
         inUpdateDuration char(1) value;
        end-pi;

        if isMegaBatch(inBatch);
         exec sql
          merge into PICKBATHP target
          using (select hp.PICKER, hp.PICKSTAT, hp.PICKSTART,
                        hp.PICKCHGUSR, hp.PICKCHGTZ,hp.PICKCHGJBU,
                        hp.PICKCHGJBN, hp.PICKCHGJB#,hp.PICKDUR,
                        hp.pickbat,
                        mega.pickbat underliningbatch,
                        hp.PICKEND
                  from  PICKBATHP as hp
                    join pickmbatdp as mega
                     on hp.pickbat = mega.pickmbat
                  where hp.pickbat = :inBatch
                  ) source
          on target.PICKBAT = source.underliningbatch
          when matched then
           update set
             target.picker = source.picker,
             target.PICKSTAT = source.PICKSTAT,
             target.PICKEND = source.PICKEND,
             target.pickstart = source.PICKSTART,
             target.PICKCHGUSR = source.PICKCHGUSR,
             target.PICKCHGTZ = source.PICKCHGTZ,
             target.PICKCHGJBU = source.PICKCHGJBU,
             target.PICKCHGJBN = source.PICKCHGJBN,
             target.PICKCHGJB# = source.PICKCHGJB#,
             target.PICKDUR = case
                               when :inUpdateDuration = 'Y'
                                 then source.PICKDUR
                                 else target.PICKDUR
                               end;


        endif;


        end-proc;

      //-------------------------------------------------//
       // Procedure:ensureToteIdForBatch          //
       //   check if batch in mega batch and if so       //
       //     display tote id screen                     //
       //  Parms                                         //
       //      inMainBatch: Header batch #               //
       //      inBatch: Detail batch #                   //
       //------------------------------------------------//


       // -----------------------------------------------------------//
       //  procedure ensureToteIdForBatch                             //
       //   Prompts for Tote ID once per *pick batch* (ubatch).        //
       //   For mega batches, ubatch is the underlying component batch //
       //   (PICKBATDP_dt.PICKBAT). Tote ID is stored in PICKBATHP.    //
       // -----------------------------------------------------------//
       dcl-proc ensureToteIdForBatch;
        dcl-pi *n;
         inMainBatch like(wk_Batch) const;
         inBatch     like(wk_Batch) const;
       end-pi;

       dcl-s wrkBatch like(wk_Batch);
       dcl-s toteBatchInUse Ind inz(*off);

       // Determine the batch we want the tote to be "per".
       // - If main batch is a megabatch, we want ONE tote per underlying batch (inBatch)
       // - Otherwise, we want ONE tote per normal batch (inMainBatch)
       if isMegaBatch(inMainBatch);
        wrkBatch = inBatch;
       else;
        wrkBatch = inMainBatch;
       endif;

       // If we changed batches, reset cached tote so we re-check/PROMPT once.
       if wrkBatch <> lastToteBatch;
        lastToteBatch = wrkBatch;
        lastToteId    = *blanks;
       endif;

       // If we already have a tote cached for this batch, reuse it.
       if %len(%trim(lastToteId)) > 0;
        utoteid = lastToteId;
        return;
       endif;

       // Try to reuse a previously assigned tote for this batch from PICKBATHP.
       chain (wrkBatch) PICKBATHP;
       if %found(PICKBATHP) and %len(%trim(PICKTOT)) > 0;
        lastToteId = %upper(%trim(PICKTOT));
        utoteid    = lastToteId;
        return;
       endif;

       // No tote yet for this batch -> prompt once.
       ubatch  = %char(wrkBatch);
       uToteMsg = *blanks;
       toteInUse = *off;

       dou not toteBatchInUse;
         utoteid = *blanks;
         toteInUse = *Off;
         exfmt toteidrec;

         utoteid = %upper(%trim(utoteid));
         if utoteid = *blanks or
         %subst(utoteid:1:1) < 'A' or
         %subst(utoteid:1:1) > 'Z';
           toteInUse = *on;
           uToteMsg  = 'Tote ID must start with a letter.';
         else;
         //Check if tote is already assigned to an active batch
          exec sql
          select count(*) into :hld_numRows
            from PICKBATHP
          where PICKTOT = :utoteid
            and PICKSTAT in ('A','P','B')
            and PICKBAT <> :wrkBatch;

         if hld_numRows > 0;
          toteInUse = *On;
          uToteMsg  = 'Tote ' + %trim(utoteid) +
                      ' is already assigned to another batch.';
         elseif toteIdInUse(utoteid);
          toteInUse = *on;
          uToteMsg  = 'Tote ' + %trim(utoteid) +
                      ' is already in use.';
         else;
           uToteMsg  = *blanks;
           toteInUse = *off;
         endif;
       endif;

         toteBatchInUse = toteInUse;
       enddo;

       lastToteId = utoteid;

       // Persist tote id on the batch header so we don't re-prompt.
       if %found(PICKBATHP);
        PICKTOT = utoteid;
        update PICKBATHR;
       endif;

       end-proc;
       // -----------------------------------------------------------//
       //  procedure loadShipSr                                         //
       //   replaced subroutine loadShipSr to allow for local variables //
       // -----------------------------------------------------------//

       // -----------------------------------------------------------//
       //  procedure loadShipSR                                         //
       //   replaced subroutine loadShipSr to allow for local variables //
       // -----------------------------------------------------------//
       dcl-proc loadShipSR;
         dcl-pi *n;
         end-pi;

         dcl-s wrkbatch like(wk_Batch);

         // Make sure we have a Tote ID for the current batch.
         // (Prompts once per batch and saves it in PICKBATHP.PICKTOT.)
         ensureToteIdForBatch(wk_Batch: PICKBATDP_dt.PICKBAT);

         hld_numRows = 0;
         exec sql
           select count(*) into :hld_numRows
             from HYPSGDT
            where GDTURN = :PICKBATDP_dt.PICKTURN
              and GDORD# = :PICKBATDP_dt.PICKORD
              and GDORL# = :PICKBATDP_dt.PICKORDL;

         // Ship-build tracking is done per underlying batch when mega.
         //if isMegaBatch(wk_Batch);
         //  wrkbatch = PICKBATDP_dt.PICKBAT;
         //else;
         //  wrkbatch = wk_Batch;
         //endif;

         wrkbatch = PICKBATDP_dt.PICKBAT;

         if hld_numRows = 0 and
            %lookup(wrkbatch: batchShipDoneArr) = 0;

           LoadShip(wrkbatch: wk_ShpGrp);
           batchShipDoneArr(*next) = wrkbatch;

         else;

           // If ship lines already exist, just make sure the tote id is populated.
           if %len(%trim(utoteid)) > 0 and hld_numRows > 0;
             exec sql
               update HYPSGDT
                  set GDTOTE = :utoteid
                where GDTURN = :PICKBATDP_dt.PICKTURN
                  and GDORD# = :PICKBATDP_dt.PICKORD
                  and GDORL# = :PICKBATDP_dt.PICKORDL;
           endif;

         endif;

       end-proc;
