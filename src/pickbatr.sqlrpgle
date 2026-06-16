       //-------------------------------------------------//
       // Title: Pick Batch Dashboard
       // Created: 9/15/2021, 10:36:46 AM
       //  REV DATE   BY   CHANGE
       // ----------  ---  ---------------------------------------------------
       // 11/29/2023  PRF  Clear busy flags when deleting
       // 01/25/2024  PRF  Only clear busy flags if the combined ship
       //                   group is not OPEN
       // 07/08/2025  MCA  When Show-Completed is checked, limit the completed
       //                  batches to batches created in the past month or
       //                  the most recently created batches (as defined in
       //                  wk_LimitBats).
       // 07/08/2025  MCA  Change default DC from ALDA to WEST.
       // 12/03/2025  RBM  Modified to support MegaBatch publishing
       //     - Mega Batches group *picking* only. Shipment publishing is done
       //       per component pick batch so shipping/FedEx processing is not
       //       blocked when the same order is released in multiple batches
       //       hours apart.
       //
       //     - PICKBATR no longer forces a shared shipment group (override
       //       group) when publishing component batches. Each call to
       //       PICKBATLR2 is allowed to generate its own shipment group
       //       (GDGRP#) via getShipGrpID#().
       //
       //     - This prevents new lines from being appended into a shipment
       //       group that has already been processed/closed in HYR0600.
       //
       //     - PICKBATLR2 interface remains:
       //       (in_Batch, out_ShpGrp [, in_OverrideGrp#])
       // 03/03/2026  RBM  Change parameters and call to Publish_Shipments
       //-------------------------------------------------//

       //---------------//
       // Control Specs //
       //---------------//

       Ctl-Opt OPTION(*SRCSTMT : *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(*CALLER)
               BNDDIR('PICKSVBDIR')
               MAIN(MainLine) ;

       //------------//
       // File Specs //
       //------------//

       Dcl-F DisplyFile WORKSTN QUALIFIED ALIAS
         EXTFILE(*EXTDESC) EXTDESC('PICKBATD')
                    SFILE(SFL:Dspf_rrn)
                    usropn
                    HANDLER('GENIE(HANDLER)');

       Dcl-F GUPTDAT usage(*input) keyed;
       Dcl-F OEORHP usage(*input) keyed;
       Dcl-F HDCUST usage(*input) keyed;
       Dcl-F PICKBATMP usage(*delete:*output) keyed;
       Dcl-F PICKBATDP usage(*update) keyed;
       Dcl-F PICKBATHP DISK(*EXT) Usage(*update:*delete:*input:*output) Keyed;
       Dcl-F HYLSGSD1 usage(*update) keyed;


      /copy pickbatsv_pr.rpgle
       //-------------//
       // Global Data //
       //-------------//
       dcl-s lastUsedBatch packed(11:0) DTAARA('PICKBATD');

       //Data Structures
       DCL-DS pgm_stat PSDS;
         status *STATUS;
         pgmUser CHAR(10) POS(358);
       END-DS;

       Dcl-DS Tmplt_PICKBATHP EXT qualified template
         ExtName('PICKBATHP') ALIAS
       End-DS;

       Dcl-DS Table_In qualified;
         PICKBAT Like(Tmplt_PICKBATHP.PICKBAT);
         PICKER Like(Tmplt_PICKBATHP.PICKER);
         PICKINVLOC Like(Tmplt_PICKBATHP.PICKINVLOC);
         PICKSTAT Like(Tmplt_PICKBATHP.PICKSTAT);
      // PICKTURN Like(Tmplt_PICKBATHP.PICKTURN);
      // PICKORD Like(Tmplt_PICKBATHP.PICKORD);
         PICKNUMITM Like(Tmplt_PICKBATHP.PICKNUMITM);
         PICKNUMPCS Like(Tmplt_PICKBATHP.PICKNUMPCS);
         PICKSTART Like(Tmplt_PICKBATHP.PICKSTART);
         PICKEND Like(Tmplt_PICKBATHP.PICKEND);
         PICKDUR Like(Tmplt_PICKBATHP.PICKDUR);
         PICKSEQNO Like(Tmplt_PICKBATHP.PICKSEQNO);
         pickcrttz like(Tmplt_PICKBATHP.PICKCRTTZ);
       End-Ds;

       Dcl-DS Table_Out LikeDS(Table_In);

       Dcl-DS Ctl_Fields LikeRec(DisplyFile.Ctl:*ALL) ;
       Dcl-DS Ctl_Fields_Out LikeRec(DisplyFile.Ctl:*OUTPUT) ;
       Dcl-DS Sfl_In LikeRec(DisplyFile.Sfl:*INPUT) ;
       Dcl-DS Sfl_Out LikeRec(DisplyFile.Sfl:*OUTPUT) ;
       Dcl-DS Delete_Fields LikeRec(DisplyFile.Delete:*ALL) ;
       Dcl-DS Msg_Fields LikeRec(DisplyFile.Message:*All) ;
       Dcl-DS Batch_Msg_Fields LikeRec(DisplyFile.Batch_Msg:*All) ;
       Dcl-DS Assign_Bat_Fields LikeRec(DisplyFile.Assign_Bat:*All) ;
       Dcl-DS Comp_Batch_Fields LikeRec(DisplyFile.Comp_Batch:*All) ;
       Dcl-DS Ship_Via_Fields LikeRec(DisplyFile.Ship_Via:*All) ;
       Dcl-DS Detail_In LikeRec(DisplyFile.Detail:*ALL) ;
       Dcl-DS Detail_Out LikeRec(DisplyFile.Detail:*ALL) ;

       Dcl-DS DS1 likeDS(Tmplt_PICKBATHP);
       //Dcl-DS DS2 likeDS(Tmplt_PICKBATHP);
       Dcl-DS DS2 QUALIFIED;
         PICKBAT Like(Tmplt_PICKBATHP.PICKBAT);
         PICKER  Like(Tmplt_PICKBATHP.PICKER);
         PICKINVLOC Like(Tmplt_PICKBATHP.PICKINVLOC);
         PICKSTAT Like(Tmplt_PICKBATHP.PICKSTAT);
         PICKNUMITM Like(Tmplt_PICKBATHP.PICKNUMITM);
         PICKNUMPCS Like(Tmplt_PICKBATHP.PICKNUMPCS);
         PICKSTART Like(Tmplt_PICKBATHP.PICKSTART);
         PICKEND Like(Tmplt_PICKBATHP.PICKEND);
         PICKDUR Like(Tmplt_PICKBATHP.PICKDUR);
         PICKCRTUSR Like(Tmplt_PICKBATHP.PICKCRTUSR);
         PICKCRTTZ Like(Tmplt_PICKBATHP.PICKCRTTZ);
         PICKCHGUSR Like(Tmplt_PICKBATHP.PICKCHGUSR);
         PICKCHGTZ Like(Tmplt_PICKBATHP.PICKCHGTZ);
         PICKCHGJBU Like(Tmplt_PICKBATHP.PICKCHGJBU);
         PICKCHGJBN Like(Tmplt_PICKBATHP.PICKCHGJBN);
         PICKCHGJB# Like(Tmplt_PICKBATHP.PICKCHGJB#);
         PICKSEQNO Like(Tmplt_PICKBATHP.PICKSEQNO);
         END-DS;
       //Standalone Fields
       Dcl-S Dspf_rrn int(5);
       Dcl-S save_rrn int(5);
       Dcl-S curr_rrn int(5);
       Dcl-S Successful ind;
       Dcl-s CheckRN packed(4);
       Dcl-s VAR1 packed(7);
       Dcl-s VAR2 packed(7);
       Dcl-S wk_OneMonthAgo date;
       Dcl-S wk_LimitBats packed(5:0) inz(1000);

       dcl-c SQLOK '00000';
       dcl-c SQL_DUPLICATE '23505';

       dcl-pr batchDetail extpgm('PICKBATDR');
         KeyPICKBAT like(Table_Out.PICKBAT) Const;
       end-pr;

       // Make completed batches available to HYR0600
       dcl-pr Publish_Shipments extpgm('PICKBATLR2');
         in_Batch   packed(8:0) const;
         out_ShpGrp packed(9:0);
         in_OverrideGrp# packed(8:0) options(*nopass: *omit);
         in_MegaBatch# packed(8:0) const options(*nopass);
       end-pr;
       //--------------------------------//
       // Procedure: MainLine            //
       //  Main logic for program        //
       //--------------------------------//

       Dcl-Proc MainLine ;

         EXEC SQL
           Set Option
             Naming = *Sys,
             Commit = *None,
             UsrPrf = *User,
             DynUsrPrf = *User,
             Datfmt = *iso,
             CloSqlCsr = *EndMod ;

       Open DisplyFile;
       Successful = Open_SQL();
       If Not Successful ;
         Display_Msg();
         Close DisplyFile;
         Return ;
       EndIf;

       Ctl_Fields.SHOWCOMP = 'N';

       wk_OneMonthAgo = %date-%months(1);

       // TODO: Figure out how to save previous by user
       Ctl_Fields.INVLOCFLTR = 'WEST';

       Dou Ctl_Fields.BtnExit = *On ;

             Close_SQL();
             Successful = Open_SQL();
                  If Not Successful ;
                    Display_Msg();
                    Close DisplyFile;
                    Return ;
                  EndIf;

          Successful = Load_SFL() ;
           If Not Successful ;
              Display_Msg();
              Close_SQL();
              Close DisplyFile;
              Return ;
           EndIf;

         Ctl_Fields.ACT_MERGE = '0';
         Ctl_Fields.ACT_SPLIT = '0';
         Ctl_Fields.DragFromId = '';
         Display_CTL();

         Select;
           When Ctl_Fields.DragFromId='Subfile   ' and
                Ctl_Fields.BtnExit = *OFF;
             If Ctl_Fields.DragFromRn <> 0 AND Ctl_Fields.DragtoID= 'Subfile';
               IF Ctl_Fields.DragFromRn > Ctl_Fields.DragToRn;
                 CHECKRN = Ctl_Fields.DragtoRN + 1;
               ENDIF;
             IF Ctl_Fields.DragFromRn < Ctl_Fields.DragToRn;
               CHECKRN = Ctl_Fields.DragtoRN;
             ENDIF;
         chain Ctl_Fields.DragFromRn DisplyFile.SFL Sfl_In;         //need to add and remove stuff
         Chain (SFL_IN.PICKBAT) PICKBATHP;
         DS2.PICKBAT = PICKBAT;
         DS2.PICKER  = PICKER;
         DS2.PICKINVLOC  = PICKINVLOC;
         DS2.PICKSTAT = PICKSTAT;
         DS2.PICKNUMITM = PICKNUMITM;
         DS2.PICKNUMPCS = PICKNUMPCS;
         DS2.PICKSTART = PICKSTART;
         DS2.PICKEND = PICKEND;
         DS2.PICKDUR = PICKDUR;
         DS2.PICKCRTUSR = PICKCRTUSR;
         DS2.PICKCRTTZ = PICKCRTTZ;
         DS2.PICKCHGUSR = PICKCHGUSR;
         DS2.PICKCHGTZ = PICKCHGTZ;
         DS2.PICKCHGJBU = PICKCHGJBU;
         DS2.PICKCHGJBN = PICKCHGJBN;
         DS2.PICKCHGJB# = PICKCHGJB#;
         DS2.PICKSEQNO = PICKSEQNO;
         VAR1=DS2.PICKSEQNO;
         Chain checkRn DisplyFile.SFL Sfl_In;
         Chain SFL_IN.PICKBAT PICKBATHP;
         DS1.PICKBAT = PICKBAT;
         DS1.PICKER  = PICKER;
         DS1.PICKINVLOC  = PICKINVLOC;
         DS1.PICKSTAT = PICKSTAT;
         DS1.PICKNUMITM = PICKNUMITM;
         DS1.PICKNUMPCS = PICKNUMPCS;
         DS1.PICKSTART = PICKSTART;
         DS1.PICKEND = PICKEND;
         DS1.PICKDUR = PICKDUR;
         DS1.PICKCRTUSR = PICKCRTUSR;
         DS1.PICKCRTTZ = PICKCRTTZ;
         DS1.PICKCHGUSR = PICKCHGUSR;
         DS1.PICKCHGTZ = PICKCHGTZ;
         DS1.PICKCHGJBU = PICKCHGJBU;
         DS1.PICKCHGJBN = PICKCHGJBN;
         DS1.PICKCHGJB# = PICKCHGJB#;
         DS1.PICKSEQNO = PICKSEQNO;
         VAR2=DS1.PICKSEQNO;

             IF DS2.PICKSEQNO > DS1.PICKSEQNO;
                CHAIN  DS2.PICKBAT PICKBATHP;
                PICKSEQNO = DS1.PICKSEQNO;
                UPDATE PICKBATHR;
                CHAIN  DS1.PICKBAT PICKBATHP;
                  DOW PICKBAT <> DS2.PICKBAT;
                    Chain checkRn DisplyFile.SFL Sfl_In;
                    Chain SFL_IN.PICKBAT PICKBATHP;
                    If PICKBAT = DS2.PICKBAT;
                     setll *loval PICKBATHP;
                     leave;
                    endif;
                    PICKSEQNO = PICKSEQNO +1;
                    UPDATE PICKBATHR;
                    CHECKRN = CHECKRN + 1;
                  ENDDO;
             ENDIF;
             Chain checkRn DisplyFile.SFL Sfl_In;
             IF DS2.PICKSEQNO < DS1.PICKSEQNO;
                CHAIN  DS2.PICKBAT PICKBATHP;
                PICKSEQNO = DS1.PICKSEQNO;
                UPDATE PICKBATHR;
                CHAIN  DS1.PICKBAT PICKBATHP;
                  DOW PICKBAT <> DS2.PICKBAT;
                    Chain checkRn DisplyFile.SFL Sfl_In;
                    Chain SFL_IN.PICKBAT PICKBATHP;
                      If PICKBAT = DS2.PICKBAT;
                       setll *loval PICKBATHP;
                       leave;
                      endif;
                    PICKSEQNO = PICKSEQNO -1;
                    UPDATE PICKBATHR;
                    CHECKRN = CHECKRN - 1;
                  ENDDO;
             ENDIF;
           Endif;
          EndSl;

         if (Ctl_Fields.ACT_MERGE = '1' or
             Ctl_Fields.ACT_SPLIT = '1' or
             Ctl_Fields.ACT_MRGSHP = '1' or
             Ctl_Fields.ACT_MEGA = '1');
           Successful =  Process_Action();
         else;
           Select ;

             When Ctl_Fields.BtnSearch = *Off and
                  Ctl_Fields.BtnExit = *Off;
               Successful =  Process_SFL() ;
               If Not Successful ;
                 Display_Msg();
                 Close_SQL();
                 Close DisplyFile;
                 Return ;
               EndIf;

           EndSl;
         endif;
       EndDo;
       Close_SQL();
       Close DisplyFile;

       *inlr = *on;
       Return;

       End-Proc;

       //-------------------------------------------------//
       // Procedure: Open_SQL                             //
       //  Procedure Prepare, Declare and Open SQL cursor //
       //    Parms:                                       //
       //      None                                       //
       //    Return:                                      //
       //      None                                       //
       //-------------------------------------------------//

       Dcl-Proc Open_SQL ;
           Dcl-Pi *n ind;
           End-Pi;
         Dcl-S SearchTerm VarChar(100);

         SearchTerm = '%' + %Trim(Ctl_Fields.Search) + '%' ;

         EXEC SQL DECLARE C1 CURSOR FOR
           SELECT
           PICKBATHP.PICKBAT ,
           PICKBATHP.PICKER ,
           PICKBATHP.PICKINVLOC ,
           PICKBATHP.PICKSTAT ,
         //PICKBATHP.PICKTURN ,
         //PICKBATHP.PICKORD ,
           PICKBATHP.PICKNUMITM ,
           PICKBATHP.PICKNUMPCS ,
           PICKBATHP.PICKSTART ,
           PICKBATHP.PICKEND ,
           PICKBATHP.PICKDUR ,
           PICKBATHP.PICKSEQNO ,
           pickbathp.pickcrttz
           FROM PICKBATHP
           WHERE
             not exists(select * from pickmbatdp t1
                         where t1.pickbat = PICKBATHP.PICKBAT)
             and
             (:SearchTerm = ' '
             OR PICKBATHP.PICKBAT LIKE :SearchTerm
             OR UPPER(PICKBATHP.PICKER) LIKE UPPER(:SearchTerm)
             OR UPPER(PICKBATHP.PICKSTAT) LIKE UPPER(:SearchTerm)
             OR UPPER(PICKBATHP.PICKINVLOC) LIKE UPPER(:SearchTerm))
           //OR PICKBATHP.PICKTURN LIKE :SearchTerm
           ORDER BY PICKBATHP.PICKSEQNO,PICKBATHP.PICKSTAT
         //ORDER BY PICKBATHP.PICKSTAT,PICKBATHP.PICKBAT
           FOR FETCH ONLY ;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         EXEC SQL
           OPEN C1 USING
           :SearchTerm
           , :SearchTerm
           , :SearchTerm
           , :SearchTerm
           , :SearchTerm;

         If SQLCODE < 0;
           Return *Off;
         EndIf;


         return *on;

       end-Proc;

       //----------------------------------//
       // Procedure: Close_SQL             //
       //  Procedure to close SQL cursor   //
       //    Parms:                        //
       //      None                        //
       //    Return:                       //
       //      None                        //
       //----------------------------------//

       Dcl-Proc Close_SQL ;
           Dcl-Pi *n ind End-Pi;

        EXEC SQL
           CLOSE C1 ;

         Return *On ;

       end-Proc;

       //----------------------------------//
       // Procedure: Load_SFL              //
       //  Procedure to populate data grid //
       //    Parms:                        //
       //      None                        //
       //    Return:                       //
       //      None                        //
       //----------------------------------//

       Dcl-Proc Load_SFL ;

         Dcl-Pi *n ind End-Pi;

         dcl-s numOrds int(10);
         dcl-s batchNum like(PICKBAT);
         dcl-s loadRow ind;
         dcl-s workCustNameList varchar(300);

         dcl-s numOfCust int(10);
         dcl-s firstCust packed(7:0);
         dcl-s megaBatchTotalPieces int(10);

         dcl-s wk_LimitBat like(PICKBAT);
         dcl-s wk_LimitBat1 like(PICKBAT);
         dcl-s wk_LimitBat2 like(PICKBAT);

       // Determine starting batch for ShowComp='Y'.  This will include
       // completed picks that were created since one month ago OR that
       // are that are a recent batch (as defined in wk_LimitBats).
          if Ctl_Fields.showcomp = 'Y';
            wk_LimitBat1 = *Zero;
            wk_LimitBat2 = *Zero;
            EXEC SQL select PICKBAT into :wk_LimitBat1
                     from PICKBATHP
                     where date(PICKCRTTZ)>:wk_OneMonthAgo
                     order by PICKCRTTZ, PICKBAT desc
                     fetch first row only;
            EXEC SQL select PICKBAT into :wk_LimitBat2
                     from PICKBATHP
                     order by PICKBAT desc
                     limit 1 offset :wk_LimitBats;
            If wk_LimitBat1 < wk_LimitBat2;
              wk_LimitBat = wk_LimitBat1 -1;
            ELSE;
              wk_LimitBat = wk_LimitBat2 -1;
            EndIf;                                                           // wk_LimitBat1<wk_Limi
          EndIf;                                                             // Ctl_Fields.showcomp=

       // -- Load All Subfile
       //Clear Ctl_Fields_Out ;
         Ctl_Fields_Out.Clear = *On ;
         Write DisplyFile.Ctl Ctl_Fields_Out ;
         Ctl_Fields_Out.Clear = *Off ;

         Ctl_Fields.SflFull = *Off;

         EXEC SQL
           FETCH C1 INTO :Table_In ;

         If SQLCODE < 0;
           Return *Off;
         EndIf;
         Dspf_rrn = 0;
         Dow SQLCODE = 0;

           If Dspf_rrn = 9999;
             Ctl_Fields.SflFull = *On;
             Leave;
           EndIf;

           // Decide to show row based on completion status
      //   if Ctl_Fields.showcomp = 'Y' or
      //      (Ctl_Fields.showcomp = 'N' and
      //      (Table_In.PICKSTAT <> 'Z' and Table_In.PICKSTAT <> 'Y'));
           if (Ctl_Fields.showcomp = 'Y' and (Table_In.PICKBAT>wk_LimitBat or
               (Table_In.PICKSTAT <> 'Z' and Table_In.PICKSTAT <> 'Y')))  or
              (Ctl_Fields.showcomp = 'N' and
              (Table_In.PICKSTAT <> 'Z' and Table_In.PICKSTAT <> 'Y'));
             loadRow = *on;
           else;
             loadRow = *off;
           endif;

           // Decide to show row based on DC (inventory location) dropdown
           //   ex: ALDA,WEST or ALL
           if loadRow and
              (Ctl_Fields.INVLOCFLTR = 'ALL' or
              (Ctl_Fields.INVLOCFLTR = Table_In.PICKINVLOC));
             loadRow = *on;
           else;
             loadRow = *off;
           endif;


           if loadRow = *on;
             Dspf_rrn+=1;
             Clear Sfl_Out ;
             Eval-Corr Sfl_Out = Table_In ;

             Sfl_Out.CHKROW = '0';

             if Table_In.PICKSTAT = 'B';
               // being picked
               Sfl_Out.iconColor = 'green';
               Sfl_Out.iconName = 'material:shopping_cart';
             elseif Table_In.PICKSTAT = 'P';
               // paused
               Sfl_Out.iconColor = '#CCCC00';
               Sfl_Out.iconName = 'material:pause';
             elseif Table_In.PICKSTAT = 'Y';
               // done -- not complete
               Sfl_Out.iconColor = '#ed1b2f';
               Sfl_Out.iconName = 'material:local_shipping';
             elseif Table_In.PICKSTAT = 'Z';
               // done
               Sfl_Out.iconColor = 'black';
               Sfl_Out.iconName = 'material:local_shipping';
             else;
               // waiting to be picked
               Sfl_Out.iconColor = '#990000';
               Sfl_Out.iconName = 'material:pending';
             endif;

             if Sfl_Out.PICKSTART <> *loval;
              // Sfl_Out.pickstrvs1 = *on;
              // Sfl_Out.pickstrvs2 = *on;
             //  Sfl_Out.pickstrdat = %date(Sfl_Out.PICKSTART);
               Sfl_Out.pickstrtim = %time(Sfl_Out.PICKSTART);
             else;
             //  Sfl_Out.pickstrvs1 = *off;
             //  Sfl_Out.pickstrvs2 = *off;
             endif;

             if Sfl_Out.PICKEND <> *loval;
               //Sfl_Out.pickendvs1 = *on;
               //Sfl_Out.pickendvs2 = *on;
               Sfl_Out.pickenddat = %date(Sfl_Out.PICKEND);
               Sfl_Out.pickendtim = %time(Sfl_Out.PICKEND);
             else;
               //Sfl_Out.pickendvs1 = *off;
               //Sfl_Out.pickendvs2 = *off;
             endif;

             chain ('SHIPUSER':Sfl_Out.PICKER) GUPTDAT;
             if %found(GUPTDAT);
               //Sfl_Out.PICKERNAME = %trim(TDKEY1) + '-' +
               //                     %trim(TDDESC);
               Sfl_Out.PICKERNAME = %trim(TDDESC);
             endif;

             batchNum = Sfl_Out.PICKBAT;
             if isMegaBatch(batchNum);
              sfl_out.pickbatstr = 'M-' + %char(Table_In.PICKBAT);
              Sfl_Out.BATCHCRTT = Table_In.pickcrttz;
              getTotalItemsAndPieces(batchNum
                                     :sfl_Out.PICKNUMITM
                                     :megaBatchTotalPieces);
              Sfl_Out.SHWNUMPICS = *off;
              Sfl_Out.PICKNUM999 = %char(megaBatchTotalPieces);
              sfl_Out.deleteImg = 'material:undo';
             else;
              sfl_Out.deleteImg = 'material:clear';
              sfl_out.pickbatstr = %char(Table_In.PICKBAT);
              EXEC SQL
                select count(distinct pickord)
                  into :numOrds
                  from PICKBATDP
                  where PICKBAT = :batchNum;



              Sfl_Out.TURNLISTTT = *blanks;
              Sfl_Out.ORDLISTTT = *blanks;
              Sfl_Out.SHIPLISTTT = *blanks;

              if numOrds > 1;
                numOfCust = 0;
                Sfl_Out.TURNLIST = '******';
                Sfl_Out.TURNLISTTT = Get_Turn_List(batchNum);
                Sfl_Out.ORDLIST = '******';
                Sfl_Out.ORDLISTTT = Get_Ord_List(batchNum);
                 Sfl_Out.SHIPLISTTT = Get_Ship_List(batchNum:'1'
                                                :workCustNameList
                                                :numOfCust
                                                :firstCust);
                Sfl_Out.CUSTNAMETT = workCustNameList;
                if numOfCust > 1;
                 Sfl_Out.SHIPLIST = '******';
                 Sfl_Out.CUSTNAME =  '******';
                else;
                 Sfl_Out.SHIPLIST = %char(firstCust);
                 Sfl_Out.CUSTNAME = workCustNameList;
                endif;

              else;
                Sfl_Out.TURNLIST = Get_Turn_List(batchNum);
                Sfl_Out.ORDLIST = Get_Ord_List(batchNum);
                Sfl_Out.SHIPLIST = Get_Ship_List(batchNum:'0');
                Sfl_Out.SHIPLISTTT = Get_Ship_List(batchNum:'1'
                                                 :workCustNameList);
                Sfl_Out.CUSTNAMETT = workCustNameList;
                Sfl_Out.CUSTNAME = workCustNameList;
              endif;

              if Sfl_Out.PICKNUMPCS < 99999;
                Sfl_Out.SHWNUMPICS = *on;
              else;
                Sfl_Out.PICKNUM999 = '+++++';
                Sfl_Out.SHWNUMPICS = *off;
              endif;

              Sfl_Out.BATCHCRTT = Table_In.pickcrttz;
             endif;
             Write DisplyFile.SFL Sfl_Out ;
           endif;

           EXEC SQL
             FETCH C1 INTO :Table_In ;
         EndDo;
         save_rrn = Dspf_rrn;

         If SQLCODE < 0;
            Return *Off;
         EndIf;

         Return *On ;
       End-Proc;

       //---------------------------------------//
       // Procedure: Get_Turn_List
       //  Procedure to get Turn #'s
       //    Parms:
       //      None
       //    Return:
       //      None
       //---------------------------------------//

       Dcl-Proc Get_Turn_List ;
        Dcl-Pi *n char(300);
          inBatch Const like(Table_Out.PICKBAT);
        END-PI;

        dcl-s turnList char(300);
        dcl-s didOne ind;
        dcl-s workTurn like(PICKTURN);

        turnList = *blanks;
        didOne = '0';

        EXEC SQL DECLARE GETTURNS CURSOR FOR
          select distinct pickturn
            from pickbatdp
            where pickbat = :inBatch
            order by pickturn
           FOR FETCH ONLY ;
        If SQLCODE < 0;
          Return turnList;
        EndIf;

        EXEC SQL
          OPEN GETTURNS USING
           :inBatch;
        If SQLCODE < 0;
          Return turnList;
        EndIf;

        EXEC SQL
          FETCH GETTURNS INTO :workTurn ;
        If SQLCODE < 0;
          Return turnList;
        EndIf;

        Dow SQLCODE = 0;
          if didOne = '0';
            turnList = %char(workTurn);
            didOne = '1';
          else;
            turnList = %trim(turnList) + X'25' + %char(workTurn);
          endif;

          EXEC SQL
            FETCH GETTURNS INTO :workTurn ;
        enddo;

        EXEC SQL
          CLOSE GETTURNS;

        return turnList;

       end-proc;

       //---------------------------------------//
       // Procedure: Get_Ord_List
       //  Procedure to get Order #'s
       //    Parms:
       //      None
       //    Return:
       //      None
       //---------------------------------------//

       Dcl-Proc Get_Ord_List ;
        Dcl-Pi *n char(300);
          inBatch Const like(Table_Out.PICKBAT);
        END-PI;

        dcl-s ordList char(300);
        dcl-s didOne ind;
        dcl-s workOrd like(PICKORD);

        ordList = *blanks;
        didOne = '0';

        EXEC SQL DECLARE GETORDS CURSOR FOR
          select distinct pickord
            from pickbatdp
            where pickbat = :inBatch
            order by pickord
           FOR FETCH ONLY ;
        If SQLCODE < 0;
          Return ordList;
        EndIf;

        EXEC SQL
          OPEN GETORDS USING
           :inBatch;
        If SQLCODE < 0;
          Return ordList;
        EndIf;

        EXEC SQL
          FETCH GETORDS INTO :workOrd ;
        If SQLCODE < 0;
          Return ordList;
        EndIf;

        Dow SQLCODE = 0;
          if didOne = '0';
            ordList = %char(workOrd);
            didOne = '1';
          else;
            ordList = %trim(ordList) + X'25' + %char(workOrd);
          endif;

          EXEC SQL
            FETCH GETORDS INTO :workOrd ;
        enddo;

        EXEC SQL
          CLOSE GETORDS;

        return ordList;

       end-proc;

       //---------------------------------------//
       // Procedure: Get_Ship_List
       //  Procedure to get Ship To #'s
       //    Parms:
       //      None
       //    Return:
       //      None
       //---------------------------------------//

       Dcl-Proc Get_Ship_List ;
        Dcl-Pi *n char(300);
          inBatch Const like(Table_Out.PICKBAT);
          inName ind Const;
          outCustomNameList varchar(300) options(*nopass:*omit);
          outNumOfCust int(10) options(*nopass:*omit);
          outLastCust packed(7:0) options(*nopass:*omit);
        END-PI;

        dcl-s shipList char(300);
        dcl-s didOne ind;
        dcl-s workTurn like(PICKTURN);
        dcl-s workShip like(PICKORD);
        dcl-s workShipT varchar(50);
        dcl-s wrkOedshp packed(7:0);
        dcl-s dsname varchar(30);
        dcl-s outCustomNameListPassed ind;
        dcl-s shiptoArray packed(7:0) dim(*auto:1000);


        shipList = *blanks;
        didOne = '0';
        if %parms() >= %parmnum(outCustomNameList) and
           %addr(outCustomNameList) <> *null;
         outCustomNameListPassed = *on;
         outCustomNameList = '';
        else;
         outCustomNameListPassed = *off;
        endif;

        EXEC SQL DECLARE GETTURNS3 CURSOR FOR
          select distinct pickturn
            from pickbatdp
            where pickbat = :inBatch
            order by pickturn
           FOR FETCH ONLY ;
        If SQLCODE < 0;
          Return shipList;
        EndIf;

        EXEC SQL
          OPEN GETTURNS3 USING
           :inBatch;
        If SQLCODE < 0;
          Return shipList;
        EndIf;

        EXEC SQL
          FETCH GETTURNS3 INTO :workTurn ;
        If SQLCODE < 0;
          Return shipList;
        EndIf;

        Dow SQLCODE = 0;

          clear workShipT;
          chain (workTurn) OEORHP;
           if not %found(OEORHP);
             IHSHTO = 0;
             workShip = 0;
           else;
             workShip = IHSHTO;
           endif;

          if %elem(shiptoArray) = 0 or %lookup(IHSHTO:shiptoArray) = 0;
           shiptoArray(*next) = IHSHTO;
           chain (workShip) HDCUST;
            if not %found(HDCUST);
             CMCNA1 = *blanks;
           endif;

           if not isDropShip(ihord#:wrkOedshp);
             if inName;
              workShipT = %char(IHSHTO) + ' - ' + %trim(CMCNA1);
              if outCustomNameListPassed;
               if %len(outCustomNameList) > 0;
                 outCustomNameList += x'25';
               endif;
               outCustomNameList +=  %trim(CMCNA1);
              endif;
            else;
              workShipT = %char(IHSHTO);
            endif;

           else;

            workShipT = %char(wrkOedshp);
            if foreignCustomer(ihord#);
               if inName;
                workShipT += ' - ' + %trim(CMCNA1);
                if outCustomNameListPassed;
                 if %len(outCustomNameList) > 0;
                   outCustomNameList += x'25';
                  endif;
                 outCustomNameList +=  %trim(CMCNA1);
                endif;
               endif;
            else;
               if inName;
                exec sql

                select trim(dsname)
                   into :dsname
                  from HDDSHP
                 where dsnmbr = :wrkOedshp;
                workShipT += ' - ' + dsname;
                if outCustomNameListPassed;
                 if %len(outCustomNameList) > 0;
                   outCustomNameList += x'25';
                  endif;
                 outCustomNameList +=  %trim(dsname);
                 endif;
               endif;
            endif;
           endif;

           if didOne = '0';
             shipList = %trim(workShipT);
             didOne = '1';
           else;
             shipList = %trim(shipList) + X'25' + %trim(workShipT);
           endif;
          endif;
          EXEC SQL
            FETCH GETTURNS3 INTO :workTurn ;
        enddo;

        EXEC SQL
          CLOSE GETTURNS3;

        if %parms() >= %parmnum(outNumOfCust) and
           %addr(outNumOfCust) <> *null;
         outNumOfCust = %elem(shiptoArray);
        endif;
        if %parms() >= %parmnum(outLastCust) and
           %addr(outLastCust) <> *null;
          outLastCust = IHSHTO;
        endif;
        return shipList;

       end-proc;

       //---------------------------------------//
       // Procedure: isDropShip                //
       //  check if Drop Ship                 //
       //---------------------------------------//
       dcl-proc isDropShip;
        dcl-pi *n ind;
         inIhord# packed(8:0) value;
         outOedshp packed(7:0);
        end-pi;

        exec sql
          select OEDSHP
            into :outOedshp
           from OEORHD
           where oeord# = :inIhord#;

         if sqlstate = '00000' or %subst(sqlstate:1:2) = '01';
           if outOedshp > 0;
             return *on;
           else;
             return *off;
           endif;
         endif;

         outOedshp = 0;
         return *off;

       end-proc;

       //---------------------------------------//
       // Procedure: foreignCustomer            //
       //  return *on if customer is foreign   //
       //---------------------------------------//

       dcl-proc foreignCustomer;
        dcl-pi *n ind;
         inOrderNum packed(8:0) value;
        end-pi;

         dcl-s tdff02 char(1);

        exec sql
         select guptdat.tdff02
            into :tdff02
            from market.guptdat guptdat
         where guptdat.tdtabl = 'CUSTCLASS'
          and tdkey1 = (
            select hdcust.cmccls
             from xxhdsdata.hdcust hdcust
               join xxhdsdata.oeorhd oeorhd
                on hdcust.cmcust = oeorhd.oeblto
                and oeorhd.oeord# = :inOrderNum
             fetch first 1 rows only
            );

           if sqlstate = '00000' or %subst(sqlstate:1:2) = '01';
            if tdff02 = 'Y';
              return *on;
            endif;
           endif;

           return *off;

       end-proc;

       //---------------------------------------//
       // Procedure: Process_SFL                //
       //  Procedure to process grid selections //
       //    Parms:                             //
       //      None                             //
       //    Return:                            //
       //      None                             //
       //---------------------------------------//

       Dcl-Proc Process_SFL ;
        Dcl-Pi *n ind End-Pi;
        Dcl-S NotFound ind;
        Dcl-S Successful ind;
         ReadC DisplyFile.SFL Sfl_In ;
         If %EOF ;
           Return *On;
         EndIf;

         Select ;
           When Sfl_In.ICONDELETE = *On ;
             if isMegaBatch(Sfl_in.PICKBAT);
              Successful = deleteMegaBatch(Sfl_In.PICKBAT);
             else;
              Successful = Delete_Record(
                             Sfl_In.PICKBAT
                             );
             endif;

           When Sfl_In.ICONEDIT = *On ;
             Successful = Assign_Batch(Sfl_In.PICKBAT);

       //      Successful = Show_Detail(
       //                     Sfl_In.PICKBAT
       //                     : 'EDIT' : NotFound) ;

           When Sfl_In.ICONMSG = *On ;
             Successful = Update_Batch_Message(
                            Sfl_In.PICKBAT
                            ) ;

           When Sfl_In.ICONDETAIL = *On;
             Successful = *on;
             batchDetail(Sfl_In.PICKBAT);

           When Sfl_In.ICONCOMP = *On ;
             Successful = Complete_Batch(Sfl_In.PICKBAT);

           When Sfl_In.ICONPAUSE = *On ;
             Successful = Toggle_Pause(Sfl_In.PICKBAT);

         EndSl;

         Return Successful;

       End-Proc;

       //---------------------------------------//
       // Procedure: Process_Action             //
       //  Procedure to process grid selections //
       //    Parms:                             //
       //      None                             //
       //    Return:                            //
       //      None                             //
       //---------------------------------------//

       Dcl-Proc Process_Action;

        Dcl-Pi *n ind End-Pi;
        Dcl-S Successful ind;
        dcl-s baseBatch like(PICKBAT);
        dcl-s wk_Batch like(PICKBAT);
        dcl-s baseInvLoc like(PICKINVLOC);
        dcl-s wk_InvLoc like(PICKINVLOC);
        dcl-s baseShipTo like(IHSHTO);
        dcl-s wk_ShipTo like(IHSHTO);
        dcl-s rowCount int(5);
        dcl-s wk_NumShip int(5);
        dcl-s batchesForMegaCount int(10) inz(0);

        dcl-s wrkPicker char(10);
        dcl-s wrkBatch zoned(8:0);
        dcl-s wrkPickername char(40);
        dcl-s wrkInvLocation char(4) inz('');
        dcl-s wrkItemcount int(10);
        dcl-s wrkPieces int(10);

         // Assume success
         Successful = *on;
         baseBatch = 0;
         rowCount = 0;

         ReadC DisplyFile.SFL Sfl_In ;
         dow not %eof();

           rowCount += 1;

           select;

             // Splitting a batch
             when Ctl_Fields.ACT_SPLIT = '1';
               Successful = Split_Batch(Sfl_In.PICKBAT);

             // Setting up for merging....
             when baseBatch = 0 and
                  (Ctl_Fields.ACT_MERGE = '1' or Ctl_Fields.ACT_MRGSHP = '1');
               baseBatch = Sfl_In.PICKBAT;

             // Second pass in for ACT_MERGE
             when Ctl_Fields.ACT_MERGE = '1' and baseBatch <> 0;
               Successful = Combine_Batch(baseBatch:Sfl_In.PICKBAT);

             when Ctl_Fields.ACT_MEGA = '1';
              if  sfl_in.PICKSTAT <> 'A';
                Msg_Fields.MsgText = 'Only batches waiting to be ' +
                                    'picked can be added to a Mega Batch';
                exfmt DisplyFile.Message Msg_Fields;
                Successful = *off;
                Return Successful;
              endif;
              batchesForMegaCount += 1;
              if batchesForMegaCount = 1;
                // create temp table batchArray
                exec sql
                  drop table session.batchArray;

                exec sql
                  declare global temporary table batchArray
                     (
                      batch numeric(8,0),
                      picker char(10),
                      pickername char(40),
                      itemcount int,
                      pieces int
                     );

                 wrkInvLocation = sfl_in.PICKINVLOC;
              else;
                 if Sfl_In.PICKINVLOC <> wrkInvLocation;
                  Msg_Fields.MsgText = 'Batches must be in the same location ' +
                                    'to be added to a Mega Batch';
                  exfmt DisplyFile.Message Msg_Fields;
                  Successful = *off;
                  Return Successful;
                 endif;
              endif;
              wrkPicker = sfl_in.picker;
              wrkBatch = sfl_in.PICKBAT;
              wrkPickername = sfl_in.pickername;
              wrkItemcount = sfl_in.PICKNUMITM;
              wrkPieces = sfl_in.PICKNUMPCS;
              exec sql
                insert into session.batchArray
                 values (:wrkBatch,
                         :wrkPicker,
                         :wrkPickername,
                         :wrkItemcount,
                         :wrkPieces);

           endsl;

           ReadC DisplyFile.SFL Sfl_In ;
         enddo;

         // If user requested creation of Mega Batch there's no need to continue in this procedure a
        //   creation of the mega batch.
         if Ctl_Fields.ACT_MEGA = '1';
           if batchesForMegaCount > 0;
            Successful = createMegaBatch(wrkInvLocation);
            return Successful;
           endif;
           return *off;
         endif;

         // validate row counts... get out if an error...
         if rowCount < 2 and Ctl_Fields.ACT_MERGE = '1';
           Msg_Fields.MsgText = 'Please choose 2 or more rows';
           exfmt DisplyFile.Message Msg_Fields;
           Successful = *off;
           Return Successful;
         elseif rowCount > 1 and Ctl_Fields.ACT_MRGSHP = '1';
           Msg_Fields.MsgText = 'Please choose only one row';
           exfmt DisplyFile.Message Msg_Fields;
           Successful = *off;
           Return Successful;
         elseif rowCount < 1 and
                (Ctl_Fields.ACT_SPLIT = '1' or Ctl_Fields.ACT_MRGSHP = '1');
           Msg_Fields.MsgText = 'Please choose at least one row';
           exfmt DisplyFile.Message Msg_Fields;
           Successful = *off;
           Return Successful;
         endif;

         // If we're still here, baseBatch has the first batch, now we want to
         //   check to make sure the baseBatch has only one Ship To.
         if Ctl_Fields.ACT_MRGSHP = '1';

           exec sql
              select count(distinct b.ihshto)
                into :wk_NumShip
                from pickbatdp a
                join oeorhp b on a.pickturn = b.ihturn
               where a.pickbat = :baseBatch;
           if sqlcode < 0;
             wk_NumShip = 0;
           endif;
           // If there is more then 1 distinct Ship To in baseBatch, error out
           if wk_NumShip <> 1;
             Msg_Fields.MsgText = 'Invalid ship to for base batch';
             exfmt DisplyFile.Message Msg_Fields;
             Successful = *off;
             Return Successful;
           endif;

           // If we're still here retrieve the Ship To and Inv Location
           //  for the baseBatch.
           exec sql
              select pickinvloc
                into :baseInvLoc
                from pickbathp
               where pickbat = :baseBatch;
           if sqlcode < 0;
             Msg_Fields.MsgText = 'Unable to get Inventory Location';
             exfmt DisplyFile.Message Msg_Fields;
             Successful = *off;
             Return Successful;
           endif;
           exec sql
              select distinct b.ihshto
                into :baseShipTo
                from pickbatdp a
                join oeorhp b on a.pickturn = b.ihturn
               where a.pickbat = :baseBatch;
           if sqlcode < 0;
             Msg_Fields.MsgText = 'Unable to get unique ship to';
             exfmt DisplyFile.Message Msg_Fields;
             Successful = *off;
             Return Successful;
           endif;

           // Loop through the SFL records
           for curr_rrn = 1 to save_rrn;

             chain curr_rrn DisplyFile.SFL Sfl_In;
             if %found() and Sfl_In.PICKBAT <> baseBatch;

               wk_Batch = Sfl_In.PICKBAT;
               wk_InvLoc = *blanks;
               exec sql
                 select pickinvloc
                   into :wk_InvLoc
                   from pickbathp
                  where pickbat = :wk_Batch;
               wk_NumShip = 0;
               wk_ShipTo = 0;
               exec sql
                 select count(distinct b.ihshto)
                   into :wk_NumShip
                   from pickbatdp a
                   join oeorhp b on a.pickturn = b.ihturn
                  where a.pickbat = :wk_Batch;
               if wk_NumShip = 1;
                 exec sql
                   select distinct b.ihshto
                     into :wk_ShipTo
                     from pickbatdp a
                     join oeorhp b on a.pickturn = b.ihturn
                    where a.pickbat = :wk_Batch;

                 if wk_InvLoc = baseInvLoc and
                    wk_ShipTo = baseShipTo;
                    Successful = Combine_Batch(baseBatch:wk_Batch);
                 endif;

               endif;

             endif;

           endfor;

         endif;

         Return Successful;

       End-Proc;

       //--------------------------------------------------//
       // Procedure: Combine_Batch                         //
       //  Procedure to combine batch records              //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Combine_Batch ;

         Dcl-Pi *n ind;
           baseBatch Const like(Table_Out.PICKBAT);
           moveBatch Const like(Table_Out.PICKBAT);
         End-Pi;

         dcl-s nextSeq like(PICKSEQ);
         dcl-s totItems int(10);
         dcl-s totPieces int(10);
         dcl-s baseTurn like(PICKTURN);
         dcl-s baseCDDS# like(CDDS#);
         dcl-s baseCDSTYP like(CDSTYP);
         dcl-s baseCDSHPV like(CDSHPV);
         dcl-s baseCDBLTO like(CDBLTO);
         dcl-s moveTurn like(PICKTURN);
         dcl-s moveCDDS# like(CDDS#);
         dcl-s moveCDSTYP like(CDSTYP);
         dcl-s moveCDSHPV like(CDSHPV);
         dcl-s moveCDBLTO like(CDBLTO);
         dcl-s multiBillTo ind;

         clear Ship_Via_Fields;

         // get the last seq# used...
         EXEC SQL
           select max(PICKSEQ) into :nextSeq
             from PICKBATDP
             where PICKBAT = :baseBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         multiBillTo = *off;

         // Use the first Turn# to get the 'base' HYLSGSD1 data but also
         //   check to see if the base batch has multiple bill to's
         setll (baseBatch) PICKBATDP;
         reade(n) (baseBatch) PICKBATDP;
         dow not %found(PICKBATDP);
           chain (PICKTURN) HYLSGSD1;
           if %found(PICKBATDP) and baseTurn = 0;
             baseTurn = PICKTURN;
             baseCDDS# = CDDS#;
             baseCDSTYP = CDSTYP;
             baseCDSHPV = CDSHPV;
             baseCDBLTO = CDBLTO;
           elseif %found(PICKBATDP) and baseTurn <> 0;
             if baseCDBLTO <> CDBLTO;
               multiBillTo = *on;
             endif;
           endif;
           reade(n) (baseBatch) PICKBATDP;
         enddo;

         // Only check the rest if we don't already have multiple Bill To's
         if multiBillTo = *off and baseCDSTYP <> *blanks;

           // Look to see if there are any HYPSGSD rows for
           //   the moving Batch
           chain(n) (moveBatch) PICKBATDP;
           if %found(PICKBATDP);
             chain (PICKTURN) HYLSGSD1;
             if %found(PICKBATDP);
               moveTurn = PICKTURN;
               moveCDDS# = CDDS#;
               moveCDSTYP = CDSTYP;
               moveCDSHPV = CDSHPV;
               moveCDBLTO = CDBLTO;
             endif;
           endif;

           // If there are HYPSGSD rows and they are different pop a screen
           //  to force the user to pick one.
           if baseCDSTYP <> *blanks and
              moveCDSTYP <> *blanks and
              (baseCDSTYP <> moveCDSTYP or baseCDSHPV <> moveCDSHPV);

              Ship_Via_Fields.SHPVCHOICE =
                Trans_ShipVia(baseCDSTYP:baseCDSHPV) + ',' +
                Trans_ShipVia(moveCDSTYP:moveCDSHPV);
              Ship_Via_Fields.SHPVVALUE = '1,2';

              exfmt displyfile.Ship_Via Ship_Via_Fields;

              if Ship_Via_Fields.UPD_SHPV = *on;
                // Update the HYPSGSD rows with the choice the user picked.
                if Ship_Via_Fields.SHPVPICK = '1';
                  exec sql
                    update HYPSGSD
                       set CDDS# = :baseCDDS#,
                           CDSTYP = :baseCDSTYP,
                           CDSHPV = :baseCDSHPV
                     where CDTURN in (:baseTurn, :moveTurn);
                else;
                  exec sql
                    update HYPSGSD
                       set CDDS# = :moveCDDS#,
                           CDSTYP = :moveCDSTYP,
                           CDSHPV = :moveCDSHPV
                     where CDTURN in (:baseTurn, :moveTurn);
                endif;
              elseif Ship_Via_Fields.CNC_SHPV = *on;
                return *off;
              endif;

           endif;

         endif;

         // Loop through the moveBatch records and update the PICKBAT
         //   and PICKSEQ
         nextSeq += 1;
         setll (moveBatch) PICKBATDP;
         reade (moveBatch) PICKBATDP;
         dow not %eof(PICKBATDP);

           PICKBAT = baseBatch;
           PICKSEQ = nextSeq;
           update PICKBATDR;

           nextSeq += 1;
           reade (moveBatch) PICKBATDP;
         enddo;

         // get the last seq# used...
         EXEC SQL
           select count(*), sum(PICKNEED)
             into :totItems, :totPieces
             from PICKBATDP
             where PICKBAT = :baseBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         if totItems > 99999;
           totItems = 99999;
         endif;
         if totPieces > 99999;
           totPieces = 99999;
         endif;

         // update the totals
         EXEC SQL
           update PICKBATHP
             set PICKNUMITM = :totItems,
                 PICKNUMPCS = :totPieces
             where PICKBAT = :baseBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         // delete the header from the moved batch
         EXEC SQL
           delete PICKBATHP
             where PICKBAT = :moveBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         pickbatsv_setDefaultPickOrder(baseBatch);

         Return *On;

       End-Proc;


       //--------------------------------------------------//
       // Procedure: Trans_ShipVia                         //
       //  Procedure to translate ship via codes           //
       //    Parms:                                        //
       //      Ship Type Code                              //
       //      Ship Via Code                               //
       //    Return:                                       //
       //      Ship Type Text                              //
       //      Ship Via Text                               //
       //--------------------------------------------------//

       Dcl-Proc Trans_ShipVia;

         Dcl-Pi *n char(30);
           inShipType char(1) const;
           inShipVia char(2) const;
         End-Pi;

         dcl-s rtnText char(30) inz;

         if inShipType = 'T';
           rtnText = 'Truck -';
         else;
           rtnText = 'Parcel -';
         endif;

         select;
           when inShipVia = 'PU';
             rtnText = %trim(rtnText) + ' PICKED UP';
           when inShipVia = 'FE';
             rtnText = %trim(rtnText) + ' FEDEX GROUND';
           when inShipVia = 'FT';
             rtnText = %trim(rtnText) + ' FDX FREIGHT';
           when inShipVia = 'OD';
             rtnText = %trim(rtnText) + ' OLD DOMINION';
           when inShipVia = 'AB';
             rtnText = %trim(rtnText) + ' ABF FREIGHT';
           when inShipVia = 'UP';
             rtnText = %trim(rtnText) + ' UPS GROUND';
           other;
             rtnText = %trim(rtnText) + ' BEST WAY';
         endsl;

         return rtnText;

       End-Proc;
       //--------------------------------------------------//
       // Procedure: Split_Batch                           //
       //  Procedure to split batch records                //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Split_Batch ;

         Dcl-Pi *n ind;
           splitBatch Const like(Table_Out.PICKBAT);
         End-Pi;

         dcl-s origTurn like(PICKTURN);
         dcl-s workTurn like(PICKTURN);
         dcl-s wk_Batch packed(11:0);
         dcl-s hdrSuccess ind;

         // get the original Turn (lowest) from this batch
         EXEC SQL
           select min(PICKTURN) into :origTurn
             from PICKBATDP
             where PICKBAT = :splitBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         // Now loop through all of the OTHER turns in this batch
         //  and give them new batch numbers....
         EXEC SQL DECLARE GETTURNS2 CURSOR FOR
           select distinct pickturn
             from pickbatdp
             where pickbat = :splitBatch
             and pickturn <> :origTurn
            FOR FETCH ONLY ;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         EXEC SQL
           OPEN GETTURNS2 USING
            :splitBatch,
            :origTurn;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         EXEC SQL
           FETCH GETTURNS2 INTO :workTurn ;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         Dow SQLCODE = 0;

           // get next batch number
           in *lock lastUsedBatch;
           lastUsedBatch += 1;
           wk_Batch = lastUsedBatch;
           out lastUsedBatch;

           // create a new header for the current Turn
           hdrSuccess = *off;
           hdrSuccess = Crt_Batch_Hdr(wk_Batch:workTurn:splitBatch);
           if hdrSuccess = *on;
             // Switch the batch# for the current Turn
             //  (leave the seq # the same, for now)
             EXEC SQL
               update pickbatdp
                 set PICKBAT = :wk_Batch
                 where PICKTURN = :workTurn;
             // update the original header
             EXEC SQL
               update pickbathp
                set PICKNUMITM =
                 (select count(*)
                  from pickbatdp where pickbat = :splitBatch),
                 PICKNUMPCS =
                 (select sum(PICKNEED)
                  from pickbatdp where pickbat = :splitBatch)
                where pickbat = :splitBatch;
           endif;

           EXEC SQL
             FETCH GETTURNS2 INTO :workTurn ;
         enddo;

         EXEC SQL
           CLOSE GETTURNS2;

         Return *On;

       End-Proc;

       //--------------------------------//
       // Procedure: Crt_Batch_Hdr
       //  Procedure to create a pick batch header
       //    Parms:                      //
       //      None                      //
       //    Return:                     //
       //      None                      //
       //--------------------------------//

       Dcl-Proc Crt_Batch_Hdr ;

         Dcl-Pi *n ind;
           newBatch Const like(PICKBAT);
           dtlTurn  Const like(PICKTURN);
           oldBatch Const like(PICKBAT);
         End-Pi;

         dcl-s numItems packed(11:0);
         dcl-s numNeeded like(PICKNEED);
         dcl-s hld_INVLOC like(PICKINVLOC);
         dcl-s loTimeStamp timestamp inz(*loval);

         EXEC SQL
           select count(*), sum(PICKNEED)
             into :numItems, :numNeeded
             from pickbatdp
             where pickturn = :dtlTurn;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         EXEC SQL
           select PICKINVLOC
             into :hld_INVLOC
             from pickbathp
             where pickbat = :oldBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         EXEC SQL
           INSERT INTO PICKBATHP
             (PICKBAT, PICKSTAT, PICKNUMITM,
              PICKNUMPCS, PICKSTART, PICKEND, PICKINVLOC)
             VALUES(:newBatch, 'A', :numItems,
                    :numNeeded, :loTimeStamp, :loTimeStamp, :hld_INVLOC);
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         return *on;

       End-Proc;

       //--------------------------------//
       // Procedure: Display_CTL         //
       //  Procedure to display grid     //
       //    Parms:                      //
       //      None                      //
       //    Return:                     //
       //      None                      //
       //--------------------------------//

       Dcl-Proc Display_CTL ;
         Exfmt DisplyFile.Ctl Ctl_Fields ;

       End-Proc;

       //--------------------------------------------------//
       // Procedure: Delete_Record                         //
       //  Procedure to confirm deletion and remove record //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Delete_Record ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
         End-Pi;

         dcl-s shipGroup zoned(9:0);
         dcl-s clearBusy ind;
         dcl-s wk_Turn zoned(9:0);

         Clear Delete_Fields ;

         Exfmt DisplyFile.Delete Delete_Fields ;

         If Delete_Fields.BTNYES = *On ;
           exec sql declare turnLoop cursor for
             select
               distinct pickturn
             from pickbatdp
             where pickbat = :KeyPICKBAT
             order by 1
             for fetch only;

           exec sql
             open turnLoop using :KeyPICKBAT;

           exec sql
             fetch turnLoop into :wk_Turn;
           dow SQLCODE = 0;

             clear shipGroup;

             // check to see if there's an active batch in combined ship
             exec sql
               select GCGRP#
                 into :shipGroup
                 from HYPSGCU
                where GCSTS = 'O' and
                      GCGRP# in
                      (select GDGRP# from HYPSGDT where GDTURN = :wk_Turn);

             if shipGroup = 0;
               // Clear the busy flag of all the orders involved
               exec sql
                 update OEORHD
                    set oebusy = ' ', oeudf9 = ' '
                  where oebusy = 'B' and
                        substring(oeudf9,1,10) = 'DIGIPICK' and
                        oeord# in
                        (select pickord from pickbatdp where
                         pickbat = :KeyPICKBAT and
                         pickturn = :wk_Turn);

               // Clear the busy flag of all the turns involved
               exec sql
                 update HYPTDTA
                    set tdbusy = ' '
                  where tdbusy = 'DIGIPICK' and
                        tdturn in
                        (select pickturn from pickbatdp where
                         pickbat = :KeyPICKBAT and
                         pickturn = :wk_Turn);
             endif;

             exec sql
               fetch turnLoop into :wk_Turn;
           enddo;

           EXEC SQL
             DELETE FROM PICKBATDP
             WHERE
                PICKBAT = :KeyPICKBAT ;

           EXEC SQL
             DELETE FROM PICKBATHP
             WHERE
                PICKBAT = :KeyPICKBAT ;

           If SQLCODE < 0;
             Return *Off;
           EndIf;

         EndIf;
         Return *On;
       End-Proc;

       //--------------------------------------------------//
       // Procedure: Complete_Batch                        //
       //                                                  //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Complete_Batch ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
         End-Pi;

         Dcl-S hldPicker char(10);
         Dcl-S out_ShpGrp packed(9:0) inz(0);

         Clear Comp_Batch_Fields ;

         // %oop until done/cancel
         dow 1 = 1;

           Exfmt DisplyFile.Comp_Batch Comp_Batch_Fields ;

           if Comp_Batch_Fields.CANCEL = *on;
             leave;
           elseif Comp_Batch_Fields.VALID_EMP = *on;

             chain ('SHIPUSER':Comp_Batch_Fields.EMPCODE) GUPTDAT;
             if %found(GUPTDAT);
               Comp_Batch_Fields.EMPNAME = TDDESC;
               Comp_Batch_Fields.INVALIDEMP = *off;
               Comp_Batch_Fields.UPD_BATCHV = *on;
             else;
               Comp_Batch_Fields.INVALIDEMP = *on;
               Comp_Batch_Fields.UPD_BATCHV = *off;
             endif;

           elseif Comp_Batch_Fields.UPD_BATCH = *on;

             // Clear the busy flag of all the orders involved
             EXEC SQL
               update OEORHD
                  set oebusy = ' ', oeudf9 = ' '
                where oebusy = 'B' and
                      substring(oeudf9,1,10) = 'DIGIPICK' and
                      oeord# in
                (select pickord from pickbatdp where pickbat = :KeyPICKBAT);

             // Clear the busy flag of all the turns involved
             EXEC SQL
               update HYPTDTA
                  set tdbusy = ' '
                where tdbusy = 'DIGIPICK' and
                      tdturn in
                (select pickturn from pickbatdp where pickbat = :KeyPICKBAT);

             EXEC SQL
               update PICKBATHP
                 set PICKSTAT = 'Z'
                 where PICKBAT = :KeyPICKBAT;
             // Trigger availability for shipment processing (HYR0600)
             Publish_Shipments(KeyPICKBAT: out_ShpGrp);
             leave;

           endif;

         enddo;

         Return *On;
       End-Proc;

       //--------------------------------------------------//
       // Procedure: Toggle_Pause                           //
       //                                                  //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Toggle_Pause ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
         End-Pi;

         EXEC SQL
           update PICKBATHP
             set PICKSTAT =
             case
               when PICKSTAT = 'A' then 'P'
               when PICKSTAT = 'P' then 'A'
               when PICKSTAT = 'B' then 'P'
               else PICKSTAT
             end
             where PICKBAT = :KeyPICKBAT;

         Return *On;
       End-Proc;

       //--------------------------------------------------//
       // Procedure: Assign_Batch                          //
       //                                                  //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Assign_Batch ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
         End-Pi;

         Dcl-S hldPicker char(10);

         Clear Assign_Bat_Fields ;

         EXEC SQL
           select PICKER into :hldPicker
             from PICKBATHP
             where PICKBAT = :KeyPICKBAT;

         Assign_Bat_Fields.scPicker = hldPicker;
         Exfmt DisplyFile.Assign_Bat Assign_Bat_Fields ;
         hldPicker = Assign_Bat_Fields.scPicker;

         if Assign_Bat_Fields.UPD_PICKER = *on ;

           EXEC SQL
             update PICKBATHP
               set PICKER = :hldPicker
               where PICKBAT = :KeyPICKBAT;

         endif;

         Return *On;
       End-Proc;

       //--------------------------------------------------//
       // Procedure: Update_Batch_Message                  //
       //                                                  //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Update_Batch_Message ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
         End-Pi;
         Clear Batch_Msg_Fields ;

         chain (KeyPICKBAT) PICKBATMP;
         if %found(PICKBATMP);
           Batch_Msg_Fields.MSGTXT = PICKBATMSG;
         endif;

         Exfmt DisplyFile.Batch_Msg Batch_Msg_Fields ;

         If Batch_Msg_Fields.UPD_MSG = *On ;

           if %found(PICKBATMP);
             PICKBATMSG = Batch_Msg_Fields.MSGTXT;
             update PICKBATMR;
           else;
             PICKBAT = KeyPICKBAT;
             PICKBATMSG = Batch_Msg_Fields.MSGTXT;
             write PICKBATMR;
           endif;

         EndIf;

         Return *On;
       End-Proc;

       //-------------------------------------//
       // Procedure: Show_Detail              //
       //  Procedure to display detail screen //
       //    Parms:                           //
       //      Mode - 'EDIT' or 'VIEW'        //
       //    Return:                          //
       //      None                           //
       //-------------------------------------//

       Dcl-Proc Show_Detail ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
           Mode CHAR(4) Const ;
           NotFound ind ;
         End-Pi;

         Dcl-S Validated Ind ;
         NotFound = *off;
         Clear Detail_In ;
         Clear Detail_Out ;

         If Mode = 'EDIT' ;
           Detail_Out.ViewMode = *Off ;
         Else ;
           Detail_Out.ViewMode = *On ;
         EndIf;

         EXEC SQL
           SELECT
             PICKBATHP.PICKBAT ,
             PICKBATHP.PICKER ,
             PICKBATHP.PICKINVLOC ,
             PICKBATHP.PICKSTAT ,
           //PICKBATHP.PICKTURN ,
           //PICKBATHP.PICKORD ,
             PICKBATHP.PICKNUMITM ,
             PICKBATHP.PICKNUMPCS ,
             PICKBATHP.PICKSTART ,
             PICKBATHP.PICKEND ,
             PICKBATHP.PICKDUR ,
             PICKBATHP.PICKCHGJBU
           INTO :Table_IN
             FROM PICKBATHP
           WHERE
              PICKBATHP.PICKBAT = :KeyPICKBAT ;

         If SQLCODE = 100;
           NotFound = *on;
           Return *On;
         EndIf;

         If SQLCODE < 0;
           Return *Off;
         EndIf;

         Eval-Corr Detail_Out = Table_In ;
         Detail_In = Detail_Out ;

         Dow Not Validated ;


           Exfmt DisplyFile.Detail Detail_Out ;
           If Detail_Out.BtnCancel = *On ;
             Return *on ;
           EndIf;

           If Mode = 'EDIT' ;
             Validated = Validate_Detail() ;
           Else ;
             Validated = *On ;
           EndIf;

         EndDo ;

         If Detail_In <> Detail_Out ;
           Clear Table_Out ;
           Eval-Corr Table_Out = Detail_Out ;

           EXEC SQL
            UPDATE PICKBATHP  SET
               PICKBAT = :Table_OUT.PICKBAT ,
               PICKER = :Table_OUT.PICKER ,
               PICKINVLOC = :Table_OUT.PICKINVLOC ,
               PICKSTAT = :Table_OUT.PICKSTAT ,
             //PICKTURN = :Table_OUT.PICKTURN ,
             //PICKORD = :Table_OUT.PICKORD ,
               PICKNUMITM = :Table_OUT.PICKNUMITM ,
               PICKNUMPCS = :Table_OUT.PICKNUMPCS ,
               PICKSTART = :Table_OUT.PICKSTART ,
               PICKEND = :Table_OUT.PICKEND ,
               PICKDUR = :Table_OUT.PICKDUR
            WHERE
               PICKBAT = :KeyPICKBAT ;

           If SQLCODE < 0;
              Return *Off;
           EndIf;

         EndIf;
         return *on;
       End-Proc;



       //-------------------------------------------------//
       // Procedure: Validate_Detail                      //
       //  Put all data validation in this procedure      //
       //    Parms:                                       //
       //      None                                       //
       //    Return:                                      //
       //      Indicator (*On if passed validation)       //
       //-------------------------------------------------//

       Dcl-Proc Validate_Detail ;
         Dcl-Pi *n Ind End-Pi;
        // Add your own validation code here.
        // Add your own validation code here.
        // Add your own validation code here.

        // Return *off if validation fails.
         Return *On ;

       End-Proc;

       //-------------------------------------------------//
       // Procedure:Display_Msg                           //
       //  Dislay an SQL error message                    //
       //    Parms:                                       //
       //      None                                       //
       //    Return:                                      //
       //      none                                       //
       //-------------------------------------------------//

       Dcl-Proc Display_Msg ;
         Dcl-Pi Display_Msg End-Pi;

         Dcl-S MessageInfo CHAR(3000) ;



       dcl-pr RtvMsgText EXTPGM('QMHRTVM');
         oMsgInfo      char(1);
         iMsgInfoLen   int(10)  const;
         iReturnFmt    char(8)  const;
         iMsgId        char(7)  const;
         iMsgFile      char(20) const;
         iMsgData      char(1)  const;
         iMsgDataLen   int(10)  const;
         iReplSubst    char(10) const;
         iRtnCtrl      char(10) const;
         ioApiErrorDS  char(1);
         iRtvOption    char(10)       options(*nopass);
         iCCSID        int(10)  const options(*nopass);
         iReplCCSID    int(10)  const options(*nopass);
       end-pr;




       dcl-ds Msg QUALIFIED;
         SQL       char(3)   INZ('SQL');
         Code      zoned(4);
         Identifer char(10) pos(1);
       end-ds;




       dcl-ds RTVM0100 Qualified;
         BytesRtn          int(10);
         BytesAvail        int(10);
         LengthRtn         int(10);
         LengthAvail       int(10);
         LengthHlpRtn      int(10);
         LengthHlpAvail    int(10);
         RTVM0100Msg       char(32767);
       end-ds;




        dcl-ds ERRC0100 Qualified;
         BytesPrv    int(10);
         BytesAvail  int(10);
         ExceptId    char(7);
         Reserved    char(1);
         ExceptData  char(32767);
        end-ds;


       Msg.Code = %Abs(SQLCODE);

       RtvMsgText(MessageInfo :
                  %size(MessageInfo) :
                  'RTVM0100' :
                  Msg.Identifer :
                  'QSQLMSG   QSYS      ':
                  SQLERM :
                  SQLERL :
                  '*YES' :
                  '*NO' :
                  ERRC0100 );
       RTVM0100 = MessageInfo;

       If RTVM0100.LengthRtn > 0;
         Msg_Fields.MsgText = %SubSt(RTVM0100.RTVM0100Msg : 1 :
                               RTVM0100.LengthRtn);

       EndIf;
       If  RTVM0100.LengthHlpRtn > 0;
         Msg_Fields.MsgText = %Trim(Msg_Fields.MsgText) +  ' ' +
              %SubSt(RTVM0100.RTVM0100Msg : RTVM0100.LengthRtn + 1 :
                     RTVM0100.LengthHlpRtn);
       EndIf;


       ExFmt DisplyFile.Message Msg_Fields;
       Return;


       End-Proc;

       //-------------------------------------------------//
       // Procedure:createMegaBatch                       //
       //  Create Mega Batch                              //
       //-------------------------------------------------//

       dcl-proc createMegaBatch;
        dcl-pi *n ind;
          location char(4) value;
        end-pi;

        dcl-s batchCount int(10);
        dcl-s picker char(10);
        dcl-s newBatchNum packed(11:0);
        dcl-s attemptCount int(10) inz(0);
        dcl-c MAXATTEMPTS 10;
        dcl-s numOfItems zoned(5:0);
        dcl-s numOfPieces zoned(5:0);
        // Publish each component pick batch independently (do not force a shared shipment group).
        dcl-s outGrp     packed(9:0);
        dcl-s compBatch  packed(8:0);
        dcl-s wkMegaBatch  packed(8:0);
        dcl-s pgmUser    char(10);

        exec sql
          select count(*)
            into :batchCount
          from session.batchArray;

         if batchCount > 0;
          exec sql
           select sum(itemcount), sum(pieces)
             into :numOfItems, :numOfPieces
            from session.batchArray;
          if sqlstate <> sqlok;
            numOfItems = 0;
            numOfPieces = 0;
          endif;

          exec sql
           select picker
            into :picker
           from session.batchArray
           order by pickername
           fetch first 1 rows only;

           dou sqlstate <> SQL_DUPLICATE
            or attemptCount > MAXATTEMPTS;
            newBatchNum = getNextBatchNum();
            exec sql
              insert into pickbathp (
                                     PICKBAT,
                                     PICKSEQNO,
                                     PICKER,
                                     PICKINVLOC,
                                     PICKSTAT,
                                     PICKNUMITM,
                                     PICKNUMPCS,
                                     PICKSTART,
                                     PICKEND,
                                     PICKDUR,
                                     PICKCRTUSR,
                                     PICKCRTTZ,
                                     PICKCHGUSR,
                                     PICKCHGTZ,
                                     PICKCHGJBU,
                                     PICKCHGJBN,
                                     PICKCHGJB#,
                                     PICKTOT)
              values (:newBatchNum,
                      10,
                      :picker,
                      :location,
                      'A',
                      :numOfItems,
                      :numOfPieces,
                      timestamp('0001-01-01 00:00:00.000000'),
                      timestamp('0001-01-01 00:00:00.000000'),
                      0,
                      :pgmUser,
                      current timestamp,
                      '',
                      timestamp('0001-01-01 00:00:00.000000'),
                      '',
                      '',
                      0,
                      ''
                      );
            if (sqlstate = SQL_DUPLICATE);
              attemptCount += 1;
              if attemptCount > MAXATTEMPTS;
               leave;
              endif;
            endif;
           enddo;

           if sqlstate <> SQLOK and %subst(sqlstate:1:2) <> '01';
            return *off;
           endif;

           exec sql
              insert into pickmbatdp
               (select :newBatchNum, batch
                from batchArray);

            if sqlstate <> SQLOK and %subst(sqlstate:1:2) <> '01';
             return *off;
            endif;
        exec sql
          declare cComp cursor for
            select pickbat
              from pickmbatdp
             where pickmbat = :newBatchNum
             order by pickbat;

        exec sql open cComp;
        dow sqlcode = 0;
           exec sql fetch cComp into :compBatch;
           if sqlcode = 0;
             wkMegaBatch = newBatchNum;
             Publish_Shipments(compBatch: outGrp: *omit: wkMegaBatch);
           Endif;
        enddo;
        exec sql close cComp;
        exec sql
            MERGE INTO pickbatdp   AS tgt
             USING (
              SELECT PICKBAT,
                     PICKSEQ,
                     ROW_NUMBER()
                       OVER (ORDER BY PICKWHS,PICKSTKRM,PICKAISLE,PICKLOC) * 10
                       AS new_ovrord
              FROM pickbatdp
              where pickbat in
                (select batch from batchArray)
                   )   AS src
            ON (tgt.PICKBAT = src.PICKBAT
             AND tgt.PICKSEQ = src.PICKSEQ)
            WHEN MATCHED THEN
           UPDATE SET PICKOVRORD = src.new_ovrord;


           if sqlstate = SQLOK or %subst(sqlstate:1:2) = '01';
            return *on;
           endif;
         endif;



         return *off;

       end-proc;

       //-------------------------------------------------//
       // Procedure:getNextBatchNum                       //
       //  Get Next Batch Number                          //
       //-------------------------------------------------//

       dcl-proc getNextBatchNum;
        dcl-pi *n packed(11:0);
        end-pi;

        dcl-s wrkbatch packed(11:0);

        in *lock lastUsedBatch;
        lastUsedBatch += 1;
        wrkbatch = lastUsedBatch;
        out lastUsedBatch;

        return wrkbatch;

       end-proc;

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
       // Procedure:getTotalItemsAndPieces                //
       //  is this batch a mega batch                     //
       //-------------------------------------------------//

       dcl-proc getTotalItemsAndPieces;
        dcl-pi *n;
         inBatch zoned(8:0) value;
         outItemCount like(sfl_Out.PICKNUMITM);
         outPieces int(10);
        end-pi;

         dcl-s wrkItemCount int(10);
         dcl-s wrkPieces int(10);

         exec sql
           select sum(itemcount), sum(pieces)
             into :wrkItemCount, :wrkPieces
            from session.batchArray;

         outItemCount = wrkItemCount;
         outPieces = wrkPieces;


       end-proc;

       //-------------------------------------------------//
       // Procedure:deleteMegaBatch                       //
       //                                                 //
       //-------------------------------------------------//

       dcl-proc deleteMegaBatch;
        dcl-pi *n ind;
          inBatch zoned(8:0) value;
        end-pi;

         exec sql
          delete  from pickmbatdp
            where pickmbat = :inBatch;

          if sqlstate <> SQLOK and %subst(sqlstate:1:2) <> '01';
            return *off;
          endif;

          exec sql
           delete from pickbathp
            where pickbat = :inBatch;

          if sqlstate <> SQLOK and %subst(sqlstate:1:2) <> '01';
            return *off;
          endif;

          return *on;

        end-proc;
