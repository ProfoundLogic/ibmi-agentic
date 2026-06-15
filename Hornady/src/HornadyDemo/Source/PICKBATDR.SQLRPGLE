       //-------------------------------------------------//
       // Title: Pick Batch Dashboard - Detail
       // Created: 12/8/2021, 4:27:31 PM
       //-------------------------------------------------//
       //-------------------------------------------------//
       // Global Defines                                  //
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
         EXTFILE(*EXTDESC) EXTDESC('PICKBATDD')
                    SFILE(SFL:Dspf_rrn)
                    usropn
                    HANDLER('PROFOUNDUI(HANDLER)');

       Dcl-F HDIMST usage(*input) keyed;
       Dcl-F PICKBATDP usage(*update) keyed;
       Dcl-F GUPTDAT usage(*input) keyed;

       /copy qcpylesrc,pickbatsvd

       //  SDS data structure
        dcl-ds sds psds qualified;
         pgm      char(10)   pos(1);
         status   zoned(5:0) pos(11);
         stmt     char(8)    pos(21);
         exception char(7)    pos(40);
         pgmlib    char(10)   pos(81);
         job       char(10)   pos(244);
         jobuser   char(10)   pos(254);
         jobnum    zoned(6:0) pos(264);
         currentUser char(10)   pos(358);
       end-ds;
       //-------------//
       // Global Data //
       //-------------//
       dcl-s lastUsedBatch packed(11:0) DTAARA('PICKBATD');

       //Data Structures

       Dcl-DS Tmplt_PICKBATDP EXT qualified template
         ExtName('PICKBATDP') ALIAS
       End-DS;

       Dcl-DS Table_In qualified;
         PICKBAT Like(Tmplt_PICKBATDP.PICKBAT);
         PICKSEQ Like(Tmplt_PICKBATDP.PICKSEQ);
         PICKOVRORD Like(Tmplt_PICKBATDP.PICKOVRORD);
         PICKTURN Like(Tmplt_PICKBATDP.PICKTURN);
         PICKORD Like(Tmplt_PICKBATDP.PICKORD);
         PICKORDL Like(Tmplt_PICKBATDP.PICKORDL);
         PICKITEM Like(Tmplt_PICKBATDP.PICKITEM);
         PICKNEED Like(Tmplt_PICKBATDP.PICKNEED);
         PICKQTYP Like(Tmplt_PICKBATDP.PICKQTYP);
         PICKSTKRM Like(Tmplt_PICKBATDP.PICKSTKRM);
         PICKAISLE Like(Tmplt_PICKBATDP.PICKAISLE);
         PICKLOC Like(Tmplt_PICKBATDP.PICKLOC);
         PICKITMST Like(Tmplt_PICKBATDP.PICKITMST);
       End-Ds;

       Dcl-DS Table_Out LikeDS(Table_In);

       Dcl-DS Ctl_Fields LikeRec(DisplyFile.Ctl:*ALL) ;
       Dcl-DS Ctl_Fields_Out LikeRec(DisplyFile.Ctl:*OUTPUT) ;
       Dcl-DS Sfl_All LikeRec(DisplyFile.Sfl:*ALL) ;
       Dcl-DS Sfl_In LikeRec(DisplyFile.Sfl:*INPUT) ;
       Dcl-DS Sfl_Out LikeRec(DisplyFile.Sfl:*OUTPUT) ;
       Dcl-DS Msg_Fields LikeRec(DisplyFile.Message:*All) ;
       Dcl-DS View_In LikeRec(DisplyFile.View:*ALL) ;
       Dcl-DS View_Out LikeRec(DisplyFile.View:*All) ;
       Dcl-DS UpdStat_Fields LikeRec(DisplyFile.UpdStat:*All) ;

       //Standalone Fields
       Dcl-S Dspf_rrn int(5);
       Dcl-S Successful ind;
       Dcl-S hld_PICKBAT like(Table_Out.PICKBAT);

       EXEC SQL
           Set Option
             Naming = *Sys,
             Commit = *None,
             UsrPrf = *User,
             DynUsrPrf = *User,
             Datfmt = *iso,
             CloSqlCsr = *EndMod ;
       //--------------------------------//
       // Procedure: MainLine            //
       //  Main logic for program        //
       //--------------------------------//

       Dcl-Proc MainLine ;
        Dcl-Pi *N;
          KeyPICKBAT like(Table_Out.PICKBAT) Const;
        End-Pi;

        hld_PICKBAT = KeyPICKBAT;
        exec sql
          drop table session.batchlist;

         exec sql
          declare global temporary table batchlist
           (
             batch numeric(8,0)
           );

       if isMegaBatch(hld_PICKBAT);
        exec sql
          insert into batchlist
           (select pickbat from pickmbatdp
              where pickmbat = :hld_PICKBAT);
       else;
        exec sql
         insert into batchlist
          values (:hld_PICKBAT);

       endif;

       Open DisplyFile;
       Successful = Open_SQL();
       If Not Successful ;
         Display_Msg();
         Close DisplyFile;
         Return ;
       EndIf;



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

         Ctl_Fields.ACT_NEW = '0';
         Display_CTL() ;

         if (Ctl_Fields.ACT_NEW = '1');
           Successful =  Process_Action();
         else;

           Select ;
             when ctl_fields.DFTSORT = *on;
              if not pickbatsv_setDefaultPickOrder(KeyPICKBAT);
                 Msg_Fields.MsgText = 'Unable to sort items. Check job log ' +
                   %editc(sds.jobnum:'X') + '/' + %trim(sds.jobuser) + '/' +
                   %trim(sds.job);
                 ExFmt DisplyFile.Message Msg_Fields;
              endif;
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

         EXEC SQL DECLARE GETDETAIL CURSOR FOR
           SELECT
           PICKBATDP.PICKBAT ,
           PICKBATDP.PICKSEQ ,
           PICKBATDP.PICKOVRORD ,
           PICKBATDP.PICKTURN ,
           PICKBATDP.PICKORD ,
           PICKBATDP.PICKORDL ,
           PICKBATDP.PICKITEM ,
           PICKBATDP.PICKNEED ,
           PICKBATDP.PICKQTYP ,
           PICKBATDP.PICKSTKRM ,
           PICKBATDP.PICKAISLE ,
           PICKBATDP.PICKLOC ,
           PICKBATDP.PICKITMST
           FROM PICKBATDP
           WHERE PICKBATDP.PICKBAT in
               (select batch from batchlist)
             AND
             (:SearchTerm = ' '
             OR UPPER(PICKBATDP.PICKITEM) LIKE UPPER(:SearchTerm))
           order by PICKOVRORD,PICKWHS,PICKSTKRM,PICKAISLE,PICKLOC
           FOR FETCH ONLY ;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         EXEC SQL
           OPEN GETDETAIL;

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
           CLOSE GETDETAIL ;

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


       // -- Load All Subfile
       //Clear Ctl_Fields_Out ;
         Ctl_Fields_Out.Clear = *On ;
         Write DisplyFile.Ctl Ctl_Fields_Out ;

         Ctl_Fields.SflFull = *Off;

         EXEC SQL
           FETCH GETDETAIL INTO :Table_In ;

         If SQLCODE < 0;
           Return *Off;
         EndIf;
         Dspf_rrn = 0;
         Dow SQLCODE = 0;

           If Dspf_rrn = 9999;
             Ctl_Fields.SflFull = *On;
             Leave;
           EndIf;

           Dspf_rrn+=1;
           Clear Sfl_Out ;
           Eval-Corr Sfl_Out = Table_In ;

           Sfl_Out.CHKROW = '0';

           Sfl_Out.LOCTXT = Table_In.PICKSTKRM + ' / ' +
                            Table_In.PICKAISLE +
                            Table_In.PICKLOC;

           chain (Sfl_Out.PICKITEM) HDIMST;
           if %found(HDIMST);
             Sfl_Out.PICKITMDSC = IMIMDS;
           endif;

           // Choose the icon type/color
           if Table_In.PICKITMST = 'S';
             // picked
             Sfl_Out.iconColor = 'green';
             Sfl_Out.iconName = 'material:shopping_cart';
             Sfl_Out.iconToolT = 'Picked';
           elseif Table_In.PICKITMST = 'X';
             // skip/underpick
             Sfl_Out.iconColor = '#ed1b2f';
             Sfl_Out.iconName = 'material:shopping_cart';
             Sfl_Out.iconToolT = 'Skipped/Underpicked';
           else;
             // waiting to be picked
             Sfl_Out.iconColor = '#990000';
             Sfl_Out.iconName = 'material:pending';
             Sfl_Out.iconToolT = 'Waiting';
           endif;

           Write DisplyFile.SFL Sfl_Out ;

           EXEC SQL
             FETCH GETDETAIL INTO :Table_In ;
         EndDo;

         If SQLCODE < 0;
            Return *Off;
         EndIf;

         Return *On ;
       End-Proc;

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

        // Check for Drag and Drop attempt.
        if Ctl_Fields.TARGETELEM <> *blanks;
          Change_Pick_Order(Ctl_Fields.ddRec : Ctl_Fields.targetRec);
          Ctl_Fields.TARGETELEM = *blanks;
          Return *on;
        endif;

         ReadC DisplyFile.SFL Sfl_In ;
         If %EOF ;
           Return *On;
         EndIf;

         Select ;

           When Sfl_In.ICONVIEW = *On ;
             Successful = Show_View(
                            Sfl_In.PICKBAT
                            : Sfl_In.PICKSEQ
                            );

           When Sfl_In.UPDSTATUS = *On ;
             Successful = Update_Status(
                            Sfl_In.PICKBAT
                            : Sfl_In.PICKSEQ
                            );

         EndSl;


         Return Successful;

       End-Proc;

       //--------------------------------------------------//
       // Procedure: Change_Pick_Order                     //
       //  Procedure to change the pick order              //
       //    Parms:                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc Change_Pick_Order ;

         Dcl-Pi *n;
           inMoveRRN Const
             like(Dspf_rrn);
           inTargetRRN Const
             like(Dspf_rrn);
         End-Pi;

         Dcl-S targetOvrOrd like(Table_Out.PICKOVRORD);
         Dcl-S moveRRN like(Dspf_rrn);
         Dcl-S targetRRN like(Dspf_rrn);
         Dcl-S saveRRN like(Dspf_rrn);
         Dcl-S goingUp ind;

         moveRRN = inMoveRRN;
         targetRRN = inTargetRRN;
         saveRRN = Dspf_rrn;

         // Should never get this...
         if moveRRN = 0;
           return;
         endif;

         // Set targetRRN to 1 if it's 0
         if targetRRN = 0;
           targetRRN = 1;
         endif;

         // If the target is < the moved row then assume going up, else going down
         if targetRRN < moveRRN;
           goingUp = *on;
         else;
           goingUp = *off;
         endif;

         chain targetRRN DisplyFile.SFL Sfl_All;
         if %found(DisplyFile);
           targetOvrOrd = Sfl_All.PICKOVRORD;
           Dspf_rrn = saveRRN;
         else;
           Dspf_rrn = saveRRN;
           return;
         endif;

         chain moveRRN DisplyFile.SFL Sfl_All;
         if %found(DisplyFile);
           if goingUp;
             Sfl_All.PICKOVRORD = targetOvrOrd - 5;
           else;
             Sfl_All.PICKOVRORD = targetOvrOrd + 5;
           endif;
           update DisplyFile.SFL Sfl_All;
           chain (Sfl_All.PICKBAT:Sfl_All.PICKSEQ) PICKBATDP;
           if %found(PICKBATDP);
             PICKOVRORD = Sfl_All.PICKOVRORD;
             update PICKBATDR;
           endif;
           Dspf_rrn = saveRRN;
         else;
           Dspf_rrn = saveRRN;
           return;
         endif;

         Dspf_rrn = saveRRN;
         return;

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

       //-------------------------------------//
       // Procedure: Show_View                //
       //  Procedure to display View screen   //
       //    Parms:                           //
       //    Return:                          //
       //      None                           //
       //-------------------------------------//

       Dcl-Proc Show_View ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
           KeyPICKSEQ Const
             like(Table_Out.PICKSEQ);
         End-Pi;

         Clear View_In ;
         Clear View_Out ;


         EXEC SQL
           SELECT
           PICKBATDP.PICKBAT ,
           PICKBATDP.PICKSEQ ,
           PICKBATDP.PICKOVRORD ,
           PICKBATDP.PICKTURN ,
           PICKBATDP.PICKORD ,
           PICKBATDP.PICKORDL ,
           PICKBATDP.PICKITEM ,
           PICKBATDP.PICKNEED ,
           PICKBATDP.PICKQTYP ,
           PICKBATDP.PICKSTKRM ,
           PICKBATDP.PICKAISLE ,
           PICKBATDP.PICKLOC ,
           PICKBATDP.PICKITMST
           INTO :Table_IN
           FROM PICKBATDP
           WHERE
              PICKBATDP.PICKBAT = :KeyPICKBAT
             AND PICKBATDP.PICKSEQ = :KeyPICKSEQ ;
           If SQLCODE < 0;
              Return *Off;
           EndIf;
         Eval-Corr View_Out = Table_In ;
         View_In = View_Out ;

         ExFmt DisplyFile.View View_Out;
         Return *on;
       End-Proc;

       //-------------------------------------//
       // Procedure: Update_Status            //
       //  Procedure to update pick status    //
       //    Parms:                           //
       //    Return:                          //
       //      None                           //
       //-------------------------------------//

       Dcl-Proc Update_Status ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
           KeyPICKSEQ Const
             like(Table_Out.PICKSEQ);
         End-Pi;

         if Sfl_In.PICKITMST <> 'X';
           Msg_Fields.MsgText =
             'You can only update Skipped/Underpicked items';
           exfmt DisplyFile.Message Msg_Fields;
         else;

           Clear UpdStat_Fields ;

           // Loop until done/cancel
           dow 1 = 1;

             Exfmt DisplyFile.UpdStat UpdStat_Fields ;

             if UpdStat_Fields.CANCEL = *on;
               leave;
             elseif UpdStat_Fields.VALID_EMP = *on;

               chain ('SHIPUSER':UpdStat_Fields.EMPCODE) GUPTDAT;
               if %found(GUPTDAT);
                 UpdStat_Fields.EMPNAME = TDDESC;
                 UpdStat_Fields.INVALIDEMP = *off;
                 UpdStat_Fields.UPD_STATV = *on;
               else;
                 UpdStat_Fields.INVALIDEMP = *on;
                 UpdStat_Fields.UPD_STATV = *off;
               endif;

             elseif UpdStat_Fields.UPD_STAT = *on;

               chain (KeyPICKBAT:KeyPICKSEQ) PICKBATDP;
               if %found(PICKBATDP);
                 PICKITMST = ' ';
                 update PICKBATDR;
               endif;
               leave;

             endif;

           enddo;

         endif;

         return *on;

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
        Dcl-S NotFound ind;
        Dcl-S Successful ind;
        dcl-s baseBatch like(Table_In.PICKBAT);
        dcl-s newBatch like(Table_In.PICKBAT);
        dcl-s rowCount int(5);
        dcl-s didIt ind;

         baseBatch = 0;
         newBatch = 0;
         rowCount = 0;

         ReadC DisplyFile.SFL Sfl_In ;
         dow not %eof();

           // If we're creating a new batch....
           if Ctl_Fields.ACT_NEW = '1' and
              Sfl_In.CHKROW = '1';

             rowCount += 1;
             // If this is the first one,get a new batch#
             //  and create the new Batch Header
             if didIt = '0';
               didIt = '1';
               baseBatch = Sfl_In.PICKBAT;
               in *lock lastUsedBatch;
               lastUsedBatch += 1;
               newBatch = lastUsedBatch;
               out lastUsedBatch;
               Successful = Crt_Batch_Hdr(newBatch);
             endif;

             New_Batch(Sfl_In.PICKBAT:
                       Sfl_In.PICKSEQ:
                       newBatch);

           endif;

           ReadC DisplyFile.SFL Sfl_In ;
         enddo;

         // if rowCount < 1 send error
         if rowCount < 1 and Ctl_Fields.ACT_NEW = '1';
           Msg_Fields.MsgText = 'Please choose at least one row';
           exfmt DisplyFile.Message Msg_Fields;
         elseif newBatch <> 0;
           Successful = Upd_Batch_Hdr(newBatch);
           Successful = Upd_Batch_Hdr(baseBatch);
         endif;

         Ctl_Fields.ACT_NEW = '0';
         didIt = '0';

         Return Successful;

       End-Proc;

       //--------------------------------------------------//
       // Procedure: New_Batch                             //
       //  Procedure to create new batch                   //
       //    Parms:                                        //
       //      None                                        //
       //    Return:                                       //
       //      None                                        //
       //--------------------------------------------------//

       Dcl-Proc New_Batch ;

         Dcl-Pi *n ind;
           KeyPICKBAT Const
             like(Table_Out.PICKBAT);
           KeyPICKSEQ Const
             like(Table_Out.PICKSEQ);
           newBatch Const
             like(Table_Out.PICKBAT);
         End-Pi;

         EXEC SQL
           UPDATE PICKBATDP
             SET PICKBAT = :newBatch
             WHERE PICKBAT = :KeyPICKBAT and
                   PICKSEQ = :KeyPICKSEQ;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

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
           newBatch Const like(Table_In.PICKBAT);
         End-Pi;

         dcl-s loTimeStamp timestamp inz(*loval);

         EXEC SQL
           INSERT INTO PICKBATHP
             (PICKBAT, PICKSTAT, PICKSTART, PICKEND)
             VALUES(:newBatch, 'A', :loTimeStamp, :loTimeStamp);
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         return *on;

       End-Proc;

       //--------------------------------//
       // Procedure: Upd_Batch_Hdr
       //  Procedure to update the pick batch header
       //    Parms:                      //
       //      None                      //
       //    Return:                     //
       //      None                      //
       //--------------------------------//

       Dcl-Proc Upd_Batch_Hdr ;

         Dcl-Pi *n ind;
           newBatch Const like(Table_In.PICKBAT);
         End-Pi;

         dcl-s numItems packed(11:0);
         dcl-s numNeeded like(Table_In.PICKNEED);
         dcl-s loTimeStamp timestamp inz(*loval);

         EXEC SQL
           select count(*), sum(PICKNEED)
             into :numItems, :numNeeded
             from pickbatdp
             where pickbat = :newBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         EXEC SQL
           UPDATE PICKBATHP
             SET PICKNUMITM = :numItems,
                 PICKNUMPCS = :numNeeded
             WHERE PICKBAT = :newBatch;
         If SQLCODE < 0;
           Return *Off;
         EndIf;

         return *on;

       End-Proc;

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
