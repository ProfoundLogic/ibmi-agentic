      // @MBRTEXT: some app
       //-------------------------------------------------//
       // Title: some app
       // Created: 2/23/2024, 4:58:17 PM
       //-------------------------------------------------//
       //-------------------------------------------------//
       // Global Defines                                  //
       //-------------------------------------------------//

       //---------------//
       // Control Specs //
       //---------------//

       Ctl-Opt OPTION(*SRCSTMT : *NODEBUGIO) DFTACTGRP(*NO) ACTGRP(*CALLER)
               MAIN(MainLine) ;


       //------------//
       // File Specs //
       //------------//

       Dcl-F DisplyFile WORKSTN QUALIFIED ALIAS
         EXTFILE(*EXTDESC) EXTDESC('OFFICE1D')
                    SFILE(SFL:Dspf_rrn)
                    usropn
                    HANDLER('PROFOUNDUI(HANDLER)');


       //-------------//
       // Global Data //
       //-------------//

       //Data Structures

       Dcl-DS Tmplt_OFFICES EXT qualified template
         ExtName('DEMOLIB/OFFICES') ALIAS
       End-DS;

       Dcl-DS Table_In qualified;
         OFFICECODE Like(Tmplt_OFFICES.OFFICECODE);
         CITY Like(Tmplt_OFFICES.CITY);
         PHONE Like(Tmplt_OFFICES.PHONE);
         ADDRESS1 Like(Tmplt_OFFICES.ADDRESS1);
         ADDRESS2 Like(Tmplt_OFFICES.ADDRESS2);
         STATE Like(Tmplt_OFFICES.STATE);
         COUNTRY Like(Tmplt_OFFICES.COUNTRY);
         POSTALCODE Like(Tmplt_OFFICES.POSTALCODE);
         TERRITORY Like(Tmplt_OFFICES.TERRITORY);
       End-Ds;

       Dcl-DS Table_Out LikeDS(Table_In);

       Dcl-DS Ctl_Fields LikeRec(DisplyFile.Ctl:*ALL) ;
       Dcl-DS Ctl_Fields_Out LikeRec(DisplyFile.Ctl:*OUTPUT) ;
       Dcl-DS Sfl_In LikeRec(DisplyFile.Sfl:*INPUT) ;
       Dcl-DS Sfl_Out LikeRec(DisplyFile.Sfl:*OUTPUT) ;
       Dcl-DS Delete_Fields LikeRec(DisplyFile.Delete:*ALL) ;
       Dcl-DS Msg_Fields LikeRec(DisplyFile.Message:*All) ;
       Dcl-DS Detail_In LikeRec(DisplyFile.Detail:*ALL) ;
       Dcl-DS Detail_Out LikeRec(DisplyFile.Detail:*ALL) ;



       //Standalone Fields
       Dcl-S Dspf_rrn int(5);
       Dcl-S Successful ind;


       //--------------------------------//
       // Procedure: MainLine            //
       //  Main logic for program        //
       //--------------------------------//

       Dcl-Proc MainLine ;

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

         Display_CTL() ;


         Select ;
           When Ctl_Fields.BtnAdd = *On ;
             Successful = Add_Record() ;
             If Not Successful ;
               Display_Msg();
               Close_SQL();
               Close DisplyFile;
               Return ;
             EndIf;

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

         EXEC SQL
           Set Option
             Naming = *Sys,
             Commit = *None,
             UsrPrf = *User,
             DynUsrPrf = *User,
             Datfmt = *iso,
             CloSqlCsr = *EndMod ;

         SearchTerm = '%' + %Trim(Ctl_Fields.Search) + '%' ;

         EXEC SQL DECLARE C1 CURSOR FOR
           SELECT
           OFFICES.OFFICECODE ,
           OFFICES.CITY ,
           OFFICES.PHONE ,
           OFFICES.ADDRESS1 ,
           COALESCE(OFFICES.ADDRESS2, '') ,
           COALESCE(OFFICES.STATE, '') ,
           OFFICES.COUNTRY ,
           OFFICES.POSTALCODE ,
           OFFICES.TERRITORY
           FROM DEMOLIB/OFFICES
           WHERE :SearchTerm = ' '
             OR UPPER(OFFICES.OFFICECODE) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.CITY) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.PHONE) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.ADDRESS1) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.ADDRESS2) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.STATE) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.COUNTRY) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.POSTALCODE) LIKE UPPER(:SearchTerm)
             OR UPPER(OFFICES.TERRITORY) LIKE UPPER(:SearchTerm)
           /* ORDER BY ____ */
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
           , :SearchTerm
           , :SearchTerm
           , :SearchTerm
           , :SearchTerm
           , :SearchTerm
           , :SearchTerm ;

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


       // -- Load All Subfile
       //Clear Ctl_Fields_Out ;
         Ctl_Fields_Out.Clear = *On ;
         Write DisplyFile.Ctl Ctl_Fields_Out ;

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

           Dspf_rrn+=1;
           Clear Sfl_Out ;
           Eval-Corr Sfl_Out = Table_In ;
           Write DisplyFile.SFL Sfl_Out ;

           EXEC SQL
             FETCH C1 INTO :Table_In ;
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
         ReadC DisplyFile.SFL Sfl_In ;
         If %EOF ;
           Return *On;
         EndIf;

         Select ;
           When Sfl_In.ICONDELETE = *On ;
             Successful = Delete_Record(
                            Sfl_In.OFFICECODE
                            );

           When Sfl_In.ICONEDIT = *On ;
             Successful = Show_Detail(
                            Sfl_In.OFFICECODE
                            : 'EDIT' : NotFound) ;

           When Sfl_In.ICONVIEW = *On ;
             Successful = Show_Detail(
                            Sfl_In.OFFICECODE
                            : 'VIEW' : NotFound) ;
         EndSl;

         Return Successful;

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
           KeyOFFICECODE Const
             like(Table_Out.OFFICECODE);
         End-Pi;
         Clear Delete_Fields ;

         Exfmt DisplyFile.Delete Delete_Fields ;

         If Delete_Fields.BTNYES = *On ;
           EXEC SQL
             DELETE FROM DEMOLIB/OFFICES
             WHERE
                OFFICECODE = :KeyOFFICECODE ;

           If SQLCODE < 0;
             Return *Off;
           EndIf;

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
           KeyOFFICECODE Const
             like(Table_Out.OFFICECODE);
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
             OFFICES.OFFICECODE ,
             OFFICES.CITY ,
             OFFICES.PHONE ,
             OFFICES.ADDRESS1 ,
             COALESCE(OFFICES.ADDRESS2, '') ,
             COALESCE(OFFICES.STATE, '') ,
             OFFICES.COUNTRY ,
             OFFICES.POSTALCODE ,
             OFFICES.TERRITORY
           INTO :Table_In
             FROM DEMOLIB/OFFICES
           WHERE
              OFFICES.OFFICECODE = :KeyOFFICECODE ;

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

         // Ensure these indicators do not cause the next comparison to evaluate
         // true when no other value has changed.
         Detail_In.BtnCancel = Detail_Out.BtnCancel;
         Detail_In.BtnSave = Detail_Out.BtnSave;

         If Detail_In <> Detail_Out ;
           Clear Table_Out ;
           Eval-Corr Table_Out = Detail_Out ;

           EXEC SQL
            UPDATE DEMOLIB/OFFICES  SET
               OFFICECODE = :Table_Out.OFFICECODE ,
               CITY = :Table_Out.CITY ,
               PHONE = :Table_Out.PHONE ,
               ADDRESS1 = :Table_Out.ADDRESS1 ,
               ADDRESS2 = :Table_Out.ADDRESS2 ,
               STATE = :Table_Out.STATE ,
               COUNTRY = :Table_Out.COUNTRY ,
               POSTALCODE = :Table_Out.POSTALCODE ,
               TERRITORY = :Table_Out.TERRITORY
            WHERE
               OFFICECODE = :KeyOFFICECODE ;

           If SQLCODE < 0;
              Return *Off;
           EndIf;
         EndIf;
         return *on;
       End-Proc;


       //--------------------------------------//
       // Procedure: Add_Record                //
       //  Procedure to add new record to file //
       //    Parms:                            //
       //      None                            //
       //    Return:                           //
       //      None                            //
       //-------------------------------------//

       Dcl-Proc Add_Record ;
       Dcl-Pi *n ind End-Pi;
         Dcl-S Validated Ind ;

         Clear Detail_Out ;
         Clear Table_Out ;

         Dow Not Validated ;
           Exfmt DisplyFile.Detail Detail_Out ;
           If Detail_Out.BtnCancel = *On ;
             Return *on;
           EndIf;

           Validated = Validate_Detail() ;
         EndDo;
         Eval-Corr Table_Out = Detail_Out ;
         EXEC SQL
           INSERT INTO DEMOLIB/OFFICES (
             OFFICECODE ,
             CITY ,
             PHONE ,
             ADDRESS1 ,
             ADDRESS2 ,
             STATE ,
             COUNTRY ,
             POSTALCODE ,
             TERRITORY
           ) VALUES (
             :Table_Out.OFFICECODE ,
             :Table_Out.CITY ,
             :Table_Out.PHONE ,
             :Table_Out.ADDRESS1 ,
             :Table_Out.ADDRESS2 ,
             :Table_Out.STATE ,
             :Table_Out.COUNTRY ,
             :Table_Out.POSTALCODE ,
             :Table_Out.TERRITORY
           );

         If SQLCODE < 0;
            Return *Off;
         EndIf;
         Return *on;
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