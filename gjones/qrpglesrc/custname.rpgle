      // @MBRTEXT: get customer name using PJS module
           Dcl-PR PJSCALL ExtPgm;

             ParmType        Char(30)          Const;
             ModuleID        VarUCS2(500)      Const;
             RoutineName     VarUCS2(70)       Const;
           InputParms      Char(16773104)    Const
                                             Options(*Varsize:*Omit:*NoPass);
           InputParmSize   Int(10)           Const
                                             Options(*Omit:*NoPass);
           OutputParms     Char(16773104)    Options(*Varsize:*Omit:*NoPass);
           OutputParmSize  Int(10)           Const Options(*Omit:*NoPass);
           End-PR;

           Dcl-DS InputDS Qualified Inz;
             custNum  Zoned(10: 0);
           End-DS;

           Dcl-DS OutputDS Qualified Inz;
             FirstName  VarChar(20);
             LastName   VarChar(20);
           End-DS;

        dcl-s dummy char(1);
        InputDS.custNum = 9001;


           Monitor;

             PJSCALL('*MODULE'
               : 'api-training:CustName.module.json'
               : 'GetCustName'
               : InputDS
               : %Size(InputDS)
               : OutputDS
               : %Size(OutputDS)
             );



           On-Error;
                  dsply 'error encountered' ' ' dummy;
           EndMon;

           dsply OutputDS.FirstName ' ' dummy;
           dsply OutputDS.LastName ' ' dummy;

           return;
           *inlr =  *on;