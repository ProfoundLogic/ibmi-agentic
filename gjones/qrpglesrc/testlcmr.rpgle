      // @MBRTEXT: test low code module
       Dcl-PR PJSCALL ExtPgm;
         ParmType        Char(30)          Const;
         ModuleID        VarUCS2(500)      Const;
         RoutineName     VarUCS2(70)       Const;
         InputParms      Char(16773104) Const Options(*Varsize:*Omit:*NoPass);
         InputParmSize   Int(10)   Const Options(*Omit:*NoPass);
         OutputParms     Char(16773104)    Options(*Varsize:*Omit:*NoPass);
         OutputParmSize  Int(10)           Const Options(*Omit:*NoPass);
       End-PR;

       Dcl-DS InputDS Qualified Inz;
         customerID  Zoned(7: 0);
       End-DS;

       Dcl-DS OutputDS Qualified Inz;
         firstname  VarChar(20);
         lastname   VarChar(20);
       End-DS;

       dcl-s dummy char(1);

       Monitor;

          inputds.customerID = 9004;



         PJSCALL('*MODULE'
           : 'pjstrain\modtrain.module.json'
           : 'get customer name'
           : InputDS
           : %Size(InputDS)
           : OutputDS
           : %Size(OutputDS)
         );

         dsply outputds.firstname '' dummy;
         dsply outputds.lastname '' dummy;




       On-Error;

       EndMon;

       *inlr = *on;
       return;