     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*CALLER)
     H/Define Convert
     H/Title WTRPTPRMSR - Premium due on Self-Reporting Employers
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  Created on 06/08/16   by C:309 for Project: WC2422           *
     H*  (Rewrite W7280R)                                             *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS                                                     *
     H*                                                               *
     H* 12/05/16 C:136 Project: SY371                                 *
     H*                Add output control window for Scriptura        *
     H*                                                               *
     H* 10/28/16 Support:43939 Fixed Heading Font size and color      *
     H*                        (Screen changes only)                  *
     H*
     H*****************************************************************
     fWTRPTPRMSRCF   e             workstn extfile('WTRPTPRMSR')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
      *** beg add ***** 12/05/16 ******************
     d/copy sPrLetter
     d/copy sPrXML
     d isUseScript     S              1n   inz
     d processType     c                   const('Premium Due Report - Self +
     d                                     Reporting')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     d svLDA           ds          1024    dtaara(*lda)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      *** end add ***** 12/05/16 ******************

     d/copy scopypr01d
     d/copy sPrCompany
     d/copy wPrUsrprms
     d/copy sPrDate

      *
     d ##Outq          s             10
     d ##Psrt          s              2
     d ##Ssrt          s              2
     d ##Sel1          s              2
     d ##S1Fr          s             15
     d ##S1To          s             15
     d ##Sel2          s              2
     d ##S2Fr          s             15
     d ##S2To          s             15
     d ##Stdt          s              8A
     d ##Endt          s              8A
     d ##Rpmy          s              6A
     d strOutq         s                   Like(Upoutq)
      *

     d CallW7280C      pr                  ExtPgm('W7280C')
     d ##Co#                          3    Const
     d ##Fnd                          3    Const
     d ##Stdt                         8
     d ##Endt                         8
     d ##Rndt                         8    Const
     d ##Rdst                         1    Const
     d ##Type                         1    Const
     d ##Stat                         1    Const
     d ##Rpmy                         6
     d ##Outq                        10
     d ##Psrt                         2
     d ##Ssrt                         2
     d ##Empf                         1    Const
     d ##Ampf                         1    Const
     d ##Prty                         2    Const
     d ##Emp#                         9    Const
     d ##Div                          5    Const
     d ##Fyr                          3    Const
     d ##Sel1                         2
     d ##S1Fr                        15
     d ##S1To                        15
     d ##Sel2                         2
     d ##S2Fr                        15
     d ##S2To                        15

     * Clear Errors
     d ClearError      pr

     * Display screen
     d ExfmtScreens    pr

     * Initialize
     d Initialize      pr

     * Main Process
     d MainProcess     pr

     * Program Start
     d ProgramStart    pr

     * Set Criteria
     d SetCriteria     pr

     * Submit Driver cl program
     d SubmitDrvCL     pr

     * Validations
     d Validate        pr              n

     * Entry parameter
     d WTRPTPRMSRp     pr                  Extpgm('WTRPTPRMSR')
     d  @@PRDS                       40
     d  @@CALL                        1

     d WTRPTPRMSRp     pi
     d  @@PRDS                       40
     d  @@CALL                        1

      *
      /Free

         ProgramStart();

         MainProcess();

         *InLr = *on;
      /End-Free

      //===================================================================
      // ClearError - Clear Error/Message fields
      //===================================================================
     p ClearError      b
     d ClearError      pi
      /Free

        erRptMmYy  = *Off    ;
        erPrimSort = *Off    ;
        erSecnSort = *Off    ;
        erFrmEmpN  = *Off    ;
        erToEmpN   = *Off    ;
        erFrmEmp#  = *Off    ;
        erToEmp#   = *Off    ;
        erFrmAgnN  = *Off    ;
        erToAgnN   = *Off    ;
        erFrmAgnt# = *Off    ;
        erToAgnt#  = *Off    ;
        erGrpNbr   = *Off    ;
        msRptMmYy  = *Blanks ;
        msPrimSort = *Blanks ;
        msSecnSort = *Blanks ;
        msFrmEmp#  = *Blanks ;
        msToEmp#   = *Blanks ;
        msToEmpN   = *Blanks ;
        msFrmEmpN  = *Blanks ;
        msFrmAgnt# = *Blanks ;
        msToAgnt#  = *Blanks ;
        msToAgnN   = *Blanks ;
        msFrmAgnN  = *Blanks ;
        msGrpNbr   = *Blanks ;

      /End-Free
     p ClearError      e

      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     P ExfmtScreens    b
     D ExfmtScreens    pi
      /Free
         Select;
         When TabIndex = 0;
            TabHeight = 770;
            APnlHeight = TabHeight + 60;

            Write rcdComPnl;
            Write RcdTabPrmD;

            Exfmt RcdPrmDue;

            Read rcdComPnl;
            Read RcdTabPrmD;
         EndSl;
      /End-Free
     P ExfmtScreens    e

      //===================================================================
      // Initialize - Initialization procedure
      //===================================================================
     P Initialize      b
     D Initialize      pi
      /Free

        scRptMMYY  = *Zeros  ;
        scFrmEmpNm = *Blanks ;
        scToEmpNm  = *Blanks ;
        scToEmp#   = *Zeros  ;
        scFrmEmp#  = *Zeros  ;
        scFrmAgnNm = *Blanks ;
        scToAgnNm  = *Blanks ;
        scFrmAgnt# = *Zeros  ;
        scToAgnt#  = *Zeros  ;
        scPrimSort = *Blanks ;
        scSecnSort = *Blanks ;

      /End-Free
     p Initialize      e

      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi
      /Free

        SetCriteria();
       // Process until exit button is pressed.
        Dou ExitNow = *On ;

           ExfmtScreens();

           Select;
           When BtnSubmit = *On;
              BtnSubmit = *Off;

       //*** beg add ***** 12/05/16 *****************
         //* Set up *LDA with Scriptura Info
              If isUseScript;
                 xxSubmit = *on;
                 xxProcType = processType;
                 In *Lock *Dtaara;
                 Clear svLDA;
                 xxDart = 'N';
                 xxRMCD = ' ';
                 Out *Dtaara;
                 xxSubmit = OutputWindow(xxProcType:xxMass);
                 If xxSubmit = *off;
                    Iter;
                 Endif;
              Endif;
       //*** end add ***** 12/05/16 *****************

             // Clear Errors
              ClearError();

             // Validate and Submit Report Program
              If Validate();
                 SubmitDrvCL();
                 Leave;
              EndIf;

           When BtnCancel = *On;
              BtnCancel = *Off;
              Leave;

           When BtnRefresh= *On;
              BtnRefresh= *Off;
              Initialize();
              ClearError();
              SetCriteria();
           EndSl;
        EndDo;
      /End-Free
     P MainProcess     e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     P ProgramStart    b
     D ProgramStart    pi
      /Free

        Initialize();
        scCompName = GetCompNam(userCo#(q1user));

       // Retrieve Output Queue of the User.
        Chain q1user pup00;
        If %Found(wmupl);
           strOutq = UPoutq;
        EndIf;

       //*** beg add ***** 12/05/16 *****************
       isUseScript = UseScriptura(processType);
       //*** end add ***** 12/05/16 *****************

      /End-Free
     p ProgramStart    e

      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi
      /Free

         GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
         scGrpDsr = UPfnd;

      /End-Free
     p SetCriteria     e

      //===================================================================
      // SubmitDrvCL - Submit W7280C Premium Due Submit Program
      //===================================================================
     P SubmitDrvCL     b
     D SubmitDrvCL     pi
      /Free

        ##RpMY = %EditC(scRptMMYY:'X')  ;
        ##OutQ = strOutq                ;
        ##Psrt = scPrimSort             ;
        ##Ssrt = scSecnSort             ;

        // Prepare Start Date
        ##stDt = %EditC(scRptMMYY:'X')  ;
        ##stDt = %Subst(##stDt:1:2) + '01' + %Subst(##stDt:3:4);

        // Prepare End Date
        ##Endt = %Editc(DateTo8(GetLastDay(cvttodate(%Dec(##stDt:8:0)))):'X');

        If scToAgnt# <> *Zeros;
           ##Sel1 = 'AN';
           ##S1Fr = %Char(scFrmAgnt#);
           ##S1To = %Char(scToAgnt#);
        Else;
           If scToAgnNm <>*Blanks;
              ##Sel1 = 'AA';
              ##S1Fr = scFrmAgnNm;
              ##S1To = scToAgnNm;
           Endif;
        Endif;

        If scToEmp# <> *Zeros;
           If scToAgnt# <> *Zeros or
            scToAgnNm <> *Blanks;
              ##Sel2 = 'EN';
              ##S2Fr = '00000' + %Char(scFrmEmp#);
              ##S2To = '99999' + %Char(scToEmp#);
           Else;
              ##Sel1 = 'EN';
              ##S1Fr = '00000' + %Char(scFrmEmp#);
              ##S1To = '99999' + %Char(scToEmp#);
           Endif;
        Endif;

        If scFrmEmpNm <> *Blanks;
           If scToAgnt# <> *Zeros or
            scToAgnNm <> *Blanks;
              ##Sel2 = 'EA';
              ##S2Fr = scFrmEmpNm;
              ##S2To = scToEmpNm;
           Else;
              ##Sel1 = 'EA';
              ##S1Fr = scFrmEmpNm;
              ##S1To = scToEmpNm;
           Endif;
        Endif;


        CallW7280C(%Char(userCo#(q1user)):%Char(scGrpDsr):##Stdt:##Endt:
         %Char(%Date():*USA0):'B':'S':'B':##Rpmy:##Outq:##Psrt:##Ssrt:'M':
         'M':*Blanks:*Blanks:*Blanks:*Blanks:##Sel1:##S1Fr:##S1To:##Sel2:
         ##S2Fr:##S2To);

      /End-Free
     P SubmitDrvCL     e

      //===================================================================
      // Validate - Validate screen fields
      //===================================================================
     p Validate        b
     d Validate        pi              n
      /Free

       // Validate if any Group is selected
        If scGrpDsr = *zeros;
           erGrpNbr =  *On;
           msGrpNbr = 'WCR0503';
           Return *Off;
        Endif;

       // Validate if Reporting Month year is not left blank
        If scRptMMYY = *Zeros;
           erRptMmYy  = *On;
           msRptMmYy  = 'WCR0503';
           Return *Off;
        Endif;

       // Validate Reporting Month year
        If dt6Check(%Dec(scRptMMYY:6:0)) = *Zeros;
           erRptMmYy  = *On;
           msRptMmYy  = 'WCR0500';
           Return *Off;
        Endif;

       // Validate "Primary Sort" field
        If scPrimSort = *Blanks;
           erPrimSort  = *On;
           msPrimSort  = 'WCR0503';
           Return *Off;
        Endif;

        If scPrimSort  <> *Blanks;
           If scPrimSort <> 'EN' and
            scPrimSort <> 'EA' and
            scPrimSort <> 'AN' and
            scPrimSort <> 'AA' ;

              erPrimSort = *On;
              msPrimSort = 'WCR0503';
              Return *Off;
           Endif;
        Endif;

       // Validate "Secondary Sort" field
        If scSecnSort <> *Blanks;
           If scSecnSort <> 'EN' and
            scSecnSort <> 'EA' ;

              erSecnSort = *On;
              msSecnSort = 'WCR0503';
              Return *Off;
           Endif;
        Endif;

       // Both From Employer# and To Employer# not enterted
        If scFrmEmp#  = *Zeros and scToEmp# <> *Zeros;
           erFrmEmp# = *On;
           msFrmEmp# = 'WCR0503';
           Return *Off;
        EndIf;

        If scFrmEmp# <> *Zeros and scToEmp# = *Zeros;
           erToEmp#  = *On;
           msToEmp#  = 'WCR0503';
           Return *Off;
        EndIf;

       // From Employer# should be less than To Employer#
        If scFrmEmp# >  scToEmp#;
           erFrmEmp# = *On;
           msFrmEmp# = 'WCR0750' ;
           Return *Off;
        Endif;

       // Both From Name and To Name not enterted
        If scFrmEmpNm = *Blanks and scToEmpNm <> *Blanks;
           erFrmEmpN = *On;
           msFrmEmpN = 'WCR0503';
           Return *Off;
        EndIf;

        If scFrmEmpNm <> *Blanks and scToEmpNm = *Blanks;
           erToEmpN  = *On;
           msToEmpN  = 'WCR0503';
           Return *Off;
        EndIf;

       // From Employer Name should be less than To Employer Name
        If scFrmEmpNm > scToEmpNm;
           erFrmEmpN = *On;
           msFrmEmpN = 'WCR0750' ;
           Return *Off;
        Endif;

       // Only one of Employer Number or Name Range can
       // be chosen as they are Mutually Exclusive
        If (scFrmEmp#  <> *Zeros  or  scToEmp#  <> *Zeros) and
         (scFrmEmpNm <> *Blanks or  scToEmpNm <> *Blanks);

           msFrmEmp#  = 'WCR0918';
           msFrmEmpN  = 'WCR0918';
           msToEmp#   = 'WCR0918';
           msToEmpN   = 'WCR0918';
           erFrmEmp#  = *On;
           erFrmEmpN  = *On;
           erToEmp#   = *On;
           erToEmpN   = *On;
           Return *Off;
        Endif;

       // Both From Agent# and To Agent# not enterted
        If scFrmAgnt# = *Zeros and scToAgnt# <> *Zeros;
           erFrmAgnt# = *On;
           msFrmAgnt# = 'WCR0503';
           Return *Off;
        EndIf;

        If scFrmAgnt#<> *Zeros and scToAgnt# =  *Zeros;
           erToAgnt# = *On;
           msToAgnt# = 'WCR0503';
           Return *Off;
        EndIf;

       // Both From Agent Name and To Agent Name not enterted
        If scFrmAgnNm = *Blanks and scToAgnNm <> *Blanks;
           erFrmAgnN = *On;
           msFrmAgnN = 'WCR0503';
           Return *Off;
        EndIf;

        If scFrmAgnNm <> *Blanks and scToAgnNm = *Blanks;
           erToAgnN  = *On;
           msToAgnN  = 'WCR0503';
           Return *Off;
        EndIf;

       // From Agent# should be less than To Agent#
        If scFrmAgnt# >  scToAgnt#;
           erFrmAgnt# = *On;
           msFrmAgnt# = 'WCR0750' ;
           Return *Off;
        Endif;

       // From Agent Name should be less than To Agent Name
        If scFrmAgnNm > scToAgnNm ;
           erFrmAgnN = *On;
           msFrmAgnN = 'WCR0750' ;
           Return *Off;
        Endif;

       // Only one of Agent Number or Name Range can
       // be chosen as they are Mutually Exclusive
        If (scFrmAgnt# <> *Zeros  or  scToAgnt# <> *Zeros) and
         (scFrmAgnNm <> *Blanks or  scToAgnNm <> *Blanks);
           msFrmAgnN  = 'WCR0918';
           msToAgnN   = 'WCR0918';
           msFrmAgnt# = 'WCR0918';
           msToAgnt#  = 'WCR0918';
           erFrmAgnN  = *On;
           erToAgnN   = *On;
           erToAgnt#  = *On;
           erFrmAgnt# = *On;
           Return *Off;
        Endif;

        Return *On;
      /End-Free
     P Validate        e
