     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTISOCPMT - ISO CMS Import Prompt
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTISOCPMT - ISO CMS Import Prompt                        *
     H*                                                               *
     H*  Created on 08/19/16   by C:309 for Project: 114WYA           *
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h*****************************************************************

     fWTISOCPMT cf   e             workstn extfile('WTISOCPMT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     fssisp     if   e             disk    Rename(pis01:pis0p)
     f                                     usropn
      *
     dpsds           esds                  extname(szq1p)

     d callWTISOCDRV   pr                  extpgm('WTISOCDRV')
     d ##JobQ                        10

     * Clear Errors
     d ClearErrors     pr

     * Refresh screen
     d ClrScreen       pr

    d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     * Get ISO Jobq
     d GetIsoJobq      pr

     * Intialize the Screen fields
     d LoadInitVal     pr

     * Load Data area values to screen
     d LoadScreen      pr

     * Main Processing
     d Main            pr

     * Program Start
     d ProgramStart    pr

     * Call Driver CL program
     d SubmitDriver    pr

     * Validate Screen fields
     d Validate        pr

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr

     * DataArea to store screen fields
     d  IsoCMSPrmp     s            160    dtaara

     * Standalone variables
     d isValid         s               n   Inz(*off)
     d intCo#          s              3  0
     d ##FileDat       s              8  0
     d ##JobQ          s             10    inz
     d outCltID        s              3A
     d datedeflt       s                   like(ScFileDat) inz

     disoDs            ds
     d strFileDat                     8
     d strIfsLoc                           like(ScIfsLoc)
     d strAuth#                       7
     d strFltrElgC                         like(ScFltrElgC)
     d strFltrMisC                         like(ScFltrMisC)
     d strFltrAlrC                         like(ScFltrAlrC)
     d strFltrElgS                         like(ScFltrElgS)
     d strFltrMisS                         like(ScFltrMisS)
     d strFltrAlrS                         like(ScFltrAlrS)
     *
     c     *Entry        Plist
     c                   Parm                    QQprds           40
     c                   Parm                    QQcall            1
      /Free

        // Start Of Program
        ProgramStart();

        // Main Procesing
        Main();

        *InLr = *on;
      /End-Free

       //===================================================================
       // LoadInitVal - Intialize the Screen fields
       //===================================================================
     p LoadInitVal     b
     d LoadInitVal     pi
      /Free
        scFileDat  = datedeflt;
        scIfsLoc   = *Blanks;
        scAuth#    = *Zeros ;
        scFltrElgC = *Blanks;
        scFltrMisC = *Blanks;
        scFltrAlrC = *Blanks;
        scFltrElgS = *Blanks;
        scFltrMisS = *Blanks;
        scFltrAlrS = *Blanks;
        isSpeclty  = *Off;
        erFileDat  = *Off;
        msFileDat  = *Blanks;
        msIfsLoc   = *Blanks;
        erIfsLoc   = *Off;
        msAuth#    = *Blanks;
        erAuth#    = *Off;

        // Load Data area values to screen
        loadScreen();
      /End-free
     p LoadInitVal     e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free

        // Get ISO Job Queue
        GetIsoJobq();

        // Load initial values
        LoadInitVal();

        // Retrieve Client Id
        GetClientID(outCltID);

        If outCltID= '114';
           isSpeclty = *On;
        Endif;

        // Load Action panel with the Company Name.
        intCo#     = userCo#(q1user);
        scCompName = GetCompNam(intCo#);
      /End-Free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /Free

        // Process until cancel button is pressed.
        Dou BtnCancel  = *On ;
           // Display Screen
           Exsr ExfmtScreens;

           Select;
           When BtnSubmit = *on;
              BtnSubmit = *off;
              ClearErrors();
              Validate();
              If isValid;
                 strFileDat  = %Char(%Dec(%Char(ScFileDat:*ISO0):8:0));

                 //##FileDat   = %Dec(%Char(ScFileDat:*ISO0):8:0);
                 //strFileDat  = %Char(##FileDat)                ;
                 strIfsLoc   = %Trim(ScIfsLoc)                 ;
                 strAuth#    = %Editc(ScAuth#:'X')             ;
                 strFltrElgC = scFltrElgC                      ;
                 strFltrMisC = scFltrMisC                      ;
                 strFltrAlrC = scFltrAlrC                      ;
                 strFltrElgS = scFltrElgS                      ;
                 strFltrMisS = scFltrMisS                      ;
                 strFltrAlrS = scFltrAlrS                      ;
                 IsoCMSPrmp  = isoDS                           ;
                 out *lock IsoCMSPrmp                          ;
                 Unlock IsoCMSPrmp                             ;
                 SubmitDriver();
                 leave;
              Endif;

           When BtnRefresh = *on;
              BtnRefresh= *off;
              ClrScreen();
           Other;
              // Do Nothing
           Endsl;
        Enddo;

        BegSr ExfmtScreens;

           Select;
           When TabIndex = 0;
              TabHeight = 1050;
              APnlHeight = TabHeight + 50;

              Write rcdComPnl;
              Write RcdTabISOC;

              Exfmt RcdISOCMUP;

              Read rcdComPnl;
              Read RcdTabISOC;
           EndSl;
        Endsr;
      /End-free
     p Main            e

       //===================================================================
       // Submit Driver Program
       //===================================================================
     p SubmitDriver    b
     d SubmitDriver    pi
      /Free

        CallWTISOCDRV(##Jobq);

        BtnCancel = *on;
      /End-free
     p SubmitDriver    e

       //===================================================================
       // Validate screen fields
       //===================================================================
     p Validate        b
     d Validate        pi
      /Free
        isValid=*On;

       // Validate File Date entered
        // Check for Blank date
        If scFileDat= datedeflt;
           erFileDat=  *On;
           msFileDat= 'WCR0503';
           isValid  = *Off;
        Endif;

       // Validate IFS location
        If ScIFSLoc = *Blanks;
           erIFSLoc =  *On;
           msIFSLoc = 'WCR0503';
           isValid  = *Off;
        Endif;

       // Validate Authorization Number
        If ScAuth#  = *Zeros;
           erAuth#  =  *On;
           msAuth#  = 'WCR0503';
           isValid  = *Off;
        Endif;
      /End-free
     p Validate        e

       //===================================================================
       // Clear Screen
       //===================================================================
     p ClrScreen       b
     d ClrScreen       pi
      /Free
        scFileDat  = datedeflt;
        scIfsLoc   = *Blanks;
        scAuth#    = *Zeros ;
        scFltrElgC = *Blanks;
        scFltrMisC = *Blanks;
        scFltrAlrC = *Blanks;
        scFltrElgS = *Blanks;
        scFltrMisS = *Blanks;
        scFltrAlrS = *Blanks;
        erFileDat  = *Off;
        msFileDat  = *Blanks;
        msIfsLoc   = *Blanks;
        erIfsLoc   = *Off;
        msAuth#    = *Blanks;
        erAuth#    = *Off;

        // Load Data area values to screen
        loadScreen();

      /End-free
     p ClrScreen       e

       //===================================================================
       // Clear Errors
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi
      /Free
        erFileDat = *Off;
        msFileDat = *Blanks;
        msIfsLoc  = *Blanks;
        erIfsLoc  = *Off;
        msAuth#   = *Blanks;
        erAuth#   = *Off;
      /End-free
     p ClearErrors     e

       //*********************************************************
       //Get ISO JobQ
       //*********************************************************
     p GetIsoJobq      b
     d GetIsoJobq      pi
      /Free
         If Not %Open(SSISP);
            Open ssisp;
         Endif;
         Read pis0p;
         If Not %Eof(ssisp);
            ##Jobq = ISjobq;
         EndIf;
         Close ssisp;
      /End-Free
     p GetIsoJobq      e

       //*********************************************************
       //Load Screen fields from Data Area
       //*********************************************************
     p LoadScreen      b
     d LoadScreen      pi
      /Free

        in *lock *dtaara;
        isods   = IsoCMSPrmp;
        ScIfsLoc   = %Trim(strIfsLoc)    ;
        Monitor;
           scFileDat  = %Date(strFileDat:*ISO0);
        On-Error;
           scFileDat  = datedeflt;
        EndMon;

        Monitor;
           ScAuth#    = %Dec(strAuth#:7:0)     ;
        On-Error;
           ScAuth#    = *Zeros;
        EndMon;
        scFltrElgC = strFltrElgC  ;
        scFltrMisC = strFltrMisC  ;
        scFltrAlrC = strFltrAlrC  ;
        scFltrElgS = strFltrElgS  ;
        scFltrMisS = strFltrMisS  ;
        scFltrAlrS = strFltrAlrS  ;
      /End-Free
     p LoadScreen      e

