     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WPICLMBP - Body Part Codes
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Process Body Part Codes.
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created on 5/01/2018 by C:053 for Project: WC1917
     h**********************************************************************
     h*  Revisions:
     h*
     h*  03/25/22 C:333 Project: SY434
     h*                 - Update Grids for Sorting Filtering Upgrade
     h*
     h*  00/00/00 C:000 Project: x
     h*
     h*
     h**********************************************************************
     fwpiclmbp  cf   e             workstn extfile('WPICLMBP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPartVal:rrnParcod)
     f***********                          Sfile(sflPayValE:rrnParcod)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     f*** end chg *****  03/06/15  *****************************************

     fwtabl     uf a e           k disk    rename(ppa01:pb200)
     f                                     InfDs(dbfds1)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fssylf     if   e           K disk
     f                                     IGNORE(SSY01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)

     d psds          esds                  extname(szq1p)
      * constant
     d intFlgDim       c                   10

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d dsPFilter       ds                  likerec(RcdPartFlt)

     d intCount        s              3  0
     d @@mode          s              1
     d @@TabIndex      s              5
     d inmode          s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d ##rrn           s              9  0
     d clientID        s              3    inz('000')
     d meetPay         s              1
     d*strcode         s                   like(scFilBPcd)
     d ##desc          s            100
     d ##pdsc          s            100
     d ##start         s              3  0 inz(1)
     d ##strlen        s              3  0 inz(100)
     d ##scnres        s              3  0
     d ##patlen        s              3  0 inz(40)
     d ##trans         s              1    inz('0')
     d ##trim          s              1    inz('1')
     d ##wild          s              1    inz(' ')


     d* Clear Subfiles
     d ClrSubFiles     pr

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Clear Filter - Visibility of button for Payment Value 1 Grid
     d LoadClrFlt      pr

     d* Load Body Part grid
     d LoadBodyPart    pr

     d* Main Processing
     d Main            pr

     d* Filter for Payment Value
     d MeetFlt         pr

     d* Program Start
     d ProgramStart    pr

     d* Process other options
     d ProcOther       pr

     d* Retrieve system values
     d Rtvsysval       pr                  ExtPgm('SA001R')
     d  ##Wcif                        1
     d  ##Mcif                        1
     d  ##Wcin                        1
     d  ##Ar                          1
     d  ##Ntwk                        1
     d  ##Ur                          1
     d  ##Med                         1
     d  ##Edi                         1
     d  ##Wcdk                        1
     d  ##Rhab                        1
     d  ##Mss                         1
     d  ##Fsch                        1
     d  ##Prvb                        1
     d  ##Npwc                        1
     d  ##Os                          1
     d  ##Arls                        1
     d  ##Ccod                        1
     d  ##Crvw                        1
     d  ##Cchk                        1
     d  ##Prom                        1
     d  ##Mcrv                        1
     d  ##Freq                        1
     d  ##Crac                        1
     d  ##Preo                        1
     d  ##Parn                        1
     d  ##Mgri                        1
     d  ##Kcif                        1
     d  ##Curs                        1
     d  ##Pcib                        1
     d  ##Clt#                        3
     d  ##Ucpf                        1
     d  ##Dcf                         1
     d  ##Deif                        1
     d  ##Aure                        1
     d  ##Cred                        1
     d  ##Idrt                        1
     d  ##Cprv                        1
     d  ##Eft                         1
     d  ##Govr                        1
     d  ##Ncdc                        1
     d  ##Splt                        1
     d  ##Last                        1

     d  ##Wcif         s              1
     d  ##Mcif         s              1
     d  ##Wcin         s              1
     d  ##Ar           s              1
     d  ##Ntwk         s              1
     d  ##Ur           s              1
     d  ##Med          s              1
     d  ##Edi          s              1
     d  ##Wcdk         s              1
     d  ##Rhab         s              1
     d  ##Mss          s              1
     d  ##Fsch         s              1
     d  ##Prvb         s              1
     d  ##Npwc         s              1
     d  ##Os           s              1
     d  ##Arls         s              1
     d  ##Ccod         s              1
     d  ##Crvw         s              1
     d  ##Cchk         s              1
     d  ##Prom         s              1
     d  ##Mcrv         s              1
     d  ##Freq         s              1
     d  ##Crac         s              1
     d  ##Preo         s              1
     d  ##Parn         s              1
     d  ##Mgri         s              1
     d  ##Kcif         s              1
     d  ##Curs         s              1
     d  ##Pcib         s              1
     d  ##Clt#         s              3
     d  ##Ucpf         s              1
     d  ##Dcf          s              1
     d  ##Deif         s              1
     d  ##Aure         s              1
     d  ##Cred         s              1
     d  ##Idrt         s              1
     d  ##Cprv         s              1
     d  ##Eft          s              1
     d  ##Govr         s              1
     d  ##Ncdc         s              1
     d  ##Splt         s              1
     d  ##Last         s              1

     d  ##CltId        s              3

     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany

     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrUsrPrms
     d/copy wTclmTblPr
    
     c* Entry of Program.
     c     *entry        plist
     c                   parm                    @@Mode
     c                   parm                    @@TabIndex

      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inlr = *on;
       return;
      /end-free
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Program Mode.
       If %parms() >= 1;
          inmode = @@Mode;
       Endif;

       Select;
       When inmode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When inmode = 'A';
          dspVisb = *On;
       When inmode = 'C';
          dspVisb = *On;
       Endsl;

       // Tab Index Mode.
       If %parms() >= 2;
          intabIndex = %dec(@@TabIndex:5:0);
       Else;
          intabIndex = 0;
       EndIf;

       // Load Action panel with the Company Name.
       // Get company name
       clear scCompName;
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;

       // Get the client id
       GetClientID(##CltId);

       clear dspFilter;
       tabClick = *Off;
       ##rrn = 0;
       Return;

      /end-free
     p ProgramStart    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Load Filter Clear Button.
          LoadClrFlt();

          // Clear Sub-files
          ClrSubfiles();

          // Load Body Part code grid
          LoadBodyPart();

          // Process Payment Value 1 code Screens
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // Filter Processing.
          When BtnFilter = *on;
             Dou BtnFilCan  = *on or BtnFilApp  = *on;

                ExFmt RcdPartFlt;
                If BtnFilApp = *on;
                   dspFilter.scfilBPCod = scfilBPCod;
                   dspFilter.scFilBPDsc = scFilBPDsc;
                   dspFilter.scFilSts  = scFilSts;
                Endif;

                If BtnFilCan  = *on;
                   scfilBPCod = dspFilter.scFilBPCod;
                   scFilBPDsc = dspFilter.scFilBPDsc;
                   scFilSts   = dspFilter.scFilSts;
                Endif;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             clear dspFilter;
             clear rcdPartFlt;
             dspClrFltr = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Body Part
          When scPartAdd = *on;
             EditAddBP();

          Other;

             ProcOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 550;
          Write rcdComPnl;
          Write rcdTabPart;
          Exfmt rcdPart;
          Read rcdComPnl;
          Read rcdTabPart;
       EndSl;

       Endsr;

      /end-free
     p Main            e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnParCod  = 0;
       clrParCod  = *On;
       dspParSub  = *Off;
       dspParCtl  = *Off;
        Write rcdParT;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       // Clear Filter - Visibility of button for Payment Value 1 Grid
       //===================================================================
     p LoadClrFlt      b
     d LoadClrFlt      pi

      /free

       // display clear filter button
       If dspFilter.scFilBPDsc = *blanks and
          dspFilter.scFilSts  = *blanks and dspFilter.scfilBPDsc=*blanks;
          dspClrFltr = *off;
       Else;
          dspClrFltr = *on;
       Endif;

      /end-free
     p LoadClrFlt      e
       //===================================================================
       // Load Payment Value 1 code grid
       //===================================================================
     p LoadBodyPart    b
     d LoadBodyPart    pi

     d strTbid         s                   like (GTtbid)

      /free
       // Is EDI module and EFT is installed
        ##Wcif = *Blanks;
        ##Mcif = *Blanks;
        ##Wcin = *Blanks;
        ##Ar   = *Blanks;
        ##Ntwk = *Blanks;
        ##Ur   = *Blanks;
        ##Med  = *Blanks;
        ##Edi  = *Blanks;
        ##Wcdk = *Blanks;
        ##Rhab = *Blanks;
        ##Mss  = *Blanks;
        ##Fsch = *Blanks;
        ##Prvb = *Blanks;
        ##Npwc = *Blanks;
        ##Os   = *Blanks;
        ##Arls = *Blanks;
        ##Ccod = *Blanks;
        ##Crvw = *Blanks;
        ##Cchk = *Blanks;
        ##Prom = *Blanks;
        ##Mcrv = *Blanks;
        ##Freq = *Blanks;
        ##Crac = *Blanks;
        ##Preo = *Blanks;
        ##Parn = *Blanks;
        ##Mgri = *Blanks;
        ##Kcif = *Blanks;
        ##Curs = *Blanks;
        ##Pcib = *Blanks;
        ##Clt# = *Blanks;
        ##Ucpf = *Blanks;
        ##Dcf  = *Blanks;
        ##Deif = *Blanks;
        ##Aure = *Blanks;
        ##Cred = *Blanks;
        ##Idrt = *Blanks;
        ##Cprv = *Blanks;
        ##Eft  = *Blanks;
        ##Govr = *Blanks;
        ##Ncdc = *Blanks;
        ##Splt = *Blanks;
        ##Last = *Blanks;

        RtvSysVal(##Wcif:##Mcif:##Wcin:##Ar:##Ntwk:##Ur:##Med:##Edi:
         ##Wcdk:##Rhab:##Mss:##Fsch:##Prvb:##Npwc:##Os:##Arls:
         ##Ccod:##Crvw:##Cchk:##Prom:##Mcrv:##Freq:##Crac:##Preo:
         ##Parn:##Mgri:##Kcif:##Curs:##Pcib:##Clt#:##Ucpf:##Dcf:
         ##Deif:##Aure:##Cred:##Idrt:##Cprv:##Eft:##Govr:##Ncdc:
         ##Splt:##Last);


       // clear the subfile.
       clear sflPartVal;
       clrParCod = *Off;
       dspParSub = *On;
       dspParCtl = *On;
       clear dbRrn1;

       Setll intCo# pb200;
       Reade intCo# pb200;
       dow not %eof(wtabl);

          meetPay = *on;

          // Validate for filter
          MeetFlt();
          If meetPay = *on;

             // Write Body Part Codes

             // Code
             scParCode =%trim(PAkey);

             // Description
             scParDesp =%trim(PAdesc);

             // Placement
          // scParExt = PAParx;  ***Remove****

             // EDI Code
             scParEDI = PAedip;

             // Status
             If PAstat = 'A';
                scParStat = 'Active';
             Else;
                scParStat = 'Inactive';
             Endif;

             scParRRN# = dbRrn1;

             rrnParCod = rrnParCod + 1;
             If rrnParCod = 1;
                scParRowSl = 'A';
             Else;
                clear scParRowSl;
             Endif;

             If inmode = 'D';
                dspVisb = *Off;
             Else;
                dspVisb = *On;
             Endif;

             // Is EFT module is installed
          // If ##Eft = 'Y';
          //    isEft =*on;
          // Else;
          //    isEft  = *off;
          // Endif;

             // Is EDI module is installed
             //****** If ##Edi  = 'Y';
          // Chain 'WC' Swc01;
          // If %found and WCfl09 = 'Y';
          //    isPayEdi  = *on;
          //    isEdiGrid = *off;
          // Else;
          //    isPayEdi  = *off;
          //    isEdiGrid = *on;
          // Endif;

          // Write sflPayValE;
             Write sflPartVal;
          Endif;
          Reade intCo# pb200;
       Enddo;

       rrnParCod  = 1;

      /end-free
     p LoadBodyPart    e
       //==================================================================
       // MeetFlt - Determine if the records meet the Filter criteria
       //==================================================================
     p MeetFlt         b
     d MeetFlt         pi
     d intIncr         s              3  0
     d meetCode        s              1n
      /free


       If meetPay = *on;
          // Check for Body Part code
          If scFilBPcod  <> ' ' and
             not StringFound(%trim(PAkey):%trim(scFilBPcod):'1');
             meetPay = *off;
          Endif;
       Endif;

       If meetPay = *on;
          // Check for status
          If scFilSts  <> ' ' and scFilSts <> PAstat;
             meetPay = *off;
          Endif;
       Endif;

       If meetPay = *on;
          // Check for Body Part description
          If scFilBPdsc  <> ' ' and
             not StringFound(%trim(PAdesc):%trim(scFilBPdsc):'1');
             meetPay = *off;
          Endif;
       Endif;

      /end-free
     p MeetFlt         e
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
      /free
       // Edit Payment value 1 codes
       //If isEdiGrid = *off ;
          Readc sflPartVal;
       //Else;
       //   Readc sflPartValE;
       //Endif;

       Dow not %eof;
          If scPayEdt = *on;
             scPayEdt = *off;
             ##rrn = scParRRN#;
             EditAddBP(##rrn);
          Endif;

        //If isEdiGrid = *off ;
             Readc sflPartVal;
        //Else;
        //   Readc sflPartValE;
        //Endif;
       Enddo;

      /end-free
     p ProcOther       E
