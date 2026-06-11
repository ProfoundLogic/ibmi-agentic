     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WPICLMPV - Payment Value 1 Codes
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Process Payment Value 1 Codes.
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created on 8/04/2014 by C:295 for Project: WC2247
     h**********************************************************************
     h*  Revisions:
     h*
     h*  03/02/23 C:340 Project: WC2430
     h*                 PV1 screen fixes : Convert CheckBoxes
     h*
     h*  03/25/22 C:333 Project: SY434
     h*                 - Update Grids for Sorting Filtering Upgrade
     h*
     h*  12/06/19 C:079 Project: 129WVK - Compile only
     h*                 Add Vocational Rehab Paid Under Comp field
     h*
     h*  08/19/19 C:079 Project: WC2604
     h*                 Add IDC Benefit Type Code
     h*
     h*  08/31/18 C:079 Support: 146/72945
     h*                 Add Order by property on drop down fields
     h*
     h*  03/23/16 C:308 Project: ELEVATE
     h*                 Recompile.
     h*
     h*  01/07/16 C:65  Project: WC2401
     h*                 Recompile.
     h*
      //   09/17/15 C:302 Project: WC2254
      //                  Recompile
     h*
     h*  08/04/15 C:306 Support: PHW33697
     h*                 Modified to initialize the EDI payment/adjt subfile
     h*                 field while loading the grid.
     h*
     h*  03/06/15 C:65  Project: WC2247
     h*                 Change program to use WCMS action panel, not system,
     h*                 since this is a WCMS project.
     h*
     h*  09/11/14 C:295 Priss: 16
     h*                 EDI flaged feilds should not get displayed if
     h*                 EDI is not installed.
     h*
     h*  09/09/14 C:295 Priss: 12
     h*                 The action panel should have a same colour through out.
     h*
     h*
     h**********************************************************************

     fwpiclmpv  cf   e             workstn extfile('WPICLMPV')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPayVal:rrnPaycod)
     f                                     Sfile(sflPayValE:rrnPaycod)
     f*** begin chg ***  03/06/15  *****************************************
     f*****stactpnl  cf   e             workstn
     f*****                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     f*** end chg *****  03/06/15  *****************************************

     fwtabl     uf a e           k disk    rename(pb201:pb200)
     f                                     InfDs(dbfds1)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     f****** End Add ***** 09/11/14 ***************************************
     fssylf     if   e           K disk
     f                                     IGNORE(SSY01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)
     f****** End Add ***** 09/11/14 ***************************************

     d psds          esds                  extname(szq1p)
      * constant
     d intFlgDim       c                   10

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d                 ds
     d dsReservcd              1     20    dim(10)
     d B2rc01                  1      2
     d B2rc02                  3      4
     d B2rc03                  5      6
     d B2rc04                  7      8
     d B2rc05                  9     10
     d B2rc06                 11     12
     d B2rc07                 13     14
     d B2rc08                 15     16
     d B2rc09                 17     18
     d B2rc10                 19     20

     d dsPFilter       ds                  likerec(RcdPayVFlt)

     d intCount        s              3  0
     d @@mode          s              1
     d @@TabIndex      s              5
     d inmode          s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d ##rrn           s              9  0
     d clientID        s              3    inz('000')
     d meetPay         s              1
     d strcode         s                   like(scFilPaycd)
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

     d* Load payment value grid
     d LoadPayValue    pr

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
     d*** Beg Add *** 08/19/19 *********************************************
     d/copy sprModule
     d*** End Add *** 08/19/19 *********************************************
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
       //*** begin chg **** 03/06/15 ***********************************
       //*****clear scActPnHdg;
       //*****intCo# = userCo#(q1user);
       // Get company name
       //*****scActPnHdg = GetCompNam(intCo#);
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;
       //*** end chg ****** 03/06/15 ***********************************

       // Get the client id
       GetClientID(##CltId);

       clear dspFilter;
       tabClick = *Off;
       ##rrn = 0;
       //*** Beg Add *** 08/19/19 ****************************************
       visIDCbTc = isModEnabled('IDC');
       //*** End Add *** 08/19/19 ****************************************
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

          // Load Payment Value 1 code grid
          LoadPayValue();

          // Process Payment Value 1 code Screens
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // Filter Processing.
          When BtnFilter = *on;
             Dou BtnFilCan  = *on or BtnFilApp  = *on;

                ExFmt RcdPayVFlt;
                If BtnFilApp = *on;
                   dspFilter.scFilPaycd = scFilPaycd;
                   dspFilter.scFilSts  = scFilSts;
                Endif;

                If BtnFilCan  = *on;
                   scFilPaycd = dspFilter.scFilPaycd;
                   scFilSts   = dspFilter.scFilSts;
                Endif;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             clear dspFilter;
             clear rcdPayVFlt;
             dspClrFltr = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add Payment value
          When scPayAdd = *on;
             EditAddPV1();

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
          //*** begin chg **** 03/06/15 ***********************************
          //****** Beg Add **** 09/09/14 ***********************************
          //*****isindactp = *on;
          //****** End Add **** 09/09/14 ***********************************
          //*****Write rcdSysTbl;
          Write rcdComPnl;
          //*** end chg ****** 03/06/15 ***********************************
          Write rcdTabPay;
          Exfmt rcdPayCode;
          //*** begin chg **** 03/06/15 ***********************************
          //*****Read rcdSysTbl;
          Read rcdComPnl;
          //*** end chg ****** 03/06/15 ***********************************
          Read rcdTabPay;
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

       rrnPayCod  = 0;
       clrPayCod  = *On;
       dspPaySub  = *Off;
       dspPayCtl  = *Off;
       Write rcdPayCode;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       // Clear Filter - Visibility of button for Payment Value 1 Grid
       //===================================================================
     p LoadClrFlt      b
     d LoadClrFlt      pi

      /free

       // display clear filter button
       If dspFilter.scFilPaycd = *blanks and
          dspFilter.scFilSts  = *blanks;
          dspClrFltr = *off;
       Else;
          dspClrFltr = *on;
       Endif;

      /end-free
     p LoadClrFlt      e
       //===================================================================
       // Load Payment Value 1 code grid
       //===================================================================
     p LoadPayValue    b
     d LoadPayValue    pi

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
       clear sflPayVal;
       clear sflPayValE;
       clrPayCod = *Off;
       dspPaySub = *On;
       dspPayCtl = *On;
       clear dbRrn1;

       Setll intCo# pb200;
       Reade intCo# pb200;
       dow not %eof(wtabl);

          meetPay = *on;

          // Validate for filter
          MeetFlt();
          If meetPay = *on;

             // Write Payment Value 1 Codes

             // Code
             scPayCode =%trim(B2key);

             // Description
             scPayDesp =%trim(B2desc);

             // Placement
             scPayPlm = B2plCd;

             // EDI Payment/Adjustment Code
             //****** Beg Chg ******** 08/04/15  ***********************************
             scPayEDI = *Blanks;
             //****** End Chg ******** 08/04/15  ***********************************
             strTbid = 'PADJ';
             chain (intCo#:strTbid:B2pyCd) pgt00;
             If %found;
                scPayEDI = %Trim(GTkey) +' - ' + GTdesc;
             Endif;

             // Recovery Flag
             If B2rcvr = 'Y';
                scRecFlag = 'Yes';
             Else;
                scRecFlag = 'No';
             Endif;

             // Status
             If B2stat = 'A';
                scPayStat = 'Active';
             Else;
                scPayStat = 'Inactive';
             Endif;

             scPayRRN# = dbRrn1;

             rrnPayCod = rrnPayCod + 1;
             If rrnPayCod = 1;
                scPayRowSl = 'A';
             Else;
                clear scPayRowSl;
             Endif;

             If inmode = 'D';
                dspVisb = *Off;
             Else;
                dspVisb = *On;
             Endif;

             // Is EFT module is installed
             //****** Beg Del ******** 03/02/23  ***********************************
             // If ##Eft = 'Y';
             //    isEft =*on;
             // Else;
             //    isEft  = *off;
             // Endif;
             //****** End Del ******** 03/02/23  ***********************************

             // Is EDI module is installed
             //****** Beg Add ******** 09/11/14  ***********************************
             //****** If ##Edi  = 'Y';
             Chain 'WC' Swc01;
             If %found and WCfl09 = 'Y';
             //****** End Add ******** 09/11/14  ***********************************
                isPayEdi  = *on;
                isEdiGrid = *off;
             Else;
                isPayEdi  = *off;
                isEdiGrid = *on;
             Endif;

             Write sflPayValE;
             Write sflPayVal;
          Endif;
          Reade intCo# pb200;
       Enddo;

       rrnPayCod  = 1;

      /end-free
     p LoadPayValue    e
       //==================================================================
       // MeetFlt - Determine if the records meet the Filter criteria
       //==================================================================
     p MeetFlt         b
     d MeetFlt         pi
     d intIncr         s              3  0
     d meetCode        s              1n
      /free

       // Check for Reserve code
       strCode = %trim(Upper(scFilPaycd));
       If strCode <> ' ';
          For intCount = 1 to 10;
             If %trim(strCode) = dsReservcd(intCount);
                meetPay = *on;
                leave;
             Else;
                meetPay = *off;
             Endif;
          Endfor;
       Endif;

       If meetPay = *on;
          // Check for status
          If scFilSts  <> ' ' and scFilSts <> B2stat;
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
       If isEdiGrid = *off ;
          Readc sflPayVal;
       Else;
          Readc sflPayValE;
       Endif;

       Dow not %eof;
          If scPayEdt = *on;
             scPayEdt = *off;
             ##rrn = scPayRRN#;
             EditAddPV1(##rrn);
          Endif;

          If isEdiGrid = *off ;
             Readc sflPayVal;
          Else;
             Readc sflPayValE;
          Endif;
       Enddo;

      /end-free
     p ProcOther       E
