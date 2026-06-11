     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     h*****************************************************************
     h/title WTGRPSTRAT - Rating and Statistics Information
     h dftname(WTGRPSTRAT)
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h bnddir('WTGRPCFG')
     h/define   profoundUI
     h ALWNULL(*UsrCtl)
     H*****************************************************************
     H/TITLE WTGRPSTRAT
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                                *
     h*   08/25/25 C:300 Support: 119623                               *
     h*                  As per old WW Groups screens, Headings for    *
     h*                  'Contractor Credit Stat Code' is not Mandatory,
     h*                  Default it to be 'Contractor Credit', if Blank*
     h*                                                                *
     h*   11/26/24 C:291 Project: 148WFA                               *
     h*                  Add Mega Deductible Threshold on Large        *
     h*                  Deductible Tab.                               *
     h*                                                                *
     h*   11/14/24 C:300 Support: 118007                               *
     h*                  DSPF: Correct input field for Loss Cost Multiplier
     h*                  (scLCM) from drop-down to text entry          *
     h*                                                                *
     h*   11/11/24 C:300 Project: WC2985                               *
     h*                  Apply Deductible Discount to Discounted Premium*
     h*                  DSPF: On Premium Algorithm Tab, Update dropdown*
     h*                  for "Deductible Discount Applied To" to have  *
     h*                  a third choice of "Discounted" (D).           *
     h*                                                                *
     h*   04/29/24 C:340 Project: WC2965                               *
     h*                  Add Email Address Required Checkbox.          *
     h*                                                                *
     h*   04/18/24 C:291 Project: WC2961                               *
     h*                  Change the existing checkbox "Apply schedule  *
     h*                  DB/CR after Deviation" to a dropdown.         *
     h*                  Also changed headings for Deductible Discount,*
     h*                  Stock Discount, Short Rate Penanly and        *
     h*                  Schedule DB/CR.                               *
     h*                                                                *
     h*   11/17/23 C:300 Support: 114374 (Vital)                       *
     h*                  Add to base checkbox to allow schd db/cr to be*
     h*                  applied after Deviation (visible for all clients)
     h*                                                                *
     h*   09/25/23 C:300 Support: 114071                               *
     h*                  DSPF: On General Tab, corrected drop-down list*
     h*                  values of PEO Policy Type field               *
     h*                                                               *
     h*   09/18/23 C:65  Project:153WAL                               *
     h*                  Add stat code to safety fee.                 *
     h*                                                                *
     h*   08/07/23 C:300 Support: 113225                               *
     h*                  Allow entry of data in Upper/Lower case in    *
     h*                  - Description field on Large Deductible screen*
     h*                                                               *
     h*  06/02/23 C:342 Support:108934                                *
     h*                 Increased height of panel on Connect Tab      *
     h*                                                               *
     h*   01/17/22 C:29  Project: WC2788                              *
     h*                  Add "Copy Configuration" option to Group     *
     h*                  Toolbar.                                     *
     h*                                                               *
     h*   08/02/21 C:318 Project:WC2820                               *
     h*                  Added visibility condition to (Gradient)     *
     h*                  Discount Calculation Logic Fields            *
     h*                                                               *
     h*   06/18/21 C:300 Support:96286                                *
     h*                  Remove 2nd Large Deductible fields (Plan #2) *
     h*                  Remove 'Update All Divisions' checkbox from  *
     h*                  Large Deductible and comment call to W7138C  *
     h*                                                               *
     h*                                                               *
     h*  -------------------------------------------------------------*
     h*   NOTE: Please add comments to the top of Revisions.          *
     h*  -------------------------------------------------------------*
     h* ==============================================================*

     fWTGRPSTRATcf   e             workstn extfile('WTGRPSTRAT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflTaxAss:rrnTax)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Statistical Report/Rating File
     fwdrrp     uf a E             disk    rename(prr01:prr0p)
     f                                     infds(pffds)
     fwdrrl     uf a e           k disk    rename(prr01:prr00)
     f                                     infds(dbfds)
      * Large Deductible File
     fwmldl4    uf a e           k disk    rename(pld01:pld04)
     f                                     infds(dbfds2)
     Fwmldl3    uf   e           k disk    rename(pld01:pld03)
      * Carrier Rating Default File
     fwiratdftl uf a e           k disk    rename(pdf01:pdf00)
      * Carrier Rating Default Detail File
     fwiratddtl uf a e           k disk    rename(pdt01:pdt00)
      * Fund Detail File
     fwdf1l1    if   e           k disk    rename(pf101:pf101)
      * Deductible Discount Code File
     fwtddl3    if   e           k disk    rename(pdd01:pdd03)
      * Limit Code Table File
     fwtlml3    if   e           k disk    rename(plm01:plm03)
      * Fund master File
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      *** Beg Add ***** 01/17/22 ****************************
     f                                     infds(dbfds3)
      *** End Add ***** 01/17/22 ****************************
      * Discount File
     fwmdsl3    if   e           k disk    rename(pds01:pds03)
      * State Code File
     fststl3    if   e           k disk    rename(pst01:pst03)
     f*  Generic table file logical
     fwtabl     if   e           k disk
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SFB01)
     f                                     ignore(SRC01)
     f                                     ignore(SDR01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
     f                                     ignore(SGL01)
     f                                     ignore(SP201)

     dpsds           esds                  extname(szq1p)
     d pffds           ds
     d  pfrrn                397    400b 0
     d dbfds           DS
     d  dbrrn                397    400B 0
     d dbfds2          DS
     d  dbrrn2               397    400B 0
      *** Beg Add ***** 01/17/22 *****************************
     d dbfds3          DS
     d  dbrrn3               397    400B 0
      *** End Add ***** 01/17/22 *****************************

     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz

     ddsAss1           ds                                           inz
     dR1stp                           9  6
     dR1stf                           9  6
     dR1stl                           1
     dR1sth                          20
     dR1stc                           5  0
     dR1ilf                           1
     dR1xfc                           1
     dR1stm                           1
     dR1idv                           1
     dR1stb                           1
     dR1atd                           1
     dR1idd                           1
     dR1atr                           1

     ddsAss2           ds                                           inz
     dR2stp                           9  6
     dR2stf                           9  6
     dR2stl                           1
     dR2sth                          20
     dR2stc                           5  0
     dR2ilf                           1
     dR2xfc                           1
     dR2stm                           1
     dR2idv                           1
     dR2stb                           1
     dR2atd                           1
     dR2idd                           1
     dR2atr                           1

     ddsAss3           ds                                           inz
     dR3stp                           9  6
     dR3stf                           9  6
     dR3stl                           1
     dR3sth                          20
     dR3stc                           5  0
     dR3ilf                           1
     dR3xfc                           1
     dR3stm                           1
     dR3idv                           1
     dR3stb                           1
     dR3atd                           1
     dR3idd                           1
     dR3atr                           1

     ddsAss4           ds                                           inz
     dR4stp                           9  6
     dR4stf                           9  6
     dR4stl                           1
     dR4sth                          20
     dR4stc                           5  0
     dR4ilf                           1
     dR4xfc                           1
     dR4stm                           1
     dR4idv                           1
     dR4stb                           1
     dR4atd                           1
     dR4idd                           1
     dR4atr                           1

     ddsAss5           ds                                           inz
     dR5stp                           9  6
     dR5stf                           9  6
     dR5stl                           1
     dR5sth                          20
     dR5stc                           5  0
     dR5ilf                           1
     dR5xfc                           1
     dR5stm                           1
     dR5idv                           1
     dR5stb                           1
     dR5atd                           1
     dR5idd                           1
     dR5atr                           1

     ddsAss6           ds                                           inz
     dR6stp                           9  6
     dR6stf                           9  6
     dR6stl                           1
     dR6sth                          20
     dR6stc                           5  0
     dR6ilf                           1
     dR6xfc                           1
     dR6stm                           1
     dR6idv                           1
     dR6stb                           1
     dR6atd                           1
     dR6idd                           1
     dR6atr                           1

     ddsAss7           ds                                           inz
     dR7stp                           9  6
     dR7stf                           9  6
     dR7stl                           1
     dR7sth                          20
     dR7stc                           5  0
     dR7ilf                           1
     dR7xfc                           1
     dR7stm                           1
     dR7idv                           1
     dR7stb                           1
     dR7atd                           1
     dR7idd                           1
     dR7atr                           1

      // work variables
     d saveDT          s                   like(FNdt)
     d saveTM          s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d flgRcdUpdated   s              1    inz('0')
     D svTabindex      s                   like(Tabindex)
     d xsKey           s              2
     d error           s              1n   inz(*off)
     d DataSaved       s              1n   inz(*off)
     d DataWrite       s              1n   inz(*off)
     d Taxinfo         s              1n   inz(*off)
     d savTaxInfo      s              1n   inz(*off)
     d DelTax          s              1n   inz(*off)
     d decRRN1         s              9  0 inz
     d svRrnTax        s              9  0 inz
     d RrnTxAs         s              9  0 inz
     D xxKey           s                   like(BSKEY)
     D xxTBID          s                   like(GTTBID)

      *
     d charTbid        s              4
     d i               s              5  0 inz
     d @@cflg          s              1
     d clientID        s              3    inz('000')
     d suffix          s              3    inz
     d inRrn           s              9  0
     d intEmp#         s              9  0
     d intCo#          s              3  0
     d xxRrn           s              9  0 inz
     d process         s               n   inz(*off)
     d recExist        s               n   inz(*off)
     d Error1          s               n   inz(*off)
     d Error2          s               n   inz(*off)
     d Error3          s               n   inz(*off)
     d Error4          s               n   inz(*off)
     d isError         s               n   inz(*off)
     d isError4        s               n   inz(*off)
      *
     d qqrrn           s              9    inz
     d qqmode          s              1    inz
     d qqupd           s              1    inz

     d chgMod          s               n   inz(*off)
     d copyMode        s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d charSt          s              2    inz
     d intEffDt        s              7  0 inz
     d dtLossCst       s              7  0 inz
     d intDate         s              7  0 inz
     d outName         s             10
     d outType         s              7
     d outRtcd         s              1
     d message         s            100
     d svst            s              2    inz
     d svefdt          s              7  0 inz

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrSql
     d/copy sPrAtrium
     d/copy sPrModule
     d/copy wprPolErr
     d/copy witmpltd
     d/copy wPrGroup
      //*** Beg Add ***** 01/17/22 *****************************
     d/copy sCopyGrpPr
      //*** End Add ***** 01/17/22 *****************************

      * Delete Taxes and Assesment Tab Info
     dDelTaxInfo       pr
     d inRrn                          9  0 const

      * Edit Taxes and Assesment Tab Info
     dEditTaxInfo      pr
     d inRrnE                         9  0

      * Get client ID
     dGetClientID      pr                  extpgm('RTVCLTID')
     d outCltID                       3

      * Get client Suffix
     dGetCltSuffix     pr                  extpgm('SA030R')
     d suffix                         3

      * Check Object
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                             like(outName)
     d outType                             like(outType)
     d outRtcd                             like(outRtcd)

      * Load Fees Information
     d LoadFeesInfo    pr

      * Intialize the Screen fields
     d LoadInVal       pr

      * Load Statistical Premium Info
     d LoadPrmInfo     pr

      * Load Statistical Rating Grid
     d LoadStRtGrid    pr

      * Main Processing
     d Main            pr

     dProc_RatDft      pr
     d                                9  0
     d                                1

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      * Large Deductible Update & Exception Report
     d UpdLargeDed     pr                  extpgm('W7138C')
     d  qqrrn                         9
     d  qqmode                        1
     d  qqupd                         1

     C     *Entry        Plist
     C                   Parm                    @@RRN             9
     C                   Parm                    @@Mode            1
     C                   Parm                    @@Next           10
     C                   Parm                    @@TabIndex        5

      /Free
       //Start Of Program

       If @@mode = 'A';    // Add Mode
       // Retrieve Company and Group fromm RRN
          If %parms >= 1;
             intCo# = %int(%subst(@@rrn:1:3));
             scFnd = %int(%subst(@@rrn:4:3));
             xxRrn = 0;
          Endif;
       Endif;

       If @@mode <> 'A';   // Not Add Mode
          If %parms >= 1;
             xxRrn = %int(@@rrn);
          Endif;

          Chain(n) xxRrn prr0p;
          If %found(wdrrp);
             charSt = RRst;
             scMstate = RRst;
             intEffDt = RRefdt;
             scEffDat = dtMDYY(RRefdt);
             intCo# = RRco#;
             scCo# = RRco#;
             scFnd = RRfnd;
             Reset dsWITMPLTparms;
             dsWITMPLTparms.co# = RRCo#;
             dsWITMPLTparms.fnd = RRFnd;
             dsWITMPLTparms.state = RRst;
          Endif;
       Endif;

       If @@mode = 'Y';   // Delete Mode
          Exsr DeleteRatInfo;
       Endif;

       If @@mode <> 'Y';
          ProgramStart(process);
          If process;
             Main();
          Endif;
       Endif;

       *InLr = *on;

      //===================================================================

       Begsr DeleteRatInfo;
        Dou not %found or %eof;
           Delete (RRco#:RRfnd:0:0:0:RRst:RRefDt) pld03;
        Enddo;
        Dou not %found or %eof;
           Delete (RRco#:RRfnd:RRst:RRefDt) pdf00;
        Enddo;
        Dou not %found or %eof;
           Delete (RRco#:RRfnd:RRst:RRefDt) pdt00;
        Enddo;

        Delete xxRrn prr0p;
       Endsr;

      /End-Free

       //===================================================================
       // Add information to list
       //===================================================================
     PAddToList        b
     DAddToList        pi           101a   varying
     D inString                     100a   varying const
     D inArray                      100a   varying dim(20)
     D inIndex                       10i 0

     DoutString        s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     PAddToList        e

      //====================================================
      // Check if Factor is set up in Rating and Stat
      //====================================================
     pDefaultNotSetUp...
     p                 b
     dDefaultNotSetUp...
     d                 pi              n
     d inEntry                       10a
     d inCode                        10a   Options(*Nopass)
     d str10a          s             10a
     d str3a           s              3a
      /Free
       If %Parms > 1;
          str10a = inCode;
          str3a = inCode;
       Endif;
       Select;
       When inEntry = 'CONCREDIT';
          If RRcccd = *Zero;
             Return *On;
          EndIf;
       When inEntry = 'DEV1';
          If RRdv1c = *Zero;
             Return *On;
          Endif;
       When inEntry = 'DEV2';
          If  RRdv2c = *Zero;
             Return *On;
          Endif;
       When inEntry = 'REINSUR ';
          If RRarcc = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SAFETYFEE';
          If RRsfpc = *Zero;
             Return *On;
          EndIf;
       When inEntry = 'SCHCREDIT';
          If RRcrcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SCHDEBIT';
          If RRdbcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SUBJECT1A';
          If RR1acd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SUBJECT1B';
          If RR1bcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'VOLUNTARY';
          If RRvlcc = *Zero;
             Return *On;
          Endif;
       When inEntry = 'WAIVER';
          If RRwvcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'EXPMOD';
          If RReccd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'MEMBERFEE';
          Setgt (RRco#:RRfnd:RRefdt) pf101;
          Readpe (RRco#:RRfnd) pf101;
          If %Eof Or F1mfee = 0;
             Return *On;
          Endif;
       When inEntry = 'INCLIMIT';
          Select;
          When str10a = *Blanks;
             Chain (RRco#:RRfnd:RRst) plm03;
             If Not(%Found);
                Return *On;
             EndIf;
          Other;
             Chain (RRco#:RRfnd:RRst:str10a) plm03;
             If Not(%Found);
                Return *On;
             Endif;
          EndSl;
       When inEntry = 'DEDUCT';
          Select;
          When str3a = *Blanks;
             Setll (RRco#:RRfnd:RRst) pdd03;
             Dou (%Eof);
                Reade (RRco#:RRfnd:RRst) pdd03;
                If %Eof Or DDstat = 'A';
                   Leave;
                EndIf;
             Enddo;
             Return %Eof;
          Other;
             Setll (RRco#:RRfnd:RRst:str3a) pdd03;
             Dou (%Eof);
                Reade (RRco#:RRfnd:RRst:str3a) pdd03;
                If %Eof Or DDstat = 'A';
                   Leave;
                EndIf;
             Enddo;
             Return %Eof;
          Endsl;
       When inEntry = 'PRMDISC';
       EndSl;
       Return *Off;
      /End-Free
     pDefaultNotSetUp...
     p                 e

      //===================================================================
      // Delete Taxes and Assessment Info
      //===================================================================
     PDelTaxInfo       b
     DDelTaxInfo       pi
     D inRrn                          9  0 const
      /Free
       Select;
       When inRrn = 1;
          R1stp = 0;
          R1stf = 0;
          R1stl = ' ';
          R1sth = ' ';
          R1stc = 0;
          R1ilf = ' ';
          R1xfc = ' ';
          R1stm = ' ';
          R1idv = ' ';
          R1stb = ' ';
          R1atd = ' ';
          R1idd = ' ';
          R1atr = ' ';
          DelTax = *on;

       When inRrn = 2;
          R2stp = 0;
          R2stf = 0;
          R2stl = ' ';
          R2sth = ' ';
          R2stc = 0;
          R2ilf = ' ';
          R2xfc = ' ';
          R2stm = ' ';
          R2idv = ' ';
          R2stb = ' ';
          R2atd = ' ';
          R2idd = ' ';
          R2atr = ' ';
          DelTax = *on;

       When inRrn = 3;
          R3stp = 0;
          R3stf = 0;
          R3stl = ' ';
          R3sth = ' ';
          R3stc = 0;
          R3ilf = ' ';
          R3xfc = ' ';
          R3stm = ' ';
          R3idv = ' ';
          R3stb = ' ';
          R3atd = ' ';
          R3idd = ' ';
          R3atr = ' ';
          DelTax = *on;

       When inRrn = 4;
          R4stp = 0;
          R4stf = 0;
          R4stl = ' ';
          R4sth = ' ';
          R4stc = 0;
          R4ilf = ' ';
          R4xfc = ' ';
          R4stm = ' ';
          R4idv = ' ';
          R4stb = ' ';
          R4atd = ' ';
          R4idd = ' ';
          R4atr = ' ';
          DelTax = *on;

       When inRrn = 5;
          R5stp = 0;
          R5stf = 0;
          R5stl = ' ';
          R5sth = ' ';
          R5stc = 0;
          R5ilf = ' ';
          R5xfc = ' ';
          R5stm = ' ';
          R5idv = ' ';
          R5stb = ' ';
          R5atd = ' ';
          R5idd = ' ';
          R5atr = ' ';
          DelTax = *on;

       When inRrn = 6;
          R6stp = 0;
          R6stf = 0;
          R6stl = ' ';
          R6sth = ' ';
          R6stc = 0;
          R6ilf = ' ';
          R6xfc = ' ';
          R6stm = ' ';
          R6idv = ' ';
          R6stb = ' ';
          R6atd = ' ';
          R6idd = ' ';
          R6atr = ' ';
          DelTax = *on;

       When inRrn = 7;
          R7stp = 0;
          R7stf = 0;
          R7stl = ' ';
          R7sth = ' ';
          R7stc = 0;
          R7ilf = ' ';
          R7xfc = ' ';
          R7stm = ' ';
          R7idv = ' ';
          R7stb = ' ';
          R7atd = ' ';
          R7idd = ' ';
          R7atr = ' ';
          DelTax = *on;
       Endsl;
      /End-Free
     PDelTaxInfo       e

      //===================================================================
      // Edit Tax and Assessment Info
      //===================================================================
     PEditTaxInfo      b
     DEditTaxInfo      pi
     D inRrnE                         9  0
     D isexit          s               n   Inz(*off)
      /Free
       Exsr BlnkTaxInfo;
       isexit = *off;

       // Load Assessment Info
       Select;
       When inRrnE = 1;
          AssType = 'First Assessment';
          scStP = R1stp;
          scStF = R1stf;
          scStL = R1stl;
          scStH = R1sth;
          scStC = R1stc;
          scIlF = R1ilf;
          scXfc = R1xfc;
          scStm = R1stm;
          scIdV = R1idv;
          scStB = R1stb;
          scAtD = R1atd;
          scIdD = R1idd;
          scAtR = R1atr;

       When inRrnE = 2;
          AssType ='Second Assessment';
          scStP = R2stp;
          scStF = R2stf;
          scStL = R2stl;
          scStH = R2sth;
          scStC = R2stc;
          scIlF = R2ilf;
          scXfc = R2xfc;
          scStm = R2stm;
          scIdV = R2idv;
          scStB = R2stb;
          scAtD = R2atd;
          scIdD = R2idd;
          scAtR = R2atr;

       When inRrnE = 3;
          AssType = 'Third Assessment';
          scStP = R3stp;
          scStF = R3stf;
          scStL = R3stl;
          scStH = R3sth;
          scStC = R3stc;
          scIlF = R3ilf;
          scXfc = R3xfc;
          scStm = R3stm;
          scIdV = R3idv;
          scStB = R3stb;
          scAtD = R3atd;
          scIdD = R3idd;
          scAtR = R3atr;

       When inRrnE = 4;
          AssType = 'Fourth Assessment';
          scStP = R4stp;
          scStF = R4stf;
          scStL = R4stl;
          scStH = R4sth;
          scStC = R4stc;
          scIlF = R4ilf;
          scXfc = R4xfc;
          scStm = R4stm;
          scIdV = R4idv;
          scStB = R4stb;
          scAtD = R4atd;
          scIdD = R4idd;
          scAtR = R4atr;

       When inRrnE = 5;
          AssType = 'Fifth Assessment';
          scStP = R5stp;
          scStF = R5stf;
          scStL = R5stl;
          scStH = R5sth;
          scStC = R5stc;
          scIlF = R5ilf;
          scXfc = R5xfc;
          scStm = R5stm;
          scIdV = R5idv;
          scStB = R5stb;
          scAtD = R5atd;
          scIdD = R5idd;
          scAtR = R5atr;

       When inRrnE = 6;
          AssType = 'Sixth Assessment';
          scStP = R6stp;
          scStF = R6stf;
          scStL = R6stl;
          scStH = R6sth;
          scStC = R6stc;
          scIlF = R6ilf;
          scXfc = R6xfc;
          scStm = R6stm;
          scIdV = R6idv;
          scStB = R6stb;
          scAtD = R6atd;
          scIdD = R6idd;
          scAtR = R6atr;

       When inRrnE = 7;
          BtnContinu = *off;
          isCont = *off;
          isSave = *on;
          AssType = 'Seventh Assessment';
          scStP = R7stp;
          scStF = R7stf;
          scStL = R7stl;
          scStH = R7sth;
          scStC = R7stc;
          scIlF = R7ilf;
          scXfc = R7xfc;
          scStm = R7stm;
          scIdV = R7idv;
          scStB = R7stb;
          scAtD = R7atd;
          scIdD = R7idd;
          scAtR = R7atr;
       Endsl;


       Dow isexit = *off;
          Exfmt RcdAssess;

          If BtnTaxSave = *on or BtnContinu = *on;
             BtnTaxSave = *off;
             BtnContinu = *off;
             If ValidTaxInfo();

                If scIdD = ' ';
                   scIdD = 'N';
                Endif;
                If scIdV = ' ';
                   scIdV = 'N';
                Endif;

                Select;
                When inRrnE = 1;
                   R1stp = scStP;
                   R1stf = scStF;
                   R1stl = scStL;
                   R1sth = scStH;
                   R1stc = scStC;
                   R1ilf = scIlF;
                   R1xfc = scXfc;
                   R1stm = scStm;
                   R1idv = scIdV;
                   R1stb = scStB;
                   R1atd = scAtD;
                   R1idd = scIdD;
                   R1atr = scAtR;

                When inRrnE = 2;
                   R2stp = scStP;
                   R2stf = scStF;
                   R2stl = scStL;
                   R2sth = scStH;
                   R2stc = scStC;
                   R2ilf = scIlF;
                   R2xfc = scXfc;
                   R2stm = scStm;
                   R2idv = scIdV;
                   R2stb = scStB;
                   R2atd = scAtD;
                   R2idd = scIdD;
                   R2atr = scAtR;

                When inRrnE = 3;
                   R3stp = scStP;
                   R3stf = scStF;
                   R3stl = scStL;
                   R3sth = scStH;
                   R3stc = scStC;
                   R3ilf = scIlF;
                   R3xfc = scXfc;
                   R3stm = scStm;
                   R3idv = scIdV;
                   R3stb = scStB;
                   R3atd = scAtD;
                   R3idd = scIdD;
                   R3atr = scAtR;

                When inRrnE = 4;
                   R4stp = scStP;
                   R4stf = scStF;
                   R4stl = scStL;
                   R4sth = scStH;
                   R4stc = scStC;
                   R4ilf = scIlF;
                   R4xfc = scXfc;
                   R4stm = scStm;
                   R4idv = scIdV;
                   R4stb = scStB;
                   R4atd = scAtD;
                   R4idd = scIdD;
                   R4atr = scAtR;

                When inRrnE = 5;
                   R5stp = scStP;
                   R5stf = scStF;
                   R5stl = scStL;
                   R5sth = scStH;
                   R5stc = scStC;
                   R5ilf = scIlF;
                   R5xfc = scXfc;
                   R5stm = scStm;
                   R5idv = scIdV;
                   R5stb = scStB;
                   R5atd = scAtD;
                   R5idd = scIdD;
                   R5atr = scAtR;

                When inRrnE = 6;
                   R6stp = scStP;
                   R6stf = scStF;
                   R6stl = scStL;
                   R6sth = scStH;
                   R6stc = scStC;
                   R6ilf = scIlF;
                   R6xfc = scXfc;
                   R6stm = scStm;
                   R6idv = scIdV;
                   R6stb = scStB;
                   R6atd = scAtD;
                   R6idd = scIdD;
                   R6atr = scAtR;

                When inRrnE = 7;
                   R7stp = scStP;
                   R7stf = scStF;
                   R7stl = scStL;
                   R7sth = scStH;
                   R7stc = scStC;
                   R7ilf = scIlF;
                   R7xfc = scXfc;
                   R7stm = scStm;
                   R7idv = scIdV;
                   R7stb = scStB;
                   R7atd = scAtD;
                   R7idd = scIdD;
                   R7atr = scAtR;
                Endsl;
                savTaxInfo = *on;
                isexit = *on;
             Endif;
          Endif;

          If BtnTxCan = *on;
             BtnTxCan = *off;
             TaxInfo = *on;
             isexit = *on;
          Endif;
       Enddo;


       Begsr BlnkTaxInfo;
        AssType = ' ';
        scStP = 0;
        scStF = 0;
        scStL = ' ';
        scStH = ' ';
        scStC = 0;
        scIlF = ' ';
        scAtR = ' ';
        scXfc = ' ';
        scStm = ' ';
        scIdV = ' ';
        scStB = ' ';
        scAtD = ' ';
        scIdD = ' ';
       Endsr;

      /End-Free
     PEditTaxInfo      e

       //===================================================================
       // Fill Large Deductible Info
       //===================================================================
     P FillDedInfo     B
     D FillDedInfo     PI
      /Free
       intEmp# = 0;
       Chain(n) (intCo#:scFnd:charSt:intEmp#:intEffdt) pld04;
       If CopyMode = *on and DataSaved = *off;      // Copy Mode
          Chain(n) (intCo#:scFnd:svst:intEmp#:svefdt) pld04;
       Endif;
       If %found(wmldl4);
          scLdDes1 = LDdes1;
          scLdPct1 = LDpct1;
          scLdTyp1 = LDtyp1;
          scLdAmp1 = LDamp1;
          scLdAmg1 = LDamg1;
          scLdVrb1 = LDvrb1;
          scLdOcv1 = LDocv1;
          scLdCls1 = LDcls1;
          scLdPlv1 = LDplv1;
          scLdPln1 = LDpln1;
          scLdTot1 = LDtot1;
          scLdAla1 = LDala1;
          scLdRnw1 = LDrnw1;
          scLdR101 = LDr101;
          scLdR102 = LDr102;
          scLdR103 = LDr103;
          scLdR104 = LDr104;
          scLdR105 = LDr105;
          scLdR106 = LDr106;
          scLdR107 = LDr107;
          scLdR108 = LDr108;
          scLdR109 = LDr109;
          scLdR110 = LDr110;
          scAdP1 = LDadp1;
          scVarA1 = LDvarA1;
          //*** Beg Add ***** 11/26/24 ****************************************
          scMegaT = LDMegaT;
          //*** End Add ***** 11/26/24 ****************************************

          saveDT2 = LDdt;
          saveTM2 = LDtime;
       Endif;
      /End-free
     P FillDedInfo     E

       //***************************************************************************//
       // Fill Fees Info
       //***************************************************************************//
     p FillFeesInfo    b
     d FillFeesInfo    pi
      /Free
        scSfPc = RRsfpc;
        scSfAp = RRsfap;
        scSfHd = RRsfhd;
        //*** begin add ***  09/18/23  ********************************
        scSfCd = RRsfCd;
        //*** end add *****  09/18/23  ********************************
        scSfAm = RRsfam;
        scEnId = RRenid;
        scSpFl = RRspfl;
        scYrad = RRyrad;
        scCxAd = RRcxad;
        scTxPc = RRtxpc;
        scTxHd = RRtxhd;
        scTdFl = RRtdfl;
        scRdTx = RRrdtx;
        scTeFl = RRtefl;
        scIdTp = RRidtp;
        scTaxm = RRtaxm;
        scWdtp = RRwdtp;
        scWdpc = RRwdpc;
        scWdfc = RRwdfc;
        scWdmn = RRwdmn;
        scWvfl = RRwvfl;
        scPfCd = RRPfCd;
        scPfAm = RRPfAm;
        scPfHd = RRPfHd;
        scLaTe = RRlate;
        scLtCd = RRltcd;
        scLtAm = RRltam;
        scRdev = RRrdev;

      /End-free
     p FillFeesInfo    e

       //===================================================================
       // FillGeneralInfo
       //===================================================================
     P FillGenInfo     B
     D FillGenInfo     PI

      /Free
       scDrat = RRdrat;
       scBscd = RRbscd;
       scDftLsID = RRlsid;
       scSemh = RRsemh;
       scBafl = RRbafl;
       scSpsg = RRspsg;
       scUrfl = RRurfl;
       scNAIC = RRnaic;
       scRifl = RRrifl;
       scPpfl = RRppfl;
       scSrrb = RRsrrb;
       scSrrq = RRsrrq;
       scNrrb = RRnrrb;
       scNrrq = RRnrrq;
       scDbcr = RRdbcr;
       scEmlb = RRemlb;
       scCeOv = RRceov;
       scAcC  = RRacc;
       scDiS  = RRdis;
       scAgG  = RRagg;
       scExSt = RRExSt;
       scPgFl = RRpgfl;
       scPeOa = RRpeoa;
       If scPeOa = ' ';
          scPeOa = 'L/C/F';
       Endif;
       scPeoPT = RRpeoF;
       scNmInDft = RRNmInDft;
       //*** Beg Add ***  04/29/24  *** WC2965 *************************
       scEmail = RREmail;
       //*** End Add ***  04/29/24  *** WC2965 *************************

      /End-free
     P FillGenInfo     E

       //===================================================================
       // FillPrmInfo -- Fill Premium Algorithm Info
       //===================================================================
     P FillPrmInfo     B
     D FillPrmInfo     PI
      /Free
       scMstate = RRst;
       scEffDat = dtMDYY(RRefdt);
       If CopyMode = *on and svst = ' ';            // Save original values
          svst = RRst;
          svefdt = RRefdt;
       Endif;
       scCcCd = RRcccd;
       scCcHd = %trim(RRccHd);
       scRwCc = RRrwcc;
       sc1Acd = RR1acd;
       sc1Ahd = %trim(RR1Ahd);
       scRW1A = RRrw1a;
       sc1Bcd = RR1bcd;
       sc1Bhd = RR1bhd;
       scRw1b = RRRw1b;
       scCrCd = RRcrcd;
       scCrHd = RRcrhd;
       scRwDc = RRrwdc;
       scDbCd = RRdbcd;
       scDbHd = RRdbhd;
       scWvAc = RRwvac;
       scWvHd = RRwvhd;
       scVlCc = RRvlcc;
       scVlCh = RRvlch;
       scDv1c = RRdv1c;
       scDv1h = RRdv1h;
       scRwD1 = RRrwd1;
       scDv1s = RRdv1s;
       scDv1Chg = RRdv1chg;
       scDv1DfRnw = RRDv1dfrnw;
       scDv1l = RRDv1l;
       scDv1Dft = RRdv1dft;
       scDv2c = RRdv2c;
       scDv2h = RRdv2h;
       scRwD2 = RRrwd2;
       scDv2s = RRdv2s;
       scDv2Chg = RRdv2Chg;
       scDv2DfRnw = RRdv2dfrnw;
       scDv2l = RRdv2l;
       scDv2Dft = RRdv2dft;
       scDcCd = RRdccd;
       scAmAx = RRamax;
       scAsFl = RRasfl;
       scMrRd = RRmrrd;
       scMrrDt = RRmrrdt;
       scMrrD2 = RRmrrd2;
       scMrRc = RRmrrc;
       scMrrCt = RRmrrct;
       scMrrC2 = RRmrrc2;
       scAnCs = RRancs;
       scAnCm = RRancm;
       scEcCd = RReccd;
       scMpCd = RRmpcd;
       scMrRn = RRmrrn;
       scIlMc = RRilmc;
       scArCc = RRarcc;
       scRdPr = RRrdpr;
       scSrAf = RRsraf;
       scLcm  = RRLcm;
       scLcEx = DtMDYY(RRlcex);
       scDv2s = RRdv2s;
       scTeRc = RRterc;
       scTeRr = RRterr;
       scTeR2 = RRter2;
       scTeRh = RRterh;
       scDtRc = RRdtrc;
       scDtRr = RRdtrr;
       scDtRh = RRdtrh;
       scMonoSt = RRmono;
       scPullSrd = RRpullsrd;
       scPtFl = RRptfl;
       scTeRl = RRterl;
       scDMax = RRdmax;
       scCMax = RRcmax;
       //*** begin add ***  11/17/23  ********************************
       scDcap = RRdcap;
       //*** end add *****  11/17/23  ********************************
       scDsPr = RRdspr;
       scDsPc = RRdspc;
       scDiSc = RRdisc;
       scDsHd = RRdshd;
       scDsIn = RRdsin;
       scDcl = RRdcl;
       If scDcl = ' ';
          scDcl = 'N';
       Endif;
       scMinFd = RRMinFD;
       scMinSd = RRMinSD;
       scMinSc = RRMinSC;
       isGradient = *off;
       If GetGrScoringVnd(RRCO#:RRFND) ='Gradient';
          isGradient = *on;
       Endif;

       saveDT = RRdt;
       saveTM = RRtime;
      /End-free
     P FillPrmInfo     E

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi
      /Free
       //scCompName = GetCompNam(userCo#(q1user));
       //intco# = RRco#;
       //scCo# = RRco#;
       //scFnd = RRfnd;
       inquiry_SR = *off;
       Clear dsAss1;
       Clear dsAss2;
       Clear dsAss3;
       Clear dsAss4;
       Clear dsAss5;
       Clear dsAss6;
       Clear dsAss7;

      /End-Free
     p LoadInVal       e

       //===================================================================
       // Load Taxes and Assessments Info
       //===================================================================
     p LoadTaxesInfo   b
     d LoadTaxesInfo   pi
      /Free

       rrnTax = 0;
       scDisSfl = *off;
       scClearSfl = *on;
       Write RcdTaxes;
       scClearSfl= *off;
       scDisSfl  = *on;
       selTax = *Off;
       scRrnTxAs = 0;
       scRowSel = 1;
       i = 1;

       visInq = *off;
       If dspMode = *on;
          visInq = *on;
       Endif;

       //Populate the subfile
       Dow (i <= 7);
          selTax = *off;

          Select;
          When i =1;
             scAssNm  = 'First';
             scPerc   = RR1stp;
             scAppTo  = RR1stl;
             scOptHdg = RR1sth;
             scStatCd = RR1stc;

          When i = 2;
             scAssNm  = 'Second';
             scPerc   = RR2ndp;
             scAppTo  = RR2ndl;
             scOptHdg = RR2ndh;
             scStatCd = RR2ndc;

          When i = 3;
             scAssNm  = 'Third';
             scPerc   = RR3rdp;
             scAppTo  = RR3rdl;
             scOptHdg = RR3rdh;
             scStatCd = RR3rdc;

          When i = 4;
             scAssNm  = 'Fourth';
             scPerc   = RR4thp;
             scAppTo  = RR4thl;
             scOptHdg = RR4thh;
             scStatCd = RR4thc;

          When i = 5;
             scAssNm  = 'Fifth';
             scPerc   = RR5thp;
             scAppTo  = RR5thl;
             scOptHdg = RR5thh;
             scStatCd = RR5thc;

          When i = 6;
             scAssNm  = 'Sixth';
             scPerc   = RR6thp;
             scAppTo  = RR6thl;
             scOptHdg = RR6thh;
             scStatCd = RR6thc;

          When i = 7;
             scAssNm  = 'Seventh';
             scPerc   = RR7thp;
             scAppTo  = RR7thl;
             scOptHdg = RR7thh;
             scStatCd = RR7thc;

          Endsl;

          //Populate Description for Applied To
          Select;
          When scAppTo = 'J';
             scAppToDes = 'Subject';
          When scAppTo = 'E';
             scAppToDes = 'Modified';
          When scAppTo = 'S';
             scAppToDes = 'Standard';
          When scAppTo = 'D';
             scAppToDes = 'Discounted';
          When scAppTo = 'X';
             scAppToDes = 'Annual';
          Other;
             scAppToDes = ' ';
          Endsl;

          rrnTax = rrnTax + 1;
          scRrnTax = dbrrn;
          scRrnTxAs = scRrnTxAs + 1;

          // set the selection on 1st record for first load
          If svRrnTax = 0;
             If scRrnTxAs = 1;
                selTax = *on;
             Endif;
          Else;
             If scRrnTxAs = svRrnTax;
                selTax = *on;
             Endif;
          Endif;

          Write sflTaxAss;

          i = i + 1;
       Enddo;

       rrnTax = 1;
       If svRrnTax <> 0;
          rrnTax = svRrnTax;
          scRowSel = svRrnTax;
          svRrnTax  = 0;
       EndIf;


       // Load all 7 Tax info DS (dsAss1-7)
       R1Stp = RR1stp;
       R1Stf = RR1stf;
       R1Stl = RR1stl;
       R1Sth = RR1sth;
       R1Stc = RR1stc;
       R1Ilf = RR1ilf;
       R1xfc = RR1xfc;
       R1stm = RR1stm;
       R1Idv = RR1idv;
       R1Stb = RR1stb;
       R1Atd = RR1atd;
       R1Idd = RR1idd;
       R1Atr = RR1atr;
       R2Stp = RR2ndp;
       R2Stf = RR2ndf;
       R2Stl = RR2ndl;
       R2Sth = RR2ndh;
       R2Stc = RR2ndc;
       R2Ilf = RR2ilf;
       R2xfc = RR2xfc;
       R2stb = RR2ndb;
       R2Idd = RR2idd;
       R2Stm = RR2ndm;
       R2Atd = RR2atd;
       R2Atr = RR2atr;
       R2Idv = RR2idv;
       R3Stp = RR3rdp;
       R3Stf = RR3rdf;
       R3Stl = RR3rdl;
       R3Sth = RR3rdh;
       R3Stc = RR3rdc;
       R3Ilf = RR3ilf;
       R3Xfc = RR3xfc;
       R3Stb = RR3rdb;
       R3Idd = RR3idd;
       R3Stm = RR3rdm;
       R3Atd = RR3atd;
       R3Atr = RR3atr;
       R3Idv = RR3idv;
       R4Stp = RR4thp;
       R4Stf = RR4thf;
       R4Stl = RR4thl;
       R4Sth = RR4thh;
       R4Stc = RR4thc;
       R4Ilf = RR4ilf;
       R4Xfc = RR4xfc;
       R4Stb = RR4thb;
       R4Idd = RR4idd;
       R4Stm = RR4thm;
       R4Atd = RR4atd;
       R4Atr = RR4atr;
       R4Idv = RR4idv;
       R5Stp = RR5thp;
       R5Stf = RR5thf;
       R5Stl = RR5thl;
       R5Sth = RR5thh;
       R5Stc = RR5thc;
       R5Ilf = RR5ilf;
       R5Xfc = RR5xfc;
       R5Stb = RR5thb;
       R5Idd = RR5idd;
       R5Stm = RR5thm;
       R5Atd = RR5atd;
       R5Atr = RR5atr;
       R5Idv = RR5idv;
       R6Stp = RR6thp;
       R6Stf = RR6thf;
       R6Stl = RR6thl;
       R6Sth = RR6thh;
       R6Stc = RR6thc;
       R6Ilf = RR6ilf;
       R6Xfc = RR6xfc;
       R6Stb = RR6thb;
       R6Idd = RR6idd;
       R6Stm = RR6thm;
       R6Atd = RR6atd;
       R6Atr = RR6atr;
       R6Idv = RR6idv;
       R7Stp = RR7thp;
       R7Stf = RR7thf;
       R7Stl = RR7thl;
       R7Sth = RR7thh;
       R7Stc = RR7thc;
       R7Ilf = RR7ilf;
       R7Xfc = RR7xfc;
       R7Stb = RR7thb;
       R7Idd = RR7idd;
       R7Stm = RR7thm;
       R7Atd = RR7atd;
       R7Atr = RR7atr;
       R7Idv = RR7idv;

      /End-free
     p LoadTaxesInfo   e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /Free
       If AddMode = *on;
          Dou isError = *off;
             pnlHeight = 750;
             APnlHeight = pnlHeight + 60;
             indPolDis = *On;                  // Hide Group Toolbar
             Write rcdGrpPnl;
             Write RcdPanel;
             Read RcdPanel;
             Read rcdGrpPnl;
             isError = *off;
             erMState = *off;
             msMState = *blanks;
             erEfDt = *off;
             msEfDt = *blanks;

             If SCMState = *Blanks;
                isError = *On;
                erMState = *On;
                msMState = 'WCR0503';
             Endif;

             erEfDt = ValidDate(scEffDat:message);
             If erEfDt = *on;
                isError = *On;
                msEfDt = message;
             Endif;

             clear intEffDt;
             intEffDt = dtCYMD(scEffDat);
             Chain(n) (intCo#:scFnd:SCMState:intEffDt) prr00;
             If %found(wdrrl);
                erMstate = *on;
                erEfdt = *on;
                msMState = 'WC30005';
                msEfDt = 'WC30005';
                isError = *On;
             Endif;
          Enddo;

          If isError = *off and dataWrite = *off;
             Clear prr0p;
             RRco# = intCo#;
             RRfnd = scFnd;
             RRst   = scMstate;
             RREfDt = dtcymd(scEffDat);
             RRiusr  = q1user;
             RRipgm  = q1pgm;
             RRidt   = Dateto7(SystemDate());
             RRitim  = Timeto6(SystemTime());
             RRuser = q1user;
             RRpgm  = q1pgm;
             RRtime = Timeto6(SystemTime());
             RRdt   = Dateto7(SystemDate());
             Write  prr0p;
             DataWrite = *on;

             // Disable State and Effective Date for Add Mode
             isNotAdd = *on;

             // Popuate screen fields
             scCo# = RRco#;
             scFnd = RRfnd;
             Reset dsWITMPLTparms;
             dsWITMPLTparms.co# = RRCo#;
             dsWITMPLTparms.fnd = RRFnd;
             dsWITMPLTparms.state = RRst;
          Endif;
       Endif;

       indPolDis = *Off;                       // Show Group Toolbar
       BtnCancel = *Off;
       tabClick = *off;

       //Process until cancel button is pressed.
       Dou BtnCancel = *On ;

          If AddMode = *on and dataWrite = *off;
             visPanel = *off;
          Else;
             visPanel = *on;
          Endif;
          isConnect = *off;

          Select;
          When error = *on;
             Tabindex = 0;
          When error1 = *on;
             Tabindex = 1;
          When error2 = *on or TaxInfo = *on;
             Tabindex = 2;
             Taxinfo = *off;
          When error3 = *on;
             Tabindex = 3;
          When error4 = *on;
             Tabindex = 4;
             hasConnect = IsModEnabled('Portal Module');
             If hasConnect = *on;
                Tabindex = 5;
             Endif;
          Endsl;

          If svTabindex <> 0;      // For Record update By another user Error
             Tabindex = svTabindex;
             svTabindex = 0;
          Endif;

          //Display Screen
          Exsr ExfmtScreens;

          error = *off;
          error1 = *off;
          error2 = *off;
          error3 = *off;
          error4 = *off;

          Select;

          //Do nothing - catch tab click event
          When tabClick = *on;
             //tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh = *Off;
             flgRcdUpdated = '0';
             //Intialize the Screen fields
             LoadInVal();
             //Load Action panel with the Company Name.
             intCo# = userCo#(q1user);
             //scCompName = GetCompNam(intCo#);

          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          //*** End Add ***** 01/17/22 *******************************

          //Enter key or Save button
          When btnSave = *on;
             btnSave = *off;

             If @@mode <> 'D';
                // Check if WDRRP updated since screen displayed (Statistical Report/Rating)
                flgRcdUpdated = RcdUpdatedErr('WDRRP':pfrrn:'RRdt':'RRtime':
                 'RRuser':saveDT:saveTM);
                if flgRcdUpdated = '0' and saveDT2 <> 0; // No error
                   // Check if WMLDL4 updated since screen displayed (Large Deductible)
                   flgRcdUpdated= RcdUpdatedErr('WMLDL4':dbrrn2:'LDdt':'LDtime':
                    'LDuser':saveDT2:saveTM2);
                EndIf;

                if flgRcdUpdated = '0'; // No error

                   //Validate Premium Info (Tab 1)
                   If ValidPrmInfo();
                      error = *off;
                   Else;
                      error = *on;
                   EndIf;

                   //Validate Fees Info (Tab 2)
                   If ValidFeesInfo();
                      error1 = *off;
                   Else;
                      error1 = *on;
                   EndIf;

                   //Validate Taxes Info (Tab 3)
                   If ValidTaxInfo();
                      error2 = *off;
                   Else;
                      error2 = *on;
                   EndIf;

                   //Validate Deductible Info (Tab 4)
                   If ValidDedInfo();
                      error3 = *off;
                   Else;
                      error3 = *on;
                   EndIf;

                   //Validate General Info (Tab 5)
                   If ValidGenInfo();
                      error4 = *off;
                   Else;
                      error4 = *on;
                   EndIf;

                Else;
                   svTabindex = Tabindex;    // For Record update By another user Error
                EndIf;

                //Show Data Saved Sucessfully Message
                If error = *off and error1 = *off and error3 = *off and
                 error4 = *off and flgRcdUpdated = '0';
                   Exsr SaveGrpDetail;

                   //Extra processing for Large Deductible screen
                   // If PopTwoBtn('Policies might exist that do not have a -
                   //  Large Deductible Plan. An Exception Report will be   -
                   //  Generated. Detail will be written to WXL7P in library-
                   //  PDSRPT' + %trim(suffix) + '.':
                   //  'Large Deductible Update & Exception Report':
                   //  'Submit':'Cancel');
                   //    qqrrn = %Editc(dbrrn:'X');
                   //    qqmode = 'G';
                   //    qqupd = 'Y';
                   //    UpdLargeDed(qqrrn:qqmode:qqupd);
                   // EndIf;

                   PopDataSaved();
                   DataSaved = *on;
                EndIf;
             Endif;
          EndSl;
       Enddo;

       //Subroutine to move Screen fields into database fields.
       BegSr SaveGrpDetail;
        recExist = *off;
        intdate = dtcymd(scEffDat);
        Chain (intCo#:scFnd:SCMState:intDate) prr00;

        intEmp# = 0;
        charSt = SCMState;
        intEffDt = dtcymd(scEffDat);
        Chain (intCo#:scFnd:charSt:intEmp#:intEffdt) pld04 ;
        If %found(wmldl4);
           recExist = *on;
        Endif;

        RRst   = scMstate;
        RREfDt = dtcymd(scEffDat);
        RRcccd = scCcCd;
        RRccHd = scCcHd;
        //*** Begin Add ***** 08/25/25 *****************************
        If RRccHd = ' ';
           RRccHd = 'Contractor Credit';
        EndIf;
        //*** End   Add ***** 08/25/25 *****************************
        IF scRwCc <> 'N';
           scRwCc = ' ';               // Save Blank for Y
        EndIf;
        RRrwcc = scRwCc;               // Auto Renew
        RR1acd = sc1Acd;
        RR1Ahd = sc1Ahd;
        IF scRw1a <> 'N';
           scRw1a = ' ';               // Save Blank for Y
        EndIf;
        RRrw1a = scRw1a;               // Auto Renew
        RR1bcd = sc1Bcd;
        RR1bhd = sc1Bhd;
        IF scRw1a <> 'N';
           scRw1a = ' ';               // Save Blank for Y
        EndIf;
        RRRw1b = scRw1b;               // Auto Renew
        RRcrcd = scCrCd;
        RRcrhd = scCrHd;
        IF scRw1a <> 'N';
           scRw1a = ' ';               // Save Blank for Y
        EndIf;
        RRrwdc = scRwDc;               // Auto Renew
        RRdbcd = scDbCd;
        RRdbhd = scDbHd;
        RRwvac = scwvac;
        RRwvhd = scWvHd;
        RRvlcc = scVlCc;
        RRvlch = scVlCh;
        RRdv1c = scDv1c;
        RRdv1h = scDv1h;
        IF scRwD1 <> 'N';
           scRwD1 = ' ';               // Save Blank for Y
        EndIf;
        RRrwd1 = scRwD1;               // Auto Renew
        RRdv1s = scDv1s;
        RRdv1CHG = scDv1Chg;
        RRdv1dfrnw = scDv1DfRnw;
        If RRdv1dfrnw = ' ';
           RRdv1dfrnw = 'N';
        Endif;
        RRdv1l = scDv1l;
        RRdv1dft = scDv1Dft;
        RRdv2c = scDv2c;
        RRdv2h = scDv2h;
        IF scRwD2 <> 'N';
           scRwD2 = ' ';               // Save Blank for Y
        EndIf;
        RRrwd2 = scRwD2;               // Auto Renew
        RRdv2s = scDv2s;
        RRdv2Chg = scDv2Chg;
        RRdv2dfrnw = scDv2DfRnw;
        If RRdv2dfrnw = ' ';
           RRdv2dfrnw = 'N';
        Endif;
        RRdv2l = scDv2l;
        RRdv2dft = scDv2Dft;
        RRdccd = scDcCd;
        RRamax = scAmAx;
        RRasfl = scAsFl;
        RRmrrd = scMrRd;
        RRmrrdt = scMrrDt;
        RRmrrd2 = scMrrD2;
        RRmrrc = scMrRc;
        RRmrrct = scMrrCt;
        RRmrrc2 = scMrrC2;
        RRancs = scAnCs;
        RRancm = scAnCm;
        RReccd = scEcCd;
        RRmpcd = scMpCd;
        RRmrrn = scMrRn;
        RRilmc = scIlMc;
        RRarcc = scArCc;
        RRrdpr = scRdPr;
        RRsraf = scSrAf;
        RRlcm  = scLcm ;
        RRlcex = dtCYMD(scLcEx);
        RRdv2s = scDv2s;
        RRterc = scTeRc;
        RRterr = scTeRr;
        RRter2 = scTeR2;
        RRterh = scTeRh;
        RRdtrc = scDtRc;
        RRdtrr = scDtRr;
        RRdtrh = scDtRh;
        RRmono = scMonoSt;
        RRpullsrd = scPullSrd;
        RRptfl = scPtFl;
        RRterl = scTeRl;
        RRdmax = scDMax;
        RRcmax = scCMax;
        //*** begin add ***  11/17/23  ********************************
        RRdcap = scDcap;
        //*** end add *****  11/17/23  ********************************
        RRdspr = scDsPr;
        RRdspc = scDsPc;
        RRdisc = scDiSc;
        RRdshd = scDsHd;
        RRdsin = scDsIn;
        RRdcl = scDcl;
        If RRdcl = ' ';
           RRdcl = 'N';
        Endif;
        RRMinFD = scMinFd;
        RRMinSD = scMinSd;
        RRMinSC = scMinSc;
        RRsfpc = scSfPc;
        RRsfap = scSfAp;
        RRsfhd = scSfHd;
        //*** begin add ***  09/18/23  ********************************
        RRSfCd = scSfCd;
        //*** end add *****  09/18/23  ********************************
        RRsfam = scSfAm;
        RRenid = scEnId;
        RRspfl = scSpFl;
        If RRspfl = ' ';
           RRspfl = 'N';
        Endif;
        RRyrad = scYrad;
        If RRyrad = ' ';
           RRyrad = 'N';
        Endif;
        RRcxad = scCxAd;
        If RRcxad = ' ';
           RRcxad = 'N';
        Endif;
        RRtxpc = scTxPc;
        RRtxhd = scTxHd;
        RRtdfl = scTdFl;
        If RRtdfl = ' ';
           RRtdfl = 'N';
        Endif;
        RRrdtx = scRdTx;
        If RRrdtx = ' ';
           RRrdtx = 'N';
        Endif;
        RRtefl = scTeFl;
        RRidtp = scIdTp;
        If RRidtp = ' ';
             RRidtp = 'N';
        Endif;
        RRtaxm = scTaxm;
        RRwdtp = scWdtp;
        RRwdpc = scWdpc;
        RRwdfc = scWdfc;
        RRwdmn = scWdmn;
        RRwvfl = scWvfl;
        RRPfCd = scPfCd;
        RRPfAm = scPfAm;
        RRPfhd = scPfHd;
        RRlate = scLate;
        RRltcd = scLtcd;
        RRltam = scLtam;
        RRrdev = scRdev;
        RRdrat = scDrat;
        RRbscd = scBscd;
        RRlsid = scDftLsId;
        RRsemh = scSemh;
        RRbafl = scBafl;
        RRspsg = scSpsg;
        RRurfl = scUrfl;
        RRnaic = scNAIC;
        RRrifl = scRifl;
        RRppfl = scPpfl;
        RRsrrb = scSrrb;
        RRsrrq = scSrrq;
        RRnrrb = scNrrb;
        RRnrrq = scNrrq;
        RRdbcr = scDbcr;
        RRemlb = scEmlb;
        RRceov = scCeOv;
        RRacc = scAcc;
        RRdis = scDis;
        RRagg = scAgg;
        RRExst = scExSt;
        RRPgfl = scPgFl;
        RRPeoa = scPeOa;
        RRpeof = scPeoPt;
        RRNmindft = scNmInDft;
        RRuser = q1user;
        RRpgm = q1pgm;
        RRtime = Timeto6(SystemTime());
        RRdt = Dateto7(SystemDate());
       //*** Beg Add ***  04/29/24  *** WC2965 *************************
        RREmail = scEmail;
       //*** End Add ***  04/29/24  *** WC2965 *************************

        saveDT = RRdt;
        saveTM = RRtime;

        RR1stp = R1Stp;
        RR1stf = R1Stf;
        RR1stl = R1Stl;
        RR1sth = R1Sth;
        RR1stc = R1Stc;
        RR1ilf = R1Ilf;
        RR1xfc = R1xfc;
        RR1stm = R1stm;
        RR1idv = R1Idv;
        If RR1idv = ' ';
           RR1idv = 'N';
        Endif;
        RR1stb = R1Stb;
        RR1atd = R1Atd;
        RR1idd = R1Idd;
        If RR1idd = ' ';
           RR1idd = 'N';
        Endif;
        RR1atr = R1Atr;
        RR2ndp = R2Stp;
        RR2ndf = R2Stf;
        RR2ndl = R2Stl;
        RR2ndh = R2Sth;
        RR2ndc = R2Stc;
        RR2ilf = R2Ilf;
        RR2xfc = R2xfc;
        RR2ndb = R2stb;
        RR2idd = R2Idd;
        If RR2idd = ' ';
           RR2idd = 'N';
        Endif;
        RR2ndm = R2Stm;
        RR2atd = R2Atd;
        RR2atr = R2Atr;
        RR2idv = R2Idv;
        If RR2idv = ' ';
           RR2idv = 'N';
        Endif;
        RR3rdp = R3Stp;
        RR3rdf = R3Stf;
        RR3rdl = R3Stl;
        RR3rdh = R3Sth;
        RR3rdc = R3Stc;
        RR3ilf = R3Ilf;
        RR3xfc = R3Xfc;
        RR3rdb = R3Stb;
        RR3idd = R3Idd;
        If RR3idd = ' ';
           RR3idd = 'N';
        Endif;
        RR3rdm = R3Stm;
        RR3atd = R3Atd;
        RR3atr = R3Atr;
        RR3idv = R3Idv;
        If RR3idv = ' ';
           RR3idv = 'N';
        Endif;
        RR4thp = R4Stp;
        RR4thf = R4Stf;
        RR4thl = R4Stl;
        RR4thh = R4Sth;
        RR4thc = R4Stc;
        RR4ilf = R4Ilf;
        RR4xfc = R4Xfc;
        RR4thb = R4Stb;
        RR4idd = R4Idd;
        If RR4idd = ' ';
           RR4idd = 'N';
        Endif;
        RR4thm = R4STm;
        RR4atd = R4Atd;
        RR4atr = R4Atr;
        RR4idv = R4Idv;
        If RR4idv = ' ';
           RR4idv = 'N';
        Endif;
        RR5thp = R5Stp;
        RR5thf = R5Stf;
        RR5thl = R5Stl;
        RR5thh = R5Sth;
        RR5thc = R5Stc;
        RR5ilf = R5Ilf;
        RR5xfc = R5Xfc;
        RR5thb = R5Stb;
        RR5idd = R5Idd;
        If RR5idd = ' ';
           RR5idd = 'N';
        Endif;
        RR5thm = R5STm;
        RR5atd = R5Atd;
        RR5atr = R5Atr;
        RR5idv = R5Idv;
        If RR5idv = ' ';
           RR5idv = 'N';
        Endif;
        RR6thp = R6Stp;
        RR6thf = R6Stf;
        RR6thl = R6Stl;
        RR6thh = R6Sth;
        RR6thc = R6Stc;
        RR6ilf = R6Ilf;
        RR6xfc = R6Xfc;
        RR6thb = R6Stb;
        RR6idd = R6Idd;
        If RR6idd = ' ';
           RR6idd = 'N';
        Endif;
        RR6thm = R6Stm;
        RR6atd = R6Atd;
        RR6atr = R6Atr;
        RR6idv = R6Idv;
        If RR6idv = ' ';
           RR6idv = 'N';
        Endif;
        RR7thp = R7Stp;
        RR7thf = R7Stf;
        RR7thl = R7Stl;
        RR7thh = R7Sth;
        RR7thc = R7Stc;
        RR7ilf = R7Ilf;
        RR7xfc = R7Xfc;
        RR7thb = R7Stb;
        RR7idd = R7Idd;
        If RR7idd = ' ';
           RR7idd = 'N';
        Endif;
        RR7thm = R7Stm;
        RR7atd = R7Atd;
        RR7atr = R7Atr;
        RR7idv = R7Idv;
        If RR7idv = ' ';
           RR7idv = 'N';
        Endif;

        // Large Deductible file (WMLDP)
        LDdes1 = scLdDes1;
        LDpct1 = scLdPct1;
        LDtyp1 = scLdTyp1;
        LDamp1 = scLdAmp1;
        LDamg1 = scLdAmg1;
        LDvrb1 = scLdVrb1;
        LDocv1 = scLdOcv1;
        LDcls1 = scLdCls1;
        LDplv1 = scLdPlv1;
        LDpln1 = scLdPln1;
        LDtot1 = scLdTot1;
        LDala1 = scLdAla1;
        LDrnw1 = scLdRnw1;
        LDr101 = scLdR101;
        LDr102 = scLdR102;
        LDr103 = scLdR103;
        LDr104 = scLdR104;
        LDr105 = scLdR105;
        LDr106 = scLdR106;
        LDr107 = scLdR107;
        LDr108 = scLdR108;
        LDr109 = scLdR109;
        LDr110 = scLdR110;
        LDadp1 = scAdP1;
        LDvarA1=  scVarA1;
        LDuser = q1user;
        LDpgm = q1pgm;
        LDtime = Timeto6(SystemTime());
        LDdt = Dateto7(SystemDate());
        //*** Beg Add ***** 11/26/24 ****************************************
        LDMegaT = scMegaT;
        //*** End Add ***** 11/26/24 ****************************************

        If %found(wdrrl);
           Update prr00;
        Else;
           RRco# = intCo#;
           RRfnd = scFnd;
           RRiusr  = q1user;
           RRipgm  = q1pgm;
           RRidt   = Dateto7(SystemDate());
           RRitim  = Timeto6(SystemTime());
           Write  prr0p;
           DataWrite = *on;
        Endif;

        // Large Deductible file (WMLDP)
        If recExist = *on;
           Update pld04;
        Endif;
        If recExist = *off and scLdDes1 <> *Blanks;
           LDco# = RRco#;
           LDfnd = RRfnd;
           LDst = scMstate;
           LDefdt = dtcymd(scEffDat);
           LDiusr = q1user;
           LDipgm = q1pgm;
           LDitim = Timeto6(SystemTime());
           LDidt = Dateto7(SystemDate());
           Write  pld04;

           //saveDT2 = LDdt;
           //saveTM2 = LDtime;
        Endif;
        If recExist = *off and scLdDes1 = *Blanks;
           If CopyMode = *on and DataSaved = *off;      // Copy Mode
              Chain(n) (intCo#:scFnd:svst:intEmp#:svefdt) pld04;
              If %found(wmldl4);
                 LDst = scMstate;
                 LDefdt = dtcymd(scEffDat);
                 LDiusr = q1user;
                 LDipgm = q1pgm;
                 LDitim = Timeto6(SystemTime());
                 LDidt = Dateto7(SystemDate());
                 Write  pld04;

                 //saveDT2 = LDdt;
                 //saveTM2 = LDtime;
              Endif;
           Endif;
        Endif;
        Unlock wdrrl;
        Unlock wmldl4;

        // Carrier Rating Default
        If CopyMode = *on and DataSaved = *off;      // Copy Mode
           // Copy Carrier Rating Default records for Connect
           //  if factor/fee/discount is set up for the new record
           Setll (intCo#:scFnd:svst:svefdt) pdf00;
           Dou %Eof(WIRATDFTL);
              Reade (intCo#:scFnd:svst:svefdt) pdf00;
              If %Eof;
                 Leave;
              Endif;
              // Ensure new WDRRP record has set up for the default, but
              //  don't check the std limits default record
              If DFpgm <> 'WICARRAT' And
               DefaultNotSetUp(DFkey) = *On;
                 Iter;
              Endif;
              DFefdt = RRefdt;
              DFst = RRst;
             // Standard Limits record
              If DFpgm = 'WICARRAT' And
               (RRdis <> 0 Or RRacc <> 0 or RRagg <> 0);
                 Write pdf00;
                 Iter;
              Endif;
              DFuser = Q1user;
              DFpgm  = Q1pgm;
              DFdt   = RRdt;
              DFtime = RRtime;
              DFiusr = Q1user;
              DFipgm = Q1pgm;
              DFidt  = RRdt;
              DFitim = RRitim;
              Write pdf00;
           Enddo;
           // Copy Carrier Rating Default Detail records for Connect
           //  if factor/fee/discount is set up for the new record
           Setll (intCo#:scFnd:svst:svefdt) pdt00;
           Dou %Eof(WIRATDDTL);
              Reade (intCo#:scFnd:svst:svefdt) pdt00;
              If %Eof;
                 Leave;
              Endif;
              // Don't check std limits default record
              If DTpgm <> 'WICARRAT' And
               DefaultNotSetUp(DTkey) = *On;
                 Iter;
              Endif;
              DTefdt = RRefdt;
              DTst = RRst;
              Select;
              When DTpgm = 'WICARRAT' And
               (RRdis <> 0 Or RRacc <> 0 or RRagg <> 0);
                // Standard Limits record
                 DTdesc = RRemlb;
                 Write pdt00;
                 Iter;
              When DTkey = 'INCLIMITS';
                 If DefaultNotSetUp(DTkey:DTentry) = *On;
                    Iter;
                 Endif;
              When DTkey = 'DEDUCT';
                 If DefaultNotSetUp(DTkey:DTentry) = *On;
                    Iter;
                 Endif;
              Endsl;
              DTuser = Q1user;
              DTpgm  = Q1pgm;
              DTdt   = RRdt;
              DTtime = RRtime;
              DTiusr = Q1user;
              DTipgm = Q1pgm;
              DTidt  = RRdt;
              DTitim = RRitim;
              Write pdt00;
           Enddo;
        Endif;
       EndSr;


       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        // Disable State and Effective Date
        If dspMode = *on or chgMode = *on or        // Display and Change Mode
         (CopyMode = *on and DataSaved = *on);      // Copy Mode
           isNotAdd = *on;
        Endif;

        // Read WDRRP file and load all tabs
        // Do not re-load any tab when there is any Error   OR
        // User just clicked another Tab
        If error = *off and error1 = *off and error3 = *off and
         error4 = *off and flgRcdUpdated = '0' and tabClick = *off;
           charSt = scMstate;
           intEffDt = dtcymd(scEffDat);
           Chain(n) (intCo#:scFnd:charSt:intEffDt) prr00;

           // Load Tab 1
           Clear RcdPrmInfo;
           FillPrmInfo();

           // Load Tab 2
           Clear RcdFees;
           FillFeesInfo();

           // Load Tab 3
           Clear RcdTaxes;
           LoadTaxesInfo();

           // Load Tab 4
           Clear RcdLrgDed;
           FillDedInfo();

           // Load Tab 6
           Clear RcdGen;
           FillGenInfo();
        Endif;

        tabClick = *off;              // Tab Click - Turn Off

        Select;
        When arrTabNames(TabIndex+1) = 'Premium Algorithm';
           pnlHeight = 840;
           APnlHeight = pnlHeight + 60;

           //If error = *off and flgRcdUpdated = '0';
           //   Clear RcdPrmInfo;
           //   FillPrmInfo();
           //Endif;

           // Inquiry Mode
           If dspMode = *on;
              inquiry_SR = *on;
           Endif;

           // Visibility of Monopolisitc State Checkbox
           isAmWins = *Off;
           If clientID = '148';
              isAmWins = *On;
           Endif;

           @@cflg = '1';           // Just like old program W0014R02
           //If clientID = '132';
           //   @@cflg = '1';
           //Endif;

           // Set Selection Criteria of Default Stock Discount Table Code
           clear intEffDt;
           intEffDt = dtCYMD(scEffDat);
           dsChoices = 'DSCO# = ' + %char(intCO#) + ' AND DSFND = '
            + %char(scFND) + ' AND DSST = ''' + charSt
            + ''' AND DSEFDT <= ' + %char(intEffDT);

           Write rcdGrpPnl;
           Write RcdPanel;

           Exfmt RcdPrmInfo;

           Read RcdPanel;
           Read rcdGrpPnl;

        When arrTabNames(TabIndex+1) = 'Fees';

           pnlHeight = 500;
           APnlHeight = pnlHeight + 60;

           //If error1 = *off and flgRcdUpdated = '0';
           //   Clear RcdFees;
           //   FillFeesInfo();
           //Endif;

           // Inquiry Mode
           If dspMode = *on;
              inquiry_SR = *on;
           Endif;

           // Visibility of Policy Fee Scetion (3 fields)
           isAmWins = *Off;
           If clientID = '148';
              isAmWins = *On;
           Endif;

           // Visibility of Include in Targeted Premium Checkbox
           vis150 = *Off;
           If clientID = '150';
              vis150 = *On;
           Endif;

           // Visibility of Waiver Surcharge Scetion (5 fields)
           If scwvac <> 0;
              isWavSur = *on;
           Else;
              isWavSur = *off;
           Endif;

           // Visibility of Payroll Rate Deviation
           visRdev = *off;
           If clientID = '129';
              visRdev = *on;
           Endif;

           Write rcdGrpPnl;
           Write RcdPanel;
           Exfmt RcdFees;

           Read RcdPanel;
           Read rcdGrpPnl;

        When arrTabNames(TabIndex+1) = 'Taxes and Assessments';

           pnlHeight = 500;
           APnlHeight = pnlHeight + 60;

           //If error2 = *off;
           //   Clear RcdTaxes;

           //   visInq = *off;
           //  If dspMode = *on;
           //     visInq = *on;
           //  Endif;

           //  LoadTaxesInfo();
           //Endif;

           visInq = *off;
           If dspMode = *on;
              visInq = *on;
           Endif;

           // Visibility of Include in Targeted Premium Checkbox
           visIncTrgP = *off;
           If clientid = '150';
              visIncTrgP = *on;
           Endif;

           Write rcdGrpPnl;
           Write RcdPanel;
           Exfmt RcdTaxes;

           Read RcdPanel;
           Read RcdGrpPnl;

           Dou %eof;
              Readc sflTaxAss;
              Select;
              When %Eof;
                 Iter;
              When isDltTax = *On;
                 isDltTax = *Off;
                 svRrnTax = scRrnTxAs;      //Save Rrn value for deleted record
                 If PopTwoBtn('Please confirm Deletion.':
                 'Tax and Assessment Info':'Delete':'Cancel':300:150);
                    DelTax  = *off;
                    DelTaxInfo(scRrnTxAs);
                 Endif;
              When isEditTax = *On;
                 isEditTax = *Off;
                 svRrnTax = scRrnTxAs;      //Save Rrn value for edited record
                 isCont = *off;
                 isSave = *on;
                 Taxinfo = *off;
                 savTaxInfo = *off;
                 selIncTerr= 'GTCO# =  ' + %char(intCO#) +
                  ' AND GTTBID = ''TERA'' AND GTSTAT = ''A''';

                 EditTaxInfo(scRrnTxAs);

              Endsl;
           Enddo;

           Select;
           When btnEdtAll = *on;
              btnEdtAll = *off;
              isCont = *on;
              isSave = *off;
              Taxinfo = *off;
              savTaxInfo = *off;
              selIncTerr= 'GTCO# =  ' + %char(intCO#) +
               ' AND GTTBID = ''TERA'' AND GTSTAT = ''A''';

              i = 1;
              Dow (i<=7);
                 RrnTxAs = i;
                 EditTaxInfo(RrnTxAs);
                 i = i + 1;
                 If Taxinfo = *on;
                    Leave;
                 Endif;
              Enddo;

           When BtnDltAll = *on;
              BtnDltAll = *off;
              If PopTwoBtn('Please confirm Deletion of all.':
              'Tax and Assessment Info':'Delete':'Cancel':300:150);
                 i = 1;
                 DelTax = *off;
                 Dow (i <= 7);
                    rrnTxAs = i;
                    DelTaxInfo(rrnTxAs);
                    i = i + 1;
                 Enddo;
              Endif;
           Endsl;

           If savTaxInfo = *on or DelTax = *on;
              intdate = dtcymd(scEffDat);
              Chain (intCo#:scFnd:SCMState:intDate) prr00;
              If %found(wdrrl);
                 RR1stp = R1Stp;
                 RR1stf = R1Stf;
                 RR1stl = R1Stl;
                 RR1sth = R1Sth;
                 RR1stc = R1Stc;
                 RR1ilf = R1Ilf;
                 RR1xfc = R1xfc;
                 RR1stm = R1stm;
                 RR1idv = R1Idv;
                 RR1stb = R1Stb;
                 RR1atd = R1Atd;
                 RR1idd = R1Idd;
                 RR1atr = R1Atr;
                 RR2ndp = R2Stp;
                 RR2ndf = R2Stf;
                 RR2ndl = R2Stl;
                 RR2ndh = R2Sth;
                 RR2ndc = R2Stc;
                 RR2ilf = R2Ilf;
                 RR2xfc = R2xfc;
                 RR2ndb = R2stb;
                 RR2idd = R2Idd;
                 RR2ndm = R2Stm;
                 RR2atd = R2Atd;
                 RR2atr = R2Atr;
                 RR2idv = R2Idv;
                 RR3rdp = R3Stp;
                 RR3rdf = R3Stf;
                 RR3rdl = R3Stl;
                 RR3rdh = R3Sth;
                 RR3rdc = R3Stc;
                 RR3ilf = R3Ilf;
                 RR3xfc = R3Xfc;
                 RR3rdb = R3Stb;
                 RR3idd = R3Idd;
                 RR3rdm = R3Stm;
                 RR3atd = R3Atd;
                 RR3atr = R3Atr;
                 RR3idv = R3Idv;
                 RR4thp = R4Stp;
                 RR4thf = R4Stf;
                 RR4thl = R4Stl;
                 RR4thh = R4Sth;
                 RR4thc = R4Stc;
                 RR4ilf = R4Ilf;
                 RR4xfc = R4Xfc;
                 RR4thb = R4Stb;
                 RR4idd = R4Idd;
                 RR4thm = R4Stm;
                 RR4atd = R4Atd;
                 RR4atr = R4Atr;
                 RR4idv = R4Idv;
                 RR5thp = R5Stp;
                 RR5thf = R5Stf;
                 RR5thl = R5Stl;
                 RR5thh = R5Sth;
                 RR5thc = R5Stc;
                 RR5ilf = R5Ilf;
                 RR5xfc = R5Xfc;
                 RR5thb = R5Stb;
                 RR5idd = R5Idd;
                 RR5thm = R5Stm;
                 RR5atd = R5Atd;
                 RR5atr = R5Atr;
                 RR5idv = R5Idv;
                 RR6thp = R6Stp;
                 RR6thf = R6Stf;
                 RR6thl = R6Stl;
                 RR6thh = R6Sth;
                 RR6thc = R6Stc;
                 RR6ilf = R6Ilf;
                 RR6xfc = R6Xfc;
                 RR6thb = R6Stb;
                 RR6idd = R6Idd;
                 RR6thm = R6Stm;
                 RR6atd = R6Atd;
                 RR6atr = R6Atr;
                 RR6idv = R6Idv;
                 RR7thp = R7Stp;
                 RR7thf = R7Stf;
                 RR7thl = R7Stl;
                 RR7thh = R7Sth;
                 RR7thc = R7Stc;
                 RR7ilf = R7Ilf;
                 RR7xfc = R7Xfc;
                 RR7thb = R7Stb;
                 RR7idd = R7Idd;
                 RR7thm = R7Stm;
                 RR7atd = R7Atd;
                 RR7atr = R7Atr;
                 RR7idv = R7Idv;
                 RRuser = q1user;
                 RRpgm = q1pgm;
                 RRtime = Timeto6(SystemTime());
                 RRdt = Dateto7(SystemDate());

                 saveDT = RRdt;
                 saveTM = RRtime;

                 Update prr00;
              Endif;
              Unlock wdrrl;
           Endif;

        When arrTabNames(TabIndex+1) = 'Large Deductible';
           //*** Beg Chg ***** 11/26/24 *****************************************
           //*** pnlHeight = 500;
           pnlHeight = 520;
           //*** End Chg ***** 11/26/24 *****************************************
           APnlHeight = pnlHeight + 60;

           //If error3 = *off and flgRcdUpdated = '0';
           //   Clear RcdLrgDed;
           //   FillDedInfo();
           //Endif;

           // Inquiry Mode
           If dspMode = *on;
              inquiry_D =  *on;
           Endif;

           // Visibility of Variable Checkbox and Aggregate %
           VisibAggrP = *off;
           VisibVarA = *off;
           If clientid = '150';
              VisibAggrP = *on;
              VisibVarA = *on;
           Endif;

           //*** Beg Add ***** 11/26/24 ****************************************
           // Visibility of Monopolisitc State Checkbox
           isAmWins = *Off;
           If clientID = '148';
              isAmWins = *On;
           Endif;
           //*** Beg Add ***** 11/26/24 ****************************************

           // Set Selection Criteria for Reserve Code fields
           Clear rtChoices;
           rtChoices = 'RTCO# = ' + %char(intCO#) + ' AND RTSTAT <> ''I''';

           // Set Selection Criteria for Deductible Plan Type
           Clear ldPlnChoic;
           ldPlnChoic = 'GTCO# = ' + %char(intCO#) + ' AND GTTBID = ''  DT'''
            + ' AND GTSTAT = ''A''';

           // Set Selection Criteria for Deductible Plan Type
           Clear LdPlvChoic;
           LdPlvChoic = 'GTCO# = ' + %char(intCO#) + ' AND GTTBID = ''BCLV'''
            + ' AND GTSTAT = ''A''';

           Write RcdGrpPnl;
           Write RcdPanel;

           Exfmt RcdLrgDed;

           Read RcdPanel;
           Read rcdGrpPnl;

        When arrTabNames(TabIndex+1) = 'Connect';
           //*** Beg Chg *** 06/02/23 ******************************************
           //*** pnlHeight = 680;
           pnlHeight = 875;
           //*** End Chg *** 06/02/23 ******************************************
           APnlHeight = pnlHeight + 60;
           isConnect = *on;

           If isconnect = *on;
              scConnect = 'T';
              Reset dsWITMPLTparms;
              intdate = dtcymd(scEffDat);
              Chain(n) (intCo#:scFnd:SCMState:intDate) prr00;
              If %found(wdrrl);
                 dsWITMPLTparms.co# = RRCo#;
                 dsWITMPLTparms.fnd = RRFnd;
                 dsWITMPLTparms.state = RRSt;
              Endif;

              If @@mode = 'A' and DataWrite <> *on;
                 PopOneBtn('Rating and Statistics Record not found.+
                  Please enter Rating and Statistics Info.':
                  'Error':'OK':500:300);
                 TabIndex = 0;
                 Leavesr;
              Endif;

              Write RcdGrpPnl;
              Write RcdPanel;

              Proc_Tmplt(dsWITMPLTparms:'Group/State':@@Mode);

              Read RcdPanel;
              Read rcdGrpPnl;

              Dow tabindex = 4;
                 Select;
                 When scConnect = 'T';
                    Write RcdGrpPnl;
                    Write RcdPanel;
                    Proc_Tmplt(dsWITMPLTparms:'Group/State':@@Mode);
                    Read RcdPanel;
                    Read rcdGrpPnl;

                 When scConnect = 'C';
                    Write RcdGrpPnl;
                    Write RcdPanel;
                    decRRN1 = %int(dbrrn);
                    Proc_RatDft(decRrn1:@@Mode);
                    Read RcdPanel;
                    Read rcdGrpPnl;
                 Endsl;
              Enddo;
           Endif;

           If isconnect = *off;
              Write RcdGrpPnl;
              Write RcdPanel;
              Write RcdConnect;
              Read RcdPanel;
              Read rcdGrpPnl;
           Endif;

        When arrTabNames(TabIndex+1) = 'General';
           pnlHeight = 500;
           APnlHeight = pnlHeight + 60;

           //If error4 = *off and flgRcdUpdated = '0';
           //   Clear RcdGen;
           //   FillGenInfo();
           //Endif;

           // Inquiry Mode
           If dspMode = *on;
              inquiry_SR = *on;
           Endif;

           // Get FNpeof from WMFNL file
           Chain (RRco#:RRfnd) pfn00;

           // Visibility of PEO Output Control Section (2 fields)
           visPEO = *off;
           If FNpeof <> *blanks;
              visPeo = *on;
           Endif;

           // Visibility of PEO Policy Type
           visPeoTyp = *off;
           If FNpeof = 'C';
              visPeoTyp = *on;
           Endif;

           // Visibility of Overseeing PEO Named Insured
           // Set Selection Criteria for Overseeing PEO Named Insured
           is150PEO = *Off;
           If clientid = '150' and FNpeof <> *Blank and FNpeof <> 'S';
              is150PEO = *On;
              scNmInSlct = 'ANCO# = ' + %CHAR(FNCO#) +
               ' AND ANFND = ' + %CHAR(FNFND) +
               ' AND ANEMP# = 1 AND ANDIV = 0 AND ANSTAT = ''A''';
           Endif;

           // Visibility of Provide for Scheduled Debit/Credit Tracking/Allow Form Print
           visFL53 = *off;
           xsKey = 'WC';
           Chain xsKey swc01;
           If %Found(ssylf);
              If WCFL53 = 'Y';
                 visFL53 = *On;
              EndIf;
           EndIf;

           // Visibility of Exclude State from Connect Checkbox
           hasConnect = IsModEnabled('Portal Module');

           // Set Selection Criteria for Default Rate Set Code
           rteChoices = 'RSCO# = ' + %char(intCO#) + ' AND RSSTAT = ''A''';

           // Set Selection Criteria for Default Leasing ID
           scLsIDSel = 'GTCO# =  ' + %char(intCO#) +
            ' AND GTTBID = ''NREL'' AND GTSTAT = ''A''';

       //*** Beg Add ***  04/29/24  *** WC2965 *************************
           ScMailPos = 915;

           If visFL53 = *off;
              ScMailPos = 475;
           EndIf;

           If visFL53 = *off and hasConnect = *Off;
              ScMailPos = 260;
           EndIf;

       //*** Beg Add ***  04/29/24  *** WC2965 *************************

           Write RcdGrpPnl;
           Write RcdPanel;

           Exfmt RcdGen;

           Read RcdPanel;
           Read rcdGrpPnl;

        EndSl;

       Endsr;
      /End-Free
     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n

      /Free
       // Program Mode.
       If q1prms >= 2;
          //*** Beg Add ***** 01/17/22 *****************************
          scActMode = @@mode;
          //*** End Add ***** 01/17/22 *****************************
          Select;
          When @@mode = 'D';
             dspMode = *on;
             isNotAdd = *on;
          When @@mode = 'C';
             chgMode = *on;
             isNotAdd = *on;
          When @@mode = 'A';
             AddMode = *on;
             isNotAdd = *off;
          When @@mode = 'X';
             CopyMode = *on;
             isNotAdd = *off;
          Endsl;
       Endif;

       isConnect = *off;
       DataSaved = *off;

       // Set the initial tab
       If %parms() > 3;
          tabIndex = %Dec(@@TabIndex:5:0);
       Else;
          tabIndex = 0;
       EndIf;

       // Intialize the Screen fields
       LoadInVal();

       GetClientID(clientID);

       GetCltSuffix(suffix);

     // Update Action Panel Info (Rrn, Group Name, Group Number)
       // Get FNname (Group Name) from WMFNL file
       Chain (intCo#:scFnd) pfn00;
       //*** Beg Chg ***** 01/17/22 ************************************
       //scActRrn = xxRrn;
       scActRrn = dbrrn3;
       //*** End Chg ***** 01/17/22 ************************************
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;
       If scCompName = ' ';
          scCompName = GetCompNam(userCo#(q1user));
       Endif;

       processMe = *on;

       If AddMode = *on and dataWrite = *off;
          visPanel = *off;
       Else;
          visPanel = *on;
       Endif;

       strTabNames += AddToList('Premium Algorithm':arrTabNames:n1);
       strTabNames += AddToList('Fees':arrTabNames:n1);
       strTabNames += AddToList('Taxes and Assessments':arrTabNames:n1);
       strTabNames += AddToList('Large Deductible':arrTabNames:n1);
       If IsModEnabled('Portal Module');
          strTabNames += AddToList('Connect':arrTabNames:n1);
       EndIf;
       strTabNames += AddToList('General':arrTabNames:n1);
       TbNmRcdPnl = strTabNames;

       Return;

      /End-Free
     p ProgramStart    e

       //===================================================================
       // Validate the Large Deductible Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidDedInfo    b
     d ValidDedInfo    pi              n
     d Error3          s               n   inz
      /Free
       error3 = *off;
       erLdDeS1 = *off;
       msLdDeS1 = *blanks;
       erLdTyp1 = *off;
       msLdTyp1 = *blanks;
       erLdVrB1 = *off;
       msLdVrB1 = *blanks;
       erLdCls1 = *off;
       msLdCls1 = *blanks;
       erLDPlv1 = *off;
       msLDPlv1 = *blanks;
       erLDR101 = *off;
       msLDR101 = *blanks;

       //Validate Gross/Net Flag
       If scLdDeS1 <> ' ' and scLdTyp1 = *blanks;
          erLdTyp1 = *on;
          msLdTyp1 = 'WCR0503';
          error3 = *on;
       Endif;

       //Validate Per Claim/Per Occurrence Flag
       If scLDdes1 <> ' ' and scLDvrb1 = *blanks;
          erLdVrB1 = *on;
          msLdVrB1 = 'WCR0503';
          error3 = *on;
       Endif;

       //Validate Statistical Class Code
       xsKey = 'WC';
       Chain xsKey swc01;
       If WcFl03 = 'Y';
          If scLDdes1 <> ' ';
             If scLdCls1 = 0;
                erLdCls1 = *on;
                msLdCls1 = 'WCR0503';
                error3 = *on;
             Endif;
          Endif;
       Endif;

       //Validate Billing Calculation Level
       If scLDdes1 <> ' ' and scLdPcT1 <> 0 and scLDPlv1 = *Blanks;
          erLDPlv1 = *on;
          msLDPlv1 = 'WCR1087';
          error3 = *on;
       Endif;

       //Reserve Hiearchy at least one Reserve Code is required
       If scLDdes1 <> ' ';
          If scLdR101 = *blanks and scLdR102 = *blanks and
           scLdR103 = *blanks and scLdR104 = *blanks and
           scLdR105 = *blanks and scLdR106 = *blanks and
           scLdR107 = *blanks and scLdR108 = *blanks and
           scLdR109 = *blanks and scLdR110 = *blanks;
             erLDR101 = *on;
             msLDR101 = 'WC40029';
             error3 = *on;
          Endif;
       Endif;

       //Description 1 is required, If any of the field has value
       If error3 = *off and scLdDeS1 = *blanks;
          If scLdTyp1 <> ' ' or scLdPcT1 <> 0 or scLdAmP1 <> 0 or
           scLdAmG1 <> 0 or scAdP1 <> 0 or SCVARA1 <> ' ' or
           scLdPln1 <> ' ' or scldvrb1 <> ' ' or scLdCls1 <> 0 or
           scldocv1 <> ' ' or SCLDTOT1 <> ' ' or scLdPlv1 <> ' ' or
           scLdR101 <> ' ' or scLdR102 <> ' ' or scLdR103 <> ' ' or
           scLdR104 <> ' ' or scLdR105 <> ' ' or scLdR106 <> ' ' or
           scLdR107 <> ' ' or scLdR108 <> ' ' or scLdR109 <> ' ' or
           //*** Beg Add ***** 11/26/24 ************************************
           scMegaT <> 0 or
           //*** End Add ***** 11/26/24 ************************************
           scLdR110 <> ' ' or scLdAla1 <> ' ' or scLdRnw1 <> ' ';
             erLdDeS1 = *on;
             msLdDeS1 = 'WCR0503';
             error3 = *on;
          Endif;
       Endif;

        If Error3 = *on;
           Return *off;
        Else;
           Return *on;
        Endif;

      /End-Free
     p ValidDedInfo    e

       //===================================================================
       // Validate the Fees Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidFeesInfo   b
     d ValidFeesInfo   pi              n
     d isError1        s               n   inz
      /Free
       isError1 = *off;
       // Initialize all the messageId and error indicators.
       erSfAp = *off;
       msSfAp = *blanks;
       erWdFc = *off;
       msWdFc = *blanks;
       erWDPC = *off;
       msWDPC = *blanks;
       erWvFl = *off;
       msWvFl = *blanks;
       erLtCd = *off;
       msLtCd = *blanks;
       erLtAm = *off;
       msLtAm = *blanks;
       erRdev = *off;
       msRdev = *blanks;
       //*** begin add ***  09/18/23  ********************************
       erSfcd = *off;
       msSfcd = *blanks;
       //*** end add *****  09/18/23  ********************************

       //Validate Safety Fees Applied To
       If scSfPc <> 0 and scSfAp = *Blanks;
          erSfAp = *on;
          msSfAp = 'WC30395';
          isError1 = *on;
       Endif;
       //*** begin add ***  09/18/23  ********************************
       If scSfcd < 0;
          erSfcd = *on;
          msSfcd = 'Stat code cannot be less than zero.';
          isError1 = *on;
       Endif;
       //*** end add *****  09/18/23  ********************************

       //Waiver Flat Charge and Waiver Percentage are Mutually Exclusive
       If scWdPc <> 0 and scWdFc <> 0;
          erWdPc = *on;
          msWdPc = 'WCR0590';
          erWdFc = *on;
          msWdFc = 'WCR0590';
          isError1 = *on;
       Endif;

       //Validate Waiver Percentage and Flat Charge
       //Both fields can't Blank if the waiver type is Blanket 'B'
       If scWdTp = 'B' and scWdPc = 0 and scWdFc = 0;
          erWdPc = *on;
          msWdPc = 'WC30192';
          erWdFc = *on;
          msWdFc = 'WC30192';
          isError1 = *on;
       Endif;

       //Validate Waiver Premium Level
       If scWdTp <> *Blanks and scWvFl = *Blanks;
          erWvFl = *on;
          msWvFl = 'WC30189';
          isError1 = *on;
       Endif;

       //Validate A/R Code and Fee Amount
       If scLate = 'Y';
          If scLtCd = *blanks;
             erLtCd = *on;
             msLtCd = 'WCR0503';
             isError1 = *on;
          Endif;
          If scLtAm = 0;
             erLtAm = *on;
             msLtAm = 'WCR0502';
             isError1 = *on;
          Endif;
       Else;
          If scLtCd <> *blanks;
             erLtCd = *on;
             msLtCd = 'WCR0450';
             isError1 = *on;
          Endif;
          If scLtAm <> 0;
             erLtAm = *on;
             msLtAm = 'WCR0450';
             isError1 = *on;
          Endif;

       Endif;

       //Validate Payroll Rate Deviation
       If scRdev > 100;
          erRdev = *on;
          msRdev = 'WCR0795';
          isError1 = *on;
       Endif;

       // Return *on indicator when No error exists.
       If isError1 = *on;
          Return *off;
       Else;
          Return *on;
       Endif;

      /End-Free
     p ValidFeesInfo   e

       //===================================================================
       // Validate the General Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidGenInfo    b
     d ValidGenInfo    pi              n
     d isError4        s               n   inz
      /Free
       isError4 = *off;
       // Initialize all the messageId and error indicators.
       erBsCd = *off;
       msBsCd = *blanks;
       erSpSg = *off;
       mgSpSg = *blanks;
       erceov = *off;
       mgceov = *blanks;
       erPEOA = *off;
       mgPEOA = *blanks;

       // Validation Check for Billing Schedule Code
       If scBsCd <> 0;
          xxKey = %Editc(scBsCd:'X');
          Chain (intCo#:xxKey) pbs01;
          If not %found or BSstat <> 'A';
             erBsCd = *on;
             isError = *on;
             msBsCd = 'WCR1401';
          Endif;
       Endif;

       //Validate Signature Page Segment
       If scSpSg <> ' ';
          outName = scspsg;
          outType = '*PAGSEG';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd = '1';
             isError4 = *on;
             erSpSg = *on;
             mgSpSg = 'Page segment not found on system';
          Endif;
       Endif;

       //Validate Certification of Insurance Overlay
       If scceov <> ' ';
          outName = scCeOv;
          outType = '*OVL   ';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd = '1';
             isError4 = *on;
             erceov = *on;
             mgceov = 'Overlay Object not Found on System.';
          Endif;
       Endif;
       If scceov <> ' ' and erceov = *off;
          charTbid = 'COVL';
          Chain (intco#:charTbid:scceov) pgt00;
          If not %found(Wtgtl);
             erceov = *On;
             mgceov = 'Overlay not Listed in System Table' +
                      ' ''COVL'' - Certificate of Insurance Overlays';
             isError4 = *on;
          Endif;
       Endif;

       //Validate PEO Acronym for Output Control
       If (scPgfl = '3' or scPgfl = ' ') and scPEOA = ' '
          and rteChoices <> *Blanks;            // User has visited General Tab
          erPEOA = *on;
          mgPEOA = 'An Acronym must be entered for Output Control 3 or blank.';
          isError4 = *on;
       Endif;

       // Return *on indicator when No error exists.
        If isError4 = *on;
           Return *off;
        Else;
           Return *on;
        Endif;

      /End-Free
     p ValidGenInfo    e

       //===================================================================
       // Validate the Premium Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidPrmInfo    b
     d ValidPrmInfo    pi              n
     d isError         s               n   inz
      /Free
       isError = *off;
       // Initialize all the messageId and error indicators.
       erMState = *off;
       msMState = *blanks;
       erSrAf = *off;
       msSrAf = *blanks;
       ermpcd = *off;
       msmpcd = *blanks;
       erRdPr = *off;
       msRdPr = *blanks;
       erECCD = *off;
       msECCD = *blanks;
       er1Ahd = *off;
       ms1Ahd = *blanks;
       er1Bhd = *off;
       ms1Bhd = *blanks;
       erCrHd = *off;
       msCrHd = *blanks;
       erDbHd = *off;
       msDbHd = *blanks;
       erWvHd = *off;
       msWvHd = *off;
       erVlCh = *off;
       msVlCh = *blanks;
       erDv1h = *off;
       msDv1h = *blanks;
       erDv1l = *off;
       msDv1l = *blanks;
       erDv2h = *off;
       msDv2h = *blanks;
       erDv2l = *off;
       msDv2l = *blanks;
       erCccd = *off;
       msCccd = *blanks;
       erCchd = *off;
       msCchd = *blanks;
       erAsFl = *off;
       msAsFl = *blanks;
       erMrRdt = *off;
       mgMrRdt = *blanks;
       erMrRd2 = *off;
       mgMrRd2 = *blanks;
       erDisc = *off;
       msDisc = *blanks;
       erAnCm = *off;
       msAnCm = *blanks;
       erLcEx = *off;
       msLcEx = *blanks;
       erDsPc = *off;
       msDsPc = *blanks;
       erDsPR = *off;
       MSDsPR = *blanks;
       erEfDt = *off;
       msEfDt = *blanks;
       erDv1c = *off;
       msDv1c = *blanks;
       erDv2c = *off;
       msDv2c = *blanks;
       er1Acd = *off;
       ms1Acd = *blanks;
       er1AhD = *off;
       ms1AhD = *blanks;
       er1Bcd = *off;
       ms1Bcd = *blanks;
       erCrCd = *off;
       msCrCd = *blanks;
       erDbCd = *off;
       msDbCd = *blanks;
       erWvAc = *off;
       msWvAc = *blanks;
       erVlCc = *off;
       msVlCc = *blanks;
       erMrrCt = *off;
       mgMrrCt = *blanks;
       erMrrC2 = *off;
       mgMrrC2 = *blanks;
       erDcl = *off;
       mgDcl = *blanks;
       erMinFd = *off;
       mgMinFd = *blanks;
       erMinSd = *off;
       mgMinSd = *blanks;
       erMinSc = *off;
       mgMinSc = *blanks;
       erDmax = *off;

       //Validate state and Effective date in Copy mode
       If @@mode = 'X' and datasaved = *off;
          xxKey = SCMState;
          Chain xxKey pst03;
          If Not %Found(ststl3);
             isError = *On;
             erMState = *On;
             msMState = 'WCR0504';
          Else;
             erMState = *Off;
          Endif;

          erEfDt = ValidDate(scEffDat:message);
          If erEfDt = *on;
             isError = *On;
             msEfDt = message;
          Endif;

          // Validation check for Duplicate record
          clear intEffDt;
          intEffDt = dtCYMD(scEffDat);
          Chain(n) (intCo#:scFnd:SCMState:intEffDt) prr00;
          If %found(wdrrl);
             erMstate = *on;
             erEfdt = *on;
             msMState = 'WC30005';
             msEfDt = 'WC30005';
            isError = *On;
          Endif;
       Endif;

       //Validate Expense Constant
       If scEcCd <= 0;
          erEcCd = *on;
          msEcCd = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Minimum Premium
       If scMpCd <= 0;
          erMpCd = *on;
          msMpCd = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Deviation # 1
       If scDv1h <> ' ' or scDv1l <> ' ';
          If (scDv1c <= 0 and @@cflg <> '1') or
           (scDv1c <= 0 and @@cflg =  '1' and charSt <> 'NM') or
           (scDv1c <  0 and @@cflg = '1' and  charSt =  'NM');
             erDv1c = *on;
             msDv1c = 'WCR0502';
             isError = *on;
          Endif;
       Endif;

       //Validate Deviation # 1 Headings
       If (scDv1c <> 0 and SCDV1H = *blanks) or
        (SCDV1L <> *blanks and SCDV1H = *blanks and
        charSt = 'NM'and @@cflg = '1');
          erDv1h = *on;
          msDv1h = 'WCR0503';
          isError = *on;
       Endif;

       //Validate Deviation # 1 Premium Level
       If scDv1c <> 0 or (scDv1l <> ' ' and charSt <> 'NM'
        and @@cflg <> '1') or (scDv1h <> ' ' and charSt = 'NM'
        and @@cflg = '1');
          If scDv1l <> 'J' and scDv1l <> 'E' and scDv1l <> 'S'
           and scDv1l <> 'D' and scDv1l <> 'M';
             erDv1l = *on;
             msDv1l = 'WC30189';
             isError = *on;
          Endif;
       Endif;

       //Validate Deviation # 2
       If scDv2h <> *blanks or scDv2l <> *blanks;
          If scDv2c <= 0;
             erDv2c = *on;
             msDv2c = 'WCR0502';
             isError = *on;
          Endif;
       Endif;

       //Validate Deviation # 2 Headings
       If (scDv2c <> 0 and SCDV2H = *blanks);
          erDv2h = *on;
          msDv2h = 'WCR0503';
          isError = *on;
       Endif;

       //Validate Deviation # 2 Premium Level
       If scDv2c <> 0 or scDv2l <> ' ';
          If scDv2l <> 'J' and scDv2l <> 'E' and scDv2l <> 'S'
           and scDv2l <> 'D' and scDv2l <> 'M';
             erDv2l = *on;
             msDv2l = 'WC30189';
             isError = *on;
          Endif;
       Endif;

       //Validate Contractor Credit
       If scCchd <> *blanks and scCccd <= 0;
          erCccd = *on;
          msCccd = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Contractor Credit Headings
       //*** Begin Del ***** 08/25/25 *****************************
       //*** If (scCccD <> 0 and scCchD = *blanks);
       //***    erCchD = *on;
       //***    msCchD = 'WCR0503';
       //***    isError = *on;
       //*** Endif;
       //*** End   Del ***** 08/25/25 *****************************

       //Validate Subject Premium Credit 1A
       If sc1Ahd <> *blanks and sc1Acd <= 0;
          er1Acd = *on;
          ms1Acd = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Subject Premium Credit 1A Headings
       If (sc1AcD <> 0 and sc1AhD = *blanks);
          er1AhD = *on;
          ms1AhD = 'WCR0503';
          isError = *on;
       Endif;

       //Validate Subject Premium Credit 1B
       If sc1Bhd <> *blanks and sc1Bcd <= 0;
          er1Bcd = *on;
          ms1Bcd = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Subject Premium Credit 1B Headings
       If (sc1BcD <> 0 and sc1BhD = *blanks);
          er1BhD = *on;
          ms1BhD = 'WCR0503';
          isError = *on;
       Endif;

       //Validate Schedule Credit Code
       If scCrHd <> *blanks and scCrCd <= 0;
          erCrCd = *on;
          msCrCd = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Schedule Credit Code Headings
       If (scCrCd <> 0 and scCrHd = *blanks);
          erCrHd = *on;
          msCrHd = 'WCR0503';
          isError = *on;
       Endif;

       //Validate Schedule Debit Code
       If (scDbHd <> *blanks and scDbCd <= 0);
          erDbCd = *on;
          msDbCd = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Schedule Debit Code Headings
       If (scDbCd <> 0 and scDbHd = *blanks);
          erDbHd = *on;
          msDbHd = 'WCR0503';
          isError = *on;
       Endif;

       //Validate Waiver Surcharge
       If (scWvHd <> *blanks and scwvac <= 0);
          erWvAc = *on;
          msWvAc = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Waiver Surcharge Headings
       If (scwvac <> 0 and scWvHd = *blanks);
          erWvHd = *on;
          msWvHd = 'WCR0503';
          isError = *on;
       Endif;

       //Validate Aircraft Surcharge Premium Level
       If scDcCd <> 0;
          If scAsFl = ' ';
             erAsFl = *on;
             msAsFl = 'WC30009';
             isError = *on;
          Endif;
       Endif;

       //Validate Voluntary Labor Stat Code
       If (scVlCh <> *blanks and scVlCc <= 0);
          erVlCc = *on;
          msVlCc = 'WCR0502';
          isError = *on;
       Endif;

       //Validate Voluntary Labor Stat Code Headings
       If (scVlCc <> 0 and scVlCh = *blanks);
          erVlCh = *on;
          msVlCh = 'WCR0503';
          isError = *on;
       Endif;

       //Validate 2nd Debit Minimum
       If (scMrRdT <= 1 and scMrRdT <> 0);
          erMrRdT = *on;
          mgMrRdT = 'Minimum Debit for 2nd Stat Code' +
           ' cannot be less than or equal to one.';
          isError = *on;
       Endif;

       //Validate 2nd Debit Minimum
       If (scMrRdT = 0 and scMrrD2 <> 0);
          erMrRdT = *on;
          mgMrRdT = 'Minimum must be specified when' +
           ' a 2nd Stat Code is entered.';
          isError = *on;
       Endif;

       //Validate 2nd Debit Status Code
       If (scMrrD2 = 0 and scMrrDt <> 0);
          erMrrD2 = *on;
          mgMrrD2 = '2nd Stat Code must be entered' +
           ' when there is a Minimum specified.';
          isError = *on;
       Endif;

       //Validate 2nd Credit Minimum
       If (scMrrCt >= 1 and scMrrCt <> 0);
          erMrrCt = *on;
          mgMrrCt = 'Minimum Credit for 2nd Stat Code' +
           ' cannot be greater than or equal to one.';
          isError = *on;
       Endif;

       //Validate 2nd Credit Minimum
       If (scMrrCt = 0 and scMrrC2 <> 0);
          erMrrCt = *on;
          mgMrrCt = 'Minimum must be specified when' +
           ' a 2nd Stat Code is entered.';
          isError = *on;
       Endif;

       //Validate 2nd Credit Status Code
       If (scMrrC2 = 0 and scMrrCt <> 0);
          erMrrC2 = *on;
          mgMrrC2 = '2nd Stat Code must be entered' +
           ' when there is a Minimum specified.';
          isError = *on;
       Endif;

       //Validate Default Stock Discount Percentage and Table Code
       // Both are Mutually Exclusive
       If scDsPc <> 0 and scDiSc <> *blanks;
          erDsPc = *on;
          msDsPc = 'WCR0590';
          erDisc = *on;
          msDisc = 'WCR0590';
          isError = *on;
       Endif;

       //Validate Default Stock Discount
       charSt = scMstate;
       intEffDt = dtcymd(scEffDat);
       If scDiSc <> *blanks;
          Setgt (scCo#:scFnd:charSt:scDiSc:intEffDt) pds03;
          Readpe (scCo#:scFnd:charSt:scDiSc) pds03;
          If not %found(wmdsl3);
             erDisc = *on;
             msDisc = 'WCR0513';
             isError = *on;
          Endif;
       Endif;

       //Validate Deductible Discount Applied
       If scRdPr = *blanks;
          erRdPr = *on;
          msRdPR = 'WC30014';
          isError = *on;
       Endif;

       //Validate Short Rate Penalty Amount
       If scSrAf = *blanks;
          erSrAf = *on;
          msSrAf = 'WC30393';
          isError = *on;
       Endif;

       //Validate Stock Discount Applied to Modified
       If scDsPr = *blanks;
          erDsPr = *on;
          msDsPr = 'WC30151';
          isError = *on;
       Endif;

       //Validate Audit Non Compliance Multiplier
       If scAnCs <> 0;
          If scAnCm <= 0;
             erAnCm = *on;
             msAnCm = 'WCR3183';
             isError = *on;
          Endif;
       Endif;

       //Validate Loss Cost Multiplier Expiry Date
       If scLcEx <> 0;
          erLCEX = ValidDate(scLcEx:message);
          If erLCEX = *on;
            isError = *On;
            msLCEX = message;
          Endif;
       Endif;

       //Validate Loss Cost Multiplier Expiry Date
       If scLcex <> 0 and erLCEX = *off;
          dtLossCst = dtCYMD(scLcEx);
          intEffDt = dtCYMD(scEffDat);
          If dtLossCst < intEffDT;
             erLcex = *on;
             msLcex = 'WC30349';
             isError = *on;
          Endif;
       Endif;

       //Validate Max Scheduled Factor Allowed
       If scDmax <> 0 and scDmax < 1.0;
          erDmax = *on;
          isError = *on;
       Endif;

       //Validate Discount Calculation Logic
       If scDcl = *Blanks;
          erDcl = *on;
          mgDcl = 'Value must be N,F, or S.';
          isError = *on;
       Endif;

       // Validate Minimum Premium to Offer Amount fields
       If scMinFd <> 0 and (scDcl = 'N' or scDcl = ' ');
          erMinFd = *on;
          mgMinFd = 'Value must be Zero if no Discount Logic Selected.';
          isError = *on;
       Endif;
       If scMinSd <> 0 and (scDcl = 'N' or scDcl = ' ');
          erMinSd = *on;
          mgMinSd = 'Value must be Zero if no Discount Logic Selected.';
          isError = *on;
       Endif;
       If scMinSc <> 0 and (scDcl = 'N' or scDcl = ' ');
          erMinSc = *on;
          mgMinSc = 'Value must be Zero if no Discount Logic Selected.';
          isError = *on;
       Endif;

       // Return *on indicator when No error exists.
       If isError = *on;
          Return *off;
       Else;
          Return *on;
       Endif;

      /End-Free
     p ValidPrmInfo    e

       //===================================================================
       // Validate the Tax Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidTaxInfo    b
     d ValidTaxInfo    pi              n
     d isError2        s               n   inz
      /Free
       isError2 = *off;
       msStl = *Blanks;
       erStl = *off;
       msStf = *Blanks;
       erStf = *off;
       msXfc = *Blanks;
       erXfc = *off;

       // Validate Applied To
       If scStp > 0 and scStl = *blanks;
          msStl = 'WC30162';
          erStl = *on;
          isError2 = *on;
       Endif;

       // F-Class Percentage and Exclude F-Classes are Mutually Exclusive
       If scStf > 0 and scXfc = 'Y';
          msStf = 'WCR1022';
          erStf = *on;
          msXfc = 'WCR1022';
          erXfc = *on;
          isError2 = *on;
       Endif;


       // Return *on indicator when No error exists.
       If isError2 = *on;
          Return *off;
       Else;
          Return *on;
       Endif;
      /End-Free
     p ValidTaxInfo    e
