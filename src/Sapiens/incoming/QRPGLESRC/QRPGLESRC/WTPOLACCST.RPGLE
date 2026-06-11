     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WTPOLACCST - Policy Account Custom Data
     H DFTNAME(WTPOLACCST)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 11/14/12                                         *
      *  Programmer: C:062                                            *
      *  Project   : WC2116                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *
      *                                                               *
      * 04/16/15 C:65  Project: SY251                                 *
      *                Add API call to launch new Department          *
      *                letters.                                       *
      *                                                               *
      * 09/03/13  C:189 Redmine: 3015                                 *
      *         Use include instead of ignore for action panel record *
      *         format                                                *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwtpolaccstcf   e             workstn extfile('WTPOLACCST')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f*********** beg chg ****** 09/03/13 *************************
     f                                     Include(rcdPolPnl)
     f                                     Include(rcdQtePnl)
     f************                         ignore(rcdClmPnl)
     f*********** End chg ****** 09/03/13 *************************
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmahl1    if   e           k disk
     fwtqsl1    if   e           k disk
     fwaqtp     uf   e             disk    rename(pqt01:pqt0p)
     fwmaul33   if   e           k disk    rename(pau01:pau33)
     f                                     InfDs(dbfds3)
     fwmaup     uf   e             disk    rename(pau01:pau0p)
     fwaqtl12   if   e           k disk    rename(pqt01:pqt12)
     f                                     InfDs(dbfds2)
     fwdell8    if   e           k disk    rename(pel01:pel08)
     fwdell     uf   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds1)
     fwdrtp     uf   e             disk    rename(prt01:prt0p)
     fwtbsp     if   e             disk    rename(pbs01:pbs0p)
     fwdrtl     if   e           k disk    rename(prt01:prt00)
     f                                     infds(dbfds4)
     fwmnbp     uf a e             disk    rename(pnb01:pnb0p)
     fwmnbl     if   e           k disk    rename(pnb01:pnb00)
     f                                     infds(dbfds5)
     fwmupl     if   e           k disk    rename(pup01:pup0u)
     f                                     prefix(u_)
     fsmupl1    if   e           k disk
     fsmupl     if   e           k disk    rename(pup01:pup00)
     f                                     prefix(l_)
     fwtacl     if   e           k disk    rename(pac01:pac00)
     fwmaxl     if   e           k disk    rename(pax01:pax00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fstctl1    if   e           k disk
     fwmhap     o    e           k disk    rename(pha01:pha0p)
     fwmhal     if   e           k disk    rename(pha01:pha00)
     fwttal     if   e           k disk    rename(pta01:pta00)
     fwtcnl1    if   e           k disk

      /copy sCopyPolPr
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************
       //*** Begin add *  09/17/15
     d/copy wPrPolCpy
       //*** End add *  09/17/15

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  rrnwaqtl             397    400b 0
     d dbfds3          ds
     d  rrnwmaul33           397    400b 0
     d dbfds4          ds
     d  rrnwdrtl             397    400b 0
     d dbfds5          ds
     d  rrnwmnbl             397    400b 0

     d                 ds
     d CX                      1     35  0
     d                                     dim(5)
     d EMcad1                  1      7  0
     d EMcad2                  8     14  0
     d EMcad3                 15     21  0
     d EMcad4                 22     28  0
     d EMcad5                 29     35  0

     d                 ds
     d EF                      1     35  0
     d                                     dim(5)
     d EMefd1                  1      7  0
     d EMefd2                  8     14  0
     d EMefd3                 15     21  0
     d EMefd4                 22     28  0
     d EMefd5                 29     35  0

     d ##co            s                   like(ELco#)
     d ##fnd           s                   like(ELfnd)
     d ##emp           s                   like(ELemp#)
     d ##div           s                   like(ELdiv)
     d ##div#          s                   like(ELdiv)
     d ##fyr           s                   like(ELfyr)
     d ##rep           s                   like(AUrep)
     d ##aco           s                   like(AUaco)
     d ##co#           s              3
     d ##fnd#          s              3
     d ##date#         s              7
     d ##date1#        s              7
     d ##date2#        s              7
     d ##que#          s             10
     d ##mode#         s              1
     d**** ##kc#           s               n
     d ##key           s             10
     d ##key#          s             40
     d ##comm          s            256
     d ##cdat          s              8
     d ##dcid          s             12
     d ##pnm           s             10
     d ##secl          s              3
     d ##user          s             10
     d ##pgm           s             10
     d ##fold          s             80
     d ##difl          s              1
     d ##ipgm          s             10
     d ##fl01          s              1
     d ##fl02          s              1
     d ##rmcd          s              5
     d ##seq#          s              5
     d ##sbyl          s              1
     d ##com2          s             19
     d ##apid          s              2
     d ##idoc          s             12
     d ##ique          s             10
     d ##imgf          s              1
     d ##ipth          s             10
     d ##pidx          s              5
     d ##qpty          s              1
     d ##dctp          s              5
     d ##csid          s              2
     d ##code#         s              2
     d ##cseq          s              5  0
     d ##cpnam         s             10
     d ##cxsts         s              1

     d* indicators
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d isValid         s               n   inz(*off)
     d isVldFlt        s               n   inz(*off)
     d isSelVld        s               n   inz(*off)
     d process         s               n   inz(*off)

     d* work fields
     d @@mode          s              1
     d @@next          s             10
     d cntFlag         s              1
     D intDate         s              7  0
     D intDate1        s              7  0
     D intToDate       s              7  0
     D intFromDate     s              7  0
     D intZip          s              9  0
     D intApr          s             11  2
     d mode            s              1
     d meetFlt         s              1
     d meetAudFlt      s              1
     d outIndex        s              5
     d clientID        s              3    inz('000')
     D intWkDt         s                   like(QTwkdt)
     d procTyp         s                   like(QTtype)
     d rmkCd           s              5
     d saveFyr         s                   like(ELfyr)
     d svYear          s                   like(ELfyr)
     d scco#           s                   like(ELco#)
     d scfnd           s                   like(ELfnd)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d scfyr           s                   like(ELfyr)
     d saveEffDt       s                   like(ELpfdt)
     d strDate         s              8
     d strOrnr         s             27
     d strrrn3         s              9
     d tempTyp         s                   like(QTtype)
     d tempDt          s              8  0
     d x1              s              1  0
     d xxSt            s              2
     d xxRrn           s              9  0
     d xxKey           s             10
     d xxMode          s              1
     d xxPxSt          s              2
     d xxCall          s              1
     d intRrn          s              9  0
     d intRrn#         s              9  0

     D                 ds
     D dartsKey                1     40
     D company#                1      3  0
     D group#                  4      6  0
     D employer#               7     15  0
     D division               16     20  0
     D quote                  40     40

     d* Main Processing
     d Main            pr

     d* Program Start
     d ProgramStart    pr

     d/copy sPrDate
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrAR
     d/copy wPrUsrPrms
     d/copy sPrString
     d/copy wPrLoc
     d/copy sPrAtrium
     d/copy wPrClaims

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@Qte             1

      /free

       ProgramStart();
       Main();

       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       // Load action bar
       LoadActionBar();

       // Process until exit button is pressed.
       Dou ExitNow = *On;
          // Process policy custom data screen
          TabHeight = 1050;
          APnlHeight = TabHeight + 60;
          Exsr WriteAction;
          Write rcdPanel;
          Exfmt rcdPolCust;
          Exsr ReadAction;
          Read rcdPanel;
       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****If btnDocWndw = *on;
       //*****   btnDocWndw = *off;
       //*****   DocWindow(scActRRN:'PS');
       //*****Endif;
       //*** end add *****  04/16/15  ****************************************
       //*** Begin add *  09/17/15
       //*****If btnShowWin = *on;
       //*****   btnShowWin = *off;
       //*****   Select;
       //*****     When TxtShowWin = 'Copy Employer';
       //*****        CopyPolicy(scActRRN:@@Qte);
       //*****     When TxtShowWin = 'Department Letter';
       //*****        DocWindow(scActRRN:'PS');
       //*****   Endsl;
       //*****Endif;
       //*** End add * 09/17/15
       If btnShowWin = *on;
          PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
          btnShowWin = *off;
       Endif;
       //*** end chg *****  01/07/16  ****************************************

       EndDo;
       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;

       If qteMode;
          Write rcdQtePnl;
       Else;
          Write rcdPolPnl;
       Endif;

       Endsr;
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;

       If qteMode;
          Read rcdQtePnl;
       Else;
          Read rcdPolPnl;
       Endif;

       Endsr;

      /end-free

     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI

      /free

       xxRrn = %int(@@rrn);
       quoteMode = @@Qte;

       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;
       scActRrn=xxRrn;

       company#=ELco#;
       group#=ELfnd;
       employer#=ELemp#;
       division=ELdiv;
       quote=@@Qte;

       // Prepare IFrame URL
       scCustData=CreateGenUrl('/iDartsWeb/profound.do':'user=' +
        %Trim(q1User):'&action=customData':'&keyProgram=W0050R':'&key=' +
        dartsKey);

      /end-free
     P ProgramStart    E
      * utility procedure definitions
      /COPY SCOPYPOLPI
