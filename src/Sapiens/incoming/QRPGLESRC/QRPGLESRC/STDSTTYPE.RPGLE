     h*
     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved. ')
     h*
     h**********************************************************************
     h/title STDSTTYPE - Contact Configuration
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Process Distribution Types.
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created: 04/19/21   C:53   Project WC2278
     h**********************************************************************
     h*  Revisions:
     h*
     h*  09/09/24 C:342 WC2826/117559
     h*                 Validate Dupliacte records in Edit mode for contact type
     h*
     h*  09/03/24 C:053 WC2826/117559
     h*                 Do not display system contacts.
     h*
     h*  07/30/24 C:342 WC2826/115874/116975/116976
     h*                 Claims Contacts.
     h*                 DSPF changes:
     h*                 1. Added visibility for edit and delete link on
     h*                    contact tab.
     h*
     h*  03/04/24 C:342 WC2826
     h*                 Claims Contacts.
     h*                 DSPF changes:
     h*                 1. Added Contact Type tab
     h*
     h*  11/02/21 C:53  Support: 99191
     h*                 Correct display mode set.
     h*
     h**********************************************************************

     fstdsttype cf   e             workstn extfile('STDSTTYPE')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflDistTyp:rrnPaycod)
      //*** Beg Add *** 03/04/24 *******************************************
     f                                     Sfile(sflContTyp:scSflRRN)
      //*** End Add *** 03/04/24 *******************************************
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

      //*** Beg Add *** 03/04/24 *******************************************
     fstcnp     uf a e             disk    rename(pcn01:pcn0p)
     f                                     InfDs(dbfds2)
     fstcnl     if   e           k disk    InfDs(dbfds3)
      //*** End Add *** 03/04/24 *******************************************
     fstdsl     uf a e           k disk    InfDs(dbfds1)
     fstdsp     uf a e             disk    rename(pds01:pds0p)
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
     d***** Beg Add *** 03/04/24 *******************************************
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d***** End Add *** 03/04/24 *******************************************
     d***** Beg Add *** 09/09/24 *******************************************
     d svContTypE      s                   like(scContTypE)
     d svContextE      s                   like(scContextE)
     d***** End Add *** 09/09/24 *******************************************

     d intCount        s              3  0
     d rCount          s              3  0
     d @@mode          s              1
     d inmode          s              1
     d ##mode          s              1    inz('C')
     d @@TabIndex      s              5
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d ##rrn           s              9  0
     d intRRN          s              9  0 inz
     d clientID        s              3    inz('000')
     d meetPay         s              1
     d ##desc          s            100
     d ##pdsc          s            100
     d ##start         s              3  0 inz(1)
     d ##strlen        s              3  0 inz(100)
     d ##scnres        s              3  0
     d ##patlen        s              3  0 inz(40)
     d ##trans         s              1    inz('0')
     d ##trim          s              1    inz('1')
     d ##wild          s              1    inz(' ')
     d isValid         s              1n   inz(*off)
     d strDist         s             25    inz('Distribution')
     d strMsg          s            256
     d**** Beg Add *** 03/04/24 *******************************************
     darrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
     d saveDT          s                   like(CNdt)
     d saveTM          s                   like(CNtime)
     d flgRcdUpdated   s              1    inz
     d svsflrrn        s              5  0 inz
     d svsflrrn1       s              5  0 inz
     d isError         s               n   inz
     d*charmsg         s            200    inz(*blanks)
     d isDelResp       s               n   inz
     d**** End Add *** 03/04/24 *******************************************

     d* Clear Subfiles
     d clrSubFiles     pr

     d* Get client ID
     d getClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d**** Beg Add *** 03/04/24 *******************************************
     d* Load Contact Type grid
     d loadContTyp     pr

      * Process Addition of Contact Info.
     d ProcessAddCont  pr

      * Process Edit/Delete for Contact Info.
     d ProcOtherCont   pr

     d**** End Add *** 03/04/24 *******************************************

     d* Load payment value grid
     d loadPayValue    pr

     d* Main Processing
     d main            pr

     d* Program Start
     d programStart    pr

     d* Process other options
     d procOther       pr

      * Validate Contact Type
     d ValidateCont    pr              n
     d strmode                        1

     d  ##CltId        s              3

     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany

     d/copy sPrUsrPrms
     d/copy sPrGenPop
    
     c* Entry of Program.
     c     *entry        plist
     c                   parm                    @@Mode
     c                   parm                    @@TabIndex

      /free
       Exec Sql Set Option Commit = *None;
       // Start Of Program
       programStart();

       // Main Procesing
       main();

       *inlr = *on;
       return;
      /end-free

      //*** Beg Add *** 03/04/24 *******************************************
       //===================================================================
       // Add information to list
       //===================================================================
     p AddToList       b
     d AddToList       pi           101a   varying
     d inString                     100a   varying const
     d inArray                      100a   varying dim(20)
     d inIndex                       10i 0

     d outString       s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     p AddToList       e
      //*** End Add *** 03/04/24 *******************************************

       //===================================================================
       // addDstTyp  - Add/Edit  Type - MoveScDb
       //===================================================================
     p addDstTyp       b
     d addDstTyp       pi

      /free

       Chain intRrn pds0p;

       DSType = Upper(scAdDisTyp);
       DSdesc = scAddDesc;
       DSstat = scStat;
       DSuser = q1user;
       DSpgm  = q1pgm;
       DSdt   = Dateto7(SystemDate());
       DStime = Timeto6(SystemTime());

       If ##mode = 'A';
          DSco#  = GetUserInfo(q1user);
          Write pds0p;
       Else;
          Update pds0p;
       Endif;
       Unlock STDSP;
      /end-free
     p addDstTyp       e

       //===================================================================
       // clrSubFiles - Clears SubFiles
       //===================================================================
     p clrSubFiles     b
     d clrSubFiles     pi
      /Free

       rrnPayCod  = 0;
       clrPayCod  = *On;
       dspPaySub  = *Off;
       dspPayCtl  = *Off;
       Write rcdDisTy;

      /End-Free
     p clrSubfiles     e
       //===================================================================
       // deleteRcd    - Delete routine
       //===================================================================
     p deleteRcd       b
     d deleteRcd       pi
     d inRRN                          9  0 options(*nopass)
     d errdel          s              1

      /Free

       Chain(n) inRRN pds0p;
       If %found;
          strMsg   = ' ';
             Exec SQL
             Select count(*) into :rCount
             From STDLPCRT Where DLCo# = :intCo# and
             DLcritID = :strDist and
             DLcritVal = :DStype;
          If rCount <= 0;
             Exec SQL
             Select count(*) into :rCount
             From STDEPCRT Where DECo# = :intCo# and
             DEcritID = :strDist and
             DEcritVal = :DStype;
          Endif;
          If rCount <= 0;
          //*** beg chg *** 09/07/21 *************//
          // Exec SQL
          // Select count(*) into :rCount
          // From STSPPCRT Where SPCo# = :intCo# and
          // SPcritID = :strDist and
          // SPcritVal = :DStype;
             Exec SQL
             Select count(*) into :rCount
             From STSPPCRT Where
             SPcritID = :strDist and
             SPcritVal = :DStype;
          //*** end chg *** 09/07/21 *************//
          Endif;
          //*** Beg Add *** 03/04/24 *******************************************
          scDstHdg = 'Delete Distribution Type';
          moveDbSc();
          Write RcdAddDis;
          //*** End Add *** 03/04/24 *******************************************
          If rCount <= 0;
             //*** Beg Chg *** 03/04/24 *******************************************
             //*** Delete inRRN pds0p;
             //*** strMsg = 'The Distribution Type has been deleted.';
             //*** PopOneBtn(strMsg:'Information');
             svsflrrn1 = *zeros;
             //*** Beg Chg *** 07/30/24 *******************************************
             //*** isDelresp = PopTwoBtn('Do you want to delete this Contact Type +
             //*** ?':'Confirm Delete':'Yes':'No');
             isDelresp = PopTwoBtn('Do you want to delete this Distribution +
             type?':'Confirm Delete':'Yes':'No');
             //*** End Chg *** 07/30/24 *******************************************
             Select;
             When isDelresp = *On;
                Delete inRRN pds0p;
                strMsg = 'The Distribution Type has been deleted.';
                PopOneBtn(strMsg:'Information');
             When isDelresp = *Off;
                svsflrrn1 = rrnPayCod;     //save rrn value
             EndSl;
             //*** End Chg *** 03/04/24 *******************************************
          Else;
             //*** Beg Add *** 03/04/24 *******************************************
             svsflrrn1 = rrnPayCod;
             //*** End Add *** 03/04/24 *******************************************
             strMsg = 'Unable to delete.  This Code is associated +
              with an output type.';
             PopOneBtn(strMsg:'Error');
          Endif;
       Endif;

       return;
      /End-Free
     p deleteRcd       e

       //===================================================================
       // editAdd   - Edit/Add
       //===================================================================
     p editAdd         b
     d editAdd         pi
     d inRRN                          9  0 options(*nopass)

      /Free
       // To get the company number
       intCo# = GetUserInfo(q1user);

       // Program Mode.
       If %parms() >  0;
          ##mode  = 'C';
          intRRN  = inRRN;
       Else;
          intRRN  = *zeros;
       Endif;

       ProcessAdd();

       intRRN  = *zeros;
       return;
      /End-Free
     p editAdd         e

       //*** Beg Add *** 03/04/24 *******************************************
       //===================================================================
       // Load Contact Type Grid
       //===================================================================
     p LoadContTyp     b
     d LoadContTyp     pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdContTyp;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;

       // Populate the subfile
       Setll intco# stcnl;
       Reade intco# stcnl;
       Dow not %eof(Stcnl);

          //*** Beg Add *** 09/03/24 *************************************
          If CNsys <> 'Y';
          //*** End Add *** 09/03/24 *************************************
          scSflRRN += 1;
          scSflSel = *off;
 
          If svsflrrn = 0;
             If scSflRRN = 1;
                scSflSel = *on;
             Endif;
          Else;
             If scSflRRN = svsflrrn;
                scSflSel = *on;
             Endif;
          Endif;
 
          scConType = CNtype;
          scConDesc = CNdesc;
          scConText = CNctxt;

          If CNstat = 'A';
             scConStat = 'Active';
          Endif;
          If CNstat = 'I';
             scConStat = 'Inactive';
          Endif;

          //If dspMode;
          //   dspConVisb = *off;
          //Endif;
          //*** Beg Add *** 07/30/24 *************************************
          visNonSys = *on;
          If CNsys = 'Y';
             visNonSys = *off;
          Endif;
          //*** End Add *** 07/30/24 *************************************

          scConRRN# = dbrrn3;

          Write sflContTyp;
          //*** Beg Add *** 09/03/24 *************************************
          Endif;
          //*** End Add *** 09/03/24 *************************************
          Reade intco# stcnl;
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadContTyp     e
       //*** End Add *** 03/04/24 *******************************************

       //===================================================================
       // Load grid values
       //===================================================================
     p loadValue       b
     d loadValue       pi

      /free

       // clear the subfile.
       clear sflDistTyp;
       clrPayCod = *Off;
       dspPaySub = *On;
       dspPayCtl = *On;
       clear dbRrn1;
       //*** beg add *** 11/02/21 ********************************
       Select;
       When inmode = 'D';
          dspVisb = *Off;
       When inmode = 'A';
          dspVisb = *On;
       When inmode = 'C';
          dspVisb = *On;
       Endsl;
       //*** end add *** 11/02/21 ********************************

       Setll intCo# pds01;
       Reade intCo# pds01;
       dow not %eof(stdsl);

          meetPay = *on;


             // Write

             // Type
             scType =%trim(DStype);

             // Description
             scDesc =%trim(DSdesc);

             // Status
             If DSstat = 'A';
                scStat = 'Active';
             Else;
                scStat = 'Inactive';
             Endif;

             scRRN# = dbRrn1;

             rrnPayCod = rrnPayCod + 1;
             //*** Beg Chg *** 03/04/24 ********************************
             //*** If rrnPayCod = 1;
             //***    scPayRowSl = 'A';
             //*** Else;
             //***    clear scPayRowSl;
             //*** Endif;
             scPayRowSl   = *off;
             If svsflrrn1 = 0;
                If rrnPayCod = 1;
                   scPayRowSl = *on;
                Endif;
             Else;
                If rrnPayCod = svsflrrn1;
                   scPayRowSl = *on;
                Endif;
             Endif;
             //*** End Chg *** 03/04/24 ********************************

       //*** beg del *** 11/02/21 ********************************
       //*   If inmode = 'D';
       //*      dspVisb = *Off;
       //*   Else;
       //*      dspVisb = *On;
       //*   Endif;
       //*** end del *** 11/02/21 ********************************

             Write sflDistTyp;
          Reade intCo# pds01;
       Enddo;

       rrnPayCod  = 1;

       //*** Beg Add *** 03/04/24 ***************************************
       If svSflRRN1 <> 0;
          rrnPayCod = svSflRRN;
          svSflRRN1 = 0;
       Endif;
       //*** End Add *** 03/04/24 ***************************************

      /end-free
     p loadValue       e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p main            b
     d main            pi

      /free

       //*** Beg Add *** 03/04/24 *******************************************
       strTabNames += AddToList('Distribution Type':arrTabNames:n1);
       strTabNames += AddToList('Contact Type':arrTabNames:n1);
       //*** End Add *** 03/04/24 *******************************************
       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          //*** Beg Del *** 03/04/24 *******************************************
          // Clear Sub-files
          //*** ClrSubfiles();

          // Load Values
          //*** LoadValue();
          //*** End Del *** 03/04/24 *******************************************

          // Process Screens
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add
          When scAdd = *on;
             ##mode = 'A';
             EditAdd();
             ##mode = 'C';

          //*** Beg Add *** 03/04/24 *******************************************
          // Add Contact Type Information.
          When btnConAdd = *On;
             btnConAdd =*Off;
             ProcessAddCont();
          //*** End Add *** 03/04/24 *******************************************

          Other;

             ProcOther();
             //*** Beg Add *** 03/04/24 *******************************************
             ProcOtherCont();           // Other options for Contact Type Info.
             //*** End Add *** 03/04/24 *******************************************
          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       //*** Beg Chg *** 03/04/24 *******************************************
       When arrTabNames(tabIndex+1) = 'Distribution Type';
       //*** When TabIndex = 0;
       //*** End Chg *** 03/04/24 *******************************************
          TabHeight = 550;
          //*** Beg Add *** 03/04/24 ******************************************
          // Clear Sub-files
          ClrSubfiles();
          // Load Values
          LoadValue();
          //*** End Add *** 03/04/24 ******************************************
          Write rcdComPnl;
          Write rcdTabDist;
          Exfmt rcdDisTy;
          Read rcdComPnl;
          Read rcdTabDist;
       //*** Beg Add *** 03/04/24 *******************************************
       When arrTabNames(tabIndex+1) = 'Contact Type';
          TabHeight = 550;
          Clear rcdContTyp;
          // Load Contact Type Grid info
           LoadContTyp();

          Write rcdComPnl;
          Write rcdTabDist;
          Exfmt rcdContTyp;
          Read rcdComPnl;
          Read rcdTabDist;
       //*** End Add *** 03/04/24 *******************************************
       EndSl;

       Endsr;

      /end-free
     p main            e

       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     p moveDbSc        b
     d moveDbSc        pi

      /free

       scAdDisTyp = DStype;
       scAddDesc  = DSdesc;
       scStat     = DSstat;
      /end-free
     p moveDbSc        e
       //===================================================================
       // ProcessAdd - process addition
       //===================================================================
     p processAdd      b
     d processAdd      pi

      /free
       clear rcdAddDis;

       btnAddRcd = *off;
       btnCanRcd = *off;

       // processing for Edit mode
       If ##mode = 'C' and intRrn <> 0;
          Chain(n) intRrn pds0p;
          If %found;
             //*** Beg Add *** 03/04/24 *******************************************
             scDstHdg   = 'Edit Distribution Type';
             svsflrrn1  = rrnPayCod;
             //*** End Add *** 03/04/24 *******************************************
             indAdDisTy = *on;
             scAdDisTyp = DStype;
             MoveDbSc();
          Endif;
       Endif;

       // processing for Add mode
       If ##mode = 'A';
          //*** Beg Add *** 03/04/24 *******************************************
          scDstHdg   = 'Add Distribution Type';
          //*** End Add *** 03/04/24 *******************************************
          scStat     = 'A';
          scDesc     = *blanks;
          scType     = *blanks;
       Endif;

       Dou BtnCanRcd = *on or (BtnAddRcd = *on and isValid = *off);
          Exfmt rcdAddDis;
          isValid = *off;

          erTypUnq  = *off;
          erTypBlnk = *off;
          erDesc    = *off;
          erDelInac = *off;

          Select;
             When BtnAddRcd = *on;
                Validate() ;
                If isValid = *off;
                   addDstTyp();
                Endif;
             When BtnCanRcd = *on;
                BtnCanRcd = *off;
                Leave;
          Endsl;
       Enddo;
      /end-free
     p processAdd      e

       //*** Beg Add *** 03/04/24 *******************************************
       //===================================================================
       // ProcessAddCont - Process Addition of Contact Type.
       //===================================================================
     P ProcessAddCont  B
     D ProcessAddCont  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear RcdAddCont;
       mode = 'A';
       isexit = *off;
       //visSvAd = *on;
       scConStatE = 'A';

       Dow isexit = *off;

          scContHdg = 'Add Contact Type';

          Exfmt rcdAddCont;
          Select;

          // Save Button
          When BtnSavCont = *on;
             BtnSavCont = *off;
             If ValidateCont(mode);
                Exsr WrtCont;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnCanCont = *on;
               BtnCanCont = *off;
               isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        CNco#   = intCo#;                // Company Number
        CNtype  = scContTypE;            // Contact Type
        CNdesc  = scConDescE;            // Description
        CNctxt  = scContextE;            // Context
        CNsys   = *blanks;               // System Contact Flag
        CNstat  = scConStatE;            // Status

        CNuser  = q1user;                // User
        CNpgm   = q1pgm;                 // Program
        CNtime  = Timeto6(SystemTime()); // Time
        CNdt    = Dateto7(SystemDate()); // Date Mintained
        CNiusr  = q1user;                // Initial User
        CNipgm  = q1pgm;                 // Initial Program
        CNitim  = Timeto6(SystemTime()); // Initial Time
        CNidt   = Dateto7(SystemDate()); // Initial Date

       EndSr;

       // Writing new Contact Type Info.
       BegSr WrtCont;
        Clear pcn0p;
        Exsr MoveScDb;
        Write pcn0p;
       EndSr;

      /end-free
     P ProcessAddCont  E

       //*** End Add *** 03/04/24 *******************************************

       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p procOther       b
     d procOther       pi
      /free
       Readc sflDistTyp;

       Dow not %eof;
          If scEdit = *on;
             scEdit = *off;
             ##rrn = scRRN#;
             EditAdd(##rrn);
          Endif;

          If scDel = *on;
             scDel = *off;
             ##rrn = scRRN#;
             DeleteRcd(##rrn);
          Endif;

          Readc sflDistTyp;
       Enddo;

      /end-free
     p procOther       E

       //*** Beg Add *** 03/04/24 *******************************************

       //===================================================================
       // ProcOtherCont - Process other Options for Contact Type
       //===================================================================
     P ProcOtherCont   B
     D ProcOtherCont   PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear RcdAddCont;
       isexit = *off;

       ReadC sflContTyp;
       Dow not %eof;

          Select;
          When scConEdit = *On; // Processing for Edit
             scConEdit = *Off;
             mode = 'C';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scConRRN# pcn0p;
             Exsr Movdbsc;
             scContHdg = 'Edit Contact Type';
             Dow isexit = *off;
                Exfmt RcdAddCont;

                Select;
                When BtnSavCont = *on;
                   BtnSavCont = *off;
                   // Check if Stcnp updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('STCNP':dbrrn2:'CNdt':'CNtime':
                    'CNuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scConRRN# pcn0p;
                      Exsr Movdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      If ValidateCont(mode);
                         Chain scConRRN# pcn0p;
                         Exsr Movscdb;
                         Update pcn0p;
                         isExit = *on;
                      Endif;
                   Endif;
                When BtnCanCont = *on;
                   BtnCanCont = *off;
                   isExit = *on;
                EndSl;
             Enddo;

          // Process Delete option for Contact Type
          When scConDel = *on;
             scConDel = *off;
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scConRRN# pcn0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             Exsr MovDbsc;
             scContHdg = 'Delete Contact Type';
             Dow isexit = *off;
                Write RcdAddCont;

                isDelresp = PopTwoBtn('Do you want to delete this Contact Type +
                ?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Chain scConRRN# pcn0p;
                   Delete pcn0p;
                   PopOneBtn('Contact Type Deleted!':
                      'Delete':'Ok');
                   isexit = *on;
                   svsflrrn = *zeros;
                When isDelresp = *Off;
                   svsflrrn = scSflrrn;  //save rrn value
                   isexit = *on;
                EndSl;

             Enddo;
          Endsl;

          Unlock Stcnp;

          ReadC sflContTyp;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbsc;
        scContTypE = CNtype;
        scConDescE = CNdesc;
        scContextE = CNctxt;
        scConStatE = CNstat;
        If CNsys  = 'Y';
           dsbContTyp = *on;
        Endif;
        //*** Beg Add *** 09/09/24 *********************************************
        svContTypE = scContTypE;
        svContextE = scContextE;
        //*** End Add *** 09/09/24 *********************************************
        saveDT = CNdt;
        saveTM = CNtime;
       EndSr;

       // Subrotine to move screen field to database file
       BegSr Movscdb;
        CNtype  = scContTypE;            // Contact Type
        CNdesc  = scConDescE;            // Description
        CNctxt  = scContextE;            // Context
        CNstat  = scConStatE;            // Status

        CNuser  = q1user;                // User
        CNpgm   = q1pgm;                 // Program
        CNtime  = Timeto6(SystemTime()); // Time
        CNdt    = Dateto7(SystemDate()); // Date Mintained
        saveDT  = CNdt;
        saveTM  = CNtime;
       EndSr;

      /end-free
     P ProcOtherCont   E

       //*** End Add *** 03/04/24 *******************************************

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p programStart    b
     d programStart    pi

      /free

       // Program Mode.
       inmode = @@Mode;

       //*** beg del *** 11/02/21 ********************************
       //* Select;
       //* When inmode = 'D';
       //*    dspVisb = *Off;
       //* When inmode = 'A';
       //*    dspVisb = *On;
       //* When inmode = 'C';
       //*    dspVisb = *On;
       //* Endsl;
       //*** end del *** 11/02/21 ********************************

       // Tab Index Mode.
       If %parms() >= 2;
          intabIndex = %dec(@@TabIndex:5:0);
       Else;
          intabIndex = 0;
       EndIf;

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = GetUserInfo(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);
       aPnlHeight = 610;

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       ##rrn = 0;
       Return;

      /end-free
     p programStart    e
       //===================================================================
       // Validate - Validate Add/Edit screen
       //===================================================================
     p validate        b
     d validate        pi

     d ##stat          s              1
     d strKey          s                   Like (scAdDisTyp)
     d Count           s              5  0
     d errrcd          s              1

      /free

       // Validate status.
       If scStat = 'I';
          exsr FileCheck;
          If errrcd = *on;
             erDelInac  = *on;
             isValid = *on;
          Endif;
       Endif;

       If ##mode = 'A' and isValid = *off;
          // Check if Type is blank.
          If scAdDisTyp = *blanks;
             erTypBlnk  = *on;
             isValid = *on;
          Endif;

          // Validate Type.
          If scAdDisTyp <> *blanks;
             scAdDisTyp = Upper(scAdDisTyp);
             strKey = scAdDisTyp;
             Chain(n) (intCo#:strKey) pds01;
             If %found;
                erTypUnq = *on;
                isValid = *on;
             Endif;
          Endif;
       Endif;

       // Validate description is blank.
       //*** Beg Chg *** 03/04/24 *******************************************
       //*** If scAddDesc  = *blanks and isValid = *off;
       If scAddDesc  = *blanks;
       //*** End Chg *** 03/04/24 *******************************************
          erDesc     = *on;
          isValid = *on;
       Endif;

       BegSr FileCheck;
          errrcd = *off;
          Exec SQL select count(*) into :count from STDLPCRT
          where DLCO# = :intco# and DLcritID = 'Distribution' and
          DLcritVal = :scAdDisTyp;
          If count > 0;
             errrcd = *on;
          Endif;

          If errrcd = *off;
             Exec SQL select count(*) into :count from STDEPCRT
             where DECO# = :intco# and DEcritID = 'Distribution' and
             DEcritVal = :scAdDisTyp;
             If count > 0;
                errrcd = *on;
             Endif;
          Endif;

          If errrcd = *off;
          //*** beg chg *** 09/07/21 *************//
          // Exec SQL select count(*) into :count from STSPPCRT
          // where SPCO# = :intco# and SPcritID = 'Distribution' and
          // SPcritVal = :scAdDisTyp;
             Exec SQL select count(*) into :count from STSPPCRT
             where SPcritID = 'Distribution' and
             SPcritVal = :scAdDisTyp;
          //*** end chg *** 09/07/21 *************//
             If count > 0;
                errrcd = *on;
             Endif;
          Endif;

       EndSr;

      /end-free
     p validate        e

       //*** Beg Add *** 03/04/24 *******************************************
      //===================================================================
      // Validate the Contact Type Info
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateCont    b
     d ValidateCont    pi              n
     d strmode                        1
      /Free

       // Initialize all the messageId and error indicators.
        isError = *off;
        erContTypE = *off;
        msContTypE = *blanks;
        erConDescE = *off;
        msConDescE = *blanks;
        erContextE = *off;
        msContextE = *blanks;
        erContTyp2 = *off;
        msgContTyp = *blanks;

        // Validate Contact Type
        If scContTypE = *blanks;
           erContTypE = *on;
           msContTypE = 'WCR0503';
           isError = *on;
        Endif;

        // Validate Description
        If scConDescE = *blanks;
           erConDescE = *on;
           msConDescE = 'WCR0503';
           isError = *on;
        Endif;

        // Validate Context
        If scContextE = *blanks;
           erContextE = *on;
           msContextE = 'WCR0503';
           isError = *on;
        Endif;

        // Check if duplicate records exist
        //*** Beg Chg *** 09/09/24 *********************************************
        //*** If strmode = 'A';
        If strmode = 'A' or (svContTypE<>scContTypE or svContextE<>scContextE);
        //*** End Chg *** 09/09/24 *********************************************
           If erContTypE = *off;
              chain (intco#:scContTypE:scContextE) pcn01;
              If %found(stcnl);
                 erContTyp2 = *on;
                 msgContTyp = 'Contact Type already exists with this Context';
                 isError = *on;
              Endif;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateCont    e

       //*** End Add *** 03/04/24 *******************************************
