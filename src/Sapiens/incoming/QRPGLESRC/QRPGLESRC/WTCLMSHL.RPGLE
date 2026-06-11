     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/TITLE WTCLMSHL - Profound Claims Shell
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     H BNDDIR('WTBNDDIR')
     h*
     h*  Description:  This Program Will Display The Claim Template.
     h*
     h*  Called from: Profound UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 3/9/2012 by C:062 for Project WC2115
     h**********************************************************************
     h* Revisions:
     H*                                                               *
     H* 09/20/24 C:344 Project: WC2952                                *
     H*                Added missing prototypes.                      *
     h*
     h*
     h**********************************************************************
     fwtClmShl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     InfDs(fids01)
     fwtActPnl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     fwmcmp     uf   e             Disk
     f                                     ReName(pcm01:dbrcdf)
     f                                     InfDs(dbfds)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     f                                     InfDs(dbfds2)

     d* Proto-types


     d* General Prototypes
     d/copy wPrUsrPrms
     d/copy sPrAtrium
     d/Copy sPrString
     d/Copy wPrGroup
     d/Copy sPrDate
     d/Copy wPrClaims
     d/Copy wPrAgent
     d/Copy wPrContact
     d/Copy wPrLoc
     d/Copy wPrEmplr
     d* Load Action Panel procedure Prototype and variables
     d/Copy sCopyClmPr
      **** Beg Add **** 09/20/24 *******************************************
     d/Copy sPrSecure
     d/Copy sPrMask
      **** End Add **** 09/20/24 *******************************************

     d* Check Add and Edit functions
     d ChkAddEdit      pr

     d* Error Check
     d ErrorCheck      pr              n

     d* Load Tab Information
     d LoadTab         pr

     D* System Data Structures
     D dbfds           ds
     D  flstcm           *status
     D  dbrrn                397    400b 0

     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     D*Variables
     D @@RRN           s              9
     D @@Mode          s              1
     D @@Next          s             10
     D @@TabIndex      s              5
     D outClientID     s              3    inz('000')
     D errorExists     s               n   inz(*off)
     D addEdit         s               n   inz(*off)
     D addMode         s               n   inz(*off)
     D chgMode         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D inRRNClm        s              9  0
     D inMode          s              1


     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
     C                   Parm                    @@TabIndex
      *

     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

      /Free

       ExSr  ProgramStart ;

       DoU (errorExists=*Off and addEdit=*Off and tabClick=*Off and
        saveData=*Off) or scMenuRsp<>*Blanks;
          If errorExists=*Off and addEdit=*Off and tabClick=*Off;
             MoveDbSc();
          EndIf;
          LoadTab();
          If tabClick=*Off;
             ChkAddEdit();
             If (chgMode=*On or addMode=*On) and scMenuRsp=*Blanks and
                 addEdit=*Off;
                errorExists=ErrorCheck();
             EndIf;
             If chgMode and scMenuRsp=*Blanks and errorExists=*Off and
                addEdit=*Off;
                UpdateRecord();
             EndIf;
          EndIf;
       EndDo;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

          //Load default values or passed in values
          If %parms()>0;
             inRRNClm=%int(@@RRN);
             inMode=@@Mode;
          Else;
             inRRNClm=41;
             inMode='C';
          EndIf;

          // Set the initial tab
          If %parms()>3;
             tabIndex=%Dec(@@TabIndex:5:0);
          Else;
             tabIndex=0;
          EndIf;

          // Access the claim record
          Chain(n) inRRNClm dbrcdf;

          // Get the client id
          GetClientID(outClientID);

          // Set the Mode
          Select;
          When inMode='D';
             dspMode=*on;
          When inMode='A';
            addMode=*on;
          When inMode='C';
             chgMode=*on;
          EndSl;

          // Assign Tab Headings
          tabHeader='Tab 1,Tab 2';
          tabClick=*Off;

       endsr;
       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

          If %Parms()>0;
             @@Next=scMenuRsp;
          EndIf;
          *inlr=*on;
          Return;

       endsr;
      /end-Free
       //===================================================================
       // ChkAddEdit - Check to see if Add/Edit indicators set off
       //===================================================================
     P ChkAddEdit      B
     D ChkAddEdit      PI
      /Free

       // Reset Indicators
       addEdit=*Off;

      /End-Free
     P ChkAddEdit      E
       //===================================================================
       // ErrorCheck - Validate the screen fields.
       //===================================================================
     P ErrorCheck      B
     D ErrorCheck      PI              n
     D error0          s               n   inz(*off)
     D error1          s               n   inz(*off)
      /Free

       If error0 Or error1;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck      E
       //===================================================================
       // MoveDbSc - Move Database fields to Screen fields
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
      /Free

          // Loads Action Panel Fields
          LoadActPan();

      /End-Free
     P MoveDbSc        E
       //===================================================================
       // MoveScDb - Move Screen fields to Database fields
       //===================================================================
     P MoveScDb        B
     D MoveScDb        PI
      /Free

      /End-Free
     P MoveScDb        E
       //===================================================================
       // LoadTab() - Load Tab Inforamtion
       //===================================================================
     P LoadTab         B
     D LoadTab         PI
      /Free

       Select;
       When TabIndex = 0;
          tabHeight=1550;
          aPnlHeight=tabHeight + 50;
          Write rcdClmPnl;
          Write rcdClmShl;
          Exfmt rcdClmShl0;
          Read rcdClmShl;
          Read rcdClmPnl;
       When TabIndex = 1;
          tabHeight=1550;
          aPnlHeight=tabHeight + 50;
          Write rcdClmPnl;
          Write rcdClmShl;
          Exfmt rcdClmShl1;
          Read rcdClmShl;
          Read rcdClmPnl;
       EndSl;

      /End-Free
     P LoadTab         E
       //===================================================================
       // UpdateRecord - Update DataBase
       //===================================================================
     P UpdateRecord    B
     D UpdateRecord    PI
      /Free
       Chain inRRNclm dbrcdf;
       If %Found;
          MoveScDb();
          Update    dbrcdf;
       EndIf;
      /End-Free
     P UpdateRecord    E
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy sCopyClmPi
