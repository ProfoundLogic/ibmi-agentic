     hCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WTCLMACC - Account Custom Data
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Displays Account custom Data.
     h*
     h*  Called from: Profound UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 3/9/2012 by C:062 for Project WC2115
     h**********************************************************************
     h* Revisions:
     H*                                                                    *
     h*  06/09/20  C:298 Project: SY420
     h*                  Mask SSN
     h*
     H*  03/23/16 C:308 Project: Elevate                                   *
     H*                 Genericize the toolbar programming               *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
     h*                                                                    *
     h*  04/16/15 C:65  Project: SY251                                     *
     h*                 Add API call to launch new Department letters.     *
     h*
      *    09/17/15 C:302 Project: WC2254
      *                   Recompile only
     h**********************************************************************
     fwtClmAcc  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     InfDs(fids01)
     fwtActPnl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
      *****Begin Add*****  03/23/16  *************************************
     f                                     include(rcdClmPnl)
      ***** End Add *****  03/23/16  *************************************
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
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     d* Load Action Panel procedure Prototype and variables
     d/Copy sCopyClmPr
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************

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
     D @@Mode          s              1    Inz('C')
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
      *

     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

      /Free

       ExSr  ProgramStart ;

       DoU saveData=*Off;
          LoadTab();
       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****   If btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
       //*****   Endif;
       //*** end add *****  04/16/15  ****************************************
       If btnShowWin = *on;
          PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
          btnShowWin = *off;
       Endif;
       //*** end chg *****  01/07/16  ****************************************
       EndDo;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

          //Load default values or passed in values
          inRRNClm=%int(@@RRN);
          inMode=@@Mode;
          tabIndex=0;
          tabHeader='Custom Data';

          // Access the claim record
          Chain(n) inRRNClm dbrcdf;

          // Get the client id
          GetClientID(outClientID);

          // Populate hidden fields
          scActCltId=outClientID;
          scActRrn=inRrnClm;

          // Prepare IFrame URL
          scCustData=CreateGenUrl('/iDartsWeb/profound.do':'user=' +
           %Trim(q1User):'&action=customData':'&keyProgram=W0070R':'&key=' +
           %EditC(CMco#:'X') + %EditC(CMfnd:'X') + %EditC(CMfYr:'X') +
           %EditC(CMcase:'X'));

          // Loads Action Panel Fields
          LoadActPan();

       endsr;
       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

          *inlr=*on;
          Return;

       endsr;
      /end-Free
       //===================================================================
       // LoadTab() - Load Tab Inforamtion
       //===================================================================
     P LoadTab         B
     D LoadTab         PI
      /Free

       Select;
       When TabIndex = 0;
          tabHeight=860;
          aPnlHeight=tabHeight + 60;
          Write rcdClmPnl;
          Write rcdClmAcc;
          Exfmt rcdClmAcc0;
          Read rcdClmPnl;
          Read rcdClmAcc;
       EndSl;

      /End-Free
     P LoadTab         E
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy sCopyClmPi
