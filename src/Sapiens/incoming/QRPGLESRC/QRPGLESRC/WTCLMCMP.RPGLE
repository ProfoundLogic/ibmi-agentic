     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WTCLMCMP - Profound Compliance
     h/DEFINE   PROFOUNDUI
     h DFTACTGRP(*no) ACTGRP('QILE')
     H BNDDIR('WTBNDDIR')
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Compliance Info.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 6/29/2012 by c:279 for Project WC2115
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h* 01/14/25  C:340 Project WC2995                                     *
     h*                 Add New Output Fields                              *
     h*                 # Classification Code                              *
     h*                 # Return to Work Date                              *
     h*                 # Zip Code of Injury Site                          *
     h*                 # Number of Dependents                             *
     h*                 # Exposure State Code                              *
     h*                 # Indemnity Claim Code                             *
     h*                 Increase Panel Height for Tab# 2.                  *
     h*                                                                    *
     h* 06/09/20  C:298 Project: SY420
     h*                 Recompile
     h*
     h*  08/19/19 C:079 Project: WC2604                                    *
     h*                 Add IDC tab                                        *
     h*                                                                    *
     h*  05/30/18 C:180 Project: WC2515                                    *
     h*                 Add EDI Rel 3.1 fields                             *
     h*                                                                    *
     h*  05/30/18 C:079 Project: WC2515                                    *
     h*                 Change logic that loads EDI release level          *
     h*                                                                    *
     h*  03/23/16 C:308 Project: Elevate                                   *
     h*                 Genericize the toolbar programming                 *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
     h*                                                                    *
     h*  02/17/17 C:180 Support: 45761                                     *
     h*                 Use CMASCL instead of CMACL# for client 114 per    *
     h*                 proj 114WJE.                                       *
     h*                                                                    *
     h*  08/17/16 C:308 Project: 114WXY                                    *
     h*                 Recompile only                                     *
     h*                                                                    *
     h*  10/14/15 C:300 Support: 37399                                     *
     h*                 Update UI by updating dollar fields with commas    *
     h*                 on Wage amount under Benefit Information Section   *
      *                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
     h*
     h*  05/13/15 C:280 Phasware: 34720
     h*                 Add code to retieve claim admin
     h*
     h*  04/16/15 C:65  Project: SY251
     h*                 Add API call to launch new Department letters.
     h*
     h* 03/19/15  C:279 Support: 32825 (Phaseware)
     h*                 Restructured program logic to not exit on enter.
     h*
     h* 07/28/14  C:295 Project: 114WMD
     h*                 Compilation only as the changes are in Action Panel
     h*
     h* 03/04/14  C:180 Support: WC114 84947                                  *
     h*                 Add MCO code.
     h*
     h* 02/25/14  C:65  Project: Redmine #4004 (V14R1 Testing)
     h*                 Expand tab height for 1st tab to cover new fields.
     h*
     h* 02/12/14  C:180 Project: WC2234 - PRISS 27
     h*                 Fix Work week type and Work Days Schedule Code
     h*
     h* 12/12/13  C:287 Project: WC2234
     h*                 Add new fields for claims EDI Rel3.
     h*                 CMpspa, CMwwtc, CMwdsc, CMesid
     h*
     h* 10/23/13  C:283 Support: WC099 83339
     h*                 Correctly populate the Claims Administrator Details.
     h*
     h* 04/12/13  C:33  Project: WC2115  #2636
     h*                 Add messages for CMS & EDI indicating modules not
     h*                  installed.
     h**********************************************************************
     fwtClmCmp  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('WTCLMCMP')
     f                                     InfDs(fids01)
     f*** Beg Add *** 08/19/19 *********************************************
     f                                     sfile(sflIDCTran:rrnIDCTran)
     f                                     sfile(sflIDCQtr:rrnIDCQtr)
     f*** Beg Add *** 08/19/19 *********************************************
     fwtActPnl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
      *****Begin Add*****  03/23/16  *************************************
     f                                     include(rcdClmPnl)
      ***** End Add *****  03/23/16  *************************************
     fwdacl     if   e           k Disk    ReName(pac01:pac00)
     fwdf2l     if   e           k Disk    ReName(pf201:pf200)
     fwdell     if   e           k Disk    ReName(pel01:pel00)
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     fwmixl     if   e           k Disk    ReName(pix01:pix00)
     fwmlgl     if   e           k Disk    ReName(plg01:plg00)
     fwmprl     if   e           k Disk    ReName(ppr01:ppr00)
     fwmepl7e   if   e           k Disk    ReName(pep01:pep00)
     fwmpxl1    if   e           k Disk    ReName(ppx01:ppx00)
     fwmrsl     if   e           k Disk    ReName(prs01:prs00)
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     fwmall     if   e           k Disk    ReName(pal01:pal00)
     fwmlll     if   e           k Disk    ReName(pll01:pll00)
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)
     fwtdcl     if   e           k Disk    ReName(pdc01:pdc00)
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     fwtabl     if   e           k Disk
     f                                     Ignore(prs01)
     f                                     Ignore(psy01)
     f                                     ReName(prt01:prt00)
     f                                     ReName(pb201:pb200)
     f                                     ReName(pna01:pna00)
     f                                     ReName(ppa01:ppa00)
     f                                     ReName(pca01:pca00)
     fsmcol     if   e           k Disk    ReName(pco01:pco00)
     f***  Begin Add  ***  10/23/13  **************************************
     fwdf3l     if   e           k Disk    ReName(pf301:pf300)
     f***  End   Add  ***  10/23/13  **************************************
     f***  Begin Add  ***  08/19/19  **************************************
     fwmidctrl  if   e           k Disk    Rename(ptr01:ptr02)
     f                                     infds(trFds)
     fwmidcscl  if   e           k disk    Rename(pcc01:pcc02)
     fwmidcqtl  if   e           k Disk
     f                                     infds(qtFds)
     f***  End   Add  ***  08/19/19  **************************************

     d* Proto-types
     d* Display main screen
     d DisplayMain     pr

     d* General Prototypes
     d/copy wPrUsrPrms
     d/copy sPrAtrium
     d/copy sPrString
     d/copy sPrDate
      ***** Beg Add ***** 06/09/20 ****************************************
     d/copy sPrMask
     d/copy sPrSecure
      ***** End Add ***** 06/09/20 ****************************************
     d***  Begin Add  ***  04/12/13  **************************************
     d/copy sPrTable
     d***  End   Add  ***  04/12/13  **************************************
     d/copy wPrClaims
     d/copy wPrAgent
     d/copy wPrContact
     d/copy wPrLoc
     d/copy wPrGroup
     d/copy wPrEmplr
     d/copy wPrEdi

     d* Load Action Panel procedure Prototype and variables
     d/copy sCopyClmPr
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************

     d* System Data Structures
     d fids01        e ds                  ExtName(szq2p)
     d psds          esds                  ExtName(szq1p)

     d                 ds
     d  cmactm                 1      6  0
     d  intactm                1      4  0

     d                 ds
     d  scIndx01               1      4
     d  scIndx02               5      8
     d  scIndx03               9     12
     d  scIndx04              13     16
     d  scIndx05              17     20
     d  scIndx06              21     24
     d  scIndx07              25     28
     d  scIndx08              29     32
     d  scIndx09              33     36
     d  scIndx10              37     40
     d  scIndx11              41     44
     d  scIndx12              45     48
     d  scIndx13              49     52
     d  scIndx14              53     56
     d  scIndx15              57     60
     d  scIndx16              61     64
     d  scIndx17              65     68
     d  scIndx18              69     72
     d  scIndx19              73     76
     d  scIndx20              77     80
     d  scIndx21              81     84
     d  scIndx22              85     88
     d  scIndx23              89     92
     d  scIndx24              93     96
     d  scIndx25              97    100
     d  scIndx26             101    104
     d  scIndx27             105    108
     d  scIndx28             109    112
     d  scIndx29             113    116
     d  scIndx30             117    120
     d  scIndx31             121    124
     d  scIndx32             125    128
     d  scIndx33             129    132
     d  scIndx34             133    136
     d  scIndx35             137    140
     d  scIndx36             141    144
     d  scIndx37             145    148
     d  scIndx38             149    152
     d  scIndx39             153    156
     d  scIndx40             157    160
     d  scIndx41             161    164
     d  scIndx42             165    168
     d  scIndx43             169    172
     d  scIndx44             173    176
     d  scIndx45             177    180
     d  scIndx46             181    184
     d  scIndx47             185    188
     d  scIndx48             189    192
     d  scIndx49             193    196
     d  scIndx50             197    200
     d  scIndx51             201    204
     d  scIndx52             205    208
     d  scIndx53             209    212
     d  scIndx54             213    216
     d  scIndx55             217    220
     d  scIndx56             221    224
     d  scIndx57             225    228
     d  scIndx58             229    232
     d  scIndx59             233    236
     d  scIndx60             237    240
     d  scIndx61             241    244
     d  scIndx62             245    248
     d  scIndx63             249    252
     d  scIndx64             253    256
     d  scIndx65             257    260
     d  scIndx66             261    264
     d  scIndx67             265    268
     d  scIndx68             269    272
     d  scIndx69             273    276
     d  scIndx70             277    280
     d  scIndx71             281    284
     d  scIndx72             285    288
     d  scIndx73             289    292
     d  scIndx74             293    296
     d  scIndx75             297    300
     d  scIndx76             301    304
     d  scIndx77             305    308
     d  scIndx78             309    312
     d  scIndx79             313    316
     d  scIndx80             317    320
     d  scIndx81             321    324
     d  scIndx82             325    328
     d  scIndx83             329    332
     d  scIndx84             333    336
     d  scIndx85             337    340
     d  scIndx86             341    344
     d  scIndx87             345    348
     d  scIndx88             349    352
     d  scIndx89             353    356
     d  scIndx90             357    360
     d  scIndx91             361    364
     d  scIndx92             365    368
     d  scIndx93             369    372
     d  scIndx94             373    376
     d  scIndx95             377    380
     d  scIndx96             381    384
     d  scIndx97             385    388
     d  scIndx98             389    392
     d  scIndx99             393    396
     d  fd                     1    396
     d                                     dim(99)

     d cf              s              1    dim(99)
     DdsFileInfo       ds
     D rrn                   397    400b 0
     DtrFds            ds                  likeds(dsFileInfo)
     DqtFds            ds                  likeds(dsFileInfo)

     d* Variables
     d @@RRN           s              9
     d @@Mode          s              1
     d @@Next          s             10
     d ##rrn           s              9
     d ##st#           s                   like(PXst)
     d ##co#           s              3
     d ##fnd           s              3
     d ##fyr           s              3
     d ##case          s              7
     d addMode         s               n   inz(*Off)
     d chgMode         s               n   inz(*Off)
     d dspMode         s               n   inz(*Off)
     d call            s              1
     d kkTbId          s              4
     d kkKey           s                   like(NAkey)
     d outClientID     s              3    inz('000')
     d inRRNClm        s              9  0
     d inMode          s              1
     d intDt           s              8  0
     d intDy           s              5  0
     d intCnt          s              5  0
     d exitReason148   s             80
     d exitReasonA49   s             80
     d***  Begin Add  ***  04/12/13  **************************************
     d shwButton       s              1n
     d***  Begin Add  ***  04/12/13  **************************************
     d***  Begin Add  ***  08/19/19  **************************************
     d xxDate          s              8  0
     d***  End   Add  ***  08/19/19  **************************************
     C*
     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
      *
     C     keyWMIXL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMjrst
      *
     C     keyWTDCL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMjrst
      *
     C     keyWMCML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
      *
     C     keyWDF2L      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
      *
     C     keySMCOL      Klist
     C                   Kfld                    CMco#
      *
     C     keyWMPRL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMclas
     C                   Kfld                    CMacdt
      *
     C     keyWMPRL#     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMclas
      *
     C     keyWMEPL7E    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv
     C                   Kfld                    CMclas
     C                   Kfld                    CMspec
     C                   Kfld                    CMext
     C*
     C     keyWMPXL1     Klist
     C                   Kfld                    EPco#
     C                   Kfld                    EPclas
     C                   Kfld                    EPext
     C                   Kfld                    ##st#
     C                   Kfld                    EPefdt
     C*
     C     keyWMPXL1#    Klist
     C                   Kfld                    EPco#
     C                   Kfld                    EPclas
     C                   Kfld                    EPext
     C                   Kfld                    ##st#
      *
     C     keyWMRSL      Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELrtcd
     C*
     C     keyWDELL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv
     C                   Kfld                    CMfyr
      *
     C     keyWMALL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv
     C                   Kfld                    CMctg1
     C*
     C     keyWMCDL      Klist
     C                   Kfld                    CMss#
     C*
     C     keyWDACL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
      *
     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

     C     keyWMFNL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkTbId
     C                   Kfld                    kkKey
     C*
     C     keyWMLLL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase
      *
     C     keyWTABL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkKey
      *
     C***  Begin Add  ***  04/12/13  **************************************
     C     keyWDF3L      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrst
     C***  End   Add  ***  04/12/13  **************************************

      /Free

       // start of program
       ExSr  ProgramStart;

       // Load screen from database
       MoveDbSc();

       DoU scMenuRsp <> *Blanks;
          DisplayMain();

          Select;
       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
       //*** end add *****  04/16/15  ****************************************
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************

          When TabClick = *On;
             TabClick = *Off;

          When refrshData=*On;
               refrshData=*Off;
               ExSr  ProgramStart;
               MoveDbSc();

          When scMenuRsp <> *Blanks;
             @@next = scMenuRsp;
             Leave;
          Other;
          Endsl;

       EndDo;

       // End of program
       ExSr  ProgramEnd;

       //===================================================================
       // ProgramEnd - End of Processing
       //===================================================================
       Begsr ProgramEnd;

          *inlr=*On;
          Return;

       endsr;
       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

       // Clear record formats
       Clear RcdEdiInfo;
       Clear RcdCmsInfo;
       //*** Beg Add *** 08/19/19 ******************************************
       Clear RcdIDCInfo;
       //*** End Add *** 08/19/19 ******************************************

          //Load default values or passed in values
          If %Parms()>0;
             inRRNClm=%Int(@@RRN);
             inMode=@@Mode;
          Else;
             inRRNClm=41;
             inMode='C';
          EndIf;

          // Access the claim record
          Chain(n) inRRNClm pcm0p;

          // Get the client id
          GetClientID(outClientID);

          // Populate hidden fields
          scActCltId=outClientID;
          scActRrn=inRrnClm;

          // Set the Mode
          shwButton=*On;
          Select;
          When inMode='D';
             dspMode=*On;
             shwButton=*Off;
          When inMode='A';
             addMode=*On;
          When inMode='C';
             chgMode=*On;
          EndSl;
          scActMode = inMode;

          //***  Begin Add  ***  04/12/13  ********************************
          // Check for EDI module installed
          shwBtnEDI = shwButton;
          isShowEDI = *on;
          isMsgEDI  = *off;
          If not EDIInstalled;
             shwBtnEDI = *off;
             isShowEDI = *off;
             isMsgEDI  = *on;
          Endif;

          // Check for CMS module installed
          shwBtnCMS = shwButton;
          isShowCMS = *on;
          isMsgCMS  = *off;
          If not CMSInstalled;
             shwBtnCMS = *off;
             isShowCMS = *off;
             isMsgCMS  = *on;
          Endif;
          //***  End   Add  ***  04/12/13  ********************************
          //***  Beg   Add  ***  08/19/19  ********************************
          // Check for CMS module installed
          isShowIDC = *on;
          isMsgIDC  = *off;
          If not isModEnabled('IDC');
             isShowIDC = *off;
             isMsgIDC  = *on;
          Endif;
          //***  End   Add  ***  08/19/19  ********************************

          // Set the Action Panel Height
          aPnlHeight=1460;
          TabIndex  = 0;

          // Check to see if EDI buttons should be disabled.
          If dspMode=*On;
             noScrnA49=*On;
             noScrn148=*On;
          Else;
             noScrnA49=*Off;
             exitA49rsn=' ';
             noScrn148=*Off;
             exit148rsn=' ';
             If ExitEdiA49(inRrnClm:exitReasonA49)=*On;
                noScrnA49=*On;
                exitA49rsn=exitReasonA49;
             EndIf;
             If ExitEdi148(inRrnClm:exitReason148)=*On;
                noScrn148=*On;
                exit148rsn=exitReason148;
             Else;
                If ShowEdi148(inRrnClm)=*Off;
                   noScrn148=*On;
                   exit148rsn='148 change is not available';
                EndIf;
             EndIf;
          EndIf;

       endsr;
      /End-Free
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // DisplayMain - Display main screen
       //===================================================================
     P DisplayMain     B
     D DisplayMain     PI
      /Free

       Select;
       When TabIndex = 0;
          //*** begin chg ***  02/26/14  **************************************
          //*****tabHeight = 1400;
            tabHeight = 1450;
          //*** end chg *****  02/26/14  **************************************
          APnlHeight = tabHeight + 60;
          Write rcdClmPnl;
          Write rcdTabPan;
          Exfmt rcdEdiInfo;
       When TabIndex = 1;
          tabHeight = 910;
          APnlHeight = tabHeight + 60;
          Write rcdClmPnl;
          Write rcdTabPan;
          Exfmt rcdCmsInfo;
       //*** Beg Add *** 08/19/19 ******************************************
       When TabIndex = 2;

         // Clearing the Subfile before loading
          ClrSubfiles();
          LoadGrids();

      *** Beg Chg *** 01/14/25 *** WC2995 *****************************
          //*** tabHeight = 1200;
          tabHeight = 1300;
      *** Beg Chg *** 01/14/25 *** WC2995 *****************************
          APnlHeight = tabHeight + 60;
          Write rcdClmPnl;
          Write rcdTabPan;
          Exfmt rcdIDCInfo;
       //*** End Add *** 08/19/19 ******************************************
       Endsl;

       //******* beg chg ******* 03/19/15 **********************************
       // Read rcdTabPan;

       Read rcdClmPnl;
       Read rcdTabPan;
       //******* end chg ******* 03/19/15 **********************************

      /End-Free
     P DisplayMain     E
       //===================================================================
       // MoveDbSc - Move Database fields to Screen fields
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
      /Free

       // Load the Action Panel Fields
       LoadActPan();

       // Load screen fields for Jurisdiction info
       Chain keyWMIXL pix00;
       If %found(WMIXL);
          scEDIRlsLv = IXrls;
          //*** Beg Add *** 05/30/18 *********************************
          If IXrls = 'A';
             scEDIRlsLv = '3.1';
          Endif;
          //*** End Add *** 05/30/18 *********************************
       Endif;

       scEdiJrSt  = CMjrst;
       scEdiJSOfc = CMoff#;
       scEDIStClm = CMacl#;

       // *** Begin Add *** 02/17/17 *******************************
       If OutClientId = '114';
          scEDIStClm = CMascl;
       Endif;
       // *** End Add *** 02/17/17 *******************************

       // Load screen fields for Transaction info
       Chain keyWMCML plg00;
       If %found(WMLGL);
          kkTbId = 'TSET';
          kkkey  = LGtset;
          Chain keyWTGTL pgt00;
          If %Found(WTGTL);
             scTrnSetID = %Trim(GTkey) + '   ' + GTdesc;
          Else;
             scTrnSetID = LGtset;
          EndIf;

          If LGmtc <> *blanks;
             If LGtset = '148';
                kkTbId = 'FMTC';
             Else;
                kkTbId = 'SMTC';
             Endif;
             Clear kkkey;
             kkkey  = LGmtc;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scMntTypCd = %Trim(GTkey) + ' - ' + GTdesc;
             Else;
                scMntTypCd = LGmtc;
             EndIf;
          Endif;
       Endif;

       scMntDt    = CvtToDate(LGmtcd);
       scTrnDueDt = CvtToDate(LGdue);
       Clear intDt;
       intDt      = dtMDYY(LGdue);
       If intDt = *zeros;
          intDt      = dtMDYY(CMacdt);
          If CMrcvd > *zeros;
             intDt      = dtMDYY(CMrcvd);
          Endif;
          If intDt = *zeros;
             intDt = UDATE;
          Endif;
          Clear intDy;
          intDy = IXsdue;

          If intDt > 0  and intDy > 0;
             intDt = dtFuture(intDt:intDy);
          Endif;
          scTrnDueDt = CvtToDate(intDt);
       Endif;

       // Load screen fields for Insurer info
       Chain keyWMFNL pfn00;

       Chain keyWDF2L pf200;
       If %found(WDF2L);
          scInsFEIN# = F2fein;
       EndIf;
       scInsFeinN = FNname;

       // Load screen fields for 3rd party admin
       Chain keySMCOL pco00;
       If %found(SMCOL);
          If F2tpa = 'Y';
             scAdmFein# = COfein;
             scAdmFeinN = COname;
          Else;
             clear scAdmFein#;
             clear scAdmFeinN;
          EndIf;
       EndIf;

       // Load screen fields for Claim Administrator
       //***  Begin Del  ***  10/23/13  ********************************
       //**************** scClmNameA = COanam;
       //***  End   Del  ***  10/23/13  ********************************
       scAdmEmad=noEmail;
       scAdmEmadT=noEmail;
       //***  Begin Chg  ***  10/23/13  ********************************
       //**************** scClmAdLn1 = COadr1;
       //**************** If COadr2<>*Blanks;
          //************* scClmAdLn2 = COadr2;
          //************* scClmAdLn3 = %Trim(COcty) + ', ' + COst + ' ' +
          //*************  %EditW(COzip:'     -    ');
       //**************** Else;
          //************* scClmAdLn2 = %Trim(COcty) + ', ' + COst + ' ' +
          //*************  %EditW(COzip:'     -    ');
          //************* scClmAdLn3 = *Blanks;
       //**************** EndIf;
       //***  Begin Chg  ***  05/13/15  ********************************
            Chain KeyWDF3L PF300;
          If not %Found(WDF3L);
            Chain (CMco#:CMfnd) PF300;
          EndIf;
       //***  End   Chg  ***  05/13/15  ********************************
       If %Found(WDF3L);
          scClmNameA = F3name;
          scClmAdLn1 = F3adr1;
          If F3adr2<>*Blanks;
             scClmAdLn2 = F3adr2;
             scClmAdLn3 = %Trim(F3cty) + ', ' + F3st + ' ' +
             %EditW(F3zip:'     -    ');
          Else;
             scClmAdLn2 = %Trim(F3cty) + ', ' + F3st + ' ' +
             %EditW(F3zip:'     -    ');
             scClmAdLn3 = *Blanks;
          EndIf;
       EndIf;
       isClmAdmV = *Off;
       If scClmNameA <> *Blanks and isShowEDI = *On;
          isClmAdmV = *On;
       EndIf;
       //***  End   Chg  ***  10/23/13  ********************************
       scAdmClm#  = %editc(CMco#:'X') + %editc(CMfnd:'X') + %editc(CMfyr:'X') +
        %editc(CMcase:'X');

       // Load screen fields for Employment information
       scEmpOcpDs = CMocds;

       Chain keyWMEML pem00;
       Chain keyWDELL pel00;
       Chain keyWMALL pal00;

       clear kkTbId;
       clear kkkey;
       kkTbId = 'EMST';
       kkkey  = CMests;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scEmpSts = %Trim(GTkey) + ' - ' + GTdesc;
       Else;
          scEmpSts = CMests;
       EndIf;
       scEmpHrDt = CvtToDate(CMhidt);
       scEmpClsSE = %editc(CMclas:'X') + ' ' + CMspec + ' ' + CMext;

       SetGt keyWMPRL ppr00;
       ReadPe keyWMPRL# ppr00;
       If not %eof(WMPRL) and PRstat = 'A';
          %subSt(scEmpClsSE:16:30) = PRdesc;
       Endif;

       Chain keyWMEPL7E pep00;
       If %found(WMEPL7E) and EPext <> *blanks;
          ##st# = *blanks;
          SetGt keyWMPXL1 ppx00;
          ReadPe keyWMPXL1# ppx00;
          If %found(WMPXL1);
             %subSt(scEmpClsSE:16:30) = PXdesc;
          Else;
             Chain keyWMRSL prs00;
             If %found(WMRSL);
                ##st# = RSjrst;
                SetGt keyWMPXL1 ppx00;
                ReadPe keyWMPXL1# ppx00;
                If %found(WMPXL1);
                   %subSt(scEmpClsSE:16:30) = PXdesc;
                Endif;
             Endif;
          Endif;
       Endif;

       // Load screen fields for Insured information
       If ALdba > *blanks;
          scInsdENam = ALdba;
       Else;
          scInsdENam = EMname;
       Endif;

       kkPcnt='1';
       scEmpEmad=%trim(GetEmpEmail(CMCo#:CMFnd:CMemp#:CMdiv:kkPcnt));
       scEmpEmadT=scEmpEmad;
       If scEmpEmadT=*Blanks;
          scEmpEmadT=noEmail;
       EndIf;

       If ALpad1 > *blanks;
          scInsdLn1 = ALpad1;
          If ALpad2<>*Blanks;
             scInsdLn2 = ALpad2;
             scInsdLn3 = %Trim(ALpCty) + ', ' + ALpSt + ' ' +
              %EditW(ALpZip:'     -    ');
          Else;
             scInsdLn2 = %Trim(ALpCty) + ', ' + ALpSt + ' ' +
              %EditW(ALpZip:'     -    ');
             scInsdLn3 = *Blanks;
          EndIf;
       Else;
          If ALmad1 > *blanks;
             scInsdLn1 = ALmad1;
             If ALmad2<>*Blanks;
                scInsdLn2 = ALmad2;
                scInsdLn3 = %Trim(ALmCty) + ', ' + ALmSt + ' ' +
                 %EditW(ALmZip:'     -    ');
             Else;
                scInsdLn2 = %Trim(ALmCty) + ', ' + ALmSt + ' ' +
                 %EditW(ALmZip:'     -    ');
                scInsdLn3 = *Blanks;
             EndIf;
          Else;
             If EMpAd1 > *blanks;
                scInsdLn1 = EMpAd1;
                If EMpAd2<>*Blanks;
                   scInsdLn2 = EMpAd2;
                   scInsdLn3 = %Trim(EMpCty) + ', ' + EMpSt + ' ' +
                    %EditW(EMpZip:'     -    ');
                Else;
                   scInsdLn2 = %Trim(EMpCty) + ', ' + EMpSt + ' ' +
                    %EditW(EMpZip:'     -    ');
                   scInsdLn3 = *Blanks;
                EndIf;
             Else;
                If EMmAd1 > *blanks;
                   scInsdLn1 = EMmAd1;
                   If EMmAd2<>*Blanks;
                      scInsdLn2 = EMmAd2;
                      scInsdLn3 = %Trim(EMmCty) + ', ' + EMmSt + ' ' +
                       %EditW(EMmZip:'     -    ');
                   Else;
                      scInsdLn2 = %Trim(EMmCty) + ', ' + EMmSt + ' ' +
                       %EditW(EMmZip:'     -    ');
                      scInsdLn3 = *Blanks;
                   EndIf;
                Endif;
             Endif;
          Endif;
       Endif;

       If ALfein > *zeros;
          scInsdFein = ALfein;
       Else;
          scInsdFein = EMfein;
       Endif;
       If FNsfin = 'Y';
          scInsdSlf = 'Yes';
       Else;
          scInsdSlf = 'No';
       Endif;

       scinsdNam = EMname;
       scInsdPol# = %editc(CMfnd:'X') + %editc(CMemp#:'X') +
        %editc(CMfyr:'X');
       scInsdRpt# = CMecs#;
       scInsdINFe = CMiif;

       If EMnaic <> *blanks and CMjrst <> 'AR' and CMjrst <> 'KY';
          scInsdSIC = EMnaic;
       Else;
          If CMsic <> *blanks;
             scInsdSIC = CMsic;
          Else;
             scInsdSIC = EMsic;
          Endif;

          If scInsdSIC <> *blanks;
             clear kkTbId;
             clear kkkey;
             scInsdSICD = *blanks;
             kkTbId = 'SIC ';
             kkkey  = scInsdSIC;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scInsdSICD = GTdesc;
             Endif;
          Endif;

          If CMjrst = 'AR' or CMjrst = 'FL' or CMjrst = 'MO' or
           CMjrst = 'TN' or CMjrst = 'CA';
             scInsdSIC = %trim(scInsdSIC) + 'SC';
          Endif;
       Endif;

       scInsdFrDt = CvtToDate(ELpfdt);
       scInsdToDt = CvtToDate(ELptdt);

       If ALelid > *blanks;
          scInsdLoc# = ALelid;
       Else;
          scInsdLoc# = %editc(EMemp#:'X') + %editc(EMdiv:'X');
       Endif;
       scInsdLoc# = %trim(scInsdLoc#) + '  ' + ALname;

       // Load screen fields for Accident information
       scAcInjDt  = CvtToDate(CMacdt);
       scAcInjTm  = intactm;
       scAcZipSt  = %editc(CMacz1:'X') + '-' + %editc(CMacz2:'X');
       If CMeadr = 'Y';
          scAcEmpPrm = 'Yes';
       Else;
          scAcEmpPrm = 'No';
       Endif;

       scAcPerImp = CMipct;
       scAcDtRptE = CvtToDate(CMdtrp);

       clear scAcNtrInj;
       clear kkKey;
       kkKey=CMnatr;
       Chain keyWTABL pna00;
       If %Found(WTABL);
          scAcNtrInj = %Trim(CMnatr) + ' - ' + NAdesc;
       Else;
          scAcNtrInj = CMnatr;
       EndIf;
       scAcDesc = %trim(CMacds) + ' ' + %trim(CMacd2);
       scAcClmAdm = CvtToDate(CMrcvd);

       clear scAcBdPart;
       clear kkKey;
       kkKey=CMpart;
       Chain keyWTABL ppa00;
       If %Found(WTABL);
          scAcBdPart = %Trim(CMpart) + ' - ' + PAdesc;
       Else;
          scAcBdPart = CMpart;
       EndIf;

       //*** Begin Add *** 05/30/18 PG *************************************
       // Load screen fields for EDI Release 3.1  information
       clear kkTbId;
       clear kkkey;
       kkTbId = 'PLOC';
       kkkey  = CMloc;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scLoc = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scLoc = CMloc;
       EndIf;

       clear kkTbId;
       clear kkkey;
       kkTbId = 'PFTL';
       kkkey  = CMlocft;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scLocFT = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scLocFT = CMlocft;
       EndIf;
       //*** End   Add *** 05/30/18 PG *************************************

       scAcIntTrt = CMitrt;
       clear kkTbId;
       clear kkkey;
       kkTbId = 'INIT';
       kkkey  = scAcIntTrt;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scAcIntTrt = %trim(scAcIntTrt) + '  ' + %trim(GTdesc);
       Endif;

       clear kkKey;
       kkKey=CMcaus;
       Chain keyWTABL pca00;
       If %Found(WTABL);
          scAcCsInj = %Trim(CMcaus) + ' - ' + %trim(CAdesc);
       Else;
          scAcCsInj = CMcaus;
       EndIf;

       // Load screen fields for Employee information
       Chain keyWMCDL pcd00;
       scEmpNam = CDlsn;
       scClmEmad=CDemAd;
       scClmEmadT=CDemAd;
       If scClmEmadT=*Blanks;
          scClmEmadT=noEmail;
       EndIf;
       If scEmpNam <> *blanks;
          scEmpNam = %trim(scEmpNam) + ', ' + %trim(CDfsn) +', '+ %trim(CDmid);
       Else;
          scEmpNam = %trim(CDfsn) + ', ' + %trim(CDmid);
       Endif;

       scEmpLn1   = CDadr1;
       If ALpad2<>*Blanks;
          scEmpLn2 = CDadr2;
          scEmpLn3 = %Trim(CDcty) + ', ' + CDst + ' ' +
           %EditW(CDzip:'     -    ');
       Else;
          scEmpLn2 = %Trim(CDcty) + ', ' + CDst + ' ' +
           %EditW(CDzip:'     -    ');
          scEmpLn3 = *Blanks;
       EndIf;
       scEmpArea  = '(' + %editc(CDarea:'X') + ')';
       scEmpPhn   = CDphn#;

       clear kkTbId;
       clear kkkey;
       kkTbId = 'GNDR';
       kkkey  = CDsex;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scEmpGen = %Trim(GTkey) + ' - ' + GTdesc;
       Else;
          scEmpGen = CDsex;
       EndIf;

       scEmpDOB = CvtToDate(CDdtob);

       clear kkTbId;
       clear kkkey;
       kkTbId = 'MSTS';
       kkkey  = CDmsts;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scEmpMrtSt = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scEmpMrtSt = CDmsts;
       EndIf;

       // Retrieve # of Dependents
       scEmpDep# = 0;
       setll keyWDACL pac00;
       readE keyWDACL pac00;
       Dow not %eof(WDACL);
          If ACrel <> *blank;
             scEmpDep# = scEmpDep# + 1;
          Endif;
          readE keyWDACL pac00;
       Enddo;

       If scEmpDep# = *zeros;
          scEmpDep# = CM#dep;
       Endif;

       scEmpDsbDt = CvtToDate(CMdbdt);
       scEmpDthDt = CvtToDate(CMdod);
       scEmpId = CMeid;
       // ****** Begin Add ***** 12/12/13 ****** WC2234 ********** /
       scEmpEsid = CMesid ;
       // ****** End Add ***** 12/12/13 ****** WC2234 ********** /

       clear kkTbId;
       clear kkkey;
       kkTbId = 'EIDQ';
       kkkey  = CMeitq;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scEmpIdTQ = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scEmpIdTQ = CMeitq;
       EndIf;

       scEmpEntEx = CMenex;

       If CMaurl ='Y';
          scEmpMRAut = 'Yes';
       Else;
          scEmpMRAut = 'No';
       Endif;

       If CMssrl = 'Y';
          scEmpSS#At = 'Yes';
       Else;
          scEmpSS#At = 'No';
       Endif;

       If CMidth = 'Y';
          scEmpDthRs = 'Yes';
       Else;
          scEmpDthRs = 'No';
       Endif;

       If CMprex ='Y';
          scEmpPrDsb = 'Yes';
       Else;
          scEmpPrDsb = 'No';
       Endif;

       clear kkTbId;
       clear kkkey;
       kkTbId = 'ETFS';
       kkkey  = CMetfs;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scEmpTaxSt = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scEmpTaxSt = CMetfs;
       EndIf;

       // ****** Begin Add ***** 12/12/13 ****** WC2234 ********** /
          scBntPspa = CMpspa;
          Select;
       // Begin change  **** 02/12/14 **********************************/
       //*** When CMwwtc = 'E';
          When CMwwtc = 'S';
       // End   change  **** 02/12/14 **********************************/
            scBntWwtc = 'Standard Work Week';
          When CMwwtc = 'F';
            scBntWwtc = 'Fixed Work Week';
          When CMwwtc = 'V';
            scBntWwtc = 'Varied Work Week';
          Other ;
            scBntWwtc = ' ';
          EndSl;
       // Begin change  **** 02/12/14 **********************************/
       //*** Select;
       //*** When CMwdsc = 'S';
       //***   scBntWdsc = 'Scheduled';
       //*** When CMwdsc = 'N';
       //***   scBntWdsc = 'Not Scheduled';
       //*** Other ;
       //***   scBntWdsc = ' ';
       //*** EndSl;
         scWDSSu = %SUBST(CMWDS:1:1);
         scWDSM  = %SUBST(CMWDS:2:1);
         scWDST  = %SUBST(CMWDS:3:1);
         scWDSW  = %SUBST(CMWDS:4:1);
         scWDSR  = %SUBST(CMWDS:5:1);
         scWDSF  = %SUBST(CMWDS:6:1);
         scWDSSa = %SUBST(CMWDS:7:1);
       // End   change  **** 02/12/14 **********************************/
       // ****** End Add ***** 12/12/13 ****** WC2234 ********** /

       // Load screen fields for Benefit information
       scBntlstDt = CvtToDate(CMdtlw);

       If CMagcp = 'Y';
          scBntAgCmp = 'Yes';
       Else;
          scBntAgCmp = 'No';
       Endif;

       If CMfsal = 'Y';
          scBntSalCn = 'Yes';
       Else;
          scBntSalCn = 'No';
       Endif;

       If CMpoi = 'Y';
          scBntFWgPd = 'Yes';
       Else;
          scBntFWgPd = 'No';
       Endif;

       clear kkTbId;
       clear kkkey;
       kkTbId = 'FRWP';
       kkkey  = CMwgpd;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scBntWgPrd = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scBntWgPrd = CMwgpd;
       EndIf;

       scBntDays = CMdays;
       scBntAWW  = CMaww;
       scBntCmpRt = CMcrt;
       scBntMMIDt = CvtToDate(CMmmi);
       scBntRlsDt = CvtToDate(CMrlw);
       scBntRlsDt = CvtToDate(CMrlw);
       scBntRTWDt = CvtToDate(CMrtw);

       clear kkTbId;
       clear kkkey;
       kkTbId = 'RTWQ';
       kkkey  = CMrtwq;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scBntRTWQl = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scBntRTWQl = CMrtwq;
       EndIf;

       scBntDNtDt = CvtToDate(CMdodd);
       scBntDsFBn = CMdfbe;
       scBntIntLT = CvtToDate(CMidlt);
       scBntDtLDW = CvtToDate(CMcdlw);

       If CMpimp = 'Y';
          scBntMinPm = 'Yes';
       Else;
          scBntMinPm = 'No';
       Endif;

       clear kkTbId;
       clear kkkey;
       kkTbId = 'NCPD';
       kkkey  = CMncpc;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scBntNCPdC = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scBntNCPdC = CMncpc;
       EndIf;

       scBntNDtLT = CvtToDate(CMdnlt);
       scBntDsbDt = CvtToDate(CMcddb);

       clear kkTbId;
       clear kkkey;
       kkTbId = 'RBAC';
       kkkey  = CMrbac;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scBntAmtCd = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scBntAmtCd = CMrbac;
       EndIf;

       If CMsame = 'Y';
          scBntRTWSm = 'Yes';
       Else;
          scBntRTWSm = 'No';
       Endif;

       scClmSts = CMstat;
       scClmLTSts = CMltfl;
       //*** Begin Add ***** 03/04/14 *************************************
       clear kkTbId;
       clear kkkey;
       kkTbId = 'MCO ';
       kkkey  = CMmctp;
       Chain keyWTGTL pgt00;
       If %Found(WTGTL);
          scClmMcoCd = %Trim(GTkey) + ' - ' + %trim(GTdesc);
       Else;
          scClmMcoCd = CMmctp;
       EndIf;
       //*** End   Add ***** 03/04/14 *************************************
       scClmMCONm = CMmcnm;
       scClmMCOFe = CMmcof;
       Chain keyWMLLL pll00;
       If %found(WMLLL);
          scClmDtAtt = CvtToDate(LLdtap);
       Endif;

       If CModis = 'Y';
          scClmOcpDs = 'Yes';
       Else;
          scClmOcpDs = 'No';
       Endif;

       If CMacqr = 'Y';
          scClmAcqr = 'Yes';
       Else;
          scClmAcqr = 'No';
       Endif;

       If CMdny = 'Y';
          scClmDny = 'Yes';
       Else;
          scClmDny = 'No';
       Endif;

       // Create URL for EDI log history
       ##co#  = %editc(CMco#:'X');
       ##fnd  = %editc(CMfnd:'X');
       ##fyr  = %editc(CMfyr:'X');
       ##case = %editc(CMcase:'X');
       ##rrn  = %editc(inRRNClm:'X');

       call=' ';
       If dspMode=*On;
          call='0';
       EndIf;

       scEDILgUrl=CreateURL('W0875RW':'EDI Log History':'0':'0':call:' ':##co#:
        ##fnd:##fyr:##case:##rrn);

       // Create URL for CMS log history
       scCmpURL=CreateURL('W7485RW':'CMS Log History':'0':'0':' ':##rrn);

       // Create URL for CMS additional claimants
       scAddClURL=CreateURL('W0572RW':'CMS Additional Claimants':call:##rrn:
        '0':'0');

       // Create URL for A49 Add/Change
       scEdiA49Ur=CreateURL('W0080R1':'A49 Add/Change':call:' ':##fyr:##case:
        ##fnd:' ');

       // Create URL for A148 Change
       scA148Url=CreateURL('W0070R6':'A148 Change':##co#:##fnd:##fyr:##case:
        ' ':' ':' ');

       // Retrieve the Data Element Criteria
       clear CF;
       clear FD;
       intCnt = *zeros;

       setll keyWTDCL pdc00;
       reade keyWTDCL pdc00;
       dow not %eof(WTDCL);
          If DCset = '148' or DCset = *blanks;
             intCnt = DCdn#;
             If intCnt <= 99 and intCnt > 0;
                If CF(intCnt) = *blanks;
                   CF(intCnt) = DCcrtr;
                Endif;
             Endif;
          Endif;
          reade keyWTDCL pdc00;
       enddo;

       intCnt = 1;
       Dow intCnt <= 99;
          If CF(intCnt) <> *blanks;
             FD(intCnt) = '(' + %trim(CF(intCnt)) + ')';
          Endif;
          intCnt = intCnt + 1;
       Enddo;
      /End-Free
     P MoveDbSc        E
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy sCopyClmPi
       //*** Beg Add *** 08/19/19 ******************************************
      //===================================================================
      // ClrSubFiles - Clears SubFile
      //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free
       // Transactional
       dspIDCTran = *off;
       dspIDCCtl  = *off;
       clrIDCSfl  = *on;
       RrnIDCTran = 0;
       // Quarterly
       dspIDCQtr  = *off;
       dspQtrCtl  = *off;
       clrQtrSfl  = *on;
       RrnIDCQtr  = 0;
       Write RcdIDCInfo;
      /End-Free
     p ClrSubFiles     e

     //===================================================================
     // Load Prior Transactional and Quarterly Submissions Grid Procedure
     //===================================================================
     p LoadGrids       b
      /Free
       // Transactional
       dspIDCTran = *On;
       dspIDCCtl  = *On;
       clrIDCsfl = *Off;
       rrnIDCTran = *zeros;
       scRRN  = 0;
       // Quarterly
       dspIDCQtr  = *On;
       dspQtrCtl  = *On;
       clrQtrsfl = *Off;
       rrnIDCQtr  = *zeros;
       scRRNQtr  = 0;

       // Transactional
       Setll (CMco#:CMfnd:CMfYr:CMcase) ptr02;
       Reade (CMco#:CMfnd:CMfYr:CMcase) ptr02;
       Dow Not %Eof(wmidctrl);
       // Only write the record if batch ID is in Submission Control File
          Chain TRbchId pcc02;
          If %Found(wmidcscl);
             scSubFilID = CCSubFilID;
             xxDate = %Dec(CCsubDate:8:0);
             scSubDt = cvtToDate(xxDate);
             Select;
             When CCsubTyCd = 'O';
                scSubFtyCd = 'Original';
             When CCsubTyCd = 'R';
                scSubFtyCd = 'Replace';
             Endsl;
             Select;
             When CCrptQtrCd = '1';
                scPrRptQtr = '1st';
             When CCrptQtrCd = '2';
                scPrRptQtr = '2nd';
             When CCrptQtrCd = '3';
                scPrRptQtr = '3rd';
             When CCrptQtrCd = '4';
                scPrRptQtr = '4th';
             Endsl;

             Monitor;
             xxDate = %Dec(TRtrnFrDt:8:0);
             scFrDt = cvtToDate(xxDate);
             xxDate = %Dec(TRtrnToDt:8:0);
             scToDt = cvtToDate(xxDate);
             scAmt  = %Dec(TRtrnAmt:12:0)/100;
             On-Error;
             Endmon;

             rrnIDCTran = rrnIDCTran + 1;
             scRRN  = trFds.rrn;
             SelTrans = *Off;
             If rrnIDCTran = 1;
                SelTrans = *on;
             Endif;
             write sflIDCTran;
          Endif;
          Reade (CMco#:CMfnd:CMfYr:CMcase) ptr02;
       Enddo;
       rrnIDCTran = 1;

       // Quarterly
       Setll (CMco#:CMfnd:CMfYr:CMcase) pqt01;
       Reade (CMco#:CMfnd:CMfYr:CMcase) pqt01;
       Dow Not %Eof(wmidcqtl);
          scBchIDqtr = QTbchId;
          Chain QTbchId pcc02;
       // Only write the record if batch ID is in Submission Control File
          If %Found(wmidcscl);
             scSbFIDQtr = CCSubFilID;
             xxDate = %Dec(CCsubDate:8:0);
             scSubDtQtr = cvtToDate(xxDate);
             Select;
             When CCsubTyCd = 'O';
                scSFTCdQtr = 'Original';
             When CCsubTyCd = 'R';
                scSFTCdQtr = 'Replace';
             Endsl;
             Select;
             When CCrptQtrCd = '1';
                scPrRpQtrQ = '1st';
             When CCrptQtrCd = '2';
                scPrRpQtrQ = '2nd';
             When CCrptQtrCd = '3';
                scPrRpQtrQ = '3rd';
             When CCrptQtrCd = '4';
                scPrRpQtrQ = '4th';
             Endsl;

             rrnIDCQtr = rrnIDCQtr + 1;
             scRRNQtr  = qtFds.rrn;
             SelQtr = *Off;
             If rrnIDCQtr = 1;
                SelQtr = *on;
             Endif;
             write sflIDCQtr;
          Endif;
          Reade (CMco#:CMfnd:CMfYr:CMcase) pqt01;
       Enddo;
       rrnIDCqtr = 1;
      /End-Free
     p LoadGrids       e
       //*** End Add *** 08/19/19 ******************************************
