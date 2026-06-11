     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE FTFSRULWDW - Fee Schedule Rules viewing
     H DFTNAME(FTFSRULWDW)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('FTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 08/05/16                                         *
      *  Programmer: C:212                                            *
      *  Project   : FS498                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *  05/28/25 C:342 Support: 119130                               *
      *                 1.Return to Rule Selection popup screen when  *
      *                   we hit cancel on State Rules Maintenance    *
      *                   screen.                                     *
      *                                                               *
      *  5/14/2018 C:079 Support: 99 70950
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fFTFSRULWDWcf   e             workstn extfile('FTFSRULWDW')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRules:rrnRules)
     ffarulp    uf a e             disk    rename(prul01:prul0p)
     ffarull1   if   e           k disk    rename(prul01:prul01)
     f                                     infds(dbfds1)
     ffdcap     if   e             disk    rename(pca01:pca0p)
     ffdfrl0    if   e           k disk
     f                                     infds(dbfds2)

     d* procedure definitions
     d*
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  rrnfarull1           397    400b 0

     d dbfds2          ds
     d  rrnfdfrl0            397    400b 0

     d                 ds
     d  @@cmd                  1     56
     d  @@st                  38     39

     d* array definitions
     d*

     d* standalone variables
     d*
     D inCmd           s           2750
     D inCmdLen        s             15  5

     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d inmode          s              1    inz
     d process         s               n   inz(*off)
     d inRrnCA         s              9  0 inz
     d inRrnFR         s              9  0 inz
     d outRrnFR        s              9  0 inz
     d company         s              3p 0 inz
     d kkdseq          s                   like (RULdseq) inz
     d kkst            s                   like (FRst) inz
     d kkrule          s                   like (FRrule) inz
     d kkefdt          s                   like (FRefdt) inz

     d* Clear Subfiles
     d clrSubFiles     pr

     d* load rules grid
     d loadRules       pr

     d* main Processing
     d main            pr

     d* process other options
     d procOther       pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d/copy wPrUsrPrms

     C* key list definitions
     C*
     C     ruky          klist
     C                   kfld                    CAco#
     C                   kfld                    CAsub#
     C                   kfld                    CAfnd
     C                   kfld                    CAfyr
     C                   kfld                    CAcase
     C                   kfld                    CAeob#
     C                   kfld                    kkDSEQ

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrnCA           9            fdcap
     c                   Parm                    @@rrnFR           9            fdfrp
     c                   Parm                    @@mode            1            mode
     c                   Parm                    @@TabInd          5            Tab index
     c*** Beg Add *** 05/28/25 ************************************************
     c                   Parm                    @@Exit            1            Exist Button
     c*** End Add *** 05/28/25 ************************************************

      /free

       // main program processing

       // RRN for FDCAP
       If %parms >= 1;
          inRrnCA = %int(@@rrnCA);
       Else;
          inRrnCA = 0;
       Endif;

       // RRN for FDFRP
       If %parms >= 2;
         If %int(@@rrnFR) = *zeros;
           inRrnFR = 1;
         Else;
           inRrnFR = %int(@@rrnFR);
         Endif;
       Else;
          inRrnFR = 1;
       Endif;

       // Program Mode.
       If %parms >= 3;
          inmode = @@mode;
       Endif;

       // Tab Index
       If %parms >= 4;
          TabIndex = %dec(@@TabInd:5:0);
       Else;
          TabIndex = 0;
       Endif;

       ProgramStart(process);

       If process;
          Main();
          @@rrnFR = %Editc(outRrnFR:'X');
       Endif;

       Unlock FARULP;
       *inlr = *on;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnRules = 0;
       clrRulSub = *On;
       dspRulSub = *Off;
       dspRulCtl = *Off;
       Write rcdRules;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // Load Rules Information
       //===================================================================
     PLoadRules        B
     dLoadRules        PI
      /free

       // set subfile indicators
       clrRulSub = *Off;
       dspRulSub = *On;
       dspRulCtl = *On;

       // read file and load the subfile
       kkdseq = CAdseq;
       Setgt RUky prul01;
       Readpe RUky prul01;
       dow not %eof(farull1);
          clear sflRules;

          scRulRule = RULrule;
          scRulDesc = RULdesc;

          scRulRRN# = rrnfarull1;

          rrnRules = rrnRules + 1;
          If rrnRules = 1;
             scRulRowSl = 'A';
             scRuleTTip = 'Final payment/adjustment was applied by this rule';
          Else;
             clear scRulRowSl;
             scRuleTTip = *blanks;
          Endif;
          If inmode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;

          Write sflRules;

          Readpe RUky prul01;
       Enddo;

       rrnRules = 1;

      /end-free
     PLoadRules        E
      //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Clear Sub-files
          ClrSubfiles();

          // Load Rules Information
          LoadRules();

          // Process Rules screen.
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh =*Off;
          //*** Beg Add *** 05/14/18 *********************************
          When BtnExit = *on;
             //*** Beg Add *** 05/28/25 *********************************
             @@Exit = '1';
             //*** End Add *** 05/28/25 *********************************
             Leave;
          //*** End Add *** 05/14/18 *********************************

          // Other options processing
          Other;
             procOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

        When TabIndex = 0;
           TabHeight = 1050;
           Write rcdTabPan;
           Exfmt rcdRules;
           Read rcdTabPan;
        EndSl;

       Endsr;

      /end-free
     P Main            E
       //===================================================================
       // procOther - process other options
       //===================================================================
     P procOther       B
     d procOther       PI
      /free

       ReadC sflRules;
       Dow not %eof;

         Select;

          // processing for Select link
          When scRulSel = *on;
             scRulSel = *off;
             // get the rrn from Rule file for the rule selected
             kkst = CAast;
             kkrule = scRulRule;
             kkefdt = *hival;
             Setgt (kkst:kkrule:kkefdt) pfr01;
             Readpe (kkst:kkrule) pfr01;
             if not %eof(fdfrl0);
               outRrnFR = rrnfdfrl0;
             else;
               outRrnFR = *zeros;
             endif;
             ExitNow = *on;

         EndSl;

         ReadC sflRules;
       Enddo;

      /end-free
     P procOther       E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // default as on
       processMe = *on;

       // If Rrn is passed, verify
       If inRrnCA > *zeros;

         // Chain to Case File
         Chain(n) inRrnCA pca0p;

         // Process only if valid record found
         If not %found(fdcap);
          processme = *Off;
          Return;
         Endif;

       Else;
          processme = *Off;

       EndIf;

       Select;
        When inmode = 'D';
           dspMode = *On;
           dspVisb = *Off;
        When inmode = 'A';
           addMode = *On;
           dspVisb = *On;
        When inmode = 'C';
           chgMode= *On;
           dspVisb = *On;
        Other;
           inMode = 'D';
           dspMode = *On;
           dspVisb = *Off;
       Endsl;

       // get company of the user
       // company = GetUserInfo(q1user);

       Return;

      /end-free
     P ProgramStart    E
       //===================================================================

