     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h/title MTPBULP2 - Provider Bill Upload Process Part 2
     h dftname(MTPBULP2)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define profoundUI
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 05/11/20                                         *
      //  Programmer: C:212                                            *
      //  Project   : MC437 - Redesign of PB Upload process            *
      //===============================================================*
      //  Revisions:                                                   *
      *                                                                *
      *                                                                *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     f* file definitions
     f*
     fmtpbulp2  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflMbrDtl:rrnMbrDtl)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')

      * company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * upload bill trans info
     ffau9l1    if a e           k disk    rename(pu901:pu900)
     f                                     infds(dbfds2)
      * upload bill trans info
     ffau9l2    if   e           k disk    rename(pu901:pu901)
      * file member listing file
     ffxu0l     if   e           k disk    rename(qwhfdml:pu001)
     f                                     infds(dbfds)

     d* prototype declaration
     d*
     d* program to submit the transfer job
     d F8040C          pr                  extpgm('F8040C')
     d  inRrn                         9
     d  inName                       10
     d  inMbr                        10
     d  inCmpy                        1

     d* get client id
     d getClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* load the member detail grid
     d loadMbrDtl      pr

     d* main Processing
     d main            pr

     d* process other options
     d procOther       pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d rtvCltId        pr                  extPgm('RTVCLTID')
     d                                3
     d*
     d/copy sPrGenPop
     d/copy sPrUsrPrms

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  rrnfxuop             397    400b 0
     d dbfds2          ds
     d  rrnfau9p             397    400b 0

     d                 ds
     d  kkMbr#                 1     10
     d  yyjul                  2      8  0
     d  B                      1      1
     d  yyyear                 2      5  0
     d  yydays                 6      8  0
     d  seq#                   9     10  0

     d                 ds
     d  u9mbr#                 1     10
     d  yyjul2                 2      8  0
     d  xxseq#                 9     10  0

     d                 ds
     d  dsDtTm                 1     14  0
     d  dsTime                 1      6  0
     d  dsDate                 7     14  0

     d* array definitions
     d*
     d* constant definitions
     d*
     d* standalone variables
     d*
     d x               s              3  0 inz
     d intCo#          s              3  0 inz
     d sfrrn           s              9  0 inz
     d outCltId        s              3    inz
     d kkName          s             10    inz
     d process         s               n   inz(*off)
     d isCallP         s               n   inz(*off)
     d indFltr         s               n   inz(*off)
     d inRrn           s              9    inz
     d inName          s             10    inz
     d inMbr           s             10    inz
     d dtJul           s              7  0 inz

     c* plist and klist declaration
     c*
     c     u9ky2         klist
     c                   kfld                    kkMbr#

     c* entry of program.
     c     *Entry        Plist
     c                   parm                    @@call            1            wdelp
     c                   parm                    @@rtcd            1            wdelp
     c                   parm                    @@cmpy            1            wdelp
     c                   parm                    @@rrn             9            wdelp
     c                   parm                    @@mode            1            mode
     c                   parm                    @@tabind          5            Tab index

     c/free

       // main program processing

       programStart(process);

       If process;

          // perform main processing
          main();

       Endif;

       *inlr = *on;
       Return;

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p clrSubFiles     b
     d clrSubFiles     pi
      /Free

       rrnMbrDtl = 0;
       clrMDsub = *On;
       dspMDSub = *Off;
       dspMDCtl = *Off;
       Write rcdMbrDtl;

      /End-Free
     p clrSubfiles     e

       //===================================================================
       // Load Member Detail Information
       //===================================================================
     p loadMbrDtl      b
     d loadMbrDtl      pi
      /free

       // set subfile indicators
       clrMDsub = *Off;
       dspMDSub = *On;
       dspMDCtl = *On;


       // read file and load the subfile
       kkName = *blanks;

       Setll *Start fxu0l;
       Read pu001;
       DoW not %Eof(fxu0l);
          kkName = MLname;

          // check if the record meets the criteria to be written
          Exsr chkfilter;
          If indFltr = *on;

             // check if a record is not already present with this name
             Chain(n) kkName pu900;
             If not %Found(fau9l1);
                Clear pu900;

                // build member name
                Exsr bldmbr;

                // write to file
                U9name = MLname;
                U9mbr# = kkMbr#;
                U9uldt = %Dec(%Char(%Date():*cymd0):7:0);
                U9ultm = %Dec(%Char(%Time():*iso0):6:0);
                U9cvdt = *zeros;
                U9cvtm = *zeros;
                U9prdt = *zeros;
                U9prtm = *zeros;
                U9stat = 'A';
                U9tcnt = *zeros;
                U9dcnt = *zeros;
                U9ccnt = *zeros;
                U9rcnt = *zeros;
                U9ucnt = *zeros;
                U9pgm  = q1pgm;
                U9user = q1user;
                U9dt   = U9uldt;
                U9time = U9ultm;
                U9ipgm = q1pgm;
                U9iusr = q1user;
                U9idt  = U9uldt;
                U9itim = U9ultm;

                Write pu900;
             EndIf;

             // write to subfile
             Clear sflMbrDtl;

             scMDRrn# = rrnfau9p;
             scMDName = U9name;
             scMDMbrDt = %Date(U9uldt:*cymd);
             scMDMbr# = U9mbr#;

             rrnMbrDtl = rrnMbrDtl + 1;
             If rrnMbrDtl = 1;
                scMDRowSl = 'A';
             Else;
                Clear scMDRowSl;
             Endif;

             If dspmode = *on;
                dspVisb = *off;
             Else;
                dspVisb = *on;
             Endif;

             Write sflMbrDtl;
          EndIf;

       Read pu001;
       EndDo;
       rrnMbrDtl = 1;

       //===================================================================
       // BldMbr - Build Member Number
       //===================================================================
       Begsr bldmbr;

        yyjul2 = *zeros;
        xxseq# = *zeros;
        B = 'B';
        dsDate = %Dec(%Char(%Date():*usa0):8:0);
        dsTime = %Dec(%Char(%Time():*hms0):6:0);
        dtjul = %Dec(%Date():*longjul);
        yyyear = %Int(%Subst(%Char(dtjul):1:4));
        yydays = %Int(%Subst(%Char(dtjul):5:3));

        seq# = 99;
        Setgt u9ky2 pu901;
        ReadP pu901;
        If not %Eof(fau9l2) and
          yyjul = yyjul2;
           If xxseq# = 99;
              seq# = 0;
           Else;
              seq# = xxseq# + 1;
           EndIf;
        Else;
           seq# = 0;
        EndIf;

       Endsr;

       //===================================================================
       // ChkFilter - Check if filtering criteria to load records is met
       //===================================================================
       Begsr chkfilter;

        indFltr = *on;

        If MLname = 'FPRVP';
           indFltr = *off;
        EndIf;

        If %Subst(MLname:1:1) = 'O';
           indFltr = *off;
        EndIf;

        Chain(n) kkName pu900;
        If %Found(fau9l1);
           If U9cvdt <> *zeros and
             U9prdt <> *zeros;
              indFltr = *off;
           EndIf;
        EndIf;

       Endsr;

     p loadMbrDtl      e

      //===================================================================
       // Main - Main processing
      //===================================================================
     p main            b
     d main            pi
      /free

        // Process until exit button is pressed.

        btnCancel = *off;
        ExitNow   = *off;

        // clear sub-files
        clrSubfiles();

        // load member detail info
        loadMbrDtl();

        // process until exit button is pressed.
        Dou ExitNow = *On;

           // display screen
           Exsr exfmtscreens;

           // perform actions on screen
           Select;

           // do nothing - catch tab click event
           When tabClick = *on;
              tabClick = *off;

           When btnRefresh = *On;
              btnRefresh =*Off;
              clrSubfiles();
              loadMbrDtl();

           // enter button pressed
           When btnSave = *on;
              btnSave = *off;

           // F12 pressed
           When btnCancel = *on;
              btnCancel = *off;
              ExitNow = *on;

           // Cancel button pressed
           When scMDCancel = *on;
              scMDCancel = *off;
              ExitNow = *on;

           //Other options processing
           Other;
              procOther();

           Endsl;

        Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       Begsr exfmtscreens;

        Select;

        When tabindex = 0;
           pnlHeight = 1050;
           aPnlHeight = pnlHeight + 60;

           Write rcdSysTbl;
           Write rcdTabPan;
           Exfmt rcdMbrDtl;
           Read rcdTabPan;
           Read rcdSysTbl;

        Other;
           // default at tab 0
           tabindex = 0;
        EndSl;

       Endsr;

     p main            e

       //===================================================================
       // procOther - process other options
       //===================================================================
     p procOther       b
     d procOther       pi
      /free

       ReadC sflMbrDtl;
       DoW not %Eof;
          Select;
           // processing for Select link
           When scMDSel = *on;
              If scMDPrFl <> 'Y';
                 scMDSel = *off;

                 // process the transfer of the selected entry
                 inRrn    = %EditC(scMDRrn#:'X');   //u9rrn
                 inName   = scMDName;               //U9Name - FPRVP mbr
                 inMbr    = scMDMbr#;               //U9Mbr# - FAU0P mbr
                 F8040C(inRrn:inName:inMbr:@@Cmpy);

                 sfrrn = rrnMbrDtl;
                 Chain sfrrn sflMbrDtl;
                 If %Found;
                    scMDPrFl = 'Y';
                    scMDMsg = 'Submitted';
                    Update sflMbrDtl;
                 EndIf;
              Else;
                 sfrrn = rrnMbrDtl;
                 Chain sfrrn sflMbrDtl;
                 If %Found;
                    scMDMsg = 'Already Submitted';
                    Update sflMbrDtl;
                 EndIf;
              EndIf;

          EndSl;
          ReadC sflMbrDtl;
       EndDo;

      /end-free
     p procOther       e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p programStart    b
     d ProgramStart    pi
     d processMe                       n

      /free

       processMe = *on;

       If q1prms >= 1;
          If @@call = 'P';
             isCallP = *on;
          Else;
             isCallP = *off;
          EndIf;
       Endif;

       // Program Mode
       If q1prms >= 5;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          Endsl;
       Endif;

       tabIndex = 0;
       If q1prms >= 6;
          If @@tabind <> ' ';
             tabIndex = %Int(@@tabind);
          Endif;
       Endif;

       // apShowMenu = *on;
       aMenHeight = 145;
       apFootHght = aMenHeight + 125;

       // get company of the user
       intCo# = GetUserInfo(q1user);
       Chain intCo# pco00;
       If COname <> ' ';
          scCompName = COName;
       Endif;

       // Get Client ID
       getClientID(outCltId);

       If not %Open(STACTPNL);
          Open STACTPNL;
       Endif;

      /end-free
     p programStart    e

      /end-free
