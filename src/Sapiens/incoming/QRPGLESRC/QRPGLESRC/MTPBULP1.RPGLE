     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h/title MTPBULP1 - Provider Bill Upload Process Part 1
     h dftname(MTPBULP1)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define profoundUI
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 05/11/20                                         *
      //  Programmer: C:212                                            *
      //  Project   : MC437 - Redesign of PB Upload process per CS strd*
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
     fmtpbulp1  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')

      * company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * file member listing file
     ffxu0l     if   e           k disk    rename(qwhfdml:pu000)
      * upload doc name file
     ffdocp     if   e             disk    rename(poc01:poc0p)
      * bill upload receive file
     ffprvp     o    e             disk    rename(ppr01:ppr0p)

     d F8039C          pr                  extpgm('F8039C')
     d  inMember                     10
     d  inDocId                     500

     d* prototype declaration
     d*
      // get client id
     d getClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* main processing
     d main            pr

     d* program start
     d programStart    pr
     d processMe                       n

     d*
     d/copy sPrGenPop
     d/copy sPrUsrPrms

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d*
     d                 DS
     d  dsDtTm                 1     14  0
     d  dsTime                 1      6  0
     d  dsDate                 7     14  0
     d*
     d                 DS
     d  dsMbr1                 1     10
     d  ltrl1                  1      1    INZ('P')
     d  yyyear                 2      5  0
     d  yydays                 6      8  0
     d  date1                  2      8  0
     d  seq1                   9     10  0
     D*
     D                 DS
     D  dsMbr2                 1     10
     D  ltrl2                  1      1    INZ('P')
     D  date2                  2      8  0
     D  seq2                   9     10  0

     D                 DS
     D  dsDocId                1    500
     D  oc                            1    dim(500)

     d* array definitions
     d*
     d* constant definitions
     d*
     d* standalone variables
     d*
     d x               s              3  0 inz
     d xxRrn           s              9  0 inz
     d intCo#          s              3  0 inz
     d outCltId        s              3    inz
     d kkName          s             10    inz
     d inMember        s             10    inz
     d inDocId         s            500    inz
     d process         s               n   inz(*off)
     d isCallP         s               n   inz(*off)
     d isConfmsg       s              1    inz(*off)
     d isProcessed     s               n   inz(*off)
     d strConfmsg      s            256    inz(*off)
     d dtJul           s              7  0 inz

     c* plist and klist declaration
     c*
     c     datchk        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      *blanks       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1
     c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *BLANKS       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *BLANKS       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     dtljul        plist
     c                   parm                    srdti
     c                   parm      '*MDYY'       srffmt
     c                   parm      '*JUL'        srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     u0ky          klist
     c                   kfld                    kkName

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@call            1            wdelp
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@mode            1            mode
     c                   Parm                    @@tabind          5            Tab index

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
       // Main - Main processing
      //===================================================================
     p main            b
     d main            pi
      /free

        // Process until exit button is pressed.

        btnCancel = *off;
        ExitNow   = *off;
        isProcessed = *off;

        // process until exit button is pressed.
        DoW btnCancel = *Off;

           // display screen
           Exsr exfmtscreens;

           // perform actions on screen
           Select;

           When ExitNow = *On;
              ExitNow = *Off;
              Leave;

           // if process is complete
           When isProcessed = *on;
              Leave;

           // if user selected to cancel the process
           When isConfmsg = *on;
              leave;

           // if user selected to execute the process
           When isConfmsg = *off;
              Exsr procfile;

           // do nothing - catch tab click event
           When tabClick = *on;
              tabClick = *off;

           When btnRefresh = *On;
              btnRefresh =*Off;

           // Enter/Save
           When btnSave = *on;
              btnSave = *off;

           //Other options processing
           Other;
            // procOther();

           Endsl;

        Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When tabindex = 0;
           pnlHeight = 1050;
           aPnlHeight = pnlHeight + 60;

           Write rcdSysTbl;
           Write rcdTabPan;

           // if process is not executed, pop window to confirm submission
           If isProcessed = *off;
              strConfmsg = 'Please make sure that the most current Upload +
                            document (in the format of Upload.txt) is +
                            in the Upload folder.';
              isConfmsg = PopTwoBtn(strConfmsg:'Bill Upload Process Part 1':
                          'Cancel':'Execute');

           // if process is executed, show pop window to update
           Else;
              strConfmsg = 'The upload part 1 process has been completed.';
              popOneBtn(strConfmsg:'Bill Upload Process Part 1':'OK');

           EndIf;

        Other;
           // default at tab 0
           tabindex = 0;
        EndSl;

       Endsr;

       //===================================================================
       // Assign member for the file data
       //===================================================================
       BegSr AssMem;

        // Assign a member to the current file data
        dsDate = %Dec(%Char(%Date():*usa0):8:0);
        dsTime = %Dec(%Char(%Time():*hms0):6:0);
        dtJul = %Dec(%Date():*longjul);
        yyyear = %Int(%Subst(%Char(dtJul):1:4));
        yydays = %Int(%Subst(%Char(dtJul):5:3));

        seq1 = 99;
        kkName = dsMbr1;
        Setgt u0ky pu000;
        ReadP pu000;
        If not %Eof(fxu0l) and
          MLname <> 'FPRVP';
           dsMbr2 = MLname;
           If date1 = date2;
              seq1 = seq2 + 1;
           Else;
              seq1 = 0;
           EndIf;
        Else;
           seq1 = 0;
        EndIf;

       Endsr;

       //===================================================================
       // Process File
       //===================================================================
       BegSr ProcFile;

        // Assign a member for the current file data
        Exsr assmem;

        Clear poc0p;
        Setll 1 poc0p;
        Read poc0p;
        If not %Eof(fdocp) or isCallP = *off;
           If isCallP = *on;
              // filter out characters that can not be displayed
              Clear oc;
              oc = OCdcid;
              For x = 1 to 500;
                 If oc(x) < X'40';
                    if oc(x) <> X'08' and oc(x) <> X'09';
                       oc(x) = X'40';
                    endif;
                 endIf;
                 If oc(x) > X'F9';
                    oc(x) = X'40';
                 EndIf;
              EndFor;
              OCdcid = dsDocid;
              Open fprvp;
              Clear ppr0p;
              PRdata = 'H' + OCdcid;
              Write ppr0p;
              Close fprvp;
              indocid = OCdcid;
           Else;
              indocid = *blanks;
           EndIf;

           // call the process to submit the list
           inmember = dsMbr1;
           F8039C(inmember:indocid);

           // turn on indicator to mark processing completion
           isprocessed = *on;
        EndIf;

       Endsr;

      /end-free
     p main            e

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p programStart    b
     d programStart    pi
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
       If q1prms >= 3;
          Select;
          When @@mode = 'D';
             //dspMode = *on;
          Endsl;
       Endif;

       tabindex = 0;
       If q1prms >= 4;
          If @@tabind <> ' ';
             tabIndex = %Int(@@tabind);
          Endif;
       Endif;

       // apShowMenu = *on;
       aMenHeight = 145;
       apFootHght = aMenHeight + 125;

       // get company of the user
       intCo# = getUserInfo(q1user);
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
