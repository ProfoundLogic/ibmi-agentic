     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     h/title MTPBULP1 - Uploaded Bill Transfer History
     h dftname(MTPBULTRHS)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define profoundUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/11/20                                         *
      *  Programmer: C:212                                            *
      *  Project   : MC437 - Redesign of PB Upload process            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *  06/25/25  C:342 Support : 146/119271                         *
      *                  Monitor Statement when converting Injury     *
      *                  date while transferring Bills.               *
      *                                                               *
      *  10/05/23  C:079 Support : 146/114210                         *
      *                  Give detailed recon error                    *
      *                                                               *
      *  09/07/23  C:079 Support : 146/113770                         *
      *                  Add Monitor to provider number               *
      *                                                               *
      *  08/25/23  C:079 Support : 133/113672                         *
      *                  Edit for Claim# being blanks on Transfer     *
      *                                                               *
      *  01/25/21  C:079 Support : 152/94138                          *
      *                  Add Monitor to date conversions              *
      *                                                               *
      *  06/09/20  C:212 Project : SY420                              *
      *                  Mask SSN                                     *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fmtpbulerr cf   e             workstn extfile('MTPBULERR')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflUplErr:rrnUplErr)
     f                                     Sfile(sflErrSts:rrnErrSts)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      * company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * upload payment header filer
     ffau1p     uf a e             disk    rename(pu101:pu10p)
      * upload payment header filer
     ffau1l3    uf   e           k disk    rename(pu101:pu102)
     f                                     infds(dbfds)
      * PPO network file
     fnmnwp     if   e             disk    rename(pnw01:pnw0p)
      * PPO network file
     fnmnwl     if   e           k disk    rename(pnw01:pnw00)
      * upload payment detail file
     ffau2l1    uf   e           k disk    rename(pu201:pu200)
      * company auxiliary file
     ffmc1l     uf   e           k disk    rename(pc101:pc100)
      * claim file MC
     ffmcmlb    if   e           k disk    rename(pcm01:pcm01)
      * claim file MC
     ffmcml1    if   e           k disk    rename(pcm01:pcm02)
      * claim file MC
     ffmcmp     if   e             disk    rename(pcm01:pcm0p)
      * WCMC provider master file
     ffmpdl     if   e           k disk    rename(ppd01:ppd00)
      * bill upload error status
     ffau6p     uf a e             disk    rename(pu601:pu60p)
      * bill upload error status
     ffau6l     if   e           k disk    rename(pu601:pu600)
     f                                     infds(dbfds2)

     d* procedure declaration
     d*
     d* work with selected SS#
     d F0267R          pr                  extpgm('F0267R')
     d  ppKC                          1
     d  ppKL                          1
     d  ppCall                        1
     d  outRrn                        9  0
     d  ppSS#                         9
     d  ppAcdt                        8  0
     d  ppSDOI                        1
     d*
     d* medical case selection
     d F0269R          pr                  extpgm('F0269R')
     d  ppCall                        1
     d  ppSub#                        5  0
     d  ppFnd                         3  0
     d  ppFyr#                        3  0
     d  ppCase                        7  0
     d  ppSS#                         9
     d  ppAcdt                        8  0
     d  ppRtcd                        1
     d*
     d* provider search
     d F0032R          pr                  extpgm('F0032R')
     d  ppCall                        1
     d  ppFein                        9
     d  ppMod                         3
     d  ppRtcd                        1
     d  ppType                        1
     d*
     d* check service date against accident date
     d F8042R1         pr                  extpgm('F8042R1')
     d  ppTrid                       15
     d  ppAcdt70                      7  0
     d  outRtCd                       1
     d*
     d* upload bill transfer - program 2
     d F8042R          pr                  extpgm('F8042R')
     d  ppFPRVMbNm                   10
     d  ppU9Rrn                       9  0
     d  ppU1Rrn                       9  0
     d  ppPrChg                       1
     d*
     d* upload bill transfer - program 2 - for 136(CW)
     d F8042RA         pr                  extpgm('F8042RA')
     d  ppFPRVMbNm                   10
     d  ppU9Rrn                       9  0
     d  ppU1Rrn                       9  0
     d  ppPrChg                       1
     d*
     d* get company, fund details
     d FA007R          pr                  extpgm('FA007R')
     d  ppCo#                         3  0
     d  ppFnd#                        3  0
     d  ppCoNm                       40
     d  ppFnNm                       40
     d  ppSub#                        5  0
     d  ppFYr#                        3  0
     d*
     d* get provider name for fein
     d FA725R          pr                  extpgm('FA725R')
     d  ppFein#                       9  0
     d  ppMod#                        3  0
     d  ppPrNm                       40
     d  ppErr                         1
     d*
     d* get client id
     d getClientID     pr                  extpgm('RTVCLTID')
     d  outCltID                      3

     d* get MC sub/fnd from WC grp
     d getMCSubFnd     pr                  extpgm('FA700R')
     d  outRtCd                       1
     d  ppCo                          3
     d  ppWcf                         3
     d  ppSub                         5
     d  ppFnd                         3

     d* get System Flags
     d getSysFlags     pr                  ExtPgm('SA001R')
     d  wcmsInst                      1
     d  wcmcInst                      1
     d  wcmcInt                       1

     d* get import/export settings
     d rtvSettings     pr                  Extpgm('SA016R')
     d  @@Cchg                        1
     d  @@Cifs                        1
     d  @@Csts                        1
     d  @@Cdny                        1
     d  @@Ccvfl                       1
     d  @@Cgrp                        1
     d  @@Cemp                        1
     d  @@Eifs                        1
     d  @@Eagt                        1
     d  @@Eemp                        1
     d  @@Fifs                        1
     d  @@Ftyp                        1
     d  @@Udup                        1
     d  @@Udrc                        1
     d  @@Urel                        1
     d  @@Upcl                        1
     d  @@Updn                        1
     d  @@Udpn                        1
     d  @@ClDays                      5  0
     d  @@IncZro                      1
     d  @@Crcd                        1
     d  @@CrDth                       1
     d  @@RtCd                        1

     d* check for duplicate payment
     d WA052R          pr                  extpgm('WA052R')
     d  ppCo#                         3  0
     d  ppFnd#                        3  0
     d  ppFyr#                        3  0
     d  ppCase#                       7  0
     d  ppFrSvDt                      7  0
     d  ppToSvDt                      7  0
     d  ppAmt                         9  2
     d  ppRecd                        2
     d  ppFein#                       9  0
     d  ppRetp                        1
     d  outRtCd2                      2

     d* scan for pattern in string
     d QCLSCAN         pr                  extpgm('QCLSCAN')
     d  ppString                    999
     d  ppStrLen                      3  0
     d  ppStrPos                      3  0
     d  ppPattern                   999
     d  ppPatLen                      3  0
     d  ppTrnslt                      1
     d  ppTrim                        1
     d  ppWild                        1
     d  ppRtCd                        3  0

     d* clear subfiles
     d clrSubFiles     pr

     d* load filter options
     d loadFilter      pr

     d* load transfer history info
     d loadUplErr      pr

     d* main processing
     d main            pr

     d* process delete link
     d procDelete      pr

     d* process error status link
     d procErrSts      pr

     d* process Other option
     d procOther       pr

     d* process transfer link
     d procTrnsfr      pr

     d* program start
     d programStart    pr
     d processMe                       n

     d* validate filter screen
     d validFltr       pr

     d/copy sPrGenPop
     d/copy sPrUsrPrms
      /copy sPrMask

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  rrnfau1p             397    400b 0

     d dbfds2          ds
     d  rrnfau6p             397    400b 0

     d dspFilter       ds                  likerec(rcdERFltr) inz

     d U1prov          ds
     d  u1fn1#                 1      9  0
     d  u1md1                 10     12

     d                 ds
     d  value                  1     11
     d  sign                   1      1
     d  amount                 2     11  2

     d* array declaration
     d*

     d* constant declaration
     d*
     d LO              c                   'abcdefghijklmnopqrstuvwxyz'
     d UP              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d* standalone variables
     d*
     d wcmsInst        s              1    inz
     d wcmcInst        s              1    inz
     d wcmcInt         s              1    inz

     d @@Cchg          s              1    inz
     d @@Cifs          s              1    inz
     d @@Csts          s              1    inz
     d @@Cdny          s              1    inz
     d @@Ccvfl         s              1    inz
     d @@Cgrp          s              1    inz
     d @@Cemp          s              1    inz
     d @@Eifs          s              1    inz
     d @@Eagt          s              1    inz
     d @@Eemp          s              1    inz
     d @@Fifs          s              1    inz
     d @@Ftyp          s              1    inz
     d @@Udup          s              1    inz
     d @@Udrc          s              1    inz
     d @@Urel          s              1    inz
     d @@Upcl          s              1    inz
     d @@Updn          s              1    inz
     d @@Udpn          s              1    inz
     d @@ClDays        s              5  0 inz
     d @@IncZro        s              1    inz
     d @@Crcd          s              1    inz
     d @@CrDth         s              1    inz
     d @@RtCd          s              1    inz

     d ppCo            s              3    inz
     d ppCo#           s              3  0 inz
     d ppSub           s              5    inz
     d ppSub#          s              5  0 inz
     d ppFnd           s              3    inz
     d ppFnd#          s              3  0 inz
     d ppFyr#          s              3  0 inz
     d ppCase#         s              7  0 inz
     d ppSS#           s              9    inz
     d ppAcdt          s              8  0 inz
     d ppAcdt70        s              7  0 inz
     d ppCall          s              1    inz
     d ppRtcd          s              1    inz
     d ppFein          s              9    inz
     d ppMod           s              3    inz
     d ppMod#          s              3  0 inz
     d ppType          s              1    inz
     d ppFein#         s              9  0 inz
     d ppPrNm          s             40    inz
     d ppErr           s              1    inz
     d ppCoNm          s             40    inz
     d ppFnNm          s             40    inz
     d ppWCF           s              3    inz
     d ppKC            s              1    inz
     d ppKL            s              1    inz
     d ppSDOI          s              1    inz
     d ppRecd          s              2    inz
     d ppRetp          s              1    inz
     d ppString        s            999    inz
     d ppStrLen        s              3  0 inz
     d ppStrPos        s              3  0 inz
     d ppPattern       s            999    inz
     d ppPatLen        s              3  0 inz
     d ppTrnslt        s              1    inz
     d ppTrim          s              1    inz
     d ppWild          s              1    inz
     d ppRtCd30        s              3  0 inz
     d ppFPRVMbNm      s             10    inz
     d ppU9Rrn         s              9  0 inz
     d ppU1Rrn         s              9  0 inz
     d ppPrChg         s              1    inz

     d outCltId        s              3    inz
     d strclm          s             20    inz
     d strClmtLNm1     s             30    inz
     d strClmtLNm2     s             30    inz
     d usCo#           s              3  0 inz
     d usSub#          s              5  0 inz
     d usFnd#          s              3  0 inz
     d usFyr#          s              3  0 inz
     d kkName          s             10    inz
     d kkNtwk          s              5    inz
     d kkTrId          s             15    inz
     d kkDate          s               d   inz
     d kkCo            s              3    inz
     d kkCo#           s              3  0 inz
     d kkSub           s              5    inz
     d kkSub#          s              5  0 inz
     d kkFnd           s              3    inz
     d kkFnd#          s              3  0 inz
     d kkWcf           s              3    inz
     d kkWcf#          s              3  0 inz
     d kkYr            s              3    inz
     d kkYr#           s              3  0 inz
     d kkCase          s              7    inz
     d kkCase#         s              7  0 inz
     d kkSsn           s              9    inz
     d kkAcdt          s              7  0 inz
     d kkFein#         s              9  0 inz
     d kkMod#          s              3  0 inz
     d svFein#         s              9  0 inz
     d svMod#          s              3  0 inz
     d pos1            s              2  0 inz
     d pos2            s              2  0 inz
     d count           s              2  0 inz
     d xxChg           s              1    inz('N')
     d xxRlin          s              1    inz
     d xxderr          s              1    inz
     d xxAmt           s              9  2 inz
     d xxFDt70         s              7  0 inz
     d xxTDt70         s              7  0 inz
     d outRrn          s              9  0 inz
     d outRtCd         s              1    inz
     d outRtCd2        s              2    inz
     d RtCdTfr         s              1    inz
     d process         s               n   inz(*off)
     d isCallP         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isFirstLoad     s               n   inz(*off)
     d meetFltr        s               n   inz(*off)
     d isError         s               n   inz(*off)
     d indNtwk         s               n   inz(*off)
     d indMultCase     s               n   inz(*off)
     d ind91           s               n   inz(*off)
     d indError1       s               n   inz(*off)
     d isSettings      s               n   inz(*off)
     d isCompW         s               n   inz(*off)
     d isCareW         s               n   inz(*off)
     d isLCTA          s               n   inz(*off)
      *** Beg Add ***** 09/07/23 ******************************
     d decProv         s             13s 0
     d isInvProv       s               n
      *** End Add ***** 09/07/23 ******************************

     c* entry of program.
     c     *Entry        Plist
     c                   parm                    @@call            1            wdelp
     c                   parm                    @@rrn             9            wdelp
     c                   parm                    @@ntwk            5            wdelp
     c                   parm                    @@mode            1            mode
     c                   parm                    @@tabind          5            Tab index

      /free

       // main program processing

       programStart(process);

       If process;
          main();
       Endif;

       *inlr = *on;
       Return;

      /end-free

       //==================================================================
       // CheckFilter - Determine if the records meet the Filter criteria
       //==================================================================
     p checkFilter     b
     d checkFilter     pi
     d yyderr          s              1    inz
      /free

       // check Claimant Last Name
       If scFClmtLNm <> *blanks;
          strClmtLNm1 = %Xlate(LO:UP:U1clsn);     // string
          strClmtLNm2 = %Xlate(LO:UP:scFClmtLNm); // pattern
          ppString = strClmtLNm1;
          ppStrLen = 30;
          ppStrPos = 1;
          ppPattern = strClmtLNm2;
          ppPatLen = 30;
          ppTrnslt = '0';
          ppTrim = '1';
          ppWild = ' ';
          ppRtCd30 = *zeros;
          CallP QCLSCAN(ppString:ppStrLen:ppStrPos:ppPattern:ppPatLen:
                        ppTrnslt:ppTrim:ppWild:ppRtCd30);
          If ppRtCd30 = 0;
             meetFltr = *off;
          Endif;
       Endif;

       // check Bill number
       If scFBill# <> *blanks and
         U1trid <> scFBill#;
          meetFltr = *off;
       Endif;

       // check Social Security number
       If scFSSN# <> *blanks and
         U1ss# <> scFSSN#;
          meetFltr = *off;
       Endif;

       // check Error Type
       If scFErTyp <> *blanks and
         U1prfl <> scFErTyp;
          meetFltr = *off;
       Endif;

       // check Duplicate Type
       EvalR yyderr = U1dlre;
       If scFDupTyp <> *blanks and
         yyderr <> scFDupTyp;
          meetFltr = *off;
       Endif;

       // check Network
       If scFNtwk <> *blanks and
         U1ntwk <> scFNtwk;
          meetFltr = *off;
       Endif;

       // check From Upload date
       Monitor;
       kkDate = %Date(U1uldt:*cymd);
       If scFFrUpDt <> *loval and
         kkDate < scFFrUpDt;
          meetFltr = *off;
       Endif;
       On-error;
         kkDate = *loval;
       Endmon;

       // check To Upload date
       Monitor;
       kkDate = %Date(U1uldt:*cymd);
       If scFToUpDt <> *loval and
         kkDate > scFToUpDt;
          meetFltr = *off;
       Endif;
       On-error;
         kkDate = *loval;
       Endmon;

       // check Error Status
       If scFErSts <> *blanks and
         U1ests <> scFErSts;
          meetFltr = *off;
       Endif;

      /end-free
     p checkFilter     e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p clrSubFiles     b
     d clrSubFiles     pi
      /Free

       rrnUplErr = 0;
       clrERSub = *On;
       dspERSub = *Off;
       dspERCtl = *Off;
       Write rcdUplErr;

      /End-Free
     p ClrSubfiles     e

       //===================================================================
       // LoadFilter - initialize, load Filter screen
       //===================================================================
     p loadFilter      b
     d loadFilter      pi

      /free

       // if first time screen load,
       If isFirstLoad = *on;
          isFirstLoad = *off;
       EndIf;

       // if network is passed in as a paremeter, load it for filtering
       If indNtwk = *on;
          dspFilter.scFNtwk = kkNtwk;
          scFNtwk = kkNtwk;
       EndIf;

       // display clear filter button
       If dspFilter.scFClmtLNm = *blanks and
          dspFilter.scFBill# = *blanks and
          dspFilter.scFSSN# = *blanks and
          dspFilter.scFErTyp = *blanks and
          dspFilter.scFDupTyp = *blanks and
          dspFilter.scFNtwk = *blanks and
          dspFilter.scFFrUpDt = *loval and
          dspFilter.scFToUpDt = *loval and
          dspFilter.scFErSts = *blanks;
          dspClrFltr = *off;
       Else;
          dspClrFltr = *on;
       Endif;

      /end-free
     p loadFilter      e

       //===================================================================
       // Load Uploaded Bill Transfer History Information
       //===================================================================
     P loadUplErr      b
     d loadUplErr      pi
      /free

       // set subfile indicators
       clrERSub = *Off;
       dspERSub = *On;
       dspERCtl = *On;
       scERMsg = *blanks;
       vsbERMsg = *Off;

       // read file and load the subfile

       Setll *start fau1l3;
       Read pu102;
       Dow not %Eof(fau1l3);

          // verify if the record read meets the filter criteria
          meetFltr = *on;
          checkFilter();
          If meetFltr = *on;

             clear sflUplErr;

             // set required indicators etc.
             vsbERLink = *on;
             If dspmode = *on;
                vsbERLink = *off;
             EndIf;

             Monitor;
              scERUplDt = %Date(U1uldt:*cymd);
             On-error;
              scERUplDt = *loval;
             EndMon;
             scERBill#  = U1trid;
             scERClmtNm = %Trim(U1clsn) + ' ' + %Trim(U1cfsn);
             scERErr    = U1prfl;
             scERstat   = U1ests;

             scERRRN# = rrnfau1p;

             rrnUplErr = rrnUplErr + 1;
             If rrnUplErr = 1;
                scERRowSl = 'A';
             Else;
                clear scERRowSl;
             Endif;

             If rrnUplErr >= 9999;
               scERMsg = 'Too many records returned; first 9999 are shown. ' +
                         'Please use filter to select the records';
               vsbERMsg = *On;
               rrnUplErr = 1;
               Leave;
             EndIf;

             Write sflUplErr;
          Endif;

          Read pu102;
       Enddo;

       rrnUplErr = 1;

      /end-free
     p loadUplErr      e

      //===================================================================
      // main - Main processing
      //===================================================================
     p main            b
     d main            pi

      /free

       // process until exit button is pressed.
       DoU ExitNow = *On;

          // load filter options
          loadFilter();

          // clear Sub-files
          clrSubfiles();

          // load transfer history information
          loadUplErr();

          // process transfer history screen
          Exsr exfmtscreens;
          isError = *off;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // filter processing.
          When btnFilter = *on;
             Dou btnFltrCan = *on or btnFltrApp = *on;

                // mask SSN before display
                 scFSSN# = MaskData('SSN':'SCFSSN#':scFSSN#);

                ExFmt rcdERFltr;

                // Unmask SSN
                 scFSSN# = UnMaskData('SSN':'SCFSSN#':scFSSN#);

                If btnFltrApp = *on;
                   dspFilter.scFClmtLNm = scFClmtLNm;
                   dspFilter.scFBill#   = scFBill#;
                   dspFilter.scFSSN#    = scFSSN#;
                   dspFilter.scFErTyp   = scFErTyp;
                   dspFilter.scFDupTyp  = scFDupTyp;
                   dspFilter.scFNtwk    = scFNtwk;
                   dspFilter.scFFrUpDt  = scFFrUpDt;
                   dspFilter.scFToUpDt  = scFToUpDt;
                   dspFilter.scFErSts   = scFErSts;
                Endif;

                If btnFltrCan = *on;
                   scFClmtLNm =  dspFilter.scFClmtLNm;
                   scFBill#   =  dspFilter.scFBill#;
                   scFSSN#    =  dspFilter.scFSSN#;
                   scFErTyp   =  dspFilter.scFErTyp;
                   scFDupTyp  =  dspFilter.scFDupTyp;
                   scFNtwk    =  dspFilter.scFNtwk;
                   scFFrUpDt  =  dspFilter.scFFrUpDt;
                   scFToUpDt  =  dspFilter.scFToUpDt;
                   scFErSts   =  dspFilter.scFErSts;
                Endif;
             Enddo;

          // filter clear processing
          When btnClrFltr = *on;
             Clear dspFilter;
             Clear rcdERFltr;
             dspClrFltr = *off;

          When btnRefresh = *on;
             btnRefresh =*Off;

          // cancel button or F12 pressed
          When scERCancel = *on;
             scERCancel = *off;
             ExitNow = *on;

          // other options processing
          Other;
             procOther();

          Endsl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr exfmtScreens;

       Select;

       When TabIndex = 0;
          pnlHeight = 1050;
          aPnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdTabPan;
          Exfmt rcdUplErr;
          Read rcdTabPan;
          Read RcdSysTbl;

       Other;
          // default at tab 0
          tabindex = 0;
       EndSl;

       Endsr;

      /end-free
     p main            e

       //===================================================================
       // ProcDelete - Delete link processing
       //===================================================================
     p procDelete      b
     d procDelete      pi

     d isDeleted       s               n   inz(*off)
     d msDelete        s            100    inz
      /free

       // verify if this can be deleted

       If scErErr <> 'E' and scErErr <> 'R';
          msDelete = 'This cannot be deleted';
          popOneBtn(msDelete:'Upload Bills with Errors Info':'OK');
          Return;
       EndIf;

       // do delete processing

       // show window for deletion
       Clear rcdERInfo;
       vsbILbl01 = *off;
       vsbIMsg01 = *off;
       dsbIMsg01 = *on;
       vsbILbl02 = *off;
       vsbIMsg02 = *off;
       dsbIMsg02 = *on;
       vsbIEnter = *off;
       dsbIEnter = *on;
       vsbICancel = *off;
       dsbICancel = *on;

       scInfHdg = 'Upload Bill Deletion';

       scILbl01 = 'Upload Bill ID';
       vsbILbl01 = *on;
       scIMsg01 = scERBill#;
       vsbIMsg01 = *on;

       scILbl02 = 'Bill Deletion Reason';
       vsbILbl02 = *on;
       scIMsg02 = *blanks;
       vsbIMsg02 = *on;
       dsbIMsg02 = *off;

       scIValBtn1 = 'Enter';
       vsbIEnter = *on;
       dsbIEnter = *off;
       vsbICancel = *on;
       dsbICancel = *off;

       isDeleted = *off;
       DoU scIEnter = *on or scICancel = *on;
          Exfmt rcdERInfo;

          Select;
          When scICancel = *on;
             scICancel = *off;
             Leave;

          When isDeleted = *on;
             isDeleted = *off;
             Leave;

          When scIEnter = *on;
             scIEnter = *off;

             If isDeleted = *off;
                erIMsg02 = *off;
                If scIMsg02 = *blanks;
                   erIMsg02 = *on;
                   msIMsg02 = 'Value cannot be equal to blanks';
                   Iter;
                EndIf;

                //soft delete fau1p records
                Chain scERRrn# pu10p;
                If %Found(fau1p);

                   U1dlus = q1user;
                   U1dlpg = q1pgm;
                   Monitor;
                   U1dltm = %Dec(%Char(%Time():*iso0):6:0);
                   On-error;
                      U1dltm = 0;
                   Endmon;
                   Monitor;
                   U1dldt = %Dec(%Char(%Date():*cymd0):7:0);
                   On-error;
                      U1dldt = 0;
                   Endmon;
                   Monitor;
                   U1prdt = %Dec(%Char(%Date():*cymd0):7:0);
                   On-error;
                      U1prdt = 0;
                   Endmon;
                   U1dlre = scIMsg02;
                   U1prfl = 'D';
                   U1co#  = *zeros;
                   U1sub# = *zeros;
                   U1fnd  = *zeros;
                   U1fyr  = *zeros;
                   U1case = *zeros;
                   U1eob# = *zeros;
                   U1pss# = *blanks;
                   U1pfei = *zeros;
                   U1mod  = *zeros;
                   U1acdt = *zeros;

                   update pu10p;
                EndIf;

                //soft delete fau2p records
                Setll kkTrId pu200;
                ReadE kkTrId pu200;
                Dow not %Eof(fau2l1);
                   U2prfl = 'D';
                   Update pu200 %Fields(U2prfl);
                   ReadE kkTrId pu200;
                EndDo;
                isDeleted = *on;
             EndIf;
          EndSl;
          If isDeleted = *on;
             //show deletion confirmation message
             scIMsg02 = 'This is deleted successfully';
             dsbIMsg02 = *on;
             vsbILbl02 = *off;
          EndIf;
       EndDo;

      /end-free
     p procDelete      e

       //===================================================================
       // ProcErrSts - Error Status link processing
       //===================================================================
     p procErrSts      b
     d procErrSts      pi

     d modeES          s              1    inz
     d countES         s              3  0 inz
     d isValidES       s               n   inz(*off)
     d sv_scESAdTrId   s             15    inz
     d sv_scESAdESts   s              1    inz
     d sv_scESAdExpl   s             60    inz
      /free

        scESCancel = *off;
        DoU scESCancel = *on;

           // clear, load and display data on screen
           Exsr ClrESSubFl;
           Exsr LoadESData;
           Exsr ExfmtESScr;

           Select;

           When scESCancel = *on;
              scESCancel = *off;
              Leave;

           When scESAdd = *on;
              scESAdd = *off;
              Clear rcdESAdd;
              modeES = 'A';
              btnESAdSav = *off;
              btnESAdCan = *off;
              Exsr procESAddEdit;

           Other;
              Exsr procESOther;

           EndSl;

        EndDo;

       //===================================================================
       // AddEdtESDtl - Add / Edit Error Status details
       //===================================================================
       BegSr AddEdtESDtl;

        Chain scESRrn# pu60p;
        U6ests = scESAdESts;
        U6reas = scESAdExpl;
        U6trid = scESAdTrId;

        U6user = q1user;
        U6pgm  = q1pgm;
        Monitor;
        U6dt   = %Dec(%Char(%Date():*cymd0):7:0);
        On-Error;
           U6dt   = 0;
        Endmon;
        Monitor;
        U6time = %Dec(%Char(%Time():*iso0):6:0);
        On-Error;
           U6time = 0;
        Endmon;

        If modeES = 'A';

           // write to FAU6P
           Monitor;
           U6stdt = %Dec(%Char(%Date():*cymd0):7:0);
           On-Error;
              U6stdt = 0;
           Endmon;
           Monitor;
           U6sttm = %Dec(%Char(%Time():*iso0):6:0);
           On-Error;
              U6sttm = 0;
           Endmon;
           U6stat = 'A';
           U6ipgm = q1pgm;
           Monitor;
           U6idt  = %Dec(%Char(%Date():*cymd0):7:0);
           On-Error;
              U6idt  = 0;
           Endmon;
           Monitor;
           U6itim = %Dec(%Char(%Time():*iso0):6:0);
           On-Error;
              U6itim = 0;
           Endmon;
           Write pu60p;

           // update FAU1P
           Chain scESAdTrId pu102;
           If %Found(fau1l3);
              U1dlus = q1user;
              U1dlpg = q1pgm;
              U1dltm = U6sttm;
              U1ests = scESAdESts;
              Update pu102;
           EndIf;

        Else;

           // update FAU6P
           Monitor;
           U6stdt = %Dec(%Char(%Date():*cymd0):7:0);
           On-Error;
              U6stdt = 0;
           Endmon;
           Monitor;
           U6sttm = %Dec(%Char(%Time():*iso0):6:0);
           On-Error;
              U6sttm = 0;
           Endmon;
           Update pu60p;

           // update FAU1P
           Chain scESAdTrId pu102;
           If %Found(fau1l3);
              U1dlus = q1user;
              U1dlpg = q1pgm;
              U1dltm = U6sttm;
              U1ests = scESAdESts;
              Update pu102;
           EndIf;

        EndIf;
        Unlock fau6p;
       Endsr;

       //===================================================================
       // ClrESSubFl - Clear subfile for Error Status screen
       //===================================================================
       BegSr ClrESSubFl;

        rrnErrSts = 0;
        clrESsub = *on;
        dspESsub = *off;
        dspESCtl = *off;
        Write rcdERSts;

       Endsr;

       //===================================================================
       // ExfmtESScr - Exfmt Error Status screen
       //===================================================================
       BegSr ExfmtESScr;

        Exfmt rcdERSts;

       Endsr;

       //===================================================================
       // LoadESData - Load data for Error Status subfile screen
       //===================================================================
       BegSr LoadESData;

        // set subfile indicators
        clrESSub = *off;
        dspESSub = *on;
        dspESCtl = *on;
        countES = *zeros;

        // read file and load the subfile

        // get TrId of the record selected
        Chain(n) scERRrn# pu10p;

        Setll U1trid fau6l;
        Reade U1trid pu600;
        DoW not %Eof(fau6l);

           clear sflErrSts;

           // set required indicators etc.
           vsbESLink = *on;
           If dspmode = *on;
              vsbESLink = *off;
           EndIf;

           Monitor;
            scESDt = %Date(U6stdt:*cymd);
            scESTm = U6sttm;
           On-error;
            scESDt = *loval;
            scESTm = *zeros;
           EndMon;
           scESstat = U6ests;
           scESrsn = U6reas;
           scEStrid = U6trid;

           scESrrn# = rrnfau6p;

           rrnErrSts = rrnErrSts + 1;
           If rrnErrSts = 1;
              scESRowSl = 'A';
           Else;
              Clear scESRowSl;
           Endif;

           Write sflErrSts;
           countES += 1;

           Reade U1trid pu600;
        Enddo;

        rrnUplErr = 1;

       Endsr;

       //===================================================================
       // ProcESAddEdit - Process Add / Edit for Error Status screen
       //===================================================================
       BegSr ProcESAddEdit;

        isValidES = *off;

       // processing for Edit mode
       If modeES = 'C';

        // upload bill ID should be disabled in edit mode
          dsbESAdTId = *on;

        // error status should not be disabled in edit mode
          dsbESAdES = *off;

        // error reason / explaination should be enabled in edit mode
          dsbESAdExp = *off;

          Chain(n) scESRrn# pu60p;
          If %Found (fau6p);
             scESAdTrId = U6trid;
             scESAdESts = U6ests;
             scESAdExpl = U6reas;

             // save loaded values for later check
             sv_scESAdTrId = scESAdTrId;
             sv_scESAdESts = scESAdESts;
             sv_scESAdExpl = scESAdExpl;
          EndIf;

       Endif;

       // processing for Add mode
       If modeES = 'A';

        // upload bill ID should be disabled in add mode
          dsbESAdTId = *on;

        // error status should not be disabled in add mode
          dsbESAdES = *off;

        // error reason / explaination should be enabled in add mode
          dsbESAdExp = *off;

          Chain(n) scESRrn# pu60p;
          If %Found (fau6p);
             scESAdTrId = U6trid;
          Else;
             // when Add button on Grid is pressed, load Trid from ER Sfile
             scESAdTrId = scERBill#;
          EndIf;

       Endif;

       DoU btnESAdCan = *on or (btnESAdSav = *on and isValidES = *on);
          Exfmt rcdESAdd;
          isValidES = *on;
          Select;

          When btnESAdSav = *on;
             Exsr ValidateES;
             If isValidES = *on;
                Exsr AddEdtESDtl;
             Endif;

          When btnESAdCan = *on;
             btnESAdCan = *off;
             Leave;

          EndSl;
       Enddo;
       unlock fau6p;

       Endsr;

       //===================================================================
       // ProcESDlt - Process Delete for Error Status screen
       //===================================================================
       BegSr ProcESDlt;

        // update FAU1P
        Chain scESTrId pu102;
        If %Found(fau1l3) and
          U1ests = scESStat;
           U1dlus = q1user;
           U1dlpg = q1pgm;
           U1dltm = U6sttm;
           U1ests = *blanks;
           Update pu102;
        EndIf;

        Chain scESRrn# pu60p;
        If %Found(fau6p);
           delete pu60p;
        EndIf;
        Unlock fau6p;

       Endsr;
       //===================================================================
       // ProcESOther - Process Other options for Error Status screen
       //===================================================================
       BegSr ProcESOther;

        ReadC sflErrSts;
        DoW not %Eof;

           Select;

           // processing for Edit link
           When scESEdt = *on;
              scESEdt = *off;
              Clear rcdESAdd;
              modeES = 'C';
              btnESAdSav = *off;
              btnESAdCan = *off;
              Exsr procESAddEdit;

           // processing for Delete link
           When scESDlt = *on;
              scESDlt = *off;
              Exsr procESDlt;

           EndSl;
           ReadC sflErrSts;
        Enddo;

       Endsr;

       //===================================================================
       // ValidateES - Validate Error Status Add/Edit screen
       //===================================================================
       BegSr ValidateES;

        isValidES = *on;
        erESAdESts = *off;
        erESAdExpl = *off;

        // error status should not be blank
        If scESAdESts = *blanks;
           msESAdESts = 'This is required';
           erESAdESts = *on;
           isValidES = *off;
           LeaveSr;
        EndIf;

        // error reason / explaination should not be blank
        If scESAdExpl = *blanks;
           msESAdExpl = 'This is required';
           erESAdExpl = *on;
           isValidES = *off;
           LeaveSr;
        EndIf;

       Endsr;

      /end-free
     p procErrSts      E

       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p procOther       b
     d procOther       pi
      /free

       ReadC sflUplErr;
       DoW not %Eof;

          Select;

          // processing for Delete link
          When scERDlt = *on;
             scERDlt = *off;
             procDelete();

          // processing for Transfer link
          When scERTfr = *on;
             scERTfr = *off;
             procTrnsfr();

          // processing for Error Status link
          When scERErSts = *on;
             scERErSts = *off;
             procErrSts();

          Endsl;
          ReadC sflUplErr;
       Enddo;

      /end-free
     p procOther       E

       //===================================================================
       // ProcTrnsfr - Transfer link processing
       //===================================================================
     p procTrnsfr      b
     d procTrnsfr      pi

     d msTrnsfr        s            100    inz
     d strTfrMsg       s            100    inz
     d strReconMs      s            100    inz
     d indReconMs      s               n   inz(*off)

      /free

       // verify if this can be transferred

       //*** Beg Del *** 10/05/23 ***************************************
       //***If scERErr = 'R';
          //***msTrnsfr = 'This has Recon error, this can not be transferred';
          //***popOneBtn(msTrnsfr:'Upload Bills with Errors Info':'OK');
          //***Return;
       //***EndIf;
       //*** End Del *** 10/05/23 ***************************************

       //*** Beg Chg *** 10/05/23 ******************************************
       //***If scERErr <> 'E' and isError = *off;
       If scERErr <> 'E' and scErErr <> 'R' and isError = *off;
       //*** End Chg *** 10/05/23 ******************************************
          msTrnsfr = 'This can not be transferred';
          popOneBtn(msTrnsfr:'Upload Bills with Errors Info':'OK');
          Return;
       EndIf;

       // clear and load transfer screen data
          Exsr LoadTfrData;

          DoU btnTfrCan = *on or btnTfrTfr = *on;

             // mask SSN before display
              scTfrSSN = MaskData('SSN':'scTfrSSN':scTfrSSN);

             Exfmt rcdERTfr;

             // Unmask SSN
              scTfrSSN = UnMaskData('SSN':'scTfrSSN':scTfrSSN);

             scTfrSubFo = *off;
             scTfrPFFo = *off;
             Select;

              When btnTfrTfr = *on;
                 btnTfrTfr = *off;
                 Exsr Errck1;
                 If IndError1 = *off;
                    Exsr UpdTrnsfr;
                    Leave;
                 EndIf;

              When btnTfrCan = *on;
                 btnTfrCan = *off;
                 Leave;

              When selClmSrch = *on;
                 selClmSrch = *off;
                 Exsr ClmSrch;

              When selPrvSrch = *on;
                 selPrvSrch = *off;
                 Exsr PrvSrch;

              Other;

             EndSl;
          EndDo;

       //===================================================================
       // ClmSrch - Search for a claim
       //===================================================================
       BegSr ClmSrch;

        ppCall = '2';
        ppSub# = *zeros;
        ppFnd# = *zeros;
        ppFyr# = *zeros;
        ppCase# = *zeros;
        ppSS# = *blanks;
        ppAcdt = *zeros;
        ppRtcd = *blanks;

        Callp F0269R (ppCall:ppSub#:ppFnd#:ppFyr#:ppCase#:ppSS#:ppAcdt:ppRtcd);

        scTfrSubFo = *on;
        If ppRtcd = 'A' or ppRtcd = 'B' or
          ppFyr# = *zeros or ppCase# = *zeros;
           // *in31 on and 90 on
           // can show a message here if required
        Else;
           Chain(n) (usCo#:ppSub#:ppFnd#:ppFyr#:ppCase#) pcm02;
           If %Found(fmcml1);
              scTfrClmnt = %Trim(CMlsn) + ', ' + %Trim(CMfsn);
              scTfrSub# = CMsub#;
              scTfrFnd = CMfnd;
              scTfrFyr = CMfyr;
              scTfrCase = CMcase;
              scTfrSSN = ppSS#;
              Monitor;
               scTfrAcdt = %Date(ppAcdt:*usa);
              On-Error;
               scTfrAcdt = *loval;
              EndMon;
              erTfrSub# = *off;
              erTfrFnd  = *off;
              erTfrFyr  = *off;
              erTfrCase = *off;
           EndIf;
        EndIf;

       Endsr;

       //===================================================================
       // ErrCK
       //===================================================================
       BegSr Errck;
        // ERRCK processing

        Exsr getclm;
        If U1prfl = 'E';
           // check WCMC claim master to see if SS#/ACDT exists
           Chain(n) (usCo#:scTfrSub#:scTfrFnd:scTfrFyr:scTfrCase) pcm02;
           If not %Found(fmcml1);
              count = 0;
              kkSsn = U1ss#;
              Monitor;
              kkAcdt = %Dec(%Char(%Date(U1doi:*iso):*cymd0):7:0);
              On-error;
                 kkAcdt = 0;
              Endmon;
              Setll (usCo#:kkSsn:kkAcdt) pcm01;
              Reade (usCo#:kkSsn:kkAcdt) pcm01;
              Dow not %Eof(fmcmlb);
                 count += 1;
                 Reade (usCo#:kkSsn:kkAcdt) pcm01;
              EndDo;
           Else;
              count = 1;
           EndIf;

           Select;
            When count = 0;
               strTfrMsg = 'Case is Invalid'; // *in21
               Exsr WrtTfrMsg;
            When count = 1;
               // get WC group number
               scTfrSub# = CMsub#;
               scTfrFnd  = CMfnd;
               scTfrFyr  = CMfyr;
               scTfrCase = CMcase;

               outRtCD = *blanks;
               ppCo    = %EditC(usCo#:'X');
               ppWCF   = *blanks;
               ppSub   = %EditC(CMsub#:'X');
               ppFnd   = %EditC(CMfnd:'X');
               Callp getMCSubFnd(outRtCd:ppCo:ppWCF:ppSub:ppFnd);

               If outRtCd = *blanks;
                  kkWcf  = ppWCF;
                  kkWcf# = %Int(kkWcf);
               Else;
                  kkWcf  = *blanks;
                  kkWcf# = *zeros;
               EndIf;

            When count > 1;
               If dspmode = *on;
                  strTfrMsg = 'Multiple cases'; // *in23
               Else;
                  strTfrMsg = 'Multiple cases, plase correct any other ' +
                              'errors and press Enter, then select ' +
                              'a case';         // *in23
               EndIf;
               Exsr WrtTfrMsg;
           EndSl;

           // FEIN/MOD does not exist
           kkFein# = U1fein;
           kkMod# = *zeros;
           If U1prov <> *blanks and
             U1prov <> '0000000000000';
             //*** Beg Add *** 09/07/23 ********************************
              If isInvProv = *off;
              //*** End Add *** 09/07/23 *******************************
                 kkFein# = U1fn1#;
                 kkMod# = %Int(U1md1);
              //*** Beg Add *** 09/07/23 ********************************
              Else;
                 kkFein# = *zeros;
                 kkMod# = *zeros;
              Endif;
              //*** End Add *** 09/07/23 ********************************
           EndIf;

           Chain(n) (kkFein#:kkMod#) ppd00;
           If not %Found(fmpdl);
              strTfrMsg = 'Provider is invalid';  //in22
              Exsr WrtTfrMsg;
              xxChg = 'Y';
           Else;
              Chain (usCo#:U1ntwk) pnw00;
              If %Found(nmnwl) and NWfl03 = 'Y';          // is Inactive prov. flag on
                 // Skip the check for inactive provider
              Elseif PDstat <> 'A' and PDstat <> *blanks; // is prov. active
                 strTfrMsg = 'Provider is invalid';  //in22
                 Exsr WrtTfrMsg;
                 xxChg = 'Y';
              Endif;
           EndIf;

        Else;
           // what kind of recon error
           If U1rcnf = 'Y';
              If isLCTA = *on;
                 strTfrMsg = 'EOB being reconsidered is :' + U1oTrid; //erBill1 = *on
                 Exsr WrtTfrMsg;
              EndIf;
              // populate error message. this will be written later if other
              // condition is met (*in17 is met). Set ind off here
              strReconMs = 'Recon error, cannot find original bill'; //*in24
              indReconMs = *off;
           Else;
              // populate error message. this will be written later if other
              // condition is met (*in17 is met). Set ind off here
              strReconMs = 'Recon error, this bill is marked as an original ' +
                          'bill, but this Bill ID has already been ' +
                          'uploaded'; //*in25
              indReconMs = *off;
           EndIf;
        EndIf;

       Endsr;

       //===================================================================
       // ErrCK1
       //===================================================================
       BegSr Errck1;
        // ERRCK1 processing
        indError1 = *off;
        indMultCase = *off;
        erTfrSub# = *off;
        erTfrFnd  = *off;
        erTfrFyr  = *off;
        erTfrCase = *off;
        erTfrPFein = *off;

        If indError1 = *off;
           kkSub# = scTfrSub#;
           kkFnd# = scTfrFnd;
           kkYr# = scTfrFyr;
           kkCase# = scTfrCase;
           Chain(n) (usCo#:scTfrSub#:scTfrFnd:scTfrFyr:scTfrCase) pcm02;
           If not %Found(fmcml1);
              count = *zeros;
              kkSsn = scTfrSSN;
              //*** Beg Chg *** 06/25/25 *******************************
              //*** kkAcdt = %Dec(%Char(scTfrAcdt:*cymd0):7:0);
              Monitor;
                kkAcdt = %Dec(%Char(scTfrAcdt:*cymd0):7:0);
              On-error;
                kkAcdt = *loval;
              Endmon;
              //*** End Chg *** 06/25/25 *******************************
              Setll (usCo#:kkSsn:kkAcdt) pcm01;
              Reade (usCo#:kkSsn:kkAcdt) pcm01;
              Dow not %Eof(fmcmlb);
                 count += 1;
                 Reade (usCo#:kkSsn:kkAcdt) pcm01;
              EndDo;
           Else;
              count = 1;
           EndIf;

           Select;

            // ss#/acdt doesn't exist
            When count = 0;
               msTfrSub# = 'Claim number not found';
               msTfrFnd  = 'Claim number not found';
               msTfrFyr  = 'Claim number not found';
               msTfrCase = 'Claim number not found';
               erTfrSub# = *on;
               erTfrFnd  = *on;
               erTfrFyr  = *on;
               erTfrCase = *on;
               indError1 = *on;

            // ss#/acdt occurs more than once
            When count > 1;
               indMultCase = *on; // *in75
           EndSl;
        EndIf;

        // fein/mod does not exist
        If indError1 = *off;
           kkFein# = scTfrPFein;
           kkMod#  = scTfrPMod;
           Chain(n) (kkFein#:kkMod#) ppd00;
           If not %Found(fmpdl);
              msTfrPFein = 'Invalid Provider Number';
              erTfrPFein = *on;
              indError1 = *on;
           Else;                                         // provider found
              Chain (usCo#:U1ntwk) pnw00;
              If %Found(nmnwl) and NWfl03 = 'Y';         // check if inactive prov. flag is on
                 // Skip the check for inactive provider.
              Elseif PDstat <> 'A' and PDstat <> *blanks; // check if prov. is active
                 msTfrPFein = 'Invalid/Inactive Provider Number';
                 erTfrPFein = *on;
                 indError1 = *on;
              Endif;
           EndIf;
        EndIf;

        // check to see if multiple cases
        If indError1 = *off and
          indMultCase = *on;

           ppKC = *blanks;
           ppKL = *blanks;
           ppCall = '2';
           outRrn = *zeros;
           ppSS#  = scTfrSSN;
           //*** Beg Chg *** 06/25/25 *******************************
           //*** ppAcdt = %Dec(%Char(scTfrAcdt:*usa0):8:0);
           Monitor;
             ppAcdt = %Dec(%Char(scTfrAcdt:*usa0):8:0);
           On-error;
             ppAcdt = *loval;
           Endmon;
           //*** End Chg *** 06/25/25 *******************************
           ppSDOI = 'Y';
           Callp F0267R (ppKC:ppKL:ppCall:outRrn:ppSS#:ppAcdt:ppSDOI);
           If ppKC = *off and ppKL = *off and
             outRrn <> *zeros;
              Chain(n) outRrn pcm0p;
           Else;
              indError1 = *on;
           EndIf;

           // check to see if duplicate
           EvalR xxderr = U1dlre;
           ind91 = *off;
           If indError1 = *off and
             xxderr= *blanks;

               value = U1paid;
               If sign = '-';
                  xxAmt = Amount * -1;
               Else;
                  xxAmt = Amount;
               EndIf;

               If xxAmt <> *zeros;
                  Monitor;
                  xxFDt70 = %Dec(%Char(%Date(U1frdt:*iso):*cymd0):7:0);
                  On-error;
                     xxFdt70 = 0;
                  Endmon;
                  Monitor;
                  xxTDt70 = %Dec(%Char(%Date(U1todt:*iso):*cymd0):7:0);
                  On-error;
                     xxTdt70 = 0;
                  Endmon;

                 outRtCd = *blanks;
                 ppCo    = %EditC(CMco#:'X');
                 ppWCF   = *blanks;
                 ppSub   = %Editc(CMsub#:'X');
                 ppFnd   = %Editc(CMfnd:'X');
                 Callp getMCSubFnd(outRtCd:ppCo:ppWCF:ppSub:ppFnd);

                 If outRtCd = *blanks;
                    kkWcf  = ppWCF;
                    kkWcf# = %Int(kkWcf);
                 Else;
                    kkWcf  = *blanks;
                    kkWcf# = *zeros;
                 EndIf;
                  // Check for duplicate request/payment if WCMS is installed
               EndIf;

               If wcmsInst = 'Y';
                  outRtCd2 = *blanks;
                  ppRecd = 'ME';
                  ppRetp = 'M';
                  Callp WA052R (CMco#:kkWcf#:CMfyr:CMcase:xxFDt70:xxTDt70:
                                xxAmt:ppRecd:PDfein:ppRetp:outRtCd2);
                  If outRtCd2 <> *blanks;

                     // show window for transfer confirmation
                     Clear rcdERInfo;
                     vsbILbl01 = *off;
                     vsbIMsg01 = *off;
                     dsbIMsg01 = *on;
                     vsbILbl02 = *off;
                     vsbIMsg02 = *off;
                     dsbIMsg02 = *on;
                     vsbIEnter = *off;
                     dsbIEnter = *on;
                     vsbICancel = *off;
                     dsbICancel = *on;

                     scInfHdg = 'Bill Transfer';

                     scIMsg01 = 'This bill may issue duplicate payments. ' +
                                'If you wish to continue, hit enter, else ' +
                                'press Cancel';
                     vsbIMsg01 = *on;

                     scIValBtn1 = 'Enter';
                     vsbIEnter = *on;
                     dsbIEnter = *off;
                     vsbICancel = *on;
                     dsbICancel = *off;

                     DoU scIEnter = *on or scICancel = *on;
                        Exfmt rcdERInfo;

                        Select;
                        When scICancel = *on;
                           scICancel = *off;
                           rtcdTfr = 'E';
                           Leave;

                        When scIEnter = *on;
                           scIEnter = *off;
                           If isSettings <> '1';
                              If outRtCd2 = 'DR' or
                                outRtCd2 = 'DP';
                                 xxderr = 'D';
                                 rtcdTfr = 'D';
                              Else;
                                 xxderr = 'P';
                                 rtcdTfr = 'P';
                              EndIf;
                           EndIf;
                           // Check for duplicates using the new settings file
                           // If @@Udup = blank then dont check for duplicates at all

                           If outRtCd2 <> ' ' and isSettings = '1';
                              // complete duplicates
                              If @@Udup = 'C' or @@Udup = 'A';
                                 If outRtCd2 = 'DR' or outRtCd2 = 'DP';
                                    xxDerr = 'D';
                                    rtCdTfr = 'D';
                                 EndIf;
                              EndIf;
                              // partial duplicates
                              If @@Udup = 'P';
                                 If outRtCd2 = 'PR' or outRtCd2 = 'PP';
                                    xxDerr = 'P';
                                    rtCdTfr = 'P';
                                 EndIf;
                              EndIf;
                           Else;
                              Ind91 = *on;
                              rtCdTfr = 'E';
                           EndIf;
                        EndSl;
                     EndDo;
                  EndIf;
               EndIf;
           EndIf;

        EndIf;
        If indError1 = *off and
          Ind91 = *off and
          isCompW = *on;
           // check for line items less than the accident date
           outRtcd = *blanks;
           ppAcdt70 = CMacdt;
           Callp F8042R1 (U1trid:ppAcdt70:outRtcd);
           If outRtcd = 'F';
              // show window for service date message
              Clear rcdERInfo;
              vsbILbl01 = *off;
              vsbIMsg01 = *off;
              dsbIMsg01 = *on;
              vsbILbl02 = *off;
              vsbIMsg02 = *off;
              dsbIMsg02 = *on;
              vsbIEnter = *off;
              dsbIEnter = *on;
              vsbICancel = *off;
              dsbICancel = *on;

              scInfHdg = 'Bill Transfer';

              scIMsg01 = 'This bill has line items with service dates that +
                          are less than the accident date of the claim.';
              vsbIMsg01 = *on;

              scIMsg02 = 'If you wish to continue hit enter, else press +
                          cancel';
              vsbIMsg02 = *on;

              scIValBtn1 = 'Enter';
              vsbIEnter = *on;
              dsbIEnter = *off;
              vsbICancel = *on;
              dsbICancel = *off;

              DoU scIEnter = *on or scICancel = *on;
                 Exfmt rcdERInfo;

                 Select;
                 When scICancel = *on;
                    scICancel = *off;
                    Ind91 = *on;
                    xxderr = 'S';
                    rtCdTfr = 'S';
                    Leave;

                 When scIEnter = *on;
                    scIEnter = *off;

                 EndSl;
              EndDo;
           EndIf;
        EndIf;

        // check if the provider changed
        If indError1 = *off and
          xxChg <> 'Y';
           If scTfrPFein <> svFein# or
             scTfrPMod <> svMod#;
              xxChg = 'Y';
           EndIf;
        EndIf;

       Endsr;

       //===================================================================
       // Get Claim Number
       //===================================================================
       BegSr Getclm;

       strClm = U1clm#;
       pos1 = %Scan('-':strClm);
       pos2 = %Scan('-':strClm:pos1+1);
       Monitor;
        If pos1 > 0;
           // assuming claim is the format xxx-yyy-zzzzzzz
           kkWcf = %Subst(strClm:1:pos1-1);
           kkYr = %Subst(strClm:pos1+1:pos2-pos1+1);
           kkCase = %Subst(strClm:pos2+1:%Len(%Trim(strclm))-pos2+1);
        Else;
           // assuming claim is the format xxxyyyzzzzzzz
           kkWcf = %Subst(strClm:1:3);
           kkYr = %Subst(strClm:4:3);
           kkCase = %Subst(strClm:7:7);
        EndIf;
        kkWcf#  = %Int(kkWcf);
        kkYr#   = %Int(kkYr);
        kkCase# = %Int(kkCase);
       On-error;
       EndMon;
       kkCo#   = usCo#;
       kkCo    = %EditC(usCo#:'X');

       outRtCD = *blanks;
       ppCo    = %EditC(usCo#:'X');
       ppWCF   = kkWcf;
       ppSub   = *blanks;
       ppFnd   = *blanks;
       Callp getMCSubFnd(outRtCd:ppCo:ppWCF:ppSub:ppFnd);

       If outRtCd = *blanks and ppSub <> *blanks and ppFnd <> *blanks;
          kkSub  = ppSub;
          kkFnd  = ppFnd;
          kkSub# = %Int(kkSub);
          kkFnd# = %Int(kkFnd);
       Else;
          kkSub  = *blanks;
          kkFnd  = *blanks;
          kkSub# = *zeros;
          kkFnd# = *zeros;
       EndIf;
       scTfrSub#  = kkSub#;
       scTfrFnd   = kkFnd#;
       scTfrFyr   = kkYr#;
       scTfrCase  = kkCase#;

       Endsr;

       //===================================================================
       // LoadTfrData - Load data for Transfer screen
       //===================================================================
       BegSr LoadTfrData;

        Clear rcdERTfr;
        Chain(n) scERRrn# pu10p;
        scTfrTrId  = U1trid;
        scTfrClmnt = %Trim(U1clsn) + ', ' + %Trim(U1cfsn);
        scTfrSSN   = U1ss#;
        Monitor;
        scTfrAcDt  = %Date(U1DOI:*iso);
        On-error;
           scTfrAcDt = *loval;
        Endmon;
        scTfrPFein = U1fein;
        scTfrPMod  = *zeros;
        If U1prov <> *blanks and
          U1prov <> '0000000000000';
          //*** Beg Add *** 09/07/23 ***********************************
           // Edit the provider ID and give an error if its not numeric
           isInvProv = *off;
           Monitor;
              decProv = %Dec(U1prov:13:0);
           On-Error;
              scTfrPFein = *zeros;
              scTfrPMod  = *zeros;
              isInvProv = *on;
           EndMon;
           //*** End Add *** 09/07/23 ***********************************
           //*** Beg Add *** 09/07/23 ***********************************
           If isInvProv = *off;
           //*** End Add *** 09/07/23 ***********************************
              scTfrPFein = U1fn1#;
              scTfrPMod  = %Int(U1md1);
           //*** Beg Add *** 09/07/23 ***********************************
           Endif;
           //*** End Add *** 09/07/23 ***********************************
        EndIf;

        If U1prov = *blanks or
          //*** Beg Chg *** 09/07/23 ***********************************
          //***U1prov = '0000000000000';
          U1prov = '0000000000000' or isInvProv = *on;
          //*** End Chg *** 09/07/23 ***********************************
           If U1pfsn = *blanks;
              scTfrPrvNm = U1pnam;
           Else;
              scTfrPrvNm = %Trim(U1pfsn) + ' ' + %Trim(U1pnam);
           EndIf;
           scTfrPrAd1 = U1adr1;
           scTfrPrCty = U1cty;
           scTfrPrSt  = U1st;
           scTfrPrZip = %Int(U1zip);
        EndIf;

        If U1prov <> *blanks and
          //*** Beg Chg *** 09/07/23 ***********************************
          //***U1prov <> '0000000000000';
          U1prov <> '0000000000000' and isInvProv = *off;
          //*** End Chg *** 09/07/23 ***********************************
           kkFein# = U1fn1#;
           kkMod#  = %Int(U1md1);
           Chain(n) (kkFein#:kkMod#) ppd00;
           If %Found(fmpdl);
              Chain(n) (usCo#:U1ntwk) pnw00;
              If (%Found(nmnwl) and NWfl03 = 'Y') or PDstat = 'A';
                 If PDfsn = *blanks;
                    scTfrPrvNm = PDname;
                 Else;
                    scTfrPrvNm = %Trim(PDfsn) + ' ' + %Trim(PDname);
                 EndIf;
                 scTfrPrAd1 = PDadr1;
                 scTfrPrCty = PDcty;
                 scTfrPrSt  = PDst;
                 scTfrPrZip = PDzip;
              EndIf;
           EndIf;
        EndIf;
        svFein# = scTfrPFein;
        svMod#  = scTfrPMod;
        //*** Beg Add *** 09/07/23 **************************************
        // If the provider name is still blank use name/address from the
        // bill import file
        If scTfrPrvNm = *blanks;
           If U1pfsn = *blanks;
              scTfrPrvNm = U1pnam;
           Else;
              scTfrPrvNm = %Trim(U1pfsn) + ' ' + %Trim(U1pnam);
           EndIf;
        Endif;
        If scTfrPrAd1 = *blanks;
           scTfrPrAd1 = U1adr1;
           scTfrPrCty = U1cty;
           scTfrPrSt  = U1st;
           scTfrPrZip = %Int(U1zip);
        Endif;
        //*** End Add *** 09/07/23 **************************************

        Exsr Errck;

        EvalR xxderr = U1dlre;
        If xxderr <> *blanks;
           If xxderr = 'D';
              strTfrMsg = 'There are complete duplicate payments'; // *in26 and *in17
              Exsr WrtTfrMsg;
           Else;
              strTfrMsg = 'There are partial duplicate payments'; // *in27 and *in17
              Exsr WrtTfrMsg;
           EndIf;
           // Set on indicator for Recon message but dont write it yet
           indReconMs = *on; //*in17 gets on here
        EndIf;
        If U1prfl = 'R';
           // Set on indicator for Recon message but dont write it yet
           indReconMs = *on; //*in17 gets on here
        EndIf;
        If xxderr = 'R';
           strTfrMsg = 'Record only claim'; // *in29
           Exsr WrtTfrMsg;
        EndIf;
        // check to see if recon error and detail lines not present
        // or missing recon line number
        If U1prfl = 'R' and U1rcnf = 'Y';
           Chain(n) U1trid pu200;
           If %Found(fau2l1);
              xxRlin = 'N';
              DoW not %Eof(fau2l1);
                 If U2rlin > 0;
                    xxRlin = 'Y';
                 Endif;
                 Reade(n) U1trid pu200;
              Enddo;
              If xxRlin = 'N';
                 strReconMs = 'Reconsideration error: Detail +
                               lines missing or recon line number(s) +
                               missing';
                 indReconMs = *on; // *in17 *off, erBill2 *on
              Endif;
           Else;
              strReconMs = 'Reconsideration error: Detail +
                            lines missing or recon line number(s) +
                            missing';
              indReconMs = *on; // *in17 *off, erBill2 *on
            Endif;
        EndIf;
        // write Recon message now
        If indReconMs = *on;
           strTfrMsg = strReconMs;
           Exsr WrtTfrMsg;
        EndIf;

        value = U1chrg;
        If sign = '-';
           scTfrPChrg = Amount * -1;
        Else;
           scTfrPChrg = Amount;
        EndIf;

        value = U1MRA;
        If sign = '-';
           scTfrPMRA = Amount * -1;
        Else;
           scTfrPMRA = Amount;
        EndIf;

        value = U1ndis;
        If sign = '-';
           scTfrNDis = Amount * -1;
        Else;
           scTfrNDis = Amount;
        EndIf;

        value = U1paid;
        If sign = '-';
           scTfrPaid = Amount * -1;
        Else;
           scTfrPaid = Amount;
        EndIf;

        scTfrCost = U1cost;
        Monitor;
        scTfrFSvDt = %Date(U1frdt:*iso);
        On-error;
           scTfrFSvDt = *loval;
        Endmon;
        Monitor;
        scTfrTSvDt = %Date(U1frdt:*iso);
        On-error;
           scTfrTSvDt = *loval;
        Endmon;
        //*** Beg Add *** 10/05/23 ******************************************
        If U1prFl = 'R';
           visTrnsBtn = *off;
        Else;
           visTrnsBtn = *on;
        Endif;
        //*** End Add *** 10/05/23 ******************************************

       EndSr;

       //===================================================================
       // PrvSrch - Search for a provider
       //===================================================================
       BegSr PrvSrch;

        ppCall = '2';
        ppFein = *blanks;
        ppMod  = *blanks;
        ppRtcd = *blanks;
        ppType = *blanks;

        Callp F0032R (ppCall:ppFein:ppMod:ppRtcd:ppType);

        scTfrPFFo = *on;
        If ppRtcd = 'A' or ppRtcd = 'B' or
          ppFein = *blanks;
           // *in32 on, may set focus
        Else;
           scTfrPFein = %Int(ppFein);
           scTfrPMod = %Int(ppMod);
           ppFein# = scTfrPFein;
           ppMod#  = scTfrPMod;
           ppPrNm = *blanks;
           ppErr  = *blanks;
           erTfrPFein = *off;

           Callp FA725R (ppFein#:ppMod#:ppPrNm:ppErr);

           If ppErr <> '0';
              scTfrPrvNm = ppPrNm;
           EndIf;

        EndIf;

       Endsr;

       //===================================================================
       // UpdTrnsfr - Update Transfer process status
       //===================================================================
       Begsr UpdTrnsfr;

        Chain scERRrn# pu10p;
        If %Found(fau1p);
           U1co# = CMco#;
           U1sub# = CMsub#;
           U1fnd = CMfnd;
           U1fyr = CMfyr;
           U1case = CMcase;
           U1pss# = CMss#;
           U1acdt = CMacdt;
           U1pfei = PDfein;
           U1mod = PDmod;
           If Ind91 = *off;
              If U1eob# = *zeros;
                 Chain usCo# pc100;
                 If %Found(fmc1l);
                    C1eob# += 1;
                    Update pc100;
                 EndIf;
                 U1eob# = C1eob#;
              EndIf;
           Else;
              EvalR U1dlre = xxderr;
           EndIf;
           U1rcn = *zeros;
           U1dlus = *blanks;
           U1dlpg = *blanks;
           U1dltm = *zeros;
           U1dldt = *zeros;
           %Subst(U1dlre:1:69) = *blanks;

           Update pu10p;
        EndIf;
        If Ind91 = *off;
           ppFPRVMbNm = *blanks;
           ppU9Rrn = *zeros;
           ppU1Rrn = scERRrn#;
           ppPrChg = xxChg;
           If isCareW = *on;
              Callp F8042RA (ppFPRVMbNm:ppU9Rrn:ppU1Rrn:ppPrChg);
           Else;
              Callp F8042R (ppFPRVMbNm:ppU9Rrn:ppU1Rrn:ppPrChg);
           EndIf;
        EndIf;

        // may update error subfile with the trasfer process status
        // would need to include a field in SF for DERSTS and load RtTfrCd
           // scERDErr = RtCdTfr; // add this field in SF as well
           // Update sflUplErr;

        // show window for transfer success of failure
        Clear rcdERInfo;
        vsbILbl01 = *off;
        vsbIMsg01 = *off;
        dsbIMsg01 = *on;
        vsbILbl02 = *off;
        vsbIMsg02 = *off;
        dsbIMsg02 = *on;
        vsbIEnter = *off;
        dsbIEnter = *on;
        vsbICancel = *off;
        dsbICancel = *on;

        scInfHdg = 'Upload Bill Transfer';

        scILbl01 = 'Upload Bill ID';
        vsbILbl01 = *on;
        scIMsg01 = scERBill#;
        vsbIMsg01 = *on;

        scILbl02 = 'Bill Transfer Status';
        vsbILbl02 = *on;
        vsbIMsg02 = *on;
        dsbIMsg02 = *on;
        If RtCdTfr <> *blanks;
           scIMsg02 = 'Transfer resulted in error : reason code ' +
                      %Trim(RtCdTfr);
        Else;
           scIMsg02 = 'Transfer completed successfully';
        EndIf;

        scIValBtn1 = 'Return';
        vsbIEnter = *on;
        dsbIEnter = *off;
        vsbICancel = *off;
        dsbICancel = *on;

        DoU scIEnter = *on or scICancel = *on;
           Exfmt rcdERInfo;

           Select;
           When scICancel = *on;
              scICancel = *off;
              Leave;

           When scIEnter = *on;
              scIEnter = *off;
              Leave;

           EndSl;
        EndDo;

       Endsr;

       //===================================================================
       // WrtTfrMsg - Write messages for Transfer screen
       //===================================================================
       BegSr WrtTfrMsg;

        Select;

         When scTfrMsg1 = *blanks;
            scTfrMsg1 = strTfrMsg;
            vsbTfrMsg1 = *on;

         When scTfrMsg2 = *blanks;
            scTfrMsg2 = strTfrMsg;
            vsbTfrMsg2 = *on;

         When scTfrMsg3 = *blanks;
            scTfrMsg3 = strTfrMsg;
            vsbTfrMsg3 = *on;

         When scTfrMsg4 = *blanks;
            scTfrMsg4 = strTfrMsg;
            vsbTfrMsg4 = *on;

         When scTfrMsg5 = *blanks;
            scTfrMsg5 = strTfrMsg;
            vsbTfrMsg5 = *on;

         When scTfrMsg6 = *blanks;
            scTfrMsg6 = strTfrMsg;
            vsbTfrMsg6 = *on;

         Other;
        EndSl;

        If scTfrMsg1 = *blanks;
           vsbTfrMsg1 = *off;
        EndIf;

        If scTfrMsg2 = *blanks;
           vsbTfrMsg2 = *off;
        EndIf;

        If scTfrMsg3 = *blanks;
           vsbTfrMsg3 = *off;
        EndIf;

        If scTfrMsg4 = *blanks;
           vsbTfrMsg4 = *off;
        EndIf;

        If scTfrMsg5 = *blanks;
           vsbTfrMsg5 = *off;
        EndIf;

        If scTfrMsg6 = *blanks;
           vsbTfrMsg6 = *off;
        EndIf;

        strTfrMsg = *blanks;
        dsbTfrMsg = *on;

       Endsr;

      /end-free
     p procTrnsfr      E

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

       indNtwk = *off;
       If q1prms >= 3;
          If @@ntwk <> *blanks;
             kkNtwk = @@ntwk;
             indNtwk = *on;
          Endif;
       Endif;

       vsbERlink = *on;
       vsbESlink = *on;
       // Program Mode
       If q1prms >= 5;
          Select;
          When @@mode = 'D';
             dspMode = *on;
             vsbERlink = *off;
             vsbESlink = *off;
          Endsl;
       Endif;

       tabindex = 0;
       If q1prms >= 6;
          If @@tabind <> ' ';
             tabIndex = %Int(@@tabind);
          Endif;
       Endif;

       // apShowMenu = *on;
       aMenHeight = 145;
       apFootHght = aMenHeight + 125;

       // get company of the user and other data based on user
       Callp FA007R (ppCo#:ppFnd#:ppCoNm:ppFnNm:ppSub#:ppFyr#);
       usCo# = ppCo#;
       usFnd# = ppFnd#;
       usSub# = ppSub#;
       usFyr# = ppFyr#;
       scCompName = ppCoNm;

       // Get Client ID
       getClientID(outCltId);

       If outCltId = '132';
          isCompW = *on;
       Else;
          isCompW = *off;
       EndIf;

       If outCltId = '136';
          isCareW = *on;
       Else;
          isCareW = *off;
       EndIf;

       If outCltId = '045';
          isLCTA = *on;
       Else;
          isLCTA = *off;
       EndIf;

       // Get System Flags
       getSysFlags(wcmsInst:wcmcInst:wcmcInt);

       rtvSettings(@@Cchg:@@Cifs:@@Csts:@@Cdny:@@Ccvfl:@@Cgrp:
                   @@Cemp:@@Eifs:@@Eagt:@@Eemp:@@Fifs:@@Ftyp:
                   @@Udup:@@Udrc:@@Urel:@@Upcl:@@Updn:@@Udpn:
                   @@ClDays:@@IncZro:@@Crcd:@@CrDth:@@RtCd);
       If @@RtCd = '1';
         isSettings = '1';
       Else;
         isSettings = '0';
       EndIf;

       If not %Open(STACTPNL);
          Open STACTPNL;
       Endif;

      /end-free
     p programStart    e
