     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE FPICDINFO - Provider Bill Code Information Program
     H NOMAIN
     H BNDDIR('FTBNDDIR')
     h**********************************************************************
     h**   PROGRAM DESCRIPTION:                                       *
     h**                                                              *
     h**    This program is used to display Code Information for      *
     h**    medical procedure codes, etc.                             *
     h**                                                              *
      *===============================================================*
      *  Created   : 06/27/18                                         *
      *  Programmer: C:079                                            *
      *  Project   : TX101                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *  02/18/20 C: 079 Project: MC433                               *
      *                  Use FMPRL in place of FMPRP                  *
      *                                                               *
      *  07/09/19 C: 079 Project: MC433                               *
      *                  CompSuite Procedure Code Information         *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     f* file definitions
     ffpicdinfo cf   e             workstn usropn
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflTXmed:rrnTXmed)
     ffdcap     if   e             disk    usropn
     ffaeolg    if   e           k disk    usropn
     ffddhl     if   e           k disk    usropn
     fftgtl     if   e           k disk    usropn
     f*** Beg Add *** 07/09/19 *********************************************
     f*** Beg Chg *** 02/18/20 *********************************************
     f*****fmprp     uf   e             disk    usropn
     ffmprl     uf   e             disk    usropn
     f*** End Chg *** 02/18/20 *********************************************
     f*** End Add *** 07/09/19 *********************************************
     d*===============================================================*
     d* prototype definitions
     d*===============================================================*
     d CodeInfo        pr
     d inPrDs                        40    const
     d inRrn                          9  0 const
     d*** Beg Chg *** 07/09/19 *********************************************
     d*** inTabInd                       5    const options(*nopass)
     d inMode                         1    const
     d*** End Chg *** 07/09/19 *********************************************
     d*** Beg Add *** 07/09/19 *********************************************
     d inState                        2    const
     d inCall                         1    const
     d inRrn2                         9  0 const
     d*** End Add *** 07/09/19 *********************************************

     d TXmedInfo       pr

     d ClrSubFiles     pr

     d LoadCdHdr       pr

     d ExFmtScreens    pr

     d GetTXmedInfo    pr

     d LoadTXMedCd     pr
     d*** Beg Add *** 07/09/19 *********************************************
     d LoadPrCdInfo    pr
     d*** End Add *** 07/09/19 *********************************************

     d programStart    pr
     d processMe                       n

     d programEnd      pr

     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d TxMedPhyApi     pr                  ExtPgm('FA763RTX')
     d   ##Rrn                        9  0
     d   ##Desc                     100
     d   ##PrCd                       5
     d   ##Mod1                       2
     d   ##EfDt                       7  0
     d   ##Loc9NonF                   9  2
     d   ##Loc9Facil                  9  2
     d   ##Loc9Prof                   9  2
     d   ##Loc9Tech                   9  2
     d   ##Loc9Anes                   9  2
     d   ##Loc11NonF                  9  2
     d   ##Loc11Facil                 9  2
     d   ##Loc11Prof                  9  2
     d   ##Loc11Tech                  9  2
     d   ##Loc11Anes                  9  2
     d   ##Loc15NonF                  9  2
     d   ##Loc15Facil                 9  2
     d   ##Loc15Prof                  9  2
     d   ##Loc15Tech                  9  2
     d   ##Loc15Anes                  9  2
     d   ##Loc18NonF                  9  2
     d   ##Loc18Facil                 9  2
     d   ##Loc18Prof                  9  2
     d   ##Loc18Tech                  9  2
     d   ##Loc18Anes                  9  2
     d   ##Loc20NonF                  9  2
     d   ##Loc20Facil                 9  2
     d   ##Loc20Prof                  9  2
     d   ##Loc20Tech                  9  2
     d   ##Loc20Anes                  9  2
     d   ##Loc28NonF                  9  2
     d   ##Loc28Facil                 9  2
     d   ##Loc28Prof                  9  2
     d   ##Loc28Tech                  9  2
     d   ##Loc28Anes                  9  2
     d   ##Loc31NonF                  9  2
     d   ##Loc31Facil                 9  2
     d   ##Loc31Prof                  9  2
     d   ##Loc31Tech                  9  2
     d   ##Loc31Anes                  9  2
     d   ##Loc99NonF                  9  2
     d   ##Loc99Facil                 9  2
     d   ##Loc99Prof                  9  2
     d   ##Loc99Tech                  9  2
     d   ##Loc99Anes                  9  2
     d   ##Loc9Used                   9  2
     d   ##Loc11Used                  9  2
     d   ##Loc15Used                  9  2
     d   ##Loc18Used                  9  2
     d   ##Loc20Used                  9  2
     d   ##Loc28Used                  9  2
     d   ##Loc31Used                  9  2
     d   ##Loc99Used                  9  2
     d   ##PcTc                       1
     d   ##Glob                       3
     d   ##PreOp                      5  2
     d   ##IntraOp                    5  2
     d   ##PostOp                     5  2
     d   ##MultSurg                   1
     d   ##Bilat                      1
     d   ##AsstSurg                   1
     d   ##CoSurg                     1
     d   ##TeamSurg                   1
     d   ##EndoBase                   5
     d   ##StatCd                     1
     d   ##Rrn         s              9  0 Inz(0)
     d   ##Desc        s            100    Inz(*blanks)
     d   ##PrCd        s              5    Inz(*blanks)
     d   ##Mod1        s              2    Inz(*blanks)
     d   ##EfDt        s              7  0 Inz(0)
     d   ##Loc9NonF    s              9  2 Inz(0)
     d   ##Loc9Facil   s              9  2 Inz(0)
     d   ##Loc9Prof    s              9  2 Inz(0)
     d   ##Loc9Tech    s              9  2 Inz(0)
     d   ##Loc9Anes    s              9  2 Inz(0)
     d   ##Loc11NonF   s              9  2 Inz(0)
     d   ##Loc11Facil  s              9  2 Inz(0)
     d   ##Loc11Prof   s              9  2 Inz(0)
     d   ##Loc11Tech   s              9  2 Inz(0)
     d   ##Loc11Anes   s              9  2 Inz(0)
     d   ##Loc15NonF   s              9  2 Inz(0)
     d   ##Loc15Facil  s              9  2 Inz(0)
     d   ##Loc15Prof   s              9  2 Inz(0)
     d   ##Loc15Tech   s              9  2 Inz(0)
     d   ##Loc15Anes   s              9  2 Inz(0)
     d   ##Loc18NonF   s              9  2 Inz(0)
     d   ##Loc18Facil  s              9  2 Inz(0)
     d   ##Loc18Prof   s              9  2 Inz(0)
     d   ##Loc18Tech   s              9  2 Inz(0)
     d   ##Loc18Anes   s              9  2 Inz(0)
     d   ##Loc20NonF   s              9  2 Inz(0)
     d   ##Loc20Facil  s              9  2 Inz(0)
     d   ##Loc20Prof   s              9  2 Inz(0)
     d   ##Loc20Tech   s              9  2 Inz(0)
     d   ##Loc20Anes   s              9  2 Inz(0)
     d   ##Loc28NonF   s              9  2 Inz(0)
     d   ##Loc28Facil  s              9  2 Inz(0)
     d   ##Loc28Prof   s              9  2 Inz(0)
     d   ##Loc28Tech   s              9  2 Inz(0)
     d   ##Loc28Anes   s              9  2 Inz(0)
     d   ##Loc31NonF   s              9  2 Inz(0)
     d   ##Loc31Facil  s              9  2 Inz(0)
     d   ##Loc31Prof   s              9  2 Inz(0)
     d   ##Loc31Tech   s              9  2 Inz(0)
     d   ##Loc31Anes   s              9  2 Inz(0)
     d   ##Loc99NonF   s              9  2 Inz(0)
     d   ##Loc99Facil  s              9  2 Inz(0)
     d   ##Loc99Prof   s              9  2 Inz(0)
     d   ##Loc99Tech   s              9  2 Inz(0)
     d   ##Loc99Anes   s              9  2 Inz(0)
     d   ##Loc9Used    s              9  2
     d   ##Loc11Used   s              9  2
     d   ##Loc15Used   s              9  2
     d   ##Loc18Used   s              9  2
     d   ##Loc20Used   s              9  2
     d   ##Loc28Used   s              9  2
     d   ##Loc31Used   s              9  2
     d   ##Loc99Used   s              9  2
     d   ##PcTc        s              1    Inz(' ')
     d   ##Glob        s              3    Inz(*blanks)
     d   ##PreOp       s              5  2 Inz(0)
     d   ##IntraOp     s              5  2 Inz(0)
     d   ##PostOp      s              5  2 Inz(0)
     d   ##MultSurg    s              1    Inz(' ')
     d   ##Bilat       s              1    Inz(' ')
     d   ##AsstSurg    s              1    Inz(' ')
     d   ##CoSurg      s              1    Inz(' ')
     d   ##TeamSurg    s              1    Inz(' ')
     d   ##EndoBase    s              5    Inz(*blanks)
     d   ##StatCd      s              1    Inz(' ')

     d*=====================================================================
     d* Data Structures
     d*=====================================================================

     d  Allow          DS                  Dim(9) Inz
     d                                     Qualified
     d  Type                         30A
     d  BrazLoc9                      9S 2
     d  DallLoc11                     9S 2
     d  GalvLoc15                     9S 2
     d  HarrLoc18                     9S 2
     d  JeffLoc20                     9S 2
     d  TarrLoc28                     9S 2
     d  TravLoc31                     9S 2
     d  OthLoc99                      9S 2
     d*
     d psds          esds                  extname(szq1p)
     d*=====================================================================
     d* constant definitions
     d*=====================================================================
     d*=====================================================================
     d* standalone variables
     d*=====================================================================
     d*
     d $DsType         s              2    inz
     d $PrDs           s             40    inz
     d $DsCode         s              3    inz
     d $Efdt           s              7p 0 inz
     d $Rrn            s              9p 0 inz
     d $TabInd         s              5p 0 inz

     d $Cmd            s           2750    inz
     d $CmdLen         s             15  5 inz

     d $co#            s              3p 0 inz
     d $dsdesc         s            100    inz
     d $com1           s             60    inz
     d $com2           s             60    inz
     d process         s               n
     d xxCdStatus      s              1a   Inz(*blank)
     d xxCdEfDt        s              7  0 Inz(*Zeros)
     d xxCode          s                   like(DHprCd) Inz(*blanks)
     d xxCdDesc        s            100    Inz(*blanks)
     d i               s              5  0
     d*** Beg Add *** 07/11/19 *********************************************
     d AdjState        s              2
     d Mode            s              1
     d Call            s              1
     d*** End Add *** 07/11/19 *********************************************
     d*** Beg Add *** 02/04/20 *********************************************
     d $Rrn2           s              9p 0 inz
     d*** End Add *** 02/04/20 *********************************************
     d*=====================================================================
     d* copybook declarations
     d*=====================================================================
     d*
     d/copy sPrUsrPrms

     D* Procedures
     P*====================================================================
     P*  CodeInfo - Process Code Info
     P*====================================================================
     P CodeInfo        B                   Export
     d CodeInfo        PI
     d inPrDs                        40    const
     d inRrn                          9  0 const
     d*** Beg Chg *** 07/09/19 *********************************************
     d*** inTabInd                       5    const options(*nopass)
     d inMode                         1    const
     d*** End Chg *** 07/09/19 *********************************************
     d*** Beg Add *** 07/09/19 *********************************************
     d inState                        2    const
     d inCall                         1    const
     d inRrn2                         9  0 const
     d*** End Add *** 07/09/19 *********************************************
      /free
       If %parms >= 1;
          If inPrDs <> *blanks;
             $PrDs = inPrDs;
          Else;
             $PrDs = 'Code Information';
          Endif;
       Endif;
       If %parms >= 2;
          if inRrn <> *zeros;
             $Rrn = inRrn;
          else;
             $Rrn = 1;
          endif;
       Else;
          $Rrn = 1;
       Endif;
       If %parms > 5;
          $Rrn2 = inRrn2;
       Endif;

       //*** Beg Del *** 07/09/19 ******************************************
       // Tab Index
       //***If %parms >= 3;
       //***   if inTabInd <> *blanks;
       //***      $TabInd = %int(inTabInd);
       //***   else;
       //***      $TabInd = 0;
       //***   endif;
       //***Else;
       //***   $TabInd = 0;
       //***Endif;
       //*** End Del *** 07/09/19 ******************************************
       //*** Beg Add *** 07/09/19 ******************************************
       If %parms > 2;
          Mode = inMode;
       Endif;

       If %parms > 3;
          AdjState = inState;
       Endif;

       If %parms > 4;
          Call = inCall;
       Endif;

       programStart(process);


       If process = *on;
          Select;
       //*** Beg Chg *** 07/09/19 *****************************************
       //**When $TabInd = 0;
           When AdjState = 'TX';
       //*** End Chg *** 07/09/19 *****************************************
            TXmedInfo();
           Other;
       //*** Beg Add *** 07/09/19 *****************************************
            AllPrCdInfo();
       //*** End Add *** 07/09/19 *****************************************
          EndSl;

       Endif;

       ProgramEnd();

       *inlr = *on;
       Return;

      /end-free
     P CodeInfo        E

     P*====================================================================
     P*  TXMedInfo - Display Texas Code Information
     P*====================================================================
     P TXMedInfo       B                   export
     D TXMedInfo       PI
     D*
     d*
     d ##mode          s              1    inz
     d #IndGrEvt       s               n   inz(*off)

      /free

       // process until exit button is pressed.
       Dou ExitNow = *On;

          // clear Sub-files
          ClrSubfiles();

          // load Header
          LoadCdHdr();

          // load Texas Medical Code Detail info
          LoadTXmedCd();

          // display and process discount screen
          ExfmtScreens();

          Select;

          // do nothing - catch tab click event
          //*** Beg Del *** 07/09/19 ***************************************
          //***When tabClick = *on;
          //***   tabClick = *off;
          //*** End Del *** 07/09/19 ***************************************

          When btnRefresh = *On;
             btnRefresh =*Off;

          When btnHdrCan = *On;
             btnHdrCan = *Off;
             leave;

          Endsl;

       Enddo;
      /end-free
     P TXMedInfo       E
       //*** Beg Add *** 07/09/19 ****************************************
     P*====================================================================
     P*  AllPrCdInfo - Display Procedure Code Information
     P*====================================================================
     P AllPrCdInfo     B                   export
     D AllPrCdInfo     PI
     D*
     d*
     d ##mode          s              1    inz
     d #IndGrEvt       s               n   inz(*off)

      /free

       // process until exit button is pressed.
       Dou ExitNow = *On;

          // clear Sub-files
          ClrSubfiles();

          // load Header
          LoadCdHdr();

          // load Texas Medical Code Detail info
          LoadPrCdInfo();

          // display and process discount screen
          ExfmtScreens();

          Select;

          // do nothing - catch tab click event
          //*** Beg Del *** 07/09/19 ***************************************
          //***When tabClick = *on;
          //***   tabClick = *off;
          //*** End Del *** 07/09/19 ***************************************

          When btnRefresh = *On;
             btnRefresh =*Off;

          When btnHdrCan = *On;
             btnHdrCan = *Off;
             leave;

          Endsl;

       Enddo;
      /end-free
     P AllPrCdInfo     E
       //*** End Add *** 07/09/19 ****************************************

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free
       //*** Beg Add *** 07/09/19 ******************************************
       Select;
       When AdjState = 'TX';
       //*** End Add *** 07/09/19 ******************************************
          rrnTXmed = 0;
          clrTXmdSub = *On;
          dspTXmdSub = *Off;
          dspTXmdCtl = *Off;
          Write RcdTXinfo;
       //*** Beg Add *** 07/09/19 ******************************************
       When AdjState <> 'TX';
          Write RcdPrInfo;

       Endsl;
       //*** End Add *** 07/09/19 ******************************************
      /End-Free

     P ClrSubFiles     E
       //===================================================================
       // LoadCdHdr - Load Header Information
       //===================================================================
     P LoadCdHdr       B
     D LoadCdHdr       PI
      /Free
        //*** Beg Add *** 07/09/19 ***************************************
        //*** Beg Chg *** 07/09/19 ***************************************
        //***   Chain(n) $Rrn ppr01;
           Chain(n) $Rrn2 ppr01;
        //*** End Chg *** 07/09/19 ***************************************
        //*** Beg Chg *** 02/18/20 ***************************************
        //**If %Found(fmprp);
           If %Found(fmprl);
        //*** End Chg *** 02/18/20 ***************************************
              xxCode = PRcpt4;
              xxCdDesc = PRdsc1;
              xxCdEfdt = PRefdt;
              xxCdStatus = PRstat;
           Endif;
        //*** End Add *** 07/09/19 ***************************************
           // load header fields
           scHdCdHdg = $prds;
           scHdCode = xxCode;
           scHdCdDesc = xxCdDesc;
           Monitor;
           scHdEfDt = %date(XXCdEfDt:*cymd);
           on-error;
           $Efdt = 0010101;
           Endmon;
           scHdStatus = xxCdStatus;
      /End-Free

     P LoadCdHdr       E
       //===================================================================
       // ExfmtScreens - Write Screen Formats;
       //===================================================================
     P ExFmtScreens    B
     D ExFmtScreens    PI

        Select;

        //*** Beg Chg *** 07/09/19 *****************************************
        //***When TabIndex = 0;
        When AdjState = 'TX';
        //*** End Chg *** 07/09/19 *****************************************

        //*** End Chg *** 07/09/19 *****************************************
           TabHeight = 600;

        //*** Beg Chg *** 07/09/19 *****************************************
        //***Write rcdTabPan;
           Write rcdCodeInf;
        //*** End Chg *** 07/09/19 *****************************************
           Write rcdCodeHdr;
           Exfmt rcdTXinfo;
        //*** Beg Chg *** 07/09/19 *****************************************
        //*** Read rcdTabPan;
           Read rcdCodeInf;
        //*** End Chg *** 07/09/19 *****************************************
           Read rcdCodeHdr;

           clear scHdrMsg;
           dspHdrMsg = *off;
        //*** Beg Add *** 07/09/19 *****************************************
        Other;

           TabHeight = 590;

           Write rcdCodeInf;
           Write rcdCodeHdr;
           Exfmt rcdPrInfo;
           Read rcdCodeInf;
           Read rcdCodeHdr;

           clear scHdrMsg;
           dspHdrMsg = *off;
        //*** End Add *** 07/09/19 *****************************************

        EndSl;

      /End-Free
     P ExFmtScreens    E
       //===================================================================
       // GetTXmedInfo - Get Texas Medical Code Info
       //===================================================================
     P GetTXmedInfo    B
     D GetTXmedInfo    PI
      /Free

       for i = 1 to 8;
          Select;
          When i = 1;
             Allow(i).Type = 'Non facility allowance';
             Allow(i).BrazLoc9 = ##Loc9NonF;
             Allow(i).DallLoc11 = ##Loc11NonF;
             Allow(i).GalvLoc15 = ##Loc15NonF;
             Allow(i).HarrLoc18 = ##Loc18NonF;
             Allow(i).JeffLoc20 = ##Loc20NonF;
             Allow(i).TarrLoc28 = ##Loc28NonF;
             Allow(i).TravLoc31 = ##Loc31NonF;
             Allow(i).OthLoc99 = ##Loc99NonF;
          When i=2;
             Allow(i).Type = 'Facility allowance';
             Allow(i).BrazLoc9 = ##Loc9Facil;
             Allow(i).DallLoc11 = ##Loc11Facil;
             Allow(i).GalvLoc15 = ##Loc15Facil;
             Allow(i).HarrLoc18 = ##Loc18Facil;
             Allow(i).JeffLoc20 = ##Loc20Facil;
             Allow(i).TarrLoc28 = ##Loc28Facil;
             Allow(i).TravLoc31 = ##Loc31Facil;
             Allow(i).OthLoc99 = ##Loc99Facil;
          When i=3;
             Allow(i).Type = 'Anes/Ambul/Rental/Hourly';
             Allow(i).BrazLoc9 = ##Loc9Anes;
             Allow(i).DallLoc11 = ##Loc11Anes;
             Allow(i).GalvLoc15 = ##Loc15Anes;
             Allow(i).HarrLoc18 = ##Loc18Anes;
             Allow(i).JeffLoc20 = ##Loc20Anes;
             Allow(i).TarrLoc28 = ##Loc28Anes;
             Allow(i).TravLoc31 = ##Loc31Anes;
             Allow(i).OthLoc99 = ##Loc99Anes;
          When i=4;
             Allow(i).Type = 'Used allowance';
             Allow(i).BrazLoc9 = ##Loc9Used;
             Allow(i).DallLoc11 = ##Loc11Used;
             Allow(i).GalvLoc15 = ##Loc15Used;
             Allow(i).HarrLoc18 = ##Loc18Used;
             Allow(i).JeffLoc20 = ##Loc20Used;
             Allow(i).TarrLoc28 = ##Loc28Used;
             Allow(i).TravLoc31 = ##Loc31Used;
             Allow(i).OthLoc99 = ##Loc99Used;
          When i=5;
             Allow(i).Type = 'Prof componenent';
             Allow(i).BrazLoc9 = ##Loc9Prof;
             Allow(i).DallLoc11 = ##Loc11Prof;
             Allow(i).GalvLoc15 = ##Loc15Prof;
             Allow(i).HarrLoc18 = ##Loc18Prof;
             Allow(i).JeffLoc20 = ##Loc20Prof;
             Allow(i).TarrLoc28 = ##Loc28Prof;
             Allow(i).TravLoc31 = ##Loc31Prof;
             Allow(i).OthLoc99 = ##Loc99Prof;
          When i=6;
             Allow(i).Type = 'Tech componenent';
             Allow(i).BrazLoc9 = ##Loc9Tech;
             Allow(i).DallLoc11 = ##Loc11Tech;
             Allow(i).GalvLoc15 = ##Loc15Tech;
             Allow(i).HarrLoc18 = ##Loc18Tech;
             Allow(i).JeffLoc20 = ##Loc20Tech;
             Allow(i).TarrLoc28 = ##Loc28Tech;
             Allow(i).TravLoc31 = ##Loc31Tech;
             Allow(i).OthLoc99 = ##Loc99Tech;
          When i=7;
             Allow(i).Type = 'Rural miles 1-17';
             Allow(i).BrazLoc9 = 0;
             Allow(i).DallLoc11 = 0;
             Allow(i).GalvLoc15 = 0;
             Allow(i).HarrLoc18 = 0;
             Allow(i).JeffLoc20 = 0;
             Allow(i).TarrLoc28 = 0;
             Allow(i).TravLoc31 = 0;
             Allow(i).OthLoc99 = 0;
          When i=8;
             Allow(i).Type = 'Rural miles';
             Allow(i).BrazLoc9 = 0;
             Allow(i).DallLoc11 = 0;
             Allow(i).GalvLoc15 = 0;
             Allow(i).HarrLoc18 = 0;
             Allow(i).JeffLoc20 = 0;
             Allow(i).TarrLoc28 = 0;
             Allow(i).TravLoc31 = 0;
             Allow(i).OthLoc99 = 0;
          EndSl;
       EndFor;

       Chain (CAco#:'MIPC':##PcTc) pgt01;
       If %Found(ftgtl);
          scPcTc = %Trim(##PcTc) + ' - ' + GTdesc;
       Else;
          scPcTc = ##PcTc;
       Endif;

       Chain (CAco#:'MI50':##Bilat) pgt01;
       If %Found(ftgtl);
          scBilat = %Trim(##Bilat) + ' - ' + GTdesc;
       Else;
          scBilat = ##Bilat;
       Endif;

       Chain (CAco#:'MI51':##MultSurg) pgt01;
       If %Found(ftgtl);
          scMultSurg = %Trim(##MultSurg) + ' - ' + GTdesc;
       Else;
          scMultSurg = ##MultSurg;
       Endif;

       Chain (CAco#:'MI62':##CoSurg) pgt01;
       If %Found(ftgtl);
          scCoSurg = %Trim(##CoSurg) + ' - ' + GTdesc;
       Else;
          scCoSurg = ##CoSurg;
       Endif;

       Chain (CAco#:'MI66':##TeamSurg) pgt01;
       If %Found(ftgtl);
          scTeamSurg = %Trim(##TeamSurg) + ' - ' + GTdesc;
       Else;
          scTeamSurg = ##TeamSurg;
       Endif;

       Chain (CAco#:'MI80':##AsstSurg) pgt01;
       If %Found(ftgtl);
          scAsstSurg = %Trim(##AsstSurg) + ' - ' + GTdesc;
       Else;
          scAsstSurg = ##AsstSurg;
       Endif;

       scGlobSurg = ##Glob;
       scPreOp = ##PreOp;
       scIntraOp = ##IntraOp;
       scPostOp = ##PostOp;
       scEndoBase = ##EndoBase;

      /End-Free
     P GetTXMedInfo    E
       //*** Beg Add *** 07/09/19 ******************************************
       //===================================================================
       // GetPrCdInfo - Get Procedure Code Info
       //===================================================================
     P GetPrCdInfo     B
     D GetPrCdInfo     PI
      /Free

       scNonFacil = PRmra;

      /End-Free
     P GetPrCdInfo     E
       //*** End Add *** 07/09/19 ******************************************

       //===================================================================
       // LoadTXMedCd - Loads Code Information for Texas Medical Bills
       //===================================================================
     P LoadTXMedCd     B
     D LoadTXMedCd     PI
      /Free

       clrTXmdSub = *off;
       dspTXmdSub = *on;
       dspTXmdCtl = *on;
       GetTXmedInfo();

       for i = 1 to 8;

          scAllowTp = Allow(i).Type;
          scBrazLoc9 = Allow(i).BrazLoc9;
          scDalLoc11 = Allow(i).DallLoc11;
          scGalLoc15 = Allow(i).GalvLoc15;
          scHarLoc18 = Allow(i).HarrLoc18;
          scJefLoc20 = Allow(i).JeffLoc20;
          scTarLoc28 = Allow(i).TarrLoc28;
          scTrvLoc31 = Allow(i).TravLoc31;
          scOthLoc99 = Allow(i).OthLoc99;

          rrnTXmed = rrnTXmed + 1;
          write sflTXmed;

       EndFor;

      /End-Free
     P LoadTXmedCd     E

       //*** Beg Add *** 07/09/19 ******************************************
       //===================================================================
       // LoadPrCdInfo - Loads Code Information for Medical Bills
       //===================================================================
     P LoadPrCdInfo    B
     D LoadPrCdInfo    PI
      /Free

       GetPrCdInfo();

      /End-Free
     P LoadPrCdInfo    E
       //*** End Add *** 07/09/19 ******************************************

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n
      /free

        processme = *on;

        If processme = *on;

       // open files
           open fpiCdInfo;
           Open fdcap;
           Open fddhl;
           Open faeolg;
           Open ftgtl;
       //*** Beg Add *** 07/09/19 ******************************************
        //*** Beg Chg *** 02/18/20 ***************************************
        //*** Open fmprp;
           Open fmprl;
        //*** End Chg *** 02/18/20 ***************************************
       //*** End Add *** 07/09/19 ******************************************
       //
       // Call API to calculate Texas Medicare Physicain Amt
       //
       Select;
          //*** Beg Chg *** 07/09/19 ***************************************
          //*** When $TabInd = 0;
           When AdjState = 'TX'and Call <> 'S';
          //*** End Chg *** 07/09/19 ***************************************
              ##Rrn = $Rrn;
              callp TxMedPhyApi(##Rrn:##Desc:##PrCd:##Mod1:##EfDt:
               ##Loc9NonF:##Loc9Facil:##Loc9Prof:##Loc9Tech:##Loc9Anes:
               ##Loc11NonF:##Loc11Facil:##Loc11Prof:##Loc11Tech:##Loc11Anes:
               ##Loc15NonF:##Loc15Facil:##Loc15Prof:##Loc15Tech:##Loc15Anes:
               ##Loc18NonF:##Loc18Facil:##Loc18Prof:##Loc18Tech:##Loc18Anes:
               ##Loc20NonF:##Loc20Facil:##Loc20Prof:##Loc20Tech:##Loc20Anes:
               ##Loc28NonF:##Loc28Facil:##Loc28Prof:##Loc28Tech:##Loc28Anes:
               ##Loc31NonF:##Loc31Facil:##Loc31Prof:##Loc31Tech:##Loc31Anes:
               ##Loc99NonF:##Loc99Facil:##Loc99Prof:##Loc99Tech:##Loc99Anes:
               ##Loc9Used:##Loc11Used:##Loc15Used:##Loc18Used:##Loc20Used:
               ##Loc28Used:##Loc31Used:##Loc99Used:
               ##PcTc:##Glob:##PreOp:##IntraOp:##PostOp:##MultSurg:
               ##Bilat:##AsstSurg:##CoSurg:##TeamSurg:##EndoBase:
               ##StatCd);

        //*** Beg Del *** 07/09/19 ***************************************
        //***
        //*** Chain $Rrn pca01;
        //*** If %Found(fdcap);
        //***    Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#) peo01;
        //***    If %Found(faeolg);
        //***       Select;
        //***       When EObtyp = 'N';
        //***          Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq)
        //***                 pdh01;
        //***          If %Found(fddhl);
        //***             xxCode = DHprCd;
        //***             xxCdDesc = ##Desc;
        //***             xxCdEfdt = CAfDat;
        //***             xxCdStatus = ##StatCd;
        //***          Endif;
        //***       Endsl;
        //***    Endif;
        //*** Endif;
        //*** End Del *** 07/09/19 ***************************************
        //*** Beg Add *** 07/09/19 ***************************************
           When AdjState = 'TX'and Call = 'S';
        //*** Beg Chg *** 07/09/19 ***************************************
        //*** Beg Chg *** 02/18/20 ***************************************
        //*** Chain(n) $rrn ppr01;
        //*** If %Found(fmprp);
              Chain(n) $rrn2 ppr01;
              If %Found(fmprl);
        //*** End Chg *** 02/18/20 ***************************************
                 ##Loc9NonF  = PRmra;
                 ##Loc9Facil = PR92N6;
                 ##Loc9Anes  = PRanAm;
                 ##Loc9Prof  = PRprof;
                 ##Loc9Tech  = PR92N1;

                 ##Loc11NonF  = PR92N8;
                 ##Loc11Facil = PR92N9;
                 ##Loc11Anes  = PR92Na;
                 ##Loc11Prof  = PR92Nb;
                 ##Loc11Tech  = PR92Nc;

                 ##Loc15NonF  = PR92Ne;
                 ##Loc15Facil = PR92Nf;
                 ##Loc15Anes  = PR92Ng;
                 ##Loc15Prof  = PR92Nh;
                 ##Loc15Tech  = PR92Ni;

                 ##Loc18NonF  = PR92Nk;
                 ##Loc18Facil = PR92Nl;
                 ##Loc18Anes  = PR92Nm;
                 ##Loc18Prof  = PR92Nn;
                 ##Loc18Tech  = PR92No;

                 ##Loc20NonF  = PR92Nq;
                 ##Loc20Facil = PR92Nr;
                 ##Loc20Anes  = PR92Ns;
                 ##Loc20Prof  = PR92Nt;
                 ##Loc20Tech  = PR92Nu;

                 ##Loc28NonF  = PR92Nw;
                 ##Loc28Facil = PR92Nx;
                 ##Loc28Anes  = PR92Ny;
                 ##Loc28Prof  = PR92Nz;
                 ##Loc28Tech  = PR92O1;

                 ##Loc31NonF  = PR92O3;
                 ##Loc31Facil = PR92O4;
                 ##Loc31Anes  = PR92O5;
                 ##Loc31Prof  = PR92O6;
                 ##Loc31Tech  = PR92O7;

                 ##Loc99NonF  = PR92O9;
                 ##Loc99Facil = PR92Oa;
                 ##Loc99Anes  = PR92Ob;
                 ##Loc99Prof  = PR92Oc;
                 ##Loc99Tech  = PR92Od;

                 ##PcTc = PR1Ab;
                 ##Glob = PR5A1;
                 ##PreOp = PR52N1;
                 ##IntraOp = PR52N2;
                 ##PostOp = PR52N3;
                 ##MultSurg = PR1Ac;
                 ##Bilat = PR1Ad;
                 ##AsstSurg = PR1A8;
                 ##CoSurg = PR1A9;
                 ##TeamSurg = PR1AA;
                 ##EndoBase = PR5A2;

              Endif;
        //*** End Add *** 07/09/19 ***************************************
           Endsl;

           If $Rrn > *zeros;

           Endif;

           // initialize variables
           //*** Beg Del *** 07/09/19 *************************************
           //***TabIndex = $TabInd;
           //*** End Del *** 07/09/19 *************************************

           $co# = GetUserInfo(q1user);

        Endif;
      /End-free
     P ProgramStart    E
       //===================================================================
       // ProgramEnd - Program End tasks
       //===================================================================
     P ProgramEnd      B
     d ProgramEnd      PI
      /free

        clear $PrDs;
        clear $Rrn;
        clear $TabInd;
        clear $Cmd;
        clear $CmdLen;
        clear $co#;

        //*** Beg Chg *** 07/09/19 *****************************************
        //***clear rcdTabPan;
        clear rcdCodeInf;
        //*** End Chg *** 07/09/19 *****************************************
        clear rcdCodeHdr;
        clear rcdTXinfo;

        if %open(fpiCdInfo);
           close fpiCdInfo;
        endif;

       If %Open(faeolg);
          Close faeolg;
       Endif;

       If %Open(fdcap);
          Close fdcap;
       Endif;

       If %Open(fddhl);
          Close fddhl;
       Endif;

       If %Open(ftgtl);
          Close ftgtl;
       Endif;
       //*** Beg Add *** 07/09/19 ******************************************
       //*** Beg Chg *** 02/18/20 ***************************************
       //***If %Open(fmprp);
       //***   Close fmprp;
       //***Endif;
       If %Open(fmprl);
          Close fmprl;
       Endif;
       //*** End Chg *** 02/18/20 ***************************************
       //*** End Add *** 07/09/19 ******************************************

      /end-free
     P ProgramEnd      E
       //===================================================================
