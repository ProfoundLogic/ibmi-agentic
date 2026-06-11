     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title ISOEXTRACT- Iso Extract File loading
     h dftname(ISOEXTRACT)
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('STBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program Will load data to ISO extract files    *
      *                                                                    *
      *  Called from:  Submit job CL ISOEXTSBC                            *
      *                                                                    *
      *  Created on 07/18/2016 by C:308 for Project: 114WXY                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  02/08/18 C:062 Project 114WJD                                     *
      *                 Populate new fields:                               *
      *                 -Employer #                                        *
      *                 -Region #                                          *
      *                 -Policy Year                                       *
      *                                                                    *
      *  07/19/17 C:079 Support: 114/48344                                 *
      *                 Add Medical to the log file                        *
      *                                                                    *
      *  06/21/17 C:079 Support: 114/47923                                 *
      *                 Add group criteria to medical extract files        *
      *                                                                    *
      *  05/18/17 C:309 Support: 114/47323                                 *
      *                 1) Changes to write on Completed bills in Medical  *
      *                    Extract files (FAISHP/FAISDP)                   *
      *                 2) Populate "ISdprcd" with DPHCPC if DPNDC is blank*
      *                                                                    *
      *  05/17/17 C:053 Support: 114/47323                                 *
      *                 Add default to claim type due to no IMF history rcds
      *                                                                    *
      *                                                                    *
      *  05/10/17 C:079 Support: 114/47024                                 *
      *                 Add log file                                       *
      *                                                                    *
      *  02/08/17 C:079 Project: 114WXY                                    *
      *                 Change to use alpha fields                         *
      *                                                                    *
      *  12/01/16 C:308 Project: 114WYA                                    *
      *                  Process group greater/equal to 300 only.          *
      *                                                                    *
      *  11/29/16 C:308 Project: 114WXY                                    *
      *                 Skip starting 2 digits for -ve & 1 digit for +ve   *
      *                 Amount fields in Extract files.                    *
      *                                                                    *
      *  11/28/16 C:308 Project: 114WXY                                    *
      *                 Add trailing 0's to all Zip code field value       *
      *                 Correct decimal place in amount fields             *
      *                                                                    *
      *  11/15/16 C:079 Project: 114WXY                                    *
      *                 Default CAunts to 1 on all bills                   *
      *                 Default DOB to 1/1/1901 if missing                 *
      *                 If amounts are negative then format with - sign    *
      *                                                                    *
      *  11/04/16 C:309 Project: 114WYA                                    *
      *                 Changed Claimant Key (added " CMSS# + 'E' ")       *
      *                                                                    *
      *  10/17/16 C:079 Project: 114WXY                                    *
      *                 Default units to 1 on dental lines                 *
      *                 Default network service code to UNK                *
      *                                                                    *
      *  10/14/16 C:309 Project: 114WYA                                    *
      *                 Fixed Claim Extract file Date Fields format        *
      *                                                                    *
      *  10/11/16 C:308 Project: 114WXY                                    *
      *                 Add Procedure code type                            *
      *                                                                    *
      *  08/19/16 C:309 Project: 114WYA                                    *
      *                 MSP (ISO) Navigator Reporting for CMS              *
      *                                                                    *
      *  09/08/16 C: 308 Support: 43269                                    *
      *                  Initialize amount fields                          *
      *                                                                    *
      *  08/30/16 C: 308 Project: 114WXY                                   *
      *                  Populate Claim Type Code & Loss Code & FDCAP      *
      *                  deletion case.                                    *
      *                                                                    *
      *  08/29/16 C: 079 Project: 114WXY                                   *
      *                  Do not call ISOEXTRACT for Standalone WCMC groups *
      *                                                                    *
      *  08/25/16 C: 079 Project: 114WXY                                   *
      *                  When EOB is created skip FAEAP                    *
      *                  Load ICD codes correctly                          *
      *                                                                    *
      *  08/18/16 C: 079 Project: 114WXY                                   *
      *                  Change criteria for selecting the EOB             *
      *                  Report Diag Xref as letters not X                 *
      *                                                                    *
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
      *====================================================================*
     fwaisl     uf a e           k disk    Rename(pis01:pis00)
     ffaishl    uf a e           k disk    Rename(pish01:pish00)
     ffaisdl    uf a e           k disk    Rename(pisd01:pisd00)
     fWmcml     if   e           k disk    Rename(pcm01:pcm00)
     fwmrpl     if   e           k disk    Rename(prp01:prp00)
     fWmcml1    if   e           k disk    Rename(pcm01:pcm01)
     fwmcml4    if   e           k disk    Rename(pcm01:pcm04)
     fwmcmla    if   e           k disk    Rename(pcm01:pcm0A)
     fWmcdl     if   e           k disk    Rename(pcd01:pcd00)
     fwmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fwtabl     if   e           k disk    Rename(pcn01:pcn00)
     fWmeml12   if   e           k disk    Rename(pem01:pem12)
     fWmeml22   if   e           k disk    Rename(pem01:pem22)
     fwmeml     if   e           k disk    Rename(pem01:pem00)
     fWdf3l     if   e           k disk    Rename(pf301:pf300)
     f***** Beg Add *** 08/19/16 ***********************
     fSmupl     if   e           k disk    Rename(pup01:pup00)
     f***** End Add *** 08/19/16 ***********************
     fSmupl1    if   e           k disk    Rename(pup01:pup01)
     fWmwhl     if   e           k disk    Rename(pwh01:pwh00)
     fWdell     if   e           k disk    Rename(pel01:pel00)
     ffaeol37   if   e           k disk    Rename(peo01:peo37)
     ffaeolu    if   e           k disk    Rename(peo01:peo0u)
     ffaeol     if   e           k disk    Rename(peo01:peo00)
     ffaeal1    if   e           k disk    Rename(pea01:pea01)
     ffdcal18   if   e           k disk    Rename(pca01:pca18)
     ffddhl     if   e           k disk    Rename(pdh01:pdh00)
     ffddul     if   e           k disk    Rename(pdu01:pdu00)
     ffddpl     if   e           k disk    Rename(pdp01:pdp00)
     ffdddl     if   e           k disk    Rename(pdd01:pdd00)
     ffddel     if   e           k disk    Rename(pde01:pde00)
     ffmcdl     if   e           k disk    Rename(pcd01:pcdfl)
     f                                     Prefix(fm)
     ffmcmlc    if   e           k disk    Rename(pcm01:pcmfc)
     f                                     Prefix(f)
     ffmpdl     if   e           k disk    Rename(ppd01:ppd00)
     ffaeul     if   e           k disk    Rename(peu01:peu00)
     ffaedl     if   e           k disk    Rename(ped01:ped00)
     ffaehl     if   e           k disk    Rename(peh01:peh00)
     ffaeolj    if   e           k disk    Rename(peo01:peo0j)
       //*** Beg Add *** 08/29/16 ********************************
     ffmf1l     if   e           k disk    UsrOpn
       //*** End Add *** 08/29/16 ********************************
      ***** Beg Add *** 08/30/16 ********************************
     fwtgtl     if   e           k disk    Rename(pgt01:pgt00)
     fwmthl     if   e           k disk    Rename(pth01:pth00)
      ***** End Add *** 08/30/16 ********************************
     f
     ***** Beg Add *** 08/19/16 ***********************
     fwdf2l     if   e           k disk    Rename(pf201:pf200)
     fwmofl     if   e           k disk    Rename(pof01:pof00)
     fwmlll     if   e           k disk    Rename(pll01:pll00)
     fwmpdl     if   e           k disk    Rename(ppd01:ppdw0)
     f                                     Prefix(W)
     fsadal3    if   e           k disk    Rename(pda01:pda03)
     fWmcmlh    if   e           k disk    Rename(pcm01:pcm0H)
     fWaisocm1l uf a e           k disk    Rename(pisocm1:pisocm100)
     fWaisocm2l uf a e           k disk    Rename(pisocm2:pisocm200)
     fWaisocm3l uf a e           k disk    Rename(pisocm3:pisocm300)
     fWaisocm4l1uf a e           k disk    Rename(pisocm4:pisocm401)
     fWaisocm5l uf a e           k disk    Rename(pisocm5:pisocm500)
     ***** End Add *** 08/19/16 ***********************
      ***** Beg Add *** 05/10/17 ********************************
     fwxislgp   uf a e             disk
      ***** End Add *** 05/10/17 ********************************
      /copy sprdate
      /copy wprgroup
     ***** Beg Add *** 08/19/16 ***********************
     disoDs            ds
     d strFileDat                     8
     d strIfsLoc                     80
     d strAuth#                       7
     d strElgRsltC                    5
     d strMisDataC                    5
     d strUsrAlrtC                    5
     d strElgRsltS                    5
     d strMisDataS                    5
     d strUsrAlrtS                    5
     d strMIRGrp                      4
      *
     d  IsoCMSPrmp     s            160    dtaara
     ***** End Add *** 08/19/16 ***********************
     d strStateCounty  s             20
     d strIsClaim#     s             25
     d strIshClaim#    s             25
     d strIshBill#     s             25
     d strCaEob#       s             25
     d strCaLin#       s              9
     d isUpdate        s              1
     d isDetail        s              1
     d intPos1         s              2  0
     d intCMss#        s              9  0
     d intPos2         s              2  0
     d @@File          s             10
     d @@Event         s              1
     d @@Co#           s              3  0
     d @@fnd           s              3  0
     d @@EmpOrEob      s              9  0
     d @@DivOrSub      s              5  0
     d @@fyr           s              3  0
     d @@Case          s              7  0
     d @@CCo#          s              3
     d @@Cfnd          s              3
     d @@CEmpOrEob     s              9
     d @@CDivOrSub     s              5
     d @@Cfyr          s              3
     d @@CCase         s              7
     d @@Med           s              1
     d @@prcd          s             11
     d @@Cseq          s              5
     d @@seq           s              5  0
      **** Beg Add *** 08/30/16 ********************************
     d intSeq          s              5  0
     d strTableId      s              4
      **** End Add *** 08/30/16 ********************************
     ***** Beg Add *** 08/19/16 ***********************
     d @@Ofc#          s              5
     d @@flgCMS        s              1
     d strCoKey        s              6    inz
     d strOfckey       s              8    inz
     d strClmkey       s                   like(I3clmkey)
     d strclmtkey      s                   like(I4clmtKey)
     d strICDcode      s                   like(I4icdCode)
     d strRre#         s              9    inz
     d strCoTaxid      s              9    inz
     ***** End Add *** 08/19/16 ***********************

     ***** Beg Add *** 11/28/16 ***********************
     d strCdZip        s              9    inz
     d strFNmzip       s              9    inz
     d strOFzip        s              9    inz
     d strWPDzip       s              9    inz
     d strLLpzip       s              9    inz
     ***** End Add *** 11/28/16 ***********************
      ***** Beg Add *** 02/08/17 *************************************
     d xxMepp          s              9  2 inz
     d xxIndp          s              9  2 inz
     d xxExpp          s              9  2 inz
      ***** End Add *** 02/08/17 *************************************
      ***** Beg Add *** 05/10/17 *************************************
     d  isScorelog     s              1
     d  IsoScoreLg     s              1    dtaara
     dpsds           esds                  extname(szQ1p)
      ***** End Add *** 05/10/17 *************************************
      * File prcessing sub procedures
     d LoadAISP        pr
     d Loadinit        pr
     d ProcessClaim    pr
     d ProcessFile     pr
     d ProcessFMCDP    pr
     d ProcessFMPDP    pr
     d ProcessSMUPP    pr
     d ProcessWDF3P    pr
     d ProcessWMCDP    pr
     d ProcessWMCMP    pr
     d ProcessWMEMP    pr
     d ProcessWMFNP    pr
     d ProcessWMRPP    pr
     d ProcessWMWHP    pr
     d ProcessWTCNP    pr
     d WriteFAISHP     pr
     d WriteFAISDP     pr
      ***** Beg Add *** 08/30/16 ********************************
     d DeleteFAISDP    pr
      ***** End Add *** 08/30/16 ********************************

     ***** Beg Add *** 08/19/16 ***********************
     d loadWAISOCM1    pr
     d loadWAISOCM2    pr
     d loadWAISOCM3    pr
     d loadWAISOCM4    pr
     d loadWAISOCM5    pr
     d ProcessWDF2P    pr
     d ProcessWMOFP    pr
     d ProcessWMLLP    pr
     d ProcessWMPDP    pr
     d ProcessSADAP    pr
     d WrtCMSExtract   pr
     ***** End Add *** 08/19/16 ***********************


     c     *entry        plist
     c                   parm                    @@File
     c                   parm                    @@Event
     c                   parm                    @@CCo#
     c                   parm                    @@Cfnd
     c                   parm                    @@CEmpOrEob
     c                   parm                    @@CDivOrSub
     c                   parm                    @@Cfyr
     c                   parm                    @@CCase
     c                   parm                    @@Med
     c                   parm                    @@prcd
     c                   parm                    @@Cseq
    c***** Beg Add *** 08/19/16 ***********************
     c                   parm                    @@Ofc#
     c                   parm                    @@FlgCMS
    c***** End Add *** 08/19/16 ***********************

      /Free

       // Initilaize
        Loadinit();
        ProcessFile();
        *inlr = *on;
      /End-free
       //===================================================================
       //  Load fields to CLaim extract file from WMCMP
       //===================================================================
     p LoadAISP        b
     d LoadAISP        pi
      /Free
        //***** Beg Add *** 09/08/16 ************************************
        //Initialize amount fields
           ISmepp  =    *zeros;
           ISmeps  =    *zeros;
           ISindp  =    *zeros;
           ISinds  =    *zeros;
           ISexpp  =    *zeros;
           ISexps  =    *zeros;
        //***** End Add *** 09/08/16 ************************************
        //***** Beg Add *** 02/08/17 ************************************
           xxMepp  =    *zeros;
           xxIndp  =    *zeros;
           xxExpp  =    *zeros;
        //***** End Add *** 02/08/17 ************************************
           ISlsn   =    CMlsn;
           ISmid   =    CMmid;
           ISfsn   =    CMfsn;
           ISeid   =    CMeid;
           ISeitq  =    CMeitq;
           ISidth  =    CMidth;
           ISdod   =    %Char(dtMDYY(CMdod));
           IShidt  =    %Char(dtMDYY(CMhidt));
           ISacdt  =    %Char(dtMDYY(CMacdt));
       //***** Beg Chg *** 02/08/17 *************************************
       //**ISactm  =    CMactm;
           ISactm  =    %CHAR(CMactm);
       //***** eND Chg *** 02/08/17 *************************************
           ISast   =    CMast;
           ISaccy  =    CMaccy;
           ISacdr  =    CMacdr;
           ISacty  =    CMacty;
           ISacz1  =    %Char(CMacz1);
           ISpart  =    CMpart;
           IScaus  =    CMcaus;
           ISnatr  =    CMnatr;
           ISclas  =    %char(CMclas);
       //***** Beg Chg *** 02/08/17 *************************************
       //**ISdays  =    CMdays;
       //**ISaww   =    CMaww ;
           ISdays  =    %char(CMdays);
           ISaww   =    %char(CMaww);
       //***** End Chg *** 02/08/17 *************************************
           ISdtrp  =    %Char(dtMDYY(CMdtrp));
           ISrcvd  =    %Char(dtMDYY(CMrcvd));
           IScacn  =    %editc(CMco#:'X') + %editc(CMfnd:'X') +
            %editc(CMfyr:'X') + %editc(CMcase:'X');
           IScaid  =    CMclex;
           If @@event = '2';
              IScsts  =    'V';
           Else;
              IScsts  =    CMstat;
           EndIf;
           If CMrt01 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar01;
              xxMepp = xxMepp + CMar01;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt02 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar02;
              xxMepp = xxMepp + CMar02;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt03 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar03;
              xxMepp = xxMepp + CMar03;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt04 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar04;
              xxMepp = xxMepp + CMar04;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt05 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar05;
              xxMepp = xxMepp + CMar05;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt06 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar06;
              xxMepp = xxMepp + CMar06;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt07 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar07;
              xxMepp = xxMepp + CMar07;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt08 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar08;
              xxMepp = xxMepp + CMar08;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt09 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //*****ISmepp = ISmepp + CMar09;
              xxMepp = xxMepp + CMar09;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt10 = 'ME';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISmepp = ISmepp + CMar10;
              xxMepp = xxMepp + CMar10;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;
       //***** Beg Add *** 02/08/17 *************************************
              ISmepp = %char(xxMepp);
       //***** End Add *** 02/08/17 *************************************

           ISmeps  =    ISmepp;

           If CMrt01 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar01;
              xxIndp = xxIndp + CMar01;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt02 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar02;
              xxIndp = xxIndp + CMar02;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt03 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar03;
              xxIndp = xxIndp + CMar03;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt04 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar04;
              xxIndp = xxIndp + CMar04;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt05 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar05;
              xxIndp = xxIndp + CMar05;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt06 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar06;
              xxIndp = xxIndp + CMar06;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt07 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar07;
              xxIndp = xxIndp + CMar07;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt08 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar08;
              xxIndp = xxIndp + CMar08;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt09 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //*****ISindp = ISindp + CMar09;
              xxIndp = xxIndp + CMar09;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt10 = 'CO';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISindp = ISindp + CMar10;
              xxIndp = xxIndp + CMar10;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;
       //***** Beg Add *** 02/08/17 *************************************
           ISindp = %char(xxIndp);
       //***** End Add *** 02/08/17 *************************************
           ISinds  =    ISindp;

           If CMrt01 = 'EX' or CMrt01 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar01;
              xxExpp = xxExpp + CMar01;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt02 = 'EX' or CMrt02 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar02;
              xxExpp = xxExpp + CMar02;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt03 = 'EX' or CMrt03 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar03;
              xxExpp = xxExpp + CMar03;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt04 = 'EX' or CMrt04 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar04;
              xxExpp = xxExpp + CMar04;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt05 = 'EX' or CMrt05 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar05;
              xxExpp = xxExpp + CMar05;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt06 = 'EX' or CMrt06 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar06;
              xxExpp = xxExpp + CMar06;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt07 = 'EX' or CMrt07 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar07;
              xxExpp = xxExpp + CMar07;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt08 = 'EX' or CMrt08 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //*****ISexpp = ISexpp + CMar08;
              xxExpp = xxExpp + CMar08;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt09 = 'EX' or CMrt09 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar09;
              xxExpp = xxExpp + CMar09;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;

           If CMrt10 = 'EX' or CMrt10 = 'LE';
       //***** Beg Chg *** 02/08/17 *************************************
       //**** ISexpp = ISexpp + CMar10;
              xxExpp = xxExpp + CMar10;
       //***** End Chg *** 02/08/17 *************************************
           EndIf;
       //***** Beg Add *** 02/08/17 *************************************
           ISexpp = %char(xxExpp);
       //***** End Add *** 02/08/17 *************************************
           ISexps  =    ISexpp;
           ISidlw  =    %Char(dtMDYY(CMdtlw));
           IScdlw  =    %Char(dtMDYY(CMrtW));
           ISirwd  =    %Char(dtMDYY(CMcdlw));
           ISrsei  =    CMsame;
           ISiddb  =    %Char(dtMDYY(CMdbdt));
           IScrdb  =    %Char(dtMDYY(CMcddb));

       // Blank fields & No report fields
       //***** Beg Chg *** 02/08/17 *************************************
       //**ISWAGE  =    CMprww;
           ISWAGE  =    %char(CMprww);
       //***** End Chg *** 02/08/17 *************************************
           ISWPCD  =    '0' + CMwgpD;
       //***** Beg Chg *** 08/30/16 *************************************
        // ISCTPC  =    *Blanks;
        // ISTOLC  =    *Blanks;
       //*** Beg Add *** 05/17/17 *************************************
       //*** Set defaults due to no I/M/R records on old/converted claims
           Select;
              When CMltfl =  'I';
                 ISCTPC  = 'L';
              When CMltfl =  'M';
                 ISCTPC  = 'M';
           EndSl;
       //*** End Add *** 05/17/17 *************************************
           If CMltfl =  'R';
              ISCTPC  = 'N';
           Else;
              SetGt (CMco#:CMfnd:CMfyr:CMcase) pth00;
              ReadPE (CMco#:CMfnd:CMfyr:CMcase) pth00;
              If not %Eof(wmthl);
                 If THfrom <> *Blanks and THto = 'I';
                    ISCTPC  = 'L';
                 EndIf;
                 Select;
                 When THfrom = *Blanks and THto = 'R';
                    ISCTPC  = 'N';

                 When THfrom = *Blanks and THto = 'M';
                    ISCTPC  = 'M';

                 When THfrom = *Blanks and THto = 'I';
                    ISCTPC  = 'I';

                 When THfrom = 'R' and THto = 'M';
                    ISCTPC  = 'M';

                 When THfrom = 'R' and THto = 'I';
                    ISCTPC  = 'I';

                 When THfrom = 'M' and THto = 'I';
                    ISCTPC  = 'L';

                 When THfrom = 'M' and THto = 'R';
                    ISCTPC  = 'N';

                 When THfrom = 'L' and THto = 'M';
                    ISCTPC  = 'B';
                 EndSl;

              EndIf;
           EndIf;
           strTableId = %trim(CMjrst) +  'LC';
           Chain (CMco#:strTableId:CMlscv) pgt00;
           If %Found(wtgtl);
              IStolc = %subst(GTcf01:3:4);
           Else;
              Chain (CMco#:'  LC') pgt00;
              IStolc = %subst(GTcf01:3:4);
           EndIf;
       //***** End Chg *** 08/30/16 *************************************
           ISMCOC  =    *Blanks;
       //===================================================================
       // Load WMEMP fields
       //===================================================================

            ISnaic =   EMnaic;
            ISname =   EMname;
       //***** Beg Chg *** 02/08/17 *************************************
       //***ISfein =   EMfein;
            ISfein =   %char(EMfein);
       //***** End Chg *** 02/08/17 *************************************
            ISmst  =   EMmst ;
            ISmcty =   EMmcty;
            ISmad1 =   EMmad1;
            ISmad2 =   EMmad2;
            ISpzip =   %char(EMpzip);
            ISmzip =   %char(EMmzip);
            //*** Begin Add *** 02/08/18 ***********************************
            ISemp# =   %EditC(EMemp#:'X');
            ISrgn# =   %EditC(EMrgn#:'X');
            //*** End   Add *** 02/08/18 ***********************************
       //===================================================================
       // Load SMUPP fields
       //===================================================================

            intPos1 = %Scan(UPusnm:' ');
            IScafn = %Subst(UPusnm:1:intPos1);
            intPos1 = intPos1 + 1 ;
            intPos2 = %Scan(UPusnm:' ':intPos1);
            IScamn = %Subst(UPusnm:intPos1:intPos2);
            intPos2 = intPos2 + 1 ;
            IScaln = %Subst(UPusnm:intPos2);
       //===================================================================
       // Load WTCNP fields
       //===================================================================
            ISecpc  =  CNdesc;
       //===================================================================
       // Load WMWHP fields
       //===================================================================
            If WHwkst = 'R';
               IScrwd =   %char(dtMDYY(WHdate));
               ISrwtc =   'A';
            EndIf;
            If WHwkst = 'E';
               IScrwd =   %char(dtMDYY(WHdate));
               ISrwtc =   'R';
            EndIf;
            If WHrtwq = '02' or WHrtwq = ' 2' or WHrtwq = '2 ' or
              WHrtwq = '06' or WHrtwq = ' 6' or WHrtwq = '6 ';
               ISprin =   'Y';
            EndIf;
            If WHrtwq = '01' or WHrtwq = ' 1' or WHrtwq = '1 ' or
              WHrtwq = '05' or WHrtwq = ' 5' or WHrtwq = '5 ';
               ISprin =   'N';
            EndIf;
       //===================================================================
       // Load WDELP fields
       //===================================================================
            ISpnid =  %char(ELfnd) + %char(Elemp#) + %char(Elfyr);
            //*** Begin Add *** 02/08/18 ***********************************
            ISfYr  = %EditC(ELfYr:'X');
            //*** End   Add *** 02/08/18 ***********************************
       //===================================================================
       // Load WDF3P fields
       //===================================================================
            IScanm = F3name;
            IScapa = F3adr1;
            IScasa = F3adr2;
            ISpcty = F3cty;
            ISpst  = F3st;
            IScapc = %char(F3zip);
       //===================================================================
       // Load WMFNP fields
       //===================================================================
            If F3name = ' ' and F3adr1 = ' ' and F3adr2 = ' ' and
              F3cty  = ' ' and F3st   = ' ' and IScapc = ' '  ;
               IScanm = FNname;
               IScapa = FNpad1;
               IScasa = FNpad2;
               ISpcty = FNpcty;
               ISpst  = FNpst;
               IScapc = %char(FNpzip);
            EndIf;
       //===================================================================
       // Load WMCDP fields
       //===================================================================
       //*** Beg Add *** 11/15/16 *********************************
       // Default Date of Birth if not populated
            If CDdtob = 0;
              CDdtob = 010101;
            Endif;
       //*** End Add *** 11/15/16 *********************************
            ISdtob  =  %Char(dtMDYY(CDdtob));
            ISsex   =  CDsex;
            ISmsts  =  CDmsts;
            ISadr1  =  CDadr1;
            ISadr2  =  CDadr2;
            IScty   =  CDcty ;
            ISst    =  CDst  ;
            ISzip   =  %char(CDzip);
      /End-free
     p LoadAISP        e
       //*********************************************************
       // Initial Load Subprocedure
       //*********************************************************
     p Loadinit        b
     d Loadinit        pi
      /Free
       //Retrieve the key values passed in params
         @@Co#  = %Dec(@@CCo#:3:0);
         @@Fnd  = %Dec(@@Cfnd:3:0);
         @@EmpOrEob = %Dec(@@CEmpOrEob:9:0);
         @@DivOrSub  = %Dec(@@CDivOrSub:5:0);
         @@Fyr  = %Dec(@@Cfyr:3:0);
         @@Case = %Dec(@@CCase:7:0);
         @@seq  = %Dec(@@Cseq:5:0);

      /End-free
     p Loadinit        e
       //===================================================================
       //  Get data records to write in WAISP
       //===================================================================
     p ProcessClaim    b
     d ProcessClaim    pi
      /Free
       // Read from WMEML File for WAISP file
              Chain (CMco#:CMfnd:CMEmp#:CMDiv) pem00;
              If not %found(wmeml);
              EndIf;

       // Read from WTCNP file for WAISP file
              strStateCounty = EMpst + EMcnty;
              Chain (CMco#:strStateCounty) pcn00;
              If not %found(wtabl);
              EndIf;

       // Read from WDELP  File for WAISP file
              Chain (EMco#:EMfnd:EMemp#:EMdiv:CMfyr) pel00;
              If not %found(wdell);
              EndIf;

       // Read from WMCDL file for WAISP file
              Chain (CMss#) pcd00;
              If not %found(wmcdl);
              EndIf;

       // Read from WMWHP  File for WAISP file
              Setll (CMco#:CMfnd:CMfyr:CMcase) pwh00;
              if %equal(wmwhl);
                 Setgt (CMco#:CMfnd:CMfyr:CMcase) pwh00;
                 Readpe (CMco#:CMfnd:CMfyr:CMcase) pwh00;
              Else;
              EndIf;

       // Read from SMUL1  File for WAISP file
              Chain (CMclex) pup01;
              If not %found(smupl1);
              EndIf;

       // Read from WDF3L  File for WAISP file
              Chain (CMco#:CMfnd) pf300;
              If %Found(wdf3l);
              Else;
                 Chain (CMco#:CMfnd) pfn00;
                 If not %Found(wmfnl);
                 EndIf;
              EndIf;

         // Write/Update record to extract work file
              strIsClaim# = %editC(CMco#:'X') + %editC(CMfnd:'X') +
                %editC(CMfyr:'X') + %editC(CMcase:'X');
              Chain (strIsClaim#) pis00;
              If %Found(waisl);
                 LoadAISP();
                 Update pis00;
              Else;
                 LoadAISP();
                 Write pis00;
          //*** Beg Add *** 05/10/17 **********************************
          //* Create a log if data area is set to allow
                 in *lock *dtaara;
                 isScoreLog   = IsoScoreLg;
                 Unlock IsoScoreLg;
                 if isScoreLog = 'Y';
                    LGfile    = @@File;
                    LGevent   = @@Event;
                    LGco#     = @@CCo#;
                    LGfnd     = @@Cfnd;
                    LGempeob  = @@CEmpOrEob;
                    LGdivSub  = @@CDivOrSub;
                    LGfyr     = @@Cfyr;
                    LGcase    = @@CCase;
                    LGdt      = dateTo7(systemDate);
                    LGtime    = timeto6(systemTime);
                    LGuser    = Q1user;
                    Write pislg01;
                 endif;
          //*** End Add *** 05/10/17 **********************************
              EndIf;
      /End-free
     p ProcessClaim    e
       //*********************************************************
       // Process File Trigger
       //*********************************************************
     p ProcessFile     b
     d ProcessFile     pi
      /Free
       Select;
          When @@File = 'FAEOP';
             WriteFAISHP();

          When @@File = 'FMCDP';
             ProcessFMCDP();

          When @@File = 'FMPDP';
             ProcessFMPDP();

          When @@File = 'FDDHP';
             WriteFAISHP();

          When @@File = 'FAEUP';
             WriteFAISHP();

          When @@File = 'FDDPP';
             WriteFAISHP();

          When @@File = 'FAEDP';
             WriteFAISHP();

          When @@File = 'FAEHP';
             WriteFAISHP();

          When @@File = 'FDDUP';
             WriteFAISHP();

          When @@File = 'FDDDP';
             WriteFAISHP();

          When @@File = 'FDCAP';
       //***** Beg Add *** 08/30/16 ********************************
             intSeq  = @@Seq;
             WriteFAISHP();
             If @@Event = '2';
                DeleteFAISDP();
             EndIf;
       //***** End Add *** 08/30/16 ********************************

          When @@File = 'FDDEP';
             WriteFAISHP();

          When @@File = 'WMCMP' ;
             ProcessWMCMP();

          When @@File = 'SMUPP';
             ProcessSMUPP();

          When @@File = 'WMRPP';
             ProcessWMRPP();

          When @@File = 'WMEMP';
             ProcessWMEMP();

          When @@File = 'WMWHP';
             ProcessWMWHP();

          When @@File = 'WMCDP';
             ProcessWMCDP();

          When @@File = 'WDF3P';
             ProcessWDF3P();

          When @@File = 'WMFNP';
             ProcessWMFNP();

          When @@File = 'WTCNP';
             ProcessWTCNP();

         // ***** Beg Add *** 08/19/16 **********************
          When @@File = 'WDF2P';
            ProcessWDF2P();

          When @@File = 'WMOFP';
            ProcessWMOFP();

          When @@File = 'WMLLP';
            ProcessWMLLP();

          When @@File = 'WMPDP';
            ProcessWMPDP();

          When @@File = 'SADAP';
            ProcessSADAP();

         // ***** End Add *** 08/19/16 **********************

        EndSl;
      /End-free
     p ProcessFile     e
      //***************************************************************
      // Process FMCDP changes
      //***************************************************************
     p ProcessFMCDP    b
     d ProcessFMCDP    pi
      /Free
         Chain (@@Co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) peo37;
         If %found(faeol37);
            Setll (@@Co#:EOss#) peo0j;
            Reade (@@Co#:EOss#) peo0j;
            Dow not %Eof(faeolj);
               @@DivOrSub = EOsub#;
               @@fnd      = EOfnd;
               @@fyr      = EOfyr;
               @@case     = EOcase;
               @@EmpOrEob = EOeob#;
               WriteFAISHP();
               Reade (@@Co#:EOss#) peo0j;
            EndDo;
         EndIf;
      /End-free
     p ProcessFMCDP    e
      //***************************************************************
      // Process FMPDP changes
      //***************************************************************
     p ProcessFMPDP    b
     d ProcessFMPDP    pi
      /Free
         Chain (@@Co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) peo37;
         If %found(faeol37);
            Setll (@@Co#:EOFein) peo0u;
            Reade (@@Co#:EOFein) peo0u;
            Dow not %Eof(faeolu);
               @@DivOrSub = EOsub#;
               @@fnd      = EOfnd;
               @@fyr      = EOfyr;
               @@case     = EOcase;
               @@EmpOrEob = EOeob#;
               WriteFAISHP();
               Reade (@@Co#:EOFein) peo0u;
            EndDo;
         EndIf;
      /End-free
     p ProcessFMPDP    e
       //===================================================================
       // Process all User Parameter File changes
       //===================================================================
     p ProcessSMUPP    b
     d ProcessSMUPP    pi
      /Free
       // Read from SMUPP staff code the file
          Chain (@@co#:@@fnd:@@EmpOrEob:@@DivOrSub:@@Fyr:@@Case) pcm00;
          If %found(wmcml);
             Chain (CMclex) pup01;
             If %found(smupl1);
                Setll (UPco#:UPstaf) pcm04;
                Reade (UPco#:UPstaf) pcm04;
                Dow not %eof(wmcml4);
                   If TestGrp(CMCo#:CMFnd) = *off and
             //*** Beg Chg ***** 12/01/16 *******************************
              //***ExpiredGrp(CMCo#:CMFnd) = *Off;
                    ExpiredGrp(CMCo#:CMFnd) = *Off and CMfnd >= 300;
             //*** End Chg ***** 12/01/16 *******************************
                      ProcessClaim();
                     //***** Beg Add *** 08/19/16 ***********************
                     // Write/Update CMS extract work files
                      If @@flgCMS = 'Y';

                        // Prepare keys to read CMS extract files
                         strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');

                         If CMoff# <> *Blanks;
                            strOfckey  = %Editc(CMco#:'X') +
                             %Editc(%Dec(%Trim(CMoff#):5:0):'X');
                         EndIf;

                         strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                          %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

                         //***** Beg Chg *** 11/04/16 ***********************
                         //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                         // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

                         strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                          %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
                         //***** End Chg *** 11/04/16 ***********************

                         WrtCMSExtract();
                      EndIf;
                     //***** End Add *** 08/19/16 ***********************
                   EndIf;
                   Reade (UPco#:UPstaf) pcm04;
                EndDo;
             EndIf;
          EndIf;

      /End-Free
     p ProcessSMUPP    e
       //===================================================================
       // Process WDF3P file for changes
       //===================================================================
     p ProcessWDF3P    b
     d ProcessWDF3P    pi
      /Free
         Chain (@@Co#:@@fnd) pf300;
         If %Found(wdf3l);
            Setll (F3co#:F3fnd) pcm00;
            Reade (F3co#:F3fnd) pcm00;
            Dow not %Eof (wmcml);
               ProcessClaim();
               Reade (F3co#:F3fnd) pcm00;
            EndDo;
         EndIf;
      /End-free
     p ProcessWDF3P    e
       //===================================================================
       // Process WMCDP file for changes
       //===================================================================
     p ProcessWMCDP    b
     d ProcessWMCDP    pi
      /Free
         Chain (@@Co#:@@fnd:@@EmpOreob:@@DivOrSub:@@Fyr:@@Case) pcm00;
         If %Found(wmcml);
            Chain (CMss#) pcd00;
            If %Found(wmcdl);
               Setll (CMss#) pcm0A;
               Reade (CMss#) pcm0A;
               Dow not %Eof (wmcmla);
                  If TestGrp(CMCo#:CMFnd) = *off and
             //*** Beg Chg ***** 12/01/16 *******************************
              //***ExpiredGrp(CMCo#:CMFnd) = *Off;
                   ExpiredGrp(CMCo#:CMFnd) = *Off and CMfnd >= 300;
             //*** End Chg ***** 12/01/16 *******************************
                     ProcessClaim();
                     //Write record to medical extract work file
                     If @@Med = 'M';
                        Setll (CMco#:CMss#:CMacdt) peo0j;
                        Reade (CMco#:CMss#:CMacdt) peo0j;
                        Dow not %Eof(faeolj);
                           @@EmpOrEob = EOeob#;
                           @@DivOrSub = EOsub#;
                           WriteFAISHP();
                           Reade (CMco#:CMss#:CMacdt) peo0j;
                        EndDo;
                     EndIf;
                    // ***** Beg Add *** 08/19/16 *******************************
                    // Write/Update CMS extract work files
                     If @@flgCMS = 'Y';

                       // Prepare keys to read CMS extract files
                        strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
                        If CMoff# <> *Blanks;
                           strOfckey  = %Editc(CMco#:'X') +
                            %Editc(%Dec(%Trim(CMoff#):5:0):'X');
                        EndIf;
                        strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                         %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

                         //***** Beg Chg *** 11/04/16 ***********************
                         //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                         // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

                         strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                          %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
                         //***** End Chg *** 11/04/16 ***********************

                        WrtCMSExtract();
                     EndIf;
                    // ***** End Add *** 08/19/16 *******************************
                  EndIf;
                  Reade (CMss#) pcm0A;
               EndDo;
            EndIf;

         EndIf;
      /End-free
     p ProcessWMCDP    e
       //===================================================================
       // Process all Employee master File NAICS code, Employer Name, FIEN,
       // Mailing State Code, Mailing City, Postal Code (Physical/Mailing)
       //===================================================================
     p ProcessWMCMP    b
     d ProcessWMCMP    pi
      /Free
       // Read from WMCMP file for WAISP file
          Setll (@@co#:@@fnd:@@EmpOrEob:@@DivOrSub:@@Fyr:@@Case) pcm00;
          Reade (@@co#:@@fnd:@@EmpOrEob:@@DivOrSub:@@Fyr:@@Case) pcm00;
          Dow not %eof(wmcml);
              ProcessClaim();

              //Write record to medical extract work file
              If @@Med = 'M';
                 Setll (CMco#:CMss#:CMacdt) peo0j;
                 Reade (CMco#:CMss#:CMacdt) peo0j;
                 Dow not %Eof(faeolj);
                    @@EmpOrEob = EOeob#;
                    @@DivOrSub  = EOsub#;
                    WriteFAISHP();
                    Reade (CMco#:CMss#:CMacdt) peo0j;
                 EndDo;
              EndIf;

             // ***** Beg Add *** 08/19/16 ********************************
             // Write/Update CMS extract work files
              If @@flgCMS = 'Y';

                // Prepare keys to read CMS extract files
                 strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
                 If CMoff# <> *Blanks;
                    strOfckey  = %Editc(CMco#:'X') +
                     %Editc(%Dec(%Trim(CMoff#):5:0):'X');
                 EndIf;
                 strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                  %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

                 //***** Beg Chg *** 11/04/16 ***********************
                 //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                 // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

                 strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                  %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
                 //***** End Chg *** 11/04/16 ***********************

                 WrtCMSExtract();
              EndIf;
             // ***** End Add *** 08/19/16 ********************************
              Reade (@@co#:@@fnd:@@EmpOrEob:@@DivOrSub:@@Fyr:@@Case) pcm00;
          EndDo;
      /End-Free
     p ProcessWMCMP    e
       //===================================================================
       // Process WMEMP file for changes
       //===================================================================
     p ProcessWMEMP    b
     d ProcessWMEMP    pi
      /Free
          Chain (@@Co#:@@Fnd:@@EmpOrEob:@@DivOrSub) pem00;
          If %Found(wmeml);
             Setll (@@Co#:@@Fnd:@@EmpOrEob:@@DivOrSub) pcm00;
             Reade (@@Co#:@@Fnd:@@EmpOrEob:@@DivOrSub) pcm00;
             Dow not %Eof(wmcml);
               // Write/Update record to extract work file
                ProcessClaim();

               //***** Beg Add *** 08/19/16 ***********************
               // Write/Update CMS extract work files
                If @@flgCMS = 'Y';

                  // Prepare keys to read CMS extract files
                   strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
                   If CMoff# <> *Blanks;
                      strOfckey  = %Editc(CMco#:'X') +
                       %Editc(%Dec(%Trim(CMoff#):5:0):'X');
                   EndIf;
                   strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                    %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

                   //***** Beg Chg *** 11/04/16 ***********************
                   //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                   // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

                   strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                    %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
                   //***** End Chg *** 11/04/16 ***********************

                   WrtCMSExtract();
                EndIf;
               //***** End Add *** 08/19/16 ***********************
                Reade (@@Co#:@@Fnd:@@EmpOrEob:@@DivOrSub) pcm00;
             EndDo;
          Endif;
      /End-Free
     p ProcessWMEMP    e
       //===================================================================
       // Process WMFNP file for changes
       //===================================================================
     p ProcessWMFNP    b
     d ProcessWMFNP    pi
      /Free
         Chain (@@Co#:@@fnd) pfn00;
         If %Found(wmfnl);
            Setll (FNCo#:FNfnd) pcm00;
            Reade (FNCo#:FNfnd) pcm00;
            Dow not %Eof (wmcml);
               ProcessClaim();

              // ***** Beg Add *** 08/19/16 *******************************
              // Write/Update CMS extract work files
               If @@flgCMS = 'Y';

                 // Prepare keys to read CMS extract files
                  strCoKey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
                  If CMoff# <> *Blanks;
                     strOfckey  = %Editc(CMco#:'X') +
                      %Editc(%Dec(%Trim(CMoff#):5:0):'X');
                  EndIf;
                  strclmkey = %Editc(CMco#:'X') + %Editc(@@fnd:'X') +
                   %Editc(CMfyr:'X') + %Editc(CMcase:'X') + 'W0070R' ;

                  //***** Beg Chg *** 11/04/16 ***********************
                  //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                  // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

                  strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                   %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
                  //***** End Chg *** 11/04/16 ***********************

                  WrtCMSExtract();
               EndIf;
              // ***** End Add *** 08/19/16 *******************************

               Reade (FNCo#:FNfnd) pcm00;
            EndDo;
         EndIf;
      /End-free
     p ProcessWMFNP    e
      //***************************************************************
      // Process WMRPP changes
      //***************************************************************
     p ProcessWMRPP    b
     d ProcessWMRPP    pi
      /Free
         Chain (@@Co#:@@Fnd:@@EmpOrEob:@@DivOrSub:@@Fyr) pcm00;
         If %found(wmcml);
            Setll (CMco#:CMss#:CMfyr:CMcase) pcmfc;
            Reade (CMco#:CMss#:CMfyr:CMcase) pcmfc;
            Dow not %Eof(fmcmlc);
               Setll (FCMco#:FCMsub#:FCMfnd:FCMfyr:FCMcase) peo00;
               Reade (FCMco#:FCMsub#:FCMfnd:FCMfyr:FCMcase) peo00;
               Dow not %Eof(faeol);
                  @@DivOrSub = EOsub#;
                  @@fnd      = EOfnd;
                  @@fyr      = EOfyr;
                  @@case     = EOcase;
                  @@EmpOrEob = EOeob#;
                  WriteFAISHP();
                  Reade (FCMco#:FCMsub#:FCMfnd:FCMfyr:FCMcase) peo00;
               EndDo;
               Reade (CMco#:CMss#:CMfyr:CMcase) pcmfc;
            EndDo;
         EndIf;
      /End-free
     p ProcessWMRPP    e
       //===================================================================
       // Process WMWHP file for changes
       //===================================================================
     p ProcessWMWHP    b
     d ProcessWMWHP    pi
      /Free
          Setll (@@Co#:@@Fnd:@@Fyr:@@Case) pwh00;
          If %equal(wmwhl);
             Setgt (@@Co#:@@Fnd:@@Fyr:@@Case) pwh00;
             Readpe (@@Co#:@@Fnd:@@Fyr:@@Case) pwh00;

             Chain (@@Co#:@@Fnd:@@Fyr:@@Case) pcm01;
             If %found (wmcml1);
                ProcessClaim();
             EndIf;
          EndIf;
      /End-Free
     p ProcessWMWHP    e
       //===================================================================
       // Process WTCNP file for changes
       //===================================================================
     p ProcessWTCNP    b
     d ProcessWTCNP    pi
      /Free
          Chain (@@Co#:@@Fnd:@@EmpOrEob:@@DivOrSub) pem00;
          If %Found(wmeml);
             strStateCounty = EMpst + EMcnty;
             Chain (@@Co#:strStateCounty) pcn00;
             If %Found(wtabl);
                Setll (@@Co#:EMpst:EMcnty) pem22;
                Reade (@@Co#:EMpst:EMcnty) pem22;
                Dow not %Eof (wmeml22);
                   If TestGrp(EMCo#:EMFnd) = *off and
          //*** Beg Chg ***** 12/01/16 *******************************
              //***ExpiredGrp(EMCo#:EMFnd) = *Off;
                   ExpiredGrp(EMCo#:EMFnd) = *Off and EMfnd >= 300;
          //*** End Chg ***** 12/01/16 *******************************
                      Setll (EMco#:EMfnd:EMemp#:EMdiv) pcm00;
                      Reade (EMco#:EMfnd:EMemp#:EMdiv) pcm00;
                      Dow not %Eof(wmcml);
                         ProcessClaim();
                         Reade (EMco#:EMfnd:EMemp#:EMdiv) pcm00;
                      EndDo;
                   EndIf;
                   Reade (@@Co#:EMpst:EMcnty) pem22;
                EndDo;
             EndIf;
          EndIf;
      /End-free
     p ProcessWTCNP    e
       //===================================================================
       // Write FAISDP Detail Extract file
       //===================================================================
     p WriteFAISDP     b
     d WriteFAISDP     pi
      /Free

       // Read FAEOP and then all detail records from FDCAP
        Chain (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) peo37;
       // *** Beg Chg *** 05/18/17 *********************************
        //If %found(faeol37);

        //*** Beg Chg ***** 06/21/17 *******************************
        //If %found(faeol37) and EOblst ='C';
        If %found(faeol37) and EOblst ='C' and
          ExpiredGrp(EOCo#:EOFnd) = *Off and EOfnd >= 300
          and TestGrp(EOCo#:EOFnd) = *Off;
        //*** End Chg ***** 06/21/17 *******************************
       // *** End Chg *** 05/18/17 *********************************
           Setll (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) pca18;
           Reade (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) pca18;
           Dow not %Eof (fdcal18);
              strCaEob# = %editC(CAeob#:'X');
              strCaLin# = %editC(CAlin#:'X');
              Chain (strCaEob#:strCaLin#) pisd00;
              If %found(faisdl);
                 isDetail = 'Y';
              Else;
                 isDetail = 'N';
              EndIf;
              @@seq = CAdseq;
              Select;
                 When EObtyp = 'N';
                    Chain (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:
                     @@seq) pdh00;
                    If %Found(fddhl);
                       ISdeob#  = %editc(DHeob#:'X');
                       ISdprcd  = DHprcd;
       //*** Beg Add *** 10/11/16 *****************************************
                       Select;

                         When DHprcd >= '00100';
                           ISdPrcdTy  = 'CPT';

                         When DHprcd < '00100';
                           ISdPrcdTy  = 'HCPCS';

                       Endsl;
       //*** End Add *** 10/11/16 *****************************************
                       ISdmod1  = DHmod1;
                       ISdmod2  = DHmod2;
       //*** Beg Chg *** 08/18/16 *****************************************
       //*********     ISddref1 = DHdxr1;
       //*********     ISddref2 = DHdxr2;
       //*********     ISddref3 = DHdxr3;
       //*********     ISddref4 = DHdxr4;
       // Set Diagnosis Cross Reference Fields
                       ISddref1 = ' ';
                       ISddref2 = ' ';
                       ISddref3 = ' ';
                       ISddref4 = ' ';
                       If DHdxr1 <> ' ';
                         Isddref1 = 'A';
                         If DHdxr2 <> ' ';
                           Isddref2 = 'B';
                           If DHdxr3 <> ' ';
                             Isddref3 = 'C';
                             If DHdxr4 <> ' ';
                               Isddref4 = 'D';
                             Endif;
                           Else;
                             If DHdxr4 <> ' ';
                               Isddref3 = 'D';
                             Endif;
                           Endif;
                         Else;
                           If DHdxr3 <> ' ';
                             Isddref2 = 'C';
                             If DHdxr4 <> ' ';
                               Isddref3 = 'D';
                             Endif;
                           Else;
                             If DHdxr4 <> ' ';
                               Isddref2 = 'D';
                             Endif;
                           Endif;
                         Endif;
                       Else;
                         If DHdxr2 <> ' ';
                           Isddref1 = 'B';
                           If DHdxr3 <> ' ';
                             Isddref2 = 'C';
                             If DHdxr4 <> ' ';
                               Isddref3 = 'D';
                             Endif;
                           Else;
                             If DHdxr4 <> ' ';
                               Isddref2 = 'D';
                             Endif;
                           Endif;
                         Else;
                           If DHdxr3 <> ' ';
                             Isddref1 = 'C';
                             If DHdxr4 <> ' ';
                               Isddref2 = 'D';
                             Endif;
                           Else;
                             If DHdxr4 <> ' ';
                               Isddref1 = 'D';
                             Endif;
                           Endif;
                         Endif;
                       Endif;
       //*** End Chg *** 08/18/16 *****************************************
                    EndIf;

                 When EObtyp = 'H';
                    Chain (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:
                     @@seq) pdu00;
                    If %found(fddul);
                       ISdeob#  = %Editc(DUeob#:'X');
                       ISdprcd  = DUrate;
       //*** Beg Add *** 10/11/16 *****************************************
                       If DUrate <> *blanks;
                         Select;
                           When DUrate >= '00100';
                             ISdPrcdTy = 'CPT';
                           When DUrate < '00100';
                             IsdPrcdTy = 'HCPCS';
                         Endsl;
                       Else;
                         ISdPrCd = %char(DUrvCd);
                         ISdPrcdTy  = 'REV';
                       Endif;
       //*** End Add *** 10/11/16 *****************************************
                       ISdmod1  = DUpmod;
                       ISdmod2  = DUmod2;
                    endif;

                 When EObtyp = 'X';
                    Chain (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:
                     @@seq) pdp00;
                    If %found(fddpl);
                       ISdeob#  = %editc(DPeob#:'X');
                      // *** Beg Chg *** 05/18/17 *****************
                       If DPndc <> *Blanks;
                          ISdprcd  = DPndc ;
                          //*** Beg Add *** 10/11/16 ***************
                          ISdPrcdTy  = 'OTHER';
                          //*** End Add *** 10/11/16 ***************
                       Else;
                          ISdprcd   = DPhcpc;
                          ISdPrcdTy = 'HCPCS';
                       EndIf;
                      // *** End Chg *** 05/18/17 *****************
                    Endif;

                 When EObtyp = 'D';
                    Chain (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:
                     @@seq) pdd00;
                    If %found(fdddl);
                       ISdeob#  = %editc(DDeob#:'X');
                       ISdprcd  = DDprcd;
       //*** Beg Add *** 10/11/16 *****************************************
                       ISdPrcdTy  = 'OTHER';
       //*** End Add *** 10/11/16 *****************************************
       //*** Beg Chg *** 08/18/16 *****************************************
       //**********    ISddref1 = DDdxr1;
       //**********    ISddref2 = DDdxr2;
       //**********    ISddref3 = DDdxr3;
       //**********    ISddref4 = DDdxr4;
       // Set Diagnosis Cross Reference Fields
                       ISddref1 = ' ';
                       ISddref2 = ' ';
                       ISddref3 = ' ';
                       ISddref4 = ' ';
                       If DDdxr1 <> ' ';
                         Isddref1 = '1';
                         If DDdxr2 <> ' ';
                           Isddref2 = '2';
                           If DDdxr3 <> ' ';
                             Isddref3 = '3';
                             If DDdxr4 <> ' ';
                               Isddref4 = '4';
                             Endif;
                           Else;
                             If DDdxr4 <> ' ';
                               Isddref3 = '4';
                             Endif;
                           Endif;
                         Else;
                           If DDdxr3 <> ' ';
                             Isddref2 = '3';
                             If DDdxr4 <> ' ';
                               Isddref3 = '4';
                             Endif;
                           Else;
                             If DDdxr4 <> ' ';
                               Isddref2 = '4';
                             Endif;
                           Endif;
                         Endif;
                       Else;
                         If DDdxr2 <> ' ';
                           Isddref1 = '2';
                           If DDdxr3 <> ' ';
                             Isddref2 = '3';
                             If DDdxr4 <> ' ';
                               Isddref3 = '4';
                             Endif;
                           Else;
                             If DDdxr4 <> ' ';
                               Isddref2 = '4';
                             Endif;
                           Endif;
                         Else;
                           If DDdxr3 <> ' ';
                             Isddref1 = '3';
                             If DDdxr4 <> ' ';
                               Isddref2 = '4';
                             Endif;
                           Else;
                             If DDdxr4 <> ' ';
                               Isddref1 = '4';
                             Endif;
                           Endif;
                         Endif;
                       Endif;
       //*** End Chg *** 08/18/16 *****************************************
                    Endif;
              Endsl;

              ISdblin  = %Editc(CAlin#:'X');
              ISdorbill= ISdblin;
              ISddos   =  %Char(dtMDYY(CAfdat));
              ISddref5 = *Blanks;
              ISddref6 = *Blanks;
              ISddref7 = *Blanks;
              ISddref8 = *Blanks;
              ISddref9 = *Blanks;
              ISddref10= *Blanks;
              ISddref11= *Blanks;
              ISddref12= *Blanks;
              ISddref13= *Blanks;
              ISddref14= *Blanks;
              ISddref15= *Blanks;
              ISddref16= *Blanks;
       //*** Beg Add *** 11/15/16 *****************************************
              If CAunts = 0;
                CAunts = 1;
              Endif;
       //*** End Add *** 11/15/16 *****************************************
              ISdunit = %Editc(CAunts:'X');
       //*** Beg Add *** 10/17/16 *****************************************
       //*** Beg Add *** 10/17/16 *****************************************
              If ISdunit = '00000';
                ISdUnit  = '00001';
              Endif;
       //*** End Add *** 10/17/16 *****************************************
              ISducr  = *Zeros;
              ISdvr   = *Zeros;
              ISdbill = %editc(CAchrg:'X');
              ISdpaid = %editc(CApaid:'X');
       //*** Beg Add *** 11/15/16 *****************************************
       //*** Beg Chg *** 11/29/16 *****************************************
         //***ISdbill = %Subst(ISdbill:1:7) + '.' + %Subst(ISdbill:8:2);
         //***ISdpaid = %Subst(ISdpaid:1:7) + '.' + %Subst(ISdpaid:8:2);
              ISdbill = %Subst(ISdbill:2:6) + '.' + %Subst(ISdbill:8:2);
              ISdpaid = %Subst(ISdpaid:2:6) + '.' + %Subst(ISdpaid:8:2);
       //*** End Chg *** 11/29/16 *****************************************
       // If charges are negative the format as '-nnnnn.nn'
              If CAchrg < 0;
                CAchrg = %Abs(CAchrg);
       //*** Beg Chg *** 11/29/16 *****************************************
          //*** ISdbill = '-' + %Subst(%editc(CAchrg:'X'):2:6);
          //*** ISdbill = %Subst(ISdbill:1:7) + '.' + %Subst(ISdbill:8:2);
                ISdbill = '-' + %Subst(%editc(CAchrg:'X'):3:5) + '.' +
                          %Subst(%editc(CAchrg:'X'):8:2);
       //*** End Chg *** 11/29/16 *****************************************
              Endif;
       // If payment is negative the format as '-nnnnn.nn'
              If CApaid < 0;
                CApaid = %Abs(CApaid);
       //*** Beg Chg *** 11/29/16 *****************************************
          //*** ISdpaid = '-' + %Subst(%editc(CApaid:'X'):2:6);
          //*** ISdpaid = %Subst(ISdpaid:1:7) + '.' + %Subst(ISdpaid:8:2);
                ISdpaid = '-' + %Subst(%editc(CApaid:'X'):3:5) + '.' +
                          %Subst(%editc(CApaid:'X'):8:2);
       //*** End Chg *** 11/29/16 *****************************************
              Endif;
       //*** End Add *** 11/15/16 *****************************************

              Chain (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:
               @@seq) pde00;
              If %found(fddel);
                 ISreascd= DEeob;
                 ISreastx= DEnot1;
              Endif;
              If isDetail = 'Y';
                 Update pisd00;
              Else;
                 Write pisd00;
              EndIf;
              Reade (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) pca18;
           EndDo;
        EndIf;
      /End-free
     p WriteFAISDP     e
       //===================================================================
       // Write FAISHP Header Extract File
       //===================================================================
     p WriteFAISHP     b
     d WriteFAISHP     pi
      /Free

       // Read FAEOP
        Chain (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) peo37;

       // *** Beg Chg *** 05/18/17 *********************************
        //If %found(faeol37);

        If %found(faeol37) and EOblst ='C';
       // *** End Chg *** 05/18/17 *********************************

          //*** Beg Add *** 08/29/16 ********************************
          // If this is a standalone group then do not process
          open FMF1L;
          Chain (@@co#:@@DivOrSub:@@fnd) pf101;
          if %found(FMF1L) and F1outa = 'Y';
            Close FMF1L;
            *inlr = *on;
            Return;
          Endif;
          Close FMF1L;
          //*** End Add *** 08/29/16 ********************************
          //Check for update or write flag
           strIshClaim# = %editc(EOco#:'X') + %editc(EOfnd:'X') +
                       %editc(EOfyr:'X') + %editc(EOcase:'X');
           strIshBill# = %editC(EOeob#:'X');
           Chain (strIshClaim#:strIshBill#) pish00;
           If %found(faishl);
              isUpdate = 'Y';
           Else;
              isUpdate = 'N';
           EndIf;
       // Check FAEAL1 for skip the EOB
        //*** Beg Del *** 08/25/16 ***************************************
        //*Setll (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:EOrcn) pea01;
        //*Reade (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:EOrcn) pea01;
        //*** End Del *** 08/25/16 ***************************************
        //*** Beg Chg *** 08/18/16 ***************************************
        //****   Dow not %Eof (faeal1) and ( EApaid > 0 and EAwcpd > 0  or
        //****    EApaid <= 0  and EAEPDT > 0 );
        //*** Beg Del *** 08/25/16 ***************************************
        //*** Dow not %Eof (faeal1);
        //*** End Del *** 08/25/16 ***************************************
        //*** End Chg *** 08/18/16 ***************************************
             // Bill Number
              IShbill#  = %editc(EOeob#:'X');

             // Tax ID number
              IShtaxid  = %editc(EOfein:'X');

             // Date Received
              IShrcdt   = %Char(dtMDYY(EOcrdt));

             // Claim Number
              IShclm#   = %editc(EOco#:'X') + %editc(EOfnd:'X') +
               %editc(EOfyr:'X') +  %editc(EOcase:'X');

          // Policy Number
              Chain (EOco#:EOfnd:EOfyr:EOcase) pcm01;
              If %found(wmcml1);
                 Chain (EOco#:EOfnd:CMemp#:EOfyr:CMdiv) prp00;
           //*** Beg Chg *** 08/25/16 *****************************
           //*** If %found(wmrpl);
                 If %found(wmrpl) and RPcap# <> ' ';
           //*** End Chg *** 08/25/16 *****************************
                    IShpol = RPcap#;
                 Else;
                    IShpol = %editc(EOfnd:'X') + %editc(CMemp#:'X') +
                            %editc(EOfyr:'X');
                 Endif;
              Endif;

          // Patient First Name
              IShfsn    = EOfsn;

          // Patient Middle Name
              IShmid    = EOmid;

          // Patient Last Name
              IShlsn    = EOlsn;

          // Type of ID
              IShidty   = 'SSN';

          // Patient Social Security Number
              IShssn    = EOss#;

          // Gender, Date of Birth, Patient Address Type, StreetLine 1&2
          // City, State, Zip Code
              Chain (EOss#) pcdfl;
              If %found(fmcdl);
                 IShgen  = fmCDsex;
       //*** Beg Add *** 11/15/16 *********************************
        // Default Date of Birth if not populated
                 If fmCDdtob = 0;
                   fmCDdtob = 010101;
                 Endif;
       //*** End Add *** 11/15/16 *********************************
                 IShdob  = %Char(dtMDYY(fmCDdtob));
                 IShadty = 'M'    ;
                 IShadr1 = fmCDadr1 ;
                 IShadr2 = fmCDadr2 ;
                 IShcty  = fmCDcty  ;
                 IShst   = fmCDst   ;
                 IShzip  = %editc(fmCDzip:'X');
              Endif;

       // Date of Loss
              IShdol    = %Char(dtMDYY(EOacdt));

          // Provider First, Middle, Last and Practice Name
              Chain (EOfein) ppd00;
              If %found(fmpdl);
                 IShpfsn = PDfsn;
                 IShpmid = *Blanks;

                 If PDfsn <> *Blanks;
                    IShplsn = PDname;
                 Else;
                    IShplsn = *Blanks;
                 Endif;

                 If PDfsn  = *Blanks;
                    IShprac = PDname;
                 Else;
                    IShprac = *Blanks;
                 Endif;
              Endif;

       // Provider NPI#
              Select;
                 When EObtyp = 'N';
                    Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pdh00;
                    If %found(fddhl);
                      IShnpi  = DHnpir;
                    Endif;

                 When EObtyp = 'H';
                    Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peu00;
                    If %found(faeul);
                       IShnpi  = EUanpi;
                    Endif;

                 When EObtyp = 'X';
                    Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pdp00;
                    If %found(fddpl);
                       IShnpi  = DPrnpi;
                    Endif;

                 When EObtyp = 'D';
                    Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) ped00;
                    If %found(faedl);
                       IShnpi  = EDtnpi;
                    Endif;
              Endsl;

          // Billing Provider Specialty code
              IShspcd    = EOspcd;

          // Network Service Code
           //*** Beg Chg *** 10/17/16 *****************************************
              //IShnets    = EOntwk;
              IShnets    = 'UNK';
           //*** End Chg *** 10/17/16 *****************************************

          // Billing Provider Taxonomy Code
              IShtaxo    = PDtaxo;

          // Provider Address Type
              IShpadty   = 'M';

       // Provider StreetLine1
              IShpadr1   = PDadr1;

       // Provider StreetLine2
              IShpadr2   = PDadr2;

       // Provider City
              IShpcty    = PDcty;

       // Provider State
              IShpst     = PDst;

       // Provider Zip Code
              IShpzip    = %editc(PDzip:'X');

          // Zip of Service
              IShzips    =  EOflic;

          // Total Amount Billed
              IShtotbill =  %editc(EOchrg:'X');

          // Total Amount Paid
              IShtotpaid = %editc(EOpaid:'X');
       //*** Beg Add *** 11/15/16 *****************************************
       //*** Beg Chg *** 11/29/16 *****************************************
        //*** IShtotbill =  %Subst(IShtotbill:1:9) + '.' +
        //***               %Subst(IShtotbill:10:2);
        //*** IShtotpaid =  %Subst(IShtotpaid:1:9) + '.' +
        //***               %Subst(IShtotpaid:10:2);
              IShtotbill =  %Subst(IShtotbill:2:8) + '.' +
                            %Subst(IShtotbill:10:2);
              IShtotpaid =  %Subst(IShtotpaid:2:8) + '.' +
                            %Subst(IShtotpaid:10:2);
       //*** End Chg *** 11/29/16 *****************************************
       // If billed amount is negative the format as '-nnnnnnn.nn)'
              If EOchrg < 0;
                EOchrg = %Abs(EOchrg);
       //*** Beg Chg *** 11/29/16 *****************************************
          //*** IShTotbill = '-' + %Subst(%editc(EOchrg:'X'):2:8);
          //*** IShTotbill = %Subst(IShTotbill:1:9) + '.' +
          //***              %Subst(IShTotbill:10:2);
                IShTotbill = '-' + %Subst(%editc(EOchrg:'X'):3:7) + '.' +
                             %Subst(%editc(EOchrg:'X'):10:2);
       //*** End Chg *** 11/29/16 *****************************************
              Endif;
       // If payment is negative the format as '-nnnnnnn.nn'
              If EOpaid < 0;
                EOpaid = %Abs(EOpaid);
       //*** Beg Chg *** 11/29/16 *****************************************
          //*** IShTotPaid = '-' + %Subst(%editc(EOpaid:'X'):2:8);
          //*** IShTotPaid = %Subst(IShTotpaid:1:9) + '.' +
          //***              %Subst(IShTotpaid:10:2);
                IShTotPaid = '-' + %Subst(%editc(EOpaid:'X'):3:7) + '.' +
                             %Subst(%editc(EOpaid:'X'):10:2);
       //*** End Chg *** 11/29/16 *****************************************
              Endif;
       //*** End Add *** 11/15/16 *****************************************

          // Bill Type Code
              If EObtyp = 'H';
                 If EUicn = 'HO' or EUicn = 'HS' ;
                    IShbtype   =  'OUT';
                 Endif;

                 If EUicn = 'IS' or EUicn = 'IP' or
                  EUicn = 'IR' or EUicn = 'IM';
                    IShbtype   =  'INP';
                 EndIf;
                 IShbtype   =  *Blanks;
              Endif;

       // Place of Service Bill
              IShpos     =  *Blanks;

       // First Date of Service
              IShfdat    = %Char(dtMDYY(EOfrdt));

         // Last Date of Service
              IShldat    =  %Char(dtMDYY(EOtodt));

         // ICD Type 1-16, ICD Ref ID 1-16, ICD Code 1-16
              Select;
                 When EObtyp = 'N';
                    Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peh00;
                    If %Found(faehl);
                       If EHicdind ='Y';
                          IShitype1  =  'ICD10';
                          IShitype2  =  'ICD10';
                          IShitype3  =  'ICD10';
                          IShitype4  =  'ICD10';
                       Else;
                          IShitype1  =  'ICD9';
                          IShitype2  =  'ICD9';
                          IShitype3  =  'ICD9';
                          IShitype4  =  'ICD9';
                       Endif;
                       IShitype5  = *Blanks  ;
                       IShitype6  = *Blanks  ;
                       IShitype7  = *Blanks  ;
                       IShitype8  = *Blanks  ;
                       IShitype9  = *Blanks  ;
                       IShitype10 = *Blanks  ;
                       IShitype11 = *Blanks  ;
                       IShitype12 = *Blanks  ;
                       IShitype13 = *Blanks  ;
                       IShitype14 = *Blanks  ;
                       IShitype15 = *Blanks  ;
                       IShitype16 = *Blanks  ;

         //*** Beg Add *** 08/25/16 ***************************
         // ICD-10 Codes
                      If EHicdind ='Y';
         //*** End Add *** 08/25/16 ***************************
                       IShicode1  = EHICD1001;
                       IShicode2  = EHICD1002;
                       IShicode3  = EHICD1003;
                       IShicode4  = EHICD1004;
                       IShicode5  = *Blanks  ;
                       IShicode6  = *Blanks  ;
                       IShicode7  = *Blanks  ;
                       IShicode8  = *Blanks  ;
                       IShicode9  = *Blanks  ;
                       IShicode10 = *Blanks  ;
                       IShicode11 = *Blanks  ;
                       IShicode12 = *Blanks  ;
                       IShicode13 = *Blanks  ;
                       IShicode14 = *Blanks  ;
                       IShicode15 = *Blanks  ;
                       IShicode16 = *Blanks  ;
         //*** Beg Del *** 08/25/16 *************************************
          //******  Endif;
         //*** End Del *** 08/25/16 *************************************
         //*** Beg Add *** 08/25/16 ***************************
         // Clear type if code is blank
                       if EHicd1001 = *Blanks;
                         IShitype1 = *Blanks  ;
                       Endif;
                       if EHicd1002 = *Blanks;
                         IShitype2 = *Blanks  ;
                       Endif;
                       if EHicd1003 = *Blanks;
                         IShitype3 = *Blanks  ;
                       Endif;
                       if EHicd1004 = *Blanks;
                         IShitype4 = *Blanks  ;
                       Endif;
                     Endif;
         // ICD-09
                       If EHicdind <> 'Y';
                         IShicode1  = EHdia1;
                         IShicode2  = EHdia2;
                         IShicode3  = EHdia3;
                         IShicode3  = EHdia4;
         // Clear type if code is blank
                         if EHdia1 = *Blanks;
                           IShitype1 = *Blanks  ;
                         Endif;
                         if EHdia2 = *Blanks;
                           IShitype2 = *Blanks  ;
                         Endif;
                         if EHdia2 = *Blanks;
                           IShitype2 = *Blanks  ;
                         Endif;
                         if EHdia2 = *Blanks;
                           IShitype2 = *Blanks  ;
                         Endif;
                      Endif;
         //*** End Add *** 08/25/16 ***************************
                       IShirefid1 =  'A'       ;
                       IShirefid2 =  'B'       ;
                       IShirefid3 =  'C'       ;
                       IShirefid4 =  'D'       ;
                       IShirefid5 =  *Blanks   ;
                       IShirefid6 =  *Blanks   ;
                       IShirefid7 =  *Blanks   ;
                       IShirefid8 =  *Blanks   ;
                       IShirefid9 =  *Blanks   ;
                       IShirfid10 =  *Blanks   ;
                       IShirfid11 =  *Blanks   ;
                       IShirfid12 =  *Blanks   ;
                       IShirfid13 =  *Blanks   ;
                       IShirfid14 =  *Blanks   ;
                       IShirfid15 =  *Blanks   ;
                       IShirfid16 =  *Blanks   ;
         //*** Beg Add *** 08/25/16 ***************************
         // Clear reference ID if type is blank
                       if IShitype1 = *Blanks;
                         IShirefid1 = *Blanks  ;
                       Endif;
                       if IShitype2 = *Blanks;
                         IShirefid2 = *Blanks  ;
                       Endif;
                       if IShitype3 = *Blanks;
                         IShirefid3 = *Blanks  ;
                       Endif;
                       if IShitype4 = *Blanks;
                         IShirefid4 = *Blanks  ;
                       Endif;
         //*** End Add *** 08/25/16 ***************************
         //*** Beg Add *** 08/25/16 *************************************
                    Endif;
         //*** End Add *** 08/25/16 *************************************

                 When EObtyp = 'H';
                    Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peu00;
                    If %Found(faeul);
                       If EUicdind ='Y';
                          IShitype1  =  'ICD10';
                          IShitype2  =  'ICD10';
                          IShitype3  =  'ICD10';
                          IShitype4  =  'ICD10';
                          IShitype5  =  'ICD10';
                          IShitype6  =  'ICD10';
                          IShitype7  =  'ICD10';
                          IShitype8  =  'ICD10';
                          IShitype9  =  'ICD10';
                          IShitype10 =  'ICD10';
                          IShitype11 =  'ICD10';
                          IShitype12 =  'ICD10';
                          IShitype13 =  'ICD10';
                          IShitype14 =  'ICD10';
                          IShitype15 =  'ICD10';
                          IShitype16 =  'ICD10';
                       Else;
                          IShitype1  =  'ICD9';
                          IShitype2  =  'ICD9';
                          IShitype3  =  'ICD9';
                          IShitype4  =  'ICD9';
                          IShitype5  =  'ICD9';
                          IShitype6  =  'ICD9';
                          IShitype7  =  'ICD9';
                          IShitype8  =  'ICD9';
                          IShitype9  =  'ICD9';
                          IShitype10 =  'ICD9';
                          IShitype11 =  'ICD9';
                          IShitype12 =  'ICD9';
                          IShitype13 =  'ICD9';
                          IShitype14 =  'ICD9';
                          IShitype15 =  'ICD9';
                          IShitype16 =  'ICD9';
                       Endif;
         //*** Beg Add *** 08/25/16 ***************************
                      If EUicdind ='Y';
         //*** End Add *** 08/25/16 ***************************
                       IShicode1  = EUICD1001  ;
                       IShicode2  = EUICD1002  ;
                       IShicode3  = EUICD1003  ;
                       IShicode4  = EUICD1004  ;
                       IShicode5  = EUICD1005  ;
                       IShicode6  = EUICD1006  ;
                       IShicode7  = EUICD1007  ;
                       IShicode8  = EUICD1008  ;
                       IShicode9  = EUICD1009  ;
                       IShicode10 = EUICD1010  ;
                       IShicode11 = *Blanks    ;
                       IShicode12 = *Blanks    ;
                       IShicode13 = *Blanks    ;
                       IShicode14 = *Blanks    ;
                       IShicode15 = *Blanks    ;
                       IShicode16 = *Blanks    ;
         //*** Beg Del *** 08/25/16 *************************************
          //******  Endif;
         //*** End Del *** 08/25/16 *************************************
         //*** Beg Add *** 08/25/16 ***************************
         // Clear type if code is blank
                       if EUicd1001 = *Blanks;
                         IShitype1 = *Blanks  ;
                       Endif;
                       if EUicd1002 = *Blanks;
                         IShitype2 = *Blanks  ;
                       Endif;
                       if EUicd1003 = *Blanks;
                         IShitype3 = *Blanks  ;
                       Endif;
                       if EUicd1004 = *Blanks;
                         IShitype4 = *Blanks  ;
                       Endif;
                       if EUicd1005 = *Blanks;
                         IShitype5 = *Blanks  ;
                       Endif;
                       if EUicd1006 = *Blanks;
                         IShitype6 = *Blanks  ;
                       Endif;
                       if EUicd1007 = *Blanks;
                         IShitype7 = *Blanks  ;
                       Endif;
                       if EUicd1008 = *Blanks;
                         IShitype8 = *Blanks  ;
                       Endif;
                       if EUicd1009 = *Blanks;
                         IShitype9 = *Blanks  ;
                       Endif;
                       IShitype10 = *Blanks  ;
                       IShitype11 = *Blanks  ;
                       IShitype12 = *Blanks  ;
                       IShitype13 = *Blanks  ;
                       IShitype14 = *Blanks  ;
                       IShitype15 = *Blanks  ;
                       IShitype16 = *Blanks  ;
                      Endif;
         // ICD-09
                       If EUicdind <> 'Y';
                         IShicode1  = EUdia1;
                         IShicode2  = EUdia2;
                         IShicode3  = EUdia3;
                         IShicode3  = EUdia4;
                         IShicode5  = EUdia5;
                         IShicode6  = EUdia6;
                         IShicode7  = EUdia7;
                         IShicode8  = EUdia8;
                         IShicode9  = EUdia9;
                         IShicode10 = *Blanks;
                         IShicode11 = *Blanks    ;
                         IShicode12 = *Blanks    ;
                         IShicode13 = *Blanks    ;
                         IShicode14 = *Blanks    ;
                         IShicode15 = *Blanks    ;
                         IShicode16 = *Blanks    ;
         // Clear type if code is blank
                         if EUdia1 = *Blanks;
                           IShitype1 = *Blanks  ;
                         Endif;
                         if EUdia2 = *Blanks;
                           IShitype2 = *Blanks  ;
                         Endif;
                         if EUdia3 = *Blanks;
                           IShitype3 = *Blanks  ;
                         Endif;
                         if EUdia4 = *Blanks;
                           IShitype4 = *Blanks  ;
                         Endif;
                         if EUdia5 = *Blanks;
                           IShitype5 = *Blanks  ;
                         Endif;
                         if EUdia6 = *Blanks;
                           IShitype6 = *Blanks  ;
                         Endif;
                         if EUdia7 = *Blanks;
                           IShitype7 = *Blanks  ;
                         Endif;
                         if EUdia8 = *Blanks;
                           IShitype8 = *Blanks  ;
                         Endif;
                         if EUdia9 = *Blanks;
                           IShitype9 = *Blanks  ;
                         Endif;
                         IShitype10 = *Blanks  ;
                         IShitype11 = *Blanks  ;
                         IShitype12 = *Blanks  ;
                         IShitype13 = *Blanks  ;
                         IShitype14 = *Blanks  ;
                         IShitype15 = *Blanks  ;
                         IShitype16 = *Blanks  ;
                       Endif;
         //*** End Add *** 08/25/16 ***************************

                       IShirefid1 =  'A';
                       IShirefid2 =  'B';
                       IShirefid3 =  'C';
                       IShirefid4 =  'D';
                       IShirefid5 =  'E';
                       IShirefid6 =  'F';
                       IShirefid7 =  'G';
                       IShirefid8 =  'H';
                       IShirefid9 =  'I';
                       IShirfid10=  'J';
                       IShirfid11=  *Blanks;
                       IShirfid12=  *Blanks;
                       IShirfid13=  *Blanks;
                       IShirfid14=  *Blanks;
                       IShirfid15=  *Blanks;
                       IShirfid16=  *Blanks;
         //*** Beg Add *** 08/25/16 *************************************
         //*** Beg Add *** 08/25/16 ***************************
         // Clear reference ID if type is blank
                       if IShitype1 = *Blanks;
                         IShirefid1 = *Blanks  ;
                       Endif;
                       if IShitype2 = *Blanks;
                         IShirefid2 = *Blanks  ;
                       Endif;
                       if IShitype3 = *Blanks;
                         IShirefid3 = *Blanks  ;
                       Endif;
                       if IShitype4 = *Blanks;
                         IShirefid4 = *Blanks  ;
                       Endif;
                       if IShitype5 = *Blanks;
                         IShirefid5 = *Blanks  ;
                       Endif;
                       if IShitype5 = *Blanks;
                         IShirefid5 = *Blanks  ;
                       Endif;
                       if IShitype6 = *Blanks;
                         IShirefid6 = *Blanks  ;
                       Endif;
                       if IShitype7 = *Blanks;
                         IShirefid7 = *Blanks  ;
                       Endif;
                       if IShitype8 = *Blanks;
                         IShirefid8 = *Blanks  ;
                       Endif;
                       if IShitype9 = *Blanks;
                         IShirefid9 = *Blanks  ;
                       Endif;
                       if IShitype10 = *Blanks;
                         IShirfid10= *Blanks  ;
                       Endif;
                       if IShitype11 = *Blanks;
                         IShirfid11= *Blanks  ;
                       Endif;
                       if IShitype12 = *Blanks;
                         IShirfid12= *Blanks  ;
                       Endif;
                       if IShitype13 = *Blanks;
                         IShirfid13= *Blanks  ;
                       Endif;
                       if IShitype14 = *Blanks;
                         IShirfid14= *Blanks  ;
                       Endif;
                       if IShitype15 = *Blanks;
                         IShirfid15= *Blanks  ;
                       Endif;
                       if IShitype16 = *Blanks;
                         IShirfid16= *Blanks  ;
                       Endif;
                    Endif;
         //*** End Add *** 08/25/16 *************************************

                 When EObtyp = 'X';
                    IShitype1  =  *Blanks;
                    IShitype2  =  *Blanks;
                    IShitype3  =  *Blanks;
                    IShitype4  =  *Blanks;
                    IShitype5  =  *Blanks;
                    IShitype6  =  *Blanks;
                    IShitype7  =  *Blanks;
                    IShitype8  =  *Blanks;
                    IShitype9  =  *Blanks;
                    IShitype10 =  *Blanks;
                    IShitype11 =  *Blanks;
                    IShitype12 =  *Blanks;
                    IShitype13 =  *Blanks;
                    IShitype14 =  *Blanks;
                    IShitype15 =  *Blanks;
                    IShitype16 =  *Blanks;

                    IShirefid1 =  *Blanks;
                    IShirefid2 =  *Blanks;
                    IShirefid3 =  *Blanks;
                    IShirefid4 =  *Blanks;
                    IShirefid5 =  *Blanks;
                    IShirefid6 =  *Blanks;
                    IShirefid7 =  *Blanks;
                    IShirefid8 =  *Blanks;
                    IShirefid9 =  *Blanks;
                    IShirfid10=  *Blanks;
                    IShirfid11=  *Blanks;
                    IShirfid12=  *Blanks;
                    IShirfid13=  *Blanks;
                    IShirfid14=  *Blanks;
                    IShirfid15=  *Blanks;
                    IShirfid16=  *Blanks;

                    IShicode1  = *Blanks;
                    IShicode2  = *Blanks;
                    IShicode3  = *Blanks;
                    IShicode4  = *Blanks;
                    IShicode5  = *Blanks;
                    IShicode6  = *Blanks;
                    IShicode7  = *Blanks;
                    IShicode8  = *Blanks;
                    IShicode9  = *Blanks;
                    IShicode10 = *Blanks;
                    IShicode11 = *Blanks;
                    IShicode12 = *Blanks;
                    IShicode13 = *Blanks;
                    IShicode14 = *Blanks;
                    IShicode15 = *Blanks;
                    IShicode16 = *Blanks;

                 When EObtyp = 'D';
                    Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) ped00;
                    If %found(faedl);
                       If EDicdind ='Y';
                          IShitype1  =  'ICD10';
                          IShitype2  =  'ICD10';
                          IShitype3  =  'ICD10';
                          IShitype4  =  'ICD10';
                       Else;
                          IShitype1  =  'ICD9';
                          IShitype2  =  'ICD9';
                          IShitype3  =  'ICD9';
                          IShitype4  =  'ICD9';
                       Endif;
                       IShitype5  = *Blanks  ;
                       IShitype6  = *Blanks  ;
                       IShitype7  = *Blanks  ;
                       IShitype8  = *Blanks  ;
                       IShitype9  = *Blanks  ;
                       IShitype10 = *Blanks  ;
                       IShitype11 = *Blanks  ;
                       IShitype12 = *Blanks  ;
                       IShitype13 = *Blanks  ;
                       IShitype14 = *Blanks  ;
                       IShitype15 = *Blanks  ;
                       IShitype16 = *Blanks  ;

         //*** Beg Add *** 08/25/16 ***************************
         // ICD-10 Codes
                    If EDicdind ='Y';
         //*** End Add *** 08/25/16 ***************************
                       IShicode1  = EDICD1001;
                       IShicode2  = EDICD1002;
                       IShicode3  = EDICD1003;
                       IShicode4  = EDICD1004;
                       IShicode5  = *Blanks  ;
                       IShicode6  = *Blanks  ;
                       IShicode7  = *Blanks  ;
                       IShicode8  = *Blanks  ;
                       IShicode9  = *Blanks  ;
                       IShicode10 = *Blanks  ;
                       IShicode11 = *Blanks  ;
                       IShicode12 = *Blanks  ;
                       IShicode13 = *Blanks  ;
                       IShicode14 = *Blanks  ;
                       IShicode15 = *Blanks  ;
                       IShicode16 = *Blanks  ;
         //*** Beg Add *** 08/25/16 ***************************
         // Clear type if code is blank
                    if EDicd1001 = *Blanks;
                      IShitype1 = *Blanks  ;
                    Endif;
                    if EDicd1002 = *Blanks;
                      IShitype2 = *Blanks  ;
                    Endif;
                    if EDicd1003 = *Blanks;
                      IShitype3 = *Blanks  ;
                    Endif;
                    if EDicd1004 = *Blanks;
                      IShitype4 = *Blanks  ;
                    Endif;
                  Endif;
         // ICD-09
                       If EDicdind <> 'Y';
                         IShicode1  = EDdia1;
                         IShicode2  = EDdia2;
                         IShicode3  = EDdia3;
                         IShicode3  = EDdia4;
         // Clear type if code is blank
                         if EDdia1 = *Blanks;
                           IShitype1 = *Blanks  ;
                         Endif;
                         if EDdia2 = *Blanks;
                           IShitype2 = *Blanks  ;
                         Endif;
                         if EDdia3 = *Blanks;
                           IShitype3 = *Blanks  ;
                         Endif;
                         if EDdia4 = *Blanks;
                           IShitype4 = *Blanks  ;
                         Endif;
                       Endif;
         //*** End Add *** 08/25/16 ***************************
         //*** Beg Del *** 08/25/16 *************************************
          //**** Endif;
         //*** End Del *** 08/25/16 *************************************
                       IShirefid1 =  '1'       ;
                       IShirefid2 =  '2'       ;
                       IShirefid3 =  '3'       ;
                       IShirefid4 =  '4'       ;
                       IShirefid5 =  *Blanks   ;
                       IShirefid6 =  *Blanks   ;
                       IShirefid7 =  *Blanks   ;
                       IShirefid8 =  *Blanks   ;
                       IShirefid9 =  *Blanks   ;
                       IShirfid10=  *Blanks   ;
                       IShirfid11=  *Blanks   ;
                       IShirfid12=  *Blanks   ;
                       IShirfid13=  *Blanks   ;
                       IShirfid14=  *Blanks   ;
                       IShirfid15=  *Blanks   ;
                       IShirfid16=  *Blanks   ;
         //*** Beg Add *** 08/25/16 ***************************
         // Clear reference ID if type is blank
                 if IShitype1 = *Blanks;
                   IShirefid1 = *Blanks  ;
                 Endif;
                 if IShitype2 = *Blanks;
                   IShirefid2 = *Blanks  ;
                 Endif;
                 if IShitype3 = *Blanks;
                   IShirefid3 = *Blanks  ;
                 Endif;
                 if IShitype4 = *Blanks;
                   IShirefid4 = *Blanks  ;
                 Endif;
         //*** End Add *** 08/25/16 ***************************
         //*** Beg Add *** 08/25/16 *************************************
                    Endif;
         //*** End Add *** 08/25/16 *************************************
              Endsl;
        //Write/Update Medical header file
              If isUpdate = 'Y';
                 Update pish00;
                 WriteFAISDP();
              Else;
                 Write pish00;
                 WriteFAISDP();
              EndIf;
          //*** Beg Add *** 07/19/17 **********************************
          //* Create a log if data area is set to allow
                 in *lock *dtaara;
                 isScoreLog   = IsoScoreLg;
                 Unlock IsoScoreLg;
                 if isScoreLog = 'Y';
                    LGfile    = @@File;
                    LGevent   = @@Event;
                    LGco#     = @@CCo#;
                    LGfnd     = @@Cfnd;
                    LGempeob  = @@CEmpOrEob;
                    LGdivSub  = @@CDivOrSub;
                    LGfyr     = @@Cfyr;
                    LGcase    = @@CCase;
                    LGdt      = dateTo7(systemDate);
                    LGtime    = timeto6(systemTime);
                    LGuser    = Q1user;
                    Write pislg01;
                 endif;
          //*** End Add *** 07/19/17 **********************************
        //*** Beg Del *** 08/25/16 ***************************************
        //*** Reade (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob:
        //***  EOrcn) pea01;

        //***   EndDo;
        //*** End Del *** 08/25/16 ***************************************
        EndIf;

      /End-free
     p WriteFAISHP     e
       //***** Beg Add *** 08/30/16 ********************************
       //===================================================================
       // Delete FAISDP Detail Extract file
       //===================================================================
     p DeleteFAISDP    b
     d DeleteFAISDP    pi
      /Free
        Setll (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) pca18;
        Reade (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) pca18;
        Dow not %Eof(fdcal18);
           If intSeq = CAdseq ;
              strCaEob# = %editC(CAeob#:'X');
              strCaLin# = %editC(CAlin#:'X');
              Chain (strCaEob#:strCaLin#) pisd00;
              If %found(faisdl);
                 Delete pisd00;
                 Leave;
              Endif;
           Endif;
           Reade (@@co#:@@DivOrSub:@@fnd:@@fyr:@@case:@@EmpOrEob) pca18;
        EndDo;
      /End-free
     p DeleteFAISDP    e
       //***** End Add *** 08/30/16 ********************************

     // ***** Beg Add *** 08/19/16 *************************
      //***************************************************************
      // WrtCMSExtract : Write CMS Claim Extract Files
      //***************************************************************
     p WrtCMSExtract   b
     d WrtCMSExtract   pi
      /Free

       // Write/Update Operating companies Extract File
        Chain (CMco#:CMfnd) wmfnl;
        Chain (CMco#:CMfnd) wdf2l;
        strRre#   = %Trim(%Char(F2rre#));
        strCoTaxid= %Trim(%Char(F2fein));

        Chain (strCoKey:strRre#:strCoTaxid) pisocm100;
        If %Found(waisocm1l);
           loadWAISOCM1();
           Update pisocm100;
        Else;
           loadWAISOCM1();
           Write pisocm100;
        Endif;

       // Write/Update Organization Extract File
        Chain (CMco#:CMoff#) pof00;
        If %Found(wmofl);
           Chain (strOfcKey) pisocm200;
           If %Found(waisocm2l);
              loadWAISOCM2();
              Update pisocm200;
           Else;
              loadWAISOCM2();
              Write pisocm200;
           EndIf;
        Endif;

       // Write/Update ISO CMS Claim Extract Files
        Chain (strclmkey) pisocm300;
        If %Found(waisocm3l);
           loadWAISOCM3();
           Update pisocm300;
        Else;
           loadWAISOCM3();
           Write pisocm300;
        Endif;

       // Write/Update ICD Extract File
        Setll (CMco#:CMfnd:CMfyr:CMcase) pda03;
        Reade (CMco#:CMfnd:CMfyr:CMcase) pda03;
        Dow Not %Eof(sadal3);
           If DAicdind   = 'Y';
              strICDcode = %Trim(DAicd1001);
           Else;
              strICDcode = %Trim(DAicd9);
           Endif;

           Chain (strClmtKey:strICDcode) pisocm401;
           If %Found(waisocm4l1);
              loadWAISOCM4();
              Update pisocm401;
           Else;
              loadWAISOCM4();
              Write pisocm401;
           Endif;
           Reade (CMco#:CMfnd:CMfyr:CMcase) pda03;
        EndDo;

       // Write/Update TPOC Extract File
        Chain (strclmkey) pisocm500;
        If %Found(waisocm5l);
           I5seq#+= 1;
           loadWAISOCM5();
           Update pisocm500;
        Else;
           I5seq# = 1;
           loadWAISOCM5();
           Write pisocm500;
        Endif;
      /End-free
     p WrtCMSExtract   e

     //===================================================================
     // ISO CMS Operating Companies Work file : WAISOCM1
     //===================================================================
     ploadWAISOCM1     B
     dloadWAISOCM1     PI
      /free
        I1cokey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
        I1rre#    = %Trim(%Char(F2rre#));
        I1cotaxid = %Trim(%Char(F2fein));
        I1coname  = %Trim(FNname);
        I1addr1   = %Trim(FNmad1);
        I1addr2   = %Trim(FNmad2);
        I1city    = %Trim(FNmcty);
        I1state   = %Trim(FNmst);
        If FNmzip <> *Zeros;
        //*** BegChg ***** 11/28/16 ****************************
          //*** I1zip  = %Subst(%Trim(%Char(FNmzip)):1:5)  ;
          //*** I1zip4 = %Subst(%Trim(%Char(FNmzip)):6:4)  ;
           strFNmzip =  %Trim(%Char(FNmzip)) + '000000000';
           I3injzip  = %Subst(strFNmzip:1:5)  ;
           I3injzip4 = %Subst(strFNmzip:6:4)  ;
        //*** EndChg ***** 11/28/16 ****************************
        EndIf;

       // Get MIR Group Stored in Prompt Data area
        in *lock *dtaara;
        isods   = IsoCMSPrmp;
        Unlock IsoCMSPrmp;
        Monitor;
           I1mirrgrp = %Dec(strMIRGrp:4:0);
        On-Error;
           I1mirrgrp = *Zeros ;
        EndMon;

        I1conlnam = *Blanks;                            // Optional field
        I1confon  = *Blanks;                            // Optional field
        I1conemail= *Blanks;                            // Optional field
      /End-free
     ploadWAISOCM1     e

     //===================================================================
     // ISO CMS Organization Work file : WAISOCM2
     //===================================================================
     ploadWAISOCM2     b
     dloadWAISOCM2     pi
      /free
        If CMoff# <> *Blanks;
           I2ofckey  = %Editc(CMco#:'X') +
            %Editc(%Dec(%Trim(CMoff#):5:0):'X');
        EndIf;
        I2teamnam = %Trim(OFname);
        //Chain (CMclex) pup01;

        Chain (OFuser) pup00;
        I2adjkey    = %Trim(UPusr);
        I2ofctaxid  = *Blanks;                            // Optional field
        I2ofcname   = %Trim(OFname);
        I2ofcadd1   = %Trim(OFadr1);
        I2ofcadd2   = %Trim(OFadr2);
        I2ofccity   = %Trim(OFcty);
        I2ofcstate  = %Trim(OFst);
        If OFzip <> *Zeros;
        //*** Beg Chg ***** 11/28/16 ****************************
          //*** I2ofczip = %Subst(%Trim(%Char(OFzip)):1:5)  ;
          //*** I2ofczip4= %Subst(%Trim(%Char(OFzip)):6:4)  ;
           strOFzip =  %Trim(%Char(OFzip)) + '000000000';
           I2ofczip = %Subst(strOFzip:1:5)  ;
           I2ofczip4= %Subst(strOFzip:6:4)  ;
        //*** End Chg ***** 11/28/16 ****************************
           EndIf;
        I2extorg  = 'N';
        I2adjlnam = %Trim(UPusnm);
        I2adjfnam = %Trim(UPusnm);
        I2adjemail= %Trim(UPmail);
        I2adjfon  = %Editc(UPphna:'X') + %Editc(UPphn#:'X');
      /End-free
     ploadWAISOCM2     e

     //===================================================================
     // Write ISO CMS Claim Work file : WAISOCM3
     //===================================================================
     ploadWAISOCM3     b
     dloadWAISOCM3     pi
      /Free

        I3clmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
         %Editc(CMfyr:'X') + %Editc(CMcase:'X') + 'W0070R' ;

        //***** Beg Chg *** 11/04/16 ***********************
        //I3clmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
        // %Editc(CMfyr:'X') + %Editc(CMcase:'X');

        I3clmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
         %Editc(CMfyr:'X') + %Editc(CMcase:'X') + CMss# + 'E';
        //***** End Chg *** 11/04/16 ***********************

        Chain (CMco#:CMfnd:CMemp#:CMdiv) pem00;
        If %Found(wmeml);
           I3oprcokey=  %Editc(EMco#:'X')  + %Editc(EMfnd:'X')  +
            %Editc(CMemp#:'X') + %Editc(CMdiv:'X');
        Endif;

        Chain (CMco#:CMfnd) pf200;
        If %Found(wdf2l);
           I3rre# = %Trim(%Char(F2rre#));
        Endif;

        Chain (CMclex) pup01;
        If %Found(smupl1);
           I3adjkey = UPusr;
        Endif;
        //***** Beg Chg *** 10/14/16*******************************
        If CMcmdi  <> *Zeros;
           //I3cmsdoi= CMcmdi;
           I3cmsdoi  = 19000000 + CMcmdi;
        Endif;
        If CMacdt  <> *Zeros;
           //I3inddoi= CMacdt;
           I3inddoi  = 19000000 + CMacdt;
        Endif;
        //***** End Chg *** 10/14/16*******************************
        I3algcoi  = *Blanks;                           // No longer in use
        I3state   = CMjrst;
        //***** Beg Chg *** 10/14/16*******************************
        If CMdtrp   <> *Zeros;
           I3dtrepd  = 19000000 + CMdtrp;
        Endif;
        //***** End Chg *** 10/14/16*******************************
        I3clmsts  = CMstat;
        I3clmrea  = *Blanks;                           // optional - for future use
        I3impper  = *Zeros ;                           // optional
        I3distypc = *Blanks;                           // optional
        I3injdesc = *Blanks;                           // optional
        I3prdliai = *Blanks;                           // optional
        I3prdgenn = *Blanks;                           // optional
        I3prdbrnn = *Blanks;                           // optional
        I3prdman  = *Blanks;                           // optional
        I3prdalgh = *Blanks;                           // optional
        I3injlstn = %Trim(CMlsn);
        I3injfsn  = %Trim(CMfsn);
        I3injmidi = CMmid;

        Chain (CMss#) pcd00;
        I3injadd1 = CDadr1 ;
        I3injadd2 = CDadr2 ;
        I3injcity = CDcty  ;
        I3injstate= CDst   ;
        If CDzip <> *Zeros;
        //*** Beg Chg ***** 11/28/16 *******************************
        // I3injzip  = %Subst(%Trim(%Char(CDzip)):1:5)  ;
        // I3injzip4 = %Subst(%Trim(%Char(CDzip)):6:4)  ;
           strCdZip =  %Trim(%Char(CDzip)) + '000000000';
           I3injzip  = %Subst(strCdZip:1:5)  ;
           I3injzip4 = %Subst(strCdZip:6:4)  ;
        //*** End Chg ***** 11/28/16 *******************************
        Else;
           I3injzip  = *Blanks;
           I3injzip4 = *Blanks;
        EndIf;
        If CDdtob <> *Zeros;
           I3injdob  = 19000000 + CDdtob ;
        EndIf;

        Select;
        When CDsex  = ' ' ;        // Unknown
           I3injsex = '0' ;

        When CDsex  = 'M' ;        // Male
           I3injsex = '1' ;

        When CDsex  = 'F' ;        // Female
           I3injsex = '2' ;
        EndSl;

        I3injssn  = CMss#  ;
        I3injhicn = CDhicn ;
        I3injlstdy= *Blanks;                           // Optional field
        I3injaww  = *Blanks;                           // Optional field
        I3clascd  = *Blanks;                           // Optional field
        I3injdod  = *Zeros ;                           // Optional field
        I3instyp  = 'E'    ;
        I3claim#  = %EditC(CMco#:'X') + %EditC(CMfnd:'X')  +
                    %EditC(CMfyr:'X') + %EditC(CMcase:'X') ;
        I3altclm# = *Blanks ;                          // Optional field
        I3polhldn = %Trim(EMname) + ' ' + %Trim(EMnam2);
        I3pol#    = %EditC(EMco#:'X') + %EditC(EMfnd:'X')  +
                    %EditC(CMemp#:'X') + %EditC(EMdiv:'X') ;
        I3polefdt = *Zeros;                            // Optional field
        I3polexdt = *Zeros;                            // Optional field
        I3sfinind = FNsfin;
        If FNsfin = 'Y';
           I3sfintyp = 'O';
        Else;
           I3sfintyp = *Blanks;
        Endif;
        I3sfinlnam = *Blanks;                          // situational field
        I3sfinfnam = *Blanks;                          // situational field
        I3dbanam   = %Trim(EMname) + ' ' + %Trim(EMnam2);
        I3lglnam   = %Trim(EMfdnm) + ' ' + %Trim(EMfdn2);
        I3nofltlmt = *Zeros;                           // Optional field
        I3sdatnfin = *Zeros;                           // Optional field
        I3medrsv   = *Zeros;                           // Optional field
        I3indrsv   = *Zeros;                           // Optional field
        I3subrsv   = *Zeros;                           // Optional field
        I3medtot   = *Zeros;                           // Optional field
        I3indtot   = *Zeros;                           // Optional field
        I3subrcv   = *Zeros;                           // Optional field

        If CMorm = *Blanks;
           I3ormind = 'N';
        Else;
           I3ormind = CMorm;
        EndIf;

        If CMormd <> *Zeros;
           I3ormtrmdt = 19000000 + CMormd;
        EndIf;
        I3medpaydt = *Zeros;

        Chain (CMco#:CMfnd:CMfyr:CMcase) pll00;
        Chain (LLplaa:LLplam) ppdw0;
        I3reptype  = LLclri;
        If LLrfnm  = *Blanks;
           I3repfnam  = %Trim(WPDfsn);
           I3replnam  = %Trim(WPDname);
        Else ;
           I3repfnam  = %Trim(LLrfnm);
           I3replnam  = %Trim(LLrfnm);
        Endif;
        I3repname     = %Trim(LLrfnm);
        If LLpad1 <> *Blanks ;
           I3repadd1     = %Trim(LLpad1);
        Else;
           I3repadd1     = %Trim(WPDadr1);
        Endif;
        If LLpad2 <> *Blanks ;
           I3repadd2     = %trim(LLpad2);
        Else;
           I3repadd2     = %trim(WPDadr2);
        Endif;
        If LLpcty <> *Blanks ;
           I3repcity     = %Trim(LLpcty);
        Else;
           I3repcity     = %Trim(WPDcty);
        Endif;
        If LLpst  <> *Blanks ;
           I3repstate    = %Trim(LLpst);
        Else;
           I3repstate    = %Trim(WPDst);
        Endif;
        If LLpzip <> *Zeros;
        //*** Beg Chg ***** 11/28/16 ******************************
          //*** I3repzip     = %Subst(%Trim(%Char(LLpzip)):1:5)  ;
          //*** I3repzip4    = %Subst(%Trim(%Char(LLpzip)):6:4)  ;
           strLLpzip    = %Trim(%Char(LLpzip)) + '000000000';
           I3repzip     = %Subst(strLLpzip:1:5)  ;
           I3repzip4    = %Subst(strLLpzip:6:4)  ;
        //*** End Chg ***** 11/28/16 ******************************
        Else;
           If WPDzip <> *Zeros;
        //*** Beg Chg ***** 11/28/16 ******************************
            //***I3repzip  = %Subst(%Trim(%Char(WPDzip)):1:5)  ;
            //***I3repzip4 = %Subst(%Trim(%Char(WPDzip)):6:4)  ;
              strWPDzip = %Trim(%Char(WPDzip)) + '000000000';
              I3repzip  = %Subst(strWPDzip:1:5)  ;
              I3repzip4 = %Subst(strWPDzip:6:4)  ;
        //*** End Chg ***** 11/28/16 ******************************
           EndIf;
        Endif;
        If LLplaa <> *Zeros ;
           I3reptaxid  = %Trim(%Char(LLplaa));
        Else;
           I3reptaxid  = %Trim(%Char(WPDfein));
        Endif;

        I3repfon    = *Blanks;                         // situatinal field
        I3repfext   = *Blanks;                         // optional field
        I3fpayind   = *Blanks;                         // optional field
        I3clmnotty    = *Blanks;                       // optional field
        I3litstlin    = *Blanks;                       // optional field
        I3medrlsin    = *Blanks;                       // optional field
      /End-free
     ploadWAISOCM3     e

     //===================================================================
     // ISO CMS ICD Work file : WAISOCM4
     //===================================================================
     ploadWAISOCM4     b
     dloadWAISOCM4     pi
      /Free
        I4clmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
         %Editc(CMfyr:'X') + %Editc(CMcase:'X') + 'W0070R' ;

        //***** Beg Chg *** 11/04/16 ***********************
        //I4clmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
        // %Editc(CMfyr:'X') + %Editc(CMcase:'X') ;

        I4clmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
         %Editc(CMfyr:'X') + %Editc(CMcase:'X') + CMss# + 'E';
        //***** End Chg *** 11/04/16 ***********************

        If DAicdind = 'Y';
           I4icdcode = %Trim(DAicd1001);
        Else;
           I4icdcode = %Trim(DAicd9);
        Endif;
        I4instype ='E';

      /End-free
     ploadWAISOCM4     e

     //===================================================================
     // ISO CMS TPOC Work file: WAISOCM5
     //===================================================================
     ploadWAISOCM5     b
     dloadWAISOCM5     pi
      /free

        I5clmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
         %Editc(CMfyr:'X') + %Editc(CMcase:'X') + 'W0070R' ;

        //***** Beg Chg *** 11/04/16 ***********************
        //I5clmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
        // %Editc(CMfyr:'X') + %Editc(CMcase:'X') ;

        I5clmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X')  +
         %Editc(CMfyr:'X') + %Editc(CMcase:'X') + CMss# + 'E';
        //***** End Chg *** 11/04/16 ***********************

        Chain (CMco#:CMfnd:CMfyr:CMcase) pll00;
        I5stlmamt  =  LLsamt   ;
        If LLstdt <> *Zeros;
           I5stlmdt   =  19000000 + LLstdt   ;
        EndIf;
        I5fundlydt = *Zeros;                           // Optional field
        I5instype  = 'E';

      /End-free
     ploadWAISOCM5     e

      //===================================================================
      // Process WDF2P file for changes
      //===================================================================
     p ProcessWDF2P    b
     d ProcessWDF2P    pi
      /Free

        Setll (@@Co#:@@Fnd) pcm00;
        Reade (@@Co#:@@Fnd) pcm00;
        Dow not %Eof (wmcml);
          // Write/Update CMS extract work files

          // Prepare keys to read CMS extract files
           strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
           If CMoff# <> *Blanks;
              strOfckey  = %Editc(CMco#:'X') +
               %Editc(%Dec(%Trim(CMoff#):5:0):'X');
           EndIf;
           strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
            %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

           //***** Beg Chg *** 11/04/16 ***********************
           //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
           // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

           strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
            %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
           //***** End Chg *** 11/04/16 ***********************

           WrtCMSExtract();
           Reade (@@Co#:@@Fnd) pcm00;
        EndDo;
      /End-free
     p ProcessWDF2P    e

      //===================================================================
      // Process WMOFP file for changes
      //===================================================================
     p ProcessWMOFP    b
     d ProcessWMOFP    pi
      /Free
         Setll (@@Co#) pcm00;
         Reade (@@Co#) pcm00;
         Dow Not %Eof(wmcml);
            If TestGrp(CMCo#:CMFnd) = *off and ExpiredGrp(CMCo#:CMFnd) = *Off
       //*** Beg Chg ***** 12/01/16 *******************************
        //***and CMoff# = @@Ofc#;
             and CMoff# = @@Ofc# and CMfnd >= 300;
       //*** End Chg ***** 12/01/16 *******************************

              // Prepare keys to read CMS extract files
               strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
               If CMoff# <> *Blanks;
                  strOfckey  = %Editc(CMco#:'X') +
                   %Editc(%Dec(%Trim(CMoff#):5:0):'X');
               EndIf;

               strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

               //***** Beg Chg *** 11/04/16 ***********************
               //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
               // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

               strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
               //***** End Chg *** 11/04/16 ***********************

               WrtCMSExtract();
            Endif;
            Reade (@@Co#) pcm00;
         Enddo;
      /End-free
     p ProcessWMOFP    e

      //===================================================================
      // Process WMLLP file for changes
      //===================================================================
     p ProcessWMLLP    b
     d ProcessWMLLP    pi
      /Free

         Setll (@@Co#:@@Fnd:@@Fyr:@@Case) pcm0H;
         Reade (@@Co#:@@Fnd:@@Fyr:@@Case) pcm0H;
         Dow Not %Eof(wmcmlh);
           // Prepare keys to read CMS extract files
            strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
            If CMoff# <> *Blanks;
               strOfckey  = %Editc(CMco#:'X') +
                %Editc(%Dec(%Trim(CMoff#):5:0):'X');
            EndIf;
            strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
             %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

            //***** Beg Chg *** 11/04/16 ***********************
            //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
            // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

            strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
             %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
            //***** End Chg *** 11/04/16 ***********************

            WrtCMSExtract();
            Reade (@@co#:@@fnd:@@fyr:@@case) pcm0H;
         Enddo;
      /End-free
     p ProcessWMLLP    e

      //===================================================================
      // Process WMPDP file for changes
      //===================================================================
     p ProcessWMPDP    b
     d ProcessWMPDP    pi
      /Free

         Setll (@@Co#:@@Fnd) pcm00;
         Reade (@@Co#:@@Fnd) pcm00;
         Dow Not %Eof(wmcml);
           // Prepare keys to read CMS extract files
            strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
            If CMoff# <> *Blanks;
               strOfckey  = %Editc(CMco#:'X') +
                %Editc(%Dec(%Trim(CMoff#):5:0):'X');
            EndIf;
            strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
             %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

            //***** Beg Chg *** 11/04/16 ***********************
            //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
            // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

            strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
             %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
            //***** End Chg *** 11/04/16 ***********************

            WrtCMSExtract();
            Reade (@@Co#:@@Fnd) pcm00;
         Enddo;
      /End-free
     p ProcessWMPDP    e

      //===================================================================
      // Process SADAP file for changes
      //===================================================================
     p ProcessSADAP    b
     d ProcessSADAP    pi
      /Free

         Setll (@@Co#:@@Fnd:@@Fyr:@@Case) pcm0H;
         Reade (@@Co#:@@Fnd:@@Fyr:@@Case) pcm0H;
         Dow Not %Eof(wmcmlh);
           // Prepare keys to read CMS extract files
            strCoKey   = %Editc(CMco#:'X') + %Editc(CMfnd:'X');
            If CMoff# <> *Blanks;
               strOfckey  = %Editc(CMco#:'X') +
                %Editc(%Dec(%Trim(CMoff#):5:0):'X');
            EndIf;
            strClmkey  = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
             %Editc(CMFyr:'X') + %Editc(CMCase:'X') + 'W0070R' ;

            //***** Beg Chg *** 11/04/16 ***********************
            //strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
            // %Editc(CMFyr:'X') + %Editc(CMCase:'X');

            strclmtkey = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
             %Editc(CMFyr:'X') + %Editc(CMCase:'X') + CMss# + 'E';
            //***** End Chg *** 11/04/16 ***********************

            WrtCMSExtract();
            Reade (@@Co#:@@Fnd:@@Fyr:@@Case) pcm0H;
         Enddo;
      /End-free
     p ProcessSADAP    e
     // ***** End Add *** 08/19/16 *************************
