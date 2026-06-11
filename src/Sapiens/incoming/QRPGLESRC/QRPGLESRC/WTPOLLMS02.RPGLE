     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve EFT Information detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLLMST
     h*  Calls to:
     h*
     h*  Created on 10/05/2012 by C:279 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 05/07/21  C:180 Project WC1986
     h*                 Do not display Account type for Employer payees.
     h*                 Make all fields all other fields that are in the
     h*                 sfl detail area and that are also found in the sfl
     h*                 invisible (Display file changes only for invisibility).
     h*
     h*                 Do not display Approved Date/Pre-note Date when
     h*                 not required.
     h*
     h*                 Display Default Info for EFT state reporting division.
     h*
     h* 05/05/15  C:062 Support: 30373(025)
     h*                 Correct address processing.
     h*
     h* 02/14/14  C:189 Support: 84865
     h*                 Detail box not loading the address for 'C' type Payee
     h*
     h**********************************************************************
     fwmefp     if   e             disk    rename(pef01:pef0p)
     fwdacl     if   e           k disk    ReName(pac01:pac00)
     f*** Begin Add *** 05/05/2015 *****************************************
     fwmcdl     if   e           k disk    ReName(pcd01:pcd00)
     f*** End   Add *** 05/05/2015 *****************************************

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString
      /copy wPrEmplr

      * *Entry
     dEFTInformation   pr                  ExtPgm('WTPOLLMS02')
     d                                9  0
     d                            32767
     d                                1

     dEFTInformation   pi
     d inRRN                          9  0
     d outJSON                    32767
     d RtnCode                        1
     D***  Begin Add  ***  05/07/21  *****************************************
     D/copy wPrBank
     D***  End   Add  ***  05/07/21  *****************************************
     d ##co#           s                   like(EFco#)
     d ##fnd           s                   like(EFfnd)
     d ##emp#          s                   like(EFemp#)
     d ##div           s                   like(EFdiv)
     d strAcTyp        s             20
     d strActTyp       s             20
     d strDtlCAd1      s             40    inz(*blanks)
     d strDtlCAd2      s             40    inz(*blanks)
     d strDtlcsz1      s             40    inz(*blanks)
      *** Begin Add *** 05/07/21 ***************************************
     d strDftInfoEft   s              1
     d intStRptDiv     s                   like(EFdiv) inz
     d isPreNote       s               N
     D sysDate7        s              7  0 inz
     D strType         s              1A   inz('E')
     d strAccountType  s             12A   inz('Account Type')
      *** End   Add *** 05/07/21 ***************************************
      *

     C     keyWDACL#     Klist
     C                   Kfld                    ##co#
     C                   Kfld                    ##fnd
     C                   Kfld                    EFfyr
     C                   Kfld                    EFcase
     C                   Kfld                    EFseq#

      /Free

       Clear strAcTyp;
       Clear strActTyp;
       Clear strDtlCAd1;
       Clear strDtlCAd2;
       Clear strDtlcsz1;

       Chain(n) inRRN pef0p;
        // set key values
       ##co#  = EFco#;
       ##fnd  = EFfnd;
       ##emp# = EFemp#;
       ##div  = EFdiv;

       IF EFeatp = 'AG';
          strAcTyp = 'Agency';
       Else;
          strAcTyp = 'Employer';
       Endif;

       Select;
       When EFtype = 'A';
          strActTyp = 'Agent';
       When EFtype = 'C';
          strActTyp = 'Claimant';
       When EFtype = 'D';
          strActTyp = 'Additional Payee';
       When EFtype = 'E';
       //*** Begin Change *** 05/07/21 ******************************
       // strActTyp = 'Employer';
          strActTyp = *blanks;
          strAcTyp  = *blanks;
          strAccountType = *blanks;
       //*** End   Change *** 05/07/21 ******************************
          strActTyp = *blanks;
       When EFtype = 'P';
          strActTyp = 'Provider';
       EndSL;
       //*********** beg chg ******02/14/14  *************************
       //  If EFtype = 'D';
       Select;
       When EFtype = 'D';
       //*** Begin Chg *** 05/05/2015 *****************************************
       //*** If EFtype = 'D' or EFtype = 'C';
       //*** End   Chg *** 05/05/2015 *****************************************
       //*********** end chg ******02/14/14  *************************
          Chain keyWDACL# pac00;
          FormatAddress(ACadr1:ACadr2:ACcty:ACst:ACzip:strDtlCAd1:strDtlCAd2:
           strDtlcsz1);
       //*** Begin Chg *** 05/05/2015 *****************************************
       When EFtype = 'C';
          Chain (%EditC(EFfein:'X')) pcd00;
          FormatAddress(CDadr1:CDadr2:CDcty:CDst:CDzip:strDtlCAd1:strDtlCAd2:
           strDtlcsz1);
       EndSl;
       //*** endif;
       //*** End   Chg *** 05/05/2015 *****************************************

       //*** Beg  Add *** 05/07/21 ********************************************
       intStRptDiv = GetStRptDv(EFco#:EFfnd:EFemp#);
       If EFdiv = intStRptDiv;
          strDftInfoEft = EFdftsrd;
       Else;
          strDftInfoEft = 'N';
       Endif;
       sysDate7 = DateTo7(SystemDate);
       isPreNote = RequiresPreNote(EFco#:EFfnd:strType:sysDate7);
       //*** End  Add *** 05/07/21 ********************************************
       outJSON = ' ';
       BuildJSON(outJSON :'OutPayeeName':%trim(EFpnam));
       BuildJSON(outJSON :'OutEftAdr1'  :%trim(strDtlCAd1));
       BuildJSON(outJSON :'OutEftAdr2'  :%trim(strDtlCAd2));
       BuildJSON(outJSON :'OutEftAdr3'  :%trim(strDtlcsz1));
       BuildJSON(outJSON :'OutEftAcTyp' :%trim(strAcTyp));
       BuildJSON(outJSON :'OutEftActTyp':%trim(strActTyp));
       BuildJSON(outJSON :'OutEFTDiv'   :%EditC(EFdiv:'X'));
       BuildJSON(outJSON :'OutEFTFein'  :%EditC(EFfein:'X'));
       BuildJSON(outJSON :'OutEFTMod'   :%EditC(EFmod:'X'));
       BuildJSON(outJSON :'OutEFTEffDt' :%EditW(dtMDYY(EFefdt):'  /  /    '));
       BuildJSON(outJSON :'OutEFTExpDt' :%EditW(dtMDYY(EFexdt):'  /  /    '));
       //*** Beg  Add  *** 05/07/21 ********************************************
       If isPrenote = '1';
          BuildJSON(outJSON :'LblEftApprovedDate' :'Approved Date');
          BuildJSON(outJSON :'LblEftPreNoteDate' :'Pre-note Date');
       Else;
          BuildJSON(outJSON :'LblEftApprovedDate' :' ');
          BuildJSON(outJSON :'LblEftPreNoteDate' :' ');
       Endif;
       //*** End  Add   *** 05/07/21 ********************************************
       BuildJSON(outJSON :'OutEFTPNtDt' :%EditW(dtMDYY(EFpdt):'  /  /    '));
       BuildJSON(outJSON :'OutEFTAprDt' :%EditW(dtMDYY(EFadt):'  /  /    '));
       BuildJSON(outJSON :'OutEFTFTrDt' :%EditW(dtMDYY(EFftdt):'  /  /    '));
       BuildJSON(outJSON :'OutEFTLTrDt' :%EditW(dtMDYY(EFltdt):'  /  /    '));

       //*** Beg  Add *** 05/07/21 ********************************************
       BuildJSON(outJSON :'LblDftInfoEft' :%trim(strDftInfoEft));
       BuildJSON(outJSON :'LblAccountType' :%trim(strAccountType));
       //*** End  Add *** 05/07/21 ********************************************
       RtnCode = ' ';
       Return;
      /End-Free
