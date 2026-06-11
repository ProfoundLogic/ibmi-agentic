     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('STBNDDIR')
     H dftactgrp(*no)
     //********************************************************************
    H/TITLE LODSTSPP - Load data for STSPP
     //********************************************************************
     // Created: 11/03/15    C:33    Project: SY371                       *
     //********************************************************************
      // Revisions:                                                        *
      //                                                                   *
      // 01/17/25 C:291 Project: WC2494                                    *
      //                Convert Deductible Invoice to Scriptura.           *
      //                - Update Deductible Invoice settings for Scriptura.*
      //                - Remove Deductible Summary Invoice,               *
      //                  Single Employer Deductible Detail Report and     *
      //                  Mass Employer Deductible Detail Report from      *
      //                  all the arrays.                                  *
      //                                                                   *
      // 01/09/25 C:291 Project: 148WFI                                    *
      //                Add Loss Prevention notice for AR for 11/24 edition*
      //                                                                   *
      // 05/30/24 C:291 Project: WC2670                                    *
      //                Changed Status array for Scriptura Bills and Premium
      //                notice with Active status for release V24R0.       *
      //                                                                   *
      // 01/26/24 C:291 Project: WC2670                                    *
      //                Add default Template of Notice of Election of     *
      //                Retrospective Rating.                              *
      //                                                                   *
      // 01/12/24 C:291 Project: WC2670                                    *
      //                Set BLTYPE to Q/E/R/A based on type of Bill.       *
      //                                                                   *
      // 08/08/23 C:333 Support: 113248 (LUBA)                             *
      //                V22R2 Sapiens Go - Premium Notice setup            *
      //                - changing status array from 'A' to ' ' for        *
      //                   'Premium Notice'                                *
      //                                                                   *
      // 08/02/23 C:300 Support: 113343 (AmWINS)                           *
      //                LODSTSPP wrote NY records in error for CX/RE config*
      //                So, Remove 148WEF changes for NY state.            *
      //                                                                   *
      // 05/26/23 C:291 Project: WC2875                                    *
      //                Delete Company zero records.                       *
      //                Remove AG Category. AG category is replaced by     *
      //                AS category.                                       *
      //                                                                   *
      // 05/10/23 C:62  Project: 148WEF                                    *
      //                Add CX/RE state records.                           *
      //                                                                   *
      // 05/10/23 C:65  Project: WC2838                                    *
      //                Clear SPPATHEXT so its value does not get copied   *
      //                over from the Rule Report record.                  *
      //                                                                   *
      // 07/05/22 C:333 Support: 102623                                    *
      //                Application Configuration-Output Control Categories*
      //                - checking Key/desc in STGTP. Accordingly perform  *
      //                  update/write operation.                          *
      //                                                                   *
      // 04/28/22 C:136 Project: WC2670                                    *
      //                Convert bills to Scriptura                         *
      //                                                                   *
      // 10/18/21 C:65  Project: WC2073                                    *
      //                Activate commission statements.                    *
      //                                                                   *
      // 10/07/21 C:65  Project: WC2278                                    *
      //                Enable email module in STSCP for Contacts.         *
      //                                                                   *
      // 07/16/21 C:65  Project: 102WIO                                    *
      //                Activate certificate of insurance type.            *
      //                                                                   *
      // 06/10/21 C:298 Project: WC2278                                    *
      //                Add new array for new field 'Category' processing. *
      //                                                                   *
      // 05/10/21 C:65  Project: WC2760                                    *
      //                Add new array to indicate process has a report.    *
      //                                                                   *
      // 03/12/21 C:300 Support: 94988 (In-House)                          *
      //                Billing Invoice system process record invalid      *
      //                This was a Sunz only record, they cancelled/removed*
      //                that project. This record was loaded for everyone  *
      //                erroneously.                                       *
      //                                                                   *
      // 11/10/20 C:62  Project: WC2760                                    *
      //                Add S/R Late Notice                                *
      //                                                                   *
      // 10/28/20 C:79  Project: WC2671                                    *
      //                Add Audit Request                                  *
      //                                                                   *
      // 07/09/20 C:65  Project: WC2073                                    *
      //                Update data on Commission Statements record.       *
      //                Remove replacement template default record if      *
      //                process is not live yet, to replace it.            *
      //                                                                   *
      // 01/08/20 C:65  Project: SY408 - Portal                            *
      //               -Remove state records from FROI template setup file,*
      //                only a single default record will be created with  *
      //                the updated template name.                         *
      //                                                                   *
      // 05/10/19 C:136 Project: SY407                                     *
      //               -Activate Self-Reporting Worksheet record           *
      //                                                                   *
      // 12/19/18 C:65  Project: SY390 - Portal                            *
      //               -Add processing record for ACORD130 form.           *
      //                                                                   *
      // 02/22/18 C:267 Project: 150WBV                                    *
      //               -Add Billing Invoice entry/processing               *
      //                                                                   *
      // 07/24/17 C:33  Project: WC2405                                    *
      //               -Add ability to skip procesing messages             *
      //               -Do not delete 'X' status                           *
      //                                                                   *
      // 07/19/17 C:053 Project: WC2499
      //                Add FROI entry/processing.
      //
      // 07/18/17 C:65  Project: WC2492  (changed from 07/19/17)           *
      //                Create default table and template records for      *
      //                MI POC notice.                                     *
      //                                                                   *
      // 06/28/17 C:33  Project: WC2405                                    *
      //               -Add 'Rule Report'                                  *
      //               -Move Retired processing to seperate loop           *
      //                                                                   *
      // 06/26/17 C:136 Project: CI143                                     *
      //                Replace 'Monthly Interim' with 'Self-Reporting     *
      //                Worksheet'                                         *
      //                                                                   *
      // 05/05/17 C:65  Project: SY371                                     *
      //                Change wintel directory default.                   *
      //                                                                   *
      // 03/30/17 C:62  Project: 148WAG                                    *
      //                Carrier Policy # Report                            *
      //                                                                   *
      // 03/10/17 C:65  Project: SY371                                     *
      //                Activate policy page, cancellation/reinstatement,  *
      //                mid-term endorsements, and estimated late notices. *
      //                                                                   *
      // 03/09/17 C:65  Project: SY371/148WAD                              *
      //                Add setup AMWINS certificates of insurance -       *
      //                RI, NY, and CA.                                    *
      //                                                                   *
      // 03/08/17 C:65  Project: SY371                                     *
      //                Add setup for certificate of insurance.            *
      //                                                                   *
      // 03/06/17 C:65  Project: WC2481                                    *
      //                Add records for MO and MT loss control notices.    *
      //                                                                   *
      // 02/27/17 C:65  Project: SY371                                     *
      //                Create CompSuite Output directory for user folders.*
      //                                                                   *
      // 02/26/17 C:65  Project: SY371                                     *
      //                Change path for estimated late notice to reports.  *
      //                                                                   *
      // 01/27/17 C:65  Project: SY371                                     *
      //                Add default records to policy page notices file    *
      //                for VA and DE notices, and add entries to system   *
      //                table file for types of notices available for      *
      //                setup.                                             *
      //                                                                   *
      // 01/23/17 C:65  Project: SY371                                     *
      //                Default fields on CX/RE setup record.              *
      //                                                                   *
      // 01/19/17 C:65  Project: SY371                                     *
      //                Do not write the CA entries for policy page.       *
      //                                                                   *
      // 01/10/17 C:65  Project: SY371                                     *
      //                Add entries for estimated late notice.             *
      //                                                                   *
      // 12/19/16 C:65  Project: SY371                                     *
      //                Create a cancellation and reinstatement record in  *
      //                the cx/re file - main template file is no longer   *
      //                used.                                              *
      //                                                                   *
      // 12/16/16 C:65  Project: SY371                                     *
      //                Create a policy page setup record for declaration  *
      //                page - main template file is no longer used.       *
      //                                                                   *
      // 12/01/16 C:136 Project: SY371                                     *
      //                Add Mid-Term Endorsement process type              *
      //                                                                   *
      // 11/16/16 C:65  Project: SY371                                     *
      //                Create CA versions of declaration page and         *
      //                schedules.                                         *
      //                                                                   *
      // 10/12/16 C:65  Project: SY371                                     *
      //                Add records to STSPP for Cancellations and         *
      //                Reinstatements.                                    *
      //                                                                   *
      // 10/11/16 C:62  Project: SY371                                     *
      //                Add records to STSPP for Reserve Worksheet Report  *
      //                                                                   *
      // 09/21/16 C:62  Project: SY371                                     *
      //                Add records to STSPP for Payment History Report    *
      //                                                                   *
      // 09/16/16 C:65  Project: SY371                                     *
      //                Add records to WSPPP for Policy Page Schedules and *
      //                Loss Prevention notices if no 1/1/1999 records     *
      //                exist.  Add the data program array to the update   *
      //                subroutine and populate the policy page extra      *
      //                setup program.                                     *
      //                                                                   *
      // 09/09/16 C:65  Project: SY371                                     *
      //                Create a record in STSP1P for each process if it   *
      //                does not have a 1/1/1999 record (for the original  *
      //                template).  Templates are pulled from STSP1P       *
      //                always now.                                        *
      //                                                                   *
      // 09/06/16 C:65  Project: SY371                                     *
      //                Add array to allow processes to be initially       *
      //                flagged to produce Scriptura output.               *
      //                                                                   *
      // 08/09/16 C:308 Project: SY375                                     *
      //                Change template path to /Reports/Base/System/      *
      //                                                                   *
      // 07/01/16 C:308 Project: SY375                                     *
      //                System Security Report.                            *
      //                Add record to all arrays at 9th position           *
      //                                                                   *
      // 06/29/16 C:65  Project: SY371                                     *
      //                Populate secure signature temp folders into STSCP. *
      //                                                                   *
      // 06/13/16 C:65  Project: SY371                                     *
      //                Add single and mass process flags to file and      *
      //                update existing active records with info.  Add     *
      //                an array for additional setup program call.        *
      //                                                                   *
      // 04/28/16 C:65  Project: SY371                                     *
      //                Add status field and array to ensure records       *
      //                which show up are truely ready for release.  If a  *
      //                record is found with a blank status, delete it.    *
      //                                                                   *
      // 04/04/16 C:65  Project: SY371                                     *
      //                Add more arrays for extra data program,            *
      //                interactive process, and default template/path to  *
      //                be populated into setup records.                   *
      //                                                                   *
     //********************************************************************
     Fstspl     uf a e           k disk    rename(psp01:psp00)
     Fstscp     uf   e             disk    rename(psc01:psc0p)
      //** Beg Chg ***** 06/26/17 ********************************************
      //*** fstsp1l    if a e           k disk    rename(psp011:psp11l)
     fstsp1l    uf a e           k disk    rename(psp011:psp11l)
      //** End Chg ***** 06/26/17 ********************************************
     fwsppl     uf a e           k disk    rename(ppp01:ppp00) usropn           wcms - policy page
     fwscrl     if a e           k disk    rename(pcr01:pcr00) usropn           wcms - cx/re
      //** Begin Add *** 01/27/17 ********************************************
      //** Beg Chg *** 07/05/22 **********************************************
     f*///stgtl     if a e           k disk    rename(pgt01:pgt00)
     fstgtl     Uf a e           k disk    rename(pgt01:pgt00)
      //** End Chg *** 07/05/22 **********************************************
      //** End Add  **** 01/27/17 ********************************************
      //** Begin Add *** 03/08/17 ********************************************
     fwscil     if a e           k disk    rename(pci01:pci00) usropn           wcms - cert of ins
      //** End Add  **** 03/08/17 ********************************************
      //** Begin Add *** 07/19/17 ********************************************
      //*** begin chg ***  01/08/20  *****************************************
     f*****wsffl     if a e           k disk    rename(pff01:pff00) usropn           FROI State Form
     fwsffl     uf a e           k disk    rename(pff01:pff00) usropn           FROI State Form file
      //*** end chg *****  01/08/20  *****************************************
     fwmixl     if a e           k disk    rename(pix01:pix00) usropn           FROI State Form file
      //** End Add  **** 07/19/17 ********************************************
      //*** beg add  ***** 04/28/22 ************************
     fwsbl1l    if a e           k disk    rename(pbl01:pb01l)
      //*** end add  ***** 04/28/22 ************************

     D#ofRcds          c                   100
     DaddKey           s            100    dim(#ofRcds) ctdata perrcd(1)
     DretireKey        s            100    dim(#ofRcds) ctdata perrcd(1)
     DdataPgm          s             10    dim(#ofRcds) ctdata perrcd(1)
     Dinteract         s              1    dim(#ofRcds) ctdata perrcd(1)
     Dtemplate         s             60    dim(#ofRcds) ctdata perrcd(1)
     Dpath             s            100    dim(#ofRcds) ctdata perrcd(1)
     Dstatus           s              1    dim(#ofRcds) ctdata perrcd(1)
     Dsingle           s              1    dim(#ofRcds) ctdata perrcd(1)
     Dmass             s              1    dim(#ofRcds) ctdata perrcd(1)
     DsetupPgm         s             10    dim(#ofRcds) ctdata perrcd(1)
     DscriptFlag       s              1    dim(#ofRcds) ctdata perrcd(1)
     DhideRepGrid      s              1    dim(#ofRcds) ctdata perrcd(1)
     D***  Begin Add  ***  07/24/17  **************************************
     Dflags            s                   like(dsFlags)
     D                                     dim(#ofRcds) ctdata perrcd(1)
     D***  begin add  ***  05/10/21  **************************************
     Dreport           s              1    dim(#ofRcds) ctdata perrcd(1)
     DreportType       s                   like(SPType)
     D***  end add ******  05/10/21  **************************************
      ***** Beg Add ***** 06/10/21 ****************************************
     Dcategory         s              2    dim(#ofRcds) ctdata perrcd(1)
      ***** End Add ***** 06/10/21 ****************************************

      ***** Beg Add ***** 07/05/22 ****************************************
     DTKEY             S             10    DIM(3) CTDATA PERRCD(1)
     DTDES             S             40    DIM(3) ALT(TKEY)
      ***** Chg Add ***** 07/05/22 ****************************************

     DdsFlags          ds                  qualified
     D msgInt                              like(SPmsgInt)
     D msgBch                              like(SPmsgBch)
     D***  End   Add  ***  07/24/17  **************************************

     Di1               s              3  0
     D***  Begin Add  ***  07/24/17  **************************************
     Di2               s              3  0
     D***  End   Add  ***  07/24/17  **************************************
     D***  Beg Add  ***  07/05/22  **************************************
     Di3               s              3  0
     D***  End Add  ***  07/05/22  **************************************
     DsysTime          s                   like(SPtime)
     DsysDate          s                   like(SPdt)
     DintCo#           s              3  0
     d*** begin add ***  02/27/17  ****************************************
     D userFolderBase  c                   const('/CompSuite Output/')
     d*** end add *****  02/27/17  ****************************************
     D tempFolderBase  c                   const('/SCR_DTM/temp/')
     D winFolderBase   c                   const('file:/D:/tmp/image/')
     D**** begin add ***  05/05/17  *****************************************
     D winFolderBase2  c                   const('file:/C:/tmp/image/')
     D**** end add *****  05/05/17  *****************************************
     D tempFolder      s            250
     D xxFolder        s            250
     d strCommand      s            255
     d suffix          s              3
      *** Beg Add ***** 05/26/23 ********************************
     d intCoZero       s              3  0 Inz(0)
      *** End Add ***** 05/26/23 ********************************
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d   command                           const like(strCommand)
     d   length                      15  5 const
     d SA030R          pr                  ExtPgm('SA030R')
     d   $#SUFX_                      3
     d   $#FLRP_                     80    options(*nopass)
     D**** Begin Add *** 01/27/17 ********************************************
     D xxTbid          s                   like(GTTbid)
     D xxKey           s                   like(GTKey)
     D xxDesc          s                   like(GTDesc)
     D xxNoteType      s                   like(GTFG01)
     D**** End Add  **** 01/27/17 ********************************************
      *** beg add ***** 06/26/17 ***************************************
     d deleteKey       s            100
      *** end add ***** 06/26/17 ***************************************
      //*** Beg Add ***** 01/09/25 ****************************************
     D svPolDocC       s                   like(PPPOLDOCC)
     D svPolPthC       s                   like(PPPOLPTHC)
      //*** End Add ***** 01/09/25 ****************************************
     d** Begin Add *** 03/09/17 ********************************************
     d clientID        s              3    inz('000')
      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d** End Add  **** 03/09/17 ********************************************

     D psds          esds                  extname(szq1p)

     DhasWCMS          s              1n
     D/copy sPrModule
     D/copy sPrDate
     d/copy sprUsrPrms

      /Free
       sysTime = TimeTo6(SystemTime);
       sysDate = DateTo7(SystemDate);
       //** Begin Add *** 03/09/17 ********************************************
       GetClientID(clientID);
       //** End Add ***** 03/09/17 ********************************************
       //*** begin add ***  02/27/17  ****************************************
       Exsr CreateUserFold;
       //*** end add *****  02/27/17  ****************************************
       Read psc0p;
       If not %eof;
          If SCSigTemp = ' ';
             SCSigTemp = %trim(tempFolderBase);
             xxFolder = %trim(SCSigTemp);
             Exsr CreateFold;
          Endif;
       //**** begin add ***  05/05/17  *****************************************
       //*****   If SCSigDir = ' ';
       //*****      SCSigDir  = %trim(winFolderBase);
          If SCSigDir = ' ' or SCSigDir = %trim(winFolderBase);
       //**** end add *****  05/05/17  *****************************************
             SCSigDir  = %trim(winFolderBase2);
          Endif;
       //*** begin add ***  10/07/21  ******************************************
       //* Activate Email Module for Contacts
          SCFlg2 = 'Y';
       //*** end add *****  10/07/21  ******************************************
          Update psc0p;
       Endif;
       //** Begin Add *** 01/27/17 ********************************************
       intCo# = GetUserInfo(q1user);
       Exsr WriteScripDef;
       //** End Add  **** 01/27/17 ********************************************

       // ***** Beg Add ***** 06/10/21 ****************************************
       xxTbid = 'DOCS';
       xxKey  = 'AG';
       //*** Beg Chg ***** 05/26/23 **************************
       //Remove AG category record as it is replaced with AS category
       //***Chain(n) (intCo#:xxTbid:xxKey) pgt00;
       //***If not %found;
         //*** Clear pgt00;
         //*** GTCo#  = intCo#;
         //*** GTTBID = xxTBID;
         //*** GTKEY  = xxKey;
         //*** GTSTAT = 'A';
         //*** GTDESC = 'Agency';
         //*** GTPgm  = Q1PGM;
         //*** GTUser = Q1USER;
         //*** GTTime = Timeto6(SystemTime());
         //*** GTDt   = Dateto7(SystemDate());
         //*** Write pgt00;
       //***Endif;
       Chain (intCo#:xxTbid:xxKey) pgt00;
       If %Found;
          Delete pgt00;
       Endif;
       //*** End Chg ***** 05/26/23 **************************

       xxKey  = 'RP';
       Chain(n) (intCo#:xxTbid:xxKey) pgt00;
       If not %found;
          Clear pgt00;
          GTCo#  = intCo#;
          GTTBID = xxTBID;
          GTKEY  = xxKey;
          GTSTAT = 'A';
          GTDESC = 'Report';
          GTPgm  = Q1PGM;
          GTUser = Q1USER;
          GTTime = Timeto6(SystemTime());
          GTDt   = Dateto7(SystemDate());
          Write pgt00;
       Endif;
       // ***** End Add ***** 06/10/21 ****************************************

       // ***** Beg Add ***** 07/05/22 ****************************************
       For i3 = 1 to 3;
          xxKEY = TKEY(i3);
          xxTBID= 'DOCS';
          Chain (intCo#:xxTbid:xxKey) pgt00;
          If %found;                          //// Key present in STGTP
             If GTDESC <> TDES(i3);           //Check for latest Description
                exsr updgtp;
             Endif;
          Else;
             exsr wrtgtp;
          EndIf;
       EndFor;
       // ***** End Add ***** 07/05/22 ****************************************

       For i1 = 1 to #ofRcds;
          Chain addKey(i1) psp00;
        //***  Begin Chg  ***  07/24/17  **********************************
        //If %found and addKey(i1) <> *blanks and SPStat <> 'A';
        //   Delete psp00;
        //   Chain addKey(i1) psp00;
        //Else;
        //   If %found and addKey(i1) <> *blanks and SPStat = 'A';
        //      Exsr UpdateRecord;
        //   Endif;
        //Endif;
        //If not %found and addKey(i1) <> *blanks;
        //   Exsr WriteRecord;
        //Endif;
          Select;
          When not %found and addKey(i1) <> *blanks;
             Exsr WriteRecord;
          When %found and addKey(i1) <> *blanks and SPstat = 'A';
             Exsr UpdateRecord;
          When %found and addKey(i1) <> *blanks and SPstat = 'X';
             Exsr UpdateRecord;
          When %found and addKey(i1) <> *blanks;
             Delete psp00;
        //***  begin add  ***  07/09/20  **********************************
             Chain addKey(i1) psp11l;
             If %found;
                Delete psp11l;
             Endif;
        //***  end add ******  07/09/20  **********************************
             Exsr WriteRecord;
          Endsl;
        //***  End   Chg  ***  07/24/17  **********************************
          Chain (addKey(i1):990101) psp11l;
          If not %found and addKey(i1) <> *blanks;

             Clear psp11l;
             S1Type    = addKey(i1);
             S1Efdt    = 990101;
             S1SDefT   = SPSDefT;
             S1PathD   = SPPathD;
             S1LangD   = SPLangD;
             S1SCltT   = SPSCltT;
             S1PathC   = SPPathC;
             S1LangC   = SPLangC;
             S1Sig     = SPSig;
             S1SigFold = SPSigFold;
             S1Stat    = 'A';
             S1IUsr    = Q1User;
             S1IDt     = Dateto7(SystemDate());
             S1Pgm     = Q1Pgm;
             S1User    = Q1User;
             S1Time    = Timeto6(SystemTime());
             S1Dt      = Dateto7(SystemDate());
             Write psp11l;

          Endif;
        //***  begin add  ***  05/10/21  **********************************
          reportType = %trim(addKey(i1)) + ' Report';
          Chain (reportType:990101) psp11l;
          If not %found and addKey(i1) <> *blanks and report(i1) = 'Y';

             Clear psp11l;
             S1Type    = reportType;
             S1Efdt    = 990101;
             S1SDefT   = %subst(SPSDefT:1:%scan('.':SPSDefT)-1) +
              ' Report.scd';
             //*** Beg Add ***** 01/17/25 ****************************
             If addKey(i1) = 'Deductible Invoice';
                S1SDefT = 'Deductible Detail Report.scd';
             Endif;
             //*** End Add ***** 01/17/25 ****************************
             S1PathD   = SPPathD;
             S1LangD   = SPLangD;
             S1SCltT   = SPSCltT;
             S1PathC   = SPPathC;
             S1LangC   = SPLangC;
             S1Sig     = SPSig;
             S1SigFold = SPSigFold;
             S1Stat    = 'A';
             S1IUsr    = Q1User;
             S1IDt     = Dateto7(SystemDate());
             S1Pgm     = Q1Pgm;
             S1User    = Q1User;
             S1Time    = Timeto6(SystemTime());
             S1Dt      = Dateto7(SystemDate());
             Write psp11l;

          Endif;
        //***  end add ******  05/10/21  **********************************
         hasWCMS = IsModEnabled('WorkComp');

         //* Add policy page records if client has WCMS.
         If hasWCMS and addKey(i1) = 'Policy Page';
            Exsr AddPolicyPage;
         Endif;
         //* Add policy page records if client has WCMS.
         If hasWCMS and addKey(i1) = 'Cancellations';
            Exsr AddCancRein;
         Endif;
         //** Begin Add *** 03/08/17 ********************************************
         //* Add policy page records if client has WCMS.
         If hasWCMS and addKey(i1) = 'Certificate of Insurance';
            Exsr AddCertData;
         Endif;
         //** End Add ***** 03/08/17 ********************************************
         //** Begin Add *** 07/19/17 ********************************************
         //* Add FROI records if client has WCMS.
         If hasWCMS and addKey(i1) = 'First Report of Injury';
         //*** begin add ***  01/08/20  *****************************************
            //* Remove setup records if process not delivered yet.
            If SPStat <> 'A';
               Open wsffl;
               Read pff00;
               Dow not %eof;
                  Delete pff00;
                  Read pff00;
               Enddo;
               Close wsffl;
            Endif;
         //*** end add *****  01/08/20  *****************************************
            Exsr AddFROIRecord;
         //*** begin add ***  01/08/20  *****************************************
         //* no state records yet.
         //*****   Exsr AddStFROIRecord;
         //*** end add *****  01/08/20  *****************************************
         Endif;
         //** End Add ***** 07/19/17 ********************************************
          Chain retireKey(i1) psp00;
          If %found;
             Exsr RetireRecord;
          Endif;
         //*** beg add ***** 04/28/22 *********************
          If hasWCMS And
           (addKey(i1) = 'Quote Bill' Or
           addKey(i1) = 'Estimated Bill' Or
           addKey(i1) = 'Revised Bill' Or
           addKey(i1) = 'Audit Bill');
             Exsr WriteWSBL1L;
          Endif;
         //*** end add ***** 04/28/22 *********************
       Endfor;

       //***  Begin Add  ***  06/28/17  ***********************************
       For i1 = 1 to #ofRcds;
          Chain retireKey(i1) psp00;
          If %found;
             Exsr RetireRecord;
          Endif;
       Endfor;
       //***  End   Add  ***  06/28/17  ***********************************
       //*** beg add ***** 06/26/17 **************
       Exsr DeleteRecords;
       //*** end add ***** 06/26/17 **************

       //*** Beg Add ***** 05/26/23 **************
       Exsr DelZeroRecords;
       //*** End Add ***** 05/26/23 **************

       *inlr = *on;

       //*** beg add ***** 06/26/17 **************
       //====================================================================
       // Delete records
       //====================================================================
       Begsr DeleteRecords;
          deleteKey = 'Monthly Interim';
          Delete deleteKey psp00;
          Delete deleteKey psp11l;
          //*** beg add ***** 03/12/21 **************
          deleteKey = 'Billing Invoice';
          Delete deleteKey psp00;
          Delete deleteKey psp11l;
          //*** end add ***** 03/12/21 **************
       Endsr;
       //*** end add ***** 06/26/17 **************

       //*** Beg Add ***** 05/26/23 **************
       //====================================================================
       // Delete Company Zero records
       //====================================================================
       Begsr DelZeroRecords;
         Setll intCoZero pgt00;
         Reade intCoZero pgt00;
         Dow not %Eof(stgtl);
          Delete pgt00;
          Reade intCoZero pgt00;
         Enddo;
       Endsr;
       //*** End Add ***** 05/26/23 **************

       //***  Begin Add  ***  07/24/17  *********************************
       //====================================================================
       // Load flags
       //====================================================================
       Begsr LoadFlags;
         Clear SPmsgInt;
         Clear SPmsgBch;
         dsFlags = flags(i1);
         SPmsgInt = dsFlags.msgInt;
         SPmsgBch = dsFlags.msgBch;
       Endsr;
       //***  End   Add  ***  07/24/17  *********************************

       //====================================================================
       // Retire records in STSPP
       //====================================================================
       Begsr RetireRecord;

         SPsplO = 'N';
         SPsplR = 'Y';

         SPuser = q1User;
         SPpgm  = q1Pgm;
         SPtime = sysTime;
         SPdt   = sysDate;

         Update psp00;

       Endsr;
       //*** beg add ***** 04/28/22 ***********************
       //==================================================
       // Write records to WSBL1L
       //==================================================
       Begsr WriteWSBL1L;
        BLco# = intCo#;
        BLfnd = 0;
        BLst = *Blanks;
        BLefdt = 990101;
        //*** Beg Chg ***** 01/12/24 **********************
        //*** BLtype = addKey(i1);
        BLtype = %Subst(addKey(i1):1:1);
        //*** End Chg ***** 01/12/24 **********************
        Chain (BLco#:BLtype:BLfnd:BLst:BLefdt) pb01l;
        If %Found;
           LeaveSr;
        Endif;
        BLdoc = template(i1);
        BLpath = path(i1);
        BLsumD = %ScanRpl('.scd':' Summary.scd':BLdoc);
        BLsumP = %ScanRpl('.scd':' Summary.scd':BLpath);
        //*** Beg Add ***** 01/26/24 ***************************
        If addKey(i1) = 'Quote Bill';
           BLnoteD = 'Notice of Election of Retrospective Rating.scd';
           BLnoteP = path(i1);
        Else;
           Clear BLnoteD;
           Clear BLnoteP;
        Endif;
        //*** End Add ***** 01/26/24 ***************************
        BLuser = q1User;
        BLpgm  = q1Pgm;
        BLtime = sysTime;
        BLdt = sysDate;
        BLiusr = q1User;
        BLipgm  = q1Pgm;
        BLitim = sysTime;
        BLidt = sysDate;
        Write pb01l;
       Endsr;
       //*** end add ***** 04/28/22 ***********************
       //====================================================================
       // Write records to STSPP
       //====================================================================
       Begsr WriteRecord;

         SPtype = addKey(i1);
         SPsplO = 'Y';
         SPsplR = 'N';
         SPscrO = 'N';
         SP3Xml = 'N';
         SPscrO = scriptFlag(i1);
         //*** begin add **** 05/10/23  ****************************************
         SPPathExt = ' ';
         //*** end add ****** 05/10/23  ****************************************

         SPhideRep = hideRepGrid(i1);

         SPDataP = dataPgm(i1);

         SPInter = interact(i1);

         SPSDefT = template(i1);

         SPPathD = path(i1);

         //***  Begin Add  ***  07/24/17  *********************************
         Exsr LoadFlags;
         //***  End   Add  ***  07/24/17  *********************************

         SPStat  = status(i1);

         If SPSDefT <> ' ';
            SPLangD = 'English';
         Endif;

         SPSingle = single(i1);
         SPMass   = mass(i1);
         SPSetup = setupPgm(i1);

         //***** Beg Add ***** 06/10/21 ****************************************
         SPcat = Category(i1);
         //***** End Add ***** 06/10/21 ****************************************

         SPuser = q1User;
         SPpgm  = q1Pgm;
         SPtime = sysTime;
         SPdt   = sysDate;
         SPiUsr = SPuser;
         SPiPgm = SPpgm;
         SPiTim = SPtime;
         SPiDt  = SPdt;

         Write psp00;

       Endsr;

       //====================================================================
       // Update records in STSPP for info which clients cannot change.
       //====================================================================
       Begsr UpdateRecord;

         SPSingle = single(i1);
         SPMass   = mass(i1);
         SPInter = interact(i1);
         SPSDefT = template(i1);
         SPPathD = path(i1);
         SPSetup = setupPgm(i1);
         SPDataP = dataPgm(i1);

         //***** Beg Add ***** 06/10/21 **************************************
         SPcat = category(i1);
         //***** End Add ***** 06/10/21 **************************************
         //***** Beg Add ***** 08/08/23 **************************************
         SPstat = status(i1);
         //***** End Add ***** 08/08/23 **************************************

         SPuser = q1User;
         SPpgm  = q1Pgm;
         SPtime = sysTime;
         SPdt   = sysDate;

         Update psp00;

       Endsr;
       //====================================================================
       // Create secure temp folder on IFS for signatures if it is populated.
       //====================================================================
       Begsr CreateFold;

          //* Build signature folder location and create if needed.

          Sa030r(suffix);

          If suffix <> ' ';
             tempFolder = '/PDS/' + suffix + %trim(xxFolder);
          Else;
             tempFolder = %trim(xxFolder);
          Endif;

          //* Attempt to create folder
          Monitor;
             clear strCommand;
             strCommand = 'MKDIR DIR(''' + %trim(tempFolder) + ''') DTAAUT(' +
              '*EXCLUDE) OBJAUT(*NONE)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

       Endsr;
       //====================================================================

       //*** begin add ***  02/27/17  ****************************************
       //====================================================================
       // Create base folder for user output on IFS
       //====================================================================
       Begsr CreateUserFold;

          //* Build folder location and create if needed.

          Sa030r(suffix);

          If suffix <> ' ';
             tempFolder = '/PDS/' + suffix + %trim(userFolderBase);
          Else;
             tempFolder = %trim(userFolderBase);
          Endif;

          //* Attempt to create folder
          Monitor;
             clear strCommand;
             strCommand = 'MKDIR DIR(''' + %trim(tempFolder) + ''') DTAAUT(' +
              '*INDIR) OBJAUT(*INDIR)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          //* Attempt to update scriptura profile permissions
          Monitor;
             clear strCommand;
             strCommand = 'CHGAUT OBJ(''' + %trim(tempFolder) + ''') USER(' +
              'SCRIPTURA) DTAAUT(*RWX) OBJAUT(*ALL)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          //* Attempt to update scriptura profile permissions
          Monitor;
             clear strCommand;
             strCommand = 'CHGAUT OBJ(''' + %trim(tempFolder) + ''') USER(' +
              'PDSSCRIP) DTAAUT(*RWX) OBJAUT(*ALL)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

       Endsr;
       //====================================================================
       //*** end add *****  02/27/17  ****************************************

       //====================================================================
       // Add records to policy page setup file.
       //====================================================================
       Begsr AddPolicyPage;

          Open wsppl;

       //* Declaration Page
          Chain ('S':'D':0:'  ':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'S';
             PPSType   = 'D';
             PPEfdt    = 990101;
             PPPolDoc  = 'Policy Information Page.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //* CA Declaration Page
          Chain ('S':'D':0:'CA':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPSt      = 'CA';
             PPType    = 'S';
             PPSType   = 'D';
             PPEfdt    = 990101;
             PPPolDoc  = 'Policy Information Page CA.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
       //*** begin del ***  01/19/17  ******************************************
       //****Write ppp00;
          Else;
             Delete ppp00;
       //*** end del *****  01/19/17  ******************************************
          Endif;

       //* Workplace Schedule
          Chain ('S':'W':0:'  ':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'S';
             PPSType   = 'W';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Workplaces.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;
       //* CA Workplace Schedule
          Chain ('S':'W':0:'CA':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPSt      = 'CA';
             PPType    = 'S';
             PPSType   = 'W';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Workplaces CA.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
       //*** begin del ***  01/19/17  ******************************************
       //****Write ppp00;
          Else;
             Delete ppp00;
       //*** end del *****  01/19/17  ******************************************
          Endif;

       //* Named Insureds Schedule
          Chain ('S':'N':0:'  ':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'S';
             PPSType   = 'N';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Named Insureds.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;
       //* CA Named Insureds Schedule
          Chain ('S':'N':0:'CA':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPSt      = 'CA';
             PPType    = 'S';
             PPSType   = 'N';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Named Insureds CA.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
       //*** begin del ***  01/19/17  ******************************************
       //****Write ppp00;
          Else;
             Delete ppp00;
       //*** end del *****  01/19/17  ******************************************
          Endif;

       //* Premium Schedule
          Chain ('S':'P':0:'  ':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'S';
             PPSType   = 'P';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Classifications.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;
       //* Premium Schedule
          Chain ('S':'P':0:'CA':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPSt      = 'CA';
             PPType    = 'S';
             PPSType   = 'P';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Classifications CA.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
       //*** begin del ***  01/19/17  ******************************************
       //****Write ppp00;
          Else;
             Delete ppp00;
       //*** end del *****  01/19/17  ******************************************
          Endif;

       //* Endorsements Schedule
          Chain ('S':'E':0:'  ':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'S';
             PPSType   = 'E';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Endorsements.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;
       //* CA Endorsements Schedule
          Chain ('S':'E':0:'CA':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPSt      = 'CA';
             PPType    = 'S';
             PPSType   = 'E';
             PPEfdt    = 990101;
             PPPolDoc  = 'Schedule of Endorsements CA.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
       //*** begin del ***  01/19/17  ******************************************
       //****Write ppp00;
          Else;
             Delete ppp00;
       //*** end del *****  01/19/17  ******************************************
          Endif;

       //* Loss Prevention Notice
          Chain ('N':' ':0:'AR':990101) ppp00;
       //** Begin Add *** 01/27/17 ********************************************
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'AR':990101) ppp00;
       //** End Add ***** 01/27/17 ********************************************
          If not %found;
             Clear ppp00;
             PPType    = 'N';
       //** Begin Add *** 01/27/17 ********************************************
             PPSType   = 'L';
       //** End Add ***** 01/27/17 ********************************************
             PPSt      = 'AR';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice AR.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //* Loss Prevention Notice
          Chain ('N':' ':0:'KS':990101) ppp00;
       //** Begin Add *** 01/27/17 ********************************************
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'KS':990101) ppp00;
       //** End Add ***** 01/27/17 ********************************************
          If not %found;
             Clear ppp00;
             PPType    = 'N';
       //** Begin Add *** 01/27/17 ********************************************
             PPSType   = 'L';
       //** End Add ***** 01/27/17 ********************************************
             PPSt      = 'KS';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice KS.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //*** begin add ***  03/06/17  *****************************************

       //* Loss Prevention Notice
          Chain ('N':' ':0:'MO':990101) ppp00;
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'MO':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'N';
             PPSType   = 'L';
             PPSt      = 'MO';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice MO.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //* Loss Prevention Notice
          Chain ('N':' ':0:'MT':990101) ppp00;
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'MT':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'N';
             PPSType   = 'L';
             PPSt      = 'MT';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice MT.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //*** end add *****  03/06/17  *****************************************

       //* Loss Prevention Notice
          Chain ('N':' ':0:'OK':990101) ppp00;
       //** Begin Add *** 01/27/17 ********************************************
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'OK':990101) ppp00;
       //** End Add ***** 01/27/17 ********************************************
          If not %found;
             Clear ppp00;
             PPType    = 'N';
       //** Begin Add *** 01/27/17 ********************************************
             PPSType   = 'L';
       //** End Add ***** 01/27/17 ********************************************
             PPSt      = 'OK';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice OK.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //* Loss Prevention Notice
          Chain ('N':' ':0:'OR':990101) ppp00;
       //** Begin Add *** 01/27/17 ********************************************
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'OR':990101) ppp00;
       //** End Add ***** 01/27/17 ********************************************
          If not %found;
             Clear ppp00;
             PPType    = 'N';
       //** Begin Add *** 01/27/17 ********************************************
             PPSType   = 'L';
       //** End Add ***** 01/27/17 ********************************************
             PPSt      = 'OR';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice OR.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //* Loss Prevention Notice
          Chain ('N':' ':0:'PA':990101) ppp00;
       //** Begin Add *** 01/27/17 ********************************************
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'PA':990101) ppp00;
       //** End Add ***** 01/27/17 ********************************************
          If not %found;
             Clear ppp00;
             PPType    = 'N';
       //** Begin Add *** 01/27/17 ********************************************
             PPSType   = 'L';
       //** End Add ***** 01/27/17 ********************************************
             PPSt      = 'PA';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice PA.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //* Loss Prevention Notice
          Chain ('N':' ':0:'TX':990101) ppp00;
       //** Begin Add *** 01/27/17 ********************************************
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'TX':990101) ppp00;
       //** End Add ***** 01/27/17 ********************************************
          If not %found;
             Clear ppp00;
             PPType    = 'N';
       //** Begin Add *** 01/27/17 ********************************************
             PPSType   = 'L';
       //** End Add ***** 01/27/17 ********************************************
             PPSt      = 'TX';
             PPEfdt    = 990101;
             PPPolDoc  = 'Loss Prevention Notice TX.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //** Begin Add *** 01/27/17 ********************************************

       //* Deductible Opt In Form
          Chain ('N':'D':0:'DE':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'N';
             PPSType   = 'D';
             PPSt      = 'DE';
             PPEfdt    = 990101;
             PPPolDoc  = 'Deductible Opt In Form DE.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;

       //* Policy Notice Form
          Chain ('N':'P':0:'VA':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'N';
             PPSType   = 'P';
             PPSt      = 'VA';
             PPEfdt    = 990101;
             PPPolDoc  = 'Policy Notice Form VA.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;
       //** End Add ***** 01/27/17 ********************************************

       //** Begin Add *** 07/18/17 ********************************************

       //* MI POC Notice Form
          Chain ('N':'C':0:'MI':990101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'N';
             PPSType   = 'C';
             PPSt      = 'MI';
             PPEfdt    = 990101;
             PPPolDoc  = 'Proof of Coverage MI.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;
       //** End Add ***** 07/18/17 ********************************************

          //*** Beg Add ***** 01/09/25 *****************************************
          //*** Loss Prevention Notice for AR for 11/24 edition
          Clear svPolDocC;
          Clear svPolPthC;
          Setll ('N':'L':0:'AR':1241101) ppp00;
          Readpe ('N':'L':0:'AR') ppp00;
          If Not %Eof(wsppl);
             svPolDocC = PPPOLDOCC;
             svPolPthC = PPPOLPTHC;
          Endif;

          Chain ('N':' ':0:'AR':1241101) ppp00;
          If %found;
             PPSType = 'L';
             Update ppp00;
          Endif;
          Chain ('N':'L':0:'AR':1241101) ppp00;
          If not %found;
             Clear ppp00;
             PPType    = 'N';
             PPSType   = 'L';
             PPSt      = 'AR';
             PPEfdt    = 1241101;
             PPPolDoc  = 'Loss Prevention Notice AR 1124.scd';
             PPPolPth  = '/Compliance/Base/Policy/';
             If svPolDocC <> *Blank;
                PPPolDocC = svPolDocC;
             Endif;
             If svPolPthC <> *Blank;
                PPPolPthC = svPolPthC;
             Endif;
             PPLangD   = 'English';
             PPIUsr    = Q1User;
             PPIDt     = Dateto7(SystemDate());
             PPPgm     = Q1Pgm;
             PPUser    = Q1User;
             PPTime    = Timeto6(SystemTime());
             PPDt      = Dateto7(SystemDate());
             Write ppp00;
          Endif;
       //*** End Add ***** 01/09/25 *****************************************

          Close wsppl;

       Endsr;
       //====================================================================
       //====================================================================
       // Add records to cancellation/reinstatement setup file.
       //====================================================================
       Begsr AddCancRein;

          intCo# = GetUserInfo(q1user);

          Open wscrl;

       //* Base Cancellation Form
          Chain (intCo#:0:'  ':990101) pcr00;
          If not %found;
             Clear pcr00;
             CRCo#     = intCo#;
             CRFnd     = 0;
             CREfdt    = 990101;
             CRCxDoc   = 'Cancel Notice.scd';
             CRCxPath  = '/Compliance/Base/Policy/';
             CRReDoc   = 'Reinstate Notice.scd';
             CRRePath  = '/Compliance/Base/Policy/';
             //*** begin add ***  01/23/17  *******************************************
             CRCXCR  = 'CX Cert Letter.scd';
             CRCXCRP = '/Compliance/Base/Policy/';
             CRCFCR  = 'CX Confirmation Cert Letter.scd';
             CRCFCRP = '/Compliance/Base/Policy/';
             CRRECR  = 'Rein Cert Letter.scd';
             CRRECRP = '/Compliance/Base/Policy/';
             //*** end add *****  01/23/17  *******************************************
             CRIUsr    = Q1User;
             CRIDt     = Dateto7(SystemDate());
             CRPgm     = Q1Pgm;
             CRUser    = Q1User;
             CRTime    = Timeto6(SystemTime());
             CRDt      = Dateto7(SystemDate());
             Write pcr00;
          Endif;

          //*** Begin Add *** 05/10/23 *************************************
          //* Florida Cancellation Form
          Chain (intCo#:0:'FL':990101) pcr00;
          If not %found;
             Clear pcr00;
             CRCo#     = intCo#;
             CRFnd     = 0;
             CRST      = 'FL';
             CREfdt    = 990101;
             CRCxDoc   = 'FL Cancel Notice.scd';
             CRCxPath  = '/Compliance/Base/Policy/';
             CRReDoc   = 'FL Reinstate Notice.scd';
             CRRePath  = '/Compliance/Base/Policy/';
             CRCXCR  = 'CX Cert Letter.scd';
             CRCXCRP = '/Compliance/Base/Policy/';
             CRCFCR  = 'CX Confirmation Cert Letter.scd';
             CRCFCRP = '/Compliance/Base/Policy/';
             CRRECR  = 'Rein Cert Letter.scd';
             CRRECRP = '/Compliance/Base/Policy/';
             CRIUsr    = Q1User;
             CRIDt     = Dateto7(SystemDate());
             CRPgm     = Q1Pgm;
             CRUser    = Q1User;
             CRTime    = Timeto6(SystemTime());
             CRDt      = Dateto7(SystemDate());
             Write pcr00;
          Endif;

          //* Georgia Cancellation Form
          Chain (intCo#:0:'GA':990101) pcr00;
          If not %found;
             Clear pcr00;
             CRCo#     = intCo#;
             CRFnd     = 0;
             CRST      = 'GA';
             CREfdt    = 990101;
             CRCxDoc   = 'Cancel Notice.scd';
             CRCxPath  = '/Compliance/Base/Policy/';
             CRReDoc   = 'GA Reinstate Notice.scd';
             CRRePath  = '/Compliance/Base/Policy/';
             CRCXCR  = 'CX Cert Letter.scd';
             CRCXCRP = '/Compliance/Base/Policy/';
             CRCFCR  = 'CX Confirmation Cert Letter.scd';
             CRCFCRP = '/Compliance/Base/Policy/';
             CRRECR  = 'Rein Cert Letter.scd';
             CRRECRP = '/Compliance/Base/Policy/';
             CRIUsr    = Q1User;
             CRIDt     = Dateto7(SystemDate());
             CRPgm     = Q1Pgm;
             CRUser    = Q1User;
             CRTime    = Timeto6(SystemTime());
             CRDt      = Dateto7(SystemDate());
             Write pcr00;
          Endif;

          //* Massachusetts Cancellation Form
          Chain (intCo#:0:'MA':990101) pcr00;
          If not %found;
             Clear pcr00;
             CRCo#     = intCo#;
             CRFnd     = 0;
             CRST      = 'MA';
             CREfdt    = 990101;
             CRCxDoc   = 'MA Cancel Notice.scd';
             CRCxPath  = '/Compliance/Base/Policy/';
             CRReDoc   = 'MA Reinstate Notice.scd';
             CRRePath  = '/Compliance/Base/Policy/';
             CRCXCR  = 'CX Cert Letter.scd';
             CRCXCRP = '/Compliance/Base/Policy/';
             CRCFCR  = 'CX Confirmation Cert Letter.scd';
             CRCFCRP = '/Compliance/Base/Policy/';
             CRRECR  = 'Rein Cert Letter.scd';
             CRRECRP = '/Compliance/Base/Policy/';
             CRIUsr    = Q1User;
             CRIDt     = Dateto7(SystemDate());
             CRPgm     = Q1Pgm;
             CRUser    = Q1User;
             CRTime    = Timeto6(SystemTime());
             CRDt      = Dateto7(SystemDate());
             Write pcr00;
          Endif;

          //* New Jersey Cancellation Form
          Chain (intCo#:0:'NJ':990101) pcr00;
          If not %found;
             Clear pcr00;
             CRCo#     = intCo#;
             CRFnd     = 0;
             CRST      = 'NJ';
             CREfdt    = 990101;
             CRCxDoc   = 'NJ Cancel Notice.scd';
             CRCxPath  = '/Compliance/Base/Policy/';
             CRReDoc   = 'NJ Reinstate Notice.scd';
             CRRePath  = '/Compliance/Base/Policy/';
             CRCXCR  = 'CX Cert Letter.scd';
             CRCXCRP = '/Compliance/Base/Policy/';
             CRCFCR  = 'CX Confirmation Cert Letter.scd';
             CRCFCRP = '/Compliance/Base/Policy/';
             CRRECR  = 'Rein Cert Letter.scd';
             CRRECRP = '/Compliance/Base/Policy/';
             CRIUsr    = Q1User;
             CRIDt     = Dateto7(SystemDate());
             CRPgm     = Q1Pgm;
             CRUser    = Q1User;
             CRTime    = Timeto6(SystemTime());
             CRDt      = Dateto7(SystemDate());
             Write pcr00;
          Endif;

          //*** Begin Del *** 08/02/23 *************************************
          //* New York Cancellation Form
          //*** Chain (intCo#:0:'NY':990101) pcr00;
          //*** If not %found;
          //***    Clear pcr00;
          //***    CRCo#     = intCo#;
          //***    CRFnd     = 0;
          //***    CRST      = 'NY';
          //***    CREfdt    = 990101;
          //***    CRCxDoc   = 'NY Cancel Notice.scd';
          //***    CRCxPath  = '/Compliance/Base/Policy/';
          //***    CRReDoc   = 'NY Reinstate Notice.scd';
          //***    CRRePath  = '/Compliance/Base/Policy/';
          //***    CRCXCR  = 'CX Cert Letter.scd';
          //***    CRCXCRP = '/Compliance/Base/Policy/';
          //***    CRCFCR  = 'CX Confirmation Cert Letter.scd';
          //***    CRCFCRP = '/Compliance/Base/Policy/';
          //***    CRRECR  = 'Rein Cert Letter.scd';
          //***    CRRECRP = '/Compliance/Base/Policy/';
          //***    CRIUsr    = Q1User;
          //***    CRIDt     = Dateto7(SystemDate());
          //***    CRPgm     = Q1Pgm;
          //***    CRUser    = Q1User;
          //***    CRTime    = Timeto6(SystemTime());
          //***    CRDt      = Dateto7(SystemDate());
          //***    Write pcr00;
          //*** Endif;
          //*** End   Del *** 08/02/23 *************************************

          //* Virginia Cancellation Form
          Chain (intCo#:0:'VA':990101) pcr00;
          If not %found;
             Clear pcr00;
             CRCo#     = intCo#;
             CRFnd     = 0;
             CRST      = 'VA';
             CREfdt    = 990101;
             CRCxDoc   = 'VA Cancel Notice.scd';
             CRCxPath  = '/Compliance/Base/Policy/';
             CRReDoc   = 'VA Reinstate Notice.scd';
             CRRePath  = '/Compliance/Base/Policy/';
             CRCXCR  = 'CX Cert Letter.scd';
             CRCXCRP = '/Compliance/Base/Policy/';
             CRCFCR  = 'CX Confirmation Cert Letter.scd';
             CRCFCRP = '/Compliance/Base/Policy/';
             CRRECR  = 'Rein Cert Letter.scd';
             CRRECRP = '/Compliance/Base/Policy/';
             CRIUsr    = Q1User;
             CRIDt     = Dateto7(SystemDate());
             CRPgm     = Q1Pgm;
             CRUser    = Q1User;
             CRTime    = Timeto6(SystemTime());
             CRDt      = Dateto7(SystemDate());
             Write pcr00;
          Endif;

          //* Wisconsin Cancellation Form
          Chain (intCo#:0:'WI':990101) pcr00;
          If not %found;
             Clear pcr00;
             CRCo#     = intCo#;
             CRFnd     = 0;
             CRST      = 'WI';
             CREfdt    = 990101;
             CRCxDoc   = 'WI Cancel Notice.scd';
             CRCxPath  = '/Compliance/Base/Policy/';
             CRReDoc   = 'WI Reinstate Notice.scd';
             CRRePath  = '/Compliance/Base/Policy/';
             CRCXCR  = 'CX Cert Letter.scd';
             CRCXCRP = '/Compliance/Base/Policy/';
             CRCFCR  = 'CX Confirmation Cert Letter.scd';
             CRCFCRP = '/Compliance/Base/Policy/';
             CRRECR  = 'Rein Cert Letter.scd';
             CRRECRP = '/Compliance/Base/Policy/';
             CRIUsr    = Q1User;
             CRIDt     = Dateto7(SystemDate());
             CRPgm     = Q1Pgm;
             CRUser    = Q1User;
             CRTime    = Timeto6(SystemTime());
             CRDt      = Dateto7(SystemDate());
             Write pcr00;
          Endif;
          //*** End   Add *** 05/10/23 *************************************

          Close wscrl;

       Endsr;
       //====================================================================

       //** Begin Add *** 03/08/17 ********************************************
       //====================================================================
       // Add records to certificate of insurance setup file.
       //====================================================================
       Begsr AddCertData;

          intCo# = GetUserInfo(q1user);

          Open wscil;

       //* Base Certificate Form
          Chain (intCo#:0:'  ':990101) pci00;
          If not %found;
             Clear pci00;
             CICo#     = intCo#;
             CIFnd     = 0;
             CIEfdt    = 990101;
             CIDoc     = 'Certificate of Insurance.scd';
             CIPath    = '/Compliance/Base/Policy/';
             CIIUsr    = Q1User;
             CIIDt     = Dateto7(SystemDate());
             CIPgm     = Q1Pgm;
             CIUser    = Q1User;
             CITime    = Timeto6(SystemTime());
             CIDt      = Dateto7(SystemDate());
             Write pci00;
          Endif;

       //** Begin Add *** 03/09/17 ********************************************
       //* CA Certificate Form
          Chain (intCo#:0:'CA':990101) pci00;
          If not %found and clientID = '148';
             Clear pci00;
             CICo#     = intCo#;
             CIFnd     = 0;
             CISt      = 'CA';
             CIEfdt    = 990101;
             CIDoc     = 'Certificate of Insurance CA.scd';
             CIPath    = '/Compliance/Base/Policy/';
             CIIUsr    = Q1User;
             CIIDt     = Dateto7(SystemDate());
             CIPgm     = Q1Pgm;
             CIUser    = Q1User;
             CITime    = Timeto6(SystemTime());
             CIDt      = Dateto7(SystemDate());
             Write pci00;
          Endif;

       //* NY Certificate Form
          Chain (intCo#:0:'NY':990101) pci00;
          If not %found and clientID = '148';
             Clear pci00;
             CICo#     = intCo#;
             CIFnd     = 0;
             CISt      = 'NY';
             CIEfdt    = 990101;
             CIDoc     = 'Certificate of Insurance NY.scd';
             CIPath    = '/Compliance/Base/Policy/';
             CIIUsr    = Q1User;
             CIIDt     = Dateto7(SystemDate());
             CIPgm     = Q1Pgm;
             CIUser    = Q1User;
             CITime    = Timeto6(SystemTime());
             CIDt      = Dateto7(SystemDate());
             Write pci00;
          Endif;

       //* RI Certificate Form
          Chain (intCo#:0:'RI':990101) pci00;
          If not %found and clientID = '148';
             Clear pci00;
             CICo#     = intCo#;
             CIFnd     = 0;
             CISt      = 'RI';
             CIEfdt    = 990101;
             CIDoc     = 'Certificate of Insurance RI.scd';
             CIPath    = '/Compliance/Base/Policy/';
             CIIUsr    = Q1User;
             CIIDt     = Dateto7(SystemDate());
             CIPgm     = Q1Pgm;
             CIUser    = Q1User;
             CITime    = Timeto6(SystemTime());
             CIDt      = Dateto7(SystemDate());
             Write pci00;
          Endif;

       //** End Add ***** 03/09/17 ********************************************

          Close wscil;

       Endsr;
       //====================================================================
       //** End Add  **** 03/08/17 ********************************************
       //** Begin Add *** 07/19/17 ********************************************
       //===================================================================
       // Write FROI records.
       //===================================================================
       BegSr AddFROIRecord;

          intCo# = GetUserInfo(q1user);

          Open wsffl;
       // FROI A1
          Chain (000:'  ':0990101) pff00;
          If not %found;
             Clear pff00;

             FFFnd     = 000;
             FFSt      = '  ';
             FFEfdt    = 0990101;

             //*** begin chg ***  01/08/20  ************************************
             //*****FFDoc     = 'FROI.scd';
             FFDoc     = 'FROI IA-1.scd';
             //*** end chg *****  01/08/20  ************************************
             FFPath    = '/Compliance/Base/Claim/';
             FFLang    = 'English';
             FFLangC   = 'English';

             FFIPgm    = Q1Pgm;
             FFIUsr    = Q1User;
             FFIDt     = Dateto7(SystemDate());
             FFITm     = Timeto6(SystemTime());
             FFPgm     = Q1Pgm;
             FFUser    = Q1User;
             FFTime    = Timeto6(SystemTime());
             FFDt      = Dateto7(SystemDate());
             Write pff00;
          Endif;
       Endsr;
       //===================================================================
       // Write State FROI records.
       //===================================================================
       BegSr AddStFROIRecord;

       // WMIXP CHAIN
          Open wmixl;

       Setll (intCo#) pix00;
       Reade(n) (intCo#) pix00;
       Dow not %eof;
          If IXtrid = '148';

          // FROI State
             Chain (000:IXjrst:0990101) pff00;
             If not %found;
                Clear pff00;

                FFFnd     = 000;
                FFSt      = IXjrst;
                FFEfdt    = 0990101;

                FFDoc     = 'FROI'+' '+IXjrst+'.scd';
                FFPath    = '/Compliance/Base/Claim/';
                FFLang    = 'English';
                FFLangC   = 'English';

                FFIPgm    = Q1Pgm;
                FFIUsr    = Q1User;
                FFIDt     = Dateto7(SystemDate());
                FFITm     = Timeto6(SystemTime());
                FFPgm     = Q1Pgm;
                FFUser    = Q1User;
                FFTime    = Timeto6(SystemTime());
                FFDt      = Dateto7(SystemDate());
                Write pff00;
             Endif;
          Endif;
          Reade(n) (intCo#) pix00;
       Enddo;

       Close wmixl;
       Close wsffl;
       Endsr;
       //** End Add *** 07/19/17 **********************************************

       //** Begin Add *** 01/27/17 ********************************************

       //===================================================================
       // Write default types of notices for Scriptura.
       //===================================================================
       BegSr WriteScripDef;

       xxTBID = 'POLF';

       //* Loss prevention notices
       xxDesc = 'Loss Prevention Notice';
       xxNoteType = 'L';

       xxKey  = 'AR';
       Exsr WriteToFile;

       xxKey  = 'KS';
       Exsr WriteToFile;

       xxKey  = 'OK';
       Exsr WriteToFile;

       xxKey  = 'OR';
       Exsr WriteToFile;

       xxKey  = 'PA';
       Exsr WriteToFile;

       xxKey  = 'TX';
       Exsr WriteToFile;


       //* Deductible Opt In Form
       xxDesc = 'Deductible Opt In Form';
       xxNoteType = 'D';

       xxKey  = 'DE';
       Exsr WriteToFile;


       //* Policy Notice
       xxDesc = 'Policy Notice';
       xxNoteType = 'P';

       xxKey  = 'VA';
       Exsr WriteToFile;

       //** begin add *** 07/18/17 ********************************************

       //* POC Notice
       xxDesc = 'Proof of Coverage Notice';
       xxNoteType = 'C';

       xxKey  = 'MI';
       Exsr WriteToFile;

       //** end add ***** 07/18/17 ********************************************

       Endsr;
       //===================================================================
       BegSr WriteToFile;

       Setll (intCo#:xxTbid:xxKey) pgt00;
       Reade(n) (intCo#:xxTbid:xxKey) pgt00;
       Dow not %eof;
          If GTDesc = xxDesc;
             Leave;
          Endif;
          Reade(n) (intCo#:xxTbid:xxKey) pgt00;
       Enddo;

       If %eof;
          Clear pgt00;
          GTCo#  = intCo#;
          GTTBID = xxTBID;
          GTKEY  = xxKey;
          GTFG01 = xxNoteType;
          GTSTAT = 'A';
          GTDESC = xxDesc;
          GTPgm  = Q1PGM;
          GTUser = Q1USER;
          GTTime = Timeto6(SystemTime());
          GTDt   = Dateto7(SystemDate());
          Write pgt00;
       Endif;

       Endsr;
       //===================================================================
       //** End Add  **** 01/27/17 ********************************************

       //** Beg Add  **** 07/05/22 ********************************************
       //===================================================================
       // Update STGTP
       //===================================================================
       Begsr updgtp;
          GTDESC = TDES(i3);
          GTPgm  = Q1PGM;
          GTUser = Q1USER;
          GTTime = Timeto6(SystemTime());
          GTDt   = Dateto7(SystemDate());
          update pgt00;
       Endsr;
       //===================================================================
       //** End Add  **** 07/05/22 ********************************************

       //** Beg Add  **** 07/05/22 ********************************************
       //===================================================================
       // Write new record to STGTP
       //===================================================================
       Begsr wrtgtp;
          GTCo#  = intCo#;
          GTTBID = xxTBID;
          GTKEY  = xxKey;
          GTSTAT = 'A';
          GTDESC = TDES(i3);
          GTPgm  = Q1PGM;
          GTUser = Q1USER;
          GTTime = Timeto6(SystemTime());
          GTDt   = Dateto7(SystemDate());
          Write pgt00;
       Endsr;
       //===================================================================
       //** End Add  **** 07/05/22 ********************************************
      /End-free
**  100 A  - All qualifying output
Commission Statements
EOB Print
Deductible Invoice
Claim Action Plan
Policy Page
Premium Notice
Aging Report
CEU Detail Report
CEU Summary Report
Premium Due Report - Self Reporting
Self-Reporting Worksheet
Checks
A_R Detail Report
System Security Report
Claim Payment History Report
Reserve Worksheet Report
Cancellations
Reinstatements
Mid-Term Endorsement
Estimated Late Notice
SR Late Notice
Certificate of Insurance
Carrier Policy# Report
Rule Report
First Report of Injury
Billing Invoice
ACORD130
Audit Request
Quote Bill
Estimated Bill
Revised Bill
Audit Bill
**  100A - Add retired hardcopy output here
System Security Report
Rule Report
Billing Invoice
ACORD130
**  10 A  - extra data program
                                                             Commission Statements
                                                             EOB Print
                                                             Deductible Invoice
WSCLMACTP                                                    Claim Action Plan
                                                             Policy Page
                                                             Premium Notice
                                                             Aging Report
                                                             CEU Detail Report
                                                             CEU Summary Report
                                                             Premium Due Report - Self Reporting
                                                             Self-Reporting Worksheet
                                                             Checks
                                                             A/R Detail Report
                                                             System Security Report
                                                             Claim Payment History Report
                                                             Reserve Worksheet Report
                                                             Cancellations
                                                             Reinstatements
                                                             Mid-Term Endorsement
                                                             Estimated Late Notice
                                                             SR Late Notice
                                                             Certificate of Insurance
                                                             Carrier Policy# Report
STFILECHN                                                    Rule Report
                                                             FROI
                                                             Billing Invoice
                                                             ACORD130
                                                             Audit Request
                                                             Quote Bill
                                                             Estimated Bill
                                                             Revised Bill
                                                             Audit Bill
**  1 A  - interactive processes
                                                             Commission Statements
                                                             EOB Print
                                                             Deductible Invoice
Y                                                            Claim Action Plan
                                                             Policy Page
                                                             Premium Notice
                                                             Aging Report
                                                             CEU Detail Report
                                                             CEU Summary Report
                                                             Premium Due Report - Self Reporting
                                                             Self-Reporting Worksheet
                                                             Checks
                                                             A/R Detail Report
                                                             System Security Report
                                                             Claim Payment History Report
                                                             Reserve Worksheet Report
                                                             Cancellations
                                                             Reinstatements
                                                             Mid-Term Endorsement
                                                             Estimated Late Notice
                                                             SR Late Notice
                                                             Certificate of Insurance
                                                             Carrier Policy# Report
                                                             Rule Report
                                                             FROI
                                                             Billing invoice
                                                             ACORD130
                                                             Audit Request
                                                             Quote Bill
                                                             Estimated Bill
                                                             Revised Bill
                                                             Audit Bill
**  60 A  - default template
Agent Commission.scd                                         Commission Statements
                                                             EOB Print
Deductible Invoice.scd                                       Deductible Invoice
SR Claim Action Plan.sld                                     Claim Action Plan
Policy Information Page.scd                                  Policy Page
Premium Notice.scd                                           Premium Notice
Aging Report.scd                                             Aging Report
CEU Detail Report.scd                                        CEU Detail Report
CEU Summary Report.scd                                       CEU Summary Report
Premium Due Report - Self Reporting.scd                      Premium Due Report - Self Reporting
Self-Reporting Worksheet.scd                                 Self-Reporting Worksheet
Check.scd                                                    Checks
A_R Detail Report.scd                                        A/R Detail Report
System Security Report.scd                                   System Security Report
Claim Payment History Report.scd                             Claim Payment History Report
Reserve Worksheet Report.scd                                 Reserve Worksheet Report
Cancel Notice.scd                                            Cancellations
Reinstate Notice.scd                                         Reinstatements
Mid-Term Endorsement.scd                                     Mid-Term Endorsement
Estimated Late Notice.scd                                    Estimated Late Notice
SR Late Notice.scd                                           SR Late Notice
Certificate of Insurance.scd                                 Certificate of Insurance
Carrier Policy Number Report.scd                             Carrier Policy# Report
RuleReport.scd                                               Rule Report
FROI.scd                                                     FROI
Self Reporting Invoice.scd                                   Billing Invoice
ACORD130.scd                                                 ACORD130
Audit Request.scd                                            Audit Request
Quote Bill.scd                                               Quote Bill
Estimated Bill.scd                                           Estimated Bill
Revised Bill.scd                                             Revised Bill
Audit Bill.scd                                               Audit Bill
** 100 A  - default template path
/Reports/Base/Policy/

/Reports/Base/Policy/

/Compliance/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/System/
/Reports/Base/Claim/
/Reports/Base/Claim/
/Compliance/Base/Policy/
/Compliance/Base/Policy/
/Compliance/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Compliance/Base/Policy/
/Reports/Base/System/
/Reports/Base/System/
/Compliance/Base/Claim/
/Reports/Client Modification/Policy/CLT150/
/Compliance/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
/Reports/Base/Policy/
**  1 A  - status
A                                                           Commission Statements
                                                            EOB Print
A                                                           Deductible Invoice          ***01/17/25
A                                                           Claim Action Plan
A                                                           Policy Page
A                                                           Premium Notice
                                                            Aging Report
                                                            CEU Detail Report
                                                            CEU Summary Report
                                                            Premium Due Report - Self Reporting
A                                                           Self-Reporting Worksheet
                                                            Checks
                                                            A/R Detail Report
A                                                           System Security Report
                                                            Claim Payment History Report
                                                            Reserve Worksheet Report
A                                                            Cancellations
A                                                            Reinstatements
A                                                           Mid-Term Endorsement
A                                                           Estimated Late Notice
A                                                           SR Late Notice
A                                                            Certificate of Insurance
A                                                           Carrier Policy# Report
X                                                            Rule Report
                                                            FROI
A                                                           Billing Invoice
A                                                           ACORD130
A                                                           Audit Request
A                                                           Quote Bill
A                                                           Estimated Bill
A                                                           Revised Bill
A                                                           Audit Bill
**  1 A  - single process
Y                                                           Commission Statements
                                                            EOB Print
Y                                                           Deductible Invoice
Y                                                           Claim Action Plan
Y                                                           Policy Page
Y                                                           Premium Notice
                                                            Aging Report
                                                            CEU Detail Report
                                                            CEU Summary Report
                                                            Premium Due Report - Self Reporting
Y                                                           Self-Reporting Worksheet
                                                            Checks
                                                            A/R Detail Report
                                                            System Security Report
Y                                                           Claim Payment History Report
Y                                                           Reserve Worksheet Report
Y                                                            Cancellations
Y                                                            Reinstatements
Y                                                           Mid-Term Endorsement
Y                                                           Estimated Late Notice
Y                                                           SR Late Notice
Y                                                            Certificate of Insurance
Y                                                           Carrier Policy# Report
                                                             Rule Report
Y                                                           FROI
Y                                                           Billing Invoice
Y                                                           ACORD130
Y                                                           Audit Request
Y                                                           Quote Bill
Y                                                           Estimated Bill
Y                                                           Revised Bill
Y                                                           Audit Bill
**  1 A  - mass process
Y                                                           Commission Statements
                                                            EOB Print
Y                                                           Deductible Invoice
                                                            Claim Action Plan
Y                                                           Policy Page
Y                                                           Premium Notice
Y                                                           Aging Report
Y                                                           CEU Detail Report
Y                                                           CEU Summary Report
Y                                                           Premium Due Report - Self Reporting
Y                                                           Self-Reporting Worksheet
Y                                                           Checks
Y                                                           A/R Detail Report
Y                                                           System Security Report
Y                                                           Claim Payment History Report
                                                            Reserve Worksheet Report
Y                                                            Cancellations
Y                                                            Reinstatements
                                                            Mid-Term Endorsement
Y                                                           Estimated Late Notice
Y                                                           SR Late Notice
Y                                                            Certificate of Insurance
                                                            Carrier Policy# Report
                                                             Rule Report
                                                            FROI
Y                                                           Billing Invoice
                                                            ACORD130
Y                                                           Audit Request
Y                                                           Quote Bill
Y                                                           Estimated Bill
Y                                                           Revised Bill
Y                                                           Audit Bill
**  10 A  - additional setup program
                                                            Commission Statements
                                                            EOB Print
                                                            Deductible Invoice
                                                            Claim Action Plan
WSDOCPOLP                                                   Policy Page
                                                            Premium Notice
                                                            Aging Report
                                                            CEU Detail Report
                                                            CEU Summary Report
                                                            Premium Due Report - Self Reporting
                                                            Self-Reporting Worksheet
                                                            Checks
                                                            A/R Detail Report
                                                            System Security Report
                                                            Claim Payment History Report
                                                            Reserve Worksheet Report
WSDOCCXRE                                                    Cancellations
WSDOCCXRE                                                    Reinstatements
                                                            Mid-Term Endorsement
                                                            Estimated Late Notice
                                                            SR Late Notice
WSDOCCERT                                                    Certificate of Insurance
                                                            Carrier Policy# Report
                                                             Rule Report
WSDOCFROI                                                    FROI
                                                             Billing Invoice
                                                            ACORD130
                                                            Audit Request
WSDOCBILL                                                   Quote Bill
WSDOCBILL                                                   Estimated Bill
WSDOCBILL                                                   Revised Bill
WSDOCBILL                                                   Audit Bill
**  1 A  - scriptura output flag
N                                                            Commission Statements
N                                                            EOB Print
N                                                            Deductible Invoice
N                                                            Claim Action Plan
N                                                            Policy Page
N                                                            Premium Notice
N                                                            Aging Report
N                                                            CEU Detail Report
N                                                            CEU Summary Report
N                                                            Premium Due Report - Self Reporting
N                                                            Self-Reporting Worksheet
N                                                            Checks
N                                                            A/R Detail Report
Y                                                            System Security Report
N                                                            Claim Payment History Report
N                                                            Reserve Worksheet Report
N                                                            Cancellations
N                                                            Reinstatements
N                                                            Mid-Term Endorsement
N                                                            Estimated Late Notice
N                                                            SR Late Notice
N                                                            Certificate of Insurance
N                                                            Carrier Policy# Report
Y                                                            Rule Report
N                                                            FROI
Y                                                            Billing Invoice
Y                                                            ACORD130
N                                                            Audit Request
N                                                            Quote Bill
N                                                            Estimated Bill
N                                                            Revised Bill
N                                                            Audit Bill
**  1 A  - Hide Replacement Grid Flag
N                                                            Commission Statements
N                                                            EOB Print
N                                                            Deductible Invoice
N                                                            Claim Action Plan
Y                                                            Policy Page
N                                                            Premium Notice
N                                                            Aging Report
N                                                            CEU Detail Report
N                                                            CEU Summary Report
N                                                            Premium Due Report - Self Reporting
N                                                            Self-Reporting Worksheet
N                                                            Checks
N                                                            A/R Detail Report
N                                                            System Security Report
N                                                            Claim Payment History Report
N                                                            Reserve Worksheet Report
Y                                                            Cancellations
Y                                                            Reinstatements
N                                                            Mid-Term Endorsement
N                                                            Estimated Late Notice
N                                                            SR Late Notice
Y                                                            Certificate of Insurance
N                                                            Carrier Policy# Report
                                                             Rule Report
Y                                                            FROI
N                                                            Billing Invoice
N                                                            ACORD130
N                                                            Audit Request
Y                                                            Quote Bill
Y                                                            Estimated Bill
Y                                                            Revised Bill
Y                                                            Audit Bill
**  Flags  07/24/17 - Skip Messages (interactive,batch)
                                                             Commission Statements
                                                             EOB Print
                                                             Deductible Invoice
                                                             Claim Action Plan
                                                             Policy Page
                                                             Premium Notice
                                                             Aging Report
                                                             CEU Detail Report
                                                             CEU Summary Report
                                                             Premium Due Report - Self Reporting
                                                             Self-Reporting Worksheet
                                                             Checks
                                                             A/R Detail Report
                                                             System Security Report
                                                             Claim Payment History Report
                                                             Reserve Worksheet Report
                                                             Cancellations
                                                             Reinstatements
                                                             Mid-Term Endorsement
                                                             Estimated Late Notice
                                                             SR Late Notice
                                                             Certificate of Insurance
                                                             Carrier Policy# Report
YN                                                           Rule Report
                                                             Billing Invoice
                                                             ACORD130
                                                             Audit Request
                                                             Quote Bill
                                                             Estimated Bill
                                                             Revised Bill
                                                             Audit Bill
**  1 A  - Report Flag
                                                             Commission Statements
                                                             EOB Print
Y                                                            Deductible Invoice     ***01/17/25
                                                             Claim Action Plan
                                                             Policy Page
                                                             Premium Notice
                                                             Aging Report
                                                             CEU Detail Report
                                                             CEU Summary Report
                                                             Premium Due Report - Self Reporting
                                                             Self-Reporting Worksheet
                                                             Checks
                                                             A/R Detail Report
                                                             System Security Report
                                                             Claim Payment History Report
                                                             Reserve Worksheet Report
                                                             Cancellations
                                                             Reinstatements
                                                             Mid-Term Endorsement
Y                                                            Estimated Late Notice
Y                                                            SR Late Notice
                                                             Certificate of Insurance
                                                             Carrier Policy# Report
                                                             Rule Report
                                                             FROI
                                                             Billing Invoice
                                                             ACORD130
                                                             Audit Request
                                                             Quote Bill
                                                             Estimated Bill
                                                             Revised Bill
                                                             Audit Bill
**  2 A  - Category
AS                                                           Commission Statements
MC                                                           EOB Print
PS                                                           Deductible Invoice
CS                                                           Claim Action Plan
PS                                                           Policy Page
PS                                                           Premium Notice
RP                                                           Aging Report
RP                                                           CEU Detail Report
RP                                                           CEU Summary Report
RP                                                           Premium Due Report - Self Reporting
PS                                                           Self-Reporting Worksheet
CS                                                           Checks
RP                                                           A/R Detail Report
RP                                                           System Security Report
RP                                                           Claim Payment History Report
RP                                                           Reserve Worksheet Report
PS                                                           Cancellations
PS                                                           Reinstatements
PS                                                           Mid-Term Endorsement
PS                                                           Estimated Late Notice
PS                                                           SR Late Notice
PS                                                           Certificate of Insurance
RP                                                           Carrier Policy# Report
RP                                                           Rule Report
CS                                                           FROI
PS                                                           Billing Invoice
PS                                                           ACORD130
PS                                                           Audit Request
PS                                                           Quote Bill
PS                                                           Estimated Bill
PS                                                           Revised Bill
PS                                                           Audit Bill
** STGTP KEYS & DESCRIPTIONS (TKEY, TDES)
PS        Policy Services                                                                 **07/05/22
CS        Claims Services                                                                 **07/05/22
AS        Agency Services                                                                 **07/05/22
