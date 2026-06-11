     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     *===============================================================================
     *
     * SPIEMAIL - Email procedures
     *
     * Created   : 04/19/21    C:33   Project: WC2278
     *
     *===============================================================================
      * Revisions:                                                                                 *
      *
      * 11/25/24 C:333 Support: 118071, Project: WC2826                                            *
      *                From the Edit Claim Contact Window and selecting the                        *
      *                "Process Output Control" type it does not                                   *
      *                pull over the email address.                                                *
      *
      * 11/21/24 C:333 Support: 118045, Project: WC2826                                            *
      *                Email Recipient is not pulling correctly                                    *
      *                on Output Control window of 'Claim Action Plan'                             *
      *
      * 11/19/24 C:300 Support: 118045, Project: WC2826                                            *
      *                Email Distribution not Pulling for Email                                    *
      *                on Output Control window of 'Claim Action Plan'                             *
      *                                                                                            *
      * 09/10/24 C:333 WC2826(116976)                                                              *
      *                Claims Contacts                                                             *
      *                - Adding Subroutine to fetch recipient for outtype.                         *
      *                                                                                            *
      * 08/30/24 C:65  Project: WC2826 (Phw#117548)                                                *
      *                The coding which added new database values retrieved by the                 *
      *                SetSQLStmt procedure did not modify the non-claim types.                    *
      *                                                                                            *
      * 08/26/24 C:053 WC2826/116977                                                               *
      *                Adding new tokens.                                                          *
      *
      * 08/06/24 C:333 WC2826/115874/116975/116976                                                 *
      *                Claims Contacts                                                             *
      *                - Adding Claimant info.
      *
      * 07/23/24 C:300 Support: 116982, Project: WC2826                                            *
      *                Email Distribution not Pulling for Email                                    *
      *
      * 03/22/24 C:333 Project: WC2826                                                             *
      *                Claims Contacts                                                             *
      *                - Adding New Parameters for "GetDefaultEmailRecipients" in SPREMAIL.        *
      *
      * 05/25/23 C:291 Project: WC2875                                                             *
      *                Change GetTokensEmail procedure: RunType will be passed as Category code    *
      *                instead of Category description.                                            *
      *
      * 05/22/23 C:062 Project: WC2838                                                             *
      *                Fix insured and agent email flags                                           *
      *                                                                                            *
      * 05/18/23 C:291 Project: WC2838                                                             *
      *                Added new parameter to GetTokensEmail. This new parameter                   *
      *                is the second run type to load Email Tokens for FTP.                        *
      *                STAPPCFG/LoadOutputCtl subroutine calls GetTokensEmail                      *
      *                with second Run type parameter to load Agency Tokens grid                   *
      *                for FTP.                                                                    *
      *                                                                                            *
      * 05/08/23 C:344 Project: WC2838                                                             *
      *                Changed the DefaultsDocuments subroutine to use the                         *
      *                default email subject and body when the email                               *
      *                subject and body fields from the Document Setup                             *
      *                file are nonexstent or blank.                                               *
      *                                                                                            *
      * 04/26/23 C:344 Project: WC2838                                                             *
      *                Added a new procedure and code to obtain the logo for                       *
      *                'Connect Notification' email types.                                         *
      *                                                                                            *
      * 04/24/23 C:344 Support: 110190                                                             *
      *                Fix field sequence in the SETSQLSMNT procedure so it                        *
      *                coincides with DSINFO datastructure.                                        *
      *                                                                                            *
      * 03/23/23 C:344 Support: 107726                                                             *
      *                Added code to ReplaceTokenEMail procedure to replace                        *
      *                21 New Email Tokens related to various forms     and                        *
      *                of names, emails and phone#s.                    and                        *
      * 02/09/23 C:062 Project: WC2838                                                             *
      *                Fix d Agent and insured email parameters.                                   *
      *                                                                                            *
      * 01/02/23 C:344 Project: WC2875                                                             *
      *                - Updated the emailing API to use the new token                             *
      *                  database files.                                                           *
      *                - Added code to the ReplaceTokensEmail procedure to                         *
      *                  replace 7 staff code email token that were    the                         *
      *                  missing from the original design.             the                         *
      *                  The email tokens include:                                                 *
      *                  #audit manager staff code#, #audit reviewer staff                         *
      *                  code#, #claim contact staff code#, #loss control                          *
      *                  rep staff code#, #policy services staff code#,                            *
      *                  #premium accountant staff code# and #underwriter                          *
      *                  staff code#                                                               *
      *                - Revised the ReplaceTokensEmail procedure to                               *
      *                  receive reporting period date from WISRMAIL.                              *
      *                                                                                            *
      * 01/16/23 C:062 Project: WC2838                                                             *
      *                - Add Agent and insured email parameters.                                   *
      *                                                                                            *
      * 09/26/22 C:344 Project: WC2838                                                             *
      *                - Add processing to the email recipients procedure to                       *
      *                  return whether the insured or the agent is part of                        *
      *                  the email data.                                                           *
      *                - Change when GetTokensEmail procedure so that when                         *
      *                  its called, the Category Description field is                             *
      *                  available to be passed to it, if it hasn't already.                       *
      *                                                                                            *
      * 07/07/22 C:333 Support: 102623                                                             *
      *                Application Configuration-Output Control Categories                         *
      *                - Comparing strRunType for 'Policy Services'.                               *
      *                                                                                            *
      * 06/03/22 C:33  Project: Phw#100993    (UBIC)                                               *
      *                Get agency name for 'Agency' runs                                           *
      *                                                                                            *
      * 05/27/22 C:65  Project: Phw#100993    (UBIC)                                               *
      *                When getting agency contacts, skip producer criteria if no                  *
      *                producer has been passed in.                                                *
      *                                                                                            *
     *===============================================================================
     fsPiEmail  cf   e             workstn usropn
     f                                     handler('PROFOUNDUI(HANDLER)')

      *** Begin Add *** 09/26/22  WC2838 **************************
     fstgtl     if   e           k disk    rename(pgt01:pgt00)
      *** End   Add *** 09/26/22  WC2838 **************************
      *** Beg Add *** 01/02/23 *** WC2875 *********************************
     fstetl     if   e           k disk    rename(pet01:pet00)
     fstet1l    if   e           k disk    rename(pet11:pet10)
      *** End Add *** 01/02/23 *** WC2875 *********************************
      *** Beg Add *** 03/23/23 *** 107726 *********************************
     fwmupp     if   e           k disk    rename(pup01:pup00)
      *** End Add *** 03/23/23 *** 107726 *********************************
      *** Beg Add *** 04/26/23 ********************************************
     fwtgtl     if   e           k disk    rename(pgt01:pgt02)
     fwidftemll if   e           k disk    rename(pem01:pem00)
      *** End Add *** 04/26/23 ********************************************
      *** Beg Add *** 03/22/24 ********************************************
     fwmupl1    if   e           k disk
      *** End Add *** 03/22/24 ********************************************
     ddsEmail        e ds                  extname(sPiEmail:RcdEmails)
     d                                     based(ptrEmail)
     dptrEmail         s               *   inz

     ddsSTDEP        e ds                  extname(STDEP)    inz
     ddsSTDEPcrt     e ds                  extname(STDEPcrt) inz qualified
     ddsSTDLP        e ds                  extname(STDLP)    inz
     ddsSTDLPcrt     e ds                  extname(STDLPcrt) inz qualified
      *** Beg Add ***** 08/07/24 *********************************
     ddsSTDLPcrt1    e ds                  extname(STDLPcrt1) inz qualified
      *** End Add ***** 08/07/24 *********************************
     ddsSTSPP        e ds                  extname(STSPP)    inz
     ddsSTSPPcrt     e ds                  extname(STSPPcrt) inz qualified
      *** Beg Add ***** 03/22/24 *********************************
     ddsSTSPPcrt1    e ds                  extname(STSPPcrt1) inz qualified
      *** End Add ***** 03/22/24 *********************************
     d
     ddsWMCDP        e ds                  extname(WMCDP)    inz

     ddsInfo           ds                  inz qualified
     d co#                            3  0
     d fnd                            3  0
     d fyr                            3  0
     d emp#                           9  0
     d div                            5  0
      *** Beg Add ***** 03/22/24 ******************************************
     d case                           7  0
      *** End Add ***** 03/22/24 ******************************************
      *** Beg Add ***** 08/26/24 ******************************************
     d acDt                           7  0
     d nameClm                       45
     d lostTmFl                       1
     d adjStaff                       3
      *** End Add ***** 08/26/24 ******************************************
     d bsCd                           3  0
     d pfDt                           7  0
     d ptDt                           7  0
     d feinEmp                        9  0
     d nameEmp                      100
     d feinAgt                        9  0
     d mod                            5  0
     d prod                           5  0
      *** Beg Add *** 01/02/23 *** WC2875 *********************************
     d polSrv                         3
     d undWtr                         3
     d prmAcc                         3
     d audRVW                         3
     d audMGR                         3
     d lossrep                        3
     d clmCon                         3
      *** End Add *** 01/02/23 *** WC2875 *********************************
     ddsInfoNull       s              5i 0 dim(13)

     di1               s              5  0 inz
     ddecCount         s             15  0 inz
     dstrSql           s               a   len(32000) varying

     d#ofDistribution  c                   9999
     daryDistribution  s             50    varying dim(#ofDistribution) inz
     d#ofRecipient     c                   9999
     daryRecipient     s             60    varying dim(#ofRecipient) inz
      *** Begin Add *** 01/16/23 *******************************************
     daryAgencyRecipient...
     d                 s             60    varying dim(50) inz
     daryInsuredRecipient...
     d                 s             60    varying dim(50) inz
      *** End   Add *** 01/16/23 *******************************************
      *** Begin Add *** 09/26/22  WC2838 **************************
     dxxCo#            s                   like(GTCO#) inz
     dxxTBid           s                   like(GTTBID) inz
     dxxKey            s                   like(GTKEY) inz
      *** End   Add *** 09/26/22  WC2838 **************************
      *** Beg Add ***** 03/22/24 **********************************
     daryClaimRecipient...
     d                 s             60    varying dim(50) inz
     d#ofStaffRoles    c                   9999
     daryStaffRoles    s             50    varying dim(#ofStaffRoles) inz
     d Type            s             25
     d Seq#            s             15  0
     d ##SeqVerify     s             15  0
     d ##email         s             60
     d SysFlag         s              1
     d CritVal         s             50
     d CritVal2        s             50
     d Supervisor      s                   like(UPstaf)
     d AccManag        s                   like(UPstaf)
     d ClmManag        s                   like(UPstaf)
     d CaseNurse       s                   like(UPstaf)
     d OutAdj          s                   like(UPstaf)
     d ReinAdj         s                   like(UPstaf)
     d FundAdj         s                   like(UPstaf)
     d HeadingAdj      s                   like(UPstaf)
     d AdjHdg          s             19
      *** End Add ***** 03/22/24 **********************************
      *** Beg Add ***** 11/21/24 **********************************
     d xxType          s            100A
     d ##Type          s             25A
     d ##seq#          s             15  0
      *** End Add ***** 11/21/24 **********************************

     d/copy sPrDate
     d/copy sPrEmail
     d/copy sPrSql
     d/copy sPrString
     d/copy sPrUsrPrms

     d psds          esds                  extname(szq1p)

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d AgtInfoAPI      pr                  extpgm('WA002R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##EMP#                       9  0
     d   ##DIV                        5  0
     d   ##FYR                        3  0
     d   ##AGT#                       5  0
     d   ##ANAM                      60
     d   ##PROD                       5  0
     d   ##PNAM                      60

      /Free
       // This must be the first line in the code
       Exec Sql Set Option Commit = *None;
       Monitor;
          AAATest();
       On-error;
       Endmon;
       *inlr = *on;

      *=====================================================================
      * Used for testing
      *=====================================================================
     pAAATest...
     p                 b                   export
     dAAATest...
     d                 pi

     ddsTest         e ds                  extname(sPiEmail:RcdEmails)
     dptrTest          s               *   inz

      /Free
       GetDefaultEmailRecipients('ClaimantContactLetter.scd':1372:'Claim');
       Return;
       Open sPiEmail;
       Clear dsTest;
       ptrTest = %addr(dsTest);
       LoadDefaultEmaildata(ptrTest:'ClaimantContactLetter.scd');
       Exfmt RcdEmails;
       Return;
      /End-free
     pAAATest...
     p                 e

      /End-free
      *=====================================================================
      * Get the default Email recipients
      *  inRunInfo: the document id or the process type
      *  inRrnType:     RRN from file
      *  ============================
      *  Policy         WDELP
      *  Claim          WMCMP
      *  Agency         WMAGP
      *  Agent          WMAAP
      *=====================================================================
     pGetDefaultEmailRecipients...
     p                 b                   export
     dGetDefaultEmailRecipients...
     d                 pi          5000a   varying
     d inRunInfo                           const likeds(dsRunInfo)
     d inRrn                          9  0 const
      //*** Beg Chg *** 01/02/23 *** WC2875 ********************************
     d***inRrnType                     25    const varying  options(*nopass)
     d inRrnType                    100    const varying  options(*nopass)
      //*** End Chg *** 01/02/23 *** WC2875 ********************************
      *** Begin Add *** 09/26/22  WC2838 **************************
     d outInsured                     1    options(*nopass)
     d outAgent                       1    options(*nopass)
      *** End   Add *** 09/26/22  WC2838 **************************
      *** Begin Add *** 01/16/23 **********************************
     d outInsuredEmails...
     d                             1000    options(*nopass) varying
     d outAgencyEmails...
     d                             1000    options(*nopass) varying
      *** End   Add *** 01/16/23 **********************************
      *** Beg Add *** 03/22/24 ************************************
     d outClaim                       1    options(*nopass)
     d outClaimEmails...
     d                             1000    options(*nopass) varying
      *** End Add *** 03/22/24 ************************************

     ddsWMAPP        e ds                  extname(WMAPP)    inz qualified
     ddsWMAPPcrt     e ds                  extname(WMAPPcrt) inz qualified
     ddsWMA5Pcrt     e ds                  extname(WMA5Pcrt) inz qualified
     ddsWMECP        e ds                  extname(WMECP)    inz qualified
     ddsWMECPcrt     e ds                  extname(WMECPcrt) inz qualified
      *** Beg Add *** 03/22/24 ************************************
     ddsWMCNP        e ds                  extname(WMCNP)    inz qualified
     ddsWMCNPcrt     e ds                  extname(WMCNPcrt) inz qualified
      *** End Add *** 03/22/24 ************************************

     ddecDiv           s              5  0 inz
     ddecEmp#          s              9  0 inz
     dintSeq#          s             15  0 inz
     disDivSpec        s              1n   inz(*on)
     disEofRecipient   s              1n   inz
     disValidRecipient...
     d                 s              1n   inz
     dstrCategory      s              5    inz
     dstrEmail         s             60    inz varying
     dstrPeo           s              1    inz
     dstrRecipients    s                   like(GetDefaultEmailRecipients)
     dstrSubCat        s              5    inz

      /Free
       //*** Begin Add *** 09/26/22  WC2838 **************************
       If %parms >= 4;
          clear outInsured;
       Endif;
       If %parms >= 5;
          clear outAgent;
       Endif;
       //*** End   Add *** 09/26/22  WC2838 **************************
       //*** Begin Add *** 01/16/23 **********************************
       If %Parms >= 6;
          Clear outInsuredEmails;
       Endif;
       If %Parms >= 7;
          Clear outAgencyEmails;
       Endif;
       //*** End   Add *** 01/16/23 **********************************
       //*** Beg Add *** 02/22/24 ************************************
       If %Parms >= 8;
          Clear outClaim;
       Endif;
       If %Parms >= 9;
          Clear outClaimEmails;
       Endif;
       //*** End Add *** 02/22/24 ************************************
       // The run type should not be the same across Documents and Output;
       //  therefore we will process both routines as only one will produce results
       Select;
       When GetInfoDocument(inRunInfo);
          strCategory = DLcat;
          strSubCat   = DLsubC;
       When GetInfoOutput(inRunInfo);
          strCategory = SPcat;
          strSubCat   = *blanks;
       Endsl;
       strSql = SetSqlStmt(inRrn:inRrnType);
       Reset dsInfo;
       Exec sql close cursorRecipientInfo;
       Exec sql prepare sqlRecipientInfo from :strSql;
       Exec sql declare cursorRecipientInfo cursor for sqlRecipientInfo;
       Exec sql open cursorRecipientInfo;
       Exec SQL fetch next from cursorRecipientInfo into :dsInfo :dsInfoNull;

       Reset i1;
       Reset aryRecipient;
       //*** Begin Add *** 01/16/23 ****************************************
       Reset aryAgencyRecipient;
       Reset aryInsuredRecipient;
       //*** End   Add *** 01/16/23 ****************************************
       //*** Beg Add ***** 03/22/24 ****************************************
       Reset aryClaimRecipient;
       //*** End Add ***** 03/22/24 ****************************************
       // If the prior SQL doesn't produce the information needed,
       //  the following will return no results.
       Exsr GetContactsClaim;
       Exsr GetContactsAgent;
       Exsr GetContactsProducer;
       Exsr GetContactsEmployer;
       //*** Beg Add ***** 09/10/24 **************************************
       Exsr GetOutTypeRecipient;
       //*** End Add ***** 09/10/24 **************************************

       // join array elements to comma seperated elements
       For i1= 1 to #ofRecipient;
          If aryRecipient(i1) = *blanks;
             Iter;
          Endif;
          strRecipients += ',' + %trim(aryRecipient(i1));
       Endfor;
       If strRecipients <> *blanks;
          strRecipients = %scanrpl(',':'':strRecipients:1:1);
       Endif;

       //*** Begin Add *** 01/16/23 *****************************
       // Build Insured Email List
       If %Parms >= 6;
          For i1= 1 to 50;
             If aryInsuredRecipient(i1) = *blanks;
                Iter;
             Endif;
             outInsuredEmails += ',' + %trim(aryInsuredRecipient(i1));
          Endfor;
          If outInsuredEmails <> *blanks;
             outInsuredEmails =
                %scanrpl(',':'':outInsuredEmails:1:1);
          //*** Begin Add *** 05/22/23 **********************************
             outInsured = 'Y';
          Else;
             outInsured = ' ';
          //*** End   Add *** 05/22/23 **********************************
          Endif;
       Endif;

       // Build Agency Email List
       If %Parms >= 7;
          For i1= 1 to 50;
             If aryAgencyRecipient(i1) = *blanks;
                Iter;
             Endif;
             outAgencyEmails += ',' + %trim(aryAgencyRecipient(i1));
          Endfor;
          If outAgencyEmails <> *blanks;
             outAgencyEmails =
                %scanrpl(',':'':outAgencyEmails:1:1);
          //*** Begin Add *** 05/22/23 **********************************
             outAgent = 'Y';
          Else;
             outAgent = ' ';
          //*** End   Add *** 05/22/23 **********************************
          Endif;
       Endif;
       //*** End   Add *** 01/16/23 *****************************
       //*** Beg Add ***** 03/22/24 *****************************
       // Build Agency Email List
       If %Parms >= 9;
          For i1= 1 to 50;
             If aryClaimRecipient(i1) = *blanks;
                Iter;
             Endif;
             outClaimEmails += ',' + %trim(aryClaimRecipient(i1));
          Endfor;
          If outClaimEmails <> *blanks;
             outClaimEmails =
                %scanrpl(',':'':outClaimEmails:1:1);
             outClaim = 'Y';
          Else;
             outClaim = ' ';
          Endif;
       Endif;
       //*** End Add ***** 03/22/24 *****************************

       Return strRecipients;

       //================================================================
       Begsr GetContactsAgent;
        strSql = 'select A5eMal, A5seq#' +
         ' from WMA5P' +
         ' where A5co#=' + %char(dsInfo.co#) +
         '  and A5fein=' + %char(dsInfo.feinAgt) +
         '  and A5mod=' + %char(dsInfo.mod);
        Exsr ResetCursorRecipient;
        Reset isEofRecipient;
        Dou isEofRecipient;
           Exec sql fetch next from cursorRecipient into :strEmail, :intSeq#;
           Select;
           When not IsValidFetch(sqlstt);
              isEofRecipient = *on;
              Iter;
           Endsl;
           Exec sql
            select * into :dsWMA5Pcrt
             from WMA5Pcrt
             where A5co#=:dsInfo.co#
              and A5fein=:dsInfo.feinAgt
              and A5mod=:dsInfo.mod
              and A5seq#=:intSeq#
              and A5critId = 'Producer'
              and A5critVal = :dsInfo.prod;
           Select;
           //*** begin add ***  05/27/22  *********************************
           //* Producer criteria does not matter if no producer passed in.
           When dsInfo.prod = 0;
           //*** end add *****  05/27/22  *********************************
           // Contact associated with specific producer
           When IsValidFetch(sqlstt);
           Other;
              // We should only send if the contact is assigned a producer.
              Iter;
              // Purposely leaving this as it was original design.
              Exec sql
               select * into :dsWMA5Pcrt
                from WMA5Pcrt
                where A5co#=:dsInfo.co#
                 and A5fein=:dsInfo.feinAgt
                 and A5mod=:dsInfo.mod
                 and A5seq#=:intSeq#
                 and A5critId = 'Producer'
                limit 1;
              // Contact assocaited with at least one producer
              If IsValidFetch(sqlstt);
                 Iter;
              Endif;
           Endsl;
           Reset isValidRecipient;
           strSql = 'select *' +
            ' from WMA5Pcrt' +
            ' where A5co#=' + %char(dsInfo.co#) +
            '  and A5fein=' + %char(dsInfo.feinAgt) +
            '  and A5mod=' + %char(dsInfo.mod) +
            '  and A5seq#=' + %char(intSeq#);
           Exsr ResetCursorCriteria;
           Dou not IsValidFetch(sqlstt);
              Exec sql fetch next from cursorCriteria into :dsWMA5Pcrt;
              Select;
              When not IsValidFetch(sqlstt);
                 sqlstt='00000';
                 Leave;
              When dsWMA5Pcrt.A5critId = 'Distribution'
               and  %lookup(dsWMA5Pcrt.A5critVal:aryDistribution) <> *zero;
                 isValidRecipient = *on;
              When dsWMA5Pcrt.A5critId = 'Sub Category'
               and dsWMA5Pcrt.A5critVal = strSubCat;
                 isValidRecipient = *on;
              When dsWMA5Pcrt.A5critId = 'Process Output Control'
               and dsWMA5Pcrt.A5critVal = inRunInfo.outType;
                 isValidRecipient = *on;
              Endsl;
           Enddo;
           If isValidRecipient;
              LoadArrayRecipient(strEmail);
              //*** Begin Add *** 01/16/23 *****************************
              LoadAgencyArrayRecipient(strEmail);
              //*** End   Add *** 01/16/23 *****************************
              //*** Begin Add *** 09/26/22  WC2838 ******************* ****
              If %parms >= 5;
                 outAgent = 'Y';
              Endif;
              //*** End   Add *** 09/26/22  WC2838 ******************* ****
           Endif;
        Enddo;
       Endsr;

       //================================================================
       Begsr GetContactsClaim;
        If DLemlClmt = 'Y';
           Reset dsWMCDP;
           Exec sql
            select WMCDP.* into :dsWMCDP
             from WMCDP
             join WMCMP on CMss#=CDss#
            where rrn(WMCMP)= :inRrn;
           LoadArrayRecipient(CDemAd);
        Endif;
        //*** Beg Add ***** 03/22/24 ***********************************
        SysFlag = 'N';

        //*** Beg Add ***** 11/21/24 ***********************************
        Exec sql DECLARE mycursor2 Cursor For

        Select CMsupr,CMaMan,CMuMan,CMnrse,CMfeAd,
         CMeSup, CMsdfA ,CMclex From WMCMP
         where CMco# = :dsInfo.co#
          and CMfnd = :dsInfo.fnd
          and CMcase = :dsInfo.case;

        Exec sql OPEN mycursor2;

        Exec sql FETCH NEXT from mycursor2
          INTO :Supervisor, :AccManag, :ClmManag,
           :CaseNurse, :OutAdj, :ReinAdj, :FundAdj, :HeadingAdj;

        Exec sql CLOSE mycursor2;
        //*** End Add ***** 11/21/24 ***********************************

        Exec sql DECLARE distinctCrt Cursor For
         Select DISTINCT CNtype, CNseq#            //fetching Unique type and Seq combination
           From WMCNPCRT
            where CNco#=:dsInfo.co#
             and CNfnd=:dsInfo.fnd
             and CNcase=:dsInfo.case;

         Exec sql OPEN distinctCrt;

         Exec sql FETCH NEXT from distinctCrt
           INTO :Type, :Seq#;

         Dow (sqlcod = 0);

            Exsr emailFetch;

            Exec sql FETCH NEXT from distinctCrt
              INTO :Type, :Seq#;

         Enddo;

         exec sql CLOSE distinctCrt;

         If SysFlag = 'N';                        // none system claim type found

            exec sql DECLARE mycursor1 Cursor For

            //*** Beg Chg ***** 11/21/24 ******************************
            //***Select CNemal
            Select CNemal, CNtype, CNseq#
            //*** End Chg ***** 11/21/24 ******************************
              From WMCNP
               where CNco#=:dsInfo.co#
                and CNfnd=:dsInfo.fnd
                and CNcase=:dsInfo.case;

            exec sql OPEN mycursor1;

            exec sql FETCH NEXT from mycursor1
            //*** Beg Chg ***** 11/21/24 ******************************
              //***INTO :##email;
              INTO :##email,:##type,:##seq#;
            //*** End Chg ***** 11/21/24 ******************************

            //*** Beg Add ***** 11/21/24 ******************************
            Dow (sqlcod = 0);
               Exec sql DECLARE DistTypeCur15 Cursor For
                Select DISTINCT CNCRITVAL From WMCNPCRT
                 Where CNco# = :dsInfo.co#
                  and CNfnd =:dsInfo.fnd
                  and CNcase =:dsInfo.case
                  and CNtype =:##type and CNseq# =:##seq#
                  and CNcritid = 'Distribution';

               Exec sql OPEN DistTypeCur15;

               Exec sql FETCH NEXT from DistTypeCur15
                 INTO :xxType;

               Dow (sqlcod = 0);
                  If %Lookup(xxType:aryDistribution) > 0;
            //*** End Add ***** 11/21/24 ******************************
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
            //*** Beg Add ***** 11/21/24 ******************************
                  Endif;
                  Exec sql FETCH NEXT from DistTypeCur15
                    INTO :xxType;
               Enddo;
            Exec sql CLOSE DistTypeCur15;
            //*** End Add ***** 11/21/24 ******************************
               exec sql FETCH NEXT from mycursor1
                 INTO :##email;
            Enddo;

            exec sql CLOSE mycursor1;
         Endif;

        // For Staff Roles
        //********************** Staff Members for the claim **********//
        //*** Beg Del ***** 11/21/24 ***********************************
        //***Exec sql DECLARE mycursor2 Cursor For

        //***Select CMsupr,CMaMan,CMuMan,CMnrse,CMfeAd,
        //*** CMeSup, CMsdfA ,CMclex From WMCMP
        //*** where CMco# = :dsInfo.co#
        //***  and CMfnd = :dsInfo.fnd
        //***  and CMcase = :dsInfo.case;

        //***Exec sql OPEN mycursor2;

        //***Exec sql FETCH NEXT from mycursor2
        //***  INTO :Supervisor, :AccManag, :ClmManag,
        //***   :CaseNurse, :OutAdj, :ReinAdj, :FundAdj, :HeadingAdj;

        //***Exec sql CLOSE mycursor2;
        //*** End Del ***** 11/21/24 ***********************************

        Exec sql DECLARE CurAdjHdg Cursor For   //Heading for Adjuster

        Select CXadj From WMCXP
         where CXco# = :dsInfo.co#
          and CXfnd = :dsInfo.fnd;

        Exec sql OPEN CurAdjHdg;

        Exec sql FETCH NEXT from CurAdjHdg
          INTO :AdjHdg;                         // Heading loaded to AdjHdg

        Exec sql CLOSE CurAdjHdg;

        //********************** Staff Members for the claim **********//

        Exec sql DECLARE mycursor3 Cursor For

         SELECT DISTINCT CNCRITVAL
           FROM wmcnpcrt
            Where CNco# = :dsInfo.co#
             and CNfnd = :dsInfo.fnd
             and CNcase = :dsInfo.case;

        Exec sql OPEN mycursor3;

        Exec sql FETCH NEXT from mycursor3
          INTO :CritVal;

        Dow (sqlcod = 0);
           Exec sql DECLARE mycursor4 Cursor For

            SELECT S1CRITVAL
             FROM STSPPcrt1 WHERE S1type = :CritVal;

           Exec sql OPEN mycursor4;

           Exec sql FETCH NEXT from mycursor4
             INTO :CritVal2;

           Dow (sqlcod = 0);

              Select;
              When CritVal2 = 'Account Manager';
                 Chain (AccManag) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When CritVal2 = 'Case Nurse';
                 Chain (CaseNurse) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When CritVal2 = 'Claim Manager';
                 Chain (ClmManag) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When CritVal2 = 'Outside Adjuster';
                 Chain (OutAdj) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When CritVal2 = 'Reinsurance Adjuster';
                 Chain (ReinAdj) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When CritVal2 = 'Special Injury/Disability Fund Adjuster';
                 Chain (FundAdj) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When CritVal2 = 'Supervisor';
                 Chain (Supervisor) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When CritVal2 = AdjHdg;         // Claims Examiner Heading
                 Chain (HeadingAdj) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              Endsl;

              Exec sql FETCH NEXT from mycursor4
                INTO :CritVal2;

           EndDo;

           Exec sql CLOSE mycursor4;

           Exec sql FETCH NEXT from mycursor3
             INTO :CritVal;
        EndDo;

        exec sql CLOSE mycursor3;

        //*** End Add ***** 03/22/24 ***********************************
       Endsr;

       //*** Beg Add ***** 03/22/24 ***********************************
       //================================================================
       Begsr EmailFetch;        // System claim code, email fetch

        Select;
         //*** Beg Add ***** 08/06/24 ********************************
         //***** CLAIMANT *****//
         //*** Beg Chg ***** 11/21/24 ********************************
         //***When Type = 'Claimant';
         When Upper(Type) = 'CLAIMANT';
            Exec sql DECLARE DistTypeCur1 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur1;

            Exec sql FETCH NEXT from DistTypeCur1
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
         //*** End Chg ***** 11/21/24 ********************************

                  Exec sql
                   Select CDemad into :##email
                    From WMCDP
                    Where CDss# in (Select Cmss# from WMCMP
                     Where CMco#=:dsInfo.co#
                      and CMfnd=:dsInfo.fnd
                      and CMcase=:dsInfo.case);

                  If IsValidFetch(sqlstt);
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
                  Endif;
         //*** Beg Add ***** 11/21/24 ********************************

               Endif;

               Exec sql FETCH NEXT from DistTypeCur1
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur1;

         //*** End Add ***** 11/21/24 ********************************

         //*** End Add ***** 08/06/24 ********************************
        //***** TREATING PHYSICIAN *****//
         //*** Beg Chg ***** 11/21/24 ********************************
         //***When Type = 'Treating Physician';
         When Upper(Type) = 'TREATING PHYSICIAN';
            Exec sql DECLARE DistTypeCur2 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur2;

            Exec sql FETCH NEXT from DistTypeCur2
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
         //*** End Chg ***** 11/21/24 ********************************
         //*** Beg Chg ***** 11/21/24 ********************************
                  //***Exec sql
                  //*** Select TPemad, TPseq# into :##email, :##SeqVerify
                  //***  From WMTPP r
                  //***  Where TPco#=:dsInfo.co#
                  //***   and TPfnd=:dsInfo.fnd
                  //***   and TPcase=:dsInfo.case
                  //***   and TPprol ='TR'
                  //***   and (TPtodt >= (SELECT VARCHAR_FORMAT(CURRENT DATE,
                  //***   'YYYYMMDD') - 19000000 FROM sysibm.sysdummy1)
                  //***   OR TPtodt = 0)
                  //***  ORDER BY TPfrdt DESC, rrn(r) ASC
                  //***  FETCH first 1 row only;
                  //***If IsValidFetch(sqlstt);
                  //***   If ##SeqVerify = Seq#;
                  //***      LoadArrayRecipient(##email);
                  //***      If %parms >= 8;
                  //***         outClaim = 'Y';
                  //***      Endif;
                  //***   Endif;
                  //***Endif;
                  Exec sql
                   Select TPemad into :##email
                    From WMTPP r
                    Where TPco#=:dsInfo.co#
                     and TPfnd=:dsInfo.fnd
                     and TPcase=:dsInfo.case
                     and TPseq# =:seq#;

                  If IsValidFetch(sqlstt);
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
                  Endif;
         //*** End Chg ***** 11/21/24 ********************************
         //*** Beg Add ***** 11/21/24 ********************************

               Endif;

               Exec sql FETCH NEXT from DistTypeCur2
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur2;

         //*** End Add ***** 11/21/24 ********************************

         //***** RESPONSIBLE PARTY *****//
         //*** Beg Chg ***** 11/21/24 ********************************
         //***When Type = 'Responsible Party';
         When Upper(Type) = 'RESPONSIBLE PARTY';
            Exec sql DECLARE DistTypeCur3 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur3;

            Exec sql FETCH NEXT from DistTypeCur3
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
         //*** End Chg ***** 11/21/24 ********************************
                  Exec sql
                   Select SBrmad into :##email
                    From WMSBP
                    Where SBco#=:dsInfo.co#
                     and SBfnd=:dsInfo.fnd
                     and SBcase=:dsInfo.case;

                  If IsValidFetch(sqlstt);
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
                  Endif;
         //*** Beg Add ***** 11/21/24 ********************************

               Endif;

               Exec sql FETCH NEXT from DistTypeCur3
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur3;

         //*** End Add ***** 11/21/24 ********************************

         //***** INSURANCE COMPANY *****//
         //*** Beg Chg ***** 11/21/24 ********************************
         //***When Type = 'Insurance Company';
         When Upper(Type) = 'INSURANCE COMPANY';
            Exec sql DECLARE DistTypeCur4 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur4;

            Exec sql FETCH NEXT from DistTypeCur4
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
         //*** End Chg ***** 11/21/24 ********************************
                  Exec sql
                   Select SBimad into :##email
                    From WMSBP
                    Where SBco#=:dsInfo.co#
                     and SBfnd=:dsInfo.fnd
                     and SBcase=:dsInfo.case;

                  If IsValidFetch(sqlstt);
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
                  Endif;
         //*** Beg Add ***** 11/21/24 ********************************

               Endif;

               Exec sql FETCH NEXT from DistTypeCur4
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur4;

         //*** End Add ***** 11/21/24 ********************************

         //***** DEFENSE ATTORNEY ******//
         //*** Beg Chg ***** 11/21/24 ********************************
         //***When Type = 'Defense Attorney';
         When Upper(Type) = 'DEFENSE ATTORNEY';
            Exec sql DECLARE DistTypeCur5 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur5;

            Exec sql FETCH NEXT from DistTypeCur5
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
         //*** End Chg ***** 11/21/24 ********************************
                  Exec sql
                   Select LLemdd into :##email
                    From WMLLP
                    Where LLco#=:dsInfo.co#
                     and LLfnd=:dsInfo.fnd
                     and LLcase=:dsInfo.case;

                  If IsValidFetch(sqlstt);
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
                  Endif;
         //*** Beg Add ***** 11/21/24 ********************************

               Endif;

               Exec sql FETCH NEXT from DistTypeCur5
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur5;

         //*** End Add ***** 11/21/24 ********************************

         //***** PLAINTIFF ATTORNEY *****//
         //*** Beg Chg ***** 11/21/24 ********************************
         //***When Type = 'Plaintiff Attorney';
         When Upper(Type) = 'PLAINTIFF ATTORNEY';
            Exec sql DECLARE DistTypeCur6 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur6;

            Exec sql FETCH NEXT from DistTypeCur6
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
         //*** End Chg ***** 11/21/24 ********************************
                  Exec sql
                   Select LLemdp into :##email
                    From WMLLP
                    Where LLco#=:dsInfo.co#
                     and LLfnd=:dsInfo.fnd
                     and LLcase=:dsInfo.case;

                  If IsValidFetch(sqlstt);
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
                  Endif;
         //*** Beg Add ***** 11/21/24 ********************************

               Endif;

               Exec sql FETCH NEXT from DistTypeCur6
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur6;

         //*** End Add ***** 11/21/24 ********************************

         //***** ADDITIONAL PAYEE *******//
         When Type = 'Additional Payee';
         //*** Beg Chg ***** 11/21/24 ********************************
         //***When Type = 'Additional Payee';
         When Upper(Type) = 'ADDITIONAL PAYEE';
            Exec sql DECLARE DistTypeCur7 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur7;

            Exec sql FETCH NEXT from DistTypeCur7
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
         //*** End Chg ***** 11/21/24 ********************************
                  Exec sql
                   Select ACemdc into :##email
                    From WDACP
                    Where ACco# = :dsInfo.co#
                     and ACfnd = :dsInfo.fnd
                     and ACcase = :dsInfo.case
                     and ACseq# = :Seq#;

                  If IsValidFetch(sqlstt);
                     LoadArrayRecipient(##email);
                     If %parms >= 8;
                        outClaim = 'Y';
                     Endif;
                  Endif;
         //*** Beg Add ***** 11/21/24 ********************************

               Endif;

               Exec sql FETCH NEXT from DistTypeCur7
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur7;

         //***** ACCOUNT MANAGER *******//
         When Upper(Type) = 'ACCOUNT MANAGER';
            Exec sql DECLARE DistTypeCur8 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur8;

            Exec sql FETCH NEXT from DistTypeCur8
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
                  Chain (AccManag) PUP01;
                  LoadArrayRecipient(UPmail);
                  If %parms >= 8;
                     outClaim = 'Y';
                  Endif;

               Endif;

               Exec sql FETCH NEXT from DistTypeCur8
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur8;


         //****** CLAIM MANAGER ********//
         When Upper(Type) = 'CLAIM MANAGER';
            Exec sql DECLARE DistTypeCur9 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur9;

            Exec sql FETCH NEXT from DistTypeCur9
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
                  Chain (ClmManag) PUP01;
                  LoadArrayRecipient(UPmail);
                  If %parms >= 8;
                     outClaim = 'Y';
                  Endif;

               Endif;

               Exec sql FETCH NEXT from DistTypeCur9
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur9;

         //****** CASE NURSE  ********//
         When Upper(Type) = 'CASE NURSE';
            Exec sql DECLARE DistTypeCur10 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur10;

            Exec sql FETCH NEXT from DistTypeCur10
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
                  Chain (CaseNurse) PUP01;
                  LoadArrayRecipient(UPmail);
                  If %parms >= 8;
                     outClaim = 'Y';
                  Endif;

               Endif;

               Exec sql FETCH NEXT from DistTypeCur10
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur10;

         //****** OUTSIDE ADJUSTER ******//
         When Upper(Type) = 'OUTSIDE ADJUSTER';
            Exec sql DECLARE DistTypeCur11 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur11;

            Exec sql FETCH NEXT from DistTypeCur11
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
                  Chain (OutAdj) PUP01;
                  LoadArrayRecipient(UPmail);
                  If %parms >= 8;
                     outClaim = 'Y';
                  Endif;

               Endif;

               Exec sql FETCH NEXT from DistTypeCur11
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur11;

         //****** REINSURANCE ADJUSTER ******//
         When Upper(Type) = 'REINSURANCE ADJUSTER';
            Exec sql DECLARE DistTypeCur12 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur12;

            Exec sql FETCH NEXT from DistTypeCur12
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
                  Chain (ReinAdj) PUP01;
                  LoadArrayRecipient(UPmail);
                  If %parms >= 8;
                     outClaim = 'Y';
                  Endif;

               Endif;

               Exec sql FETCH NEXT from DistTypeCur12
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur12;

         //****** Special Injury/Disability Fund Adjuster ******//
         When Upper(Type) = 'SIDF ADJUSTER';
            Exec sql DECLARE DistTypeCur13 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur13;

            Exec sql FETCH NEXT from DistTypeCur13
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
                  Chain (FundAdj) PUP01;
                  LoadArrayRecipient(UPmail);
                  If %parms >= 8;
                     outClaim = 'Y';
                  Endif;

               Endif;

               Exec sql FETCH NEXT from DistTypeCur13
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur13;

         //****** Supervisor ******//
         When Upper(Type) = 'SUPERVISOR';
            Exec sql DECLARE DistTypeCur14 Cursor For
             Select DISTINCT CNCRITVAL From WMCNPCRT
              Where CNco# = :dsInfo.co#
               and CNfnd =:dsInfo.fnd
               and CNcase =:dsInfo.case
               and CNtype =:Type
               and CNcritid = 'Distribution';

            Exec sql OPEN DistTypeCur14;

            Exec sql FETCH NEXT from DistTypeCur14
              INTO :xxType;

            Dow (sqlcod = 0);
               If %Lookup(xxType:aryDistribution) > 0;
                  Chain (Supervisor) PUP01;
                  LoadArrayRecipient(UPmail);
                  If %parms >= 8;
                     outClaim = 'Y';
                  Endif;

               Endif;

               Exec sql FETCH NEXT from DistTypeCur14
                 INTO :xxType;

            Enddo;

            Exec sql CLOSE DistTypeCur14;

         //*** End Add ***** 11/21/24 ********************************

        Endsl;

       Endsr;

       //*** Beg Add ***** 09/10/24 **************************************
       //================================================================
       Begsr GetOutTypeRecipient;

        Exec sql DECLARE mycursor5 Cursor For

         SELECT S1CRITVAL
          FROM STSPPcrt1 WHERE S1type = :inruninfo.outtype;

        Exec sql OPEN mycursor5;

        Exec sql FETCH NEXT from mycursor5
          INTO :CritVal2;

        Dow (sqlcod = 0);

           Select;
           When CritVal2 = 'Account Manager';
              Chain (AccManag) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Case Nurse';
              Chain (CaseNurse) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Claim Manager';
              Chain (ClmManag) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Outside Adjuster';
              Chain (OutAdj) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Reinsurance Adjuster';
              Chain (ReinAdj) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Special Injury/Disability Fund Adjuster';
              Chain (FundAdj) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Supervisor';
              Chain (Supervisor) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Adjuster';
              Chain (HeadingAdj) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Policy Services';
              Chain (DSINFO.POLSRV) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Audit Reviewer';
              Chain (DSINFO.AUDRVW) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Claim Contact';
              Chain (DSINFO.CLMCON) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Underwriter';
              Chain (DSINFO.UNDWTR) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Audit Manager';
              Chain (DSINFO.AUDMGR) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Premium Accountant';
              Chain (DSINFO.PRMACC) PUP01;
              LoadArrayRecipient(UPmail);

           When CritVal2 = 'Loss Control Representative';
              Chain (DSINFO.LOSSREP) PUP01;
              LoadArrayRecipient(UPmail);

           Endsl;

           Exec sql FETCH NEXT from mycursor5
             INTO :CritVal2;

        EndDo;

        Exec sql CLOSE mycursor5;

        //*** Beg Add ***** 11/25/24 *****************************
        // Fetching emails as per Process Output Control

        Exec sql DECLARE ProcOutCon Cursor For

         SELECT CNtype, CNseq#, CNCRITVAL
          FROM WMCNPCRT WHERE
           CNCRITID = 'Process Output Control'
           and CNco#=:dsInfo.co#
           and CNfnd=:dsInfo.fnd
           and CNcase=:dsInfo.case;

        Exec sql OPEN ProcOutCon;

        Exec sql FETCH NEXT from ProcOutCon
          INTO :Type, :Seq#, :CritVal2;

        Dow (sqlcod = 0);
           If CritVal2 = inRunInfo.Outtype;
              Select;
              When Upper(Type) = 'CLAIMANT';
                 Exec sql
                  Select CDemad into :##email
                   From WMCDP
                   Where CDss# in (Select Cmss# from WMCMP
                    Where CMco#=:dsInfo.co#
                     and CMfnd=:dsInfo.fnd
                     and CMcase=:dsInfo.case);

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              When Upper(Type) = 'TREATING PHYSICIAN';
                 Exec sql
                  Select TPemad into :##email
                   From WMTPP
                   Where TPco#=:dsInfo.co#
                    and TPfnd=:dsInfo.fnd
                    and TPcase=:dsInfo.case
                    and TPseq# =:seq#;

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              When Upper(Type) = 'RESPONSIBLE PARTY';
                 Exec sql
                  Select SBrmad into :##email
                   From WMSBP
                   Where SBco#=:dsInfo.co#
                    and SBfnd=:dsInfo.fnd
                    and SBcase=:dsInfo.case;

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              When Upper(Type) = 'INSURANCE COMPANY';
                 Exec sql
                  Select SBimad into :##email
                   From WMSBP
                   Where SBco#=:dsInfo.co#
                    and SBfnd=:dsInfo.fnd
                    and SBcase=:dsInfo.case;

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              When Upper(Type) = 'DEFENSE ATTORNEY';
                 Exec sql
                  Select LLemdd into :##email
                   From WMLLP
                   Where LLco#=:dsInfo.co#
                    and LLfnd=:dsInfo.fnd
                    and LLcase=:dsInfo.case;

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              When Upper(Type) = 'PLAINTIFF ATTORNEY';
                 Exec sql
                  Select LLemdp into :##email
                   From WMLLP
                   Where LLco#=:dsInfo.co#
                    and LLfnd=:dsInfo.fnd
                    and LLcase=:dsInfo.case;

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              When Upper(Type) = 'ADDITIONAL PAYEE';
                 Exec sql
                  Select ACemdc into :##email
                   From WDACP
                   Where ACco# = :dsInfo.co#
                    and ACfnd = :dsInfo.fnd
                    and ACcase = :dsInfo.case
                    and ACseq# = :Seq#;

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              When Upper(Type) = 'ACCOUNT MANAGER';
                 Chain (AccManag) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When Upper(Type) = 'CLAIM MANAGER';
                 Chain (ClmManag) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When Upper(Type) = 'CASE NURSE';
                 Chain (CaseNurse) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When Upper(Type) = 'OUTSIDE ADJUSTER';
                 Chain (OutAdj) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When Upper(Type) = 'REINSURANCE ADJUSTER';
                 Chain (ReinAdj) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When Upper(Type) = 'SIDF ADJUSTER';
                 Chain (FundAdj) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              When Upper(Type) = 'SUPERVISOR';
                 Chain (Supervisor) PUP01;
                 LoadArrayRecipient(UPmail);
                 If %parms >= 8;
                    outClaim = 'Y';
                 Endif;

              Other;

                 Exec sql
                  Select CNemal into :##email
                   From WMCNP
                   Where CNtype =:Type
                     and CNSeq#=:Seq#
                     and CNco#=:dsInfo.co#
                     and CNfnd=:dsInfo.fnd
                     and CNcase=:dsInfo.case;

                 If IsValidFetch(sqlstt);
                    LoadArrayRecipient(##email);
                    If %parms >= 8;
                       outClaim = 'Y';
                    Endif;
                 Endif;

              Endsl;

           Endif;

           Exec sql FETCH NEXT from ProcOutCon
             INTO :Type, :Seq#, :CritVal2;

        EndDo;

        Exec sql CLOSE ProcOutCon;
        //*** End Add ***** 11/25/24 *****************************

       Endsr;
       //*** End Add ***** 09/10/24 **************************************
       //*** End Add ***** 03/22/24 ***********************************

       //================================================================
       Begsr GetContactsEmployer;
        Exec sql
         select FNpeoF into :strPeo
          from WMFNP
          where FNco# = :dsInfo.co# and FNfnd = :dsInfo.fnd;
        Select;
        When strPeo <> *blank;
           decEmp# = 1;
           decDiv = 0;
        Other;
           decEmp# = dsInfo.emp#;
           decDiv = dsInfo.div;
        Endsl;
        Reset isDivSpec;
        strSql = 'select *' +
         ' from WMECP' +
         ' where ECco#=' + %char(dsInfo.co#) +
         '  and ECfnd=' + %char(dsInfo.fnd) +
         '  and ECemp#=' + %char(decEmp#) +
         '  and ECdiv=' + %char(decDiv) +
         '  and ECdivSpec=''Y''';
        Exsr ResetCursorRecipient;
        Dou not IsValidFetch(sqlstt);
           Exec sql fetch next from cursorRecipient into :dsWMECP;
           Select;
           When not IsValidFetch(sqlstt) and isDivSpec;
              Reset sqlstt;
              isDivSpec = *off;
              strSql = 'select *' +
               ' from WMECP' +
               ' where ECco#=' + %char(dsInfo.co#) +
               '  and ECfnd=' + %char(dsInfo.fnd) +
               '  and ECemp#=' + %char(decEmp#) +
               '  and ECdivSpec<>''Y''';
              Exsr ResetCursorRecipient;
              Iter;
           When not IsValidFetch(sqlstt);
              Leave;
           Endsl;
           strSql = 'select *' +
            ' from WMECPcrt' +
            ' where ECco#=' + %char(dsWMECP.ECco#) +
            '  and ECfnd=' + %char(dsWMECP.ECfnd) +
            '  and ECemp#=' + %char(dsWMECP.ECemp#) +
            '  and ECdiv=' + %char(dsWMECP.ECdiv) +
            '  and ECseq#=' + %char(dsWMECP.ECseq#);
           Exsr ResetCursorCriteria;
           Dou not IsValidFetch(sqlstt);
              Exec sql fetch next from cursorCriteria into :dsWMECPcrt;
              Select;
              When not IsValidFetch(sqlstt);
                 sqlstt='00000';
                 Leave;
              When dsWMECPcrt.ECcritId = 'Distribution'
               and  %lookup(dsWMECPcrt.ECcritVal:aryDistribution) <> *zero;
                 LoadArrayRecipient(dsWMECP.ECeMal);
                 //*** Begin Add *** 01/16/23 *****************************
                 LoadInsuredArrayRecipient(dsWMECP.ECeMal);
                 //*** End   Add *** 01/16/23 *****************************
                 //*** Begin Add *** 09/26/22  WC2838 *********************
                 If %parms >= 4;
                    outInsured = 'Y';
                 Endif;
                 //*** End   Add *** 09/26/22  WC2838 *********************
              When dsWMECPcrt.ECcritId = 'Sub Category'
               and dsWMECPcrt.ECcritVal = strSubCat;
                 LoadArrayRecipient(dsWMECP.ECeMal);
                 //*** Begin Add *** 01/16/23 *****************************
                 LoadInsuredArrayRecipient(dsWMECP.ECeMal);
                 //*** End   Add *** 01/16/23 *****************************
                 //*** Begin Add *** 09/26/22  WC2838 *********************
                 If %parms >= 4;
                    outInsured = 'Y';
                 Endif;
                 //*** End   Add *** 09/26/22  WC2838 *********************
              When dsWMECPcrt.ECcritId = 'Process Output Control'
               and dsWMECPcrt.ECcritVal=inRunInfo.outType;
                 LoadArrayRecipient(dsWMECP.ECeMal);
                 //*** Begin Add *** 01/16/23 *****************************
                 LoadInsuredArrayRecipient(dsWMECP.ECeMal);
                 //*** End   Add *** 01/16/23 *****************************
                 //*** Begin Add *** 09/26/22  WC2838 *********************
                 If %parms >= 4;
                    outInsured = 'Y';
                 Endif;
                 //*** End   Add *** 09/26/22  WC2838 *********************
              Endsl;
           Enddo;
        Enddo;
       Endsr;

       //================================================================
       Begsr GetContactsProducer;
        Exec sql
         select * into :dsWMAPP
          from WMAPP
          where APco#=:dsInfo.co#
           and APfein=:dsInfo.feinAgt
           and APmod=:dsInfo.mod
           and APprod= :dsInfo.prod;
        If IsValidFetch(sqlstt);
           strSql = 'select *' +
            ' from WMAPPcrt' +
            ' where APco#=' + %char(dsInfo.co#) +
            '  and APfein=' + %char(dsInfo.feinAgt) +
            '  and APmod=' + %char(dsInfo.mod) +
            '  and APprod=' + %char(dsInfo.prod);
           Exsr ResetCursorCriteria;
           Dou not IsValidFetch(sqlstt);
              Exec sql fetch next from cursorCriteria into :dsWMAPPcrt;
              Select;
              When not IsValidFetch(sqlstt);
                 Reset sqlstt;
                 Leave;
              When dsWMAPPcrt.APcritId = 'Distribution'
               and  %lookup(dsWMAPPcrt.APcritVal:aryDistribution) <> *zero;
                 LoadArrayRecipient(dsWMAPP.APeMal);
                 //*** Begin Add *** 01/16/23 *****************************
                 LoadAgencyArrayRecipient(dsWMAPP.APeMal);
                 //*** End   Add *** 01/16/23 *****************************
                 //*** Begin Add *** 09/26/22  WC2838 *********************
                 If %parms >= 5;
                    outagent = 'Y';
                 Endif;
                 //*** End   Add *** 09/26/22  WC2838 *********************
              When dsWMAPPcrt.APcritId = 'Sub Category'
               and dsWMAPPcrt.APcritVal = strSubCat;
                 LoadArrayRecipient(dsWMAPP.APeMal);
                 //*** Begin Add *** 01/16/23 *****************************
                 LoadAgencyArrayRecipient(dsWMAPP.APeMal);
                 //*** End   Add *** 01/16/23 *****************************
                 //*** Begin Add *** 09/26/22  WC2838 *********************
                 If %parms >= 5;
                    outagent = 'Y';
                 Endif;
                 //*** End   Add *** 09/26/22  WC2838 *********************
              When dsWMAPPcrt.APcritId = 'Process Output Control'
               and dsWMAPPcrt.APcritVal=inRunInfo.outType;
                 LoadArrayRecipient(dsWMAPP.APeMal);
                 //*** Begin Add *** 01/16/23 *****************************
                 LoadAgencyArrayRecipient(dsWMAPP.APeMal);
                 //*** End   Add *** 01/16/23 *****************************
                 //*** Begin Add *** 09/26/22  WC2838 *********************
                 If %parms >= 5;
                    outAgent = 'Y';
                 Endif;
                 //*** End   Add *** 09/26/22  WC2838 *********************
              Endsl;
           Enddo;
        Endif;
       Endsr;

       //================================================================
       Begsr ResetCursorCriteria;
        Exec sql close cursorCriteria;
        Exec sql prepare sqlCriteria from :strSql;
        Exec sql declare cursorCriteria cursor for sqlCriteria;
        Exec sql open cursorCriteria;
       Endsr;

       //================================================================
       Begsr ResetCursorRecipient;
        Exec sql close cursorRecipient;
        Exec sql prepare sqlRecipient from :strSql;
        Exec sql declare cursorRecipient cursor for sqlRecipient;
        Exec sql open cursorRecipient;
       Endsr;
      /End-free
     pGetDefaultEmailRecipients...
     p                 e

      *=====================================================================
      * Get document information
      *=====================================================================
     pGetInfoDocument...
     p                 b
     dGetInfoDocument...
     d                 pi             1n
     d inRunInfo                           const likeds(dsRunInfo)
      //*** Begin Add *** 09/26/22  WC2838 **************************
     d inCat                          5    options(*nopass)
      //*** End   Add *** 09/26/22  WC2838 **************************

     ddecCo#           s              3  0 inz

      /Free
       decCo# = GetUserInfo(q1user);

       Reset aryDistribution;
       Reset i1;
       Exec sql
        select * into :dsSTDLP
         from STDLP
         where DLco# = :decCo#
          and (DLdcId = :inRunInfo.docId or DLinid = :inRunInfo.docId)
          and DLcat = :inRunInfo.docCat and DLsubC = :inRunInfo.docSub
          and DLlang = :inRunInfo.docLang
         limit 1;
       If IsValidFetch(sqlstt);
          Reset dsSTDEP;
          Exec sql
           select * into :dsSTDEP
            from STDEP
            where DEco# = :decCo# and DEcat = :DLcat and DEsubC = :DLsubC;
          strSql = 'select * from STDLPcrt ' +
           'where DLco# = ' + %char(DLco#) +
           ' and DLcat = ''' + %trim(DLcat) + '''' +
           ' and DLsubC = ''' + %trim(DLsubC) + '''' +
           ' and DLdcId = ''' + %trim(DLdcId) + '''' +
           ' and DLcritId = ''Distribution''';
          Exsr BuildCriteriaCursor;
          Dou not IsValidFetch(sqlstt);
             Exec sql fetch next from cursorCriteriaDoc into :dsSTDLPcrt;
             Select;
             When not IsValidFetch(sqlstt);
                Iter;
             Endsl;
             LoadArrayDistribution(dsSTDLPcrt.DLcritVal);
          Enddo;

          strSql = 'select * from STDEPcrt ' +
           'where DEco# = ' + %char(DLco#) +
           ' and DEcat = ''' + %trim(DLcat) + '''' +
           ' and DEsubC = ''' + %trim(DLsubC) + '''' +
           ' and DEdcId = ''' + %trim(DLdcId) + '''' +
           ' and DEcritId = ''Distribution''';
          Exsr BuildCriteriaCursor;
          Dou not IsValidFetch(sqlstt);
             Exec sql fetch next from cursorCriteriaDoc into :dsSTDEPcrt;
             Select;
             When not IsValidFetch(sqlstt);
                Iter;
             Endsl;
             LoadArrayDistribution(dsSTDEPcrt.DEcritVal);
          Enddo;
          //*** Begin Add *** 09/26/22  WC2838 *******************
          If %parms >= 2;
             inCat = DLcat;
          Endif;
          //*** End   Add *** 09/26/22  WC2838 *******************
          Return *on;
          //*** Begin Add *** 09/26/22  WC2838 *******************
          If %parms >= 2;
             Clear inCat;
          Endif;
          //*** End   Add *** 09/26/22  WC2838 *******************
       Endif;
       Return *off;

       //======================================================================
       Begsr BuildCriteriaCursor;
        Exec sql close cursorCriteriaDoc;
        Exec sql prepare sqlCriteriaDoc from :strSql;
        Exec sql declare cursorCriteriaDoc cursor for sqlCriteriaDoc;
        Exec sql open cursorCriteriaDoc;
       Endsr;
      /End-free
     pGetInfoDocument...
     p                 e

      *=====================================================================
      * Get output information
      *=====================================================================
     pGetInfoOutput...
     p                 b
     dGetInfoOutput...
     d                 pi             1n
     d inRunInfo                           const likeds(dsRunInfo)
      //*** Begin Add *** 09/26/22  WC2838 **************************
     d inCat                          5    options(*nopass)
      //*** End   Add *** 09/26/22  WC2838 **************************

      *** Beg Add ***** 08/07/24 ************************************
     ddecCo#           s              3  0 inz

       decCo# = GetUserInfo(q1user);
      *** End Add ***** 08/07/24 ************************************
      /Free
       Exec sql
        select * into :dsSTSPP
         from STSPP
         where SPtype = :inRunInfo.outType;
       If IsValidFetch(sqlstt);
          Exec sql close cursorSTSPPcrt;
          Exec sql declare cursorSTSPPcrt cursor for
           select *
            from STSPPcrt
            where SPtype = :SPtype
             and SPcritId = 'Distribution';
          Exec sql open cursorSTSPPcrt;
          Reset i1;
          Dou not IsValidFetch(sqlstt);
             Exec sql fetch next from cursorSTSPPcrt into :dsSTSPPcrt;
             Select;
             When not IsValidFetch(sqlstt);
                Iter;
             Endsl;
             LoadArrayDistribution(dsSTSPPcrt.SPcritVal);
          Enddo;

       //*** Beg Add ***** 03/22/24 ******************************

          Exec sql close cursorSTSPPcrt1;
          Exec sql declare cursorSTSPPcrt1 cursor for
           select *
            from STSPPcrt1
            where S1type = :SPtype
             and S1critId = 'Staff Roles';
          Exec sql open cursorSTSPPcrt1;

          Dou not IsValidFetch(sqlstt);
             Exec sql fetch next from cursorSTSPPcrt1 into :dsSTSPPcrt1;
             Select;
             When not IsValidFetch(sqlstt);
                Iter;
             Endsl;
             LoadArrayStaffRoles(dsSTSPPcrt1.S1critVal);
          Enddo;
       //*** End Add ***** 03/22/24 ******************************
          //*** Begin Add *** 09/26/22  WC2838 *******************
          If %parms >= 2;
             inCat = SPcat;
          Endif;
          //*** End   Add *** 09/26/22  WC2838 *******************
          Return *on;
          //*** Begin Add *** 09/26/22  WC2838 *******************
          If %parms >= 2;
             Clear inCat;
          Endif;
          //*** End   Add *** 09/26/22  WC2838 *******************
       Endif;
       //*** Beg Add ***** 08/07/24 ******************************
       Exec sql
        select * into :dsSTDLP
         from STDLP
         where DLco# = :decCo#
          and (DLdcId = :inRunInfo.docId or DLinid = :inRunInfo.docId)
          and DLcat = :inRunInfo.docCat and DLsubC = :inRunInfo.docSub
          and DLlang = :inRunInfo.docLang
          limit 1;
       If IsValidFetch(sqlstt);
          Exec sql close cursorSTDLPcrt1;
          Exec sql declare cursorSTDLPcrt1 cursor for
           select *
            from STDLPcrt1
            where D1co# = :DLco#
                and (D1dcId = :DLdcId or D1inid = :DLinid)
                and D1cat = :DLcat and D1subC = :DLsubC
                and D1lang = :DLlang and D1critId = 'Staff Roles';
          Exec sql open cursorSTDLPcrt1;

          Dou not IsValidFetch(sqlstt);
             Exec sql fetch next from cursorSTDLPcrt1 into :dsSTDLPcrt1;
             Select;
             When not IsValidFetch(sqlstt);
                Iter;
             Endsl;
             LoadArrayStaffRoles(dsSTDLPcrt1.D1critVal);
          Enddo;
          If %parms >= 2;
             inCat = DLcat;
          Endif;
          Return *on;
          If %parms >= 2;
             Clear inCat;
          Endif;
       Endif;
       //*** End Add ***** 08/07/24 ******************************
       Return *off;
      /End-free
     pGetInfoOutput...
     p                 e

      *** Beg Add *** 04/26/23 *********************************************
      *=====================================================================
      * Get Connect information
      *=====================================================================
     pGetInfoConnect...
     p                 b
     dGetInfoConnect...
     d                 pi             1n
     d inRunInfo                           const likeds(dsRunInfo)
     d*
     dintCo#           s              3  0 inz
     dstrCtgy          s              4    inz
     dstrType          s             10    inz
     d*
       intCo# = GetUserInfo(q1user);
       strCtgy = %subst(inRunInfo.runInfo:1:4);
       strType = %subst(inRunInfo.runInfo:5:10);

       Setll (intCo#:strCtgy:strType) pgt02;

       // If rec exists in Table file then it is a Connect Notification
       If %equal(WTGTL);
          return *on;
       Else;
          return *off;
       Endif;

     pGetInfoConnect...
     p                 e
      *** End Add *** 04/26/23 *********************************************

      *=====================================================================
      * Get the available tokens for the run type
      *=====================================================================
       //*** Begin Add *** 09/26/22  WC2838 **************************
      *=====================================================================
      * *NOTES: A distinct Run Type '@@@INITIALIZE@@@' was generated to
      * clear the Email Tokens Array and ultimately initializes the Email
      * Email Tokens Grid on the Add Document Setup screen.
      *=====================================================================
       //*** End   Add *** 09/26/22  WC2838 **************************
     pGetTokensEmail...
     p                 b                   export
     dGetTokensEmail...
     d                 pi             1n
     d inScreenPtr                     *   const
     d inRunType                    100    const varying options(*nopass)
      *** Beg Add *** 01/02/23 *** WC2875 *********************************
     d inPrcType                    100    const varying options(*nopass)
      *** End Add *** 01/02/23 *** WC2875 **************************
      *** Beg Add ***** 05/18/23 ************************************
     d inRunType2                   100    const varying options(*nopass)       //Second Run Type
      *** End Add ***** 05/18/23 ************************************

     dt1               s              5  0 inz
     d#ofTokens        c                   9999
     daryTokens        s             50    varying dim(#ofTokens)
     dstrToken         s             50    varying

     dstrJobN          s              6
     dstrRuntype       s            100    varying inz
      //*** Beg Add *** 01/02/23 *** WC2875 ********************************
     dstrPrctype       s            100    varying inz
     dintCo#           s                   like(ETCO#) inz
     dintRecCount      s             15s 0 inz
      //*** End Add *** 01/02/23 *** WC2875 ********************************
      //*** Beg Add ***** 05/18/23 ************************************
     dstrRunType2      s            100    varying inz
     dstrRun_Type      s              1
     daryRun_Type      s              1    dim(#ofTokens)
      //*** End Add ***** 05/18/23 ************************************
     dsysDt7           s              7  0

      /Free
       If %parms >= 2;
          strRunType = inRunType;
       Endif;
      *** Beg Add *** 01/02/23 *** WC2875 *********************************
       If %parms >= 3;
          strPrcType = inPrcType;
       Endif;
      *** End Add *** 01/02/23 **** WC2875 ********************************
       //*** Beg Add ***** 05/18/23 ************************************
       If %parms >= 4;
          strRunType2 = inRunType2;
       Endif;
       //*** End Add ***** 05/18/23 ************************************
       ptrEmail = inScreenPtr;
       Reset i1;
       // Tokens should be in alphabetical order
       //*** Begin Add *** 09/26/22  WC2838 ********************************
       // Clear Tokens Array when Initialize is requested.
       If strRunType = '@@@INITIALIZE@@@';
          Clear aryTokens(Index());
          //*** Beg Add ***** 05/18/23 ************************************
          Clear aryRun_Type(i1);
          //*** End Add ***** 05/18/23 ************************************
       Else;
       //*** End   Add *** 09/26/22  WC2838 ********************************
       //*** Beg Del *** 01/02/23 *** WC2875 ******************************
       //***aryTokens(Index()) = '#agency#';
       //***aryTokens(Index()) = '#company name#';
       //***Beg Chg *** 07/07/22 ****************************************** ***
       //***If strRunType = 'Policy'
       //***If strRunType = 'Policy Services'
       //*** End Chg *** 07/07/22 *********************************************
       //***or strRunType = 'Connect';
       //***aryTokens(Index()) = '#current billed#';
       //***Endif;
       //***aryTokens(Index()) = '#current date#';
       //***aryTokens(Index()) = '#division number#';
       //*** Beg Chg *** 07/07/22 *********************************************
       //***If strRunType = 'Policy'
       //***If strRunType = 'Policy Services'
       //*** End Chg *** 07/07/22 *********************************************
       //***or strRunType = 'Connect';
       //***aryTokens(Index()) = '#due date#';
       //***Endif;
       //***aryTokens(Index()) = '#insured fein#';
       //***aryTokens(Index()) = '#insured name#';
       //***If strRunType = 'Connect';
       //***aryTokens(Index()) = '#ip#';
       //***Endif;
       //***aryTokens(Index()) = '#logo#';
       //***aryTokens(Index()) = '#policy effective date#';
       //***aryTokens(Index()) = '#policy expiration date#';
       //***aryTokens(Index()) = '#policy number#';
       //*** Beg Chg *** 07/07/22 *********************************************
       //***If strRunType = 'Policy'
       //***If strRunType = 'Policy Services'
       //*** End Chg *** 07/07/22 *********************************************
       //*** or strRunType = 'Connect';
       //***   aryTokens(Index()) = '#posting date#';
       //***Endif;
       //*** Beg Chg *** 07/07/22 *********************************************
       //***//If strRunType = 'Policy'
       //***If strRunType = 'Policy Services'
       //*** End Chg *** 07/07/22 *********************************************
       //*** or strRunType = 'Connect';
       //***   aryTokens(Index()) = '#previous balance#';
       //***Endif;
       //***aryTokens(Index()) = '#producer#';
       //*** Beg Chg *** 07/07/22 *********************************************
       //***strRunType = 'Policy'
       //***If strRunType = 'Policy Services'
       //*** End Chg *** 07/07/22 *********************************************
       //*** or strRunType = 'Connect';
       //***   aryTokens(Index()) = '#report period#';
       //***Endif;
       //***If strRunType = 'Connect';
       //***   aryTokens(Index()) = '#submitter email#';
       //***Endif;
       //***If strRunType = 'Connect';
       //***   aryTokens(Index()) = '#submitter name#';
       //***Endif;
       //***If strRunType = 'Connect';
       //***   aryTokens(Index()) = '#submitter phone#';
       //***Endif;
       //*** Beg Chg *** 07/07/22 *********************************************
       //***//If strRunType = 'Policy'
       //***If strRunType = 'Policy Services'
       //*** End Chg *** 07/07/22 *********************************************
       //***If strRunType = 'Connect';
       //***   aryTokens(Index()) = '#total period payment#';
       //***Endif;
       //*** Begin Add *** 09/26/22  WC2838 **************************
       //***If strRunType = 'Policy Services'
       //*** or strRunType = 'Agency Services';
       //***   aryTokens(Index()) = '#audit manager email#';
       //***   aryTokens(Index()) = '#audit manager name#';
       //***   aryTokens(Index()) = '#audit manager phone#';
       //***   aryTokens(Index()) = '#audit reviewer email#';
       //***   aryTokens(Index()) = '#audit reviewer name#';
       //***   aryTokens(Index()) = '#audit reviewer phone#';
       //***   aryTokens(Index()) = '#claim contact email#';
       //***   aryTokens(Index()) = '#claim contact name#';
       //***   aryTokens(Index()) = '#claim contact phone#';
       //***   aryTokens(Index()) = '#policy services email#';
       //***   aryTokens(Index()) = '#policy services name#';
       //***   aryTokens(Index()) = '#policy services phone#';
       //***   aryTokens(Index()) = '#loss control rep email#';
       //***   aryTokens(Index()) = '#loss control rep name#';
       //***   aryTokens(Index()) = '#loss control rep phone#';
       //***   aryTokens(Index()) = '#premium accountant email#';
       //***   aryTokens(Index()) = '#premium accountant name#';
       //***   aryTokens(Index()) = '#premium accountant phone#';
       //***   aryTokens(Index()) = '#underwriter email#';
       //***   aryTokens(Index()) = '#underwriter name#';
       //***   aryTokens(Index()) = '#underwriter phone#';
       //*** Endif;
       //*** Endif;
       //*** End   Add *** 09/26/22  WC2838 **************************
       //*** End Del *** 01/02/23 *** WC2875 *******************************
          //*** Beg Add *** 01/02/23 *** WC2875 ****************************
          Setll *loval PET00;
          Dou %Eof(STETL);
             Read PET00;
             If %Eof(STETL);
                Leave;
             Endif;

             Clear intCo#;
             Clear strToken;
             //*** Beg Add ***** 05/18/23 ************************************
             Clear strRun_Type;
             //*** End Add ***** 05/18/23 ************************************
             Select;
                //*** Beg Chg ***** 05/25/23 ************************************
                // Process Policy Run Types
                //***When (strRunType = 'Policy' or
                //*** strRunType = 'Policy Services') and ETpolicy = 'Y';
                //***   intCo# = ETco#;
                //***   strToken = ETtoken;

                // Process Claim Run Types
                //***When (strRunType = 'Claim' or
                //*** strRunType = 'Claim Services') and ETclaim = 'Y';
                //***   intCo# = ETco#;
                //***  strToken = ETtoken;

                // Process Agency Run Types
                //***When (strRunType = 'Agency' or
                //*** strRunType = 'Agency Services') and ETagent = 'Y';
                //***   intCo# = ETco#;
                //***   strToken = ETtoken;

                // Process Medical Case Types
                //***When (strRunType = 'Medical Case' or
                //*** strRunType = 'Managed Care Services') and ETmangc = 'Y';
                //***   intCo# = ETco#;
                //***   strToken = ETtoken;

                // Process Report Types
                //***When (strRunType = 'Report' or
                //*** strRunType = 'Report Services') and ETreport = 'Y';
                //***   intCo# = ETco#;
                //***   strToken = ETtoken;

                // Process Pre-Certification Types
                //***When (strRunType = 'Pre-Certification' or
                //*** strRunType = 'Pre-Certification Services') and
                //*** ETprecert = 'Y';
                //***   strToken = ETtoken;
                //New
                // Process Policy Run Types: Policy Services
                When strRunType = 'PS' and ETpolicy = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;

                // Process Claim Run Types: Claims Services
                When strRunType = 'CS' and ETclaim = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;

                // Process Agency Run Types: Agency Services
                When strRunType = 'AS' and ETagent = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;

                // Process Medical Case Types: Managed Care Services
                When strRunType = 'MC' and ETmangc = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;

                // Process Report Types: Report Services
                When strRunType = 'RP' and ETreport = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;

                // Process Pre-Certification Types: Pre-Certification Services
                When strRunType = 'PC' and ETprecert = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;
                //*** End Chg ***** 05/25/23 ************************************

                // Process Connect Quote Type
                When strRunType = 'Connect Quote' and ETquote = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;

                // Process Connect Paygo Type
                When strRunType = 'Connect PayGo' and ETpaygo = 'Y';
                   intCo# = ETco#;
                   strToken = ETtoken;

             Endsl;


             Select;
             // If process type has been passed into the program then
             // retrieve email tokens from the Process Type file.
             When strPrcType <> ' ';

                // Check if recs in email token process type file.
                Exec sql
                Select Count(*) into :intRecCount From STET1L;
                Chain (intCo#:strToken:strPrcType) PET10;

                // If no records exist or email token process type     file
                // If rec is found include the token else exclude it.  file
                If (intRecCount = 0) or %found(STET1L);
                   aryTokens(Index()) = strToken;
                   //*** Beg Add ***** 05/18/23 **********************************
                   aryRun_Type(i1) = *Blank;
                   //*** End Add ***** 05/18/23 **********************************
                Endif;

             Other;
                aryTokens(Index()) = strToken;
                //*** Beg Add ***** 05/18/23 **********************************
                aryRun_Type(i1) = *Blank;
                //*** End Add ***** 05/18/23 **********************************
             Endsl;
          Enddo;
          //*** End Add *** 01/02/23 *** WC2875 ****************************
          //*** Beg Add ***** 05/18/23 ********************************
          // For FTP: Load Agency Service tokens
          If strRunType2 <> ' ';
             Setll *loval PET00;
             Dou %Eof(STETL);
                Read PET00;
                If %Eof(STETL);
                   Leave;
                Endif;
                Clear strToken;
                If ETagent = 'Y';
                   intCo# = ETco#;
                   aryTokens(Index()) = ETtoken;
                   aryRun_Type(i1) = 'A';
                Endif;
             Enddo;
          Endif;
          //*** End Add ***** 05/18/23 ********************************

       Endif;
       //*** End   Add *** 09/26/22  WC2838 **************************

       strJobN = %char(q1JobN);
       sysDt7 = DateTo7(SystemDate());
       Exec sql
        delete from SXTOKENS
         where job = :strJobN;
       For t1 = 1 to #ofTokens;
          If aryTokens(t1) = *blanks;
             Iter;
          Endif;
          strToken = aryTokens(t1);
          //*** Beg Chg ***** 05/18/23 ********************************
          //***Exec sql
          //*** insert into SXTOKENS (token, job, crt_date)
          //***  values(:strToken, :strJobN, :sysDt7);
          strRun_Type = aryRun_Type(t1);
          Exec sql
           insert into SXTOKENS (token, job, crt_date, Run_Type)
            values(:strToken, :strJobN, :sysDt7, :strRun_Type);
          //*** End Chg ***** 05/18/23 ********************************
       Endfor;

       scTokenCrt = 'job = ''' + strJobN + '''';

       Return *on;
      /End-free
     pGetTokensEmail...
     p                 e

      *=====================================================================
      * Increment index based on ptr
      *=====================================================================
     pIndex...
     p                 b
     dIndex...
     d                 pi             5  0
      /Free
       i1 += 1;
       Return i1;
      /End-free
     pIndex...
     p                 e

      *=====================================================================
      * Load arrry of distribution types for document/output
      *=====================================================================
     pLoadArrayDistribution...
     p                 b
     dLoadArrayDistribution...
     d                 pi
     d inDistribution                25    const
      /Free
       If %lookup(inDistribution:aryDistribution) = *zero;
          aryDistribution(Index()) = inDistribution;
       Endif;
      /End-free
     pLoadArrayDistribution...
     p                 e

      *** Beg Add ***** 03/22/24 *******************************************
      *=====================================================================
      * Load arrry of Staff Roles for document/output
      *=====================================================================
     pLoadArrayStaffRoles...
     p                 b
     dLoadArrayStaffRoles...
     d                 pi
     d inStaffRoles                  25    const
      /Free
       If %lookup(inStaffRoles:aryStaffRoles) = *zero;
          aryStaffRoles(Index()) = inStaffRoles;
       Endif;
      /End-free
     pLoadArrayStaffRoles...
     p                 e

      *** End Add ***** 03/22/24 *******************************************
      *=====================================================================
      * Load arrry of recipients
      *=====================================================================
     pLoadArrayRecipient...
     p                 b
     dLoadArrayRecipient...
     d                 pi
     d inEmail                       60    const
      /Free
       If %lookup(inEmail:aryRecipient) = *zero;
          aryRecipient(Index()) = inEmail;
       Endif;
      /End-free
     pLoadArrayRecipient...
     p                 e

       //*** Begin Add *** 01/16/23 ****************************************
      *=====================================================================
      * Load Agency array of recipients
      *=====================================================================
     pLoadAgencyArrayRecipient...
     p                 b
     dLoadAgencyArrayRecipient...
     d                 pi
     d inEmail                       60    const

     d agencyIndex     s              4  0

      /Free
       agencyIndex = %lookup(inEmail:aryAgencyRecipient);
       If agencyIndex = *zero;
          agencyIndex = %lookup(' ':aryAgencyRecipient);
          If agencyIndex <> 0;
             aryAgencyRecipient(agencyIndex) = inEmail;
          Endif;
       Endif;
      /End-free

     pLoadAgencyArrayRecipient...
     p                 e

      *=====================================================================
      * Load Insured array of recipients
      *=====================================================================
     pLoadInsuredArrayRecipient...
     p                 b
     dLoadInsuredArrayRecipient...
     d                 pi
     d inEmail                       60    const

     d insuredIndex    s              4  0

      /Free
       insuredIndex = %lookup(inEmail:aryInsuredRecipient);
       If insuredIndex = *zero;
          insuredIndex = %lookup(' ':aryInsuredRecipient);
          If insuredIndex <> 0;
             aryInsuredRecipient(insuredIndex) = inEmail;
          Endif;
       Endif;
      /End-free

     pLoadInsuredArrayRecipient...
     p                 e
       //*** End   Add *** 01/16/23 ****************************************
      *=====================================================================
      * Load the default Email information (subject/body/recipients) to screen
      *  inRunInfo: the document id or the process type
      *  inRrnType:     RRN from file
      *  ============================
      *  Policy         WDELP
      *  Claim          WMCMP
      *  Agency         WMAGP
      *  Agent          WMAAP
      *=====================================================================
     pLoadDefaultEmailData...
     p                 b                   export
     dLoadDefaultEmailData...
     d                 pi             1n
     d inScreenPtr                     *   const
     d inRunInfo                           const likeds(dsRunInfo)
     d inRrn                          9  0 const options(*nopass)
      //*** Beg Chg *** 01/02/23  WC2875 ****************************
     d***inRrnType                     25    const varying  options(*nopass)
     d inRrnType                    100    const varying  options(*nopass)
      //*** End Chg *** 01/02/23  WC2875 ****************************
      //*** Beg Add *** 01/02/23  WC2875 ****************************
     d inPrcType                    100    const varying  options(*nopass)
      //*** End Add *** 01/02/23  WC2875 ****************************

     disFound          s              1n   inz
     dstrBody          s               a   len(16000) varying inz
     dstrEmlBody       s                   like(scEmlBody)
     dstrEmlSub        s                   like(scEmlSub)
     dstrEmlUsrO       s                   like(scEmlUsrO)
       //*** Beg Chg *** 01/02/23  WC2875 ****************************
     d***strRRNType       s             25    varying inz
     dstrRRNType       s            100    varying inz
      //*** End Add *** 01/02/23  WC2875 **************************
      //*** Beg Add *** 01/02/23  WC2875 **************************
     dstrPrcType       s            100    varying inz
      //*** End Add *** 01/02/23  WC2875 **************************
     dstrSubject       s               a   len(100)   varying inz
      //*** Begin Add *** 09/26/22  WC2838 **************************
     dxxCat            s              5    static inz
      //*** End   Add *** 09/26/22  WC2838 **************************

      /Free
       If %parms >= 4;
          strRRNType = inRrnType;
       Endif;
      //*** Beg Add *** 01/02/23  WC2875 **************************
       If %parms >= 5;
          strPrcType = inPrcType;
       Endif;
      //*** End Add *** 01/02/23 *** WC2875 ********************************

       ptrEmail = inScreenPtr;

       //*** Begin Add *** 09/26/22  WC2838 **************************
       If strRRNType <> ' ';
       //*** End   Add *** 09/26/22  WC2838 **************************
       //*** Beg Chg *** 01/02/23 *** WC2875 *******************************
       //***GetTokensEmail(inScreenPtr:strRrnType);
       GetTokensEmail(ptrEmail:strRrnType:strPrcType);
       //*** End Chg *** 01/02/23 *** WC2875 *******************************
       //*** Begin Add *** 09/26/22  WC2838 **************************
       Endif;
       //*** End   Add *** 09/26/22  WC2838 **************************

       // The run type should not be the same across Documents and Output;
       //  therefore we will process both routines as only one will produce results
       Select;

       //*** Begin Chg *** 09/26/22  WC2838 **************************
       //******When GetInfoDocument(inRunInfo);
       When GetInfoDocument(inRunInfo:xxCat);
       //*** End   Chg *** 09/26/22  WC2838 **************************
          Exsr DefaultsDocuments;

       //*** Begin Chg *** 09/26/22  WC2838 **************************
       //******When GetInfoOutput(inRunInfo);
       When GetInfoOutput(inRunInfo:xxCat);
       //*** End   Chg *** 09/26/22  WC2838 **************************
          Exsr DefaultsOutput;
       Endsl;

       scEmlBody = strEmlBody;
       scEmlSub  = strEmlSub;
       scEmlUsrO = strEmlUsrO;

       //*** Begin Add *** 09/26/22  WC2838 **************************
       If strRRNType = ' ';
          //*** Beg Chg ***** 05/25/23 *******************************
          //***xxCo# = GetUserInfo(q1user);
          //***xxTBid = 'DOCS';
          //***xxKey = xxCat;
          //***Chain (xxCo#:xxTbid:xxKey) pgt00;
             //***If %found(STGTL);
             //***   strRRNType = %trim(GTDesc);
             //***Endif;
          strRRNType = xxCat;
          //*** End  Chg ***** 05/25/23 *******************************
          //*** Beg Chg *** 01/02/23 *** WC2875 ****************************
          //***GetTokensEmail(inScreenPtr:strRrnType);
          GetTokensEmail(inScreenPtr:strRrnType:strPrcType);
          //*** End Chg *** 01/02/23 ***  WC2875 ***************************
       Endif;
       //*** End   Add *** 09/26/22  WC2838 **************************
       Return isFound;

       //==================================================
       Begsr DefaultsDocuments;
        isFound = *on;
        strEmlBody = DLemlBody;
        strEmlSub  = DLemlSub;
        strEmlUsrO = DLemlUsrO;
        //*** Beg Chg *** 05/08/23 *****************************************
        //***If DEemlBody <> *blanks;
        If DEemlBody <> *blanks and strEmlBody = ' ';
        //*** End Chg *** 05/08/23 *****************************************
           strEmlBody = DEemlBody;
        Endif;
        //*** Beg Chg *** 05/08/23 *****************************************
        //***If DEemlSub  <> *blanks;
        If DEemlSub  <> *blanks and strEmlSub = ' ';
        //*** Beg Chg *** 05/08/23 *****************************************
           strEmlSub  = DEemlSub;
        Endif;
        //*** Beg Chg *** 05/08/23 *****************************************
        //***If DEemlUsrO <> *blanks;
        If DEemlUsrO <> *blanks and strEmlUsrO = ' ';
        //*** End Chg *** 05/08/23 *****************************************
           strEmlUsrO = DEemlUsrO;
        Endif;
       Endsr;

       //==================================================
       Begsr DefaultsOutput;
        isFound = *on;
        strEmlBody = SPemlBody;
        strEmlSub  = SPemlSub;
        strEmlUsrO = SPemlUsrO;
       Endsr;
      /End-free
     pLoadDefaultEmailData...
     p                 e

      *=====================================================================
      * Replace tokens with the appropriate associated data
      * - For Connect notifications, inRunInfo will be Category concat w/ Type
      *=====================================================================
     pReplaceTokensEmail...
     p                 b                   export
     dReplaceTokensEmail...
     d                 pi              a   len(40000) varying
     d inString                        a   const len(32000) varying
     d inRunInfo                           const likeds(dsRunInfo)
     d inRrn                          9  0 const
      //*** Beg Chg *** 01/02/23 *** WC2875 ********************************
     d***inRrnType                     25    const varying  options(*nopass)
     d inRrnType                    100    const varying  options(*nopass)
      //*** End Chg *** 01/02/23 *** WC2875 ********************************
     d inOtherInfo                     a   const len(16000) varying             json format
     d                                     options(*nopass)

     ddsSTLGP        e ds                  extname(STLGP)    inz
     ddsWINOTIP      e ds                  extname(WINOTIP)  inz
     ddsWINOTIP1     e ds                  extname(WINOTIP1) inz
     ddsWISRPREM     e ds                  extname(WISRPREM) inz
     d***  Begin Add  ***  06/03/22  *********************
     ddsWMAGP        e ds                  extname(WMAGP)    inz
     d***  End   Add  ***  06/03/22  *********************
     ddsWMFNP        e ds                  extname(WMFNP)    inz
     ddsWMRPP        e ds                  extname(WMRPP)    inz
      *** Beg Add *** 01/02/23 *** WC2875 *********************************
       // Data Structure for defining subfields passed in the optional
       // inOtherInfo parameter.
       DCL-DS dsOthInfo QUALIFIED;
          RptDate CHAR(8);
       END-DS;
      *** End Add *** 01/02/23 *** WC2875 *********************************

     dxxCo#            s              3  0 inz
     dxxFnd            s              3  0 inz
     dxxEmp#           s              9  0 inz
     dxxDiv            s              5  0 inz
     dxxFyr            s              3  0 inz

       Dcl-S chrFrequency Char(1);
       Dcl-S decAgt#  Packed(5:0);
       Dcl-S decBalancePrv Packed(9:0);
       Dcl-S decCurrentBilled Packed(9:0);
       Dcl-S decPmtTotal Packed(9:0);
       Dcl-S decProd# Packed(5:0);
       Dcl-S decReportDate zoned(8:0) inz;
       Dcl-S decSubmitterPhone zoned(10:0) inz;
       Dcl-S dtCurrent Date(*USA) inz(*sys);
       Dcl-S intDayOfMth Zoned(2:0);
       Dcl-S intPostCymd Packed(7:0);
       Dcl-s isLogoTest ind inz(*off);
       Dcl-S outString like(inString);
       Dcl-s strAgtName Char(60);
       Dcl-s strLogo varchar(300) inz;
       Dcl-S strPolicy# Like(RPcap#);
       Dcl-s strProcess varchar(25);
       Dcl-S strProdName Char(60);
       Dcl-S strReportPeriod Char(23);
       Dcl-S strSubmitterEmail varchar(100) inz;
       Dcl-S strSubmitterName  varchar(100) inz;
       Dcl-S strSubmitterPhone varchar(100) inz;
       Dcl-s strUser char(10) inz;
       Dcl-S strToken VarChar(25);
       Dcl-S nullDate date(*usa) inz;
       Dcl-S workBegDate Date(*USA) inz;
       Dcl-S workEndDate Date(*USA) inz;
       Dcl-S workPolFrDt Date(*USA) inz;
       Dcl-S workDueDate Date(*USA) inz;
       //*** Beg Add *** 03/23/23 *** 107726 *******************************
       Dcl-S inStaffCode varchar(3) inz;
       Dcl-S strStaffName  varchar(100) inz;
       Dcl-S strStaffEmail varchar(100) inz;
       Dcl-S strStaffPhone varchar(100) inz;
       Dcl-S decStaffArea Like(UPPHNA) inz;
       Dcl-S decStaffPhone Like(UPPHN#) inz;
       //*** End Add *** 03/23/23 *** 107726 *******************************
       //*** Beg Add ***** 08/26/24 ***************************************
       Dcl-S strClaimNbr varchar(100) inz;
       //*** Beg Add ***** 08/26/24 ***************************************

       outString = inString;
       //*** Beg Add *** 01/02/23 *** WC2875 *******************************
       // If Other Information optional parmeter was passed then
       // extract the other info subroutine.
       If %parms >= 5;
          Exsr ExtractOtherInfo;
       Endif;
       //*** End Add *** 01/02/23 *** WC2875 *******************************

       // Get agent info
       strSql = SetSqlStmt(inRrn:inRrnType);
       Reset dsInfo;
       Exec sql close cursorTokensInfo;
       Exec sql prepare sqlTokensInfo from :strSql;
       Exec sql declare cursorTokensInfo cursor for sqlTokensInfo;
       Exec sql open cursorTokensInfo;
       Exec sql fetch next from cursorTokensInfo into :dsInfo :dsInfoNull;

       Exsr LoadOtherInfo;
       Exsr LoadConnectInfo;
       Exsr LoadLogo;

       outString = %scanrpl('#agency#':%Trim(strAgtName):outString);
       outString = %scanrpl('#company name#':%Trim(FNname):outString);
       If NOctgy = 'N001';
          outString = %scanrpl('#current billed#':
           %EditC(decCurrentBilled:'J':'$'):outString);
       Endif;
       outString = %scanrpl('#current date#':%Char(dtCurrent:*Usa/):outString);
       outString = %scanrpl('#division number#':
        %Editc(dsInfo.div:'X'):outString);
       If NOctgy = 'N001';
          Monitor;
             outString =
              %scanrpl('#due date#':%Char(workDueDate:*Usa/):outString);
          On-error;
             outString = %scanrpl('#due date#':' ':outString);
          Endmon;
       Endif;
       outString = %scanrpl('#insured fein#':
        %Editc(dsInfo.feinEmp:'X'):outString);
       outString = %scanrpl('#insured name#':%Trim(dsInfo.nameEmp):outString);
       outString = %scanrpl('#ip#':%trim(N1portIP):outString);
       outString = %scanrpl('#logo#':strLogo:outString);
       Monitor;
          outString = %scanrpl('#policy effective date#':
           %Char(%Date(dsInfo.pfDt:*Cymd):*Usa/):outString);
       On-error;
          outString = %scanrpl('#policy effective date#':' ':outString);
       Endmon;
       Monitor;
          outString = %scanrpl('#policy expiration date#':
           %Char(%Date(dsInfo.ptDt:*Cymd):*Usa/):outString);
       On-error;
          outString = %scanrpl('#policy expiration date#':' ':outString);
       Endmon;
       outString = %scanrpl('#policy number#':%Trim(strPolicy#):outString);
       If NOctgy = 'N001';
          outString = %scanrpl('#posting date#':%Char(%Date():*Usa/):outString);
       Endif;
       outString = %scanrpl('#previous balance#':
        %EditC(decBalancePrv:'J':'$'):outString);
       outString = %scanrpl('#producer#':%Trim(strProdName):outString);
       // Currently, report date is passed in the request JSON
       If NOctgy = 'N001';
          outString = %scanrpl('#report period#':strReportPeriod:outString);
       EndIf;
       outString = %scanrpl('#submitter email#':strSubmitterEmail:outString);
       outString = %scanrpl('#submitter name#':strSubmitterName:outString);
       outString = %scanrpl('#submitter phone#':strSubmitterPhone:outString);
       If NOctgy = 'N001';
          outString = %scanrpl('#total period payment#':
           %EditC(decPmtTotal:'J':'$'):outString);
       EndIf;
       outString = %scanrpl('#userID#':%Trim(strUser):outString);

       //*** Beg Add *** 08/26/24 **********************************
       Monitor;
          outString = %scanrpl('#accident date#':
           %Char(%Date(dsInfo.acDt:*Cymd):*Usa/):outString);
       On-error;
          outString = %scanrpl('#accident date#':' ':outString);
       Endmon;
       outString = %scanrpl('#claimant name#':%Trim(dsInfo.nameClm):outString);
       //*** Beg claim number combo ********************************
       clear strClaimNbr;
       strClaimNbr = %EditC(dsInfo.fnd:'X') +
        %EditC(dsInfo.fyr:'X') + %EditC(dsInfo.case:'X');
        strClaimNbr = %subst(strClaimNbr:1:3) + '-' +
        %subst(strClaimNbr:4:3) + '-' +
        %subst(strClaimNbr:7:7);
       outString = %scanrpl('#claim number#':%trim(strClaimNbr):
        outString);
       //*** End claim number combo ********************************
       Select;
          When dsInfo.lostTmFl = 'I';
             outString = %scanrpl('#claim type#':'indemnity':outString);
          When dsInfo.lostTmFl = 'M';
             outString = %scanrpl('#claim type#':'medical only':outString);
          When dsInfo.lostTmFl = 'R';
             outString = %scanrpl('#claim type#':'record only':outString);
       Endsl;

       // Get adjuster staff info
       inStaffCode = dsInfo.adjStaff;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#adjuster email#':%trim(strstaffEmail):
        outString);
       outString = %scanrpl('#adjuster name#':%trim(strstaffName):
        outString);
       outString = %scanrpl('#adjuster phone#':%trim(strstaffPhone):
        outString);

       //*** End Add *** 08/26/24 **********************************

       //*** Beg Add *** 03/23/23 *** 107726 *******************************
       // Get policy services staff info
       outString = %scanrpl('#policy services staff code#':
        %Trim(dsInfo.polSrv):outString);
       inStaffCode = dsInfo.polSrv;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#policy services email#':%trim(strstaffEmail):
        outString);
       outString = %scanrpl('#policy services name#':%trim(strstaffName):
        outString);
       outString = %scanrpl('#policy services phone#':%trim(strstaffPhone):
        outString);

       // Get underwriter staff info
       outString = %scanrpl('#underwriter staff code#':
        %Trim(dsInfo.undWtr):outString);
       inStaffCode = dsInfo.undWtr;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#underwriter email#':
        %trim(strstaffEmail):outString);
       outString = %scanrpl('#underwriter name#':
        %trim(strstaffName):outString);
       outString = %scanrpl('#underwriter phone#':
        %trim(strstaffPhone):outString);

       // Get premium accountant staff info
       outString = %scanrpl('#premium accountant staff code#':
        %Trim(dsInfo.prmAcc):outString);
       inStaffCode = dsInfo.prmAcc;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#premium accountant email#':
        %trim(strstaffEmail):outString);
       outString = %scanrpl('#premium accountant name#':
        %trim(strstaffName):outString);
       outString = %scanrpl('#premium accountant phone#':
        %trim(strstaffPhone):outString);

       // Get audit reviewer staff code info
       outString = %scanrpl('#audit reviewer staff code#':
        %Trim(dsInfo.audRVW):outString);
       inStaffCode = dsInfo.audRvw;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#audit reviewer email#':
        %trim(strstaffEmail):outString);
       outString = %scanrpl('#audit reviewer name#':
        %trim(strstaffName):outString);
       outString = %scanrpl('#audit reviewer phone#':
        %trim(strstaffPhone):outString);

       // Get audit manager staff code info
       outString = %scanrpl('#audit manager staff code#':
        %Trim(dsInfo.audMGR):outString);
       inStaffCode = dsInfo.audMGR;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#audit manager email#':
        %trim(strstaffEmail):outString);
       outString = %scanrpl('#audit manager name#':
        %trim(strstaffName):outString);
       outString = %scanrpl('#audit manager phone#':
        %trim(strstaffPhone):outString);

       // Get loss control rep staff info
       outString = %scanrpl('#loss control rep staff code#':
        %Trim(dsInfo.lossrep):outString);
       inStaffCode = dsInfo.lossRep;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#loss control rep email#':
        %trim(strstaffEmail):outString);
       outString = %scanrpl('#loss control rep name#':
        %trim(strstaffName):outString);
       outString = %scanrpl('#loss control rep phone#':
        %trim(strstaffPhone):outString);

       // Get claim contact staff info
       outString = %scanrpl('#claim contact staff code#':
        %Trim(dsInfo.clmCon):outString);
       inStaffCode = dsInfo.clmCon;
       Exsr LoadStaffInfo;
       outString = %scanrpl('#claim contact email#':
        %trim(strstaffEmail):outString);
       outString = %scanrpl('#claim contact name#':
        %trim(strstaffName):outString);
       outString = %scanrpl('#claim contact phone#':
        %trim(strstaffPhone):outString);
       //*** End Add *** 03/23/23 *** 107726 *******************************
       Return outString;

       //===================================================
       Begsr ExtractOtherInfo;
         strUser = q1User;
         // Extract json information from inOtherInfo
         //   Connect will need to send in:
         //   - user => strUser
         //   - report date => decReportDate
         //*** Beg Add *** 01/02/23 *** WC2875 ****************************
         // If run type is related to a policy type then parse and extract
         // the report date.
         If %subst(inRunInfo:1:4) = 'N001';
            dsOthInfo = inOtherInfo;
            Monitor;
               decReportDate = %Dec((dsOthInfo.RptDate):8:0);
            On-Error;
               clear decReportDate;
            Endmon;
         Endif;
       //*** End Add *** 01/02/23 *** WC2875 *******************************
       Endsr;

       //===================================================
       Begsr LoadConnectInfo;
         NOctgy = %subst(inRunInfo.runInfo:1:4);
         NOtype = %subst(inRunInfo.runInfo:5:10);
         Exec sql
          select * into :dsWINOTIP
           from WINOTIP
           where Noco# = :dsInfo.co# and NOctgy = :NOctgy and NOtype = :NOtype;
         Exec sql
          select * into :dsWINOTIP1
           from WINOTIP1
           where N1co# = :dsInfo.co#;
         Exec sql
          select APSBMUSER, APSBMEMAIL, APSBMPHN#
           into :strSubmitterName, :strSubmitterEmail, :decSubmitterPhone
           from WIAPP
           join WIEMP on APCO# = EMCO# and APAPP# = EMAPP#
           where EMCO# = :dsInfo.co# and EMFND = :dsInfo.fnd
            and EMEMP# = :dsInfo.emp# and EMFYR = :dsInfo.fyr;
         If decSubmitterPhone <> *zero;
            strSubmitterPhone = %editc(decSubmitterPhone:'Z');
            Select;
            When %len(strSubmitterPhone) > 7;
               strSubmitterPhone = '(' + %subst(strSubmitterPhone:1:3) + ')' +
                %subst(strSubmitterPhone:4:3) + '-' +
                %subst(strSubmitterPhone:7:4);
            Other;
               strSubmitterPhone = %subst(strSubmitterPhone:1:3) + '-' +
                %subst(strSubmitterPhone:4:4);
            Endsl;
         Endif;
         // Convert report date to CYMD
         Monitor;
            intPostCymd = %Dec(%Char(%Date(decReportDate:*Usa):*Cymd0):7:0);
         On-Error;
            intPostCymd = 0;
         EndMon;
         // Pull A/R numbers from WISRPREM workfile
         Reset dsWISRPREM;
         Exec sql
          select * into :dsWISRPREM
           from WISRPREM
           where REco# = :dsInfo.co# and REfnd = :dsInfo.fnd
            and REemp# = :dsInfo.emp# and REdiv = :dsInfo.div
            and REfyr = :dsInfo.fyr;
         If IsValidFetch(sqlstt);
            // Calc total payment
            decPmtTotal = REprem - REpprm + REtax - REptax + REprev;
            // Calc prev bal
            decBalancePrv = REprev;
            // Calc current billed
            decCurrentBilled = REprem - REpprm;
         EndIf;
       Endsr;
       //===================================================
       Begsr LoadLogo;
       //*** Beg Add *** 04/26/23 ******************************************
       Select;
       // Perform logo extraction for 'Documents' or 'Output Control' types.
       When strProcess = 'Documents' or strProcess = 'Output Control';
       //*** End Add *** 04/26/23 ******************************************
         Reset dsSTLGP;
         Exec sql
          select a.* into :dsSTLGP
           from STLGP a
           join STLGPGRP b
            on a.LGco#=b.LGco# and a.LGseq#=b.LGseq#
           where b.LGco# = :dsInfo.co# and b.LGfnd = :dsInfo.fnd
            and a.LGprocess = :strProcess and a.LGstat='A';
         Select;
         When LGpath <> *blanks;
            strLogo = '<img src="' + LGpath + '"/>';
         When isLogoTest;
            strLogo = '<img src="' +
             'http://10.91.212.10:43099/profoundui/userdata/images/' +
             'cancelled.png' + '"/>';
         Other;
            Reset strLogo;
         Endsl;
       //*** Beg Add *** 04/26/23 ******************************************
       // Perform logo extraction for 'Connect Notification' type.
       When strProcess = 'Connect Notification';
          Clear strLogo;
          Chain (dsInfo.co#:NOctgy:dsInfo.fnd) pem00;
          If %found(WIDFTEMLL);
             strLogo = '<img src="' + EMlogo + '"/>';
          Else;
             Chain (dsInfo.co#:NOctgy:0) pem00;
             If %found(WIDFTEMLL);
                strLogo = '<img src="' + EMlogo + '"/>';
             Endif;
          Endif;
       Endsl;
       //*** End Add *** 04/26/23 ******************************************
       Endsr;

       //*** Beg Add *** 03/23/2023 **** 107726 ***************************
       //===================================================
       // Load Staff Name, Email and Phone#
       //===================================================
       Begsr LoadStaffInfo;
         clear strStaffEmail;
         clear strStaffName;
         clear decStaffArea;
         clear decStaffPhone;
         clear strStaffPhone;
         If inStaffCode <> ' ';
            Exec sql
            select UPUSNM, UPMAIL, UPPHNA, UPPHN#
            into :strStaffName, :strStaffEmail, :decStaffArea,
             :decStaffPhone
            from WMUPP
            where UPSTAF = :inStaffCode;

            // Format Phone Number
            If decStaffArea <> *zero and decStaffPhone <> *zero;
               strStaffPhone = %editc(decStaffArea:'X') +
               %editc(decStaffPhone:'X');
               strStaffPhone = '(' + %subst(strStaffPhone:1:3) + ')' +
               %subst(strStaffPhone:4:3) + '-' +
               %subst(strStaffPhone:7:4);
            Else;
               clear strStaffPhone;
            Endif;
         Endif;
       Endsr;
       //*** End Add *** 03/23/2023 **** 107726 ***************************

       Begsr LoadOtherInfo;
         // The run type should not be the same across Documents and Output;
         //  therefore we will process both routines as only one will produce results
         Select;
         When GetInfoDocument(inRunInfo);
            strProcess = 'Documents';
         When GetInfoOutput(inRunInfo);
            strProcess = 'Output Control';
         //*** Beg Add *** 04/26/23 ***************************************
         When GetInfoConnect(inRunInfo);
            strProcess = 'Connect Notification';
         //*** End Add *** 04/26/23 ***************************************
         Endsl;
         Reset dsWMFNP;
         Exec sql
          select * into :dsWMFNP
           from WMFNP
           where FNco# = :dsInfo.co# and FNfnd = :dsInfo.fnd;
         Exec Sql
          select BSfreq
           into :chrFrequency
           from WTBSP
           where BSco# = :dsInfo.co#
            and BSkey = :dsInfo.bsCd;
         Reset dsWMRPP;
         Exec Sql
          select * into :dsWMRPP
           from WMRPP
           where RPco# = :dsInfo.co# and RPfnd = :dsInfo.fnd
            and RPemp# = :dsInfo.emp# and RPdiv = :dsInfo.div
            and RPfyr = :dsInfo.fyr;
       //***  Begin Add  ***  06/03/22  ******************
         Reset dsWMAGP;
         Exec Sql
          select * into :dsWMAGP
           from WMAGP
           where AGco# = :dsInfo.co#
            and AGfein = :dsInfo.feinAgt and AGmod = :dsInfo.mod;
       //***  End   Add  ***  06/03/22  ******************
         strPolicy# = RPcap#;
         If strPolicy# = *Blanks;
            strPolicy# = %EditC(dsInfo.fnd:'X') + %EditC(dsInfo.emp#:'X') +
             %EditC(dsInfo.fyr:'X');
         Endif;

         xxCo# = dsInfo.co#;
         xxFnd = dsInfo.fnd;
         xxEmp# = dsInfo.emp#;
         xxDiv = dsInfo.div;
         xxFyr = dsInfo.fyr;

         Monitor;
            AgtInfoAPI(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr
             :decAgt#:strAgtName:decProd#:strProdName);
         On-error;
         Endmon;
       //***  Begin Add  ***  06/03/22  ******************
         Select;
         When strAgtName = *blanks;
            strAgtName = AGname;
         Endsl;
       //***  End   Add  ***  06/03/22  ******************
         If FNmidy <> 0 and decReportDate <> 0 And chrFrequency <> 'W';
            workDueDate = %Date(decReportDate:*Usa) + %Days(FNmidy);
            workPolFrDt = %Date(dsInfo.pfdt:*Cymd);
            workEndDate = %Date(decReportDate:*Usa);
         //*** Begin Del *** 02/09/23 **************************************
         //**********Endif;
         //*** End   Del *** 02/09/23 **************************************
         Select;
         When chrFrequency = 'W';
            workBegDate = workEndDate - %Days(6);
            If workBegDate < workPolFrDt;
                workBegDate = workPolFrDt;
            EndIf;
         Other;
            intDayOfMth = %Subdt(workEndDate:*Days);
            intDayOfMth -= 1;
            workBegDate = workEndDate - %Days(intDayOfMth);
            If workBegDate < workPolFrDt;
               workBegDate = workPolFrDt;
            EndIf;
         EndSl;
         //*** Begin Add *** 02/09/23 **************************************
         Endif;
         //*** End   Add *** 02/09/23 **************************************
         If workBegDate <> nullDate;
            strReportPeriod = %Char(workBegDate) + ' - ' + %Char(workEndDate);
         Endif;
       Endsr;
     pReplaceTokensEmail...
     p                 e

      *=====================================================================
      * Create the sql statement based on the RRN type
      *=====================================================================
     pSetSqlStmt...
     p                 b
     dSetSqlStmt...
     d                 pi              a   len(32000) varying
     d inRrn                          9  0 const
      //*** Beg Chg *** 01/02/23 *** WC2875 ********************************
     d***inRrnType                     25    const varying  options(*nopass)
     d inRrnType                    100    const varying  options(*nopass)
      //*** End Chg *** 01/02/23 *** WC2875 ********************************

     dstrSql           s               a   len(32000) varying inz

       Select;
       When Upper(inRrnType) = 'AGENCY';
          // Get info from WMAGP
          strSql =
          //*** Beg Chg ***** 03/22/24 **********************************
          //*** 'select AGco#, 0, 0, 0, 0, 0, 0, 0, ' +
          //*** Beg Chg ***** 07/23/24 **********************************
          //***  'select AGco#, 0, 0, 0, 0, 0, 0, 0, 0 ' +
          //*** begin chg ***  08/30/24  ********************************
          //***** 'select AGco#, 0, 0, 0, 0, 0, 0, 0, 0, ' +
           'select AGco#, 0, 0, 0, 0, 0, 0, '' '', '' '', '' '', 0, 0, 0,' +
          //*** end chg *****  08/30/24  ********************************
          //*** End Chg ***** 07/23/24 **********************************
          //*** End Chg ***** 03/22/24 **********************************
           ' 0, '' '', ' +
           ' AGfein, AGmod, 0, ' +
          //*** Beg Add *** 01/02/23 *** WC2875 *************************
           ' '' '', '' '', '' '', '' '', '' '', '' '', '' '' ' +
          //*** End Add *** 01/02/23 *** WC2875 *************************
           ' from WMAGP' +
           ' where rrn(WMAGP)=' + %char(inRrn);
       When Upper(inRrnType) = 'AGENT';
          // Get info from WMAAP
          strSql =
          //*** Beg Chg ***** 03/22/24 **********************************
          //*** 'select AAco#, AAfnd, 0, 0, 0, 0, 0, 0, ' +
          //*** Beg Chg ***** 07/23/24 **********************************
          //***  'select AAco#, AAfnd, 0, 0, 0, 0, 0, 0, 0 ' +
          //*** begin chg ***  08/30/24  ********************************
          //***** 'select AAco#, AAfnd, 0, 0, 0, 0, 0, 0, 0, ' +
           'select AAco#, AAfnd, 0, 0, 0, 0, 0, '' '', '' '', '' '', 0,' +
          //*** begin chg ***  08/30/24  ********************************
          //*** End Chg ***** 07/23/24 **********************************
          //*** End Chg ***** 03/22/24 **********************************
           ' 0, 0, 0, '' '', ' +
           ' AAfein, AAmod, 0,' +
          //*** Beg Add *** 01/02/23 *** WC2875 *************************
           ' '' '', '' '', '' '', '' '', '' '', '' '', '' '' ' +
          //*** End Add *** 01/02/23 *** WC2875 *************************
           ' from WMAAP' +
           ' where rrn(WMAAP)=' + %char(inRrn);
       When Upper(inRrnType) = 'CLAIM';
          // Get info from WMCMP
          strSql =
          //*** Beg Chg ***** 03/22/24 **********************************
          //*** 'select CMco#, CMfnd, CMfyr, CMemp#, CMdiv,' +
          //*** Beg Chg ***** 07/23/24 **********************************
          //***  'select CMco#, CMfnd, CMfyr, CMemp#, CMdiv, CMcase' +
          //*** Beg Chg *** 08/26/24 **********************************
           'select CMco#, CMfnd, CMfyr, CMemp#, CMdiv, CMcase,' +
          //*** Beg Chg ***** 11/19/24 **********************************
          //***  ' CMacdt, CMfsn || CMlsn, CMltfl, CMclex' +
           ' CMacdt, CMfsn || CMlsn, CMltfl, CMclex,' +
          //*** Beg Chg ***** 11/19/24 **********************************
          //*** End Chg *** 08/26/24 **********************************
          //*** End Chg ***** 07/23/24 **********************************
          //*** End Chg ***** 03/22/24 **********************************
           ' p.ELbsCd, p.ELpfDt, p.ELptDt,' +
           ' p.EMfein, p.EMname || p.EMnam2,' +
           ' AAfein, AAmod, AHprod,' +
          //*** Beg Chg *** 04/24/23 ************************************
          //*** ' p.EMamgr, p.EMarvw, p.EMclmc, p.EMlrep,' +
          //*** ' p.EMpsrv, p.EMpacc, p.EMuwtr' +
           ' p.EMpsrv, p.EMuwtr, p.EMpacc, p.EMarvw,' +
           ' p.EMamgr, p.EMlrep, p.EMclmc' +
          //*** End Chg *** 01/02/23 *** WC2875 *************************
           ' from WMCMP' +
           ' left join policy_info p on' +
           '  CMco#=p.ELco# and CMfnd=p.ELfnd and CMemp#=p.ELemp#' +
           '   and CMdiv=p.ELdiv and CMfyr=p.ELfyr' +
           ' where rrn(WMCMP)=' + %char(inRrn);
          strSql = 'with ' + zSqlWithPolicyInfo + ' ' + strSql;
       When Upper(inRrnType) = 'POLICY';
          // Get info from WDELP
          strSql =
          //*** Beg Chg ***** 03/22/24 **********************************
          //*** 'select r.ELco#, r.ELfnd, r.ELfyr, r.ELemp#, r.ELdiv,' +
          //*** Beg Chg ***** 07/23/24 **********************************
          //***  'select r.ELco#, r.ELfnd, r.ELfyr, r.ELemp#, r.ELdiv, 0' +
           'select r.ELco#, r.ELfnd, r.ELfyr, r.ELemp#, r.ELdiv, 0,' +
          //*** begin add ***  08/30/24  ********************************
           ' 0, '' '', '' '', '' '',' +
          //*** end add *****  08/30/24  ********************************
          //*** End Chg ***** 07/23/24 **********************************
          //*** End Chg ***** 03/22/24 **********************************
           ' r.ELbsCd, r.ELpfDt, r.ELptDt,' +
           ' p.EMfein, p.EMname || p.EMnam2,' +
           ' AAfein, AAmod, AHprod,' +
          //*** Beg Chg *** 04/24/23 *********************************
          //*** ' p.EMamgr, p.EMarvw, p.EMclmc, p.EMlrep,' +
          //*** ' p.EMpsrv, p.EMpacc, p.EMuwtr' +
           ' p.EMpsrv, p.EMuwtr, p.EMpacc, p.EMarvw,' +
           ' p.EMamgr, p.EMlrep, p.EMclmc' +
          //*** End Chg *** 04/24/23 *********************************
           ' from WDELP r' +
           ' left join policy_info p on' +
           '  r.ELco#=p.ELco# and r.ELfnd=p.ELfnd and r.ELemp#=p.ELemp#' +
           '   and r.ELdiv=p.ELdiv and r.ELfyr=p.ELfyr' +
           ' where rrn(r)=' + %char(inRrn);
          strSql = 'with ' + zSqlWithPolicyInfo + ' ' + strSql;
       Endsl;
       Return strSql;
     pSetSqlStmt...
     p                 e

       //===================================================================
       // This was copied from WPIEMPLR... be sure it matches if issues arise.
       // Create 'with' conditions to build policy information including data from:
       //  WDELP, WMEMP, WMAHP, WMAAP
       //             - Returns a partial SQL string to be included in 'with'
       // as of Date  - agent history as of date (current date if not passed)
       //==================================================================
     pzSqlWithPolicyInfo...
     p                 b
     dzSqlWithPolicyInfo...
     d                 pi          5000    varying
     d inAsOfDate                      d   options(*nopass)
     d
     ddtAsOf           s               d   inz(*sys)
     ddtAsOf7          s              7  0
     ddtNull           s               d   inz
     dstrWith          s           5000    varying
      /Free
       If %parms >= 1 and inAsOfDate <> dtNull;
          dtAsOf = inAsOfDate;
       Endif;
       dtAsOf7 = DateTo7(dtAsOf);
       strWith = ' ' +
        'max_agt as ' +
        '(select AHco#, AHfnd, AHemp#, AHdiv, AHfyr, max(AHefDt) max_date ' +
        '  from WMAHP ' +
        '  where AHstat = ''A'' and AHefDt <= ' + %editc(dtAsOf7:'X') +
        '  group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr), ' +
        'agent_history as ' +
        '(select a.* ' +
        '  from WMAHP a ' +
        '  join max_agt m ' +
        '   on a.AHco# = m.AHco# and a.AHfnd = m.AHfnd ' +
        '    and a.AHemp# = m.AHemp# and a.AHdiv = m.AHdiv ' +
        '    and a.AHfyr = m.AHfyr and a.AHefDt = m.max_date ' +
        '  where AHstat = ''A''), ' +
        'agent_info as ' +
        '(select * '+
        '  from agent_history ' +
        '  left join WMAAP ' +
        '   on AAco# = AHco# and AAfnd = AHfnd and AAagt# = AHagt# ' +
        '  left join WMAGP ' +
        '   on AGco# = AAco# and AGfein = AAfein and AGmod = AAmod), ' +
        'policy_number as ' +
        '(select ELco# as RPco#, ELfnd as RPfnd, ELemp# as RPemp#, ' +
        '  ELdiv as RPdiv, ELfyr as RPfyr, RPCAP#, ' +
        '  case when (RPcap# is null or RPcap# = '''') ' +
        '   then digits(ELfnd)||''-''||digits(ELemp#)||''-''||digits(ELfyr) ' +
        '  else RPcap# end as policy# ' +
        '  from WDELP ' +
        '  left join WMRPP ' +
        '   on ELco# = RPco# and ELfnd = RPfnd ' +
        '    and ELemp# = RPemp# and ELdiv = RPdiv ' +
        '    and ELfyr = RPfyr), ' +
        'policy_info as ' +
        '(select * ' +
        '  from WDELP ' +
        '  left join WMEMP ' +
        '   on ELco# = EMco# and ELfnd = EMfnd ' +
        '    and ELemp# = EMemp# and ELdiv = EMdiv ' +
        '   left join agent_info ' +
        '    on ELco# = AHco# and ELfnd = AHfnd ' +
        '     and ELemp# = AHemp# and ELdiv = AHdiv ' +
        '     and ELfyr = AHfyr ' +
        '  left join WMRSP ' +
        '   on ELco# = RSco# and ELrtCd = RSrtCd ' +
        '  left join policy_number ' +
        '   on ELco# = RPco# and ELfnd = RPfnd ' +
        '    and ELemp# = RPemp# and ELdiv = RPdiv ' +
        '    and ELfyr = RPfyr)';
       Return strWith;
      /End-free
     pzSqlWithPolicyInfo...
     p                 e

