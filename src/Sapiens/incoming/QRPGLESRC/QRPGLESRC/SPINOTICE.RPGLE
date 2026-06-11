     H* (c) 2014 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H/TITLE SPINOTICE - API FOR SENDING NOTIFICATIONS/EMAILS
     H nomain
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**    THIS PROGRAM IS USED TO WRITE/UPDATE RECORDS IN THE       *
     H**    NOTIFICATION MASTER/DETAIL FILES FOR GENERATING EMAIL     *
     H**    MESSAGES WITH ATTACHED DOCUMENTS.                         *
     H**                                                              *
     H**    There are two main procedures:                            *
     H**                                                              *
     H** GenerateNotice: WRITE/UPDATE RECORDS IN THE NOTIFICATIONS    *
     H**                 MASTER/DETAIL FILES FOR THE REQUESTED TYPE   *
     H**                 AND THE INDICATED KEY.  THE TYPE REPRESENTS  *
     H**                 A REQUESTED EMAIL - SUCH AS "POLICY DOCS".   *
     H**                 THE KEY INDICATES WHICH DOCUMENT SHOULD BE   *
     H**                 STORED IN THE DETAIL FILE.                   *
     H**                                                              *
     H** SendNotice:     GENERATE XML DATA TO THE DATA QUEUE FOR      *
     H*                  SENDING AN EMAIL WITH REQUESTED DOCUMENTS.   *
     H*                  THE TYPE PASSED IN WILL BE USED TO PULL      *
     H*                  ALL OF THE DOCUMENTS ASSOCIATED WITH IT      *
     H*                  IN THE DETAIL RECORDS.                       *
     H*                                                               *
     H** PolicyLevCheck: CHECKS IF THE NOTIFICATION TYPE IS POLICY    *
     H*                  WIDE OR RESTRICTED TO DIVISION.              *
     H*                                                               *
     H** EmailInfo:      Returns pieces of information used by emails.*
     H*                                                               *
     H** EmailUpdate:    Displays outlook style popup window to allow *
     H*                  editing of email information: from/to email, *
     H*                  cc emails, subject, and body.                *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    04/10/14            PROJECT: WC2277               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/04/14 C:65  Project:  WC2277                               *
     H*                Add new procedure to display an outlook window *
     H*                for editing email information.  Create new     *
     H*                procedure to generate info for email pieces    *
     H*                so both new procedure for outlook window and   *
     H*                the SendNotice procedure can both use it.      *
     H*                SendNotice updated to only create new subject  *
     H*                and body info if the notification has not been *
     H*                manually updated with the info.                *
     H*                                                               *
     H*****************************************************************
     fspinotice cf   e             workstn usropn
     f                                     handler('PROFOUNDUI(HANDLER)')
     FWDELP     IF   E             DISK    rename(pel01:pel0p)
     FWMCMP     IF   E             DISK    rename(pcm01:pcm0p)
     FWMNEL     UF A E           K DISK    rename(pne01:pne00)
     FWDNDL     UF A E           K DISK    rename(pnd01:pnd00)
     FWDNDL1    IF   E           K DISK    rename(pnd01:pnd01)
     FWMEML     IF   E           K DISK    rename(pem01:pem00)
     FWMEXL     IF   E           K DISK    rename(pex01:pex00)
     FWMUPL1    IF   E           K DISK    rename(pup01:pup01)

     D psds          esds                  extname(szq1p)

     DCheckObjInLIBL   pr                  extPgm('SRCHKOBJ')
     D outObject                     10    const
     D outObjectType                  7    const
     D inError                        1

     DRtvObjectLib     pr                  extPgm('RTVOBJLIB')
     D outObject                     10    const
     D outObjectType                  7    const
     D inLib                         10

     DCreateDataQ      pr                  extPgm('SCCRTDTQ')
     D outDataQ                      10    const
     D outDataQLib                   10    const
     D outDataQLen                    5  0 const
     D outDataQText                  20    const
     D inReturn                       1

     D AgtInfo         pr                  extpgm('WA002R')
     D   ##CO#                        3  0 const
     D   ##FND                        3  0 const
     D   ##EMP#                       9  0 const
     D   ##DIV                        5  0 const
     D   ##FYR                        3  0 const
     D   ##AGT#                       5  0
     D   ##ANAM                      60
     D   ##PROD                       5  0
     D   ##PNAM                      60
     D   ##AREA                       3  0
     D   ##PHON                       7  0
     D   ##PLAN                       1
     D   ##DATE                       8  0
     D   ##NWRT                       5  2
     D   ##RWRT                       5  2
     D   ##RT#                        3  0
     D   ##PEML                      60

     d QSndDtaq        pr                  extpgm('QSNDDTAQ')
     d   XXQNM                       10    const
     d   XXLIB                       10    const
     d   XXFLDL                       5  0 const
     d   XXDATA                   64512    const

     D DataCenter      pr                  extpgm('SA030R')
     D   ##suff                       3

     D* Email Info procedure
     D EmailInfo       pr          2000
     D   ##rrn                        9  0 const
     D   ##mode                       1    const
     D   ##type                      20    const
     D   ##part                      20    const

     D/copy WprEmplr
     D/copy sPrDate
     D/copy sPrUsrPrms
     D/copy sPrNotice

     P*=====================================================================
     P* GenerateNotice - Write/Updates records to the notification master/detail files.
     P*          Parm 1 = Relative Record #         (9.0S)
     P*          Parm 2 = Mode (E = WDELP, C=WMCMP) ( 1 A)
     P*          Parm 3 = Type (type of process)    (20 A)
     P*          Parm 4 = Key  (type of document)   (20 A)
     P*          Parm 5 = Document ID               (12 A)
     P*          Parm 6 = Folder                    (80 A)
     P*          Parm 7 = Seq#                      (5.0S)
     P*          Parm 8 = UNCN                     (255 A)  {Optional}
     P*=====================================================================
     PGenerateNotice   B                   Export
     DGenerateNotice   PI
     D inRRN                          9  0 const
     D inMode                         1    const
     D inType                        20    const
     D inKey                         20    const
     D inDcid                        12    const
     D inFold                        80    const
     D inSeq#                         5  0 const
     D inUNCN                       255    options(*nopass) const
     D*
     D*
     D keyCo#          s                   like(ELCO#)
     D keyFnd          s                   like(ELFND)
     D keyEmp#         s                   like(ELEMP#)
     D keyDiv          s                   like(ELDIV)
     D keyFyr          s                   like(ELFYR)
     D keyCase         s                   like(CMCASE) inz(0)
     D xxUNCN          s            255    inz(' ')
     D strEmail        s             60    inz(' ')
     D policyLevel     s              1    inz(' ')
     D xxStDiv         s                   like(ELDIV)
     D*
     D #1AGT#          s              5  0 inz(0)
     D #1ANAM          s             60    inz(' ')
     D #1PROD          s              5  0 inz(0)
     D #1PNAM          s             60    inz(' ')
     D #1AREA          s              3  0 inz(0)
     D #1PHON          s              7  0 inz(0)
     D #1PLAN          s              1    inz('*')
     D #1DATE          s              8  0 inz(0)
     D #1NWRT          s              5  2 inz(0)
     D #1RWRT          s              5  2 inz(0)
     D #1RT#           s              3  0 inz(0)
     D*

     C     keyWMNEL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    xxStDiv
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C                   kfld                    inType
     C*
     C     keyWDNDL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C                   kfld                    inType
     C                   kfld                    inKey
     C*
     C     keyWMEXL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C*
     C     keyWMEML      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C*
      /Free

       If %parms >= 7;
          xxUNCN = inUNCN;
       Endif;

       //** Establish key data based on incoming RRN#.
       Select;
       When inMode = 'E';
          Chain inRRN pel0p;
          If not %found;
             Return;
          Endif;
          keyCO#  = ELCO#;
          keyFND  = ELFND;
          keyEMP# = ELEMP#;
          keyDIV  = ELDIV;
          keyFYR  = ELFYR;
       When inMode = 'C';
          Chain inRRN pcm0p;
          If not %found;
             Return;
          Endif;
          keyCO#  = CMCO#;
          keyFND  = CMFND;
          keyEMP# = CMEMP#;
          keyDIV  = CMDIV;
          keyFYR  = CMFYR;
          keyCASE = CMCASE;
       Other;
          Return;
       Endsl;

       //**  Verify group is setup to perform specified noticiation.
       Chain keyWMEXL pex00;

       Select;
       When inType = 'Policy Set' and EXNotP <> 'Y';
          Return;
       Endsl;

       //* Determine if the notification type is for entire policy.
       policyLevel = PolicyLevCheck(inType);

       xxStDiv = keyDiv;

       If policyLevel = 'Y';
          Setll keyWMEML pem00;
          Reade(n) keyWMEML pem00;
          Dow not %eof;
             If EMStrp = 'Y';
                xxStDiv = EMDiv;
                Leave;
             Endif;
             Reade(n) keyWMEML pem00;
          Enddo;
       Endif;

       //**  Write or Update the Notifications Master record for the type.
       Chain keyWMNEL pne00;

       If not %found(wmnel);
          Clear pne00;
          NECo#  = keyCo#;
          NEFnd  = keyFnd;
          NEEmp# = keyEmp#;
          NEDiv  = xxStDiv;
          NEFyr  = keyFyr;
          NECase = keyCase;
          NEType = inType;
          NEIDt  = Dateto7(SystemDate());
          NEITim = Timeto6(SystemTime());
          NEIPgm = Q1PGM;
          NEIUsr = Q1USER;
       Endif;

       NEStat = ' ';
       NEStDt = 0;
       NEMail = ' ';
       NEDt   = Dateto7(SystemDate());
       NETime = Timeto6(SystemTime());
       NEPgm  = Q1PGM;
       NEUser = Q1USER;

       //* Flag the master record as Pending to indicate notification is ready to send based
       //* on the last document of the notification being added/updated to the set.
       Select;
       When inType = 'Policy Set' and inKey = 'Policy Page';
          NEStat = 'P';
          NEStDt = Dateto7(SystemDate());
          //* Producer email address.
          AgtInfo(keyCo#:keyFnd:keyEmp#:keyDiv:keyFyr:#1Agt#:#1Anam:#1Prod:
           #1Pnam:#1Area:#1Phon:#1Plan:#1Date:#1Nwrt:#1Rwrt:#1Rt#:strEmail);
          NEMail = strEmail;
          //* User email address.
          NEUsrM = GetUserEmail(q1user);
       Endsl;

       //* Write/Update the master record.
       If not %found(wmnel);
          Write pne00;
       Else;
          Update pne00;
       Endif;

       //**  Write or Update the Notifications Detail record for the type/key.
       Chain keyWDNDL pnd00;

       If not %found(wdndl);
          Clear pnd00;
          NDCo#  = keyCo#;
          NDFnd  = keyFnd;
          NDEmp# = keyEmp#;
          NDDiv  = keyDiv;
          NDFyr  = keyFyr;
          NDCase = keyCase;
          NDType = inType;
          NDKey  = inKey;
          NDIDt  = Dateto7(SystemDate());
          NDITim = Timeto6(SystemTime());
          NDIPgm = Q1PGM;
          NDIUsr = Q1USER;
       Endif;

       NDFold = inFold;
       NDDcid = inDcid;
       NDUNCN = xxUNCN;
       NDSeq# = inSeq#;
       NDDt   = Dateto7(SystemDate());
       NDTime = Timeto6(SystemTime());
       NDPgm  = Q1PGM;
       NDUser = Q1USER;

       //**  Write or Update the detail record.
       If not %found(wdndl);
          Write pnd00;
       Else;
          Update pnd00;
       Endif;

       Return;

      /End-Free
     PGenerateNotice   E
      *
     P*=====================================================================
     P* SendNotice - Generates record to data queue for sending email notification.
     P*          Parm 1 = Relative Record #         (9.0S)
     P*          Parm 2 = Mode (E = WDELP, C=WMCMP) ( 1 A)
     P*          Parm 3 = Type (type of process)    (20 A)
     P*=====================================================================
     PSendNotice       B                   Export
     DSendNotice       PI             1
     D inRRN                          9  0 const
     D inMode                         1    const
     D inType                        20    const
     D*
     D keyCo#          s                   like(ELCO#)
     D keyFnd          s                   like(ELFND)
     D keyEmp#         s                   like(ELEMP#)
     D keyDiv          s                   like(ELDIV)
     D keyFyr          s                   like(ELFYR)
     D keyCase         s                   like(CMCASE) inz(0)
     D*
     D notAvail        s              1    inz(' ')
     D strLib          s             10    inz(' ')
     D strQueue        s             10    inz(' ')
     D inRC            s              1    inz(' ')
     D strData         s          64512    inz(' ')
     D strTimeStamp    s             19    inz(' ')
     D strTime         s              6    inz(' ')
     D strDate         s              8    inz(' ')
     D strPgm          s              6    inz(' ')
     D strEmail        s             60    inz(' ')
     D strUserEmail    s             60    inz(' ')
     D strSubject      s            200    inz(' ')
     D strBody         s           2000    inz(' ')
     D suffix          s              3    inz(' ')
     D policyLevel     s              1    inz(' ')
     D xxStDiv         s                   like(ELDIV)
     D*
     C     keyWMNEL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C                   kfld                    inType
     C*
     C     keyWDNDL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C                   kfld                    inType
     C*
     C     keyWDNDL1     klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C                   kfld                    inType
     C*
     C     keyWMEML      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C*
     C     keyWMEXL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C*
      /Free

       //** Set key data based on incoming RRN#.
       Select;
       When inMode = 'E';
          Chain inRRN pel0p;
          If not %found;
             Return 'E';
          Endif;
          keyCO#  = ELCO#;
          keyFND  = ELFND;
          keyEMP# = ELEMP#;
          keyDIV  = ELDIV;
          keyFYR  = ELFYR;
          strPgm  = 'W0050R';
       When inMode = 'C';
          Chain inRRN pcm0p;
          If not %found;
             Return 'E';
          Endif;
          keyCO#  = CMCO#;
          keyFND  = CMFND;
          keyEMP# = CMEMP#;
          keyDIV  = CMDIV;
          keyFYR  = CMFYR;
          keyCASE = CMCASE;
          strPgm  = 'W0070R';
       Other;
          Return 'E';
       Endsl;

       //**  Verify group is setup to perform specified noticiation.
       Chain keyWMEXL pex00;

       Select;
       When inType = 'Policy Set' and EXNotP <> 'Y';
          Return 'N';
       Endsl;

       //* Determine if the notification type is for entire policy.
       policyLevel = PolicyLevCheck(inType);

       xxStDiv = keyDiv;

       If policyLevel = 'Y';
          Setll keyWMEML pem00;
          Reade(n) keyWMEML pem00;
          Dow not %eof;
             If EMStrp = 'Y';
                xxStDiv = EMDiv;
                Leave;
             Endif;
             Reade(n) keyWMEML pem00;
          Enddo;
       Endif;

       //**  Chain to Notification Master record to send email.
       Chain keyWMNEL pne00;
       If %found;

          DataCenter(suffix);

          strLib = 'PDSTOOLS';
          strQueue = 'MAILSND' + suffix;

          CheckObjInLIBL(strQueue:'*DTAQ':notAvail);

          If notAvail = '1';
             CreateDataQ(strQueue:strLib:64512:'Email Data Queue':inRC);
          Endif;

          strEmail = NEMail;
          StrUserEmail = NEUsrm;

          If strEmail <> *blanks and strUserEmail <> *blanks;

             //*String fields for email data.
             strTime = %editc(Timeto6(SystemTime()) : 'X');
             strDate = %editc(Dateto8(SystemDate()) : 'X');
             strTimeStamp = %subst(strDate:5:4) + '/' + %subst(strDate:1:2) +
              '/' + %subst(strDate:3:2) + ' ' + %subst(strTime:1:2) + ':' +
              %subst(strTime:3:2) + ':' + %subst(strTime:5:2);

             If NESubj = ' ';
                strSubject = %trim(EmailInfo(inRRN:inMode:inType:'Subject'));
             Else;
                strSubject = %trim(NESubj);
             Endif;

             If NEBody = ' ';
                strBody = %trim(EmailInfo(inRRN:inMode:inType:'Body'));
             Else;
                strBody = %trim(NEBody);
             Endif;

             //*Email data.
             strData = '<?xml version="1.0" encoding="UTF-8"?> <notification '+
              'program="' +strPgm + '" timestamp="'+ strTimeStamp + '" releas'+
              'e="Y"> <email> <recipients> <to>' + %trim(strEmail) + '</to> <' +
              '/recipients> <from>' + %trim(strUserEmail) + '</from> <subject>'+
              %trim(strSubject) + '</subject> <body><![CDATA[' + %trim(strBody)+
              ']]></body> <attachments> ';

             If policyLevel = 'Y';
                Setll keyWDNDL1 pnd01;
                Reade(n) keyWDNDL1 pnd01;
             Else;
                Setll keyWDNDL pnd00;
                Reade(n) keyWDNDL pnd00;
             Endif;
             Dow not %eof;

                strData = %trim(strData) + ' <attachment description="' +
                 %trim(NDKey) + '">';

                If NDUncn <> *blanks;
                   strData = %trim(strData) + %trim(NDUncn);
                Else;
                   strData = %trim(strData) + '/QDLS/' + %trim(NDFold) + '/' +
                    %trim(NDDcid);
                Endif;
                strData = %trim(strData) + '</attachment>';

                If policyLevel = 'Y';
                   Reade(n) keyWDNDL1 pnd01;
                Else;
                   Reade(n) keyWDNDL pnd00;
                Endif;
             Enddo;

             strData = %trim(strData) + ' </attachments> </email> <job name="'+
              %trim(Q1Job) + '" number = "' + %editc(Q1Jobn : 'X') + '" user="'+
              %trim(Q1User) + '"> </job> </notification>';

             QSndDtaq(strQueue:strLib:64512:strData);

             NEStat = 'S';
             NEStDt = DateTo7(SystemDate());
             NEDt   = Dateto7(SystemDate());
             NETime = Timeto6(SystemTime());
             NEPgm  = Q1PGM;
             NEUser = Q1USER;
             Update pne00;

          Else;

             NEStat = 'E';
             NEStDt = DateTo7(SystemDate());
             NEDt   = Dateto7(SystemDate());
             NETime = Timeto6(SystemTime());
             NEPgm  = Q1PGM;
             NEUser = Q1USER;
             Update pne00;

          Endif;         //* From and To email addresses required.

       Endif;

       Return 'S';

      /End-Free
     PSendNotice       E

     P*=====================================================================
     P* PolicyLevCheck - Checks if the notification set is policy wide or division only.
     P*          Parm 1 = Type (type of process)    (20 A)
     P*=====================================================================
     PPolicyLevCheck   B                   Export
     DPolicyLevCheck   PI             1
     D inType                        20    const
     D policyLevel     s              1    inz(' ')
     C*
      /Free

       policyLevel = ' ';

       Select;
       When inType = 'Policy Set';
          policyLevel = 'Y';
       Endsl;

       Return policyLevel;

      /End-Free
     PPolicyLevCheck   E
      *
     P*=====================================================================
     P* EmailInfo - Builds and returns information for emails.
     P*          Parm 1 = Relative Record #         (9.0S)
     P*          Parm 2 = Mode (E = WDELP, C=WMCMP) ( 1 A)
     P*          Parm 3 = Type (type of process)    (20 A)
     P*          Parm 4 = Part (part of email)      (20 A)
     P*=====================================================================
     PEmailInfo        B
     DEmailInfo        PI          2000
     D inRRN                          9  0 const
     D inMode                         1    const
     D inType                        20    const
     D inPart                        20    const
     D
     D keyCo#          s                   like(ELCO#)
     D keyFnd          s                   like(ELFND)
     D keyEmp#         s                   like(ELEMP#)
     D keyDiv          s                   like(ELDIV)
     D keyFyr          s                   like(ELFYR)
     D keyCase         s                   like(CMCASE) inz(0)
     D
     D strInfo         s           2000    inz(' ')
     D strSubject      s            200    inz(' ')
     D strBody         s           2000    inz(' ')
     D strEmpName      s            100    inz(' ')
     D strPhone        s             25    inz(' ')
     D polNumber       s             25    inz(' ')
     D polPFDT         s              8  0 inz(0)
     D polPTDT         s              8  0 inz(0)
     D
     D policySubject   c                   const('Workers Compensation Policy a-
     D                                     nd Billing Information for ')
     D policyBody      c                   const('Please see attached all infor-
     D                                     mation relevant to above issued Work-
     D                                     ers Compensation Policy.')
     D
     C     keyWMEML      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C*
      /Free

       strInfo = ' ';

       //** Set key data based on incoming RRN#.
       Select;
       When inMode = 'E';
          Chain inRRN pel0p;
          keyCO#  = ELCO#;
          keyFND  = ELFND;
          keyEMP# = ELEMP#;
          keyDIV  = ELDIV;
          keyFYR  = ELFYR;
       When inMode = 'C';
          Chain inRRN pcm0p;
          keyCO#  = CMCO#;
          keyFND  = CMFND;
          keyEMP# = CMEMP#;
          keyDIV  = CMDIV;
          keyFYR  = CMFYR;
          keyCASE = CMCASE;
       Endsl;

       Chain keyWMEML pem00;

       Exsr EmplrName;

       Select;
       When inType = 'Policy Set';
          strSubject = policySubject + %trim(strEmpName);

          Clear polNumber;
          Clear polPFDT;
          Clear polPTDT;

          polNumber = GetEmpPol#(inRRN:' ');
          polPFDT = dtMDYY(ELPFDT);
          polPTDT = dtMDYY(ELPTDT);

          strBody = %trim(strEmpName) + ' <br/> ' +
           %trim(polNumber) + ' - ' + %editw(polPFDT:'  /  /    ') +
           ' to ' + %editw(polPTDT:'  /  /    ') + ' <br/> <br/> ' +
           %trim(policyBody) + ' <br/> <br/> ';

          Chain(n) EMUwtr pup01;
          If %found and EMUwtr <> ' ';
             Clear strPhone;
             If UPPhn# <> 0;
                strPhone = '(' + %editc(UPPHNA:'X') + ') ' +
                 %editw(UPPHN#:'   -    ');
                If UPExt# <> 0;
                   strPhone = %trim(strPhone) + ' Ext ' +
                    %editc(UPExt#:'Z');
                Endif;
             Endif;
             strBody = %trim(strBody) + %trim(UPUSNM) + ' <br/> ' +
              %trim(UPTITL) + ' <br/> ' + %trim(strPhone) + ' <br/> ' +
              %trim(UPMail) + ' <br/> ';
          Endif;

       Other;
          strSubject = 'Email for Claim/Policy.';
          strBody    = 'This is default text for the body of the email.';
       Endsl;


       Select;
       When inPart = 'Subject';
          strInfo = %trim(strSubject);
       When inPart = 'Body';
          strInfo = %trim(strBody);
       Endsl;

       Return strInfo;

      /End-Free
     C**************************************************************************
     C*
     C* Concatenate the Employer name together into 100A field.
     C*
     C**************************************************************************
     C     EmplrName     BEGSR
     C*
     C                   If        EMFDNM <> *blanks
     C                   Movel(p)  emfdnm        xxnam1           50
     C                   Movel(p)  emfdn2        xxnm10           10
     C                   Move      xxnm10        xxnam1
     C                   Move(p)   emfdn2        xxnam2           50
     C                   Else
     C                   Movel(p)  emname        xxnam1
     C                   Movel(p)  emnam2        xxnm10
     C                   Move      xxnm10        xxnam1
     C                   Move(p)   emnam2        xxnam2
     C                   Endif
     C*
     C                   Eval      strEmpName = xxnam1 + xxnam2
     C*
     C                   ENDSR                                                  EmplrName Subr
     C**************************************************************************
     PEmailInfo        E

     P*=====================================================================
     P* EmailUpdate - Displays outlook style popup window to allow editing of email info.
     P*          Parm 1 = Relative Record #         (9.0S)
     P*          Parm 2 = Mode (E = WDELP, C=WMCMP) ( 1 A)
     P*          Parm 3 = Type (type of process)    (20 A)
     P*=====================================================================
     PEmailUpdate      B                   Export
     DEmailUpdate      PI             1
     D inRRN                          9  0 const
     D inMode                         1    const
     D inType                        20    const
     D*
     D keyCo#          s                   like(ELCO#)
     D keyFnd          s                   like(ELFND)
     D keyEmp#         s                   like(ELEMP#)
     D keyDiv          s                   like(ELDIV)
     D keyFyr          s                   like(ELFYR)
     D keyCase         s                   like(CMCASE) inz(0)
     D error           s               n   inz(*off)
     D saveDt          s                   like(NEDt)
     D saveTime        s                   like(NETime)
     D*
     C     keyWMNEL      klist
     C                   kfld                    keyCo#
     C                   kfld                    keyFnd
     C                   kfld                    keyEmp#
     C                   kfld                    keyDiv
     C                   kfld                    keyFyr
     C                   kfld                    keyCase
     C                   kfld                    inType
     C*
      /Free

       //** Set key data based on incoming RRN#.
       Select;
       When inMode = 'E';
          Chain inRRN pel0p;
          If not %found;
             Return 'E';
          Endif;
          keyCO#  = ELCO#;
          keyFND  = ELFND;
          keyEMP# = ELEMP#;
          keyDIV  = ELDIV;
          keyFYR  = ELFYR;
       When inMode = 'C';
          Chain inRRN pcm0p;
          If not %found;
             Return 'E';
          Endif;
          keyCO#  = CMCO#;
          keyFND  = CMFND;
          keyEMP# = CMEMP#;
          keyDIV  = CMDIV;
          keyFYR  = CMFYR;
          keyCASE = CMCASE;
       Other;
          Return 'E';
       Endsl;

       Open spinotice;

       //**  Chain to Notification Master record to send email.
       Chain(n) keyWMNEL pne00;
       If %found;

          scFrmEmail = %trim(NEUsrM);
          scToEmail  = %trim(NEMail);
          scCCEmail  = %trim(NECCEM);
          scSubject  = %trim(NESubj);
          scBody     = %trim(NEBody);

          If scSubject = ' ';
             scSubject = %trim(EmailInfo(inRRN:inMode:inType:'Subject'));
          Endif;

          If scBody = ' ';
             scBody = %trim(EmailInfo(inRRN:inMode:inType:'Body'));
          Endif;

          saveDt   = NEDt;
          saveTime = NETime;

          erFrmEmail = *off;
          erToEmail  = *off;
          erCCEmail  = *off;
          erSubject  = *off;
          erBody     = *off;
          msFrmEmail = ' ';
          msToEmail  = ' ';
          msCCEmail  = ' ';
          msSubject  = ' ';
          msBody     = ' ';
          btnCancel = *off;
          btnSave = *off;
          error = *off;

          Dou btnCancel = *on or btnSave = *on;

             Exfmt rcdUpdMail;

             erFrmEmail = *off;
             erToEmail  = *off;
             erCCEmail  = *off;
             erSubject  = *off;
             erBody     = *off;
             msFrmEmail = ' ';
             msToEmail  = ' ';
             msCCEmail  = ' ';
             msSubject  = ' ';
             msBody     = ' ';
             error      = *off;

             If btnCancel = *off;

                Exsr EmailUpdateEdit;

                If error = *on;
                   btnSave = *off;
                Endif;

             Endif;

          Enddo;

          If btnSave = *on;

             Chain keyWMNEL pne00;
             NEUsrM = %trim(scFrmEmail);
             NEMail = %trim(scToEmail);
             NECCEM = %trim(scCCEmail);
             NESubj = %trim(scSubject);
             NEBody = %trim(scBody);
             NEDt   = Dateto7(SystemDate());
             NETime = Timeto6(SystemTime());
             NEPgm  = Q1PGM;
             NEUser = Q1USER;
             Update pne00;

          Endif;

       Endif;

       Close spinotice;

       Return ' ';

       //*********************************************************************
       //*
       //* Edit email changes.
       //*
       //*********************************************************************
       Begsr EmailUpdateEdit;

          If scFrmEmail = *blanks;
             erFrmEmail = *on;
             msFrmEmail = 'The From email address cannot be blank.';
             error = *on;
          Endif;

          If scToEmail = *blanks;
             erToEmail = *on;
             msToEmail = 'The To email address cannot be blank.';
             error = *on;
          Endif;

          If scSubject = *blanks;
             erSubject = *on;
             msSubject = 'Email subject cannot be blank.';
             error = *on;
          Endif;

          If scBody = *blanks;
             erBody = *on;
             msBody = 'Email body cannot be blank.';
             error = *on;
          Endif;

          If error = *off;
             Chain(n) keyWMNEL pne00;
             If saveDt <> NEDt or saveTime <> NETime;
                erFrmEmail = *on;
                msFrmEmail = 'This record has been updated by another user.';
                error = *on;
             Endif;
          Endif;

       Endsr;
       //*********************************************************************
      /End-Free
     PEmailUpdate      E
      *
