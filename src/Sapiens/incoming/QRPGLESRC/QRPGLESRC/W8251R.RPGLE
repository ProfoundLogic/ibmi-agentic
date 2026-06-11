     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H*=========================================================================
    H/TITLE   W8251R - Claim Import Processing
    H*=========================================================================
     H* Created on 3/14/2017 by c:62 for project 114WCT
    H*=========================================================================
     h* Revisions
     H*                                                               *
     H* 07/09/25 C:349 Project: PHW118941 (CSI)                       *
     H*                Default medical reserve amount to WMFNL table  *
     H*                value amount                                   *
     H*                                                               *
     H* 11/07/24 C:333 Project: 114WEX (CSI)                          *
     H*                Claims Import Process Changes                  *
     H*                                                               *
     H* 05/23/24 C:300 Support: 115634 (SUNZ)                         *
     H*                Update WMFNP Maintenance fields while updating *
     H*                Next Available Claim Number                    *
     H*
     H* 03/20/18 C:62  Ticket: 69817(114)                             *
     H*                Enhance the enhanced zip code edit.            *
     H*
     H* 01/02/18 C:62  Ticket: 63369(114)                             *
     H*                Correct losttime and status defaults.          *
     H*
     H* 10/03/17 C:62  Ticket: 49987                                  *
     H*                Enhance zip code edit                          *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     h*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
     F* Import File
     F*=========================================================================
    F*
    F* WC Claim Setup File
     Fwxcip     if   e             Disk    Rename(pci01:pci0p) InfDS(dbfds)

     F*=========================================================================
     F* Additional Master Files
     F*=========================================================================
    F*
    F* Employer Master File by CO#/FND/EMP#/DIV
     Fwmeml     if   e           k Disk    Rename(pem01:pem00)
    F*
    F* Group Master File by CO#/FND
     Fwmfnl     uf   e           k Disk    Rename(pfn01:pfn00)
    F*
    F* Group Detail File by CO#/FND/Start Date
     Fwdf1l1    if   e           k Disk

     F*=========================================================================
     F* Claim Files
     F*=========================================================================
    F*
    F* Claimant Demographics by SSN#
     Fwmcdl     uf a e           k Disk    Rename(pcd01:pcd00)
    F*
    F* Claim file
     Fwmcml1    uf a e           k Disk
     Fwmcml17   if   e           k Disk    Rename(pcm01:pcm017)
    F*
    F* Reserve history file
     Fwmrhp     o    e             Disk    Rename(prh01:prh0p)
    F*
    F* Claim Status History File
     Fwmchp     o    e             Disk    Rename(pch01:pch0p)
    F*
    F*  Claim Legal Master File
     Fwmlll     uf a e           k Disk    Rename(pll01:pll00)
    F*
    F*  Average Weekly Wage History File
     Fwmawp     o    e             Disk    Rename(paw01:paw0p)

     F*=========================================================================
     F* Table Files
     F*=========================================================================
    F*
    F* User Parameters by Staff Code
     Fsmupl1    if   e           k Disk
    F*
    F* State File
     Fstabl     if   e           k Disk    Rename(pst01:pst00)
    F*
    F* Generic Table ID
     Fwtgil     if   e           k Disk    Rename(pgi01:pgi00)
    F*
    F* Generic Table
     Fwtgtl     if   e           k Disk    Rename(pgt01:pgt00)
    F*
    F* Cause Code Table
     Fwtcaledi  if   e           k Disk    Rename(pca01:pcaedi)
    F*
    F* Nature Code Table
     Fwtnaledi  if   e           k Disk    Rename(pna01:pnaedi)
    F*
    F* Part Code Table
     Fwtpaledi  if   e           k Disk    Rename(ppa01:ppaedi)

     F*=========================================================================
     F* Printer Files
     F*=========================================================================
    F*
    F* Updated/Added Claims
     Fqsysprt   o    f  132        Printer OflInd(*inOA)
    F*
    F* Claims That weren't Loaded Because they Already Exist
     Fqsysprt1  o    f  132        Printer OflInd(*inOB)
    F*
    F* Claim Error Report
     Fqsysprt2  o    f  132        Printer OflInd(*inOC)
    F*
    F* Rejected Claim Report
     Fqsysprt3  o    f  132        Printer OflInd(*inOD)

    D*=========================================================================
    D* External Prototypes
    D*=========================================================================
     d* Create WCMC Case Demographic
     d CrtWcmcClmDemo  pr                  ExtPgm('FA002RC')
     d   qmRtCd                       1
     d   qmSS#                        9    Const
     d   qmAcDt                       7    Const
     d   qmCo#                        3    Const
     d   qmSub#                       5    Const
     d   qmFnd                        3    Const
     d   qmFyr                        3    Const
     d   qmCase                       7    Const
     d   qmLsN                       30    Const
     d   qmFsN                       15    Const
     d   qmMid                        1    Const
     d   qmAdr1                      40    Const
     d   qmAdr2                      40    Const
     d   qmCty                       25    Const
     d   qmSt                         2    Const
     d   qmZip                        9    Const
     d   qmDtoB                       7    Const
     d   qmSex                        1    Const
     d   qmArea                       3    Const
     d   qmPhn#                       7    Const
     d   qmMsts                       1    Const
     d   qmWara                       3    Const
     d   qmWph#                       7    Const
     d   qmCtry                       3    Const
     d   qmAdr3                      40    Const

     d* Create WCMC Case
     d CrtWcmcClmMast  pr                  ExtPgm('FA003RC')
     d   qmRtCd                       1
     d   qmSS#                        9    Const
     d   qmAcDt                       7    Const
     d   qmCo#                        3    Const
     d   qmSub#                       5    Const
     d   qmFnd                        3    Const
     d   qmFyr                        3    Const
     d   qmCase                       7    Const
     d   qmEmp#                       9    Const
     d   qmDiv                        5    Const
     d   qmAcTm                       6    Const
     d   qmDtRp                       7    Const
     d   qmDtLw                       7    Const
     d   qmAcDs                      60    Const
     d   qmMcon                       1    Const
     d   qmEntD                       7    Const
     d   qmRcvD                       7    Const
     d   qmMMI                        7    Const
     d   qm1a1                        1    Const
     d   qm10a1                      10    Const
     d   qmLtFl                       1    Const
     d   qmScl#                      25    Const
     d   qmMcTm                       4    Const
     d   qmStWk                       2    Const
     d   qmDeDt                       7    Const
     d   qmDeem                       2    Const
     d   qmCmNu                       3    Const
     d   qmCmSu                       3    Const
     d   qmClad                       3    Const
     d   qmRrn                        9  0
     d   qmLsN                       30    Const
     d   qmFsN                       15    Const
     d   qmMid                        1    Const
     d   qmDtRe                       7    Const
     d   qmClSu                       3    Const
     d   qm#Dep                       3    Const
     d   qmOcDs                      40    Const
     d   qmHiDt                       7  0 Const
     d   qmEsts                       2    Const
     d   qmWkPt                       2    Const
     d   qmPrex                       1    Const
     d   qmAcD2                      60    Const
     d   qmCvFl                       1    Const
     d   qmMca                        1    Const
     d   qmMmiP                       5  2 Const
     d   qmOdis                       1    Const
     d   qmYrlx                       4  0 Const
     d   qmDbDt                       7  0 Const
     d   qmHrDy                       3  0 Const
     d   qmDyWk                       1  0 Const
     d   qmClFa                       3    Const
     d   qmDny                        1    Const
     d   qmImp                        1    Const
     d   qmJrSt                       2    Const
      *** Beg Chg ***** 11/07/24 ***********************************
     d***qmPoSt                       2    Const
     d   qmStat                       1    Const
     d   qmSrc                       20    Const
     d   qmPoSt                       2    Const
      *** End Chg ***** 11/07/24 ***********************************

     d* Create WCMC Case History
     d CrtWcmcClmHist  pr                  ExtPgm('FA004RC')
     d   qmCo#Int                     3  0 Const
     d   qmFndInt                     3  0 Const
     d   qmFyrInt                     3  0 Const
     d   qmCaseInt                    7  0 Const
     d   qmEfDt                       7  0 Const
     d   qmOCSt                       1    Const
     d   qmExpl                      60    Const
     d   qmClCd                       3    Const
     d   qmRtCd                       1
     d   qmSub#Int                    5  0 options(*nopass)

     d* Retrieve Company Information
     d RtvCompInfo     pr                  ExtPgm('WA001R')
     d   qmCo#Int                     3  0
     d   qmFndInt                     3  0
     d   qmCoName                    40
     d   qmGrpName                   40

    D*=========================================================================
    D* Internal Prototypes
    D*=========================================================================
     d* Add Leading Zeros to Date
     d CvtDates        pr             7  0
     d  dateIn                        8    Const

     d* Edit Claim Import File - Errors
     d EdtClmImportE   pr

     d* Edit Claim Import File - Rejected
     d EdtClmImportR   pr              n

     d* Get claim #
     d GetClaim#       pr             7  0
     d   group#                       3  0

     d* Get Year
     d GetYear         pr             3  0
     d   group#                       3  0
     d   accidentDate                 7  0

     d* Process Claim File
     d ProcClaims      pr

     d* Program End
     d ProgramEnd      pr

     d* Print Claim Totals
     d PrtClmTotals    pr

     d* Print Claim Has Been Converted
     d PrtClmConvert   pr

     d* Print Claim Errors
     d PrtClmErrors    pr
     d  type                          1    Const
     d  error                         2  0 Const
     d  importData                   40

     d* Print Claim Already Exists
     d PrtClmExists    pr

     d* Write Claim Master
     d WriteClaim      pr

     d* Write Claim Average Weekly Wage
     d WrtClmAWW       pr

     d* Write Claim Demographic
     d WrtClmDemo      pr

     d* Write Claim History
     d WrtClmHistory   pr

     d* Write Claim Legal
     d WrtClmLegal     pr

     d* Write Reserve History
     d WrtClmRsvs      pr

     d* Write WCMC Claim Demographic
     d WrtWcmcClmDemo  pr
     d  returnCode                    1

     d* Write WCMC Claim History
     d WrtWcmcClmHist  pr
     d  returnCode                    1

     d* Write WCMC Claim Master
     d WrtWcmcClmMast  pr
     d  returnCode                    1
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************

     d*** Beg Add *** 07/09/25 ******************************************
      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d    cltId                       3
     d*** End Add *** 07/09/25 ******************************************

    D*=========================================================================
    D* Arrays and Data Structures
    D*=========================================================================
     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
     D dbfds           ds
     D  dbrrn                397    400B 0
    D*
    D*     Run Date and Time.
    D*
     D                 DS                  INZ
     D dsDttm                        20  0
     D dsDate                         8  0 Overlay(dsDttm)
     D dsTime                         6  0 Overlay(dsDttm:*Next)

     d* Error Messages/Error Counts
     d err             s             35    Dim(30) CtData PerRcd(1)             Error Messages
     d erc             s              7  0 Dim(30) Inz                          Errors by type
     d rej             s             35    Dim(05) CtData PerRcd(1)             Reject Messages
     d rjc             s              7  0 Dim(05) Inz                          Rejects by type

    D*=========================================================================
    D*  Variables
    D*=========================================================================
       // FA002RC And FA003RC Shared parameters
     d   qmRtCd        s              1
     d   qmSS#         s              9
     d   qmAcDt        s              7
     d   qmCo#         s              3
     d   qmSub#        s              5
     d   qmFnd         s              3
     d   qmFyr         s              3
     d   qmCase        s              7
     d   qmLsN         s             30
     d   qmFsn         s             15
     d   qmMid         s              1

       // FA002RC parameters
     d   qmAdr1        s             40
     d   qmAdr2        s             40
     d   qmCty         s             25
     d   qmSt          s              2
     d   qmZip         s              9
     d   qmDtoB        s              7
     d   qmSex         s              1
     d   qmArea        s              3
     d   qmPhn#        s              7
     d   qmMsts        s              1
     d   qmWara        s              3
     d   qmWph#        s              7
     d   qmCtry        s              3
     d   qmAdr3        s             40

       // FA003RC parameters
     d   qmEmp#        s              9
     d   qmDiv         s              5
     d   qmAcTm        s              6
     d   qmDtRp        s              7
     d   qmDtLw        s              7
     d   qmAcDs        s             60
     d   qmMcon        s              1
     d   qmEntD        s              7
     d   qmRcvD        s              7
     d   qmMMI         s              7
     d   qm1a1         s              1
     d   qm10a1        s             10
     d   qmLtFl        s              1
     d   qmScl#        s             25
     d   qmMcTm        s              4
     d   qmStWk        s              2
     d   qmDeDt        s              7
     d   qmDeem        s              2
     d   qmCmNu        s              3
     d   qmCmSu        s              3
     d   qmClad        s              3
     d   qmRrn         s              9  0
     d   qmDtRe        s              7
     d   qmClSu        s              3
     d   qm#Dep        s              3
     d   qmOcDs        s             40
     d   qmHiDt        s              7  0
     d   qmEsts        s              2
     d   qmWkPt        s              2
     d   qmPrex        s              1
     d   qmAcD2        s             60
     d   qmCvFl        s              1
     d   qmMca         s              1
     d   qmMmiP        s              5  2
     d   qmOdis        s              1
     d   qmYrlx        s              4  0
     d   qmDbDt        s              7  0
     d   qmHrDy        s              3  0
     d   qmDyWk        s              1  0
     d   qmClFa        s              3
     d   qmDny         s              1
     d   qmImp         s              1
     d   qmJrSt        s              2
     d   qmPoSt        s              2

       // Retrieve Company Information Variables
     d company#        s              3  0 Inz(777)
     d group#          s              3  0
     d companyName     s             40
     d groupName       s             40

       // Work Fields
     d e               s              5  0
     d n               s              5  0
     D i               s              3  0
     D accidentDate    s                   Like(CMacDt)
     D countCvt        s              5  0 Inz(0)
     D countErr1       s              5  0 Inz(0)
     D countErr2       s              5  0 Inz(0)
     D countRej        s              5  0 Inz(0)
     D claimName       s             40
     D fieldValue      s             36
     D currentDate     s              8  0
     D line132         s            132    Inz(*ALL'_')
     D amTrustClm#     s             40
     D*** Beg Add *** 07/09/25 ******************************************
     D CltId           s              3a   Inz
     D*** End Add *** 07/09/25 ******************************************
    D*==============================================================+
    D* Constants
    D*===============================================================
    D*
    D* Report Title lines
    D*
     D titleImported   C                   CONST('             Importe-
     D                                     d Claims            ')
     D titleExists     C                   CONST('           Preexisti-
     D                                     ng Claims           ')
     D titleErrors     C                   CONST('              Claim -
     D                                     Errors              ')
     D titleRejects    C                   CONST('            Rejected-
     D                                      Claims             ')
     D rhExp           C                   CONST('Initial Reserves Set-
     D                                     up')
    C*=========================================================================
    C*      M  A I  N    L  O  G  I  C
    C*=========================================================================
      /free
       //*** Beg Add *** 07/09/25 ******************************************
       // Get the client id
       GetClientID(CltId);
       //*** End Add *** 07/09/25 ******************************************

       // Process Claim Master and Other Files
       ProcClaims();

       // End of Program Processes
       ProgramEnd();

       *inLR = *On;

       //------------------------------------------------------------
       // Initialization Routine
       //------------------------------------------------------------
       Begsr *InzSR;

          // Get Company Information
          RtvCompInfo(company#:group#:companyName:groupName);

          // Initialize number of input date fields
          dsDttm = %Dec(%Timestamp());
          currentDate = %Int(%Subst(%EditC(dsDate:'X'):5:4) +
                             %Subst(%EditC(dsDate:'X'):1:4));

          // Print Report Headings
          Except HdrCvt;
          Except HdrDtlCvt;
          Except HdrErr1;
          Except HdrDtlErr1;
          Except HdrErr2;
          Except HdrDtlErr2;
          Except HdrRej;
          Except HdrDtlRej;

       EndSR;
       //------------------------------------------------------------

      /End-Free
     O*===================================================================
     O*  Claims Uploaded
     O*===================================================================
     Oqsysprt   E            HdrCvt         1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         86
     O                       currentDate        132 '  /  /    '
     O*
     O          E            HdrCvt         2
     O                       Q1pgm               10
     O                       titleImported       86
     O                       Q1user             120
     O                                          127 'Page'
     O                       Page          Z    132
     O*
     O          E            HdrDtlCvt      0
     O                       line132            132
    O*
     O          E            HdrDtlCvt      1
     O                                           20 'AmTrust Claim #     '
     O                                           40 '                    '
     O                                           60 '  WC Claim #       C'
     O                                           80 'laimant Name        '
    O*
     O          E            DtlCvt         1
     O                       CMpds#              40
     O                       CMfnd         X   +002
     O                                         +000 '-'
     o                       CMfYr         X   +000
     O                                         +000 '-'
     o                       CMcase        X   +000
     O                       claimName         +002
    O*
     O          E            DtlTotCvt   2
     O                                           22 'Converted Claim Count:'
     O                       countCvt      Z   +001
    O*
     O          E            NoConvert      1
     O                                           25 'No claims were imported.'
     O*===================================================================
     O*  Preexisting Claims
     O*===================================================================
     Oqsysprt1  E            HdrErr1        1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         86
     O                       currentDate        132 '  /  /    '
     O*
     O          E            HdrErr1        2
     O                       Q1pgm               10
     O                       titleExists         86
     O                       Q1user             120
     O                                          127 'Page'
     O                       Page1         Z    132
     O*
     O          E            HdrDtlErr1     0
     O                       line132            132
    O*
     O          E            HdrDtlErr1     1
     O                                           20 'AmTrust Claim #     '
     O                                           40 '                    '
     O                                           60 '  WC Claim #       C'
     O                                           80 'laimant Name        '
    O*
     O          E            DtlErr1        1
     O                       CMpds#              40
     O                       CMfnd         X   +002
     O                                         +000 '-'
     o                       CMfYr         X   +000
     O                                         +000 '-'
     o                       CMcase        X   +000
     O                       claimName         +002
    O*
     O          E            DtlTotErr1  2
     O                                           25 'Pre-existing Claim Count:'
     O                       countErr1     Z   +001
    O*
     O          E            NoExists       1
     O                                           24 'No pre-existing claims.'
     O*===================================================================
     O*  Claims with Errors.
     O*===================================================================
     Oqsysprt2  E            HdrErr2        1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         86
     O                       currentDate        132 '  /  /    '
    O*
     O          E            HdrErr2        2
     O                       Q1pgm               10
     O                       TitleErrors         86
     O                       Q1user             120
     O                                          127 'Page'
     O                       Page2         Z    132
     O*
     O          E            HdrDtlErr2     0
     O                       line132            132
    O*
     O          E            HdrDtlErr2     1
     O                                           20 'AmTrust Claim #     '
     O                                           40 '                    '
     O                                           60 '  WC Claim #       R'
     O                                           80 'eason               '
     O                                          100 '                Data'
    O*
     O          E            DtlErr2        1
     O                       amTrustClm#         40
     O                       CMfnd         X   +002
     O                                         +000 '-'
     o                       CMfYr         X   +000
     O                                         +000 '-'
     o                       CMcase        X   +000
     O                       err(e)            +002
     O                       fieldValue        +002
    O*
     O          E            DtlTotErr2  2
     O                                           12 'Error Count:'
     O                       countErr2     Z   +001
    O*
     O          E            NoErrors       1
     O                                           17 'No claim errors.'
     O*
     O          E            ErrorSum2      2 06
     O                                           35 '** Error Summary **   '
    O*
     O          E            ErrSum2Dtl     1
     O                       erc(i)        Z     10
     O                       err(i)            +  3
     O*===================================================================
     O*  Rejected Claims
     O*===================================================================
     Oqsysprt3  E            HdrRej         1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         86
     O                       currentDate        132 '  /  /    '
    O*
     O          E            HdrRej         2
     O                       Q1pgm               10
     O                       TitleRejects        86
     O                       Q1user             120
     O                                          127 'Page'
     O                       Page3         Z    132
     O*
     O          E            HdrDtlRej      0
     O                       line132            132
    O*
     O          E            HdrDtlRej      1
     O                                           20 'AmTrust Claim #     '
     O                                           40 '                    '
     O                                           60 '  Reason            '
     O                                           80 '                   D'
     O                                          100 'ata                 '
    O*
     O          E            DtlRej         1
     O                       amTrustClm#         40
     O                       rej(e)            +002
     O                       fieldValue        +002
    O*
     O          E            DtlTotRej   2
     O                                           16 'Rejection Count:'
     O                       countRej      Z   +001
    O*
     O          E            NoRejects      2
     O                                           19 'No rejected claims.'
     O*
     O          E            RejectSum      2 06
     O                                           36 '** Rejection Summary **'
    O*
     O          E            RejSumDtl      1
     O                       rjc(i)        Z     10
     O                       rej(i)            +  3
       //===================================================================
       // CvtDates - Convert Incoming Dates.
       //===================================================================
     P CvtDates        B
     D CvtDates        PI             7  0
     d   dateIn                       8    Const

     d  intDate        s              7  0
     d  intDate8       s              8  0
      /free

       If dateIn <> ' ';
          intDate8 = %Int(%Trim(dateIn));
       Else;
          intDate8 = 0;
       EndIf;

       If intDate8 <> 0;
          Monitor;
             intDate = %Dec(%Date(intDate8:*iso):*cymd);
          On-Error;
             intdate = 0;
          EndMon;
       Else;
          intdate = 0;
       EndIf;

       Return intDate;

      /End-Free
     P CvtDates        E
       //===================================================================
       // GetClaim# - Get Claim #
       //===================================================================
     P GetClaim#       B
     D GetClaim#       PI             7  0
     D   group#                       3  0

     D claim#          s              7  0
      /Free

       Chain (company#:group#) pfn00;
       If %Found(wmfnl);
          claim# = FNnacs;
          FNnacs += 1;
          //*** Beg Add ***** 05/23/24 ***********************
          // Maintenance Fields
          FNuser = q1User;
          FNpgm = q1pgm;
          FNtime = %Dec(%Char(%Time():*hms0):6:0);
          FNdt   = %Int(%Char(%Date():*cymd0));
          //*** End Add ***** 05/23/24 ***********************
          Update pfn00;
       EndIf;

       Return claim#;

      /End-Free
     P GetClaim#       E
       //===================================================================
       // GetYear - Get Policy Year
       //===================================================================
     P GetYear         B
     D GetYear         PI             3  0
     D   group#                       3  0
     D   accidentDate                 7  0

     D year            s              3  0
      /Free

       SetGT  (company#:group#:accidentDate) pf101;
       ReadPE (company#:group#)              pf101;
       If Not %Eof(wdf1l1);
          If accidentDate <= F1enPo;
             year = F1fYr;
          Else;
             year = %Int(%Subst(%EditC(accidentDate:'X'):1:3));
          EndIf;
       Else;
          year = %Int(%Subst(%EditC(accidentDate:'X'):1:3));
       EndIf;

       Return year;

      /End-Free
     P GetYear         E
       //===================================================================
       // EdtClmImportE - Edit the Claim Import - Errors
       //===================================================================
     P EdtClmImportE   B
     D EdtClmImportE   PI

     D importData      s             40
     D tbid            s                   Like(GTtbid)
     D key             s                   Like(GTkey)
     D zipcode         s              9  0

      /Free

       //
       // First Name
       If CIfsn = ' ';
          importData = 'Missing';
          PrtClmErrors('E':1:importData);
       EndIf;

       //
       // Last Name
       If CIlsn = ' ';
          importData = 'Missing';
          PrtClmErrors('E':2:importData);
       EndIf;

       //
       // Address 1
       If CIadr1 = ' ';
          importData = 'Missing';
          PrtClmErrors('E':3:importData);
       EndIf;

       //
       // City
       If CIcty = ' ';
          importData = 'Missing';
          PrtClmErrors('E':4:importData);
       EndIf;

       //
       // State
       If CIst = ' ';
          importData = 'Missing';
          PrtClmErrors('E':5:importData);
       EndIf;

       //
       // Zip Code
       If CIzip = ' ';
          CIzip      = '000000000';
          importData = 'Missing';
          PrtClmErrors('E':6:importData);
       EndIf;

       //
       // Country
       If CIctry = ' ';
          importData = 'Missing';
          PrtClmErrors('E':7:importData);
       EndIf;

       //
       // Zip Code Must be valid number
       If CIzip <> ' ';
          Monitor;
             zipcode = %Int(CIzip);
          On-Error *All;
             importData = CIzip;
             CIzip = '000000000';
             PrtClmErrors('E':6:importData);
          EndMon;
       EndIf;

       //
       // Gender
       tbid = 'GNDR';
       key  = CIsex;
       Chain (company#:tbid:key) pgt00;
       If Not %Found(wtgtl);
          If CIsex = ' ';
             importData = 'Missing';
          Else;
             importData = CIsex;
          EndIf;
          PrtClmErrors('E':8:importData);
       EndIf;

       //
       // Marital Status
       tbid = 'MSTS';
       key  = CImSts;
       Chain (company#:tbid:key) pgt00;
       If Not %Found(wtgtl);
          If CImSts = ' ';
             importData = 'Missing';
          Else;
             importData = CImSts;
          EndIf;
          PrtClmErrors('E':9:importData);
       EndIf;

       //
       // Date of Birth
       If CvtDates(CIdtob) = 0;
          If CIdtob <> ' ';
             importData = CIdtob;
          Else;
             importData = 'Missing';
          EndIf;
          PrtClmErrors('E':10:importData);
       EndIf;

       //
       // Lost Time Flag
       If CIltFl <> ' ' And CIltFl <> 'I' And CIltFl <> 'M' And
          CIltFl <> 'R';
          importData = CIltFl;
          PrtClmErrors('E':11:importData);
          CIltFl = 'M';
       EndIf;

       //
       // Claim Status
       If CIstat <> ' ' And CIstat <> 'C' And CIstat <> 'O';
          importData = CIstat;
          PrtClmErrors('E':12:importData);
          CIstat = 'O';
       EndIf;

       //
       // Accident Description
       If CIacds = ' ';
          importData = 'Missing';
          PrtClmErrors('E':13:importData);
       EndIf;

       //
       // Nature Code
       Chain (company#:CInatr) pnaedi;
       If Not %Found(wtnaledi);
          If CInatr = ' ';
             importData = 'Missing';
          Else;
             importData = CInatr;
          EndIf;
          PrtClmErrors('E':14:importData);
       EndIf;

       //
       // Part Code
       Chain (company#:CIpart) ppaedi;
       If Not %Found(wtpaledi);
          If CIpart = ' ';
             importData = 'Missing';
          Else;
             importData = CIpart;
          EndIf;
          PrtClmErrors('E':15:importData);
       EndIf;

       //
       // Cause Code
       Chain (company#:CIcaus) pcaedi;
       If Not %Found(wtcaledi);
          If CIcaus = ' ';
             importData = 'Missing';
          Else;
             importData = CIcaus;
          EndIf;
          PrtClmErrors('E':16:importData);
       EndIf;

       //
       // Loss Coverage Code
       tbid = CIjrSt + 'LC';
       Chain (company#:tbid) pgi00;
       If Not %Found(wtgil);
          tbid = '  LC';
       EndIf;

       key  = CIlscv;
       Chain (company#:tbid:key) pgt00;
       If Not %Found(wtgtl);
          If CIlsCv = ' ';
             importData = 'Missing';
          Else;
             importData = CIlscv;
          EndIf;
          PrtClmErrors('E':17:importData);
       EndIf;

       //
       // NCCI Code
       tbid = CIjrSt + 'NC';
       Chain (company#:tbid) pgi00;
       If Not %Found(wtgil);
          tbid = '  NC';
       EndIf;

       key  = CIncci;
       Chain (company#:tbid:key) pgt00;
       If Not %Found(wtgtl);
          If CIncci = ' ';
             importData = 'Missing';
          Else;
             importData = CIncci;
          EndIf;
          PrtClmErrors('E':18:importData);
       EndIf;

       //
       // Jurisdiction State
       key = CIjrSt;
       Chain (key) pst00;
       If Not %Found(stabl);
          If CIjrSt = ' ';
             importData = 'Missing';
          Else;
             importData = CIjrSt;
          EndIf;
          PrtClmErrors('E':19:importData);
       EndIf;

       //
       // Date Received
       If CvtDates(CIrcvD) = 0;
          If CIrcvd <> ' ';
             importData = CIrcvD;
          Else;
             importData = 'Missing';
          EndIf;
          PrtClmErrors('E':20:importData);
       Else;
          If CvtDates(CIrcvD) > CvtDates(%EditC(dsDate:'X'));
             importData = CIrcvD;
             PrtClmErrors('E':30:importData);
          EndIf;
       EndIf;

       //
       // Date Reported
       If CvtDates(CIrpDt) = 0;
          If CIrpDt <> ' ';
             importData = CIrpDt;
          Else;
             importData = 'Missing';
          EndIf;
          PrtClmErrors('E':21:importData);
       Else;
          If CvtDates(CIrpDt) > CvtDates(%EditC(dsDate:'X'));
             importData = CIrpDt;
             PrtClmErrors('E':29:importData);
          EndIf;
       EndIf;

       //
       // Date of Death
       If CIdod <> ' ';
          If CvtDates(CIdod) = 0;
             If CIdod <> ' ';
                importData = CIdod;
             EndIf;
             PrtClmErrors('E':22:importData);
          EndIf;
       EndIf;

       //
       // Date of Hire
       If CIhiDt <> ' ';
          If CvtDates(CIhiDt) = 0;
             If CIhiDt <> ' ';
                importData = CIhiDt;
             EndIf;
             PrtClmErrors('E':23:importData);
          EndIf;
       EndIf;

       //
       // Date Last Reported
       If CIdtLw <> ' ';
          If CvtDates(CIdtLw) = 0;
             If CIdtLw <> ' ';
                importData = CIdtLw;
             EndIf;
             PrtClmErrors('E':24:importData);
          EndIf;
       EndIf;

       //
       // Initial Entry Date
       If CvtDates(CIiDt) = 0;
          If CIiDt <> ' ';
             importData = CIiDt;
          Else;
             importData = 'Missing';
          EndIf;
          PrtClmErrors('E':25:importData);
       EndIf;

       // Claim Adjuster must be valid
       If CIclex = ' ';
          CIclex = 'KH';
       Else;
          Chain CIclex pup01;
          If Not %Found(smupl1);
             importData = CIclex;
             PrtClmErrors('E':26:importData);
          EndIf;
       EndIf;

       // Claim Supervisor must be valid
       If CIsupr = ' ';
          CIsupr = 'RS';
       Else;
          Chain CIsupr pup01;
          If Not %Found(smupl1);
             importData = CIsupr;
             PrtClmErrors('E':27:importData);
          EndIf;
       EndIf;

       // Accident shouln't be future date.
       If CvtDates(CIacDt) > CvtDates(%Editc(dsDate:'X'));
          importData = CIacDt;
          PrtClmErrors('E':28:importData);
       EndIf;

      /End-Free

     P EdtClmImportE   E
       //===================================================================
       // EdtClmImportR - Edit the Claim Import - Rejected
       //===================================================================
     P EdtClmImportR   B
     D EdtClmImportR   PI              n

     D skip            s               n   Inz(*off)
     D importData      s             40
     D tbid            s                   Like(GTtbid)
     D key             s                   Like(GTkey)

      /Free

       // Check Errors that will keep the claim from being imported
       //
       // Group must be valid if sent
       If CIfnd <> 0;
          Chain (company#:CIfnd) pfn00;
          If Not %Found(wmfnl);
             importData = %EditC(CIfnd:'X');
             PrtClmErrors('R':1:importData);
             skip = *On;
          EndIf;
       Else;
          CIfnd = 73;
       EndIf;

       //
       // Accident Date must be valid
       If CvtDates(CIacdt) = 0;
          If CIacDt <> ' ';
             importData = CIacDt;
          Else;
             importData = 'Missing';
          EndIf;
          PrtClmErrors('R':2:importData);
          skip = *On;
       EndIf;

       //
       // Social Security # must be sent.
       If CIss# = ' ';
          importData = 'Missing';
          PrtClmErrors('R':3:importData);
          skip = *On;
       EndIf;

       //
       // AmTrust Claim # must be sent.
       If CIclm# = ' ';
          importData = 'Missing';
          PrtClmErrors('R':4:importData);
          skip = *On;
       EndIf;

       //
       // Employer/Division must be valid if sent
       If CIemp# <> 0;
          Chain (company#:CIfnd:CIemp#:CIdiv) pem00;
          If Not %Found(wmeml);
             importData = %EditC(CIemp#:'X') + '-' + %EditC(CIdiv:'X');
             PrtClmErrors('R':5:importData);
             skip = *On;
          EndIf;
       Else;
          CIemp# = 1;
          CIdiv  = 0;
       EndIf;

       Return skip;

      /End-Free

     P EdtClmImportR   E
       //===================================================================
       // ProcClaims - Process Claim Master  (WMCMP/FMCMP)
       //===================================================================
     P ProcClaims      B
     D ProcClaims      PI

     D skip            s               n   Inz(*off)
      /Free

       // Read thru the claim import file
       Read pci0p;
       DoW Not %Eof(wxcip);
          // Does the Claim Already Exist?
          //*** Beg Chg ***** 11/07/24 ********************************
          //***amTrustClm# = %Trim(CIclm#) + '-' + %EditC(CIclmSeq#:'X');
          amTrustClm# = %Trim(CIclm#);
          //*** End Chg ***** 11/07/24 ********************************
          Chain (company#:amTrustClm#) pcm017;
          If Not %Found(wmcml17);
             // Edit Claim Import
             skip = EdtClmImportR();
             // Skip Claim Creation if critical error occurs
             If skip = *Off;
                // Write Claim
                WriteClaim();
             EndIf;
          Else;
             // Print Claim Already Exists
             PrtClmExists();
          EndIf;
          Read pci0p;
       EndDo;
    
      /End-Free
     P ProcClaims      E
       //===================================================================
       // ProgramEnd - Program End Processing
       //===================================================================
     P ProgramEnd      B
     D ProgramEnd      PI

     D chrRtnCode      s              1    Inz('*')
      /free

       // Shut down MC files.

       // Shutdown WCMC Medical Cases
       WrtWcmcClmMast(chrRtnCode);

       // Shutdown WCMC Medical Case Demographics
       WrtWcmcClmDemo(chrRtnCode);

       // Shutdown WCMC Medical Case History
       WrtWcmcClmHist(chrRtnCode);

       // Print Totals
       PrtClmTotals();

      /End-Free
     P ProgramEnd      E
       //===================================================================
       // PrtClmConvert - Print that the claim was converted
       //===================================================================
     P PrtClmConvert   B
     D PrtClmConvert   PI

      /free

       If *inOA = *On;
          Except HdrCvt;
          Except HdrDtlCvt;
          *inOA = *Off;
       EndIf;

       If CMmid <> ' ';
          claimName = %Trim(CMfsn) + ' ' + %Trim(CMmid) + '. ' + %Trim(CMlsn);
       Else;
          claimName = %Trim(CMfsn) + ' ' + %Trim(CMlsn);
       EndIf;

       Except DtlCvt;

       countCvt += 1;

      /End-Free
     P PrtClmConvert   E
       //===================================================================
       // PrtClmErrors - Print that the claim has errors
       //===================================================================
     P PrtClmErrors    B
     D PrtClmErrors    PI
     d  type                          1    Const
     d  error                         2  0 Const
     d  importData                   40

      /free

       e = error;
       fieldValue = importData;

       Select;
       // Print Claim Errors
       When type = 'E';
          If *inOC = *On;
             Except HdrErr2;
             Except HdrDtlErr2;
             *inOC = *Off;
          EndIf;

          Except DtlErr2;
          erc(e)    += 1;
          countErr2 += 1;
       // Print Rejected Claims
       When type = 'R';
          If *inOD = *On;
             Except HdrRej;
             Except HdrDtlRej;
             *inOD = *Off;
          EndIf;

          Except DtlRej;
          rjc(e)   += 1;
          countRej += 1;
       EndSl;

      /End-Free
     P PrtClmErrors    E
       //===================================================================
       // PrtClmExists - Print that the claim already exists
       //===================================================================
     P PrtClmExists    B
     D PrtClmExists    PI

      /free

       If *inOB = *On;
          Except HdrErr1;
          Except HdrDtlErr1;
          *inOB = *Off;
       EndIf;

       If CMmid <> ' ';
          claimName = %Trim(CMfsn) + ' ' + %Trim(CMmid) + '. ' + %Trim(CMlsn);
       Else;
          claimName = %Trim(CMfsn) + ' ' + %Trim(CMlsn);
       EndIf;

       Except DtlErr1;

       countErr1 += 1;

      /End-Free
     P PrtClmExists    E
       //===================================================================
       // PrtClmTotals - Print claim totals
       //===================================================================
     P PrtClmTotals    B
     D PrtClmTotals    PI

      /free

       //* Print the Total Claims Converted
       If *inOA = *On;
          Except HdrCvt;
          *inOA = *Off;
       EndIf;

       If countCvt = 0;
          Except NoConvert;
       Else;
          Except DtlTotCvt;
       EndIf;

       //* Print the Total Claims Not Converted because the claim exists
       If *inOB = *On;
          Except HdrErr1;
          *inOB = *Off;
       EndIf;

       If countErr1 = 0;
          Except NoExists;
       Else;
          Except DtlTotErr1;
       EndIf;

       //* Print Claim Error Totals
       If *inOC = *On;
          Except HdrErr2;
          *inOC = *Off;
       EndIf;

       If countErr2 = 0;
          Except NoErrors;
       Else;
          Except DtlTotErr2;
          Except ErrorSum2;
          For i=1 to %Elem(err);
             Except ErrSum2Dtl;
          EndFor;
       EndIf;

       //* Print Rejected Claim Totals
       If countRej = 0;
          Except NoRejects;
       Else;
          Except DtlTotRej;
          Except RejectSum;
          For i=1 to %Elem(rej);
             Except RejSumDtl;
          EndFor;
       EndIf;

      /End-Free
     P PrtClmTotals    E
       //===================================================================
       // WriteClaim - Write Claim Master/Demo/History
       //===================================================================
     P WriteClaim      B
     D WriteClaim      PI

     D chrRtnCode      s              1    Inz(' ')
      /Free

       Clear pcm01;

       CMco#  = company#;
       CMfnd  = CIfnd;
       CMss#  = CIss#;
       CMacDt = CvtDates(CIacDt);
       CMfyr  = GetYear(CMfnd:CMacdt);
       CMcase = GetClaim#(CMfnd);
       CMemp# = CIemp#;
       CMdiv  = CIdiv;
       CMclex = CIclex;
       CMsupr = CIsupr;

       If CIrt01 = ' ';
          CMrt01 = 'ME';
       Else;
          CMrt01 = CIrt01;
       EndIf;

       If CIrt02 = ' ';
          CMrt02 = 'EX';
       Else;
          CMrt02 = CIrt02;
       EndIf;

       If CIrt03 = ' ';
          CMrt03 = 'CO';
       Else;
          CMrt03 = CIrt03;
       EndIf;

       If CIrt04 = ' ';
          CMrt04 = 'LE';
       Else;
          CMrt04 = CIrt04;
       EndIf;

       If CIrt05 = ' ';
          CMrt05 = 'RE';
       Else;
          CMrt05 = CIrt05;
       EndIf;

       If CIrr01 = 0;
          CMrr01 = 500000;
         //*** Beg Add *** 07/09/25 ******************************************
          // For CSI, load Medical Reserve amount from default WMFNL table value
          If  CltId = '114'
          and FNdfrc = CMrt01;    //Medical reserve code
             CMrr01 = FNdfra;     //Medical reserve amount
          Endif;
         //*** End Add *** 07/09/25 ******************************************
       Else;
          CMrr01 = CIrr01;
       Endif;
       CMrr02 = CIrr02;
       CMrr03 = CIrr03;
       CMrr04 = CIrr04;
       CMrr05 = CIrr05;
       CMar01 = CIar01;
       CMar02 = CIar02;
       CMar03 = CIar03;
       CMar04 = CIar04;
       CMar05 = CIar05;

       CMrtwQ = ' ';
       CMiTrt = ' ';
       CMacSt = ' ';
       CMstat = CIstat;

       CMdtLw = CvtDates(CIdtLw);
       If CMdtLw < CMacdt;
          CMdtLw = 0;
       EndIf;

       CMdod = CvtDates(CIdod);
       If CMdod <> 0;
          CMdth = 'Y';
       EndIf;

       CMltFl = CIltFl;
       CMncci = CIncci;

       CMeAdr = 'Y';

       If CMeAdr = 'Y';
          Chain (CMco#:CMfnd:CMemp#:CMdiv) pem00;
          If %Found(wmeml);
             CMacNm = EMname;
             If EMmpFl = 'M';
                CMacdr = EMmad1;
                CMaCty = EMmCty;
                CMaSt  = EMmSt;
                CMacZ1 = %Int(%Subst(%EditC(EMmZip:'X'):1:5));
                CMacZ2 = %Int(%Subst(%EditC(EMmZip:'X'):6:4));
             Else;
                CMacdr = EMpad1;
                CMaCty = EMpCty;
                CMaSt  = EMpSt;
                CMacZ1 = %Int(%Subst(%EditC(EMpZip:'X'):1:5));
                CMacZ2 = %Int(%Subst(%EditC(EMpZip:'X'):6:4));
             EndIf;
             CMoff# = EMoff#;
          EndIf;
       EndIf;

       CMacDs = CIacDS;
       CMacD2 = CIacD2;
       CMocDs = CIocDs;
       CMctg1 = CIctg1;
       CMsubr = CIsubr;
       CMiPct = %Dec(%Trim(CIiPct):5:2);

       CMlsn = CIlsn;
       CMfsn = CIfsn;
       CMmid = CImid;

       CMhiDt = CvtDates(CIhiDt);

       If CIidt = ' ';
          CMentD = %Int(%Char(%Date():*cymd0));
       Else;
          CMentD = CvtDates(CIiDt);
       EndIf;

       CMdtRp = CvtDates(CIrpDt);
       If CMdtRp = 0;
          CMdtRp = CMentD;
       EndIf;

       CMrcvD = CvtDates(CIrcvD);
       If CMrcvD = 0;
          CMrcvD = CMdtRp;
       EndIf;

       // Assign Nature, Part and Cause
       //Nature
       Chain (CMco#:CInatr) pnaedi;
       If %Found(wtnaledi);
          CMnatr = NAkey;
       EndIf;
       //Part
       Chain (CMco#:CIpart) ppaedi;
       If %Found(wtpaledi);
          CMpart = PAkey;
       EndIf;
       //Cause
       Chain (CMco#:CIcaus) pcaedi;
       If %Found(wtcaledi);
          CMcaus = CAkey;
       EndIf;

       If CMltFl = 'I' and CMdbDt < CMacDt;
          CMdbDt = CMacDt;
       EndIf;

       //*** Beg Chg ***** 11/07/24 ********************************
       //***CMpds# = %Trim(CIclm#) + '-' + %EditC(CIclmSeq#:'X');
       //***CMcvt# = %Trim(CIclm#) + '-' + %EditC(CIclmSeq#:'X');
       CMpds# = %Trim(CIclm#);
       CMcvt# = %Trim(CIclm#);
       CMacl# = %Trim(CIclm#);
       //*** End Chg ***** 11/07/24 ********************************

       If CIclas <> ' ';
          CMclas = %Int(%Trim(CIclas));
       EndIf;

       CMdny  = CIdny;
       CMcvFl = CIcvFl;
       CMjrSt = CIjrSt;
       CMlsCv = CIlsCv;
       CMcovt = CIcovT;
       CMmcTp = CImcTp;
       CMvorc = CIvorc;

       If CIcata <> ' ';
          Monitor;
             CMcata = %Int(%Trim(CIcata));
          On-Error;
             CMcata = 0;
          EndMon;
       EndIf;

       If CMltFl = 'I';
          CMaww = CIaww;
          CMcrt = 0;
          // Write/Update AWW History
          WrtClmAWW();
       EndIf;

       // Write/Update Claim Legal
       WrtClmLegal();

       // Write Claim Reserves
       WrtClmRsvs();

       // Write/Update Claim Demographic Data
       WrtClmDemo();

       // Write/Update Claim History
       WrtClmHistory();

       // Maintenance Fields
       CMuser = q1User;
       CMpgm = q1pgm;
       CMtime = %Dec(%Char(%Time():*hms0):6:0);
       CMdt   = %Int(%Char(%Date():*cymd0));

       // Init Fields
       CMiUsr = q1User;
       CMiPgm = q1pgm;
       CMiTm = %Dec(%Char(%Time():*hms0):6:0);
       CMiDt = %Int(%Char(%Date():*cymd0));

       Write pcm01;

       // Check for Claim Errors
       EdtClmImportE();

       // Print Claim Converted Report
       PrtClmConvert();

       // Create the WCMC Medical Case File
       WrtWcmcClmMast(chrRtnCode);

      /End-Free

     P WriteClaim      E
       //===================================================================
       // WrtClmAWW - Write Claim Average Weekly Wage (WMAWP)
       //===================================================================
     P WrtClmAWW       B
     D WrtClmAWW       PI
      /Free

          Clear paw0p;
          AWco#= CMco#;
          AWfnd= CMfnd;
          AWfyr= CMfyr;
          AWcase= CMcase;

          AWAWW  = CMaww;
          AWtrDt = CMacDt;
          AWcrt  = CMcrt;

          // Maintenance Fields
          AWuser = q1User;
          AWpgm = q1pgm;
          AWtime = %Dec(%Char(%Time():*hms0):6:0);
          AWdt   = %Int(%Char(%Date():*cymd0));

          Write paw0p;

      /End-Free
     P WrtClmAWW       E
       //===================================================================
       // WrtClmDemo - Write Claim Demographic
       //===================================================================
     P WrtClmDemo      B
     D WrtClmDemo      PI

     D chrRtnCode      s              1    Inz(' ')
     D demoFound       s               n   Inz(*Off)
     D zipcode         s              9  0
      /Free

       Chain CMss# pcd00;
       If %Found(WMCDL);
          demoFound = *On;
       Else;
          demoFound = *Off;
       EndIf;

       ExSr MoveToDB;

       //------------------------------------------------------------
       //  Move claim data to Database
       //------------------------------------------------------------
       BegSr MoveToDB;

          If Not DemoFound;
             Clear pcd00;
             CDss# = CMss#;

             // Init Fields
             CDiUsr = q1User;
             CDiPgm = q1pgm;
             CDiTm = %Dec(%Char(%Time():*hms0):6:0);
             CDiDt = %Int(%Char(%Date():*cymd0));
          EndIf;

          CDlsn  = CMlsn;
          CDfsn  = CMfsn;
          CDmid  = CMmid;
          CDadr1 = CIadr1;
          CDadr2 = CIadr2;
          CDcty  = CIcty;
          CDst   = CIst;
          CDadr3 = CIadr3;

          If CIzip <> ' ';
             zipcode = %Int(%Trim(CIzip));
             If zipCode < 100000;
                CDzip = zipCode * 10000;
             Else;
                CDzip = zipCode;
             EndIf;
          EndIf;

          CDdtob = CvtDates(CIdtob);
          CDsex  = CIsex;
          CDmSts = CImSts;

          If CDsex = ' ';
             CDsex = 'U';
          Endif;

          If CDmSts = ' ';
             CDmSts = 'K';
          Endif;

          CDctry = CIctry;
          CDhicn = ' ';
          CDmdBn = ' ';

          // Maintenance Fields
          CDuser = q1User;
          CDpgm = q1pgm;
          CDtime = %Dec(%Char(%Time():*hms0):6:0);
          CDdt   = %Int(%Char(%Date():*cymd0));

          If demoFound;
             Update pcd00;
          Else;
             Write pcd00;
          EndIf;

          // Write/Update WCMC Case Demographics
          WrtWcmcClmDemo(chrRtnCode);

       EndSr;
       //-------------------------------------------------------------------
    
      /End-Free
     P WrtClmDemo      E
       //===================================================================
       // WrtClmHistory - Write Claim History (WMCHP)
       //===================================================================
     P WrtClmHistory   B
     D WrtClmHistory   PI

     D chrRtnCode      s              1    Inz(' ')
      /Free

       Clear pch0p;

       CHco# = CMco#;
       CHfnd = CMfnd;
       CHfyr = CMfyr;
       CHcase = CMcase;
       CHuser = q1User;
       CHpgm = q1Pgm;
       CHtime = %Dec(%Char(%Time():*hms0):6:0);
       CHdt   = CMentD;
       CHefDt = CMentD;
       CHocSt = 'O';
       CHexpl = 'Claim opened';

       Write pch0p;

       // Write WCMC Case History
       WrtWcmcClmHist(chrRtnCode);

       If CIstat = 'C';
          CHefDt = CMdt;
          CHocSt = 'C';
          CHexpl = 'Claim closed';

          Write pch0p;

          // Write WCMC Case History
          WrtWcmcClmHist(chrRtnCode);
       EndIf;

      /End-Free
     P WrtClmHistory   E
       //===================================================================
       // WrtClmLegal - Write Claim Legal (WMLLP)
       //===================================================================
     P WrtClmLegal     B
     D WrtClmLegal     PI

     D legalFound      s               n   Inz(*Off)
      /Free

       Chain (CMco#:CMfnd:CMfYr:CMcase) pll00;
       If %Found(wmlll);
          legalFound = *On;
       EndIf;

       ExSr MoveToDB;

       //------------------------------------------------------------
       //  Move claim data to Database
       //------------------------------------------------------------
       BegSr MoveToDB;

          If Not LegalFound;
             Clear pll00;
             LLco#= CMco#;
             LLfnd= CMfnd;
             LLfyr= CMfyr;
             LLcase= CMcase;

             // Init Fields
             LLiUsr = q1User;
             LLiPgm = q1pgm;
             LLiTm = %Dec(%Char(%Time():*hms0):6:0);
             LLiDt = %Int(%Char(%Date():*cymd0));
          EndIf;

          LLdtAp = 0;
          LLdefA = 0;
          LLdefM = 0;
          LLdefN = ' ';
          LLdefR = 0;
          LLdefP = 0;
          LLdeDt = 0;
          LLplaA = 0;
          LLplaM = 0;
          LLplaN = ' ';
          LLplaR = 0;
          LLplaP = 0;
          LLlump = CIlump;
          LLmTst = CImTst;
          LLfrau = CIfrau;
          LLlitg = CIlitg;
          LLlwDt = 0;
          LLstDt = 0;
          LLsAmt = 0;

          // Maintenance Fields
          LLuser = q1User;
          LLpgm = q1pgm;
          LLtime = %Dec(%Char(%Time():*hms0):6:0);
          LLdt   = %Int(%Char(%Date():*cymd0));

          If legalFound;
             Update pll00;
          Else;
             Write pll00;
          EndIf;

       EndSr;
       //-------------------------------------------------------------------

      /End-Free
     P WrtClmLegal     E
       //===================================================================
       // WrtClmRsvs  - Write Reserve History
       //===================================================================
     P WrtClmRsvs      B
     D WrtClmRsvs      PI

      /Free

       Clear prh0p;

       RHco# = company#;
       RHfnd = CMfnd;
       RHfYr = CMfYr;
       RHcase = CMcase;

       RHrt01 = CMrt01;
       RHrt02 = CMrt02;
       RHrt03 = CMrt03;
       RHrt04 = CMrt04;
       RHrt05 = CMrt05;
       RHsu01 = CMrr01 + CMar01;
       RHsu02 = CMrr02 + CMar02;
       RHsu03 = CMrr03 + CMar03;
       RHsu04 = CMrr04 + CMar04;
       RHsu05 = CMrr05 + CMar05;

       RHefDt = CMentD;
       RHexpl = rhExp;

       // Maintenance Fields
       RHuser = q1User;
       RHpgm = q1pgm;
       RHtime = %Dec(%Char(%Time():*hms0):6:0);
       RHdt   = %Int(%Char(%Date():*cymd0));

       // Init Fields
       RHiUsr = q1User;
       RHiPgm = q1pgm;
       RHiTim = %Dec(%Char(%Time():*hms0):6:0);
       RHiDt  = %Int(%Char(%Date():*cymd0));
       //***  Begin Add  ***  08/15/17  ************************************
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
       //***  End   Add  ***  08/15/17  ************************************

       Write prh0p;

      /End-Free
     P WrtClmRsvs      E
       //===================================================================
       // WrtWcmcClmDemo  - Write Manged Care Claim Demographics
       //===================================================================
     P WrtWcmcClmDemo  B
     D WrtWcmcClmDemo  PI
     D   returnCode                   1

     D chrRtnCode      s              1    Inz(' ')
     D chrAccDate      s              7    Inz(' ')
     D chrCompany#     s              3    Inz(' ')
     D chrSubscriber#  s              5    Inz(' ')
     D chrGroup#       s              3    Inz(' ')
     D chrYear         s              3    Inz(' ')
     D chrCase         s              7    Inz(' ')
      /Free
         chrRtnCode = returnCode;

         CrtWcmcClmDemo(chrRtnCode:CDss#:chrAccDate:chrCompany#:chrSubscriber#:
           chrGroup#:chrYear:chrCase:CDlsn:CDfsn:CDmid:CDadr1:CDadr2:CDcty:
           CDst:%EditC(CDzip:'X'):%EditC(CDdtob:'X'):CDsex:%EditC(CDarea:'X'):
           %EditC(CDphn#:'X'):CDmSts:%EditC(CDwAra:'X'):%EditC(CDwPh#:'X'):
           CDctry:CDadr3);

      /End-Free
     P WrtWCMCClmDemo  E
       //===================================================================
       // WrtWcmcClmHist  - Write Manged Care Claim History
       //===================================================================
     P WrtWcmcClmHist  B
     D WrtWcmcClmHist  PI
     D   returnCode                   1

     D chrRtnCode      s              1    Inz(' ')
     D intSubscriber#  s              5  0 Inz(0)
     d closeCode       s              3    Inz('1  ')
      /Free

         chrRtnCode = returnCode;

         CrtWcmcClmHist(CHco#:CHfnd:CHfyr:CHcase:CHefDt:CHocSt:CHExpl:closeCode:
           chrRtnCode);
    
      /End-Free
     P WrtWcmcClmHist  E
       //===================================================================
       // WrtWcmcClmMast  - Write Manged Care Claim Master
       //===================================================================
     P WrtWcmcClmMast  B
     D WrtWcmcClmMast  PI
     D   returnCode                   1

     D chrRtnCode      s              1
     D chrSubscriber#  s              5    Inz(' ')
     D chr1A1          s              1    Inz(' ')
     D chr10A1         s              1    Inz(' ')
     D chrMcTm         s              4    Inz(' ')
     D chrStWk         s              2    Inz(' ')
     D chrDeem         s              2    Inz(' ')
     D chrCmSu         s              3    Inz(' ')
     D intRrn          s              9  0 Inz(0)
     D chrWkPt         s              2    Inz(' ')
      /Free

         chrRtnCode = returnCode;

         If CMdth = 'Y';
            chrDeem = 'DI';
         Else;
            chrDeem = ' ';
         EndIf;

         chrWkPt = CMshft + ' ';

         CrtWcmcClmMast(chrRtnCode:CMss#:%EditC(CMacDt:'X'):%EditC(CMco#:'X'):
           chrSubscriber#:%EditC(CMfnd:'X'):%EditC(CMfyr:'X'):
           %EditC(CMcase:'X'):%EditC(CMemp#:'X'):%EditC(CMdiv:'X'):
           %EditC(CMacTm:'X'):%EditC(CMdtRp:'X'):%EditC(CMdtLw:'X'):CMacDs:
           CMmCon:%EditC(CMentD:'X'):%EditC(CMrcvD:'X'):%EditC(CMmmi:'X'):
           chr1A1:chr10A1:CMltFl:CMacl#:chrMcTm:chrStWk:%EditC(CMdod:'X'):
           chrDeem:CMnrse:chrCmSu:CMclex:intRrn:CMlsn:CMfsn:CMmid:
           %EditC(CMdtRe:'X'):CMsupr:%EditC(CM#Dep:'X'):CMocDs:CMhiDt:CMeSts:
           chrWkPt:CMprex:CMacD2:CMcvFl:CMmco:CMiPct:CMoDis:CMyrLx:CMdbDt:
           //*** Beg Chg ***** 11/07/24 **************************************
           //***CMhrsD:CMdays:CMfeAd:CMdny:CMimp:CMJrSt:CMstat);
           CMhrsD:CMdays:CMfeAd:CMdny:CMimp:CMJrSt:CMstat:'':CIjrst);
           //*** End Chg ***** 11/07/24 **************************************

      /End-Free
     P WrtWcmcClmMast  E
** TABLE   ERR               v   ERROR #    DESCRIPTIONS
First Name is missing.                 1
Last Name is missing.                  2
Claimant Address is missing.           3
Claimant City is missing.              4
Claimant State is missing.             5
Claimant Zip code is invalid.          6
Claimant Country is missing.           7
Invalid Gender.                        8
Invalid Marital Status.                9
Invalid Birth Date.                   10
Invalid Lost Time Flag.               11
Invalid Claim Status.                 12
Accident Description is missing.      13
Invalid Nature Code.                  14
Invalid Part Code.                    15
Invalid Cause Code.                   16
Invalid Loss Coverage Code.           17
Invalid NCCI Code.                    18
Invalid Jurisdiction Date.            19
Invalid Date Received.                20
Invalid Date Reported.                21
Invalid Date of Death.                22
Invalid Hire Date.                    23
Invalid Date Last Worked.             24
Invalid Initial Entry Date.           25
Invalid Claim Adjuster.               26
Invalid Supervisor.                   27
Accident Date is a future date.       28
Date Received is a future date.       29
Report Date is a future date.         30
** TABLE   REJ               v   ERROR #    DESCRIPTIONS
Invalid Group.                         1
Invalid Accident Date.                 2
Social Security # is missing.          3
AmTrust Claim # is missing.            4
Invalid Employer/Division.             5
