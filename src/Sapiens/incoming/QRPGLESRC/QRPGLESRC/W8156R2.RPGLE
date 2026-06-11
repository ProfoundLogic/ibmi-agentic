     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H*=========================================================================
    H/TITLE   W8156R2 - Update/Add Gallagher-Basset Claims
    H*=========================================================================
     H* Created on 9/19/2016 by c:62 for project 114WYV
    H*=========================================================================
     h* Revisions
     H*                                                               *
     H* 05/23/24 C:300 Support: 115634 (SUNZ)                         *
     H*                Update WMFNP Maintenance fields while updating *
     H*                Next Available Claim Number                    *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     h*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
    F*
    F* WC Claim Setup File
     Fwagbcl    uf   e           k Disk    Rename(pgbc01:pgbc00) InfDS(dbfds)
    F*
    F* WC Claim Exposure File
     Fwagbrl    uf   e           k Disk    Rename(pgbr01:pgbr00)
    F*
    F* WC Claim Payment File
     Fwagbpl    uf   e           k Disk    Rename(pgbp01:pgbp00)
    F*
    F* WC Claim 3rd Party Recovery File
     Fwagb3l    uf   e           k Disk    Rename(pgb301:pgb300)
    F*
    F* WC Claim Excess Recovery File
     Fwagbel    uf   e           k Disk    Rename(pgbe01:pgbe00)
    F*
    F* Employer Master File by CO#/FND/EMP#/DIV
     Fwmeml     if   e           k Disk    Rename(pem01:pem00)
    F*
    F* Group Master File by CO#/FND
     Fwmfnl     uf   e           k Disk    Rename(pfn01:pfn00)
    F*
    F* Claim Conversion Cross-reference
     Fwacxl     if a e           k Disk    Rename(pcx01:pcx00)
    F*
    F* Employer xross-reference file
     Fwaexl     if   e           k DISK    Rename(pex01:pex00)
    F*
    F* Provider cross-reference file                                      cmc
     Fwapxl     if a e           k Disk    Rename(ppx01:ppx00)
    F*
    F* Table File xross-reference file
     Fwaxcl     if   e           k DISK    Rename(pxc01:pxc00)
    F*
    F* Payee demographics master by fed tax id/modifier
     Fwmpdl     if   e           k Disk    Rename(ppd01:ppd00)
    F*
    F* Claimant Demographics by SSN#
     Fwmcdl     uf a e           k Disk    Rename(pcd01:pcd00)
    F*
    F* Check View file by co/fnd/fyr/case/seq#
     Fwdrpl3    if a e           k Disk    Rename(prp01:prp03)
    F*
    F* Claims checks payment file
     Fwdpap     o    e             Disk    Rename(ppa01:ppa0p)
     Fwdpal     if   e           k Disk    Rename(ppa01:ppa00) Prefix(p1:2)
    F*
    F* Claim file
     Fwmcml1    uf a e           k Disk
    F*
    F* Payee demographics master
     Fwmpdp     o    e             Disk    Rename(ppd01:ppd0p)
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
    F*
    F* Updated/Added Claims
     Fqsysprt   o    f  132        Printer OflInd(*inOA)
    F*
    F* Claims That weren't Loaded.
     Fqsysprt1  o    f  132        Printer OflInd(*inOB)
    F*
    F* Claim Error Register
     Fqsysprt2  o    f  132        Printer OflInd(*inOC)
    F*
    F* Providers Added.
     Fqsysprt3  o    f  132        Printer OflInd(*inOD)

    D*=========================================================================
    D* Prototypes
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
     d   qmPoSt                       2    Const

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

     d* Create WCMC Provider Information
     d CrtWcmcProv     pr                  ExtPgm('FA005RC')
     d   qmMode                       1
     d   qmRRNpd                      9  0
     d   qmRRNp1                      9  0
     d   qmRtCd                       1
     d   qmFein                       9  0 Const
     d   qmMod                        3  0 Const
     d   qmName                      40    Const
     d   qmFsn                       15    Const
     d   qmTitl                      15    Const
     d   qmAdr1                      40    Const
     d   qmCty                       25    Const
     d   qmSt                         2    Const
     d   qmZipInt                     9  0 Const
     d   qmAreaInt                    3  0 Const
     d   qmPhn#Int                    7  0 Const
     d   qmShNm                      10    Const
     d   qm1099                       1    Const
     d   qmType                       1    Const
     d   qmSpCd                       3    Const
     d   qmLic#                      13    Const
     d   qmVFlg                       1    Const
     d   qmAdr2                      40    Const
     d   qmDBA1                       1    Const
     d   qmFxAr                       3  0 Const
     d   qmFax#                       7  0 Const
     d   qmWcce                       1    Const
     d   qmStat                       1    Const
     d   qmWcDt                       7  0 Const
     d   qmBWC#                      11    Const
     d   qmBWCT                       3  0 Const
     d   qmNameP1                    40    Const
     d   qmFsnP1                     15    Const
     d   qmAdr1P1                    40    Const
     d   qmAdr2P1                    40    Const
     d   qmCtyP1                     25    Const
     d   qmStP1                       2    Const
     d   qmZipP1                      9  0 Const
     d   qmAreaP1                     3  0 Const
     d   qmPhn#P1                     7  0 Const

     d* Add Leading Zeros to Date
     d CvtDates        pr             7  0
     d  dateIn                        8  0

     d* Check for Provider Information
     d ChkProvInfo     pr

     d* Edit Claim Master
     d EdtClmMaster    pr

     d* Edit Payments
     d EdtPayments     pr

     d* Edit 3rd Party Recoveries
     d EdtRecovery1    pr

     d* Edit Excess Recoveries
     d EdtRecovery2    pr

     d* Edit Reserve History
     d EdtRsvHistory   pr

     d* Get claim #
     d GetClaim#       pr             7  0
     d   group#                       3  0

     d* Get the division
     d GetDivision     pr             5  0
     d   group#                       3  0
     d   employer#                    9  0
     d   jurState                     2

     d* Get Employer #
     d GetEmployer     pr             9  0
     d   reportUnit                  10

     d* Get Next Sequence #
     d GetNextSeq#     pr             5  0
     d   company#                     3  0
     d   group#                       3  0
     d   year                         3  0
     d   case                         7  0

     d* Get PV1 Codes
     d GetPV1Code      pr             2
     d   paycode                      3
     d   type                         1    Const

     d* Lookup and retrieve Table Values
     d GetTableCodes   pr             2
     d   codeType                     5    Const
     d   code                        10

     d* Process 3rd Pary Recovery File
     d Prc3rdPartyRec  pr

     d* Process Claim File
     d PrcClmMaster    pr

     d* Process Excess Recovery File
     d PrcExcessRec    pr

     d* Process Payment File
     d PrcPmtHistory   pr

     d* Process Exposure/Reserve File
     d PrcRsvHistory   pr

     d* Pre fix Data
     d PreFixData      pr

     d* Program End
     d ProgramEnd      pr

     d* Write Claim Average Weekly Wage
     d WrtClmAWW       pr

     d* Write Claim Demographic
     d WrtClmDemo      pr

     d* Write Claim History
     d WrtClmHistory   pr

     d* Write Claim Legal
     d WrtClmLegal     pr

     d* Write Claim Master
     d WrtClmMaster    pr

     d* Write Claim X-ref
     d WrtClmXref      pr

     d* Write Payments
     d WrtPayments     pr

     d* Write Provider Information
     d WrtProvInfo     pr

     d* Write Provider X-Ref
     d WrtProvXref     pr

     d* Write 3rd Party Recoveries
     d WrtRecovery1    pr

     d* Write Excess Recoveries
     d WrtRecovery2    pr

     d* Write Reserve History
     d WrtRsvHistory   pr

     d* Write WCMC Claim Demographic
     d WrtWcmcClmDemo  pr
     d  returnCode                    1

     d* Write WCMC Claim History
     d WrtWcmcClmHist  pr
     d  returnCode                    1

     d* Write WCMC Claim Master
     d WrtWcmcClmMast  pr
     d  returnCode                    1

     d* Write WCMC Provider Information
     d WrtWcmcProv     pr
     d  returnCode                    1

      /copy wPrClmRsv
      /copy wPrEmplr
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
     D  dsDttm                       20  0
     D  dsDate                        8  0 Overlay(dsDttm)
     D  dsTime                        6  0 Overlay(dsDttm:*Next)

     D  diagnosisDS    DS
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

       // FA005RC parameters
     d   qmMode        s              1
     d   qmRRNpd       s              9  0
     d   qmRRNp1       s              9  0
     d   qmFein        s              9  0
     d   qmMod         s              3  0
     d   qmAreaInt     s              3  0
     d   qmPhn#Int     s              7  0
     d   qmName        s             40
     d   qmTitl        s             15
     d   qmZipInt      s              9  0
     d   qmShNm        s             10
     d   qm1099        s              1
     d   qmType        s              1
     d   qmSpCd        s              3
     d   qmLic#        s             13
     d   qmVFlg        s              1
     d   qmDBA1        s              1
     d   qmFxAr        s              3  0
     d   qmFax#        s              7  0
     d   qmWcce        s              1
     d   qmStat        s              1
     d   qmWcDt        s              7  0
     d   qmBWC#        s             11
     d   qmBWCT        s              3  0
     d   qmNameP1      s             40
     d   qmFsnP1       s             15
     d   qmAdr1P1      s             40
     d   qmAdr2P1      s             40
     d   qmCtyP1       s             25
     d   qmStP1        s              2
     d   qmZipP1       s              9  0
     d   qmAreaP1      s              3  0
     d   qmPhn#P1      s              7  0

       // Initial Values
     d   company#      s              3  0 Inz(45)
     d   group#        s              3  0 Inz(5)

       // Work Fields
     d hhFein          s                   Like(PDfein)
     d hhMod           s                   Like(PDmod)
     d prvCnt          s              7  0 Inz(0)
     d misCnt          s              7  0 Inz(0)
     d dn              s              2  0
     d e               s              5  0
     d n               s              5  0
     D i               s              3  0
     d outTitle        s             44
     d tableKey        s             10
     d npcDsc          s             10
     D erCnt           s              7  0
     D erDta           s             30
     D inCnt           s              7  0
     D kkFein          s                   Like(PDfein)
     D kkMod           s                   Like(PDmod)
     D kkSS#           s                   Like(CDss#)
     D ss#             s                   Like(CDss#)
     D pdsDsc          s             30
     D accidentDate    s                   Like(CMacDt)
     d chrSSN          s              9
     d isClaimant      s               n
     D addClaims       s              1
     D countCvt        s              5  0
     D countErr        s              5  0
     D countErr2       s              5  0
     D countProv       s              5  0
     D companyName     s             40
     D claimName       s             40
     D fieldValue      s             40
     D provName        s             40
     D titleConvert    s             40
     D titleError1     s             40
     D titleError2     s             40
     D txt             s             40
     D wcClaim#        s             40
     D titleProvider   s             40
    D*==============================================================+
    D* Constants
    D*===============================================================
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
    D*
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SSZERO          C                   CONST('0        ')
     D SSCAP           C                   CONST('SS# Assign')
     D SSTITL          C                   CONST('SS Numbers Assigned')
     D BPEXPL          C                   CONST('Reserves increased,')
     D NOFID           C                   CONST('999999999')
    D*
    D* Report Title lines
    D*
     D TITL1           C                   CONST('CSI - Client 114  -
     D                                      Project 114WXN   ')
     D TITL2           C                   CONST('SJ Trans Auth WC Cla-
     D                                     im Conversion   ')
     D TITLP           C                   CONST(' *** PROVIDERS ADDED-
     D                                      TO FILES ***  ')
     D TITL3           C                   CONST('* MISSING PROVIDER N-
     D                                     AME & ADDRESS *')
     D TITL4           C                   CONST('* Employer Translati-
     D                                     on FAULT LIST *')
     D natTitle        C                   CONST('*** NATURE PART and-
     D                                      CAUSE Translations-
     D                                     ***')
     D SSTTL           C                   CONST('*** Social Security -
     D                                     Numbers Assigned  -
     D                                     ***')
     D ZIPAM           C                   CONST('         0')
     D RHEXPO          C                   CONST('Original Claim Rese-
     D                                     rves')
     D RHEXPC          C                   CONST('Reserve Change as of ')
    C*=========================================================================
    C*      E N T R Y  P A R A M E T E R S
    C*=========================================================================
     C     *entry        plist
     C                   parm                    addClaims
    C*=========================================================================
    C*      M  A I  N    L  O  G  I  C
    C*=========================================================================
      /free


       // Read thru the claim file
       Read pgbc00;
       DoW Not %Eof(wagbcl);

          // Process Claim Master and Other Files
          PrcClmMaster();

          If GBCdataTp = 'DS';
             // Process Reserve History
             PrcRsvHistory();

             // Process Payment History
             PrcPmtHistory();

             // Process 3rd Pary Recoveries
             Prc3rdPartyRec();

             // Process Excess Recoveries
             PrcExcessRec();

          EndIf;

          GBCprFl = '1';
          Update pgbc00;
          Read pgbc00;
       EndDo;

       // End of Program Processes
       ProgramEnd();

       *inLR = *On;

       //------------------------------------------------------------
       // Initialization Routine
       //------------------------------------------------------------
       Begsr *InzSR;

          // Initialize number of input date fields . . .
          dsDttm = %Dec(%Timestamp());

       EndSR;
       //------------------------------------------------------------

      /End-Free
     O*===================================================================
     O*  Claims Uploaded
     O*===================================================================
     Oqsysprt   E            HdrCvt         1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         50
     O                       dsDate              98 '  /  /    '
     O          E            HdrCvt         2
     O                       Q1pgm               10
     O                       titleConvert        70
     O                       Q1user             116
     O                       Page          Z    128
     O                                          123 'Page'
     O          E            HdrCvtDtl      2
    O*
     O                                           22 'GB Claim #        WC C'
     O                                           44 'laim#        Claimant '
     O                                           66 'Name                  '
     O                                           88 '                      '
    O*
     O          E            DtlCvt         1
     O                       GBCclm#             16
     O                       wcClaim#            40
     O                       claimName           83
    O*
     O          E            DtlTotCvt      1
     O                       countCvt             9
     O*===================================================================
     O*  Claims that Weren't Uploaded
     O*===================================================================
     Oqsysprt1  E            HdrErr1        1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         50
     O                       dsDate              98 '  /  /    '
     O          E            HdrErr1        2
     O                       Q1pgm               10
     O                       titleError1         70
     O                       Q1user             116
     O                       Page          Z    128
     O                                          123 'Page'
     O          E            HdrDtlErr1     2
    O*
     O                                           22 'GB Claim #        Reas'
     O                                           44 'on                    '
     O                                           66 '                      '
    O*
     O          E            DtlErr1        1
     O                       GBCclm#             16
     O*                      err(e)              67
    O*
     O          E            DtlTotErr1     1
     O                       countErr             9
     O          E            ErrorSum    2  2
     O                                           22 '** Error Summary **   '
    O*
     O          E            ErrorSum1      2
     O*                      erc(i)        Z     10
     O*                      txt(i)            +  3
     O*===================================================================
     O*  Claims with Errors.
     O*===================================================================
     Oqsysprt2  E            HdrErr2        1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         50
     O                       dsDate              98 '  /  /    '
     O          E            HdrErr2        2
     O                       Q1pgm               10
     O                       TitleError2         70
     O                       Q1user             116
     O                       Page          Z    128
     O                                          123 'Page'
     O          E            HdrDtlErr2     2
    O*
     O                                           22 'GB Claim #        WC C'
     O                                           44 'laim#        Reason   '
     O                                           66 '                  Inco'
     O                                           88 'ming Data             '
    O*
     O          E            DtlErr2        1
     O                       GBCclm#             16
     O                       wcClaim#            40
     O*                      err(e)              60
     O                       fieldValue          82
    O*
     O          E            DtlTotErr2     1
     O                       countErr2            9
     O          E            ErrorSum2   2  2
     O                                           22 '** Error Summary **   '
    O*
     O          E            ErrorSum2      2
     O*                      erc(i)        Z     10
     O*                      txt(i)            +  3
     O*===================================================================
     O*  Providers that were added
     O*===================================================================
     Oqsysprt3  E            HdrProv        1 06
     O                       dsTime               8 '  :  :  '
     O                       companyName         50
     O                       dsDate              98 '  /  /    '
     O          E            HdrProv        2
     O                       Q1pgm               10
     O                       titleProvider       70
     O                       Q1user             116
     O                       Page          Z    128
     O                                          123 'Page'
     O          E            HdrDtlProv     2
    O*
     O                                           22 'GB Claim #            '
     O                                           44 ' Fein  Mod   Name     '
     O                                           66 '                      '
     O                                           66 '                      '
    O*
     O          E            DtlProv        1
     O                       GBCclm#             16
     O                       PDfein              27
     O                       PDmod               32
     O                       provName            82
    O*
     O          E            DtlTotProv     1
     O                       countProv            9
       //===================================================================
       // ChkProvInfo - Check for Provider Information or Create It
       //===================================================================
     P ChkProvInfo     B
     D ChkProvInfo     PI
      /free

       isClaimant = *Off;

       // Check Fein
       Select;
       When %Subst(GBPtaxID:3:1) = '-';
          kkFein = %Int(%Subst(GBPtaxID:1:2) + %Subst(GBPtaxID:4:7));
       When %Subst(GBPtaxID:4:1) = '-';
          kkFein = 0;
          isClaimant = *On;
       When GBPtaxID = ' ' Or GBPtaxID = '000000000';
          isClaimant = *On;
       Other;
          If CMss# = GBPtaxID Or GBPtaxID = ' ';
             isClaimant = *On;
          EndIf;
       EndSl;

       If Not isClaimant;
          // First, look in the cross-reference.
          Chain (GBPtaxID:GBPpayNam) ppx00;
          If %Found(WAPXL);
             Chain (PXfein:PXmod) ppd00;
          Else;
             // Get next avail MOD in WMPDL.
             SetGt (kkFein:*Hival) ppd00;
             ReadPE (kkFein) ppd00;
             If Not %Eof(WMPDL);
                PDfein = kkFein;
                PDmod += 1;
             Else;
                PDmod = 0;
                PDfein = kkFein;
             EndIf;

             // Add Provider
             WrtProvInfo();

             // Create Provider X-Ref
             WrtProvXref();

          EndIf;
       EndIf;

      /End-Free
     P ChkProvInfo     E
       //===================================================================
       // CvtDates - Convert Incoming Dates.
       //===================================================================
     P CvtDates        B
     D CvtDates        PI             7  0
     d   dateIn                       8  0

     d  intDate        s              7  0
      /free

       If dateIn <> 0;
          Monitor;
             intDate = %Dec(%Date(dateIn:*iso):*cymd);
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
       // EdtClmMaster - Edit Claim Master
       //===================================================================
     P EdtClmMaster    B
     D EdtClmMaster    PI

     D claim#          s              7  0
      /Free


      /End-Free
     P EdtClmMaster    E
       //===================================================================
       // EdtPayments  - Edit Payments
       //===================================================================
     P EdtPayments     B
     D EdtPayments     PI

     D claim#          s              7  0
      /Free


      /End-Free
     P EdtPayments     E
       //===================================================================
       // EdtRecovery1 - Edit Recovery 1
       //===================================================================
     P EdtRecovery1    B
     D EdtRecovery1    PI

     D claim#          s              7  0
      /Free


      /End-Free
     P EdtRecovery1    E
       //===================================================================
       // EdtRecovery2 - Edit Recovery 2
       //===================================================================
     P EdtRecovery2    B
     D EdtRecovery2    PI

     D claim#          s              7  0
      /Free


      /End-Free
     P EdtRecovery2    E
       //===================================================================
       // EdtRsvHistory - Edit Reserve Histroy
       //===================================================================
     P EdtRsvHistory   B
     D EdtRsvHistory   PI

     D claim#          s              7  0
      /Free


      /End-Free
     P EdtRsvHistory   E
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
       // GetDivision - Get Division
       //===================================================================
     P GetDivision     B
     D GetDivision     PI             5  0
     D   group#                       3  0
     D   employer#                    9  0
     d   jurState                     2

     D   division      s              5  0 Inz(0)
      /Free

         Chain (company#:group#:employer#) pem00;
         If %Found(WMEML);
            DoU %Eof(WMEML);
               If jurState = EMpSt;
                  division = EMdiv;
                  Leave;
               EndIf;
               ReadE (company#:group#:employer#) pem00;
            EndDo;
         EndIf;

       Return division;

      /End-Free
     P GetDivision     E
       //===================================================================
       // GetEmployer - Get Employer #
       //===================================================================
     P GetEmployer     B
     D GetEmployer     PI             9  0
     D   rptUnit                     10

     D   employer#     s              9  0 Inz(0)
      /Free

         Chain (rptUnit) pex00;
         If %Found(WAEXL);
            employer# = EXemp#;
         EndIf;

       Return employer#;

      /End-Free
     P GetEmployer     E
       //===================================================================
       // GetNextSeq# - Get Next Sequence #
       //===================================================================
     P GetNextSeq#     B
     D GetNextSeq#     PI             5  0
     D   company#                     3  0
     D   group#                       3  0
     D   year                         3  0
     D   case                         7  0

     D   sequence#     s              5  0 Inz(1)
      /Free

         SetGT  (company#:group#:year:case) ppa00;
         ReadPE (company#:group#:year:case) ppa00;
         If %Found(WDPAL);
            sequence# = P1seq# + 1;
         EndIf;

       Return sequence#;

      /End-Free
     P GetNextSeq#     E
       //===================================================================
       // GetPV1Code - Get Payment Value 1 codes
       //===================================================================
     P GetPV1Code      B
     D GetPV1Code      PI             2
     D   paymentCode                  3
     D   payType                      1    Const

     D   payCode       s              2    Inz('MC')
      /Free

         Select;
         When payType = 'C';
            Select;
            When paymentCode = '001';
               payCode = 'TT';
            When paymentCode = '003';
               payCode = 'PT';
            When paymentCode = '004';
               payCode = 'PP';
            When paymentCode = '006';
               payCode = 'IC';
            When paymentCode = '009';
               payCode = 'WC';
            When paymentCode = '024';
               payCode = 'IC';
            When paymentCode = '079';
               payCode = 'IB';
            EndSl;
         When payType = 'M';
            Select;
            When paymentCode = '001';
               payCode = 'M1';
            When paymentCode = '002';
               payCode = 'M6';
            When paymentCode = '003';
               payCode = 'MH';
            When paymentCode = '004';
               payCode = 'MA';
            When paymentCode = '005';
               payCode = 'AC';
            When paymentCode = '007';
               payCode = 'MK';
            When paymentCode = '011';
               payCode = 'M2';
            When paymentCode = '013';
               payCode = 'ER';
            When paymentCode = '032';
               payCode = 'HP';
            When paymentCode = '056';
               payCode = 'MF';
            EndSl;
         EndSl;

       Return payCode;

      /End-Free
     P GetPV1Code      E
       //===================================================================
       // GetTableCodes - Retieve Table Values
       //===================================================================
     P GetTableCodes   B
     D GetTableCodes   PI             2
     D   codeType                     5    Const
     D   code                        10

     d   tableCode     s              2
      /Free

       tableCode = 'XX';

       If code <> ' ';
          Chain (codeType:code) pxc00;
          If %Found(waxcl);
             tableCode = XCcodeWC;
          EndIf;
       EndIf;

       Return tableCode;

      /End-Free
     P GetTableCodes   E
       //===================================================================
       // Prc3rdPartyRec - Process 3rd Party Recoveries
       //===================================================================
     P Prc3rdPartyRec  B
     D Prc3rdPartyRec  PI

     d   sequence#     s              5  0 Inz(1)
     d   chrCode       s             10
     d   dateIn        s             23
      /Free

       // Read thru the payment file
       ReadE (GBCclm#) pgb300;
       If %Found(wagb3l);
          DoU %Eof(wagb3l);

             If GB3prFl = ' ';
                EdtRecovery1();
                WrtRecovery1();
             EndIf;

             GB3prFl = 'Y';
             Update pgb300;
             ReadE (GBCclm#) pgb300;
          EndDo;
       EndIf;

      /End-Free
     P Prc3rdPartyRec  E
       //===================================================================
       // PrcClmMaster - Process Claim Master  (WMCMP/FMCMP)
       //===================================================================
     P PrcClmMaster    B
     D PrcClmMaster    PI

     D chrRtnCode      s              1    Inz(' ')
     D chrCode         s             10
      /Free

          If addClaims = 'Y' Or (addClaims <> 'Y' And GBCdataTp = 'DC');

             // Write/Update Claim Master
             If GBCprfl = ' ';
                EdtClmMaster();
                WrtClmMaster();
             EndIf;

          EndIf;
    
      /End-Free
     P PrcClmMaster    E
       //===================================================================
       // PrcExcessRec - Process Excess Recoveries
       //===================================================================
     P PrcExcessRec    B
     D PrcExcessRec    PI

     d   sequence#     s              5  0 Inz(1)
     d   chrCode       s             10
     d   dateIn        s             23
      /Free

       // Read thru the payment file
       ReadE (GBCclm#) pgbe00;
       If %Found(wagbel);
          DoU %Eof(wagbel);

             If GBEprfl = ' ';
                EdtRecovery2();
                WrtRecovery2();
             EndIf;

             GBEprFl = 'Y';
             Update pgbe00;
             ReadE (GBCclm#) pgbe00;
          EndDo;
       EndIf;

      /End-Free
     P PrcExcessRec    E
       //===================================================================
       // PrcPmtHistory - Write Payment History (WDPAP/WDRPP)
       //===================================================================
     P PrcPmtHistory   B
     D PrcPmtHistory   PI

     d   sequence#     s              5  0 Inz(1)
     d   chrCode       s             10
     d   dateIn        s             23
      /Free

       // Read thru the payment file
       Chain (GBCclm#) pgbp00;
       If %Found(wagbpl);
          DoU %Eof(wagbpl);

             If GBPprFl = ' ';
                EdtPayments();
                WrtPayments();
             EndIf;

             GBPprFl = 'Y';
             Update pgbp00;
             ReadE (GBCclm#) pgbp00;
          EndDo;
       EndIf;

      /End-Free
     P PrcPmtHistory   E
       //===================================================================
       // PrcRsvHistory - Process Reserve History (WMRHP)
       //===================================================================
     P PrcRsvHistory   B
     D PrcRsvHistory   PI

      /Free

       // Read thru the exposure file

       ReadE (GBCclm#) pgbr00;
       If %Found(wagbrl);
          DoU %Eof(wagbrl);
             If GBPprFl = ' ';
                EdtRsvHistory();
                WrtRsvHistory();

                GBPprFl = 'Y';
                Update pgbr00;
             EndIf;
             ReadE (GBCclm#) pgbr00;
          EndDo;
       EndIf;

      /End-Free
     P PrcRsvHistory   E
       //===================================================================
       // PreFixdata - Preparation/rearrangement of input data
       //===================================================================
     P PreFixData      B
     D PreFixData      PI
      /Free
    
      /End-Free
     P PreFixData      E
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

       // Shutdown WCMC Provider Information
       WrtWcmcProv(chrRtnCode);

      /End-Free
     P ProgramEnd      E
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
     D pos             s              3  0
     D chrCode         s             10
     D testChar        s              1
     D testChar2       s              1
     D demoFound       s               n   Inz(*Off)
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

          CDlsn = GBCclmLsn;
          CDfsn = GBCclmFsn;
          CDmid = ' ';

          Select;
          When GBCclmAdr1 <> ' ';
              CDadr1 = GBCclmAdr1;
          When GBCclmAdr2 <> ' ' And GBCclmAdr1 = ' ';
              CDadr1 = GBCclmAdr2;
          When GBCclmAdr3 <> ' ' And GBCclmAdr1 = ' ' And
               GBCclmAdr2 = ' ';
              CDadr1 = GBCclmAdr3;
          EndSl;

          Select;
          When GBCclmAdr2 <> ' ' And GBCclmAdr1 <> ' ';
              CDadr2 = GBCclmAdr2;
          When GBCclmAdr3 <> ' ' And ((GBCclmAdr1 = ' ' And
               GBCclmAdr2 <> ' ') Or (GBCclmAdr1 <> ' ' And
               GBCclmAdr2 = ' '));
              CDadr2 = GBCclmAdr3;
          EndSl;

          CDcty = GBCclmCity;
          CDst = GBCclmSt;

          If %Int(%Trim(GBCclmZip)) > 99999;
             CDzip = %Int(%Trim(GBCclmZip));
          Else;
             CDzip = %Int(%Trim(GBCclmZip)) * 10000;
          EndIf;

          CDdtob = CvtDates(GBCbrthDt);
          CDsex = GBCclmSex;

          Select;
          When GBCmSts = 'M';
             CDmSts = 'M';
          When GBCmSts = 'P';
             CDmSts = 'S';
          When GBCmSts = 'S';
             CDmSts = 'U';
          Other;
             CDmSts = 'K';
          EndSl;

          If GBCjrCtry = '000';
             CDctry = 'USA';
          EndIf;

          CDhicn = ' ';
          CDmdBn = ' ';

          testChar  = %Subst(GBCclmPhn#:4:1);
          testChar2 = %Subst(GBCclmPhn#:5:1);

          Select;
          When GBCclmPhn# = ' ';
             CDarea = 0;
             CDphn# = 0;
          When testChar = ' ' Or testChar = '-';
             CDarea = %Int(%Subst(GBCclmPhn#:1:3));
             CDphn# = %Int(%Subst(GBCclmPhn#:5:3) + %Subst(GBCclmPhn#:9:4));
          When testChar2 = ')';
             CDarea = %Int(%Subst(GBCclmPhn#:2:3));
             CDphn# = %Int(%Subst(GBCclmPhn#:6:3) + %Subst(GBCclmPhn#:10:4));
          Other;
             CDarea = %Int(%Subst(GBCclmPhn#:1:3));
             CDphn# = %Int(%Subst(GBCclmPhn#:4:7));
          EndSl;

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
     D chrCode         s             10
      /Free

       Clear pch0p;

       CHco# = CMco#;
       CHfnd = CMfnd;
       CHfyr = CMfyr;
       CHcase = CMcase;
       CHuser = q1User;
       CHpgm = q1Pgm;
       CHtime = %Dec(%Char(%Time():*hms0):6:0);
       CHdt   = CvtDates(GBCclmStsD);
       CHefDt = CvtDates(GBCclmStsD);

       If GBCclmSts = 'CL' Or GBCclmSts = 'VO';
          CHocSt = 'C';
          CHexpl = 'Claim closed, GB Update on ' +
                   %Char(%Date(GBCclmStsD:*iso):*usa);
       Else;
          CHocSt = 'O';

          If GBCclmSts = 'RE';
             CHexpl = 'Claim Reopened, GB Reopened on ' +
                      %Char(%Date(GBCclmStsD:*iso):*usa);
          Else;
             CHexpl = 'Claim opened, GB Opened on ' +
                      %Char(%Date(GBCclmStsD:*iso):*usa);
          EndIf;
       EndIf;

       Write pch0p;

       // Write WCMC Case History
       WrtWcmcClmHist(chrRtnCode);

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

          LLdtAp = CvtDates(GBCattDsDt);
          LLdefA = 0;

          LLdefM = 0;
          LLdefN = ' ';
          LLdefR = 0;
          LLdefP = 0;
          LLdeDt = CvtDates(GBCdtRep);
          LLplaA = 0;
          LLplaM = 0;
          LLplaN = ' ';
          LLplaR = 0;
          LLplaP = 0;

          LLfrau = '0' + %EditC(GBCfrauInd:'X');

          LLlwDt = 0;
          LLmTst = GBCmthSetN;
          LLstDt = 0;
          LLsAmt = 0;
          LLlitg = GBCinSuit;

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
       // WrtClmMaster - Write Claim Master
       //===================================================================
     P WrtClmMaster    B
     D WrtClmMaster    PI

     D chrRtnCode      s              1    Inz(' ')
     D pos             s              3  0
     D chrCode         s             10
     D claimFound      s               n   Inz(*Off)
     D addMode         s               n   Inz(*Off)
     D claimExists     s               n   Inz(*Off)
     D group#          s              3  0
     D year            s              3  0
     D case            s              7  0
     D ss#             s              9
     D accDt           s              7  0
     D claimCnt        s              5  0
      /Free

       // Find the claim if it exists
       ExSr FindClaim#;

       If claimFound;
          Chain (company#:group#:year:case) pcm01;
          If %Found(WMCML1);
             ExSr MoveToDB;
          EndIf;
       Else;
          If addClaims = 'Y' And GBCdataTp = 'DS';
             addMode = *On;
             ExSr MoveToDB;
          EndIf;
       EndIf;


       //------------------------------------------------------------
       //  Find Claim#
       //------------------------------------------------------------
       BegSr FindClaim#;

          Chain GBCclm# pcx00;
          If %Found(wacxl);
             claimExists = *On;
             group#      = CXfnd;
             year        = CXfyr;
             case        = CXcase;
             claimFound  = *On;
          EndIf;

       EndSr;
       //------------------------------------------------------------
       //  Move claim data to Database
       //------------------------------------------------------------
       BegSr MoveToDB;

          If addMode;
             Clear pcm01;

             CMco#  = company#;
             CMfnd  = group#;
             CMcase = GetClaim#(CMfnd);
             CMemp# = GetEmployer(GBCrptUnt);
             CMdiv  = 0;
             CMss#  = %Subst(GBCclmSS#:1:3) + %Subst(GBCclmSS#:5:2) +
                      %Subst(GBCclmSS#:8:4);

             // Write/Update Claim Demographic Data
             WrtClmDemo();

             CMacDt = CvtDates(GBCacDt);
             CMacTm = GBCacTm * 100;
             CMfyr  = RtvPolYr(CMco#:CMfnd:CMemp#:CMdiv:CMacDt);

             CMclex = 'CCW';
             CMsupr = 'CW1';
             CMcec  = 'CCW';
             CMnrse = '   ';

             // Init Fields
             CMiUsr = q1User;
             CMiPgm = q1pgm;
             CMiTm = %Dec(%Char(%Time():*hms0):6:0);
             CMiDt = %Int(%Char(%Date():*cymd0));
          EndIf;

          // Write/Update Claim History
          If GBCdataTp = 'DS' Or
             (GBCdataTp = 'DC' And CMstat = 'O' And GBCclmSts = 'CL') Or
             (GBCdataTp = 'DC' And CMstat = 'C' And GBCclmSts <> 'CL');
             WrtClmHistory();
          EndIf;

          If GBCclmSts = 'CL' Or GBCclmSts = 'VO';
             CMstat = 'C';
          Else;
             CMstat = 'O';
          Endif;

          CMrt01 = 'M';
          CMrt02 = 'E';
          CMrt03 = 'I';
          CMrt04 = 'L';
          CMrt05 = 'R';

          CMrtw = CvtDates(GBCrtwDt);
          If CMrtw < CMacdt;
             CMrtw = 0;
          EndIf;

          CMrtwS = GBCrtwRest;
          CMrtwQ = ' ';
          CMiTrt = ' ';
          CMacSt = ' ';

          CMdtLw = CvtDates(GBCdtLstW);
          If CMdtLw < CMacdt;
             CMdtLw = 0;
          EndIf;

          CMdod = CvtDates(GBCdeathDt);
          If CMdod <> 0;
             CMdth = 'Y';
          EndIf;

          CMltFl = GBCwcClmTp;
          CMncci = GBCncci;

          CMdbDt = CvtDates(GBCdisDt);
          If CMdbDt = 0 and CMltFl = 'I';
             CMdbDt =CMacDt;
          EndIf;

          CMeAdr = GBCempPrm;

          If CMeAdr = ' ';
             CMeAdr = 'Y';
          EndIf;

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
          Else;
             CMacdr = GBClosAdr;
             CMaCty = GBClosCty;
             CMaSt  = GBCaccSt;
             CMacZ1 = %Int(GBaccZip);
             CMacZ2 = 0;
          EndIf;

          CMacDs = %Subst(GBCaccDsc:1:60);
          CMacD2 = %Subst(GBCaccDsc:61:60);
          CMocDs = %Subst(GBCempODs:1:40);
          CMsubr = GBCsubRec;
          CMiPct = GBCperImp;

          CMlsn = CDlsn;
          CMfsn = CDfsn;
          CMmid = CDmid;

          CMhiDt = CvtDates(GBChireDt);
          CMentD = CvtDates(GBCclmDt);
          CMfsDt = CvtDates(GBCsalEnDt);

          CMdtRp = CvtDates(GBCempNot);
          If CMdtRp = 0;
             CMdtRp = CMentD;
          EndIf;

          CMrcvD = CvtDates(GBCrptDt);
          If CMrcvD = 0;
             CMrcvD = CMdtRp;
          EndIf;

          Select;
          When GBCempSts = 'A';
             CMeSts = 'AD';
          When GBCempSts = 'B';
             CMeSts = 'AP';
          When GBCempSts = 'C';
             CMeSts = 'PW';
          When GBCempSts = 'D';
             CMeSts = 'DS';
          When GBCempSts = 'F';
             CMeSts = 'FT';
          When GBCempSts = 'L';
             CMeSts = 'SL';
          When GBCempSts = 'O';
             CMeSts = 'ZZ';
          When GBCempSts = 'P';
             CMeSts = 'PT';
          When GBCempSts = 'R';
             CMeSts = 'RT';
          When GBCempSts = 'S';
             CMeSts = 'OS';
          When GBCempSts = 'U';
             CMeSts = 'NE';
          When GBCempSts = 'V';
             CMeSts = 'VO';
          EndSl;

          CMrcvD = CMdtRp;

          // Assign Nature, Part and Cause

          chrCode = GBCnatrB;
          CMnatr = GetTableCodes('NATR':chrCode);

          chrCode = GBCpart;
          CMpart = GetTableCodes('PART':chrCode);

          chrCode = GBCaccTyp;
          CMcaus = GetTableCodes('CAUS':chrCode);

          If CMltFl = 'I' and CMdbDt < CMacDt;
             CMdbDt = CMacDt;
          EndIf;

          CMpds# = GBCclm#;
          CMcvt# = GBCclm#;
          CMacl# = GBCscl#;
          CMyrLx = GBClstYr;
          CM#Dep = GBC#Dep;
          CMeID  = GBCempID#;
          CMdept = GBCempDpt;
          CMpoww = GBCpostAWW;
          CMssFF = GBCssOff;
          CMueFF = GBCueOff;
          CMpeFF = GBCpenOff;
          CMotFF = GBCothOff;
          CMsic  = GBCsicCd;
          CMpoi  = GBCfullPay;
          CMfSal = GBCsalCont;
          CMcvFl = GBCeverCtv;

          If GBCdthRcd = 'Y';
             CMdth = 'Y';
          Else;
             CMdth = 'N';
          EndIf;

          If GBCclmClas <> ' ';
             CMclas = %Int(GBCclmClas);
          EndIf;

          CMaww = GBCclmAWW;
          CMcrt = GBCcompRt;

          // Write/Update AWW History
          If CMaww <> 0 And CMltFl = 'I';
             WrtClmAWW();
          EndIf;

          CMotWk = GBCothPmts;

          If GBCdenDt <> 0;
             CMdny = 'Y';
          Else;
             CMdny = 'N';
          EndIf;

          CMdyEf = CvtDates(GBCdenDt);

          CMjrSt = GBCjrSt;
          CMlsCv = GBCcovCd;

          If CMcovT = ' ';
             CMcovt = GBCcovCd;
          EndIf;

          CMmmi  = CvtDates(GBCmaxMed);

          // Write/Update Claim Legal
          WrtClmLegal();

          // Maintenance Fields
          CMuser = q1User;
          CMpgm = q1pgm;
          CMtime = %Dec(%Char(%Time():*hms0):6:0);
          CMdt   = %Int(%Char(%Date():*cymd0));

          If addMode;
             Write pcm01;
          Else;
             Update pcm01;
          EndIf;

          WrtClmXref();

          // Create the WCMC Medical Case File
          WrtWcmcClmMast(chrRtnCode);

       EndSr;
       //------------------------------------------------------------

      /End-Free

     P WrtClmMaster    E
       //===================================================================
       // WrtClmXref - Write Claim X-Ref
       //===================================================================
     P WrtClmXref      B
     D WrtClmXref      PI

      /Free

          Chain GBCclm# pcx00;
          If Not %Found(WACXL);
             Clear pcx00;

             CXclm#  = GBCclm#;
             CXfnd   = CMfnd;
             CXfyr   = CMfyr;
             CXcase  = CXcase;

             Write pcx00;
          EndIf;

      /End-Free

     P WrtClmXref      E
       //===================================================================
       // WrtPayments  - Write Payments
       //===================================================================
     P WrtPayments     B
     D WrtPayments     PI

     D chrRtnCode      s              1    Inz(' ')
     D pos             s              3  0
     D chrCode         s             10
     D compAmt         s             18  2
     D expenseAmt      s             18  2
     D medicalAmt      s             18  2
     D rehabAmt        s             18  2
      /Free

       Chain (GBPclm#) pcx00;
       If %Found(wacxl);

          Clear ppa0p;

          ChkProvInfo();

          PAco# = company#;
          PAfnd = CXfnd;
          PAfyr = CXfyr;
          PAcase = CXcase;

          Chain(n) (PAco#:PAfnd:PAfYr:PAcase) pcm01;

          PAseq# = GetNextSeq#(company#:PAfnd:PAfYr:PAcase);

          PAtrDt = Cvtdates(GBPtrDt);
          PAfrDt = CvtDates(GBPsrvStDt);
          PAtoDt = CvtDates(GBPsrvEnDt);
          PApoDt = Cvtdates(GBPpayDt);
          PArcvD = Cvtdates(GBPpayEntD);
          PArqDt = CvtDates(GBPpayEntD);

          medicalAmt = %Dec(GBPpayAmt2:18:2)/100;
          compAmt    = %Dec(GBPpayAmt1:18:2)/100;
          expenseAmt = %Dec(GBPpayAmt6:18:2)/100;
          rehabAmt   = %Dec(GBPpayAmt3:18:2)/100;

          Select;
          When medicalAmt <> 0;
             PAamt  = medicalAmt;
             PAbr2  = GetPV1Code(GBPpayCd2:'M');
             PAreCd = 'M';
             PAreTp = 'M';
             If medicalAmt > 0;
                PAtrCd = 'MP';
             Else;
                PAtrCd = 'MA';
             EndIf;
          When expenseAmt <> 0;
             PAamt  = expenseAmt;
             PAbr2  = 'EX';
             PAreCd = 'E';
             PAreTp = 'E';
             If expenseAmt > 0;
                PAtrCd = 'EP';
             Else;
                PAtrCd = 'EA';
             EndIf;
          When compAmt <> 0;
             PAamt  = compAmt;
             PAbr2  = GetPV1Code(GBPpayCd1:'C');
             PAreCd = 'I';
             PAreTp = 'C';
             If compAmt > 0;
                PAtrCd = 'CP';
             Else;
                PAtrCd = 'CA';
             EndIf;
          When rehabAmt <> 0;
             PAamt  = rehabAmt;
             PAbr2  = 'RE';
             PAreCd = 'R';
             PAreTp = 'M';
             If compAmt > 0;
                PAtrCd = 'RP';
             Else;
                PAtrCd = 'RA';
             EndIf;
          EndSl;

          If PAamt > 0;
             PAtype = 'P';
          Else;
             PAtype = 'A';
          Endif;

          PAckTp = ' ';
          PAoff# = CMoff#;

          PAdesc = %Subst(GBPpayMsg:1:30);

          If GBPtaxID = ' ';
             PAfein = 0;
             PAmod  = 0;
          Else;
             PAfein = PXfein;
             PAmod = PXmod;
          EndIf;

          If CMltFl = 'I';
             PAbr7 = 'LT';
          Else;
             PAbr7 = 'NL';
          EndIf;

          PAchk# = 0;

          PAstat = ' ';
          PAfnal = 'N';
          PArqst = q1User;

          // Maintenance Fields
          PAuser = q1User;
          PApgm = q1pgm;
          PAtime = %Dec(%Char(%Time():*hms0):6:0);
          PAdt   = %Int(%Char(%Date():*cymd0));

          // Init Fields
          PAiUsr = q1User;
          PAiPgm = q1pgm;
          PAiTim = %Dec(%Char(%Time():*hms0):6:0);
          PAiDt  = CvtDates(GBPpayEntD);

          Write ppa0p;

          Clear prp03;

          RPco# = PAco#;
          RPfnd = PAfnd;
          RPfyr = PAfyr;
          RPcase = PAcase;
          RPchk# = PAchk#;
          RPtrDt = PAtrDt;
          RPrmDs = 'Check#: ' + %Trim(GBPChk#);

          If PAfein = 0;
             Chain CMss# pcd00;
             RPpNam = %Trim(CDfsn) +  ' ' + %Trim(CDlsn);
             RPadr1 = CDadr1;
             RPadr2 = CDadr2;
             RPcty  = CDcty;
             RPst   = CDst;
             RPzip  = CDzip;
          Else;
             Chain (PXfein:PXmod) ppd00;
             RPpNam = PDname;
             RPadr1 = PDadr1;
             RPadr2 = PDadr2;
             RPcty  = PDcty;
             RPst   = PDst;
             RPzip  = PDzip;
          EndIf;

          RPseq# = PAseq#;

          // Maintenance Fields
          RPuser = q1User;
          RPpgm = q1pgm;
          RPtime = %Dec(%Char(%Time():*hms0):6:0);
          RPdt   = %Int(%Char(%Date():*cymd0));

          Write prp03;

          // Update the claim
          Chain (PAco#:PAfnd:PAfYr:PAcase) pcm01;
          If %Found(wmcml1);
             Select;
             When PAreCd = 'M';
                CMrr01 = CMrr01 - PAamt;
                CMar01 = CMar01 + PAamt;
             When PAreCd = 'E';
                CMrr02 = CMrr02 - PAamt;
                CMar02 = CMar02 + PAamt;
             When PAreCd = 'I';
                CMrr03 = CMrr03 - PAamt;
                CMar03 = CMar03 + PAamt;
             When PAreCd = 'L';
                CMrr04 = CMrr04 - PAamt;
                CMar04 = CMar04 + PAamt;
             When PAreCd = 'R';
                CMrr05 = CMrr05 - PAamt;
                CMar05 = CMar05 + PAamt;
             EndSl;
             Update pcm01;
          EndIf;
       EndIf;

      /End-Free
     P WrtPayments     E
       //===================================================================
       // WrtProvInfo - Write Provider Info (WMPDP)
       //===================================================================
     P WrtProvInfo     B
     D WrtProvInfo     PI

     D chrRtnCode      s              1    Inz(' ')
      /Free

          Clear ppd0p;

          PDfein = kkFein;
          PDmod  = kkMod;

          PDname = %Subst(GBPpayNam:1:40);

          Select;
          When GBPpayAdr1 <> ' ';
              PDadr1 = GBPpayAdr1;
          When GBPpayAdr2 <> ' ' And GBPpayAdr1 = ' ';
              PDadr1 = GBPpayAdr2;
          When GBPpayAdr3 <> ' ' And GBPpayAdr1 = ' ' And
               GBPpayAdr2 = ' ';
              PDadr1 = GBPpayAdr3;
          EndSl;

          Select;
          When GBPpayAdr2 <> ' ' And GBPpayAdr1 <> ' ';
              PDadr2 = GBPpayAdr2;
          When GBPpayAdr3 <> ' ' And ((GBPpayAdr1 = ' ' And
               GBPpayAdr2 <> ' ') Or (GBPpayAdr1 <> ' ' And
               GBPpayAdr2 = ' '));
              PDadr2 = GBPpayAdr3;
          EndSl;

          PDtype = '1';
          PD1099 = 'N';
          PDstat = 'A';
          PDflic = q1User;

          // Maintenance Fields
          PDuser = q1User;
          PDpgm = q1pgm;
          PDtime = %Dec(%Char(%Time():*hms0):6:0);
          PDdt   = %Int(%Char(%Date():*cymd0));

          // Init Fields
          PDiUsr = q1User;
          PDiPgm = q1pgm;
          PDiTim = %Dec(%Char(%Time():*hms0):6:0);
          PDiDt  = %Int(%Char(%Date():*cymd0));

          Write ppd0p;
    
          WrtWcmcProv(chrRtnCode);

      /End-Free

     P WrtProvInfo     E
       //===================================================================
       // WrtProvXref - Write Provider X-Ref (WAPXP)
       //===================================================================
     P WrtProvXref     B
     D WrtProvXref     PI

      /Free

          Clear ppx00;

          PXtaxID  = GBPtaxID;
          PXpayNam = GBPpayNam;
          PXfein   = PDfein;
          PXmod    = PDmod;

          Write ppx00;

      /End-Free

     P WrtProvXref     E
       //===================================================================
       // WrtRecovery1 - Write 3rd Party Recoveries
       //===================================================================
     P WrtRecovery1    B
     D WrtRecovery1    PI

     D chrRtnCode      s              1    Inz(' ')
     D pos             s              3  0
     D chrCode         s             10
     D compAmt         s             18  2
     D expenseAmt      s             18  2
     D medicalAmt      s             18  2
     D rehabAmt        s             18  2
      /Free

       Chain (GB3clm#) pcx00;
       If %Found(wacxl);

          Clear ppa0p;

          ChkProvInfo();

          PAco# = company#;
          PAfnd = CXfnd;
          PAfyr = CXfyr;
          PAcase = CXcase;

          Chain(n) (PAco#:PAfnd:PAfYr:PAcase) pcm01;

          PAseq# = GetNextSeq#(company#:PAfnd:PAfYr:PAcase);

          PAtrDt = Cvtdates(GB3trDt);
          PAfrDt = CvtDates(GB3recDt);
          PAtoDt = CvtDates(GB3recDt);
          PApoDt = Cvtdates(GB3recDt);
          PArcvD = Cvtdates(GB3recDt);
          PArqDt = CvtDates(GB3recDt);

          medicalAmt = %Dec(GB3recAmt2:18:2)/100;
          compAmt    = %Dec(GB3recAmt1:18:2)/100;
          expenseAmt = %Dec(GB3recAmt6:18:2)/100;
          rehabAmt   = %Dec(GB3recAmt3:18:2)/100;

          Select;
          When medicalAmt <> 0;
             PAamt  = medicalAmt;
             PAreCd = 'M';
             PAreTp = 'M';
             PAtrCd = 'MR';
          When expenseAmt <> 0;
             PAamt  = expenseAmt;
             PAreCd = 'E';
             PAreTp = 'E';
             PAtrCd = 'ER';
          When compAmt <> 0;
             PAamt  = compAmt;
             PAreCd = 'I';
             PAreTp = 'C';
             PAtrCd = 'CR';
          When rehabAmt <> 0;
             PAamt  = rehabAmt;
             PAreCd = 'R';
             PAreTp = 'M';
             PAtrCd = 'RR';
          EndSl;

          PAtype = 'R';

          PAckTp = ' ';
          PAoff# = CMoff#;
          PAbr2  = 'TH';

          PAdesc = %Subst(GB3recMsg:1:30);

          PAfein = 0;
          PAmod  = 0;

          If CMltFl = 'I';
             PAbr7 = 'LT';
          Else;
             PAbr7 = 'NL';
          EndIf;

          PAchk# = 0;

          PAstat = ' ';
          PAfnal = 'N';
          PArqst = q1User;

          // Maintenance Fields
          PAuser = q1User;
          PApgm = q1pgm;
          PAtime = %Dec(%Char(%Time():*hms0):6:0);
          PAdt   = %Int(%Char(%Date():*cymd0));

          // Init Fields
          PAiUsr = q1User;
          PAiPgm = q1pgm;
          PAiTim = %Dec(%Char(%Time():*hms0):6:0);
          PAiDt  = CvtDates(GB3recDt);

          Write ppa0p;

          Clear prp03;

          RPco# = PAco#;
          RPfnd = PAfnd;
          RPfyr = PAfyr;
          RPcase = PAcase;
          RPchk# = PAchk#;
          RPtrDt = PAtrDt;
          RPrmDs = ' ';

          Chain CMss# pcd00;
          RPpNam = %Trim(CDfsn) +  ' ' + %Trim(CDlsn);
          RPadr1 = CDadr1;
          RPadr2 = CDadr2;
          RPcty  = CDcty;
          RPst   = CDst;
          RPzip  = CDzip;

          RPseq# = PAseq#;

          // Maintenance Fields
          RPuser = q1User;
          RPpgm = q1pgm;
          RPtime = %Dec(%Char(%Time():*hms0):6:0);
          RPdt   = %Int(%Char(%Date():*cymd0));

          Write prp03;

          // Update the claim
          Chain (PAco#:PAfnd:PAfYr:PAcase) pcm01;
          If %Found(wmcml1);
             Select;
             When PAreCd = 'M';
                CMrr01 = CMrr01 + PAamt;
                CMar01 = CMar01 - PAamt;
             When PAreCd = 'E';
                CMrr02 = CMrr02 + PAamt;
                CMar02 = CMar02 - PAamt;
             When PAreCd = 'I';
                CMrr03 = CMrr03 + PAamt;
                CMar03 = CMar03 - PAamt;
             When PAreCd = 'L';
                CMrr04 = CMrr04 + PAamt;
                CMar04 = CMar04 - PAamt;
             When PAreCd = 'R';
                CMrr05 = CMrr05 + PAamt;
                CMar05 = CMar05 - PAamt;
             EndSl;
             Update pcm01;
          EndIf;

       EndIf;

      /End-Free
     P WrtRecovery1    E
       //===================================================================
       // WrtRecovery2 - Write Excess Recoveries
       //===================================================================
     P WrtRecovery2    B
     D WrtRecovery2    PI

     D chrRtnCode      s              1    Inz(' ')
     D pos             s              3  0
     D chrCode         s             10
     D compAmt         s             18  2
     D expenseAmt      s             18  2
     D medicalAmt      s             18  2
     D rehabAmt        s             18  2
      /Free

       Chain (GBEclm#) pcx00;
       If %Found(wacxl);

          Clear ppa0p;

          ChkProvInfo();

          PAco# = company#;
          PAfnd = CXfnd;
          PAfyr = CXfyr;
          PAcase = CXcase;

          Chain(n) (PAco#:PAfnd:PAfYr:PAcase) pcm01;

          PAseq# = GetNextSeq#(company#:PAfnd:PAfYr:PAcase);

          PAtrDt = Cvtdates(GBEtrDt);
          PAfrDt = CvtDates(GBErecDt);
          PAtoDt = CvtDates(GBErecDt);
          PApoDt = Cvtdates(GBErecDt);
          PArcvD = Cvtdates(GBErecDt);
          PArqDt = CvtDates(GBErecDt);

          medicalAmt = %Dec(GBErecAmt2:18:2)/100;
          compAmt    = %Dec(GBErecAmt1:18:2)/100;
          expenseAmt = %Dec(GBErecAmt6:18:2)/100;
          rehabAmt   = %Dec(GBErecAmt3:18:2)/100;

          Select;
          When medicalAmt <> 0;
             PAamt  = medicalAmt;
             PAreCd = 'M';
             PAreTp = 'M';
             PAtrCd = 'MR';
          When expenseAmt <> 0;
             PAamt  = expenseAmt;
             PAreCd = 'E';
             PAreTp = 'E';
             PAtrCd = 'ER';
          When compAmt <> 0;
             PAamt  = compAmt;
             PAreCd = 'I';
             PAreTp = 'C';
             PAtrCd = 'CR';
          When rehabAmt <> 0;
             PAamt  = rehabAmt;
             PAreCd = 'R';
             PAreTp = 'M';
             PAtrCd = 'RR';
          EndSl;

          PAtype = 'R';

          PAckTp = ' ';
          PAoff# = CMoff#;
          PAbr2  = 'MC';

          PAdesc = %Subst(GBErecMsg:1:30);

          PAfein = 0;
          PAmod  = 0;

          If CMltFl = 'I';
             PAbr7 = 'LT';
          Else;
             PAbr7 = 'NL';
          EndIf;

          PAchk# = 0;

          PAstat = ' ';
          PAfnal = 'N';
          PArqst = q1User;

          // Maintenance Fields
          PAuser = q1User;
          PApgm = q1pgm;
          PAtime = %Dec(%Char(%Time():*hms0):6:0);
          PAdt   = %Int(%Char(%Date():*cymd0));

          // Init Fields
          PAiUsr = q1User;
          PAiPgm = q1pgm;
          PAiTim = %Dec(%Char(%Time():*hms0):6:0);
          PAiDt  = CvtDates(GBErecDt);

          Write ppa0p;

          Clear prp03;

          RPco# = PAco#;
          RPfnd = PAfnd;
          RPfyr = PAfyr;
          RPcase = PAcase;
          RPchk# = PAchk#;
          RPtrDt = PAtrDt;
          RPrmDs = ' ';

          Chain CMss# pcd00;
          RPpNam = %Trim(CDfsn) +  ' ' + %Trim(CDlsn);
          RPadr1 = CDadr1;
          RPadr2 = CDadr2;
          RPcty  = CDcty;
          RPst   = CDst;
          RPzip  = CDzip;

          RPseq# = PAseq#;

          // Maintenance Fields
          RPuser = q1User;
          RPpgm = q1pgm;
          RPtime = %Dec(%Char(%Time():*hms0):6:0);
          RPdt   = %Int(%Char(%Date():*cymd0));

          Write prp03;

          // Update the claim
          Chain (PAco#:PAfnd:PAfYr:PAcase) pcm01;
          If %Found(wmcml1);
             Select;
             When PAreCd = 'M';
                CMrr01 = CMrr01 + PAamt;
                CMar01 = CMar01 - PAamt;
             When PAreCd = 'E';
                CMrr02 = CMrr02 + PAamt;
                CMar02 = CMar02 - PAamt;
             When PAreCd = 'I';
                CMrr03 = CMrr03 + PAamt;
                CMar03 = CMar03 - PAamt;
             When PAreCd = 'L';
                CMrr04 = CMrr04 + PAamt;
                CMar04 = CMar04 - PAamt;
             When PAreCd = 'R';
                CMrr05 = CMrr05 + PAamt;
                CMar05 = CMar05 - PAamt;
             EndSl;
             Update pcm01;
          EndIf;
       EndIf;

      /End-Free
     P WrtRecovery2    E
       //===================================================================
       // WrtRsvHistory  - Write Reserve History
       //===================================================================
     P WrtRsvHistory   B
     D WrtRsvHistory   PI

     D chrRtnCode      s              1    Inz(' ')
     D pos             s              3  0
     D chrCode         s             10
      /Free

       Chain GBRclm# pcx00;
       If %Found(wacxl);

          Clear prh0p;

          RHco# = company#;
          RHfnd = CXfnd;
          RHfYr = CXfYr;
          RHcase = CXcase;

          RHrt01 = 'M';
          RHrt02 = 'E';
          RHrt03 = 'I';
          RHrt04 = 'L';
          RHrt05 = 'R';

          RHsu01 = %Dec(GBRamtRsv2:18:2)/100;
          RHsu02 = %Dec(GBRamtRsv6:18:2)/100;
          RHsu03 = %Dec(GBRamtRsv1:18:2)/100;
          RHsu05 = %Dec(GBRamtRsv3:18:2)/100;

          RHefDt = CvtDates(GBRtrDt);
          If GBRdataTp = 'DS';
             RHexpl = rhExpO;
          Else;
             RHexpl = rhExpC + %Char(%Date(GBRtrDt:*usa));
          EndIf;

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
          RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);

          Write prh0p;

          // Update the claim
          Chain (RHco#:RHfnd:RHfYr:RHcase) pcm01;
          If %Found(wmcml1);
             CMrr01 = CMrr01 + RHsu01;
             CMrr02 = CMrr02 + RHsu02;
             CMrr03 = CMrr03 + RHsu03;
             CMrr04 = CMrr04 + RHsu04;
             CMrr05 = CMrr05 + RHsu05;
             Update pcm01;
          EndIf;

       EndIf;

      /End-Free
     P WrtRsvHistory   E
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
     d closeCode       s              3    Inz(' ')
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
           CMhrsD:CMdays:CMfeAd:CMdny:CMimp:CMJrSt:CMstat);

      /End-Free
     P WrtWcmcClmMast  E
       //===================================================================
       // WrtWCMCProv - Write Manged Care Provider Informationa
       //===================================================================
     P WrtWcmcProv     B
     D WrtWcmcProv     PI
     D  returnCode                    1

     D  chrRtnCode     s              1    Inz(' ')
     D  intRrnPD       s              9  0 Inz(0)
     D  intRrnP1       s              9  0 Inz(0)
     D  chrMode        s              1
      /Free

         chrMode = returnCode;
         qmRRNpd = 0;
         qmRRNp1 = 0;

         If PDtype = '1' Or PDtype = '4';
            CrtWcmcProv(chrMode:intRrnPD:intRrnP1:chrRtnCode:PDfein:PDmod:
              PDname:PDfsn:PDtitl:PDadr1:PDcty:PDst:PDzip:PDarea:PDphn#:
              PDshNm:PD1099:PDtype:PDspCd:PDlic#:PDvFlg:PDadr2:PDdba1:PDfxAr:
              PDfax#:PDwcce:PDstat:PDwcDt:PDbwc#:PDbwCt:PDname:PDfsn:PDadr1:
              PDadr2:PDcty:PDst:PDzip:PDarea:PDphn#);
         EndIf;

      /End-Free
     P WrtWcmcProv     E
