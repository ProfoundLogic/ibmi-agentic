     H*  (c) Copyright Sapiens, Inc., 2019
     H*****************************************************************
     H/TITLE CVTCLAIMS - Converts Claim
     h dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H* Revisions:
     H*
     H*****************************************************************
     Fwxlgl1    if   e           k Disk
     Fwxcll1    if   e           k Disk    Rename(wxclp:pcl01) InfDs(dbfds)
     Fwmeml     if   e           k Disk    Rename(pem01:pem00)
     Fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
     Fwmcml1    uf   e           k Disk
     Fwmcml17   if   e           k Disk    Rename(pcm01:pcm17)
     Fwmcmlg    if   e           k Disk    Rename(pcm01:pcm0g)
     Fwmcdl     uf a e           k Disk    Rename(pcd01:pcd00)
     fwdrbl     if   e           k Disk    ReName(prb01:prb00)
     fwmwhp     if a e             Disk    ReName(pwh01:pwh0p)
     fwmawp     if a e             Disk    ReName(paw01:paw0p)
     fwmbnp     if a e             Disk    ReName(pbn01:pbn0p)
     fwde3p     if a e             Disk    ReName(pe301:pe30p)
     fwda3p     if a e             Disk    ReName(pa301:pa30p)
     fwmllp     if a e             Disk    ReName(pll01:pll0p)
     fwdacp     if a e             Disk    ReName(pac01:pac0p)
     fwmbyp     if a e             Disk    ReName(pby01:pby0p)
     fwmbyl     if a e           k Disk    ReName(pby01:pby00)

     Fqsysprt   o    f  132        Printer OfLind(*inoa)
     Fqsysprt1  o    f  198        Printer OfLind(*inob)

     D psds          esds                  ExtName(SZQ1P)

     d/copy wprClaims
     d/copy wprEmplr

      *entry
     DRequestPgm       pr                  ExtPgm('W8079R4')
     D                                1
     DRequestPgm       pi
     D inUpdate                       1

     d* Create WCMC Case
     d UpdateWCMCdemo  pr                  ExtPgm('FA002RC')
     d   qmRtCd                       1
     d   qmSS#                        9
     d   qmAcDt                       7
     d   qmCo#                        3
     d   qmSub#                       5
     d   qmFnd                        3
     d   qmFyr                        3
     d   qmCase                       7
     d   qmLsN                       30
     d   qmFsN                       15
     d   qmMid                        1
     d   qmAdr1                      40
     d   qmAdr2                      40
     d   qmCty                       25
     d   qmSt                         2
     d   qmZip                        9
     d   qmDtoB                       7
     d   qmSex                        1
     d   qmArea                       3
     d   qmPhn#                       7
     d   qmMsts                       1
     d   qmWara                       3
     d   qmWph#                       7
     d   qmCtry                       3
     d   qmAdr3                      40
     d   qmCara                       3
     d   qmCphn                       7

     d* Create WCMC Case
     d UpdateWCMC      pr                  ExtPgm('FA003RC')
     d   qmRtCd                       1
     d   qmSS#                        9
     d   qmAcDt                       7
     d   qmCo#                        3
     d   qmSub#                       5
     d   qmFnd                        3
     d   qmFyr                        3
     d   qmCase                       7
     d   qmEmp#                       9
     d   qmDiv                        5
     d   qmAcTm                       6
     d   qmDtRp                       7
     d   qmDtLw                       7
     d   qmAcDs                      60
     d   qmMcon                       1
     d   qmEntD                       7
     d   qmRcvD                       7
     d   qmMMI                        7
     d   qm1a1                        1
     d   qm10a1                      10
     d   qmLtFl                       1
     d   qmScl#                      25
     d   qmMcTm                       4
     d   qmStWk                       2
     d   qmDeDt                       7
     d   qmDeem                       2
     d   qmCmNu                       3
     d   qmCmSu                       3
     d   qmClad                       3
     d   qmRrn                        9  0
     d   qmLsN                       30
     d   qmFsN                       15
     d   qmMid                        1
     d   qmDtRe                       7
     d   qmClSu                       3
     d   qm#Dep                       3
     d   qmOcDs                      40
     d   qmHiDt                       7  0
     d   qmEsts                       2
     d   qmWkPt                       2
     d   qmPrex                       1
     d   qmAcD2                      60
     d   qmCvFl                       1
     d   qmMca                        1
     d   qmMmiP                       5  2
     d   qmOdis                       1
     d   qmYrlx                       4  0
     d   qmDbDt                       7  0
     d   qmHrDy                       3  0
     d   qmDyWk                       1  0
     d   qmClFa                       3
     d   qmDny                        1
     d   qmImp                        1
     d   qmJrSt                       2
     d   qmStat                       1
     d   qmSrc                       20

     D openStatus      c                   Const('Opened Claim     Cv-
     D                                     t - 2019     ')
     D closedStatus    c                   Const('Closed Claim     Cv-
     D                                     t - 2019     ')
     D reopenStatus    c                   Const('Reopened Claim   Cv-
     D                                     t - 2019     ')
     D upper           C                   Const('ABCDEFGHIJKLMNOPQRST+
     D                                     UVWXYZ')
     D lower           C                   Const('abcdefghijklmnopqrst+
     D                                     uvwxyz')

     D currentDate     s              7  0
     D currentTime     s              6  0
     D company#        s              3  0 Inz(777)
     D subscriber#     s              5  0 Inz(777)
     D group#          s              3  0 Inz(754)
     D pds#            s                   Like(CMpds#)
     D found           s               n   Inz(*Off)
     D updateAWW       s               n   Inz(*Off)
     D kkFnd           s                   Like(CMfnd)
     D kkFyr           s                   Like(CMfYr)
     D kkCase          s                   Like(CMcase)
     D chrZipCode      s              9
     D accidentDesc    s            120
     D amount52        s              5  2
     D amount72        s              7  2
     D amount92        s              9  2
     D xxLsCv          s                   Like(CMlsCv)
     D xxRtwQ          s                   Like(CMrtwQ)
     D xxAdjPtd        s                   Like(CLadjPtd01)
     D xxAdjStd        s                   Like(CLadjStd01)
     D xxAdjEnd        s                   Like(CLadjEnd01)
     D xxMtcD          s                   Like(LXmtcD) Inz(*Hival)
     D holdClm#        s                   Like(CLclm#)

     D                 DS
     D  DsDtTm                 1     14  0
     D  DsTime                 1      6  0
     D  DsDate                 7     14  0

       // FA002R And FA003R Shared parameters
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

       // FA002R parameters
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
     d   qmCara        s              3
     d   qmCphn        s              7

       // FA003R parameters
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
     d   qmStat        s              1
     d   qmSrc         s             20

     d claim#          s             25
     d errorMsg        s             40
     d fieldDsc        s             40
     d oldValue        s             40
     d newValue        s             40
     d countErr        s              9  0
     d countDiff       s              9  0
     d wxclpRRN        s              9  0

     D dbfds           ds
     D  dbrrn                397    400b 0
     C*================================================================
     C*      M  A I  N    L  O  G  I  C
     C*================================================================
     C*
     C                   time                    dsdttm
      /free

       // Get current date and time
       currentTime = %Dec(%Time():*HMS);
       currentDate = %Dec(%Date():*CYMD);

       // Create Report Headers
       Except HeadD;
       Except HeadE;

       Read plx01;
       DoW  Not %Eof(wxlgl1);
          SetGt  (LXacl#:LXsts:LXjSt:xxMtcD) plx01;
          ReadPE (LXacl#:LXsts:LXjSt) plx01;
          If Not %Eof(wxlgl1) And LXsts='TA';
             Chain (LXacl#:LXsts:LXjSt:LXmtc:LXmtcD) pcl01;
             If %Found(wxcll1);
                wxclpRRN = dbRRN;
                UpdateClaim();
             Else;
                WriteErrRpt(LXaclc:'Missing EDI Claim Record.':0);
             EndIf;
          EndIf;
          Read plx01;
       EndDo;

       Except totalE;
       Except totalD;

       *inLR = *On;
       Return;
     O*--------------------------------------------------------------
     O* Error Report
     O*--------------------------------------------------------------
     OQSYSPRT   E            HEADE          1 06
     O                       DSTIME               8 '  :  :  '
     O                                           50 'Error Report'
     O                       DSDATE              78 '  /  /    '
     O          E            HEADE          2
     O                       Q1PGM               10
     O                       Q1USER              66
     O                       PAGE          Z     78
     O                                           73 'PAGE'
     O          E            HeadE          2
     O                                           22 'Claim #               '
     O                                           44 '    Error             '
     O                                           66 '                      '
     O                                           88 '   In Rcd#            '
     O*
     O          E            DetailE        1
     O                       claim#              25
     O                       ErrorMsg            68
     O                       wxclpRRN            78
     O
     O          E            TotalE      1  2
     O                                            7 'Count: '
     O                       countErr      4     14
     O*--------------------------------------------------------------
     O* Difference Report
     O*--------------------------------------------------------------
     OQSYSPRT1  E            HEADD          1 06
     O                       DSTIME               8 '  :  :  '
     O                                           60 'Difference Report'
     O                       DSDATE              78 '  /  /    '
     O          E            HeadD          2
     O                       Q1PGM               10
     O                       Q1USER              66
     O                       PAGE          Z     78
     O                                           73 'PAGE'
     O          E            HeadD          2
     O                                           20 'Claim #             '
     O                                           40 '      Field Name    '
     O                                           60 '                    '
     O                                           80 '       Old Value    '
     O                                          100 '                    '
     O                                          120 '        New Value   '
     O                                          140 '                    '
     O                                          160 '         In Rcd#    '
     O*
     O          E            DetailD        1
     O                       claim#              25
     O                       FieldDsc            66
     O                       oldValue           107
     O                       newValue           148
     O                       wxclpRRN           158
     O
     O          E            Line           1
     O          E            TotalD      1  2
     O                                            7 'Count: '
     O                       countDiff     4     14

       //===================================================================
       // Convert Dates
       //===================================================================
       Dcl-Proc ConvertDate;
          Dcl-Pi *N Zoned(7:0);
             inDate Char(8) Const;
          End-Pi;

          Dcl-S decDate7 Zoned(7);

          If inDate = ' ';
             decDate7 = 0;
          Else;
             decDate7 = %Dec(inDate:8:0) - 19000000;
          EndIf;

          Return decDate7;

       End-Proc;

       //===================================================================
       // Update Claim Information
       //===================================================================
       Dcl-Proc UpdateClaim;

          // Update Claim Master
          CLclm# = %Xlate(lower:upper:CLclm#);
          pds#   = CLclm#;
          found  = *Off;
          Chain (company#:pds#) pcm17;
          If %Found(wmcml17);
             Chain (CMco#:CMfnd:CMfYr:CMcase) pcm01;
             If %Found(wmcml1);
                ExSr UpdateWMCDP;
                ExSr UpdateWMCMP;
             EndIf;
          Else;
             If %Subst(CLclm#:4:1) = '-' And %Subst(CLclm#:8:1) = '-' And
                %Subst(CLclm#:16:1) = ' ';
                kkFnd  = %Dec(%Subst(CLclm#:1:3):3:0);
                kkFyr  = %Dec(%Subst(CLclm#:5:3):3:0);
                kkCase = %Dec(%Subst(CLclm#:9:7):7:0);
                Chain (company#:kkFnd:kkFyr:kkCase) pcm01;
                If %Found(wmcml1);
                   ExSr UpdateWMCDP;
                   ExSr UpdateWMCMP;
                Else;
                   Chain (company#:CLjClm#) pcm0g;
                   If %Found(wmcmlg);
                      Chain (CMco#:CMfnd:CMfYr:CMcase) pcm01;
                      If %Found(wmcml1);
                         ExSr UpdateWMCDP;
                         ExSr UpdateWMCMP;
                      EndIf;
                   Else;
                      WriteErrRpt(CLclm#:'Claim record does not exist.':0);
                   EndIf;
                EndIf;
             Else;
                WriteErrRpt(CLclm#:'Claim record does not exist.':0);
             EndIf;
          EndIf;

          //==============================================================
          // Write/Update FMCDP
          //==============================================================
          Begsr UpdateFMCDP;

             qmRtCd = ' ';
             qmSS#  = CMss#;
             qmAcDt = %EditC(CMacDt:'X');
             qmCo#  = %EditC(CMco#:'X');
             qmSub# = %EditC(subscriber#:'X');
             qmFnd  = %EditC(CMfnd:'X');
             qmFyr  = %EditC(CMfYr:'X');
             qmCase = %EditC(CMcase:'X');
             qmLsn  = CDlsn;
             qmFsn  = CDfsn;
             qmMid  = CDMid;
             qmAdr1 = CDadr1;
             qmAdr2 = CDadr2;
             qmCty  = CDcty;
             qmSt   = CDst;
             qmZip  = %EditC(CDzip:'X');
             qmDtoB = %EditC(CDdtob:'X');
             qmSex  = CDsex;
             qmArea = %EditC(CDarea:'X');
             qmPhn# = %EditC(CDphn#:'X');
             qmMsts = CDmSts;
             qmWara = %EditC(CDwara:'X');
             qmWph# = %EditC(CDwPh#:'X');
             qmCtry = CDctry;
             qmAdr3 = CDadr3;
             qmCara = %EditC(CDcAra:'X');
             qmCphn = %EditC(CDcPhn:'X');

             UpdateWCMCdemo(qmRtCd:qmSS#:qmAcDt:qmCo#:qmSub#:qmFnd:
              qmFyr:qmCase:qmLsN:qmFsN:qmMid:qmAdr1:qmAdr2:
              qmCty:qmSt:qmZip:qmDtoB:qmSex:qmArea:qmPhn#:
              qmMsts:qmWara:qmWph#:qmCtry:qmAdr3:qmCara:qmCphn);

          Endsr;

          //==============================================================
          // Write/Update FMCMP
          //==============================================================
          Begsr UpdateFMCMP;

             qmrtCd = ' ';
             qmss#  = CMss#;
             qmAcDt = %EditC(CMacDt:'X');
             qmCo#  = %EditC(CMco#:'X');
             qmSub# = ' ';
             qmFnd  = %EditC(CMfnd:'X');
             qmFyr  = %EditC(CMfYr:'X');
             qmCase = %EditC(CMcase:'X');
             qmEmp# = %EditC(CMemp#:'X');
             qmdiv  = %EditC(CMdiv:'X');
             qmAcTm = %EditC(CMacTm:'X');
             qmDtRp = %EditC(CMdtRp:'X');
             qmDtLw = %EditC(CMdtLw:'X');
             qmacDs = CMacDs;
             qmMcon = CMmCon;
             qmEntD = %EditC(CMentD:'X');
             qmRcvD = %EditC(CMRcvD:'X');
             qmMMI  = %EditC(CMmmi:'X');
             qm1A1  = ' ';
             qm10A1 = ' ';
             qmLtFl = CMltFl;
             qmScl# = CMacl#;
             qmMcTm = ' ';
             qmstWk = ' ';
             qmdeDt = %EditC(CMdod:'X');

             If CMdth = 'Y';
                qmDeem = 'DI';
             Else;
                qmDeem = '  ';
             EndIf;

             qmCmNu = CMnrse;
             qmCmSu = ' ';
             qmClad = CMclex;
             qmRrn  = 0;
             qmLsn  = CMlsn;
             qmFsn  = CMfsn;
             qmMid  = CMmid;
             qmdtRe = %EditC(CMdtRe:'X');
             qmClSu = CMsupr;
             qm#Dep = %EditC(CM#Dep:'X');
             qmOcDs = cmOcDs;
             qmHiDt = CMhiDt;
             qmEsts = CMeSts;
             qmEsts = CMests + ' ';
             qmPrex = CMprex;
             qmAcD2 = CMacD2;
             qmCvFl = CMcvFl;
             qmMCA  = CMmco;
             qmMmip = CMiPct;
             qmOdis = CMoDis;
             qmYrLx = CMyrLx;
             qmDbDt = CMdbDt;
             qmHrDy = CMhrsD;
             qmDyWk = CMdays;
             qmClFa = CMfead;
             qmDny  = CMdny;
             qmImp  = CMimp;
             qmJrSt = CMjrSt;
             qmStat = CMstat;
             qmSrc  = ' ';

             // Write WCMC Claim Master
             UpdateWCMC(qmRtCd:qmSS#:qmAcDt:qmCo#:qmSub#:qmFnd:qmFyr:qmCase:
              qmEmp#:qmDiv:qmAcTm:qmDtRp:qmDtLw:qmAcDs:qmMcon:qmEntD:qmRcvD:
              qmMMI:qm1a1:qm10a1:qmLtFl:qmScl#:qmMcTm:qmStWk:qmDeDt:qmDeem:
              qmCmNu:qmCmSu:qmClad:qmRrn:qmLsn:qmFsn:qmMid:qmDtRe:qmClSu:
              qm#Dep:qmOcDs:qmHiDt:qmEsts:qmWkPt:qmPrex:qmAcD2:qmCvFl:qmMca:
              qmMmiP:qmOdis:qmYrlx:qmDbDt:qmHrDy:qmDyWk:qmClFa:qmDny:qmImp:
               qmJrSt:qmStat:qmSrc);

          Endsr;

          //==============================================================
          // Write/Update WMCDP
          //==============================================================
          Begsr UpdateWMCDP;

             Clear pcd00;
             Chain CMss# pcd00;
             If inUpdate <> 'Y';
                Unlock wmcdl;
             EndIf;
             If Not %Found(wmcdl);
                WriteErrRpt(LXaclc:'Claim Demographic Record does not exist.'
                             :wxclpRRN);
                LeaveSR;
             EndIf;

             // SS#
             If WriteDiffRpt(CLclm#:'SS#':CDss#:CLss#:wxclpRRN);
                // CDss# = CLss#;
             EndIf;

             // Claimant Address
             If CLclmAdr11 <> ' ';
                If WriteDiffRpt(CLclm#:'Claim Address 1':CDadr1:
                                 CLclmAdr11:wxclpRRN);
                   // CDadr1 = CLclmAdr11;
                EndIf;
                If WriteDiffRpt(CLclm#:'Claim Address 2':CDadr2:
                                CLclmAdr21:wxclpRRN);
                   // CDadr2 = CLclmAdr21;
                EndIf;
                iF WriteDiffRpt(CLclm#:'Claim City':CDcty:
                                 CLclmCty1:wxclpRRN);
                   // CDcty  = CLclmCty1;
                EndIf;
                If WriteDiffRpt(CLclm#:'Claim State':CDst:
                                 CLclmSt1:wxclpRRN);
                   // CDst   = CLClmSt1;
                EndIf;
                If WriteDiffRpt(CLclm#:'Claim Zip Code':
                           %EditC(CDzip:'X'):CLclmZip1:wxclpRRN);
                   Monitor;
                      // CDzip = %Dec(CLclmZip1:9:0);
                   On-Error;
                      // CDzip = 0;
                   EndMon;
                EndIf;
             EndIf;

             // Claimant Phone#
             If CLclmPhn# <> ' ';
                If WriteDiffRpt(CLclm#:'Claim Phone Area Code':
                    %EditC(CDarea:'X'):%Subst(CLclmPhn#:1:3):wxclpRRN);
                   // CDarea = %Dec(%Subst(CLclmPhn#:1:3):3:0);
                EndIf;
                If WriteDiffRpt(CLclm#:'Claim Phone #':
                    %EditC(CDphn#:'X'):%Subst(CLclmPhn#:4:7):wxclpRRN);
                   // CDphn# = %Dec(%Subst(CLclmPhn#:4:7):7:0);
                EndIf;
             EndIf;

             // Gender
             If WriteDiffRpt(CLclm#:'Gender':CDsex:CLsex:wxclpRRN);
                // CDsex = CLsex;
             EndIf;

             // Marital Status
             If WriteDiffRpt(CLclm#:'Marital Status':
                  CDmSts:CLmSts:wxclpRRN);
                // CDmSts = CLmSts;
             EndIf;

             // Date of Birth
             If WriteDiffRpt(CLclm#:'Date of Birth':%EditC(CDdtob:'X'):
                              %EditC(ConvertDate(CLdtob):'X'):wxclpRRN);
                // CDdtob = ConvertDate(CLdtob);
             EndIf;

             // Claimant Last Name
             If WriteDiffRpt(CLclm#:'Claimant Last Name':CDlsn:CLclmLsn:
                              wxclpRRN);
                // CDlsn  = CLclmLsn + ' ' + CLClmSfx;
             EndIf;

             // Claimant First Name
             iF WriteDiffRpt(CLclm#:'Claimant First Name':CDfsn:CLclmFsn:
                              wxclpRRN);
                // CDfsn  = CLclmFsn;
             EndIf;

             // Claimant Middle Intial
             If WriteDiffRpt(CLclm#:'Claimant Middle Name':CDmid:CLclmMid:
                              wxclpRRN);
                // CDmid  = CLclmMid;
             EndIf;

             CDdt   = currentDate;
             CDtime = currentTime;
             CDuser = Q1user;
             CDpgm  = Q1pgm;

             If inUpdate = 'Y';
                If %Found(wmcdl);
                   Update pcd00;
                   ExSr UpdateFMCDP;
                EndIf;
             EndIf;

          Endsr;

          //==============================================================
          // Write/Update WMCMP
          //==============================================================
          Begsr UpdateWMCMP;

             CMss#  = CDss#;
             CMlsn  = CDlsn;
             CMfsn  = CDfsn;
             CMmid  = CDmid;

             // Jurisdiction State
             If WriteDiffRpt(CLclm#:'Jurisdiction State':CMjrSt:CLjrSt:
                              wxclpRRN);
                CMjrSt = CLjrSt;
             EndIf;

             // Loss Coverag Code
             Select;
             When CLlossType = '1';
                xxLsCv = '01';
             When CLlossType = '2';
                xxLsCv = '14';
             When CLlossType = '3';
                xxLsCv = '17';
             Other;
                xxLsCv = '01';
             EndSl;
             If WriteDiffRpt(CLclm#:'Loss Coverage':CMlsCV:xxLsCv:
                              wxclpRRN);
                CMlsCv  = xxLsCv;
             EndIf;

             // SIC Code
             If WriteDiffRpt(CLclm#:'SIC Code':CMsic:CLsic:
                              wxclpRRN);
                CMsic  = CLsic;
             EndIf;

             // Occupation Description
             If WriteDiffRpt(CLclm#:'Occupation Description':CMocDs:CLocDs:
                              wxclpRRN);
                CMocDs = CLocDs;
             EndIf;

             // Date Last Worked
             If WriteDiffRpt(CLclm#:'Date Last Worked':%EditC(CMdtLw:'X'):
                              %EditC(ConvertDate(CLldw):'X'):wxclpRRN);
                CMdtLw = ConvertDate(CLldw);
             EndIf;

             // Full Wages Paid On Injury Date
             If WriteDiffRpt(CLclm#:'Full Wages Paid On Injury Date':
                              CMpoi:CLfSal:wxclpRRN);
                CMPOI  = %Trim(CLfSal);
             EndIf;

             // Released To Work Date Or Return To Work Date
             If CLrtwType = 'R';
                If WriteDiffRpt(CLclm#:'Released To Work Date':
                     %EditC(CMrlw:'X'):%EditC(ConvertDate(CLrtw):'X'):
                     wxclpRRN);
                   CMrlw  = ConvertDate(CLrtw);
                EndIf;
             Else;
                If WriteDiffRpt(CLclm#:'Return To Work Date':
                     %EditC(CMrtw:'X'):%EditC(ConvertDate(CLrtw):'X'):
                     wxclpRRN);
                   CMrtw  = ConvertDate(CLrtw);
                EndIf;
             EndIf;

             // RTW Qualifier
             If CLphyRst = 'Y';
                xxRtwQ = '2';
             Else;
                Select;
                When CLrtwType = 'A';
                   xxRtwQ = '1';
                When CLrtwType = 'R';
                   xxRtwQ = '5';
                EndSl;
             EndIf;
             If CLrtwDt = ' ';
                If WriteDiffRpt(CLclm#:'RTW Qualifier':
                                 CMrtWQ:xxRtwQ:wxclpRRN);
                   CMrtwQ = xxrtwQ;
                EndIf;
             Else;
                If inUpdate = 'Y';
                   // Write to work history
                   Clear pwh0p;
                   WHco#  = CMco#;
                   WHfnd  = CMfnd;
                   WHfYr  = CMfYr;
                   WHcase = CMcase;
                   WHdate = ConvertDate(CLrtwDt);
                   WHwkSt = 'E';
                   WHstat = 'A';
                   WHuser = Q1user;
                   WHpgm  = Q1pgm;
                   WHdt   = currentDate;
                   WHtime = currentTime;
                   WHiUsr = Q1user;
                   WHiPgm = Q1pgm;
                   WHiDt  = currentDate;
                   WHiTim = currentTime;
                   WHrtwQ = xxRtwQ;
                   Write    pwh0p;
                Endif;
             EndIf;

             // Late Reason Code
             If WriteDiffRpt(CLclm#:'Late Reason Code':CMltRe:
                              CLlat00001:wxclpRRN);
                CMltRe = %Trim(CLlat00001);
             EndIf;

             // Denial code 1;
             If WriteDiffRpt(CLclm#:'Denial Code 1':CMdny1:
                              CLdenRea01:wxclpRRN);
                CMdny1 = %Trim(CLdenRea01);
             EndIf;

             // Denial code 2;
             If WriteDiffRpt(CLclm#:'Denial Code 2':CMdny2:
                              CLdenRea02:wxclpRRN);
                CMdny2 = %Trim(CLdenRea02);
             EndIf;

             // Denial code 3;
             If WriteDiffRpt(CLclm#:'Denial Code 3':CMdny3:
                              CLdenRea03:wxclpRRN);
                CMdny3 = %Trim(CLdenRea03);
             EndIf;

             // Denial code 4;
             If WriteDiffRpt(CLclm#:'Denial Code 4':CMdny4:
                              CLdenRea04:wxclpRRN);
                CMdny4 = %Trim(CLdenRea04);
             EndIf;

             // Denial code 5;
             If WriteDiffRpt(CLclm#:'Denial Code 5':CMdny5:
                              CLdenRea05:wxclpRRN);
                CMdny5 = %Trim(CLdenRea05);
             EndIf;

             // Denial reason narrative
             If WriteDiffRpt(CLclm#:'Denial reason narrative':CMdnyN:
                              CLdenReas:wxclpRRN);
                CMdnyN = %Trim(CLdenReas);
             EndIf;

             // Suspension narrative
             If WriteDiffRpt(CLclm#:'Suspension narrative':CMsusN:
                              CLsusReas:wxclpRRN);
                CMsusN = %Trim(CLsusReas);
             EndIf;

             // Other ID Type
             If WriteDiffRpt(CLclm#:'Other ID Type':
                              CMeitQ:CLclmIdTp:wxclpRRN);
                CMeitQ = %Trim(CLclmIdTp);
             EndIf;

             // Full Salary Continued
             If WriteDiffRpt(CLclm#:'Full Salary Continued':
                              CMfSAl:CLsalCont:wxclpRRN);
                CMfSal = %Trim(CLsalCont);
             EndIf;

             // # of Days a Week
             If CLdays = ' ';
                CLdays = '0';
             EndIf;
             If WriteDiffRpt(CLclm#:'# of Days a Week':
                              %EditC(CMdays:'X'):CLdays:wxclpRRN);
                CMdays = %Dec(%Trim(CLdays):1:0);
             EndIf;

             // Payroll Class Code
             If CLmanClass = ' ';
                CLmanClass = '00000';
             EndIf;
             If WriteDiffRpt(CLclm#:'Payroll Class Code':
                              %EditC(CMclas:'X'):CLmanClass:wxclpRRN);
                // CMclas = %Dec(%Trim(CLmanClass):5:0);
             EndIf;

             // Employment Status
             Select;
             When CLeSts = 'A';
                CLeSts = 'AD';
             When CLeSts = 'B';
                CLeSts = 'AP';
             When CLeSts = 'C';
                CLeSts = 'PW';
             When CLeSts = '1';
                CLeSts = 'FT';
             When CLeSts = '2';
                CLeSts = 'PT';
             When CLeSts = '3';
                CLeSts = 'NE';
             When CLeSts = '4';
                CLeSts = 'OS';
             When CLeSts = '5';
                CLeSts = 'DS';
             When CLeSts = '6';
                CLeSts = 'RT';
             When CLeSts = '7';
                CLeSts = 'ZZ';
             When CLeSts = '8';
                CLeSts = 'SL';
             When CLeSts = '9';
                CLeSts = 'VO';
             Other;
                CLeSts = 'ZZ';
             EndSl;
             If WriteDiffRpt(CLclm#:'Employment Status':
                              CMeSts:CLeSts:wxclpRRN);
                CMeSts = CLeSts;
             EndIf;

             // Accident Date
             If WriteDiffRpt(CLclm#:'Accident Date':
                %EditC(CMacDt:'X'):%EditC(ConvertDate(CLacDt):'X'):
                wxclpRRN);
                // CMacDt = ConvertDate(CLacDt);
             EndIf;

             // Accident Time
             If CLacTm = ' ';
                CLacTm = '000000';
             EndIf;
             If WriteDiffRpt(CLclm#:'Accident Time':
                              %EditC(CMacTm:'X'):CLacTm:wxclpRRN);
                // CMactm = %Dec(CLacTm:6:0);
             EndIf;

             // Date of Death
             If WriteDiffRpt(CLclm#:'Date of Death':
                %EditC(CMdod:'X'):%EditC(ConvertDate(CLdeath):'X'):
                wxclpRRN);
                CMdod  = ConvertDate(CLdeath);
             EndIf;

             // Compensation Rate
             updateAww = *Off;
             If CLclcWCAm <> ' ';
                amount72 = %Dec(%Trim(CLclcWCAm):7:0)/100;
             Else;
                amount72 = 0;
             EndIf;
             If WriteDiffRpt(CLclm#:'Compensation Rate':
                   %EditC(CMcrt:'X'):%EditC(amount72:'X'):
                   wxclpRRN);
                CMcrt = amount72;
                updateAww = *On;
             EndIf;

             // Average Weekly Wage
             If CLrealAw  <> ' ';
                amount72 = %Dec(%Trim(CLrealAw):7:0)/100;
             Else;
                amount72 = 0;
             EndIf;
             If WriteDiffRpt(CLclm#:'Average Weekly Wage':
                   %EditC(CMaww:'X'):%EditC(amount72:'X'):
                   wxclpRRN);
                CMaww = amount72;
                updateAww = *On;
             EndIf;

             // Partial Denial Reason Code
             If WriteDiffRpt(CLclm#:'Partial Denial Reason Code':
                              CMpDny:CLpartDeny:wxclpRRN);
                CMpDny = %Trim(CLpartDeny);
             EndIf;

             // Date Notified of LT
             If WriteDiffRpt(CLclm#:'Date Notified of LT':
                %EditC(CMdnLT:'X'):%EditC(ConvertDate(CLadmKDt):'X'):
                wxclpRRN);
                CMdnLT = ConvertDate(CLadmKDt);
             EndIf;

             // Current Date Disability Began
             If WriteDiffRpt(CLclm#:'Current Date Disability Began':
                %EditC(CMcddb:'X'):
                %EditC(ConvertDate(%Subst(CLdisBegDt:1:8)):'X'):
                wxclpRRN);
                CMcddB = ConvertDate(%Subst(CLdisBegDt:1:8));
             EndIf;

             // Current Date Last Worked
             If WriteDiffRpt(CLclm#:'Current Date Last Worked':
                %EditC(CMcdLw:'X'):%EditC(ConvertDate(CLldwDt):'X'):
                wxclpRRN);
                CMcdLw = ConvertDate(CLldwDT);
             EndIf;

             // Weekly Amount File
             Select;
             When CLadjCod01 = '240';
                xxAdjPtd = CLadjPtd01;
                xxAdjStd = CLadjStd01;
                xxAdjEnd = CLadjEnd01;
             When CLadjCod02 = '240';
                xxAdjPtd = CLadjPtd02;
                xxAdjStd = CLadjStd02;
                xxAdjEnd = CLadjEnd02;
             When CLadjCod03 = '240';
                xxAdjPtd = CLadjPtd03;
                xxAdjStd = CLadjStd03;
                xxAdjEnd = CLadjEnd03;
             When CLadjCod04 = '240';
                xxAdjPtd = CLadjPtd04;
                xxAdjStd = CLadjStd04;
                xxAdjEnd = CLadjEnd04;
             When CLadjCod05 = '240';
                xxAdjPtd = CLadjPtd05;
                xxAdjStd = CLadjStd05;
                xxAdjEnd = CLadjEnd05;
             When CLadjCod06 = '240';
                xxAdjPtd = CLadjPtd06;
                xxAdjStd = CLadjStd06;
                xxAdjEnd = CLadjEnd06;
             When CLadjCod07 = '240';
                xxAdjPtd = CLadjPtd07;
                xxAdjStd = CLadjStd07;
                xxAdjEnd = CLadjEnd07;
             When CLadjCod08 = '240';
                xxAdjPtd = CLadjPtd08;
                xxAdjStd = CLadjStd08;
                xxAdjEnd = CLadjEnd08;
             When CLadjCod09 = '240';
                xxAdjPtd = CLadjPtd09;
                xxAdjStd = CLadjStd09;
                xxAdjEnd = CLadjEnd09;
             When CLadjCod10 = '240';
                xxAdjPtd = CLadjPtd10;
                xxAdjStd = CLadjStd10;
                xxAdjEnd = CLadjEnd10;
             EndSl;

             If inUpdate = 'Y';
                   // Write to Average Weekly Wage
                If updateAww = *On;
                   Clear paw0p;
                   AWco#  = CMco#;
                   AWfnd  = CMfnd;
                   AWfYr  = CMfYr;
                   AWcase = CMcase;
                   AWtrDt = currentDate;
                   AWaww  = CMaww;
                   AWcrt  = CMcrt;
                   AWstat = 'A';
                   AWuser = Q1user;
                   AWpgm  = Q1pgm;
                   AWdt   = currentDate;
                   AWtime = currentTime;
                   AWefDt = ConvertDate(%Trim(CLwageEfDt));
                   Write    paw0p;
                Endif;
                // Populate Benefit Adjustments
                // Benefit Code 1
                If CLbenPmt01 <> ' ';
                   Clear pbn0p;
                   BNco#  = CMco#;
                   BNfnd  = CMfnd;
                   BNfYr  = CMfYr;
                   BNcase = CMcase;
                   BNaCod = CLbenPmt01;
                   BNstDt = ConvertDate(%Trim(CLbenStd01));
                   If CLbenAww01 <> ' ';
                      amount72 = %Dec(%Trim(CLbenAww01):7:0)/100;
                   Else;
                      amount72 = 0;
                   EndIf;
                   BNamt  = amount72;
                   BNstat = 'A';
                   BNuser = Q1user;
                   BNpgm  = Q1pgm;
                   BNdt   = currentDate;
                   BNtime = currentTime;
                   Write    pbn0p;
                EndIf;
                // Benefit Code 2
                If CLbenPmt02 <> ' ';
                   Clear pbn0p;
                   BNco#  = CMco#;
                   BNfnd  = CMfnd;
                   BNfYr  = CMfYr;
                   BNcase = CMcase;
                   BNaCod = CLbenPmt02;
                   BNstDt = ConvertDate(%Trim(CLbenStd02));
                   If CLbenAww02 <> ' ';
                      amount72 = %Dec(%Trim(CLbenAww02):7:0)/100;
                   Else;
                      amount72 = 0;
                   EndIf;
                   BNamt  = amount72;
                   BNstat = 'A';
                   BNuser = Q1user;
                   BNpgm  = Q1pgm;
                   BNdt   = currentDate;
                   BNtime = currentTime;
                   Write    pbn0p;
                EndIf;
                // Benefit Code 3
                If CLbenPmt03 <> ' ';
                   Clear pbn0p;
                   BNco#  = CMco#;
                   BNfnd  = CMfnd;
                   BNfYr  = CMfYr;
                   BNcase = CMcase;
                   BNaCod = CLbenPmt03;
                   BNstDt = ConvertDate(%Trim(CLbenStd03));
                   If CLbenAww03 <> ' ';
                      amount72 = %Dec(%Trim(CLbenAww03):7:0)/100;
                   Else;
                      amount72 = 0;
                   EndIf;
                   BNamt  = amount72;
                   BNstat = 'A';
                   BNuser = Q1user;
                   BNpgm  = Q1pgm;
                   BNdt   = currentDate;
                   BNtime = currentTime;
                   Write    pbn0p;
                EndIf;
                // Benefit Code 4
                If CLbenPmt01 <> ' ';
                   Clear pbn0p;
                   BNco#  = CMco#;
                   BNfnd  = CMfnd;
                   BNfYr  = CMfYr;
                   BNcase = CMcase;
                   BNaCod = CLbenPmt04;
                   BNstDt = ConvertDate(%Trim(CLbenStd04));
                   If CLbenAww04 <> ' ';
                      amount72 = %Dec(%Trim(CLbenAww04):7:0)/100;
                   Else;
                      amount72 = 0;
                   EndIf;
                   BNamt  = amount72;
                   BNstat = 'A';
                   BNuser = Q1user;
                   BNpgm  = Q1pgm;
                   BNdt   = currentDate;
                   BNtime = currentTime;
                   Write    pbn0p;
                EndIf;

                // Concurrent Employer
                If CLcemNm101 <> ' ';
                   Clear pe30p;
                   E3co#  = CMco#;
                   E3fnd  = CMfnd;
                   E3fYr  = CMfYr;
                   E3case = CMcase;
                   E3eNam = CLcemNm101;
                   If CLcemWag01 <> ' ';
                      amount72 = %Dec(%Trim(CLcemWag01):7:0)/100;
                   Else;
                      amount72 = 0;
                   EndIf;
                   E3wage = %EditC(amount72:'4');
                   Write    pE30p;
                EndIf;

                // Weekly Amount
                If CLadjCod01 = '240' Or CLadjCod02 = '240' or
                   CLadjCod03 = '240' Or CLadjCod04 = '240' or
                   CLadjCod05 = '240' Or CLadjCod06 = '240' or
                   CLadjCod07 = '240' Or CLadjCod08 = '240' or
                   CLadjCod09 = '240' Or CLadjCod10 = '240';
                   Clear pa30p;
                   A3co#  = CMco#;
                   A3fnd  = CMfnd;
                   A3fYr  = CMfYr;
                   A3case = CMcase;
                   A3btc  = '240';
                   If xxAdjPtd <> ' ';
                      amount92 = %Dec(%Trim(xxAdjPtd):9:0)/100;
                   Else;
                      amount92 = 0;
                   EndIf;
                   A3paid = amount92;
                   A3stDt = ConvertDate(xxAdjStd);
                   A3enDt = ConvertDate(xxAdjEnd);
                   A3user = Q1user;
                   A3pgm  = Q1pgm;
                   A3dt   = currentDate;
                   A3time = currentTime;
                   A3iUsr = Q1user;
                   A3iPgm = Q1pgm;
                   A3iDt  = currentDate;
                   A3iTim = currentTime;
                   Write    pA30p;
                EndIf;

                // Auxiliary Comp Payee
                // Payee 1
                If CLdepRel01 <> ' ';
                   Clear pac0p;
                   ACco#  = CMco#;
                   ACfnd  = CMfnd;
                   ACfYr  = CMfYr;
                   ACcase = CMcase;
                   ACrel  = CLdepRel01;
                   ACord  = CLdepBO01;
                   ACstat = 'A';
                   ACuser = Q1user;
                   ACpgm  = Q1pgm;
                   ACdt   = currentDate;
                   ACtime = currentTime;
                   ACiUsr = Q1user;
                   ACiPgm = Q1pgm;
                   ACiDt  = currentDate;
                   Write    pAC0p;
                EndIf;
                // Payee 2
                If CLdepRel02 <> ' ';
                   Clear pac0p;
                   ACco#  = CMco#;
                   ACfnd  = CMfnd;
                   ACfYr  = CMfYr;
                   ACcase = CMcase;
                   ACrel  = CLdepRel02;
                   ACord  = CLdepBO02;
                   ACstat = 'A';
                   ACuser = Q1user;
                   ACpgm  = Q1pgm;
                   ACdt   = currentDate;
                   ACtime = currentTime;
                   ACiUsr = Q1user;
                   ACiPgm = Q1pgm;
                   ACiDt  = currentDate;
                   Write    pAC0p;
                EndIf;
                // Payee 3
                If CLdepRel03 <> ' ';
                   Clear pac0p;
                   ACco#  = CMco#;
                   ACfnd  = CMfnd;
                   ACfYr  = CMfYr;
                   ACcase = CMcase;
                   ACrel  = CLdepRel03;
                   ACord  = CLdepBO03;
                   ACstat = 'A';
                   ACuser = Q1user;
                   ACpgm  = Q1pgm;
                   ACdt   = currentDate;
                   ACtime = currentTime;
                   ACiUsr = Q1user;
                   ACiPgm = Q1pgm;
                   ACiDt  = currentDate;
                   Write    pAC0p;
                EndIf;
                // Payee 4
                If CLdepRel04 <> ' ';
                   Clear pac0p;
                   ACco#  = CMco#;
                   ACfnd  = CMfnd;
                   ACfYr  = CMfYr;
                   ACcase = CMcase;
                   ACrel  = CLdepRel04;
                   ACord  = CLdepBO04;
                   ACstat = 'A';
                   ACuser = Q1user;
                   ACpgm  = Q1pgm;
                   ACdt   = currentDate;
                   ACtime = currentTime;
                   ACiUsr = Q1user;
                   ACiPgm = Q1pgm;
                   ACiDt  = currentDate;
                   Write    pAC0p;
                EndIf;
                // Payee 5
                If CLdepRel05 <> ' ';
                   Clear pac0p;
                   ACco#  = CMco#;
                   ACfnd  = CMfnd;
                   ACfYr  = CMfYr;
                   ACcase = CMcase;
                   ACrel  = CLdepRel05;
                   ACord  = CLdepBO05;
                   ACstat = 'A';
                   ACuser = Q1user;
                   ACpgm  = Q1pgm;
                   ACdt   = currentDate;
                   ACtime = currentTime;
                   ACiUsr = Q1user;
                   ACiPgm = Q1pgm;
                   ACiDt  = currentDate;
                   Write    pAC0p;
                EndIf;
                // Payee 6
                If CLdepRel06 <> ' ';
                   Clear pac0p;
                   ACco#  = CMco#;
                   ACfnd  = CMfnd;
                   ACfYr  = CMfYr;
                   ACcase = CMcase;
                   ACrel  = CLdepRel06;
                   ACord  = CLdepBO06;
                   ACstat = 'A';
                   ACuser = Q1user;
                   ACpgm  = Q1pgm;
                   ACdt   = currentDate;
                   ACtime = currentTime;
                   ACiUsr = Q1user;
                   ACiPgm = Q1pgm;
                   ACiDt  = currentDate;
                   Write    pAC0p;
                EndIf;
                // Payee 7
                If CLdepRel07 <> ' ';
                   Clear pac0p;
                   ACco#  = CMco#;
                   ACfnd  = CMfnd;
                   ACfYr  = CMfYr;
                   ACcase = CMcase;
                   ACrel  = CLdepRel07;
                   ACord  = CLdepBO07;
                   ACstat = 'A';
                   ACuser = Q1user;
                   ACpgm  = Q1pgm;
                   ACdt   = currentDate;
                   ACtime = currentTime;
                   ACiUsr = Q1user;
                   ACiPgm = Q1pgm;
                   ACiDt  = currentDate;
                   Write    pAC0p;
                EndIf;

                // Body Part Record
                // Body Part 1
                If CLpart1 <> ' ';
                   Clear pac0p;
                   BYco#  = CMco#;
                   BYfnd  = CMfnd;
                   BYfYr  = CMfYr;
                   BYcase = CMcase;
                   BYseq# = 1;
                   BYkey  = CLpart1;
                   If CLdisRat1 <> ' ';
                      amount52 = %Dec(%Trim(CLdisRat1):5:0)/100;
                   Else;
                      amount52 = 0;
                   EndIf;
                   BYpct  = amount52;
                   If CLpart1 = '99';
                      CMiPct = amount52;
                   EndIf;
                   BYstat = 'A';
                   BYuser = Q1user;
                   BYpgm  = Q1pgm;
                   BYdt   = currentDate;
                   BYtime = currentTime;
                   Chain (BYco#:BYfnd:BYfYr:BYcase:BYseq#:BYkey) pby00;
                   If Not %Found(wmbyl);
                      Write    pby0p;
                   EndIf;
                EndIf;
                // Body Part 2
                If CLpart2 <> ' ';
                   Clear pac0p;
                   BYco#  = CMco#;
                   BYfnd  = CMfnd;
                   BYfYr  = CMfYr;
                   BYcase = CMcase;
                   BYseq# = 2;
                   BYkey  = CLpart2;
                   If CLdisRat2 <> ' ';
                      amount52 = %Dec(%Trim(CLdisRat2):5:0)/100;
                   Else;
                      amount52 = 0;
                   EndIf;
                   BYpct  = amount52;
                   If CLpart2 = '99';
                      CMiPct = amount52;
                   EndIf;
                   BYstat = 'A';
                   BYuser = Q1user;
                   BYpgm  = Q1pgm;
                   BYdt   = currentDate;
                   BYtime = currentTime;
                   Chain (BYco#:BYfnd:BYfYr:BYcase:BYseq#:BYkey) pby00;
                   If Not %Found(wmbyl);
                      Write    pby0p;
                   EndIf;
                EndIf;

                // Legal File
                Clear pll0p;
                LLco#  = CMco#;
                LLfnd  = CMfnd;
                LLfYr  = CMfYr;
                LLcase = CMcase;

                Select;
                When CLlmpPmtCd = '06' Or CLlmpPmtCd = '09';
                   LLmtSt = 'SF';
                When CLlmpPmtCd = '03';
                   LLmtSt = 'AS';
                When CLlmpPmtCd = '04';
                   LLmtSt = 'AW';
                EndSl;

                LLstDt = ConvertDate(CLawdOrdDt);

                LLstat = 'A';
                LLuser = Q1user;
                LLpgm  = Q1pgm;
                LLdt   = currentDate;
                LLtime = currentTime;
                LLiUsr = Q1user;
                LLiPgm = Q1pgm;
                LLiDt  = currentDate;
                LLiTm  = currentTime;
                Write    pll0p;
             EndIf;

             // Date Reported
             If WriteDiffRpt(CLclm#:'Date Reported':
                %EditC(CMdtRp:'X'):%EditC(ConvertDate(CLdtEmpK):'X'):
                wxclpRRN);
                CMdtRp = ConvertDate(CLdtEmpK);
             EndIf;

             // Date Received
             If WriteDiffRpt(CLclm#:'Date Received':
                %EditC(CMrcvD:'X'):%EditC(ConvertDate(CLdtAdmK):'X'):
                wxclpRRN);
                CMrcvD = ConvertDate(CLdtAdmK);
             EndIf;

             // Hire Date
             If WriteDiffRpt(CLclm#:'Hire Date':
                %EditC(CMhiDt:'X'):%EditC(ConvertDate(CLhiDt):'X'):
                wxclpRRN);
                CMhiDt = ConvertDate(CLhiDt);
             EndIf;

             // Wage Period
             If WriteDiffRpt(CLclm#:'Wage Period':
                              CMwgPd:CLwagPer:wxclpRRN);
                CMwgPd = %Trim(CLwagPer);
             EndIf;

             // Death Result of Injury
             If WriteDiffRpt(CLclm#:'Death Result of Injury':
                              CMiDth:CLiDth:wxclpRRN);
                CMiDth = %Trim(CLiDth);
             EndIf;

             // ID other than SS#
             If CLclmIdTp <> 'S';
                If WriteDiffRpt(CLclm#:'ID other than SS#':
                                 CMeId:CLss#:wxclpRRN);
                   CMeId  = %Trim(CLss#);
                EndIf;
             EndIf;

             // Permanent Impairment Min Pmt Flag
             If WriteDiffRpt(CLclm#:'Permanent Impairment Min Pmt Flag':
                              CMpImp:CLpimInPmt:wxclpRRN);
                CMpImp = CLpimInPmt;
             EndIf;

             // RTW with Same Employer
             If CLrtwSEmp = 'Y';
                CLrtwSEmp = 'Y';
             Else;
                CLrtwSEmp = 'N';
             EndIf;
             If WriteDiffRpt(CLclm#:'RTW with Same Employer':
                              CMsame:CLrtwSEmp:wxclpRRN);
                CMsame = CLrtwSEmp;
             EndIf;

             // Suspension Effective Date
             If WriteDiffRpt(CLclm#:'Suspension Effective Date':
                   %EditC(CMsuEf:'X'):
                   %EditC(ConvertDate(%Subst(CLsusEfDt:1:8)):'X'):
                   wxclpRRN);
                CMsuEf = ConvertDate(%Subst(CLsusEfDt:1:8));
             EndIf;

             // Full Denial Recission Date
             If WriteDiffRpt(CLclm#:'Full Denial Recission Date':
                   %EditC(CMdres:'X'):
                   %EditC(ConvertDate(%Subst(CLdenResDt:1:8)):'X'):
                   wxclpRRN);
                CMdres = ConvertDate(%Subst(CLdenResDt:1:8));
             EndIf;

             // Denial Effective Date
             If WriteDiffRpt(CLclm#:'Denial Effective Date':
                   %EditC(CMdyef:'X'):
                   %EditC(ConvertDate(%Subst(CLdenEfDt:1:8)):'X'):
                   wxclpRRN);
                CMdyef = ConvertDate(%Subst(CLdenEfDt:1:8));
             EndIf;

             // Reduced Benefit Amount Type
             If WriteDiffRpt(CLclm#:'Reduced Benefit Amount Type':
                              CMrbac:CLredBenCd:wxclpRRN);
                CMrbac = %Trim(CLredBenCd);
             EndIf;

             // Non-consecutive Period Code
             If WriteDiffRpt(CLclm#:'Non-consecutive Period Code':
                              CMncpc:CLnonCon:wxclpRRN);
                CMncpc = %Trim(CLnonCon);
             EndIf;

             // Tax Filing Status
             If WriteDiffRpt(CLclm#:'Tax Filing Status':
                              CMetfs:CLtaxSts:wxclpRRN);
                CMetfs = %Trim(CLtaxSts);
             EndIf;

             // EDI Claim Type
             If (CMeTyp = 'P' And CLclmType = 'I') Or
                (CMeTyp = 'N' And CLclmType = 'M');
                If WriteDiffRpt(CLclm#:'EDI Claim Type':
                                 CMeTyp:CLclmType:wxclpRRN);
                   CMeTyp = CLclmType;
                Endif;
             EndIf;

             // Lost Time Flag
             If CLclmType <> 'I' And CLclmType <> 'M';
                If CMar03 > 0;
                   CLclmType = 'I';
                Else;
                   CLclmType = 'M';
                EndIf;
             EndIf;
             If WriteDiffRpt(CLclm#:'Lost Time Flag':
                              CMltfl:CLclmType:wxclpRRN);
                CMltFl = %Trim(CLclmType);
             EndIf;

             // Disability Date
             If WriteDiffRpt(CLclm#:'Disability Date':
                   %EditC(CMdbDt:'X'):
                   %EditC(ConvertDate(%Subst(CLdisDate:1:8)):'X'):
                   wxclpRRN);
                CMdbDt = ConvertDate(%Subst(CLdisDate:1:8));
             EndIf;

             // Initial Date of Lost Time
             If WriteDiffRpt(CLclm#:'Initial Lost Time Date':
                   %EditC(CMidLt:'X'):
                   %EditC(ConvertDate(%Subst(CLiDtLt:1:8)):'X'):
                   wxclpRRN);
                CMidLt = ConvertDate(%Subst(CLiDtLt:1:8));
             EndIf;

             // PDS Convert #
             If WriteDiffRpt(CLclm#:'PDS Convert #':
                              CMpds#:CLclm#:wxclpRRN);
                If CMpds# = ' ';
                   CMpds# = CLclm#;
                EndIf;
             EndIf;

             // Agency Claim #
             If WriteDiffRpt(CLclm#:'Agency Claim #':
                              CMacl#:CLjClm#:wxclpRRN);
                If CMacl# = ' ';
                   CMacl# = CLjClm#;
                EndIf;
             EndIf;

             // Maintenance Fields
             CMdt   = currentDate;
             CMtime = currentTime;
             CMpgm  = Q1pgm;
             CMuser = Q1user;

             If inUpdate = 'Y';
                Update pcm01;
                // Update Managed Care
                ExSr UpdateFMCMP;
             Else;
                Unlock wmcml1;
             EndIf;

          EndSr;

       End-Proc;

       //===================================================================
       // Write Difference Report
       //===================================================================
       Dcl-Proc WriteDiffRpt;
          Dcl-Pi *N Ind;
             inClaim#    Char(25)    Const;
             inFieldName Char(40)    Const;
             inOldValue  Char(40)    Const;
             inNewValue  Char(40)    Const;
             inRRN       Packed(9:0) Const;
          End-Pi;

          Dcl-S updateField Ind Inz;

          claim#     = inClaim#;
          fieldDsc   = inFieldName;
          oldValue   = inOldValue;
          newValue   = inNewValue;
          wxclpRRN   = inRRN;
          countDiff += 1;

          If oldValue <> ' ' And %Trim(oldValue) <> '00000' And
             %Trim(oldValue) <> '0000000' And %Trim(oldValue) <> '000000000'
             And %Trim(oldValue) <> '00000000000' And oldValue <> newValue;
             If *inOB = *On;
                Except HeadD;
             EndIf;

             If holdClm# = ' ';
                holdClm# = CLclm#;
             EndIf;
             If holdClm# <> CLclm#;
                Except Line;
                holdClm# = CLclm#;
             EndIf;

             Except DetailD;
          EndIf;

          If inUpdate = 'Y' And newValue <> ' ' And
             %Trim(newValue) <> '00000' And %Trim(newValue) <> '0000000'
             And %Trim(newValue) <> '000000000'
             And %Trim(newValue) <> '00000000000';
             updateField = *On;
          Else;
             updateField = *Off;
          EndIf;

          Return updateField;

       End-Proc;

       //===================================================================
       // Write Error Report
       //===================================================================
       Dcl-Proc WriteErrRpt;
          Dcl-Pi *N;
             inClaim# Char(25) Const;
             inErrorMsg Char(40) Const;
             inRRN Packed(9:0) Const;
          End-Pi;

          claim#    = inClaim#;
          errorMsg  = inErrorMsg;
          wxclpRRN  = inRRN;
          countErr += 1;

          If *inOA = *On;
             Except HeadE;
          EndIf;

          Except DetailE;

       End-Proc;

      /end-free
