    H*=========================================================================
    H* (c) 2017 StoneRiver, Inc. All rights Reserved.  -
    H*       StoneRiver is a trademark of StoneRiver, Inc.
    H*=========================================================================
    H/TITLE   F8093R - Default EOB OutBound Payment Processing Program
     h DFTACTGRP(*no) ACTGRP(*Caller)
     h option(*nodebugio: *srcstmt)
    H*=========================================================================
     H* Created on 12/20/2017 by c:62 for Project 114MIC
    H*=========================================================================
     h* Revisions
     h*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
    F*
    F* EOB Transaction File
     Ffaeal1    uf   e           k Disk
     Ffaeal2    if   e           k Disk    Rename(pea01:pea02) Prefix(e2:2)
    F*
    F* EOB Dental Header File
     Ffaedl     if   e           k Disk    Rename(ped01:ped00)
    F*
    F* EOB Comment Header File
     Ffaeel     if   e           k Disk    Rename(pee01:pee00)
    F*
    F* EOB Physician Header File
     Ffaehl     if   e           k Disk    Rename(peh01:peh00)
    F*
    F* EOB Pharmacy Header File
     Ffaepl     if   e           k Disk    Rename(pep01:pep00)
    F*
    F* EOB Hospital Header File
     Ffaeul     if   e           k Disk    Rename(peu01:peu00)
    F*
    F* EOB Header File
     Ffaeol4    if   e           k Disk    Rename(peo01:peo04)
    F*
    F* WCMC EOB Detail File
     Ffdcal19   if   e           k Disk    Rename(pca01:pca19)
    F*
    F* WCMC EOB Detail Transaction File
     Ffddal2    if   e           k Disk    Rename(pda01:pda02) Prefix(D2:2)
     Ffddal4    if   e           k Disk    Rename(pda01:pda04)
    F*
    F* EOB Dental Detail File
     Ffdddl     if   e           k Disk    Rename(pdd01:pdd00)
    F*
    F* EOB Comments Detail File
     Ffddel3    if   e           k Disk    Rename(pde01:pde03)
    F*
    F* EOB Physician Detail File
     Ffddhl     if   e           k Disk    Rename(pdh01:pdh00)
    F*
    F* EOB Pharmacy Detail File
     Ffddpl     if   e           k Disk    Rename(pdp01:pdp00)
    F*
    F* EOB Hospital Detail File
     Ffddul     if   e           k Disk    Rename(pdu01:pdu00)
    F*
    F* WCMC Diagnosis Code File (ICD9)
     Ffmdil     if   e           k Disk    Rename(pdi01:pdi00)
    F*
    F* WCMC Group Master File by FNfnd
     Ffmfnl     if   e           k Disk    Rename(pfn01:pfn00)
    F*
    F* WCMC Group Master File by FNWCF#
     Ffmfnl1    if   e           k Disk
    F*
    F* WCMC Diagnosis Code File (ICD10)
     Ffmicd10l1 if   e           k Disk
    F*
    F* WCMC Provider Master File
     Ffmpdl     if   e           k Disk    Rename(ppd01:ppd00)
    F*
    F* WCMC Specialty Code File
     Fftspl1    if   e           k Disk
    F*
    F* Network File
     Fnmnwl     if   e           k Disk    Rename(pnw01:pnw00)
    F*
    F* Payment File by EOB/Recon
     Fwdpalg    if   e           k Disk    Rename(ppa01:ppa0g) Prefix(P2:2)
    F*
    F* Payment File by Seq#
     Fwdpal     uf   e           k Disk    Rename(ppa01:ppa00)
    F*
    F* Payment Demographic File
     Fwdrpl3    if   e           k Disk    Rename(prp01:prp03)
    F*
    F* Company File
     Fsmcol     if   e           k Disk    Rename(pco01:pco00)
    F*
    F* User Parameter File
     Fsmupl     if   e           k Disk    Rename(pup01:pup00)
    F*
    F* Claim File
     Fwmcml1    if   e           k Disk
     F*
    F* Claim File
     Fwmfnl     if   e           k Disk    Rename(pfn01:wfn00) Prefix(F2:2)
     F*
     F* Univeral Table
     Fwtabl     if   e           k disk
     F                                     Rename(prc01:prc00)
     F                                     ignore(pat01)
     F                                     ignore(psp01)
     F                                     ignore(pnc01)
     F                                     ignore(pna01)
     F                                     ignore(ppa01)
     F                                     ignore(pca01)
     F                                     ignore(prt01)
     F                                     ignore(ppc01)
     F                                     ignore(pb201)
     F                                     ignore(pb701)
     F                                     ignore(pbs01)
     F                                     ignore(ptt01)
     F                                     ignore(pcc01)
     F                                     ignore(pbc01)
     F                                     ignore(pcs01)
     F                                     ignore(pcn01)
     F                                     ignore(prs01)
     F                                     ignore(plr01)
     F                                     ignore(pvs01)
     F                                     ignore(ptr01)
     F                                     ignore(phy01)
     F                                     ignore(psy01)
     F                                     ignore(pba01)
     F                                     ignore(pid01)
     F                                     ignore(pis01)
     F*
     F* Generic Table
     Fwtgtl     if   e           k disk    Rename(pgt01:pgt00)
    F*
    F*  Average Weekly Wage History File
     Ffxebob    o    e             Disk    Rename(peb01:peb0p)
    F*
    F*
    F* EOB Outbound Payment Summary Report
     Fqsysprt   o    f  132        Printer OflInd(*inOA)

    D*=========================================================================
    D* Prototypes
    D*=========================================================================
      * *Entry
     d F8093R          pr                  extpgm('F8093R')
     d inChrCo#                       3
     d inChrSub#                      5
     d inChrGrp                       3

     d F8093R          pi
     d inChrCo#                       3
     d inChrSub#                      5
     d inChrGrp                       3

     d* Build Check Comment
     d BldChkComment   pr           300
     D   inCompany                    3  0
     D   inRemarkDsc                 30
     D   inRemarkCd1                  2
     D   inRemarkCd2                  2
     D   inRemarkCd3                  2

     d* Get Payment Sequence#
     d GetPmtSeq#      pr             5  0
     D   inCompany                    3  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEob#                       9  0
     D   inEobs                       7  0
     D   inChk#                       7  0
     D   inAmount                    11  2

     d* Get WCMC Subscriber
     d GetSubscriber   pr             5  0
     d  company#                      3  0
     d  wcmsGroup                     3  0
     d  wcmcGroup                     3  0

     d* Get WCMS Group #
     d GetWcmsGrp      pr             3  0
     d  company#                      3  0
     d  subscriber#                   5  0
     d  wcmcGroup                     3  0

     d* Program End
     d ProgramEnd      pr

     d* Update the payment Extract Process Flag
     d SRCenter        pr                  ExtPgm('SRCENTER')
     D   inData                      80
     D   inLength                     3  0

     d* Update the eobs Extract Process Flag
     d UpdEAEXPR       pr
     D   inCompany                    3  0
     D   inSubscriber                 5  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEOB#                       9  0
     D   inRecon                      3  0

     d* Update the payment Extract Process Flag
     d UpdPAEXPR       pr
     D   inCompany                    3  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inSeq#                       5  0

     d* Determine if reissue is associarted with void/stop
     d VoidStop        pr              n
     D   inCompany                    3  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEob#                       9  0
     D   inEobs                       7  0
     D   inAmt                        9  2
     D   inFein                       9  0
     D   inMod                        3  0

     d* Write Payment EOB Extract
     d WrtFXEBEXT      pr
     D   inCompany                    3  0
     D   inSub#                       5  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEob#                       9  0
     D   inRcn                        3  0
     D   inSeq#                       5  0
     D   inType                       1    Const
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

     D  dsFAEAP      e ds                  extname(FAEAP)
     D  dsWDPAP      e ds                  extname(WDPAP)

    D*=========================================================================
    D*  Variables
    D*=========================================================================
       // Work Fields

     D  sqlStmtStr     s            200a
     D  moreRows       s               n   Inz(*on)
     D  totalEOB       s              9s 0 Inz(0)
     D  totalVoid      s              9s 0 Inz(0)
     D  totalStop      s              9s 0 Inz(0)
     D  totalReissue   s              9s 0 Inz(0)
     D  totalAll       s              9s 0 Inz(0)
     d  chrData        s             80a
     d  chkComment     s            300a
     d  intLength      s              3  0
     d  wcmsGroup      s              3  0
     d  wcmcGroup      s              3  0
     d  eob#           s              9  0 Inz(0)
     d  eobSeq#        s              7  0
     d  subscriber#    s              5  0
     d  eobRcn#        s              3  0
     d  sequence#      s              5  0
     d  dtlComm#       s              1  0
     d  tableID        s                   Like(GTtbID)
     d  tableKey       s                   Like(GTkey)
     d  pos            s              5  0
     d  company#       s                   Like(EAco#)
     d  group#         s                   Like(EAfnd)
    D*==============================================================+
    D* Constants
    D*===============================================================
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D BadDateData     c                   00112
    D*
    D* Report Title lines
    D*
     D title           C                   CONST('EOB Outbound Payment -
     D                                      Summary Report')
     D line            C                   CONST('____________________-
     D                                     ________________')
    C*=========================================================================
    C*      M  A I  N    L  O  G  I  C
    C*=========================================================================
      /free


       //=======================================================================
       // Access WCMS Group file
       //=======================================================================
       company#    = %Dec(inChrCo#:3:0);
       subscriber# = %Dec(inChrSub#:5:0);
       group#      = %Dec(inChrGrp:3:0);
       wcmsGroup = GetWcmsGrp(company#:subscriber#:group#);
       Chain (company#:wcmsGroup) wfn00;

       //=======================================================================
       //  Pick up New EOB Payments
       //=======================================================================

       // Build eob sql statement
       sqlStmtStr = 'Select * from FAEAP where EAfnd = ' + inChrGrp + ' ' +
                    'and EAexPr <> ''E'' And (EAWCPC <> 0 Or (EAPAID = 0 And ' +
                    'EAEPFL = ''P''))';

       // Prepare the SQL statement
       Exec SQL prepare eobStmt from :sqlStmtStr;

       // Declare the SQL cursor
       Exec SQL declare eobcursor cursor for eobStmt;

       // Run SQL
       Exec SQL open eobcursor;

       // Get first record if there is one
       Exec SQL fetch next from eobcursor into :dsFAEAP;

       // Check to see if any records were fetched
       If sqlSTT <> '00000';
          moreRows = *off;
          Exec SQL close eobcursor;
       EndIf;

       // If row was fetched then process
       DoW moreRows = *On;

           // Find payment Sequence # if not 0 eob
           If EApaid <> 0;
              wcmsGroup = GetWcmsGrp(EAco#:EAsub#:EAfnd);
              eobSeq#   = EArcn;
              sequence# = GetPmtSeq#(EAco#:wcmsGroup:EAfYr:EAcase:EAeob#:
                            eobSeq#:EAwcPc:EApaid);
           Else;
              sequence# = -1;
           EndIf;

           // Write to the Export File
           WrtFXEBEXT(EAco#:EAsub#:EAfnd:EAfYr:EAcase:
              EAeob#:EArcn:sequence#:'I');

           // Update Export Process Flag
           UpdEAexPR(EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:EArcn);

           // Only payment record if not Zero EOB
           If EApaid <> 0;
              wcmsGroup = GetWcmsGrp(EAco#:EAsub#:EAfnd);
              UpdPAexPR(EAco#:wcmsGroup:EAfYr:EAcase:sequence#);
           EndIf;

           // Fetch next Record
           Exec SQL fetch next from eobcursor into :dsFAEAP;

           // check to see if there was another record fetched.
           If sqlstt <> '00000';
              moreRows = *Off;
              Exec SQL close eobcursor;
           EndIf;
        EndDo;
       //=======================================================================
       //  Find Voids or Stops
       //=======================================================================
       // Reset Rows Indicator
       moreRows = *On;

       // Build eob sql statement
       sqlStmtStr = 'Select * from WDPAP where PAfnd = ' + inChrGrp + ' ' +
                    'and PAexPr <> ''E'' And PAtype in (''V'', ''S'') And ' +
                    'PAeob# <> 0';

       // Prepare the SQL statement
       Exec SQL prepare voidStmt from :sqlStmtStr;

       // Declare the SQL cursor
       Exec SQL declare voidCursor cursor for voidStmt;

       // Run SQL
       Exec SQL open voidCursor;

       // Get first record if there is one
       Exec SQL Fetch Next From voidCursor Into :dsWDPAP;

       // Check to see if any records were fetched
       If sqlSTT <> '00000';
          moreRows = *Off;
          Exec SQL close voidCursor;
       EndIf;

       // If row was fetched then process
       DoW moreRows = *On;
          // Check to see if void/stop is for the main provider payment
          subscriber# = GetSubscriber(PAco#:PAfnd:wcmcGroup);
          eobRcn# = PAeobS;
          Chain (PAco#:subscriber#:wcmcGroup:PAfYr:PAcase:PAeob#:eobRcn#) pea01;
          If %Found(faeal1) And EAfein=PAfein And EAmod=PAmod And
             EAwcpc=PAchk# And EApaid = PAamt;

             // Write to the Export File
             WrtFXEBEXT(PAco#:subscriber#:PAfnd:PAfYr:PAcase:
                PAeob#:eobRcn#:PAseq#:PAtype);

             // Update Export Process Flag
             UpdPAexPR(PAco#:PAfnd:PAfYr:PAcase:PAseq#);
          EndIf;

          // Fetch next Record
          Exec SQL fetch next from voidCursor into :dsWDPAP;

          // check to see if there was another record fetched.
          If sqlstt <> '00000';
             moreRows = *off;
             Exec SQL close voidCursor;
          EndIf;
       EndDo;

       //=======================================================================
       //  Find Reissues
       //=======================================================================
       // Reset Rows Indicator
       moreRows = *On;

       // Build eob sql statement
       sqlStmtStr = 'Select * from WDPAP where PAfnd = ' + inChrGrp + ' ' +
                    'and PAexPr <> ''E'' And PAtype = ''M'' And PAeob# <> 0';

       // Prepare the SQL statement
       Exec SQL prepare reissueStmt from :sqlStmtStr;

       // Declare the SQL cursor
       Exec SQL declare reissueCursor cursor for reissueStmt;

       // Run SQL
       Exec SQL open reissueCursor;

       // Get first record if there is one
       Exec SQL Fetch Next From reissueCursor Into :dsWDPAP;

       // Check to see if any records were fetched
       If sqlSTT <> '00000';
          moreRows = *off;
          Exec SQL close reissueCursor;
       EndIf;

       // If row was fetched then process
       DoW moreRows = *On;
          // Check to see if reissue is for the main provider payment
          subscriber# = GetSubscriber(PAco#:PAfnd:wcmcGroup);
          eobRcn# = PAeobS;
          Chain (PAco#:subscriber#:wcmcGroup:PAfYr:PAcase:PAeob#:eobRcn#) pea01;
          If %Found(faeal1) And EAfein=PAfein And EAmod=PAmod And
             EApaid = PAamt;

             If voidStop(PAco#:PAfnd:PAfYr:PAcase:PAeob#:PAeobs:PAamt:PAfein:
                         PAmod);

                // Write to the Export File
                WrtFXEBEXT(PAco#:subscriber#:PAfnd:PAfYr:PAcase:
                   PAeob#:eobRcn#:PAseq#:'R');

                // Update Export Process Flag
                UpdPAexPR(PAco#:PAfnd:PAfYr:PAcase:PAseq#);
             EndIf;
          EndIf;

          // Fetch next Record
          Exec SQL fetch next from reissueCursor into :dsWDPAP;

          // check to see if there was another record fetched.
          If sqlstt <> '00000';
             moreRows = *off;
             Exec SQL close reissueCursor;
          EndIf;
       EndDo;

       // End of Program Processes
       ProgramEnd();

       //===================================================
       // Initialization Routine
       //===================================================
       Begsr *InzSR;

          // Initialize number of input date fields . . .
          dsDttm = %Dec(%Timestamp());
          dsDate = %Dec(%Subst(%EditC(dsDate:'X'):5:4) +
                   %Subst(%EditC(dsDate:'X'):1:4):8:0);

          // Lets get the User Parameters
          Chain q1User pup00;
          If Not %Found(smupl);
             UPco# = 777;
          EndIf;

          // Lets get the Company Name
          Chain UPco# pco00;
          If %Found(smcol);
             chrData = COname;
             intLength = 40;
             SRCenter(chrData:intLength);
             COname = chrData;
          EndIf;

       EndSR;

      /End-Free
     OQSYSPRT   E            SUMMARY        1 06
     O                       dsTime               8 '  :  :  '
     O                       COname              86
     O                       dsDate             132 '  /  /    '
     O          E            summary        2
     O                       Q1pgm               10
     O                       title               83
     O                       Q1user             120
     O                                          127 'Page'
     O                       page          Z    132
     O          E            summary        2
    O*
     O                                           67 'Report Date ='
     O                       dsDate              78 '0  /  /    '
     O          E            summary        1
    O*
     O                                           67 'Group ='
     O                       FNfnd         4   +001
     O                                         +001 '-'
     O                       FNshNm            +001
    O*
     O          E            summary        2
     O                                           25 'Total EOBs Processed    :'
     O                       totalEOB          +001 '       0 '
     O          E            summary        1
     O                                           25 'Total Voids Processed   :'
     O                       totalVoid         +001 '       0 '
     O          E            summary        1
     O                                           25 'Total Stops Processed   :'
     O                       totalStop         +001 '       0 '
     O          E            summary        1
     O                                           25 'Total Reissues Processed:'
     O                       totalReissue      +001 '       0 '
     O          E            summary        1
     O                       line              +000
     O          E            summary        1
     O                                           25 'Grand Total             :'
     O                       totalAll          +001 '       0 '
    O*
       //===================================================================
       // BldChkComment - Build the Check Comment
       //===================================================================
     P BldChkComment   B
     D BldChkComment   PI           300
     D   inCompany                    3  0
     D   inRemarkDsc                 30
     D   inRemarkCd1                  2
     D   inRemarkCd2                  2
     D   inRemarkCd3                  2

     D   comment       s            300    Inz(' ')
     D   remarkDsc1    s             40    Inz(' ')
     D   remarkDsc2    s             40    Inz(' ')
     D   remarkDsc3    s             40    Inz(' ')
      /free

       // Get Remark Code 1s Description
       If inRemarkCd1 <> ' ';
          tableKey = inRemarkCd1;
          Chain (inCompany:tableKey) prc00;
          If %Found(wtabl);
             remarkDsc1 = RCdesc;
          EndIf;
       EndIf;

       // Get Remark Code 2s Description
       If inRemarkCd2 <> ' ';
          tableKey = inRemarkCd2;
          Chain (inCompany:tableKey) prc00;
          If %Found(wtabl);
             remarkDsc2 = RCdesc;
          EndIf;
       EndIf;

       // Get Remark Code 3s Description
       If inRemarkCd3 <> ' ';
          tableKey = inRemarkCd3;
          Chain (inCompany:tableKey) prc00;
          If %Found(wtabl);
             remarkDsc3 = RCdesc;
          EndIf;
       EndIf;

       If remarkDsc1 <> ' ';
          comment = %Trim(remarkDsc1);
       EndIf;

       If comment <> ' ';
          comment = %Trim(comment) + ' ' + %Trim(remarkDsc2);
       Else;
          comment = %Trim(remarkDsc2);
       EndIf;

       If comment <> ' ';
          comment = %Trim(comment) + ' ' + %Trim(remarkDsc3);
       Else;
          comment = %Trim(remarkDsc3);
       EndIf;

       If comment <> ' ';
          comment = %Trim(comment) + ' ' + %Trim(inRemarkDsc);
       Else;
          comment = %Trim(inRemarkDsc);
       EndIf;

       Return comment;

      /End-Free

     P BldChkComment   E
       //===================================================================
       // GetPmtSeq# - Get the Payments Sequence #
       //===================================================================
     P GetPmtSeq#      B
     D GetPmtSeq#      PI             5  0
     D   inCompany                    3  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEob#                       9  0
     D   inEobs                       7  0
     D   inChk#                       7  0
     D   inAmount                    11  2

     D   seq#          s              5  0 Inz(-1)
      /free

       Chain (inCompany:inGroup:inYear:inCase:inEob#:inEobS) ppa0g;
       If %Found(wdpalg);
          DoU %Eof(wdpalg);
             If P2chk# = inChk# And P2amt=inAmount And P2type = 'P';
                seq# = P2seq#;
                Leave;
             EndIf;
             ReadE (inCompany:inGroup:inYear:inCase:inEob#:inEobS) ppa0g;
          EndDo;
       EndIf;

       Return seq#;

      /End-Free

     P GetPmtSeq#      E
       //===================================================================
       // GetSubscriber - Get the WCMC Subscriber/Group #
       //===================================================================
     P GetSubscriber   B
     D GetSubscriber   PI             5  0
     D  inCompany#                    3  0
     D  inWCMSgrp                     3  0
     D  outWCMCgrp                    3  0
      /free

       Chain (inCompany#:inWCMSgrp) pfn01;
       If %Found(fmfnl1);
          outWCMCgrp = FNfnd;
          Return FNsub#;
       Else;
          outWCMCgrp = 0;
          Return 0;
       EndIf;

      /End-Free
     P GetSubscriber   E
       //===================================================================
       // GetWCMSgrp - Get the WCMS Group #
       //===================================================================
     P GetWcmsGrp      B
     D GetWcmsGrp      PI             3  0
     D  inCompany#                    3  0
     D  inSub#                        5  0
     D  inWCMCgrp                     3  0
      /free

       Chain (inCompany#:inSub#:inWCMCgrp) pfn00;
       If %Found(fmfnl);
          Return FNwcf#;
       Else;
          Return 0;
       EndIf;

      /End-Free
     P GetWcmsGrp      E
       //===================================================================
       // ProgramEnd - Program End Processing
       //===================================================================
     P ProgramEnd      B
     D ProgramEnd      PI

      /free

       // Print Summary Report
       Except summary;

       *inLR = *On;

      /End-Free
     P ProgramEnd      E
       //===================================================================
       // UpdEAEXPR - Update the EOB Extract Process Flag
       //===================================================================
     P UpdEAEXPR       B
     D UpdEAEXPR       PI
     D   inCompany                    3  0
     D   inSubscriber                 5  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEOB#                       9  0
     D   inRecon                      3  0

      /Free

       Chain (inCompany:inSubscriber:inGroup:inYear:
              inCase:inEOB#:inRecon) pea01;
       If %Found(faeal1);
          EAexPr = 'E';

          // Maintenance Fields
          EAuser = q1User;
          EApgm = q1pgm;
          EAtime = %Dec(%Char(%Time():*hms0):6:0);
          EAdt   = %Int(%Char(%Date():*cymd0));

          Update pea01;
       EndIf;
    
      /End-Free
     P UpdEAEXPR       E
       //===================================================================
       // UpdPAEXPR - Update the Payment Extract Process Flag
       //===================================================================
     P UpdPAEXPR       B
     D UpdPAEXPR       PI
     D   inCompany                    3  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inSeq#                       5  0

      /Free

       Chain (inCompany:inGroup:inYear:inCase:inSeq#) ppa00;
       If %Found(wdpal);
          PAexPr = 'E';

          // Maintenance Fields
          PAuser = q1User;
          PApgm = q1pgm;
          PAtime = %Dec(%Char(%Time():*hms0):6:0);
          PAdt   = %Int(%Char(%Date():*cymd0));

          Update ppa00;
       EndIf;
    
      /End-Free
     P UpdPAEXPR       E
       //===================================================================
       //
       //===================================================================
     P VoidStop        B
     D VoidStop        PI              n
     D   inCompany                    3  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEob#                       9  0
     D   inEobS                       7  0
     D   inAmt                        9  2
     D   inFein                       9  0
     D   inMod                        3  0

     D   voidStopped   s               n   Inz(*Off)
      /Free

       Chain (inCompany:inGroup:inYear:inCase:inEob#:inEobS) ppa0g;
       If %Found(wdpalg);
          DoU %Eof(wdpalg);
             If (P2type = 'S' Or P2type = 'V') And P2fein = inFein And
                P2mod = inMod And P2Amt = inAmt;
                voidStopped = *On;
                Leave;
             EndIf;
             ReadE (inCompany:inGroup:inYear:inCase:inEob#:inEobS) ppa0g;
          EndDo;
       EndIf;

       Return voidStopped;
    
      /End-Free
     P VoidStop        E
       //===================================================================
       // WrtFXEBEXT - Write EOB Extract Process File (FXEBEXT)
       //===================================================================
     P WrtFXEBEXT      B
     D WrtFXEBEXT      PI
     D   inCompany                    3  0
     D   inSub#                       5  0
     D   inGroup                      3  0
     D   inYear                       3  0
     D   inCase                       7  0
     D   inEob#                       9  0
     D   inRcn                        3  0
     D   inSeq#                       5  0
     D   inType                       1    Const

     D   prvRecon      s              3  0
      /Free

          // Access Claim file
          Chain (inCompany:inGroup:inYear:inCase) pcm01;

          // If a Non-Zero EOB then retrieve the payment record;
          If inType = 'I' and inSeq# <> -1;
             Chain(n) (inCompany:inGroup:inYear:inCase:inSeq#) ppa00;
          EndIf;

          Clear peb0p;

          //*** Begin Chg *** 07/21/17 ******************************
          //**********EBclm#    = CMpds#;
          pos = %Scan('-':CMpds#);
          If pos <> 0;
             EBclm#    = %Subst(CMpds#:1:pos-1);
             EBclmSeq# = %Subst(CMpds#:pos+1:2);
          Else;
             EBclm#    = CMpds#;
             EBclmSeq# = '01';
          EndIf;
          //*** End   Chg *** 07/21/17 ******************************
          EBclmLsn  = CMlsn;
          EBclmFsn  = CMfsn;
          EBclmMid  = CMmid;
          EBss#     = CMss#;

          Monitor;
             EBacDt    = %Char(%Date(CMacDt:*cymd):*usa);
          On-Error BadDateData;
             EBacDt    = ' ';
          EndMon;

          EBeob#    = %EditC(inEob#:'X') + '-' + %EditC(inRcn:'X');
          EBcltName = F2shNm;

          If FNmAd1 <> ' ';
             EBcltAdr1 = F2mAd1;
             EBcltAdr2 = F2mAd2;
             EBcltCity = F2mCty;
             EBcltSt   = F2mSt;
             EBcltZip  = %EditC(F2mZip:'X');
          Else;
             EBcltAdr1 = F2pAd1;
             EBcltAdr2 = F2pAd2;
             EBcltCity = F2pCty;
             EBcltSt   = F2pSt;
             EBcltZip  = %EditC(F2pZip:'X');
          EndIf;

          EBchkSts  = inType;
          EBahClm#  = %EditC(EAfnd:'X') + %EditC(EAfYr:'X') +
                      %EditC(EAcase:'X');
          EBeor     = 'EOR';

          // Not Zero Eob (Has a payment record)
          If inSeq# <> -1;

             // Get Payment Demographic Information
             Chain (inCompany:inGroup:inYear:inCase:inSeq#) prp03;
             If %Found(wdrpl3);
                // Build the comments
                chkComment = BldChkComment(RPco#:RPrmds:RPrmk1:RPrmk2:RPrmk3);
                // If Void or Stop then get Reason
                If inType = 'S' Or inType = 'V';
                   EBvsReas  = chkComment;
                Else;
                   EBchkComm = chkComment;
                EndIf;
             EndIf;

             // Remaining payment information

             tableId  = 'PVXR';
             tableKey = PAbr2;
             Chain (PAco#:tableID:tableKey) pgt00;
             If %Found(wtgtl);
                EBpmtKind = GTcf01;
             EndIf;

             EBpaidTo  = EApNam;
             EBmAdr1   = EAadr1;
             EBmAdr2   = EAadr2;
             EBmCity   = EActy;
             EBmSt     = EAst;
             EBmZip    = %EditC(EAzip:'X');
             EBchk#    = %Trim(%EditC(PAchk#:'4'));

             Monitor;
                EBchkDt   = %Char(%Date(PAtrDt:*cymd):*usa);
             On-Error BadDateData;
                EBchkDt   = ' ';
             EndMon;

             EBchkAmt  = PAamt;
          Endif;

          // Process eobs
          If inType = 'I';
             // Header Records

             // Get the EOB Work File
             Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) peo04;

             // Access EOB Header Files
             Select;
             When EObTyp = 'D'; // Dental
                Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) ped00;
                If %Found(faedl);
                   If EDicd1001 = ' ';
                      Chain EDdia1 pdi00;
                      If %Found(fmdil);
                         EBdiag1 = DIds1;
                      EndIf;
                   Else;
                      Chain EDicd1001 pic01;
                      If %Found(fmicd10l1);
                         EBdiag1 = ICfCode;
                      EndIf;
                   EndIf;
                   If EDicd1002 = ' ';
                      Chain EDdia2 pdi00;
                      If %Found(fmdil);
                         EBdiag2 = DIds1;
                      EndIf;
                   Else;
                      Chain EDicd1002 pic01;
                      If %Found(fmicd10l1);
                         EBdiag2 = ICfCode;
                      EndIf;
                   EndIf;
                   If EDicd1003 = ' ';
                      Chain EDdia3 pdi00;
                      If %Found(fmdil);
                         EBdiag3 = DIds1;
                      EndIf;
                   Else;
                      Chain EDicd1003 pic01;
                      If %Found(fmicd10l1);
                         EBdiag3 = ICfCode;
                      EndIf;
                   EndIf;
                   If EDicd1004 = ' ';
                      Chain EDdia4 pdi00;
                      If %Found(fmdil);
                         EBdiag4 = DIds1;
                      EndIf;
                   Else;
                      Chain EDicd1004 pic01;
                      If %Found(fmicd10l1);
                         EBdiag4 = ICfCode;
                      EndIf;
                   EndIf;
                   EBinv#    = EDacct;
                EndIf;
             When EObTyp = 'H'; // Hospital
                Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) peu00;
                If %Found(faeul);
                   If EUicd1001 = ' ';
                      Chain EUdia1 pdi00;
                      If %Found(fmdil);
                         EBdiag1 = DIds1;
                      EndIf;
                   Else;
                      Chain EUicd1001 pic01;
                      If %Found(fmicd10l1);
                         EBdiag1 = ICfCode;
                      EndIf;
                   EndIf;
                   If EUicd1002 = ' ';
                      Chain EUdia2 pdi00;
                      If %Found(fmdil);
                         EBdiag2 = DIds1;
                      EndIf;
                   Else;
                      Chain EUicd1002 pic01;
                      If %Found(fmicd10l1);
                         EBdiag2 = ICfCode;
                      EndIf;
                   EndIf;
                   If EUicd1003 = ' ';
                      Chain EUdia3 pdi00;
                      If %Found(fmdil);
                         EBdiag3 = DIds1;
                      EndIf;
                   Else;
                      Chain EUicd1003 pic01;
                      If %Found(fmicd10l1);
                         EBdiag3 = ICfCode;
                      EndIf;
                   EndIf;
                   If EUicd1004 = ' ';
                      Chain EUdia4 pdi00;
                      If %Found(fmdil);
                         EBdiag4 = DIds1;
                      EndIf;
                   Else;
                      Chain EUicd1004 pic01;
                      If %Found(fmicd10l1);
                         EBdiag4 = ICfCode;
                      EndIf;
                   EndIf;
                   EBinv#    = EUacct;
                EndIf;
             When EObTyp = 'N'; // Physician
                Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) peh00;
                If %Found(faehl);
                   If EHicd1001 = ' ';
                      Chain EHdia1 pdi00;
                      If %Found(fmdil);
                         EBdiag1 = DIds1;
                      EndIf;
                   Else;
                      Chain EHicd1001 pic01;
                      If %Found(fmicd10l1);
                         EBdiag1 = ICfCode;
                      EndIf;
                   EndIf;
                   If EHicd1002 = ' ';
                      Chain EHdia2 pdi00;
                      If %Found(fmdil);
                         EBdiag2 = DIds1;
                      EndIf;
                   Else;
                      Chain EHicd1002 pic01;
                      If %Found(fmicd10l1);
                         EBdiag2 = ICfCode;
                      EndIf;
                   EndIf;
                   If EHicd1003 = ' ';
                      Chain EHdia3 pdi00;
                      If %Found(fmdil);
                         EBdiag3 = DIds1;
                      EndIf;
                   Else;
                      Chain EHicd1003 pic01;
                      If %Found(fmicd10l1);
                         EBdiag3 = ICfCode;
                      EndIf;
                   EndIf;
                   If EHicd1004 = ' ';
                      Chain EHdia4 pdi00;
                      If %Found(fmdil);
                         EBdiag4 = DIds1;
                      EndIf;
                   Else;
                      Chain EHicd1004 pic01;
                      If %Found(fmicd10l1);
                         EBdiag4 = ICfCode;
                      EndIf;
                   EndIf;
                   EBinv#    = EHacct;
                EndIf;
             When EObTyp = 'X'; // Pharmacy
                Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) pep00;
                If %Found(faepl);
                   EBinv#    = EPacct;
                EndIf;
             EndSl;

             Monitor;
                EBrcvDt   = %Char(%Date(EOcrDt:*cymd):*usa);
             On-Error BadDateData;
                EBrcvDt   = ' ';
             EndMon;

             EBadjID   = EOuser;

             // Get Network Name
             Chain (EAco#:EOntwk) pnw00;
             If %Found(nmnwl);
                EBppoID = NWname;
                EBnetSvCd = NWfl02;
                EBnetName = NWname;
             EndIf;

             Monitor;
                EBdtBill  = %Char(%Date(EOcrDt:*cymd):*usa);
             On-Error BadDateData;
                EBdtBill  = ' ';
             EndMon;

             Monitor;
                EBrcvDt1  = %Char(%Date(EOcrDt:*cymd):*usa);
             On-Error BadDateData;
                EBrcvDt1  = ' ';
             EndMon;

             If EAwcPD <> 0;
                Monitor;
                   EBpaidDt  = %Char(%Date(EAwcPd:*iso):*usa);
                On-Error BadDateData;
                   EBpaidDt  = ' ';
                EndMon;
             EndIf;

             EBprovID  = %EditC(EAfein:'X') + '-' + %EditC(EAmod:'X');
             EBprvFein = %EditC(EAfein:'X');

             Chain (EAfein:EAmod) ppd00;
             If %Found(fmpdl);
                EBprvLsn  = %Subst(PDname:1:30);
                EBprvFsn  = PDfsn;
                EBprvMid  = ' ';
                EBprvAdr1 = PDadr1;
                EBprvAdr2 = PDadr2;
                EBprvCity = PDcty;
                EBprvSt   = PDst;
                EBprvZip  = %Editc(PDzip:'X');
                EBtaxCd   = PDtaxO;
             EndIf;

             tableKey = EOspCd;
             Chain (EAco#:tableKey) psp01;
             If %Found(ftspl1);
                EBprvType = %Subst(SPdesc:1:30);
             EndIf;

             EBrvwSt   = EOaSt;

             Monitor;
                EBinvDt   = %Char(%Date(EOcsDt:*cymd):*usa);
             On-Error BadDateData;
                EBinvDt   = ' ';
             EndMon;

             EBhdrChrg = 0;
             EBhdrPaid = 0;
             EBhdrMRA  = 0;
             EBhdrNDIS = 0;
             EBhdrOthr = 0;
             Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) pea02;
             If %Found(faeal2);
                DoU %Eof(faeal2);
                   If E2rcn <= EArcn;
                      EBhdrChrg += E2chrg;
                      EBhdrPaid += E2paid;
                      EBhdrMRA  += E2mra;
                      EBhdrNDIS += E2ndis;
                      EBhdrOthr += E2othr + E2oth2;
                   EndIf;
                   ReadE (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) pea02;
                EndDo;
             EndIf;

             EBbillFee = EAbAmt;
             EBnetFee  = EAnAmt;
             EBothFee  = EAoca1;

             If EArcn > 0;
                prvRecon = EArcn - 1;
                EBprvBill = %EditC(EAeob#:'X') + '-' + %EditC(prvRecon:'X');
                EBreconF  = 'R';
             EndIf;

             EBcarSeq# = ' ';
             EBbillDed = 0;
             EBbillCop = 0;

             If EApaid = 0;
                Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) pee00;
                If %Found(faeel);
                   DoU %Eof(faeel);
                      EB0Reas   = %Trim(EB0Reas) + %TRim(EEnot1) + ' ' +
                         %TRim(EEnot2) + ' ' + %TRim(EEnot3) + ' ' +
                         %TRim(EEnot4);
                      ReadE (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) pee00;
                   EndDo;
                EndIf;
             EndIf;

             // Get EOB Detail Information
             Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) pca19;
             If %Found(fdcal19);
                DoU %Eof(fdcal19);
                   // Access EOB Detail Files
                   Select;
                   When EObTyp = 'D'; // Dental
                      Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:
                             CAdSeq) pdd00;
                      If %Found(fdddl);
                         EBprcCode = DDprCd;
                         EBpos     = %Trim(DDpos);
                         EBcptDesc = DDprDs;
                      EndIf;
                   When EObTyp = 'H'; // Hospital
                      Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:
                             CAdSeq) pdu00;
                      If %Found(fddul);
                         EBprcCode = %EditC(DUrvCd:'X');
                         EBmod1    = DUpMod;
                         EBmod2    = DUmod2;
                         EBpos     = %Trim(DUpos);
                         EBcptDesc = DUdesc;
                         EBhcpcs   = DUrate;

                         If EOaSt = 'PA';
                            EBdrcCd   = DUrate;
                         Else;
                            EBdrcCd   = %EditC(EUdrg#:'X');
                         EndIf;

                      EndIf;
                   When EObTyp = 'N'; // Physician
                      Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:
                             CAdSeq) pdh00;
                      If %Found(fddhl);
                         EBprcCode = DHprCd;
                         EBmod1    = DHmod1;
                         EBmod2    = DHmod2;
                         EBpos     = %Trim(DHpos);
                         EBtos     = DHtos;
                         EBcptDesc = DHprDs;
                      EndIf;
                   When EObTyp = 'X'; // Pharmacy
                      Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:
                             CAdSeq) pdp00;
                      If %Found(fddpl);
                         EBprcCode = DPndc;
                         EBmod1    = ' ';
                         EBmod2    = ' ';
                         EBpos     = ' ';
                         EBtos     = ' ';
                         EBcptDesc = DPdesc;
                      EndIf;
                   EndSl;

                   Monitor;
                      EBsvcDt   = %Char(%Date(CAfDat:*cymd):*usa);
                   On-Error BadDateData;
                      EBsvcDt   = ' ';
                   EndMon;

                   EBunits   = CAunts;
                   EBrvwCod1 = ' ';
                   EBrvwAmt1 = 0;
                   EBrvwDsc1 = ' ';
                   EBrvwCod2 = ' ';
                   EBrvwAmt2 = 0;
                   EBrvwDsc2 = ' ';
                   EBrvwCod3 = ' ';
                   EBrvwAmt3 = 0;
                   EBrvwDsc3 = ' ';
                   EBrvwCod4 = ' ';
                   EBrvwAmt4 = 0;
                   EBrvwDsc4 = ' ';

                   EBdtlChrg = 0;
                   EBdtlPaid = 0;
                   EBdtlMRA  = 0;
                   EBdtlNDIS = 0;
                   EBdtlOthr = 0;
                   Chain (CAco#:CAsub#:CAfnd:CAfYr:CAcase:CAeob#:CAdSeq) pda02;
                   If %Found(fddal2);
                      DoU %Eof(fddal2);
                         If D2rcn# <= EArcn;
                            EBdtlChrg += D2chrg;
                            EBdtlPaid += D2paid;
                            EBdtlMRA  += D2mra;
                            EBdtlNDIS += D2ndis;
                            EBdtlOthr += D2othr + D2oth2;
                         EndIf;
                         ReadE (CAco#:CAsub#:CAfnd:CAfYr:CAcase:CAeob#:
                                CAdSeq) pda02;
                      EndDo;
                   EndIf;

                   // Try to get recon detail amounts
                   Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:
                          EArcn:CAdSeq) pda04;
                   If %Found(fddal4);

                      dtlComm# = 1;
                      Chain (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:
                             EArcn:CAdSeq) pde03;
                      If %Found(fddel3);
                         DoU %Eof(fddel3) Or dtlComm# > 4;
                            If DEeob <> ' ';
                               Select;
                               When dtlComm# = 1;
                                  EBrvwCod1 = DEeob;
                                  EBrvwAmt1 = EBdtlMRA + EBdtlNDIS + EBdtlOthr;
                                  EBrvwDsc1 = %Trim(DEnot1) + ' ' +
                                              %Trim(DEnot2);
                               When dtlComm# = 2;
                                  EBrvwCod2 = DEeob;
                                  EBrvwAmt1 = EBdtlMRA + EBdtlNDIS + EBdtlOthr;
                                  EBrvwDsc2 = %Trim(DEnot1) + ' ' +
                                              %Trim(DEnot2);
                               When dtlComm# = 3;
                                  EBrvwCod3 = DEeob;
                                  EBrvwAmt1 = EBdtlMRA + EBdtlNDIS + EBdtlOthr;
                                  EBrvwDsc3 = %Trim(DEnot1) + ' ' +
                                              %Trim(DEnot2);
                               When dtlComm# = 4;
                                  EBrvwCod4 = DEeob;
                                  EBrvwAmt1 = EBdtlMRA + EBdtlNDIS + EBdtlOthr;
                                  EBrvwDsc4 = %Trim(DEnot1) + ' ' +
                                              %Trim(DEnot2);
                               EndSl;
                            EndIf;
                            ReadE (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#:
                                   EARCN:CAdSeq) pde03;
                            dtlComm# += 1;
                         EndDo;
                      EndIf;
                   EndIf;

                   EBtrCd    = '01';
                   EBprocCd2 = ' ';

                   Monitor;
                      EBfDat    = %Char(%Date(CAfDat:*cymd):*usa);
                   On-Error BadDateData;
                      EBfDat    = ' ';
                   EndMon;

                   Monitor;
                      EBtDat    = %Char(%Date(CAtDat:*cymd):*usa);
                   On-Error BadDateData;
                      EBtDaT    = ' ';
                   EndMon;

                   EBdeduct  = 0;
                   EBcoPay   = 0;

                   Write peb0p;
                   ReadE (EAco#:EAsub#:EAfnd:EAfYr:EAcase:EAeob#) pca19;
                EndDo;
             // No EOB Detail Lines
             Else;
                Write peb0p;
             EndIf;
          // Write Stop/Void/Reissues
          Else;
             Write peb0p;
          EndIf;

          Select;
          When inType = 'I';
             totalEOB += 1;
          When inType = 'S';
             totalStop += 1;
          When inType = 'V';
             totalVoid += 1;
          When inType = 'R';
             totalReissue += 1;
          EndSl;

          totalAll += 1;

      /End-Free
     P WrtFXEBEXT      E
