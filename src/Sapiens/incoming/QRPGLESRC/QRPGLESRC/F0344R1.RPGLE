     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      //****************************************************************
     H/TITLE F0344R1 - Eft Eomb WorkFile Build                        *
      //****************************************************************
      // Created on 05/27/2015 by c:62 for Project 114MRH             *
      //****************************************************************
      // Program Description:                                          *
      //                                                               *
      // Called From:  F0344C                                          *
      // Calls To:     none                                            *
      //                                                               *
      //****************************************************************
      // Revisions:                                                    *
      //                                                               *
      //****************************************************************
     ffaeal1    uf   e           k Disk
     ffyeap     o    e           k Disk    Rename(pea01:pea0p)
     ffmf1l     if   e           k Disk    Rename(pf101:pf100)
     fwmnrl     if   e           k Disk    Rename(pnr01:pnr00)
     fwdpalg    if   e           k Disk    Rename(ppa01:ppa0g)

     d                 DS
     d  dsDTTM                 1     14  0
     d  dsTime                 1      6  0
     d  dsDate                 7     14  0

     d ##wcif          s              1                                         1
     d ##mcif          s              1                                         2
     d ##wcin          s              1                                         3
     d ##ar            s              1                                         4
     d ##ntwk          s              1                                         5
     d ##ur            s              1                                         6
     d ##med           s              1                                         7
     d ##edi           s              1                                         8
     d ##wcdk          s              1                                         9
     d ##rhab          s              1                                         10
     d ##mss           s              1                                         11
     d ##fsch          s              1                                         12
     d ##prvb          s              1                                         13
     d ##npwc          s              1                                         14
     d ##os            s              1                                         15
     d ##qrls          s              1                                         16
     d ##ccod          s              1                                         17
     d ##cchk          s              1                                         18
     d ##prom          s              1                                         19
     d ##mcrv          s              1                                         20
     d ##freq          s              1                                         21
     d ##crac          s              1                                         22
     d ##preo          s              1                                         23
     d ##parn          s              1                                         24
     d ##mgri          s              1                                         25
     d ##kcif          s              1                                         26
     d ##curs          s              1                                         27
     d ##pcib          s              1                                         28
     d ##clt#          s              3                                         29
     d ##ucpf          s              1                                         30
     d ##dcf           s              1                                         31
     d ##autb          s              1                                         32
     d ##deif          s              1                                         33
     d ##aure          s              1                                         34
     d ##cred          s              1                                         35
     d ##idrt          s              1                                         36
     d ##cprv          s              1                                         37
     d ##eft           s              1                                         38
     d ##govr          s              1                                         39
     d ##ncdc          s              1                                         40
     d ##splt          s              1                                         41
     d ##last          s              1                                         42
     d ##pblt          s              1                                         43
     d ##inze          s              1                                         44

     d inCompany#      s              3
     d inType          s              1
     d company#        s                   Like(EAco#)
     d subscriber#     s                   Like(EAsub#)
     d group#          s                   Like(EAfnd)
     d outsideAcct     s                   Like(F1outA)
     d clientID        s              3

     d psds          esds                  ExtName(SZQ1P)

     d GetSystemFlags  pr                  ExtPgm('SA001R')
     d ##Wcif                         1                                         WC Inst
     d ##Mcif                         1                                         MC Inst
     d ##Wcin                         1                                         WC Interface
     d ##ar                           1                                         MC AR Inst
     d ##ntwk                         1                                         Network Inst
     d ##ur                           1                                         Precert Inst
     d ##med                          1                                         MedMgmt Inst
     d ##edi                          1                                         Edi Inst
     d ##wcdk                         1                                         WC Darts Key
     d ##rhab                         1                                         Rehab Inst
     d ##mss                          1                                         MS Support
     d ##fsch                         1                                         Fee Schedule
     d ##prvb                         1                                         Provider Bills
     d ##npwc                         1                                         Non-PDS WC LS
     d ##os                           1                                         Outcome/Savings
     d ##qrls                         1                                         Auto Release Flag
     d ##ccod                         1                                         Correct Coding Flag
     d ##cchk                         1                                         Claim Check Flag
     d ##prom                         1                                         ProMatch Flag
     d ##mcrv                         1                                         Medicare CR Flag
     d ##freq                         1                                         Frequency Flag
     d ##crac                         1                                         CR Auto Complete
     d ##preo                         1                                         CR Auto Complete
     d ##parn                         1                                         Par/Non-Par/Both
     d ##mgri                         1                                         Multi Group Restr
     d ##kcif                         1                                         Keep Claimant info
     d ##curs                         1                                         PBILLS entry cursor
     d ##pcib                         1                                         Prov Check in Batch
     d ##clt#                         3                                         Client Number
     d ##ucpf                         1                                         UCR Percentile
     d ##dcf                          1                                         Data Center
     d ##autb                         1                                         Auto Batch Flag
     d ##deif                         1                                         DE/Image Flag
     d ##aure                         1                                         Auto-Reinstate
     d ##cred                         1                                         Credit Module
     d ##idrt                         1                                         Idarts Module
     d ##cprv                         1                                         Create Provider
     d ##eft                          1                                         EFT Flag
     d ##govr                         1                                         GO Version
     d ##ncdc                         1                                         NCCI data
     d ##splt                         1                                         Split
     d ##last                         1                                         LA Parish Sales Tax
     d ##pblt                         1                                         Provider bill light
     d ##inze                         1                                         Include $0 EOB

     d RetrieveCltID   pr                  ExtPgm('RTVCLTID')
     d clientID                       3                                         WC Inst

     d* Determine if Check Issued
     d CheckIssued     pr              n

     d* Determine if Provider is Correct Group Type
     d CorrectGrpType  pr              n

     c* Entry Parameters
     c     *Entry        Plist
     c                   Parm                    inCompany#
     c                   Parm                    inType

      /free

       // Retieve Client ID
       RetrieveCltID(clientID);

       // Get System Flags
       GetSystemFlags(##wcif:##mcif:##wcin:##ar:##ntwk:##ur:##med:##edi:##wcdk:
                      ##rhab:##mss:##fsch:##prvb:##npwc:##os:##qrls:##ccod:
                      ##cchk:##prom:##mcrv:##freq:##crac:##preo:##parn:##mgri:
                      ##kcif:##curs:##pcib:##clt#:##ucpf:##dcf:##autb:##deif:
                      ##aure:##cred:##idrt:##cprv:##eft:##govr:##ncdc:##splt:
                      ##last:##pblt:##inze);

       // Initialize Fields
       company# = %Dec(inCompany#:3:0);
       subscriber# = 0;
       group# = 0;

       // Read Eob File
       SetLL company# pea01;
       ReadE company# pea01;
       Dow Not %Eof(FAEAL1);

          // Determine if this is an outside account (Not linked to WCMS)
          If EAsub# <> subscriber# Or EAfnd <> group#;
             subscriber# = EAsub#;
             group# = EAfnd;

             Chain (company#:subscriber#:group#) pf100;
             If %Found(FMF1L);
                outsideAcct = F1outA;
             EndIf;
          EndIf;

          If outsideAcct <> 'Y';
             If CorrectGrpType() = *On;
                If EApaid <> 0;
                   // Check and see if check issued
                   If CheckIssued() = *On;
                      Write pea0p;
                   EndIf;
                Else;
                   If ##inze = 'Y';
                      Write pea0p;
                   EndIf;
                EndIf;
             EndIf;
          EndIf;

          ReadE company# pea01;
       EndDo;                                                                 //*IN50 DOWEQ

       *InLR = *ON;

      /end-free
       //===================================================================
       // CheckIssued - Was the check issued?
       //===================================================================
     P CheckIssued     B
     D CheckIssued     PI              n

     d GroupTranslate  pr                  ExtPgm('FA700R')
     d  returnCode                    1                                         Return Code
     d  strCompany                    3                                         Company #
     d  strWCMSGroup                  3                                         WCMS Group #
     d  strSubscriber                 5                                         Subscriber #
     d  strWCMCGroup                  3                                         WCMC Group

     d issued          s               n   Inz(*Off)

     d returnCode      s              1    Inz(' ')
     d strCompany      s              3
     d strWCMSGroup    s              3    Inz(' ')
     d strSubscriber   s              5
     d strWCMCGroup    s              3

     d wcmsGroup       s                   Like(EAfnd)
     d recon#          s                   Like(PAeobs)

      /Free

       // Get the WCMS Group Number
       strCompany    = %EditC(EAco#:'X');
       strSubscriber = %EditC(EAsub#:'X');
       strWCMCGroup  = %EditC(EAfnd:'X');

       GroupTranslate(returnCode:strCompany:strWCMSGroup:strSubscriber:
                      strWCMCGroup);

       If returnCode <> 'F';

          wcmsGroup = %Dec(strWCMSGroup:3:0);
          recon# = EArcn;

          Chain (EAco#:wcmsGroup:EAfYr:EAcase:EAeob#:recon#) ppa0g;
          If %Found(WDPALG);
             DoW Not %Eof(WDPALG);
                If EAfein = PAfein And EAmod = PAmod And EApaid = PAamt;
                   issued = *On;
                   Leave;
                EndIf;
                ReadE (EAco#:wcmsGroup:EAfYr:EAcase:EAeob#:recon#) ppa0g;
             EndDo;
          EndIf;

       EndIf;

       Return issued;

      /End-Free
     P CheckIssued     E
       //===================================================================
       // CorrectGrpType - Is this correct group type?
       //     -Only process group checks in group batch
       //     -Only process non-group checks in daily batch
       //===================================================================
     P CorrectGrpType  B
     D CorrectGrpType  PI              n

     d correct         s               n   Inz(*Off)
     d groupCheck      s               n   Inz(*Off)
     d providerCheck   s               n   Inz(*Off)

      /Free

       Chain (EAco#:EAfein:EAmod) pnr00;
       If %Found(WMNRL);
          groupCheck = *On;
          If NRgrTy = 'O';
             providerCheck = *On;
          EndIf;
       Else;
          groupCheck = *Off;
       EndIf;

       Select;
       // Group Check
       When inType = 'G';
          If groupCheck = *On And providerCheck = *On;
             correct = *On;
          EndIf;
       // Single Check
       When inType = ' ';
          If groupCheck = *Off;
             correct = *On;
          EndIf;
       EndSl;

       Return correct;

      /End-Free
     P CorrectGrpType  E
