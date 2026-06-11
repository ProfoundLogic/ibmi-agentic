     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H*=========================================================================
    H/TITLE   FIXGBPMTS - Fix Converted Claim Payments
    H*=========================================================================
     H* Created on 5/02/2018 by c:62 for project 45WLH
    H*=========================================================================
     h* Revisions
     H*
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     h*
    F*=========================================================================
    F*        F i l e s
    F*=========================================================================
    F*
    F* Work File
     Fpmtchgs   if   e             Disk    Rename(ppf01:ppf0p)
    F*
    F* Claims checks payment file
     Fwdpal     uf a e           k Disk    Rename(ppa01:ppa00)
     Fwdrpl3    uf a e           k Disk    Rename(prp01:prp03)
    F*
    F* Claim file
     Fwmcml15   uf   e           k Disk    Rename(pcm01:pcm15)
     Fwmcml1    uf   e           k Disk
    F*
    F* Reserve History
     Fwmrhp     o    e             Disk    Rename(prh01:prh0p)
    F*
    F* Payee demographics master
     Fwmpdl     if   e           k Disk    Rename(ppd01:ppd00)
    F*

    D*=========================================================================
    D* Prototypes
    D*=========================================================================

     d* Apply Changes
     d ApplyChanges    pr

     d* Add Leading Zeros to Date
     d CvtDates        pr             7  0
     d  dateIn                       10

     d* Extract Dates From Description
     d ExtractDates    pr

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

    D*=========================================================================
    D*  Variables
    D*=========================================================================

       // Initial Values
     d   company#      s              3  0 Inz(45)
     d   group#        s              3  0 Inz(5)
     d   claim#        s             40    Inz('011155000952WC01')
     d   hldSeq#       s                   Like(PAseq#)
     d   nxtSeq#       s                   Like(PAseq#)

       // Work Fields
     d dn              s              2  0
     d e               s              5  0
     d n               s              5  0
     D i               s              3  0
     D amount          s                   Like(CMar01)
    D*==============================================================+
    D* Constants
    D*===============================================================
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
    D*
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
    C*=========================================================================
    C*      E N T R Y  P A R A M E T E R S
    C*=========================================================================
    C*=========================================================================
    C*      M  A I  N    L  O  G  I  C
    C*=========================================================================
      /free


       // Read thru the change file and fix payment records
       Read ppf0p;
       DoW Not %Eof(pmtchgs);
          Chain(n) (company#:PFfnd:PFclm#) pcm15;
          If %Found(wmcml15);
             Chain (CMco#:CMfnd:CMfYr:CMcase) ppa00;
             If %Found(wdpal);
                DoU %Eof(wdpal);
                   If PApgm <> 'FIXGBPMTS' And %Trim(PAdesc) = %Trim(PFdesc) And
                      PAbr2 = PFpv1 And PAtrCd = PFtrCd And PAreCd = PFreCd And
                      PAamt = PFamt And PAreTp = PFreTp And
                      PAtrDt = CvtDates(PFtrDt);
                      ApplyChanges();
                   EndIf;
                   ReadE (CMco#:CMfnd:CMfYr:CMcase) ppa00;
                EndDo;
             EndIf;
          EndIf;
          Read ppf0p;
       EndDo;

       // Fix one claim where SS# and Name is incorrect
       Chain (company#:group#:claim#) pcm15;
       If %Found(wmcml15);
          CMss# = '437758038';
          CMlsN = 'Vetkoetter';
          Update pcm15;

          // Add Payment Record
          Clear ppa00;
          PAco# = CMco#;
          PAfnd = CMfnd;
          PAfYr = CMfYr;
          PAcase = CMcase;
          PAseq# = 1;
          PAtrDt = 1180108;
          PApoDt = 1180108;
          PAfein = 204969903;
          PAamt  = 31.86;
          PAbr2  = 'DG';
          PAbr7  = 'NL';
          PAfrDt = 1171118;
          PAtoDt = 1171118;
          PAtrCd = 'MP';
          PAtype = 'P';
          PAreCd = 'M';
          PAreTp = 'M';
          PAdesc = 'RX -Inv # FSPBM000000010469338';
          PAstat = 'A';
          PAuser = 'PDSFISV';
          PApgm  = 'FIXGBPMTS';
          PAtime = 120300;
          PAdt   = 1180508;
          PAiUsr = 'PDSFISV';
          PAiPgm = 'FIXGBPMTS';
          PAiTim = 120300;
          PAiDt  = PAtrDt;
          PAoff# = '00005';
          PArcvD = PAtrDt;
          PArqDt = PAtrDt;
          Write ppa00;
          // Write Payment Demographic Record
          Clear prp03;
          RPco# = CMco#;
          RPfnd = CMfnd;
          RPfYr = CMfYr;
          RPcase = CMcase;
          RPseq# = 1;
          RPtrDt = 1180108;
          Chain (PAfein:PAmod) ppd00;
          If %Found(wmpdl);
             If PDdba1 <> 'Y';
                RPpNam = PDname;
                RPadr1 = PDadr1;
                RPadr2 = PDadr2;
             Else;
                RPpNam = PDadr1;
                RPadr1 = PDadr2;
                RPadr2 = ' ';
             EndIf;
             RPcty = PDcty;
             RPst  = PDst;
             RPzip = PDzip;
          EndIf;
          Write prp03;
       EndIf;

       // Adjust reserves and update staff codes
       Chain (company#:group#) pcm01;
       If %Found(wmcml1);
          DoU %Eof(wmcml1);
             CMclex = 'NXH';
             CMsupr = 'GW1';
             // Balance Pay Buckets
             CMar01 = 0;
             CMar02 = 0;
             CMar03 = 0;
             CMar04 = 0;
             CMar05 = 0;
             Chain(n) (CMco#:CMfnd:CMfYr:CMcase) ppa00;
             If %Found(wdpal);
                DoU %Eof(wdpal);
                   If PAtype = 'R' Or PAtype = 'S' Or PAtype = 'V';
                      amount = PAamt * -1;
                   Else;
                      amount = PAamt;
                   EndIf;
                   Select;
                   When PAreCd = 'M';
                      CMar01 = CMar01 + amount;
                   When PAreCd = 'E';
                      CMar02 = CMar02 + amount;
                   When PAreCd = 'I';
                      CMar03 = CMar03 + amount;
                   When PAreCd = 'L';
                      CMar04 = CMar04 + amount;
                   When PAreCd = 'R';
                      CMar05 = CMar05 + amount;
                   EndSl;
                   ReadE(n) (CMco#:CMfnd:CMfYr:CMcase) ppa00;
                EndDo;
             EndIf;
             Update pcm01;
             ReadE (company#:group#) pcm01;
          EndDo;
       EndIf;

       // Need to create reserve history for the legal entries
       Chain(n) (company#:group#) pcm01;
       If %Found(wmcml1);
          DoU %Eof(wmcml1);
             If CMar04 <> 0;
                Clear prh0p;
                RHco#  = CMco#;
                RHfnd  = CMfnd;
                RHfYr  = CMfYr;
                RHcase = CMcase;
                RHefdt = CMentD;
                RHexpl = 'Reserves set for legal payments';
                RHrt01 = 'M';
                RHrt02 = 'E';
                RHrt03 = 'I';
                RHrt04 = 'L';
                RHrt05 = 'R';
                RHsu04 = CMar04;
                RHstat = ' ';
                RHdt   = CMdt;
                RHtime = CMtime;
                RHuser = CMuser;
                RHpgm  = CMpgm;
                RHiDt  = CMdt;
                RHiTim = CMtime;
                RHiUsr = CMuser;
                RHiPgm = CMpgm;
                Write prh0p;
             EndIf;
             ReadE(n) (company#:group#) pcm01;
          EndDo;
       EndIf;

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
       //===================================================================
       // ApplyChanges - Apply Payment Changes
       //===================================================================
     P ApplyChanges    B
     D ApplyChanges    PI

      /free

       // Apply Transaction Code Change
       If PFnTrc <> ' ';
          PAtrCd = PFnTrc;
          Select;
          When PFnTrC = 'EP' Or PFnTrC = 'ER';
             PAreCd = 'E';
             PAreTp = 'E';
             If PAtrCd = 'EP';
                PAtype = 'P';
             Else;
                PAtype = 'R';
             EndIf;
          When PFnTrC = 'LP' Or PFnTrC = 'LR';
             PAreCd = 'L';
             PAreTp = 'L';
             If PAtrCd = 'LP';
                PAtype = 'P';
             Else;
                PAtype = 'R';
             EndIf;
          When PFnTrC = 'MP' Or PFnTrC = 'MR';
             PAreCd = 'M';
             PAreTp = 'M';
             If PAtrCd = 'MP';
                PAtype = 'P';
             Else;
                PAtype = 'R';
             EndIf;
          When PFnTrC = 'RP' Or PFnTrC = 'RR';
             PAreCd = 'R';
             PAreTp = 'M';
             If PAtrCd = 'RP';
                PAtype = 'P';
             Else;
                PAtype = 'R';
             EndIf;
          When PFnTrC = 'VO';
             PAtype = 'V';
             PAamt  = PAamt * -1;
          When PFnTrC = 'SP';
             PAtype = 'S';
             PAamt  = PAamt * -1;
          EndSl;
       EndIf;

       // Apply Payment Value 1 Code Change
       If PFnPv1 <> ' ';
          PAbr2 = PFnPv1;
       EndIf;

       // Apply Amount Change
       If PFnAmt <> 0;
          PAamt = PFnAmt;
       EndIf;

       // Apply Other Changes
       Select;
       When PFcTyp = 'F'; // Fein Change
          PAfein = %Int(%Trim(PFfein));
          PAmod  = 0;
          Chain (PAfein:PAmod) ppd00;
          If %Found(wmpdl);
             Chain (PAco#:PAfnd:PAfYr:PAcase:PAseq#) prp03;
             If %Found(wdrpl3);
                If PDdba1 <> 'Y';
                   RPpNam = PDname;
                   RPadr1 = PDadr1;
                   RPadr2 = PDadr2;
                Else;
                   RPpNam = PDadr1;
                   RPadr1 = PDadr2;
                   RPadr2 = ' ';
                EndIf;
                RPcty = PDcty;
                RPst  = PDst;
                RPzip = PDzip;
                Update prp03;
             EndIf;
          EndIf;
       When PFcTyp = 'S'; // Apply Service Date Change
          PAfrDt = CvtDates(PFfrDt);
          PAtoDt = CvtDates(PFtoDt);
       When PFcTyp = 'D'; // Get Service Dates from Description
          ExtractDates();
       EndSl;

       PApgm = 'FIXGBPMTS';
       Update ppa00;

      /End-Free

     P ApplyChanges    E
       //===================================================================
       // CvtDates - Convert Incoming Dates.
       //===================================================================
     P CvtDates        B
     D CvtDates        PI             7  0
     d   dateIn                      10

     d  intDate        s              7  0
     d  day            s              2  0
     d  month          s              2  0
     d  year           s              4  0
     d  pos1           s              2  0
     d  pos2           s              2  0
     d  chrDate        s              8
      /free

       If dateIn <> ' ';

          pos1    = %Scan('/':dateIn);
          pos2    = %Scan('/':dateIn:pos1+1);
          month   = %Int(%Subst(dateIn:1:pos1-1));
          day     = %Int(%Subst(dateIn:pos1+1:pos2-1-pos1));
          year    = %Int(%Subst(dateIn:pos2+1:4));
          chrDate = %EditC(year:'X') + %EditC(month:'X') + %EditC(day:'X');

          Monitor;
             intDate = %Int(chrDate) - 19000000;
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
       // ExtractDates - Extract Dates from Description
       //===================================================================
     P ExtractDates    B
     D ExtractDates    PI

     d  intDate        s              7  0
     d  day            s              2  0
     d  month          s              2  0
     d  year           s              4  0
     d  pos1           s              2  0
     d  pos2           s              2  0
     d  chrDate        s              8
     d  chrDate1       s              8
     d  chrDate2       s              5
      /free

       // Extract date range
       chrDate1 = %Subst(PFdesc:14:8);
       chrDate2 = %Subst(PFdesc:25:5);

       // Lets Convert the From Date
       pos1    = %Scan('/':chrDate1);
       pos2    = %Scan('/':chrDate1:pos1+1);
       month   = %Int(%Subst(chrDate1:1:pos1-1));
       day     = %Int(%Subst(chrDate1:pos1+1:pos2-1-pos1));

       If %Int(%Subst(chrDate1:pos2+1:2)) < 40;
          year = %Int(%Subst(chrDate1:pos2+1:2)) + 2000;
       Else;
          year = %Int(%Subst(chrDate1:pos2+1:2)) + 1900;
       EndIf;

       chrDate = %EditC(year:'X') + %EditC(month:'X') + %EditC(day:'X');

       Monitor;
          PAfrDt = %Int(chrDate) - 19000000;
       On-Error;
          PAfrDt = 0;
       EndMon;

       // Lets Convert the To Date
       pos1    = %Scan('/':chrDate2);
       month   = %Int(%Subst(chrDate2:1:pos1-1));
       day     = %Int(%Subst(chrDate2:pos1+1:2));

       chrDate = %EditC(year:'X') + %EditC(month:'X') + %EditC(day:'X');

       Monitor;
          PAtoDt = %Int(chrDate) - 19000000;
       On-Error;
          PAtoDt = 0;
       EndMon;

      /End-Free
     P ExtractDates    E
