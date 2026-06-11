     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title CVTAUTODRY - Convert Existing Auto-Diary Setup to Triggers
     h dftname(CVTAUTODRY)
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program Will convert existing auto-diary setup *
      *                to triggers.                                        *
      *                                                                    *
      *  Called from:  None                                                *
      *                                                                    *
      *  Created on 05/22/2015 by C:279 for Project: 114WKA                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  06/01/16 C:242 Project 114WKA                                     *
      *                 Do not convert for Expired Groups and Inactive     *
      *                 Triggers                                           *
      *                                                                    *
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
      *====================================================================*
     fwmadp     if   e             disk    rename(pad01:pad0p)
     f                                     usropn
      *** Begin add *** 06/01/16 **************************************
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      *** End   add *** 06/01/16 **************************************
     fsmtrp     if a e             disk    rename(ptr01:ptr0p)
     fsmtrl     if   e           k disk    rename(ptr01:ptr00)
     f                                     prefix(m)
     fsmtrpgrp  if a e           k disk    rename(ptr01:ptr0grp)
     fsmtrlrec  if   e           k disk    rename(ptr01:ptr0rc1)
     fsmtrprec  if a e             disk    rename(ptr01:ptr0rec)
     fsmtrpclm  if a e           k disk    rename(ptr01:ptr0clm)
     fsmtrppol  if a e           k disk    rename(ptr01:ptr0pol)
     fsmtrptbl  if a e           k disk    rename(ptr01:ptr0tbl)
     fwtgtl     if a e           k disk    rename(pgt01:pgt00)

     d psds          esds                  extname(szq1p)

      * work variables
     d ##Wcif          s              1
     d ##Mcif          s              1
     d ##Wcin          s              1
     d intSeq#         s              5  0
     d isDupFnd        s               n   inz(*on)
     d svADco#         s                   like(ADco#)
     d kkTbId          s              4
     d kkKey           s             10
      *** Begin add *** 06/01/16 **************************************
     d GroupStatus     s              1
      *** End   add *** 06/01/16 **************************************

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      /copy sPrTable
      /copy sPrDate

     c     keyWTGTL      Klist
     c                   Kfld                    ADco#
     c                   Kfld                    kkTbId
     c                   Kfld                    kkKey

      /free

       // Get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);

       // If WCMS is not installed, do not run.
       If ##Wcif <> 'Y';
          Return;
       Endif;

       Open WMADP;

       // Read file WMADP for existing auto diary triggers
       Setll 1 pad0p;
       Read pad0p;
       Dow not %eof(WMADP);
          //check if CMS Module Installed
          Exsr ChkCMSInstalled;

       //*** Begin add *** 06/01/16 **************************************
          Exsr ChkGroupStatus;
          If GroupStatus = 'A';
       //*** End   add *** 06/01/16 **************************************

          // check if setup can be combined.
          Exsr ChkSetup;

       //*** Begin Chg *** 06/01/16 **************************************
          // If trigger is combined with existing trigger, move to next record
          //***If isDupFnd = *on;
          //***   Read pad0p;
          //***   Iter;
          //***Endif;
          If isDupFnd = *off;
       //*** End   Chg *** 06/01/16 **************************************

          // get sequence #
          Setgt (ADco#:ADkey:*hival) ptr00;
          Readpe (ADco#:ADkey) ptr00;
          If not %eof(SMTRL);
             intSeq# = mTRseq# + 1;
          Else;
             intSeq# = 0;
          Endif;

          clear ptr0p;
          clear ptr0grp;
          clear ptr0rec;
          clear ptr0pol;
          clear ptr0clm;
          clear ptr0tbl;

          // write record to SMTRP
          Exsr WrtMstTrg;

          // write record to SMTRPGRP
          If ADfnd > 0;
             TRfnd  = ADfnd;
             write ptr0grp;
          Endif;

          // write record to SMTRPREC
          Exsr WrtRecpt;

          // write record to SMTRPCLM
          Exsr WrtClaim;

          // write record in Table file
          Exsr WrtTable;

       //*** Begin add *** 06/01/16 **************************************
          Endif;
          Endif;
       //*** End   add *** 06/01/16 **************************************

          Read pad0p;
       Enddo;

       Close WMADP;
       *inlr  = *on;

       //===================================================================
       // Check if CMS Module is Installed
       //===================================================================
       Begsr ChkCMSInstalled;

       IF svADco# <> ADco#;
          If CMSInstalled;
             kkTbId = 'AXCI';
             kkkey  = '10  ';
             Chain keyWTGTL pgt00;
             If not %Found(WTGTL);
                GTCO#  = ADco#;
                GTTBID = 'AXCI';
                GTKEY  = '10';
                GTDESC = 'CMS';
                GTSTAT = 'A';
                GTUSER = q1User;
                GTPGM  = q1Pgm;
                GTTIME = Timeto6(SystemTime());
                GTDT   = Dateto7(SystemDate());
                write pgt00;
             Endif;
          Endif;
          svADco# = ADco#;
       Endif;

       Endsr;

       //===================================================================
       // Check if same trigger setup present
       //===================================================================
       Begsr ChkSetup;

       isDupFnd = *on;

       // Check for master trigger setup
       Setll (ADco#:ADkey) ptr00;
       Reade (ADco#:ADkey) ptr00;
       If %eof(SMTRL);
          isDupFnd = *off;
       Endif;

       Dow not %eof(SMTRL);

          isDupFnd = *on;

          // Check for common elements setup
          If mTRdart = ADrmCd and mTRAddlDays = ADdiDy and ((ADdiDt = '1' and
           mTRdiaryDt = 'SystemDt') or (ADdiDt  = '2' and mTRdiaryDt = 'DtRcvd')
           or (ADdiDt  = '3' and mTRdiaryDt = 'AccDate') or (ADdiDt  = '4' and
           mTRdiaryDt = 'RptDate')) and ((ADocur = '1' and mTR#Occur = 'One') or
           (ADocur = '2' and mTR#Occur = 'OneOpen') or (ADocur = '3' and
           mTR#Occur = 'Multiple'));

             // Check for recipients setup
             Chain (ADco#:ADkey:mTRseq#) ptr0rc1;
             If %found(SMTRLREC) and ((TRtype = 'Individual' and ADdiUs = ' '
              and TRrecip = ADusr) or (TRtype = 'Role' and ((ADdiUs = '01' and
              TRrecip = 'ClmAdj') or (ADdiUs = '02' and TRrecip = 'ClmSuper')
              or (ADdiUs = '03' and TRrecip = 'AcctMgr') or (ADdiUs = '04' and
              TRrecip = 'ClmMgr') or (ADdiUs = '05' and TRrecip = 'CaseNurse')
              or (ADdiUs= '06' and TRrecip = 'OutAdj') or (ADdiUs = '07' and
              TRrecip= 'SDFAdj') or (ADdiUs= '08' and TRrecip= 'ExcessAdj'))))
              and ((TRaltType = ' ' and ADdiAu = ' ') or (TRaltType = 'Role'
              and ((ADdiAu = '01' and TRaltRecip = 'ClmAdj') or (ADdiAu = '02'
              and TRaltRecip = 'ClmSuper') or (ADdiAu = '03' and TRaltRecip =
              'AcctMgr') or (ADdiAu = '04' and TRaltRecip = 'ClmMgr') or
              (ADdiAu = '05' and TRaltRecip = 'CaseNurse') or (ADdiAu = '06'
              and TRaltRecip = 'OutAdj') or (ADdiAu = '07' and TRaltRecip =
              'SDFAdj') or (ADdiAu = '08' and TRaltRecip = 'ExcessAdj'))));

                // Check for claim control setup
                Chain(n) (ADco#:ADkey:mTRseq#) ptr0clm;
                If %found(SMTRPCLM) and ((ADcSts = ' ' and TRclmSts = ' ') or
                 (ADcSts = 'O' and TRclmSts = 'Open') or ((ADcSts = 'C' and
                 TRclmSts = 'Closed'))) and TRamt = ADamt and TRReten = ADgRet
                 and TRClmType = ADlTFl and TRpayCount = ADcnt and TRTotPaid =
                 ADtotP and TRWeeksPd = ADwkPd;

                   // Check for table setup for jurisdiction states
                   Chain(n) (ADco#:ADkey:mTRseq#:'States') ptr0tbl;
                   If (%found(SMTRPTBL) and TRkey = ADjrSt) or
                    (not %found(SMTRPTBL) and ADjrSt=' ');

                      // Check for table setup for claim indicators
                      Chain(n) (ADco#:ADkey:mTRseq#:'Claim Indicator') ptr0tbl;
                      If (%found(SMTRPTBL) and TRkey = ADcInd) or
                       (not %found(SMTRPTBL) and ADcInd = ' ');
                         Exsr ChkNCCI;
                      Else;
                         isDupFnd = *off;
                      Endif;
                   Else;
                      isDupFnd = *off;
                   Endif;
                Else;
                   isDupFnd = *off;
                Endif;
             Else;
                isDupFnd = *off;
             Endif;
          Else;
             isDupFnd = *off;
          Endif;

          // Check for table setup for reserve codes
          If isDupFnd = *on;
             Exsr ChkReserve;
          Endif;

          // Check for table setup for payment codes
          If isDupFnd = *on;
             Exsr ChkPayment;
          Endif;

          // If duplicate trigger setup and different groups are present then combine.
          If isDupFnd = *on;
             Exsr WrtGroup;
             leave;
          Endif;

          Reade (ADco#:ADkey) ptr00;
       Enddo;

       Endsr;
       //===================================================================
       // ChkNCCI
       //===================================================================
       Begsr ChkNCCI;

       Chain(n) (ADco#:ADkey:mTRseq#:'NCCI Injury') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADncc1) or
        (not %found(SMTRPTBL) and ADncc1 = ' '));
          isDupFnd = *off;
       Endif;

       Chain(n) (ADco#:ADkey:mTRseq#:'NCCI Injury') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADncc2) or
        (not %found(SMTRPTBL) and ADncc2 = ' '));
          isDupFnd = *off;
       Endif;

       Chain(n) (ADco#:ADkey:mTRseq#:'NCCI Injury') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADncc3) or
        (not %found(SMTRPTBL) and ADncc3 = ' '));
          isDupFnd = *off;
       Endif;

       Endsr;
       //===================================================================
       // ChkReserve
       //===================================================================
       Begsr ChkReserve;

       Chain(n) (ADco#:ADkey:mTRseq#:'Reserve') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADreCd) or
        (not %found(SMTRPTBL) and ADreCd = ' '));
          isDupFnd = *off;
       Endif;

       Endsr;

       //===================================================================
       // ChkPayment
       //===================================================================
       Begsr ChkPayment;

       Chain(n) (ADco#:ADkey:mTRseq#:'Payment') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADbr2) or
        (not %found(SMTRPTBL) and ADbr2 = ' '));
          isDupFnd = *off;
       Endif;

       Chain(n) (ADco#:ADkey:mTRseq#:'Payment') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADbr2a) or
        (not %found(SMTRPTBL) and ADbr2a = ' '));
          isDupFnd = *off;
       Endif;

       Chain(n) (ADco#:ADkey:mTRseq#:'Payment') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADbr2b) or
        (not %found(SMTRPTBL) and ADbr2b = ' '));
          isDupFnd = *off;
       Endif;

       Chain(n) (ADco#:ADkey:mTRseq#:'Payment') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADbr2c) or
        (not %found(SMTRPTBL) and ADbr2c = ' '));
          isDupFnd = *off;
       Endif;

       Chain(n) (ADco#:ADkey:mTRseq#:'Payment') ptr0tbl;
       If not ((%found(SMTRPTBL) and TRkey = ADbr2d) or
        (not %found(SMTRPTBL) and ADbr2d = ' '));
          isDupFnd = *off;
       Endif;

       Endsr;

       //===================================================================
       // Write group record
       //===================================================================
       Begsr WrtGroup;

       If ADfnd > 0;
          Setll (mTRco#:mTRname:mTRseq#:ADfnd) ptr0grp;
          If not %equal(SMTRPGRP);
             TRco#  = mTRco#;
             TRname = mTRname;
             TRseq# = mTRseq#;
             TRiPgm = q1Pgm;
             TRiUsr = q1User;
             TRiTim = Timeto6(SystemTime());
             TRiDt  = Dateto7(SystemDate());
             TRpgm  = q1Pgm;
             TRuser = q1User;
             TRtime = TRiTim;
             TRdt   = TRiDt;

             TRfnd  = ADfnd;
             write ptr0grp;
          Endif;
       Endif;

       Endsr;

       //===================================================================
       // WrtPayment
       //===================================================================
       Begsr WrtPayment;

       TRtable = 'Payment';

       TRkey = ADbr2;
       Exsr WrtTbl;

       TRkey = ADbr2a;
       Exsr WrtTbl;

       TRkey = ADbr2b;
       Exsr WrtTbl;

       TRkey = ADbr2c;
       Exsr WrtTbl;

       TRkey = ADbr2d;
       Exsr WrtTbl;

       Endsr;

       //===================================================================
       // WrtTbl
       //===================================================================
       Begsr WrtTbl;

       If TRkey <> ' ';
          write ptr0tbl;
       Endif;

       Endsr;

       //===================================================================
       // WrtRecpt
       //===================================================================
       Begsr WrtRecpt;

       TRtype = 'Role';
       Select;
       When ADdiUs = *blank;
          TRtype   = 'Individual';
          TRrecip  = ADusr;
       When ADdiUs = '01';
          TRrecip  = 'ClmAdj';
       When ADdiUs = '02';
          TRrecip  = 'ClmSuper';
       When ADdiUs = '03';
          TRrecip  = 'AcctMgr';
       When ADdiUs = '04';
          TRrecip  = 'ClmMgr';
       When ADdiUs = '05';
          TRrecip  = 'CaseNurse';
       When ADdiUs = '06';
          TRrecip  = 'OutAdj';
       When ADdiUs = '07';
          TRrecip  = 'SDFAdj';
       When ADdiUs = '08';
          TRrecip  = 'ExcessAdj';
       Endsl;

       TRaltType = 'Role';
       Select;
       When ADdiAu   = *blank;
          TRaltType  = ' ';
       When ADdiAu   = '01';
          TRAltRecip = 'ClmAdj';
       When ADdiAu   = '02';
          TRAltRecip = 'ClmSuper';
       When ADdiAu   = '03';
          TRAltRecip = 'AcctMgr';
       When ADdiAu   = '04';
          TRAltRecip = 'ClmMgr';
       When ADdiAu   = '05';
          TRAltRecip = 'CaseNurse';
       When ADdiAu   = '06';
          TRAltRecip = 'OutAdj';
       When ADdiAu   = '07';
          TRAltRecip = 'SDFAdj';
       When ADdiAu   = '08';
          TRAltRecip = 'ExcessAdj';
       Endsl;

       If TRrecip <> ' ' or TRAltRecip <> ' ';
          write ptr0rec;
       Endif;

       Endsr;

       //===================================================================
       // WrtMstTrg
       //===================================================================
       Begsr WrtMstTrg;

       TRco#  = ADco#;
       TRname = ADkey;
       TRseq# = intSeq#;
       TRdesc = ADdesc;
       TRtype = 'Claims';
       TRdart = ADrmCd;
       TRAddlDays = ADdiDy;

       Select;
       When ADdiDt  = '1';
          TRdiaryDt = 'SystemDt';
       When ADdiDt  = '2';
          TRdiaryDt = 'DtRcvd';
       When ADdiDt  = '3';
          TRdiaryDt = 'AccDate';
       When ADdiDt  = '4';
          TRdiaryDt = 'RptDate';
       Endsl;

       Select;
       When ADocur = '1';
          TR#Occur = 'One';
       When ADocur = '2';
          TR#Occur = 'OneOpen';
       When ADocur = '3';
          TR#Occur = 'Multiple';
       Endsl;

       TRSecLvl = 0;
       TRstat = ADstat;
       TRiPgm = q1Pgm;
       TRiUsr = q1User;
       TRiTim = Timeto6(SystemTime());
       TRiDt  = Dateto7(SystemDate());
       TRpgm  = q1Pgm;
       TRuser = q1User;
       TRtime = TRiTim;
       TRdt   = TRiDt;

       write ptr0p;

       Endsr;

       //===================================================================
       // WrtClaim
       //===================================================================
       Begsr WrtClaim;

       Select;
       When ADcSts = 'O';
          TRclmSts = 'Open';
       When ADcSts = 'C';
          TRclmSts = 'Closed';
       Endsl;

       TRamt      = ADamt;
       TRReten    = ADgRet;
       TRTotInc   = 0;
       TRClmType  = ADlTFl;
       TRpayCount = ADcnt;
       TRTotPaid  = ADtotP;
       TRWeeksPd  = ADwkPd;

       write ptr0clm;

       Endsr;

       //===================================================================
       // WrtTable
       //===================================================================
       Begsr WrtTable;

       // write record to SMTRPTBL for jurisdiction state
       TRtable = 'States';
       TRkey   = ADjrSt;
       Exsr WrtTbl;

       // write record to SMTRPTBL for claim indicator
       TRtable = 'Claim Indicator';
       TRkey   = ADcInd;
       Exsr WrtTbl;

       // write record to SMTRPTBL for NCCI injury codes
       TRtable = 'NCCI Injury';
       TRkey = ADncc1;
       Exsr WrtTbl;

       TRkey = ADncc2;
       Exsr WrtTbl;

       TRkey = ADncc3;
       Exsr WrtTbl;

       // write record to SMTRPTBL for reserve codes
       TRtable = 'Reserve';
       TRkey   = ADreCd;
       Exsr WrtTbl;

       // write record to SMTRPTBL for payment codes
       Exsr WrtPayment;

       Endsr;

       //*** Begin add *** 06/01/16 **************************************
       //===================================================================
       // ChkGroupStatus
       //===================================================================
       Begsr ChkGroupStatus;

       Chain(n) (ADco#:ADfnd) pfn00;
       If (%found(WMFNL) and FNexdt = 0 and ADstat = 'A');
          GroupStatus = 'A';
       Else;
          GroupStatus = 'I';
       Endif;

       Endsr;
       //*** End   add *** 06/01/16 **************************************
      /end-free
