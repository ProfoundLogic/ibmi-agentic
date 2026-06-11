     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE') bnddir('WTBNDDIR')

     /TITLE W9500R01 - TASB Froi File Handling
     * ***************************************************************
     * Description:  This program interacts with the TASB Froi       *
     *               File TFFRP.  It has two modes controlled by     *
     *               the mode parameter.                             *
     *               1) Mode='1' - Retrieves employer/claim info     *
     *               2) Mode='2' - Writes claim record.              *
     *                                                               *
     * Called from: WTCLMADD                                         *
     * Calls to:    None                                             *
     *                                                               *
     *****************************************************************
     * Created by c:62 on 02/19/2014 for project 25WTL               *
     *****************************************************************
     * Revisions:      v                                             *
      *                                                               *
     *****************************************************************
     Ftffrp     if   e             disk    rename(pfr01:pfr0p)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     fwmcll     if   e           k Disk    Rename(pcl01:pcl00)
     fwdrbl     if   e           k Disk    ReName(prb01:prb00)
     Fwmcdl     uf a e           k disk    rename(pcd01:pcd00)
     Fwmfnl     uf   e           k disk    rename(pfn01:pfn00)
     Fwmcmp     o    e             disk    rename(pcm01:pcm0p) infds(dbfds)
     Fwmnip     o    e             disk    rename(pni01:pni0p)
     Fwmchp     o    e             disk    rename(pch01:pch0p)
     Fwmthp     o    e             disk    rename(pth01:pth0p)
     fwmawp     o    e             Disk    rename(paw01:paw0p)

     d DataRetrieve    PR
     d   rrnTFFRP                     9  0  const

     d CreateClaim     PR
     d   rrnTFFRP                     9  0  const

     d/copy sPrDate
     d/copy wPrEmplr

     D                 ds
     D locationZip                    9  0
     D  CMacZ1                        5  0 overlay(locationZip)
     D  CMacZ2                        4  0 overlay(locationZip:6)

     D dbfds           ds
     D dbrrn                 397    400b 0
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)

     D xcorgo          c                   const('Original open of -
     D                                     claim')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d inRrnTFFRP      s              9  0
     d inModeTFFRP     s              1
     d outRrnWMCMP     s              9  0
     d outSocSec#      s              9
     d outAccDate      s              7  0
     d outCompany#     s              3  0
     d outGroup        s              3  0
     d outEmployer#    s              9  0
     d outDivision     s              5  0
     d kkCo#           s                   Like(CMco#)
     d kkFnd           s                   Like(CMfnd)
     d kkFyr           s                   Like(CMfYr)
     d kkCase          s                   Like(CMcase)
     d kkEmp#          s                   Like(CMemp#)
     d kkDiv           s                   Like(CMdiv)
     D kkAcDt          s                   like(CMacDt)
     D kkSS#           s                   like(CMss#)
     D kkEfDt          s                   like(CMacDt)
     D demoFound       s               n
     d charRRN         s              9
     d charMode        s              1
    C*================================================================
     C* Key Lists
    C*================================================================

     C     key1WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt
     C                   Kfld                    kkEfDt

     C     key2WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt
    
     C     keyWMCDL      klist
     C                   kfld                    kkSS#
    
     C     keyWMCLL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv
     C                   Kfld                    CMcmps
    
     C     keyWMFNL      klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
    
     C     keyWMUPL      klist
     C                   kfld                    q1user
    
     C*===========================================================
    C*  Entry Parameter List
     C*===========================================================
     C     *entry        plist
     C                   parm                    inRrnTFFRP
     C                   parm                    inModeTFFRP
     C                   parm                    outRrnWMCMP
     C                   parm                    outSocSec#
     C                   parm                    outAccDate
     C                   parm                    outCompany#
     C                   parm                    outGroup
     C                   parm                    outEmployer#
     C                   parm                    outDivision
    C*
      /free
       Select;
       When inModeTFFRP='1'; // Retrieve info for wpiclmadd
          dataRetrieve(inRrnTFFRP);
       When inModeTFFRP='2'; // Write the Claim Record         q
          CreateClaim(inRrnTFFRP);
       EndSl;
      /end-free
     C*
     C                   seton                                            lr
     P*=====================================================================
     P* DataRetrieve - Retrieves values that will populate clearance screen
     P*  Parameter 1 - TFFRP rrn
     P*=====================================================================
     PDataRetrieve     B                   Export
     DDataRetrieve     PI
     D rrnTFFRP                       9  0 Const

      /free
       Chain rrnTFFRP pfr0p;
       outSocSec#=FRss;
       If FRacDt<>' ';
          outAccDate=dtCYMD(%Dec(FRacDt:8:0));
       EndIf;
       Chain keyWMUPL pup00;
       If %Found(WMUPL);
          outCompany#=UPco#;
       EndIf;
       outGroup=FRfnd;
       outEmployer#=FRemp;
       outDivision=0;
      /end-free

     PDataRetrieve     E
     P*=====================================================================
     P* CreateClaim - Creates Claim Record based upon TFFRP
     P*  Parameter 1 - TFFRP rrn
     P*=====================================================================
     PCreateClaim      B                   Export
     DCreateClaim      PI
     D rrnTFFRP                       9  0 Const

      /free

       // Access FROI file
       Chain rrnTFFRP pfr0p;

       // Populate other key fields
       kkCo#=outCompany#;
       kkFnd=outGroup;
       kkEmp#=outEmployer#;
       kkDiv=outDivision;
       kkSS#=outSocSec#;
       kkAcDt=outAccdate;
       kkFyr=RtvPolYr(kkCo#:kkFnd:kkEmp#:kkDiv:kkAcDt);

      // Get the Next Available Claim #
       FNnacs=0;
       Chain keyWMFNL pfn00;
       If %Found(WMFNL);
          If FNnacs = 0;
             FNnacs = 1;
          Else;
             FNnacs = FNnacs + 1;
          EndIf;
          Update pfn00;
       EndIf;

       kkCase = FNnacs - 1;

       // Transfer Claim Information
       ExSr CrtClaim;
      //=======================================================================
      //  This Subroutine creates the claim
      //=======================================================================
       BegSr CrtClaim;
    
      // Create the Claim Record
      Clear pcm0p;
      Clear pcd00;
      Clear pni0p;

       demoFound=*Off;
       Chain keyWMCDL pcd00;
       If %Found(WMCDL);
          demoFound=*On;
       EndIf;

       CMco# = kkCo#;
       CMfnd = kkFnd;
       CMfYr = kkFyr;
       CMcase= kkCase;
       CMemp#= kkEmp#;
       CMdiv = kkDiv;

       NIco# = kkCo#;
       NIfnd = kkFnd;
       NIfYr = kkFyr;
       NIcase= kkCase;

       CMss# = kkSS#;
       CDss# = kkSS#;
       CMacDt= kkAcDt;
    
       CMuser=q1User;
       CDuser=q1User;
       NIuser=q1User;

       CMpgm = q1Pgm;
       CDpgm = q1Pgm;
       NIpgm = q1Pgm;

       CMdt  = DateTo7(SystemDate());
       CDdt  = DateTo7(SystemDate());
       NIdt  = DateTo7(SystemDate());

       CMtime = TimeTo6(SystemTime());
       CDtime = TimeTo6(SystemTime());
       NItime = TimeTo6(SystemTime());
    
       CMiUsr=q1User;
       If Not demoFound;
          CDiUsr=q1User;
       EndIf;
       NIiUsr=q1User;

       CMpgm = q1Pgm;
       If Not demoFound;
          CDpgm = q1Pgm;
       EndIf;

       CMiDt = DateTo7(SystemDate());
       If Not demoFound;
          CDiDt = DateTo7(SystemDate());
       EndIf;
       NIiDt = DateTo7(SystemDate());

       CMiTm  = TimeTo6(SystemTime());
       If Not demoFound;
          CDiTm  = TimeTo6(SystemTime());
       EndIf;
       NIiTm  = TimeTo6(SystemTime());

       If FRltFl = 'I' Or FRltFl = 'M';
          CMentD = CMiDt;
       EndIf;

       CMstat = 'O';
       CDstat = 'A';
       NIstat = 'A';

      //
      // Move TFFRP matching fields to WMCMP required fields
      //
       Select;
       When FRfreq = '1';
          CMaww = FRaww;
       When FRfreq = '2';
          CMaww = (fraww * 26) / 52;
       When FRfreq = '3';
          CMaww = (fraww * 24) / 52;
       When FRfreq = '4';
          CMaww = (fraww * 12) / 52;
       When FRfreq = '5';
          CMaww = fraww * 40;
       EndSl;

       CMjrSt = 'TX';

       // Write AWW History Record
       Clear paw0p;
       AWco#=CMco#;
       AWfnd=CMfnd;
       AWfyr=CMfyr;
       AWcase=CMcase;
       AWtrDt = DateTo7(SystemDate());
       AWaww=CMAww;
       kkEfDt=CMacDt;
       SetGt key1WDRBL prb00;
       ReadPe key2WDRBL prb00;
       If not %Eof(WDRBL);
          AWcrt=CMaww * RBcmpr / 100;
          If AWcrt<RBmnrt;
             AWcrt=RBmnrt;
          EndIf;
          If AWcrt>RBpacp;
             AWcrt=RBpacp;
          EndIf;
       EndIf;
       AWuser=q1User;
       AWpgm=q1Pgm;
       AWtime=TimeTo6(SystemTime());
       AWdt=AWtrDt;
       AWstat='A';
       Write paw0p;

       CMcRt = AWcRt;
       If FRdtRp<>' ';
          CMdtRp = dtCYMD(dtMDYY2(%Dec(FRdtRp:8:0)));
       EndIf;
       If FRdate<>' ';
          CMrcvD = dtCYMD(dtMDYY2(%Dec(FRdate:8:0)));
       EndIf;
       CMacds = %Subst(FR80a1:1:60);
       CMacd2 = %Subst(FR80a1:61:20);
       CMltFl = FRltFl;
       CMnatr = FRnatr;
       CMpart = FRpart;
       CMcaus = FRcaus;
       CMeAdr = FReAdr;
       If FRhiDt<>' ';
          CMhiDt = dtCYMD(dtMDYY2(%Dec(FRhiDt:8:0)));
       EndIf;
    
      // Move TFFRP matching fields to WMCMP non-required fields
       CMacTm = FRacTm;
       If FRdtLw<>' ';
          CMdtLw = dtCYMD(dtMDYY2(%Dec(FRdtLw:8:0)));
       EndIf;
       If FRdbDt<>' ';
          CMdbDt = dtCYMD(dtMDYY2(%Dec(FRdbDt:8:0)));
       EndIf;
       If FRdod<>' ';
          CMdod  = dtCYMD(dtMDYY2(%Dec(FRdod:8:0)));
       EndIf;
       CMeSts = FReSts;
       CMocDs = FRocDs;
       CMprWw = FRprWw;
       If FRrtw<>' ';
          CMrtw  = dtCYMD(dtMDYY2(%Dec(FRrtw:8:0)));
       EndIf;
       CMpoi  = FRpoi;
       CMfSal = FRfSal;
       CMhosN = FRhosN;
       CMhosA = FRhosA;
       CMhosC = FRhosC;
       CMhosS = FRhosS;
       CMhosZ = FRhosZ;
       CMiTrt = FRiTrt;
       CM#dep = FRdep;
       CMdays = FRdays;
       CMecs# = FRrpt;
       CMcmps = FRecs;

       Chain keyWMCLL pCL00;
       If %Found(WMCLL);
        CMacNm=CLdesc;
        CMacDr=CLaddr;
        CMaCty=CLccty;
        CMaSt=CLcst;
        locationZip=CLczip;
       Endif;

       CMesPv = %Subst(FRcont:1:20);

      // Move TFFRP matching fields to WMCDP required fields
       CDlsn = FRlsn;
       CDfsn = FRfsn;
       CDmid = FRmid;

       CMlsn = FRlsn;
       CMfsn = FRfsn;
       CMmid = FRmid;

       CDadr1 = FRadr1;
       CDadr2 = FRadr2;
       CDcty  = FRcty;
       CDst   = FRst;
       CDzip  = FRzip;

       If FRdtob<>' ';
          CDdtob = dtCYMD(dtMDYY2(%Dec(FRdtob:8:0)));
       EndIf;
       CDsex  = FRsex;
       CDarea = FRarea;
       CDphn# = FRphn;
       CDmSts = FRmSts;
    
      // Move TFFRP matching fields to WMNIP required fields
       NI60a1 = FR60a1;
       NI60a2 = %Subst(FR60a1:51:10);
       NI60a5 = FR60a5;
       NI60a6 = %Subst(FR60a5:51:10);
       NI1a1  = FR1a1;
       NI1a2  = FR1a2;
       NI60a3 = FR60a3;
       NI60a4 = %Subst(FR60a3:51:10);
       NI80a1 = FR80a1;
       NI80a2 = %Subst(FR80a1:41:40);
       NI80a3 = FR80a3;
       NIst   = CMjrSt;

       // write the records to the WMCMP, WMNIP, WMCDP
       Write pcm0p;
       outRrnWMCMP=dbrrn;
       If demoFound;
          Update pcd00;
       Else;
          Write pcd00;
       EndIf;
       Write pni0p;
    
      // Write Claim History File
       CHco#  = CMco#;
       CHfnd  = CMfnd;
       CHfYr  = CMfYr;
       CHcase = CMcase;
       CHefDt = DateTo7(SystemDate());
       CHexpl = xCorgo;
       CHocSt = CMstat;
       CHstat = ' ';
       CHuser = q1User;
       CHpgm  = q1pgm;
       CHdt   = DateTo7(SystemDate());
       CHtime = TimeTo6(SystemTime());

       Write pch0p;
    
      // Write a I/M/R Transfer History
       If CMltFl <> ' ';
          THco#  = CMco#;
          THfnd  = CMfnd;
          THfYr  = CMfYr;
          THcase = CMcase;
          THtrDt = DateTo7(SystemDate());
          THfrom = ' ';
          THto   = CMltFl;
          THstat = ' ';
          THuser = q1User;
          THpgm  = q1pgm;
          THdt   = DateTo7(SystemDate());
          THtime = TimeTo6(SystemTime());

          Write pth0p;
       EndIf;
    
       EndSr;
      /end-free

     PCreateClaim      E
