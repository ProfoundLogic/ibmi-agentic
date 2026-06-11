     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H  StoneRiver is a trademark of StoneRiver, Inc.')
     HDFTACTGRP(*NO) OPTION(*NODEBUGIO)
     H* Needs to be compiled with a LIBL including a Profound library
     H*
     H*CREATE TABLE xxxxxxxx/SECAUDIT
     H* (XXTYPE CHAR (50 ) NOT NULL WITH DEFAULT,
     H*  XXDESC CHAR (100) NOT NULL WITH DEFAULT,
     H*  XXUSER CHAR (100) NOT NULL WITH DEFAULT,
     H*  XXGROUP CHAR (1 ) NOT NULL WITH DEFAULT,
     H*  XXALLOW CHAR (1 ) NOT NULL WITH DEFAULT)
     H*
     H* After running, copy /SECAUDIT/Security Audit - Shell.xlsx:
     H*  Download all input files to appropriate tabs in XLSX
     H*   Update SDOFP columns:
     H*    AA - program/process
     H*    AB - Option/funtion description
     H*    Ensure AC,AD,AE,AF,AG,AH are updated for all rows
     H*   Update SMASP columns:
     H*    N  - program/process
     H*    O  - Section/funtion description
     H*    Ensure P,R,S,T are updated for all rows
     H*   Update SDACP columns:
     H*    AD - process description
     H*  Download SECUAUDIT file to 'All' tab in XLSX
     H*   Ensure Columns F,G,H,I are updated for all rows
     H*  Refresh pivot tables in 'Authorities' tab in XLXS
     FATNAVP    IF   E           K DISK
     FATUSERSP  IF   E           K DISK
      /IF NOT DEFINED(PUI532)
     FATSUPGRPP IF   E           K DISK    usropn
      /ENDIF
     FATGROUPSP IF   E           K DISK
     FATAUTHP   IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     ignore(spc01)
     FSMUPP     IF   E           K DISK    rename(pup01:sup0p)
     FSMSLP     IF   E           K DISK    rename(psl01:ssl0p)
     FSMAZL     IF   E           K DISK    rename(paz01:waz00)
     FSMAZL1    IF   E           K DISK    rename(paz01:maz01)
     FSDOFP     IF   E           K DISK    rename(pof01:sof0p)
     FSMASP     IF   E           K DISK    rename(pas01:mas0p)
     FSTCTP     IF   E           K DISK    rename(pct01:sct0p)
     FSTSSP     IF   E           K DISK    rename(pss01:sss0p)
     FSDACP     IF   E           K DISK    rename(pac01:sac0p)
     FSAASL     IF   E           K DISK    rename(pas01:aas00)
     FWMCXP     IF   E           K DISK    rename(pcx01:wcx0p)
     FWMEXP     IF   E           K DISK    rename(pex01:wex0p)
     FWMUPP     IF   E           K DISK    rename(pup01:wup0p)
     FWMCUP     IF   E           K DISK    rename(pcu01:wcu0p)
     FWTPCP     IF   E           K DISK    rename(ppc01:wpc0p)
     FWTSFP     IF   E           K DISK    rename(psf01:wsf0p)
     FWTTTP     IF   E           K DISK    rename(ptt01:wtt0p)
     FSecAudit  uf a e             disk    rename(secaudit:wf)

     DGroupAuthorized  pr             1
     D inItem                         9  0 const
     D inGroup                        9  0 const

     DUserAuthorized   pr             1n
     D inUserType                     1    const
     D inUserLvl                      3  0 const
     D inCmpType1                     1    const
     D inCmpLvl1                      3  0 const
     D inCmpType2                     1    const options(*nopass)
     D inCmpLvl2                      3  0 const options(*nopass)
     D inCmpType3                     1    const options(*nopass)
     D inCmpLvl3                      3  0 const options(*nopass)
     D inCmpType4                     1    const options(*nopass)
     D inCmpLvl4                      3  0 const options(*nopass)
     D inCmpType5                     1    const options(*nopass)
     D inCmpLvl5                      3  0 const options(*nopass)
     D inCmpType6                     1    const options(*nopass)
     D inCmpLvl6                      3  0 const options(*nopass)
     D inCmpType7                     1    const options(*nopass)
     D inCmpLvl7                      3  0 const options(*nopass)
     D inCmpType8                     1    const options(*nopass)
     D inCmpLvl8                      3  0 const options(*nopass)
     D inCmpType9                     1    const options(*nopass)
     D inCmpLvl9                      3  0 const options(*nopass)
     D inCmpType10                    1    const options(*nopass)
     D inCmpLvl10                     3  0 const options(*nopass)

     DallIncExc        s              1
     DincExc           s              1

      /Free
       // Read through Atrium Navigation
       Setll *start ATNAVP;
       Dou %eof(ATNAVP);
          Clear XXgroup;
          Read ATNAVP;
          If %eof;
             Iter;
          Endif;
          Select;
          When ANsect = 'N';
             XXtype = 'Menu';
          When ANsect = 'T';
             XXtype = 'Atrium Toolbar Dropdown';
          When ANsect = 'H';
             XXtype = 'Home Page(s)';
          Other;
             XXtype = 'Other Atrium';
          Endsl;
          XXdesc  = ANtext;
          // Check global authority
          Clear AAallow;
          Chain (ANitem:0) ATAUTHP;
          // When no record found, assume '0' (record should always be found)
          If AAallow = *blank;
             AAallow = '0';
          Endif;
          allIncExc = AAallow;
          // Read through Atrium users
          Setll *start ATUSERSP;
          Dou %eof(ATUSERSP);
             Read ATUSERSP;
             If %eof or AUuser = 0;
                Iter;
             Endif;
             Clear AAallow;
             Chain (ANitem:AUuser) ATAUTHP;
             // Check user group(s)
             If not %found;
                // Primary group
                AAallow = GroupAuthorized(ANitem:AUgroup);
      /IF NOT DEFINED(PUI532)
                Monitor;
                   Open ATSUPGRPP;
                   // Supplemetnal group(s)
                   Setll AUuser ATSUPGRPP;
                   Dow not %eof(ATSUPGRPP) and AAallow <> '1';
                     Reade (AUuser) ATSUPGRPP;
                     If %eof;
                         Iter;
                      Endif;
                      AAallow = GroupAuthorized(ANitem:ASuGroup);
                   Enddo;
                On-Error;
                Endmon;
      /ENDIF
             Endif;
             // No record found, use default
             If AAallow = *blank;
                AAallow = allIncExc;
             Endif;
             incExc = AAallow;
             // Write to workfile
             XXuser  = AUprof;
             XXallow = incExc;
             Write wf;
          Enddo;
          // Read through Atrium groups
          Setll *start ATGROUPSP;
          Dou %eof(ATGROUPSP);
             Read ATGROUPSP;
             If %eof;
                Iter;
             Endif;
             Clear AAallow;
             AAallow = GroupAuthorized(ANitem:AGgroup);
             // No record found, use default
             If AAallow = *blank;
                AAallow = allIncExc;
             Endif;
             incExc = AAallow;
             // Write to workfile
             XXgroup = 'Y';
             XXuser  = AGname;
             XXallow = incExc;
             Write wf;
          Enddo;
       Enddo;

       // Read through user file for restrictions
       Setll *start SMUPP;
       Dou %eof(SMUPP);
          Read SMUPP;
          Select;
          // End of file
          When %eof or UPstat <> 'A';
             Iter;
          // No restrictions exist
          When UPxFnd <> 'Y' and UPxEmp <> 'Y' and
           UPmrSu <> 'Y' and UPmrGr <> 'Y' and UPmrEm <> 'Y';
             Iter;
          Endsl;
          XXtype  = 'Data Restrictions';
          XXuser  = UPusr;
          // Check managed care restrictions
          Chain (UPco#:UPusr) maz01;
          If not %found;
             If UPmrSu = 'Y';
                XXdesc  = 'MC Subscriber - ' + %editc(UPsub#:'X');
                Write wf;
             Endif;
             If UPmrGr = 'Y';
                XXdesc  = 'MC Group - ' + %editc(UPfnd:'X');
                Write wf;
             Endif;
             If UPmrEm = 'Y';
                XXdesc  = 'MC Employer - ' + %editc(UPemp#:'X');
                Write wf;
             Endif;
          Endif;
          // Check WC restrictions
          Chain (UPco#:UPusr) waz00;
          If not %found;
             If UPxFnd = 'Y';
                XXdesc  = 'WC Group - ' + %editc(UPfnd:'X');
                Write wf;
             Endif;
             If UPxEmp = 'Y';
                XXdesc  = 'WC Employer - ' + %editc(UPemp#:'X');
                Write wf;
             Endif;
          Endif;
          // Review WC additional restrictions
          Setll (UPco#:UPusr) waz00;
          Dou %eof;
             Reade (UPco#:UPusr) waz00;
             If %eof or AZstat <> 'A';
                Iter;
             Endif;
             XXallow = '1';
             If AZauth = 'I';
                XXallow = 'I';
             Endif;
             If AZfnd <> *zero;
                XXdesc  = 'WC Group - ' + %editc(AZfnd:'X');
                Write wf;
             Endif;
             If (AZfEmp <> *zero or AZtEmp <> *zero);
                XXdesc  = 'WC Employer Range - ' + %editc(AZfEmp:'X') +
                 '-' + %editc(AZtEmp:'X');
                Write wf;
             Endif;
          Enddo;
          // Review MC additional restrictions
          Setll (UPco#:UPusr) maz01;
          Dou %eof;
             Reade (UPco#:UPusr) maz01;
             If %eof or AZstat <> 'A';
                Iter;
             Endif;
             XXallow = '1';
             If AZauth = 'I';
                XXallow = 'I';
             Endif;
             If AZsub# <> *zero;
                XXdesc  = 'MC Subscriber - ' + %editc(AZsub#:'X');
                Write wf;
             Endif;
             If AZfnd <> *zero;
                XXdesc  = 'MC Group - ' + %editc(AZfnd:'X');
                Write wf;
             Endif;
             If (AZfEmp <> *zero or AZtEmp <> *zero);
                XXdesc  = 'MC Employer Range - ' + %editc(AZfEmp:'X') +
                 ' - ' + %editc(AZtEmp:'X');
                Write wf;
             Endif;
          Enddo;
       Enddo;

       // Read through Remark type file
       Setll *start STCTP;
       Dou %eof(STCTP);
          Read STCTP;
          If %eof or CTstat <> 'A';
             Iter;
          Endif;
          // Read through user file
          Setll *start SMSLP;
          Dou %eof(SMSLP);
             Read SMSLP;
             If %eof or SLstat <> 'A';
                Iter;
             Endif;
             If SLsecL >= CTdSLv;
                Iter;
             Endif;
             XXtype  = 'Data Restrictions';
             XXdesc  = 'DARTS Remark Code - ' + CTrmCd;
             XXuser  = SLusrP;
             XXallow = '0';
             Write wf;
          Enddo;
       Enddo;

       // Read through Agent Rate file
       Setll *start WTPCP;
       Dou %eof(WTPCP);
          Read WTPCP;
          If %eof or PCstat <> 'A';
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             If UserAuthorized(UPtype:UPlvl:PCty01:PClvl:
              PCty02:PClvl:PCty03:PClvl:PCty04:PClvl:PCty05:PClvl);
                Iter;
             Endif;
             XXtype  = 'Data Restrictions';
             XXdesc  = 'Payment Code: ' + %trim(PCkey);
             XXuser  = UPusr;
             XXallow = '0';
             Write wf;
          Enddo;
       Enddo;

       // Read through Agent Rate file
       Setll *start WTTTP;
       Dou %eof(WTTTP);
          Read WTTTP;
          If %eof or TTstat <> 'A';
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             If UserAuthorized(UPtype:UPlvl:TTty01:TTminS:
              TTty02:TTminS:TTty03:TTminS:TTty04:TTminS:TTty05:TTminS);
                Iter;
             Endif;
             XXtype  = 'Data Restrictions';
             XXdesc  = 'A/R Transaction Code: ' + %trim(TTkey);
             XXuser  = UPusr;
             XXallow = '0';
             Write wf;
          Enddo;
       Enddo;

       // Read through SSN criteria file
       Setll *start STSSP;
       Dou %eof(STSSP);
          Read STSSP;
          If %eof or SSstat <> 'A';
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             // If report can be overridden, full SSN can be seen
             If UserAuthorized(UPtype:UPlvl:SSty01:SSlvl1:SSty02:SSlvl2:
              SSty03:SSlvl3:SSty04:SSlvl4:SSty05:SSlvl5:SSty06:SSlvl6);
                SSmask = 'N';
             Endif;
             XXtype  = 'Reprt SSN';
             XXdesc  = %trim(SSctId);
             XXuser  = UPusr;
             Select;
             When SSmask = 'N';
                Iter;
             When SSmask = 'Y';
                XXallow = '0';
             When SSmask = '4';
                XXallow = 'L';
             Endsl;
             Write wf;
          Enddo;
       Enddo;

       // Read through SSN criteria file
       Setll *start WTSFP;
       Dou %eof(WTSFP);
          Read WTSFP;
          If %eof;
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             // If report can be overridden, full SSN can be seen
             If UserAuthorized(UPtype:UPlvl:SFty01:SFlvl:SFty02:SFlvl:
              SFty03:SFlvl:SFty04:SFlvl:SFty05:SFlvl:SFty06:SFlvl);
                SSmask = 'N';
             Endif;
             XXtype  = 'Reprt SSN';
             XXdesc  = %trim(SSctId);
             XXuser  = UPusr;
             Select;
             When SSmask = 'N';
                Iter;
             When SSmask = 'Y';
                XXallow = '0';
             When SSmask = '4';
                XXallow = 'L';
             Endsl;
             Write wf;
          Enddo;
       Enddo;

       // Read through Option/Function security file
       Setll *start SDOFP;
       Dou %eof(SDOFP);
          Read SDOFP;
          If %eof or OFstat <> 'A';
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             If UserAuthorized(UPtype:UPlvl:OFtype:OFlvl);
                Iter;
             Endif;
             XXtype  = 'Option/Function';
             XXdesc  = OFpgId + OFtype + %editc(OFnbr:'X');
             XXuser  = UPusr;
             XXallow = '0';
             Write wf;
          Enddo;
       Enddo;

       // Read through ASE master file
       Setll *start SMASP;
       Dou %eof(SMASP);
          Read SMASP;
          If %eof or ASstat <> 'A';
             Iter;
          Endif;
          // Read through ASE security level file
          Setll (ASco#:ASarea) SAASL;
          Dou %eof(SAASL);
             Reade (ASco#:ASarea) SAASL;
             If %eof or ASstat <> 'A';
                Iter;
             Endif;
             // Read through user file
             Setll *start SMUPP;
             Dou %eof(SMUPP);
                Read SMUPP;
                If %eof or UPstat <> 'A';
                   Iter;
                Endif;
                XXtype  = 'Application Security';
                XXdesc  = ASenv + ASfrmt + ASarea;
                XXuser  = UPusr;
                XXallow = '0';
                // Check 'view' authority
                If UserAuthorized(UPtype:UPlvl:ASvST1:ASvSL1:ASvST2:ASvSL2:
                 ASvST3:ASvSL3:ASvST4:ASvSL4:ASvST5:ASvSL5:ASvST6:ASvSL6);
                   XXallow = 'I';
                Endif;
                // Check 'edit' authority
                If UserAuthorized(UPtype:UPlvl:ASeST1:ASeSL1:ASeST2:ASeSL2:
                 ASeST3:ASeSL3:ASeST4:ASeSL4:ASeST5:ASeSL5:ASeST6:ASeSL6);
                   XXallow = '1';
                Endif;
                Write wf;
             Enddo;
          Enddo;
       Enddo;

       // Read through Application Configuration file
       Setll *start SDACP;
       Dou %eof(SDACP);
          Read SDACP;
          If %eof;
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             XXtype  = 'Actions';
             XXdesc  = ACapId;
             XXuser  = UPusr;
             XXallow = UserAuthorized(UPtype:UPlvl:ACsTp1:ACsLv1:ACsTp2:ACsLv2:
              ACsTp3:ACsLv3:ACsTp4:ACsLv4:ACsTp5:ACsLv5:ACsTp6:ACsLv6:
              ACsTp7:ACsLv7:ACsTp8:ACsLv8:ACsTp9:ACsLv9:ACsTp0:ACsLv0);
             Write wf;
          Enddo;
       Enddo;

       // Read through Admin Configuration file
       Setll *start WMEXP;
       Dou %eof(WMEXP);
          Read WMEXP;
          If %eof;
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             XXtype  = 'Actions';
             XXallow = '0';
             XXuser  = UPusr;
             If not UserAuthorized(UPtype:UPlvl:EXdltT:EXdltL);
                XXdesc  = 'Policy Deletion - Group: ' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXupdT:EXupdL);
                XXdesc  = 'Update Next Available Emp#' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXadLT:EXadLL);
                XXdesc  = 'Add Live Policy' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXadQT:EXadQL);
                XXdesc  = 'Add Quote Policy' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXpDCT:EXpDCL);
                XXdesc  = 'Policy Date Change' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXplnT:EXplnL);
                XXdesc  = 'Commission Plan Change' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXlDAT:EXlDAL);
                XXdesc  = 'Audit Billing for Large Deductible Policies' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXqTIT:EXqTIL);
                XXdesc  = 'Quote Transfer:Immediate' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:EXqTBT:EXqTBL);
                XXdesc  = 'Quote Transfer:Batch' +
                 ' - Group: ' + %editc(EXfnd:'Z');
                Write wf;
             Endif;
          Enddo;
       Enddo;

       // Read through Claim Configuration file
       Setll *start WMCXP;
       Dou %eof(WMCXP);
          Read WMCXP;
          If %eof;
             Iter;
          Endif;
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             XXtype  = 'Actions';
             XXallow = '0';
             XXuser  = UPusr;
             If not UserAuthorized(UPtype:UPlvl:CXcDT:CXsecL:
              CXcDt2:CXsecL:CXcDT3:CXsecL:CXcDt4:CXsecL:CXcDt5:CXsecL);
                XXdesc  = 'Claim Deletion - Group: ' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXckDT:CXsecC:
              CXckD2:CXsecC:CXckD3:CXsecC:CXckD4:CXsecC:CXckD5:CXsecC);
                XXdesc  = 'Override CX Date Edit' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXoCAT:CXckDL:
              CXoCA2:CXsecL:CXoCA3:CXsecL:CXoCA4:CXsecL:CXoCA5:CXsecL);
                XXdesc  = 'EOB Check Request Deletion' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXdPPT:CXdltD:
              CXdPP2:CXdltD:CXdPP3:CXdltD:CXdPP4:CXdltD:CXdPP5:CXdltD);
                XXdesc  = 'Delete Pended Payment' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXpDCT:CXpyOd:
              CXpDC2:CXpyOd:CXpDC3:CXpyOd:CXpDC4:CXpyOd:CXpDC5:CXpyOd);
                XXdesc  = 'Pay on Denied/Delayed Claims' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXvSZT:CXvSZL:
              CXvSZ2:CXvSZL:CXvSZ3:CXvSZL:CXvSZ4:CXvSZL:CXvSZ5:CXvSZL);
                XXdesc  = 'Void/Stop/Reverse Payment' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXfSVT:CXfSVL:
              CXfSV2:CXfSVL:CXfSV3:CXfSVL:CXfSV4:CXfSVL:CXfSV5:CXfSVL);
                XXdesc  = 'Override Full Salary Voucher Edit' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXaut1:CXauSL:
              CXaut2:CXauSL:CXaut3:CXauSL:CXaut4:CXauSL:CXaut5:CXauSL);
                XXdesc  = 'Override Estimated/Audited Payroll Edit' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXds01:CXdsLv:
              CXds02:CXdsLv:CXds03:CXdsLv:CXds04:CXdsLv:CXds05:CXdsLv);
                XXdesc  = 'Change Decision Date' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXdpT1:CXdpLv:
              CXdpT2:CXdpLv:CXdpT3:CXdpLv:CXdpT4:CXdpLv:CXdpT5:CXdpLv);
                XXdesc  = 'Release Exact Duplicate Payment' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXdpT6:CXdpL2:
              CXdpT7:CXdpL2:CXdpT8:CXdpL2:CXdpT9:CXdpL2:CXdpT0:CXdpL2);
                XXdesc  = 'Release Possible Duplicate Payment' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXmbS1:CXmbS:
              CXmbS2:CXmbS:CXmbS3:CXmbS:CXmbS4:CXmbS:CXmbS5:CXmbS);
                XXdesc  = 'CMS Submission' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
             If not UserAuthorized(UPtype:UPlvl:CXrbS1:CXrbS:
              CXrbS2:CXrbS:CXrbS3:CXrbS:CXrbS4:CXrbS:CXrbS5:CXrbS);
                XXdesc  = 'CMS Re-Submission' +
                 ' - Group: ' + %editc(CXfnd:'Z');
                Write wf;
             Endif;
          Enddo;
       Enddo;

       // Read the system control file
       Chain 'FB' sfb01;
       If %found and %subst(FBal05:5:1) = 'Y';
          // Read through user file
          Setll *start SMUPP;
          Dou %eof(SMUPP);
             Read SMUPP;
             If %eof or UPstat <> 'A';
                Iter;
             Endif;
             If UPlvl < FBdt02;
                XXtype  = 'Actions';
                XXdesc  = 'Delete EOB';
                XXuser  = UPusr;
                XXallow = '0';
                Write wf;
             Endif;
          Enddo;
       Endif;

       // Read through claim security file
       Setll *start WMCUP;
       Dou %eof(WMCUP);
          Read WMCUP;
          If %eof or CUstat <> 'A';
             Iter;
          Endif;
          XXtype  = 'Record Specific';
          XXdesc  = 'Claim - ' + %editc(CUfnd:'X') + '-' + %editc(CUfyr:'X') +
           '-' + %editc(CUcase:'X');
          XXuser  = CUaUsr;
          XXallow = '1';
          Write wf;
       Enddo;

       // Read through user file for limits
       Setll *start WMUPP;
       Dou %eof(WMUPP);
          Read WMUPP;
          If %eof or UPstat <> 'A';
             Iter;
          Endif;
          XXuser  = UPusr;
          XXtype  = 'Actions';
          Select;
          When UPfl2 = 'Y';
             xxAllow = '1';
          Other;
             xxAllow = '0';
          Endsl;
          XXdesc = 'Claimant Name(s)/Address(es)';
          Write wf;

          XXtype  = 'Limits';
          XXallow = 'L';
          If UPpAl  > 0;
             XXdesc = 'Payment - ' + %trim(%editc(UPpAl:'J'));
             Write wf;
          Endif;
          If UPmRal > 0;
             XXdesc = 'Medical Payment - ' + %trim(%editc(UPmRal:'J'));
             Write wf;
          Endif;
          If UPcRal > 0;
             XXdesc = 'Comp Payment - ' + %trim(%editc(UPcRal:'J'));
             Write wf;
          Endif;
          If UPeRal > 0;
             XXdesc = 'Expense Payment - ' + %trim(%editc(UPeRal:'J'));
             Write wf;
          Endif;
          If UPlRal > 0;
             XXdesc = 'Legal Payment - ' + %trim(%editc(UPlRal:'J'));
             Write wf;
          Endif;
          If UPral > 0;
             XXdesc = 'Cumulative Reserve - ' + %trim(%editc(UPral:'J')) +
              ' for ' + UPrsv1;
             If UPrsv2 <> *blank;
                XXdesc = %trim(XXdesc) + ', ' + UPrsv2;
             Endif;
             If UPrsv3 <> *blank;
                XXdesc = %trim(XXdesc) + ', ' + UPrsv3;
             Endif;
             If UPrsv4 <> *blank;
                XXdesc = %trim(XXdesc) + ', ' + UPrsv4;
             Endif;
             Write wf;
          Endif;
       Enddo;

       *inlr = *on;
      /End-free
     p*=======================================================================
     PGroupAuthorized  b
     DGroupAuthorized  pi             1
     D inItem                         9  0 const
     D inGroup                        9  0 const

     DsaveGroup        s              9  0

      /Free
       saveGroup = inGroup;
       Chain inGroup ATGROUPSP;
       Clear AAallow;
       Chain (inItem:inGroup) ATAUTHP;
       If not %found and AGparent <> 0;
          AAallow = GroupAuthorized(inItem:AGparent);
       Endif;
       Chain saveGroup ATGROUPSP;
       Return AAallow;
      /End-free

     PGroupAuthorized  e
     p*=======================================================================
     PUserAuthorized   b
     DUserAuthorized   pi             1n
     D inUserType                     1    const
     D inUserLvl                      3  0 const
     D inCmpType1                     1    const
     D inCmpLvl1                      3  0 const
     D inCmpType2                     1    const options(*nopass)
     D inCmpLvl2                      3  0 const options(*nopass)
     D inCmpType3                     1    const options(*nopass)
     D inCmpLvl3                      3  0 const options(*nopass)
     D inCmpType4                     1    const options(*nopass)
     D inCmpLvl4                      3  0 const options(*nopass)
     D inCmpType5                     1    const options(*nopass)
     D inCmpLvl5                      3  0 const options(*nopass)
     D inCmpType6                     1    const options(*nopass)
     D inCmpLvl6                      3  0 const options(*nopass)
     D inCmpType7                     1    const options(*nopass)
     D inCmpLvl7                      3  0 const options(*nopass)
     D inCmpType8                     1    const options(*nopass)
     D inCmpLvl8                      3  0 const options(*nopass)
     D inCmpType9                     1    const options(*nopass)
     D inCmpLvl9                      3  0 const options(*nopass)
     D inCmpType10                    1    const options(*nopass)
     D inCmpLvl10                     3  0 const options(*nopass)

     DCheckSec         PR                  extpgm('SRCCSC')
     D userType                       1    const
     D userLevel                      3    const
     D cmpType1                       1    const
     D cmpLevel1                      3    const
     D type                           1    const
     D failed                         1
     D cmpType2                       1    const
     D cmpType3                       1    const
     D cmpType4                       1    const
     D cmpType5                       1    const
     D cmpType6                       1    const
     D cmpLevel2                      3    const
     D cmpLevel3                      3    const
     D cmpLevel4                      3    const
     D cmpLevel5                      3    const
     D cmpLevel6                      3    const
     D cmpType7                       1    const
     D cmpType8                       1    const
     D cmpType9                       1    const
     D cmpType10                      1    const
     D cmpLevel7                      3    const
     D cmpLevel8                      3    const
     D cmpLevel9                      3    const
     D cmpLevel10                     3    const

     dchkTypeLvl       c                   '1'
     dchkLvlOnly       c                   '2'
     dcmpType1         s              1    inz
     dcmpType2         s              1    inz
     dcmpType3         s              1    inz
     dcmpType4         s              1    inz
     dcmpType5         s              1    inz
     dcmpType6         s              1    inz
     dcmpType7         s              1    inz
     dcmpType8         s              1    inz
     dcmpType9         s              1    inz
     dcmpType10        s              1    inz
     dcmpLevel1        s              3    inz
     dcmpLevel2        s              3    inz
     dcmpLevel3        s              3    inz
     dcmpLevel4        s              3    inz
     dcmpLevel5        s              3    inz
     dcmpLevel6        s              3    inz
     dcmpLevel7        s              3    inz
     dcmpLevel8        s              3    inz
     dcmpLevel9        s              3    inz
     dcmpLevel10       s              3    inz
     disFailed         s              1n

      /Free
       cmpType1  = inCmpType1;
       cmpLevel1 = %editc(inCmpLvl1:'X');
       If %parms >= 5;
          cmpType2  = inCmpType2;
          cmpLevel2 = %editc(inCmpLvl2:'X');
       Endif;
       If %parms >= 7;
          cmpType3  = inCmpType3;
          cmpLevel3 = %editc(inCmpLvl3:'X');
       Endif;
       If %parms >= 9;
          cmpType4  = inCmpType4;
          cmpLevel4 = %editc(inCmpLvl4:'X');
       Endif;
       If %parms >= 11;
          cmpType5  = inCmpType5;
          cmpLevel5 = %editc(inCmpLvl5:'X');
       Endif;
       If %parms >= 13;
          cmpType6  = inCmpType6;
          cmpLevel6 = %editc(inCmpLvl6:'X');
       Endif;
       If %parms >= 15;
          cmpType7  = inCmpType7;
          cmpLevel7 = %editc(inCmpLvl7:'X');
       Endif;
       If %parms >= 17;
          cmpType8  = inCmpType8;
          cmpLevel8 = %editc(inCmpLvl8:'X');
       Endif;
       If %parms >= 19;
          cmpType9  = inCmpType9;
          cmpLevel9 = %editc(inCmpLvl9:'X');
       Endif;
       If %parms >= 21;
          cmpType10  = inCmpType10;
          cmpLevel10 = %editc(inCmpLvl10:'X');
       Endif;
       CheckSec(inUserType:%editc(inUserLvl:'X'):
        cmpType1:cmpLevel1:chkTypeLvl:isFailed:cmpType2:cmpType3:cmpType4:
        cmpType5:cmpType6:cmpLevel2:cmpLevel3:cmpLevel4:cmpLevel5:cmpLevel6:
        cmpType7:cmpType8:cmpType9:cmpType10:
        cmpLevel7:cmpLevel8:cmpLevel9:cmpLevel10);
       Return not isFailed;
      /End-free
     PUserAuthorized   e
