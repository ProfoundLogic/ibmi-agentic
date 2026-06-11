     H Copyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
     H*====================================================================
     H/TITLE CRTSCRPR - Create table info for Scriptura Phase 1
     H*====================================================================
     H*  Description:
     H*
     H*  This program will write records to the system table file for
     H*  Scriptura phase 1.
     H*
     H*  Created:  10/14/15  C:65  Project: SY251
     H*====================================================================
     H*  Revisions:
     H*
/*  0H* 08/04/16 C:65  Project: SY371
/*   H*                Remove directory field - unused.
     H*
/*  0H*  5/26/16 C:65  Project: SY251
/*   H*                Populate the SCR_DTM folder to the client's
/*   H*                base folder for datacenter clients.
     H*
     H*====================================================================
     FSSYLF     if   e           k disk
     fstscp     if a e             disk    rename(psc01:psc0p)
     fstgtl     if a e           k disk    rename(pgt01:pgt00)
     fstdel     if a e           k disk    rename(pde01:pde00)
     D
     d SubWCMS         s             57    dim(12) ctData perRcd(1)
     d SubWCMC         s             57    dim(2) ctData perRcd(1)
     d psds          esds                  extname(szq1p)
     D
     D kkKey           s                   like(SYKEY)
     d intCo#          s              3  0
     d x               s              3  0
     D xxKey           s                   like(GTKey)
     D xxTbid          s                   like(GTTbid)
     D xxCat           s                   like(DECat)
     D xxSubC          s                   like(DESubC)
     D*** begin add ***  05/26/16  ****************************************
     D ##suffix        s              3a
     D*** end add *****  05/26/16  ****************************************
     D
     d/copy sprUsrPrms
     d/copy sPrDate
     C*
     C* Generic Table File Key List
     C*
     C     keySTGTL      Klist
     C                   Kfld                    intCo#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C* Sub Category Table File Key List
     C*
     C     keySTDEL      Klist
     C                   Kfld                    intCo#
     C                   Kfld                    xxCat
     C                   Kfld                    xxSubC
     C*
     C*** begin add ***  05/26/16  ****************************************
     C                   call      'SA030R'
     C                   parm      '   '         ##suffix
     C*** end add *****  05/26/16  ****************************************
      /free

       kkKey = 'SY';
       Chain kkKey ssy01;

       intCo# = GetUserInfo(q1user);

       //* Write default info to scriptura module file is no record exists.

       Read psc0p;
       If %eof;

          SCSUrl = 'http://xx.xx.xx.xx:xxxxx/web/StoneRiver-livedocs?letter=';
          SCXMLF = '/SCR_DTM/';
       //*** begin del ***  08/04/16  *******************************
       //*****   SCXMLD = '/SCR_DTM/';
       //*** end del *****  08/04/16  *******************************
          SCXMLA = '/SCR_DTM/ARCHIVE/';
       //*** begin add ***  05/26/16  *******************************
          If ##suffix <> ' ';
             SCXMLF = '/PDS/' + ##suffix + '/SCR_DTM/';
       //*** begin del ***  08/04/16  *******************************
       //****SCXMLD = SCXMLF;
       //*** end del *****  08/04/16  *******************************
             SCXMLA = '/PDS/' + ##suffix + '/SCR_DTM/ARCHIVE/';
          Endif;
       //*** end add *****  05/26/16  *******************************
          SCFLG3 = 'Y';
          SCFLG4 = 'N';
          SC1101 = 6;
          SC1102 = 14;
          SCLANG01 = 'English';
          SCLANG02 = 'Spanish';
          SCLANG03 = 'French';
          SCLANG04 = 'German';
          SCLANG05 = 'Dutch';
          SCLANG06 = 'Russian';
          SCLANG07 = 'Arabic';
          SCLANG08 = 'Italian';
          SCLANG09 = 'Greek';
          SCLANG10 = 'Polish';
          SCLANG11 = 'Norweigan';
          SCLANG12 = 'Japanese';
          SCLANG13 = 'Chinese';
          SCLANG14 = 'Korean';

          //*** begin add ***  01/06/16  ******************************************
          SCCODE01 = 'en';
          SCCODE02 = 'es';
          SCCODE03 = 'fr';
          SCCODE04 = 'de';
          SCCODE05 = 'nl';
          //*** end add *****  01/06/16  ******************************************

          SCPgm  = Q1PGM;
          SCUser = Q1USER;
          SCTime = Timeto6(SystemTime());
          SCDt   = Dateto7(SystemDate());
          Write psc0p;

       Endif;

       //* Write categories to system table file.

       xxTBID = 'DOCS';

       //* WCMS system

       If SYWCIF = 'Y';

          xxKey  = 'PS';
          Chain(n) keySTGTL pgt00;
          If not %found;
             Clear pgt00;
             GTCo#  = intCo#;
             GTTBID = xxTBID;
             GTKEY  = xxKey;
             GTSTAT = 'A';
             GTDESC = 'Policy';
             GTPgm  = Q1PGM;
             GTUser = Q1USER;
             GTTime = Timeto6(SystemTime());
             GTDt   = Dateto7(SystemDate());
             Write pgt00;
          Endif;

          xxKey  = 'CS';
          Chain(n) keySTGTL pgt00;
          If not %found;
             Clear pgt00;
             GTCo#  = intCo#;
             GTTBID = xxTBID;
             GTKEY  = xxKey;
             GTSTAT = 'A';
             GTDESC = 'Claim';
             GTPgm  = Q1PGM;
             GTUser = Q1USER;
             GTTime = Timeto6(SystemTime());
             GTDt   = Dateto7(SystemDate());
             Write pgt00;
          Endif;

       //* Write sub categories to sub category table file.

          For x = 1 to 12;

             xxCat  = %subst(SubWCMS(x):1:2);
             xxSubC = %subst(SubWCMS(x):3:5);

             Chain(n) keySTDEL pde00;
             If not %found;

                Clear pde00;
                DECo#  = intCo#;
                DECat  = xxCat;
                DESubC = xxSubC;

                DEDesc = %subst(SubWCMS(x):8:50);
                DEStat = 'A';

                DEIUsr = Q1USER;
                DEIDt  = Dateto7(SystemDate());
                DEPgm  = Q1PGM;
                DEUser = Q1USER;
                DETime = Timeto6(SystemTime());
                DEDt   = Dateto7(SystemDate());

                Write pde00;
             Endif;

          Endfor;

       Endif;

       //* WCMC system

       If SYFL02 = 'Y';

          xxKey  = 'MC';
          Chain(n) keySTGTL pgt00;
          If not %found;
             Clear pgt00;
             GTCo#  = intCo#;
             GTTBID = xxTBID;
             GTKEY  = xxKey;
             GTSTAT = 'A';
             GTDESC = 'Medical Case';
             GTPgm  = Q1PGM;
             GTUser = Q1USER;
             GTTime = Timeto6(SystemTime());
             GTDt   = Dateto7(SystemDate());
             Write pgt00;
          Endif;

          xxKey  = 'PC';
          Chain(n) keySTGTL pgt00;
          If not %found;
             Clear pgt00;
             GTCo#  = intCo#;
             GTTBID = xxTBID;
             GTKEY  = xxKey;
             GTSTAT = 'A';
             GTDESC = 'Pre-Certification';
             GTPgm  = Q1PGM;
             GTUser = Q1USER;
             GTTime = Timeto6(SystemTime());
             GTDt   = Dateto7(SystemDate());
             Write pgt00;
          Endif;

       //* Write sub categories to sub category table file.

          For x = 1 to 2;

             xxCat  = %subst(SubWCMC(x):1:2);
             xxSubC = %subst(SubWCMC(x):3:5);

             Chain(n) keySTDEL pde00;
             If not %found;

                Clear pde00;
                DECo#  = intCo#;
                DECat  = xxCat;
                DESubC = xxSubC;

                DEDesc = %subst(SubWCMC(x):8:50);
                DEStat = 'A';

                DEIUsr = Q1USER;
                DEIDt  = Dateto7(SystemDate());
                DEPgm  = Q1PGM;
                DEUser = Q1USER;
                DETime = Timeto6(SystemTime());
                DEDt   = Dateto7(SystemDate());

                Write pde00;
             Endif;

          Endfor;

       Endif;

       *inlr = *on;
      /end-free

**  WCMS data
PSACCTNAccounting
PSAGENTAgency
PSAUDITAudit
PSLOSSCLoss Control
PSMIDENMid-term Endorsement
PSNEWBSNew Business
PSRENEWRenewal Business
PSSAFTYSafety
CSCLAIMClaimant
CSEMPLREmployer
CSMEDICMedical
CSSUBROSubrogation
**  WCMC data
MCMEDCSMedical Case Management
PCPROVBProvider Bills
