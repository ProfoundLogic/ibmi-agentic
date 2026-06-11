     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
    H/TITLE West Virginia State Specific Information Entry Program
     H Bnddir('WTBNDDIR')
    H*********************************************************************
    H*  Description:                                                     *
    H*                                                                   *
    H*  Called from: W0070R12                                            *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*********************************************************************
    H*  Created on 05/04/2012 by c:255 for Project 114DET                *
    H*********************************************************************
    H*  Revisions:                                                       *
     H*                                                                   *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 06/19/12 C:255 Project 114DET: Priss#19                           *
     H*                Moved Screen field values in Database correctly.   *
     H*                                                                   *
     H*                                                                   *
    H*********************************************************************
     FW0070FWV  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     Fwmcml1    if   e           k disk
     F                                     infds(DBFDS3)
     F*
     Fwmfnl     if   e           k disk
     F*
     Fwmeml     if   e           k disk
     F*
     Fwmnil     uf a e           k disk
     F*
     Fwmcxl     if   e           k disk
     D/COPY SCOPYPR01D
     D/COPY Sprdate
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************

     D GetclientID     pr                  ExtPgm('RTVCLTID')
     D cltid                          3
      *
     D SRCENTER        pr                  ExtPgm('SRCENTER')
     D ##data                        80
     D ##leng                         3  0
      *
     D ComAPI          pr                  ExtPgm('WA001R')
     D ##co#                          3  0
     D ##fnd                          3  0
     D ##conm                              like(##conm)
     D ##fnam                        40
      *
     D W0190R          pr                  ExtPgm('W0190R')
     D xxprds                        40
     D xxcall                              LIKE (@@CALL)
     D @@co#                          3
     D @@fnd                          3
     D @@fyr                          3
     D @@case                         7
     D xaacdt                         8
     D xxrtcd                         1
      *
     D W0070R08        pr                  ExtPgm('W0070R08')
     D @@kc                           1
     D @@kl                           1
     D @@mode                         1
     D @@rrn                          9  0
     D @@cont                         1
      *
     D SC010R          pr                  ExtPgm('SC010R')
     D qakey                         40
     D qqpgm                         10

     DDBFDS3           ds
     D DBRR3B                397    400b 0
      *
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@cont          s              1
     D cltid           s              3
     D qakey           s             40
     D qqpgm           s             10
     D xaacdt          s              8
     D xxrtcd          s              1
     D xxprds          s             40
     D numdate7        s              7  0
     D numdate8        s              8  0
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('West Virginia State Specific -
     D                                     Info')
     D @@call          s              1
     D @@prds          s             40
     D @@rtcd          s              1
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D xxss#           ds             9
     D  ss1                    1      3
     D  ss2                    4      5
     D  ss3                    6      9
     D xxdiv           s              5
     D @@co#           s              3
     D @@fnd           s              3
     D @@fyr           s              3
     D @@case          s              7
     D xxacdt          s              7
     D xxdate          ds             8
     D  mm                     1      2
     D  dd                     4      5
     D  ccyy                   6      8
     D xxtime          ds             4
     D  hr                            2  0
     D  mn                            2  0
     D kkco#           s                   like(cmco#)
     D kkfnd           s                   like(cmfnd)
     D kkfyr           s                   like(cmfyr)
     D kkcase          s                   like(cmcase)
     D kkst            s                   like(cmjrst)
     D pos1            s              2  0
     D pos2            s              2  0
     D length          s              2  0
     D qckey           ds            40
     D    xxco#                1      3  0
     D    xxfnd                4      6  0
     D    xxfyr                7      9  0
     D    xxcase              10     16  0

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*

    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@rtcd
ks01 C                   parm                    @@cont
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
    C*  KEY LIST FOR CLAIM/DARTS CONTROL FILE
    C*
     C     cxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
     C* State NOI record file
     C*
     C     niky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkst
     C*
    C*  KEY LIST FOR THE FUND MASTER FILE
    C*
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
    C*  KEY LIST FOR THE FUND MASTER FILE
    C*
     C     emky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
    C*
     C/COPY SCOPYPR01C
    
    
      //DSCAPI - Call API for getting descriptions (use 91 on error)
    
    C*
      /Free
       BegSr DSCAPI;
          Callp comapi(##co#: ##fnd: ##conm: ##fnam);
       EndSr;
    
    
      //ERRCK1 - Error checking for srceen 1 (SCR01)
    
       BegSr Errck1;
        Dow *In90 = *Off;

       //Date of Report must be valid if entered
           If scdt2 <> 0;
             scdt2 = dtcheck(scdt2);
              If scdt2 = 0;
                 *In71 = *On;
                 *In90 = *On;
                  Leave;
              EndIf;
           EndIf;

       //Awareness of prior injury?
           If SC1a3 <> 'Y' And SC1a3 <> 'N' And
            SC1a3 <> ' ' ;
             *In72 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //Reason to question injury?
           If SC1a4 <> 'Y' And SC1a4 <> 'N' And
            SC1a4 <> ' ' ;
             *In73 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //Emergency room
           If SC1a5 <> 'Y' And SC1a5 <> 'N' And
            SC1a5 <> ' ' ;
             *In74 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //Hospitalized.
           If SC1a6 <> 'Y' And SC1a6 <> 'N' And
            SC1a6 <> ' ' ;
             *In75 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //Light Duty available
           If SC1a7 <> 'Y' And SC1a7 <> 'N' And
            SC1a7 <> ' ' ;
             *In76 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //Hour/Day/Week/Month validation
           If SC2a2 <> 'H' And SC2a2 <> 'D' And
            SC2a2 <> 'W' And SC2a2 <> 'M' And SC2a2 <> ' ';
             *In77 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //Alternative of Modified work?
           If SC1a10 <> 'Y' And SC1a10 <> 'N' And
            SC1a10 <> ' ' ;
             *In78 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //Hour/Day/Week/Month validation
           If SC2a3 <> 'H' And SC2a3 <> 'D' And
            SC2a3 <> 'W' And SC2a3 <> 'M' And SC2a3 <> ' ';
             *In79 = *On;
             *In90 = *On;
             Leave;
           EndIf;

       //All edits passed
         Exsr Upwrtr;

         Leave;
       Enddo;
       //Errck1 ends here
       endsr;
    
      //ERRCK2 - Error checking for srceen 2 (SCR02)
    
       BegSr ERRCK2;
       EndSr;
    
      //KJDO   - Processing for F10
    
       BegSr kjdo;
      EndSr;
    
      //MVDBSC - Move database to screen filed
    
       BegSr MVDBSC;
    
         Chain(n) niky pni01;
         If %found(WMNIL);

           SC1A8  = NI1A8;
           SC1A9  = NI1A9;

           SCDT2  = dtMDYY(NIDT2);
           If SCdt2 <> *Zeros;
             SCA10  = 'X';
           EndIf;

           SC60A7 = NI60a7;
           SC80A1 = NI80a1;
           SC80A2 = NI80a2;
           SC1a3  = NI1a3;
           SC1a4  = NI1a4;
           SC30a5 = NI30a5;
           SC1a5  = NI1a5;
           SC1a6  = NI1a6;
           SC5n1  = NI5n1;
           SC1a7  = NI1a7;
           SC92n1 = NI92n1;
           SC2a2  = NI2a2 ;
           SC1a10 = NI1a10;
           SC92n2 = NI92n2;
           SC2a3  = NI2a3 ;
           SC92n3 = NI92n3;
           SC92n4 = NI92n4;

         Else;
           Clear  SC1a8  ;
           Clear  SC1a9  ;
           Clear  SCA10  ;
           Clear  SCDT2  ;
           Clear  SC60A7 ;
           Clear  SC80A1;
           Clear  SC80A2;
           Clear  SC1a3 ;
           Clear  SC1a4 ;
           Clear  SC30a5;
           Clear  SC1a5 ;
           Clear  SC1a6 ;
           Clear  SC5n1 ;
           Clear  SC1a7 ;
           Clear  SC92n1;
           Clear  SC2a2 ;
           Clear  SC1a10;
           Clear  SC92n2;
           Clear  SC2a3 ;
           Clear  SC92n3;
           Clear  SC92n4;
         EndIf;
      //MVDBSC ends here
      EndSr;
    
      // MVSCDB - Move screen to Database filed
    
       Begsr MVSCDB;

           NI1A8  = SC1A8;
           NI1A9  = SC1A9;

           If SCdt2 <>  *Zeros;
             NIdt2= DateTo7(CvtToDate(SCdt2));
       //** Beg Add ***** 06/19/12 ***************************************
           Else;
             NIdt2 = *Zeros;
       //** End Add ***** 06/19/12 ***************************************
           EndIf;
           NI60A7 = SC60a7;
           NI80A1 = SC80a1;
           NI80A2 = SC80a2;
           NI1a3  = SC1a3;
           NI1a4  = SC1a4;
           NI30a5 = SC30a5;
           NI1a5  = SC1a5;
           NI1a6  = SC1a6;
           NI5n1  = SC5n1;
           NI1a7  = SC1a7;
           NI92n1 = SC92n1;
           NI2a2  = SC2a2 ;
           NI1a10 = SC1a10;
           NI92n2 = SC92n2;
           NI2a3  = SC2a3 ;
           NI92n3 = SC92n3;
           NI92n4 = SC92n4;
           NIuser= Q1user;
           NIpgm = Q1pgm;
           NItime = TimeTo6(SystemTime);
           NIdt = DateTo7(SystemDate);
       //MVSCDB ends here
       EndSr;
    
      //NXTPGM - Call next programs if set up to do so
    
    
       BegSr  NXTPGM;

       // IF @@call = A, GO ON TO THE NEXT PROGRAM IN ADD MODE.
       // IF @@call = Y, GO ON TO THE NEXT PROGRAM IN CHANGE MODE.
       // IF @@call = Z, GO ON TO THE NEXT PROGRAM IN DISPLAY MODE.

         If CltId <> '133';

           Dow *In90 = *Off;
             If @@call = 'Y' or @@call = 'Z';
               xxrtcd = '1';
               xaacdt = *Zeros;

               Callp W0190R(xxprds: xxcall: @@co#: @@fnd: @@fyr: @@case:
                            xaacdt: xxrtcd);

               If xxrtcd = 'B';
                 *In90 = *On;
                 Leave;
               EndIf;

               If xxrtcd = 'A';
                 *In90 = *Off;
                 Leave;
               EndIf;
       // If @@call = 'Y' ends here
             EndIf;

             If xxcall = 'A';
               Chain cxky pcx01;
               If %found(WMCXL);

                 Select;
                   When cxdben = 'Y';
                     xxrtcd = '1';
                     Callp W0190R(xxprds: xxcall: @@co#: @@fnd: @@fyr: @@case:
                                  xaacdt: xxrtcd);

                   When cxdart = 'Y';
                     qakey = *Blanks;
                     qakey = 'W0070R';
                     Callp SC010R(qakey: qqpgm);

                 EndSl;

                 If xxrtcd = 'B';
                 *In90 = *On;
                 Leave;
                 EndIf;

                 If xxrtcd = 'A';
                   *In90 = *Off;
                   Leave;
                 EndIf;

        //Chain @ PCX01 ends here
               EndIf;
             EndIf;
             Leave;
           EndDo;
        //Cleint Id <> 133 ends here
         EndIf;

         If cltid = '133' And @@Call <> *Blanks;
           Chain CMky PCM01;
           @@rrn = DBrr3b;

           Select;
             When @@Call = 'A';
               @@mode = 'A';

             When @@Call = 'Y';
               @@mode = 'C';

             When @@Call = 'Z';
               @@mode = 'D';

           EndSl;

           CallP W0070R08(@@kc: @@kl: @@mode: @@rrn: @@cont);
           *Inkc = @@kc;
           *Inkl = @@kl;

           If *Inkl;
             *In90 = *On;
             *Inkl = *off;
           EndIf;

         EndIf;

       // NXTPGM ends here
       EndSr;
    
      //OTHFKS - Processing for other functions keys
    
       Begsr OthFks;
         If *Inkj = *On;
            Exsr KJdo;
        EndIf;
      EndSr;

      // PGMEND - End of program processing
    
       BegSr PgmEnd;
         If cltid = '133';
           If *Inkl = *On;
             @@rtcd = 'B';
           Endif;

           If *Inkc = *On;
             @@rtcd = 'A';
           Endif;
         Endif;
       EndSr;
    
      //PGMSTR - Used for any start of program processing (one-time)
    
       Begsr PgmStr;
         //Retrieve Client Id.
         CallP GetClientID(CltId);

         If q1prms >=1 And @@prds <> *Blanks;
           ##data = @@prds;
         Else;
           ##data = optnm;
         EndIf;
    
         ##leng = 40;
         Callp srcenter(##data: ##leng);
        SCprds = ##data;

        If q1prms >= 2;
           xxcall = @@call;
         Else;
           Clear xxcall;
         EndIf;
    
         kkco# = %Int(@@co#);
         kkfnd = %Int(@@fnd);
         kkfyr = %Int(@@fyr);
        kkcase =%Int( @@case);

        Chain cmky pcm01;
         scfnd = cmfnd;
         scfnam = ' ' ;

        Chain fnky pfn01;
         If %found(WMFNL);
           scfnam = fnname;
         EndIf;

         If CMss# <> *blanks;
           xxss# = cmss#;
         //***  Begin Chg  ***  06/09/20  **********************
           //***scss# = %trim(ss1) + '-' + %trim(ss2) + '-' + %trim(ss3);
           scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
         //***  End Chg  ***  06/09/20  **********************
         EndIf;

         SCcnam = *Blanks;

         sccnam = %trim(cmlsn) + ',' + ' ' + %trim(cmfsn) + ' ' +
                    %trim(cmmid);

         SCedn = *Blanks;
         Chain EMky pem01;
         SCedn = %trim(%char(ememp#)) + ' ' + %trim(%char(emdiv)) + ' '  +
                    %trim(emname);

         SCyrcs = %trim(@@fyr) + '-' + %trim(@@case);
         SCscl# = *Blanks;
        SCscl# = cmacl#;
         SCacdt = dtMDYY(cmacdt);

         kkst = cmjrst;

         ExSr mvdbsc;
       EndSr;
    
      //PREDSP - Pre-processing before displaying (every time)

       BegSr predsp;
       EndSr;
    
      //PRSCTL - Process control after no errors found (Call Submitter)
    
       BegSr prsctl;
         Exsr nxtpgm;
       EndSr;

    
      // UPWRTR - Update/Write Database file
      BegSr upwrtr;
         Chain niky pni01;
         ExSr mvscdb;
         If %found(WMNIL);
           update pni01;
         Else;
           nico#  = cmco#;
           nifnd  = cmfnd;
           nifyr  = cmfyr;
           nicase = cmcase;
           nist   = 'WV';
           niiusr = niuser;
           niitm = nitime;
           niidt = nidt;
           write pni01;
         EndIf;

       EndSr;
      /End-Free
