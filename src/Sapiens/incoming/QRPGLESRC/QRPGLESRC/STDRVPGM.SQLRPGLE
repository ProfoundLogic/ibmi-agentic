     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE STDRVPGM - Driver Program for Action Panel
     h/DEFINE   PROFOUNDUI
     h DFTACTGRP(*no) ACTGRP('WTWCMS')
     h BndDir('STBNDDIR':'QC2LE')
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Be Used to call Action Panel
     h*                Programs.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 3/2/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h*    06/03/21  C:33   Project: WC2196
     h*                     Incorporate group processing.
     h*
     h*    06/02/21  C:33   Support: 93938
     h*                     Load the rrn for the request for later processing.
     h*
     h*    09/25/17  C:33   Support: 48910
     h*                     Account for missing detail record.
     h*
     h*    06/11/15  C:062  Support: 35284(99)
     h*                     Correct load of rrn into numeric field.
     h*
     h*    03/12/15  C:279  Project: DR337
     h*                     Modified to check user authority before accessing
     h*                     Claims and Policy.
     h*
     h*    03/23/12  C:191  Project: Transform
     h*                      Move to ST*
     h*
     h**********************************************************************
     Fstdrvpgm  cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     fstdrl     if   e           k disk    rename(pdr01:pdr00)
     fsmazl     if   e           k disk    rename(paz01:paz00)
     fwdelp     if   e             disk    rename(pel01:pel0p) usropn
     fwmcmp     if   e             disk    rename(pcm01:pcm0p) usropn
     fwmupl     if   e           k disk    rename(pup01:pup00) usropn
     fwmemp     if   e             disk    rename(pem01:pem0p) usropn
     d* Proto-types
     d***  Begin Add  ***  06/02/21  ***********************************
     DDelayJob         pr            10u 0 extproc('sleep')
     D interval                      10u 0 value
     d***  End   Add  ***  06/02/21  ***********************************

     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d* Program to be called
     d CallProgram     pr                  ExtPgm(strCallPgm)
     d   inRRN                        9
     d   inMode                       1
     d   outCallPgm                  10
     d   outTab                       5    Options(*NoPass)
     d   inQuote                      1    Options(*NoPass)

     d GetRstrct       pr                  extpgm('WA067R')
     d ##co                           3
     d ##fnd                          3
     d ##fyrc                         3
     d ##casec                        7
     d ##user                        10
     d ##rtcd                         1

     d GetAuth         pr                  extpgm('SA008R')
     d ##co#                          3  0 const
     d ##user                        10    const
     d ##appl                         1    const
     d ##rtcd                         1
     d ##auth                         1          options(*nopass)
     d ##grp                          3  0 const options(*nopass)
     d ##emp#                         9  0 const options(*nopass)
     d ##sub#                         5  0 const options(*nopass)

     d GetSystem       pr                  ExtPgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     D*Variables
     D @@RRN           s              9
     D @@Mode          s              1
     D @@Pgm           s             10
     D @@Tab           s              5
     d @@Quote         s              1

     D inRRN           s              9    inz
     D inMode          s              1    inz
     D inTab           s              5    inz('00000')
     D inQuote         s              1    inz
     D outNextPgm      s             10    inz
     D outTab          s              5    inz('00000')
     D strCallPgm      s             10    inz
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
     d isAuth          s               n   inz(*on)
     d intRrn          s              9  0 inz
     d intGrpRstCnt    s              5  0 inz
     d charCo          s              3
     d charFnd         s              3
     d charYr          s              3
     d charClaim       s              7
     d charApp         s              1
     d charAuth        s              1
     d***  Begin Add  ***  06/02/21  ***********************************
     d decCount        s              9  0 inz
     d***  End   Add  ***  06/02/21  ***********************************
     d isAuthority     s               n
     d ReturnCode      s              1

     d/copy sprGenPop
     d***  Begin Add  ***  06/02/21  ***********************************
     d/copy sPrSql
     d***  End   Add  ***  06/02/21  ***********************************

     C     *Entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Pgm
     C                   Parm                    @@Tab
     C                   Parm                    @@Quote

      /Free
       //***  Begin Add  ***  06/02/21  ******************************
       // This must be the first line in the program
       Exec Sql Set Option Commit = *None;
       //***  End   Add  ***  06/02/21  ******************************

         If %parms()>0;
            inRRN = @@RRN;
            inMode = @@Mode;
            strCallPgm = @@Pgm;
         EndIf;
         If %parms()>=4;
            inTab = @@Tab;
         EndIf;
         If %parms()>=5;
            inQuote = @@Quote;
         EndIf;

         outTab = inTab;

         dou strCallPgm = *Blanks;
            Exsr chkAuth;
            If isAuth <> *on;
               PopOneBtn('You are not authorized to ' + %trim(DRtype) + '. +
                Please check with your security administrator for access.':
                'Authorization Issue');
               Leave;
            ElseIf charAuth = 'I' and inMode = 'C';
               inMode = 'D';
            Endif;
            //***  Begin Add  ***  06/02/21  ******************
            SetRunCriteria(1);
            //***  End   Add  ***  06/02/21  ******************
            Monitor;
               CallP(e) CallProgram(inRRN :inMode :outNextPgm :outTab :inQuote);
            On-Error;
               CallP(e) CallProgram(inRRN :inMode :outNextPgm);
               outTab='00000';
            EndMon;
            If %Error;
              DoU BtnClose = *On;
                scJobInfo = %char(Q1JOBN) +
                 '/' + %trim(Q1CUSR) + '/' + %trim(Q1JOB);
                Exfmt RcdError;
              EndDo;
               strCallPgm = *Blanks;
               Leave;
            EndIf;
            strCallPgm = outNextPgm;
         EndDo;

         *inLR=*On;
         Return;

       //===================================================================
       // chkAuth
       //===================================================================
       Begsr chkAuth;

       ReturnCode  = ' ';
       isAuthority = *off;
       isAuth   = *on;
       charApp  = 'W';
       charAuth = ' ';
       Monitor;
          intRrn   = %int(inRrn);
       On-Error 105;
          intRrn=0;
       EndMon;

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);

       // get the group restriction count
       Exsr GetGrpRstCnt;

       //***  Begin Add  ***  06/02/21  ***********************
       Clear DRtype;
       //***  End   Add  ***  06/02/21  ***********************
       Chain strCallPgm pdr00;
       If %found(STDRL);
          Select;
          When %trim(DRtype) = 'Policy';
             Exsr processPolicy;
          When %trim(DRtype) = 'Claim';
             Exsr processClaims;
          Endsl;
       Endif;

       If ##WCif = 'Y';
          Close WMUPL;
       Endif;

       Endsr;
       //===================================================================
       // processClaims
       //===================================================================
       Begsr processClaims;

       If ##WCif = 'Y';
          Open WMCMP;

          Chain intRRN pcm0p;
          If %found(WMCMP);

             If UPxFnd = 'Y' and UPfnd <> CMfnd and intGrpRstCnt = 0;
                isAuth = *off;
             Else;
                GetAuth(CMco#:q1User:charApp:isAuthority:charAuth:CMfnd:CMemp#);
                If isAuthority = *off;
                   charCo  = %editC(CMco#:'X');
                   charFnd = %editC(CMfnd:'X');
                   charYr  = %editC(CMfyr:'X');
                   charClaim = %editC(CMcase:'X');
                   GetRstrct(charCo:charFnd:charYr:charClaim:q1User:ReturnCode);

                   If ReturnCode = '2';
                      isAuth = *off;
                   Endif;
                Else;
                   isAuth = *off;
                Endif;
             Endif;
          Endif;
          Close WMCMP;
       Endif;

       Endsr;

       //===================================================================
       // processPolicy
       //===================================================================
       Begsr processPolicy;

       If ##WCif = 'Y';
          Open WDELP;
          Open WMEMP;

          Clear EMco#;
          Clear EMfnd;
          Clear EMemp#;
          Clear ELco#;
          Clear ELfnd;
          Clear ELemp#;
          Select;
          When inMode = 'Y';
             Chain intRRN pem0p;
             ELco#  = EMco#;
             ELfnd  = EMfnd;
             ELemp# = EMemp#;
          Other;
             Chain intRRN pel0p;
          Endsl;
          If %found;

             If UPxFnd = 'Y' and UPfnd <> ELfnd and intGrpRstCnt = 0;
                isAuth = *off;
             Else;
                GetAuth(ELco#:q1User:charApp:isAuthority:charAuth:ELfnd:ELEmp#);
                If isAuthority <> *off;
                   isAuth = *off;
                Endif;
             Endif;
          Endif;
          Close WDELP;
          Close WMEMP;
       Endif;

       Endsr;
       //===================================================================
       // GetGrpRstCnt
       //===================================================================
       Begsr GetGrpRstCnt;

       intGrpRstCnt = 0;

       If ##WCif = 'Y';
          Open WMUPL;

          Chain q1User pup00;
          If %found(WMUPL);

             Setll (UPco#:q1User) paz00;
             Reade (UPco#:q1user) paz00;
             Dow not %eof(SMAZL);
                If AZstat <> 'I' and AZappl = 'W';
                   intGrpRstCnt = intGrpRstCnt + 1;
                Endif;
                Reade (UPco#:q1user) paz00;
             Enddo;
          Endif;
       Endif;

       Endsr;

      /End-Free
      //***  Begin Add  ***  06/02/21  ******************
      //===================================================================
      // Set criteria for usage by WTGETTBFLG
      //===================================================================
     PSetRunCriteria   b
     DSetRunCriteria   pi
     D inCounter                      3  0 const

     DconDelaySec      c                   1
     DconRetries       c                   9
      /Free
       Reset decCount;
       Exec sql
        select count(*) into :decCount
         from WTGETTBFLG
         where TBuser = :q1User;
       // The data will be cleared in WTGETTBFLG program, after use
       Select;
       When inCounter = conRetries;
          Exec sql
           update WTGETTBFLG
            set TBrrn = :inRRN,
                TBrrnType = :DRtype
            where TBuser = :q1User;
       When decCount <> 0;
          DelayJob(conDelaySec);
          SetRunCriteria(inCounter+1);
       Other;
          Exec sql
           insert into WTGETTBFLG
           values (:q1User, :inRRN, :DRtype);
       Endsl;
       Return;
      /End-free
     PSetRunCriteria   e
      //***  End   Add  ***  06/02/21  ******************
