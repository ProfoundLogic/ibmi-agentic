     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F6603R - NCCI Data Call Submission Flat File Pgm
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*****************************************************************
     H*  Created on 04/20/2016 by C:308    Project C114MWK            *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     F* Submission Control File
     Ffev0p     uf a e             disk    usropn
     Ffev1l     if   e           k disk    usropn Rename(pv101:pv102)
     Ffev1p     uf a e             disk    usropn
     Ffev2l2    uf   e           k disk    Rename(pv201:pv202)
     Ffmncl3    uf   e           k disk    Rename(pnc01:pnc03)
     Ffev9p     uf a e             disk    usropn
     Fftgtl     if   e           k disk    prefix(F_) Rename(pgt01:fgt01)
     Fwtgtl     if   e           k disk    usropn
     Ffenlp     uf a e             disk    usropn
     Ffenll1    uf   e           k disk    Rename(pnl01:pnl02)
     Ffenll2    uf   e           k disk    Rename(pnl01:pnl03)
     Ffxwnl1    if   e             disk    usropn
     Ffaeal14   if   e           k disk    Rename(pea01:pea02)
     Ffmc1l     if   e           k disk    usropn
      *
     d fev1pds       e ds                  extname(fev1l)
      *
     d F3SubpgmR       pr                  Extpgm('F6603R')
     d  inMode                        1A
     d  inFile                       30A
     d  inState                       2A

     d F3SubpgmR       pi
     d inMode                         1A
     d inFile                        30A
     d inState                        2A
     d IsWcomp         S              1    Inz(*Blank)
     d IsNPdsWc        S              1    Inz(*Blank)
     d strYear         S              4    Inz(*Blank)
     d strQtr          S              1    Inz(*Blank)
     d strQtr2         S              2    Inz(*Blank)
     d strQtr3         S              3    Inz(*Blank)
     d intPos          S              2  0 Inz(*Blank)
     d strDate         S              8    Inz(*Blank)
     d strTime         S              6    Inz(*Blank)
     d intCount        S              9  0 Inz(*zero)
     d strmthfr        S              2    Inz(*Blank)
     d strmthTo        S              2    Inz(*Blank)
     d strYrFr8        S              8    Inz(*Blank)
     d intYrFr8        S              8  0 Inz(*zero)
     d strYrFr         S              4    Inz(*Blank)
     d strV2           S            500    Inz(*Blank)
     d strV9Dta        S            200    Inz(*Blank)
     d strv1dte        S             10    Inz(*Blank)
     d strv1YY         S              4    Inz(*Blank)
     d strv1MM         S              2    Inz(*Blank)
     d strv1DD         S              2    Inz(*Blank)
     d strv9dte        S             10    Inz(*Blank)
     d strv9YY         S              4    Inz(*Blank)
     d strv9MM         S              2    Inz(*Blank)
     d strv9DD         S              2    Inz(*Blank)
     d strV9DT         S              8    Inz(*Blank)
     d strJscd2        S              2    Inz(*Blank)
     d strWTky10       S             10    Inz(*Blank)
     d strstate10      S             10    Inz(*Blank)
     d isNCCI          S              1    Inz(*Blank)
     d isV1exst        S              1    Inz(*Blank)
     d inteob#         S             15  0 Inz(*zero)

     dpsds           esds                  extname(szq1p)

      * Prototypes
     d/copy sPrModule
     d/copy sPrDate

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Intialize the work variable
     d LoadInitVal     pr

      * To Write record in Fev1l file
     d FileV1Wrt       pr

      * To Write record in Fev9p file
     d FileV9Wrt       pr

      * To Write record in Fenlp file
     d FileNLWrt       pr
     C*================================================================

      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       Close Wtgtl;
       close fxwnl1;
       close fev0p;
       close fev1l;
       close fev1p;
       close fev9p;
       close fenlp;
       close fmc1l;
       *InLr = *On;
      /end-free
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       open fxwnl1;
       open fev0p;
       open fev1l;
       open fev1p;
       open fev9p;
       open fenlp;
       open fmc1l;
       // Intialize the Program work variable & Year Qtr Date & Time
       LoadInitVal();

      /End-Free
     p ProgramStart    e
       //===================================================================
       // LoadInitVal - To get Year and Quarter from file name & program var
       //===================================================================
     p LoadInitVal     b
     d LoadInitVal     pi
      /Free
       If inFile <> *Blanks;
          intPos = %Scan('.':inFile);
          If intPos <> *zero;
             intPos = intPos + 1;
             strYear = %SubSt(inFile:intPos:4);
             intPos  = intPos + 4;
          EndIf;

          If inState = 'N' or inState = ' N' or inState = 'N ';
             strQtr3 = %SubSt(inFile:intPos:3);
          Else;
             strQtr2 = %SubSt(inFile:intPos:2);
             strQtr = %Subst(strQtr2:2:1);
          EndIf;
          If strQtr3 <> *Blanks;
             Select;

                When  strQtr3  = 'Jan' or strQtr3  = 'Feb' or strQtr3  = 'Mar';
                   strQtr  = '1';

                When  strQtr3  = 'Apr' or strQtr3  = 'May' or strQtr3  = 'Jun';
                   strQtr  = '2';

                When  strQtr3  = 'Jul' or strQtr3  = 'Aug' or strQtr3  = 'Sep';
                   strQtr  = '3';

                When  strQtr3  = 'Oct' or strQtr3  = 'Nov' or strQtr3  = 'Dec';
                   strQtr  = '4';

             EndSl;
          EndIf;

          intCount = *zero;
       EndIf;
      /End-Free
     p LoadInitVal     e
       //===================================================================
       // FileV9Wrt - To get Year and Quarter from file name
       //===================================================================
     p FileV9Wrt       b
     d FileV9Wrt       pi

      /Free
       If inFile  <> *Blanks;
          If inMode <> 'O' ;
             strV9DT =  strv1YY + strv1MM + strv1DD ;
          EndIf;
          If inMode <> 'R' or inMode <> 'D';
             strv9dte = %Char(%Date()) ;
             strv9YY =  %Subst(strv9dte:1:4);
             strv9MM =  %Subst(strv9dte:6:2);
             strv9DD =  %Subst(strv9dte:9:2);
             strV9DT =  strv9YY + strv9MM + strv9DD ;
          EndIf;
          V9FILE  =  inFile      ;
          V9SBDT  =  %dec(strV9DT:8:0);
          V9SBTM  =  q1Time      ;
          V9PRFL  =  'P'         ;
          If WNRPYMTHF <> *zero  ;
             strmthfr = %Subst(%char(WNRPYMTHF):4:2);
          Else;
             strmthfr = *Blank;
             V9MTH    = *zero;
          Endif;
          If strmthfr <> *Blank ;
             V9MTH   =  %Dec(strmthfr:2:0);
          EndIf;
          If WNRPYMTHT <> *zero  ;
             strmthTo = %Subst(%char(WNRPYMTHT):4:2);
          Else;
             strmthTo = *Blank;
             V9MTHTO  = *zero;
          Endif;
          If strmthTo <> *Blank ;
             V9MTHTO =  %Dec(strmthTo:2:0);
          EndIf;
          If WNRPYMTHF <> *zero  ;
             intYrFr8 = 190000 + WNRPYMTHF;
             strYrFr8 = %char(intYrFr8);
             strYrFr = %Subst(strYrFr8:1:4);
             If strYrFr  <> *Blank ;
                V9YEAR  =  %Dec(strYrFr:4:0);
             EndIf;
          Else;
             V9YEAR  =  *zero ;
          EndIf;
          V9YEARTO=  V9YEAR      ;
          If V2CARR <> *Blank;
             V9CARR  =  %Dec(V2CARR:5:0);
          Else;
             V9CARR  =  *zero;
          EndIf;
          V9RCDS  =  intCount    ;
          If inMode = 'O'        ;
             V9FTCD  =  'O'         ;
             V9OFIL  =  *Blanks     ;
          ElseIf inMode = 'R' ;
             V9FTCD  =  'R'         ;
             V9OFIL  =  inFile      ;
          EndIf                  ;
          V9IUSR  =  q1User      ;
          V9IPGM  =  q1Pgm       ;
          V9IDT   =  DateTo7(SystemDate);
          V9ITIM  =  q1Time      ;
          V9USER  =  q1User      ;
          V9PGM   =  q1Pgm       ;
          V9DT    =  DateTo7(SystemDate);
          V9TIME  =  q1Time      ;
          Write pv901;
       EndIf;
      /End-Free
     p FileV9Wrt       e
       //===================================================================
       // FileV1Wrt - To get Year and Quarter from file name
       //===================================================================
     p FileV1Wrt       b
     d FileV1Wrt       pi

      /Free
       If inFile  <> *Blanks;
          V1RCTY  =  'SUBCTRLREC';
          If inMode = 'R' or inMode = 'D';
             Setgt (inFile) pv102   ;
             ReadPE (inFile) pv102 ;
             If Not %Eof (fev1l)  ;
                V1SBTY  =  'R'   ;
                isv1exst =  '1'   ;
                Write pv101      ;
             Else                ;
                isv1exst =  ' '  ;
             EndIf               ;
          EndIf         ;
          If inMode = 'O'         ;
             V1SBTY = 'O'         ;
             isv1exst =  '1'   ;
          If WNco# <> *zero    ;
             Chain WNCO# pc101   ;
             If %Found(fmc1l)    ;
                V1CARR = %Subst(c110a4:6:5);
             EndIf               ;
          Else                   ;
          V1CARR = *Blanks       ;
          EndIf                  ;
          V1QTR   =  strQtr      ;
          V1YEAR  =  strYear     ;
          V1SBID  =  inFile      ;
          strv1dte = %Char(%Date()) ;
          strv1YY =  %Subst(strv1dte:1:4);
          strv1MM =  %Subst(strv1dte:6:2);
          strv1DD =  %Subst(strv1dte:9:2);
          V1SBDT  =  strv1YY + strv1MM + strv1DD ;
          V1SBTM  =  %Char(q1Time) ;
          V1RCDS  =  '00000000000';
          V1FILL  =  *Blanks     ;
          Write pv101            ;
          EndIf         ;
       EndIf;
      /End-Free
     p FileV1Wrt       e
       //===================================================================
       // FileNLWrt - To write record in FENLP file
       //===================================================================
     p FileNLWrt       b
     d FileNLWrt       pi

      /Free
        nlfile = infile;
        If V2eob# <> *Blanks;
           NLeob# = %Dec(V2eob#:15:0);
        Else;
           NLeob# = *zero;
        EndIf;
        If NLeob# <> *zero;
           Chain (WnCo#:NLeob#)pea02;
           If %found(FAEAL14);
              NLrcn = EArcn;
           Else;
              NLrcn = *zero;
           EndIf;
        Else;
           NLrcn = *zero;
        EndIf;
        NLQtr = %Dec(strQtr:1:0);
        NLYear = %Dec(strYear:4:0);
        NLSBDT = %Dec(V1SBDT:8:0);
        NLSBTM = q1Time;
        If inMode = 'O';
           NLTRCD = '01';
        EndIf;
        NLPRFL = 'P';
        NLIUSR = q1User      ;
        NLIPGM = q1Pgm       ;
        NLIDT  = DateTo7(SystemDate);
        NLITIM = q1Time      ;
        NLUSER = q1User      ;
        NLPGM  = q1Pgm       ;
        NLDT   = DateTo7(SystemDate);
        NLTIME = q1Time      ;
        Write pnl01;
      /End-Free
     p FileNLWrt       e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /Free

       // Process Original & Replacement
       If inMode = 'R' or inMode = 'O' or inMode = 'D';
          If inState = 'N' or inState = ' N' or inState = 'N ';
       // Get the Mode Enabled or not for NCCI
       //*** Beg Chg *** 07/15/16 ***********************************
       //*** IsWcomp = IsModEnabled('WORKCOMP');
             IsWcomp = IsModEnabled('WorkComp');
       //*** End Chg *** 07/15/16 ***********************************
             If IsWcomp = '1';
       //*** Beg Chg *** 07/15/16 ***********************************
       //****** IsNPdsWc= IsModEnabled('NONPDSWC');
                IsNPdsWc= IsModEnabled('NonPDSWC');
       //*** End Chg *** 07/15/16 ***********************************
                If IsNPdsWc = '0';
                   Open Wtgtl;
                   isNCCI = '1';
                EndIf;
             EndIf;
          EndIf;
       // Read NCCI data call Workfile to create the file
          Read  pwn01;
       // Write control record in fev1l and to fev0p flat file
          FileV1Wrt();
          If isv1exst = '1';
          V0data = Fev1pds;
          Write pv001;
          EndIf;
          If isv1exst = '1' and inMode <> 'D';
       // Read through the work file fxwnl1 to report record to fev0p
          Dow Not %Eof(Fxwnl1);
             Setll (WNrpymthf:WNrpymtht:WNfnd) pv202;
             If %equal(Fev2l2);
                ReadE (WNrpymthf:WNrpymtht:WNfnd) pv202;
                DoW not %EoF(Fev2l2);
                   If inState <> 'N' and inState <> ' N' and inState <> 'N ';
                      strState10 = inState;
                      Chain (WNco#:'JSCD':strState10) Fgt01;
                      If %Found(Ftgtl);
                        strJscd2 = *Blank;
                        strJscd2 = %Subst(F_GTdesc:1:2);
                        If strJscd2 = V2jscd;
                         strV2 = V2CARR + V2POL# + V2EFDT + V2CLM# + V2TRCD +
                                 V2JSCD + V2SEX  + V2BYR  + V2ACDT + V2TRDT +
                                 V2EOB# + V2LIN# + V2SRDT + V2SFDT + V2STDT +
                                 V2PRCD + V2MOD  + V2CHRG + V2PAID + V2PDIA +
                                 V2SDIA + V2PRTY + V2FEIN + V2ZIP  + V2NTWK +
                                 V2UNTS + V2POS  + V2BPRC + V2FILL ;

       // Write record to fev0p from fev2p extract file for state
                         V0data = strV2;
                         Write pv001;
                         intCount = intCount + 1;
       // Report data to NCCI log file fenlp for original submission
                         If inMode = 'O';
                            FileNLWrt();
                         EndIf;
                        EndIf;
                      EndIf;
                   ElseIf inState = 'N' or inState = ' N' or inState = 'N ';
       // Process NCCI state from Wtgtp file
                      If isNCCI = '1';
                         Setll (WNco#:'NCCI') pgt01;
                         If %Equal(wtgtl);
                             Reade (WNco#:'NCCI') pgt01;
                             Dow not %Eof(wtgtl);
                                Eval strWTky10 = GTkey;
                                Chain (WNco#:'JSCD':strWTky10) Fgt01;
                                If %Found(Ftgtl);
                                   strJscd2 = *Blank;
                                   strJscd2 = %Subst(F_GTdesc:1:2);
                                   If strJscd2 = V2jscd;
                                      strV2 = V2CARR + V2POL# + V2EFDT +
                                      V2CLM# + V2TRCD + V2JSCD + V2SEX +
                                      V2BYR  + V2ACDT + V2TRDT + V2EOB# +
                                      V2LIN# + V2SRDT + V2SFDT + V2STDT +
                                      V2PRCD + V2MOD  + V2CHRG + V2PAID +
                                      V2PDIA + V2SDIA + V2PRTY + V2FEIN +
                                      V2ZIP  + V2NTWK + V2UNTS + V2POS  +
                                      V2BPRC + V2FILL ;

       // Write record to fev0p from fev2p extract file for NCCI state
                                      V0data = strV2;
                                      Write pv001;
                                      intCount = intCount + 1;
       // Report data to NCCI log file fenlp for original submission
                                      If inMode = 'O';
                                         FileNLWrt();
                                      EndIf;
                                   EndIf;
                                EndIf;
                                Reade (WNco#:'NCCI') pgt01;
                             EndDo;
                         EndIf;
                      EndIf;
                   EndIf;
       // Update NCCI log file fmnlp for Resubmission
                   If inMode = 'R' and V2trcd = '01';
                      inteob# = %Dec(V2eob#:15:0);
                      Setll (inteob#)pnl02;
                      If %Equal(fenll1);
                         Reade (inteob#)pnl02;
                         Dow not %Eof(fenll1);
                            If NLprfl <> 'F';
                               NLtrcd = '03';
                               V2trcd = '03';
                               Update pnl02;
                            EndIf;
                            Reade (inteob#)pnl02;
                         EndDo;
                      EndIf;
                   EndIf;

                   Chain (WNco#:WNfnd:WNrpymthf:WNrpymtht) pnc03;
                   If %Found(fmncl3);
                      NCsbdt = DateTo7(SystemDate);
                      Update pnc03;
                   EndIf;
                   Update pv202;
                   ReadE (WNrpymthf:WNrpymtht:WNfnd) pv202;
                EndDo;
             EndIf;
             Read pwn01;
          EndDo;
       // Write the trailer record in file fev9p for O & R process
          FileV9Wrt();
          strV9Dta = V9FILE + %Char(V9SBDT) + %Char(V9SBTM)      +
          V9PRFL + %Char(V9MTH) + %Char(V9MTHTO) + %Char(V9YEAR) +
          %Char(V9YEARTO) + %Char(V9CARR) + %Char(V9RCDS) + V9FTCD +
          V9OFIL + V9IUSR + V9IPGM + %Char(V9IDT) + %Char(V9ITIM) +
          V9USER + V9PGM + %Char(V9DT) + %Char(V9TIME) ;
          V0data = strV9Dta;
       // Write pv001;
       EndIf;
       If inMode = 'D';
          Setll (inFile) pnl03;
          If %Equal(fenll2);
             ReadE (inFile) pnl03;
             Dow not %Eof(fenll2);
                NLprfl = 'F';
                Update pnl03;
                ReadE (inFile) pnl03;
             EndDo;
          Endif;
       Endif;
       Endif;
      /End-Free
     p Main            e
