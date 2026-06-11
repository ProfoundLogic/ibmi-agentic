     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) actgrp(*caller)
    h/title UDS-M Prompt
    h*********************************************************************
    h*                                                                   *
    h*  Called from: Menu                                                *
    h*  Calls to: W7235C - Submitter                                     *
    h*                                                                   *
    h*********************************************************************
    h*  Created on 02/11/13 by c:282 for Project 114WFC                  *
    h*********************************************************************
    h* Revisions:                                                        *
     h*                                                                   *
    h*********************************************************************
     fw7235f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmeml21   if   e           k disk    rename(pem01:pem00)
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d psds          esds                  extname(SZQ1P)
     d optName         c                   const('UDS-M Extract')
     d charCo#         s              3
     d charNaic        s              5
     d intCo#          s              3  0
     d intFnd          s              3  0
     d charConm        s             40
     d charFnam        s             40
     d charData        s             80
     d intLeng         s              3  0
     d @@kc            s              1
     d @@kl            s              1
     d charRsk#        s                   Like(EMrsk#)
     d qqKC            s              1
     d qqKL            s              1
     d qqCall          s              1
     d qqRrn           s              9  0
     d qqRrn2          s              9  0
     d qqQte           s              1
     d qqView          s              1  0
      *
     d                 ds
     d intRsk#                        9  0
     d  intRsk#4                      4  0 overlay(intRsk#)
     d  intRsk#5                      5  0 overlay(intRsk#:5)
      *
      * Get Company Name
     dGetCompName      pr                  ExtPgm('WA001R')
     dintCo#                          3  0
     dintFnd                          3  0
     dcharConm                       40
     dcharFnam                       40
      *
      * Subprogram to center a field
     dCenterField      pr                  ExtPgm('SRCENTER')
     dchardata                       80
     dintLeng                         3  0
      *
      * Select the Employer
     dGetEmployer      pr                  ExtPgm('W0050R')
     dqqKC                            1
     dqqKL                            1
     dqqCall                          1
     dqqRrn                           9  0
     dqqRrn2                          9  0
     dqqQte                           1
     dqqView                          1  0
      *
      * UDS-M Submit Program
     dSubmissionPgm    pr                  ExtPgm('W7235C')
     dcharCo#                         3
     dcharNaic                        5
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     c     *Entry        Plist
     c                   Parm      *InKC         @@kc
     c                   Parm      *InKL         @@kl
     *
     c     keyWMEML21    Klist
     c                   Kfld                    intCo#
     c                   Kfld                    charRsk#
     *
      /Free
       Exsr Pgmstr;

       Dou Not *In90 Or *InKC Or *InKL;
          Exfmt SCr01;
          If Not *InKC;
             Exsr Errck;
             If Not *In90;
                Exsr Submit;
             EndIf;
          EndIf;
       EndDo;

       *InLr = *On;
      /End-Free
     *====================================================================
     * ERRCK - Error checking
     *====================================================================
      /Free
       BegSr Errck;

       *In90 = *Off;
       If *InKJ = *On;
          Exsr KJDO;
       Else;

       //   NAIC # Must be valid
          If Scnaic <> 0;
             intRsk# = *Zeros;
             intRsk#5 = SCnaic;
             charRsk# = %EditC(intRsk#:'X');
             Chain keyWMEML21 pem00;
             If Not %Found(WMeml21);
                *In72 = *On;
                *In90 = *On;
                LeaveSr;
             EndIf;                              // Not %Found(WMeml21)
          Else;
             *In72 = *On;
             *In90 = *On;
             LeaveSr;
          EndIf;                                 // scnaic <> 0

       EndIf;                                    // *InKJ = *On

       EndSr;
      /End-Free
     *====================================================================
     * KJDO - F10=Search
     *====================================================================
      /Free
       BegSr KJDO;
       *In90 = *On;

       Select;
       When  hfld='SCNAIC';
          QQkc   = '0';
          QQkl   = '0';
          QQcall = '2';
          QQrrn  = 0;
          QQrrn2 = 0;
          QQqte  = *Blanks;
          QQview = 1;
          GetEmployer(QQkc:QQkl:QQcall:QQrrn:QQrrn2:QQqte:QQview);

          If QQrrn = 0;
             *In31 = *On;
          Else;
             Chain QQrrn pem0p;
             If %Found(WMemp);
                intRsk# = %Int(EMrsk#);
                SCnaic = intRsk#5;
             EndIf;
          EndIf;
       EndSl;

       EndSr;
      /End-Free
     *====================================================================
     * PGMSTR - Used for any start of program processing (one-time)
     *====================================================================
      /Free
       BegSr PgmStr;
    
       intCo#   = *Zeros;
       intFnd   = *Zeros;
       charConm = *Blanks;
       charFnam = *Blanks;
       GetCompName(intCo#:intFnd:charConm:charFnam);
      SCconm = charConm;
    
      charData = optName;
       intLeng  = 40;
      CenterField(charData:intLeng);
      SCprds = charData;
    
       EndSr;
      /End-Free
     *====================================================================
     * SUBMIT - Submit UDS-M Process
     *====================================================================
      /Free
       BegSr Submit;

      charCo#  = %Editc(intCo#:'X');
       charNaic = %EditC(scNaic:'X');
       SubmissionPgm(charCo#:charNaic);
       *InKC = *On;

       Endsr;
      /End-Free
