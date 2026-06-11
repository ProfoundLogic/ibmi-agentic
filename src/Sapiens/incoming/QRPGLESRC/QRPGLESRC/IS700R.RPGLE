     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
    H*********************************************************************
    H/TITLE UDS Upload - Prompt
    H*********************************************************************
    H*  DESCRIPTION: UDS Upload - Prompt                                 *
    H*                                                                   *
    H*  CALLED FROM: menu option                                         *
    H*                                                                   *
    H*  CALLS TO:    IS700C                                              *
    H*                                                                   *
    H*  CREATED: 03/27/14      C:280     PROJECT: 114ijm                 *
    H*                                                                   *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     FIS700F    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F* uds upload prompt screen
    F*
     FWMFNL     if   e           k disk    rename(PFN01:PFN00)
    F*
    F* Fund master File by CO#/FND
    F*
     FWMEML     if   e           k disk    rename(PEM01:PEM00)
    F*
    F* Employer Master File by CO#/FND/EMP#/DIV
    F*
     FSTCTL1    if   e           k disk    rename(PCT01:PCT00)
    F*
    F* Remark Type Table File by RMCD
    F*
     FSMUPL     if   e           k disk    rename(PUP01:PUP00)
    F*
    F* User Parameters File by user
    F*
     FSMCOL     if   e           k disk    rename(PCO01:PCO00)
    F*
    F* System company file for work comp by co#
    F*
     FUDSHDRIL  if   e           k disk    rename(PSH01:PSH00)
    F*
    F* UDS Header logical by root
    F*
    D*
     D/COPY SCOPYPR01D
    D*
     D/COPY SPRDATE
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
    D*
     D optnm           c                   const('Upload UDS File')
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##Lib           s             10
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
    D*                                                                -----
     D ifsobj          s            256
     D ifsobjfound     s              1
    D*                                                                -----
     D isfFnd          s              3
     D isfEmp          s              9
     D isfDiv          s              5
     D isfCode         s              5
     D isfPath         s             51
    D*                                                                -----
     DChkIFSObj        pr                  extPgm('CHKIFSOBJ')
     D ifsObj                       256
     D ifsObjFound                    1n
    D*                                                                -----
     D comapi          c                   const('WA001R')
    D*                                                                -----
     DSbmIfsJob        pr                  extPgm('IS700C')
     D isfFnd                         3
     D isfEmp                         9
     D isfDiv                         5
     D isfCode                        5
     D isfPath                       51
    D*                                                                -----
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*
     d/COPY sprsecure
    D*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm      *inkc         @@inkc            1
     C                   parm      *inkl         @@inkl            1
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/COPY SCOPYPR01C

    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*

      /free

       BegSr Errck1;

       *In10 = *Off;
       *In11 = *Off;
       *In12 = *Off;
       *In13 = *Off;
       *In14 = *Off;
       *In15 = *Off;
       *In90 = *Off;

       // Validate Fund (required)

b01    If Scfnd <> *Zeros;

       Chain (777:scfnd) WMFNL;

b02       If Not %Found(WMFNL);
             *In10 = *On;
             *In90 = *On;
             Leavesr;
e02       Endif;

x01    Else;
          *In10 = *On;
          *In90 = *On;
          Leavesr;
e01    Endif;

      // Validate Employer (required)

b01    If Scemp# <> *ZEros;

       Chain (777:Scfnd:Scemp#) WMEML;

b02       If Not %Found(WMEML);
             *In11 = *On;
             *In90 = *On;
             Leavesr;
e02       Endif;

x01    Else;
          *In11 = *On;
          *In90 = *On;
          Leavesr;
e01    Endif;

      // Validate Division (required)

       Chain (777:Scfnd:Scemp#:Scdiv) WMEML;

b01       If Not %Found(WMEML);
             *In12 = *On;
             *In90 = *On;
             Leavesr;
e01       Endif;

      // Validate Object Link (required)

b01    If SCroot <> *Blanks;

      // Has this been processed ?

       Chain Scroot UDSHDRIL;

b02      If Not %Found(UDSHDRIL);

            IfsObj = %Trim('/UDS/') + %Trim(Scroot);
            IfsObjFound = *Blanks;

            Chkifsobj(Ifsobj:Ifsobjfound);

b03         If Ifsobjfound = *Blank;
              *In13 = *On;
              *In90 = *On;
              Leavesr;
e03         Endif;

x02      Else;
            *In14 = *On;
            *In90 = *On;
            Leavesr;
e02      Endif;

x01    Else;
          *In13 = *On;
          *In90 = *On;
          Leavesr;
e01    Endif;

      // Validate DARTS Remark Code (Required)

b01    If Sccode <> *Blanks;

       Chain Sccode STCTL1;

b02       If Not %Found(STCTL1);
           *In15 = *On;
           *In90 = *On;
           Leavesr;
e02       Endif;

x01    Else;
          *In15 = *On;
          *In90 = *On;
          Leavesr;
e01    Endif;

       If *In90 = *Off And *Inkg = *Off;
          *In90 = *On;
       Endif;

       Endsr;

      /end-free

    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*

      /free

       BegSr kjdo;
       Endsr;

      /end-free

    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*

      /free

       BegSr othfks;

       If *Inkg;

      // Submit job

          isfFnd = %char(SCfnd);
          isfEmp = %char(SCemp#);
          isfDiv = %char(SCdiv);
          isfCode = SCcode;
          isfPath = %Trim('/UDS/') + (Scroot);

          %SUBST(ISFPATH:51:1) = '*';

          SbmIfsJob(IsfFnd:IsfEmp:IsfDiv:IsfCode:IsfPath);

       Endif;

       Endsr;

      /end-free

    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*

      /free

       BegSr pgmend;

       *Inlr = *On;

       Endsr;

      /end-free

    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*

      /free

       BegSr pgmstr;

       Chain Q1user SMUPL;

b01    If %Found(SMUPL);

          Exsr Center;

e01    Endif;

       Endsr;

      /end-free

    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*

      /free

       BegSr predsp;

       Endsr;

      /end-free

    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*

      /free

       BegSr prsctl;

       Endsr;

      /end-free

    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
     C*
     C     dscapi        begsr
     C                   call      comapi        compl                  91
     C                   endsr
     C*
    C*====================================================================
    C* CENTER - Used for any start of program processing (one-time)
    C*====================================================================
     C     CENTER        begsr
    C*
     C                   if        %parms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   if        %parms >= 2
     C                   movel     @@call        XXCAll
     C                   else
     C                   clear                   XXcall
     C                   endif
    C*
     C                   endsr                                                  pgmstr
