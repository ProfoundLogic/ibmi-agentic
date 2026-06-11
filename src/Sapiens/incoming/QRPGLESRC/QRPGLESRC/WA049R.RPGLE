     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - Produce New Claim letters based on the auto claim letter  WCMC
     H*       control file.                                             WCMC
     H*
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA049R                                            *
     H*    CREATED: 04/22/03  C:53 PROJ: WC1121                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     h*                                                               *
     h*  06/20/25 C:349 Support: 118427 (CoveRisk)                    *
     H*                 Fix trigger of Auto Letter for Reopened claim *
     H*                 condition.                                    *
     H*                                                               *
     H*  01/14/25 C:079 Support: 99/117929                            *
     H*                 Fix trigger conditions                        *
     H*                                                               *
     H*  09/16/22 C:291 Project: 129WXJ                               *
     H*                 Auto Letter can be issued based uopn the Lost *
     H*                 time flag alone instead of change in lost     *
     H*                 time flag value.                              *
     H*                                                               *
     H*  05/10/19 C:300 Support: 77946 (SCS)
     H*                 Auto Letters configuration for setting up a NEW
     H*                 claim when it is Med Only, Indemnity or Record Only
     h*                                                               *
     h*  10/27/16 C:180 Support 43948                                 *
     H*                 Donot trigger auto letter if letter is        *
     H*                 deactivated.                                  *
     h*                                                               *
     h*  03/01/16 C:062 Project: SY251                                *
     h*                 Add processing for Scriptura Auto Letters.    *
     h*                                                               *
     h*  11/25/13 C:282 Project: 129WSC                               *
     h*                 Added the Lost Time Beg and Claim Sataus Beg  *
     h*                 parameters.                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     FWACLL     IF   E           K DISK
     F                                     RENAME(PCL01:PCL03)
     FWDCLL     UF A E           K DISK
     FWDCLL3    O    E           K DISK
     F                                     RENAME(PCL01:PCL02)
     FWMCMP     IF   E             DISK
     fsacll     if   e           k disk    rename(pcl01:scl00)
     f                                     prefix(m)
     fsacllgrp  if   e           k disk    rename(pcl01:scl0grp)
     f                                     prefix(g)
     fsaclljst  if   e           k disk    rename(pcl01:scl0jst)
     f                                     prefix(s)
     fsaclloff  if   e           k disk    rename(pcl01:scl0off)
     f                                     prefix(o)
     Fxmlprel1  uf   e           k disk
     fstdll     if   e           k disk    rename(pdl01:pdl00)
     fstscp     if   e             disk    rename(psc01:psc0p)
     Fstp1l     IF   E           K DISK    rename(pp101:pp100)

     D*
     d STLETTER        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)

     d scriptMode      s             10
     d scriptXML       s             60
     d scriptMsg       s            250
     d scriptMode2     s              1
     d scriptRrn       s              9
     d scriptPrefix    s              2
     d scriptString    s             10
     d scriptCo#       s              3
     d scriptFnd       s              3

     D isScriptura     s              1    Inz('N')
     D scriptura       s               n   Inz(*Off)
     d statCondMet     s               n   Inz(*Off)
     d ltCondMet       s               n   Inz(*Off)
     d isStatCond      s               n   Inz(*Off)
     d isLtCond        s               n   Inz(*Off)
     d trigCondMet     s               n   Inz(*Off)
     d goodAutoLetter  s               n   Inz(*Off)
     d i               s              3  0
     d currentDate     s              7  0
     d currentTime     s              6  0
     d ltLstCondMet    s               n   Inz(*On)
     d arrLtLst        s              1    Dim(3) inz

     D languageDs      DS
     D  SClang01
     D  SClang02
     D  SClang03
     D  SClang04
     D  SClang05
     D  SClang06
     D  SClang07
     D  SClang08
     D  SClang09
     D  SClang10
     D  SClang11
     D  SClang12
     D  SClang13
     D  SClang14
     D  SClang15
     D  SClang16
     D  SClang17
     D  SClang18
     D  SClang19
     D  SClang20
     D languageArr                   40    Overlay(languageDs) Dim(20)

     D langCodeDs      DS
     D  SCcode01
     D  SCcode02
     D  SCcode03
     D  SCcode04
     D  SCcode05
     D  SCcode06
     D  SCcode07
     D  SCcode08
     D  SCcode09
     D  SCcode10
     D  SCcode11
     D  SCcode12
     D  SCcode13
     D  SCcode14
     D  SCcode15
     D  SCcode16
     D  SCcode17
     D  SCcode18
     D  SCcode19
     D  SCcode20
     D langCodeArr                    2    Overlay(langCodeDs) Dim(20)
     d charLTFrm       s              1
     d charCSFrm       s              1
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0


     IPCL01
     I              CLREQ#                      HIREQ#
     I              CLCO#                       HICO#
     I              CLFND                       HIFND
     I              CLFYR                       HIFYR
     I              CLFCLM                      HIFCLM
     I              CLEMP#                      HIEMP#
     I              CLDIV                       HIDIV
     I              CLDPCD                      HIDPCD
     I              CLFLDR                      HIFLDR
     I              CLDCID                      HIDCID
     I              CL#CPY                      HI#CPY
     I              CLSTAT                      HISTAT
     I              CLIUSR                      HIIUSR
     I              CLUSER                      HIUSER
     I              CLPGM                       HIPGM
     I              CLIPGM                      HIIPGM
     I              CLDT                        HIDT
     I              CLTIME                      HITIME
     I              CLITIM                      HIITIM
     I              CLIDT                       HIIDT
     I*
     IPCL03
     I              CLCO#                       CACO#
     I              CLFND                       CAFND
     I              CLOFC#                      CAOFC#
     I              CLST                        CAST
     I              CLDPCD                      CADPCD
     I              CLFLDR                      CAFLDR
     I              CLDCID                      CADCID
     I              CLLTFL                      CALTFL
     I              CL#CPY                      CA#CPY
     I              CLOUTQ                      CAOUTQ
     I              CLIUSR                      CAIUSR
     I              CLUSER                      CAUSER
     I              CLPGM                       CAPGM
     I              CLIPGM                      CAIPGM
     I              CLDT                        CADT
     I              CLTIME                      CATIME
     I              CLITM                       CAITM
     I              CLIDT                       CAIDT
     I              CLLTTO                      CALTTO
     I              CLCSFR                      CACSFR
     I              CLCSTO                      CACSTO
     I              CLANDR                      CAANDR

     C*****************************************************************
     C* Parameter Lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@LTFrm           1            Lost Time Beg
     C                   PARM                    @@CSFrm           1            Claim Status Beg
     C*****************************************************************
     C* Key  Lists
     C*****************************************************************
     C*
     C*
      *
     C     CAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    KKOFC#
     C                   KFLD                    CMJRST
     C*
     C     CLKY          KLIST
     C                   KFLD                    HICO#
     C                   KFLD                    HIFND
     C*
     C     CLKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    CLDPCD
     C                   KFLD                    CLFLDR
     C                   KFLD                    CLDCID
     C*
     C     CLKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    CLDPCD
     C                   KFLD                    XXREQ#
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    CAOFC#        KKOFC#
     C*
     C*****************************************************************
     C* Mainline
     C*****************************************************************
      /Free
       If %parms > 2;
          charLTFrm = @@LTFrm;
       EndIf;
       If %parms > 3;
          charCSFrm = @@CSFrm;
       EndIf;
      /End-Free
     C*
     C* Is Scriptura Enabled?
     C*
     C                   call      'SA047R'
     C                   parm      ' '           isScriptura
     C*
     C                   Eval      scriptura = *Off
     C                   If        isScriptura = 'Y'
     C                   Eval      scriptura = *On
     C                   EndIf
     C* Let's get times
     C                   Time                    DSDtTm
     C*
     C                   Z-Add     dsDate        Srdti
     C                   Call      'SRCVTDAT'    DTcymd
     C                   MoveL     Srdto         currentDate
     C*
     C                   Z-Add     dsTime        currentTime
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* If Scriptura *nstalled then Access new Auto Letter Process.
     C*
     C                   If        scriptura = *On
     C                   ExSr      IssueAutoLet
     C                   Else
     C                   MOVE      CMOFF#        KKOFC#
     C*
     C     CAKY          CHAIN     PCL03                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
      /Free
       If (charLTFrm = CAltfl And CAltto = CMltfl And CAandr = ' ')    Or
          (charCSFrm = CAcsfr And CAcsto = CMstat And CAandr = ' ')    Or
          (CAandr = 'A' And ((charLTFrm = CAltfl And CAltto = CMltfl)
           And (charCSFrm = CAcsfr And CAcsto = CMstat) ))             Or
          (CAandr = 'O' And ((charLTFrm = CAltfl And CAltto = CMltfl)
           Or (charCSFrm = CAcsfr And CAcsto = CMstat) ))              Or
          ((CAcsfr = 'N' And CAcsto = 'O') And
           (CAandr = 'O' Or  CAandr = ' ') And
           (charLTFrm = *Blank And charCSFrm = *Blank));
      /End-Free
     C                   EXSR      WRTUPD
     C                   EXSR      SUBMIT
      /Free
       EndIf;
      /End-Free
     C*
     C     CAKY          READE     PCL03                                  98
     C                   ENDDO
     C                   Endif
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C****************************************************************
     C* UpdateXMLCtl - Update the XML contrl record with Doc infi
     C****************************************************************
     C     UpdateXmlCtl  BEGSR
      /Free

          Chain (scriptXML) xp100;

          XPCat  = mCLcat;
          XPSubc = mCLsubc;
          XPDcid = mCLdcid;

          Chain (CMco#:mCLcat:mCLsubC:mCLdcid:mCLlang) pdl00;
          If %Found(STDLL);
             DoU %Eof(STDLL);
                If DLstat <> 'I';
                   XPrmCd = DLrmcd;
                   XPIntr = DLintr;
                   XPInid = DLinID;
                   XPPath = DLpath;
                   XPCopy = DLcopy;
                   XPTray = DLtray;
                   XPMail = ' ';
                   XPSubj = DLsubj;
                   XPBody = DLbody;

                   If DLprin <> ' ';
                      Chain (DLprin) pp100;
                      If %found(STP1L);
                         XPPnam = %trim(P1Name);
                         If P1Type = 'N';
                            XPPunc = %trim(P1UNCN);
                         Else;
                            XPOutq = %trim(P1KEY);
                            XPOLib = %trim(P1OLIB);
                         Endif;
                      Endif;
                   EndIf;
                   Leave;
                EndIf;
                ReadE (CMco#:mCLcat:mCLsubC:mCLdcid:mCLlang) pdl00;
             EndDo;
          EndIf;

          SetLL 1 psc0p;
          Read psc0p;

          i = %LookUp(mCLlang:languageArr);
          If i <> 0;
             XPlang = langCodeArr(i);
          Else;
             XPLang = 'en';
          EndIf;

          XPScrp = 'Y';
          XPData = 'Y';
          XPDart = 'Y';

          XPPgm  = q1PGM;
          XPUser = q1User;
          XPTime = currentTime;
          XPDt   = currentDate;

          Update xp100;
      /end-free
     C*
     C                   EndSr
     C* ***************************************************************
     C* WRTUPD SUBROUTINE              ** Subroutine **   W R T U P D *
     C* ***************************************************************
     C     WRTUPD        BEGSR
     C* Check to see if Scriptura is installed
     C                   If        scriptura = *On
     C* Setup initial XML request record
     C                   Eval      scriptMode   = 'XMLREQUEST'
     C                   Eval      scriptXML    = ' '
     C                   Eval      scriptMsg    = ' '
     C                   Eval      scriptString = ' '
     C*                  CallP     STLETTER('CHKXMLM':mCLdcid:scriptMsg)
     C                   Eval      scriptMode2  = 'C'
     C                   Eval      scriptRrn    = %EditC(@@rrn:'X')
     C                   Eval      scriptPrefix = mCLcat
     C                   Eval      scriptCo#    = %EditC(CMco#:'X')
     C                   Eval      scriptFnd    = %EditC(CMfnd:'X')

     C                   CallP     StLetter(scriptMode:scriptXML:scriptMsg:
     C                             scriptMode2:scriptRrn:scriptPrefix:
     C                             scriptString:scriptCo#:scriptFnd)
     C                   ExSr      UpdateXMLCtl
     C                   Else
     C*
     C*  CALL CNTROL TO OBTAIN A 'REQUEST NUMBER'
     C*
     C                   EXSR      CNTROL
     C*
     C*  IF THIS IS A NEW REQUEST, INITIALIZE WORKFILE FIELDS
     C*
     C                   CLEAR                   PCL02
     C*
     C                   Z-ADD     CMCO#         CLCO#
     C                   Z-ADD     CMFND         CLFND
     C                   Z-ADD     CMFYR         CLFYR
     C                   Z-ADD     CMCASE        CLFCLM
     C                   Z-ADD     XXREQ#        CLREQ#
     C                   Z-ADD     CMEMP#        CLEMP#
     C                   Z-ADD     CMDIV         CLDIV
     C*
     C                   MOVE      CADPCD        CLDPCD
     C                   MOVE      CAFLDR        CLFLDR
     C                   MOVE      CADCID        CLDCID
     C     CA#CPY        IFNE      *ZEROS
     C                   Z-ADD     CA#CPY        CL#CPY
     C                   ELSE
     C                   Z-ADD     1             CL#CPY
     C                   END
      *
     C                   MOVE      'A'           CLSTAT
     C                   MOVEL     Q1USER        CLIUSR
     C                   MOVEL     Q1PGM         CLIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLIDT
     C*
     C                   Z-ADD     DSTIME        CLITIM
     C                   MOVE      CAOUTQ        CLOUTQ
     C*
     C                   WRITE     PCL02
     C                   EndIf
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************
     C* CNTROL - SUBROUTINE TO MAINTAIN A UNIQUE REQUEST NUMBER
     C*****************************************************************
     C     CNTROL        BEGSR
     C*
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C*
     C     CLKY          CHAIN     PCL01                              65
     C*
     C     *IN65         IFEQ      *OFF
     C*
     C     HIREQ#        IFLT      99999
     C     HIREQ#        ADD       1             HIREQ#
     C                   ELSE
     C                   Z-ADD     1             HIREQ#
     C                   END
     C*
     C*  UPDATE CONTROL RECORD
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C*
     C                   Z-ADD     DSTIME        HITIME
     C                   Z-ADD     DSTIME        HIITIM
     C                   MOVEL     Q1USER        HIIUSR
     C                   MOVEL     Q1USER        HIUSER
     C                   MOVEL     Q1PGM         HIIPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C*
     C                   UPDATE    PCL01
     C*
     C                   ELSE
     C*
     C*  WRITE THE CONTROL RECORD FOR DEPARTMENT REQUESTS
     C*
     C                   Z-ADD     1             HIREQ#
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C                   Z-ADD     *HIVAL        HIFYR
     C                   Z-ADD     *HIVAL        HIFCLM
     C                   Z-ADD     *HIVAL        HIEMP#
     C                   Z-ADD     *HIVAL        HIDIV
     C                   MOVE      *BLANKS       HIDPCD
     C                   MOVE      *BLANKS       HIFLDR
     C                   MOVE      *BLANKS       HIDCID
     C                   Z-ADD     *HIVAL        HI#CPY
     C                   MOVE      'A'           HISTAT
     C                   MOVE      Q1USER        HIIUSR
     C                   MOVE      Q1PGM         HIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C*
     C                   Z-ADD     DSTIME        HITIME
     C                   MOVE      Q1USER        HIIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C*
     C                   WRITE     PCL01
     C                   END
     C*
     C                   Z-ADD     HIREQ#        XXREQ#
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************
     C* SUBMIT SUBROUTINE              ** Subroutine **   S U B M I T *
     C*****************************************************************
     C     SUBMIT        BEGSR
     C*
     C*   ASSIGN PARM FIELDS, THEN CALL SUBMITTER CLP
     C*
     C                   If        scriptura = *On
     C                   Eval      ##xmlFile = scriptXML + '$'
     C                   CALL      'S4400C'
     C                   PARM                    ##XMLFILE        61
     C                   Else
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      XXREQ#        QQREQ#
     C                   MOVE      CADPCD        QQDPCD
     C                   MOVE      CAFLDR        QQFLDR
     C                   MOVE      CADCID        QQDCID
     C*
     C                   CALL      'W4110C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQREQ#            5
     C                   PARM                    QQDPCD            5
     C                   PARM                    QQFLDR           80
     C                   PARM                    QQDCID           12
     C                   EndIf
     C*
     C                   ENDSR
     C*
      /free
       //===================================================================
       // Check to see if Auto letter will be issued.
       //===================================================================
       BegSr IssueAutoLet;

       Chain CMco# scl00;
       If %Found(SACLL);
          DoU %Eof(SACLL);
             // Initialize Fields
             statCondMet    = *Off;
             ltCondMet      = *Off;
             isStatCond     = *Off;
             isLtCond       = *Off;
             trigCondMet    = *Off;
             goodAutoLetter = *Off;

             // Lets see if the trigger conditions have been met.

             // Evaluate if New status condition set and if so was it met.
             If mCLcsFR = 'N';
                //*** Beg Chg *** 01/15/24 ************************************
                //***If @@csFrm = ' ' And @@ltFrm = ' ' And CMstat = mCLcsTo
                //*** And CMltfl = mCLltTo;
                If @@csFrm = ' ' And CMstat = mCLcsTo;
                //*** End Chg *** 01/15/24 ************************************
                   statCondMet = *On;
                EndIf;
                isStatCond = *On;
             Else;
                // Evaluate if status condition set and if so was it met.
                If mCLcsFR <> ' ';
                  //*** Beg Chg *** 06/20/25 ******************************************
                   //*** If @@csFrm = mCLcsFr And CMstat = mCLcsTo;
                   If @@csFrm = mCLcsFr And CMstat = mCLcsTo
                   or @@csFrm = mCLcsTo And CMstat = mCLcsFr   //Close claim with Close/Open
                   or @@csFrm='C' and CMstat='O'
                   and (mCLcsFr='O' or mCLcsTo='O'); //Claim is Reopened
                  //*** End Chg *** 06/20/25 ******************************************
                      statCondMet = *On;
                   EndIf;
                   isStatCond = *On;
                EndIf;
             EndIf;

             // Evaluate if lost time condition set and if so was it met.
             ltLstCondMet = *On;
             Select;
             When mCLltFR <> ' ';
                If @@LtFrm = mCLltFr And CMltfl = mCLltTo;
                   ltCondMet = *On;
                EndIf;
                isLtCond = *On;
             When mCLltFlLst <> ' ';
                For i = 1 to 3;
                   arrLtLst(i) = %Subst(mCLLTFlLst:i:1);
                Endfor;
                i = %Lookup(CMltfl:arrLtLst);
                If i = 0;
                   ltLstCondMet = *Off;
                EndIf;
             Endsl;

             // Now lets see if the overall trigger condition is met

             Select;


             When mCLstat = 'I';
                trigCondMet = *Off;


             When isStatCond = *On And isLtCond = *On;
                If mCLandR = 'O';
                   If statCondMet = *On Or ltCondMet = *On;
                      trigCondMet = *On;
                   EndIf;
                Else;
                   If statCondMet = *On And ltCondMet = *On;
                      trigCondMet = *On;
                   EndIf;
                EndIf;
             When isStatCond = *On;
                If statCondMet = *On;
                   trigCondMet = *On;
                EndIf;
             When isLtCond = *On;
                If ltCondMet = *On;
                   trigCondMet = *On;
                EndIf;
             EndSl;

             If trigCondMet = *On;
                If ltLstCondMet = *Off;
                   trigCondMet = *Off;
                Endif;
             Endif;

             // Trigger conditions have been met
             // Now we are seeing if this Auto Letter matches by
             // Group, Jurisdiction State and Office#.
             If trigCondMet = *On;
                goodAutoLetter = *On;
                // Let's Check the group
                Chain (CMco#:mCLalSq) scl0grp;
                If %Found(SACLLGRP);
                   Chain (CMco#:mCLalSq:CMfnd) scl0grp;
                   If Not %Found(SACLLGRP);
                      goodAutoLetter = *Off;
                   EndIf;
                EndIf;
                // Let's Check the Jurisdiction State
                If goodAutoLetter = *On;
                   Chain (CMco#:mCLalSq) scl0jst;
                   If %Found(SACLLJST);
                      Chain (CMco#:mCLalSq:CMjrSt) scl0jst;
                      If Not %Found(SACLLJST);
                         goodAutoLetter = *Off;
                      EndIf;
                   EndIf;
                EndIf;
                // Let's Check the Office #
                If goodAutoLetter = *On;
                   Chain (CMco#:mCLalSq) scl0off;
                   If %Found(SACLLOFF);
                      Chain (CMco#:mCLalSq:CMoff#) scl0off;
                      If Not %Found(SACLLOFF);
                         goodAutoLetter = *Off;
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             // All conditions have been met so lets issue this letter
             If goodAutoLetter = *On;
                ExSr WrtUpd;
                ExSr Submit;
             EndIf;

             ReadE CMco# scl00;
          EndDo;
       EndIf;

       EndSr;
      /end-free
