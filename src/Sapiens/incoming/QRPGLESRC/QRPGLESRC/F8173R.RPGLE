     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F8173R - Provider Export Program
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*     PROVIDER EXPORT PROGRAM                                   *
     H*                                                               *
     H*  CALLED FROM: F8173C - Provider Export Override Program       *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED: 10/21/10       PROGRAMMER: 227     PROJECT: 133MJS  *
     H*                          Copied from CLT1036/W8163R           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/08/19   C:079   Project: 152MAP                           *
     H*                     Default Parn to N if blank                *
     H*                                                               *
     H*  12/06/17   C:079   Project: 146MGU                           *
     H*                     Run type 5 separate from otheres          *
     H*                                                               *
     H*  11/07/17   C:079   Project: 146MGU                           *
     H*                     Allow provider types other than 1,4 to    *
     H*                     be extracted                              *
     H*                                                               *
     H*  05/01/17   C:079   Project: MC325                            *
     H*                     Add logic for SA016R                      *
     H*                                                               *
     H*  08/25/14   C:296   Support: 133MNC 86423                     *
     H*                     Correct client coding to select           *
     H*                     providers.                                *
     H*                                                               *
     H*  04/04/13   C:277   PROJECT: 133MNC                           *
     H*                     Include active and inactive providers     *
     H*                                                               *
     H*  12/07/10   C:227   PROJECT: 133MJS                           *
     H*                     Priss#8 Zero fill FEIN                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*  FMPDL    PPD01: PDFEIN PDMOD            WCMC Provider Master *
     H*  FMP1L    PP101: P1FEIN P1MOD            WCMC Provider Master *
     H*  FAPEP    PPE01:                         Provider Export File *
     H*                                                               *
     H*****************************************************************
     FFMPDL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFAPEP     O    E             DISK
     f*** Beg Add *** 11/07/17 ***********************************
     fwmpdl     if   e           k Disk    UsrOpn Rename(ppd01:ppd02)
     f                                     Prefix(P2:2)
      /copy sPrModule
     f*** End Add *** 11/07/17 ***********************************
     d*** Beg Add *** 05/01/17 ***********************************
     d rtvSettings     pr                  Extpgm('SA016R')
     d  @@Cchg                        1
     d  @@Cifs                        1
     d  @@Csts                        1
     d  @@Cdny                        1
     d  @@Ccvfl                       1
     d  @@Cgrp                        1
     d  @@Cemp                        1
     d  @@Eifs                        1
     d  @@Eagt                        1
     d  @@Eemp                        1
     d  @@Fifs                        1
     d  @@Ftyp                        1
     d  @@Udup                        1
     d  @@Udrc                        1
     d  @@Urel                        1
     d  @@Upcl                        1
     d  @@Updn                        1
     d  @@Udpn                        1
     d  @@ClDays                      5  0
     d  @@IncZro                      1
     d  @@Crcd                        1
     d  @@CrDth                       1
     d  @@RtCd                        1
     d  @@Cchg         s              1
     d  @@Cifs         s              1
     d  @@Csts         s              1
     d  @@Cdny         s              1
     d  @@Ccvfl        s              1
     d  @@Cgrp         s              1
     d  @@Cemp         s              1
     d  @@Eifs         s              1
     d  @@Eagt         s              1
     d  @@Eemp         s              1
     d  @@Fifs         s              1
     d  @@Ftyp         s              1
     d  @@Udup         s              1
     d  @@Udrc         s              1
     d  @@Urel         s              1
     d  @@Upcl         s              1
     d  @@Updn         s              1
     d  @@Udpn         s              1
     d  @@ClDays       s              5  0
     d  @@IncZro       s              1
     d  @@Crcd         s              1
     d  @@CrDth        s              1
     d  @@RtCd         s              1
     d  isSettings     s              1
     d*** End Add *** 05/01/17 ***********************************
     d*** Beg Add *** 12/06/17 ***********************************
     d CvtDate         pr                  Extpgm('SRCVTDAT')
     d  @@inDate                      8  0
     d  @@InFromFmt                   8
     d  @@InToFmt                     8
     d  @@OutDate                    10
     d  @@OutErr                      1
     d  @@inDate       s              8  0
     d  @@InFromFmt    s              8
     d  @@InToFmt      s              8
     d  @@OutDate      s             10
     d  @@OutErr       s              1
     d*** End Add *** 12/06/17 ***********************************

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PEDATA         UDS
     D  PEDADS                 1     10
     D  PEDAMM                 1      2
     D  PEDAS1                 3      3
     D  PEDADD                 4      5
     D  PEDAS2                 6      6
     D  PEDACY                 7     10
     d*** Beg Add *** 12/06/17 *****************************************
     d PEdatatyp5     uds
     d  PEdaDstyp5             1     10
     d  PEDaMmtyp5             1      2
     d  PEdAs1typ5             3      3
     d  PEdAddtyp5             4      5
     d  PEdAs2typ5             6      6
     d  PEdAcytyp5             7     10
     d*** End Add *** 12/06/17 *****************************************
     D*
     D                 DS
     D  RUNDAT                 1      8  0
     D  RUNDMM                 1      2
     D  RUNDDD                 3      4
     D  RUNDCY                 5      8
     D*** Beg Add *** 04/04/13 ****************************************
     D RTVCLTID        Pr                  EXTPGM('RTVCLTID')
     D ##CLT#                         3
     D*
     D ##CLT#          s              3
     D*** End Add *** 04/04/13 ****************************************
     d*** Beg Add *** 12/06/17 ****************************************
     d Rundt7          s              7  0
     d*** End Add *** 12/06/17 ****************************************

     C*****************************************************************
     C*   KEY LISTS
     C*****************************************************************
     C*
     C*   Provider Auxiliary File
     C*
     C     P1KY          KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     C*
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DCID           12
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*****************************************************************
     C*   Initialize
     C*****************************************************************
     C*
     C*  PGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*  Read 1st record
     C*
     C                   READ      PPD01                                  99
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C*  If subset criteria met
     C*
     C                   EXSR      SUBSET
     C*
     C     MEETSS        IFEQ      *ON
     C                   EXSR      WRITER
     C                   ENDIF                                                  MEETSS = *ON
     C*
     C                   READ      PPD01                                  99
     C                   ENDDO                                                  *IN99 DOWEQ'0'
       //*** Beg Add *** 11/07/17 ***********************************
      /Free
       // Read 1st record of the Provider File on the Claims side
       // if selection is other than 1's and 4's.
                         if isSettings = '1' and @@fTyp <> ' ' and
                           @@fTyp <> 'A' and @@fTyp <> '1' and
                           @@fTyp <> '4';
                            Read ppd02;
                            Dow Not %Eof(wmpdl);
       // If subset criteria met
                               Exsr SubsetClaims;
                               If MeetSS = *On;
                                  Exsr WriterClaims;
                               Endif;
                               Read ppd02;
                            Enddo;
                         Endif;
      /End-Free
      //*** End Add *** 11/07/17 ***********************************
     C*
     C*  Last Record processing
     C*
     C                   EXSR      CRTDOC
     C*
     C*** BEGIN ADD *** 10/25/05 **************************************
     C*** Beg Add *** 12/06/17 ****************************************
     C     @@ftyp        IfNe      '5'
     C*** End Add *** 12/06/17 ****************************************
     C                   MOVEL     DSDATE        RUNDAT
     C                   MOVEL     RUNDMM        PEDAMM
     C                   MOVEL     '/'           PEDAS1
     C                   MOVEL     RUNDDD        PEDADD
     C                   MOVEL     '/'           PEDAS2
     C                   MOVEL     RUNDCY        PEDACY
     C*** Beg Add *** 12/06/17 ****************************************
     C                   Endif
     C*** End Add *** 12/06/17 ****************************************
     C*** Beg Add *** 12/06/17 ****************************************
     C     @@ftyp        IfEq      '5'
     C                   MOVEL     DSDATE        RUNDAT
     C                   MOVEL     RUNDMM        PEDAMMTYP5
     C                   MOVEL     '/'           PEDAS1TYP5
     C                   MOVEL     RUNDDD        PEDADDTYP5
     C                   MOVEL     '/'           PEDAS2TYP5
     C                   MOVEL     RUNDCY        PEDACYTYP5
     C                   Endif
     C*** End Add *** 12/06/17 ****************************************
     C*** END   ADD *** 10/25/05 **************************************
     C*** Beg Add *** 11/07/17 ****************************************
     C                   Exsr      pgmEnd
     C*** Beg Add *** 11/07/17 ****************************************
     C*
     C*  End of Processing
     C*
     C                   MOVE      *ON           *INLR
     C*** Beg Add *** 11/07/17 ******************************************
     C*******************************************************************
     C* SubsetClaims - Check if member of records processed subset Claims
     C*******************************************************************
      /Free
       Begsr SubsetClaims;
       // MEETSS = *On initially if provider has been changed
           MeetSS = *off;
           if P2dt >= RunDt7;
              MeetSS = *on;
           Endif;
       // MEETSS = *OFF, provider is not active
           if P2stat <> 'A';
              MeetSS = *off;
           Endif;
           if isSettings = '1' and @@fTyp <> ' ';
              if P2type <> @@fTyp;
                 meetss = *off;
              endif;
           endif;
       Endsr;
      /End-Free
     C*** End Add *** 11/07/17 ******************************************
     C*****************************************************************
     C* SUBSET - Check if member of records processed subset
     C*****************************************************************
     CSR   SUBSET        BEGSR
     C*
     C                   MOVE      *OFF          MEETSS            1
     C*
     C*  MEETSS = *ON, if to be processed
     C*
     C     PDDT          IFGE      RUNDT7
     C                   MOVEL     *ON           MEETSS
     C                   ENDIF                                                  PDDT >= RUNDT7
     C*
     C*  MEETSS = *OFF, provider is not active
     C*
     C*** Beg Add *** 04/04/13 ****************************************
      /Free
          // Beg Chg *** 08/25/14 ****************************************
          // If ##CLT# = '133' and (PDstat <> 'A' and PDstat <> 'I') ;
          // MEETSS = *Off;
             If ##CLT# = '133';
                If (PDstat <> 'A' and PDstat <> 'I');
                   meetss = *off;
                Endif;
          // End Chg *** 08/25/14 ****************************************
          Else;
      /End-Free
     C*** End Add *** 04/04/13 ****************************************
     C     PDSTAT        IFNE      'A'
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF                                                  PDSTAT <> 'A'
     C*** Beg Add *** 04/04/13 ****************************************
      /Free
          Endif;
      /End-Free
     C*** End Add *** 04/04/13 ****************************************
     C*
     C*  MEETSS = *OFF, Use Settings file if loaded. If not default 1,4
     C*
     C*** Beg Chg *** 05/01/17 ****************************************
     C**** PDTYPE        IFNE      '1'
     C**** PDTYPE        ANDNE     '4'
     C****               MOVEL     *OFF          MEETSS
     C****               ENDIF                                                  PDSTAT <> 'A'
      /free
           if isSettings = '1' and @@fTyp <> ' ';
              if PDtype <> @@fTyp and @@fTyp <> 'A';
                 meetss = *off;
              endif;
              if @@fTyp = 'A' and PDtype <> '1' and PDtype <> '4';
                 meetss = *off;
              endif;
           else;
              if PDtype <> '1' and PDtype <> '4';
                 meetss = *off;
              endif;
           endif;
      /end-free
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE FAPEP
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PPE01
     C*
     C                   MOVE      'Y'           HIT
     C*
     C*** BEG CHG *** 12/07/10 ****************************************
     C***********          MOVELPDFEIN    XXFEIN 11
     C***********          MOVELPDMOD     XXMOD   3
     C                   MOVEL     PDFEIN        XXFEIN            9
     C                   MOVE      *ALL'0'       XXMOD
     C                   MOVEL     PDMOD         XXMOD             3
     C*** END CHG *** 12/07/10 ****************************************
     C*
     C     XXFEIN        CAT       XXMOD         PEPROV                         Provr Identifier
     C                   MOVEL     XXFEIN        PEFEIN                         Tax Id.
     C*
     C                   MOVEL     PDNAME        PENAME                         Provr Last Name
     C                   MOVEL     PDFSN         PEFSN                          Provr First Nam
     C                   MOVEL     *BLANKS       PEMID                          Provr Middle In
     C                   MOVEL     PDADR1        PEADR1                         Provider Addr 1
     C                   MOVEL     PDADR2        PEADR2                         Provider Addr 2
     C                   MOVEL     PDCTY         PECTY                          Provider City
     C                   MOVEL     PDST          PEST                           Provider State
     C                   MOVEL     PDZIP         PEZIP                          Provider Zip
     C*
     C                   SELECT
     C     PDTYPE        WHENEQ    '1'
     C                   MOVEL     '01'          PEPTYP                         Provider Type
     C     PDTYPE        WHENEQ    '4'
     C                   MOVEL     '04'          PEPTYP                         Provider Type
     C                   ENDSL
     C*
     C                   MOVEL     PDSPCD        PESPEC                         Provr Specialty
     C*
     C                   MOVEL     PDLIC#        PELIC#                         License Number
     C                   MOVEL     PDLIC#        PEMED#                         Medicare Oscar#
     C                   MOVEL     PDLIC#        PEMSPC                         Medicare Specialty
     C                   MOVEL     *BLANKS       PENID                          National ID
     C                   MOVEL     *BLANKS       PENABP                         NABP Number
     C                   MOVEL     PDSTAT        PESTAT                         Provider Status
     C                   MOVEL     PDTAXO        PETAXO                         Taxonomy Code
     C                   MOVEL     *BLANKS       PEFT                           Facility Type
     C                   MOVEL     PDPARN        PEPARN                         Participating Networ
       //*** Beg Add *** 05/08/19 ******************************************
      /Free
                         If PEparn = ' ';
                            PEparn = 'N';
                         Endif;
      /End-free
       //*** End Add *** 05/08/19 ******************************************
     C*
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     P1MCR#        PEMED#                         Medicare ID
     C                   MOVEL     P1N#02        PENID                          Medicare ID
     C                   ENDIF
     C*
     C                   WRITE     PPE01
     C*
     C                   ENDSR                                                  WRITER
     C*** Beg Add *** 11/07/17 ******************************************
     C*****************************************************************
     C* WriterClaims - Write FAPEP for Claims Provider File
     C*****************************************************************
      /Free
                         BegSr WriterClaims;
                         Clear ppe01;
                         Hit = 'Y';
                         PEprov =  %EditC(P2fein:'X') + %EditC(P2mod:'X');
                         PEfein = %EditC(P2fein:'X');
                         PEname = P2name;
                         PEfsN = P2fsN;
                         PEmid = *Blanks;
                         PEadr1 = P2adr1;
                         PEadr2 = P2adr2;
                         PEcty = P2cty;
                         PEst = P2st;
                         PEzip = %Char(P2zip);
                         PEpTyp = '0' + P2type;
                         PEspec = P2spCd;
                         PElic# = P2lic#;
                         PEmed# = P2lic#;
                         PEmSpc = P2lic#;
                         PEnID = *Blanks;
                         PENaBp = *Blanks;
                         PEstat = P2stat;
                         PEtaxo = *Blanks;
                         PEft = *Blanks;
        //*** Beg Chg *** 05/08/19 *****************************************
        //*********      PEparN = *Blanks;
                         PEparN = 'N';
        //*** End Chg *** 05/08/19 *****************************************
                         Write ppe01;
                         Endsr;
      /End-Free
     C*****************************************************************
     C* PGMEND - Close files, etc.
     C*****************************************************************
      /Free
       Begsr PgmEnd;

           If isModEnabled('WorkComp') and %Open(wmpdl);
             Close wmpdl;
           Endif;

       endsr;
      /End-Free
       //*** End Add *** 11/07/17 ****************************************
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C                   MOVE      'N'           HIT               1
     C                   TIME                    DSDTTM
     C*
     C* Get Last run date
     C*
     C*** Beg Del *** 12/06/17 ****************************************
     C****
     C**** PEDADS        IFEQ      *BLANKS
     C****               MOVE      *ZEROS        RUNDT7            7 0
     C****               ELSE
     C****               MOVEL     PEDAMM        RUNDMM
     C****               MOVEL     PEDADD        RUNDDD
     C****               MOVEL     PEDACY        RUNDCY
     C****               Z-ADD     RUNDAT        SRDTI
     C****               CALL      'SRCVTDAT'    DTCYMD
     C****               MOVEL     SRDTO         RUNDT7
     C****               ENDIF
     C*** End Del *** 12/06/17 ****************************************
     C*** Beg Add *** 04/04/13 ****************************************
      /Free
          ##CLT# = *Blanks;
          callp RtvCltID(##CLT#);
      /End-Free
     C*** End Add *** 04/04/13 ****************************************
     C*** Beg Add *** 05/01/17 ****************************************
      /free
            rtvSettings(@@Cchg:@@Cifs:@@Csts:@@Cdny:@@Ccvfl:@@Cgrp:
                         @@Cemp:@@Eifs:@@Eagt:@@Eemp:@@Fifs:@@Ftyp:
                         @@Udup:@@Udrc:@@Urel:@@Upcl:@@Updn:@@Udpn:
                         @@ClDays:@@IncZro:@@Crcd:@@CrDth:@@RtCd);
            if @@RtCd = '1';
               isSettings = '1';
            else;
               isSettings = '0';
            endif;
      /end-free
     C*** End Add *** 05/01/17 ****************************************
       //*** Beg Add *** 11/07/17 ****************************************
      /Free
       If isModEnabled('WorkComp');
         Open wmpdl;
       Endif;
      /End-free
       //*** End Add *** 11/07/17 ****************************************
     C*
     C*** Beg Add *** 12/06/17 ****************************************
      /Free
       Select;
       When @@ftyp <> '5';
          if PEdaDs = *blanks;
             RunDt7 = *Zeros;
          Else;
             RunDmm = PEdaMm;
             RunDdd = PEdadd;
             RunDcy = PEdacy;
             @@InDate = RunDat;
             @@InFromFmt = *blanks;
             @@InToFmt = '*CYMD';
             Callp CvtDate(@@InDate:@@inFromFmt:@@IntoFmt:
               @@OutDate:@@OutErr);
             RunDt7 = %Dec(@@OutDate:7:0);
          Endif;
       When @@ftyp = '5';
          if PEdaDsTyp5 = *blanks;
             RunDt7 = *Zeros;
          Else;
             RunDmm = PEdaMmTyp5;
             RunDdd = PEdaddTyp5;
             RunDcy = PEdacyTyp5;
             Monitor;
             @@InDate = RunDat;
             On-Error;
             @@InDate = 0;
             Endmon;
             @@InFromFmt = *blanks;
             @@InToFmt = '*CYMD';
             Callp CvtDate(@@InDate:@@inFromFmt:@@IntoFmt:
             @@OutDate:@@OutErr);
             Monitor;
             RunDt7 = %Dec(@@OutDate:7:0);
             On-Error;
             RunDt7 = *zeros;
             Endmon;
          Endif;
       Endsl;

      /End-free
     C*** End Add *** 12/06/17 ****************************************
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* CRTDOC - CREATE THE DOC NAME
     C*****************************************************************
     CSR   CRTDOC        BEGSR
     C*
     C     HIT           IFEQ      'Y'
     C*
     C                   TIME                    DSDTTM
     C*
     C     DSTIME        IFLE      120000
     C                   CALL      'SRPAST8'
     C     DSDATE        PARM      DSDATE        ##DATE            8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM      *BLANKS       ##ERR             1
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT7            7
     C                   MOVE      XXDAT7        XXDAT6            6
     C*
     C     'PE'          CAT(P)    XXDAT6:0      @@DCID
     C                   CAT       '.TXT':0      @@DCID
     C*
     C                   ELSE
     C                   CLEAR                   @@DCID
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
