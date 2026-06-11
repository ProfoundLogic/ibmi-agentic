     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*                                                               *
     H*  Design Note: Re-Wrote based on Program W7511R to process     *
     H*  (12/09/24)   Mass Certificate Print for Groups selected.     *
     H*               Date Range (Policy Effective Date Range)        *
     H*               RNB (Renewal, New, Both)                        *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H/TITLE W7512R - MASS PRINT CERTIFICATE HOLDER LETTER            *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/20/24 C:340 Project: WC2988                                *
     H*                Re-Wrote based on Program W7511R to process    *
     H*                Mass Certificate Print.                        *
     H*                Groups selected.                               *
     H*                Date Range (Policy Effective Date Range)       *
     H*                RNB (Renewal, New, Both)                       *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* N72 - OK TO OPEN OUTPUT PRINTER FILE                          *
     H*                                                               *
     H*****************************************************************
     FWTLML1    IF   E           K DISK
     F                                     ReName(PLM01:PLM02)
     FWMRSL     IF   E           K DISK
     FWTXCL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     InfDS(dbfds)
     FWMCEL6    UF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWDRRL     IF   E           K DISK
      * Additional Named Insured
      *
      *  Carrier
     FWMRPL2    IF   E           K DISK
      *  Group Auxiliary File
     FWDF2L     IF   E           K DISK
     F* Owners/Officers Master File
     FWMOOL8    IF   E           K DISK
      *
      *  Chapter File
     FWMCPL     IF   E           K DISK
     FWTGTL     IF   E           K DISK    ReName(pgt01:pgt00)
     Fwxchp1    o    e             disk    UsrOpn
     F                                     ReName(pch01:pch01p)
     FXMLPREL1  UF   E           K DISK
     Fwscil     IF   E           K DISK    ReName(pci01:pci00)
     FXMLTEML1  UF   E           K DISK
      *
     FW7511O    O    E             PRINTER OFLIND(*IN01)
     F                                     UsrOpn
     FW7511O1   O    E             PRINTER OFLIND(*IN02)
     F                                     USROPN
     fWXGSL     IF   E           K Disk
     FWDELL2    IF   E           K DISK    ReName(PEL01:PEL02)
     F                                     InfDS(DBFDSL2)
     D                 DS
     D  amn                    1    540    dim(9)
     D  ##amn1               001    060
     D  ##amn2               061    120
     D  ##amn3               121    180
     D  ##amn4               181    240
     D  ##amn5               241    300
     D  ##amn6               301    360
     D  ##amn7               361    420
     D  ##amn8               421    480
     D  ##amn9               481    540
    D*
     d #1emp#          s              9
     d #1div           s              5
     d ##odr           s              1
     d ##aodr          s              1
     d ##nodr          s              1
     d ##emn1          s             50
     d ##emn2          s             50
     d ##emn3          s             50
     d ##emn4          s             50
     d ##emn5          s             50
     d ##emn6          s             50
     d ##emn7          s             50
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d ##dbaf          s              1
     d ##peol          s              3  0
     d ##al2           s              3  0
     d ##nl2           s              3  0
     d ##fl2           s              3  0
     d ##pl2           s              3  0
     d ##cl2           s              3  0
     d QQname1         s            120
     d QQname2         s            120
     d QQname3         s            120
     d QQname4         s            120
     d QQPEOExt        s             10
     d QQfein          s              9  0
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x@RRN           s              9  0 inz(0)
     D*
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(CHXML)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D isUseScript     S              1n   inz
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('CI')
     d processType     c                   const('Certificate of Insurance')

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D*
     D/copy sPrModule
     D/copy sPrXML
     D/copy wPrEmplr
     D*
     D S4401C          pr                  ExtPgm('S4401C')
     D   xmlName                     61    const
     D*
     D ObjExists       pr             1n
     D  object                       10    const
     D  objType                       7    const

     D*
     D*   DEFINE TABLE FOR CANCEL DATES
     D*
     D NM              S             40    DIM(400) ASCEND                      Emp/Ent/Loc Names
     D RMK             S            112    DIM(14) ASCEND                       OUTPUT LINES
     D CE              S             55    DIM(7) ASCEND                        CERT HOLD ADDRESS
     D IN              S            112    DIM(6) ASCEND                        INSURED ADDRESS
     D VB              S             80    DIM(4) CTDATA PERRCD(1)              VERBAGE
     D VC              S             80    DIM(1) CTDATA PERRCD(1)              VERBAGE 102
     D VD              S             80    DIM(1) CTDATA PERRCD(1)              VERBAGE
     D V1              S            112    DIM(20)                              VBG ELEMENTS
     D*
     D*   DEFINE DATA STRUCTURES
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
      *
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      *
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANCEL DT ARRAY
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  CEZIP                  1      9  0
     D  CEZIP1                 1      5  0
     D  CEZIP2                 6      9  0
     D                 DS
     D  X8FRDT                 1      8
     D  X8MM                   1      2
     D  X8DD                   3      4
     D  X8CCYY                 5      8
     D                 DS
     D  X8CXDT                 1      8
     D  X8CMM                  1      2
     D  X8CDD                  3      4
     D  X8CYY                  5      8
     DCheckObjInLIBL   pr                  extPgm('SRCHKOBJ')
     D outObject                     10    const
     D outObjectType                  7    const
     D inError                        1
     D
     D notAvail        s              1    inz(' ')
     D xxDtaAra        s             10    inz(' ')
     D                 ds
     D dataArea                       8    dtaara(*var:xxDtaAra)
     D  xxCoord1                      4  2 overlay(dataArea)
     D  xxCoord2                      4  2 overlay(dataArea:5)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D PEOEXT          C                   CONST('L/C/F')
     D LONG            C                   CONST(70)
     C*
     D
     dGetRenewStatus   Pr                  Extpgm('WA069R')
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d inEMP#                         9  0 const
     d inFYR                          3  0 const
     d renewFlagValue                 1
     D
     D DBFDSL2         DS
     D  dbrrnL2              397    400b 0
     D
     D flgMultiGroup   S               N   Inz(*Off)
     D FrDate          S              8  0
     D ToDate          S              8  0
     D DateFr          S              7  0
     D DateTo          S              7  0
     D flgRNB          S              1A
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     EMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      *
     C     EMKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1EMP#
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE CANCELLATION/REINSTATEMENT STATE FILE
     C*
     C     XCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTE
     C                   KFLD                    XXEFDT
     C*
     C     XCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTE
     C*
     C*   KEY LIST FOR THE CERTIFICATE MASTER FILE
     C*
     C     CEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCER#
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMCHAP
      *
      *   STATISTICAL REPORTING/RATING FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXSTA
     C                   KFLD                    ELPFDT
      *
     C     RRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXSTA
     C*
     C*  FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  LIMITS FILE KEY
     C*
     C     LMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTA             2
     C                   KFLD                    XXKEY            10
     C                   KFLD                    XXILEF
     C*
     C     LMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTA             2
     C                   KFLD                    XXKEY            10
     C*
     C*
     C*  AGENT ASSIGNMENT BY FUND KEY
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ##AGT#
     C*
     C*  AGENT MASTER FILE KEY
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      *  Carrier
      *
     C     RPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
      * Additional Named Insured
      *
     C*
     C*  Owners/Officers Master File Key
     C*
     C     OOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *===============================================================
      * *LIKE DEFN
      *===============================================================
     C     *LIKE         DEFINE    EMEMP#        X1EMP#
     C     *LIKE         DEFINE    EMNAME        SVEM1
     C     *LIKE         DEFINE    EMNAME        SVEM
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ELI2EF        CURDAT
     C     *LIKE         DEFINE    LMEFDT        XXILEF
     C     *LIKE         DEFINE    LMEFDT        XXDATE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    XCEFDT        XXEFDT
     C     *LIKE         DEFINE    XCST          XXSTE
     C     *LIKE         DEFINE    EMCAD1        XXCX
     C     *LIKE         DEFINE    EMEMP#        XSEMP#
     C     *LIKE         DEFINE    EMDIV         XSDIV
     C*
     C                   Z-ADD     *ZEROS        XXACC             7 0
     C                   Z-ADD     *ZEROS        XXDIS             7 0
     C                   Z-ADD     *ZEROS        XXAGG             7 0
     C*                                   XVEMP
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQCER#            5
     C                   PARM                    QQCERT            5
     C                   PARM                    QQBLNK            1
     C                   PARM                    QQDATE            8
     C                   PARM                    QQRSFL            1
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQFrDt            8
     C                   PARM                    QQToDt            8
     C                   PARM                    QQRNBF            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYC        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
     C*
          If QQFrDt <> ' ' and QQToDt <> ' ';
             Monitor;
               FrDate = %Dec(QQFrDt:8:0);
               ToDate = %Dec(QQToDt:8:0);
               DateFr = %Dec(%Char(%Date(FrDate:*USA):*CYMD0):7:0);
               DateTo = %Dec(%Char(%Date(ToDate:*USA):*CYMD0):7:0);
             On-Error;
               DateFr = 0;
               DateTo = 0;
             EndMon;
          EndIf;
     C*
      /free
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);

       Clear xmlLink;
       Clear xmlFlowName;

       If QQEmp# <> *zeros and QQEmp# <> ' ' and QQCER# <> ' ' and
        QQCer# <> *zeros and (QQCerT = ' ' or QQCerT = *zeros or
        QQCer# = QQCerT);
          isMassReq = *off;
          massFlag = ' ';
       Else;
          isMassReq = *on;
          massFlag = 'Y';
       Endif;

       If isUseXML;
          Open wxchp1;
       Endif;

       If isUseSpoolOut;
          Open w7511o;
       Endif;

      /end-free
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQCER#        XXCER#            5 0
     C                   MOVE      QQBLNK        XXBLNK            1            BLANK CERT FLAG
     C                   MOVE      QQDATE        X1DATE            8 0
     C                   Clear                   svEmp#            9 0
     C                   Clear                   svDiv             5 0
     C                   Z-ADD     0             XXCERT            5 0
     C*
     C     QQCERT        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        QQCERT
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        XXCERT
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C                   MOVE      'X'           XXPRX
     C                   MOVE      'A'           XXPRA
      *
      *  XXWRT=Y
      *  Indicates there has been something written to spool file that
      *  is being resolved to DARTS
      *
     C                   MOVE(P)   'N'           XXWRT             1
      *
      * If the 'Resolve to DARTS' flag was passed and eq 'Y' then allow
      * program to write the DARTS record.
      *
     C                   MOVEL(P)  'N'           XXRSFL            1
     C     Q1PRMS        IFGE      10
     C     QQRSFL        ANDEQ     'Y'
     C                   MOVEL     'Y'           XXRSFL
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X8DATE            8 0
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CURDAT
     C                   MOVE      '0'           *IN40
     C                   MOVE      '0'           *IN41
     C                   MOVE      '0'           *IN42
     c*
     c* Check client data area
     c*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
      *
     C     ##CLT#        IFEQ      '126'
     C     'REFERENC'    CAT(P)    'E:':0        XXREF            11
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       XXREF            11
     C                   ENDIF                                                  ##CLT#
      *
     C     QQEMP#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN40
     C                   Z-ADD     0             XXEMP#
     C                   END
     C     QQCER#        IFEQ      '00000'
     C                   MOVE      '1'           *IN41
     C                   Z-ADD     0             XXCER#
     C                   END
     C*
     C     QQBLNK        IFEQ      'Y'
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   GET THE COMPANY INFORMATION
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   GET THE FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      *BLANKS       XXFNNM
     C     *IN60         IFEQ      '0'
      *
      * Use Aternate Group Name if entered
      *
     C                   MOVEL(P)  FNNAME        XXFNNM
     C                   ENDIF
     C*
     C*   GET THE USER PARAMETER INFORMATION
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANKS       UPUSNM
     C*
     C     FNKY          CHAIN     PF201                              50
     C*
     C*
     C*   Check if group is passed.
     C*   If Not passed (QQFnd = '000'), Loop through WXGSL for each
     C*   group included.
     C*   If group is passed then process for only that Group.
     C*
          flgMultiGroup = *Off;
          If QQFnd = ' ' Or QQFnd = '000';
             flgMultiGroup = *On;
             SetLL xxCo# pgs01;
             Reade xxCo# pgs01;
          Else;
             SetLL (xxCo#:XXFnd) pgs01;
             Reade (xxCo#:XXFnd) pgs01;
          EndIf;

          Dow Not %Eof(WXGSL);
              If GSIEFL <> 'I';
                 If flgMultiGroup = *On;
                    Reade xxCo# pgs01;
                 Else;
                    Reade (xxCo#:XXFnd) pgs01;
                 EndIf;
                 Iter;
              EndIf;
              XXFnd = GSFnd;

     C*
     C*   CHAIN TO THE EMPLOYER FILE FOR ALL EMPLOYERS FOR FUND
     C*
     C     EMKEY         CHAIN     PEM01                              50
     C*
     C*
     C     *IN50         DOWEQ     '0'
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C*   DO NOT PRINT IF EMPLOYER CANCELLED PRIOR TO RUN DATE
      *
     C                   Z-ADD     1             X1                3 0
      *
     C     *ZERO         LOOKUP    EF(X1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             X1
      *
     C                   ELSE
      *
     C                   Z-ADD     5             X1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     X1            IFNE      0
     C                   Z-ADD     CX(X1)        XXCX
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         XXCX
     C                   ENDIF                                                  X1 IFNE 0
      *
     C     XXCX          IFNE      *ZERO
     C*
     C* If employer is cancelled, format cancel date for output
     C*    turn on indicator to suppress printing of 'Continuous...'
     C*
     C                   Z-ADD     XXCX          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXENDT            6 0
      *
     C                   ELSE
      *
     C                   Z-ADD     0             X1
     C                   END
      *
     C*
     C*   RETRIEVE EMPLOYER DETAIL FILE
     C*
           SetLL (xxCO#:xxFND:xxEMP#:xxDIV:DateFr) PEL02;
           ReadE (xxCO#:xxFND:xxEMP#:xxDIV) PEL02;
           Dow Not %EOF(WDELL2);                             // Str - Employee Detail Loop
              If ELPFDT > DateTo;
                 ReadE (xxCO#:xxFND:xxEMP#:xxDIV) PEL02;
                 Iter;
              EndIf;
              GetRenewStatus(ELCO#: ELFND: ELEMP#: ELFYR: FlgRNB);
              If QQRNBF <> 'B' and FlgRNB <> QQRNBF;
                 ReadE (xxCO#:xxFND:xxEMP#:xxDIV) PEL02;
                 Iter;
              EndIf;
              x@RRN = dbRRNL2;
              xxFYR = ELFYR;
      *
      * Determine which Overlay and Signature to use for policy
      *
     C                   EXSR      GETOVL
     C*
     C*   FLIP POLICY FROM AND TO DATES
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C     ##CLT#        IFEQ      '135'
     C                   MOVEL     XXTODT        XXENDT
     C                   ENDIF
     C     XXCX          IFGT      ELPTDT
     C     XXCX          OREQ      *ZERO
     C                   Z-ADD     XXTODT        XXENDT
     C                   ENDIF
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8FRDT            8
     C     X8MM          CAT       '/':0         X8FRD2           10
     C                   CAT       X8DD:0        X8FRD2
     C                   CAT       '/':0         X8FRD2
     C                   CAT       X8CCYY:0      X8FRD2
     C*
     C     ##CLT#        IFEQ      '135'
     C     XXCX          ANDNE     *ZEROS
     C                   Z-ADD     XXCX          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8CXDT            8
     C     X8CMM         CAT       '/':0         X8FRX2           10
     C                   CAT       X8CDD:0       X8FRX2
     C                   CAT       '/':0         X8FRX2
     C                   CAT       X8CYY:0       X8FRX2
     C                   ENDIF
     C*
     C*   BLANK CERTIFICATE CONDITION
     C*
     C     *IN42         IFNE      '1'
     C*
     C*   CHAIN TO THE CERTIFICATE FILE FOR EACH EMPLOYER
     C*
     C     CEKEY         CHAIN     PCE01                              52
     C*
     C*   CHAIN TO CERTIFICATE HOLDERS FILE FOR ONE CERTIFICATE
     C*
     C     CEKEY2        SETLL     PCE01
     C     CEKEY1        READE     PCE01                                  52
     C*
     C     *IN52         DOWEQ     '0'
     C                   MOVE      *OFF          *IN77
     C*
     C*  DO NOT PRINT IF STATUS IS INACTIVE
     C*
     C     CESTAT        IFNE      'A'
     C                   GOTO      NXT1
     C                   END
      * If Exclude Officers flag is "Y", Print "YES" on PRTF forms
     C*** A logic is introduced to see if the officer excluded flag is
     C*** blank. If it is left blanks, then a blank should be passed in
     C*** XXYORN field. If OFEXBL is 'Y', then blanks should passed to XXYORN
     C                   MOVE      'N'           OFEXBL            1            Officer Excluded bln
     C                   SELECT
     C     CEEOFL        WHENEQ    'Y'
     C                   MOVE      *ON           *IN77
     C     CEEOFL        WHENEQ    'N'
     C                   MOVE      *OFF          *IN77
     C     CEEOFL        WHENEQ    ' '
     C                   MOVE      'Y'           OFEXBL
     C                   ENDSL
      *
     C                   MOVEL(P)  *BLANKS       XXDBA           112
     C                   MOVEL(P)  *BLANKS       XXFDNM          112
      *
     C                   move      xxco#         #1co#
     C                   move      xxfnd         #1fnd
     C                   move      xxemp#        #1emp#
     C                   move      xxdiv         #1div
     C                   move      ' '           ##odr
     C                   move      'F'           ##nodr
     C                   move      'M'           ##aodr
     C                   clear                   ##emn1
     C                   clear                   ##emn2
     C                   clear                   ##emn3
     C                   clear                   ##emn4
     C                   clear                   ##emn5
     C                   clear                   ##emn6
     C                   clear                   ##emn7
     C                   clear                   ##amn1
     C                   clear                   ##amn2
     C                   clear                   ##amn3
     C                   clear                   ##amn4
     C                   clear                   ##amn5
     C                   clear                   ##amn6
     C                   clear                   ##amn7
     C                   clear                   ##amn8
     C                   clear                   ##amn9
     C                   Clear                   QQPEOExt
     C*
     C                   select
     C                   when      FNPEOF = *blanks
     C                   clear                   ##pgfl
     C                   when      rrst <> '  '
     C                   eval      ##pgfl = RRPGFL
     c                   if        ##pgfl = *blank
     c                   move      'B'           ##pgfl
     c                   endif
     C                   Eval      QQPEOExt = RRPEOA
     C                   other
     C                   clear                   ##pgfl
     C                   endsl
     C                   Eval      ##dbaf = ' '
     C*
       ##adrl  = 0;
       ##naml  = 0;
       ##fedl  = 0;
       ##peo1  = ' ';
       ##peo2  = ' ';
       ##peol  = 0;
       ##al2   = 0;
       ##nl2   = 0;
       ##fl2   = 0;
       ##pl2   = 0;
       ##cl2   = 0;
       QQname1 = ' ';
       QQname2 = ' ';
       QQname3 = ' ';
       QQname4 = ' ';
       QQfein  = 0;

       GetNameAddress(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:
              ##emn1:##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:
              ##adrl:##naml:##fedl:##peo1:##peo2:##conl:##amn1:
              ##amn2:##amn3:##amn4:##amn5:##amn6:##amn7:##amn8:##amn9:
              ##peol:##al2:##nl2:##fl2:##pl2:##cl2:QQname1:QQname2:
              QQname3:QQname4:QQfein);
      *
     C     XXRSFL        IFEQ      'Y'
     C     isUseSpoolOut ANDEQ     *ON
     C  N72              OPEN      W7511O1
     C  N72              SETON                                        72
     C                   ENDIF
      *
     C                   If        isUseSpoolOut
     C                   WRITE     DETDT
     C   72              WRITE     DETDT1
     C                   Endif
     C                   MOVE(P)   'Y'           XXWRT             1
      *
      *  Get Agent Information
      *
     C                   EXSR      CURAGT
      *
     C                   If        isUseSpoolOut
     C                   WRITE     DETAGT
     C   72              WRITE     DETAGT1
     C                   WRITE     FNAM
     C   72              WRITE     FNAM1
     C                   Endif
     C*
     C*   EXECUTE SUB1 SUBROUTINE
     C*
     C                   EXSR      SUB1
     C*
     C     ELCXDY        IFNE      *ZEROS
     C                   Z-ADD     ELCXDY        XXDAYS
     C                   ELSE
     C                   MOVE      RSJRST        XXSTE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     XCKY          SETGT     PXC01
     C     XCKY2         READPE    PXC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     XCNOTC        XXDAYS            3 0
     C                   ELSE
     C                   Z-ADD     30            XXDAYS
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF
      *
      *  WRITE CERT HOLDER NAME / ADDRESS
      *
     C                   EXSR      WRTCE
     C                   MOVEL(P)  *BLANKS       XXDBA
     C                   MOVEL(P)  *BLANKS       XXFDNM
     C*
     C*   UPDATE CERTIFICATE COUNTER
     C                   ADD       1             CECNT
     C                   UPDATE    PCE01
     C                   TIME                    DSDTTM
     C                   Exsr      ScriptXML
     C*
     C*   GET NEXT CERTIFICATE
     C*
     C     NXT1          TAG
     C*
     C     *LIKE         DEFINE    CECER#        SVCER#
     C     *LIKE         DEFINE    CENAME        SVNAME
     C     *LIKE         DEFINE    CEFYR         SVFYR
     C*
     C                   MOVEL     CECER#        SVCER#
     C                   MOVEL     CENAME        SVNAME
     C                   MOVEL     CEFYR         SVFYR
     C*
     C     CEKEY1        READE     PCE01                                  52
     C*
     C     CECER#        IFGT      XXCERT
     C                   MOVE      *ON           *IN52
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ELSE
     C                   EXSR      SUB1
     C                   TIME                    DSDTTM
     C                   Z-add     0             CEZip
     C                   Clear                   amn
     C                   z-add     1             ##adrl
     C                   Exsr      ScriptXML
     C                   END
     C*
              ReadE (xxCO#:xxFND:xxEMP#:xxDIV) PEL02;
           EndDo;                                    // End - Employee Detail Loop
     C*
     C*   GET NEXT EMPLOYER
     C*
     C     NXT           TAG
      *
     C     XXWRT         IFEQ      'Y'
     C     XXRSFL        ANDEQ     'Y'
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C     EMKEY         READE     PEM01                                  50    ALL EMP IN FND
     C*
     C                   END
      /Free
     C*
     C*   Check if its Multi Group
     C*
          If flgMultiGroup = *On;
             Reade xxCo# pgs01;
          Else;
             Reade (xxCo#:XXFnd) pgs01;
          EndIf;
          EndDo;

       If isUseSpoolOut;
          Close w7511o;
       Endif;

       If isUseXML;
          Close wxchp1;
       Endif;

       Select;
       When xmlFlowName   = *Blanks;
       When objExists('PDSDEV':'*LIB');
          Monitor;
          S4401C(xmlFlowName + '$');
          On-error;
          Endmon;
       Other;
          S4401C(xmlFlowName + '$');
       Endsl;

      /End-free
     C*
     C*   END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
      *
      *
      *==============================================================
      * GETOVL - DETERMINE OVERLAY AND SIGNATURE TO APPLY
      *        - POSITIONING FOR PRINTING DATA
      *==============================================================
     C     GETOVL        BEGSR
      *
     C                   MOVE      *OFF          *IN30
     C                   MOVE      *OFF          *IN31
     C     FNKY          CHAIN     PF201                              60
     C                   CLEAR                   PRS01
     C                   CLEAR                   PRR01
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXSTA
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  RSKY
      *
      **************
      * OVERLAY
      **************
     C                   SELECT
      * STATE
     C     RRCEOV        WHENNE    *BLANK
     C                   MOVEL(P)  RRCEOV        XXCEOV            8
      * GROUP
     C     F2CEOV        WHENNE    *BLANK
     C                   MOVEL(P)  F2CEOV        XXCEOV
      * DEFAULT
     C                   OTHER
     C                   MOVEL(P)  'CERT01'      XXCEOV
      *
     C                   ENDSL
      *
      * Determine offsets for OVERLAY to be printed.
      *
     C                   SELECT
      *
     C     XXCEOV        WHENEQ    'CERT01'
     C                   EXSR      CERT01
      *
     C     XXCEOV        WHENEQ    'CERTNC'
     C     XXCEOV        OREQ      'CERT0909'
     C                   EXSR      CERTNC
      *
      /Free
       When xxCEOV = 'CERT1005';
          Exsr Cert1005;
      /End-free
     C                   OTHER
     C                   EXSR      CERT01
      *
     C                   ENDSL
      *
      *
      ****************
      * SIGNATURE
      ****************
     C                   SELECT
      * STATE
     C     RRSPSG        WHENNE    *BLANK
     C                   MOVEL(P)  RRSPSG        XXSPSG            8
      * GROUP
     C     F2SPSG        WHENNE    *BLANK
     C     RRCEOV        ANDEQ     *BLANK
     C                   MOVEL(P)  F2SPSG        XXSPSG
      * DEFAULT
     C                   OTHER
     C                   MOVEL(P)  *BLANKS       XXSPSG
      *
     C                   ENDSL
      /free

       xxDtaAra = xxCeov;

       CheckObjInLIBL(xxDtaAra:'*DTAARA':notAvail);

       If notAvail <> '1';
          In dataArea;
          VTSPSG = xxCoord1;
          HZSPSG = xxCoord2;
       Else;
      /end-free
      *
      * Determine offsets for SIGNATURE to be printed.
      *
     C                   SELECT
      *
     C     XXSPSG        WHENEQ    'SG10201'
     C                   Z-ADD     9.66          VTSPSG
     C                   Z-ADD     5.60          HZSPSG
     C                   OTHER
     C                   Z-ADD     9.95          VTSPSG
     C                   Z-ADD     5.60          HZSPSG
     C                   ENDSL
     C                   Endif                                                  no data area found
      *
     C                   ENDSR                                                  GETOVL
      *
      *
      *==============================================================
      * WRTFRM - DETERMINE OFFSETS AND WRITE OVERLAY / SIGNATURE
      *==============================================================
     C     WRTFRM        BEGSR
      *
      * Print Overlay
      *
     C                   If        isUseSpoolOut
     C     XXCEOV        IFNE      *BLANKS
     C                   WRITE     CEOV
     C   72              WRITE     CEOV1
     C                   ENDIF
      *
      * Print Signature
      *
     C     XXSPSG        IFNE      *BLANKS
     C                   WRITE     SPSG
     C   72              WRITE     SPSG1
     C                   ENDIF
     C                   Endif
      *
     C                   ENDSR                                                  WRTFRM
      *
      *
      *==============================================================
      * CERT01 - Overlay offsets
      *==============================================================
     C     CERT01        BEGSR
      *
     C                   MOVE      *ON           *IN30
      *
     C                   Z-ADD     .1            VTCEOV
     C                   Z-ADD     0             HZCEOV
      *
     C                   Z-ADD     .55           VTDATE
     C                   Z-ADD     6.7           HZDATE
      *
     C                   Z-ADD     0.95          VTAGNM
     C                   Z-ADD     0.3           HZAGNM
      *
     C                   Z-ADD     1.07          VTMAD1
     C                   Z-ADD     0.3           HZMAD1
      *
     C                   Z-ADD     1.19          VTCSZ
     C                   Z-ADD     0.3           HZCSZ
      *
     C                   Z-ADD     1.67          VTFNNM
     C                   Z-ADD     4.90          HZFNNM
      *
     C                   Z-ADD     1.67          VTNAIC
     C                   Z-ADD     7.00          HZNAIC
      *
     C                   Z-ADD     1.82          VTINS1
     C                   Z-ADD     0.30          HZINS1
      *
     C                   Z-ADD     1.94          VTINS2
     C                   Z-ADD     0.30          HZINS2
      *
     C                   Z-ADD     2.06          VTINS3
     C                   Z-ADD     0.30          HZINS3
      *
     C                   Z-ADD     2.18          VTINS4
     C                   Z-ADD     0.30          HZINS4
      *
     C                   Z-ADD     2.30          VTINS5
     C                   Z-ADD     0.30          HZINS5
      *
     C                   Z-ADD     2.42          VTINS6
     C                   Z-ADD     0.30          HZINS6
      *
     C                   Z-ADD     6.76          VTPRX
     C                   Z-ADD     5.43          HZPRX
      *
     C                   Z-ADD     7.10          VTPRA
     C                   Z-ADD     0.33          HZPRA
      *
     C                   Z-ADD     7.07          VTYORN
     C                   Z-ADD     2.04          HZYORN
      *
     C                   Z-ADD     7.10          VTCAP#
     C                   Z-ADD     2.52          HZCAP#
      *
     C                   Z-ADD     7.10          VTFRDT
     C                   Z-ADD     3.80          HZFRDT
      *
     C                   Z-ADD     7.10          VTENDT
     C                   Z-ADD     4.65          HZENDT
      *
     C                   Z-ADD     6.95          VTACC
     C                   Z-ADD     7.23          HZACC
      *
     C                   Z-ADD     7.16          VTDIS
     C                   Z-ADD     7.23          HZDIS
      *
     C                   Z-ADD     7.36          VTAGG
     C                   Z-ADD     7.23          HZAGG
      *
     C                   Z-ADD     7.62          VTREF
     C                   Z-ADD     0.73          HZREF
      *
     C                   Z-ADD     7.62          VTCOM1
     C                   Z-ADD     1.45          HZCOM1
      *
     C                   Z-ADD     7.96          VTDS01
     C                   Z-ADD     0.30          HZDS01
      *
     C                   Z-ADD     8.07          VTDS02
     C                   Z-ADD     0.30          HZDS02
      *
     C                   Z-ADD     8.18          VTDS03
     C                   Z-ADD     0.30          HZDS03
      *
     C                   Z-ADD     8.29          VTDS04
     C                   Z-ADD     0.30          HZDS04
      *
     C                   Z-ADD     8.40          VTDS05
     C                   Z-ADD     0.30          HZDS05
      *
     C                   Z-ADD     8.51          VTDS06
     C                   Z-ADD     0.30          HZDS06
      *
     C                   Z-ADD     8.62          VTDS07
     C                   Z-ADD     0.30          HZDS07
      *
     C                   Z-ADD     8.73          VTDS08
     C                   Z-ADD     0.30          HZDS08
      *
     C                   Z-ADD     8.84          VTDS09
     C                   Z-ADD     0.30          HZDS09
      *
     C                   Z-ADD     8.95          VTDS10
     C                   Z-ADD     0.30          HZDS10
      *
     C                   Z-ADD     9.06          VTDS11
     C                   Z-ADD     0.30          HZDS11
      *
     C                   Z-ADD     9.17          VTDS12
     C                   Z-ADD     0.30          HZDS12
      *
     C                   Z-ADD     9.28          VTDS13
     C                   Z-ADD     0.30          HZDS13
      *
     C                   Z-ADD     9.39          VTDS14
     C                   Z-ADD     0.30          HZDS14
      *
     C                   Z-ADD     9.59          VTDAYS
     C                   Z-ADD     4.38          HZDAYS
      *
     C                   Z-ADD     9.47          VTCE1
     C                   Z-ADD     0.80          HZCE1
      *
     C                   Z-ADD     9.59          VTCE2
     C                   Z-ADD     0.80          HZCE2
      *
     C                   Z-ADD     9.71          VTCE3
     C                   Z-ADD     0.80          HZCE3
      *
     C                   Z-ADD     9.83          VTCE4
     C                   Z-ADD     0.80          HZCE4
      *
     C                   Z-ADD     9.95          VTCE5
     C                   Z-ADD     0.80          HZCE5
      *
     C                   Z-ADD     10.07         VTCE6
     C                   Z-ADD     0.80          HZCE6
      *
     C                   Z-ADD     10.19         VTCE7
     C                   Z-ADD     0.80          HZCE7
      *
     C                   ENDSR                                                  CERT01
      *
      *
      *==============================================================
      * CERTNC - Overlay offsets - North Carolina Certificate
      *==============================================================
     C     CERTNC        BEGSR
      *
     C                   MOVE      *ON           *IN31
      *
     C                   Z-ADD     .1            VTCEOV
     C                   Z-ADD     0             HZCEOV
      *
     C                   Z-ADD     0.88          VTDATE
     C                   Z-ADD     7.1           HZDATE
      *
     C                   Z-ADD     2.12          VTAGNM
     C                   Z-ADD     0.40          HZAGNM
      *
     C                   Z-ADD     2.24          VTMAD1
     C                   Z-ADD     0.40          HZMAD1
      *
     C                   Z-ADD     2.36          VTCSZ
     C                   Z-ADD     0.40          HZCSZ
      *
     C                   Z-ADD     2.84          VTFNNM
     C                   Z-ADD     4.66          HZFNNM
      *
     C                   Z-ADD     2.84          VTNAIC
     C                   Z-ADD     7.26          HZNAIC
      *
     C                   Z-ADD     2.93          VTINS1
     C                   Z-ADD     0.40          HZINS1
      *
     C                   Z-ADD     3.05          VTINS2
     C                   Z-ADD     0.40          HZINS2
      *
     C                   Z-ADD     3.17          VTINS3
     C                   Z-ADD     0.40          HZINS3
      *
     C                   Z-ADD     3.29          VTINS4
     C                   Z-ADD     0.40          HZINS4
      *
     C                   Z-ADD     3.41          VTINS5
     C                   Z-ADD     0.40          HZINS5
      *
     C                   Z-ADD     3.53          VTINS6
     C                   Z-ADD     0.40          HZINS6
      ****
      *
     C                   Z-ADD     3.80          VTCER#
     C                   Z-ADD     3.50          HZCER#
      *
     C                   Z-ADD     3.80          VTREV#
     C                   Z-ADD     6.90          HZREV#
      ****
     C                   Z-ADD     7.51          VTPRX
     C                   Z-ADD     5.74          HZPRX
      *
     C                   Z-ADD     7.82          VTPRA
     C                   Z-ADD     0.28          HZPRA
      *
     C                   Z-ADD     7.72          VTYORN
     C                   Z-ADD     2.10          HZYORN
      *
     C                   Z-ADD     7.72          VTCAP#
     C                   Z-ADD     2.90          HZCAP#
      *  ZZZ
     C                   Z-ADD     7.72          VTFRDT
     C                   Z-ADD     4.50          HZFRDT
      *
     C                   Z-ADD     7.72          VTENDT
     C                   Z-ADD     5.11          HZENDT
      *
     C                   Z-ADD     7.67          VTACC
     C                   Z-ADD     7.32          HZACC
      *
     C                   Z-ADD     7.82          VTDIS
     C                   Z-ADD     7.32          HZDIS
      *
     C                   Z-ADD     7.97          VTAGG
     C                   Z-ADD     7.32          HZAGG
      ******
     C                   Z-ADD     8.54          VTDS01
     C                   Z-ADD     0.40          HZDS01
      *
     C                   Z-ADD     8.65          VTDS02
     C                   Z-ADD     0.40          HZDS02
      *
     C                   Z-ADD     8.76          VTDS03
     C                   Z-ADD     0.40          HZDS03
      *
     C                   Z-ADD     8.87          VTDS04
     C                   Z-ADD     0.40          HZDS04
      *
      ****
     C                   Z-ADD     9.40          VTCE1
     C                   Z-ADD     0.96          HZCE1
      *
     C                   Z-ADD     9.52          VTCE2
     C                   Z-ADD     0.96          HZCE2
      *
     C                   Z-ADD     9.64          VTCE3
     C                   Z-ADD     0.96          HZCE3
      *
     C                   Z-ADD     9.76          VTCE4
     C                   Z-ADD     0.96          HZCE4
      *
     C                   Z-ADD     9.88          VTCE5
     C                   Z-ADD     0.96          HZCE5
      *
     C                   Z-ADD     10.00         VTCE6
     C                   Z-ADD     0.96          HZCE6
      *
     C                   Z-ADD     10.12         VTCE7
     C                   Z-ADD     0.96          HZCE7
      *
     C                   ENDSR                                                  CERTNC
      *
      ***** END ADD ******* 04/23/10 **********************************
      /Free
       Begsr Cert1005;
         *in31 = *on;
         VTCEOV = .1;
         HZCEOV = 0;
         VTDATE = 0.47;
         HZDATE = 7.1;
         VTAGNM = 1.75;
         HZAGNM = 0.10;
         VTMAD1 = 1.94;
         HZMAD1 = 0.10;
         VTCSZ  = 2.06;
         HZCSZ  = 0.10;
         VTFNNM = 2.32;
         HZFNNM = 4.66;
         VTNAIC = 2.34 ;
         HZNAIC = 7.26 ;
         VTINS1 = 2.63 ;
         HZINS1 = 0.10 ;
         VTINS2 = 2.75 ;
         HZINS2 = 0.10 ;
         VTINS3 = 2.87 ;
         HZINS3 = 0.10 ;
         VTINS4 = 2.99 ;
         HZINS4 = 0.10 ;
         VTINS5 = 2.91;
         HZINS5 = 0.40;
         VTINS6 = 3.03;
         HZINS6 = 0.40;
         VTCER# = 3.30;
         HZCER# = 3.50;
         VTREV# = 3.30;
         HZREV# = 6.90;
         VTPRX  = 6.65;
         HZPRX  = 5.72;
         VTPRA  = 7.00;
         HZPRA  = 0.06;
         VTYORN =  6.90;
         HZYORN =  1.95;
         VTCAP# =  7.02;
         HZCAP# =  2.90;
         VTFRDT =  7.02;
         HZFRDT =  4.35;
         VTENDT =  7.02;
         HZENDT =  5.00;
         VTACC  =  6.80;
         HZACC  =  7.32;
         VTDIS  =  6.98;
         HZDIS  =  7.32;
         VTAGG  =   7.15;
         HZAGG  =   7.32;
         VTDS01 =   8.04;
         HZDS01 =   0.40;
         VTDS02 =   8.35;
         HZDS02 =   0.40;
         VTDS03 =   8.26;
         HZDS03 =   0.40;
         VTDS04 =   8.37;
         HZDS04 =   0.40;
         VTCE1  =   9.20;
         HZCE1  =   0.08;
         VTCE2  =   9.32;
         HZCE2  =   0.08;
         VTCE3  =   9.44;
         HZCE3  =   0.08;
         VTCE4  =   9.56;
         HZCE4  =   0.08;
         VTCE5  =   9.68;
         HZCE5  =   0.08;
         VTCE6  =   09.80;
         HZCE6  =   0.08;
         VTCE7  =   09.92;
         HZCE7  =   0.08;
       Endsr;
      /End-free
      *==============================================================
      * DARTS  - Create Resolved DARTS output.
      *==============================================================
     C     DARTS         BEGSR
      *
     C                   If        isUseSpoolOut
     C                   MOVE      'N'           XXWRT
     C                   CLOSE     W7511O1
     C                   SETOFF                                       72
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #1FOLD           80
     C                   PARM      *BLANKS       #1DCID           12
     C                   PARM      'CI'          #1PRE             2
     C                   PARM      QQCO#         #1CO#             3
     C                   PARM      QQFND         #1FND             3
     C                   PARM      *BLANKS       #1RTCD            1
      *
     C                   CALL      'W7511C1'
     C                   PARM                    #1FOLD           80
     C                   PARM                    #1DCID           12
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C                   MOVEL(P)  QCKEY         #2KEY            40
      *
      *
      * If a DARTS remark was entered then, use it,otherwise default to
      * 'CERTS' for the remark to be used.
      *
     C                   MOVEL(P)  'CERTS'       #2RMCD            5
     C     Q1PRMS        IFGE      11
     C     QQRMCD        ANDNE     *BLANKS
     C                   MOVEL(P)  QQRMCD        #2RMCD
     C                   ENDIF
      *
     C                   SELECT
     C     XXCER#        WHENEQ    XXCERT
     C     'Cert'        CAT(P)    'Holder':1    #2COMM          256
     C                   CAT(P)    '#':1         #2COMM          256
     C                   MOVE      SVCER#        XXTMP2            5
     C                   CAT(P)    XXTMP2:1      #2COMM          256
     C                   CAT(P)    'Yr:':1       #2COMM          256
     C                   MOVEL(P)  SVFYR         XXTMP2
     C                   CAT(P)    XXTMP2:1      #2COMM          256
     C                   CAT(P)    SVNAME:1      #2COMM          256
     C                   OTHER
     C     'Cert'        CAT(P)    'Holder':1    #2COMM          256
     C                   CAT(P)    'Print':1     #2COMM          256
     C                   ENDSL
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         #2CDAT            8
      *
     C                   MOVEL(P)  #1DCID        #2DCID
     C                   MOVEL(P)  'W0050R'      #2PNAM           10
     C                   MOVEL(P)  #1FOLD        #2FOLD
      *
     C                   CALL      'DA700R'
     C                   PARM                    #2KEY
     C                   PARM                    #2COMM
     C                   PARM                    #2CDAT
     C                   PARM                    #2DCID           12
     C                   PARM                    #2PNAM
     C                   PARM      *ZEROS        #2SECL            3
     C                   PARM      Q1USER        #2USER           10
     C                   PARM      Q1PGM         #2PGM            10
     C                   PARM                    #2FOLD           80
     C                   PARM      'N'           #2DIFL            1
     C                   PARM      Q1PGM         #2IPGM           10
     C                   PARM      *BLANK        #2FL01            1
     C                   PARM      *BLANK        #2FL02            1
     C                   PARM                    #2RMCD
     C                   PARM      '00000'       #2SEQ#            5
     C                   PARM      'N'           #2SBYL            1
     C                   PARM      *BLANKS       #2COM2           19
     C                   PARM      *BLANKS       #2APID            2
     C                   PARM      *BLANKS       #2IDOC           12
     C                   PARM      *BLANKS       #2IQUE           10
     C                   PARM      'O'           #2IMGF            1
     C                   PARM      *BLANKS       #2IPTH           10
     C                   PARM      *ZEROS        #2PIDX            5
     C                   PARM      *BLANK        #2QPTY            1
     C                   PARM      *BLANKS       #2DCTP            5
     C                   PARM      *BLANKS       #2CSID            2
     C                   PARM      *BLANKS       #2CODE            2
      *
     C                   Endif
     C                   ENDSR                                                  DARTS
      *
      *
      *
      *==============================================================
      * WRTCE  - Write Certificate Holder
      *==============================================================
     C     WRTCE         BEGSR
     C                   Z-ADD     1             J4                3 0
     C                   CLEAR                   CE
      *
     C     XXFDNM        IFNE      *BLANKS
     C     CE3PTY        ANDNE     'Y'
     C     FNPEOF        ANDNE     *BLANK
     C                   MOVEL(P)  XXFDNM        CE(J4)
     C                   ADD       1             J4
      *
     C                   ELSE
      *
     C     CENAME        IFNE      *BLANKS
     C                   MOVEL(P)  CENAME        CE(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     XXDBA         IFNE      *BLANKS
     C     CE3PTY        ANDNE     'Y'
     C     FNPEOF        ANDNE     *BLANK
     C                   MOVEL(P)  XXDBA         CE(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     CEATTN        IFNE      *BLANKS
     C     'ATTN:'       CAT(P)    CEATTN:1      CE(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     CEADR1        IFNE      *BLANKS
     C                   MOVEL(P)  CEADR1        CE(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     CEADR2        IFNE      *BLANKS
     C                   MOVEL(P)  CEADR2        CE(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     CECTY         IFNE      *BLANKS
     C                   MOVEL(P)  CECTY         CE(J4)
     C                   CAT(P)    ',':0         CE(J4)
     C                   CAT(P)    CEST:1        CE(J4)
     C                   MOVEL     CEZIP         XXZIP5
     C                   CAT(P)    XXZIP5:1      CE(J4)
     C                   MOVE      CEZIP         XXZIP4
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANKS
     C                   CAT(P)    '-':0         CE(J4)
     C                   CAT(P)    XXZIP4:0      CE(J4)
     C                   ENDIF
      *
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     CEPROV        IFNE      *BLANKS
     C                   MOVEL(P)  CEPROV        CE(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     CECTRY        IFNE      *BLANKS
     C                   MOVEL(P)  CECTRY        CE(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C                   MOVEL(P)  CE(1)         XXCE1
     C                   MOVEL(P)  CE(2)         XXCE2
     C                   MOVEL(P)  CE(3)         XXCE3
     C                   MOVEL(P)  CE(4)         XXCE4
     C                   MOVEL(P)  CE(5)         XXCE5
     C                   MOVEL(P)  CE(6)         XXCE6
     C                   MOVEL(P)  CE(7)         XXCE7
     C                   Z-ADD     CECER#        X1CER#
     C                   Z-ADD     CEREV#        X1REV#
      *
     C                   If        isUseSpoolOut
     C                   WRITE     CNUM
     C   72              WRITE     CNUM1
      *
     C                   WRITE     DAY
     C                   WRITE     DET
     C   72              WRITE     DAY1
     C   72              WRITE     DET1
     C                   Endif
      *
     C                   ENDSR                                                  WRTCE
      *==============================================================
      * VERBAG - Print Remarks
      *==============================================================
     C     VERBAG        BEGSR
     C*
     C                   MOVEL(P)  EMNAME        XXENM            50
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        XXENM
     C*
     C* CHECK FOR FEDERAL NAMES
     C*
     C     VPEOF         IFNE      *BLANKS
     C                   MOVE      *ON           *IN21
     C                   ELSE
     C                   MOVE      *OFF          *IN21
     C                   ENDIF
     C*
     C     VFDNM         IFNE      *BLANKS
     C                   MOVE      *ON           *IN22
     C                   ELSE
     C                   MOVE      *OFF          *IN22
     C                   ENDIF
     C*
     C* LOAD ELEMENTS FOR PEO
     C*
     C     ##CLT#        IFNE      '133'
     C     *IN20         IFEQ      *ON
     C                   MOVEL(P)  VB(1)         V1(1)
     C   21              MOVEL(P)  VPEOF         V1(2)
     C   21              MOVEL(P)  'dba'         V1(3)
     C                   MOVEL(P)  VPEO          V1(4)
     C                   MOVEL(P)  VB(2)         V1(5)
     C   22              MOVEL(P)  VFDNM         V1(6)
     C   22              MOVEL(P)  'dba'         V1(7)
     C                   MOVEL(P)  VEMNM         V1(8)
     C                   MOVEL(P)  '.'           V1(9)
     C                   MOVEL(P)  VB(3)         V1(10)
     C   21              MOVEL(P)  VPEOF         V1(11)
     C   21              MOVEL(P)  'dba'         V1(12)
     C                   MOVEL(P)  VPEO          V1(13)
     C                   MOVEL(P)  'to'          V1(14)
     C   22              MOVEL(P)  VFDNM         V1(15)
     C   22              MOVEL(P)  'dba'         V1(16)
     C                   MOVEL(P)  VEMNM         V1(17)
     C                   MOVEL(P)  VB(4)         V1(18)
     C                   MOVEL(P)  X8FRD2        V1(19)
     C     ##CLT#        IFEQ      '135'
     C     XXCX          ANDNE     *ZEROS
     C                   MOVEL(P)  VD(1)         V1(20)
     C                   MOVE      '.'           XXCXA            11
     C                   MOVEL     X8FRX2        XXCXA
     C     VD(1)         CAT(P)    XXCXA:1       V1(20)
     C                   MOVEL     V1(20)        XSTR             28
     C                   MOVE      *BLANKS       BSTR             28
     C                   ENDIF
     C*
     C* NON PEO
     C                   ELSE
     C                   MOVEL(P)  VB(1)         V1(1)
     C                   MOVEL(P)  XXENM         V1(2)
     C                   MOVEL(P)  VB(3)         V1(3)
     C                   MOVEL(P)  XXENM         V1(4)
     C                   MOVEL(P)  VB(4)         V1(5)
     C                   MOVEL(P)  X8FRD2        V1(6)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CLEAR                   RMK
     C                   Z-ADD     1             D                 3 0
     C                   Z-ADD     1             L                 3 0
     C*
     C                   DO        30
     C*
     C                   MOVEL(P)  RMK(L)        FLD1            112
     C                   MOVEL(P)  V1(D)         FLD2            112
     C                   EXSR      CONVRT
     C*
     C     XCAT          IFEQ      'N'
     C                   ADD       1             L
     C*
     C                   ELSE
     C                   MOVEL(P)  FLDM          RMK(L)
     C                   ADD       1             D
     C                   ENDIF
     C*
     C     L             IFGE      15
     C                   LEAVE
     C                   ENDIF
     C     D             IFGE      21
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ADD       2             L
     C*
     C                   ENDSR
      *==============================================================
      * CONVRT -
      *==============================================================
     C     CONVRT        BEGSR
     C*
     C                   CLEAR                   FLDM
     C                   Z-ADD     112           FLGTH             3 0
     C*
     C     ' '           CHECKR    FLD1          LGT1              3 0
     C     ' '           CHECKR    FLD2          LGT2              3 0
     C*
     C     FLGTH         SUB       LGT1          AVAIL             3 0          M-AVAILBL
     C*
     C     LGT2          IFLT      AVAIL
     C     LGT1          IFNE      0
     C     FLD2          ANDNE     V1(9)
     C     FLD1          CAT       FLD2:1        FLDM            112
     C                   ELSE
     C     FLD1          CAT       FLD2:0        FLDM
     C                   ENDIF
     C                   MOVE      'Y'           XCAT              1
     C                   ELSE
     C                   MOVE      'N'           XCAT
     C                   ENDIF
     C*
     C                   ENDSR
      *
      *==============================================================
      * BAKFIL - Backfill address line in case of overflow
      *==============================================================
     C     BAKFIL        BEGSR
      *
     C     *IN20         IFEQ      *OFF
      *
      *
      * *IN20 = *ON
     C                   ELSE
     C                   MOVEL(P)  XPCSZ         IN(6)
      *
     C     XPADD2        IFNE      *BLANKS
     C                   MOVEL(P)  XPADD2        IN(5)
     C                   MOVEL(P)  XPADD1        IN(4)
     C                   ELSE
     C                   MOVEL(P)  XPADD1        IN(5)
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      *==============================================================
      * REMARK - Print Remarks
      *==============================================================
     C     REMARK        BEGSR
     C*
     C     L             IFGT      *ZEROS
     C                   Z-ADD     L             J3
     C                   ELSE
     C*
     C                   Z-ADD     1             J3                3 0
     C*
     C                   ENDIF
     c*
     C     ##CLT#        IFEQ      '133'
     C                   ADD       2             J3
     C                   ENDIF
      *
     C     *IN42         IFNE      '1'
      *
     C     CEREM1        IFNE      *BLANKS
     C     CEREM2        ORNE      *BLANKS
      *
     C     J3            IFLE      14
     C                   MOVEL(P)  CEREM1        RMK(J3)
     C                   MOVEL     CEREM2        XXTMP1           52
     C                   MOVE      XXTMP1        RMK(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C                   ENDIF                                                  CEREM1
      *
     C     CEREM3        IFNE      *BLANKS
     C     CEREM4        ORNE      *BLANKS
      *
     C     J3            IFLE      14
     C                   MOVEL(P)  CEREM3        RMK(J3)
     C                   MOVEL     CEREM4        XXTMP1           52
     C                   MOVE      XXTMP1        RMK(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C                   ENDIF                                                  CEREM2
      *
     C     CEREM5        IFNE      *BLANKS
     C     J3            ANDLE     14
     C                   MOVEL(P)  CEREM5        RMK(J3)
     C                   ADD       1             J3
     C                   ENDIF                                                  CEREM5
      *
      *
     C     XXSAN         IFNE      *BLANKS
     C     J3            ANDLE     14
     C                   MOVEL(P)  XXSAN         RMK(J3)
     C                   ADD       1             J3
     C                   ENDIF
      *
     C     CEREM1        IFNE      *BLANKS
     C     CEREM2        ORNE      *BLANKS
     C     CEREM3        ORNE      *BLANKS
     C     CEREM4        ORNE      *BLANKS
     C     CEREM5        ORNE      *BLANKS
     C     XXSAN         ORNE      *BLANKS
     C                   ADD       1             J3
     C                   ENDIF                                                  REM1 2
      *
     C                   MOVEL(P)  RMK(1)        XXDS01
     C                   MOVEL(P)  RMK(2)        XXDS02
     C                   MOVEL(P)  RMK(3)        XXDS03
     C                   MOVEL(P)  RMK(4)        XXDS04
     C                   MOVEL(P)  RMK(5)        XXDS05
     C                   MOVEL(P)  RMK(6)        XXDS06
     C                   MOVEL(P)  RMK(7)        XXDS07
     C                   MOVEL(P)  RMK(8)        XXDS08
     C                   MOVEL(P)  RMK(9)        XXDS09
     C                   MOVEL(P)  RMK(10)       XXDS10
     C                   MOVEL(P)  RMK(11)       XXDS11
     C                   MOVEL(P)  RMK(12)       XXDS12
     C                   MOVEL(P)  RMK(13)       XXDS13
     C                   MOVEL(P)  RMK(14)       XXDS14
      *
     C     ##CLT#        IFEQ      '135'
     C     XXCX          ANDNE     *ZEROS
     C                   EXSR      BOLD
     C                   ENDIF
     C*
     C                   If        isUseSpoolOut
     C                   WRITE     REM                                          REMARKS
     C   72              WRITE     REM1                                         REMARKS
     C                   Endif
     C                   ENDIF
      *
     C                   ENDSR                                                  ADDINS
      *
     C*
     C*================================================================
     C*   SUBROUTINE FOR PEO PROCESSING
     C*================================================================
     C*
     C     SRPEO         BEGSR
     C*
     C     XXEMP#        IFNE      1
     C     XXDIV         ORNE      0
     C*
     C                   MOVE      XXEMP#        XSEMP#
     C                   MOVE      XXDIV         XSDIV
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     FNPEOF        ANDNE     *BLANKS
     C*
     C                   Z-ADD     1             XXEMP#
     C                   Z-ADD     0             XXDIV
     C*
     C     EMKEY1        CHAIN     PEM01                              60
     C*
     C* ASSEMBLE NAMES.
     C*                                   VPEO
     C                   MOVEL     EMNAME        XXPEO1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        XXPEO1           50
     C                   MOVE      EMNAM2        XXPEO2           50
     C*
     C                   MOVEL(P)  XXPEO1        QQPEO1           57
     C                   MOVEL(P)  XXPEO2        QQPEO2           57
     C                   MOVEL(P)  XXPEO1        Q2PEO1           57
     C*
     C     QQPEO1        IFNE      *BLANKS
     C                   MOVEL(P)  QQPEO1        VPEO
     C*
     C     QQPEO2        IFNE      *BLANKS
     C     VPEO          CAT(P)    QQPEO2:1      VPEO            105
     C                   ENDIF
     C                   ENDIF
     C*
     C     ' '           CHECKR    EMNAM2        LEN               3 0
     C                   SELECT
     C     LEN           WHENGT    10
     C                   CAT       PEOEXT:1      QQPEO2
     C                   OTHER
     C                   CAT       PEOEXT:1      QQPEO1
     C                   ENDSL
     C*
     C     EMMPFL        IFEQ      'P'
      *
     C                   MOVEL(P)  EMPAD1        XPADD1           55
     C                   MOVEL(P)  EMPAD2        XPADD2           55
      *
     C     EMPCTY        IFNE      *BLANKS
     C                   MOVEL(P)  EMPCTY        XPCSZ            55
     C                   CAT(P)    ',':0         XPCSZ
     C                   CAT(P)    EMPST:1       XPCSZ
     C                   MOVEL     EMPZIP        XXZIP5
     C                   CAT(P)    XXZIP5:1      XPCSZ
     C                   MOVE      EMPZIP        XXZIP4
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANKS
     C                   CAT(P)    '-':0         XPCSZ
     C                   CAT(P)    XXZIP4:0      XPCSZ
     C                   ENDIF
      *
     C                   ENDIF
     C                   ELSE
      *
     C                   MOVEL(P)  EMMAD1        XPADD1
     C                   MOVEL(P)  EMMAD2        XPADD2
      *
     C     EMMCTY        IFNE      *BLANKS
     C                   MOVEL(P)  EMMCTY        XPCSZ
     C                   CAT(P)    ',':0         XPCSZ
     C                   CAT(P)    EMMST:1       XPCSZ
     C                   MOVEL     EMMZIP        XXZIP5
     C                   CAT(P)    XXZIP5:1      XPCSZ
     C                   MOVE      EMMZIP        XXZIP4
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANKS
     C                   CAT(P)    '-':0         XPCSZ
     C                   CAT(P)    XXZIP4:0      XPCSZ
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL(P)  EMFDNM        VPEOF           105
     C                   MOVEL(P)  EMFDN2        XXNM10
     C                   MOVE(P)   EMFDN2        XXNM50
     C     VPEOF         CAT(P)    XXNM10:0      VPEOF
     C     VPEOF         CAT(P)    XXNM50:1      VPEOF
     C                   ENDIF
      *
     C*
     C*** OVERRIDE EFF DATES W/PEO'S
     C*
     C     ELKEY         CHAIN     PEL01                              51
     C*
     C     FNPEOF        IFEQ      'S'
     C     FNpeoF        OrEQ      'C'
     C     ELemp#        AndEQ     1
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C                   ENDIF
     C*
     C* RESET FILE POINTERS
     C*
     C                   MOVE      XSEMP#        XXEMP#
     C                   MOVE      XSDIV         XXDIV
     C*
     C     EMKEY1        CHAIN     PEM01                              60
     C     ELKEY         CHAIN     PEL01                              51
     C*
     C                   ELSE
     C                   MOVEL     EMNAME        XXPEO1
     C                   MOVEL(P)  EMNAM2        XXNM10
     C                   MOVE      XXNM10        XXPEO1
     C                   MOVEL(P)  XXPEO1        Q2PEO1
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL     EMNAME        XXPEO1
     C                   MOVEL(P)  EMNAM2        XXNM10
     C                   MOVE      XXNM10        XXPEO1
     C                   MOVEL(P)  XXPEO1        Q2PEO1
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   SUBROUTINE FOR EMPLOYER INFORMATION AND PRINT CERTIFICATE
     C*================================================================
     C     SUB1          BEGSR
      *
     C     XXRSFL        IFEQ      'Y'
     C     isUseSpoolOut ANDEQ     *on
     C  N72              OPEN      W7511O1
     C                   SETON                                        72
     C                   ENDIF
      *
      * Determine offsets and Print Overlay / Signature
      *
     C                   EXSR      WRTFRM
      *
     C                   Z-ADD     1             J4                3 0
     C                   CLEAR                   IN
     C                   CLEAR                   V1
     C                   CLEAR                   VPEO
     C                   CLEAR                   VPEOF
     C                   CLEAR                   VEMNM
     C                   CLEAR                   VFDNM
     C*
     C                   MOVE      *BLANKS       QQPEO1
     C*
     C                   EXSR      SRPEO
     C*
     C     QQPEO1        IFNE      *BLANKS
     C                   MOVEL(P)  QQPEO1        IN(J4)
     C*
     C     QQPEO2        IFNE      *BLANKS
     C     IN(J4)        CAT(P)    QQPEO2:1      IN(J4)
     C                   ENDIF
     C*
     C     EMCHAP        IFNE      *ZERO
     C     ##CLT#        IFEQ      '135'
     C     CPKY          CHAIN     PCP01                              60
     C                   MOVE      ' '           IN(J4)
     C                   MOVEL(P)  CPDESC        IN(J4)
     C                   CAT(P)    PEOEXT:1      IN(J4)
     C                   CLEAR                   VPEO
     C                   CLEAR                   VPEOF
     C                   MOVEL(P)  CPDESC        VPEO
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ADD       1             J4
     C                   MOVE      *ON           *IN20
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN20
     C                   ENDIF
      *
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL(P)  EMFDNM        IN(J4)
     C                   MOVEL(P)  EMFDNM        XXFDNM
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE(P)   EMFDN2        XXNM50           50
     C     XXFDNM        CAT(P)    XXNM10:0      XXFDNM
     C     XXFDNM        CAT(P)    XXNM50:1      XXFDNM
     C                   MOVEL(P)  XXFDNM        IN(J4)
     C                   MOVEL(P)  IN(J4)        VFDNM           105
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     EMNAME        IFNE      *BLANKS
     C                   MOVEL(P)  EMNAME        VEMNM           105
     C                   MOVEL(P)  EMNAM2        XXNM10
     C                   MOVE(P)   EMNAM2        XXNM50
     C     VEMNM         CAT(P)    XXNM10:0      VEMNM
     C     VEMNM         CAT(P)    XXNM50:1      VEMNM
      *
     C     *IN20         IFEQ      *OFF
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL(P)  'd/b/a'       IN(J4)
     C                   CAT(P)    EMNAME:1      IN(J4)
     C                   MOVEL(P)  EMNAM2        XXNM10
     C                   MOVE(P)   EMNAM2        XXNM50
     C     IN(J4)        CAT(P)    XXNM10:0      IN(J4)
     C     IN(J4)        CAT(P)    XXNM50:1      IN(J4)
     C                   MOVEL(P)  IN(J4)        XXDBA
     C                   MOVEL(P)  IN(J4)        VEMNM           105
     C                   ADD       1             J4
     C                   ELSE                                                   FDNM
     C                   MOVEL(P)  EMNAME        IN(J4)
     C                   MOVEL(P)  EMNAM2        XXNM10
     C                   MOVE(P)   EMNAM2        XXNM50
     C     IN(J4)        CAT(P)    XXNM10:0      IN(J4)
     C     IN(J4)        CAT(P)    XXNM50:1      IN(J4)
     C                   ADD       1             J4
     C                   ENDIF                                                  FDNM
      *
     C                   ELSE                                                   *IN20
     C     EMFDNM        IFEQ      *BLANKS
     C                   MOVEL(P)  EMNAME        IN(J4)
     C                   MOVEL(P)  EMNAM2        XXNM10
     C                   MOVE(P)   EMNAM2        XXNM50
     C     IN(J4)        CAT(P)    XXNM10:0      IN(J4)
     C     IN(J4)        CAT(P)    XXNM50:1      IN(J4)
      *
     C                   ADD       1             J4
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN20         IFEQ      *OFF
     C     EMMPFL        IFEQ      'P'
      *
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD1        IN(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     EMPAD2        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD2        IN(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     EMPCTY        IFNE      *BLANKS
     C                   MOVEL(P)  EMPCTY        IN(J4)
     C                   CAT(P)    ',':0         IN(J4)
     C                   CAT(P)    EMPST:1       IN(J4)
     C                   MOVEL     EMPZIP        XXZIP5
     C                   CAT(P)    XXZIP5:1      IN(J4)
     C                   MOVE      EMPZIP        XXZIP4
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANKS
     C                   CAT(P)    '-':0         IN(J4)
     C                   CAT(P)    XXZIP4:0      IN(J4)
     C                   ENDIF
      *
     C                   ENDIF
     C                   ELSE
      *
     C     EMMAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMMAD1        IN(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     EMMAD2        IFNE      *BLANKS
     C                   MOVEL(P)  EMMAD2        IN(J4)
     C                   ADD       1             J4
     C                   ENDIF
      *
     C     EMMCTY        IFNE      *BLANKS
     C                   MOVEL(P)  EMMCTY        IN(J4)
     C                   CAT(P)    ',':0         IN(J4)
     C                   CAT(P)    EMMST:1       IN(J4)
     C                   MOVEL     EMMZIP        XXZIP5
     C                   CAT(P)    XXZIP5:1      IN(J4)
     C                   MOVE      EMMZIP        XXZIP4
      *
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANKS
     C                   CAT(P)    '-':0         IN(J4)
     C                   CAT(P)    XXZIP4:0      IN(J4)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  XPADD1        IN(J4)
     C                   ADD       1             J4
     C     XPADD2        IFNE      *BLANKS
     C                   MOVEL(P)  XPADD2        IN(J4)
     C                   ADD       1             J4
     C                   ENDIF
     C     J4            IFEQ      7
     C                   EXSR      BAKFIL
     C                   ELSE
     C                   MOVEL(P)  XPCSZ         IN(J4)
     C                   ENDIF
     C                   ENDIF
      *
      *
     C                   MOVEL(P)  IN(1)         XXINS1
     C                   MOVEL(P)  IN(2)         XXINS2
     C                   MOVEL(P)  IN(3)         XXINS3
     C                   MOVEL(P)  IN(4)         XXINS4
     C                   MOVEL(P)  IN(5)         XXINS5
     C                   MOVEL(P)  IN(6)         XXINS6
     C*
     C     *LIKE         DEFINE    *IN60         SVIN60
     C                   MOVE      *IN60         SVIN60
     C     ' '           CHECKR    XXINS1        LEN1              3 0  60
     C     ' '           CHECKR    XXINS2        LEN2              3 0  60
     C     ' '           CHECKR    XXINS3        LEN3              3 0  60
     C     ' '           CHECKR    XXINS4        LEN4              3 0  60
     C     ' '           CHECKR    XXINS5        LEN5              3 0  60
     C     ' '           CHECKR    XXINS6        LEN6              3 0  60
     C                   MOVE      SVIN60        *IN60
     C*
     C                   If        isUseSpoolOut
     C                   SELECT
     C     LEN1          WHENGT    LONG
     C     LEN2          ORGT      LONG
     C     LEN3          ORGT      LONG
     C     LEN4          ORGT      LONG
     C     LEN5          ORGT      LONG
     C     LEN6          ORGT      LONG
     C                   WRITE     INSDL
     C   72              WRITE     INSDL1
     C                   OTHER
     C                   WRITE     INSD
     C   72              WRITE     INSD1
     C                   ENDSL
     C                   Endif
      *
     C*
      * REMOVE THE DELETES FROM 03/14/03
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXSTA
     C*
     C     ELILCD        IFEQ      *BLANK
     C                   Z-ADD     0010101       XXILEF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C     RRACC         MULT      1000          XXACC
     C     RRDIS         MULT      1000          XXDIS
     C     RRAGG         MULT      1000          XXAGG
     C                   ELSE
      *
      * Use 2nd Increased limits if entered and effective as of current
      * date
      *
     C                   MOVE      'N'           XX2LM             1
      *
     C     ELINL2        IFNE      *BLANK
     C     ELI2EF        ANDLE     CURDAT
     C                   MOVEL(P)  ELINL2        XXKEY
     C                   Z-ADD     ELI2EF        XXILEF
      *
     C     LMKY1         SETGT     PLM02
     C     LMKY2         READPE    PLM02                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     XXILEF        ANDGE     LMEFDT
     C     1000          MULT      LMACC         XXACC
     C     1000          MULT      LMDIS         XXDIS
     C     1000          MULT      LMAGG         XXAGG
     C                   MOVE      'Y'           XX2LM
     C                   ENDIF
      *
     C                   ENDIF                                                  ELINL2
      *
     C     XX2LM         IFEQ      'N'
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     ELILCD        XXKEY
     C*
     C* IF INCR LIMIT CODE ENTERED & NO DATE, THEN IT IS EFFECTIVE AS
     C* OF THE POLICY START DATE
     C*
     C     ELILEF        IFEQ      *ZERO
     C                   Z-ADD     ELPFDT        XXILEF
     C                   ELSE
     C                   Z-ADD     ELILEF        XXILEF
     C                   END
      *
     C     LMKY1         SETGT     PLM02
     C                   READP     PLM02                                  60
     C     *IN60         IFNE      '0'
     C     XXCO#         ORNE      LMCO#
     C     XXFND         ORNE      LMFND
     C     XXSTA         ORNE      LMST
     C     XXKEY         ORNE      LMKEY
     C     XXILEF        ORLT      LMEFDT
     C                   Z-ADD     0             XXACC
     C                   Z-ADD     0             XXDIS
     C                   Z-ADD     0             XXAGG
     C                   ELSE
     C     LMACC         MULT      1000          XXACC
     C     LMDIS         MULT      1000          XXDIS
     C     LMAGG         MULT      1000          XXAGG
     C                   END
      *
     C                   ENDIF                                                  XX2LM
      *
     C                   END                                                    ENDIF ELILCD
     C*
     C*  RETRIEVE OWNER/OFFICER FILE AND DETERMINE IF THE OO'S ARE
     C*  ACTIVE AND IF THEY ARE INCLUDED, BASED ON OOEXFL. CLIENT
     C*  SPECIFIC TO 102
     C*
     C     ##CLT#        IFEQ      '102'
     C                   Z-ADD     0             OOTOT             3 0          Tot active OO's
     C                   Z-ADD     0             OOICT             3 0          Cnt inc OO's
     C*
     C     OOKEY         CHAIN     POO01                              70
     C     *IN70         DOWEQ     '0'
     C     OOEXDT        IFLT      DSDATE
     C     OOEXFL        IFEQ      'I'
     C                   ADD       1             OOICT
     C                   ENDIF                                                  OOEXFL = 'I'
     C                   ADD       1             OOTOT
     C                   ENDIF                                                  OOEXD GT DSDATE
     C*
     C     OOKEY         READE     POO01                                  70
     C                   ENDDO                                                  *IN70 = 0
     C*
     C*  DETERMINE IF ALL, SOME OR NONE OF THE OO'S ARE INCLUDED
     C*
     C                   MOVE      *BLANKS       XXSAN            40
     C     OOTOT         IFNE      0
     C     OOICT         ANDNE     0
     C     OOTOT         IFEQ      OOICT
     C                   MOVEL     'All'         XXSAN
     C                   ELSE
     C                   MOVEL     'Some'        XXSAN
     C                   ENDIF                                                  OOTOT = OOICT
     C                   ELSE
     C                   MOVEL     'No  '        XXSAN
     C                   ENDIF                                                  OOTOT NE 0
      *
     C     XXSAN         CAT       VC(1):1       XXSAN
     C                   ENDIF                                                  ##CLT#NE 0
      *
      *  Print Carrier Policy Number, if entered.
      *
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL(P)  RPCAP#        XXCAP#           18
     C                   ELSE
      *
     C                   MOVEL     *BLANKS       XXCAP#
     C                   MOVEL(P)  XXFND         XXCAP#
     C                   MOVEL(P)  XXEMP#        XXTMP3            9
     C                   CAT(P)    XXTMP3:0      XXCAP#
     C                   MOVEL(P)  XXFYR         XXTMP3
     C                   CAT(P)    XXTMP3:0      XXCAP#
     C                   ENDIF
      *
     C     ##CLT#        IFEQ      '126'
     C                   MOVEL(P)  EMCOM1        XXCOM1
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       XXCOM1
     C                   ENDIF
      *
     C     ##CLT#        IFEQ      '133'
     C     ##CLT#        OREQ      '102'
     C                   MOVE      *BLANKS       XXYORN
     C                   ELSE
     C**** If OFEXBL is 'Y', the Excluded flag was left blanks for this
     C**** certificate, so a blank should be passed to the XXYORN field.
     C     OFEXBL        IFEQ      'Y'
     C                   MOVE      '   '         XXYORN
     C                   ELSE
     C   77              MOVE      'YES'         XXYORN
     C  N77              MOVE      'NO '         XXYORN
     C                   ENDIF                                                  OFEXBL IFEQ 'Y'
     C                   ENDIF
      *
      /free
       If xxCeOv = 'CERTNC' or xxCeOv = 'CERT0909' or
        xxCeOv = 'CERT1005';
      /end-free
     C**** If OFEXBL is 'Y', the Excluded flag was left blanks for this
     C**** certificate, so a blank should be passed to the XXYORN field.
     C     OFEXBL        IFEQ      'Y'
     C                   MOVE      '   '         XXYORN
     C                   ELSE
      /free
       If xxCeOv = 'CERT1005';
          If *in77 = *on;
             xxYorN= ' Y ';
          Else;
             xxYorN= ' N ';
          Endif;
       Else;
      /end-free
     C   77              MOVEL(P)  'Y'           XXYORN
     C  N77              MOVE(P)   'N'           XXYORN
      /free
       Endif;
      /end-free
     C                   ENDIF                                                  OFEXBL IFEQ 'Y'
     C                   ENDIF
      *
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1FRDT            8 0
      *
     C                   Z-ADD     XXENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X1ENDT            8 0
      *
     C                   If        isUseSpoolOut
     C                   WRITE     BODY                                         COVERAGE &DATES
     C   72              WRITE     BODY1                                        COVERAGE &DATES
     C                   Endif
      *
      * Print REMARKS
      *
     C                   CLEAR                   RMK
      *
      * Don't print Verbage if FNPEOF is blank.
     C     FNPEOF        IFNE      *BLANK
     C                   EXSR      VERBAG
     C                   ENDIF                                                  FNPEOF <> BLANK
     C*
     C                   EXSR      REMARK
     C                   MOVE      'Y'           XXWRT
     C*
     C                   ENDSR                                                  SUB1
      *
     C*===============================================================*
     C*  CURAGT - SUBROUTINE TO FIND MOST CURRENT ACTIVE AGT ON ACCOUNT
     C*===============================================================*
     C     CURAGT        BEGSR
      *
     C                   CLEAR                   AGNAME
     C                   CLEAR                   AGMAD1
     C                   CLEAR                   AGCSZ
      *
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         ##CO#             3 0
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C                   PARM      *ZERO         ##AGT#            5 0
     C                   PARM      *BLANKS       ##ANAM           60
      *
     C     ##AGT#        IFNE      *ZERO
     C*
     C*  CHAIN TO AGENT ASSIGNED BY FUND FILE TO GET FEIN/MOD
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C     AGKY          CHAIN     PAG01                              50
     C     *IN50         IFEQ      '0'
     C*
     C*   COMBINE AGENT CITY STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP5                                     8
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     AGMCTY        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVEL     AGMZIP        XXZIP5            5                        8
     C                   MOVE      AGMZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     XXZIP5        XXST10                                     8
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C*  CONCATENATE CITY, STATE AND ZIP
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        AGCSZ            40
     C                   END
     C                   END
     C*
     C                   END
     C                   ENDSR
     C     BOLD          BEGSR
     C                   MOVEL     VD(1)         FLDX             16
     C*
     C     FLDX          SCAN      XXDS01:1      POS               5 0
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS02:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS01        XBDS01
     C                   MOVE      '1'           *IN85
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS01:POS    XBDS02
     C     XSTR:BSTR     XLATE     XXDS01:POS    XXDS01
     C                   MOVE      '1'           *IN86
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS03:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS02        XBDS02
     C                   MOVE      '1'           *IN86
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS02:POS    XBDS03
     C     XSTR:BSTR     XLATE     XXDS02:POS    XXDS02
     C                   MOVE      '1'           *IN87
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS04:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS03        XBDS03
     C                   MOVE      '1'           *IN87
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS03:POS    XBDS04
     C     XSTR:BSTR     XLATE     XXDS03:POS    XXDS03
     C                   MOVE      '1'           *IN88
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS05:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS04        XBDS04
     C                   MOVE      '1'           *IN88
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS04:POS    XBDS05
     C     XSTR:BSTR     XLATE     XXDS04:POS    XXDS04
     C                   MOVE      '1'           *IN89
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS06:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS05        XBDS05
     C                   MOVE      '1'           *IN89
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS05:POS    XBDS06
     C     XSTR:BSTR     XLATE     XXDS05:POS    XXDS05
     C                   MOVE      '1'           *IN90
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS07:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS06        XBDS06
     C                   MOVE      '1'           *IN90
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS06:POS    XBDS07
     C     XSTR:BSTR     XLATE     XXDS06:POS    XXDS06
     C                   MOVE      '1'           *IN91
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS08:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS07        XBDS07
     C                   MOVE      '1'           *IN91
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS07:POS    XBDS08
     C     XSTR:BSTR     XLATE     XXDS07:POS    XXDS07
     C                   MOVE      '1'           *IN92
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS09:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS08        XBDS08
     C                   MOVE      '1'           *IN92
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS08:POS    XBDS09
     C     XSTR:BSTR     XLATE     XXDS08:POS    XXDS08
     C                   MOVE      '1'           *IN93
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS10:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS09        XBDS09
     C                   MOVE      '1'           *IN93
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS09:POS    XBDS10
     C     XSTR:BSTR     XLATE     XXDS09:POS    XXDS09
     C                   MOVE      '1'           *IN94
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS11:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS10        XBDS10
     C                   MOVE      '1'           *IN94
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS10:POS    XBDS11
     C     XSTR:BSTR     XLATE     XXDS10:POS    XXDS10
     C                   MOVE      '1'           *IN95
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS12:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS11        XBDS11
     C                   MOVE      '1'           *IN95
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS11:POS    XBDS12
     C     XSTR:BSTR     XLATE     XXDS11:POS    XXDS11
     C                   MOVE      '1'           *IN96
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS13:1      POS
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS12        XBDS12
     C                   MOVE      '1'           *IN96
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS12:POS    XBDS13
     C     XSTR:BSTR     XLATE     XXDS12:POS    XXDS12
     C                   MOVE      '1'           *IN97
     C                   GOTO      END
     C                   ENDIF
     C                   ENDIF
     C*
     C     POS           IFEQ      0
     C     FLDX          SCAN      XXDS14:1      POS
     C     POS           IFNE      0
     C                   MOVEL     XXDS14        XBDS14
     C                   MOVE      '1'           *IN98
     C                   ENDIF
     C                   ELSE
     C     POS           IFEQ      1
     C                   MOVEL     XXDS13        XBDS13
     C                   MOVE      '1'           *IN97
     C                   GOTO      END
     C                   ELSE
     C     28            SUBST     XXDS13:POS    XBDS14
     C     XSTR:BSTR     XLATE     XXDS13:POS    XXDS13
     C                   MOVE      '1'           *IN98
     C                   ENDIF
     C                   ENDIF
     C*
     C     END           TAG
     C                   ENDSR
      /free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       If not isUseXML;
          Leavesr;
       Endif;

       //* xmlFile - contains the name of the template XML file, used for work file linking.
       //* xmlFlowName - contains the name of the flow XML file, used for S4401C call.
       //* xmlLink - used to link individual records together in a mass run - XML flow file name.

       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';

       //* Request Master record for mass run.
       If isMassReq;

          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
           'CertofInsr':'000':'000':massFlag:xmlLink);

          isMassReq = *off;
          //* Flow XML file name is used for the S4401C call.
          xmlFlowName = xmlFile;
          xmlLink = xmlFile;

          Chain (xmlFile) xp100;
          XPCat = 'PS';
          Update xp100;

       Endif;

       //* If processing multiple documents, combine certs into one if same
       //* employer/division.
       If xxEmp# <> svEmp# or xxDiv <> svDiv;
          svEmp# = xxEmp#;
          svDiv  = xxDiv;

       //* Request record for individual Certificate of Insurance.
       StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x@RRN:'X'):
        xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

       //* Flow XML file name is used for the S4401C call.
       If xmlFlowName = ' ';
          xmlFlowName = xmlFile;
       Endif;

       //* The template XML file name must be pulled from record for work files for a
       //* single request.  Otherwise it has been returned from the XML Request call.
       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       XPData = 'Y';

       //* Set the template from the additional setup file.

       Setgt (xxCO#:xxFND:RSJrSt:ELPfdt) pci00;
       Readpe (xxCO#:xxFND:RSJrSt) pci00;
       If not %eof;
          If CIDocC <> ' ';
             XPDcid = %trim(CIDocC);
             XPPath = %trim(CIPathC);
          Else;
             If CIDoc <> ' ';
                XPDcid = %trim(CIDoc);
                XPPath = %trim(CIPath);
             Endif;
          Endif;
       Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pci00;
          Readpe (xxCO#:xxFnd:'  ') pci00;
          If not %eof;
             If CIDocC <> ' ';
                XPDcid = %trim(CIDocC);
                XPPath = %trim(CIPathC);
             Else;
                If CIDoc <> ' ';
                   XPDcid = %trim(CIDoc);
                   XPPath = %trim(CIPath);
                Endif;
             Endif;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pci00;
             Readpe (xxCO#:000:RSJrst) pci00;
             If not %eof;
                If CIDocC <> ' ';
                   XPDcid = %trim(CIDocC);
                   XPPath = %trim(CIPathC);
                Else;
                   If CIDoc <> ' ';
                      XPDcid = %trim(CIDoc);
                      XPPath = %trim(CIPath);
                   Endif;
                Endif;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pci00;
                Readpe (xxCO#:000:'  ') pci00;
                If not %eof;
                   If CIDocC <> ' ';
                      XPDcid = %trim(CIDocC);
                      XPPath = %trim(CIPathC);
                   Else;
                      If CIDoc <> ' ';
                         XPDcid = %trim(CIDoc);
                         XPPath = %trim(CIPath);
                      Endif;
                   Endif;
                Endif;
             Endif;
          Endif;
       Endif;

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DSTime;
       XPDt   = sysDt;

       Update xp100;
       Else;

          xmlFile = XPFile;

          StLetter('TEMPLRQST':xmlFile:message:'E':%editc(x@RRN:'X'):
           xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'));

          Chain (xmlFile) xt100;

          XTDcid = XPDcid;
          XTPath = XPPath;

          Update xt100;

       Endif;

       //* Write data for late notice work file.
       Clear pch01p;

       CHXML  = xmlFile;

       CHCo#  = xxCo#;
       CHFnd  = xxFnd;
       CHEmp# = xxEmp#;
       CHDiv  = xxDiv;
       CHFyr  = xxFyr;
       CHCer# = CECer#;

       CHBlank = QQBlnk;

       CHCNam = CEName;
       CHStat = CEStat;
       CHCtry = CECtry;
       CHProv = CEProv;
       CHAdr1 = CEAdr1;
       CHAdr2 = CEAdr2;
       CHCity = CECty;
       CHSt   = CESt;
       CHZip  = CEZip;
       CHRev# = CERev#;
       CHEOFL = CEEOFL;
       CH3Pty = CE3Pty;
       CHRLFL = CERLFL;
       CHType = CEType;
       CHAttn = CEAttn;
       CHRem1 = CERem1;
       CHRem2 = CERem2;
       CHRem3 = CERem3;
       CHRem4 = CERem4;
       CHRem5 = CERem5;
       CHWVFL = CEWVFL;
       CHCTG1 = CECTG1;
       Select;
       When xxYorn = 'YES' or xxYorn = ' Y ' or xxYorn = 'Y';
          CHYORN = 'Y';
       When xxYorn = 'NO' or xxYorn = ' N ' or xxYorn = 'N';
          CHYORN = 'N';
       Endsl;

       xxKey = CHCtry;
       Chain(n) (xxCO#:'CTRY':xxKey) pgt00;
       If %found;
          CHCtryD = GTDesc;
       Endif;

       CHForNam1 = QQName1;
       CHForNam2 = QQName2;
       CHForNam3 = QQName3;
       CHForNam4 = QQName4;
       CHForAdr1 = %trim(amn(##adrl));
       CHForAdr2 = %trim(amn(##adrl+1));
       CHForAdr3 = %trim(amn(##adrl+2));

       CHPgm  = Q1PGM;
       CHUser = Q1USER;
       CHTime = DSTime;
       CHDt   = sysDt;

       Write pch01p;

       Endsr;

      /end-free
      *
     P*====================================================================
     P* Determine if the object exists
     P*====================================================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
     C*** end add *****  03/08/17  *****************************************
** VB - VERBAGE
Workers' compensation coverage is provided by contract to all employees of
assigned to
Coverage does not apply to any employees not approved and assigned by
effective
** VC - VERBAGE FOR 102
owner/officers covered.
** VD - VERBAGE FOR 135
AND CANCELLED ON
