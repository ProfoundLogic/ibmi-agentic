     hCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H/define WEB
     H**********************************************************************
     H/TITLE W3029RPRT - Reprint and Attach Commission Statements - Submit *
     H**********************************************************************
     H*  Called From: Menu                                                 *
     H*  Calls To:    W3029CPRT                                            *
     H**********************************************************************
     H* Created on 10/31/2025 by c:62 for Ticket 119676(146)               *
     H**********************************************************************
     H* Revisions:                                                         *
     H*                                                                    *
     H**********************************************************************
     FW3029F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWZD2P     UF A E             DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDB)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMLSL1    IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSTSPL     if   e           k disk    rename(psp01:psp00)
     Fxmlprel4  if a e           k disk    rename(xp100:xp104)

     d/copy sPrLetter
     d/copy sPrXML

     d isUseScript     S              1n   inz
     d processType     c                   const('Commission Statements')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1

     D LDA             DS          1024    DTAARA(*LDA)
     d ldPgmNm                       10    overlay(lda:1)
     D xxDart                         1    overlay(lda:251)
     d xxRMCD                         5    overlay(lda:252)
     d xxCopy                         1    overlay(lda:257)
     d xxPrint                        1    overlay(lda:258)
     d xxPrinter                     10    overlay(lda:259)
     d xxTray                        50    overlay(lda:269)
     d xx#CopiesA                     3    overlay(lda:319)
     d xxEmail                        1    overlay(lda:322)
     d xxToEmail                    200    overlay(lda:323)
     d xxSubject                    150    overlay(lda:523)
     d xxBody                       350    overlay(lda:673)
     d xxLDATag                      10    overlay(lda:251)
     d xxLDAKey                      10    overlay(lda:261)


     D MG              S             60    DIM(4) CTDATA PERRCD(1)
     D*
     D OPTNM           C                   CONST('Commission Check -
     D                                     Process')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DLYA                   1     80
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDB           DS
     D  DBRRB                397    400B 0

       Dcl-s isUseSpool ind inz;
       Dcl-s isDartsRequired ind inz;
       Dcl-S isCSVmsg Char(1);
       Dcl-S chrSuffix Char(3);
       Dcl-Pr RtvSuffix ExtPgm;
        suffix Char(3);
       End-Pr;

     C*
     C     *DTAARA       DEFINE    WCOMCHK       DLYA
     C*
     C     *LIKE         DEFINE    FNDT          XXDSDT
     C     *LIKE         DEFINE    FNDT          XXPRDT
     C     *LIKE         DEFINE    LSFRDT        XXDAT1
     C     *LIKE         DEFINE    LSTODT        XXDAT2
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     c                   Parm                    inStmtPgm        10
     C*
     C                   SELECT
     C     Q1PRMS        WHENGE    1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        QQPRDS           40
     C                   OTHER
     C                   MOVEL(P)  OPTNM         QQPRDS           40
     C                   ENDSL
     C                   Eval      isUseScript = UseScriptura(processType)
     C*
      /free
       In LDA;
       Clear LDA;

       If q1Prms > 1 and inStmtPgm <> ' ';
          ldPgmNm =  inStmtPgm;
       Else;
          ldPgmNm =  'W7421C';
       Endif;

       Out LDA;
      /end-free
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
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C     R2KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C     LSKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDSDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD           80
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      COCO#         SCCO#
     C*
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPFYR         SCFYR
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVE      FNFND         SCFND
     C     LSKY          SETGT     PLS01
     C     LSKY          READPE    PLS01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     LSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXACSR        SCACSR
     C                   ENDIF
     C*
     C*   CHECK TO SEE WHETHER THE DAILY PROCESS IS ALREADY EXECUTING
     C*
     C     *LOCK         IN        DLYA                                 80
     C     *IN80         IFEQ      '1'
     C                   EXFMT     SCR02
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C*   GET THE DAILY BATCH RECORD
      /FREE
          CHAIN 1 WZD2P;
          IF %FOUND(WZD2P);
      /END-FREE
      *
     C*
     C                   Z-ADD     D2SDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   MOVE      *BLANK        SCCURD
     C*
     C*
     C     D2TAG         IFNE      *BLANK
     C                   MOVE      '1'           *IN90
     C                   MOVE      D2USR         SCUSR
     C                   MOVE      D2JOB         SCJOB
     C                   MOVE      D2SDT         SCSDT
     C                   MOVE      D2STM         SCSTM
     C                   MOVE      D2TAG         SCTAG
     C                   MOVE      D2CO#         SCRCO
     C                   MOVE      D2FND         SCRFN
     C                   END
     C*
     C                   ELSE
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C*
     C                   END
     C*
       isUseSpool = UseSpoolOut(processType);
       isCSVmsg = 'Y';
       isVisDarts = not isUseScript;
       Clear SPallowD ;
       xxProcType = processType;
       Chain xxProcType psp00;
       If SPallowD <> 'Y';
          isDartsRequired = *on;
       Endif;
     C                   MOVE      *OFF          NORECS            1
     C                   CLEAR                   SCMSG1
     C                   CLEAR                   SCMSG2
     C*
     C                   MOVEL(P)  SCMSG1        SRDFLD           80
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCMSG1
     C*
     C                   MOVEL(P)  SCMSG2        SRDFLD           80
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCMSG2
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C     *INKJ         IFEQ      *ON
     C*
      ***********************
      *  GROUP YEAR SEARCH  *
      ***********************
     C     HFLD          IFEQ      'SCFYR'
     C*
     C     SCFND         IFNE      0
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     DBRRB         ##RRN
     C                   ENDIF                                                  SCFND EQ 0
     C*
     C                   CALL      'W0011R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##YR              3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT                     22     PC @ SCFYR
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ##YR          SCFYR
     C                   MOVE      *ON           *IN23                          PC @ DATE
     C                   ELSE
     C                   MOVE      *ON           *IN22                          PC @ SCFYR
     C                   ENDIF                                                  *IN60
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFYR
     C*
     C     HFLD          IFEQ      'SCACSR'
     C                   MOVE      *BLANKS       QQRMCD            5
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVE      QQRMCD        SCACSR
     C                   ENDIF
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVE      *ON           *IN24                          EM WC40004
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ
     C*
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN09         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C                   SELECT
     C     NORECS        WHENEQ    *ON
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDSL
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK FOR A VALID DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCURD        IFNE      *BLANKS
     C     SCCURD        ANDNE     'Y'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*   CHECK FOR A VALID FROM & TO DATE
     C*
     C     SCFRDT        IFNE      *ZEROS
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        32  31
     C                   GOTO      STRT
     C                   END
     C                   END                                                    SCFRDT
     C     SCTODT        IFNE      *ZEROS
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        34  33
     C                   GOTO      STRT
     C                   END
     C                   END                                                    SCTODT
     C*  DATES MUST BE ASCENDING ORDER
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C     XXDAT2        IFLT      XXDAT1
     C                   SETON                                        41
     C                   GOTO      STRT
     C                   ENDIF
     C* If dates are entered  then DARTS remark must be entered
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ANDNE     *ZEROS
     C     isVisDarts    andeq     '1'
     C*
     C     SCACSR        IFEQ      *BLANKS
     C                   SETON                                        38
     C                   GOTO      STRT
     C                   ELSE
     C*If not blank then check validity
     C     SCACSR        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        36
     C                   GOTO      STRT
      *Active status of Rmk code
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        37
     C                   GOTO      STRT
     C                   ENDIF                                                  CTSTAT=I
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                  SCACSR <> ' '
     C                   ENDIF                                                  SCFRDT & SCTODT
     C*
       // We need a DARTS value to pass to Scriptura
       If isDartsRequired and isUseScript and scAcSr = *blanks;
          msFrDt = 'Remark code must be established in Admin/DARTS ' +
           'for this group.';
          erFrDt = *on;
     C                   GOTO      STRT
       Endif;
     C*
     C  N90SCACSR        IFNE      *BLANKS
     C*
     C     SCFRDT        IFEQ      *ZEROS
     C     SCTODT        OREQ      *ZEROS
     C                   SETON                                        39
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFRDT & SCTODT
     C                   ENDIF                                                  SCACSR <> ' '
     C*Move dates to variables for CL Parameters
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCACSR        QQACSR            5
     C*
     C*  SEND A VERIFICATION MESSAGE IF ENTERED DATE DOES NOT EQUAL
     C*  CURRENT SYSTEM DATE AND WE ARE NOT RE-SUBMITTING AN ABNORMAL
     C*  TERMINATION
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT
     C*
     C     SCMSG1        IFEQ      *BLANKS
     C     XXPRDT        ANDNE     XXDSDT
     C     *IN90         ANDNE     '1'
     C                   MOVEL     MG(1)         SCMSG1
     C                   MOVEL     MG(2)         SCMSG2
     C                   GOTO      STRT
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
     C                   END
       RtvSuffix(chrSuffix);
       If EXacsrCsv = 'Y' and isCSVmsg = 'Y' and scCsvMsg = *blanks;
          scCsvMsg = 'This selected group is set to include CSV with DARTS. ' +
           'This will update output file WFACP in library PDSRPT' + chrSuffix +
           '.';
     C                   Goto      Strt
       Endif;
      /Free
          xxSubmit = *on;

          If isUseScript;

             xxProcType = processType;

             xxMass = ' ';

             In *Lock *dtaara;

             If scACSR = ' ';
                xxDart = 'N';
                xxRMCD = ' ';
             Else;
                xxDart = 'Y';
                xxRMCD = scACSR;
             Endif;
           // Tell Output Window to require DARTS
             If scFrDt <> *zeros or scToDt <> *zeros;
                xxDart = 'R';
             Endif;

             Out *dtaara;

             xxSubmit = OutputWindow(xxProcType:xxMass);

           // Get remark code from Output Window
             In LDA;
             Select;
             When EXacsrCsv <> 'Y' and not isUseSpool;
                Clear scAcSr;
             When xxLDATag = '%key%';
                Clear XPrmCd;
                Chain xxLDAkey xmlprel4;
                scAcSr = XPrmCd;
             Other;
                scAcSr = xxRmCd;
             Endsl;
             qqAcSr = scAcSr;
             Out LDA;

          Endif;
      /End-Free
     C                   If        xxSubmit = *off
     C                   Goto      STRT
     C                   Endif
     C*
     C*  WE HAVE PASSED ALL EDITS, WRITE THE RECORD
     C*
     C                   MOVE      SCDATE        D2SDT
     C                   MOVE      DSTIME        D2STM
     C                   MOVE      Q1USER        D2USR
     C                   MOVE      Q1JOB         D2JOB
     C                   MOVE      COCO#         D2CO#
     C                   MOVE      FNFND         D2FND
     C     *IN90         IFEQ      '1'
     C                   MOVE      'Y'           D2RTRT
     C                   MOVE      SCRCO         D2CO#
     C                   MOVE      SCRFN         D2FND
     C                   END
     C*
      /FREE
            IF %FOUND(WZD2P);
            UPDATE PD201;
            ELSE;
            WRITE PD201;
            ENDIF;
      /END-FREE
     C*
     C*  SUBMIT THE JOB
     C*
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W3029CPRT'
     C                   PARM                    QQFYR
     C                   PARM                    QQOUTQ
     C                   PARM      SCCURD        QQCURD            1
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQACSR
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE MG
      Date entered is not equal to current system date.
    Change date if necessary and press ENTER to continue.
No records for this group available for processing.
Use 'Chg user parms' to select a different group.
