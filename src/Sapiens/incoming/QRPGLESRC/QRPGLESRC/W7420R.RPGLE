     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W7420R - AGENT COMMISION STATEMENT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/02/22  C:33  Support: 101993                              *
     H*                 -Do not allow email for mass runs             *
     H*                 -Do not allow DARTS for mass runs             *
     H*                 -Get DARTS code from Output Window            *
     H*                                                               *
     H*  03/04/22  C:300 Support: 101283 (UBIC)                       *
     H*                  Error in Agent Commission Statement Report   *
     H*                  In DSPF -- In From Date Range field, changed *
     H*                  3rd error messge from MC40004 (FSMESG MSGF)  *
     H*                  to WCR1058 (WMESGG MSGF)                     *
     H*                  Becasue FSMESG MSGF not available in UBIC    *
     H*                                                               *
     H*  07/09/20  C:65  PROJECT: WC2073                              *
     H*                  Implement Scriptura for agent commission     *
     H*                  statements.  Add binding directory above.    *
     H*                                                               *
     H*  03/28/13  C:132 PROJECT: WA102 81962                         *
     H*                  Initialize field.                            *
     H*                                                               *
     H*  07/29/04  C:132 PROJECT: WA129  43181                        *
     H*                  Correct F10 search on Employer/div to load   *
     H*                  Employer number and division in screen fields*
     H*                                                               *
     H*  05/11/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*  04/19/00  C:118 PROJECT: WC577                               *
     H*                  Add output choice of P/F/B.                  *
     H*                                                               *
     H*  09/08/99  C:61  PROJECT WC577                                *
     H*                  Correct agent search.                        *
     H*                                                               *
     H*  02/01/99  C:33  SEQ#: WA031/16356                            *
     H*                  CHANGE DATE RANGE VALIDATION.                *
     H*                                                               *
     H*  06/15/98 C: 90  PROJECT: WC577                               *
     H*                  CHANGED SCREENS TO CURRENT STANDARDS         *
     H*                                                               *
     H*  05/21/98 C: 91  PROJECT: WC577                               *
     H*                  ADDED EMP/DIV SEARCH (W0050R)                *
     H*                  CHANGED SCREENS TO CURRENT STANDARDS         *
     H*                                                               *
     H*  PLEASE ADD TO TOP OF REVISIONS                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   28  EMPLOYER RECORD NOT FOUND                               *
     H*   30  POSITION CURSOR AT AGENT                                *
     H*   31  POSITION CURSOR AT EMPLOYER                             *
     H*   32  POSITION CURSOR AT DATE                                 *
     H*   33  POSITION CURSOR AT ZERO BALANCE                         *
     H*   34  POSITION CURSOR AT AGENT                                *
     H*   35  POSITION CURSOR AT EMPLOYER/DIVISION                    *
     H*   36  MC40004 - INVALID KEY FOR SEARCH                        *
     H*   38  OUTPUT TYPE MUST BE P/F/B
     H*                                                               *
     H*****************************************************************
      *
      *
     FW7420F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)
     FWMEML     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F***  Begin Add  ***  06/02/22  ***************
     FSTSPL     if   e           k disk    rename(psp01:psp00)
     Fxmlprel4  if a e           k disk    rename(xp100:xp104)
     F***  End   Add  ***  06/02/22  ***************
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Commission Statements')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d***  Begin Add  ***  06/02/22  ***************
     d xxLDATag                      10    overlay(svlda:251)
     d xxLDAKey                      10    overlay(svlda:261)
     d***  End   Add  ***  06/02/22  ***************

     D OPTNM           C                   CONST('Agent Commission Sta-
     D                                     tement')
     D OPTSEL          C                   CONST(', if selected.')
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
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
     C*  AGENT ASSIGNMNET FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCAGT#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     *LIKE         DEFINE    QQPRDS        XXPRDS
     C     *LIKE         DEFINE    QQCALL        XXCALL
     C     *LIKE         DEFINE    FNDT          X7FDT
     C     *LIKE         DEFINE    FNDT          X7TDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C                   CLEAR                   XXPRDS
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE(P)   QQPRDS        XXPRDS
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      QQCALL        XXCALL
     C                   ENDIF                                                  Q1PRMS>=2
     C                   Eval      isUseScript = UseScriptura(processType)
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ELSE
     C                   MOVE      UPFND         SCFND
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      *ZEROS        SCAGT#
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      Q1USER        SCUSER
     C*
     C     XXPRDS        IFEQ      *BLANKS
     C                   MOVEL(P)  OPTNM         XXPRDS
     C                   ENDIF                                                  XXPRDS=*BLANKS
     C*
     C                   MOVEL(P)  XXPRDS        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     STRT          TAG
     C*
     C* Convert screen dates to MM/DD/YYYY.
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   FUNCTION 3 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*   FUNCTION 10 = AGENT NUMERIC SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
      ********************
      * AGENT # SEARCH   *
      ********************
     C*
     C     HFLD          IFEQ      'SCAGT#'
      *
     C                   CALL      'W0040R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNF        ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
      *
     C     ##RRN         CABEQ     *ZERO         STRT                     34
      *
     C     ##RRN         CHAIN     PAA02                              60
     C                   MOVE(P)   AAAGT#        SCAGT#
     C                   MOVE      *ON           *IN35                          PC @ EMP/DIV
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCAGT#
     C*
      *****************************
      * EMPLOYER/DIVISION SEARCH  *
      *****************************
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN2            9 0
     C                   PARM      DBRRNF        ##RRN3            9 0
     C                   PARM                    ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN4            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCEMP#        OREQ      *ZERO
     C     SCDIV         OREQ      *ZERO
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C                   MOVE      *ON           *IN35
     C                   ELSE
     C                   MOVE      *ON           *IN37
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##RRN2        CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMEMP#        SCEMP#
     C                   MOVEL     EMDIV         SCDIV
     C                   ELSE
     C                   MOVE      *ON           *IN35                          PC @ EMP
     C                   ENDIF                                                  *IN60       O
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCEMP#/SCDIV
     C*
     C                   MOVE      *ON           *IN36                          ERROR MESS
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ
     C*
     C                   ENDIF                                                  *IN11
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
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
     C                   MOVE      UPFND         SCFND
     C*  GET THE GROUP RECORD, LOAD GROUP NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C                   GOTO      STRT01
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   VALIDATE THE ENTERED FROM DATE
     C*
     C                   Z-ADD     SCFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SPERR IFEQ *ON
     C*
     C*  VALIDATE THE ENTERED TO DATE
     C*
     C                   Z-ADD     SCTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SPERR IFEQ *ON
     C*
     C*  VALIDATE THE ENTERED DATES FOR ASCENDING ORDER
     C*
     C     X7TDT         IFLT      X7FDT
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF                                                  X7TDT<X7FDT
     C*
     C*  VALIDATE AGT# IF ENTERED
     C*
     C     SCAGT#        IFNE      0
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE EMPLOYER, IF ENTERED
     C*
     C     SCEMP#        IFNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE ZERO BALANCE
     C*
     c* If field is blank, change it to N
     c*
     C                   If        scZb = ' '
     C                   Eval      scZb = 'N'
     C                   EndIf
     C     SCZB          IFNE      'Y'
     C     SCZB          ANDNE     'N'
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE OUTPUT TYPE IS P/F/B
     C*
     C     SCTYPO        IFNE      'P'
     C     SCTYPO        ANDNE     'F'
     C     SCTYPO        ANDNE     'B'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTYPO<>P/F/
      /Free
        //***  Begin Add  ***  06/02/22  ***************
          Clear qqRmCd;
        //***  End   Add  ***  06/02/22  ***************
          xxSubmit = *on;

          If isUseScript;

             xxProcType = processType;

             xxMass = ' ';

             In *Lock *dtaara;

             Clear svLDA;

             xxDart = 'N';
             xxRMCD = ' ';

           //***  Begin Add  ***  06/02/22  ***************
             Select;
             When scAgt# = 0;
                xxEmail = '*';
                xxDart  = '*';
             Endsl;
           //***  End   Add  ***  06/02/22  ***************

             Out *dtaara;

             xxSubmit = OutputWindow(xxProcType:xxMass);

           //***  Begin Add  ***  06/02/22  ***************
           // Get remark code from Output Window
             In svLDA;
             Select;
             When xxLDATag = '%key%';
                Clear XPrmCd;
                Chain xxLDAkey xmlprel4;
                qqRmCd = XPrmCd;
             Other;
                qqRmCd = xxRmCd;
             Endsl;
             Out svLDA;
           //***  End   Add  ***  06/02/22  ***************

          Endif;
      /End-Free
     C                   If        xxSubmit = *off
     C                   Goto      STRT
     C                   Endif
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFDT         QQFDT             8
     C                   MOVE      SCTDT         QQTDT             8
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      SCAGT#        QQAGT#            5
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCZB          QQZB              1
     C                   CALL      'W7420C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQAGT#
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQZB
     C                   PARM      SCTYPO        QQTYPO            1
           //***  Begin Add  ***  06/02/22  ***************
     C                   PARM                    QQRMCD            5
           //***  End   Add  ***  06/02/22  ***************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
