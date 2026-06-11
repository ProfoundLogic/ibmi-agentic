     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H****************************************************************
     H/TITLE W8060R - PROMPT FOR MASS REVISED BILLS
     H*****************************************************************
     H*  PROGRAMMER: C:65                                             *
     H*  CREATED:    02/26/04                                         *
     H*  PROJECT#:   SUPPORT                                          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 02/29/24  C:291 Project: WC2670                               *
     H*                 Additional changes for Scriptuar Bills        *
     H*                                                               *
     H* 04/28/22  C:33  Project: WC2670
     H*                 Add output control window for Scriptura
     H*                                                               *
     H* 04/20/22  C:65  Project WC2869
     H*                 Add employer # to screen to run for single
     H*                 employer within a group.  Hide the mid-term
     H*                 endorsement fields for later update with WC2670.
     H*
     H* 07/27/07  C:190 Project WC1454
     H*                 Add new screen field to the prompt to
     H*                 allow mid-term endoresment to be gen-
     H*                 rated with the mass revised bills al-
     H*                 ong with endorsement comment fields.
     H*
     H*  10/11/06  C:33  Seq#: WA099/41801                           *
     H*                  Use SC305R instead of SC580R.               *
     H*                                                               *
     H*****************************************************************
      *
     FW8060F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     F*** begin add ***  04/20/22  **************************************
     FWMEML     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F*** end add *****  04/20/22  **************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)

     D*
     D OPTNM           C                   CONST('Mass Revised Bills')
     D***  Begin Add  ***  04/28/22  ****************
     D processType     C                   const('Revised Bill')
     D*
     DisUseScript      s              1n
     DisUseSpool       s              1n
     D*
     DsvLDA            ds          1024    dtaara(*LDA)
      //*** Beg Chg ***** 02/29/24 ********************************************
     D*** xxDart                         1    overlay(svlda:251)
     D*** xxRmCdLDA                      5    overlay(svlda:252)
     D*** xxLDAtag                      10    overlay(svlda:251)
     D*** xxLDAKey                      10    overlay(svlda:261)
     d ldEnd#                         5  0 overlay(svlda:1)
     d ldEndA                         5    overlay(svlda:1)
     d ldMYRR                         9  0 overlay(svlda:6)
     d ldEEDt                         8  0 overlay(svlda:15)
     d ldFyr                          3  0 overlay(svlda:23)
     d ldPyrl                         1    overlay(svlda:26)
     d keyEmail                      40    overlay(svLDA:56)
     d keySeq#                        5    overlay(svLDA:96)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d x1Copy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
      //*** End Chg ***** 02/29/24 ********************************************
     D***  End   Add  ***  04/28/22  ****************
      //*** Beg Add ***** 02/29/24 ********************************************
     d xxMass          s              1
      //*** End Add ***** 02/29/24 ********************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D***  Begin Add  ***  04/28/22  ****************
      /copy sPrLetter
      /copy sPrXml
     D***  End   Add  ***  04/28/22  ****************
      *****************************************************************

     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
      *
      *  GROUP DETAIL FILE
      *
     C     F1KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C*** begin add ***  04/20/22  **************************************
      *
      *  Employer Master File
      *
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      *
      *  Employer Detail File
      *
     C     ELKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C*** end add *****  04/20/22  **************************************
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
      *
      *****************************************************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  CHECK IF OFFICE IS INSTALLED.
     C*
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C     SYOFIF        ORNE      'Y'
     C                   MOVE      *ON           *IN61
     C                   MOVE      'N'           SCDART
     C                   ELSE
     C                   MOVE      *OFF          *IN61
     C                   MOVE      'Y'           SCDART
     C                   ENDIF                                                  *IN60 EQ 1
      *
     C***  Begin Add  ***  04/28/22  ********************
       isUseScript = UseScriptura(processType);
       isUseSpool = UseSpoolOut(processType);

       Select;
       When *in61;
          isVisDarts = *off;
       When isUseScript;
          isVisDarts = *off;
       Other;
          isVisDarts = *on;
       Endsl;
     C***  End   Add  ***  04/28/22  ********************
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
      *
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C     *IN61         ANDEQ     *OFF
     C     EXREV         ANDNE     *BLANKS
     C                   MOVE      EXREV         SCRMCD
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      'N'           SCDART
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        SCPRDS
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF IFEQ
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
      *   COMMAND 03 = END OF JOB
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKA IFEQ
     C*
     C*  CMD 10 - TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     HFLD          IFEQ      'SCBCM1'
     C     HFLD          OREQ      'SCBCM2'
     C     HFLD          OREQ      'SCBCM3'
     C     HFLD          OREQ      'SCICM1'
     C     HFLD          OREQ      'SCICM2'
     C     HFLD          OREQ      'SCICM3'
     C*
     C                   MOVEL(P)  *BLANKS       QQKEY
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY            10
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBCM1'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        51
     C                   ELSE
     C                   SETON                                        52
     C                   MOVEL     QQKEY         SCBCM1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCBCM2'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        52
     C                   ELSE
     C                   MOVEL     QQKEY         SCBCM2
     C                   SETON                                        53
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCBCM3'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        53
     C                   ELSE
     C                   MOVEL     QQKEY         SCBCM3
     C                   SETON                                        54
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCICM1'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        54
     C                   ELSE
     C                   SETON                                        55
     C                   MOVEL     QQKEY         SCICM1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCICM2'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        55
     C                   ELSE
     C                   MOVEL     QQKEY         SCICM2
     C                   SETON                                        56
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCICM3'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        56
     C                   ELSE
     C                   MOVEL     QQKEY         SCICM3
     C                   SETON                                        57
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C                   MOVE      *BLANKS       XXRMCD            5
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     XXRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     XXRMCD        IFNE      *BLANKS
     C                   MOVEL     XXRMCD        SCRMCD
     C                   ELSE
     C                   MOVE      *ON           *IN58
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ IFEQ
     C*
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      *ON
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
     C                   ENDIF                                                  *IN10 IFEQ
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      UPFND         SCFND
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM IFEQ
      *
     C                   ENDIF                                                  *IN11 IFEQ
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FYR
     C*
     C     F1KY          CHAIN     PF101                              39
     C*
     C     *IN39         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN39 IFEQ
      *
      * VALIDATE EFFECTIVE DATE
      *
     C*** begin add ***  04/20/22  **************************************
     C                   If        scEmp# = 0
     C*** end add *****  04/20/22  **************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     c*
     C                   Move      *off          *in26
     C                   Move      *off          *in27
     C*
     C* Check on employer information if entered.
     C*
     C                   If        scEmp# <> 0
     C*
     C* Verify Employer # is valid record.
     C     EMKY          Chain     PEM01                              27
     C                   If        *in27 = *on
     C                   Goto      STRT
     C                   Endif
     C*
     C* Verify Employer Policy Year is valid record.
     C     ELKY          Chain     PEL01                              26
     C                   If        *in26 = *on
     C                   Goto      STRT
     C                   Endif
     C*
     C                   Endif                                                  If scEmp# <> 0
     C*** end add *****  04/20/22  **************************************
     C*
     C*  VALIDATE THE BILLING COMMENTS
     C*
     C     SCBCM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBCM1        XXKEY
     C     TBKY          CHAIN     PBC01                              42
     C  N42BCSTAT        COMP      'I'                                    42
     C     *IN42         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCBCM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBCM2        XXKEY
     C     TBKY          CHAIN     PBC01                              43
     C  N43BCSTAT        COMP      'I'                                    43
     C     *IN43         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCBCM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBCM3        XXKEY
     C     TBKY          CHAIN     PBC01                              44
     C  N44BCSTAT        COMP      'I'                                    44
     C     *IN44         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE INSTALLMENT COMMENTS
     C*
     C     SCICM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCICM1        XXKEY
     C     TBKY          CHAIN     PBC01                              45
     C  N45BCSTAT        COMP      'I'                                    45
     C     *IN45         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCICM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCICM2        XXKEY
     C     TBKY          CHAIN     PBC01                              46
     C  N46BCSTAT        COMP      'I'                                    46
     C     *IN46         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCICM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCICM3        XXKEY
     C     TBKY          CHAIN     PBC01                              47
     C  N47BCSTAT        COMP      'I'                                    47
     C     *IN47         CABEQ     '1'           STRT
     C                   END
      *
     C     *IN61         IFEQ      *OFF
     C*
     C*  VALIDATE THE RESOLVE TO DARTS FLAG.
     C*
     C     SCDART        IFNE      'Y'
     C     SCDART        ANDNE     'N'
     C***  Begin Add  ***  04/28/22  ********************
     C     isVisDarts    andeq     *on
     C***  End   Add  ***  04/28/22  ********************
     C                   MOVE      *ON           *IN48
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDART NE Y
     C*
     C     SCDART        IFEQ      'Y'
     C***  Begin Add  ***  04/28/22  ********************
     C     isVisDarts    andeq     *on
     C***  End   Add  ***  04/28/22  ********************
     C     SCRMCD        CHAIN     PCT01                              49
     C*
     C     *IN49         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN50
     C                   GOTO      STRT
     C                   ENDIF                                                  CTSTAT IFEQ
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
     C                   ENDIF                                                  SCDART IFEQ
      *
     C                   ENDIF                                                  *IN61 IFEQ
      *
     C*  VALIDATE REVISED BILLING ENDROSMENT
     C     SCPEND        IFNE      'Y'
     C     SCPEND        ANDNE     ' '
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
     C*  VALIDATE MID TERM ENDROSMENT DESCRIPTION.
     C     SCPEND        IFEQ      'Y'
     C     SCDSC1        ANDEQ     ' '
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPEND = 'Y'

     C***  Begin Add  ***  04/28/22  ********************
     C     SCRIPT        TAG
       //*** Beg Chg ***** 02/29/24 ********************************
       // Process the Output Control Window for Scriptura
       //***If isUseScript;
          // Give the window default Dart values
          //***In svLDA;
          //***Clear svLDA;
          //***xxDart = scDart;
          //***xxRmCdLDA = EXrev;
          //***Out svLDA;
          //***If OutputWindow(processType:'Y') = *off;
     C****               GOTO      STRT
          //***Endif;
          // Update process from information from window
          //***In svLDA;
          //***  If xxLDATag = '%key%';
          //***     Exec sql
          //***      select XPdart, XPrmCd into :scDart, :scRmCd
          //***       from XMLPREP
          //***       where XPpgm = :xxLDAkey;
          //***  Else;
          //***     scDart = xxDart;
          //***     scRmCd = xxRmCdLDA;
          //***  Endif;
       //***Endif;
     C***  End   Add  ***  04/28/22  ********************
       If isUseScript;
          // Give the window default Dart values
          In svLDA;
          Clear svLDA;
          xxDart = 'N';
          xxRMCD = ' ';
          If EXrev <> ' ';
             xxDart = 'Y';
             xxRMCD = EXREV;
          Endif;
          Out svLDA;
          xxMass = 'Y';
          If OutputWindow(processType:xxMass) = *off;
     C                   GOTO      STRT
          Endif;
          // Update process from information from window
          In svLDA;
          scDart = xxDart;
          scRmCd = xxRMCD;
       //*** End Chg ***** 02/29/24 ********************************
       Endif;
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPOUTQ        QQQUE            10
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   Z-ADD     0             XXEMP#            9 0
     C                   MOVE      XXEMP#        QQEMP#            9
     C*** begin add ***  04/20/22  **************************************
     C                   MOVE      scEmp#        QQSEMP#           9
     C*** end add *****  04/20/22  **************************************
     C                   MOVE      SCEFDT        QQEFDT            8
     C                   MOVE      SCBCM1        QQBCM1            2
     C                   MOVE      SCBCM2        QQBCM2            2
     C                   MOVE      SCBCM3        QQBCM3            2
     C                   MOVE      SCICM1        QQICM1            2
     C                   MOVE      SCICM2        QQICM2            2
     C                   MOVE      SCICM3        QQICM3            2
     C                   MOVE      SCDART        QQDART            1
     C                   MOVE      SCRMCD        QQRMCD            5
     C                   MOVE      SCRMCD        QQRMCD            5
     C                   MOVE      SCPEND        QQPEND            1
     C                   MOVE(P)   SCDSC1        QQDSC1           40
     C                   MOVE(P)   SCDSC2        QQDSC2           40
     C                   MOVE(P)   SCDSC3        QQDSC3           40
     C                   MOVE(P)   '*'           QQDS1            41
     C                   MOVE(P)   '*'           QQDS2            41
     C                   MOVE(P)   '*'           QQDS3            41
     C                   MOVEL     QQDSC1        QQDS1            41
     C                   MOVEL     QQDSC2        QQDS2            41
     C                   MOVEL     QQDSC3        QQDS3            41
     C*
     C                   CALL      'W8060C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQEFDT
     C                   PARM                    QQQUE
     C                   PARM                    QQBCM1
     C                   PARM                    QQBCM2
     C                   PARM                    QQBCM3
     C                   PARM                    QQICM1
     C                   PARM                    QQICM2
     C                   PARM                    QQICM3
     C                   PARM                    QQDART
     C                   PARM                    QQRMCD
     C                   PARM                    QQPEND
     C                   PARM                    QQDS1
     C                   PARM                    QQDS2
     C                   PARM                    QQDS3
     C*** begin add ***  04/20/22  **************************************
     C                   PARM                    QQSEMP#
     C*** end add *****  04/20/22  **************************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
