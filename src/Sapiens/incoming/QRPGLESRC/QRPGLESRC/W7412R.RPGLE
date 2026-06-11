     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7412R - NCCI POLICY INFORMATION PAGE - PROMPT
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROMPTS THE USER TO ENTER THE FYR, AND     **
     H**     WHETHER THE REPORT IS A REISSUE.  IF CALLED FROM THE    **
     H**     MENU, THE EMPLOYER # IS ZERO AND PENDING POLICY         **
     H**     GENERATION WILL BE RUN FOR ALL EMPLOYERS.  IF CALLED    **
     H**     FROM W0050R, THE POLICY INFORMATION PAGE WILL PRINT     **
     H**     FOR THE REQUESTED EMPLOYER.                             **
     H**   CALLED FROM:                                              **
     H**     MENU                                                    **
     H**     W0050R - EMPLOYER MASTER MAINTENANCE                    **
     H**   CALLS TO:                                                 **
     H**     W7412C - NCCI POLICY INFORMATION PAGE SUBMITTER         **
     H**   PROGRAMMER: C:24                                          **
     H**   CREATED:    6/27/94      PROJECT: WC313                   **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/07/16 C:65  Project: SY371                                 *
     H*                Update positions on LDA to prevent overlap     *
     H*                with other processes which utilize it.         *
     H*                                                               *
     H* 09/30/16  C:65  PROJECT: SY371                                *
     H*                 Implement Scriptura output control window     *
     H*                 when process is flagged to generate Scriptura *
     H*                 output.  The settings are stored/passed via   *
     H*                 a local data area which is picked up by       *
     H*                 API's and the printing program.               *
     H*                                                               *
     H* 02/10/16 C:65  Support: Phw#39774 (LCTA)                      *
     H*                Consider 'N' codes for non-rating submissions  *
     H*                as resubmission transactions too.              *
     H*                                                               *
     H* 01/20/15  C:180  SUPPORT 32762                                *
     H*                  Change error indicator for 11/11/14 fix.     *
     H*
     H* 11/11/14  C:180  SUPPORT WA114 87252                          *
     H*                  Handle NCCI record lock                      *
     H*
     H* 06/20/11  C:189  PROJECT: 25WQK                               *
     H*                  -We need to get the submission history       *
     H*                   edit for TASB which was previuosly blocked  *
     H*                                                               *
     H* 03/12/10  C:62   PROJECT: 25WQK                               *
     H*                  -Change heading for TASB                     *
     H*                  -Add F13=Chg User Parms                      *
     H*                                                               *
     H* 05/11/09  C:61   PROJECT: 93WHW                               *
     H*                  Add new prompt for resubmissions due to      *
     H*                  interstate coverage change.                  *
     H*                                                               *
     H* 10/22/07  C:65   PROJECT: WC1633/114WAK                       *
     H*                  INTREPRET A 'T' SUBMISSION CODE THE SAME AS  *
     H*                  AN 'I' FOR REISSUES.                         *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                ADD INFORMATIONAL MESSAGE THAT CHECKS IF       *
     H*                POLICY WITH A VOLUNTARY LABOR CHARGE HAS A     *
     H*                'WC000311A' ENDORSEMENT ATTACHED.              *
     H*                                                               *
     H* 08/19/02  C:35   PROJECT: SUPPORT WA099 #33368                *
     H*                  TITLE INCORRECT WHEN CALLED FROM MENU.       *
     H*                                                               *
     H* 03/07/02  C:35   PROJECT: SUPPORT WA099                       *
     H*                  IF A RE-ISSUE IS ISSUED AGAIN BEFORE A       *
     H*                  SUBMISSION IS PROCESSED, IT SHOULD STILL     *
     H*                  SHOW AS A RE-ISSUE.                          *
     H*                                                               *
     H* 03/07/01  C:35   PROJECT: SUPPORT WA093 #26652                *
     H*                  REMOVE CODE THAT CHECKS IF A POLICY HAS BEEN *
     H*                  BILLED.  THIS CHECK IS DONE IN W0051R, BASED *
     H*                  ON ELFAF ONLY.                               *
     H*                                                               *
     H* 01/02/01  C:35   PROJECT: SUPPORT WA081 #25578                *
     H*                  ONLY INVOKE EDIT IF PROCESSING SINGLE PAGE   *
     H*                  REQUEST.                                     *
     H*                                                               *
     H* 12/15/00  C:35   PROJECT: WC577                               *
     H*                  ADD CODE TO ESTABLISH A SUBMISSION HISTORY   *
     H*                  AS A "KEY CHANGE" (SHCODE = 'K') IF POLICY   *
     H*                  EFFECTIVE DATE HAS CHANGED.                  *
     H*                                                               *
     H* 08/02/99  C:118  PROJECT: WC577
     H*                  V3 screen fixes in display file, deleted
     H*                  SCR99 and SCUSER.
     H*
     H* 05/25/99  C:35   PROJECT: SUPPORT WA096 #16679                *
     H*                  IF POLICY HAS ALREADY BEEN SUBMITTED, SET    *
     H*                  REISSUE FLAG.                                *
     H*                                                               *
     H* 02/13/98  C:65   PROJECT: V3                                  *
     H*                  CHANGE TITLE AND PROTECT FYR FIELD.          *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      22      Run for all employers.                         **
     H**      23      WCR0517 - Fund year invalid for fund.          **
     H**      24      WCR0694 - No employer detail for fund year.    **
     H**      25      WCR0694 - No employer detail for fund year.    **
     H**      60      General Usage (CHAIN)                          **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO                                        **
     H**    WMUPL (PUP01): USER                                      **
     H**    WMFNL (PFN01): CO/FND                                    **
     H**    WMEML (PEM01): CO/FND/EMP#/DIV                           **
     H**    WDELL (PEM01): CO/FND/EMP#/DIV/FYR                       **
     H**    WMSHL1 (PSH01): CO/FND/EMP#/FYR/TYPE                     **
     H*****************************************************************
     FW7412F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *** BEG ADD ***** 09/01/04 **************************************
     F                                     SFILE(SCR02S:RRN2)
      *** END ADD ***** 09/01/04 **************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
      *** BEG ADD ***** 09/01/04 **************************************
     F                                     RENAME(PEL01:PEL00)
      *** END ADD ***** 09/01/04 **************************************
     FWMSHL1    UF   E           K DISK
      *** BEG ADD ***** 09/01/04 **************************************
     FWMMUL     IF   E           K DISK
     FWDELL3    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWDELL1    IF   E           K DISK
      *** END ADD ***** 09/01/04 **************************************
      **** BEGIN DEL **** 03/07/01 *********************************************
      **** BEGIN ADD **** 11/21/95 ************************************
     F********** WMEPL   IF  E           K        DISK
      **** END   ADD **** 11/21/95 ************************************
      **** END   DEL **** 03/07/01 *********************************************
     F**** BEGIN ADD ***  11/21/96  ***********************************
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
     F*** END ADD *****  11/21/96  ************************************
      *** begin add ***  09/30/16  ************************************
     FWMEXL     IF   E           K DISK    rename(pex01:pex00)
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Policy Page')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     d*** begin chg ***  10/07/16  ********************************************
     D***** SVLDA           DS          1024    DTAARA(*LDA)
     D***** xxDart                         1
     d***** xxRMCD                         5
     d***** xxCopy                         1
     d***** xxPrint                        1
     d***** xxPrinter                     10
     d***** xxTray                        50
     d***** xx#CopiesA                     3
     d***** xxEmail                        1
     d***** xxToEmail                    250
     d***** xxSubject                    200
     d***** xxBody                       500
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
     d*** end chg *****  10/07/16  ********************************************
      *** end add *****  09/30/16  ************************************
     F*** BEGIN ADD ***  11/21/96  ************************************

     D*** BEGIN ADD ***  02/13/98  ************************************
     D TITLE           C                   CONST('           Submit Po-
     D                                     licy Page           ')
     D*** END ADD *****  02/13/98  ************************************
      **** BEGIN ADD **** 08/19/02 ************************************
     D TITLE2          C                   CONST('Generate Policy Page-
     D                                      Submission Requests')
     D*** BEGIN ADD *** 03/12/10 **************************************
     D TITLE3          C                   CONST('                CCS -
     D                                     Forms               ')
     D TITLE4          C                   CONST(' Generate CCS Forms -
     D                                     Submission Requests ')
     D*** END   ADD *** 03/12/10 **************************************
      **** BEGIN ADD **** 08/19/02 ************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
      *** BEG ADD ***** 09/01/04 ************************************
     D ENDCON          C                   CONST('WC000311A')
      *** END ADD ***** 09/01/04 ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                KFLD           UPCO#
     C                   KFLD                    X1CO#
     C*** END CHG *****  11/04/97  ************************************
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** BEGIN ADD ***  11/21/96  ************************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** END ADD *****  11/21/96  ************************************
     C*
     C*  KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                KFLD           UPCO#
     C                   KFLD                    X1CO#
     C*** END CHG *****  11/04/97  ************************************
     C                   KFLD                    SCFND
     C*
     C*  KEY LIST FOR FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                KFLD           UPCO#
     C                   KFLD                    X1CO#
     C*** END CHG *****  11/04/97  ************************************
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                KFLD           UPCO#
     C                   KFLD                    X1CO#
     C*** END CHG *****  11/04/97  ************************************
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C*
     C*  KEY LIST FOR EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                KFLD           UPCO#
     C                   KFLD                    X1CO#
     C*** END CHG *****  11/04/97  ************************************
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
      *** BEG ADD ***** 09/01/04 **************************************
     C*
     C*  KEY LIST FOR ASSESSABLE MUTUAL FORMS
     C*
     C     ELKY1         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C*
     C     ELKY3         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXDAT1
     C*
     C     SHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
     C                   KFLD                    XXTYPE            1
     C*
     C     MUKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    SHFYR
     C                   KFLD                    XXFORM
      *
     C     EMKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
      *** END ADD ***** 09/01/04 **************************************
     C*
     C*  KEY LIST FOR SUBMISSION HISTORY FILE
     C*
     C     SHKY          KLIST
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                KFLD           UPCO#
     C                   KFLD                    X1CO#
     C*** END CHG *****  11/04/97  ************************************
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCFYR
     C                   KFLD                    XXTYPE            1
      **** BEGIN DEL **** 03/07/01 *********************************************
      **** BEGIN ADD **** 11/21/95 ************************************
     C********** EPKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXFND
     C**********           KFLD           XXEMP#
     C**********           KFLD           XXDIV
     C**********           KFLD           XXFYR
      *****************************************************************
      **********
     C********** *LIKE     DEFN UPCO#     XXCO#
     C********** *LIKE     DEFN UPFND     XXFND
     C********** *LIKE     DEFN EPEMP#    XXEMP#
     C********** *LIKE     DEFN EPDIV     XXDIV
     C********** *LIKE     DEFN EPFYR     XXFYR
      **********
      **** END   ADD **** 11/21/95 ************************************
      **** END   DEL **** 03/07/01 *********************************************
      **** BEGIN ADD **** 05/03/96 ************************************
     C     *LIKE         DEFINE    ELPFDT        XXDAT1
     C     *LIKE         DEFINE    ELPFDT        XXDAT2
      **** END   ADD **** 05/03/96 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*********            PARM           QQEMP#  5
     C                   PARM                    QQEMP#            9
     C*** END CHG *****  11/04/97  ************************************
     C                   PARM                    QQRTC             1
     C*** BEGIN ADD ***  11/04/97  ************************************
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1FND             3 0
     C                   PARM                    Q1FYR             3 0
     C*
     C*** END ADD *****  11/04/97  ************************************
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
      ***** BEG ADD ******* 05/11/09 **********************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     CLTID         COMP      '093'                                  93
      ***** END ADD ******* 05/11/09 **********************************
     C*
     C     TOP           TAG
     C*
     C*** BEGIN ADD ***  11/21/96  ************************************
     C*  CHECK IF OFFICE IS INSTALLED.
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C     SYOFIF        ORNE      'Y'
     C                   MOVE      *OFF          *IN31
     C                   MOVE      'N'           SCDART
     C                   ELSE
     C                   MOVE      'Y'           SCDART
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  OFIF NE Y
     C*** END ADD *****  11/21/96  ************************************
      *** begin add ***  09/30/16  ***********************************
      /Free
       isUseScript = UseScriptura(processType);
       visScript = *off;
       If isUseScript or *in31 = *off;
          visScript = *on;
       Endif;
      /end-free
      *** end add *****  09/30/16  ***********************************
     C                   TIME                    DSDTTM
     C                   MOVE      'P'           XXTYPE
     C*
     C*  CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                Z-ADDUPFND     SCFND
      **** BEGIN CHG **** 03/07/01 *********************************************
     C********** *LIKE     DEFN EPEMP#    X1EMP#
     C********** *LIKE     DEFN EPCO#     X1CO#
     C********** *LIKE     DEFN EPFND     X1FND
     C********** *LIKE     DEFN EPFYR     X1FYR
     C     *LIKE         DEFINE    ELEMP#        X1EMP#
     C     *LIKE         DEFINE    UPCO#         X1CO#
     C     *LIKE         DEFINE    UPFND         X1FND
     C     *LIKE         DEFINE    UPFYR         X1FYR
      **** END   CHG **** 03/07/01 *********************************************
      **** BEG ADD ****** 09/01/04 ************************************
     C     *LIKE         DEFINE    MUFORM        XXFORM
      **** BEG ADD ****** 09/01/04 ************************************
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       CLTID             3
     C*** END   ADD *** 03/12/10 **************************************
      *
     C                   MOVE      QQEMP#        X1EMP#
     C     X1EMP#        IFEQ      *ZERO
     C                   Z-ADD     UPCO#         X1CO#
     C                   Z-ADD     UPFND         X1FND
     C                   Z-ADD     UPFYR         X1FYR
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   SETON                                        16
     C*** END   ADD *** 03/12/10 **************************************
     C                   ELSE
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   SETOFF                                       16
     C*** END   ADD *** 03/12/10 **************************************
     C                   Z-ADD     Q1CO#         X1CO#
     C                   Z-ADD     Q1FND         X1FND
     C                   Z-ADD     Q1FYR         X1FYR
     C                   ENDIF
     C                   Z-ADD     X1FND         SCFND
     C                   Z-ADD     X1FYR         SCFYR
     C*** END CHG *****  11/04/97  ************************************
     C*
     C*  CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*
     C*  CHAIN TO THE FUND MASTER FILE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C***** BEGIN DEL **** 08/02/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/02/99 ***********************************
     C*** BEGIN CHG ***  02/13/98  ************************************
     C*****                MOVE QQPRDS    SCPRDS
     C                   MOVEL(P)  TITLE         SCPRDS
     C*** BEGIN ADD *** 03/12/10 **************************************
     C     CLTID         IFEQ      '025'
     C                   MOVEL(P)  TITLE3        SCPRDS
     C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
     C*** END CHG *****  02/13/98  ************************************
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      *ZERO         SCDIV
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     0             SCFPFD
     C                   Z-ADD     0             SCTPFD
      **** END   ADD **** 05/03/96 ************************************
     C*
     C*  FIND THE REPORTING EMPLOYER MASTER RECORD
     C*
     C     SCEMP#        IFNE      *ZERO
     C                   MOVE      *OFF          *IN22
     C*
     C     EMKY          SETLL     PEM01
     C     EMKY          READE     PEM01                                  60
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVE      EMDIV         SCDIV
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      *ON           *IN60
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF
     C*
     C  N60EMKY          READE     PEM01                                  60
     C                   ENDDO
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCENAM
     C                   MOVE      *ON           *IN22
     C                   MOVE      *BLANKS       SCENAM
      **** BEGIN ADD **** 08/19/02 *********************************************
     C                   MOVEL(P)  TITLE2        SCPRDS
      **** END   ADD **** 08/19/02 *********************************************
     C*** BEGIN ADD *** 03/12/10 **************************************
     C     CLTID         IFEQ      '025'
     C                   MOVEL(P)  TITLE4        SCPRDS
     C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
     C                   ENDIF
     C*
     C                   MOVE      'N'           SCREIS            1
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C     Q1PRMS        IFEQ      4
     C                   MOVE      'A'           QQRTC
     C                   ENDIF
     C                   GOTO      TERM
     C                   END                                                    *INKA
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C*   COMMAND 13 = CHG USER PARMS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   GOTO      TOP
     C                   END
     C*** END   ADD *** 03/12/10 **************************************
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
     C***** BEGIN DEL **** 08/02/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/02/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    *IN10
     C*
     C                   END
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  EDIT FUND YEAR
     C*
      **** BEGIN ADD **** 05/03/96 ************************************
     C     SCEMP#        IFNE      0
      **** END   ADD **** 05/03/96 ************************************
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   ENDIF                                                  SCEMP# IFNE
      **** END   ADD **** 05/03/96 ************************************
     C*
     C     SCEMP#        IFNE      *ZERO
      *** BEG CHG ***** 09/01/04 **********************************
     C********** ELKY      CHAINPEL01                60
     C     ELKY          CHAIN     PEL00                              60
      *** END CHG ***** 09/01/04 **********************************
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
      **** BEGIN DEL **** 01/02/01 *********************************************
     C******************** END
      **** END   DEL **** 01/02/01 *********************************************
     C*
     C*  CHECK SUBMISSION HISTORY
     C*
     c**** Begin Add **** 01/20/15 *********************************************
     C                   MOVE      *OFF          *IN61
     c**** End   Add **** 01/20/15 *********************************************
     C     SHKY          CHAIN     PSH01                              6061
     C     *IN60         IFEQ      *OFF
     c**** Begin Add **** 01/20/15 *********************************************
     C     *IN61         ANDEQ     *OFF
     c**** End   Add **** 01/20/15 *********************************************
      **** BEGIN ADD **** 05/25/99 *********************************************
     C     SHSTAT        IFEQ      '4'
      **** BEGIN ADD **** 03/07/02 ************************************
     C     SHCODE        OREQ      'I'
      **** END   ADD **** 03/07/02 ************************************
      **** BEGIN ADD **** 10/22/07 ************************************
     C     SHCODE        OREQ      'T'
      **** END   ADD **** 10/22/07 ************************************
     C*** begin add ***  02/10/16  ***************************************
     C     SHCODE        OREQ      'N'
     C*** end add *****  02/10/16  ***************************************
     C                   MOVE      'Y'           SCREIS
     C                   ENDIF                                                  SHSTAT
      **** END   ADD **** 05/25/99 *********************************************
     C     *INKE         IFEQ      *OFF
     C     SHSTAT        ANDGT     '2'
     C*** BEGIN DEL *** 06/20/11 **************************************
     C*** BEGIN ADD *** 03/12/10 **************************************
     C**************           CLTID     ANDNE'025'
     C*** END   ADD *** 03/12/10 **************************************
     C*** END   DEL *** 06/20/11 **************************************
     C                   UPDATE    PSH01
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 12/15/00 *********************************************
     C     ELPFDT        IFNE      SHPFDT
     C     SHPFDT        ANDNE     0
     C     SHSTAT        ANDEQ     '4'
     C                   Z-ADD     SHPFDT        SHSBDT
     C                   Z-ADD     ELPFDT        SHPFDT
     C                   MOVE      'K'           SHCODE
     C                   ELSE
     C                   MOVE      *BLANKS       SHCODE
     C                   Z-ADD     0             SHSBDT
     C                   ENDIF                                                  ELPFDT IFNE SHPFDT
      **** END   ADD **** 12/15/00 *********************************************
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
      **** Begin add  11/11/14 *************************************************
     C                   ELSE
     c**** Begin Add **** 01/20/15 *********************************************
     C   61              EXFMT     WDWMSG
     C   61              GOTO      TERM
      **** End add    11/11/14 *************************************************
     C                   END
      **** BEGIN ADD **** 01/02/01 *********************************************
     C                   ENDIF                                                  SCEMP# IFNE 0
      **** END   ADD **** 01/02/01 *********************************************
      **** BEGIN DEL **** 03/07/01 *********************************************
      **** BEGIN ADD **** 11/21/95 ************************************
      ** DO NOT ALLOW A POLICY PAGE TO BE SUBMITTED FOR AN EMPLOYER/FYR
      ** THAT HAS NOT BEEN BILLED.
      ** IF THE MANUAL PREMIUM IS ZERO AND AT LEAST ONE PAYROLL
      ** RECORD EXISTS, THE EMPLOYER HAS NOT BEEN BILLED.
      **
     C********** SCEMP#    IFNE 0
      **********
     C********** ELMAPR    IFEQ *ZERO
      **********
     C**********           MOVE UPCO#     XXCO#
     C**********           MOVE SCFND     XXFND
     C**********           MOVE SCEMP#    XXEMP#
     C**********           MOVE SCDIV     XXDIV
     C**********           MOVE SCFYR     XXFYR
      **********
     C********** EPKY      CHAINPEP01                60
      **********
     C********** *IN60     DOWEQ*OFF
      **********
     C********** EPSAPR    IFNE *ZERO
     C********** EPEAPR    ORNE *ZERO
     C**********           MOVE *ON       *IN26
     C**********           GOTO STRT
     C**********           ENDIF                           EPSAPR IFNE
      **********
     C********** EPKY      READEPEP01                    60
      **********
     C**********           ENDDO                           *IN60 DOWEQ
      **********
     C**********           ENDIF                           ELMAPR IFEQ
      **********
     C**********           ENDIF                           SCEMP# IFNE
      **** END   ADD **** 11/21/95 ************************************
      **** END   DEL **** 03/07/01 *********************************************
      **** BEGIN ADD **** 05/03/96 ************************************
      *
      * VALIDATE BEGINNING POLICY EFFECTIVE DATE
      *
     C     SCEMP#        IFEQ      0
      *
      *
     C                   Z-ADD     SCFPFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFPFD
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN27
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SRERR IFEQ
      *
      * VALIDATE ENDING POLICY EFFECTIVE DATE
      *
      *
     C                   Z-ADD     SCTPFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTPFD
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN29
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SRERR IFEQ
      *
      * DATES MUST BE IN ASCENDING ORDER
      *
     C                   Z-ADD     SCFPFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C                   Z-ADD     SCTPFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
      *
     C     XXDAT2        IFLT      XXDAT1
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF                                                  XXDAT2
      *
     C                   ENDIF                                                  SCEMP# IFNE
      **** END   ADD **** 05/03/96 ************************************
      *
     C*** BEGIN ADD ***  11/21/96  ***********************************
     C*
     C*  VALIDATE THE RESOLVE TO DARTS FLAG.
     C*
     C     SCDART        IFNE      'Y'
     C     SCDART        ANDNE     'N'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDART NE Y
     C*** END ADD *****  11/21/96  ***********************************
      ***** BEG ADD ******* 05/11/09 **********************************
      *
      *  IF CLT093 AND "Resubmit policy for interstate change only" = Y
      *
     C     CLTID         IFEQ      '093'
      *
     C     SCSTAT        IFNE      'Y'
     C     SCSTAT        ANDNE     ' '
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSTAT
      *
     C     SCSTAT        IFEQ      'Y'
      *
     C     SHKY          CHAIN     PSH01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF                                                  SHKY
      *
     C                   ENDIF                                                  SCSTAT = Y
      *
     C                   ENDIF                                                  CLT093
      *
      ***** END ADD ******* 05/11/09 **********************************
      *** BEG ADD ***** 09/01/04 **************************************
      *
      *  GIVE INFO MSG THAT POLICIES WITH VOLUNTARY LABOR CHARGE HAVE A
      *  'WC000311A' ENDORSEMENT ATTACHED
      *
     C     VLCFLG        IFEQ      ' '
     C                   EXSR      LODVLC
      *
     C     VLCFLG        IFEQ      *ON
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   ENDIF
      *
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   ENDIF                                                  VLCFLG = *OFF
      *** END ADD ***** 09/01/04 **************************************
      *** begin add ***  09/30/16  ***********************************
      /free
       xxSubmit = *on;

       If isUseScript and scEmp# <> 0;

          xxProcType = processType;

          xxMass = ' ';

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) (x1Co#:scFnd) pex00;
          If %found and EXPPCD <> ' ';
             xxDart = 'Y';
             xxRMCD = EXPPCD;
          Else;
             xxDart = 'N';
             xxRMCD = ' ';
          Endif;

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass);

       Endif;
      /end-free
     C                   If        xxSubmit = *off
     C                   Goto      Strt
     C                   Endif
      *** end add *****  09/30/16  ***********************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                MOVE UPCO#     QQCO#   3
     C                   MOVE      X1CO#         QQCO#             3
     C*** END CHG *****  11/04/97  ************************************
     C                   MOVE      SCFND         QQFND             3
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                MOVE SCEMP#    QQEMP#  5
     C*****                MOVE SCDIV     QQDIV   3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C*** END CHG *****  11/04/97  ************************************
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCREIS        QQREIS            1
      **** BEGIN ADD **** 05/03/96 ************************************
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                MOVE SCFPFD    QQFPFD  6
     C*****                MOVE SCTPFD    QQTPFD  6
     C                   MOVE      SCFPFD        QQFPFD            8
     C                   MOVE      SCTPFD        QQTPFD            8
     C*** END CHG *****  11/04/97  ************************************
      **** END   ADD **** 05/03/96 ************************************
     C***  BEGIN ADD  ***  09/11/96  *********************************
     C                   MOVE      UPOUTQ        QQQUE            10
     C***  END   ADD  ***  09/11/96  *********************************
     C                   CALL      'W7412C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQREIS
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   PARM                    QQFPFD
     C                   PARM                    QQTPFD
      **** END   ADD **** 05/03/96 ************************************
     C***  BEGIN ADD  ***  09/11/96  *********************************
     C                   PARM                    QQQUE
     C***  END   ADD  ***  09/11/96  *********************************
     C*** BEGIN ADD ***  11/21/96  ***********************************
     C                   PARM                    SCDART
     C                   PARM      'Y'           QQHARD            1
     C                   PARM      'Y'           QQAGTO            1
     C*** END ADD *****  11/21/96  ***********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      **** BEG ADD ***** 09/01/04 *************************************
     C*====================================================================*
     C* LOAD THE INFO MSG SUBFILE FOR POLICIES THAT HAVE A VOL LABOR CHARGE
     C*====================================================================*
     C     LODVLC        BEGSR
      *
     C                   MOVE      *ON           *IN05
     C                   WRITE     SCR02C
     C                   MOVE      *OFF          *IN05
     C                   Z-ADD     1             RRN2              5 0
     C                   MOVEL     ENDCON        XXFORM
      *
     C     SCEMP#        IFEQ      *ZERO
      *
     C     ELKY3         SETLL     PEL03
     C     FNKY          READE     PEL03                                  60
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        ANDLE     XXDAT2
     C     RRN2          ANDLE     9999
      *
     C     ELVLCA        IFNE      *ZERO
      *
     C     SHKY1         CHAIN(N)  PSH01                              60
     C     SHSTAT        IFEQ      '2'
     C     *IN60         ANDEQ     *OFF
      *
     C     MUKY          CHAIN     PMU01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           VLCFLG            1
     C                   Z-ADD     ELEMP#        S2EMP#
     C                   Z-ADD     ELDIV         S2DIV
     C                   Z-ADD     ELFYR         S2FYR
     C     EMKY1         CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        S2NAME
     C                   WRITE     SCR02S
     C                   ADD       1             RRN2
     C                   ENDIF                                                  *IN60 = *ON
      *
     C                   ENDIF                                                  SHSTAT = '2'
      *
     C                   ENDIF                                                  ELVLCA <> 0
      *
     C     FNKY          READE     PEL03                                  60
     C                   ENDDO
      *
     C                   ELSE
     C     ELKY1         SETLL     PEL01
     C     ELKY1         READE     PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C     RRN2          ANDLE     9999
      *
     C     SHKY          CHAIN(N)  PSH01                              60
     C     SHSTAT        IFEQ      '2'
      *
     C     ELVLCA        IFNE      *ZERO
      *
     C     MUKY          CHAIN     PMU01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           VLCFLG            1
     C                   Z-ADD     ELEMP#        S2EMP#
     C                   Z-ADD     ELDIV         S2DIV
     C                   Z-ADD     ELFYR         S2FYR
     C     EMKY1         CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        S2NAME
     C                   WRITE     SCR02S
     C                   ADD       1             RRN2
     C                   ENDIF                                                  *IN60 = *ON
      *
     C                   ENDIF                                                  ELVLCA <> *ZERO
      *
     C                   ENDIF                                                  SHSTAT = '2'
      *
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  SCEMP# = 0
      *
      *
     C                   ENDSR                                                  LODVLC
      **** END ADD ***** 09/01/04 *************************************
