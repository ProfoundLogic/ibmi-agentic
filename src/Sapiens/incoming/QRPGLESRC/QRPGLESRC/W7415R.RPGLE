     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*
     H/TITLE W7415R - NCCI Hold/Release Policy Submission             *
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*    DISPLAYS ALL PENDING NCCI POLICY SUBMISSION HISTORY        *
     H*    RECORDS AND ALLOWS THE USER TO HOLD OR RELEASE RECORDS     *
     H*    FOR POLICY SUBMISSION.  THE USER CAN SUBMIT POLICY         *
     H*    GENERATION FOR ALL RELEASED RECORDS VIA A CMD KEY.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*      MENU                                                     *
     H*  CALLS TO:                                                    *
     H*      W7415C - NCCI POLICY INFORMATION PAGE SUBMITTER          *
     H*                                                               *
     H*    PROGRAMMER: C:24                                           *
     H*    CREATED:    6/27/94      PROJECT: WC313                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    HIGHLIGHT RECORD IN ERROR                              *
     H*  21    INVALID OPTION SELECTED                                *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  49    AT LEAST ON NON-BLANK OPTION ENTERED                   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*  71    R/N/B.                                                 *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*    SMCOL (PCO01): CO                                          *
     H*    WMUPL (PUP01): USER                                        *
     H*    WMFNL (PFN01): CO/FND                                      *
     H*****WMSHL2  (PSH01): CO/FND/FYR/EMP# TYPE = P, STAT = 1 OR 2   *
     H*    WMSHL8  (PSH01): CO/FND/PFDT/EMP# TYPE = P, STAT = 1 OR 2  *
     H*    WMEML   (PEM01): CO/FND/EMP#/DIV                           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/15/23 C:65  Project: WC2908                                *
     H*                Add field to store value of "run policies in   *
     H*                blocks" in order to submit the policy pages in *
     H*                increments via a timestamp value.              *
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
     H* 07/12/16 C:136 Support: 38832                                 *
     H*                Don't unncessarily lock WMXHL8                 *
     H*                                                               *
     H* 03/12/10 C:062 PROJECT 25WQK                                  *
     H*                Display CCS headings if client 25              *
     H*                                                               *
     H* 07/23/07 C:132 PROJECT 114WMS                                 *
     H*              - Add WDRRP file and check flag to see if RRDBCR *
     H*                is a "Y", if not don't check for scheduled     *
     H*                DBCR form being printed.                       *
     H*              - Additional error corrections found in QA tests *
     H*                                                               *
     H* 07/13/07 C:132 PROJECT 114WMS                                 *
     H*              - Chg edit to prevent Release of Policy Page as  *
     H*                per issues found in QA of project.             *
     H*              - Add WDRRL file to check flag RRDBCR for "Y"    *
     H*              - Bypass edits for client 114 if RRDBCR <> "Y"   *
     H*                in both OPT6 and CMD9 (F9)                     *
     H*                                                               *
     H* 04/04/07 C:61  PROJECT 114WMS                                 *
     H*                Add edit to prevent Release of Policy Page if  *
     H*                DB/CR entered and DB/CR form not filled out    *
     H*                (No WMRFP record for policy).                  *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                ADD INFORMATIONAL MESSAGE THAT CHECKS IF       *
     H*                POLICY WITH A VOLUNTARY LABOR CHARGE HAS A     *
     H*                'WC000311A' ENDORSEMENT ATTACHED.              *
     H*                                                               *
     H* 02/17/04 C:65  PROJECT SY059                                  *
     H*                ADD A BLANK PARM FOR CALL TO W7415C.           *
     H*                                                               *
     H* 11/11/03 C:61  SUPPORT GO129 39804                            *
     H*              - Change screen to display Eff Dt first.         *
     H*              - Change to have 10 line in subfile.             *
     H*                                                               *
     H* 04/24/01 C:61  PROJECT WC1032                                 *
     H*                Add new prompt for "Generate for renewal, new  *
     H*                business OR both".                             *
     H*                                                               *
     H* 08/04/99  C:118  PROJECT: WC577                               *
     H*                  Made V3 changes to display file and RPG.     *
     H*                                                               *
     H* 09/04/98  C:35   PROJECT: SUPPORT WC095 13221                 *
     H*                  SCREEN WOULD NOT ROLL DUE TO THE SVEMP#      *
     H*                  FIELD BEING THE WRONG SIZE.                  *
     H*                                                               *
     H* 04/10/98  C:65  PROJECT:WC702                                 *
     H*                 YOU CANNOT SET BOTH DARTS AND HARDCOPY FLAGS  *
     H*                 TO 'N'.                                       *
     H*                                                               *
     H* 04/09/98  C:65  PROJECT:WC702                                 *
     H*                 RELAX THE EDIT ON THE HARDCOPY FLAG.          *
     H*                                                               *
     H* 11/04/97  C:65  PROJECT:WC702                                 *
     H*                 GENERAL CLEANUP                               *
     H*                                                               *
     H* 11/21/96  C:65  PROJECT: CM023                                *
     H*                 ADDED STEP FLAG FOR RESUBMISSION.             *
     H*                 ADDED RESOLVE TO DARTS, GENERATE HARDCOPY,    *
     H*                 AND GENERATE 2ND COPY FLAGS TO A NEW SCREEN 2.*
     H*                                                               *
     H* 09/11/96  C:33  PROJECT: WC420                                *
     H*                 USE USER PARM OUTQ ON DEMAND.                 *
     H*                                                               *
     H* 05/03/96  C:35  PROJECT:WC525                                 *
     H*                 CHANGED TO DISPLAY BY POLICY EFF DATE INSTEAD *
     H*                 OF FUND YEAR.                                 *
     H*                                                               *
     H*****************************************************************
     FW7415F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
      *** BEG ADD ***** 09/01/04 ************************************
     F                                     SFILE(SCR03S:RRN3)
      *** END ADD ***** 09/01/04 ************************************
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      **** BEGIN CHG **** 05/03/96 ************************************
     F********** WMSHL2  UF  E           K        DISK
     FWMSHL8    UF   E           K DISK
      **** END   CHG **** 05/03/96 ************************************
      *** BEG ADD ***** 09/01/04 **************************************
     F                                     INFDS(SHL8DS)
     F*** begin add ***  08/15/23  ************************************
     FWMSHL12   UF   E           K DISK    RENAME(PSH01:PSH12)
     F*** end add *****  08/15/23  ************************************
     FWMMUL     IF   E           K DISK
     FWDELL     IF   E           K DISK
      *** END ADD ***** 09/01/04 **************************************
     FWMEML     IF   E           K DISK
     F*** BEGIN ADD ***  11/21/96  ************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*** END ADD *****  11/21/96  ************************************
      ***** BEG ADD ******* 04/04/07 **********************************
      *
      * Scheduled Rating Form
     FWMRFL     IF   E           K DISK
      ***** END ADD ******* 04/04/07 **********************************
      *
      ***** BEG ADD ******* 07/13/07 **********************************
     F*
     F** Statistical Reporting/Rating File
     FWDRRL     IF   E           K DISK
     F*
     F** Rate Set Code Table File
     FWMRSL     IF   E           K DISK
      ***** END ADD ******* 07/13/07 **********************************
      *** begin add ***  09/30/16  ************************************
     FWMEXL     IF   E           K DISK    rename(pex01:pex00)
     d/copy sPrLetter
     D/copy sPrXML
     D*** begin add ***  08/15/23  ************************************
     d myStamp         s             20
     d myStampInt      s             20  0
     d count           s              4  0
     D*** end add *****  08/15/23  ************************************
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
      *
      *** BEG ADD ***** 09/01/04 **************************************

     D SHL8DS          DS
     D  SHRRN                397    400B 0
      *** END ADD ***** 09/01/04 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *** BEG ADD ***** 09/01/04 ************************************
     D ENDCON          C                   CONST('WC000311A')
      *** END ADD ***** 09/01/04 ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*** BEGIN ADD ***  11/21/96  ************************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** END ADD *****  11/21/96  ************************************
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR SUBMISSION HISTORY FILE
     C*
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      **** BEGIN CHG **** 05/03/96 ************************************
     C******************** KFLD           XXFYR
     C                   KFLD                    XXPFDT
      **** END   CHG **** 05/03/96 ************************************
     C                   KFLD                    XXEMP#            9 0
     C*
     C     SHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      **** BEGIN DEL **** 05/03/96 ************************************
     C******************** KFLD           XXFYR
      **** END   DEL **** 05/03/96 ************************************
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
      *
      *** BEG ADD ***** 09/01/04 **************************************
     C*
     C*  KEY LIST FOR ASSESSABLE MUTUAL FORMS
     C*
     C     ELKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    SHFYR
     C*
     C     MUKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    SHFYR
     C                   KFLD                    XXFORM
      *** END ADD ***** 09/01/04 **************************************
      *
      ***** BEG ADD ******* 07/13/07 **********************************
     C*
     C*  STATISTICAL REPORTING/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST              2
     C                   KFLD                    ELPFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELRTCD
     C*
      ***** END ADD ******* 07/13/07 **********************************
      *
      ***** BEG ADD ******* 07/23/07 **********************************
     C*
     C*  Scheduled Rating Data file
     C*
     C     RFKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      ***** END ADD ******* 07/23/07 **********************************
      *
      **** BEGIN ADD **** 05/03/96 ************************************
     C     *LIKE         DEFINE    SHPFDT        XXPFDT
     C     *LIKE         DEFINE    SHPFDT        SVPFD1
     C     *LIKE         DEFINE    SHPFDT        SVPFDT
      **** END   ADD **** 05/03/96 ************************************
      **** BEGIN ADD **** 09/04/98 ************************************
     C     *LIKE         DEFINE    SHEMP#        SVEMP#
      **** END   ADD **** 09/04/98 ************************************
      **** BEG ADD ****** 09/01/04 ************************************
     C     *LIKE         DEFINE    MUFORM        XXFORM
      **** BEG ADD ****** 09/01/04 ************************************
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
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
      *
      *
      ***** BEG ADD ******* 04/04/07 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ******* 04/04/07 **********************************
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   SETOFF                                       16
     C     CLTID         IFEQ      '025'
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
     C                   Z-ADD     UPFYR         XXFYR             3 0
     C                   MOVE      UPFND         SCFND             3 0
      **** BEGIN DEL **** 05/03/96 ************************************
     C******************** Z-ADDUPFYR     SCFYR   30
      **** END   DEL **** 05/03/96 ************************************
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
     C***** BEGIN DEL **** 08/04/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/04/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
      ***** BEG ADD ******* 04/24/01 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      ***** END ADD ******* 04/24/01 **********************************
     C*
     C*   RESET POINT IN PROGRAM
     C*
     C     TOP           TAG
     C*
     C*   INITIALIZE SCREEN
     C*
     C                   Z-ADD     0             RRN2              5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEGIN CHG ***  11/04/97  ************************************
     C******               MOVE SCEMPX    SVEMP1  50
     C                   MOVE      SCEMPX        SVEMP1            9 0
     C*** END CHG *****  11/04/97  ************************************
     C                   MOVE      SCEMPX        SVEMP#
     C                   SUB       1             SVEMP#
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     SCPFDX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVPFD1
     C                   Z-ADD     SCPFDX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVPFDT
      **** END   ADD **** 05/03/96 ************************************
     C*
     C*   CLEAR SUBFILE
     C*
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN01
      ***** BEG ADD ******* 07/13/07 **********************************
     C                   MOVE      *OFF          *IN34
     C                   MOVE      *OFF          *IN35
     C                   MOVE      *OFF          *IN36
      ***** END ADD ******* 07/13/07 **********************************
     C                   END
     C*** BEGIN ADD ***  11/21/96  ************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF                                                  Q2RECF IFEQ
     C*** END ADD *****  11/21/96  ************************************
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  CMD 1 - END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKA
     C*** BEGIN ADD ***  11/21/96  ************************************
     C*
     C*  CMD 2 - PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  ENDIF *INKB
     C*** END ADD *****  11/21/96  ************************************
     C*
     C*  CMD 7 - HOLD ALL SUBMISSIONS
     C*
     C     *INKG         IFEQ      *ON
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
      *** beg chg ***** 07/12/16 *****************
     C*****SHKY1         CHAIN     PSH01                              60
     C******IN60         DOWEQ     *OFF
     C**********         MOVE      '1'           SHSTAT
     C**********         UPDATE    PSH01
     C*****SHKY1         READE     PSH01                                  60
     C**********         ENDDO

      /free
       Setll shky1 psh01;
       Dou %Eof(WMSHL8);
          Reade(E) shky1 psh01;
          If %Error();
             Reade(N) shky1 psh01;
             Iter;
          Endif;
          If Not(%Eof(WMSHL8));
             SHstat = '1';
             Update psh01;
          Endif;
       Enddo;
      /end-free
      *** end chg ***** 07/12/16 *****************
     C                   GOTO      TOP
     C                   END                                                    END *INKG
     C*
     C*  CMD 9 - RELEASE ALL SUBMISSIONS
     C*
     C     *INKI         IFEQ      '1'
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
      *** beg chg ***** 07/12/16 **********************
     C*****SHKY1         CHAIN     PSH01                              60
     C******IN60         DOWEQ     *OFF
      /free
       Setll shky1 psh01;
       Dou %Eof(WMSHL8);
          Reade(E) shky1 psh01;
          If %Error();
             Reade(N) shky1 psh01;
             Iter;
          Endif;
          If %Eof(WMSHL8);
             Leave;
          Endif;
      /end-free
      *** end chg ***** 07/12/16 **********************
      ***** BEG DEL ******* 04/04/07 **********************************
     C*********            MOVE '2'       SHSTAT
     C*********            UPDATPSH01
      ***** END DEL ******* 04/04/07 **********************************
      ***** BEG ADD ******* 04/04/07 **********************************
     C                   SELECT
      *
     C     CLTID         WHENEQ    '114'
      *
      ***** BEG ADD ******* 07/13/07 **********************************
     C                   MOVE      *OFF          *IN41                          Process Edits
      ***** END ADD ***** 07/13/07 ************************************
     C     ELKY          CHAIN     PEL01                              60
      *
     C  N60ELKY          CHAIN     RFP01                              60
      *
      ***** BEG ADD ******* 07/23/07 **********************************
      * Get the state from the Rate Set file, for key to Stat file
     C  N60RSKY          CHAIN     WMRSL                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
      * Now check RRDBCR flag to do the edits, else bypass
     C     RRKY          SETGT     WDRRL
     C     RRKY1         READPE    WDRRL                                  61
     C     *IN61         IFEQ      *OFF
     C     RRDBCR        ANDEQ     'Y'
     C                   MOVE      *ON           *IN41                          Process Edits
     C                   ELSE
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF                                                  RRDBCR <> "Y"
     C*
     C                   ENDIF                                                  60 OR 61 ON
      ***** END ADD ******* 07/23/07 **********************************
      *
      * If record found, continue processing, else bypass
     C     *IN60         IFEQ      *OFF
      *
      ***** BEG ADD ******* 07/23/07 **********************************
      * Check to see if Scheduled DB/CR form printed. If so, Release
     C     RFPRTF        IFNE      'Y'
     C                   MOVE      *OFF          *IN41                          Process Edits
     C                   ENDIF                                                  60 OR 61 ON
      ***** END ADD ******* 07/23/07 **********************************
      *
      ***** BEG ADD ******* 07/13/07 **********************************
      *
      *  Check sum of WMRFP BD/CR fields = ELBDCR
      *
     C     *IN41         IFEQ      *ON
      *
     C                   Z-ADD     *ZEROS        ZZDBCD            5 2
     C                   Z-ADD     *ZEROS        ZZDBCC            5 2
     C                   Z-ADD     *ZEROS        ZZTOTC            5 4
     C                   Z-ADD     *ZEROS        ZZTOTD            5 4
     C                   Z-ADD     *ZEROS        ZZDBCR            5 4
      *
     C                   ADD       RFPRCA        ZZDBCC
     C                   ADD       RFPRDA        ZZDBCD
     C                   ADD       RFCPCA        ZZDBCC
     C                   ADD       RFCPDA        ZZDBCD
     C                   ADD       RFMFCA        ZZDBCC
     C                   ADD       RFMFDA        ZZDBCD
     C                   ADD       RFSDCA        ZZDBCC
     C                   ADD       RFSDDA        ZZDBCD
     C                   ADD       RFEMCA        ZZDBCC
     C                   ADD       RFEMDA        ZZDBCD
     C                   ADD       RFMCCA        ZZDBCC
     C                   ADD       RFMCDA        ZZDBCD
     C                   ADD       RFMSCA        ZZDBCC
     C                   ADD       RFMSDA        ZZDBCD
      *
     C     ZZDBCC        IFNE      *ZEROS
     C     ZZDBCC        MULT      -1            ZZDBCC
     C     ZZDBCC        DIV       100           ZZTOTC                         Total Credits
     C                   ENDIF
     C     ZZDBCD        IFNE      *ZEROS
     C     ZZDBCD        DIV       100           ZZTOTD                         Total Debits
     C                   ENDIF
      *
     C     ZZTOTC        IFNE      *ZEROS
     C     ZZTOTD        ORNE      *ZEROS
     C     ZZTOTC        ADD       ZZTOTD        ZZDBCR
     C     ELDBCR        IFNE      *ZEROS
     C     ZZDBCR        ADD       1.000         ZZDBCR
     C                   ENDIF                                                  ELDCR <> *ZEROS
     C                   ENDIF                                                  ZZTOTC <> *ZEROS
      ***** END ADD ******* 07/13/07 **********************************
      *
      * If ELDBCR = ZZDBCR and RFPRTF = "Y" then update record.
     C     ELDBCR        IFEQ      ZZDBCR
     C     RFPRTF        ANDEQ     'Y'
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF                                                  ELDBCR<>ZZDBCR
      *
      ***** BEG ADD ***** 07/13/07 ************************************
     C                   ENDIF                                                  *IN41 = *ON
      ***** END ADD ***** 07/13/07 ************************************
     C                   ENDIF                                                  *IN60 = *OFF
      *
      *
      ***** BEG CHG ******* 07/13/07 **********************************
      *  If client is not 114, then update record.
     C*****************    OTHER
     C     CLTID         WHENNE    '114'
      ***** END CHG ******* 07/13/07 **********************************
      *
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
      *
     C                   ENDSL
      *
      ***** END ADD ******* 04/04/07 **********************************
      *** beg del ***** 07/12/16 *****************
     C*****SHKY1         READE     PSH01                                  60
      *** end del ***** 07/12/16 *****************
     C                   ENDDO
     C                   GOTO      TOP
     C                   END                                                    END *INKI
     C*
     C*  CMD 11 - SUBMIT POLICY PAGE JOB
     C*
     C     *INKK         IFEQ      *ON
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*** BEGIN ADD ***  11/21/96  ************************************
      *
      *** BEG ADD ***** 09/01/04 **************************************
      *
      *  GIVE INFO MSG THAT POLICIES WITH VOLUNTARY LABOR CHARGE HAVE A
      *  'WC000311A' ENDORSEMENT ATTACHED
      *
     C     VLCFLG        IFEQ      ' '
      *
     C                   Z-ADD     *ZERO         RRN3              5 0
     C                   MOVEL     ENDCON        XXFORM
     C     SHKY1         CHAIN(N)  PSH01                              60
     C     *IN60         DOWEQ     *OFF
     C     RRN3          ANDLE     9999
      *
     C     SHSTAT        IFEQ      '2'
     C     ELKY          CHAIN     PEL01                              60
     C     ELVLCA        IFNE      *ZERO
     C     *IN60         ANDEQ     *OFF
      *
     C     MUKY          CHAIN     PMU01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           VLCFLG            1
     C                   EXSR      LODVLC
     C                   ENDIF                                                  *IN60 = *ON
      *
     C                   ENDIF                                                  ELVLCA = *OFF
      *
     C                   ENDIF                                                  SHSTAT = '2'
      *
      *** beg chg ***** 07/12/16 **********************
     C*****SHKY1         READE     PSH01                                  60
     C     SHKY1         READE(N)  PSH01                                  60
      *** end chg ***** 07/12/16 **********************
     C                   ENDDO
      *
     C                   ENDIF                                                  VLCFLG = *OFF
      *
     C     VLCFLG        IFEQ      *ON
     C                   WRITE     SCR03
     C                   EXFMT     SCR03C
     C                   ENDIF
      *** END ADD ***** 09/01/04 **************************************
      *
     C                   MOVEA     '00'          *IN(51)
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      'N'           SCAGTO
     C                   MOVE      'Y'           SCHARD
     C                   MOVE      *BLANK        SCDART
     C*
     C*  CHECK IF OFFICE IS INSTALLED.
     C*
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     SYOFIF        ORNE      'Y'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'N'           SCDART
     C                   ELSE                                                   *IN60 EQ 1
     C                   MOVE      'Y'           SCDART
     C                   ENDIF                                                  *IN60 EQ 1
      *** begin add ***  09/30/16  ***********************************
      /Free
       isUseScript = UseScriptura(processType);
       visScript = *off;
       If isUseScript or *in51 = *on;
          visScript = *on;
       Endif;
      /end-free
      *** end add *****  09/30/16  ***********************************
     C*
     C*  GET ELECTRONIC SUBMISSION FLAG
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     WCFL03        ORNE      'Y'
     C                   MOVE      *ON           *IN52
     C                   ENDIF                                                  *IN60 IFEQ 1
     C*
     C                   GOTO      STRT
     C*** begin del ***  08/15/23  ************************************
     C*****SUBMIT        TAG
     C*** END ADD *****  11/21/96  ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE POLICY PAGES.
     C*
     C*******            MOVE      XXCO#         QQCO#             3
     C*******            MOVE      XXFND         QQFND             3
     C*** BEGIN CHG ***  11/04/97  ************************************
     C*****                MOVE *ZERO     QQEMP#  5
     C*****                MOVE *ZERO     QQDIV   3
     C*******            MOVE      *ZERO         QQEMP#            9
     C*******            MOVE      *ZERO         QQDIV             5
     C*** END CHG *****  11/04/97  ************************************
      **** BEGIN DEL **** 05/03/96 ************************************
     C******************** MOVE XXFYR     QQFYR   3
      **** END   DEL **** 05/03/96 ************************************
     C*******            MOVE      *BLANK        QQREIS            1
     C***  BEGIN ADD  ***  09/11/96  *********************************
     C*******            MOVE      UPOUTQ        QQQUE            10
     C***  END   ADD  ***  09/11/96  *********************************
     C*** BEGIN ADD ***  11/21/96  ************************************
     C*******            MOVE      '0'           QQSTEP            1
     C*** END ADD *****  11/21/96  ************************************
     C*******            CALL      'W7415C'
     C*******            PARM                    QQCO#
     C*******            PARM                    QQFND
     C*******            PARM                    QQEMP#
     C*******            PARM                    QQDIV
      **** BEGIN DEL **** 05/03/96 ************************************
     C******************** PARM           QQFYR
      **** END   DEL **** 05/03/96 ************************************
     C*******            PARM                    QQREIS
     C***  BEGIN ADD  ***  09/11/96  *********************************
     C*******            PARM                    QQQUE
     C***  END   ADD  ***  09/11/96  *********************************
     C*** BEGIN ADD ***  11/21/96  ************************************
     C*******            PARM                    QQSTEP
     C*******            PARM                    SCDART
     C*******            PARM                    SCHARD
     C*******            PARM                    SCAGTO
     C*** END ADD *****  11/21/96  ************************************
      ***** BEG ADD ******* 04/24/01 **********************************
     C*******            PARM                    SCRNB
      ***** END ADD ******* 04/24/01 **********************************
      ***** BEG ADD ******* 02/17/04 **********************************
     C*******            PARM      *BLANKS       QQDMBR           10
      ***** END ADD ******* 02/17/04 **********************************
     C*
     C*******            GOTO      TERM
     C*** end del *****  08/15/23  ************************************
     C                   END                                                    END *INKK
     C*
     C*  CMD 13 - PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
     C                   Z-ADD     UPFYR         XXFYR             3 0
     C                   MOVE      UPFND         SCFND             3 0
      **** BEGIN DEL **** 05/03/96 ************************************
     C******************** Z-ADDUPFYR     SCFYR   30
      **** END   DEL **** 05/03/96 ************************************
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
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C*  PROCESS HELP REQUESTS - *IN10
     C*
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C*  *IN17 -  ROLLDOWN REQUEST
     C*
     C     *IN17         IFEQ      *ON                                          ROLLDOWN
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
     C                   MOVE      SVEMP1        XXEMP#
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   MOVE      SVPFD1        XXPFDT
      **** END   ADD **** 05/03/96 ************************************
     C     SHKY          SETLL     PSH01
      *** beg chg ***** 07/12/16 *****************
     C*****SHKY1         READPE    PSH01                                  50
     C     SHKY1         READPE(N) PSH01                                  50
      *** end chg ***** 07/12/16 *****************
     C     *IN50         IFEQ      *ON
     C                   MOVE      *ON           *IN31
     C                   Z-ADD     1             HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN31
     C                   MOVE      SHEMP#        SCEMPX
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     SHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDX
      **** END   ADD **** 05/03/96 ************************************
     C                   DO        10            X
      *** beg chg ***** 07/12/16 *****************
     C*****SHKY1         READPE    PSH01                                  50
     C     SHKY1         READPE(N) PSH01                                  50
      *** end chg ***** 07/12/16 *****************
     C   50              LEAVE
     C                   MOVE      SHEMP#        SCEMPX
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     SHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDX
      **** END   ADD **** 05/03/96 ************************************
     C                   ENDDO
     C                   GOTO      TOP
     C                   END                                                    *IN17 ROLLDOWN
     C*
     C*  *IN18 -  ROLLUP REQUEST
     C*
     C     *IN18         IFEQ      *ON                                          ROLLUP
     C*
     C     *IN32         IFEQ      *ON
     C                   MOVE      XXLAST        HHREC#
     C                   GOTO      STRT                                         NO ROLL IF EOF
     C                   ENDIF
     C*
     C                   MOVE      XXRRN2        RRN2
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    *IN18 ROLLUP
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*** BEGIN ADD ***  11/21/96  ************************************
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C*** END ADD *****  11/21/96  ************************************
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   MOVE      *OFF          *IN49
     C     CKSEL         TAG
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      *OFF
     C*
     C                   MOVE      *ON           *IN49
     C*
     C*  OPTION BLANK - BYPASS
     C*
     C     SCSEL         IFEQ      *BLANK
     C                   UPDATE    SCR01S
     C                   GOTO      CKSEL
     C                   ENDIF
     C*
     C     *IN21         IFEQ      *OFF
     C                   Z-ADD     RRN2          HHREC#
     C                   ENDIF
     C*
     C*  OPTION 3 - HOLD SUBMISSION
     C*
     C     SCSEL         IFEQ      '3'
     C                   MOVE      SCEMP#        XXEMP#
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
      **** END   ADD **** 05/03/96 ************************************
      *** beg chg ***** 07/12/16 ***************************
     C*****SHKY**        CHAIN     PSH01                              60
     C***********        MOVE      '1'           SHSTAT
     C**N60******        UPDATE    PSH01
     C***********        MOVE      *BLANK        SCSTAT
     C***********        MOVEL     'HELD'        SCSTAT
     C***********        MOVE      *BLANK        SCSEL
     C***********        UPDATE    SCR01S
     C     SHKY          CHAIN(E)  PSH01                              60
     C                   IF        %ERROR()
     C                   ELSE
     C                   MOVE      '1'           SHSTAT
     C  N60              UPDATE    PSH01
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVEL     'HELD'        SCSTAT
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   ENDIF
      *** end chg ***** 07/12/16 ***************************
     C                   GOTO      CKSEL
     C                   END                                                    END EQ '3'
     C*
     C*  OPTION 6 - RELEASE SUBMISSION
     C*
     C     SCSEL         IFEQ      '6'
     C                   MOVE      SCEMP#        XXEMP#
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
      **** END   ADD **** 05/03/96 ************************************
      *** beg chg ***** 07/12/16 **************************************
     C*****SHKY          CHAIN     PSH01                              60
      ***** BEG ADD ******* 04/04/07 **********************************
     C******IN60         IFEQ      *OFF
     C     SHKY          CHAIN(E)  PSH01                              60
     C                   IF        NOT(*IN60) AND NOT(%ERROR())
      *** beg chg ***** 07/12/16 **************************************
      *
     C                   SELECT
      *
     C     CLTID         WHENEQ    '114'
      *
     C                   MOVE      *OFF          *IN34
     C                   MOVE      *OFF          *IN35
     C                   MOVE      *OFF          *IN36
      ***** BEG ADD ******* 07/23/07 **********************************
     C                   MOVE      *OFF          *IN41                          Process Edits
      ***** END ADD ******* 07/23/07 **********************************
      *
     C     ELKY          CHAIN     RFP01                              60
      *
      * If record not found, flag for error message
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN34
     C                   ENDIF                                                  *IN60 = *ON
      *
     C  N60ELKY          CHAIN     PEL01                              60
      *
      * If record found, continue processing, else bypass
     C     *IN60         IFEQ      *OFF
      *
      ***** BEG ADD ******* 07/23/07 **********************************
      * Get the state from the Rate Set file, for key to Stat file
     C  N60RSKY          CHAIN     WMRSL                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
      * Now check RRDBCR flag to do the edits, else bypass
     C     RRKY          SETGT     WDRRL
     C     RRKY1         READPE    WDRRL                                  61
     C     *IN61         IFEQ      *OFF
     C     RRDBCR        ANDEQ     'Y'
     C                   MOVE      *ON           *IN41                          Process Edits
     C                   ELSE
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF                                                  RRDBCR <> "Y"
     C*
     C                   ENDIF                                                  60 OR 61 ON
      *
      ***** END ADD ******* 07/23/07 **********************************
      *
      ***** BEG ADD ******* 07/13/07 **********************************
      *
     C     *IN41         IFEQ      *ON
      *
      *  Check sum of WMRFP BD/CR fields = ELBDCR
      *
     C                   Z-ADD     *ZEROS        ZZDBCD            5 2
     C                   Z-ADD     *ZEROS        ZZDBCC            5 2
     C                   Z-ADD     *ZEROS        ZZTOTC            5 4
     C                   Z-ADD     *ZEROS        ZZTOTD            5 4
     C                   Z-ADD     *ZEROS        ZZDBCR            5 4
      *
     C                   ADD       RFPRCA        ZZDBCC
     C                   ADD       RFPRDA        ZZDBCD
     C                   ADD       RFCPCA        ZZDBCC
     C                   ADD       RFCPDA        ZZDBCD
     C                   ADD       RFMFCA        ZZDBCC
     C                   ADD       RFMFDA        ZZDBCD
     C                   ADD       RFSDCA        ZZDBCC
     C                   ADD       RFSDDA        ZZDBCD
     C                   ADD       RFEMCA        ZZDBCC
     C                   ADD       RFEMDA        ZZDBCD
     C                   ADD       RFMCCA        ZZDBCC
     C                   ADD       RFMCDA        ZZDBCD
     C                   ADD       RFMSCA        ZZDBCC
     C                   ADD       RFMSDA        ZZDBCD
      *
     C     ZZDBCC        IFNE      *ZEROS
     C     ZZDBCC        MULT      -1            ZZDBCC
     C     ZZDBCC        DIV       100           ZZTOTC                         Total Credits
     C                   ENDIF
     C     ZZDBCD        IFNE      *ZEROS
     C     ZZDBCD        DIV       100           ZZTOTD                         Total Debits
     C                   ENDIF
      *
     C     ZZTOTC        IFNE      *ZEROS
     C     ZZTOTD        ORNE      *ZEROS
     C     ZZTOTC        ADD       ZZTOTD        ZZDBCR
     C     ZZDBCR        ADD       1.000         ZZDBCR
     C                   ENDIF
      ***** END ADD ******* 07/13/07 **********************************
      *
     C     ELDBCR        IFNE      ZZDBCR
     C                   MOVE      *ON           *IN36
     C                   ENDIF                                                  RFPRTF <> "Y"
      *
      ***** BEG ADD ******* 07/23/07 **********************************
      * Check to see if Scheduled DB/CR form printed.
     C  N36RFPRTF        IFNE      'Y'
     C                   MOVE      *ON           *IN35                          RFPRTF <> "Y"
     C                   ENDIF                                                  *IN41 = *ON
      ***** END ADD ******* 07/23/07 **********************************
      *
      * If ELDBCR = ZZDBCR and RFPRTF = "Y" then update record.
     C     ELDBCR        IFEQ      ZZDBCR
     C     RFPRTF        ANDEQ     'Y'
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF                                                  ELDBCR<>ZZDBCR
      *
      ***** BEG ADD ***** 07/13/07 ************************************
     C                   ENDIF                                                  *IN41 = *ON
      ***** END ADD ***** 07/13/07 ************************************
      *
     C                   ENDIF                                                  ELKY - *IN60
      *
      *  If client is not 114, then update record.
     C     CLTID         WHENNE    '114'
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  SHKY - *IN60
      ************
      ***** END ADD ******* 04/04/07 **********************************
      ***** BEG DEL ******* 04/04/07 **********************************
     C************         MOVE '2'       SHSTAT
     C**N60*******         UPDATPSH01
     C************         MOVE *BLANK    SCSTAT
     C************         MOVEL'RELEASED'SCSTAT
     C************         MOVE *BLANK    SCSEL
     C************         UPDATSCR01S
      ***** END DEL ******* 04/04/07 **********************************
      *
      ***** BEG ADD ******* 07/13/07 **********************************
     C     SHSTAT        IFEQ      '2'
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVEL     'RELEASED'    SCSTAT
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR01S
     C                   ENDIF                                                  SHSTAT = "2"
     C                   GOTO      CKSEL
      ***** END ADD ******* 07/13/07 **********************************
     C                   END                                                    END EQ '6'
     C*
     C*  ANY OTHER OPTIONS ARE INVALID.
     C*
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN21
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN20
     C                   GOTO      CKSEL
     C*
     C*  IF THE SUBFILE HAS NOT BEEN CHANGED, END THE PROGRAM
     C*
     C                   ELSE                                                   ELSE 0 *IN50
     C*
     C     *IN49         IFEQ      *OFF
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCEMPX        IFNE      *ZERO
      **** BEGIN ADD **** 05/03/96 ************************************
     C     SCPFDX        ORNE      *ZERO
      **** END   ADD **** 05/03/96 ************************************
     C                   GOTO      TOP
     C                   END                                                                     S
     C                   GOTO      TERM
     C*
     C                   ELSE
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   END                                                    END 0 *IN50
     C*** BEGIN ADD ***  11/21/96  ************************************
     C                   ENDIF                                                  Q2RECF IFEQ
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C* VALIDATE THE DARTS FLAG
     C*
     C     SCDART        IFNE      'Y'
     C     SCDART        ANDNE     'N'
     C                   MOVE      *ON           *IN53
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDART IFNE
     C*
     C* VALIDATE THE HARDCOPY FLAG
     C*
     C     SCHARD        IFNE      'Y'
     C     SCHARD        ANDNE     'N'
     C                   MOVE      *ON           *IN54
     C                   GOTO      STRT
     C                   ENDIF                                                  SCHARD IFNE
     C*
     C* VALIDATE THE 2ND COPY FLAG
     C*
     C     SCAGTO        IFNE      'Y'
     C     SCAGTO        ANDNE     'N'
     C                   MOVE      *ON           *IN55
     C                   GOTO      STRT
     C                   ENDIF                                                  SCAGTO IFNE
     C*** BEGIN DEL ***  04/09/98  ************************************
     C*
     C* IS DARTS IF 'Y' THEN HARDCOPY MUST BE 'Y'
     C*
     C*****      SCDART    IFEQ 'Y'
     C*****      SCHARD    ANDEQ'N'
     C*****                MOVE *ON       *IN57
     C*****                GOTO STRT
     C*****                ENDIF                           SCDART IFEQ
     C*** END DEL *****  04/09/98  ************************************
     C*** BEGIN ADD ***  04/10/98  ************************************
     C*
     C* BOTH DARTS AND HARDCOPY FLAGS CANNOT BE SET TO 'N'.
     C*
     C     SCDART        IFEQ      'N'
     C     SCHARD        ANDEQ     'N'
     C                   MOVE      *ON           *IN57
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDART IFEQ
     C*** END ADD *****  04/10/98  ************************************
     C*
     C* CANNOT HAVE A 2ND COPY WITH NO OUTPUT
     C*
     C     SCHARD        IFEQ      'N'
     C     SCAGTO        ANDEQ     'Y'
     C                   MOVE      *ON           *IN56
     C                   GOTO      STRT
     C                   ENDIF                                                  SCHARD IFEQ
      ***** BEG ADD ******* 04/24/01 **********************************
      *
      * Generate for Renewal, New business OR Both
      *
     C     SCRNB         IFNE      'R'
     C     SCRNB         ANDNE     'N'
     C     SCRNB         ANDNE     'B'
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF                                                  SCHARD IFEQ
      ***** END ADD ******* 04/24/01 **********************************
     C*** begin add ***  08/15/23  ************************************
     C                   Clear                   msBlocks
     C                   Clear                   erBlocks
     C                   If        scBlocks < 0
     C                   Eval      msBlocks = 'This field cannot be less' +
     C                              ' than zero.'
     C                   Eval      erBlocks = *on
     C                   Goto      Strt
     C                   Endif
     C*** end add *****  08/15/23  ************************************
      *** begin add ***  09/30/16  ***********************************
      /free
       xxSubmit = *on;

       If isUseScript;

          xxProcType = processType;

          xxMass = 'Y';

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) (xxCo#:xxFnd) pex00;
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
     C*** begin chg ***  08/15/23  ************************************
     C********           GOTO      SUBMIT
      /Free
       //* Update the records with a timestamp to allow them to be run
       //* in blocks and to reset them if Scriptura fails.

       myStamp = %subst(%char(%timestamp():*iso0):1:20);
       count = 0;
       //* Submit all into one batch if no value entered.
       If scBlocks = 0;
          scBlocks = *hival;
       Endif;

       Setll (xxco#:xxfnd) psh12;
       Reade (xxco#:xxfnd) psh12;
       Dow not %eof;
          //* Assign timestamp value if set to release status and submit job
          //* if maximum number of policy pages reached.
          If SHStat = '2';
             SHRun = myStamp;
             Update psh12;
             count += 1;
             If count = scBlocks;
                Exsr SubmitJob;
                myStampInt = %dec(myStamp:20:0);
                myStampInt = myStampInt + 1;
                myStamp = %editc(myStampInt:'X');
                count = 0;
             Endif;
          Endif;
          Reade (xxco#:xxfnd) psh12;
       Enddo;

       //* Submit last batch of policy pages.
       If count <> 0;
          Exsr SubmitJob;
       Endif;

      /end-free
     C*** end chg *****  08/15/23  ************************************
     C*
     C                   ENDIF                                                  Q2RECF IFEQ
     C*** END ADD *****  11/21/96  ************************************
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANK        SCSEL
     C*
     C                   Z-ADD     SVEMP#        XXEMP#
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   MOVE      SVPFDT        XXPFDT
      **** END   ADD **** 05/03/96 ************************************
     C     SHKY          SETGT     PSH01
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UP TO 10 LINES
     C*
      ***** BEG CHG ******* 11/11/03 **********************************
     C**********           DO   11        X       50
     C                   DO        10            X                 5 0
      ***** END CHG ******* 11/11/03 **********************************
     C*
     C     SHKY1         READE(N)  PSH01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C*
     C     X             IFEQ      1
     C     *IN21         ANDEQ     *OFF
     C                   Z-ADD     RRN2          HHREC#
     C                   Z-ADD     RRN2          XXLAST            5 0
     C                   ENDIF
     C*
     C                   MOVEL     SHEMP#        SCEMP#
      **** BEGIN CHG **** 09/04/98 *********************************************
     C******************** MOVELSHEMP#    SVEMP#  50
     C                   MOVEL     SHEMP#        SVEMP#
      **** END   CHG **** 09/04/98 *********************************************
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     SHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDT
     C                   MOVE      SHPFDT        SVPFDT
      **** END   ADD **** 05/03/96 ************************************
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVE      *BLANK        SCSTAT
     C     SHSTAT        IFEQ      '1'
     C                   MOVEL     'HELD'        SCSTAT
     C                   ELSE
     C                   MOVEL     'RELEASED'    SCSTAT
     C                   ENDIF
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *ZERO         SCEMP#
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   MOVE      0             SCPFDT
      **** END   ADD **** 05/03/96 ************************************
     C                   MOVEL     *BLANK        SCENAM
     C                   MOVE      *BLANK        SCSTAT
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             XXLAST
     C                   Z-ADD     1             HHREC#
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*  SAVE LAST WRITTEN RECORD NUMBER
     C*
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C*
     C                   MOVEL     *ZERO         SCEMPX
      **** BEGIN ADD **** 05/03/96 ************************************
     C                   Z-ADD     0             SCPFDX
      **** END   ADD **** 05/03/96 ************************************
     C*
     C                   ENDSR
      **** BEG ADD ***** 09/01/04 *************************************
     C*====================================================================*
     C* LOAD THE INFO MSG SUBFILE FOR POLICIES THAT HAVE A VOL LABOR CHARGE
     C*====================================================================*
     C     LODVLC        BEGSR
      *
     C                   ADD       1             RRN3
     C                   Z-ADD     SHEMP#        S3EMP#
     C                   Z-ADD     SHDIV         S3DIV
     C                   Z-ADD     SHFYR         S3FYR
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        S3NAME
     C                   WRITE     SCR03S
      *
     C                   ENDSR                                                  LODVLC
      **** EDG ADD ***** 09/01/04 *************************************
     C*** begin add ***  08/15/23  ************************************
     C*====================================================================*
     C* Submit the policy pages
     C*====================================================================*
     C     SubmitJob     Begsr
     C*
     C*   CALL THE CLP TO SUBMIT THE POLICY PAGES.
     C*
     C                   Move      XXCo#         QQCO#             3
     C                   Move      XXFnd         QQFND             3
     C                   Move      *ZERO         QQEMP#            9
     C                   Move      *ZERO         QQDIV             5
     C                   Move      *BLANK        QQREIS            1
     C                   Move      UPOutQ        QQQUE            10
     C                   Move      '0'           QQSTEP            1
     C                   Move      scDart        QQDart            1
     C                   Move      scHard        QQHard            1
     C                   Move      scAgtO        QQAgtO            1
     C                   Move      scRNB         QQRNB             1
     C                   Move      myStamp       QQStamp          20
     C
     C                   Call      'W7415C'
     C                   Parm                    QQCO#
     C                   Parm                    QQFND
     C                   Parm                    QQEMP#
     C                   Parm                    QQDIV
     C                   Parm                    QQREIS
     C                   Parm                    QQQUE
     C                   Parm                    QQSTEP
     C                   Parm                    QQDART
     C                   Parm                    QQHARD
     C                   Parm                    QQAGTO
     C                   Parm                    QQRNB
     C                   Parm      *BLANKS       QQDMBR           10
     C                   Parm                    QQSTAMP
     C
     C                   Endsr                                                  SubmitJob
     C*** end add *****  08/15/23  ************************************
