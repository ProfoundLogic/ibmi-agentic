     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H* ***************************************************************
     H/TITLE W4110R - GENERATE CLAIM DEPARTMENT LETTERS - SCRN PGM    *
     H* ***************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for the required information    *
     H* in order to submit a job to print a Claim Dept. Letter.       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     MENU                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     W4110C                                                    *
     H*                                                               *
     H*  PROGRAMMER: C:30                                             *
     H*                                                               *
     H*  CREATED:    11/28/94               PROJECT: WC376            *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* SCREEN PROCESSING:                                            *
     H*                                                               *
     H* SCR01: FUND:            USER PARAMETER FUND                   *
     H*        FUND YEAR:       REQUIRED                              *
     H*            CLAIM:       REQUIRED                             *
     H*                                                               *
     H* THE DEPARMENT NAMES (AND SELECTION FIELDS) ARE PART OF A      *
     H* SUBFILE PROCESSED FROM FILE WMDEL.                            *
     H*                                                               *
     H*    SCR02:                                                     *
     H*                                                               *
     H*        CHOICE:             ADD IS ALLOWED ON INPUT ONLY       *
     H*        DOCUMENT ID:        VALUE FOUND IN FILE WMDOL          *
     H*        DOCUMENT DESC:      VALUE FOUND IN FILE WMDOL          *
     H*        ADDITIONAL INPUT:   VALUE FOUND IN DOAIFL IN WMDOL     *
     H*        # OF COPIES:        ON ADD (INPUT ONLY), VALUE IS      *
     H*                            WRITTEN TO WDCLL                   *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/02/15 C:300 Project: SY251 Support PHW34960               *
     H*                 Group is not displayed on Old Department      *
     H*                 letter module. So Adding the missing line     *
     H*                                                               *
     H*  04/08/15 C:65  Project: SY251                                *
     H*                 Call new SPILETTER API - this program will    *
     H*                 now be a shell for new process - only process *
     H*                 in this manner if Scriptura Module is enabled.*
     H*                                                               *
     H*  05/20/13 C:279 Redmine: 2857                                 *
     H*                 Modified to display all letters in subfile.   *
     H*                                                               *
     H*  10/17/11 C:277 Project: SY195                                *
     H*                 QDLS migration to IFS                         *
     H*                 Add two parameters with command               *
     H*                                                               *
     H*  04/15/10 C:232 SUPPORT: 72280                                *
     H*                 Fixed the issue of Page down.                 *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                                                               *
     H*  05/27/06  C:02 ADD "INTERACTIVE PROCESSING" INFO             *
     H*
     H*  10/28/98 C:02 ADDED FLAG TO PASS AS PARM TO SDSPDOC FOR
     H*                MS OFFICE INTEGRATION COMPATIBILITY
     H* 08/10/95 C:11 PROJECT WC376                                   *
     H*            1. FIX: EXFMT AFTER EACH ERROR ON SCRO2.           *
     H*            2. REMOVE "PREVIOUSLY SELECTED" FIELD ON SCR02.    *
     H*            3. ALLOW USER SPECIFY THE OUTQ FOR EACH DOCUMENT.  *
     H*               DEFAULT IT TO THE OUTQ IN USER'S PARM FILE.     *
     H*                                                               *
     H* 09/04/97 C:47 PROJECT WC577                                   *
     H*               UPDATED TO VERSION 3 SPECS.                     *
     H* ***************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* GLOBAL VARIABLES:                                             *
     H*  50    USED IN READE FOR SUB-FILE SCREENS                     *
     H*  60    USED TO CHAIN TO MANY DIFFERENT FILES                  *
     H*  61    USED TO CHAIN TO MANY DIFFERENT FILES                  *
     H*  94    USED TO DETERMINE WHETHER USER HAS ATTEMPTED A ROLL    *
     H*        DOWN AND NO PRIOR DEPARTMENT RECORDS EXIST.            *
     H*  95    USED TO DETERMINE IF PROGRAM IS INVOKED BY A CLAIMS    *
     H*        DEPARTMENT MANAGER                                     *
     H*  96    CALLED FROM CLAIM MASTER OPT 30                        *
     H* N96    CALLED FROM MENU OPTION                                *
     H*  99    USED TO DISPLAY DEPARTMENTS ON SCR01 BEGINNING WITH    *
     H*        THE DEPARTMENT SELETED FROM F7=CLM DEPT LTR RQST SRCH  *
     H* SCR01:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN          *
     H*        SEPARATOR ON SELECT FLAG FIELDS.                       *
     H*  20    INVALID FUND YEAR FOR FUND (WCR0517)                   *
     H*  21    INVALID FROM CLAIM NUMBER FOR FUND, FUND YEAR (WCR0681)*
     H*  26    CASE STATUS SELECT MUST BE O (OPEN), C (CLOSED) OR     *
     H*        A (ALL)  (WCR0881)                                     *
     H*  27    LOST TIME FLAG MUST BE L (LOST TIME), N (NO LOST TIME) *
     H*        OR A (ALL)  (WCR0882)                                  *
     H*  28    POSITION CURSOR TO FROM CLAIM                          *
     H*  29    POSITION CURSOR AT TO CLAIM                            *
     H*  33    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*        (SCM0510)                                              *
     H*  34    A SELECTION IS REQUIRED ON SCREEN ONE                  *
     H*        (WCR0833)                                              *
     H*  35    POSITION CURSOR TO FUND YEAR                           *
     H*  36    INVALID EMPLOYER FOR FUND YEAR SPECIFIED               *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*  99    DEPT CODE VALUE BROUGHT FROM REQUEST LETTER SEARCH     *
     H*        LOAD A '1' INTO CORRESPONDING SCSEL,                   *
     H*        (TURN ON BYPASS FLAG)                                  *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN          *
     H*        SEPARATOR ON SELECT FLAG FIELDS                        *
     H*  20    INVALID CHAR USED TO INDICATE A SELECTED RECORD        *
     H*        (SCM0510)                                              *
     H*  22    A SELECTION IS REQUIRED ON SCREEN TWO  (WCR0833)       *
     H*  23    NO SCREEN AVAILABLE FOR ADDITIONAL INPUT (WCR0834)     *
     H*  24    DOCUMENT NOT FOUND IN SPECIFIED FOLDER (SCM0516)       *
     H*  25    NOT AUTHORIZED TO DOCUMENT OR FOLDER (SCM0517)         *
     H*  26    USER NOT ENROLLED IN OFFICE/400 (SCM0518)              *
     H*  27    UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT (SCM0519)  *
     H*  28    INVALID OUTQ (MCR0509)                                 *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H* ***************************************************************
     FW4110F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN1)
     F                                     SFILE(SCR02S:RRN2)
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWDCLL     UF A E           K DISK
     FWDCLL3    O    E           K DISK
     F                                     RENAME(PCL01:PCL02)
     FWMCML1    IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
      *

     D**********          PTL     1   9 40                 PGM TITLES
     D CS1             S              1    DIM(7) ASCEND
     D CS2             S              1    DIM(7) ASCEND
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** begin add ***  04/08/15  *********************************************
     d xxRrn           s              9  0
     d xaRrn           s              9

      /copy SprLetter
      /copy sPrGenPop
     D*** end add *****  04/08/15  *********************************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  TYR                    1      4
     D                                     DIM(4)
     D  SCTYR1                 1      1
     D  SCTYR2                 2      2
     D  SCTYR3                 3      3
     D  SCTYR4                 4      4
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D*
     D*   ADDITIONAL PROGRAM PARMS
     D*
     D                 DS
     D  PARMS                  1    136
     D  QQCO#                  1      3
     D  QQFND                  4      6
     D  QQREQ#                 7     11
     D  QQDPCD                12     16
     D  QQDCID                17     28
     D  QQDESC                29     56
     D  QQFLDR                57    136
      *
      *** Beg add ***** 10/17/11 ***********************************************
     D RPGM1           C                   CONST('RTVDOCENV')
      *** End add ***** 10/17/11 ***********************************************
     D OPTNM           C                   CONST('GENERATE CLAIM -
     D                                     DEPARTMENT LETTERS')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

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
     I********      CLQTFL                          HIQTFL
     I              CLSTAT                      HISTAT
     I              CLIUSR                      HIIUSR
     I              CLUSER                      HIUSER
     I              CLPGM                       HIPGM
     I              CLIPGM                      HIIPGM
     I              CLDT                        HIDT
     I              CLTIME                      HITIME
     I              CLITIM                      HIITIM
     I              CLIDT                       HIIDT

     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*  DEPT LETTER SCREEN WORKFILE
     C*
     C     CLKEY         KLIST
     C                   KFLD                    HICO#
     C                   KFLD                    HIFND
     C*
     C     CLKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    SCDPCD
     C                   KFLD                    SCFLDR
     C                   KFLD                    SCDCID
     C*
     C     CLKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXFCLM
     C                   KFLD                    XXDPCD
     C                   KFLD                    XXREQ#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  DARTS REMARK FILE KEY LIST
     C*
     C     CMKY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*   COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  DARTS REMARK TYPE FILE KEY LIST
     C*
     C**         CTKY      KLIST
     C**                   KFLD           XXRMCD  5
     C*
     C*  DEPT MASTER FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    SCDPCD
     C*
     C     DEKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C*
     C*  DEPT/DOC ID MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    SCDPCD
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C*
     C*  FUND AUXILIARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C*
     C*  WORKERS' COMPENSATION APPLICATION CONTROL FILE
     C*
     C     WCKY1         KLIST
     C                   KFLD                    XXCNTL
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1            Q=QUOTE SYSTEM
     C                   PARM                    QQRTCD            1
     C                   PARM                    X3FND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCLM#            7
     C                   PARM                    QQDEPT            2
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*   INITIALIZE VARIABLE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFND         X1FND
     C     *LIKE         DEFINE    CMFYR         X1FYR
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMSS#         QQSS#
     C     *LIKE         DEFINE    DEDPCD        HLDPCD
     C     *LIKE         DEFINE    DEDPNM        HLDPNM
     C     *LIKE         DEFINE    DOFLDR        HLFLDR
     C     *LIKE         DEFINE    DODCID        HLDCID
     C     *LIKE         DEFINE    CLFYR         XXFYR
     C     *LIKE         DEFINE    CLFCLM        XXFCLM
     C     *LIKE         DEFINE    CLDPCD        XXDPCD
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C     *LIKE         DEFINE    WCKEY         XXCNTL
     C     *LIKE         DEFINE    DEDPCD        SVDPCD
     C*
     C*** begin add ***  04/08/15  *********************************************
     C*
     C                   CALL      'SA047R'
     C                   PARM      *BLANK        QQScrip           1
     C*
     C                   If        QQScrip = 'Y'
     C*
     C     Q1PRMS        IFEQ      1
     C*
     C                   Movel(p)  qqPrds        xaRRN
     C                   Testn                   xaRRN                60
     C*
     C                   If        *in60 = *on
     C*
     C                   Move      xaRRN         xxRRN
     C
      /free
                         DocWindow(xxRRN:'CS');
      /end-free
     C                   Endif
     C*
     C                   Else
      /free
       //***PopOneBtn('The mass functionality is not available for claim +
       //***           department letters yet.':'Department Letters':'Ok');
                         DocWindow(0:'CS');
      /end-free
     C                   Endif
     C
     C                   GOTO      Term
     C*
     C                   Endif
     C*** end add *****  04/08/15  *********************************************
     C     QQCALL        COMP      '2'                                    96
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C**** ADD 5/27/06 *********************************************************
     C*** CHECK IF DTM IS INSTALLED *** DTMINS FIELD HOLDS THIS VALUE **********
     C                   CALL      'DTMCHK'
     C                   PARM      *BLANKS       DTMINS            1
     C*
     C*** CHECK TO SEE THE DEFAULT EDITOR FOR THIS USER
     C*
     C                   CALL      'DA510R'
     C                   PARM      SCUSER        X$USER           10
     C                   PARM      *BLANKS       X$EDTR           10
     C                   PARM      *BLANKS       X$RTNC            1
     C*
     C*
     C**** END ADD 5/27/06 *****************************************************
     C*** Beg add ***** 10/17/11 **************************************
      * Find IFS or QDLS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
     C*
     C     ENV           IFEQ      'I'
     C                   MOVE      *BLANKS       X$EDTR
     C                   ENDIF
     C*** END add ***** 10/17/11 **************************************
     C**************       MOVE QQPRDS    SCPRDS
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      QQCALL        HLCALL            1
     C                   Z-ADD     XXFND         SCFND
     C                   MOVE      QQDEPT        XXDEPT
     C  N96              MOVE      '1'           *IN35
     C     *IN96         IFEQ      '1'
     C     QQFYR         ANDNE     *ZEROS
     C     *IN96         OREQ      '1'
     C     QQCLM#        ANDNE     *ZEROS
     C     *IN96         OREQ      '1'
     C     QQCLM#        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN30
     C                   END
     C*
     C*  SET INDICATOR THAT DETERMINES IF TO CLAIM VALUE, AND FUNCTIONS
     C*  SIX ON SCREENS ONE AND TWO ARE TO APPEAR.  THESE SHOULD ONLY
     C*  APPEAR ON A MANAGER'S SCREEN (I.E. A USER WITH A SECURITY LEVEL
     C*  DEFINED TO BE GREATER THAN OR EQUAL TO THE SECURITY LEVEL SET UP
     C*  IN THE WORKERS' COMPENSATION APPLICATION CONTROL FILE AS THE
     C*  SECURITY LEVEL FOR CLAIMS SERVICES MANAGERS)
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      '0'           *IN95
     C                   MOVE      'WC'          XXCNTL
     C     WCKY1         CHAIN     SWC01                              60
     C     *IN60         IFEQ      '0'
     C     UPLVL         ANDGE     WCSL01
     C                   MOVE      '1'           *IN95
     C                   END
     C                   MOVE      QQFYR         SCFYR
     C     QQCLM#        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       QQCLM#
     C                   END
     C                   MOVE      QQCLM#        SCFCLM
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C****** beg add ******* 06/02/15 *********************
     C                   MOVE      X3FND         SCFND
     C****** end add ******* 06/02/15 *********************
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   MOVE      *BLANK        UPOUTQ
     C                   END
     C*
     C     X3FND         IFEQ      *ZERO
     C                   MOVE      UPFND         SCFND
     C                   END
     C*
     C                   MOVE      SCFND         XXFND
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCFCLM        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   GOTO      TERM
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  INITIALIZE ROUTINE
     C*
     C                   EXSR      INIT1
     C                   EXSR      LOAD1                                        LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C*************        MOVELQ2FILE    SCFILE           SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
      /If Not Defined (WEB)
     C                   WRITE     SCR01
      /Endif
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      '1'           *IN01
      /If Not Defined (WEB)
     C                   WRITE     SCR02
      /Endif
     C                   EXFMT     SCR02C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C* *****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C* *****************************************************************
     C*
     C                   Select
     C* **************************************
     C*   F3 = EXIT                          *
     C* **************************************
     C     *INKC         Wheneq    '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
      * *******************************
      *   F10 = SCR01 - CLAIM # SEARCH      *
      * *******************************
     C     *INKJ         Wheneq    *ON
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
      *
     C                   Z-ADD     SCFYR         @@FYR
     C     SCFYR         IFEQ      *ZERO
     C                   Z-ADD     UPFYR         @@FYR
     C                   END
      *
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@FYR             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      'B'
     C     @@RRN         OREQ      *ZERO
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C                   MOVE      CMCASE        SCFCLM
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMSS#         QQSS#
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C*
     C   95              MOVE      '1'           *IN29
     C  N95              MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C                   END                                                    SCR01
     C* **************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C* **************************************
     C     *IN10         Wheneq    '1'
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
     C* **************************************
     C*   F12 = CANCEL                       *
     C* **************************************
     C     *INKL         Wheneq    '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      *BLANKS       QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C*
     C*****************    MOVE *ZEROS    SCFYR
     C*****************    MOVE *BLANKS   SCFCLM
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      *BLANKS       SVDPCD
     C                   MOVE      '1'           *IN35
     C                   GOTO      TOP
     C                   END
     C*
     C     Q2RECF        IFNE      'SCR01'
     C     Q2RECF        ANDNE     'SCR01C'
     C     Q2RECF        ANDNE     'SCR01S'
     C     Q2RECF        ANDNE     'SCR02'
     C     Q2RECF        ANDNE     'SCR02C'
     C     Q2RECF        ANDNE     'SCR02S'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   EXSR      INIT2
     C                   EXSR      LOAD2
     C                   GOTO      STRT
     C                   END
     C*
     C* **************************************
     C*  F13 = CHG USER PARAMETERS           *
     C* **************************************
     C     *INKM         Wheneq    '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   MOVE      *ZERO         UPFYR
     C                   ELSE
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C                   END
     C                   MOVE      *BLANKS       SCDPCD
     C                   GOTO      TOP
     C*
     C* **************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C* **************************************
     C     *IN18         Wheneq    '1'                                          ROLLUP
     C*
     C*   CHECK FOR END OF FILE (EITHER SCREEN)
     C*
     C     *IN40         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C* **********
     C*   SCR01  *
     C* **********
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      '0'           *IN94
     C                   EXSR      LOAD1
     C                   END                                                    END EQ SCR01
     C* **********
     C*   SCR02  *
     C* **********
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C                   Z-ADD     #@$2          RRN2
     C                   EXSR      LOAD2
     C                   END                                                    END EQ SCR02
     C*
     C                   GOTO      STRT
     C*
     C*  THIS CODE SEEMS TO EXECUTE ONLY IF THE USER TRIES TO ROLL PAST
     C*  THE BEGINNING OF THE SEARCH ITEMS, OTHERWISE THE SYSTEM HANDLES
     C*  THE ROLL DOWN FUNCTION INTERNALLY
     C*
     C* **************************************
     C*   *IN19 -  ROLLDOWN REQUEST          *
     C* **************************************
     C     *IN19         Wheneq    '1'                                          ROLLDOWN
     C* **********
     C*   SCR01  *
     C* **********
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C*
     C     *IN94         CABEQ     '1'           STRT                     41
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      '1'           *IN94
     C                   GOTO      TOP
     C*
     C                   END                                                    END EQ SCR01
     C*
     C                   Endsl                                                  END 1 *IN19 ROLLUP
     C*
     C* ***************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C* ***************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR01S'      SCR01T
     C     Q2RECF        CABEQ     'SCR01C'      SCR01T
     C*
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02S'      SCR02T
     C     Q2RECF        CABEQ     'SCR02C'      SCR02T
     C*
     C* **********************
     C*   PROCESS SCREEN 1   *
     C* **********************
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE INFORMATION ENTERED:
     C*  FUND YEAR MUST BE ENTERED
     C*
     C     SCFYR         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*
     C*  VALIDATE THE FROM CLAIM EXISTS
     C*
     C                   MOVE      SCFCLM        XXCASE
     C     CMKEY         CHAIN     PCM01                              21
     C     *IN21         CABEQ     '1'           STRT
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              61
     C                   MOVE      *BLANKS       XXCDNM           30
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   READC     SCR01S                                 50
     C*
     C*  CHECK FOR DEPARTMENT SELECTION
     C*
     C     NXTRD1        TAG
     C     *IN50         IFEQ      '0'
     C*
     C*  IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*  SELECT A RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C*
     C     SCSLCT        IFNE      '1'
     C     SCSLCT        ANDNE     ' '
     C                   MOVE      '1'           *IN33
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C*
     C*  IF THE SUBFILE HAS CHANGED BUT THE USER USED A BLANK,
     C*  READ NEXT CHANGED RECORD (NXTRD1 TAG)
     C*
     C     SCSLCT        IFEQ      ' '
     C*
     C*  IF THE USER BLANKED OUT A PREVIOUS SELECTION BE SURE TO
     C*  CLEAR THE VARIABLE SVDPCD THAT MAY HAVE BEEN SET IF F7 =
     C*  CLAIM LETTER REQUEST SEARCH WAS USED
     C*
     C     SCDPCD        IFEQ      SVDPCD
     C                   MOVE      *BLANKS       SVDPCD
     C                   END                                                    SCDPCD = SVDPCD
     C                   READC     SCR01S                                 50
     C                   GOTO      NXTRD1
     C                   END                                                    END NE ' '
     C*
     C*  IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*  SAVE THAT DATA TO BE PASSED TO NEXT SCREEN
     C*  PGMR NOTE:  ONLY 1ST DEPARTMENT WILL BE SELECTED & USED
     C*
     C     SCSLCT        IFEQ      '1'
     C*
     C*  LOAD SCR02 INFORMATION
     C*
     C                   EXSR      INIT2
     C                   EXSR      LOAD2
     C                   GOTO      STRT
     C*
     C                   END                                                    END EQ '1'
     C*
     C*  IF THE SUBFILE HAS NOT BEEN CHANGED
     C*
     C                   ELSE                                                   ELSE 0 *IN50
     C*
     C*  REDISPLAY SCR01 (SELECTION REQUIRED)
     C*
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN50
     C*
     C* **********************
     C*   PROCESS SCREEN 2   *
     C* **********************
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   MOVE      '0'           ERR               1
     C*
     C*  VALIDATE INFORMATION ENTERED:
     C*  CHECK FOR LETTER SELECTION
     C*
     C                   READC     SCR02S                                 50
     C*
     C     *IN50         IFEQ      '1'
     C*
     C*  SUBFILE HAS NOT BEEN CHANGED, REDISPLAY SCR02, SELECTION REQUIRED
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*   SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*   VERIFY SELECTION
     C*  IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A CORRECT VALUE
     C*  SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C*
     C     SCSEL2        IFNE      '1'                                          INVALID VALUE
     C     SCSEL2        ANDNE     '5'
     C     SCSEL2        ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C*
     C*  IF THE SUBFILE HAS CHANGED BUT THE USER USED A BLANK,
     C*  READ NEXT CHANGED RECORD (NXT2 TAG)
     C*
     C     SCSEL2        IFEQ      ' '
     C                   GOTO      NXT2
     C                   END                                                    END EQ ' '
     C*
     C*  IF THE SUBFILE HAS CHANGED AND USER HAS REQUESTED TO VIEW A DOCUMENT
     C*  VIEW DOCUMENT, THEN READ NEXT CHANGED RECORD (NXT2 TAG)
     C*
     C     SCSEL2        IFEQ      '5'
     C                   MOVEL     SCDCID        QQDCID
     C                   MOVEL     SCFLDR        QQFLDR
     C                   MOVE      *ZEROS        XXSQ#             5
     C                   MOVEL     *BLANKS       XXKY             40
     C                   MOVEL     *BLANKS       XXFL03            1
     C                   MOVEL     *BLANK        QQRTCD
     C** ADD 5/27/06 ****************************************************
     C*
     C     DTMINS        IFEQ      'Y'
     C     X$EDTR        ANDEQ     '*MSWORD'
     C                   CALL      'DTDSPDOC'
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C                   PARM                    X$EDTR
     C                   PARM      *BLANKS       QQRTCD
     C*
     C                   GOTO      BYPDT
     C*
     C                   ENDIF
     C** END ADD 5/27/06 ****************************************************
     C                   CALL      'SDSPDOC'
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    XXFL03
     C                   PARM                    QQRTCD
     C                   PARM      *BLANKS       QQMSFL            1
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANKS       QQUNCN          255
     C                   PARM      *BLANK        QQFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C*** ADDED ABOVE LINE FOR MS INTEGRATION COMPATIBILITY ** 10.28.98
     C** ADD 5/27/06 ****************************************************
     C     BYPDT         TAG
     C                   MOVEL     SCOUTQ        XX8A              8
     C     XX8A          IFEQ      '*INTERAC'
     C                   MOVE      *ON           *IN84
     C                   ELSE
     C                   MOVE      *OFF          *IN84
     C                   ENDIF
     C** END ADD 5/27/06 ****************************************************
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C                   MOVE      SCSEL2        HLSEL2            1
     C                   MOVE      *BLANKS       SCSEL2
     C                   UPDATE    SCR02S
     C                   GOTO      STRT
     C                   END                                                    END EQ '5'
     C*
     C*  IF ADD:
     C*  CHECK NEXT PROCESSING ON RECORD (I.E. VALIDATION, ADDITIONAL INPUT)
     C*
     C     SCSEL2        IFEQ      '1'
     C*
     C*  DOCUMENT OUTPUT QUEUE MUST BE VALID ON SYSTEM.
     C*
     C*** ADD 5/27/06 ******************************************************
     C                   MOVEL     SCOUTQ        XX8A              8
     C     XX8A          IFEQ      '*INTERAC'
     C                   MOVE      *ON           *IN84
     C                   ELSE
     C                   MOVE      *OFF          *IN84
     C                   ENDIF
     C     *IN84         IFEQ      *OFF
     C*** END ADD 5/27/06 ******************************************************
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XFOUND            1
     C     XFOUND        IFEQ      '1'
     C                   MOVE      *ON           *IN28                          ERROR
     C                   MOVE      *ON           *IN29                          RECORD CHANGED
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN29
     C                   GOTO      STRT
     C                   END
     C*** ADD 5/27/06 ******************************************************
     C                   ENDIF
     C*** END ADD 5/27/06 ******************************************************
     C*
     C*  IF THIS IS A NEW DOCUMENT REQUEST W/O ADDITIONAL INPUT,
     C*  WRITE RECORD IN WSDLP
     C*  THEN RETURN TO NXT2 TAG TO LOOK FOR OTHER CHOICES
     C*
     C     SCAINP        IFEQ      *BLANK
     C                   EXSR      WRTUPD
     C                   MOVE      SCSEL2        HLSEL2
     C                   MOVE      *BLANKS       SCSEL2
     C                   UPDATE    SCR02S
     C                   EXSR      SUBMIT
     C                   GOTO      NXT2
     C*
     C                   ELSE                                                   ELSE EQ SCAINP
     C*
     C*  A NEW REQUEST WITH ADDITIONAL INPUT WILL DISPLAY THE APPROPIRATE
     C*  SCREEN, WRITE A DOCUMENT REQUEST RECORD TO WSDLP AND RETURN TO
     C*  NXT2 TAG TO LOOK FOR OTHER CHOICES
     C*  PGMR NOTE:  SEE INDIVIDUAL ADDITIONAL INPUT SCREEN PROCESSING
     C*  IF NO SCREEN HAS BEEN INDICATED FOR THIS ADDTL INPUT,
     C*  SEND ERROR MESSAGE
     C*
     C     SCPGMN        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C                   MOVE      SCSEL2        HLSEL2
     C                   MOVE      *BLANKS       SCSEL2
     C*
     C*  CALL ADDITIONAL PROGRAM
     C*
     C                   EXSR      WRTUPD
     C                   EXSR      ADDPGM
     C*
     C     QQRTCD        IFEQ      'A'                                          F3 PRESSED
     C                   GOTO      TERM
     C                   END
     C*
     C     QQRTCD        IFEQ      'B'                                          F12 PRESSED
     C                   MOVE      *BLANK        SCOUTQ
     C                   MOVE      *ZERO         SC#CPY
     C                   ELSE
     C                   END
     C*
     C                   UPDATE    SCR02S
     C*
     C     QQRTCD        IFEQ      'B'                                          F12 PRESSED
     C                   MOVE      *BLANK        QQRTCD
     C                   ELSE
     C                   EXSR      SUBMIT
     C                   END
     C*
     C                   GOTO      NXT2
     C                   END                                                    END EQ SCAINP
     C*
     C                   END                                                    END EQ '1'
     C*
     C*  READ NEXT CHANGED RECORD
     C*
     C     NXT2          TAG
     C*
     C                   READC     SCR02S                                 50
     C                   END                                                    END DOWEQ *IN50
     C*
     C**         *IN96     IFEQ *OFF                       F12 PRESSED
     C**                   Z-ADD*ZERO     SCFYR
     C**                   Z-ADD*ZERO     SCFCLM
     C**                   END                             END DOWEQ *IN50
     C*
     C                   MOVE      *OFF          *IN50                          RESET
     C                   EXSR      INIT1
     C                   EXSR      LOAD1                                        LOAD RCDS FROM FILE
     C*
     C                   GOTO      STRT
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C* ***************************************************************
     C* INIT1  SUBROUTINE              ** Subroutine **   I N I T 1   *
     C* ***************************************************************
     C*
     C     INIT1         BEGSR
     C                   Z-ADD     0             RRN1              5 0
     C                   MOVE      *BLANKS       SCSLCT
     C                   MOVE      *BLANKS       HLDPCD
     C                   MOVE      *BLANKS       HLDPNM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     DEKEY         SETLL     PDE01                              60
     C*
     C*   CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   ENDSR
     C* ***************************************************************
     C* INIT2  SUBROUTINE              ** Subroutine **   I N I T 2   *
     C* ***************************************************************
     C*
     C     INIT2         BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C                   MOVE      *BLANKS       HLSEL2
     C                   MOVE      *BLANKS       SCSEL2
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *BLANKS       SCFLDR
     C                   MOVE      *BLANKS       SCDCID
     C*
     C* REPOSITION FILE
     C*
     C     DOKEY         SETLL     PDO01
     C*
     C* CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C*
     C                   ENDSR
     C* ***************************************************************
     C* INITAF  SUBROUTINE             ** Subroutine **   I N I T A F *
     C* ***************************************************************
     C     INITAF        BEGSR
     C                   CLEAR                   PCL02
     C                   ENDSR
     C*
     C* ***************************************************************
     C* LOAD1  SUBROUTINE              ** Subroutine **   L O A D 1   *
     C* ***************************************************************
     C*
     C     LOAD1         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN40
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UPTO 11 LINES
     C*
     C*** BEG CHG ****** 04/15/10 **********************************************
     C********             DO   11        X       50
     C                   MOVEL     00001         X                 5 0
     C                   MOVEL     00000         Y                 5 0
     C*** END CHG ****** 04/15/10 **********************************************
     C*
     C     DEKEY1        READE     PDE01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN40
     C                   END
     C*
     C*** BEG CHG ****** 04/15/10 **********************************************
     C     *IN60         DOWEQ     '0'
     C********   *IN60     IFEQ '0'
     C********   DESTAT    ANDEQ'A'
     C     DESTAT        IFEQ      'A'
     C*** END CHG ****** 04/15/10 **********************************************
     C                   ADD       1             RRN1
     C*** BEG ADD ****** 04/15/10 **********************************************
     C                   ADD       1             X
     C                   ADD       1             Y
     C*** END ADD ****** 04/15/10 **********************************************
     C                   MOVEL     DEDPCD        SCDPCD
     C                   MOVEL     DEDPNM        SCDPNM
     C                   MOVE      ' '           SCSLCT
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C*** BEG CHG ****** 04/15/10 **********************************************
     C     Y             IFEQ      11
     C                   LEAVE
     C                   ENDIF
     C********             ELSE
     C********             MOVE '1'       *IN05
     C*** END CHG ****** 04/15/10 **********************************************
     C                   END
     C*** BEG ADD ****** 04/15/10 **********************************************
     C     DEKEY1        READE     PDE01                                  60
     C*** END ADD ****** 04/15/10 **********************************************
     C                   END
     C*** BEG ADD ****** 04/15/10 **********************************************
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN05
     C                   ENDIF
     C*** END ADD ****** 04/15/10 **********************************************
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN1          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      *BLANKS       SCDPNM
     C                   Z-ADD     1             RRN1
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*  TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     RRN1          #@$1
     C*
     C                   ENDSR
     C* ***************************************************************
     C* LOAD2  SUBROUTINE              ** Subroutine **   L O A D 2   *
     C* ***************************************************************
     C*
     C     LOAD2         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN40
     C                   Z-ADD     0             X
     C                   MOVE      '0'           *IN60
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UPTO 12 LINES
     C*
     C****** beg chg ******* 05/20/13 *********************
     C*****X             DOWLT     12
     C******IN60         ANDEQ     '0'
     C     *IN60         DOWEQ     '0'
     C****** beg chg ******* 05/20/13 *********************
     C*
     C     NEXT          TAG
     C     DOKEY         READE     PDO01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     0
     C                   MOVE      '1'           *IN40
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     DOSTAT        ANDEQ     'A'
     C                   ADD       1             RRN2
     C                   ADD       1             X
     C                   MOVEL     DODCID        SCDCID
     C                   MOVEL     DOFLDR        SCFLDR
     C                   MOVEL     DODESC        SCDESC
     C                   MOVEL     DOPGMN        SCPGMN
     C     DOAIFL        IFEQ      'Y'
     C                   MOVEL     'YES'         SCAINP
     C                   ELSE                                                   ELSE Y DOAIFL
     C                   MOVEL     '   '         SCAINP
     C                   END                                                    END Y DOAIFL
     C*
     C                   MOVEL     DORMCD        SCRMCD
     C                   MOVE      *ZEROS        SC#CPY
     C                   MOVEL     UPOUTQ        SCOUTQ
     C**** ADD 5/27/06 *****************************************************
     C*** IF THE DOINTR FLAG IS SET TO "I" THEN THIS IS TO BE INTERACTIVE REQUES
     C***  REQUEST.  PUT THE "*INTERACT*" INTO THE OUTQ FIELD, SET INDICATOR TO
     C***  MAKE PROTECTED, (OUTQ AND #COPIES).  NO UNDERLINE, HIGHLIGHT?
     C***  WILL ALSO CHANGE THE EDITING FROM SCR02S TO NOT EDIT VALUES IF THE IN
     C***  INDICATOR IS ON (MEANING INTERACTIVE).     (ONLY IF DTM IS INSTALLED)
     C***
     C***
     C                   MOVE      *OFF          *IN84
     C     DOINTR        IFEQ      'I'
     C     DTMINS        ANDEQ     'Y'
     C                   MOVEL     '*INTERAC'    SCOUTQ
     C                   MOVE      'T*'          SCOUTQ
     C                   MOVE      *ON           *IN84
     C                   ENDIF
     C****
     C***
     C**
     C**** END ADD 5/27/06 *************************************************
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR02S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCSEL2
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANKS       SCFLDR
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPGMN
     C                   MOVE      *BLANKS       SCAINP
     C                   MOVE      *ZEROS        SC#CPY
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       SCOUTQ
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR02S
     C                   END
     C*
     C*  TO POSITION SCREEN AND CURSOR
     C*
     C***** beg chg ****** 05/20/13 ******************************
     C********           Z-ADD     RRN2          #@$2
     C                   Eval      #@$2 = 1
     C***** end chg ****** 05/20/13 ******************************
     C*
     C                   ENDSR
     C*
     C* ***************************************************************
     C* WRTUPD SUBROUTINE              ** Subroutine **   W R T U P D *
     C* ***************************************************************
     C     WRTUPD        BEGSR
     C*
     C*  CALL CNTROL TO OBTAIN A 'REQUEST NUMBER'
     C*
     C                   EXSR      CNTROL
     C*
     C*  IF THIS IS A NEW REQUEST, INITIALIZE WORKFILE FIELDS
     C*
     C                   EXSR      INITAF
     C*
     C                   Z-ADD     UPCO#         CLCO#
     C                   Z-ADD     XXFND         CLFND
     C                   Z-ADD     SCFYR         CLFYR
     C                   MOVE      SCFCLM        CLFCLM
     C                   Z-ADD     XXREQ#        CLREQ#
     C                   Z-ADD     CMEMP#        CLEMP#
     C                   Z-ADD     CMDIV         CLDIV
     C*
     C                   MOVE      SCDPCD        CLDPCD
     C                   MOVE      SCFLDR        CLFLDR
     C                   MOVE      SCDCID        CLDCID
     C     SC#CPY        IFNE      *ZEROS
     C                   Z-ADD     SC#CPY        CL#CPY
     C                   ELSE
     C                   Z-ADD     1             CL#CPY
     C                   END
     C************         MOVE HLCALL    CLQTFL
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
     C                   MOVE      SCOUTQ        CLOUTQ
     C**** ADD 5/27/06 *************************************************
     C                   MOVEL     CLOUTQ        XX8A              8
     C     XX8A          IFEQ      '*INTERAC'
     C                   MOVEL     'QPRINT  '    CLOUTQ
     C                   MOVE      '  '          CLOUTQ
     C                   ENDIF
     C**** END ADD 5/27/06 *************************************************
     C*
     C                   WRITE     PCL02
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* ADDPGM SUBROUTINE              ** Subroutine **   A D D P G M *
     C*****************************************************************
     C     ADDPGM        BEGSR
     C*
     C*   ASSIGN FIELDS TO PARMS DS, THEN CALL ADDITIONAL PROGRAM
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXREQ#        QQREQ#
     C                   MOVE      SCDPCD        QQDPCD
     C                   MOVE      SCDCID        QQDCID
     C                   MOVE      SCDESC        QQDESC
     C                   MOVE      SCFLDR        QQFLDR
     C*
     C                   CALL      SCPGMN
     C                   PARM                    QQPRDS
     C                   PARM                    PARMS
     C                   PARM                    XXDEPT
     C                   PARM                    QQRTCD
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SUBMIT SUBROUTINE              ** Subroutine **   S U B M I T *
     C*****************************************************************
     C     SUBMIT        BEGSR
     C*
     C*   ASSIGN PARM FIELDS, THEN CALL SUBMITTER CLP
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXREQ#        QQREQ#
     C                   MOVE      SCDPCD        QQDPCD
     C                   MOVE      SCFLDR        QQFLDR
     C                   MOVE      SCDCID        QQDCID
     C*
     C                   CALL      'W4110C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQREQ#
     C                   PARM                    QQDPCD
     C                   PARM                    QQFLDR
     C                   PARM                    QQDCID
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* CNTROL - SUBROUTINE TO MAINTAIN A UNIQUE REQUEST NUMBER
     C*****************************************************************
     C     CNTROL        BEGSR
     C*
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C*
     C     CLKEY         CHAIN     PCL01                              65
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
     C**********           MOVE *BLANKS   HIQTFL
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
     C********************************************************
      **  TABLE PTL - PROGRAM TITLES         ¦
      *    DEPARTMENT MASTER MAINTENANCE
      *   DEPT/FOLDER/DOC ID MAINTENANCE
      *  DEPARTMENT LETTER REQUEST SEARCH
      *         CLAIM NUMBER SEARCH
      *           SS#/CASE SEARCH
      *         CLAIM ALPHA SEARCH
      *       EMPLOYER NUMBER SEARCH
      *        EMPLOYER ALPHA SEARCH
      *   PAYEE DEMOGRAPHICS ALPHA SEARCH
