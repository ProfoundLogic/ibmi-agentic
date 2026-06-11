     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0001R - COMPANY AUXILIARY FILE MAINTENANCE
     H*****************************************************************
     H* PROGRAM: F0001R                                               *
     H* DESCRIPTION: COMPANY AUXILIARY FILE MAINTENANCE               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  02/22/00 C:79  Support: MC048/#21939                         *
     H*                 Use CHKSTR to conditionally display location  *
     H*                 code field                                    *
     H*                                                               *
     H*  10/31/99 C:117 PROJ: LA014  LA DATA SUBMISSION               *
     H*                 ADDING NEW FIELD SCDCON (DATA CONTRIBUTOR)    *
     H*                                                               *
     H*  10/29/99 C:53  REQ/PROJ: MCMNT                               *
     H*                 REMOVE LAST PROV INT #                        *
     H*                                                               *
     H*  06/24/98 C:53  REQ/PROJ: MC145                               *
     H*                 SWITCH OLD NETWORK/AR FLAG TO NEW A/R FLAG    *
     H*                                                               *
     H*  03/04/98 C:31  Project: MC145                                *
     H*                 Yr 2000, RISC Development                     *
     H*                 1. Correct indicator 99 usage                 *
     H*                                                               *
     H* 12/02/96 C:59 Req/Proj: 203/FS134                             *
     H*               Call to program CHKSTR to verify: if            *
     H*               State is installed.  Returns '1' If found       *
     H*               and '0' if not found.                           *
     H*                                                               *
     H* 09/27/96 C:05 REQ/PROJ: MCMNT                                 *
     H*               Chgd indicator used for Location Code           *
     H*               from *in40 to *in45 because *in40 is            *
     H*               used to determine ADD vs. UPDATE on             *
     H*               primary file FMC1P.                             *
     H*                                                               *
     H* 08/14/95 C:33 REQ/PROJ: TX002                                 *
     H*               ADD LOCATION CODE TO SCR02                      *
     H*                                                               *
     H* 07/26/95 C:20 REQ/PROJ: 25MAE                                 *
     H*               1. ADD PRE-AUDI AND POST-AUDIT CSC TO           *
     H*                  SCREEN TWO.                                  *
     H*                                                               *
     H* 12/01/93 C:20 PROJ: MCMNT                                     *
     H*               REMOVE CONDITION THAT NETWORK HAS TO BE         *
     H*               INSTALLED TO DISPLAY PROV INT#.                 *
     H*                                                               *
     H* 08/24/92 KRR  PROJ: 99AAA                                     *
     H*               ADDED LAST INTERNAL PROVIDER NUMBER             *
     H*                                                               *
     H* 10/14/91 LJD: PER FS070/0021/BL:                              *
     H*               * ALLOW COMPANY NAME TO CHANGED                 *
     H*                                                               *
     H* 08/05/91 TMN -ADD LAST INVOICE NUMBER USED & PROTECT          *
     H*               IT ON SCREEN WITH RESTRICTED STATUS.            *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H* SCR01:                                                        *
     H*  20          COMPANY REQUIRED                                 *
     H*  21          INVALID COMPANY                                  *
     H* SCR02:                                                        *
     H*  09          INQUIRY MODE (QQCALL = 1)                        *
     H*  20          ADDR 1 REQUIRED                                  *
     H*  21          CITY REQUIRED                                    *
     H*  22          INVALID STATE                                    *
     H*  23          ZIP CODE 1 REQUIRED                              *
     H*  24          TELEPHONE REQUIRED                               *
     H*  25          TAX ID REQURIED                                  *
     H*  26          ANESTHESIA RATE REQUIRED                         *
     H*  27          LAST CASE NUMBER REQUIRED                        *
     H*  28          LAST EOMB NUMBER REQUIRED                        *
     H*  29          LAST LETTER NUMBER REQUIRED                      *
     H*  30  (W/81)  SUBMITTER/CARRIER CODE REQUIRED                  *
     H*  31  (W/83)  TAPE DEVICE NAME REQUIRED                        *
     H*  32  (W/83)  TAPE VOLUME ID REQUIRED                          *
     H*  33  (W/83)  TAPE DENSITY REQUIRED                            *
     H*  34  (W/83)  TAPE DEVICE NAME INVALID                         *
     H*  45  (W/82)  LOCATION CODE REQUIRED                           *
     H*  35          LAST INVOICE NUMBER REQUIRED                     *
     H*  36          COMPANY NAME REQUIRED             MCR0503        *
     H*  38          INVALID CSC                                      *
     H*  39          INVALID CSC                                      *
     H*  71          WC INTERFACE INSTALLED                           *
     H*  74          PPO/NETWORK INSTALL                              *
     H* N74          NON DISPLAY LAST PROV. INT# USED.                *
     H*  81          FL TAPE SUBMISSION INSTALLED                     *
     H*  82          TX TAPE SUBMISSION INSTALLED                     *
     H* N83          NONDISPLAY TAPE SUBMISSION FIELDS                *
     H*  91          INVOICING INSTALLED                              *
     H* N91          NONDISPLAY LAST INVOICE NUMBER FIELD             *
     H*  98          CHKSTR not found                                 *
     H*  99          PROTECT LAST CASE/EOMB (QQCALL = 2)              *
     H*****************************************************************
     FF0001F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFMC1L     UF A E           K DISK
     F*** BEGIN DEL *** 08/15/00 **********************************************
     F**********SSYLF   IF  E           K        DISK
     F**********  SCS01                             KIGNORE
     F**********  SRC01                             KIGNORE
     F*** END   DEL *** 08/15/00 **********************************************
     F*** BEGIN ADD *** 07/26/95 **********************************************
     FFTABL     IF   E           K DISK
     F*** END ADD *** 07/26/95 *************************************************
     F*** BEG ADD *** 10/31/99 ************************************************
     FSTSTL     IF   E           K DISK
     F*** END ADD *** 10/31/99 *************************************************

     D*
     D PTL             S             40    DIM(1) CTDATA PERRCD(1)              CALLED PGM TITLES
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 04/23/91 *************************************
     D                 DS
     D*** BEGIN CHG *** 08/15/00 **********************************************
     D**********                              1  20 TP
     D**********                              1   2 FBTP01
     D**********                              3   4 FBTP02
     D**********                              5   6 FBTP03
     D**********                              7   8 FBTP04
     D**********                              9  10 FBTP05
     D**********                             11  12 FBTP06
     D**********                             13  14 FBTP07
     D**********                             15  16 FBTP08
     D**********                             17  18 FBTP09
     D**********                             19  20 FBTP10
     D  TP                     1     20
     D                                     DIM(10)                              ST TAPE FROM SFBPF
     D  ##TP01                 1      2
     D  ##TP02                 3      4
     D  ##TP03                 5      6
     D  ##TP04                 7      8
     D  ##TP05                 9     10
     D  ##TP06                11     12
     D  ##TP07                13     14
     D  ##TP08                15     16
     D  ##TP09                17     18
     D  ##TP10                19     20
     D*** END   CHG *** 08/15/00 **********************************************
     D*** END ADD ***** 04/23/91 *************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*** BEGIN ADD *** 04/23/91 *************************************
     C*  FS BASE CONTROL FILE
     C*** BEGIN DEL *** 08/15/00 **********************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **********************************************
     C*** END ADD ***** 04/23/91 *************************************
     C*
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*** END ADD *** 07/26/95 *************************************************
     C*** BEG ADD *** 10/31/99 *************************************************
     C*
     C*  STATE CODE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*** END ADD *** 10/31/99 *************************************************
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
     C* INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    COCO#         XXCO#
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVE      *BLANKS       XX2               2
     C*** END ADD *** 07/26/95 *************************************************
     C*** BEGIN ADD *** 08/15/00 ***********************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C*** END   ADD *** 08/15/00 ***********************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C     QQCALL        COMP      '1'                                    09
     C     QQCALL        COMP      '2'                                    99     PROTECT CSE/EOB
     C*
     C*** BEGIN ADD *** 04/23/91 *************************************
     C*** BEGIN DEL *** 08/15/00 **********************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C*** END   DEL *** 08/15/00 **********************************************
     C                   MOVE      'FL'          FLTAPE            2
     C***  BEGIN ADD  ***  08/14/95  *********************************
     C                   MOVE      'TX'          TXTAPE            2
     C***  END   ADD  ***  08/14/95  *********************************
     C                   Z-ADD     1             X                 2 0
     C*** BEGIN DEL *** 08/15/00 **********************************************
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **********************************************
     C*** DETERMINE IF INTERFACED WITH WC  **********
     C*** BEGIN CHG *** 08/15/00 **********************************************
     C********** FBFL01    IFEQ 'Y'
     C     ##WCIN        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **********************************************
     C                   MOVE      '1'           *IN71
     C                   ELSE                                                   ELSE Y FBFL01
     C                   MOVE      '0'           *IN71
     C                   END                                                    END Y FBFL01
     C*** DETERMINE IF FLORIDA TAPE SUBMISSION IS INSTALLED **********
     C*
     C*** BEGIN ADD *** 12/02/96 **************************************
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C*
     C                   MOVEL     FLTAPE        QQSTT
     C*** BEGIN CHG *** 03/04/98 **************************************
     C********             CALL 'CHKSTR'               99
     C                   CALL      'CHKSTR'                             98
     C*** END CHG *** 03/04/98 ****************************************
     C                   PARM                    QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C*** BEGIN CHG *** 03/04/98 **************************************
     C********   *IN99     IFEQ '0'
     C     *IN98         IFEQ      '0'
     C*** END CHG *** 03/04/98 ****************************************
     C     QQSTRT        IFEQ      '1'
     C                   MOVE      '1'           *IN81
     C                   ELSE                                                   ELSE *IN60 LOKUP TP
     C                   MOVE      '0'           *IN81
     C                   ENDIF                                                  END LOKUP ST
     C                   ELSE
     C*** END ADD ***** 12/02/96 **************************************
     C     FLTAPE        LOOKUP    TP(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN81
     C                   ELSE                                                   ELSE *IN60 LOKUP TP
     C                   MOVE      '0'           *IN81
     C                   END                                                    END *IN60 LOKUP TP
     C*** BEGIN ADD *** 12/02/96 **************************************
     C                   ENDIF                                                  END *IN98
     C*** END ADD ***** 12/02/96 **************************************
     C*
     C*** BEG DEL *** 02/22/00 ****************************************
     C***  BEGIN ADD  ***  08/14/95  *********************************
     C*** DETERMINE IF TEXAS TAPE SUBMISSION IS INSTALLED ************
     C********** TXTAPE    LOKUPTP,X                     60
     C********** *IN60     IFEQ '1'
     C**********           MOVE '1'       *IN82
     C**********           ELSE                            ELSE *IN60 LOKUP TP
     C**********           MOVE '0'       *IN82
     C**********           END                             END *IN60 LOKUP TP
     C*** END DEL *** 02/22/00 ****************************************
     C*** BEG ADD *** 02/22/00 ****************************************
     C                   CALL      'CHKSTR'                             98
     C*
     C                   PARM      TXTAPE        QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C*
     C                   MOVEL     QQFLRT        XXTAPE            1
     C*
     C     XXTAPE        IFEQ      'Y'
     C                   MOVE      '1'           *IN82
     C                   ELSE
     C                   MOVE      '0'           *IN82
     C                   END
     C*** END ADD *** 02/22/00 ****************************************
     C*** DETERMINE IF ANY TAPE SUBMISSION IS INSTALLED **************
     C     *IN82         IFEQ      '0'
     C     *IN81         ANDEQ     '0'
     C                   MOVE      '0'           *IN83
     C                   ELSE
     C                   MOVE      '1'           *IN83
     C                   END
     C***  END   ADD  ***  08/14/95  *********************************
     C*** BEG ADD *** 10/31/99 ****************************************
     C*
     C*  CHECKING TO SEE IF ST1A3 = 'Y' FOR LA DATA SUBMISSION
     C*
     C                   MOVEL     'LA'          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        48
     C                   ELSE
     C                   SETOFF                                       48
     C                   ENDIF
     C*** END ADD *** 10/31/99 ****************************************
     C*
     C*** DETERMINE IF INVOICING INSTALLED **********
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C********** FBINVF    IFEQ 'Y'
     C**********           MOVE '1'       *IN91
     C**********           ELSE                            ELSE Y FBFL01
     C**********           MOVE '0'       *IN91
     C**********           END                             END Y FBFL01
     C**********           SUBSTFBAL04:5  XXAR    1        ADD 06/24/98
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** BEGIN CHG *** 08/15/00 **********************************************
     C********** XXAR      IFEQ 'Y'                        ADD 06/24/98
     C     ##AR          IFEQ      'Y'                                          ADD 06/24/98
     C*** END   CHG *** 08/15/00 **********************************************
     C********** FBFL06    IFEQ 'Y'                        DEL 06/24/98
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   END
     C*** BEGIN DEL *** 08/15/00 **********************************************
     C**********           END                             END *IN60 SFB01
     C*** END   DEL *** 08/15/00 **********************************************
     C*** END ADD ***** 04/23/91 *************************************
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C*
     C* ONLY DISPLAY THE RECON/PRE AND POST AUDIT CLAIM STATUS CODES IF SYSTEM
     C* CONTROL FLAG IS SET TO PROCESS RECON/AUDIT
     C*
     C*** BEGIN DEL *** 08/15/00 **********************************************
     C**********           Z-ADD10        X
     C********** 1         SUBSTFBAL01:X  XXAR    1    65
     C*** END   DEL *** 08/15/00 **********************************************
     C*** BEGIN CHG *** 08/15/00 **********************************************
     C********** XXAR      IFEQ 'Y'
     C     ##ARIN        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **********************************************
     C                   MOVE      '1'           *IN42
     C                   ELSE                                                   ELSE XXAR EQ 'Y'
     C                   MOVE      '0'           *IN42
     C                   END                                                    END XXAR EQ 'Y'
     C*** END ADD *** 07/26/95 *************************************************
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C***********************************************************
     C*   PROCESS ALL COMMAND KEYS
     C     *IN11         IFEQ      '1'
     C**************************************
     C*   COMMAND 03 = END OF JOB          *
     C**************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C***********************************
     C*  F10 = CLAIM STATUS CODE SEARCH *
     C***********************************
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   MOVEL     SCCSC1        XX2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        QQCSCD
     C                   CALL      'F2111R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCSCD            3
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQCSCD        SCCSC1
     C     QQRTCD        CABEQ     'A'           TERM
     C                   END
     C*
     C                   MOVEL     SCCSC2        XX2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        QQCSCD
     C                   CALL      'F2111R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCSCD            3
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQCSCD        SCCSC2
     C     QQRTCD        CABEQ     'A'           TERM
     C                   END
     C*
     C                   MOVEL     SCCSC3        XX2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        QQCSCD
     C                   CALL      'F2111R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCSCD            3
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQCSCD        SCCSC3
     C     QQRTCD        CABEQ     'A'           TERM
     C                   END
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*** END ADD *** 07/26/95 *************************************************
     C**************************************
     C*   COMMAND 12 = PRIOR SCREEN        *
     C**************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C**************************************
     C*   COMMAND 06 = COMPANY SEARCH      *
     C**************************************
     C     *INKF         IFEQ      '1'
     C                   MOVE      *BLANKS       QQPRDS
     C                   MOVEL     PTL(1)        QQPRDS
     C                   CALL      'S0501R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCCO#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCO#         OREQ      *ZERO
     C                   WRITE     SCR03
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END *INKF
     C******
     C                   END                                                    END CMDKEYS
     C***********************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C***************************************
     C*   SCR01T - PROCESS REQUEST SCREEN   *
     C***************************************
     C     SCR01T        TAG
     C*
     C*  COMPANY NUMBER MUST BE GREATER THAN ZERO
     C*
     C     SCCO#         IFLE      *ZERO
     C*
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  FIRST GOTO THE COMPANY MASTER FILE.
     C*
     C                   MOVE      SCCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              40
     C*
     C*  COMPANY NUMBER MUST BE A VALID NUMBER.
     C*
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     COKEY         CHAIN     PC101                              40
     C*
     C* UPDATE MODE
     C     *IN40         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     C1NAME        SCNAME                         ADD 10/14/91
     C                   MOVEL     C1ADR1        SCADR1
     C                   MOVEL     C1ADR2        SCADR2
     C                   MOVEL     C1CTY         SCCTY
     C                   MOVEL     C1ST          SCST
     C                   MOVEL     C1ZIP         SCZIP1
     C                   MOVE      C1ZIP         SCZIP2
     C                   Z-ADD     C1AREA        SCAREA
     C                   Z-ADD     C1PHN#        SCPHN#
     C                   Z-ADD     C1FEIN        SCFEIN
     C                   Z-ADD     C1ANER        SCANER
     C                   Z-ADD     C1LCS#        SCLCS#
     C                   Z-ADD     C1EOB#        SCEOB#
     C*
     C*** ADD NEXT LINE *** PEND LETTER ***************************
     C                   Z-ADD     C1LTR#        SCLTR#
     C*
     C*** ADD NEXT LINE *** INVOICE *** 08/05/91 ******************
     C                   Z-ADD     C1LIN#        SCLIN#                         LAST INVOICE# USED
     C*** BEG DEL *** 10/29/99 ************************************
     C***************      Z-ADDC1LPI#    SCLPI#           ADD 8/14/92 KRR
     C*** END DEL *** 10/29/99 ************************************
     C*
     C*** BEGIN ADD *** TAPE SUBMISSION ***************************
     C                   Z-ADD     C1SUBM        SCSUBM
     C                   MOVE      C1DEV         SCDEV
     C                   MOVE      C1VOL         SCVOL
     C                   Z-ADD     C1TAPE        SCTAPE
     C***  BEGIN ADD  ***  08/14/95  *********************************
     C                   MOVE      C1LOC         SCLOC
     C***  END   ADD  ***  08/14/95  *********************************
     C*** END ADD ***** TAPE SUBMISSION ***************************
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C                   MOVEL     C1CSC1        SCCSC1
     C                   MOVEL     C1CSC2        SCCSC2
     C                   MOVEL     C1CSC3        SCCSC3
     C*** END ADD *** 07/26/95 *************************************************
     C*** BEG ADD *** 10/31/99 ****************************************
     C     *IN48         IFEQ      *ON
     C                   MOVEL     C110A1        SCDCON
     C                   ENDIF
     C*** END ADD *** 10/31/99 ****************************************
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C* ADD MODE
     C                   MOVE      UA(1)         SCMODE
     C                   MOVEL     CONAME        SCNAME                         ADD 10/14/91
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   Z-ADD     *ZEROS        SCZIP1
     C                   Z-ADD     *ZEROS        SCZIP2
     C                   Z-ADD     *ZEROS        SCAREA
     C                   Z-ADD     *ZEROS        SCPHN#
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCANER
     C                   Z-ADD     *ZEROS        SCLCS#
     C                   Z-ADD     *ZEROS        SCEOB#
     C*
     C*** ADD NEXT LINE *** PEND LETTER ***************************
     C                   Z-ADD     *ZEROS        SCLTR#
     C*
     C*** ADD NEXT LINE *** INVOICE *** 08/02/91 ******************
     C                   Z-ADD     *ZEROS        SCLIN#                         LAST INVOICE# USED
     C*** BEG DEL *** 10/29/99 ************************************
     C***************      Z-ADD*ZEROS    SCLPI#           ADD 08/14/92 KRR
     C*** END DEL *** 10/29/99 ************************************
     C*
     C*** BEGIN ADD *** TAPE SUBMISSION ***************************
     C                   Z-ADD     *ZEROS        SCSUBM
     C                   MOVE      *BLANKS       SCDEV
     C                   MOVE      *BLANKS       SCVOL
     C                   Z-ADD     *ZEROS        SCTAPE
     C***  BEGIN ADD  ***  08/14/95  *********************************
     C                   MOVE      *BLANKS       SCLOC
     C***  END   ADD  ***  08/14/95  *********************************
     C*** END ADD ***** TAPE SUBMISSION ***************************
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C                   MOVE      *BLANKS       SCCSC1
     C                   MOVE      *BLANKS       SCCSC2
     C                   MOVE      *BLANKS       SCCSC3
     C*** END ADD *** 07/26/95 *************************************************
     C*** BEG ADD *** 10/31/99 ****************************************
     C   48              MOVE      *BLANKS       SCDCON
     C*** BEG ADD *** 10/31/99 ****************************************
     C*
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C***************************************
     C* SCR02T - PROCESS RECORD             *
     C***************************************
     C     SCR02T        TAG
     C*** BEG ADD *** 10/31/99 ****************************************
     C*
     C*  CHECKING TO SEE IF ST1A3 = 'Y' FOR LA DATA SUBMISSION
     C*
     C                   MOVEL     'LA'          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        48
     C                   ELSE
     C                   SETOFF                                       48
     C                   ENDIF
     C*** END ADD *** 10/31/99 ****************************************
     C*
     C*  NAME CANNOT BE BLANKS
     C     SCNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADDRESS 1 CANNOT BE BLANKS
     C     SCADR1        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  CITY CANNOT BE BLANKS
     C     SCCTY         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  STATE MUST BE VALID
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  ZIP CODE 1 CANNOT BE ZERO
     C     SCZIP1        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  PHONE NUMBER CANNOT BE ZERO
     C     SCPHN#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  TAX ID CANNOT BE ZERO
     C     SCFEIN        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C*
     C     *IN42         IFEQ      '1'
     C*
     C* THE RECON CLAIM STATUS CODE MUST BE VALID
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCSC1        XXKEY
     C     TBKEY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* THE PRE-AUDIT CLAIM STATUS CODE MUST BE VALID
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCSC2        XXKEY
     C     TBKEY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* THE POST-AUDIT CLAIM STATUS CODE MUST BE VALID
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCSC3        XXKEY
     C     TBKEY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END                                                    END *IN60 EQ '1'
     C*
     C                   END
     C*
     C*** END ADD *** 07/26/95 *************************************************
     C*
     C*  ANESTHESIA RATE CANNOT BE EQUAL TO ZERO
     C*********  SCANER    IFLE *ZERO
     C*********            MOVE '1'       *IN26
     C*********            GOTO STRT
     C*********            END
     C***
     C*** ONLY EDIT THIS FIELD IF NOT INTERFACED WITH WC ***
     C     *IN71         IFEQ      '0'
     C* LAST CASE NUMBER CANNOT BE ZERO
     C     SCLCS#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END 0 *IN71
     C*
     C*
     C*  LAST EOMB NUMBER CANNOT BE ZERO
     C     SCEOB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN ADD *** PEND LETTER *******************************
     C*  LAST LETTER NUMBER CANNOT BE ZERO
     C     SCLTR#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*** END ADD ***** PEND LETTER *******************************
     C*
     C*** BEGIN ADD *** INVOICE *** 08/05/91 **********************
     C*  LAST INVOICE NUMBER CANNOT BE ZERO
     C     *IN91         IFEQ      '1'
     C     *IN74         OREQ      '1'
     C     SCLIN#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** END ADD ***** INVOICE ***********************************
     C*** BEG DEL *** 10/29/99 ************************************
     C*** BEGIN ADD *** INTERNAL*** 08/24/92 **********************
     C***LAST*PROV INTERNAL NUMBER CANNOT BE ZERO
     C********** *IN74     IFEQ '1'
     C********** SCLPI#    IFEQ *ZEROS
     C**********           MOVE '1'       *IN37
     C**********           GOTO STRT
     C**********           END
     C**********           END
     C*** END ADD ***** INTERNAL***********************************
     C*** END DEL *** 10/29/99 ************************************
     C*
     C*** BEGIN ADD *** TAPE SUBMISSION ***************************
     C*** ONLY EDIT THESE FIELDS IF TAPE SUBMISSION IS INSTALLED ***
     C***  BEGIN ADD  ***  08/14/95  *********************************
     C     *IN83         IFEQ      '1'
     C***  END   ADD  ***  08/14/95  *********************************
     C     *IN81         IFEQ      '1'
     C*  SUBMITTER/CARRIER CODE IS REQUIRED
     C     SCSUBM        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C***  BEGIN ADD  ***  08/14/95  *********************************
     C                   END
     C*
     C     *IN82         IFEQ      '1'
     C*  LOCATION CODE IS REQUIRED
     C     SCLOC         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN45                          CHG 09/27/96
     C                   GOTO      STRT
     C                   END
     C                   END
     C***  END   ADD  ***  08/14/95  *********************************
     C*
     C*  TAPE DEVICE NAME IS REQUIRED
     C     SCDEV         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  TAPE DEVICE NAME IS INVALID
     C                   MOVE      ' '           QQRTC
     C                   CALL      'F0001C'
     C                   PARM                    SCDEV
     C                   PARM                    QQRTC
     C     QQRTC         IFEQ      '1'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C*  TAPE VOLUME ID IS REQUIRED
     C     SCVOL         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*  TAPE DENSITY IS REQUIRED
     C     SCTAPE        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END 1 *IN81
     C*** END ADD ***** TAPE SUBMISSION ***************************
     C*** BEG ADD *** 10/31/99 ****************************************
     C*
     C*  DATA CONTRIBUTOR CANNOT BE BLANK IF LA DATA SUBMISSION INST
     C     *IN48         IFEQ      *ON
     C     SCDCON        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN19
     C                   GOTO      STRT
     C                   END
     C*** END ADD *** 10/31/99 ****************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C                   Z-ADD     SCCO#         C1CO#
     C                   MOVEL     SCNAME        C1NAME
     C                   MOVEL     SCADR1        C1ADR1
     C                   MOVEL     SCADR2        C1ADR2
     C                   MOVEL     SCCTY         C1CTY
     C                   MOVEL     SCST          C1ST
     C                   MOVEL     SCZIP1        C1ZIP
     C                   MOVE      SCZIP2        C1ZIP
     C                   Z-ADD     SCAREA        C1AREA
     C                   Z-ADD     SCPHN#        C1PHN#
     C                   Z-ADD     SCFEIN        C1FEIN
     C                   Z-ADD     SCANER        C1ANER
     C                   MOVE      *BLANK        C1STAT
     C                   MOVEL     Q1USER        C1USER
     C                   MOVEL     Q1PGM         C1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        C1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DT
     C                   Z-ADD     SCLCS#        C1LCS#
     C                   Z-ADD     SCEOB#        C1EOB#
     C                   Z-ADD     SCLTR#        C1LTR#
     C*** ADD NEXT LINE *** INVOICE *** 08/05/91 ******************
     C                   Z-ADD     SCLIN#        C1LIN#
     C*** BEG DEL *** 10/29/99 ************************************
     C*****************    Z-ADDSCLPI#    C1LPI#           ADD 8/14/92 KRR
     C*** END DEL *** 10/29/99 ************************************
     C*
     C*** BEGIN ADD *** TAPE SUBMISSION ***************************
     C                   Z-ADD     SCSUBM        C1SUBM
     C                   MOVE      SCDEV         C1DEV
     C                   MOVE      SCVOL         C1VOL
     C                   Z-ADD     SCTAPE        C1TAPE
     C                   Z-ADD     *ZEROS        C1CRA#
     C                   Z-ADD     *ZEROS        C1AGA#
     C***  BEGIN ADD  ***  08/14/95  *********************************
     C                   MOVE      SCLOC         C1LOC
     C***  END   ADD  ***  08/14/95  *********************************
     C*** END ADD ***** TAPE SUBMISSION ***************************
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C                   MOVEL     SCCSC1        C1CSC1
     C                   MOVEL     SCCSC2        C1CSC2
     C                   MOVEL     SCCSC3        C1CSC3
     C*** END ADD *** 07/26/95 *************************************************
     C*** BEG ADD *** 10/31/99 ****************************************
     C   48              MOVEL     SCDCON        C110A1
     C*** END ADD *** 10/31/99 ****************************************
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PC101
     C                   ELSE
     C*** DELETE NEXT LINE *** INVOICE *** 08/05/91 ***************
     C**********           Z-ADD*ZEROS    C1LIN#
     C                   MOVEL     Q1USER        C1IUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1IDT
     C                   WRITE     PC101
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCCO#
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *========================================================================*
**  TABLE PTL - PROGRAM TITLES
         COMPANY MASTER SEARCH
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
