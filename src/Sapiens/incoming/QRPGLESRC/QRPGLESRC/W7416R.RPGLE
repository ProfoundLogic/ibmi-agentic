     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7416R - NCCI SUBMISSION HISTORY FUNCTION                *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    DISPLAYS ALL NCCI SUBMISSION HISTORY RECORDS PENDING       *
     H*    TRANSMISSION AND ALLOWS THE USER TO HOLD OR RELEASE RECORDS*
     H*    FOR TRANSMISSION.      ALSO USED TO DISPLAY TRANSMITTED    *
     H*    HISTORY RECORDS, PROVIDING AN OPTION TO REPRINT ON PAPER   *
     H*    FROM RESOLVED OUTPUT, IF OFFICE AND DARTS ARE INSTALLED.   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*      MENU                                                     *
     H*  CALLS TO:                                                    *
     H*      W7416C1- PRINT RESOLVED DOCUMENT                         *
     H*                                                               *
     H*    PROGRAMMER: C:24                                           *
     H*    CREATED:    7/11/94      PROJECT: 03AZK                    *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/17/11 C:278 PROJECT: SY195                                 *
     H*                QDLS MIGRATION TO IFS                          *
     H*                ADD TWO PARAMETERS WITH COMMAND.               *
     H*                                                               *
     H* 09/27/11 C:255 WA139 SEQ76195                                 *
     H*                Added code to display error msg when document  *
     H*                is not present or is of '.pdf' tupe.           *
     H*                                                               *
     H* 03/30/11 C:246 SEQ 76166  CLT:099                             *
     H*                Do not delete the Darts record in SDCMP file   *
     H*                When policy page submission History deleted.   *
     H*                Instead of deleting change the status to       *
     H*                inactive.                                      *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*  10    HELP KEY                                               *
     H*  11    VALID COMMAND KEY                                      *
     H*                                                               *
     H* SCR01:                                                        *
     H*                                                               *
     H*  21    WCR0505 - RECORD NOT FOUND FOR EMPLOYER #              *
     H*                                                               *
     H* SCR02:                                                        *
     H*                                                               *
     H*  01    SFLDSP, SFLDSPCTL                                      *
     H*  02    SFLINZ                                                 *
     H*  03    SFLDLT                                                 *
     H*  04    SFLCLR                                                 *
     H*  05    SFLEND                                                 *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY SELECT FIELDS   *
     H*  19    SFLNXTCHG                                              *
     H*  20    HIGHLIGHT RECORD IN ERROR                              *
     H*  21    INVALID OPTION SELECTED                                *
     H*  32    ROLL PAST FIRST OR LAST RECORD IN FILE                 *
     H*  48    AT LEAST ONE RECORD DELETION REQUESTED                 *
     H*  49    AT LEAST ONE NON-BLANK OPTION ENTERED                  *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*  51    SCM0516-DOCUMENT NOT FOUND IN SPECIFIED FOLDER         *
     H*  52    SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER           *
     H*  53    SCM0518-USER NOT ENROLLED IN OFFICE 400                *
     H*  54    SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT    *
     H*  70    SHOW PENDING TRANSMISSIONS                             *
     H* N70    SHOW COMPLETED TRANSMISSIONS                           *
     H*  80    REPRINT ENABLED - DARTS AND OFFICE ARE INSTALLED       *
     H*  85    DISPLAY A SINGLE EMPLOYER ONLY                         *
     H*                                                               *
     H* SCR03:                                                        *
     H*                                                               *
     H*  01    SFLDSP, SFLDSPCTL                                      *
     H*  02    SFLINZ                                                 *
     H*  03    SFLDLT                                                 *
     H*  04    SFLCLR                                                 *
     H*  06    SFLEND                                                 *
     H*  19    SFLNXTCHG                                              *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*    SMCOL (PCO01): CO                                          *
     H*    WMUPL (PUP01): USER                                        *
     H*    WMFNL (PFN01): CO/FND                                      *
     H*    WMSHL3  (PSH01): CO/FND/EMP#/TRDT      STAT = 3 OR 5       *
     H*    WMSHL4  (PSH02): CO/FND/EMP#/SBDT      STAT = 4            *
     H*    WMEML   (PEM01): CO/FND/EMP#/DIV                           *
     H*    SDCMLA  (PCM01): KEY/SEQ# , STAT ne I                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 04/30/01  C:65  PROJECT:114WAK                                *
     H*                 ADD OPTION 8 FOR STATE STATUS ON PENDING      *
     H*                 TRANSACTIONS ONLY.                            *
     H*                                                               *
     H* 08/10/99  C:118 PROJECT: WC577
     H*                 Make V3 screen chgs. Combine emp. alpha and
     H*                 numeric searches in one F10=Search.
     H*
     H* 05/03/96  C:35  PROJECT:WC525                                 *
     H*                 VARIOUS CHANGES TO MAKE CMD KEYS FUNCTION     *
     H*                 PROPERLY.                                     *
     H*                                                               *
     H* 08/13/96  C:35  PROJECT:WC525                                 *
     H*                 CORRECTIONS TO MAKE SELECT OPTIONS FUNCTION   *
     H*                 PROPERLY.                                     *
     H*                                                               *
     H*****************************************************************
     FW7416F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR02S:RRN2)
     F                                     SFILE(SCR03S:RRN3)
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMSHL3    UF   E           K DISK
     F*** BEGIN ADD ***  04/30/01  ************************************
     F                                     INFDS(DBFDS2)
     FSWCPF     IF   E             DISK
     F*** END ADD *****  04/30/01  ************************************
     FWMSHL4    IF   E           K DISK
     F                                     RENAME(PSH01:PSH02)
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FSDCMLA    UF   E           K DISK
     FSSYPF     IF   E             DISK

     D*
     D TC              S              1    DIM(4) CTDATA PERRCD(1)              TYPE CODE/DESC
     D TD              S              9    DIM(4) ALT(TC)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** BEGIN ADD ***  04/30/01  ************************************
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*** END ADD *****  04/30/01  ************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        XXEMPX
     C     *LIKE         DEFINE    EMEMP#        SVEMP#
     C     *LIKE         DEFINE    EMEMP#        SVEMP1
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          XXTRDT
     C     *LIKE         DEFINE    EMDT          SVTRDT
     C     *LIKE         DEFINE    EMDT          SVTRD1
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
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
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXTRDT
     C                   KFLD                    XXUNID           12 0
     C*
     C     SHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     SHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*
     C*  DARTS REMARKS FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    SHSEQ#
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
     C*
     C*  CHECK TO SEE IF DARTS AND OFFICE ARE INSTALLED
     C*
     C                   READ      SSY01                                  60
     C     *IN60         IFEQ      *OFF
     C     SYSCIF        ANDEQ     'Y'
     C     SYOFIF        ANDEQ     'Y'
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*** BEGIN ADD ***  04/30/01  ************************************
     C*
     C*  CHECK TO SEE IF PA/DE/NJ REPORTING IS ENABLED.
     C*
     C                   READ      SWC01                                  60
     C     *IN60         IFEQ      *OFF
     C     WCFL18        ANDEQ     'Y'
     C                   MOVE      *ON           *IN59
     C                   ELSE
     C                   MOVE      *OFF          *IN59
     C                   ENDIF
     C*** END ADD *****  04/30/01  ************************************
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
     C                   MOVE      UPFND         SCFND             3 0
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
     C***** BEGIN DEL **** 08/10/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/10/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      'P'           SCPCFL
     C*
     C*   GO TO THE PROMPT
     C*
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C*
     C     TOP           TAG
     C*
     C*   INITIALIZE SCREEN
     C*
     C                   Z-ADD     0             RRN2              5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C*
     C     SCEMP#        IFNE      0
     C                   Z-ADD     SCEMP#        SCEMPX
     C                   ENDIF                                                  SCEMP# IFNE
     C                   MOVE      SCEMPX        SVEMP1
     C                   MOVE      XXTRDT        SVTRD1
     C                   MOVE      XXUNID        SVUNI1           12 0
     C                   MOVE      SCEMPX        SVEMP#
     C                   MOVE      XXTRDT        SVTRDT
     C                   MOVE      XXUNID        SVUNID
     C                   SUB       1             SVUNID
     C*
     C*   CLEAR SUBFILE
     C*
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      *OFF          *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      *ON           *IN01
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
     C                   MOVE      *OFF          *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   MOVE      *ON           *IN01
     C                   MOVE      *ON           *IN06
     C                   WRITE     SCR03
     C                   EXFMT     SCR03C
     C                   MOVE      *OFF          *IN01
     C                   MOVE      *OFF          *IN06
     C                   END
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
     C*
     C*  CMD 2 - PREVIOUS SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END SCR02
     C                   END                                                    END *INKA
     C*
     C*  CMD 5
     C*
     C     *INKE         IFEQ      *ON
     C*
     C*  CMD 5 - EMPLOYER # SEARCH - SCR01
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL
     C                   ENDIF                                                  END SCR01
     C*
     C*  CMD 5 - RESET ALL OPTIONS - SCR02
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C*
     C                   READC     SCR02S                                 50
     C     *IN50         DOWEQ     *OFF
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR02S
     C                   Z-ADD     RRN2          HHREC#
     C                   READC     SCR02S                                 50
     C                   ENDDO
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  END SCR02
     C*
     C                   END                                                    END *INKE
     C*
     C*  CMD 7
     C*
     C     *INKG         IFEQ      *ON
     C*
     C*  CMD 7 - EMPLOYER NAME SEARCH - SCR01
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  END SCR01
     C*
     C*  CMD 7 - HOLD ALL TRANSMISSIONS - SCR02
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR02S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR02S                                 50
     C                   ENDDO
     C*
     C     SCEMP#        IFNE      0
     C                   MOVE      SCEMP#        XXEMP#
     C     SHKY2         CHAIN     PSH01                              60
     C                   ELSE
     C     SHKY1         CHAIN     PSH01                              60
     C                   ENDIF                                                  SCEMP# IFNE
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      '5'           SHSTAT
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   Z-ADD     DSTIME        SHTIME
     C                   UPDATE    PSH01
     C     SCEMP#        IFNE      0
     C     SHKY2         READE     PSH01                                  60
     C                   ELSE
     C     SHKY1         READE     PSH01                                  60
     C                   ENDIF                                                  SCEMP# IFNE
     C                   ENDDO
     C                   MOVE      *ZERO         XXTRDT
     C                   MOVE      *ZERO         XXUNID
     C                   GOTO      TOP
     C                   ENDIF                                                  END SCR02
     C*
     C                   END                                                    END *INKG
     C*
     C*  CMD 9 - RELEASE ALL TRANSMISSIONS
     C*
     C     *INKI         IFEQ      '1'
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR02S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR02S                                 50
     C                   ENDDO
     C*
     C     SCEMP#        IFNE      0
     C                   MOVE      SCEMP#        XXEMP#
     C     SHKY2         CHAIN     PSH01                              60
     C                   ELSE
     C     SHKY1         CHAIN     PSH01                              60
     C                   ENDIF                                                  SCEMP# IFNE
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      '3'           SHSTAT
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   Z-ADD     DSTIME        SHTIME
     C                   UPDATE    PSH01
     C     SCEMP#        IFNE      0
     C     SHKY2         READE     PSH01                                  60
     C                   ELSE
     C     SHKY1         READE     PSH01                                  60
     C                   ENDIF                                                  SCEMP# IFNE
     C                   ENDDO
     C                   MOVE      *ZERO         XXTRDT
     C                   MOVE      *ZERO         XXUNID
     C                   GOTO      TOP
     C                   END                                                    END *INKI
     C*
     C*  CMD 12
     C*
     C     *INKB         IFEQ      '1'
     C*
     C*  CMD 12 - TOGGLE DISPLAY MODES - PENDING OR COMPLETED -SCR02
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C*
     C*  VERIFY NO PENDING OPTIONS EXIST
     C*
     C                   READC     SCR02S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR02S                                 50
     C                   ENDDO
     C*
     C     *IN70         IFEQ      *ON
     C                   MOVE      *OFF          *IN70
     C                   ELSE
     C                   MOVE      *ON           *IN70                    60
     C                   ENDIF
     C                   MOVE      *ZERO         XXTRDT
     C                   MOVE      *ZERO         XXUNID
     C                   GOTO      TOP
     C                   ENDIF                                                  END SCR02
     C*
     C*  CMD 12 - CANCEL DELETIONS - SCR03
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   MOVEL     'SCR02   '    Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END SCR03
     C                   END                                                    END *INKL
     C*
     C*  CMD 13 - PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
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
     C                   MOVE      UPFND         SCFND             3 0
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
     C                   MOVE      *ZERO         XXTRDT
     C                   MOVE      *ZERO         XXUNID
     C                   GOTO      STRT
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
     C                   READC     SCR02S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SCSEL         IFNE      ' '
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN22
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   Z-ADD     RRN2          HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C                   READC     SCR02S                                 50
     C                   ENDDO
     C*
     C                   MOVE      SVEMP1        XXEMP#
     C                   MOVE      SVTRD1        XXTRDT
     C                   MOVE      SVUNI1        XXUNID
     C     *IN70         IFEQ      *ON
     C     SHKY          SETLL     PSH01
     C     SHKY1         READPE    PSH01                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C     XXEMPX        ANDNE     *ZERO
     C     XXEMPX        ANDNE     SHEMP#
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C*
     C                   ELSE
     C     SHKY          SETLL     PSH02
     C     SHKY1         READPE    PSH02                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C     XXEMPX        ANDNE     *ZERO
     C     XXEMPX        ANDNE     SHEMP#
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C*
     C                   ENDIF
     C     *IN50         IFEQ      *ON
     C                   MOVE      *ON           *IN31
     C                   Z-ADD     1             HHREC#
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN31
     C                   MOVE      SHEMP#        SCEMPX
     C     *IN70         IFEQ      *ON
     C                   MOVEL     SHTRDT        XXTRDT
     C                   ELSE
     C                   MOVEL     SHSBDT        XXTRDT
     C                   ENDIF
     C                   MOVE      SHUNID        XXUNID
     C                   DO        10            X
     C     *IN70         IFEQ      *ON
     C     SHKY1         READPE    PSH01                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C     XXEMPX        ANDNE     *ZERO
     C     XXEMPX        ANDNE     SHEMP#
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C*
     C                   ELSE
     C     SHKY1         READPE    PSH02                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C     XXEMPX        ANDNE     *ZERO
     C     XXEMPX        ANDNE     SHEMP#
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C*
     C                   ENDIF
     C   50              LEAVE
     C                   MOVE      SHEMP#        SCEMPX
     C     *IN70         IFEQ      *ON
     C                   MOVEL     SHTRDT        XXTRDT
     C                   ELSE
     C                   MOVEL     SHSBDT        XXTRDT
     C                   ENDIF
     C                   MOVE      SHUNID        XXUNID
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
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      SCR01T
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   GOTO      SCR02T
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   GOTO      SCR03T
     C                   END
     C*
     C*  PROCESS SCREEN 1
     C*
     C     SCR01T        TAG
     C*
     C*  EDIT THE EMPLOYER #
     C*
     C     SCEMP#        IFNE      *ZERO
     C     EMKY1         CHAIN     PEM02                              21
     C     *IN21         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  NO MORE EDITS - DISPLAY THE SUBFILE
     C*
     C                   MOVE      *ZERO         XXTRDT
     C                   MOVE      *ZERO         XXUNID
     C                   MOVE      SCEMP#        SCEMPX
     C                   MOVE      SCEMP#        XXEMPX
     C*
     C     SCPCFL        IFEQ      'P'
     C                   MOVE      *ON           *IN70
     C                   ELSE
     C                   MOVE      *OFF          *IN70
     C                   ENDIF
     C*
     C     XXEMPX        IFNE      *ZERO
     C                   MOVE      *ON           *IN85
     C                   ELSE
     C                   MOVE      *OFF          *IN85
     C                   ENDIF
     C*
     C                   GOTO      TOP
     C*
     C*  PROCESS SCREEN 2
     C*
     C     SCR02T        TAG
     C*
     C*   CLEAR DELETION SUBFILE
     C*
     C                   Z-ADD     0             RRN3              5 0
     C*
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR03C
     C                   MOVE      *OFF          *IN04
     C                   MOVE      *OFF          *IN48
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   MOVE      *OFF          *IN49
     C     CKSEL         TAG
     C                   READC     SCR02S                                 50
     C     *IN50         IFEQ      *OFF
     C*
     C                   MOVE      *ON           *IN49
     C*
     C*  OPTION BLANK - BYPASS
     C*
     C     SCSEL         IFEQ      *BLANK
     C                   UPDATE    SCR02S
     C                   GOTO      CKSEL
     C                   ENDIF
     C*
     C     *IN21         IFEQ      *OFF
     C                   Z-ADD     RRN2          HHREC#
     C                   ENDIF
     C*
     C*  OPTION 3 - HOLD TRANSMISSION
     C*
     C     SCSEL         IFEQ      '3'
     C     *IN70         ANDEQ     *ON
     C                   MOVE      SCEMPS        XXEMP#
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      SCUNID        XXUNID
     C     SHKY          CHAIN     PSH01                              60
     C                   MOVE      '5'           SHSTAT
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   Z-ADD     DSTIME        SHTIME
     C  N60              UPDATE    PSH01
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVEL     'HELD'        SCSTAT
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR02S
     C                   GOTO      CKSEL
     C                   END                                                    END EQ '3'
     C*
     C*  OPTION 4 - DELETE RECORD
     C*
     C     SCSEL         IFEQ      '4'
     C     *IN70         ANDEQ     *ON
     C                   MOVE      *ON           *IN48
     C*
     C                   ADD       1             RRN3
     C                   MOVE      *ON           *IN19
     C                   WRITE     SCR03S
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   GOTO      CKSEL
     C                   END                                                    END EQ '4'
     C*
     C*  OPTION 6 - RELEASE TRANSMISSION
     C*
     C     SCSEL         IFEQ      '6'
     C     *IN70         ANDEQ     *ON
     C                   MOVE      SCEMPS        XXEMP#
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      SCUNID        XXUNID
     C     SHKY          CHAIN     PSH01                              60
     C                   MOVE      '3'           SHSTAT
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   Z-ADD     DSTIME        SHTIME
     C  N60              UPDATE    PSH01
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVEL     'RELEASED'    SCSTAT
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR02S
     C                   GOTO      CKSEL
     C                   END                                                    END EQ '6'
     C*** BEGIN ADD ***  04/30/01  ************************************
     C*
     C*  OPTION 8 - STATE STATUS
     C*
     C     SCSEL         IFEQ      '8'
     C     *IN70         ANDEQ     *ON
     C     *IN59         ANDEQ     *ON
     C*
     C                   MOVE      SCEMPS        XXEMP#
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      SCUNID        XXUNID
     C     SHKY          CHAIN(N)  PSH01                              60
     C  N60              CALL      'W7416R02'
     C                   PARM      DBRRN2        QQRRN             9 0
     C                   PARM      'C'           @@MODE            1
     C     *INKC         PARM                    @@KC
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR02S
     C                   GOTO      CKSEL
     C                   END                                                    END EQ '8'
     C*** END ADD *****  04/30/01  ************************************
     C*
     C*  OPTION 9 - REPRINT TRANSMISSION
     C*
     C     SCSEL         IFEQ      '9'
     C     *IN80         ANDEQ     *ON
     C                   MOVE      SCEMPS        XXEMP#
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      SCUNID        XXUNID
     C     *IN70         IFEQ      '0'
     C     SHKY          CHAIN     PSH02                              60
     C                   ELSE
     C     SHKY          CHAIN     PSH01                              60
     C                   END
     C                   MOVE      SHCO#         QCCO#
     C                   MOVE      SHFND         QCFND
     C                   MOVE      SHEMP#        QCEMP#
     C                   MOVE      SHDIV         QCDIV
     C                   MOVE      *BLANK        QQRTCD
     C  N60CMKY          CHAIN(N)  PCM01                              60
     C     *IN60         IFEQ      *OFF
     C     CMDCID        ANDNE     *BLANK
     C     CMFOLD        ANDNE     *BLANK
     C***** BEG ADD 09/27/11 ************************************
     C     '.PDF'        SCAN      CMDCID        ##RSLT            2 0
     C     ##RSLT        IFEQ      *ZEROS
     C                   CALL      'DTGETDIR'
     C                   PARM      CMFOLD        ##FLR            80
     C                   PARM                    ##DIR            80
     C*** BEGIN ADD **** 10/17/11  ************************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
     C*
     C     '/QDLS'       SCAN      ##DIR         ##RSLT
     C     '/QDLS'       SCAN      ##FLR         ##RSL1            2 0
     C*
     C     CMUNCN        IFEQ      *BLANKS
     C     ##RSLT        ANDNE     *ZEROS
     C     ##RSL1        ANDEQ     *ZEROS
     C                   MOVEL     ##FLR         ##PTH
     C                   ELSE
     C*** END  ADD ****  10/17/11  ************************************
     C*
     C                   MOVEL     ##DIR         ##PTH
     C*** BEG  ADD ****  10/17/11  ************************************
     C                   ENDIF
     C*** END  ADD ****  10/17/11  ************************************
     C*
     C                   CALL      'SRCHKDLO'
     C                   PARM                    CMDCID
     C                   PARM                    ##PTH            32
     C                   PARM      '*DOC'        ##TYP             4
     C                   PARM                    ##ERR             1
     C*** BEGIN ADD **** 10/17/11  ************************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
     C*** END  ADD ****  10/17/11  ************************************
     C* When document not found in specified folder.
     C     ##ERR         IFEQ      '1'
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C* When document is of '.PDF' type.
     C                   ELSE
     C                   MOVE      *ON           *IN34
     C                   ENDIF
     C*
     C     *IN34         IFEQ      *ON
     C     ##ERR         OREQ      '1'
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C***** END ADD 09/27/11 ************************************
     C                   CALL      'W7416C1'
     C                   PARM                    CMDCID
     C                   PARM                    CMFOLD
     C                   PARM                    QQRTCD            1
     C*** BEGIN ADD **** 10/17/11  ************************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
     C*** END  ADD ****  10/17/11  ************************************
     C     QQRTCD        IFNE      *BLANK
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      *ON           *IN52
     C                   ENDIF
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      *ON           *IN53
     C                   ENDIF
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      *ON           *IN54
     C                   ENDIF
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN54
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVE      *BLANK        SCSEL
     C                   UPDATE    SCR02S
     C                   GOTO      CKSEL
     C                   END                                                    END EQ '6'
     C*
     C*  ANY OTHER OPTIONS ARE INVALID.
     C*
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN21
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN19
     C                   MOVE      *OFF          *IN20
     C                   GOTO      STRT
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
     C                   MOVE      *ZERO         XXTRDT
     C                   MOVE      *ZERO         XXUNID
     C                   GOTO      TOP
     C                   END                                                                     S
     C                   GOTO      TERM
     C*
     C                   ELSE
     C*
     C*  IF RECORD DELETION WAS REQUESTED, DISPLAY SCREEN 3
     C*
     C     *IN48         IFEQ      *ON
     C                   MOVEL     'SCR03   '    Q2RECF
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   END                                                    END 0 *IN50
     C*
     C*  PROCESS SCREEN 3
     C*
     C     SCR03T        TAG
     C*
     C*  READ THROUGH THE SUBFILE AND DELETE THE RECORDS
     C*
     C                   READC     SCR03S                                 50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVE      SCEMPS        XXEMP#
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      SCUNID        XXUNID
     C     SHKY          CHAIN     PSH01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SHCO#         QCCO#
     C                   MOVE      SHFND         QCFND
     C                   MOVE      SHEMP#        QCEMP#
     C                   MOVE      SHDIV         QCDIV
     C***** BEG DEL 03/30/11 ************************************
     C*          CMKY      DELETPCM01                60
     C***** END DEL 03/30/11 ************************************
     C***** BEG ADD 03/30/11 ************************************
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'I'           CMSTAT
     C                   UPDATE    PCM01
     C                   ENDIF
     C***** END ADD 03/30/11 ************************************
     C                   DELETE    PSH01
     C                   ENDIF
     C*
     C                   READC     SCR03S                                 50
     C                   ENDDO
     C*
     C*  FIND THE TOP RECORD OF THE LAST SCREEN USED.  SET
     C*  THE TOP BASED ON THAT RECORD.
     C*
     C     HHREC#        DIV       11            XXTOP             3 0
     C                   MVR                     XXTOP
     C     XXTOP         IFEQ      *ZERO
     C                   Z-ADD     10            XXTOP
     C                   ELSE
     C                   SUB       1             XXTOP
     C                   ENDIF
     C     HHREC#        SUB       XXTOP         XXTOP
     C     XXTOP         CHAIN     SCR02S                             60
     C                   MOVE      SCEMP#        SCEMPX
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   MOVE      SCUNID        XXUNID
     C                   GOTO      TOP
     C*
     C*  END OF THE PROGRAM.
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
     C                   Z-ADD     SVTRDT        XXTRDT
     C                   Z-ADD     SVUNID        XXUNID
     C*
     C     *IN70         IFEQ      *ON
     C     SHKY          SETGT     PSH01
     C                   ELSE
     C     SHKY          SETGT     PSH02
     C                   ENDIF
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UP TO 11 LINES
     C*
     C                   DO        11            X                 5 0
     C*
     C     *IN70         IFEQ      *ON
     C     SHKY1         READE(N)  PSH01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXEMPX        ANDNE     *ZERO
     C     XXEMPX        ANDNE     SHEMP#
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ELSE
     C     SHKY1         READE     PSH02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXEMPX        ANDNE     *ZERO
     C     XXEMPX        ANDNE     SHEMP#
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ENDIF
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
     C                   MOVE      SHEMP#        SCEMPS
     C                   MOVE      SHEMP#        SVEMP#
     C     *IN70         IFEQ      *ON
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      SHTRDT        SVTRDT
     C                   ELSE
     C                   Z-ADD     SHSBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      SHSBDT        SVTRDT
     C                   ENDIF
     C                   MOVE      SHUNID        SCUNID
     C                   MOVE      SHUNID        SVUNID           12 0
     C     EMKY          CHAIN     PEM02                              60
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVE      *BLANK        SCSTAT
     C*
     C     *IN70         IFEQ      *ON
     C*
     C     SHSTAT        IFEQ      '5'
     C                   MOVEL     'HELD'        SCSTAT
     C                   ELSE
     C                   MOVEL     'RELEASED'    SCSTAT
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL     'SENT    '    SCSTAT
     C                   ENDIF
     C*
     C                   Z-ADD     1             Y                 3 0
     C     SHTYPE        LOOKUP    TC(Y)                                  60
     C                   MOVEL     TD(Y)         SCTYPE
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
     C                   MOVEL     *ZERO         SCEMPS
     C                   MOVEL     *BLANK        SCENAM
     C                   MOVE      *BLANK        SCSTAT
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *ZERO         SCTRDT
     C                   MOVE      *ZERO         SCUNID
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             XXLAST
     C                   Z-ADD     1             HHREC#
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR02S
     C                   END
     C*
     C*  SAVE LAST WRITTEN RECORD NUMBER
     C*
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C*
     C                   MOVEL     *ZERO         SCEMPX
     C*
     C                   ENDSR
**  TC/TD - TYPE CODE/DESCRIPTION
PPOLICY
CCANCEL
EENDORSE
RREINSTATE
