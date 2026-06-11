     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N0175R - CORRECT CHECK# FOR CASH POSTING PURPOSES
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*   THIS OPTION ALLOWS YOU TO CHANGE THE CHECK# POSTED IN       *
     H*        PROVIDER CHECK#: FAEOP/EOWCCK                          *
     H*        BRS CHECK#:      FAE1P/E110A2                          *
     H*        NWS CHECK#:      FAE1P/E110A1                          *
     H*                                                               *
     H*   NOTE:  THIS IS NOT FOR GENERAL PUBLIC USE                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   A/R PROCESSING MENU                                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   F0508R         GROUP SEARCH                                 *
     H*   F0545R         SUBSCRIBER SEARCH                            *
     H*   F0570R         CASE # SEARCH                                *
     H*   F0571R         SOCIAL SECURITY/CASE# SEARCH                 *
     H*   F0573R         SS#/ACCIDENT DATE SEARCH                     *
     H*   F0578R         ALPHA SEARCH                                 *
     H*   F0579R         MULTIPLE CASE SEARCH                         *
     H*   N2000R         EOB#/SERVICE# SEARCH                         *
     H*   N2005R         PRE-CERT/SERVICE# SEARCH                     *
     H*                                                               *
     H*  PROGRAMMER: C:05               PROJECT: NWMNT                *
     H*  CREATED: 08/27/96                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* GENERAL:                                                      *
     H*  09    INQUIRY MODE.                                          *
     H* SCR01:                                                        *
     H*  21    MCR0301 -  INVALID SUBSCRIBER                          *
     H*  22    MCR0346 -  INVALID GROUP                               *
     H*  23    MCR0730 -  INVALID CASE                                *
     H*  24    MCR0721 -  INVALID EOB#                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                  DSPF changed to have SCSSN as char9 from num9*
     H*                  Pgm Recompile only                           *
     H*                                                               *
     H*  12/28/98  C:94  PROJECT: MC152                               *
     H*                  1.  CHANGE FROM USING NATRP, NAMBP, NAPBP,   *
     H*                      FAHBP, AND FAE1P TO USING FAEAP IF       *
     H*                      ##PRVB = 'Y' (NEW PROVIDER BILL)         *
     H*                                                               *
     H*****************************************************************
     FN0175F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMCDL     IF   E           K DISK
     FFAEOL4    UF   E           K DISK
     FFAE1L     UF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFAHBL     IF   E           K DISK
     FNATRL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     FNAMBL1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEG ADD *** 12/28/98 ****************************************
     FFAEAL1    UF   E           K DISK
     F*** END ADD *** 12/28/98 ****************************************
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D CK1A            S             78    DIM(4) CTDATA PERRCD(1)              CMD KEYS FOR SCR01 6
     D CK1B            S             78    DIM(3) CTDATA PERRCD(1)              CMD KEYS FOR SCR01 6
     D CK2A            S             78    DIM(1) CTDATA PERRCD(1)              CMD KEYS FOR SCR02
     D CK2B            S             78    DIM(1) CTDATA PERRCD(1)              CMD KEYS FOR SCR02
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
     C* KEY LIST FOR THE COMPANY FILE
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* KEY LIST FOR THE USER PARAMETER FILE
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR THE CLAIM MASTER FILE
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    SCSS#
     C                   KFLD                    XXACDT            7 0
     C*
     C* KEY LIST FOR THE CLAIM DEMOGRAPHIC FILE
     C     CDKY          KLIST
     C                   KFLD                    SCSS#
     C*
     C*** BEG ADD *** 12/28/98 ***************************************
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C*** END ADD *** 12/28/98 ***************************************
     C* KEY LIST FOR THE HBA BILLING FILE
     C     HBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C* KEY LIST FOR THE SUBSCRIBER FILE
     C     SMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C*
     C* KEY LIST FOR THE FUND
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C* KEY LIST FOR THE EOB FILE
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C* KEY LIST FOR THE EOB AUX FILE
     C     E1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C* FS BASE CONTROL FILE
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C* KEY LIST FOR THE UR BILLING FILE
     C     PBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C* KEY LIST FOR THE NETWORK BILLING FILE
     C     TRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C* KEY LIST FOR THE MEDICAL MANAGEMENT BILLING FILE
     C     MBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C* PARAMETER LIST FOR CALLED PROGRAM
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
     C*** BEG ADD *** 12/28/98 ***************************************
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
     C*
     C*** END ADD *** 12/28/98 ***************************************
     C* INITIALIZE PROMPT SCREEN VALUES
     C                   MOVE      *ZERO         SCSUB#
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *ZERO         SCFYR
     C                   MOVE      *ZERO         SCCASE
     C                   MOVE      *ZERO         SCEOB#
     C*
     C* DETERMINE IF CALLED FROM MENU OR FROM PROGRAM AND LOAD FIELDS ACCORDINGL
     C     QQCALL        IFEQ      '1'                                          MENU INQUIRY
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   MOVE      *ZERO         SCFYR
     C                   MOVE      *ZERO         SCCASE
     C                   MOVE      *BLANKS       SCSYS
     C                   MOVE      *ZEROS        SCEOB#
     C                   MOVE      '0'           *IN07                          EXIT VS. RETURN
     C                   MOVE      '1'           *IN09
     C                   ELSE                                                   ELSE QQCALL EQ '1'
     C     QQCALL        IFEQ      *BLANKS
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   MOVE      *ZERO         SCFYR
     C                   MOVE      *ZERO         SCCASE
     C                   MOVE      *BLANKS       SCSYS
     C                   MOVE      *ZEROS        SCEOB#
     C                   MOVE      '0'           *IN07                          EXIT VS. RETURN
     C                   MOVE      '0'           *IN09
     C                   END                                                    END QQCALL EQ *BLANK
     C                   END                                                    END QQCALL EQ '3'
     C*
     C* INITIALIZE SECTION
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CMSUB#        Q1SUB#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         Q1FND
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMCASE        Q1CASE
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* GET THE BASE USER DEFAULT VALUES
     C     TOP           TAG
     C                   Z-ADD     1             CK
     C   07              MOVEL     CK1A(CK)      SCCMD1
     C  N07              MOVEL     CK1B(CK)      SCCMD1
     C  N07              MOVEL     CK2A(CK)      SCCMD2
     C   07              MOVEL     CK2B(CK)      SCCMD2
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        SCSUB#
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXFND
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      UPFND         SCFND
     C                   END
     C*
     C* CHAIN TO THE COMPANY FILE TO GET COMPANY NAME
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* CHAIN OUT TO FS SYSTEM CONTROL FILE TO SEE IF UR IS INSTALLED
     C* IF IT IS SET ON *IN01 THIS WILL ALLOW USE OF F17  ALSO CHECK FOR
     C* WCMS INTERFACE INSTALLED
     C*
     C                   MOVE      '0'           *IN01
     C                   MOVE      '0'           *IN81                          ADD 08/26/93
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL07        IFEQ      'Y'
     C                   MOVE      '1'           *IN01
     C                   END                                                    END FBFL07
     C     FBFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN81
     C                   END                                                    END FBFL01
     C                   END                                                    END *IN60
     C*
     C* LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* DISPLAY ALL FORMATS
     C     STRT          TAG
     C*
     C                   SETOFF                                       99
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   MOVE      *BLANKS       SVSCRN
     C                   MOVEL     Q2RECF        SVSCRN
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   MOVE      *BLANKS       SVSCRN
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F6                                 *
     C****************************************
     C     *INKF         IFEQ      '1'
     C*********************************
     C*  SCR01 - SUBSCRIBER SEARCH    *
     C*********************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     SCSUB#        Q1SUB#
     C                   CALL      'F0545R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    Q1SUB#
     C                   PARM                    QQRTCD            1
     C                   Z-ADD     Q1SUB#        SCSUB#
     C     QQRTCD        CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCSUB#        OREQ      *ZERO
     C                   MOVE      '1'           *IN37
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EQUAL B
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN38
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END *INKF
     C****************************************
     C*  F7                                  *
     C****************************************
     C     *INKG         IFEQ      '1'
     C****************************************
     C*  SCR01 = GROUP SEARCH                *
     C****************************************
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   MOVE      XXSUB#        Q1SUB#
     C                   ELSE
     C                   MOVE      SCSUB#        Q1SUB#
     C                   END
     C                   Z-ADD     SCFND         Q1FND
     C                   CALL      'F0508R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    Q1SUB#
     C                   PARM                    Q1FND
     C                   PARM                    QQRTCD
     C                   Z-ADD     Q1FND         SCFND
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFND         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      Q1SUB#        SCSUB#
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END *INKG
     C****************************************
     C*  F8 = SS#/ACDT SEARCH                *
     C****************************************
     C     *INKH         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'F0573R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSS#             9
     C                   PARM                    SCACDT
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCSS#         OREQ      *BLANK
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C                   Z-ADD     *ZEROS        CNTCAS            3 0
     C     CMKY1         CHAIN     PCM02                              20
     C     *IN20         DOWEQ     '0'
     C                   ADD       1             CNTCAS
     C     CMKY1         READE     PCM02                                  20
     C                   END                                                    END *IN20
     C*** IF NO HIT, THIS IS AN INVALID SS#/ACDT - SEND ERROR MESSAGE (*IN20)
     C*** ELSE, SETOF *IN20 SO MESSAGE IS NOT DISPLAYED
     C     CNTCAS        IFEQ      *ZEROS
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C*** IF THERE IS MORE THAN 1 CASE, THEN DISPLAY "Multi Case" Search F0579F
     C     CNTCAS        IFGT      1
     C                   MOVE      SCSS#         QTSS#
     C                   MOVE      SCACDT        QTACDT
     C                   MOVE      *BLANKS       QTFND
     C                   MOVE      *BLANKS       QTFYR
     C                   MOVE      *BLANKS       QTCASE
     C                   CALL      'F0579R'
     C                   PARM                    QTPRDS           40
     C                   PARM                    QTCALL            1
     C                   PARM                    QTSS#             9
     C                   PARM                    QTACDT            8
     C                   PARM                    QTFND             3
     C                   PARM                    QTFYR             3
     C                   PARM                    QTCASE            7
     C                   PARM                    QTRTCD            1
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   WRITE     SCR01
     C*
     C     QTRTCD        IFEQ      'A'
     C     QTRTCD        OREQ      'B'
     C     QTFND         OREQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END                                                    END EQ A/B/*BLANKS
     C*
     C     QTRTCD        IFEQ      '*'
     C                   MOVE      QTFND         SCFND
     C                   MOVE      QTFYR         SCFYR
     C                   MOVE      QTCASE        SCCASE
     C     CMKY          CHAIN     PCM01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END                                                    END EQ *
     C*
     C                   END                                                    END GT 1
     C*
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C                   MOVE      '1'           *IN26
     C*
     C                   GOTO      STRT
     C                   END                                                    ENDEQ '*'
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END *INKH
     C****************************************
     C*  F9 = CASE # SEARCH                  *
     C****************************************
     C     *INKI         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      XXFND         Q1FND
     C                   Z-ADD     SCCASE        Q1CASE
     C                   CALL      'F0570R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCYR              3 0
     C                   PARM                    Q1CASE
     C                   PARM                    SCSS#
     C                   PARM                    QQRTCD
     C                   PARM                    Q1FND
     C                   Z-ADD     Q1CASE        SCCASE
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCYR          OREQ      *ZERO
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END                                                    END EQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   Z-ADD     SCYR          SCFYR
     C                   Z-ADD     Q1FND         SCFND
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         SCSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN60
     C                   END                                                    END EQ '*'
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END *INKI
     C****************************************
     C*   F11 = SS#/CASE SEARCH              *
     C****************************************
     C     *INKK         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     *ZEROS        Q1FND
     C                   Z-ADD     SCCASE        Q1CASE
     C                   Z-ADD     SCFYR         SCYR
     C*
     C                   CALL      'F0571R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSS#
     C                   PARM                    SCYR
     C                   PARM                    Q1CASE
     C                   PARM                    Q1FND
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCSS#         OREQ      *BLANK
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   Z-ADD     Q1FND         SCFND
     C                   Z-ADD     SCYR          SCFYR
     C                   Z-ADD     Q1CASE        SCCASE
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         SCSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      '1'           *IN26                          POS CURSOR PROV
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN60
     C                   END                                                    ENDEQ '*'
     C                   END                                                    ENDEQ SCR01
     C                   END                                                    ENDIF *INKK
     C****************************************
     C*   F12 = PREVIOUS                     *
     C****************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END                                                    END SCR02
     C                   END                                                    END *INKL
     C****************************************
     C*   F13 = CHG USER PARMS               *
     C****************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   END                                                    END 1 *IN60
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END                                                    END *INKM
     C************************************
     C*   F15 = ALPHA SEARCH             *
     C************************************
     C     *INKP         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'F0578R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSS#
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCSS#         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   Z-ADD     SCCASE        Q1CASE
     C                   Z-ADD     *ZEROS        Q1FND
     C                   CALL      'F0571R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSS#
     C                   PARM                    SCYR
     C                   PARM                    Q1CASE
     C                   PARM                    Q1FND
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCSS#         OREQ      *BLANK
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   Z-ADD     SCYR          SCFYR
     C                   Z-ADD     Q1FND         SCFND
     C                   Z-ADD     Q1CASE        SCCASE
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         SCSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN60
     C                   END                                                    ENDEQ '*'
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END SCR01
     C                   END                                                    ENDIF *INKP
     C************************************
     C*  F16 = EOMB#/SERVICE# SEARCH     *
     C************************************
     C     *INKQ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCSUB#        Q2SUB#
     C                   MOVE      SCFND         Q2FND
     C                   MOVE      SCFYR         Q2FYR
     C                   MOVE      SCCASE        Q2CASE
     C                   MOVE      SCEOB#        Q2EOB#
     C                   CALL      'N2000R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           Q2CALL            1
     C                   PARM                    Q2SUB#            5
     C                   PARM                    Q2FND             3
     C                   PARM                    Q2FYR             3
     C                   PARM                    Q2CASE            7
     C                   PARM                    Q2EOB#            9
     C                   PARM                    QQRTCD
     C                   MOVE      Q2SUB#        SCSUB#
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     Q2SUB#        OREQ      *BLANKS
     C                   MOVE      *ZEROS        SCSUB#
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      *ZEROS        SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCEOB#
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      Q2SUB#        SCSUB#
     C                   MOVE      Q2FND         SCFND
     C                   MOVE      Q2FYR         SCFYR
     C                   MOVE      Q2CASE        SCCASE
     C                   MOVE      Q2EOB#        SCEOB#
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END SCR01
     C                   END                                                    ENDIF *INKQ
     C************************************
     C*  F17 = PRE-CERT/SERVICE SEARCH   *
     C************************************
     C     *INKR         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN65         ANDEQ     '1'
     C                   MOVE      SCSUB#        Q2SUB#
     C                   MOVE      SCFND         Q2FND
     C                   MOVE      *BLANKS       Q2FYR
     C                   MOVE      *BLANKS       Q2CASE
     C                   MOVE      *BLANKS       Q2EOB#
     C                   CALL      'N2005R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           Q2CALL            1
     C                   PARM                    Q2SUB#            5
     C                   PARM                    Q2FND             3
     C                   PARM                    Q2FYR             3
     C                   PARM                    Q2CASE            7
     C                   PARM                    Q2EOB#            9
     C                   PARM                    QQRTCD
     C                   MOVE      Q2SUB#        SCSUB#
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     Q2SUB#        OREQ      *BLANKS
     C                   MOVE      *ZEROS        SCSUB#
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      *ZEROS        SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCEOB#
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      Q2SUB#        SCSUB#
     C                   MOVE      Q2FND         SCFND
     C                   MOVE      Q2FYR         SCFYR
     C                   MOVE      Q2CASE        SCCASE
     C                   MOVE      Q2EOB#        SCEOB#
     C                   GOTO      STRT
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END SCR01
     C                   END                                                    ENDIF *INKR
     C****************************************
     C*      F24 = MORE COMMAND KEYS         *
     C****************************************
     C     *INKY         IFEQ      '1'
     C                   MOVEL     Q2RECF        HLRECF           10
     C     CK            ADD       1             CK                5 0
     C*
     C     HLRECF        IFEQ      'SCR01'
     C     *IN65         IFEQ      '1'
     C     CK            IFGT      4
     C                   Z-ADD     1             CK
     C                   END                                                    END CK GT 4
     C                   MOVEL     CK1A(CK)      SCCMD1
     C                   ELSE
     C     CK            IFGT      3
     C                   Z-ADD     1             CK
     C                   END                                                    END CK GT 4
     C                   MOVEL     CK1B(CK)      SCCMD1
     C                   END                                                    END *IN65
     C                   END                                                    END SCR01
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     HLRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKY
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C**************
     C* SCR01      *
     C**************
     C     SCR01T        TAG
     C*
     C* THE SUBSCRIBER MUST BE VALID
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* THE GROUP MUST BE VALID
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C* THE CASE MUST BE VALID
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      CMSS#         SCSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      *BLANKS       SCFSN
     C                   MOVE      *BLANKS       SCLSN
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDFSN         SCFSN
     C                   MOVEL     CDLSN         SCLSN
     C                   END
     C                   END
     C*
     C* THE EOB# MUST BE VALID
     C     EOKY          CHAIN     PEO01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      EOWCCK        SCWCCK                         PROV CHECK#
     C     EOAMT         SUB       EONDIS        SCPRVA                         PROV AMT
     C*** BEG ADD *** 12/28/98 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKY          CHAIN     PEA01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     EAWCBC        SC10A2                         BRS CHECK#
     C                   MOVEL     EAWCNC        SC10A1                         NWS CHECK#
     C                   Z-ADD     EANAMT        SCNWSA                         PROV AMT
     C                   ENDIF
     C                   ELSE
     C*** END ADD *** 12/28/98 ****************************************
     C* GET FAE1P INFO
     C     E1KY          CHAIN     PE101                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     E110A2        SC10A2                         BRS CHECK#
     C                   MOVEL     E110A1        SC10A1                         NWS CHECK#
     C                   Z-ADD     E1AMT1        SCNWSA                         PROV AMT
     C                   END
     C*** BEG ADD *** 12/28/98 ****************************************
     C                   ENDIF
     C*** END ADD *** 12/28/98 ****************************************
     C* DISPLAY BILLING AMOUNT
     C                   Z-ADD     *ZEROS        SCBRSA
     C*** BEG CHG *** 12/28/98 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C     EOSYS         IFEQ      'P'
     C                   MOVE      EOBAMT        SCBRSA
     C                   ELSE
     C     EOSYS         IFEQ      'H'
     C     EOSYS         OREQ      'U'
     C     EOSYS         OREQ      'M'
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      EABAMT        SCBRSA
     C                   END
     C                   ELSE
     C     EOSYS         IFEQ      'N'
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      EANAMT        SCBRSA
     C                   END
     C                   END                                                    END EQ 'N'
     C                   END                                                    END EQ 'U,M,H'
     C                   END                                                    END EQ 'P'
     C                   ELSE
     C     EOSYS         IFEQ      'P'
     C                   MOVE      EOBAMT        SCBRSA
     C                   ELSE
     C     EOSYS         IFEQ      'H'
     C     HBKY          CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      HB1AAM        SCBRSA
     C                   END
     C                   ELSE
     C     EOSYS         IFEQ      'U'
     C     PBKY          CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PBBAMT        SCBRSA
     C                   END
     C                   ELSE
     C     EOSYS         IFEQ      'N'
     C     TRKY          CHAIN     PTR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      TRBAMT        SCBRSA
     C                   END
     C                   ELSE
     C     EOSYS         IFEQ      'M'
     C     MBKY          CHAIN     PMB01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      MBBAMT        SCBRSA
     C                   END
     C                   END                                                    END EQ 'M'
     C                   END                                                    END EQ 'N'
     C                   END                                                    END EQ 'U'
     C                   END                                                    END EQ 'H'
     C                   END                                                    END EQ 'P'
     C                   ENDIF                                                  ##PRVB = 'Y'
     C                   ENDIF
     C*** END CHG *** 12/28/98 ****************************************
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C**************
     C* SCR02      *
     C**************
     C*
     C     SCR02T        TAG
     C*
     C* NO EDITS
     C*
     C* UPDATE CHECK# IN FAEOP FILE FIELDS
     C     *IN50         IFEQ      '0'
     C                   MOVE      SCWCCK        EOWCCK
     C                   UPDATE    PEO01
     C                   END
     C*
     C* UPDATE CHECK# IN FAE1P FILE FIELDS
     C     *IN51         IFEQ      '0'
     C*** BEG ADD *** 12/28/98 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   MOVE      SC10A2        EAWCBC
     C                   MOVE      SC10A1        EAWCNC
     C                   UPDATE    PEA01
     C                   ELSE
     C*** END ADD *** 12/28/98 ***************************************
     C                   MOVE      SC10A2        E110A2
     C                   MOVE      SC10A1        E110A1
     C                   UPDATE    PE101
     C                   END
     C*** BEG ADD *** 12/28/98 ***************************************
     C                   ENDIF
     C*** END ADD *** 12/28/98 ***************************************
     C*
     C                   MOVE      *BLANKS       SCFYR
     C                   MOVE      *BLANKS       SCCASE
     C                   MOVE      *BLANKS       SCEOB#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C**************
**  CK1A - COMMAND KEYS for SCR01 *IN01 = 1
F3=Exit       F6=Subscriber Search       F7=Group Search       F24=More keys
F8=SS#/ACDT Search    F9=Case # Search   F11=SS#/Case Search   F24=More keys
F13=Chg User Parms  F15=Alpha Search  F16=EOB#/SVC# Search     F24=More keys
F17=Pre-Cert#/SVC# Search                                      F24=More keys
**  CK1B - COMMAND KEYS for SCR01  *IN01 = 0
F3=Exit       F6=Subscriber Search       F7=Group Search       F24=More keys
F8=SS#/ACDT Search    F9=Case # Search   F11=SS#/Case Search   F24=More keys
F13=Chg User Parms  F15=Alpha Search  F16=EOB#/SVC# Search     F24=More keys
**  CK2A - COMMAND KEYS for SCR02   *IN75=0
F3=Exit                  F12=Previous
**  CK2B - COMMAND KEYS for SCR02   *IN75=1
F3=Return                F12=Previous
