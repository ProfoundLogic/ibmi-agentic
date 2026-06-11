     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*
     H/TITLE W0070RA1- STATE NOI INFORMATION MAINTENANCE
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*       THIS PROGRAM UPDATES THE STATE NOI INFORMATION FILE     *
     H*       THE FIELDS ARE NOT REQUIRED OR EDITED.  THE DATA       *
     H*       WILL APPEAR ON THE REPORT AS ENTERED.                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070R6 - LOG API                                          *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0270R5- EDI LOG HISTORY                                   *
     H*                                                               *
     H*  PROGRAMMER: C:46                                             *
     H*  CREATED:    02/20/96    PROJECT: 25WDE                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 10/22/09 C:189 SEQ#70040                                      *
     H*                Additional screens being displayed which       *
     H*                has not been set up in Group set up.           *
    H*                                                               *
     H*                                                               *
     H* 08/28/09 C:189 Project: 133WHE                                *
     H*                Add the call to Accident Info program          *
     H*                W0070R08 and not call W0190R and SC010R.       *
     H*                                                               *
     H* 08/25/06 C:15 Support: WC025/54305                            *
     H*               Removed code that was added on 5/29/06 (w/o     *
     H*               comments) to require the entry of the Preparer  *
     H*               name, title and phone #.  These fields should   *
     H*               not be required.                                *
     H*                                                               *
     H* 04/03/03 C:136 PROJECT: WC1084                                *
     H*                Add hiring state to second screen.             *
     H*                                                               *
     H* 05/18/00 C:53 PROJECT: WC962
     H*               Added jurisdiction state to WMNIP chain.
     H*               Load CMJRST to NIST.
     H*                                                               *
     H* 11/22/98 C:58 Support: WC081/14631                            *
     H*               Use claim accident description if blank.        *
     H*                                                               *
     H* 07/13/98 C:15 Support: WC051/12451                            *
     H*               Removed a parm from the entry list.             *
     H*                                                               *
     H*  10/30/97 C:58 PROJECT: WC468                                 *
     H*               -Added CO and Fund to W0270R5 parameters.
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   N09  -  MAINTENANCE MODE                                    *
     H*    09  -  INQUIRY MODE                                        *
     H*    40  -  CONTROL ADD/UPDATE MODE                             *
     H*    60  -  GENERAL USAGE                                       *
     H*                                                               *
     H*****************************************************************
     H*
     FW0070FA1  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F*** BEG ADD ***** 08/28/09 **************************************
     F                                     INFDS(DBFDS3)
     F*** END ADD ***** 08/28/09 **************************************
     FWMCDL     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FWMNIL     UF A E           K DISK

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D NT              S              1    DIM(40)
     D NM              S              1    DIM(20)
     D TI              S              1    DIM(19)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  CMCO#                  1      3  0
     D  CMFND                  4      6  0
     D  CMFYR                  7      9  0
     D  CMCASE                10     16  0
     D*** BEG ADD ***** 08/28/09 **************************************
     D DBFDS3          DS
     D  DBRR3B               397    400B 0
     D*** END ADD ***** 08/28/09 **************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*  PROGRAM FILE KEY LISTINGS
     C*****************************************************************
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  STATE NOI INFORMATION FILE
     C*
     C     NIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXST                           ADD 05/18/00
     C*
     C*   KEY LIST FOR THE CLAIM MASTER
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              KFLD                    SCSS#
     C                   KFLD                    CMSS#
      *****  End   Chg  ***  06/09/20  **********************
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHICS FILE
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*** BEG ADD ***** 08/28/09 **************************************
     C*   KEY LIST FOR THE CLAIM FILE
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** END ADD ***** 08/28/09 **************************************
     C*
     C*****************************************************************
     C*  *LIKE DEFINES
     C*****************************************************************
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    NIFND         XXFYR
     C     *LIKE         DEFINE    NICASE        XXCASE
     C     *LIKE         DEFINE    NIST          XXST                           ADD 05/18/00
     C                   MOVE      *BLANK        XXKEY            10
     C*
     C*****************************************************************
     C*  PROGRAM ENTRY PARAMETERS
     C*****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*** BEGIN DLT *** 07/13/98 **************************************
     C*** BEGIN ADD *** 08/26/96 **************************************
     C******************** PARM           QQAZIP  90
     C*** END ADD ***** 08/26/96 **************************************
     C*** END DLT ***** 07/13/98 **************************************
     C                   PARM                    QQRTCD            1
      ***** BEG ADD ******* 10/22/09 **********************************
     C                   PARM                    @@CONT            1
      ***** END ADD ******* 10/22/09 **********************************
     C*
     C     ACDPL3        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM      '1'           QQTRCD            1
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
     C*** BEG ADD ***** 08/28/09 **************************************
     C     ACDPL4        PLIST
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      ***** BEG CHG ******* 10/22/09 **********************************
     C************         PARM 'Y'       @@CONT  1
     C                   PARM                    @@CONT            1
      ***** END CHG ******* 10/22/09 **********************************
     C*** END ADD ***** 08/28/09 **************************************
     C*****************************************************************
     C*  PROGRAM START
     C*****************************************************************
     C*
     C*  MOVE THE PARM FIELDS TO WORK FIELDS
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C*
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD ***** 08/28/09 **************************************
     C                   MOVE      *BLANK        WARN              1
     C*
     C*  IF THIS IS CALLED AS AN INQUIRY PROGRAM, SET ON 09 TO
     C*  CONTROL HOW FIELDS ARE DISPLAYED ON THE SCREEN.
     C*
     C     QQCALL        IFEQ      '1'
     C     QQCALL        OREQ      'Z'
     C                   MOVE      *ON           *IN09
     C                   END
     C*
     C*  FORMAT THE YEAR AND CLAIM NUMBER INTO 1 FIELD (9.0)
     C*
     C                   MOVE      QQFYR         XXYR2             2 0
     C                   MOVE      QQCASE        SCYRCS
     C                   MOVEL     XXYR2         SCYRCS
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
     C*  GET FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        SCFNAM
     C                   ELSE
     C                   MOVE      *BLANKS       SCFNAM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      XXFND         SCFND
     C                   MOVE      Q1USER        SCUSER
     C*
     C*
     C*  GET THE CLAIMS MASTER (SOC SEC NUMBER)
     C*
     C     CMKEY         CHAIN     PCM01                              60
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVEL     SRDTO         QQACDT
     C*
     C                   MOVE      CMJRST        XXST                           ADD 05/18/00
     C*
     C*  GET THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C                   MOVE      *BLANKS       WRK              50
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ALL'*'       SCCNAM
     C                   ELSE
     C     CDLSN         CAT       ',':0         WRK
     C     WRK           CAT       CDFSN:1       WRK
     C     WRK           CAT       CDMID:1       WRK
     C                   MOVEL     WRK           SCCNAM
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, AND LOAD "ADD OR UPDATE" TO
     C*  THE SCREEN.
     C*
     C     NIKY          CHAIN(N)  PNI01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     NI60A1        SC60A1
     C                   MOVEL     NI60A2        SC60A2
     C                   MOVEL     NI60A3        SC60A3
     C                   MOVEL     NI60A4        SC60A4
     C                   MOVEL     NI60A5        SC60A5
     C                   MOVEL     NI60A6        SC60A6
     C                   MOVEL     NI80A1        SC40X4
     C                   MOVE      NI80A1        SC40X7
     C                   MOVEL     NI80A2        SC40X5
     C                   MOVE      NI80A2        SC40X6
     C*
     C*** BEGIN ADD *** 11/22/96 **************************************
     C     NI80A1        IFEQ      *BLANK
     C     NI80A2        ANDEQ     *BLANK
     C                   MOVEL     CMACDS        SC40X4
     C                   MOVE      CMACDS        WORK20           20
     C     WORK20        CAT       CMACD2:0      SC40X5
     C                   MOVE      CMACD2        SC40X6
     C                   ENDIF                                                   SC60A3 *BL
     C*** END ADD ***** 11/22/96 **************************************
     C                   MOVEL     NI1A1         SC1A1
     C                   MOVEL     NI1A2         SC1A2
     C                   MOVEL     NI80A3        SC40X1
     C                   MOVE      NI80A3        SC40X2
     C*** BEG ADD ***** 04/03/03 **************************************
     C                   MOVEL(P)  NI10A2        SCSTHR
     C*** END ADD ***** 04/03/03 **************************************
     C*
     C     ','           SCAN      NIPRNM        X                 2 0
     C     X             IFEQ      0
     C                   CLEAR                   SCPRNM
     C                   CLEAR                   SCPRTI
     C                   ELSE
     C     X             IFEQ      1
     C                   CLEAR                   SCPRNM
     C     19            SUBST     NIPRNM:2      SCPRTI
     C                   ELSE
     C     X             SUB       1             END               2 0
     C     X             ADD       1             STR               2 0
     C     END           SUBST     NIPRNM:1      SCPRNM
     C     19            SUBST     NIPRNM:STR    SCPRTI
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      NIPRPH        SCPRPH
     C                   MOVE      NITIM2        SCTIM2
     C*
     C                   ELSE
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SC60A1
     C                   MOVE      *BLANK        SC60A2
     C                   MOVE      *BLANK        SC60A3
     C                   MOVE      *BLANK        SC60A4
     C                   MOVE      *BLANK        SC60A5
     C                   MOVE      *BLANK        SC60A6
     C                   MOVE      *BLANK        SC40X4
     C                   MOVE      *BLANK        SC40X5
     C                   MOVE      *BLANK        SC40X6
     C                   MOVE      *BLANK        SC1A1
     C                   MOVE      *BLANK        SC1A2
     C                   MOVE      *BLANK        SC40X1
     C                   MOVE      *BLANK        SC40X2
     C*** BEG ADD ***** 04/03/03 **************************************
     C                   MOVE      *BLANK        SCSTHR
     C*** END ADD ***** 04/03/03 **************************************
     C                   MOVE      *BLANK        SCTIM2
     C                   MOVE      *BLANK        SCPRNM
     C                   MOVE      *BLANK        SCPRTI
     C                   MOVE      *BLANK        SCPRPH
     C                   MOVE      *BLANK        SCSTHR
     C                   ENDIF
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
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*****************************************************************
     C*
B001 C     *IN11         IFEQ      *ON
     C*
     C***************************
     C*   F3 = END OF JOB
     C***************************
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C********************************
     C*   F12 = PREVIOUS SCREEN
     C********************************
     C     *INKL         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C********************************
     C*   COMMAND 11 = EDI LOG HISTORY
     C********************************
     C*
     C     *INKK         IFEQ      *ON
     C*** BEGIN ADD *** 10/30/97 **************************************
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXCO#         QQCO#
     C*** END ADD ***** 10/30/97 **************************************
     C                   CALL      'W0270R5'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL
     C                   PARM                    QQCASE
     C                   PARM                    QQFYR
     C*** BEGIN ADD *** 10/30/97 **************************************
     C                   PARM                    QQFND
     C                   PARM                    QQCO#
     C*** END ADD ***** 10/30/97 **************************************
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C**************************************************************
E001 C                   ENDIF                                                  END - CMD KEYS
     C*****************************************************************
     C*   END - PROCESS ALL COMMAND KEYS
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN 1
     C*****************************************************************
     C*
     C     SCR01T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ1
     C*
     C*** Begin dlt *** 08/25/06 **************************************
     C******     SCPRNM    IFEQ *BLANKS
     C******               SETON                     24
     C******               GOTO STRT
     C******               ENDIF
     C*
     C******     SCPRTI    IFEQ *BLANKS
     C******               SETON                     25
     C******               GOTO STRT
     C******               ENDIF
     C*
     C******     SCPRPH    IFEQ *ZEROS
     C******               SETON                     26
     C******               GOTO STRT
     C******               ENDIF
     C*** End dlt ***** 08/25/06 **************************************
     C*
     C     INQ1          TAG
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN 2
     C*****************************************************************
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ2
     C*
     C*
     C*  WERE SAFEGAURDS PROVIDED MUST BLANK, Y, OR N
     C*
     C     SC1A1         IFNE      *BLANK
     C     SC1A1         ANDNE     'Y'
     C     SC1A1         ANDNE     'N'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  WERE THEY USED MUST BE BLANK, Y, OR N
     C*
     C     SC1A2         IFNE      *BLANK
     C     SC1A2         ANDNE     'Y'
     C     SC1A2         ANDNE     'N'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     INQ2          TAG
     C*
     C*    ACCESS THE RECORD TO BE UPDATED OR WRITTEN
     C*
     C     NIKY          CHAIN     PNI01                              40
     C*
     C                   MOVE      XXCO#         NICO#
     C                   MOVE      XXFND         NIFND
     C                   MOVE      XXFYR         NIFYR
     C                   MOVE      XXCASE        NICASE
     C                   MOVEL     SC60A1        NI60A1
     C                   MOVEL     SC60A2        NI60A2
     C                   MOVEL     SC60A3        NI60A3
     C                   MOVEL     SC60A4        NI60A4
     C                   MOVEL     SC60A5        NI60A5
     C                   MOVEL     SC60A6        NI60A6
     C                   MOVEL     SC40X4        NI80A1
     C                   MOVE      SC40X7        NI80A1
     C                   MOVEL     SC40X5        NI80A2
     C                   MOVE      SC40X6        NI80A2
     C                   MOVEL     SC1A1         NI1A1
     C                   MOVEL     SC1A2         NI1A2
     C                   MOVEL     SC40X1        NI80A3
     C                   MOVE      SC40X2        NI80A3
     C                   MOVE      CMJRST        NIST                           ADD 05/18/00
     C                   MOVEL     SCTIM2        NITIM2
     C     SCPRNM        CAT(P)    ',':0         NIPRNM
     C                   CAT       SCPRTI:0      NIPRNM
     C                   MOVE      SCPRPH        NIPRPH
     C*
     C                   MOVE      Q1USER        NIIUSR
     C                   Z-ADD     DSTIME        NIITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIIDT
     C                   MOVE      Q1USER        NIUSER
     C                   MOVE      Q1PGM         NIPGM
     C                   Z-ADD     DSTIME        NITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIDT
     C*** BEG ADD ***** 04/03/03 **************************************
     C                   MOVEL(P)  SCSTHR        NI10A2
     C*** END ADD ***** 04/03/03 **************************************
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PNI01
     C                   ELSE
     C                   MOVE      Q1USER        NIIUSR
     C                   Z-ADD     DSTIME        NIITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NIIDT
     C                   WRITE     PNI01
     C                   END
     C*
      *  -------------------------------------------------------
      *  END THE PROGRAM
      *
      *
      *  IF QQCALL = A, GO ON TO THE NEXT PROGRAM IN ADD MODE.
      *  IF QQCALL = Y, GO ON TO THE NEXT PROGRAM IN CHANGE MODE.
      *  IF QQCALL = Z, GO ON TO THE NEXT PROGRAM IN DISPLAY MODE.
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C     CLTID         IFNE      '133'
      *
     C*** END ADD ***** 08/28/09 **************************************
     C     QQCALL        IFEQ      'Y'
     C     QQCALL        OREQ      'Z'
     C                   MOVE      *BLANK        QQTRCD
     C                   CALL      'W0190R'      ACDPL3                 99
      *
     C     QQTRCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   END
      *
     C     QQTRCD        IFEQ      'A'
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QAKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   END
     C                   END                                                     QQCALL = Y,Z
      ** **
     C     QQCALL        IFEQ      'A'
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   SELECT
     C     CXDBEN        WHENEQ    'Y'
     C                   MOVE      *BLANK        QQTRCD
     C                   CALL      'W0190R'      ACDPL3                 99
      *
     C     CXDART        WHENEQ    'Y'
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QAKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   ENDSL
      *
     C     QQTRCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C                   END                                                     *IN60
     C                   END                                                     QQCALL = A
      *
     C*** BEG ADD ***** 08/28/09 **************************************
     C                   ENDIF                                                   CLTID <> 133
      *
     C     CLTID         IFEQ      '133'
     C     QQCALL        ANDNE     *BLANKS
      *
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     DBRR3B        @@RRN
      *
     C                   SELECT
     C     QQCALL        WHENEQ    'A'
     C                   MOVEL     'A'           @@MODE
     C     QQCALL        WHENEQ    'Y'
     C                   MOVEL     'C'           @@MODE
     C     QQCALL        WHENEQ    'Z'
     C                   MOVEL     'D'           @@MODE
     C                   ENDSL
      *
     C                   CALL      'W0070R08'    ACDPL4                   99
      *
     C   KL              MOVE      'B'           QQRTCD
     C   KC              MOVE      'A'           QQRTCD
      *
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C                   ENDIF                                                   CLTID = 133
     C*** END ADD ***** 08/28/09 **************************************
      *
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
     C**************************************************************************
**  TABLE UA
-- ADD --
- UPDATE -
