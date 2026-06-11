     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0035R - GROUP CHECK PROVIDER MAINTENANCE
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER   *
     H*   GROUP TYPE PROVIDERS INTO THE SYSTEM.                       *
     H* CALLED FROM: NONE                                             *
     H* CALLS TO:    NONE                                             *
     H* PROGRAMMER:  C:15                                             *
     H* CREATED:     09/06/94       PROJECT: WC361                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/21/22 C:079 Support: PHW102560                             *
     H*                Remove the informational message that was      *
     H*                indicating that an EOMB cannot be attached to  *
     H*                a group check when the provider is setup as O. *
     H*                EOB's do print in this case (see W7002R)       *
     H*                                                               *
     H* 04/07/04 C:062 PROJECT: WC1220                                *
     H*                ADD PROCESSING FOR PRINT DETAIL FLAG           *
     H*                                                               *
     H* 05/28/03 C:062 PROJECT: 129MBO                                *
     H*                ALLOW NEW TYPES 1-3.                           *
     H*                                                               *
     H* 01/17/02 C:062 PROJECT: WC095 30818                           *
     H*                LIFT EDIT ON ONE 'B' TYPE LIMIT EDIT.          *
     H*                                                               *
     H* 01/15/02 C:062 PROJECT: WC095 30747                           *
     H*                LIFT EDIT ON SAME FEIN EDIT FOR 'B' AND 'N'.   *
     H*                                                               *
     H* 11/29/99 C:118 PROJECT: WC577                                 *
     H*                Display file fixes, program name and F keys.   *
     H*                                                               *
     H* 03/03/99 C:112 Support# 11733, WC065                          *
     H*          Allow an 'O' type group provider type is '1' or '4'. *
     H*          Display an informational message indicating an EOMB  *
     H*          cannot be attched to a group check.                  *
     H*                                                               *
     H* 12/15/97  C:80  Project: WC659                                *
     H*                 Added edits for "Group Type on SCR02.         *
     H*                                                               *
     H* 04/15/97  C:30  Project: WC607                                *
     H*                 Change FEIN# SRCH & ALPHA SRCH to PAYEE SRCH. *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*     60  -  GENERAL CHAIN                                      *
     H*   SCR01                                                       *
     H*     20  -  INVALID PROVIDER                                   *
     H*   SCR02                                                       *
     H*     20  -  INVALID GROUP TYPE ENTERED                         *
     H*     21  -  GROUP TYPE NOT ALLOWED                             *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*     WMUPL (PUP01): Q1USER                                     *
     H*     SMCOL (PCO01): CO#                                        *
     H*     WMPDL (PPD01): FEIN/MOD                                   *
     H*     WMNRL (PNR01): CO#/FEIN/MOD                               *
     H*     WMNRL1(PNR02): CO#/GRTY/STAT(OMIT NE 'A')                 *
     H*****************************************************************
     FW0035F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMNRL     UF A E           K DISK
     F*** BEGIN DEL *** 01/17/02 **************************************
     F**********WMNRL1  IF  E           K        DISK
     F**********  PNR01                             KRENAMEPNR02
     F*** END   DEL *** 01/17/02 **************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SSY01)

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
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
     C*  COMPANY MASTER FILE KEY LIST
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE KEY LIST
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  PAYEE DEMOGRAPHICS FILE KEY LIST
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  REVIEW ID FILE KEY LIST
     C*
     C     NRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     NRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXGRTY
     C*
     C     NRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C*
     C*  WORK COMP CONTROL FILE
     C*
     C     WCKY          KLIST
     C                   KFLD                    XCKEY
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
     C     *LIKE         DEFINE    NRCO#         XXCO#
     C     *LIKE         DEFINE    NRFEIN        XXFEIN
     C     *LIKE         DEFINE    NRMOD         XXMOD
     C     *LIKE         DEFINE    NRGRTY        XXGRTY
     C     *LIKE         DEFINE    WCKEY         XCKEY
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET SYSTEM VALUE FOR GROUP CHECKS.
     C*
     C                   MOVE      *BLANKS       XCKEY
     C                   MOVEL     'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      '1'
     C     WCFL04        ORNE      'Y'
     C                   GOTO      TERM
     C                   END
     C*
     C* CHECK TO SEE IF CHECK DETAIL SHOULD BE SEEN
     C*
     C                   SETOFF                                       16
     C     *IN60         IFEQ      *OFF
     C     WCFL27        ANDEQ     'Y'
     C                   SETON                                        16
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
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C**************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C******************************************
     C*   CMD 1-END OF JOB                     *
     C******************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C******************************************
     C*   CMD 2-PRIOR SCREEN                   *
     C******************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C******************************************
     C*   CMD 5-PROVIDER # SEARCH              *
     C******************************************
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
      *
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTC             1
      *
     C     QQRTC         CABEQ     'A'           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C*
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   GOTO      SCR01T
     C                   ENDIF
      *
     C                   END
     C                   END
     C******************************************
     C*   CMD 6-GROUP PROVIDER # SEARCH        *
     C******************************************
     C     *INKF         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'W0036R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFEIN        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C                   END
      *
      *
     C******************************************
     C*   CMD 8-GROUP PROVIDER ALPHA SEARCH    *
     C******************************************
     C     *INKH         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'W0037R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFEIN        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C                   END
     C******************************************
     C*   COMMAND 20 -  CHANGE STATUS          *
     C******************************************
     C     *INKU         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END                                                    END *INKU
     C******************************************
     C*   PROCESS HELP REQUESTS                *
     C******************************************
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
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C******************************************
     C*   CMD 13-CHG USER PARAMETERS           *
     C******************************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*****************************************************************
     C                   END                                                    END CMDKEYS
     C******************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C*******************************************
     C     SCR01T        TAG
     C*******************************************
     C*
     C*  VALIDATE THE PROVIDER ENTERED
     C*
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   MOVE      PDNAME        SCNAME
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C     NRKY          CHAIN     PNR01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PNR01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      NRGRTY        SCTYPE
     C                   MOVE      NRCHKD        SCCHKD
     C                   MOVE      NRSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *BLANKS       SCCHKD
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*******************************************
     C     SCR02T        TAG
     C*******************************************
     C*
     C*  THE GROUP TYPE MUST BE 'B', 'N', 'O', '1', '2' OR '3'
     C*
     C     SCTYPE        IFNE      'B'
     C     SCTYPE        ANDNE     'N'
     C     SCTYPE        ANDNE     'O'
     C     SCTYPE        ANDNE     '1'
     C     SCTYPE        ANDNE     '2'
     C     SCTYPE        ANDNE     '3'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ONLY ONE ACTIVE 'B' AND ONE ACTIVE 'N' TYPE PROVIDERS ARE
     C*  ALLOWED FOR THE GROUP PROVIDER FILE.
     C*
     C     SCSTAT        IFNE      'I'
     C*
     C*
     C*** Beg Del *** 06/21/22 ****************************************
     C* GROUP PROVIDER TYPE 'O' CAN'T BE SETUP IF THE PROVIDER TYPE IS
     C* '1' OR '4'.
     C****
     C**** SCTYPE        IFEQ      'O'
     C****
     C****               MOVE      SCFEIN        XXFEIN
     C****               MOVE      SCMOD         XXMOD
     C**** PDKY          CHAIN     PPD01                              60
     C****
     C**** *IN60         IFEQ      *OFF
     C****
     C**** PDTYPE        IFEQ      '1'
     C**** PDTYPE        OREQ      '4'
     C**** PASS1         IFNE      'N'
     C****               MOVE      'N'           PASS1             1
     C****
     C****               MOVE      *ON           *IN24
     C****               GOTO      STRT
     C****               ENDIF
     C****
     C****               ENDIF                                                  PDTYPE IFEQ '1'
     C****
     C****               ENDIF                                                  *IN60 IFEQ *OFF
     C****
     C****               ENDIF                                                  SCTYPE IFNE *ZERO
     C*** End Del *** 06/21/22 ****************************************
     C*
     C                   ENDIF                                                  SCSTAT IFNE 'I'
     C     *IN16         IFEQ      *ON
     C     SCCHKD        ANDNE     *BLANKS
     C     SCCHKD        ANDNE     'Y'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C     NRKY          CHAIN     PNR01                              40
     C                   Z-ADD     XXCO#         NRCO#
     C                   Z-ADD     SCFEIN        NRFEIN
     C                   Z-ADD     SCMOD         NRMOD
     C                   MOVEL     PDNAME        NRNAME
     C                   MOVEL     PDFSN         NRFSN
     C                   MOVE      SCTYPE        NRGRTY
     C                   MOVE      SCCHKD        NRCHKD
     C                   MOVE      SCSTAT        NRSTAT
     C                   MOVE      Q1USER        NRUSER
     C                   MOVE      Q1PGM         NRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NRDT
     C*
     C                   Z-ADD     DSTIME        NRTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PNR01
     C                   ELSE
     C                   Z-ADD     0             NRNTWF
     C                   Z-ADD     0             NRBLRF
     C                   MOVE      Q1USER        NRIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NRIDT
     C*
     C                   WRITE     PNR01
     C                   END
     C*** Beg Del *** 06/21/22 *****************************************
     C****************   MOVE      *BLANK        PASS1
     C*** End Del *** 06/21/22 *****************************************
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ZERO         SCFEIN
     C                   MOVE      *ZERO         SCMOD
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPNR01     E            PNR01E
      *====================================================================*
**  TABLE UA
-- ADD --
- UPDATE -
