     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0001R - COMPANY MASTER AUXILIARY MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     THIS PROGRAM PROVIDES MAINTENANCE FOR THE SYSTEM       **
     H**     COMPANY FILE - PROPERTY & CASUALTY FIELDS              **
     H**   CALLED FROM:                                             **
     H**     MENU                                                   **
     H**   CALLS TO:                                                **
     H**     C2161R - Search of a/r code                            **
     H**     S0501R - Search of company                             **
     H**     C2001R - Search of section code                        **
     H**     C2041R - Search of transaction code                    **
     H**     M0006C - Help request                                  **
     H**   PROGRAMMER: LJQ                                          **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      09      Inquiry Only                                  **
     H**      40      General Usage (CHAIN)                         **
     H**      KC     Function 03 - EXIT                             **
     H**      KE     Function 05 - Restart (screen 2)               **
     H**      KF     Function 06 - Company Search (screen 1)        **
     H**      KJ     Function 10 - Table Search                     **
     H**      KL     Function 12 - CANCEL                           **
     H**  SCR01:                                                    **
     H**      20      CO# <> be blanks. (MCR0500)                   **
     H**      21      Record not found; inquiry mode. (MCR0505)     **
     H**      22      Invalid CO#; CO# must exist before adding P&C **
     H**              data.  (MCR0507)                              **
     H**  SCPC1:                                                    **
     H**      21      CO Short name <> blanks. (CCR501)             **
     H**      23      Mailing City <> blanks. (CCR501)              **
     H**      24      Mailing State <> blanks. (CCR505)             **
     H**      25      Mailing Zip <> 0. (CCR0504)                   **
     H**      26      Physical City <> blanks. (CCR501)             **
     H**      27      Mailing State <> blanks. (CCR505)             **
     H**      28      Physical Zip <> 0. (CCR0504)                  **
     H**  SCPC2:                                                    **
     H**      20      Admin Co Name <> blanks. (CCR0501)            **
     H**      21      Admin address 1 <> blanks (CCR0501)           **
     H**      25      Check Number invalid. (CCR0506)               **
     H**      26      Case number <> 0  (CCR0504)                   **
     H**      27      ISO Assign Co <> Blanks (CCR0501)             **
     H**      28      ISO Trans type = 1,2,3  (CCR0525)             **
     H**      31      A/R Trans code invalid (CCR0567)              **
     H**      32      A/R status = I    (CCR0536)                   **
     H**      33      Sec code is invalid (CCR0630)                 **
     H**      34      Sec code  status = I (CCR0536)                **
     H**      35      Sec code is invalid for policy fee (CCR0631)  **
     H**      38      A/R cancel code invalid (CCR0567)             **
     H**      39      A/R cancel code status = I (CCR0536)          **
     H**      43      Pol Tran Code is invalid (CCR0524)            **
     H**      44      Pol Tran Code status = I (CCR0536)            **
     H**      45      Pol Tran Code value must = E (CCR0837)        **
     H*       99      PROTECT CHECK#'S AND CLAIM# (QQCALL = 2)      **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**    C0001F:  Screen Display                                 **
     H**    SMCOL (PCO01): CO                                       **
     H****************************************************************
     F*
     F* Screen File
     FC0001F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     UF A E           K DISK

     D*
     D* Define Tables and Arrays
     D UA              S             10    DIM(2) CTDATA PERRCD(1)              Update/Add Notes
     D ST              S              2    DIM(52) CTDATA PERRCD(26)            State
     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Screen Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* External File Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Comany Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C* Entry Parameters
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
     C* Initialize and Set-up
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C     QQCALL        COMP      '2'                                    99
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
     C     Q2RECF        IFEQ      'SCPC1'                                      P&C SCRN 1
     C                   EXFMT     SCPC1
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCPC2'                                      P&C SCRN 2
     C                   EXFMT     SCPC2
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  Function 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  Function 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCPC1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCPC2'
     C                   MOVEL     'SCPC1'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*Function 05 = RESTART
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCPC2'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCCO#
     C                   GOTO      STRT
     C                   END                                                    SCRN PC2
     C                   END                                                    F5 End
     C*
     C*  Function 06 = SEARCH COMPANY
     C*
     C     *INKF         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'S0501R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCCO#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCO#         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    SCRN 1
     C                   END                                                    F6 END
     C*
     C*
     C*  Function 10 = TABLE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCPC2'
     C*
     C*   SEARCH A/R TRANSACTION CODE - CANCELLATION CODE
     C*
     C                   MOVEL     SCCNAR        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C*                    MOVELSCCNAR    XXKEY
     C                   CALL      'C2161R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCCNAR
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCPC2'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF '? '
     C*
     C*   SEARCH A/R TRANSACTION CODE
     C*
     C                   MOVEL     SCPFAR        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C*                    MOVELSCPFAR    XXKEY
     C                   CALL      'C2161R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCPFAR
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCPC2'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF '? '
     C*
     C*   SEARCH SECTION CODE
     C*
     C                   MOVEL     SCPFSC        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C*                    MOVELSCPFSC    XXKEY
     C                   CALL      'C2001R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCPFSC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCPC2'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C                   END                                                    ENDIF '? '
     C*
     C*   SEARCH TRANSACTION CODE FOR DELETION OF AUTO, LOCATION/BUILDING
     C*
     C                   MOVEL     SCTRDL        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2041R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCTRDL
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCPC2'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END                                                    ENDIF SCPC2
     C*
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKJ
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCPC1'       SCPC1T
     C     Q2RECF        CABEQ     'SCPC2'       SCPC2T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
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
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UA(2)         SCMODE
     C*
     C* PROPERTY & CASUALTY SCREEN
     C*
     C                   MOVE      CONAME        SCNAME
     C                   MOVE      COSHNM        SCSHNM
     C                   MOVE      COMAD1        SCMAD1
     C                   MOVE      COMAD2        SCMAD2
     C                   MOVE      COMCTY        SCMCTY
     C                   MOVE      COMST         SCMST
     C                   MOVE      COMZIP        SCMZP2
     C                   MOVEL     COMZIP        SCMZP1
     C                   MOVE      COPAD1        SCPAD1
     C                   MOVE      COPAD2        SCPAD2
     C                   MOVE      COPCTY        SCPCTY
     C                   MOVE      COPST         SCPST
     C                   MOVE      COPZIP        SCPZP2
     C                   MOVEL     COPZIP        SCPZP1
     C                   MOVE      COAREA        SCAREA
     C                   MOVE      COPH1         SCPH1
     C                   MOVE      COPH2         SCPH2
     C                   MOVE      COCHK1        SCCHK1
     C                   MOVE      COCHK2        SCCHK2
     C                   MOVE      CONACS        SCCASE
     C                   MOVE      COCOI         SCCOI
     C                   MOVE      COISO         SCISO
     C                   MOVE      COANAM        SCANAM
     C                   MOVE      COADR1        SCAAD1
     C                   MOVE      COADR2        SCAAD2
     C                   MOVE      COCTY         SCACTY
     C                   MOVE      COST          SCAST
     C                   MOVEL     COZIP         SCAZP1
     C                   MOVE      COZIP         SCAZP2
     C                   MOVE      COPFAR        SCPFAR
     C                   MOVE      COPFSC        SCPFSC
     C                   MOVE      COCNAR        SCCNAR
     C                   MOVE      COARRE        SCARRE
     C                   MOVE      COTRDL        SCTRDL
     C                   MOVE      COTCC         SCTCC
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* Company must exist before allowing user to add P&C additional
     C* information
     C*
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCPC1'       Q2RECF
     C                   GOTO      STRT
     C*
     C* PROPERTY & CASUALTY SCREENS
     C*
     C     SCPC1T        TAG
     C*
     C*  SHORT NAME CANNOT BE BLANK
     C*
     C     SCSHNM        IFEQ      *BLANK
     C*
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  IF THE ADDRESS LINE IS NOT BLANK WE HAVE TO HAVE MORE INFO
     C*
     C     SCMAD1        IFNE      *BLANK
     C*
     C*  CITY CANNOT BE EQUAL TO BLANK
     C*
     C     SCMCTY        IFEQ      *BLANK
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  STATE CANNOT BE BLANK
     C*
     C                   Z-ADD     1             IN                5 0
     C     SCMST         LOOKUP    ST(IN)                                 50
     C     *IN50         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  ZIP CODE CANNOT BE BLANK
     C*
     C     SCMZP1        IFLE      *ZERO
     C*
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C*
     C                   END
     C                   END
     C*
     C*  IF THE ADDRESS LINE IS NOT BLANK WE HAVE TO HAVE MORE INFO
     C*
     C     SCPAD1        IFNE      *BLANK
     C*
     C*  CITY CANNOT BE EQUAL TO BLANK
     C*
     C     SCPCTY        IFEQ      *BLANK
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  STATE CANNOT BE BLANK
     C*
     C                   Z-ADD     1             IN                5 0
     C     SCPST         LOOKUP    ST(IN)                                 50
     C     *IN50         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  ZIP CODE CANNOT BE BLANK
     C*
     C     SCPZP1        IFLE      *ZERO
     C*
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C*
     C                   END
     C                   END
     C*
     C*  IRS TRANSMITTER CONTROL CODE CANNOT BE BLANK
     C*
     C*
     C                   MOVEL     'SCPC2'       Q2RECF
     C                   GOTO      STRT
     C*
     C* SCREEN 2 FOR P&C
     C*
     C     SCPC2T        TAG
     C*
     C*
     C*  THE ADMIN NAME CANNOT BE EQUAL TO BLANK
     C*
     C     SCANAM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE ADDRESS LINE CANNOT BE EQUAL TO BLANK
     C*
     C     SCAAD1        IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  CITY CANNOT BE EQUAL TO BLANK
     C*
     C     SCACTY        IFEQ      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  STATE CANNOT BE BLANK
     C*
     C                   Z-ADD     1             IN                5 0
     C     SCAST         LOOKUP    ST(IN)                                 50
     C     *IN50         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  ZIP CODE CANNOT BE BLANK
     C*
     C     SCAZP1        IFLE      *ZERO
     C*
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  CHECK NUMBERS MUST NOT BE ZERO
     C*
     C     SCCHK1        IFEQ      *ZERO
     C     SCCHK2        OREQ      *ZERO
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  CLAIM NUMBER MUST NOT BE ZERO
     C*
     C     SCCASE        IFEQ      *ZERO
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  ISO ASSIGNED NUMBER CANNOT BE BLANK
     C*
     C     SCCOI         IFEQ      *BLANK
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  ISO TRANSACTION TYPE MUST BE '1', '2', OR '3'.
     C*
     C     SCISO         IFNE      '1'
     C     SCISO         ANDNE     '2'
     C     SCISO         ANDNE     '3'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     ENDIT         TAG
     C*
     C                   Z-ADD     SCCO#         COCO#
     C                   MOVE      *BLANK        COSTAT
     C                   MOVE      Q1USER        COUSER
     C                   MOVE      Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   Z-ADD     DSTIME        COTIME
     C*
     C* PROPERTY & CASUALTY SCREEN
     C*
     C                   MOVE      SCSHNM        COSHNM
     C                   MOVE      SCMAD1        COMAD1
     C                   MOVE      SCMAD2        COMAD2
     C                   MOVE      SCMCTY        COMCTY
     C                   MOVE      SCMST         COMST
     C                   MOVE      SCMZP2        COMZIP
     C                   MOVEL     SCMZP1        COMZIP
     C                   MOVE      SCPAD1        COPAD1
     C                   MOVE      SCPAD2        COPAD2
     C                   MOVE      SCPCTY        COPCTY
     C                   MOVE      SCPST         COPST
     C                   MOVE      SCPZP2        COPZIP
     C                   MOVEL     SCPZP1        COPZIP
     C                   MOVE      SCAREA        COAREA
     C                   MOVE      SCPH1         COPH1
     C                   MOVE      SCPH2         COPH2
     C                   MOVE      SCCHK1        COCHK1
     C                   MOVE      SCCHK2        COCHK2
     C                   MOVE      SCCASE        CONACS
     C                   MOVE      SCCOI         COCOI
     C                   MOVE      SCISO         COISO
     C                   MOVE      SCANAM        COANAM
     C                   MOVE      SCAAD1        COADR1
     C                   MOVE      SCAAD2        COADR2
     C                   MOVE      SCACTY        COCTY
     C                   MOVE      SCAST         COST
     C                   MOVEL     SCAZP1        COZIP
     C                   MOVE      SCAZP2        COZIP
     C                   MOVE      SCPFAR        COPFAR
     C                   MOVE      SCPFSC        COPFSC
     C                   MOVE      SCCNAR        COCNAR
     C                   MOVE      SCARRE        COARRE
     C                   MOVE      SCTRDL        COTRDL
     C                   MOVE      SCTCC         COTCC
     C*
     C     *IN40         IFEQ      '1'
     C*
     C                   MOVE      *ZERO         COBUL1
     C                   MOVE      *ZERO         COBUL2
     C                   MOVE      *ZERO         COBUL3
     C                   MOVE      *ZERO         COBUL4
     C                   MOVE      *ZERO         COBUL5
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   MOVE      Q1USER        COUSER
     C*
     C                   END
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCO01
     C                   ELSE
     C                   WRITE     PCO01
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCCO#
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
