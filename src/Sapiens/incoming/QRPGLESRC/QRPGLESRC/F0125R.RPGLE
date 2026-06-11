     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0125R - WCMC EDI CONTROL MAINTENANCE
     H*****************************************************************
     H**   WCMC Claims Control Maintenance                           **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   06/02/99      PROJECT: KY010                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 03/26/07 C:182 Project: MC281
     H*                Added Fld for 'Process denied bills'
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*  01/06/00 C:53  PROJECT: KY010
     H*                 ADDED DARTS REMARK CODE PROCESSING.
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09      Inquiry mode                                   **
     H**      10      Help key                                       **
     H**      11      Valid command key                              **
     H**      20      SCR02  - SCCSDT is invalid date                **
     H**      21      SCR01  - SCJRST can't be blank                 **
     H**      21      SCR02  - SCMAXT can't be less than or equal 0  **
     H**      21      SCR837 - position cursor at SC837              **
     H**      22      SCR01  - invalid state code for SCJRST         **
     H**      22      SCR02  - SCSEIN can't be blank                 **
     H**      22      SCR837 - position cursor at SC837A             **
     H**      23      SCR02  - SCSZP can't be blank                  **
     H**      24      SCR01  - invalid table value for SCTSET        **
     H**      24      SCR02  - SCREIN can't be blank                 **
     H**      25      SCR01  - position cursor at SCTSET             **
     H**      25      SCR02  - SCRZP can't be blank                  **
     H**      26      SCR837 - invalid table for SCLOG               **
     H**      27      SCR837 - invalid table for SC837               **
     H**      28      SCR837 - invalid table for SC837A              **
     H**      31      Transaction Set ID not found in table file     **
     H**      40      Add mode                                       **
     H**      60      General usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FEEXL  (PEX01): WCMC EDI Control File (CO#/JRST/TSET)  **
     H**      FTGTL  (PGT01): Generic table file (CO#/ID/KEY)        **
     H**      SMCOL  (PCO01): System company file for work comp (CO#)**
     H**      SMUPL  (PUP01): User Parameters File (USR)             **
     H**      STABL                                                  **
     H**             (PST01): State Code Table File (KEY)            **
     H*****************************************************************
     H*
     FF0125F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFEEXL     UF A E           K DISK
     FFTGTL     IF   E           K DISK
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F*** BEG ADD *** 01/06/00 ********************************
     FSTCTL1    IF   E           K DISK
     F*** END ADD *** 01/06/00 ********************************

     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D*
     D* ---------------------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* ---------------------------------------------------------------
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* ---------------------------------------------------------------
     D* SYSTEM DATE AND TIME
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

     C*
     C*
     C* ---------------------------------------------------------------
     C* PARM LISTS
     C* ---------------------------------------------------------------
     C*
     C*          ENTRY PARM LIST
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
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C* ---------------------------------------------------------------
     C* KEY LISTS
     C* ---------------------------------------------------------------
     C*          KEY LIST FOR THE COMPANY FILE
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*          USER PARAMETER FILE
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*
     C*          EDI CLAIMS CONTROL FILE
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTSET
     C*
     C*          STATE CODE TABLE FILE
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*          Generic Table File
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*** BEG ADD *** 01/06/00 ********************************
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
     C*** END ADD *** 01/06/00 ********************************
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    SCJRST        XXJRST
     C     *LIKE         DEFINE    SCTSET        XXTSET
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    EXCSDT        XXCSDT                           Julian 7.0
     C     *LIKE         DEFINE    EXCSDT        XXDSDT                         Today julian
     C     *LIKE         DEFINE    GTTBID        XXTBID                         Generic Tbl
     C     *LIKE         DEFINE    GTTBID        QQTBID                         Parms/Genric
     C     *LIKE         DEFINE    GTKEY         QQKEY                           tbl search
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Initialize Parms and get time
     C*          ------------------------------------------------------
     C                   MOVE      Q1USER        XXUSER
     C                   MOVE      Q1PGM         XXPGM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*          Set up SCR01
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       SCJRST
     C                   MOVE      *BLANKS       SCTSET
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*          Get User Parameter file
     C*          ------------------------------------------------------
     C     *INLR         DOWEQ     *OFF
     C     Q2RECF        IFEQ      'SCR01'
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  RECF = 'SCR01'
     C                   MOVE      UPCO#         XXCO#
     C*
     C*          Get Company Master file
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL(P)  SRDFLD        SCCONM
     C                   MOVEL(P)  SRDFLD        XXCONM
     C                   ENDIF                                                  RECF = 'SCR01'
     C*
     C*          Process appropriate SCREEN
     C*          ------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  RECF = 'SCR01'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF                                                  RECF = 'SCR02'
     C*
     C     Q2RECF        IFEQ      'SCR837'
     C                   EXFMT     SCR837
     C                   ENDIF                                                  RECF = 'SCR837'
     C*
     C*          Process Function Keys
     C*          ------------------------------------------------------
     C   11              EXSR      SR11
     C   11              ITER
     C   LR              LEAVE                                                  VVV Leave
     C*
     C*          Process appropriate screen
     C*          ------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXSR      SC01T
     C                   ITER
     C                   ENDIF                                                  RECF = 'SCR01'
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXSR      SC02T
     C                   ITER
     C                   ENDIF                                                  RECF = 'SCR02'
     C     Q2RECF        IFEQ      'SCR837'
     C                   EXSR      SC837T
     C                   ITER
     C                   ENDIF                                                  RECF = 'SCR837'
     C                   ENDDO                                                  DO Loop
     C*
     C**************************************************************************
     C*   SR11 - PROCESS ALL COMMAND KEYS
     C**************************************************************************
     C     SR11          BEGSR
     C                   DO
     C*
     C*          F3 = END OF JOB
     C*          ------------------------------------------------------
     C     *INKC         IFEQ      *ON
     C                   SETON                                        LR
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *INKC = *On
     C*
     C*          F10 = SEARCH
     C*          ------------------------------------------------------
     C     *INKJ         IFEQ      *ON
     C*
     C*          Process SCR01 searches
     C*          ------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*             Jurisdiction State Search
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCJRST'
     C                   CALL      'SY600R'
     C                   PARM      *BLANKS       QQKEY
     C                   PARM      ' '           QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   SETON                                        LR
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'A'
     C     QQRTCD        IFEQ      '*'
     C     QQKEY         ANDNE     *BLANKS
     C                   MOVEL(P)  QQKEY         SCJRST
     C                   MOVEL(P)  QQKEY         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCJRSD
     C                   SETON                                        25        POS CUSR
     C                   ENDIF                                                  RTCD = '*'
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  HFLD = 'SCJRST'
     C*
     C*             Transaction Set ID search
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCTSET'
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'TSET'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   SETON                                        LR
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'A'
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   SETON                                        25
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'B'
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCTSET
     C                   MOVEL(P)  'TSET'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCTRDS
     C                   ENDIF                                                  RTCD = '*'
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  HFLD = 'SCTSET'
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RECF = 'SCR01'
     C*
     C*          Process SCR02 searches
     C*          ------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR02'
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RECF = 'SCR02'
     C*
     C*          Process SCR837 searches
     C*          ------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR837'
     C*
     C*             Trigger for 837 LOG search
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SCLOG'
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'CLOG'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      'A'
     C                   SETON                                        LR
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'A'
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   SETON                                        21
     C                   MOVEL(P)  QQKEY         SCLOG
     C                   MOVEL(P)  'CLOG'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCLOGD
     C                   ENDIF
     C*
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  HFLD = 'SCLOG'
     C*
     C*             837 file format search
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SC837'
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'FFOR'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   SETON                                        LR
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'A'
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   SETON                                        21
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'B'
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SC837
     C                   MOVEL(P)  'FFOR'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SC837D
     C                   SETON                                        22
     C                   ENDIF                                                  RTCD = '*'
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  HFLD = 'SC837'
     C*
     C*             837/824 file format search
     C*             ---------------------------------------------------
     C     HFLD          IFEQ      'SC837A'
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'FFOR'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   SETON                                        LR
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'A'
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   SETON                                        22
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RTCD = 'B'
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SC837A
     C                   MOVEL(P)  'FFOR'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SC834D
    C*** BEG ADD ***** 05/22/07 **************************************
     C                   SETON                                        33
    C*** END ADD ***** 05/22/07 **************************************
     C                   ENDIF                                                  RTCD = '*'
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  HFLD = 'SC837A'
     C*** BEG ADD *** 01/06/00 ********************************
     C     HFLD          IFEQ      'SCDR01'
     C     HFLD          OREQ      'SCDR02'
     C*
     C     HFLD          IFEQ      'SCDR01'
     C                   MOVE(P)   SCDR01        QQRMCD
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCDR02'
     C                   MOVE(P)   SCDR02        QQRMCD
     C                   ENDIF
     C*
     C                   CALL      'SC305R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQRMCD            5
     C                   PARM      *BLANK        QQDESC           40
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         IFEQ      '*'
     C*
     C     HFLD          IFEQ      'SCDR01'
     C                   MOVE(P)   QQRMCD        SCDR01
     C                   MOVEL(P)  QQDESC        SCD01D
     C                   MOVE      *ON           *IN31
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCDR02'
     C                   MOVE(P)   QQRMCD        SCDR02
     C                   MOVEL(P)  QQDESC        SCD02D
     C                   ENDIF
     C**
     C                   ELSE                                                   RTC = '*'
     C     HFLD          COMP      'SCDR01'                               29
     C     HFLD          COMP      'SCDR02'                               31
     C                   ENDIF                                                  RTC = '*'
     C                   ENDIF                                                  HFLD
     C*
     C*** END ADD *** 01/06/00 ********************************
     C*
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RECF = 'SCR837'
     C*
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *INKJ = *On
     C*
     C*          F12 = CANCEL / PRIOR SCREEN
     C*          ------------------------------------------------------
     C     *INKL         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RECF = SCR02
     C     Q2RECF        IFEQ      'SCR837'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RECF = SCR837
     C                   ENDIF                                                  *INKL = *On
     C*
     C*          F13 = CHANGE USER PARMS
     C*          ------------------------------------------------------
     C     *INKM         IFEQ      *ON
     C                   CALL      'F0006R'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *INKM = *On
     C*
     C                   LEAVE                                                  VVV Leave
     C                   ENDDO                                                  Do Loop
     C                   ENDSR
     C*
     C**************************************************************************
     C*  Screen 1 processing
     C**************************************************************************
     C     SC01T         BEGSR
     C*
     C*          Jurisdiction may not be blank
     C*          ------------------------------------------------------
     C                   DO
     C     SCJRST        IFEQ      *BLANK
     C                   SETON                                        21
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  JRST = blanks
     C*
     C*          Jurisdiction must be in state table
     C*          ------------------------------------------------------
     C                   MOVEL(P)  'JRST'        XXTBID
     C                   MOVEL(P)  SCJRST        XXKEY
     C     STKY          CHAIN     PST01                              22
     C     *IN22         IFEQ      *ON
     C                   SETON                                        22
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN22 = On
     C*
     C*          Validate Transaction Set ID
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       SCTRDS
     C     SCTSET        IFNE      *BLANK
     C                   MOVEL(P)  'TSET'        XXTBID
     C                   MOVEL(P)  SCTSET        XXKEY
     C     GTKY          CHAIN     PGT01                              24
     C     *IN24         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        24
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN24 = On
     C                   MOVEL(P)  GTDESC        SCTRDS
     C                   ENDIF                                                  TSET <> blanks
     C*
     C                   MOVE      SCJRST        XXJRST
     C                   MOVEL(P)  SCJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCJRSD
     C                   MOVEL(P)  SCTSET        XXTSET
     C                   MOVEL(P)  'TSET'        XXTBID
     C                   MOVEL(P)  SCTSET        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCTRDS
     C     EXKY          CHAIN     PEX01                              40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      UA(2)         SCMODE
     C                   Z-ADD     EXCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCSDT
     C                   Z-ADD     EXMAXT        SCMAXT
     C                   MOVE      EXSEIN        SCSEIN
     C                   MOVE      EXSZP         SCSZP
     C                   MOVE      EXREIN        SCREIN
     C                   MOVE      EXRZP         SCRZP
     C*** BEG ADD ***** 03/26/07 **************************************
     C                   MOVE      EXPDNY        SCPDNY
     C*** END ADD ***** 03/26/07 **************************************
     C                   MOVE      EXLOG         SCLOG
     C                   MOVEL(P)  'CLOG'        XXTBID
     C                   MOVEL(P)  SCLOG         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCLOGD
     C                   ENDIF                                                  *IN60 = Off
     C                   MOVE      EX837         SC837
     C                   MOVEL(P)  'FFOR'        XXTBID
     C                   MOVEL(P)  SC837         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SC837D
     C                   ENDIF                                                  *IN60 = Off
     C                   MOVE      EX837A        SC837A
     C                   MOVEL(P)  'FFOR'        XXTBID
     C                   MOVEL(P)  SC837A        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SC834D
     C                   ENDIF                                                  *IN60 = Off
     C*
     C                   ELSE                                                   *IN40 = Off
     C*
     C                   Z-ADD     0             SCCSDT
     C                   Z-ADD     0             SCMAXT
     C                   MOVE      *BLANKS       SCSEIN
     C                   MOVE      *BLANKS       SCSZP
     C                   MOVE      *BLANKS       SCREIN
     C                   MOVE      *BLANKS       SCRZP
     C*
     C                   ENDIF                                                  *IN40 = Off
     C                   SELECT
     C     SCTSET        WHENEQ    *BLANKS
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C     SCTSET        WHENEQ    '837'
     C                   MOVEL(P)  'SCR837'      Q2RECF
     C                   ENDSL
     C                   LEAVE                                                  VVV Leave
     C                   ENDDO                                                  Do Loop
     C*** BEG ADD *** 01/06/00 ********************************
     C                   MOVE      EXDR01        SCDR01
     C                   MOVE      EXDR02        SCDR02
     C*
     C                   MOVE      EXDR01        XXRMCD            5
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTDESC        SCD01D
     C                   ELSE
     C                   MOVEL     *BLANKS       SCD01D
     C                   END
     C*
     C                   MOVE      EXDR02        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTDESC        SCD02D
     C                   ELSE
     C                   MOVEL     *BLANKS       SCD02D
     C                   END
     C*** END ADD *** 01/06/00 ********************************
     C                   ENDSR
     C*                                                                *********
     C**************************************************************************
     C*  Screen 2 processing
     C**************************************************************************
     C     SC02T         BEGSR
     C*
     C                   DO
     C     *IN09         IFEQ      *ON
     C                   MOVE      *BLANK        SCTSET
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN09 = On
     C*
     C*          If present, start must be valid date.
     C*          ------------------------------------------------------
     C     SCCSDT        IFGT      0
     C                   Z-ADD     SCCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCSDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        20
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  CSDT = 0
     C                   ENDIF                                                  ERR = On
     C*
     C*          The max # of transactions cannot be less than 0
     C*          ------------------------------------------------------
     C     SCMAXT        IFLE      0
     C                   SETON                                        21
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  MAXT <= 0
     C*
     C*          The Sender id number can not be blank
     C*          ------------------------------------------------------
     C     SCSEIN        IFEQ      *BLANKS
     C                   SETON                                        22
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  SEIN = 0
     C*
     C*          The Sender postal code can not be blank
     C*          ------------------------------------------------------
     C     SCSZP         IFEQ      *BLANKS
     C                   SETON                                        23
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  SZP = blanks
     C*
     C*          The Receiver id number can not be blank
     C*          ------------------------------------------------------
     C     SCREIN        IFEQ      *BLANKS
     C                   SETON                                        24
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  REIN = 0
     C*
     C*          The Receiver postal code can not be blank
     C*          ------------------------------------------------------
     C     SCRZP         IFEQ      *BLANKS
     C                   SETON                                        25
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  RZP = blanks
     C*
     C*          ALL EDITS HAVE BEEN PASSED
     C*          ------------------------------------------------------
     C*
     C     EXKY          CHAIN     PEX01                              40
     C*
     C     *IN40         IFEQ      *ON
     C                   CLEAR                   PEX01
     C                   ENDIF                                                  *IN40 = On
     C*
     C                   Z-ADD     XXCO#         EXCO#
     C                   MOVE      SCJRST        EXJRST
     C                   MOVE      SCTSET        EXTSET
     C                   Z-ADD     SCCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXCSDT
     C                   Z-ADD     SCMAXT        EXMAXT
     C*
     C                   MOVE      SCSEIN        EXSEIN
     C                   MOVE      SCSZP         EXSZP
     C                   MOVE      SCREIN        EXREIN
     C                   MOVE      SCRZP         EXRZP
     C*
     C                   MOVE      'A'           EXSTAT
     C                   MOVE      Q1PGM         EXPGM
     C                   MOVE      Q1USER        EXUSER
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        EXTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXDT
     C*
     C     *IN40         IFEQ      *OFF
     C                   UPDATE    PEX01
     C                   ELSE                                                   *IN40 = Off
     C                   MOVE      Q1USER        EXIUSR
     C                   MOVE      Q1PGM         EXIPGM
     C                   MOVE      DSTIME        EXITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXIDT
     C                   WRITE     PEX01
     C                   ENDIF                                                  *IN40 = Off
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   EXSR      CLRSR
     C                   LEAVE                                                  VVV Leave
     C                   ENDDO                                                  Do loop
     C                   ENDSR
     C*                                                                *********
     C**************************************************************************
     C*  Screen 837 processing
     C**************************************************************************
     C     SC837T        BEGSR
     C*
     C*          Validate Trigger for 837 log creation
     C*          ------------------------------------------------------
     C                   DO
     C     SCLOG         IFNE      *BLANK
     C                   MOVEL(P)  'CLOG'        XXTBID
     C                   MOVEL(P)  SCLOG         XXKEY
     C     GTKY          CHAIN     PGT01                              26
     C     *IN26         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        26
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN26 = On
     C                   MOVEL(P)  GTDESC        SCLOGD
     C                   ELSE                                                   LOG <> blank
     C                   SETON                                        26
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  LOG <> blank
     C*
     C*          837 file format
     C*          ------------------------------------------------------
     C     SC837         IFNE      *BLANK
     C                   MOVEL(P)  'FFOR'        XXTBID
     C                   MOVEL(P)  SC837         XXKEY
     C     GTKY          CHAIN     PGT01                              27
     C     *IN27         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        27
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN27 = On
     C                   MOVEL(P)  GTDESC        SC837D
     C                   ELSE                                                   837 <> blank
     C                   SETON                                        27
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  837 <> blank
     C*
     C*          837 file format
     C*          ------------------------------------------------------
     C     SC837A        IFNE      *BLANK
     C                   MOVEL(P)  'FFOR'        XXTBID
     C                   MOVEL(P)  SC837A        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              28
     C     *IN28         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        28
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  *IN28 = On
     C*
     C                   MOVEL(P)  GTDESC        SC834D
     C                   ELSE                                                   837A <> blank
     C*
     C                   SETON                                        28
     C                   LEAVE                                                  VVV Leave
     C                   ENDIF                                                  837A <> blank
     C*
     C*** BEG ADD *** 01/06/00 ********************************
     C*
     C     SCDR01        IFNE      *BLANKS
     C                   MOVE      SCDR01        XXRMCD            5
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTDESC        SCD01D
     C                   ELSE
     C                   MOVEL     *BLANKS       SCD01D
     C                   SETON                                        30
     C                   LEAVE
     C                   END
     C                   END
     C*
     C     SCDR02        IFNE      *BLANKS
     C                   MOVE      SCDR02        XXRMCD            5
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CTDESC        SCD02D
     C                   ELSE
     C                   MOVEL     *BLANKS       SCD02D
     C                   SETON                                        32
     C                   LEAVE
     C                   END
     C                   END
    C*
    C*** BEG ADD ***** 03/26/07 ******************************
    C*
    C*   Validate Process Denial Bills field.
    C*
     C     SCPDNY        IFNE      ' '
     C     SCPDNY        ANDNE     'Y'
     C*
     C                   SETON                                        35
     C                   LEAVE
     C*
     C                   ENDIF                                                  SCPDNY = 'Y' /' '
     C*
     C     SCLOG         IFEQ      '1'
     C     SCPDNY        ANDNE     ' '
     C*
     C                   SETON                                        34
     C                   LEAVE
     C*
     C                   ENDIF                                                  SCLOG = '1'
    C*
    C*** END ADD ***** 03/26/07 ******************************
    C*
     C*
     C*** END ADD *** 01/06/00 ********************************
     C     EXKY          CHAIN     PEX01                              40
     C                   Z-ADD     XXCO#         EXCO#
     C                   MOVE      SCJRST        EXJRST
     C                   MOVE      SCTSET        EXTSET
     C                   MOVE      SCLOG         EXLOG
     C                   MOVE      SC837         EX837
     C                   MOVE      SC837A        EX837A
     C*** BEG ADD *** 01/06/00 ********************************
     C                   MOVE      SCDR01        EXDR01
     C                   MOVE      SCDR02        EXDR02
     C*** END ADD *** 01/06/00 ********************************
    C*** BEG ADD *** 03/26/07 ********************************
     C                   MOVE      SCPDNY        EXPDNY
    C*** END ADD *** 03/26/07 ********************************
     C                   MOVE      'A'           EXSTAT
     C                   MOVE      Q1PGM         EXPGM
     C                   MOVE      Q1USER        EXUSER
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        EXTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXDT
     C*
     C     *IN40         IFEQ      *OFF
     C                   UPDATE    PEX01
     C                   ELSE                                                   *IN40 = Off
     C                   MOVE      Q1USER        EXIUSR
     C                   MOVE      Q1PGM         EXIPGM
     C                   MOVE      DSTIME        EXITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXIDT
     C                   WRITE     PEX01
     C                   ENDIF                                                  *IN40 = Off
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   EXSR      CLRSR
     C                   LEAVE                                                  VVV Leave
     C                   ENDDO                                                  Do Loop
     C                   ENDSR
     C*                                                                *********
     C**************************************************************************
     C*  Clear fields subroutine
     C**************************************************************************
     C     CLRSR         BEGSR
     C                   MOVE      *BLANKS       SCJRST
     C                   MOVE      *BLANKS       SCJRSD
     C                   MOVE      *BLANKS       SCTSET
     C                   MOVE      *BLANKS       SCTRDS
     C                   Z-ADD     0             SCCSDT
     C                   Z-ADD     0             SCMAXT
     C                   MOVE      *BLANKS       SCSEIN
     C                   MOVE      *BLANKS       SCSZP
     C                   MOVE      *BLANKS       SCREIN
     C                   MOVE      *BLANKS       SCRZP
     C                   MOVE      *BLANKS       SCLOG
     C                   MOVE      *BLANKS       SCLOGD
     C                   MOVE      *BLANKS       SC837
     C                   MOVE      *BLANKS       SC837D
     C                   MOVE      *BLANKS       SC837A
     C                   MOVE      *BLANKS       SC834D
     C*** BEG ADD *** 01/06/00 ********************************
     C                   MOVE      *BLANKS       SCDR01
     C                   MOVE      *BLANKS       SCDR02
     C                   MOVEL     *BLANKS       SCD01D
     C                   MOVEL     *BLANKS       SCD02D
     C*** END ADD *** 01/06/00 ********************************
     C                   ENDSR
**  TABLE UA
-- ADD --
- CHANGE -
