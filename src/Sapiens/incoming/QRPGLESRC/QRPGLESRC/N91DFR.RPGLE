     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N91DFR - TYPE DF DISCOUNT CODE MAINTENANCE
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program maintains the 'DF' discount code.          **
     H**   CALLED FROM:                                              **
     H**     N0910R - Prompt for Discount and Billing Type Programs  **
     H**   CALLS TO:                                                 **
     H**     F0006R - Change User Parameters                         **
     H**     M0006C - Help Processor                                 **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED: 05/21/98         PROJECT: 51MCT                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  09/20/00 C:62  Project: NW068
     H*                 -Remove F20=Change status
     H*                 -Remove F23=Delete
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09      Inquiry mode                                   **
     H**      20      Error MCR0503 - Comment can not be blank       **
     H**      40      Add Mode - Code was not found                  **
     H**      60      General Usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      SMCOL  (PCO01): Company Master file (CO#)              **
     H**      SMUPL  (PUP01): User Parameters file (USER)            **
     H**      NMBDLD (PBD01): Billing Rate/Discount Master           **
     H**                      (CODE/EFDT/RCDT = 'D')                 **
     H*****************************************************************
     H*
     FN91DFF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK

     D*
     D* ---------------------------------------------------------------
     D*         TABLES AND ARRAYS
     D* ---------------------------------------------------------------
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D TD              S             70    DIM(1) CTDATA PERRCD(1)
     D*
     D* ---------------------------------------------------------------
     D*         DATA STRUCTURES
     D* ---------------------------------------------------------------
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
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  BILILNG CODE TABLE FILE
     C*
     C     BDKY          KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT            7 0
     C*
     C* ---------------------------------------------------------------
     C*        PARM LISTS
     C* ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCODE            3
     C                   PARM                    QQEFDT            8
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
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C                   MOVE      QQEFDT        XXEFDT            8 0
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         YMEFDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 = On
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   MOVE IN VALUES FROM PARMS TO SCREEN.
     C*
     C                   MOVE      'DF'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C*
     C*   GET RECORD IF IT EXISTS, ELSE ITS AN ADD
     C*
     C     BDKY          CHAIN     PBD01                              40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      BDCODE        SCCODE
     C                   MOVE      BDSTAT        SCSTAT
     C                   MOVE      BDCOM1        SCCOM1
     C                   MOVE      BDCOM2        SCCOM2
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVEL     BDPC01        SCPC01
     C                   MOVEL     BDPC02        SCPC02
     C                   MOVEL     BDPC03        SCPC03
     C                   MOVEL     BDPC04        SCPC04
     C                   MOVE      UA(2)         SCMODE
     C*** BEGIN DEL *** 09/20/00 **************************************
     C********** SCSTAT    IFEQ 'I'
     C**********           MOVE '1'       *IN64                        ING
     C**********           ELSE                            STAT = 'I'
     C**********           MOVE '0'       *IN64                        ING
     C**********           ENDIF                           STAT = 'I'
     C*** END   DEL *** 09/20/00 **************************************
     C*
     C                   ELSE                                                   *IN40 = Off
     C                   MOVE      QQCODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   Z-ADD     YMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      *ZEROS        SCPC01
     C                   MOVE      *ZEROS        SCPC02
     C                   MOVE      *ZEROS        SCPC03
     C                   MOVE      *ZEROS        SCPC04
     C                   MOVE      UA(1)         SCMODE                                     OR
     C*** BEGIN DEL *** 09/20/00 **************************************
     C**********           SETOF                     64
     C*** END   DEL *** 09/20/00 **************************************
     C                   MOVE      'A'           SCSTAT
     C                   ENDIF                                                  *IN40 = Off
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     Q2RECF        SCRECF
     C*
     C                   MOVEL     Q2FILE        SCFILE
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  RECF =  SCR01
     C*
     C*
     C                   MOVE      '0'           *IN30
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   SETON                                        LR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC = On
     C*
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        LR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKL = On
     C*** BEGIN DEL *** 09/20/00 **************************************
     C*
     C*   COMMAND 20 = CHANGE STATUS
     C*
     C********** *INKU     IFEQ *ON
     C********** *IN40     ANDEQ'0'
     C********** SCSTAT    IFEQ 'A'
     C**********           MOVE 'I'       SCSTAT
     C**********           SETON                     64
     C**********           ELSE                            STAT = 'A'
     C**********           MOVE 'A'       SCSTAT
     C**********           SETOF                     64
     C**********           ENDIF                           STAT = 'A'
     C**********           GOTO STRT
     C**********           ENDIF                           *INKU = On
     C*** END   DEL *** 09/20/00 **************************************
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
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
     C                   ENDIF                                                  *IN10 = On
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 = On
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKM = On
     C*** BEGIN DEL *** 09/20/00 **************************************
     C*
     C*   COMMAND 23 = DELETE CODE
     C*
     C********** *INKX     IFEQ *ON
     C********** *IN64     ANDEQ*ON
     C**********           DELETPBD01
     C**********           SETON                     LR
     C**********           GOTO TERM
     C**********           ENDIF                           *INKX = On
     C*** END   DEL *** 09/20/00 **************************************
     C*
     C                   ENDIF                                                  *IN11 = On
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE BILLING CODE CANNOT BLANK
     C*
     C     SCCOM1        IFEQ      *BLANK
     C     SCCOM2        ANDEQ     *BLANK
     C                   SETON                                        20
     C                   GOTO      STRT
     C                   ENDIF                                                  COM1 = Blank
     C*
     C*  ALL EDITS HAVE BEEN PASSED  *** WRITE/UPDATE FILE!
     C*
     C     BDKY          CHAIN     PBD01                              40
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        BDCOM2
     C                   MOVEL     SCPC01        BDPC01
     C                   MOVEL     SCPC02        BDPC02
     C                   MOVEL     SCPC03        BDPC03
     C                   MOVEL     SCPC04        BDPC04
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C*
     C                   MOVE      Q1USER        BDUSER
     C                   MOVE      Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C     *IN40         IFEQ      *OFF
     C                   UPDATE    PBD01
     C                   ELSE                                                   *IN40 = Off
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      YMEFDT        BDEFDT
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C                   ENDIF                                                  *IN40 = Off
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   SETON                                        LR
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE TD
CorVel Network
