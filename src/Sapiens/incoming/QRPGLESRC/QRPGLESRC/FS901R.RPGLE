     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* TITLE FS901R - MEDICODE TAPE UPLOAD PROMPT
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FS901R                                            *
     H*    CREATED: 02/27/01   PROJ: FS225    C:117                   *
     H*                        CREATED ANES MODULE                    *
     H*                        MOVED MEDICODE PGMS TO BASE            *
     H*    DESCRIPTION: MEDICODE TAPE UPLOAD PROMPT PROGRAM           *
     H*                                                               *
     H*    1. VALIDATE THE TAPE DEVICE                                *
     H*    2. VALIDATE THE FILE NAME                                  *
     H*    3. VALIDATE THE PERCENTILE TO CONVERT                      *
     H*    4. SUBMIT JOB TO PROCESS THE TAPE UPLOAD                   *
     H*                                                               *
     H*    CALLED FROM: MENU                                          *
     H*    CALLS TO: FS901C                                           *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/28/2001 C:62 PROJECT FS226                                *
     H*                  PROCESS CD/TAPE FLAG                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H* 20 - INVALID TAPE DEVICE                                      *
     H* 21 - INVALID FILE NAME ENTERED                                *
     H* 22 - INVALID PERCENTILE ENTERED                               *
     H* 23 - POSITION CURSOR @ FILE NAME, NO SELECTION ON FILE SEARCH *
     H* 24 - POSITION CURSOR @ PERCENTILE, SELECTION ON FILE SEARCH   *
     H* 25 - INVALID UPLOAD TYPE                                      *
     H*                                                    `          *
     H* 60 - GENERAL CHAINING                                         *
     H* 70 - LOOK UP                                                  *
     H*****************************************************************
     FFS901F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     F*** BEGIN ADD *** 11/28/01 **************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END   ADD *** 11/28/01 **************************************

     D*
     D TABPCT          S              2    DIM(8) CTDATA PERRCD(8)              PERCENTILE VALUES
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C* GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXTBID            4
     C                   KFLD                    XXKEY            10
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
     C* GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C* GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C*
     C     COKY          CHAIN     PCO01                              61
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C**************************************************************************
     C*  F3 = EXIT
     C**************************************************************************
     C*
     C* CALL FOR END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C**************************************************************************
     C*  F10 = GENERIC TABLE SEARCH
     C**************************************************************************
     C*
     C* CALL GENERIC TABLE SEARCH(F2380R)
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFNAM'
     C*** BEGIN DEL *** 11/28/01 **************************************
     C**********           MOVE *BLANKS   SCFNAM
     C**********           MOVE *BLANKS   SCFDSC
     C*** END   DEL *** 11/28/01 **************************************
     C     *INKJ         IFEQ      *ON
     C*** BEGIN CHG *** 11/28/01 **************************************
     C**********           CALL 'F2493R'
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C**********           PARM 'MDR'     QQTID  10
     C**********           PARM           QQTKEY 10
     C**********           PARM           QQRTCD  1
     C**********           MOVELQQTKEY    SCFNAM    P
     C*
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     C*
     C********** QQRTCD    IFEQ 'B'
     C********** QQTKEY    OREQ *BLANKS
     C**********           MOVE *BLANKS   SCFDSC
     C**********           MOVE *ON       *IN23
     C**********           ENDIF
     C*
     C********** QQRTCD    IFEQ '*'
     C**********           MOVE *ON       *IN24
     C**********           MOVELSCFNAM    XXKEY     P
     C**********           MOVEL'MDR'     XXTBID    P
     C********** GTKY      CHAINPGT01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELGTDESC    SCFDSC    P
     C**********           END
     C**********           END                             ENDEQ
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'MDR'         ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        2390
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL(P)  GTKEY         SCFNAM
     C                   MOVEL(P)  GTDESC        SCFDSC
     C                   SETON                                        2490
     C*
     C                   ENDIF
     C*** END   CHG *** 11/28/01 **************************************
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCTDEV'
     C                   SETON                                        7590
     C     HFLD          WHENEQ    'SCPCT1'
     C                   SETON                                        7690
     C     HFLD          WHENEQ    'SCTYPE'
     C                   SETON                                        7790
     C                   ENDSL
     C*
     C**************************************************************************
     C*  F13 = CHANGE USER PARMS
     C**************************************************************************
     C*
     C* PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C* GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   GOTO      STRT01
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*** BEGIN ADD *** 11/28/01 **************************************
     C*
     C* TYPE OF UPLOADED IS REQUIRED
     C*
     C     SCTYPE        IFNE      'T'
     C     SCTYPE        ANDNE     'C'
     C*
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*** END   ADD *** 11/28/01 **************************************
     C*
     C* TAPE DEVICE REQUIRED
     C*  VALIDATE THE TAPE DEVICE
     C*
     C*** BEGIN ADD *** 11/28/01 **************************************
     C     SCTYPE        IFEQ      'T'
     C*** END   ADD *** 11/28/01 **************************************
     C                   MOVE      *OFF          *IN20
     C     SCTDEV        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT01
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       RETCDE            1
     C                   CALL      'FS901C1'
     C                   PARM                    SCTDEV
     C                   PARM                    RETCDE
     C*
     C     RETCDE        IFEQ      '1'
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEGIN ADD *** 11/28/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/28/01 **************************************
     C*
     C* VALIDATE THE FILE NAME ENTERD
     C*
     C                   MOVE      *OFF          *IN22
     C                   MOVEL(P)  SCFNAM        XXKEY
     C                   MOVEL(P)  'MDR'         XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   MOVE      *BLANKS       SCFDSC
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCFDSC
     C                   ENDIF
     C*
     C* VALIDATE THE PERCENT ENTERED IS ALLOWED
     C*
     C                   MOVE      *OFF          *IN22
     C     SCPCT1        LOOKUP    TABPCT                                 70
     C     *IN70         IFEQ      *OFF
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* ALL EDITS WERE PASSED LOAD THE PARM FIELDS
     C* CALL THE CLP TO SUBMIT THE UPDATE
     C*
     C                   MOVE      UPCO#         X@CO#             3
     C                   CALL      'FS901C'
     C                   PARM                    X@CO#
     C                   PARM                    SCTDEV
     C                   PARM      GTFG01        @@SEL             1
     C                   PARM                    SCPCT1
     C                   PARM      GTCF01        @@LABL           10
     C                   PARM      GTCF02        @@PGM            10
     C*** BEGIN ADD *** 11/28/01 **************************************
     C                   PARM      SCTYPE        ##TYPE            1
     C*** END   ADD *** 11/28/01 **************************************
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**   TABLE OF PERCENTILES
5060707580859095
