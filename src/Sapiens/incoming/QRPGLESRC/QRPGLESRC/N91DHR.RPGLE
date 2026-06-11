     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM: N91DHR                                              *
     H*  DESCRIPTION: DH DISCOUNT CODE - MAINTENANCE PROGRAM          *
     H*****************************************************************
     H*  Created on 03/06/2002 by C:62 for Project 114MDE             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/20/14 C:079 Project: 114MMP Revised                       *
     H*                 Remove original programming and use 9.2 fields*
     H*                                                               *
     H*  05/19/14 C:274 Project: 114MMP                               *
     H*                 Move decimal place one spot over when wrting  *
     H*                 or updating fields.                           *
     H*                                                               *
     H*  02/21/08 C:183 Project: 114MPX                               *
     H*                 Call FA753R to write Tracking Record.         *
     H*
     H*  07/12/05 C:132 Project: MMC045  47783
     H*                 ADD MSG TO SCREEN FOR BLANK COMMENT ERROR
     H*                 COMPILE ONLY
     H*                                                               *
     H*  03/21/05 C:62 PROJECT:114MJD                                 *
     H*                ADD MADIGAN PROCESSING                         *
     H*                                                               *
     H*  05/14/04 C:62 PROJECT:114MIC                                 *
     H*                ADD PROCESSING FOR INHOUSE %                   *
     H*                                                               *
     H*  04/14/03 C:62 PROJECT:114MFG                                 *
     H*                ADD PROCESSING FOR % OFF FEE SCHEDULE          *
     H*                                                               *
     H*****************************************************************
     FN91DHF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDLD    UF A E           K DISK
     F*** BEGIN ADD *** 03/21/05 **************************************
     FNMBDL1    IF   E           K DISK
     F                                     RENAME(PBD01:PBD02)
     FNMBDP     IF   E             DISK
     F                                     RENAME(PBD01:PBD03)
     F** END   ADD *** 03/21/05 **************************************

     D*** Begin Add *** 05/19/14 **************************************
     D*** Begin Chg *** 10/20/14 **************************************
     D*** HDPC01          s              4  3
     D*** HDPC02          s              4  3
     D*** HDPC03          s              4  3
     D HDPC01          s              5  2
     D HDPC02          s              5  2
     D HDPC03          s              5  2
     D*** End   Chg *** 10/20/14 **************************************
     D*** End Add   *** 05/19/14 **************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D TD              S             70    DIM(1) CTDATA PERRCD(1)
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
     C     BDKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C*** BEGIN ADD *** 03/21/05 **************************************
     C*
     C     BDKY2A        KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKEFDT
     C*
     C     BDKY2B        KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKTYPE
     C*** END   ADD *** 03/21/05 **************************************
     C*
     C     *LIKE         DEFINE    UPDT          YMEFDT
     C*** BEGIN ADD *** 03/21/05 **************************************
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDTYPE        KKTYPE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C*** END   ADD *** 03/21/05 **************************************
     C*
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
     C                   MOVE      QQEFDT        XXEFDT            8 0
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         YMEFDT
     C*
     C*** BEG ADD *** 02/21/08 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD *** 02/21/08 **************************************
      *
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              99
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
     C*** MOVE IN VALUES FROM PARMS TO SCREEN.
     C*
     C                   MOVE      'DH'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C*
     C*** GET RECORD IF IT EXISTS, ELSE ITS AN ADD
     C*
     C     BDKY1         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      BDCODE        SCCODE
     C                   MOVE      BDSTAT        SCSTAT
     C                   MOVE      BDCOM1        SCCOM1
     C                   MOVE      BDCOM2        SCCOM2
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     BDAM01        SCAM01
     C*** Begin Chg *** 05/19/14 **************************************
     C****               MOVE      BDPC01        SCPC01
     C*** Begin Chg *** 10/20/14 **************************************
     C***************    EVAL      SCPC01 = BDPC01*10
     C                   EVAL      SCPC01 = BDAM12
     C*** End   Chg *** 10/20/14 **************************************
     C*** BEGIN ADD *** 04/14/03 **************************************
     C****               MOVE      BDPC02        SCPC02
     C*** Begin Chg *** 10/20/14 **************************************
     C****************   EVAL      SCPC02 = BDPC02*10
     C                   EVAL      SCPC02 = BDAM13
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   ADD *** 04/14/03 **************************************
     C*** BEGIN ADD *** 05/14/04 **************************************
     C****               MOVE      BDPC03        SCPC03
     C*** Begin Chg *** 10/20/14 **************************************
     C***************    EVAL      SCPC03 = BDPC03*10
     C                   EVAL      SCPC03 = BDAM14
     C*** End   Chg *** 10/20/14 **************************************
     C*** End   Chg *** 05/19/14 **************************************
     C*** END   ADD *** 05/14/04 **************************************
     C*** BEGIN ADD *** 03/21/05 **************************************
     C                   MOVEL     BDMADG        SCMADG
     C*
     C                   MOVEL     BDMADG        KKCODE
     C                   MOVEL     'DK'          KKTYPE
     C                   Z-ADD     YMEFDT        KKEFDT
     C     BDKY2A        SETGT     PBD02
     C     BDKY2B        READPE    PBD02                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     BDCOM1        SCMADD
     C                   ELSE
     C                   CLEAR                   SCMADD
     C                   ENDIF
     C*** END   ADD *** 03/21/05 **************************************
     C                   SETOFF                                       41
     C                   MOVE      UA(2)         SCMODE                         RECORD
     C                   ELSE
     C                   MOVE      QQCODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C*
     C                   Z-ADD     YMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      *ZEROS        SCAM01
     C                   MOVE      *ZEROS        SCPC01
     C*** BEGIN ADD *** 04/14/03 **************************************
     C                   MOVE      *ZEROS        SCPC02
     C*** END   ADD *** 04/14/03 **************************************
     C*** BEGIN ADD *** 05/14/04 **************************************
     C                   MOVE      *ZEROS        SCPC03
     C*** END   ADD *** 05/14/04 **************************************
     C*** BEGIN ADD *** 03/21/05 **************************************
     C                   MOVE      *BLANKS       SCMADG
     C                   MOVE      *BLANKS       SCMADD
     C*** END   ADD *** 03/21/05 **************************************
     C*
     C                   SETON                                        41
     C                   MOVE      UA(1)         SCMODE                         BRINGING PRIOR
     C                   MOVE      'A'           SCSTAT                         RCD'S DATA
     C                   ENDIF
     C*
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCCOM1        HDCOM1           60
     C                   MOVE      SCCOM2        HDCOM2           60
     C                   MOVE      SCAM01        HDAM01            9 2
     C*** Beg Chg *** 05/19/14 **************************************
     C***                MOVE      SCPC01        HDPC01            4 3
     C*** Begin Chg *** 10/20/14 **************************************
     C****************   Eval      HDPC01 = SCPC01/10
     C                   Eval      HDPC01 = SCPC01
     C*** End   Chg *** 10/20/14 **************************************
     C***                MOVE      SCPC02        HDPC02            4 3
     C*** Begin Chg *** 10/20/14 **************************************
     C****************   Eval      HDPC02 = SCPC02/10
     C                   Eval      HDPC02 = SCPC02
     C*** End   Chg *** 10/20/14 **************************************
     C***                MOVE      SCPC03        HDPC03            4 3
     C*** Begin Chg *** 10/20/14 **************************************
     C****************** Eval      HDPC03 = SCPC03/10
     C                   Eval      HDPC03 = SCPC03
     C*** End   Chg *** 10/20/14 **************************************
     C*** End Chg *** 05/19/14 **************************************
     C                   MOVE      SCMADG        HDMADG            3
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   MOVEL     Q2RECF        SCRECF
     C                   MOVEL     Q2FILE        SCFILE
     C*
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*
     C                   SETOFF                                       30
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 03 = PRIOR SCREEN
     C*
     C     *INKC         IFEQ      *ON
     C                   SETON                                        LR
     C                   GOTO      TERM
     C                   ENDIF
     C*** BEGIN ADD *** 03/21/05 **************************************
     C*
     C*   COMMAND 10 = SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   CALL      'FA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'N2330R'      ##FNM            10
     C                   PARM      'N2330Q'      ##TNM            10
     C*
     C                   CALL      'N2330Q'
     C                   PARM      '2'           ##CALL            1
     C     *INKL         PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'DK'          ##TYPE            2
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C     ##RRN         CHAIN     PBD03                              99
     C                   MOVEL     BDCODE        SCMADG
     C                   MOVEL     BDCOM1        SCMADD
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*** END   ADD *** 03/21/05 **************************************
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        LR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*   PROCESS HELP REQUESTS
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
     C                   ENDIF
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
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
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 05/14/04 **************************************
     C*
     C*  BOTH PERDIEM AND INHOUSE % NOT ALLOWED
     C*
     C     SCAM01        IFNE      *ZEROS
     C     SCPC03        ANDNE     *ZEROS
     C                   SETON                                        26
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 05/14/04 **************************************
     C*** BEGIN ADD *** 03/21/05 **************************************
     C*
     C* CODE MUST BE VALID AND EFFECTIVE
     C*
     C     SCMADG        IFNE      *BLANKS
     C                   MOVEL(P)  SCMADG        KKCODE
     C                   MOVEL(P)  'DK'          KKTYPE
     C                   Z-ADD     YMEFDT        KKEFDT
     C*
     C     BDKY2A        SETGT     PBD02
     C     BDKY2B        READPE    PBD02                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        27
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/21/05 **************************************
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED  *** WRITE/UPDATE FILE!
     C*
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      YMEFDT        BDEFDT
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        BDCOM2
     C                   Z-ADD     SCAM01        BDAM01
     C*** Begin Chg *** 05/19/14 **************************************
     C*****              MOVE      SCPC01        BDPC01
     C*** Begin Chg *** 10/20/14 **************************************
     C****************   EVAL      BDPC01 = SCPC01/10
     C                   EVAL      BDAM12 = SCPC01
     C*** End   Chg *** 10/20/14 **************************************
     C*** BEGIN ADD *** 04/14/03 **************************************
     C*****              MOVE      SCPC02        BDPC02
     C*** Begin Chg *** 10/20/14 **************************************
     C*****************  EVAL      BDPC02 = SCPC02/10
     C                   EVAL      BDAM13 = SCPC02
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   ADD *** 04/14/03 **************************************
     C*** BEGIN ADD *** 05/14/04 **************************************
     C*****              MOVE      SCPC03        BDPC03
     C*** Begin Chg *** 10/20/14 **************************************
     C*****************  EVAL      BDPC03 = SCPC03/10
     C                   EVAL      BDAM14 = SCPC03
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   ADD *** 05/14/04 **************************************
     C*** End   Chg *** 05/19/14 **************************************
     C*** BEGIN ADD *** 03/21/05 **************************************
     C                   MOVE      SCMADG        BDMADG
     C*** END   ADD *** 03/21/05 **************************************
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C*
     C                   MOVE      Q1USER        BDUSER
     C                   MOVE      Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C     *IN41         IFEQ      *OFF
     C                   UPDATE    PBD01
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'U'           @UWFLG            1
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ELSE
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C*** BEG ADD *** 02/21/08 **************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      'W'           @UWFLG
     C                   EXSR      WRTTR
     C                   ENDIF                                                  CLTID IFEQ 114
     C*** END ADD *** 02/21/08 **************************************
     C                   ENDIF
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   SETON                                        LR
     C*** BEG ADD *** 02/21/08 **************************************
     C*================================================================
     C* WRTTR - Write Tracking Record
     C*================================================================
     C     WRTTR         BEGSR
      *
     C                   SELECT
      *
     C     @UWFLG        WHENEQ    'U'
      *
     C     HDCOM1        IFNE      BDCOM1
      *
     C                   MOVEL(P)  'BDCOM1'      @@FLD
     C                   MOVEL(P)  HDCOM1        @@FVAL
     C                   MOVEL(P)  BDCOM1        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE            3
     C                   PARM      BDTYPE        @@TYPE            2
     C                   PARM      BDRCDT        @@RCDT            1
     C                   PARM      BDEFDT        @@EFDT            7 0
     C                   PARM      *BLANKS       @@CPT4            5
     C                   PARM      *BLANKS       @@PMOD            2
     C                   PARM      *BLANKS       @@10A1           10
     C                   PARM      *BLANKS       @@10A2           10
     C                   PARM      *BLANKS       @@10A3           10
     C                   PARM      *BLANKS       @@10A4           10
     C                   PARM                    @@FLD            10
     C                   PARM                    @@FVAL           80
     C                   PARM                    @@TVAL           80
     C                   ENDIF                                                  HDCOM1 IFNE BDCOM1
      *
     C     HDCOM2        IFNE      BDCOM2
      *
     C                   MOVEL(P)  'BDCOM2'      @@FLD
     C                   MOVEL(P)  HDCOM2        @@FVAL
     C                   MOVEL(P)  BDCOM2        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDCOM2    IFNE
      *
     C     HDAM01        IFNE      BDAM01
      *
     C                   MOVEL(P)  'BDAM01'      @@FLD
     C                   MOVEL(P)  HDAM01        @@FVAL
     C                   MOVEL(P)  BDAM01        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDAM01    IFNE
      *
     C     HDPC01        IFNE      BDPC01
      *
     C                   MOVEL(P)  'BDPC01'      @@FLD
     C                   MOVEL(P)  HDPC01        @@FVAL
     C                   MOVEL(P)  BDPC01        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDPC01    IFNE
      *
     C     HDPC02        IFNE      BDPC02
      *
     C                   MOVEL(P)  'BDPC02'      @@FLD
     C                   MOVEL(P)  HDPC02        @@FVAL
     C                   MOVEL(P)  BDPC02        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDPC02    IFNE
      *
     C     HDPC03        IFNE      BDPC03
      *
     C                   MOVEL(P)  'BDPC03'      @@FLD
     C                   MOVEL(P)  HDPC03        @@FVAL
     C                   MOVEL(P)  BDPC03        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDPC03    IFNE
      *
     C     HDMADG        IFNE      BDMADG
      *
     C                   MOVEL(P)  'BDMADG'      @@FLD
     C                   MOVEL(P)  HDMADG        @@FVAL
     C                   MOVEL(P)  BDMADG        @@TVAL
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
     C                   ENDIF                                                  HDMADG    IFNE
      *
     C     @UWFLG        WHENEQ    'W'
      *
     C                   MOVEL(P)  'Added'       @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM      BDCODE        @@CODE
     C                   PARM      BDTYPE        @@TYPE
     C                   PARM      BDRCDT        @@RCDT
     C                   PARM      BDEFDT        @@EFDT
     C                   PARM      *BLANKS       @@CPT4
     C                   PARM      *BLANKS       @@PMOD
     C                   PARM      *BLANKS       @@10A1
     C                   PARM      *BLANKS       @@10A2
     C                   PARM      *BLANKS       @@10A3
     C                   PARM      *BLANKS       @@10A4
     C                   PARM                    @@FLD
     C                   PARM                    @@FVAL
     C                   PARM                    @@TVAL
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  WRTTR
     C*** END ADD *** 02/21/08 **************************************
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE TD
NETWORK DISCOUNT: PRUDENT BUY PERCENTAGE.
