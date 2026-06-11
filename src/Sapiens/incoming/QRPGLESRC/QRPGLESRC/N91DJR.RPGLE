     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM: N91DJR                                              *
     H*  DESCRIPTION: DJ DISCOUNT CODE - MAINTENANCE PROGRAM          *
     H*****************************************************************
     H*  Created on 07/10/2002 by C:62 for Project NW074              *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                              **
     H*  10/20/03 C:62 PROJECT NW077                                 **
     H*                ADD SKIP DENIED LINE LOGIC.                   **
     H*                                                              **
     H*  10/24/02 C:62 PROJECT NW074                                 **
     H*                ADD LEAVE MRA FLAG LOGIC.                     **
     H*                                                               *
     H*****************************************************************
     FN91DJF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFTBSP     IF   E             DISK
     FFTBSL     IF   E           K DISK
     F                                     RENAME(PBS01:PBS02)
     FNMBDLD    UF A E           K DISK

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
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    YMEFDT
     C*
     C*  BILL STATUS CODE TABLE FILE
     C*
     C     BSKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    UPDT          YMEFDT
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
     C                   MOVE      'DJ'          SCTYPE
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
     C                   MOVE      BDPC01        SCPC01
     C                   MOVEL(P)  BDMRAF        SCMRAF
     C                   MOVEL(P)  BDOT1F        SCOT1F
     C                   MOVEL(P)  BDOT2F        SCOT2F
     C                   MOVEL(P)  BDNETF        SCNETF
     C                   MOVEL(P)  BDOT1C        SCOT1C
     C*** BEGIN ADD *** 10/24/02 **************************************
     C                   MOVEL(P)  BDLMRA        SCLMRA
     C*** END   ADD *** 10/24/02 **************************************
     C*** BEGIN ADD *** 10/20/03 **************************************
     C                   MOVEL(P)  BDSKPD        SCSKPD
     C*** END   ADD *** 10/20/03 **************************************
     C*
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  SCOT1C        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     BSDESC        SCOT1D
     C                   ELSE
     C                   MOVEL     *BLANKS       SCOT1D
     C                   ENDIF
     C*
     C                   MOVEL(P)  BDOT2C        SCOT2C
     C*
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  SCOT2C        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     BSDESC        SCOT2D
     C                   ELSE
     C                   MOVEL     *BLANKS       SCOT2D
     C                   ENDIF
     C*
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
     C                   MOVE      *ZEROS        SCPC01
     C                   MOVEL(P)  *BLANKS       SCMRAF
     C                   MOVEL(P)  *BLANKS       SCOT1F
     C                   MOVEL(P)  *BLANKS       SCOT2F
     C                   MOVEL(P)  *BLANKS       SCNETF
     C                   MOVEL(P)  *BLANKS       SCOT1C
     C                   MOVEL(P)  *BLANKS       SCOT1D
     C                   MOVEL(P)  *BLANKS       SCOT2C
     C                   MOVEL(P)  *BLANKS       SCOT2D
     C*** BEGIN ADD *** 10/24/02 **************************************
     C                   MOVEL(P)  *BLANKS       SCLMRA
     C*** END   ADD *** 10/24/02 **************************************
     C*
     C                   SETON                                        41
     C                   MOVE      UA(1)         SCMODE                         BRINGING PRIOR
     C                   MOVE      'A'           SCSTAT                         RCD'S DATA
     C                   ENDIF
     C*
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
     C*
     C*   COMMAND 10 = SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCOT1C'
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'O'           ##TYPE            1
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE                                                   ##KC = 1
     C     ##RRN         CHAIN     PBS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSCODE        SCOT1C
     C                   MOVEL(P)  BSDESC        SCOT1D
     C                   SETON                                        32
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRB NE *ZEROS
     C     HFLD          WHENEQ    'SCOT2C'
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'O'           ##TYPE
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        33
     C                   ELSE                                                   ##KC = 1
     C     ##RRN         CHAIN     PBS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSCODE        SCOT2C
     C                   MOVEL(P)  BSDESC        SCOT2D
     C                   SETON                                        34
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRB NE *ZEROS
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C                   ENDIF
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
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE TO MRA FLAG MUST BE Y OR N
     C*
     C     SCMRAF        IFNE      'Y'
     C     SCMRAF        ANDNE     'N'
     C                   SETON                                        72
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE TO OTHER 1 FLAG MUST BE Y OR N
     C*
     C     SCOT1F        IFNE      'Y'
     C     SCOT1F        ANDNE     'N'
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE TO OTHER 2 FLAG MUST BE Y OR N
     C*
     C     SCOT2F        IFNE      'Y'
     C     SCOT2F        ANDNE     'N'
     C                   SETON                                        74
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE TO NETWORK FLAG MUST BE Y OR N
     C*
     C     SCNETF        IFNE      'Y'
     C     SCNETF        ANDNE     'N'
     C                   SETON                                        75
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ONE FLAG NEEDS TO BE 'Y'
     C*
     C     SCMRAF        IFNE      'Y'
     C     SCOT1F        ANDNE     'Y'
     C     SCOT2F        ANDNE     'Y'
     C     SCNETF        ANDNE     'Y'
     C                   SETON                                        76
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ONLY ONE FLAG CAN BE 'Y'
     C*
     C     SCMRAF        IFEQ      'Y'
     C     SCOT1F        ANDEQ     'Y'
     C     SCMRAF        OREQ      'Y'
     C     SCOT2F        ANDEQ     'Y'
     C     SCMRAF        OREQ      'Y'
     C     SCNETF        ANDEQ     'Y'
     C     SCOT1F        OREQ      'Y'
     C     SCOT2F        ANDEQ     'Y'
     C     SCOT1F        OREQ      'Y'
     C     SCNETF        ANDEQ     'Y'
     C     SCOT2F        OREQ      'Y'
     C     SCNETF        ANDEQ     'Y'
     C                   SETON                                        77
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* IF THE TO OTHER 1 FLAG IS NO THEN A STATUS CODE
     C* CAN'T BE ENTERED
     C*
     C     SCOT1F        IFEQ      'N'
     C     SCOT1C        ANDNE     *BLANKS
     C                   SETON                                        78
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* IF THE TO OTHER 1 FLAG IS YES THEN A STATUS CODE
     C* MUST BE ENTERED
     C*
     C     SCOT1F        IFEQ      'Y'
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  SCOT1C        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        79
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF THE TO OTHER 2 FLAG IS NO THEN A STATUS CODE
     C* CAN'T BE ENTERED
     C*
     C     SCOT2F        IFEQ      'N'
     C     SCOT2C        ANDNE     *BLANKS
     C                   SETON                                        80
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* IF THE TO OTHER 2 FLAG IS YES THEN A STATUS CODE
     C* MUST BE ENTERED
     C*
     C     SCOT2F        IFEQ      'Y'
     C                   MOVEL(P)  'O'           KKTYPE
     C                   MOVEL(P)  SCOT2C        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        81
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 10/24/02 **************************************
     C*
     C*  THE LEAVE MRA FLAG MUST BE Y OR N
     C*
     C     SCLMRA        IFNE      'Y'
     C     SCLMRA        ANDNE     'N'
     C                   SETON                                        82
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 10/24/02 **************************************
     C*** BEGIN ADD *** 10/20/03 **************************************
     C*
     C*  THE SKIP DENIED LINE FLAG MUST BE Y OR BLANK
     C*
     C     SCSKPD        IFNE      'Y'
     C     SCSKPD        ANDNE     ' '
     C                   SETON                                        83
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 10/20/03 **************************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED  *** WRITE/UPDATE FILE!
     C*
     C                   MOVEL     SCCODE        BDCODE
     C                   MOVE      YMEFDT        BDEFDT
     C                   MOVE      SCSTAT        BDSTAT
     C                   MOVE      SCCOM1        BDCOM1
     C                   MOVE      SCCOM2        BDCOM2
     C                   MOVE      SCPC01        BDPC01
     C                   MOVE      SCTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C                   MOVEL(P)  SCMRAF        BDMRAF
     C                   MOVEL(P)  SCOT1F        BDOT1F
     C                   MOVEL(P)  SCOT2F        BDOT2F
     C                   MOVEL(P)  SCNETF        BDNETF
     C                   MOVEL(P)  SCOT1C        BDOT1C
     C                   MOVEL(P)  SCOT2C        BDOT2C
     C*** BEGIN ADD *** 10/24/02 **************************************
     C                   MOVEL(P)  SCLMRA        BDLMRA
     C*** END   ADD *** 10/24/02 **************************************
     C*** BEGIN ADD *** 10/20/03 **************************************
     C                   MOVEL(P)  SCSKPD        BDSKPD
     C*** END   ADD *** 10/20/03 **************************************
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
     C                   ELSE
     C                   MOVE      Q1USER        BDIUSR
     C                   WRITE     PBD01
     C                   ENDIF
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   SETON                                        LR
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE TD
NETWORK DISCOUNT: % OFF BILLED CHARGES.
