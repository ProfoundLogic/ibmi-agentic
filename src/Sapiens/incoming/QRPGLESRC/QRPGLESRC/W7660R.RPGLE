     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7660R - PROMPT FOR CALC. VS. BILLED PREMIUM REPORT
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL PRINT A LIST OF ALL          *
     H*                EMPLOYERS WHOSE CALCULATED PREMIUM IS          *
     H*                DIFFERENT THAN BILLED PREMIUM BASED ON THE     *
     H*                ENTERED POLICY EFFECTIVE DATE RANGE.           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W7660C                                                     *
     H*                                                               *
     H*  CREATED: 02/28/01  PROGRAMMER: C:90  PROJECT: WC1000         *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/07/04 C:132 SUPPORT: WA099 42798                           *
     H*                Display Location of Output File.               *
     H*                                                               *
     H* 01/02/03 C:136 SUPPORT: WA099 35328                           *
     H*                Title is blank if QQPRDS is blank.             *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     FW7660F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK

     D SCAR            S             10    DIM(7) CTDATA PERRCD(1) ASCEND       A/R SCRN FLDS
     D*
      *** BEG ADD **** 01/02/03 *********************************************
     D TITLE           C                   CONST('  CALCULATED VS. BIL-
     D                                     LED PREMIUM REPORT  ')
      *** END ADD **** 01/02/03 *********************************************
     D*
      ***** BEG ADD ***** 06/07/04 **************************************
     D OPTSEL          C                   CONST(', if selected.')
      ***** END ADD ***** 06/07/04 **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  AR                     1     14
     D                                     DIM(7) ASCEND                        A/R CODES
     D  SCAR1                  1      2
     D  SCAR2                  3      4
     D  SCAR3                  5      6
     D  SCAR4                  7      8
     D  SCAR5                  9     10
     D  SCAR6                 11     12
     D  SCAR7                 13     14
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
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    UPCO#
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60
     C*
     C                   MOVE      UPFND         SCFND
     C*
     C                   MOVE      'Y'           SCSRAC
     C*
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKY          CHAIN     PFN01                              21
     C     *IN21         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCNAME
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   ENDIF                                                  *IN21=OFF
     C*
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   ENDIF                                                  XXFND <> 0
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
      *** BEG ADD **** 01/02/03 *********************************************
     C     QQPRDS        IFNE      *BLANKS
      *** END ADD **** 01/02/03 *********************************************
     C                   MOVE      QQPRDS        SCPRDS
      *** BEG ADD **** 01/02/03 *********************************************
     C                   ELSE
     C                   MOVE      TITLE         SCPRDS
     C                   ENDIF
      *** END ADD **** 01/02/03 *********************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      ***** BEG ADD ***** 06/07/04 **************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
      ***** END ADD ***** 06/07/04 **************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10
     C*
     C*  TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C     HFLD          LOOKUP    SCAR(X)                                99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C                   MOVEL     SCKEY         AR(X)
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANK
     C     X             ADD       26            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     X             ADD       27            X1                2 0
     C*
     C     X1            IFNE      34
     C*
     C                   MOVE      '1'           *IN(X1)
     C*
     C                   ENDIF                                                  X1 <>
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ
     C*
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      UPFND         SCFND
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKY          CHAIN     PFN01                              21
     C     *IN21         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCNAME
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   ENDIF                                                  *IN21=OFF
     C*
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   ENDIF                                                  XXFND
     C*
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM
     C*
     C                   ENDIF                                                  *IN11
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      *
      *  VALIDATE EFFECTIVE DATES
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  EFFECTIVE DATES MUST BE IN ASCENDING ORDER
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C* S/R ACCOUNT FLAG MUST BE Y OR N
     C*
     C     SCSRAC        IFNE      'Y'
     C     SCSRAC        ANDNE     'N'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* SORT SELECTION MUST BE A OR E
     C*
     C     SCSORT        IFNE      'A'
     C     SCSORT        ANDNE     'E'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
      *************************************************************
     C*
     C*  VALIDATE THE A/R CODES
     C*
     C                   Z-ADD     1             X
     C     X             DOUEQ     7
     C*
     C     AR(X)         IFNE      *BLANKS
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     AR(X)         XXKEY
     C     TBKY          CHAIN     PTT01                              40
     C     *IN40         IFEQ      *ON
     C     X             ADD       33            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
      *************************************************************
      *
      *  PRINT FLAG MUST BE P, F, OR B.
      *
     C     SCPRTF        IFNE      'P'
     C     SCPRTF        ANDNE     'F'
     C     SCPRTF        ANDNE     'B'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     XXFND         IFNE      *ZERO
     C     FNKY          CHAIN     PFN01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCSRAC        QQSRAC            1
     C                   MOVEA     AR            QQARCD           14
     C                   MOVE      SCPRTF        QQPRTF            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      SCSORT        QQSORT            1
     C*
     C                   CALL      'W7660C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQSRAC
     C                   PARM                    QQARCD
     C                   PARM                    QQPRTF
     C                   PARM                    QQOUTQ
     C                   PARM                    QQSORT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
** SCAR
SCAR1
SCAR2
SCAR3
SCAR4
SCAR5
SCAR6
SCAR7
