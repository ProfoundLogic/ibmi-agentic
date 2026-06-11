     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE SC735R - DARTS DIARY LISTING PROMPT
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM DESCRIPTION:                                       *
     H*        PROMPT FOR DARTS DIARY LISTING PGM.                    *
     H*  NOTE:  RECORD TYPE IS COMMENTED OUT AND THE PGM IS HARDCODED *
     H*         TO PASS A 'C' FOR NOW.   THIS TYPE DETERMINES HOW THE *
     H*         DARTS KEY WILL BE BUILT AS A FUNCTION OF WHICH PGM    *
     H*         GENERATES THE KEY. (C = CASE FOR W0070R & F0070R)     *
     H*    CALLED FROM:                                               *
     H*        MENU                                                   *
     H*    CALLS TO:                                                  *
     H*        SRDCK, SC530R, SC580R, SC735C                          *
     H*    PROGRAMMER: T. Anzalone                                    *
     H*    CREATED:    09/05/91     PROJECT: AIT                      *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  11/08/04 C:136 SUPPORT: DR065 43386                          *
     H*                 Restrict user access if SMSLP/SLRACC = 'Y'    *
     H*                                                               *
     H* 08/06/99 C:61  PROJECT WC577                                  *
     H*                Center program Title.                          *
     H*                                                               *
     H*  04/27/92 KRR   PROJECT: AJP                                  *
     H*                 CHANGE REFERENCE FIELDS FROM CMCODE(2A) TO    *
     H*                 CMRMCD(5A).                                   *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING                           *
     H*                                                               *
     H*       08      Batch Mode                                      *
     H*       09      Demand Mode                                     *
     H*       20      MCR0877 - Invalid user profile                  *
     H*       21      MCR0878 - Invalid remark type code              *
     H*       22      MCR0879 - Invalid diary status, B/C/N are valid *
     H*       23      MCR0500 - Invalid From Date                     *
     H*       24      MCR0879 - Invalid sort, K/N/R are valid         *
     H*       25      MCR0883 - Invalid record type,C/E/D/A/P are     *
     H*                         valid;  note:  not used at this time. *
     H*                         record type C is hard coded for claims*
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*     SMCOL (PCO01): CO                                         *
     H*     FSTCTL(PCT01): -----                                      *
     H*     FMUPL (PUP01): USER                                       *
     H*     SMSLL (PSL01): USER                                       *
     H*****************************************************************
     FSC735F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSMSLL     IF   E           K DISK

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
     C     *LIKE         DEFINE    SLUSER        QQDUSR
     C     *LIKE         DEFINE    SLUSER        XXDUSR
     C     *LIKE         DEFINE    CTRMCD        QQCODE
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*
     C*   KEY LIST FOR THE SECURITY FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    XXDUSR
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
     C     PL735         PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQDUSR
     C                   PARM                    QQCODE
     C                   PARM                    QQSTAT
     C                   PARM                    QQCDAT
     C                   PARM                    QQSORT
     C                   PARM                    QQREC
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C     PL530         PLIST
     C                   PARM                    SCDUSR
     C                   PARM                    QQRTC             1
     C*
     C     PL580         PLIST
     C                   PARM                    SCCODE
     C                   PARM                    QQRTC
     C*
     C     PLDCK         PLIST
     C                   PARM                    SRDTE             6
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*  GET THE USER PARAMETER FILE
     C*
      *** BEG ADD ***** 11/08/04 *************************************
     C     UPKEY         CHAIN     PSL01                              99
     C     SLRACC        IFEQ      'Y'
     C     *IN99         ANDEQ     *OFF
     C                   GOTO      TERM
     C                   ENDIF
     C*** END ADD ***** 11/08/04 **************************************
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*  GET THE COMPANY RECORD
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      ***** BEG ADD ******* 08/06/99 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 08/06/99 **********************************
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
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C********************************************
     C*    F3 = EXIT                             *
     C********************************************
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C********************************************
     C*  F10 = SECURITY FILE SEARCH              *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C*
     C     HFLD          IFEQ      'SCDUSR'
     C*
     C                   CALL      'SC530R'      PL530
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCDUSR        OREQ      *BLANK
     C*
     C                   SETON                                        26
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   SETON                                        27
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  QQRTC EQ 'B'
     C*
     C                   ENDIF                                                  HFLD SCDUSR
     C*
     C********************************************
     C*     F10 - REMARK CODE SEARCH             *
     C********************************************
     C*
     C     HFLD          IFEQ      'SCCODE'
     C*
     C                   CALL      'SC580R'      PL580
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCODE        OREQ      *BLANK
     C*
     C                   SETON                                        27
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   SETON                                        28
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  QQRTC EQ 'B'
     C*
     C                   ENDIF                                                  HFLD SCCODE
     C*
     C                   ENDIF                                                  END *INKJ
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
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
     C                   GOTO      STRT
     C                   END
     C*
     C********************************************
     C*   CMD 13 - PROCESS CHANGE OF USER PARM   *
     C********************************************
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  USER MUST BE ENROLLED IN DARTS
     C     SCDUSR        IFNE      *BLANKS
     C                   MOVE      SCDUSR        XXDUSR
     C     SLKY          CHAIN     PSL01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C*
     C*  REMARKS CODE MUST BE VALID
     C     SCCODE        IFNE      *BLANKS
     C     SCCODE        CHAIN     PCT01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*   DIARY STATUS MUST BE "B/C/N"
     C*
     C     SCSTAT        IFNE      'B'
     C     SCSTAT        ANDNE     'C'
     C     SCSTAT        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   SORT SELECTION MUST BE "K/N/R"
     C*
     C     SCSORT        IFNE      'K'
     C     SCSORT        ANDNE     'N'
     C     SCSORT        ANDNE     'R'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   RECORD TYPE MUST BE "C/E/D/A/P"
     C*
     C                   MOVE      'C'           SCREC
     C*
     C*
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C                   MOVE      *BLANK        QQCO#
     C                   MOVE      *BLANK        QQDUSR
     C                   MOVE      *BLANK        QQCODE
     C                   MOVE      *BLANK        QQSTAT
     C                   MOVE      *BLANK        QQCDAT
     C                   MOVE      *BLANK        QQSORT
     C                   MOVE      *BLANK        QQREC
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCDUSR        QQDUSR
     C                   MOVE      SCCODE        QQCODE
     C                   MOVE      SCSTAT        QQSTAT            1
     C                   MOVE      SCCDAT        QQCDAT            8
     C                   MOVE      SCSORT        QQSORT            1
     C                   MOVE      SCREC         QQREC             1
     C                   CALL      'SC735C'      PL735
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
