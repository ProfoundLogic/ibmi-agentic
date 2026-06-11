     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7170R5- Prompt for Claims Listing / Selection Criteria
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: MENU                                              *
     H*                                                               *
     H*  CALLS TO:  W7170C5 - SUBMIT                                  *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  12/03/98 C:61 PROJECT WC577                                  *
     H*                Copied and converted from V2.                  *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/03/03 C:53 Support: WC065 # 36122                         *
     H*                Add group restriction to pgm.                  *
     H*                                                               *
     H*  09/08/99 C:61 PROJECT WC577                                  *
     H*                Give warning when submitting for all groups.   *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
      *
     FW7170F5   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMCMP     IF   E             DISK
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DATA                   1    225
     D  SCFFND                 1      3  0
     D  SCTFND                 4      6  0
     D  SCFCAS                 7     13  0
     D  SCTCAS                14     20  0
     D  SCFYR                 21     23  0
     D  SCTYR                 24     26  0
     D  SCFCLA                27     31  0
     D  SCTCLA                32     36  0
     D  SCFRGN                37     39  0
     D  SCTRGN                40     42  0
     D  SCFAWW                43     49  2
     D  SCTAWW                50     56  2
     D  SCFEMP                57     65  0
     D  SCTEMP                66     74  0
     D  SCFRAT                75     81  2
     D  SCTRAT                82     88  2
     D  SCFDIV                89     93  0
     D  SCTDIV                94     98  0
     D  SCFACC                99    106  0
     D  SCTACC               107    114  0
     D  SCFLT                115    115
     D  SCTLT                116    116
     D  SCFREC               117    124  0
     D  SCTREC               125    132  0
     D  SCFNCC               133    134
     D  SCTNCC               135    136
     D  SCFENT               137    144  0
     D  SCTENT               145    152  0
     D  SCFNAT               153    154
     D  SCTNAT               155    156
     D  SCFCLO               157    164  0
     D  SCTCLO               165    172  0
     D  SCFPAR               173    174
     D  SCTPAR               175    176
     D  SCFPAY               177    187  2
     D  SCTPAY               188    198  2
     D  SCFCAU               199    200
     D  SCTCAU               201    202
     D  SCFRES               203    213  2
     D  SCTRES               214    224  2
     D  SCSTAT               225    225
      *
      *  KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
      *
      *
      *
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      * FIRST CYCLE PROCESSING
      *
     C  N11              EXSR      SR1ST
      *
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
      *
     C                   MOVE      UPFND         SCFND             3
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE             8 0
      *
      *  GET THE COMPANY RECORD
      *
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  SCCONM        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCCONM
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  QQPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        QQPRDS
      *
     C                   MOVE      QQPRDS        SCPRDS
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      ***** BEG ADD ******* 09/08/99 **********************************
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
      ***** END ADD ******* 09/08/99 **********************************
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *   COMMAND 01 = END OF JOB
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90     F10=SEARCH
      ***** BEG ADD ******* 09/08/99 **********************************
     C     *INKT         CASEQ     *ON           KTDO                     90     F19=SUBMIT
      ***** END ADD ******* 09/08/99 **********************************
     C                   ENDCS
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      '1'
      *
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
      ***** BEG ADD ******* 09/08/99 **********************************
      *
      *   PREV SCREEN
      *
     C     *INKL         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 09/08/99 **********************************
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE
     C                   GOTO      STRT01
     C                   END
      *
     C                   END
      ***** BEG ADD ******* 09/08/99 **********************************
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      ***** END ADD ******* 09/08/99 **********************************
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      ***** BEG ADD ******* 09/08/99 **********************************
     C                   MOVEL(P)  'SCR01'       Q2RECF
      ***** END ADD ******* 09/08/99 **********************************
      *
      *  VALIDATE FROM LOST TIME CODE
      *
     C     SCFLT         IFNE      'L'
     C     SCFLT         ANDNE     'N'                                          SAJ 10-3-88
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
      *
      *  VALIDATE TO LOST TIME CODE
      *
     C     SCTLT         IFNE      'N'
     C     SCTLT         ANDNE     'L'                                          SAJ 10-3-88
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
      *
      *  VALIDATE FROM ACCIDENT DATE
      *
     C     SCFACC        IFNE      *ZERO
     C                   Z-ADD     SCFACC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFACC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *  Validate TO Accident Date
      *
     C     SCTACC        IFNE      *HIVAL
     C                   Z-ADD     SCTACC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTACC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *  VALIDATE FROM RECEIVED DATE
      *
     C     SCFREC        IFNE      *ZERO
     C                   Z-ADD     SCFREC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFREC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *  VALIDATE TO RECEIVED DATE
      *
     C     SCTREC        IFNE      *HIVAL
     C                   Z-ADD     SCTREC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTREC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *  VALIDATE FROM ENTERED DATE
      *
     C     SCFENT        IFNE      *ZERO
     C                   Z-ADD     SCFENT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFENT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *  VALIDATE TO ENTERED DATE
      *
     C     SCTENT        IFNE      *HIVAL
     C                   Z-ADD     SCTENT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTENT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *  VALIDATE FROM CLOSED DATE
      *
     C     SCFCLO        IFNE      *ZERO
     C                   Z-ADD     SCFCLO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFCLO
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      *  VALIDATE TO CLOSED DATE
      *
     C     SCTCLO        IFNE      *HIVAL
     C                   Z-ADD     SCTCLO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTCLO
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      * VALIDATE CLAIM STATUS SELECTION - 'C' 'O' OR *BLANK
      *
     C     SCSTAT        IFNE      *BLANK
     C     SCSTAT        IFNE      'C'
     C     SCSTAT        ANDNE     'O'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C                   END
      *
      ***** BEG DEL ******* 09/08/99 **********************************
     C************         MOVE *BLANKS   ##CO#   3
     C************         MOVE UPCO#     ##CO#
      ************
      ****CALL*THE CLP TO SUBMIT THE LIST.
      ************
     C************         CALL 'W7170C5'
     C************         PARM           UPOUTQ
     C************         PARM QQCALL    ##CALL
     C************         PARM           DATA
     C************         PARM           ##CO#            ADD 01/05/94
      ***** END DEL ******* 09/08/99 **********************************
      ***** BEG ADD ******* 09/08/99 **********************************
      *
      *  IF THE USER HIT ENTER WITH THE GROUP AND POLICY YEAR FIELDS
      *   LEFT AS THE DEFULTS THEN SEND AN INFORMATIONAL SCREEN
      *   TO VERIFY THAT THEY REALLY WANT TO DO THIS.
      *
     C                   SETOFF                                       8384
      *
     C     SCFFND        IFEQ      *ZEROS
     C     SCTFND        ANDEQ     *HIVAL
     C                   SETON                                        83
     C                   ENDIF
      *
     C     SCFYR         IFEQ      *ZEROS
     C     SCTYR         ANDEQ     *HIVAL
     C                   SETON                                        84
     C                   ENDIF
      *
     C     *IN83         IFEQ      *ON
     C     *IN84         OREQ      *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   EXSR      KTDO
      *
      *
      *
     C     SCR02T        TAG
      *
     C                   SETOFF                                       8384
      *
     C     SCFFND        IFEQ      *ZEROS
     C     SCTFND        ANDEQ     *HIVAL
     C                   SETON                                        83
     C                   ENDIF
      *
     C     SCFYR         IFEQ      *ZEROS
     C     SCTYR         ANDEQ     *HIVAL
     C                   SETON                                        84
     C                   ENDIF
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
      ***** END ADD ******* 09/08/99 **********************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
      *
     CSR   SR1ST         BEGSR
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
      *  INITIALIZE FIELDS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      UPCO#         XXCO#             3 0
      *
      *  INITIALIZE FIELDS FOR OUTPUT TO SCREEN
      *
     C                   Z-ADD     *ZERO         SCFFND
     C                   Z-ADD     *HIVAL        SCTFND
     C                   Z-ADD     *ZERO         SCFCAS
     C                   Z-ADD     *HIVAL        SCTCAS
     C                   Z-ADD     *ZERO         SCFYR
     C                   Z-ADD     *HIVAL        SCTYR
     C                   Z-ADD     *ZERO         SCFCLA
     C                   Z-ADD     *HIVAL        SCTCLA
     C                   Z-ADD     *ZERO         SCFRGN
     C                   Z-ADD     *HIVAL        SCTRGN
     C                   Z-ADD     *ZERO         SCFAWW
     C                   Z-ADD     *HIVAL        SCTAWW
     C                   Z-ADD     *ZERO         SCFEMP
     C                   Z-ADD     *HIVAL        SCTEMP
     C                   Z-ADD     *ZERO         SCFRAT
     C                   Z-ADD     *HIVAL        SCTRAT
     C                   Z-ADD     *ZERO         SCFDIV
     C                   Z-ADD     *HIVAL        SCTDIV
     C                   Z-ADD     *ZERO         SCFACC
     C                   Z-ADD     *HIVAL        SCTACC
     C                   MOVE      'L'           SCFLT
     C                   MOVE      'N'           SCTLT
     C                   Z-ADD     *ZERO         SCFREC
     C                   Z-ADD     *HIVAL        SCTREC
     C                   MOVE      *BLANK        SCFNCC
     C                   MOVE      '99'          SCTNCC
     C                   Z-ADD     *ZERO         SCFENT
     C                   Z-ADD     *HIVAL        SCTENT
     C                   MOVE      *BLANK        SCFNAT
     C                   MOVE      '99'          SCTNAT
     C                   MOVE      *ZERO         SCFCLO
     C                   MOVE      *HIVAL        SCTCLO
     C                   MOVE      *BLANK        SCFPAR
     C                   MOVE      '99'          SCTPAR
     C                   Z-ADD     *ZERO         SCFPAY
     C                   Z-ADD     *HIVAL        SCTPAY
     C                   MOVE      *BLANK        SCFCAU
     C                   MOVE      '99'          SCTCAU
     C                   Z-ADD     *ZERO         SCFRES
     C                   Z-ADD     *HIVAL        SCTRES
     C                   MOVE      *BLANK        SCSTAT
      *
     C*** BEG ADD *** 03/03/03 ****************************
     C*
     C* IF UPXFND IS YES SET ON INDICATOR 61 TO LOAD FND AND RESTRICT.
     C*
     C     UPXFND        IFEQ      'Y'
     C*
     C                   SETON                                        12
     C                   Z-ADD     UPFND         SCFFND
     C                   Z-ADD     UPFND         SCTFND
     C*
     C                   ENDIF
     C*
     C* IF UPXEMP IS YES SET ON INDICATOR 62 TO LOAD EMP AND RESTRICT.
     C*
     C     UPXEMP        IFEQ      'Y'
     C*
     C                   SETON                                        13
     C                   Z-ADD     UPEMP#        SCFEMP
     C                   Z-ADD     UPEMP#        SCTEMP
     C                   Z-ADD     UPDIV         SCFDIV
     C                   Z-ADD     UPDIV         SCTDIV
     C*
     C                   ENDIF
     C*** END ADD *** 03/03/03 ****************************
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
      *
     C                   ENDSR
      *
      *
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      *   FROM FUND
      *
     C     HFLD          WHENEQ    'SCFFND'
      *
     C                   MOVE      UPCO#         ##CO#
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM      *BLANKS       #1FYR             3
     C                   PARM                    ##VIEW            1 0
      *
     C     ##FND         IFEQ      *BLANKS
     C                   SETON                                        41
     C                   ELSE
     C                   SETON                                        42
     C                   MOVE(P)   ##FND         SCFFND
     C                   ENDIF
      *
      *   TO FUND
      *
     C     HFLD          WHENEQ    'SCTFND'
      *
     C                   MOVE      UPCO#         ##CO#
     C                   CALL      'W0010R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM      *BLANKS       #1FYR             3
     C                   PARM                    ##VIEW            1 0
      *
     C     ##FND         IFEQ      *BLANKS
     C                   SETON                                        42
     C                   ELSE
     C                   SETON                                        43
     C                   MOVE(P)   ##FND         SCTFND
     C                   ENDIF
      *
      *   FROM POLICY YEAR
      *
     C     HFLD          WHENEQ    'SCFYR'
     C                   SETON                                        4382
     C                   GOTO      STRT
      *
      *   TO POLICY YEAR
      *
     C     HFLD          WHENEQ    'SCTYR'
     C                   SETON                                        4482
     C                   GOTO      STRT
      *
      *   FROM REGION
      *
     C     HFLD          WHENEQ    'SCFRGN'
     C                   SETON                                        4582
     C                   GOTO      STRT
      *
      *   TO REGION
      *
     C     HFLD          WHENEQ    'SCTRGN'
     C                   SETON                                        4682
     C                   GOTO      STRT
      *
      *   FROM EMPLOYER
      *
     C     HFLD          WHENEQ    'SCFEMP'
     C     HFLD          OREQ      'SCFDIV'
     C                   CALL      'W0050R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      *ZERO         ##RRN2            9 0
     C                   PARM      *BLANK        ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM      *ZERO         ##RRN3            9 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        47
     C                   ELSE
     C                   SETON                                        48
     C     ##RRN         CHAIN     PEM01                              60
     C                   Z-ADD     EMEMP#        SCFEMP
     C                   Z-ADD     EMDIV         SCFDIV
     C                   ENDIF
      *
      *   TO EMPLOYER
      *
     C     HFLD          WHENEQ    'SCTEMP'
     C     HFLD          OREQ      'SCTDIV'
     C                   CALL      'W0050R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      *ZERO         ##RRN2            9 0
     C                   PARM      *BLANK        ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM      *ZERO         ##RRN3            9 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        48
     C                   ELSE
     C                   SETON                                        49
     C     ##RRN         CHAIN     PEM01                              60
     C                   Z-ADD     EMEMP#        SCTEMP
     C                   Z-ADD     EMDIV         SCTDIV
     C                   ENDIF
      *
      *   FROM LOSS TIME FLAG
      *
     C     HFLD          WHENEQ    'SCFLT'
     C                   SETON                                        4982
     C                   GOTO      STRT
      *
      *   TO LOSS TIME FLAG
      *
     C     HFLD          WHENEQ    'SCTLT'
     C                   SETON                                        5082
     C                   GOTO      STRT
      *
      *   FROM NCCI CODE
      *
     C     HFLD          WHENEQ    'SCFNCC'
      *
     C                   CALL      'W2493R'
     C                   PARM      QQPRDS        ##PRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      '  NC'        QQID              4
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    QQRTCD            1
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        51
     C                   ELSE
     C                   SETON                                        52
     C                   MOVEL(P)  ##KEY         SCFNCC
     C                   ENDIF
      *
      *   TO NCCI CODE
      *
     C     HFLD          WHENEQ    'SCTNCC'
      *
     C                   CALL      'W2493R'
     C                   PARM      QQPRDS        ##PRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      '  NC'        QQID              4
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    QQRTCD            1
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        52
     C                   ELSE
     C                   SETON                                        53
     C                   MOVEL(P)  ##KEY         SCTNCC
     C                   ENDIF
      *
      *   FROM NATURE CODE
      *
     C     HFLD          WHENEQ    'SCFNAT'
      *
     C                   CALL      'W2121R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    ##VIEW            1 0
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        53
     C                   ELSE
     C                   SETON                                        54
     C                   MOVEL(P)  ##KEY         SCFNAT
     C                   ENDIF
      *
      *   TO NATURE CODE
      *
     C     HFLD          WHENEQ    'SCTNAT'
      *
     C                   CALL      'W2121R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    ##VIEW            1 0
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        54
     C                   ELSE
     C                   SETON                                        55
     C                   MOVEL(P)  ##KEY         SCTNAT
     C                   ENDIF
      *
      *   TABLE FILE SEARCH - FROM PART CODE
      *
     C     HFLD          WHENEQ    'SCFPAR'
      *
     C                   CALL      'W2126R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    ##VIEW            1 0
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        55
     C                   ELSE
     C                   SETON                                        56
     C                   MOVEL(P)  ##KEY         SCFPAR
     C                   ENDIF
      *
      *   TO PART CODE
      *
     C     HFLD          WHENEQ    'SCTPAR'
      *
     C                   CALL      'W2126R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    ##VIEW            1 0
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        56
     C                   ELSE
     C                   SETON                                        57
     C                   MOVEL(P)  ##KEY         SCTPAR
     C                   ENDIF
      *
      *   FROM CAUSE CODE
      *
     C     HFLD          WHENEQ    'SCFCAU'
      *
     C                   CALL      'W2131R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM                    ##VIEW            1 0
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        57
     C                   ELSE
     C                   SETON                                        58
     C                   MOVEL(P)  ##KEY         SCFCAU
     C                   ENDIF
      *
      *   TO CAUSE CODE
      *
     C     HFLD          WHENEQ    'SCTCAU'
      *
     C                   CALL      'W2131R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    ##VIEW            1 0
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        58
     C                   ELSE
     C                   SETON                                        59
     C                   MOVEL(P)  ##KEY         SCTCAU
     C                   ENDIF
      *
      *   CLAIM STATUS
      *
     C     HFLD          WHENEQ    'SCSTAT'
     C                   SETON                                        5982
     C                   GOTO      STRT
      *
      *   FROM CASE
      *
     C     HFLD          WHENEQ    'SCFCAS'
      *
     C                   CALL      'W0070R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      'CN'          ##SORT            2
     C                   PARM      *ZERO         ##RRN2            9 0
     C                   PARM                    ##FYR             3 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        61
     C                   ELSE
     C                   SETON                                        62
     C     ##RRN         CHAIN     PCM01                              60
     C                   Z-ADD     CMCASE        SCFCAS
     C                   ENDIF
      *
      *   TO CASE
      *
     C     HFLD          WHENEQ    'SCTCAS'
      *
     C                   CALL      'W0070R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      'CN'          ##SORT            2
     C                   PARM      *ZERO         ##RRN2            9 0
     C                   PARM                    ##FYR             3 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        62
     C                   ELSE
     C                   SETON                                        63
     C     ##RRN         CHAIN     PCM01                              60
     C                   Z-ADD     CMCASE        SCTCAS
     C                   ENDIF
      *
      *  FROM CLASS
      *
     C     HFLD          WHENEQ    'SCFCLA'
     C                   SETON                                        6382
     C                   GOTO      STRT
      *
      *  TO CLASS
      *
     C     HFLD          WHENEQ    'SCTCLA'
     C                   SETON                                        6482
     C                   GOTO      STRT
      *
      *  FROM AWW
      *
     C     HFLD          WHENEQ    'SCFAWW'
     C                   SETON                                        6582
     C                   GOTO      STRT
      *
      *  TO AWW
      *
     C     HFLD          WHENEQ    'SCTAWW'
     C                   SETON                                        6682
     C                   GOTO      STRT
      *
      *  FROM RATE
      *
     C     HFLD          WHENEQ    'SCFRAT'
     C                   SETON                                        6782
     C                   GOTO      STRT
      *
      *  TO RATE
      *
     C     HFLD          WHENEQ    'SCTRAT'
     C                   SETON                                        6882
     C                   GOTO      STRT
      *
      *  FROM ACCIDENT DATE
      *
     C     HFLD          WHENEQ    'SCFACC'
     C                   SETON                                        6982
     C                   GOTO      STRT
      *
      *  TO ACCIDENT DATE
      *
     C     HFLD          WHENEQ    'SCTACC'
     C                   SETON                                        7082
     C                   GOTO      STRT
      *
      *  FROM DATE RECEIVED
      *
     C     HFLD          WHENEQ    'SCFREC'
     C                   SETON                                        7182
     C                   GOTO      STRT
      *
      *  TO DATE RECEIVED
      *
     C     HFLD          WHENEQ    'SCTREC'
     C                   SETON                                        7282
     C                   GOTO      STRT
      *
      *  FROM DATE ENTERED
      *
     C     HFLD          WHENEQ    'SCFENT'
     C                   SETON                                        7382
     C                   GOTO      STRT
      *
      *  TO DATE ENTERED
      *
     C     HFLD          WHENEQ    'SCTENT'
     C                   SETON                                        7482
     C                   GOTO      STRT
      *
      *  FROM DATE CLOSED
      *
     C     HFLD          WHENEQ    'SCFCLO'
     C                   SETON                                        7582
     C                   GOTO      STRT
      *
      *  TO DATE CLOSED
      *
     C     HFLD          WHENEQ    'SCTCLO'
     C                   SETON                                        7682
     C                   GOTO      STRT
      *
      *  FROM PAYMENT AMMOUNT
      *
     C     HFLD          WHENEQ    'SCFPAY'
     C                   SETON                                        7782
     C                   GOTO      STRT
      *
      *  TO PAYMENT AMOUNT
      *
     C     HFLD          WHENEQ    'SCTPAY'
     C                   SETON                                        7882
     C                   GOTO      STRT
      *
      *  FROM TOTAL INCURRED
      *
     C     HFLD          WHENEQ    'SCFRES'
     C                   SETON                                        7982
     C                   GOTO      STRT
      *
      *  TO TOTAL INCURRED
      *
     C     HFLD          WHENEQ    'SCTRES'
     C                   SETON                                        8082
     C                   GOTO      STRT
      *
      *
     C                   ENDSL
      *
     C                   WRITE     SCR99
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSR                                                  KJDO
      *
      ***** BEG ADD ******* 09/08/99 **********************************
      *
      *
      *===============================================================
      * KTDO - SUBMIT
      *===============================================================
     C     KTDO          BEGSR
      *
     C                   MOVE      *BLANKS       ##CO#             3
     C                   MOVE      UPCO#         ##CO#             3
      *
     C                   CALL      'W7170C5'
     C                   PARM                    UPOUTQ
     C                   PARM      QQCALL        ##CALL
     C                   PARM                    DATA
     C                   PARM                    ##CO#                          ADD 01/05/94
      *
     C                   GOTO      TERM
      *
     C                   ENDSR                                                  KTDO
      ***** END ADD ******* 09/08/99 **********************************
      *
      *
