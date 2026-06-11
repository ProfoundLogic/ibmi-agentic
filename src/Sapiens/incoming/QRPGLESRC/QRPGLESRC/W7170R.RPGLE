     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7170R - Prompt for Claims Listing / Selection Criteria
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  06/21/04 C:53 SUPPORT: 42169
     H*                CHANGE UNIT MANAGER TO CLAIM MANAGER
     H*                                                               *
     H*  08/16/02 C:15  PROJECT: WC577                                *
     H*                -Reversed the primary and secondary sorts on   *
     H*                 the prompt screen.                            *
     H*                -Added Jurisdiction state to the primary sort  *
     H*                 selection.                                    *
     H*                                                               *
     H*  05/14/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*  03/20/00 C:118 PROJECT: WC577                                *
     H*                 Add output option to display SCTYPO(P/F/B).   *
     H*                 Validate entry and pass parm.                 *
     H*                                                               *
     H*  11/08/99 C:15  Support:  WC096/20563                         *
     H*                 Changed pgm to load the correct user's outq   *
     H*                 when calling the submitter pgm.               *
     H*                                                               *
     H*  07/01/99 C:118 SUPPORT:  WC033/16897                         *
     H*                 Modified display file fields SCFINC and SCTINC*
     H*                 so report W7171R routing page would print out *
     H*                 range up to 999,999,999.99.                   *
     H*                                                               *
     H*  06/02/99 C:61 PROJECT 101AT                                  *
     H*                Restrict access to claims upon a combination   *
     H*                of user parm flags and Group/Employer          *
     H*                restrictions.                                  *
     H*                                                               *
     H*  05/10/99 C:33 SEQ#: WC051/17998                              *
     H*                ALLOW FOR DIVISION RESTRICTION.                *
     H*                                                               *
     H*  03/12/99 C:15 Support: WC033/16897                           *
     H*                Changed the total incurred parms to match the  *
     H*                length of the screen fields.                   *
     H*                                                               *
     H*  06/16/98 C:90 PROJECT: WC828                                 *
     H*                ADD SELECTION OF TOTAL INCURRED RANGE.         *
     H*                                                               *
     H*  11/07/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                REARRANGED REVISIONS SECTION.                  *
     H*                                                               *
     H*  05/06/97 C:62 PROJ: WC665                                    *
     H*                PROVIDE OPTION TO RUN THE REPORT IN CLAIMANT   *
     H*                NAME ORDER.                                    *
     H*                                                               *
     H*  01/05/94 C:10 PROJ: WC290                                    **
     H*                FIX REPORT USAGE OF UPCO#                      **
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
      *
      *
     FW7170F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FWMCMP     IF   E             DISK

     D*
     D***  BEGIN ADD  ***  05/10/99  *********************************
     D OPTNM           C                   CONST('Claims Listing / -
     D                                     Selection Criteria')
     D***  END   ADD  ***  05/10/99  *********************************
     D*
     D***  BEGIN ADD  ***  05/14/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/14/01  *********************************
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
     D                 DS
     D*
     D* CHANGED 4-26-88 LJQ TO ADD SELECTION FOR CLAIM STATUS
     D*
     D  DATA                   1    241
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
     D  SCFRNM               226    226
     D  SCTONM               227    227
     D  SCSUPR               228    230
     D  SCUNMG               231    233
     D  SCADJR               234    236
     D  SCFLDA               237    239
     D  SCSORT               240    240
     D  SCSRT2               241    241

     I*
     I* CHANGED 4-26-88 LJQ TO ADD SELECTION FOR CLAIM STATUS
     I*
      *
      *

     C*
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
     C     *LIKE         DEFINE    SCFEMP        QQFEMP
     C     *LIKE         DEFINE    SCFDIV        QQFDIV
     C     *LIKE         DEFINE    UPPRDT        XXDATE
     C*
     C     *ENTRY        PLIST
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C***  END   CHG  ***  05/10/99  *********************************
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
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/14/01 ********************************************
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
     C*** END ADD ******** 05/14/01 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C* ==================================================================
     C*   F12 = CANCEL
     C* ==================================================================
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*
     C*  CMD 10 - TABLE FILE SEARCH - FROM NCCI CODE
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFNCC'
     C*
     C                   MOVE      *BLANKS       SCKEY
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      '  NC'        QQID              4
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN53
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN28
     C                   MOVEL     SCKEY         SCFNCC
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  CMD 10 - TABLE FILE SEARCH - TO NCCI CODE
     C*
     C     HFLD          WHENEQ    'SCTNCC'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      '  NC'        QQID
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN29
     C                   MOVEL     SCKEY         SCTNCC
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  CMD 10 - TABLE FILE SEARCH - FROM NATURE CODE
     C*
     C     HFLD          WHENEQ    'SCFNAT'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2121R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    SCKEY
     C                   PARM      2             QQVIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN30
     C                   MOVEL     SCKEY         SCFNAT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  CMD 10 - TABLE FILE SEARCH - TO NATURE CODE
     C*
     C     HFLD          WHENEQ    'SCTNAT'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2121R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      2             QQVIEW
     C*
     C     *INKC         IFEQ      *ON
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN31
     C                   MOVEL     SCKEY         SCTNAT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  CMD 10 - TABLE FILE SEARCH - FROM PART CODE
     C*
     C     HFLD          WHENEQ    'SCFPAR'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2126R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      2             QQVIEW
     C*
     C     *INKC         IFEQ      *ON
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN55
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN32
     C                   MOVEL     SCKEY         SCFPAR
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  CMD 10 - TABLE FILE SEARCH - TO PART CODE
     C*
     C     HFLD          WHENEQ    'SCTPAR'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2126R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      2             QQVIEW
     C*
     C     *INKC         IFEQ      *ON
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN33
     C                   MOVEL     SCKEY         SCTPAR
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  CMD 10 - TABLE FILE SEARCH - FROM CAUSE CODE
     C*
     C     HFLD          WHENEQ    'SCFCAU'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2131R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      2             QQVIEW
     C*
     C     *INKC         IFEQ      *ON
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN56
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN34
     C                   MOVEL     SCKEY         SCFCAU
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*   COMMAND 10 = CLAIM SEARCH CALL
     C*
     C     HFLD          WHENEQ    'SCFCAS'
     C     HFLD          OREQ      'SCTCAS'
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZERO         @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      *ZERO         @@RRN2            9 0
     C                   PARM      *ZERO         @@FYR             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFNE      *ON
     C     @@RRN         ANDNE     *ZERO
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C     HFLD          IFEQ      'SCFCAS'
     C*
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCFCAS
     C                   MOVE      CMEMP#        SCFEMP
     C                   MOVE      CMDIV         SCFDIV
     C*
     C                   ELSE
     C*
     C                   MOVE      CMFYR         SCTYR
     C                   MOVE      CMCASE        SCTCAS
     C                   MOVE      CMEMP#        SCTEMP
     C                   MOVE      CMDIV         SCTDIV
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  CMD 10 - TABLE FILE SEARCH - TO CAUSE CODE
     C*
     C     HFLD          WHENEQ    'SCTCAU'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2131R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      2             QQVIEW
     C*
     C     *INKC         IFEQ      *ON
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN35
     C                   MOVEL     SCKEY         SCTCAU
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL
     C*
     C     HFLD          WHENEQ    'SCSUPR'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                        40
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL     UPSTAF        SCSUPR
     C                   SETON                                        41
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCUNMG'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                        41
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL     UPSTAF        SCUNMG
     C                   SETON                                        42
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCADJR'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                        42
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL     UPSTAF        SCADJR
     C                   SETON                                        43
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCFLDA'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                        43
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL     UPSTAF        SCFLDA
     C                   SETON                                        44
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   OTHER
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ EQ *ON
     C*
     C     *INKT         IFEQ      '1'
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#
     C*** BEGIN DLT *** 03/12/99 **************************************
      **** BEGIN ADD **** 06/16/98 ************************************
     C******************** MOVE SCFINC    QQFINC  9
     C******************** MOVE SCTINC    QQTINC  9
      **** END   ADD **** 06/16/98 ************************************
     C*** END DLT ***** 03/12/99 **************************************
     C*** BEGIN ADD *** 03/12/99 **************************************
     C                   MOVE      SCFINC        QQFINC           11
     C                   MOVE      SCTINC        QQTINC           11
     C*** END ADD ***** 03/12/99 **************************************
     C*
     C                   CALL      'W7170C'
     C*** BEGIN CHG *** 11/08/99 *************************************
     C******************** PARM           UPOUTQ
     C                   PARM                    SVOUTQ
     C*** END CHG ***** 11/08/99 *************************************
     C                   PARM                    QQCALL
     C                   PARM                    DATA
     C                   PARM                    QQCO#
      **** BEGIN ADD **** 06/16/98 ************************************
     C                   PARM                    QQFINC
     C                   PARM                    QQTINC
      **** END   ADD **** 06/16/98 ************************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   PARM      SCTYPO        QQTYPO            1
     C***** END   ADD **** 03/20/00 ***********************************
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  *INKT
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
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*** BEGIN ADD *** 11/08/99 *************************************
     C  N60              MOVEL     UPOUTQ        SVOUTQ
     C   60              MOVE      *BLANKS       SVOUTQ
     C*** END ADD ***** 11/08/99 *************************************
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  GROUP NUMBER MUST BE > OR = ZERO, AND < OR = 999
     C*
     C     SCTFND        IFGT      999
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE FROM LOST TIME CODE
     C*
     C     SCFLT         IFNE      'L'
     C     SCFLT         ANDNE     'N'                                          SAJ 10-3-88
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE TO LOST TIME CODE
     C*
     C     SCTLT         IFNE      'N'
     C     SCTLT         ANDNE     'L'                                          SAJ 10-3-88
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE FROM ACCIDENT DATE
     C*
     C     SCFACC        IFNE      *ZERO
     C                   Z-ADD     SCFACC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFACC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  Validate TO Accident Date
     C*
     C     SCTACC        IFNE      *HIVAL
     C                   Z-ADD     SCTACC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTACC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE FROM RECEIVED DATE
     C*
     C     SCFREC        IFNE      *ZERO
     C                   Z-ADD     SCFREC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFREC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE TO RECEIVED DATE
     C*
     C     SCTREC        IFNE      *HIVAL
     C                   Z-ADD     SCTREC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTREC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE FROM ENTERED DATE
     C*
     C     SCFENT        IFNE      *ZERO
     C                   Z-ADD     SCFENT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFENT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE TO ENTERED DATE
     C*
     C     SCTENT        IFNE      *HIVAL
     C                   Z-ADD     SCTENT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTENT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE FROM CLOSED DATE
     C*
     C     SCFCLO        IFNE      *ZERO
     C                   Z-ADD     SCFCLO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFCLO
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE TO CLOSED DATE
     C*
     C     SCTCLO        IFNE      *HIVAL
     C                   Z-ADD     SCTCLO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTCLO
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* CHANGED 4-26-88 LJQ TO ADD SELECTION FOR CLAIM STATUS
     C* VALIDATE CLAIM STATUS SELECTION - 'C' 'O' OR *BLANK
     C*
     C     SCSTAT        IFNE      *BLANK                                       LJQ 4-26-88
     C     SCSTAT        IFNE      'C'                                          LJQ 4-26-88
     C     SCSTAT        ANDNE     'O'                                          LJQ 4-26-88
     C                   MOVE      '1'           *IN39                          LJQ 4-26-88
     C                   GOTO      STRT                                         LJQ 4-26-88
     C                   END                                                    LJQ 4-26-88
     C                   END
     C*
     C*  SUPERVISOR STAFF CODE MUST BE ON FILE
     C*
     C     SCSUPR        IFNE      *BLANKS
     C     SCSUPR        CHAIN     PUP02                              45
     C     *IN45         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  UNIT MANAGER STAFF CODE MUST BE ON FILE
     C*
     C     SCUNMG        IFNE      *BLANKS
     C     SCUNMG        CHAIN     PUP02                              46
     C     *IN46         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ADJUSTER STAFF CODE MUST BE ON FILE
     C*
     C     SCADJR        IFNE      *BLANKS
     C     SCADJR        CHAIN     PUP02                              47
     C     *IN47         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  FIELD ADJUSTER STAFF CODE MUST BE ON FILE
     C*
     C     SCFLDA        IFNE      *BLANKS
     C     SCFLDA        CHAIN     PUP02                              48
     C     *IN48         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VERIFY THAT THE CLAIMANTS LAST NAME RANGE IS NOT BLANK
     C*    AND THE LETTERS ARE IN ACENDING ORDER
     C*
     C     SCFRNM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   END
     C*
     C*  VERIFY THAT THE CLAIMANTS LAST NAME RANGE IS NOT BLANK
     C*    AND THE LETTERS ARE IN ACENDING ORDER
     C*
     C     SCTONM        IFEQ      *BLANK
     C     SCTONM        ORLT      SCFRNM
     C                   MOVE      '1'           *IN50
     C                   GOTO      STRT
     C                   END
     C*
     C*  SORT SELECTION MUST EITHER BE "N" OR "C"
     C*
     C     SCSORT        IFNE      'N'
     C     SCSORT        ANDNE     'C'
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C*  SORT SELECTION #2, STAFF SORT  MUST EITHER BE "N", "S",
     C*   "U", "A" OR "F".
     C*
     C     SCSRT2        IFNE      'N'
     C     SCSRT2        ANDNE     'S'
     C*** BEG CHG *** 06/21/04 *****************************
     C********** SCSRT2    ANDNE'U'
     C     SCSRT2        ANDNE     'C'
     C*** END CHG *** 06/21/04 *****************************
     C     SCSRT2        ANDNE     'A'
     C     SCSRT2        ANDNE     'F'
     C*** Begin add *** 08/16/02 **************************************
     C     SCSRT2        ANDNE     'J'
     C*** End add ***** 08/16/02 **************************************
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   END
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*  VALIDATE OUTPUT TYPE IS P/F/B
     C*
     C     SCTYPO        IFNE      'P'
     C     SCTYPO        ANDNE     'F'
     C     SCTYPO        ANDNE     'B'
     C                   MOVE      '1'           *IN57
     C                   GOTO      STRT
     C                   ENDIF                                                  SCTYPO<>P/F/
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C*  IF THE USER HIT ENTER WITH THE GROUP AND POLICY YEAR FIELDS
     C*   LEFT AS THE DEFULTS THEN SEND AN INFORMATIONAL SCREEN
     C*   TO VERIFY THAT THEY REALLY WHAT TO DO THIS.
     C*
     C                   MOVE      *BLANK        XXIN20            1
     C                   MOVE      *BLANK        XXIN21            1
     C*
     C     SCFFND        IFEQ      *ZEROS
     C     SCTFND        ANDEQ     *HIVAL
     C                   MOVE      '1'           XXIN20                         FOR SCR02
     C                   END
     C*
     C     SCFYR         IFEQ      *ZEROS
     C     SCTYR         ANDEQ     *HIVAL
     C                   MOVE      '1'           XXIN21                         FOR SCR02
     C                   END
     C*
     C     XXIN20        COMP      '1'                                    20
     C     XXIN21        COMP      '1'                                    21
     C*
     C     *IN20         IFEQ      '1'
     C     *IN21         OREQ      '1'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
      **** BEGIN ADD **** 06/16/98 ************************************
     C                   MOVE      SCFINC        QQFINC
     C                   MOVE      SCTINC        QQTINC
      **** END   ADD **** 06/16/98 ************************************
     C                   CALL      'W7170C'
     C*** BEGIN CHG *** 11/08/99 *************************************
     C******************** PARM           UPOUTQ
     C                   PARM                    SVOUTQ
     C*** END CHG ***** 11/08/99 *************************************
     C                   PARM                    QQCALL
     C                   PARM                    DATA
     C                   PARM                    QQCO#
      **** BEGIN ADD **** 06/16/98 ************************************
     C                   PARM                    QQFINC
     C                   PARM                    QQTINC
      **** END   ADD **** 06/16/98 ************************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   PARM      SCTYPO        QQTYPO
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C*   PROCESS SCREEN 2 (INFORMATIONAL SCREEN)
     C*
     C     SCR02T        TAG
     C*
     C*  IF THIS SCREEN IS DISPLAYED, THEN THE ONLY WAY TO SUBMIT
     C*  THE JOB IS BY PRESSING COMMAND 19
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C     XXIN20        COMP      '1'                                    20
     C     XXIN21        COMP      '1'                                    21
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C**
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPPRDT        XXDATE
     C                   MOVE      UPFND         SCFND             3
     C*** BEGIN ADD *** 11/08/99 *************************************
     C  N60              MOVEL     UPOUTQ        SVOUTQ           10
     C   60              MOVE      *BLANKS       SVOUTQ
     C*** END ADD ***** 11/08/99 *************************************
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE             8 0
     C*
     C* IF UPXFND IS YES SET ON INDICATOR 61 TO LOAD FND AND RESTRICT.
     C*
     C     UPXFND        IFEQ      'Y'
     C*
     C                   SETON                                        80
     C*
     C                   ENDIF
     C*
     C* IF UPXEMP IS YES SET ON INDICATOR 62 TO LOAD EMP AND RESTRICT.
     C*
     C     UPXEMP        IFEQ      'Y'
     C*
     C                   SETON                                        81
     C*
     C                   ENDIF
     C*
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * IF Fund/Employer Restriction records SMAZP exist,
      *   allow entry of Fund/Employer
      *
     C     *IN80         IFEQ      *ON
     C     *IN81         OREQ      *ON
      *
     C                   CALL      'SA008R'
     C                   PARM                    UPCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C                   PARM      *BLANK        ##AUTH            1
      *
     C     ##RCDS        IFEQ      *ON
      *
      *    Restriction to Fund/Emplr to take place in report W7171R
      *
     C                   SETOFF                                       8081
     C                   ENDIF                                                  ##RTCD
      *
     C                   ENDIF                                                  *IN80, *IN81
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 06/16/98 ************************************
     C*******************  MOVE Q1USER    SCUSER
      **** END   DEL **** 06/16/98 ************************************
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  END   CHG  ***  05/10/99  *********************************
     C**
     C* * * INITIALIZE FIELDS FOR OUTPUT TO SCREEN
     C*
     C     *IN80         IFEQ      *OFF
     C*
     C                   Z-ADD     *ZERO         SCFFND
     C                   Z-ADD     *HIVAL        SCTFND
     C*
     C                   ELSE
     C*
     C                   Z-ADD     UPFND         SCFFND
     C                   Z-ADD     UPFND         SCTFND
     C*
     C                   ENDIF                                                  *IN61 *OFF
     C*
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
     C*
     C     *IN81         IFEQ      *OFF
     C*
     C                   Z-ADD     *ZERO         SCFEMP
     C                   Z-ADD     *HIVAL        SCTEMP
     C                   Z-ADD     *ZERO         SCFDIV
     C                   Z-ADD     *HIVAL        SCTDIV
     C*
     C                   ELSE
     C*
     C                   Z-ADD     UPEMP#        SCFEMP
     C                   Z-ADD     UPEMP#        SCTEMP
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     UPFL5         IFEQ      'Y'
     C                   MOVE      *ON           *IN82
     C***  END   ADD  ***  05/10/99  *********************************
     C                   Z-ADD     UPDIV         SCFDIV
     C                   Z-ADD     UPDIV         SCTDIV
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   ELSE
     C                   MOVE      *OFF          *IN82
     C                   Z-ADD     *ZERO         SCFDIV
     C                   Z-ADD     *HIVAL        SCTDIV
     C                   ENDIF                                                  UPFL5=5
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   ENDIF                                                  *IN81=*OFF
     C*
     C                   Z-ADD     *ZERO         SCFRAT
     C                   Z-ADD     *HIVAL        SCTRAT
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
      **** BEGIN ADD **** 06/16/98 ************************************
     C                   Z-ADD     *ZERO         SCFINC
     C                   Z-ADD     *HIVAL        SCTINC
      **** END   ADD **** 06/16/98 ************************************
     C                   MOVEL     'A'           SCFRNM
     C                   MOVEL     'Z'           SCTONM
     C                   MOVE      *BLANKS       SCSUPR
     C                   MOVE      *BLANKS       SCUNMG
     C                   MOVE      *BLANKS       SCADJR
     C                   MOVE      *BLANKS       SCFLDA
     C                   MOVE      'C'           SCSORT
     C                   MOVE      'N'           SCSRT2
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C*
