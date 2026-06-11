     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0360R - EXCESS CLAIMS DIARY
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*  08/27/02 C:15  Project: WC577                                *
     H*                -Chgd the Rsv Srch pgm call.                   *
     H*                -Fixed the SCR02 function keys process.        *
     H*                                                               *
     H*  12/08/99 C:118 PROJECT: 577
     H*                 V3 DISPLAY FILE CHANGES.
     H*
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/07/92  JEI  PROJECT: WC157                                *
     H*                 CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR     *
     H*                 'BASE' FILES (XXCASE) - CASE NUMBER REMAINS   *
     H*                 14 ALPHA FOR REINSURANCE FILES (X@CASE)       *
     H*                                                               *
     H*  04/03/92  LPK  PROJECT: MD027                                *
     H*                 CHANGED CASE NUMBER FROM 7 NUMERIC TO 14      *
     H*                 ALPHA FIELD.  CHANGED PARMS ON CMD-11 TO      *
     H*                 MATCH EXISTING PARMS IN W0310R.               *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0360F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*
     F*** BEGIN ADD *** 01/28/98 ***********************************************
     F                                     INFDS(DBFDS)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     F*** END   ADD *** 01/28/98 ***********************************************
     F*
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWAMCL     IF   E           K DISK
     FWMEDL     UF A E           K DISK
     FWTABL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*
     D*** BEGIN CHG *** 01/28/98 ***********************************************
     D**********          UA      1   2 10
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D*** END   CHG *** 01/28/98 ***********************************************
     D*
     D*
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  DSSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D*** END   ADD *** 01/28/98 ***********************************************
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  SCX01                  1      2
     D  SCX02                  3      4
     D  SCX03                  5      6
     D  SCX04                  7      8
     D  SCX05                  9     10
     D  SCX06                 11     12
     D  SCX07                 13     14
     D  SCX08                 15     16
     D  SCX09                 17     18
     D  SCX10                 19     20
     D                 DS
     D  TP                     1     10
     D                                     DIM(10)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D  SCTP10                10     10
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  SCRR01                 1      9  2
     D  SCRR02                10     18  2
     D  SCRR03                19     27  2
     D  SCRR04                28     36  2
     D  SCRR05                37     45  2
     D  SCRR06                46     54  2
     D  SCRR07                55     63  2
     D  SCRR08                64     72  2
     D  SCRR09                73     81  2
     D  SCRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  SCAR01                 1      9  2
     D  SCAR02                10     18  2
     D  SCAR03                19     27  2
     D  SCAR04                28     36  2
     D  SCAR05                37     45  2
     D  SCAR06                46     54  2
     D  SCAR07                55     63  2
     D  SCAR08                64     72  2
     D  SCAR09                73     81  2
     D  SCAR10                82     90  2
     D                 DS
     D  TI                     1     90  2
     D                                     DIM(10)
     D  SCTI01                 1      9  2
     D  SCTI02                10     18  2
     D  SCTI03                19     27  2
     D  SCTI04                28     36  2
     D  SCTI05                37     45  2
     D  SCTI06                46     54  2
     D  SCTI07                55     63  2
     D  SCTI08                64     72  2
     D  SCTI09                73     81  2
     D  SCTI10                82     90  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Excess Claims Diary -
     D                                     Maintenance')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*  EXCESS CLAIM DIARY MASTER FILE
     C*
     C     EDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    S@CASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           KFLD           SCSS#
     C                   KFLD                    XXSS#             9
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C*  MULTIPLE CLAIM FILE
     C*
     C     MCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    S@CASE
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  REIN POLICY FILE
     C*
     C     RIKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL            1
     C                   KFLD                    CMACDT
     C*
     C     *LIKE         DEFINE    RIEMP#        XXEMP#
     C     *LIKE         DEFINE    RIDIV         XXDIV
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFND         Q1FND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        Q1CASE
     C     *LIKE         DEFINE    MCCLM#        QQCLM#
     C     *LIKE         DEFINE    CMEMP#        QQEMP#
     C     *LIKE         DEFINE    CMDIV         QQDIV
     C     *LIKE         DEFINE    CMSS#         QQSS#
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C********** *LIKE     DEFN CMACDT    QQEFDT
     C                   Z-ADD     *ZEROS        QQEFDT            8 0
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C     *LIKE         DEFINE    EDCASE        S@CASE
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
     C     QQCALL        COMP      '1'                                    09
     C                   TIME                    DSDTTM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPFND         SCFND
     C                   Z-ADD     UPFYR         SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        S@CASE
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C***** BEGIN ADD **** 12/08/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 12/08/99 ***********************************
     C                   MOVE      FNNAME        SCNAME
     C***** BEGIN ADD **** 12/08/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 12/08/99 ***********************************
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* F13=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C**********
     C****COMMAND 05 = CASE# SRCH - SCREEN 1
     C**********
     C********** Q2RECF    IFEQ 'SCR01'
     C**********
     C**********           MOVE *BLANKS   QQSS#
     C**********           Z-ADD0         QQEMP#
     C**********           Z-ADD0         QQDIV
     C**********           MOVE *BLANKS   QQRTCD  1
     C**********           CALL 'W0570R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCYR
     C**********           PARM           SCCASE
     C**********           PARM           QQSS#
     C**********           PARM           QQEMP#
     C**********           PARM           QQDIV
     C**********           PARM           QQRTCD
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** SCYR      OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           GOTO SCR01T
     C**********           END
     C**********           END
     C*
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C*   COMMAND 05 = RESERVE HISTORY SRCH - SCR02
     C*
     C*** Begin dlt *** 08/27/02 **************************************
     C********** Q2RECF    IFEQ 'SCR02'
     C*** End dlt ***** 08/27/02 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *INKE         IFEQ      *ON
     C*** END   ADD *** 01/28/98 **************************************
     C*** Begin add *** 08/27/02 **************************************
     C     Q2RECF        IFEQ      'SCR02'
     C*** End add ***** 08/27/02 **************************************
     C*
     C*** Begin chg *** 08/27/02 **************************************
     C******************** CALL 'W0574R'
     C                   CALL      'W0574R1'
     C*** End chg ***** 08/27/02 **************************************
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C*** Begin add *** 08/27/02 **************************************
     C                   PARM                    XXCO#
     C*** End add ***** 08/27/02 **************************************
     C                   PARM                    XXFND
     C*** Begin dlt *** 08/27/02 **************************************
     C******************** PARM           QQEMP#
     C******************** PARM           QQDIV
     C*** End dlt ***** 08/27/02 **************************************
     C                   PARM                    QQSS#
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    QQEFDT
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C**********
     C**F10=SEARCH
     C**********
     C********** *INKJ     IFEQ '1'
     C********** HFLD      IFEQ 'SCCASE'
     C**********           MOVE *BLANKS   QQSS#
     C********** SRTAG     TAG
     C**********
     C**********           Z-ADD0         QQEMP#
     C**********           Z-ADD0         QQDIV
     C**********           CALL 'W0571R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQSS#
     C**********           PARM           SCYR
     C**********           PARM           SCCASE
     C**********           PARM           QQEMP#
     C**********           PARM           QQDIV
     C**********           PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTCD    IFEQ 'B'
     C********** QQSS#     OREQ *BLANK
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********           GOTO SCR01T
     C**********           END
     C**********
     C**********           END
     C**********
     C**********           GOTO STRT
     C**********           END
     C*
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  SCREEN ONE
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 - SEARCH - POLICY YEAR OR CASE
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      'NM'          @@SORT            2
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM      SCFYR         @@FYR             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PCM02                              60
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*   COMMAND 07 = CLAIM STATUS SRCH - SCR02
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *INKG         IFEQ      *ON
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********
     C**********           CALL 'W0575R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQEMP#
     C**********           PARM           QQDIV
     C**********           PARM           QQSS#
     C**********           PARM           SCYR
     C**********           PARM           SCCASE
     C**********           PARM           QQEFDT
     C**********           PARM           QQRTCD  1
     C*
     C                   CALL      'W0575R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQSS#
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    QQEFDT
     C                   PARM                    QQRTCD            1
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   ENDIF                                                  *INKG=ON
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*
     C*  CMD  9
     C*
     C     *INKI         IFEQ      '1'
     C*
     C*  CMD  9 - ALPHA SEARCH - SCREEN 1
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANKS       QQSS#
     C                   CALL      'W0572R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQSS#
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQSS#         OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C********** QQRTCD    CABEQ'*'       SRTAG
     C*
     C                   END
     C*
     C*  CMD  9 - MULT CLAIM SRCH -  SCR02
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   Z-ADD     SCFYR         XXYR              2 0
     C                   MOVE      SCCLM#        QQCLM#
     C                   CALL      'W0321R'
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXYR
     C**********           PARM           QQCLM#
     C**********           PARM           QQRTCD
     C*
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    QQCALL
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C*** Begin add *** 08/27/02 **************************************
     C                   PARM                    XXFYR
     C*** End add ***** 08/27/02 **************************************
     C                   PARM                    QQCLM#
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
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
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 11 = REINSURANCE POLICY SEARCH - SCR02
     C*
     C     *INKK         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C                   MOVE      'S'           XXASFL
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C     RIKEY         SETGT     PRI01
     C                   READP     PRI01                                  60
     C     *IN60         IFEQ      '1'
     C     RICO#         ORNE      XXCO#
     C     RIFND         ORNE      XXFND
     C     RIEMP#        ORNE      XXEMP#
     C     RIDIV         ORNE      XXDIV
     C     RIASFL        ORNE      'S'
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     RIKEY         SETGT     PRI01
     C                   READP     PRI01                                  60
     C                   END
     C     *IN60         IFEQ      '0'
     C     RICO#         ANDEQ     XXCO#
     C     RIFND         ANDEQ     XXFND
     C     RIEMP#        ANDEQ     XXEMP#
     C     RIDIV         ANDEQ     XXDIV
     C     RIASFL        ANDEQ     'S'
     C                   MOVE      XXEMP#        Q@EMP#
     C                   MOVE      XXDIV         Q@DIV
     C*
     C                   Z-ADD     RIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEDT             8 0
     C*
     C                   MOVE      XXEDT         Q1EFDT            8
     C                   MOVE      'S'           QQASFL
     C                   ELSE
     C                   MOVE      '0'           Q@EMP#
     C                   MOVE      '0'           Q@DIV
     C                   MOVE      '00000000'    Q1EFDT
     C                   MOVE      *BLANKS       QQASFL            1
     C                   END
     C                   MOVE      '000'         QQFYR             3
     C                   MOVE      SCFYR         QQFYR
     C*
     C                   CALL      'W0310R'
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   PARM                    QQPRDS
     C                   PARM      '1'           QQCALL
     C                   PARM                    Q@EMP#            9
     C                   PARM                    Q@DIV             5
     C                   PARM                    QQASFL            1
     C                   PARM                    Q1EFDT
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C                   END
     C*
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C*   COMMAND 15 = SEARCH PAYMENTS - SCR01
     C*
     C     *INKP         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCFND         Q1FND
     C                   MOVE      SCCASE        Q1CASE
     C                   CALL      'W0576R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    Q1FND
     C                   PARM                    XXFYR
     C                   PARM                    Q1CASE
     C                   PARM                    XXSEQ             5 0
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  CLAIM MUST BE VALID
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     CMKEY         CHAIN     PCM01                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  GET EXCESS DIARY INFO FOR CLAIM
     C*
     C                   MOVE      *ZEROS        S@CASE
     C                   MOVE      SCCASE        S@CASE
     C     EDKEY         CHAIN     PED01                              60
     C*
     C     *IN60         IFEQ      '1'
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVE UA,1      SCMODE
     C                   MOVE      MD(2)         SCMODE
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   Z-ADD     0             SCDATE
     C                   MOVE      *BLANKS       SCCOMM
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PED01E
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVE UA,2      SCMODE
     C                   MOVE      MD(3)         SCMODE
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C*
     C                   Z-ADD     EDDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   MOVE      EDCOMM        SCCOMM
     C                   END
     C*
     C                   Z-ADD     CMEMP#        QQEMP#
     C                   Z-ADD     CMDIV         QQDIV
     C                   MOVE      CMSS#         QQSS#
     C                   Z-ADD     0             QQEFDT
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVE CMSS#     SCSS#
     C                   MOVE      CMSS#         XXSS#
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C                   MOVE      CMSS#         DSSS#
     C*
     C     DSSS#         IFNE      *BLANKS
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#            11
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'DSSS#':DSSS#));
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C                   ENDIF                                                  DSSS#<>BLANKS
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*  GET CLAIMANTS NAME
     C*
     C                   MOVE      *BLANKS       SCCLNM
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCLNM
     C                   END
     C*
     C*  CHECK FOR MULTIPLE CLAIM NUMBER
     C*
     C                   Z-ADD     0             SCCLM#
     C     MCKEY         CHAIN     PMC01                              78
     C  N78              Z-ADD     MCCLM#        SCCLM#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      CMSTAT        SCOCST
     C*
     C                   MOVE      CMT           RT
     C                   Z-ADD     CMR           RR
     C                   Z-ADD     CMA           AR
     C                   XFOOT     RR            SCRRTO
     C                   XFOOT     CMA           SCARTO
     C     CMRR01        ADD       CMAR01        SCTI01
     C     CMRR02        ADD       CMAR02        SCTI02
     C     CMRR03        ADD       CMAR03        SCTI03
     C     CMRR04        ADD       CMAR04        SCTI04
     C     CMRR05        ADD       CMAR05        SCTI05
     C     CMRR06        ADD       CMAR06        SCTI06
     C     CMRR07        ADD       CMAR07        SCTI07
     C     CMRR08        ADD       CMAR08        SCTI08
     C     CMRR09        ADD       CMAR09        SCTI09
     C     CMRR10        ADD       CMAR10        SCTI10
     C                   XFOOT     TI            SCTITO
     C*
     C                   Z-ADD     1             I                 3 0
     C     I             DOUGT     10
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RT(I)         XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C  N60              MOVEL     RTTYPE        TP(I)
     C   60              MOVE      *BLANK        TP(I)
     C                   ADD       1             I
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C* PROCESS SCREEN 2 - CLAIM INFORMATION
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           INQ2
     C*
     C*  DIARY DATE MUST BE VALID IF ENTERED
     C*
     C     SCDATE        IFNE      0
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ALL EDITS PASSED, UPDATE DIARY RECORD
     C*
     C     EDKEY         CHAIN     PED01                              60
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EDDATE
     C*
     C                   MOVE      SCCOMM        EDCOMM
     C                   MOVE      *BLANK        EDSTAT
     C                   MOVE      Q1USER        EDUSER
     C                   MOVE      Q1PGM         EDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EDDT
     C*
     C                   Z-ADD     DSTIME        EDTIME
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PED01
     C                   ELSE
     C     SCDATE        IFNE      0
     C                   Z-ADD     CMCO#         EDCO#
     C                   Z-ADD     CMFND         EDFND
     C                   Z-ADD     CMFYR         EDFYR
     C                   MOVE      *ZEROS        EDCASE
     C                   MOVE      CMCASE        EDCASE
     C                   WRITE     PED01
     C                   END
     C                   END
     C*
     C     INQ2          TAG
     C*
     C                   Z-ADD     0             SCFYR
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        S@CASE
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPED01     E            PED01E
     O*
     O*** BEGIN DEL *** 01/28/98 **************************************
     O********** **  TABLE UA
     O********** -- ADD --
     O********** - UPDATE -
     O*** END   DEL *** 01/28/98 **************************************
     O*
     O*** BEGIN ADD *** 01/28/98 **************************************
     O*   ADDED TABLE MD FOR SCMODE
     O*** END   ADD *** 01/28/98 **************************************
     O*
**  TABLE MD
- Display-
-- Add --
- Change -
- Delete -
