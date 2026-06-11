     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8400R -  GENERATE EXPERIENCE RATING MODIFIER REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM PROMPTS THE USER FOR UP TO THREE FUND YEARS     *
     H*  AND ONE OR ALL EMPLOYERS FOR WHICH THE EXPERIENCE RATING     *
     H*  MODIFIER REPORT IS TO BE RUN.                                *
     H*                                                               *
     H*  CALLED BY: MENU                                              *
     H*                                                               *
     H*  CALLS TO:  W0550R - EMPLOYER NUMERIC SEARCH                  *
     H*             W0551R - EMPLOYER ALPHA SEARCH                    *
     H*             W0511R - FUND YEAR SEARCH                         *
     H*             M0006C - HELP PROCESSOR                           *
     H*             W0006R - CHANGE USER PARAMETERS                   *
     H*             W8400C - SUBMIT ERM-6 REPORT                      *
     H*                                                               *
     H*  PROGRAMMER: C:09                                             *
     H*  CREATED:    05/27/93          PROJECT: 45AAI                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/18/02 C:61  SUPPORT WA099 34166                            *
     H*              - Correct searches.  Values not being returned.  *
     H*              - Correct inception mm/yy edit.                 *
     H*                                                               *
     H*  12/03/99  C:118 PROJECT: WC577                               *
     H*                  Made V3 changes to display file.             *
     H*                                                               *
     H*  06/30/98  C:90  Project:  WC577                              *
     H*                  CHANGED INCEPTION DATE FORMAT FROM MM/YY     *
     H*                  TO MM/YYYY & FUND TO GROUP ON SCREEN         *
     H*                                                               *
     H*  05/15/98  C:90  Project:  WC577                              *
     H*                  CHANGED SCREEN TO MEET VERSION 3 STANDARDS   *
     H*                  COMBINED ALL SEARCHES INTO F10 SEARCH        *
     H*                                                               *
     H*  01/19/97 C:62 PROJECT WC460                                  *
     H*                REESTABLISH OLD ERM-6 REPORT PATH.             *
     H*                8400F/R TO 8400C TO 8401C INSTEAD OF 8400C TO  *
     H*                8400C1, REMOVED AMT PARM.                      *
     H*                                                               *
     H*  05/29/96 C:33 PROJECT WC420                                  *
     H*                IF THIS REPORT IS CALLED ON DEMAND IT SHOULD   *
     H*                BE ROUTED TO THE USER PARM OUTQ INSTEAD OF THE *
     H*                OVERRIDE OUTQ.                                 *
     H*                                                               *
     H*  10/26/95  C:30  Project:  WC460                              *
     H*                  Expanded Format changes                      *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  GLOBAL                                                       *
     H*  60   GENERAL CHAIN INDICATOR                                 *
     H*  61   SEARCH FUND YEARS FOR FUND YEAR #1                      *
     H*  62   SEARCH FUND YEARS FOR FUND YEAR #2                      *
     H*  63   SEARCH FUND YEARS FOR FUND YEAR #3                      *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  20   FUND YEAR #1 IS NOT VALID FOR FUND                      *
     H*  21   EMPLOYER DOES NOT EXIST FOR FUND YEAR #1                *
     H*  22   FUND YEAR #2 IS NOT VALID FOR FUND                      *
     H*  23   EMPLOYER DOES NOT EXIST FOR FUND YEAR #2                *
     H*  24   FUND YEAR #3 IS NOT VALID FOR FUND                      *
     H*  25   EMPLOYER DOES NOT EXIST FOR FUND YEAR #3                *
     H*  26   AT LEAST ONE FUND YEAR MUST BE SPECIFIED                *
     H*  27   EMPLOYER IS NOT VALID                                   *
     H*  28   EFFECTIVE DATE MUST BE ENTERED                          *
     H*  29   DATA SET VALUED AS OF DATE MUST BE ENTERED              *
     H*  30   AMOUNT (TOTAL INCURRED) MUST BE ENTERED                 *
     H*  31   AMOUNT (CASTASTROPHE) MUST BE ENTERED                   *
     H*  41   POSITION CURSOR TO FUND YEAR #1                         *
     H*  42   POSITION CURSOR TO FUND YEAR #2                         *
     H*  43   POSITION CURSOR TO FUND YEAR #3                         *
     H*  44   POSITION CURSOR TO EMPLOYER                             *
     H*  45   POSITION CURSOR TO EFFECTIVE DATE                       *
     H*  46   POSITION CURSOR TO DATA SET VALUED AS OF DATE           *
     H*  47   POSITION CURSOR TO TOTAL INCURRED AMOUNT                *
     H*  48   POSITION CURSOR TO CASTASTROPHE AMOUNT                  *
     H*****************************************************************
     FW8400F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
     FWDELL     IF   E           K DISK

     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      ***** BEG DEL ******* 09/18/02 **********************************
      *********
     D*********   DS
      *****BEGIN CHG **** 06/30/98 ************************************
     D**************************************  1   40SCMMYY
     D*********                               1   60SCMMYY
      *****END*  CHG **** 06/30/98 ************************************
     D*********                               1   20XXSCMM
     D*********                               3   40XXSCYY
     D*********   DS
     D*********                               1   80XXDATE
     D*********                               1   20XXMM
     D*********                               3   40XXDD
     D*********                               5   60XXCN
     D*********                               7   80XXYY
      ***** END DEL ******* 09/18/02 **********************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
      ***** BEG DEL ******* 09/18/02 **********************************
     C*********            KFLD           XXDIV
      ***** END DEL ******* 09/18/02 **********************************
     C*
     C*  KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    F1FYR         XXFYR
     C     *LIKE         DEFINE    ELDIV         XXDIV
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
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      UPEMP#        SCEMP#
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   MOVE      *ZERO         SCEMP#
     C                   END
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********            MOVE *BLANKS   SCFYR1
     C*********            MOVE *BLANKS   SCFYR2
     C*********            MOVE *BLANKS   SCFYR3
     C                   Z-ADD     *ZERO         SCFYR1
     C                   Z-ADD     *ZERO         SCFYR2
     C                   Z-ADD     *ZERO         SCFYR3
      ***** END CHG ******* 09/18/02 **********************************
     C                   Z-ADD     *ZEROS        XXDIV
     C                   Z-ADD     0             SCMMYY
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
     C***** BEGIN ADD **** 12/03/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 12/03/99 ***********************************
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C***** BEGIN ADD **** 12/03/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFND
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 12/03/99 ***********************************
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      ***** BEG ADD ******* 09/18/02 **********************************
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 09/18/02 **********************************
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      **** BEGIN CHG **** 05/15/98 ************************************
      *
     C***************************************
     C*          F10 = SEARCH               *
     C***************************************
      *
      ***** BEG ADD ******* 09/18/02 **********************************
     C   KJ              EXSR      KJDO
      ***** END ADD ******* 09/18/02 **********************************
      ***** BEG DEL ******* 09/18/02 **********************************
     C*********  *INKJ     IFEQ '1'
     C*********
      *****BEGIN ADD **** 06/30/98 ************************************
     C*********            Z-ADDSCMMYY    SRDTI
     C*********            CALL 'SRCVTMY' DATCHK
     C*********  SRERR     IFEQ *OFF
     C*********            MOVELSRDTO     SCMMYY
     C*********            ENDIF
     C*********
     C*********            Z-ADDSCEFDT    SRDTI
     C*********            CALL 'SRCVTDAT'DATCHK
     C*********  SRERR     IFEQ *OFF
     C*********            MOVELSRDTO     SCEFDT
     C*********            ENDIF
     C*********
      *****BEGIN ADD **** 06/30/98 ************************************
      *********
     C***************************************
     C*******EMPLOYER SEARCH                *
     C***************************************
      *********
     C*********  HFLD      IFEQ 'SCEMP#'
      *********
     C*********
     C************  COMMAND 05 = EMPLOYER NUMERIC SEARCH
     C*********
     C********** *INKE     IFEQ '1'
     C*********            MOVE *BLANKS   SCEMP#
      *****END*  CHG **** 05/15/98 ************************************
     C*********  Q2RECF    IFEQ 'SCR01'
     C*********            CALL 'W0050R'
     C*********  *INKC     PARM           @@KC    1
     C*********  *INKL     PARM           @@KL    1
     C*********            PARM '2'       @@CALL  1
     C*********            PARM           @@RRN   90
     C*********            PARM DBRRN     @@RRN2  90
     C*********            PARM           @@QTE   1
     C*********            PARM 2         @@VIEW  10
     C*********            PARM           @@RRN3  90
     C*********
     C*********  *INKC     CABEQ*ON       TERM
     C*********
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********  *INKL     IFEQ *ON
     C*********  @@RRN     OREQ *ZEROS
     C*********            MOVE '1'       *IN44
     C*********            GOTO STRT
     C*********            ELSE
     C*********
     C*********  @@RRN     CHAINWMEMP                60
     C*********
     C*********            MOVE EMEMP#    SCEMP#
     C*********            MOVE EMDIV     XXDIV
     C*********            MOVE '1'       *IN45
     C*********            GOTO SCR01T
     C*********            END
     C*********
     C*********            END
      *****BEGIN DEL **** 05/15/98 ************************************
     C******************** END
     C*********
     C**********   COMMAND 07 = EMPLOYER ALPHA SEARCH
     C*********
     C********** *INKG     IFEQ '1'
      *****END*  DEL **** 05/15/98 ************************************
     C*********  Q2RECF    IFEQ 'SCR01'
     C*********            CALL 'W0050R'
     C*********  *INKC     PARM           @@KC    1
     C*********  *INKL     PARM           @@KL    1
     C*********            PARM '2'       @@CALL  1
     C*********            PARM           @@RRN   90
     C*********            PARM DBRRN     @@RRN2  90
     C*********            PARM           @@QTE   1
     C*********            PARM 1         @@VIEW  10
     C*********            PARM           @@RRN3  90
     C*********
     C*********  *INKC     CABEQ*ON       TERM
     C*********
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********  *INKL     IFEQ *ON
     C*********  @@RRN     OREQ *ZEROS
     C*********            MOVE '1'       *IN44
     C*********            GOTO STRT
     C*********            ELSE
     C*********
     C*********  @@RRN     CHAINWMEMP                60
     C*********
     C*********            MOVE EMEMP#    SCEMP#
     C*********            MOVE EMDIV     XXDIV
     C*********            MOVE '1'       *IN45
     C*********            GOTO SCR01T
     C*********            END
     C*********
     C*********            END
     C*********            END
      *****BEGIN CHG **** 05/15/98 ************************************
     C*********
     C**********  COMMAND 09 = FUND YEAR SEARCH
     C*********
     C********** *INKI     IFEQ '1'
      *********
     C***************************************
     C***FUND*YEAR SEARCH                   *
     C***************************************
      *********
     C*********  HFLD      IFEQ 'SCFYR1'
     C*********  HFLD      OREQ 'SCFYR2'
     C*********  HFLD      OREQ 'SCFYR3'
      *****END*  CHQ **** 05/15/98 ************************************
     C*********  Q2RECF    IFEQ 'SCR01'
     C*********  SCFYR1    IFEQ '?  '
     C*********  SCFYR2    OREQ '?  '
     C*********  SCFYR3    OREQ '?  '
     C*********  HFLD      OREQ 'SCFYR1'
     C*********  HFLD      OREQ 'SCFYR2'
     C*********  HFLD      OREQ 'SCFYR3'
     C*********
     C*********  SCFYR1    COMP '?  '                    61
     C*********  SCFYR2    COMP '?  '                    62
     C*********  SCFYR3    COMP '?  '                    63
     C*********  HFLD      COMP 'SCFYR1'                 61
     C*********  HFLD      COMP 'SCFYR2'                 62
     C*********  HFLD      COMP 'SCFYR3'                 63
     C*********
     C*********            CALL 'W0011R'
     C*********            PARM *INKC     @@KC    1
     C*********            PARM *INKL     @@KL    1
     C*********            PARM '2'       @@CALL  1
     C*********            PARM DBRRN     @@RRN   90
     C*********            PARM *ZEROS    @@YR    30
     C*********
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********  *INKC     CABEQ*ON       TERM
     C*********
     C*********  *INKL     IFEQ *ON
     C*********  @@YR      OREQ *ZERO
     C***61****            MOVE '1'       *IN41
     C***62****            MOVE '1'       *IN42
     C***63****            MOVE '1'       *IN43
     C***61****            MOVE *BLANKS   SCFYR1
     C***62****            MOVE *BLANKS   SCFYR2
     C***63****            MOVE *BLANKS   SCFYR3
     C*********            GOTO STRT
     C*********            ELSE
     C*********
     C***61****            MOVE @@YR      SCFYR1
     C***62****            MOVE @@YR      SCFYR2
     C***63****            MOVE @@YR      SCFYR3
     C*********
     C***61****            MOVE '1'       *IN42
     C***62****            MOVE '1'       *IN43
     C***63****            MOVE '1'       *IN44
     C*********            GOTO SCR01T
     C*********
     C*********            ENDIF
     C*********            END
     C*********            END
     C*********
     C*********            GOTO STRT
     C*********            END
      *****BEGIN ADD **** 05/15/98 ************************************
      *********
     C*********            MOVE *ON       *IN90
     C*********            GOTO STRT
      *********
     C*********            ENDIF                           IFEQ *INKJ
      *****END*  ADD **** 05/15/98 ************************************
      *********
      ***** END DEL ******* 09/18/02 **********************************
     C*
     C*  PROCESS HELP REQUESTS
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
     C*  PROCESS CHANGE OF USER PARAMETERS
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
     C                   GOTO      STRT
     C                   END
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*  PROCESS REQUEST SCREEN 1
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR IS VALID FOR THE FUND
     C*
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********  SCFYR1    IFNE *BLANKS
     C     SCFYR1        IFNE      *ZERO
      ***** END CHG ******* 09/18/02 **********************************
     C                   MOVE      SCFYR1        XXFYR
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* VALIDATE THE INCEPTION MM/YY
     C*
      ***** BEG DEL ******* 09/18/02 **********************************
     C*********  SCMMYY    IFNE 0
      *********
     C*********            MOVE XXSCMM    XXMM
     C*********            MOVE XXSCYY    XXYY
     C*********            Z-ADD1         XXDD
     C*********            Z-ADD19        XXCN
     C*********
     C*********            Z-ADDXXDATE    SRDTI
      *****BEGIN CHG **** 06/30/98 ************************************
     C*******************  CALL 'SRCVTDAT'DATCHK
     C*********            CALL 'SRCVTMY' DATCHK
      *****END*  CHG **** 06/30/98 ************************************
     C*********            MOVELSRDTO     XXDATE
     C*********
     C*********  SRERR     IFEQ '1'
     C*********            MOVE *ON       *IN32
     C*********
     C*********            GOTO STRT
     C*********
     C*********            ENDIF                           SRERR IFEQ
     C*********
     C*********            ENDIF                           SCMMYY IFNE
      ***** END DEL ******* 09/18/02 **********************************
      ***** BEG ADD ******* 09/18/02 **********************************
      *
     C     SCMMYY        IFNE      0
      *
     C                   Z-ADD     SCMMYY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCMMYY
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
     C                   ENDIF                                                  SCMMYY IFNE
      *
      ***** END ADD ******* 09/18/02 **********************************
     C*
     C*  VALIDATE THE FUND YEAR IS VALID FOR THE FUND
     C*
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********  SCFYR2    IFNE *BLANKS
     C     SCFYR2        IFNE      *ZERO
      ***** END CHG ******* 09/18/02 **********************************
     C                   MOVE      SCFYR2        XXFYR
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE THE FUND YEAR IS VALID FOR THE FUND
     C*
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********  SCFYR3    IFNE *BLANKS
     C     SCFYR3        IFNE      *ZERO
      ***** END CHG ******* 09/18/02 **********************************
     C                   MOVE      SCFYR3        XXFYR
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  IF NOT ZERO, EMPLOYER MUST BE VALID
     C*
     C     SCEMP#        IFNE      *ZEROS
     C     EMKEY         CHAIN     PEM02                              27
     C     *IN27         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE EMPLOYER DETAIL EXISTS
     C*
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********  SCFYR1    IFNE *BLANKS
     C     SCFYR1        IFNE      *ZERO
      ***** END CHG ******* 09/18/02 **********************************
     C     SCEMP#        ANDNE     *ZEROS
     C                   MOVE      SCFYR1        XXFYR
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE EMPLOYER DETAIL EXISTS
     C*
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********  SCFYR2    IFNE *BLANKS
     C     SCFYR2        IFNE      *ZERO
      ***** END CHG ******* 09/18/02 **********************************
     C     SCEMP#        ANDNE     *ZEROS
     C                   MOVE      SCFYR2        XXFYR
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE EMPLOYER DETAIL EXISTS
     C*
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********  SCFYR3    IFNE *BLANKS
     C     SCFYR3        IFNE      *ZERO
      ***** END CHG ******* 09/18/02 **********************************
     C     SCEMP#        ANDNE     *ZEROS
     C                   MOVE      SCFYR3        XXFYR
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  AT LEAST ONE FUND YEAR MUST BE SPECIFIED
     C*
      ***** BEG CHG ******* 09/18/02 **********************************
     C*********  SCFYR1    IFEQ *BLANKS
     C*********  SCFYR2    ANDEQ*BLANKS
     C*********  SCFYR3    ANDEQ*BLANKS
     C     SCFYR1        IFEQ      *ZERO
     C     SCFYR2        ANDEQ     *ZERO
     C     SCFYR3        ANDEQ     *ZERO
      ***** END CHG ******* 09/18/02 **********************************
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED, CALL THE CLP TO SUBMIT THE REPORT
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPFND         QQFND             3
     C                   MOVE      SCFYR1        QQFYR1            3
     C                   MOVE      SCFYR2        QQFYR2            3
     C                   MOVE      SCFYR3        QQFYR3            3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCEFDT        QQEFDT            8
     C                   MOVE      UPOUTQ        QQOUTQ           10
      **** BEGIN CHG **** 06/30/98 ************************************
     C******************** MOVE SCMMYY    QQMMYY  4
     C                   MOVE      SCMMYY        QQMMYY            6
      **** END   CHG **** 06/30/98 ************************************
     C                   CALL      'W8400C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR1
     C                   PARM                    QQFYR2
     C                   PARM                    QQFYR3
     C                   PARM                    QQEMP#
     C                   PARM                    QQEFDT
     C                   PARM                    QQMMYY
     C                   PARM                    QQOUTQ
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      ***** BEG ADD ******* 09/18/02 **********************************
      *
      *==============================================================
      * KJDO - Searches
      *==============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      *  FYR 1
      *
     C     HFLD          WHENEQ    'SCFYR1'
      *
     C                   CALL      'W0011R'
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZEROS        ##YR              3 0
      *
     C     ##YR          IFEQ      *ZERO
     C                   MOVE      '1'           *IN41
      *
     C                   ELSE
      *
     C                   MOVE      '1'           *IN42
     C                   Z-ADD     ##YR          SCFYR1
     C                   ENDIF
      *
      *  FYR 2
      *
     C     HFLD          WHENEQ    'SCFYR2'
      *
     C                   CALL      'W0011R'
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZEROS        ##YR              3 0
      *
     C     ##YR          IFEQ      *ZERO
     C                   MOVE      '1'           *IN42
      *
     C                   ELSE
      *
     C                   MOVE      '1'           *IN43
     C                   Z-ADD     ##YR          SCFYR2
     C                   ENDIF
      *
      *  FYR 3
      *
     C     HFLD          WHENEQ    'SCFYR3'
      *
     C                   CALL      'W0011R'
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZEROS        ##YR              3 0
      *
     C     ##YR          IFEQ      *ZERO
     C                   MOVE      '1'           *IN43
      *
     C                   ELSE
      *
     C                   MOVE      '1'           *IN44
     C                   Z-ADD     ##YR          SCFYR3
     C                   ENDIF
      *
      *  EMPLOYER
      *
     C     HFLD          WHENEQ    'SCEMP#'
      *
     C                   CALL      'W0050R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      *BLANK        ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C                   PARM      *ZERO         ##RRN3            9 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   MOVE      '1'           *IN44
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     WMEMP                              60
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      '1'           *IN45
     C                   END
      *
      *  Rating Effective Date
      *
     C     HFLD          WHENEQ    'SCEFDT'
     C                   SETON                                        71
      *
      *  Policy Inception (mm/ccyy)
      *
     C     HFLD          WHENEQ    'SCMMYY'
     C                   SETON                                        72
      *
      *
     C                   ENDSL
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSR                                                  KJDO
      *
      *
      ***** END ADD ******* 09/18/02 **********************************
