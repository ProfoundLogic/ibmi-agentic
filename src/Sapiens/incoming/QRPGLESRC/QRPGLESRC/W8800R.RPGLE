     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8800R - PROMPT FOR SPECIFIC PARTIAL FILING
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  09/09/02 C:15  Support: WC065/34060                          *
     H*                 Chgd the length of X@FYR to 3 digits.         *
     H*                                                               *
     H*  12/08/99 C:118 PROJECT: WC577                                *
     H*                 V3 DISPLAY FILE MODIFICATIONS.                *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  02/20/95 C:30 PROJECT WC397                                  *
     H*                Write DARTS remark when a claim goes to        *
     H*                specific reinsurance.                          *
     H*                                                               *
     H*  07/07/92  JEI  PROJECT: WC157                                *
     H*                 CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR     *
     H*                 'BASE' FILES (XXCASE) - CASE NUMBER REMAINS   *
     H*                 14 ALPHA FOR REINSURANCE FILES                *
     H*                                                               *
     H*  04/03/92  LPK  PROJECT: MD027                                *
     H*                 CHANGED CASE NUMBER FROM 7 NUMERIC TO 14      *
     H*                 ALPHA FIELD.                                  *
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
     FW8800F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F*
     F*** BEGIN ADD *** 01/28/98 ***********************************************
     F                                     INFDS(DBFDS)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     F*** END   ADD *** 01/28/98 ***********************************************
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FSTCTL1    IF   E           K DISK

     D*
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END   ADD *** 01/28/98 ***********************************************
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
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Generate Specific Pa-
     D                                     rtial Filing')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    SCCASE
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND             3 0
     C*
     C*  DARTS REMARK FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
     C*
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C*
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           Z-ADDUPFYR     SCFYR
     C*
     C     UPFYR         IFNE      *ZEROS
     C                   Z-ADD     UPFYR         SCFYR
     C                   ENDIF                                                  UPFYR<>0
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND NAME
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
     C*** BEGIN DEL *** 01/28/98 **************************************
     C**********           TIME           DSDTTM
     C*** END   DEL *** 01/28/98 **************************************
     C*
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
     C**********           MOVE *BLANK    SCRMCD           Do Not Initialize
     C*
     C*** END   CHG *** 01/28/98 **************************************
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
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C**********
     C**F10=SEARCH
     C**********
     C********** *INKJ     IFEQ '1'
     C********** HFLD      IFEQ 'SCCASE'
     C**********
     C**********           CALL 'W0570R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCYR
     C**********           PARM           SCCASE
     C**********           PARM           XXSS#   9
     C**********           PARM           XXEMP#
     C**********           PARM           XXDIV
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  SCREEN ONE
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  SEARCHES
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C**      ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
     C     HFLD          COMP      'SCRNDT'                               34    ERR MSG 1058
     C*
     C*  F10 - SEARCH - POLICY YEAR OR CASE
     C*
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZEROS        @@RRN             9 0
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
     C                   MOVE      *ON           *IN32                          SCRNDT POSITION
     C                   ENDIF                                                  *INKL=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C     HFLD          COMP      'SCFYR'                                30    SCFYR POSITION
     C     HFLD          COMP      'SCCASE'                               31    SCCASE POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C*  F10 - SEARCH - DARTS REMARK CODE
     C*
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C                   CALL      'SC580R'
     C                   PARM      *BLANKS       QQRMCD            5
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      '*'
     C     QQRMCD        ANDNE     *BLANKS
     C                   MOVEL     QQRMCD        SCRMCD
     C                   ENDIF                                                  QQRTC=*        F
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRMCD        OREQ      *BLANKS
     C                   MOVE      *ON           *IN33                          SCRMCD POSITION
     C                   ENDIF                                                  QQRTC=B        F
     C*
     C                   ENDIF                                                  HFLD=SCRMCD
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
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C*                                                                8
     C*  GET THE FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  FUND YEAR MUST BE VALID
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*  RUN DATE MUST BE VALID
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE CLAIM MASTER RECORD TO VALIDATE THE CASE NUMBER
     C*
     C     SCCASE        IFNE      *ZEROS
     C     CMKEY         CHAIN     PCM01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C*
     C*  Verify Valid DARTS Remark Code
     C*
     C                   MOVEL     SCRMCD        XXRMCD
     C     CTKY          CHAIN     STCTL1                             23
     C     *IN23         CABEQ     '1'           STRT
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCFND         X@FND
     C                   MOVE      SCFYR         X@FYR
     C                   MOVE      SCRNDT        X@RNDT
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C                   CALL      'W8800C'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C*** Begin chg *** 09/09/02 **************************************
     C******************** PARM           X@FYR   2
     C                   PARM                    X@FYR             3
     C*** End chg ***** 09/09/02 **************************************
     C                   PARM                    X@RNDT            8
     C                   PARM                    X@CASE            7
     C                   PARM                    UPOUTQ
     C                   PARM                    XXRMCD
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
