     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W8613R -  DEMAND DUP PAYMENT REPORT -  Prompt Pgm
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     Prompt program for Duplicate Payment Report on          **
     H**     demand                                                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     Menu                                                    **
     H**   CALLS TO:                                                 **
     H**     W8613C                                                  **
     H**   PROGRAMMER: C: 30                                         **
     H**   CREATED  12/26/95   Project: WC457                        **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H*  05/14/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   INDICATOR            FUNCTION DESCRIPTION                   *
     H*                                                               *
     H*      20           EDIT FOR AMOUNT 2 WITHOUT AMOUNT 1          *
     H*      21           AMOUNT 2 GREATER THAN AMOUNT 1              *
     H*      22           INVALID TO DATE                             *
     H*      23           EDIT FOR BOTH AMOUNTS                       *
     H*      24           INVALID FROM DATE                           *
     H*      25           INVALID GROUP TYPE                          *
     H*      26           INVALID Duplicate Selection                 *
     H*      27           DATES OUT OF SEQUENCE                       *
     H*      28           INVALID Type of Output Code                 *
     H*      33           Invalid Reserve Code # 1                    *
     H*      34           Invalid Reserve Code # 2                    *
     H*      35           Invalid Reserve Code # 3                    *
     H*      36           Invalid Reserve Code # 4                    *
     H*      60-62        GENERAL CHAINING                            *
     H*****************************************************************
     FW8613F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *** Beg Add ***  11/25/13  ******************************************************
     FWMCXL     IF   E           K DISK
      *** End Add ***  11/25/13  ***********************************************HEADING

     D***  BEGIN ADD  ***  05/14/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/14/01  *********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Beg Add ***  11/25/13  ******************************************************
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      *** End Add ***  11/25/13  ***********************************************HEADING

     C*****************************************************************
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
     C                   KFLD                    UPFND
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C     *LIKE         DEFINE    FNDT          XXFRDT
     C     *LIKE         DEFINE    FNDT          XXTODT
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
     C***************************************************************
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C                   MOVE      *BLANKS       XXAMT1           11
     C                   MOVE      *BLANKS       XXAMT2           11
     C                   Z-ADD     *ZEROS        X                 3 0
     C                   Z-ADD     *ZEROS        Y                 3 0
     C                   Z-ADD     *ZEROS        LENGTH           15 5
     C                   Z-ADD     *ZEROS        XXFRDT
     C                   Z-ADD     *ZEROS        XXTODT
     C                   MOVE      *BLANKS       XXCO#             3
     C                   MOVE      *BLANKS       XXFND             3
     C                   MOVE      *BLANKS       XXDTE1            8
     C                   MOVE      *BLANKS       XXDTE2            8
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   MOVE      *BLANKS       XXCMD            36
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
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
     C*  GET THE FUND NAME
     C*
     C                   MOVE      UPFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       scLEGL = *Blanks;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                scLEGL = CXlgrh;
             Else;
                scLEGL = 'Legal';
             EndIf;
          Endif;
       Else;
          scLEGL = 'Legal';
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F1 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
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
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT01
     C*
     C                   END
     C                   END
     C*
     C******************************
     C*   PROCESS REQUEST SCREEN
     C******************************
     C*
     C     SCR01T        TAG
     C*
     C*  Edit the FROM Date
     C*
     C     SCFRDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  Edit the TO Date
     C*
     C     SCTODT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  Compare the FROM Date with the TO Date.
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  Edit for None or Both Amounts
     C*
     C     SCAMT1        IFGT      *ZERO
     C     SCAMT2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  Edit for SCAMT2 > SCAMT1
     C*
     C     SCAMT2        IFLT      SCAMT1
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  Edit for Duplication Selection 'Y' = EXACT  'N' = POSSIBLE
     C*
     C     SCTDUP        IFNE      'Y'
     C     SCTDUP        ANDNE     'N'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  Edit for Group Type
     C*
     C     SCTYP         IFNE      *BLANK
     C     SCTYP         ANDNE     'G'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  Edit for Type of Output.
     C*
     C     SCOTYP        IFNE      'F'
     C     SCOTYP        ANDNE     'P'
     C     SCOTYP        ANDNE     'B'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  Edit Reserve Types
     C*
     C     SCRST1        IFNE      'M'
     C     SCRST1        ANDNE     'C'
     C     SCRST1        ANDNE     'E'
     C     SCRST1        ANDNE     'L'
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCRST2        IFNE      *BLANK
     C     SCRST2        ANDNE     'M'
     C     SCRST2        ANDNE     'C'
     C     SCRST2        ANDNE     'E'
     C     SCRST2        ANDNE     'L'
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCRST3        IFNE      *BLANK
     C     SCRST3        ANDNE     'M'
     C     SCRST3        ANDNE     'C'
     C     SCRST3        ANDNE     'E'
     C     SCRST3        ANDNE     'L'
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCRST4        IFNE      *BLANK
     C     SCRST4        ANDNE     'M'
     C     SCRST4        ANDNE     'C'
     C     SCRST4        ANDNE     'E'
     C     SCRST4        ANDNE     'L'
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCFRDT        XXDTE1
     C                   MOVE      SCTODT        XXDTE2
     C                   MOVE      SCAMT1        XXAMT1
     C                   MOVE      SCAMT2        XXAMT2
     C                   CALL      'W8613C'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXDTE1
     C                   PARM                    XXDTE2
     C                   PARM                    UPOUTQ
     C                   PARM                    SCOTYP
     C                   PARM                    SCTDUP
     C                   PARM                    XXAMT1
     C                   PARM                    XXAMT2
     C                   PARM                    SCTYP
     C                   PARM                    SCRST1
     C                   PARM                    SCRST2
     C                   PARM                    SCRST3
     C                   PARM                    SCRST4
     C                   PARM      'D'           QQRUN             1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
