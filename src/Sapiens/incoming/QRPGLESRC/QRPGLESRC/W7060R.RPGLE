     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7060R - PROMPT FOR PAY HISTORY REPORT
     H*****************************************************************    **
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/29/14  C:062 Support WC114 85413                           *    *
     H*                 Restore mode parameter in W0070R.             *
     H*                                                               *
     H* 03/04/13  C:277 RedMine# 2496                                 *    *
     H*                 Pre-filled values for Group, Year, Case#,     *
     H*                 Employer & Division.                          *
     H*                                                               *
     H* 12/08/12  C:277 Support:WC81108                               *    *
     H*           Compile Only - In DSPF, On field SCTCAS (*In48)     *    *
     H*           Change Msg WMESGG/WCR0502 from CMESGG/CCR0504       *
     H*                                                               *
     H* 08/24/10  C:062 Support:WC1910                                *    *
     H*           Compile Only                                        *    *
     H*                                                               *
     H* 08/12/10  C:246 Support:73811                                 *    *
     H*           Remove default value from TOCASE field.             *    *
     H*                                                               *
     H* 03/11/10  C:189 Project:  139WNS                              *
     H*                Pay History Sort By Check# / Transaction Date  *
     H*                                                               *
     H* 03/01/02 C:53  SUPPORT # 31446                                *
     H*                Corrected display of fund on option 13         *
     H*                                                               *
     H* 08/13/01 C:61  PROJECT WC577                                  *
     H*                Allow selection of 5 Reserve Codes and         *
     H*                4 Reserve Types.                               *
     H*                                                               *
     H*  05/15/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* 10/23/97 C:58   New for Version 3.
     H*                                                                    *
     H***********************************************************************
     H* INDICATORS:
     H* 35    WCR1179: Either both FYR/CASE have to be blank or both entered
     H* 36    Employer Number required
     H* 37    Employer Number is Invalid for Fund
     H* 38    Employer Nunber is Invalid for Fund Year
     H* 39    Employer Number is Invalid for Claim Number entered.
     H* 40    Invalid Table Value
     H* 45    Record not found for information                         tered
     H* 46    Invalid Range Entered
     H***********************************************************************
      *                                                                    *
     FW7060F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
     FWDELL     IF   E           K DISK

      ***  BEG ADD  ***  03/04/13  *********************************
     d/copy wPrEmpLr
      ***  END ADD  ***  03/04/13  *********************************
     D*
     D***  BEGIN ADD  ***  05/15/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/15/01  *********************************
      ***  BEG ADD  ***  03/04/13  *********************************
     d claimRrn        s              9  0
     d emRRN           s              9  0
      ***  END ADD  ***  03/04/13  *********************************
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
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  FUND YEAR KEY LIST.
     C*
      ***** BEG CHG ******* 03/04/13 **********************************
     C*****F1KEY         KLIST
     C*****              KFLD                    UPCO#
     C*****              KFLD                    UPFND
     C*****              KFLD                    XXFYR
     C*****
     C*****CLAIM MASTER FILE
     C*****
     C*****CMKEY         KLIST
     C*****              KFLD                    UPCO#
     C*****              KFLD                    UPFND
     C*****              KFLD                    XXFYR             3 0
     C*****              KFLD                    SCCASE            7 0

     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCYR
     C                   KFLD                    SCCASE            7 0
      ***** END CHG ******* 03/04/13 **********************************
     C*
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
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
     C*
     C*  Employer Master
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  Employer Detail
     C*
     C     ELKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR             3 0
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      ***** BEG ADD ******* 03/04/13 **********************************
     C                   PARM                    QQRRN             9
      ***** END ADD ******* 03/04/13 **********************************
     C*
     C* EMPLOYER SEARCH
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'L'           ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      *ZERO         ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
      ***** BEG ADD ******* 08/13/01 **********************************
      *
     C     RCPL          PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
      ***** END ADD ******* 08/13/01 **********************************
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
     C     *LIKE         DEFINE    SCCASE        SVCASE
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     STRT01        TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   Z-ADD     UPFYR         SCYR
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      ***  BEG ADD  ***  03/04/13  *********************************
      /Free
       If %Parms >= 3;
         claimRrn=%Dec(qqRrn:9:0);
         Chain(n) claimRrn pcm02;
         scfnd=CMfnd;
         scyr=CMfYr;
       Endif;
      /End-Free
      ***  End Add  ***  03/04/13  *********************************
     C*
     C*  GET THE FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCCASE
     C*** BEG CHG ******** 08/12/10 ********************************************
     C**                   Z-ADD999999999 SCTCAS
     C                   Z-ADD     0             SCTCAS
     C*** END CHG ******** 08/12/10 ********************************************
      ***  BEG ADD  ***  03/04/13  *********************************
      /Free

       If %Parms >= 3;
         sccase=CMcase;
         emRRN = GetEmpRRN(CMemp#:CMdiv:CMfnd:Cmco#);
         Chain(n) emRRN pem0p;
         If %Found(wmemp);
            scemp# = EMemp#;
            scdiv = EMdiv;
         Endif;
       Endif;

      /End-Free
      ***  END ADD  ***  03/04/13  *********************************
     C*** BEG ADD ******** 03/11/10 ********************************************
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '139'
     C                   MOVE      'C'           SCSORT
     C                   ELSE
     C                   MOVE      'T'           SCSORT
     C                   ENDIF
     C*
     C*** END ADD ******** 03/11/10 ********************************************
     C*** BEG ADD ******** 05/15/01 ********************************************
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
     C*** END ADD ******** 05/15/01 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
      ***** BEG ADD ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
     C   KJ              WRITE     SCR99
     C***** BEG ADD ******* 03/01/02 **********************************
     C   KM              WRITE     SCR99
     C***** END ADD ******* 03/01/02 **********************************
      ***** END ADD ******* 08/13/01 **********************************
     C                   EXFMT     SCR01
      ***** BEG ADD ******* 08/13/01 **********************************
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
      ***** END ADD ******* 08/13/01 **********************************
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=Exit ======================================
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F10=Search =====================================
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*     Screen 1 searches
     C     Q2RECF        IFEQ      'SCR01'                                      Screen
     C     HREC          ANDEQ     'SCR01'                                       cursor
     C*
     C*     Claim master search
     C     HFLD          IFEQ      'SCYR'                                        Cursor
     C     HFLD          OREQ      'SCCASE'                                       loc-
     C     HFLD          OREQ      'SCTCAS'                                         ation
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C*** BEGIN ADD *** 04/29/14 **********************************************
     C                   PARM                    @@MODE            1
     C*** END   ADD *** 04/29/14 **********************************************
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN1        @@RRN2            9 0
     C                   PARM      SCYR          @@FYR             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     HFLD          IFEQ      'SCYR'
     C                   MOVE      *ON           *IN51
     C                   ENDIF                                                  e FLD = YR
     C     HFLD          IFEQ      'SCCASE'
     C                   MOVE      *ON           *IN52
     C                   ENDIF                                                  e FLD = CASE
     C     HFLD          IFEQ      'SCTCAS'
     C                   MOVE      *ON           *IN53
     C                   ENDIF                                                  e FLD = TCAS
     C                   GOTO      STRT
     C                   ENDIF                                                  e INKL/RRN
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C                   MOVE      CMFYR         SCYR
     C     HFLD          IFEQ      'SCCASE'
     C     HFLD          OREQ      'SCYR'
     C                   MOVE      CMCASE        SCCASE
     C                   MOVE      *ON           *IN53
     C                   ELSE
     C                   MOVE      CMCASE        SCTCAS
     C                   MOVE      *ON           *IN54
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  e FLD = CASE
     C*
     C*   Field EMP# = Employer search
     C*
     C     HFLD          IFEQ      'SCEMP#'                                      Cursor
     C     HFLD          OREQ      'SCDIV'                                         field
     C                   Z-ADD     *ZEROS        ##RRN3
     C                   CALL      'W0050R'      ACDPL1                 99
     C*
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT                     54
     C     ##RRN         CABEQ     *ZEROS        STRT                     54
     C*
     C     ##RRN         CHAIN     PEM0P                              60
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  e FLD = EMP#
     C*
     C*   Field TRCD = Payment code table search
     C*
     C     HFLD          IFEQ      'SCTRCD'                                      Cursor
      ***** BEG DEL ******* 08/13/01 **********************************
     C*********  HFLD      OREQ 'SCTYPE'                    Cursor
      ***** END DEL ******* 08/13/01 **********************************
     C*                                                       field
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2141R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   MOVEL     SCKEY         SCTRCD
     C*
     C     *INKC         CABEQ     '1'           TERM
     C*
     C     SCKEY         CABEQ     *BLANK        STRT                     26
     C     *INKL         CABEQ     *ON           STRT                     26
     C*
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF                                                  e FLD = TRCD
      ***** BEG DEL ******* 08/13/01 **********************************
     C*********
     C*****Field SCRECD - reserve code table search
     C*********
     C*********  HFLD      IFEQ 'SCRECD'                    Cursor
     C*********  HFLD      OREQ 'SCRETP'                      field
     C*********            MOVE *BLANK    SCKEY  10
     C*********            CALL 'W2136R'
     C*********            PARM '2'       QQCALL
     C*********  *INKC     PARM           QQKC    1
     C*********  *INKL     PARM           QQKL    1
     C*********            PARM           SCKEY
     C*********
     C*********  *INKC     CABEQ'1'       TERM
     C*********  *INKL     CABEQ'1'       STRT           33
     C*********  SCKEY     CABEQ*BLANK    STRT           33
     C*********
     C*********            MOVELSCKEY     SCRECD
     C*********            MOVE *ON       *IN28
     C*********            GOTO STRT
     C*********            ENDIF                           e FLD = RECD
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
      *
      *  Reserve Code Searches
      *
     C     HFLD          IFEQ      'SCRCD1'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        71
     C                   ELSE
     C                   MOVEL     ##KEY         SCRCD1
     C                   SETON                                        72
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCRCD2'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        72
     C                   ELSE
     C                   MOVEL     ##KEY         SCRCD2
     C                   SETON                                        73
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCRCD3'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        73
     C                   ELSE
     C                   MOVEL     ##KEY         SCRCD3
     C                   SETON                                        74
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCRCD4'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        74
     C                   ELSE
     C                   MOVEL     ##KEY         SCRCD4
     C                   SETON                                        75
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCRCD5'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        75
     C                   ELSE
     C                   MOVEL     ##KEY         SCRCD5
     C                   SETON                                        76
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Reserve Type Searches
      *
     C     HFLD          IFEQ      'SCRTP1'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        76
     C                   ELSE
     C                   MOVEL     ##KEY         SCRTP1
     C                   SETON                                        77
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCRTP2'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        77
     C                   ELSE
     C                   MOVEL     ##KEY         SCRTP2
     C                   SETON                                        78
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCRTP3'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        78
     C                   ELSE
     C                   MOVEL     ##KEY         SCRTP3
     C                   SETON                                        79
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCRTP4'
     C                   CALL      'W2136R'      RCPL
      *
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        79
     C                   ELSE
     C                   MOVEL     ##KEY         SCRTP4
     C                   SETON                                        55
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
      *
      ***** END ADD ******* 08/13/01 **********************************
     C*
     C*
     C*   Field BR2 = Payment value 2 table search
     C*
     C     HFLD          IFEQ      'SCBR2'                                       Cursor
     C*                                                       field
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2146R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   MOVEL     SCKEY         SCBR2
     C*
     C     *INKC         CABEQ     '1'           TERM
     C*
     C     *INKL         CABEQ     '1'           STRT                     55
     C     SCKEY         CABEQ     *BLANK        STRT                     55
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** BEG CHG ******** 03/11/10 ********************************************
     C****************     MOVE *ON       *IN47
     C                   MOVE      *ON           *IN62
     C*** END CHG ******** 03/11/10 ********************************************
     C                   GOTO      STRT
     C                   ENDIF                                                  e FLD = BR2
     C                   MOVE      *ON           *IN59                           CURSOR LOST
     C*
     C                   ENDIF                                                  e REC=SCR01
     C                   GOTO      STRT
     C                   ENDIF                                                  e INKJ ON
     C*
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
     C     SCOUT         CABEQ     *BLANK        STRT01                   47
     C     SCYR          CABEQ     *ZERO         STRT01                   51
     C     SCCASE        CABEQ     *ZERO         STRT01                   52
     C     SCTCAS        CABEQ     *ZERO         STRT01                   53
     C     SCEMP#        CABEQ     *ZERO         STRT01                   54
     C     SCDIV         CABEQ     *ZERO         STRT01                   54
     C     SCFRDT        CABEQ     *ZERO         STRT01                   31
     C     SCTRCD        CABEQ     *BLANK        STRT01                   26
     C     SCTYPE        CABEQ     *BLANK        STRT01                   27
      ***** BEG DEL ******* 08/13/01 **********************************
     C*********  SCRECD    CABEQ*BLANK    STRT01         33
     C*********  SCRETP    CABEQ*BLANK    STRT01         28
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
     C     SCRCD1        CABEQ     *BLANK        STRT01                   71
     C     SCRTP1        CABEQ     *BLANK        STRT01                   76
      ***** END ADD ******* 08/13/01 **********************************
     C     SCBR2         CABEQ     *BLANK        STRT01                   55
     C                   GOTO      STRT01
     C                   END                                                    e INKM
     C*
     C                   END                                                    e IN11
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* Validate Output flag
     C*
     C*
     C     SCOUT         IFNE      'P'
     C     SCOUT         ANDNE     'F'
     C     SCOUT         ANDNE     'B'
     C                   MOVE      *ON           *IN44
     C                   MOVE      *ON           *IN47
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C*  GET THE CLAIM MASTER RECORD TO VALIDATE THE CASE NUMBER
     C*
     C*
     C     SCYR          IFNE      *ZERO
     C*
     C* Validate Fund Year for Fund
     C*
     C                   Z-ADD     SCYR          XXFYR
     C     F1KEY         CHAIN     PF101                              23
     C     *IN23         CABEQ     '1'           STRT
     C*
     C     SCCASE        IFEQ      *ZERO
     C     SCEMP#        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ELSE                                                   SCYR Zero
     C*
     C     SCCASE        IFGT      *ZERO
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCCASE        IFGT      *ZERO
     C*
     C* Validate Case#
     C*
     C     CMKEY         CHAIN     PCM01                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C* Check 'TO' case for valid number
     C*
     C     SCTCAS        IFGT      *ZEROS
     C     SCTCAS        ANDLT     9999999
     C                   Z-ADD     SCCASE        SVCASE
     C                   Z-ADD     SCTCAS        SCCASE
     C     CMKEY         CHAIN     PCM01                              45
     C                   Z-ADD     SVCASE        SCCASE
     C     *IN45         CABEQ     '1'           STRT
     C                   END
     C                   MOVE      '0'           *IN46
     C     SCTCAS        IFNE      *ZEROS
     C     SCTCAS        ANDLT     SCCASE
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C*** BEG CHG ******** 08/12/10 ********************************************
     C                   MOVE      '0'           *IN48
     C     SCTCAS        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C*** END CHG ******** 08/12/10 ********************************************
     C*
     C*
     C*  Validate Employer # equal to Claim Employer #
     C*
     C*          SCEMP#    IFGT *ZERO
     C*          SCEMP#    IFNE CMEMP#
     C*                    MOVE *ON       *IN39
     C*                    GOTO STRT
     C*                    ENDIF
     C*                    ELSE
     C*                    Z-ADDCMEMP#    SCEMP#
     C*                    Z-ADDCMDIV     SCDIV
     C*                    WRITESCR01
     C*                    ENDIF
     C                   ENDIF
     C*
     C*  Edit Employer Number (Optional)
     C*
     C     SCEMP#        IFGT      *ZERO
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCYR          IFGT      *ZERO
     C*
     C                   Z-ADD     SCYR          XXFYR
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  EDIT THE FROM DATE
     C*
     C     SCFRDT        IFNE      *ZERO
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT THE TO DATE
     C*
     C     SCTODT        IFNE      *ZERO
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  COMPARE THE FROM DATE WITH THE TO DATE.
     C*
     C     SCFRDT        IFNE      *ZERO
     C     SCTODT        ANDNE     *ZERO
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  TEST TRANSACTION CODE VALUE ENTERED.
     C*
     C     SCTRCD        IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCTRCD        XXKEY
     C     TBKEY         CHAIN     PPC01                              60
     C  N60PCSTAT        COMP      'I'                                    60
     C*
     C     *IN60         IFEQ      '1'
     C                   SETON                                        2126
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  TEST TRANSACTION TYPE VALUE ENTERED.
     C*
     C     SCTYPE        IFNE      *BLANK
     C     SCTYPE        ANDNE     'P'
     C     SCTYPE        ANDNE     'M'
     C     SCTYPE        ANDNE     'V'
     C     SCTYPE        ANDNE     'R'
     C     SCTYPE        ANDNE     'A'
     C     SCTYPE        ANDNE     'S'
     C     SCTYPE        ANDNE     'Z'
     C                   SETON                                        2227
     C                   GOTO      STRT
     C                   END
      ***** BEG DEL ******* 08/13/01 **********************************
     C**********
     C***TEST*RESERVE CODE VALUE ENTERED.
     C**********
     C********** SCRECD    IFNE *BLANK
     C**********           MOVE *BLANK    XXKEY
     C**********           MOVELSCRECD    XXKEY
     C********** TBKEY     CHAINPRT01                60
     C**N60***** RTSTAT    COMP 'I'                      60
     C**********
     C********** *IN60     IFEQ '1'
     C**********           SETON                     29
     C**********           GOTO STRT
     C**********           END
     C**********           END
     C**********
     C***TEST*TRANSACTION TYPE VALUE ENTERED.
     C**********
     C********** SCRETP    IFNE *BLANK
     C********** SCRETP    ANDNE'M'
     C********** SCRETP    ANDNE'C'
     C********** SCRETP    ANDNE'L'
     C********** SCRETP    ANDNE'E'
     C**********           SETON                     30
     C**********           GOTO STRT
     C**********           END
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
      *
      * Reserve Codes must be valid, if entered.
      *
     C     SCRCD1        IFNE      *BLANK
     C                   MOVEL(P)  SCRCD1        XXKEY
     C     TBKEY         CHAIN     PRT01                              81
     C  N81RTSTAT        COMP      'I'                                    81
     C   81              GOTO      STRT
     C                   END
      *
     C     SCRCD2        IFNE      *BLANK
     C                   MOVEL(P)  SCRCD2        XXKEY
     C     TBKEY         CHAIN     PRT01                              82
     C  N82RTSTAT        COMP      'I'                                    82
     C   82              GOTO      STRT
     C                   END
      *
     C     SCRCD3        IFNE      *BLANK
     C                   MOVEL(P)  SCRCD3        XXKEY
     C     TBKEY         CHAIN     PRT01                              83
     C  N83RTSTAT        COMP      'I'                                    83
     C   83              GOTO      STRT
     C                   END
      *
     C     SCRCD4        IFNE      *BLANK
     C                   MOVEL(P)  SCRCD4        XXKEY
     C     TBKEY         CHAIN     PRT01                              84
     C  N84RTSTAT        COMP      'I'                                    84
     C   84              GOTO      STRT
     C                   END
      *
     C     SCRCD5        IFNE      *BLANK
     C                   MOVEL(P)  SCRCD5        XXKEY
     C     TBKEY         CHAIN     PRT01                              85
     C  N85RTSTAT        COMP      'I'                                    85
     C   85              GOTO      STRT
     C                   END
      *
      * Transaction Types must be valid, if entered.
      *
     C     SCRTP1        IFNE      *BLANK
     C     SCRTP1        ANDNE     'M'
     C     SCRTP1        ANDNE     'C'
     C     SCRTP1        ANDNE     'L'
     C     SCRTP1        ANDNE     'E'
     C                   SETON                                        86
     C                   GOTO      STRT
     C                   END
      *
     C     SCRTP2        IFNE      *BLANK
     C     SCRTP2        ANDNE     'M'
     C     SCRTP2        ANDNE     'C'
     C     SCRTP2        ANDNE     'L'
     C     SCRTP2        ANDNE     'E'
     C                   SETON                                        87
     C                   GOTO      STRT
     C                   END
      *
     C     SCRTP3        IFNE      *BLANK
     C     SCRTP3        ANDNE     'M'
     C     SCRTP3        ANDNE     'C'
     C     SCRTP3        ANDNE     'L'
     C     SCRTP3        ANDNE     'E'
     C                   SETON                                        88
     C                   GOTO      STRT
     C                   END
      *
     C     SCRTP4        IFNE      *BLANK
     C     SCRTP4        ANDNE     'M'
     C     SCRTP4        ANDNE     'C'
     C     SCRTP4        ANDNE     'L'
     C     SCRTP4        ANDNE     'E'
     C                   SETON                                        89
     C                   GOTO      STRT
     C                   END
      *
      ***** END ADD ******* 08/13/01 **********************************
     C*
     C*  Edit Payment Value 1
     C*
     C     SCBR2         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKEY         CHAIN     PB201                              60
     C  N60B2STAT        COMP      'I'                                    60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*** BEG ADD ******** 03/11/10 ********************************************
     C     SCSORT        IFNE      *BLANKS
     C     SCSORT        IFNE      'C'
     C     SCSORT        ANDNE     'T'
     C                   MOVE      *ON           *IN63
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ******** 03/11/10 ********************************************
     C*   A l l     E d i t s     P a s s e d
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCFND         X@FND
     C                   MOVE      SCYR          X@FYR
     C                   MOVE      SCCASE        X@CASE
     C     SCTCAS        IFEQ      *ZERO
     C                   MOVEL     999999999     X@TCAS
     C                   ELSE
     C                   MOVE      SCTCAS        X@TCAS
     C                   END
     C                   MOVE      SCOUT         X@OUT
     C                   MOVE      SCFRDT        X@DAT1
     C                   MOVE      SCTODT        X@DAT2
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
      *
     C*** BEG ADD ******** 03/11/10 ********************************************
     C     SCSORT        IFEQ      *BLANKS
     C     CLTID         IFEQ      '139'
     C                   MOVE      'C'           SCSORT
     C                   ELSE
     C                   MOVE      'T'           SCSORT
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      SCSORT        QQSORT            1
     C*** END ADD ******** 03/11/10 ********************************************
     C                   CALL      'W7060C'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@FYR             3
     C                   PARM                    X@CASE            7
     C                   PARM                    X@OUT             1
     C                   PARM                    X@DAT1            8
     C                   PARM                    X@DAT2            8
     C                   PARM                    SCTRCD
     C                   PARM                    SCTYPE
      ***** BEG DEL ******* 08/13/01 **********************************
     C*********            PARM           SCRECD
     C*********            PARM           SCRETP
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
     C                   PARM                    SCRCD1
     C                   PARM                    SCRCD2
     C                   PARM                    SCRCD3
     C                   PARM                    SCRCD4
     C                   PARM                    SCRCD5
     C                   PARM                    SCRTP1
     C                   PARM                    SCRTP2
     C                   PARM                    SCRTP3
     C                   PARM                    SCRTP4
      ***** END ADD ******* 08/13/01 **********************************
     C                   PARM                    UPOUTQ
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    SCBR2
     C                   PARM                    X@TCAS            7
     C*** BEG ADD ******** 03/11/10 ********************************************
     C                   PARM                    QQSORT
     C*** END ADD ******** 03/11/10 ********************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
