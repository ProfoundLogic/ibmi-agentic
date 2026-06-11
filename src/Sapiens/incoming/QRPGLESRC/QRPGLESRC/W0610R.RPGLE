     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0610R - EMPLOYER A/R SUMMARY SEARCH
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM IS USED TO DISPLAY AN A/R DETAIL SUMMARY FOR A   *
     H* GIVEN DATE RANGE BY TRANSACTION CODE AND TYPE FOR AN EMPLOYER.*
     H* ONLY TRANSACTIONS WITH A POSTING DATE WITHIN THE SPECIFIED    *
     H* DATE RANGE ARE SUMMARIZED.                                    *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   ACCOUNTING PROCESSING MENU                                  *
     H*   W0051R -  WORK WITH EMPLOYER POLICY YEARS                   *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   W0550R                                                      *
     H*   W0551R                                                      *
     H*   W0006R                                                      *
     H*   M0006C                                                      *
     H*   SRDCK                                                       *
     H*   W0045R02
     H*                                                               *
     H* PROGRAMMER: C:25                                              *
     H* CREATED: 05/11/94               PROJECT: WC326                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/15/99 C:33 SEQ#: WA095/20095                              *
     H*                CORRECT PAGING.                                *
     H*                                                               *
     H*  09/29/99 C:118 PROJECT: WC939                                *
     H*                 Added 'Premium Finance Co.' in RI in display  *
     H*                 if a premium finance co. has been assigned to *
     H*                 the policy.  Added F7=Finance info which calls*
     H*                 W0045R02 WW Premium Finance Co. maintenance   *
     H*                 to display finance company details. Added     *
     H*                 WMPFP.                                        *
     H*
     H*  08/25/98 C:61 PROJECT: WC577                                 *
     H*                Changed Employer search to also return fund    *
     H*                number.                                        *
     H*                                                               *
     H*  12/05/97 C:85 PROJECT: WC577                                 *
     H*                DELETED DEFAULTS BEING MOVED TO SCFRDT, SCTODT.*
     H*                CHANGED F5 & F7 SEARCHES TO F10=SEARCH FOR     *
     H*                SCFYR, SCEMP#, & SCDIV.                        *
     H*                                                               *
     H*  11/03/97 C:85 PROJECT: WC764                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                AND ADDED DIVISION COUNT TO SCR02.             *
     H*                                                               *
     H*  06/24/97 C:62 PROJECT: WC440                                 *
     H*                PASS 'W0050R' AS PROGRAM NAME SO THAT DARTS    *
     H*                FILES WILL HAVE XXPNAM FIELD LOADED WITH THE   *
     H*                PROPER VALUE.                                  *
     H*                                                               *
     H*  07/24/96 C:59 PROJECT: WC541                                 *
     H*               INSERTED CANCEL DATE IN DISPLAY FILE.           *
     H*                                                               *
     H*  10/06/95 C:22 PROJECT: SUPPORT                               *
     H*               COMMENTED OUT RESET OF EOF INDICATOR            *
     H*                                                               *
     H*  08/09/95 C:38 PROJECT WC440                                  *
     H*                ALLOW ACCESS TO DARTS USING FUNCTION KEY 14 AT *
     H*               DETAIL SCREEN (CO,FND,EMP,DIV CHANGES).         *
     H*                                                               *
     H*  11/16/94 C:15 PROJECT: WC326                                 *
     H*                FIXED ROLLING PROBLEM.                         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   10       HELP KEY                                           *
     H*   11       VALID COMMAND KEY                                  *
     H*   12       PROGRAM NOT CALLED FROM THE MENU                   *
     H*   18       ROLL UP                                            *
     H*   19       ROLL DOWN                                          *
     H*   50       A/R DETAIL FILE CHAIN, DO LOOP CONTROLLER          *
     H*   51       A/R TRANSACTION CODE TABLE FILE READ               *
     H*   60       TEMPORARY WORK INDICATOR                           *
     H*   61       ARRAY LOAD CONTROL INDICATOR                       *
     H*                                                               *
     H*  SCR01:                                                       *
     H*   20       FISCAL YEAR LESS THAN OR EQUAL TO ZERO             *
     H*   21       NO RECORD FOR EMPLOYER/DIV                         *
     H*   22       INVALID "FROM" DATE                                *
     H*   23       NO A/R DETAIL IN THIS DATE RANGE                   *
     H*   24       INVALID "TO" DATE                                  *
     H*   59       POSITION CURSOR ON "FROM" DATE                     *
     H*                                                               *
     H*  SCR02:                                                       *
     H*   20       INVALID ROLL REQUEST                               *
     H*   41       ESTIMATED PREMIUM                                  *
     H*   42       AUDITED PREMIUM                                    *
     H*   77       DISPLAY PREM FIN CODE AND NAME IN RI               *
     H*   78       CANCEL  DATE                                       *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
      *
     FW0610F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
     FWMEAL7    IF   E           K DISK
      *
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
      *** BEGIN ADD *** 12/05/97 **************************************
     F                                     INFDS(DBFDS3)
      *** END   ADD *** 12/05/97 **************************************
     F                                     RENAME(PEM01:PEM02)
      *
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
      *
     FSMCOL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FWTABL     IF   E           K DISK
      *
     FWDELL     IF   E           K DISK
      *** BEGIN ADD *** 12/05/97 **************************************
     F                                     INFDS(DBFDS2)
      *** END   ADD *** 12/05/97 **************************************
      *** BEGIN ADD *** 11/03/97 **************************************
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *** END   ADD *** 11/03/97 **************************************
      *
      *** BEGIN ADD *** 12/05/97 **************************************
      *
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL03)
      *** END   ADD *** 12/05/97 **************************************
     F***** BEGIN ADD **** 09/29/99 ***********************************
     F* Logical for Premium Finance Co.
     F*
     FWMPFL     IF   E           K DISK    INFDS(DBFDSF)
     F*
     F***** END   ADD **** 09/29/99 ***********************************
      *

     D COD             S              2    DIM(199)
     D TYP             S              1    DIM(199)
     D DES             S             30    DIM(199)
     D AMT             S              9  2 DIM(199)
     D RR              S              9    DIM(3) CTDATA PERRCD(1)
     D KEYC            S              3  0 DIM(2)
      *
     D*
     D OPTNM           C                   CONST('A/R Detail Summary')
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** BEGIN ADD *** 12/05/97 **************************************
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D*** END   ADD *** 12/05/97 **************************************
     D***** BEGIN ADD **** 09/29/99 ***********************************
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D***** END   ADD **** 09/29/99 ***********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  SCFRDT                 1      8  0
     D  SCFRMM                 1      2  0
     D  SCFRDD                 3      4  0
     D  SCFRYY                 7      8  0
     D                 DS
     D  SCTODT                 1      8  0
     D  SCTOMM                 1      2  0
     D  SCTODD                 3      4  0
     D  SCTOYY                 7      8  0
     D                 DS
     D  XXFRDT                 1      7  0
     D  XXFRYY                 2      3  0
     D  XXFRMM                 4      5  0
     D  XXFRDD                 6      7  0
     D                 DS
     D  XXTODT                 1      7  0
     D  XXTOYY                 2      3  0
     D  XXTOMM                 4      5  0
     D  XXTODD                 6      7  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5) ASCEND                        CANCAL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  CD                     1     22
     D                                     DIM(11) ASCEND
     D  SCAR01                 1      2
     D  SCAR02                 3      4
     D  SCAR03                 5      6
     D  SCAR04                 7      8
     D  SCAR05                 9     10
     D  SCAR06                11     12
     D  SCAR07                13     14
     D  SCAR08                15     16
     D  SCAR09                17     18
     D  SCAR10                19     20
     D  SCAR11                21     22
     D                 DS
     D  TP                     1     11
     D                                     DIM(11)
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
     D  SCTP11                11     11
     D                 DS
     D  DS                     1    330
     D                                     DIM(11)
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D  SCDS05               121    150
     D  SCDS06               151    180
     D  SCDS07               181    210
     D  SCDS08               211    240
     D  SCDS09               241    270
     D  SCDS10               271    300
     D  SCDS11               301    330
     D                 DS
     D  AM                     1     99  2
     D                                     DIM(11)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  GROUP MASTER FILE
      *

     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *** BEGIN ADD *** 12/05/97 **************************************
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *** END   ADD *** 12/05/97 **************************************
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *  EMPLOYER A/R FILE
      *
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
      *
      *** BEGIN ADD *** 11/03/97 **************************************
      *
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
      *
      *** END   ADD *** 11/03/97 **************************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C* KEY LIST FOR EMPLOYER DETAIL (FOR PREMIUM FINANCE CO.)
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C*  PREMIUM FINANCE CO. KEY
     C*
     C     PFKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCFCOD
     C*
     C     PFKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCFCOD
     C                   KFLD                    XXPFDT
     C***** END   ADD **** 09/29/99 ***********************************
      *
     C     *LIKE         DEFINE    EAPGM         QQDPGM
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C***** BEGIN ADD **** 09/29/99 **********************************
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C***** END   ADD **** 09/29/99 **********************************
     C*
      *** BEGIN ADD *** 11/03/97 **************************************
      *
      *
      *  DEFINE DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
      *
      *** END   ADD *** 11/03/97 **************************************
      *
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQYR1             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQRTCD            1
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
      *  SET INDICATOR FOR NON-DISPLAY OF CMD 13 IF THE PROGRAM IS
      *  CALLED FROM WITHIN ANOTHER PROGRAM AND WE DON'T WANT THE
      *  USER PARAMETERS TO BE CHANGED MID-STREAM.
      *
     C     QQCALL        IFEQ      '2'
     C                   MOVE      '1'           *IN12
     C                   ELSE
     C                   MOVE      '0'           *IN12
     C                   END
      *
     C                   Z-ADD     0             QQTOTL            9 2
     C                   Z-ADD     0             SCFRDT
     C                   Z-ADD     0             SCTODT
     C                   MOVE      *ALL'0'       AM
     C                   MOVE      *ALL'0'       AMT
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
      *
     C     TOP           TAG
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     XXCO#         IFEQ      *ZERO
     C                   Z-ADD     UPCO#         XXCO#
     C                   ENDIF                                                  XXCO#=*ZERO
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C     XXFND         IFEQ      *ZERO
     C                   Z-ADD     UPFND         XXFND
     C                   ENDIF                                                  XXFND=*ZERO
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
     C                   MOVE      QQYR1         SCYR
     C                   MOVE      QQYR1         SCFYR
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
      *
     C     SCYR          IFEQ      *ZERO
     C                   Z-ADD     UPFYR         SCYR
     C                   ENDIF                                                  SCYR=*ZERO
      *
     C     SCEMP#        IFEQ      *ZERO
      *
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
      *
     C                   ELSE
      *
     C     EMKY          CHAIN     PEM02                              60
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
     C                   ENDIF                                                  SCEMP#=*ZERO
      *
     C  N12SCYR          IFNE      *ZERO
     C                   MOVE      *ON           *IN59
     C                   ENDIF                                                  SCYR<>*ZERO
      *
      *** BEGIN DEL *** 12/05/97 **************************************
      **********           Z-ADDUPPRDT    SRDTI
      **********           CALL 'SRCVTDAT'DTMDYY
      **********           MOVELSRDTO     SCFRDT
      *
      **********           Z-ADDUPPRDT    SRDTI
      **********           CALL 'SRCVTDAT'DTMDYY
      **********           MOVELSRDTO     SCTODT
      *** END   DEL *** 12/05/97 **************************************
      *
      *  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
      *** BEGIN ADD *** 12/05/97 **************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
      *** END   ADD *** 12/05/97 **************************************
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR02'
      *
     C     XXUP          IFEQ      '1'
     C                   MOVEL     RR(3)         SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR02
     C                   END
      *
      *  PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
      *  F3=EXIT
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQEMP#
     C                   MOVE      *BLANKS       QQDIV
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  F7=FINANCE INFO
     C*
     C     *INKG         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C                   CALL      'W0045R02'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM      DBRRNF        @@RRN             9 0          WMPFP
     C                   PARM      DBRRN         @@RRN2            9 0          WMFNP
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKG=ON
     C*
     C***** END   ADD **** 09/29/99 ***********************************
      *
      *  F12=CANCEL
      *
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQEMP#
     C                   MOVE      *BLANKS       QQDIV
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      ' '           XXUP
     C                   MOVE      ' '           XXDOWN
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
      *** BEGIN DEL *** 12/05/97 **************************************
      **********
      ** F5=EMPLOYER NUMERIC SEARCH
      **********
      ********** *INKE     IFEQ '1'
      ********** Q2RECF    IFEQ 'SCR01'
      **********           CALL 'W0050R'
      ********** *INKC     PARM           @@KC    1
      ********** *INKL     PARM           @@KL    1
      **********           PARM '2'       @@CALL  1
      **********           PARM           @@RRN   90
      **********           PARM DBRRN     @@RRN2  90
      **********           PARM           @@QTE   1
      **********           PARM 2         @@VIEW  10
      **********           PARM           @@RRN3  90
      **********
      ********** *INKC     CABEQ*ON       TERM
      **********
      ********** *INKL     IFNE *ON
      ********** @@RRN     ANDNE*ZEROS
      **********
      ********** @@RRN     CHAINWMEMP                60
      **********
      **********           MOVE EMEMP#    SCEMP#
      **********           MOVE EMDIV     SCDIV
      **********
      **********           ENDIF                           *INKL
      **********
      **********
      **********
      **********           MOVE *BLANK    Q2RECF
      **********           MOVEL'SCR01'   Q2RECF
      **********           GOTO STRT
      **********
      **********           END
      **********           END
      **********
      ** F7=EMPLOYER ALPHA SEARCH
      **********
      ********** *INKG     IFEQ '1'
      ********** Q2RECF    IFEQ 'SCR01'
      **********           CALL 'W0050R'
      ********** *INKC     PARM           @@KC    1
      ********** *INKL     PARM           @@KL    1
      **********           PARM '2'       @@CALL  1
      **********           PARM           @@RRN   90
      **********           PARM DBRRN     @@RRN2  90
      **********           PARM           @@QTE   1
      **********           PARM 1         @@VIEW  10
      **********           PARM           @@RRN3  90
      **********
      ********** *INKC     CABEQ*ON       TERM
      **********
      **********
      ********** *INKL     IFNE *ON
      ********** @@RRN     ANDNE*ZEROS
      **********
      ********** @@RRN     CHAINWMEMP                60
      **********
      **********           MOVE EMEMP#    SCEMP#
      **********           MOVE EMDIV     SCDIV
      **********
      **********           ENDIF                           *INKL
      **********
      **********
      **********
      **********           MOVE *BLANK    Q2RECF
      **********           MOVEL'SCR01'   Q2RECF
      **********           GOTO STRT
      **********
      **********           END
      **********           END
      *** END   DEL *** 12/05/97 **************************************
      *
      *** BEGIN ADD *** 12/05/97 **************************************
      *
      *  F10 - SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
     C     Q2RECF        IFEQ      'SCR01'
      *
     C                   MOVE      *OFF          *IN38                          SCYR POSITION
     C                   MOVE      *OFF          *IN39                          SCEMP# POSITION
     C                   MOVE      *OFF          *IN40                          SCFRDT POSITION
      *
      *  F10 - TABLE FILE SEARCH - POLICY YEAR
      *
     C     HFLD          IFEQ      'SCYR'
      *
     C                   MOVE      *ON           *IN38                          SCYR POSITION
      *
      *  EMPLOYER NUMBER EQUAL TO ZERO
      *
     C     SCEMP#        IFEQ      *ZEROS
      *
     C                   MOVE      *ZEROS        @@RRN
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
      *
     C     @@RRN         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
      ***** BEG ADD ******* 08/25/98 **********************************
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCNAME
      *
      ***** END ADD ******* 08/25/98 **********************************
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  *INKL=OFF,@@RRN
      *
     C                   ENDIF                                                  SCEMP#=0
      *
      *  EMPLOYER NUMBER NOT EQUAL TO ZERO
      *
     C     SCEMP#        IFNE      *ZEROS
      *
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
      *
     C     EMKEY         CHAIN     PEM02                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     XXFND         @1RRN
     C                   Z-ADD     DBRRN3        @1RRN2
      *
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
      *
     C     @1RRN         IFNE      *ZEROS
      *
     C     @1RRN         CHAIN     PEL03                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCYR
      *
     C                   MOVE      *OFF          *IN38                          SCYR POSITION
     C                   MOVE      *ON           *IN40                          SCFRDT POSITION
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  @1RRN<>0
      *
     C                   ENDIF                                                  *INKL=OFF
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  SCEMP#<>0
      *
     C                   ENDIF                                                  HFLD=SCYR
      *
      *  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
      *
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
      *
     C                   MOVE      *ON           *IN39                          SCEMP# POSITION
      *
     C                   MOVE      *ZEROS        @@RRN
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
      *
     C     @@RRN         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
      *
     C                   MOVE      *OFF          *IN39                          SCEMP# POSITION
     C                   MOVE      *ON           *IN40                          SCFRDT POSITION
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  *INKL=OFF,@@RRN
      *
     C                   ENDIF                                                  HFLD=SCEMP#
      *
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF=SCR01
      *
     C                   ENDIF                                                  *INKJ=ON
      *
      *** END   ADD *** 12/05/97 **************************************
      *
      *  PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
      *
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
      *
      *   ROLL UP REQUEST
      *
     C     *IN18         CABEQ     '1'           SCR02T
      *
      *   ROLL DOWN REQUEST
      *
     C     *IN19         CABEQ     '1'           SCR02T
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
      *
      *
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
      *
      *  F14=DARTS
      *
     C     *INKN         IFEQ      '1'
      *
      *  RETAIN SCREEN NUMBER
      *
     C                   MOVE      Q2RECF        HLRECF           10
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
      *                    MOVE *BLANKS   QCQTE
      *          QQQTE     IFEQ 'Q'
      *                    MOVE 'Q'       QCQTE
      *                    END
      *
      *  CALL COMMENT PROGRAM
      *
     C                   MOVEL     'W0050R'      QQDPGM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    QQDPGM
      *
      *
      *
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      HLRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKN
     C                   END
      *
      *  GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      *
      *  PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  YEAR CANNOT BE LESS THAN OR EQUAL TO ZERO
      *
     C     SCYR          CABLE     0             STRT                   2020
      *
      *  EMPLOYER AND DIVISION NUMBERS MUST BE VALID
      *
     C     EMKY          CHAIN     WMEML                              21
     C     *IN21         CABEQ     '1'           STRT
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
      *  VALIDATE DATE
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
      *
      *  DATES MUST BE IN ASCENDING ORDER
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
      *
      *  REFORMAT SCREEN DATES IN YMD ORDER       
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
      *  CHECK FOR EXISTING RECORD IN A/R FILE    
      *
     C                   Z-ADD     SCYR          XXFYR
      *
      *** BEGIN ADD *** 11/03/97 **************************************
      *
      *
      *  BEGIN CALCULATION FOR DIVISION COUNT
      *
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
      *
     C     ELKY1         CHAIN     PEL02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 = OFF
      *
      *  END CALCULATION FOR DIVISION COUNT
      *
      *** END   ADD *** 11/03/97 **************************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  IF A PREMIUM FINANCE CO. IS ASSIGNED TO THE POLICY LOAD CODE
     C*
     C                   MOVE      *OFF          *IN77
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     ELPFIN        SCFCOD
     C                   MOVEL     ELPFDT        XXPFDT
     C*
     C     SCFCOD        IFNE      *BLANKS
     C                   MOVE      *ON           *IN77
     C*
     C     PFKY2         SETGT     PPF01
     C*
     C     PFKY          READPE    PPF01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PFNAME        SCPFNM
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *SCFCOD<>BLK
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C***** END   ADD **** 09/29/99 ***********************************
      *
     C     EAKY          CHAIN     WMEAL7                             50
     C  N50EAPODT        IFGT      XXTODT
     C                   MOVE      '1'           *IN50
     C                   END
      *
      *  CLEAR AND LOAD ARRAYS FROM THE A/R TRANSACTION TABLE FILE
      *
     C                   MOVE      *BLANKS       COD
     C                   MOVE      *BLANKS       TYP
     C                   MOVE      *BLANKS       DES
     C                   MOVE      *ZEROS        AMT
     C                   MOVE      *ZEROS        SCPRBL
     C                   MOVE      *ZEROS        SCCUDU
     C                   MOVE      *ZEROS        SCBILL
     C                   MOVE      *ZEROS        SCPMTS
     C                   MOVE      *ZEROS        SCRFND
     C                   MOVE      *ZEROS        SCPREM
     C                   MOVE      *ZEROS        SCDBLL
     C                   MOVE      *ZEROS        SCDPMT
     C                   MOVE      *ZEROS        SCFBLL
     C                   MOVE      *ZEROS        SCFPMT
     C                   MOVE      *BLANKS       XXKEY
     C     TBKY          SETLL     PTT01
      *
     C                   Z-ADD     1             X                 3 0
     C     XXCO#         READE     PTT01                                  51
     C     *IN51         DOWEQ     '0'
     C                   MOVEL     TTKEY         COD(X)
     C                   MOVE      TTTYPE        TYP(X)
     C                   MOVE      TTDESC        DES(X)
     C     XXCO#         READE     PTT01                                  51
     C     X             IFLT      199
     C                   ADD       1             X
     C                   ELSE
     C                   MOVE      '1'           *IN51
     C                   END
     C                   END
      *
      *  SET UP BALANCE DUE LINE
      *
     C     X             IFLT      199
     C                   Z-ADD     X             Z                 3 0
     C                   ELSE
     C                   Z-ADD     199           Z
     C                   END
     C                   MOVEL     'AMOUNT D'    WORK             10
     C                   MOVE      'UE'          WORK
     C                   MOVEL     WORK          DES(Z)
      *
      *  CHAIN TO THE EMPLOYER DETAIL FILE, LOAD PREMIUM
      *
     C     EAKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        SCPREM
     C                   MOVEA     '01'          *IN(41)
     C                   ELSE
     C                   Z-ADD     ELANPR        SCPREM
     C                   MOVEA     '10'          *IN(41)
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZERO         SCPREM
     C                   ENDIF
      *
      *  READ A/R FILE AND ADD TO APPROPRIATE BUCKETS AND ARRAYS
      *
     C     *IN50         DOWEQ     '0'
      *
     C     EAPODT        IFLT      XXFRDT
      *
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C                   SUB       EAAMT         SCPRBL
     C                   ELSE
     C                   ADD       EAAMT         SCPRBL
     C                   END
      *
     C                   ELSE
      *
     C                   Z-ADD     1             X
     C     EAARCD        LOOKUP    COD(X)                                 60
     C     *IN60         IFEQ      '1'
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C                   SUB       EAAMT         AMT(X)
     C                   ELSE
     C                   ADD       EAAMT         AMT(X)
     C                   ENDIF
      *
     C                   END
      *
     C     EAARTY        IFEQ      'B'                                          BILLS
     C     EAARTY        OREQ      'A'                                          ADJUSTMENTS
     C     EAARTY        OREQ      'D'                                          AUDIT BILLS
     C                   ADD       EAAMT         SCBILL
     C                   ADD       EAAMT         SCCUDU
     C                   ELSE
      *
     C     EAARTY        IFEQ      'P'                                          PREMIUM PAYMENTS
     C                   ADD       EAAMT         SCPMTS
     C                   SUB       EAAMT         SCCUDU
     C                   ELSE
      *
     C     EAARTY        IFEQ      'V'                                          VOIDS AND NSF
     C     EAARTY        OREQ      'N'
     C                   SUB       EAAMT         SCPMTS
     C                   ADD       EAAMT         SCCUDU
     C                   ELSE
      *
     C     EAARTY        IFEQ      'H'                                          DEPOSIT BILLED
     C                   ADD       EAAMT         SCDBLL
     C                   ADD       EAAMT         SCCUDU
     C                   ELSE
      *
     C     EAARTY        IFEQ      'I'                                          DEPOSIT PMNTS
     C                   ADD       EAAMT         SCDPMT
     C                   SUB       EAAMT         SCCUDU
     C                   ELSE
      *
     C     EAARTY        IFEQ      'F'                                          FEE BILLED
     C                   ADD       EAAMT         SCFBLL
     C                   ADD       EAAMT         SCCUDU
     C                   ELSE
      *
     C     EAARTY        IFEQ      'Q'                                          FEE PAYMENTS
     C                   ADD       EAAMT         SCFPMT
     C                   SUB       EAAMT         SCCUDU
     C                   ELSE
      *
     C     EAARTY        IFEQ      'R'                                          REFUNDS
     C                   ADD       EAAMT         SCRFND
     C                   ADD       EAAMT         SCCUDU
      *
     C                   ENDIF                                                  R
     C                   ENDIF                                                  Q
     C                   ENDIF                                                  F
     C                   ENDIF                                                  I
     C                   ENDIF                                                  H
     C                   ENDIF                                                  V,N
     C                   ENDIF                                                  P
     C                   ENDIF                                                  B,A,D
      *
     C                   END
      *
     C     EAKY          READE     WMEAL7                                 50
      * N50      QQYRMO    IFGT QQYM
      *                    MOVE '1'       *IN50
      *                    END
     C  N50EAPODT        IFGT      XXTODT
     C                   MOVE      '1'           *IN50
     C                   END
      *
     C                   END
      *
     C                   Z-ADD     SCCUDU        AMT(Z)
     C                   ADD       SCPRBL        SCCUDU
     C                   MOVE      '1'           XXDOWN
     C                   Z-ADD     1             X
     C                   MOVE      '1'           *IN60
     C                   EXSR      LOAD
      *
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
      *
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    CX(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
      *
     C     Y             IFEQ      0
     C     CX(Y)         ORLT      ED(X)
     C                   MOVE      '0'           *IN78                          ACTIVE
     C                   ELSE
     C                   MOVE      '1'           *IN78                          CANCELLED
     C                   Z-ADD     CX(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C                   END
      *
      *
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C     SCR02T        TAG
      *
      *  INVALID ROLL REQUEST
      *
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
      *  ROLL UP REQUEST
      *
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN            1
     C     KEYC(2)       ADD       1             X
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
      *
      *  ROLL DOWN REQUEST
      *
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP              1
     C     KEYC(1)       SUB       1             X
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
      *
      *  END THE PROGRAM
      *
      *  CALL PROGRAM TO DISPLAY DETAIL
      *
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCYR          QQYR1
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCFRDT        QQFRDT
     C                   MOVE      SCTODT        QQTODT
     C                   MOVE      *BLANKS       QQAM
     C                   MOVE      *BLANKS       QQCHK#
     C                   CALL      'W0611R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQYR1             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQAM              9
     C                   PARM                    QQCHK#            7
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   GOTO      TERM
     C                   ENDIF                                                  QQRTCD=A
      *
     C     QQRTCD        IFEQ      'B'
      *
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C                   MOVE      '*'           QQRTCD
      *
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      ' '           XXUP
     C                   MOVE      ' '           XXDOWN
     C     QQCALL        IFEQ      ' '
     C                   GOTO      STRT
     C                   END
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *  CLEAR SCREEN ARRAYS
      *
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *ZEROS        AM
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
      *
      *  FILL SCREEN ARRAYS
      *
     C     FILL          BEGSR
     C                   MOVE      COD(X)        CD(I1)
     C                   MOVE      TYP(X)        TP(I1)
     C                   MOVE      DES(X)        DS(I1)
     C                   Z-ADD     AMT(X)        AM(I1)
     C                   MOVE      '1'           XXOK              1
     C     I1            IFEQ      1
     C                   MOVE      X             KEYC(1)
     C                   END
     C     I1            IFEQ      11
     C                   MOVE      X             KEYC(2)
     C                   END
     C                   ENDSR
      *
      *  LOAD FORWARD SUBROUTINE
      *
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
      *
      *  FILL SCREEN AND GET NEXT RECORD
      *
     C                   ADD       1             I1
     C                   MOVE      '0'           XXOK
     C     REFIL         TAG
     C     AMT(X)        IFNE      *ZEROS
     C                   EXSR      FILL
     C                   END
      *
     C                   ADD       1             X
     C     COD(X)        IFEQ      *BLANKS
     C     DES(X)        ANDEQ     *BLANKS
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     XXOK          IFEQ      '0'
     C                   GOTO      REFIL
     C                   END
      *
     C     I1            COMP      11                                   61
     C   61              GOTO      OVER
     C*
     C***  BEGIN ADD  ***  10/15/99  *********************************
     C                   SUB       1             X
     C***  END   ADD  ***  10/15/99  *********************************
      *
      *  SCAN FORWARD IN THE ARRAYS TO MAKE SURE ROLL UP IS VALID
      *
     C     CHKUP         TAG
     C                   ADD       1             X
     C     X             IFEQ      200
     C     COD(X)        OREQ      *BLANKS
     C     DES(X)        ANDEQ     *BLANKS
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   ENDIF
     C     AMT(X)        IFEQ      *ZERO
     C                   GOTO      CHKUP
     C                   ENDIF
     C     DONE          TAG
     C                   ENDSR
      *
      *  LOAD BACKWARD SUBROUTINE
      *
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     12            I1
     C     OVERP         TAG
      *
      *  FILL SCREEN AND GET NEXT RECORD
      *
     C                   SUB       1             I1
     C                   MOVE      '0'           XXOK
     C     REFIL2        TAG
     C     AMT(X)        IFNE      *ZEROS
     C                   EXSR      FILL
     C                   END
      *
     C                   SUB       1             X
     C     X             IFEQ      *ZERO
     C                   Z-ADD     1             X
     C                   EXSR      LOAD
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      DONEP
     C                   ENDIF
     C     XXOK          IFEQ      '0'
     C                   GOTO      REFIL2
     C                   END
      *
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C*
     C***  BEGIN ADD  ***  10/15/99  *********************************
     C                   ADD       1             X
     C***  END   ADD  ***  10/15/99  *********************************
      *
      *  SCAN BACKWARD IN THE ARRAYS TO MAKE SURE ROLL DOWN IS VALID
      *
     C     CHKDN         TAG
     C                   SUB       1             X
     C     X             IFEQ      0
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      DONEP
     C                   ENDIF
     C     AMT(X)        IFEQ      *ZERO
     C                   GOTO      CHKDN
     C                   ENDIF
     C     DONEP         TAG
     C                   ENDSR
**
  More...

   Bottom
