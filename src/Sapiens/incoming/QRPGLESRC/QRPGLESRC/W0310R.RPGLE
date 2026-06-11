     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0310R - REINSURANCE POLICY MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/13 C:283 Support: WA146 82656                          *
     H*                 Changed the Expense allocation to TextBox on  *
     H*                 SCR02 and Re-Compiled the program.            *
     H*                                                               *
     H*  09/11/07 C: 53 Project: WC132 - 59375                        *
     H*                 Remove display condition of SCR01 fields      *
     H*                                                               *
     H*  08/29/05 C:132 Project: 114WKT - WC1332                      *
     H*               - Reformat Screen SCR02 for two more sets of    *
     H*                 lines for additional CO.s                     *
     H*               - Add Code to populate the added Fields.        *
     H*               - Add Two more items to each DS                 *
     H*                                                               *
     H*  07/18/05 C:132 Project: WC114  48106                         *
     H*                 Add Missing Message to DDS for not 100% on    *
     H*                 limit Amount. Setup correct indicator, so the *
     H*                 correct field is highlighted.                 *
     H*                                                               *
     H*  06/17/05 C:132 Project: WA114  47202                         *
     H*                 Chain for ReInsure Co. Name, If get record,   *
     H*                 add ReInsure Co. Name to screen.              *
     H*                                                               *
     H*  08/30/02 C:15 Project: WC577                                 *
     H*                Chgd F23 to F04.                               *
     H*                                                               *
     H*  07/20/98 C:33 SEQ#: WC051/12105                              *
     H*                RELOAD THE USER PARMS AFTER F13 IS PRESSED.    *
     H*                                                               *
     H*  06/24/98 C:33 SEQ#: WC051/12105                              *
     H*                BE SURE THAT IF THIS PROGRAM IS CALLED FROM    *
     H*                OTHER THE GROUP IS PASSED.                     *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99   IF CALL TO PGM SC010R (DARTS) ENDS IN ERROR RETURN TO   *
     H*       SAME SCREEN                                             *
     H*                                                               *
     H*****************************************************************
     FW0310F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMRIL1    UF A E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F                                     INFDS(DBFDS2)
     FWMRCL     IF   E           K DISK

     D*
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
      ***** BEG DEL ***** 08/29/05 ************************************
     D**************
     D**************      CO         10  3 0
     D**************      PO         10 15
     D**************      CV         10  5 2A
     D**************      LM         10 11 2
     D**************      RCO        10  3 0
     D**************      RPO        10 15
     D**************      RCV        10  5 2A
     D**************      RLM        10 11 2
      ***** END DEL ***** 08/29/05 ************************************
      *
      ***** BEG ADD ***** 08/29/05 ************************************
      ***** END ADD ***** 08/29/05 ************************************
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
      *
      ***** BEG CHG ***** 08/29/05 ************************************
     D*****************                       1  30 CO
     D  CO                     1     36  0
     D                                     DIM(12)
      ***** END CHG ***** 08/29/05 ************************************
     D  SCCO01                 1      3  0
     D  SCCO02                 4      6  0
     D  SCCO03                 7      9  0
     D  SCCO04                10     12  0
     D  SCCO05                13     15  0
     D  SCCO06                16     18  0
     D  SCCO07                19     21  0
     D  SCCO08                22     24  0
     D  SCCO09                25     27  0
     D  SCCO10                28     30  0
      ***** BEG ADD ***** 08/29/05 ************************************
     D  SCCO11                31     33  0
     D  SCCO12                34     36  0
      ***** END ADD ***** 08/29/05 ************************************
      *
     D                 DS
      ***** BEG CHG ***** 08/29/05 ************************************
      ***** END CHG ***** 08/29/05 ************************************
     D****************                        1 150 PO
     D  PO                     1    180
     D                                     DIM(12)
      ***** END CHG ***** 08/29/05 ************************************
     D  SCPO01                 1     15
     D  SCPO02                16     30
     D  SCPO03                31     45
     D  SCPO04                46     60
     D  SCPO05                61     75
     D  SCPO06                76     90
     D  SCPO07                91    105
     D  SCPO08               106    120
     D  SCPO09               121    135
     D  SCPO10               136    150
      ***** BEG ADD ***** 08/29/05 ************************************
     D  SCPO11               151    165
     D  SCPO12               166    180
      ***** END ADD ***** 08/29/05 ************************************
     D                 DS
      ***** BEG CHG ***** 08/29/05 ************************************
     D****************                        1  50 CV
     D  CV                     1     60  2
     D                                     DIM(12) ASCEND
      ***** END CHG ***** 08/29/05 ************************************
     D  SCCV01                 1      5  2
     D  SCCV02                 6     10  2
     D  SCCV03                11     15  2
     D  SCCV04                16     20  2
     D  SCCV05                21     25  2
     D  SCCV06                26     30  2
     D  SCCV07                31     35  2
     D  SCCV08                36     40  2
     D  SCCV09                41     45  2
     D  SCCV10                46     50  2
      ***** BEG ADD ***** 08/29/05 ************************************
     D  SCCV11                51     55  2
     D  SCCV12                56     60  2
      ***** END ADD ***** 08/29/05 ************************************
     D                 DS
      ***** BEG CHG ***** 08/29/05 ************************************
     D************                            1 110 LM
     D  LM                     1    132  2
     D                                     DIM(12)
      ***** END CHG ***** 08/29/05 ************************************
     D  SCLM01                 1     11  2
     D  SCLM02                12     22  2
     D  SCLM03                23     33  2
     D  SCLM04                34     44  2
     D  SCLM05                45     55  2
     D  SCLM06                56     66  2
     D  SCLM07                67     77  2
     D  SCLM08                78     88  2
     D  SCLM09                89     99  2
     D  SCLM10               100    110  2
      ***** BEG ADD ***** 08/29/05 ************************************
     D  SCLM11               111    121  2
     D  SCLM12               122    132  2
      ***** END ADD ***** 08/29/05 ************************************
     D                 DS
      ***** BEG CHG ***** 08/29/05 ************************************
     D*****************                       1  30 RCO
     D  RCO                    1     36  0
     D                                     DIM(12)
      ***** END CHG ***** 08/29/05 ************************************
     D  RICO01                 1      3  0
     D  RICO02                 4      6  0
     D  RICO03                 7      9  0
     D  RICO04                10     12  0
     D  RICO05                13     15  0
     D  RICO06                16     18  0
     D  RICO07                19     21  0
     D  RICO08                22     24  0
     D  RICO09                25     27  0
     D  RICO10                28     30  0
      ***** BEG ADD ***** 08/29/05 ************************************
     D  RICO11                31     33  0
     D  RICO12                34     36  0
      ***** END ADD ***** 08/29/05 ************************************
     D                 DS
      ***** BEG CHG ***** 08/29/05 ************************************
      ***** END CHG ***** 08/29/05 ************************************
     D*******************                     1 150 RPO
     D  RPO                    1    180
     D                                     DIM(12)
      ***** END CHG ***** 08/29/05 ************************************
     D  RIPO01                 1     15
     D  RIPO02                16     30
     D  RIPO03                31     45
     D  RIPO04                46     60
     D  RIPO05                61     75
     D  RIPO06                76     90
     D  RIPO07                91    105
     D  RIPO08               106    120
     D  RIPO09               121    135
     D  RIPO10               136    150
      ***** BEG ADD ***** 08/29/05 ************************************
     D  RIPO11               151    165
     D  RIPO12               166    180
      ***** END ADD ***** 08/29/05 ************************************
     D                 DS
      ***** BEG CHG ***** 08/29/05 ************************************
     D**********************                  1  50 RCV
     D  RCV                    1     60  2
     D                                     DIM(12) ASCEND
      ***** END CHG ***** 08/29/05 ************************************
     D  RICV01                 1      5  2
     D  RICV02                 6     10  2
     D  RICV03                11     15  2
     D  RICV04                16     20  2
     D  RICV05                21     25  2
     D  RICV06                26     30  2
     D  RICV07                31     35  2
     D  RICV08                36     40  2
     D  RICV09                41     45  2
     D  RICV10                46     50  2
      ***** BEG ADD ***** 08/29/05 ************************************
     D  RICV11                51     55  2
     D  RICV12                56     60  2
      ***** END ADD ***** 08/29/05 ************************************
     D                 DS
      ***** BEG CHG ***** 08/29/05 ************************************
     D***************************             1 110 RLM
     D  RLM                    1    132  2
     D                                     DIM(12)
      ***** END CHG ***** 08/29/05 ************************************
     D  RILM01                 1     11  2
     D  RILM02                12     22  2
     D  RILM03                23     33  2
     D  RILM04                34     44  2
     D  RILM05                45     55  2
     D  RILM06                56     66  2
     D  RILM07                67     77  2
     D  RILM08                78     88  2
     D  RILM09                89     99  2
     D  RILM10               100    110  2
      ***** BEG ADD ***** 08/29/05 ************************************
     D  RILM11               111    121  2
     D  RILM12               122    132  2
      ***** END ADD ***** 08/29/05 ************************************
     D*                                                               *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCASFL                10     10
     D  QCEFDT                11     18  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*                                                               *
     D OPTNM           C                   CONST('Reinsurance Policy M-
     D                                     aintenance')

     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  REINSURANCE POLICY FILE
     C*
     C     RIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL            1
     C                   KFLD                    XXEFDT
     C*
     C*  REINSURANCE COMPANY FILE
     C*
     C     RCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRCO             3 0
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    RIDT          XXEFDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMEMP#        Q@EMP#
     C     *LIKE         DEFINE    EMDIV         Q@DIV
     C     *LIKE         DEFINE    EMEMP#        QCEMP#
     C     *LIKE         DEFINE    EMDIV         QCDIV
     C     *LIKE         DEFINE    SCEFDT        Q@EFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQASFL            1
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQFND             3
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
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
     C                   MOVE      QQASFL        SCASFL
     C                   MOVE      QQEFDT        SCEFDT
     C     QQCALL        COMP      '1'                                    09
     C     QQASFL        COMP      ' '                                    56
     C*
     C*** BEG ADD *** 09/11/07 ****************************
     C     *IN09         IFNE      *ON
     C                   MOVE      MD(3)         SCMODE
     C                   ELSE
     C                   MOVE      MD(1)         SCMODE
     C                   ENDIF                                                  *IN09<>ON
     C*** END ADD *** 09/11/07 ****************************
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C*
     C                   MOVE      *ON           *IN92
     C                   MOVE      *ON           *IN91
     C*
     C     Q1PRMS        IFGT      8
     C                   MOVE      QQFND         XXFND
     C     XXFND         COMP      *ZERO                                  91
     C                   ENDIF                                                  Q1PRMS>8
     C*
     C  N91SCEMP#        COMP      *ZERO                                  92
     C*
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      ***** BEG ADD ***** 08/29/05 ************************************
     C                   MOVEL     FNNAME        SCNAM2
      ***** END ADD ***** 08/29/05 ************************************
     C*
     C     QQASFL        IFNE      *BLANK
     C*
     C                   MOVE      'N'           SC01FL            1
     C                   MOVE      Q1USER        SCUSER
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
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      SCR01T
     C*
     C*  SET ON SCREEN ONE FLAG (FOR SCREEN TWO CANCEL FUNCTION)
     C*  INQ3 TAG?
     C                   ELSE
     C*
     C                   MOVE      'Y'           SC01FL
     C*
     C                   END
     C*
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       SCASFL
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
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
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C*================================================================
     C*  PROCESS ALL FUNCTION KEYS
     C*================================================================
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*================================================================
     C*  F3 = EXIT (ALL SCREENS)
     C*================================================================
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C*================================================================
     C*  SCREEN 1 - FUNCTION KEYS
     C*================================================================
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F12 = CANCEL
     C*
     C     *INKL         CABEQ     *ON           TERM
     C*
     C*  F7 = UNCOVERED CODE MAINTENANCE/INQUIRY
     C*
     C     *INKG         IFEQ      *ON
     C*
     C**      VALIDATE EMP, EFDT, & FLAG
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      SCASFL        XXASFL
     C*
     C     RIKY          CHAIN     PRI01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXCEPT    PRI01E
     C*
     C                   CALL      'W0312R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM                    SCEFDT
     C                   PARM                    SCASFL
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C                   GOTO      SCR01T
     C                   ENDIF                                                  *INKL=OFF,QQRTCF
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON,QQRTC
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   ENDIF                                                  *INKG=ON
     C*
     C*  F10 = SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C**      ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
     C     HFLD          COMP      'SCASFL'                               83
     C*
     C*  F10 = SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C   92HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      *ZEROS        @@RRNF            9 0
     C                   PARM      DBRRNF        @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRNF        ANDNE     *ZEROS
     C*
     C     @@RRNF        CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVE      *ON           *IN81                          SCEFDT POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRNF
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRNF        OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN80                          SCEMP# POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRNF
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C*  F10 = SEARCH - EFFECTIVE DATE
     C*
     C     HFLD          IFEQ      'SCEFDT'
     C*
     C                   CALL      'W0311R'
     C                   PARM                    QQPRDS
     C                   PARM      '1'           QQCALL
     C                   PARM      SCEMP#        Q@EMP#
     C                   PARM      SCDIV         Q@DIV
     C                   PARM      SCEFDT        Q@EFDT
     C                   PARM      SCASFL        Q@ASFL            1
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     QQRTC         ANDEQ     '*'
     C     Q@EFDT        ANDNE     *ZEROS
     C                   Z-ADD     Q@EMP#        SCEMP#
     C                   Z-ADD     Q@DIV         SCDIV
     C                   Z-ADD     Q@EFDT        SCEFDT
     C                   MOVE      Q@ASFL        SCASFL
     C                   MOVE      *ON           *IN82                          SCASFL POSITION
     C                   ENDIF                                                  *INKL=OFF,QQRTCF
     C*
     C     *INKL         IFEQ      *ON
     C     QQRTC         OREQ      'B'
     C     Q@EFDT        OREQ      *ZEROS
     C                   MOVE      *ON           *IN81                          SCEFDT POSITION
     C                   ENDIF                                                  *INKL=ON,QQRTC
     C*
     C                   ENDIF                                                  HFLD=SCEFDT
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C*  F13 = PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'W0006R'
     C*
     C**       GET THE USER PARAMETER FILE
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   MOVE      *BLANK        SCCONM
     C                   MOVEL     *ZERO         SCFND
     C                   Z-ADD     *ZERO         SCEMP#
     C                   Z-ADD     *ZERO         SCDIV
     C                   MOVEL     *BLANK        SCNAME
     C*
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   GOTO      STRT01
     C*
     C                   ENDIF                                                  *INKM=ON
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*================================================================
     C*  SCREEN 2 - FUNCTION KEYS
     C*================================================================
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*  F12 = CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C     SC01FL        IFEQ      'Y'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C                   GOTO      TERM
     C                   ENDIF                                                  SC01FL='Y'
     C*
     C                   ENDIF                                                  *INKL=ON
     C*
     C*  F7 = REIN COMPANY MASTER INQUIRY
     C*
     C     *INKG         IFEQ      *ON
     C*
     C                   CALL      'W0300R'
     C                   PARM                    QQPRDS
     C                   PARM      *BLANK        QQCLL             1
     C*
     C                   ENDIF                                                  *INKG=ON
     C*
     C*  F9 = DELETE DETAIL RECORDS
     C*
     C     *INKI         IFEQ      *ON
     C*
     C**      POSITION CURSOR AFTER DELETION OF DETAIL RECORD
     C*
     C     HFLD          COMP      'SCCO01'                               65
     C     HFLD          COMP      'SCCO02'                               66
     C     HFLD          COMP      'SCCO03'                               67
     C     HFLD          COMP      'SCCO04'                               68
     C     HFLD          COMP      'SCCO05'                               69
     C     HFLD          COMP      'SCCO06'                               70
     C     HFLD          COMP      'SCCO07'                               71
     C     HFLD          COMP      'SCCO08'                               72
     C     HFLD          COMP      'SCCO09'                               73
     C     HFLD          COMP      'SCCO10'                               74
      ***** BEG ADD ***** 08/29/05 ************************************
     C     HFLD          COMP      'SCCO11'                               75
     C     HFLD          COMP      'SCCO12'                               76
      ***** END ADD ***** 08/29/05 ************************************
     C*
     C**      EXTRACT NUMBER FROM HFLD POSITION
     C*
     C                   MOVEL(P)  HFLD          HFLD06            6
     C                   MOVE(P)   HFLD06        XX                2 0
     C*
     C     XX            IFGE      1
      ***** BEG CHG ***** 08/29/05 ************************************
     C********** XX        ANDLE10
     C     XX            ANDLE     12
      ***** END CHG ***** 08/29/05 ************************************
     C*
     C                   MOVE      *BLANKS       CO(XX)
     C                   MOVE      *BLANKS       PO(XX)
     C                   MOVE      *BLANKS       CV(XX)
     C                   MOVE      *BLANKS       LM(XX)
     C*
     C                   ENDIF                                                  XX >1 & <10
     C*
     C                   ENDIF                                                  *INKG=ON
     C*
     C*  F10 = SEARCHES
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C**       ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
      ****** BEG DEL ***** 06/17/05 ***************************************
     C*******    HFLD      IFGE 'SCPO01'
     C*******    HFLD      ANDLE'SCPO10'
     C*******              MOVE *ON       *IN84
     C*******              ENDIF                           HFLD=SCPOXX
      ****** END DEL ***** 06/17/05 ***************************************
     C*
      ***************
      ***** BEG DEL ***** 08/29/05 ************************************
     C***********HFLD      IFGE 'SCCV01'
     C***********HFLD      ANDLE'SCCV10'
     C***************      MOVE *ON       *IN85
     C***************      ENDIF                           HFLD=SCCVXX
     C***************
     C***********HFLD      IFGE 'SCLM01'
     C***********HFLD      ANDLE'SCLM10'
     C***************      MOVE *ON       *IN86
     C***************      ENDIF                           HFLD=SCLMXX
     C***************
     C***F10*=*SEARCH - COMPANY NUMBER
     C***************
     C***********HFLD      IFGE 'SCCO01'
     C***********HFLD      ANDLE'SCCO10'
     C***************
      *******BEG*ADD****** 06/17/05 ***************************************
     C***********HFLD      ORGE 'SCPO01'
     C***********HFLD      ANDLE'SCPO10'
      ****** END ADD ***** 06/17/05 ***************************************
      ***** END DEL ***** 08/29/05 ************************************
      *
      ***** BEG ADD ***** 08/29/05 ************************************
     C     HFLD          IFGE      'SCCV01'
     C     HFLD          ANDLE     'SCCV12'
     C                   MOVE      *ON           *IN85
     C                   ENDIF                                                  HFLD=SCCVXX
     C*
     C     HFLD          IFGE      'SCLM01'
     C     HFLD          ANDLE     'SCLM12'
     C                   MOVE      *ON           *IN86
     C                   ENDIF                                                  HFLD=SCLMXX
     C*
     C*  F10 = SEARCH - COMPANY NUMBER
     C*
     C     HFLD          IFGE      'SCCO01'
     C     HFLD          ANDLE     'SCCO12'
     C*
     C     HFLD          ORGE      'SCPO01'
     C     HFLD          ANDLE     'SCPO12'
      ***** END ADD ***** 08/29/05 ************************************
     C*
     C**       EXTRACT NUMBER FROM HFLD POSITION
     C*
     C                   MOVEL(P)  HFLD          HFLD06            6
     C                   MOVE(P)   HFLD06        XX                2 0
     C*
     C     XX            IFGE      1
      ***** BEG CHG ***** 08/29/05 ************************************
     C********** XX        ANDLE10
     C     XX            ANDLE     12
      ***** END CHG ***** 08/29/05 ************************************
     C*
     C**********           Z-ADDCO,XX     XXRCO   30
     C*
     C                   CALL      'W0301R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      *ZEROS        XXRCO             3 0
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     QQRTC         ANDEQ     '*'
     C     XXRCO         ANDNE     *ZEROS
     C*
     C                   Z-ADD     XXRCO         CO(XX)
     C*
      ****** BEG ADD ***** 06/17/05 ***************************************
     C     RCKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCSHNM        PO(XX)
      ****** END ADD ***** 06/17/05 ***************************************
     C*
     C     XX            ADD       44            X                 2 0
     C                   MOVE      *ON           *IN(X)                         SCCOXX POSITION
     C                   ENDIF                                                  *INKL=OFF,QQRTCF
     C*
     C     *INKL         IFEQ      *ON
     C     QQRTC         OREQ      'B'
     C     XXRCO         OREQ      *ZEROS
     C     XX            ADD       64            X                 2 0
     C                   MOVE      *ON           *IN(X)                         SCCOXX POSITION
     C                   ENDIF                                                  *INKL=ON,QQRTC
     C*
     C                   ENDIF                                                  XX >1 & <10
     C*
     C                   ENDIF                                                  HFLD=SCCOXX
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C*  F14 = DARTS
     C*
     C     *INKN         IFEQ      *ON
     C                   EXSR      SRDART
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKN=ON
     C*
     C*  F23 = DELETE POLICY RECORD (MUST DO F23 TWICE SUCCESSIVELY)
     C*
     C*** Begin dlt *** 08/30/02 **************************************
     C********** *INKX     IFEQ *ON
     C**********           MOVE *OFF      *INKX
     C*** End dlt ***** 08/30/02 **************************************
     C*** Begin add *** 08/30/02 **************************************
     C     *INKD         IFEQ      *ON
     C                   MOVE      *OFF          *INKD
     C*** End add ***** 08/30/02 **************************************
     C                   MOVE      *ON           *IN79                          ERR MSG 1119
     C                   EXFMT     SCR02
     C                   MOVE      *OFF          *IN79
     C*
     C*** Begin chg *** 08/30/02 **************************************
     C********** *INKX     IFEQ *ON
     C     *INKD         IFEQ      *ON
     C*** End chg ***** 08/30/02 **************************************
     C*
     C     RIKY          CHAIN     PRI01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PRI01
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       SCASFL
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKX=ON
     C*
     C                   ENDIF                                                  *INKX=ON
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C*================================================================
     C*  SCREEN 3 - FUNCTION KEYS
     C*================================================================
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C*  F12 = CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*
     C*  F14 = DARTS
     C*
     C     *INKN         IFEQ      *ON
     C                   EXSR      SRDART
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKN=ON
     C*
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR03
     C*
     C                   ENDIF                                                  *IN11=ON
     C*
     C*================================================================
     C*  END OF FUNCTION KEYS
     C*================================================================
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE(P)   Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE(P)   XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*  VALIDATE EMPLOYER IF ENTERED
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   MOVE      *BLANKS       SCENAM
     C*
     C     SCEMP#        IFNE      0
     C*
     C     EMKY          CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      EMNAME        SCENAM
     C                   END
     C                   END
     C*
     C*  VALIDATE EFFECTIVE DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  AGGREGATE/SPECIFIC FLAG MUST BE 'A' OR 'S'
     C*
     C     SCASFL        IFNE      'A'
     C     SCASFL        ANDNE     'S'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  SET INDICATOR FOR AGGREGATE INFO
     C*
     C     SCASFL        COMP      'A'                                    78
     C*
     C*  CLEAR FIELDS FOR ADD
     C*
     C                   MOVE      MD(2)         SCMODE
     C*
     C                   Z-ADD     0             SCATTP
     C                   MOVE      *BLANKS       SCPRTP
     C                   Z-ADD     0             SCPAMT
     C                   Z-ADD     0             SCMNLS
     C                   MOVE      *BLANKS       SCAUDF
     C                   Z-ADD     0             SCIPCT
     C*
     C                   Z-ADD     *ZERO         CO
     C                   MOVE      *BLANKS       PO
     C                   Z-ADD     *ZERO         CV
     C                   Z-ADD     *ZERO         LM
     C*
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANKS       SCCOM4
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      SCASFL        XXASFL
     C*
     C     RIKY          CHAIN     PRI01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PRI01E
     C*
     C*
     C     *IN09         IFNE      *ON
     C                   MOVE      MD(3)         SCMODE
     C                   ELSE
     C                   MOVE      MD(1)         SCMODE
     C                   ENDIF                                                  *IN09<>ON
     C*
     C                   Z-ADD     RIATTP        SCATTP
     C                   MOVE      RIPRTP        SCPRTP
     C                   Z-ADD     RIPAMT        SCPAMT
     C                   Z-ADD     RIMNLS        SCMNLS
     C                   MOVE      RIAUDF        SCAUDF
     C                   Z-ADD     RIIPCT        SCIPCT
     C                   Z-ADD     RICO01        SCCO01
     C                   Z-ADD     RICO02        SCCO02
     C                   Z-ADD     RICO03        SCCO03
     C                   Z-ADD     RICO04        SCCO04
     C                   Z-ADD     RICO05        SCCO05
     C                   Z-ADD     RICO06        SCCO06
     C                   Z-ADD     RICO07        SCCO07
     C                   Z-ADD     RICO08        SCCO08
     C                   Z-ADD     RICO09        SCCO09
     C                   Z-ADD     RICO10        SCCO10
     C                   MOVE      RIPO01        SCPO01
     C                   MOVE      RIPO02        SCPO02
     C                   MOVE      RIPO03        SCPO03
     C                   MOVE      RIPO04        SCPO04
     C                   MOVE      RIPO05        SCPO05
     C                   MOVE      RIPO06        SCPO06
     C                   MOVE      RIPO07        SCPO07
     C                   MOVE      RIPO08        SCPO08
     C                   MOVE      RIPO09        SCPO09
     C                   MOVE      RIPO10        SCPO10
     C                   Z-ADD     RICV01        SCCV01
     C                   Z-ADD     RICV02        SCCV02
     C                   Z-ADD     RICV03        SCCV03
     C                   Z-ADD     RICV04        SCCV04
     C                   Z-ADD     RICV05        SCCV05
     C                   Z-ADD     RICV06        SCCV06
     C                   Z-ADD     RICV07        SCCV07
     C                   Z-ADD     RICV08        SCCV08
     C                   Z-ADD     RICV09        SCCV09
     C                   Z-ADD     RICV10        SCCV10
     C                   Z-ADD     RILM01        SCLM01
     C                   Z-ADD     RILM02        SCLM02
     C                   Z-ADD     RILM03        SCLM03
     C                   Z-ADD     RILM04        SCLM04
     C                   Z-ADD     RILM05        SCLM05
     C                   Z-ADD     RILM06        SCLM06
     C                   Z-ADD     RILM07        SCLM07
     C                   Z-ADD     RILM08        SCLM08
     C                   Z-ADD     RILM09        SCLM09
     C                   Z-ADD     RILM10        SCLM10
     C                   MOVE      RICOM1        SCCOM1
     C                   MOVE      RICOM2        SCCOM2
     C                   MOVE      RICOM3        SCCOM3
     C                   MOVE      RICOM4        SCCOM4
     C                   MOVE      RIEXPF        SCEXPF
     C                   MOVE      RIEXPR        SCEXPR
      *
      ***** BEG ADD ***** 08/29/05 ************************************
     C                   Z-ADD     RICO11        SCCO11
     C                   Z-ADD     RICO12        SCCO12
     C                   MOVE      RIPO11        SCPO11
     C                   MOVE      RIPO12        SCPO12
     C                   Z-ADD     RICV11        SCCV11
     C                   Z-ADD     RICV12        SCCV12
     C                   Z-ADD     RILM11        SCLM11
     C                   Z-ADD     RILM12        SCLM12
      ***** END ADD ***** 08/29/05 ************************************
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C     *IN09         CABEQ     '1'           INQ2
     C*
     C*   IF AGGREGATE, VALIDATE ATTACHMENT POINT; PREMIUM TYPE;
     C*   PREMIUM AMOUNT; MINIMUM LOSS FUND; AND AUDIT FLAG.
     C*
     C     SCASFL        IFEQ      'A'
     C*
     C*   ATTACHMENT POINT CANNOT BE LESS THAN 0
     C*
     C     SCATTP        IFLE      0
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  PREMIUM TYPE AND AMOUNT ARE MUTUALLY EXCLUSIVE
     C*
     C     SCPRTP        IFNE      *BLANKS
     C     SCPAMT        ANDNE     0
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C     SCPRTP        IFEQ      *BLANKS
     C     SCPAMT        ANDEQ     0
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  PREMIUM TYPE FOR ATTACHMENT MUST BE 'S', 'M', OR 'N', IF
     C*  ENTERED
     C*
     C     SCPRTP        IFNE      *BLANKS
     C     SCPRTP        ANDNE     'S'
     C     SCPRTP        ANDNE     'M'
     C     SCPRTP        ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  PREMIUM AMOUNT CANNOT BE LESS THAN 0,IF ENTERED.
     C*
     C     SCPAMT        CABLT     0             STRT                   21
     C*
     C*  MINIMUM LOSS FUND CANNOT BE LESS THAN OR EQUAL TO 0
     C*
     C     SCMNLS        IFLT      0
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  AUDIT FLAG MUST BE "Y" OR "N".
     C*
     C     SCAUDF        IFNE      'Y'
     C     SCAUDF        ANDNE     'N'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*   PERCENTAGE CLAIMS FUND INCLUDING SELF FUNDED AGGREGATE
     C*   CANNOT BE LESS THAN 0
     C*
     C     SCIPCT        IFLT      0
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*  EXP PAYMENT FLAG MUST BE EQUAL "Y" OR "N".
     C*
     C     SCEXPF        IFNE      'Y'
     C     SCEXPF        ANDNE     'N'
     C                   MOVE      '1'           *IN55
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   IF SPECIFIC, VALIDATE EXPENSE ALLOCATION FLAG
     C*
     C     SCASFL        IFEQ      'S'
     C*
     C*  EXPENSE ALLOCATION FLAG MUST BE EQUAL "P" OR "F".
     C*
     C     SCEXPR        IFNE      'P'
     C     SCEXPR        ANDNE     'F'
     C                   MOVE      '1'           *IN57
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  VALIDATE INFO FOR ALL COMPANIES ENTERED
     C*
      ***** BEG CHG ***** 08/29/05 ************************************
     C*********  1         DO   10        Y       20
     C     1             DO        12            Y                 2 0
      ***** END CHG ***** 08/29/05 ************************************
     C*
     C     CO(Y)         IFNE      0
     C     PO(Y)         ORNE      *BLANKS
     C     CV(Y)         ORNE      0
     C     LM(Y)         ORNE      0
     C*
     C                   MOVEA     '0000000'     *IN(35)
     C                   MOVEA     '000'         *IN(42)
     C*
     C     Y             ADD       34            Y1                3 0
     C                   MOVE      '1'           *IN(Y1)
     C*
     C*  VALIDATE COMPANY# IF ENTERED
     C*
     C     CO(Y)         IFNE      0
     C     CO(Y)         ANDNE     999
     C                   Z-ADD     CO(Y)         XXRCO
     C     RCKY          CHAIN     PRC01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C*
     C*  COVERAGE % AND LIMIT CANNOT BE ENTERED WITHOUT CO#
     C*
     C     CO(Y)         IFEQ      0
     C     CV(Y)         IFGT      0
     C     LM(Y)         ORGT      0
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  % OF COVERAGE CANNOT BE LESS THAN OR EQUAL TO 0
     C*
     C     CV(Y)         IFLE      0
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  LIMIT CANNOT BE LESS THAN OR EQUAL TO 0
     C*
     C     LM(Y)         IFLE      0
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
      ***** BEG ADD ***** 07/18/05 *************************************
     C                   Z-ADD     Y             X1                3 0
      ***** END ADD ***** 07/18/05 *************************************
     C*
     C                   END
     C*
     C                   END
     C*
     C*  TOTAL COVERAGE SPECIFIED FOR A LIMIT MUST TOTAL 100%
     C*
     C                   Z-ADD     0             XXCOVR            7 2
     C                   Z-ADD     0             XXLMT            11 2
     C                   Z-ADD     1             Y
     C     *ZERO         LOOKUP    CV(Y)                              60
     C     *IN60         DOWEQ     '1'
     C*
     C     XXLMT         IFEQ      LM(Y)
     C     XXCOVR        ANDNE     100
     C                   ADD       CV(Y)         XXCOVR
     C                   ELSE
     C     XXCOVR        DIV       100           XXDIVD            5 2
     C     XXDIVD        IFNE      1
     C     XXDIVD        ANDNE     0
     C                   MOVE      '1'           *IN30
      ***** BEG ADD ***** 07/18/05 *************************************
      * Setup Indicator for Incorrect Percnetage Field
     C     X1            ADD       34            X1                3 0
     C                   MOVE      '0'           *IN(X1)
     C     Y             ADD       33            Y1                3 0
     C                   MOVE      '1'           *IN(Y1)
      ***** END ADD ***** 07/18/05 *************************************
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     LM(Y)         XXLMT
     C                   Z-ADD     CV(Y)         XXCOVR
     C                   END
     C*
     C                   ADD       1             Y
      ***** BEG CHG ***** 08/29/05 ************************************
     C********** Y         COMP 11                     60
     C     Y             COMP      13                                   60
      ***** END CHG ***** 08/29/05 ************************************
     C                   END
     C*
     C     XXCOVR        DIV       100           XXDIVD            5 2
     C     XXDIVD        IFNE      1
     C     XXDIVD        ANDNE     0
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     INQ2          TAG
     C*
     C*  ALL EDITS SCR02  PASSED  - GOTO SCR03
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  VALIDATE SCREEN 3
     C*
     C     SCR03T        TAG
     C   09              GOTO      INQ3
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     RIKY          CHAIN     PRI01                              60
     C*
     C*  ON AN ADD, MOVE *BLANKS TO UNCOVERED CODE FIELD
     C*
     C   60              MOVE      *BLANKS       RIUNCV
     C*
     C                   Z-ADD     XXCO#         RICO#
     C                   Z-ADD     SCFND         RIFND
     C                   Z-ADD     SCEMP#        RIEMP#
     C                   Z-ADD     SCDIV         RIDIV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RIEFDT
     C*
     C                   MOVE      SCASFL        RIASFL
     C                   Z-ADD     SCATTP        RIATTP
     C                   MOVE      SCPRTP        RIPRTP
     C                   Z-ADD     SCPAMT        RIPAMT
     C                   Z-ADD     SCMNLS        RIMNLS
     C                   MOVE      SCAUDF        RIAUDF
     C                   Z-ADD     SCIPCT        RIIPCT
     C                   MOVE      SCEXPF        RIEXPF
     C                   MOVE      SCEXPR        RIEXPR
     C*
     C                   Z-ADD     *ZERO         RCO
     C                   MOVE      *BLANKS       RPO
     C                   Z-ADD     *ZERO         RCV
     C                   Z-ADD     *ZERO         RLM
     C                   Z-ADD     0             Y
     C                   Z-ADD     1             X                 2 0
     C*
      ***** BEG CHG ***** 08/29/05 ************************************
     C****************     DO   10        Y
     C                   DO        12            Y
      ***** END CHG ***** 08/29/05 ************************************
     C     CO(Y)         IFNE      *ZERO
     C                   MOVE      CO(Y)         RCO(X)
     C                   MOVE      PO(Y)         RPO(X)
     C                   MOVE      CV(Y)         RCV(X)
     C                   MOVE      LM(Y)         RLM(X)
     C                   ADD       1             X
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANK        RISTAT
     C                   MOVE      Q1USER        RIUSER
     C                   MOVE      Q1PGM         RIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RIDT
     C*
     C                   Z-ADD     DSTIME        RITIME
     C                   MOVE      SCCOM1        RICOM1
     C                   MOVE      SCCOM2        RICOM2
     C                   MOVE      SCCOM3        RICOM3
     C                   MOVE      SCCOM4        RICOM4
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PRI01
     C                   ELSE
     C                   WRITE     PRI01
     C                   END
     C*
     C     INQ3          TAG
     C*
     C*  IF PROGRAM IS BEING CALLED FROM ANOTHER PGM, GO BACK TO
     C*  ORIGINAL PROGRAM OTHERWISE GOTO FIRST SCREEN
     C*
     C     QQASFL        IFEQ      *BLANK
     C     *IN91         ANDEQ     *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*================================================================
     C*  SUBROUTINES
     C*================================================================
     C*----------------------------------------------------------------
     C*  F14 = DARTS (SCREENS 2 & 3)
     C*
     C     SRDART        BEGSR
     C*
     C                   MOVE      Q2RECF        HLRECF           10
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   Z-ADD     SCEMP#        QCEMP#
     C                   Z-ADD     SCDIV         QCDIV
     C                   MOVE      SCASFL        QCASFL
     C                   MOVE      SCEFDT        QCEFDT
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   MOVE(P)   HLRECF        Q2RECF
     C*
     C                   ENDSR                                                  SRDART
     C*----------------------------------------------------------------
     C*
     OPRI01     E            PRI01E
     O*
     O*   ADDED TABLE MD FOR SCMODE
     O*
**  TABLE MD
- Display-
-- Add --
- Change -
- Delete -
