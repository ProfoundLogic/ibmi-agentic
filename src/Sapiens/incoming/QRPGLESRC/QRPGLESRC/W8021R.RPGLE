     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8021R - POST NET DEDUCTIBLE RECOVERY
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM PRESENTS A LIST OF DEDUCTIBLE PAYMENTS       *
     H*     MADE BY NET REPORTING EMPLOYERS.  WHEN A PAYMENT IS       *
     H*     SELECTED, W8022R IS CALLED TO ALLOW THE USER TO SPREAD    *
     H*     THE RECOVERIES ASSOCIATED WITH THE PAYMENT ACROSS CLAIMS  *
     H*     WITH BILLED DEDUCTIBLES.  W8022R SUBMITS A JOB TO         *
     H*     POST THE ACTUAL RECOVERIES.                               *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     M0002R - MENU PROCESSOR                                   *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     W8022R - SPREAD NET RECOVERIES PENDING                    *
     H*                                                               *
     H*   PROGRAMMER: C:24                                            *
     H*   CREATED:    03/01/95    PROJECT:WC311                       *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/13/19 C:62  Support: 76927(146)                            *
     H*                Initialize other arrays.                       *
     H*                                                               *
     H* 12/28/18 C:79  Project:WC2654                                 *
     H*                Change primary logical to sort by Date/Time    *
     H*                                                               *
     H* 08/06/99 C:61  PROJECT WC577                                  *
     H*                Center screen title.                           *
     H*                                                               *
     H* 02/01/99  C:15 Support: WC065/15291                           *
     H*               -Chgd the delete function to zero out DQAMTR    *
     H*                instead of deleting the record.                *
     H*               -Chgd the pgm to allow negative amounts to be   *
     H*                posted back to claims with negative pending    *
     H*                recoveries.                                    *
     H*                                                               *
     H* 08/19/98  C:15 Support: WC065/12476                           *
     H*                Chgd code to prevent a data decimal error      *
     H*                caused by variable 'I1'.                       *
     H*                                                               *
     H* 04/09/98  C:65 PROJECT: WC812                                 *
     H*                ADD AMOUNT REMAINING FIELD TO SCREEN.          *
     H*                USE AMOUNT REMAINING FOR CALL TO W8022R.       *
     H*                                                               *
     H* 03/04/98  C:65 PROJECT: WC506                                 *
     H*                ALLOW CHANGE AND DELETE OPTIONS FOR PAYMENTS.  *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   31-42 - CONTROL DISPLAY OF OPTION LINES                     *
     H*   60  -  GENERAL WORK INDICATOR                               *
     H*   71-82 - EMPLOYER NOT ELIGIBLE FOR PROCESSING  (WCR1171)     *
     H*****************************************************************
     FW8021F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*** Beg Chg *** 12/28/18 ******************************************
     F*****WADQL     UF   E           K DISK
     FWADQL2    UF   E           K DISK
     F*** End Chg *** 12/28/18 ******************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSSYLF     IF   E           K DISK

     D*** BEGIN ADD ***  04/09/98  ************************************
     D*** END ADD *****  04/09/98  ************************************
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*** Beg Chg *** 12/28/18 ***********************************
     D*** KEY             S             24    DIM(2)
     D KEY             S             30    DIM(2)
     D*** End Chg *** 12/28/18 ***********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  OP                     1     10
     D                                     DIM(10)
     D  SCOP01                 1      1
     D  SCOP02                 2      2
     D  SCOP03                 3      3
     D  SCOP04                 4      4
     D  SCOP05                 5      5
     D  SCOP06                 6      6
     D  SCOP07                 7      7
     D  SCOP08                 8      8
     D  SCOP09                 9      9
     D  SCOP10                10     10
     D                 DS
     D  EM                     1     90  0
     D                                     DIM(10)
     D  SCEM01                 1      9  0
     D  SCEM02                10     18  0
     D  SCEM03                19     27  0
     D  SCEM04                28     36  0
     D  SCEM05                37     45  0
     D  SCEM06                46     54  0
     D  SCEM07                55     63  0
     D  SCEM08                64     72  0
     D  SCEM09                73     81  0
     D  SCEM10                82     90  0
     D                 DS
     D  DV                     1     50  0
     D                                     DIM(10)
     D  SCDV01                 1      5  0
     D  SCDV02                 6     10  0
     D  SCDV03                11     15  0
     D  SCDV04                16     20  0
     D  SCDV05                21     25  0
     D  SCDV06                26     30  0
     D  SCDV07                31     35  0
     D  SCDV08                36     40  0
     D  SCDV09                41     45  0
     D  SCDV10                46     50  0
     D                 DS
     D  NM                     1    250
     D                                     DIM(10)
     D  SCNM01                 1     25
     D  SCNM02                26     50
     D  SCNM03                51     75
     D  SCNM04                76    100
     D  SCNM05               101    125
     D  SCNM06               126    150
     D  SCNM07               151    175
     D  SCNM08               176    200
     D  SCNM09               201    225
     D  SCNM10               226    250
     D                 DS
     D  BF                     1     30  0
     D                                     DIM(10)
     D  SCBF01                 1      3  0
     D  SCBF02                 4      6  0
     D  SCBF03                 7      9  0
     D  SCBF04                10     12  0
     D  SCBF05                13     15  0
     D  SCBF06                16     18  0
     D  SCBF07                19     21  0
     D  SCBF08                22     24  0
     D  SCBF09                25     27  0
     D  SCBF10                28     30  0
     D                 DS
     D  CK                     1     70  0
     D                                     DIM(10)
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D  SCCK04                22     28  0
     D  SCCK05                29     35  0
     D  SCCK06                36     42  0
     D  SCCK07                43     49  0
     D  SCCK08                50     56  0
     D  SCCK09                57     63  0
     D  SCCK10                64     70  0
     D                 DS
     D  AM                     1     90  2
     D                                     DIM(10)
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
     D*** BEGIN ADD ***  04/09/98  ************************************
     D                 DS
     D  RM                     1     90  2
     D                                     DIM(10)
     D  SCRM01                 1      9  2
     D  SCRM02                10     18  2
     D  SCRM03                19     27  2
     D  SCRM04                28     36  2
     D  SCRM05                37     45  2
     D  SCRM06                46     54  2
     D  SCRM07                55     63  2
     D  SCRM08                64     72  2
     D  SCRM09                73     81  2
     D  SCRM10                82     90  2
     D*** END ADD *****  04/09/98  ************************************
     D*** Beg Add ***  12/28/18 ************************************
     D                 DS
     D  DT                     1     90  0
     D                                     DIM(10)
     D  SCDT01                 1      7  0
     D  SCDT02                14     20  0
     D  SCDT03                21     27  0
     D  SCDT04                28     34  0
     D  SCDT05                35     41  0
     D  SCDT06                42     48  0
     D  SCDT07                49     55  0
     D  SCDT08                56     62  0
     D  SCDT09                63     69  0
     D  SCDT10                70     90  0
     D                 DS
     D  TM                     1     60  0
     D                                     DIM(10)
     D  SCTM01                 1      6  0
     D  SCTM02                 7     12  0
     D  SCTM03                13     18  0
     D  SCTM04                19     24  0
     D  SCTM05                25     30  0
     D  SCTM06                31     36  0
     D  SCTM07                37     42  0
     D  SCTM08                43     48  0
     D  SCTM09                49     54  0
     D  SCTM10                55     60  0
     D*** End Add ***  12/28/18 ************************************
     D*** Beg Chg *** 12/28/18 ****************************************
     D**               DS
     D**XXKEY                  1     24
     D**XXEMP#                 1      9  0
     D**XXDIV                 10     14  0
     D**XXFYR                 15     17  0
     D**XXCHK#                18     24  0
     D                 DS
     D  XXKEY                  1     30
     D  XXEMP#                 1      9  0
     D  XXDIV                 10     14  0
     D  XXFYR                 15     17  0
     D  XXDT                  18     24  0
     D  XXTIME                25     30  0
     D*** End Chg *** 12/28/18 ****************************************
     D*** Beg Add *** 12/28/18 ****************************************
     D  XXCHK#                        7  0
     D*** End Add *** 12/28/18 ****************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    DQEMP#
     C                   KFLD                    DQDIV
     C*
     C*  KEYS FOR THE DEDUCTIBLE PAYMENT RECOVERIES PENDING FILE
     C*
     C     DQKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*** Beg Chg *** 12/28/18 *************************************
     C**************     KFLD                    XXCHK#
     C                   KFLD                    XXDT
     C                   KFLD                    XXTIME
     C*** End Chg *** 12/28/18 *************************************
     C*
     C     DQKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*** Beg Add *** 12/28/18 *************************************
     C     DQKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCHK#
     C*** End Add *** 12/28/18 *************************************
     C*
     C     *LIKE         DEFINE    DQCO#         XXCO#
     C     *LIKE         DEFINE    DQFYR         QQFYR
     C     *LIKE         DEFINE    DQEMP#        QQEMP#
     C     *LIKE         DEFINE    DQDIV         QQDIV
     C     *LIKE         DEFINE    DQCHK#        QQCHK#
     C     *LIKE         DEFINE    DQAMT         QQAMT
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL06        IFNE      'Y'
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         SCFND
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
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
     C                   MOVE      *ZERO         XXEMP#
     C                   MOVE      *ZERO         XXDIV
     C                   MOVE      *ZERO         XXFYR
     C                   MOVE      *ZERO         XXCHK#
     C*** Beg Add 12/28/18 ********************************************
     C                   MOVE      *ZERO         XXDT
     C                   MOVE      *ZERO         XXTIME
     C*** End Add 12/28/18 ********************************************
     C                   MOVE      *ON           *IN21
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C******     Q2RECF    IFEQ 'SCR02'
     C******               EXFMTSCR02
     C******               END
     C*
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = Exit
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = Cancel
     C*
     C     *INKL         IFEQ      '1'
     C*
     C*******    Q2RECF    IFEQ 'SCR02'
     C*******              MOVEL'SCR01'   Q2RECF
     C*******              END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*  Process screen one
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY(2)        XXKEY
     C     DQKY          CHAIN     PDQ01                              60
     C     DQKY2         READE     PDQ01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXKEY
     C     DQKY          CHAIN     PDQ01                              60
     C     DQKY2         READPE    PDQ01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCEMP#        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *ZEROS        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C                   MOVE      *ZERO         XXFYR
     C                   MOVE      *ZERO         XXCHK#
     C                   MOVE      *ZERO         SCEMP#
     C                   MOVE      *ZERO         SCDIV
     C*** Beg Add *** 12/28/18 ***************************************
     C                   MOVE      *ZERO         XXDT
     C                   MOVE      *ZERO         XXTIME
     C*** End Add *** 12/28/18 ***************************************
     C     DQKY          SETLL     PDQ01
     C     *IN21         IFEQ      *ON
     C                   MOVE      '1'           XXDOWN
     C                   ELSE
     C     DQKY2         READPE    PDQ01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXDOWN
     C                   ELSE
     C                   MOVE      *BLANK        XXDOWN
     C                   ENDIF
     C     DQKY          SETLL     PDQ01
     C                   ENDIF
     C                   MOVE      '0'           *IN21
     C     DQKY2         READE     PDQ01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK THE OPTION FLAGS, AND EXECUTE THE SPREAD PROGRAM
     C*  FOR EACH CHECK SELECTED.
     C*
     C                   MOVE      *BLANK        QQRTCD
     C     1             DO        10            I1
     C*
     C     OP(I1)        IFEQ      '1'
     C** BEGIN DLT *** 02/01/99 ***************************************
     C********** AM,I1     ANDGT*ZERO
     C** END DLT ***** 02/01/99 ***************************************
     C*
     C                   MOVE      BF(I1)        QQFYR
     C                   MOVE      EM(I1)        QQEMP#
     C                   MOVE      DV(I1)        QQDIV
     C                   MOVE      CK(I1)        QQCHK#
     C*** BEGIN CHG ***  04/09/98  ************************************
     C*****                MOVE AM,I1     QQAMT
     C                   MOVE      RM(I1)        QQAMT
     C*** END CHG *****  04/09/98  ************************************
     C*
     C                   CALL      'W8022R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQCHK#
     C                   PARM                    QQAMT
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      'E'
     C                   WRITE     SCR01
     C     I1            ADD       70            X                 3 0
     C                   MOVE      *ON           *IN(X)
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      *BLANK        OP(I1)
     C                   END
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  03/04/98  ************************************
     C     OP(I1)        IFEQ      '2'
     C                   EXSR      SRCHG
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      *BLANK        OP(I1)
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   ENDIF
     C*
     C     OP(I1)        IFEQ      '4'
     C                   EXSR      SRDLT
     C                   MOVE      'C'           QQRTCD
     C                   MOVE      *BLANK        OP(I1)
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   ENDIF
     C*** END ADD *****  03/04/98  ************************************
     C*
     C                   ENDDO
     C*
     C     QQRTCD        IFNE      *BLANK
     C                   MOVE      EM(1)         XXEMP#
     C                   MOVE      DV(1)         XXDIV
     C                   MOVE      BF(1)         XXFYR
     C                   MOVE      CK(1)         XXCHK#
     C*** Beg Add 12/28/18 ********************************************
     C                   MOVE      DT(1)         XXDT
     C                   MOVE      TM(1)         XXTIME
     C*** End Add 12/28/18 ********************************************
     C     DQKY          SETLL     PDQ01
     C     DQKY2         READE     PDQ01                                  60
     C                   EXSR      LOAD
     C                   END
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C     31            DO        42            I1
     C                   MOVE      *OFF          *IN(I1)
     C                   ENDDO
     C                   MOVE      *BLANKS       OP
     C                   MOVE      *ZEROS        EM
     C                   MOVE      *ZEROS        DV
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *ZEROS        BF
     C                   MOVE      *ZEROS        CK
     C                   MOVE      *ZEROS        AM
     C*** BEGIN ADD ***  04/09/98  ************************************
     C                   MOVE      *ZEROS        RM
     C*** END ADD *****  04/09/98  ************************************
     C*** BEGIN ADD ***  03/13/19  ************************************
     C                   MOVE      *ZEROS        DT
     C                   MOVE      *ZEROS        TM
     C*** END ADD *****  03/13/19  ************************************
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      DQEMP#        EM(I1)
     C                   MOVE      DQDIV         DV(I1)
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        NM(I1)
     C                   MOVE      DQFYR         BF(I1)
     C                   MOVE      DQCHK#        CK(I1)
     C                   Z-ADD     DQAMT         AM(I1)
     C*** BEGIN ADD ***  04/09/98  ************************************
     C                   Z-ADD     DQAMTR        RM(I1)
     C*** END ADD *****  04/09/98  ************************************
     C*** Beg Add *** 12/28/18 ****************************************
     C                   Z-ADD     DQDT          DT(I1)
     C                   Z-ADD     DQTIME        TM(I1)
     C*** End Add *** 12/28/18 ****************************************
     C     I1            ADD       30            X
     C                   MOVE      *ON           *IN(X)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*  IF DQAMT = 0, DELETE THE RECORD.
     C*
     C*** BEGIN CHG ***  04/09/98  ************************************
     C*****      DQAMT     IFEQ 0
     C*****                DELETPDQ01
     C     DQAMTR        IFEQ      0
     C                   MOVE      *BLANK        DQSTAT
     C                   UPDATE    PDQ01
     C*** END CHG *****  04/09/98  ************************************
     C                   ELSE
     C                   EXCEPT    PDQ01E
     C     DQSTAT        IFEQ      *BLANK
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   ENDIF
     C                   END
     C     DQKY2         READE     PDQ01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVER
     C                   EXCEPT    PDQ01E
     C     DONE          TAG
     C                   MOVE      EM(1)         XXEMP#
     C                   MOVE      DV(1)         XXDIV
     C                   MOVE      BF(1)         XXFYR
     C                   MOVE      CK(1)         XXCHK#
     C*** Beg Add *** 12/28/18 ***************************************
     C                   MOVE      DT(1)         XXDT
     C                   MOVE      TM(1)         XXTIME
     C*** End Add *** 12/28/18 ***************************************
     C                   MOVEL     XXKEY         KEY(1)
     C                   MOVE      EM(10)        XXEMP#
     C                   MOVE      DV(10)        XXDIV
     C                   MOVE      BF(10)        XXFYR
     C                   MOVE      CK(10)        XXCHK#
     C*** Beg Add *** 12/28/18 ***************************************
     C                   MOVE      DT(10)        XXDT
     C                   MOVE      TM(10)        XXTIME
     C*** End Add *** 12/28/18 ***************************************
     C                   MOVEL     XXKEY         KEY(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C*** BEGIN CHG *** 08/19/98 **************************************
     C******************** Z-ADD13        I1
     C                   Z-ADD     11            I1
     C*** END CHG ***** 08/19/98 **************************************
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C*  IF DQAMT = 0, DELETE THE RECORD.
     C*
     C*** BEGIN CHG ***  04/09/98  ************************************
     C*****      DQAMT     IFEQ 0
     C*****                DELETPDQ01
     C     DQAMTR        IFEQ      0
     C                   MOVE      *BLANK        DQSTAT
     C                   UPDATE    PDQ01
     C*** END CHG *****  04/09/98  ************************************
     C                   ELSE
     C                   EXCEPT    PDQ01E
     C     DQSTAT        IFEQ      *BLANK
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   ENDIF
     C                   END
     C     DQKY2         READPE    PDQ01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C                   EXCEPT    PDQ01E
     C     DONEP         TAG
     C                   MOVE      EM(1)         XXEMP#
     C                   MOVE      DV(1)         XXDIV
     C                   MOVE      BF(1)         XXFYR
     C                   MOVE      CK(1)         XXCHK#
     C*** Beg Add *** 12/28/18 ***************************************
     C                   MOVE      DT(1)         XXDT
     C                   MOVE      TM(1)         XXTIME
     C*** End Add *** 12/28/18 ***************************************
     C                   MOVEL     XXKEY         KEY(1)
     C                   MOVE      EM(10)        XXEMP#
     C                   MOVE      DV(10)        XXDIV
     C                   MOVE      BF(10)        XXFYR
     C                   MOVE      CK(10)        XXCHK#
     C*** Beg Add *** 12/28/18 ***************************************
     C                   MOVE      DT(10)        XXDT
     C                   MOVE      TM(10)        XXTIME
     C*** End Add *** 12/28/18 ***************************************
     C                   MOVEL     XXKEY         KEY(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZERO         XXEMP#
     C                   MOVE      *ZERO         XXDIV
     C                   MOVE      *ZERO         XXFYR
     C                   MOVE      *ZERO         XXCHK#
     C*** Beg Add 12/28/18 *************************************************
     C                   MOVE      *ZEROS        XXDT
     C                   MOVE      *ZEROS        XXTIME
     C*** End Add 12/28/18 *************************************************
     C     DQKY          SETLL     PDQ01
     C     DQKY2         READE     PDQ01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*  IF DQAMT = 0, DELETE THE RECORD.
     C*
     C*** BEGIN CHG ***  04/09/98  ************************************
     C*****      DQAMT     IFEQ 0
     C*****                DELETPDQ01
     C     DQAMTR        IFEQ      0
     C                   MOVE      *BLANK        DQSTAT
     C                   UPDATE    PDQ01
     C*** END CHG *****  04/09/98  ************************************
     C                   ELSE
     C                   EXCEPT    PDQ01E
     C     DQSTAT        IFEQ      *BLANK
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   ENDIF
     C                   END
     C     DQKY2         READE     PDQ01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVERB
     C                   EXCEPT    PDQ01E
     C     DONEB         ENDSR
     C*** BEGIN ADD ***  03/04/98  ************************************
     C*==============================================================
     C* SRCHG - THIS SUBROUTINE WILL PROCESS A CHECK CHANGE.
     C*==============================================================
     C     SRCHG         BEGSR
     C*
     C*  MOVE SELECTED RECORD INTO SCREEN FIELDS THEN DISPLAY WINDOW.
     C*
     C                   MOVE      BF(I1)        SCFYR
     C                   MOVE      EM(I1)        SCEMP#
     C                   MOVE      DV(I1)        SCDIV
     C                   MOVE      CK(I1)        SCCHK#
     C                   Z-ADD     AM(I1)        SCAMT
     C*** BEGIN ADD ***  04/09/98  ************************************
     C                   Z-ADD     RM(I1)        SCAMTR
     C*** END ADD *****  04/09/98  ************************************
     C     WDW1          TAG
     C                   EXFMT     WDW01
     C*
     C*  PROCESS 'F12=CANCEL' FUNCTION KEY
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C*  UPDATE THE DEDUCTIBLE PAYMENT RECORD
     C*
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C                   MOVE      SCCHK#        XXCHK#
     C*** Beg Chg *** 12/28/18 ******************************************
     C**** DQKY          CHAIN     PDQ01                              60
     C     DQKY3         CHAIN     PDQ01                              60
     C*** End Chg *** 12/28/18 ******************************************
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     SCAMTR        DQAMTR
     C                   MOVEL     Q1USER        DQUSER
     C                   MOVEL     Q1PGM         DQPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DQTIME
     C                   Z-ADD     XXDATE        DQDT
     C                   UPDATE    PDQ01
     C                   END
     C*
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C*
     C                   ENDSR
     C*================================================================
     C* SRDLT - THIS SUBROUTINE WILL PROCESS A CHECK DELETE.
     C*================================================================
     C     SRDLT         BEGSR
     C*
     C*  MOVE SELECTED RECORD INTO SCREEN FIELDS THEN DISPLAY WINDOW.
     C*
     C                   MOVE      BF(I1)        SCFYR
     C                   MOVE      EM(I1)        SCEMP#
     C                   MOVE      DV(I1)        SCDIV
     C                   MOVE      CK(I1)        SCCHK#
     C                   Z-ADD     AM(I1)        SCAMT
     C*** BEGIN ADD ***  04/09/98  ************************************
     C                   Z-ADD     RM(I1)        SCAMTR
     C*** END ADD *****  04/09/98  ************************************
     C                   EXFMT     WDW02
     C*
     C*  PROCESS 'F12=CANCEL' FUNCTION KEY
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C*  UPDATE THE DEDUCTIBLE PAYMENT RECORD
     C*
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C                   MOVE      SCCHK#        XXCHK#
     C** BEGIN ADD *** 02/01/99 ***************************************
     C     DQKY          CHAIN     PDQ01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     0             DQAMTR
     C                   MOVEL     Q1USER        DQUSER
     C                   MOVEL     Q1PGM         DQPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DQTIME
     C                   Z-ADD     XXDATE        DQDT
     C                   UPDATE    PDQ01
     C                   ENDIF
     C** END ADD ***** 02/01/99 ***************************************
     C** BEGIN DLT *** 02/01/99 ***************************************
     C********** DQKY      DELETPDQ01                60
     C** END DLT ***** 02/01/99 ***************************************
     C*
     C                   MOVE      *ZEROS        SCEMP#
     C                   MOVE      *ZEROS        SCDIV
     C*
     C                   ENDSR
     C*** END ADD *****  03/04/98  ************************************
     OPDQ01     E            PDQ01E
**
ROLL UP
ROLL DOWN
