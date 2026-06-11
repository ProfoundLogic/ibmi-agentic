     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0030R - POLICY MASTER MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program maintains the Policy Master File (CMPMP)  **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     C0622R    Agent Producer Number Search                 **
     H**     C0625R    Agent Producer Alpha Search                  **
     H**     SRCNTR - Center Data Subroutine                        **
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 03/27/07 C:02  Project: 114COF                                *
     H*                CHANGE TO ALLOW ADDTL PARM FOR DART KEY        *
     H*                THIS WILL ALLOW DATAENTRY DIRECT ACCESS TO POLICY
     H*                                                               *
     H*                                                               *
     H* 02/08/05 C:53  Project: PC011                                 *
     H*                Update CMCMP w/name changes                    *
     H*                                                               *
     H* 02/12/02 C:15  Project: PCxxx                                 *
     H*                Added a delete function to allow policies to   *
     H*                be deleted.                                    *
     H*                                                               *
     H* 08/25/01 C:15  Project: 94xxx                                 *
     H*               -Removed certain edits to allow easier entry of *
     H*                policy.                                        *
     H*               -Added code to allow user to create their own   *
     H*                policy numbers.                                *
     H*                                                               *
     H*   11/18/96 *** C:107 *** V2000 ***                            *
     H*            1. Version 2 format changes.                       *
     H*****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      03       Policy is cancelled status.                  **
     H**      05       Policy is cancellation pending status.       **
     H**      06       Policy is reinstatement pending status.      **
     H**      21       SCR04 Error CCR0925: Binder must by no ...   **
     H**      39       Policy is Non-renewal Status                 **
     H**      57       Screen 3 error - Effective Date error.       **
     H**      99       Call to remarks system ended in error.       **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   SMCOL  (PCO01): Company Master      (CO#)                **
     H**   CMUPL2 (PUP02): User Parameters     (STAF)               **
     H**   CMUPL  (PUP01): User Parameters     (USR)                **
     H**   CMPML  (PPM01): Policy Master       (CO#/POL#)           **
     H**   CMPXL  (PPX01): Policy Auxilary     (CO#/POL#)           **
     H**   CMQLL  (PQL01): Quote Log           (CO#/QUT#)           **
     H**   CDPRL  (PPR01): Agent Producer      (CO#/FEIN/MOD/PROD)  **
     H**   CMAGL2 (PAG01): Agent Master        (CO#/ACC#)           **
     H**   CMPAL  (PPA01): Policy Auxilliary   (CO#/POL#)           **
     H**   CDFRL  (PFR01): PCMS Forms Master   (CO#/FORM)           **
     H**   CTABL  (PBC01): Billing Code Table  (CO#/KEY)            **
     H**          (PCV01): Coverage Code Table (CO#/KEY)            **
     H**          (PCY01): County Code Table   (CO#/KEY)            **
     H****************************************************************
     FC0030F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL2    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FCMUPL     IF   E           K DISK
     FCMPML     UF A E           K DISK
     FCMPXL     IF A E           K DISK
     FCMQLL     UF A E           K DISK
     FCDPRL     IF   E           K DISK
     FCMAGL2    IF   E           K DISK
     FCMPAL     UF A E           K DISK
     FCDFRL     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F                                     IGNORE(PPR01)
     F*** Begin add *** 02/12/02 **************************************
     F*** BEG CHG *** 02/08/05 **************************************
     F*CMCML***IF**E           K        DISK
     FCMCML     UF   E           K DISK
     F*** END CHG *** 02/08/05 **************************************
     F*** End add ***** 02/12/02 **************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D Z0              S              1    DIM(20) CTDATA PERRCD(20)
     D TABPTP          S              2    DIM(30) CTDATA PERRCD(1)
     D TABGRP          S              3    DIM(30) ALT(TABPTP)
     D MSG             S             60    DIM(4) CTDATA PERRCD(1)
     D L124            S             78    DIM(4)                               SCR01 LINE 24
     D L224            S             78    DIM(4)                               SCR02 LINE 24
     D L2A4            S             78    DIM(4)                               SCR02A LINE 24
     D L324            S             78    DIM(4)                               SCR03 LINE 24
     D L424            S             78    DIM(4)                               SCR04 LINE 24
     D TABL24          S              2    DIM(11) CTDATA PERRCD(1)             LINE 24 TEXT
     D TABL2D          S             78    DIM(11) ALT(TABL24)
     D NUM             S              1    DIM(5)                               NUMERIC TST ARRAY
     D TABNUM          S              1    DIM(11) CTDATA PERRCD(11)            NUMERIC TST TABLE
     D FRM             S              6    DIM(20)                              FORM INPUT AREA
     D FRD             S              4    DIM(20)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  PMACMY                 1      5  0
     D  PMACCN                 1      1  0
     D  PMACYR                 2      3  0
     D  PMACMM                 4      5  0
     D*** Begin dlt *** 08/25/01 **************************************
     D*********** DS
     D***********                             1  13 POLCHK
     D***********                             1   3 CHK1
     D***********                             4  120FILR
     D***********                            13  13 CHK2
     D*** End dlt ***** 08/25/01 **************************************
     D                 DS
     D  SA                     1     40
     D                                     DIM(20) ASCEND
     D  SCCC01                 1      2
     D  SCCC02                 3      4
     D  SCCC03                 5      6
     D  SCCC04                 7      8
     D  SCCC05                 9     10
     D  SCCC06                11     12
     D  SCCC07                13     14
     D  SCCC08                15     16
     D  SCCC09                17     18
     D  SCCC10                19     20
     D  SCCC11                21     22
     D  SCCC12                23     24
     D  SCCC13                25     26
     D  SCCC14                27     28
     D  SCCC15                29     30
     D  SCCC16                31     32
     D  SCCC17                33     34
     D  SCCC18                35     36
     D  SCCC19                37     38
     D  SCCC20                39     40
     D                 DS
     D  FL                     1     20
     D                                     DIM(20)
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D  SCFL15                15     15
     D  SCFL16                16     16
     D  SCFL17                17     17
     D  SCFL18                18     18
     D  SCFL19                19     19
     D  SCFL20                20     20
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCPOL#                 4     16
     D*
     D*   NON-RENEWAL MESSAGE
     D*
     D                 DS
     D  XXNRDS                 1     25
     D  XXNRD1                 3     20    INZ('NON-RENEWAL - CODE')
     D  XXNRD2                22     23
     D*
     D*   AUTO FORMS SELECTION HEADING
     D*
     D                 DS
     D  XXPRD                  1     30
     D  XXPRD1                 1     10
     D  XXPRD2                11     30    INZ('AUTO FORMS SELECTION')
     D*
     D*           POLICY FORMS WORK AREA
     D*
     D                 DS
     D  SCFR                   1    100
     D                                     DIM(10)                              FORMS ARRAY
     D  SCFR01                 1     10
     D  SCFR02                11     20
     D  SCFR03                21     30
     D  SCFR04                31     40
     D  SCFR05                41     50
     D  SCFR06                51     60
     D  SCFR07                61     70
     D  SCFR08                71     80
     D  SCFR09                81     90
     D  SCFR10                91    100
     D**** ADD 03/27/07 ************************************************
     D*
     D                 DS
     D  XKEY                   1     40
     D  XCO#                   1      3
     D  XPOL#                  4     16
     D**** END ADD 03/27/07 ********************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPUP02
     I              UPCO#                       UPCO#2

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE KEY LISTS
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY2        KLIST
     C                   KFLD                    SCUNCD
     C*
     C*  TYPE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  POLICY MASTER FILE & POLICY AUXILLARY FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  QUOTE LOG FILE
     C*
     C     QLKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCQUT#
     C*
     C     QLKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C                   KFLD                    XXPROD            3 0
     C*
     C*
     C*  AGENT MASTER FILE KEY LIST
     C*
     C     AGKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXACC#            5 0
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C**** ADD 03/27/07 ************************************************
     C                   PARM                    QQKY1            40
     C**** END ADD 03/27/07 ********************************************
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
     C*  POLICY AUXILLUARY FILE KEY LIST
     C*
     C     PAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  PCMS FORMS MASTER FILE KEY LIST
     C*
     C     FRKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCC
     C     *LIKE         DEFINE    FRCC          XXCC
     C*** BEG ADD *** 02/08/05 ****************************
     C     *LIKE         DEFINE    PMFIN         SBFIN
     C*** END ADD *** 02/08/05 ****************************
     C*
     C                   MOVE      *BLANK        SCQUT#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              50
     C                   MOVE      *BLANKS       QQDATA           80
     C                   MOVEL     CONAME        QQDATA
     C                   MOVE      '0040'        QQSIZE            4
     C                   CALL      'SRCNTR'
     C                   PARM                    QQDATA
     C                   PARM                    QQSIZE
     C                   MOVEL     QQDATA        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*
     C*   LOAD LINE 24 TABLES
     C*
     C     '11'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L124(1)
     C     '12'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L124(2)
     C     '2A'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L2A4(1)
     C     *IN09         IFEQ      *OFF
     C     '21'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L224(1)
     C     *IN09         IFEQ      *ON
     C     '22'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L224(1)
     C     '23'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L224(2)
     C     *IN09         IFEQ      *ON
     C     '31'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L324(1)
     C     *IN09         IFEQ      *OFF
     C     '32'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L324(1)
     C     *IN09         IFEQ      *OFF
     C     '33'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L324(2)
     C     *IN09         IFEQ      *OFF
     C     '34'          LOOKUP    TABL24        TABL2D                   50
     C                   END
     C                   MOVE      TABL2D        L324(3)
     C     '41'          LOOKUP    TABL24        TABL2D                   50
     C                   MOVE      TABL2D        L424(1)
     C                   MOVE      L124(1)       SC124
      *** BEG DEL *** 8/11/98 **************************************
     C*******              MOVE L2A4,1    SC2A24
      *** END DEL *** 8/11/98 **************************************
     C                   MOVE      L224(1)       SC224
     C                   MOVE      L324(1)       SC324
     C                   MOVE      L424(1)       SC424
     C                   Z-ADD     1             I1                3 0
     C                   Z-ADD     1             I2                3 0
     C                   Z-ADD     1             I2A               3 0
     C                   Z-ADD     1             I3                3 0
     C                   Z-ADD     1             I4                3 0
     C**** ADD 03/27/07 ************************************************
      **
     C*** IF PARMS PASSED IN INCLUDE THE KEY THEN DO THIS PART AND GO DIRECTLY
     C***    TO THE POLICY
     C     Q1PRMS        IFGT      2
     **
     C                   MOVEL     QQKY1         XKEY
     **
     C                   MOVEL     XPOL#         SCPOL#
     *** GO TO SCREEN 1 EDITS AND CONTINUE NORMAL PROCESSING.
     C                   GOTO      SCR01T
     **
     C                   ENDIF
     C*
      **
     C**** END ADD 03/27/07 ********************************************
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
      *** BEG DEL *** 8/11/98 **************************************
     C****       Q2RECF    IFEQ 'SCR02A'
     C****                 EXFMTSCR02A
     C****                 END
      *** END DEL *** 8/11/98 **************************************
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
      *** BEG DEL *** 8/11/98 **************************************
     C********   Q2RECF    IFEQ 'SCR02A'
     C********             MOVEL'SCR02 '  Q2RECF
     C********             END
      *** END DEL *** 8/11/98 **************************************
     C     Q2RECF        IFEQ      'SCR03'
      *** BEG CHG *** 8/11/98 **************************************
     C*********            MOVEL'SCR02A'  Q2RECF
     C                   MOVEL     'SCR02 '      Q2RECF
      *** END CHG *** 8/11/98 **************************************
     C                   END
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCNEW
     C                   MOVE      *BLANK        SCQUT#
     C                   GOTO      STRT
     C                   END
     C*
     C*** Begin add *** 02/12/02 **************************************
     C*  Function 4 = Delete
     C*
     C     *INKD         IFEQ      '1'
     C*
     C*    If a claim exists on this policy, do not allow the deletion.
     C*
     C*** BEG CHG *** 02/08/05 **************************************
     C*******    PMKEY     CHAINPCM01                99
     C     PMKEY         CHAIN(N)  PCM01                              99
     C*** END CHG *** 02/08/05 **************************************
     C     *IN99         IFEQ      '0'
     C                   EXFMT     WDW01
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*    Display window to make user confirm the deletion.
     C*
     C     *IN99         DOUEQ     '0'
     C                   EXFMT     WDW02
     C*
     C     *INKD         IFEQ      '1'
     C*
     C                   MOVE(P)   UPCO#         QQCO#
     C                   MOVE(P)   SCPOL#        QQPOL#
     C                   CALL      'C0030R04'
     C                   PARM                    QQCO#
     C                   PARM                    QQPOL#
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCNEW
     C                   MOVE      *BLANK        SCQUT#
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  *INKD = *ON
     C*** End add ***** 02/12/02 **************************************
     C*
     C*  FUNCTION 05 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#           13
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C***** IF USER SELECTS A POLICY # THEN GO DIRECTLY TO SCREEN 2
     C*****  SINCE CANCELLATION/REINSTATEMENT (CMD-12) IS NOT ALLOWED.
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH LIST CALL - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C     '?'           SCAN      SCAGCO        XXWRK1            3 0    58
     C     '?'           SCAN      SCAGRC        XXWRK1                   59
     C     *IN58         IFEQ      *ON
     C     *IN59         OREQ      *ON
      *
     C     *IN58         IFEQ      *ON
     C                   CLEAR                   SCAGCO
     C                   END
     C     *IN59         IFEQ      *ON
     C                   CLEAR                   SCAGRC
     C                   END
      *
     C                   Z-ADD     *ZERO         QQACC#            5 0
     C                   CALL      'C0623R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQACC#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQACC#        OREQ      *ZERO
     C     *IN58         IFEQ      *ON
     C                   MOVE      *ON           *IN41
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      *ON           *IN43
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     *IN58         IFEQ      *ON
     C                   MOVE      *ON           *IN42
     C                   MOVE      QQACC#        SCAGCO
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      *ON           *IN44
     C                   MOVE      QQACC#        SCAGRC
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME, SCR01
     C*                CALL ADDITIONAL INTEREST MAINTENANCE, SCR02
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       QQPOL#
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C*
     C*  CALL ADDITION INTEREST MAINTENANCE - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   CALL      'C0130R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  SEARCH UNDERWRITERS - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      *BLANK        QQUNCD            3
     C                   MOVEL     SCUNCD        QQUNCD
     C                   CALL      'C0507R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQUNCD
     C                   PARM                    QQRTC             1
     C                   MOVE      QQUNCD        SCUNCD
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     QQUNCD        OREQ      *BLANK
     C                   MOVE      '1'           *IN47
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 9 = ADDITIONAL INTEREST SEARCH
     C*
     C     *INKI         IFEQ      '1'
     C*
     C*  FUNCTION 09 = DBA NAME SEARCH  SCREEN 1
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVE      SCPOL#        QQPOL#                          IN QUESTION
     C                   CALL      'C0533R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      *BLANK        QQADI             2
     C                   MOVE      'C'           QQTYPE            1
     C                   CALL      'C0630R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQADI
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*   SEARCH PRODUCER - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C     '?'           SCAN      SCPRCO        XXWRK1                   58
     C     '?'           SCAN      SCPRRC        XXWRK1                   59
     C     *IN58         IFEQ      *ON
     C                   MOVE      SCAGCO        XXACC#
     C                   Z-ADD     35            A                 2 0
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      SCAGRC        XXACC#
     C                   Z-ADD     36            A
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   CHECK FOR VALID AGENT NUMBER
     C*
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL SEARCH
     C*
     C                   MOVE      *BLANK        QQPROD            3
     C                   MOVE      AGFEIN        QQFEIN            9
     C                   MOVE      AGMOD         QQMOD             3
     C                   CALL      'C0622R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQPROD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPROD        OREQ      *BLANKS
     C     *IN58         IFEQ      *ON
     C                   MOVE      *BLANK        SCPRCO
     C                   MOVE      '1'           *IN42
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      *BLANK        SCPRRC
     C                   MOVE      '1'           *IN44
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     *IN58         IFEQ      *ON
     C                   MOVE      QQPROD        SCPRCO
     C                   MOVE      '1'           *IN43
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      QQPROD        SCPRRC
     C                   MOVE      '1'           *IN45
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
      *
     C*
     C* FUNCTION 10 = TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*   COUNTY CODE SEARCH
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     SCCNTY        IFEQ      '? '
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCCNTY        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C2221R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCCNTY
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN15
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN16
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      *BLANKS       XXQ               2
     C*
     C                   MOVEL     SCBLCD        XXQ               2
     C     XXQ           IFEQ      '? '
     C     XXQ           OREQ      ' ?'
     C*
     C                   MOVE      *BLANK        XXKEY
     C*                    MOVELSCBLCD    XXKEY
     C                   CALL      'C2021R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCBLCD
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN47
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   CHECK FOR TABLE SEARCH IN COVERAGE CODES
     C*
     C                   Z-ADD     1             X                 2 0
     C     '? '          LOOKUP    SA(X)                                  50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2001R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SA(X)
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   END
     C                   GOTO      STRT
     C                   END
     C***************************************
     C*  FUNCTION 14 - REMARKS SYSTEM       *
     C***************************************
     C     *INKN         IFEQ      '1'
     C*
     C*** ONLY ALLOW ACCESS TO REMARKS SYSTEM IN UPDATE MODE
     C     SCMODE        IFEQ      UA(2)
     C*
     C*** RETAIN SCREEN NUMBER
     C                   MOVE      Q2RECF        HLRECF           10
     C*
     C*** ESTABLISH KEY
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      UPCO#         QCCO#
     C                   MOVE      SCPOL#        QCPOL#
     C*
     C*** CALL REMARKS PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      HLRECF        Q2RECF
     C                   END                                                    END EQ UA,2
     C                   GOTO      STRT
     C                   END                                                    END *INKN
     C*
     C*         FUNCTION 13 = CHANGE USER PARAMETERS
     C*          -----------------------------------------------------
     C     *INKM         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR03'
     C                   CALL      'C0006R'
     C     UPKEY         CHAIN     PUP01                              50
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 15 = SEARCH QUOTE LOG FILE - QUOTE ORDER - SCREEN 1
     C*
     C     *INKP         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQQUT#            7 0
     C                   MOVE      SCQUT#        QQQUT#
     C                   CALL      'C0520R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQQUT#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQQUT#        SCQUT#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQQUT#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 17 = SEARCH QUOTE LOG FILE BY NAME, SCR01
     C*
     C     *INKR         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       QQQUT#
     C                   CALL      'C0521R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQQUT#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQQUT#        SCQUT#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCQUT#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C                   END
     C*
     C*  FUNCTION 17 - PREMIUM VERIFICATION EDIT - SCREEN 4
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      *BLANK        QQPRMQ            8
     C                   MOVE      *BLANK        QQPRMP            8
     C                   MOVE      *BLANK        QQERR             1
     C                   CALL      'C0031R'
     C                   PARM                    QQCO#
     C                   PARM                    QQPOL#
     C                   PARM                    QQPRMQ
     C                   PARM                    QQPRMP
     C                   PARM                    QQERR
     C*
     C     QQERR         IFEQ      '1'
     C                   MOVEL     MSG(2)        SCMSG4
     C                   MOVEL     MSG(3)        SCQMSG
     C                   MOVEL     MSG(4)        SCMSGP
     C                   MOVE      QQPRMQ        SCPRMQ
     C                   MOVE      QQPRMP        SCPRMP
     C                   ELSE
     C                   MOVEL     MSG(1)        SCMSG4
     C                   MOVE      *BLANK        SCQMSG
     C                   MOVE      *BLANK        SCMSGP
     C                   MOVE      *BLANK        SCPRMQ
     C                   MOVE      *BLANK        SCPRMP
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 18 - AUTO FORMS SELECTION - SCREEN 4
     C*
     C     *INKS         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR04'
     C     PMAUD         IFNE      'N'
     C                   MOVE      '1'           *IN21
     C                   ELSE
     C                   MOVE      *BLANKS       QQPRD2           40
     C                   MOVEL     XXPRD         QQPRD2
     C                   MOVE      *BLANK        QQCAL2            1
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   CALL      'C7025R'
     C                   PARM                    QQPRD2           40
     C                   PARM                    QQCAL2            1
     C                   PARM                    QQPOL#
     C                   END
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  FUNCTION 19 - A/R INQUIRY
     C*
     C     *INKT         IFEQ      *ON
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0180R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   FUNCTION 10 - PROCESS HELP REQUESTS
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
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C*  FUNCTION 23 - PRODUCER ALPHA SEARCH
     C*---------------------------------------------------------------
     C     *INKX         IFEQ      *ON
     C     '?'           SCAN      SCPRCO        XXWRK1                   58
     C     '?'           SCAN      SCPRRC        XXWRK1                   59
     C     *IN58         IFEQ      *ON
     C                   MOVE      SCAGCO        XXACC#
     C                   Z-ADD     35            A                 2 0
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      SCAGRC        XXACC#
     C                   Z-ADD     36            A
     C                   ELSE
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   CHECK FOR VALID AGENT NUMBER
     C*
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      '1'           *IN(A)
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        QQPROD            3
     C                   MOVE      AGFEIN        QQFEIN            9
     C                   MOVE      AGMOD         QQMOD             3
     C                   CALL      'C0625R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQPROD
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPROD        OREQ      *BLANKS
     C     *IN58         IFEQ      *ON
     C                   MOVE      *BLANK        SCPRCO
     C                   MOVE      '1'           *IN42
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      *BLANK        SCPRRC
     C                   MOVE      '1'           *IN44
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     *IN58         IFEQ      *ON
     C                   MOVE      QQPROD        SCPRCO
     C                   MOVE      '1'           *IN43
     C                   ELSE
     C     *IN59         IFEQ      *ON
     C                   MOVE      QQPROD        SCPRRC
     C                   MOVE      '1'           *IN45
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  FUNCTION 24 - CHANGE COMMAND LINE
     C*---------------------------------------------------------------
     C     *INKY         IFEQ      *ON
     C                   MOVEL     Q2RECF        SCHOLD            5
     C     Q2CPOS        DIV       256           #ROW
     C                   MVR                     #COL
     C*
     C*          SCREEN 1 LINE 24
     C*          -----------------------------------------------------
     C     Q2RECF        IFEQ      'SCR01'
     C                   ADD       1             I1
     C     L124(I1)      IFEQ      *BLANKS
     C                   MOVE      L124(1)       SC124
     C                   Z-ADD     1             I1
     C                   ELSE
     C                   MOVE      L124(I1)      SC124
     C                   END
     C                   END
     C*
     C*          SCREEN 2 LINE 24
     C*          -----------------------------------------------------
     C     Q2RECF        IFEQ      'SCR02'
     C                   ADD       1             I2
     C     L224(I2)      IFEQ      *BLANKS
     C                   MOVE      L224(1)       SC224
     C                   Z-ADD     1             I2
     C                   ELSE
     C                   MOVE      L224(I2)      SC224
     C                   END
     C                   END
     C*
     C*          SCREEN 2A LINE 24
     C*          -----------------------------------------------------
      *** BEG DEL *** 8/11/98 **************************************
     C*******    Q2RECF    IFEQ 'SCR02A'
     C*******              ADD  1         I2A
     C*******    L2A4,I2A  IFEQ *BLANKS
     C*******              MOVE L2A4,1    SC2A24
     C*******              Z-ADD1         I2A
     C*******              ELSE
     C*******              MOVE L2A4,I2A  SC2A24
     C*******              END
     C*******              END
      *** END DEL *** 8/11/98 **************************************
     C*
     C*          SCREEN 3 LINE 24
     C*          -----------------------------------------------------
     C     Q2RECF        IFEQ      'SCR03'
     C                   ADD       1             I3
     C     L324(I3)      IFEQ      *BLANKS
     C                   MOVE      L324(1)       SC324
     C                   Z-ADD     1             I3
     C                   ELSE
     C                   MOVE      L324(I3)      SC324
     C                   END
     C                   END
     C*
     C*          SCREEN 4 LINE 24
     C*          -----------------------------------------------------
     C     Q2RECF        IFEQ      'SCR04'
     C                   ADD       1             I4
     C     L424(I4)      IFEQ      *BLANKS
     C                   MOVE      L424(1)       SC424
     C                   Z-ADD     1             I4
     C                   ELSE
     C                   MOVE      L424(I4)      SC424
     C                   END
     C                   END
     C*
     C*          FUNCTION 24 EXIT
     C*          -----------------------------------------------------
     C     C24X          TAG
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     SCHOLD        Q2RECF
     C                   SETON                                        59
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END REACTIVE
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02A'      SCR2AT
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   VALIDATE VALUE IN NEW POLICY FIELD
     C*
     C     SCNEW         IFNE      'Y'
     C     SCNEW         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK IF NEW OR UPDATE POLICY
     C*
     C     SCNEW         IFEQ      'Y'
     C*
     C*   POLICY NUMBER CANOT BE BLANKS USER MUST ENTER.
     C*
     C     SCPOL#        IFEQ      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*** Begin dlt *** 08/25/01 **************************************
     C**********           MOVE SCPOL#    POLCHK
     C********** CHK1      IFNE 'CPP'
     C********** CHK1      ANDNE'GL-'
     C********** CHK1      ANDNE'CA-'
     C********** CHK1      ANDNE'CF-'
     C********** CHK1      ANDNE'IM-'
     C********** CHK1      ANDNE'BU-'
     C********** CHK1      ANDNE'FR-'
     C********** CHK1      ANDNE'GA-'
     C********** CHK1      ANDNE'AL-'
     C**********           MOVE '1'       *IN29
     C**********           GOTO STRT
     C**********           END
     C*
     C**********           MOVE CHK1      SCPLTY  3
     C*
     C********** CHK2      IFNE 'A'
     C**********           MOVE '1'       *IN30
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 08/25/01 **************************************
     C*
     C*   EDITS HAVE BEEN PASSED FOR A NEW POLICY PULL ACROSS QUOTE
     C*   LOG INFORMATION AND BLANK OUT REST OF FIELDS
     C*
     C*   CREATE NEW POLICY NUMBER
     C*
     C                   MOVEL     SCQUT#        SCQUT#            7 0
     C*
     C*   VALIDATE THAT THE GENERATED POLICY NUMBER DOES NOT EXIST
     C*
     C     PMKEY         CHAIN     PPM01                              60
     C*
     C     *IN60         IFNE      '1'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      '0'           *IN52
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDBA
     C                   MOVE      *BLANKS       SCFIN
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCCNTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVEL     *BLANKS       SCZIP1
     C                   MOVE      *BLANKS       SCZIP2
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCPFEE
     C                   MOVE      *BLANKS       SCQPRM
     C                   MOVE      *BLANKS       SCORT
     C                   MOVE      *BLANK        SCTPOL
     C                   MOVE      *BLANKS       SCMPRM
     C                   MOVE      *BLANKS       SCYIB2
     C                   MOVE      *BLANKS       SCNOB
     C                   MOVE      *BLANK        SCACN
     C                   MOVE      *BLANK        SCARAC
     C                   MOVEL     *BLANK        SCPHA1
     C                   MOVE      *BLANK        SCPHA2
     C                   MOVE      *BLANK        SCINM
     C                   MOVE      *BLANK        SCARIN
     C                   MOVEL     *BLANK        SCPHI1
     C                   MOVE      *BLANK        SCPHI2
     C                   MOVE      *BLANKS       SCEFDT
     C                   MOVE      *BLANK        SCEXDT
     C                   MOVE      *BLANK        SCCNDT
     C                   MOVE      *BLANKS       SCAGCO
     C                   MOVE      *BLANK        SCPRCO
     C                   MOVE      *BLANKS       SCANM
     C                   MOVE      *BLANKS       SCPNM
     C                   MOVE      *BLANK        SCAGRC
     C                   MOVE      *BLANK        SCPRRC
     C                   MOVE      *BLANK        SCDTRC
     C                   MOVE      *BLANK        SCBLCD
     C                   MOVE      *BLANK        SCACMM
     C                   MOVE      *BLANK        SCACYR
     C                   MOVE      *BLANKS       SCUNCD
     C                   MOVE      *BLANK        SCUNAS
     C*** Begin chg *** 08/25/01 **************************************
     C*******************  MOVE *BLANK    SCTYPO
     C                   MOVE      '10'          SCTYPO
     C*** End chg ***** 08/25/01 **************************************
     C                   MOVE      *BLANKS       SCCC01
     C                   MOVE      *BLANKS       SCCC02
     C                   MOVE      *BLANKS       SCCC03
     C                   MOVE      *BLANKS       SCCC04
     C                   MOVE      *BLANKS       SCCC05
     C                   MOVE      *BLANKS       SCCC06
     C                   MOVE      *BLANKS       SCCC07
     C                   MOVE      *BLANKS       SCCC08
     C                   MOVE      *BLANKS       SCCC09
     C                   MOVE      *BLANKS       SCCC10
     C                   MOVE      *BLANKS       SCCC11
     C                   MOVE      *BLANKS       SCCC12
     C                   MOVE      *BLANKS       SCCC13
     C                   MOVE      *BLANKS       SCCC14
     C                   MOVE      *BLANKS       SCCC15
     C                   MOVE      *BLANKS       SCCC16
     C                   MOVE      *BLANKS       SCCC17
     C                   MOVE      *BLANKS       SCCC18
     C                   MOVE      *BLANKS       SCCC19
     C                   MOVE      *BLANKS       SCCC20
     C*
     C*          SET UP TO LOAD FORMS TABLE
     C*          ------------------------------------------------------
     C                   CLEAR                   SCFR
     C                   CLEAR                   FRM
     C                   CLEAR                   FRD
     C                   MOVE      '**'          XXCC
     C     FRKY          CHAIN     PFR01                              50
     C                   Z-ADD     1             J                 3 0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   MOVEL     XXDATE        XXMMYY            5 0
     C*
     C*          READ FORMS MASTER FILE
     C*          ------------------------------------------------------
     C     *IN50         DOWEQ     *OFF
     C     FREFDT        IFLE      XXMMYY
     C                   Z-ADD     1             V                 3 0
     C*
     C*             FORM DATE IS ACTIVE
     C*             ---------------------------------------------------
     C     FRFORM        LOOKUP    FRM(V)                                 50
     C     *IN50         IFEQ      *ON
     C*
     C*                IF ALREADY ON TABLE - UPDATE DATE
     C*                ------------------------------------------------
     C                   MOVE      FREFDT        FRD(V)
     C                   ELSE
     C                   Z-ADD     1             V                 3 0
     C*
     C*                NOT ON TABLE - ADD IT
     C*                ------------------------------------------------
     C     '      '      LOOKUP    FRM(V)                                 50
     C     *IN50         IFEQ      *ON
     C                   MOVE      FRFORM        FRM(V)
     C                   MOVE      FREFDT        FRD(V)
     C                   ADD       1             J
     C                   END
     C                   END
     C                   END
     C     J             COMP      10                                 50
     C  N50FRKY          READE     PFR01                                  50
     C                   ENDDO
     C*
     C*             LOAD FORMS TO SCREEN
     C*             ---------------------------------------------------
     C                   DO        10            V
     C                   MOVEL     FRM(V)        SCFR(V)
     C     FRM(V)        IFNE      *BLANKS
     C                   MOVEL     FRD(V)        XXMMY4            4 0
     C                   MULT      100.01        XXMMY4
     C                   MOVE      XXMMY4        SCFR(V)
     C                   END
     C                   ENDDO
     C                   MOVEL     *BLANK        SCCOMM
     C                   MOVE      'Y'           SCAUD
     C                   Z-ADD     0             SCCMNR
     C                   MOVEA     Z0            *IN(80)
     C                   SETOFF                                       030506
     C*
     C                   ELSE
     C*
     C*   VALIDATE POLICY NUMBER FOR UPDATE MODE AND GET INFORMATION
     C*
     C     PMKEY         CHAIN     PPM01                              40
     C*
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C     PMCANC        IFEQ      'P'
     C                   MOVE      '1'           *IN05
     C                   ELSE
     C                   MOVE      '0'           *IN05
     C                   END
     C     PMCANC        IFEQ      'Q'
     C                   MOVE      '1'           *IN06
     C                   ELSE
     C                   MOVE      '0'           *IN06
     C                   END
     C*
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   EXCEPT    PPM01E
     C*
     C                   MOVE      *BLANK        FL
     C                   MOVE      '1'           *IN52
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      PMQUT#        SCQUT#
     C                   MOVE      PMDBA         SCDBA
     C                   MOVE      PMFIN         SCFIN
     C*** BEG ADD *** 02/08/05 ****************************
     C                   MOVE      PMFIN         SBFIN                          BEFORE
     C*** END ADD *** 02/08/05 ****************************
     C                   MOVE      PMADR1        SCADR1
     C                   MOVE      PMADR2        SCADR2
     C                   MOVE      PMCTY         SCCTY
     C                   MOVE      PMCNTY        SCCNTY
     C                   MOVE      PMST          SCST
     C                   MOVEL     PMZIP         SCZIP1
     C                   MOVE      PMZIP         SCZIP2
     C                   MOVE      PMFEIN        SCFEIN
     C                   MOVE      PMPFEE        SCPFEE
     C                   MOVE      PMORT         SCORT
     C                   MOVE      PMTPOL        SCTPOL
     C                   MOVE      PMMPRM        SCMPRM
     C                   MOVE      PMYIB         SCYIB2
     C                   MOVE      PMNOB         SCNOB
     C                   MOVE      PMACN         SCACN
     C                   MOVE      PMARAC        SCARAC
     C                   MOVEL     PMPHNA        SCPHA1
     C                   MOVE      PMPHNA        SCPHA2
     C                   MOVE      PMINM         SCINM
     C                   MOVE      PMARIN        SCARIN
     C                   MOVEL     PMPHNI        SCPHI1
     C                   MOVE      PMPHNI        SCPHI2
     C                   Z-ADD     PMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     PMEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   Z-ADD     PMCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C*
     C*   DISPLAY MESSAGE IF 'NON-RENEWAL' STATUS
     C*
     C     PMNRDT        IFNE      *ZEROS
     C     PMNRDT        ANDGT     PMRSDT
     C                   MOVE      *ON           *IN39
     C                   MOVE      PMNRCD        XXNRD2
     C                   MOVE      XXNRDS        SCNRDS
     C                   ELSE
     C                   MOVE      *OFF          *IN39
     C                   MOVE      *BLANKS       SCNRDS
     C                   END
     C                   MOVE      PMCACD        SCCACD
     C                   MOVE      PMAGCO        SCAGCO
     C*
     C     PMPRCO        IFEQ      *ZERO
     C                   MOVE      *BLANKS       SCPRCO
     C                   ELSE
     C                   MOVE      PMPRCO        SCPRCO
     C                   END
     C*
     C*   GET AGENT NAME
     C*
     C                   MOVE      SCAGCO        XXACC#
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         IFEQ      *ON
     C                   MOVE      *BLANKS       SCANM
     C                   ELSE
     C                   MOVE      *BLANKS       QQDATA           80
     C                   MOVEL     AGNAME        QQDATA
     C                   CALL      'SRRMVB'
     C                   PARM                    QQDATA
     C                   MOVEL     QQDATA        SCANM
     C*
     C*   GET SUB-PRODUCER NAME
     C*
     C                   MOVE      *BLANKS       SCPNM
     C     PMPRCO        IFNE      *ZEROS
     C                   MOVE      AGFEIN        XXFEIN
     C                   MOVE      AGMOD         XXMOD
     C                   MOVE      PMPRCO        XXPROD
     C     PRKEY         CHAIN     PPR01                              50
     C                   MOVE      *BLANKS       SCPNM
     C     *IN50         IFEQ      *OFF
     C     PRDBA         IFNE      *BLANKS
     C                   MOVEL     PRDBA         SCPNM
     C                   ELSE
     C                   MOVE      *BLANKS       QQLSN            56
     C                   MOVEL     PRLSN         QQLSN
     C                   MOVE      PRFSN         QQFSN            15
     C                   MOVE      *BLANKS       QQCDE             1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFSN
     C                   PARM                    QQCDE
     C                   MOVEL     QQLSN         SCPNM
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     PMAGRC        IFEQ      *ZERO
     C                   MOVE      *BLANKS       SCAGRC
     C                   ELSE
     C                   MOVE      PMAGRC        SCAGRC
     C                   END
     C*
     C     PMPRRC        IFEQ      *ZERO
     C                   MOVE      *BLANKS       SCPRRC
     C                   ELSE
     C                   MOVE      PMPRRC        SCPRRC
     C                   END
     C*
     C                   Z-ADD     PMDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRC
     C                   MOVE      PMBLCD        SCBLCD
      *
     C                   Z-ADD     PMACMM        SCACMM
     C                   Z-ADD     PMACYR        SCACYR
     C*** Begin add *** 08/25/01 **************************************
     C     PMACYR        IFNE      0
     C*** End add ***** 08/25/01 **************************************
     C     PMACCN        IFEQ      1
     C                   ADD       2000          SCACYR
     C                   ELSE
     C                   ADD       1900          SCACYR
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
      *
     C                   MOVE      PMUNCD        SCUNCD
     C                   MOVE      PMUNAS        SCUNAS
     C                   MOVE      PMTYPO        SCTYPO
     C                   MOVE      PMCC01        SCCC01
     C                   MOVE      PMCC02        SCCC02
     C                   MOVE      PMCC03        SCCC03
     C                   MOVE      PMCC04        SCCC04
     C                   MOVE      PMCC05        SCCC05
     C                   MOVE      PMCC06        SCCC06
     C                   MOVE      PMCC07        SCCC07
     C                   MOVE      PMCC08        SCCC08
     C                   MOVE      PMCC09        SCCC09
     C                   MOVE      PMCC10        SCCC10
     C                   MOVE      PMCC11        SCCC11
     C                   MOVE      PMCC12        SCCC12
     C                   MOVE      PMCC13        SCCC13
     C                   MOVE      PMCC14        SCCC14
     C                   MOVE      PMCC15        SCCC15
     C                   MOVE      PMCC16        SCCC16
     C                   MOVE      PMCC17        SCCC17
     C                   MOVE      PMCC18        SCCC18
     C                   MOVE      PMCC19        SCCC19
     C                   MOVE      PMCC20        SCCC20
      * IF ONLY ONE SECTION CODE, FLAG IT FOR REVIEW.
     C     SCCC01        IFNE      *BLANKS
     C     SCCC02        ANDEQ     *BLANKS
     C                   MOVE      'X'           SCFL01
     C                   ELSE
     C                   MOVE      ' '           SCFL01
     C                   END
      *
     C                   MOVE      PMFR01        SCFR01
     C                   MOVE      PMFR02        SCFR02
     C                   MOVE      PMFR03        SCFR03
     C                   MOVE      PMFR04        SCFR04
     C                   MOVE      PMFR05        SCFR05
     C                   MOVE      PMFR06        SCFR06
     C                   MOVE      PMFR07        SCFR07
     C                   MOVE      PMFR08        SCFR08
     C                   MOVE      PMFR09        SCFR09
     C                   MOVE      PMFR10        SCFR10
     C                   MOVEL     PMCOMM        SCCOMM
     C                   MOVE      PMAUD         SCAUD
     C                   MOVE      PMCMNR        SCCMNR
     C     QLKEY         CHAIN     PQL01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PQL01E
     C                   Z-ADD     QLPRMQ        SCQPRM
     C                   SETON                                        19
     C                   ELSE
     C                   Z-ADD     0             SCQPRM
     C                   END
     C*
     C*   PROTECT COVERAGE FIELDS THAT HAVE ALREADY BEEN PROCESSED
     C*
     C                   MOVEA     Z0            *IN(80)
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     20
     C     *BLANK        LOOKUP    SA(X)                              50
     C     *IN50         IFEQ      '1'
     C     X             ADD       79            R                 2 0          PROTECT IND.
     C                   MOVE      '1'           *IN(R)
     C*
     C                   ELSE
     C                   Z-ADD     20            X
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   GET POLICY AUXILLARY FILE RECORD OR Z-ADD ZERO TO ANNUAL PREM
     C*
     C     PMKEY         CHAIN     PPX01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         SCANPR
     C                   ELSE
     C                   Z-ADD     PXANPR        SCANPR
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR02B
     C*
     C*  NAMED INSURED CANNOT BE BLANK
     C*
     C     SCFIN         IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADDRESS CANNOT BE BLANK
     C*
     C     SCADR1        IFEQ      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  CLIENT CITY CANNOT BE BLANK
     C*
     C     SCCTY         IFEQ      *BLANK
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE COUNTY CODE MUST BE VALID
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCCNTY        IFNE      *BLANKS
     C*** End add ***** 08/25/01 **************************************
     C                   MOVEL     SCCNTY        XXKEY
     C     TBKEY         CHAIN     PCY01                              31
     C     *IN31         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C     CYSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*  THE STATE MUST BE VALID
     C*
     C     SCST          LOOKUP    ST                                     50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  ZIP CODE CANNOT BE BLANK
     C*
     C     SCZIP1        IFEQ      *ZERO
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  POLICY FEE RANGE IS 0 - $250
     C*
     C     SCPFEE        IFLT      0.00
     C     SCPFEE        ORGT      250.00
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*** Begin dlt *** 08/25/01 **************************************
     C*  QUOTED PREMIUM CANNOT BE ZERO
     C**********
     C********** SCQPRM    IFEQ 0.00
     C**********           MOVE '1'       *IN32
     C**********           GOTO STRT
     C**********           END
     C**********
     C*   COMMISSION RATE MUST BE FROM 0.01 THRU 999.99
     C**********
     C********** SCCMNR    IFLT 0.01
     C********** SCCMNR    ORGT 999.99
     C**********           MOVE '1'       *IN29
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 08/25/01 **************************************
     C*
     C*    ORGANIZATION TYPE MUST BE I, P, C, J, OR O
     C*
     C     SCORT         IFNE      'I'
     C     SCORT         ANDNE     'P'
     C     SCORT         ANDNE     'C'
     C     SCORT         ANDNE     'J'
     C     SCORT         ANDNE     'O'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*          TYPE OF POLICY MUST BE G, R, OR BLANK
     C*          ------------------------------------------------------
     C     SCTPOL        IFNE      'G'
     C     SCTPOL        ANDNE     'R'
     C     SCTPOL        ANDNE     ' '
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C*          MINIMUM PREMIUM REQUIRED IF TYPE OF POLICY G OR R
     C*          ------------------------------------------------------
     C     SCTPOL        IFEQ      'G'
     C     SCMPRM        ANDEQ     0
     C     SCTPOL        OREQ      'R'
     C     SCMPRM        ANDEQ     0
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C     SCR02B        TAG
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      'N'           SCEXC
     C                   MOVE      'N'           SCHIRE
     C                   MOVE      'N'           SCRETL
     C                   MOVE      'N'           SCWHOL
     C                   MOVE      'N'           SC1CRN
     C                   MOVE      'N'           SCTRCO
     C                   MOVEL     *BLANKS       SCNMCO
     C                   MOVEL     *BLANKS       SCCRGO
     C                   MOVE      'N'           SCTMC
     C                   MOVEL     *BLANKS       SCTMD
     C                   MOVE      'N'           SCHZD
     C                   MOVEL     *BLANKS       SCHZDD
     C                   MOVE      'N'           SCCTY1
     C                   MOVE      'N'           SCCTY2
     C                   MOVE      'N'           SCCTY3
     C                   MOVE      'N'           SCCTY4
     C                   MOVE      'N'           SCCTY5
     C                   MOVE      'N'           SCCTY6
     C                   MOVEL     *BLANKS       SCCTYO
     C                   MOVE      'N'           SCOUTF
     C                   MOVE      'N'           SCOUTR
     C                   MOVE      'N'           SCOUTO
     C                   MOVE      'N'           SCLDEV
     C                   MOVEL     *BLANKS       SCVEHD
     C                   MOVE      'N'           SCDTTR
     C                   MOVE      'N'           SCHST
     C                   MOVE      'N'           SCBTL
     C                   MOVEL     *BLANKS       SCBTLD
     C                   MOVE      'N'           SCPSG
     C                   MOVEL     *BLANKS       SCPSGD
     C                   MOVE      'N'           SCLOG
     C                   MOVE      'N'           SCLOGD
     C                   MOVE      'N'           SCHTR
     C                   MOVEL     *BLANKS       SCHTRD
     C                   MOVE      'N'           SCCPL
     C                   ELSE
     C                   EXCEPT    PPA01E
     C                   MOVE      PAEXC         SCEXC
     C                   MOVE      PAHIRE        SCHIRE
     C                   MOVE      PARETL        SCRETL
     C                   MOVE      PAWHOL        SCWHOL
     C                   MOVE      PA1CRN        SC1CRN
     C                   MOVE      PATRCO        SCTRCO
     C                   MOVEL     PANMCO        SCNMCO
     C                   MOVEL     PACRGO        SCCRGO
     C                   MOVE      PATMC         SCTMC
     C                   MOVEL     PATMD         SCTMD
     C                   MOVE      PAHZD         SCHZD
     C                   MOVEL     PAHZDD        SCHZDD
     C                   MOVE      PACTY1        SCCTY1
     C                   MOVE      PACTY2        SCCTY2
     C                   MOVE      PACTY3        SCCTY3
     C                   MOVE      PACTY4        SCCTY4
     C                   MOVE      PACTY5        SCCTY5
     C                   MOVE      PACTY6        SCCTY6
     C                   MOVEL     PACTYO        SCCTYO
     C                   MOVE      PAOUTF        SCOUTF
     C                   MOVE      PAOUTR        SCOUTR
     C                   MOVE      PAOUTO        SCOUTO
     C                   MOVE      PALDEV        SCLDEV
     C                   MOVEL     PAVEHD        SCVEHD
     C                   MOVE      PADTTR        SCDTTR
     C                   MOVE      PAHST         SCHST
     C                   MOVE      PABTL         SCBTL
     C                   MOVEL     PABTLD        SCBTLD
     C                   MOVE      PAPSG         SCPSG
     C                   MOVEL     PAPSGD        SCPSGD
     C                   MOVE      PALOG         SCLOG
     C                   MOVE      PALOGD        SCLOGD
     C                   MOVE      PAHTR         SCHTR
     C                   MOVEL     PAHTRD        SCHTRD
     C                   MOVE      PACPL         SCCPL
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   MOVE      '1'           *IN49
     C                   MOVE      *BLANK        Q2RECF
      *** BEG CHG *** 8/11/98 **************************************
     C**********           MOVEL'SCR02A'  Q2RECF
     C                   MOVEL     'SCR03 '      Q2RECF
      *** END CHG *** 8/11/98 **************************************
     C                   GOTO      STRT
     C     SCR2AT        TAG
     C     *IN09         CABEQ     '1'           SCR2AB
     C     PAKEY         CHAIN     PPA01                              60
     C                   MOVEL     UPCO#         PACO#
     C                   MOVEL     SCPOL#        PAPOL#
     C                   MOVE      SCEXC         PAEXC
     C                   MOVE      SCHIRE        PAHIRE
     C                   MOVE      SCRETL        PARETL
     C                   MOVE      SCWHOL        PAWHOL
     C                   MOVE      SC1CRN        PA1CRN
     C                   MOVE      SCTRCO        PATRCO
     C                   MOVEL     SCNMCO        PANMCO
     C                   MOVEL     SCCRGO        PACRGO
     C                   MOVE      SCTMC         PATMC
     C                   MOVEL     SCTMD         PATMD
     C                   MOVE      SCHZD         PAHZD
     C                   MOVEL     SCHZDD        PAHZDD
     C                   MOVE      SCCTY1        PACTY1
     C                   MOVE      SCCTY2        PACTY2
     C                   MOVE      SCCTY3        PACTY3
     C                   MOVE      SCCTY4        PACTY4
     C                   MOVE      SCCTY5        PACTY5
     C                   MOVE      SCCTY6        PACTY6
     C                   MOVEL     SCCTYO        PACTYO
     C                   MOVE      SCOUTF        PAOUTF
     C                   MOVE      SCOUTR        PAOUTR
     C                   MOVE      SCOUTO        PAOUTO
     C                   MOVE      SCLDEV        PALDEV
     C                   MOVEL     SCVEHD        PAVEHD
     C                   MOVE      SCDTTR        PADTTR
     C                   MOVE      SCHST         PAHST
     C                   MOVE      SCBTL         PABTL
     C                   MOVEL     SCBTLD        PABTLD
     C                   MOVE      SCPSG         PAPSG
     C                   MOVEL     SCPSGD        PAPSGD
     C                   MOVE      SCLOG         PALOG
     C                   MOVE      SCLOGD        PALOGD
     C                   MOVE      SCHTR         PAHTR
     C                   MOVEL     SCHTRD        PAHTRD
     C                   MOVE      SCCPL         PACPL
     C     *IN60         IFEQ      *ON
     C                   WRITE     PPA01
     C                   ELSE
     C                   UPDATE    PPA01
     C                   END
     C*
     C     SCR2AB        TAG
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR03T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR03B
     C*
     C*** Begin dlt *** 08/25/01 **************************************
      * INIT PROCESS DATE FOR DISPLAY ON SCR03
     C**********           Z-ADDUPPRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCPRDT
     C*** End dlt ***** 08/25/01 **************************************
      *
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C     SCEFDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCEFDT        SRDTI
HERE C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
     C*** Begin dlt *** 08/25/01 **************************************
     C********** SCMODE    IFEQ UA,1
     C********** XXEFDT    ANDNEUPPRDT
     C**********           MOVE *ON       *IN57
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 08/25/01 **************************************
     C*
     C*  EXPIRATION DATE MUST BE VALID
     C*
     C     SCEXDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   EXPIRATION DATE CANNOT BE LESS THAN EFFECTIVE DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT            7 0
     C     XXEFDT        IFGE      XXEXDT
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   AGENT NUMBER MUST BE VALID
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCAGCO        IFNE      *ZERO
     C     SCAGCO        ANDNE     *BLANK
     C*** End add ***** 08/25/01 **************************************
     C                   CLEAR                   NUM
     C                   MOVEA     SCAGCO        NUM
     C                   MOVE      *ON           *IN50
     C                   DO        5             I                 3 0
     C   50NUM(I)        LOOKUP    TABNUM                                 50
     C                   END
     C     *IN50         IFEQ      *OFF
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      SCAGCO        XXACC#
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         IFEQ      *ON
     C*** Begin dlt *** 08/25/01 **************************************
     C**********           CALL 'C0220R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           GOTO STRT
     C**********           END
     C********** AGSTAT    IFEQ 'I'
     C********** SCMODE    ANDEQUA,1
     C*** End dlt ***** 08/25/01 **************************************
     C*** Begin add *** 08/25/01 **************************************
     C     AGSTAT        OREQ      'I'
     C*** End add ***** 08/25/01 **************************************
     C                   SETON                                        1541
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   SUB-PRODUCER MUST BE VALID IF NOT BLANK
     C*
     C     SCPRCO        IFNE      *BLANK
     C                   CLEAR                   NUM
     C                   MOVEA     SCPRCO        NUM
     C                   MOVE      *ON           *IN50
     C                   DO        5             I                 3 0
     C   50NUM(I)        LOOKUP    TABNUM                                 50
     C                   END
     C     *IN50         IFEQ      *OFF
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      AGFEIN        XXFEIN
     C                   MOVE      AGMOD         XXMOD
     C                   MOVE      SCPRCO        XXPROD
     C     PRKEY         CHAIN     PPR01                              50
     C     *IN50         IFEQ      *ON
     C                   CALL      'C0220R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   GOTO      STRT
     C                   END
     C     PRSTAT        IFEQ      'I'
     C     SCMODE        ANDEQ     UA(1)
     C                   SETON                                        1642
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*   AGENT OF RECORD MUST BE VALID
     C*
     C     SCAGRC        IFNE      *BLANK
     C                   CLEAR                   NUM
     C                   MOVEA     SCAGRC        NUM
     C                   MOVE      *ON           *IN50
     C                   DO        5             I                 3 0
     C   50NUM(I)        LOOKUP    TABNUM                                 50
     C                   END
     C     *IN50         IFEQ      *OFF
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      SCAGRC        XXACC#
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         IFEQ      *ON
     C                   CALL      'C0220R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   GOTO      STRT
     C                   END
     C     AGSTAT        IFEQ      'I'
     C     SCMODE        ANDEQ     UA(1)
     C                   SETON                                        1743
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*   SUB-PRODUCER MUST BE VALID IF NOT BLANK
     C*
     C     SCPRRC        IFNE      *BLANK
     C                   CLEAR                   NUM
     C                   MOVEA     SCPRRC        NUM
     C                   MOVE      *ON           *IN50
     C                   DO        5             I                 3 0
     C     *IN50         IFEQ      *ON
     C     NUM(I)        LOOKUP    TABNUM                                 50
     C                   END
     C                   END
     C     *IN50         IFEQ      *OFF
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      AGFEIN        XXFEIN
     C                   MOVE      AGMOD         XXMOD
     C                   MOVE      SCPRRC        XXPROD
     C     PRKEY         CHAIN     PPR01                              50
     C     *IN50         IFEQ      *ON
     C                   CALL      'C0220R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   GOTO      STRT
     C                   END
     C     PRSTAT        IFEQ      'I'
     C     SCMODE        ANDEQ     UA(1)
     C                   SETON                                        1844
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*   VALIDATE DATE AND DATE MUST BE BETWEEN EFFECTIVE DATE AND
     C*   EXPIRATION DATE OR CANCELATION DATE IF LESS
     C*
     C     SCAGRC        IFNE      *BLANK
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTRC
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTRC            7 0
     C     SCCNDT        IFNE      *ZERO
     C     XXDTRC        IFLT      XXEFDT
     C*
     C     XXDTRC        ORGT      PMCNDT
     C*
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C     XXDTRC        IFLT      XXEFDT
     C     XXDTRC        ORGT      XXEXDT
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C*  THE BILLING SCHEDULE CODE MUST BE VALID
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCBLCD        IFNE      *BLANKS
     C*** End add ***** 08/25/01 **************************************
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCBLCD        XXKEY
     C     TBKEY         CHAIN     PBC01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   UNDERWRITERS CODE MUST BE VALID
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCUNCD        IFNE      *BLANKS
     C*** End add ***** 08/25/01 **************************************
     C     UPKEY2        CHAIN     PUP02                              50
     C     *IN50         CABEQ     '1'           STRT                     32
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   POLICY TYPE MUST BE VALID
     C*
     C     SCTYPO        LOOKUP    TABPTP        TABGRP                   33
     C     *IN33         IFNE      '1'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '0'           *IN33
     C                   END
     C*
     C*   ACCOUNTING MONTH/YEAR MUST BE VALID
     C*
     C*** Begin add *** 08/25/01 **************************************
     C     SCACMM        IFNE      0
     C*** End add ***** 08/25/01 **************************************
     C     SCACMM        IFLT      1
     C     SCACMM        ORGT      12
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 08/25/01 **************************************
     C                   END
     C*** End add ***** 08/25/01 **************************************
     C*
     C*   AUDIT FLAG MUST BE "Y" OR "N"
     C*
     C     SCAUD         IFNE      'Y'
     C     SCAUD         ANDNE     'N'
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE THE COVERAGE CODE FIELDS
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     20
     C     *BLANK        LOOKUP    SA(X)                              50
     C*
     C     X             ADD       79            P                 2 0
     C     *IN50         IFEQ      '1'
     C     *IN(P)        IFNE      '1'
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SA(X)         XXKEY
     C*
     C     TBKEY         CHAIN     PCV01                              50
     C*
     C     *IN50         IFEQ      '1'
     C     X             ADD       59            E                 2 0
     C                   MOVE      '1'           *IN(E)
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   ELSE
     C                   Z-ADD     20            X
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*   CANNOT HAVE DUPLICATE SECTIONS ATTACHED
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     20
     C     *BLANK        LOOKUP    SA(X)                              50
     C*
     C     *IN50         IFEQ      '1'
     C                   MOVEL     SA(X)         XXCKSA            2
     C                   Z-ADD     1             Z                 2 0
     C     XXCKSA        LOOKUP    SA(Z)                                  50
     C*
     C     *IN50         IFEQ      '1'
     C                   ADD       1             Z
     C     XXCKSA        LOOKUP    SA(Z)                                  50
     C     *IN50         IFEQ      '1'
     C     Z             ADD       59            Y                 2 0
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      '1'           *IN53
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   ELSE
     C                   Z-ADD     20            X
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C     SCNEW         IFEQ      'Y'
     C     QLKY1         SETGT     PQL01
     C                   READP     PQL01                                  60
     C  N60UPCO#         COMP      QLCO#                              6060
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             SCQUT#
     C                   ELSE
     C                   EXCEPT    PQL01E
     C     QLQUT#        ADD       1             SCQUT#
     C                   END
     C                   MOVE      UPCO#         QLCO#
     C                   MOVE      SCQUT#        QLQUT#
     C                   MOVE      SCFIN         QLFIN
     C                   MOVE      SCDBA         QLDBA
     C                   MOVE      SCADR1        QLADR1
     C                   MOVE      SCADR2        QLADR2
     C                   MOVE      SCCTY         QLCTY
     C                   MOVE      SCCNTY        QLCNTY
     C                   MOVE      SCST          QLST
     C                   MOVEL     SCZIP1        QLZIP
     C                   MOVE      SCZIP2        QLZIP
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLEFDT
     C                   MOVE      SCFEIN        QLFEIN
     C                   MOVE      SCAGCO        QLAGT#
     C                   MOVE      SCORT         QLORT
     C                   MOVE      SCNOB         QLNOB
     C                   MOVE      *BLANKS       QLYIB
     C                   MOVE      *BLANKS       QLNOB
     C                   MOVE      4             QLSTAC
     C                   MOVE      SCQPRM        QLPRMQ
     C                   MOVE      SCPFEE        QLPFEE
     C*** Begin chg *** 08/25/01 **************************************
     C******************** MOVE SCPLTY    QLPLTY
     C                   MOVE      SA(1)         QLPLTY
     C*** End chg ***** 08/25/01 **************************************
     C                   MOVE      *BLANKS       QLUNCD
     C                   MOVE      SCCC01        QLCC01
     C                   MOVE      SCCC02        QLCC02
     C                   MOVE      SCCC03        QLCC03
     C                   MOVE      SCCC04        QLCC04
     C                   MOVE      SCCC05        QLCC05
     C                   MOVE      SCCC06        QLCC06
     C                   MOVE      SCCC07        QLCC07
     C                   MOVE      SCCC08        QLCC08
     C                   MOVE      SCCC09        QLCC09
     C                   MOVE      SCCC10        QLCC10
     C                   MOVE      SCCC11        QLCC11
     C                   MOVE      SCCC12        QLCC12
     C                   MOVE      SCCC13        QLCC13
     C                   MOVE      SCCC14        QLCC14
     C                   MOVE      SCCC15        QLCC15
     C                   MOVE      SCCC16        QLCC16
     C                   MOVE      SCCC17        QLCC17
     C                   MOVE      SCCC18        QLCC18
     C                   MOVE      SCCC19        QLCC19
     C                   MOVE      SCCC20        QLCC20
     C                   MOVE      Q1USER        QLUSER
     C                   MOVE      Q1PGM         QLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLDT
     C                   Z-ADD     DSTIME        QLTIME
     C*
     C                   Z-ADD     *ZERO         QLQUTL
     C                   Z-ADD     *ZERO         QLQUTN
     C                   WRITE     PQL01
     C                   ELSE
     C     QLKEY         CHAIN     PQL01                              60
     C*
     C                   MOVE      UPCO#         QLCO#
     C                   MOVE      SCQUT#        QLQUT#
     C                   MOVE      SCFIN         QLFIN
     C                   MOVE      SCDBA         QLDBA
     C                   MOVE      SCADR1        QLADR1
     C                   MOVE      SCADR2        QLADR2
     C                   MOVE      SCCTY         QLCTY
     C                   MOVE      SCCNTY        QLCNTY
     C                   MOVE      SCST          QLST
     C                   MOVEL     SCZIP1        QLZIP
     C                   MOVE      SCZIP2        QLZIP
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLEFDT
     C                   MOVE      SCFEIN        QLFEIN
     C                   MOVE      SCAGCO        QLAGT#
     C                   MOVE      SCORT         QLORT
     C                   MOVE      SCNOB         QLNOB
     C                   MOVE      *BLANKS       QLYIB
     C                   MOVE      *BLANKS       QLNOB
     C                   MOVE      4             QLSTAC
     C                   MOVE      SCQPRM        QLPRMQ
     C                   MOVE      SCPFEE        QLPFEE
     C*** Begin chg *** 08/25/01 **************************************
     C******************** MOVE SCPLTY    QLPLTY
     C                   MOVE      SA(1)         QLPLTY
     C*** End chg ***** 08/25/01 **************************************
     C                   MOVE      *BLANKS       QLUNCD
     C                   MOVE      SCCC01        QLCC01
     C                   MOVE      SCCC02        QLCC02
     C                   MOVE      SCCC03        QLCC03
     C                   MOVE      SCCC04        QLCC04
     C                   MOVE      SCCC05        QLCC05
     C                   MOVE      SCCC06        QLCC06
     C                   MOVE      SCCC07        QLCC07
     C                   MOVE      SCCC08        QLCC08
     C                   MOVE      SCCC09        QLCC09
     C                   MOVE      SCCC10        QLCC10
     C                   MOVE      SCCC11        QLCC11
     C                   MOVE      SCCC12        QLCC12
     C                   MOVE      SCCC13        QLCC13
     C                   MOVE      SCCC14        QLCC14
     C                   MOVE      SCCC15        QLCC15
     C                   MOVE      SCCC16        QLCC16
     C                   MOVE      SCCC17        QLCC17
     C                   MOVE      SCCC18        QLCC18
     C                   MOVE      SCCC19        QLCC19
     C                   MOVE      SCCC20        QLCC20
     C                   MOVE      Q1USER        QLUSER
     C                   MOVE      Q1PGM         QLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QLDT
     C                   Z-ADD     DSTIME        QLTIME
     C*
     C                   Z-ADD     *ZERO         QLQUTL
     C                   Z-ADD     *ZERO         QLQUTN
     C     *IN60         IFEQ      *ON
     C                   WRITE     PQL01
     C                   ELSE
     C                   UPDATE    PQL01
     C                   END
     C                   END
     C*
     C*** BEG ADD *** 02/08/05 ****************************
     C*
     C*** IF NAME HAS CHANGED UPDATE THE CLAIM MASTER RCDS
     C*
     C     SBFIN         IFNE      SCFIN
     C     PMKEY         CHAIN     PCM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C                   MOVE      SCFIN         CMFIN
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   UPDATE    PCM01
     C*
     C     PMKEY         READE     PCM01                                  99
     C                   ENDDO
     C                   ENDIF
     C*** END ADD *** 02/08/05 ****************************
     C*
     C     PMKEY         CHAIN     PPM01                              40
     C                   MOVE      UPCO#         PMCO#
     C                   MOVE      SCPOL#        PMPOL#
     C                   MOVE      SCDBA         PMDBA
     C                   MOVE      SCFIN         PMFIN
     C                   MOVE      SCADR1        PMADR1
     C                   MOVE      SCADR2        PMADR2
     C                   MOVE      SCCTY         PMCTY
     C                   MOVE      SCCNTY        PMCNTY
     C                   MOVE      SCST          PMST
     C                   MOVEL     SCZIP1        PMZIP
     C                   MOVE      SCZIP2        PMZIP
     C                   MOVE      SCFEIN        PMFEIN
     C                   MOVE      SCPFEE        PMPFEE
     C                   MOVE      SCORT         PMORT
     C                   MOVE      SCTPOL        PMTPOL
     C                   MOVE      SCMPRM        PMMPRM
     C                   MOVE      SCYIB2        PMYIB
     C                   MOVE      SCNOB         PMNOB
     C                   MOVE      SCACN         PMACN
     C                   MOVE      SCARAC        PMARAC
     C                   MOVEL     SCPHA1        PMPHNA
     C                   MOVE      SCPHA2        PMPHNA
     C                   MOVE      SCINM         PMINM
     C                   MOVE      SCARIN        PMARIN
     C                   MOVEL     SCPHI1        PMPHNI
     C                   MOVE      SCPHI2        PMPHNI
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMEFDT
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMEXDT
     C                   Z-ADD     SCCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMCNDT
     C                   MOVE      SCCACD        PMCACD
     C                   MOVE      SCAGCO        PMAGCO
     C                   MOVE      SCPRCO        PMPRCO
     C                   MOVE      SCAGRC        PMAGRC
     C                   MOVE      SCPRRC        PMPRRC
     C                   Z-ADD     SCDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMDTRC
     C                   MOVE      SCBLCD        PMBLCD
      *
     C                   Z-ADD     SCACMM        PMACMM
     C                   MOVE      SCACYR        PMACYR
     C     SCACYR        IFGT      1999
     C                   MOVEL     1             PMACCN
     C                   ELSE
     C                   MOVEL     0             PMACCN
     C                   END
      *
     C                   MOVE      SCUNCD        PMUNCD
     C                   MOVE      SCUNAS        PMUNAS
     C                   MOVE      SCTYPO        PMTYPO
     C                   MOVE      SCCC01        PMCC01
     C                   MOVE      SCCC02        PMCC02
     C                   MOVE      SCCC03        PMCC03
     C                   MOVE      SCCC04        PMCC04
     C                   MOVE      SCCC05        PMCC05
     C                   MOVE      SCCC06        PMCC06
     C                   MOVE      SCCC07        PMCC07
     C                   MOVE      SCCC08        PMCC08
     C                   MOVE      SCCC09        PMCC09
     C                   MOVE      SCCC10        PMCC10
     C                   MOVE      SCCC11        PMCC11
     C                   MOVE      SCCC12        PMCC12
     C                   MOVE      SCCC13        PMCC13
     C                   MOVE      SCCC14        PMCC14
     C                   MOVE      SCCC15        PMCC15
     C                   MOVE      SCCC16        PMCC16
     C                   MOVE      SCCC17        PMCC17
     C                   MOVE      SCCC18        PMCC18
     C                   MOVE      SCCC19        PMCC19
     C                   MOVE      SCCC20        PMCC20
     C                   MOVE      SCFR01        PMFR01
     C                   MOVE      SCFR02        PMFR02
     C                   MOVE      SCFR03        PMFR03
     C                   MOVE      SCFR04        PMFR04
     C                   MOVE      SCFR05        PMFR05
     C                   MOVE      SCFR06        PMFR06
     C                   MOVE      SCFR07        PMFR07
     C                   MOVE      SCFR08        PMFR08
     C                   MOVE      SCFR09        PMFR09
     C                   MOVE      SCFR10        PMFR10
     C                   MOVEL     SCCOMM        PMCOMM
     C                   MOVE      SCAUD         PMAUD
     C                   MOVE      SCCMNR        PMCMNR
     C                   MOVE      Q1USER        PMUSER
     C                   MOVE      Q1PGM         PMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PMDT
     C                   Z-ADD     DSTIME        PMTIME
     C                   MOVE      SCQUT#        PMQUT#
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPM01
     C                   ELSE
     C                   MOVEL     *BLANK        PMPOLL
     C                   MOVEL     *BLANK        PMPOLN
     C                   Z-ADD     1             PMNEND
     C                   MOVE      *BLANK        PMCANC
     C                   MOVE      *BLANK        PMSTAT
     C                   Z-ADD     0             PMMEM#
     C                   Z-ADD     0             PMNRDT
     C                   Z-ADD     0             PMRSDT
     C                   MOVE      *BLANKS       PMNRCD
     C                   MOVE      *BLANKS       PMRSCD
     C                   MOVE      *BLANKS       PMPOPP
     C                   Z-ADD     0             PMCCDT
     C                   MOVE      *BLANK        PMCCFL
     C                   Z-ADD     0             PMNCDT
     C                   MOVE      *BLANK        PMNCFL
     C                   WRITE     PPM01
     C                   END
     C*
     C*   ADD POLICY AUXILLARY FILE RECORD IF NOT THERE
     C*
     C     PMKEY         CHAIN     PPX01                              40
     C     *IN40         IFEQ      '1'
     C*
     C                   MOVE      UPCO#         PXCO#
     C                   MOVE      SCPOL#        PXPOL#
     C                   Z-ADD     *ZERO         PXANPR
     C                   Z-ADD     *ZERO         PXENPR
     C                   Z-ADD     *ZERO         PXORPR
     C                   Z-ADD     *ZERO         PXADPR
     C                   Z-ADD     *ZERO         PXTLCM
     C                   Z-ADD     *ZERO         PXAAMT
     C                   Z-ADD     *ZERO         PXBAMT
     C                   Z-ADD     *ZERO         PXDAMT
     C                   Z-ADD     *ZERO         PXHAMT
     C                   Z-ADD     *ZERO         PXIAMT
     C                   Z-ADD     *ZERO         PXPAMT
     C                   Z-ADD     *ZERO         PXNAMT
     C                   Z-ADD     *ZERO         PXVAMT
     C                   Z-ADD     *ZERO         PXRAMT
     C                   Z-ADD     *ZERO         PXPFB
     C                   Z-ADD     *ZERO         PXPFP
     C                   MOVE      'A'           PXSTAT
     C                   MOVE      Q1USER        PXUSER
     C                   MOVE      Q1PGM         PXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PXDT
     C                   Z-ADD     DSTIME        PXTIME
     C*
     C                   WRITE     PPX01
     C                   END
     C*
     C     SCR03B        TAG
     C*
     C*   CALL APPROPIATE PROGRAM FOR EACH COVERAGE CODE ENTERED
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLE     20
     C     *BLANK        LOOKUP    SA(X)                              50
     C*
     C     X             ADD       79            P                 2 0
     C     *IN50         IFEQ      '1'
     C     *IN(P)        IFNE      '1'
     C     FL(X)         ORNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SA(X)         XXKEY
     C*
     C     TBKEY         CHAIN     PCV01                              50
     C*
     C     CVVAL         CASEQ     'L'           LIAB
     C     CVVAL         CASEQ     'A'           AUTO
     C     CVVAL         CASEQ     'I'           INLMAR
     C     CVVAL         CASEQ     'G'           GLASS
     C     CVVAL         CASEQ     'B'           BOILMC
     C     CVVAL         CASEQ     'C'           FIRE
     C     CVVAL         CASEQ     'T'           CRIME
     C     CVVAL         CASEQ     'F'           FIDLTY
     C     CVVAL         CASEQ     'O'           BUSOWN
     C     CVVAL         CASEQ     'R'           FARM
     C                   END
     C                   END
     C                   ELSE
     C*
     C                   Z-ADD     20            X
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   MOVEL     'SCR04'       Q2RECF
     C                   MOVE      *BLANK        SCMSG4
     C                   MOVE      *BLANK        SCQMSG
     C                   MOVE      *BLANK        SCMSGP
     C                   MOVE      *BLANK        SCPRMQ
     C                   MOVE      *BLANK        SCPRMP
     C                   GOTO      STRT
     C*
     C     SCR04T        TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C     SCPRMQ        IFEQ      SCPRMP
     C                   MOVE      *BLANK        SCPOL#
     C                   END
     C                   MOVE      *BLANK        SCNEW
     C                   MOVE      *BLANK        SCQUT#
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*   SUBROUTINES TO CALL THE APPROPIATE PROGRAM AFTER COVERAGE   *
     C*   HAS BEEN SELECTED AND VALIDATED BY PROGRAM                  *
     C*****************************************************************
     C*
     C*
     C*   CALL LIABALITY PROGRAM
     C*
     C     LIAB          BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0050R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL AUTO PROGRAM
     C*
     C     AUTO          BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0060R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL INLAND MARINE PROGRAM
     C*
     C     INLMAR        BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0070R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL GLASS PROGRAM
     C*
     C     GLASS         BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0080R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL BOILER MACHINERY PROGRAM
     C*
     C     BOILMC        BEGSR
     C                   ENDSR
     C*
     C*   CALL FIRE PROGRAM
     C*
     C     FIRE          BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0040R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL BURGLARY AND THIEFT PROGRAM
     C*
     C     CRIME         BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0100R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL FIDELITY AND FORGERY PROGRAM
     C*
     C     FIDLTY        BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0120R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL BUSINESSOWNER PROGRAM
     C*
     C     BUSOWN        BEGSR
     C                   MOVEL     SA(X)         QQCODE            2
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   CALL      'C0110R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCODE
     C                   ENDSR
     C*
     C*   CALL FARM & RANCH OWNERS PROGRAM
     C*
     C     FARM          BEGSR
     C                   ENDSR
     C*
     C*
     OPPM01     E            PPM01E
     OPQL01     E            PQL01E
     OPPA01     E            PPA01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
** TABLE Z0 - ZERO OUT PROTECT INDICATORS
00000000000000000000
** TABLES TABPTP & TABGRP - TYPE OF POLICY CODE TABLE
10MON
11MON
12MON
31SMP
32SMP
33SMP
34SMP
35SMP
36SMP
37SMP
38SMP
71CMP
72CMP
73CMP
74CMP
75CMP
76CMP
77CMP
78CMP
85NON
90NON
91NON
92NON
93NON
94NON
95NON
96NON
97NON
98NON
99NON
** MESSAGES ARRAY
       Premium Verified.  Press ENTER to Continue.
Quoted Premium and Premiums entered for Policy DO NOT MATCH.
Quoted Premium:
Total Premiums Entered:
**         1         2         3         4         5         6         7
11F1=End of job  F5=Policy search  F13=Change user parms F9=DBA search  F24=More
12F7=Named ins Search(Pol)  F15=Quote Search  F17=Named ins search(Qt)  F24=More
2AF1=End of job          F2=Prior screen
21F1=End of job   F2=Prior screen   F4=Delete   F7=Add addl insureds    F24=More
22F1=End of job      F2=Prior screen                                    F24=More
23F9=Search additional insureds   F10=Table search   F19=A/R Inquiry    F24=More
31F1=End of job           F2=Prior screen
32F1=End of job           F2=Prior screen                               F24=More
33F5=Agent search      F7="D/E ID#" Search      F9=Producer search      F24=More
34F10=Table search   F13=Change user parms   F23=Producer Alpha Search  F24=More
41F1=End of job    F2=Prior screen    F17=Premium verification    F18=Auto forms
**
 0123456789
