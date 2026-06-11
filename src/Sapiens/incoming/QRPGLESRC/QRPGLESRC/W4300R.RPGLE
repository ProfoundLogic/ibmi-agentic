     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4300R - CLAIMANT STAFF CODE CHANGE/ASSIGN
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 02/08/16 C:062 SUPPORT: 39838(114)                            *
     H*                Initialize SCALLF and back out 6/5/13 change.  *
     H*                                                               *
     H* 06/05/13 C:189 SUPPORT: 82273                                 *
     H*              - Screen field has a check box and is demanding  *
     H*                value in terms of 'YES'/'NO'                   *
     H*                                                               *
     H* 08/30/06 C:132 Project: 132WGI                                *
     H*              - Added selections for employer alpha name       *
     H*                range. Add edits for new range.                *
     H*              - Add new employer alpha selections to QQDATA    *
     H*                                                               *
     H* 08/22/03 C:15  Project: WC1138                                *
     H*                Added a selection for claim status.            *
     H*                                                               *
     H* 01/19/01 C:15  Support: WC065/25877                           *
     H*                Chgd pgm to prevent incorrect CO# from being   *
     H*                passed to the submitting pgm W4300C.           *
     H*                                                               *
     H* 08/06/99 C:61  PROJECT: WC577                                 *
     H*                Center screen title.                           *
     H*                                                               *
     H*  11/24/97  C:79  PROJECT: BASE FIX                            *
     H*                 -ADDED CODE TO MOVE UPOUTQ TO QQQUE           *
     H*                  IMMEDIATELY AFTER THE CHAIN TO PUP01         *
     H*                 -ADDED LOST TIME FIELD TO PROMPT SCREEN       *
     H*                                                               *
     H*                                                               *
     H*  11/07/97  C:79  PROJECT: BASE FIX                            *
     H*                  CHANGED FUND SEARCH TO CALL W0010R           *
     H*                                                               *
     H*  07/08/97  C:79  PROJECT: WC706                               *
     H*                  V3 CHANGES. CLAIM STAFF CODE CHANGES/ASSIGN  *
     H*****************************************************************
     H* INDICATOR USAGE --- SCR01                                     *
     H*****************************************************************
     H*  10  HELP REQUESTS                                            *
     H*  11  VALID FUNCTION KEYS                                      *
     H*  19  YOU CANNOT ENTER BOTH EMPLOYER AND CLAIMANT RANGES       *
     H*  20  "FROM" CLAIMANT IS NOT VALID                             *
     H*  21  "TO" CLAIMANT IS NOT VALID                               *
     H*  22  "FROM" CLAIMANT IS > "TO" CLAIMANT                       *
     H*  26  COUNTY CODE SCCN1 IS INVALID                             *
     H*  27  COUNTY CODE SCCN1 IS INACTIVE                            *
     H*  28  COUNTY CODE SCCN2 IS INVALID                             *
     H*  29  COUNTY CODE SCCN2 IS INVACTIVE                           *
     H*  30  COUNTY CODE SCCN3 IS INVALID                             *
     H*  31  COUNTY CODE SCCN3 IS INVACTIVE                           *
     H*  32  COUNTY CODE SCCN4 IS INVALID                             *
     H*  33  COUNTY CODE SCCN4 IS INVACTIVE                           *
     H*  34  COUNTY CODE SCCN5 IS INVALID                             *
     H*  58  COUNTY CODE SCCN5 IS INVACTIVE                           *
     H*  35  POSITION CURSOR TO SCFLET                                *
     H*  36  POSITION CURSOR TO SCTLET                                *
     H*  37  POSITION CURSOR TO SCCN1                                 *
     H*  38  POSITION CURSOR TO SCCN2                                 *
     H*  39  POSITION CURSOR TO SCCN3                                 *
     H*  40  POSITION CURSOR TO SCCN4                                 *
     H*  41  POSITION CURSOR TO SCCN5                                 *
     H*  42  POSITION CURSOR TO SCFADJ                                *
     H*  43  POSITION CURSOR TO SCTADJ                                *
     H*  44  POSITION CURSOR TO SCFSPV (FROM SUPERVISOR             ) *
     H*  45  POSITION CURSOR TO SCTSPV (TO SUPERVISOR               ) *
     H*  46  POSITION CURSOR TO SCFAMN (FROM ACCOUNT MANAGER        ) *
     H*  47  POSITION CURSOR TO SCTAMN (TO ACCOUNT MANAGER          ) *
     H*  48  POSITION CURSOR TO SCFUMN (FROM CLAIM MANAGER          ) *
     H*  49  POSITION CURSOR TO SCTUMN (TO CLAIM MANAGER            ) *
     H*  50  POSITION CURSOR TO SCFNSE (FROM CASE NURSE             ) *
     H*  51  POSITION CURSOR TO SCTNSE (TO CASE NURSE               ) *
     H*  52  POSITION CURSOR TO SCFFAD (FROM OUTSIDE ADJUSTER       ) *
     H*  53  POSITION CURSOR TO SCTFAD (TO OUTSIDE ADJUSTER         ) *
     H*  54  POSITION CURSOR TO SCFSDF (FROM SDF ADJUSTER           ) *
     H*  55  POSITION CURSOR TO SCTSDF (TO SDF ADJUSTER             ) *
     H*  56  POSITION CURSOR TO SCFESP (FROM EXCESS SUPERVISOR      ) *
     H*  57  POSITION CURSOR TO SCTESP (TO EXCESS SUPERVISOR        ) *
     H*  59  POSITION CURSOR TO SCLTFL (LOST TIME FLAG              ) *
     H*  60  GENERAL CHAIN INDICATOR                                  *
     H*  61  SCFADJ OR SCTADJ BLANK    (ADJUSTER                    ) *
     H*  62  SCTADJ IS INVALID         (ADJUSTER                    ) *
     H*  64  SCFSPV OR SCTSPV BLANK    (SUPERVISOR                  ) *
     H*  65  SCFAMN OR SCTAMN BLANK    (ACCOUNT MANAGER             ) *
     H*  66  SCTAMN IS INVALID         (ACCOUNT MANAGER             ) *
     H*  67  SCFUMN OR SCTUMN BLANK    (CLAIM MANAGER               ) *
     H*  68  SCTUMN IS INVALID         (CLAIM MANAGER               ) *
     H*  69  SCFNSE OR SCTNSE BLANK    (CASE NURSE                  ) *
     H*  70  SCTNSE IS INVALID         (CASE NURSE                  ) *
     H*  71  SCFFAD OR SCTFAD BLANK    (OUTSIDE ADJUSTER            ) *
     H*  72  SCTFAD IS INVALID         (OUTSIDE ADJUSTER            ) *
     H*  73  SCFSDF OR SCTSDF BLANK    (SDF ADJUSTER                ) *
     H*  74  SCTSDF IS INVALID         (SDF ADJUSTER                ) *
     H*  75  SCFESP OR SCTESP BLANK    (EXCESS ADJUSTER             ) *
     H*  76  SCTESP IS INVALID         (EXCESS ADJUSTER             ) *
     H*  77  INVALID SEARCH FIELD                                     *
     H*  78  SCALLF VALUE IS INVALID   (SELECT ALL FUNDS (Y/N)      ) *
     H*  79  POSITION CURSOR SCFEMP    ("FROM" EMPLOYER #           ) *
     H*  80  SCFEMP > SCTEMP           ("FROM" > "TO" EMPLOYER      ) *
     H*  81  POSITION CURSOR SCTEMP    ("TO" EMPLOYER #             ) *
     H*  82  SCFEMP CANNOT = 0         ("FROM" EMPLOYER CANT = O    ) *
     H*  83  PROMPT SCREEN HEADING     (ASSIGN OR CHANGE MODE       ) *
     H*  84  SCFADJ INVALID            (ASSIGN MODE                 ) *
     H*  85  SCFSPV INVALID            (ASSIGN MODE                 ) *
     H*  86  SCFAMN INVALID            (ASSIGN MODE                 ) *
     H*  87  SCFUNM INVALID            (ASSIGN MODE                 ) *
     H*  88  SCFNSE INVALID            (ASSIGN MODE                 ) *
     H*  89  SCFFAD INVALID            (ASSIGN MODE                 ) *
     H*  91  SCLTFL INVALID SEARCH FIELD                            ) *
     H*  92  POSITION CURSOR SCALLF    (ALL FUNDS FIELD             ) *
     H*  93  SCLTFL (WC30647- ENTER L,N, OR B                       ) *
     H*  96  SCFENM > SCTENM/OR INVALID ("FROM" > "TO" EMPLOYER     ) *
     H*  97  APLHA EMPLOYER & NUMERIC EMPLOYER RANGE ENTERED        ) *
     H*  98  VALIDATE THAT STAFF CODES WERE ENTERED                   *
     H*****************************************************************
     H* INDICATOR USAGE --- SCR02                                     *
     H*****************************************************************
     H*  20  POSITION CURSOR SCFD01 (SCREEN FUND 1                  ) *
     H*  21  POSITION CURSOR SCFD02 (SCREEN FUND 2                  ) *
     H*  22  POSITION CURSOR SCFD03 (SCREEN FUND 3                  ) *
     H*  23  POSITION CURSOR SCFD04 (SCREEN FUND 4                  ) *
     H*  24  POSITION CURSOR SCFD05 (SCREEN FUND 5                  ) *
     H*  25  POSITION CURSOR SCFD06 (SCREEN FUND 6                  ) *
     H*  26  POSITION CURSOR SCFD07 (SCREEN FUND 7                  ) *
     H*  27  POSITION CURSOR SCFD08 (SCREEN FUND 8                  ) *
     H*  28  POSITION CURSOR SCFD09 (SCREEN FUND 9                  ) *
     H*  29  POSITION CURSOR SCFD10 (SCREEN FUND 10                 ) *
     H*  30  SCFD01 INVALID         (SCREEN FUND 1                  ) *
     H*  31  SCFD02 INVALID         (SCREEN FUND 2                  ) *
     H*  32  SCFD03 INVALID         (SCREEN FUND 3                  ) *
     H*  33  SCFD04 INVALID         (SCREEN FUND 4                  ) *
     H*  34  SCFD05 INVALID         (SCREEN FUND 5                  ) *
     H*  35  SCFD06 INVALID         (SCREEN FUND 6                  ) *
     H*  36  SCFD07 INVALID         (SCREEN FUND 7                  ) *
     H*  37  SCFD08 INVALID         (SCREEN FUND 8                  ) *
     H*  38  SCFD09 INVALID         (SCREEN FUND 9                  ) *
     H*  39  SCFD10 INVALID         (SCREEN FUND 10                 ) *
     F*
     FW4300F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FWMEMP     IF   E             DISK
     FWMCMP     IF   E             DISK
     FWMCDL     IF   E           K DISK
     FWTCNP     IF   E             DISK
     FWMCXL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     RENAME(PCN01:PCN02)
      *

     D LET             S              1    DIM(26) CTDATA PERRCD(26)
     D FND             S             10    DIM(10) CTDATA PERRCD(1)             FUND TABLE
     D STCD            S             10    DIM(16) CTDATA PERRCD(1)             STAFF CODES
      *
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS                  INZ
     D  CC                     1     20
     D                                     DIM(5)                               COUNTY CODE ARRAY
     D  SCCN1                  1      4
     D  SCCN2                  5      8
     D  SCCN3                  9     12
     D  SCCN4                 13     16
     D  SCCN5                 17     20
     D                 DS                  INZ
     D  FD                     1     30
     D                                     DIM(10) ASCEND                       FUND ARRAY
     D  SCFD01                 1      3  0
     D  SCFD02                 4      6  0
     D  SCFD03                 7      9  0
     D  SCFD04                10     12  0
     D  SCFD05                13     15  0
     D  SCFD06                16     18  0
     D  SCFD07                19     21  0
     D  SCFD08                22     24  0
     D  SCFD09                25     27  0
     D  SCFD10                28     30  0
     D                 DS                  INZ
     D  ST                     1     48
     D                                     DIM(16) ASCEND                       STAFF CODES
     D  SCFADJ                 1      3
     D  SCTADJ                 4      6
     D  SCFSPV                 7      9
     D  SCTSPV                10     12
     D  SCFAMN                13     15
     D  SCTAMN                16     18
     D  SCFUMN                19     21
     D  SCTUMN                22     24
     D  SCFNSE                25     27
     D  SCTNSE                28     30
     D  SCFFAD                31     33
     D  SCTFAD                34     36
     D  SCFSDF                37     39
     D  SCTSDF                40     42
     D  SCFESP                43     45
     D  SCTESP                46     48
     D*** Begin add *** 08/22/03 **************************************
     D                 DS                  INZ
      ***** BEG CHG ***** 08/30/06 ************************************
     D************                            1  98 QQDATA
     D  QQDATA                 1    100
      ***** END CHG ***** 08/30/06 ************************************
     D  QQCN1                  1      4
     D  QQCN2                  5      8
     D  QQCN3                  9     12
     D  QQCN4                 13     16
     D  QQCN5                 17     20
     D  QQFADJ                21     23
     D  QQTADJ                24     26
     D  QQFSPV                27     29
     D  QQTSPV                30     32
     D  QQFAMN                33     35
     D  QQTAMN                36     38
     D  QQFUMN                39     41
     D  QQTUMN                42     44
     D  QQFNSE                45     47
     D  QQTNSE                48     50
     D  QQFFAD                51     53
     D  QQTFAD                54     56
     D  QQFSDF                57     59
     D  QQTSDF                60     62
     D  QQFESP                63     65
     D  QQTESP                66     68
     D  QQFD01                69     71
     D  QQFD02                72     74
     D  QQFD03                75     77
     D  QQFD04                78     80
     D  QQFD05                81     83
     D  QQFD06                84     86
     D  QQFD07                87     89
     D  QQFD08                90     92
     D  QQFD09                93     95
     D  QQFD10                96     98
      ***** BEG ADD ***** 08/30/06 ************************************
     D  QQFENM                99     99
     D  QQTENM               100    100
      ***** BEG ADD ***** 08/30/06 ************************************
      *
     D*** End add ***** 08/22/03 **************************************
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D DOTS            C                   CONST(' . . . . . . . . . .')
     D XXADJ           C                   CONST('Adjuster')
     D SCASSN          C                   CONST('Assign staff codes:')
     D SCCHNG          C                   CONST('Change staff codes:-
     D                                           From   To-
     D                                          ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKEY         KLIST
     C                   KFLD                    XXCO#                          C:04 12/16
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#                          C:04 12/16
     C                   KFLD                    XXFND                          C:04 12/16
      *
      *  TABLE FILE - FOR COUNTY CODE
      *
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
      *  CLAIM MASTER FILE
      *
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#                          C:04 12/16
     C                   KFLD                    XXFND
      *
      *  CLAIMANT DEMOGRAPHIC FILE
      *
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
      *
      *  CLAIMS DARTS INTAKE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFLAG            1            ASSIGN/CHANGE
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
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
      *****************************************************************
      *  LIKE DEFINES
      *
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    CMSS#         XXSS#
      *****************************************************************
      *  GET THE USER PARAMETER FILE
      *
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
     *** BEG ADD *** 11/24/97 *****************************************
     C                   MOVEL(P)  'QPRINT'      QQQUE            10
     *** END ADD *** 11/24/97 *****************************************
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     *** BEG ADD *** 11/24/97 *****************************************
     C                   MOVE      UPOUTQ        QQQUE            10
     *** END ADD *** 11/24/97 *****************************************
     C                   END
      *
      *  GET THE COMPANY RECORD
      *
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C     FNKEY         CHAIN     PFN01                              60
     *** BEG DEL *** 11/24/97 *****************************************
     C******************** MOVE UPFND     SCFND
     C******************** MOVE FNNAME    SCNAME
     *** END DEL *** 11/24/97 *****************************************
      *****************************************************************
      *  GET THE FLEXIBLE ADJUSTER HEADING FROM DARTS INTAKE
      *
     C     CXKY          CHAIN     PCX01                              60
      *
      ***************************************************************** -
      *
      * LOAD FLEXIBLE FIELD FROM CLAIM/DARTS CONTROL FILE
      *
      /If Not Defined (WEB)
     C                   MOVE(P)   DOTS          SCADJ
      /Endif
      *
     C     CXADJ         IFNE      *BLANK
     C     CXADJ         CAT       ' ':0         SCADJ
      *
     C                   ELSE
     C     XXADJ         CAT       ' ':0         SCADJ
      *
     C                   ENDIF
      *
      * SCREEN DISPLAY RULE: YOU CAN'T HAVE ONLY ONE DOT IN PROMPT TEXT
      *
     C     ' '           CHECKR    CXADJ         XXLEN             3 0
      *
     C     XXLEN         IFEQ      18
     C     XXLEN         OREQ      17
      *
     C                   MOVEL(P)  CXADJ         SCADJ
      *
     C                   END
      *****************************************************************
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   Z-ADD     *ZERO         SCTEMP
      ***** BEG ADD ******* 08/06/99 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*** Begin Add *** 02/08/16 **************************************
     C*
     C* Initialize the All group flag
     C*
     C                   Eval      scAllF = 'N'
     C*** End   Add *** 02/08/16 **************************************
      *
      ***** END ADD ******* 08/06/99 **********************************
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *
      *   DISPLAY CORRECT HEADING (ASSIGN OR CHANGE)
      *
     C                   SELECT
      *
     C     QQFLAG        WHENEQ    'A'
     C                   MOVEL(P)  SCASSN        SCHEAD
     C                   MOVE      *OFF          *IN83
      *
     C     QQFLAG        WHENEQ    'C'
     C                   MOVEL(P)  SCCHNG        SCHEAD
     C                   MOVE      *ON           *IN83
      *
     C                   ENDSL
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
      *
      *   PROCESS ALL FUNCTION KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *   FUNCTION 3 = EXIT
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *   FUNCTION 12= PRIOR SCREEN
      *
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *   FUNCTION 10= TABLE CODE SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
      *
      *   CLAIMANT SEARCH
      *
     C     HFLD          IFEQ      'SCFLET'
     C     HFLD          OREQ      'SCTLET'
      *
     C                   CALL      'W0070R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQMODE            1
     C                   PARM      *ZEROS        QQRRN             9 0
     C                   PARM      *BLANK        QQSORT            2
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM      *ZERO         QQFYR             3 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
      *  *INKL WAS NOT PRESSED. WAS A RECORD SELECTED
      *
     C     *INKL         IFEQ      *OFF
      *
     C     QQRRN         IFNE      *ZERO
      *
     C     QQRRN         CHAIN     WMCMP                              60
     C                   MOVE      CMSS#         XXSS#
      *
     C     CDKEY         CHAIN     PCD01                              60
      *
      *  LOAD THE NAME AND POSITION TO THE NEXT FIELD
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFLET'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CDLSN         SCFLET
     C                   MOVE      *ON           *IN36
      *
     C                   ELSE
     C                   MOVE      *ON           *IN35
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     HFLD          WHENEQ    'SCTLET'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CDLSN         SCTLET
     C                   MOVE      *ON           *IN79
      *
     C                   ELSE
     C                   MOVE      *ON           *IN36
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDSL
      *
     C                   ELSE
      *
      *  NO RECORDS FOUND SO RETURN AND POSITION TO CURRENT FIELD
      *
     C     HFLD          IFEQ      'SCFLET'
     C                   MOVE      *ON           *IN35
      *
     C                   ELSE
     C                   MOVE      *ON           *IN36
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  QQRRN IFNE
      *
     C                   ELSE                                                   HFLD IFEQ
      *
     C     HFLD          IFEQ      'SCFLET'
     C                   MOVE      *ON           *IN35
      *
     C                   ELSE
     C                   MOVE      *ON           *IN36
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD  IFEQ
      *
      *  EMPLOYER NUMBER SEARCH
      *
     C     HFLD          IFEQ      'SCFEMP'
     C     HFLD          OREQ      'SCTEMP'
      ***** BEG ADD ***** 08/30/06 ************************************
     C     HFLD          OREQ      'SCFENM'
     C     HFLD          OREQ      'SCTENM'
      ***** END ADD ***** 08/30/06 ************************************
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      0             QQRRN2            9 0
     C                   PARM                    QQQTE             1
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     *INKL         IFEQ      *ON
      *
      *  POSITION CURSOR TO CORRECT FIELD
      *
     C     HFLD          COMP      'SCFEMP'                               79
      *
     C     HFLD          COMP      'SCTEMP'                               81
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
      * LOAD EMPLOYER NUMBER
      *
     C     QQRRN         IFNE      *ZERO
      *
     C     QQRRN         CHAIN     WMEMP                              60
      *
     C     *IN60         IFEQ      *ZERO
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCFEMP'
      *
     C                   MOVEL     EMEMP#        SCFEMP
     C                   MOVE      *ON           *IN81
      *
     C     HFLD          WHENEQ    'SCTEMP'
      *
     C                   MOVEL     EMEMP#        SCTEMP
     C                   MOVE      *ON           *IN37
      *
      ***** BEG ADD ***** 08/30/06 ************************************
      * LOAD EMPLOYER NAME RANGE.
     C     HFLD          WHENEQ    'SCFENM'
      *
     C                   MOVEL     EMNAME        SCFENM
     C                   MOVE      *ON           *IN99
      *
     C     HFLD          WHENEQ    'SCTENM'
      *
     C                   MOVEL     EMNAME        SCTENM
     C                   MOVE      *ON           *IN99
      *
      ***** END ADD ***** 08/30/06 ************************************
      *
     C                   ENDSL
      *
     C                   ELSE
      *
      *  RECORD NOT FOUND SO POSITION TO CORRECT FIELD
      *
     C     HFLD          COMP      'SCFEMP'                               79
      *
     C     HFLD          COMP      'SCTEMP'                               81
      *
      ***** BEG ADD ***** 08/30/06 ************************************
     C     HFLD          COMP      'SCFENM'                               99
     C     HFLD          COMP      'SCTENM'                               99
      ***** END ADD ***** 08/30/06 ************************************
      *
     C                   ENDIF                                                  *IN60=ON
      *
     C                   ELSE
      *
      *  RECORD NOT FOUND SO POSITION TO CORRECT FIELD
      *
     C     HFLD          COMP      'SCFEMP'                               79
      *
     C     HFLD          COMP      'SCTEMP'                               81
      *
     C                   ENDIF                                                  QQRRN NE 0
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFEMP
      *
      * COUNTY CODE SEARCH
      *
     C     HFLD          IFEQ      'SCCN1'
     C     HFLD          OREQ      'SCCN2'
     C     HFLD          OREQ      'SCCN3'
     C     HFLD          OREQ      'SCCN4'
     C     HFLD          OREQ      'SCCN5'
      *
     C                   CALL      'W2215R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQMODE            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      *BLANKS       QQST              2
      *
     C     *INKC         CABEQ     *ON           TERM
      *
      *  F12 WAS NOT PRESSED.  IF A RECORD WAS SELECTED, LOAD VALUE
      *
     C     *INKL         IFEQ      *OFF
      *
     C     QQRRN         CHAIN     PCN01                              60
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCCN1'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNKEY         SCCN1
     C                   MOVE      *ON           *IN38
      *
     C                   ELSE
     C                   MOVE      *ON           *IN37
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     HFLD          WHENEQ    'SCCN2'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNKEY         SCCN2
     C                   MOVE      *ON           *IN39
      *
     C                   ELSE
     C                   MOVE      *ON           *IN38
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     HFLD          WHENEQ    'SCCN3'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNKEY         SCCN3
     C                   MOVE      *ON           *IN40
      *
     C                   ELSE
     C                   MOVE      *ON           *IN39
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     HFLD          WHENEQ    'SCCN4'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNKEY         SCCN4
     C                   MOVE      *ON           *IN41
      *
     C                   ELSE
     C                   MOVE      *ON           *IN40
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     HFLD          WHENEQ    'SCCN5'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNKEY         SCCN5
     C                   MOVE      *ON           *IN42
      *
     C                   ELSE
     C                   MOVE      *ON           *IN41
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDSL
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
      *  F12 WAS PRESSED. POSITION CURSOR TO CORRECT FIELD
      *
     C                   ELSE                                                   *INKL IFEQ
      *
     C     HFLD          COMP      'SCCN1'                                37
     C     HFLD          COMP      'SCCN2'                                38
     C     HFLD          COMP      'SCCN3'                                39
     C     HFLD          COMP      'SCCN4'                                40
     C     HFLD          COMP      'SCCN5'                                41
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   ENDIF                                                  HFLD=SCCN1
      ****************************************************************
      *   STAFF CODE SEARCH
      ****************************************************************
     C                   Z-ADD     42            Y
     C                   Z-ADD     1             X                 3 0
      *
      *  DETERMINE CURSOR LOCATION
      *
     C     HFLD          LOOKUP    STCD(X)                                60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   CALL      'S0005R'      UPRMPL
      *
      *  WAS A STAFF CODE SELECTED. POSITION CURSOR TO CORRECT FIELD
      *
     C     ##RRN         IFNE      *ZEROS
      *
     C     ##RRN         CHAIN     PUP03                              99
      *
     C                   MOVEL     UPSTAF        ST(X)
     C     X             ADD       Y             Z                 3 0
      *
      *  IF ASSIGN MODE SKIP THE "TO" FIELDS ON THE SCREEN
      *
     C     QQFLAG        IFEQ      'A'
     C     Z             IFEQ      53
      *
     C                   ADD       5             Z
      *
     C                   ELSE
     C                   ADD       1             Z
      *
     C                   ENDIF                                                  Z = 54      NK
      *
     C                   ENDIF                                                  QQFLAG=A    NK
      *
      *  IS THIS THE LAST FIELD ON THE SCREEN
      *
     C     Z             IFEQ      58
     C                   ADD       1             Z
     C                   ENDIF                                                  QQSTAF NE   NK
      *
     C                   MOVE      *ON           *IN(Z)
      *
      *  F12 OR ENTER WERE PRESSED POSITION TO THE CORRECT FIELD
      *
     C                   ELSE
     C     Y             SUB       1             Z
     C                   ADD       X             Z
     C                   MOVE      *ON           *IN(Z)
      *
     C                   ENDIF                                                  *INKL = OFF
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60= *ON
      *
      *  INVALID SEARCH FIELD
      *
     C     HFLD          IFEQ      'SCALLF'
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     *** BEG ADD *** 11/24/97 *****************************************
     C     HFLD          IFEQ      'SCLTFL'
     C                   MOVE      *ON           *IN91
     C                   ENDIF
     *** END ADD *** 11/24/97 *****************************************
     C*** Begin add *** 08/22/03 **************************************
     C     HFLD          IFEQ      'SCOCST'
     C                   MOVE      *ON           *IN94
     C                   ENDIF
     C*** End add ***** 08/22/03 **************************************
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C     Q2RECF        WHENEQ    'SCR02'
      *****************************************************************
      *  FUND SEARCH
      *****************************************************************
     C                   Z-ADD     1             X
      *
     C     HFLD          LOOKUP    FND(X)                                 60
      *
     *** BEG DEL *** 11/07/97 *****************************************
     C*****      *IN60     IFEQ *ON
     C*****                CALL 'W0510R'
     C*****                PARM           QQPRDS
     C*****                PARM           QQCALL
     C*****                PARM *ZEROS    QQKEY   30
     C*****                PARM           QQRTCD  1
     ******
     C*****      QQRTCD    CABEQ'A'       TERMCD  1
     C*****                PARM           QQRTCD  1
     C*****      QQRTCD    IFEQ 'B'       QQRTCD  1
     C*****      QQKEY     OREQ *ZEROS    QQRTCD  1
     *** END DEL *** 11/07/97 *****************************************
     **
     *** BEG ADD *** 11/07/97 *****************************************
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      XXCO#         ##CO#             3
      *
     C                   CALL      'W0010R'
     C     *INKC         PARM                    QQKC
     C     *INKL         PARM                    QQKL
     C                   PARM      '2'           QQCALL
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM      1             ##VIEW
      *
     C     ##FND         IFEQ      *BLANKS
     *** END ADD *** 11/07/97 *****************************************
      *
     C                   ADD       19            X
     C                   MOVE      *ON           *IN(X)
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  ##FND=BLANKS
      *
     *** BEG DEL *** 11/07/97 *****************************************
     C********   QQRTCD    IFEQ '*'
     C********   QQKEY     ANDNE*ZEROS
      ********
     C********             MOVE QQKEY     FD,X
     *** END DEL *** 11/07/97 *****************************************
      *
     *** BEG ADD *** 11/07/97 *****************************************
     C     ##FND         IFNE      *BLANK
      *
     C                   MOVE      ##FND         FD(X)
     *** END ADD *** 11/07/97 *****************************************
      *
     C                   ADD       20            X
      *
      *  IF NOT THE LAST FIELD SET POSITION CURSOR INDICATOR ON
      *
     C     X             IFNE      30
     C                   MOVE      *ON           *IN(X)
     C                   ENDIF
      *
     C                   ENDIF                                                  ##FND NE BLANKS
      *
     C                   ENDIF                                                  *IN60 = ON
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDSL                                                  Q2RECF=SCRO2
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *INKJ = *ON
      *****************************************************************
      *   PROCESS HELP REQUESTS
      *****************************************************************
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
     C                   MOVE      XXRECF        Q2RECF
      *
     C                   GOTO      STRT
     C                   END
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     *** BEG ADD *** 11/24/97 *****************************************
     C                   MOVEL(P)  'QPRINT'      QQQUE            10
     *** END ADD *** 11/24/97 *****************************************
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     *** BEG ADD *** 11/24/97 *****************************************
     C                   MOVE(P)   UPOUTQ        QQQUE            10
     *** END ADD *** 11/24/97 *****************************************
     C                   GOTO      TOP
     C                   END
      *
     C                   END
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      ****************************************************************
      *   PROCESS REQUEST SCREEN
      ****************************************************************
     C     SCR01T        TAG
      *
      *****************************************************************
      * VALIDATE THAT EMPLOYER AND CLAIMANT RANGES ARE BOTH NOT ENTERED
      *
     C                   MOVEA     CC            XXCC             20
      *
     C     SCTLET        IFNE      *BLANKS
     C     SCTEMP        ANDNE     *ZERO
      ***** BEG ADD ***** 08/30/06 ************************************
     C     SCTLET        ORNE      *BLANKS
     C     SCTENM        ANDNE     *BLANKS                                      EMP NAME RANGE
     C     SCTENM        ORNE      *BLANKS
     C     SCTEMP        ANDNE     *ZERO
      ***** END ADD ***** 08/30/06 ************************************
      *
     C                   MOVE      *ON           *IN19
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCTLET NE BLANK
      *****************************************************************
      * IF SELECTED, VALIDATE THE CLAIMANT NAME RANGE
      *
     C     SCTLET        IFNE      *BLANKS
      *
     C     LO:UP         XLATE(P)  SCFLET        SCFLET
     C     LO:UP         XLATE(P)  SCTLET        SCTLET
      *
     C                   Z-ADD     1             X
     C     SCFLET        LOOKUP    LET(X)                                 60
      *
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     1             Y                 3 0
     C     SCTLET        LOOKUP    LET(Y)                                 60
      *
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     X             IFGT      Y
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  SCTLET NE BLANK
      *
      *****************************************************************
      ***** BEG ADD ***** 08/30/06 ************************************
      * If Selected, validate the Empolyer Name Range
      *
     C     SCTENM        IFNE      *BLANKS
      *
     C     LO:UP         XLATE(P)  SCFENM        SCFENM
     C     LO:UP         XLATE(P)  SCTENM        SCTENM
      *
     C                   Z-ADD     1             X
     C     SCFENM        LOOKUP    LET(X)                                 60
      *
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     1             Y                 3 0
     C     SCTENM        LOOKUP    LET(Y)                                 60
      *
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN96
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     X             IFGT      Y
     C                   MOVE      '1'           *IN97
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF                                                  SCTENM NE BLANK
      ***** END ADD ***** 08/30/06 ************************************
      *****************************************************************
      * IF SELECTED, VALIDATE THE EMPLOYER NUMBER RANGE
      *
     C     SCTEMP        IFNE      *ZERO
      *
     C     SCFEMP        IFEQ      *ZERO
      *
     C                   MOVE      *ON           *IN82
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C     SCTEMP        IFLT      SCFEMP
     C                   MOVE      *ON           *IN80
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  SCFEMP = 0
      *
     C                   ENDIF                                                  SCTEMP NE 0
      *
      ***** BEG ADD ***** 08/30/06 ************************************
      *****************************************************************
      * If Selected, Validate the Employer Name Range
      *
     C     SCTENM        IFNE      *BLANKS
      *
     C     SCFENM        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN96
     C                   GOTO      STRT
     C                   ELSE
     C     SCTENM        IFLT      SCFENM
     C                   MOVE      *ON           *IN97
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF                                                  SCFENM = BLANK
      *
     C                   ENDIF                                                  SCTENM NE BLNK
      ***** END ADD ***** 08/30/06 ************************************
      *****************************************************************
      * IF SELECTED, VALIDATE THE COUNTY CODES
      *
     C     XXCC          IFNE      *BLANKS
      *
      *  COUNTY CODES MUST BE VALID AND ACTIVE
      *
     C     SCCN1         IFNE      *BLANKS
     C                   MOVEL(P)  SCCN1         XXKEY
      *
     C     TBKEY         CHAIN     PCN02                              26
      *
     C     *IN26         CABEQ     *ON           STRT
      *
     C     CNSTAT        CABEQ     'I'           STRT                     27
      *
     C                   ENDIF
      *
     C     SCCN2         IFNE      *BLANKS
     C                   MOVEL(P)  SCCN2         XXKEY
      *
     C     TBKEY         CHAIN     PCN02                              28
      *
     C     *IN28         CABEQ     *ON           STRT
      *
     C     CNSTAT        CABEQ     'I'           STRT                     29
      *
     C                   ENDIF
      *
     C     SCCN3         IFNE      *BLANKS
     C                   MOVEL(P)  SCCN3         XXKEY
      *
     C     TBKEY         CHAIN     PCN02                              30
      *
     C     *IN30         CABEQ     *ON           STRT
      *
     C     CNSTAT        CABEQ     'I'           STRT                     31
      *
     C                   ENDIF
      *
     C     SCCN4         IFNE      *BLANKS
     C                   MOVEL(P)  SCCN4         XXKEY
      *
     C     TBKEY         CHAIN     PCN02                              32
      *
     C     *IN32         CABEQ     *ON           STRT
      *
     C     CNSTAT        CABEQ     'I'           STRT                     33
      *
     C                   ENDIF
      *
     C     SCCN5         IFNE      *BLANKS
     C                   MOVEL(P)  SCCN5         XXKEY
      *
     C     TBKEY         CHAIN     PCN02                              34
      *
     C     *IN34         CABEQ     *ON           STRT
      *
     C     CNSTAT        CABEQ     'I'           STRT                     58
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  CC NE BLANKS
      *****************************************************************
      *
      *  VALIDATE STAFF CODES WERE ENTERED
      *
     C                   MOVEA     ST            ST1              48
     C     ST1           COMP      *BLANKS                                98
      *
     C     *IN98         CABEQ     *ON           STRT
      *
      *  FROM & TO CLAIMS EXAMINER STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFADJ        IFNE      *BLANKS
      *
     C     SCTADJ        COMP      *BLANKS                                61
     C     *IN61         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTADJ        IFNE      *BLANKS
      *
     C     SCTADJ        CHAIN     PUP02                              62
      *
     C     *IN62         CABEQ     *ON           STRT
     C     SCFADJ        COMP      *BLANKS                                61
     C     *IN61         CABEQ     *ON           STRT
      *
     C                   END
      *
      *  CLAIMS ASSIGNMENT MODE (IGNORE "TO" VALUE)
      *
     C                   ELSE
      *
     C     SCFADJ        IFNE      *BLANKS
      *
     C     SCFADJ        CHAIN     PUP02                              84
      *
     C     *IN84         IFEQ      *ON
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  SCFADJ NE BLANK
      *
     C                   ENDIF                                                  QQFLAG = C
      *****************************************************************
      *  FROM & TO CLAIMS SUPERVISOR STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFSPV        IFNE      *BLANKS
      *
     C     SCTSPV        COMP      *BLANKS                                63
     C     *IN63         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTSPV        IFNE      *BLANKS
      *
     C     SCTSPV        CHAIN     PUP02                              64
      *
     C     *IN64         CABEQ     *ON           STRT
     C     SCFSPV        COMP      *BLANKS                                63
     C     *IN63         CABEQ     *ON           STRT
      *
     C                   END
      *
      *  CLAIMS ASSIGNMENT MODE (IGNORE "TO" VALUE)
      *
     C                   ELSE
      *
     C     SCFSPV        IFNE      *BLANKS
      *
     C     SCFSPV        CHAIN     PUP02                              85
      *
     C     *IN85         IFEQ      *ON
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                              ANK
      *
     C                   ENDIF                                                  SCFSPV NE BLANK
      *
     C                   ENDIF                                                  QQFLAG = C
      *****************************************************************
      *  FROM & TO CLAIMS ACCOUNT MANAGER STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFAMN        IFNE      *BLANKS
      *
     C     SCTAMN        COMP      *BLANKS                                65
     C     *IN65         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTAMN        IFNE      *BLANKS
      *
     C     SCTAMN        CHAIN     PUP02                              66
      *
     C     *IN68         CABEQ     *ON           STRT
     C     SCFAMN        COMP      *BLANKS                                65
     C     *IN65         CABEQ     *ON           STRT
      *
     C                   END
      *
      *  CLAIMS ASSIGNMENT MODE (IGNORE "TO" VALUE)
      *
     C                   ELSE
      *
     C     SCFAMN        IFNE      *BLANKS
      *
     C     SCFAMN        CHAIN     PUP02                              86
      *
     C     *IN86         IFEQ      *ON
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  SCFAMN NE BLANK
      *
     C                   ENDIF                                                  QQFLAG=C
      *****************************************************************
      *  FROM & TO CLAIMS MANAGER STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFUMN        IFNE      *BLANKS
      *
     C     SCTUMN        COMP      *BLANKS                                67
     C     *IN67         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTUMN        IFNE      *BLANKS
      *
     C     SCTUMN        CHAIN     PUP02                              68
      *
     C     *IN68         CABEQ     *ON           STRT
     C     SCFUMN        COMP      *BLANKS                                67
     C     *IN67         CABEQ     *ON           STRT
      *
     C                   END
      *
      *  CLAIMS ASSIGNMENT MODE (IGNORE "TO" VALUE)
      *
     C                   ELSE
      *
     C     SCFUMN        IFNE      *BLANKS
      *
     C     SCFUMN        CHAIN     PUP02                              87
      *
     C     *IN87         IFEQ      *ON
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  SCFUMN NE BLANK
      *
     C                   ENDIF                                                  QQFLAG = C
      *****************************************************************
      *  FROM & TO CLAIMS CASE NURSE STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFNSE        IFNE      *BLANKS
      *
     C     SCTNSE        COMP      *BLANKS                                69
     C     *IN69         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTNSE        IFNE      *BLANKS
      *
     C     SCTNSE        CHAIN     PUP02                              70
      *
     C     *IN70         CABEQ     *ON           STRT
     C     SCFNSE        COMP      *BLANKS                                69
     C     *IN69         CABEQ     *ON           STRT
      *
     C                   END
      *
      *  CLAIMS ASSIGNMENT MODE (IGNORE "TO" VALUE)
      *
     C                   ELSE
      *
     C     SCFNSE        IFNE      *BLANKS
      *
     C     SCFNSE        CHAIN     PUP02                              88
      *
     C     *IN88         IFEQ      *ON
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  SCFNSE NE BLANK
      *
     C                   ENDIF                                                  QQFLAG = C
      *****************************************************************
      *  FROM & TO CLAIMS OUTSIDE ADJUSTER STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFFAD        IFNE      *BLANKS
      *
     C     SCTFAD        COMP      *BLANKS                                71
     C     *IN71         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTFAD        IFNE      *BLANKS
      *
     C     SCTFAD        CHAIN     PUP02                              72
      *
     C     *IN72         CABEQ     *ON           STRT
     C     SCFFAD        COMP      *BLANKS                                71
     C     *IN71         CABEQ     *ON           STRT
      *
     C                   END
      *
      *  CLAIMS ASSIGNMENT MODE (IGNORE "TO" VALUE)
      *
     C                   ELSE
      *
     C     SCFFAD        IFNE      *BLANKS
      *
     C     SCFFAD        CHAIN     PUP02                              89
      *
     C     *IN89         IFEQ      *ON
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  SCFFAD NE BLANK
      *
     C                   ENDIF                                                  QQFLAG = C
      *****************************************************************
      *  FROM & TO CLAIMS SDF ADJUSTER STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFSDF        IFNE      *BLANKS
      *
     C     SCTSDF        COMP      *BLANKS                                73
     C     *IN73         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTSDF        IFNE      *BLANKS
      *
     C     SCTSDF        CHAIN     PUP02                              74
      *
     C     *IN74         CABEQ     *ON           STRT
     C     SCFSDF        COMP      *BLANKS                                73
     C     *IN73         CABEQ     *ON           STRT
      *
     C                   END
      *
     C                   END                                                    QQFLAG=C
      *****************************************************************
      *  FROM & TO CLAIMS EXCESS ADJUSTER STAFF CODE IF ENTERED
      *  FROM CODE DOES NOT HAVE TO BE VALID
      *
     C     QQFLAG        IFEQ      'C'
      *
     C     SCFESP        IFNE      *BLANKS
      *
     C     SCTESP        COMP      *BLANKS                                75
     C     *IN75         CABEQ     *ON           STRT
      *
     C                   END
      *
     C     SCTESP        IFNE      *BLANKS
      *
     C     SCTESP        CHAIN     PUP02                              76
      *
     C     *IN76         CABEQ     *ON           STRT
     C     SCFESP        COMP      *BLANKS                                75
     C     *IN75         CABEQ     *ON           STRT
      *
     C                   END
      *
     C                   END                                                    QQFLAG=C
     C*** Begin add *** 08/22/03 **************************************
      *****************************************************************
      *  Claim status edit.
      *****************************************************************
     C     SCOCST        IFNE      'O'
     C     SCOCST        ANDNE     'C'
     C     SCOCST        ANDNE     'B'
     C                   MOVE      *ON           *IN95
     C                   GOTO      STRT
     C                   ENDIF
     C*** End add ***** 08/22/03 **************************************
     *** BEG ADD *** 11/24/97 *****************************************
      *****************************************************************
      *  PROCESS FOR CLAIMS WITH LOST TIME, NO LOST TIME OR BOTH
      *****************************************************************
     C     SCLTFL        IFNE      'I'
     C     SCLTFL        ANDNE     'M'
     C     SCLTFL        ANDNE     'B'
      *
     C                   MOVE      *ON           *IN93
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCLTFL NE L
      *
     *** END ADD *** 11/24/97 *****************************************
      *****************************************************************
      *  PROCESS FOR ONE OR MORE FUNDS
      *****************************************************************
     C                   SELECT
      *
     *** BEG CHG *** 06/05/13 *****************************************
     C************     SCALLF        WHENEQ    'N'
     C*** Begin Chg *** 02/08/16 **************************************
     C*****SCALLF        WHENEQ    ' '
     C     SCALLF        WHENEQ    'N'
     C*** End   Chg *** 02/08/16 **************************************
     *** END CHG *** 06/05/13 *****************************************
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C     SCALLF        WHENEQ    'Y'
      *
     C                   GOTO      CALLIT
      *
     C                   OTHER
      *
     *** BEG DEL *** 06/05/13 *****************************************
     C*************      MOVE      *ON           *IN78
     C*************      MOVEL(P)  'SCR01'       Q2RECF
     C*************      GOTO      STRT
     *** END DEL *** 06/05/13 *****************************************
      *
     C                   ENDSL
      ****************************************************************
      *   PROCESS REQUEST SCREEN
      *****************************************************************
     C     SCR02T        TAG
      *
      *  VALIDATE FUNDS
      *
     C                   MOVE      *OFF          *IN78
      *
     C     1             DO        10            X
      *
     C     FD(X)         IFNE      *ZEROS
      *
     C                   MOVE      FD(X)         XXFND
      *
     C     FNKEY         CHAIN     PFN01                              60
      *
      *  INVALID FUND ENTERED
      *
     C     *IN60         IFEQ      *ON
      *
     C     X             ADD       29            Y
     C                   MOVE      *ON           *IN(Y)
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ELSE                                                   *IN60=ON
     C                   MOVE      *ON           *IN78
      *
     C                   ENDIF                                                  *IN60=ON
      *
     C                   ENDIF                                                  FD,X NE BLAN
      *
     C                   ENDDO
      *
     C     *IN78         IFEQ      *OFF
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     *** BEG CHG *** 11/24/97 *****************************************
     C******************** MOVE *ON       *IN59
     C                   MOVE      *ON           *IN92
     *** BEG CHG *** 11/24/97 *****************************************
     C                   GOTO      STRT
      *
     C                   ENDIF
      *****************************************************************
      *   CALL THE CLP TO SUBMIT THE LIST.
      *****************************************************************
      *
     C     CALLIT        TAG
     C*** BEGIN CHG *** 01/19/01 **************************************
     C******************** MOVE UPCO#     QQCO#   3
     C                   MOVE      XXCO#         QQCO#             3
     C*** END CHG ***** 01/19/01 **************************************
     *** BEG CHG *** 11/24/97 *****************************************
     C******************** MOVE SCFND     QQFND   3
     C                   MOVE      SCLTFL        QQLTFL            1
     *** END CHG *** 11/24/97 *****************************************
     C                   MOVE      SCFLET        QQFLET            1
     C                   MOVE      SCTLET        QQTLET            1
     C                   MOVE      SCFEMP        QQFEMP            9
     C                   MOVE      SCTEMP        QQTEMP            9
     C                   MOVE      SCCN1         QQCN1             4
     C                   MOVE      SCCN2         QQCN2             4
     C                   MOVE      SCCN3         QQCN3             4
     C                   MOVE      SCCN4         QQCN4             4
     C                   MOVE      SCCN5         QQCN5             4
     C                   MOVE      SCFADJ        QQFADJ            3
     C                   MOVE      SCTADJ        QQTADJ            3
     C                   MOVE      SCFSPV        QQFSPV            3
     C                   MOVE      SCTSPV        QQTSPV            3
     C                   MOVE      SCFAMN        QQFAMN            3
     C                   MOVE      SCTAMN        QQTAMN            3
     C                   MOVE      SCFUMN        QQFUMN            3
     C                   MOVE      SCTUMN        QQTUMN            3
     C                   MOVE      SCFNSE        QQFNSE            3
     C                   MOVE      SCTNSE        QQTNSE            3
     C                   MOVE      SCFFAD        QQFFAD            3
     C                   MOVE      SCTFAD        QQTFAD            3
     C                   MOVE      SCFSDF        QQFSDF            3
     C                   MOVE      SCTSDF        QQTSDF            3
     C                   MOVE      SCFESP        QQFESP            3
     C                   MOVE      SCTESP        QQTESP            3
     C**********           MOVE SCALLF    QQALLF  1
     C                   MOVE      SCFD01        QQFD01            3
     C                   MOVE      SCFD02        QQFD02            3
     C                   MOVE      SCFD03        QQFD03            3
     C                   MOVE      SCFD04        QQFD04            3
     C                   MOVE      SCFD05        QQFD05            3
     C                   MOVE      SCFD06        QQFD06            3
     C                   MOVE      SCFD07        QQFD07            3
     C                   MOVE      SCFD08        QQFD08            3
     C                   MOVE      SCFD09        QQFD09            3
     C                   MOVE      SCFD10        QQFD10            3
     C                   MOVE      'D'           QQMODE            1
     *** BEG DEL *** 11/24/97 *****************************************
     C******************** MOVE UPOUTQ    QQQUE  10
     *** END DEL *** 11/24/97 *****************************************
      *
      ***** END ADD ***** 08/30/06 ************************************
     C     SCTENM        IFNE      *BLANKS
     C                   MOVE      SCFENM        QQFENM            1
     C                   MOVE      SCTENM        QQTENM            1
     C                   ELSE
     C                   MOVE      '0'           QQFENM
     C                   MOVE      '0'           QQTENM
     C                   ENDIF                                                  SCTENM <> BLANK
      ***** BEG ADD ***** 08/30/06 ************************************
      *
      *  SUBMIT THE CL
      *
     C                   CALL      'W4300C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFLET
     C                   PARM                    QQTLET
     C                   PARM                    QQFEMP
     C                   PARM                    QQTEMP
     C*** Begin dlt *** 08/22/03 **************************************
     C**********           PARM           QQCN1
     C**********           PARM           QQCN2
     C**********           PARM           QQCN3
     C**********           PARM           QQCN4
     C**********           PARM           QQCN5
     C**********           PARM           QQFADJ
     C**********           PARM           QQTADJ
     C**********           PARM           QQFSPV
     C**********           PARM           QQTSPV
     C**********           PARM           QQFAMN
     C**********           PARM           QQTAMN
     C**********           PARM           QQFUMN
     C**********           PARM           QQTUMN
     C**********           PARM           QQFNSE
     C**********           PARM           QQTNSE
     C**********           PARM           QQFFAD
     C**********           PARM           QQTFAD
     C**********           PARM           QQFSDF
     C**********           PARM           QQTSDF
     C**********           PARM           QQFESP
     C**********           PARM           QQTESP
     C**********           PARM           QQFD01
     C**********           PARM           QQFD02
     C**********           PARM           QQFD03
     C**********           PARM           QQFD04
     C**********           PARM           QQFD05
     C**********           PARM           QQFD06
     C**********           PARM           QQFD07
     C**********           PARM           QQFD08
     C**********           PARM           QQFD09
     C**********           PARM           QQFD10
     C*** End dlt ***** 08/22/03 **************************************
     C*** Begin add *** 08/22/03 **************************************
     C                   PARM                    QQDATA
     C*** End add ***** 08/22/03 **************************************
     C                   PARM                    QQQUE
     C                   PARM                    QQMODE
     C                   PARM                    QQFLAG
     C                   PARM                    QQLTFL
     C*** Begin add *** 08/22/03 **************************************
     C                   PARM      SCOCST        QQOCST            1
     C*** End add ***** 08/22/03 **************************************
      *****************************************************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
SCFD01
SCFD02
SCFD03
SCFD04
SCFD05
SCFD06
SCFD07
SCFD08
SCFD09
SCFD10
**
SCFADJ
SCTADJ
SCFSPV
SCTSPV
SCFAMN
SCTAMN
SCFUMN
SCTUMN
SCFNSE
SCTNSE
SCFFAD
SCTFAD
SCFSDF
SCTSDF
SCFESP
SCTESP
