     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W0600R - WORKERS COMP REFUND REQUEST SEARCH
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/23/14 C:062 Support: WC089 87311                          *
     H*                 Change select field to output field.          *
     H*                 Recompile only                                *
     H*                                                               *
     H*  05/21/14 C:283 Support: WC090 82759                          *
     H*                 Handle the Visibility Input field SCFL01.     *
     H*                 This field should be Visible only when a      *
     H*                 record is populated in the corresponding      *
     H*                 fields (Output).                              *
     H*
     H*  11/07/03 C:53 Project: WC1163
     H*                Added F6=Add. Actually will return to W0100R
     H*
     H*  01/23/98 C:85 PROJECT: WC577                                 *
     H*                CHANGED SCREEN ONE NAME TO RECOVERY REQUEST    *
     H*                SEARCH.                                        *
     H*                ALSO VERSION 3 STANDARDS.                      *
     H*                                                               *
     H*  JJJ REVISED 8-9-90:  INCLUDE REQ# WHEN CHAINING, ALSO        *
     H*                       NEED TO MAKE SURE USER SELECTED A RECORD*
     H*                       WHICH DISPLAYED A REQUEST.              *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW0600F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMRRL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*  JJJ ADDED ARRAY KEYR 8-9-90
     D KEYR            S              5  0 DIM(2)
     D*
     D*** BEGIN CHG *** 01/23/98 ***********************************************
     D**********    'Request Refund Searc-C         OPTNM
     D**********    'h'
     D*
     D OPTNM           C                   CONST('Recovery Request Sea-
     D                                     rch')
     D*** END   CHG *** 01/23/98 ***********************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FY                     1     42  0
     D                                     DIM(14)
     D  SCFY01                 1      3  0
     D  SCFY02                 4      6  0
     D  SCFY03                 7      9  0
     D  SCFY04                10     12  0
     D  SCFY05                13     15  0
     D  SCFY06                16     18  0
     D  SCFY07                19     21  0
     D  SCFY08                22     24  0
     D  SCFY09                25     27  0
     D  SCFY10                28     30  0
     D  SCFY11                31     33  0
     D  SCFY12                34     36  0
     D  SCFY13                37     39  0
     D  SCFY14                40     42  0
     D                 DS
     D  CS                     1     98  0
     D                                     DIM(14)
     D  SCCS01                 1      7  0
     D  SCCS02                 8     14  0
     D  SCCS03                15     21  0
     D  SCCS04                22     28  0
     D  SCCS05                29     35  0
     D  SCCS06                36     42  0
     D  SCCS07                43     49  0
     D  SCCS08                50     56  0
     D  SCCS09                57     63  0
     D  SCCS10                64     70  0
     D  SCCS11                71     77  0
     D  SCCS12                78     84  0
     D  SCCS13                85     91  0
     D  SCCS14                92     98  0
     D                 DS
     D  RS                     1     70  0
     D                                     DIM(14)
     D  SCRS01                 1      5  0
     D  SCRS02                 6     10  0
     D  SCRS03                11     15  0
     D  SCRS04                16     20  0
     D  SCRS05                21     25  0
     D  SCRS06                26     30  0
     D  SCRS07                31     35  0
     D  SCRS08                36     40  0
     D  SCRS09                41     45  0
     D  SCRS10                46     50  0
     D  SCRS11                51     55  0
     D  SCRS12                56     60  0
     D  SCRS13                61     65  0
     D  SCRS14                66     70  0
     D                 DS
     D  FE                     1    126  0
     D                                     DIM(14)
     D  SCFE01                 1      9  0
     D  SCFE02                10     18  0
     D  SCFE03                19     27  0
     D  SCFE04                28     36  0
     D  SCFE05                37     45  0
     D  SCFE06                46     54  0
     D  SCFE07                55     63  0
     D  SCFE08                64     72  0
     D  SCFE09                73     81  0
     D  SCFE10                82     90  0
     D  SCFE11                91     99  0
     D  SCFE12               100    108  0
     D  SCFE13               109    117  0
     D  SCFE14               118    126  0
     D                 DS
     D  MO                     1     42  0
     D                                     DIM(14)
     D  SCMO01                 1      3  0
     D  SCMO02                 4      6  0
     D  SCMO03                 7      9  0
     D  SCMO04                10     12  0
     D  SCMO05                13     15  0
     D  SCMO06                16     18  0
     D  SCMO07                19     21  0
     D  SCMO08                22     24  0
     D  SCMO09                25     27  0
     D  SCMO10                28     30  0
     D  SCMO11                31     33  0
     D  SCMO12                34     36  0
     D  SCMO13                37     39  0
     D  SCMO14                40     42  0
     D                 DS
     D  NM                     1    182
     D                                     DIM(14)
     D  SCNM01                 1     13
     D  SCNM02                14     26
     D  SCNM03                27     39
     D  SCNM04                40     52
     D  SCNM05                53     65
     D  SCNM06                66     78
     D  SCNM07                79     91
     D  SCNM08                92    104
     D  SCNM09               105    117
     D  SCNM10               118    130
     D  SCNM11               131    143
     D  SCNM12               144    156
     D  SCNM13               157    169
     D  SCNM14               170    182
     D                 DS
     D  CD                     1     28
     D                                     DIM(14)
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D  SCCD05                 9     10
     D  SCCD06                11     12
     D  SCCD07                13     14
     D  SCCD08                15     16
     D  SCCD09                17     18
     D  SCCD10                19     20
     D  SCCD11                21     22
     D  SCCD12                23     24
     D  SCCD13                25     26
     D  SCCD14                27     28
     D                 DS
     D  TY                     1     14
     D                                     DIM(14)
     D  SCTY01                 1      1
     D  SCTY02                 2      2
     D  SCTY03                 3      3
     D  SCTY04                 4      4
     D  SCTY05                 5      5
     D  SCTY06                 6      6
     D  SCTY07                 7      7
     D  SCTY08                 8      8
     D  SCTY09                 9      9
     D  SCTY10                10     10
     D  SCTY11                11     11
     D  SCTY12                12     12
     D  SCTY13                13     13
     D  SCTY14                14     14
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D  SCST11                11     11
     D  SCST12                12     12
     D  SCST13                13     13
     D  SCST14                14     14
     D                 DS
     D  DT                     1    112  0
     D                                     DIM(14)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D  SCDT12               089    096  0
     D  SCDT13               097    104  0
     D  SCDT14               105    112  0
     D                 DS
     D  AR                     1    126  2
     D                                     DIM(14)
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
     D  SCAR11                91     99  2
     D  SCAR12               100    108  2
     D  SCAR13               109    117  2
     D  SCAR14               118    126  2
     D*** BEGIN ADD *** 05/21/14 ***********************************************
     D                 DS
     D  ID                     1     14
     D                                     DIM(14)
     D  indDsp1                1      1
     D  indDsp2                2      2
     D  indDsp3                3      3
     D  indDsp4                4      4
     D  indDsp5                5      5
     D  indDsp6                6      6
     D  indDsp7                7      7
     D  indDsp8                8      8
     D  indDsp9                9      9
     D  indDsp10              10     10
     D  indDsp11              11     11
     D  indDsp12              12     12
     D  indDsp13              13     13
     D  indDsp14              14     14
     D*** END   ADD *** 05/21/14 ***********************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C*
     C*  REFUND REQUEST     FILE
     C*
     C     RRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C     RRKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C                   KFLD                    XXREQ#            5 0
     C*
     C     PDKEY         KLIST
     C                   KFLD                    RRFEIN
     C                   KFLD                    RRMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQYR              3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQREQ#            5 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQFND             3 0
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
     C*** BEG ADD *** 11/07/03 ****************************
     C     QQRTCD        IFEQ      'X'
     C                   MOVE      *ON           *IN13
     C                   MOVE      *BLANK        QQRTCD
     C                   ENDIF
     C*** END ADD *** 11/07/03 ****************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     Q1PRMS        IFGE      7
     C                   Z-ADD     QQFND         SCFND
     C                   ELSE
     C                   Z-ADD     UPFND         SCFND
     C                   ENDIF                                                  Q1PRMS>=7
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   Z-ADD     QQYR          SCYR              3 0
     C                   Z-ADD     QQCASE        SCCASE            7 0
     C                   MOVE      '1'           XXDOWN                         JJJ 8-9-90
     C                   Z-ADD     QQYR          XXFYR
     C                   Z-ADD     QQCASE        XXCASE
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
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQREQ#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*** BEG ADD *** 11/07/03 ****************************
     C*
     C*   F6 = ADD
     C*
     C     *INKF         IFEQ      *ON
     C                   MOVE      '6'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*** END ADD *** 11/07/03 ****************************
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQREQ#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
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
     C*
     C*   JJJ CHANGED CHAIN TO REQUEST FILE 8-9-90
     C*
     C                   Z-ADD     KEYR(2)       XXREQ#                         JJJ 8-9-80
     C     RRKEY1        CHAIN     WMRRL                              60        |
     C     RRKEY         READE     WMRRL                                  60    |
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C*
     C*   JJJ CHANGED CHAIN TO REQUEST FILE 8-9-90
     C*
     C                   Z-ADD     KEYR(1)       XXREQ#                         JJJ 8-9-80
     C     RRKEY1        CHAIN     WMRRL                              60        |
     C                   READP     WMRRL                                  60    |
     C  N60RRCO#         COMP      UPCO#                              6060      |
     C  N60RRFND         COMP      SCFND                              6060      |
     C  N60RRFYR         COMP      XXFYR                              6060      |
     C  N60RRCASE        COMP      XXCASE                             6060      |
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCYR          IFNE      *ZERO
     C                   MOVE      *BLANK        XXCASE
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYR                           JJJ 8-9-90
     C                   MOVE      *BLANK        SCYR
     C                   MOVE      *BLANK        SCCASE
     C     RRKEY         SETLL     WMRRL
     C     RRKEY         READE     WMRRL                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C     RS(I1)        ANDNE     0                                            |
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     RS(I1)        QQREQ#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQREQ#
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       FY
     C                   MOVE      *BLANKS       CS
     C                   MOVE      *BLANKS       RS
     C                   MOVE      *BLANKS       FE
     C                   MOVE      *BLANKS       MO
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       TY
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       AR
     C                   Z-ADD     *ZERO         I1
     C                   MOVE      *BLANK        KEYR                           |
     C*** BEGIN ADD *** 05/21/14 ***********************************************
      /Free
       ID = '0';
      /End-Free
     C*** END   ADD *** 05/21/14 ***********************************************
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     RRFYR         FY(I1)
     C                   MOVE      RRCASE        CS(I1)
     C                   MOVE      RRREQ#        RS(I1)
     C                   MOVE      RRFEIN        FE(I1)
     C                   MOVE      RRMOD         MO(I1)
     C*** BEGIN ADD *** 05/21/14 ***********************************************
      /Free
       ID(I1) = '1';
      /End-free
     C*** END   ADD *** 05/21/14 ***********************************************
     C*
     C                   Z-ADD     RRDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   Z-ADD     RRRAMT        AR(I1)
     C                   MOVE      RRSTCD        CD(I1)
     C                   MOVE      RRSTAT        ST(I1)
     C*
     C*  GET NAME FIELD FROM PD FILE USING FEIN & MOD TO CHAIN
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        XXLAST           56
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        NM(I1)
     C                   MOVE      PDTYPE        TY(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       NM(I1)
     C                   END
     C*
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
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     RRKEY         READE     WMRRL                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     RS(1)         KEYR(1)
     C                   Z-ADD     RS(14)        KEYR(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
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
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     WMRRL                                  60
     C  N60RRCO#         COMP      UPCO#                              6060
     C  N60RRFND         COMP      SCFND                              6060
     C  N60RRFYR         COMP      XXFYR                              6060
     C  N60RRCASE        COMP      XXCASE                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     RS(1)         KEYR(1)
     C                   Z-ADD     RS(14)        KEYR(2)
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
     C                   Z-ADD     *ZERO         XXREQ#                         JJJ 8-9-90
     C     RRKEY1        SETLL     WMRRL                                        |
     C     RRKEY         READE     WMRRL                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     RRKEY         READE     WMRRL                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
  ...More

