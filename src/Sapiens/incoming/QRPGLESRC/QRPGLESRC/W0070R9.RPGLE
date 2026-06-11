     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0070R9 - BENEFIT ADJUSTMENT MAINTENANCE
     H*****************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER           *
     H*               BENEFIT ADJUSTMENTS ON CLAIMS.                  *
     H*                                                               *
     H* CALLED FROM:  W0070R5                                         *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   15                                              *
     H* PROJECT:      25WDE                01/10/96                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H*  12/04/08 C:53  Project: SUPPORT 65536                        *
     H*                 Delete records based on window answer         *
     H*                                                               *
     H*  02/04/08 C:53  Project: WC1584 PRISS 21                      *
     H*                 REMOVE EDITS                                  *
     H*                                                               *
     H*  10/19/07 C:191 Project: WC1584                               *
     H*                 EDI Release 3: Add End date column            *
     H*                                                               *
     H*  08/20/01 C:15 Support: WC072/28660                           *
     H*                Changed pgm to allow the entry of a Pmt Adj    *
     H*                code, which is required for the Benefit Adj    *
     H*                entry.                                         *
     H*                                                               *
     H*  01/26/96 C:47 PROJECT 25WDE                                  *
     H*                START DATE MUST NOT BE BEFORE ACCIDENT DATE    *
     H*                                                               *
     H*  07/14/97 C:47 PROJECT 60ANC                                  *
     H*                UPDATED TO VERSION 3 SPECS                     *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  *IN09  -  INDICATES MODE (MAINT/INQ)                         *
     H*  *IN20  -  INVALID ROLL REQUEST                               *
     H*  *IN21  -  INVALID TABLE VALUE FOUND                          *
     H*  *IN22  -  STATUS MUST BE 'A' OR 'I'                          *
     H*  *IN23  -  INVALID DATE ENTERED                               *
     H*  *IN24  -  AMOUNT CANNOT BE LESS THAN OR EQUAL TO ZERO        *
     H*  *IN25  -  ACCIDENT DATE CAN NOT BE BEFORE START DATE         *
     H*  *IN28  -  POSITION CURSOR ON THE CODE FIELD                  *
     H*  *IN29  -  POSITION CURSOR ON THE AMOUNT FIELD                *
     H*  *IN31  -  INDICATES LINE 1                                   *
     H*  *IN32  -  INDICATES LINE 2                                   *
     H*  *IN33  -  INDICATES LINE 3                                   *
     H*  *IN34  -  INDICATES LINE 4                                   *
     H*  *IN35  -  INDICATES LINE 5                                   *
     H*  *IN36  -  INDICATES LINE 6                                   *
     H*  *IN37  -  INDICATES LINE 7                                   *
     H*  *IN38  -  INDICATES LINE 8                                   *
     H*  *IN41  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 1)        *
     H*  *IN42  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 2)        *
     H*  *IN43  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 3)        *
     H*  *IN44  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 4)        *
     H*  *IN45  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 5)        *
     H*  *IN46  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 6)        *
     H*  *IN47  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 7)        *
     H*  *IN48  -  PROTECTS FIELDS FROM BEING CHANGED (LINE 8)        *
     H*                                                               *
     H*  *IN40  -  INDICATES WHETHER TO WRITE OR UPDATE RECORD        *
     H*  *IN60  -  GENERAL USAGE (CHAIN, READ, ETC..)                 *
     H*****************************************************************
     FW0070F9   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMBNL     UF A E           K DISK
     FWTGTL     IF   E           K DISK

     D*** BEG ADD ****** 10/19/07 ************************************
     D*** END ADD ****** 10/19/07 ************************************
     D*** Begin chg *** 08/20/01 **************************************
     D******************* CD          8  3
     D******************* DS          8 20
     D*** End chg ***** 08/20/01 **************************************
     D SQ              S              5  0 DIM(8)
     D ODT             S              6  0 DIM(8)
     D*** BEG ADD ****** 10/19/07 ************************************
     D OEN             S              6  0 DIM(8)
     D*** END ADD ****** 10/19/07 ************************************
     D*** Begin chg *** 08/20/01 **************************************
     D******************* OCD         8  3
     D OCD             S              1    DIM(8)
     D*** End chg ***** 08/20/01 **************************************
     D OAM             S              9  2 DIM(8)
     D OST             S              1    DIM(8)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYS            S              5  0 DIM(2)
     D PRDS            S             40    DIM(2) CTDATA PERRCD(1)
     D*** Begin add *** 08/20/01 **************************************
     D OPA             S              3    DIM(8)
     D*** End add ***** 08/20/01 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DT                     1    064  0
     D                                     DIM(8)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D*** BEG ADD ****** 10/19/07 ************************************
     D                 DS
     D  EN                     1    064  0
     D                                     DIM(8)
     D  SCEN01               001    008  0
     D  SCEN02               009    016  0
     D  SCEN03               017    024  0
     D  SCEN04               025    032  0
     D  SCEN05               033    040  0
     D  SCEN06               041    048  0
     D  SCEN07               049    056  0
     D  SCEN08               057    064  0
     D*** END ADD ****** 10/19/07 ************************************
     D*** Begin dlt *** 08/20/01 **************************************
     D*********** DS
     D***********                             1  24 CD
     D***********                             1   3 SCCD01
     D***********                             4   6 SCCD02
     D***********                             7   9 SCCD03
     D***********                            10  12 SCCD04
     D***********                            13  15 SCCD05
     D***********                            16  18 SCCD06
     D***********                            19  21 SCCD07
     D***********                            22  24 SCCD08
     D*********** DS
     D***********                             1 160 DS
     D***********                             1  20 SCDS01
     D***********                            21  40 SCDS02
     D***********                            41  60 SCDS03
     D***********                            61  80 SCDS04
     D***********                            81 100 SCDS05
     D***********                           101 120 SCDS06
     D***********                           121 140 SCDS07
     D***********                           141 160 SCDS08
     D*** End dlt ***** 08/20/01 **************************************
     D*** Begin add *** 08/20/01 **************************************
     D                 DS
     D  CD                     1      8
     D                                     DIM(8)
     D  SCCD01                 1      1
     D  SCCD02                 2      2
     D  SCCD03                 3      3
     D  SCCD04                 4      4
     D  SCCD05                 5      5
     D  SCCD06                 6      6
     D  SCCD07                 7      7
     D  SCCD08                 8      8
     D                 DS
     D  DS                     1    120
     D                                     DIM(8)
     D  SCDS01                 1     15
     D  SCDS02                16     30
     D  SCDS03                31     45
     D  SCDS04                46     60
     D  SCDS05                61     75
     D  SCDS06                76     90
     D  SCDS07                91    105
     D  SCDS08               106    120
     D                 DS
     D  PA                     1     24
     D                                     DIM(8)
     D  SCPA01                 1      3
     D  SCPA02                 4      6
     D  SCPA03                 7      9
     D  SCPA04                10     12
     D  SCPA05                13     15
     D  SCPA06                16     18
     D  SCPA07                19     21
     D  SCPA08                22     24
     D                 DS
     D  PD                     1    120
     D                                     DIM(8)
     D  SCPD01                 1     15
     D  SCPD02                16     30
     D  SCPD03                31     45
     D  SCPD04                46     60
     D  SCPD05                61     75
     D  SCPD06                76     90
     D  SCPD07                91    105
     D  SCPD08               106    120
     D*** End add ***** 08/20/01 **************************************
     D                 DS
     D  AM                     1     72  2
     D                                     DIM(8)
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D                 DS
     D  ST                     1      8
     D                                     DIM(8)
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D XCEXRE          C                   CONST('EXCS REIN ')
     D*********     'Benefit Maintenance' C         OPTNM
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display- ')
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE ADDITIONAL BODY PARTS FILE
     C*
     C     BNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C     BNKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C********************************************************** ********
     C*  *ENTRY PARAMETERS FOR PROGRAM
     C********************************************************** ********
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
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
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    BNACOD        XXKEY
     C     *LIKE         DEFINE    BNSEQ#        XXSEQ#
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    CMACDT        XXSDT
     C*
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  MODEC         SCMODE
     C     QQCALL        COMP      '1'                                    09
     C   09              MOVEL(P)  MODED         SCMODE
     C  N09              MOVE      PRDS(1)       SCPRDS
     C   09              MOVE      PRDS(2)       SCPRDS
     C*
     C*  RETRIEVE THE USER PARAMETER, COMPANY AND FUND RECORDS
     C*
     C                   Z-ADD     0             XXCO#
     C****************     Z-ADD0         XXFND
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     UPCO#         XXCO#
     C****************     Z-ADDUPFND     XXFND
     C                   END
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C*  RETRIEVE THE CLAIMANT INFORMATION
     C*
     C                   MOVE      XXFYR         XXYR              2
     C                   MOVEL     XXYR          SCYRCL
     C                   MOVE      XXCASE        SCYRCL
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DBRRN         XXRRN                          FOR WMCMP
     C******               MOVE CMSS#     SCSS#
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   END
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C     CMEXCS        IFNE      'Y'
     C     XXRRN         ANDNE     *ZERO
     C                   CALL      'WA010R'                             99
     C                   PARM                    XXRRN             9 0          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'                             99
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   END
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
     C*
     C*                    MOVELOPTNM     ##DATA    P
     C*                    CALL 'SRCENTER'
     C*                    PARM           ##DATA 80
     C*                    PARM 40        ##LENG  30
     C*                    MOVEL##DATA    SCPRDS
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C*  RETRIEVE THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVEL     CDLSN         SCCNAM
     C     CDLSN         IFNE      *BLANKS
     C     SCCNAM        CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ELSE
     C                   MOVEL     CDFSN         SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ENDIF
     C*
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ZEROS        KEYS
     C                   MOVE      '1'           XXDOWN            1
     C     BNKY1         CHAIN(N)  PBN01                              60
     C                   EXSR      LOAD
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 10 = TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*   - Benefit Adjustment Code search
     C*
     C************         DO   8         X
     C*
     C     HFLD          IFEQ      'SCCD01'
     C     HFLD          OREQ      'SCCD02'
     C     HFLD          OREQ      'SCCD03'
     C     HFLD          OREQ      'SCCD04'
     C     HFLD          OREQ      'SCCD05'
     C     HFLD          OREQ      'SCCD06'
     C     HFLD          OREQ      'SCCD07'
     C     HFLD          OREQ      'SCCD08'
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCD01'
     C                   Z-ADD     1             X
     C     HFLD          WHENEQ    'SCCD02'
     C                   Z-ADD     2             X
     C     HFLD          WHENEQ    'SCCD03'
     C                   Z-ADD     3             X
     C     HFLD          WHENEQ    'SCCD04'
     C                   Z-ADD     4             X
     C     HFLD          WHENEQ    'SCCD05'
     C                   Z-ADD     5             X
     C     HFLD          WHENEQ    'SCCD06'
     C                   Z-ADD     6             X
     C     HFLD          WHENEQ    'SCCD07'
     C                   Z-ADD     7             X
     C     HFLD          WHENEQ    'SCCD08'
     C                   Z-ADD     8             X
     C                   ENDSL
     C*
     C                   MOVEL     CD(X)         XX1A              1
     C*******    XX1A      IFEQ '?'
     C*
     C     30            ADD       X             X1
     C                   MOVE      '1'           *IN(X1)
     C*
     C                   MOVEL     'BADJ'        QQTBID            4
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     'BADJ'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C     GTSTAT        CABEQ     'I'           STRT                     28
     C                   MOVEL     XXKEY         CD(X)
     C                   MOVEL     GTDESC        DS(X)
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*************        END                              CD,X  IFEQ '?'
     C*
     C                   END                                                     HFLD
     C*************        END                              DO   8    X
     C*
     C*** Begin add *** 08/20/01 **************************************
     C*
     C*   - Payment Adjustment Code search
     C*
     C     HFLD          IFEQ      'SCPA01'
     C     HFLD          OREQ      'SCPA02'
     C     HFLD          OREQ      'SCPA03'
     C     HFLD          OREQ      'SCPA04'
     C     HFLD          OREQ      'SCPA05'
     C     HFLD          OREQ      'SCPA06'
     C     HFLD          OREQ      'SCPA07'
     C     HFLD          OREQ      'SCPA08'
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCPA01'
     C                   Z-ADD     1             X
     C     HFLD          WHENEQ    'SCPA02'
     C                   Z-ADD     2             X
     C     HFLD          WHENEQ    'SCPA03'
     C                   Z-ADD     3             X
     C     HFLD          WHENEQ    'SCPA04'
     C                   Z-ADD     4             X
     C     HFLD          WHENEQ    'SCPA05'
     C                   Z-ADD     5             X
     C     HFLD          WHENEQ    'SCPA06'
     C                   Z-ADD     6             X
     C     HFLD          WHENEQ    'SCPA07'
     C                   Z-ADD     7             X
     C     HFLD          WHENEQ    'SCPA08'
     C                   Z-ADD     8             X
     C                   ENDSL
     C*
     C     30            ADD       X             X1
     C                   MOVE      '1'           *IN(X1)
     C*
     C                   MOVEL     'PADJ'        QQTBID            4
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     XXKEY         PA(X)
     C                   MOVEL     'PADJ'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C     GTSTAT        CABEQ     'I'           STRT                     29
     C                   MOVEL     GTDESC        PD(X)
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                     HFLD
     C*** End add ***** 08/20/01 **************************************
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
     C*****
     C                   END                                                    END OF CMD KEYS
     C**************************************************************************
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
     C     *IN18         IFEQ      '1'
     C                   EXSR      RITE
     C     XXUP          IFNE      *BLANK
     C     CD(8)         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     *IN19         IFEQ      '1'
     C                   EXSR      RITE
     C     XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYS(2)       XXSEQ#
     C     BNKY          CHAIN(N)  PBN01                              60
     C     BNKY1         READE(N)  PBN01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYS(1)       XXSEQ#
     C     XXSEQ#        IFEQ      0
     C     BNKY1         SETGT     PBN01
     C                   ELSE
     C     BNKY          CHAIN(N)  PBN01                              60
     C                   END
     C     BNKY1         READPE(N) PBN01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  PROCESS ANY CHANGES.
     C*
     C                   EXSR      RITE
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     C*  CLEAR THE SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *ZEROS        DT
     C*** BEG ADD ****** 10/19/07 ************************************
     C                   MOVE      *ZEROS        EN
     C                   MOVE      *ZEROS        OEN
     C*** END ADD ****** 10/19/07 ************************************
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *ZEROS        AM
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *ZEROS        SQ
     C                   MOVE      *ZEROS        ODT
     C                   MOVE      *BLANKS       OCD
     C                   MOVE      *ZEROS        OAM
     C                   MOVE      *BLANKS       OST
     C*** Begin add *** 08/20/01 **************************************
     C                   MOVE      *BLANKS       PA
     C                   MOVE      *BLANKS       PD
     C                   MOVE      *BLANKS       OPA
     C*** End add ***** 08/20/01 **************************************
     C                   MOVEA     '00000000'    *IN(41)
     C                   Z-ADD     *ZERO         I1                3 0          ARRAY INDEX
     C                   ENDSR
     C*
     C**************************************************************************
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     BNSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C                   MOVEL     BNACOD        CD(I1)
     C                   MOVEL     'BADJ'        XXTBID
     C*** Begin dlt *** 08/20/01 **************************************
     C******************** MOVE *BLANKS   XXKEY
     C******************** MOVELBNACOD    XXKEY
     C*** End dlt ***** 08/20/01 **************************************
     C*** Begin add *** 08/20/01 **************************************
     C                   MOVEL(P)  CD(I1)        XXKEY
     C*** End add ***** 08/20/01 **************************************
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        DS(I1)
     C*** Begin add *** 08/20/01 **************************************
     C     3             SUBST     BNACOD:2      PA(I1)
     C                   MOVEL     'PADJ'        XXTBID
     C                   MOVEL(P)  PA(I1)        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        PD(I1)
     C*** End add ***** 08/20/01 **************************************
     C                   Z-ADD     BNAMT         AM(I1)
     C                   MOVE      BNSTAT        ST(I1)
     C                   MOVE      BNSEQ#        SQ(I1)
     C*** BEG ADD ****** 10/19/07 ************************************
     C                   Z-ADD     BNENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EN(I1)
     C*** END ADD ****** 10/19/07 ************************************
     C                   MOVE      DT(I1)        ODT(I1)
     C*** BEG ADD ****** 10/19/07 ************************************
     C                   MOVE      EN(I1)        OEN(I1)
     C*** END ADD ****** 10/19/07 ************************************
     C                   MOVE      CD(I1)        OCD(I1)
     C                   MOVE      AM(I1)        OAM(I1)
     C                   MOVE      ST(I1)        OST(I1)
     C     I1            ADD       40            X1
     C*** Begin dlt *** 08/20/01 **************************************
     C*******************  MOVE '1'       *IN,X1
     C*** End dlt ***** 08/20/01 **************************************
     C                   ENDSR
     C*
     C**************************************************************************
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVER          TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     BNKY1         READE(N)  PBN01                                  60
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C     I1            COMP      8                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(8)         KEYS(2)
     C                   ENDSR
     C*
     C**************************************************************************
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     9             I1
     C*
     C     OVERP         TAG
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C     BNKY1         READPE(N) PBN01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(8)         KEYS(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C**************************************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN
     C                   MOVE      *BLANK        XXUP
     C     BNKY1         CHAIN(N)  PBN01                              60
     C   60              MOVE      '1'           XXUP
     C   60              GOTO      DONEB
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     BNKY1         READE(N)  PBN01                                  60
     C   60              MOVE      '1'           XXUP
     C   60              GOTO      DONEB
     C     I1            COMP      8                                    61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C**************************************************************
     C*     RITE SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     RITE          BEGSR
     C*
     C*   TURN OFF POSITION CURSOR INDICATORS IF ROLLING UP OR DOWN
     C*
     C                   MOVEA     '00000000'    *IN(31)
     C*** BEG ADD *** 12/04/08 **************************************
     C                   MOVE      *BLANK        SCDLT
     C*** END ADD *** 12/04/08 **************************************
     C*
     C                   DO        8             X                 3 0
     C     X             ADD       30            X1                2 0          ERROR
     C*
     C*   COMPARE HOLD FIELDS FROM ORIGINAL SCREEN TO THE CURRENT
     C*   FIELDS. IF THE FIELDS CHANGED, PERFORM EDITS.
     C*
     C     DT(X)         IFNE      ODT(X)
     C     CD(X)         ORNE      OCD(X)
     C*** Begin add *** 08/20/01 **************************************
     C     PA(X)         ORNE      OPA(X)
     C*** End add ***** 08/20/01 **************************************
     C*** BEG ADD ****** 10/19/07 ************************************
     C     EN(X)         ORNE      OEN(X)
     C*** END ADD ****** 10/19/07 ************************************
     C     AM(X)         ORNE      OAM(X)
     C     OST(X)        OREQ      *BLANK
     C     ST(X)         ANDNE     *BLANK
     C*
     C*  VALIDATE THE START DATE
     C*
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DT(X)
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C**** BEG ADD **** 01/26/96 ***********************************
     C*  THE START DATE CAN NOT BE BEFORE THE ACCIDENT DATE
     C*
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDT
     C     XXSDT         IFLT      CMACDT
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C**** END ADD **** 01/26/96 ***********************************
     C*
     C*  VALIDATE THE BENEFIT ADJUSTMENT CODE.
     C*
     C*** BEG ADD *** 02/04/08 **************************************
     C     CD(X)         IFNE      *BLANKS
     C*** END ADD *** 02/04/08 **************************************
     C                   MOVEL     'BADJ'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CD(X)         XXKEY
     C     GTKY          CHAIN     PGT01                              21
     C     *IN21         IFEQ      '1'
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*** BEG ADD *** 02/04/08 **************************************
     C                   ENDIF
     C*** END ADD *** 02/04/08 **************************************
     C*
     C*** Begin add *** 08/20/01 **************************************
     C*  Validate the Payment Adjustment code.
     C*
     C     PA(X)         IFNE      *BLANKS
     C*
     C                   MOVEL     'PADJ'        XXTBID
     C                   MOVEL(P)  PA(X)         XXKEY
     C     GTKY          CHAIN     PGT01                              71
     C     *IN71         IFEQ      '1'
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*** End add ***** 08/20/01 **************************************
     C*
     C*  VALIDATE THE BENEFIT ADJUSTMENT AMOUNT.
     C*
     C     AM(X)         IFLE      0
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEG ADD ****** 10/19/07 ************************************
     C*  VALIDATE THE END DATE
     C*** BEG ADD *** 02/04/08 **************************************
     C     EN(X)         IFNE      *ZEROS
     C*** END ADD *** 02/04/08 **************************************
     C                   Z-ADD     EN(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         EN(X)
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR = '1'
     C*** BEG ADD *** 02/04/08 **************************************
     C                   ENDIF
     C*** END ADD *** 02/04/08 **************************************
     C*** END ADD ****** 10/19/07 ************************************
     C*
     C                   END
     C*
     C*  VALIDATE THE STATUS CODE IF IT WAS CHANGED.
     C*
     C     ST(X)         IFNE      *BLANK
     C     ST(X)         ANDNE     'A'
     C     ST(X)         ANDNE     'I'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C*** END ADD *** 12/04/08 **************************************
     C     ST(X)         IFEQ      'I'
     C     SCDLT         ANDEQ     *BLANKS
     C*
     C     SCDLT         DOUEQ     'Y'
     C     SCDLT         OREQ      'N'
     C                   EXFMT     WDW01
     C                   MOVE      *ON           *IN51                          error msg
     C                   ENDDO
     C*
     C                   MOVE      *OFF          *IN51
     C                   ENDIF
     C*
     C*** END ADD *** 12/04/08 **************************************
     C*
     C*   IF ANY OF THE FIELDS CHANGED, UPDATE OR WRITE THE RECORD.
     C*
     C     DT(X)         IFNE      ODT(X)
     C     CD(X)         ORNE      OCD(X)
     C*** Begin add *** 08/20/01 **************************************
     C     PA(X)         ORNE      OPA(X)
     C*** End add ***** 08/20/01 **************************************
     C*** BEG ADD ****** 10/19/07 ************************************
     C     EN(X)         ORNE      OEN(X)
     C*** END ADD ****** 10/19/07 ************************************
     C     AM(X)         ORNE      OAM(X)
     C     ST(X)         ORNE      OST(X)
     C*
     C*  IF THE SEQ# IS 0, RETRIEVE THE NEXT AVAILABLE SEQ#.
     C*
     C     SQ(X)         IFEQ      0
     C     BNKY1         SETGT     PBN01
     C     BNKY1         READPE(N) PBN01                                  60
     C  N60BNSEQ#        ADD       1             SQ(X)
     C   60              Z-ADD     1             SQ(X)
     C                   END
     C*
     C*  WRITE A NEW RECORD OR UPDATE AN EXISTING RECORD.
     C*
     C                   MOVEL     SQ(X)         XXSEQ#
     C     BNKY          CHAIN     PBN01                              40
     C*
     C                   Z-ADD     XXCO#         BNCO#
     C                   Z-ADD     XXFND         BNFND
     C                   Z-ADD     CMFYR         BNFYR
     C                   Z-ADD     CMCASE        BNCASE
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BNSTDT
     C                   MOVEL     CD(X)         BNACOD
     C*** Begin add *** 08/20/01 **************************************
     C                   MOVEL(P)  PA(X)         XXWK9A            9
     C                   MOVE      XXWK9A        BNACOD
     C*** End add ***** 08/20/01 **************************************
     C                   Z-ADD     AM(X)         BNAMT
     C                   Z-ADD     XXSEQ#        BNSEQ#
     C                   MOVE      ST(X)         BNSTAT
     C     ST(X)         IFEQ      *BLANK
     C                   MOVE      'A'           BNSTAT
     C                   END
     C*** BEG ADD ****** 10/19/07 ************************************
     C                   Z-ADD     EN(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BNENDT
     C*** END ADD ****** 10/19/07 ************************************
     C*
     C                   MOVE      Q1USER        BNUSER
     C                   MOVE      Q1PGM         BNPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BNDT
     C                   Z-ADD     DSTIME        BNTIME
     C*
     C     *IN40         IFEQ      '0'
     C*** END ADD *** 12/04/08 **************************************
     C     SCDLT         IFEQ      'Y'
     C     BNSTAT        ANDEQ     'I'
     C                   DELETE    PBN01
     C                   ELSE
     C*** END ADD *** 12/04/08 **************************************
     C                   UPDATE    PBN01
     C*** END ADD *** 12/04/08 **************************************
     C                   ENDIF
     C*** END ADD *** 12/04/08 **************************************
     C                   ELSE
     C*** BEG ADD *** 12/04/08 **************************************
     C     SCDLT         IFNE      'Y'
     C     BNSTAT        ANDEQ     'A'
     C*** END ADD *** 12/04/08 **************************************
     C                   WRITE     PBN01
     C*** BEG ADD *** 12/04/08 **************************************
     C                   ENDIF
     C*** END ADD *** 12/04/08 **************************************
     C                   END
     C*
     C*  MOVE NEW FIELDS TO OLD FIELDS
     C*
     C                   MOVE      DT(X)         ODT(X)
     C*** BEG ADD ****** 10/19/07 ************************************
     C                   MOVE      EN(X)         OEN(X)
     C*** END ADD ****** 10/19/07 ************************************
     C                   MOVE      CD(X)         OCD(X)
     C*** Begin add *** 08/20/01 **************************************
     C                   MOVE      PA(X)         OPA(X)
     C*** End add ***** 08/20/01 **************************************
     C                   MOVE      AM(X)         OAM(X)
     C                   MOVE      ST(X)         OST(X)
     C*
     C                   END
     C                   END
     C*
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(8)         KEYS(2)
     C*
     C                   ENDSR
     C**************************************************************************
**
ROLL UP
ROLL DOWN
** PGM DESC
     Benefit Adjustment Maintenance
       Benefit Adjustment Inquiry
