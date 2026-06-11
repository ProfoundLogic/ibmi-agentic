     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE W0585R - DEDUCTIBLE DETAIL SEARCH
     H Bnddir('WTBNDDIR')
      *****************************************************************
      *                                                               *
      *   PROGRAM DESCRIPTION:                                        *
      *     THIS PROGRAM SHOWS ALL DEDUCTIBLES BILLED FOR A CLAIM.    *
      *     IT IS ACCESSED FROM CLAIM MASTER MAINTENANCE.             *
      *                                                               *
      *   CALLED FROM:                                                *
      *     W0070R06 - CLAIM MASTER MAINTENANCE                       *
      *                                                               *
      *   CALLS TO:                                                   *
      *     NONE                                                      *
      *                                                               *
      *   PROGRAMMER: C:24                                            *
      *   CREATED:    02/28/95    PROJECT:WC311                       *
      *                                                               *
      * ***************************************************************
      * REVISIONS:                                                    *
      *                                                                   *
      * 06/09/20 C:298 Project: SY420
      *                Mask SSN
      *                                                               *
      * 04/07/05     PROJECT: 126WDK                                  *
      *        C:33  PROJECT: 126WCD                                  *
      *              Multiple Deductible discounts per Year.          *
      *                                                               *
      *   05/29/98 C:65 PROJECT: WC?                                  *
      *              REMOVE JURISDICTION STATE MOVE FROM CLAIMS.      *
      *                                                               *
      *   06/19/97   47   60ANC                                       *
      *              MADE SCREEN INTO A WINDOW.                       *
      * ***************************************************************
      * INDICATORS:                                                   *
      *   59 - NET REPORTING - SHOW BALANCE INFORMATION               *
      * ***************************************************************
     FW0585F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMCDL     IF   E           K DISK
     FWDDDL4    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWTDDL2    IF   E           K DISK
     F                                     RENAME(PDD01:TDD01)

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D KEYF            S              3  0 DIM(2)
     D KEYI            S              8  0 DIM(2)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FY                     1     30  0
     D                                     DIM(10)
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
     D                 DS
     D  ID                     1    080  0
     D                                     DIM(10)
     D  SCID01               001    008  0
     D  SCID02               009    016  0
     D  SCID03               017    024  0
     D  SCID04               025    032  0
     D  SCID05               033    040  0
     D  SCID06               041    048  0
     D  SCID07               049    056  0
     D  SCID08               057    064  0
     D  SCID09               065    072  0
     D  SCID10               073    080  0
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
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
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
     D                 DS
     D  BA                     1     90  2
     D                                     DIM(10)
     D  SCBA01                 1      9  2
     D  SCBA02                10     18  2
     D  SCBA03                19     27  2
     D  SCBA04                28     36  2
     D  SCBA05                37     45  2
     D  SCBA06                46     54  2
     D  SCBA07                55     63  2
     D  SCBA08                64     72  2
     D  SCBA09                73     81  2
     D  SCBA10                82     90  2
     D                 DS
     D  DC                     1     20
     D                                     DIM(10)
     D  SCDC01                 1      2
     D  SCDC02                 3      4
     D  SCDC03                 5      6
     D  SCDC04                 7      8
     D  SCDC05                 9     10
     D  SCDC06                11     12
     D  SCDC07                13     14
     D  SCDC08                15     16
     D  SCDC09                17     18
     D  SCDC10                19     20
     D                 DS
     D  XXSS2                  1      9
     D  XXSS01                 1      3
     D  XXSS02                 4      5
     D  XXSS03                 6      9
     D                 DS
     D  SCSS2                  1     11
     D  XXSS21                 1      3
     D  XXHYP3                 4      4
     D  XXSS22                 5      6
     D  XXHYP4                 7      7
     D  XXSS23                 8     11
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D OPTNM           C                   CONST('Deductible History')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display- ')
     D DBFDS           DS
     D  DBRRN                397    400B 0
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
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  RATE SET CODE TABLE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY             3
     C*
     C*  KEY LIST FOR DEDUCTIBLE DETAIL FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXBFYR
     C                   KFLD                    XXINDT
     C*
     C     DDKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR DEDUCTIBLE CODE FILE
     C*
     C     DDKYT         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C***  BEGIN CHG  ***  04/07/05 *************************************
     C**********           KFLD           ELDCDE
     C**********           KFLD           ELDCEF
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDEF
     C***  END   CHG  ***  04/07/05 *************************************
     C*
     C     DDKYT2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C***  BEGIN CHG  ***  04/07/05 *************************************
     C**********           KFLD           ELDCDE
     C                   KFLD                    XXDDCD
     C***  END   CHG  ***  04/07/05 *************************************
     C*
     C     *LIKE         DEFINE    DDCO#         XXCO#
     C     *LIKE         DEFINE    DDFND         XXFND
     C     *LIKE         DEFINE    DDFYR         XXFYR
     C     *LIKE         DEFINE    DDEMP#        XXEMP#
     C     *LIKE         DEFINE    DDDIV         XXDIV
     C     *LIKE         DEFINE    DDCASE        XXCASE
     C     *LIKE         DEFINE    DDBFYR        XXBFYR
     C     *LIKE         DEFINE    DDPODT        XXINDT
     C     *LIKE         DEFINE    DDST          XXST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
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
     C                   CLEAR                   SCR01
     C*
     C                   MOVEL(P)  MODED         SCMODE
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         SCPGM
     C                   MOVEL     Q1USER        SCUSER
     C************         MOVE QQPRDS    SCPRDS
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      '-'           XXHYP3            1
     C                   MOVE      '-'           XXHYP4            1
     C*
     C     TOP           TAG
     C*
     C     UPKY          CHAIN     PUP01                              60
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQFND         SCFND
     C******               MOVE QQEMP#    SCEMP#
     C******               MOVE QQDIV     SCDIV
     C***********          MOVE QQFYR     SCYR
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C     EMKY          CHAIN     PEM01                              60
     C***********          MOVELEMNAME    SCENAM
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C*
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      ELRTCD        XXKEY
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*** BEGIN DEL ***  05/29/98  ************************************
     C*****                MOVE CMJRST    XXST
     C*** END DEL *****  05/29/98  ************************************
     C***  BEGIN DEL  ***  04/07/05 *************************************
     C********** DDKYT     SETGTTDD01
     C********** DDKYT2    REDPETDD01                    60
     C********** DDTYPE    IFEQ 'N'
     C**********           MOVE *ON       *IN59
     C**********           ELSE
     C**********           MOVE *OFF      *IN59
     C**********           ENDIF
     C***59*****           EXSR GETBAL
     C***  END   DEL  ***  04/07/05 *************************************
     C     CMKY          CHAIN     PCM01                              60
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C     *LIKE         DEFINE    ELDCDE        XXDDCD
     C     *LIKE         DEFINE    ELDCEF        XXDDEF
     C     *LIKE         DEFINE    ELDCEX        XXDCEX
     C     *LIKE         DEFINE    EL2DEX        XX2DEX
     C*
     C                   Z-ADD     ELDCEX        XXDCEX
     C                   Z-ADD     EL2DEX        XX2DEX
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        XXDCEX
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        XX2DEX
     C                   ENDIF
     C*
     C                   SELECT
     C     ELDCD2        WHENNE    *BLANK
     C     EL2DEF        ANDLE     CMACDT
     C     XX2DEX        ANDGE     CMACDT
     C                   MOVE      ELDCD2        XXDDCD
     C                   MOVE      EL2DEF        XXDDEF
     C     ELDCDE        WHENNE    *BLANK
     C     ELDCEF        ANDLE     CMACDT
     C     XXDCEX        ANDGE     CMACDT
     C                   MOVE      ELDCDE        XXDDCD
     C                   MOVE      ELDCEF        XXDDEF
     C                   ENDSL
     C*
     C     DDKYT         SETGT     TDD01
     C     DDKYT2        READPE    TDD01                                  60
     C*
     C                   SELECT
     C     DDTYPE        WHENEQ    'N'
     C                   MOVE      *ON           *IN59
     C                   EXSR      GETBAL
     C                   OTHER
     C                   MOVE      *OFF          *IN59
     C                   ENDSL
     C***  END   ADD  ***  04/07/05 *************************************
     C*
     C*************        MOVE CMCASE    SCCASE
     C*                    MOVE CMSS#     XXSS2
     C*                    MOVE XXSS01    XXSS21
     C*                    MOVE XXSS02    XXSS22
     C*                    MOVE XXSS03    XXSS23
     C*          CDKY      CHAINPCD01                61
     C*                    MOVE *BLANKS   XXLAST 56
     C*                    MOVE *BLANKS   XXFRST 15
     C*                    MOVELCDFSN     XXFRST
     C*                    MOVELCDLSN     XXLAST
     C*                    MOVE '1'       XXTERM  1
     C*                    CALL 'SRFRLS'
     C*                    PARM           XXLAST
     C*                    PARM           XXFRST
     C*                    PARM           XXTERM
     C**************       MOVELXXLAST    SCCLNM
     C                   Z-ADD     DBRRN         XXRRN             9 0
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scss# = DisplaySS#(MaskData('SSN':'cmss#':cmss#));
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C                   END
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C     CMEXCS        IFNE      'Y'
     C     XXRRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    XXRRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
      *
      *  CLAIMANT NAME
      *
     C     CDKY          CHAIN     PCD01                              61
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C                   END
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
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
      *
     C                   MOVE      *ZEROS        SCBFYR
     C                   MOVE      *ZEROS        SCINDT
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      '1'           *IN21
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C*
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
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
     C                   MOVE      *BLANK        XXDOWN            1
     C                   Z-ADD     KEYF(2)       XXBFYR
     C                   Z-ADD     KEYI(2)       X1INDT            8 0
     C*
     C                   Z-ADD     X1INDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXINDT
     C*
     C     DDKY          CHAIN     PDD01                              60
     C     DDKY2         READE     PDD01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYF(1)       XXBFYR
     C                   Z-ADD     KEYI(1)       X1INDT
     C*
     C                   Z-ADD     X1INDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXINDT
     C*
     C     DDKY          CHAIN     PDD01                              60
     C  N60              READP     PDD01                                  60
     C  N60DDCO#         COMP      XXCO#                              6060
     C  N60DDFND         COMP      XXFND                              6060
     C  N60DDFYR         COMP      XXFYR                              6060
     C  N60DDCASE        COMP      XXCASE                             6060
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCBFYR        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANKS       XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        KEYI
     C                   Z-ADD     SCBFYR        XXBFYR
     C*
     C                   Z-ADD     SCINDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXINDT
     C*
     C                   MOVE      *ZEROS        SCBFYR
     C                   MOVE      *ZEROS        SCINDT
     C     DDKY          SETLL     PDD01
     C     DDKY2         READE     PDD01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
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
     C                   MOVE      *BLANKS       ID
     C                   MOVE      *BLANKS       AM
     C                   MOVE      *BLANKS       DC
     C                   MOVE      *BLANKS       BA
     C                   MOVE      *BLANKS       AR
     C                   Z-ADD     *ZERO         I1                3 0
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     DDBFYR        FY(I1)
     C*
     C                   Z-ADD     DDPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ID(I1)
     C*
     C                   MOVE      DDRECD        DC(I1)
     C                   Z-ADD     DDAMTB        AM(I1)
     C     *IN59         IFEQ      *ON                                          NET REPORTING
     C                   Z-ADD     DDAMTB        BA(I1)
     C                   Z-ADD     DDAMTR        AR(I1)
     C                   SUB       DDAMTR        BA(I1)
     C                   ENDIF
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
     C     DDKY2         READE     PDD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     FY(1)         KEYF(1)
     C                   Z-ADD     ID(1)         KEYI(1)
     C                   Z-ADD     FY(10)        KEYF(2)
     C                   Z-ADD     ID(10)        KEYI(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     10            I1
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
     C                   READP     PDD01                                  60
     C  N60DDCO#         COMP      XXCO#                              6060
     C  N60DDFND         COMP      XXFND                              6060
     C  N60DDFYR         COMP      XXFYR                              6060
     C  N60DDCASE        COMP      XXCASE                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     FY(1)         KEYF(1)
     C                   Z-ADD     ID(1)         KEYI(1)
     C                   Z-ADD     FY(10)        KEYF(2)
     C                   Z-ADD     ID(10)        KEYI(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZEROS        XXBFYR
     C                   Z-ADD     *ZEROS        XXINDT
     C     DDKY          SETLL     PDD01
     C     DDKY2         READE     PDD01                                  60
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
     C     DDKY2         READE     PDD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*  ACCUMULATE TOTAL BILLED, PAID, AND BALANCE.
     C*
     C     GETBAL        BEGSR
     C                   CLEAR                   SCBILL
     C                   CLEAR                   SCPAID
     C                   CLEAR                   SCBAL
     C*
     C     DDKY2         SETLL     PDD01
     C     DDKY2         READE     PDD01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   ADD       DDAMTB        SCBILL
     C                   ADD       DDAMTB        SCBAL
     C                   ADD       DDAMTR        SCPAID
     C                   SUB       DDAMTR        SCBAL
     C     DDKY2         READE     PDD01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
**
ROLL UP
ROLL DOWN
