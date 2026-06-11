     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE W4315R - CLAIMANT WORK HISTORY MAINTENANCE/INQUIRY
      * ***************************************************************
      * DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER           *
      *               WORK HISTORY RECORDS ON A CLAIM.                *
      *                                                               *
      * CALLED FROM:  W0070R11                                        *
      * CALLS TO:     NONE                                            *
      * ***************************************************************
      * REVISIONS:                                                    *
      *                                                               *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
      * 08/29/11  C:190 Project: WC2067                               **
      *                 Call API WA066R to calculate the days lost.   *
      *                                                               *
      * 05/04/09  C:62 Project: WC1807                                **
      *                Add Release to RTW processing.                 *
      *                                                               *
      * 05/20/08  C:53 Project: CI054                                 **
      *                If called from an option (W0070R) we will update
      *                CMTADL/WMCMP. If called from a FKey (W0070R11)  *
      *                pass the value back so the claim pgm can update
      *                without causing the user to refresh.
      *
      * ***************************************************************
     FW4315F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMWHL     UF A E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
      *** BEG ADD *** 05/20/08 ********************************
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:PCM02)
      *** END ADD *** 05/20/08 ********************************
     FWMCDL     IF   E           K DISK
     F*** BEGIN ADD *** 05/04/09 **************************************
     FWTGTL     IF   E           K DISK
     F*** END   ADD *** 05/04/09 **************************************
      *

     D*** BEGIN CHG *** 05/04/09 **************************************
     D**********          WK         12  8 0
     D**********          ST         12  1
     D**********          OWK        12  8 0
     D OWK             S              8  0 DIM(10)
     D OST             S              1    DIM(10)
     D*** END   CHG *** 05/04/09 **************************************
     D*** BEGIN ADD *** 05/04/09 **************************************
     D HRQ             S             10    DIM(10) CTDATA PERRCD(1)
     D PR              S              1    DIM(999)
     D*** END   ADD *** 05/04/09 **************************************
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYD            S              8  0 DIM(2)
     D LR              S              7  0 DIM(198)                             ALL WHDATE DATES
     D L               S              7  0 DIM(99)                              LOOSING TIME
     D R               S              7  0 DIM(99)                              RETURNED TO WORK
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN CHG *** 05/04/09 **************************************
     D**********  DS
     D**********                              1 096 WK
     D**********                            001 0080SCWK01
     D**********                            009 0160SCWK02
     D**********                            017 0240SCWK03
     D**********                            025 0320SCWK04
     D**********                            033 0400SCWK05
     D**********                            041 0480SCWK06
     D**********                            049 0560SCWK07
     D**********                            057 0640SCWK08
     D**********                            065 0720SCWK09
     D**********                            073 0800SCWK10
     D**********                            081 0880SCWK11
     D**********                            089 0960SCWK12
     D**********  DS
     D**********                              1  12 ST
     D**********                              1   1 SCST01
     D**********                              2   2 SCST02
     D**********                              3   3 SCST03
     D**********                              4   4 SCST04
     D**********                              5   5 SCST05
     D**********                              6   6 SCST06
     D**********                              7   7 SCST07
     D**********                              8   8 SCST08
     D**********                              9   9 SCST09
     D**********                             10  10 SCST10
     D**********                             11  11 SCST11
     D**********                             12  12 SCST12
     D                 DS
     D  WK                     1    080  0
     D                                     DIM(10)
     D  SCWK01               001    008  0
     D  SCWK02               009    016  0
     D  SCWK03               017    024  0
     D  SCWK04               025    032  0
     D  SCWK05               033    040  0
     D  SCWK06               041    048  0
     D  SCWK07               049    056  0
     D  SCWK08               057    064  0
     D  SCWK09               065    072  0
     D  SCWK10               073    080  0
     D                 DS
     D  ST                     1     10
     D                                     DIM(10)
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
     D*** END   CHG *** 05/04/09 **************************************
     D*** BEGIN ADD *** 05/04/09 **************************************
     D                 DS
     D  RQ                     1     20
     D                                     DIM(10)
     D  SCRQ01                 1      2
     D  SCRQ02                 3      4
     D  SCRQ03                 5      6
     D  SCRQ04                 7      8
     D  SCRQ05                 9     10
     D  SCRQ06                11     12
     D  SCRQ07                13     14
     D  SCRQ08                15     16
     D  SCRQ09                17     18
     D  SCRQ10                19     20
     D                 DS
     D  QD                     1    250
     D                                     DIM(10)
     D  SCQD01                 1     25
     D  SCQD02                26     50
     D  SCQD03                51     75
     D  SCQD04                76    100
     D  SCQD05               101    125
     D  SCQD06               126    150
     D  SCQD07               151    175
     D  SCQD08               176    200
     D  SCQD09               201    225
     D  SCQD10               226    250
     D*** END   ADD *** 05/04/09 **************************************
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D XCEXRE          C                   CONST('EXCS REIN ')
     D OPTNM           C                   CONST('Work History')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display- ')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0

     C*
     C*  CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*
     C*   KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  WORK HISTORY FILE
     C*
     C     WHKEY1        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     WHKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDATE
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*
     C     WHKEY2        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDATE
     C                   KFLD                    XXWKST
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C     *LIKE         DEFINE    WHDT          XXDATE
     C     *LIKE         DEFINE    WHDT          XXWKDT
     C     *LIKE         DEFINE    WHDT          SVWKDT
     C     *LIKE         DEFINE    WHFND         XXFND
     C     *LIKE         DEFINE    WHFYR         XXFYR
     C     *LIKE         DEFINE    WHCASE        XXCASE
     C*** BEGIN ADD *** 05/04/09 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END   ADD *** 05/04/09 **************************************
     C*** BEGIN ADD *** 05/04/09 **************************************
     C     *LIKE         DEFINE    WHWKST        XXWKST
     C     *LIKE         DEFINE    WHDT          SVWKD2
     C     *LIKE         DEFINE    WHWKST        SVWKST
     C     *LIKE         DEFINE    WHWKST        SVWKS2
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
      *** BEG ADD *** 05/20/08 ****************************
     C                   PARM                    QQTADL            4 0
     C                   PARM                    QQUPDC            1            UPDATE CLAIM
      *** END ADD *** 05/20/08 ****************************
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
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   Z-ADD     1             PG                3 0
     C                   MOVE      *OFF          LOST              1
     C                   MOVE      *OFF          RTW               1
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET THE CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C                   Z-ADD     DBRRN         XXRRN                          FOR WMCMP
     C                   Z-ADD     CMLOST        SCLOST
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*
     C* POPULATE THE WORK DATE ARRAY
     C*
     C*** BEG   CHG *** 08/29/11 **************************************
     C**************     EXSR      POPWDA
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   Z-ADD     CMRTW         ##RTW
     C                   Z-ADD     CMDBDT        ##DDB
      *
     C                   CALL      'WA066R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##RTW             7 0
     C                   PARM                    ##DDB             7 0
     C                   PARM      0             ##TOTD            5 0
     C                   PARM      ' '           @@WCEP            1
     C                   Z-ADD     ##TOTD        SCTADL
     C*** END   CHG *** 08/29/11 **************************************
     C*** END   ADD *** 05/04/09 **************************************
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
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVEL(P)  MODEC         SCMODE
      *  DATE OF DISABILITY
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDBDT
      *  RTW DATE
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTW
      *
      *  IF THERE IS NO RETURNED TO WORK DATE, DO NOT ALLOW ENTRY.
      *
     C*** BEGIN DEL *** 05/04/09 **************************************
     C********** CMRTW     IFEQ *ZERO
     C**********           MOVE '1'       QQCALL
     C**********           MOVE *ON       *IN63
     C**********           END
     C*** END   DEL *** 05/04/09 **************************************
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *BLANKS       SCCNAM
     C     CDLSN         CAT       ',':0         SCCNAM
     C                   CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C                   CAT       CDMID:1       SCCNAM
     C                   CAT       '.':0         SCCNAM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        SCUSER
     C     QQCALL        COMP      '1'                                    09
     C   09              MOVEL(P)  MODED         SCMODE
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCWDAT
     C                   GOTO      REPOS
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
      *** BEG DEL *** 08/29/11 ********************************
     C***********        EXSR      SRDAYS
      *** END DEL *** 08/29/11 ********************************
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*
     C*   COMMAND 10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C                   EXSR      KJDO
     C                   GOTO      STRT
     C                   END
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   MOVE      'B'           QQRTCD
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN10
     C*
     C                   END                                                    *IN11
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   EXSR      RITE
     C     XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     *IN19         IFEQ      '1'
     C                   EXSR      RITE
     C     XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C*** BEGIN CHG *** 05/04/09 **************************************
     C                   MOVE      ST(10)        PR(PG)
     C                   ADD       1             PG
     C*** END   CHG *** 05/04/09 **************************************
     C                   MOVE      *BLANK        XXDOWN
     C*
     C                   Z-ADD     KEYD(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     WHKEY         CHAIN     PWH01                              60
     C     WHKEY1        READE     PWH01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C*** BEGIN CHG *** 05/04/09 **************************************
     C                   SUB       1             PG
     C*** END   CHG *** 05/04/09 **************************************
     C*
     C                   Z-ADD     KEYD(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     WHKEY         CHAIN     PWH01                              60
     C                   READP     PWH01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCWDAT        IFNE      *ZEROS
     C                   EXSR      RITE
     C     REPOS         TAG
     C                   Z-ADD     0             KEYD
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      '1'           XXDOWN
     C*
     C                   Z-ADD     SCWDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   Z-ADD     *ZERO         SCWDAT
     C     WHKEY         SETLL     PWH01
     C     WHKEY1        READE     PWH01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF ENTER WAS PRESSED EXECUTE THE RITE SUBROUTINE
     C*
     C     QQCALL        IFNE      '1'
     C                   EXSR      RITE
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
      *** BEG ADD *** 05/20/08 ********************************
     C     Q1PRMS        IFGT      6
     C     QQUPDC        IFNE      'N'
     C     XXRRN         CHAIN     PCM02                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   Z-ADD     SCTADL        CMTADL
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
     C                   UPDATE    PCM02
     C                   ENDIF
     C*
     C                   ELSE
     C                   Z-ADD     SCTADL        QQTADL
     C                   ENDIF
     C                   ENDIF
      *** END ADD *** 05/20/08 ********************************
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*================================================================
     C* CHKSTS - CHECK WORK STATUS TO MAKE SURE THEY
     C*          ARE IN THE CORRECT ORDER
     C*================================================================
     C     CHKSTS        BEGSR
     C*
     C                   MOVEL(P)  *BLANKS       STSERR            1
     C     X             SUB       1             PL                2 0
     C*
B01  C     X             IFEQ      1
     C*
B02  C     PG            IFEQ      1
     C     ST(1)         IFNE      *BLANKS
     C     ST(2)         ORNE      *BLANKS
B03  C     ST(X)         IFNE      'E'
     C     ST(X)         ANDNE     'L'
     C                   MOVEL     'L'           STSERR
E03  C                   ENDIF
     C                   ENDIF
     C*
S02  C                   ELSE
     C*
     C     PG            SUB       1             PP                3 0
B04  C     ST(X)         IFEQ      'L'
     C     PR(PP)        ANDEQ     'L'
     C                   MOVEL     'E'           STSERR
E04  C                   ENDIF
E02  C                   ENDIF
     C*
S01  C                   ELSE
     C*
B05  C     ST(X)         IFEQ      'L'
     C     ST(PL)        ANDEQ     'L'
     C                   MOVEL     'E'           STSERR
E05  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
      *** BEG DLT *** 08/29/11 ********************************
     C*================================================================
     C* POPWDA - POPULATE WORK DATE ARRAYS
     C*================================================================
     C*****POPWDA        BEGSR
     C***********
     C***********        Z-ADD     0             Y
     C***********
     C*****WHKEY1        CHAIN(N)  PWH01                              99
     C******IN99         DOWEQ     *OFF
     C*******
     C*****LOST          IFEQ      *OFF
     C*****WHWKST        IFEQ      'L'
     C***********        ADD       1             Y
     C***********        Z-ADD     WHDATE        LR(Y)
     C***********        MOVE      *ON           LOST
     C***********        ENDIF
     C***********        ELSE
     C*****WHWKST        IFEQ      'R'
     C*****RTW           IFEQ      *OFF
     C************       ADD       1             Y
     C************       Z-ADD     WHDATE        LR(Y)
     C************       MOVE      *ON           RTW
     C************       ELSE
     C************       Z-ADD     WHDATE        LR(Y)
     C************       ENDIF
     C************       ELSE
     C*****WHWKST        IFEQ      'L'
     C************       ADD       1             Y
     C************       Z-ADD     WHDATE        LR(Y)
     C************       MOVE      *OFF          RTW
     C************       ENDIF
     C************       ENDIF
     C************       ENDIF
     C************
     C*****WHKEY1        READE(N)  PWH01                                  99
     C************       ENDDO
     C************
     C************       ENDSR
      *** END DLT *** 08/29/11 ********************************
     C*** END   ADD *** 05/04/09 **************************************
     C* ================================================================
     C*  CLEAR THE SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C*
     C                   MOVE      *BLANKS       WK
     C                   MOVE      *BLANKS       ST
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVE      *BLANKS       RQ
     C                   MOVE      *BLANKS       QD
     C*** END   ADD *** 05/04/09 **************************************
     C*  CLEAR HOLD ARRAYS
     C                   MOVE      *BLANKS       OWK
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVE      *BLANKS       OST
     C*** END   ADD *** 05/04/09 **************************************
     C                   Z-ADD     *ZERO         I1                2 0          ARRAY INDEX
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C*
     C                   Z-ADD     WHDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WK(I1)
     C*
     C                   MOVE      WHWKST        ST(I1)
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVE      WHRTWQ        RQ(I1)
     C                   MOVEL(P)  'RTWQ'        KKTBID
     C                   MOVEL(P)  WHRTWQ        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        QD(I1)
     C                   ELSE
     C                   MOVEL     *BLANKS       QD(I1)
     C                   ENDIF
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C* ACCUMULATE WORK HISTORY DATES TO LOAD INTO  LR  ARRAY.
     C*
     C**         FIRST     IFEQ *BLANK
     C**                   Z-ADD1         Y
     C**                   Z-ADDCMDBDT    LR,Y
     C**                   MOVE 'N'       FIRST   1
     C**                   END
     C*
     C*** BEGIN DEL *** 05/04/09 **************************************
     C**********           ADD  1         Y
     C**********           Z-ADDWHDATE    LR,Y
     C*** END   DEL *** 05/04/09 **************************************
     C*
     C                   EXCEPT    PWH01E
     C*  FILL HOLD ARRAYS
     C                   MOVE      WK(I1)        OWK(I1)
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVE      ST(I1)        OST(I1)
     C*** END   ADD *** 05/04/09 **************************************
     C                   ENDSR
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   Z-ADD     1             X
     C     HFLD          LOOKUP    HRQ(X)                                 99
     C     *IN99         IFEQ      *ON
     C*
     C     ST(X)         IFEQ      'L'
     C                   ELSE
     C                   MOVEL     ST(X)         ##FG03
     C*
     C                   CALL      'W2493R'
     C                   PARM      QQPRDS        ##PRDS           40
     C                   PARM      '2'           ##CALL            1
     C                   PARM      'RTWQ'        ##TBID            4
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##FG03            1
     C*
     C*** BEG   CHG *** 08/29/11 **************************************
     C**********     X             ADD       35            Y
     C     X             ADD       35            Y                 3 0
     C*** END   CHG *** 08/29/11 **************************************
     C     ##KEY         IFEQ      *BLANKS
     C                   SETON                                        33
     C                   ELSE
     C                   MOVEL     ##KEY         RQ(X)
     C                   MOVEL(P)  'RTWQ'        KKTBID
     C                   MOVEL(P)  RQ(X)         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        QD(X)
     C                   ELSE
     C                   MOVEL     *BLANKS       QD(X)
     C                   ENDIF
     C     X             IFLT      10
     C                   ADD       1             Y
     C                   ENDIF
     C                   SETON                                        32
     C                   ENDIF
     C*
     C     X             IFLT      10
     C     X             OREQ      10
     C     ##KEY         ANDEQ     *BLANKS
     C                   MOVEL     *ON           *IN(Y)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 05/04/09 **************************************
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
     C     WHKEY1        READE     PWH01                                  60
     C   60              DO
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** I1        IFLT 12
     C     I1            IFLT      10
     C*** END   CHG *** 05/04/09 **************************************
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONE
     C                   END
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** I1        COMP 12                     61
     C     I1            COMP      10                                   61
     C*** END   CHG *** 05/04/09 **************************************
     C   61              GOTO      OVER
     C     DONE          TAG
     C  N60              EXCEPT    PWH01E
     C     I1            IFNE      0
     C                   Z-ADD     WK(1)         KEYD(1)
     C*** BEGIN CHG *** 05/04/09 **************************************
     C**********           Z-ADDWK,12     KEYD,2
     C                   Z-ADD     WK(10)        KEYD(2)
     C*** END   CHG *** 05/04/09 **************************************
     C                   END
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C*** BEGIN CHG *** 05/04/09 **************************************
     C**********           Z-ADD13        I1
     C                   Z-ADD     11            I1
     C*** END   CHG *** 05/04/09 **************************************
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
     C                   READP     PWH01                                  60
     C  N60              DO
     C     WHCO#         IFNE      UPCO#
     C     WHFND         ORNE      XXFND
     C     WHFYR         ORNE      XXFYR
     C     WHCASE        ORNE      XXCASE
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C  N60              EXCEPT    PWH01E
     C                   Z-ADD     WK(1)         KEYD(1)
     C*** BEGIN CHG *** 05/04/09 **************************************
     C**********           Z-ADDWK,12     KEYD,2
     C                   Z-ADD     WK(10)        KEYD(2)
     C*** END   CHG *** 05/04/09 **************************************
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
     C                   Z-ADD     0             XXDATE
     C     WHKEY         SETLL     PWH01
     C     WHKEY1        READE     PWH01                                  60
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
     C     WHKEY1        READE     PWH01                                  60
     C   60              DO
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** I1        IFLT 12
     C     I1            IFLT      10
     C*** END   CHG *** 05/04/09 **************************************
     C                   MOVE      '1'           XXUP
     C                   END
     C                   GOTO      DONEB
     C                   END
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** I1        COMP 12                     61
     C     I1            COMP      10                                   61
     C*** END   CHG *** 05/04/09 **************************************
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C**************************************************************
     C*     RITE SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     RITE          BEGSR
     C*
     C*** BEGIN CHG *** 05/04/09 **************************************
     C**********           DO   12        X       20
     C                   DO        10            X                 2 0
     C*** END   CHG *** 05/04/09 **************************************
     C     X             ADD       35            X2                2 0          ERROR
     C     X             ADD       47            X3                2 0          STATUS ERROR
     C     X             ADD       69            X4                2 0          CHRON  ERROR
     C*** BEGIN ADD *** 05/04/09 **************************************
     C     X             ADD       81            X5                2 0          STATUS/DATE
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*   WORK DATE MUST BE VALID
     C*
     C     WK(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     WK(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         WK(X)
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  THE FIRST WORK DATE MUST BE AFTER THE ACTUAL RTW DATE.
     C*
     C*** BEGIN DEL *** 05/04/09 **************************************
     C********** X         IFEQ 1
     C**********           Z-ADDWK,X      SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXWKDT
     C*
     C********** XXWKDT    IFLE CMRTW
     C**********           MOVE '1'       *IN62
     C**********           GOTO STRT
     C**********           END
     C*
     C**********           Z-ADDXXWKDT    SVWKDT
     C**********           END
     C*** END   DEL *** 05/04/09 **************************************
     C*
     C*  THE WORK DATES MUST BE IN CHRONOLOGICAL ORDER.
     C*
     C     X             IFGT      1
     C                   Z-ADD     WK(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWKDT
     C*
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** XXWKDT    IFLE SVWKDT
     C     XXWKDT        IFLT      SVWKDT
     C*** END   CHG *** 05/04/09 **************************************
     C     XXWKDT        ANDNE     *ZERO
     C                   MOVE      '1'           *IN(X4)
     C                   GOTO      STRT
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   ELSE
     C     XXWKDT        IFEQ      SVWKDT
     C     XXWKDT        ANDNE     *ZERO
     C     ST(X)         ANDEQ     SVWKST
     C     XXWKDT        OREQ      SVWKD2
     C     XXWKDT        ANDNE     *ZERO
     C     ST(X)         ANDEQ     SVWKS2
     C     XXWKDT        OREQ      SVWKDT
     C     XXWKDT        ANDNE     *ZERO
     C     ST(X)         ANDNE     'E'
     C     ST(X)         ANDNE     'R'
     C                   MOVE      '1'           *IN(X5)
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 05/04/09 **************************************
     C                   END
     C*
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   Z-ADD     SVWKDT        SVWKD2
     C*** END   ADD *** 05/04/09 **************************************
     C                   Z-ADD     XXWKDT        SVWKDT
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVEL(P)  SVWKST        SVWKS2
     C                   MOVEL(P)  ST(X)         SVWKST
     C*** END   ADD *** 05/04/09 **************************************
     C                   ELSE
     C                   Z-ADD     WK(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWKDT
     C*
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   Z-ADD     SVWKDT        SVWKD2
     C*** END   ADD *** 05/04/09 **************************************
     C                   Z-ADD     XXWKDT        SVWKDT
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVEL(P)  SVWKST        SVWKS2
     C                   MOVEL(P)  ST(X)         SVWKST
     C*** END   ADD *** 05/04/09 **************************************
     C                   END
     C*
     C*  WORK STATUS MUST BE 'R' 'E' OR 'L'
     C*
     C     ST(X)         IFNE      *BLANK
     C     ST(X)         ANDNE     'R'
     C     ST(X)         ANDNE     'L'
     C*** BEGIN ADD *** 05/04/09 **************************************
     C     ST(X)         ANDNE     'E'
     C*** END   ADD *** 05/04/09 **************************************
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*** BEGIN DEL *** 05/04/09 **************************************
     C*
     C*  THE FIRST WORK STATUS ENTRY MUST BE 'L'
     C*
     C********** X         IFEQ 1
     C********** ST,X      ANDNE'L'
     C**********           MOVE '1'       *IN29
     C**********           GOTO STRT
     C**********           END
     C*
     C*  THE WORK STATUS ENTRY MUST BE 'L'
     C*
     C********** X         IFEQ 3
     C********** X         OREQ 5
     C********** X         OREQ 7
     C********** X         OREQ 9
     C********** X         OREQ 11
     C********** ST,X      IFNE 'L'
     C********** ST,X      ANDNE*BLANK
     C**********           MOVE '1'       *IN,X3
     C**********           GOTO STRT
     C**********           END
     C**********           END
     C*
     C*  THE WORK STATUS ENTRY MUST BE 'R'
     C*
     C********** X         IFEQ 2
     C********** X         OREQ 4
     C********** X         OREQ 6
     C********** X         OREQ 8
     C********** X         OREQ 10
     C********** X         OREQ 12
     C********** ST,X      IFNE 'R'
     C********** ST,X      ANDNE*BLANK
     C**********           MOVE '1'       *IN,X3
     C**********           GOTO STRT
     C**********           END
     C**********           END
     C*** END   DEL *** 05/04/09 **************************************
     C*
     C*  WORK HISTORY DATE ,STATUS, & COMMENTS ARE MUTUALLY INCLUSIVE
     C*
     C     WK(X)         IFNE      *ZEROS
     C     ST(X)         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*
     C     WK(X)         IFEQ      *ZEROS
     C     ST(X)         ANDNE     *BLANK
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD *** 05/04/09 **************************************
     C*
     C* THE STATUS MUST BE IN CORRECT ORDER
     C*
     C                   EXSR      CHKSTS
     C     STSERR        IFNE      *BLANKS
     C                   SELECT
     C     STSERR        WHENEQ    'L'
     C                   SETON                                        26
     C     STSERR        WHENEQ    'E'
     C                   SETON                                        27
     C                   ENDSL
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* RTW QUALIFIER MUST BE VALID IF ENTERED
     C*
     C     RQ(X)         IFNE      *BLANKS
     C                   MOVEL(P)  'RTWQ'        KKTBID
     C                   MOVEL(P)  RQ(X)         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C* RTW QUALIFIER MUST MATCH THE STATUS
     C*
     C     RQ(X)         IFNE      *BLANKS
     C                   MOVEL(P)  'RTWQ'        KKTBID
     C                   MOVEL(P)  RQ(X)         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C     GTFG03        ANDNE     ST(X)
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN(X2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   Z-ADD     OWK(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVEL(P)  OST(X)        XXWKST
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** WHKEY     CHAINPWH01                61
     C     WHKEY2        CHAIN     PWH01                              61
     C*** END   CHG *** 05/04/09 **************************************
     C*
     C     *IN61         IFEQ      '0'
     C     WK(X)         ANDEQ     *ZERO
     C     ST(X)         ANDEQ     *BLANKS
     C                   DELETE    PWH01
     C                   GOTO      NEXT
     C                   END
     C*
     C     WK(X)         IFNE      0
     C*
     C                   MOVE      CMCO#         WHCO#
     C                   MOVE      XXFND         WHFND
     C                   Z-ADD     XXFYR         WHFYR
     C                   Z-ADD     XXCASE        WHCASE
     C*
     C                   Z-ADD     WK(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WHDATE
     C*
     C                   MOVE      ST(X)         WHWKST
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVE      RQ(X)         WHRTWQ
     C*** END   ADD *** 05/04/09 **************************************
     C                   MOVE      *BLANKS       WHSTAT
     C                   MOVEL     Q1USER        WHUSER
     C                   MOVEL     Q1PGM         WHPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WHDT
     C*
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        WHTIME
     C*
     C*  PAYROLL MAINTENANCE SELECTED, UPDATE QUOTE FILE
     C*
     C     *IN61         IFEQ      '0'
     C                   UPDATE    PWH01
     C                   ELSE
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   MOVEL     Q1USER        WHIUSR
     C                   MOVEL     Q1PGM         WHIPGM
     C                   Z-ADD     WHDT          WHIDT
     C                   Z-ADD     WHTIME        WHITIM
     C*** END   ADD *** 05/04/09 **************************************
     C                   WRITE     PWH01
     C                   END
     C*
     C                   END
     C*
     C*  MOVE NEW FIELDS TO OLD FIELDS
     C*
     C     NEXT          TAG
     C                   MOVE      WK(X)         OWK(X)
     C*
     C*** BEGIN CHG *** 05/04/09 **************************************
     C**********           END                             DO 12 X
     C                   ENDDO                                                  DO 10 X
     C*** END   CHG *** 05/04/09 **************************************
     C*
     C*  RELOAD POSITION KEYS
     C*
     C                   Z-ADD     WK(1)         KEYD(1)
     C*** BEGIN CHG *** 05/04/09 **************************************
     C**********           Z-ADDWK,12     KEYD,2
     C                   Z-ADD     WK(10)        KEYD(2)
     C*** END   CHG *** 05/04/09 **************************************
     C*
     C*  ALLOW ROLL UP IF ADDING NEW PAYROLLS AND NEED MORE ENTRY LINES
     C*
     C     *IN18         IFEQ      '1'
     C*** BEGIN CHG *** 05/04/09 **************************************
     C********** WK,12     ANDNE*ZEROS
     C     WK(10)        ANDNE     *ZEROS
     C*** END   CHG *** 05/04/09 **************************************
     C*
     C     XXUP          IFNE      *BLANKS
     C     WHKEY1        SETGT     PWH01
     C                   READP     PWH01                                  60
     C*
     C                   Z-ADD     WHDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   END
     C                   Z-ADD     KEYD(2)       KEYD(1)
     C                   MOVE      *BLANK        XXUP
     C                   END
     C*
     C     RITEN         ENDSR
     C*
      *** BEG DLT *** 08/29/11 ********************************
      * ================================================================
      *  THIS WILL CALCULATE TOTAL ACCUMULATED NUMBER OF DAYS LOST
      * ================================================================
     C*******     SRDAYS        BEGSR
      ***********
     C***********        Z-ADD     1             Y
     C***********        Z-ADD     1             Z                 3 0
     C*****LR(Y)         DOUEQ     *ZERO
     C***********        Z-ADD     LR(Y)         L(Z)
     C***********        ADD       1             Y
     C***********        Z-ADD     LR(Y)         R(Z)
     C***********        ADD       1             Y
     C***********        ADD       1             Z
     C***********        ENDDO
      ***********
     C***********        Z-ADD     1             Y                 3 0
     C***********        Z-ADD     *ZERO         SCTADL
      ***********
     C*****L(Y)          DOUEQ     *ZERO
     C***********        Z-ADD     L(Y)          SRDTI
     C***********        CALL      'SRCVTDAT'    DTMDYY                         7,0 --> 8,8
     C***********        MOVEL     SRDTO         QQFROM            8 0
      ***********
     C***********        Z-ADD     R(Y)          SRDTI
     C***********        CALL      'SRCVTDAT'    DTMDYY
     C***********        MOVEL     SRDTO         QQTO              8 0
      ***********
     C*****QQFROM        IFNE      *ZERO
     C*****QQTO          ANDNE     *ZERO
     C***********        CALL      'SRDDIF8'
     C***********        PARM                    QQFROM
     C***********        PARM                    QQTO
     C***********        PARM      *ZERO         QQDAYS            5 0
     C***********        PARM      *BLANK        QQERR             1
      ***********
     C***********        ADD       QQDAYS        SCTADL
     C***********        ADD       1             Y
     C***********        ELSE
     C***********        LEAVE
     C***********        END
     C***********        ENDDO
      ***********
      *  NOW ADD DAYS BETWEEN DATE OF DISABILITY & RTW DATE
      *
     C***********        Z-ADD     SCDBDT        QQFROM
     C***********        Z-ADD     SCRTW         QQTO
      ***********
     C***********        CALL      'SRDDIF8'
     C***********        PARM                    QQFROM
     C***********        PARM                    QQTO
     C***********        PARM      *ZERO         QADAYS            5 0
     C***********        PARM      *BLANK        QQERR
      ***********
     C***********        ADD       QADAYS        SCTADL
     C***********        ENDSR
      * ================================================================
      *
     OPWH01     E            PWH01E
** HRQ
SCRQ01
SCRQ02
SCRQ03
SCRQ04
SCRQ05
SCRQ06
SCRQ07
SCRQ08
SCRQ09
SCRQ10
**
ROLL UP
ROLL DOWN
