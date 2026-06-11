     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7132R - UNDERWRITERS REPORT - OUTPUT WORKFILE
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*                                                               *
     H* 10/09/03 C:53  Support: WC102/39432                           *
     H*                Correct name issue                             *
     H*                                                               *
     H* 05/27/03 C:15  Support: WC072/37330                           *
     H*                Fixed pgm to retrieve the correct claim records*
     H*                when the division selection isleft blank.      *
     H*                                                               *
     H*  08/07/02  C:136 Support: WC114  32095                        *
     H*                  The division field is passing in zeros into  *
     H*                  the report, even when left blank. This is    *
     H*                  causing the report to err.                   *
     H*                                                               *
     H* 04/16/02 C:15 Support: WC121/32025                            *
     H*               Chgd pgm to always load the state/county field  *
     H*               in the work file, not just when the county code *
     H*               sort is selected.                               *
     H*                                                               *
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY           *
     H*                                                               *
     H*****************************************************************
     FWMCML5    IF   E           K DISK
     F* * * Claims File
     F*
     FWMCDL     IF   E           K DISK
     F* * * Claimant Demographics File
     F*
     FWDF1L     IF   E           K DISK
     F* * * Fund Detail File
     F*
     FWMCHL     IF   E           K DISK
     F* * * Claim History File
     F*
     FWXEFL     O    E             DISK
     F* * * Work File Output
     F*
     FWMEML     IF   E           K DISK
     F* * * Employer File
     F*

     D*
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              OUTSTAND RSV TYPE
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RHS                    1     90  2
     D                                     DIM(10)                              CHGS TO RESERVE
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D                 DS
     D  RHR                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D**
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*NO AUTHORITY*')
      *** END ADD ****** 05/25/07 **********************************

     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XQEMP#
     C                   KFLD                    XQDIV
     C*
      *** BEG ADD **** 08/07/02 ************************************************
     C     CMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XQEMP#
      *** END ADD **** 08/07/02 ************************************************
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XXCASE            7 0
     C                   KFLD                    XXYMD
     C*
     C*   KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     F1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C*
     C     *LIKE         DEFINE    EMEMP#        XQEMP#
     C     *LIKE         DEFINE    EMDIV         XQDIV
     C     *LIKE         DEFINE    EMDT          XXYMD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQSELC            1
     C                   PARM                    QQSELO            1
     C                   PARM                    QQMNTH            3
     C                   PARM                    QQSORT            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQSSRT            1
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
     C     DTYMD         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YMD'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*JOB'        SRTFMT
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDTE         XXMDY             8 0
     C*
     C                   Z-ADD     XXMDY         SRDTI
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         XXYYMM            4 0
     C*
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQFYR         XXFYR3            3 0
     C                   MOVE      QQSELO        XXSELO            1
     C                   MOVE      QQSELC        XXSELC            1
     C                   MOVE      QQSORT        XXSORT            1
     C                   MOVE      QQMNTH        MNTH              3 0
     C                   MOVE      QQEMP#        XQEMP#
     C                   MOVE      QQDIV         XQDIV
     C*
     C*  GET THE FUND YEAR RECORD
     C*
     C     XXFYR         IFEQ      0
     C     F1KEY         CHAIN     PF101                              61
     C                   ELSE
     C     F1KY2         CHAIN     PF101                              61
     C                   END
     C     *IN61         DOWEQ     '0'
     C                   MOVE      F1FYR         XXFYR3
     C*
     C*  READ CLAIM FILE
     C*
     C                   Z-ADD     *ZERO         XXCASE
     C     XQEMP#        IFEQ      *ZERO
     C     CMKEY         CHAIN     PCM01                              60
     C                   ELSE
     C*** Begin add *** 05/27/03 **************************************
     C     QQDIV         IFNE      *BLANKS
     C*** End add ***** 05/27/03 **************************************
     C     CMKEY1        CHAIN     PCM01                              60
     C*** Begin add *** 05/27/03 **************************************
     C                   ELSE
     C     CMKEY2        CHAIN     PCM01                              60
     C                   ENDIF
     C*** End add ***** 05/27/03 **************************************
     C                   END
     C     *IN60         DOWEQ     '0'
     C                   MOVE      CMCASE        XXCASE
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '1'                                          *JJJ 01-27-88
     C     CHCO#         ORNE      CMCO#                                        *
     C     CHFND         ORNE      CMFND                                        *
     C     CHFYR         ORNE      CMFYR                                        *
     C     CHCASE        ORNE      CMCASE                                       *
     C                   MOVE      CMSTAT        CHOCST                         *
     C                   MOVE      CMENTD        CHEFDT                         *
     C                   END                                                    *
     C*
     C*    IF CLAIM IS CLOSED, OUTPUT 'C' TO WORK FILE
     C*    IF CLAIM IS OPEN, OUTPUT 'O' TO WORK FILE
     C*
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        EFCLDT
     C                   ELSE
     C                   Z-ADD     *ZERO         EFCLDT
     C                   END
     C                   MOVE      CHOCST        EFOPCL
     C*
     C****************************************************************
     C**
     C**    CALL PGM TO CALCULATE DAYS SINCE CLAIM WAS CLOSED
     C**
     C     EFCLDT        IFNE      *ZERO
     C*
     C                   Z-ADD     EFCLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         EFMDY             8 0
     C*
     C                   MOVE      EFMDY         QQDOB             8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      ' '           QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQDOB
     C                   PARM                    XXMDY
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   MOVE      QQDAYS        XXDAYS            5 0
     C     XXDAYS        DIV(H)    30            EFMNTH            5 2
     C                   END
     C*
     C*  GET CLAIMANT DEMOGRAPHICS FILE - RETRIEVE NAME
     C*
     C                   MOVE      *BLANK        EFCLNM
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG CHG *** 10/09/03 ****************************
     C***********          MOVELCDLSN     QQLAST 56
     C                   MOVEL(P)  CDLSN         QQLAST           56
     C*** END CHG *** 10/09/03 ****************************
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        EFCLNM
     C                   END
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       EFCLNM
     C                   MOVEL(P)  XXATTX        EFCLNM
     C                   MOVE      *OFF          XXAUTH            1
     C                   ELSE
     C                   MOVE      *ON           XXAUTH
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  CHAIN TO EMPLOYER FILE - GET CHAPTER, COUNTY, AREA, & REGION
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C*** Begin add *** 04/16/02 **************************************
     C*  Load the state/county field.
     C*
     C                   MOVE      EMCNTY        EFSTCO
     C*
     C     EMPST         IFNE      *BLANKS
     C                   MOVEL     EMPST         EFSTCO
     C                   ELSE
     C                   MOVEL     EMMST         EFSTCO
     C                   ENDIF
     C*** End add ***** 04/16/02 **************************************
     C*
     C*  DETERMINE SORT FIELD
     C*
     C     XXSORT        IFEQ      'C'
     C                   MOVEL     CMCASE        EFSORT
     C                   END
     C**
     C     XXSORT        IFEQ      'N'
     C                   MOVEL     EFCLNM        EFSORT
     C                   END
     C**
     C     XXSORT        IFEQ      'H'
     C                   MOVEL     EMCHAP        EFSORT
     C                   END
     C**
     C     XXSORT        IFEQ      'O'
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVELEMCNTY    EFSORT
     C*
     C*** Begin dlt *** 04/16/02 **************************************
     C**********           MOVE EMCNTY    EFSTCO
     C**********
     C********** EMPST     IFNE *BLANKS
     C**********           MOVELEMPST     EFSTCO
     C**********           ELSE
     C**********           MOVELEMMST     EFSTCO
     C**********           ENDIF                           EMPST
     C*** End dlt ***** 04/16/02 **************************************
     C*
     C                   MOVEL     EFSTCO        EFSORT
     C*
     C*** END   ADD *** 06/26/97 **************************************
     C                   END
     C**
     C     XXSORT        IFEQ      'A'
     C                   MOVEL     EMAREA        EFSORT
     C                   END
     C**
     C     XXSORT        IFEQ      'R'
     C                   MOVEL     EMRGN#        EFSORT
     C                   END
     C*
     C     EFCLDT        IFNE      0
     C     MNTH          ANDNE     0
     C     EFMNTH        ANDGT     MNTH
     C                   MOVE      'N'           EFPRNT
     C                   END
     C*
     C     XXSELO        IFEQ      'N'
     C     EFOPCL        ANDEQ     'O'
     C                   MOVE      'N'           EFPRNT
     C                   END
     C*
     C     XXSELC        IFEQ      'N'
     C     EFOPCL        ANDEQ     'C'
     C                   MOVE      'N'           EFPRNT
     C                   END
     C*                                                    *
     C*   INITAILLY WRITE ALL RECORDS TO WXEFL (FOR CALC. PURPOSES),
     C*   USE EFPRNT TO DISTINGUISH WHICH RECORDS SHOULD BE PRINTED
     C*                                                    *
     C     EFPRNT        IFEQ      *BLANK
     C                   MOVE      'Y'           EFPRNT
     C                   END
     C                   MOVE      CMCO#         EFCO#
     C                   MOVE      CMFND         EFFND
     C                   MOVE      CMFYR         EFFYR
     C                   MOVE      CMEMP#        EFEMP#
     C                   MOVE      CMDIV         EFDIV
     C                   MOVE      CMCASE        EFCASE
     C*** BEG ADD ****** 05/25/07 **********************************************
     C     XXAUTH        IFEQ      *ON
     C                   MOVE      CMSS#         EFSS#
     C                   ELSE
     C                   MOVE      *BLANKS       EFSS#
     C                   ENDIF
     C*** END ADD ****** 05/25/07 **********************************************
     C                   MOVE      CMCLAS        EFCLAS
     C                   MOVE      CMAWW         EFAWW
     C                   MOVE      CMDAYS        EFDAYS
     C                   MOVE      CMACDT        EFACDT
     C                   MOVE      CMACTM        EFACTM
     C                   MOVE      CMDTRP        EFDTRP
     C                   MOVE      CMDTLW        EFDTLW
     C                   MOVE      CMACDS        EFACDS
     C                   MOVE      CMLTFL        EFLTFL
     C                   MOVE      CMNCCI        EFNCCI
     C                   MOVE      CMNATR        EFNATR
     C                   MOVE      CMPART        EFPART
     C                   MOVE      CMCAUS        EFCAUS
     C                   MOVE      CMSUPR        EFSUPR
     C                   MOVE      CMCLEX        EFCLEX
     C                   MOVE      CMFEAD        EFFEAD
     C                   MOVE      CMMCON        EFMCON
     C                   MOVE      CMDTIC        EFDTIC
     C                   MOVE      CMRT01        EFRT01
     C                   MOVE      CMRT02        EFRT02
     C                   MOVE      CMRT03        EFRT03
     C                   MOVE      CMRT04        EFRT04
     C                   MOVE      CMRT05        EFRT05
     C                   MOVE      CMRT06        EFRT06
     C                   MOVE      CMRT07        EFRT07
     C                   MOVE      CMRT08        EFRT08
     C                   MOVE      CMRT09        EFRT09
     C                   MOVE      CMRT10        EFRT10
     C                   MOVE      CMRR01        EFRR01
     C                   MOVE      CMRR02        EFRR02
     C                   MOVE      CMRR03        EFRR03
     C                   MOVE      CMRR04        EFRR04
     C                   MOVE      CMRR05        EFRR05
     C                   MOVE      CMRR06        EFRR06
     C                   MOVE      CMRR07        EFRR07
     C                   MOVE      CMRR08        EFRR08
     C                   MOVE      CMRR09        EFRR09
     C                   MOVE      CMRR10        EFRR10
     C                   MOVE      CMAR01        EFAR01
     C                   MOVE      CMAR02        EFAR02
     C                   MOVE      CMAR03        EFAR03
     C                   MOVE      CMAR04        EFAR04
     C                   MOVE      CMAR05        EFAR05
     C                   MOVE      CMAR06        EFAR06
     C                   MOVE      CMAR07        EFAR07
     C                   MOVE      CMAR08        EFAR08
     C                   MOVE      CMAR09        EFAR09
     C                   MOVE      CMAR10        EFAR10
     C                   MOVE      CMCTG1        EFCTG1
     C                   MOVE      CMCTG2        EFCTG2
     C                   MOVE      CMDEFA        EFDEFA
     C                   MOVE      CMDEFM        EFDEFM
     C                   MOVE      CMPLAA        EFPLAA
     C                   MOVE      CMPLAM        EFPLAM
     C                   MOVE      CMDTRE        EFDTRE
     C                   MOVE      CMSTAT        EFSTAT
     C                   MOVE      CMUSER        EFUSER
     C                   MOVE      CMPGM         EFPGM
     C                   MOVE      CMTIME        EFTIME
     C                   MOVE      CMDT          EFDT
     C                   MOVE      CMPCTR        EFPCTR
     C                   MOVE      CMBUL1        EFBUL1
     C                   MOVE      CMENTD        EFENTD
     C                   MOVE      CMRCVD        EFRCVD
     C     QQSSRT        IFEQ      '1'
     C                   MOVEL     CMPART        XXSRT4            4
     C                   MOVE      CMCAUS        XXSRT4
     C                   ELSE
     C                   MOVEL     CMCAUS        XXSRT4
     C                   MOVE      CMPART        XXSRT4
     C                   END
     C                   MOVEL     CMNATR        XXSRT6            6
     C                   MOVE      XXSRT4        XXSRT6
     C                   MOVE      *BLANKS       EFSSRT
     C                   MOVEL     XXSRT6        EFSSRT
     C*
     C                   WRITE     PEF01
     C                   MOVE      *BLANKS       EFPRNT                         LJQ 5-3-88
     C     NOT1          TAG
     C*
      *** BEG CHG **** 08/07/02 ************************************************
     C***********XQEMP#*** IFEQ *ZERO
     C***********CMKEY**** READEPCM01                    60
     C******************** ELSE
     C***********CMKEY1*** READEPCM01                    60
     C******************** END
     C                   SELECT
     C     XQEMP#        WHENEQ    *ZERO
     C     CMKEY         READE     PCM01                                  60
     C     QQDIV         WHENEQ    *BLANKS
     C     CMKEY2        READE     PCM01                                  60
     C                   OTHER
     C     CMKEY1        READE     PCM01                                  60
     C                   ENDSL
      *** END CHG **** 08/07/02 ************************************************
     C                   END
     C*
     C     XXFYR         IFEQ      0
     C     F1KEY         READE     PF101                                  61
     C                   ELSE
     C                   MOVE      '1'           *IN61
     C                   END
     C                   END
     C**
     C     END           TAG
     C                   MOVE      '1'           *INLR
     C**
