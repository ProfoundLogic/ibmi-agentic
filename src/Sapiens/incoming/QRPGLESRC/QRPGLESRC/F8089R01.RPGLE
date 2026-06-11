     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8089R01 - HES Bill Information                              *
     H*                                                               *
     H*  Called from: F8089R - Work with HES Bills with Errors        *
     H*  Calls to   : F8087R - Upload HES Bills - Transfer Pgm 2      *
     H*                                                               *
     H*****************************************************************
     H*  Created on 11/03/2009 by C:227 for Project 135MMX            *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  05/25/10 C:227 Support 72855                                 *
     H*                 Added ability to delete payment error         *
     H*                                                               *
     H*  02/11/04 C:62 PROJECT 41250                                  *
     H*                ASSIGN EOB# IF 'P' TYPE ERROR                  *
     H*                                                               *
     H*  12/11/03 C:62 PROJECT 129MEQ                                 *
     H*                -DISPLAY PAYMENT ERRORS                        *
     H*                -IF OTHER ERRORS CORRECTED CHECK FOR DUPS      *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FF8089F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FFAH1P     UF A E             DISK
     F                                     RENAME(PH101:DBRCDF)
     F*FAH1L3  IF  E           K        DISK
     F*            PH101                             KRENAMEPH103
     F*FAH1L4  IF  E           K        DISK
     F*            PH101                             KRENAMEPH104
     F*FAH1L6  IF  E           K        DISK
     F*            PH101                             KRENAMEPH106
     FFMC1L     UF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFTHEL1    UF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D CLM             S              1    DIM(20)
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  ETP                    1    160
     D                                     DIM(16)
     D  SCET01                 1     10
     D  SCET02                11     20
     D  SCET03                21     30
     D  SCET04                31     40
     D  SCET05                41     50
     D  SCET06                51     60
     D  SCET07                61     70
     D  SCET08                71     80
     D  SCET09                81     90
     D  SCET10                91    100
     D  SCET11               101    110
     D  SCET12               111    120
     D  SCET13               121    130
     D  SCET14               131    140
     D  SCET15               141    150
     D  SCET16               151    160
     D                 DS
     D  EDS                    1    400
     D                                     DIM(16)
     D  SCED01                 1     25
     D  SCED02                26     50
     D  SCED03                51     75
     D  SCED04                76    100
     D  SCED05               101    125
     D  SCED06               126    150
     D  SCED07               151    175
     D  SCED08               176    200
     D  SCED09               201    225
     D  SCED10               226    250
     D  SCED11               251    275
     D  SCED12               276    300
     D  SCED13               301    325
     D  SCED14               326    350
     D  SCED15               351    375
     D  SCED16               376    400
     D                 DS
     D  VALUE1                 1     10
     D  SIGN1                  1      1
     D  AMT1                   2     10  2
     D                 DS
     D  VALUE2                 1     10
     D  SIGN2                  1      1
     D  AMT2                   2     10  2
     D                 DS
     D  CLAIM#                 1     30
     D  XXFND                  1      3
     D  XXFYR                  4      6
     D  XXCASE                 7     13
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('HES Bill -
     D                                     Transfer')
     D OPTNMT          C                   CONST('HES BILL -
     D                                     Transfer')
     D OPTNMD          C                   CONST('HES BILL -
     D                                     Deletion')
     D COMAPI          C                   CONST('FA007R')
     D ZEROS           C                   CONST('000000000')
     D ERRDS1          C                   CONST('Claim')
     D ERRDS2          C                   CONST('Payment')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RTCD            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C     DTMDY3        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
     C*
     C     HEKY          KLIST
     C                   KFLD                    H1CR#
     C*
     C     HEKY1         KLIST
     C                   KFLD                    H1CR#
     C                   KFLD                    KKERR
     C*
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C                   KFLD                    ##FYR
     C                   KFLD                    ##CASE
     C*
     C     CMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    HEERR         KKERR
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMSS#         ##SS#
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMSUB#        ##SUB#
     C     *LIKE         DEFINE    CMFND         ##FND
     C     *LIKE         DEFINE    CMFYR         ##FYR
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    PDFEIN        HDFEIN
     C     *LIKE         DEFINE    PDMOD         HDMOD
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C     *LIKE         DEFINE    H1CR#         KKCR#
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK - Error checking for why this bill wasn't processed.
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C                   SETOFF                                       212223
     C                   SETOFF                                       242526
     C*
     C                   MOVEL     H1CR#         KKCR#
     C                   Z-ADD     0             X                 2 0
     C     *IN90         IFEQ      *OFF
     C     *LOVAL        SETLL     PHE01
     C     HEKY          SETLL     PHE01
     C     HEKY          READE     PHE01                                  75
     C     *IN75         DOWEQ     *OFF
     C                   ADD       1             X
     C                   MOVEA     HEERDS        EDS(X)
     C                   MOVE      HEERR         XXERR#            2 0
     C     XXERR#        IFEQ      14
     C                   MOVEL     ERRDS2        ETP(X)
     C                   ELSE
     C                   MOVEL     ERRDS1        ETP(X)
     C                   ENDIF
     C     HEKY          READE     PHE01                                  75
     C                   ENDDO
     C                   ENDIF
     C*
     C*
     C                   MOVEL     H1CLM#        CLAIM#
     C                   MOVE      XXFND         SCFND
     C                   MOVE      XXFYR         SCFYR
     C                   MOVE      XXCASE        SCCASE
     C*
     C* Check WCMC Claim Master to see if SS#/ACDT exists.
     C*
     C     CMKY2         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        2190
     C                   ENDIF
     C*
     C* FEIN/MOD does not exist.
     C*
     C                   MOVE      H1FEIN        KKFEIN
     C                   Z-ADD     *ZEROS        KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     PDSTAT        ANDEQ     'I'
     C                   SETON                                        2290
     C                   MOVEL     'Y'           XXCHG
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/11/03 **************************************
     C*
     C* Payment error
     C*
     C                   SETOFF                                       26
     C     H1PRFL        IFEQ      'P'
     C                   SETON                                        26
     C                   ENDIF
     C*** END   ADD *** 12/11/03 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEG ADD *** 05/25/10 *********************************************
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C*** END ADD *** 05/25/10 *********************************************
     C*
     C*CHECK FOR PAYMENT ERROR
     C*
     C     *LOVAL        SETLL     PHE01
     C     HEKY          SETLL     PHE01
     C     HEKY          READE     PHE01                                  61
     C     *IN61         DOWEQ     *OFF
     C     HEERR         IFEQ      13
     C                   SETON                                        7490
     C                   LEAVE
     C                   ENDIF
     C     HEKY          READE     PHE01                                  61
     C                   ENDDO
     C*** BEG DEL *** 05/25/10 *********************************************
     C* Moved before checking for Payment error
     C**********************************************************************
     C*******    *IN16     IFEQ *OFF
     C*** END DEL *** 05/25/10 *********************************************
     C*
     C* Check WCMC Claim Master to see if the claim number exists
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     ##SUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C     CMKY2         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C*
     C*CHECK FOR CLAIM ERROR
     C                   Z-ADD     1             KKERR
     C     HEKY1         CHAIN     PHE01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PHE01
     C                   ENDIF
     C*
     C*CHECK FOR SS# ERROR
     C                   Z-ADD     2             KKERR
     C     HEKY1         CHAIN     PHE01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PHE01
     C                   ENDIF
     C*
     C*CHECK FOR LAST NAME ERROR
     C                   Z-ADD     3             KKERR
     C     HEKY1         CHAIN     PHE01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PHE01
     C                   ENDIF
     C*
     C*CHECK FOR DOI NAME ERROR
     C                   Z-ADD     4             KKERR
     C     HEKY1         CHAIN     PHE01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PHE01
     C                   ENDIF
     C* CHEKC IF ERROR STILL EXISTS
     C     HEKY          CHAIN     PHE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* DELETION REASON CAN'T BE BLANKS
     C*
     C     SCDLRE        IFEQ      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFND'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C     HFLD          OREQ      'SCSS#'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##SUB#
     C                   PARM      *ZEROS        ##FND
     C                   PARM      *ZEROS        ##FYR
     C                   PARM      *ZEROS        ##CASE
     C                   PARM      *BLANKS       ##SS#
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FYR         OREQ      *ZEROS
     C     ##CASE        OREQ      *ZEROS
     C                   SETON                                        3190
     C                   ELSE
     C     CMKY1         CHAIN     PCM01                              99
     C*
     C                   MOVEL(P)  CMLSN         ##LAST
     C                   MOVEL(P)  CMFSN         ##FRST
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY3
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVEL     CMSS#         SCSS#
     C                   MOVE      '1'           ##TERM
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST           56
     C                   PARM                    ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C* PROVIDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *BLANKS       ##TYPE            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   SETON                                        9032
     C                   ELSE
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C*
     C                   CALL      'FA725R'
     C                   PARM      SCFEIN        #1FEIN            9 0
     C                   PARM      SCMOD         #1MOD             3 0
     C                   PARM      *BLANKS       #1NAME           40
     C                   PARM      *BLANKS       #1ERR             1
     C     #1ERR         IFNE      '0'
     C                   MOVEL(P)  #1NAME        SCPNAM
     C                   ENDIF
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C                   EXSR      ERRCK
     C                   ENDIF
     C*
     C                   MOVEL(P)  H1CR#         SCCR#
     C*
     C                   CALL      'SRNAME'
     C                   PARM      H1HLSN        ##LAST           56
     C                   PARM      H1FSN         ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
     C*
     C                   MOVEL     H1HADT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVEL     H1HSS#        SCSS#
     C*
     C                   MOVEL     H1FEIN        KKFEIN
     C                   Z-ADD     0             KKMOD
     C*
     C                   Z-ADD     KKFEIN        SCFEIN
     C                   MOVEL     *ALL'0'       SCMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     PDSTAT        ANDEQ     'I'
     C                   SETON                                        22
     C                   ENDIF
     C*
     C     PDDBA1        IFNE      'Y'
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL     PDNAME        SCPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      SCPNAM
     C                   ENDIF
     C                   MOVEL     PDADR1        SCPAD1
     C                   ELSE
     C                   MOVEL     PDADR1        SCPNAM
     C                   MOVEL     PDADR2        SCPAD1
     C                   ENDIF
     C                   MOVEL     PDCTY         SCPCTY
     C                   MOVEL     PDST          SCPST
     C                   MOVEL     PDZIP         SCPZIP
     C*
     C                   MOVEL     H1FLDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCFRDT            8 0
     C*
     C                   MOVEL     H1FLDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCTODT            8 0
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q1USER        H1DLUS
     C                   MOVEL(P)  Q1PGM         H1DLPG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        H1DLTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1DLDT
     C                   MOVEL     SRDTO         H1PRDT
     C*
     C                   MOVEL(P)  SCDLRE        H1DLRE
     C                   MOVEL(P)  'D'           H1PRFL
     C*
     C                   Z-ADD     *ZEROS        H1CO#
     C                   Z-ADD     *ZEROS        H1SUB#
     C                   Z-ADD     *ZEROS        H1FND
     C                   Z-ADD     *ZEROS        H1FYR
     C                   Z-ADD     *ZEROS        H1CASE
     C                   Z-ADD     *ZEROS        H1EOB#
     C                   MOVEL     *BLANKS       H1SS#
     C                   MOVEL     *BLANKS       H1HADT
     C*
     C                   ELSE
     C*** BEGIN ADD *** 12/11/03 **************************************
     C*
     C* Now that the the errors have been corrected let's check for
     C* duplicate payments
     C*
     C                   SETOFF                                       91
     C     *IN90         IFEQ      *OFF
     C     H1PRFL        ANDEQ     'E'
     C*
     C*  PAID
     C*
     C     H1PAID        IFNE      *BLANKS
     C     H1PAID        ANDNE     *BLANKS
     C*
     C                   MOVE      H1PAID        VALUE1
     C*
     C     SIGN1         IFEQ      '-'
     C                   Z-SUB     AMT1          XXAMT1            9 2
     C                   ELSE
     C                   Z-ADD     AMT1          XXAMT1
     C                   ENDIF
     C*
     C                   Z-ADD     XXAMT1        SCPAID            9 2
     C                   ENDIF
     C*
     C     SCPAID        IFNE      *ZEROS
     C*
     C                   MOVEL     H1FLDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXFRDT
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        $#FND
     C                   ELSE
     C                   Z-ADD     *ZEROS        $#FND
     C                   ENDIF
     C*
     C* Check for duplicate request/payment
     C*
     C                   CALL      'WA052R'
     C                   PARM      CMCO#         $#CO#             3 0          Company #
     C                   PARM                    $#FND             3 0          Fund
     C                   PARM      CMFYR         $#FYR             3 0          Fund Year
     C                   PARM      CMCASE        $#CASE            7 0          Case
     C                   PARM      XXFRDT        $#FROM            7 0          From Date
     C                   PARM      XXTODT        $#TO              7 0          To Date
     C                   PARM      SCPAID        $#AMT             9 2          Amount
     C                   PARM      'ME'          $#RECD            2            Reserve Code
     C                   PARM      PDFEIN        $#FEIN            9 0          FEIN #
     C                   PARM      'M'           $#RETP            1            Reserve Type
     C                   PARM      *BLANKS       $#RTCD            2
     C*
     C     $#RTCD        IFNE      *BLANKS
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *ON
     C                   SETON                                        91
     C                   SETOFF                                       KL
     C                   MOVEL     'E'           H1PRFL
     C                   MOVEL     'P'           @@RTCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 12/11/03 **************************************
     C*
     C                   Z-ADD     CMCO#         H1CO#
     C                   Z-ADD     CMSUB#        H1SUB#
     C                   Z-ADD     CMFND         H1FND
     C                   Z-ADD     CMFYR         H1FYR
     C                   Z-ADD     CMCASE        H1CASE
     C                   MOVEL     CMSS#         H1SS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         H1HADT
     C*
     C     *IN91         IFEQ      *OFF
     C     H1EOB#        IFEQ      *ZEROS
     C     H1PRFL        ANDEQ     'E'
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1EOB#
     C                   UPDATE    PC101
     C                   ENDIF
     C                   Z-ADD     C1EOB#        H1EOB#
     C                   ENDIF
     C*** BEGIN ADD *** 12/11/03 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/11/03 **************************************
     C*
     C                   MOVEL     *BLANKS       H1DLUS
     C                   MOVEL     *BLANKS       H1DLPG
     C                   Z-ADD     *ZEROS        H1DLTM
     C                   Z-ADD     *ZEROS        H1DLDT
     C                   MOVEL     *BLANKS       H1DLRE
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   SETOFF                                       3132
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       0916
     C                   SETOFF                                       212223
     C                   SETOFF                                       242526
     C                   MOVE      *BLANKS       ETP
     C                   MOVE      *BLANKS       EDS
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@CALL        COMP      'D'                                    16
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C                   MOVEL     'N'           XXCHG             1
     C*
     C     *IN16         IFEQ      *OFF
     C                   MOVEL(P)  OPTNMT        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNMD        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDX
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C*
     C     *IN16         IFEQ      *OFF
     C     *IN17         IFEQ      *OFF
     C     *IN91         IFEQ      *OFF
     C                   CALL      'F8087R'
     C                   PARM      *BLANKS       ##DCID           12            Document ID
     C                   PARM      *ZEROS        ##RRN             9 0          FAN9P RRN
     C                   PARM      @@RRN         ##RRN2            9 0          FAN1P RRN
     C                   PARM      XXCHG         ##PRVC            1            PROV CHANGE
     C                   ENDIF
     C                   ELSE
     C                   CALL      'F8087R'                             99
     C                   PARM      *BLANKS       ##DCID           12            Document ID
     C                   PARM      *ZEROS        ##RRN             9 0          FAN9P RRN
     C                   PARM      @@RRN         ##RRN2            9 0          FAN1P RRN
     C                   PARM      XXCHG         ##PRVC            1            PROV CHANGE
     C                   ENDIF
     C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAH1P
     C*
     C                   ENDSR
