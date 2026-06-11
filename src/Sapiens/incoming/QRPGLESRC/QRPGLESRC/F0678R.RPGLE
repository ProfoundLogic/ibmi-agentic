     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H*****************************************************************
     H*  F0678R - PRINT TREATMENT PLAN INFORMATION    C:62 02/09/01   *
     H*                                               PROJ: 114MAT    *
     H*  CALLED FROM: F0678C                                          *
     H*  CALLS TO:    NONE
     H*                                                               *
     H*****************************************************************
     H*  Created on 08/29/06 by C:163 for project 114WMF
     H*  BASED ON: F0678R CREATED ON 02/09/01 BY C:62 FOR PROJECT 114MAT
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  *IN99 - USED FOR GENERAL CHAINING PURPOSES                   *
     H*                                                               *
     H*****************************************************************
     H dftactgrp(*no) actgrp(*caller)
     F*
     F*  Physical file definition (for maintaining record)
     FFMCMP     IF   E             DISK
     FFATRP     IF   E             DISK
     FFATRL     IF   E           K DISK
     F                                     RENAME(PTR01:PTR02)
     FFATDL1    IF   E           K DISK
     FFATIL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F*
     F* Check requests created
     F*
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOA)
     F*
     D DS              S              1    DIM(792)                             TREATMENT DESC
     D NT              S              1    DIM(792)                             TREATMENT NOTE
     D**************** /COPY SCOPYMT01I
     D/COPY SCOPYPR01D
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D TITLE           C                   CONST('Treatment Plan In-
     D                                     formation')
     D*
     D                 DS                  INZ
     D  ST1                    1    792
     D                                     DIM(11)
     D  DDLN01                 1     72
     D  DDLN02                73    144
     D  DDLN03               145    216
     D  DDLN04               217    288
     D  DDLN05               289    360
     D  DDLN06               361    432
     D  DDLN07               433    504
     D  DDLN08               505    576
     D  DDLN09               577    648
     D  DDLN10               649    720
     D  DDLN11               721    792
     D*
     D                 DS                  INZ
     D  ST2                    1    792
     D                                     DIM(11)
     D  NNLN01                 1     72
     D  NNLN02                73    144
     D  NNLN03               145    216
     D  NNLN04               217    288
     D  NNLN05               289    360
     D  NNLN06               361    432
     D  NNLN07               433    504
     D  NNLN08               505    576
     D  NNLN09               577    648
     D  NNLN10               649    720
     D  NNLN11               721    792
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     DIKY          KLIST
     C                   KFLD                    TIICD9
     C                   KFLD                    TILOST
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKFIL#
     C*
     C     TDKY          KLIST
     C                   KFLD                    TRCO#
     C                   KFLD                    TRSUB#
     C                   KFLD                    TRFND
     C                   KFLD                    TRFYR
     C                   KFLD                    TRCASE
     C                   KFLD                    TRTX#
     C                   KFLD                    KKTYPE
     C*
     C     TIKY          KLIST
     C                   KFLD                    TRCO#
     C                   KFLD                    TRSUB#
     C                   KFLD                    TRFND
     C                   KFLD                    TRFYR
     C                   KFLD                    TRCASE
     C                   KFLD                    TRTX#
     C*
     C     TRKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ORFIL#        KKFIL#
     C     *LIKE         DEFINE    ##L1          XXLIN1
     C     *LIKE         DEFINE    ##L2          XXLIN2
     C     *LIKE         DEFINE    EMNAME        XXEMNM
     C     *LIKE         DEFINE    OROUTQ        XXOUTQ
     C     *LIKE         DEFINE    TDTYPE        KKTYPE
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C*  CHKOVF - Check the overflow of QSYSPRT.
     C*================================================================
     C     CHKOVF        BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD1
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* LOADR1 - LOAD TREATEMENT PLAN INFORMATION
     C*================================================================
     C     LOADR1        BEGSR
     C*
     C                   Z-ADD     TRBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBGDT            8 0
     C*
     C                   Z-ADD     TRENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXENDT            8 0
     C*
     C                   Z-ADD     TRLTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXLTDT            8 0
     C*
     C                   MOVEL     TRPHN#        XXPHNA            3 0
     C                   MOVE      TRPHN#        XXPHNB            4 0
     C*
     C                   EXCEPT    HEAD2
     C*
     C                   ENDSR
     C*================================================================
     C* LOADR2 - LOAD DESCRIPTION OF SERVICES
     C*================================================================
     C     LOADR2        BEGSR
     C*
     C* TREATMENT DESCRIPTIONS
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C                   MOVEL(P)  'D'           KKTYPE
     C*
     C     TDKY          CHAIN     PTD01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     TDSEQ#        WHENEQ    1
     C                   MOVEA     TDDESC        DS(1)
     C*
     C     TDSEQ#        WHENEQ    2
     C                   MOVEA     TDDESC        DS(265)
     C*
     C     TDSEQ#        WHENEQ    3
     C                   MOVEA     TDDESC        DS(529)
     C*
     C                   ENDSL
     C*
     C     TDKY          READE     PTD01                                  99
     C*
     C                   ENDDO
     C*
     C                   MOVEA     DS(1)         DDLN01
     C                   MOVEA     DS(73)        DDLN02
     C                   MOVEA     DS(145)       DDLN03
     C                   MOVEA     DS(217)       DDLN04
     C                   MOVEA     DS(289)       DDLN05
     C                   MOVEA     DS(361)       DDLN06
     C                   MOVEA     DS(433)       DDLN07
     C                   MOVEA     DS(505)       DDLN08
     C                   MOVEA     DS(577)       DDLN09
     C                   MOVEA     DS(649)       DDLN10
     C                   MOVEA     DS(721)       DDLN11
     C*
     C     1             DO        11            X                 2 0
     C*
     C     ST1(X)        IFNE      *BLANKS
     C*
     C                   EXSR      CHKOVF
     C                   EXCEPT    DESC
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD3
     C*
     C                   ENDSR
     C*================================================================
     C* LOADR3 - LOAD ADDITIONAL FILE NOTES
     C*================================================================
     C     LOADR3        BEGSR
     C*
     C* TREATMENT NOTES
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C                   MOVEL(P)  'N'           KKTYPE
     C*
     C     TDKY          CHAIN     PTD01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     TDSEQ#        WHENEQ    1
     C                   MOVEA     TDDESC        NT(1)
     C*
     C     TDSEQ#        WHENEQ    2
     C                   MOVEA     TDDESC        NT(265)
     C*
     C     TDSEQ#        WHENEQ    3
     C                   MOVEA     TDDESC        NT(529)
     C*
     C                   ENDSL
     C*
     C     TDKY          READE     PTD01                                  99
     C*
     C                   ENDDO
     C*
     C                   MOVEA     NT(1)         NNLN01
     C                   MOVEA     NT(73)        NNLN02
     C                   MOVEA     NT(145)       NNLN03
     C                   MOVEA     NT(217)       NNLN04
     C                   MOVEA     NT(289)       NNLN05
     C                   MOVEA     NT(361)       NNLN06
     C                   MOVEA     NT(433)       NNLN07
     C                   MOVEA     NT(505)       NNLN08
     C                   MOVEA     NT(577)       NNLN09
     C                   MOVEA     NT(649)       NNLN10
     C                   MOVEA     NT(721)       NNLN11
     C*
     C     1             DO        11            X                 2 0
     C*
     C     ST2(X)        IFNE      *BLANKS
     C*
     C                   EXSR      CHKOVF
     C                   EXCEPT    NOTE
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD4
     C*
     C                   ENDSR
     C*================================================================
     C* LOADR4 - LOAD TREATMENT PLAN ICD9 CODES
     C*================================================================
     C     LOADR4        BEGSR
     C*
     C     DIKY          CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     DIDS1         XXDS1
     C                   MOVEL     DIDSC1        XXDESC
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXDS1             6
     C                   MOVEL     *BLANKS       XXDESC           40
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     TIBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYBGDT            8 0
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   ENDSR
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   EXSR      PGMSTR
     C*
     C     @@RRN2        IFNE      *ZEROS
     C*
     C     @@RRN2        CHAIN     PTR01                              99
     C*
     C                   EXSR      LOADR1
     C                   EXSR      LOADR2
     C                   EXSR      LOADR3
     C*
     C* TREATMENT ICD9 CODES
     C*
     C     TIKY          CHAIN     PTI01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      CHKOVF
     C                   EXSR      LOADR4
     C*
     C     TIKY          READE     PTI01                                  99
     C*
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C     TRKY          CHAIN     PTR02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      LOADR1
     C                   EXSR      LOADR2
     C                   EXSR      LOADR3
     C*
     C* TREATMENT ICD9 CODES
     C*
     C     TIKY          CHAIN     PTI01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      CHKOVF
     C                   EXSR      LOADR4
     C*
     C     TIKY          READE     PTI01                                  99
     C*
     C                   ENDDO
     C*
     C     TRKY          READE     PTR02                                  99
     C*
     C  N99              EXCEPT    HEAD1
     C*
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C                   EXCEPT    NOREC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If @@mode <> 'M';
          kkFil# = '0';
          Chain orKy POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* PRINT ROUTING PAGE IF NOT MASS PRINT
     C*
     C     @@MODE        IFNE      'M'
     C*
     C                   MOVEL     '0'           KKFIL#
     C*
     C     ORKY          CHAIN     POR01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     @@QUE         IFNE      *BLANKS
     C                   MOVEL(P)  @@QUE         XXOUTQ
     C                   ELSE
     C                   MOVEL(P)  OROUTQ        XXOUTQ
     C                   ENDIF
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*************      EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* GENERATE THREE LINE HEADERS
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMSUB#        ##SUB#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C     XXLIN1        PARM      *BLANKS       ##L1             78
     C     XXLIN2        PARM      *BLANKS       ##L2             78
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C  N99              MOVEL(P)  EMNAME        XXEMNM
     C   99              MOVEL(P)  *BLANKS       XXEMNM
     C*
     C                   EXCEPT    HEAD1
     C*
     C                   ENDSR
     O*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                 10 'XXXXXXXX'
     O**********                                 22 'XXXXXX'
     O**********                                 36 'XX      XX'
     O**********                                 47 'XXXXXXXXXX'
     O**********                                 58 'XXXXXXXXXX'
     O**********                                 69 'XXX     XX'
     O**********                                 79 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 11 'XXXXXXXXX'
     O**********                                 23 'XXXXXXXX'
     O**********                                 36 'XX      XX'
     O**********                                 47 'XXXXXXXXXX'
     O**********                                 58 'XXXXXXXXXX'
     O**********                                 69 'XXXX    XX'
     O**********                                 80 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 12 'XX      XX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XXXX    XX'
     O**********                                 81 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 12 'XX      XX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XX XX   XX'
     O**********                                 73 'XX'
     O**********E            ROUTE          1
     O**********                                 11 'XXXXXXXXX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XX XX   XX'
     O**********                                 73 'XX'
     O**********E            ROUTE          1
     O**********                                 10 'XXXXXXXX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XX  XX  XX'
     O**********                                 73 'XX'
     O**********E            ROUTE          1
     O**********                                  6 'XXXX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XX  XX  XX'
     O**********                                 81 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  7 'XXXXX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XX   XX XX'
     O**********                                 81 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  8 'XX XXX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XX   XX XX'
     O**********                                 81 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  9 'XX  XXX'
     O**********                                 24 'XX      XX'
     O**********                                 36 'XX      XX'
     O**********                                 43 'XX'
     O**********                                 54 'XX'
     O**********                                 69 'XX    XXXX'
     O**********                                 81 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX   XXX'
     O**********                                 23 'XXXXXXXX'
     O**********                                 35 'XXXXXXXX'
     O**********                                 43 'XX'
     O**********                                 58 'XXXXXXXXXX'
     O**********                                 69 'XX    XXXX'
     O**********                                 80 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                 11 'XX    XXX'
     O**********                                 22 'XXXXXX'
     O**********                                 34 'XXXXXX'
     O**********                                 43 'XX'
     O**********                                 58 'XXXXXXXXXX'
     O**********                                 69 'XX     XXX'
     O**********                                 79 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             Q1USER              26
     O**********E            ROUTE          1
     O**********E            ROUTE          1
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       Q1USER              26
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       TITLE             +001
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             XXOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE            60
     O                                           20 '(C) COPYRIGHT PROGRE'
     O                                           41 'SSIVE DATA SOLUTIONS,'
     O                                           58 'INC. 2001       '
     OQSYSPRT   E            HEAD1          2 02
     O                       TITLE               54
     O          E            HEAD1          1
     O                                           18 'Employer . . :'
     O                       XXEMNM              59
     O          E            HEAD1          1
     O                       XXLIN1              82
     O          E            HEAD1          2
     O                       XXLIN2              82
     O          E            HEAD2          1
     O                                           24 'Treatment plan begin'
     O                                           33 ' date . :'
     O                       XXBGDT              44 '0  /  /    '
     O          E            HEAD2          1
     O                                           24 'Treatment plan end d'
     O                                           33 'ate . . :'
     O                       XXENDT              44 '0  /  /    '
     O          E            HEAD2          1
     O                                           24 'Maximum medical impr'
     O                                           33 'ovement :'
     O                       TRMMI               35
     O          E            HEAD2          2
     O                                           24 'Est. date of last tr'
     O                                           33 'eatment :'
     O                       XXLTDT              44 '0  /  /    '
     O          E            HEAD2          0
     O                                           28 'Description of services:'
     O          E            HEAD2          1
     O                                           28 '________________________'
     O          E            DESC           1
     O                       ST1(X)              76
     O          E            HEAD3       1  0
     O                                           26 'Additional file notes:'
     O          E            HEAD3          1
     O                                           26 '______________________'
     O          E            NOTE           1
     O                       ST2(X)              76
     O          E            HEAD4       1  0
     O                                           23 'Treatment plan ICD9'
     O                                           30 ' codes:'
     O          E            HEAD4          1
     O                                           23 '___________________'
     O                                           30 '_______'
     O          E            HEAD4          1
     O                                           11 'ICD9'
     O                                           26 'Description'
     O                                           64 'Primary?'
     O                                           77 'Begin date'
     O          E            DETAIL         1
     O                       XXDS1               13
     O                       XXDESC              55
     O                       TIPRIM              60
     O                       YYBGDT              77 '  /  /    '
     O          E            NOREC          1
     O                                           23 'No treatment plan i'
     O                                           43 'nformation exists fo'
     O                                           54 'r this case'
