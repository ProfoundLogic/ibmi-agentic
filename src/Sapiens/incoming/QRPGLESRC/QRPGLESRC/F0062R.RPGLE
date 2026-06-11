     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0062R - WCMC ADD GROUP PROMPT              C:62 12/17/97    *
     H*                                                               *
     H*  CALLED FROM: W0010R02                                        *
     H*  CALLS TO:    FA027R                                          *
     H*               F0060R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/19/99 C:53                                                *
     H*                EDIT ON A ZERO FUND # ON ADD FROM WCMS SCREEN  *
     H*                TO PROHIBIT A ZERO FUND # WRITEN ON WCMC SIDE  *
     H*                                                               *
     H*  POSITION CURSORS IN KJDO:  73 - 75.                          *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1: 71.                              *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK2: 71 - 72.                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0062F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFMFNL     IF   E           K DISK
     FFMSMP     IF   E             DISK
     FFMSML     IF   E           K DISK
     F                                     RENAME(PSM01:PSM02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Add Group -
     D                                      Prompt')
     D COMAPI          C                   CONST('FA007R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@FND
     C*
     C*  API PARM LIST
     C*
     C     APIPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      SCSUB#        ##SUB#
     C                   PARM      SCGRP         ##GRP
     C                   PARM      @@FND         ##FND
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C* GROUP MASTER PARM LIST
     C*
     C     GRPPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      '1'           ##CALL            1
     C*
     C* SUBSCRIBER PARM LIST
     C*
     C     SUBPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKGRP
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNCO#         ##CO#
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKGRP
     C     *LIKE         DEFINE    FNFND         ##GRP
     C     *LIKE         DEFINE    FNFND         ##FND
     C     *LIKE         DEFINE    FNFND         @@FND
     C*
     C                   MOVEL     Q1USER        SCUSER           10
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *OFF          OUTFLG            1
     C                   MOVE      *OFF          DSPTYP            1
     C*
     C                   EXSR      GETCOF
     C                   EXSR      GETPGT
     C                   EXSR      PGMSTR
     C*
B01  C     *INKC         DOUEQ     *ON
     C     *INKL         OREQ      *ON
     C     OUTFLG        OREQ      *ON
B02  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR01
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   CAS                     OTHFKS
     C                   ENDCS
B04  C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCK1
E04  C                   ENDIF
B04  C     *IN90         IFEQ      *OFF
B05  C     #OFSCR        IFEQ      1
     C                   EXSR      FINISH
X05  C                   ELSE
     C                   EXSR      XSCR02
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDDO
E01  C                   ENDDO
     C                   EXSR      PGMEND
     C*
     C                   RETURN
     C*================================================================
     C*  FINISH - Final processing for file update/display
     C*================================================================
     C     FINISH        BEGSR
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
     C*================================================================
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*================================================================
     C     GETCOF        BEGSR
     C                   CALL      COMAPI        COMPL                  99
B01  C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  PDSNAM        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
E01  C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM
     C                   ENDSR
     C*================================================================
     C*  GETPGT - Get program title for display heading
     C*================================================================
     C     GETPGT        BEGSR
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   ENDSR
     C*================================================================
     C*  XSCR02 - Display second screen and edit check
     C*================================================================
     C     XSCR02        BEGSR
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR02
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   CAS                     OTHFKS
     C                   ENDCS
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCK2
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
B04  C     #OFSCR        IFEQ      2
     C                   EXSR      FINISH
X04  C                   ELSE
     C                   EXSR      XSCR03
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  XSCR03 - Display third screen and edit check
     C*================================================================
     C     XSCR03        BEGSR
B01  C     *IN90         DOUEQ     *OFF
     C                   EXFMT     SCR03
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   CAS                     OTHFKS
     C                   ENDCS
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCK3
E03  C                   ENDIF
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      FINISH
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
E01  C                   ENDIF
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  WCMC PROMPT MUST BE 'Y' OR 'N'
     C*
     C  N90SCMCFL        IFNE      'Y'
     C     SCMCFL        ANDNE     'N'
     C*
     C                   SETON                                        71  90
     C*
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C     SCMCFL        IFEQ      'Y'
     C*
     C                   Z-ADD     2             #OFSCR
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             #OFSCR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* SUBSCRIBER NUMBER MUST BE VALID.
     C*
     C  N90              MOVE      SCSUB#        KKSUB#
     C*
     C  N90SMKY          CHAIN     PSM02                              99
     C*
     C   99
     CANN90              SETON                                        7190
     C*** BEG ADD *** 03/19/99 ****************************
     C  N90SCGRP         IFEQ      *ZEROS
     C                   SETON                                        9076
     C                   GOTO      ENDCK2
     C                   ENDIF
     C*** END ADD *** 03/19/99 ****************************
     C*
     C* GROUP NUMBER MUST NOT EXIST
     C*
     C  N90              MOVE      SCSUB#        KKSUB#
     C  N90              MOVE      SCGRP         KKGRP
     C*
     C  N90FNKY          CHAIN     PFN01                              99
     C*
     C  N99
     CANN90              SETON                                        7290
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   CALL      'FA027R'      APIPL
     C*
     C                   CALL      'F0060R01'    GRPPL
     C*
     C                   ENDIF
     C*
     C     ENDCK2        ENDSR                                                  CHG 03/19/99
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C*
     C                   CALL      'F0055R'      SUBPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        73
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM01                              99
     C*
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   SETON                                        74
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCGRP'
     C*
     C                   SETON                                        75
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
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
     C                   Z-ADD     1             #OFSCR            2 0
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   ENDSR                                                  PGMSTR
