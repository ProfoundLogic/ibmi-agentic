     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0072R02 - EDI PTD/Reduced Earnings/Recoveries Entry Pgm
     H*****************************************************************
     H* Description:  This pgm is used to enter EDI 'paid to date'/   *
     H*   'reduced earnings'/'recoveries'.  This entries will be      *
     H*   associated with specific A49 Log records.                   *
     H*                                                               *
     H* Called from:  W0072R                                          *
     H*                                                               *
     H* Created:  09/22/00      C:15      Project: WC468              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 06/09/20 C:318  Project: SY420
     H*                 Mask SSN
     H*
     H* 10/19/07 C:53  Project :WC1584                              *
     H*                EDI Release 3.0 changes                        *
     H*                                                               *
     H*****************************************************************
     FW0072F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWEREP     UF A E             DISK
     F                                     RENAME(PRE01:DBRCDF)
     FWEREL     IF   E           K DISK
     F*
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D                 DS
     D  SS#                    1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI PTD/Reduced Ea-
     D                                     rnings/Recoveries')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@SEQ#            5
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Key list for the PTD/Reduced Earning/Recovery File
     C*
     C     REKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C                   KFLD                    KKCODE
     C*
     C*  Key list for the Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Key list for the Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  Key list for the Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    RECO#         KKCO#
     C     *LIKE         DEFINE    REFND         KKFND
     C     *LIKE         DEFINE    REFYR         KKFYR
     C     *LIKE         DEFINE    RECASE        KKCASE
     C     *LIKE         DEFINE    RESEQ#        KKSEQ#
     C     *LIKE         DEFINE    RECODE        KKCODE
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C     RESTAT        IFEQ      'I'
     C                   MOVE      'A'           RESTAT
     C                   ELSE
     C                   MOVE      'I'           RESTAT
     C                   ENDIF                                                  PASTAT
     C*
     C                   MOVEL     Q1USER        REUSER
     C                   MOVEL     Q1PGM         REPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         REDT
     C                   MOVEL     DSTIME        RETIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
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
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  In 'add' mode, process code edits.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  Reduced earnings code can not be blank.
     C*
     C     SCCODE        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C*  If the code has already been used, send an error.
     C*
     C                   MOVEL     SCCODE        KKCODE
     C     REKY          CHAIN     PRE01                              99
     C  N99              SETON                                        72  90
     C*
     C                   ENDIF
     C*
     C*** BEG ADD ****** 10/19/07 ************************************
     C                   MOVEL     'PADJ'        KKTBID
     C                   MOVEL(P)  SCCODE        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        75  90
     C                   ENDIF
     C*** END ADD ****** 10/19/07 ************************************
     C*
     C*  Earnings amount can not be 0.
     C*
     C  N90SCAMT         IFEQ      0
     C                   SETON                                        73  90
     C                   ENDIF                                                  IN90/SCEDIP
     C*
     C*  All edits have been passed.
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - Process F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCODE'
     C*
     C                   MOVE      SCPRDS        QQPRDS           40
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      ' '           QQCALL            1
     C*** BEG CHG ****** 10/19/07 ************************************
     C***********          PARM 'PTD '    KKTBID
     C                   PARM      'PADJ'        KKTBID
     C*** END CHG ****** 10/19/07 ************************************
     C                   PARM      *BLANKS       KKTKEY
     C                   PARM      ' '           QQRTN             1
     C*
     C     QQRTN         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     QQRTN         IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   ENDIF
     C*
     C     QQRTN         IFEQ      '*'
     C                   MOVEL     KKTKEY        SCCODE
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCDESC
     C                   MOVE      *ON           *IN74                          Position cursor
     C                   ENDIF
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
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     RECODE        SCCODE
     C*
     C*** BEG CHG ****** 10/19/07 ************************************
     C***********          MOVEL'PTD '    KKTBID
     C                   MOVEL     'PADJ'        KKTBID
     C*** END CHG ****** 10/19/07 ************************************
     C                   MOVEL(P)  SCCODE        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C*** BEG CHG ****** 10/19/07 ************************************
     C** 99                MOVELGTDESC    SCDESC
     C**N99                MOVE *BLANKS   SCDESC
     C  N99              MOVEL     GTDESC        SCDESC
     C   99              MOVE      *BLANKS       SCDESC
     C*** END CHG ****** 10/19/07 ************************************
     C                   MOVE      REAAMT        SCAMT
     C                   MOVEL     RESTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *ZEROS        SCAMT
     C                   MOVE      'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     KKCO#         RECO#
     C                   MOVE      KKFND         REFND
     C                   MOVE      KKFYR         REFYR
     C                   MOVE      KKCASE        RECASE
     C                   MOVE      KKSEQ#        RESEQ#
     C                   MOVE      SCCODE        RECODE
     C                   MOVE      SCAMT         REAAMT
     C                   MOVE      SCSTAT        RESTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
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
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@SEQ#        KKSEQ#
     C*
     C*  Load the screen headings.
     C*
     C                   Z-ADD     KKFND         SCFND                          Group info
     C     FNKY          CHAIN     PFN01                              99
     C  N99              MOVEL(P)  FNNAME        SCFNAM
     C*
     C*
     C     CMKY          CHAIN     PCM01                              99        Claim info
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      CMSS#         SS#                            SS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'SS#'         QQNAME           25
     C     SS#           PARM      SS#           QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C                   CAT       SS2:0         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       SS3:0         SCSS#
     C*
     C     CMLSN         CAT(P)    ',':0         SCCNAM                         Claimant name
     C                   CAT       CMFSN:1       SCCNAM
     C                   CAT       CMMID:1       SCCNAM
     C*
     C                   MOVEL(P)  CMFYR         SCCS#                          Case #
     C                   CAT       '-':0         SCCS#
     C                   MOVE      CMCASE        SCCS#
     C*
     C                   MOVEL(P)  CMACL#        SCSCL#                         State claim #
     C*
     C                   Z-ADD     CMACDT        SRDTI                          Accident date
     C                   CALL      'SRCVTDAT'    DTMDYY                          YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        REUSER
     C                   MOVEL     Q1PGM         REPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         REDT
     C*
     C                   MOVEL     DSTIME        RETIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        REUSER
     C                   MOVEL     Q1PGM         REPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         REDT
     C                   MOVEL     DSTIME        RETIME
     C*
     C                   MOVEL     Q1USER        REIUSR
     C                   MOVEL     Q1PGM         REIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         REIDT
     C                   MOVEL     DSTIME        REITM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   ENDSR
