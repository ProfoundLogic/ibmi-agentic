     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*      F2500R01 - EDI DATA ELEMENT MAINTENANCE
     H*
     H*================================================================
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  04/11/02 C:62  PROJECT: MCMNT LOG# 216
     H*                 USE SY600R INSTEAD OF SY601R.
     H*
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*****************************************************************
     H*
     FF2500F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTDNP     UF A E             DISK
     F                                     RENAME(PDN01:DBRCDF)
     F*
     F*  Logical file definition (for verifying record)
     FFTDNL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN DEL *** 04/11/02 **************************************
     F**********STSTP   IF  E                    DISK
     F*** END   DEL *** 04/11/02 **************************************
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F                                     RENAME(PST01:PST02)
     F*
     D***PY SCOPYMT01I
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Data Element Cr-
     D                                     iteria')
     D COMAPI          C                   CONST('FA013R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     DNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKJST
     C                   KFLD                    KKTSET
     C                   KFLD                    KKDN#
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    DNJST         KKJST
     C     *LIKE         DEFINE    DNTSET        KKTSET
     C     *LIKE         DEFINE    DNDN#         KKDN#
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DNSTAT        IFEQ      'I'
     C*
     C                   MOVE      'A'           DNSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      'I'           DNSTAT
     C*
     C                   ENDIF                                                  DNSTAT
     C*
     C                   MOVEL     Q1USER        DNUSER
     C                   MOVEL     Q1PGM         DNPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DNDT
     C*
     C                   MOVEL     DSTIME        DNTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  CHGSTS
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
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* VALIDATE THE JURISDICTION STATE
     C*
     C   40*IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCJST         KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* PREVENTS DUPLICATE ENTRIES.
     C*
     C   40*IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCTSET        KKTSET
     C                   Z-ADD     SCDN#         KKDN#
     C*
     C     DNKY          CHAIN     PDN01                              99
     C*
     C  N99              SETON                                        7690
     C*
     C                   ENDIF
     C*
     C* VALIDATE THE TRANSACTION SET ID
     C*
     C   40*IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCTSET        KKTKEY
     C                   MOVEL(P)  'TSET'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* DATA ELEMENT NUMBER CAN'T BE ZERO
     C*
     C   40*IN90         IFEQ      *OFF
     C     SCDN#         ANDEQ     *ZEROS
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* DATA ELEMENT DESCRIPTION CAN'T BE BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDESC        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* VALIDATE THE TRANSACTION SET ID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCCRTR        KKTKEY
     C                   MOVEL(P)  'EDIC'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7590
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90DNDT          IFGT      SAVEDT
     C     DNDT          OREQ      SAVEDT
     C     DNTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
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
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCJST'
     C*
     C*** BEGIN CHG *** 04/11/02 **************************************
     C**********           CALL 'SY601R'
     C**********           PARM '2'       ##CALL  1
     C********** *INKC     PARM '0'       ##KC    1
     C********** *INKL     PARM '0'       ##KL    1
     C**********           PARM *ZEROS    ##RRN   90
     C*
     C********** *INKL     IFEQ *ON
     C********** ##RRN     OREQ *ZEROS
     C*
     C**********           SETON                     31
     C*
     C**********           ELSE
     C*
     C********** ##RRN     CHAINPST01                99
     C*
     C**********           MOVELSTKEY     SCJST     P
     C**********           SETON                     32
     C*
     C**********           ENDIF
     C**********
     C                   CALL      'SY600R'
     C                   PARM      *BLANKS       ##ST              2
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##ST          OREQ      *BLANKS
     C                   SETON                                        31
     C                   ELSE
     C                   MOVEL     ##ST          SCJST
     C                   SETON                                        32
     C                   ENDIF
     C*** END   CHG *** 04/11/02 **************************************
     C*
     C     HFLD          WHENEQ    'SCTSET'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'TSET'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   SETON                                        33
     C                   MOVEL(P)  GTKEY         SCTSET
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCRTR'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'EDIC'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        34
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCCRTR
     C*
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
     C                   MOVEL(P)  DNJST         SCJST
     C                   MOVEL(P)  DNTSET        SCTSET
     C                   Z-ADD     DNDN#         SCDN#
     C                   MOVEL(P)  DNCRTR        SCCRTR
     C                   MOVEL(P)  DNDESC        SCDESC
     C                   MOVEL(P)  DNSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCJST
     C                   MOVEL     *BLANKS       SCTSET
     C                   Z-ADD     *ZEROS        SCDN#
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCCRTR
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         DNCO#
     C                   MOVEL     SCJST         DNJST
     C                   MOVEL     SCTSET        DNTSET
     C                   Z-ADD     SCDN#         DNDN#
     C                   MOVEL     *BLANKS       DNMTC
     C                   MOVEL     SCDESC        DNDESC
     C                   MOVEL     SCCRTR        DNCRTR
     C                   MOVEL     SCSTAT        DNSTAT
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
     C                   MOVEA     '0000'        *IN(31)
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
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
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
     C                   MOVEL     Q1USER        DNUSER
     C                   MOVEL     Q1PGM         DNPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DNDT
     C*
     C                   MOVEL     DSTIME        DNTIME
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
     C                   MOVEL     Q1USER        DNUSER
     C                   MOVEL     Q1PGM         DNPGM
     C                   MOVEL     Q1USER        DNIUSR
     C                   MOVEL     Q1PGM         DNIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DNDT
     C                   MOVEL     SRDTO         DNIDT
     C*
     C                   MOVEL     DSTIME        DNTIME
     C                   MOVEL     DSTIME        DNITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTDNP
     C*
     C                   ENDSR
