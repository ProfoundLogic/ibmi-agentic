     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*  CodeReview EOB code update screen
     H*
     H*****************************************************************
     H*  Created on 07/16/02 by C:62 for Project 114MDC
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*****************************************************************
     FF2330FCR  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTEBPCR   UF A E             DISK
     F                                     RENAME(PEB01:DBRCDF)
     FSTABL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D                 DS
     D  EBDSC2                 1     70
     D  XXDSC2                 1      3
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('CodeReview EOMB Code-
     D                                      Maintenance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C                   PARM                    @@CALL            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     PGMLS         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EBCO#         ##CO#
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         ##KEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    STKEY         KKST
     C*
     C/COPY SCOPYMT01C
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
     C* CodeReview Status must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCCRST        KKKEY
     C                   MOVEL(P)  'CRST'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C*
     C* CodeReview Edit Type must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCCRET        KKKEY
     C                   MOVEL(P)  'CRET'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C*
     C* CodeReview Current Action must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCCRCA        KKKEY
     C                   MOVEL(P)  'CRCA'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C*
     C* CodeReview Pend Flag must be valid "Y" or "N" if entered.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRPF        ANDNE     *BLANKS
     C     SCCRPF        ANDNE     'Y'
     C     SCCRPF        ANDNE     'N'
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C* CodeReview History Action must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCCRHA        KKKEY
     C                   MOVEL(P)  'CRHA'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7590
     C                   ENDIF
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90EBDT          IFGT      SAVEDT
     C     EBDT          OREQ      SAVEDT
     C     EBTIME        ANDGT     SAVETM
     C                   SETON                                        7890
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
     C* KJDO - F10 processing
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETON                                        90
     C*
     C                   SELECT
     C*
     C* CodeReview Status Search
     C*
     C     HFLD          WHENEQ    'SCCRST'
     C                   MOVEL(P)  'CRST'        ##TBID
     C                   CALL      'F2380R'      PGMLS
     C*
     C     ##RRN         IFNE      *ZEROS
     C                   SETON                                        38
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCCRST
     C                   MOVEL(P)  GTDESC        SCCRSD
     C                   SETON                                        32
     C                   ELSE
     C                   SETON                                        31
     C                   ENDIF                                                  ##RRN
     C*
     C* CodeReview Edit Type Search
     C*
     C     HFLD          WHENEQ    'SCCRET'
     C                   MOVEL(P)  'CRET'        ##TBID
     C                   CALL      'F2380R'      PGMLS
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCCRET
     C                   MOVEL(P)  GTDESC        SCCRED
     C                   SETON                                        33
     C                   ELSE
     C                   SETON                                        32
     C                   ENDIF                                                  ##RRN
     C*
     C* CodeReview Current Action Search
     C*
     C     HFLD          WHENEQ    'SCCRCA'
     C                   MOVEL(P)  'CRCA'        ##TBID
     C                   CALL      'F2380R'      PGMLS
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCCRCA
     C                   MOVEL(P)  GTDESC        SCCRCD
     C                   SETON                                        34
     C                   ELSE
     C                   SETON                                        33
     C                   ENDIF                                                  ##RRN
     C*
     C* CodeReview History Action Search
     C*
     C     HFLD          WHENEQ    'SCCRHA'
     C                   MOVEL(P)  'CRHA'        ##TBID
     C                   CALL      'F2380R'      PGMLS
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCCRHA
     C                   MOVEL(P)  GTDESC        SCCRHD
     C                   ELSE
     C                   SETON                                        35
     C                   ENDIF                                                  ##RRN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     @@CALL        IFEQ      'A'
     C                   MOVEL(P)  MODEA         SCMODE
     C                   ENDIF
     C*
     C                   MOVEL     EBKEY         SCKEY
     C                   MOVEL     EBDESC        SCDESC
     C                   MOVEL     EBSTAT        SCSTAT
     C                   MOVEL(P)  EBCRST        SCCRST
     C                   MOVEL(P)  EBCRST        KKKEY
     C                   MOVEL(P)  'CRST'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCRSD
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCCRSD
     C                   ENDIF
     C*
     C                   MOVEL(P)  EBCRET        SCCRET
     C                   MOVEL(P)  EBCRET        KKKEY
     C                   MOVEL(P)  'CRET'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCRED
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCCRED
     C                   ENDIF
     C*
     C                   MOVEL(P)  EBCRCA        SCCRCA
     C                   MOVEL(P)  EBCRCA        KKKEY
     C                   MOVEL(P)  'CRCA'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCRCD
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCCRCD
     C                   ENDIF
     C*
     C                   MOVEL(P)  EBCRHA        SCCRHA
     C                   MOVEL(P)  EBCRHA        KKKEY
     C                   MOVEL(P)  'CRHA'        KKTBID
     C*
     C                   MOVEL(P)  EBCRPF        SCCRPF
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCRHD
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCCRHD
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL(P)  SCCRST        EBCRST
     C                   MOVEL(P)  SCCRET        EBCRET
     C                   MOVEL(P)  SCCRCA        EBCRCA
     C                   MOVEL(P)  SCCRPF        EBCRPF
     C                   MOVEL(P)  SCCRHA        EBCRHA
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
      *
      *  Process F10 search
      *
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   RETURN
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
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
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
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     DSTIME        EBTIME
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
     C                   MOVEL     Q1USER        EBUSER
     C                   MOVEL     Q1PGM         EBPGM
     C                   MOVEL     Q1USER        EBIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C*
     C                   MOVEL     SRDTO         EBIDT
     C*
     C                   MOVEL     DSTIME        EBTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTEBPCR
     C*
     C                   ENDSR
