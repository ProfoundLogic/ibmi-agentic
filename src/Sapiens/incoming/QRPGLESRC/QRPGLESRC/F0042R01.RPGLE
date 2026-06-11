     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0042R01 - PROVIDER STATE INFORMATION   C:62 03/26/98        *
     H*                                                               *
     H*  CALLED FROM: F0042R                                          *
     H*                                                               *
     H*  CALLS TO   : NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/26/05 C:079 PROJ: MI001                                   *
     H*                USE SCAM01 FOR MI                              *
     H*                                                               *
     H*  12/22/04 C:079 SUPPORT:44873                                 *
     H*                USE SCAM01 FOR NY                              *
     H*                                                               *
     H*  12/14/04 C:062 PROJECT:SUPPORT                               *
     H*                 ALLOW HOSPITAL TO ACT LIKE A PHYSICIAN        *
     H*                                                               *
     H*  07/24/04 C:079 PROJECT:MA012                                 *
     H*                Z-ADD Fee Sched Value to DB for state=MA       *
     H*                                                               *
     H*  08/06/02 C:62 PROJECT ALMNT # 32464                          *
     H*                Use SCAM03 if state = AL                       *
     H*                Hard coding "THRSH" for use with Threshold     *
     H*                                                               *
     H*  01/08/02 C:79 PROJECT WV004                                  *
     H*                Use SCAM03 if state = WV                       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0042F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMP2P     UF A E             DISK
     F                                     RENAME(PP201:DBRCDF)
     FFMP2L1    IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMPDP     IF   E             DISK
     FFMSIL     IF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN
     FFMSVL1    IF   E           K DISK    USROPN
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D                 DS
     D  ##CMD1                 1     56
     D  ##ST1                 38     39
     D                 DS
     D  ##CMD2                 1     56
     D  ##ST2                 40     41
     D                 DS
     D  SCSV01                 1     11
     D  SCSVC2                 1      3
     D  SCSVC1                 4      8
     D  SCSVC3                 9     11
     D                 DS
     D  HDSV01                 1     11
     D  HDSVC2                 1      3
     D  HDSVC1                 4      8
     D  HDSVC3                 9     11
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider State Inf-
     D                                     ormation')
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
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*  Search parameter list.
     C*
     C     SCHPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     P2KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKSV01
     C*
     C     SVKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCMSV
     C                   KFLD                    KKEFDT
     C*
     C     SVKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCMSV
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     SIKY          KLIST
     C                   KFLD                    KKAJST
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKSPCD
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
     C     *LIKE         DEFINE    P2FEIN        KKFEIN
     C     *LIKE         DEFINE    P2MOD         KKMOD
     C     *LIKE         DEFINE    P2EFDT        KKEFDT
     C     *LIKE         DEFINE    P2SV01        KKSV01
     C     *LIKE         DEFINE    PDST          KKAJST
     C     *LIKE         DEFINE    PDTYPE        KKTYPE
     C     *LIKE         DEFINE    PDSPCD        KKSPCD
     C     *LIKE         DEFINE    PRCO#         KKCO#
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    SVCMSV        KKCMSV
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    SCEFDT        HDEFDT
     C     *LIKE         DEFINE    SCSDSC        HDSDSC
     C     *LIKE         DEFINE    SCAM01        HDAM01
     C*** BEG ADD *** 01/08/02 ****************************************
     C     *LIKE         DEFINE    SCAM03        HDAM03
     C*** END ADD *** 01/08/02 ****************************************
     C     *LIKE         DEFINE    SCFL01        HDFL01
     C     *LIKE         DEFINE    SCTDSC        HDTDSC
     C     *LIKE         DEFINE    SCAM02        HDAM02
     C     *LIKE         DEFINE    SCFL02        HDFL02
     C     *LIKE         DEFINE    SCPLDS        HDPLDS
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   ENDSR
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
     C                   MOVEA     '00000'       *IN(31)
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* MAKE SURE EFFECTIVE DATE IS VALID.
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C  N90SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      *HIVAL        KKEFDT
     C*
     C* OPEN/CLOSE AND SET/DELETE OVERRIDE.
     C*
     C  N90              EXSR      OPNOVR
     C*
     C  N90              SELECT
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '1'
     C     *IN16         WHENEQ    *ON
     C*** END   CHG *** 12/14/04 **************************************
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL(P)  SCSVC1        KKCPT4
     C*
     C     PRKY2         CHAIN     PPR01                              99
     C*
     C   99              SETON                                        7290
     C  N99              MOVEL     PRDSC1        SCSDSC
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '4'
     C     *IN16         WHENEQ    *OFF
     C*** END   CHG *** 12/14/04 **************************************
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL(P)  SCSV01        KKCMSV
     C*
     C     SVKY2         CHAIN     PSV01                              99
     C*
     C   99              SETON                                        7290
     C  N99              MOVEL     SVDESC        SCSDSC
     C*
     C                   ENDSL
     C*
     C* CHECK FOR DUPLICATE RECORD
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   MOVEL     SCSV01        KKSV01
     C*
     C     P2KY          CHAIN     PP201                              99
     C*
     C  N99              SETON                                        7390
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* FEE SCHEDULE VALUE CAN'T BE ZERO
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C*
     C* Use SCAM03 (A9.2) if state is WV
     C*
     C     *IN39         IFEQ      *ON
     C*
     C  N90SCAM03        IFEQ      *ZEROS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C*** BEGIN ADD *** 08/06/02 **************************************
     C     SCSVC1        IFNE      'THRSH'
     C     *IN18         OREQ      *OFF
     C*** END   ADD *** 08/06/02 **************************************
     C  N90SCAM01        IFEQ      *ZEROS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/06/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/06/02 **************************************
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   ENDIF                                                  *IN39 = *ON
     C*** END ADD *** 01/08/02 ****************************************
     C*** BEGIN ADD *** 08/06/02 **************************************
     C     SCSVC1        IFNE      'THRSH'
     C     *IN18         OREQ      *OFF
     C     SCFL01        ORNE      *BLANKS
     C*** END   ADD *** 08/06/02 **************************************
     C*
     C* TYPE MUST BE A VALID CODE.
     C*
     C                   MOVEL(P)  SCFL01        KKTKEY
     C                   MOVEL(P)  'SITY'        KKTBID
     C*
     C  N90GTKY          CHAIN     PGT02                              99
     C*
     C  N90
     CAN 99              SETON                                        7590
     C  N90
     CANN99              MOVEL     GTDESC        SCTDSC
     C*** BEGIN ADD *** 08/06/02 **************************************
     C                   ENDIF
     C*
     C* If threshold code then discount % is required
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSVC1        ANDEQ     'THRSH'
     C     SCAM02        ANDEQ     *ZEROS
     C                   SETON                                        7790
     C                   ENDIF
     C*
     C* If threshold code then discount % is required
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSVC1        ANDEQ     'THRSH'
     C     SCAL03        ANDEQ     *ZEROS
     C                   SETON                                        7890
     C                   ENDIF
     C*** END   ADD *** 08/06/02 **************************************
     C*
     C* PAY LESSER MUST BE A VALID CODE IF NOT BLANK.
     C*
     C  N90SCFL02        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCFL02        KKTKEY
     C                   MOVEL(P)  'SIPL'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7690
     C  N99              MOVEL     GTDESC        SCPLDS
     C*
     C                   ENDIF
     C*
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   MOVEA     '00000'       *IN(31)
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCSVC1'
     C     HFLD          OREQ      'SCSVC2'
     C     HFLD          OREQ      'SCSVC3'
     C*
     C                   SELECT
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '1'
     C     *IN16         WHENEQ    *ON
     C*** END   CHG *** 12/14/04 **************************************
     C*
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCAJST        ##ST              2
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C     *INKL         IFEQ      *ON
     C     ##CPT4        OREQ      *BLANKS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ##CPT4        SCSVC1
     C                   MOVEL     ##DESC        SCSDSC
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '4'
     C     *IN16         WHENEQ    *OFF
     C*** END   CHG *** 12/14/04 **************************************
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCAJST        ##ST              2
     C                   PARM      *BLANKS       ##CMSV           15
     C                   PARM      *BLANKS       ##CDSC           40
     C*
     C     *INKL         IFEQ      *ON
     C     ##CMSV        OREQ      *BLANKS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C                   MOVEL     ##CMSV        SCSV01
     C                   MOVEL     ##CDSC        SCSDSC
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C     HFLD          WHENEQ    'SCFL01'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'SITY'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCFL01
     C                   MOVEL(P)  GTDESC        SCTDSC
     C                   SETON                                        34
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCFL02'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'SIPL'        ##TBID
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        35
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCFL02
     C                   MOVEL(P)  GTDESC        SCPLDS
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   EXSR      DATCVT
     C                   SETON                                          90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C* OPEN/CLOSE AND SET/DELETE OVERRIDE.
     C*
     C                   EXSR      OPNOVR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   Z-ADD     P2EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     P2SV01        SCSV01
     C*
     C                   SELECT
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '1'
     C     *IN16         WHENEQ    *ON
     C*** END   CHG *** 12/14/04 **************************************
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL(P)  SCSVC1        KKCPT4
     C                   MOVEL     *HIVAL        KKEFDT
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C*
     C   99              MOVEL     *BLANKS       SCSDSC
     C  N99              MOVEL     PRDSC1        SCSDSC
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '4'
     C     *IN16         WHENEQ    *OFF
     C*** END   CHG *** 12/14/04 **************************************
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL(P)  SCSV01        KKCMSV
     C                   MOVEL     *HIVAL        KKEFDT
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C*
     C   99              MOVEL     *BLANKS       SCSDSC
     C  N99              MOVEL     SVDESC        SCSDSC
     C*
     C                   ENDSL
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C     *IN39         IFEQ      *ON
     C*
     C                   Z-ADD     P2AM01        SCAM03
     C*
     C                   ELSE
     C*** END ADD *** 01/08/02 ****************************************
     C*** BEG ADD *** 07/24/04 ****************************************
     C     SCAJST        IFEQ      'MA'
     C*** BEG ADD *** 12/22/04 ****************************************
     C     SCAJST        OREQ      'NY'
     C*** END ADD *** 12/22/04 ****************************************
     C*** BEG ADD *** 04/26/05 ****************************************
     C     SCAJST        OREQ      'MI'
     C*** END ADD *** 04/26/05 ****************************************
     C                   Z-ADD     P2AM01        SCAM01
     C                   ELSE
     C*** END ADD *** 07/24/04 ****************************************
     C                   MOVE      P2AM01        SCAM01
     C*** BEG ADD *** 07/24/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 07/24/04 ****************************************
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   ENDIF                                                  *IN39 IFEQ *ON
     C*** END ADD *** 01/08/02 ****************************************
     C*** BEGIN ADD *** 08/06/02 **************************************
     C     *IN18         IFEQ      *ON
     C                   Z-ADD     P2AM03        SCAL03
     C                   ENDIF
     C*** END   ADD *** 08/06/02 **************************************
     C*
     C                   MOVEL     P2FL01        SCFL01
     C*
     C                   MOVEL(P)  P2FL01        KKTKEY
     C                   MOVEL(P)  'SITY'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              MOVEL     *BLANKS       SCTDSC
     C  N99              MOVEL     GTDESC        SCTDSC
     C*
     C                   MOVE      P2AM02        SCAM02
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEL     P2FL02        SCFL02
     C*
     C                   MOVEL(P)  P2FL02        KKTKEY
     C                   MOVEL(P)  'SIPL'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              MOVEL     *BLANKS       SCPLDS
     C  N99              MOVEL     GTDESC        SCPLDS
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCPLDS
     C*
     C                   ENDIF
     C*
     C     P2STAT        IFNE      *BLANKS
     C*
     C                   MOVEL     P2STAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             SCEFDT
     C                   MOVEL     *BLANKS       SCSV01
     C                   MOVEL     *BLANKS       SCSDSC
     C                   Z-ADD     0             SCAM01
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   Z-ADD     0             SCAM03
     C*** END ADD *** 01/08/02 ****************************************
     C*** BEGIN ADD *** 08/06/02 **************************************
     C                   Z-ADD     0             SCAL03
     C*** END   ADD *** 08/06/02 **************************************
     C                   MOVEL     *BLANKS       SCFL01
     C                   MOVEL     *BLANKS       SCTDSC
     C                   Z-ADD     0             SCAM02
     C                   MOVEL     *BLANKS       SCFL02
     C                   MOVEL     *BLANKS       SCPLDS
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
     C                   MOVE      SCFEIN        P2FEIN
     C                   MOVE      SCMOD         P2MOD
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    IFEQ '1'
     C     *IN16         IFEQ      *ON
     C*** END   CHG *** 12/14/04 **************************************
     C*
     C                   MOVEL     *BLANKS       SCSVC2
     C                   MOVEL     *BLANKS       SCSVC3
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2EFDT
     C*
     C                   MOVEL     SCSV01        P2SV01
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C     *IN39         IFEQ      *ON
     C*
     C                   Z-ADD     SCAM03        P2AM01
     C*
     C                   ELSE
     C*** END ADD *** 01/08/02 ****************************************
     C*** BEG ADD *** 07/24/04 ****************************************
     C     SCAJST        IFEQ      'MA'
     C*** BEG ADD *** 12/22/04 ****************************************
     C     SCAJST        OREQ      'NY'
     C*** END ADD *** 12/22/04 ****************************************
     C*** BEG ADD *** 04/26/05 ****************************************
     C     SCAJST        OREQ      'MI'
     C*** END ADD *** 04/26/05 ****************************************
     C                   Z-ADD     SCAM01        P2AM01
     C                   ELSE
     C*** END ADD *** 07/24/04 ****************************************
     C                   MOVE      SCAM01        P2AM01
     C*** BEG ADD *** 07/24/04 ****************************************
     C                   ENDIF
     C*** END ADD *** 07/24/04 ****************************************
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   ENDIF                                                  *IN39 = *ON
     C*** END ADD *** 01/08/02 ****************************************
     C*** BEGIN ADD *** 08/06/02 **************************************
     C     *IN18         IFEQ      *ON
     C                   Z-ADD     SCAL03        P2AM03
     C                   ENDIF
     C*** END   ADD *** 08/06/02 **************************************
     C*
     C                   MOVEL     SCFL01        P2FL01
     C                   MOVE      SCAM02        P2AM02
     C                   MOVEL     SCFL02        P2FL02
     C                   MOVEL     SCSTAT        P2STAT
     C*
     C* Save screen field so that they can be used with dup key.
     C*
     C   40              EXSR      SAVDUP
     C*
     C                   ENDSR
     C*================================================================
     C* OPNOVR - Subroutine that opens and overrides FMP2P/FMP2L1
     C*================================================================
     C     OPNOVR        BEGSR
     C*
     C* Checks to see if files are open, if they are then close them. .
     C*
     C     OPNFL1        IFEQ      '1'
     C*
     C   16              CLOSE     FMPRL
     C  N16              CLOSE     FMSVL1
     C                   MOVEL     ' '           OPNFL1
     C*
     C                   ENDIF
     C*
     C* Checks to see if the files have overrides, if so it deletes it.
     C*
     C     OVRFL1        IFEQ      '1'
     C*
     C* FMPRL
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   Z-ADD     56            LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C                   MOVEL     ' '           OVRFL1
     C*
     C                   ELSE
     C*
     C* FMSVL1
     C*
     C                   Z-ADD     56            LENGTH
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C                   MOVEL     ' '           OVRFL1
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* If files have no overrides, then the overrides are established.
     C*
     C     OVRFL1        IFEQ      ' '
     C*
     C     *IN16         IFEQ      *ON
     C*
     C* FMPRL
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        ##CMD1
     C                   MOVEL     SCAJST        ##ST1
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    ##CMD1
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     '1'           OVRFL1
     C*
     C                   ELSE
     C*
     C* FMSVL1
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(4)        ##CMD2
     C                   MOVEL     SCAJST        ##ST2
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    ##CMD2
     C                   PARM                    LENGTH
     C*
     C                   MOVEL     '1'           OVRFL1
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* If the files are closed then they are openned.
     C*
     C     OPNFL1        IFEQ      ' '
     C*
     C   16              OPEN      FMPRL
     C  N16              OPEN      FMSVL1
     C                   MOVEL     '1'           OPNFL1
     C*
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
     C     *INKJ         CASEQ     *ON           KJDO
     C     *IN40         CASEQ     *ON           PRCDUP
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   EXSR      OPNOVR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL     ' '           OPNFL1            1
     C                   MOVEL     ' '           OVRFL1            1
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
     C* GET PROVIDER INFORMATION
     C*
     C     @@RRN2        CHAIN     PPD01                              99
     C*
     C                   MOVE      PDFEIN        SCFEIN
     C                   MOVE      PDMOD         SCMOD
     C                   MOVEL     PDNAME        SCPNAM
     C                   MOVEL     PDTYPE        SCPTYP
     C                   MOVEL     PDSPCD        SCSPCD
     C*
     C                   MOVE      PDFEIN        KKFEIN
     C                   MOVE      PDMOD         KKMOD
     C*
     C     P1KY          CHAIN     PP101                              99
     C*
     C                   MOVEL     P1ST          SCAJST
     C*
     C* SET INDICATOR TO DETERMINE WHETHER TO USE PROCEDURE CODE
     C* OR SERVICE CODE.
     C*
     C                   SELECT
     C*
     C     SCPTYP        WHENEQ    '1'
     C*** BEGIN ADD *** 12/14/04 **************************************
     C     PDSPCD        IFEQ      'REH'
     C     P1ST          ANDEQ     'KY'
     C     PDSPCD        OREQ      'ASC'
     C     P1ST          ANDEQ     'KY'
     C                   SETOFF                                       16
     C                   ELSE
     C*** END   ADD *** 12/14/04 **************************************
     C                   SETON                                        16
     C*** BEGIN ADD *** 12/14/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/14/04 **************************************
     C*
     C     SCPTYP        WHENEQ    '4'
     C                   SETOFF                                       16
     C*
     C                   ENDSL
     C*
     C                   MOVEL     SCAJST        KKAJST
     C                   MOVEL     SCPTYP        KKTYPE
     C*
     C                   SELECT
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '1'
     C     *IN16         WHENEQ    *ON
     C*** END   CHG *** 12/14/04 **************************************
     C                   MOVEL     SCSPCD        KKSPCD
     C*
     C*** BEGIN CHG *** 12/14/04 **************************************
     C********** SCPTYP    WHEQ '4'
     C     *IN16         WHENEQ    *OFF
     C*** END   CHG *** 12/14/04 **************************************
     C                   MOVEL     *BLANKS       KKSPCD
     C*
     C                   ENDSL
     C*
     C     SIKY          CHAIN     PSI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     SIFL02        ANDEQ     'Y'
     C*
     C                   SETON                                        17
     C                   MOVEL     SIF2HD        SCF2HD
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       17
     C                   MOVEL     *BLANKS       SCF2HD
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C*
     C* Set indicator to determine screen field for P2AM01
     C*
     C     SCAJST        IFEQ      'WV'
     C                   SETON                                        39
     C                   ELSE
     C                   SETOFF                                       39
     C                   ENDIF
     C*** END ADD *** 01/08/02 ****************************************
     C*** BEGIN ADD *** 08/06/02 **************************************
     C*
     C* Let's use SCAM03 and condition heading, if state is Alabama
     C*
     C     SCAJST        IFEQ      'AL'
     C     SCPTYP        ANDEQ     '4'
     C                   SETON                                        18
     C                   ELSE
     C                   SETOFF                                       18
     C                   ENDIF
     C*** END   ADD *** 08/06/02 **************************************
     C*
     C* OPEN AND SET OVERRIDES OF FMP2P AND FMP2L1.
     C*
     C                   EXSR      OPNOVR
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
     C* PRCDUP - Process duplicate key.
     C*================================================================
     C     PRCDUP        BEGSR
     C*
     C     *IN41         IFEQ      *ON
     C*
     C                   MOVE      HDEFDT        SCEFDT
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C     *IN42         IFEQ      *ON
     C*
     C                   MOVEL     HDSVC1        SCSVC1
     C   16              MOVEL     HDSDSC        SCSDSC
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C     *IN43         IFEQ      *ON
     C*
     C                   MOVEL     HDSVC2        SCSVC2
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C     *IN44         IFEQ      *ON
     C*
     C                   MOVEL     HDSVC3        SCSVC3
     C                   SETON                                        90
     C*
     C     *IN42         IFEQ      *ON
     C     *IN43         ANDEQ     *ON
     C     *IN44         ANDEQ     *ON
     C*
     C                   MOVEL     HDSDSC        SCSDSC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN45         IFEQ      *ON
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C     *IN39         IFEQ      *ON
     C*
     C                   Z-ADD     HDAM03        SCAM03
     C*
     C                   ELSE
     C*** END ADD *** 01/08/02 ****************************************
     C                   MOVE      HDAM01        SCAM01
     C*** BEG ADD *** 01/08/02 ****************************************
     C*
     C                   ENDIF                                                  *IN39 = *ON
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C     *IN46         IFEQ      *ON
     C*
     C                   MOVEL     HDFL01        SCFL01
     C                   MOVEL     HDTDSC        SCTDSC
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C     *IN47         IFEQ      *ON
     C*
     C                   MOVE      HDAM02        SCAM02
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C     *IN48         IFEQ      *ON
     C*
     C                   MOVEL     HDFL02        SCFL02
     C                   MOVEL     HDPLDS        SCPLDS
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C                   EXSR      DATCVT
     C*
     C                   ENDSR
     C*================================================================
     C* SAVDUP - Place duplicate fields in holding fields.
     C*================================================================
     C     SAVDUP        BEGSR
     C*
     C                   MOVE      SCEFDT        HDEFDT
     C                   MOVEL     SCSV01        HDSV01
     C                   MOVEL     SCSDSC        HDSDSC
     C*** BEG ADD *** 01/08/02 ****************************************
     C     *IN39         IFEQ      *ON
     C*
     C                   Z-ADD     SCAM03        HDAM03
     C*
     C                   ELSE
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   MOVE      SCAM01        HDAM01
     C*
     C*** BEG ADD *** 01/08/02 ****************************************
     C                   ENDIF                                                  *IN39 = *ON
     C*** END ADD *** 01/08/02 ****************************************
     C*
     C                   MOVEL     SCFL01        HDFL01
     C                   MOVEL     SCTDSC        HDTDSC
     C                   MOVE      SCAM02        HDAM02
     C   17              MOVEL     SCFL02        HDFL02
     C   17              MOVEL     SCPLDS        HDPLDS
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
     C                   MOVEL     Q1USER        P2USER
     C                   MOVEL     Q1PGM         P2PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2DT
     C*
     C                   MOVEL     DSTIME        P2TIME
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
     C                   MOVEL     Q1USER        P2USER
     C                   MOVEL     Q1PGM         P2PGM
     C                   MOVEL     Q1PGM         P2IPGM
     C                   MOVEL     Q1USER        P2IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         P2DT
     C*
     C                   MOVEL     SRDTO         P2IDT
     C*
     C                   MOVEL     DSTIME        P2TIME
     C                   MOVEL     DSTIME        P2ITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMP2P
     C*
     C                   ENDSR
** QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
