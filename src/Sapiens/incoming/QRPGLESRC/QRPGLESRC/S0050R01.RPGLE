     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H****************************************************************
     H*                                                              *
     H*  CREATED:  07/14/98   C:94  PROJECT:  MC179                  *
     H*                                                              *
     H****************************************************************
     H*                                                              *
     H*  REVISIONS:                                                  *
     H*                                                              *
     H****************************************************************
     H*
     FS0050F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSMCOP     UF A E             DISK
     F                                     RENAME(PCO01:DBRCDF)
     FSMCOL     IF   E           K DISK
     FSTSTP     IF   E             DISK
     FSTABL     IF   E           K DISK
     F                                     RENAME(PST01:PST02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Company Master Inf-
     D                                     ormation')
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
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    COCO#         KKCO#
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
     C     COSTAT        IFEQ      'I'
     C                   MOVE      'A'           COSTAT
     C                   ELSE
     C                   MOVE      'I'           COSTAT
     C                   ENDIF                                                  COSTAT
     C*
     C                   MOVEL     Q1USER        COUSER
     C                   MOVEL     Q1PGM         COPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   MOVEL     DSTIME        COTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
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
     C*** COMPANY NUMBER IS REQUIRED
     C*
     C  N90
     CAN 40SCCO#         IFEQ      *ZEROS
     C*
     C                   SETON                                          9071
     C*
     C                   ENDIF
     C*
     C*** PREVENT DUPLICATE COMPANY NUMBER
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   Z-ADD     SCCO#         KKCO#
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C  N99              SETON                                          9077
     C*
     C                   ENDIF
     C*
     C*** COMPANY NAME IS REQUIRED
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C*
     C                   SETON                                          9072
     C*
     C                   ENDIF
     C*
     C*** COMPANY ADDRESS IS REQUIRED
     C*
     C  N90SCADR1        IFEQ      *BLANKS
     C*
     C                   SETON                                          9073
     C*
     C                   ENDIF
     C*
     C*** CITY IS REQUIRED
     C*
     C  N90SCCTY         IFEQ      *BLANKS
     C*
     C                   SETON                                          9074
     C*
     C                   ENDIF
     C*
     C*** STATE IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCST          KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C   99              SETON                                          9075
     C*
     C                   ENDIF
     C*
     C*** ZIP CODE IS REQUIRED
     C*
     C  N90SCZIP1        IFEQ      *ZEROS
     C*
     C                   SETON                                          9076
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       3031
     C*
     C                   SELECT
     C*
     C*** STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCST'
     C*
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        30
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   MOVEL(P)  STKEY         SCST
     C                   SETON                                        31
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
     C                   MOVEL     *ZEROS        SCCO#
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVEL     *ZEROS        SCZIP1
     C                   MOVE      *BLANKS       SCZIP2
     C                   Z-ADD     0             SCFEIN
     C                   Z-ADD     0             SC1099
     C                   Z-ADD     0             SCAREA
     C                   Z-ADD     0             SCPHN#
     C                   Z-ADD     0             SCFARA
     C                   Z-ADD     0             SCFPHN
     C*
     C     @@MODE        IFNE      'A'                                          NOT ADD MODE
     C                   Z-ADD     COCO#         SCCO#
     C                   MOVE      COSTAT        SCSTAT
     C                   MOVE      CONAME        SCNAME
     C                   MOVE      COADR1        SCADR1
     C                   MOVE      COADR2        SCADR2
     C                   MOVE      COCTY         SCCTY
     C                   MOVE      COST          SCST
     C                   MOVEL     COZIP         SCZIP1
     C                   MOVE      COZIP         SCZIP2
     C                   Z-ADD     COFEIN        SCFEIN
     C                   Z-ADD     CO1099        SC1099
     C                   Z-ADD     COAREA        SCAREA
     C                   Z-ADD     COPHN#        SCPHN#
     C                   Z-ADD     COFARA        SCFARA
     C                   Z-ADD     COFAX#        SCFPHN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      SCCO#         COCO#
     C                   MOVEL     SCSTAT        COSTAT
     C                   MOVEL     SCNAME        CONAME
     C                   MOVEL     SCADR1        COADR1
     C                   MOVEL     SCADR2        COADR2
     C                   MOVEL     SCCTY         COCTY
     C                   MOVEL     SCST          COST
     C                   MOVEL     SCZIP1        COZIP
     C                   MOVE      SCZIP2        COZIP
     C                   Z-ADD     SCFEIN        COFEIN
     C                   Z-ADD     SC1099        CO1099
     C                   Z-ADD     SCAREA        COAREA
     C                   Z-ADD     SCPHN#        COPHN#
     C                   Z-ADD     SCFARA        COFARA
     C                   Z-ADD     SCFPHN        COFAX#
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
     C                   SETOFF                                       3031
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      SMCOP
     C                   ENDSR
