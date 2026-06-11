     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0061RTX - WCMC GROUP STATE INFORMATION TX  C:62 12/17/97    *
     H*                                                               *
     H*  CALLED FROM: F0061R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/02/01 C:62 PROJECT:MCMNT   BASE ONLY                      *
     H*                PROPERLY POPULATE SCMODE                       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1: 71 AND 89.                       *
     H*                                                               *
     H*****************************************************************
     FF0061FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FSTSTP     UF   E             DISK
     F                                     RENAME(PST01:DBRCDF)
     FFMFNP     IF   E             DISK
     FFDF2P     UF A E             DISK
     FFDF2L     IF   E           K DISK
     F                                     RENAME(PF201:PF202)
     F                                     INFDS(DBFDS)
     FFMSML     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Group State-
     D                                      Information TX')
     D COMAPI          C                   CONST('FA007R')

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
     C     @@MODE        IFEQ      'A'
     C*
     C                   MOVE      'A'           XXMODE            1
     C                   MOVE      'C'           @@MODE
     C*
     C                   ENDIF
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
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
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C*
     C     F2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNCO#         ##CO#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    F2ST          KKST
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* MASTER BUSINESS IDENTIFIER CAN'T BE ZERO.
     C*
     C  N90SC9N01        IFEQ      *ZEROS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90FNDT          IFGT      SAVEDT
     C     FNDT          OREQ      SAVEDT
     C     FNTIME        ANDGT     SAVETM
     C                   SETON                                        9089
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
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** BEGIN DEL *** 01/02/01 **************************************
     C********** XXMODE    IFEQ 'A'
     C*
     C**********           MOVELMODEA     SCMODE
     C*
     C**********           ENDIF
     C*** END   DEL *** 01/02/01 **************************************
     C*
     C     @@RRN2        CHAIN(N)  PFN01                              99
     C*
     C                   MOVE      FNSUB#        SCSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCGNAM
     C                   MOVEL     FNSTAT        SCSTAT
     C*
     C                   MOVEL     STKEY         SCST
     C                   MOVEL     STNAME        SCSTNM
     C*
     C     F2KY          CHAIN(N)  PF202                              99
     C*
     C*** BEGIN ADD *** 01/02/01 **************************************
     C   99              MOVEL(P)  MODEA         SCMODE
     C*** END   ADD *** 01/02/01 **************************************
     C                   MOVE      F29N01        SC9N01
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         F2CO#
     C                   MOVE      SCSUB#        F2SUB#
     C                   MOVE      SCFND         F2FND
     C                   MOVEL     SCST          F2ST
     C*
     C                   MOVE      SC9N01        F29N01
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
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   Z-ADD     1             #OFSCR            1 0
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
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update detail record if no record writes detail record
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     F2KY          CHAIN     PF202                              99
     C*
     C  N99DBRRN         CHAIN     PF201                              99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F2USER
     C                   MOVEL     Q1PGM         F2PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2DATE
     C*
     C                   MOVEL     DSTIME        F2TIME
     C*
     C                   UPDATE    PF201
     C*
     C                   ELSE
     C*
     C                   EXSR      WRITED
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITED - Write a detail record when there is already a master
     C*================================================================
     C     WRITED        BEGSR
     C*
     C                   CLEAR                   PF201
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F2USER
     C                   MOVEL     Q1PGM         F2PGM
     C                   MOVEL     Q1PGM         F2IPGM
     C                   MOVEL     Q1USER        F2IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2DATE
     C*
     C                   MOVEL     SRDTO         F2IDT
     C*
     C                   MOVEL     DSTIME        F2TIME
     C                   MOVEL     DSTIME        F2ITIM
     C*
     C                   WRITE     PF201
     C*
     C                   FEOD      FDF2P
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write a record
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
