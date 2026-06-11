     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0061RFL - WCMC GROUP STATE INFORMATION FL  C:62 12/17/97    *
     H*                                                               *
     H*  CALLED FROM: F0061R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/26/05  C:079  PROJ: 135MEQ                                *
     H*                   DONT REQUIRE SERVICE CO/TPA                 *
     H*                                                               *
     H*  01/30/01  C:117  PROJ: FSMNT #152  SUPP SEQ# 26111           *
     H*                   SPLITTING THE ZIP CODE FIELD INTO 2         *
     H*                   SEPERATE FIELDS - 9N02 & 9N04               *
     H*                                                               *
     H*  01/02/01 C:62 PROJECT:MCMNT  BASE ONLY                       *
     H*                PROPERLY POPULATE SCMODE                       *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1: 71 - 76, 89.                     *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0061FFL  CF   E             WORKSTN
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
     D*** BEG ADD *** 01/30/01 ****************************************
     D                 DS
     D  DS9N02                 1      9  0
     D  DS9N2A                 1      5  0
     D  DS9N2B                 6      9  0
     D*
     D                 DS
     D  DS9N04                 1      9  0
     D  DS9N4A                 1      5  0
     D  DS9N4B                 6      9  0
     D*** END ADD *** 01/30/01 ****************************************
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Group State-
     D                                      Information FL')
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
     C* CARRIER INTERNAL AUDIT # CANT BE ZERO.
     C*
     C  N90SC5N01        IFEQ      *ZEROS
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* CARRIER FEDERAL ID # CAN'T BE ZERO.
     C*
     C  N90SC9N01        IFEQ      *ZEROS
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* CARRIER ZIP CODE CAN'T BE ZERO.
     C*
     C*** BEG CHG *** 01/30/01 ****************************************
     C*****N90***SC9N02    IFEQ *ZEROS
     C  N90SC9N2A        IFEQ      *ZEROS
     C*** END CHG *** 01/30/01 ****************************************
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* AGENT INTERNAL AUDIT # CANT BE ZERO.
     C*
     C*** BEG ADD *** 07/26/05 ****************************************
     C     XXCLT#        IFNE      135
     C*** END ADD *** 07/26/05 ****************************************
     C  N90SC5N02        IFEQ      *ZEROS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* AGENT FEDERAL ID # CAN'T BE ZERO.
     C*
     C  N90SC9N03        IFEQ      *ZEROS
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C*
     C* AGENT ZIP CODE CAN'T BE ZERO.
     C*
     C*** BEG CHG *** 01/30/01 ****************************************
     C*****N90***SC9N04    IFEQ *ZEROS
     C  N90SC9N4A        IFEQ      *ZEROS
     C*** BEG CHG *** 01/30/01 ****************************************
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF
     C*** BEG ADD *** 07/26/05 ****************************************
     C                   ENDIF
     C*** END ADD *** 07/26/05 ****************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90F2DATE        IFGT      SAVEDT
     C     F2DATE        OREQ      SAVEDT
     C     F2TIME        ANDGT     SAVETM
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
     C*** BEGIN ADD *** 01/02/01 **************************************
     C   99              MOVEL(P)  MODEA         SCMODE
     C*** END   ADD *** 01/02/01 **************************************
     C*
     C                   MOVE      F25N01        SC5N01
     C                   MOVE      F25N02        SC5N02
     C                   MOVE      F29N01        SC9N01
     C*** BEG CHG *** 01/30/01 ****************************************
     C***********          MOVE F29N02    SC9N02
     C                   MOVE      F29N02        DS9N02
     C                   MOVE      DS9N2A        SC9N2A
     C                   MOVE      DS9N2B        SC9N2B
     C*** END CHG *** 01/30/01 ****************************************
     C                   MOVE      F29N03        SC9N03
     C*** BEG CHG *** 01/30/01 ****************************************
     C***********          MOVE F29N04    SC9N04
     C                   MOVE      F29N04        DS9N04
     C                   MOVE      DS9N4A        SC9N4A
     C                   MOVE      DS9N4B        SC9N4B
     C*** END CHG *** 01/30/01 ****************************************
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
     C                   MOVE      SC5N01        F25N01
     C                   MOVE      SC5N02        F25N02
     C                   MOVE      SC9N01        F29N01
     C*** BEG CHG *** 01/30/01 ****************************************
     C***********          MOVE SC9N02    F29N02
     C                   MOVEL     SC9N2A        F29N02
     C                   MOVE      SC9N2B        F29N02
     C*** END CHG *** 01/30/01 ****************************************
     C                   MOVE      SC9N03        F29N03
     C*** BEG CHG *** 01/30/01 ****************************************
     C***********          MOVE SC9N04    F29N04
     C                   MOVEL     SC9N4A        F29N04
     C                   MOVE      SC9N4B        F29N04
     C*** END CHG *** 01/30/01 ****************************************
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
     C*** BEG ADD *** 07/26/05 ****************************************
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC
     C                   PARM      *BLANK        ##OS              1            OUTCOME/SAVINGS
16   C                   PARM                    ##ARLS            1            Auto Release Flag
17   C                   PARM                    ##CCOD            1            Correct Coding Flag
18   C                   PARM                    ##CRVW            1            Code Review Flag
19   C                   PARM                    ##PROM            1            ProMatch Flag
20   C                   PARM                    ##MCRV            1            Medicare CR Flag
21   C                   PARM                    ##FREQ            1            Frequency Flag
22   C                   PARM                    ##CRAC            1            CR Auto Complete
23   C                   PARM                    ##PREO            1            CR Auto Complete
24   C                   PARM                    ##PARN            1            Par/Non-Par/Both
25   C                   PARM                    ##MGRI            1            Multi Group Restr
26   C                   PARM                    ##KCIF            1            Keep Claimant info
27   C                   PARM                    ##CURS            1            Cursor Positioning
28   C                   PARM                    ##PCIB            1            PCIB
29   C                   PARM                    ##CLT#            3            Client number
     C                   MOVE      ##CLT#        XXCLT#            3 0
     C*
     C* DISPLAY REQUIRED FIELDS IN HI INTENSITY OR NOT
     C*
     C     XXCLT#        IFEQ      135
     C                   SETON                                        21
     C                   ELSE
     C                   SETOFF                                       21
     C                   ENDIF
     C*** END ADD *** 07/26/05 ****************************************
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
