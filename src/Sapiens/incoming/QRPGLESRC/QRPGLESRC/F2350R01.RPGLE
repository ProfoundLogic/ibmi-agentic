     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/25/18 C:180  PROJECT:114MJS                               *
     H*                  Recompile only.                              *
     H*                                                               *
     H*  02/25/02 C:62 PROJECT:114MBH                                 *
     H*                -ADD PROCESSING FOR PRODUCT QUANTITY.          *
     H*                -ADD SEARCH FOR PRODUCT QUANTITY.              *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                CHANGE OPTNM.                                  *
     H*                                                               *
     H*  08/26/99 C:62 Proj: 25MMG                                    *
     H*                CHANGE FIELDS FOR STRENGTH AND SIZE.           *
     H*                                                               *
     H*****************************************************************
     FF2350F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMRXP     UF A E             DISK
     F                                     RENAME(PRX01:DBRCDF)
     FFMRXL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'NDC Drug Code Maint- C         OPTNM
     D**********    'enance'
     D OPTNM           C                   CONST('NDC Drug Code Infor-
     D                                     mation')
     D*** END   CHG *** 05/05/00 **************************************
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
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     RXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNDC
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKGKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    RXCO#         ##CO#
     C     *LIKE         DEFINE    RXNDC         KKNDC
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKGKEY
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
     C     RXSTAT        IFEQ      'I'
     C                   MOVE      'A'           RXSTAT
     C                   ELSE
     C                   MOVE      'I'           RXSTAT
     C                   ENDIF                                                  RXSTAT
     C*
     C                   MOVEL     Q1USER        RXUSER
     C                   MOVEL     Q1PGM         RXPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXDT
     C*
     C                   MOVEL     DSTIME        RXTIME
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
     C* NDC DRUG CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCNDC         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE NDC DRUG CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCNDC         KKNDC
     C*
     C   40
     CANN90RXKY          CHAIN     PRX01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* NDC DRUG CODE NAME CANNOT BE BLANK
     C*
     C  N90SCNAM1        IFEQ      *BLANKS
     C     SCNAM2        ANDEQ     *BLANKS
     C                   SETON                                        73  90
     C*
     C                   ENDIF                                                  SCNAME
     C*
     C* AWP CURRENT UNIT PRICE CAN NOT BE ZERO.
     C*
     C  N90SCAWP         IFEQ      *ZEROS
     C*
     C                   SETON                                        80  90
     C*
     C                   ENDIF                                                  SCAWP
     C*
     C*  MUST HAVE VALID AWP CURRENT EFFECTIVE DATE
     C*
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCAWPD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAWPD
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF                                                  SRERR=1
     C*
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C*  MUST HAVE VALID FORM CODE IF NOT EQUAL BLANK
     C*
     C  N90SCFORM        IFNE      *BLANKS
     C*
     C                   MOVEL     'FORM'        ##TBID
     C                   MOVEL(P)  SCFORM        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7590
     C*
     C                   ENDIF                                                  SCFORM
     C*
     C*  MUST HAVE VALID MEASURE CODE
     C*
     C     *IN90         IFNE      *ON
     C*
     C                   MOVEL     'MEAS'        ##TBID
     C                   MOVEL(P)  SCMEAS        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7690
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C*** BEGIN ADD *** 02/25/02 **************************************
     C     *IN90         IFEQ      *OFF
     C     SCSTRN        ANDNE     *BLANKS
     C*
     C                   MOVEL     'PQTY'        ##TBID
     C                   MOVEL(P)  SCSTRN        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSTRD
     C                   ELSE
     C                   SETON                                        8790
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 02/25/02 **************************************
     C*
     C*  MUST HAVE VALID UNIT DOES INDICATOR
     C*
     C  N90SCUNTD        IFNE      *BLANKS
     C*
     C                   MOVEL     'DOSE'        ##TBID
     C                   MOVEL(P)  SCUNTD        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7790
     C*
     C                   ENDIF                                                  SCUNTD NE
     C*
     C*  MUST HAVE VALID PRODUCT TYPE
     C*
     C     *IN90         IFNE      *ON
     C*
     C                   MOVEL     'PROD'        ##TBID
     C                   MOVEL(P)  SCPTYP        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7890
     C*
     C                   ENDIF                                                  *IN90  NE
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90RXDT          IFGT      SAVEDT
     C     RXDT          OREQ      SAVEDT
     C     RXTIME        ANDGT     SAVETM
     C*
     C                   SETON                                        7990
     C*
     C                   ENDIF                                                  RXDT=SAVEDT
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
     C* KJDO - F10=SEARCHES
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* FORM CODE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCFORM'
     C*
     C                   MOVEL     'FORM'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        81
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCFORM
     C                   MOVEL     GTDESC        SCFORD
     C                   SETON                                        82
     C*
     C                   ENDIF
     C*
     C* MEASURE CODE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCMEAS'
     C*
     C                   MOVEL     'MEAS'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        83
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCMEAS
     C                   MOVEL     GTDESC        SCMEAD
     C*** BEGIN CHG *** 02/25/02 **************************************
     C**********           SETON                     84
     C                   SETON                                        31
     C*** END   CHG *** 02/25/02 **************************************
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 02/25/02 **************************************
     C*
     C* PRODUCT QUANTITY CODE
     C*
     C     HFLD          WHENEQ    'SCSTRN'
     C                   MOVEL     'PQTY'        ##TBID
     C                   CALL      'F2380R'      ACDPL
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        31
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCSTRN
     C                   MOVEL     GTDESC        SCSTRD
     C                   SETON                                        84
     C                   ENDIF
     C*** END   ADD *** 02/25/02 **************************************
     C*
     C* UNIT DOSE INDICATOR SEARCH
     C*
     C     HFLD          WHENEQ    'SCUNTD'
     C*
     C                   MOVEL     'DOSE'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        85
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCUNTD
     C                   MOVEL     GTDESC        SCUNID
     C                   SETON                                        86
     C*
     C                   ENDIF
     C*
     C* PRODUCT TYPE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCPTYP'
     C*
     C                   MOVEL     'PROD'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        86
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCPTYP
     C                   MOVEL     GTDESC        SCPROD
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
     C                   MOVEL     RXNDC         SCNDC
     C*** BEGIN ADD *** 08/26/99 **************************************
     C                   MOVEL     RXONDC        SCONDC
     C*** END   ADD *** 08/26/99 **************************************
     C*** BEGIN ADD *** 02/25/02 **************************************
     C                   MOVEL     RXSTRN        SCSTRN
     C*
     C                   MOVEL     'PQTY'        ##TBID
     C                   MOVEL(P)  RXSTRN        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSTRD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCSTRD
     C                   ENDIF
     C*** END   ADD *** 02/25/02 **************************************
     C                   MOVEL     RXNAME        SCNAM1
     C                   MOVE      RXNAME        SCNAM2
     C                   MOVE      RXAWP         SCAWP
     C                   MOVE      RX1AWP        SC1AWP
     C*
     C                   Z-ADD     RXAWPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAWPD
     C*
     C                   MOVEL     RXFORM        SCFORM
     C*
     C                   MOVEL     'FORM'        ##TBID
     C                   MOVEL(P)  RXFORM        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCFORD
     C   99              MOVEL     *BLANKS       SCFORD
     C*
     C*** BEGIN CHG *** 08/26/99 **************************************
     C**********           MOVELRXSTRN    SCSTRN
     C**********           MOVE RXTOTU    SCTOTU
     C                   MOVEL(P)  RXSTRG        SCSTRG
     C                   MOVEL(P)  RXMSZE        SCMSZE
     C*** END   CHG *** 08/26/99 **************************************
     C                   MOVEL     RXMEAS        SCMEAS
     C*
     C                   MOVEL     'MEAS'        ##TBID
     C                   MOVEL(P)  RXMEAS        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCMEAD
     C   99              MOVEL     *BLANKS       SCMEAD
     C*
     C                   MOVE      RXPSZE        SCPSZE
     C                   MOVEL     RXUNTD        SCUNTD
     C*
     C                   MOVEL     'DOSE'        ##TBID
     C                   MOVEL(P)  RXUNTD        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCUNID
     C   99              MOVEL     *BLANKS       SCUNID
     C*
     C                   MOVEL     RXPTYP        SCPTYP
     C*
     C                   MOVEL     'PROD'        ##TBID
     C                   MOVEL(P)  RXPTYP        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCPROD
     C   99              MOVEL     *BLANKS       SCPROD
     C*
     C                   MOVEL     RXSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCNDC
     C*** BEGIN ADD *** 08/26/99 **************************************
     C                   MOVEL     *BLANKS       SCONDC
     C*** END   ADD *** 08/26/99 **************************************
     C*** BEGIN ADD *** 02/25/02 **************************************
     C                   MOVEL     *BLANKS       SCSTRN
     C*** END   ADD *** 02/25/02 **************************************
     C                   MOVEL     *BLANKS       SCNAM1
     C                   MOVE      *BLANKS       SCNAM2
     C                   MOVE      *ZEROS        SCAWP
     C                   MOVE      *ZEROS        SC1AWP
     C                   MOVE      *ZEROS        SCAWPD
     C                   MOVEL     *BLANKS       SCFORM
     C                   MOVEL     *BLANKS       SCFORD
     C*** BEGIN CHG *** 08/26/99 **************************************
     C**********           MOVEL*BLANKS   SCSTRN
     C**********           MOVE *ZEROS    SCTOTU
     C                   MOVEL     *BLANKS       SCSTRG
     C                   MOVEL     *BLANKS       SCMSZE
     C*** END   CHG *** 08/26/99 **************************************
     C                   MOVEL     *BLANKS       SCMEAS
     C                   MOVEL     *BLANKS       SCMEAD
     C                   MOVE      *ZEROS        SCPSZE
     C                   MOVEL     *BLANKS       SCUNTD
     C                   MOVEL     *BLANKS       SCUNID
     C                   MOVEL     *BLANKS       SCPTYP
     C                   MOVEL     *BLANKS       SCPROD
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
     C                   Z-ADD     ##CO#         RXCO#
     C                   MOVEL     SCNDC         RXNDC
     C*** BEGIN ADD *** 08/26/99 **************************************
     C                   MOVEL     SCONDC        RXONDC
     C*** END   ADD *** 08/26/99 **************************************
     C*** BEGIN ADD *** 02/25/02 **************************************
     C                   MOVEL(P)  SCSTRN        RXSTRN
     C*** END   ADD *** 02/25/02 **************************************
     C                   MOVEL     SCNAM1        RXNAME
     C                   MOVE      SCNAM2        RXNAME
     C                   MOVE      SCAWP         RXAWP
     C                   MOVE      SC1AWP        RX1AWP
     C*
     C                   Z-ADD     SCAWPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXAWPD
     C*
     C                   MOVEL     SCFORM        RXFORM
     C*** BEGIN CHG *** 08/26/99 **************************************
     C**********           MOVELSCSTRN    RXSTRN
     C**********           MOVE SCTOTU    RXTOTU
     C                   MOVEL(P)  SCSTRG        RXSTRG
     C                   MOVEL(P)  SCMSZE        RXMSZE
     C*** END   CHG *** 08/26/99 **************************************
     C                   MOVEL     SCMEAS        RXMEAS
     C                   MOVE      SCPSZE        RXPSZE
     C                   MOVEL     SCUNTD        RXUNTD
     C                   MOVEL     SCPTYP        RXPTYP
     C                   MOVEL     SCSTAT        RXSTAT
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
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
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
     C                   MOVEL     Q1USER        RXUSER
     C                   MOVEL     Q1PGM         RXPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXDT
     C*
     C                   MOVEL     DSTIME        RXTIME
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
     C                   MOVEL     Q1USER        RXUSER
     C                   MOVEL     Q1PGM         RXPGM
     C                   MOVEL     Q1USER        RXIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXDT
     C*
     C                   MOVEL     SRDTO         RXIDT
     C*
     C                   MOVEL     DSTIME        RXTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMRXP
     C*
     C                   ENDSR
