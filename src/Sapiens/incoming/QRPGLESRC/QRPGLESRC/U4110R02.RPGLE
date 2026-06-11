     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  U4110R02 - PEND LETTER INFORMATION             C:62 02/19/99 *
     H*                                                               *
     H*  CALLED FROM: U4110R01                                        *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  ERROR INDICATORS IN ERRCK1: 71 - 74.                         *
     H*                                                               *
     H*****************************************************************
     FU4110F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FUXLCP1    UF A E             DISK
     F                                     RENAME(PLC01:DBRCDF)
     F                                     INFDS(DBFDS)
     FUXLCL     UF   E           K DISK
     FSMDOP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pend Letter Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          UXLCP1
     C                   PARM                    @@RRN2            9 0          UMPCP
     C                   PARM                    @@RRN3            9 0          SMDOP
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
     C* Network code search parm list.
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     LCKY          KLIST
     C                   KFLD                    LCCO#
     C                   KFLD                    LCSUB#
     C                   KFLD                    LCFND
     C                   KFLD                    LCFYR
     C                   KFLD                    LCCASE
     C                   KFLD                    LCCERT
     C                   KFLD                    LCEXT
     C                   KFLD                    LCREQ#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
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
     C* FLAG 1 MUST BE 'X' OR BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SC1A1         ANDNE     *BLANKS
     C     SC1A1         ANDNE     'X'
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C* FLAG 2 MUST BE 'X' OR BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SC1A2         ANDNE     *BLANKS
     C     SC1A2         ANDNE     'X'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* FLAG 3 MUST BE 'X' OR BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SC1A3         ANDNE     *BLANKS
     C     SC1A3         ANDNE     'X'
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* SC60A1 MUST EQUAL *BLANKS IF FLAG 3 IS BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SC1A3         ANDNE     *BLANKS
     C     SC60A1        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
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
     C* SCR01 VARIABLES
     C*
     C                   MOVEL     *BLANKS       SC1A1
     C                   MOVEL     *BLANKS       SC1A2
     C                   MOVEL     *BLANKS       SC1A3
     C                   MOVEL     *BLANKS       SC60A1
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* SCR01 VARIABLES
     C*
     C                   MOVEL     SC1A1         LC1A1
     C                   MOVEL     SC1A2         LC1A2
     C                   MOVEL     SC1A3         LC1A3
     C                   MOVEL     SC60A1        LC60A1
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
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
     C*** GET SYSTEM INFO
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
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C                   MOVE      LCCO#         #1CO#
     C                   MOVE      LCSUB#        #1SUB#
     C                   MOVE      LCFND         #1FND
     C                   MOVE      LCFYR         #1FYR
     C                   MOVE      LCCASE        #1CASE
     C*
     C* TWO LINE CLAIM HEADER
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C     SCHDL1        PARM      *BLANK        ##L1             78
     C     SCHDL2        PARM      *BLANK        ##L2             78
     C*
     C* ASSIGN DOCUMENT ID AND FOLDER
     C*
     C                   MOVEL(P)  LCDCID        SCDCID
     C                   MOVEL(P)  LCFLDR        SCFLDR
     C*
     C* GET DEPARTMENT DOCUMENT RECORD.
     C*
     C     @@RRN3        CHAIN     PDO01                              99
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
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C     LCKY          CHAIN     PLC01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        LCUSER
     C                   MOVEL     Q1PGM         LCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LCDT
     C*
     C                   MOVEL     DSTIME        LCTIME
     C*
     C                   UPDATE    PLC01
     C*
     C                   ENDIF                                                  *IN99 EQ *OFF
     C*
E01  C                   ENDIF                                                  *IN99 EQ *OFF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      UXLCP1
     C*
     C                   ENDSR
