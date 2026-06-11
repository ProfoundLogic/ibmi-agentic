     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*******************************************************************
     H/TITLE F2300R01 - ADJUDICATION CODE INFORMATION
     H*******************************************************************
     H*  Revisions:                                                   * *
     H*                                                               * *
     H*  12/03/02 C:79 PROJECT:FSMNT                                  * *
     H*                GIVE ERROR ON NON-SEARCHABLE FIELDS            * *
     H*                                                               * *
     H*  09/27/01 C:62 PROJECT:FSMNT                                  * *
     H*                COMPILE ONLY                                   * *
     H*                                                               * *
     H*  08/15/00 C:62  PROJECT: MC215                                * *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               * *
     H*******************************************************************
     H*  Indicators:                                                  * *
     H*  30 - These indicators are to used as error indicators for    *
     H* thru  invalid searches                                        *
     H*  33 -                                                         *
     H*                                                               * *
     H*******************************************************************
     FF2300F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTCSP     UF A E             DISK
     F                                     RENAME(PCS01:DBRCDF)
     FFTABL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F**********  SCS01                             KIGNORE
     F**********  SRC01                             KIGNORE
     F*** END   DEL *** 08/15/00 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Adjudication Code-
     D                                      Information')
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
     C     CSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           KKKEY
     C*** END   DEL *** 08/15/00 **************************************
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
     C     *LIKE         DEFINE    CSCO#         ##CO#
     C     *LIKE         DEFINE    CSKEY         ##KEY
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** *LIKE     DEFN FBKEY     KKKEY
     C*** END   DEL *** 08/15/00 **************************************
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
     C     CSSTAT        IFEQ      'I'
     C                   MOVE      'A'           CSSTAT
     C                   ELSE
     C                   MOVE      'I'           CSSTAT
     C                   ENDIF                                                  CSSTAT
     C*
     C                   MOVEL     Q1USER        CSUSER
     C                   MOVEL     Q1PGM         CSPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSDT
     C*
     C                   MOVEL     DSTIME        CSTIME
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
     C* CLAIM STATUS CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE CLAIM STATUS CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90CSKY          CHAIN     PCS01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* CLAIM STATUS CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C*  THE DISPOSITION CODE MUST AN 'N', 'C' OR 'P' OR BLANKS
     C*
     C  N90SCDISC        IFNE      *BLANKS
     C*
     C                   MOVEL     'ARTY'        ##TBID
     C                   MOVEL(P)  SCDISC        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        74  90
     C*
     C                   ENDIF
     C*
     C*  THE OVERRIDE FIELD MUST BE Y OR N.
     C*
     C  N90SCOVRD        IFNE      'N'
     C     SCOVRD        ANDNE     'Y'
     C*
     C                   SETON                                        75  90
     C*
     C                   ENDIF
     C*
     C*  OVERRIDE FLAG MUST BE Y, IN ORDER TO INDICATE AN OVERRIDE PERCENTAGE
     C*
     C  N90SCOVRD        IFNE      'Y'
     C     SCOVRP        ANDNE     *ZEROS
     C*
     C                   SETON                                        76  90
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90SPDT          IFGT      SAVEDT
     C     SPDT          OREQ      SAVEDT
     C     SPTIME        ANDGT     SAVETM
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
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** BEG ADD *** 12/03/02 ****************************************
     C                   SETOFF                                       303132
     C                   SETOFF                                       33
     C*** END ADD *** 12/03/02 ****************************************
     C*
     C                   SELECT
     C*
     C* SEARCH DISPOSITION CODE.
     C*
     C     HFLD          WHENEQ    'SCDISC'
     C*
     C                   MOVEL     'DISP'        ##TBID
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
     C                   MOVEL     GTKEY         SCDISC
     C                   SETON                                        82
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 12/03/02 ****************************************
     C*
     C* NON SEARCHABLE FIELDS
     C*
     C     HFLD          WHENEQ    'SCKEY'
     C*
     C                   SETON                                        30
     C*
     C     HFLD          WHENEQ    'SCDESC'
     C*
     C                   SETON                                        31
     C*
     C     HFLD          WHENEQ    'SCOVRD'
     C*
     C                   SETON                                        32
     C*
     C     HFLD          WHENEQ    'SCOVRP'
     C*
     C                   SETON                                        33
     C*** END ADD *** 12/03/02 ****************************************
     C*
     C                   OTHER
     C                   WRITE     SCR01
     C*** BEG DEL *** 12/03/02 ****************************************
     C******************** SETON                     89
     C*** END DEL *** 12/03/02 ****************************************
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
     C*
     C                   MOVEL     CSKEY         SCKEY
     C                   MOVEL     CSDESC        SCDESC
     C                   MOVEL     CSDISC        SCDISC
     C                   MOVE      CSDIDY        SCDIDY
     C                   MOVEL     CSOVRD        SCOVRD
     C                   MOVE      CSOVRP        SCOVRP
     C                   MOVEL     CSSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCDISC
     C                   MOVE      *ZEROS        SCDIDY
     C                   MOVEL     *BLANKS       SCOVRD
     C                   MOVE      *ZEROS        SCOVRP
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
     C                   Z-ADD     ##CO#         CSCO#
     C                   MOVEL     SCKEY         CSKEY
     C                   MOVEL     SCDESC        CSDESC
     C                   MOVEL     SCDISC        CSDISC
     C                   MOVE      SCDIDY        CSDIDY
     C                   MOVEL     SCOVRD        CSOVRD
     C                   MOVE      SCOVRP        CSOVRP
     C                   MOVEL     SCSTAT        CSSTAT
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
     C                   SETOFF                                       818289
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
     C                   SETOFF                                       16
     C*
     C*** BEG ADD *** 12/03/02 ****************************************
     C                   SETOFF                                       303132
     C                   SETOFF                                       33
     C*** END ADD *** 12/03/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C                   PARM      *BLANKS       ##ST01            2            STATE RULE 01
     C                   PARM      *BLANKS       ##ST02            2            STATE RULE 02
     C                   PARM      *BLANKS       ##ST03            2            STATE RULE 03
     C                   PARM      *BLANKS       ##ST04            2            STATE RULE 04
     C                   PARM      *BLANKS       ##ST05            2            STATE RULE 05
     C                   PARM      *BLANKS       ##ST06            2            STATE RULE 06
     C                   PARM      *BLANKS       ##ST07            2            STATE RULE 07
     C                   PARM      *BLANKS       ##ST08            2            STATE RULE 08
     C                   PARM      *BLANKS       ##ST09            2            STATE RULE 09
     C                   PARM      *BLANKS       ##ST10            2            STATE RULE 10
     C                   PARM      *BLANKS       ##DIFI            1            DIARY FUNCTION INST
     C                   PARM      *BLANKS       ##APRM            1            PROVIDER MAINT
     C                   PARM      *BLANKS       ##CLED            1            CLAIM EDIT INST
     C                   PARM      *BLANKS       ##DMEI            1            DME INSTALLED
     C                   PARM      *BLANKS       ##DLOS            1            DEFAULT LOS RGN
     C                   PARM      *BLANKS       ##PHPR            1            PHARMACY PROCESSING
     C*** END   ADD *** 08/15/00 **************************************
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
     C** CHECK FOR DIARY FUNCTION
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVEL'FB'      KKKEY     P
     C*
     C********** FBKY      CHAINSFB01                99
     C*
     C********** *IN99     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** FBFL02    IFEQ 'Y'
     C     ##DIFI        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C                   MOVE      '1'           *IN16
     C*
     C                   ELSE                                                   ELSE FBFL02 = 'Y'
     C*
     C                   MOVE      '0'           *IN16
     C*
     C                   ENDIF                                                  END FBFL02 = 'Y'
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           ENDIF                           END *IN60 SFB01
     C*** END   DEL *** 08/15/00 **************************************
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
     C                   MOVEL     Q1USER        CSUSER
     C                   MOVEL     Q1PGM         CSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSDT
     C*
     C                   MOVEL     DSTIME        CSTIME
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
     C                   MOVEL     Q1USER        CSUSER
     C                   MOVEL     Q1PGM         CSPGM
     C                   MOVEL     Q1USER        CSIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSDT
     C*
     C                   MOVEL     SRDTO         CSIDT
     C*
     C                   MOVEL     DSTIME        CSTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTCSP
     C*
     C                   ENDSR
