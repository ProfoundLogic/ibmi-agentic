     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H***************************************************************
     H*  REVISIONS
     H*
     H*  10/25/99 C:62 Proj:MC216
     H*                ADD MED/VOC AND CONDITION BASED UPON REHAB.
     H*
     H****************************************************************
     FN2315F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTATP     UF A E             DISK
     F                                     RENAME(PAT01:DBRCDF)
     FFTABL     IF   E           K DISK
     F*** BEGIN ADD *** 10/25/99 **************************************
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END   ADD *** 10/25/99 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('T/M Activity Code-
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
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C* Search parm list.
     C*
     C     SRCHPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*** END   ADD *** 10/25/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     ATKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C*** Key list for WCMC generic table file
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ATCO#         ##CO#
     C     *LIKE         DEFINE    ATKEY         ##KEY
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END   ADD *** 10/25/99 **************************************
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
     C     ATSTAT        IFEQ      'I'
     C                   MOVE      'A'           ATSTAT
     C                   ELSE
     C                   MOVE      'I'           ATSTAT
     C                   ENDIF                                                  ATSTAT
     C*
     C                   MOVEL     Q1USER        ATUSER
     C                   MOVEL     Q1PGM         ATPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ATDT
     C*
     C                   MOVEL     DSTIME        ATTIME
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
     C* T/M ACTIVITY CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE T/M ACTIVITY CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90ATKY          CHAIN     PAT01                              99
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
     C* P/N FLAG MUST BE P OR N.
     C*
     C  N90SCTYP2        IFNE      'P'
     C     SCTYP2        ANDNE     'N'
     C*
     C                   SETON                                        74  90
     C*
     C                   ENDIF                                                  SCTYP2 NE P
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C* THE MEDICAL/VOCATIONAL FLAG MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *ON
     C*
     C                   MOVEL(P)  'MVFL'        KKTBID
     C                   MOVEL(P)  SCTYP1        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   SETON                                        9075
     C                   MOVE(P)   *BLANKS       SCAT1D
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  GTDESC        SCAT1D
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90ATDT          IFGT      SAVEDT
     C     ATDT          OREQ      SAVEDT
     C     ATTIME        ANDGT     SAVETM
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
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* SEARCH MEDICAL/VOCATIONAL FIELD
     C*
     C     HFLD          IFEQ      'SCTYP1'
     C*
     C                   MOVEL     'MVFL'        ##TBID
     C*
     C                   CALL      'F2380R'      SRCHPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCTYP1
     C                   MOVEL(P)  GTDESC        SCAT1D
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 10/25/99 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     ATKEY         SCKEY
     C                   MOVEL     ATDESC        SCDESC
     C                   MOVEL     ATTYP2        SCTYP2
     C                   MOVEL     ATSTAT        SCSTAT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL(P)  ATTYP1        SCTYP1
     C*
     C                   MOVEL(P)  'MVFL'        KKTBID
     C                   MOVEL(P)  SCTYP1        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCAT1D
     C   99              MOVEL     *BLANKS       SCAT1D
     C*
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCTYP1
     C                   MOVEL     *BLANKS       SCAT1D
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCTYP2
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C                   MOVEL     *BLANKS       SCTYP1
     C                   MOVEL     *BLANKS       SCAT1D
     C*
     C*** END   ADD *** 10/25/99 **************************************
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
     C                   Z-ADD     ##CO#         ATCO#
     C                   MOVEL     SCKEY         ATKEY
     C                   MOVEL     SCDESC        ATDESC
     C                   MOVEL     SCTYP2        ATTYP2
     C                   MOVEL     SCSTAT        ATSTAT
     C*** BEGIN ADD *** 10/25/99 **************************************
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL(P)  SCTYP1        ATTYP1
     C*
     C                   ENDIF
     C*** END   ADD *** 10/25/99 **************************************
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
     C*** BEGIN ADD *** 10/25/99 **************************************
     C*
     C                   SETOFF                                       31
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*** END   ADD *** 10/25/99 **************************************
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
     C*** BEGIN ADD *** 10/25/99 **************************************
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*
     C* CHECK TO SEE IF REHAB IS INSTALLED AND IF SO SETON 16.
     C*
     C     ##RHAB        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 10/25/99 **************************************
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
     C                   MOVEL     Q1USER        ATUSER
     C                   MOVEL     Q1PGM         ATPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ATDT
     C*
     C                   MOVEL     DSTIME        ATTIME
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
     C                   MOVEL     Q1USER        ATUSER
     C                   MOVEL     Q1PGM         ATPGM
     C                   MOVEL     Q1USER        ATIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ATDT
     C*
     C                   MOVEL     SRDTO         ATIDT
     C*
     C                   MOVEL     DSTIME        ATTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTATP
     C*
     C                   ENDSR
