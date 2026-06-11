     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 01/22/03  C:142 SEQ. 35604                                    *
     H*                 CORRECTED SEARCHES                            *
     H*                                                               *
     H* 09/11/02  C:142 PROJECT WC1077                                *
     H*                 ADDED LOOKUP FOR CATEGORY                     *
     H*                                                               *
     H* 11/24/97  C:62  PROJECT WC768                                 *
     H*                 ADDED API INTERFACE TO BKCAL1.                *
     H*                                                               *
     H*****************************************************************
     FW2136F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTRTP     UF A E             DISK
     F                                     RENAME(PRT01:DBRCDF)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SPC01)
     FWTGTL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Reserve Type Master-
     D                                      Information')
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
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEGIN ADD *** 11/24/97 **************************************
     C*
     C*  AP001R PARM LIST
     C*
     C     AP01PL        PLIST
     C                   PARM                    ##CO##
     C                   PARM                    ##FAC#
     C                   PARM                    ##DPT#
     C                   PARM                    ##ACC#
     C                   PARM                    ##SUB#
     C                   PARM                    ##RTC             1
     C*** END   ADD *** 11/24/97 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     RTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##SKY
     C*
     C     GLKY          KLIST
     C                   KFLD                    ##GKY
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    H4                4
     C                   KFLD                    H10              10
     C*
     C                   MOVE      'RECT'        H4
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    RTKEY         ##KEY
     C     *LIKE         DEFINE    GLKEY         ##GKY
     C     *LIKE         DEFINE    SYKEY         ##SKY
     C     *LIKE         DEFINE    RTDCO         ##CO#
     C     *LIKE         DEFINE    RTDCO         ##CO##
     C     *LIKE         DEFINE    RTDFAC        ##FAC#
     C     *LIKE         DEFINE    RTDDPT        ##DPT#
     C     *LIKE         DEFINE    RTDACC        ##ACC#
     C     *LIKE         DEFINE    RTDSUB        ##SUB#
     C     *LIKE         DEFINE    RTDCO         @@CO#
     C     *LIKE         DEFINE    RTDFAC        @@FAC#
     C     *LIKE         DEFINE    RTDDPT        @@DPT#
     C     *LIKE         DEFINE    RTDACC        @@ACC#
     C     *LIKE         DEFINE    RTDSUB        @@SUB#
     C*** BEGIN ADD *** 09/11/02 **************************************
     C     *LIKE         DEFINE    SCPRDS        QQPRDS
     C*** END   ADD *** 09/11/02 **************************************
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
     C     RTSTAT        IFEQ      'I'
     C                   MOVE      'A'           RTSTAT
     C                   ELSE
     C                   MOVE      'I'           RTSTAT
     C                   ENDIF                                                  RTSTAT
     C*
     C                   MOVEL     Q1USER        RTUSER
     C                   MOVEL     Q1PGM         RTPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RTDT
     C*
     C                   MOVEL     DSTIME        RTTIME
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
     C* RESERVE TYPE CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE RESERVE TYPE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90RTKY          CHAIN     PRT01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* RESERVE TYPE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C*  THE RESERVE TYPE RECORD TYPE MUST BE M, C, L, E.
     C*
     C  N90SCTYPE        IFNE      'M'
     C     SCTYPE        ANDNE     'C'
     C     SCTYPE        ANDNE     'L'
     C     SCTYPE        ANDNE     'E'
     C*
     C                   SETON                                        74  90
     C*
     C                   ENDIF                                                  SCTYPE
     C*
     C*  THE G/L DEBIT ACCOUNT MUST BE VALID
     C*
     C  N90*IN16         IFEQ      '1'
     C*
     C                   MOVE      SCDCO         ##CO##
     C                   MOVE      SCDFAC        ##FAC#
     C                   MOVE      SCDDPT        ##DPT#
     C                   MOVE      SCDACC        ##ACC#
     C                   MOVE      SCDSUB        ##SUB#
     C*
     C*** BEGIN CHG *** 11/24/97 **************************************
     C  N90              CALL      'AP001R'      AP01PL
     C*
     C  N90##RTC         IFEQ      '1'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF                                                  ##RTC
     C*** END   CHG *** 11/24/97 **************************************
     C*
     C                   ENDIF                                                  DEBIT ACCT
     C*
     C*  THE G/L CREDIT ACCOUNT MUST BE VALID
     C*
     C  N90*IN16         IFEQ      '1'
     C*
     C                   MOVE      SCCCO         ##CO##
     C                   MOVE      SCCFAC        ##FAC#
     C                   MOVE      SCCDPT        ##DPT#
     C                   MOVE      SCCACC        ##ACC#
     C                   MOVE      SCCSUB        ##SUB#
     C*
     C*** BEGIN CHG *** 11/24/97 **************************************
     C  N90              CALL      'AP001R'      AP01PL
     C*
     C  N90##RTC         IFEQ      '1'
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF                                                  ##RTC
     C*** END   CHG *** 11/24/97 **************************************
     C*
     C                   ENDIF                                                  CREDIT ACCT
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90RTDT          IFGT      SAVEDT
     C     RTDT          OREQ      SAVEDT
     C     RTTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* VALIDATE RESERVE CATEGOTY
     C*
     C  N90SCCTGY        IFNE      *BLANK
     C                   MOVE      *BLANKS       H10
     C                   MOVEL     SCCTGY        H10
     C     GTKY          CHAIN     WTGTL                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9070
     C                   ENDIF
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     RTKEY         SCKEY
     C                   MOVEL     RTDESC        SCDESC
     C                   MOVEL     RTTYPE        SCTYPE
     C*** BEGIN ADD *** 09/11/02 **************************************
     C                   MOVEL     RTCTGY        SCCTGY
     C*** END   ADD *** 09/11/02 **************************************
     C                   MOVEL     RTSTAT        SCSTAT
     C                   MOVEL     RTDCO         SCDCO
     C                   MOVEL     RTDFAC        SCDFAC
     C                   MOVEL     RTDDPT        SCDDPT
     C                   MOVEL     RTDACC        SCDACC
     C                   MOVEL     RTDSUB        SCDSUB
     C                   MOVEL     RTCCO         SCCCO
     C                   MOVEL     RTCFAC        SCCFAC
     C                   MOVEL     RTCDPT        SCCDPT
     C                   MOVEL     RTCACC        SCCACC
     C                   MOVEL     RTCSUB        SCCSUB
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCTYPE
     C*** BEGIN ADD *** 09/11/02 **************************************
     C                   MOVEL     *BLANKS       SCCTGY
     C*** END   ADD *** 09/11/02 **************************************
     C                   MOVEL     'A'           SCSTAT
     C                   MOVE      *ZERO         SCDCO
     C                   MOVE      *ZERO         SCDFAC
     C                   MOVE      *ZERO         SCDDPT
     C                   MOVE      *ZERO         SCDACC
     C                   MOVE      *ZERO         SCDSUB
     C                   MOVE      *ZERO         SCCCO
     C                   MOVE      *ZERO         SCCFAC
     C                   MOVE      *ZERO         SCCDPT
     C                   MOVE      *ZERO         SCCACC
     C                   MOVE      *ZERO         SCCSUB
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         RTCO#
     C                   MOVEL     SCKEY         RTKEY
     C                   MOVEL     SCDESC        RTDESC
     C                   MOVEL     SCTYPE        RTTYPE
     C*** BEGIN ADD *** 09/11/02 **************************************
     C                   MOVEL     SCCTGY        RTCTGY
     C*** END   ADD *** 09/11/02 **************************************
     C                   MOVEL     SCSTAT        RTSTAT
     C                   MOVEL     SCDCO         RTDCO
     C                   MOVEL     SCDFAC        RTDFAC
     C                   MOVEL     SCDDPT        RTDDPT
     C                   MOVEL     SCDACC        RTDACC
     C                   MOVEL     SCDSUB        RTDSUB
     C                   MOVEL     SCCCO         RTCCO
     C                   MOVEL     SCCFAC        RTCFAC
     C                   MOVEL     SCCDPT        RTCDPT
     C                   MOVEL     SCCACC        RTCACC
     C                   MOVEL     SCCSUB        RTCSUB
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
     C     *INKJ         IFEQ      *ON
     C*** BEGIN DEL *** 01/22/03 ************************************* *
     C*** BEGIN ADD *** 09/11/02 **************************************
     C*          HFLD      ANDNE'SCCTGY'
     C*** END   ADD *** 09/11/02 **************************************
     C*** END   DEL *** 01/22/03 ************************************* *
     C*** BEGIN ADD *** 01/22/03 ************************************* *
     C     HFLD          IFEQ      'SCDCO'
     C     HFLD          OREQ      'SCDFAC'
     C     HFLD          OREQ      'SCDDPT'
     C     HFLD          OREQ      'SCDACC'
     C     HFLD          OREQ      'SCDSUB'
     C     HFLD          OREQ      'SCCCO'
     C     HFLD          OREQ      'SCCFAC'
     C     HFLD          OREQ      'SCCDPT'
     C     HFLD          OREQ      'SCCACC'
     C     HFLD          OREQ      'SCCSUB'
     C*** END   ADD *** 01/22/03 ************************************* *
     C*
     C                   MOVE      *ZEROS        @@CO#
     C                   MOVE      *ZEROS        @@FAC#
     C                   MOVE      *ZEROS        @@DPT#
     C                   MOVE      *ZEROS        @@ACC#
     C                   MOVE      *ZEROS        @@SUB#
     C                   MOVE      *BLANKS       @@TYPE            1
     C                   MOVE      *BLANKS       @1TYPE            1
     C                   MOVE      *BLANKS       @@RTC             1
     C*
     C                   CALL      'BKD010R'
     C                   PARM                    @@TYPE
     C                   PARM                    @@CO#
     C                   PARM                    @@FAC#
     C                   PARM                    @@DPT#
     C                   PARM                    @@ACC#
     C                   PARM                    @@SUB#
     C                   PARM                    @1TYPE
     C                   PARM                    @@RTC
     C*
     C     HFLD          IFEQ      'SCDCO'
     C     HFLD          OREQ      'SCDFAC'
     C     HFLD          OREQ      'SCDDPT'
     C     HFLD          OREQ      'SCDACC'
     C     HFLD          OREQ      'SCDSUB'
     C*
     C     @@CO#         IFNE      *ZEROS
     C*
     C                   Z-ADD     @@CO#         SCDCO
     C                   Z-ADD     @@FAC#        SCDFAC
     C                   Z-ADD     @@DPT#        SCDDPT
     C                   Z-ADD     @@ACC#        SCDACC
     C                   Z-ADD     @@SUB#        SCDSUB
     C*
     C                   MOVE      *ON           *IN81
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN80
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCCCO'
     C     HFLD          OREQ      'SCCFAC'
     C     HFLD          OREQ      'SCCDPT'
     C     HFLD          OREQ      'SCCACC'
     C     HFLD          OREQ      'SCCSUB'
     C*
     C     @@CO#         IFNE      *ZEROS
     C*
     C                   MOVE      @@CO#         SCCCO
     C                   MOVE      @@FAC#        SCCFAC
     C                   MOVE      @@DPT#        SCCDPT
     C                   MOVE      @@ACC#        SCCACC
     C                   MOVE      @@SUB#        SCCSUB
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN81
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/11/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/11/02 **************************************
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/11/02 **************************************
      *      RESERVE CATEGORY CODE
      *
     C     *INKJ         IFEQ      *ON
     C     HFLD          ANDEQ     'SCCTGY'
      *
     C                   MOVEL(P)  'RECT'        XXTBID            4
     C                   MOVE(P)   *BLANK        XXKEY            10
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD            1
     C                   MOVEL     XXKEY         SCCTGY
     C*** BEGIN ADD *** 01/22/03 ************************************* *
     C                   MOVE      *ON           *IN90
     C*** END   ADD *** 01/22/03 ************************************* *
     C                   ENDIF
     C*** END   ADD *** 09/11/02 **************************************
     C*
     C*                    MOVE *ON       *IN90
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
     C* CHECK THAT G/L IS INSTALLED, OR IGNORE G/L INFO
     C*
     C                   MOVEL     'SY'          ##SKY
     C*
     C     SYKY          CHAIN     SSY01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     SYFL01        IFNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN16
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  SYFL01
     C*
     C* CHECK G/L FLAG TO SEE IF FUND WILL BE USED AS CO#
     C*
     C                   MOVEL     'GL'          ##GKY
     C*
     C     GLKY          CHAIN     SGL01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     GLFL03        IFNE      *BLANKS
     C*
     C                   SETON                                            17    90
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  GLFL03
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
     C                   MOVEL     Q1USER        RTUSER
     C                   MOVEL     Q1PGM         RTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RTDT
     C*
     C                   MOVEL     DSTIME        RTTIME
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
     C                   MOVEL     Q1USER        RTUSER
     C                   MOVEL     Q1PGM         RTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RTDT
     C*
     C                   MOVEL     DSTIME        RTTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTRTP
     C*
     C                   ENDSR
