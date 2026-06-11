     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 07/10/03  C:15  Project: WC11353                              *
     H*                 Added security level and types.               *
     H*                                                               *
     H* 11/24/97  C:62  PROJECT WC768                                 *
     H*                 ADDED API INTERFACE TO BKCAL1.                *
     H*                                                               *
     H*****************************************************************
     FW2141F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTPCP     UF A E             DISK
     F                                     RENAME(PPC01:DBRCDF)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SPC01)
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
     D OPTNM           C                   CONST('Payment Type Master-
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
     C     PCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
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
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PCKEY         ##KEY
     C     *LIKE         DEFINE    GLKEY         ##GKY
     C     *LIKE         DEFINE    SYKEY         ##SKY
     C     *LIKE         DEFINE    PCDCO         ##CO#
     C     *LIKE         DEFINE    PCDCO         ##CO##
     C     *LIKE         DEFINE    PCDFAC        ##FAC#
     C     *LIKE         DEFINE    PCDDPT        ##DPT#
     C     *LIKE         DEFINE    PCDACC        ##ACC#
     C     *LIKE         DEFINE    PCDSUB        ##SUB#
     C     *LIKE         DEFINE    PCDCO         @@CO#
     C     *LIKE         DEFINE    PCDFAC        @@FAC#
     C     *LIKE         DEFINE    PCDDPT        @@DPT#
     C     *LIKE         DEFINE    PCDACC        @@ACC#
     C     *LIKE         DEFINE    PCDSUB        @@SUB#
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
     C     PCSTAT        IFEQ      'I'
     C                   MOVE      'A'           PCSTAT
     C                   ELSE
     C                   MOVE      'I'           PCSTAT
     C                   ENDIF                                                  PCSTAT
     C*
     C                   MOVEL     Q1USER        PCUSER
     C                   MOVEL     Q1PGM         RTPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C*
     C                   MOVEL     DSTIME        PCTIME
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
     C* PAYMENT TYPE CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE PAYMENT TYPE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90PCKY          CHAIN     PPC01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* PAYMENT TYPE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C*  WE NOW HAVE TO CHECK AGAINST ANOTHER TABLE, GET THE RESERVE
     C*  ENTRY WE HAVE KEYED, VALIDATE IF THE TRANS TYPE IS NOT A
     C*  VOID, STOP PAY, OR REVERSAL.
     C*
     C  N90SCTYPE        IFNE      'V'
     C     SCTYPE        ANDNE     'S'
     C     SCTYPE        ANDNE     'Z'
     C*
     C                   MOVEL(P)  SCRECD        ##KEY
     C*
     C     RTKY          CHAIN(N)  PRT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      RTTYPE        SCTTYP
     C                   MOVE      RTDESC        SCRDSC
     C*
     C                   ELSE
     C*
     C                   SETON                                        9079
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  SCTYPE
     C*
     C*  THE PAYMENT TYPE RECORD TYPE MUST BE M, P, V, R, S Z AND A.
     C*
     C  N90SCTYPE        IFNE      'M'
     C     SCTYPE        ANDNE     'P'
     C     SCTYPE        ANDNE     'V'
     C     SCTYPE        ANDNE     'R'
     C     SCTYPE        ANDNE     'S'
     C     SCTYPE        ANDNE     'Z'
     C     SCTYPE        ANDNE     'A'
     C*
     C                   SETON                                        74  90
     C*
     C                   ENDIF                                                  SCTYPE
     C*
     C*  A VOID, STOP PAY, OR REVERSE TYPE OF CODE CANNOT HAVE
     C*  A RESERVE CODE
     C*
     C  N90SCTYPE        IFEQ      'V'
     C     SCRECD        ANDNE     *BLANKS
     C*
     C     SCTYPE        OREQ      'S'
     C     SCRECD        ANDNE     *BLANKS
     C*
     C     SCTYPE        OREQ      'Z'
     C     SCRECD        ANDNE     *BLANKS
     C*
     C                   SETON                                        8290
     C*
     C                   ENDIF
     C*
     C*  CHECK TRANSFER TO CHECK REC FLAG FOR VALIDITY
     C*
     C  N90SCCKTR        IFNE      'Y'
     C     SCCKTR        ANDNE     'N'
     C     SCTYPE        OREQ      'A'
     C     SCCKTR        ANDNE     'N'
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF                                                  SCCKTR
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
     C  N90##RTC         IFEQ      *ON
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
     C  N90PCDT          IFGT      SAVEDT
     C     PCDT          OREQ      SAVEDT
     C     PCTIME        ANDGT     SAVETM
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL(P)  PCRECD        ##KEY
     C*
     C     RTKY          CHAIN(N)  PRT01                              99
     C*
     C                   MOVE      RTTYPE        SCTTYP
     C                   MOVE      RTDESC        SCRDSC
     C*
     C                   MOVEL     PCKEY         SCKEY
     C                   MOVEL     PCDESC        SCDESC
     C                   MOVEL     PCTRTP        SCTYPE
     C                   MOVEL     PCCKTR        SCCKTR
     C                   MOVEL     PCRECD        SCRECD
     C                   MOVEL     PCSTAT        SCSTAT
     C                   MOVEL     PCDCO         SCDCO
     C                   MOVEL     PCDFAC        SCDFAC
     C                   MOVEL     PCDDPT        SCDDPT
     C                   MOVEL     PCDACC        SCDACC
     C                   MOVEL     PCDSUB        SCDSUB
     C                   MOVEL     PCCCO         SCCCO
     C                   MOVEL     PCCFAC        SCCFAC
     C                   MOVEL     PCCDPT        SCCDPT
     C                   MOVEL     PCCACC        SCCACC
     C                   MOVEL     PCCSUB        SCCSUB
     C*** Begin add *** 07/10/03 **************************************
     C                   MOVEL     PCLVL         SCLVL
     C                   MOVEL     PCTY01        SCTY01
     C                   MOVEL     PCTY02        SCTY02
     C                   MOVEL     PCTY03        SCTY03
     C                   MOVEL     PCTY04        SCTY04
     C                   MOVEL     PCTY05        SCTY05
     C*** End add ***** 07/10/03 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCRECD
     C                   MOVEL     *BLANKS       SCTYPE
     C                   MOVEL     *BLANKS       SCTTYP
     C                   MOVEL     *BLANKS       SCCKTR
     C                   MOVEL     *BLANKS       SCRDSC
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
     C*** Begin add *** 07/10/03 **************************************
     C                   MOVE      *ZEROS        SCLVL
     C                   MOVE      *BLANKS       SCTY01
     C                   MOVE      *BLANKS       SCTY02
     C                   MOVE      *BLANKS       SCTY03
     C                   MOVE      *BLANKS       SCTY04
     C                   MOVE      *BLANKS       SCTY05
     C*** End add ***** 07/10/03 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         PCCO#
     C                   MOVEL     SCKEY         PCKEY
     C                   MOVEL     SCDESC        PCDESC
     C                   MOVEL     SCTYPE        PCTRTP
     C                   MOVEL     SCCKTR        PCCKTR
     C                   MOVEL     SCRECD        PCRECD
     C                   MOVEL     SCSTAT        PCSTAT
     C                   MOVEL     SCDCO         PCDCO
     C                   MOVEL     SCDFAC        PCDFAC
     C                   MOVEL     SCDDPT        PCDDPT
     C                   MOVEL     SCDACC        PCDACC
     C                   MOVEL     SCDSUB        PCDSUB
     C                   MOVEL     SCCCO         PCCCO
     C                   MOVEL     SCCFAC        PCCFAC
     C                   MOVEL     SCCDPT        PCCDPT
     C                   MOVEL     SCCACC        PCCACC
     C                   MOVEL     SCCSUB        PCCSUB
     C*** Begin add *** 07/10/03 **************************************
     C                   MOVEL     SCLVL         PCLVL
     C                   MOVEL     SCTY01        PCTY01
     C                   MOVEL     SCTY02        PCTY02
     C                   MOVEL     SCTY03        PCTY03
     C                   MOVEL     SCTY04        PCTY04
     C                   MOVEL     SCTY05        PCTY05
     C*** End add ***** 07/10/03 **************************************
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
     C*
     C     HFLD          IFEQ      'SCRECD'
     C*
     C* RESERVE CODE SEARCH
     C*
     C                   MOVE      *BLANK        RCKEY            10
     C*
     C                   CALL      'W2136R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    RCKEY
     C*
     C     *INKL         IFEQ      *ON
     C     RCKEY         OREQ      *BLANKS
     C*
     C                   MOVE      *BLANKS       SCRECD
     C*
     C                   ELSE
     C*
     C                   MOVEL     RCKEY         SCRECD
     C*
     C*  CHAIN TO GET DESCRIPTION & RESERVE TYPE TO DISPLAY AS OUTPUT
     C*
     C                   MOVEL(P)  SCRECD        ##KEY
     C*
     C     RTKY          CHAIN(N)  PRT01                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      RTTYPE        SCTTYP
     C                   MOVE      RTDESC        SCRDSC
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCTTYP
     C                   MOVE      *BLANKS       SCRDSC
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  *INKL
     C*
     C                   ENDIF                                                  HFLD SCRECD
     C*
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
     C*
     C* SEARCH G/L ACCOUNTS
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
     C                   ENDIF                                                  HFLD SCDCO
     C*
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  *INKJ
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
     C                   MOVE      *BLANK        PCRYTP
     C                   MOVEL     Q1USER        PCUSER
     C                   MOVEL     Q1PGM         PCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C*
     C                   MOVEL     DSTIME        PCTIME
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
     C                   MOVE      *BLANK        PCRYTP
     C                   MOVEL     Q1USER        PCUSER
     C                   MOVEL     Q1PGM         PCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C*
     C                   MOVEL     DSTIME        PCTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTPCP
     C*
     C                   ENDSR
