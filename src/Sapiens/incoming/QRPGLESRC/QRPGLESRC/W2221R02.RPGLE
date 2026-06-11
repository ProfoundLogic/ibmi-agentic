     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H/TITLE W2221R02 - LATE REASON CODE
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  10/11/06  C:33  Seq#: WA099/41801                           *
     H*                  Use SC305R instead of SC580R.               *
     H*                                                              *
     H*  06/11/98  C:33  SEQ#: WC051/11364                           *
     H*                  ALWAYS DISPLAY EDI LATE REASON CODE         *
     H****************************************************************
     FW2221F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTLRP     UF A E             DISK
     F                                     RENAME(PLR01:DBRCDF)
     FWTABL     IF   E           K DISK
     FSTCTL2    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Late Reason Master-
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     LRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C     CTKY          KLIST
     C                   KFLD                    ##STAT
     C                   KFLD                    SCDART
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    ##KEY
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##SKY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    LRCO#         ##CO#
     C     *LIKE         DEFINE    LRKEY         ##KEY
     C     *LIKE         DEFINE    LRKEY         @@KEY
     C     *LIKE         DEFINE    LRSTAT        ##STAT
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    WCKEY         ##SKY
     C     *LIKE         DEFINE    SCDART        ##DART
     C***  Begin Add  ***  10/11/06  **************************************
     C                   MOVEL     *BLANKS       QQRTC             1
     C***  Begin End  ***  10/11/06  **************************************
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
     C     LRSTAT        IFEQ      'I'
     C                   MOVE      'A'           LRSTAT
     C                   ELSE
     C                   MOVE      'I'           LRSTAT
     C                   ENDIF                                                  LRSTAT
     C*
     C                   MOVEL     Q1USER        LRUSER
     C                   MOVEL     Q1PGM         LRPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LRDT
     C*
     C                   MOVEL     DSTIME        LRTIME
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
     C* LATE REASON CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE LATE REASON CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         ##KEY
     C*
     C   40
     CANN90LRKY          CHAIN     PLR01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* LATE REASON CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* EXPLANATION MUST BE Y OR N
     C*
     C  N90SCREAS        IFNE      'Y'
     C     SCREAS        ANDNE     'N'
     C*
     C                   SETON                                        80  90
     C*
     C                   ENDIF                                                  SCREAS
     C*
     C* DART CODE MUST BE VALID
     C*
     C  N90SCDART        IFNE      *BLANKS
     C*
     C                   MOVE      'A'           ##STAT
     C*
     C     CTKY          CHAIN     PCT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        81  90
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  SCDART NE ''
     C*
     C* IF REASON CODE IS Y THEN DART CODE MUST BE ENTERED.
     C*
     C  N90SCREAS        IFEQ      'Y'
     C     SCDART        ANDEQ     *BLANKS
     C*
     C                   SETON                                        82  90
     C*
     C                   ENDIF                                                  SCREAS/SCDART
     C*
     C* IF EDI IS INSTALLED, VALIDATE THE EDI LATE REASON CODE.
     C*
     C  N90WCFL09        IFEQ      'Y'
     C***  BEGIN ADD  ***  06/11/98  *********************************
     C     SCLRCD        ORNE      *BLANKS
     C***  END   ADD  ***  06/11/98  *********************************
     C*
     C                   MOVEL     'LRSN'        ##TBID
     C                   MOVEL     SCLRCD        ##KEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        83  90
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  WCFL09
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90LRDT          IFGT      SAVEDT
     C     LRDT          OREQ      SAVEDT
     C     LRTIME        ANDGT     SAVETM
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
     C                   MOVEL     LRKEY         SCKEY
     C                   MOVEL     LRDESC        SCDESC
     C                   MOVEL     LRSTAT        SCSTAT
     C                   MOVEL     LRREAS        SCREAS
     C                   MOVEL     LRDART        SCDART
     C                   MOVEL     LRLRCD        SCLRCD
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVEL     'A'           SCSTAT
     C                   MOVE      *BLANKS       SCREAS
     C                   MOVE      *BLANKS       SCDART
     C                   MOVE      *BLANKS       SCLRCD
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         LRCO#
     C                   MOVEL     SCKEY         LRKEY
     C                   MOVEL     SCDESC        LRDESC
     C                   MOVEL     SCSTAT        LRSTAT
     C                   MOVEL     SCREAS        LRREAS
     C                   MOVEL     SCDART        LRDART
     C                   MOVEL     SCLRCD        LRLRCD
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
     C* DARTS REMARK CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCDART'
     C*
     C                   MOVE      *BLANKS       SCDART
     C                   MOVE      *BLANKS       QQRMCD
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********
     C**********           PARM *BLANKS   QQRMCD  5
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *LIKE         DEFINE    ##RMCD        QQRMCD
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C***  Begin Del  ***  10/11/06  **************************************
     C**********           MOVELQQRMCD    SCDART
     C**********
     C********** QQRTC     IFEQ 'A'
     C**********
     C**********           MOVE *ON       *INKC
     C**********
     C**********           ENDIF                           QQRTC='A'
     C***  End   Del  ***  10/11/06  **************************************
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C********** QQRTC     IFEQ 'B'
     C     QQRMCD        IFEQ      *BLANKS
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C                   MOVE      *ON           *IN84
     C*
     C                   ELSE
     C*
     C***  Begin Add  ***  10/11/06  **************************************
     C                   MOVEL     QQRMCD        SCDART
     C***  End   Add  ***  10/11/06  **************************************
     C                   MOVE      *ON           *IN85
     C*
     C                   ENDIF                                                  QQRTC='B'
     C*
     C                   ENDIF                                                  HFLD=SCDART
     C*
     C* EDI LATE REASON CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCLRCD'
     C*
     C                   MOVE      *BLANKS       SCLRCD
     C                   MOVEL     'LRSN'        QQID              5
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVEL     SCPRDS        QQPRDS           40
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    @@MODE
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCLRCD
     C*
     C     QQRTC         IFEQ      'A'
     C*
     C                   MOVE      *ON           *INKC
     C*
     C                   ENDIF                                                  QQRTC='A'
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        85
     C*
     C                   ENDIF                                                  QQRTC='B'
     C*
     C                   ENDIF                                                  HFLD=SCLRCD
     C*
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  *INKJ
     C*
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
     C* CHECK THAT EDI IS INSTALLED, OR IGNORE EDI INFO
     C*
     C                   MOVEL     'WC'          ##SKY
     C*
     C     SYKY          CHAIN     SWC01                              99
     C*
     C     WCFL09        COMP      'Y'                                    16
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
     C                   MOVEL     Q1USER        LRUSER
     C                   MOVEL     Q1PGM         LRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LRDT
     C*
     C                   MOVEL     DSTIME        LRTIME
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
     C                   MOVEL     Q1USER        LRUSER
     C                   MOVEL     Q1PGM         LRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LRDT
     C*
     C                   MOVEL     DSTIME        LRTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WTLRP
     C*
     C                   ENDSR
