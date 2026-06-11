     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2162R02 - A/R CATEGORY MAINTENANCE -  UPDATE/DISPLAY
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W2162R                                          *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 06/02/98  PROGRAMMER: C:90  PROJECT: WC837          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/16/05  C:132  PROJECT: WA099 - 48564                       *
     H*                  When in Display mode, Description could be   *
     H*                  changed. Added *IN09 on W2162F02 screen on   *
     H*                  Category description.                        *
     H*                                                               *
     H* 09/25/98  C:35   PROJECT: WC837                               *
     H*                  PROGRAM WAS GIVING ERROR ON PRE-EXISTING     *
     H*                  RECORD WHEN IN CHANGE MODE                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  71 - Used to display WC30005; Record already exists          *
     H*  72 - Used to display WCR0520; Decription cannot be blank     *
     H*  73 - Used to display WCR0503; Field cannot be blank          *
     H*  90 - Used to general purpose error indicator                 *
     H*                                                               *
     H*****************************************************************
     H*
     FW2162F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTCTP     UF A E             DISK
     F                                     RENAME(PCT01:DBRCDF)
     FWTCTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('A/R Categories')
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
     C     CTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCTYP
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CTCTYP        KKCTYP
     C     *LIKE         DEFINE    CTCO#         KKCO#
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
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      'A'           CTSTAT
     C                   ELSE
     C                   MOVE      'I'           CTSTAT
     C                   ENDIF                                                  CTSTAT
     C*
     C                   MOVEL     Q1USER        CTUSER
     C                   MOVEL     Q1PGM         CTPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C*
     C                   MOVEL     DSTIME        CTTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
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
     C*  CHECK TO BE SURE COMPANY # HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCCTYP        KKCTYP
     C*
      **** BEGIN CHG **** 09/25/98 *********************************************
     C   40
B02  CANN90CTKY          CHAIN     PCT01                              60
     C   40
     CANN90
     CANN60              SETON                                        9071
      **** END   CHG **** 09/25/98 *********************************************
     C*
     C*  DESCRIPTION CANNOT BE BLANK
     C*
      **** BEGIN CHG **** 09/25/98 *********************************************
     C********** SCDESC    IFEQ *BLANKS
     C  N90SCDESC        IFEQ      *BLANKS
      **** END   CHG **** 09/25/98 *********************************************
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF
     C*
     C*  CATEGORY TYPE CANNOT BE BLANK
     C*
      **** BEGIN CHG **** 09/25/98 *********************************************
     C********** SCCTYP    IFEQ *BLANKS
     C  N90SCCTYP        IFEQ      *BLANKS
      **** END   CHG **** 09/25/98 *********************************************
     C                   MOVE      *ON           *IN73
     C                   MOVE      *ON           *IN90
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
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  CTCTYP        SCCTYP
     C                   MOVEL(P)  CTDESC        SCDESC
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         CTCO#
     C*
     C                   MOVEL(P)  SCCTYP        CTCTYP
     C*
     C                   MOVEL(P)  SCDESC        CTDESC
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
     C                   MOVE      ##CO#         KKCO#
     C*
      **** BEGIN CHG **** 09/25/98 *********************************************
     C********** @@MODE    IFEQ 'A'
     C******************** MOVE *ON       *IN40
     C******************** ENDIF                           @@MODE EQ A
     C     @@MODE        COMP      'A'                                    40
      **** END   CHG **** 09/25/98 *********************************************
     C*
     C                   ENDSR
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
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CTUSER
     C                   MOVEL     Q1PGM         CTPGM
     C                   Z-ADD     DSTIME        CTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CTUSER
     C                   MOVEL     Q1USER        CTIUSR
     C                   MOVEL     Q1PGM         CTPGM
     C                   Z-ADD     DSTIME        CTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C                   MOVEL     SRDTO         CTIDT
     C                   MOVE      'A'           CTSTAT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WTCTP
     C                   ENDSR
