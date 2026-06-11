     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*******************************************************************
     H/TITLE F2375R01 - GENERIC TABLE ID MAINTENANCE.
     H*******************************************************************
     H*  Revisions:                                                   * *
     H*                                                               *
     H*  05/20/09 C:33  Project: WC1770                               *
     H*                 Ensure files are kept in sync.                *
     H*                                                               *
     H*  10/12/00 C:62  PROJECT: MC183                                *
     H*                 ADD PROCESSING FOR THE NEW HEADER FIELDS.     *
     H*                                                               * *
     H*******************************************************************
     H*  Indicators:                                                  * *
     H*                                                               * *
     H*******************************************************************
     FF2375F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTGIP     UF A E             DISK
     F                                     RENAME(PGI01:DBRCDF)
     F***  Begin Add  ***  05/20/09  **************************************
     F                                     INFDS(GIFDS)
     F***  End   Add  ***  05/20/09  **************************************
     FFTGIL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D***  Begin Add  ***  05/20/09  **************************************
     D GIFDS           DS
     D  GIRRN                397    400B 0
     D***  End   Add  ***  05/20/09  **************************************
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Generic Table ID -
     D                                     Maintenance')
     D COMAPI          C                   CONST('FA007R')
     D***  Begin Add  ***  05/20/09  **************************************
     D SYSICD          C                   CONST('LODSYSICD9')
     D***  End   Add  ***  05/20/09  **************************************
     D #OFSCR          C                   CONST(3)

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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    GICO#         ##CO#
     C     *LIKE         DEFINE    GITBID        KKTBID
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
     C     GISTAT        IFEQ      'I'
     C                   MOVE      'A'           GISTAT
     C                   ELSE
     C                   MOVE      'I'           GISTAT
     C                   ENDIF                                                  GISTAT
     C*
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIDT
     C*
     C                   MOVEL     DSTIME        GITIME
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
     C* TABLE ID MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCTBID        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCTBID
     C*
     C* CHECK TO BE SURE TABLE ID HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCTBID        KKTBID
     C*
     C   40
     CANN90GIKY          CHAIN     PGI01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* TABLE ID DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* KEY FIELD CANNOT BE BLANK
     C*
     C  N90SCFN01        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCFN01
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90GIDT          IFGT      SAVEDT
     C     GIDT          OREQ      SAVEDT
     C     GITIME        ANDGT     SAVETM
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
     C                   MOVEL     GITBID        SCTBID
     C                   MOVEL     GIDESC        SCDESC
     C                   MOVEL     GIFN01        SCFN01
     C*** BEGIN ADD *** 10/12/00 **************************************
     C                   MOVEL     GIFH01        SCFH01
     C                   MOVEL     GIFH02        SCFH02
     C                   MOVEL     GIFH03        SCFH03
     C                   MOVEL     GIFH04        SCFH04
     C                   MOVEL     GIFH05        SCFH05
     C                   MOVEL     GIFH06        SCFH06
     C                   MOVEL     GIFH07        SCFH07
     C                   MOVEL     GIFH08        SCFH08
     C                   MOVEL     GIFH09        SCFH09
     C                   MOVEL     GIFH10        SCFH10
     C                   MOVEL     GICH01        SCCH01
     C                   MOVEL     GICH02        SCCH02
     C                   MOVEL     GICH03        SCCH03
     C                   MOVEL     GICH04        SCCH04
     C                   MOVEL     GICH05        SCCH05
     C                   MOVEL     GI5H01        SC5H01
     C                   MOVEL     GI5H02        SC5H02
     C                   MOVEL     GI5H03        SC5H03
     C                   MOVEL     GI5H04        SC5H04
     C                   MOVEL     GI5H05        SC5H05
     C                   MOVEL     GI9H01        SC9H01
     C                   MOVEL     GI9H02        SC9H02
     C                   MOVEL     GI9H03        SC9H03
     C                   MOVEL     GI9H04        SC9H04
     C                   MOVEL     GI9H05        SC9H05
     C                   MOVEL     GI1H01        SC1H01
     C                   MOVEL     GI1H02        SC1H02
     C                   MOVEL     GI1H03        SC1H03
     C                   MOVEL     GI1H04        SC1H04
     C*** END   ADD *** 10/12/00 **************************************
     C                   MOVEL     GISTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCTBID
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     *BLANKS       SCFN01
     C*** BEGIN ADD *** 10/12/00 **************************************
     C                   MOVEL     *BLANKS       SCFH01
     C                   MOVEL     *BLANKS       SCFH02
     C                   MOVEL     *BLANKS       SCFH03
     C                   MOVEL     *BLANKS       SCFH04
     C                   MOVEL     *BLANKS       SCFH05
     C                   MOVEL     *BLANKS       SCFH06
     C                   MOVEL     *BLANKS       SCFH07
     C                   MOVEL     *BLANKS       SCFH08
     C                   MOVEL     *BLANKS       SCFH09
     C                   MOVEL     *BLANKS       SCFH10
     C                   MOVEL     *BLANKS       SCCH01
     C                   MOVEL     *BLANKS       SCCH02
     C                   MOVEL     *BLANKS       SCCH03
     C                   MOVEL     *BLANKS       SCCH04
     C                   MOVEL     *BLANKS       SCCH05
     C                   MOVEL     *BLANKS       SC5H01
     C                   MOVEL     *BLANKS       SC5H02
     C                   MOVEL     *BLANKS       SC5H03
     C                   MOVEL     *BLANKS       SC5H04
     C                   MOVEL     *BLANKS       SC5H05
     C                   MOVEL     *BLANKS       SC9H01
     C                   MOVEL     *BLANKS       SC9H02
     C                   MOVEL     *BLANKS       SC9H03
     C                   MOVEL     *BLANKS       SC9H04
     C                   MOVEL     *BLANKS       SC9H05
     C                   MOVEL     *BLANKS       SC1H01
     C                   MOVEL     *BLANKS       SC1H02
     C                   MOVEL     *BLANKS       SC1H03
     C                   MOVEL     *BLANKS       SC1H04
     C*** END   ADD *** 10/12/00 **************************************
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
     C                   Z-ADD     ##CO#         GICO#
     C                   MOVEL     SCTBID        GITBID
     C                   MOVEL     SCDESC        GIDESC
     C                   MOVEL     SCFN01        GIFN01
     C*** BEGIN ADD *** 10/12/00 **************************************
     C                   MOVEL     SCFH01        GIFH01
     C                   MOVEL     SCFH02        GIFH02
     C                   MOVEL     SCFH03        GIFH03
     C                   MOVEL     SCFH04        GIFH04
     C                   MOVEL     SCFH05        GIFH05
     C                   MOVEL     SCFH06        GIFH06
     C                   MOVEL     SCFH07        GIFH07
     C                   MOVEL     SCFH08        GIFH08
     C                   MOVEL     SCFH09        GIFH09
     C                   MOVEL     SCFH10        GIFH10
     C                   MOVEL     SCCH01        GICH01
     C                   MOVEL     SCCH02        GICH02
     C                   MOVEL     SCCH03        GICH03
     C                   MOVEL     SCCH04        GICH04
     C                   MOVEL     SCCH05        GICH05
     C                   MOVEL     SC5H01        GI5H01
     C                   MOVEL     SC5H02        GI5H02
     C                   MOVEL     SC5H03        GI5H03
     C                   MOVEL     SC5H04        GI5H04
     C                   MOVEL     SC5H05        GI5H05
     C                   MOVEL     SC9H01        GI9H01
     C                   MOVEL     SC9H02        GI9H02
     C                   MOVEL     SC9H03        GI9H03
     C                   MOVEL     SC9H04        GI9H04
     C                   MOVEL     SC9H05        GI9H05
     C                   MOVEL     SC1H01        GI1H01
     C                   MOVEL     SC1H02        GI1H02
     C                   MOVEL     SC1H03        GI1H03
     C                   MOVEL     SC1H04        GI1H04
     C*** END   ADD *** 10/12/00 **************************************
     C                   MOVEL     SCSTAT        GISTAT
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
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
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
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIDT
     C*
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C***  Begin Add  ***  05/20/09  **************************************
     C                   CALL      SYSICD                               99
     C***  End   Add  ***  05/20/09  **************************************
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
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIDT
     C*
     C                   MOVEL     DSTIME        GITIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FTGIP
     C*
     C***  Begin Add  ***  05/20/09  **************************************
     C                   CALL      SYSICD                               99
     C***  End   Add  ***  05/20/09  **************************************
     C*
     C                   ENDSR
