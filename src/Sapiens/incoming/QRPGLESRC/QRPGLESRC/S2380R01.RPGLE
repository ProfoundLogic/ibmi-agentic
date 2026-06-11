     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H* CREATED 01/26/98   C:86   PROJECT DR052
     H*****************************************************************
     H* REVISIONS:
     H*
     H*  05/09/05 C:62  PROJECT: SY120
     H*                 ADD PROCESSING FOR THE NEW HEADER FIELDS.
     H*
     H* 09/18/00 C:062 PROJ:PR008
     H*                COMPILE ONLY.
     H*
     H*****************************************************************
     H*
     FS2380F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSTGTP     UF A E             DISK
     F                                     RENAME(PGT01:DBRCDF)
     FSTGTL     IF   E           K DISK
     FSTGIL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D TIN             S              1    DIM(46)                              TBID NAME
     D*** BEGIN ADD *** 05/09/05 **************************************
     D HDR             S             20    DIM(29) CTDATA PERRCD(1)             DEFAULT HEADINGS
     D*** END   ADD *** 05/09/05 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Generic Table Item-
     D                                      Maintenance')
     D COMAPI          C                   CONST('FA007R')
     D*** BEGIN CHG *** 05/09/05 **************************************
     D**********    1                     C         #OFSCR
     D #OFSCR          C                   CONST(2)

     I*** END   CHG *** 05/09/05 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@TBID
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
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKKEY
     C*
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    GTCO#         ##CO#
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        @@TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
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
     C     GTSTAT        IFEQ      'I'
     C                   MOVE      'A'           GTSTAT
     C                   ELSE
     C                   MOVE      'I'           GTSTAT
     C                   ENDIF                                                  GTSTAT
     C*
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C*
     C                   MOVEL     DSTIME        GTTIME
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
     C* TABLE ITEM MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE TABLE ITEM HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         KKKEY
     C*
     C   40
     CANN90GTKY          CHAIN     PGT01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* TABLE ITEM DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90GTDT          IFGT      SAVEDT
     C     GTDT          OREQ      SAVEDT
     C     GTTIME        ANDGT     SAVETM
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
     C* MKTBHD - Makes the table id header.
     C*================================================================
     C     MKTBHD        BEGSR
     C*
     C     GIKY          CHAIN     PGI01                              99
     C*
     C                   MOVEL     GITBID        SCTBID
     C                   MOVEL     GIDESC        SCTBNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCKEY
     C                   MOVEL     GTDESC        SCDESC
     C                   MOVE      GTFG01        SCFG01
     C                   MOVE      GTFG02        SCFG02
     C                   MOVE      GTFG03        SCFG03
     C                   MOVE      GTFG04        SCFG04
     C                   MOVE      GTFG05        SCFG05
     C                   MOVE      GTFG06        SCFG06
     C                   MOVE      GTFG07        SCFG07
     C                   MOVE      GTFG08        SCFG08
     C                   MOVE      GTFG09        SCFG09
     C                   MOVE      GTFG10        SCFG10
     C                   MOVE      GTCF01        SCCF01
     C                   MOVE      GTCF02        SCCF02
     C                   MOVE      GTCF03        SCCF03
     C                   MOVE      GTCF04        SCCF04
     C                   MOVE      GTCF05        SCCF05
     C                   MOVE      GTN501        SCN501
     C                   MOVE      GTN502        SCN502
     C                   MOVE      GTN503        SCN503
     C                   MOVE      GTN504        SCN504
     C                   MOVE      GTN505        SCN505
     C                   MOVE      GTN901        SCN901
     C                   MOVE      GTN902        SCN902
     C                   MOVE      GTN903        SCN903
     C                   MOVE      GTN904        SCN904
     C                   MOVE      GTN905        SCN905
     C                   MOVE      GTNB01        SCNB01
     C                   MOVE      GTNB02        SCNB02
     C                   MOVE      GTNB03        SCNB03
     C                   MOVE      GTNB04        SCNB04
     C                   MOVEL     GTSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCFG01
     C                   MOVE      *BLANKS       SCFG02
     C                   MOVE      *BLANKS       SCFG03
     C                   MOVE      *BLANKS       SCFG04
     C                   MOVE      *BLANKS       SCFG05
     C                   MOVE      *BLANKS       SCFG06
     C                   MOVE      *BLANKS       SCFG07
     C                   MOVE      *BLANKS       SCFG08
     C                   MOVE      *BLANKS       SCFG09
     C                   MOVE      *BLANKS       SCFG10
     C                   MOVE      *BLANKS       SCCF01
     C                   MOVE      *BLANKS       SCCF02
     C                   MOVE      *BLANKS       SCCF03
     C                   MOVE      *BLANKS       SCCF04
     C                   MOVE      *BLANKS       SCCF05
     C                   MOVE      *ZEROS        SCN501
     C                   MOVE      *ZEROS        SCN502
     C                   MOVE      *ZEROS        SCN503
     C                   MOVE      *ZEROS        SCN504
     C                   MOVE      *ZEROS        SCN505
     C                   MOVE      *ZEROS        SCN901
     C                   MOVE      *ZEROS        SCN902
     C                   MOVE      *ZEROS        SCN903
     C                   MOVE      *ZEROS        SCN904
     C                   MOVE      *ZEROS        SCN905
     C                   MOVE      *ZEROS        SCNB01
     C                   MOVE      *ZEROS        SCNB02
     C                   MOVE      *ZEROS        SCNB03
     C                   MOVE      *ZEROS        SCNB04
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 05/09/05 **************************************
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
     C*
     C     SCFH01        IFEQ      *BLANKS
     C                   MOVEL     HDR(1)        SCFH01
     C                   ENDIF
     C*
     C     SCFH02        IFEQ      *BLANKS
     C                   MOVEL     HDR(2)        SCFH02
     C                   ENDIF
     C*
     C     SCFH03        IFEQ      *BLANKS
     C                   MOVEL     HDR(3)        SCFH03
     C                   ENDIF
     C*
     C     SCFH04        IFEQ      *BLANKS
     C                   MOVEL     HDR(4)        SCFH04
     C                   ENDIF
     C*
     C     SCFH05        IFEQ      *BLANKS
     C                   MOVEL     HDR(5)        SCFH05
     C                   ENDIF
     C*
     C     SCFH06        IFEQ      *BLANKS
     C                   MOVEL     HDR(6)        SCFH06
     C                   ENDIF
     C*
     C     SCFH07        IFEQ      *BLANKS
     C                   MOVEL     HDR(7)        SCFH07
     C                   ENDIF
     C*
     C     SCFH08        IFEQ      *BLANKS
     C                   MOVEL     HDR(8)        SCFH08
     C                   ENDIF
     C*
     C     SCFH09        IFEQ      *BLANKS
     C                   MOVEL     HDR(9)        SCFH09
     C                   ENDIF
     C*
     C     SCFH10        IFEQ      *BLANKS
     C                   MOVEL     HDR(10)       SCFH10
     C                   ENDIF
     C*
     C     SCCH01        IFEQ      *BLANKS
     C                   MOVEL     HDR(11)       SCCH01
     C                   ENDIF
     C*
     C     SCCH02        IFEQ      *BLANKS
     C                   MOVEL     HDR(12)       SCCH02
     C                   ENDIF
     C*
     C     SCCH03        IFEQ      *BLANKS
     C                   MOVEL     HDR(13)       SCCH03
     C                   ENDIF
     C*
     C     SCCH04        IFEQ      *BLANKS
     C                   MOVEL     HDR(14)       SCCH04
     C                   ENDIF
     C*
     C     SCCH05        IFEQ      *BLANKS
     C                   MOVEL     HDR(15)       SCCH05
     C                   ENDIF
     C*
     C     SC5H01        IFEQ      *BLANKS
     C                   MOVEL     HDR(16)       SC5H01
     C                   ENDIF
     C*
     C     SC5H02        IFEQ      *BLANKS
     C                   MOVEL     HDR(17)       SC5H02
     C                   ENDIF
     C*
     C     SC5H03        IFEQ      *BLANKS
     C                   MOVEL     HDR(18)       SC5H03
     C                   ENDIF
     C*
     C     SC5H04        IFEQ      *BLANKS
     C                   MOVEL     HDR(19)       SC5H04
     C                   ENDIF
     C*
     C     SC5H05        IFEQ      *BLANKS
     C                   MOVEL     HDR(20)       SC5H05
     C                   ENDIF
     C*
     C     SC9H01        IFEQ      *BLANKS
     C                   MOVEL     HDR(21)       SC9H01
     C                   ENDIF
     C*
     C     SC9H02        IFEQ      *BLANKS
     C                   MOVEL     HDR(22)       SC9H02
     C                   ENDIF
     C*
     C     SC9H03        IFEQ      *BLANKS
     C                   MOVEL     HDR(23)       SC9H03
     C                   ENDIF
     C*
     C     SC9H04        IFEQ      *BLANKS
     C                   MOVEL     HDR(24)       SC9H04
     C                   ENDIF
     C*
     C     SC9H05        IFEQ      *BLANKS
     C                   MOVEL     HDR(25)       SC9H05
     C                   ENDIF
     C*
     C     SC1H01        IFEQ      *BLANKS
     C                   MOVEL     HDR(26)       SC1H01
     C                   ENDIF
     C*
     C     SC1H02        IFEQ      *BLANKS
     C                   MOVEL     HDR(27)       SC1H02
     C                   ENDIF
     C*
     C     SC1H03        IFEQ      *BLANKS
     C                   MOVEL     HDR(28)       SC1H03
     C                   ENDIF
     C*
     C     SC1H04        IFEQ      *BLANKS
     C                   MOVEL     HDR(29)       SC1H04
     C                   ENDIF
     C*** END   ADD *** 05/09/05 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         GTCO#
     C                   MOVEL     SCTBID        GTTBID
     C                   MOVEL     SCKEY         GTKEY
     C                   MOVEL     SCDESC        GTDESC
     C                   MOVE      SCFG01        GTFG01
     C                   MOVE      SCFG02        GTFG02
     C                   MOVE      SCFG03        GTFG03
     C                   MOVE      SCFG04        GTFG04
     C                   MOVE      SCFG05        GTFG05
     C                   MOVE      SCFG06        GTFG06
     C                   MOVE      SCFG07        GTFG07
     C                   MOVE      SCFG08        GTFG08
     C                   MOVE      SCFG09        GTFG09
     C                   MOVE      SCFG10        GTFG10
     C                   MOVE      SCCF01        GTCF01
     C                   MOVE      SCCF02        GTCF02
     C                   MOVE      SCCF03        GTCF03
     C                   MOVE      SCCF04        GTCF04
     C                   MOVE      SCCF05        GTCF05
     C                   MOVE      SCN501        GTN501
     C                   MOVE      SCN502        GTN502
     C                   MOVE      SCN503        GTN503
     C                   MOVE      SCN504        GTN504
     C                   MOVE      SCN505        GTN505
     C                   MOVE      SCN901        GTN901
     C                   MOVE      SCN902        GTN902
     C                   MOVE      SCN903        GTN903
     C                   MOVE      SCN904        GTN904
     C                   MOVE      SCN905        GTN905
     C                   MOVE      SCNB01        GTNB01
     C                   MOVE      SCNB02        GTNB02
     C                   MOVE      SCNB03        GTNB03
     C                   MOVE      SCNB04        GTNB04
     C                   MOVEL     SCSTAT        GTSTAT
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
     C                   MOVEL     @@TBID        ##TBID
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
     C* EXECUTE SUBROUTINE TO CREATE TABLE ID HEADER
     C*
     C                   EXSR      MKTBHD
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
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C*
     C                   MOVEL     DSTIME        GTTIME
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
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C*
     C                   MOVEL     DSTIME        GTTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      STGTP
     C*
     C                   ENDSR
** HDR - DEFAULT HEADING
FLAG 01            :
FLAG 02            :
FLAG 03            :
FLAG 04            :
FLAG 05            :
FLAG 06            :
FLAG 07            :
FLAG 08            :
FLAG 09            :
FLAG 10            :
CHAR 01            :
CHAR 02            :
CHAR 03            :
CHAR 04            :
CHAR 05            :
NUM (5.0) 01       :
NUM (5.0) 02       :
NUM (5.0) 03       :
NUM (5.0) 04       :
NUM (5.0) 05       :
NUM (9.0) 01       :
NUM (9.0) 02       :
NUM (9.0) 03       :
NUM (9.0) 04       :
NUM (9.0) 05       :
NUM (11.2) 01      :
NUM (11.2) 02      :
NUM (11.2) 03      :
NUM (11.2) 04      :
