     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H******************************************************************
     H/TITLE W2493R02 - SYSTEM TABLE FILE UPDATE/DISPLAY              *
     H******************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM ADD/CHANGES/DISPLAYS THE    *
     H*                      SYSTEM TABLES.                           *
     H*                                                               *
     H* CALLED FROM:  W2493R                                          *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED:  04/09/04 PROJECT: WC577  PGMR: C:136                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/03/05  C: 132  WA102 44503                                 *
     H*                 - Value Heading does not show on Add record   *
     H*                 - Status of "A" not appearing on Add record   *
     H*                                                               *
     H*****************************************************************
     FW2493F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWTGTP     UF A E             DISK
     F                                     RENAME(PGT01:DBRCDF)
     FWTGIL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D PRDS            C                   CONST('          -
     D                                     System Table Search')
     D OPTNM           C                   CONST('System Table Values')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      ***** BEG ADD ***** 01/03/05 ************************************
     C                   PARM                    @@TBID            4
      ***** END ADD ***** 01/01/05 ************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCID
     C                   KFLD                    SCKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    GITBID        KKTBID
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        IFEQ      'I'
     C                   MOVE      'A'           GTSTAT
     C                   ELSE
     C                   MOVE      'I'           GTSTAT
     C                   ENDIF                                                  DPSTAT=I
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C                   MOVEL     DSTIME        GTTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
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
      *
     C     @@MODE        IFEQ      'A'
      *
      * REDISPLAY SCREEN
      *
     C     GITBID        IFNE      SCID
     C                   SETON                                            90
     C                   ENDIF
      *
     C                   MOVE      SCID          KKTBID
     C     GIKY          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *OFF
     C     GISTAT        ANDEQ     'A'
     C                   MOVE      GITBID        SCID
     C                   MOVEL     GIDESC        SCIDSC
     C                   MOVEL     GIFN01        SCKDSC
     C                   ELSE
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ENDIF
      *
     C  N90SCKEY         IFNE      GTKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ENDIF
      *
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*===============================================================
     C* KJDO   - SEARCH SUBROUTINE
     C*===============================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCID'
     C                   MOVEL     SCID          QQID
     C                   MOVEL     PRDS          QQPRDS
     C                   CALL      'W2491R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQID          SCID
      *
     C     QQID          IFNE      *BLANKS
     C                   MOVEL     QQID          SCID
     C                   MOVE      SCID          KKTBID
     C     GIKY          CHAIN     PGI01                              60
     C                   MOVEL     GIDESC        SCIDSC
     C                   MOVEL     GIFN01        SCKDSC
     C                   SETON                                        81
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      GTSTAT        SCSTAT
     C                   MOVE      GTKEY         SCKEY
     C                   MOVEL     GTDESC        SCDESC
     C                   MOVE      GTFG01        SCFL01
     C                   MOVE      GTFG02        SCFL02
     C                   MOVE      GTFG03        SCFL03
     C                   MOVE      GTFG04        SCFL04
     C                   MOVE      GTFG05        SCFL05
     C                   MOVE      GTFG06        SCFL06
     C                   MOVE      GTFG07        SCFL07
     C                   MOVE      GTFG08        SCFL08
     C                   MOVE      GTFG09        SCFL09
     C                   MOVE      GTFG10        SCFL10
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
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      ##CO#         GTCO#
     C                   MOVE      SCID          GTTBID
     C                   MOVE      SCKEY         GTKEY
     C                   MOVEL     SCDESC        GTDESC
     C                   MOVE      SCSTAT        GTSTAT
     C                   MOVE      SCFL01        GTFG01
     C                   MOVE      SCFL02        GTFG02
     C                   MOVE      SCFL03        GTFG03
     C                   MOVE      SCFL04        GTFG04
     C                   MOVE      SCFL05        GTFG05
     C                   MOVE      SCFL06        GTFG06
     C                   MOVE      SCFL07        GTFG07
     C                   MOVE      SCFL08        GTFG08
     C                   MOVE      SCFL09        GTFG09
     C                   MOVE      SCFL10        GTFG10
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C     @@MODE        IFNE      'A'
     C     @@RRN         CHAIN(N)  DBRCDF                             60
     C                   MOVE      GTTBID        KKTBID
     C     *IN60         IFEQ      *OFF
     C     GIKY          CHAIN     PGI01                              60
     C                   MOVE      GITBID        SCID
     C                   MOVEL     GIDESC        SCIDSC
     C                   MOVEL     GIFN01        SCKDSC
     C                   ENDIF
     C                   ENDIF
      *
      ***** BEG ADD ***** 01/03/05 ************************************
      * If pass in parms on add, get items for display on ADD
      *
     C     @@MODE        IFEQ      'A'
     C     Q1PRMS        ANDGT     4
     C                   MOVE      @@TBID        KKTBID
     C     GIKY          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      GITBID        SCID
     C                   MOVEL     GIDESC        SCIDSC
     C                   MOVEL     GIFN01        SCKDSC
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ***** 01/01/05 ************************************
      *
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVE      Q1USER        GTUSER
     C                   MOVE      Q1PGM         GTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C                   Z-ADD     DSTIME        GTTIME
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
      ***** BEG ADD ***** 01/03/05 ************************************
     C     *IN40         IFEQ      *ON
     C     GTSTAT        OREQ      *BLANK
     C                   MOVE      'A'           GTSTAT
     C                   ENDIF
      ***** END ADD ***** 01/01/05 ************************************
      *
     C                   MOVE      Q1USER        GTUSER
     C                   MOVE      Q1PGM         GTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C                   Z-ADD     DSTIME        GTTIME
     C                   WRITE     DBRCDF
     C                   FEOD      WTGTP
     C                   ENDSR
