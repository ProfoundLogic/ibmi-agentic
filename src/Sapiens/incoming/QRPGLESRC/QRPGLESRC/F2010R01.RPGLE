     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H/TITLE ICD9 codes display/update
     H****************************************************************
     H*  Revisions:                                                  *
     H*                                                              *  *
     H*  05/20/09 C:33 Project: WC1770                               *  *
     H*                This program will now only call the system    *  *
     H*                 version.                                     *  *
     H*                                                              *
     H*  08/04/03 C:62 PROJECT: FS264                                *
     H*                - UPDATE ALL MAINTENANCE FIELDS ON ADD        *
     H*                - KEEP FROM ADDING DUPLICATES                 *
     H*                - MAKE SURE THAT OPERATIVES BEGIN WITH A ' '  *
     H*                                                              *
     H*  01/07/99 C:53 PROJECT: MC098 15878                          *
     H*                CHANGED CALL TO STATE SPECIFIC SCREEN FOR OHIO*
     H*                                                              *
     H*  08/27/98 C:62 PROJECT: MC090 13205                          *
     H*                OPT20 SHOULD NOT EXECUTE THE SCREEN OF        *
     H*                F2010F01.                                     *
     H*                                                              *
     H*    Date   Cons  Project/Revision                             *
     H*  05/05/97 C:69  Project: MC150; developed display/program    *
     H*  05/22/97 C:32  Project: 87MBY; Added OH BWC group/invalid fl*
     H*                                                              *
     H****************************************************************
     FF2010F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMDIP     UF A E             DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDI01:DBRCDF)
     F*** BEGIN ADD *** 08/04/03 **************************************
     FFMDIL     IF   E           K DISK
     F*** END   ADD *** 08/04/03 **************************************
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('ICD9 Codes')
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
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C********** DTYMD     PLIST
     C**********           PARM           SRDTI
     C**********           PARM *BLANKS   SRFFMT
     C**********           PARM '*YMD'    SRTFMT
     C**********           PARM           SRDTO
     C**********           PARM           SRERR
     C*** END   DEL *** 12/02/97 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** BEGIN ADD *** 08/04/03 **************************************
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*** END   ADD *** 08/04/03 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*** BEGIN ADD *** 08/04/03 **************************************
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C*** END   ADD *** 08/04/03 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
     C*===============================================================
     C*
     C     CALLR         BEGSR
     C*** BEGIN ADD *** 08/27/98 **************************************
     C     @@MODE        IFNE      'S'
     C*** END   ADD *** 08/27/98 **************************************
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C                   MOVE      DBRRN         ##RRN
     C                   MOVE      'C'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVE      @@RRN         ##RRN
     C                   MOVE      'D'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   CALL      'F2010R99'    ACDPL                  99
     C  N99              EXSR      CALLRT
     C*** BEGIN ADD *** 08/27/98 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/27/98 **************************************
     C*
     C                   ENDSR                                                  CALLR
     C*================================================================
     C* CALLRT - RETURN FROM STATE SPECIFIC SCREEN PROGRAMS
     C*================================================================
     C     CALLRT        BEGSR
     C*
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C*                    CAS            PREOTH
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* CHGSTS - Change status of item
     C*================================================================
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C     DISTAT        IFEQ      'I'
     C                   MOVEL     'A'           DISTAT
     C                   ELSE
     C                   MOVEL     'I'           DISTAT
     C                   ENDIF
     C                   EXSR      UPDFLD
E01  C                   ENDIF
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C                   Z-ADD     1             SCRN#
     C*
     C*** BEG DEL *** 01/07/99 **************************
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
     C*** END DEL *** 01/07/99 **************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C     SCICD9        IFEQ      *BLANKS
     C                   SETON                                        8990
     C                   ENDIF
     C*** BEGIN ADD *** 08/04/03 **************************************
     C     SCLOST        IFEQ      'D'
     C                   MOVEL(P)  SCICD9        KKICD9
     C                   ELSE
     C                   MOVEL(P)  SCICD9        XXONE             1
     C     XXONE         IFEQ      *BLANKS
     C                   MOVEL(P)  SCICD9        KKICD9
     C                   ELSE
     C                   MOVEL(P)  SCICD9        XXFOUR            4
     C                   MOVE(P)   XXFOUR        KKICD9
     C                   ENDIF
     C                   ENDIF
     C*
     C* MUST BE A VALID LOST TYPE
     C*
     C     *IN40         IFEQ      *ON
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLOST        ANDNE     'D'
     C     SCLOST        ANDNE     'O'
     C                   SETON                                        8690
     C                   ENDIF
     C*
     C* CHECK FOR DUPLICATES
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCLOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        8590
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 08/04/03 **************************************
     C     *IN90         IFEQ      *OFF
     C     SCDSC1        ANDEQ     *BLANKS
     C                   SETON                                        8890
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C     SCDS1         ANDEQ     *BLANKS
     C                   SETON                                        8790
     C                   ENDIF
     C*** BEGIN DEL *** 08/04/03 **************************************
     C********** *IN90     IFEQ *OFF
     C********** SCLOST    ANDNE'D'
     C********** SCLOST    ANDNE'O'
     C**********           SETON                     8690
     C**********           ENDIF
     C*** END   DEL *** 08/04/03 **************************************
     C*
     C                   Z-ADD     1             SCRN#             1 0
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
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     DIICD9        SCICD9
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     DILGRP        SCLGRP
     C                   MOVEL     DILOST        SCLOST
     C                   MOVEL     DISTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCICD9
     C                   MOVEL     *BLANKS       SCDSC1
     C                   MOVEL     *BLANKS       SCDS1
     C                   MOVEL     *BLANKS       SCLGRP
     C                   MOVEL     *BLANKS       SCLOST
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*** BEGIN CHG *** 08/04/03 **************************************
     C**********           MOVELSCICD9    DIICD9
     C                   MOVEL(P)  KKICD9        DIICD9
     C*** END   CHG *** 08/04/03 **************************************
     C                   MOVEL     SCDSC1        DIDSC1
     C                   MOVEL     SCDS1         DIDS1
     C                   MOVEL     SCLGRP        DILGRP
     C                   MOVEL     SCLOST        DILOST
     C                   MOVEL     SCSTAT        DISTAT
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
     C***  Begin Add  ***  05/20/09  **************************************
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@KC          ##KC
     C                   ENDIF
     C     Q1PRMS        IFGE      2
     C                   MOVE      @@KL          ##KL
     C                   ENDIF
     C     Q1PRMS        IFGE      3
     C                   MOVE      @@MODE        ##MODE
     C                   ENDIF
     C     Q1PRMS        IFGE      4
     C                   MOVE      @@RRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'S2010R02'
     C                   PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM      'MC'          ##AREA            2
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C***  End   Add  ***  05/20/09  **************************************
     C*
     C     @@MODE        COMP      'C'                                    10
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C*                    Z-ADD1         #OFSCR  10
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C     @@MODE        IFEQ      'S'
     C                   EXSR      CHGSTS
     C*** BEGIN ADD *** 08/26/98 **************************************
     C                   RETURN
     C*** END   ADD *** 08/26/98 **************************************
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   EXSR      UPDFLD
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDFLD        BEGSR
     C                   MOVEL     Q1PGM         DIPGM
     C                   MOVEL     Q1USER        DIUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           CALL 'SRCVTDAT'DTYMD
     C*** END   DEL *** 12/02/97 **************************************
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** END   CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         DIDT
     C                   MOVEL     DSTIME        DITIME
     C                   UPDATE    DBRCDF
     C                   EXSR      CALLR                                        DEL 01/07/99
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVE      'A'           DISTAT
     C                   MOVEL     Q1PGM         DIPGM
     C                   MOVEL     Q1USER        DIIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           CALL 'SRCVTDAT'DTYMD
     C*** END   DEL *** 12/02/97 **************************************
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** END   CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         DIIDT
     C*** BEGIN ADD *** 08/04/03 **************************************
     C                   MOVEL     SRDTO         DIDT
     C                   Z-ADD     DSTIME        DITIME
     C                   MOVEL(P)  Q1USER        DIUSER
     C*** END   ADD *** 08/04/03 **************************************
     C                   WRITE     DBRCDF
     C                   FEOD      FMDIP
     C                   EXSR      CALLR                                        DEL 01/07/99
     C                   ENDSR
