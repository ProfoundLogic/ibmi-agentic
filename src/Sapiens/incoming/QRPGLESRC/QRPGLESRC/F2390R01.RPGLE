     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F2390R01 - STATE EOMB COMMENTS UPDATE/DISPLAY                *
     H*                                                               *
     H*  CALLED FROM: F2390R - WORK WITH STATE EOMB COMMENTS          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  09/14/07 C:47 PROJECT FL069
     H*                ADDED ABILITY TO PRINT COMMENTS IN BOLD FONT
     H*                BY ADDING  FIELD (F4BLD1) TO FDF4P.
     H*
     H*  01/02/01 C:62 PROJECT: MCMNT    BASE ONLY                    *
     H*                COMPILE ONLY                                   *
     H*                                                               *
     H*  08/31/99 C:62 PROJECT: MC211                                 *
     H*                CHANGE THE THIRTY 70A FIELDS TO TWENTY 105AS.  *
     H*                                                               *
     H*****************************************************************
     FF2390F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDF4P     UF A E             DISK
     F                                     RENAME(PF401:DBRCDF)
     FSTABL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*** BEGIN ADD *** 08/31/99 **************************************
     D                 DS
     D  XXLIN1                 1    105
     D  XXPRTA                 1     70
     D  XXPRTB                71    105
     D                 DS
     D  XXLIN2                 1    105
     D  XXPRTC                 1     35
     D  XXPRTD                36    105
     D*** END   ADD *** 08/31/99 **************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State EOMB Comments')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
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
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    F4EFDT        KKEFDT
     C     *LIKE         DEFINE    F4EC01        KKEC01
     C     *LIKE         DEFINE    F4EC02        KKEC02
     C     *LIKE         DEFINE    F4EC03        KKEC03
     C     *LIKE         DEFINE    F4EC04        KKEC04
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    F4EC05        KKEC05
     C     *LIKE         DEFINE    F4EC06        KKEC06
     C     *LIKE         DEFINE    F4EC07        KKEC07
     C     *LIKE         DEFINE    F4EC08        KKEC08
     C     *LIKE         DEFINE    F4EC09        KKEC09
     C     *LIKE         DEFINE    F4EC10        KKEC10
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTING FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     F4STAT        IFEQ      'I'
     C                   MOVE      'A'           F4STAT
     C                   ELSE
     C                   MOVE      'I'           F4STAT
     C                   ENDIF                                                  F4STAT
     C*
     C                   MOVEL     Q1USER        F4USER
     C                   MOVEL     Q1PGM         F4PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F4DT
     C*
     C                   MOVEL     DSTIME        F4TIME
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
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C* Check Effective Date for Validity and Blanks
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C*
     C                   ENDIF
     C*
     C* Line 1 of EOMB Comments Must Not Be Blank. (If in Add Mode)
     C*
     C*** BEGIN CHG *** 08/31/99 **************************************
     C**N90***** SCEC01    IFEQ *BLANKS
     C  N90SCLN01        IFEQ      *BLANKS
     C*** END   CHG *** 08/31/99 **************************************
     C                   SETON                                        9072
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for screen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
      *
      * DISPLAY STATE CODE AND STATE NAME IN HEADER
      *
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
      *
     C     STKY          CHAIN     PST01                              99
      *
     C                   MOVEL(P)  STNAME        SCSTNM
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C* CLEAR SCREEN DATA
     C*   IF *IN40 IS ON, WE'RE IN ADD MODE.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      *ZEROS        SCEFDT
     C*** BEGIN CHG *** 08/31/99 **************************************
     C**********           MOVE *BLANKS   SCEC01
     C**********           MOVE *BLANKS   SCEC02
     C**********           MOVE *BLANKS   SCEC03
     C**********           MOVE *BLANKS   SCEC04
     C**********           MOVE *BLANKS   SCEC05
     C**********           MOVE *BLANKS   SCEC06
     C**********           MOVE *BLANKS   SCEC07
     C**********           MOVE *BLANKS   SCEC08
     C**********           MOVE *BLANKS   SCEC09
     C**********           MOVE *BLANKS   SCEC10
     C**********           MOVE *BLANKS   SCEC11
     C**********           MOVE *BLANKS   SCEC12
     C**********           MOVE *BLANKS   SCEC13
     C**********           MOVE *BLANKS   SCEC14
     C**********           MOVE *BLANKS   SCEC15
     C**********           MOVE *BLANKS   SCEC16
     C**********           MOVE *BLANKS   SCEC17
     C**********           MOVE *BLANKS   SCEC18
     C**********           MOVE *BLANKS   SCEC19
     C**********           MOVE *BLANKS   SCEC20
     C                   MOVE      *BLANKS       SCLN01
     C                   MOVE      *BLANKS       SCLN02
     C                   MOVE      *BLANKS       SCLN03
     C                   MOVE      *BLANKS       SCLN04
     C                   MOVE      *BLANKS       SCLN05
     C                   MOVE      *BLANKS       SCLN06
     C                   MOVE      *BLANKS       SCLN07
     C                   MOVE      *BLANKS       SCLN08
     C                   MOVE      *BLANKS       SCLN09
     C                   MOVE      *BLANKS       SCLN10
     C                   MOVE      *BLANKS       SCLN11
     C                   MOVE      *BLANKS       SCLN12
     C                   MOVE      *BLANKS       SCLN13
     C                   MOVE      *BLANKS       SCLN14
     C                   MOVE      *BLANKS       SCLN15
     C                   MOVE      *BLANKS       SCLN16
     C                   MOVE      *BLANKS       SCLN17
     C                   MOVE      *BLANKS       SCLN18
     C                   MOVE      *BLANKS       SCLN19
     C                   MOVE      *BLANKS       SCLN20
     C*** END   CHG *** 08/31/99 **************************************
     C**********
     C*** BEGIN DEL *** 08/31/99 **************************************
     C**********           MOVE *BLANKS   SCEC21
     C**********           MOVE *BLANKS   SCEC22
     C**********           MOVE *BLANKS   SCEC23
     C**********           MOVE *BLANKS   SCEC24
     C**********           MOVE *BLANKS   SCEC25
     C**********           MOVE *BLANKS   SCEC26
     C**********           MOVE *BLANKS   SCEC27
     C**********           MOVE *BLANKS   SCEC28
     C**********           MOVE *BLANKS   SCEC29
     C**********           MOVE *BLANKS   SCEC30
     C*** END   DEL *** 08/31/99 **************************************
     C                   MOVE      'A'           SCSTAT            1
     C*
     C                   ELSE
     C*
     C*
     C* MOVE DATA TO SCREEN
     C*
     C* Date Conversion
     C*
     C                   Z-ADD     F4EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*** BEGIN CHG *** 08/31/99 **************************************
     C**********           MOVELF4EC01    SCEC01
     C**********           MOVELF4EC02    SCEC02
     C**********           MOVELF4EC03    SCEC03
     C**********           MOVELF4EC04    SCEC04
     C**********           MOVELF4EC05    SCEC05
     C**********           MOVELF4EC06    SCEC06
     C**********           MOVELF4EC07    SCEC07
     C**********           MOVELF4EC08    SCEC08
     C**********           MOVELF4EC09    SCEC09
     C**********           MOVELF4EC10    SCEC10
     C**********           MOVELF4EC11    SCEC11
     C**********           MOVELF4EC12    SCEC12
     C**********           MOVELF4EC13    SCEC13
     C**********           MOVELF4EC14    SCEC14
     C**********           MOVELF4EC15    SCEC15
     C**********           MOVELF4EC16    SCEC16
     C**********           MOVELF4EC17    SCEC17
     C**********           MOVELF4EC18    SCEC18
     C**********           MOVELF4EC19    SCEC19
     C**********           MOVELF4EC20    SCEC20
     C*
     C                   MOVEL(P)  F4EC01        XXPRTA
     C                   MOVEL(P)  F4EC02        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN01
     C*
     C                   MOVE(P)   F4EC02        XXPRTC
     C                   MOVEL(P)  F4EC03        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN02
     C*
     C                   MOVEL(P)  F4EC04        XXPRTA
     C                   MOVEL(P)  F4EC05        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN03
     C*
     C                   MOVE(P)   F4EC05        XXPRTC
     C                   MOVEL(P)  F4EC06        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN04
     C*
     C                   MOVEL(P)  F4EC07        XXPRTA
     C                   MOVEL(P)  F4EC08        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN05
     C*
     C                   MOVE(P)   F4EC08        XXPRTC
     C                   MOVEL(P)  F4EC09        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN06
     C*
     C                   MOVEL(P)  F4EC10        XXPRTA
     C                   MOVEL(P)  F4EC11        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN07
     C*
     C                   MOVE(P)   F4EC11        XXPRTC
     C                   MOVEL(P)  F4EC12        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN08
     C*
     C                   MOVEL(P)  F4EC13        XXPRTA
     C                   MOVEL(P)  F4EC14        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN09
     C*
     C                   MOVE(P)   F4EC14        XXPRTC
     C                   MOVEL(P)  F4EC15        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN10
     C*
     C                   MOVEL(P)  F4EC16        XXPRTA
     C                   MOVEL(P)  F4EC17        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN11
     C*
     C                   MOVE(P)   F4EC17        XXPRTC
     C                   MOVEL(P)  F4EC18        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN12
     C*
     C                   MOVEL(P)  F4EC19        XXPRTA
     C                   MOVEL(P)  F4EC20        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN13
     C*
     C                   MOVE(P)   F4EC20        XXPRTC
     C                   MOVEL(P)  F4EC21        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN14
     C*
     C                   MOVEL(P)  F4EC22        XXPRTA
     C                   MOVEL(P)  F4EC23        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN15
     C*
     C                   MOVE(P)   F4EC23        XXPRTC
     C                   MOVEL(P)  F4EC24        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN16
     C*
     C                   MOVEL(P)  F4EC25        XXPRTA
     C                   MOVEL(P)  F4EC26        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN17
     C*
     C                   MOVE(P)   F4EC26        XXPRTC
     C                   MOVEL(P)  F4EC27        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN18
     C*
     C                   MOVEL(P)  F4EC28        XXPRTA
     C                   MOVEL(P)  F4EC29        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN19
     C*
     C                   MOVE(P)   F4EC29        XXPRTC
     C                   MOVEL(P)  F4EC30        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN20
     C*
     C*** END   CHG *** 08/30/99 **************************************
     C**********
     C*** BEGIN DEL *** 08/30/99 **************************************
     C**********           MOVELF4EC21    SCEC21
     C**********           MOVELF4EC22    SCEC22
     C**********           MOVELF4EC23    SCEC23
     C**********           MOVELF4EC24    SCEC24
     C**********           MOVELF4EC25    SCEC25
     C**********           MOVELF4EC26    SCEC26
     C**********           MOVELF4EC27    SCEC27
     C**********           MOVELF4EC28    SCEC28
     C**********           MOVELF4EC29    SCEC29
     C**********           MOVELF4EC30    SCEC30
     C*** END   DEL *** 08/30/99 **************************************
     C                   MOVEL     F4STAT        SCSTAT
     C*** BEG ADD *** 09/14/07 **************************************
     C                   MOVEL     F4BLD1        SCBLD1
     C*** END ADD *** 09/14/07 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*
     C* MOVE DATA FROM SCREEN TO PHYSICAL FILE
     C*
     C                   Z-ADD     ##CO#         F4CO#
     C                   MOVEL     @@ST          F4ST
     C*
     C* Date Conversion
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F4EFDT
     C*
     C*** BEGIN CHG *** 08/30/99 **************************************
     C**********           MOVELSCEC01    F4EC01
     C**********           MOVELSCEC02    F4EC02
     C**********           MOVELSCEC03    F4EC03
     C**********           MOVELSCEC04    F4EC04
     C**********           MOVELSCEC05    F4EC05
     C**********           MOVELSCEC06    F4EC06
     C**********           MOVELSCEC07    F4EC07
     C**********           MOVELSCEC08    F4EC08
     C**********           MOVELSCEC09    F4EC09
     C**********           MOVELSCEC10    F4EC10
     C**********           MOVELSCEC11    F4EC11
     C**********           MOVELSCEC12    F4EC12
     C**********           MOVELSCEC13    F4EC13
     C**********           MOVELSCEC14    F4EC14
     C**********           MOVELSCEC15    F4EC15
     C**********           MOVELSCEC16    F4EC16
     C**********           MOVELSCEC17    F4EC17
     C**********           MOVELSCEC18    F4EC18
     C**********           MOVELSCEC19    F4EC19
     C**********           MOVELSCEC20    F4EC20
     C*
     C                   MOVEL(P)  SCLN01        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC01
     C                   MOVEL(P)  XXPRTB        F4EC02
     C*
     C                   MOVEL(P)  SCLN02        XXLIN2
     C                   MOVE      XXPRTC        F4EC02
     C                   MOVEL(P)  XXPRTD        F4EC03
     C*
     C                   MOVEL(P)  SCLN03        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC04
     C                   MOVEL(P)  XXPRTB        F4EC05
     C*
     C                   MOVEL(P)  SCLN04        XXLIN2
     C                   MOVE      XXPRTC        F4EC05
     C                   MOVEL(P)  XXPRTD        F4EC06
     C*
     C                   MOVEL(P)  SCLN05        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC07
     C                   MOVEL(P)  XXPRTB        F4EC08
     C*
     C                   MOVEL(P)  SCLN06        XXLIN2
     C                   MOVE      XXPRTC        F4EC08
     C                   MOVEL(P)  XXPRTD        F4EC09
     C*
     C                   MOVEL(P)  SCLN07        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC10
     C                   MOVEL(P)  XXPRTB        F4EC11
     C*
     C                   MOVEL(P)  SCLN08        XXLIN2
     C                   MOVE      XXPRTC        F4EC11
     C                   MOVEL(P)  XXPRTD        F4EC12
     C*
     C                   MOVEL(P)  SCLN09        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC13
     C                   MOVEL(P)  XXPRTB        F4EC14
     C*
     C                   MOVEL(P)  SCLN10        XXLIN2
     C                   MOVE      XXPRTC        F4EC14
     C                   MOVEL(P)  XXPRTD        F4EC15
     C*
     C                   MOVEL(P)  SCLN11        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC16
     C                   MOVEL(P)  XXPRTB        F4EC17
     C*
     C                   MOVEL(P)  SCLN12        XXLIN2
     C                   MOVE      XXPRTC        F4EC17
     C                   MOVEL(P)  XXPRTD        F4EC18
     C*
     C                   MOVEL(P)  SCLN13        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC19
     C                   MOVEL(P)  XXPRTB        F4EC20
     C*
     C                   MOVEL(P)  SCLN14        XXLIN2
     C                   MOVE      XXPRTC        F4EC20
     C                   MOVEL(P)  XXPRTD        F4EC21
     C*
     C                   MOVEL(P)  SCLN15        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC22
     C                   MOVEL(P)  XXPRTB        F4EC23
     C*
     C                   MOVEL(P)  SCLN16        XXLIN2
     C                   MOVE      XXPRTC        F4EC23
     C                   MOVEL(P)  XXPRTD        F4EC24
     C*
     C                   MOVEL(P)  SCLN17        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC25
     C                   MOVEL(P)  XXPRTB        F4EC26
     C*
     C                   MOVEL(P)  SCLN18        XXLIN2
     C                   MOVE      XXPRTC        F4EC26
     C                   MOVEL(P)  XXPRTD        F4EC27
     C*
     C                   MOVEL(P)  SCLN19        XXLIN1
     C                   MOVEL(P)  XXPRTA        F4EC28
     C                   MOVEL(P)  XXPRTB        F4EC29
     C*
     C                   MOVEL(P)  SCLN20        XXLIN2
     C                   MOVE      XXPRTC        F4EC29
     C                   MOVEL(P)  XXPRTD        F4EC30
     C*
     C*** END   CHG *** 08/30/99 **************************************
     C*** BEGIN DEL *** 08/30/99 **************************************
     C**********           MOVELSCEC21    F4EC21
     C**********           MOVELSCEC22    F4EC22
     C**********           MOVELSCEC23    F4EC23
     C**********           MOVELSCEC24    F4EC24
     C**********           MOVELSCEC25    F4EC25
     C**********           MOVELSCEC26    F4EC26
     C**********           MOVELSCEC27    F4EC27
     C**********           MOVELSCEC28    F4EC28
     C**********           MOVELSCEC29    F4EC29
     C**********           MOVELSCEC30    F4EC30
     C*** END   DEL *** 08/30/99 **************************************
     C                   MOVEL     SCSTAT        F4STAT
     C*** BEG ADD *** 09/14/07 **************************************
     C                   MOVEL     SCBLD1        F4BLD1
     C*** END ADD *** 09/14/07 **************************************
     C*
     C                   ENDSR
     C*
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
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
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
     C                   MOVEL     Q1USER        F4USER
     C                   MOVEL     Q1PGM         F4PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F4DT
     C*
     C                   MOVEL     DSTIME        F4TIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        F4USER
     C                   MOVEL     Q1PGM         F4PGM
     C                   MOVEL     Q1PGM         F4IPGM
     C                   MOVEL     Q1USER        F4IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F4DT
     C*
     C                   MOVEL     SRDTO         F4IDT
     C*
     C                   MOVEL     DSTIME        F4TIME
     C                   MOVEL     DSTIME        F4ITIM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FDF4P
     C                   ENDSR
