     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  PGMNAME  - WHAT IT'S CALLED        C:XX XX/XX/XX             *
     H*                                     PROJ:XXXXX                *
     H*  CALLED FROM: SOMEWHERE                                       *
     H*  CALLS TO:    SOMETHING                                       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/02/01 C:62 PROJECT: MCMNT    BASE ONLY                    *
     H*                COMPILE ONLY                                   *
     H*                                                               *
     H*  12/08/99 C:62 PROJECT: Y2K                                   *
     H*                ASSIGN VALUE TO INITIAL PROGRAM.               *
     H*                                                               *
     H*  08/31/99 C:62 PROJECT: MC211                                 *
     H*                CHANGE THE THIRTY 70A FIELDS TO TWENTY 105AS.  *
     H*                                                               *
     H*****************************************************************
     FF0063F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDF3P     UF A E             DISK
     F                                     RENAME(PF301:DBRCDF)
     FFMFNP     IF   E             DISK
     FFMSML     IF   E           K DISK
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
     D OPTNM           C                   CONST('Group EOMB Comments')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FMFRP
     C                   PARM                    @@RRN2            9 0          FMSMP
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
     C     F3KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEFDT
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    FNEFDT        KKEFDT
     C*
     C     *LIKE         DEFINE    F3EC01        KKEC01
     C     *LIKE         DEFINE    F3EC02        KKEC02
     C     *LIKE         DEFINE    F3EC03        KKEC03
     C     *LIKE         DEFINE    F3EC04        KKEC04
     C     *LIKE         DEFINE    F3EC05        KKEC05
     C     *LIKE         DEFINE    F3EC06        KKEC06
     C     *LIKE         DEFINE    F3EC07        KKEC07
     C     *LIKE         DEFINE    F3EC08        KKEC08
     C     *LIKE         DEFINE    F3EC09        KKEC09
     C     *LIKE         DEFINE    F3EC10        KKEC10
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
     C     F3STAT        IFEQ      'I'
     C                   MOVE      'A'           F3STAT
     C                   ELSE
     C                   MOVE      'I'           F3STAT
     C                   ENDIF                                                  F3STAT
     C*
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F3DT
     C*
     C                   MOVEL     DSTIME        F3TIME
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
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C* CLEAR SCREEN DATA
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
     C                   Z-ADD     FNCO#         F3CO#
     C                   Z-ADD     FNSUB#        F3SUB#
     C                   Z-ADD     FNFND         F3FND
     C*
     C                   Z-ADD     F3EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C*** BEGIN CHG *** 08/31/99 **************************************
     C**********           MOVELF3EC01    SCEC01
     C**********           MOVELF3EC02    SCEC02
     C**********           MOVELF3EC03    SCEC03
     C**********           MOVELF3EC04    SCEC04
     C**********           MOVELF3EC05    SCEC05
     C**********           MOVELF3EC06    SCEC06
     C**********           MOVELF3EC07    SCEC07
     C**********           MOVELF3EC08    SCEC08
     C**********           MOVELF3EC09    SCEC09
     C**********           MOVELF3EC10    SCEC10
     C**********           MOVELF3EC11    SCEC11
     C**********           MOVELF3EC12    SCEC12
     C**********           MOVELF3EC13    SCEC13
     C**********           MOVELF3EC14    SCEC14
     C**********           MOVELF3EC15    SCEC15
     C**********           MOVELF3EC16    SCEC16
     C**********           MOVELF3EC17    SCEC17
     C**********           MOVELF3EC18    SCEC18
     C**********           MOVELF3EC19    SCEC19
     C**********           MOVELF3EC20    SCEC20
     C*
     C                   MOVEL(P)  F3EC01        XXPRTA
     C                   MOVEL(P)  F3EC02        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN01
     C*
     C                   MOVE(P)   F3EC02        XXPRTC
     C                   MOVEL(P)  F3EC03        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN02
     C*
     C                   MOVEL(P)  F3EC04        XXPRTA
     C                   MOVEL(P)  F3EC05        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN03
     C*
     C                   MOVE(P)   F3EC05        XXPRTC
     C                   MOVEL(P)  F3EC06        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN04
     C*
     C                   MOVEL(P)  F3EC07        XXPRTA
     C                   MOVEL(P)  F3EC08        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN05
     C*
     C                   MOVE(P)   F3EC08        XXPRTC
     C                   MOVEL(P)  F3EC09        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN06
     C*
     C                   MOVEL(P)  F3EC10        XXPRTA
     C                   MOVEL(P)  F3EC11        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN07
     C*
     C                   MOVE(P)   F3EC11        XXPRTC
     C                   MOVEL(P)  F3EC12        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN08
     C*
     C                   MOVEL(P)  F3EC13        XXPRTA
     C                   MOVEL(P)  F3EC14        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN09
     C*
     C                   MOVE(P)   F3EC14        XXPRTC
     C                   MOVEL(P)  F3EC15        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN10
     C*
     C                   MOVEL(P)  F3EC16        XXPRTA
     C                   MOVEL(P)  F3EC17        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN11
     C*
     C                   MOVE(P)   F3EC17        XXPRTC
     C                   MOVEL(P)  F3EC18        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN12
     C*
     C                   MOVEL(P)  F3EC19        XXPRTA
     C                   MOVEL(P)  F3EC20        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN13
     C*
     C                   MOVE(P)   F3EC20        XXPRTC
     C                   MOVEL(P)  F3EC21        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN14
     C*
     C                   MOVEL(P)  F3EC22        XXPRTA
     C                   MOVEL(P)  F3EC23        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN15
     C*
     C                   MOVE(P)   F3EC23        XXPRTC
     C                   MOVEL(P)  F3EC24        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN16
     C*
     C                   MOVEL(P)  F3EC25        XXPRTA
     C                   MOVEL(P)  F3EC26        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN17
     C*
     C                   MOVE(P)   F3EC26        XXPRTC
     C                   MOVEL(P)  F3EC27        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN18
     C*
     C                   MOVEL(P)  F3EC28        XXPRTA
     C                   MOVEL(P)  F3EC29        XXPRTB
     C                   MOVEL(P)  XXLIN1        SCLN19
     C*
     C                   MOVE(P)   F3EC29        XXPRTC
     C                   MOVEL(P)  F3EC30        XXPRTD
     C                   MOVEL(P)  XXLIN2        SCLN20
     C*
     C*** END   CHG *** 08/31/99 **************************************
     C**********
     C*** BEGIN DEL *** 08/31/99 **************************************
     C**********           MOVELF3EC21    SCEC21
     C**********           MOVELF3EC22    SCEC22
     C**********           MOVELF3EC23    SCEC23
     C**********           MOVELF3EC24    SCEC24
     C**********           MOVELF3EC25    SCEC25
     C**********           MOVELF3EC26    SCEC26
     C**********           MOVELF3EC27    SCEC27
     C**********           MOVELF3EC28    SCEC28
     C**********           MOVELF3EC29    SCEC29
     C**********           MOVELF3EC30    SCEC30
     C*** END   DEL *** 08/31/99 **************************************
     C*
     C                   MOVEL     F3STAT        SCSTAT
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
     C                   Z-ADD     FNCO#         F3CO#
     C                   Z-ADD     FNSUB#        F3SUB#
     C                   Z-ADD     FNFND         F3FND
     C*
     C* Date Conversion
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F3EFDT
     C*
     C*** BEGIN CHG *** 08/31/99 **************************************
     C**********           MOVELSCEC01    F3EC01
     C**********           MOVELSCEC02    F3EC02
     C**********           MOVELSCEC03    F3EC03
     C**********           MOVELSCEC04    F3EC04
     C**********           MOVELSCEC05    F3EC05
     C**********           MOVELSCEC06    F3EC06
     C**********           MOVELSCEC07    F3EC07
     C**********           MOVELSCEC08    F3EC08
     C**********           MOVELSCEC09    F3EC09
     C**********           MOVELSCEC10    F3EC10
     C**********           MOVELSCEC11    F3EC11
     C**********           MOVELSCEC12    F3EC12
     C**********           MOVELSCEC13    F3EC13
     C**********           MOVELSCEC14    F3EC14
     C**********           MOVELSCEC15    F3EC15
     C**********           MOVELSCEC16    F3EC16
     C**********           MOVELSCEC17    F3EC17
     C**********           MOVELSCEC18    F3EC18
     C**********           MOVELSCEC19    F3EC19
     C**********           MOVELSCEC20    F3EC20
     C*
     C                   MOVEL(P)  SCLN01        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC01
     C                   MOVEL(P)  XXPRTB        F3EC02
     C*
     C                   MOVEL(P)  SCLN02        XXLIN2
     C                   MOVE      XXPRTC        F3EC02
     C                   MOVEL(P)  XXPRTD        F3EC03
     C*
     C                   MOVEL(P)  SCLN03        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC04
     C                   MOVEL(P)  XXPRTB        F3EC05
     C*
     C                   MOVEL(P)  SCLN04        XXLIN2
     C                   MOVE      XXPRTC        F3EC05
     C                   MOVEL(P)  XXPRTD        F3EC06
     C*
     C                   MOVEL(P)  SCLN05        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC07
     C                   MOVEL(P)  XXPRTB        F3EC08
     C*
     C                   MOVEL(P)  SCLN06        XXLIN2
     C                   MOVE      XXPRTC        F3EC08
     C                   MOVEL(P)  XXPRTD        F3EC09
     C*
     C                   MOVEL(P)  SCLN07        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC10
     C                   MOVEL(P)  XXPRTB        F3EC11
     C*
     C                   MOVEL(P)  SCLN08        XXLIN2
     C                   MOVE      XXPRTC        F3EC11
     C                   MOVEL(P)  XXPRTD        F3EC12
     C*
     C                   MOVEL(P)  SCLN09        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC13
     C                   MOVEL(P)  XXPRTB        F3EC14
     C*
     C                   MOVEL(P)  SCLN10        XXLIN2
     C                   MOVE      XXPRTC        F3EC14
     C                   MOVEL(P)  XXPRTD        F3EC15
     C*
     C                   MOVEL(P)  SCLN11        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC16
     C                   MOVEL(P)  XXPRTB        F3EC17
     C*
     C                   MOVEL(P)  SCLN12        XXLIN2
     C                   MOVE      XXPRTC        F3EC17
     C                   MOVEL(P)  XXPRTD        F3EC18
     C*
     C                   MOVEL(P)  SCLN13        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC19
     C                   MOVEL(P)  XXPRTB        F3EC20
     C*
     C                   MOVEL(P)  SCLN14        XXLIN2
     C                   MOVE      XXPRTC        F3EC20
     C                   MOVEL(P)  XXPRTD        F3EC21
     C*
     C                   MOVEL(P)  SCLN15        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC22
     C                   MOVEL(P)  XXPRTB        F3EC23
     C*
     C                   MOVEL(P)  SCLN16        XXLIN2
     C                   MOVE      XXPRTC        F3EC23
     C                   MOVEL(P)  XXPRTD        F3EC24
     C*
     C                   MOVEL(P)  SCLN17        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC25
     C                   MOVEL(P)  XXPRTB        F3EC26
     C*
     C                   MOVEL(P)  SCLN18        XXLIN2
     C                   MOVE      XXPRTC        F3EC26
     C                   MOVEL(P)  XXPRTD        F3EC27
     C*
     C                   MOVEL(P)  SCLN19        XXLIN1
     C                   MOVEL(P)  XXPRTA        F3EC28
     C                   MOVEL(P)  XXPRTB        F3EC29
     C*
     C                   MOVEL(P)  SCLN20        XXLIN2
     C                   MOVE      XXPRTC        F3EC29
     C                   MOVEL(P)  XXPRTD        F3EC30
     C*
     C*** END   CHG *** 08/31/99 **************************************
     C*** BEGIN DEL *** 08/31/99 **************************************
     C**********           MOVELSCEC21    F3EC21
     C**********           MOVELSCEC22    F3EC22
     C**********           MOVELSCEC23    F3EC23
     C**********           MOVELSCEC24    F3EC24
     C**********           MOVELSCEC25    F3EC25
     C**********           MOVELSCEC26    F3EC26
     C**********           MOVELSCEC27    F3EC27
     C**********           MOVELSCEC28    F3EC28
     C**********           MOVELSCEC29    F3EC29
     C**********           MOVELSCEC30    F3EC30
     C*** END   DEL *** 08/31/99 **************************************
     C*
     C                   MOVEL     SCSTAT        F3STAT
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
     C     @@RRN2        CHAIN     PFN01                              99
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNSUB#        KKSUB#
     C                   Z-ADD     FNFND         KKFND
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSNAM
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
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F3DT
     C*
     C                   MOVEL     DSTIME        F3TIME
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
     C                   MOVEL     Q1USER        F3USER
     C                   MOVEL     Q1PGM         F3PGM
     C*** BEGIN ADD *** 12/08/99 **************************************
     C                   MOVEL     Q1PGM         F3IPGM
     C*** END   ADD *** 12/08/99 **************************************
     C                   MOVEL     Q1USER        F3IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F3DT
     C*
     C                   MOVEL     SRDTO         F3IDT
     C*
     C                   MOVEL     DSTIME        F3TIME
     C*** BEGIN ADD *** 12/08/99 **************************************
     C                   Z-ADD     DSTIME        F3ITIM
     C*** END   ADD *** 12/08/99 **************************************
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FDF3P
     C                   ENDSR
