     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************************
     H* Revisions
     H* 08/04/16 C:079 Support: 42706/099
     H*                Load State and Protect on Add/Change
     H**********************************************************************
     FSTABL     IF   E           K DISK
     F*** Beg Add *** 08/04/16 *********************************************
     FSTSTP     IF   E             DISK    rename(pST01:pST02)
     F*** End Add *** 08/04/16 *********************************************
     FF2401F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDRUP     UF A E             DISK
     F                                     RENAME(PRU01:DBRCDF)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WORK WITH STATE ADJ-
     D                                     UDICATION PROGRAMS')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D** Data structure for XXEFDT
     D                 DS
     D  XXEFDT                 1      8
     D  XXMON                  1      2
     D  XXDAY                  3      4
     D  XXYR                   5      8

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*** Beg Add *** 08/04/16 ***************************************
     C                   PARM                    @@RRN2            9 0
     C*** End'Add *** 08/04/16 ***************************************
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
     C     STKY          KLIST
     C                   KFLD                    KKST1            10
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
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
     C*****************************************************************
     C*** Beg Del *** 08/04/16 ****************************************
     C*******
     C****VERIFY STATE IS VALID
     C*******
     C**N90SCST          IFNE      *BLANKS
     C*******              SETOF                     43
     C*******            MOVEL(P)  SCST          KKST1
     C*****STKY          CHAIN     PST01                              99
     C***99**            SETON                                        9041
     C*******            ELSE
     C*******              SETOF                     41
     C*******            SETON                                        9043
     C*******            ENDIF
     C*** End Del *** 08/04/16 ****************************************
     C*****************************************************************
     C*   CHECK EFFECTIVE DATE FOR VALIDITY AND BLANK
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9048                  OR
     C                   ENDIF
     C                   ENDIF
     C*****************************************************************
     C*   VERIFY ADJ CODE IS VALID
     C*
     C  N90SCATYP        IFEQ      *BLANKS
     C                   SETON                                        9044
     C                   ELSE
     C     SCATYP        IFNE      'I'
     C     SCATYP        ANDNE     'D'
     C     SCATYP        ANDNE     'X'
     C     SCATYP        ANDNE     'H'
     C     SCATYP        ANDNE     'P'
     C     SCATYP        ANDNE     'A'
     C     SCATYP        ANDNE     'N'
     C     SCATYP        ANDNE     *BLANKS
     C                   SETON                                        9042
     C                   ENDIF
     C                   ENDIF
     C*****************************************************************
     C*   VERIFY STATUS IS NOT BLANK
     C*
     C  N90SCSTAT        IFEQ      *BLANK
     C                   SETON                                        9045
     C                   ENDIF
     C*****************************************************************
     C*   VERIFY ADJUDICATION PROGRAM IS NOT BLANK
     C*
     C  N90SCAPGM        IFEQ      *BLANK
     C                   SETON                                        9046
     C                   ENDIF
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
     C                   Z-ADD     RUEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C*          XXMON     CAT  XXDAY:0   XXEFDT
     C*                    CAT  XXYR:0    XXEFDT
     C*                    CAT  '/':0     SCEFDT
     C*                    CAT  XXYR:0    SCEFDT
     C*
     C** MOVE DATA BASE FIELDS TO SCREEN FEILDS
     C*
     C                   MOVEL     RUST          SCST
     C                   MOVEL     XXEFDT        SCEFDT
     C                   MOVEL     RUAPGM        SCAPGM
     C                   MOVEL     RUSTAT        SCSTAT
     C                   MOVEL     RUATYP        SCATYP
     C*** Beg Add *** 08/04/16 ****************************************
     C     @@rrn2        Chain     pSt02                              99
     C     *in99         ifeq      *off
     C                   eval      scST = STkey
     C                   endif
     C*** End Add *** 08/04/16 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RUEFDT
     C                   MOVE      SCST          RUST
     C                   MOVE      SCATYP        RUATYP
     C                   MOVE      SCAPGM        RUAPGM
     C                   MOVE      SCSTAT        RUSTAT
     C                   ENDIF
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
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     @@MODE        IFEQ      'A'
     C                   SETON                                        70
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL(P)  Q1USER        RUUSER
     C                   MOVEL(P)  Q1PGM         RUPGM
     C                   MOVEL     DSTIME        RUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   Z-ADD     XXDATE        RUDT
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
     C                   MOVEL(P)  Q1USER        RUUSER
     C                   MOVEL(P)  Q1PGM         RUPGM
     C                   MOVEL     DSTIME        RUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     XXDATE        RUDT
     C                   MOVEL(P)  Q1USER        RUIUSR
     C                   Z-ADD     XXDATE        RUIDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FDRUP
     C                   ENDSR
