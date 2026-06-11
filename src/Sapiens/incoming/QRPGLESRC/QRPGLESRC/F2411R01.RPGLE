     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* PROJECT: MC234  WORK WITH BEAST RECORD
     H* 09/27/00  C:117
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/04/16 C:079 Support: 42706/099
     H*                Load State and Protect on Add/Change
     H*
     H* 03/11/10  C:227  PROJECT: FL083
     H*                  ADDED EDIT AND SEARCH FOR MEIG REVISION
     H*                  INDICATOR
     H*
     H* 04/15/04  C: 79  Project: FL046
     H*                  Allow E for EDI default for Florida
     H*****************************************************************
     FF2411F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMBEP     UF A E             DISK
     F                                     RENAME(PBE01:DBRCDF)
     F*
     F* State files
     FSTSTP     IF   E             DISK
     FSTSTL     IF   E           K DISK
     F                                     RENAME(PST01:PST02)
     F*
     F*  Generic Table FIle
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*
     F*  Fee Schedule Claims Status Code File
     FFTABL     IF   E           K DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('BEAST Record')
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
     C                   PARM                    @@CO#             3 0
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
     C*
     C*  STATE PARM LIST
     C*
     C     STST          PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C*
     C** GENERIC TABLE FILE PARM LIST
     C*
     C     GTPLT         PLIST
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##TBID            4
     C*
     C** CLAIM STATUS CODE TABLE FILE PARM LIST
     C*
     C     CSPLT         PLIST
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* System State file
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY            10
     C*
     C** KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKTBID            4
     C                   KFLD                    KKKEY            10
     C*
     C** KEY LIST FOR GENERIC TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKKEY            10
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
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
     C     BESTAT        IFEQ      'I'
     C                   MOVE      'A'           BESTAT
     C                   ELSE
     C                   MOVE      'I'           BESTAT
     C                   ENDIF                                                  CVSTAT
     C*
     C                   MOVEL     Q1USER        BEUSER
     C                   MOVEL     Q1PGM         BEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BEDT
     C*
     C                   MOVEL     DSTIME        BETIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
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
     C*
     C*  Adj State is required and validated
     C*
     C     SCAST         IFEQ      *BLANKS
     C                   SETON                                        7090
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     *BLANKS       KKKEY
     C                   MOVEL     SCAST         KKKEY
     C     STKY          CHAIN     PST02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7190
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  System is required and validated
     C*
     C     SCSYS         IFEQ      *BLANKS
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     *BLANKS       KKKEY
     C                   MOVEL     'SYS '        KKTBID
     C                   MOVEL     SCSYS         KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7390
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Bill Type is required and validated
     C*
     C     SCBTYP        IFEQ      *BLANKS
     C                   SETON                                        7490
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     *BLANKS       KKKEY
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7590
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  <> *BLANKS
     C*
     C*  Bill Sub-Type is validated
     C*
     C     SCBTYP        IFEQ      'H'
     C     SCSTYP        ANDNE     *BLANKS
     C                   SETON                                        7690
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*  VALIDATE BILL SUBTYPE IF ENTERED
     C*
     C     SCSTYP        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       KKKEY
     C                   MOVEL(P)  'STYP'        KKTBID
     C                   MOVEL(P)  SCSTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7790
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCSTYP IFNE *BLANKS
     C*
     C*  Payment Plan is validated
     C*
     C     SCPPLN        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       KKKEY
     C*** BEG ADD *** 04/15/04 ****************************************
     C*******************  MOVEL'FLPP'    KKTBID    P
     C                   MOVEL(P)  'FLPA'        KKTBID
     C*** END ADD *** 04/15/04 ****************************************
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7890
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C*  EDI Default values: Y, N, or blank
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'N'
     C     SCEDI         ANDNE     'Y'
     C*** BEG ADD *** 04/15/04 ****************************************
     C     SCAST         ANDNE     'FL'
     C*** END ADD *** 04/15/04 ****************************************
     C                   SETON                                        7990
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEG ADD *** 04/15/04 ****************************************
     C     SCAST         IFEQ      'FL'
     C     SCEDI         ANDNE     *BLANKS
     C     SCEDI         ANDNE     'N'
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'E'
     C                   SETON                                        8490
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C*  Default adj code is validated
     C*
     C     SCADCD        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       KKKEY
     C                   MOVEL(P)  SCADCD        KKKEY
     C     TBKY          CHAIN     PCS02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        8090
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  <> *BLANKS  ON
     C*
     C     SCHEPG        IFEQ      *BLANKS
     C                   SETON                                        8190
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCHESM        IFEQ      *BLANKS
     C                   SETON                                        8290
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDEPG        IFEQ      *BLANKS
     C                   SETON                                        8390
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*** BEG ADD *** 03/11/10 ****************************************
     C     SCREVI        IFEQ      *BLANKS
     C     SCAST         ANDEQ     'FL'
     C                   SETON                                        424390
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCREVI        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       KKKEY
     C                   MOVEL     'REVI'        KKTBID
     C                   MOVEL     SCREVI        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        424490
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 03/11/10 ****************************************
     C     CK1END        ENDSR
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C** ADJUDICATION STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCAST'
     C*
     C                   MOVEL     SCAST         HLAST             2
     C*
     C                   CALL      'F0601R'      STST
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLAST         SCAST
     C                   SETON                                        9032      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PST01                              99
     C                   MOVEL(P)  STKEY         SCAST
     C                   SETON                                        9036      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C*  SYSTEM SEARCH
     C*
     C     HFLD          WHENEQ    'SCSYS'
     C                   MOVEL     SCSYS         HLSYS             1
     C                   MOVEL(P)  'SYS'         ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLSYS         SCSYS
     C                   SETON                                        9033      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCSYS
     C                   SETON                                        9039      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C*  BILL TYPE SEARCH
     C*
     C     HFLD          WHENEQ    'SCBTYP'
     C                   MOVEL     SCBTYP        HLBTYP            1
     C                   MOVEL(P)  'BTYP'        ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLBTYP        SCBTYP
     C                   SETON                                        9034      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCBTYP
     C                   SETON                                        9037      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C*  BILL SUB-TYPE SEARCH
     C*
     C     HFLD          WHENEQ    'SCSTYP'
     C                   MOVEL     SCSTYP        HLSTYP            2
     C                   MOVEL(P)  'STYP'        ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLSTYP        SCSTYP
     C                   SETON                                        9035      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCSTYP
     C*** BEG CHG *** 03/11/10 ****************************************
     C***********          SETON                     9038  pos cursor
     C                   SETON                                        9042      pos cursor
     C*** BEG CHG *** 03/11/10 ****************************************
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C*** BEG ADD *** 03/11/10 ****************************************
     C*
     C*  MEIG INDICATOR SEARCH
     C*
     C     HFLD          WHENEQ    'SCREVI'
     C                   MOVEL     SCREVI        HLREVI            1
     C                   MOVEL(P)  'REVI'        ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLREVI        SCREVI
     C                   SETON                                        9042      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCREVI
     C                   SETON                                        9038      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*** END ADD *** 03/11/10 ****************************************
     C*  PAYMENT PLAN SEARCH
     C*
     C     HFLD          WHENEQ    'SCPPLN'
     C                   MOVEL     SCPPLN        HLPPLN            2
     C                   MOVEL(P)  'FLPP'        ##TBID
     C*** BEG ADD *** 04/15/04 ****************************************
     C*******************  MOVEL'FLPP'    ##TBID    P
     C                   MOVEL(P)  'FLPA'        ##TBID
     C*** END ADD *** 04/15/04 ****************************************
     C                   CALL      'F2380R'      GTPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLPPLN        SCPPLN
     C                   SETON                                        9036
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPPLN
     C                   SETON                                        9033
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*  DEFAULT ADJ CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCADCD'
     C                   MOVEL     SCADCD        HLADCD            3
     C                   CALL      'F2300R'      CSPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLADCD        SCADCD
     C                   SETON                                        9037      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PCS01                              99
     C                   MOVEL(P)  CSKEY         SCADCD
     C                   SETON                                        9035      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* CLEAR FIELDS ON AN ADD
     C*
     C                   MOVE      *BLANKS       SCAST
     C                   MOVE      *BLANKS       SCSYS
     C                   MOVE      *BLANKS       SCBTYP
     C                   MOVE      *BLANKS       SCSTYP
     C                   MOVE      *BLANKS       SCPPLN
     C                   MOVE      *BLANKS       SCEDI
     C                   MOVE      *BLANKS       SCADCD
     C                   MOVE      *BLANKS       SCHEPG
     C                   MOVE      *BLANKS       SCHESM
     C                   MOVE      *BLANKS       SCDEPG
     C                   MOVE      *BLANKS       SCEDHD
     C                   MOVE      *BLANKS       SCEDDT
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVE      *BLANKS       SCREVI
     C*** END ADD *** 03/11/10 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     BEAST         SCAST
     C                   MOVEL     BESYS         SCSYS
     C                   MOVEL     BEBTYP        SCBTYP
     C                   MOVEL     BESTYP        SCSTYP
     C                   MOVEL     BEPPLN        SCPPLN
     C                   MOVEL     BEEDI         SCEDI
     C                   MOVEL     BEADCD        SCADCD
     C                   MOVEL     BEHEPG        SCHEPG
     C                   MOVEL     BEHESM        SCHESM
     C                   MOVEL     BEDEPG        SCDEPG
     C                   MOVEL     BEEDHD        SCEDHD
     C                   MOVEL     BEEDDT        SCEDDT
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL     BEREVI        SCREVI
     C*** END ADD *** 03/11/10 ****************************************
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*** BEG ADD *** 03/11/10 ****************************************
     C     SCAST         IFEQ      'FL'
     C                   SETON                                        45
     C                   ENDIF                                                  *IN40=*ON
     C*** END ADD *** 03/11/10 ****************************************
     C*** Beg Add *** 08/04/16 ****************************************
     C     @@rrn2        Chain     pSt01                              99
     C     *in99         ifeq      *off
     C                   eval      scAST = STkey
     C                   endif
     C*** End Add *** 08/04/16 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCAST         BEAST
     C                   MOVEL     SCSYS         BESYS
     C                   MOVEL     SCBTYP        BEBTYP
     C                   MOVEL     SCSTYP        BESTYP
     C                   MOVEL     SCPPLN        BEPPLN
     C                   MOVEL     SCEDI         BEEDI
     C                   MOVEL     SCADCD        BEADCD
     C                   MOVEL     SCHEPG        BEHEPG
     C                   MOVEL     SCHESM        BEHESM
     C                   MOVEL     SCDEPG        BEDEPG
     C                   MOVEL     SCEDHD        BEEDHD
     C                   MOVEL     SCEDDT        BEEDDT
     C                   MOVEL     'A'           BESTAT
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   MOVEL     SCREVI        BEREVI
     C*** END ADD *** 03/11/10 ****************************************
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
     C     *INKJ         CASEQ     *ON           KJDO                           F10=Search
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETOFF                                       094041
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*
     C                   Z-ADD     @@CO#         KKCO#
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
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMBEP
     C                   ENDSR
