     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  AUTO CLAIM LETTERS
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: W0971R02                                          *
     H*    CREATED: 04/22/03  C:53 PROJ: WC1121                       *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 11/25/13  C:282 Project: 129WSC                               *
     H*                 Added the new Lost Time Flag From and To.     *
     H*                 Added the Claim Status From and To.           *
     H*                 Added the And/Or for the relationship         *
     H*                 between the Lost time flag and Claim status   *
     H*                 information.                                  *
     H*                                                               *
     H* 04/13/07  C:33  Seq#: WA099/54243                             *
     H*                 Use standard cursor positioning on searches.  *
     H*****************************************************************
     FW0971F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWACLP     UF A E             DISK
     F                                     RENAME(PCL01:DBRCDF)
     FWMDOL     IF   E           K DISK
     FWMFNP     IF   E             DISK
     FSTSTL3    IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('AUTO CLAIM LETTERS')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D PTL             C                   CONST('DEPARTMENT/DOC ID SE-
     D                                     ARCH')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0          FUND
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
     C     DOKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    CLDPCD
     C                   KFLD                    CLFLDR
     C                   KFLD                    CLDCID
     C*
     C     DOKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    SCDPCD
     C                   KFLD                    SCFLDR
     C                   KFLD                    SCDCID
     C*
     C*  DEPT CODE TABLE FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    SCDPCD
      *
     C     OFKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKOFC#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DODEPT        KKDEPT
     C     *LIKE         DEFINE    OFOFC#        KKOFC#
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
     C*
     C*** STATE
     C*
     C     SCST          IFEQ      *BLANKS
     C                   SETON                                        707190
     C                   GOTO      ENDCK1
     C                   ENDIF
      *
     C                   MOVEL(P)  SCST          XXKEY            10
     C     XXKEY         CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7190
     C                   GOTO      ENDCK1
     C                   END
     C*
     C*** LOST TIME
     C*
      *** Beg DEL *****  11/25/13 ************************************
     C*****SCLTFL        IFEQ      *BLANKS
     C*****              SETON                                        707290
     C*****              GOTO      ENDCK1
     C*****              ENDIF
     C*
     C* Must be I, M, or R
     C*
     C*****SCLTFL        IFNE      'I'
     C*****SCLTFL        ANDNE     'M'
     C*****SCLTFL        ANDNE     'R'
     C*****              SETON                                        7290
     C*****              GOTO      ENDCK1
     C*****              ENDIF
      *** END DEL *****  11/25/13 ************************************
     C*
     C*** DEPT CODE
     C*
     C     SCDPCD        IFEQ      *BLANKS
     C                   SETON                                        707390
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*  THE DEPT CODE MUST BE VALID
     C*
     C     DEKEY         CHAIN     PDE01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        7390
     C                   GOTO      ENDCK1
     C                   END
     C*
     C*** FOLDER
     C*
     C     SCFLDR        IFEQ      *BLANKS
     C                   SETON                                        707590
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C                   MOVE      ' '           QQRTCD            1
     C                   CALL      'SVALFLR'
     C                   PARM                    SCFLDR
     C                   PARM                    QQRTCD
     C*
     C*  FOLDER MUST EXIST
     C*
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        7590
     C                   GOTO      ENDCK1
     C                   ENDIF                                                  END EQ '1'
     C*
     C*  USER MUST HAVE AUTHORITY TO THE FOLDER
     C*
     C     QQRTCD        IFEQ      '2'
     C                   SETON                                        8090
     C                   GOTO      ENDCK1
     C                   ENDIF                                                  END EQ '2'
     C*
     C*** DOCUMENT ID
     C*
     C     SCDCID        IFEQ      *BLANKS
     C                   SETON                                        707490
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*  THE DOCUMENT ID MUST BE VALID
     C*
     C                   CALL      'SVALDOC'
     C                   PARM                    SCDCID
     C                   PARM                    SCFLDR
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     QQRTCD        IFEQ      '1'
     C                   SETON                                        7490
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*** # OF COPIES
     C*
     C     SC#CPY        IFEQ      *ZEROS
     C                   SETON                                        707690
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*** OUTQ
     C*
     C     SCOUTQ        IFEQ      *BLANKS
     C                   SETON                                        707790
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C*
     C*  DOCUMENT OUTPUT QUEUE MUST BE VALID ON SYSTEM.
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XFOUND            1
     C     XFOUND        IFEQ      '1'
     C                   SETON                                        7790
     C                   GOTO      ENDCK1
     C                   END
     C*
     C*** OFFICE
     C*
      *  OFFICE # CANNOT BE BLANK
      *
     C     SCOFC#        IFEQ      *BLANKS
     C                   SETON                                        707890
     C                   GOTO      ENDCK1
     C                   ENDIF
      *
     C                   MOVE      SCOFC#        KKOFC#
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9078
     C                   GOTO      ENDCK1
     C                   END
      *** Beg Add *****  11/25/13 ************************************
     C     SCCSFR        IFEQ      'N'
     C     SCCSTO        ANDNE     'O'
     C                   Eval      erCsFR = *On
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
      *** END Add *****  11/25/13 ************************************
      *
     C*
     C     ENDCK1        ENDSR
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
     C     CLFND         IFNE      *ZEROS
     C                   MOVE      CLFND         SCFND
     C                   ELSE
     C                   MOVE      FNFND         SCFND
     C                   ENDIF
     C                   MOVE      CLST          SCST
     C                   MOVE      CLLTFL        SCLTFL
      *** Beg Add *****  11/25/13 ************************************
     C                   MOVE      CLLTTO        SCLTTO
     C                   MOVE      CLCSFR        SCCSFR
     C                   MOVE      CLCSTO        SCCSTO
     C                   MOVE      CLANDR        SCANDR
      *** End Add *****  11/25/13 ************************************
     C                   MOVE      CLDPCD        SCDPCD
     C                   MOVEL     CLDCID        SCDCID
     C                   MOVEL     CLFLDR        SCFLDR
     C     DOKY          CHAIN     PDO01                              99
     C  N99              MOVEL     DODESC        SCDESC
     C                   MOVE      CL#CPY        SC#CPY
     C                   MOVE      CLOUTQ        SCOUTQ
     C                   MOVE      CLOFC#        SCOFC#
     C                   MOVE      SCOFC#        KKOFC#
     C     OFKY          CHAIN     POF01                              60
     C  N60              MOVEL(P)  OFNAME        SCOFCD
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         CLCO#
     C                   MOVE      SCFND         CLFND
     C                   MOVE      SCST          CLST
     C                   MOVE      SCLTFL        CLLTFL
      *** Beg Add *****  11/25/13 ************************************
     C                   MOVE      SCLTTO        CLLTTO
     C                   MOVE      SCCSFR        CLCSFR
     C                   MOVE      SCCSTO        CLCSTO
     C                   MOVE      SCANDR        CLANDR
      *** End Add *****  11/25/13 ************************************
     C                   MOVE      SCDPCD        CLDPCD
     C                   MOVEL     SCDCID        CLDCID
     C                   MOVEL     SCFLDR        CLFLDR
     C                   MOVE      SC#CPY        CL#CPY
     C                   MOVE      SCOUTQ        CLOUTQ
     C                   MOVE      SCOFC#        CLOFC#
     C*
     C                   MOVEL     Q1USER        CLUSER
     C                   MOVEL     Q1PGM         CLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLDT
     C                   MOVEL     DSTIME        CLTIME
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
     C* F10 = SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDPCD'
     C     HFLD          OREQ      'SCDCID'
     C     HFLD          OREQ      'SCFLDR'
     C*
     C                   MOVEL(P)  PTL           QQPRDS
     C                   MOVE      *BLANKS       QQCALL
     C                   CALL      'W2321R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    KKDEPT
     C                   PARM                    SCDPCD
     C                   PARM                    SCFLDR
     C                   PARM                    SCDCID
     C                   PARM                    QQRTCD            1
     C*
     C***  Begin Chg  ***  04/13/07  **************************************
     C********** QQRTCD    IFEQ 'A'
     C**********           EXSR PGMEND
     C**********           RETRN
     C**********           ENDIF
     C**********
     C********** QQRTCD    IFEQ '*'
     C**********
     C********** DOKEY     CHAINPDO01                40
     C********** *IN40     IFEQ '0'
     C**********           MOVELDODESC    SCDESC
     C**********           ENDIF
     C**********           ENDIF
     C                   SELECT
     C     HFLD          WHENEQ    'SCDPCD'
     C                   Z-ADD     32            IN                2 0
     C     HFLD          WHENEQ    'SCDCID'
     C                   Z-ADD     33            IN
     C     HFLD          WHENEQ    'SCFLDR'
     C                   Z-ADD     34            IN
     C                   ENDSL
     C*
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   EXSR      PGMEND
     C                   RETURN
     C*
     C     QQRTCD        WHENEQ    '*'
     C*
     C     DOKEY         CHAIN     PDO01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     DODESC        SCDESC
     C                   ENDIF
     C                   ADD       1             IN
     C*
     C                   OTHER
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           *IN(IN)
     C***  End   Chg  ***  04/13/07  **************************************
     C*
     C     HFLD          WHENEQ    'SCOFC#'
     C                   MOVE      *BLANK        QAKC
     C                   MOVE      *BLANK        QAKL
     C                   MOVE      '2'           QACALL
     C                   MOVE      *BLANK        QATPA
     C                   CALL      'W0002R'
     C     *INKC         PARM                    QAKC              1
     C     *INKL         PARM                    QAKL              1
     C                   PARM                    QACALL            1
     C                   PARM                    QATPA             5
     C     QATPA         IFEQ      *BLANK
     C***  Begin Chg  ***  04/13/07  **************************************
     C**********           MOVE *ON       *IN41            POS CUSR
     C                   MOVE      *ON           *IN31
     C***  End   Chg  ***  04/13/07  **************************************
     C                   ELSE
     C********             MOVELQATPA     SCTPA1    P
     C                   MOVEL(P)  QATPA         SCOFC#
     C                   MOVE      SCOFC#        KKOFC#
     C***  Begin Chg  ***  04/13/07  **************************************
     C**********           MOVE *ON       *IN42            POS CUSR
     C                   MOVE      *ON           *IN32
     C***  End   Chg  ***  04/13/07  **************************************
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCOFCD
     C                   END
     C                   END
      *
     C*
     C                   OTHER
     C                   ENDSL
     C                   ENDIF
     C*
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
     C                   MOVE      'CS'          KKDEPT
     C     @@RRN2        CHAIN     WMFNP                              60
     C                   MOVE      FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNAM
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
     C                   MOVEL     Q1USER        CLIUSR
     C                   MOVEL     Q1PGM         CLIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLIDT
     C                   MOVEL     DSTIME        CLITM
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      WACLP
     C                   ENDSR
