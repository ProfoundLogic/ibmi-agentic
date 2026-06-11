     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE C0077R02 - Claimant Maintenance
     H****************************************************************
     H* PROGRAM DESCRIPTION:                                         *
     H*                                                              *
     H*    CALLED FROM:                                              *
     H*                                                              *
     H*    CALLS TO:                                                 *
     H*                                                              *
     H*    Created by:  C:53                                         *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 01/23/07 C:15  Suupport: PC051/56136                          *
     H*                Chgd pgm to load the zip code correctly.       *
     H*                                                               *
     H*****************************************************************
     H*
     FC0077F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FCMCLP     UF A E             DISK
     F                                     RENAME(PCL01:DBRCDF)
     FCMCML1    IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMCLL     IF   E           K DISK
      /COPY SCOPYMT01D
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('CLAIMANTS')
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
     C                   PARM                    @@CLM#
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     CMKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCLM#
     C*
     C     PMKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    CMPOL#
     C*
     C     CLKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCLM#
     C                   KFLD                    KKCLID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMCLM#        KKCLM#
     C     *LIKE         DEFINE    CLCLID        KKCLID
     C     *LIKE         DEFINE    CMCLM#        @@CLM#
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
     C                   DO
     C*
     C*  THE CLAIMANT NAME CANNOT BE BLANK
     C*
     C     SCCNAM        IFEQ      *BLANK
     C                   SETON                                        7190
     C                   LEAVE
     C                   ENDIF
     C*
     C*  THE CLAIMANT STATE MUST BE VALID
     C*
     C     SCCST         IFNE      *BLANK
     C     SCCST         LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   SETON                                        7290
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C*  EDIT BIRTH DATE
     C*
     C     SCCDOB        IFNE      0
     C                   Z-ADD     SCCDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDOB
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7390
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C*  THE EMPLOYER STATE MUST BE VALID
     C*
     C     SCEST         IFNE      *BLANK
     C     SCEST         LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   SETON                                        7490
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
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
     C     @@MODE        IFEQ      'A'
     C                   MOVE      SCCLM1        KKCLM#
     C                   ELSE
     C                   MOVE      CLCLM#        KKCLM#
     C                   ENDIF
     C*
     C     CMKEY         CHAIN     PCM01                              99
     C                   Z-ADD     CMLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLSDT
     C*
     C     PMKEY         CHAIN     PPM01                              99
     C                   MOVEL     PMPOL#        SCPOL#
     C                   MOVEL(P)  PMFIN         SCFIN
     C                   MOVE      PMAGCO        SCAGT#
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      SCCLM1        SCCLM#
     C                   MOVE      SCCLI1        SCCLID
     C*
     C                   ELSE
     C                   MOVE      CLCLM#        SCCLM#
     C                   MOVE      CLCLID        SCCLID
     C                   ENDIF
     C*
     C                   MOVE      CLCNAM        SCCNAM
     C                   Z-ADD     CLCDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDOB
     C                   MOVE      CLCADR        SCCADR
     C                   MOVEL     CLCCTY        SCCCTY
     C                   MOVE      CLCST         SCCST
     C                   MOVEL     CLCZIP        SCCZP1
     C                   MOVE      CLCZIP        SCCZP2
     C                   MOVE      CLCARA        SCCARA
     C                   MOVE      CLCPHN        SCCPHN
     C                   MOVE      CLFEIN        SCFEIN
     C                   MOVE      CLCSEX        SCCSEX
     C                   MOVE      CLENAM        SCENAM
     C                   MOVE      CLEAD1        SCEAD1
     C                   MOVEL     CLECTY        SCECTY
     C                   MOVE      CLEST         SCEST
     C                   MOVEL     CLEZIP        SCEZP1
     C                   MOVE      CLEZIP        SCEZP2
     C                   MOVE      CLEARA        SCEARA
     C                   MOVE      CLEPHN        SCEPHN
     C                   MOVEL     CLINDS        SCINDS
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         CLCO#
     C                   MOVE      SCCLM#        CLCLM#
     C                   MOVE      SCCLID        CLCLID
     C                   MOVE      SCCNAM        CLCNAM
     C                   Z-ADD     SCCDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLCDOB
     C                   MOVE      SCCADR        CLCADR
     C                   MOVEL     SCCCTY        CLCCTY
     C                   MOVE      SCCST         CLCST
     C                   MOVEL     SCCZP1        CLCZIP
     C*** Begin chg *** 01/23/07 **************************************
     C*******************  MOVE SCCZP1    CLCZIP
     C                   MOVE      SCCZP2        CLCZIP
     C*** End chg ***** 01/23/07 **************************************
     C                   MOVE      SCCARA        CLCARA
     C                   MOVE      SCCPHN        CLCPHN
     C                   MOVE      SCFEIN        CLFEIN
     C                   MOVE      SCCSEX        CLCSEX
     C                   MOVE      SCENAM        CLENAM
     C                   MOVE      SCEAD1        CLEAD1
     C                   MOVEL     SCECTY        CLECTY
     C                   MOVE      SCEST         CLEST
     C                   MOVEL     SCEZP1        CLEZIP
     C                   MOVE      SCEZP1        CLEZIP
     C                   MOVE      SCEARA        CLEARA
     C                   MOVE      SCEPHN        CLEPHN
     C                   MOVEL     SCINDS        CLINDS
     C                   MOVE      Q1USER        CLUSER
     C                   MOVE      Q1PGM         CLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLDT
     C                   Z-ADD     DSTIME        CLTIME
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
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     UPKEY         CHAIN     PUP01                              99
     C                   Z-ADD     UPCO#         KKCO#
     C                   Z-ADD     UPCO#         ##CO#
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      @@CLM#        SCCLM1
     C                   MOVE      *BLANKS       SCCLI1
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C* F3/F12
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   RETURN
     C                   ENDIF
     C*
     C     SCCLI1        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN72
     C                   ITER
     C                   ENDIF
     C*
     C                   MOVE      SCCLM1        KKCLM#
     C                   MOVE      SCCLI1        KKCLID
     C     CLKEY         CHAIN     PCL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN71
     C                   CLEAR                   DBRCDF
     C                   ITER
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ELSE
     C                   ENDIF
     C*
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
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      CMCLP
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
