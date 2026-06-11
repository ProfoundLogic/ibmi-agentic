     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H***************************************************************
     H*                                                             *
     H*  CREATED:  07/31/98  C:94  PROJECT:  MC179                  *
     H*                                                             *
     H*  WCMC COMPANY STATE INFORMATION FOR TEXAS                   *
     H*                                                             *
     H***************************************************************
     H*                                                             *
     H*  REVISIONS:                                                 *
     H*                                                             *
     H*  10/17/14 C:079 Project: MC099 83519                        *
     H*                 Remove tape fields                          *
     H*                                                             *
     H***************************************************************
     H*
     FF0053FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FSMCOP     UF   E             DISK
     F                                     RENAME(PCO01:DBRCDF)
     F*  Physical file definition (for maintaining record)
     FFMC1L     UF A E           K DISK
     F                                     RENAME(PC101:DBRCD2)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Company State I-
     D                                     nformation')
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
     C     C1KY          KLIST
     C                   KFLD                    KKCO#             3 0
     C*
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
     C*
     C  N90SCLOC         IFEQ      *BLANKS
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF
     C*
     C*** Beg Del *** 10/17/14 ****************************************
     C**N90SCDEV         IFNE      *BLANKS
     C*********
     C*********          CALL      'SA021C'
     C*********          PARM      ' '           ##RTNC            1
     C*********          PARM      SCDEV         DEVICE           10
     C*********
     C*****##RTNC        IFEQ      'F'
     C*********
     C*********          SETON                                        9072
     C*********
     C*********          ENDIF
     C*********
     C*********          ELSE
     C*********
     C*********          SETON                                        9073
     C*********
     C*********          ENDIF
     C*********
     C**N90SCTAPE        IFEQ      *ZEROS
     C*********
     C*********          SETON                                        9074
     C*********
     C*********          ENDIF
     C*********
     C**N90SCVOL         IFEQ      *BLANKS
     C*********
     C*********          SETON                                        9075
     C*********
     C*********          ENDIF
     C*********
     C*** End Del *** 10/17/14 ****************************************
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
     C* GETAUX - GET AXILIARY FILE
     C*================================================================
     C     GETAUX        BEGSR
     C                   MOVE      COCO#         KKCO#
     C     C1KY          CHAIN(N)  DBRCD2                             98
     C     *IN98         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   EXSR      GETAUX
     C                   MOVE      COSTAT        SCSTAT
     C                   MOVEL(P)  C1LOC         SCLOC
     C*** Beg Del *** 10/17/14 ****************************************
     C******             MOVEL(P)  C1DEV         SCDEV
     C******             MOVEL(P)  C1TAPE        SCTAPE
     C******             MOVEL(P)  C1VOL         SCVOL
     C*** End Del *** 10/17/14 ****************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL(P)  SCLOC         C1LOC
     C*** Beg Del *** 10/17/14 ****************************************
     C*******            MOVEL(P)  SCDEV         C1DEV
     C*******            MOVEL(P)  SCTAPE        C1TAPE
     C*******            MOVEL(P)  SCVOL         C1VOL
     C*** End Del *** 10/17/14 ****************************************
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
     C     @@RRN         CHAIN     DBRCDF                             99
     C                   MOVEL(P)  COCO#         SCCO#
     C                   MOVEL(P)  CONAME        SCNAME
     C                   MOVEL     COCO#         KKCO#
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
     C     C1KY          CHAIN     DBRCD2                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         C1PGM
     C                   MOVEL     Q1USER        C1USER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        C1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DT
     C                   UPDATE    DBRCD2
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCD2
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1PGM         C1PGM
     C                   MOVEL     Q1USER        C1USER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        C1TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DT
     C                   WRITE     DBRCD2
     C                   FEOD      FMC1L
     C                   ENDSR
