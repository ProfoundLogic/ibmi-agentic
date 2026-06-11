     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0061R01 - PROCEDURE CODE MAINTENANCE
      * ***************************************************************
      * DESCRIPTION :
      *    TO ADD, CHANGE, OR DISPLAY PROCEDURE CODES FOR A SINGLE    *
      *    PRE/RE-CERT.                                               *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    U0061R   - WORK WITH PROCEDURE CODES                       *
      *                                                               *
      *  CREATED:  2/23/98   47    87MCB                              *
      * ********************************************************
      * REVISIONS:
      *
      *  11/03/04 C:079 PROJECT: MC257
      *                 ADD PRE-AUTH AMOUNT FIELD
      *
      *  11/27/01 C:062 PROJECT: MC096 29993
      *                 CHANGE HOW ADJUDICATION STATE IS OBTAINED.
      *
      *  05/26/99 C:062 PROJECT: MC145
      *                 ADD SEQUENCE # CALCULATION AND REQUIRE AT LEAST
      *                 ONE FIELD TO BE ENTERED.
      *
      *  03/12/99 C:062 PROJECT: MC087 17051
      *                 PROTECT THE PROCEDURE CODE FIELD DURING
      *                 CHANGE MODE.
      *
      * ********************************************************
      *
     FU0061F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FUDPOP     UF A E             DISK
     F                                     RENAME(PPO01:DBRCDF)
     F*
     F*  Pre-Cert Master
     FUMPCL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F*** BEGIN ADD *** 05/26/99 **************************************
     FUDPOL3    IF   E           K DISK
     F*** END   ADD *** 05/26/99 **************************************
      * Reasonable & Customary file
     FFMRCL1    IF   E           K DISK
      * Case Master file
     FFMCML1    IF   E           K DISK
      * State Master file
     FSTSTL     IF   E           K DISK
      * Provider Auxiliary file
     FFMP1L     IF   E           K DISK
      * Procedure Code file
     FFMPRL     IF   E           K DISK    USROPN
      *  GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT0P)
     F                                     INFDS(DBFDS3)
      *  TYPE OF SERVICE
     FFTTSL1    IF   E           K DISK
     FFTTSP     IF   E             DISK
     F                                     RENAME(PTS01:PTS0P)
     F                                     INFDS(DBFDS4)
      *
      /COPY SCOPYMT01D
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
      *
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
      *  FTGTP
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
      *  FTTSP
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
     D                 DS
     D  @@CMD                  1     56
     D  @@ST                  38     39
     D                 DS                  INZ
     D  XXTOS                  1     10
     D  X3TOS                  3      5
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pre/Re-Cert -
     D                                     Procedure Code -
     D                                     Information')
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
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* PRE-CERT MASTER
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C*
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C*** BEGIN ADD *** 05/26/99 **************************************
     C*
     C     POKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C*
     C     POKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C                   KFLD                    XXSEQ#
     C*** END   ADD *** 05/26/99 **************************************
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCPCD
      *
     C     P1KY          KLIST
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
      *
     C     STKY          KLIST
     C                   KFLD                    XXAST            10
     C*
     C*   KEY LIST FOR THE TYPE OF SERVICE TABLE FILE
     C*
     C     TSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTOS
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    POCO#         XXCO#
     C     *LIKE         DEFINE    POSUB#        XXSUB#
     C     *LIKE         DEFINE    POFND         XXFND
     C     *LIKE         DEFINE    POFYR         XXFYR
     C     *LIKE         DEFINE    POCASE        XXCASE
     C     *LIKE         DEFINE    POCERT        XXCERT
     C     *LIKE         DEFINE    POEXT         XXEXT
     C*** BEGIN ADD *** 05/26/99 **************************************
     C     *LIKE         DEFINE    POSEQ#        XXSEQ#
     C*** END   ADD *** 05/26/99 **************************************
     C     *LIKE         DEFINE    POSUB#        KKSUB#
     C     *LIKE         DEFINE    POFND         KKFND
     C     *LIKE         DEFINE    POFYR         KKFYR
     C     *LIKE         DEFINE    POCASE        KKCASE
     C     *LIKE         DEFINE    POCERT        KKCERT
     C     *LIKE         DEFINE    POEXT         KKEXT
     C     *LIKE         DEFINE    PCACDT        XXACDT
     C     *LIKE         DEFINE    PCACDT        XXFRDT
     C     *LIKE         DEFINE    PCACDT        XXTODT
     C     *LIKE         DEFINE    PODT          XXDT
     C     *LIKE         DEFINE    CMAST         XXPRST
     C     *LIKE         DEFINE    PCCPT1        XXCPCD
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTTBID        @@TBID
      *
     C*================================================================
      * THE USER REQUESTED THAT A PROCEDURE CODE BE DELETED.
      * INSTEAD, INACTIVATE THE RECORD SO THAT IT DOES NOT DISPLAY.
      *
     C     @@MODE        IFEQ      'E'
     C     @@RRN         CHAIN     DBRCDF                             99
     C                   MOVEL(P)  'I'           POSTAT
     C                   MOVEL     Q1USER        POUSER
     C                   MOVEL     Q1PGM         POPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         PODT
     C                   Z-ADD     DSTIME        POTIME
B01  C     *IN99         IFEQ      *OFF
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
      *
     C                   MOVE      *ON           OUTFLG
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   END                                                    @@MODE = E
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
      *
     C                   DO
     C*** BEGIN ADD *** 03/12/99 **************************************
     C     *IN40         IFEQ      *ON
     C*** END   ADD *** 03/12/99 **************************************
     C*** BEGIN ADD *** 03/12/99 **************************************
     C*** END   ADD *** 03/12/99 **************************************
      *
      *  EITHER PROCEDURE CODE OR DESC MUST BE ENTERED
      *
     C     SCPROC        IFEQ      *BLANK
     C     SCPROD        ANDEQ     *BLANK
     C*** BEGIN ADD *** 05/26/99 **************************************
     C     SCTOS         ANDEQ     *BLANK
     C     SCVST         ANDEQ     *ZEROS
     C     SCFRDT        ANDEQ     *ZEROS
     C*** END   ADD *** 05/26/99 **************************************
     C*** BEGIN CHG *** 05/26/99 **************************************
     C**********           MOVE *ON       *IN71
     C                   MOVE      *ON           *IN82
     C*** END   CHG *** 05/26/99 **************************************
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C*** BEGIN ADD *** 03/12/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/12/99 **************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE TO FMPRL
     C*
     C     OVRFLG        IFEQ      '1'
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C*   OVERRIDE TO FMPRL
     C*
     C     OVRFLG        IFEQ      ' '
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXPRST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
     C*** BEGIN ADD *** 03/12/99 **************************************
     C*** BEGIN CHG *** 05/26/99 **************************************
     C********** *IN40     IFEQ *ON
     C     SCPROC        IFNE      *BLANKS
     C*** END   CHG *** 05/26/99 **************************************
     C*** END   ADD *** 03/12/99 **************************************
      *
     C                   MOVEL(P)  SCPROC        POPROC
     C                   MOVEL(P)  SCPROC        XXCPCD
      *
     C     PRKY          CHAIN     PPR01                              60
     C     *IN60         IFEQ      *ON                                          NOT FOUND
      *
     C     PRKY          CHAIN     PRC01                              61
     C     *IN61         IFEQ      *ON
     C     RCSTAT        ORNE      'A'
     C                   SETON                                        9078
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 03/12/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/12/99 **************************************
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      * VALIDATE TYPE OF SERVICE
      *
     C     SCTOS         IFNE      *BLANK
     C                   MOVE      *BLANK        XXTOS
     C                   MOVEL     SCTOS         X3TOS
     C                   MOVEL     XXPRST        XXTOS
     C     TSKY          CHAIN     PTS01                              60
     C     *IN60         IFEQ      *ON
     C     TSSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN81                          POS CUSR
     C                   MOVE      *ON           *IN90                          POS CUSR
     C                   LEAVE
     C                   END
     C                   END
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  AUTHORIZED VISITS CAN NOT BE LESS THAN ZERO
      *
     C     SCVST         IFLT      *ZERO
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * VALIDATE FROM DATE OF SERVICE
      *
     C     SCFRDT        IFNE      *ZEROS
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * FROM DATE MUST NOT BE LESS THAN ADMIT/PLAN DATE
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C     XXFRDT        IFLT      PCADDT
     C                   MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END                                                    SCFRDT NE ZERO
      *
      * VALIDATE TO DATE OF SERVICE
      *
     C     SCTODT        IFNE      *ZEROS
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN73
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END                                                    SCTODT NE ZERO
      *
      * IF FROM OR TO DATE IS BLANK AND THE OTHER ONE NOT BLANK, ERROR.
      *
     C     SCFRDT        IFNE      *ZERO
     C     SCTODT        ANDEQ     *ZERO
     C     SCFRDT        OREQ      *ZERO
     C     SCTODT        ANDNE     *ZERO
     C                   MOVE      *ON           *IN76                          MCR2404
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    SCFR/TODT NE ZERO
      *
      * TO DATE MAY NOT BE LESS THAN FROM DATE
      *
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ANDNE     *ZEROS
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXTODT        IFLT      XXFRDT
     C                   MOVE      *ON           *IN77                          MCR2413
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END                                                    SCFR/TODT NE ZERO
     C**
     C* VALIDATE THE ALLOWANCE CODE
     C*
     C     SCALLW        IFNE      *BLANK
     C                   MOVEL(P)  'PCAL'        XXTBID
     C                   MOVEL(P)  SCALLW        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9080
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C**
     C* VALIDATE THE ALLOWANCE AUTHORITY
     C*
     C     SCAUTH        IFNE      *BLANK
     C                   MOVEL(P)  'PCAU'        XXTBID
     C                   MOVEL(P)  SCAUTH        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
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
     C* KFDO   - F6 = CPT4 SEARCH
     C*================================================================
     C     KFDO          BEGSR
     C                   ENDSR                                                  KFDO
     C*================================================================
     C* KGDO   - F7 = HCPCS SEARCH
     C*================================================================
     C     KGDO          BEGSR
     C                   ENDSR                                                  KGDO
     C*================================================================
     C* KFJO   - F10 = SEARCH
     C*================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
     C*  PROCEDURE CODE
     C*
     C     HFLD          WHENEQ    'SCPROC'
     C*** BEGIN ADD *** 03/12/99 **************************************
     C     *IN40         ANDEQ     *ON
     C*** END   ADD *** 03/12/99 **************************************
     C     HFLD          OREQ      'SCPROD'
     C*** BEGIN ADD *** 03/12/99 **************************************
     C     *IN40         ANDEQ     *ON
     C*** END   ADD *** 03/12/99 **************************************
     C     PCKY1         CHAIN     PPC01                              60
     C                   Z-ADD     DBRRN2        @@RRN1            9 0          FADAP FILE
     C                   CALL      'F2013R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RRN1            9 0          UMPCP FILE
     C                   PARM      *BLANK        @@PROC            5            PROC CODE
     C                   PARM      *BLANK        @@PROD           50            PROC DESC
     C                   PARM      *BLANK        @@RTCD            1
      *
      *  DID NOT MAKE A SELECTION
      *
     C     @@RTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN30                          STAY ON FIELD
      *
     C                   ELSE
     C                   MOVE      *ON           *IN32                          GO TO NEXT FLD
     C                   MOVEL     @@PROC        SCPROC                         PROC CODE
     C                   MOVEL     @@PROD        SCPROD                         PROC DESC
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  TYPE OF SERVICE SEARCH  *
      *
     C     HFLD          WHENEQ    'SCTOS'
     C                   CALL      'F2365R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZERO         @@RRN4            9 0
     C                   PARM      XXPRST        @@ST              2
      *
     C     @@RRN4        IFEQ      *ZERO
     C                   MOVE      *ON           *IN32                          POS CUSR
     C                   END
      *
     C     @@RRN4        IFNE      *ZERO
     C     @@RRN4        CHAIN     PTS0P                              60
     C     *IN60         IFEQ      *OFF
     C     TSSTAT        ANDEQ     'A'
      *
      * USE POSITIONS 3,4,& 5 OF TSKEY; DON'T USE STATE IN TSKEY
      *
     C                   MOVEL(P)  TSKEY         XXTOS
     C                   MOVEL(P)  X3TOS         SCTOS                          POS 3-5
     C                   MOVEL(P)  TSDESC        SCTOSD
     C                   MOVE      *ON           *IN33                          POS CUSR
     C                   END
     C                   END
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  ALLOWANCE CODE
      *
     C     HFLD          WHENEQ    'SCALLW'
     C                   CALL      'F2380R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C                   PARM      *ZERO         @@RRN3            9 0
     C                   PARM      'PCAL'        @@TBID
      *
     C     @@RRN3        IFEQ      *ZERO
     C                   MOVE      *ON           *IN36                          POS CUSR
     C                   END
      *
     C     @@RRN3        IFNE      *ZERO
     C                   Z-ADD     @@RRN3        DBRRN3
     C     DBRRN3        CHAIN     PGT0P                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTKEY         SCALLW
     C                   MOVEL(P)  GTDESC        SCALLD
     C                   MOVE      *ON           *IN37                          POS CUSR
     C                   END
     C                   END
      *
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  ALLOWANCE AUTHORITY
      *
     C     HFLD          WHENEQ    'SCAUTH'
     C                   CALL      'F2380R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C                   PARM      *ZERO         @@RRN3            9 0
     C                   PARM      'PCAU'        @@TBID
      *
     C     @@RRN3        IFEQ      *ZERO
     C                   MOVE      *ON           *IN37                          POS CUSR
     C                   END
      *
     C     @@RRN3        IFNE      *ZERO
     C                   Z-ADD     @@RRN3        DBRRN3
     C     DBRRN3        CHAIN     PGT0P                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTKEY         SCAUTH
     C                   MOVEL(P)  GTDESC        SCAUTD
     C                   MOVE      *ON           *IN38                          POS CUSR
     C                   END
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C                   OTHER
     C                   MOVE      *ON           *IN20                          INVALID SEARCH
     C                   ENDSL
     C                   ENDSR                                                  KJDO
     C*================================================================
     C* KNJO   - F10 = DARTS
     C*================================================================
     C     KNDO          BEGSR
      *
      * CALL API TO ACCESS DARTS
      *
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      XXSUB#        XASUB#
     C                   MOVE      XXFND         XAFND
     C                   MOVE      XXFYR         XAFYR
     C                   MOVE      XXCASE        XACASE
      *
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@WCIN
     C                   PARM                    @@WCDK
     C                   PARM                    XACO#             3
     C                   PARM                    XASUB#            5
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C                   PARM                    XACASE            7
     C                   PARM      *BLANK        ##KEY            40
     C                   ENDSR                                                  KNDO
      * ================================================================
      * INIT SUBROUTINE - INITALIZE ALL FIELDS ON ENTRY
      * ================================================================
     C     INIT          BEGSR
     C*
     C                   MOVE      *BLANK        SCPROC
     C                   MOVE      *BLANK        SCPROD
     C                   MOVE      *BLANK        SCTOS
     C                   MOVE      *BLANK        SCTOSD
     C                   Z-ADD     *ZERO         SCVST
     C                   Z-ADD     *ZEROS        XXAGEC            1 0          1.0N
     C                   MOVE      *BLANK        SCFRDT
     C                   MOVE      *BLANK        SCTODT
     C                   MOVE      *BLANK        SCALLW
     C                   MOVE      *BLANK        SCAUTH
     C                   MOVE      *BLANK        SCEOB
      *
     C                   ENDSR                                                  INIT
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C*** BEG ADD *** 02/11/99 ****************************************
     C     *IN40         IFEQ      *ON
     C                   MOVE      *BLANK        SCPROC
     C                   MOVE      *BLANK        SCPROD
     C                   MOVE      *BLANK        SCTOS
     C                   MOVE      *BLANK        SCTOSD
     C                   MOVE      *ZERO         SCVST
     C                   MOVE      *ZERO         SCFRDT
     C                   MOVE      *ZERO         SCTODT
     C*** BEG ADD *** 11/03/04 ****************************************
     C                   Z-ADD     0             SCPAMT
     C*** END ADD *** 11/03/04 ****************************************
     C*** BEGIN ADD *** 05/26/99 **************************************
     C                   MOVEL     *BLANKS       SCTOSD
     C                   Z-ADD     *HIVAL        XXSEQ#
     C*
     C     POKY2         SETGT     PPO01
     C     POKY1         READPE    PPO01                                  99
     C*
     C  N99POSEQ#        ADD       1             SCSEQ#
     C   99              Z-ADD     1             SCSEQ#
     C*** END   ADD *** 05/26/99 **************************************
     C                   ELSE
     C*
     C*** END ADD *** 02/11/99 ****************************************
     C*
     C*** BEGIN ADD *** 05/26/99 **************************************
     C                   Z-ADD     POSEQ#        SCSEQ#
     C*** END   ADD *** 05/26/99 **************************************
     C                   MOVEL(P)  POPROC        SCPROC
     C                   MOVEL(P)  PODESC        SCPROD
     C                   MOVEL(P)  POTOS         SCTOS
     C                   Z-ADD     POVST         SCVST
      *
     C                   Z-ADD     POFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCFRDT
      *
     C                   Z-ADD     POTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCTODT
     C*** BEG ADD *** 11/03/04 ****************************************
     C                   Z-ADD     POPAMT        SCPAMT
     C*** END ADD *** 11/03/04 ****************************************
     C*** BEG ADD *** 02/11/99 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/11/99 ****************************************
      *
      * LOAD TYPE OF SERVICE
      *
     C*** BEGIN ADD *** 05/26/99 **************************************
     C     *IN40         IFEQ      *OFF
     C*** END   ADD *** 05/26/99 **************************************
     C     POTOS         IFNE      *BLANK
     C                   MOVE      *BLANK        XXTOS
     C                   MOVEL     POTOS         X3TOS
     C                   MOVEL     XXPRST        XXTOS
     C     TSKY          CHAIN     PTS01                              60
     C     *IN60         IFEQ      *OFF
     C     TSSTAT        ANDEQ     'A'
     C                   MOVEL(P)  TSDESC        SCTOSD
     C                   END
     C                   END
     C*** BEGIN ADD *** 05/26/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/26/99 **************************************
      *
      * Default disallowance EOB to FTGTP/GTCF01 based on 'D'
      * value of FTGTP via 'PCAL', but user may change.
      *
     C                   MOVEL(P)  POALLW        SCALLW
     C     SCALLW        IFEQ      'D'
     C     POEOB         ANDEQ     *BLANK
     C                   MOVEL(P)  'PCAL'        XXTBID
     C                   MOVEL(P)  SCALLW        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTCF01        SCEOB
     C                   MOVEL(P)  GTDESC        SCALLD
     C                   ENDIF
     C                   END
      *
      *  ALLOWANCE AUTHORITY
      *
     C                   MOVEL(P)  POAUTH        SCAUTH
     C     POAUTH        IFNE      *BLANK
     C                   MOVEL(P)  'PCAU'        XXTBID
     C                   MOVEL(P)  POAUTH        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCAUTD
     C                   ENDIF
     C                   END
      *
     C     POEOB         IFNE      *BLANK
     C                   MOVEL(P)  POEOB         SCEOB
     C                   END
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C     @@MODE        IFEQ      'A'
     C*** BEGIN ADD *** 05/26/99 **************************************
     C                   Z-ADD     *HIVAL        XXSEQ#
     C*
     C     POKY2         SETGT     PPO01
     C     POKY1         READPE    PPO01                                  99
     C*
     C  N99              ADD       1             POSEQ#
     C   99              Z-ADD     1             POSEQ#
     C*** END   ADD *** 05/26/99 **************************************
     C                   Z-ADD     XXCO#         POCO#
     C                   Z-ADD     XXSUB#        POSUB#
     C                   Z-ADD     XXFND         POFND
     C                   Z-ADD     XXFYR         POFYR
     C                   Z-ADD     XXCASE        POCASE
     C                   MOVEL(P)  XXCERT        POCERT
     C                   Z-ADD     XXEXT         POEXT
     C                   MOVEL(P)  'A'           POSTAT
     C                   MOVEL(P)  Q1USER        POIUSR
     C                   MOVEL(P)  Q1PGM         POIPGM
     C                   Z-ADD     XXDT          POIDT
     C                   Z-ADD     DSTIME        POITIM
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LOAD PROCEDURE CODE
      *
     C                   MOVEL(P)  SCPROC        POPROC
     C                   MOVEL(P)  SCPROD        PODESC
     C     SCPROD        IFEQ      *BLANK
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE TO FMPRL
     C*
     C     OVRFLG        IFEQ      '1'
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
     C*
     C*   OVERRIDE TO FMPRL
     C*
     C     OVRFLG        IFEQ      ' '
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXPRST        @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG            1
     C                   END                                                    END OVRFLG=0
     C*
     C*   OPEN FILE FOR PROCESSING
     C*
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   MOVE      '1'           OPNFLG
     C                   END                                                    END OPNFLG= ' '
      *
     C                   MOVEL(P)  SCPROC        XXCPCD
      *
     C     PRKY          CHAIN     PPR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        PODESC
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON                                          NOT FOUND
     C     PRKY          CHAIN     PRC01                              61
     C     *IN61         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'A'
     C                   MOVEL(P)  RCDSC1        PODESC
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF                                                  SCPROD = BLANK
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   MOVEL(P)  SCTOS         POTOS
     C                   Z-ADD     SCVST         POVST
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         POFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDCY --> CYMD
     C                   MOVEL(P)  SRDTO         POTODT
      *
     C                   MOVEL(P)  SCALLW        POALLW
     C                   MOVEL(P)  SCAUTH        POAUTH
     C                   MOVEL(P)  SCEOB         POEOB
     C*** BEG ADD *** 11/03/04 ****************************************
     C                   Z-ADD     SCPAMT        POPAMT
     C*** END ADD *** 11/03/04 ****************************************
     C*
     C                   MOVEL     Q1USER        POUSER
     C                   MOVEL     Q1PGM         POPGM
     C                   Z-ADD     XXDT          PODT
     C                   Z-ADD     DSTIME        POTIME
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
      *
      * RESET ERROR INDICATORS
      *   SCREEN 1
     C                   MOVEA     '0'           *IN(20)                                    CH
     C                   MOVEA     '000000'      *IN(30)                        *IN30 - 35
     C                   MOVEA     '000'         *IN(36)                        *IN36 - 38
      *
     C     *INKF         CASEQ     *ON           KFDO                     90     F6  = CPT4
     C     *INKG         CASEQ     *ON           KGDO                     90     F7  = HCPCS
     C     *INKJ         CASEQ     *ON           KJDO                     90     F10 = SEARCH
     C     *INKN         CASEQ     *ON           KNDO                     90     F14 = DARTS
     C                   ENDCS
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*   CLOSE FILE IF OPENED
     C*
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           OPNFLG            1
     C                   END                                                    END OPNFLG=1
     C*
     C*   DELETE OVERRIDE TO FMPRL
     C*
     C     OVRFLG        IFEQ      '1'
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG            1
     C                   END                                                    END OVRFLG=1
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      *ON           *IN40
     C     @@MODE        WHENEQ    'D'
     C                   MOVE      *ON           *IN09
     C     @@MODE        WHENEQ    '2'
     C                   MOVEL(P)  MODED         SCMODE                         SELECT
     C                   ENDSL
     C*
     C* INITIALIZE ALL SCREEN FIELDS
     C*
     C                   EXSR      INIT
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVEL     QQCERT        XXCERT
     C                   MOVE      QQEXT         XXEXT
     C                   MOVEL     QQCERT        SCCERT
     C                   MOVE      QQEXT         SCEXT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      * CALL API TO CREATE A TWO LINE HEADING
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C     SCL1HD        PARM      *BLANK        QQL1             78
     C     SCL2HD        PARM      *BLANK        QQL2             78
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  GET ADJUDICATION STATE
      *
     C     PCKY          CHAIN     PPC01                              60
     C     CMKY          CHAIN     FMCML1                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CMAST         XXAST
      *
     C     STKY          CHAIN     STSTL                              61
     C*** BEGIN CHG *** 11/27/01 **************************************
     C********** *IN61     IFEQ *OFF
     C********** STINST    IFEQ 'Y'
     C     *IN61         IFEQ      *OFF
     C     STINST        ANDEQ     'Y'
     C*** END   CHG *** 11/27/01 **************************************
     C                   MOVEL(P)  CMAST         XXPRST
     C*** BEGIN CHG *** 11/27/01 **************************************
     C**********           END
     C*
     C********** STINST    IFEQ *BLANK
     C                   ELSE
     C*** END   CHG *** 11/27/01 **************************************
     C     P1KY          CHAIN     FMP1L                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL(P)  P1ST          XXPRST
     C                   END
     C*** BEGIN DEL *** 11/27/01 **************************************
     C**********           END
     C*** BEGIN DEL *** 11/27/01 **************************************
      *
     C                   END
     C                   END
      *
     C                   MOVE      *BLANK        OPNFLG
     C                   MOVE      *BLANK        OVRFLG
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALCULATE TOTAL RECOMMENDED DAYS
      *
     C                   Z-ADD     *ZEROS        SCTREC
     C     PCKY1         SETGT     PPC01
     C     *IN60         DOUEQ     *ON
     C     PCKY          READPE    PPC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   ADD       PCRECD        SCTREC
     C                   END
     C                   ENDDO
      *
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
     C*****                MOVE *ON       OUTFLG           DEL 07/08/98
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      UDPOP
     C*****                MOVE *ON       OUTFLG           DEL 07/08/98
     C                   ENDSR
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
