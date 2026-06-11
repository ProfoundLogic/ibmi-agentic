     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                              *
     H****************************************************************
     H*                                                              *
     H*  CREATED:  07/24/98   C:94  PROJECT:  MC179                  *
     H*                                                              *
     H****************************************************************
     H*                                                              *
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  01/11/23 C:079 Support: 105892                              *
     H*                 Fix format issues on phone number fields.    *
     H*                 Display file only changes.                   *
     H*                                                              *
     H*  02/12/18 C:180 Project: C114MJC                             *
     H*                 Added Last 838 Payment/Remittance Advice Id  *
     H*                                                              *
     H*  07/17/14 C:079 Project: MC376                               *
     H*                 Added Last 837 Tset ID                       *
     H*                                                              *
     H*  04/02/09 C:227 Project: MC331                               *
     H*                 Added NCCI Carrier Group Code.               *
     H*                                                              *
     H*  03/23/07 C:188 Support: 55783                               *
     H*                 Screen change, recompile only.               *
     H*                                                              *
     H*  10/31/06 C:62 PROJECT: TN004                                 *
     H*                ADD TN SUBMISSION ID.                          *
     H*                                                               *
     H*  06/19/00 C:62 SUPPORT: MC065 19768                           *
     H*                SWAP OUT USAGE OF STABL TO STSTL3, SO THAT UC  *
     H*                BECOMES INVALID                                *
     H*                                                              *
     H****************************************************************
     H*
     FF0052F01  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for Default information)
     FSMCOP     UF   E             DISK
     F                                     RENAME(PCO01:DBRCDF)
     F*
     F*  Physical file definition (for maintaining record)
     FFMC1L     UF A E           K DISK
     F                                     RENAME(PC101:DBRCD2)
     FSTSTP     IF   E             DISK
     F*** BEGIN CHG *** 06/19/00 **************************************
     F**********STABL   IF  E           K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END   CHG *** 06/19/00 **************************************
     F                                     RENAME(PST01:PST02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Company Setup I-
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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    C1CO#         KKCO#
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
     C*** COMPANY NAME IS REQUIRED
     C*
     C  N90SCNAM         IFEQ      *BLANKS
     C*
     C                   SETON                                          9072
     C*
     C                   ENDIF
     C*
     C*** COMPANY ADDRESS IS REQUIRED
     C*
     C  N90SCADR1        IFEQ      *BLANKS
     C*
     C                   SETON                                          9073
     C*
     C                   ENDIF
     C*
     C*** CITY IS REQUIRED
     C*
     C  N90SCCTY         IFEQ      *BLANKS
     C*
     C                   SETON                                          9074
     C*
     C                   ENDIF
     C*
     C*** STATE IS REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCST          KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                          9075
     C*
     C                   ENDIF
     C*
     C*** ZIP CODE IS REQUIRED
     C*
     C  N90SCZIP1        IFEQ      *ZEROS
     C*
     C                   SETON                                          9076
     C*
     C                   ENDIF
     C*
     C*** CASE NUMBER IS REQUIRED
     C*
     C  N90SCCASE        IFEQ      *ZEROS
     C*
     C                   SETON                                          9077
     C*
     C                   ENDIF
     C*
     C*** EOMB NUMBER IS REQUIRED
     C*
     C  N90SCEOB#        IFEQ      *ZEROS
     C*
     C                   SETON                                          9078
     C*
     C                   ENDIF
     C*
     C*** INVOICE NUMBER IS REQUIRED
     C*
     C  N90SCINV#        IFEQ      *ZEROS
     C*
     C                   SETON                                          9079
     C*
     C                   ENDIF
     C*
     C*** LETTER NUMBER IS REQUIRED
     C*
     C  N90SCLTR#        IFEQ      *ZEROS
     C*
     C                   SETON                                          9080
     C*
     C                   ENDIF
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
     C*
     C                   Z-ADD     COCO#         KKCO#
     C*
     C     C1KY          CHAIN(N)  DBRCD2                             98
     C*
     C     *IN98         IFEQ      *ON
     C     @@MODE        ANDEQ     'C'
     C*
     C                   MOVE      'A'           @@MODE
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       3031
     C                   SELECT
     C*
     C*** STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCST'
     C*
     C                   CALL      'SY601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        30
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   MOVEL     STKEY         SCST
     C                   SETON                                        31
     C*
     C                   ENDIF
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
     C                   EXSR      GETAUX
     C*
     C                   Z-ADD     COCO#         SCCO#
     C                   MOVEL(P)  CONAME        SCNAME
     C*
     C     @@MODE        IFEQ      'C'                                          NOT ADD MODE
     C     @@MODE        OREQ      'D'                                          NOT ADD MODE
     C*
     C*          C1STAT    IFEQ *BLANK
     C                   MOVE      COSTAT        SCSTAT
     C*                    ELSE
     C*                    MOVE C1STAT    SCSTAT
     C*                    ENDIF
     C*
     C     C1NAME        IFEQ      *BLANKS
     C                   MOVEL(P)  CONAME        SCNAM
     C                   ELSE
     C                   MOVEL(P)  C1NAME        SCNAM
     C                   ENDIF
     C*
     C     C1ADR1        IFEQ      *BLANKS
     C                   MOVE      COADR1        SCADR1
     C                   ELSE
     C                   MOVE      C1ADR1        SCADR1
     C                   ENDIF
     C*
     C     C1ADR2        IFEQ      *BLANKS
     C                   MOVE      COADR2        SCADR2
     C                   ELSE
     C                   MOVE      C1ADR2        SCADR2
     C                   ENDIF
     C*
     C     C1CTY         IFEQ      *BLANKS
     C                   MOVE      COCTY         SCCTY
     C                   ELSE
     C                   MOVE      C1CTY         SCCTY
     C                   ENDIF
     C*
     C     C1ST          IFEQ      *BLANKS
     C                   MOVE      COST          SCST
     C                   ELSE
     C                   MOVE      C1ST          SCST
     C                   ENDIF
     C*
     C     C1ZIP         IFEQ      *ZEROS
     C                   MOVEL     COZIP         SCZIP1
     C                   MOVE      COZIP         SCZIP2
     C                   ELSE
     C                   MOVEL     C1ZIP         SCZIP1
     C                   MOVE      C1ZIP         SCZIP2
     C                   ENDIF
     C*
     C     C1AREA        IFEQ      *ZEROS
     C                   MOVE      COAREA        SCAREA
     C                   ELSE
     C                   MOVE      C1AREA        SCAREA
     C                   ENDIF
     C*
     C     C1PHN#        IFEQ      *ZEROS
     C                   MOVE      COPHN#        SCPHN#
     C                   ELSE
     C                   MOVE      C1PHN#        SCPHN#
     C                   ENDIF
     C*
     C     C1FARA        IFEQ      *ZEROS
     C                   MOVE      COFARA        SCFARA
     C                   ELSE
     C                   MOVE      C1FARA        SCFARA
     C                   ENDIF
     C*
     C     C1FAX#        IFEQ      *ZEROS
     C                   MOVE      COFAX#        SCFPHN
     C                   ELSE
     C                   MOVE      C1FAX#        SCFPHN
     C                   ENDIF
     C*
     C     C1FEIN        IFEQ      *ZEROS
     C                   MOVE      COFEIN        SCFEIN
     C                   ELSE
     C                   MOVE      C1FEIN        SCFEIN
     C                   ENDIF
     C*
     C     C1LCS#        IFLE      *ZEROS
     C                   MOVE      COCASE        SCCASE
     C                   ELSE
     C                   MOVE      C1LCS#        SCCASE
     C                   ENDIF
     C*
     C     C1LIN#        IFLE      *ZEROS
     C                   MOVE      COINV#        SCINV#
     C                   ELSE
     C                   MOVE      C1LIN#        SCINV#
     C                   ENDIF
     C*
     C     C1EOB#        IFLE      *ZEROS
     C                   MOVE      0             SCEOB#
     C                   ELSE
     C                   MOVE      C1EOB#        SCEOB#
     C                   ENDIF
     C*
     C     C1LTR#        IFLE      *ZEROS
     C                   Z-ADD     0             SCLTR#
     C                   ELSE
     C                   Z-ADD     C1LTR#        SCLTR#
     C                   ENDIF
     C*** BEGIN ADD *** 10/31/06 **************************************
     C                   MOVEL     C110A3        SC10A3
     C*** END   ADD *** 10/31/06 **************************************
     C*** BEG ADD *** 04/02/09 **************************************
     C                   MOVE      C110A4        SC10A4
     C*** END ADD *** 04/02/09 **************************************
     C*** Beg Add *** 07/17/14 **************************************
     C                   eval      scTsid = C1tsetID#
     C*** End Add *** 07/17/14 **************************************
      /Free
       //* Beg Add *** 02/12/18 **************************************
       scRAID = C1raid ;
       //* End Add *** 02/12/18 **************************************
      /End-Free
     C*
     C                   ENDIF
     C     @@MODE        IFEQ      'A'
     C                   MOVE      COCO#         SCCO#
     C                   MOVE      COSTAT        SCSTAT
     C                   MOVEL     CONAME        SCNAM
     C                   MOVE      COADR1        SCADR1
     C                   MOVE      COADR2        SCADR2
     C                   MOVE      COCTY         SCCTY
     C                   MOVE      COST          SCST
     C                   MOVEL     COZIP         SCZIP1
     C                   MOVE      COZIP         SCZIP2
     C                   MOVE      COFEIN        SCFEIN
     C                   MOVE      COAREA        SCAREA
     C                   MOVE      COPHN#        SCPHN#
     C                   MOVE      COFARA        SCFARA
     C                   MOVE      COFAX#        SCFPHN
     C     COCASE        IFLE      *ZEROS
     C                   MOVE      0             SCCASE
     C                   ELSE
     C     COCASE        SUB       1             SCCASE
     C                   ENDIF
     C*
     C     COINV#        IFLE      *ZEROS
     C                   MOVE      0             SCINV#
     C                   ELSE
     C     COINV#        SUB       1             SCINV#
     C                   ENDIF
     C*
     C     C1EOB#        IFLE      *ZEROS
     C                   MOVE      0             SCEOB#
     C                   ELSE
     C                   MOVE      C1EOB#        SCEOB#
     C                   ENDIF
     C*
     C     C1LTR#        IFLE      *ZEROS
     C                   Z-ADD     0             SCLTR#
     C                   ELSE
     C                   Z-ADD     C1LTR#        SCLTR#
     C                   ENDIF
     C*** BEG ADD *** 04/02/09 **************************************
     C                   MOVE      C110A4        SC10A4
     C*** END ADD *** 04/02/09 **************************************
     C*** Beg Add *** 07/17/14 **************************************
     C                   eval      scTsid = C1tsetID#
     C*** End Add *** 07/17/14 **************************************
      /Free
       //* Beg Add *** 02/12/18 **************************************
       scRAID = *Zeros;
       //* End Add *** 02/12/18 **************************************
      /End-Free
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      SCCO#         C1CO#
     C                   MOVEL     SCSTAT        C1STAT
     C                   MOVEL     SCNAM         C1NAME
     C                   MOVEL     SCADR1        C1ADR1
     C                   MOVEL     SCADR2        C1ADR2
     C                   MOVEL     SCCTY         C1CTY
     C                   MOVEL     SCST          C1ST
     C                   MOVEL     SCZIP1        C1ZIP
     C                   MOVE      SCZIP2        C1ZIP
     C                   Z-ADD     SCFEIN        C1FEIN
     C                   Z-ADD     SCAREA        C1AREA
     C                   Z-ADD     SCPHN#        C1PHN#
     C                   Z-ADD     SCFARA        C1FARA
     C                   Z-ADD     SCFPHN        C1FAX#
     C                   Z-ADD     SCCASE        C1LCS#
     C                   Z-ADD     SCEOB#        C1EOB#
     C                   Z-ADD     SCINV#        C1LIN#
     C                   Z-ADD     SCLTR#        C1LTR#
     C*** BEGIN ADD *** 10/31/06 **************************************
     C                   MOVEL     SC10A3        C110A3
     C*** END   ADD *** 10/31/06 **************************************
     C*** BEG ADD *** 04/02/09 **************************************
     C                   MOVE      SC10A4        C110A4
     C*** END ADD *** 04/02/09 **************************************
     C*** Beg Add *** 07/17/14 **************************************
     C                   eval      C1tsetID# = scTsid
     C*** End Add *** 07/17/14 **************************************
      /Free
       //* Beg Add *** 02/12/18 **************************************
       C1raid = scRAID;
       //* End Add *** 02/12/18 **************************************
      /End-Free
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
     C     *INKJ         CASEQ     *ON           KJDO
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
     C*
     C     @@MODE        COMP      'D'                                    09
     C*
     C                   SETOFF                                       303150
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C     ##AR          IFEQ      'Y'
     C                   SETON                                        50
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
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
