     H* (C) Copyright Progressive Data Solutions, Inc., 2001
     H*
     H/TITLE W8126R3 - Build A/R Detail Workfile(WXX1P)
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H*****************************************************************
     H* Created: 10/15/01  C:61  Project WC970                        *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 02/19/04 C:136 Project: WC1033                                *
     H*                Add processing for new A/R type = 'T' (true    *
     H*                void)                                          *
     H*                                                               *
     H* 12/28/01 C:15  Project: WC970                                 *
     H*                Added code to prevent duplicate entries.       *
     H*                                                               *
     H*****************************************************************
      *
      *  Group Control
     FWXPGL     IF   E           K DISK
      *
      *  A/R Detail
     FWMEAL20   IF   E           K DISK
     FWMEAL13   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
      *
      *  Table (A/R transaction type code table, WTTTP)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
      *
      *  Employer Policy Year
     FWDELL     IF   E           K DISK
      *
      * Rating State
     FWMRSL     IF   E           K DISK
      *
      *  Employer Master
     FWMEML     IF   E           K DISK
      *
      *  Agency Billing Detail
     FWDABL7    IF   E           K DISK
      *
      *  A/R Detail Work File (Clarendon)
     FWXX1P     O    E             DISK
      *
      *
      *
     D/copy wpragent
     D/copy wprAR
     D/copy wpremplr
      *
      * Effective and Cancel Date Arrays
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D agentName       s             50
     D AHrrn           s              9  0
      *
      *
      *=================================================================
      *  KLIST
      *=================================================================
      *
      *  Pre-Processor Group
      *
     C     PGKY          KLIST
     C                   KFLD                    KKCO#
      *
      *  A/R Detail
      *
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PGGRP
     C                   KFLD                    KKFRDT
      *
     C     EAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PGGRP
      *
      *  Table
      *
     C     TABKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
      *
      *  Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
      *
     C     ELKY1         KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    ABFND
     C                   KFLD                    ABEMP#
     C                   KFLD                    ABDIV
     C                   KFLD                    ABFYR
      *
      * Rating State
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
      *
      *  Agency Billing Detail
      *
     C     ABKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PGGRP
      *
      *
      *
      *=================================================================
      *  *LIKE
      *=================================================================
     C     *LIKE         DEFINE    X1PLAN        ##PLAN
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EATRDT        XX7DT
     C     *LIKE         DEFINE    EMEFD1        XXEFDX
     C     *LIKE         DEFINE    EAPODT        SVDATE
     C     *LIKE         DEFINE    EATRDT        KKFRDT
     C     *LIKE         DEFINE    EATRDT        KKTODT
     C     *LIKE         DEFINE    TTKEY         KKKEY
      *
      *
      *
      *=================================================================
      *  PLIST
      *=================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *
      *=================================================================
      *
      * Initialize
      *
     C                   MOVE      @@CO#         KKCO#
      *
     C                   MOVEL     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFRDT
      *
     C                   MOVEL     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTODT
      *
      *=================================================================
      *
     C     PGKY          Setll     XPG01
      *
     C                   Dou       %Eof (Wxpgl)
      *
     C     PGKY          READE     Wxpgl
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Pgflag <> 'I'
     C                   Iter
     C                   Endif
      *
      /eject
      *
     C     EAKY          SETLL     PEA01
     C     EAKY1         READE     PEA01                                  60
      *
      * Select if trans date is less/equal month end date.
      *
     C                   DOW       *In60 = *Off and
     C                             EATRDT <= KKTODT
      *
      * Cash Items
      *
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'R'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'Q'
     C     EAARTY        OREQ      'I'
      *** beg add ***** 02/19/04 **********************************************
     C     EAARTY        OREQ      'T'
      *** end add ***** 02/19/04 **********************************************
     C                   Z-ADD     EATRDT        SVDATE
     C                   EXSR      SRSEL
     C                   ELSE
      *
      * Bill Items if trans date greater/equal posting date
      *
     C     EATRDT        IFGE      EAPODT
      *
     C                   Z-ADD     EATRDT        SVDATE
     C                   EXSR      SRSEL
      *
     C                   ENDIF
     C                   ENDIF
      *
     C     EAKY1         READE     PEA01                                  60
     C                   ENDDO
      *
      /eject
      *
     C     EAKY          SETLL     PEA02
     C     EAKY1         READE     PEA02                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     EAPODT        ANDLE     KKTODT
      *
      * Bill items if posting date is less than month end date
      *
     C     EATRDT        IFLT      EAPODT
     C     EAARTY        IFNE      'P'
     C     EAARTY        ANDNE     'I'
     C     EAARTY        ANDNE     'N'
     C     EAARTY        ANDNE     'Q'
     C     EAARTY        ANDNE     'R'
     C     EAARTY        ANDNE     'V'
      *** beg add ***** 02/19/04 **********************************************
     C     EAARTY        ANDNE     'T'
      *** end add ***** 02/19/04 **********************************************
     C                   Z-ADD     EAPODT        SVDATE
     C                   EXSR      SRSEL
     C                   ENDIF
      *                                                                     ****
     C                   ENDIF
      *
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
      *
      /eject
      *
     C     ABKY          CHAIN     PAB01                              60
      *
     C                   DOW       *In60 = *OFF
      *
     C     ABTRDT        IFGE      KKFRDT
     C     ABTRDT        ANDLE     KKTODT
      *
     C     ABARTY        IFEQ      'P'
     C     ABARTY        OREQ      'N'
     C     ABARTY        OREQ      'V'
     C     ABARTY        OREQ      'R'
     C     ABARTY        OREQ      'Q'
     C     ELKY1         CHAIN     PEL01
      *
     C                   If        %Found
     C                   CLEAR                   PX101
      *
      * Write to work file WXX1P
      *
     C                   Z-ADD     ABCO#         X1CO#
     C                   Z-ADD     ABFND         X1FND
     C                   Z-ADD     ABEMP#        X1EMP#
     C                   Z-ADD     ABDIV         X1DIV
     C                   Z-ADD     ABFYR         X1FYR
      *
      *  Get Rating State
      *
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        X1ST
      *
     C                   MOVE      ABARCD        X1ARCD
     C                   MOVE      ABARTY        X1ARTY
      *
     C                   MOVEL(P)  ABARCD        KKKEY
     C     TABKY         CHAIN     PTT01                              60
     C  N60              MOVE      TTCTYP        X1CTYP
      *
     C                   Z-ADD     ABAMT         X1AMT
     C                   Z-ADD     ABCHK#        X1CHK#
     C                   Z-ADD     ABTRDT        X1TRDT
     C                   Z-ADD     ABPODT        X1PODT
     C                   Z-ADD     ABTRDT        X1DATE
      *
     C                   eval      X1AGT# = GetEmpAgt(ABCO#:ABFND:ABEMP#:
     C                                      ABDIV:ABFYR:ABTRDT)
      *
     C                   MOVE      'A'           X1PLAN
      *
     C                   MOVEL     Q1PGM         X1PGM
     C                   MOVEL     Q1USER        X1USER
     C                   TIME                    DSDTTM
     C                   MOVEL     DSTIME        X1TIME
      *
     C                   MOVEL     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DT
      *
     C                   WRITE     PX101
     C                   ENDIF                                                  ELKY
      *
     C                   ENDIF                                                  ABARTY
      *
     C                   ENDIF                                                  ABTRDT
      *
     C     ABKY          READE     PAB01                                  60
     C                   ENDDO                                                  ABKY
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
      *=================================================================
      * SRSEL - Write record to A/R Detail work file WXX1P
      *=================================================================
     C     SRSEL         BEGSR
     C                   CLEAR                   PX101
     C                   MOVE      'D'           X1PLAN
     C     ELKY          CHAIN     PEL01
      *
     C                   IF        %Found
      *
      *  Direct Bill commission is based upon premium payments
      *
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'R'
      *** BEG ADD ***** 02/19/04 **********************************************
     C     EAARTY        OREQ      'T'
      *** END ADD ***** 02/19/04 **********************************************
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TABKY         CHAIN     PTT01
      *
     C                   IF        %Found and
     C                             TTECFL <> 'Y'
     C                   eval      X1AGT# = getEmpAgt(EACO#:EAFND:EAEMP#:
     C                                        EADIV:EAFYR:EATRDT:
     C                                        agentName:AHrrn)
     C                   eval      ##PLAN = GetEmpPlan(EACO#:EAFND:EAEMP#:
     C                                      EADIV:EAFYR:EATRDT)
      *
     C     ##PLAN        IFEQ      *BLANK
     C                   eval      X1CRAT = EmpCommRt(AHrrn:EAARCD:EATRDT)
     C                   eval      X1CAMT = EAAMT * (X1CRAT/100)
      *
     C     EAARTY        IFEQ      'N'
     C     EAARTY        OREQ      'R'
     C                   eval      X1CAMT = X1CAMT * (-1)
     C                   ENDIF
      *
     C                   MOVE      'D'           X1PLAN
      *
     C                   ELSE
      *
     C                   MOVE      'A'           X1PLAN
     C                   ENDIF                                                  ##PLAN = *BLANK
      *
     C                   ENDIF                                                  TTECFL <> Y
      *
     C                   ENDIF                                                  ARTY = P N V R
      *
      *  Agent Current Commission is based upon premium billings
      *
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TABKY         CHAIN     PTT01
      *
     C                   IF        %Found and
     C                             TTECFL <> 'Y'
     C                   eval      X1AGT# = getEmpAgt(EACO#:EAFND:EAEMP#:
     C                                        EADIV:EAFYR:EAPODT:
     C                                        agentName:AHrrn)
     C                   eval      ##PLAN = GetEmpPlan(EACO#:EAFND:EAEMP#:
     C                                      EADIV:EAFYR:EAPODT)
      *
     C     ##PLAN        IFEQ      'A'
     C                   eval      X1CRAT = EmpCommRt(AHrrn:EAARCD:EAPODT)
     C                   eval      X1CAMT = EAAMT * (X1CRAT/100)
     C                   MOVE      'A'           X1PLAN
      *
     C                   ELSE
      *
     C                   MOVE      'D'           X1PLAN
     C                   ENDIF                                                  ##PLAN = A
      *
     C                   ENDIF                                                  TTECFL <> Y
      *
     C                   ENDIF                                                  ARTY = B A D
      *
      * Write to work file WXX1P
      *
     C                   Z-ADD     EACO#         X1CO#
     C                   Z-ADD     EAFND         X1FND
     C                   Z-ADD     EAEMP#        X1EMP#
     C                   Z-ADD     EADIV         X1DIV
     C                   Z-ADD     EAFYR         X1FYR
      *
      *  Get Rating State
      *
     C     RSKY          CHAIN     PRS01
     C                   IF        %Found
     C                   MOVE      RSJRST        X1ST
     C                   Endif
      *
     C                   MOVE      EAARCD        X1ARCD
     C                   MOVE      EAARTY        X1ARTY
      *
     C                   MOVEL(P)  EAARCD        KKKEY
     C     TABKY         CHAIN     PTT01
     C                   IF        %Found
     C                   MOVE      TTCTYP        X1CTYP
     C                   Endif
      *
     C                   Z-ADD     EAAMT         X1AMT
     C                   Z-ADD     EACHK#        X1CHK#
     C                   Z-ADD     EATRDT        X1TRDT
     C                   MOVEL     EAPODT        X1PODT
     C                   MOVEL     SVDATE        X1DATE
      *
     C                   MOVEL     Q1PGM         X1PGM
     C                   MOVEL     Q1USER        X1USER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        X1TIME
      *
     C                   MOVEL     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DT
      *
     C                   WRITE     PX101
     C                   ENDIF                                                  ELKY
      *
     C                   ENDSR                                                  SRSEL
      *
      *
      *
