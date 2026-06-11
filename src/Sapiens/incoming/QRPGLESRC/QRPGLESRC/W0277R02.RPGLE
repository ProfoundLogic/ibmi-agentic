     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0277R02 - Agent Contacts
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0277R                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED:   01/18/06 C:168 PROJECT 93WFP                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/20/21 C:298 Project: WC2278                                *
     H*               -Multiple selection entry are added             *
     H*                for Producers.                                 *
      *          C:33 -Multiple selection entries are added for       *
      *                Distribution Types,Sub-Categories,Process      *
      *                Output Control and their handling.             *
     H*                                                               *
     H* 10/25/10  C:33  Project: SY190  Priss #199                    *
     H*                 Position cursor after search(s).              *
     H*****************************************************************
      *
     FW0277F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      *
      *  Agent Contacts
     FWMA5P     UF A E             DISK
     F                                     RENAME(PAC01:DBRCDF)
     F*WMECL1  IF  E           K        DISK
     F*                                              KINFDS ECFDS
     FWMA5L     IF   E           K DISK
     F                                     INFDS(EC2FDS)
     F                                     RENAME(PAC01:PAC02)
      *
      *  Agent Contact Type
     FWTAYL1    IF   E           K DISK
      *
      *  Agent Master
     F*WMAGP   IF  E                    DISK
      *
      *  Group Master
     F*WMFNL   IF  E           K        DISK
      *
      *  Admin/Darts Control
     F*MEXL   IF  E           K        DISK
      *
      *  System State
     FSTSTL3    IF   E           K DISK
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      *
      *
      *
      *
      ***** Beg Add ***** 04/20/21 *********************************
     fwma5pcrt  uf a e           k disk    rename(pac01:pac0p)
     f                                     prefix(CR_)
      ***** End Add ***** 04/20/21 *********************************
      /COPY SCOPYMT01D
      ***** Beg Add ***** 04/20/21 *********************************
     d/copy sPrString
      ***** End Add ***** 04/20/21 *********************************
     D ECFDS           DS
     D  ECRRN                397    400B 0
      *
     D EC2FDS          DS
     D  EC2RRN               397    400B 0
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
      ***** Beg Add ***** 04/20/21 *********************************
     d maxseq#         s             15  0 inz
      ***** End Add ***** 04/20/21 *********************************
      *
      *
     D***PY SCOPYMT01I
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Agent Contact -
     D                                     Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          WMA5P
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FEIN            9 0
     C                   PARM                    @@MOD             5 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  System State Code Table
      *
     C     STKY          KLIST
     C                   KFLD                    KKST             10
      *
      *  Agent Contacts
      *
     C     ECKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCACTY
      *
     C     ECKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    SCLSN
     C                   KFLD                    SCFSN
     C                   KFLD                    SCMID
     C*                    KFLD           SCPOSN
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    A5CO#         KKCO#
     C     *LIKE         DEFINE    A5FEIN        KKFEIN
     C     *LIKE         DEFINE    A5MOD         KKMOD
      *
     C                   MOVEL     'A'           SCSTAT
      *
     C/COPY SCOPYMT01C
      *
      *
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
     C     A5STAT        IFNE      'A'
     C                   MOVE      'A'           A5STAT
     C                   ELSE
     C                   MOVE      'I'           A5STAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        A5USER
     C                   MOVEL     Q1PGM         A5PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A5DT
     C*
     C                   MOVEL     DSTIME        A5TIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
      *
     C                   ENDSR                                                  DLTPRE
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
      *
     C                   ENDSR                                                  DSPLYR
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      * Last Name is required entry
      *
     C  N90SCLSN         IFEQ      *BLANKS
     C                   SETON                                        90  71
     C                   ENDIF
      *
      * Either Title or First Name must be entered.
      *
     C  N90SCTITL        IFEQ      *BLANKS
     C     SCFSN         ANDEQ     *BLANKS
     C                   SETON                                        90  72
     C                   ENDIF
      *
      * State must be valid if entered
      *
     C  N90SCST          IFNE      *BLANK
     C                   MOVEL     SCST          KKST
     C     STKY          CHAIN     PST01                              60
     C   60              SETON                                        90  78
     C                   ENDIF
      *
      * Contact type must be entered
      *
     C* N90      SCACTY    IFEQ *BLANK
     C*                    SETON                     90  73
     C*                    ENDIF
      *
      * Check the contact type if it is valid
      *
     C  N90SCACTY        IFNE      *BLANK
     C     ECKY1         CHAIN     PAY01                              60
     C   60              SETON                                        90  74
     C                   ENDIF
      *
      * If Phone Number, Cell Number and Primary Contact entered THEN
      * Primary Phone Flag must be entered.
      *
     C  N90SCPH1         IFNE      *ZERO
     C     SCCPH1        ANDNE     *ZERO
     C     SCPPHF        ANDEQ     *BLANK
     C                   SETON                                        90  75
     C                   ENDIF
      *
      * Primary Phone Flag must be valid if entered
      *
     C  N90SCPPHF        IFNE      *BLANK
     C     SCPPHF        ANDNE     'P'
     C     SCPPHF        ANDNE     'C'
     C                   SETON                                        90  76
     C                   ENDIF
      *
      * Record must have a unique key
      *    co#/fein/mod/lsn/fsn/mid/posn
      *
     C     *IN90         IFEQ      *OFF
      *
     C     @@MODE        IFEQ      'A'
     C     ECKY2         CHAIN     PAC02                              60
      *
     C     *IN60         IFEQ      *OFF
     C     SCACTY        ANDEQ     A5ACTY
     C     EC2RRN        ANDNE     @@RRN
     C                   SETON                                        90  77
     C                   ENDIF
     C                   ENDIF                                                  @@MODE = A
      *
     C     @@MODE        IFEQ      'C'
     C     ECKY2         CHAIN     PAC02                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EC2RRN        IFNE      @@RRN
     C     SCACTY        ANDEQ     A5ACTY
     C                   SETON                                        90  77
     C                   LEAVE
     C                   ENDIF
      *
     C     ECKY2         READE     PAC02                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  @@MODE = C
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N90A5DT          IFGT      SAVEDT
     C     A5DT          OREQ      SAVEDT
     C     A5TIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  EMDT>SAVEDT
      *
     C                   ENDSR                                                  ERRCK1
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
      *
     C                   ENDSR                                                  ERRCK2
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
      *
     C                   ENDSR                                                  ERRCK3
      *================================================================
      * KJDO - F10=Search
      *================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      * State - PROMPT
      *
     C     HFLD          WHENEQ    'SCST'
      *
     C                   CALL      'SY601R'
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      *ZEROS        #1RRN             9 0
      *
     C     #1RRN         IFEQ      *ZERO
     C                   SETON                                        79
      *
     C                   ELSE
      *
     C     #1RRN         CHAIN     PST02                              99
     C                   MOVEL     STKEY         SCST
     C                   SETON                                        80
     C                   ENDIF
      *
      * Contact Type - PROMPT
      *
     C     HFLD          WHENEQ    'SCACTY'
      *
     C                   CALL      'W2287R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C***  Begin Chg  ***  10/25/10  **************************************
     C**********           PARM           ##KEY   3
     C                   PARM      *BLANKS       ##KEY             3
     C***  End   Chg  ***  10/25/10  **************************************
     C                   PARM      1             ##VIEW            1 0
      *
     C***  Begin Chg  ***  10/25/10  **************************************
     C**********           MOVEL##KEY     SCACTY
     C                   SELECT
     C     ##KEY         WHENNE    *BLANKS
     C                   MOVEL     ##KEY         SCACTY
     C                   SETON                                        22
     C                   OTHER
     C                   SETON                                        21
     C                   ENDSL
     C***  End   Chg  ***  10/25/10  **************************************
      *
     C                   OTHER
     C                   SETON                                        81
      *
     C                   ENDSL
      *
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL(P)  A5TITL        SCTITL
     C                   MOVEL(P)  A5FSN         SCFSN
     C                   MOVEL(P)  A5MID         SCMID
     C                   MOVEL(P)  A5LSN         SCLSN
     C                   MOVEL(P)  A5POSN        SCPOSN
     C                   MOVEL(P)  A5ADR1        SCADR1
     C                   MOVEL(P)  A5ADR2        SCADR2
     C                   MOVEL(P)  A5CTY         SCCTY
     C                   MOVEL(P)  A5ST          SCST
      *
     C                   MOVEL     A5ZIP         SCZIP5
     C                   MOVE      A5ZIP         SCZIP4
      *
     C                   MOVEL     A5ACTY        SCACTY
     C                   MOVEL     A5STAT        SCSTAT
     C     SCSTAT        IFEQ      *BLANKS
     C                   MOVEL     'A'           SCSTAT
     C                   ENDIF
      *
     C                   Z-ADD     A5AREA        SCAREA
     C                   MOVEL     A5PHN#        SCPH1
     C                   MOVE      A5PHN#        SCPH2
     C                   Z-ADD     A5EXT#        SCEXT#
      *
     C                   Z-ADD     A5FARA        SCFARA
     C                   MOVEL     A5FPHN        SCFPH1
     C                   MOVE      A5FPHN        SCFPH2
      *
     C                   Z-ADD     A5CARA        SCCARA
     C                   MOVEL     A5CPHN        SCCPH1
     C                   MOVE      A5CPHN        SCCPH2
      *
     C                   MOVE      A5PPHF        SCPPHF
     C                   MOVEL(P)  A5EMAL        SCEMAL
      ***** Beg Add ***** 04/20/21 *************************************
      /Free
       Clear selProd;

       selProd = 'APco# = ' + %char(kkco#) + ' AND APFein = '
        + %char(kkFein) + ' AND APMod  = ' + %char(kkMod);
       scProd = ' ';
       Clear scDisTyp;
       Clear scSubCat;
       Clear scOutCnt;
       Setll (kkco#:kkFein:kkMod:A5seq#) pac0p;
       Reade(n) (kkco#:kkFein:kkMod:A5seq#) pac0p;
       Dow not %eof(wma5pcrt);
          Select;
          When CR_A5critID = 'Producer';
             scProd = %trim(scProd) + ',' + %trim(CR_A5critVal);
          When CR_A5critID = 'Distribution';
             scDisTyp = %trim(scDisTyp) + ',' + %trim(CR_A5critVal);
          When CR_A5critID = 'Sub Category';
             scSubCat = %trim(scSubCat) + ',' + %trim(CR_A5critVal);
          When CR_A5critID = 'Process Output Control';
             scOutCnt = %trim(scOutCnt) + ',' + %trim(CR_A5critVal);
          Endsl;
          Reade(n) (kkco#:kkFein:kkMod:A5seq#) pac0p;
       Enddo;
       If scProd <> *blank;
          scProd = %subst(scProd:2);
       Endif;
       If scDisTyp <> *blank;
          scDisTyp = %subst(scDisTyp:2);
       Endif;
       If scSubCat <> *blank;
          scSubCat = %subst(scSubCat:2);
       Endif;
       If scOutCnt <> *blank;
          scOutCnt = %subst(scOutCnt:2);
       Endif;
      /End-free
      ***** End Add ***** 04/20/21 *************************************
      *
     C                   ENDSR                                                  MVDBSC
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C     SCSTAT        IFEQ      *BLANKS
     C                   MOVEL     'A'           SCSTAT
     C                   ENDIF
      *
     C                   Z-ADD     KKCO#         A5CO#
     C                   Z-ADD     KKFEIN        A5FEIN
     C                   Z-ADD     KKMOD         A5MOD
     C                   MOVEL(P)  SCTITL        A5TITL
     C                   MOVEL(P)  SCFSN         A5FSN
     C                   MOVEL(P)  SCMID         A5MID
     C                   MOVEL(P)  SCACTY        A5ACTY
     C                   MOVEL(P)  SCSTAT        A5STAT
     C                   MOVEL(P)  SCLSN         A5LSN
     C                   MOVEL(P)  SCPOSN        A5POSN
     C                   MOVEL(P)  SCADR1        A5ADR1
     C                   MOVEL(P)  SCADR2        A5ADR2
     C                   MOVEL(P)  SCCTY         A5CTY
     C                   MOVEL(P)  SCST          A5ST
     C     SCZIP5        MULT      10000         A5ZIP
     C                   ADD       SCZIP4        A5ZIP
     C                   Z-ADD     SCAREA        A5AREA
     C     SCPH1         MULT      10000         A5PHN#
     C                   ADD       SCPH2         A5PHN#
     C                   Z-ADD     SCEXT#        A5EXT#
     C                   Z-ADD     SCFARA        A5FARA
     C     SCFPH1        MULT      10000         A5FPHN
     C                   ADD       SCFPH2        A5FPHN
     C                   Z-ADD     SCCARA        A5CARA
     C     SCCPH1        MULT      10000         A5CPHN
     C                   ADD       SCCPH2        A5CPHN
     C                   MOVE      SCPPHF        A5PPHF
     C                   MOVEL(P)  SCEMAL        A5EMAL
      ***** Beg Add ***** 04/20/21 *****************************
      /Free
       If @@mode = 'C';
          Setll (kkco#:kkfein:kkmod:A5seq#) pac0p;
          Reade (kkco#:kkfein:kkmod:A5seq#) pac0p;
          Dow not %eof(wma5pcrt);
             Delete wma5pcrt;
             Reade (kkco#:kkfein:kkmod:A5seq#) pac0p;
          Enddo;
          Unlock wma5pcrt;
       Endif;
       If scProd <> ' ';
          Dow scProd  <> ' ';
             CR_A5critID  = 'Producer';
             CR_A5critVal = ParseString(scProd:',':1:'1');
             Exsr Write_CritRec;
          Enddo;
       Endif;
       Dow scDisTyp <> *blank;
          CR_A5critID  = 'Distribution';
          CR_A5critVal = ParseString(scDisTyp:',':1:'1');
          Exsr Write_CritRec;
       Enddo;
       Dow scSubCat <> *blank;
          CR_A5critID  = 'Sub Category';
          CR_A5critVal = ParseString(scSubCat:',':1:'1');
          Exsr Write_CritRec;
       Enddo;
       Dow scOutCnt <> *blank;
          CR_A5critID  = 'Process Output Control';
          CR_A5critVal = ParseString(scOutCnt:',':1:'1');
          Exsr Write_CritRec;
       Enddo;
      /End-free
      ***** End Add ***** 04/20/21 *****************************
      *
     C                   ENDSR                                                  MVSCDB
      ***** Beg Add ***** 04/20/21 *****************************
      /Free
       Begsr Write_CritRec;
          CR_A5co#   = kkco#;
          CR_A5fein  = kkfein;
          CR_A5mod   = kkmod;
          CR_A5seq#  = A5seq#;
          CR_A5stat  = A5stat;
          CR_A5iusr  = q1user;
          CR_A5ipgm  = q1pgm;
          CR_A5user  = q1user;
          CR_A5pgm   = q1pgm;
      /End-Free
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CR_A5idt
     C                   MOVEL     SRDTO         CR_A5DT
     C                   MOVEL     DSTIME        CR_A5itim
     C                   MOVEL     DSTIME        CR_A5TIME
      /Free
          Write pac0p;
          Unlock wma5pcrt;
       Endsr;
      /End-free
      ***** End Add ***** 04/20/21 *****************************
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
      *
     C                   ENDSR                                                  OTHERR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90     SEARCHES
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
     C                   ENDSR                                                  PGMEND
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD         XC
      *
      *
      *  Get Employer record
      *
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FEIN        KKFEIN
     C                   Z-ADD     @@MOD         KKMOD
      *
     C                   Z-ADD     *ZEROS        X                 3 0
      *
     C                   ENDSR                                                  PGMSTR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR                                                  PREOTH
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        A5USER
     C                   MOVEL     Q1PGM         A5PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A5DT
     C                   MOVEL     DSTIME        A5TIME
      *
     C                   UPDATE    DBRCDF
     C                   ENDIF
      *
     C                   ENDSR                                                  UPDATR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
      ***** Beg Add ***** 04/20/21 ****************************************
      /Free
       Exec SQL Select Max(A5seq#) Into :maxseq#  From Wma5l
       Where A5co# = :kkco# And A5fein = :kkfein And A5mod = :kkmod;
       A5seq# = maxseq# + 1;
      /End-free
      ***** End Add ***** 04/20/21 ****************************************
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        A5IUSR
     C                   MOVEL     Q1USER        A5USER
     C                   MOVEL     Q1PGM         A5IPGM
     C                   MOVEL     Q1PGM         A5PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A5IDT
     C                   MOVEL     SRDTO         A5DT
     C                   MOVEL     DSTIME        A5ITIM
     C                   MOVEL     DSTIME        A5TIME
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMA5P
      *
     C                   ENDSR                                                  WRITER
