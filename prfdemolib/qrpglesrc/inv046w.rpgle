      * @MBRTEXT: INQ - INVENTORY BAL ALL STORES - VW web (EJS) version
/* @#$* Copyright Statement ********* Do not remove *********/
/************************************************************/
/************************************************************/
/******                                                 *****/
/******  Copyright JDA Software Services Inc. All       *****/
/******  rights reserved. This program is protected     *****/
/******  by the copyright Act of 1976, as amended, as   *****/
/******  an unpublished work and the foregoing notice   *****/
/******  and legend shall not be deemed to constitute   *****/
/******  publication or an intent to publish thereunder.*****/
/******  This program is proprietary and confidential   *****/
/******  information of JDA Software Services Inc., and *****/
/******  may be disclosed and used only as authorized   *****/
/******  in the license Agreement controlling such      *****/
/******  disclosure and use.                            *****/
/******                                                 *****/
/************************************************************/
/************************************************************/
     F*****************************************************************
     F*    THIS PROGRAM WAS WRITTEN BY YUEY LOW  -  JULY 31,1986      *
     F*****************************************************************
     F*                                                               *
     F*  PROGRAM DOES THE FOLLOWING:                                  *
     F*                                                               *
     F*    CURRENT PERIOD INQUIRY TO INVENTORY BALANCES FOR ALL
     F*    STORES                                                     *
     F*                                                               *
     F*****************************************************************
     F* 01/16/02 - MODIFIED TO ADD STORE GROUP INVENTORY INQUIRY     *
     F*            FUNCTIONALITY (F6).                      BWE      *
     F*****************************************************************
     F* 03/27/02 - Kay R. Ayscue                                     *
     F*            Modified crtpf command to not print source listing*
     F*            when file is created.                             *
     F* -------------------------------------------------------------*
     F* 06/06/12 Moss - Modified to add POS flag for stores.         *
     F*                                                              *
     F* 11/03/26 - PRF2 Demo build: calls to programs that are not   *
     F*            in the PRFDEMOLIB restore are commented out       *
     F*            (C*prf): F3, F4, F6, F7, F8 and option I now      *
     F*            just redisplay the screen.                        *
     F*                                                              *
VW01 F* 11/03/26 - VW01 Web clone INV046W (menu option 10). Converted *
VW01 F*            mechanically from RPG III by tools/cvtrpg3.py     *
VW01 F*            (Open Access needs ILE RPG); the converted source *
VW01 F*            was proven screen-for-screen identical to INV046. *
VW01 F*            EJS changes, all tagged VW01: INV046WD + HANDLER, *
VW01 F*            AID in INFDS, EJSOUT/EJSIN around the EXFMT (map  *
VW01 F*            ACTION/AID to KA-KH, emulate CHANGE(02) and       *
VW01 F*            ROLLUP(03)), SCRN3 fields merged into SCRN1,      *
VW01 F*            SFLCLR (*IN50) mirrored into EJSCLR.              *
     F*                                                              *
     F*****************************************************************
VW01 H DFTACTGRP(*NO)
VW01 FINV046WD  CF   E             WORKSTN INFDS(INFDS)
     F                                     SFILE(SCRN2:RRN)
VW01 F                                     HANDLER('PROFOUNDUI(HANDLER)')
     FOPTIONS   IF   E           K DISK    USROPN
     FINVMST    IF   E           K DISK
     FINVBAL    IF   E           K DISK
     FTBLSTR    IF   E           K DISK
     FTBLSTRL1  IF   E           K DISK    RENAME(RTBLSTR:L1FILE)
     FTBLSTRL2  IF   E           K DISK    RENAME(RTBLSTR:L2FILE)
     FTBLREG    IF   E           K DISK
     FTBLDST    IF   E           K DISK
     FTBLADI    IF   E           K DISK
     FINVADI    IF   E           K DISK
     FTBLIGH    IF   E           K DISK
     FTBLIGD    IF   E           K DISK
     FINVCBL    IF   E           K DISK
     FTBL800    IF   E           K DISK
     E* ------------------------------------------------------------------
     E*           A R R A Y            S P E C I F I C A T I O N S
     E* ------------------------------------------------------------------
     DMSG              S             60    DIM(13) CTDATA PERRCD(1)
     DTTL              S             40    DIM(2) CTDATA PERRCD(1)
     DRDS              S             15    DIM(5) CTDATA PERRCD(1)
     DSRC              S             11    DIM(5) CTDATA PERRCD(1)
     DCMD              S             64    DIM(2) CTDATA PERRCD(1)
     DCRTPFC           C                   CONST('CRTPF FILE(QTEMP/WRKSGB) SRC-
     D                                     FILE(CUSTOMR/QDDSSRC) SRCMBR(WRKSGB-
     D                                     ) OPTION(*NOSRC *NOLIST)')
     D                 DS
     D ASL                     1     40S 0 DIM(8)
     D IBWK01                  1      5S 0
     D IBWK02                  6     10S 0
     D IBWK03                 11     15S 0
     D IBWK04                 16     20S 0
     D IBWK05                 21     25S 0
     D IBWK06                 26     30S 0
     D IBWK07                 31     35S 0
     D IBWK08                 36     40S 0
     D                 DS
     D CASL                    1     56S 0 DIM(8)
     D CBWK01                  1      7S 0
     D CBWK02                  8     14S 0
     D CBWK03                 15     21S 0
     D CBWK04                 22     28S 0
     D CBWK05                 29     35S 0
     D CBWK06                 36     42S 0
     D CBWK07                 43     49S 0
     D CBWK08                 50     56S 0
     I*
     D               E DS                  EXTNAME(SYS001ED)
     I*           SYSTEM EXTERNAL DATA STRUCTURE
     D PARM1                   1    256
     I*
     D                SDS
     I*           PROGRAM DATA STRUCTURE
     D QPROG             *PROC
     D QPARMS            *PARMS
     D QJOBN                 244    253
     D QUSER                 254    263
     DINFDS            DS
VW01 D EJSKEY                369    369
     D SFLLOC                378    379B 0
VW01  * EJS (web) display support
VW01 D EJSAID          S              1
VW01 D EJSN            S              3S 0
VW01 D EJSAIDS         C                   CONST(x'3132333435363738393A3B3C-
VW01 D                                     B1B2B3B4B5B6B7B8B9BABBBC')
VW01 D EJSINP          S               *   INZ(%ADDR(*IN))
VW01 D EJSINA          S             99    BASED(EJSINP)
VW01 D EJSNUM          S                   LIKE(INUMBR)
VW01 D EJSTYP          S                   LIKE(INTYPE)
VW01 D EJSRDS          S                   LIKE(INRDS)
VW01 D EJSSTR          S                   LIKE(INSTR)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    INSKU
     C*
     C                   EXSR      HSEKPG
     C*
     C                   MOVE      'INZ01 '      STEP              6
     C*
     C********************************************************************
     C* M A I N L I N E
     C* ------------------------------------------------------------------
     C*
     C     STEP          DOUEQ     *BLANK
     C*
     C     STEP          CASEQ     'INZ01 '      INZ01
     C*
     C     STEP          CASEQ     'DSP01 '      DSP01
     C*
     C     STEP          CASEQ     'EDT01 '      EDT01
     C*
     C     STEP          CASEQ     'EDT02 '      EDT02
     C*
     C                   CAS                     ENDJOB
     C*
     C                   ENDCS
     C*
     C                   ENDDO
     C*****************************************************************
     C* S U B R O U T I N E S
     C* ---------------------------------------------------------------
     C* INZ01  - INITIALIZE SCREEN 1
     C* ---------------------------------------------------------------
     C*
     C     INZ01         BEGSR
     C*
     C                   MOVE      'DSP01 '      STEP
     C*
     C     INUMBR        IFGT      0
     C                   SETON                                        02
     C     INUMBR        CHAIN     INVMST                             99
     C     *IN99         IFEQ      '1'
     C                   GOTO      INZ01Z
     C                   END
     C                   ELSE
     C                   GOTO      INZ01Z
     C                   END
     C*
     C                   EXSR      NOP01
     C*
     C     INZ01Z        ENDSR
     C* ------------------------------------------------------------------
     C* DSP01    DISPLAY SCREEN 1
     C* ------------------------------------------------------------------
     C     DSP01         BEGSR
     C*
VW01 C***                WRITE     SCRN3
VW01 C                   EXSR      EJSOUT
     C                   EXFMT     SCRN1                                        SCRN1
VW01 C                   EXSR      EJSIN
     C*
     C                   MOVE      *BLANKS       ERROR
     C                   MOVEA     '00'          *IN(20)
     C                   MOVEA     '000'         *IN(26)
     C*
     C* CMD 1 EXIT PROGRAM
     C*
     C     *INKA         IFEQ      '1'
     C     QPARMS        IFLE      1
     C                   EXSR      ENDJOB
     C                   ELSE
     C                   EXSR      EXIT
     C                   END
     C                   END
     C*
     C                   Z-ADD     SFLLOC        SFSTRT
     C*
     C* CMD 3 SKU SEARCH
     C*
     C     *INKC         IFEQ      '1'
     C*prf                 CALL 'INV002'
     C*prf                 PARM           INUMBR
     C                   MOVE      *BLANKS       IDESCR
     C     INUMBR        CHAIN     INVMST                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(1)        ERROR
     C                   SETON                                        022021
     C                   MOVE      'INZ01 '      STEP
     C                   GOTO      DSP01Z
     C                   END
     C                   SETON                                        02
     C                   MOVE      'INZ01 '      STEP
     C                   GOTO      DSP01Z
     C                   END                                                    END IF KC
     C*
     C* CMD 5 TOGGLE PTD/YTD
     C*
     C     *INKE         IFEQ      '1'
     C     1             SUB       TOG           TOG
     C                   MOVE      TOG           *IN(41)
     C                   SETON                                        02
     C                   MOVE      'INZ01 '      STEP
     C                   GOTO      DSP01Z
     C                   END
     C*
     C* SEARCH GROUP
     C     *INKD         IFEQ      '1'
     C                   SELECT
     C     INRDS         WHENEQ    'R'
     C*prf                 CALL 'TBL048'
     C*prf                 PARM 0         INREGX  30
     C                   Z-ADD     0             INREGX            3 0
     C     INREGX        IFNE      0
     C                   Z-ADD     INREGX        INSTR
     C                   END
     C*
     C     INRDS         WHENEQ    'D'
     C*prf                 CALL 'TBL049'
     C*prf                 PARM 0         INREGX
     C     INREGX        IFNE      0
     C                   Z-ADD     INREGX        INSTR
     C                   END
     C*
     C     INRDS         WHENEQ    'S'
     C*prf                 CALL 'TBL009'
     C*prf                 PARM 0         XXSTR
     C     XXSTR         IFNE      0
     C                   Z-ADD     XXSTR         INSTR
     C                   END
     C*
     C     INRDS         WHENEQ    'G'
     C*prf                 CALL 'TBL018'
     C*prf                 PARM 0         INREGX
     C     INREGX        IFNE      0
     C                   Z-ADD     INREGX        INSTR
     C                   END
     C*
     C     INRDS         WHENEQ    'I'
     C*prf                 CALL 'TBL064'
     C*prf                 PARM 0         INREGX
     C     INREGX        IFNE      0
     C                   Z-ADD     INREGX        INSTR
     C                   END
     C                   ENDSL
     C*
     C     INSTR         IFGT      0
     C                   SETON                                        02
     C                   ENDIF
     C                   END                                                    *INKD='1'
     C*
     C* F6 = STORE GROUP INVENTORY INQUIRY
     C     *INKF         IFEQ      '1'
     C*
     C                   MOVEL     CMD(1)        SBM
     C                   CALL      'QCMDEXC'     CMDEXC                 30      Clear workfile
     C     *IN30         IFEQ      '1'                                          If error,
     C*kra                 MOVELCMD,2     SBM              Create workfile
     C                   MOVE      *BLANKS       SBM
     C                   MOVEL     CRTPFC        SBM                            Create workfile
     C                   CALL      'QCMDEXC'     CMDEXC                 30
     C                   ENDIF
     C*
     C*prf                 CALL 'INV860'  INV860
     C*prf                 CALL 'INV861'  INV861
     C                   ENDIF
     C*
     C*
     C* F7 = INV EVALUATION INQ W/ PRICE & COST
     C     *INKG         IFEQ      '1'
     C                   MOVE      *ON           FLG417
     C*prf                 CALL 'INV417'  INV417
     C                   END
     C*
     C* F8 = CHAIN INVENTORY INQS
     C     *INKH         IFEQ      '1'
     C*prf                 CALL 'INV416'
     C*prf                 PARM INUMBR    XXSKU
     C                   END
     C*
     C*-------------------------------------------------------
     C* SEE IF THERE WAS A SELECTION (IF SUBFILE NOT EMPTY)
     C*
     C     *IN51         IFEQ      '1'
     C                   SETOFF                                       90
     C                   READC     SCRN2                                  90
     C     *IN90         DOWEQ     '0'
     C*
     C     SEL           IFEQ      'I'
     C                   EXSR      CALC
     C                   END
     C*
     C                   MOVE      ' '           SEL
     C                   UPDATE    SCRN2
     C*
     C                   READC     SCRN2                                  90
     C                   END                                                    END DO 90
     C                   END                                                    END IF 51
     C*
     C*-------------------------------------------------------
     C* NEW SEARCH FIELD ENTERED?
     C*
     C* INDICATOR 02 - ONE OR MORE FIELDS ON SCRN1 CHANGED.
     C*
     C* ONE OR MORE SELECTION FIELDS HAVE CHANGED.
     C*
     C     *IN02         IFEQ      '1'
     C                   MOVE      'EDT02 '      STEP
     C                   GOTO      DSP01Z
     C                   END
     C*
     C*-------------------------------------------------------
     C* ROLLUP KEY ACTIVATED, LOAD NEXT PAGE
     C*
     C* INDICATOR 03 MEANS THE ROLLUP KEY WAS PRESSED AND SUB-FILE
     C* WAS FULL, THEREFORE, FILL UP ANOTHER SUBFILE PAGE.
     C*
     C     *IN03         IFEQ      '1'
     C                   EXSR      LSF01
     C                   END
     C*
     C* *IN51 IS SET TO "0" WHEN THE SUBFILE IS EMPTY.
     C*
     C  N51              GOTO      DSP01Z
     C*
     C* OTHERWISE READ THE CHANGES TO THE SUBFILE AND EDIT.
     C*
     C                   MOVE      'EDT01 '      STEP
     C*
     C     DSP01Z        ENDSR
     C********************************************************************
     C* NOP01  - NEW OPTION ON SCREEN                                    *
     C********************************************************************
     C     NOP01         BEGSR
     C*
     C                   Z-ADD     0             TOHAND
     C                   Z-ADD     0             TONORD
     C                   Z-ADD     0             TOTSLS
     C                   Z-ADD     0             TOTREC
     C                   Z-ADD     0             TOTIO
     C                   Z-ADD     0             TOTADF
     C                   Z-ADD     0             TOTWHS
     C                   Z-ADD     0             TOTDIV
     C*
     C                   Z-ADD     0             TOH
     C                   Z-ADD     0             TORD
     C                   Z-ADD     0             TSLS
     C                   Z-ADD     0             TREC
     C                   Z-ADD     0             TTFR
     C                   Z-ADD     0             TADJ
     C                   Z-ADD     0             ASLTOT            9 0
     C*
     C                   Z-ADD     0             COH
     C                   Z-ADD     0             CORD
     C                   Z-ADD     0             CSLS
     C                   Z-ADD     0             CREC
     C                   Z-ADD     0             CTFR
     C                   Z-ADD     0             CADJ
     C*
     C*   FIND THE UPPER LIMITS OF SEARCH                              *
     C*   SET TO FIRST RECORD.
     C*
     C                   Z-ADD     0             RRN               5 0
     C                   Z-ADD     0             SAVRRN            5 0
     C                   Z-ADD     0             SCRRN             5 0
     C                   Z-SUB     1             SVREG
     C                   Z-SUB     1             SVDST
     C                   Z-SUB     1             SVADI
     C                   Z-SUB     1             SVING
     C                   SETON                                        50
VW01 C                   EVAL      EJSCLR = *IN50
     C                   WRITE     SCRN1
VW01 C                   EVAL      EJSCLR = '0'
     C                   SETOFF                                       505155
     C                   SETOFF                                       66
     C*
     C*  CLEAR WORKING FIELDS
     C*
     C                   MOVE      *BLANKS       RDSDSC
     C                   MOVE      '0'           *IN23
     C*
     C*  MOVE PARAMETER IF RETRIEVED FROM SEARCH
     C*
     C*
     C*  BY REGION
     C*
     C     INRDS         IFEQ      'R'
     C                   MOVE      SRC(1)        CMD04
     C                   MOVE      SRC(1)        GRPNAM
     C                   MOVE      *BLANKS       REGNAM
     C                   Z-ADD     INSTR         INRDS3
     C     KEYAST        CHAIN     TBLREG                             99
     C                   MOVEL     REGNAM        RDSDSC
     C                   MOVE      RDS(1)        OUTRDS
     C     KEYAST        SETLL     TBLSTRL1
     C                   ELSE
     C*
     C*  BY DISTRICT
     C*
     C     INRDS         IFEQ      'D'
     C                   MOVE      SRC(2)        CMD04
     C                   MOVE      SRC(2)        GRPNAM
     C                   MOVE      *BLANKS       DSTNAM
     C                   Z-ADD     INSTR         INRDS3
     C     KEYAST        CHAIN     TBLDST                             99
     C                   MOVEL     DSTNAM        RDSDSC
     C                   MOVE      RDS(2)        OUTRDS
     C     KEYAST        SETLL     TBLSTRL2
     C                   ELSE
     C*
     C*  BY STORE
     C*
     C     INRDS         IFEQ      'S'
     C                   MOVE      '1'           *IN23
     C                   MOVE      SRC(3)        CMD04
     C                   MOVE      SRC(3)        GRPNAM
     C                   MOVE      *BLANKS       STRNAM
     C     KEYLOC        CHAIN     TBLSTR                             99
     C                   MOVEL     STRNAM        RDSDSC
     C                   MOVE      RDS(3)        OUTRDS
     C     KEYLOC        SETLL     TBLSTR
     C                   ELSE
     C*
     C*  BY STORE GROUP
     C*
     C     INRDS         IFEQ      'G'
     C                   MOVE      SRC(4)        CMD04
     C                   MOVE      SRC(4)        GRPNAM
     C                   MOVE      *BLANKS       ADINAM
     C                   Z-ADD     INSTR         INRDS3
     C     KEYAST        CHAIN     TBLADI                             99
     C                   MOVEL     ADINAM        RDSDSC
     C                   MOVE      RDS(4)        OUTRDS
     C     KEYAST        SETLL     INVADI
     C                   ELSE
     C*
     C*  BY INVENTORY GROUP
     C*
     C     INRDS         IFEQ      'I'
     C                   MOVE      SRC(5)        CMD04
     C                   MOVE      SRC(5)        GRPNAM
     C                   MOVE      *BLANKS       INGNAM
     C                   Z-ADD     INSTR         INRDS3
     C     KEYAST        CHAIN     TBLIGH                             99
     C                   MOVEL     INGNAM        RDSDSC
     C                   MOVE      RDS(5)        OUTRDS
     C     KEYAST        SETLL     TBLIGD
     C*
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C* IF THE ERROR INDICATOR IS ON, DON'T LOAD THE SUBFILE
     C*
     C     *IN20         IFEQ      '0'
     C                   EXSR      LSF01
     C                   EXSR      CFILL
     C                   END
     C*
     C                   ENDSR
     C* ------------------------------------------------------------------
     C* EDT01    EDIT DISPLAY 1 INPUT
     C* ------------------------------------------------------------------
     C     EDT01         BEGSR
     C*
     C                   MOVE      'DSP01 '      STEP
     C*
     C     EDT01Z        ENDSR
     C* ------------------------------------------------------------------
     C* EDT02 - EDIT THE SELECT TYPE CODE BEFORE RE-DISPLAY OF SUBFILE
     C* ------------------------------------------------------------------
     C*
     C     EDT02         BEGSR
     C*
     C                   MOVE      'DSP01 '      STEP
     C*
     C  N41              MOVE      TTL(1)        TITLE
     C   41              MOVE      TTL(2)        TITLE
     C*
     C                   MOVE      *BLANKS       IDESCR
     C     INUMBR        CHAIN     INVMST                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(1)        ERROR
     C                   SETON                                        022021
     C                   GOTO      EDT02Z
     C                   END
     C*
     C     INRDS         IFNE      'R'
     C     INRDS         ANDNE     'D'
     C     INRDS         ANDNE     'S'
     C     INRDS         ANDNE     'G'
     C     INRDS         ANDNE     'I'
     C                   MOVE      MSG(2)        ERROR
     C                   SETON                                        022022
     C                   GOTO      EDT02Z
     C                   END
     C*
     C* EDIT THE STORE TYPE VALUES
     C*
     C     INTYPE        IFNE      'A'
     C     INTYPE        ANDNE     'S'
     C     INTYPE        ANDNE     'O'
     C     INTYPE        ANDNE     'W'
     C     INTYPE        ANDNE     'I'
     C                   SETON                                        022025
     C                   MOVE      MSG(3)        ERROR
     C                   GOTO      EDT02Z
     C                   END
     C*
     C*
     C*  CHECK REGION
     C     INRDS         IFEQ      'R'
     C                   MOVE      SRC(1)        CMD04
     C                   MOVE      SRC(1)        GRPNAM
     C                   MOVE      *BLANKS       REGNAM
     C                   Z-ADD     INSTR         INRDS3
     C*
     C     INRDS3        IFEQ      0
     C                   MOVE      MSG(4)        ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C*
     C     KEYAST        CHAIN     TBLREG                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(5)        ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C*
     C                   MOVEL     REGNAM        RDSDSC
     C                   MOVE      RDS(1)        OUTRDS
     C*
     C                   ELSE
     C*
     C*  CHECK DISTRICT
     C*
     C     INRDS         IFEQ      'D'
     C                   MOVE      SRC(2)        CMD04
     C                   MOVE      SRC(2)        GRPNAM
     C                   MOVE      *BLANKS       DSTNAM
     C                   Z-ADD     INSTR         INRDS3
     C*
     C     INRDS3        IFEQ      0
     C                   MOVE      MSG(6)        ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C*
     C     KEYAST        CHAIN     TBLDST                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(7)        ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     c*
     C                   MOVEL     DSTNAM        RDSDSC
     C                   MOVE      RDS(2)        OUTRDS
     C                   ELSE
     C*
     C*  CHECK STORE
     C*
     C     INRDS         IFEQ      'S'
     C                   MOVE      '1'           *IN23
     C                   MOVE      SRC(3)        CMD04
     C                   MOVE      SRC(3)        GRPNAM
     C                   MOVE      *BLANKS       STRNAM
     C*
     C     INSTR         IFNE      0
     C     KEYLOC        CHAIN     TBLSTR                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(8)        ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C                   END
     C*
     C                   MOVEL     STRNAM        RDSDSC
     C                   MOVE      RDS(3)        OUTRDS
     C                   ELSE
     C*
     C*  CHECK STORE GROUP
     C*
     C     INRDS         IFEQ      'G'
     C                   MOVE      SRC(4)        CMD04
     C                   MOVE      SRC(4)        GRPNAM
     C                   MOVE      *BLANKS       ADINAM
     C                   Z-ADD     INSTR         INRDS3
     C*
     C     INRDS3        IFEQ      0
     C                   MOVE      MSG(9)        ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C*
     C     KEYAST        CHAIN     TBLADI                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(10)       ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C*
     C                   MOVEL     ADINAM        RDSDSC
     C                   MOVE      RDS(4)        OUTRDS
     C                   ELSE
     C*
     C*  CHECK INVENTORY GROUP
     C*
     C     INRDS         IFEQ      'I'
     C                   MOVE      SRC(5)        CMD04
     C                   MOVE      SRC(5)        GRPNAM
     C                   MOVE      *BLANKS       INGNAM
     C                   Z-ADD     INSTR         INRDS3
     C*
     C     INRDS3        IFEQ      0
     C                   MOVE      MSG(11)       ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C*
     C     KEYAST        CHAIN     TBLIGH                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(12)       ERROR
     C                   SETON                                        022026
     C                   GOTO      EDT02Z
     C                   END
     C*
     C                   MOVEL     INGNAM        RDSDSC
     C                   MOVE      RDS(5)        OUTRDS
     C*
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVE      'INZ01 '      STEP
     C*
     C     EDT02Z        ENDSR
     C********************************************************************
     C* LSF01  - LOAD ONE PAGE OF SUBFILE.                               *
     C********************************************************************
     C     LSF01         BEGSR
     C                   SETOFF                                       66
     C*
     C                   Z-ADD     SAVRRN        RRN
     C                   Z-ADD     0             SCRRN
     C*
     C* LOAD BY REGION
     C*
     C     INRDS         IFEQ      'R'
     C     READR         TAG
     C     INRDS3        READE     TBLSTRL1                               55
     C*
     C     *IN55         DOWEQ     '0'
     C*
     C     INTYPE        IFNE      'A'
     C     INTYPE        ANDNE     'I'
     C     STRHDO        IFNE      INTYPE
     C                   GOTO      NXTR
     C                   END
     C                   END
     C*
     C     INTYPE        IFEQ      'I'
     C     STRHDO        IFNE      'S'
     C     STRHDO        ANDNE     'W'
     C                   GOTO      NXTR
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN51
     C*
     C                   MOVEL     STSHRT        XXSHRT
     C                   EXSR      GETPOS
     C                   Z-ADD     0             ASL
     C                   Z-ADD     0             TOTASL
     C                   Z-ADD     0             AVGSLS
     C                   EXSR      FILL
     C                   ADD       1             RRN
     C                   ADD       1             SCRRN
     C                   WRITE     SCRN2
     C     NXTR          TAG
     C     INRDS3        READE     TBLSTRL1                               55
     C                   END
     C                   END
     C*
     C* LOAD BY DISTRICT
     C*
     C     INRDS         IFEQ      'D'
     C     READD         TAG
     C     INRDS3        READE     TBLSTRL2                               55
     C*
     C     *IN55         DOWEQ     '0'
     C*
     C     INTYPE        IFNE      'A'
     C     INTYPE        ANDNE     'I'
     C     STRHDO        IFNE      INTYPE
     C                   GOTO      NXTD
     C                   END
     C                   END
     C*
     C     INTYPE        IFEQ      'I'
     C     STRHDO        IFNE      'S'
     C     STRHDO        ANDNE     'W'
     C                   GOTO      NXTD
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN51
     C*
     C                   MOVEL     STSHRT        XXSHRT
     C                   EXSR      GETPOS
     C                   Z-ADD     0             ASL
     C                   Z-ADD     0             TOTASL
     C                   Z-ADD     0             AVGSLS
     C                   EXSR      FILL
     C                   ADD       1             RRN
     C                   ADD       1             SCRRN
     C*
     C                   WRITE     SCRN2
     C     NXTD          TAG
     C     INRDS3        READE     TBLSTRL2                               55
     C                   END
     C                   END
     C*
     C* LOAD BY STORE
     C*
     C     INRDS         IFEQ      'S'
     C     READS         TAG
     C                   READ      TBLSTR                                 55
     C*
     C     *IN55         DOWEQ     '0'
     C*
     C                   MOVE      '1'           *IN51
     C*
     C     INTYPE        IFNE      'A'
     C     INTYPE        ANDNE     'I'
     C     STRHDO        IFNE      INTYPE
     C                   GOTO      NXTS
     C                   END
     C                   END
     C*
     C     INTYPE        IFEQ      'I'
     C     STRHDO        IFNE      'S'
     C     STRHDO        ANDNE     'W'
     C                   GOTO      NXTS
     C                   END
     C                   END
     C*
     C                   MOVEL     STSHRT        XXSHRT
     C                   EXSR      GETPOS
     C                   Z-ADD     0             ASL
     C                   Z-ADD     0             TOTASL
     C                   Z-ADD     0             AVGSLS
     C                   EXSR      FILL
     C                   ADD       1             RRN
     C                   ADD       1             SCRRN
     C                   WRITE     SCRN2
     C     NXTS          TAG
     C                   READ      TBLSTR                                 55
     C*
     C                   END
     C                   END
     C*
     C* LOAD BY STORE GROUP
     C*
     C     INRDS         IFEQ      'G'
     C     INRDS3        READE     INVADI                                 55
     C     *IN55         DOWEQ     '0'
     C*
     C     INTYPE        IFNE      'A'
     C     INTYPE        ANDNE     'I'
     C     STRHDO        IFNE      INTYPE
     C                   GOTO      NXTG
     C                   END
     C                   END
     C*
     C     INTYPE        IFEQ      'I'
     C     STRHDO        IFNE      'S'
     C     STRHDO        ANDNE     'W'
     C                   GOTO      NXTG
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN51
     C*
     C     STRNUM        CHAIN     TBLSTR                             99
     C                   MOVEL     STSHRT        XXSHRT
     C                   EXSR      GETPOS
     C                   Z-ADD     0             ASL
     C                   Z-ADD     0             TOTASL
     C                   Z-ADD     0             AVGSLS
     C                   EXSR      FILL
     C                   ADD       1             RRN
     C                   ADD       1             SCRRN
     C*
     C                   WRITE     SCRN2
     C     NXTG          TAG
     C     INRDS3        READE     INVADI                                 55
     C                   END
     C                   END
     C*
     C* LOAD BY INV GROUP
     C*
     C     INRDS         IFEQ      'I'
     C     INRDS3        READE     TBLIGD                                 55
     C     *IN55         DOWEQ     '0'
     C*
     C     INTYPE        IFNE      'A'
     C     INTYPE        ANDNE     'I'
     C     STRHDO        IFNE      INTYPE
     C                   GOTO      NXTI
     C                   END
     C                   END
     C*
     C     INTYPE        IFEQ      'I'
     C     STRHDO        IFNE      'S'
     C     STRHDO        ANDNE     'W'
     C                   GOTO      NXTI
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN51
     C*
     C     STRNUM        CHAIN     TBLSTR                             99
     C                   MOVEL     STSHRT        XXSHRT
     C                   EXSR      GETPOS
     C                   Z-ADD     0             ASL
     C                   Z-ADD     0             TOTASL
     C                   Z-ADD     0             AVGSLS
     C                   EXSR      FILL
     C                   ADD       1             RRN
     C                   ADD       1             SCRRN
     C*
     C                   WRITE     SCRN2
     C     NXTI          TAG
     C     INRDS3        READE     TBLIGD                                 55
     C                   END
     C                   END
     C*
     C     *IN51         IFEQ      '1'
     C     ASLTOT        DIV(H)    8             TAVGS
     C*
     C                   Z-ADD     0             WRK80             8 0
     C                   Z-ADD     0             TSUP
     C     TOH           ADD       TORD          WRK80
     C     TAVGS         IFGT      0
     C     WRK80         DIV(H)    TAVGS         TSUP
     C                   END
     C                   END
     C*
     C* DO NOT DISPLAY SUBFILE IF EMPTY
     C*
     C     RRN           IFEQ      0
     C                   MOVE      '0'           *IN51
     C                   END
     C*
     C*    ALLOW DISPLAY OF SUBFILE IF AT LEAST 1 RECORD READ.
     C*
     C   55              SETON                                        66
     C   65              Z-ADD     1             SFSTRT
     C  N65              Z-ADD     RRN           SFSTRT
     C                   Z-ADD     RRN           SAVRRN            5 0
     C                   ENDSR
     C*****************************************************************
     C     CALC          BEGSR
     C*
     C                   MOVE      INUMBR        XNUMBR
     C                   MOVE      STRNUM        XSTORE
     C*
     C*prf                 CALL 'INV042'
     C*prf                 PARM           XNUMBR
     C*prf                 PARM           XSTORE
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C     GETPOS        BEGSR
     C*
     C                   MOVE      *BLANK        POSFLG
     C     STRNUM        CHAIN     TBL800                             47
     C     *IN47         IFEQ      '0'
     C                   MOVE      POSYN         POSFLG
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C     FILL          BEGSR
     C*
     C                   Z-ADD     0             IBHAND
     C                   Z-ADD     0             IBPOOQ
     C                   Z-ADD     0             IBTOOQ
     C                   Z-ADD     0             IBRSUP
     C                   Z-ADD     0             IBASUP
     C                   Z-ADD     0             IBRCUP
     C                   Z-ADD     0             IBTIUP
     C                   Z-ADD     0             IBTOUP
     C                   Z-ADD     0             IBRVUP
     C                   Z-ADD     0             IBAJUP
     C                   Z-ADD     0             IBPHUP
     C                   Z-ADD     0             IBRSUY
     C                   Z-ADD     0             IBASUY
     C                   Z-ADD     0             IBRCUY
     C                   Z-ADD     0             IBTIUY
     C                   Z-ADD     0             IBTOUY
     C                   Z-ADD     0             IBRVUY
     C                   Z-ADD     0             IBAJUY
     C                   Z-ADD     0             IBPHUY
     C*
     C     STRNUM        IFEQ      *ZEROS
     C                   Z-ADD     TOHAND        QTYOH
     C                   Z-ADD     TONORD        QTYORD
     C                   Z-ADD     TOTSLS        QTYSLS
     C                   Z-ADD     TOTREC        QTYREC
     C                   Z-ADD     TOTIO         TFRDIF
     C                   Z-ADD     TOTADF        ADJDIF
     C                   Z-ADD     0             SUPWKS
     C     TOTDIV        IFGT      0
     C     TOTWHS        DIV(H)    TOTDIV        AVGSLS
     C                   END
     C                   ELSE
     C     KEYBAL        CHAIN     INVBAL                             99
     C                   Z-ADD     IBHAND        QTYOH
     C     IBPOOQ        ADD       IBTOOQ        QTYORD
     C                   ADD       IBHAND        TOHAND
     C                   ADD       QTYORD        TONORD
     C                   XFOOT     ASL           TOTASL            6 0
     C                   ADD       TOTASL        TOTWHS            7 0
     C                   ADD       1             TOTDIV            4 0
     C     TOTASL        DIV(H)    8             AVGSLS
     C  N41IBRSUP        ADD       IBASUP        QTYSLS
     C  N41              Z-ADD     IBRCUP        QTYREC
     C  N41IBTIUP        SUB       IBTOUP        TFRDIF
     C  N41IBAJUP        ADD       IBPHUP        ADJDIF
     C   41IBRSUY        ADD       IBASUY        QTYSLS
     C   41              Z-ADD     IBRCUY        QTYREC
     C   41IBTIUY        SUB       IBTOUY        TFRDIF
     C   41IBAJUY        ADD       IBPHUY        ADJDIF
     C                   ADD       QTYSLS        TOTSLS
     C                   ADD       QTYREC        TOTREC
     C                   ADD       TFRDIF        TOTIO
     C                   ADD       ADJDIF        TOTADF
     C*
     C                   Z-ADD     0             WRK80             8 0
     C                   Z-ADD     0             SUPWKS
     C     QTYOH         ADD       QTYORD        WRK80
     C     AVGSLS        IFGT      0
     C     WRK80         DIV(H)    AVGSLS        SUPWKS
     C                   END
     C*
     C                   ADD       QTYOH         TOH
     C                   ADD       QTYORD        TORD
     C                   ADD       QTYSLS        TSLS
     C                   ADD       QTYREC        TREC
     C                   ADD       TFRDIF        TTFR
     C                   ADD       ADJDIF        TADJ
     C                   ADD       TOTASL        ASLTOT
     C                   END
     C                   ENDSR
     C*****************************************************************
     C     CFILL         BEGSR
     C*
     C                   CLEAR                   RINVCBL
     C                   Z-ADD     *ZERO         CASL
     C*
     C     KEYMST        CHAIN     INVCBL                             99
     C     *IN99         IFEQ      '1'
     C                   MOVE      MSG(13)       ERROR
     C                   SETON                                        0220
     C                   Z-ADD     0             COH
     C                   Z-ADD     0             CORD
     C                   Z-ADD     0             CSLS
     C                   Z-ADD     0             CREC
     C                   Z-ADD     0             CTFR
     C                   Z-ADD     0             CADJ
     C                   Z-ADD     0             CAVGS
     C                   Z-ADD     0             CSUP
     C                   ELSE
     C*
     C                   Z-ADD     CBHAND        COH
     C     CBPOOQ        ADD       CBTOOQ        CORD
     C                   XFOOT     CASL          COTASL            6 0
     C                   ADD       COTASL        COTWHS            7 0
     C                   ADD       1             COTDIV            4 0
     C     COTASL        DIV(H)    8             CAVGS
     C  N41CBRSUP        ADD       CBASUP        CSLS
     C  N41              Z-ADD     CBRCUP        CREC
     C  N41CBTIUP        SUB       CBTOUP        CTFR
     C  N41CBAJUP        ADD       CBPHUP        CADJ
     C   41CBRSUY        ADD       CBASUY        CSLS
     C   41              Z-ADD     CBRCUY        CREC
     C   41CBTIUY        SUB       CBTOUY        CTFR
     C   41CBAJUY        ADD       CBPHUY        CADJ
     C*
     C                   Z-ADD     0             WRK80             8 0
     C                   Z-ADD     0             CSUP
     C     COH           ADD       CORD          WRK80
     C     CAVGS         IFGT      0
     C     WRK80         DIV(H)    CAVGS         CSUP
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C* ------------------------------------------------------------------
     C* ENDJOB   END THE JOB ROUTINE
     C* ------------------------------------------------------------------
     C     ENDJOB        BEGSR
     C*
     C     FLG417        IFEQ      *ON
     C                   MOVE      *ALL'LR'      CANCOD
     C*prf                 CALL 'INV417'  INV417
     C                   END
     C*
     C                   SETON                                        LR
     C                   EXSR      EXIT
     C     ENDJOZ        ENDSR
     C* ------------------------------------------------------------------
     C* EXIT     END PROGRAM ROUTINE
     C* ------------------------------------------------------------------
     C     EXIT          BEGSR
     C*
     C* TEST FOR PROGRAM PARAMETER INPUT/OUTPUT
     C*
     C***        W1SEL     IFEQ '1'
     C***        QPARMS    ANDEQ1
     C***                  MOVE S1STR     INSTR
     C***                  END
     C*
     C***        QPARMS    IFGT 1
     C***        W1SEL     ANDEQ'1'
     C***                  MOVE S1STR     INSTR
     C***                  END
     C*
     C                   MOVE      '0'           W1SEL
     C*
     C                   RETURN
     C*
     C     EXITZ         ENDSR
     C* ------------------------------------------------------------------
     C* HSEKPG   FIRST CYCLE SUBROUTINE
     C* ------------------------------------------------------------------
     C     HSEKPG        BEGSR
     C*
     C*  RETRIEVE SYSTEM DATA STRUCTURE DATA IF NOT PASSED IN
     C*
     C                   CALL      'SYSGET'
     C                   PARM                    PARM1
     C*
     C                   Z-ADD     DATPER        PERIOD            2 0
     C*
     C                   Z-ADD     09            SCRNSZ            3 0
     C*
     C     *LOVAL        SETLL     TBLSTR
     C**
     C                   OPEN      OPTIONS
     C     1632          CHAIN     OPTIONS                            99
     C     CTLQST        IFEQ      'Y'
     C                   SETON                                        65
     C                   END
     C                   CLOSE     OPTIONS
     C*
     C                   Z-ADD     0             TOG               1 0          DEF - PTD
     C                   MOVE      TOG           *IN(41)
     C                   MOVE      TTL(1)        TITLE
     C*
     C*  FIRST PASS PROCESSING DURING "INZ01" ROUTINE TO
     C*  LOAD STORE FROM PARMS PASSED
     C*
     C                   MOVE      0             INSTR
     C                   MOVE      'S'           INRDS
     C                   MOVE      'S'           W1NRDS
     C                   MOVE      'A'           WTYPE
     C                   MOVE      'A'           INTYPE
     C                   MOVE      SRC(3)        CMD04
     C                   MOVE      SRC(3)        GRPNAM
     C                   MOVE      RDS(3)        OUTRDS
     C                   Z-ADD     INSKU         INUMBR
     C*
     C                   ENDSR
     C* ------------------------------------------------------------------
     C* DEFINE   DEFINE FILE KEY AND SYNONYMS
     C* ------------------------------------------------------------------
     C     DEFINE        BEGSR
     C*
     C     *LIKE         DEFINE    *INLR         W1SEL
     C     *LIKE         DEFINE    INRDS         W1NRDS
     C     *LIKE         DEFINE    INTYPE        WTYPE
     C     *LIKE         DEFINE    REGNUM        INRDS3
     C     *LIKE         DEFINE    REGNUM        SVREG
     C     *LIKE         DEFINE    STSDAT        S1SDAT
     C     *LIKE         DEFINE    STRDST        SVDST
     C     *LIKE         DEFINE    ADINUM        SVADI
     C     *LIKE         DEFINE    INGNUM        SVING
     C     *LIKE         DEFINE    STRNUM        INSTR#
     C     *LIKE         DEFINE    RRN           MYRRN
     C     *LIKE         DEFINE    RRN           X
     C     *LIKE         DEFINE    IBHAND        TOHAND
     C     *LIKE         DEFINE    QTYORD        TONORD
     C     *LIKE         DEFINE    QTYSLS        TOTSLS
     C     *LIKE         DEFINE    QTYREC        TOTREC
     C     *LIKE         DEFINE    TFRDIF        TOTIO
     C     *LIKE         DEFINE    ADJDIF        TOTADF
     C     *LIKE         DEFINE    INUMBR        XNUMBR
     C     *LIKE         DEFINE    INUMBR        INSKU
     C     *LIKE         DEFINE    STRNUM        XSTORE
     C     *LIKE         DEFINE    INUMBR        XXSKU
     C     *LIKE         DEFINE    INTYPE        XXTYPE
     C     *LIKE         DEFINE    INRDS         XXRDS
     C     *LIKE         DEFINE    INSTR         XXSTR
     C     *LIKE         DEFINE    *INLR         FLG417
     C*
     C     KEYAST        KLIST
     C                   KFLD                    INRDS3
     C*
     C     KEYLOC        KLIST
     C                   KFLD                    INSTR
     C*
     C     KEYBAL        KLIST
     C                   KFLD                    INUMBR
     C                   KFLD                    STRNUM
     C*
     C     KEYMST        KLIST
     C                   KFLD                    INUMBR
     C*
     C     INV417        PLIST
     C                   PARM                    PARM1
     C                   PARM      INUMBR        XXSKU
     C                   PARM      INTYPE        XXTYPE
     C                   PARM      INRDS         XXRDS
     C                   PARM      INSTR         XXSTR
     C*
     C     INV860        PLIST
     C                   PARM      INUMBR        SKUNBR            9 0
     C                   PARM      INTYPE        STRTYP            1
     C                   PARM      INRDS         SRCHBY            1
     C                   PARM      INSTR         STRGRP            5 0
     C*
     C     INV861        PLIST
     C                   PARM      INUMBR        SKUNBR            9 0
     C                   PARM      INSTR         STRGRP            5 0
     C*
     C     CMDEXC        PLIST
     C*kra                 PARM           SBM    64
     C                   PARM                    SBM              87
     C*kra                 PARM 64        LEN    155
     C                   PARM      87            LEN              15 5
     C*
     C                   ENDSR
     C*
      ********************************************************************
      * T A B L E S
      * ------------------------------------------------------------------
VW01 C*****************************************************************
VW01 C* EJS (web) display support - added for INV046W
VW01 C*****************************************************************
VW01 C* EJSOUT - before EXFMT: indicators to the template, remember the
VW01 C*          header input so EJSIN can emulate CHANGE(02)
VW01 C     EJSOUT        BEGSR
VW01 C                   EVAL      INDS = EJSINA
VW01 C                   EVAL      EJSCLR = '0'
VW01 C                   EVAL      ACTION = *BLANKS
VW01 C                   EVAL      EJSNUM = INUMBR
VW01 C                   EVAL      EJSTYP = INTYPE
VW01 C                   EVAL      EJSRDS = INRDS
VW01 C                   EVAL      EJSSTR = INSTR
VW01 C                   ENDSR
VW01 C* EJSIN  - after EXFMT: a browser button posts ACTION (F01..F24,
VW01 C*          PAGEDOWN); a real key leaves it blank and the handler
VW01 C*          puts its AID in the INFDS. Set what the DDS keywords set
VW01 C*          on a 5250: CA/CF keys -> KA..KH, CHANGE(02), and
VW01 C*          ROLLUP(03) when the subfile can load another page.
VW01 C     EJSIN         BEGSR
VW01 C                   EVAL      EJSAID = EJSKEY
VW01 C                   IF        %SUBST(ACTION:1:1) = 'F'
VW01 C                             AND %LEN(%TRIMR(ACTION)) > 1
VW01 C                             AND %CHECK('0123456789':
VW01 C                                 %TRIMR(%SUBST(ACTION:2))) = 0
VW01 C                   EVAL      EJSN = %INT(%TRIMR(%SUBST(ACTION:2)))
VW01 C                   IF        EJSN >= 1 AND EJSN <= 24
VW01 C                   EVAL      EJSAID = %SUBST(EJSAIDS:EJSN:1)
VW01 C                   ENDIF
VW01 C                   ELSE
VW01 C                   IF        ACTION = 'PAGEDOWN'
VW01 C                   EVAL      EJSAID = X'F5'
VW01 C                   ENDIF
VW01 C                   IF        ACTION = 'ENTER'
VW01 C                   EVAL      EJSAID = X'F1'
VW01 C                   ENDIF
VW01 C                   ENDIF
VW01 C                   EVAL      *INKA = EJSAID = X'31'
VW01 C                   EVAL      *INKC = EJSAID = X'33'
VW01 C                   EVAL      *INKD = EJSAID = X'34'
VW01 C                   EVAL      *INKE = EJSAID = X'35'
VW01 C                   EVAL      *INKF = EJSAID = X'36'
VW01 C                   EVAL      *INKG = EJSAID = X'37'
VW01 C                   EVAL      *INKH = EJSAID = X'38'
VW01 C                   EVAL      *IN02 = INUMBR <> EJSNUM OR
VW01 C                                     INTYPE <> EJSTYP OR
VW01 C                                     INRDS <> EJSRDS OR
VW01 C                                     INSTR <> EJSSTR
VW01 C                   EVAL      *IN03 = EJSAID = X'F5' AND
VW01 C                                     NOT *IN55 AND NOT *IN20
VW01 C                   ENDSR
**   ERROR MESSAGES TABLE........60 LENGTH..................
                The SKU Entered is Invalid                  01
The "Search For" type code entry must be R, D, S, G or I    02
   The "Store Type" code entry must be A, S, O, W or I      03
                   Region must be entered                   04
                    Region is not valid                     05
                 District must be entered                   06
                   District is not valid                    07
                    Store is not valid                      08
               Store Group must be entered                  09
                 Store Group is not valid                   10
                 Inv Group must be entered                  11
                   Inv Group is not valid                   12
    Chain level inventory balance for SKU does not exist    13
** TTL
   Current Period Inventory History
    Year to Date Inventory History
** RDS
Region Number.: 01
Dist Number...: 02
Store Number..: 03
Store Group...: 04
Inv Group.....: 05
** SRC
Region      01
District    02
Store/Whs   03
Store Group 04
Inv Group   05
** CMD
CLRPFM FILE(QTEMP/WRKSGB)
CRTPF FILE(QTEMP/WRKSGB) SRCFILE(CUSTOMR/QDDSSRC) SRCMBR(WRKSGB)
