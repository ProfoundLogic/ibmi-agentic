     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXTASB - FIX PROGRAM FOR BACK TO BASE CONVERSIONS
     H*****************************************************************
     H*
     H*  THIS PROGRAM PERFORMS VARIOUS CONVERSIONS FOR TASB BACK TO BASE.
     H*
     H*  MODE '1':  COPY AGGREGATE AMOUNT TO WMLDP
     H*  MODE '2':  POPULATE POLICY TYPE CODE TO WDELP
     H*  MODE '3':  CREATE BACKUP DATA IN WDDAP FOR DEDUC POLICIES
     H*  MODE '4':  CREATE DEDUCTIBLE DETAIL FOR ASO GROUPS (REQUIRES DATE PARM)
     H*  MODE '5':  CREATE A/R RECORDS FOR DEDUCTIBLE DETAIL IN ASO GROUPS
     H*  MODE '6':  CREATE SUBMISSION HISTORY RECORDS FOR ALL POLICIES.
     H*
     H*****************************************************************
     H* Revisions:
     H*
     H* 04/28/10  C:33  Project: 25WRB
     H*                 Deductible records are no longer needed as we will
     H*                  be using the deductible process to create the
     H*                  history.  This eliminates FIX3, FIX4 & FIX5.
     H*
     H* 04/23/10  C:65  Project: 25WQK  Priss #38
     H*                -ADD MODE '6' TO CREATE SUBMISSION HISTORY RECORDS.
     H*
     H* 04/16/10  C:33  Project: 25WRB  Priss #25
     H*                -FIX1 should be run for 'Pool Groups' (F1CLMS <> 'Y')
     H*                -FIX1 should will have different defaults for
     H*                 'Pool Groups' vs. 'ASO Groups'
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDELL1    UF   E           K DISK
     FWMCML     IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FWDCBL1    IF   E           K DISK
     FWMLDL1    UF A E           K DISK
     FWDDDL1    IF A E           K DISK
     FWDDAP     O  A E             DISK
     FWMEAP     O  A E             DISK
     F*** BEGIN ADD ***  04/23/10  **************************************
     FWMAHL     IF   E           K DISK
     FWMSHL1    IF A E           K DISK
     F*** END ADD *****  04/23/10  **************************************

     D DC              S              2    DIM(10)
     D PD              S             11  2 DIM(10)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D DEDB            C                   CONST('DED BILLING CONVERS-
     D                                     ION TRANS')
     D DEDP            C                   CONST('DED PAYMENT CONVERS-
     D                                     ION TRANS')
     D***  Begin Add  ***  04/16/10  **************************************
     D DED             C                   CONST('DEDUCTIBLE')
     D ADED            C                   CONST('AGGREGATE DEDUCTIBLE')

     I***  End   Add  ***  04/16/10  **************************************

     C*
     C     FNKY          KLIST
     C                   KFLD                    F1CO#
     C                   KFLD                    F1FND
     C*
     C     ELKY          KLIST
     C                   KFLD                    F1CO#
     C                   KFLD                    F1FND
     C                   KFLD                    F1FYR
     C*
     C     LDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     PAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    CMCASE
     C*
     C     CDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELPFDT
     C*
     C     CDKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*** BEGIN ADD ***  04/23/10  **************************************
      *
     C     SHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
     C                   KFLD                    XXTYPE
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*** END ADD *****  04/23/10  **************************************
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQMODE            1
     C                   PARM                    QQUPD             1
     C                   PARM                    QQDATE            8
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXDT8             8 0
     C                   Z-ADD     0             XXDATE            7 0
     C     Q1PRMS        IFGT      2
     C                   MOVE      QQDATE        XXDT8
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C* COPY AGGREGATE AMOUNT TO WMLDP
     C*
     C     QQMODE        IFEQ      '1'
     C     QQUPD         ANDEQ     'Y'
     C                   EXSR      FIX1
     C                   ENDIF
     C*
     C* POPULATE POLICY TYPE CODE TO WDELP
     C*
     C     QQMODE        IFEQ      '2'
     C     QQUPD         ANDEQ     'Y'
     C                   EXSR      FIX2
     C                   ENDIF
     C*
     C* CREATE BACKUP DATA IN WDDAP FOR DEDUC POLICIES
     C*
     C     QQMODE        IFEQ      '3'
     C     QQUPD         ANDEQ     'Y'
     C**********           EXSR FIX3
     C                   ENDIF
     C*
     C* CREATE DEDUCTIBLE DETAIL FOR ASO GROUPS
     C*
     C     QQMODE        IFEQ      '4'
     C     QQUPD         ANDEQ     'Y'
     C     XXDATE        ANDNE     0
     C**********           EXSR FIX4
     C                   ENDIF
     C*
     C* CREATE A/R RECORDS FOR DEDUCTIBLE DETAIL IN ASO GROUPS
     C*
     C     QQMODE        IFEQ      '5'
     C     QQUPD         ANDEQ     'Y'
     C**********           EXSR FIX5
     C                   ENDIF
     C*** BEGIN ADD ***  04/23/10  **************************************
     C*
     C* CREATE SUBMISSION HISTORY RECORDS.
     C*
     C     QQMODE        IFEQ      '6'
     C     QQUPD         ANDEQ     'Y'
     C                   EXSR      FIX6
     C                   ENDIF
     C*** END ADD *****  04/23/10  **************************************
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*
     C* ASSIGN LARGE DEDUCTIBLE AGGREGATE AMOUNT INTO WMLDP FILE.
     C*
     C     FIX1          BEGSR
     C*
     C                   READ      PF101                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C***  Begin Del  ***  04/16/10  **************************************
     C********** F1CLMS    IFEQ 'Y'
     C***  End   Del  ***  04/16/10  **************************************
     C*
     C     ELKY          CHAIN(N)  PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELAMAG        IFNE      *ZERO
     C***  Begin Add  ***  04/16/10  **************************************
     C     F1CLMS        ANDNE     'Y'
     C     F1CLMS        OREQ      'Y'
     C*
     C                   CLEAR                   PLD01
     C***  End   Add  ***  04/16/10  **************************************
     C*
     C     LDKY          CHAIN     PLD01                              40
     C*
     C     *IN40         IFEQ      *ON
     C                   Z-ADD     ELCO#         LDCO#
     C                   Z-ADD     ELFND         LDFND
     C                   Z-ADD     ELFYR         LDFYR
     C                   Z-ADD     ELEMP#        LDEMP#
     C                   Z-ADD     ELDIV         LDDIV
     C                   Z-ADD     ELPFDT        LDEFD1
     C*
     C                   MOVEL     Q1USER        LDIUSR
     C                   MOVEL     Q1PGM         LDIPGM
     C                   Z-ADD     DSTIME        LDITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDIDT
     C                   ENDIF
     C*
     C***  Begin Chg  ***  04/16/10  **************************************
     C**********           Z-ADDELAMAG    LDAMG1
     C                   CLEAR                   LDPCT1
     C                   CLEAR                   LDPLN1
     C                   CLEAR                   LDTOT1
     C*
     C                   MOVEL(P)  'G'           LDTYP1
     C                   Z-ADD     9999          LDCLS1
     C                   MOVEL(P)  'M'           LDPLV1
     C                   MOVEL(P)  'CO'          LDR101
     C                   MOVEL(P)  'ME'          LDR102
     C                   MOVEL(P)  'AE'          LDR103
     C                   MOVEL(P)  'Y'           LDALA1
     C                   MOVEL(P)  'Y'           LDRNW1
     C*
     C                   SELECT
     C     F1CLMS        WHENNE    'Y'
     C                   MOVEL(P)  ADED          LDDES1
     C                   Z-ADD     *ZEROS        LDAMP1
     C                   Z-ADD     ELAMAG        LDAMG1
     C                   MOVEL(P)  'O'           LDVRB1
     C                   MOVEL(P)  'O'           LDOCV1
     C                   OTHER
     C                   MOVEL(P)  DED           LDDES1
     C                   Z-ADD     *HIVAL        LDAMP1
     C                   Z-ADD     *ZEROS        LDAMG1
     C                   MOVEL(P)  'C'           LDVRB1
     C                   MOVEL(P)  'C'           LDOCV1
     C                   ENDSL
     C***  End   Chg  ***  04/16/10  **************************************
     C*
     C     ELFYR         IFGE      94
     C***  Begin Add  ***  04/16/10  **************************************
     C     F1CLMS        ANDNE     'Y'
     C***  End   Add  ***  04/16/10  **************************************
     C*
     C     CDKY          SETGT     PCB01
     C     CDKY1         READPE    PCB01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CBLIRT        LDLIRT
     C                   Z-ADD     CBCLAL        LDCLAL
     C                   ENDIF
     C                   ENDIF                                                  ELFYR IFGE 94
     C*
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSTIME        LDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C*
     C     *IN40         IFEQ      *ON
     C                   WRITE     PLD01
     C                   ELSE
     C                   UPDATE    PLD01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ELKY          READE(N)  PEL01                                  60
     C                   ENDDO
     C*
     C***  Begin Del  ***  04/16/10  **************************************
     C**********           ENDIF                           F1CLMS IFEQ
     C***  End   Del  ***  04/16/10  **************************************
     C*
     C                   READ      PF101                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* ASSIGN POLICY TYPE CODE BASED ON BILLING TYPE INTO WDELP FILE.
     C*
     C     FIX2          BEGSR
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     ELDEDP        IFNE      *BLANK
     C*
     C                   SELECT
     C     ELDEDP        WHENEQ    'F'
     C                   MOVE      'RP'          ELQTYP
     C                   MOVE      *BLANK        ELDEDP
     C     ELDEDP        WHENEQ    'T'
     C                   MOVE      'FL'          ELQTYP
     C                   MOVE      *BLANK        ELDEDP
     C     ELDEDP        WHENEQ    'M'
     C                   MOVE      'MC'          ELQTYP
     C                   MOVE      *BLANK        ELDEDP
     C     ELDEDP        WHENEQ    'A'
     C                   MOVE      'RD'          ELQTYP
     C                   MOVE      *BLANK        ELDEDP
     C                   ENDSL
     C*
     C     ELDEDP        IFEQ      *BLANK
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSTIME        ELTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELDEDP IFNE
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* CREATE BACKUP DATA IN WDDAP BASED ON WDDDP.
     C*
     C     FIX3          BEGSR
     C*
     C                   Z-ADD     0             XXAMTB           11 2
     C                   CLEAR                   PDA01
     C*
     C                   READ      PDD01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     DDCO#         IFNE      DACO#
     C     DDFND         ORNE      DAFND
     C     DDEMP#        ORNE      DAEMP#
     C     DDDIV         ORNE      DADIV
     C     DDFYR         ORNE      DAFYR
     C     DDBFYR        ORNE      DABFYR
     C     DDCASE        ORNE      DACASE
     C     DDPODT        ORNE      DAPODT
     C*
     C     XXAMTB        IFNE      *ZERO
     C                   Z-ADD     XXAMTB        DAAMTB
     C                   WRITE     PDA01
     C                   Z-ADD     0             XXAMTB
     C                   ENDIF
     C*
     C                   CLEAR                   PDA01
     C*
     C                   Z-ADD     DDCO#         DACO#
     C                   Z-ADD     DDFND         DAFND
     C                   Z-ADD     DDEMP#        DAEMP#
     C                   Z-ADD     DDDIV         DADIV
     C                   Z-ADD     DDFYR         DAFYR
     C                   Z-ADD     DDBFYR        DABFYR
     C                   Z-ADD     DDCASE        DACASE
     C                   Z-ADD     DDPODT        DAPODT
     C                   Z-ADD     DDAMTR        DAAMTR
     C                   MOVE      DDSTAT        DASTAT
     C                   MOVE      DDUSER        DAUSER
     C                   MOVE      DDPGM         DAPGM
     C                   Z-ADD     DDDT          DADT
     C                   Z-ADD     DDTIME        DATIME
     C                   MOVE      DDIUSR        DAIUSR
     C                   Z-ADD     DDIDT         DAIDT
     C*
     C                   Z-ADD     DDAMTB        XXAMTB
     C                   ELSE
     C                   ADD       DDAMTB        XXAMTB
     C                   ENDIF
     C*
     C                   READ      PDD01                                  60
     C                   ENDDO
     C*
     C     XXAMTB        IFNE      *ZERO
     C                   Z-ADD     XXAMTB        DAAMTB
     C                   WRITE     PDA01
     C                   Z-ADD     0             XXAMTB
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* CREATE DEDUCTIBLE DETAIL FOR ASO GROUPS.
     C*
     C     FIX4          BEGSR
     C*
     C                   READ      PF101                                  40
     C     *IN40         DOWEQ     '0'
     C*
     C     F1CLMS        IFEQ      'Y'
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     ELKY          CHAIN(N)  PEL01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     LDKY          CHAIN     PDD01                              60
     C     *IN60         IFEQ      *ON
     C*
     C     LDKY          CHAIN     PCM01                              70
     C     *IN70         DOWEQ     *OFF
     C*
     C     CMACDT        IFLE      XXDATE
     C*
     C                   CLEAR                   DC
     C                   CLEAR                   PD
     C                   Z-ADD     1             W                 3 0
     C*
     C     1             DO        10            X                 3 0
     C*
     C                   Z-ADD     1             Y                 3 0
     C     RT(X)         LOOKUP    DC(Y)                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   ADD       AR(X)         PD(Y)
     C                   ELSE
     C                   MOVE      RT(X)         DC(W)
     C                   ADD       AR(X)         PD(W)
     C                   ADD       1             W
     C                   ENDIF
     C*
     C                   ENDDO                                                  1 DO 10 X
     C*
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PATRDT        IFLE      XXDATE
     C     PABR2         ANDNE     *BLANKS
     C*
     C     PABR2         IFEQ      FNMEP1
     C     PABR2         OREQ      FNCOP1
     C     PABR2         OREQ      FNEXP1
     C     PABR2         OREQ      FNLEP1
     C*
     C                   Z-ADD     1             Y                 3 0
     C     PARECD        LOOKUP    DC(Y)                                  60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         PD(Y)
     C                   ELSE
     C                   SUB       PAAMT         PD(Y)
     C                   ENDIF                                                  PATYPE IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  PABR2 IFEQ
     C*
     C                   ENDIF                                                  PATRDT IFLE
     C*
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     1             DO        10            X
     C*
     C     DC(X)         IFNE      *BLANKS
     C     PD(X)         ANDNE     *ZERO
     C*
     C                   CLEAR                   PDD01
     C*
     C                   Z-ADD     CMCO#         DDCO#
     C                   Z-ADD     CMFND         DDFND
     C                   Z-ADD     CMEMP#        DDEMP#
     C                   Z-ADD     CMDIV         DDDIV
     C                   Z-ADD     CMFYR         DDFYR
     C                   Z-ADD     CMFYR         DDBFYR
     C                   Z-ADD     CMCASE        DDCASE
     C                   Z-ADD     XXDATE        DDPODT
     C                   Z-ADD     PD(X)         DDAMTB
     C                   MOVE      DC(X)         DDRECD
     C                   Z-ADD     X             DDSORT
     C*
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   MOVEL     Q1USER        DDIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDIDT
     C*
     C                   WRITE     PDD01
     C*
     C                   ENDIF                                                  DC,X IFNE
     C*
     C                   ENDDO                                                  1 DO 10 X
     C*
     C                   ENDIF                                                  CMACDT IFLE
     C*
     C     LDKY          READE     PCM01                                  70
     C                   ENDDO                                                  *IN70 DOWEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY          READE(N)  PEL01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C                   ENDIF                                                  F1CLMS IFEQ
     C*
     C                   READ      PF101                                  40
     C                   ENDDO                                                  *IN40 DOWEQ
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* CREATE A/R RECORDS FOR DEDUCTIBLES.
     C*
     C     FIX5          BEGSR
     C*
     C                   READ      PF101                                  40
     C     *IN40         DOWEQ     '0'
     C*
     C     F1CLMS        IFEQ      'Y'
     C*
     C     ELKY          CHAIN(N)  PEL01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   Z-ADD     0             XXAMTB
     C*
     C     LDKY          CHAIN     PDD01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     DDPGM         IFEQ      Q1PGM
     C                   ADD       DDAMTB        XXAMTB
     C                   ENDIF                                                  DDPGM IFEQ
     C*
     C     LDKY          READE     PDD01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXAMTB        IFNE      *ZERO
     C*
     C                   CLEAR                   PEA01
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     ELCO#         EACO#
     C                   Z-ADD     ELFND         EAFND
     C                   Z-ADD     ELEMP#        EAEMP#
     C                   Z-ADD     ELDIV         EADIV
     C                   Z-ADD     ELFYR         EAFYR
     C                   Z-ADD     XXAMTB        EAAMT
     C                   MOVE      'DF'          EAARCD
     C                   MOVE      'F'           EAARTY
     C                   MOVE      'R'           EACTYP
     C                   MOVEL     DEDB          EADESC
     C                   MOVE      'A'           EASTAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C*
     C                   WRITE     PEA01
     C*
     C                   MOVE      'DQ'          EAARCD
     C                   MOVE      'Q'           EAARTY
     C                   MOVE      'R'           EACTYP
     C                   MOVEL(P)  DEDP          EADESC
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   WRITE     PEA01
     C*
     C                   ENDIF                                                  XXAMTB IFNE
     C*
     C     ELKY          READE(N)  PEL01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
     C                   ENDIF                                                  F1CLMS IFEQ
     C*
     C                   READ      PF101                                  40
     C                   ENDDO                                                  *IN40 DOWEQ
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*** BEGIN ADD ***  04/23/10  **************************************
     C*
     C* CREATE SUBMISSION HISTORY RECORDS.
     C*
     C     FIX6          BEGSR
     C*
     C                   MOVE      'P'           XXTYPE            1
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     SHKY          CHAIN     PSH01                              70
     C*
     C     *IN70         IFEQ      *ON
     C*
     C                   CLEAR                   PSH01
     C*
     C                   MOVE      ELCO#         SHCO#
     C                   MOVE      ELFND         SHFND
     C                   MOVE      ELEMP#        SHEMP#
     C                   MOVE      ELDIV         SHDIV
     C                   MOVE      ELFYR         SHFYR
     C                   MOVE      ELPFDT        SHPFDT
     C                   MOVE      'P'           SHTYPE
     C                   MOVE      '4'           SHSTAT
     C     ELNWRW        IFEQ      'R'
     C                   MOVE      'R'           SHCODE
     C                   ELSE
     C                   MOVE      'O'           SHCODE
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         SHAGT#
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   ENDDO                                                  *IN60 == 0
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AHAGT#        SHAGT#
     C                   ENDIF                                                  *IN60 == 0
     C*
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   MOVEL     SRDTO         SHTRDT
     C*
     C                   Z-ADD     DSTIME        SHTIME
     C                   Z-ADD     SHDT          SHIDT
     C                   MOVEL     Q1USER        SHIUSR
     C*
     C                   WRITE     PSH01
     C                   FEOD      WMSHL1
     C*
     C                   ENDIF                                                  *IN70 IFEQ
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*** END ADD *****  04/23/10  **************************************
