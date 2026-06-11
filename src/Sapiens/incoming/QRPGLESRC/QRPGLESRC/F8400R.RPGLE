     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H*
     H*****************************************************************
     H*  F8400R - WRITE TO FAZ0P                     C:227 12/18/10   *
     H*                                              PROJ: 146MCV     *
     H*  CALLED FROM: F8400C                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/05/15 C:212 Support: 37612                                *
     H*                 Fix date conversion error                     *
     H*                                                               *
     H*  09/29/15 C:212 Project: MC400                                *
     H*                 ICD 10 changes for Procura                    *
     H*                                                               *
     H*  09/03/15 C:212 Project: MC400                                *
     H*                 ICD 10 changes for Procura                    *
     H*                                                               *
     H*  03/02/15 C:296 Project: 114MRD                               *
     H*                 Add logic to update FAEOP/EONSNT.             *
     H*                                                               *
     H*  09/17/13 C:079 Project: 114MID                               *
     H*                 Map NDC code to procedure code for Rx bills   *
     H*                                                               *
     H*  04/17/13 C:284 Project: 114MID                               *
     H*                 Add new fields to Z2DS                        *
     H*                                                               *
     H*  05/22/12 C:251 Project: 146MEZ                               *
     H*                 Add Claimant Last/First Name                  *
     H*                                                               *
     H*  09/26/11 C: 280 SUPPORT: MC146/78023                         *
     H*                                                               *
     H*  08/03/11 C:273 Project: 146MEK                               *
     H*                 Set outbound Rendering Provider data from     *
     H*                 FAEDL, FAEHL, FAEUL                           *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     F*  Physical file definition (for maintaining record)
     FFAEAL14   UF   E           K DISK
     F***** Beg Chg ***** 03/02/15 ******************************
     F****** FAEOL4    IF   E           K DISK
     FFAEOL4    UF A E           K DISK
     F***** End Chg ***** 03/02/15 ******************************
     FFDCAL19   IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEDL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDPL     IF   E           K DISK
     FFDDDL     IF   E           K DISK
     FFAZ2P     O    E             DISK
     FFAZ1L1    IF   E           K DISK    RENAME(PZ101:PZ1L1)
     FFAZ2L1    IF   E           K DISK    RENAME(PZ201:PZ2L1)
     FFTBSL     IF   E           K DISK
     FFAESP     UF A E             DISK
     FFAZ0P     UF A E             DISK
     FFAZ1P     UF A E             DISK
     F***** Beg Add *** 09/03/15 *******************************************
     FFMP1L     IF   E           K DISK
     F***** End Add *** 09/03/15 *******************************************
     F*
     D*
     D/COPY SCOPYWW01I
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D***** Beg Chg *** 09/29/15 *******************************************
     D                 DS
     D  Z1DS                   1     93
     D  Z1ACCT                 1     50
     D  Z1LNCT                51     55
     D  Z1CHRG                56     65
     D  Z1FSAL                66     75
     D  Z1PPO                 76     85
     D  Z1PRDT                86     93

     D***                 DS
     D***  Z1DS                   1     53
     D***  Z1ACCT                 1     10
     D***  Z1LNCT                11     15
     D***  Z1CHRG                16     25
     D***  Z1FSAL                26     35
     D***  Z1PPO                 36     45
     D***  Z1PRDT                46     53
     D***** End Chg *** 09/29/15 *******************************************

     D                 DS
     D*** Beg Chg *** 05/22/12 ***********************************************
     D**Z2DS****               1    859
     D*** Beg Chg *** 04/17/13 ***********************************************
     D*****  Z2DS                   1   1129
     D***** Beg Chg *** 09/03/15 *******************************************
     D*****  Z2DS                   1   1184
     D  Z2DS                   1   1849
     D***** End Chg *** 09/03/15 *******************************************
     D*** End Chg *** 04/17/13 ***********************************************
     D*** End Chg *** 05/22/12 ***********************************************
     D  Z2EOB#                 1     25
     D  Z2RFEI                26     35
     D  Z2RLSN                36     65
     D  Z2RFSN                66     85
     D  Z2RAD1                86    115
     D  Z2RAD2               116    145
     D  Z2RCTY               146    175
     D  Z2RST                176    177
     D  Z2RZIP               178    186
     D  Z2BFEI               187    196
     D  Z2BLSN               197    226
     D  Z2BFSN               227    246
     D  Z2BAD1               247    276
     D  Z2BAD2               277    306
     D  Z2BCTY               307    336
     D  Z2BST                337    338
     D  Z2BZIP               339    347
     D  Z2DIA1               348    353
     D  Z2DIA2               354    359
     D  Z2DIA3               360    365
     D  Z2DIA4               366    371
     D  Z2LIN#               372    374
     D  Z2DOS                375    382
     D  Z2PRCD               383    397
     D  Z2RVCD               398    400
     D  Z2BTYP               401    401
     D  Z2MOD                402    406
     D  Z2UNTS               407    411
     D  Z2ATIM               412    416
     D  Z2CHRG               417    425
     D  Z2PAID               426    434
     D  Z2PPO                435    443
     D  Z2AST                444    445
     D  Z2LBUS               446    447
     D  Z2UCR                448    448
     D  Z2REEV               449    449
     D  Z2ORID               450    474
     D***** Beg Chg *** 09/03/15 *******************************************
     D*** Z2FIL1               475    475
     D  Z2INOUT              475    475
     D***** End Chg *** 09/03/15 *******************************************
     D  Z2INET               476    476
     D  Z2FIL2               477    501
     D  Z2DRG                502    507
     D  Z2TOS                508    509
     D  Z2POS                510    511
     D  Z2CLM#               512    561
     D  Z2ACCT               562    611
     D  Z2ABID               612    636
     D  Z2RSCD               637    637
     D  Z2RSIN               638    652
     D  Z2BSCD               653    653
     D  Z2BRCD               654    668
     D  Z2BRDS               669    718
     D  Z2LSCD               719    719
     D  Z2LRCD               720    734
     D  Z2LRDS               735    784
     D  Z2NTNM               785    859
     D*** Beg Add *** 05/22/12 ****************************************************
     D***** Beg Chg *** 09/03/15 *******************************************
     D  Z2FSN                860    889
     D  Z2LSN                890    919
     D*** Z2LSN                860    889
     D*** Z2FSN                890    919
     D***** End Chg *** 09/03/15 *******************************************
     D  Z2RCOD               920    944
     D  Z2RCDS               945   1144
     D*** End Add *** 05/22/12 ****************************************************
     D*** Beg Add *** 04/17/13 ****************************************************
     D***** Beg Chg *** 09/03/15 **************************************************
     D  Z2RNPI              1145   1164
     D  Z2BNPI              1165   1184
     D*
     D*** Z2BNPI              1145   1164
     D*** Z2RNPI              1165   1184
     D***** End Chg *** 09/03/15 **************************************************
     D***** Beg Add *** 09/03/15 **************************************************
     D  Z2MOD2              1185   1189
     D  Z2MOD3              1190   1194
     D  Z2MOD4              1195   1199
     D  Z2CERT              1200   1200
     D  Z2ICDQUAL1          1201   1205
     D  Z2PDIAG             1206   1225
     D  Z2ADIAG             1226   1245
     D  Z2DIAG1             1246   1265
     D  Z2DIAG2             1266   1285
     D  Z2DIAG3             1286   1305
     D  Z2DIAG4             1306   1325
     D  Z2DIAG5             1326   1345
     D  Z2DIAG6             1346   1365
     D  Z2DIAG7             1366   1385
     D  Z2DIAG8             1386   1405
     D  Z2DIAG9             1406   1425
     D  Z2DIAG10            1426   1445
     D  Z2DIAG11            1446   1465
     D  Z2DIAG12            1466   1485
     D  Z2PRC               1486   1505
     D  Z2PDATE             1506   1513
     D  Z2PRC1              1514   1533
     D  Z2PRCDT1            1534   1541
     D  Z2PRC2              1542   1561
     D  Z2PRCDT2            1562   1569
     D  Z2PRC3              1570   1589
     D  Z2PRCDT3            1590   1597
     D  Z2PRC4              1598   1617
     D  Z2PRCDT4            1618   1625
     D  Z2PRC5              1626   1645
     D  Z2PRCDT5            1646   1653
     D  Z2PRC6              1654   1673
     D  Z2PRCDT6            1674   1681
     D  Z2PRC7              1682   1701
     D  Z2PRCDT7            1702   1709
     D  Z2PRC8              1710   1729
     D  Z2PRCDT8            1730   1737
     D  Z2PRC9              1738   1757
     D  Z2PRCDT9            1758   1765
     D  Z2PRC10             1766   1785
     D  Z2PRCDT10           1786   1793
     D  Z2PRC11             1794   1813
     D  Z2PRCDT11           1814   1821
     D  Z2PRC12             1822   1841
     D  Z2PRCDT12           1842   1849
     D***** End Add *** 09/03/15 **************************************************
     D*** End Add *** 04/17/13 ****************************************************
     D                 DS
     D  XXCLM#                 1     13
     D  CMFND                  1      3  0
     D  CMFYR                  4      6  0
     D  CMCASE                 7     13  0

     D                 DS
     D  ##DIAG                       24
     D  ##DIA1                 1      6
     D  ##DIA2                 7     12
     D  ##DIA3                13     18
     D  ##DIA4                19     24

     D  XXDIAG                        1    DIM(4)
     D  Z2DCD                         6    DIM(4)
     D  XXDCD                         5    DIM(4)
     D KKCODE          S                   LIKE(BSCODE)
     D KKTYPE          S                   LIKE(BSTYPE)
     D ##CHRG          S             10  2
     D ##FSAL          S             10  2
     D ##LNCT          S                   LIKE(CADSEQ)
     D KKDTSN          S                   LIKE(Z1DTSN)
     D KKSEQ           S                   LIKE(Z1FIL#)
     I*================================================================
     I*  /COPY required constants
     I*================================================================
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD1       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C     CMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C*
     C     DHKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DUKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
     C*** Beg Add *** 09/17/13 ********************************************
     C*
     C     DPKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
     C*** End Add *** 09/17/13 ********************************************
     C*
     C     EHKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EUKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     PDKY          KLIST
     C                   KFLD                    EAFEIN
     C                   KFLD                    EAMOD
     C*
     C     Z1KY1         KLIST
     C                   KFLD                    KKDTSN
     C                   KFLD                    KKSEQ
     C*
     C     Z1KY2         KLIST
     C                   KFLD                    KKDTSN
     C*
     C     BSKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C                   EXSR      PGMEND
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C* GET THE FILE NUMBER
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDTSN
     C                   MOVEL     *HIVAL        KKSEQ
     C*
     C     Z1KY1         SETGT     PZ1L1
     C     Z1KY2         READPE    PZ1L1                                  99
     C     *IN99         IFEQ      *OFF
     C     1             ADD       Z1FIL#        KKSEQ
     C                   ELSE
     C                   Z-ADD     1             KKSEQ
     C                   ENDIF
     C*
     C*** INITIALIZE FILES FOR FAZ1P
     C*
     C                   Z-ADD     0             ##LNCT
     C                   Z-ADD     0             ##CHRG
     C                   Z-ADD     0             ##FSAL
     C*
     C     *LOVAL        SETLL     PEA01
     C                   READ      PEA01                                  98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C     EOKY          CHAIN     PEO01                              97
     C     *IN97         IFEQ      *OFF
     C*
     C                   EXSR      INTHDR
     C                   EXSR      WRTHDR
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      SRMED
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      SRHOSP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      SRPHAR
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      SRDENT
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C                   EXSR      WRTREC
     C*
     C                   READ      PEA01                                  98
     C*
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C                   EXSR      WRTZ0P
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* SRDIAG - LOAD DIAGNOSIS CODES
     C*================================================================
     C     SRDIAG        BEGSR
     C*
     C                   EVAL      J = 1
     C                   DOW       J <= 4
     C                   EVAL      XXDIAG(J) = *BLANKS
     C                   EVAL      XXDCD(J) = *BLANKS
     C                   EVAL      Z2DCD(J) = *BLANKS
     C                   EVAL      J = J + 1
     C                   ENDDO
     C                   SELECT
     C*
     C                   WHEN      EOBTYP = 'N'
     C*
     C                   EVAL      XXDCD(1) = EHDIA1
     C                   EVAL      XXDCD(2) = EHDIA2
     C                   EVAL      XXDCD(3) = EHDIA3
     C                   EVAL      XXDCD(4) = EHDIA4
     C                   EVAL      XXDIAG(1) = DHDXR1
     C                   EVAL      XXDIAG(2) = DHDXR2
     C                   EVAL      XXDIAG(3) = DHDXR3
     C                   EVAL      XXDIAG(4) = DHDXR4
     C                   Z-ADD     1             J                 1 0
     C                   Z-ADD     1             CNT               1 0
     C                   DOW       J <= 4
     C                   IF        XXDIAG(J) <> ' '
     C                   MOVEL(P)  XXDCD(J)      Z2DCD(CNT)
     C                   EVAL      CNT = CNT + 1
     C                   ENDIF
     C                   EVAL      J = J + 1
     C                   ENDDO
     C*
     C                   MOVEA     Z2DCD         ##DIAG
     C                   MOVEL     ##DIA1        Z2DIA1
     C                   MOVEL     ##DIA2        Z2DIA2
     C                   MOVEL     ##DIA3        Z2DIA3
     C                   MOVEL     ##DIA4        Z2DIA4
     C*
     C                   WHEN      EOBTYP = 'D'
     C*
     C                   EVAL      XXDCD(1) = EDDIA1
     C                   EVAL      XXDCD(2) = EDDIA2
     C                   EVAL      XXDCD(3) = EDDIA3
     C                   EVAL      XXDCD(4) = EDDIA4
     C                   EVAL      XXDIAG(1) = DDDXR1
     C                   EVAL      XXDIAG(2) = DDDXR2
     C                   EVAL      XXDIAG(3) = DDDXR3
     C                   EVAL      XXDIAG(4) = DDDXR4
     C                   Z-ADD     1             J                 1 0
     C                   Z-ADD     1             CNT               1 0
     C                   DOW       J <= 4
     C                   IF        XXDIAG(J) <> ' '
     C                   MOVEL(P)  XXDCD(J)      Z2DCD(CNT)
     C                   EVAL      CNT = CNT + 1
     C                   ENDIF
     C                   EVAL      J = J + 1
     C                   ENDDO
     C*
     C                   MOVEA     Z2DCD         ##DIAG
     C                   MOVEL     ##DIA1        Z2DIA1
     C                   MOVEL     ##DIA2        Z2DIA2
     C                   MOVEL     ##DIA3        Z2DIA3
     C                   MOVEL     ##DIA4        Z2DIA4
     C*
     C                   WHEN      EOBTYP = 'H'
     C*
     C                   MOVEL(P)  EUDIA1        Z2DIA1
     C                   MOVEL(P)  EUDIA2        Z2DIA2
     C                   MOVEL(P)  EUDIA3        Z2DIA3
     C                   MOVEL(P)  EUDIA4        Z2DIA4
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SRMED - LOAD FIELDS FOR PHYSICIAN BILLS
     C*================================================================
     C     SRMED         BEGSR
     C*
     C     EHKY          CHAIN     PEH01                              96
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   EXSR      INTDET
     C*
     C                   EVAL      Z2LIN# = %TRIML(%EDITC(CALIN#:'4'))          LINE SEQ#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         Z2DOS                          DATE OF SERVICE
     C*
     C     DHKY          CHAIN     PDH01                              61
     C  N61              MOVEL(P)  DHPRCD        Z2PRCD                         PROCEDURE CODE
     C                   MOVE      *BLANKS       Z2RVCD                         REVENUE CODE
     C                   MOVEL     'H'           Z2BTYP                         BILL TYPE
     C*
     C***** Beg Chg *** 09/03/15 **************************************************
     C/Free
        If DHMod1 <> *blanks;
           Z2Mod = DHMod1;
        EndIf;
        If DHMod2 <> *blanks;
           Z2Mod2 = DHMod2;
        EndIf;
        If DHMod3 <> *blanks;
           Z2Mod3 = DHMod3;
        EndIf;
        If DHMod4 <> *blanks;
           Z2Mod4 = DHMod4;
        EndIf;

      /End-Free
     C*
     C****               MOVEL     '0'           MODFL             1
     C**** DHMOD1        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DHMOD1        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C**** DHMOD2        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DHMOD2        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C**** DHMOD3        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DHMOD3        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C**** DHMOD4        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DHMOD4        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C***** End Chg *** 09/03/15 **************************************************
     C*
     C***** Beg Chg *** 09/03/15 **************************************************
     C                   EXSR      SRLOADDIAG
     C****               EXSR      SRDIAG
     C***** End Chg *** 09/03/15 **************************************************
     C*
     C                   Z-ADD     CAUNTS        ##UNTS            3 0          UNITS
     C                   EVAL      Z2UNTS = %EDITC(##UNTS:'4')
     C                   EVAL      Z2UNTS = %TRIML(Z2UNTS)
     C                   MOVE      *ALL'0'       Z2ATIM                         ANESTHESIA TIME
     C*
     C                   EVAL      Z2CHRG = %EDITC(CACHRG:'Z')
     C*
     C                   IF        CAPAID > 0
     C                   EVAL      Z2PAID = %EDITC(CAPAID:'Z')
     C                   ELSE
     C                   MOVEL     CAPAID        Z2PAID
     C                   ENDIF
     C*
     C                   MOVE      *ALL'0'       Z2PPO                          PPO REDUCTION
     C                   MOVEL(P)  EOAST         Z2AST                          STATE JURISDICTION
     C                   MOVEL     'WC'          Z2LBUS                         LINE OF BUSINESS
     C     CARCFL        IFEQ      'Y'
     C                   MOVEL     'U'           Z2UCR                          UCR/FS FLAG
     C                   ELSE
     C                   MOVEL     'F'           Z2UCR                          UCR/FS FLAG
     C                   ENDIF
     C                   MOVEL     'N'           Z2REEV                         REEVAL FLAG
     C                   MOVE      *BLANKS       Z2ORID                         ORIGINAL BILL ID
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL1                         FILLER1
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      *BLANKS       Z2INET                         INNETWORK
     C                   MOVE      *BLANKS       Z2FIL2                         FILLER2
     C*
     C     EUKY          CHAIN     PEU01                              95
     C                   IF        *IN95 = *OFF                                 DRUG
     C                   EVAL      Z2DRG = %TRIML(%EDITC(EUDRG#:'4'))
     C                   ELSE
     C                   MOVE      *BLANKS       Z2DRG
     C                   ENDIF
     C*
     C                   MOVE(P)   DHTOS         Z2TOS                          TOS
     C                   MOVE(P)   DHPOS         Z2POS                          POS
     C                   MOVE      *BLANKS       Z2ACCT                         ACCOUNT CLIENT
     C                   MOVE      *BLANKS       Z2ABID                         ALTERNATE BILL ID
     C                   MOVE      *BLANKS       Z2RSCD                         RESUBMISSION CODE
     C                   MOVE      *BLANKS       Z2RSIN                         RESUBMISSION IND.DES
     C                   MOVE      *BLANKS       Z2BSCD                         BILL LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2BRCD                         BILL LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2BRDS                         BILL LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2LSCD                         LINE LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2LRCD                         LINE LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2LRDS                         LINE LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2NTNM                         INNETWORK NAME
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL3                         FILLER3
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      KKDTSN        Z2DTSN                         DATE SENT
     C                   MOVE      KKSEQ         Z2FIL#                         FILE NUMBER
     C                   MOVE      'S'           Z2PRFL                         PROCESSED FLG
     C*** Beg Chg *** 08/03/11 ****************************************
     C                   MOVEL(P)  EHRFEI        Z2RFEI
     C                   MOVEL(P)  EHRLSN        Z2RLSN
     C                   MOVEL(P)  EHRFSN        Z2RFSN
     C                   MOVEL(P)  EHRAD1        Z2RAD1
     C                   MOVEL(P)  EHRAD2        Z2RAD2
     C                   MOVEL(P)  EHRCTY        Z2RCTY
     C                   MOVEL(P)  EHRST         Z2RST
     C*** Beg Add *** 04/17/13 ****************************************
      /Free
       //***** Beg Chg *** 09/03/15 ****************************************
        Z2RNPI = EHSNPI;
        // Z2RNPI = EHRNPI;
       //***** End Chg *** 09/03/15 ****************************************
      /End-Free
     C*** End Add *** 04/17/13 ****************************************
     C                   EVAL      Z2RZIP = %EDITC(EHRZIP:'4')
     C                   EVAL      Z2RZIP = %TRIML(Z2RZIP)
     C*** End Chg *** 08/03/11 ****************************************
     C*
     C                   EXSR      WRTZ2P
     C*
     C                   EXSR      LODZ1P
     C*
     C     CAKY          READE     PCA01                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* SRHOSP - LOAD FIELDS FOR HOSPITAL BILLS
     C*================================================================
     C     SRHOSP        BEGSR
     C*
     C     EUKY          CHAIN     PEU01                              96
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   EXSR      INTDET
     C*
     C                   EVAL      Z2LIN# = %TRIML(%EDITC(CALIN#:'4'))          LINE SEQ#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         Z2DOS                          DATE OF SERVICE
     C*
     C     DUKY          CHAIN     PDU01                              61
     C  N61              MOVEL(P)  DURATE        Z2PRCD                         PROCEDURE CODE
     C                   MOVEL     DURVCD        Z2RVCD                         REVENUE CODE
     C                   MOVEL     'U'           Z2BTYP                         BILL TYPE
     C*
     C***** Beg Chg *** 09/03/15 **************************************************
     C/Free
        If DUPMod <> *blanks;
           Z2Mod = DUPMod;
        EndIf;
        If DUMod2 <> *blanks;
           Z2Mod2 = DUMod2;
        EndIf;
        If DUMod3 <> *blanks;
           Z2Mod3 = DUMod3;
        EndIf;
        If DUMod4 <> *blanks;
           Z2Mod4 = DUMod4;
        EndIf;

      /End-Free
     C*
     C****               MOVEL     '0'           MODFL             1
     C**** DUPMOD        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DUPMOD        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C**** DUMOD2        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DUMOD2        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C**** DUMOD3        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DUMOD3        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C**** DUMOD4        IFNE      *BLANKS
     C**** MODFL         ANDEQ     '0'
     C****               MOVEL(P)  DUMOD4        Z2MOD                          MODIFIER
     C****               MOVEL     '1'           MODFL
     C****               ENDIF
     C***** End Chg *** 09/03/15 **************************************************
     C*
     C***** Beg Chg *** 09/03/15 **************************************************
     C                   EXSR      SRLOADDIAG
     C****               EXSR      SRDIAG
     C***** End Chg *** 09/03/15 **************************************************
     C*
     C                   Z-ADD     CAUNTS        ##UNTS                         UNITS
     C                   EVAL      Z2UNTS = %EDITC(##UNTS:'4')
     C                   EVAL      Z2UNTS = %TRIML(Z2UNTS)
     C                   MOVE      *ALL'0'       Z2ATIM                         ANESTHESIA TIME
     C*
     C                   EVAL      Z2CHRG = %EDITC(CACHRG:'Z')
     C*
     C                   IF        CAPAID > 0
     C                   EVAL      Z2PAID = %EDITC(CAPAID:'Z')
     C                   ELSE
     C                   MOVEL     CAPAID        Z2PAID
     C                   ENDIF
     C*
     C                   MOVE      *ALL'0'       Z2PPO                          PPO REDUCTION
     C                   MOVEL(P)  EOAST         Z2AST                          STATE JURISDICTION
     C                   MOVEL     'WC'          Z2LBUS                         LINE OF BUSINESS
     C     CARCFL        IFEQ      'Y'
     C                   MOVEL     'U'           Z2UCR                          UCR/FS FLAG
     C                   ELSE
     C                   MOVEL     'F'           Z2UCR                          UCR/FS FLAG
     C                   ENDIF
     C                   MOVEL     'N'           Z2REEV                         REEVAL FLAG
     C                   MOVE      *BLANKS       Z2ORID                         ORIGINAL BILL ID
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL1                         FILLER1
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      *BLANKS       Z2INET                         INNETWORK
     C                   MOVE      *BLANKS       Z2FIL2                         FILLER2
     C*
     C     EUKY          CHAIN     PEU01                              95
     C                   IF        *IN95 = *OFF
     C                   EVAL      Z2DRG = %TRIML(%EDITC(EUDRG#:'4'))
     C                   ELSE
     C                   MOVE      *BLANKS       Z2DRG
     C                   ENDIF
     C*
     C                   MOVE(P)   *BLANKS       Z2TOS                          TOS
     C                   MOVE(P)   DUPOS         Z2POS                          POS
     C                   MOVE      *BLANKS       Z2ACCT                         ACCOUNT CLIENT
     C                   MOVE      *BLANKS       Z2ABID                         ALTERNATE BILL ID
     C                   MOVE      *BLANKS       Z2RSCD                         RESUBMISSION CODE
     C                   MOVE      *BLANKS       Z2RSIN                         RESUBMISSION IND.DES
     C                   MOVE      *BLANKS       Z2BSCD                         BILL LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2BRCD                         BILL LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2BRDS                         BILL LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2LSCD                         LINE LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2LRCD                         LINE LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2LRDS                         LINE LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2NTNM                         INNETWORK NAME
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL3                         FILLER3
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      KKDTSN        Z2DTSN                         DATE SENT
     C                   MOVE      KKSEQ         Z2FIL#                         FILE NUMBER
     C                   MOVE      'S'           Z2PRFL                         PROCESSED FLG
     C*** Beg Chg *** 08/03/11 ****************************************
     C                   MOVEL(P)  EURFEI        Z2RFEI
     C                   MOVEL(P)  EURLSN        Z2RLSN
     C                   MOVEL(P)  EURFSN        Z2RFSN
     C                   MOVEL(P)  EURAD1        Z2RAD1
     C                   MOVEL(P)  EURAD2        Z2RAD2
     C                   MOVEL(P)  EURNCT        Z2RCTY
     C                   MOVEL(P)  EURNST        Z2RST
     C                   EVAL      Z2RZIP = %EDITC(EURZIP:'4')
     C                   EVAL      Z2RZIP = %TRIML(Z2RZIP)
     C*** End Chg *** 08/03/11 ****************************************
     C*** Beg Add *** 09/03/15 ****************************************
     C/free
        If EObtyp = 'H';
           if EUicn = 'IS' or EUicn = 'IP' or
              EUicn = 'IR' or EUicn = 'IM';
              Z2InOut = 'I';
           endif;
           if EUicn = 'HO' or EUicn = 'HS' or
              EUicn = 'AO' or EUicn = 'AS';
              Z2InOut = 'O';
           endif;
        EndIf;
      /end-free
     C*** End Add *** 09/03/15 ****************************************
     C*
     C                   EXSR      WRTZ2P
     C*
     C                   EXSR      LODZ1P
     C*
     C     CAKY          READE     PCA01                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* SRPHAR - LOAD FIELDS FOR PHARMACY BILLS
     C*================================================================
     C     SRPHAR        BEGSR
     C*
     C     EHKY          CHAIN     PEP01                              96
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   EXSR      INTDET
     C*
     C                   EVAL      Z2LIN# = %TRIML(%EDITC(CALIN#:'4'))          LINE SEQ#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         Z2DOS                          DATE OF SERVICE
     C*
     C                   MOVE(P)   *BLANKS       Z2PRCD                         PROCEDURE CODE
     C*** Beg Add *** 09/17/13 *********************************************
     C     dpky          Chain     pdp01                              99
     C                   if        *in99 = *off
     C                   eval      z2prcd = dpndc
     C                   endif
     C*** End Add *** 09/17/13 *********************************************
     C                   MOVE      *BLANKS       Z2RVCD                         REVENUE CODE
     C                   MOVEL     'H'           Z2BTYP                         BILL TYPE
     C*
     C                   MOVE      *BLANKS       Z2MOD                          MODIFIER
     C                   Z-ADD     CAUNTS        ##UNTS                         UNITS
     C                   EVAL      Z2UNTS = %EDITC(##UNTS:'4')
     C                   EVAL      Z2UNTS = %TRIML(Z2UNTS)
     C                   MOVE      *ALL'0'       Z2ATIM                         ANESTHESIA TIME
     C*
     C                   EVAL      Z2CHRG = %EDITC(CACHRG:'Z')
     C*
     C                   IF        CAPAID > 0
     C                   EVAL      Z2PAID = %EDITC(CAPAID:'Z')
     C                   ELSE
     C                   MOVEL     CAPAID        Z2PAID
     C                   ENDIF
     C*
     C                   MOVE      *ALL'0'       Z2PPO                          PPO REDUCTION
     C                   MOVEL(P)  EOAST         Z2AST                          STATE JURISDICTION
     C                   MOVEL     'WC'          Z2LBUS                         LINE OF BUSINESS
     C     CARCFL        IFEQ      'Y'
     C                   MOVEL     'U'           Z2UCR                          UCR/FS FLAG
     C                   ELSE
     C                   MOVEL     'F'           Z2UCR                          UCR/FS FLAG
     C                   ENDIF
     C                   MOVEL     'N'           Z2REEV                         REEVAL FLAG
     C                   MOVE      *BLANKS       Z2ORID                         ORIGINAL BILL ID
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL1                         FILLER1
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      *BLANKS       Z2INET                         INNETWORK
     C                   MOVE      *BLANKS       Z2FIL2                         FILLER2
     C*
     C                   MOVE      *BLANKS       Z2DRG                          DRUG
     C*
     C                   MOVE      *BLANKS       Z2TOS                          TOS
     C                   MOVE      *BLANKS       Z2POS                          POS
     C                   MOVE      *BLANKS       Z2ACCT                         ACCOUNT CLIENT
     C                   MOVE      *BLANKS       Z2ABID                         ALTERNATE BILL ID
     C                   MOVE      *BLANKS       Z2RSCD                         RESUBMISSION CODE
     C                   MOVE      *BLANKS       Z2RSIN                         RESUBMISSION IND.DES
     C                   MOVE      *BLANKS       Z2BSCD                         BILL LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2BRCD                         BILL LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2BRDS                         BILL LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2LSCD                         LINE LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2LRCD                         LINE LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2LRDS                         LINE LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2NTNM                         INNETWORK NAME
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL3                         FILLER3
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      KKDTSN        Z2DTSN                         DATE SENT
     C                   MOVE      KKSEQ         Z2FIL#                         FILE NUMBER
     C                   MOVE      'S'           Z2PRFL                         PROCESSED FLG
     C*
     C                   EXSR      WRTZ2P
     C*
     C                   EXSR      LODZ1P
     C*
     C     CAKY          READE     PCA01                                  60
     C*
     C                   ENDDO
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SRDENT - LOAD FIELDS FOR DENTAL BILLS
     C*================================================================
     C     SRDENT        BEGSR
     C*
     C     EHKY          CHAIN     PED01                              96
     C*
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   EXSR      INTDET
     C*
     C                   EVAL      Z2LIN# = %TRIML(%EDITC(CALIN#:'4'))          LINE SEQ#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         Z2DOS                          DATE OF SERVICE
     C*
     C     DHKY          CHAIN     PDD01                              61
     C  N61              MOVEL(P)  DDPRCD        Z2PRCD                         PROCEDURE CODE
     C                   MOVE      *BLANKS       Z2RVCD                         REVENUE CODE
     C                   MOVEL     'H'           Z2BTYP                         BILL TYPE
     C*
     C                   MOVE      *BLANKS       Z2MOD                          MODIFIER
     C*
     C***** Beg Chg *** 09/03/15 **************************************************
     C                   EXSR      SRLOADDIAG
     C****               EXSR      SRDIAG
     C***** End Chg *** 09/03/15 **************************************************
     C*
     C                   Z-ADD     CAUNTS        ##UNTS                         UNITS
     C                   EVAL      Z2UNTS = %EDITC(##UNTS:'4')
     C                   EVAL      Z2UNTS = %TRIML(Z2UNTS)
     C                   MOVE      *ALL'0'       Z2ATIM                         ANESTHESIA TIME
     C*
     C                   EVAL      Z2CHRG = %EDITC(CACHRG:'Z')
     C*
     C                   IF        CAPAID > 0
     C                   EVAL      Z2PAID = %EDITC(CAPAID:'Z')
     C                   ELSE
     C                   MOVEL     CAPAID        Z2PAID
     C                   ENDIF
     C*
     C                   MOVE      *ALL'0'       Z2PPO                          PPO REDUCTION
     C                   MOVEL(P)  EOAST         Z2AST                          STATE JURISDICTION
     C                   MOVEL     'WC'          Z2LBUS                         LINE OF BUSINESS
     C     CARCFL        IFEQ      'Y'
     C                   MOVEL     'U'           Z2UCR                          UCR/FS FLAG
     C                   ELSE
     C                   MOVEL     'F'           Z2UCR                          UCR/FS FLAG
     C                   ENDIF
     C                   MOVEL     'N'           Z2REEV                         REEVAL FLAG
     C                   MOVE      *BLANKS       Z2ORID                         ORIGINAL BILL ID
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL1                         FILLER1
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      *BLANKS       Z2INET                         INNETWORK
     C                   MOVE      *BLANKS       Z2FIL2                         FILLER2
     C*
     C     EUKY          CHAIN     PEU01                              95
     C                   IF        *IN95 = *OFF
     C                   EVAL      Z2DRG = %TRIML(%EDITC(EUDRG#:'4'))
     C                   ELSE
     C                   MOVE      *BLANKS       Z2DRG
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Z2TOS                          TOS
     C                   MOVE      *BLANKS       Z2POS                          POS
     C                   MOVE      *BLANKS       Z2ACCT                         ACCOUNT CLIENT
     C                   MOVE      *BLANKS       Z2ABID                         ALTERNATE BILL ID
     C                   MOVE      *BLANKS       Z2RSCD                         RESUBMISSION CODE
     C                   MOVE      *BLANKS       Z2RSIN                         RESUBMISSION IND.DES
     C                   MOVE      *BLANKS       Z2BSCD                         BILL LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2BRCD                         BILL LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2BRDS                         BILL LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2LSCD                         LINE LEVEL STATUS CD
     C                   MOVE      *BLANKS       Z2LRCD                         LINE LEVEL REJECT CD
     C                   MOVE      *BLANKS       Z2LRDS                         LINE LVL REJECT CD D
     C                   MOVE      *BLANKS       Z2NTNM                         INNETWORK NAME
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL3                         FILLER3
     C***** End Dlt *** 09/03/15 **************************************************
     C*** Beg Chg *** 08/03/11 ****************************************
     C                   MOVEL(P)  EDRFEI        Z2RFEI
     C                   MOVEL(P)  EDRLSN        Z2RLSN
     C                   MOVEL(P)  EDRFSN        Z2RFSN
     C                   MOVEL(P)  EDRAD1        Z2RAD1
     C                   MOVEL(P)  EDRAD2        Z2RAD2
     C                   MOVEL(P)  EDRCTY        Z2RCTY
     C                   MOVEL(P)  EDRST         Z2RST
     C*** Beg Add *** 04/17/13 ****************************************
      /Free
        Z2RNPI = EDTNPI;
      /End-Free
     C*** End Add *** 04/17/13 ****************************************
     C                   EVAL      Z2RZIP = %EDITC(EDRZIP:'4')
     C                   EVAL      Z2RZIP = %TRIML(Z2RZIP)
     C*** End Chg *** 08/03/11 ****************************************
     C*
     C                   EXSR      WRTZ2P
     C*
     C                   EXSR      LODZ1P
     C*
     C     CAKY          READE     PCA01                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* WRTZ2P - WRITE RECORDS IN FAZ2P
     C*================================================================
     C     WRTZ2P        BEGSR
     C*
     C                   MOVE      KKDTSN        Z2DTSN                         DATE SENT
     C                   MOVE      KKSEQ         Z2FIL#                         FILE NUMBER
     C                   MOVE      'S'           Z2PRFL                         PROCESSED FLG
     C*
     C                   WRITE     PZ201
     C*
     C                   ENDSR
     C*================================================================
     C* WRTREC - WRITE RECORDS IN FAEAP and FAESP
     C*================================================================
     C     WRTREC        BEGSR
     C*
     C* WRITE A NEW RECORD IN BILL STATUS HISTORY FILE
     C*
     C                   CLEAR                   PES01
     C                   EVAL      ESCO# = EACO#
     C                   EVAL      ESSUB# = EASUB#
     C                   EVAL      ESFND = EAFND
     C                   EVAL      ESFYR = EAFYR
     C                   EVAL      ESCASE = EACASE
     C                   EVAL      ESSS# = EASS#
     C                   EVAL      ESACDT = EAACDT
     C                   EVAL      ESEOB# = EAEOB#
     C                   EVAL      ESBLST = 'P'
     C                   EVAL      ESSTCD = 'SPR'
     C                   EVAL      KKTYPE = 'P'
     C                   EVAL      KKCODE = 'SPR'
     C*
     C     BSKY          CHAIN     PBS01                              45
     C  N45              EVAL      ESREAS = BSDESC
     C*
     C                   EVAL      ESSTAT = 'A'
     C                   EVAL      ESUSER = Q1USER
     C                   EVAL      ESPGM = Q1PGM
     C                   EVAL      ESTIME = DSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESDT
     C                   EVAL      ESIUSR = Q1USER
     C                   EVAL      ESIPGM = Q1PGM
     C                   EVAL      ESITIM = DSTIME
     C                   EVAL      ESIDT = ESDT
     C*
     C                   WRITE     PES01
     C*
     C* UPDATE FAEAP TO MARK BILL AS SENT TO PROCURA FOR RE-PRICING
     C*
     C                   EVAL      EANSNT = 'S'
     C                   UPDATE    PEA01
     C*
     C***** Beg Add ***** 03/02/15 ************************************
     C* Update FAEOP setting network sent flag as 'S'.
      /Free
       Chain eoky peo01;
          If %Found(faeol4);
             EOnsnt = 'S';
             EOpgm  = Q1pgm;
             EOtime = DStime;
       //*** Beg Chg *** 10/05/15 ******************************************
             EOdt = %dec(%char(%date(DSdate:*usa):*cymd0):7:0);
       //***    EOdt   = DSdate;
       //*** End Chg *** 10/05/15 ******************************************

             Update peo01;
          Endif;
      /End-Free
     C***** End Add ***** 03/02/15 ************************************
     C                   ENDSR
     C*================================================================
     C* LODZ1P - ACCUMULATE TOTALS FOR FAZ1P
     C*================================================================
     C     LODZ1P        BEGSR
     C*
     C                   ADD       1             ##LNCT                         TOTAL LINES
     C                   ADD       CACHRG        ##CHRG                         TOTAL CHARGES
     C                   IF        CACSCD = '1'                                 TOTAL FS ALLOWANCES
     C                   EVAL      ##FSAL = ##FSAL + (CACHRG - CAMRA)
     C                   ELSE
     C*
     C*** beg add *** 09/26/11 *********************************************************************
     C*
     C                   IF        CAMRAA <> 0
     C*
     C*** end add *** 09/26/11 *********************************************************************
     C*
     C                   EVAL      ##FSAL = ##FSAL + CAMRAA
     C*
     C*** beg add *** 09/26/11 *********************************************************************
     C*
     C                   ELSE
     C                   EVAL      ##FSAL = ##FSAL + (CACHRG - CAMRA)
     C                   ENDIF
     C*
     C*** end add *** 09/26/11 *********************************************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTZ0P - WRITE DATA TO EXTRACT FILE
     C*================================================================
     C     WRTZ0P        BEGSR
     C*
     C*** Beg Add *** 04/17/13 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End Add *** 04/17/13 ****************************************
     C                   CLEAR                   PZ101
     C*
     C*Account Id
     C*
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   EVAL      Z1ACCT = 'CSI'
     C                   ELSE
     C*** End Add *** 04/17/13 ****************************************
     C                   EVAL      Z1ACCT = 'SYNERGY'
     C*** Beg Add *** 04/17/13 ****************************************
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*
     C*Number of lines
     C*
     C                   EVAL      Z1LNCT = %EDITC(##LNCT:'Z')
     C*
     C                   EVAL      Z1LNCT = %TRIML(Z1LNCT)
     C*
     C*Total Charges
     C*
     C                   EVAL      Z1CHRG = %EDITC(##CHRG:'Z')
     C*
     C*Total FS Aloowance
     C*
     C                   EVAL      Z1FSAL = %EDITC(##FSAL:'Z')
     C*
     C*Date sent
     C*
     C                   EVAL      Z1DTSN = KKDTSN
     C*
     C*File Number
     C*
     C                   EVAL      Z1FIL# = KKSEQ
     C*
     C                   WRITE     PZ101
     C*
     C*** WRITE RECORD IN FAZ0P FROM FAZ1P
     C*
     C     Z1KY1         CHAIN     PZ1L1                              45
     C                   CLEAR                   PZ001
     C  N45              MOVEL(P)  Z1DS          Z0DATA
     C                   WRITE     PZ001
     C*
     C*** WRITE RECORD IN FAZ0P FROM FAZ2P
     C*
     C     Z1KY1         SETLL     PZ2L1
     C     Z1KY1         READE     PZ2L1                                  45
     C                   DOW       *IN45 = *OFF
     C                   CLEAR                   PZ001
     C                   MOVEL(P)  Z2DS          Z0DATA
     C                   WRITE     PZ001
     C     Z1KY1         READE     PZ2L1                                  45
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* INTHDR - INITIALIZE HEADER FIELDS
     C*================================================================
     C     INTHDR        BEGSR
     C*
     C                   MOVE      *BLANKS       Z2EOB#
     C                   MOVE      *BLANKS       Z2RFEI
     C                   MOVE      *BLANKS       Z2RLSN
     C                   MOVE      *BLANKS       Z2RFSN
     C                   MOVE      *BLANKS       Z2RAD1
     C                   MOVE      *BLANKS       Z2RAD2
     C                   MOVE      *BLANKS       Z2RCTY
     C                   MOVE      *BLANKS       Z2RST
     C                   MOVE      *BLANKS       Z2RZIP
     C*
     C                   MOVE      *BLANKS       Z2BFEI
     C                   MOVE      *BLANKS       Z2BLSN
     C                   MOVE      *BLANKS       Z2BFSN
     C                   MOVE      *BLANKS       Z2BAD1
     C                   MOVE      *BLANKS       Z2BAD2
     C                   MOVE      *BLANKS       Z2BCTY
     C                   MOVE      *BLANKS       Z2BST
     C                   MOVE      *BLANKS       Z2BZIP
     C*
     C                   MOVE      *BLANKS       Z2CLM#
     C*
     C*** Beg Add *** 05/22/12 ************************************************
      /Free
        Z2LSN = ' ' ;
        Z2FSN = ' ' ;
      /End-Free
     C*** End Add *** 05/22/12 ************************************************
     C*** Beg Add *** 04/17/13 ************************************************
      /Free
        Z2BNPI = ' ' ;
        Z2RNPI = ' ' ;
      /End-Free
     C*** End Add *** 04/17/13 ************************************************
     C                   ENDSR
     C*================================================================
     C* INTDET - INITIALIZE DETAIL FIELDS
     C*================================================================
     C     INTDET        BEGSR
     C*
     C                   MOVE      *BLANKS       Z2DIA1
     C                   MOVE      *BLANKS       Z2DIA2
     C                   MOVE      *BLANKS       Z2DIA3
     C                   MOVE      *BLANKS       Z2DIA4
     C                   MOVE      *BLANKS       Z2LIN#
     C                   MOVE      *BLANKS       Z2DOS
     C                   MOVE      *BLANKS       Z2PRCD
     C                   MOVE      *BLANKS       Z2RVCD
     C                   MOVE      *BLANKS       Z2BTYP
     C*
     C                   MOVE      *BLANKS       Z2MOD
     C                   MOVE      *BLANKS       Z2UNTS
     C                   MOVE      *BLANKS       Z2ATIM
     C                   MOVE      *BLANKS       Z2CHRG
     C                   MOVE      *BLANKS       Z2PAID
     C                   MOVE      *BLANKS       Z2PPO
     C                   MOVE      *BLANKS       Z2AST
     C                   MOVE      *BLANKS       Z2LBUS
     C*
     C                   MOVE      *BLANKS       Z2UCR
     C                   MOVE      *BLANKS       Z2REEV
     C                   MOVE      *BLANKS       Z2ORID
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL1
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      *BLANKS       Z2INET
     C                   MOVE      *BLANKS       Z2FIL2
     C***** Beg Add *** 09/03/15 **************************************************
     C                   MOVE      *BLANKS       Z2FILL2
     C***** End Add *** 09/03/15 **************************************************
     C                   MOVE      *BLANKS       Z2DRG
     C                   MOVE      *BLANKS       Z2TOS
     C                   MOVE      *BLANKS       Z2POS
     C                   MOVE      *BLANKS       Z2ACCT
     C                   MOVE      *BLANKS       Z2ABID
     C                   MOVE      *BLANKS       Z2RSCD
     C                   MOVE      *BLANKS       Z2RSIN
     C                   MOVE      *BLANKS       Z2BSCD
     C                   MOVE      *BLANKS       Z2BRCD
     C                   MOVE      *BLANKS       Z2BRDS
     C                   MOVE      *BLANKS       Z2LSCD
     C                   MOVE      *BLANKS       Z2LRCD
     C                   MOVE      *BLANKS       Z2LRDS
     C                   MOVE      *BLANKS       Z2NTNM
     C***** Beg Dlt *** 09/03/15 **************************************************
     C****               MOVE      *BLANKS       Z2FIL3
     C***** End Dlt *** 09/03/15 **************************************************
     C                   MOVE      *ZEROS        Z2DTSN
     C                   MOVE      *ZEROS        Z2FIL#
     C                   MOVE      *BLANKS       Z2PRFL
     C***** Beg Add *** 09/03/15 ****************************************
     C/Free
        Z2MOD2     = *blanks;
        Z2MOD3     = *blanks;
        Z2MOD4     = *blanks;
        Z2CERT     = *blanks;
        Z2ICDQUAL1 = *blanks;
        Z2PDIAG    = *blanks;
        Z2ADIAG    = *blanks;
        Z2DIAG1    = *blanks;
        Z2DIAG2    = *blanks;
        Z2DIAG3    = *blanks;
        Z2DIAG4    = *blanks;
        Z2DIAG5    = *blanks;
        Z2DIAG6    = *blanks;
        Z2DIAG7    = *blanks;
        Z2DIAG8    = *blanks;
        Z2DIAG9    = *blanks;
        Z2DIAG10   = *blanks;
        Z2DIAG11   = *blanks;
        Z2DIAG12   = *blanks;
        Z2PRC      = *blanks;
        Z2PDATE    = *blanks;
        Z2PRC1     = *blanks;
        Z2PRCDT1   = *blanks;
        Z2PRC2     = *blanks;
        Z2PRCDT2   = *blanks;
        Z2PRC3     = *blanks;
        Z2PRCDT3   = *blanks;
        Z2PRC4     = *blanks;
        Z2PRCDT4   = *blanks;
        Z2PRC5     = *blanks;
        Z2PRCDT5   = *blanks;
        Z2PRC6     = *blanks;
        Z2PRCDT6   = *blanks;
        Z2PRC7     = *blanks;
        Z2PRCDT7   = *blanks;
        Z2PRC8     = *blanks;
        Z2PRCDT8   = *blanks;
        Z2PRC9     = *blanks;
        Z2PRCDT9   = *blanks;
        Z2PRC10    = *blanks;
        Z2PRCDT10  = *blanks;
        Z2PRC11    = *blanks;
        Z2PRCDT11  = *blanks;
        Z2PRC12    = *blanks;
        Z2PRCDT12  = *blanks;
      /End-Free
     C***** End Add *** 09/03/15 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* WRTHDR - LOAD HEADER FIELDS
     C*================================================================
     C     WRTHDR        BEGSR
     C*
     C                   EVAL      Z2EOB# = %EDITC(EAEOB#:'4')
     C                   EVAL      Z2EOB# = %TRIML(Z2EOB#)
     C*** Beg Del *** 08/03/11 ****************************************
      *******            MOVEL(P)  EOFEIN        Z2RFEI
      *******            MOVEL(P)  EAPNAM        Z2RLSN
     C*** End Del *** 08/03/11 ****************************************
     C*
     C*** GET RENDERING PROVIDER FIRST NAME
     C*
     C     PDKY          CHAIN     PPD01                              60
     C*** Beg Chg *** 08/03/11 ****************************************
      *************  N60              MOVEL(P)  PDFSN         Z2RFSN
     C  N60              MOVEL(P)  PDFSN         Z2BFSN
     C*
      *************      MOVEL(P)  EAADR1        Z2RAD1
      *************      MOVEL(P)  EAADR2        Z2RAD2
      *************      MOVEL(P)  EACTY         Z2RCTY
      *************      MOVEL(P)  EAST          Z2RST
      *************      EVAL      Z2RZIP = %EDITC(EAZIP:'4')
      *************      EVAL      Z2RZIP = %TRIML(Z2RZIP)
     C*
      *************      MOVEL(P)  Z2RFEI        Z2BFEI
      *************      MOVEL(P)  Z2RLSN        Z2BLSN
      *************      MOVEL(P)  Z2RFSN        Z2BFSN
      *************      MOVEL(P)  Z2RAD1        Z2BAD1
      *************      MOVEL(P)  Z2RAD2        Z2BAD2
      *************      MOVEL(P)  Z2RCTY        Z2BCTY
      *************      MOVEL(P)  Z2RST         Z2BST
      *************      MOVEL(P)  Z2RZIP        Z2BZIP
     C                   MOVEL(P)  EAFEIN        Z2BFEI
     C                   MOVEL(P)  EAPNAM        Z2BLSN
     C                   MOVEL(P)  EAADR1        Z2BAD1
     C                   MOVEL(P)  EAADR2        Z2BAD2
     C                   MOVEL(P)  EACTY         Z2BCTY
     C                   MOVEL(P)  EAST          Z2BST
     C                   EVAL      Z2BZIP = %EDITC(EAZIP:'4')
     C                   EVAL      Z2BZIP = %TRIML(Z2BZIP)
     C*** End Chg *** 08/03/11 ****************************************
     C*** Beg Add *** 04/17/13 ****************************************
      /Free
       //***** Beg Chg *** 09/03/15 **************************************
        Chain(n) pdky pp101;
        if %found(fmp1l);
           Z2BNPI = P1N#02;
        endif;

        // Z2BNPI = EABNPI;
       //***** End Chg *** 09/03/15 **************************************
      /End-Free
     C*** End Add *** 04/17/13 ****************************************
     C*
     C*** GET CLAIM#
     C*
     C     CMKY          CHAIN     PCM01                              60
     C  N60              MOVEL(P)  XXCLM#        Z2CLM#
     C*
     C*** Beg Add *** 05/22/12 ****************************************
      /Free
       If *in60 =  *off ;
        Z2LSN = CMLSN;
        Z2FSN = CMFSN;
       Endif;
      /End-Free
     C*** End Add *** 05/22/12 ****************************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update FAEAP.
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C                   MOVEL(P)  'S'           EANSNT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EANPDT
     C*
     C                   MOVEL(P)  Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDSR
     C***** Beg Add *** 09/03/15 **************************************************
     C*================================================================
     C* SRLOADDIAG - LOAD DIAGNOSIS CODES
     C*================================================================
     C     SRLOADDIAG    BEGSR
     C*
     C/Free

        Select;

         // Physician
         When EOBTyp = 'N';

          If EHICDInd = 'Y';
            // If ICD 10

            Z2ICDQual1 = 'ICD10';
            Z2Diag1    = EHICD1001;
            Z2Diag2    = EHICD1002;
            Z2Diag3    = EHICD1003;
            Z2Diag4    = EHICD1004;

          Else;
            // If ICD 9

            Z2ICDQual1 = 'ICD9';
            Z2Diag1    = EHDia1;
            Z2Diag2    = EHDia2;
            Z2Diag3    = EHDia3;
            Z2Diag4    = EHDia4;

          EndIf;

         // Dental
         When EOBTyp = 'D';

          If EDICDInd = 'Y';
            // If ICD 10

            Z2ICDQual1 = 'ICD10';
            Z2Diag1    = EDICD1001;
            Z2Diag2    = EDICD1002;
            Z2Diag3    = EDICD1003;
            Z2Diag4    = EDICD1004;

          Else;
            // If ICD 9

            Z2ICDQual1 = 'ICD9';
            Z2Diag1    = EDDia1;
            Z2Diag2    = EDDia2;
            Z2Diag3    = EDDia3;
            Z2Diag4    = EDDia4;

          EndIf;

         // Hospital
         When EOBTyp = 'H';

          If EUICDInd = 'Y';
            // If ICD 10

            Z2ICDQual1 = 'ICD10';
            Z2PDiag    = EUICD1001;
            Z2ADiag    = EUADia1001;
            Z2Diag1    = EUICD1002;
            Z2Diag2    = EUICD1003;
            Z2Diag3    = EUICD1004;
            Z2Diag4    = EUICD1005;
            Z2Diag5    = EUICD1006;
            Z2Diag6    = EUICD1007;
            Z2Diag7    = EUICD1008;
            Z2Prc      = EUPrc1001;
            Monitor;
             Z2PDate    = %char(%date(EUPrD1:*cymd):*iso0);
            On-Error;
             Z2PDate    = '00010101';
            EndMon;
            Z2Prc1     = EUPrc1002;
            Monitor;
             Z2PrcDt1   = %char(%date(EUPrD2:*cymd):*iso0);
            On-Error;
             Z2PrcDt1   = '00010101';
            EndMon;
            Z2Prc2     = EUPrc1003;
            Monitor;
             Z2PrcDt2   = %char(%date(EUPrD3:*cymd):*iso0);
            On-Error;
             Z2PrcDt2   = '00010101';
            EndMon;
            Z2Prc3     = EUPrc1004;
            Monitor;
             Z2PrcDt3   = %char(%date(EUPrD4:*cymd):*iso0);
            On-Error;
             Z2PrcDt3   = '00010101';
            EndMon;
            Z2Prc4     = EUPrc1005;
            Monitor;
             Z2PrcDt4   = %char(%date(EUPrD5:*cymd):*iso0);
            On-Error;
             Z2PrcDt4   = '00010101';
            EndMon;
            Z2Prc5     = EUPrc1006;
            Monitor;
             Z2PrcDt5   = %char(%date(EUPrD6:*cymd):*iso0);
            On-Error;
             Z2PrcDt5   = '00010101';
            EndMon;

          Else;
            // If ICD 9

            Z2ICDQual1 = 'ICD9';
            Z2PDiag    = EUDia1;
            Z2ADiag    = EUADDI;
            Z2Diag1    = EUDia2;
            Z2Diag2    = EUDia3;
            Z2Diag3    = EUDia4;
            Z2Diag4    = EUDia5;
            Z2Diag5    = EUDia6;
            Z2Diag6    = EUDia7;
            Z2Diag7    = EUDia8;
            Z2Prc      = EUPrc1;
            Monitor;
             Z2PDate    = %char(%date(EUPrD1:*cymd):*iso0);
            On-Error;
             Z2PDate    = '00010101';
            EndMon;
            Z2Prc1     = EUPrc2;
            Monitor;
             Z2PrcDt1   = %char(%date(EUPrD2:*cymd):*iso0);
            On-Error;
             Z2PrcDt1   = '00010101';
            EndMon;
            Z2Prc2     = EUPrc3;
            Monitor;
             Z2PrcDt2   = %char(%date(EUPrD3:*cymd):*iso0);
            On-Error;
             Z2PrcDt2   = '00010101';
            EndMon;
            Z2Prc3     = EUPrc4;
            Monitor;
             Z2PrcDt3   = %char(%date(EUPrD4:*cymd):*iso0);
            On-Error;
             Z2PrcDt3   = '00010101';
            EndMon;
            Z2Prc4     = EUPrc5;
            Monitor;
             Z2PrcDt4   = %char(%date(EUPrD5:*cymd):*iso0);
            On-Error;
             Z2PrcDt4   = '00010101';
            EndMon;
            Z2Prc5     = EUPrc6;
            Monitor;
             Z2PrcDt5   = %char(%date(EUPrD6:*cymd):*iso0);
            On-Error;
             Z2PrcDt5   = '00010101';
            EndMon;

          EndIf;

        EndSl;

      /End-Free
     C*
     C                   ENDSR
     C***** End Add *** 09/03/15 **************************************************
