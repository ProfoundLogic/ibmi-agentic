     H* (c) 2013 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H*
     H*****************************************************************
     H*  F8067R - WRITE TO FOZ9P                     C:277 12/02/11   *
     H*                                              PROJ: 114MCV     *
     H*  CALLED FROM: F8066C1                                         *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*   08/21/13 C:296 PROJ : 114MCV P27361                         *
     H*                  Change Field Mapping Interface               *
     H*                  from WCMC to SIS for checks                  *
     H*                                                               *
     H*   03/19/12 C:277 PROJ : 114MCV PRISS #29                      *
     H*                  Add logic to fomat -ive amount               *
     H*                                                               *
     H*   02/07/12 C:277 PROJ : 114MCV PRISS # 25                     *
     H*                  Change the logic to pay amount correctly for *
     H*                  RCN                                          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     F*  Physical file definition (for maintaining record)
     FFAEOL4    IF   E           K DISK
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
     FFTBSL     IF   E           K DISK
     FFAESP     UF A E             DISK
     FFAEAL1    UF   E             DISK
     FFAEEL     IF   E           K DISK
     FFDDEL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFDDAL1    IF   E           K DISK
     FFOZ0L1    O    E             DISK
     FFOZ1P     O    E             DISK
     FFOZ2P     O    E             DISK
     FFOZ9L1    O    E             DISK
     FFOZ1L1    IF   E           K DISK
     F                                     RENAME(Z1P01:Z1P02)
     F*** BEG ADD 02/07/12 *****************************************************
     FFAEAL10   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02) PREFIX(P2:2)
     F*** END ADD 02/07/12 *****************************************************
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
     D                 DS
     D Z1DS          E DS                  EXTNAME(FOZ1P)
     D Z2DS          E DS                  EXTNAME(FOZ2P)
     D*
     D                 DS
     D  ##DIAG                       24
     D  ##DIA1                 1      6
     D  ##DIA2                 7     12
     D  ##DIA3                13     18
     D  ##DIA4                19     24
     D*
     D                 DS
     D XXCLM#                        20
     D XXCLM                   1      7  0
     D XXFND                   8     10
     D XXFYR                  11     13
     D XXCASE                 14     20
     D*
     D  XXDIAG                        1    DIM(4)
     D  XXDCD                         5    DIM(4)
     D KKCODE          S                   LIKE(BSCODE)
     D KKTYPE          S                   LIKE(BSTYPE)
     D XXPROV          S             12
     D XXFEIN          S              9
     D XXMOD           S              3
     D XXPOS           S              1  0
     D XXCOUNT         S              3  0
     D XXDXR           S              4
     D XXRCD           S              9  0
     D XXPAID          S                   LIKE(EOPAID)
     D KKICD9          S                   LIKE(DIICD9)
     D XXMBR           S                   LIKE(Z9MBR)
     D KKTRID          S                   LIKE(U1TRID)
     D*** BEG ADD 02/07/12 *****************************************************
     D X1CHRG          S                   LIKE(EACHRG)
     D X1MRA           S                   LIKE(EAMRA )
     D X1NDIS          S                   LIKE(EANDIS)
     D X1PAID          S                   LIKE(EAPAID)
     D X1OTHR          S                   LIKE(EAOTHR)
     D X1OTH2          S                   LIKE(EAOTH2)
     D X1RCN           S                   LIKE(EARCN )
     D*** END ADD 02/07/12 *****************************************************
     I*================================================================
     I*  /COPY required constants
     I*================================================================
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXMBR
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
     C                   KFLD                    EOSS#
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
     C*
     C     BSKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     EEKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     DEKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DPKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C*
     C     Z1KY          KLIST
     C                   KFLD                    KKTRID
     C*
     C     DDKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
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
     C*
     C*
     C*
     C                   Z-ADD     0             XXRCD
     C*
     C                   READ      PEA01                                  98
     C*** BEG ADD 02/07/12 ****************************************************
     C                   EVAL      X1RCN  = EARCN
     C*** END ADD 02/07/12 ****************************************************
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C     EOKY          CHAIN     PEO01                              97
     C     *IN97         IFEQ      *OFF
     C     EOBLST        ANDEQ     'C'
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
     C*** BEG ADD 02/07/12 ****************************************************
     C                   EVAL      X1RCN  = EARCN
     C*** END ADD 02/07/12 ****************************************************
     C*
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C                   EXSR      WRTZ9P
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* SRDIAG - LOAD DIAGNOSIS CODES
     C*================================================================
     C     SRDIAG        BEGSR
     C*
     C                   SELECT
     C*
     C                   WHEN      EOBTYP = 'N'
     C*
     C                   EVAL      XXPOS = 0
     C                   MOVEL     *BLANKS       XXDXR
     C                   EVAL      XXPOS = %SCAN('X':DHDXR1)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '1'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   EVAL      XXPOS = %SCAN('X':DHDXR2)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '2'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   EVAL      XXPOS = %SCAN('X':DHDXR3)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '3'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   EVAL      XXPOS = %SCAN('X':DHDXR4)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '4'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   MOVE      XXDXR         U2DXR                           ICD9 X-ref
     C                   WHEN      EOBTYP = 'D'
     C*
     C                   EVAL      XXPOS = 0
     C                   MOVEL     *BLANKS       XXDXR
     C                   EVAL      XXPOS = %SCAN('X':DDDXR1)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '1'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   EVAL      XXPOS = %SCAN('X':DDDXR2)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '2'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   EVAL      XXPOS = %SCAN('X':DDDXR3)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '3'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   EVAL      XXPOS = %SCAN('X':DDDXR4)
     C                   IF        XXPOS > 0
     C                   EVAL      XXDXR = %TRIM(XXDXR) + '4'
     C                   EVAL      XXPOS = 0
     C                   ENDIF
     C                   MOVE      XXDXR         U2DXR
     C*
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SRMED - LOAD FIELDS FOR PHYSICIAN BILLS
     C*================================================================
     C     SRMED         BEGSR
     C*
     C     EHKY          CHAIN     PEH01                              96
     C*** BEG ADD 02/07/12 *****************************************************
     C* Header Line
     C
     C                   MOVEL(P)  EHDIA1        U1ICD1
     C                   MOVEL(P)  EHDIA2        U1ICD2
     C                   MOVEL(P)  EHDIA3        U1ICD3
     C                   MOVEL(P)  EHDIA4        U1ICD4
     C                   MOVEL(P)  'N'           U1BTYP
     C                   MOVEL(P)  EHACCT        U1ACCT
     C                   MOVEL(P)  EHFLIC        U1BLIC
     C                   MOVEL(P)  U1ICD1        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC1D
     C                   MOVEL(P)  U1ICD2        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC2D
     C                   MOVEL(P)  U1ICD3        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC3D
     C                   MOVEL(P)  U1ICD4        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC4D
     C*
     C* Write Header record
     C*
     C                   EXSR      WRTZ1P
     C*
     C* Detail Line
     C*** END ADD 02/07/12 *****************************************************
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEG ADD 02/07/12 *****************************************************
     C                   CLEAR                   Z2P01
     C                   IF        CARCN > X1RCN
     C                   ELSE
     C*** END ADD 02/07/12 *****************************************************
     C*
     C                   EVAL      U2TYPE = '2'                                 Record Type
     C                   MOVEL     U1TRID        U2TRID                         Transaction ID
     C                   MOVE      CALIN#        U2LIN#                         Line Seq#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U2SVDT                         Date of Service
     C     DHKY          CHAIN     PDH01                              61
     C  N61              MOVEL(P)  DHPRCD        U2CODE                         Paid Code
     C                   MOVEL     DHPRDS        U2CDSC                         Code Description
     C                   MOVE      *BLANKS       U2RVCD                         Revenue Code
     C                   MOVEL(P)  DHMOD1        U2MOD                          Modifier
     C                   MOVEL(P)  DHMOD2        U2MOD2                         Modifier
     C                   MOVEL(P)  DHMOD3        U2MOD3                         Modifier
     C                   MOVEL(P)  DHMOD4        U2MOD4                         Modifier
     C
     C                   EXSR      SRDIAG
     C
     C                   Z-ADD     CAUNTS        U2UNTS                         Units
     C*
     C     DDKY          CHAIN     PDA01                              60
     C                   IF        *IN60 = *OFF
     C*** BEG ADD 02/07/12 *****************************************************
     C*
     C                   IF        CARCN <= X1RCN
     C*
     C                   MOVE (P)  DACHRG        U2CHRG                         Charged
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DACHRG < 0
     C                   EVAL      DACHRG = DACHRG * -1
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   MOVEL     '-'           U2CHRG
     C                   ELSE
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAMRA         U2MRA                          MRA Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAMRA  < 0
     C                   EVAL      DAMRA = DAMRA * -1
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   MOVEL     '-'           U2MRA
     C                   ELSE
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DANDIS        U2NDIS                         Network Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DANDIS < 0
     C                   EVAL      DANDIS = DANDIS * -1
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   MOVEL     '-'           U2NDIS
     C                   ELSE
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTHR        U2OTHR                         Other reduction 1
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTHR < 0
     C                   EVAL      DAOTHR = DAOTHR * -1
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   MOVEL     '-'           U2OTHR
     C                   ELSE
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTH2        U2OTH2                         Other reduction 2
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTH2 < 0
     C                   EVAL      DAOTH2 = DAOTH2 * -1
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   MOVEL     '-'           U2OTH2
     C                   ELSE
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAPAID        U2PAID                         Paid amount
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAPAID < 0
     C                   EVAL      DAPAID = DAPAID * -1
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   MOVEL     '-'           U2PAID
     C                   ELSE
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C*
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
     C*** BEG DLT 02/07/12 *****************************************************
     C***                MOVE      DACHRG        U2CHRG                         Charged
     C***                MOVE      DAMRA         U2MRA                          MRA Reductions
     C***                MOVE      DANDIS        U2NDIS                         Network Reductions
     C***                MOVEL(P)  DAOTHR        U2OTHR                         Other reduction 1
     C***                MOVEL(P)  DAOTH2        U2OTH2                         Other reduction 2
     C***
     C***                IF        DAPAID > 0                                   Paid amount
     C***                MOVE      DAPAID        U2PAID
     C***                ELSE
     C*** **             MOVEL     DAPAID        U2PAID
     C***                ENDIF
     C*** END DLT 02/07/12 *****************************************************
     C                   ENDIF
     C*
     C                   Z-Add     0             XXCOUNT
     C     DEKY          CHAIN     PDE01                              95
     C                   DOW       NOT %EOF(FDDEL)
     C                             AND DEPRNT <> 'N'
     C                   EVAL      XXCOUNT = XXCOUNT + 1
     C*
     C                   SELECT
     C                   WHEN      XXCOUNT = 1
     C                   MOVEL(P)  DEEOB         U2EOB1
     C                   EVAL      U2EOD1 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 2
     C                   MOVEL(P)  DEEOB         U2EOB2
     C                   EVAL      U2EOD2 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 3
     C                   MOVEL(P)  DEEOB         U2EOB3
     C                   EVAL      U2EOD3 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 4
     C                   MOVEL(P)  DEEOB         U2EOB4
     C                   EVAL      U2EOD4 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C                   ENDSL
     C     DEKY          READE     PDE01                                  95
     C                   ENDDO
     C                   MOVE(P)   DHPOS         U2POS                          Place of Service
     C                   MOVE      *BLANKS       U2NDC                          NDC drug Paid code
     C                   MOVE      *BLANKS       U2RX#                          Prescription Number
     C                   MOVE      *BLANKS       U2EOB                          Service Adjustment
     C                   MOVE      *BLANKS       U2EOBD                         Description
     C                   MOVE      *ZEROS        U2RLIN                         Recon Line Number
     C                   MOVE      *BLANKS       U2FIL1                         Filler1
     C                   MOVE      *BLANKS       U2FIL2                         Filler2
     C                   MOVE      *BLANKS       U2TH#                          Tooth Number
     C                   MOVE      *BLANKS       U2NWRF                         Prescription: N/R
     C                   MOVE      *BLANKS       U2CERT                         Certification
     C                   MOVE      *BLANKS       U2LIC#                         Prescribers Lince#
     C                   MOVE      *BLANKS       U2NDCD                         Drug Name
     C                   MOVE      *BLANKS       U2RXDY                         Drugs/Supplies
     C                   MOVE      *BLANKS       U2DFEE                         D/S Dispensing Fee
     C                   MOVE      *BLANKS       U2RXDI                         Dispense as written
     C                   MOVE      *BLANKS       U2BCDT                         Basis of Determinat
     C                   MOVE      *BLANKS       U2BNDC                         NDC Billed Code
     C                   MOVE      *BLANKS       U2BLFC                         Billing Frequency
     C                   MOVE      *BLANKS       U2DUCD                         Days/Units Code
     C                   MOVE      *BLANKS       U2BPRC                         Billed Code
     C                   MOVE      *BLANKS       U2BMD1                         Billed Modifier
     C                   MOVE      *BLANKS       U2RXPT                         DRG/EQU/SUP Indica
     C                   MOVE      *BLANKS       U2PRIN                         Pur/Ren Indicator
     C                   MOVE      *BLANKS       U2RNPR                         Rendering Pro Grp N
     C                   MOVE      *BLANKS       U2RNPI                         Rendering Pro NPI#
     C                   MOVE      *BLANKS       U2FILL                         Future Expansion
     C                   MOVE      *BLANKS       U2SAG1                         
     C                   MOVE      *BLANKS       U2SAG2                         
     C                   MOVE      *BLANKS       U2SAG3                         
     C                   MOVE      *BLANKS       U2SAG4                         
     C*** BEG DLT 02/07/12 *****************************************************
     C*** Header data
     C***
     C***                MOVEL(P)  EHDIA1        U1ICD1
     C***                MOVEL(P)  EHDIA2        U1ICD2
     C***                MOVEL(P)  EHDIA3        U1ICD3
     C***                MOVEL(P)  EHDIA4        U1ICD4
     C***                MOVEL(P)  'N'           U1BTYP
     C***                MOVEL(P)  EHACCT        U1ACCT
     C***                MOVEL(P)  EHFLIC        U1BLIC
     C***                MOVEL(P)  U1ICD1        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC1D
     C***                MOVEL(P)  U1ICD2        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC2D
     C***                MOVEL(P)  U1ICD3        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC3D
     C***                MOVEL(P)  U1ICD4        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC4D
     C***
     C***                EXSR      WRTZ1P
     C*** END DLT 02/07/12 *****************************************************
     C                   EXSR      WRTZ2P
     C*** BEG ADD 02/07/12 *****************************************************
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
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
     C*** BEG ADD 02/07/12 *****************************************************
     C* Header Line
     C*
     C                   MOVEL(P)  EUDIA1        U1ICD1
     C                   MOVEL(P)  EUDIA2        U1ICD2
     C                   MOVEL(P)  EUDIA3        U1ICD3
     C                   MOVEL(P)  EUDIA4        U1ICD4
     C                   MOVEL(P)  'H'           U1BTYP
     C                   MOVEL(P)  EUBLTY        U1BLTY
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U1ADDT
     C                   Z-ADD     EUSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U1CFDT
     C                   Z-ADD     EUSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U1CTDT
     C                   MOVEL(P)  EUICN         U1SVTY
     C                   MOVEL(P)  EUACCT        U1ACCT
     C                   MOVEL(P)  EUFLIC        U1BLIC
     C                   MOVE (P)  EUDRG#        U1DRG#
     C                   MOVEL(P)  EUDGDS        U1DGDS
     C*
     C                   MOVEL(P)  U1ICD1        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC1D
     C                   MOVEL(P)  U1ICD2        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC2D
     C                   MOVEL(P)  U1ICD3        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC3D
     C                   MOVEL(P)  U1ICD4        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC4D
     C*
     C* Write Header record
     C*
     C                   EXSR      WRTZ1P
     C*
     C* Detail Line
     C*** END ADD 02/07/12 *****************************************************
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEG ADD 02/07/12 *****************************************************
     C                   IF        CARCN > X1RCN
     C                   ELSE
     C*** END ADD 02/07/12 *****************************************************
     C                   EVAL      U2TYPE = '2'                                 Record Type
     C                   MOVEL     U1TRID        U2TRID                         Transaction ID
     C                   MOVE      CALIN#        U2LIN#                         LINE SEQ#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U2SVDT                         Date of Service
     C*
     C     DUKY          CHAIN     PDU01                              61
     C  N61              MOVEL(P)  DURATE        U2CODE                         Paid Code
     C                   MOVEL     DURVCD        U2RVCD                         Revenue Code
     C                   MOVEL     DUDESC        U2CDSC                         Code Description
     C*
     C                   MOVEL(P)  DUPMOD        U2MOD                          Modifier
     C                   MOVEL(P)  DUMOD2        U2MOD2                         Modifier
     C                   MOVEL(P)  DUMOD3        U2MOD3                         Modifier
     C                   MOVEL(P)  DUMOD4        U2MOD4                         Modifier
     C*
     C*
     C                   Z-ADD     CAUNTS        U2UNTS                         Units
     C*
     C     DDKY          CHAIN     PDA01                              60
     C                   IF        *IN60 = *OFF
     C*** BEG ADD 02/07/12 *****************************************************
     C*
     C                   IF        CARCN <= X1RCN
     C*
     C                   MOVE (P)  DACHRG        U2CHRG                         Charged
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DACHRG < 0
     C                   EVAL      DACHRG = DACHRG * -1
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   MOVEL     '-'           U2CHRG
     C                   ELSE
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAMRA         U2MRA                          MRA Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAMRA  < 0
     C                   EVAL      DAMRA = DAMRA * -1
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   MOVEL     '-'           U2MRA
     C                   ELSE
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DANDIS        U2NDIS                         Network Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DANDIS < 0
     C                   EVAL      DANDIS = DANDIS * -1
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   MOVEL     '-'           U2NDIS
     C                   ELSE
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTHR        U2OTHR                         Other reduction 1
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTHR < 0
     C                   EVAL      DAOTHR = DAOTHR * -1
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   MOVEL     '-'           U2OTHR
     C                   ELSE
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTH2        U2OTH2                         Other reduction 2
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTH2 < 0
     C                   EVAL      DAOTH2 = DAOTH2 * -1
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   MOVEL     '-'           U2OTH2
     C                   ELSE
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAPAID        U2PAID                         Paid amount
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAPAID < 0
     C                   EVAL      DAPAID = DAPAID * -1
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   MOVEL     '-'           U2PAID
     C                   ELSE
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C*
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
     C*** BEG DLT 02/07/12 *****************************************************
     C***                MOVE      DACHRG        U2CHRG                         Charged
     C***                MOVE      DAMRA         U2MRA                          MRA Reductions
     C***                MOVE      DANDIS        U2NDIS                         Network Reductions
     C***                MOVEL(P)  DAOTHR        U2OTHR                         Other reduction 1
     C***                MOVEL(P)  DAOTH2        U2OTH2                         Other reduction 2
     C***
     C***                IF        DAPAID > 0                                   Paid amount
     C***                MOVE      DAPAID        U2PAID
     C***                ELSE
     C***                MOVEL     DAPAID        U2PAID
     C***                ENDIF
     C*** BEG DLT 02/07/12 *****************************************************
     C                   ENDIF
     C*
     C                   MOVEL     *ZEROS        U2DXR                          ICD9 X-ref
     C*
     C                   Z-Add     0             XXCOUNT
     C     DEKY          CHAIN     PDE01                              95
     C                   DOW       NOT %EOF(FDDEL)
     C                             AND DEPRNT <> 'N'
     C                   EVAL      XXCOUNT = XXCOUNT + 1
     C*
     C                   SELECT
     C                   WHEN      XXCOUNT = 1
     C                   MOVEL(P)  DEEOB         U2EOB1
     C                   EVAL      U2EOD1 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 2
     C                   MOVEL(P)  DEEOB         U2EOB2
     C                   EVAL      U2EOD2 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 3
     C                   MOVEL(P)  DEEOB         U2EOB3
     C                   EVAL      U2EOD3 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 4
     C                   MOVEL(P)  DEEOB         U2EOB4
     C                   EVAL      U2EOD4 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C                   ENDSL
     C     DEKY          READE     PDE01                                  95
     C                   ENDDO
     C                   MOVE(P)   DUPOS         U2POS                          Place of Service
     C                   MOVE      *ZEROS        U2DXR                          ICD9 X-ref
     C                   MOVE      *BLANKS       U2NDC                          NDC drug Paid code
     C                   MOVE      *BLANKS       U2RX#                          Prescription Number
     C                   MOVE      *BLANKS       U2EOB                          Service Adjustment
     C                   MOVE      *BLANKS       U2EOBD                         Description
     C                   MOVE      *ZEROS        U2RLIN                         Recon Line Number
     C                   MOVE      *BLANKS       U2FIL1                         Filler1
     C                   MOVE      *BLANKS       U2FIL2                         Filler2
     C                   MOVE      *BLANKS       U2TH#                          Tooth Number
     C                   MOVE      *BLANKS       U2NWRF                         Prescription: N/R
     C                   MOVE      *BLANKS       U2CERT                         Certification
     C                   MOVE      *BLANKS       U2LIC#                         Prescribers Lince#
     C                   MOVE      *BLANKS       U2NDCD                         Drug Name
     C                   MOVE      *BLANKS       U2RXDY                         Drugs/Supplies
     C                   MOVE      *BLANKS       U2DFEE                         D/S Dispensing Fee
     C                   MOVE      *BLANKS       U2RXDI                         Dispense as written
     C                   MOVE      *BLANKS       U2BCDT                         Basis of Determinat
     C                   MOVE      *BLANKS       U2BNDC                         NDC Billed Code
     C                   MOVE      *BLANKS       U2BLFC                         Billing Frequency
     C                   MOVE      *BLANKS       U2DUCD                         Days/Units Code
     C                   MOVE      *BLANKS       U2BPRC                         Billed Code
     C                   MOVE      *BLANKS       U2BMD1                         Billed Modifier
     C                   MOVE      *BLANKS       U2RXPT                         DRG/EQU/SUP Indica
     C                   MOVE      *BLANKS       U2PRIN                         Pur/Ren Indicator
     C                   MOVE      *BLANKS       U2RNPR                         Rendering Pro Grp N
     C                   MOVE      *BLANKS       U2RNPI                         Rendering Pro NPI#
     C                   MOVE      *BLANKS       U2FILL                         Future Expansion
     C                   MOVE      *BLANKS       U2SAG1                         
     C                   MOVE      *BLANKS       U2SAG2                         
     C                   MOVE      *BLANKS       U2SAG3                         
     C                   MOVE      *BLANKS       U2SAG4                         
     C*** BEG DLT 02/07/12 *****************************************************
     C*** Header data
     C***                MOVEL(P)  EUDIA1        U1ICD1
     C***                MOVEL(P)  EUDIA2        U1ICD2
     C***                MOVEL(P)  EUDIA3        U1ICD3
     C***                MOVEL(P)  EUDIA4        U1ICD4
     C***                MOVEL(P)  'H'           U1BTYP
     C***                MOVEL(P)  EUBLTY        U1BLTY
     C***                Z-ADD     EUADDT        SRDTI
     C***                CALL      'SRCVTDAT'    DTYYMD1
     C***                MOVEL     SRDTO         U1ADDT
     C***                Z-ADD     EUSCPF        SRDTI
     C***                CALL      'SRCVTDAT'    DTYYMD1
     C***                MOVEL     SRDTO         U1CFDT
     C***                Z-ADD     EUSCPT        SRDTI
     C***                CALL      'SRCVTDAT'    DTYYMD1
     C***                MOVEL     SRDTO         U1CTDT
     C***                MOVEL(P)  EUICN         U1SVTY
     C***                MOVEL(P)  EUACCT        U1ACCT
     C***                MOVEL(P)  EUFLIC        U1BLIC
     C***                MOVE (P)  EUDRG#        U1DRG#
     C***                MOVEL(P)  EUDGDS        U1DGDS
     C***
     C***                MOVEL(P)  U1ICD1        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC1D
     C***                MOVEL(P)  U1ICD2        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC2D
     C***                MOVEL(P)  U1ICD3        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC3D
     C***                MOVEL(P)  U1ICD4        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC4D
     C***
     C***
     C***
     C***                EXSR      WRTZ1P
     C*** END DLT 02/07/12 ****************************************************
     C                   EXSR      WRTZ2P
     C*
     C*** BEG ADD 02/07/12 ****************************************************
     C                   ENDIF
     C*** END ADD 02/07/12 ****************************************************
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
     C*** BEG ADD 02/07/12 ****************************************************
     C* Header Line
     C*
     C                   MOVEL(P)  'P'           U1BTYP
     C                   MOVEL(P)  EPACCT        U1ACCT
     C                   MOVEL(P)  EPFLIC        U1BLIC
     C*
     C                   MOVEL(P)  *BLANKS       U1IC1D
     C                   MOVEL(P)  *BLANKS       U1IC2D
     C                   MOVEL(P)  *BLANKS       U1IC3D
     C                   MOVEL(P)  *BLANKS       U1IC4D
     C* Write header record
     C                   EXSR      WRTZ1P
     C*
     C*** END ADD 02/07/12 ****************************************************
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEG ADD 02/07/12 ****************************************************
     C                   IF        CARCN > X1RCN
     C                   ELSE
     C*
     C                   EVAL      U2TYPE = '2'                                 Record Type
     C                   MOVEL     U1TRID        U2TRID                         Transaction ID
     C                   MOVE      CALIN#        U2LIN#                         Line Seq#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U2SVDT                         Date of Service
     C*
     C                   MOVE(P)   *BLANKS       U2CODE                         Paid Code
     C                   MOVE      *BLANKS       U2RVCD                         Revenue Code
     C                   MOVEL     *BLANKS       U2CDSC                         Code Description
     C*
     C                   MOVE      *BLANKS       U2MOD                          Modifier
     C                   MOVE      *BLANKS       U2MOD2                         Modifier
     C                   MOVE      *BLANKS       U2MOD3                         Modifier
     C                   MOVE      *BLANKS       U2MOD4                         Modifier
     C                   Z-ADD     CAUNTS        U2UNTS                         Units
     C*
     C     DDKY          CHAIN     PDA01                              60
     C                   IF        *IN60 = *OFF
     C*** BEG ADD 02/07/12 *****************************************************
     C*
     C                   IF        CARCN <= X1RCN
     C*
     C                   MOVE (P)  DACHRG        U2CHRG                         Charged
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DACHRG < 0
     C                   EVAL      DACHRG = DACHRG * -1
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   MOVEL     '-'           U2CHRG
     C                   ELSE
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAMRA         U2MRA                          MRA Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAMRA  < 0
     C                   EVAL      DAMRA = DAMRA * -1
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   MOVEL     '-'           U2MRA
     C                   ELSE
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DANDIS        U2NDIS                         Network Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DANDIS < 0
     C                   EVAL      DANDIS = DANDIS * -1
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   MOVEL     '-'           U2NDIS
     C                   ELSE
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTHR        U2OTHR                         Other reduction 1
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTHR < 0
     C                   EVAL      DAOTHR = DAOTHR * -1                         Other reduction 1
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   MOVEL     '-'           U2OTHR
     C                   ELSE
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTH2        U2OTH2                         Other reduction 2
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTH2 < 0
     C                   EVAL      DAOTH2 = DAOTH2 * -1
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   MOVEL     '-'           U2OTH2
     C                   ELSE
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAPAID        U2PAID                         Paid amount
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAPAID < 0
     C                   EVAL      DAPAID = DAPAID * -1
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   MOVEL     '-'           U2PAID
     C                   ELSE
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   ENDIF
     C                   MOVEL     *ZEROS        U2DXR                          ICD9 X-ref
     C*** End Add *** 03/19/12 ************************************************
     C*
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
     C*** BEG DLT 02/07/12 *****************************************************
     C***                MOVE      DACHRG        U2CHRG                         Charged
     C***                MOVE      DAMRA         U2MRA                          MRA Reductions
     C***                MOVE      DANDIS        U2NDIS                         Network Reductions
     C***                MOVEL(P)  DAOTHR        U2OTHR                         Other reduction 1
     C***                MOVEL(P)  DAOTH2        U2OTH2                         Other reduction 2
     C***
     C***                IF        DAPAID > 0
     C***                MOVE      DAPAID        U2PAID
     C***                ELSE
     C***                MOVEL     DAPAID        U2PAID
     C***                ENDIF
     C*** END DLT 02/07/12 *****************************************************
     C                   ENDIF
     C*
     C                   Z-Add     0             XXCOUNT
     C     DEKY          CHAIN     PDE01                              95
     C                   DOW       NOT %EOF(FDDEL)
     C                             AND DEPRNT <> 'N'
     C                   EVAL      XXCOUNT = XXCOUNT + 1
     C*
     C                   SELECT
     C                   WHEN      XXCOUNT = 1
     C                   MOVEL(P)  DEEOB         U2EOB1
     C                   EVAL      U2EOD1 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 2
     C                   MOVEL(P)  DEEOB         U2EOB2
     C                   EVAL      U2EOD2 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 3
     C                   MOVEL(P)  DEEOB         U2EOB3
     C                   EVAL      U2EOD3 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 4
     C                   MOVEL(P)  DEEOB         U2EOB4
     C                   EVAL      U2EOD4 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C                   ENDSL
     C     DEKY          READE     PDE01                                  95
     C                   ENDDO
     C*
     C     DPKY          CHAIN     PDP01                              95
     C                   IF        *IN95 = *OFF
     C                   MOVEL(P)  DPNDC         U2NDC
     C                   ENDIF
     C                   MOVE(P)   *BLANKS       U2POS                          Place of Service
     C                   MOVE      *BLANKS       U2RX#                          Prescription Number
     C                   MOVE      *BLANKS       U2EOB                          Service Adjustment
     C                   MOVE      *BLANKS       U2EOBD                         Description
     C                   MOVE      *ZEROS        U2RLIN                         Recon Line Number
     C                   MOVE      *BLANKS       U2FIL1                         Filler1
     C                   MOVE      *BLANKS       U2FIL2                         Filler2
     C                   MOVE      *BLANKS       U2TH#                          Tooth Number
     C                   MOVE      *BLANKS       U2NWRF                         Prescription: N/R
     C                   MOVE      *BLANKS       U2CERT                         Certification
     C                   MOVE      *BLANKS       U2LIC#                         Prescribers Lince#
     C                   MOVE      *BLANKS       U2RXDY                         Drugs/Supplies
     C                   MOVE      *BLANKS       U2DFEE                         D/S Dispensing Fee
     C                   MOVE      *BLANKS       U2RXDI                         Dispense as written
     C                   MOVE      *BLANKS       U2BCDT                         Basis of Determinat
     C                   MOVE      *BLANKS       U2BNDC                         NDC Billed Code
     C                   MOVE      *BLANKS       U2BLFC                         Billing Frequency
     C                   MOVE      *BLANKS       U2DUCD                         Days/Units Code
     C                   MOVE      *BLANKS       U2BPRC                         Billed Code
     C                   MOVE      *BLANKS       U2BMD1                         Billed Modifier
     C                   MOVE      *BLANKS       U2RXPT                         DRG/EQU/SUP Indica
     C                   MOVE      *BLANKS       U2PRIN                         Pur/Ren Indicator
     C                   MOVE      *BLANKS       U2RNPR                         Rendering Pro Grp N
     C                   MOVE      *BLANKS       U2RNPI                         Rendering Pro NPI#
     C                   MOVE      *BLANKS       U2FILL                         Future Expansion
     C                   MOVE      *BLANKS       U2SAG1                         
     C                   MOVE      *BLANKS       U2SAG2                         
     C                   MOVE      *BLANKS       U2SAG3                         
     C                   MOVE      *BLANKS       U2SAG4                         
     C*** BEG DLT 02/07/12 *****************************************************
     C* Header data
     C***                MOVEL(P)  'P'           U1BTYP
     C***                MOVEL(P)  EPACCT        U1ACCT
     C***                MOVEL(P)  EPFLIC        U1BLIC
     C***
     C***                MOVEL(P)  *BLANKS       U1IC1D
     C***                MOVEL(P)  *BLANKS       U1IC2D
     C***                MOVEL(P)  *BLANKS       U1IC3D
     C***                MOVEL(P)  *BLANKS       U1IC4D
     C***
     C***                EXSR      WRTZ1P
     C*** END DLT 02/07/12 *****************************************************
     C                   EXSR      WRTZ2P
     C*** BEG ADD 02/07/12 *****************************************************
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
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
     C*** BEG ADD 02/07/12 ****************************************************
     C* Header Line
     C*
     C                   MOVEL(P)  EDDIA1        U1ICD1
     C                   MOVEL(P)  EDDIA2        U1ICD2
     C                   MOVEL(P)  EDDIA3        U1ICD3
     C                   MOVEL(P)  EDDIA4        U1ICD4
     C                   MOVEL(P)  'D'           U1BTYP
     C                   MOVEL(P)  EDACCT        U1ACCT
     C                   MOVEL(P)  EDFLIC        U1BLIC
     C*
     C                   MOVEL(P)  U1ICD1        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC1D
     C                   MOVEL(P)  U1ICD2        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC2D
     C                   MOVEL(P)  U1ICD3        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC3D
     C                   MOVEL(P)  U1ICD4        KKICD9
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL(P)  DIDSC1        U1IC4D
     C*
     C* Write Header Record
     C*
     C                   EXSR      WRTZ1P
     C*
     C* Detail Line
     C*
     C*** END ADD 02/07/12 ****************************************************
     C     EHKY          CHAIN     PED01                              96
     C*
     C*
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEG ADD 02/07/12 *****************************************************
     C                   IF        CARCN > X1RCN
     C                   ELSE
     C*** END ADD 02/07/12 *****************************************************
     C*
     C                   EVAL      U2TYPE = '2'                                 Record Type
     C                   MOVEL     U1TRID        U2TRID                         Transaction ID
     C                   MOVE      CALIN#        U2LIN#                         Line Seq#
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U2SVDT                         Date of Service
     C*
     C     DHKY          CHAIN     PDD01                              61
     C  N61              MOVEL(P)  DDPRCD        U2CODE                         Paid Code
     C                   MOVE      *BLANKS       U2RVCD                         Revenue Code
     C                   MOVEL     DDPRDS        U2CDSC                         Code Description
     C*
     C                   MOVE      *BLANKS       U2MOD                          Modifier
     C                   MOVE      *BLANKS       U2MOD2                         Modifier
     C                   MOVE      *BLANKS       U2MOD3                         Modifier
     C                   MOVE      *BLANKS       U2MOD4                         Modifier
     C*
     C                   EXSR      SRDIAG
     C*
     C                   Z-ADD     CAUNTS        U2UNTS                         Units
     C*
     C*
     C     DDKY          CHAIN     PDA01                              60
     C                   IF        *IN60 = *OFF
     C*** BEG ADD 02/07/12 *****************************************************
     C*
     C                   IF        CARCN <= X1RCN
     C*
     C                   MOVE (P)  DACHRG        U2CHRG                         Charged
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DACHRG < 0
     C                   EVAL      DACHRG = DACHRG * -1
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   MOVEL     '-'           U2CHRG
     C                   ELSE
     C                   EVAL      U2CHRG =  %editc(DACHRG:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAMRA         U2MRA                          MRA Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAMRA  < 0
     C                   EVAL      DAMRA  = DAMRA * -1
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   MOVEL     '-'           U2MRA
     C                   ELSE
     C                   EVAL      U2MRA  =  %editc(DAMRA :'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DANDIS        U2NDIS                         Network Reductions
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DANDIS < 0
     C                   EVAL      DANDIS = DANDIS * -1
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   MOVEL     '-'           U2NDIS
     C                   ELSE
     C                   EVAL      U2NDIS =  %editc(DANDIS:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTHR        U2OTHR                         Other reduction 1
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTHR < 0
     C                   EVAL      DAOTHR = DAOTHR * -1
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   MOVEL     '-'           U2OTHR
     C                   ELSE
     C                   EVAL      U2OTHR =  %editc(DAOTHR:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAOTH2        U2OTH2                         Other reduction 2
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAOTH2 < 0
     C                   EVAL      DAOTH2 = DAOTH2 * -1
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   MOVEL     '-'           U2OTH2
     C                   ELSE
     C                   EVAL      U2OTH2 =  %editc(DAOTH2:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  DAPAID        U2PAID                         Paid amount
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        DAPAID < 0
     C                   EVAL      DAPAID = DAPAID * -1
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   MOVEL     '-'           U2PAID
     C                   ELSE
     C                   EVAL      U2PAID =  %editc(DAPAID:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C*
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
     C*** BEG DLT 02/07/12 *****************************************************
     C***                MOVE      DACHRG        U2CHRG                         Charged
     C***                MOVE      DAMRA         U2MRA                          MRA Reductions
     C***                MOVE      DANDIS        U2NDIS                         Network Reductions
     C***                MOVEL(P)  DAOTHR        U2OTHR                         Other reduction 1
     C***                MOVEL(P)  DAOTH2        U2OTH2                         Other reduction 2
     C***
     C***                IF        DAPAID > 0
     C***                MOVE      DAPAID        U2PAID
     C***                ELSE
     C***                MOVEL     DAPAID        U2PAID
     C***                ENDIF
     C*** END DLT 02/07/12 *****************************************************
     C                   ENDIF
     C*
     C                   Z-Add     0             XXCOUNT
     C     DEKY          CHAIN     PDE01                              95
     C                   DOW       NOT %EOF(FDDEL)
     C                             AND DEPRNT <> 'N'
     C                   EVAL      XXCOUNT = XXCOUNT + 1
     C*
     C                   SELECT
     C                   WHEN      XXCOUNT = 1
     C                   MOVEL(P)  DEEOB         U2EOB1
     C                   EVAL      U2EOD1 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 2
     C                   MOVEL(P)  DEEOB         U2EOB2
     C                   EVAL      U2EOD2 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 3
     C                   MOVEL(P)  DEEOB         U2EOB3
     C                   EVAL      U2EOD3 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C*
     C                   WHEN      XXCOUNT = 4
     C                   MOVEL(P)  DEEOB         U2EOB4
     C                   EVAL      U2EOD4 = %TRIM(DENOT1) + ' ' + %TRIM(DENOT2)
     C                   ENDSL
     C     DEKY          READE     PDE01                                  95
     C                   ENDDO
     C                   MOVE(P)   DDPOS         U2POS                          Place of Service
     C                   MOVE      *BLANKS       U2NDC                          NDC drug Paid code
     C                   MOVE      *BLANKS       U2RX#                          Prescription Number
     C                   MOVE      *BLANKS       U2EOB                          Service Adjustment
     C                   MOVE      *BLANKS       U2EOBD                         Description
     C                   MOVE      *ZEROS        U2RLIN                         Recon Line Number
     C                   MOVE      *BLANKS       U2FIL1                         Filler1
     C                   MOVE      *BLANKS       U2FIL2                         Filler2
     C                   MOVE      *BLANKS       U2TH#                          Tooth Number
     C                   MOVE      *BLANKS       U2NWRF                         Prescription: N/R
     C                   MOVE      *BLANKS       U2CERT                         Certification
     C                   MOVE      *BLANKS       U2LIC#                         Prescribers Lince#
     C                   MOVE      *BLANKS       U2NDCD                         Drug Name
     C                   MOVE      *BLANKS       U2RXDY                         Drugs/Supplies
     C                   MOVE      *BLANKS       U2DFEE                         D/S Dispensing Fee
     C                   MOVE      *BLANKS       U2RXDI                         Dispense as written
     C                   MOVE      *BLANKS       U2BCDT                         Basis of Determinat
     C                   MOVE      *BLANKS       U2BNDC                         NDC Billed Code
     C                   MOVE      *BLANKS       U2BLFC                         Billing Frequency
     C                   MOVE      *BLANKS       U2DUCD                         Days/Units Code
     C                   MOVE      *BLANKS       U2BPRC                         Billed Code
     C                   MOVE      *BLANKS       U2BMD1                         Billed Modifier
     C                   MOVE      *BLANKS       U2RXPT                         DRG/EQU/SUP Indica
     C                   MOVE      *BLANKS       U2PRIN                         Pur/Ren Indicator
     C                   MOVE      *BLANKS       U2RNPR                         Rendering Pro Grp N
     C                   MOVE      *BLANKS       U2RNPI                         Rendering Pro NPI#
     C                   MOVE      *BLANKS       U2FILL                         Future Expansion
     C                   MOVE      *BLANKS       U2RVCD                         Revenue Code
     C                   MOVE      *BLANKS       U2SAG1                         
     C                   MOVE      *BLANKS       U2SAG2                         
     C                   MOVE      *BLANKS       U2SAG3                         
     C                   MOVE      *BLANKS       U2SAG4                         
     C*** BEG DLT 02/07/12 *****************************************************
     C* Header data
     C***                MOVEL(P)  EDDIA1        U1ICD1
     C****               MOVEL(P)  EDDIA2        U1ICD2
     C***                MOVEL(P)  EDDIA3        U1ICD3
     C***                MOVEL(P)  EDDIA4        U1ICD4
     C***                MOVEL(P)  'D'           U1BTYP
     C***                MOVEL(P)  EDACCT        U1ACCT
     C***                MOVEL(P)  EDFLIC        U1BLIC
     C***
     C***                 MOVEL(P)  U1ICD1        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC1D
     C***                 MOVEL(P)  U1ICD2        KKICD9
     C***   DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC2D
     C***                MOVEL(P)  U1ICD3        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC3D
     C***                MOVEL(P)  U1ICD4        KKICD9
     C***  DIKY          CHAIN     PDI01                              60
     C*** N60              MOVEL(P)  DIDSC1        U1IC4D
     C***
     C***
     C***
     C***
     C* Header record
     C****               EXSR      WRTZ1P
     C*** END DLT 02/07/12 *****************************************************
     C* Detail record
     C                   EXSR      WRTZ2P
     C*** BEG ADD 02/07/12 *****************************************************
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
     C*
     C     CAKY          READE     PCA01                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* WRTZ1P - WRITE RECORDS IN FOZ0P AND FOZ1P
     C*================================================================
     C     WRTZ1P        BEGSR
     C*
     C                   CLEAR                   Z0P01
     C                   MOVEL     U1TRID        KKTRID
     C     Z1KY          CHAIN     Z1P02                              96
     C                   IF        *IN96 = *ON
     C                   EVAL      XXRCD = XXRCD + 1
     C*** BEG ADD 08/21/13 *****************************************************
     C                   EVAL      U1FEIN = EAFEIN
     C*** END ADD 08/21/13 *****************************************************
     C                   WRITE     Z1P01
     C                   MOVEL     Z1DS          Z0DATA
     C                   MOVEL     'Y'           Z0PRFL
     C                   WRITE     Z0P01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTZ2P - WRITE RECORDS IN FOZ0P AND FOZ2P
     C*================================================================
     C     WRTZ2P        BEGSR
     C*
     C                   CLEAR                   Z0P01
     C                   WRITE     Z2P01
     C                   MOVEL     Z2DS          Z0DATA
     C                   MOVEL     'Y'           Z0PRFL
     C                   WRITE     Z0P01
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
     C                   EVAL      EANSNT = 'E'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EANPDT
     C                   UPDATE    PEA01
     C*
     C                   ENDSR
     C*================================================================
     C* WRTZ9P - WRITE DATA TO EXTRACT FILE
     C*================================================================
     C     WRTZ9P        BEGSR
     C*
     C                   CLEAR                   Z9P01
     C*
     C* Member Name
     C*
     C                   MOVEL     XXMBR         Z9MBR
     C                   EVAL      Z9#PRC = XXRCD
     C                   EVAL      ESSTAT = 'A'
     C                   EVAL      Z9IUSR = Q1USER
     C                   EVAL      Z9IPGM= Q1PGM
     C                   EVAL      Z9PRTM = DSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         Z9PRDT
     C                   EVAL      Z9USR  = Q1USER
     C                   EVAL      Z9PGM = Q1PGM
     C                   EVAL      Z9ITM  = DSTIME
     C                   EVAL      ESIDT = Z9TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         Z9DT
     C                   WRITE     Z9P01
     C*
     C                   ENDSR
     C*================================================================
     C* INTHDR - INITIALIZE HEADER FIELDS
     C*================================================================
     C     INTHDR        BEGSR
     C*
     C                   CLEAR                   Z1P01
     C*
     C                   ENDSR
     C*================================================================
     C* WRTHDR - LOAD HEADER FIELDS
     C*================================================================
     C     WRTHDR        BEGSR
     C*
     C                   EVAL      U1TYPE = '1'
     C                   MOVE      EAEOB#        XXEOB#            9
     C                   MOVE      EARCN         XXRCN             3
     C                   EVAL      U1TRID = %editc(eaeob#:'X') +
     C                              %editc(earcn:'X')
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U1CRDT
     C                   MOVE      EAFND         XXFND
     C                   MOVE      EAFYR         XXFYR
     C                   MOVE      EACASE        XXCASE
     C                   Z-ADD     0             XXCLM
     C                   MOVEL     XXCLM#        U1CLM#
     C                   Z-ADD     EAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U1DOI
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVEL(P)  CDLSN         U1CLSN
     C                   MOVEL(P)  CDFSN         U1CFSN
     C*
     C*** BEG ADD 02/07/12 ***************************************************
     C                   EVAL      X1CHRG = 0
     C                   EVAL      X1MRA  = 0
     C                   EVAL      X1NDIS = 0
     C                   EVAL      X1PAID = 0
     C                   EVAL      X1OTHR = 0
     C                   EVAL      X1OTH2 = 0
     C*
     C     EAKY          SETLL     PEA02                              60
     C                   READ      PEA02                                  60
     C                   DOW       *IN60 = *OFF
     C                   IF        X1RCN >= P2RCN
     C                   EVAL      X1CHRG = X1CHRG + P2CHRG
     C                   EVAL      X1MRA  = X1MRA  + P2MRA
     C                   EVAL      X1NDIS = X1NDIS + P2NDIS
     C                   EVAL      X1PAID = X1PAID + P2PAID
     C                   EVAL      X1OTHR = X1OTHR + P2OTHR
     C                   EVAL      X1OTH2 = X1OTH2 + P2OTH2
     C                   ENDIF
     C     EAKY          READE     PEA02                                  60
     C                   ENDDO
     C*
     C                   MOVE (P)  X1CHRG        U1CHRG
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        X1CHRG < 0
     C                   EVAL      X1CHRG = X1CHRG * -1
     C                   EVAL      U1CHRG =  %editc(X1CHRG:'X')
     C                   MOVEL     '-'           U1CHRG
     C                   ELSE
     C                   EVAL      U1CHRG =  %editc(X1CHRG:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  X1MRA         U1MRA
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        X1MRA  < 0
     C                   EVAL      X1MRA  = X1MRA * -1
     C                   EVAL      U1MRA  =  %editc(X1MRA :'X')
     C                   MOVEL     '-'           U1MRA
     C                   ELSE
     C                   EVAL      U1MRA  =  %editc(X1MRA :'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  X1NDIS        U1NDIS
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        X1NDIS < 0
     C                   EVAL      X1NDIS = X1NDIS * -1
     C                   EVAL      U1NDIS =  %editc(X1NDIS:'X')
     C                   MOVEL     '-'           U1NDIS
     C                   ELSE
     C                   EVAL      U1NDIS =  %editc(X1NDIS:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  X1PAID        U1PAID
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        X1PAID < 0
     C                   EVAL      X1PAID = X1PAID * -1
     C                   EVAL      U1PAID =  %editc(X1PAID:'X')
     C                   MOVEL     '-'           U1PAID
     C                   ELSE
     C                   EVAL      U1PAID =  %editc(X1PAID:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  X1OTHR        U1OTHR
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        X1OTHR < 0
     C                   EVAL      X1OTHR = X1OTHR * -1
     C                   EVAL      U1OTHR =  %editc(X1OTHR:'X')
     C                   MOVEL     '-'           U1OTHR
     C                   ELSE
     C                   EVAL      U1OTHR =  %editc(X1OTHR:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  X1OTH2        U1OTH2
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        X1OTH2 < 0
     C                   EVAL      X1OTH2 = X1OTH2 * -1
     C                   EVAL      U1OTH2 =  %editc(X1OTH2:'X')
     C                   MOVEL     '-'           U1OTH2
     C                   ELSE
     C                   EVAL      U1OTH2 =  %editc(X1OTH2:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C*** END ADD 02/07/12 ***************************************************
     C*** BEG DLT 02/07/12 ***************************************************
     C***                MOVE (P)  EACHRG        U1CHRG
     C***                MOVE (P)  EAMRA         U1MRA
     C***                MOVE (P)  EANDIS        U1NDIS
     C***                MOVE (P)  EAPAID        U1PAID
     C*** END DLT 02/07/12 ***************************************************
     C*
     C*** GET RENDERING PROVIDER FIRST NAME
     C*
     C     PDKY          CHAIN     PPD01                              60
     C*
     C                   MOVEL(P)  EAPNAM        U1PNAM
     C                   MOVEL(P)  EAADR1        U1ADR1
     C                   MOVEL(P)  EAADR2        U1ADR2
     C                   MOVEL(P)  EACTY         U1CTY
     C                   MOVEL(P)  EAST          U1ST
     C                   EVAL      U1ZIP  = EAZIP
     C* Recon Flag
     C                   IF        EARCN > 0
     C                   MOVEL(P)  'Y'           U1RCNF
     C                   ELSE
     C*
     C                   IF        EARCN = 0
     C                   MOVEL(P)  'N'           U1RCNF
     C                   ENDIF
     C                   ENDIF
     C* Recon Number
     C                   IF        U1RCNF = 'Y'
     C                   MOVE      EARCN         U1RCN
     C                   ENDIF
     C* Bill Adjustment Reason Code
     C     EEKY          CHAIN     PEE01                              96
     C                   DOW       *IN96 = *OFF
     C                   IF        EEPRNT <> 'N'
     C                   MOVEL(P)  EEEOB         U1BADJ
     C                   LEAVE
     C                   ENDIF
     C     EEKY          READE     PEE01                                  96
     C                   ENDDO
     C*
     C                   EVAL      U1BACD = %TRIM(EENOT1) + %TRIM(EENOT2)
     C* Provider ID (FEIN and Mod)
     C                   MOVE      EAFEIN        XXFEIN
     C                   MOVE      EAMOD         XXMOD
     C                   EVAL      XXPROV =   XXFEIN + XXMOD
     C                   MOVE      XXPROV        U1PROV
     C*
     C*** BEG DLT 02/07/12 *****************************************************
     C***                MOVEL(P)  EAOTH2        U1OTH2
     C*** END DLT 02/07/12 *****************************************************
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   MOVEL     SRDTO         U1DTRV
     C*** BEG DLT 02/07/12 *****************************************************
     C***                MOVE (P)  EAOTHR        U1OTHR
     C*** END DLT 02/07/12 *****************************************************
     C                   EVAL      XXPAID  = EOPAID - EAPAID
     C*** BEG ADD 02/07/12 *****************************************************
     C* IF EARCN=0 there should be no "previously paid amount" or "this payment amount"
     C                   IF        EARCN = 0
     C                   MOVE (P)  *BLANKS       U1PRPD
     C                   MOVE (P)  *BLANKS       U1THPD
     C                   ELSE
     C*** END ADD 02/07/12 *****************************************************
     C*
     C                   MOVE (P)  XXPAID        U1PRPD
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        XXPAID < 0
     C                   EVAL      XXPAID = XXPAID * -1
     C                   EVAL      U1PRPD = %editc(XXPAID:'X')
     C                   MOVEL     '-'           U1PRPD
     C                   ELSE
     C                   EVAL      U1PRPD = %editc(XXPAID:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C                   MOVE (P)  EAPAID        U1THPD
     C*** Beg Add *** 03/19/12 ************************************************
     C                   IF        EAPAID < 0
     C                   EVAL      EAPAID = EAPAID * -1
     C                   EVAL      U1THPD = %editc(EAPAID:'X')
     C                   MOVEL     '-'           U1THPD
     C                   ELSE
     C                   EVAL      U1THPD = %editc(EAPAID:'X')
     C                   ENDIF
     C*** End Add *** 03/19/12 ************************************************
     C*** BEG ADD 02/07/12 *****************************************************
     C                   ENDIF
     C*** END ADD 02/07/12 *****************************************************
     C*
     C*   GET CLAIM#
     C*
     C     CMKY          CHAIN     PCM01                              60
     C  N60              MOVEL(P)  CMSCL#        U1ACLM
     C
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
