     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DLR - Type DL Discount Code Calcualtions
     H*****************************************************************
     H* This pgm is called by N9600R to calcualte the network discount*
     H* amt based on the providers discount code. The discount will   *
     H* only be calculated for the single sequence number passed to   *
     H* this program.                                                 *
     H*                                                               *
     H*  Called from: N9600R                                          *
     H*  Calls to: None                                               *
     H*                                                               *
     H*****************************************************************
     H*  Created on 02/28/2007 by c:62 for Project 114MMZ             *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFTABL     IF   E           K DISK
     FFMMCL1    IF   E           K DISK    USROPN
     FFMZRL     IF   E           K DISK    USROPN
     FFMP1L     IF   E           K DISK

     D*================================================================
     D* Arrays
     D*================================================================
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)
     D*================================================================
     D* Data Structures
     D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  40     41
     D                 DS
     D  QQCMD2                 1     56
     D  QQST2                 38     39
     D                 DS
     D  MOD                    1      8
     D                                     DIM(4)
     D  DHMOD1                 1      2
     D  DHMOD2                 3      4
     D  DHMOD3                 5      6
     D  DHMOD4                 7      8
     D*================================================================
     D* Constants
     D*================================================================
     D XXNOT1          C                   CONST('REDUCTIONS BASED ON-
     D                                      MEDICARE AND MED-
     D                                     ICARE GUIDELINES')
     D XXNOT2          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

     C*================================================================
     C* Parameter Lists
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQCODE            3
     C                   PARM                    QQCEDT            8
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFL1             1
     C                   PARM                    QQFL2             1
     C                   PARM                    QQFL3             1
     C                   PARM                    QQFL4             1
     C                   PARM                    QQFL5             1
     C                   PARM                    QQDS1            10
     C                   PARM                    QQDS2            10
     C                   PARM                    QQDS3            10
     C                   PARM                    QQDS4            10
     C                   PARM                    QQDS5            10
     C                   PARM                    QQRTCD            1
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
     C*================================================================
     C* Key Lists
     C*================================================================
     C*
     C* Billing Code Table File
     C*
     C     BDKY1         KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*
     C     BDKY2         KLIST
     C                   KFLD                    QQCODE            3
     C*
     C* Case Detail File
     C*
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXFDAT
     C                   KFLD                    XXDSEQ
     C*
     C* Detail Notes File
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C* Physician Detail Information
     C*
     C     DHKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C* EOMB Transaction File
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    CAEOB#
     C*
     C* Medicare Rates File
     C*
     C     MCKY1         KLIST
     C                   KFLD                    CACPT4
     C                   KFLD                    CAFDAT
     C*
     C     MCKY2         KLIST
     C                   KFLD                    CACPT4
     C*
     C* Provider Auxiliary File
     C*
     C     P1KY          KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C* Claim Status Code Table File
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C* Zip Region File
     C*
     C     ZRKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    KKFZCD
     C                   KFLD                    CAFDAT
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    BDDT          XXFDAT
     C     *LIKE         DEFINE    BDPC01        XXPC00
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C     *LIKE         DEFINE    CADIFF        XXDIFF
     C     *LIKE         DEFINE    CAMRAA        XXALOW
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    ZRFZCD        KKFZCD
     C     *LIKE         DEFINE    CAPAID        XXPAID
     C     *LIKE         DEFINE    BDPC01        XXPCT
     C*
     C*================================================================
     C* Initialize variables
     C*================================================================
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C*
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      *OFF          POC               1            % OF CHRGS
     C*================================================================
     C* Main Processing
     C*================================================================
     C*
     C* Get the case detail record
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF                                         If 1
     C     CACPT4        ANDNE     'FSMRA'
     C*
     C* Be sure claim status code is not "C" for cap on EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              99
     C     *IN99         IFEQ      *OFF                                         If 2
     C     CSDISC        ANDNE     'C'
     C*
     C* For each detail record read, find the discount master record
     C* that applies to that date of service
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF                                         If 3
     C     EOBTYP        ANDEQ     'N'
     C*
     C     DHKY          CHAIN     PDH01                              99        If 4
     C     *IN99         IFEQ      *OFF
     C*
     C*
     C* Let's retrieve the billing record
     C*
     C     BDKY1         SETGT     PBD01
     C     BDKY2         READPE    PBD01                                  99
     C     *IN99         DOWEQ     *OFF
     C     BDSTAT        IFNE      'I'
     C                   LEAVE
     C                   ENDIF                                                              T
     C     BDKY2         READPE    PBD01                                  99
     C                   ENDDO
     C*
     C* If no code is retrieved then let's skip this line
     C*
     C     *IN99         IFEQ      *OFF                                         If 5
     C                   DO
     C*
     C* Let's get the medicare rate info.
     C*
     C                   EXSR      OPNMCP
     C*
     C     MCKY1         SETGT     PMC01
     C     MCKY2         READPE    PMC01                                  99
     C     *IN99         IFEQ      *OFF                                         If 6
     C*
     C* Let's calculate the network discount
     C*
     C* Get the Locality
     C*
     C                   EXSR      OPNZRP
     C                   MOVEL     EOFLIC        KKFZCD
     C     KKFZCD        IFEQ      0
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     P1ZIP         KKFZCD
     C                   ENDIF
     C                   ENDIF
     C     ZRKY          SETGT     PZR01                              99
     C                   READP     PZR01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKFZCD        IFGE      ZRFZCD
     C     KKFZCD        ANDLE     ZRTZCD
     C                   MOVE(P)   ZRRGN#        XXLOC             3 0
     C                   ENDIF
     C                   EXSR      CLSZRP
     C                   ENDIF
     C*
     C     XXLOC         IFEQ      0
     C                   LEAVE
     C                   ENDIF
     C*
     C* What is the key modifier
     C*
     C                   MOVEL     *BLANKS       XXPMOD            2
     C     1             DO        4             X                 1 0
     C     MOD(X)        IFEQ      '26'
     C     MOD(X)        OREQ      'TC'
     C     MOD(X)        OREQ      '53'
     C                   MOVEL     MOD(X)        XXPMOD
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C* Is this a facility or a non-facility?
     C*
     C                   MOVE      *ON           FAC               1
     C     DHPOS         IFEQ      ' 11'
     C                   MOVE      *OFF          FAC
     C                   ENDIF
     C*
     C* Retrieve the Medicare Rate
     C*
     C                   SELECT
     C     XXLOC         WHENEQ    1
     C                   SELECT
     C     XXPMOD        WHENEQ    '  '
     C     FAC           IFEQ      *ON
     C                   Z-ADD     MCFA01        XXRATE            9 2
     C                   ELSE
     C                   Z-ADD     MCNF01        XXRATE
     C                   ENDIF
     C     XXPMOD        WHENEQ    '53'
     C     FAC           IFEQ      *ON
     C                   Z-ADD     MCF501        XXRATE
     C                   ELSE
     C                   Z-ADD     MCN501        XXRATE
     C                   ENDIF
     C     XXPMOD        WHENEQ    '26'
     C                   Z-ADD     MC2601        XXRATE
     C     XXPMOD        WHENEQ    'TC'
     C                   Z-ADD     MCTC01        XXRATE
     C                   ENDSL
     C     XXLOC         WHENEQ    99
     C                   SELECT
     C     XXPMOD        WHENEQ    '  '
     C     FAC           IFEQ      *ON
     C                   Z-ADD     MCFA99        XXRATE            9 2
     C                   ELSE
     C                   Z-ADD     MCNF99        XXRATE
     C                   ENDIF
     C     XXPMOD        WHENEQ    '53'
     C     FAC           IFEQ      *ON
     C                   Z-ADD     MCF599        XXRATE
     C                   ELSE
     C                   Z-ADD     MCN599        XXRATE
     C                   ENDIF
     C     XXPMOD        WHENEQ    '26'
     C                   Z-ADD     MC2699        XXRATE
     C     XXPMOD        WHENEQ    'TC'
     C                   Z-ADD     MCTC99        XXRATE
     C                   ENDSL
     C                   ENDSL
     C*
     C* Now let's accumulate the percentages
     C*
     C                   Z-ADD     BDPC01        XXPCT
     C* Check for Bilateral
     C                   Z-ADD     1             X
     C     '50'          LOOKUP    MOD(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MULT(H)   1.5           XXPCT
     C                   ENDIF
     C* Check for Multiple Surgery
     C                   Z-ADD     1             X
     C     '51'          LOOKUP    MOD(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MULT(H)   1             XXPCT
     C                   ENDIF
     C* Check for Co-surgeon
     C                   Z-ADD     1             X
     C     '62'          LOOKUP    MOD(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MULT(H)   .625          XXPCT
     C                   ENDIF
     C* Check for Assistant surgeon 1
     C                   Z-ADD     1             X
     C     '80'          LOOKUP    MOD(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MULT(H)   .16           XXPCT
     C                   ENDIF
     C* Check for Assistant surgeon 2
     C                   Z-ADD     1             X
     C     '82'          LOOKUP    MOD(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MULT(H)   .16           XXPCT
     C                   ENDIF
     C* Check for Assistant surgeon physician's assistant
     C                   Z-ADD     1             X
     C     'AS'          LOOKUP    MOD(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   MULT(H)   .16           XXPCT
     C                   ENDIF
     C*
     C* Calculate the fee amount
     C*
     C     XXRATE        MULT(H)   XXPCT         XXPAID
     C*
     C* Compare the fee amount with the fee schedule amount
     C*
     C     XXPAID        IFEQ      0
     C                   MOVE      *ON           POC
     C     CACHRG        MULT(H)   .8            CAPAID
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   Z-ADD     0             CAMRA
     C                   Z-ADD     0             CAOTHR
     C                   Z-ADD     0             CAOTH2
     C                   ELSE
     C     XXPAID        IFLT      CAPAID
     C                   Z-ADD     XXPAID        CAPAID
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   SUB       CAMRA         CANDIS
     C                   SUB       CAOTHR        CANDIS
     C                   SUB       CAOTH2        CANDIS
     C                   ENDIF
     C                   ENDIF
     C*
     C* If discount applies: update Case Detail
     C*
     C     CANDIS        IFGT      *ZEROS                                       If 7
     C*
     C* Get next detail sequence number
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C* Write detail note
     C*
     C                   CLEAR                   PDE01
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C     POC           IFEQ      *OFF
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      '395'         DEEOB
     C                   MOVEL     '395'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   ELSE
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      '398'         DEEOB
     C                   MOVEL     '398'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT2        DENOT1
     C                   ENDIF
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'N'           DEESRC
     C                   Z-ADD     CARCN         DERCN
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   WRITE     PDE01
     C                   ENDIF                                                   CANDIS 7
     C                   ENDIF                                                   *in99 6
     C                   EXSR      CLSMCP
     C                   ENDDO
     C                   ENDIF                                                   *IN99 5
     C                   ENDIF                                                   *IN99 4
     C                   ENDIF                                                   *IN99 3
     C                   ENDIF                                                   *IN99 2
     C                   UPDATE    PCA01
     C                   ENDIF                                                   *IN99 1
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C* CLSMCP
     C*****************************************************************
     C     CLSMCP        BEGSR
     C*
     C                   CLOSE     FMMCL1
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(2)
     C                   PARM      56            LENGTH           15 5
     C*
     C                   ENDSR
     C*****************************************************************
     C* CLSZRP
     C*****************************************************************
     C     CLSZRP        BEGSR
     C*
     C                   CLOSE     FMZRL
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM      56            LENGTH           15 5
     C*
     C                   ENDSR
     C*****************************************************************
     C* OPNMCP
     C*****************************************************************
     C     OPNMCP        BEGSR
     C*
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL     EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM      56            LENGTH           15 5
     C*
     C                   OPEN      FMMCL1
     C*
     C                   ENDSR
     C*****************************************************************
     C* OPNZRP
     C*****************************************************************
     C     OPNZRP        BEGSR
     C*
     C                   MOVEL(P)  QCA(3)        QQCMD2
     C                   MOVEL     EOAST         QQST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD2
     C                   PARM      56            LENGTH           15 5
     C*
     C                   OPEN      FMZRL
     C*
     C                   ENDSR
**  (QCMDEX)
OVRDBF FILE(FMMCL1) TOFILE(*LIBL/FMMCL1  )
DLTOVR FILE(FMMCL1)
OVRDBF FILE(FMZRL) TOFILE(*LIBL/FMZRL  )
DLTOVR FILE(FMZRL)
