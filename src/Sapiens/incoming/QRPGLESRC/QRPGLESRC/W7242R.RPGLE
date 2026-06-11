     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7242R - RECOVERY REPORT
     H*   JJJ REVISED 3-3-89  ADDED CODE TO SORT BY EMPLOYER; COUNTY;
     H*                       CHAPTER; OR AGENT
     H*   JJJ REVISED 5-30-90 ADDED CODE TO SELECT STATUS, FIELD ADJ,
     H*                       & ATTORNEY
     H*******************************************************************
     H** REVISIONS:                                                  ****
     H*                                                               *
     H* 11/22/16 C:300 Project: WC2462                                *
     H*                -Show Full Name of Sort Selection, rather      *
     H*                -than just showing a single letter.            *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  01/07/07 C:53 SUPPORT: 61754                                 *
     H*                CLEAR INDICATOR FROM PREVIOUS READ.            *
     H*                                                               *
     H*  11/04/05 C:132 PROJECT: WC129 - 48736                        *
     H*                If Sort = 'N', and Employer Range entered,     *
     H*                Add edit for Employer Range.                   *
     H*                                                              ****
     H* 08/05/99 C:118 PROJECT: WC577                                 *
     H*                Report revisions.                              *
     H*                                                               *
     H* 06/20/99 C:118 PROJECT WC577                                  *
     H*                V3 REPORT REVISIONS.                           *
     H*                                                               *
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H* 08/12/97 C:78 PROJECT WC577                                   *
     H*               FIELDS SVEMP# AND SVDIV FORCED INTO SMALLER     *
     H*               FIELDS FOR OUTPUT PURPOSES.                     *
     H*                                                               *
     H* 08/27/97 C:78 PROJECT WC577                                   *
     H*               CHG OUTPUT TO MEET STANDARDS.                   *
     H*                                                               *
     H*******************************************************************
     H*   INDICATOR USAGE:
     H*
     FWXREL1    IF   E           K DISK
     FWXREL2    IF   E           K DISK
     F                                     RENAME(PRE01:PRE02)
     FWXREL3    IF   E           K DISK
     F                                     RENAME(PRE01:PRE03)
     FWXREL4    IF   E           K DISK
     F                                     RENAME(PRE01:PRE04)
     FWXREL     IF   E           K DISK
     F                                     RENAME(PRE01:PRE05)
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOA)

     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              PMT AMOUNT
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
     D  RR                     1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D*** BEGIN CHG *** 06/26/97 **************************************
     D**********  DS
     D**********                              1  66 DATA
     D**********                              1   1 QQSORT
     D**********                              2  100QQFEMP
     D**********                             11  190QQTEMP
     D**********                             20  240QQFDIV
     D**********                             25  290QQTDIV
     D**********                             30  31 QQFCNT
     D**********                             32  33 QQTCNT
     D**********                             34  360QQFCHP
     D**********                             37  390QQTCHP
     D**********                             40  440QQFAGT
     D**********                             45  490QQTAGT
     D**********                             50  51 QQSTAT
     D**********                             52  54 QQFEAD
     D**********                             55  630QQATTY
     D**********                             64  660QQMOD
     D                 DS
     D  DATA                   1     70
     D  QQSORT                 1      1
     D  QQFEMP                 2     10  0
     D  QQTEMP                11     19  0
     D  QQFDIV                20     24  0
     D  QQTDIV                25     29  0
     D  QQFCNT                30     33
     D  QQTCNT                34     37
     D  QQFCHP                38     40  0
     D  QQTCHP                41     43  0
     D  QQFAGT                44     48  0
     D  QQTAGT                49     53  0
     D  QQSTAT                54     55
     D  QQFEAD                56     58
     D  QQATTY                59     67  0
     D  QQMOD                 68     70  0
     D*** END   CHG *** 06/26/97 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      ***** Beg Add ***** 11/22/16 ***************************************
     D strSortSel      S             20
      ***** End Add ***** 11/22/16 ***************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR WORK FILE
     C*
     C     REKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHIC FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    REFEIN
     C                   KFLD                    REMOD
     C*                                                    JJJ 5-30-90
     C     PDKY1         KLIST                                                  *
     C                   KFLD                    REATTY                         *
     C                   KFLD                    REAMOD                         *
     C*
     C*   KEY LIST FOR THE CLAIM FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    REFYR
     C                   KFLD                    RECASE
     C*                                                    JJJ 3-7-89
     C* KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE      |
     C*                                                    |
     C     AAKEY         KLIST                                                  |
     C                   KFLD                    XXCO#                          |
     C                   KFLD                    XXFND                          |
     C                   KFLD                    REAGT#            5 0          |
     C*                                                    |
     C* KEY LIST FOR THE AGENT MASTER FILE                 |
     C*                                                    |
     C     AGKEY         KLIST                                                  |
     C                   KFLD                    XXCO#                          |
     C                   KFLD                    AAFEIN                         |
     C                   KFLD                    AAMOD                          |
     C*                                                    |
     C* KEY LIST FOR THE COUNTY CODE TABLE FILE            |
     C*                                                    |
     C     TBKEY         KLIST                                                  |
     C                   KFLD                    XXCO#                          |
     C                   KFLD                    XXKEY            10            |
     C*                                                    |
     C* KEY LIST FOR THE CHAPTER MASTER FILE               |
     C*                                                    |
     C     CPKEY         KLIST                                                  |
     C                   KFLD                    XXCO#                          |
     C                   KFLD                    XXFND                          |
     C                   KFLD                    RECHAP                         |
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     *LIKE         DEFINE    RECNTY        HCNTY
     C     *LIKE         DEFINE    FNANAM        XXCONM
     C     *LIKE         DEFINE    CNDESC        X1DESC
     C*** END   ADD *** 06/26/97 **************************************
     C*
     C*   ENTRY PARMS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           PARM           QQDATA 66        |
     C                   PARM                    QQDATA           70            |
     C*** END   CHG *** 06/26/97 **************************************
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQDATA        DATA
     C                   MOVE      QQMODE        XXMODE            1
      ***** Beg Add ***** 11/22/16 ***************************************
      /Free
       Select;
          When QQSORT = 'N';
             strSortSel = 'Claimant Name';
          When QQSORT = 'E';
             strSortSel = 'Employer & Division';
          When QQSORT = 'C';
             strSortSel = 'County Code';
          When QQSORT = 'H';
             strSortSel = 'Chapter Number';
          When QQSORT = 'A';
             strSortSel = 'Agent Number';
       EndSl;
      /End-Free
      ***** End Add ***** 11/22/16 ***************************************
     C     QQSORT        COMP      'E'                                    31     JJJ 3-7-89
     C     QQSORT        COMP      'C'                                    32      |
     C     QQSORT        COMP      'H'                                    33      |
     C     QQSORT        COMP      'A'                                    34      |
     C     QQSORT        COMP      'N'                                    35      |
     C     QQSTAT        COMP      *BLANK                                 25     JJJ 5-30-90
     C     QQFEAD        COMP      *BLANK                                 26     *
     C     QQATTY        COMP      *ZERO                                  27     *
     C*
     C     XXMODE        IFEQ      'D'                                          DEMAND MODE
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      ***** BEG ADD ***** 11/04/05 *************************************
     C     QQFEMP        IFNE      *ZEROS
     C     QQSORT        ANDEQ     'N'
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF                                                  QQFEMP <> ZEROS
      ***** END ADD ***** 11/04/05 *************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME           60
     C                   END
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVEL     FNANAM        XXCONM
     C*
     C                   ELSE
     C*
     C                   MOVEL     CONAME        XXCONM
     C*
     C                   ENDIF                                                  FNANAM
     C*
     C                   MOVEL(P)  XXCONM        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXCONM
     C*
     C*** END   ADD *** 06/26/97 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C***** BEGIN ADD **** 08/05/99 ***********************************
     C*
     C*  LEFT JUSTIFY REPORT TITLE ON ROUTING PAGE
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>BLKS
     C*
     C***** END   ADD **** 08/05/99 ***********************************
     C     *IN60         IFEQ      '0'
     C***** BEGIN DEL **** 08/05/99 ***********************************
     C**********           MOVE *BLANKS   XXTITL 60
     C**********           MOVELORTITL    XXTITL
     C***** END   DEL **** 08/05/99 ***********************************
     C***** BEGIN ADD **** 08/05/99 ***********************************
     C*
     C*  CENTER REPORT TITLE ON REPORT
     C*
     C                   MOVE      *BLANKS       XXTITL           60
     C                   MOVEL     ORTITL        ##DATA           80
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C*
     C*  SET UP ROUTING PAGE
     C*
     C                   SELECT
     C*
     C     QQSORT        WHENEQ    'E'
     C                   MOVE      *ON           *IN91
     C*
     C     QQSORT        WHENEQ    'C'
     C                   MOVE      *ON           *IN92
     C*
     C     QQSORT        WHENEQ    'H'
     C                   MOVE      *ON           *IN93
     C*
     C     QQSORT        WHENEQ    'A'
     C                   MOVE      *ON           *IN94
     C*
     C                   ENDSL
     C***** END   ADD **** 08/05/99 ***********************************
      ***** Beg Del ***** 08/26/14 ***************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ***************************************
     C                   END
     C*
     C                   MOVE      'N'           XXPRNT
     C*
     C*  START READING WORK FILE
     C*
     C   31REKEY         CHAIN     PRE01                              60        JJJ 3-7-89
     C   32REKEY         CHAIN     PRE02                              60        |
     C   33REKEY         CHAIN     PRE03                              60        |
     C   34REKEY         CHAIN     PRE04                              60        |
     C   35REKEY         CHAIN     PRE05                              60        |
     C     *IN60         DOWEQ     '0'
      *
      ***** BEG ADD ***** 11/04/05 *************************************
      * IF SORT BY CLAIMANT AND HAVE EMP RANGE - CHK EMP IN RANGE
     C     *IN35         IFEQ      *ON
     C     *IN69         ANDEQ     *ON
     C     REEMP#        IFLT      QQFEMP
     C     REEMP#        ORGT      QQTEMP
     C                   GOTO      NXTRCD
     C                   ENDIF                                                  REEMP# < QQFEMP
     C                   ENDIF                                                  *IN35 & *IN69
      ***** END ADD ***** 11/04/05 *************************************
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  CHECK FOR FUND YEAR BREAK
     C*
     C     REFYR         IFNE      XXHYR
     C     XXHYR         ANDNE     0
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C  N31
     CANN35              EXCEPT    TOTL1                                        JJJ 3-7-89
     C                   MOVE      RECNTY        HCNTY                          |
     C                   Z-ADD     RECHAP        HCHAP                          |
     C                   Z-ADD     REAGT#        HAGT#                          |
     C  N31
     CANN35              EXSR      SRDES                                        |
     C                   EXCEPT    TOTLY
     C                   Z-ADD     1900          XXYEAR
     C                   ADD       REFYR         XXYEAR
     C                   EXCEPT    HEAD
     C*    ACCUMULATE FUND TOTALS AND CLEAR FUND YEAR TOTALS
     C                   ADD       XXYRTI        XXFNTI
     C                   ADD       XXYRER        XXFNER
     C                   ADD       XXYRRE        XXFNRE
     C                   ADD       XXYRAR        XXFNAR
     C                   ADD       XXYRAC        XXFNAC
     C                   Z-ADD     0             XXYRTI
     C                   Z-ADD     0             XXYRER
     C                   Z-ADD     0             XXYRRE
     C                   Z-ADD     0             XXYRAR
     C                   Z-ADD     0             XXYRAC
     C                   Z-ADD     0             XXL1TI                         JJJ 3-7-89
     C                   Z-ADD     0             XXL1ER                         |
     C                   Z-ADD     0             XXL1RE                         |
     C                   Z-ADD     0             XXL1AR                         |
     C                   Z-ADD     0             XXL1AC                         |
     C                   END
     C*
      **** BEGIN ADD ***** 08/12/97 ***********************************
     C                   Z-ADD     REEMP#        REEMP2            5 0
     C                   Z-ADD     REDIV         REDIV2            3 0
      **** END   ADD ***** 08/12/97 ***********************************
     C*
     C*                                                    JJJ 3-7-89
     C*  CHECK FOR SORT BREAK                              |
     C*                                                    |
     C                   MOVE      'N'           XXBRK             1            |
     C*                                                    |
     C   32RECNTY        IFNE      HCNTY                                        |
     C                   MOVE      'Y'           XXBRK                          |
     C                   END                                                    |
     C*                                                    |
     C   33RECHAP        IFNE      HCHAP                                        |
     C                   MOVE      'Y'           XXBRK                          |
     C                   END                                                    |
     C*                                                    |
     C   34REAGT#        IFNE      HAGT#                                        |
     C                   MOVE      'Y'           XXBRK                          |
     C                   END                                                    |
     C*                                                    |
     C     XXBRK         IFEQ      'Y'                                          |
     C     *INOA         IFEQ      '1'                                          |
     C                   EXCEPT    HEAD                                         |
     C                   MOVE      '0'           *INOA                          |
     C                   END                                                    |
     C                   EXCEPT    TOTL1                                        |
     C                   MOVE      RECNTY        HCNTY                          |
     C                   Z-ADD     RECHAP        HCHAP                          |
     C                   Z-ADD     REAGT#        HAGT#                          |
     C                   EXSR      SRDES                                        |
     C                   EXCEPT    HEAD                                          *JJJ 6-5-90
     C                   EXCEPT    SUBH                                         |
     C                   Z-ADD     0             XXL1TI                         |
     C                   Z-ADD     0             XXL1ER                         |
     C                   Z-ADD     0             XXL1RE                         |
     C                   Z-ADD     0             XXL1AR                         |
     C                   Z-ADD     0             XXL1AC                         |
     C                   END                                                    |
     C*
     C*  CHECK TO SEE IF THERE IS ANOTHER REQUEST UNDER SAME CLAIM
     C*  SET INDICATORS FOR DETAIL PRINT LINE & CALC TOTAL INCURRED
     C*  IF WE HAVE A DIFFERENT CLAIM.
     C*
     C                   MOVE      '0'           *IN78
     C     RECASE        IFNE      XHCASE
     C     REFYR         ORNE      XXHYR
     C*
     C                   MOVE      '1'           *IN78
     C                   Z-ADD     0             XXTOTI            9 2
     C                   Z-ADD     0             XXTOTA            9 2
     C                   Z-ADD     0             XXTOTR            9 2
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   XFOOT     AR            XXTOTA
     C                   XFOOT     RR            XXTOTR
     C     XXTOTA        ADD       XXTOTR        XXTOTI
     C                   ELSE
     C                   Z-ADD     0             CMACDT
     C                   END
     C*
     C                   ADD       XXTOTI        XXYRTI
     C                   ADD       XXTOTI        XXL1TI                         JJJ 3-7-89
     C                   END
     C*
     C*  GET PAYEE NAME FROM PAYEE DEMOGRAPHICS FILE
     C*
     C                   MOVE      *BLANK        XXPNAM           30
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        XXPNAM
     C                   END
     C*                                                    JJJ 5-30-90
     C*  GET ATTORNEY NAME FROM PAYEE DEMOGRAPHICS FILE    *
     C*                                                    *
     C                   MOVE      *BLANK        XXATTY           30            *
     C     PDKY1         CHAIN     PPD01                              60        *
     C     *IN60         IFEQ      '0'                                          *
     C                   MOVEL     PDNAME        XXATTY                         *
     C                   END                                                    *
     C*                                                    *
     C*  GET SDF ADJUSTOR FROM SDF LOG MASTER FILE         *
     C*                                                    *
     C                   MOVE      *BLANK        XXFNAM           30            *
     C*                                                    *
     C*  GET STATUS DESCRIPTION FROM TABLE FILE            *
     C*                                                    *
     C                   MOVE      *BLANK        XXKEY                          *
     C                   MOVEL     RESTCD        XXKEY                          *
     C                   MOVE      *BLANKS       XXDESC           30            *
     C     TBKEY         CHAIN     PRS01                              60        *
     C     *IN60         IFEQ      '0'                                          *
     C                   MOVEL     RSDESC        XXDESC                         *
     C                   END                                                    *
     C*
     C*  IF CODE HAS ACCEPTED TYPE, PRINT ESTIMATED REIM UNDER ACCEPTED
     C*  CLAIMS, OTHERWISE PRINT UNDER UNFILED.
     C*
     C*** BEG ADD *** 01/07/07 ************************************
     C                   MOVE      *OFF          *IN75
     C*** END ADD *** 01/07/07 ************************************
     C*** BEG CHG *** 01/07/07 ************************************
     C********** RSTYPE    COMP 'A'                      75
     C  N60RSTYPE        COMP      'A'                                    75
     C*** END CHG *** 01/07/07 ************************************
     C*
     C*  PRINT DETAIL LINE
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C                   Z-ADD     REDTFI        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTFI            6 0
     C                   MOVE      *BLANKS       XXNAME           24
     C                   MOVEL     RENAME        XXNAME
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DETAIL
     C*
     C*  ACCUMULATE YEAR TOTALS & SORT TOTALS
     C*
     C     *IN75         IFEQ      '0'
     C                   ADD       REEAMT        XXL1ER                         JJJ 3-7-89
     C                   ADD       REEAMT        XXYRER
     C                   ADD       RERCVD        XXL1AC                         JJJ 6-12-90
     C                   ADD       RERCVD        XXYRAC                         |
     C                   ELSE
     C                   ADD       REEAMT        XXL1RE                         JJJ 3-7-89
     C                   ADD       RERAMT        XXL1AR                         |
     C                   ADD       RERCVD        XXL1AC                         |
     C                   ADD       REEAMT        XXYRRE
     C                   ADD       RERAMT        XXYRAR
     C                   ADD       RERCVD        XXYRAC
     C                   END
     C*
     C                   Z-ADD     REFYR         XXHYR
     C                   Z-ADD     RECASE        XHCASE
      *
      ***** BEG ADD ***** 11/04/05 *************************************
     C     NXTRCD        TAG
      ***** END ADD ***** 11/04/05 *************************************
     C*
     C   31REKEY         READE     PRE01                                  60      JJJ 3-7-89
     C   32REKEY         READE     PRE02                                  60      |
     C   33REKEY         READE     PRE03                                  60      |
     C   34REKEY         READE     PRE04                                  60      |
     C   35REKEY         READE     PRE05                                  60      |
     C                   END
     C*
     C*  PRINT TOTALS FOR SORT, YR AND FUND
     C*
     C     XXPRNT        IFEQ      'Y'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C  N31
     CANN35              EXCEPT    TOTL1                                        JJJ 3-7-89
     C                   EXCEPT    TOTLY
     C*    ACCUMULATE FUND TOTALS
     C                   ADD       XXYRTI        XXFNTI
     C                   ADD       XXYRER        XXFNER
     C                   ADD       XXYRRE        XXFNRE
     C                   ADD       XXYRAR        XXFNAR
     C                   ADD       XXYRAC        XXFNAC
     C*
     C                   MOVE      '1'           *IN12
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTLF
     C*
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     C     SR1ST         BEGSR
     C*
     C*   INITIALIZE SORT, FUND YEAR AND FUND TOTALS
     C*
     C                   Z-ADD     0             XXL1TI           11 2          JJJ 3-7-89
     C                   Z-ADD     0             XXL1ER           11 2          |
     C                   Z-ADD     0             XXL1RE           11 2          |
     C                   Z-ADD     0             XXL1AR           11 2          |
     C                   Z-ADD     0             XXL1AC           11 2          |
     C*
     C                   Z-ADD     0             XXYRTI           11 2
     C                   Z-ADD     0             XXYRER           11 2
     C                   Z-ADD     0             XXYRRE           11 2
     C                   Z-ADD     0             XXYRAR           11 2
     C                   Z-ADD     0             XXYRAC           11 2
     C*
     C                   Z-ADD     0             XXFNTI           11 2
     C                   Z-ADD     0             XXFNER           11 2
     C                   Z-ADD     0             XXFNRE           11 2
     C                   Z-ADD     0             XXFNAR           11 2
     C                   Z-ADD     0             XXFNAC           11 2
     C*
     C*   SET FYR & CASE HOLD FIELDS TO 0 & PRINT HEADING
     C*
     C                   Z-ADD     0             XXHYR             3 0
     C*** BEGIN CHG *** 06/26/97 **************************************
     C**********           MOVE RECNTY    HCNTY   2        JJJ 3-7-89
     C                   MOVE      RECNTY        HCNTY
     C*** END   CHG *** 06/26/97 **************************************
     C                   Z-ADD     RECHAP        HCHAP             3 0          |
     C                   Z-ADD     REAGT#        HAGT#             5 0          |
     C                   Z-ADD     0             XHCASE            7 0
     C                   Z-ADD     1900          XXYEAR            4 0
     C                   ADD       REFYR         XXYEAR
     C                   MOVE      '1'           *IN11
     C                   MOVE      'Y'           XXPRNT            1
     C  N31              EXSR      SRDES                                        JJJ 3-7-89
     C*
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
     C*==================================================================*
     C*   GET HEADING DESCRIPTION FOR SORT ** Subroutine **   S R D E S  *
     C*==================================================================*
     C*                          SUBROUTINE ADDED JJJ 3-7-89
     C     SRDES         BEGSR
     C*
     C*   IF COUNTY SORT, GET COUNTY DESCRIPTION
     C*
     C     *IN32         IFEQ      '1'
     C                   MOVE      *BLANKS       CNDESC
     C                   MOVEL     RECNTY        XXKEY
     C     TBKEY         CHAIN     PCN01                              58         cnty name
     C*** BEGIN ADD *** 06/26/97 **************************************
     C*
     C     *IN58         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CNDESC        X1DESC
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       X1DESC
     C*
     C                   ENDIF                                                   *IN58
     C*** END   ADD *** 06/26/97 **************************************
     C                   END
     C*
     C*   IF CHAPTER SORT, GET CHAPTER DESCRIPTION
     C*
     C     *IN33         IFEQ      '1'
     C                   MOVE      *BLANKS       CPDESC
     C     CPKEY         CHAIN     PCP01                              58         chap name
     C                   END
     C*
     C* IF AGENT SORT, GET AGT ASSIGNMENT & MASTER RECORD LOAD ADDR
     C*
     C     *IN34         IFEQ      '1'
     C                   MOVE      *BLANKS       AGNAME
     C                   MOVE      *BLANKS       AGMAD1
     C                   MOVE      *BLANKS       AGCS
     C     AAKEY         CHAIN     PAA01                              60
     C  N60AGKEY         CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     AGMCTY        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     AGMZIP        XXST10           10
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        AGCS             40
     C                   END
     C                   END
     C*
     C                   ENDSR
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ***************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O**********               XXTITL    80
     O                       ORTITL              80
     O***** END   CHG **** 08/05/99 ***********************************
      ***** Beg Del ***** 08/26/14 ***************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O*  MLS 1/29/88 ADD OUT QUEUE AND INDICATORS
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ***************************************
     O          E            ROUTE          2 36
     O***** BEGIN ADD **** 08/05/99 ***********************************
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O***** END   ADD **** 08/05/99 ***********************************
     O                                           22 'SORT SELECTION:'
      ***** Beg Chg ***** 11/22/16 ***************************************
     O******                 QQSORT              24
     O                       strSortSel          43
      ***** End Chg ***** 11/22/16 ***************************************
     O          E            ROUTE          2
     O                                           22 'EMPLOYER RANGE:'
     O                       QQFEMP        Z     32
     O                       QQFDIV        Z     38
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O***********                        40 '-'
     O               91                          40 '-'
     O***** END   CHG **** 08/05/99 ***********************************
     O                       QQTEMP        Z     50
     O                       QQTDIV        Z     56
     O          E            ROUTE          2
     O                                           20 'COUNTY RANGE:'
     O                       QQFCNT              25
     O***** BEGIN CHG **** 08/04/99 ***********************************
     O**********                         27 '-'
     O               92                          27 '-'
     O***** END   CHG **** 08/04/99 ***********************************
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               QQTCNT    30
     O                       QQTCNT              32
     O*** END   CHG *** 06/26/97 **************************************
     O          E            ROUTE          2
     O                                           21 'CHAPTER RANGE:'
     O                       QQFCHP        Z     26
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O**********                         28 '-'
     O               93                          28 '-'
     O***** END   CHG **** 08/05/99 ***********************************
     O                       QQTCHP        Z     32
     O          E            ROUTE          2
     O                                           19 'AGENT RANGE:'
     O                       QQFAGT        Z     28
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O**********                         30 '-'
     O               94                          30 '-'
     O***** END   CHG **** 08/05/99 ***********************************
     O                       QQTAGT        Z     36
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***  END   CHG  ***  01/28/10  *********************************
      ****** BEGIN CHG ****** 08/27/97 ********************************
     O***********************************46 'VE DATA SOLUTIONS. INC,'
     O***********************************51 '.1997'
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O***  END   CHG  ***  01/28/10  *********************************
     O***  BEGIN ADD  ***  01/28/10  *********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   ADD ***** 01/28/10 **********************************
      ****** BEGIN CHG ****** 08/27/97 ********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME   115
     O                       XXCONM             125
     O*** END   CHG *** 06/26/97 **************************************
     O                       DSDATE        Y    196
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL             126
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           2
      ****** BEGIN CHG ****** 08/27/97 ********************************
     O***********************************05 'FUND:'
     O*************************XXFND Z***09
     O*************************XFNAME****71
     O****************N12****************88 'FUND YEAR:'
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       XFNAME              72
     O***** END   CHG **** 08/27/97 ***********************************
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O**********      N12                88 'POLICY YEAR:'
     O              N12                          98 'POLICY YEAR:'
     O**********      N12      XXYEAR    93
     O              N12      XXYEAR             103
     O***** END   CHG **** 08/05/99 ***********************************
     O          E   N12 32   HEAD           2
     O                                           07 'COUNTY:'
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               HCNTY     10
     O**********               CNDESC    41
     O                       HCNTY               12
     O                       X1DESC              43
     O*** END   CHG *** 06/26/97 **************************************
     O          E   N12 33   HEAD           2
     O                                           08 'CHAPTER:'
     O                       HCHAP         Z     12
     O                       CPDESC              43
     O          E   N12 34   HEAD           1
     O                                            6 'AGENT:'
     O                       HAGT#         Z     12
     O                       AGNAME              53
     O          E   N12 34   HEAD           1
     O                       AGMAD1              53
     O          E   N12 34   HEAD           2
     O                       AGCS                53
     O          E            HEAD           0
     O              N12                         117 'REIMBURSEMENT FROM'
     O                                          181 'ACCEPTED CLAIMS'
     O          E            HEAD           1
     O                                          174 '_______________________'
     O                                          195 '______________________'
     O          E            HEAD           1
     O              N12                          57 'ACCIDENT'
     O                                           69 'TOTAL'
     O              N12                          84 'DATE'
     O              N12                         117 'REFUND STATUS     '
     O                                          145 'UNFILED EST.'
     O                                          164 'UNFILED EST.'
     O                                          193 'AMOUNT          AMOUNT'
     O          E            HEAD           2
      ****** BEGIN CHG **** 08/12/97 **********************************
     O****************N12                22 'EMPLR DIV   CASE  NAME'
     O              N12                          09 'EMPLR DIV'
     O              N12                          28 'CASE NAME'
      ****** BEGIN CHG **** 08/12/97 **********************************
     O              N12                          55 'DATE'
     O                                           70 'INCURRED'
     O              N12                          97 'NOTIFIED/ACCEPTED  CODE'
     O              N12                         117 'SDF ATTORNEY      '
     O                                          145 'REIMBURSMENT'
     O                                          164 'REIMBURSMENT'
     O                                          179 'UNCOLLECTED'
     O                                          194 'COLLECTED'
     O          E       32   SUBH           2
     O                                           07 'COUNTY:'
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               HCNTY     10
     O**********               CNDESC    41
     O                       HCNTY               12
     O                       X1DESC              43
     O*** END   CHG *** 06/26/97 **************************************
     O          E       33   SUBH           2
     O                                           08 'CHAPTER:'
     O                       HCHAP         Z     12
     O                       CPDESC              43
     O          E       34   SUBH           1
     O                                            6 'AGENT:'
     O                       HAGT#         Z     12
     O                       AGNAME              53
     O          E       34   SUBH           1
     O                       AGMAD1              53
     O          E       34   SUBH           2
     O                       AGCS                53
     O          E            DETAIL         1
      ****** BEGIN CHG **** 08/12/97 **********************************
     O********************78   REEMP#Z   09
     O********************78   REDIV Z   15
     O                  78   REEMP2        Z     05
     O                  78   REDIV2        Z     09
      ****** END   CHG **** 08/12/97 **********************************
     O                  78   RECASE        Z     23
     O                  78   XXNAME              48
     O                  78   XXACDT        Y     57
     O                  78   XXTOTI        J     72
     O                       XXDTFI        Y     85
     O                       RESTCD              96
     O                       XXPNAM             129
     O              N75      REEAMT        J    145
     O               75      REEAMT        J    164
     O               75      RERAMT        J    180
     O                       RERCVD        J    196
     O          E            DETAIL         1
     O                       XXDESC             129
     O          E            DETAIL         1
     O                       XXATTY             129
     O          E            TOTL1       1  2
     O                  32                       55 'COUNTY TOTALS:'
     O                  33                       55 'CHAPTER TOTALS:'
     O                  34                       55 'AGENT TOTALS:'
     O                       XXL1TI        J     72
     O                       XXL1ER        J    145
     O                       XXL1RE        J    164
     O                       XXL1AR        J    180
     O                       XXL1AC        J    196
     O          E            TOTLY       1  2
     O***** BEGIN CHG **** 06/20/99 ***********************************
     O**********                         55 'FUND YEAR TOTALS:'
     O                                           55 'POLICY YEAR TOTALS:'
     O***** END   CHG **** 06/20/99 ***********************************
     O                       XXYRTI        J     72
     O                       XXYRER        J    145
     O                       XXYRRE        J    164
     O                       XXYRAR        J    180
     O                       XXYRAC        J    196
     O          E            TOTLF          2
     O***** BEGIN CHG **** 06/20/99 ***********************************
     O**********                         55 'FUND TOTALS:'
     O                                           55 'GROUP TOTALS:'
     O***** END   CHG **** 06/20/99 ***********************************
     O                       XXFNTI        J     72
     O                       XXFNER        J    145
     O                       XXFNRE        J    164
     O                       XXFNAR        J    180
     O                       XXFNAC        J    196
