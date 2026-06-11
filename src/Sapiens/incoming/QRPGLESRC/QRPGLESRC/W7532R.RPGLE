     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7532R - CUMULATIVE LOSS RATIO - Specialized Sort Sequence
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM IS A COPY FROM PCA VERSION OF CUMULATIVE RATIO   *
     H* REPORT WITH THE THE EXCEPTION OF EMPLOYER UNDERWRITER.        *
     H* THIS PROGRAM PRINTS A REPORT OF LOSS INFORMATION FOR ALL      *
     H* EMPLOYERS WITHIN A FUND.  A FUND YEAR RANGE IS SELECTED AND   *
     H* LOSS INFORMATION FOR EACH YEAR WILL PRINT ALONG WITH THE      *
     H* CUMULATIVE TOTAL OVER ALL YEARS. THIS REPORT PRINTS THE       *
     H* REPORT SORTED IN ONE OF THE FOLLOWING ORDERS: EMPLOYER,       *
     H* COUNTY, CHAPTER, REGION, AGENT, OR UNDERWRITER.  RANGES MUST  *
     H* BE SELECTED FOR ONE OF THESE SORT TYPES AND THE USER IS ABLE  *
     H* TO EXCLUDE NON-ACTIVE MEMBERS.                                *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7531C                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER:   C:60   09/17/96        PROJECT: 48WDV           *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/04/20 C:079 Project: 102WIH                                *
     H*                Delete records for current user in WFLRP       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/15/06 C:53  SUPPORT: 55633                                 *
     H*                CORRECT WORK FIELD THAT CAUSED A DD ERROR      *
     H*                                                               *
     H*  09/22/06 C: 58 PROJECT: 71WHI
     H*              Add SIC code sort & selection changes.
     H*                                                               *
     H* 06/11/04 C:53  PROJECT: 81WVK                                 *
     H*                Add SIC Code and description to file           *
     H*                                                               *
     H* 04/08/01 C:61  PROJECT WC577                                  *
     H*                Add Rating State and Rate Set Code to output   *
     H*                file.                                          *
     H*                                                               *
     H* 04/01/01 C:61  PROJECT 81WSC                                  *
     H*              - Detail, Summary, or Both.                      *
     H*              - 4 digit Policy Year output.                    *
     H*                                                               *
     H* 07/03/00  C:35   PROJECT: WC577                               *
     H*                  ADDITIONAL CHANGES FOR WRITING OUTFILE.      *
     H*                                                               *
     H* 06/18/00 C:61  PROJECT WC577                                  *
     H*                Write to outfile WFLRP.                        *
     H*                                                               *
     H* 09/04/98 C:78 PROJECT WC577                                   *
     H*               FIELDS SVEMP# AND SVDIV CHANGED BACK TO 9 AND 5 *
     H*               CHRS RESPECTIVELY. THEY WERE MOVED BACK TO      *
     H*               SMALLER FIELDS BACK IN 08/12/97 CAUSING 7 OR 6  *
     H*               DIGIT EMPLOYER NUMBER NOT TO BE PRINTED         *
     H*               CORRECTLY.                                      *
     H*                                                               *
     H* 08/25/98 C:65 PROJECT SUPPORT                                 *
     H*               PROGRAM IS NOT PRINTING WHEN THERE IS 1 RECORD. *
     H*                                                               *
     H*  01/07/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                                                               *
     H* 08/27/97 C:78 PROJECT WC577                                   *
     H*               OUTPUT CHANGED TO MEET NEW STANDARDS.           *
     H*                                                               *
     H* 08/12/97 C:78 PROJECT WC577                                   *
     H*               FIELDS SVEMP# AND SVDIV FORCED INTO SMALLER     *
     H*               FIELDS FOR OUTPUT PURPOSES.                     *
     H*                                                               *
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   08  BATCH MODE                                              *
     H*   09  DEMAND MODE                                             *
     H*   15  PRINT DETAIL PORTIONS OF HEADER                         *
     H*   16  PRINT SELECTION                                         *
     H*   20  PRINT CANCEL DATE                                       *
     H*   21  EMPLOYER SORT SELECTED                                  *
     H*   22  COUNTY SORT SELECTED                                    *
     H*   23  CHAPTER SORT SELECTED                                   *
     H*   24  REGION SORT SELECTED                                    *
     H*   25  AGENT SORT SELECTED                                     *
     H*   51  PRINT DETAIL REPORT                                     *
     H*   52  PRINT SUMMARY REPORT                                    *
     H*   58  GENERAL CHAIN INDICATOR                                 *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*                                                               *
     H*****************************************************************
     H*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWXCUL     IF   E           K DISK
     FWMCML7    IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PRS01)
     FWMCPL     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
      *
      * Employer Policy Year
     FWDELL     IF   E           K DISK
      *
      * Rate Set Code
     FWMRSL     IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F  196        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FWFLRP     UF A E             DISK
     F                                     RENAME(PLR01:POUT)
      *  SIC CODE DESCRIPTION FILE
     FWTGTL     IF   E           K DISK
     F*

     D YEAR            S              3  0 DIM(100)
     D AMT             S             63    DIM(100)
     D COMB            S             66    DIM(100)
     D TLYR            S              3  0 DIM(100)
     D AMT2            S             63    DIM(100)
     D NM              S             12    DIM(6) CTDATA PERRCD(1)
     D* Reserve Types
     D                 DS
     D  CMT                    1     20
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
     D*
     D* Payment Amounts
     D                 DS
     D  CMA                    1     90  2
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
     D*
     D* Remaining Reserves
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)
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
     D                 DS
     D  XAMT                   1     63
     D  PREM                   1     15  2
     D  TOTO                  16     24  0
     D  TOTC                  25     33  0
     D  INC                   34     48  2
     D  TPD                   49     63  2
     D                 DS
     D  TAMT                   1     63
     D  TPREM                  1     15  2
     D  TTOTO                 16     24  0
     D  TTOTC                 25     33  0
     D  TINC                  34     48  2
     D  TTPD                  49     63  2
     D                 DS
     D  XCOMB                  1     66
     D  CYEAR                  1      3  0
     D  CPREM                  4     18  2
     D  CTOTO                 19     27  0
     D  CTOTC                 28     36  0
     D  CINC                  37     51  2
     D  CTPD                  52     66  2
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Selection Data
     D                 DS
     D  DATA                   1    112
     D  SCFFYR                 1      3  0
     D  SCTFYR                 4      6  0
     D  SCFEMP                 7     15  0
     D  SCTEMP                16     24  0
     D  SCFDIV                25     29  0
     D  SCTDIV                30     34  0
     D  SCFCNT                35     38
     D  SCTCNT                39     42
     D  SCFCHP                43     45  0
     D  SCTCHP                46     48  0
     D  SCFRGN                49     51  0
     D  SCTRGN                52     54  0
     D  SCFAGT                55     59  0
     D  SCTAGT                60     64  0
     D  SCFLOS                65     71  2
     D  SCTLOS                72     78  2
     D  SCFPRM                79     89  2
     D  SCTPRM                90    100  2
     D  SCFSIC               101    106
     D  SCTSIC               107    112
     D*
     D                 DS
     D  XXKEY                  1     14  0
     D  CUEMP#                 1      9  0
     D  CUDIV                 10     14  0
     D                 DS
     D  SVSRT1                 1     40
     D  XXSSRT                 1      5
     D  XXSSIC                 1      6
     D*
     D*
     D* External Program Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPOUT
     I              LRCO#                       L9CO#
     I              LRFND                       L9FND
     I              LREMP#                      L9EMP#
     I              LRDIV                       L9DIV
     I              LRFYR                       L9FYR
     I              LRENAM                      L9ENAM
     I              LREFDT                      L9EFDT
     I              LRCXDT                      L9CXDT
     I              LRWALS                      L9WALS
     I              LREXM1                      L9EXM1
     I              LREXM2                      L9EXM2
     I              LREXM3                      L9EXM3
     I              LREXD1                      L9EXD1
     I              LREXD2                      L9EXD2
     I              LREXD3                      L9EXD3
     I              LRPREM                      L9PREM
     I              LROCLM                      L9OCLM
     I              LRTCLM                      L9TCLM
     I              LRTINC                      L9TINC
     I              LRPAID                      L9PAID
     I              LRLOSS                      L9LOSS
     I              LRFREQ                      L9FREQ
     I              LRDT                        L9DT
     I              LRTIME                      L9TIME
     I              LRUSER                      L9USER
     I              LRPGM                       L9PGM
     I              LRAGT#                      L9AGT#
     I              LRANAM                      L9ANAM
     I              LRCNAM                      L9CNAM
     I              LRFNAM                      L9FNAM
     I              LREFD8                      L9EFD8
     I              LRCXD8                      L9CXD8
     I              LREMD1                      L9EMD1
     I              LREMD2                      L9EMD2
     I              LREMD3                      L9EMD3
     I              LRCNTY                      L9CNTY
     I              LRCNDS                      L9CNDS
     I              LRCHAP                      L9CHAP
     I              LRCHDS                      L9CHDS
     I              LRRGN#                      L9RGN#
     I              LRREDS                      L9REDS
     I              LRJRST                      L9JRST
     I              LRRTCD                      L9RTCD
     I              LRSIC                       L9SIC
     I              LRSICD                      L9SICD

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY1        KLIST
     C                   KFLD                    XXSTAF            3
     C*
     C* Key List for the Override Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* KEY LIST FOR CUMULATIVE LOSS RATIO WORKFILE
     C*
     C     CUKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    XXFYR
      *
      *  Rate Set Code
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C* Key List for the Claim Master File
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CUEMP#
     C                   KFLD                    CUDIV
     C                   KFLD                    XXFYR
     C*
     C* Key List for the Claim History File
     C*
     C     CHKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXYMD
     C*
     C* Key list for the County Code Table File
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKY             10
     C*
     C* Key List for the Chapter Master File
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHAP
     C*
     C* Key List for the Region Master File
     C*
     C     REKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRGN#
     C*
     C* Key List for the AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C* Key List for the AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C* Key List for the SIC CODE DESC FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXGKEY
     C*
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXGKEY
     C     *LIKE         DEFINE    EMSIC         XXSIC
     C     *LIKE         DEFINE    L9SICD        XXSICD
     C*
     C     *LIKE         DEFINE    ORTITL        STITL
     C     *LIKE         DEFINE    CUCO#         XXCO#
     C     *LIKE         DEFINE    CUFND         XXFND
     C     *LIKE         DEFINE    CUEMP#        XXEMP#
     C     *LIKE         DEFINE    CUEMP#        SVEMP#
     C     *LIKE         DEFINE    CUEMP#        TTLEMP
     C     *LIKE         DEFINE    CUDIV         XXDIV
     C     *LIKE         DEFINE    CUDIV         SVDIV
     C     *LIKE         DEFINE    CUFYR         XXFYR
     C     *LIKE         DEFINE    CUFYR         SVFYR
     C     *LIKE         DEFINE    CUEFDT        XXYMD
     C     *LIKE         DEFINE    CUEFDT        XXRNDT
     C     *LIKE         DEFINE    FNNAME        XFNAME
     C     *LIKE         DEFINE    CUENAM        ENAME
     C     *LIKE         DEFINE    CUENAM        XXDESC
     C     *LIKE         DEFINE    CPCHAP        XXCHAP
     C     *LIKE         DEFINE    RERGN#        XXRGN#
     C     *LIKE         DEFINE    AAAGT#        XXAGT#
     C     *LIKE         DEFINE    XXKEY         SVKEY
     C     *LIKE         DEFINE    CUEFDT        SVEFDT
     C     *LIKE         DEFINE    CUCNDT        SVCNDT
     C     *LIKE         DEFINE    CUEXM1        SVEXM1
     C     *LIKE         DEFINE    CUEXM2        SVEXM2
     C     *LIKE         DEFINE    CUEXM3        SVEXM3
     C     *LIKE         DEFINE    CUWALS        SVWALS
     C     *LIKE         DEFINE    FNANAM        XXCONM
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQDATA          112
     C                   PARM                    QQSORT            1
     C                   PARM                    QQACTV            1
     C                   PARM                    QQDSB             1
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
      *
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
       //*** Beg Add *** 03/04/20 ******************************************
      /Free
        Exec Sql Delete from wflrp where LRuser = :Q1user;
      /End-free
       //*** End Add *** 03/04/20 ******************************************
     C*
     C* Set-up and Initialize
     C*
     C   LR
     CANNLR              READ      POUT                                   LR
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQDTE         X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQDATA        DATA
     C                   MOVE      QQSORT        SCSORT            1
     C                   MOVE      'Y'           XXHDG             1
     C                   MOVE      'N'           XXTOT             1
     C*
     C* Added field to accumulate the number of employers printed on
     C* the report.
     C                   Z-ADD     0             TTLEMP
     C*
     C* Determine Sort Selection then Get the Employer Master File
     C*
     C     QQSORT        IFEQ      'E'
     C                   MOVE      NM(1)         XXSRTN           12
     C                   MOVE      '1'           *IN21
     C                   END
     C     QQSORT        IFEQ      'C'
     C                   MOVE      NM(2)         XXSRTN
     C                   MOVE      '1'           *IN22
     C                   END
     C     QQSORT        IFEQ      'H'
     C                   MOVE      NM(3)         XXSRTN
     C                   MOVE      '1'           *IN23
     C                   END
     C     QQSORT        IFEQ      'R'
     C                   MOVE      NM(4)         XXSRTN
     C                   MOVE      '1'           *IN24
     C                   END
     C     QQSORT        IFEQ      'A'
     C                   MOVE      NM(5)         XXSRTN
     C                   MOVE      '1'           *IN25
     C                   END
     C*
     C     QQSORT        IFEQ      'S'
     C                   MOVEL     NM(6)         XXSRTN
     C                   MOVE      *ON           *IN26
     C                   END
     C*
     C* First Cycle Processing
     C*
     C                   EXSR      SR1ST
     C*
     C* Get the Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     FNNAME        XFNAME
     C                   MOVE      *ZEROS        COMB
     C                   MOVE      *ZEROS        AMT
     C                   MOVE      *ZEROS        YEAR
     C                   MOVE      *ZEROS        AMT2
     C                   MOVE      *ZEROS        TLYR
     C                   MOVE      *BLANKS       CMT
     C                   MOVE      *ZEROS        CMA
     C                   MOVE      *ZEROS        CMR
     C                   MOVE      *ZEROS        XAMT
     C                   MOVE      *ZEROS        TAMT
     C                   MOVE      *ZEROS        XCOMB
     C                   MOVE      'Y'           XX1ST             1
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVEL     FNANAM        XXCONM
     C*
     C                   ELSE
     C*
     C                   MOVEL     CONAME        XXCONM
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXCONM        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXCONM
     C*
     C*
     C*  START READING WORKFILE
     C*
     C                   READ      PCU01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C     XX1ST         IFEQ      'Y'
     C                   Z-ADD     CUFYR         SVFYR
     C                   Z-ADD     XXKEY         SVKEY
     C                   MOVE      CUSRT1        SVSRT1
     C                   MOVE      'Y'           XXPRNT            1
     C                   MOVE      'N'           XX1ST
     C                   EXSR      SRHDG
     C                   MOVE      '1'           *INOF
     C                   END
     C*
     C     SVSRT1        IFNE      CUSRT1
     C     *IN21         ANDEQ     '0'
     C                   EXSR      YRTTL
     C                   EXSR      EMPTTL
     C                   EXSR      GRDTTL
     C*
     C                   Z-ADD     CUFYR         SVFYR
     C                   Z-ADD     XXKEY         SVKEY
     C                   MOVE      CUSRT1        SVSRT1
     C                   MOVE      '1'           *IN15
     C                   EXSR      SRHDG
     C                   MOVE      '1'           *INOF
     C                   ELSE
     C*
     C     SVKEY         IFNE      XXKEY
     C                   EXSR      YRTTL
     C                   EXSR      EMPTTL
     C*
     C                   Z-ADD     CUFYR         SVFYR
     C                   Z-ADD     XXKEY         SVKEY
     C                   EXSR      SRHDG
     C                   ELSE
     C*
     C     SVFYR         IFNE      CUFYR
     C                   EXSR      YRTTL
     C*
     C                   Z-ADD     CUFYR         SVFYR
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   EXSR      DETL
     C*
     C     CUKEY         READE     PCU01                                  50
     C                   END
     C*
     C                   END
     C*
     C*  IF NO RECORDS WERE PROCESSED PRINT NO RECORDS MESSAGE,
     C*  OTHERWISE PRINT TOTALS.
     C*
     C     XXPRNT        IFEQ      *BLANKS
     C   51              EXCEPT    NORECS
     C   52              EXCEPT    SNOREC
     C                   GOTO      TERM
     C                   ELSE
     C                   EXSR      YRTTL
     C                   EXSR      EMPTTL
     C                   EXSR      GRDTTL
     C                   EXSR      LRTTL
     C                   END
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If qqdsb = 'D' or qqdsb = 'B';
          xxPrt = '0';
          Chain Orkey Por01;
          If %found;
             Except Route;
          Endif;
       Endif;
       If qqdsb = 'S' or qqdsb = 'B';
          xxPrt = '1';
          Chain Orkey Por01;
          If %found;
             Except Sroute;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C*  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
     C*****************************************************************
     C*
     C     SR1ST         BEGSR
      *
      *
      *  Detail Report
      *
     C     QQDSB         COMP      'D'                                    51
     C  N51QQDSB         COMP      'B'                                    51
     C   51              OPEN      QSYSPRT
      *
      *  Summary Report
      *
     C     QQDSB         COMP      'S'                                    52
     C  N52QQDSB         COMP      'B'                                    52
     C   52              OPEN      QSYSPRT1
      *
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Initialize Fields
     C*
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
      *
      * Get the Summary Report Routing record
      *
     C     *IN52         IFEQ      *ON
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      '0'
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
     C                   MOVEL(P)  ORTITL        STITL
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    SROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
      *
     C                   ENDIF
      *
      * Get the Detail Report Routing record
      *
     C     *IN51         IFEQ      *ON
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
     C*
     C* Print the Distribution Information
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C                   ENDIF
     C                   MOVE      '1'           *INOA
     C                   MOVE      '1'           *INOF
     C                   MOVE      '1'           *IN15
     C                   MOVE      '1'           *IN16
     C                   ENDSR
     C*
     C**
     C*****************************************************************==*
     C*   PREMIUM CALCULATIONS         ** SUBROUTINE **   S U B P R   *
     C*****************************************************************
     C*
     C     SUBPR         BEGSR
     C*
     C*     CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C*                       AND EARNED PREMIUM THIS PERIOD
     C*
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     XXFND         SRFND             3
     C                   MOVEL     XXFYR         SRFYR             3
     C                   MOVEL     SVEMP#        SREMP#            9
     C                   MOVEL     SVDIV         SRDIV             5
     C                   MOVE      *ZEROS        SRAMT            11
     C                   MOVE      *ZEROS        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         PRMYTD           15 2
     C                   MOVE      SRAMT2        PRMCUR           15 2
     C*
     C* CALCULATE LOSS RATIO
     C*
     C     PRMYTD        IFNE      *ZERO
     C     L1ITOT        MULT      100           XXITOT           15 2
     C     XXITOT        DIV(H)    PRMYTD        LOSS              7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         LOSS
     C                   END
     C*
     C* CALCULATE FREQUENCY RATIO
     C*
     C     PRMYTD        DIV(H)    1000          THPREM           15 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L1TOTC        DIV(H)    THPREM        FREQ              6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
     C                   END
     C*
     C                   ENDSR
     C**
     C*****************************************************************
     C* EMPLOYER DETAIL                ** SUBROUTINE **   D E T L     *
     C*****************************************************************
     C*
     C     DETL          BEGSR
     C*
     C                   MOVE      '1'           *IN15
     C*
     C                   Z-ADD     CUFYR         SVFYR
     C                   Z-ADD     CUEMP#        SVEMP#
     C                   Z-ADD     CUDIV         SVDIV
     C*
     C                   Z-ADD     CUEFDT        SVEFDT
     C                   Z-ADD     CUCNDT        SVCNDT
     C*
     C                   Z-ADD     CUEXM1        SVEXM1
     C                   Z-ADD     CUEXM2        SVEXM2
     C                   Z-ADD     CUEXM3        SVEXM3
     C                   MOVE      CUWALS        SVWALS
     C*
     C* FLIP EFFECTIVE AND CANCEL DATES FOR OUTPUT
     C*
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   Z-ADD     SVCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCNDT            6 0
     C*
     C                   MOVE      '0'           *IN20
     C     CUCNDT        IFNE      0
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C* Need to flip effective date of the experience mods for output
     C*
     C                   Z-ADD     CUEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD1            6 0
     C*
     C                   Z-ADD     CUEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD2            6 0
     C*
     C                   Z-ADD     CUEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD3            6 0
     C*
     C                   Z-ADD     CUFYR         XXFYR
     C     XXFYR         ADD       1900          X1FYR             4 0
      *
     C                   Z-ADD     CUEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1EMD1            8 0
      *
     C                   Z-ADD     CUEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1EMD2            8 0
      *
     C                   Z-ADD     CUEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         X1EMD3            8 0
      *
     C                   Z-ADD     CUEMD1        SVEMD1            7 0
     C                   Z-ADD     CUEMD2        SVEMD2            7 0
     C                   Z-ADD     CUEMD3        SVEMD3            7 0
     C*
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '1'
     C     CHCO#         ORNE      CMCO#
     C     CHFND         ORNE      CMFND
     C     CHFYR         ORNE      CMFYR
     C     CHCASE        ORNE      CMCASE
     C     CHEFDT        ORGT      XXYMD
     C                   MOVE      CMSTAT        CHOCST
     C                   END
     C*
     C     CHOCST        IFNE      'C'
     C                   ADD       1             L1OPEN            9 0
     C                   END
     C*
     C                   ADD       1             L1TOTC            9 0
     C*
     C                   Z-ADD     *ZERO         TOTPMT           15 2
     C                   Z-ADD     *ZERO         TOTRES           15 2
     C*
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C*
     C* ACCUMULATE PAYMENT AMOUNTS
     C*
     C                   ADD       CMA(R)        TOTPMT
     C*
     C* ACCUMULATE RESERVE AMOUNTS
     C*
     C                   ADD       CMR(R)        TOTRES
     C                   END
     C*
     C                   ADD       TOTPMT        L1ITOT           15 2
     C                   ADD       TOTRES        L1ITOT
     C                   ADD       TOTPMT        L1PTOT           15 2
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C*
     C                   ENDSR
     C**
     C*****************************************************************
     C* EMPLOYER POLICY YR CALCULATIONS  ** SUBROUTINE **  Y R T T L  *
     C*****************************************************************
     C*
     C     YRTTL         BEGSR
     C*
     C                   EXSR      SUBPR
     C*                                                                8
     C                   Z-ADD     PRMYTD        L1PREM           15 2
     C                   Z-ADD     FREQ          L1FREQ            6 2
     C                   Z-ADD     LOSS          L1LOSS            7 2
     C*
     C     L1LOSS        IFGE      SCFLOS
     C     L1LOSS        ANDLE     SCTLOS
     C     L1PREM        ANDGE     SCFPRM
     C     L1PREM        ANDLE     SCTPRM
     C                   MOVE      'Y'           XXTOT
     C                   MOVE      'Y'           XXPRNT            1
     C*
     C                   ADD       L1ITOT        L2ITOT           15 2
     C                   ADD       L1PTOT        L2PTOT           15 2
     C                   ADD       L1PREM        L2PREM           15 2
     C                   ADD       L1OPEN        L2OPEN            9 0
     C                   ADD       L1TOTC        L2TOTC            9 0
     C*
     C*   ACCUMULATE POLICY YEAR TOTALS  TO ARRAY FIELDS
     C*
     C                   Z-ADD     1             V                 3 0
     C     XXFYR         LOOKUP    YEAR(V)                                33
     C     *IN33         IFEQ      '0'
     C*
     C*  FIND NEXT AVAILABLE POSITION
     C*
     C                   Z-ADD     1             V
     C     YEAR(V)       DOWNE     *ZERO
     C                   ADD       1             V
     C                   END
     C*
     C*  ADD FIELDS TO THE ARRAY
     C*
     C                   Z-ADD     XXFYR         YEAR(V)
     C                   MOVE      *ZEROS        XAMT
     C                   Z-ADD     L1PREM        PREM
     C                   Z-ADD     L1OPEN        TOTO
     C                   Z-ADD     L1TOTC        TOTC
     C                   Z-ADD     L1ITOT        INC
     C                   Z-ADD     L1PTOT        TPD
     C                   MOVEA     XAMT          AMT(V)
     C*
     C*   IF CODE IS IN ARRAY ADD TO EXISTING TOTALS
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        XAMT
     C                   MOVEA     AMT(V)        XAMT
     C                   ADD       L1PREM        PREM
     C                   ADD       L1OPEN        TOTO
     C                   ADD       L1TOTC        TOTC
     C                   ADD       L1ITOT        INC
     C                   ADD       L1PTOT        TPD
     C                   MOVEA     XAMT          AMT(V)
     C*
     C                   END
     C*
     C*   ACCUMULATE POLICY YEAR TOTALS TO ARRAY
     C*
     C                   Z-ADD     1             V                 3 0
     C     XXFYR         LOOKUP    TLYR(V)                                33
     C     *IN33         IFEQ      '0'
     C*
     C*  FIND NEXT AVAILABLE POSITION
     C*
     C                   Z-ADD     1             V
     C     TLYR(V)       DOWNE     *ZERO
     C                   ADD       1             V
     C                   END
     C*
     C*  ADD FIELDS TO THE ARRAY
     C*
     C                   Z-ADD     XXFYR         TLYR(V)
     C                   MOVE      *ZEROS        TAMT
     C                   Z-ADD     L1PREM        TPREM
     C                   Z-ADD     L1OPEN        TTOTO
     C                   Z-ADD     L1TOTC        TTOTC
     C                   Z-ADD     L1ITOT        TINC
     C                   Z-ADD     L1PTOT        TTPD
     C                   MOVEA     TAMT          AMT2(V)
     C*
     C*   IF CODE IS IN ARRAY ADD TO EXISTING TOTALS
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        TAMT
     C                   MOVEA     AMT2(V)       TAMT
     C                   ADD       L1PREM        TPREM
     C                   ADD       L1OPEN        TTOTO
     C                   ADD       L1TOTC        TTOTC
     C                   ADD       L1ITOT        TINC
     C                   ADD       L1PTOT        TTPD
     C                   MOVEA     TAMT          AMT2(V)
     C*
     C                   END
     C*
     C* Check for page overflow
     C*
     C     *INOF         IFEQ      '1'
     C     *IN51         ANDEQ     *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
      *
     C                   ELSE
      *
     C     XXHDG         IFEQ      'Y'
     C     *IN51         ANDEQ     *ON
     C                   EXCEPT    HEAD1
     C                   END
      *
     C                   END
     C*
     C   51              EXCEPT    TOTL1
     C                   MOVE      'N'           XXHDG
      *
      * WRITE TO OUTPUT FILE
      *
     C                   EXSR      SROUT
      *
     C                   END
     C*
     C                   Z-ADD     0             L1PREM
     C                   Z-ADD     0             L1OPEN
     C                   Z-ADD     0             L1TOTC
     C                   Z-ADD     0             L1ITOT
     C                   Z-ADD     0             L1PTOT
     C                   Z-ADD     0             L1LOSS
     C                   Z-ADD     0             L1FREQ
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* EMP CUMULATIVE CALCULATIONS    ** SUBROUTINE **   E M P T T L *
     C*****************************************************************
     C*
     C     EMPTTL        BEGSR
     C*
     C     XXHDG         IFEQ      'N'
     C                   MOVE      'Y'           XXHDG
     C                   ADD       L2PREM        L3PREM           15 2
     C                   ADD       L2OPEN        L3OPEN            9 0
     C                   ADD       L2TOTC        L3TOTC            9 0
     C                   ADD       L2ITOT        L3ITOT           15 2
     C                   ADD       L2PTOT        L3PTOT           15 2
     C*
     C* CALCULATE LOSS RATIO
     C*
     C     L2PREM        IFNE      *ZEROS
     C     L2ITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    L2PREM        L2LOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L2LOSS
     C                   END
     C*
     C* CALCULATE FREQUENCY RATIO
     C*
     C     L2PREM        DIV(H)    1000          THPREM           15 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L2TOTC        DIV(H)    THPREM        L2FREQ            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L2FREQ
     C                   END
     C*
     C* Check for page overflow
     C*
     C     *INOF         IFEQ      '1'
     C     *IN51         ANDEQ     *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   ADD       1             TTLEMP
     C*
     C   51              EXCEPT    TOTL2
     C*
     C                   END
     C                   Z-ADD     0             L2PREM
     C                   Z-ADD     0             L2OPEN
     C                   Z-ADD     0             L2TOTC
     C                   Z-ADD     0             L2ITOT
     C                   Z-ADD     0             L2PTOT
     C                   Z-ADD     0             L2LOSS
     C                   Z-ADD     0             L2FREQ
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  SRT - Subroutine to sort arrays for policy totals by year    *
     C*****************************************************************
     C*
     C     SRT           BEGSR
     C*  SORT THE ARRAY
     C                   SORTA     COMB
     C*
     C*  FIND THE FIRST RECORD
     C*
     C                   Z-ADD     1             V
     C                   MOVE      '0'           *IN33
     C     COMB(V)       DOWEQ     *ZEROS
     C     *IN33         ANDEQ     '0'
     C     V             IFEQ      100
     C                   MOVE      '1'           *IN33
     C                   ELSE
     C                   ADD       1             V
     C                   END
     C                   END
     C*
     C     V             DOWLE     100
     C*
     C                   MOVE      *ZEROS        XCOMB
     C                   MOVEA     COMB(V)       XCOMB
     C*
     C* CALCULATE LOSS RATIO
     C*
     C     CPREM         IFNE      *ZERO
     C     CINC          MULT      100           XXITOT
     C     XXITOT        DIV(H)    CPREM         YRLOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         YRLOSS
     C                   END
     C*
     C* CALCULATE FREQUENCY RATIO
     C*
     C     CPREM         DIV(H)    1000          THPREM           15 2
     C*
     C     THPREM        IFNE      *ZERO
     C     CTOTC         DIV(H)    THPREM        YRFREQ            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         YRFREQ
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C     *IN51         ANDEQ     *ON
     C                   EXCEPT    HEAD
     C                   END
      *
      *
     C     *INOA         IFEQ      '1'
     C     *IN52         ANDEQ     *ON
     C                   EXCEPT    SHEAD
     C                   END
      *
     C     1900          ADD       CYEAR         XYEAR             4 0
      *
     C   51              EXCEPT    YRTOT
     C   52              EXCEPT    SYRTOT
     C*
     C                   ADD       1             V
     C                   END
     C*
     C                   ENDSR
     C*
     C**
     C*****************************************************************
     C* SELECTION TOTAL CALULATIONS    ** Subroutine **   G R D T T L *
     C*****************************************************************
     C*
     C     GRDTTL        BEGSR
     C*
     C     XXTOT         IFEQ      'Y'
     C                   MOVE      'N'           XXTOT
     C                   MOVE      '0'           *IN15
     C*
     C                   ADD       L3PREM        LRPREM           15 2
     C                   ADD       L3OPEN        LROPEN            9 0
     C                   ADD       L3TOTC        LRTOTC            9 0
     C                   ADD       L3ITOT        LRITOT           15 2
     C                   ADD       L3PTOT        LRPTOT           15 2
     C                   ADD       TTLEMP        LRTLEP            7 0
     C*
     C* CALCULATE LOSS RATIO
     C*
     C     L3PREM        IFNE      *ZERO
     C     L3ITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    L3PREM        L3LOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L3LOSS
     C                   END
     C*
     C* CALCULATE FREQUENCY RATIO
     C*
     C     L3PREM        DIV(H)    1000          THPREM           15 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L3TOTC        DIV(H)    THPREM        L3FREQ            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L3FREQ
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C     *IN51         ANDEQ     *ON
     C                   EXCEPT    HEAD
     C                   END
      *
      *
     C     *INOA         IFEQ      '1'
     C     *IN52         ANDEQ     *ON
     C                   EXCEPT    SHEAD
     C                   SETOFF                                       OA
     C                   END
      *
     C*
     C* MOVE CODE AND TOTAL ARRAYS TO COMBINATION ARRAY
     C*
     C                   Z-ADD     1             V
     C     YEAR(V)       DOWNE     *ZEROS
     C                   MOVEL     YEAR(V)       COMB(V)
     C                   MOVE      AMT(V)        COMB(V)
     C                   ADD       1             V
     C                   END
     C                   EXSR      SRT
     C*
     C   51              EXCEPT    TOTL3
     C   52              EXCEPT    STOTL3
     C                   SETON                                        OA
     C*
     C                   END
     C                   Z-ADD     0             L3PREM
     C                   Z-ADD     0             L3OPEN
     C                   Z-ADD     0             L3TOTC
     C                   Z-ADD     0             L3ITOT
     C                   Z-ADD     0             L3PTOT
     C                   Z-ADD     0             L3LOSS
     C                   Z-ADD     0             L3FREQ
     C                   Z-ADD     0             TTLEMP
     C*
     C                   MOVE      *ZEROS        COMB
     C                   MOVE      *ZEROS        AMT
     C                   MOVE      *ZEROS        YEAR
     C*
     C                   ENDSR
     C*
     C**
     C*****************************************************************
     C*   REPORT  TOTAL CALULATIONS    ** Subroutine **   L R T T L   *
     C*****************************************************************
     C*
     C     LRTTL         BEGSR
     C*
     C                   MOVE      '0'           *IN15
     C                   MOVE      '0'           *IN16
     C*
     C* CALCULATE LOSS RATIO
     C*
     C     LRPREM        IFNE      *ZERO
     C     LRITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    LRPREM        LRLOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         LRLOSS
     C                   END
     C*
     C* CALCULATE FREQUENCY RATIO
     C*
     C     LRPREM        DIV(H)    1000          THPREM           15 2
     C*
     C     THPREM        IFNE      *ZERO
     C     LRTOTC        DIV(H)    THPREM        LRFREQ            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         LRFREQ
     C                   END
     C*
      *
     C   51              EXCEPT    HEAD
     C   51              EXCEPT    TOTL4
      *
     C   52              EXCEPT    SHEAD
     C   52              EXCEPT    STOTL4
     C*
     C                   Z-ADD     0             LRPREM
     C                   Z-ADD     0             LROPEN
     C                   Z-ADD     0             LRTOTC
     C                   Z-ADD     0             LRITOT
     C                   Z-ADD     0             LRPTOT
     C                   Z-ADD     0             LRLOSS
     C                   Z-ADD     0             LRFREQ
     C*
     C                   ENDSR
     C**
     C*****************************************************************
     C*   SELECTION NAME               ** Subroutine **   H D G       *
     C*****************************************************************
     C*
     C     SRHDG         BEGSR
     C*
     C                   Z-ADD     CUFYR         SVFYR
     C                   Z-ADD     CUEMP#        SVEMP#
     C                   Z-ADD     CUDIV         SVDIV
     C                   Z-ADD     CUEFDT        SVEFDT
     C                   Z-ADD     CUCNDT        SVCNDT
     C                   Z-ADD     CUEXM1        SVEXM1
     C                   Z-ADD     CUEXM2        SVEXM2
     C                   Z-ADD     CUEXM3        SVEXM3
     C                   MOVE      CUWALS        SVWALS
     C                   MOVE      *BLANKS       ENAME
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVEL     CUENAM        ENAME
     C*
     C* If SIC Selection, get Description of SIC Code
     C     *IN26         IFEQ      *ON
     C                   MOVEL     CUSRT1        XXSIC
     C                   EXSR      SUBSIC
     C                   MOVEL     XXSICD        XXDESC
     C                   ENDIF
     C*
     C* FLIP EFFECTIVE AND CANCEL DATES FOR OUTPUT
     C*
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SVCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCNDT
     C*
     C                   MOVE      '0'           *IN20
     C     SVCNDT        IFNE      0
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C* Need to flip effective date of the experience mods for output
     C*
     C                   Z-ADD     CUEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD1
     C*
     C                   Z-ADD     CUEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD2
     C*
     C                   Z-ADD     CUEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD3
     C*
      *
      *  COUNTY NAME
      *
     C                   MOVE      CUCNTY        X1CNTY            2
     C                   MOVEL     CUCNTY        XXKY
     C     TBKEY         CHAIN     PCN01                              58
     C  N58
     CAN 22              MOVEL     CNDESC        XXDESC
     C  N58              MOVEL(P)  CNDESC        L9CNDS
     C   58              MOVEL(P)  *BLANKS       L9CNDS
      *
      *  CHAP NAME
      *
     C                   MOVE      CUCHAP        XXCHAP
     C     CPKEY         CHAIN     PCP01                              58
     C  N58
     CAN 23              MOVEL     CPDESC        XXDESC
     C  N58              MOVEL(P)  CPDESC        L9CHDS
     C   58              MOVEL(P)  *BLANKS       L9CHDS
      *
      *  REGION NAME
      *
     C                   MOVE      CURGN#        XXRGN#
     C     REKEY         CHAIN     PRE01                              58
     C  N58
     CAN 24              MOVEL     REDESC        XXDESC
     C  N58              MOVEL(P)  REDESC        L9REDS
     C   58              MOVEL(P)  *BLANKS       L9REDS
      *
      *  AGENT NAME
      *
     C                   MOVE      CUAGT#        XXAGT#
     C     AAKEY         CHAIN     PAA01                              58
     C     AGKEY         CHAIN     PAG01                              58
     C  N58
     CAN 25              MOVEL     AGNAME        XXDESC
     C  N58              MOVEL(P)  AGNAME        L9ANAM
     C   58              MOVEL(P)  *BLANKS       L9ANAM
      *
     C*
     C                   ENDSR
      *
      *
      *================================================================
      * SUBSIC- Get SIC Code Descptn   ** Subroutine **  S U B S I C
      *================================================================
     C     SUBSIC        BEGSR
      *
     C                   MOVEL(P)  'SIC'         XXTBID
     C                   MOVEL(P)  XXSIC         XXGKEY
     C     GTKY          CHAIN     WTGTL                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     GTDESC        XXSICD
     C                   ELSE
     C                   MOVE      *BLANKS       XXSICD
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      *================================================================
      * SROUT - Write record to output file WFCUP
      *================================================================
     C     SROUT         BEGSR
      *
     C                   Z-ADD     XXCO#         L9CO#
     C                   MOVEL(P)  CONAME        L9CNAM
     C                   Z-ADD     XXFND         L9FND
     C                   MOVEL(P)  FNNAME        L9FNAM
     C                   Z-ADD     SVEMP#        L9EMP#
     C                   Z-ADD     SVDIV         L9DIV
     C                   Z-ADD     XXFYR         L9FYR
     C                   MOVEL(P)  ENAME         L9ENAM
      *
     C                   Z-ADD     SVEFDT        L9EFDT
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         L9EFD8
      *
     C                   Z-ADD     SVCNDT        L9CXDT
     C                   Z-ADD     SVCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         L9CXD8
      *
     C                   MOVE      SVWALS        L9WALS
     C                   Z-ADD     SVEXM1        L9EXM1
     C                   Z-ADD     X1EMD1        L9EMD1
     C                   Z-ADD     SVEXM2        L9EXM2
     C                   Z-ADD     X1EMD2        L9EMD2
     C                   Z-ADD     SVEXM3        L9EXM3
     C                   Z-ADD     X1EMD3        L9EMD3
     C                   Z-ADD     SVEMD1        L9EXD1
     C                   Z-ADD     SVEMD2        L9EXD2
     C                   Z-ADD     SVEMD3        L9EXD3
     C                   Z-ADD     L1PREM        L9PREM
     C                   Z-ADD     L1OPEN        L9OCLM
     C                   Z-ADD     L1TOTC        L9TCLM
     C                   Z-ADD     L1ITOT        L9TINC
     C                   Z-ADD     L1PTOT        L9PAID
     C                   Z-ADD     L1LOSS        L9LOSS
     C                   Z-ADD     L1FREQ        L9FREQ
     C                   MOVE      X1CNTY        L9CNTY
     C                   Z-ADD     XXCHAP        L9CHAP
     C                   Z-ADD     XXRGN#        L9RGN#
     C                   Z-ADD     XXAGT#        L9AGT#
      *
     C                   MOVEL     Q1USER        L9USER
     C                   MOVEL     Q1PGM         L9PGM
     C                   TIME                    DSDTTM
     C                   MOVEL     DSTIME        L9TIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         L9DT
      *
      *
     C     ELKY          CHAIN     PEL01                              60
     C  N60              MOVE      ELRTCD        L9RTCD
     C   60              MOVE      *BLANKS       L9RTCD
     C  N60RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        L9JRST
     C   60              MOVE      *BLANKS       L9JRST
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMSIC         XXSIC
     C                   EXSR      SUBSIC
     C                   ELSE
     C                   MOVE      *BLANKS       XXSIC
     C                   MOVE      *BLANKS       XXSICD
     C                   MOVE      *BLANKS       XXDESC
     C                   ENDIF
      *
     C                   MOVEL     XXSIC         L9SIC
     C                   MOVEL     XXSICD        L9SICD
      *
     C                   WRITE     POUT
      *
     C                   ENDSR
      *
      *
      *
     C*****************************************************************
      *
      * DETAIL REPORT ROUTING PAGE
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
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
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O*
     O* Added output specs to show operator what was selected
     O*
     O          E            ROUTE       2  2
      ***** BEG CHG ******* 04/01/01 **********************************
     O*********                          20 'REPORT SELECTIONS:'
     O                                         +000 'REPORT SELECTIONS:'
      ***** END CHG ******* 04/01/01 **********************************
     O          E            ROUTE          2
     O*
     O*** BEGIN CHG *** 01/07/98 **************************************
     O*
     O* CHANGED ROUTING PAGE TO COMPLY WITH VERSION 3 STANDARDS.
     O*
     O                                           22 'GROUP  . . . . . . :'
     O                       XXFND         Z     26
     O*
     O          E            ROUTE          2
     O                                           22 'RUN DATE . . . . . :'
     O                       XXDATE              31 '  /  /  '
     O*
     O          E            ROUTE          2
     O                                           22 'POLICY YEAR RANGE  :'
     O                       SCFFYR        Z     26
     O                                           28 '-'
     O                       SCTFYR        Z     32
     O*
     O          E            ROUTE          2
     O                                           22 'SORT SELECTION . . :'
     O                       SCSORT              24
      ***** BEG ADD ******* 04/01/01 **********************************
     O*
     O          E            ROUTE          2
     O                                           22 'Report output  . . :'
     O                       QQDSB             +001
     O                                         +004 '(Detail, Summary, Both)'
      ***** END ADD ******* 04/01/01 **********************************
     O*
     O          E            ROUTE          2
     O                                           22 'ACTIVE MEMBERS ONLY:'
     O                       QQACTV              24
     O*
     O          E    21      ROUTE          2
     O*
     O                                           22 'EMP/DIV RANGE  . . :'
     O                       SCFEMP        Z     32
     O                       SCFDIV        Z     38
     O                                           40 '-'
     O                       SCTEMP        Z     50
     O                       SCTDIV        Z     56
     O*
     O          E    22      ROUTE          2
     O*
     O                                           22 'COUNTY CODE RANGE  :'
     O                       SCFCNT              26
     O                                           28 '-'
     O                       SCTCNT              33
     O*
     O          E    23      ROUTE          2
     O*
     O                                           22 'CHAPTER CODE RANGE :'
     O                       SCFCHP              26 ' 0 '
     O                                           28 '-'
     O                       SCTCHP              33 ' 0 '
     O*
     O          E    24      ROUTE          2
     O*
     O                                           22 'REGION RANGE . . . :'
     O                       SCFRGN              26 ' 0 '
     O                                           28 '-'
     O                       SCTRGN              33 ' 0 '
     O*
     O          E    25      ROUTE          2
     O*
     O                                           22 'AGENT # RANGE  . . :'
     O                       SCFAGT        Z     28
     O                                           30 '-'
     O                       SCTAGT        Z     36
     O*
     O          E            ROUTE          2
     O*
     O                                           22 'LOSS RATIO RANGE   :'
     O                       SCFLOS        3     31
     O                                           33 '-'
     O                       SCTLOS        3     42
     O*
     O          E            ROUTE          2
     O*
     O                                           22 'PREMIUM RANGE  . . :'
     O                       SCFPRM        1     37
     O                                           39 '-'
     O                       SCTPRM        1     54
     O*
     O***** END   CHG ******** 01/07/98 *******************************
     O***** BEG ADD ******* 09/22/06 **********************************
     O*
     O          E    26      ROUTE          2
     O*
     O                                           22 'SIC CODE RANGE . . :'
     O                       SCFSIC              29
     O                                           30 '-'
     O                       SCTSIC              37
     O***** END ADD ******* 09/22/06 **********************************
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ****** END DLT ***** 01/28/10 **********************************
      ***** BEGIN CHG ******** 08/27/97 *******************************
     O***********************************46 'VE DATA SOLUTIONS, INC.'
     O***********************************53 ', 1996'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                47 'VE DATA SOLUTIONS, INC.,'
     O***                      *YEAR   +  1
      ****** END DLT ***** 01/28/10 **********************************
      ***** END   CHG ******** 08/27/97 *******************************
     O*
     O* MAIN PAGE HEADING
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME   115
     O                       XXCONM             124
     O*** END   CHG *** 06/26/97 **************************************
     O                       DSDATE        Y    196
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           2
      ***** BEGIN CHG ******** 08/27/97 *******************************
     O************************************5 'FUND:'
     O*************************XXFND Z    9
     O*************************XFNAME    70
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       XFNAME              71
     O                                          100 'SORT SELECTION:'
     O                       XXSRTN             114
     O                                          144 'RUN DATE:'
     O                       XXDATE        Y    153
     O          E       16   HEAD           2
     O               22                          07 'COUNTY:'
     O               22      XXSSRT              13
     O               22      XXDESC              54
     O               23                          08 'CHAPTER:'
     O               23      XXSSRT              14
     O               23      XXDESC              55
     O               24                          07 'REGION:'
     O               24      XXSSRT              13
     O               24      XXDESC              54
     O               25                          06 'AGENT:'
     O               25      XXSSRT              12
     O               25      XXDESC              53
     O***** BEG ADD ******* 09/22/06 **********************************
     O               26                          06 'SIC:  '
     O               26      XXSSIC              12
     O               26      XXDESC              53
     O***** END ADD ******* 09/22/06 **********************************
     O                                          125 'LOSS RATIO RANGE:'
     O                       SCFLOS        3    133
     O                                          135 '-'
     O                       SCTLOS        3    143
     O                                          164 'PREMIUM RANGE:'
     O                       SCFPRM        1    179
     O                                          181 '-'
     O                       SCTPRM        1    196
     O          E    15      HEAD           1
     O****** BEGIN CHG ******** 09/04/98 ******************************
     O**************************         12 'EMP#  DIV'
     O**************************         27 'EMPLOYER NAME'
     O**************************         64 'EFF DATE'
     O**************************         74 'CNX DATE'
     O**************************         86 'WATCH LIST'
     O                                           17 'EMP#   DIV'
     O                                           31 'EMPLOYER NAME'
     O                                           68 'EFF DATE'
     O                                           78 'CNX DATE'
     O                                           89 'WATCH LIST'
     O****** END   CHG ******** 09/04/98 ******************************
     O          E            HEAD           1
     O               15                          12 'EXP MOD 1'
     O               15                          22 'EFF DATE'
     O               15                          34 'EXP MOD 2'
     O               15                          44 'EFF DATE'
     O               15                          56 'EXP MOD 3'
     O               15                          66 'EFF DATE'
      ****** BEGIN CHG ***** 08/27/97 *********************************
     O*****************15****************79 'FUND'
     O               15                          79 'POLICY'
      ****** END   CHG ***** 08/27/97 *********************************
     O                                          101 'PREMIUM'
     O                                          114 'OPEN'
     O                                          128 ' TOTAL'
     O                                          150 'TOTAL INCURRED'
     O                                          173 'TOTAL PAID'
     O                                          183 'LOSS'
     O                                          194 'FREQ'
     O          E            HEAD           2
     O               15                          79 'YEAR'
     O                                          115 'TO DATE        CLAIMS'
     O                                          128 'CLAIMS'
     O                                          195 'RATIO      RATIO'
     O*
     O* EMPLOYER NAME HEADING
     O*
     O          E            HEAD1          1
      ****** BEGIN CHG ***** 09/04/98 *********************************
      ****** BEGIN CHG ***** 08/12/97 *********************************
     O*************************SVEMP#Z***11
     O*************************SVDIV Z***18
     O*************************ENAME*****60
     O*************************XXEFDTY***70
     O*****************20******XXCNDTY***80
     O*************************SVWALS****88
     O************************ SVEMP2Z    8
     O************************ SVDIV2Z   12
     O************************ ENAME     54
     O************************ XXEFDTY   64
     O*****************20***** XXCNDTY   74
     O************************ SVWALS    82
     O                       SVEMP#        Z     11
     O                       SVDIV         Z     17
     O                       ENAME               58
     O                       XXEFDT        Y     68
     O               20      XXCNDT        Y     78
     O                       SVWALS              85
      ****** END   CHG ***** 08/12/97 *********************************
      ****** END   CHG ***** 09/04/98 *********************************
     O*
     O* EMPLOYER POLICY YEAR TOTAL
     O*
     O          E            TOTL1          1
     O                       SVEXM1        K     11
     O                       XXEMD1              22 '  /  /  '
      ***** BEGIN CHG ***** 08/12/97 **********************************
     O*************************SVEXM2K   33
     O                       SVEXM2        K     34
      ***** END   CHG ***** 08/12/97 **********************************
     O                       XXEMD2              44 '  /  /  '
      ***** BEGIN CHG ***** 08/12/97 **********************************
     O*************************SVEXM3K   55
     O                       SVEXM3        K     56
      ***** END   CHG ***** 08/12/97 **********************************
     O                       XXEMD3              66 '  /  /  '
      ***** BEG CHG ******* 04/01/01 **********************************
     O*********                XXFYR Z   78
     O                       X1FYR         Z     79
      ***** END CHG ******* 04/01/01 **********************************
     O                       L1PREM        J    102
     O                       L1OPEN        3    115
     O                       L1TOTC        3    128
     O                       L1ITOT        J    151
     O                       L1PTOT        J    174
     O                       L1LOSS        J    185
     O                       L1FREQ        J    196
     O*
     O* EMPLOYER CUMULATIVE TOTAL
     O*
     O          E            TOTL2          2
     O                                           56 '* EMPLOYER'
     O                                           75 'CUMULATIVE TOTAL *'
     O                       L2PREM        J    102
     O                       L2OPEN        3    115
     O                       L2TOTC        3    128
     O                       L2ITOT        J    151
     O                       L2PTOT        J    174
     O                       L2LOSS        J    185
     O                       L2FREQ        J    196
     O*
     O* SELECTION CUMULATIVE TOTAL
     O*
     O          E   N21      TOTL3       1  3
     O                                           15 ' TOTAL MEMBERS:'
     O                       TTLEMP        J     25
     O               22                          56 '  * COUNTY'
     O               23                          56 ' * CHAPTER'
     O               24                          56 '  * REGION'
     O               25                          56 '   * AGENT'
     O***** BEG ADD ******* 09/22/06 **********************************
     O               26                          56 '* SIC CODE'
     O***** END ADD ******* 09/22/06 **********************************
     O                                           75 'CUMULATIVE TOTAL *'
     O                       L3PREM        J    102
     O                       L3OPEN        3    115
     O                       L3TOTC        3    128
     O                       L3ITOT        J    151
     O                       L3PTOT        J    174
     O                       L3LOSS        J    185
     O                       L3FREQ        J    196
     O*
     O* Fund Total by Year
     O*
     O          E            YRTOT          1
      ***** BEG DEL ******* 04/01/01 **********************************
      ******BEGIN CHG ***** 08/27/97 **********************************
     O***********************************61 'FUND YEAR'
     O*********                          61 'POLICY YEAR'
      ******END   CHG ***** 08/27/97 **********************************
     O*********                CYEAR Z   65
      ***** END DEL ******* 04/01/01 **********************************
      ***** BEG ADD ******* 04/01/01 **********************************
     O                                           60 'POLICY YEAR'
     O                       XYEAR         Z   +001
      ***** END ADD ******* 04/01/01 **********************************
     O                                           73 'TOTALS:'
     O                       CPREM         J    102
     O                       CTOTO         3    115
     O                       CTOTC         3    128
     O                       CINC          J    151
     O                       CTPD          J    174
     O                       YRLOSS        J    185
     O                       YRFREQ        J    196
     O*
     O* FUND GRAND TOTALS
     O*
     O*** BEGIN CHG *** 01/07/98 **************************************
     O******* E                TOTL4
     O          E            TOTL4          1
     O*** END   CHG *** 01/07/98 **************************************
     O                                           15 ' TOTAL MEMBERS:'
     O                       LRTLEP        J     25
      ****** BEGIN CHG ***** 08/27/97 *********************************
     O***********************************73 ' FUND TOTALS:'
     O                                           73 ' GROUP TOTALS:'
      ****** BEGIN CHG ***** 08/27/97 *********************************
     O                       LRPREM        J    102
     O                       LROPEN        3    115
     O                       LRTOTC        3    128
     O                       LRITOT        J    151
     O                       LRPTOT        J    174
     O                       LRLOSS        J    185
     O                       LRFREQ        J    196
     O          E            NORECS         1 06
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME   115
     O                       XXCONM             124
     O*** END   CHG *** 06/26/97 **************************************
     O                       DSDATE        Y    196
     O          E            NORECS         1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            NORECS         3
     O*** BEGIN CHG *** 01/07/98 **************************************
     O**********                          5 'FUND:'
     O                                            6 'GROUP:'
     O*** END   CHG *** 01/07/98 **************************************
     O                       XXFND         Z      9
     O                       XFNAME              70
     O                                          100 'SORT SELECTION:'
     O                       XXSRTN             114
     O                                          144 'RUN DATE:'
     O                       XXDATE        Y    153
     O          E            NORECS      2
     O                                           25 '*** NO RECORDS MEET'
     O                                           47 'SELECTED CRITERIA ***'
      ***** BEG ADD ******* 04/01/01 **********************************
      *
      * SUMMARY REPORT ROUTING PAGE
      *
     OQSYSPRT1  E            SROUTE         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            SROUTE         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            SROUTE         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            SROUTE         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            SROUTE         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            SROUTE         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            SROUTE         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            SROUTE         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            SROUTE         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            SROUTE         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            SROUTE         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            SROUTE         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'

     O**********E            SROUTE         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
      *
     O**********E            SROUTE         1
     O**********     08      ORDIS2              60
      *
     O**********E            SROUTE         1
     O**********     08      ORDIS3              60
      *
     O**********E            SROUTE         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            SROUTE         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
      *
     O          E            SROUTE         1
     O               08      ORDIS2              60
      *
     O          E            SROUTE         1
     O               08      ORDIS3              60
      *
     O          E            SROUTE         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
      *
     O          E            SROUTE         2
     O                                           19 'REPORT TITLE:'
     O                       STITL               80
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            SROUTE         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      *
     O**********E            SROUTE         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      *
     O          E            SROUTE      2  2
     O                                         +000 'REPORT SELECTIONS:'
      *
     O          E            SROUTE         2
      *
     O                                           22 'GROUP  . . . . . . :'
     O                       XXFND         Z     26
      *
     O          E            SROUTE         2
     O                                           22 'RUN DATE . . . . . :'
     O                       XXDATE              31 '  /  /  '
      *
     O          E            SROUTE         2
     O                                           22 'POLICY YEAR RANGE  :'
     O                       SCFFYR        Z     26
     O                                           28 '-'
     O                       SCTFYR        Z     32
      *
     O          E            SROUTE         2
     O                                           22 'SORT SELECTION . . :'
     O                       SCSORT              24
      *
     O          E            SROUTE         2
     O                                           22 'REPORT OUTPUT  . . :'
     O                       QQDSB             +001
     O                                         +004 '(Detail, Summary, Both)'
      *
     O          E            SROUTE         2
     O                                           22 'ACTIVE MEMBERS ONLY:'
     O                       QQACTV              24
      *
     O          E    21      SROUTE         2
      *
     O                                           22 'EMP/DIV RANGE  . . :'
     O                       SCFEMP        Z     32
     O                       SCFDIV        Z     38
     O                                           40 '-'
     O                       SCTEMP        Z     50
     O                       SCTDIV        Z     56
      *
     O          E    22      SROUTE         2
     O                                           22 'COUNTY CODE RANGE  :'
     O                       SCFCNT              26
     O                                           28 '-'
     O                       SCTCNT              33
      *
     O          E    23      SROUTE         2
     O                                           22 'CHAPTER CODE RANGE :'
     O                       SCFCHP              26 ' 0 '
     O                                           28 '-'
     O                       SCTCHP              33 ' 0 '
      *
     O          E    24      SROUTE         2
     O                                           22 'REGION RANGE . . . :'
     O                       SCFRGN              26 ' 0 '
     O                                           28 '-'
     O                       SCTRGN              33 ' 0 '
      *
     O          E    25      SROUTE         2
     O                                           22 'AGENT # RANGE  . . :'
     O                       SCFAGT        Z     28
     O                                           30 '-'
     O                       SCTAGT        Z     36
      *
     O          E            SROUTE         2
     O                                           22 'LOSS RATIO RANGE   :'
     O                       SCFLOS        3     31
     O                                           33 '-'
     O                       SCTLOS        3     42
      *
     O          E            SROUTE         2
     O                                           22 'PREMIUM RANGE  . . :'
     O                       SCFPRM        1     37
     O                                           39 '-'
     O                       SCTPRM        1     54
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           SROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS, INC.,'
     O          E            SROUTE         1 59
     O                       #BRND1
     O          E            SROUTE         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      *
     O          E            SHEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             124
     O                       DSDATE        Y    196
      *
     O          E            SHEAD          1
     O                       Q1PGM               10
     O                       STITL              129
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
      *
     O          E            SHEAD          2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       XFNAME              71
     O                                          100 'SORT SELECTION:'
     O                       XXSRTN             114
     O                                          144 'RUN DATE:'
     O                       XXDATE        Y    153
      *
     O          E       16   SHEAD          2
     O               22                          07 'COUNTY:'
     O               22      XXSSRT              13
     O               22      XXDESC              54
     O               23                          08 'CHAPTER:'
     O               23      XXSSRT              14
     O               23      XXDESC              55
     O               24                          07 'REGION:'
     O               24      XXSSRT              13
     O               24      XXDESC              54
     O               25                          06 'AGENT:'
     O               25      XXSSRT              12
     O               25      XXDESC              53
     O***** BEG ADD ******* 09/22/06 **********************************
     O***** END ADD ******* 09/22/06 **********************************
     O***** BEG ADD ******* 09/22/06 **********************************
     O               26                          06 'SIC:  '
     O               26      XXSSIC              12
     O               26      XXDESC              53
     O***** END ADD ******* 09/22/06 **********************************
     O                                          125 'LOSS RATIO RANGE:'
     O                       SCFLOS        3    133
     O                                          135 '-'
     O                       SCTLOS        3    143
     O                                          164 'PREMIUM RANGE:'
     O                       SCFPRM        1    179
     O                                          181 '-'
     O                       SCTPRM        1    196
      *
     O          E    15      SHEAD          1
     O                                           17 'EMP#   DIV'
     O                                           31 'EMPLOYER NAME'
     O                                           68 'EFF DATE'
     O                                           78 'CNX DATE'
     O                                           89 'WATCH LIST'
      *
     O          E            SHEAD          1
     O               15                          12 'EXP MOD 1'
     O               15                          22 'EFF DATE'
     O               15                          34 'EXP MOD 2'
     O               15                          44 'EFF DATE'
     O               15                          56 'EXP MOD 3'
     O               15                          66 'EFF DATE'
     O               15                          79 'POLICY'
     O                                          101 'PREMIUM'
     O                                          114 'OPEN'
     O                                          128 ' TOTAL'
     O                                          150 'TOTAL INCURRED'
     O                                          173 'TOTAL PAID'
     O                                          183 'LOSS'
     O                                          194 'FREQ'
     O          E            SHEAD          2
     O               15                          79 'YEAR'
     O                                          115 'TO DATE        CLAIMS'
     O                                          128 'CLAIMS'
     O                                          195 'RATIO      RATIO'
      *
      *
     O          E   N21      STOTL3      1  3
     O                                           15 ' TOTAL MEMBERS:'
     O                       TTLEMP        J     25
     O               22                          56 '  * COUNTY'
     O               23                          56 ' * CHAPTER'
     O               24                          56 '  * REGION'
     O               25                          56 '   * AGENT'
     O***** BEG ADD ******* 09/22/06 **********************************
     O               26                          56 '* SIC CODE'
     O***** END ADD ******* 09/22/06 **********************************
     O                                           75 'CUMULATIVE TOTAL *'
     O                       L3PREM        J    102
     O                       L3OPEN        3    115
     O                       L3TOTC        3    128
     O                       L3ITOT        J    151
     O                       L3PTOT        J    174
     O                       L3LOSS        J    185
     O                       L3FREQ        J    196
      *
      *
     O          E            SYRTOT         1
     O                                           60 'POLICY YEAR'
     O                       XYEAR         Z   +001
     O                                           73 'TOTALS:'
     O                       CPREM         J    102
     O                       CTOTO         3    115
     O                       CTOTC         3    128
     O                       CINC          J    151
     O                       CTPD          J    174
     O                       YRLOSS        J    185
     O                       YRFREQ        J    196
      *
      *
     O          E            STOTL4         1
     O                                           15 ' TOTAL MEMBERS:'
     O                       LRTLEP        J     25
     O                                           73 ' GROUP TOTALS:'
     O                       LRPREM        J    102
     O                       LROPEN        3    115
     O                       LRTOTC        3    128
     O                       LRITOT        J    151
     O                       LRPTOT        J    174
     O                       LRLOSS        J    185
     O                       LRFREQ        J    196
      *
      *
     O          E            SNOREC         1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             124
     O                       DSDATE        Y    196
      *
     O          E            SNOREC         1
     O                       Q1PGM               10
     O                       STITL              129
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
      *
     O          E            SNOREC         3
     O                                            6 'GROUP:'
     O                       XXFND         Z      9
     O                       XFNAME              70
     O                                          100 'SORT SELECTION:'
     O                       XXSRTN             114
     O                                          144 'RUN DATE:'
     O                       XXDATE        Y    153
      *
     O          E            SNOREC      2
     O                                           25 '*** NO RECORDS MEET'
     O                                           47 'SELECTED CRITERIA ***'
      *
      *
      ***** END ADD ******* 04/01/01 **********************************
**  TABLE NM
EMPLOYER
COUNTY
CHAPTER
REGION
AGENT #
SIC CODE
