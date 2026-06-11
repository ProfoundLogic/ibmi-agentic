     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE APB041 - PRINT ACCOUNTS PAYABLE CHECKS                  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF OPEN QUERY REMARKS DETAIL FILE    *
     H*       60      GENERAL USAGE (CHAIN)                           *
      *****************************************************************
     FAPCSL3    UF   E           K DISK
     FAPBKL1    UF   E           K DISK
     FAPADL1    IF   E           K DISK
     FAPVML1    IF   E           K DISK
     FBKCRL2    UF A E           K DISK
     FCHECKS    O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

0054 D NUM             S              1    DIM(170) CTDATA PERRCD(80)           NUMBER DESCR ARR.
0055 D UNIT            S              3  0 DIM(10) CTDATA PERRCD(10)            UNITS INDEXES.
0056 D TEEN            S              3  0 DIM(11) CTDATA PERRCD(11)            13-19TEEN INDEXS.
0057 D TEN             S              3  0 DIM(9) CTDATA PERRCD(9)              TENS INDEXES.
0058 D PT70            S              1    DIM(70) CTDATA PERRCD(70)            70X1-CHAR PRT ARR
0008 F*
0009 D*                         **************************************
0010 D*                         * NAME CONCATENATION ARRAYS          *
0011 D*                         **************************************
0140 D*                         *************************************
0141 D*                         * DEFINE THE DATA STRUCTURE USED    *
0142 D*                         * FOR NAME CONCATENATION.           *
0143 D*                         *************************************
0144 D                 DS
0145 D  XXLNAM                 1     38
0146 D  XXL                    1     38
     D                                     DIM(38)
0147 D  XXFNAM                39     76
0148 D  XXF                   39     76
     D                                     DIM(38)
0149 D  XXCNAM                77    152
0150 D  XXCN                  77    152
     D                                     DIM(76)
     F*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE CHECK RECONCILIATION FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXAPP             2
     C                   KFLD                    XXBNK
     C                   KFLD                    SCCHK#
     C*
     C*   KEY LIST FOR THE VOUCHER FILE
     C*
     C     ADKEY         KLIST
     C                   KFLD                    CSCO#
     C                   KFLD                    CSFAC#
     C                   KFLD                    CSVDR
     C                   KFLD                    CSSEQ1
     C*
     C*   DEFINE THE PARM LIST.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXBNK             3 0
     C                   PARM                    XXCHKD            8 0
     C                   PARM                    XXCHK1
     C                   PARM                    XXVOID            1
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
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  FIRST CYCLE - INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CSCHK#        XXCHK1
     C     *LIKE         DEFINE    CSCHK#        SCCHK#
     C     *LIKE         DEFINE    CSCHK#        XXCHK#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      'AP'          XXAPP
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*  ASSIGN FIRST CHECK NUMBER
     C*
     C     XXBNK         CHAIN     PBK01                              60
     C                   ADD       1             BKLCHK
     C                   UPDATE    PBK01
     C                   MOVE      BKLCHK        XXCHK#
     C*
     C*** POSITION IN FILE
     C*
     C     XXBNK         SETLL     PCS01
     C*
     C*** BEGIN READING RECORDS
     C*
     C     READ          TAG
     C     XXBNK         READE     PCS01                                  50
     C*
     C     *IN50         IFEQ      '0'
     C*
     C*** FIRST CHECK FOR RESTART
     C*
     C     XXCHK1        IFGT      *ZERO
     C     CSCHK#        ANDNE     *ZERO
     C     CSCHK#        IFLT      XXCHK1
     C                   GOTO      READ
     C                   ELSE
     C                   MOVE      CSCHK#        SCCHK#
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C     XXVOID        IFEQ      'Y'
     C                   MOVE      'V'           CRCLR
     C                   MOVE      Q1USER        CRCUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCDT
     C*
     C                   Z-ADD     DSTIME        CRCTIM
     C                   UPDATE    PCR01
     C                   ELSE
     C                   DELETE    PCR01
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*  IF SEPARATE CHECK, FORCE LEVEL BREAK
     C*
     C     CSSEP         IFEQ      'Y'
     C                   MOVE      *ZEROS        XXVDR
     C                   END
     C*
     C*  LEVEL BREAK ON VENDOR - OUTPUT LAST CHECK
     C*
     C     CSVDR         IFNE      XXVDR
     C*
     C     FIRST         IFNE      *BLANK
     C                   EXSR      PRINT
     C                   END
     C*
     C                   MOVE      CSVDR         XXVDR             7 0
     C                   MOVE      CSCO#         XXCO#             3 0
     C                   MOVE      CSFAC#        XXFAC#            3 0
     C                   MOVE      *BLANKS       XXADD3           40
     C     CSVDR         CHAIN     PVM01                              98
     C   98              MOVE      *BLANKS       VMNAME
     C   98              MOVE      *BLANKS       VMADR1
     C   98              MOVE      *BLANKS       VMADR2
     C   98              MOVE      *BLANKS       VMCITY
     C   98              MOVE      *BLANKS       VMST
     C   98              MOVE      *BLANKS       VMZIP1
     C   98              MOVE      *BLANKS       VMZIP2
     C   98              MOVE      *BLANKS       VMHOLD
     C*
     C*  IF VENDOR IS ON HOLD, DO NOT PRINT CHECKS
     C*
     C     VMHOLD        IFEQ      'Y'
     C                   GOTO      READ
     C                   END
     C*
     C                   MOVEL     VMNAME        XXNAME           40
     C                   MOVEL     VMADR1        XXADD1           40
     C                   MOVE      VMADR1        XXADD2           40
     C                   MOVEL     VMCITY        XXLNAM
     C                   MOVE      *BLANKS       XXFNAM
     C                   MOVEL     VMST          XXST              8
     C                   MOVE      VMZIP1        XXST
     C     VMZIP2        IFGT      *BLANKS
     C                   MOVEL     '-'           XXZIP             5
     C                   MOVE      VMZIP2        XXZIP
     C                   MOVEL     XXST          XXST13           13
     C                   MOVE      XXZIP         XXST13
     C                   MOVEL     XXST13        XXFNAM
     C                   ELSE
     C                   MOVEL     XXST          XXFNAM
     C                   END
     C                   EXSR      AUFLN
     C     VMADR2        IFGT      *BLANKS
     C                   MOVEL     VMADR2        XXADD2
     C                   MOVEL     XXCNAM        XXADD3
     C                   ELSE
     C                   MOVEL     XXCNAM        XXADD2
     C                   END
     C*
     C                   EXCEPT    HEAD
     C*
     C*  CHECK FOR OVERFLOW
     C*
     C                   ELSE
     C     COUNT         IFGE      12
     C                   EXSR      VOID
     C                   END
     C*
     C                   END
     C*
     C*  IF SEPARATE CHECK, FORCE LEVEL BREAK FOR NEXT CHECK
     C*
     C     CSSEP         IFEQ      'Y'
     C                   MOVE      *ZEROS        XXVDR
     C                   END
     C*
     C*  PRINT STUB DETAIL
     C*
     C     ADKEY         CHAIN     PAD01                              60
     C   60              GOTO      READ
     C                   MOVE      'Y'           FIRST             1
     C                   ADD       1             COUNT             5 0
     C                   Z-ADD     CSAMT         XXGRS            11 2
     C                   Z-ADD     CSDISC        XXDISC           11 2
     C     CSAMT         SUB       CSDISC        XXNET            11 2
     C                   ADD       XXNET         XXCHKA           11 2
     C                   ADD       XXGRS         TTGRS            11 2
     C                   ADD       XXDISC        TTDISC           11 2
     C*
     C                   Z-ADD     ADINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   Z-ADD     ADDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDUED            8 0
     C*
0085 C                   MOVEL     CSINV#        XXINV#            7
     C                   EXCEPT    DET
     C*
     C*  UPDATE DETAIL RECORD
     C*
     C                   Z-ADD     XXCHK#        CSCHK#
     C*
     C                   Z-ADD     XXCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSCHKD
     C*
     C                   Z-ADD     XXCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1CHKD            8 0
     C*
     C                   UPDATE    PCS01
     C*
     C*  READ NEXT RECORD
     C*
     C                   GOTO      READ
     C                   END                                                    *IN50 = 0
     C*
     C*  OUTPUT LAST CHECK
     C*
     C                   EXSR      PRINT
     C*
     C*** END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C/COPY AUFLN
     O*************************************************************************
     C* PRINT CHECK
     O*************************************************************************
     C*
     C     PRINT         BEGSR
     C*
     C*  WRITE CHECK RECONCILIATION
     C*
     C                   MOVE      'AP'          XXAPP
     C                   Z-ADD     XXCHK#        SCCHK#
     C     CRKEY         CHAIN     PCR01                              40
     C*
     C                   MOVE      'AP'          CRAPP
     C                   MOVE      XXVDR         CRVDR
     C                   MOVE      XXBNK         CRBNK#
     C                   MOVE      XXCO#         CRCO#
     C                   MOVE      XXFAC#        CRFAC#
     C*
     C                   Z-ADD     XXCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCHKD
     C*
     C                   Z-ADD     XXCHKA        CRAMT
     C                   Z-ADD     *ZERO         CRSS#
     C                   MOVE      *BLANK        CRCLR
     C     XXCHKA        IFEQ      *ZERO
     C                   MOVE      'V'           CRCLR
     C                   END
     C                   Z-ADD     XXCHK#        CRCHK#
     C*
     C                   MOVE      Q1USER        CRCUSR
     C                   MOVE      Q1PGM         CRIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCDT
     C*
     C                   Z-ADD     DSTIME        CRCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCR01
     C                   ELSE
     C                   MOVE      Q1USER        CRFUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRIDT
     C*
     C                   Z-ADD     DSTIME        CRITIM
     C                   WRITE     PCR01
     C                   END
     C*
     C*  DO NOT WRITE CHECK IF AMOUNT IS LESS/EQUAL ZERO
     C*
     C     XXCHKA        IFLE      *ZERO
     C                   EXSR      VOID
     C                   GOTO      NOCHK
     C                   END
     C*
     C                   EXCEPT    TOT
     C*
     C*  GET CHECK AMOUNT IN WORDS
     C*
     C                   Z-ADD     XXCHKA        CRAMT
     C                   Z-ADD     XXCHKA        DOLLAR            9 0
     C                   MOVE      XXCHKA        CENTS             2 0
     C                   EXSR      WORDS
     C*
     C                   EXCEPT    CHK
     C*
     C*  ASSIGN NEXT CHECK NUMBER
     C*
     C     XXBNK         CHAIN     PBK01                              60
     C                   ADD       1             BKLCHK
     C                   UPDATE    PBK01
     C                   MOVE      BKLCHK        XXCHK#
     C*
     C*  CLEAR TOTALS
     C*
     C     NOCHK         TAG
     C                   Z-ADD     *ZERO         XXCHKA
     C                   Z-ADD     *ZERO         TTDISC
     C                   Z-ADD     *ZERO         TTGRS
     C                   Z-ADD     *ZERO         COUNT
     C*
     C                   ENDSR
     O*************************************************************************
     C* VOID CHECK
     O*************************************************************************
     C*
     C     VOID          BEGSR
     C*
     C     XXCHKA        IFLE      *ZERO
     C                   EXCEPT    TOT
     C                   Z-ADD     *ZERO         XXCHKA
     C                   END
     C                   EXCEPT    PVOID
     C*
     C*  ASSIGN NEXT CHECK NUMBER
     C*
     C     XXBNK         CHAIN     PBK01                              60
     C                   ADD       1             BKLCHK
     C                   UPDATE    PBK01
     C                   MOVE      BKLCHK        XXCHK#
     C*
     C                   Z-ADD     *ZERO         COUNT
     C                   ENDSR
0603 C********************************************************************
0604 C* WORDS  * TRANSALATE A NUMERIC VALUE TO PRINTED WORDS FOR CHECK $ *
0607 C********************************************************************
0608 C     WORDS         BEGSR
0609 C                   Z-ADD     XXCHKA        CHQDOL            9 0          $$$,$$$,$$$
0610 C                   Z-ADD     XXCHKA        TWO2              2 2          .CC
0611 C                   MOVE      TWO2          CHQCNT            2
0612 C                   MLLZO     '1'           CHQCNT
0624 C                   MOVE      *BLANKS       XXZER             1
0624 C                   MOVE      'X'           XXZER1            1
0613 C                   MOVE      *BLANKS       XXFULL            1
0614 C                   MOVE      '*'           PT70                           PROPAGATE/FILL
0616 C                   Z-ADD     1             P                 2 0          PRT ARR POINTER
0617 C                   MOVE      CHQDOL        NUMER             9 0          SAVE ORIGINAL
0618 C                   MOVEL     NUMER         DIGIT             1 0          $--,---,---
0620 C*
0620 C* FIRST DO HUNDRED MILLION
0620 C*
0619 C     DIGIT         IFLE      *ZERO                                        DIGIT 1-9
0620 C                   GOTO      XTRN1
0620 C                   END
0621 C                   EXSR      UNITS
0622 C                   MOVEA     'HUNDRED '    PT70(P)
0623 C                   ADD       8             P
0624 C                   MOVE      *BLANKS       XXZER1
0620 C*
0620 C* TEN MILLIONS
0620 C*
0625 C     XTRN1         TAG
0626 C                   MULT      10            NUMER
0627 C                   MOVEL     NUMER         DIGIT                          -$-,---,---
0628 C*
0628 C                   MOVE      *BLANKS       XXAND
0628 C                   MOVE      *BLANKS       XXIND
0628 C     P             IFGT      1
0628 C                   MOVE      'X'           XXIND             1
0629 C     DIGIT         IFGT      *ZERO
0628 C                   MOVE      'X'           XXAND             1
0630 C                   MOVEA     'AND '        PT70(P)
0631 C                   ADD       4             P
0631 C                   END
0631 C                   END
0629 C     DIGIT         IFGT      1
0632 C                   EXSR      TENS
0631 C                   END
0629 C     DIGIT         IFLT      1
0632 C                   MOVE      'X'           XXZER             1
0631 C                   ELSE
0632 C                   MOVE      *BLANK        XXZER
0631 C                   END
0629 C     DIGIT         IFEQ      1
0632 C                   MOVE      'X'           XXTEEN            1
0631 C                   ELSE
0632 C                   MOVE      *BLANK        XXTEEN
0631 C                   END
0620 C*
0620 C* MILLIONS
0620 C*
0633 C                   MULT      10            NUMER
0634 C                   MOVEL     NUMER         DIGIT                          --$,---,---
0635 C     DIGIT         IFGT      *ZERO
0635 C     XXZER1        IFEQ      ' '
0635 C     XXAND         ANDNE     'X'
0636 C                   MOVEA     'AND '        PT70(P)
0637 C                   ADD       4             P
0638 C                   END
0638 C     XXTEEN        IFEQ      'X'
0638 C                   EXSR      TEENS
0638 C                   ELSE
0638 C     DIGIT         IFGT      *ZERO
0639 C                   EXSR      UNITS
0638 C                   END
0638 C                   END
0638 C                   END
0638 C*
0638 C*  IF NO MILLIONS WERE FOUND, GO ON TO THOUSANDS
0638 C*
0640 C     XXZER         IFEQ      'X'
0642 C     XXZER1        ANDEQ     'X'
0641 C     DIGIT         ANDEQ     *ZERO
0643 C                   GOTO      DOTHOU
0643 C                   END
0643 C*
0643 C                   MOVEA     'MILLION '    PT70(P)
0644 C                   ADD       8             P
0620 C*
0620 C* HUNDRED THOUSANDS
0620 C*
0645 C     DOTHOU        TAG
0645 C                   MOVE      *BLANKS       XXZER
0645 C                   MOVE      'X'           XXZER1
0628 C                   MOVE      *BLANKS       XXAND
0646 C                   MULT      10            NUMER
0647 C                   MOVEL     NUMER         DIGIT                           ---,$--,---
0648 C     DIGIT         IFLE      0
0649 C                   GOTO      XTRN2
0649 C                   END
0650 C                   EXSR      UNITS
0651 C                   MOVEA     'HUNDRED '    PT70(P)
0652 C                   ADD       8             P
0652 C                   MOVE      *BLANKS       XXZER1
0620 C*
0620 C* TEN THOUSANDS
0620 C*
0654 C     XTRN2         TAG
0655 C                   MULT      10            NUMER
0656 C                   MOVEL     NUMER         DIGIT                           ---,-$-,---
0628 C                   MOVE      *BLANKS       XXIND
0628 C     P             IFGT      1
0628 C                   MOVE      'X'           XXIND             1
0629 C     DIGIT         IFGT      *ZERO
0628 C                   MOVE      'X'           XXAND
0630 C                   MOVEA     'AND '        PT70(P)
0631 C                   ADD       4             P
0631 C                   END
0631 C                   END
0629 C     DIGIT         IFGT      1
0632 C                   EXSR      TENS
0631 C                   END
0629 C     DIGIT         IFLT      1
0632 C                   MOVE      'X'           XXZER             1
0631 C                   ELSE
0632 C                   MOVE      *BLANK        XXZER
0631 C                   END
0629 C     DIGIT         IFEQ      1
0632 C                   MOVE      'X'           XXTEEN            1
0631 C                   ELSE
0632 C                   MOVE      *BLANK        XXTEEN
0631 C                   END
0620 C*
0620 C* THOUSANDS
0620 C*
0633 C                   MULT      10            NUMER
0634 C                   MOVEL     NUMER         DIGIT                          ---,--$,---
0635 C     DIGIT         IFGT      *ZERO
0635 C     XXZER1        IFEQ      ' '
0635 C     XXAND         ANDNE     'X'
0636 C                   MOVEA     'AND '        PT70(P)
0637 C                   ADD       4             P
0638 C                   END
0638 C     XXTEEN        IFEQ      'X'
0638 C                   EXSR      TEENS
0638 C                   ELSE
0638 C     DIGIT         IFGT      *ZERO
0639 C                   EXSR      UNITS
0638 C                   END
0638 C                   END
0638 C                   END
0638 C*
0638 C*  IF NO THOUSANDS WERE FOUND, GO ON TO HUNDREDS
0638 C*
0640 C     XXZER         IFEQ      'X'
0642 C     XXZER1        ANDEQ     'X'
0641 C     DIGIT         ANDEQ     *ZERO
0643 C                   GOTO      DOHUND
0643 C                   END
0643 C*
0643 C                   MOVE      'X'           XXTHOU            1
0643 C                   EXSR      THOUS
0643 C                   MOVE      *BLANKS       XXTHOU
0620 C*
0620 C* HUNDREDS
0620 C*
0673 C     DOHUND        TAG
0674 C                   MULT      10            NUMER
0675 C                   MOVEL     NUMER         DIGIT                           ---,---,$--
0676 C     DIGIT         IFGT      0
0677 C                   EXSR      UNITS
0678 C                   EXSR      THOUS
0678 C                   END
0678 C*
0679 C     P             IFGT      1
0680 C     DIGIT         IFLE      *ZERO
0680 C     NUMER         MULT      10            TEMP              9 0
0681 C     TEMP          IFNE      *ZERO
0681 C                   MOVEA     'AND '        PT70(P)
0682 C                   ADD       4             P
0682 C                   END
0682 C                   END
0682 C                   END
0620 C*
0620 C* TENS
0620 C*
0684 C                   MULT      10            NUMER
0685 C                   MOVEL     NUMER         DIGIT                           ---,---,-$-
0687 C                   MOVE      *BLANKS       XXONE             1
0686 C     DIGIT         IFGT      1
0687 C                   EXSR      TENS
0687 C                   ELSE
0686 C     DIGIT         IFEQ      1
0687 C                   MOVE      'X'           XXONE
0687 C                   END
0687 C                   END
0620 C*
0620 C* ONES
0620 C*
0688 C                   MULT      10            NUMER
0689 C                   MOVEL     NUMER         DIGIT                           ---,---,--$
0691 C     XXONE         IFEQ      'X'
0691 C                   EXSR      TEENS
0691 C                   ELSE
0691 C     DIGIT         IFGT      *ZERO
0692 C                   EXSR      UNITS
0691 C                   END
0691 C                   END
0693 C                   EXSR      DOLL                                         FOR CC/100 DOLLARS
0694 C                   ENDSR
0695 C********************************************************************
0696 C* UNITS  * EXAMINE THE UNITS POSITION...                           *
0697 C********************************************************************
0698 C     UNITS         BEGSR
0699 C                   Z-ADD     DIGIT         K                 3 0
0700 C                   Z-ADD     UNIT(K)       LO                3 0
0701 C                   ADD       1             K
0702 C                   Z-ADD     UNIT(K)       HI                3 0
0703 C                   EXSR      MOVE
0704 C                   ENDSR
0705 C********************************************************************
0706 C* TEENS  * EXAMINE THE TEENS POSITION...                           *
0707 C********************************************************************
0708 C     TEENS         BEGSR
0709 C                   Z-ADD     DIGIT         K
0710 C                   ADD       1             K
0711 C                   Z-ADD     TEEN(K)       LO
0712 C                   ADD       1             K
0713 C                   Z-ADD     TEEN(K)       HI
0714 C                   EXSR      MOVE
0715 C                   ENDSR
0716 C********************************************************************
0717 C* TENS   * EXAMINE THE TENS POSITION...                            *
0718 C********************************************************************
0719 C     TENS          BEGSR
0720 C                   Z-ADD     DIGIT         DD                1 0
0721 C                   Z-ADD     TEN(DD)       HI
0722 C                   SUB       1             DD
0723 C                   Z-ADD     TEN(DD)       LO
0724 C                   EXSR      MOVE
0725 C                   ENDSR
0726 C********************************************************************
0727 C* THOUS  * EXAMINE THE THOUSAND POSTION...                         *
0728 C********************************************************************
0729 C     THOUS         BEGSR
0730 C     XXTHOU        IFEQ      'X'
0730 C                   Z-ADD     153           LO                               'THOUSAND'
0731 C                   Z-ADD     161           HI
0734 C                   EXSR      MOVE
0731 C                   ELSE
0732 C                   Z-ADD     161           LO                               'HUNDRED'
0733 C                   Z-ADD     168           HI
0734 C                   EXSR      MOVE
0735 C     NUMER         MULT      10            TEMP                     84     NO MORE DIGITS
0736 C     TEMP          IFNE      *ZERO
0736 C                   Z-ADD     168           LO                              'AND'
0737 C                   Z-ADD     171           HI
0738 C                   EXSR      MOVE
0731 C                   END
0731 C                   END
0739 C                   ENDSR
0740 C********************************************************************
0741 C* DOLL   * EXAMINE CENTS FOR PENNIES...                            *
0742 C********************************************************************
0743 C     DOLL          BEGSR
0744 C                   Z-ADD     56            P
0745 C                   MOVEA     ' '           PT70(P)
0746 C                   ADD       1             P
0747 C                   MOVEA     CHQCNT        PT70(P)                        CC.....CENTS
0748 C                   ADD       2             P
0749 C                   MOVEA     '/100 '       PT70(P)                        CC/100
0750 C                   ADD       5             P
0751 C                   MOVEA     'DOLLARS'     PT70(P)                        CC/100 DOLLARS
0752 C                   ENDSR
0753 C********************************************************************
0754 C* MOVE   * POSITION THE CURSOR...                                  *
0755 C********************************************************************
0756 C     MOVE          BEGSR
0757 C     XXFULL        IFEQ      'X'
0757 C                   GOTO      XMOV3                                        EXIT IMMED
0757 C                   END
0758 C     XMOV1         TAG
0759 C     P             IFEQ      *ZERO
0760 C                   GOTO      XMOV2
0760 C                   END
0761 C                   MOVE      NUM(LO)       PT70(P)                                    ARR
0762 C                   ADD       1             LO
0763 C                   ADD       1             P
0764 C     LO            IFNE      HI
0765 C                   GOTO      XMOV1
0765 C                   END
0766 C     P             IFGT      55                                                       AY
0766 C                   MOVE      'X'           XXFULL                                     AY
0766 C                   END                                                                AY
0766 C     XXFULL        IFNE      'X'                                                      AY
0767 C                   MOVE      ' '           PT70(P)
0768 C                   ADD       1             P
0769 C                   GOTO      XMOV3
0766 C                   END                                                                AY
0770 C     XMOV2         TAG
0772 C                   MOVE      '*'           PT70                           * PROPAGATE/FILL
0773 C     XMOV3         ENDSR
     O*************************************************************************
     OCHECKS    E            HEAD           2 02
     O                       XXCHK#        Z     67
00640O                       CSVDR         Z      7
00640O                       XXNAME              48
0065 O          E            DET            1
     O                       XXINV#              10
     O                       XXDATE        YB    21
     O                       XXDUED        YB    31
0074 O                       XXGRS         MB    44
0074 O                       XXDISC        MB    59
0073 O                       XXNET         MB    76
     O          E            TOT            1 19
0073 O                       XXCHKA        M     73
0074 O                       TTDISC        MB    56
0074 O                       TTGRS         MB    41
0084 O          E            PVOID          1 28
00850O                                           35 '     VOID VOID VOID VOID'
0086 O                                           60 'VOID VOID VOID VOID VOID'
00870O                                           80 'VOID VOID VOID VOID'
     O          E            CHK            1 28
00790O                       X1CHKD        Y     20
00810O                       DOLLAR        2     67 '*'
00820O                                           68 '.'
00830O                       CENTS               70
     O          E            CHK            1 30
     O                       PT70                76
     O          E            CHK            1 35
     O                       XXNAME              50
     O          E            CHK            1
     O                       XXADD1              50
     O          E            CHK            1
     O                       XXADD2              50
     O          E            CHK            1
     O                       XXADD3              50
**  NUMBER DESCRIPTION ARRAY TO PRT AMT IN WORDS
ONETWOTHREEFOURFIVESIXSEVENEIGHTNINETENELEVENTWELVETHIRTEENFOURTEENFIFTEENSIXTEE
NSEVENTEENEIGHTEENNINETEENTWENTYTHIRTYFORTYFIFTYSIXTYSEVENTYEIGHTYNINETYTHOUSAND
HUNDREDAND
**  UNITS INDEX
001004007012016020023028033037                                    UNITS INDEXES.
**  13 THRU 19 INDEX
037040046052060068075082091099107                             13-19TEEN INDEXES.
**  TENS INDEX
107113119124129134141147153                                        TENS INDEXES.
** BLANK ARRAY FOR WORDS
                                                                      70X1-CHAR.
