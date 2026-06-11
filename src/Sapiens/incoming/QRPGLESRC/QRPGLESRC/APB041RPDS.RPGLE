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
     FAPCSL2    IF   E           K DISK
     F                                     RENAME(PCS01:PCS02)
     FAPBKL1    UF   E           K DISK
     FAPADL1    IF   E           K DISK
     FAPVML1    IF   E           K DISK
     FBKCRL2    UF A E           K DISK
     FCHECKS    O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

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
0143 D*
0144 D                 DS
0145 D  XXACCT                 1      9  0
0146 D  XXGLD                  1      3  0
0147 D  XXACT                  4      6  0
0148 D  XXSUB                  7      9  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

0144 IPCS02
0145 I              CSCO#                       C2CO#
     I              CSFAC#                      C2FAC#
     I              CSVDR                       C2VDR
     I              CSINV#                      C2INV#
     I              CSSEQ1                      C2SEQ1
     I              CSBNK#                      C2BNK#
     I              CSAMT                       C2AMT
     I              CSDISC                      C2DISC
     I              CSPODS                      C2PODS
     I              CSPART                      C2PART
     I              CSCHK#                      C2CHK#
     I              CSCHKD                      C2CHKD
     I              CSSEP                       C2SEP
     I              CSIPGM                      C2IPGM
     I              CSCTIM                      C2CTIM
     I              CSCDT                       C2CDT
     I              CSCUSR                      C2CUSR
     I              CSFUSR                      C2FUSR
     I              CSITIM                      C2ITIM
     I              CSIDT                       C2IDT
     I              CSSTAT                      C2STAT

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
     C     A2KEY         KLIST
     C                   KFLD                    C2CO#
     C                   KFLD                    C2FAC#
     C                   KFLD                    C2VDR
     C                   KFLD                    C2SEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER FILE
     C*
     C     X2KEY         KLIST
     C                   KFLD                    X2BNK             3 0
     C                   KFLD                    X2CHK#            7 0
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
     C                   MOVE      *ZEROS        XXGLD
     C                   MOVE      *ZEROS        XXACT
     C                   MOVE      *ZEROS        XXSUB
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
     C     FIRST         IFEQ      'Y'
     C     VMHOLD        ANDNE     'Y'
     C                   EXSR      PRINT
     C                   END
     C*
     C                   MOVE      'Y'           FIRST             1
     C                   MOVE      CSVDR         XXVDR             7 0
     C                   MOVE      *BLANKS       XXADD3           30
     C     CSVDR         CHAIN     PVM01                              98
     C   98              MOVE      *BLANKS       VMNAME
     C   98              MOVEL     '*INVALID'    VMNAME
     C   98              MOVE      *BLANKS       VMADR1
     C   98              MOVE      *BLANKS       VMADR2
     C   98              MOVE      *BLANKS       VMCITY
     C   98              MOVE      *BLANKS       VMST
     C   98              MOVE      *BLANKS       VMZIP1
     C   98              MOVE      *BLANKS       VMZIP2
     C   98              MOVE      *BLANKS       VMHOLD
     C*
     C                   MOVEL     VMNAME        XXNAME           30
     C                   MOVEL     VMADR1        XXADD1           30
     C                   MOVEL     VMADR2        XXADD2           30
     C                   MOVEL     VMCITY        XXLNAM
     C                   MOVE      VMGLD#        XXGLD
     C                   MOVE      VMACC#        XXACT
     C                   MOVE      VMSUB#        XXSUB
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
     C                   MOVEL     XXCNAM        XXADD3           30
     C                   ELSE
     C                   MOVEL     XXCNAM        XXADD2
     C                   END
     C*
     C                   END
     C*
     C*  IF VENDOR IS ON HOLD, DO NOT PRINT CHECKS
     C*
     C     VMHOLD        IFEQ      'Y'
     C     CSCHK#        IFGT      *ZERO                                        RE-PRINT
     C                   MOVE      *ZERO         CSCHK#
     C                   MOVE      *ZERO         CSCHKD
     C                   UPDATE    PCS01
     C                   END
     C                   GOTO      READ
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
     C     CSAMT         SUB       CSDISC        XXNET            11 2
     C                   ADD       XXNET         XXCHKA           11 2
     C*
     C*  UPDATE DETAIL RECORD
     C*
     C                   Z-ADD     XXCHK#        CSCHK#
     C*
     C                   Z-ADD     XXCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSCHKD
     C*
     C                   UPDATE    PCS01
     C*
     C*  WRITE CHECK RECONCILIATION
     C*
     C                   MOVE      'AP'          XXAPP
     C                   Z-ADD     XXCHK#        SCCHK#
     C     CRKEY         CHAIN     PCR01                              40
     C*
     C                   MOVE      'AP'          CRAPP
     C                   MOVE      CSVDR         CRVDR
     C                   MOVE      XXBNK         CRBNK#
     C                   MOVE      CSCO#         CRCO#
     C                   MOVE      CSFAC#        CRFAC#
     C*
     C                   Z-ADD     XXCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCHKD
     C*
     C                   Z-ADD     XXCHKA        CRAMT
     C                   Z-ADD     *ZERO         CRSS#
     C                   MOVE      *BLANK        CRCLR
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
     C*  READ NEXT RECORD
     C*
     C                   GOTO      READ
     C                   END                                                    *IN50 = 0
     C*
     C*** PRINT LAST CHECK
     C*
     C     XXCHKA        IFNE      *ZERO
     C                   EXSR      PRINT
     C                   END
     C*
     C*  BACK OUT UNUSED CHECK NUMBER
     C*
     C     XXBNK         CHAIN     PBK01                              60
     C                   SUB       1             BKLCHK
     C                   UPDATE    PBK01
     C                   MOVE      BKLCHK        XXCHK#
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
     C*  DO NOT WRITE CHECK IF AMOUNT IS LESS/EQUAL ZERO
     C*
     C     XXCHKA        IFLE      *ZERO
     C                   EXSR      VOID
     C                   GOTO      NOCHK
     C                   END
     C*
     C*  GET CHECK AMOUNT IN WORDS
     C*
     C                   Z-ADD     XXCHKA        CRAMT
     C                   Z-ADD     XXCHKA        DOLLAR            9 0
     C                   MOVE      XXCHKA        CENTS             2 0
     C                   MOVE      XXCHKA        QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       XXAMT1           60
     C                   MOVE      *BLANKS       XXAMT2           60
     C                   MOVE      QQAMT1        XXAMT1
     C                   MOVE      QQAMT2        XXAMT2
     C*                    EXSR WORDS
     C*
     C                   EXCEPT    CHK
     C                   EXCEPT    HEAD
     C*
     C*  PRINT STUB DETAIL
     C*
     C                   MOVE      XXBNK         X2BNK
     C                   MOVE      XXCHK#        X2CHK#
     C     X2KEY         SETLL     PCS02
     C                   Z-ADD     *ZERO         COUNT             5 0
     C     OVER2         TAG
     C     X2KEY         READE     PCS02                                  60
     C   60              GOTO      DONE2
     C     A2KEY         CHAIN     PAD01                              60
     C   60              GOTO      OVER2
     C                   ADD       1             COUNT             5 0
     C     C2AMT         SUB       C2DISC        XXNET            11 2
     C*
     C                   Z-ADD     ADINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   EXCEPT    DET
     C     COUNT         IFLT      12
     C                   GOTO      OVER2
     C                   ELSE
     C                   EXSR      VOID
     C                   GOTO      OVER2
     C                   END
     C     DONE2         TAG
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
     C                   Z-ADD     *ZERO         XXCHKA
     C                   END
     C                   EXCEPT    PVOID
     C                   EXCEPT    HEAD
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
     O*************************************************************************
0084 OCHECKS    E            PVOID          1 09
00850O                                           35 '     VOID VOID VOID VOID'
0086 O                                           60 'VOID VOID VOID VOID VOID'
00870O                                           80 'VOID VOID VOID VOID'
     O          E            CHK            1 06
00790O                       XXCHKD        Y     67
     O          E            CHK            1 09
00810O                       DOLLAR        2     78
00830O                       CENTS               82
     O                       XXAMT1              67
     O          E            CHK            1
     O                       XXAMT2              67
     O          E            CHK            1 13
     O                       XXNAME              43
     O          E            CHK            1
     O                       XXADD1              43
     O          E            CHK            1
     O                       XXADD2              43
     O          E            CHK            1
     O                       XXADD3              43
0065 O          E            HEAD           2 24
0065 O          E            DET            1
     O                       XXDATE        YB    11
     O                       XXACCT              23 '   -   -   '
     O                       C2PODS              46
0073 O                       XXNET         MB    84
