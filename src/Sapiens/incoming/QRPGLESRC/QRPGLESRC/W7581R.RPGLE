     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7581R - Build Work File for Self-Rpt Emp Agent Late Notic
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/26/00 C:61  SUPPORT 23685                                  *
     H*                Skip emp if eff dt (emp master) >= first day   *
     H*                of run date month.                             *
     H*                                                               *
     H*  12/16/99  C:87  PROJECT: SUPP Y2KLOG #72                     *
     H*                  Run date was being change when it should not *
     H*                  have been.                                   *
     H*                                                               *
     H*  11/13/98  C:33  SEQ#: WA090/14584                            *
     H*                  CORRECT THE CYYMM LOGIC.                     *
     H*                                                               *
     H*  10/10/95 C:11 PROJECT WC453                                  *
     H*                GET THE LAST ACTIVE AGENT, AS OF THE FYR, NOT  *
     H*                AS OF THE RUN DATE                             *
     H*****************************************************************
     F*
     F* Agent Late Notice Work File - Output
     FWXL1L     O    E             DISK
     F*
     F* Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F* Employer Detail File
     FWDELL     IF   E           K DISK
     F*
     F* Table File - Billing Schedule Code File
     FWTABL     IF   E           K DISK
     F*
     F* Self Reporting Payroll Transaction File
     FWASRL2    IF   E           K DISK
     F*
     F*  Agent History Master File
     F*
     FWMAHL     IF   E           K DISK
     F*

     D*
     D* Define Tables, Arrays and Data Structures
     D*
     D*
     D* Systems Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Employer's Effective Dates
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        Effect Date
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D* Employer's Cancel Dates
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        Cancel Date
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D* Run Date
     D                 DS
     D  XXDATE                 1      7  0
     D  XXYM                   1      5  0
     D*
     D* Work Fields for Date
     D                 DS
     D  ZZYM                   1      5  0
     D***  BEGIN CHG  ***  11/13/98  *********************************
     D**********                              1   20XXYR
     D**********                              3   40XXMN
     D  XXYR                   1      3  0
     D  XXMN                   4      5  0
     D***  END   CHG  ***  11/13/98  *********************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR             3 0
     C*
     C* Key List for the Billing Schedule Code File
     C*
     C     BSKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBLSC           10
     C*
     C* Key list for the Self-Reporting Payroll Transaction File
     C*
     C     SRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C***  BEGIN CHG  ***  12/16/99  **********************************
     C*******************  KFLD           XXYM
     C                   KFLD                    KKYM              5 0
     C***  END   CHG  ***  12/16/99  **********************************
     C*
     C* Key List for the Agent History Master File
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR                          LJQ 7-29-88
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDATE            8
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
     C* Set Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        X1DATE            8 0
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   TIME                    DSDTTM
      ***** BEG ADD ******* 07/26/00 **********************************
      *
      * Calc date - 1st date of run date month
      *
     C                   Z-ADD     *ZERO         XX1STD            7 0
     C                   Z-ADD     *ZERO         XXTMP1            5 0
     C                   MOVEL     XXDATE        XXTMP1
     C                   MOVEL     XXTMP1        XX1STD
     C                   ADD       1             XX1STD
      *
      ***** END ADD ******* 07/26/00 **********************************
     C*
     C* Take date and subtract 1 month to see if previous month's
     C* bill has been paid
     C*
     C                   Z-ADD     XXYM          ZZYM
     C     XXMN          IFEQ      1
     C                   Z-ADD     12            XXMN
     C                   SUB       1             XXYR                           JJJ 1-25-89
     C                   ELSE
     C                   SUB       1             XXMN
     C                   END
     C***  BEGIN CHG  ***  12/16/99  **********************************
     C*******************  Z-ADDZZYM      XXYM
     C                   Z-ADD     ZZYM          KKYM
     C***  END   CHG  ***  12/16/99  **********************************
     C*
     C* Start Reading the Employer Master File
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C* Get the Employer Detail Record
     C*
     C     ELKEY         CHAIN     PEL01                              61
     C     *IN61         IFEQ      '0'
     C*
     C* Check to see if Employer Is Cancelled
     C*
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C* Check to see if Employer is Effective After Run Date
     C*
     C     ED(Y)         IFNE      0
      ***** BEG CHG ******* 07/26/00 **********************************
     C********** ED,Y      ANDGTXXDATE
     C     ED(Y)         ANDGE     XX1STD
      ***** END CHG ******* 07/26/00 **********************************
     C                   GOTO      READE
     C                   END
     C*
     C     Y             DOWNE     0
     C     ELPTDT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C  N60              END
     C*
     C     CD(Y)         IFNE      0
     C     CD(Y)         ANDLT     XXDATE
     C                   GOTO      READE
     C                   END
     C*
     C* Get Billing Schedule, ONLY Process 'S' (Self-Reporting) Types
     C*
     C                   MOVEL     ELBSCD        XXBLSC
     C     BSKEY1        CHAIN     PBS01                              62
     C     BSSEFL        IFEQ      'S'
     C*
     C* Get Payroll Transaction Record
     C*
     C     SRKEY         CHAIN     PSR01                              63
     C*
     C* Write to Work File if we did not get a hit
     C*
     C     *IN63         IFEQ      '1'
     C*
     C* Clear the Fields in the Work File
     C*
     C                   Z-ADD     0             L1CO#
     C                   Z-ADD     0             L1FND
     C                   Z-ADD     0             L1AGT#
     C                   Z-ADD     0             L1PROD
     C                   Z-ADD     0             L1EMP#
     C                   Z-ADD     0             L1DIV
     C                   MOVE      *BLANKS       L1STAT
     C                   MOVE      *BLANKS       L1USER
     C                   MOVE      *BLANKS       L1PGM
     C                   Z-ADD     0             L1TIME
     C                   Z-ADD     0             L1DT
     C*
     C* Move Information to Work File Fields
     C*
     C                   Z-ADD     EMCO#         L1CO#
     C                   Z-ADD     EMFND         L1FND
     C                   Z-ADD     EMEMP#        L1EMP#
     C                   Z-ADD     EMDIV         L1DIV
     C*
     C* Get the Agent # and Producer #
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  60
     C*
     C*  GET ACTIVE AGENT          JJJ ADDED CODE 3-9-90
     C*
     C     *IN60         DOWEQ     '0'                                           JJJ 3-9-90
     C     AHSTAT        ANDEQ     'I'                                           |
     C                   READP     PAH01                                  60     |
     C                   END                                                     |
     C*                                                     |
     C     *IN60         IFEQ      '1'                                           |
     C     AHCO#         ORNE      XXCO#
     C     AHFND         ORNE      XXFND
     C     AHEMP#        ORNE      EMEMP#
     C     AHDIV         ORNE      EMDIV
     C     AHFYR         ORNE      XXFYR                                        LJQ 7-29-88
     C                   Z-ADD     0             L1PROD
     C                   Z-ADD     0             L1AGT#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     AHPROD        L1PROD
     C                   Z-ADD     AHAGT#        L1AGT#
     C*
     C                   END                                                    Agnt get
     C*
     C                   MOVE      *BLANKS       L1STAT            1
     C                   MOVE      Q1USER        L1USER           10
     C                   MOVE      Q1PGM         L1PGM            10
     C                   MOVE      DSTIME        L1TIME            6 0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         L1DT
     C*
     C* Write the Work Record
     C*
     C                   WRITE     PL101
     C*
     C                   END                                                    SR Rec Test
     C*
     C                   END                                                    Bill Schd Test
     C*
     C                   END                                                    Emp Det Rec
     C*
     C* Get the Next Employer
     C*
     C     READE         TAG
     C     EMKEY         READE     PEM01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
