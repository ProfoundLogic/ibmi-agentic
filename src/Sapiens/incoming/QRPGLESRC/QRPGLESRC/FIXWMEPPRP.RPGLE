     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE FIXWMEPPRP -  COMPARISON OF WASRP DATA TO WMEPP DATA
     H*****************************************************************
     H* DESCRIPTION                                                   *
     H*  THIS REPORT COMPARES S/R PAYROLLS IN WASRP TO PAYROLLS IN    *
     H*  WMEPP.  IF DIFFERENCES ARE FOUND AN EXCEPTION RECORD IS      *
     H*  CREATED IN THE OUTPUT REPORT.                                *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name
     H*                                                               *
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDELL3    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMEPL1    IF   E           K DISK
     FWASRL5    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      /COPY SCOPYMT01D
     D*
     D TITLE           C                   CONST('S/R Payroll Discrepa-
     D                                     ncy Report (WASRP -
     D                                     vs. WMEPP)')
      ***** BEG DEL ***** 01/28/10 ************************************

     I***           'Progressive Data Sol-C         PDSNAM
     I***           'utions, Inc.'
     I*
     I***         DS
     I***                                     1  140DSDTTM
     I***                                     1   60DSTIME
     I***                                     7  140DSDATE
     I*
     I***PSDS      ESDSSZQ1P
      ***** END DEL ***** 01/28/10 ************************************
      ***** BEG ADD ***** 01/28/10 ************************************
     I*
     I***PY SCOPYMT01I
     I*
      ***** END ADD ***** 01/28/10 ************************************

     C****************************************************************
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    FNCO#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXDATE
     C*
     C     ELKY1         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  EMPLOYER SELF-REPORTING PAYROLL TRANS FILE
     C*
     C     SRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
     C                   KFLD                    EPSPEC
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    FNFND         SVFND
     C     *LIKE         DEFINE    XXEFDT        XXPFDT
     C     *LIKE         DEFINE    XXEFDT        XXPTDT
     C*_______________________________________________________________
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*_______________________________________________________________
     C*
     C*  INITIALIZE FIELDS
     C*
     C                   Z-ADD     *ZERO         XXEFDT            8 0
     C                   Z-ADD     *ZERO         XXMEPP           11 2
     C                   Z-ADD     *ZERO         XXASRP           11 2
     C                   Z-ADD     *ZERO         XXDIFF           11 2
     C                   Z-ADD     *ZERO         XTCNT             5 0
     C                   Z-ADD     *ZERO         X1CNT             5 0
     C                   Z-ADD     *ZERO         X1MEPP           11 2
     C                   Z-ADD     *ZERO         X1ASRP           11 2
     C                   Z-ADD     *ZERO         X1DIFF           11 2
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   SUB       20000         XXDATE                         GO BACK 2 YRS
     C*
     C                   MOVEL(P)  TITLE         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*_______________________________________________________________
     C*
     C*  Start of main loop.
     C*
     C                   READ      PFN01                                  60
     C*
     C                   MOVE      *BLANK        CONAME
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C     CONAME        IFEQ      *BLANK
     C                   MOVEL(P)  PDSNAM        CONAME
     C                   ENDIF                                                  CONAME=*BLANKS
     C*
     C***  Begin Chg  ***  01/28/10  **************************************
     C********             MOVELCONAME    ##DATA    P
     C                   CALL      'RTVBRND'                            99
     C                   PARM      'I'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C*
     C                   MOVEL(P)  #BRND1        ##DATA
     C***  End   Chg  ***  01/28/10  **************************************
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C*
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  PRINT GROUP TOTALS
     C*
     C     FNFND         IFNE      SVFND
     C                   Z-ADD     FNFND         SVFND
     C*
     C     X1CNT         IFNE      0
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT
     C                   ADD       X1CNT         XTCNT
     C                   Z-ADD     *ZERO         X1CNT
     C                   Z-ADD     *ZERO         X1MEPP
     C                   Z-ADD     *ZERO         X1ASRP
     C                   Z-ADD     *ZERO         X1DIFF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  READ THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          SETLL     PEL01
     C     ELKY1         READE     PEL01                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C     ELPFDT        IFLT      XXDATE
     C                   GOTO      NEXTEL
     C                   ENDIF
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT
     C*
     C*  ACCOUNT MUST BE AN S/R TYPE
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *ON
     C     BSSEFL        ORNE      'S'
     C                   GOTO      NEXTEL
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  GET THE PAYROLL INFO IN WMEPP
     C*
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  62
     C     *IN62         DOWEQ     *OFF
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     *ZERO         XXASRP
     C                   Z-ADD     EPSAPR        XXMEPP
     C*
     C*  GET THE EMPLR SELF-REPORTING PAYROLL TRANSACTION INFO
     C*
     C     SRKY          CHAIN     PSR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       SRAMTP        XXASRP
     C     SRKY          READE     PSR01                                  60
     C                   ENDDO
     C*
     C*  IF THE WMEPP INFO DOES NOT MATCH THE WASRP INFO, PRINT
     C*
     C     XXMEPP        IFNE      XXASRP
     C*
     C     XXASRP        SUB       XXMEPP        XXDIFF
     C*
     C                   MOVE      *BLANKS       XXEMNM
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL     EMNAME        XXEMNM           15
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             X1CNT
     C                   ADD       XXMEPP        X1MEPP
     C                   ADD       XXASRP        X1ASRP
     C                   ADD       XXDIFF        X1DIFF
     C                   ENDIF                                                  XXMEPP<>XXASRP
     C*
     C     NEXTEP        TAG
     C     EPKY          READE     PEP01                                  62
     C                   ENDDO
     C*
     C     NEXTEL        TAG
     C     ELKY1         READE     PEL01                                  61
     C                   ENDDO
     C*
     C                   READ      PFN01                                  60
     C                   ENDDO
     C*
     C*  Print report totals
     C*
     C     XTCNT         IFEQ      0
     C                   EXCEPT    NOREC
     C                   ELSE
     C     X1CNT         IFNE      0
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  End of program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C********************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           49 'POLICY'
     O                                           60 'POLICY'
     O                                           74 'WASRP'
     O                                           89 'WMEPP'
     O                                          116 'CLASS'
     O                                          125 'PAYROLL'
     O                                          131 'EP'
     O          E            HEAD           2
     O                                              'GRP'
     O                                           14 'EMPLOYER'
     O                                         +003 'DIV'
     O                                           40 'YR'
     O                                         +002 'FROM DATE'
     O                                         +003 'TO DATE'
     O                                           77 'PAYROLL AMT'
     O                                           93 'PAYROLL AMT'
     O                                          109 'DIFFERENCE'
     O                                          116 'CODE'
     O                                          126 'EFF DATE'
     O                                          132 'STS'
     O          E            DET            1
     O                       ELFND         4   +001
     O                       ELEMP#        4   +001
     O                       ELDIV         4   +001
     O                       XXEMNM            +001
     O                       ELFYR         4   +001
     O                       XXPFDT            +001 '  /  /    '
     O                       XXPTDT            +001 '  /  /    '
     O                       XXASRP        J   +001
     O                       XXMEPP        J   +001
     O                       XXDIFF        J   +001
     O                       EPCLAS            +001
     O                       XXEFDT            +001 '  /  /    '
     O                       EPSTAT            +003
     O          E            TOT         1  2
     O                                           17 'GROUP TOTALS:'
     O                                           32 'EMPLOYERS:'
     O                       X1CNT         J     40
     O                       X1ASRP        J     78
     O                       X1MEPP        J   +001
     O                       X1DIFF        J   +001
     O          E            NOREC       3
     O                                           28 '*** NO RECORDS MEET'
     O                                         +001 'CRITERIA ***'
