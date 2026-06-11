     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8107R - PURGE POLICY ADMIN FILES
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE POLICY     *
     H*       ADMIN RELATED QUOTE FILES FOR THE SELECTED FUND        *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    W8106C - POLICY ADMIN QUOTE PURGE OVERRIDE CL             *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:77                                            *
     H*  CREATED:    05/21/98    PROJECT: WC494                      *
     H****************************************************************
     H*                                                              *
     H* REVISIONS:                                                   *
     H*                                                              *
     F* 08/28/25 C:300 Support: 119457 (SUNZ)                        *
     F*                Old Darts records did not delete in SDCMP file*
     F*                for all quotes for the given group            *
     F*                1) Read Dart records files without Emp# and Div
     F*                2) Delete Darts records using SQL NOT in WMEMP loop
     H*                                                              *
     F* 08/07/25 C:300 Support: 119457 (SUNZ)                        *
     F*                Darts records in SDCMP and SMARP did not delete
     F*                1) Adding 'Q' at last position of CMKY        *
     F*                2) Delete additional Darts records from SMCLP *
     H*                                                              *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                *
     H*                -Implement Routing Page Changes.              *
     H*                                                              *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H****************************************************************
     H*                                                              *
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H****************************************************************
      *
      *
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *
     FWABLL     UF   E           K DISK
     FWAQOL     UF   E           K DISK
     FWASRL     UF   E           K DISK
     FWDELL     UF   E           K DISK
     FWMAHL     UF   E           K DISK
     FWMALL     UF   E           K DISK
     FWMA3L     UF   E           K DISK
     FWMEAL     UF   E           K DISK
     FWMEML     UF   E           K DISK
     FWMEPL     UF   E           K DISK
     FWMOOL     UF   E           K DISK
     FSDCMLB    UF   E           K DISK
     FSMARL1    UF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
      * ZZZ
      *
     FQSYSPRT   O    F  139        PRINTER OFLIND(*INOA)
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  QCKEY                  1     40
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXEMP#                 7     15  0
     D  XXDIV                 16     20  0
      ***** Beg Add ***** 08/07/25 ************************************
     D  XXBLANK               21     39    inz(' ')
     D  XXQUOTE               40     40    inz('Q')
      ***** End Add ***** 08/07/25 ************************************
      ***** Beg Add ***** 08/28/25 ************************************
     D                 DS
     D  QCKEY2                 1      7
     D  XXCO#2                 1      3  0
     D  XXFND2                 4      6  0
     D  XXLIKE                 7      7    inz('%')

     D  QCKEY3         s              2    inz('%Q')
      ***** End Add ***** 08/28/25 ************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@FND         XXFND             3 0
     C                   MOVE      X@QUE         QQQUE            10
      *
      *   SET-UP AND INITIALIZE
      *
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *   KEY LIST FOR ALL FILES
      *
     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE REPORT OVERRIDE
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
      *
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *   KEY LIST FOR THE DARTS FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    QCKEY            40
      ***** Beg Add ***** 08/07/25 ************************************
      /free
       Exec Sql Set Option Commit = *None;
      /end-free
      ***** End Add ***** 08/07/25 ************************************
      *
      *   CHAIN TO THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
     C                   MOVE      X@CO#         XXCO#
     C                   MOVE      UDATE         XXDATE            6 0
      *
      *   CHAIN TO THE COMPANY FILE
      *
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   GET THE DISTRIBUTION INFORMATION
      *
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
      *
      *   PRINT THE DISTRIBUTION INFORMATION
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C                   MOVE      *ON           *INOF
      *
     C                   EXCEPT    HEAD
      *
     C                   Z-ADD     *ZERO         XX#REC            6 0
     C                   Z-ADD     *ZERO         XXTREC            8 0
     C                   Z-ADD     *ZERO         XXEREC            6 0
     C                   Z-ADD     *ZERO         XXDREC            6 0
     C                   Z-ADD     *ZERO         XXMREC            6 0
     C                   Z-ADD     *ZERO         XXAREC            6 0
     C                   Z-ADD     *ZERO         XXBREC            6 0
      ***** Beg Add ***** 08/07/25 ************************************
     C                   Z-ADD     *ZERO         XXLREC            6 0
     C                   Z-ADD     *ZERO         XXCOUNT           6 0
      ***** End Add ***** 08/07/25 ************************************
      *
      *  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
      *
      *  BILLING WORK FILE
      *
     C     XXKEY         CHAIN     PBL01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PBL01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PBL01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WABLP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  TRANSFER QUOTE FILE  (WAQOP)
      *
     C     XXKEY         CHAIN     PQO01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PQO01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PQO01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAQOP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER PAYROLL FILE (WASRP)
      *
     C     XXKEY         CHAIN     PSR01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PSR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PSR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WASRP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER DETAIL FILE (WDELP)
      *
     C     XXKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEL01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEL01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDELP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  AGENT HISTORY FILE (WMAHP)
      *
     C     XXKEY         CHAIN     PAH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMAHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  ADDITIONAL ENTITY/LOCATION FILE (WMALP)
      *
     C     XXKEY         CHAIN     PAL01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAL01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAL01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMALP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER THIRD ADDRESS FILE (WMA3P)
      *
     C     XXKEY         CHAIN     PA301                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PA301
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PA301                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMA3P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER ACCOUNTS RECEIVABLE FILE (WMEAP)
      *
     C     XXKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMEAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER MASTER FILE (WMEMP)
      *
     C     XXKEY         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
      ***** Beg Del ***** 08/28/25 ************************************
      *
      *  DELETE ALL THE DARTS RECORDS FOR THIS EMPLOYER
     C*****              MOVE      EMEMP#        XXEMP#
     C*****              MOVE      EMDIV         XXDIV
      *
      *  DARTS MAJOR REMARKS BY EMPLOYER (SDCMP)
      *
     C*****CMKY          CHAIN     PCM01                              61
     C******IN61         DOWEQ     '0'
     C*****              DELETE    PCM01
     C*****              ADD       1             XXDREC
     C*****              ADD       1             XXTREC
     C*****CMKY          READE     PCM01                                  61                R
     C*****              END
      *
      *  DARTS ADDITIONAL REMARKS BY EMPLOYER (SMARP)
      *
     C*****CMKY          CHAIN     PAR02                              62
     C******IN62         DOWEQ     '0'
     C*****              DELETE    PAR02
     C*****              ADD       1             XXMREC
     C*****              ADD       1             XXTREC
     C*****CMKY          READE     PAR02                                  62                R
     C*****              END
      ***** End Del ***** 08/28/25 ************************************
      ***** Beg Add ***** 08/07/25 ************************************
      *
      *  DARTS ADDITIONAL REMARKS BY EMPLOYER (SMCLP)
      *
     C*****              EVAL      XXCOUNT = 0
     c*****/EXEC SQL
     c*****+ SELECT COUNT(*) INTO :XXCOUNT
     c*****+ FROM SMCLP
     c*****+ WHERE CLKEY = :QCKEY
     c*****/END-EXEC
     C*****              ADD       XXCOUNT       XXLREC
     C*****              ADD       XXCOUNT       XXTREC
      *
     c*****/EXEC SQL
     c*****+ DELETE FROM SMCLP
     c*****+ WHERE CLKEY = :QCKEY
     c*****/END-EXEC
      ***** End Add ***** 08/07/25 ************************************
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT A BY EMPLOYER (SMHDP)
      *
     C*****CMKY          CHAIN     PHD01                              63
     C******IN63         DOWEQ     '0'
     C*****              DELETE    PHD01
     C*****              ADD       1             XXAREC
     C*****              ADD       1             XXTREC
     C*****CMKY          READE     PHD01                                  63                R
     C*****              END
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT B BY EMPLOYER (SMH1P)
      *
     C*****CMKY          CHAIN     PH101                              64
     C******IN64         DOWEQ     '0'
     C*****              DELETE    PH101
     C*****              ADD       1             XXBREC
     C*****              ADD       1             XXTREC
     C*****CMKY          READE     PH101                                  64                R
     C*****              END
      ***** End Del ***** 08/28/25 ************************************
      *
      *  DELETE THE EMPLOYER MASTER RECORD
     C                   DELETE    PEM01
     C                   ADD       1             XXEREC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEM01                                  60                R
      *
     C                   END
      *
      *  PRINT THE EMPLOYER MASTER DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXEREC        XX#REC
     C                   MOVEL     'WMEMP'       XXFILE           10
     C                   EXCEPT    DET

      ***** Beg Add ***** 08/28/25 ************************************
      *  Delete All 5 Darts Files Records using CO#-FND key in SQL
     C                   MOVE      XXCO#         XXCO#2
     C                   MOVE      XXFND         XXFND2
      *
      *  1. DARTS MAJOR REMARKS BY EMPLOYER (SDCMP)
      *
     C                   EVAL      XXCOUNT = 0
     c/EXEC SQL
     c+ SELECT COUNT(*) INTO :XXCOUNT
     c+ FROM SDCMP
     c+ WHERE CMKEY LIKE :QCKEY2 and CMKEY LIKE :QCKEY3
     c/END-EXEC
     C                   ADD       XXCOUNT       XXDREC
     C                   ADD       XXCOUNT       XXTREC
      *
     c/EXEC SQL
     c+ DELETE FROM SDCMP
     c+ WHERE CMKEY LIKE :QCKEY2 and CMKEY LIKE :QCKEY3
     c/END-EXEC
      *
      *  2. DARTS ADDITIONAL REMARKS BY EMPLOYER (SMARP)
      *
     C                   EVAL      XXCOUNT = 0
     c/EXEC SQL
     c+ SELECT COUNT(*) INTO :XXCOUNT
     c+ FROM SMARP
     c+ WHERE ARKEY LIKE :QCKEY2 and ARKEY LIKE :QCKEY3
     c/END-EXEC
     C                   ADD       XXCOUNT       XXMREC
     C                   ADD       XXCOUNT       XXTREC
      *
     c/EXEC SQL
     c+ DELETE FROM SMARP
     c+ WHERE ARKEY LIKE :QCKEY2 and ARKEY LIKE :QCKEY3
     c/END-EXEC
      *
      *  3. DARTS ADDITIONAL REMARKS BY EMPLOYER (SMCLP)
      *
     C                   EVAL      XXCOUNT = 0
     c/EXEC SQL
     c+ SELECT COUNT(*) INTO :XXCOUNT
     c+ FROM SMCLP
     c+ WHERE CLKEY LIKE :QCKEY2 and CLKEY LIKE :QCKEY3
     c/END-EXEC
     C                   ADD       XXCOUNT       XXLREC
     C                   ADD       XXCOUNT       XXTREC
      *
     c/EXEC SQL
     c+ DELETE FROM SMCLP
     c+ WHERE CLKEY LIKE :QCKEY2 and CLKEY LIKE :QCKEY3
     c/END-EXEC
      *
      *  4. CUSTOM GROUP ID DATA FILE - FORMAT A BY EMPLOYER (SMHDP)
      *
     C                   EVAL      XXCOUNT = 0
     c/EXEC SQL
     c+ SELECT COUNT(*) INTO :XXCOUNT
     c+ FROM SMHDP
     c+ WHERE HDKEY LIKE :QCKEY2 and HDKEY LIKE :QCKEY3
     c/END-EXEC
     C                   ADD       XXCOUNT       XXAREC
     C                   ADD       XXCOUNT       XXTREC
      *
     c/EXEC SQL
     c+ DELETE FROM SMHDP
     c+ WHERE HDKEY LIKE :QCKEY2 and HDKEY LIKE :QCKEY3
     c/END-EXEC
      *
      *  5. CUSTOM GROUP ID DATA FILE - FORMAT B BY EMPLOYER (SMH1P)
      *
     C                   EVAL      XXCOUNT = 0
     c/EXEC SQL
     c+ SELECT COUNT(*) INTO :XXCOUNT
     c+ FROM SMH1P
     c+ WHERE H1KEY LIKE :QCKEY2 and H1KEY LIKE :QCKEY3
     c/END-EXEC
     C                   ADD       XXCOUNT       XXBREC
     C                   ADD       XXCOUNT       XXTREC
      *
     c/EXEC SQL
     c+ DELETE FROM SMH1P
     c+ WHERE H1KEY LIKE :QCKEY2 and H1KEY LIKE :QCKEY3
     c/END-EXEC
      ***** End Add ***** 08/28/25 ************************************
      *
      *  PRINT THE DART MAJOR REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXDREC        XX#REC
     C                   MOVEL     'SDCMP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DART ADDITIONAL REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXMREC        XX#REC
     C                   MOVEL     'SMARP'       XXFILE           10
     C                   EXCEPT    DET
      ***** Beg Add ***** 08/07/25 ************************************
      *
      *  PRINT THE DART ADDITIONAL REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXLREC        XX#REC
     C                   MOVEL     'SMCLP'       XXFILE           10
     C                   EXCEPT    DET
      ***** End Add ***** 08/07/25 ************************************
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT A DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXAREC        XX#REC
     C                   MOVEL     'SMHDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT B DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXBREC        XX#REC
     C                   MOVEL     'SMH1P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER PAYROLL FILE
      *
     C     XXKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMEPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  OWNERS/OFFICERS MASTER FILE
      *
     C     XXKEY         CHAIN     POO01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    POO01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     POO01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMOOP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * ZZZ
      *
      *
      *  PRINT TOTAL
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTL
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
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
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           20 'CO#:'
     O                       XXCO#               25
     O          E            ROUTE          2
     O                                           20 'DATE:'
     O                       XXDATE              30 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1996'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     O****************************************************************
     O*     HEADING
     O****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       XXDATE        Y    130
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              94
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           2
     O                                           18 'FILE NAME'
     O                                           44 '# DELETED RECORDS'
     O****************************************************************
     O*     DETAIL LINE WITH FILE NAME AND COUNT
     O****************************************************************
     O          E            DET            1
     O                       XXFILE              19
     O                       XX#REC        ZB    40
     O****************************************************************
     O*     TOTAL RECORD COUNT FOR ALL FILES
     O****************************************************************
     O          E            TOTL        2  2
     O                                           18 'ALL FILES'
     O                       XXTREC        Z     40
