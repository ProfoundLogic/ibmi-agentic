     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8107R - PURGE POLICY ADMIN FILES
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE POLICY     *
     H*       ADMIN RELATED FILES FOR THE SELECTED FUND              *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    W8106C - POLICY ADMIN PURGE OVERRIDE CL                   *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:46                                            *
     H*  CREATED:    03/13/96    PROJECT: WC494                      *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                               *
     H* 06/14/23  C340  Project: 114WRJ (CSI)                         *
     H*                 Receive the delete Darts/Image Flag.          *
     H*                 If Delete Flag is Y then call SC950CL         *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                              *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/07/09 C:220 SUPPORT: 66684                                *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP            *
     H*                - JUST COMPILED                               *
     H*                                                              *
     H*  04/12/05 C:61 PROJECT WC1283 135WBW                         *
     H*                Purge Employee Payroll WMEEP.                 *
     H*                                                              *
     H*  11/12/04 C:62 PROJECT WA099 44375                           *
     H*                Add 2 more file for W8107R1 to process.       *
     H*                                                              *
     H*  04/15/04   C:58    PROJECT WC1178                           *
     H*             Add call to W8107R1 to process additional files. *
     H*                                                              *
     H*  02/15/98 C:57 PROJECT CM027                                 *
     H*                NO LONGER NECESSARY TO PROCESS WMENL11,       *
     H*                                                              *
     H*  02/03/98 C:61 PROJECT 60APK                                 *
     H*                Convert for V3.                               *
     H*                                                              *
     H* 10/29/97  C:33  PROJECT: WC577                               *
     H*                 Recompiled due to fields being added to WMCEP*
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
     FWAQTL     UF   E           K DISK
     FWASRL     UF   E           K DISK
     FWDDSL     UF   E           K DISK
     FWDEAL2    UF   E           K DISK
     FWDELL     UF   E           K DISK
     FWMEAL     UF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWDCXL     UF   E           K DISK
     FWMALL     UF   E           K DISK
     FWMCEL     UF   E           K DISK
     FWMEML     UF   E           K DISK
     FWMEPL     UF   E           K DISK
     FWDEHL     UF   E           K DISK
     FWDXHL     UF   E           K DISK
     FWMSHL     UF   E           K DISK
     FWMENL     UF   E           K DISK
     F*** BEG DEL ***** 02/15/98 ***********************************************
     F***WMENL11 UF  E           K        DISK
     F***            PEN01                             KRENAMEPEN02
     F*** END DEL ***** 02/15/98 ***********************************************
     FWXC#L     UF   E           K DISK
     FWMAAL1    UF   E           K DISK
     FWMAHL     UF   E           K DISK
     FWMACL     UF   E           K DISK
     FSDCMLB    UF   E           K DISK
     FSMARL1    UF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
      *
      ***** BEG ADD ******* 02/03/98 *********************************
     FWADQL     UF   E           K DISK
     FWDEXL     UF   E           K DISK
     FWMAUL     UF   E           K DISK
     FWMA3L     UF   E           K DISK
     FWMDPL     UF   E           K DISK
     FWMOOL     UF   E           K DISK
     FWMRPL     UF   E           K DISK
     FWTXCL     UF   E           K DISK
     FWX#@L     UF   E           K DISK
     FWAFPL     UF   E           K DISK
     FWMBAL     UF   E           K DISK
     FWMCCL     UF   E           K DISK
     FWXNAL     UF   E           K DISK
     FWXNEL     UF   E           K DISK
     FWXNHL     UF   E           K DISK
     FWXNXL     UF   E           K DISK
     FWXPDL     UF   E           K DISK
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     FWMVIL5    UF   E           K DISK    Rename(pvi01:pvi05)
     FWXDA1L    UF   E           K DISK
     FWZEXL     UF   E           K DISK    RENAME(PEX01:PEX02)
     FWAOFL     UF   E           K DISK
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      * ZZZ
      ***** END ADD ******* 02/03/98 *********************************
      *
     FQSYSPRT   O    F  139        PRINTER OFLIND(*INOA)
      *

     D***** BEGIN ADD ***** 04/15/04 *********************************
     D*   Files in Program W8107R1               (31)
     D*** BEGIN CHG *** 11/12/04 **************************************
     D**********          FN      1  32  8
     D*** BEGIN CHG *** 04/12/05 **************************************
     D*********           FN      1  34  8
     D FN              S              8    DIM(35) CTDATA PERRCD(1)
     D*** END   CHG *** 04/12/05 **************************************
     D*** END   CHG *** 11/12/04 **************************************
     D***** END ADD ******* 04/15/04 *********************************
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
     D*
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     D  p@Fnd          S              3A
     D  p@FrDt         S              8A
     D  p@ToDt         S              8A
     D  p@ClmF         S              1A
     D  p@PolF         S              1A
     D  p@AgtF         S              1A
     D  p@FCYD         S              7A
     D  p@TCYD         S              7A
     D  p@DltF         S              1A
     D*
     D PurgeClaims     Pr                  ExtPgm('SC950CL')
     D  p@Fnd                         3
     D  p@FrDt                        8
     D  p@ToDt                        8
     D  p@ClmF                        1
     D  p@PolF                        1
     D  p@AgtF                        1
     D  p@FCYD                        7
     D  p@TCYD                        7
     D  p@DltF                        1
     *** End Add *** 06/14/23 *** 114WRJ ****************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     C                   PARM                    X@DLT             1
     *** End Add *** 06/14/23 *** 114WRJ ****************************
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@FND         XXFND             3 0
     C                   MOVE      X@QUE         QQQUE            10
      *
     C     LIST01        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C                   PARM                    XXFILE
     C                   PARM                    XX#REC
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
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      // Call SC950CL to Purge Employer Attachments
      *
         p@Fnd  = X@FND ;
         p@FrDt = '00000000';
         p@ToDt = '99999999';
         p@ClmF = 'N';
         p@PolF = 'Y';
         p@AgtF = 'N';
         p@FCYD = '0000000';
         p@TCYD = '9999999';
         p@DltF = 'Y';

         PurgeClaims (p@Fnd:p@FrDt:p@ToDt:p@ClmF:p@PolF:p@AgtF
                      :p@FCYD:p@TCYD:p@DltF) ;
     C
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
      *  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
      *
      *  QUOTE SYSTEM - AGENT ACTIVITY
      *
     C     XXKEY         CHAIN     PQT01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PQT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PQT01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAQTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER PAYROLL SELF REPORTING TRANSACTIONS
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
      *  EMPLOYER DISCOUNT DETAIL FILE
      *
     C     XXKEY         CHAIN     PDS01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDS01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDS01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDDSP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER A/R BATCH FILE
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
     C                   MOVEL     'WDEAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER DETAIL FILE
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
      *  EMPLOYER A/R FILE
      *
     C     XXKEY         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEA02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEA02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMEAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CANCELLATION DETAIL FILE
      *
     C     XXKEY         CHAIN     PCX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCX01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCX01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDCXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  ADDITIONAL ENTITY/LOCATION FILE
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
      *  CERTIFICATE HOLDERS
      *
     C     XXKEY         CHAIN     PCE01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCE01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMCEP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER MASTER FILE
      *
     C     XXKEY         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
      *
      *  DELETE ALL THE DARTS RECORDS FOR THIS EMPLOYER
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C     CMKY          CHAIN     PCM01                              61
     C     *IN61         DOWEQ     '0'
     C                   DELETE    PCM01
     C                   ADD       1             XXDREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PCM01                                  61                R
     C                   END
      *
      *  DARTS ADDITIONAL REMARKS BY EMPLOYER
      *
     C     CMKY          CHAIN     PAR02                              62
     C     *IN62         DOWEQ     '0'
     C                   DELETE    PAR02
     C                   ADD       1             XXMREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PAR02                                  62                R
     C                   END
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
       //
       // Darts Modern Additional Remarks - By Employer
        Exec Sql
          Delete from smclp Where clkey = :qcKey;
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT A BY EMPLOYER
      *
     C     CMKY          CHAIN     PHD01                              63
     C     *IN63         DOWEQ     '0'
     C                   DELETE    PHD01
     C                   ADD       1             XXAREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PHD01                                  63                R
     C                   END
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT B BY EMPLOYER
      *
     C     CMKY          CHAIN     PH101                              64
     C     *IN64         DOWEQ     '0'
     C                   DELETE    PH101
     C                   ADD       1             XXBREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PH101                                  64                R
     C                   END
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
      *  EMPLOYER DETAIL HISTORY FILE
      *
     C     XXKEY         CHAIN     PEH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDEHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CANCELLATION HISTORY FILE
      *
     C     XXKEY         CHAIN     PXH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PXH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PXH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDXHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  NCCI REPORTING SUBMISSION HISTORY
      *
     C     XXKEY         CHAIN     PSH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PSH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PSH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMSHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  ENDORSEMENT FILE
      *
     C     XXKEY         CHAIN     PEN01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEN01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEN01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMENP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG DEL ***** 02/15/98 ***********************************************
     C***
     C***ENDORSEMENT WORKFILE
     C***
     C***        XXKEY     CHAINPEN02                60
     C***        *IN60     DOWEQ'0'
     C***                  DELETPEN02
     C***                  ADD  1         XX#REC
     C***                  ADD  1         XXTREC
     C***        XXKEY     READEPEN02                    60            R
     C***                  END
     C***OA                EXCPTHEAD
     C***                  MOVE '0'       *INOA
     C***                  MOVEL'WMENP1'  XXFILE 10
     C***                  EXCPTDET
     C*** END DEL ***** 02/15/98 ***********************************************
      *
      *  NCCI CANCELLATION/REINSTATEMENT TRANSACTION FILE
      *
     C     XXKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVE      *BLANK        XXFILE
     C                   MOVEL     'WXC#P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  AGENT ASSIGNMENT FILE
      *
     C     XXKEY         CHAIN     PAA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVE      *BLANK        XXFILE
     C                   MOVEL     'WMAAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  AGENT HISTORY FILE
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
     C                   MOVE      *BLANK        XXFILE
     C                   MOVEL     'WMAHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  AGENT COMMISSION FILE
      *
     C     XXKEY         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVE      *BLANK        XXFILE
     C                   MOVEL     'WMACP'       XXFILE           10
     C                   EXCEPT    DET
      *
      ***** BEG ADD ******* 02/03/98 *********************************
      *
      *  Employer Discount Detail File
      *
     C     XXKEY         CHAIN     PDQ01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDQ01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDQ01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WADQP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Exp Modification Tracking File
      *
     C     XXKEY         CHAIN     PEX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEX01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEX01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WDEXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Audit Log File
      *
     C     XXKEY         CHAIN     PAU01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAU01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAU01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMAUP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Employer Third Address File
      *
     C     XXKEY         CHAIN     PA301                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PA301
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PA301                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMA3P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Department File
      *
     C     XXKEY         CHAIN     PDP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDP01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMDPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Owners/Officers File
      *
     C     XXKEY         CHAIN     POO01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    POO01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     POO01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMOOP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Carrier/State Reporting Master File
      *
     C     XXKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRP01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMRPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Cancellation Maintenance File
      *
     C     XXKEY         CHAIN     PXC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PXC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PXC01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WTXCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Cancel/Reinst Trans File
      *
     C     XXKEY         CHAIN     P#@01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    P#@01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     P#@01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WX#@P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  WAFPP - Financial Earned Premium Work File
      *
     C     XXKEY         CHAIN     PFP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PFP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PFP01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WAFPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  WMBAP - Bank Account Master File
      *
     C     XXKEY         CHAIN     PBA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PBA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PBA01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMBAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  WMCCP - Contractors Class File
      *
     C     XXKEY         CHAIN     PCC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCC01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMCCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * WXNAP - NCCI Reporting - Name/Address Transaction
      *
     C     XXKEY         CHAIN     PNA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PNA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXNAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * WXNEP - NCCI Reporting - Endorsement Transaction
      *
     C     XXKEY         CHAIN     PNE01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PNE01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXNEP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * WXNHP - NCCI Reporting - Policy Page Header Transaction
      *
     C     XXKEY         CHAIN     PNH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PNH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXNHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * WXNXP - NCCI Reporting - Exposure Transaction
      *
     C     XXKEY         CHAIN     PNX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNX01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PNX01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXNXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * WXPDP - Premium Due (Self-Reporting) Workfile
      *
     C     XXKEY         CHAIN     PPD01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PPD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXPDP'       XXFILE           10
     C                   EXCEPT    DET
      *
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      *
      * WMVIP - Visit Tracking
      *
     C     XXKEY         CHAIN     PVI05                              60
     C     *IN60         DOWEQ     '0'
     C                   If        VIemp# <> 0
     C                   DELETE    PVI05
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C                   EndIf
     C     XXKEY         READE     PVI05                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMVIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * WXDA1P - New Division# in Division Add
      *
     C     XXKEY         CHAIN     PDA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXDA1P'      XXFILE           10
     C                   EXCEPT    DET
      *
      * WZEXP -  Admin/DARTS Control Definition File
      *
     C     XXKEY         CHAIN     PEX02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEX02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEX02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WZEXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * WAOFP -  WCMS New Business
      *
     C     XXKEY         CHAIN     POF01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    POF01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     POF01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAOFP'       XXFILE           10
     C                   EXCEPT    DET
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
      *
      ***** END ADD ******* 02/03/98 *********************************
      *
     C***** BEGIN ADD ***** 04/15/04 *********************************
     C*
     C*    LOOP to process additional files -- call program W8107R1
     C*** BEGIN CHG *** 11/12/04 **************************************
     C**********           DO   32        F       30
     C*** BEGIN CHG *** 04/12/05 **************************************
     C**********           DO   34        F       30
     C                   DO        35            F                 3 0
     C*** END   CHG *** 04/12/05 **************************************
     C*** END   CHG *** 11/12/04 **************************************
     C*
     C                   Z-ADD     0             XX#REC
     C                   MOVEL(P)  FN(F)         XXFILE
     C                   CALL      'W8107R1'     LIST01
     C     *INOA         IFEQ      *ON                                                      R
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET
     C*
     C                   ENDDO
     C***** END ADD ******* 04/15/04 *********************************
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
**    File names - auxiliary program W8107R1
QFEUP
QFE2P
WAC6P
WAR4P
WDABP        5
WDILP
WDLCP
WDLDP
WDLSP
WDL2P       10
WDPFP
WDRVP
WDVHP
WDXSP
WHESP       15
WHXAP
WMANP
WMASP
WMAVP
WMA2P       20
WMCAP
WMDBP
WMDHP
WMECP
WMEWP       25
WMIFP
WMRIP
WMRQP
WMWVP
WMXAP       30
WXNAP1      31
WDRTP       32
WMNBP       33
WMEEP       34
*           35    End of process: shut down called program.
