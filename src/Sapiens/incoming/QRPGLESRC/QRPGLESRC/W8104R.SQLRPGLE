     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8104R - PURGE FUND MASTER/TABLE FILES
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE FUND       *
     H*       MASTER FILE AND FUND TABLE FILES FOR THE SELECTED FUND *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    W8103C - FUND MASTER/TABLE PURGE OVERRIDE CL              *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:46                                            *
     H*  CREATED:    03/13/96    PROJECT: WC494                      *
     H****************************************************************
     H*                                                              *
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 06/14/23  C340  Project: 114WRJ (CSI)                        *
     H*                 Add additional files for Group being Purge.  *
     H*                                                              *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H*  04/15/04   C:58    PROJECT WC1178
     H*             Add call to W8104R2 to process additional files.
     H*
     H*  02/03/98 C:61 PROJECT 60APK                                 *
     H*                Convert for V3.                               *
     H*                                                              *
     H* 12/02/97 C:30 Project: WC500                                 *
     H*           Re-compile.  File WAC2P file changed.              *
     H*           Re-compile.  File WAR2P file changed.              *
     H*                                                              *
     H* 09/16/97  47  PROJECT: WC659.2                               *
     H*           REMOVE REFERENCE TO WMBAP;  FILE IS NO LONGER USED.*
     H*                                                              *
     H****************************************************************
      *
      *
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *
      *
     FWMFNL     UF   E           K DISK
     FWDF1L     UF   E           K DISK
     FWDF2L     UF   E           K DISK
     FWTLML     UF   E           K DISK
     FWMDSL     UF   E           K DISK
     FWMREL     UF   E           K DISK
     FWMCPL     UF   E           K DISK
     FWMBXL     UF   E           K DISK
     FWABLL     UF   E           K DISK
     FWADSL     UF   E           K DISK
     F                                     RENAME(PDS01:PDS02)
     FWMARL     UF   E           K DISK
     FWAR2L1    UF   E           K DISK
     FWAC2L1    UF   E           K DISK
     FWMBIL     UF   E           K DISK
      *** BEG DEL *** 09/16/97 **************************************
     F*** WMBAL   UF  E           K        DISK
      *** END DEL *** 09/16/97 **************************************
     FWMPCL     UF   E           K DISK
     FWXWCL     UF   E           K DISK
     FWXWPL1    UF   E           K DISK
     FWAC3L1    UF   E           K DISK
     F                                     RENAME(PCP01:PCP02)
     FWACPL1    UF   E           K DISK
     F                                     RENAME(PCP01:PCP03)
     FWAR3L7    UF   E           K DISK
     FWMCIL     UF   E           K DISK
     FWADQL     UF   E           K DISK
     FWADDL     UF   E           K DISK
     FWMCXL     UF   E           K DISK
     FWTDDL     UF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FWDRCL     UF   E           K DISK
     FSDCMLB    UF   E           K DISK
     FSMARL1    UF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
      *
      ***** BEG ADD ******* 02/03/98 *********************************
     FWAOFL     UF   E           K DISK
     FWDECL     UF   E           K DISK
     FWDHEL     UF   E           K DISK
     FWDRBL     UF   E           K DISK
     FWDRRL     UF   E           K DISK
     FWDSTL     UF   E           K DISK
     FWDUSL5    UF   E           K DISK
     FWMHXL     UF   E           K DISK
     FWMMUL     UF   E           K DISK
     FWMNXL     UF   E           K DISK
     FWMRDL2    UF   E           K DISK
     FWMSTL     UF   E           K DISK
     F                                     RENAME(PST01:PST02)
     FWMUDL1    UF   E           K DISK
     FWMXCL     UF   E           K DISK
     FWSDLL     UF   E           K DISK
     FWTENL     UF   E           K DISK
      * ZZZ
      ***** END ADD ******* 02/03/98 *********************************
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     FWACLL     UF   E           K DISK
     FWMAAL1    UF   E           K DISK
     FWMGRL     UF   E           K DISK
     FWMDCL     UF   E           K DISK
     FWMIFL     UF   E           K DISK
     FWMFIP     UF   E           K DISK
     FWDXSL     UF   E           K DISK
     FWTC3L     UF   E           K DISK
     FWDSCL     UF   E           K DISK
     FWMRTL     UF   E           K DISK
     FWTMYL     UF   E           K DISK
     FWDCDL     UF   E           K DISK
     FWAMLL     UF   E           K DISK
     FWMLDL1    UF   E           K DISK
     FWDFHL     UF   E           K DISK
     FWDF4L     UF   E           K DISK
     FWDF3L     UF   E           K DISK
     FWMADL     UF   E           K DISK
     FWMPEL     UF   E           K DISK
     FWMFCL     UF   E           K DISK
     FWDR1L     UF   E           K DISK
     FWTXCL     UF   E           K DISK    RENAME(PXC01:PXC02)
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
     FQSYSPRT   O    F  139        PRINTER OFLIND(*INOA)
     F*

     D*
     D***** BEGIN ADD ***** 04/15/04 *********************************
     D*   Files in Program W8104R2
     D FN              S              8    DIM(8) CTDATA PERRCD(1)
     D***** END ADD ******* 04/15/04 *********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  QCKEY                  1     40
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
      *
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
     C*
     C*    PLIST TO CALL W8104R2
     C     LIST01        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C                   PARM                    XXFILE
     C                   PARM                    XX#REC
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
      *
      *  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
      *
      *  FUND MASTER FILE
      *
     C     XXKEY         CHAIN     PFN01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PFN01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PFN01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMFNP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  DARTS MAJOR REMARKS BY FUND
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCM01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PCM01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'SDCMP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  DARTS ADDITIONAL REMARKS BY FUND
      *
     C     CMKY          CHAIN     PAR02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAR02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PAR02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'SMARP'       XXFILE           10
     C                   EXCEPT    DET
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
       //
       // Darts Modern Additional Remarks - By Employer
        Exec Sql
          Delete from smclp Where clkey = :qcKey;
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT A
      *
     C     CMKY          CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PHD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PHD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'SMHDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT B
      *
     C     CMKY          CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PH101
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PH101                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'SMH1P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FUND DETAIL BY YEAR
      *
     C     XXKEY         CHAIN     PF101                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PF101
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PF101                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDF1P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  FUND AUXILIARY
      *
     C     XXKEY         CHAIN     PF201                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PF201
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PF201                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDF2P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  LIMITS TABLE FILE
      *
     C     XXKEY         CHAIN     PLM01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLM01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PLM01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WTLMP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EMPLOYER DISCOUNT DETAIL
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
     C                   MOVEL     'WMDSP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  REGION MASTER FILE
      *
     C     XXKEY         CHAIN     PRE01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRE01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMREP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CHAPTER MASTER FILE
      *
     C     XXKEY         CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMCPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Billing Schedule Code Aux File
      *
     C     XXKEY         CHAIN     PBX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PBX01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PBX01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMBXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  BILLING SCHEDULE FILE
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
      *  EMPLOYER DISCOUNT DETAIL FILE
      *
     C     XXKEY         CHAIN     PDS02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDS02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDS02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WADSP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  AGENT RATE FILE
      *
     C     XXKEY         CHAIN     PAR01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMARP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  REQUEST COMMISSION PAYMENT FILE
      *
     C     XXKEY         CHAIN     PR201                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PR201
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PR201                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAR2P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CHECK SCHEDULE FILE
      *
     C     XXKEY         CHAIN     PC201                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PC201
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PC201                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAC2P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  MICR BANK INFORMATION FILE
      *
     C     XXKEY         CHAIN     PBI01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PBI01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PBI01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMBIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *** BEG DEL *** 09/16/97 **************************************
     C***BANK ACCOUNT MASTER FILE
     C***
     C***        XXKEY     CHAINPBA01                60
     C***        *IN60     DOWEQ'0'
     C***                  DELETPBA01
     C***                  ADD  1         XX#REC
     C***                  ADD  1         XXTREC
     C***        XXKEY     READEPBA01                    60            R
     C***                  END
     C***OA                EXCPTHEAD
     C***                  MOVE '0'       *INOA
     C***                  MOVEL'WMBAP'   XXFILE 10
     C***                  EXCPTDET
      *** END DEL *** 09/16/97 **************************************
      *
      *  COMP PAYMENT CONTROL FILE
      *
     C     XXKEY         CHAIN     PPC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PPC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMPCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  WASTE CHECK TRANSACTIONS
      *
     C     XXKEY         CHAIN     PWC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PWC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PWC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXWCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  WASTE COMMISSION CHECK FILE
      *
     C     XXKEY         CHAIN     PWP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PWP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PWP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WXWPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  GROUP CHECK PROCESSING FILE
      *
     C     XXKEY         CHAIN     PCP02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCP02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCP02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAC3P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CHECK PROCESSING FILE
      *
     C     XXKEY         CHAIN     PCP03                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCP03
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCP03                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WACPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  GROUP REQUEST PAYMENT TRANSACTION FILE
      *
     C     XXKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAR3P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Check Issuance Control File
      *
     C     XXKEY         CHAIN     PCI01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCI01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCI01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMCIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  DEDUCTIBLE PAYMENT RECOVERIES PENDING
      *
     C     XXKEY         CHAIN     PDQ01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDQ01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDQ01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WADQP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  DEDUCTIBLE DETAIL FILE - OVERRIDE
      *
     C     XXKEY         CHAIN     PDD01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WADDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CLAIM/DARTS CONTROL DEFINITION FILE
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
     C                   MOVEL     'WMCXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  DEDUCTIBLE DISCOUNT CODE
      *
     C     XXKEY         CHAIN     PDD02                              60
     C     *IN60         DOWEQ     '0'
     C*** BEG ADD *****  10/11/07  ************************************
     C     DDSTAT        ANDEQ     'A'
     C*** END ADD *****  10/11/07  ************************************
     C                   DELETE    PDD02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C                   MOVEL     'WTDDP'       XXFILE           10
     C     XXKEY         READE     PDD02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WTDDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  RESERVE CUSHION FILE
      *
     C     XXKEY         CHAIN     PRC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDRCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      ***** BEG ADD ******* 02/03/98 *********************************
      *
      *  Office Auxiliary File
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
      *
      *  Expense Constant Definition by State
      *
     C     XXKEY         CHAIN     PEC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDECP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Harbor Exposure File
      *
     C     XXKEY         CHAIN     PHE01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PHE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PHE01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDHEP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Compensation Reimbursement File
      *
     C     XXKEY         CHAIN     PRB01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRB01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRB01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDRBP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  State Report/Rating File
      *
     C     XXKEY         CHAIN     PRR01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDRRP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Short Rate Detail File
      *
     C     XXKEY         CHAIN     PST01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PST01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PST01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDSTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  User/Report Detail File
      *
     C     XXKEY         CHAIN     PUS01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PUS01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PUS01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDUSP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Client Control Heading File
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
     C                   MOVEL     'WMHXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Assessable Mutual Forms
      *
     C     XXKEY         CHAIN     PMU01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PMU01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PMU01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMMUP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Endorsement Form State Info File
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
     C                   MOVEL     'WMNXP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Report Definition File
      *
     C     XXKEY         CHAIN     PRD01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMRDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Short Rate Master File
      *
     C     XXKEY         CHAIN     PST02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PST02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PST02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMSTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  User Destination File
      *
     C     XXKEY         CHAIN     PUD01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PUD01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PUD01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMUDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Client Control Master File
      *
     C     XXKEY         CHAIN     PXC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PXC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PXC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMXCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Policy Department Letter File
      *
     C     XXKEY         CHAIN     PDL01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDL01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDL01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WSDLP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Endorsement Form Table File
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
     C                   MOVEL     'WTENP'       XXFILE           10
     C                   EXCEPT    DET
      *
      * ZZZ
      ***** END ADD ******* 02/03/98 *********************************
     C***** BEGIN ADD ***** 04/15/04 *********************************
     C*
     C*    LOOP to process additional files -- call program W8104R2
     C                   DO        8             F                 3 0
     C*
     C                   Z-ADD     0             XX#REC
     C                   MOVEL(P)  FN(F)         XXFILE
     C                   CALL      'W8104R2'     LIST01
     C     *INOA         IFEQ      *ON                                                      R
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET
     C*
     C                   ENDDO
     C***** END ADD ******* 04/15/04 *********************************
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      *
      *  Agent Assignment File
      *
     C     XXKEY         Chain     PAA01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PAA01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PAA01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMAAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Auto Claim Letters Definition File
      *
     C     XXKEY         Chain     PCL01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PCL01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PCL01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WACLP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Group Rate Set Code File
      *
     C     XXKEY         Chain     PGR01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PGR01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PGR01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMGRP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  ACH Control File
      *
     C     XXKEY         Chain     PDC01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PDC01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PDC01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMDCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Installment Fee Information File
      *
     C     XXKEY         Chain     PIF01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PIF01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PIF01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMIFP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Group Master - Web Interface Associ
      *
     C     XXKEY         Chain     PFI01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PFI01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PFI01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMFIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Excluded States Detail File
      *
     C     XXKEY         Chain     PXS01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PXS01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PXS01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDXSP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  A/R Premium Category Information
      *
     C     XXKEY         Chain     PC301                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PC301
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PC301                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WTC3P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  A/R Premium Category Information
      *
     C     XXKEY         Chain     PSC01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PSC01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PSC01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDSCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  State Rate Change File
      *
     C     XXKEY         Chain     PRT01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PRT01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PRT01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMRTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Mid Yr Endorsmnt Tabl File
      *
     C     XXKEY         Chain     PMY01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PMY01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PMY01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WTMYP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Cash Dissolve Hierarchy File
      *
     C     XXKEY         Chain     PCD01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PCD01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PCD01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDCDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Auto Claim Letters Definition
      *
     C     XXKEY         Chain     PML01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PML01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PML01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAMLP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Large Deductible Discount Master File
      *
     C     XXKEY         Chain     PLD01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PLD01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PLD01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMLDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Flexible information file
      *
     C     XXKEY         Chain     PFH01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PFH01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PFH01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDFHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Group Printing Text File
      *
     C     XXKEY         Chain     PF401                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PF401
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PF401                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDF4P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Group EDI information
      *
     C     XXKEY         Chain     PF301                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PF301
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PF301                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDF3P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Auto DARTS Control File
      *
     C     XXKEY         Chain     PAD01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PAD01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PAD01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMADP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Group Policy Export Control File
      *
     C     XXKEY         Chain     PPE01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PPE01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PPE01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMPEP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Group Contact Information
      *
     C     XXKEY         Chain     PFC01                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PFC01
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PFC01                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMFCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  State Compensation Rates by Benefit
      *
     C     XXKEY         Chain     PR101                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PR101
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PR101                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDR1P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Cancellation Maintenance File
      *
     C     XXKEY         Chain     PXC02                              60
     C     *IN60         DOWEQ     '0'
     C                   Delete    PXC02
     C                   Add       1             XX#REC
     C                   Add       1             XXTREC
     C     XXKEY         READE     PXC02                                  60                R
     C                   EndDo
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WTXCP'       XXFILE           10
     C                   EXCEPT    DET
     *** End Add *** 06/14/23 *** 114WRJ ****************************
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
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1996'
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
**    File names - auxiliary program W8104R2
WMAXL
WMCFL
WMDXL
WMEXL
WMGCL        5
WMMGL
WMPFL        7
*             8    End of process: shut down called program.
