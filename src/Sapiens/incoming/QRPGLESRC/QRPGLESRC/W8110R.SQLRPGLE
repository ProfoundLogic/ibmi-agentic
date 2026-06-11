     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8110R - PURGE CLAIM MASTER RELATED FILES
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE CLAIM      *
     H*       MASTER FILES FOR THE SELECTED FUND                     *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    W8109C - CLAIM MASTER FILE PURGE OVERRIDE CL              *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:46                                            *
     H*  CREATED:    03/13/96    PROJECT: WC494                      *
     H****************************************************************
     H*                                                              *
     H* REVISIONS:                                                   *
     H*
     H* 06/14/23  C340  Project: 114WRJ (CSI)                         *
     H*                 Receive the delete Darts/Image Flag.          *
     H*                 If Delete Flag is Y then call SC950CL         *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*                  Retrieve brand Name                          *
     H*                                                               *
     H*  04/15/04   C:58    PROJECT WC1178
     H*             Add processing for 8 additional files.
     H*
     H*                                                              *
     H*  02/03/98 C:61 PROJECT 60APK                                 *
     H*                Convert for V3.                               *
     H*                                                              *
     H****************************************************************
     H*                                                              *
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H*                                                              *
     H****************************************************************
      *
      *
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *
     FWMCML     UF   E           K DISK
     FWMCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     UF   E           K DISK
     FWMDDL     UF   E           K DISK
     FWMWHL     UF   E           K DISK
     FWAMCL     UF   E           K DISK
      ***** BEG DEL ******* 02/03/98 *********************************
     F*WMEDL   UF  E           K        DISK
      ***** END DEL ******* 02/03/98 *********************************
     FWSFRL     UF   E           K DISK
     FWMRIL     UF   E           K DISK
     FWDLSL     UF   E           K DISK
     FWMRQL     UF   E           K DISK
     FWDRVL     UF   E           K DISK
     FWDPFL     UF   E           K DISK
     FWMRRL     UF   E           K DISK
     FWACTL2    UF   E           K DISK
     FWDDDL     UF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FSDCMLB    UF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FSMARL1    UF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
      *
      ***** BEG ADD ******* 02/03/98 *********************************
     FWACML     UF   E           K DISK
     F                                     RENAME(PCM01:PCM04)
     FWADTL     UF   E           K DISK
     FWDACL     UF   E           K DISK
     FWDNTL     UF   E           K DISK
     FWMAWL     UF   E           K DISK
     FWMBHL     UF   E           K DISK
     FWMBNL     UF   E           K DISK
     FWMBYL     UF   E           K DISK
     FWMEIL1    UF   E           K DISK
     FWMLGL     UF   E           K DISK
     FWMLLL     UF   E           K DISK
     FWMNIL     UF   E           K DISK
     FWMTPL     UF   E           K DISK
      ***** END ADD ******* 02/03/98 *********************************
     F***** BEGIN ADD ***** 04/15/04 *********************************
     F*   Additional Files
     FWADHL1    UF   E           K DISK
     FWAIDL     UF   E           K DISK
     FWEREL     UF   E           K DISK
     FWMC1L     UF   E           K DISK
     FWMEDL     UF   E           K DISK
     FWMEFP     UF   E           K DISK
     FWMIHL     UF   E           K DISK
     FWMTIL     UF   E           K DISK
     F***** END ADD ******* 04/15/04 *********************************
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
     FWMVIL5    UF   E           K DISK    Rename(pvi01:pvi05)
     *** End Add *** 06/14/23 *** 114WRJ ****************************
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
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D                 DS
     D  RIKEY                  1     40
     D  X1CO#                  1      3  0
     D  X1FND                  4      6  0
     D  X1FYR                  7      9
     D  X1ASFL                10     10
     D  X1EFDT                11     18  0
     D                 DS
     D  PFKEY                  1     40
     D  X2CO#                  1      3  0
     D  X2FND                  4      6  0
     D  X2FYR                  7      9  0
     D  X2RNDT                10     16  0
     D  X2CASE                17     23
     D  X2MCLM                24     30  0
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
     **** Beg Add *** 06/14/23 *** 114WRJ ****************************
     C                   PARM                    X@DLT             1
     **** End Add *** 06/14/23 *** 114WRJ ****************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
      *
      *   KEY LIST FOR THE DARTS FILE ASSOCIATED WITH REINSURANCE
      *
     C     RIKY          KLIST
     C                   KFLD                    RIKEY            40
      *
      *   KEY LIST FOR THE DARTS FILE ASSOCIATED WITH SPECIFIC FILING
      *
     C     PFKY          KLIST
     C                   KFLD                    PFKEY            40
      *
      *   KEY LIST FOR THE CLAIM DEOGRPAHIC FILE
      *
     C     SSKEY         KLIST
     C                   KFLD                    XXSS#             9
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
     C                   Z-ADD     *ZERO         XXCREC            6 0
     C                   Z-ADD     *ZERO         XXTREC            8 0
     C                   Z-ADD     *ZERO         XXDREC            6 0
     C                   Z-ADD     *ZERO         XXMREC            6 0
     C                   Z-ADD     *ZERO         XXAREC            6 0
     C                   Z-ADD     *ZERO         XXBREC            6 0
     C                   Z-ADD     *ZERO         XX1REC            6 0
     C                   Z-ADD     *ZERO         XX2REC            6 0
     C                   Z-ADD     *ZERO         XX3REC            6 0
     C                   Z-ADD     *ZERO         XX4REC            6 0
     C                   Z-ADD     *ZERO         XX5REC            6 0
     C                   Z-ADD     *ZERO         XX6REC            6 0
     C                   Z-ADD     *ZERO         XX7REC            6 0
     C                   Z-ADD     *ZERO         XX8REC            6 0
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      // Call SC950CL to Purge Claim Attachments
      *
         p@Fnd  = X@FND ;
         p@FrDt = '00000000';
         p@ToDt = '99999999';
         p@ClmF = 'Y';
         p@PolF = 'N';
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
      *  CLAIM MASTER AND CLAIM DEMOGRPAPIC FILES
      *
     C     XXKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
      *
      *  IF ANOTHER CLAIM DOES NOT EXIST WITH THIS CLAIM'S SOCIAL
      *  SECURITY NUMBER, THEN DELETE THE CLAIM DEMOGRAPHIC
      *  RECORD IN THE FILE WMCDP.
      *
     C                   Z-ADD     *ZERO         XX#CLM            3 0
     C                   MOVE      CMSS#         XXSS#
     C     SSKEY         CHAIN     PCM02                              61
     C     *IN61         DOWEQ     '0'
      *
      *  ADD ONE TO THE NUMBER OF CLAIMS FOR THIS SS#.
     C                   ADD       1             XX#CLM
     C     SSKEY         READE     PCM02                                  61                R
     C                   END
      *
      *  IF THE NUMBER OF CLAIMS THAT HAVE THE SOCIAL SECURITY IS
      *  EQUAL TO 1, DELETE THE DEMOGRPAHIC RECORD.
     C     XX#CLM        IFEQ      1
     C     SSKEY         CHAIN     PCD01                              60
      ***** BEG DEL ******* 02/03/98 *********************************
     C*********            DELETPCD01
     C*********            ADD  1         XXCREC
     C*********            ADD  1         XXTREC
      ***** END DEL ******* 02/03/98 *********************************
      ***** BEG ADD ******* 02/03/98 *********************************
     C  N60              DELETE    PCD01
     C  N60              ADD       1             XXCREC
     C  N60              ADD       1             XXTREC
      ***** END ADD ******* 02/03/98 *********************************
     C                   END
      *
      *  DELETE ALL THE DARTS RECORDS FOR THIS CLAIM
     C                   MOVE      CMFYR         XXFYR
     C                   MOVE      CMCASE        XXCASE
      *
      *  DARTS MAJOR REMARKS BY CLAIM
      *
     C     CMKY          CHAIN     PCM03                              62
     C     *IN62         DOWEQ     '0'
     C                   DELETE    PCM03
     C                   ADD       1             XXDREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PCM03                                  62                R
     C                   END
      *
      *  DARTS ADDITIONAL REMARKS BY CLAIM
      *
     C     CMKY          CHAIN     PAR02                              63
     C     *IN63         DOWEQ     '0'
     C                   DELETE    PAR02
     C                   ADD       1             XXMREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PAR02                                  63                R
     C                   END
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
       //
       // Darts Modern Additional Remarks - By Claims
        Exec Sql
          Delete from smclp Where clkey = :qcKey;
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT A BY CLAIM
      *
     C     CMKY          CHAIN     PHD01                              64
     C     *IN64         DOWEQ     '0'
     C                   DELETE    PHD01
     C                   ADD       1             XXAREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PHD01                                  64                R
     C                   END
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT B BY CLAIM
      *
     C     CMKY          CHAIN     PH101                              65
     C     *IN65         DOWEQ     '0'
     C                   DELETE    PH101
     C                   ADD       1             XXBREC
     C                   ADD       1             XXTREC
     C     CMKY          READE     PH101                                  65                R
     C                   END
      *
      *  DELETE THE CLAIM RECORD
      *
     C                   DELETE    PCM01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
      *
     C     XXKEY         READE     PCM01                                  60                R
     C                   END
      *
      *  PRINT THE NUMBER OF CLAIM RECORDS DELETED
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMCMP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  PRINT THE NUMBER OF DEMOGRAPHIC RECORDS DELETED
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXCREC        XX#REC
     C                   MOVEL     'WMCDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DART MAJOR REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXDREC        XX#REC
     C                   MOVEL     'SDCMP'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DART ADDITIONAL REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXMREC        XX#REC
     C                   MOVEL     'SMARP'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT A DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXAREC        XX#REC
     C                   MOVEL     'SMHDP'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT B DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XXBREC        XX#REC
     C                   MOVEL     'SMH1P'       XXFILE           10
     C                   MOVEL     'CLAIM'       XXDESC           10
     C                   EXCEPT    DET
      *
      *  DIARY DATE FILE
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
     C                   MOVE      *BLANK        XXDESC
     C                   MOVEL     'WMDDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  WORK HISTORY FILE
      *
     C     XXKEY         CHAIN     PWH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PWH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PWH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMWHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  MULTIPLE CLAIM FILE - REINSURANCE SYSTEM
      *
     C     XXKEY         CHAIN     PMC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PMC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PMC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAMCP'       XXFILE           10
     C                   EXCEPT    DET
      ***** BEG DEL ******* 02/03/98 *********************************
      *
      *  EXCESS CLAIM DIARY FILE
      *
     C********** XXKEY     CHAINPED01                60
     C********** *IN60     DOWEQ'0'
     C**********           DELETPED01
     C**********           ADD  1         XX#REC
     C**********           ADD  1         XXTREC
     C********** XXKEY     READEPED01                    60            R
     C**********           END
     C** OA ****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           MOVEL'WMEDP'   XXFILE 10
     C**********           EXCPTDET
      ***** END DEL ******* 02/03/98 *********************************
      *
      *  DWC13 INITIAL SUMMARY ENTRY FILE
      *
     C     XXKEY         CHAIN     PFR01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PFR01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PFR01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WSFRP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  REINSUARNCE RECOVERY REQUEST FILE
      *
     C     XXKEY         CHAIN     PRI01                              60
     C     *IN60         DOWEQ     '0'
      *
      *  DELETE ALL THE DARTS RECORDS FOR THIS REINSURANCE RECORD
     C                   MOVE      X@CO#         X1CO#
     C                   MOVE      X@FND         X1FND
     C                   MOVE      *BLANK        X1FYR
     C                   MOVE      RIASFL        X1ASFL
      *
     C                   Z-ADD     RIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT
      *
      *  DARTS MAJOR REMARKS BY REINSURANCE
      *
     C     RIKY          CHAIN     PCM03                              61
     C     *IN61         DOWEQ     '0'
     C                   DELETE    PCM03
     C                   ADD       1             XX1REC
     C                   ADD       1             XXTREC
     C     RIKY          READE     PCM03                                  61                R
     C                   END
      *
      *  DARTS ADDITIONAL REMARKS BY REINSURANCE
      *
     C     RIKY          CHAIN     PAR02                              62
     C     *IN62         DOWEQ     '0'
     C                   DELETE    PAR02
     C                   ADD       1             XX2REC
     C                   ADD       1             XXTREC
     C     RIKY          READE     PAR02                                  62                R
     C                   END
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT A BY REINSURANCE
      *
     C     RIKY          CHAIN     PHD01                              63
     C     *IN63         DOWEQ     '0'
     C                   DELETE    PHD01
     C                   ADD       1             XX3REC
     C                   ADD       1             XXTREC
     C     RIKY          READE     PHD01                                  63                R
     C                   END
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT B BY REINSURANCE
      *
     C     RIKY          CHAIN     PH101                              64
     C     *IN64         DOWEQ     '0'
     C                   DELETE    PH101
     C                   ADD       1             XX4REC
     C                   ADD       1             XXTREC
     C     RIKY          READE     PH101                                  64                R
     C                   END
      *
     C                   DELETE    PRI01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRI01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMRIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DART MAJOR REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX1REC        XX#REC
     C                   MOVEL     'SDCMP'       XXFILE           10
     C                   MOVEL     'REINSUR'     XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DART ADDITIONAL REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX2REC        XX#REC
     C                   MOVEL     'SMARP'       XXFILE           10
     C                   MOVEL     'REINSUR'     XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT A DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX3REC        XX#REC
     C                   MOVEL     'SMHDP'       XXFILE           10
     C                   MOVEL     'REINSUR'     XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT B DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX4REC        XX#REC
     C                   MOVEL     'SMH1P'       XXFILE           10
     C                   MOVEL     'REINSUR'     XXDESC           10
     C                   EXCEPT    DET
      *
      *  REINSUARNCE LOSS REPORT DETAIL
      *
     C     XXKEY         CHAIN     PLS01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLS01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PLS01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDLSP'       XXFILE           10
     C                   MOVE      *BLANK        XXDESC
     C                   EXCEPT    DET
      *
      *  REINSUARNCE RECOVERY REQUEST FILE
      *
     C     XXKEY         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRQ01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRQ01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMRQP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  REINSUARNCE RECOVERY RECEIVED FILE
      *
     C     XXKEY         CHAIN     PRV01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRV01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRV01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDRVP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  REINSUARNCE PARTIAL FILING FILE
      *
     C     XXKEY         CHAIN     PPF01                              60
     C     *IN60         DOWEQ     '0'
      *
      *  DELETE ALL THE DARTS RECORDS FOR THIS SPECIFIC FILING
     C                   MOVE      X@CO#         X2CO#
     C                   MOVE      X@FND         X2FND
     C                   MOVE      PFFYR         X2FYR
     C                   MOVE      PFRNDT        X2RNDT
     C                   MOVE      PFCASE        X2CASE
     C                   MOVE      PFMCLM        X2MCLM
      *
      *  DARTS MAJOR REMARKS BY SPECIFIC FILING
      *
     C     PFKY          CHAIN     PCM03                              61
     C     *IN61         DOWEQ     '0'
     C                   DELETE    PCM03
     C                   ADD       1             XX5REC
     C                   ADD       1             XXTREC
     C     PFKY          READE     PCM03                                  61                R
     C                   END
      *
      *  DARTS ADDITIONAL REMARKS BY SPECIFIC FILING
      *
     C     PFKY          CHAIN     PAR02                              62
     C     *IN62         DOWEQ     '0'
     C                   DELETE    PAR02
     C                   ADD       1             XX6REC
     C                   ADD       1             XXTREC
     C     PFKY          READE     PAR02                                  62                R
     C                   END
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT A BY SPECIFIC FILING
      *
     C     PFKY          CHAIN     PHD01                              63
     C     *IN63         DOWEQ     '0'
     C                   DELETE    PHD01
     C                   ADD       1             XX7REC
     C                   ADD       1             XXTREC
     C     PFKY          READE     PHD01                                  63                R
     C                   END
      *
      *  CUSTOM GROUP ID DATA FILE - FORMAT B BY SPECIFIC FILING
      *
     C     PFKY          CHAIN     PH101                              64
     C     *IN64         DOWEQ     '0'
     C                   DELETE    PH101
     C                   ADD       1             XX8REC
     C                   ADD       1             XXTREC
     C     PFKY          READE     PH101                                  64                R
     C                   END
      *
     C                   DELETE    PPF01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PPF01                                  60                R
     C                   END
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDPFP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DART MAJOR REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX5REC        XX#REC
     C                   MOVEL     'SDCMP'       XXFILE           10
     C                   MOVEL     'SPEC FIL'    XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DART ADDITIONAL REMARK DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX6REC        XX#REC
     C                   MOVEL     'SMARP'       XXFILE           10
     C                   MOVEL     'SPEC FIL'    XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT A DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX7REC        XX#REC
     C                   MOVEL     'SMHDP'       XXFILE           10
     C                   MOVEL     'SPEC FIL'    XXDESC           10
     C                   EXCEPT    DET
      *
      *  PRINT THE DARTS CUSTOM GROUP ID FORMAT B DELETE TOTALS
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     XX8REC        XX#REC
     C                   MOVEL     'SMH1P'       XXFILE           10
     C                   MOVEL     'SPEC FIL'    XXDESC           10
     C                   EXCEPT    DET
      *
      *  REQUEST FOR REFUND FILE
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
     C                   MOVEL     'WMRRP'       XXFILE           10
     C                   MOVE      *BLANK        XXDESC
     C                   EXCEPT    DET
      *
      *  CLAIM TRANSFER TRANSACTION FILE
      *
     C     XXKEY         CHAIN     PCT01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCT01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WACTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  DEDUCTIBLE DETAIL FILE
      *
     C     XXKEY         CHAIN     PDD02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDD02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDD02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDDDP'       XXFILE           10
     C                   EXCEPT    DET
      *
      ***** BEG ADD ******* 02/03/98 *********************************
      *
      *  Claim Deletion File
      *
     C     XXKEY         CHAIN     PCM04                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCM04
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCM04                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WACMP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Diary Transfer File
      *
     C     XXKEY         CHAIN     PDT01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDT01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WADTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Auxiliary Compensation Payee
      *
     C     XXKEY         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAC01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDACP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Claimant Changes Tracking File
      *
     C     XXKEY         CHAIN     PNT01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNT01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PNT01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDNTP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Average Weekly Wage
      *
     C     XXKEY         CHAIN     PAW01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAW01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAW01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMAWP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Benefit History File
      *
     C     XXKEY         CHAIN     PBH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PBH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PBH01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMBHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Benefit Adjustments
      *
     C     XXKEY         CHAIN     PBN01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PBN01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PBN01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMBNP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Body Part Record
      *
     C     XXKEY         CHAIN     PBY01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PBY01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PBY01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMBYP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EDI  Record
      *
     C     XXKEY         CHAIN     PEI01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEI01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PEI01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMEIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  EDI Log History File
      *
     C     XXKEY         CHAIN     PLG01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLG01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PLG01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMLGP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Claim Legal File
      *
     C     XXKEY         CHAIN     PLL01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLL01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PLL01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMLLP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  State NOI Record
      *
     C     XXKEY         CHAIN     PNI01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNI01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PNI01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMNIP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Treating Phys History
      *
     C     XXKEY         CHAIN     PTP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PTP01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMTPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      ***** END ADD ******* 02/03/98 *********************************
     *** Beg Add *** 06/14/23 *** 114WRJ ****************************
      *
      * WMVIP - Visit Tracking
      *
     C     XXKEY         CHAIN     PVI05                              60
     C     *IN60         DOWEQ     '0'
     C                   If        VIcase <> 0
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
     *** End Add *** 06/14/23 *** 114WRJ ****************************
      *
     C***** BEGIN ADD ***** 04/15/04 *********************************
     C*---------------------------------------------------------------
     C*  Claims Denial History
     C*---------------------------------------------------------------
     C     XXKEY         CHAIN     PDH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PDH01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WADHL1'      XXFILE
     C                   EXCEPT    DET
     C*
     C*---------------------------------------------------------------
     C*  Index Claim Diary Suspense
     C*---------------------------------------------------------------
     C     XXKEY         CHAIN     PID01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PID01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PID01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WAIDL'       XXFILE
     C                   EXCEPT    DET
     C*
     C*---------------------------------------------------------------
     C*  EDI Reduced Earnings File
     C*---------------------------------------------------------------
     C     XXKEY         CHAIN     PRE01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRE01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRE01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WEREL'       XXFILE
     C                   EXCEPT    DET
     C*
     C*---------------------------------------------------------------
     C*  Claim Auxiliary File
     C*---------------------------------------------------------------
     C     XXKEY         CHAIN     PC101                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PC101
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PC101                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMC1L'       XXFILE
     C                   EXCEPT    DET
     C*
     C*---------------------------------------------------------------
     C*  Excess Claim Diary
     C*---------------------------------------------------------------
     C     XXKEY         CHAIN     PED01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PED01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PED01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMEDL'       XXFILE
     C                   EXCEPT    DET
     C*
     C*---------------------------------------------------------------
     C*  EFT Account File
     C*---------------------------------------------------------------
     C                   READ      PEF01                                  60
     C     *IN60         DOWEQ     '0'
     C     EFCO#         IFEQ      XXCO#
     C     EFFND         ANDEQ     XXFND
     C                   DELETE    PEF01
     C                   ADD       1             XX#REC
     C                   ENDIF
     C                   READ      PEF01                                  60                R
     C                   ENDDO
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WDEFP'       XXFILE
     C                   EXCEPT    DET
     C*
     C*---------------------------------------------------------------
     C*  Claim Injury Type History
     C*---------------------------------------------------------------
     C     XXKEY         CHAIN     PIH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PIH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PIH01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMIHL'       XXFILE
     C                   EXCEPT    DET
     C*
     C*---------------------------------------------------------------
     C*  Claim Trial Information
     C*---------------------------------------------------------------
     C     XXKEY         CHAIN     PTI01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTI01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PTI01                                  60
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL(P)  'WMTIL'       XXFILE
     C                   EXCEPT    DET
     C*
     C*
     C***** END ADD ******* 04/15/04 *********************************
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
      ****************************************************************
      *     HEADING
      ****************************************************************
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
     O                                           48 '# DELETED RECORDS'
      ****************************************************************
      *     DETAIL LINE WITH FILE NAME AND COUNT
      ****************************************************************
     O          E            DET            1
     O                       XXFILE              19
     O                                           21 '-'
     O                       XXDESC              32
     O                       XX#REC        ZB    42
      ****************************************************************
      *     TOTAL RECORD COUNT FOR ALL FILES
      ****************************************************************
     O          E            TOTL        2  2
     O                                           18 'ALL FILES'
     O                       XXTREC        Z     42
