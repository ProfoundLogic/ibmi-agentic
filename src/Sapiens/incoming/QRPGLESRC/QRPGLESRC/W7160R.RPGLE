     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7160R - CLAIMS CURRENT RESERVE REPORT                  *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  05/05/00  C:15  Support: WC096/22751                        *
     H*                  Recompiled pgm due to chg to WMRHL3.        *
     H*                                                              *
     H*  06/28/99  C:118 PROJECT: WC893
     H*                  Add a line in the header for the Employer
     H*                  number, Division and Name, directly below
     H*                  the line with Case and Claimant.
     H*
     H*  10/19/95  C:33  PROJECT# WC455                              *
     H*                 -ADJUSTED OUTPUT FOR READABILITY             *
     H*                 -CLEAR DETAIL ARRAYS AFTER PRINTING TO AVOID *
     H*                  CARRY OVER                                  *
     H****************************************************************
     FWMRHL3    IF   E           K DISK
     FWMRHL1    IF   E           K DISK
     F                                     RENAME(PRH01:PRH02)
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F***** BEGIN ADD **** 06/28/99 ***********************************
     FWMEML     IF   E           K DISK
     F***** END   ADD **** 06/28/99 ***********************************
     F***** BEGIN CHG **** 06/28/99 ***********************************
     F*QSYSPRT O  F     173     OA     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F***** END   CHG **** 06/28/99 ***********************************

     D RT              S              2    DIM(10)
     D SU              S              9  2 DIM(10)
     D RTL             S              2    DIM(10)
     D RTT             S              1    DIM(10)
     D SUL             S              9  2 DIM(10)
     D RT4             S              2    DIM(4)
     D RTT4            S              1    DIM(4)
     D SU4             S              9  2 DIM(4)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RTSU                   1    110
     D                                     DIM(10) ASCEND
     D  RHRT01                 1      2
     D  RHSU01                 3     11  2
     D  RHRT02                12     13
     D  RHSU02                14     22  2
     D  RHRT03                23     24
     D  RHSU03                25     33  2
     D  RHRT04                34     35
     D  RHSU04                36     44  2
     D  RHRT05                45     46
     D  RHSU05                47     55  2
     D  RHRT06                56     57
     D  RHSU06                58     66  2
     D  RHRT07                67     68
     D  RHSU07                69     77  2
     D  RHRT08                78     79
     D  RHSU08                80     88  2
     D  RHRT09                89     90
     D  RHSU09                91     99  2
     D  RHRT10               100    101
     D  RHSU10               102    110  2
     D                 DS
     D  RHFYR                  1      3  0
     D  XXCNTR                 1      1  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    CMDT          XXDATE
     C***** BEGIN ADD **** 06/28/99 ***********************************
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    CMEMP#        ##EMP#
     C     *LIKE         DEFINE    CMDIV         ##DIV
     C     *LIKE         DEFINE    EMNAME        ##NAME
     C***** END   ADD **** 06/28/99 ***********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQMODE            1
     C                   PARM                    QQUSR            10
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
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR RESERVE MASTER FILE, FIND ALL RECORDS THAT HAVE THE
     C*   "DATE MAINTAINED" EQUAL TO THE "PASSED" PARM DATE.
     C*
     C     RHKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDATE
     C*
     C*   KEY LIST FOR RESERVE MASTER FILE, FIND ALL RECORDS FOR A GIVEN
     C*   CASE THAT HAS AN EQUAL "DATE MAINTAINED".
     C*
     C     RHKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIMS MASTER FILE.
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE.
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE TABLE FILE.
     C*
     C     XTKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR THE USER PARAMETERS FILE.
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR
     C*
     C***** BEGIN ADD **** 06/28/99 ***********************************
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE.
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C***** END   ADD **** 06/28/99 ***********************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQUSR         XXUSR            10
     C*
     C*   CHAIN TO THE COMPANY FILE. IF NOT FOUND, ABEND.
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
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C*
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN CHG **** 06/28/99 ***********************************
     C********** ORSTAT    ANDEQ'A'
     C     ORSTAT        ANDNE     'I'
     C***** BEGIN CHG **** 06/28/99 ***********************************
     C*
     C***** BEGIN ADD **** 06/28/99 ***********************************
     C*
     C*   LEFT JUSTIFY TITLE ON ROUTING PAGE
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF                                                  N LE 60
     C                   ENDIF                                                  ORTITL NE BL
     C*
     C***** END   ADD **** 06/28/99 ***********************************
     C*
     C*   IF THIS REPORT IS A DEMAND, BLANK OUT ALL DISBURSEMENT INFO
     C*   FIELDS AND PRINT THE USER'S NAME INSTEAD. IF A BATCH REPORT,
     C*   PRINT NORMAL DISTRIBUTION.
     C*
     C     XXMODE        IFEQ      'D'
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        ORDIS1
     C                   ELSE
     C                   MOVE      *BLANK        ORDIS1
     C                   END
     C                   MOVE      *BLANK        ORDIS2
     C                   MOVE      *BLANK        ORDIS3
     C                   MOVE      *BLANK        ORDIS4
     C                   END
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C***** BEGIN ADD **** 06/28/99 ***********************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
     C*
     C*  CENTER COMPANY NAME IN DISPLAY FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C***** END   ADD **** 06/28/99 ***********************************
     C                   TIME                    DSDTTM
     C*
     C*   GET ALL THE RECORDS FOR THE SELECTED COMPANY AND DATE.
     C*
     C     RHKEY1        SETLL     PRH01
     C     RHKEY1        READE     PRH01                                  59
     C*
     C     *IN59         DOWEQ     '0'
     C*
     C                   MOVE      RHFND         XXFND             3 0
     C                   MOVE      RHFYR         XXFYR             3 0
     C                   MOVE      RHCASE        XXCASE            7 0
     C                   MOVE      *ZERO         SUL
     C*
     C     RHKEY2        SETLL     PRH02
     C     RHKEY2        READE     PRH02                                  58
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*   FIND THE CENTURY, SETUP CENTURY/YEAR.
     C*
     C                   MOVE      RHFYR         XXCNYR            4 0
     C     XXCNTR        IFEQ      0
     C                   MOVEL     19            XXCNYR
     C                   ELSE
     C                   MOVEL     20            XXCNYR
     C                   END
     C*
     C*   GET THE FUND NAME.
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        FNNAME
     C                   END
     C*
     C*   GET THE CLAIMANT'S NAME.
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 06/28/99 ***********************************
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C***** END   ADD **** 06/28/99 ***********************************
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XXFRLS           56
     C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXFRLS
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *BLANK        XXFRLS
     C                   END
     C*
     C***** BEGIN ADD **** 06/28/99 ***********************************
     C*
     C*   GET THE EMPLOYER'S NAME
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMNAME        XXNAME
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*   COMPRESS EMP#/DIV/NAME FOR HEADER
     C*
     C                   CALL      'WA005R'
     C                   PARM      CMEMP#        ##EMP#
     C                   PARM      CMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME
     C                   PARM                    ##EDN            80
     C*
     C                   MOVEL     ##EDN         XXEDN            80
     C*
     C***** END   ADD **** 06/28/99 ***********************************
     C*
     C*   OUTPUT STANDARD, FUND AND CLAIMANT HEADINGS.
     C*
     C                   MOVE      '1'           *INOA
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C                   END
     C*
     C     *IN58         DOWEQ     '0'
     C*
     C*   SORT THE CODE AND CHANGED AMOUNT ARRAY.
     C*
     C                   SORTA     RTSU
     C                   MOVE      *BLANK        RTT
     C                   MOVE      *ZERO         SU
     C*
     C*   SEPERATE THE CODE/CHG.AMT ARRAY INTO THEIR OWN ARRAYS. DO NOT
     C*   LOAD OUTPUT ARRAYS IF CODE IS BLANK.
     C*
     C                   Z-ADD     0             Y                 3 0
     C                   DO        10            X                 3 0
     C                   MOVEL     RTSU(X)       XXTRT             2
     C     XXTRT         IFNE      *BLANK
     C                   ADD       1             Y
     C                   MOVEL     XXTRT         XXKEY
     C     XTKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        RTT(Y)
     C                   ELSE
     C                   MOVE      RTTYPE        RTT(Y)
     C                   END
     C                   MOVEL     XXTRT         RT(Y)
     C                   MOVE      RTSU(X)       SU(Y)
     C                   Z-ADD     1             Z
     C     RT(Y)         LOOKUP    RTL(Z)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       SU(Y)         SUL(Z)
     C                   ELSE
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    RTL(Z)                                 60
     C     *IN60         IFEQ      '1'
     C                   MOVE      RT(Y)         RTL(Z)
     C                   MOVE      SU(Y)         SUL(Z)
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   Z-ADD     RHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C*
     C*   DETERMINE THE # OF VALID RESERVE CHANGES USING "Y". LOAD THE
     C*   ARRAYS FOR PRINTING 4 LINE ACROSS. IF ALL 4 ARE LOADED, PRINT
     C*   THE LINE AND RESET FOR NEXT 4. IF THE NUMBER OF ELEMENTS ARE
     C*   LESS THAN 4, THEY WILL BE PRINTED AFTER THE LOOP ENDS.
     C*
     C                   Z-ADD     1             Z                 3 0
     C                   Z-ADD     1             X
     C                   MOVE      *BLANK        RT4
     C                   MOVE      *BLANK        RTT4
     C                   MOVE      *ZERO         SU4
     C*
     C     Z             DOWLE     Y
     C*
     C                   MOVE      RT(Z)         RT4(X)
     C                   MOVE      RTT(Z)        RTT4(X)
     C                   MOVE      SU(Z)         SU4(X)
     C     X             IFEQ      4
     C   OA              EXCEPT    HEAD
     C   OA              EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DETTOT
     C                   MOVE      *BLANKS       RT4
     C                   MOVE      *BLANKS       RTT4
     C                   Z-ADD     *ZEROS        SU4
     C                   Z-ADD     0             X
     C                   END
     C*
     C                   ADD       1             X
     C                   ADD       1             Z
     C                   END
     C*
     C*   PRINT THE ARRAYS IF THEY CONTAIN DATA.
     C*
     C     X             IFGT      1
     C   OA              EXCEPT    HEAD
     C   OA              EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DETTOT
     C                   MOVE      *BLANKS       RT4
     C                   MOVE      *BLANKS       RTT4
     C                   Z-ADD     *ZEROS        SU4
     C                   END
     C*
     C*   READ THE NEXT DETAIL RECORD.
     C*
     C     RHKEY2        READE     PRH02                                  58
     C                   END
     C*
     C*   REACHED THE END OF THIS CASE. LOAD ACCUMULATED RESERVE ARRAYS
     C*   INTO THE SAME ARRAY THAT WAS LOADED FROM THE FILE.
     C*
     C                   MOVE      *BLANK        RTSU
     C                   DO        10            X
     C                   MOVEL     RTL(X)        RTSU(X)
     C                   MOVE      SUL(X)        RTSU(X)
     C                   END
     C*
     C                   MOVE      *BLANK        RTL
     C                   MOVE      *ZERO         SUL
     C*
     C                   SORTA     RTSU
     C*
     C*   MOVE THE SORTED ARRAY ELEMENTS BACK INTO THE SAME ARRAYS THAT
     C*   WERE USED FOR DETAIL PRINTING.
     C*
     C                   MOVE      *BLANK        RT
     C                   MOVE      *BLANK        RTT
     C                   MOVE      *ZERO         SU
     C                   Z-ADD     0             Y                 3 0
     C                   DO        10            X                 3 0
     C                   MOVEL     RTSU(X)       XXTRT             2
     C                   MOVE      RTSU(X)       XXTSU             9 2
     C     XXTSU         IFNE      *ZERO
     C                   ADD       1             Y
     C                   MOVEL     XXTRT         XXKEY
     C     XTKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        RTT(Y)
     C                   ELSE
     C                   MOVE      RTTYPE        RTT(Y)
     C                   END
     C                   MOVEL     XXTRT         RT(Y)
     C                   MOVE      RTSU(X)       SU(Y)
     C                   END
     C                   END
     C*
     C*   OUTPUT THE CASE'S CURRENT RESERVE TOTALS.
     C*
     C                   Z-ADD     1             Z
     C                   Z-ADD     1             X
     C                   MOVE      *BLANK        RT4
     C                   MOVE      *BLANK        RTT4
     C                   MOVE      *ZERO         SU4
     C*
     C*   OUTPUT TOTAL HEADING, IF DATA WAS LOADED INTO ARRAY.
     C*
     C     Y             IFGT      0
     C   OA              EXCEPT    HEAD
     C   OA              EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTHD1
     C                   END
     C*
     C     Z             DOWLE     Y
     C*
     C                   MOVE      RT(Z)         RT4(X)
     C                   MOVE      RTT(Z)        RTT4(X)
     C                   MOVE      SU(Z)         SU4(X)
     C     X             IFEQ      4
     C   OA              EXCEPT    HEAD
     C   OA              EXCEPT    TOTHD2
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DETTOT
     C                   MOVE      *BLANKS       RT4
     C                   MOVE      *BLANKS       RTT4
     C                   Z-ADD     *ZEROS        SU4
     C                   Z-ADD     0             X
     C                   END
     C*
     C                   ADD       1             X
     C                   ADD       1             Z
     C                   END
     C*
     C     X             IFGT      1
     C   OA              EXCEPT    HEAD
     C   OA              EXCEPT    DET
     C   OA              EXCEPT    TOTHD2
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DETTOT
     C                   MOVE      *BLANKS       RT4
     C                   MOVE      *BLANKS       RTT4
     C                   Z-ADD     *ZEROS        SU4
     C                   END
     C*
     C*   READ THE NEXT CASE RECORD. CHECK THAT CASE IS DIFFERENT. IF NOT,
     C*   READ ANOTHER RECORD.
     C*
     C                   MOVE      RHCASE        X1CASE            7 0
     C     AGN           TAG
     C     RHKEY1        READE     PRH01                                  59
     C     *IN59         IFEQ      '0'
     C     RHCASE        ANDEQ     X1CASE
     C                   GOTO      AGN
     C                   END
     C*
     C     RHCASE        IFNE      X1CASE
     C                   Z-ADD     0             PAGE
     C                   END
     C*
     C                   END
     C*
     C*
     C*   END OF JOB.
     C*
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
     C*
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
     O***** BEGIN CHG **** 06/28/99 ***********************************
     O**********                         23 '(C) COPYRIGHT PROGRESSI'
     O**********                         46 'VE DATA SOLUTIONS. INC,'
     O**********                         57 '.1996      '
     O******* E  106           HEAD
     O**********               DSTIME    10 '  :  :  '
     O**********               CONAME   105
     O**********               X1DATEY  173
     O******* E  1             HEAD
     O**********                          8 'W7160R'
     O**********               ORTITL   115
     O**********                        169 'PAGE:'
     O**********               PAGE  Z  173
     O******* E 11             HEAD
     O**********                          7 'FUND:'
     O**********               FNFND Z   11
     O**********               FNNAME    72
     O******* E  2             HEAD
     O**********                         12 'FUND YEAR:'
     O**********               XXCNYR    17
     O******* E  2             HEAD
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS, INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              10 '  :  :  '
     O                       CONAME             120
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               12
     O                       ORTITL             130
     O                                          193 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD        1  1
     O                                            8 'GROUP:'
     O                       FNFND         Z     12
     O                       FNNAME              73
     O          E            HEAD           2
     O                                           14 'POLICY YEAR:'
     O                       XXCNYR              19
     O          E            HEAD           1
     O***** END   CHG **** 06/28/99 ***********************************
     O                                            7 'CASE:'
     O                       RHCASE        Z     15
     O                                           26 'CLAIMANT:'
     O                       XXFRLS              83
     O***** BEGIN ADD **** 06/28/99 ***********************************
     O          E            HEAD           2
     O                                           11 'EMPLOYER:'
     O                       XXEDN               93
     O***** END   ADD **** 06/28/99 ***********************************
     O          E            HEAD           1
     O                                           10 'EFF DATE'
     O                                           34 'EXPLANATION FOR CHANGE'
     O                                           91 'RESERVE CODES AND'
     O                                          106 'AMOUNT CHANGED'
     O*
     O          E            DET         1  0
     O                       XXEFDT        Y     10
     O                       RHEXPL              72
     O          E            DETTOT         1
     O                       RT4(1)              76
     O                       RTT4(1)             78
     O                       SU4(1)        K     92
     O                       RT4(2)             103
     O                       RTT4(2)            105
     O                       SU4(2)        K    119
     O                       RT4(3)             130
     O                       RTT4(3)            132
     O                       SU4(3)        K    146
     O                       RT4(4)             156
     O                       RTT4(4)            158
     O                       SU4(4)        K    173
     O          E            TOTHD1      2  2
     O                                           87 '* * * CURRENT'
     O                                          108 'RESERVE TOTALS * * *'
     O          E            TOTHD2      1  2
     O                                           95 '* * * CURRENT RESERVE'
     O                                          116 'TOTALS (CONT.) * * *'
