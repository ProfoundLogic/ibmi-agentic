     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8603R - DAILY CLAIMS REGISTER
     H*****************************************************************
     H*  REVISIONS:                                                   *
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
     H*  12/14/99  C:15  Project:  WC577                              *
     H*                  Y2K - Fixed date conversion parm list.       *
     H*                                                               *
     H*  03/01/99  C:118 PROJECT:  WC577                              *
     H*                  ADJUSTED 'ENTERED' COLUMN HEADING INTO       *
     H*                  PROPER ALIGNMENT                             *
     H*                                                               *
     H*  02/26/99  C:118 PROJECT:  WC577                              *
     H*                  ADJUSTED 'ACCIDENT', 'DATE', 'RECEIVED'      *
     H*                  COLUMN HEADINGS INTO PROPER ALIGNMENT        *
     H*                                                               *
     H*  01/05/99  C:90  SEQ#: WA096/15609                            *
     H*                  LOAD CORRECT ADJUSTER NAME                   *
     H*                                                               *
     H*  06/09/98  C:33  SEQ#: WC033/12143                            *
     H*                  LOAD CORRECT EMPLOYER NAME                   *
     H*                                                               *
     H*  03/06/98  C:78  PROJECT: WC577                               *
     H*            PRINT EMPLOYER NAME AFTER EMPLOYER #               *
     H*                                                               *
     H*  02/07/94  C:25  -  PROJ: WC273                               *
     H*            1. CHANGED THE RECEIVED DATE TITLE TO DATE ENTERED *
     H*               TITLE                                           *
     H*            2. ADDED NEW COLUMN FOR RECEIVED DATE TO PRINT     *
     H*****************************************************************
     F*
     F* * * COMPANY FILE
     FSMCOL     IF   E           K DISK
     F*
     F* * * BATCH ROUTING FILE
     FSMORL     IF   E           K DISK
     F*
     F* * * USER PARAMETER FILE
     FWMUPL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F* * * DAILY CLAIM REGISTER WORKFILE
     FWXDCL     IF   E           K DISK
     F*
     F****** BEGIN ADD **** 03/06/98 **********************************
     F* * * EMPLOYER MASTER FILE
     FWMEML     IF   E           K DISK
     F****** END   ADD **** 03/06/98 **********************************
     F*
     F****** BEGIN ADD **** 03/06/98 **********************************
     F****** &QSYSPRT O   F     155     OF     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D**** END   ADD **** 01/05/99 ************************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F****** END   ADD **** 03/06/98 **********************************
     F*

     I**** BEGIN ADD **** 01/05/99 ************************************
     I*
     IPUP02
     I              UPUSNM                      U2USNM

     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     F****** BEGIN ADD **** 03/06/98 **********************************
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER
     C     EMKEY         KLIST
     C                   KFLD                    COCO#
     C                   KFLD                    DCFND
     C                   KFLD                    DCEMP#
     C                   KFLD                    DCDIV
     F****** END   ADD **** 03/06/98 **********************************
     C*
     C*   KEY LIST FOR THE USER PARAMETERS FILE.
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY1        KLIST
     C                   KFLD                    DCCLEX            3
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C*
     C     DT6MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C*** BEGIN DLT *** 12/14/99 **************************************
     C******************** PARM '*YMD'    SRFFMT
     C******************** PARM '*MDYY'   SRTFMT
     C*** END DLT ***** 12/14/99 **************************************
     C*** BEGIN ADD *** 12/14/99 **************************************
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C*** END ADD ***** 12/14/99 **************************************
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C***  BEGIN ADD  ***  06/09/98  *********************************
     C     *LIKE         DEFINE    DCEMP#        SVEMP#
     C     *LIKE         DEFINE    DCDIV         SVDIV
     C**** BEGIN ADD **** 01/05/99 ************************************
     C     *LIKE         DEFINE    DCFND         SVFND
     C**** END   ADD **** 01/05/99 ************************************
     C***  END   ADD  ***  06/09/98  *********************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PDC01                                  60
     C*
     C*  IF NO RECORDS FOUND, PRINT MESSAGE
     C*
     C     *IN60         IFEQ      '1'
     C*
     C                   EXSR      SR1ST
     C                   EXCEPT    NORCD
     C*
     C                   ENDIF                                                  *IN60 IFNE *ZERO
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* * * INITAIL SET-UP
     C  N11              EXSR      SR1ST
     C*
     C***  BEGIN ADD  ***  06/09/98  *********************************
     C     DCEMP#        IFNE      SVEMP#
     C     DCDIV         ORNE      SVDIV
     C**** BEGIN ADD **** 01/05/99 ************************************
     C     DCFND         ORNE      SVFND
     C**** END   ADD **** 01/05/99 ************************************
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   Z-ADD     DCEMP#        SVEMP#
     C                   Z-ADD     DCDIV         SVDIV
     C**** BEGIN ADD **** 01/05/99 ************************************
     C                   Z-ADD     DCFND         SVFND
     C**** END   ADD **** 01/05/99 ************************************
     C*
     C                   ENDIF                                                  DCEMP#<>SVEMP#
     C***  END   ADD  ***  06/09/98  *********************************
     C*
     C* * *       CHECK FOR LEVEL BRAKE
     C     DCCLEX        IFNE      XXCLEX
     C                   EXSR      SRUBRK
     C                   END
     C*
     C                   Z-ADD     DCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C                   Z-ADD     DCENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXENTD            8 0
     C*
     C                   Z-ADD     DCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCVD            8 0
     C*
     C                   Z-ADD     DCCLOS        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCLOS            8 0
     C*
     C*
     C                   MOVEL     DCNAME        XXNAME           30
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    DET
     C*
     C* * *       READ THE NEXT RECORD.
     C*          CMKEY     READEPCM01                    60
     C                   READ      PDC01                                  60
     C                   ENDDO                                                  *IN60 DOEQ *OFF
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C* * *            END OF JOB.
     C     EOJ           TAG
     C                   MOVE      '1'           *INLR
     C*=========================================================**
     C*   SET-UP / INITIALIZE          S R 1 S T                **
     C*=========================================================**
     CSR   SR1ST         BEGSR
     C*
     C     UPKEY         CHAIN     PUP02                              60
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*  Convert the entry dates from MDY to MDCY, if necessary.
     C*
     C     X1FRDT        IFLT      1000000
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         X1FRDT
     C                   END
     C*
     C     X1TODT        IFLT      1000000
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         X1TODT
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* CENTER TEXT IN FIELD CONAME
     C*
     C                   MOVEL     CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXNAM1           40
     C***  BEGIN CHG  ***  06/09/98  *********************************
     C**********
     C**CENTER*TEXT IN FIELD ORTITL
     C**********
     C**********           MOVELORTITL    ##DATA
     C**********
     C**********           CALL 'SRCENTER'
     C**********           PARM           ##DATA 80
     C**********           PARM 60        ##LENG  30
     C**********
     C**********           MOVEL##DATA    XXTITL 60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C***  END   CHG  ***  06/09/98  *********************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION                           D.
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C***  BEGIN ADD  ***  06/09/98  *********************************
     C*
     C* CENTER TEXT IN FIELD ORTITL
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C***  END   ADD  ***  06/09/98  *********************************
     C*
     C                   END
     C     UPKEY1        CHAIN     PUP01                              60
     C*** BEGIN ADD *** 03/06/98 **************************************
     C*** GET EMPLOYER NAME
     C     EMKEY         CHAIN     PEM01                              60
     C*** END   ADD *** 03/06/98 **************************************
     C***  BEGIN ADD  ***  06/09/98  *********************************
     C                   Z-ADD     DCEMP#        SVEMP#
     C                   Z-ADD     DCDIV         SVDIV
     C**** BEGIN ADD **** 01/05/99 ************************************
     C                   Z-ADD     DCFND         SVFND
     C**** END   ADD **** 01/05/99 ************************************
     C***  END   ADD  ***  06/09/98  *********************************
     C*
     C                   MOVE      DCCLEX        XXCLEX            3
     C                   EXCEPT    HEAD
     C                   MOVE      '1'           *IN11
     C                   ENDSR
     C*=========================================================**
     C*   LEVEL BREAK ON DCCLEX        S R U B R K              **
     C*=========================================================**
     CSR   SRUBRK        BEGSR
     C     UPKEY1        CHAIN     PUP01                              60
     C                   MOVE      DCCLEX        XXCLEX
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   ENDSR
     C*
     O**************************************************************************
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
     O**********             UPUSNM              67
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
     O                       UPUSNM              67
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***  BEGIN CHG  ***  06/09/98  *********************************
     O**********               XXTITL    79
     O                       ORTITL              80
     O***  END   CHG  ***  06/09/98  *********************************
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
     O                                           29 'CLAIM ENTRY DATE RANGE:'
     O                       X1FRDT              40 '  /  /    '
     O                                           42 '-'
     O                       X1TODT              53 '  /  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O******* BEGIN CHG ***** 03/06/98 ********************************
     O**********************   XXNAM1    98
     O**********************   DSDATEY  149
     O                       XXNAM1             118
     O                       DSDATE        Y    198
     O******* END   CHG ***** 03/06/98 ********************************
     O          E            HEAD           1
     O                       Q1PGM               10
     O******* BEGIN CHG ***** 03/06/98 ********************************
     O**********************   XXTITL   108
     O**********************            139 'PAGE:'
     O**********************   PAGE  Z  149
     O                       XXTITL             128
     O                                          188 'PAGE:'
     O                       PAGE          Z    198
     O******* END   CHG ***** 03/06/98 ********************************
     O          E            HEAD           1
     O******* BEGIN CHG ***** 03/06/98 ********************************
     O*******************               125 'CLAIM ENTRY DATE RANGE:'
     O*******************      X1FRDT   136 '  /  /    '
     O*******************               138 '-'
     O*******************      X1TODT   149 '  /  /    '
     O                                          174 'CLAIM ENTRY DATE RANGE:'
     O                       X1FRDT             185 '  /  /    '
     O                                          187 '-'
     O                       X1TODT             198 '  /  /    '
     O******* END   CHG ***** 03/06/98 ********************************
     O          E            HEAD           2
     O                                            9 'ADJUSTER:'
     O                       UPUSNM              62
     O          E            HEAD           1
     O******* BEGIN CHG *****  02/26/99 *******************************
     O*
     O**********                         94 'ACCIDENT'
     O**********                        104 'DATE'
     O**********                        120 'RECEIVED'
     O*
     O                                          135 'ACCIDENT'
     O                                          147 'DATE'
     O                                          161 'RECEIVED'
     O******* END CHG ***** 02/26/99 *********************************
     O          E            HEAD           2
     O                                            5 'GROUP'
     O                                           10 'YR'
     O******* BEGIN CHG ***** 03/06/98 ********************************
     O***********                        21 'EMPLR#'
     O***********                        28 'DIV'
     O***********                        37 'CASE'
     O***********                        54 'CLAIMANT NAME'
     O***********                        75 'TYPE'
     O***********                        82 'NCCI'
     O***********                        92 'DATE'
     O***********                       106 'ENTERED'
     O***********                       118 'DATE'
     O***********                       135 'CLOSE DATE'
     O***********                       149 'ENTERED BY'
     O                                           20 'EMPLOYER#'
     O                                           26 'DIV'
     O******* BEGIN ADD ***** 03/06/98 ********************************
     O                                           40 'EMPLOYER NAME'
     O******* END   ADD ***** 03/06/98 ********************************
     O                                           79 'CASE'
     O                                           95 'CLAIMANT NAME'
     O                                          116 'TYPE'
     O                                          123 'NCCI'
     O                                          133 'DATE'
     O******* BEGIN CHG ***** 03/01/99 ********************************
     O**********                        147 'ENTERED'
     O                                          148 'ENTERED'
     O******* END CHG ******* 03/01/99 ********************************
     O                                          159 'DATE'
     O                                          176 'CLOSE DATE'
     O                                          190 'ENTERED BY'
     O******* END   CHG ***** 03/06/98 ********************************
     O          E            DET            1
     O                       DCFND         Z      5
     O                       DCFYR         Z     10
     O******* BEGIN CHG ***** 03/06/98 ********************************
     O*******                  DCEMP#Z   21
     O*******                  DCDIV Z   28
     O*******                  DCCASEZ   37
     O*******                  XXNAME    71
     O*******                  DCLTFL    74
     O*******                  DCNCCI    81
     O*******                  XXACDT    95 '  /  /    '
     O*******                  XXENTD   108 '  /  /    '
     O*******                  XXRCVD   121 '  /  /    '
     O*******                  XXCLOS   135 '  /  /    '
     O*******                  DCIUSR   149
     O*
     O                       DCEMP#        Z     20
     O                       DCDIV         Z     26
     O******* BEGIN ADD ***** 03/06/98 ********************************
     O                       EMNAME              67
     O******* END   ADD ***** 03/06/98 ********************************
     O                       DCCASE        Z     78
     O                       XXNAME             112
     O                       DCLTFL             115
     O                       DCNCCI             122
     O                       XXACDT             136 '  /  /    '
     O                       XXENTD             149 '  /  /    '
     O                       XXRCVD             162 '  /  /    '
     O                       XXCLOS             176 '  /  /    '
     O                       DCIUSR             190
     O******* END   CHG ***** 03/06/98 ********************************
     O          E            NORCD          1
     O                                           67 'NO RECORDS MEET '
     O                                           77 'SELECTION '
     O                                           85 'CRITERIA'
