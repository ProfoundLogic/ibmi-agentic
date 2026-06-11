     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*/TITLE SC737R - DARTS DIARY LISTING  - PRINT PROGRAM           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H**                                                             **
     H**   PROGRAM DESCRIPTION:  THIS PROGRAM GENERATES A DIARY RE-  **
     H**PORT FOR ONE OR ALL DIARY USERS WHICH IS SORTED BY CLAIMANT  **
     H**LAST NAME, KEY, OR REMARK DATE UP TO THE DIARY DATE SPECIFIED**
     H**ADDITIONAL CRITERIA = ONE OR ALL REMARK CODES, AND THE DIARY **
     H**STATUS OF COMPLETE (CMDCDT NE BLANKS),NONCOMPLETE, OR BOTH.  **
     H**   CALLED FROM:                                              **
     H**     SC736C                                                  **
     H**   CALLS TO:                                                 **
     H**     ------                                                  **
     H**   PROGRAMMER:  TOM ANZALONE                                 **
     H**   CREATED:  09/16/91       PROJECT:  AIT                    **
     H**                                                             **
     H****************************************************************
     H** REVISIONS:                                                  **
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:XXX  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  03/08/00 C:113 PROJECT: DR098                                *
     H*                 Changed width of qsysprt and adjusted O specs *
     H*                 to print portrait.                            *
     H*                                                               *
     H** 04/27/92 KRR   PROJECT: AJP                                 **
     H**                CHANGE REFERENCE FIELDS FROM CMCODE(2A) TO   **
     H**                CMRMCD(5A).                                  **
     H****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*       61      READ ON SMARL                                   *
     H*       70      CASEQ SUBROUTINE INDICATOR                      *
      *****************************************************************
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    WXDDP                                                    **
     H**    WMUPL (PUP01): USER                                      **
     H**    SMCOL (PCO01): CO                                        **
     H**    SMORL (PCO01): CO/PGM/PRT                                **
     H**    SMARL (PAR01): KEY/SEQ#/ASQ# STAT NE 'I'                 **
     H*****************************************************************
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWXDDP     IF   E           K DISK
     FSMARL     IF   E           K DISK
     F*** BEGIN CHG *** 03/08/00 ***********************************************
     F***QSYSPRT O   F     128     OF     PRINTER      KINFDS PRTDS
     FQSYSPRT   O    F  120        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*** END   CHG *** 03/08/00 ***********************************************
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*  SYSTEM DATE AND  TIME DATA STRUCTURE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D                 DS
     D  QQKEY                  1     40
     D  Q1CO#                  1      3  0
     D  Q1FND                  4      6  0
     D  Q1FYR                  7      9  0
     D  Q1CASE                10     16  0
     D****************************************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     d***** Beg Add *** 06/09/20 *******************************************
     d yyss#           s              9    inz
     d***** End Add *** 06/09/20 *******************************************

     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    COCO#         XYCO#
     C     *LIKE         DEFINE    Q1CO#         XXCO#
     C     *LIKE         DEFINE    Q1FND         XXFND
     C     *LIKE         DEFINE    Q1FYR         XXFYR
     C     *LIKE         DEFINE    Q1CASE        XXCASE
     C     *LIKE         DEFINE    DDSS#         XXSS#
     C     *LIKE         DEFINE    DDLSN         XXLSN
     C     *LIKE         DEFINE    DDFSN         XXFSN
     C     *LIKE         DEFINE    DDMID         XXMID
     C     *LIKE         DEFINE    DDUSER        XXUSER
     C     *LIKE         DEFINE    DDCNAM        XXCNAM
     C     *LIKE         DEFINE    ARCM75        XXCM75
     C     *LIKE         DEFINE    DDSEQ#        XXSEQ#
     C*
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XYCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CKEY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*  ADDITIONAL REMARKS FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    DDKEY
     C                   KFLD                    XXSEQ#
     C                   KFLD                    XXASQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDUSR           10
     C                   PARM                    QQCODE            5
     C                   PARM                    QQSTAT            1
     C                   PARM                    QQCDAT            8
     C                   PARM                    QQSORT            1
     C                   PARM                    QQREC             1
     C                   PARM                    QQQUE            10
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
     C                   MOVE      QQCO#         XYCO#
     C                   TIME                    DSDTTM
     C*
     C* ESTABLISH ROUTING SELECTION FOR USER, & CODE
     C                   MOVE      *BLANK        RSDUSR           10
     C                   MOVE      *BLANK        RSCODE            5
     C                   MOVE      *BLANK        RSCDAT            8 0
     C*
     C                   MOVE      *BLANK        RSSTAT            1
     C                   MOVE      *BLANK        RSSORT            1
     C                   MOVE      *BLANK        RSREC             1
     C     QQDUSR        IFNE      *BLANK
     C                   MOVE      QQDUSR        RSDUSR
     C                   ELSE
     C                   MOVEL     '*ALL'        RSDUSR
     C                   END
     C     QQCODE        IFNE      *BLANK
     C                   MOVEL     QQCODE        RSCODE
     C                   ELSE
     C                   MOVEL     '*ALL'        RSCODE
     C                   END
     C*
     C                   MOVE      QQCDAT        RSCDAT
     C*
     C                   Z-ADD     RSCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RXCDAT            6 0
     C*
     C                   MOVE      QQSTAT        RSSTAT
     C                   MOVE      QQSORT        RSSORT
     C                   MOVE      QQREC         RSREC
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   EXCEPT    HEAD
     C*
     C*****************************************************************
     C*  BEGIN LOOP
     C*****************************************************************
     C                   MOVE      '0'           XXONCE
     C                   MOVE      *BLANKS       XXUSER
     C*   START READING THE WORK FILE
     C*
     C                   READ      WXDDP                                  60
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C     XXONCE        IFEQ      '0'
     C                   MOVE      '1'           XXONCE            1
     C                   MOVEL     DDUSER        XXUSER
     C                   EXCEPT    HEAD1
     C                   END
     C*
     C*  IF USER NAME CHANGES, PRINT NEW USER NAME WITH PAGE BREAK
     C*
     C     DDUSER        IFNE      XXUSER
     C                   MOVEL     DDUSER        XXUSER
     C                   MOVE      '1'           *INOF
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C     DD#KY         IFNE      *BLANKS
     C*** BEGIN CHG *** 03/08/00 ***********************************************
     C**********           MOVELDD#KY     XXKEY  50
     C                   MOVEL     DD#KY         XXKEY            40
     C*** BEGIN CHG *** 03/08/00 ***********************************************
     C                   ELSE
     C                   MOVEL     DDKEY         XXKEY
     C                   END
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       yySS# = MaskData('SSN':'DDSS#':DDSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   MOVEL     DDCNAM        XXCNAM
     C*
     C                   Z-ADD     DDACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   Z-ADD     DDCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCDAT            6 0
     C*
     C* PRINT OUT UP TO 3 LINES OF THE REMARK.
     C*
     C                   EXSR      SRDET
     C**************
     C* GET ADDITIONAL REMARKS AND PRINT THEM
     C*
     C                   MOVE      '0'           XXONE
     C                   MOVE      *ZEROS        XXASQ#            5 0
     C                   MOVE      *ZEROS        XXSEQ#
     C                   MOVE      DDSEQ#        XXSEQ#
     C     ARKY          SETLL     PAR01
     C                   READ      PAR01                                  61
     C     *IN61         DOWEQ     '0'
     C     ARKEY         ANDEQ     DDKEY
     C     ARSEQ#        ANDEQ     XXSEQ#
     C*
     C     XXONE         IFEQ      '0'
     C     ARCM75        ANDNE     *BLANKS
     C                   MOVE      '1'           XXONE             1
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DET4
     C                   ELSE                                                    IFEQXXONE
     C*
     C     ARCM75        IFNE      *BLANKS
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DET
     C                   EXCEPT    DET4
     C                   MOVE      '0'           *INOF
     C                   ELSE
     C                   EXCEPT    DET5
     C                   END                                                    IFEQ*INOF
     C*
     C                   END                                                    IFEQXXCM75
     C                   END                                                    IFEQXXONE
     C*
     C                   READ      PAR01                                  61
     C                   END                                                    *IN61DOWEQ
     C**********
     C                   READ      WXDDP                                  60
     C                   END                                                    ENDDO
     C*
     C*
     C*** END PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*================================================================
     C*   SRDET     -                  SUB                            *
     C*================================================================
     C* BEGIN SUBROUTINE
     C*
     C     SRDET         BEGSR
     C*
     C*  BREAK UP 275 BYTES OF COMMENT INTO THREE LINES
     C*
     C                   MOVE      *BLANKS       XXCOMM
     C                   MOVE      *BLANKS       XXCM1
     C                   MOVE      *BLANKS       XXCM2
     C                   MOVE      *BLANKS       XXCM3
     C                   MOVEL     DDCOMM        XXCOMM          184
     C                   MOVEL     XXCOMM        XXCM1            92
     C                   MOVE      XXCOMM        XXCM2            92
     C                   MOVE      DDCOMM        XXCM3            91
     C*
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C     XXCM1         IFNE      *BLANKS
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET1
     C                   END
     C*
     C     XXCM2         IFNE      *BLANKS
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET2
     C                   END
     C*
     C     XXCM3         IFNE      *BLANKS
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET3
     C                   END
     C*  END OF SUBROUTINE
     C                   ENDSR
     C*
     O*****************************************************************
     O*      REPORT ROUTING                                           *
     O*****************************************************************
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**
     O**   SELECTION CRITERIA
     O**
     O          E            ROUTE          1
     O                                           20 'USER         :'
     O                       RSDUSR              32
     O*
     O          E            ROUTE          1
     O                                           20 'REMARK CODE  :'
     O                       RSCODE              26
     O*
     O          E            ROUTE          1
     O                                           20 'DIARY STATUS :'
     O                       RSSTAT              23
     O*
     O          E            ROUTE          1
     O                                           20 'DIARY DATE   :'
     O                       RXCDAT              30 '  /  /  '
     O*
     O          E            ROUTE          1
     O                                           20 'SORT SELECT  :'
     O                       RSSORT              23
     O*
1    O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               13 '(C) COPYRIGHT'
     O****                               30 'PROGRESSIVE DATA'
     O****                               45 'SOLUTIONS,INC.'
     O****                     *YEAR     50
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ******END CHG ***** 01/28/10 **********************************
      ***** END CHG ****** 02/02/06 ***********************************
     O*****************************************************************
     O*      REPORT HEADING                                           *
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSDATE        Y      8
     O                                           71 'DARTS DIARY LISTING'
     O                       DSTIME             120 '  :  :  '
     O          E            HEAD           2
     O                                            6 'SC737R'
     O                                          115 'PAGE:'
     O                       PAGE               120
     O*****************************************************************
     O*      PROGRAM NAME                                             *
     O*****************************************************************
     O          E            HEAD1          2
     O                                           11 'DIARY USER:'
     O                       DDUSER              22
     O*****************************************************************
     O*        DETAIL LINE                                            *
     O*****************************************************************
     O          E            DET         2  1
     O                                            7 'TYPE:'
     O                       DDRMCD              13
     O                                           19 'KEY:'
     O                       XXKEY               61
     O                                           77 'CLAIMANT NAME:'
     O                       XXCNAM             110
     O          E            DET            1
     O                                           14 'REMARK DATE:'
     O                       XXCDAT              23 '  /  /  '
     O                                           77 'SS#:'
      ***** Beg Chg *** 06/09/20 *******************************************
     O***                    DDSS#               89
     O                       YYSS#               89
      ***** End Chg *** 06/09/20 *******************************************
     O                                          105 'ACCIDENT DATE:'
     O                       XXACDT             115 '  /  /  '
     O          E            DET1           1
     O                       XXCM1               94
     O          E            DET2           1
     O                       XXCM2               94
     O          E            DET3           1
     O                       XXCM3               93
     O*
     O          E            DET4           1
     O                                           11 'ADDL RMK:'
     O                       XXCM75              88
     O          E            DET5           1
     O                       XXCM75              88
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
