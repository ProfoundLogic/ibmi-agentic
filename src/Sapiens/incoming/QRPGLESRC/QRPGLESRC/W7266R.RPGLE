     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7266R - CLAIMS TRANSFER AUDIT REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 10/31/00  C:35   PROJECT: WC990                               *
     H*                  CHANGE TO USE STANDAR PARM FORMAT.           *
     H*                                                               *
     H* 05/10/00 C:53 Project WC990                                   *
     H*               Change program to process with a date range and *
     H*               re-write output to match the x-fer search.      *
     H*               Moved messages to print last.                   *
     H*               Corrected dates                                 *
     H*                                                               *
     H* 12/13/99 C:15 Project WC597                                   *
     H*               Changes for Y2K.  BFFYR and AFFYR chgd to 3.0   *
     H*                                                               *
     H* 10/10/96 C:60 PROJECT WC597                                   *
     H*               DEFINE XTSS# AND XTXSS# AS 9 CHARACTER INSTEAD  *
     H*               OF 9 NUMERIC.                                   *
     H*****************************************************************
     H* INDICATORS:
     H*            01 - Controls how the file is read
     H*            05 - print message/detail for DET05
     H*            06 - print message/detail for DET06
     H*            07 - print message/detail for DET07
     H*            10 - print message/detail for DET10
     H*            11 - print message/detail for DET11
     H*            20 - switch fund print or *all on routing
     H*            25 - ACDT change
     H*            50 - main read eof
     H*            99 - secondary reads eof
     H*****************************************************************
     F*
     F*** BEG CHG *** 05/10/00 ******************************
     FWACTL6    UF   E           K DISK
     F*WACTL***UF**E*****      K        DISK
     F*** END CHG *** 05/10/00 ******************************
     F*** BEG ADD *** 05/10/00 ******************************
     FWTGTL     IF   E           K DISK
     F*** END ADD *** 05/10/00 ******************************
     F* * * Batch Routing File
     FSMORL     IF   E           K DISK
     F*
     F* * * Company File
     FSMCOL     IF   E           K DISK
     F*
     F* * * Fund File
     FWMFNL     IF   E           K DISK
     F* * * Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F                                     INFDS(PIDS01)
     FWMUPL     IF   E           K DISK
     F*

     D* * * TOTAL ARRAYS
     D*
     D CHGNUM          C                   CONST('SA020R')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  CTSS#                  1      9
     D  XXSS1#                 1      3
     D  XXSS2#                 4      5
     D  XXSS3#                 6      9
     D                 DS
     D  CTXSS#                 1      9
     D  XTSS1#                 1      3
     D  XTSS2#                 4      5
     D  XTSS3#                 6      9
     D*
     D*
     D*** BEG ADD *** 05/10/00 ******************************
     D PIDS01        E DS                  EXTNAME(SZQ4P)
     D*** END ADD *** 05/10/00 ******************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C* MLS 2/01/88        MOVE '1'       *IN09            DEMAND MODE
     C*     "              MOVE '0'       *IN08            BATCH  MODE
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   CLAIMS TRANSFER WORK FILE
     C*
     C     CTKEY         KLIST
     C                   KFLD                    XXCO#
     C*** BEG ADD *** 05/10/00 **************************************
     C                   KFLD                    XXFND
     C*
     C     CTKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFDAT
     C*
     C*  SYSTEM TABLE VALUE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    CTCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*** END ADD *** 05/10/00 **************************************
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETERS FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER           10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQQUE            10            MLS 1/29/88
     C                   PARM                    QQFND             3            ADD 05/10/00
      **** BEGIN CHG **** 10/31/00 *********************************************
     C******************** PARM           QQFDAT  70       ADD 05/10/00
     C******************** PARM           QQTDAT  70       ADD 05/10/00
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQTDAT            8
      **** END   CHG **** 10/31/00 *********************************************
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
     C*** BEG ADD *** 05/10/00 **************************************
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      *ZEROS        ##DEC             3 0
     C                   PARM      *BLANK        ##THOU            1
     C*
     C     *LIKE         DEFINE    CTTRDT        XXFDAT
      **** BEGIN ADD **** 10/31/00 *********************************************
     C     *LIKE         DEFINE    CTTRDT        XXTDAT
      **** END   ADD **** 10/31/00 *********************************************
     C     *LIKE         DEFINE    FNNAME        XFNAME
     C     *LIKE         DEFINE    GTDESC        XXDLTD
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*** END ADD *** 05/10/00 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C*   CHAIN TO USER PARAMETER FILE
     C*
     C                   MOVE      Q1USER        XXUSER
     C     UPKEY         CHAIN     PUP01                              99
     C  N99              MOVEL     UPUSNM        XXUSNM           50
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C**N11**************  EXSR SR1ST                      DEL 05/10/00
     C*** BEG ADD *** 05/10/00 **************************************
     C*
     C                   EXSR      SR1ST
     C* SET KEYS
     C*
     C     QQFND         IFNE      *BLANKS
     C     QQFND         ANDNE     *ZEROS
     C                   MOVE      QQFND         XXFND
     C                   MOVE      *ON           *IN01                          SINGLE FUND
     C                   ELSE
     C                   MOVE      *LOVAL        XXFND
     C                   ENDIF
     C*
      **** BEGIN CHG **** 10/31/00 *********************************************
     C******************** MOVE QQFDAT    XXFDAT
     C                   MOVE      QQFDAT        XXFDT             8 0
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
      *
     C                   MOVE      QQTDAT        XXTDT             8 0
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT
      **** END   CHG **** 10/31/00 *********************************************
     C*** END ADD *** 05/10/00 **************************************
     C*
     C* READ CLAIMS TRANSFER WORK FILE
     C*
     C********** CTKEY     SETLLPCT01                      DEL 05/10/00
     C     CTKEY1        SETLL     PCT01                                        ADD 05/10/00
     C*
     C     *IN50         DOUEQ     '1'
     C********** CTKEY     READEPCT01                    50DEL 05/10/00
     C*
     C*** BEG ADD *** 05/10/00 **************************************
     C*** READE BASED ON SINGLE OR GLOBAL FUND SELECTION
     C*
     C   01CTKEY         READE     PCT01                                  50
     C  N01COKEY         READE     PCT01                                  50
     C*** END ADD *** 05/10/00 **************************************
     C     *IN50         IFEQ      '0'
     C*
     C*   GET THE FUND NAME
     C*
     C     CTFND         IFNE      XXFND
     C                   MOVE      CTFND         XXFND             3 0
     C*** BEG CHG *** 05/10/00 **************************************
     C***************      MOVE '1'       *IN22
     C                   MOVE      '1'           *INOF
     C*** END CHG *** 05/10/00 **************************************
     C     FNKEY         CHAIN     WMFNL                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME
     C                   END
     C                   END
     C*
     C*** BEG ADD *** 05/10/00 **************************************
     C*
     C*  MUST PASS DATE SELECTION
     C*
      **** BEGIN CHG **** 10/31/00 *********************************************
     C********** CTTRDT    IFLT QQFDAT
     C********** CTTRDT    ORGT QQTDAT
     C     CTTRDT        IFLT      XXFDAT
     C     CTTRDT        ORGT      XXTDAT
      **** END   CHG **** 10/31/00 *********************************************
     C                   ITER
     C                   ENDIF
     C*
     C                   Z-ADD     CTTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM            80
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XXTRDT           10
     C*** END ADD *** 05/10/00 **************************************
     C*
     C* PRINT CLAIM HEADING
     C*
     C*** BEGIN DLT *** 12/13/99 **************************************
     C******************** Z-ADDCTXFYR    BFFYR   20
     C******************** Z-ADDCTFYR     AFFYR   20
     C*** END DLT ***** 12/13/99 **************************************
     C*** BEGIN ADD *** 12/13/99 **************************************
     C                   Z-ADD     CTXFYR        BFFYR             3 0
     C                   Z-ADD     CTFYR         AFFYR             3 0
     C*** END ADD ***** 12/13/99 **************************************
     C*** BEG ADD *** 05/10/00 **************************************
     C     *INOF         IFEQ      *ON
     C     Q4LIN#        ORGT      52
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET1
     C*
     C*** END ADD *** 05/10/00 **************************************
     C*** BEG DEL *** 05/10/00 **************************************
     C***22                EXCPTHEAD
     C*****                SETOF                     22
     C*****                EXCPTDET1
     C***OF                SETON                     22
     C*****                SETOF                     OF
     C*** END DEL *** 05/10/00 **************************************
     C*
     C* PRINT CHANGED ITEMS
     C*   YR, CASE, ACDT INFO
     C*
     C                   MOVE      '0'           *IN25                          RLS 2  JJJ
     C     CTACDT        IFNE      CTXACD
     C*
     C                   Z-ADD     CTACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XTACDT            8 0          CHG 05/10/00
     C*
     C                   Z-ADD     CTXACD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XTXACD            8 0          CHG 05/10/00
     C*
     C                   MOVE      '1'           *IN25                          RLS 2  JJJ
     C                   EXCEPT    DET4
     C                   END
     C*
     C                   MOVE      *BLANKS       CHG               1
     C     CTFYR         IFNE      CTXFYR
     C                   MOVE      '1'           CHG
     C*** BEG CHG *** 05/10/00 **************************************
     C**N25*************** EXCPTDET10                      RLS 2  JJJ
     C  N25              MOVE      *ON           *IN10
     C*** BEG CHG *** 05/10/00 **************************************
     C*                                                    |  7-14-88
     C     CTCASE        IFNE      CTXCAS                                       |
     C** 25 ***********    EXCPTDET6                       DEL 05/10/00
     C   25              MOVE      *ON           *IN06                          ADD 05/10/00
     C                   ELSE                                                   |
     C** 25 ***********    EXCPTDET5                       DEL 05/10/00
     C   25              MOVE      *ON           *IN05                          ADD 05/10/00
     C                   END                                                    |
     C*                                                    |
     C                   ELSE                                                   |
     C*                                                    |
     C     CTCASE        IFNE      CTXCAS                                       |
     C*** BEG CHG *** 05/10/00 **************************************
     C**N25 ************   EXCPTDET10                      |
     C**N25 ************   MOVE '1'       CHG              |
     C                   MOVE      '1'           CHG
     C                   MOVE      *ON           *IN10
     C*** END CHG *** 05/10/00 **************************************
     C                   END                                                    |
     C                   END                                                    |
     C*
     C*   PRINT EMPLOYER CHANGES
     C*
     C     CHG           IFEQ      '1'
     C                   EXCEPT    DET7
     C                   MOVE      *ON           *IN11                          ADD 05/10/00
     C                   END
     C*
     C     CTEMP#        IFNE      CTXEMP
     C     CTDIV         ORNE      CTXDIV
     C                   EXCEPT    DET2
     C                   END
     C*
     C*   PRINT CLASS CODE CHANGES
     C*
     C     CTCLAS        IFNE      CTXCLS
     C                   EXCEPT    DET9
     C                   END
     C*
     C*   PRINT SS# CHANGES
     C*
     C     CTSS#         IFNE      CTXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CTSS#'       QQNAME           25
     C     CTSS#         PARM      CTSS#         QQVAL           100
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CTXSS#'      QQNAME           25
     C     CTXSS#        PARM      CTXSS#        QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
     C                   MOVE      XXSS1#        XTSS#1            3
     C                   MOVE      XXSS2#        XTSS#2            2
     C                   MOVE      XXSS3#        XTSS#3            4
     C                   MOVE      XTSS1#        XTXSS1            3
     C                   MOVE      XTSS2#        XTXSS2            2
     C                   MOVE      XTSS3#        XTXSS3            4
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CTSS#'       QQNAME           25
     C     CTSS#         PARM      CTSS#         QQVAL           100
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CTXSS#'      QQNAME           25
     C     CTXSS#        PARM      CTXSS#        QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
     C                   EXCEPT    DET3
     C                   END
     C*
     C*   GET DELETION CODE DESC
     C*
     C*
     C                   MOVEL(P)  'DLTR'        XXTBID
     C                   MOVEL(P)  CTDLTR        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        XXDLTD
     C   99              MOVE(P)   *BLANKS       XXDLTD
     C*
     C*
     C*   GET CHANGED BY NAME
     C*
     C                   MOVEL     CTUSER        XXUSER
     C     UPKEY         CHAIN     PUP01                              99
     C  N99              MOVEL     UPUSNM        XXCHG            30
     C   99              MOVE      *BLANKS       XXCHG            30
     C                   EXCEPT    DET8
     C*
     C***********          SETON                     22    DEL 05/10/00
     C***********          Z-ADD0         PAGE             DEL 05/10/00
     C*
     C*   PRINT MESSAGES
     C*
     C*** BEG ADD *** 05/10/00 **************************************
     C   05              EXCEPT    DET5
     C   06              EXCEPT    DET6
     C   11              EXCEPT    DET11
     C   10              EXCEPT    DET10
     C                   MOVEA     '000'         *IN(05)
     C                   MOVEA     '00'          *IN(10)
     C*** END ADD *** 05/10/00 **************************************
     C*
     C*   UPDATE PRINTED FLAG
     C*
     C                   MOVE      'Y'           CTPRT
     C                   UPDATE    PCT01
     C*
     C                   ENDIF
     C                   ENDDO
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
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C*                                                    MLS 1/29/88
     C*   SET-UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*** BEG ADD *** 05/10/00 **************************************
     C*
     C*** FORMAT FROM & TO DATES
     C*
     C                   MOVEL(P)  'DATE'        ##MASK           10
     C*
      **** BEGIN CHG **** 10/31/00 *********************************************
     C******************** Z-ADDQQFDAT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     ##NUM  80 P
     C                   MOVEL(P)  QQFDAT        ##NUM            80
      **** END   CHG **** 10/31/00 *********************************************
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XPFDAT           10
     C*
      **** BEGIN CHG **** 10/31/00 *********************************************
     C******************** Z-ADDQQTDAT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     ##NUM  80 P
     C                   MOVEL(P)  QQTDAT        ##NUM            80
      **** END   CHG **** 10/31/00 *********************************************
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         XPTDAT           10
     C*
     C*** PICKUP FUND NAME
     C*
     C     FNKEY         CHAIN     WMFNL                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME
     C                   END
     C*                                                         "
     C                   MOVE      QQFND         XPFND             3 0
     C     XPFND         COMP      *ZEROS                                 20
     C*** END ADD *** 05/10/00 **************************************
     C*                                                         "
     C                   MOVE      Q1USER        XXUSER                              "
     C     UPKEY         CHAIN     PUP01                              99             "
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              99
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              99
     C     *IN99         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C*** BEG DEL *** 05/10/00 **************************************
     C*******************  MOVE '1'       *IN22            HEADING IND IC.
     C*******************  MOVE '1'       *IN11            1ST TIME INDIC.
     C*** END DEL *** 05/10/00 **************************************
     C*
     C                   ENDSR
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
     ***********                                 20 'XXXXXX'
     **********                                  32 'XXXXXX'
     **********                                  41 'XX'
     **********                                  56 'XXXXXXXXXX'
     **********                                  67 'XX     XXX'
     **********                                  77 'XXXXXX'
     ********** E            ROUTE          1
     **********                                  15 'ROUTE TO:'
     **********      08      ORDIS1              60
     O*  MLS 1/29/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
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
     O*  MLS 1/29/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
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
     O*  MLS 1/29/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**** BEG ADD **** 05/10/00 ************************************
     O          E            ROUTE          1
     O                                           19 'GROUP       :'
     O              N20      XPFND         Z     23
     O               20                          24 '*ALL'
     O          E            ROUTE          1
     O                                           19 'DATE RANGE  :'
     O                       XPFDAT              30
     O                                           32 '-'
     O                       XPTDAT              43
     O*
     O**** END ADD **** 05/10/00 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              85
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O                                           79 'DATE RANGE:'
     O                       XPFDAT              90
     O                                           92 '-'
     O                       XPTDAT             103
     O*
     O*** BEG ADD *** 05/10/00 **************************************
     O*
     O          E            DET1        2  2
     O                                           14 'TRANSFER DATE:'
     O                       XXTRDT              25
     O                                           66 'ADJUSTMENTS TO CLAIM #:'
     O                       CTFND         Z     70
     O                                           71 '-'
     O                       BFFYR               74
     O                                           75 '-'
     O                       CTXCAS              82
     O*
     O          E            DET2           1
     O                                           42 'EMPLOYER:'
     O                       CTXEMP        Z     52
     O                       CTXDIV        Z     58
     O                                           85 'CHANGED TO:'
     O                       CTEMP#        Z     95
     O                       CTDIV         Z    101
     O*
     O          E            DET3           1
     O                                           42 'SOCIAL SECURITY #:'
     O                       XTXSS1              46
     O                                           47 '-'
     O                       XTXSS2              49
     O                                           50 '-'
     O                       XTXSS3              54
     O                                           85 'CHANGED TO:'
     O                       XTSS#1              89
     O                                           90 '-'
     O                       XTSS#2              92
     O                                           93 '-'
     O                       XTSS#3              97
     O*
     O          E            DET4           1
     O                                           42 'ACCIDENT DATE:'
     O                       XTXACD        Y     53
     O                                           85 'CHANGED TO:'
     O                       XTACDT        Y     96
     O*
     O          E            DET5           1
     O                                           63 'ACCIDENT DATE CHANGE'
     O                                           86 'RESULTED IN CHANGE OF:'
     O                                         +  1 'POLICY YEAR'
     O*
     O          E            DET6           1
     O                                           63 'ACCIDENT DATE CHANGE'
     O                                           86 'RESULTED IN CHANGE OF:'
     O                                         +  1 'POLICY YEAR'
     O*
     O          E            DET6           1
     O                                           99 'CLAIM NUMBER'
     O*
     O          E            DET7           1
     O                                           42 'CLAIM #:'
     O                       CTFND         Z     46
     O                                           47 '-'
     O                       BFFYR               50
     O                                           51 '-'
     O                       CTXCAS              58
     O                                           85 'CHANGED TO:'
     O                       CTFND         Z     89
     O                                           90 '-'
     O                       AFFYR               93
     O                                           94 '-'
     O                       CTCASE             101
     O*
     O          E            DET11          1
     O                       CTDET#        3     48
     O                                           69 'DETAIL ITEMS CHANGED'
     O                                           89 'TO NEW CLAIM NUMBER'
     O*
     O          E            DET8           1
     O                                           42 'CHANGES MADE BY:'
     O                       XXCHG               73
     O*
     O          E            DET8           1
     O                                           41 'ADDITIONAL TRANSFER INFO'
     O                                         +  0 ':'
     O                       CTREQ               73
     O*
     O          E            DET8           1
     O                                           42 'CLAIM DELETION CODE:'
     O                       CTDLTR              45
     O                       XXDLTD              86
     O*
     O          E            DET8           1
     O                                           39 'PAYMENT AMOUNT TRANSFERR'
     O                                         +  0 'ED:'
     O                       CTAMT         J     58
     O*
     O          E            DET9           1
     O                                           42 'JOB CLASS CODE:'
     O                       CTXCLS        Z     48
     O                                           85 'CHANGED TO:'
     O                       CTCLAS        Z     90
     O*
     O          E            DET10          1
     O                                           65 'CLAIM HAS BEEN CHANGED'
     O*** END ADD *** 05/10/00 **************************************
     O*** BEG DEL *** 05/10/00 **************************************
     O*** THE DETAIL OUTPUT HAS BEEN REWRITTEN THIS IS THE OLD OUTPUT
     O***
     O********
     O********E  3             DET1
     O********                           32 'ADJUSTMENTS TO CLAIM #:'
     O********                 CTFND Z   36
     O********                           37 '-'
     O****BEGIN DLT *** 12/13/99 **************************************
     O************************ BFFYR     39
     O************************           40 '-'
     O************************ CTXCAS    47
     O****END*DLT ***** 12/13/99 **************************************
     O****BEGIN ADD *** 12/13/99 **************************************
     O********                 BFFYR     40
     O********                           41 '-'
     O********                 CTXCAS    48
     O****END*ADD ***** 12/13/99 **************************************
     O********E  2             DET2
     O********                           19 'EMPLOYER:'
     O********                 CTXEMPZ   29
     O********                 CTXDIVZ   35
     O********                           47 'CHANGED TO:'
     O********                 CTEMP#Z   57
     O********                 CTDIV Z   63
     O********E  2             DET3
     O********                           27 'SOCIAL SECURITY#:'
     O********
     O********                 XTXSS1    31
     O********                           32 '-'
     O********                 XTXSS2    34
     O********                           35 '-'
     O********                 XTXSS3    39
     O********
     O********                           53 'CHANGED TO:'
     O********
     O********                 XTSS#1    57
     O********                           58 '-'
     O********                 XTSS#2    60
     O********                           61 '-'
     O********                 XTSS#3    65
     O********
     O********E  2             DET4
     O********                           24 'ACCIDENT DATE:'
     O********                 XTXACDY   33
     O********                           48 'CHANGED TO:'
     O********                 XTACDTY   57
     O********E  2             DET5
     O********                           30 'ACCIDENT DATE CHANGE'
     O********                           53 'RESULTED IN CHANGE OF:'
     O********                         +  1 'POLICY YEAR'
     O********E  1             DET6
     O********                           30 'ACCIDENT DATE CHANGE'
     O********                           53 'RESULTED IN CHANGE OF:'
     O********                         +  1 'POLICY YEAR'
     O********E  2             DET6
     O********                           67 'CLAIM NUMBER'
     O********E  2             DET7
     O********                           22 '** NEW CLAIM #:'
     O********                 CTFND Z   26
     O********                           27 '-'
     O****BEGIN DLT *** 12/13/99 **************************************
     O************************ AFFYR     29
     O************************           30 '-'
     O************************ CTCASE    37
     O****END*DLT ***** 12/13/99 **************************************
     O****BEGIN ADD *** 12/13/99 **************************************
     O********                 AFFYR     30
     O********                           31 '-'
     O********                 CTCASE    38
     O****END*ADD ***** 12/13/99 **************************************
     O********E  3             DET7
     O********                 CTDET#Z   15
     O********                           36 'DETAIL ITEMS CHANGED'
     O********                           56 'TO NEW CLAIM NUMBER'
     O********E  2             DET8
     O********                           26 'CHANGES MADE BY:'
     O********                 XXCHG     57
     O********E  3             DET8
     O********                           23 'REQUESTED BY:'
     O********                 CTREQ     54
     O********E  2             DET9
     O********                           25 'JOB CLASS CODE:'
     O********                 CTXCLSZ   31
     O********                           50 'CHANGED TO:'
     O********                 CTCLASZ   56
     O********E  2             DET10
     O********                           30 'CLAIM HAS BEEN CHANGED'
     O*** END DEL *** 05/10/00 **************************************
